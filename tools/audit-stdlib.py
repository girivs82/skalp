#!/usr/bin/env python3
"""Exercise every importable stdlib entity from real (non-generic) hardware.

The stdlib is mostly generic, so nothing compiles it until a design
instantiates it with concrete arguments. That leaves whole modules untested by
the suite, and the failures they hide are quiet ones: a design that builds and
emits SystemVerilog that cannot elaborate.

For each entity under `skalp::numeric::*` this generates a top that
instantiates it with concrete generic arguments and drives one of its outputs,
builds it, and checks the emitted SystemVerilog for:

  TEMPLATE-INST  a module WITHOUT parameters instantiates one WITH parameters —
                 real hardware wired to an unspecialized generic, which
                 simulates and synthesizes as zeros
  DANGLING       an instance names a module defined nowhere in the output. The
                 build still exits 0; the design cannot elaborate
  EMPTY-REACHED  real hardware instantiates a module with no body at all

Build failures are reported separately and are NOT findings on their own: the
generic arguments substituted here may not suit every entity, so each one needs
reading before it counts. Run it as:

    SKALP_STDLIB_PATH=crates/skalp-stdlib python3 tools/audit-stdlib.py

Exits non-zero if any invariant finding is present.
"""
import glob
import json
import os
import re
import subprocess
import sys
import tempfile

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
SKALP = os.path.join(ROOT, "target/release/skalp")

# Concrete values substituted for each generic parameter, by NAME. A parameter
# this does not know becomes a plausible default, and the entity shows up as a
# build failure to be read rather than as a finding.
BIND = {
    "T": "fp32", "N": "3", "N_IN": "3", "N_OUT": "3",
    "W": "32", "W_IN": "32", "W_OUT": "32", "W_INT": "16",
    "S": "true", "S_IN": "true", "S_OUT": "true",
}
# `const F` is fraction-bits in fixed<> but a FloatFormat in fp<>; the declared
# parameter type is what tells them apart.
FLOAT_FORMATS = {"F": "IEEE754_32", "FROM": "IEEE754_32", "TO": "IEEE754_16"}
NAT_F = {"F": "16", "F_IN": "16", "F_OUT": "16"}

PRELUDE = """use skalp::numeric::fp::*;
use skalp::numeric::formats::*;
use skalp::numeric::vector::*;
use skalp::numeric::fixed::*;
use skalp::numeric::int::*;
use skalp::numeric::cordic::*;
use skalp::numeric::trig::*;
use skalp::numeric::partitionable_adder::*;
use skalp::numeric::partitionable_multiplier::*;
"""

KW = {
    "input", "output", "inout", "wire", "reg", "assign", "always", "if", "else",
    "case", "begin", "end", "logic", "module", "endmodule", "function", "task",
    "parameter", "localparam", "generate", "endgenerate", "initial",
}


def parse_generics(sig):
    """[(name, kind)] from a <...> signature; kind in type/nat/bool/fmt/intent."""
    if not sig:
        return []
    out = []
    for part in [p.strip() for p in sig.strip()[1:-1].split(",") if p.strip()]:
        if part.startswith("intent "):
            out.append((part.split()[1], "intent"))
        elif part.startswith("const "):
            name, _, ty = part[len("const "):].partition(":")
            kind = {"nat": "nat", "bool": "bool", "FloatFormat": "fmt"}.get(ty.strip(), "nat")
            out.append((name.strip(), kind))
        else:
            out.append((part.split(":")[0].strip(), "type"))
    return out


def value_for(name, kind):
    if kind == "type":
        return BIND.get(name, "fp32")
    if kind == "fmt":
        return FLOAT_FORMATS.get(name, "IEEE754_32")
    if kind == "bool":
        return BIND.get(name, "true")
    if name in NAT_F and name.startswith("F"):
        return NAT_F[name]
    return BIND.get(name, "8")


def subst(text, env):
    return re.sub(r"\b[A-Za-z_][A-Za-z0-9_]*\b", lambda m: env.get(m.group(0), m.group(0)), text)


def entities_in(path):
    src = open(path).read()
    for m in re.finditer(r"^(?:pub )?entity\s+(\w+)\s*(<[^{]*?>)?\s*\{(.*?)^\}", src, re.M | re.S):
        ports = [
            (pm.group(1), pm.group(2), pm.group(3).strip())
            for pm in re.finditer(
                r"^\s*(in|out)\s+(\w+)\s*:\s*([^\n]+?)\s*(?://.*)?$", m.group(3), re.M
            )
        ]
        yield m.group(1), m.group(2), ports


def design_for(name, sig, ports):
    gens = [(n, k) for n, k in parse_generics(sig) if k != "intent"]
    env = {n: value_for(n, k) for n, k in gens}
    args = [env[n] for n, _ in gens]
    ins = [(p, subst(t, env)) for d, p, t in ports if d == "in"]
    outs = [(p, subst(t, env)) for d, p, t in ports if d == "out"]
    if not outs:
        return None
    target = f"{name}<{', '.join(args)}>" if args else name
    return (
        PRELUDE
        + "\nentity AuditTop {\n"
        + "\n".join(f"    in {p}: {t}" for p, t in ins)
        + f"\n    out probe: {outs[0][1]}\n}}\n\nimpl AuditTop {{\n"
        + f"    inst u = {target} {{ {', '.join(f'{p}: {p}' for p, _ in ins)} }}\n"
        + f"    probe = u.{outs[0][0]}\n}}\n"
    )


def check(sv_path):
    sv = open(sv_path).read()
    defined = set(re.findall(r"^module\s+(\w+)", sv, re.M))
    params, empty = set(), set()
    cur, buf = None, []
    for line in sv.split("\n"):
        m = re.match(r"^module\s+(\w+)", line)
        if m:
            cur, buf = m.group(1), [line]
            continue
        if cur is None:
            continue
        buf.append(line)
        if line.startswith("endmodule"):
            if any("#(" in x for x in buf[:2]):
                params.add(cur)
            has_body = [
                x for x in buf
                if re.search(r"\b(assign|always)\b", x) or re.match(r"^\s+\w+\s+\w+\s*\($", x)
            ]
            if not has_body:
                empty.add(cur)
            cur = None
    findings, cur = [], None
    for line in sv.split("\n"):
        m = re.match(r"^module\s+(\w+)", line)
        if m:
            cur = m.group(1)
            continue
        mi = re.match(r"^\s+(\w+)\s+(\w+)\s*\($", line)
        if not (mi and cur) or mi.group(1) in KW:
            continue
        tgt = mi.group(1)
        if tgt not in defined:
            findings.append(("DANGLING", f"{cur}.{mi.group(2)} -> {tgt}"))
        elif cur not in params and tgt in params:
            findings.append(("TEMPLATE-INST", f"{cur}.{mi.group(2)} -> {tgt}"))
        elif cur not in params and tgt in empty:
            findings.append(("EMPTY-REACHED", f"{cur}.{mi.group(2)} -> {tgt}"))
    return findings


def main():
    if not os.path.exists(SKALP):
        sys.exit(f"build the compiler first: cargo build --release -p skalp ({SKALP} missing)")
    env = dict(os.environ)
    env.setdefault("SKALP_STDLIB_PATH", os.path.join(ROOT, "crates/skalp-stdlib"))
    work = tempfile.mkdtemp(prefix="skalp-stdlib-audit-")
    results = []
    for path in sorted(glob.glob(os.path.join(ROOT, "crates/skalp-stdlib/skalp/numeric/*.sk"))):
        module = os.path.basename(path)[:-3]
        for name, sig, ports in entities_in(path):
            src = design_for(name, sig, ports)
            if src is None:
                results.append({"entity": name, "module": module, "status": "no-output-port"})
                continue
            sk = os.path.join(work, f"{name}.sk")
            open(sk, "w").write(src)
            p = subprocess.run(
                [SKALP, "build", "-s", sk, "-o", os.path.join(work, f"o_{name}")],
                cwd=ROOT, env=env, capture_output=True, text=True, timeout=900,
            )
            sv = os.path.join(work, f"o_{name}", "design.sv")
            if p.returncode != 0 or not os.path.exists(sv):
                txt = re.sub(r"\x1b\[[0-9;]*m", "", p.stderr + p.stdout)
                err = next(
                    (l.strip() for l in txt.split("\n")
                     if re.match(r"\s*(Error|error(\[|:)|thread .* panicked)", l)),
                    "(no error line)",
                )
                results.append({
                    "entity": name, "module": module,
                    "status": "build-failed", "error": err[:300],
                })
                continue
            f = check(sv)
            results.append({
                "entity": name, "module": module,
                "status": "findings" if f else "clean", "findings": f,
            })

    clean = [r for r in results if r["status"] == "clean"]
    found = [r for r in results if r["status"] == "findings"]
    failed = [r for r in results if r["status"] == "build-failed"]
    print(f"exercised {len(results)}: {len(clean)} clean, {len(found)} with findings, "
          f"{len(failed)} build-failed")
    # Findings are reported by WHERE THEY LIVE, not per design. Every design
    # glob-imports the whole stdlib, so one bad instance inside a shared entity
    # would otherwise be re-reported once per entity exercised and read as a
    # hundred defects instead of one.
    unique = {}
    for r in found:
        for kind, detail in r["findings"]:
            unique.setdefault((kind, detail), []).append(f"{r['module']}::{r['entity']}")
    if unique:
        print(f"\n{len(unique)} distinct finding(s):")
        for (kind, detail), seen_in in sorted(unique.items()):
            print(f"    {kind}: {detail}   (surfaced by {len(seen_in)} design(s))")
    if failed:
        print("\nbuild failures (read each — the substituted generic arguments may not suit):")
        for r in failed:
            print(f"    {r['module']}::{r['entity']}: {r['error'][:140]}")
    out = os.path.join(work, "results.json")
    json.dump(results, open(out, "w"), indent=1)
    print(f"\nfull results: {out}")
    sys.exit(1 if found else 0)


if __name__ == "__main__":
    main()

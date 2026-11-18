#!/usr/bin/env python3
"""Pick the tests a change can affect.

Given the files changed since a base revision, prints the cargo commands that
run only the tests those files can affect:

  * a crate's own tests (unit tests and its `tests/` directory) run when the
    crate or any workspace crate it depends on changed;
  * a module of the integration suite (`tests/suite/<module>.rs`) runs when any
    crate it uses changed (directly, through the crates those depend on, or
    through the `skalp` binary), when the module itself changed, or when a
    fixture or library source file it names changed;
  * documentation changes run nothing; changes to the build, CI or anything
    this script does not recognise run everything.

Usage:
  scripts/select_tests.py [--base REV] [--run] [--print-modules]

  --base REV   compare the working tree (and HEAD) against REV (default:
               origin/master); the merge base is used, as a pull request does
  --run        run the selected commands instead of printing them
  --print-modules
               list the selected suite modules, one per line
  --files F..  take these paths as the changed files (to check a selection)

The output is either `full` followed by the full-suite command, or one command
per line. Every suite module must be declared in tests/suite/main.rs; a module
file that is not declared is an error, so no test can silently drop out.
"""

import argparse
import json
import os
import re
import subprocess
import sys

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
SUITE = os.path.join(ROOT, "tests", "suite")

# Crates never tested in CI (see .github/workflows/test-suite.yml)
EXCLUDED = {"skalp-verify", "skalp-formal"}

FULL_CMD = (
    "cargo test --workspace --exclude skalp-verify --exclude skalp-formal "
    "--all-targets --all-features --no-fail-fast"
)
FULL_DOC_CMD = (
    "cargo test --doc --workspace --all-features --exclude skalp "
    "--exclude skalp-verify --exclude skalp-formal"
)

# Paths that change nothing a test can see
DOC_ONLY = re.compile(r"(\.md$|^docs/|^LICENSE|\.txt$|^\.gitignore$|^examples/.*\.md$)")


def git(*args):
    return subprocess.run(
        ["git", *args], cwd=ROOT, check=True, capture_output=True, text=True
    ).stdout


def changed_files(base):
    """The changed paths, or None when there is no merge base to compare with
    (a shallow clone, an unknown base): the caller then runs everything"""
    try:
        merge_base = git("merge-base", base, "HEAD").strip()
    except subprocess.CalledProcessError as e:
        print(f"no merge base with {base}: {e.stderr.strip()}", file=sys.stderr)
        return None
    files = set(git("diff", "--name-only", merge_base).split())
    # untracked files count too (a new test or fixture not yet committed)
    files |= set(git("ls-files", "--others", "--exclude-standard").split())
    return sorted(files)


def workspace():
    """{crate: (directory relative to ROOT, [workspace dependencies])}"""
    meta = json.loads(
        subprocess.run(
            ["cargo", "metadata", "--format-version", "1", "--no-deps"],
            cwd=ROOT,
            check=True,
            capture_output=True,
            text=True,
        ).stdout
    )
    names = {p["name"] for p in meta["packages"]}
    crates = {}
    for p in meta["packages"]:
        d = os.path.relpath(os.path.dirname(p["manifest_path"]), ROOT)
        deps = sorted(
            {x["name"] for x in p["dependencies"] if x["name"] in names}
        )
        crates[p["name"]] = (d, deps)
    return crates


def target_kinds(crate):
    """The kinds of target a workspace crate has (lib, bin, test, ...)"""
    meta = json.loads(
        subprocess.run(
            ["cargo", "metadata", "--format-version", "1", "--no-deps"],
            cwd=ROOT,
            check=True,
            capture_output=True,
            text=True,
        ).stdout
    )
    for p in meta["packages"]:
        if p["name"] == crate:
            return {k for t in p["targets"] for k in t["kind"]}
    return set()


def closure(crates, start):
    """`start` and every workspace crate it depends on"""
    seen, todo = set(), [start]
    while todo:
        c = todo.pop()
        if c in seen or c not in crates:
            continue
        seen.add(c)
        todo.extend(crates[c][1])
    return seen


def suite_modules():
    main = open(os.path.join(SUITE, "main.rs")).read()
    declared = re.findall(r"^\s*mod\s+(\w+)\s*;", main, re.M)
    files = {f[:-3] for f in os.listdir(SUITE) if f.endswith(".rs") and f != "main.rs"}
    missing = sorted(files - set(declared))
    if missing:
        sys.exit(f"tests/suite modules not declared in main.rs: {', '.join(missing)}")
    return declared


def module_uses(crates, module):
    """The workspace crates a suite module uses, with their dependencies"""
    text = open(os.path.join(SUITE, module + ".rs")).read()
    # crate paths count in code only: a design source in a string literal says
    # `use skalp::memory::...` without using the crate
    code = re.sub(r"//[^\n]*", "", text)
    code = re.sub(r'r#*"(?:.|\n)*?"#*|"(?:\\.|[^"\\])*"', '""', code)
    used = set()
    for ident in set(re.findall(r"\bskalp_[a-z_]+", code)):
        name = ident.replace("_", "-")
        if name in crates:
            used.add(name)
    # the skalp crate itself: its library, or the binary it builds
    if re.search(r"\bskalp::", code) or re.search(r"CARGO_BIN_EXE_skalp|Command::new", text):
        used.add("skalp")
    # every module goes through the testing crate's helpers or the frontend
    deps = set()
    for c in used:
        deps |= closure(crates, c)
    return deps, text


def owning_crate(crates, path):
    best = None
    for name, (d, _) in crates.items():
        if d == ".":
            continue
        if path.startswith(d + "/") and (best is None or len(d) > len(crates[best][0])):
            best = name
    return best


def select(base, given=None):
    crates = workspace()
    files = given if given is not None else changed_files(base)
    if files is None:
        return None, f"no merge base with {base}", []
    modules = suite_modules()
    uses = {m: module_uses(crates, m) for m in modules}

    changed_crates = set()
    picked_modules = set()
    full_reason = None
    for f in files:
        if DOC_ONLY.search(f):
            continue
        if f.startswith("tests/suite/"):
            name = os.path.basename(f)[:-3] if f.endswith(".rs") else None
            if name == "main":
                full_reason = f
            elif name in uses:
                picked_modules.add(name)
            elif name is None:
                full_reason = f
            continue
        if f.startswith("tests/") or f.startswith("examples/"):
            # a fixture or example: the modules that name it
            stem = os.path.splitext(os.path.basename(f))[0]
            parent = os.path.basename(os.path.dirname(f))
            hits = [
                m for m, (_, text) in uses.items()
                if os.path.basename(f) in text or (parent not in ("fixtures", "examples", "tests") and parent in text) or stem in text
            ]
            picked_modules.update(hits)
            if not hits and not f.startswith("examples/"):
                full_reason = f
            continue
        if f.startswith("crates/skalp-stdlib/skalp/"):
            # a library source: the modules that name it, and the stdlib crate's own tests
            stem = os.path.splitext(os.path.basename(f))[0]
            parent = os.path.basename(os.path.dirname(f))
            for m, (_, text) in uses.items():
                if stem in text or (parent + "::") in text or (parent + "/") in text:
                    picked_modules.add(m)
            changed_crates.add("skalp-stdlib-sources")
            continue
        crate = owning_crate(crates, f)
        if crate is not None and not f.endswith("Cargo.toml"):
            changed_crates.add(crate)
            continue
        if f.startswith("src/"):
            changed_crates.add("skalp")
            continue
        # build files, CI, scripts, lockfile, anything else: everything
        full_reason = f
    if full_reason:
        return None, f"{full_reason} changed", files

    real = {c for c in changed_crates if c in crates}
    # crates whose tests are affected: those depending on a changed crate
    test_crates = sorted(
        c for c in crates
        if c not in EXCLUDED and closure(crates, c) & real
    )
    for m, (deps, _) in uses.items():
        if deps & real:
            picked_modules.add(m)
    if "skalp-stdlib-sources" in changed_crates:
        test_crates = sorted(set(test_crates) | {"skalp-stdlib"})
    return (test_crates, sorted(picked_modules)), None, files


def commands(selection):
    test_crates, modules = selection
    cmds = []
    others = [c for c in test_crates if c != "skalp"]
    if others:
        pkgs = " ".join(f"-p {c}" for c in others)
        cmds.append(f"cargo test --all-targets --all-features --no-fail-fast {pkgs}")
        cmds.append(f"cargo test --doc --all-features --no-fail-fast {pkgs}")
    if "skalp" in test_crates:
        # its unit tests: `--lib` only if it has a library, which cargo
        # otherwise refuses
        kinds = target_kinds("skalp")
        targets = [f"--{k}" for k, have in (("lib", "lib"), ("bins", "bin")) if have in kinds]
        if targets:
            cmds.append(
                f"cargo test -p skalp {' '.join(targets)} --all-features --no-fail-fast"
            )
    if modules:
        cmds.append(
            "cargo test -p skalp --test suite --all-features --no-fail-fast -- "
            + " ".join(f"{m}::" for m in modules)
        )
    return cmds


def main():
    ap = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    ap.add_argument("--base", default="origin/master")
    ap.add_argument("--run", action="store_true")
    ap.add_argument("--print-modules", action="store_true")
    ap.add_argument(
        "--files", nargs="*", help="take these as the changed files instead of asking git"
    )
    args = ap.parse_args()

    selection, full_reason, files = select(args.base, args.files)
    if selection is None:
        print(f"full ({full_reason})", file=sys.stderr)
        cmds = [FULL_CMD, FULL_DOC_CMD]
    else:
        if args.print_modules:
            print("\n".join(selection[1]))
            return
        cmds = commands(selection)
        print(
            f"{len(files)} files changed: {len(selection[0])} crates, "
            f"{len(selection[1])} suite modules",
            file=sys.stderr,
        )
    if not args.run:
        print("\n".join(cmds) if cmds else "# nothing to test")
        return
    status = 0
    for c in cmds:
        print(f"+ {c}", file=sys.stderr, flush=True)
        status |= subprocess.run(c, shell=True, cwd=ROOT).returncode
    sys.exit(1 if status else 0)


if __name__ == "__main__":
    main()

#!/usr/bin/env python3
"""Doc snippet checker: extracts fenced code blocks from markdown files,
classifies SKALP snippets, compiles complete designs with `skalp build`,
and flags dead-dialect markers in all SKALP blocks.

Tutorial chapters build one multi-file project across chapters (snippets
import earlier snippets with `use module::Entity`), so complete snippets
are staged as module files — named from a leading `// name.sk` comment or
the snake_cased first entity name — and each snippet is compiled with all
previously staged modules present, in file order across the invocation.
This mirrors the reader's src/ directory at that point in the tutorial.

Usage: python3 tools/doc_snippet_check.py FILE.md [FILE.md ...]
"""
import re
import subprocess
import sys
import tempfile
import pathlib

SKALP = pathlib.Path(__file__).resolve().parent.parent / "target/release/skalp"

FENCE_RE = re.compile(r"^```(\w*)\s*$")

# Textual markers of the dead dialects / removed constructs
MARKERS = [
    (re.compile(r"^\s*[\w\.\[\]]+\s*<=\s*[^=<]", re.M), "`<=` used as assignment (comparison-only in current dialect)"),
    (re.compile(r"^\s*let\s+\w+\s*=\s*[A-Z]\w*\s*(?:::)?\s*(?:<[^>{;\n]*>)?\s*\{", re.M), "`let x = Entity { }` instantiation (removed; use `inst`)"),
    (re.compile(r"\bstream\s*<", re.M), "stream<T> port (unimplemented; hard error)"),
    (re.compile(r"^\s*process\s*[({]", re.M), "`process` block (dead dialect)"),
    (re.compile(r"^\s*always\b", re.M), "`always` block (dead dialect)"),
]

FILE_HEADER_RE = re.compile(r"^\s*//\s*([\w-]+\.sk)\b")


def module_name(code: str) -> str | None:
    """Module filename for staging: `// name.sk` header comment, else the
    snake_cased first entity name."""
    for line in code.split("\n"):
        if not line.strip():
            continue
        m = FILE_HEADER_RE.match(line)
        if m:
            return m.group(1)
        if not line.strip().startswith("//"):
            break
    m = re.search(r"^\s*entity\s+(\w+)", code, re.M)
    if m:
        return re.sub(r"(?<=[a-z0-9])(?=[A-Z])", "_", m.group(1)).lower() + ".sk"
    return None


def is_rust(code: str) -> bool:
    return bool(
        re.search(r"#\[tokio::test\]|#\[test\]|use skalp_|fn main\(\)|\.await|println!|assert_eq!|let mut \w+ = Testbench", code)
    )


def is_skalp(code: str) -> bool:
    return bool(re.search(r"^\s*(entity|impl|trait|on\s*\(|signal|inst)\b", code, re.M))


def is_complete(code: str) -> bool:
    return bool(re.search(r"^\s*(?:pub\s+)?(?:async\s+)?entity\s+\w+", code, re.M)) and bool(
        re.search(r"^\s*impl\s+\w+", code, re.M)
    )


def blocks(path: pathlib.Path):
    lang = None
    buf = []
    start = 0
    for i, line in enumerate(path.read_text().split("\n"), 1):
        m = FENCE_RE.match(line)
        if m:
            if lang is None:
                lang = m.group(1) or ""
                buf = []
                start = i
            else:
                yield (start, lang, "\n".join(buf))
                lang = None
        elif lang is not None:
            buf.append(line)


def main():
    total_bad = 0
    staged: dict[str, str] = {}  # module filename -> code, accumulated in file order
    for f in sys.argv[1:]:
        path = pathlib.Path(f)
        issues = []
        n_skalp = n_compiled = 0
        for start, lang, code in blocks(path):
            if lang in ("bash", "toml", "tcl", "ebnf", "systemverilog", "verilog", "vhdl", "sv", "cpp", "c", "python", "json", "text"):
                continue
            if is_rust(code) or not is_skalp(code):
                continue
            n_skalp += 1
            for rx, msg in MARKERS:
                if rx.search(code):
                    issues.append((start, f"MARKER: {msg}"))
            if is_complete(code):
                n_compiled += 1
                name = module_name(code) or "snippet.sk"
                with tempfile.TemporaryDirectory() as td:
                    for mod, mod_code in staged.items():
                        if mod != name:
                            (pathlib.Path(td) / mod).write_text(mod_code)
                    src = pathlib.Path(td) / name
                    src.write_text(code)
                    r = subprocess.run(
                        [str(SKALP), "build", str(src), "-o", td + "/out"],
                        capture_output=True,
                        text=True,
                        timeout=120,
                    )
                    if r.returncode != 0:
                        first_err = next(
                            (l for l in (r.stderr + r.stdout).split("\n") if "error" in l.lower()),
                            (r.stderr + r.stdout).strip().split("\n")[-1] if (r.stderr + r.stdout).strip() else "unknown",
                        )
                        issues.append((start, f"COMPILE FAIL: {first_err.strip()[:160]}"))
                    else:
                        # Only stage snippets that compile — the reader's
                        # project only ever contains working files.
                        staged[name] = code
        status = "OK" if not issues else f"{len(issues)} ISSUES"
        print(f"== {f}: {n_skalp} skalp blocks, {n_compiled} compiled — {status}")
        for line, msg in issues:
            print(f"   L{line}: {msg}")
        total_bad += len(issues)
    print(f"\nTOTAL ISSUES: {total_bad}")
    return 0


if __name__ == "__main__":
    sys.exit(main())

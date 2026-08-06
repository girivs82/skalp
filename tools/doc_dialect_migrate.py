#!/usr/bin/env python3
"""Migrate dead-dialect `<=` assignments to `=` inside SKALP code blocks in
markdown files. Only touches statement-position lines (identifier/index/field
target followed by `<=`) inside fenced blocks classified as SKALP.

Usage: python3 tools/doc_dialect_migrate.py FILE.md [...]
"""
import re
import sys
import pathlib

FENCE_RE = re.compile(r"^```(\w*)\s*$")
ASSIGN_RE = re.compile(r"^(\s*[\w\.\[\]\+\s]*?[\w\]])\s*<=\s*(?![=<])")


def is_rust(code: str) -> bool:
    return bool(
        re.search(r"#\[tokio::test\]|#\[test\]|use skalp_|fn main\(\)|\.await|println!|assert_eq!", code)
    )


def is_skalp(code: str) -> bool:
    return bool(re.search(r"^\s*(entity|impl|trait|on\s*\(|signal|inst)\b", code, re.M))


def migrate(path: pathlib.Path) -> int:
    lines = path.read_text().split("\n")
    # First pass: find skalp block ranges
    ranges = []
    lang = None
    start = 0
    for i, line in enumerate(lines):
        m = FENCE_RE.match(line)
        if m:
            if lang is None:
                lang = m.group(1) or ""
                start = i
            else:
                code = "\n".join(lines[start + 1 : i])
                if lang not in ("bash", "toml", "tcl", "ebnf", "systemverilog", "verilog", "vhdl", "sv", "cpp", "c", "python", "json", "text") \
                        and not is_rust(code) and is_skalp(code):
                    ranges.append((start + 1, i))
                lang = None
    changed = 0
    for lo, hi in ranges:
        for i in range(lo, hi):
            l = lines[i]
            s = l.strip()
            # Skip comparison contexts: lines starting with control keywords
            if re.match(r"^(if|while|for|assert|return|match|\}|\)|else)\b", s):
                continue
            new = ASSIGN_RE.sub(lambda m: m.group(1) + " = ", lines[i], count=1)
            if new != lines[i]:
                lines[i] = new
                changed += 1
    if changed:
        path.write_text("\n".join(lines))
    return changed


total = 0
for f in sys.argv[1:]:
    n = migrate(pathlib.Path(f))
    if n:
        print(f"{f}: {n} assignments migrated")
    total += n
print(f"TOTAL: {total}")

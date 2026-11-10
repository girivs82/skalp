#!/usr/bin/env python3
"""Build a Nexus device's IO database from Lattice's public pinout and BSDL.

Usage: gen_nexus_iodb.py <pinout.csv> <device.bsm> <output.json>

Lattice publishes a pinout CSV and BSDL files for each device. prjoxide has no
pin data for CertusPro-NX, so this writes the same shape prjoxide uses for
other Nexus devices, plus each pad's boundary-scan bit.
"""
import csv
import json
import re
import sys

PACKAGES = ["BBG484", "LFG672", "ASG256", "CBG256", "BFG484"]
PAD = re.compile(r"^P([LRTB])(\d+)([AB])$")


def read_pads(path):
    with open(path, encoding="utf-8") as source:
        lines = [
            line
            for line in source
            if not line.startswith(("#", '"#')) and line.strip(",\n ")
        ]
    pads = []
    for row in csv.DictReader(lines):
        match = PAD.match(row["Pin/Ball Funcion"].strip())
        if not match:
            continue
        side, offset, half = match.groups()
        functions = row[" Dual Function"].strip()
        pads.append(
            {
                "bank": int(row["BANK"]),
                "dqs": [],
                "func": [] if functions == "-" else functions.split("/"),
                "offset": int(offset),
                "name": match.group(0),
                "pins": [row[package].strip() or "-" for package in PACKAGES],
                "pio": 0 if half == "A" else 1,
                "side": side,
                "vref": -1,
            }
        )
    return pads


def read_boundary(path):
    with open(path, encoding="utf-8") as source:
        text = source.read()
    cells = re.findall(r'"\s*(\d+)\s*\(\s*BC_\w+\s*,\s*(P[LRTB]\d+[AB])\s*,\s*(\w+)', text)
    return [
        {"bit": int(bit), "pad": pad}
        for bit, pad, function in cells
        if function in ("bidir", "input", "output3", "output2")
    ]


def main():
    pinout, bsdl, output = sys.argv[1:4]
    database = {
        "packages": PACKAGES,
        "pads": read_pads(pinout),
        "boundary": read_boundary(bsdl),
    }
    with open(output, "w", encoding="utf-8") as target:
        json.dump(database, target, separators=(",", ":"))
        target.write("\n")


if __name__ == "__main__":
    main()

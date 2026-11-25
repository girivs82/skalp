#!/usr/bin/env python3
"""Generate the delay-calibration probe: ring oscillators of each kind of delay, counted on the
board clock and reported over a UART.

  python3 gen_probe.py --clk-pin <pin> --clk-mhz <MHz> --uart-pin <pin> [--io-standard LVCMOS33]

writes `delay_probe.sk` (the design), `hex_report.sk` (the UART reporter, its baud divider set
for the clock) and `delay_probe.json` (the manifest `fit_calibration.py` reads: which UART word
counts which ring, and which signal in the build the timed simulation watches for it). See
README.md for the whole procedure.

Rings (each one inversion, so it oscillates once `go` rises after a short power-on wait):

- `ro`: LUT latches, placed freely;
- `r<region>`: the same, pinned to small regions spread over the die (percent of the die);
- `lw`: a few latch stages pinned alternately to two opposite corners, so every hop is a long
  wire across the chip;
- `carry`, `mult`, `dram`: a latch, then stages through one kind of hard block each (the carry
  out of a wide adder, a bit of a multiplier's product, a distributed-RAM read);
- `clk`: two flip-flops toggling each other through the dedicated clock network.

Each ring's rising edges are counted on the board clock (the clock ring counts its own edges in
Gray code, read through two flip-flops). Every 2^18 board clocks a line of sixteen 8-digit hex
words goes out on the UART (8N1, 1 Mbaud): w0 = 4d000000, w1 = line number, w2/w3 = board
clocks since power-on (high 16 bits, low 32 bits), w4.. = the rings' edge counts.
"""
import argparse
import json
from pathlib import Path

HERE = Path(__file__).resolve().parent

# small regions spread over the die, in percent (x0, y0, x1, y1)
REGIONS = {
    "ctr": (45, 40, 55, 60),
    "nw": (2, 5, 12, 25),
    "ne": (88, 5, 98, 25),
    "sw": (2, 75, 12, 95),
    "se": (88, 75, 98, 95),
}
# the long-wire ring's two corners
LONG_REGIONS = {"lw_a": (3, 7, 16, 27), "lw_b": (84, 73, 97, 93)}
# stages per hard-block ring, sized so each ring runs at tens of MHz
HARD_STAGES = {"carry": 8, "mult": 12, "dram": 64}
COUNT_WIDTH = 24  # clock ring Gray counter


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--clk-pin", required=True, help="board clock input pin")
    ap.add_argument("--clk-mhz", type=float, required=True, help="board clock frequency, MHz")
    ap.add_argument("--uart-pin", required=True, help="UART transmit pin (to the host)")
    ap.add_argument("--io-standard", default="LVCMOS33", help="IO standard of both pins")
    ap.add_argument("--baud", type=float, default=1e6, help="UART baud rate (default 1 Mbaud)")
    ap.add_argument("--stages", type=int, default=63, help="stages of each latch ring")
    ap.add_argument("--long-stages", type=int, default=8, help="stages of the long-wire ring (even)")
    ap.add_argument("--out", default=".", help="directory to write the files to")
    a = ap.parse_args()
    if a.long_stages % 2:
        ap.error("--long-stages must be even (the stages alternate between two corners)")
    div = round(a.clk_mhz * 1e6 / a.baud)
    if not 2 <= div <= 256:
        ap.error(f"the clock gives a baud divider of {div}; it must be 2 to 256")
    out = Path(a.out)
    out.mkdir(parents=True, exist_ok=True)

    rings = []  # manifest entries, in UART word order from w4
    L = []
    w = L.append

    def latch_ring(stem, n):
        names = [f"{stem}_{i}" for i in range(n)]
        for nm in names:
            w(f"    signal {nm}: bit")
        for i, nm in enumerate(names):
            src = f"~{names[-1]}" if i == 0 else names[i - 1]
            w(f"    {nm} = if go == 1 {{ {src} }} else {{ {nm} }}")
        edge_counter(stem, names[-1])
        return names

    def edge_counter(stem, last):
        w(f"    signal {stem}_s1: bit = 0\n    signal {stem}_s2: bit = 0\n    signal {stem}_s3: bit = 0")
        w(f"    signal {stem}_n: bit[32] = 0")
        w("    on(clk.rise) {")
        w(f"        {stem}_s1 = {last}\n        {stem}_s2 = {stem}_s1\n        {stem}_s3 = {stem}_s2")
        w(f"        if {stem}_s2 == 1 && {stem}_s3 == 0 {{\n            {stem}_n = {stem}_n + 32'h00000001\n        }}")
        w("    }")

    def add(name, kind, counter, watch):
        rings.append({"name": name, "kind": kind, "word": 4 + len(rings), "counter": counter, **watch})

    w("    // ---------------------------------------------------------------- latch rings")
    latch_ring("ro", a.stages)
    add("ro", "latch", "ro_n", {"watch_latch": "ro_0", "changes_per_period": 2})
    floorplan = []
    for region, area in REGIONS.items():
        stem = f"r{region}"
        names = latch_ring(stem, a.stages)
        floorplan.append((f"ring_{region}", area, names))
        add(stem, "latch", f"{stem}_n", {"watch_latch": f"{stem}_0", "changes_per_period": 2})

    w("    // ---------------------------------------------------------------- long-wire ring")
    names = latch_ring("lw", a.long_stages)
    for j, (region, area) in enumerate(LONG_REGIONS.items()):
        floorplan.append((region, area, names[j::2]))
    add("lw", "long_wire", "lw_n", {"watch_latch": "lw_0", "changes_per_period": 2})

    w("    // ---------------------------------------------------------------- hard-block rings")
    w("    // each: a latch (the one inversion, held until go), then stages through one kind of block")
    w("    signal cr_ones: bit[64] = 0\n    signal mu_op: bit[18] = 0")
    w("    on(clk.rise) {")
    w("        if go == 1 {\n            cr_ones = 64'hFFFFFFFFFFFFFFFF\n            mu_op = 18'h3FFFF\n        }")
    w("    }")
    for kind, n in HARD_STAGES.items():
        w(f"    // {kind} ring")
        for k in range(n + 1):
            w(f"    signal {kind}_{k}: bit")
        w(f"    {kind}_0 = if go == 1 {{ ~{kind}_{n} }} else {{ {kind}_0 }}")
        for k in range(1, n + 1):
            prev = f"{kind}_{k - 1}"
            if kind == "carry":
                # the carry out of all-ones + the bit: the bit rippled through the whole chain
                w(f"    signal carry_sum{k}: bit[65]")
                w(f"    carry_sum{k} = {{1'b0, cr_ones}} + {{64'h0000000000000000, {prev}}}")
                w(f"    carry_{k} = carry_sum{k}[64]")
            elif kind == "mult":
                # bit 17 of the bit times 18'h3FFFF (from a register, so it is not folded away)
                w(f"    signal mult_p{k}: bit[36]")
                w(f"    mult_p{k} = mu_op * {{17'h00000, {prev}}}")
                w(f"    mult_{k} = mult_p{k}[17]")
            else:
                # bit 0 of a 16x4 RAM holding its own addresses, read at the bit
                w("    #[memory(depth = 16)]")
                w(f"    signal dram_m{k}: bit[4][16] = [" + ", ".join(str(x) for x in range(16)) + "]")
                w("    on(clk.rise) {")
                w(f"        if go == 0 {{\n            dram_m{k}[por[3:0]] = por[3:0]\n        }}")
                w("    }")
                w(f"    signal dram_r{k}: bit[4]")
                w(f"    dram_r{k} = dram_m{k}[{{3'b000, {prev}}}]")
                w(f"    dram_{k} = dram_r{k}[0]")
        edge_counter(kind, f"{kind}_{n}")
        add(kind, kind, f"{kind}_n", {"watch_latch": f"{kind}_0", "changes_per_period": 2})

    w("    // ---------------------------------------------------------------- clock-network ring")
    w("    // a toggles on c rising, b on d rising: every half period passes a LUT, a clock-network")
    w("    // entry, the clock tree, a flip-flop and the route back (d is not written as ~c: a clock")
    w("    // taken as another's inverse can lose the inversion in synthesis)")
    W = COUNT_WIDTH
    w("    signal ca: bit = 0\n    signal cb: bit = 0\n    signal cc: bit\n    signal ccn: bit")
    w("    cc = ~(ca ^ cb) & go\n    ccn = (ca ^ cb) & go")
    w(f"    signal cn: bit[{W}] = 0\n    signal cg: bit[{W}] = 0")
    w("    on(cc.rise) {")
    w(f"        ca = ~ca\n        cn = cn + {W}'h000001")
    w(f"        cg = (cn + {W}'h000001) ^ ((cn + {W}'h000001) >> 1)")
    w("    }")
    w("    on(ccn.rise) {\n        cb = ~cb\n    }")
    w(f"    signal cg_s1: bit[{W}] = 0\n    signal cg_s2: bit[{W}] = 0\n    signal cn_bin: bit[{W}]")
    w("    cn_bin = {" + ", ".join(
        " ^ ".join(f"cg_s2[{j}]" for j in range(W - 1, i - 1, -1)) for i in range(W - 1, -1, -1)) + "}")
    w("    on(clk.rise) {\n        cg_s1 = cg\n        cg_s2 = cg_s1\n    }")
    w(f"    signal clk_n: bit[32]\n    clk_n = {{{32 - W}'h0, cn_bin}}")
    add("clk", "clock", "clk_n", {"watch_bus": "cn", "changes_per_period": 1, "counter_bits": W})
    if len(rings) > 12:
        raise SystemExit("more rings than UART words")

    w("    // ---------------------------------------------------------------- UART report")
    w("    signal rp_now: bit[48] = 0\n    signal rp_seq: bit[32] = 0\n    signal rp_pend: bit = 0")
    w("    signal rp_hb: bit[18] = 0\n    signal rp_send: bit\n    rp_send = rp_pend & ~rpt.busy")
    w("    signal rp_w2: bit[32]\n    rp_w2 = {16'h0000, rp_now[47:32]}")
    w("    signal rp_w3: bit[32]\n    rp_w3 = rp_now[31:0]")
    words = {r["word"]: r["counter"] for r in rings}
    w("    inst rpt = HexReport {")
    w("        clk: clk,\n        send: rp_send,\n        w0: 32'h4d000000,\n        w1: rp_seq,")
    w("        w2: rp_w2,\n        w3: rp_w3,")
    for k in range(4, 16):
        w(f"        w{k}: {words.get(k, "32'h00000000")}" + ("," if k < 15 else ""))
    w("    }")
    w("    uart_tx = rpt.tx")
    w("    on(clk.rise) {")
    w("        rp_now = rp_now + 1\n        rp_hb = rp_hb + 1")
    w("        if rp_send == 1 {\n            rp_pend = 0\n            rp_seq = rp_seq + 1\n        }")
    w("        if rp_hb == 0 {\n            rp_pend = 1\n        }")
    w("    }")
    w("}")

    head = [
        "// GENERATED by gen_probe.py: do not edit (see README.md).",
        "mod hex_report;",
        "use hex_report::HexReport;",
        "",
        "constraint physical {",
        "    floorplan {",
    ]
    for region, (x0, y0, x1, y1), names in floorplan:
        head += [f'        region "{region}" {{', f"            area: ({x0}%, {y0}%, {x1}%, {y1}%)",
                 f"            instances: [{', '.join(names)}]", "        }"]
    head += ["    }", "}", ""]
    io = f'io_standard: "{a.io_standard}"'
    head += [
        "entity DelayProbe {",
        f'    in clk: clock @ {{ pin: "{a.clk_pin}", {io} }}',
        f'    out uart_tx: bit @ {{ pin: "{a.uart_pin}", {io} }}',
        "}",
        "",
        "impl DelayProbe {",
        "    // power-on wait: 4096 board clocks, then every ring starts",
        "    signal por: bit[12] = 0",
        "    signal go: bit = 0",
        "    on(clk.rise) {",
        "        if por != 12'hFFF {\n            por = por + 1\n        } else {\n            go = 1\n        }",
        "    }",
    ]
    (out / "delay_probe.sk").write_text("\n".join(head + L) + "\n")

    reporter = (HERE / "hex_report.sk").read_text()
    old = "    div_m1 = 124\n"
    assert reporter.count(old) == 1
    (out / "hex_report.sk").write_text(reporter.replace(old, f"    div_m1 = {div - 1}\n"))

    period_ns = 1e3 / a.clk_mhz
    manifest = {
        "design": "delay_probe.sk",
        "clock_mhz": a.clk_mhz,
        "baud": a.clk_mhz * 1e6 / div,
        "marker": "4d000000",
        "clock_words": [2, 3],
        # the rings start at power-on + 4096 clocks; the simulation measures from a little later
        "start_ns": round((4096 + 256) * period_ns, 3),
        "rings": rings,
    }
    (out / "delay_probe.json").write_text(json.dumps(manifest, indent=2) + "\n")
    print(f"wrote {out / 'delay_probe.sk'}, {out / 'hex_report.sk'} (baud divider {div}), "
          f"{out / 'delay_probe.json'} ({len(rings)} rings)")


if __name__ == "__main__":
    main()

#!/usr/bin/env python3
"""Fit a device's delay calibration from ring oscillators measured on a chip.

  python3 fit_calibration.py --skalp skalp --groups groups_nexus.json \\
      --run BUILD delay_probe.json capture.txt [capture2.txt ...] [--run ...] \\
      --out silicon_delays.json

Each `--run` is one place-and-route build of the probe (`gen_probe.py`), its manifest, and one
or more captures of its UART lines (`skalp uart`), one per load of the bitstream. For every ring
the script takes its frequency on the chip from the captures, and its period in the timed
simulation of the same build (`skalp sim BUILD --corner calibrated:FILE`, watching the ring).

A ring's period is a sum of delays, and the calibration scales each kind of delay by its own
factor, so the simulated period is linear in the factors: with every group of delays at factor
1 (the corner itself), then one group at a time at factor 2, the differences give each group's
share of each ring. The factors are then fitted (non-negative least squares, relative error per
ring) so the simulated periods match the chip's, and the result is checked by simulating every
build once more with the fitted file.

`--check FILE` skips the fit and only compares the chip with the simulation under FILE.

The groups file says which delays share a factor (see groups_nexus.json): `wires` and `cells`
are ordered lists of [pattern, group] (the first matching pattern wins, as in the calibration
file), and `clock` names the group of the clock network's wires. Groups that no ring tells
apart from each other cannot be fitted separately; the script says so.
"""
import argparse
import concurrent.futures
import datetime
import itertools
import json
import os
import re
import subprocess
import sys
import tempfile
from pathlib import Path

try:
    import numpy as np
except ImportError:  # pragma: no cover
    sys.exit("fit_calibration.py needs numpy")

LINE = re.compile(r"^\s*((?:[0-9a-f]{8}\s+){15}[0-9a-f]{8})\s*$")


# ------------------------------------------------------------------------------------ chip
def read_capture(path, manifest):
    """Each ring's frequency (MHz) in one capture, and the spread of its rate between lines."""
    lines = []
    for text in Path(path).read_text(errors="replace").splitlines():
        m = LINE.match(text)
        if m:
            words = [int(x, 16) for x in m.group(1).split()]
            if words[0] == int(manifest["marker"], 16):
                lines.append(words)
    if len(lines) < 3:
        raise SystemExit(f"{path}: fewer than three report lines")
    hi, lo = manifest["clock_words"]
    clocks = [(w[hi] << 32) | w[lo] for w in lines]
    out = {}
    for ring in manifest["rings"]:
        bits = ring.get("counter_bits", 32)
        k = ring["word"]
        deltas = [((b[k] - a[k]) % (1 << bits), cb - ca)
                  for a, b, ca, cb in zip(lines, lines[1:], clocks, clocks[1:]) if cb > ca]
        rates = sorted(d / c for d, c in deltas)
        median = rates[len(rates) // 2]
        if median == 0:
            out[ring["name"]] = (None, 0.0)
            continue
        # a line the UART garbled gives one or two wild intervals: leave those out
        kept = [(d, c) for d, c in deltas if abs(d / c - median) <= 0.5 * median]
        dropped = len(deltas) - len(kept)
        if dropped > len(deltas) // 10:
            raise SystemExit(f"{path}: {ring['name']}: {dropped} of {len(deltas)} intervals are off")
        total = sum(d for d, _ in kept)
        span = sum(c for _, c in kept)
        rates = [d / c for d, c in kept]
        f = total / span * manifest["clock_mhz"]
        out[ring["name"]] = (f, (max(rates) - min(rates)) / (total / span))
    return out


def chip_frequencies(run, manifest, spread_warn, leave_out):
    per = [read_capture(c, manifest) for c in run["captures"]]
    result = {}
    for ring in manifest["rings"]:
        name = ring["name"]
        if name in leave_out:
            continue
        loads = [i for i in range(len(per)) if (name, i + 1) not in leave_out]
        if not loads:
            continue
        fs = [per[i][name][0] for i in loads]
        if any(f is None for f in fs):
            print(f"  {run['label']}:{name}: did not run in some capture, left out")
            continue
        for i in loads:
            c, p = run["captures"][i], per[i]
            if p[name][1] > spread_warn:
                print(f"  warning: {run['label']}:{name} changed rate by {100 * p[name][1]:.1f}% "
                      f"within {c} (a ring that jumps to another mode should be left out)")
        mean = sum(fs) / len(fs)
        if (max(fs) - min(fs)) / mean > spread_warn:
            print(f"  warning: {run['label']}:{name} differs by "
                  f"{100 * (max(fs) - min(fs)) / mean:.1f}% between loads: "
                  + ", ".join(f"{f:.3f}" for f in fs) + " MHz")
        result[name] = mean
    return result


# ------------------------------------------------------------------------------ simulation
def watch_nets(build, manifest):
    routed = json.loads((Path(build) / "design_routed.json").read_text())
    nets = routed["nets"]
    names = [n["name"] for n in nets]
    watch = {}
    for ring in manifest["rings"]:
        if "watch_latch" in ring:
            suffix = "." + ring["watch_latch"] + ".lat0"
            cells = [c for c in routed["cells"] if c["path"].endswith(suffix)]
            if len(cells) != 1:
                raise SystemExit(f"{build}: {len(cells)} latch cells named *{suffix}")
            watch[ring["name"]] = names[cells[0]["outputs"][0]]
        else:
            bus = re.compile(r"^(.*\.)?" + re.escape(ring["watch_bus"]) + r"\[0\]$")
            hits = [n[: -len("[0]")] for n in names if bus.match(n)]
            if len(hits) != 1:
                raise SystemExit(f"{build}: {len(hits)} buses named {ring['watch_bus']}")
            watch[ring["name"]] = hits[0]
    return watch


def simulate(skalp, build, calibration, manifest, watch, window_us, extra):
    """Each ring's period (ns) in the timed simulation of `build` under `calibration`."""
    start = manifest["start_ns"]
    with tempfile.NamedTemporaryFile("w", suffix=".json", delete=False) as f:
        json.dump(calibration, f)
        cal = f.name
    try:
        cmd = [skalp, "sim", str(build), "--corner", f"calibrated:{cal}",
               "-d", f"{start + window_us * 1e3:.0f}ns", "--watch-from", f"{start:.0f}ns", *extra]
        for net in watch.values():
            cmd += ["--watch", net]
        res = subprocess.run(cmd, capture_output=True, text=True)
    finally:
        os.unlink(cal)
    if res.returncode != 0:
        raise SystemExit(f"{' '.join(cmd)}\n{res.stdout[-2000:]}{res.stderr[-2000:]}")
    text = res.stdout
    periods = {}
    by_ring = {r["name"]: r for r in manifest["rings"]}
    for name, net in watch.items():
        m = re.search(r"^" + re.escape(net) + r" \((\d+) changes\):\n((?:\s+[0-9.]+ ns\s+\S+\n)*)",
                      text, re.M)
        times = [float(t) for t in re.findall(r"([0-9.]+) ns", m.group(2))] if m else []
        k = by_ring[name]["changes_per_period"]
        if len(times) < 2 * k + 1:
            raise SystemExit(f"{build}: {net} ({name}) changed {len(times)} times in the window; "
                             "the ring does not run in simulation, or the window is too short")
        periods[name] = (times[-1] - times[0]) / ((len(times) - 1) / k)
    return periods


# ------------------------------------------------------------------------------ calibration
def calibration_file(groups, factors, corner):
    """The calibration (as a dict) with every group at its factor (groups absent: 1)."""
    f = lambda g: round(float(factors.get(g, 1.0)), 4)  # noqa: E731
    cal = {"corner": corner,
           "wires": [{"class": p, "factor": f(g)} for p, g in groups["wires"]],
           "cells": [{"cell": p, "factor": f(g)} for p, g in groups["cells"]]}
    if groups.get("clock"):
        cal["clock"] = {"factor": f(groups["clock"])}
    return cal


def group_names(groups):
    names = []
    for _, g in groups["wires"] + groups["cells"] + ([[None, groups["clock"]]] if groups.get("clock") else []):
        if g not in names:
            names.append(g)
    return names


def nnls(A, y):
    """Non-negative least squares by trying every subset of columns (a handful of groups)."""
    k = A.shape[1]
    best = None
    for m in range(1, k + 1):
        for sub in itertools.combinations(range(k), m):
            x, *_ = np.linalg.lstsq(A[:, sub], y, rcond=None)
            if (x < 0).any():
                continue
            r = float(np.sum((A[:, sub] @ x - y) ** 2))
            if best is None or r < best[0]:
                best = (r, sub, x)
    x = np.zeros(k)
    x[list(best[1])] = best[2]
    return x


def report(rows, title):
    print(title)
    errs = []
    for label, sil, sim in rows:
        e = 100 * (sim - sil) / sil
        errs.append(e)
        print(f"  {label:28s} chip {1e3 / sil:8.3f} MHz  sim {1e3 / sim:8.3f} MHz  period {e:+6.2f}%")
    rms = float(np.sqrt(np.mean(np.square(errs))))
    print(f"  rms {rms:.2f}%, worst {max(errs, key=abs):+.2f}%")
    return rms, errs


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--skalp", default="skalp", help="the skalp binary (with the nexus-real feature)")
    ap.add_argument("--run", action="append", nargs="+", required=True, metavar="ARG",
                    help="BUILD MANIFEST CAPTURE [CAPTURE ...] (repeatable)")
    ap.add_argument("--groups", help="groups file (see groups_nexus.json); needed to fit")
    ap.add_argument("--corner", default="slow", help="the corner the factors scale (default slow)")
    ap.add_argument("--check", help="only compare the chip with the simulation under this calibration")
    ap.add_argument("--out", help="where to write the fitted calibration (JSON)")
    ap.add_argument("--leave-out", action="append", default=[], metavar="BUILD:RING[:N]",
                    help="leave a ring out of the fit, or only its measurement in the Nth capture "
                         "(a load where it ran in another mode, say); BUILD is the build directory's name")
    ap.add_argument("--window-us", type=float, default=30.0, help="simulated window per ring (us)")
    ap.add_argument("--spread-warn", type=float, default=0.02, help="warn when a rate varies more")
    ap.add_argument("--jobs", type=int, default=min(4, os.cpu_count() or 1), help="simulations at once")
    ap.add_argument("--sim-arg", action="append", default=[], help="extra argument for skalp sim")
    ap.add_argument("--note", default="", help="device and conditions, for the file's `measured`")
    a = ap.parse_args()
    if not a.check and not (a.groups and a.out):
        ap.error("give --groups and --out to fit, or --check FILE")

    runs = []
    for spec in a.run:
        if len(spec) < 3:
            ap.error("--run takes BUILD MANIFEST CAPTURE [CAPTURE ...]")
        build, manifest_path, *captures = spec
        manifest = json.loads(Path(manifest_path).read_text())
        run = {"build": build, "manifest": manifest, "captures": captures,
               "label": Path(build).resolve().name}
        print(f"{run['label']}: {len(captures)} capture(s)")
        leave_out = set()
        for lo in a.leave_out:
            b, r, *n = lo.split(":")
            if b == run["label"]:
                leave_out.add((r, int(n[0])) if n else r)
        run["chip"] = chip_frequencies(run, manifest, a.spread_warn, leave_out)
        run["watch"] = {k: v for k, v in watch_nets(build, manifest).items() if k in run["chip"]}
        runs.append(run)

    def sim_all(cal):
        with concurrent.futures.ThreadPoolExecutor(a.jobs) as pool:
            futs = [pool.submit(simulate, a.skalp, r["build"], cal, r["manifest"], r["watch"],
                                a.window_us, a.sim_arg) for r in runs]
            return [f.result() for f in futs]

    def rows(periods):
        return [(f"{r['label']}:{name}", 1e3 / r["chip"][name], p[name])
                for r, p in zip(runs, periods) for name in r["chip"]]

    if a.check:
        text = Path(a.check).read_text()
        if a.check.endswith(".toml"):
            sys.exit("--check takes the calibration as JSON")
        report(rows(sim_all(json.loads(text))), f"chip vs simulation under {a.check}:")
        return

    groups = json.loads(Path(a.groups).read_text())
    names = group_names(groups)
    print(f"simulating {len(runs)} build(s) with {len(names)} group(s) at factor 1, then each at 2")
    jobs = [{}] + [{g: 2.0} for g in names]
    with concurrent.futures.ThreadPoolExecutor(a.jobs) as pool:
        futs = [[pool.submit(simulate, a.skalp, r["build"], calibration_file(groups, j, a.corner),
                             r["manifest"], r["watch"], a.window_us, a.sim_arg) for r in runs] for j in jobs]
        basis = [[f.result() for f in fs] for fs in futs]
    labels, A, rest, target = [], [], [], []
    for i, r in enumerate(runs):
        for name in r["chip"]:
            base = basis[0][i][name]
            share = [basis[1 + g][i][name] - base for g in range(len(names))]
            labels.append(f"{r['label']}:{name}")
            A.append(share)
            rest.append(base - sum(share))
            target.append(1e3 / r["chip"][name])
    A, rest, target = np.array(A), np.array(rest), np.array(target)

    print("\neach group's share of each ring's period at the corner (ns):")
    print("  " + " " * 28 + "".join(f"{g[:9]:>10s}" for g in names))
    for lab, row in zip(labels, A):
        print(f"  {lab:28s}" + "".join(f"{v:10.2f}" for v in row))
    used = [g for g in range(len(names)) if np.any(np.abs(A[:, g]) > 1e-3)]
    for g in range(len(names)):
        if g not in used:
            print(f"warning: no ring passes delays of group {names[g]}; it keeps the corner's delays")
    W = A[:, used] / target[:, None]
    sv = np.linalg.svd(W, compute_uv=False)
    if len(used) > len(labels) or sv[-1] < 1e-6 * sv[0]:
        _, _, vt = np.linalg.svd(W)
        tied = [names[used[j]] for j in np.argsort(-np.abs(vt[-1]))[:3]]
        print(f"warning: the rings do not tell groups {', '.join(tied)} apart; "
              "merge them in the groups file or add rings that separate them")
    y = (target - rest) / target
    x = nnls(W, y)
    factors = {names[g]: float(v) for g, v in zip(used, x)}
    # how well the rings pin each factor down: its standard error from the residuals (a group
    # the fit put at 0 is held there); large ones mean the rings hardly tell that group apart
    free = [j for j in range(len(used)) if x[j] > 0]
    dof = len(labels) - len(free)
    err = {}
    if dof > 0:
        sigma2 = float(np.sum((W @ x - y) ** 2)) / dof
        cov = np.linalg.pinv(W[:, free].T @ W[:, free]) * sigma2
        err = {names[used[j]]: float(np.sqrt(max(cov[i, i], 0.0))) for i, j in enumerate(free)}
    print("\nfitted factors of the " + a.corner + " corner (with standard errors where the rings "
          "leave freedom to estimate them):")
    for g, v in factors.items():
        note = f" +- {err[g]:.3f}" if g in err else (" (held at 0: check this group)" if v == 0 else "")
        print(f"  {g:10s} {v:.3f}{note}")
    if dof <= 0:
        print("warning: no more rings than fitted groups: the fit is exact and says nothing of its errors")

    cal = calibration_file(groups, factors, a.corner)
    rms, errs = report(rows(sim_all(cal)), "\nchip vs simulation with the fitted calibration:")
    cal["comment"] = (f"Delays on silicon as factors of the {a.corner} corner, by routing switch class "
                      "and cell type, fitted from ring oscillators measured on a chip (SKALP "
                      "examples/delay_calibration).")
    cal["measured"] = {
        "device": a.note or "(fill in: device, board, temperature, supply)",
        "date": datetime.date.today().isoformat(),
        "method": f"{len(labels)} rings in {len(runs)} build(s) of gen_probe.py, each measured on the "
                  "chip against the board clock and simulated with the build's routed delays; one factor "
                  "per group of groups file, fitted by non-negative least squares on relative period error.",
        "results": f"rings match within {rms:.2f}% rms (worst {max(errs, key=abs):+.2f}%)",
        "rings": {lab: round(e, 2) for lab, e in zip(labels, errs)},
    }
    Path(a.out).write_text(json.dumps(cal, indent=2) + "\n")
    print(f"\nwrote {a.out}")


if __name__ == "__main__":
    main()

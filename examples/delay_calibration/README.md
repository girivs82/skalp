# Calibrating a device's delays against a chip

Timed simulation (`skalp sim <build dir>`) runs a placed and routed design with
its routed delays, at one of the vendor's corners. The fast and slow corners
bracket silicon, but a real part does not sit at one place between them. On
the one family measured so far (Lattice CertusPro-NX, one chip at room
temperature), logic ran at about half the slow corner's delay, short and
long wires well below the slow corner, and medium wires right at it. A single
fraction between the corners was off by 5 to 10% on designs whose routes used
more of one kind of wire than the rings it was fitted on.

So SKALP can scale delays **by kind**: each routing switch by its delay
class, each cell's own delays by its cell type, and the clock network's wires.
The factors live in the device database, in
`<family>/timing/silicon_delays.json`. Place and route copies that file into
the build as `design_calibration.json`, and

```
skalp sim <build dir> --corner calibrated
```

simulates with it (`--corner calibrated:FILE` takes another file). The file
format is described in `skalp_lir::delay_calibration`; in short:

```json
{
  "corner": "slow",
  "wires": [ { "class": "*span6*", "factor": 0.76 }, { "class": "*", "factor": 0.54 } ],
  "cells": [ { "cell": "CCU2", "factor": 0.62 }, { "cell": "*", "factor": 0.54 } ],
  "clock": { "factor": 0.95 },
  "measured": { "device": "...", "date": "...", "method": "...", "results": "..." }
}
```

Each `factor` multiplies the named corner's delay. The first pattern that
matches wins. A cell or clock entry may give `between` instead, which is a
fraction of the way from the `from` corner (default `fast`) to the named one.

This directory measures a chip and fits such a file:

- `gen_probe.py` generates the probe design: ring oscillators of each kind of
  delay, counted on the board clock and reported over a UART.
- `hex_report.sk` is the UART reporter it uses.
- `fit_calibration.py` takes a probe build and its captures, and fits the
  factors with the timed simulation of the same build.
- `groups_nexus.json` lists which delays share a factor, for Lattice Nexus.

## The probe

```
python3 gen_probe.py --clk-pin P24 --clk-mhz 125 --uart-pin M9 --out probe
```

This writes `delay_probe.sk`, `hex_report.sk` (with its baud divider set for
the clock) and `delay_probe.json`, the manifest the fit reads. Give your own
board's clock input and a UART transmit pin that the board's USB serial
bridge reads. Use only pins the board leaves free for the FPGA to drive.

The rings are:

| ring | what it passes |
|------|----------------|
| `ro` | 63 LUT latches in a loop, placed freely |
| `rctr`, `rnw`, `rne`, `rsw`, `rse` | the same, pinned to small regions at the centre and corners of the die |
| `lw` | 8 latch stages pinned alternately to two opposite corners, so every hop is a long wire |
| `carry` | a latch, then 8 carry chains (the carry out of a 64-bit add) |
| `mult` | a latch, then 12 multipliers (one bit of an 18x18 product) |
| `dram` | a latch, then 64 distributed-RAM reads |
| `clk` | two flip-flops toggling each other through the clock network |

Each ring has one inversion and starts 4096 board clocks after power-on.
Every 2^18 board clocks the probe sends one line of sixteen 8-digit hex words:
`4d000000`, a line number, the board clocks since power-on (two words), and
each ring's rising edges, in the manifest's order.

The pinned rings check that place on the die does not matter (it did not on
the family measured so far). The others give the different kinds of delay in
different mixes, which is what lets the fit tell them apart.

## Procedure

1. **Build** the probe with the family's current database:
   `skalp synth delay_probe.sk -d <device> -f -o build`. The build must carry
   each wire's split by switch class (`design_delays.json` lists `classes`);
   any build from a SKALP with `--corner calibrated` does.
2. **Measure.** Load the bitstream into SRAM, and capture its UART for a
   minute: `skalp uart > uart1.txt`. Load it again and capture again into
   `uart2.txt`. A second load matters. On the family measured so far, a
   latch ring sometimes came up in a faster mode (12 to 28% faster) on one
   load and not on another. The fit warns where loads disagree.
3. **Fit**, with a skalp binary built with the device's feature
   (`--features nexus-real` for Nexus):

   ```
   python3 fit_calibration.py --skalp skalp --groups groups_nexus.json \
       --run build probe/delay_probe.json uart1.txt uart2.txt \
       --note "LFCPNX-100-9, Versa board, room temperature, nominal supply" \
       --out silicon_delays.json
   ```

   The fit simulates the build once with every group of delays at factor 1
   (the corner itself), then once per group with that group at 2. Delays add
   along a ring, so the differences give each group's share of each ring's
   period. The table it prints shows those shares. It then fits one factor
   per group, by non-negative least squares on each ring's relative error,
   simulates the build once more with the result, and prints chip against
   simulation for every ring.

   Leave a ring out with `--leave-out build:rctr`, or only one load's
   measurement of it with `--leave-out build:rctr:2`. Several `--run`s (other
   builds of the probe, or other designs with rings and a manifest) all go
   into one fit.
4. **Check** the result. Each ring should match within a few percent. A
   ring that stays well off with the others fitted means a kind of delay the
   groups do not separate. Split that group and add a ring that weighs it
   differently.
5. **Install** the file as `<family>/timing/silicon_delays.json` in the
   device database. Fill in `measured` with the device, board, conditions
   and date. Builds made after that carry it. To compare any file with the
   chip again: `fit_calibration.py --check FILE --run ...`.

## Groups

`groups_nexus.json` maps every switch class and cell type to a group. All
delays in a group share one factor. The groups are:

- **`stage`**: the LUT's own delay and the switches into and out of it. Every
  latch ring passes these three equally often, so separate factors could not
  be fitted.
- **`span0`, `span1`, `span2`, `span6`**: the routing wires by length.
- **`carry`, `mult`, `dram`**: the hard blocks' own delays.
- **`clocked`**: the clock network's wires and the flip-flops' own delays. Only
  the clock ring passes either, so they share a factor.

The fit warns when the rings do not tell two groups apart. Then either merge
the groups or add a ring that passes one of them more than the other. For
another family, write its groups from the switch classes its builds list (the
`classes` of `design_delays.json`) and the cell types its netlists use.

## What the probe does not see

The rings run on a quiet die. A large design running at full rate draws
enough current to slow the whole die. On the family measured so far, about
50 thousand busy cells slowed every ring by 1.85%, in the same way all over
the die, and at once rather than as it warmed. Measure that for your own
design with a ring placed beside it, with the design running and stopped, and
give it to the simulation as `--derate 1.0185`. It belongs to the design and
its workload, not to the device, so it stays out of the calibration file.

The figures are from one chip at one temperature and supply. Delays grow
when the chip is hot or the supply is low, and they differ from chip to chip.
Use the calibrated corner to predict what a board does. Sign timing off
against the vendor's corners.

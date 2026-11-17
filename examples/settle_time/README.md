# Measuring a device family's settle time

SKALP builds the step control of self-timed blocks and the mutual exclusion
of `match arbitrate` from one element: a latch in one LUT, with its output
fed back into its own input. Two events that are independent of each other
race into it. When they arrive close together, the latch can hang between 0
and 1 for a while before it settles. So every decision waits a
**resolution time** before acting on the latch. The wait is sized so that a
failure is expected less than once in 1000 years per element:

```
MTBF(t) = exp(t / tau) / (T0 * f1 * f2)
```

Here `t` is the wait, `tau` is the element's resolution time constant, `T0`
is its metastability window, and `f1` and `f2` are the event rates (up to
200 MHz each).

`tau` depends on the silicon, so it has to be measured on a board. A
family's figure lives in its device database, in
`<family>/timing/metastability.json`. Where a family has no figure, SKALP
does three things:

- It assumes tau 300 ps, which is ten times the bound measured on the one
  family characterised so far. That gives waits of about 12.5 ns.
- It warns that the build's async timing is not signed off.
- It still checks every wait with routed delays.

This directory has what you need to measure a new family:

- `settle_race.sk` is the race test design.
- `skalp synth --settle-wait TIME` builds it with every decision wait cut
  short on purpose, so failures become frequent enough to count.

## The race test

`settle_race.sk` runs a self-timed source and sink, connected by a stream,
under a clocked checker. The checker works in a loop:

1. It runs the pair for a pseudo-random 16 to 1039 clocks.
2. It drops the run request and waits for both blocks to stop.
3. It starts them again.

At a 125 MHz clock that is about 200 thousand stops a second. Each stop can
land just as a block's `ready` rises. That races the request against
`ready` in the block's step latch.

A latch that settles after the wait ends gives a **partial step**. The sink
counts values that do not follow the previous value. Its count is the
number of partial steps.

Random run lengths matter. With fixed run lengths, the stop lands at the
same point of the blocks' step every time, and the test reads zero.

Before building, add your board's pins to `clk` and the eight LEDs, for
example on the Lattice CertusPro-NX Versa board:

```skalp
in clk: clock @ { pin: "P24", io_standard: "LVCMOS33" }
out led0: bit @ { pin: "R5", io_standard: "LVCMOS33" }
...
```

Use only pins the board leaves free for the FPGA to drive. The LEDs are
treated as active low; invert `leds` if your board lights them high.

## Procedure

1. **Build at the shipped wait** with a plain `skalp synth settle_race.sk -d
   <device> -f`. Load it, and make sure it reads zero for several minutes.
   A non-zero count here means the design or the flow is wrong, not the
   latch.
2. **Build a sweep** of shorter waits:
   `skalp synth settle_race.sk -d <device> -f --settle-wait 0.5ns -o w05`,
   and the same for 1ns, 1.5ns, 2ns, 3ns and so on, down to the shortest
   wait SKALP can build. Each build's report gives the wait it really got:
   the `delay` of its `step decision` lines. Use that figure, not the
   requested one, because a chain cannot be shorter than one buffer.
   Where the family has several delay corners, take the fast-corner figure,
   since the database's tau is in the units of the family's fastest delay
   tables.
3. **Count.** Load each build (into SRAM, never flash). LED7 toggles every
   2^30 clocks. After each toggle, LEDs 6:0 show that window's count as
   `{msb position p (5 bits), next two bits m}`, meaning about
   `2^p * (1 + m/4)` failures; all off means none. Record several windows
   per build. All LEDs lit means a stop never completed: the build is
   broken.
4. **Work out the stops** in each window: the window's clock count divided
   by the mean loop length. The mean loop is the mean run of about 542
   clocks, plus the clocks a stop takes, which you can read from a timed
   simulation of the build (`skalp sim <build dir> --clock clk=8ns --watch
   ...`).

## From counts to tau

The chance that a stop fails after a wait `t` is at most
`exp(-t / tau)`, because the chance at zero wait is at most 1. So a build
with wait `t`, `S` stops and `N` failures bounds tau:

```
tau <= t / ln(S / N)
```

Take the tightest bound over the sweep. A build with no failures still
bounds tau, with `N = 1`. A longer run makes that bound tighter.

The family measured so far had 2 failures in 6.9 million stops at a 0.45 ns
wait, and none at 1.1 ns and above. That gives `tau <= 0.45 / ln(6.9e6 / 2)`,
about 30 ps.

Then add the figure to the family's database:

```json
{
  "elements": {
    "lut4_feedback_latch": {
      "tau_ps": 30.0,
      "pvt_margin": 3.0,
      "t0_ps": 1000.0,
      "kind": "upper_bound",
      "measured": {
        "device": "part, board, temperature, supply",
        "date": "YYYY-MM-DD",
        "method": "settle_race.sk, wait sweep",
        "results": "failures and stops per build"
      }
    }
  }
}
```

The fields are:

- **`tau_ps`**: the measured bound.
- **`pvt_margin`**: covers what one chip at room temperature and nominal
  supply does not show. tau worsens faster than gate delay at low supply
  and cold, and other chips and speed grades differ. Use 3 unless you have
  measured across those.
- **`t0_ps`**: hard to measure directly. 1 ns is a conservative
  assumption.

With the figure in place, SKALP sizes that family's waits from it and stops
warning.

## Keep the latch in one LUT

The same latch built as a loop of three LUTs gave hundreds of partial steps
a second, even at a 3.2 ns wait (tau of roughly 0.7 to 1 ns). Routing
between cells adds delay inside the loop, which slows its resolution by an
order of magnitude. SKALP always builds these elements in one LUT. A family
whose LUTs cannot feed back into themselves needs its own element, measured
the same way.

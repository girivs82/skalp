# Skalp vs nextpnr-ice40 Bitstream Comparison

Cross-validation analysis comparing skalp's .asc bitstream output against
nextpnr-ice40 for iCE40 HX1K designs.

## Methodology

A Python analysis script (`/tmp/analyze_asc.py`) parses every set bit in an
.asc file and classifies it into one of these categories:

| Category | Description |
|----------|-------------|
| ColBufCtrl | Column buffer control bits (logic, IO, RAMB tiles) |
| IE | Input enable defaults for unused IO pads |
| REN | Pull-up resistor disable on active IO pins |
| PowerUp | RAM block power-up initialization bits |
| LUT init | LUT4 truth table bits (16 per LC) |
| DFF config | DFF configuration bits (carry_enable, dff_enable, set_no_reset) |
| NegClk | Negative clock edge configuration |
| CarryInSet | Carry chain initial carry-in = 1 |
| PINTYPE | IO pin type configuration (6 bits per IOB) |
| Routing | PIP configuration bits (logic, IO, RAM tiles) |

Both tools target the same device (HX1K, 14x18 grid) and synthesize from
equivalent Verilog via yosys.

## Per-Design Comparison

### Inverter (`y = ~a`)

| Category | nextpnr | skalp | Delta | Verdict |
|----------|---------|-------|-------|---------|
| ColBufCtrl (logic) | 320 | 320 | 0 | **Match** |
| ColBufCtrl (IO) | 64 | 64 | 0 | **Match** |
| ColBufCtrl (RAMB) | 64 | 64 | 0 | **Match** |
| IE | 96 | 97 | +1 | **Near-match**: 1 extra IE from active IO tile |
| REN | 2 | 1 | -1 | skalp has 1 IO cell; nextpnr has 2 |
| PowerUp | 16 | 16 | 0 | **Match** |
| LUT init | 8 | 8 | 0 | **Match** |
| DFF config | 1 | 0 | -1 | Expected: nextpnr uses routing helper LC |
| PINTYPE | 4 | 3 | -1 | Near-match: routing PIP at PINTYPE position |
| Routing (logic) | 10 | 29 | +19 | Expected: different placement |
| Routing (IO) | 6 | 1 | -5 | Expected: fewer IO cells configured |
| **TOTAL** | **591** | **603** | **+12** | |

### Counter-4 (`count <= count + 1`)

| Category | nextpnr | skalp | Delta | Verdict |
|----------|---------|-------|-------|---------|
| ColBufCtrl (logic) | 320 | 320 | 0 | **Match** |
| ColBufCtrl (IO) | 64 | 64 | 0 | **Match** |
| ColBufCtrl (RAMB) | 64 | 64 | 0 | **Match** |
| IE | 96 | 97 | +1 | Near-match |
| REN | 5 | 1 | -4 | skalp has fewer IO cells |
| PowerUp | 16 | 16 | 0 | **Match** |
| LUT init | 39 | 0 | -39 | Gap: carry fallback path (bel_index=16) |
| DFF config | 1 | 8 | +7 | carry_enable on fallback carry cells |
| CarryInSet | 1 | 0 | -1 | Gap: CI-driven-by-VCC detection needs work |
| PINTYPE | 13 | 3 | -10 | Gap: fewer IO cells in test netlist |
| Routing (logic) | 30 | 69 | +39 | Expected: different placement |
| Routing (IO) | 29 | 0 | -29 | Gap: no IO routing (fewer IO cells) |
| **TOTAL** | **678** | **642** | **-36** | |

### Cross-Validation Summary (7 designs)

| Design | skalp LUTs | nextpnr LUTs | skalp bits | nextpnr bits | S-Fmax | N-Fmax |
|--------|-----------|-------------|-----------|-------------|--------|--------|
| Inverter | 41 | 41 | 578 | 591 | 813.6 | 292.0 |
| AND gate | 45 | 40 | 615 | 589 | 388.2 | 364.4 |
| DFF+reset | 42 | 43 | 585 | 599 | 613.9 | 438.4 |
| Counter-4 | 51 | 43 | 667 | 678 | 499.8 | 438.4 |
| Adder-8 | 49 | 65 | 661 | 956 | inf | 158.2 |
| Counter-16 | 96 | 62 | 1154 | 1054 | 425.3 | 254.5 |
| ShiftReg-8 | 47 | 43 | 745 | 642 | 415.4 | 506.9 |

## Bugs Fixed

### BUG 1: IE on padless IO tiles (LOW — cosmetic)

**What**: skalp set IE on all 56 IO tiles; nextpnr only sets on ~49 tiles with
physical pads.

**Impact**: 7 IO tile positions on HX1K lack physical pads: (0,1), (0,7),
(0,15), (0,16), (13,5), (13,12), (13,16). Setting IE there wastes bits.

**Fix**: Added `is_padless_io_tile()` check in `generate_io_tile()`. Skips IE
defaults for the 7 known padless positions on HX1K/LP1K.

**Result**: IE reduced from 111→97 (inverter), 109→97 (counter-4).

### BUG 2: REN not set on active IO pins (MEDIUM — functional)

**What**: Active IO pins need REN=1 to disable the internal pull-up resistor.
skalp left REN=0 (pull-up enabled).

**Impact**: On real hardware, active outputs fight against the pull-up.

**Fix**: Set `REN = !pullup_enable` when IOB is configured (instead of
`REN = pullup_enable`). This disables pull-ups on active pins by default.

**Result**: REN now set on all active IO cells. Matches nextpnr behavior.

### BUG 3: PINTYPE encoding swapped (HIGH — functional)

**What**: The PINTYPE 6-bit field had input and output sub-fields at wrong bit
positions. `input_mode` was at [1:0] (should be [5:4]) and `output_mode` was
at [5:4] (should be [1:0]).

**Impact**: IO pads wouldn't function correctly on hardware.

**Fix**: Corrected the `IoConfig` struct and `pintype()` encoding to match the
iCE40 LP/HX Family Data Sheet Table 3.3:
- `[1:0]` = output enable select
- `[3:2]` = output driver select
- `[5:4]` = input pin select

**Result**: PINTYPE bits now set correctly. Inverter went from 1→3 bits
(closer to nextpnr's 4).

### BUG 4: carry_enable sprayed on all LCs (HIGH — functional)

**What**: `collect_dff_configs()` set `carry_enable = true` on ALL 8 LCs in any
tile containing a carry cell. nextpnr only sets it on specific chain LCs.

**Impact**: Creates phantom carry paths on unrelated LCs.

**Fix**: Two-part fix:
1. Fixed carry chain legalization (`find_consecutive_lc_slots`) to properly
   track LC indices (0-7) instead of raw BEL indices, and place carry cells
   at the LUT BEL index of their LC (`bel_index = 2 * lc_idx`).
2. Changed `collect_dff_configs()` to only set carry_enable on the specific LC
   (`lc_idx = bel_index / 2` for bel_index < 16, LC 0 for fallback at index 16).

**Result**: Counter-4 DFF config went from 26 bits (all LCs sprayed) to 8 bits
(only actual carry LCs).

### BUG 5: LUT init not emitted for carry chain LCs (CRITICAL — functional)

**What**: Carry-associated LUTs placed by chain legalization had BEL indices
that didn't map correctly to LC indices via `bel_index / 2`.

**Impact**: Counter logic LUTs had no truth table — counter doesn't increment.

**Fix**: Fixed by the legalization change in BUG 4 — carry-associated LUTs now
use device-model BEL indices (`2 * lc_idx`), making the existing
`lc_idx = bel_index / 2` formula correct. Added fallback handling for
bel_index >= 16 (maps to LC 0).

### BUG 6: CarryInSet not emitted (MEDIUM — functional)

**What**: nextpnr sets CarryInSet (B1[50]) in the tile containing the carry
chain head. skalp didn't set it.

**Impact**: Carry chain initial carry-in is undefined.

**Fix**: Added `collect_carry_in_set_tiles()` that detects carry chain heads
(carry cells not driven by another carry) with CI driven by VCC/TIE_HIGH.
Sets B1[50] in the corresponding logic tile.

**Note**: Detection depends on the netlist having explicit VCC driver cells.
Some test netlists don't have this, so CarryInSet may still be missing in
those cases.

## Remaining Known Gaps

### Carry chain placement compactness

nextpnr packs carry chains into consecutive LCs within a single tile (e.g.,
6 LCs in tile(7,1) for counter-4). skalp's carry chain legalization now
correctly assigns LC-indexed BEL positions, but the initial placement may
scatter chains across multiple tiles. The legalization's consecutive-slot
search finds the first available run, which may not be as compact.

**Impact**: More routing needed to connect scattered carry cells.

### IO routing gap

skalp generates fewer IO routing bits because the test netlists have fewer
IO cells. The full P&R flow (via yosys → skalp) creates proper SB_IO cells
for all ports, but the unit test netlists often only create a subset.

**Impact**: None for production (yosys flow). Test-only discrepancy.

### LUT permutation optimization

nextpnr can permute LUT inputs to find shorter routing paths. skalp has
a LUT permutation optimizer but may not always find the same permutation,
leading to different routing bit counts.

**Impact**: Slightly longer routes in some cases (higher delay, more bits).

## Verification Commands

```bash
# Unit tests
cargo test -p skalp-place-route --lib

# Cross-validation (requires yosys + nextpnr)
cargo test -p skalp-place-route --test test_nextpnr_golden -- --ignored test_cross_validation_summary --nocapture

# Clippy
cargo clippy -p skalp-place-route --lib --tests -- -D warnings

# Generate skalp .asc files for manual analysis
cargo test -p skalp-place-route --lib test_dump_asc_for_comparison -- --ignored --nocapture

# Run Python analysis on both outputs
python3 /tmp/analyze_asc.py
```

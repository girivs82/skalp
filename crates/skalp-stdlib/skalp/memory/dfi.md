# LPDDR4 over DFI: controller, behavioural PHY, channel model

Three library entities in `skalp::memory` let an LPDDR4 controller be built and checked in
simulation before any vendor PHY is involved:

| Entity | File | Synthesisable | Role |
|---|---|---|---|
| `Lpddr4DfiController` (+ `Lpddr4BankState`) | `lpddr4_dfi_controller.sk` | yes | LPDDR4 controller, one x16 channel, DFI 1:4 |
| `DfiLpddr4BehavPhy` | `dfi_lpddr4_behav_phy.sk` | no | DFI to LPDDR4 pins, write delays, read capture, training evaluation |
| `Lpddr4ChannelModel` | `lpddr4_model.sk` | no | One LPDDR4 channel: storage, JEDEC timing and protocol checks, bus contention |

`tests/fixtures/lpddr4_dfi_sim.sk` wires the three together with a traffic generator, and
`tests/suite/test_lpddr4_dfi_sim.rs` runs it.

## DFI subset

The interface follows DFI 5.x at a 1:4 frequency ratio: one DFI clock (`dfi_clk`) cycle carries
four phases, p0 to p3, one CK each. Only what LPDDR4 with PHY-evaluated read training needs is
present.

| Group | Signals (controller to PHY unless marked) | Notes |
|---|---|---|
| Command | `dfi_cs_pN`, `dfi_address_pN[5:0]`, `dfi_cke_pN`, `dfi_reset_n_pN` | `dfi_address` is CA[5:0]; CS high marks a command's first CK |
| Write data | `dfi_wrdata_en_pN`, `dfi_wrdata_pN[31:0]`, `dfi_wrdata_mask_pN[3:0]` | two beats per phase, the earlier beat in [15:0] |
| Read data | `dfi_rddata_en_pN`; PHY to controller: `dfi_rddata_wN[31:0]`, `dfi_rddata_valid_wN` | `rddata_en` marks the slots data is expected in |
| Status | `dfi_init_start`; PHY to controller: `dfi_init_complete` | |
| Update | `dfi_ctrlupd_req`, PHY: `dfi_ctrlupd_ack`; PHY: `dfi_phyupd_req`, `dfi_phyupd_ack` | |
| Training | `dfi_wrlvl_en`, `dfi_wrlvl_strobe`, `dfi_rdlvl_gate_en`, `dfi_rdlvl_en`, `dfi_wdqlvl_en`; PHY: `dfi_wrlvl_resp[1:0]`, `dfi_rdlvl_gate_resp[1:0]`, `dfi_rdlvl_resp[1:0]`, `dfi_wdqlvl_resp[1:0]` | one response bit per byte lane |

Read data words: `dfi_rddata_wN` holds beats 2N and 2N+1 of both byte lanes, so one BL16 burst
comes back as two dfi_clk cycles of four words.

## Timing contract

- Every DFI phase reaches the pins two dfi_clk cycles after the controller drives it, commands
  and data alike, so slot-to-slot timing on DFI equals DRAM timing.
- CK rises in the middle of each slot; CA and CS change at slot starts.
- Write: with the write CAS-2 in slot c, the first `wrdata_en` slot is c + WL + 3
  (`TPHY_WRLAT_ADJ`). The PHY launches DQS a trained delay after its slot (half a tCK before
  write leveling) and DQ a trained delay after DQS, with a 2 tCK preamble; at the nominal
  leveling point this adds one tCK, which the 3 accounts for.
- Read: with the read CAS-2 in slot c, the nominal first `rddata_en` slot is c + RL + 3
  (`TRDDATA_EN_ADJ`). The real arrival is later by tDQSCK and board delay; the trained read gate
  absorbs it.
- A PHY with other latencies changes `TPHY_WRLAT_ADJ` and `TRDDATA_EN_ADJ`.

## Controller

- At most one command per dfi_clk cycle. 4-CK commands (ACT, RD/WR + CAS-2, MRW, MRR,
  MPC + CAS-2) fill p0 to p3; 2-CK commands (PRE, PREab, REF, ZQ start and latch) fill p0 and p1.
- JEDEC timings are generics in nCK and are converted to DFI cycles between the CK edges the DRAM
  measures them from, so they hold at any CK frequency.
- Init: DFI init handshake, RESET_N low (`T_INIT1_CYC`), CKE low (`T_INIT3_CYC`), CKE high
  (`T_INIT5_CYC`), MRW of MR13, MR1, MR2, MR3, MR11, MR12, MR14 and MR22, ZQ start and latch,
  training, then MRR of MR5 and MR8 (`mr5`, `mr8` outputs).
- Training (PHY evaluation), in this order:
  - write leveling: MRW MR2 with OP7 set, `TWLMRD_NCK`, then `dfi_wrlvl_strobe` every
    `WL_GAP_CYC` cycles with `dfi_wrlvl_en` set until the PHY raises every lane's response (at
    most `WL_MAX_STROBES`), then MRW MR2 back;
  - read gate, then read eye: Read DQ Calibration reads (MPC) with `rdlvl_gate_en` or `rdlvl_en`
    set until every lane responds;
  - write DQ: with `dfi_wdqlvl_en` set, a write to `TRAIN_ADDR` and its read-back in turn until
    every lane responds. The PHY supplies the write data and checks the read data, which does not
    come back to the user port.

  A phase that reaches `TRAIN_MAX_READS` reads without every response sets `train_fail`.
- Operation: one request at a time, in order; open page per bank (row hit: CAS; other row: PRE,
  ACT; closed: ACT); per-bank tRCD, tRAS, tRP, tWR and tRTP in `Lpddr4BankState`; tRRD, tFAW,
  tCCD and tWTR across banks; all-bank refresh every tREFI (PREab, REF, then a controller
  update of `CTRLUPD_CYC` cycles once reads issued before the refresh have come back); while the PHY requests an update no new request is taken, and the window is
  granted once no read or write is on the bus.
- User port (DFI clock): `req_valid`/`req_ready`, `req_write`, `req_addr = {row[16:0],
  bank[2:0], column[9:4]}`, `req_wdata[255:0]` (beat k in [16k+15:16k]); read data in request
  order on `rsp_valid`/`rsp_rdata`.

### Bus safety

The FPGA side must never drive a line the DRAM can drive. The controller guarantees it by
construction:

- a write CAS follows a read CAS by at least RL - WL + tDQSCK(max) + 10 nCK plus
  `RTW_GUARD_NCK`, so the read burst, its postamble and the write preamble cannot overlap;
- `wrdata_en` is raised only for slots that belong to a write command;
- the PHY drives DQ and DQS only in `wrdata_en` slots and their preamble and postamble.

The channel model counts any clock in which both sides drive DQ or DQS (error 26), and the tests
require none.

## Behavioural PHY

- Runs on a fast simulation clock with `CLK_PER_CK` clocks per CK (even, 4 to 16) and makes
  `dfi_clk` itself. It is for simulation only.
- Write path per byte lane: DQS goes out half a tCK plus the lane's write leveling delay
  (`wl_dly0/1`) after its slot, DQ a further write DQ delay (`wdq_dly0/1`) after DQS. Before
  training they are half a tCK and a quarter tCK.
- Write leveling: each strobe sends one DQS pulse with the next delay (0 to `CLK_PER_CK` - 1) and
  samples the DRAM's feedback on DQ at the next strobe. Each lane keeps the first delay whose
  feedback goes from 0 to 1, where its DQS rise meets CK at the DRAM (`wrlvl_fail` if none).
- Write DQ training: the n-th write goes out with write DQ delay n and the PHY's own data (byte
  k of lane 0 is {k, k}, inverted on odd beats; lane 1 is the inverse); its read-back is compared
  with it. After half a tCK of delays each lane takes the middle of the longest passing run, or
  reports `wdq_fail`.
- Read capture per byte lane: the gate opens a trained number of clocks after the first slot of
  each burst, takes 16 beats on DQS edges with a trained sample delay, and closes after 4 tCK if no
  DQS edge comes (`gate_timeouts`). A burst goes back on DFI once both lanes have it.
- Gate training: each lane measures the clocks from the burst's first slot to the first DQS rise
  after at least 1 tCK low (the preamble). `GATE_MATCHES` equal readings in a row set the gate half
  a tCK before that rise (`gate_dly0/1`).
- Read-eye training: each burst is captured with the next sample delay (0 to half a tCK - 1) and
  compared with the Read DQ Calibration pattern (`RDC_PATTERN` = {MR40, MR32}, lane inversion
  `RDC_INV0/1` = MR15/MR20). The lane takes the middle of the longest passing run
  (`rd_dly0/1`), or reports `lvl_fail`.
- PHY updates: with `PHYUPD_EVERY` != 0 it requests a window every `PHYUPD_EVERY` cycles and holds
  it `PHYUPD_LEN` cycles once acknowledged. A DFI cycle carrying a command or data enable while
  the window is granted counts in `upd_violations`. `ctrlupd_count` and `phyupd_count` count completed updates.

## Channel model

- Decodes the JESD209-4 command truth table on CK rising edges and checks command sequencing,
  write DQS timing (tDQSS), DQ setup and hold around DQS edges, DQS_c complement, CA/CS stability,
  and tRCD, tRP, tRAS, tRRD, tFAW, tRFCab, tMRW, tCCD, tWR, tRTP, tWTR, tZQCAL, tZQLAT, refresh
  overdue (9 x tREFI) and bus contention. `errors` counts them; `err_code` and `err_cycle` give the
  first. The codes are listed at the top of `lpddr4_model.sk`.
- Board delay behind CK per byte lane: `WR_SKEW0/1_CLKS` for DQS and DQ, plus
  `WR_DQ_SKEW0/1_CLKS` more for DQ. Each lane's write capture (`Lpddr4WriteLane`) sees its pins
  through that delay; a write whose DQ breaks setup or hold around a DQS edge stores corrupted
  data and counts in `dq_viol` rather than `errors`, so write DQ training can sweep through bad
  delays.
- Write leveling mode (MR2 OP7): on each DQS rise the model samples CK and drives it back on DQ
  `TWLO_NCK` later, driving DQ for as long as the mode is on; contention checks cover it.
- Reads, MRR and Read DQ Calibration drive DQS with a static 2 tCK preamble ending RL x tCK +
  `DQSCK_CLKS` after CAS-2, with DQ lagging DQS by `DQ_SKEW_CLKS`.
- Storage keeps one burst per (bank, column[9:4]), tagged with its row. `aliased` counts reads of
  a different row at the same place (a model limit), `unwritten` reads of places never written.

## Not covered yet

- Write DQ training through the MPC FIFO commands (WR-FIFO/RD-FIFO); the training here uses
  ordinary writes and reads to `TRAIN_ADDR`. The model reports unknown MPC opcodes as error 25.
- Per-bit (deskew) delays; the delays here are per byte lane.
- Data mask / DBI (MR13 DMD is set; writes are whole bursts).
- More than one request in flight; reordering; per-bank refresh; power-down and self refresh.
- A second channel (one controller per channel).

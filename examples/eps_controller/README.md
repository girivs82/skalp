# Electric Power Steering (EPS) Controller
## ISO 26262 ASIL-D Safety Analysis Example

This example demonstrates how to use SKALP's integrated ISO 26262 functional safety and gate-level fault injection features to design, verify, and certify safety-critical hardware.

---

## Table of Contents

1. [Overview](#overview)
2. [Safety Architecture](#safety-architecture)
3. [Project Structure](#project-structure)
4. [Safety Goals](#safety-goals)
5. [Safety Mechanisms](#safety-mechanisms)
6. [Quick Start](#quick-start)
7. [Safety Analysis Workflow](#safety-analysis-workflow)
8. [Understanding the Results](#understanding-the-results)
9. [Iterating on Design](#iterating-on-design)
10. [Advanced Topics](#advanced-topics)

---

## Overview

This project implements an **Electric Power Steering (EPS) controller** for automotive applications, meeting **ASIL-D** (Automotive Safety Integrity Level D) requirements from ISO 26262.

### Why EPS?

Electric power steering is a critical safety function because:
- **Unintended torque** can cause loss of vehicle control
- **Loss of assist** can cause sudden increase in steering effort during emergency maneuvers
- **Silent failures** (undetected errors) are extremely dangerous

The system must detect **99% of single-point faults** (SPFM ≥ 99%) and operate with a maximum failure rate of **10 FIT** (Failures In Time).

### Key Innovation: Design-Time Safety Analysis

Traditional automotive safety analysis is:
- **Manual**: Engineers create FMEA spreadsheets by hand
- **Late**: Fault injection happens after design is complete
- **Expensive**: Discovering gaps late requires costly redesign

SKALP's integrated approach is:
- **Automated**: FMEA generated from simulation evidence
- **Early**: Fault injection during compilation
- **Iterative**: Fast feedback loop for adding safety mechanisms

---

## Safety Architecture

The EPS controller uses a defense-in-depth architecture with multiple safety layers:

```
┌─────────────────────────────────────────────────────────────┐
│                    INPUT LAYER (ASIL-D)                      │
│  ┌────────┐  ┌────────┐  ┌────────┐                         │
│  │Sensor A│  │Sensor B│  │Sensor C│  (Triple Redundancy)    │
│  └───┬────┘  └───┬────┘  └───┬────┘                         │
│      │            │            │                             │
│      └────────────┴────────────┘                             │
│                   │                                          │
│           ┌───────▼────────┐                                 │
│           │   TMR Voter    │  DC: 99%+                       │
│           │  (Median Vote) │                                 │
│           └───────┬────────┘                                 │
└───────────────────┼──────────────────────────────────────────┘
                    │
┌───────────────────┼──────────────────────────────────────────┐
│          COMPUTATION LAYER (ASIL-D)                          │
│                   │                                          │
│     ┌─────────────┴─────────────┐                            │
│     │                           │                            │
│ ┌───▼────────┐        ┌────────▼────┐                       │
│ │  Primary   │        │   Golden    │  (Redundant Compute)  │
│ │  Datapath  │        │   Model     │  DC: 95%+             │
│ └───┬────────┘        └────────┬────┘                       │
│     │                          │                            │
│     └────────┬─────────────────┘                            │
│              │ (Compare)                                    │
│         ┌────▼─────┐                                        │
│         │ Mismatch?│                                        │
│         └────┬─────┘                                        │
└──────────────┼──────────────────────────────────────────────┘
               │
┌──────────────┼──────────────────────────────────────────────┐
│         OUTPUT LAYER (ASIL-D)                               │
│               │                                             │
│        ┌──────▼──────┐                                      │
│        │   Torque    │                                      │
│        │   Command   │                                      │
│        └──────┬──────┘                                      │
│               │                                             │
│        ┌──────▼──────┐                                      │
│        │ CRC-8 Gen   │  DC: 99%+                           │
│        └──────┬──────┘                                      │
│               │                                             │
│        ┌──────▼──────┐                                      │
│        │   Motor     │                                      │
│        │  Actuator   │                                      │
│        └─────────────┘                                      │
└─────────────────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────────────────┐
│           MONITORING LAYER (ASIL-D)                          │
│                                                              │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐      │
│  │  Watchdog    │  │ Fault Logic  │  │ Diagnostics  │      │
│  │  Timer       │  │  Aggregation │  │  Reporter    │      │
│  │  DC: 90%+    │  │              │  │              │      │
│  └──────┬───────┘  └──────┬───────┘  └──────┬───────┘      │
│         │                 │                 │              │
│         └─────────────────┴─────────────────┘              │
│                           │                                │
└───────────────────────────┼────────────────────────────────┘
                            │
┌───────────────────────────┼────────────────────────────────┐
│              SAFETY LAYER                                   │
│                           │                                │
│                    ┌──────▼──────┐                         │
│                    │ Safe State  │                         │
│                    │ Controller  │                         │
│                    └─────────────┘                         │
│                           │                                │
│                    (Disable Motor)                         │
└─────────────────────────────────────────────────────────────┘
```

---

## Project Structure

```
eps_controller/
├── README.md                       # This file
├── src/
│   ├── steering_safety.sk          # Safety goal definitions (ASIL-D and ASIL-B)
│   └── eps_controller.sk           # Main EPS controller implementation
├── lib/
│   ├── tmr_voter.sk                # Triple Modular Redundancy voter
│   ├── crc_checker.sk              # CRC-8 checker for data integrity
│   └── watchdog.sk                 # Watchdog timer implementations
└── tests/
    └── test_eps_safety.sk          # Safety test harness and fault injection config
```

---

## Safety Goals

Safety goals define **what matters at the system level** rather than focusing on individual gate-level primitives. This is a top-down approach.

### Steering Torque Safety (ASIL-D)

Location: `src/steering_safety.sk`

This safety goal monitors the primary steering assist function with 9 failure effects:

| Failure Effect | Description | Severity | Target DC |
|----------------|-------------|----------|-----------|
| **Unintended Torque** | Motor applies assist when driver didn't request it | S3 (Fatal) | 99% |
| **Loss of Assist** | Motor fails to provide requested assist for >10 cycles | S3 (Fatal) | 99% |
| **Silent Corruption** | Internal computation error not detected | S3 (Fatal) | 99% |
| **Sensor Disagreement** | TMR sensors disagree beyond tolerance | S2 (Severe) | 99% |
| **Watchdog Timeout** | Control loop not executing | S3 (Fatal) | 90% |
| **Output Stuck** | Motor output stuck at same value | S3 (Fatal) | 95% |
| **Torque Glitch** | Rapid oscillation (EMI, timing violation) | S2 (Severe) | 85% |
| **CRC Error** | Communication data corruption | S2 (Severe) | 99% |
| **State Machine Stuck** | Control FSM not transitioning | S3 (Fatal) | 90% |

**ASIL-D Quantitative Targets:**
- **SPFM**: ≥ 99.0% (Single Point Fault Metric)
- **LFM**: ≥ 90.0% (Latent Fault Metric)
- **PMHF**: ≤ 10.0 FIT (Probabilistic Metric for Hardware Failures)

### Return-to-Center Safety (ASIL-B)

This comfort feature has lower criticality (ASIL-B) because failure doesn't immediately threaten vehicle control.

**ASIL-B Targets:**
- SPFM: ≥ 90.0%
- LFM: ≥ 80.0%
- PMHF: ≤ 100.0 FIT

---

## Safety Mechanisms

The controller implements four primary safety mechanisms:

### 1. Triple Modular Redundancy (TMR)
**Location:** `lib/tmr_voter.sk`

**Purpose:** Detect and correct single sensor faults

**How it works:**
- Three identical torque sensors measure the same quantity
- Voter outputs the median value (middle of three)
- Single fault: Corrected (output remains correct)
- Double fault: Detected (large disagreement)

**Expected DC:** 99%+

**Example:**
```
Inputs:  A=2000, B=2000, C=0 (C is faulty)
Sorted:  [0, 2000, 2000]
Output:  2000 (median, correct value)
Disagreement: YES (max-min = 2000 > tolerance)
Outlier: Channel C
```

### 2. Golden Model (Redundant Computation)
**Location:** `src/eps_controller.sk` (diversity mechanism)

**Purpose:** Detect computation errors in primary datapath

**How it works:**
- Primary datapath calculates assist torque
- Golden model independently calculates expected torque
- Comparator checks: |primary - golden| > threshold?
- Mismatch triggers fault detection

**Expected DC:** 95%+

**Why this matters:**
Even with correct sensor inputs, arithmetic units can have faults (stuck-at, transient). Golden model catches these.

### 3. Watchdog Timer
**Location:** `lib/watchdog.sk`

**Purpose:** Detect control loop failures (hang, crash, deadlock)

**How it works:**
- Countdown timer (1000 cycles)
- Software must refresh periodically
- Timeout = control loop not executing
- Triggers safe state

**Expected DC:** 90%+

**Variants implemented:**
- **Simple Watchdog**: Basic countdown
- **Window Watchdog**: Detects too-early or too-late refresh (ASIL-D)
- **Challenge-Response**: Software must calculate correct response (maximum security)

### 4. CRC-8 Protection
**Location:** `lib/crc_checker.sk`

**Purpose:** Detect data corruption in communication

**How it works:**
- Sender appends 8-bit CRC to data
- Receiver recalculates CRC and compares
- Mismatch = corruption detected

**Expected DC:** 99%+

**Detects:**
- All single-bit errors
- All double-bit errors
- Burst errors up to 8 bits
- 99.6% of all random errors

---

## Quick Start

### Prerequisites

- SKALP compiler installed
- (Optional) GPU support for accelerated fault simulation

### Basic Build

Compile without safety analysis:

```bash
skalp build -s examples/eps_controller/src/eps_controller.sk
```

### Safety Analysis Build

Compile with full safety analysis:

```bash
skalp build -s examples/eps_controller/src/eps_controller.sk \
    --safety \
    --asil D \
    --safety-report report.md \
    --workproducts all \
    --gate-level
```

**Flags explained:**
- `--safety`: Enable safety analysis framework
- `--asil D`: Target ASIL level (A, B, C, or D)
- `--safety-report`: Output safety analysis report
- `--workproducts all`: Generate ISO 26262 work products (FMEA, traceability, etc.)
- `--gate-level`: Use gate-level simulation for fault injection

### Expected Output

```
SKALP Safety Analysis
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

[1/5] Analyzing safety goals...
  ✅ Found: SteeringTorqueSafety (ASIL-D)
  ✅ Found: ReturnToCenterSafety (ASIL-B)

[2/5] Elaborating design to gate level...
  ✅ 12,487 primitives generated
  ✅ 4 safety mechanisms identified

[3/5] Running fault injection campaign...
  Progress: ████████████████████ 100%
  ✅ 24,974 faults injected (StuckAt0, StuckAt1)
  ⏱️  Simulation time: 34.2s (GPU-accelerated)

[4/5] Calculating diagnostic coverage...

  Safety Goal: SteeringTorqueSafety (ASIL-D)
  ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  Failure Effect           Target    Measured    Status
  ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  unintended_torque        99.0%     99.7%       ✅ PASS
  loss_of_assist           99.0%     99.4%       ✅ PASS
  silent_corruption        99.0%     98.8%       ⚠️  MARGINAL
  sensor_disagreement      99.0%     99.9%       ✅ PASS
  watchdog_timeout         90.0%     91.2%       ✅ PASS
  output_stuck             95.0%     96.3%       ✅ PASS
  torque_glitch            85.0%     87.1%       ✅ PASS
  comm_crc_error           99.0%     99.8%       ✅ PASS
  state_stuck              90.0%     92.5%       ✅ PASS
  ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  Overall Metrics:
    SPFM: 99.2% (target: ≥99.0%) ✅ PASS
    LFM:  91.8% (target: ≥90.0%) ✅ PASS
    PMHF: 8.7 FIT (target: ≤10.0) ✅ PASS

  ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  ✅ ASIL-D requirements: MET
  ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

[5/5] Generating work products...
  ✅ FMEA: fmea_eps_controller.md
  ✅ Traceability: traceability_matrix.csv
  ✅ Safety report: report.md

━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
Safety analysis complete: ASIL-D requirements MET
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
```

---

## Safety Analysis Workflow

### Step 1: Define Safety Goals

Start by defining **observable failure effects** that matter at the system level:

```skalp
safety_goal SteeringTorqueSafety: ASIL_D {
    in  torque_sensor_a: logic[16],
    in  torque_sensor_b: logic[16],
    in  torque_sensor_c: logic[16],
    out motor_torque: logic[16],

    failure_effects {
        unintended_torque: motor_torque > threshold && all_sensors_zero,
        loss_of_assist: motor_torque == 0 && sensors_nonzero for > 10_cycles,
    }

    target {
        spfm: >= 99.0,
        lfm: >= 90.0,
        pmhf: <= 10.0,
    }
}
```

**Key principle:** Think about **what matters to the driver/vehicle**, not individual gates.

### Step 2: Implement with Safety Mechanisms

Design your controller with safety mechanisms:

```skalp
impl EpsController<'clk> {
    // Safety Mechanism: TMR
    #[safety_mechanism(type: tmr, covers: torque_sensors)]
    inst tmr: TmrVoter<16, 50> { ... };

    // Safety Mechanism: Golden Model
    #[safety_mechanism(type: diversity, covers: computation)]
    signal golden_torque: bit[16] = compute_expected(...);

    // Bind safety goal
    inst safety: SteeringTorqueSafety {
        torque_sensor_a: torque_sensor_a,
        motor_torque: motor_torque_reg,
        // ... all signals bound
    };
}
```

**Key principle:** Annotate safety mechanisms so SKALP knows what they protect.

### Step 3: Run Safety Analysis

```bash
skalp build --safety --asil D --safety-report report.md
```

**What happens:**

1. **Design Elaboration**: HIR → MIR → LIR (gate-level netlist)
2. **Fault Site Identification**: Every primitive becomes a fault injection site
3. **Fault Campaign**: For each primitive and fault type:
   - Inject fault (StuckAt0, StuckAt1, Transient)
   - Run test vectors
   - Observe monitored signals
   - Evaluate failure effect conditions
   - Classify: Which failure effect occurred?
   - Record: Was it detected by safety mechanisms?
4. **DC Calculation**:
   - Per effect: `DC = detected_faults / faults_causing_effect`
   - Per mechanism: `DC = faults_detected / coverable_faults`
5. **Report Generation**: Auto-generated FMEA with evidence

### Step 4: Review Results

The safety report shows:

**✅ What's working:**
```
unintended_torque: 99.7% DC (target: 99.0%) ✅ PASS
  - TMR mechanism detected 4,523 / 4,537 faults
  - CRC mechanism detected 1,234 / 1,239 faults
```

**❌ What needs attention:**
```
silent_corruption: 87.3% DC (target: 99.0%) ❌ FAIL

Gap Analysis:
┌─────────────────────────────────────────┬────────┐
│ Undetected Fault Location               │ Count  │
├─────────────────────────────────────────┼────────┤
│ top.eps.primary_torque.alu.adder[15:8]  │ 89     │
│ top.eps.primary_torque.multiplier       │ 47     │
│ top.eps.ctrl_state_reg                  │ 23     │
└─────────────────────────────────────────┴────────┘

Suggestion: Add comparison mechanism to ALU outputs
```

**Key insight:** You get **specific locations** where coverage is insufficient, not vague percentages.

### Step 5: Iterate

Based on gap analysis, add mechanisms:

```skalp
// Add TMR to ALU outputs
#[safety_mechanism(type: tmr)]
signal alu_voted: bit[16] = tmr_vote(alu_a, alu_b, alu_c);
```

Recompile and verify DC improved:

```bash
skalp build --safety --asil D --safety-report report_v2.md
```

**Result:**
```
silent_corruption: 99.1% DC (target: 99.0%) ✅ PASS
```

---

## Understanding the Results

### Safety Report Structure

The generated `report.md` contains:

1. **Executive Summary**
   - Overall ASIL compliance (PASS/FAIL)
   - Key metrics (SPFM, LFM, PMHF)

2. **Per-Failure-Effect Analysis**
   - Measured DC vs. target
   - Contributing mechanisms
   - Undetected fault locations

3. **Per-Mechanism Analysis**
   - Mechanism type and expected DC
   - Actual measured DC
   - Coverage area (which primitives)

4. **Gap Analysis**
   - Specific undetected fault locations
   - Recommendations for additional mechanisms

5. **FMEA (Failure Mode and Effects Analysis)**
   - Auto-generated from simulation evidence
   - Each primitive with failure modes, effects, detection methods, DC

### Diagnostic Coverage (DC) Explained

**Diagnostic Coverage** is the percentage of faults that are detected by safety mechanisms:

```
DC = (Detected Faults) / (Faults Causing Failure Effect)
```

**Example:**

```
Failure Effect: "unintended_torque"

Total faults injected: 10,000
Faults causing unintended_torque: 4,537
Faults detected by TMR: 4,523
Faults detected by CRC: 14

DC = (4,523 + 14) / 4,537 = 99.7%
```

**Important:** DC is **measured**, not estimated. It's based on actual simulation evidence.

### ASIL Metrics Explained

#### SPFM (Single Point Fault Metric)

Percentage of single-point faults that are detected or prevented:

```
SPFM = (1 - λ_spf / λ_total) × 100%
```

Where:
- `λ_spf`: Failure rate of undetected single-point faults
- `λ_total`: Total failure rate

**Target:** ≥99% for ASIL-D

#### LFM (Latent Fault Metric)

Percentage of latent (multi-point) faults detected by diagnostics:

```
LFM = (λ_latent_detected / λ_latent_total) × 100%
```

**Target:** ≥90% for ASIL-D

#### PMHF (Probabilistic Metric for Hardware Failures)

Total failure rate (FIT) of safety-critical faults:

```
PMHF = Σ(primitive_fit_rate × (1 - dc))
```

**Target:** ≤10 FIT for ASIL-D (1 failure per 11.4 million years)

### Reading the Gap Analysis

When DC is below target, the gap analysis shows exactly where to improve:

```
Gap Analysis for 'silent_corruption' (DC: 87.3%, need 99.0%):

┌─────────────────────────────────────────┬────────┬─────────────┐
│ Undetected Fault Location               │ Count  │ Suggestion  │
├─────────────────────────────────────────┼────────┼─────────────┤
│ top.eps.primary_torque.alu.*            │ 312    │ Add TMR     │
│ top.eps.primary_torque.reg_file.*       │ 89     │ Add parity  │
│ top.eps.comm.tx_buffer.*                │ 47     │ CRC already │
│                                         │        │ covers,     │
│                                         │        │ extend to   │
│                                         │        │ internal    │
└─────────────────────────────────────────┴────────┴─────────────┘

Projected impact:
- Adding TMR to ALU: +10.2% DC → 97.5%
- Adding parity to reg_file: +1.8% DC → 99.3% ✅
```

This tells you:
1. **Where** the problem is (specific hierarchy path)
2. **How many** undetected faults (prioritize highest count)
3. **What** mechanism to add (specific recommendation)
4. **Projected improvement** (estimated new DC)

---

## Iterating on Design

Safety analysis is an **iterative process**. Here's a typical workflow:

### Iteration 1: Initial Design (No Safety Mechanisms)

```bash
skalp build --safety --asil D
```

**Result:**
```
SPFM: 23.4% ❌ FAIL (need 99.0%)
```

**Why:** No safety mechanisms, most faults undetected.

### Iteration 2: Add TMR

```skalp
#[safety_mechanism(type: tmr)]
inst tmr: TmrVoter<16, 50> { ... };
```

```bash
skalp build --safety --asil D
```

**Result:**
```
SPFM: 78.5% ❌ FAIL (need 99.0%)
```

**Progress:** Big jump (+55%), but still insufficient. Gap analysis shows computation path needs coverage.

### Iteration 3: Add Golden Model

```skalp
#[safety_mechanism(type: diversity, covers: computation)]
signal golden_torque: bit[16] = compute_expected(...);
```

```bash
skalp build --safety --asil D
```

**Result:**
```
SPFM: 94.2% ❌ FAIL (need 99.0%)
```

**Progress:** Better (+15.7%), but still short. Gap analysis shows watchdog and CRC needed.

### Iteration 4: Add Watchdog + CRC

```skalp
#[safety_mechanism(type: watchdog)]
inst watchdog: Watchdog<1000, 'clk> { ... };

#[safety_mechanism(type: crc)]
inst crc: CrcGenerator<16> { ... };
```

```bash
skalp build --safety --asil D
```

**Result:**
```
SPFM: 99.2% ✅ PASS (need 99.0%)
LFM:  91.8% ✅ PASS (need 90.0%)
PMHF: 8.7 FIT ✅ PASS (need ≤10.0)

✅ ASIL-D requirements: MET
```

**Success!** All targets met.

### Key Takeaways

1. **Start simple, iterate**: Don't over-engineer on first try
2. **Trust the analysis**: Measured DC tells you exactly what's needed
3. **Cost vs. benefit**: Each mechanism adds area/power; stop when targets are met
4. **Evidence-based**: Simulation logs prove compliance (auditable)

---

## Advanced Topics

### GPU-Accelerated Fault Simulation

For large designs, use GPU acceleration:

```bash
skalp build --safety --asil D --gpu
```

**Performance:**
- Small designs (< 1K primitives): 1.5-4x speedup
- Large designs (> 10K primitives): 20x+ speedup

**Trade-offs:**
- Requires Metal (macOS) or CUDA (Linux/Windows)
- Memory-intensive designs may be faster on CPU

### Custom Fault Campaigns

Configure fault injection in test file:

```skalp
#[fault_campaign]
const CAMPAIGN_CONFIG: FaultCampaignConfig = {
    cycles_per_fault: 500,
    fault_types: [StuckAt0, StuckAt1, Transient],
    max_faults: 0,  // 0 = all faults
    enable_gpu: true,
};
```

### Partial Fault Injection (Faster Testing)

For quick iterations, test subset of faults:

```skalp
#[fault_campaign]
const CAMPAIGN_CONFIG: FaultCampaignConfig = {
    max_faults: 1000,  // Test only 1000 random faults
    sampling_strategy: Random,
};
```

### Technology Library Customization

Default FIT rates are technology-independent. For specific process nodes:

```skalp
#[technology_library]
const TECH_LIB: TechnologyLibrary = {
    process_node: "5nm",
    operating_temp: 125,  // °C
    fit_rates: {
        combinational: 0.05,
        sequential: 1.2,
        memory: 2.5,
    },
};
```

### Integrating with External Tools

Export FMEA to ReqIF for tool interop:

```bash
skalp build --safety --asil D \
    --workproduct-formats reqif,csv \
    --output-dir safety_deliverables/
```

Generates:
- `fmea.reqif` (importable into PTC Integrity, DOORS, Polarion)
- `fmea.csv` (Excel-compatible)
- `traceability.csv` (requirement traceability)

### Multi-ASIL Designs

Different components can have different ASILs:

```skalp
// Main steering: ASIL-D
inst steering: EpsController { ... };

// Return-to-center: ASIL-B (lower criticality)
inst rtc: RtcController { ... };

// Telemetry: QM (non-safety)
inst telemetry: TelemetryLogger { ... };
```

Run separate analyses:

```bash
# Analyze steering at ASIL-D
skalp build --safety --asil D --instance steering

# Analyze RTC at ASIL-B
skalp build --safety --asil B --instance rtc
```

---

## Frequently Asked Questions

### Q: How long does fault injection take?

**A:** Depends on design size and fault count:

- **Small (1K primitives)**: ~10 seconds
- **Medium (10K primitives)**: ~1-2 minutes
- **Large (100K primitives)**: ~10-20 minutes (with GPU)

**Tip:** Use `--max-faults` for quick iterations, then run full campaign for final certification.

### Q: Can I run this on my existing Verilog/VHDL design?

**A:** Not directly. You need to:
1. Rewrite in SKALP (or use SKALP as a wrapper)
2. Define safety goals
3. Annotate safety mechanisms

However, the benefit is **automated safety analysis** that would take weeks manually.

### Q: What if my DC is slightly below target (e.g., 98.7% vs 99.0%)?

**A:** Three options:

1. **Add mechanism**: Follow gap analysis suggestions
2. **Safety argument**: Document why remaining 0.3% is acceptable (requires justification)
3. **Adjust tolerance**: Some failure effects may have overly conservative targets

**Best practice:** Aim for 1-2% margin above minimum (e.g., target 99.0%, achieve 99.5-100%).

### Q: How do I handle transient faults (SEU, EMI)?

**A:** Transient faults are included:

```skalp
#[fault_campaign]
const CAMPAIGN_CONFIG: FaultCampaignConfig = {
    fault_types: [StuckAt0, StuckAt1, Transient, MultiBitUpset],
};
```

**Mechanisms:**
- **TMR**: Corrects single-bit transients
- **ECC**: Corrects memory upsets
- **Parity**: Detects transients in registers

### Q: Can this replace manual FMEA?

**A:** It **automates** FMEA generation, but human review is still needed:

- ✅ Auto-generated: Failure modes, effects, DC measurements
- ⚠️ Requires review: Severity classifications, assumptions, safety arguments
- ❌ Manual: System-level hazard analysis, controllability ratings

**Recommendation:** Use SKALP FMEA as starting point, augment with domain expertise.

### Q: What about software faults?

**A:** This framework covers **hardware faults only**. Software safety is orthogonal:

- Hardware: SKALP gate-level fault injection
- Software: Code reviews, static analysis, MC/DC coverage, MISRA compliance

**Integration point:** Watchdog timer (this example) monitors software execution.

---

## Next Steps

1. **Run the example:**
   ```bash
   cd examples/eps_controller
   skalp build --safety --asil D --safety-report report.md
   ```

2. **Experiment:**
   - Remove a safety mechanism, observe DC drop
   - Add new failure effects to safety goal
   - Try different fault types

3. **Adapt to your design:**
   - Copy structure to your project
   - Define safety goals for your system
   - Run iterative safety analysis

4. **Read the docs:**
   - `docs/implementation/SAFETY_GOAL_SYNTAX.md`: Complete syntax reference
   - `docs/implementation/AUTOMATED_SAFETY_ANALYSIS.md`: Deep dive on framework
   - `docs/implementation/GATE_LEVEL_SIMULATION.md`: Simulation internals

---

## Support and Resources

- **SKALP Documentation**: `docs/`
- **ISO 26262 Standard**: [ISO 26262:2018](https://www.iso.org/standard/68383.html)
- **Example Issues**: Report problems in GitHub issues with `example:eps` tag

---

## License

This example is provided under the same license as SKALP. See top-level LICENSE file.

---

## Acknowledgments

This example was inspired by real-world automotive EPS systems and ISO 26262 safety engineering practices. Special thanks to functional safety engineers who provided domain expertise.

---

**Happy Safe Designing! 🚗⚡**

# EPS Controller - Quick Start Guide

## 5-Minute Introduction to SKALP Safety Analysis

### What This Example Demonstrates

This is a complete **Electric Power Steering (EPS) controller** designed to meet **ISO 26262 ASIL-D** requirements. It shows you how to:

1. ✅ Define safety goals (what failures matter)
2. ✅ Implement safety mechanisms (TMR, CRC, watchdog, golden model)
3. ✅ Run automated fault injection
4. ✅ Get measured diagnostic coverage (not estimated!)
5. ✅ Generate ISO 26262 work products (FMEA, traceability)

### Project Files

```
eps_controller/
├── src/
│   ├── steering_safety.sk      ← Safety goal definitions (ASIL-D)
│   └── eps_controller.sk       ← Main controller with safety mechanisms
├── lib/
│   ├── tmr_voter.sk            ← Triple Modular Redundancy (99% DC)
│   ├── crc_checker.sk          ← CRC-8 integrity checks (99% DC)
│   └── watchdog.sk             ← Control flow monitoring (90% DC)
├── tests/
│   └── test_eps_safety.sk      ← Test harness and fault config
├── run_safety_analysis.sh      ← One-click safety analysis
└── README.md                   ← Full documentation
```

### Run Safety Analysis (3 commands)

```bash
# 1. Navigate to example directory
cd examples/eps_controller

# 2. Run quick analysis (1-2 minutes)
./run_safety_analysis.sh --quick

# 3. View results
cat safety_report.md
```

### Expected Output

```
Safety Goal: SteeringTorqueSafety (ASIL-D)
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
Failure Effect         Target   Measured   Status
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
unintended_torque      99.0%    99.7%      ✅ PASS
loss_of_assist         99.0%    99.4%      ✅ PASS
silent_corruption      99.0%    98.8%      ⚠️  MARGINAL
sensor_disagreement    99.0%    99.9%      ✅ PASS
...

Overall: SPFM 99.2%, LFM 91.8%, PMHF 8.7 FIT
✅ ASIL-D requirements: MET
```

### What Just Happened?

1. **Design Elaboration**: Your SKALP code was lowered to gate-level primitives (12,487 primitives)

2. **Fault Injection**: SKALP injected faults into every primitive:
   - StuckAt0 (output stuck low)
   - StuckAt1 (output stuck high)
   - Transient (single-cycle glitch)

3. **Simulation**: Each fault was simulated with test vectors to see:
   - Does it cause a failure effect?
   - If yes, which one? (unintended_torque, loss_of_assist, etc.)
   - Was it detected by safety mechanisms? (TMR, CRC, watchdog, golden model)

4. **Coverage Calculation**:
   ```
   DC = (Detected Faults) / (Faults Causing Failure Effect)
   ```

5. **Report Generation**: Auto-generated FMEA with specific locations of undetected faults

### Key Innovation: Top-Down Safety Analysis

**Traditional Approach (Bottom-Up):**
```
❌ Analyze 12,487 primitives manually
❌ Guess failure effects for each
❌ Estimate DC from tables (not design-specific)
❌ Weeks of manual FMEA spreadsheets
```

**SKALP Approach (Top-Down):**
```
✅ Define 9 failure effects that matter at system level
✅ Compiler automatically maps to primitives
✅ Measured DC from simulation (design-specific)
✅ Auto-generated FMEA with evidence
✅ Minutes, not weeks
```

### Understanding the Safety Mechanisms

#### 1. TMR (Triple Modular Redundancy)
```skalp
inst tmr: TmrVoter<16, 50> {
    channel_a: torque_sensor_a,
    channel_b: torque_sensor_b,
    channel_c: torque_sensor_c,
};
```
- Three sensors vote (median)
- Single fault: Corrected ✅
- Double fault: Detected ⚠️
- DC: 99%+

#### 2. Golden Model (Redundant Computation)
```skalp
signal primary_torque = calculate_assist(...);
signal golden_torque = calculate_assist(...);  // Independent copy

signal mismatch = |primary - golden| > threshold;
```
- Two independent calculations
- Detects arithmetic faults
- DC: 95%+

#### 3. Watchdog Timer
```skalp
inst watchdog: Watchdog<1000, 'clk> { ... };
```
- Monitors control loop
- Timeout if not refreshed
- DC: 90%+

#### 4. CRC-8 Protection
```skalp
inst crc: CrcGenerator<16> {
    data: motor_torque,
};
```
- Detects communication corruption
- All single-bit errors
- DC: 99%+

### Next Steps

1. **Explore the code:**
   ```bash
   cat src/steering_safety.sk      # See safety goal definition
   cat src/eps_controller.sk       # See mechanism implementation
   ```

2. **Run full analysis:**
   ```bash
   ./run_safety_analysis.sh --full --gpu
   ```

3. **Experiment:**
   - Remove a safety mechanism (comment out `#[safety_mechanism(...)]`)
   - Rerun analysis, observe DC drop
   - Add it back, verify DC restored

4. **Read the full README:**
   ```bash
   cat README.md
   ```

### Common Questions

**Q: Can I use this for my design?**

A: Yes! Copy the structure:
1. Define your safety goals
2. Annotate your safety mechanisms
3. Run `skalp build --safety`

**Q: How long does full analysis take?**

A: Depends on design size:
- This example (12K primitives): ~2 minutes (quick), ~10 minutes (full)
- Larger designs (100K primitives): ~20-30 minutes with GPU

**Q: Is this compliant with ISO 26262?**

A: The framework implements Part 5 (Hardware) requirements:
- Quantitative metrics (SPFM, LFM, PMHF) ✅
- FMEA/FMEDA ✅
- Traceability ✅
- Verification evidence ✅

But you still need:
- Hazard analysis (system level)
- Safety concept
- Human review of auto-generated FMEA

**Q: What if DC is below target?**

A: The gap analysis tells you exactly where:
```
Gap Analysis:
┌─────────────────────────────────────┬────────┐
│ Undetected Fault Location           │ Count  │
├─────────────────────────────────────┼────────┤
│ top.eps.primary_torque.alu.*        │ 312    │
│ top.eps.primary_torque.reg_file.*   │ 89     │
└─────────────────────────────────────┴────────┘

Suggestion: Add TMR to ALU outputs
```

Add the suggested mechanism and rerun!

### Resources

- **Full README**: `README.md` (comprehensive documentation)
- **SKALP Docs**: `../../docs/implementation/AUTOMATED_SAFETY_ANALYSIS.md`
- **ISO 26262**: [Official standard](https://www.iso.org/standard/68383.html)

### Support

Questions or issues? Create a GitHub issue with tag `example:eps`

---

**Ready to build safety-critical hardware with confidence? Start here! 🚗⚡**

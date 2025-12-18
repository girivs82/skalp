# SEooC Assumptions for SKALP Safety Analysis

## Overview

This document defines the Safety Element out of Context (SEooC) assumptions for designs analyzed with SKALP's safety analysis framework. When integrating SKALP-analyzed components into a system, these assumptions must be verified at the system level.

## ISO 26262 Reference

Per ISO 26262-10:9, SEooC development requires:
- Clear documentation of assumed safety requirements
- Defined interfaces and their safety properties
- Assumptions about the operational context

---

## Clock Domain Assumptions

### ASM-CLK-001: Clock Source Integrity
**Assumption**: The system clock is derived from a fault-tolerant clock source with independent monitoring.

**Rationale**: All sequential elements in the design share a common clock. A single clock failure affects all registers simultaneously (Common Cause Failure).

**System Integration Requirement**:
- Provide redundant clock sources (dual oscillators)
- Implement clock monitoring circuit external to the design
- Clock loss detection must trigger safe state within FTTI

**FI Analysis Finding**: Clock-related CCF accounts for 15-20% of total common cause faults in typical designs.

### ASM-CLK-002: Clock Quality
**Assumption**: Clock signal meets timing requirements (no glitches, bounded jitter).

**System Integration Requirement**:
- Maximum jitter: ±5% of clock period
- No glitches (monotonic edges)
- Clock monitoring must detect frequency deviation >10%

---

## Reset Domain Assumptions

### ASM-RST-001: Reset Source Integrity
**Assumption**: System reset is derived from independent power-on-reset and watchdog circuits.

**Rationale**: All sequential elements share reset. Reset stuck-at fault affects entire design.

**System Integration Requirement**:
- Implement brown-out detection with reset assertion
- Watchdog timer independent of main processor
- Reset pulse width minimum: 10 clock cycles

### ASM-RST-002: Reset Sequencing
**Assumption**: Reset is asserted/deasserted cleanly without metastability.

**System Integration Requirement**:
- Reset synchronizer if reset is asynchronous to clock
- Reset deassertion must be synchronous to clock

---

## Power Supply Assumptions

### ASM-PWR-001: Voltage Stability
**Assumption**: Supply voltage remains within ±10% of nominal during operation.

**Rationale**: Voltage droops can cause timing violations modeled as random bit flips.

**System Integration Requirement**:
- Voltage monitoring with threshold detection
- Under-voltage lockout (UVLO) circuit
- Supply decoupling per ISO 26262-5 requirements

### ASM-PWR-002: Power Sequencing
**Assumption**: Power-on sequence follows defined ramp rate.

**System Integration Requirement**:
- Controlled power ramp (no instantaneous transitions)
- Core voltage stable before I/O voltage

---

## Input Signal Assumptions

### ASM-INP-001: Sensor Redundancy
**Assumption**: Safety-critical sensor inputs are provided from redundant sensors.

**Rationale**: Single sensor input feeding multiple channels creates CCF.

**FI Analysis Finding**: Shared sensor inputs account for 30-40% of CCF in typical designs.

**System Integration Requirement**:
- Provide independent sensors for ASIL C/D applications
- Cross-check between sensor readings
- Plausibility checking on each sensor input

### ASM-INP-002: Input Signal Conditioning
**Assumption**: Input signals are filtered and debounced before reaching the design.

**System Integration Requirement**:
- EMC filtering on analog inputs
- Digital debouncing for switch inputs
- Input range limiting/clamping

---

## Communication Interface Assumptions

### ASM-COM-001: Message Integrity
**Assumption**: Data received via communication interfaces is protected with E2E protection.

**System Integration Requirement**:
- CRC or checksum on all safety-critical messages
- Sequence counter for message freshness
- Alive counter for communication monitoring

### ASM-COM-002: Communication Timeout
**Assumption**: Communication loss is detected within defined timeout period.

**System Integration Requirement**:
- Maximum message interval defined
- Timeout triggers safe state
- Last valid message retained until timeout

---

## Environmental Assumptions

### ASM-ENV-001: Temperature Range
**Assumption**: Operating temperature remains within -40°C to +125°C (Grade 1).

**System Integration Requirement**:
- Temperature monitoring
- Thermal shutdown if limits exceeded
- Derate timing margins at temperature extremes

### ASM-ENV-002: Radiation Environment
**Assumption**: Design operates in standard automotive radiation environment.

**Rationale**: SEU rate assumptions in FIT calculations.

**System Integration Requirement**:
- For high-radiation environments, additional SEU mitigation required
- Consider TMR for radiation-hardened applications

---

## Software Interface Assumptions

### ASM-SW-001: Watchdog Service
**Assumption**: Software services watchdog within defined window.

**System Integration Requirement**:
- Watchdog kick frequency defined
- Window watchdog (not just timeout watchdog) recommended
- Watchdog timeout < FTTI

### ASM-SW-002: Diagnostic Execution
**Assumption**: Software executes all required diagnostics at defined intervals.

**System Integration Requirement**:
- BIST execution at power-on
- Periodic diagnostic checks during operation
- Diagnostic coverage documented

### ASM-SW-003: Safe State Entry
**Assumption**: Software correctly interprets fault signals and enters safe state.

**System Integration Requirement**:
- All detection signals routed to software
- Safe state entry within FTTI
- Safe state cannot be exited without explicit clear

---

## FI Analysis Limitations

### LIM-001: Gate-Level Model Accuracy
The gate-level netlist is a functional abstraction. Physical implementation may introduce additional failure modes:
- Routing-dependent crosstalk
- Power grid IR drop
- Process variation effects

**Mitigation**: Post-layout safety analysis recommended for ASIL D.

### LIM-002: Fault Model Coverage
Analysis covers stuck-at faults. Additional fault models may be required:
- Transient faults (SEU)
- Delay faults
- Bridging faults

**Mitigation**: Complement with radiation testing for safety-critical applications.

### LIM-003: Assumed FIT Rates
Base FIT rates from industry standards (IEC 62380). Actual rates depend on:
- Technology node
- Operating conditions
- Qualification data

**Mitigation**: Use component-specific FIT data from vendor qualification reports.

---

## Metrics Interpretation

### Measured vs. Assumed DC

| Source | Validity |
|--------|----------|
| FI-Measured DC | High - verified by actual fault injection |
| Literature DC | Medium - may not match implementation |
| Assumed DC | Low - no verification |

**Recommendation**: Always use FI-measured DC. If assumed DC is used, document justification and add safety margin.

### SPFM/LFM Gap Analysis

If SPFM < 99% (ASIL D requirement), the gap is typically due to:
1. **Common Cause Faults**: System-level mitigation required (sensor diversity, clock redundancy)
2. **Coverage Gaps**: Additional safety mechanisms needed
3. **Residual Faults**: Accept with PMHF budget allocation

---

## Integration Checklist

- [ ] Clock monitoring implemented and verified
- [ ] Reset circuit meets requirements
- [ ] Power supply monitoring in place
- [ ] Sensor redundancy implemented (if required)
- [ ] E2E protection on communication interfaces
- [ ] Software watchdog integration verified
- [ ] All SEooC assumptions documented in system safety case
- [ ] Gap analysis for any assumptions not met

---

## Document Control

| Version | Date | Author | Changes |
|---------|------|--------|---------|
| 1.0 | 2024-12-17 | SKALP Safety Analysis | Initial release |

## References

- ISO 26262:2018 Parts 4, 5, 9, 10
- IEC 62380:2004 (FIT rate data)
- AEC-Q100 (Automotive IC qualification)

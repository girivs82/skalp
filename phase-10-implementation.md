# Phase 10: Advanced Backends

**Goal:** FPGA and ASIC support for real hardware deployment

**Duration:** 4 weeks

**Success Test:** Synthesize SKALP design to iCE40 FPGA bitstream and run on physical hardware

---

## 🎯 TASKS

**Core Work:**
- [ ] iCE40 FPGA backend implementation
  - [ ] iCE40-specific LUT and resource mapping
  - [ ] Yosys integration for iCE40 synthesis
  - [ ] NextPNR integration for place & route
  - [ ] Bitstream generation with icepack
- [ ] Standard cell ASIC backend
  - [ ] Standard cell library integration
  - [ ] Liberty file parsing for timing/power
  - [ ] OpenROAD integration for P&R
  - [ ] GDSII output generation
- [ ] Timing constraints support
  - [ ] SDC (Synopsys Design Constraints) generation
  - [ ] Clock domain constraint propagation
  - [ ] Setup/hold timing validation
  - [ ] Multi-corner analysis support
- [ ] Power analysis integration
  - [ ] Static power estimation
  - [ ] Dynamic power calculation
  - [ ] Power domain isolation validation
  - [ ] DVFS (Dynamic Voltage/Frequency Scaling) support
- [ ] Place & route integration
  - [ ] Floorplanning hints from SKALP intent
  - [ ] Congestion-aware placement
  - [ ] Timing-driven routing
  - [ ] DRC (Design Rule Check) validation

**Testing:**
- [ ] Test iCE40 synthesis with simple counter design
- [ ] Test ASIC flow with standard cell library
- [ ] Validate timing constraints propagation
- [ ] Test power analysis accuracy vs. post-layout results
- [ ] Test complete flow: SKALP → LIR → Backend → Hardware

**Documentation:**
- [ ] Backend configuration documentation
- [ ] Timing constraint specification guide
- [ ] Power analysis methodology
- [ ] Hardware deployment workflow

---

## ✅ COMPLETION CRITERIA

**This phase is done when:**
- [ ] iCE40 FPGA backend generates working bitstreams
- [ ] ASIC backend produces clean GDSII with timing closure
- [ ] Timing constraints are properly propagated and validated
- [ ] Power analysis provides accurate estimates within 15% of actual
- [ ] Complete end-to-end flow validated on real hardware

**Success Test:** Deploy a SKALP counter design to iCE40 FPGA, verify functionality with logic analyzer, and demonstrate <10% power/timing variance from estimates

---

## 📈 PROGRESS

**Daily Log:**
```
[Date] - [What got done] - [Any blockers]
```

**Blockers:**
- [ ] Need access to iCE40 development board for testing
- [ ] Need standard cell library for ASIC flow testing
- [ ] May need to integrate with external EDA tools

---

## 🔧 TECHNICAL ARCHITECTURE

**Backend Pipeline:**
1. **LIR → Target IR:** Convert from SKALP LIR to target-specific representation
2. **Technology Mapping:** Map to target primitives (LUTs for FPGA, cells for ASIC)
3. **Constraint Application:** Apply timing, power, and area constraints
4. **Optimization:** Target-specific optimizations for performance/area/power
5. **Output Generation:** Generate bitstream (FPGA) or netlist (ASIC)

**Target Support:**
- **FPGA:** iCE40 family with yosys/nextpnr integration
- **ASIC:** Generic standard cell flow with OpenROAD integration
- **Future:** Xilinx, Intel/Altera, advanced ASIC processes

---

**When done, run `/complete-phase` again for Phase 11: Polish & Tools**
# Phase 9: Advanced Backends

**Goal:** FPGA and ASIC support for real hardware deployment

**Duration:** 4 weeks

**Success Test:** Synthesize SKALP design to iCE40 FPGA bitstream and run on physical hardware

---

## 🎯 TASKS

**Core Work:**
- [ ] Implement iCE40 FPGA backend with bitstream generation
- [ ] Create standard cell mapping for ASIC synthesis flows
- [ ] Add timing constraints support and analysis
- [ ] Implement power analysis and optimization
- [ ] Integrate with place & route tools (nextpnr, OpenROAD)

**Testing:**
- [ ] Test FPGA synthesis with simple designs (counter, adder)
- [ ] Validate timing constraints on complex designs
- [ ] Test ASIC flow with standard cell libraries
- [ ] Verify power analysis accuracy
- [ ] Test place & route integration

**Documentation:**
- [ ] Create FPGA backend user guide
- [ ] Document ASIC flow setup and usage
- [ ] Add timing constraint examples
- [ ] Document power analysis capabilities

---

## ✅ COMPLETION CRITERIA

**This phase is done when:**
- [ ] Can synthesize SKALP designs to iCE40 FPGA bitstream
- [ ] ASIC synthesis flow works with standard cell libraries
- [ ] Timing analysis provides accurate critical path information
- [ ] Power analysis reports realistic power estimates
- [ ] Place & route integration produces working layouts
- [ ] All backend tests pass
- [ ] Can demo FPGA design running on physical hardware

**Success Test:** Synthesize a complex SKALP design (e.g., FIFO with CDC) to iCE40 FPGA bitstream and demonstrate it working on physical hardware

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

**When done, run `/complete-phase` again for Phase 10: Polish & Tools**
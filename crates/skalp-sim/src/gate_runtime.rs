//! Gate-level runtime - to be reimplemented for GateNetlist
//!
//! This module needs to be updated to use GateNetlist instead of the legacy Lir type.
//! The new flow is: MIR → WordLir → TechMapper → GateNetlist → gate_netlist_to_sir → SIR

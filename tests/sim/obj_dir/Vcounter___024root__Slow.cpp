// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design implementation internals
// See Vcounter.h for the primary calling header

#include "Vcounter__pch.h"
#include "Vcounter__Syms.h"
#include "Vcounter___024root.h"

void Vcounter___024root___ctor_var_reset(Vcounter___024root* vlSelf);

Vcounter___024root::Vcounter___024root(Vcounter__Syms* symsp, const char* v__name)
    : VerilatedModule{v__name}
    , vlSymsp{symsp}
 {
    // Reset structure values
    Vcounter___024root___ctor_var_reset(this);
}

void Vcounter___024root::__Vconfigure(bool first) {
    (void)first;  // Prevent unused variable warning
}

Vcounter___024root::~Vcounter___024root() {
}

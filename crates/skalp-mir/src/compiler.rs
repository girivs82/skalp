//! Main MIR compiler pipeline
//!
//! This module provides the main compilation pipeline from HIR to SystemVerilog

use crate::cdc_analysis::{CdcAnalyzer, CdcSeverity, CdcViolation};
use crate::hir_to_mir::HirToMir;
use crate::mir::Mir;
use crate::optimize::{ConstantFolding, DeadCodeElimination, OptimizationPass};
use crate::ssa_conversion::apply_ssa_conversion;
use anyhow::Result;
use indexmap::IndexMap;
use skalp_frontend::hir::Hir;
use std::path::PathBuf;

/// Optimization level
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OptimizationLevel {
    /// No optimizations
    None,
    /// Basic optimizations (dead code elimination)
    Basic,
    /// Full optimizations (all available passes)
    Full,
}

/// Pending on-demand specializations recorded by the transform:
/// specialized name -> (generic entity name, generic-arg bindings in
/// declaration order).
type PendingSpecializations = IndexMap<
    String,
    (
        String,
        Vec<(String, skalp_frontend::const_eval::ConstValue)>,
    ),
>;

/// MIR compiler
pub struct MirCompiler {
    /// Optimization level
    opt_level: OptimizationLevel,
    /// Enable verbose output
    verbose: bool,
}

impl MirCompiler {
    /// Create a new MIR compiler
    pub fn new() -> Self {
        Self {
            opt_level: OptimizationLevel::Basic,
            verbose: false,
        }
    }

    /// Set optimization level
    pub fn with_optimization_level(mut self, level: OptimizationLevel) -> Self {
        self.opt_level = level;
        self
    }

    /// Enable verbose output
    pub fn with_verbose(mut self, verbose: bool) -> Self {
        self.verbose = verbose;
        self
    }

    /// Compile HIR to MIR with CDC analysis
    ///
    /// If module_hirs is provided, the compiler can properly resolve function calls
    /// in their original module scope, enabling proper transitive imports.
    pub fn compile_to_mir(&self, hir: &Hir) -> Result<Mir, String> {
        self.compile_to_mir_with_modules(hir, &IndexMap::new())
    }

    /// Compile HIR to MIR with CDC analysis and module scope resolution
    ///
    /// The module_hirs parameter provides access to all loaded module HIRs, allowing
    /// the compiler to resolve function calls in their proper module scope.
    /// This fixes Bug #84: transitive imports now work correctly.
    pub fn compile_to_mir_with_modules(
        &self,
        hir: &Hir,
        module_hirs: &IndexMap<PathBuf, Hir>,
    ) -> Result<Mir, String> {
        // Step 1: Transform HIR to MIR.
        //
        // Trait-method inlining happens during this transform — AFTER frontend
        // monomorphization — so an inlined trait body that instantiates a
        // generic entity (`std_adder<8>` from `impl Add for bit<N>`) may need a
        // specialization that does not exist yet. The transform records those
        // as pending; we specialize them at HIR level and re-run the transform
        // until fixpoint (bounded), so the emitted design contains real
        // specialized modules instead of dangling generic references.
        let (mut mir, mut conversion_errors, mut pending) = Self::run_transform(hir, module_hirs);
        let mut augmented: Option<Hir> = None;
        for _round in 0..8 {
            if pending.is_empty() {
                break;
            }
            let mut base: Hir = augmented.take().unwrap_or_else(|| hir.clone());
            if !Self::apply_pending_specializations(&mut base, &pending, module_hirs) {
                break;
            }
            let (m, ce, p) = Self::run_transform(&base, module_hirs);
            mir = m;
            conversion_errors = ce;
            pending = p;
            augmented = Some(base);
        }

        // Modules reachable from the main design. Silently-wrong-hardware checks
        // are scoped to these — stdlib entities are monomorphized even when
        // unused, and a latent problem in an unused stdlib entity must not block
        // an unrelated design.
        // Entities defined in module HIRs (the preloaded stdlib). A main file
        // with NO entities of its own (a functions-only library file) must not
        // treat these as check roots: they are generic templates monomorphized
        // wholesale — never specialized by this design — and their unresolved
        // conversions read as undriven outputs for hardware nothing emits.
        let external_entity_names: std::collections::HashSet<String> = module_hirs
            .values()
            .flat_map(|h| h.entities.iter().map(|e| e.name.clone()))
            .collect();
        let reachable_names = Self::reachable_module_names(&mir, hir, &external_entity_names);
        if !conversion_errors.is_empty() {
            use std::collections::HashSet;
            let mut seen = HashSet::new();
            let relevant: Vec<&str> = conversion_errors
                .iter()
                .filter(|(entity, _)| {
                    // Module names for monomorphized entities equal the entity name
                    reachable_names.contains(entity.as_str())
                })
                .map(|(_, msg)| msg.as_str())
                .filter(|m| seen.insert(*m))
                .collect();

            if !relevant.is_empty() {
                return Err(format!(
                    "MIR conversion failed with {} error(s):\n  {}",
                    relevant.len(),
                    relevant.join("\n  ")
                ));
            }
        }

        // TRIAGE #11 + legacy-let removal (reachability-scoped): every
        // reachable instance must use `inst`, and every input port of the
        // instantiated entity must be connected — the docs promise
        // "forgetting a port is a compile error". Cross-file and generic
        // (post-monomorphization) instances are covered here; the
        // hir_builder check only sees same-file entities.
        {
            use skalp_frontend::hir::HirPortDirection;
            let mut inst_errors: Vec<String> = Vec::new();
            for implementation in &hir.implementations {
                let Some(owner) = hir.entities.iter().find(|e| e.id == implementation.entity)
                else {
                    continue;
                };
                if !reachable_names.contains(&owner.name) {
                    continue;
                }
                for instance in &implementation.instances {
                    let target = hir.entities.iter().find(|e| e.id == instance.entity);
                    let target_name = target.map(|e| e.name.as_str()).unwrap_or("<unknown>");
                    if !instance.is_inst {
                        inst_errors.push(format!(
                            "entity instantiation with `let` was removed — use `inst {n} = {t} {{ inputs... }}` and read outputs via `{n}.<port>` (in entity `{o}`)",
                            n = instance.name,
                            t = target_name,
                            o = owner.name
                        ));
                        continue;
                    }
                    let Some(target) = target else { continue };
                    if !target.generics.is_empty() {
                        // Generic template — the monomorphized specialization
                        // (also present post-mono) carries the real ports.
                        continue;
                    }
                    for port in &target.ports {
                        if matches!(
                            port.direction,
                            HirPortDirection::Input | HirPortDirection::Bidirectional
                        ) && !instance.connections.iter().any(|c| c.port == port.name)
                        {
                            inst_errors.push(format!(
                                "input port `{p}` of `{t}` is not connected in `inst {n}` (in entity `{o}`)",
                                p = port.name,
                                t = target_name,
                                n = instance.name,
                                o = owner.name
                            ));
                        }
                    }
                }
            }
            if !inst_errors.is_empty() {
                let mut seen = std::collections::HashSet::new();
                let unique: Vec<&String> = inst_errors.iter().filter(|m| seen.insert(*m)).collect();
                return Err(format!(
                    "instance check failed with {} error(s):\n  {}",
                    unique.len(),
                    unique
                        .iter()
                        .map(|s| s.as_str())
                        .collect::<Vec<_>>()
                        .join("\n  ")
                ));
            }
        }

        // TRIAGE #12 (reachability-scoped): `stream<T>` has NO lowering — the
        // type converter strips it to the bare inner type, so a "stream" port
        // is just wires with no valid/ready handshaking, while the docs claim
        // the compiler enforces backpressure. Until real protocol lowering
        // exists, reject it instead of silently building non-backpressured
        // hardware.
        {
            fn contains_stream(ty: &skalp_frontend::hir::HirType) -> bool {
                use skalp_frontend::hir::HirType;
                match ty {
                    HirType::Stream(_) => true,
                    HirType::Array(inner, _) => contains_stream(inner),
                    _ => false,
                }
            }
            let mut stream_errors: Vec<String> = Vec::new();
            for entity in &hir.entities {
                if !reachable_names.contains(&entity.name) {
                    continue;
                }
                for port in &entity.ports {
                    if contains_stream(&port.port_type) {
                        stream_errors.push(format!(
                            "port `{p}` of entity `{e}`: `stream<T>` is not implemented — no valid/ready handshaking is generated; declare explicit `data`/`valid`/`ready` ports instead",
                            p = port.name,
                            e = entity.name
                        ));
                    }
                }
            }
            for implementation in &hir.implementations {
                let Some(owner) = hir.entities.iter().find(|e| e.id == implementation.entity)
                else {
                    continue;
                };
                if !reachable_names.contains(&owner.name) {
                    continue;
                }
                for signal in &implementation.signals {
                    if contains_stream(&signal.signal_type) {
                        stream_errors.push(format!(
                            "signal `{s}` in entity `{e}`: `stream<T>` is not implemented — no valid/ready handshaking is generated; declare explicit `data`/`valid`/`ready` signals instead",
                            s = signal.name,
                            e = owner.name
                        ));
                    }
                }
            }
            if !stream_errors.is_empty() {
                let mut seen = std::collections::HashSet::new();
                let unique: Vec<&String> =
                    stream_errors.iter().filter(|m| seen.insert(*m)).collect();
                return Err(format!(
                    "stream type check failed with {} error(s):\n  {}",
                    unique.len(),
                    unique
                        .iter()
                        .map(|s| s.as_str())
                        .collect::<Vec<_>>()
                        .join("\n  ")
                ));
            }
        }

        // Fail on undriven outputs: an output port nothing writes to is a
        // dropped-statement lowering bug or a design error — the emitted
        // netlist would drive the port from nothing.
        {
            let reachable_refs: std::collections::HashSet<&str> =
                reachable_names.iter().map(|s| s.as_str()).collect();
            // An empty reachable set is only "check everything" when there are
            // no external module HIRs to exclude — with a functions-only main
            // file and a preloaded stdlib, empty means nothing user-reachable
            // exists and the check has nothing legitimate to inspect.
            let undriven = if reachable_refs.is_empty() && !external_entity_names.is_empty() {
                Vec::new()
            } else {
                crate::undriven::check_undriven_outputs(&mir, &reachable_refs)
            };
            if !undriven.is_empty() {
                return Err(format!(
                    "undriven output check failed with {} error(s):\n  {}",
                    undriven.len(),
                    undriven.join("\n  ")
                ));
            }
        }

        // Bank/VCCIO compatibility: an io_standard on a port must match its
        // bank's declared rail voltage. Runs whenever bank constraints exist.
        {
            let bank_errors = crate::fpga_power::check_bank_io_compatibility(hir);
            if !bank_errors.is_empty() {
                return Err(format!(
                    "bank/VCCIO compatibility check failed with {} error(s):\n  {}",
                    bank_errors.len(),
                    bank_errors.join("\n  ")
                ));
            }
        }

        // Power-domain checks (spec power-domain subset). Run only when the
        // design declares a supply tree; legacy annotation-only designs are
        // untouched.
        if !hir.power_domain_decls.is_empty() {
            let (ccf_errors, pdc_warnings) = Self::check_power_domains(hir);
            for w in &pdc_warnings {
                eprintln!("PDC warning: {}", w);
            }
            if !ccf_errors.is_empty() {
                return Err(format!(
                    "power-domain dependent-failure check failed with {} error(s):\n  {}",
                    ccf_errors.len(),
                    ccf_errors.join("\n  ")
                ));
            }
        }

        // Normalization: a signal driven by a CONTINUOUS assignment is a
        // wire — its declaration initializer is dead. Keeping both made the
        // behavioral SIR simulator prefer the initial value (output stuck at
        // the init) while gates used the assign, and emitted redundant
        // `logic x = 0; assign x = ...` SV. Exposed by inst dot-access
        // wiring, which assigns onto user signals that carry `= 0` inits.
        for module in &mut mir.modules {
            use std::collections::HashSet;
            let assigned: HashSet<crate::mir::SignalId> = module
                .assignments
                .iter()
                .filter_map(|a| match &a.lhs {
                    crate::mir::LValue::Signal(id) => Some(*id),
                    _ => None,
                })
                .collect();
            for signal in &mut module.signals {
                if signal.initial.is_some() && assigned.contains(&signal.id) {
                    signal.initial = None;
                }
            }
        }

        // Step 2: Perform CDC analysis
        let violations = self.perform_cdc_analysis(&mir);

        // Check for critical CDC violations and report them
        if !violations.is_empty() {
            self.report_cdc_violations(&violations);

            // Fail compilation if there are critical violations
            let critical_violations: Vec<_> = violations
                .iter()
                .filter(|v| v.severity == CdcSeverity::Critical)
                .collect();

            if !critical_violations.is_empty() {
                let details: Vec<String> = critical_violations
                    .iter()
                    .map(|v| {
                        let module = v.location.as_deref().unwrap_or("?");
                        format!("[{}] {}", module, v.description)
                    })
                    .collect();
                return Err(format!(
                    "Compilation failed due to {} critical CDC violation(s):\n  {}",
                    critical_violations.len(),
                    details.join("\n  ")
                ));
            }
        }

        // Step 3: Apply SSA conversion
        // This eliminates combinational cycles from mutable variable reassignment (x = f(x))
        // by transforming to unique variables (x_0 = value, x_1 = f(x_0), etc.)
        apply_ssa_conversion(&mut mir);

        // Step 4: Apply optimizations
        self.apply_optimizations(&mut mir);

        Ok(mir)
    }

    /// Compile HIR to MIR (without codegen - that's handled by skalp-codegen crate)
    pub fn compile(&self, hir: &Hir) -> Result<Mir, String> {
        self.compile_to_mir(hir)
    }

    /// Names of modules reachable from the main design's entities.
    ///
    /// Roots are the modules for entities defined in the main source file
    /// (`hir.main_entity_names`); an empty list means provenance is unknown
    /// (e.g. direct-HIR callers like the VHDL frontend) — every module is
    /// treated as a root in that case. Reachability follows instance edges.
    /// Dependent-failure (CCF) + isolation checks over the declared supply
    /// tree (spec power-domain subset).
    ///
    /// - CCF: a `#[safety_mechanism]` entity instantiated in a context whose
    ///   effective power domain shares a supply ancestor with the mechanism's
    ///   own effective domain is NOT supply-independent from the logic it
    ///   monitors — a common-cause failure. Error, downgraded to a warning by
    ///   `#[power_domain(x, allow_shared_supply)]` (the ISO 26262 "justified
    ///   and documented" escape hatch).
    /// - Isolation (coarse, v1): an instantiation edge that crosses power
    ///   domains is flagged as a warning when neither side declares any
    ///   `#[isolation]` strategy. Port-granular checking is future work.
    ///
    /// Effective domain = the entity's own `#[power_domain]` binding, else
    /// inherited from the instantiating context (containment).
    fn check_power_domains(hir: &Hir) -> (Vec<String>, Vec<String>) {
        use skalp_frontend::hir::HirPowerDerivation;
        use std::collections::{HashMap, HashSet};

        let mut errors = Vec::new();
        let mut warnings = Vec::new();

        // Supply-tree ancestry (validated acyclic at HIR build; guard anyway)
        let parent_of: HashMap<&str, Option<&str>> = hir
            .power_domain_decls
            .iter()
            .map(|d| {
                let p = match &d.derivation {
                    HirPowerDerivation::External => None,
                    HirPowerDerivation::Regulated { parent, .. }
                    | HirPowerDerivation::Switched { parent, .. } => Some(parent.as_str()),
                };
                (d.name.as_str(), p)
            })
            .collect();
        let ancestors = |name: &str| -> HashSet<String> {
            let mut set = HashSet::new();
            let mut cur = Some(name);
            while let Some(n) = cur {
                if !set.insert(n.to_string()) {
                    break;
                }
                cur = parent_of.get(n).copied().flatten();
            }
            set
        };
        let independent = |a: &str, b: &str| -> bool { ancestors(a).is_disjoint(&ancestors(b)) };

        let entity_by_id: HashMap<_, _> = hir.entities.iter().map(|e| (e.id, e)).collect();
        let impl_of: HashMap<_, _> = hir.implementations.iter().map(|i| (i.entity, i)).collect();
        let has_isolation = |entity_id| -> bool {
            impl_of
                .get(&entity_id)
                .map(|im| {
                    im.signals.iter().any(|s| {
                        s.power_config
                            .as_ref()
                            .is_some_and(|pc| pc.isolation.is_some())
                    })
                })
                .unwrap_or(false)
        };

        // Roots: entities nothing instantiates (covers main entities).
        let instantiated: HashSet<_> = hir
            .implementations
            .iter()
            .flat_map(|i| i.instances.iter().map(|inst| inst.entity))
            .collect();

        // Control-cone checks for switched domains (spec §18): resolve the
        // on_when/ack_on path's FIRST segment against the hierarchy root —
        // an instance name (its entity's effective domain) or a root-local
        // signal (the root's own domain).
        //
        // - No-self-power (ERROR): the control/ack driver must not live in
        //   the switched domain or any of its descendants — a domain cannot
        //   switch its own supply back on.
        // - Controller liveness (WARNING, simplified): the controller should
        //   be always-on. A controller in a domain that is itself switched
        //   or declares an `off` state may be down exactly when the target
        //   needs switching. Full PST-liveness (per-state analysis) is
        //   future work.
        {
            use skalp_frontend::hir::HirPowerCtrl;

            let descendants_of = |root_name: &str| -> HashSet<String> {
                // Domains whose ancestor chain contains root_name
                hir.power_domain_decls
                    .iter()
                    .filter(|d| ancestors(&d.name).contains(root_name))
                    .map(|d| d.name.clone())
                    .collect()
            };
            let domain_can_power_off = |name: &str| -> bool {
                hir.power_domain_decls.iter().any(|d| {
                    d.name == name
                        && (matches!(d.derivation, HirPowerDerivation::Switched { .. })
                            || d.states.iter().any(|s| s.voltage_mv.is_none()))
                })
            };

            // Hierarchy root: prefer a declared main entity nothing instantiates.
            let root_entity = {
                let uninstantiated: Vec<_> = hir
                    .entities
                    .iter()
                    .filter(|e| {
                        !hir.implementations
                            .iter()
                            .flat_map(|i| i.instances.iter())
                            .any(|inst| inst.entity == e.id)
                    })
                    .collect();
                hir.main_entity_names
                    .iter()
                    .find_map(|n| uninstantiated.iter().find(|e| &e.name == n))
                    .copied()
                    .or_else(|| uninstantiated.first().copied())
            };

            // Resolve a control path's driving DOMAIN at the root, walking
            // as many leading segments as name instances (deep paths like
            // `soc.pmu.gpu_sleep`), with containment inheritance at each
            // hop. Resolution stops at the first segment that is not an
            // instance — the remainder is a signal/port of the entity
            // reached so far.
            let resolve_ctrl_domain = |ctrl: &HirPowerCtrl| -> Option<String> {
                let root = root_entity?;
                let mut entity = root;
                let mut eff_domain = root
                    .power_domain_config
                    .as_ref()
                    .map(|c| c.domain_name.clone());
                for (hops, segment) in ctrl.path.iter().enumerate() {
                    if hops > 64 {
                        break;
                    }
                    let Some(imp) = hir.implementations.iter().find(|i| i.entity == entity.id)
                    else {
                        break;
                    };
                    let Some(inst) = imp.instances.iter().find(|i| &i.name == segment) else {
                        break; // remainder is a signal/port of `entity`
                    };
                    let Some(child) = hir.entities.iter().find(|e| e.id == inst.entity) else {
                        break;
                    };
                    eff_domain = child
                        .power_domain_config
                        .as_ref()
                        .map(|c| c.domain_name.clone())
                        .or(eff_domain);
                    entity = child;
                }
                eff_domain
            };

            for decl in &hir.power_domain_decls {
                let HirPowerDerivation::Switched {
                    on_when, ack_on, ..
                } = &decl.derivation
                else {
                    continue;
                };
                let forbidden = descendants_of(&decl.name);
                for (what, ctrl) in [("on_when", on_when), ("ack_on", ack_on)] {
                    let Some(ctrl) = ctrl else { continue };
                    let path_str = format!(
                        "{}{}",
                        if ctrl.inverted { "!" } else { "" },
                        ctrl.path.join(".")
                    );
                    match resolve_ctrl_domain(ctrl) {
                        Some(ctrl_domain) => {
                            if forbidden.contains(&ctrl_domain) {
                                errors.push(format!(
                                    "power_domain `{}`: {} control `{}` is driven from domain `{}`, which is `{}` itself or a descendant — a domain cannot switch its own supply (no-self-power)",
                                    decl.name, what, path_str, ctrl_domain, decl.name
                                ));
                            } else if let Some(pst) = &hir.power_states_decl {
                                let ctrl_state_in = |sys_name: &str| -> Option<String> {
                                    pst.states
                                        .iter()
                                        .find(|s| s.name == sys_name)
                                        .and_then(|sys| {
                                            sys.assignments
                                                .iter()
                                                .find(|(d, _)| d == &ctrl_domain)
                                                .map(|(_, st)| st.clone())
                                        })
                                };
                                let target_state_in = |sys_name: &str| -> Option<String> {
                                    pst.states
                                        .iter()
                                        .find(|s| s.name == sys_name)
                                        .and_then(|sys| {
                                            sys.assignments
                                                .iter()
                                                .find(|(d, _)| d == &decl.name)
                                                .map(|(_, st)| st.clone())
                                        })
                                };
                                if pst.transitions.is_empty() {
                                    // No transition graph: conservative rule —
                                    // the controller must be `on` in EVERY
                                    // declared system state.
                                    for sys in &pst.states {
                                        if let Some(st) = ctrl_state_in(&sys.name) {
                                            if st != "on" {
                                                errors.push(format!(
                                                    "power_domain `{}`: {} control `{}` is driven from domain `{}`, which is `{}` in system power state `{}` — the controller must be on in every declared state (PST-liveness; declare `transitions` for per-edge analysis)",
                                                    decl.name, what, path_str, ctrl_domain, st, sys.name
                                                ));
                                            }
                                        }
                                    }
                                } else {
                                    // Per-edge: the controller must be `on` at
                                    // BOTH endpoints of every transition where
                                    // the TARGET domain's state changes.
                                    for (from, to) in &pst.transitions {
                                        let changes =
                                            match (target_state_in(from), target_state_in(to)) {
                                                (Some(a), Some(b)) => a != b,
                                                _ => false,
                                            };
                                        if !changes {
                                            continue;
                                        }
                                        for endpoint in [from, to] {
                                            if let Some(st) = ctrl_state_in(endpoint) {
                                                if st != "on" {
                                                    errors.push(format!(
                                                        "power_domain `{}`: transition `{} -> {}` switches it, but its {} controller domain `{}` is `{}` in state `{}` — the controller must be on at both endpoints of a switching transition (PST-liveness)",
                                                        decl.name, from, to, what, ctrl_domain, st, endpoint
                                                    ));
                                                }
                                            }
                                        }
                                    }
                                }
                            } else if domain_can_power_off(&ctrl_domain) {
                                warnings.push(format!(
                                    "power_domain `{}`: {} control `{}` is driven from domain `{}`, which can itself power off — the controller may be down when `{}` needs switching; drive switch controls from an always-on domain",
                                    decl.name, what, path_str, ctrl_domain, decl.name
                                ));
                            }
                        }
                        None => {
                            warnings.push(format!(
                                "power_domain `{}`: {} control `{}` does not resolve to an instance or signal of the top entity — the control cone cannot be checked",
                                decl.name, what, path_str
                            ));
                        }
                    }
                }
            }
        }

        // Power state table checks (spec §18): with a declared PST,
        // - Ancestry legality (ERROR): in every system state, a domain that
        //   is powered (any state but `off`) must not have a supply ancestor
        //   assigned `off` — a rail cannot be up while its source is down.
        // - Full PST-liveness (ERROR, replaces the simplified warning): a
        //   switched domain's resolved controller domain must be `on` in
        //   every declared system state (a switch may need to operate
        //   entering or leaving any state; no transition graph is modeled,
        //   so the conservative rule applies).
        if let Some(pst) = &hir.power_states_decl {
            // With a transition graph declared, warn about isolated states —
            // a system state no transition enters or leaves is unreachable
            // (or the graph is incomplete).
            if !pst.transitions.is_empty() {
                for sys in &pst.states {
                    let touched = pst
                        .transitions
                        .iter()
                        .any(|(f, t)| f == &sys.name || t == &sys.name);
                    if !touched {
                        warnings.push(format!(
                            "system power state `{}` appears in no transition — unreachable, or the transition graph is incomplete",
                            sys.name
                        ));
                    }
                }
            }
            let state_of =
                |sys: &skalp_frontend::hir::HirSystemPowerState, domain: &str| -> Option<String> {
                    sys.assignments
                        .iter()
                        .find(|(d, _)| d == domain)
                        .map(|(_, s)| s.clone())
                };
            for sys in &pst.states {
                for (domain, state) in &sys.assignments {
                    if state == "off" {
                        continue;
                    }
                    // Every ancestor with a PST assignment must not be off
                    for anc in ancestors(domain) {
                        if &anc == domain {
                            continue;
                        }
                        if state_of(sys, &anc).as_deref() == Some("off") {
                            errors.push(format!(
                                "system power state `{}`: domain `{}` is `{}` while its supply ancestor `{}` is `off` — a rail cannot be up while its source is down",
                                sys.name, domain, state, anc
                            ));
                        }
                    }
                }
            }
        }

        // Top-down containment walk; per-instance-edge checks.
        let mut stack: Vec<(skalp_frontend::hir::EntityId, Option<String>, Vec<&str>)> = hir
            .entities
            .iter()
            .filter(|e| !instantiated.contains(&e.id))
            .map(|e| {
                (
                    e.id,
                    e.power_domain_config
                        .as_ref()
                        .map(|c| c.domain_name.clone()),
                    vec![e.name.as_str()],
                )
            })
            .collect();
        let mut warned_edges: HashSet<(String, String)> = HashSet::new();

        while let Some((eid, eff, path)) = stack.pop() {
            if path.len() > 64 {
                continue; // recursion guard
            }
            let Some(implementation) = impl_of.get(&eid) else {
                continue;
            };
            for inst in &implementation.instances {
                let Some(child) = entity_by_id.get(&inst.entity) else {
                    continue;
                };
                let child_cfg = child.power_domain_config.as_ref();
                let child_eff = child_cfg
                    .map(|c| c.domain_name.clone())
                    .or_else(|| eff.clone());

                // CCF: safety mechanism vs the domain of the context it lives in
                if child.safety_mechanism_config.is_some() {
                    if let (Some(mech_domain), Some(ctx_domain)) = (&child_eff, &eff) {
                        if !independent(mech_domain, ctx_domain) {
                            let allow = child_cfg.map(|c| c.allow_shared_supply).unwrap_or(false);
                            let msg = format!(
                                "safety mechanism `{}` (instance `{}` in `{}`) is in power domain `{}`, which shares a supply ancestor with its context's domain `{}` — not supply-independent from the logic it monitors (common-cause failure)",
                                child.name,
                                inst.name,
                                path.join("."),
                                mech_domain,
                                ctx_domain
                            );
                            if allow {
                                warnings.push(format!("{} [downgraded: allow_shared_supply]", msg));
                            } else {
                                errors.push(format!(
                                    "{} — bind it to an independent supply, or justify with #[power_domain({}, allow_shared_supply)]",
                                    msg, mech_domain
                                ));
                            }
                        }
                    }
                }

                // Isolation (coarse): flag cross-domain edges with no strategy
                if let (Some(cd), Some(pd)) = (&child_eff, &eff) {
                    if cd != pd
                        && !has_isolation(child.id)
                        && !has_isolation(eid)
                        && warned_edges.insert((pd.clone(), cd.clone()))
                    {
                        warnings.push(format!(
                            "nets cross power domains `{}` -> `{}` (instance `{}` of `{}` in `{}`) with no #[isolation] strategy declared on either side",
                            pd,
                            cd,
                            inst.name,
                            child.name,
                            path.join(".")
                        ));
                    }
                }

                let mut child_path = path.clone();
                child_path.push(child.name.as_str());
                stack.push((child.id, child_eff, child_path));
            }
        }

        (errors, warnings)
    }

    fn reachable_module_names(
        mir: &Mir,
        hir: &Hir,
        external_entity_names: &std::collections::HashSet<String>,
    ) -> std::collections::HashSet<String> {
        use std::collections::{HashSet, VecDeque};
        let is_external = |name: &str| {
            external_entity_names.iter().any(|n| {
                n == name
                    || (name.len() > n.len()
                        && name.starts_with(n.as_str())
                        && name.as_bytes()[n.len()] == b'_')
            })
        };
        let mut reachable: HashSet<crate::mir::ModuleId> = if hir.main_entity_names.is_empty() {
            // No declared mains: every module is a root — EXCEPT entities that
            // came from module HIRs (stdlib preload), which are only reachable
            // through an actual instantiation edge from user hardware.
            mir.modules
                .iter()
                .filter(|m| !is_external(&m.name))
                .map(|m| m.id)
                .collect()
        } else {
            mir.modules
                .iter()
                .filter(|m| {
                    hir.main_entity_names.iter().any(|n| {
                        // Exact match, or a monomorphized specialization of a
                        // main entity (e.g. "TmrCounter_8" for generic entity
                        // "TmrCounter"). Specialized top modules are not
                        // instantiated by anything, so they must be roots.
                        n == &m.name
                            || (m.name.len() > n.len()
                                && m.name.starts_with(n.as_str())
                                && m.name.as_bytes()[n.len()] == b'_')
                    })
                })
                .map(|m| m.id)
                .collect()
        };
        let mut queue: VecDeque<crate::mir::ModuleId> = reachable.iter().copied().collect();
        while let Some(mid) = queue.pop_front() {
            if let Some(module) = mir.modules.iter().find(|m| m.id == mid) {
                for inst in &module.instances {
                    if reachable.insert(inst.module) {
                        queue.push_back(inst.module);
                    }
                }
            }
        }
        mir.modules
            .iter()
            .filter(|m| reachable.contains(&m.id))
            .map(|m| m.name.clone())
            .collect()
    }

    /// One HIR→MIR transform pass, returning owned results so the transformer
    /// (and its borrow of the HIR) does not outlive the call.
    #[allow(clippy::type_complexity)]
    fn run_transform(
        hir: &Hir,
        module_hirs: &IndexMap<PathBuf, Hir>,
    ) -> (
        Mir,
        Vec<(String, String)>,
        IndexMap<
            String,
            (
                String,
                Vec<(String, skalp_frontend::const_eval::ConstValue)>,
            ),
        >,
    ) {
        let mut transformer = HirToMir::new_with_modules(module_hirs);
        let mir = transformer.transform(hir);
        let conversion_errors = transformer.conversion_errors().to_vec();
        let pending = transformer.pending_entity_specializations();
        (mir, conversion_errors, pending)
    }

    /// Specialize the pending generic entities at HIR level and append the
    /// specializations to `base`. Returns true if at least one specialization
    /// was added (i.e. re-running the transform can make progress).
    ///
    /// The generic entity + implementation are looked up in `base` first, then
    /// in each module HIR (entity and impl must come from the SAME Hir — ids
    /// collide across HIRs). The specialized entity keeps the exact name the
    /// instantiation site constructed, so the re-run finds it by name.
    fn apply_pending_specializations(
        base: &mut Hir,
        pending: &PendingSpecializations,
        module_hirs: &IndexMap<PathBuf, Hir>,
    ) -> bool {
        use skalp_frontend::monomorphization::{Instantiation, MonomorphizationEngine};

        let mut next_entity_id = base
            .entities
            .iter()
            .map(|e| e.id.0)
            .chain(
                module_hirs
                    .values()
                    .flat_map(|h| h.entities.iter().map(|e| e.id.0)),
            )
            .max()
            .unwrap_or(0)
            + 1;
        let mut next_port_id: u32 = base
            .entities
            .iter()
            .flat_map(|e| e.ports.iter())
            .map(|p| p.id.0)
            .chain(
                module_hirs
                    .values()
                    .flat_map(|h| h.entities.iter().flat_map(|e| e.ports.iter()))
                    .map(|p| p.id.0),
            )
            .max()
            .unwrap_or(0)
            + 1;

        let mut engine = MonomorphizationEngine::new();
        let mut added = false;
        for (specialized_name, (generic_name, const_args)) in pending {
            if base.entities.iter().any(|e| e.name == *specialized_name) {
                continue;
            }
            // Find the generic entity and its implementation from the same HIR
            let found = std::iter::once(&*base)
                .chain(module_hirs.values())
                .find_map(|h| {
                    let e = h.entities.iter().find(|e| e.name == *generic_name)?;
                    let i = h.implementations.iter().find(|i| i.entity == e.id).cloned();
                    Some((e.clone(), i))
                });
            let Some((generic_entity, generic_impl)) = found else {
                continue;
            };

            let instantiation = Instantiation {
                entity_name: generic_entity.name.clone(),
                entity_id: generic_entity.id,
                type_args: IndexMap::new(),
                const_args: const_args.iter().cloned().collect(),
                intent_args: IndexMap::new(),
            };

            let (mut spec_entity, port_id_map) = engine.specialize_entity(
                &generic_entity,
                &instantiation,
                skalp_frontend::hir::EntityId(next_entity_id),
                &mut next_port_id,
            );
            next_entity_id += 1;
            // Keep the exact name the instantiation site will look up — the
            // engine's mangling sorts params alphabetically, which can differ
            // from declaration order for multi-parameter entities.
            spec_entity.name = specialized_name.clone();

            let spec_impl = generic_impl.map(|gi| {
                let mut si = engine.specialize_implementation(
                    &gi,
                    &spec_entity,
                    &instantiation,
                    &port_id_map,
                );
                si.entity = spec_entity.id;
                si
            });

            base.entities.push(spec_entity);
            if let Some(si) = spec_impl {
                base.implementations.push(si);
            }
            added = true;
        }
        added
    }

    /// Apply optimization passes based on optimization level
    fn apply_optimizations(&self, mir: &mut Mir) {
        match self.opt_level {
            OptimizationLevel::None => {}
            OptimizationLevel::Basic => {
                // Apply dead code elimination
                self.apply_pass(mir, &mut DeadCodeElimination::new());
            }
            OptimizationLevel::Full => {
                // Apply all optimizations in order
                self.apply_pass(mir, &mut ConstantFolding::new());
                self.apply_pass(mir, &mut DeadCodeElimination::new());
            }
        }
    }

    /// Apply a single optimization pass
    fn apply_pass(&self, mir: &mut Mir, pass: &mut dyn OptimizationPass) {
        pass.apply(mir);
    }

    /// Perform CDC analysis on all modules in the MIR
    fn perform_cdc_analysis(&self, mir: &Mir) -> Vec<CdcViolation> {
        let mut all_violations = Vec::new();

        for module in &mir.modules {
            let mut analyzer = CdcAnalyzer::new();
            let mut violations = analyzer.analyze_module(module);
            // Carry the module name for diagnostics (TRIAGE #10)
            for v in &mut violations {
                if v.location.is_none() {
                    v.location = Some(module.name.clone());
                }
            }
            all_violations.extend(violations);
        }

        all_violations
    }

    /// Report CDC violations to the user
    fn report_cdc_violations(&self, violations: &[CdcViolation]) {
        if violations.is_empty() {
            return;
        }

        // TRIAGE #10: the rendering below had been stripped ("Violation
        // details removed") — critical violations failed the build with only
        // a count. Restored: one line per violation with severity, type,
        // module (carried in `location`), and the analyzer's description.
        for violation in violations.iter() {
            let severity_str = match violation.severity {
                CdcSeverity::Critical => "CRITICAL",
                CdcSeverity::Warning => "WARNING",
                CdcSeverity::Info => "INFO",
            };

            let violation_type_str = match violation.violation_type {
                crate::cdc_analysis::CdcViolationType::DirectCrossing => {
                    "direct clock-domain crossing"
                }
                crate::cdc_analysis::CdcViolationType::CombinationalMixing => {
                    "combinational logic mixing"
                }
                crate::cdc_analysis::CdcViolationType::AsyncResetCrossing => "async reset crossing",
                crate::cdc_analysis::CdcViolationType::ArithmeticMixing => "arithmetic mixing",
            };

            let where_str = violation
                .location
                .as_deref()
                .map(|l| format!(" [{}]", l))
                .unwrap_or_default();
            eprintln!(
                "CDC {}: {}{}: {}",
                severity_str, violation_type_str, where_str, violation.description
            );
        }

        // Summary
        let critical_count = violations
            .iter()
            .filter(|v| v.severity == CdcSeverity::Critical)
            .count();
        let warning_count = violations
            .iter()
            .filter(|v| v.severity == CdcSeverity::Warning)
            .count();
        let info_count = violations
            .iter()
            .filter(|v| v.severity == CdcSeverity::Info)
            .count();

        eprintln!(
            "CDC analysis: {} critical, {} warning(s), {} info",
            critical_count, warning_count, info_count
        );
    }
}

impl Default for MirCompiler {
    fn default() -> Self {
        Self::new()
    }
}

// Old convenience functions removed - use skalp_codegen directly for code generation

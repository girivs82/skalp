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

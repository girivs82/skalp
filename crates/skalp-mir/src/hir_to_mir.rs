//! HIR to MIR transformation
//!
//! This module converts the high-level HIR representation
//! to the mid-level MIR suitable for code generation

use crate::mir::*;
use crate::type_flattening::{FlattenedField as TypeFlattenedField, TypeFlattener};
use crate::{ExpressionKind, Type};
use indexmap::IndexMap;
use skalp_frontend::const_eval::{ConstEvaluator, ConstValue};
use skalp_frontend::hir::{self as hir, DetectionConfig, Hir};
use skalp_frontend::span::SourceSpan;
use skalp_frontend::types::Width;
use std::path::PathBuf;
use std::sync::OnceLock;

/// Check if debug output is enabled (set SKALP_MIR_DEBUG=1 to enable)
/// This is cached to avoid repeated environment variable lookups
fn debug_enabled() -> bool {
    static DEBUG: OnceLock<bool> = OnceLock::new();
    *DEBUG.get_or_init(|| {
        std::env::var("SKALP_MIR_DEBUG")
            .map(|v| v == "1")
            .unwrap_or(false)
    })
}

/// Debug print macro that only prints when SKALP_MIR_DEBUG=1
macro_rules! debug_println {
    ($($arg:tt)*) => {
        if debug_enabled() {
            println!($($arg)*);
        }
    };
}

/// Debug eprint macro that only prints when SKALP_MIR_DEBUG=1
macro_rules! debug_eprintln {
    ($($arg:tt)*) => {
        if debug_enabled() {
            eprintln!($($arg)*);
        }
    };
}

/// Maximum recursion depth for type inference and expression annotation
/// This prevents stack overflow on deeply nested expressions like {{{{{...}}}}}
/// Increased to 32768 to support complex match expressions with nested function calls (e.g., exec_l4_l5)
/// Note: This extremely high depth is required for deeply nested vector operations in ray-triangle intersection
const MAX_EXPRESSION_RECURSION_DEPTH: usize = 32768;

/// Maximum number of function calls in a function body before using module instantiation instead of inlining
/// Functions with > this many calls will be synthesized as separate modules (like Verilog modules)
/// Functions with <= this many calls will be inlined (existing behavior)
/// This hybrid approach ensures:
/// - Simple functions (fp_mul, fp_sub, etc.) are inlined for efficiency
/// - Complex functions (quadratic_solve, etc.) become modules for correctness and scalability
const MAX_INLINE_CALL_COUNT: usize = 5;

/// Information about a flattened port or signal field
#[derive(Debug, Clone)]
struct FlattenedField {
    /// MIR port/signal ID for this field
    id: u32,
    /// Field path (e.g., ["position", "x"] for vertex.position.x)
    field_path: Vec<String>,
    /// Leaf type of this field
    leaf_type: DataType,
}

/// HIR to MIR transformer
#[allow(clippy::type_complexity)]
pub struct HirToMir<'hir> {
    /// Next module ID
    next_module_id: u32,
    /// Next port ID
    next_port_id: u32,
    /// Next signal ID
    next_signal_id: u32,
    /// Next variable ID
    next_variable_id: u32,
    /// Next process ID
    next_process_id: u32,
    /// Next clock domain ID
    next_clock_domain_id: u32,
    /// Next match expression ID (for globally unique match arm variable prefixes)
    /// BUG FIX #6: Prevents variable name collisions when multiple functions with
    /// match expressions are inlined into the same entity
    next_match_id: u32,
    /// Entity to module ID mapping
    entity_map: IndexMap<hir::EntityId, ModuleId>,
    /// Function to module ID mapping (for module instantiation instead of inlining)
    /// Maps function name -> ModuleId
    function_map: IndexMap<String, ModuleId>,
    /// Port ID mapping (HIR to MIR) - now 1-to-many for flattened structs
    port_map: IndexMap<hir::PortId, PortId>,
    /// Flattened ports: HIR port ID -> list of flattened MIR ports
    flattened_ports: IndexMap<hir::PortId, Vec<FlattenedField>>,
    /// Reverse port lookup: MIR port ID -> (HIR port ID, field path)
    /// BUG #33 FIX: This allows finding the correct HIR port for a given MIR port
    port_to_hir: IndexMap<PortId, (hir::PortId, Vec<String>)>,
    /// Signal ID mapping (HIR to MIR) - now 1-to-many for flattened structs
    signal_map: IndexMap<hir::SignalId, SignalId>,
    /// Flattened signals: HIR signal ID -> list of flattened MIR signals
    flattened_signals: IndexMap<hir::SignalId, Vec<FlattenedField>>,
    /// Reverse signal lookup: MIR signal ID -> (HIR signal ID, field path)
    /// BUG #33 FIX: This allows finding the correct HIR signal for a given MIR signal
    signal_to_hir: IndexMap<SignalId, (hir::SignalId, Vec<String>)>,
    /// Variable ID mapping (HIR to MIR)
    variable_map: IndexMap<hir::VariableId, VariableId>,
    /// Context-aware variable mapping for match arm isolation
    /// Maps (match_arm_prefix, HIR VariableId) -> MIR VariableId
    /// This prevents HIR VariableId collisions across different match arms
    context_variable_map: IndexMap<(Option<String>, hir::VariableId), VariableId>,
    /// MIR VariableId to name mapping for reverse lookups
    /// CRITICAL FIX #IMPORT_MATCH: This prevents name lookup errors when
    /// variable_map contains collisions (multiple HIR IDs mapping to same MIR ID)
    mir_variable_names: IndexMap<VariableId, String>,
    /// MIR VariableId to HIR type mapping
    /// CRITICAL FIX #IMPORT_MATCH: This prevents type lookup errors when
    /// dynamic_variables contains collisions (same HIR ID for different variables)
    mir_variable_types: IndexMap<VariableId, hir::HirType>,
    /// Clock domain ID mapping (HIR to MIR)
    clock_domain_map: IndexMap<hir::ClockDomainId, ClockDomainId>,
    /// Reference to HIR for type resolution
    hir: Option<&'hir Hir>,
    /// All loaded module HIRs for proper scope resolution (Bug #84 fix)
    /// This allows resolving function calls in their original module scope
    module_hirs: IndexMap<PathBuf, Hir>,
    /// Current entity being converted (for generic parameter resolution)
    current_entity_id: Option<hir::EntityId>,
    /// Const evaluator for evaluating const expressions (including user-defined const functions)
    const_evaluator: ConstEvaluator,
    /// Track dynamically created variables (from let bindings in event blocks)
    /// Maps HIR VariableId to (MIR VariableId, name, type)
    dynamic_variables: IndexMap<hir::VariableId, (VariableId, String, hir::HirType)>,
    /// Track RHS expressions for dynamic variables (for module instance argument expansion)
    /// Maps MIR VariableId to the converted RHS expression
    /// This allows expanding variable references to their actual values when used as module args
    dynamic_variable_rhs: IndexMap<VariableId, Expression>,
    /// Type flattener for consistent struct/vector expansion
    type_flattener: TypeFlattener,
    /// Pending statements from block expressions
    /// These need to be emitted before the current assignment/statement
    pending_statements: Vec<Statement>,
    /// Variable name prefix for match arm scoping
    /// When set, all variables created in this context will be prefixed
    /// to avoid name collisions between match arms
    match_arm_prefix: Option<String>,
    /// Entity instance name prefix that persists through function inlining
    /// BUG #198 FIX: Function inlining clears match_arm_prefix to prevent variable
    /// contamination, but entity instances still need unique names based on match
    /// arm context. This field is NOT cleared during function inlining.
    entity_instance_prefix: Option<String>,
    /// BUG #199 FIX: Global counter for unique entity instance IDs
    /// This ensures all entity instances have unique names regardless of context,
    /// preventing connection mixing when multiple instances have the same base name.
    next_entity_instance_id: u32,
    /// Function inlining context stack to prevent variable ID collisions
    /// Each function inlining pushes a unique context ID onto this stack
    /// This allows variables from different functions to coexist even if they have the same HIR VariableId
    inlining_context_stack: Vec<u32>,
    /// Next inlining context ID (incremented for each function inline)
    next_inlining_context_id: u32,
    /// BUG #160 FIX: Stack of expected return types during function inlining
    /// When inlining a function, push its return type so concat expressions can infer widths
    inlining_return_type_stack: Vec<Option<hir::HirType>>,
    /// Synthesized modules from functions (module instantiation instead of inlining)
    /// These modules will be added to the MIR output at the end of transformation
    synthesized_modules: Vec<Module>,
    /// Pending module instances from complex function calls
    /// These are created during expression conversion and added to the module after
    /// Format: (result_signal_ids, function_name, module_id, argument_expressions, hir_return_type, frontend_type)
    /// BUG FIX #92: Changed from SignalId to Vec<SignalId> to support tuple-returning functions
    pending_module_instances: Vec<(
        Vec<SignalId>,
        String,
        ModuleId,
        Vec<Expression>,
        Option<hir::HirType>,
        Type,
    )>,
    /// Entity instance output signals for hierarchical elaboration (Bug #13-16, #21-23 fix)
    /// Maps HIR VariableId (from let binding) to a map of port name -> SignalId
    /// Example: let inner = Inner { data }; -> inner maps to {"result" -> SignalId(5)}
    entity_instance_outputs: IndexMap<hir::VariableId, IndexMap<String, SignalId>>,
    /// Entity instance names for hierarchical elaboration
    /// Maps HIR VariableId to (instance_name, ModuleId)
    entity_instance_info: IndexMap<hir::VariableId, (String, ModuleId)>,
    /// Pending entity instances from let bindings (Bug #13-16, #21-23 fix)
    /// Format: (ModuleInstance, Vec<(SignalId, signal_name, DataType)>)
    /// Pending entity instances: (instance, Vec<(signal_id, name, type, detection_config)>)
    pending_entity_instances: Vec<(
        ModuleInstance,
        Vec<(SignalId, String, DataType, Option<DetectionConfig>)>,
    )>,
    /// Instance output signals by name for hierarchical elaboration (Bug #13-16, #21-23 fix)
    /// Maps instance name (String) -> port name -> SignalId
    /// This is used when FieldAccess has GenericParam("instance_name") as base
    instance_outputs_by_name: IndexMap<String, IndexMap<String, SignalId>>,
    /// BUG FIX #125: Cache for Call expression results in module context
    /// This prevents creating duplicate module instances when multiple FieldAccess expressions
    /// reference the same Call result (e.g., for tuple destructuring like `let (a, b, c) = func()`)
    /// Maps a string key representing the Call (function_name + serialized_args) to the cached result Expression
    module_call_cache: IndexMap<String, Expression>,
    /// Current pipeline style for flow block processing
    /// This is set from intent attributes and controls how |> operators insert registers
    current_pipeline_style: Option<hir::PipelineStyle>,
    /// Next pipeline register ID (for generating unique register names)
    next_pipeline_reg_id: u32,
    /// Loop unrolling substitution context
    /// When set, iterator variable references are replaced with the constant value
    /// Format: (iterator_var_id, constant_value)
    unroll_substitution: Option<(hir::VariableId, i64)>,
    /// Next generate block ID (for #[preserve_generate] mode)
    /// When preserve_generate is used, generate statements are converted to MIR GenerateBlocks
    /// instead of being elaborated at compile time
    next_generate_block_id: u32,
    /// BUG #178 FIX: Pending MIR parameter substitutions for inline_function_call_with_mir_args
    /// Maps parameter name to pre-converted MIR expression
    /// This is set during inline_function_call_with_mir_args and checked during convert_expression
    /// for GenericParam handling
    pending_mir_param_subs: IndexMap<String, Expression>,

    /// BUG #181 FIX: Maps parameter VariableId to parameter name
    /// This is set during inline_function_call_with_mir_args so that Variable references
    /// to parameters can be resolved via pending_mir_param_subs
    pending_param_var_to_name: IndexMap<hir::VariableId, String>,

    /// BUG #167 FIX: Maps placeholder signal VariableIds to entity output SignalIds and their types
    /// During trait method inlining, when `let adder = FpAdd{result: result}` is processed
    /// and `result` is a placeholder signal connected to entity output port `result`,
    /// this maps VariableId(result) -> (SignalId, Type) of the created `adder_result` wire.
    /// Used to correctly wire up return expressions that reference these placeholder signals.
    /// BUG #201 FIX: Now includes Type so convert_expression uses the correct expression type
    /// instead of the passed-in type which may be wrong (e.g., Bit(Fixed(1)) instead of Float32).
    placeholder_signal_to_entity_output: IndexMap<hir::VariableId, (SignalId, Type)>,

    /// BUG #190 FIX: Maps placeholder HIR SignalIds to entity output MIR SignalIds and their types
    /// Similar to placeholder_signal_to_entity_output, but for cases where the placeholder
    /// is an HIR Signal (from `signal result: fp32;`) rather than a Variable.
    /// When `let adder = FpAdd{result: result as fp32}` and `result` is an HIR Signal,
    /// this maps the original HIR SignalId to (SignalId, Type) of the entity output.
    /// BUG #201 FIX: Now includes Type for correct type propagation.
    placeholder_hir_signal_to_entity_output: IndexMap<hir::SignalId, (SignalId, Type)>,

    /// BUG #200 FIX: Current module synthesis context's var_to_signal mapping
    /// This is set when synthesizing a function as a module and is used by convert_expression
    /// to resolve Variable references to Signal references for entity struct literal fields.
    /// Without this, entity connections reference Variables that don't exist in the module.
    current_module_var_to_signal: Option<IndexMap<hir::VariableId, SignalId>>,

    /// BUG #205 FIX: Current module synthesis context's param_to_port mapping
    /// This is set when synthesizing a function as a module and is used by convert_expression
    /// to resolve GenericParam references to Port references for entity struct literal fields.
    /// Without this, entity connections with GenericParam references resolve to 0 instead of
    /// the correct input port.
    current_module_param_to_port: Option<IndexMap<String, PortId>>,
}

/// Context for converting HIR expressions within a synthesized module
/// This holds the mappings from variable names to their corresponding MIR representations
struct ModuleSynthesisContext {
    /// BUG #118 DEBUG: Track which function this context belongs to
    func_name: String,
    /// Maps parameter names to their input port IDs
    param_to_port: IndexMap<String, PortId>,
    /// BUG FIX #85: Maps HIR VariableId to signal ID (not by name, to avoid collisions)
    /// Different match arms can have variables with the same name but different IDs
    var_to_signal: IndexMap<hir::VariableId, SignalId>,
    /// Maps names to their types (for type inference)
    name_to_type: IndexMap<String, hir::HirType>,
    /// Maps HIR VariableId to variable name (for resolving variable references)
    var_id_to_name: IndexMap<hir::VariableId, String>,
    /// BUG FIX #130: Maps tuple temp VariableIds to their converted MIR expressions (Concat)
    /// These are used for tuple destructuring: `let (a, b) = func()` creates
    /// `let _tuple_tmp_N = func()` and we store the Concat here so FieldAccess can extract elements
    tuple_temp_to_mir: IndexMap<hir::VariableId, Expression>,
}

impl ModuleSynthesisContext {
    fn new(func_name: &str) -> Self {
        Self {
            func_name: func_name.to_string(),
            param_to_port: IndexMap::new(),
            var_to_signal: IndexMap::new(),
            name_to_type: IndexMap::new(),
            var_id_to_name: IndexMap::new(),
            tuple_temp_to_mir: IndexMap::new(),
        }
    }
}

/// Convert HIR SafetyConfig to MIR SafetyContext
fn convert_safety_config(config: &hir::SafetyConfig) -> SafetyContext {
    SafetyContext {
        implementing_goal: config.goal.clone(),
        // If there's an implements_path, this is a safety-related signal
        is_sm_signal: config.implements_path.is_some(),
        mechanism_name: config.mechanism.clone(),
        dc_override: config.dc_override,
        lc_override: config.lc_override,
    }
}

impl<'hir> HirToMir<'hir> {
    /// Create a new transformer
    pub fn new() -> Self {
        Self::new_with_modules(&IndexMap::new())
    }

    /// Create a new transformer with module HIRs for proper scope resolution (Bug #84 fix)
    pub fn new_with_modules(module_hirs: &IndexMap<PathBuf, Hir>) -> Self {
        Self {
            next_module_id: 0,
            next_port_id: 0,
            next_signal_id: 0,
            next_variable_id: 0,
            next_process_id: 0,
            next_clock_domain_id: 0,
            next_match_id: 0, // BUG FIX #6
            entity_map: IndexMap::new(),
            function_map: IndexMap::new(),
            port_map: IndexMap::new(),
            flattened_ports: IndexMap::new(),
            port_to_hir: IndexMap::new(),
            signal_map: IndexMap::new(),
            flattened_signals: IndexMap::new(),
            signal_to_hir: IndexMap::new(),
            variable_map: IndexMap::new(),
            context_variable_map: IndexMap::new(),
            mir_variable_names: IndexMap::new(),
            mir_variable_types: IndexMap::new(),
            clock_domain_map: IndexMap::new(),
            hir: None,
            module_hirs: module_hirs.clone(),
            current_entity_id: None,
            const_evaluator: ConstEvaluator::new(),
            dynamic_variables: IndexMap::new(),
            dynamic_variable_rhs: IndexMap::new(),
            type_flattener: TypeFlattener::new(0), // Will be re-initialized per use
            pending_statements: Vec::new(),
            match_arm_prefix: None,
            entity_instance_prefix: None,
            next_entity_instance_id: 0,
            inlining_context_stack: Vec::new(),
            next_inlining_context_id: 0,
            inlining_return_type_stack: Vec::new(),
            synthesized_modules: Vec::new(),
            pending_module_instances: Vec::new(),
            entity_instance_outputs: IndexMap::new(),
            entity_instance_info: IndexMap::new(),
            pending_entity_instances: Vec::new(),
            instance_outputs_by_name: IndexMap::new(),
            module_call_cache: IndexMap::new(),
            current_pipeline_style: None,
            next_pipeline_reg_id: 0,
            unroll_substitution: None,
            next_generate_block_id: 0,
            pending_mir_param_subs: IndexMap::new(),
            pending_param_var_to_name: IndexMap::new(),
            placeholder_signal_to_entity_output: IndexMap::new(),
            placeholder_hir_signal_to_entity_output: IndexMap::new(),
            current_module_var_to_signal: None, // BUG #200 FIX
            current_module_param_to_port: None, // BUG #205 FIX
        }
    }

    /// Transform HIR to MIR
    pub fn transform(&mut self, hir: &'hir Hir) -> Mir {
        use std::time::Instant;
        let transform_start = Instant::now();

        self.hir = Some(hir);
        // Register all user-defined const functions in the evaluator
        self.const_evaluator.register_functions(&hir.functions);

        // BUG #208 FIX: Register all implementation constants in the evaluator
        // This is needed so that module-level constants like IEEE754_32 can be
        // resolved when evaluating generic_args in StructLiteral entity instantiations.
        for implementation in &hir.implementations {
            self.const_evaluator
                .register_constants(&implementation.constants);
        }

        let mut mir = Mir::new(hir.name.clone());

        // Propagate safety definitions from HIR to MIR
        mir.safety_definitions = hir.safety_definitions.clone();

        println!(
            "[HIR_TO_MIR] transform: hir has {} entities, {} implementations",
            hir.entities.len(),
            hir.implementations.len()
        );

        // First pass: create modules for entities
        for entity in &hir.entities {
            if entity.name.contains("FpAdd")
                || entity.name == "KarythraCLEAsync"
                || entity.name == "FpSub"
            {
                println!(
                    "[HIR_TO_MIR] First pass: entity '{}' with id {:?}",
                    entity.name, entity.id
                );
            }
            let entity_start = Instant::now();
            let module_id = self.next_module_id();
            self.entity_map.insert(entity.id, module_id);
            if entity.name.contains("FpAdd")
                || entity.name == "KarythraCLEAsync"
                || entity.name == "FpSub"
            {
                println!(
                    "[HIR_TO_MIR] entity_map[{:?}] = {:?} for '{}'",
                    entity.id, module_id, entity.name
                );
            }

            let mut module = Module::new(module_id, entity.name.clone());

            // Propagate pipeline configuration from HIR entity to MIR module
            if let Some(ref config) = entity.pipeline_config {
                module.pipeline_config = Some(config.clone());
                eprintln!(
                    "🔧 PIPELINE: Propagating pipeline_config (stages={}) to entity '{}'",
                    config.stages, entity.name
                );
            }

            // Propagate vendor IP configuration from HIR entity to MIR module
            if let Some(ref config) = entity.vendor_ip_config {
                module.vendor_ip_config = Some(config.clone());
                debug_println!("🔧 VENDOR_IP: Propagating vendor_ip_config (ip={}, vendor={:?}) to entity '{}'",
                         config.ip_name, config.vendor, entity.name);
            }

            // Propagate compiled IP configuration from HIR entity to MIR module
            // When set, this module is a blackbox with pre-compiled netlist
            if let Some(ref config) = entity.compiled_ip_config {
                module.compiled_ip_config = Some(config.clone());
                eprintln!(
                    "📦 COMPILED_IP: Entity '{}' is a compiled IP from '{}'{}",
                    entity.name,
                    config.skb_path,
                    if config.encrypted { " (encrypted)" } else { "" }
                );
            }

            // Propagate power domains from HIR entity to MIR module
            if !entity.power_domains.is_empty() {
                module.power_domains = entity.power_domains.clone();
            }

            // Propagate power domain config for CCF analysis (from #[power_domain("name")] attribute)
            if let Some(ref config) = entity.power_domain_config {
                module.power_domain_config = Some(config.clone());
                eprintln!(
                    "🔌 POWER_DOMAIN: Propagating power_domain_config (domain={}) to entity '{}'",
                    config.domain_name, entity.name
                );
            }

            // Propagate safety mechanism config from HIR entity to MIR module
            if let Some(ref sm_config) = entity.safety_mechanism_config {
                module.safety_context = Some(SafetyContext {
                    implementing_goal: None, // Mechanism itself doesn't implement a goal
                    is_sm_signal: true,      // Entity marked as safety mechanism
                    mechanism_name: sm_config.mechanism_type.clone(),
                    dc_override: sm_config.dc,
                    lc_override: sm_config.lc,
                });
            }

            // Propagate async (NCL) flag from HIR entity to MIR module
            // NCL modules use dual-rail encoding and have no clocks
            if entity.is_async {
                module.is_async = true;
                eprintln!(
                    "⚡ NCL: Entity '{}' is async (will use dual-rail NCL synthesis)",
                    entity.name
                );
            }

            // Convert generic parameters
            for hir_generic in &entity.generics {
                let parameter = GenericParameter {
                    name: hir_generic.name.clone(),
                    param_type: self.convert_generic_type(&hir_generic.param_type),
                    default: hir_generic
                        .default_value
                        .as_ref()
                        .and_then(|expr| self.convert_literal_expr(expr)),
                };
                module.parameters.push(parameter);
            }

            // Convert ports - flatten structs/vectors into individual ports
            for hir_port in &entity.ports {
                let port_type = self.convert_type(&hir_port.port_type);
                let direction = self.convert_port_direction(&hir_port.direction);

                let (flattened_ports, flattened_fields) = self.flatten_port(
                    &hir_port.name,
                    &port_type,
                    direction,
                    hir_port.physical_constraints.clone(),
                    None, // TODO: Add span to HirPort
                );

                // BUG #29 FIX: Track ALL flattened composite types, not just multi-field ones
                // For single-field structs like TestData{value}, we still need to track the flattening
                // because the type changed (Struct→Bit) and the name changed (rd_data→rd_data_value)
                // This matches the signal flattening logic (Bug #21 fix)
                if !flattened_fields.is_empty() {
                    self.flattened_ports
                        .insert(hir_port.id, flattened_fields.clone());
                }

                // BUG #33 FIX: Also populate reverse map for each flattened port
                for field in &flattened_fields {
                    self.port_to_hir
                        .insert(PortId(field.id), (hir_port.id, field.field_path.clone()));
                }

                // For simple mapping (first port or single non-struct port)
                if let Some(first_port) = flattened_ports.first() {
                    self.port_map.insert(hir_port.id, first_port.id);
                }

                // Add all flattened ports to the module, propagating detection_config
                for mut port in flattened_ports {
                    // Propagate the detection config from HIR
                    port.detection_config = hir_port.detection_config.clone();
                    module.ports.push(port);
                }
            }

            // Process entity body signals and assignments (before impl blocks)
            eprintln!(
                "[HIR→MIR] Processing entity '{}' with {} signals and {} assignments",
                entity.name,
                entity.signals.len(),
                entity.assignments.len()
            );

            // Set current entity for generic parameter resolution
            self.current_entity_id = Some(entity.id);

            // Convert entity body signals - flatten structs/vectors into individual signals
            for hir_signal in &entity.signals {
                let signal_type = self.convert_type(&hir_signal.signal_type);
                let initial = hir_signal
                    .initial_value
                    .as_ref()
                    .and_then(|expr| self.convert_literal_expr(expr));
                let clock_domain = hir_signal.clock_domain.map(|id| ClockDomainId(id.0));

                let (flattened_signals, flattened_fields) = self.flatten_signal(
                    &hir_signal.name,
                    &signal_type,
                    initial,
                    clock_domain,
                    hir_signal.span.clone(),
                );

                if !flattened_fields.is_empty() {
                    self.flattened_signals
                        .insert(hir_signal.id, flattened_fields.clone());
                }

                for field in &flattened_fields {
                    self.signal_to_hir.insert(
                        SignalId(field.id),
                        (hir_signal.id, field.field_path.clone()),
                    );
                }

                if let Some(first_signal) = flattened_signals.first() {
                    self.signal_map.insert(hir_signal.id, first_signal.id);
                }

                for mut signal in flattened_signals {
                    // Propagate memory config from HIR signal to MIR signal
                    // Memory signals should typically not flatten (they're arrays)
                    if hir_signal.memory_config.is_some() {
                        signal.memory_config = hir_signal.memory_config.clone();
                    }
                    // Propagate trace config from HIR signal to MIR signal
                    if hir_signal.trace_config.is_some() {
                        signal.trace_config = hir_signal.trace_config.clone();
                    }
                    // Propagate CDC config from HIR signal to MIR signal
                    if hir_signal.cdc_config.is_some() {
                        signal.cdc_config = hir_signal.cdc_config.clone();
                    }
                    // Propagate breakpoint config from HIR signal to MIR signal
                    if hir_signal.breakpoint_config.is_some() {
                        signal.breakpoint_config = hir_signal.breakpoint_config.clone();
                    }
                    // Propagate power config from HIR signal to MIR signal
                    if hir_signal.power_config.is_some() {
                        signal.power_config = hir_signal.power_config.clone();
                    }
                    // Propagate power domain from HIR signal to MIR signal
                    if hir_signal.power_domain.is_some() {
                        signal.power_domain = hir_signal.power_domain.clone();
                    }
                    // Propagate safety config from HIR signal to MIR signal
                    if let Some(ref safety_config) = hir_signal.safety_config {
                        signal.safety_context = Some(convert_safety_config(safety_config));
                    }
                    module.signals.push(signal);
                }

                // BUG FIX: Generate continuous assignment for signals with non-literal initial expressions
                // e.g., "signal dx: fp32 = x2 - x1" needs to generate an assignment dx = x2 - x1
                if let Some(init_expr) = &hir_signal.initial_value {
                    // Only generate assignment if it's NOT a literal (literals are handled above)
                    if !matches!(init_expr, hir::HirExpression::Literal(_)) {
                        eprintln!(
                            "[HIR→MIR] Signal '{}' has non-literal initial_value, generating continuous assign",
                            hir_signal.name
                        );
                        // Convert the expression
                        if let Some(rhs) = self.convert_expression(init_expr, 0) {
                            // Get the first MIR signal ID for this HIR signal
                            if let Some(&mir_signal_id) = self.signal_map.get(&hir_signal.id) {
                                let continuous = ContinuousAssign {
                                    lhs: LValue::Signal(mir_signal_id),
                                    rhs,
                                    span: None,
                                };
                                module.assignments.push(continuous);
                                eprintln!(
                                    "[HIR→MIR] Created ContinuousAssign for signal '{}' -> SignalId({:?})",
                                    hir_signal.name, mir_signal_id
                                );
                            }
                        }
                    }
                }
            }

            // Convert entity body assignments (may expand to multiple for structs)
            for (idx, hir_assign) in entity.assignments.iter().enumerate() {
                eprintln!(
                    "[HIR→MIR] Entity body assignment {}: LHS={:?}, RHS={:?}",
                    idx,
                    std::mem::discriminant(&hir_assign.lhs),
                    std::mem::discriminant(&hir_assign.rhs)
                );

                // Clear any pending statements from previous assignments
                self.pending_statements.clear();

                // Convert the assignment
                let mir_assigns = self.convert_continuous_assignment_expanded(hir_assign);

                // Add any pending statements (from block expressions)
                for pending_stmt in self.pending_statements.drain(..) {
                    if let Statement::Assignment(assign) = pending_stmt {
                        let continuous = ContinuousAssign {
                            lhs: assign.lhs,
                            rhs: assign.rhs,
                            span: None,
                        };
                        module.assignments.push(continuous);
                    }
                }

                // Drain pending module instances (from complex function calls)
                self.drain_pending_module_instances(&mut module);

                // Drain pending entity instances (from let bindings with entity instantiation)
                self.drain_pending_entity_instances(&mut module);

                // Add the main assignments
                module.assignments.extend(mir_assigns);
            }

            eprintln!(
                "⏱️  [PERF]   Finished entity '{}' in {:?}",
                entity.name,
                entity_start.elapsed()
            );
            mir.add_module(module);
        }

        // Second pass: add implementations
        for impl_block in &hir.implementations {
            let impl_start = Instant::now();
            // Debug: Find entity name
            let impl_entity_name = hir
                .entities
                .iter()
                .find(|e| e.id == impl_block.entity)
                .map(|e| e.name.clone())
                .unwrap_or_else(|| "?".to_string());
            if impl_entity_name.contains("FpAdd") {
                println!(
                    "[HIR_TO_MIR] Second pass: impl for '{}' ({:?}), {} signals",
                    impl_entity_name,
                    impl_block.entity,
                    impl_block.signals.len()
                );
            }
            if let Some(&module_id) = self.entity_map.get(&impl_block.entity) {
                // Set current entity for generic parameter resolution
                self.current_entity_id = Some(impl_block.entity);

                // Skip implementations for entities that are not used (0 instances) and are not the first entity
                if let Some(entity) = hir.entities.iter().find(|e| e.id == impl_block.entity) {
                    // Check if this is a generic entity (has unresolved generics)
                    let has_unresolved_generics = entity.generics.iter().any(|g| {
                        g.default_value.is_none()
                            && !matches!(g.param_type, hir::HirGenericType::ClockDomain)
                    });

                    // Skip if: has unresolved generics AND no instances
                    // This skips unspecialized generic entities that are never instantiated
                    if has_unresolved_generics && impl_block.instances.is_empty() {
                        continue;
                    }

                    // NOTE: We do NOT skip monomorphized entities even if they have 0 instances in their own impl block.
                    // Monomorphized entities (like FpAdd_fp32) are created by monomorphization because some other
                    // entity instantiates them. The instances are registered in the parent entity's impl block,
                    // not in the monomorphized entity's own impl block.
                    // Skipping them would cause their signals/logic to be missing from the final output.
                }

                // Bind generic parameter default values into const evaluator for expression evaluation
                // Track which names we bound so we can clean them up later
                let mut bound_generic_names = Vec::new();
                if let Some(entity) = hir.entities.iter().find(|e| e.id == impl_block.entity) {
                    for generic in &entity.generics {
                        if let Some(default_expr) = &generic.default_value {
                            // Try to evaluate the default value and bind it
                            if let Ok(const_val) = self.const_evaluator.eval(default_expr) {
                                self.const_evaluator.bind(generic.name.clone(), const_val);
                                bound_generic_names.push(generic.name.clone());
                            }
                        }
                    }
                }

                // Find the module
                if let Some(module) = mir.modules.iter_mut().find(|m| m.id == module_id) {
                    // Add signals - flatten structs/vectors into individual signals
                    for hir_signal in &impl_block.signals {
                        let signal_type = self.convert_type(&hir_signal.signal_type);
                        let initial = hir_signal
                            .initial_value
                            .as_ref()
                            .and_then(|expr| self.convert_literal_expr(expr));
                        let clock_domain = hir_signal.clock_domain.map(|id| ClockDomainId(id.0));

                        let (flattened_signals, flattened_fields) = self.flatten_signal(
                            &hir_signal.name,
                            &signal_type,
                            initial,
                            clock_domain,
                            hir_signal.span.clone(),
                        );
                        // CRITICAL FIX (Bug #21): Store flattening info for ALL composite types
                        // Even single-field structs need mapping because field name != signal name
                        // (e.g., signal "data: SimpleData" → flattened signal "data_value")
                        if !flattened_fields.is_empty() {
                            self.flattened_signals
                                .insert(hir_signal.id, flattened_fields.clone());
                        }

                        // BUG #33 FIX: Also populate reverse map for each flattened signal
                        for field in &flattened_fields {
                            self.signal_to_hir.insert(
                                SignalId(field.id),
                                (hir_signal.id, field.field_path.clone()),
                            );
                        }

                        // For simple mapping (first signal or single non-struct signal)
                        if let Some(first_signal) = flattened_signals.first() {
                            self.signal_map.insert(hir_signal.id, first_signal.id);
                        }

                        // Add all flattened signals to the module
                        for mut signal in flattened_signals {
                            // Propagate memory config from HIR signal to MIR signal
                            // Memory signals should typically not flatten (they're arrays)
                            if hir_signal.memory_config.is_some() {
                                signal.memory_config = hir_signal.memory_config.clone();
                            }
                            // Propagate trace config from HIR signal to MIR signal
                            if hir_signal.trace_config.is_some() {
                                signal.trace_config = hir_signal.trace_config.clone();
                            }
                            // Propagate CDC config from HIR signal to MIR signal
                            if hir_signal.cdc_config.is_some() {
                                signal.cdc_config = hir_signal.cdc_config.clone();
                            }
                            // Propagate breakpoint config from HIR signal to MIR signal
                            if hir_signal.breakpoint_config.is_some() {
                                signal.breakpoint_config = hir_signal.breakpoint_config.clone();
                            }
                            // Propagate power config from HIR signal to MIR signal
                            if hir_signal.power_config.is_some() {
                                signal.power_config = hir_signal.power_config.clone();
                            }
                            // Propagate power domain from HIR signal to MIR signal
                            if hir_signal.power_domain.is_some() {
                                signal.power_domain = hir_signal.power_domain.clone();
                            }
                            // Propagate safety config from HIR signal to MIR signal
                            if let Some(ref safety_config) = hir_signal.safety_config {
                                signal.safety_context = Some(convert_safety_config(safety_config));
                            }
                            module.signals.push(signal);
                        }

                        // BUG FIX: Generate continuous assignment for impl signals with non-literal initial expressions
                        // e.g., "signal dx: fp32 = x2 - x1" needs to generate an assignment dx = x2 - x1
                        // BUG #207 DEBUG: Trace all signals with initial values for FP entities
                        let is_fp_module = module.name.contains("FpAdd")
                            || module.name.contains("FpSub")
                            || module.name.contains("FpMul");
                        let has_init = hir_signal.initial_value.is_some();
                        if is_fp_module {
                            println!(
                                "[BUG #207 TRACE] Module '{}' Signal '{}' initial_value: {}",
                                module.name, hir_signal.name, has_init
                            );
                        }
                        if has_init {
                            let init_expr = hir_signal.initial_value.as_ref().unwrap();
                            if is_fp_module {
                                println!(
                                    "[BUG #207 EXPR] Module '{}' Signal '{}' expr_type: {:?}",
                                    module.name,
                                    hir_signal.name,
                                    std::mem::discriminant(init_expr)
                                );
                            }
                        }
                        // Use separate variable to avoid borrow issues
                        let opt_init = &hir_signal.initial_value;
                        if is_fp_module {
                            println!(
                                "[BUG #207 PRE-IF] Module '{}' Signal '{}'",
                                module.name, hir_signal.name
                            );
                        }
                        if let Some(init_expr) = opt_init {
                            if is_fp_module {
                                // Debug: print expression type
                                println!(
                                    "[BUG #207 ENTRY] Module '{}' Signal '{}' is_literal: {}",
                                    module.name,
                                    hir_signal.name,
                                    matches!(init_expr, hir::HirExpression::Literal(_))
                                );
                            }
                            // Only generate assignment if it's NOT a literal (literals are handled above)
                            let is_literal = matches!(init_expr, hir::HirExpression::Literal(_));
                            if is_fp_module {
                                println!(
                                    "[BUG #207 NOT-LIT] Module '{}' Signal '{}' is_literal={}, entering non-literal block: {}",
                                    module.name, hir_signal.name, is_literal, !is_literal
                                );
                            }
                            if !is_literal {
                                if is_fp_module {
                                    println!(
                                        "[BUG #207 CONVERT] Module '{}' Signal '{}' about to call convert_expression",
                                        module.name, hir_signal.name
                                    );
                                }
                                // Convert the expression
                                let convert_result = self.convert_expression(init_expr, 0);
                                if is_fp_module {
                                    println!(
                                        "[BUG #207 RESULT] Module '{}' Signal '{}' convert_expression returned: {}",
                                        module.name, hir_signal.name, if convert_result.is_some() { "Some" } else { "None" }
                                    );
                                }
                                if let Some(rhs) = convert_result {
                                    // Get the first MIR signal ID for this HIR signal
                                    if let Some(&mir_signal_id) =
                                        self.signal_map.get(&hir_signal.id)
                                    {
                                        let continuous = ContinuousAssign {
                                            lhs: LValue::Signal(mir_signal_id),
                                            rhs,
                                            span: None,
                                        };
                                        module.assignments.push(continuous);
                                        eprintln!(
                                            "[HIR→MIR] Created ContinuousAssign for impl signal '{}' -> SignalId({:?})",
                                            hir_signal.name, mir_signal_id
                                        );
                                    }
                                } else {
                                    // BUG #207: Signal initializer conversion FAILED
                                    eprintln!(
                                        "⚠️ [BUG #207] SIGNAL INITIALIZER FAILED: Signal '{}' init_expr conversion returned None!",
                                        hir_signal.name
                                    );
                                    eprintln!(
                                        "⚠️ [BUG #207]   Expression type: {:?}",
                                        std::mem::discriminant(init_expr)
                                    );
                                    // Print more details based on expression type
                                    match init_expr {
                                        hir::HirExpression::Index(base, idx) => {
                                            eprintln!(
                                                "⚠️ [BUG #207]   Index: base={:?}, idx={:?}",
                                                std::mem::discriminant(&**base),
                                                std::mem::discriminant(&**idx)
                                            );
                                        }
                                        hir::HirExpression::Range(base, hi, lo) => {
                                            eprintln!(
                                                "⚠️ [BUG #207]   Range: base={:?}, hi={:?}, lo={:?}",
                                                std::mem::discriminant(&**base),
                                                std::mem::discriminant(&**hi),
                                                std::mem::discriminant(&**lo)
                                            );
                                        }
                                        hir::HirExpression::Cast(cast) => {
                                            eprintln!(
                                                "⚠️ [BUG #207]   Cast: inner={:?}, target={:?}",
                                                std::mem::discriminant(&*cast.expr),
                                                cast.target_type
                                            );
                                        }
                                        hir::HirExpression::Binary(bin) => {
                                            eprintln!(
                                                "⚠️ [BUG #207]   Binary: op={:?}, left={:?}, right={:?}",
                                                bin.op,
                                                std::mem::discriminant(&*bin.left),
                                                std::mem::discriminant(&*bin.right)
                                            );
                                        }
                                        _ => {}
                                    }
                                }
                            }
                        }
                    }
                    if module.name.contains("AsyncFifo_8") {
                        for sig in &module.signals {}
                    }

                    // Add variables
                    for hir_var in &impl_block.variables {
                        let var_id = self.next_variable_id();
                        self.variable_map.insert(hir_var.id, var_id);

                        let mir_var_type = self.convert_type(&hir_var.var_type);
                        let variable = Variable {
                            id: var_id,
                            name: hir_var.name.clone(),
                            var_type: mir_var_type,
                            initial: hir_var
                                .initial_value
                                .as_ref()
                                .and_then(|expr| self.convert_literal_expr(expr)),
                            span: None,
                        };
                        module.variables.push(variable);
                    }

                    // Convert event blocks to processes
                    for event_block in &impl_block.event_blocks {
                        let process = self.convert_event_block(event_block);
                        module.processes.push(process);
                    }

                    // Add any dynamically created variables (from let bindings in event blocks)
                    // Note: Duplicates are already handled at creation time - each unique name
                    // only gets one variable ID, which is reused by all let bindings with that name
                    let dynamic_vars: Vec<_> = self.dynamic_variables.values().cloned().collect();

                    // BUG #71 DEBUG: Check if var_148 is in dynamic_variables before adding
                    if let Some((_, name, _)) = self
                        .dynamic_variables
                        .values()
                        .find(|(id, _, _)| id.0 == 148)
                    {
                        eprintln!(
                            "[BUG #71 BEFORE LOC2] var_148 IS in dynamic_variables with name: {}",
                            name
                        );
                    } else {
                        eprintln!(
                            "[BUG #71 BEFORE LOC2] var_148 NOT in dynamic_variables (size={})",
                            self.dynamic_variables.len()
                        );
                    }

                    for (mir_var_id, name, hir_type) in dynamic_vars {
                        eprintln!(
                            "[DEBUG] Adding dynamic variable: name={}, hir_type={:?}",
                            name, hir_type
                        );
                        let mir_type = self.convert_type(&hir_type);
                        debug_println!("[DEBUG]   -> mir_type={:?}", mir_type);
                        // BUG #71 DEBUG
                        if name.contains("edge1")
                            || name.contains("edge2")
                            || name.contains("_h")
                            || name.contains("_s")
                        {
                            debug_println!("[BUG #71 EVENT BLOCK VAR] Adding variable '{}' (MIR {:?}): HIR={:?} -> MIR={:?}",
                                name, mir_var_id, hir_type, mir_type);
                        }
                        let variable = Variable {
                            id: mir_var_id,
                            name: name.clone(),
                            var_type: mir_type,
                            initial: None,
                            span: None,
                        };
                        eprintln!(
                            "[BUG #71 PUSH LOC2] Pushing dynamic variable: id={:?}, name={}",
                            mir_var_id, name
                        );
                        module.variables.push(variable);
                    }
                    // Clear dynamic variables for next impl block
                    self.dynamic_variables.clear();

                    // BUG FIX #13-16, #21-23: Pre-process instances BEFORE assignments
                    // This creates signals for instance output ports so FieldAccess can resolve them
                    self.instance_outputs_by_name.clear();
                    println!(
                        "🔍 [INSTANCE_PRE] impl_block for module '{}' has {} instances",
                        module.name,
                        impl_block.instances.len()
                    );
                    for hir_instance in &impl_block.instances {
                        println!(
                            "🔍 [INSTANCE_LOOP] Processing instance '{}' of entity {:?}",
                            hir_instance.name, hir_instance.entity
                        );
                        // Find the entity to get its output ports
                        if let Some(entity) = self
                            .hir
                            .as_ref()
                            .and_then(|h| h.entities.iter().find(|e| e.id == hir_instance.entity))
                        {
                            println!(
                                "🔍 [INSTANCE_LOOP] Found entity '{}' with {} ports",
                                entity.name,
                                entity.ports.len()
                            );
                            let mut output_ports: IndexMap<String, SignalId> = IndexMap::new();

                            // Create signals for each output port
                            for port in &entity.ports {
                                if matches!(port.direction, hir::HirPortDirection::Output) {
                                    println!(
                                        "🔍 [OUTPUT_PORT] Port '{}' is_detection={}",
                                        port.name,
                                        port.is_detection_signal()
                                    );
                                    let signal_id = self.next_signal_id();
                                    let signal_name =
                                        format!("{}_{}", hir_instance.name, port.name);
                                    let signal_type = self.convert_type(&port.port_type);

                                    // Create the signal and add to module
                                    // Propagate detection signal config from sub-module port
                                    let signal = Signal {
                                        id: signal_id,
                                        name: signal_name.clone(),
                                        signal_type: signal_type.clone(),
                                        initial: None,
                                        clock_domain: None,
                                        span: None,
                                        memory_config: None,
                                        trace_config: None,
                                        cdc_config: None,
                                        breakpoint_config: None,
                                        power_config: None,
                                        safety_context: None,
                                        detection_config: port.detection_config.clone(),
                                        power_domain: port
                                            .power_domain_config
                                            .as_ref()
                                            .map(|c| c.domain_name.clone()),
                                    };
                                    module.signals.push(signal);

                                    output_ports.insert(port.name.clone(), signal_id);

                                    // BUG FIX: Also propagate detection flag to the connected signal
                                    // in the parent module (if this output port connects to a local signal)
                                    if port.is_detection_signal() {
                                        // Find the connection to this port
                                        for conn in &hir_instance.connections {
                                            if conn.port == port.name {
                                                // Get the signal ID from the RHS expression
                                                if let hir::HirExpression::Signal(hir_sig_id) =
                                                    &conn.expr
                                                {
                                                    if let Some(&mir_sig_id) =
                                                        self.signal_map.get(hir_sig_id)
                                                    {
                                                        // Mark the parent's connected signal
                                                        if let Some(parent_signal) = module
                                                            .signals
                                                            .iter_mut()
                                                            .find(|s| s.id == mir_sig_id)
                                                        {
                                                            println!(
                                                                "✅ [DETECTION_PROP] Marking '{}' (id={}) as detection from {}.{}",
                                                                parent_signal.name,
                                                                mir_sig_id.0,
                                                                hir_instance.name,
                                                                port.name
                                                            );
                                                            // Propagate detection config from port
                                                            parent_signal.detection_config =
                                                                port.detection_config.clone();
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }

                            self.instance_outputs_by_name
                                .insert(hir_instance.name.clone(), output_ports);
                        }
                    }

                    // Convert continuous assignments (may expand to multiple for structs)
                    eprintln!(
                        "⏱️  [PERF]     Processing {} assignments from impl block",
                        impl_block.assignments.len()
                    );
                    println!(
                        "🔴🔴🔴 BUG #127 DEBUG: impl_block has {} assignments 🔴🔴🔴",
                        impl_block.assignments.len()
                    );
                    for (i, a) in impl_block.assignments.iter().enumerate() {
                        println!(
                            "🔴🔴🔴 BUG #127 DEBUG: Assignment {}: LHS={:?}, RHS={:?} 🔴🔴🔴",
                            i,
                            std::mem::discriminant(&a.lhs),
                            std::mem::discriminant(&a.rhs)
                        );
                    }
                    for (idx, hir_assign) in impl_block.assignments.iter().enumerate() {
                        let assignment_start = Instant::now();
                        println!(
                            "🟢🟢🟢 FOR_LOOP_ENTRY: Processing assignment {}/{} 🟢🟢🟢",
                            idx + 1,
                            impl_block.assignments.len()
                        );
                        eprintln!(
                            "⏱️  [PERF]       Starting assignment {}/{}: LHS={:?}, RHS={:?}",
                            idx + 1,
                            impl_block.assignments.len(),
                            std::mem::discriminant(&hir_assign.lhs),
                            std::mem::discriminant(&hir_assign.rhs)
                        );
                        eprintln!(
                            "[TRAIT_DEBUG_ENTRY] Entering convert_continuous_assignment_expanded for assignment {}", idx
                        );
                        // Clear any pending statements from previous assignments
                        self.pending_statements.clear();

                        // Convert the assignment (may generate pending statements from block expressions)
                        let assigns = self.convert_continuous_assignment_expanded(hir_assign);
                        eprintln!(
                            "[TRAIT_DEBUG_EXIT] convert_continuous_assignment_expanded returned {} assigns", assigns.len()
                        );
                        eprintln!(
                            "⏱️  [PERF]       Finished assignment {}/{} in {:?}",
                            idx + 1,
                            impl_block.assignments.len(),
                            assignment_start.elapsed()
                        );

                        // BUG #71 DEBUG: Check variable_map size after assignment conversion
                        eprintln!(
                            "[BUG #71 AFTER ASSIGNMENT {}] variable_map size={}",
                            idx,
                            self.variable_map.len()
                        );
                        debug_println!("[BUG #71 AFTER ASSIGNMENT {}] Checking if HIR VariableId(5) is in variable_map: {}",
                            idx, self.variable_map.contains_key(&hir::VariableId(5)));

                        // BUGFIX: First, scan pending_statements for any variables that need to be declared
                        // These come from let bindings in block expressions that don't go through
                        // the dynamic_variables mechanism (e.g., when functions are inlined)

                        // BUG FIX #71: Don't use a snapshot - use current dynamic_variables directly!
                        // Variables are created during assignment conversion (via match expressions, etc.)
                        // and preserved by BUG #68. A snapshot taken before conversion would be empty.
                        //
                        // Original BUG #56 tried to use a snapshot to avoid borrow checker issues,
                        // but that was taken BEFORE assignment conversion when dynamic_variables was
                        // empty (cleared at line 308).
                        //
                        // The correct approach: Process pending statements and look up types from
                        // the CURRENT state of dynamic_variables, which includes all preserved variables.
                        let pending_stmts_snapshot = self.pending_statements.clone();

                        // BUG #71 DEBUG: Check variable_map size before processing pending statements
                        eprintln!(
                            "[BUG #71 BEFORE PENDING LOOP] variable_map size={}",
                            self.variable_map.len()
                        );
                        debug_println!("[BUG #71 BEFORE PENDING LOOP] Checking if HIR VariableId(5) is in variable_map: {}",
                            self.variable_map.contains_key(&hir::VariableId(5)));

                        for pending_stmt in &pending_stmts_snapshot {
                            if let Statement::Assignment(assign) = pending_stmt {
                                if let LValue::Variable(var_id) = &assign.lhs {
                                    // BUG #71 DEBUG: Check if var_148 is already in the module
                                    let in_module =
                                        module.variables.iter().any(|v| v.id == *var_id);
                                    if var_id.0 == 148 {
                                        eprintln!(
                                            "[BUG #71 IN_MODULE] var_148 in module? {}",
                                            in_module
                                        );
                                    }
                                    // Check if this variable is already in the module
                                    if !in_module {
                                        // BUG FIX #56/#71: Look up type from dynamic_variables instead of inferring from RHS
                                        // This preserves Float types that were declared in let statements
                                        // BUG #71: Use CURRENT dynamic_variables, not empty snapshot

                                        // BUG #71 FIX: Search dynamic_variables by MIR ID, not HIR ID!
                                        // HIR IDs are reused across function contexts, but MIR IDs are unique.
                                        // dynamic_variables is keyed by HIR ID, but the values contain MIR IDs.
                                        // We must search the values to find the entry with matching MIR ID.

                                        // BUG #71 DEBUG: Check if var_148 is in dynamic_variables
                                        if var_id.0 == 148 {
                                            debug_println!("[BUG #71 DYN_VAR LOOKUP] Searching for MIR {:?} in dynamic_variables (size={})",
                                                var_id, self.dynamic_variables.len());
                                            let found = self
                                                .dynamic_variables
                                                .values()
                                                .any(|(id, _, _)| id == var_id);
                                            eprintln!(
                                                "[BUG #71 DYN_VAR LOOKUP] Found var_148? {}",
                                                found
                                            );
                                            if !found {
                                                debug_println!("[BUG #71 DYN_VAR LOOKUP] Listing all variables with 'edge' in name:");
                                                for (mir_id, name, hir_type) in
                                                    self.dynamic_variables.values()
                                                {
                                                    if name.contains("edge") {
                                                        eprintln!("  - MIR {:?}: name='{}', HIR type={:?}", mir_id, name, hir_type);
                                                    }
                                                }
                                            }
                                        }

                                        let dyn_var_info = self
                                            .dynamic_variables
                                            .values()
                                            .find(|(id, _, _)| id == var_id)
                                            .cloned();

                                        let (var_name, var_type) = if let Some((
                                            _,
                                            name,
                                            hir_type,
                                        )) = dyn_var_info
                                        {
                                            // Found in dynamic_variables - use its declared type
                                            let mir_type = self.convert_type(&hir_type);
                                            debug_println!("[BUG #65/#66 DEBUG] Using dyn_var: var_id={}, name={}, hir_type={:?}, mir_type={:?}",
                                                    var_id.0, name, hir_type, mir_type);
                                            // BUG #71 DEBUG
                                            if name.contains("edge1")
                                                || name.contains("edge2")
                                                || name.contains("_h")
                                                || name.contains("_s")
                                            {
                                                debug_println!("[BUG #71 PENDING VAR] Variable '{}' (from pending_statements): HIR={:?} -> MIR={:?}",
                                                    name, hir_type, mir_type);
                                            }
                                            (name, mir_type)
                                        } else {
                                            // Not in dynamic_variables - fall back to name lookup and type inference
                                            // CRITICAL FIX #IMPORT_MATCH: Use mir_variable_names for reverse lookup
                                            // to avoid collision issues when multiple HIR IDs map to same MIR ID
                                            let var_name = self
                                                .mir_variable_names
                                                .get(var_id)
                                                .cloned()
                                                .or_else(|| {
                                                    // Fall back to old method if not in mir_variable_names
                                                    self.variable_map
                                                        .iter()
                                                        .find(|(_, &mir_id)| mir_id == *var_id)
                                                        .and_then(|(hir_id, _)| {
                                                            // Try to find the variable name from the HIR
                                                            self.find_variable_name(*hir_id)
                                                        })
                                                })
                                                .unwrap_or_else(|| format!("var_{}", var_id.0));

                                            eprintln!(
                                                "[BUG #IMPORT_MATCH] Reverse lookup for MIR {:?}: found name '{}'",
                                                var_id, var_name
                                            );

                                            // Infer the type from the RHS expression
                                            let var_type = self.infer_expression_type_with_module(
                                                &assign.rhs,
                                                module,
                                            );
                                            // BUG #71 DEBUG
                                            if var_name.contains("edge1")
                                                || var_name.contains("edge2")
                                                || var_name.contains("_h")
                                                || var_name.contains("_s")
                                            {
                                                debug_println!("[BUG #71 PENDING VAR INFERRED] Variable '{}' (from pending_statements, TYPE INFERRED): inferred_type={:?}",
                                                    var_name, var_type);
                                            }
                                            (var_name, var_type)
                                        };

                                        let variable = Variable {
                                            id: *var_id,
                                            name: var_name.clone(),
                                            var_type,
                                            initial: None,
                                            span: None,
                                        };
                                        debug_println!("[BUG #71 PUSH LOC3] Pushing pending variable: id={:?}, name={}", var_id, var_name);
                                        module.variables.push(variable);
                                    }
                                }
                            }
                        }

                        // Add any new dynamic variables that were created during conversion
                        // This ensures variables from let bindings in block expressions are declared
                        // before we try to assign to them
                        let dynamic_vars: Vec<_> =
                            self.dynamic_variables.values().cloned().collect();
                        for (mir_var_id, name, hir_type) in dynamic_vars {
                            // Check if this variable is already in the module
                            let already_in = module.variables.iter().any(|v| v.id == mir_var_id);
                            if !already_in {
                                println!("[DEBUG] Adding dynamic variable (in impl): name={}, hir_type={:?}", name, hir_type);
                                let mir_type = self.convert_type(&hir_type);
                                debug_println!("[DEBUG]   -> mir_type={:?}", mir_type);
                                // BUG #71 DEBUG: Check if this is one of the problematic variables
                                if name.contains("edge1")
                                    || name.contains("edge2")
                                    || name.contains("_h")
                                    || name.contains("_s")
                                {
                                    debug_println!("[BUG #71 DYNAMIC VAR] Variable '{}' (MIR {:?}): HIR={:?} -> MIR={:?}",
                                        name, mir_var_id, hir_type, mir_type);
                                }
                                let variable = Variable {
                                    id: mir_var_id,
                                    name: name.clone(),
                                    var_type: mir_type,
                                    initial: None,
                                    span: None,
                                };
                                debug_println!("[BUG #71 PUSH LOC4] Pushing assignment-dynamic variable: id={:?}, name={}", mir_var_id, name);
                                module.variables.push(variable);
                            }
                        }

                        // Emit pending statements as continuous assignments first
                        // These come from let bindings in block expressions within conditionals
                        for pending_stmt in self.pending_statements.drain(..) {
                            if let Statement::Assignment(assign) = pending_stmt {
                                let continuous = ContinuousAssign {
                                    lhs: assign.lhs,
                                    rhs: assign.rhs,
                                    span: None,
                                };
                                module.assignments.push(continuous);
                            }
                        }

                        // Drain pending module instances (from complex function calls)
                        self.drain_pending_module_instances(module);

                        // Drain pending entity instances (from let bindings with entity instantiation)
                        self.drain_pending_entity_instances(module);

                        // Then emit the main assignment
                        module.assignments.extend(assigns);
                    }

                    // Convert module instances
                    for hir_instance in &impl_block.instances {
                        if let Some(instance) = self.convert_instance(hir_instance) {
                            module.instances.push(instance);
                        }

                        // BUG FIX: Propagate detection signal flags from sub-module output ports
                        // to connected signals in the parent module
                        if let Some(hir) = self.hir.as_ref() {
                            if let Some(entity) =
                                hir.entities.iter().find(|e| e.id == hir_instance.entity)
                            {
                                for port in &entity.ports {
                                    // Only process output ports with detection_signal flag
                                    if port.is_detection_signal()
                                        && matches!(port.direction, hir::HirPortDirection::Output)
                                    {
                                        // Find the connection to this port
                                        for conn in &hir_instance.connections {
                                            if conn.port == port.name {
                                                // Get the signal ID from the RHS expression
                                                let signal_id = match &conn.expr {
                                                    hir::HirExpression::Signal(id) => {
                                                        self.signal_map.get(id).copied()
                                                    }
                                                    _ => None,
                                                };

                                                if let Some(mir_signal_id) = signal_id {
                                                    // Mark this signal as detection signal
                                                    if let Some(signal) = module
                                                        .signals
                                                        .iter_mut()
                                                        .find(|s| s.id == mir_signal_id)
                                                    {
                                                        eprintln!(
                                                            "[DETECTION_PROPAGATE] Marking signal '{}' (id={}) as detection signal (from {}.{})",
                                                            signal.name,
                                                            mir_signal_id.0,
                                                            hir_instance.name,
                                                            port.name
                                                        );
                                                        // Propagate detection config from port
                                                        signal.detection_config =
                                                            port.detection_config.clone();
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }

                    // Convert formal verification statements (assert!/assume!/cover!)
                    for stmt in &impl_block.statements {
                        match stmt {
                            hir::HirStatement::Assert(assert_stmt) => {
                                if let Some(condition) =
                                    self.convert_expression(&assert_stmt.condition, 0)
                                {
                                    module.assertions.push(Assertion {
                                        kind: AssertionKind::Assert,
                                        condition,
                                        message: assert_stmt.message.clone(),
                                        span: None,
                                    });
                                }
                            }
                            hir::HirStatement::Assume(assume_stmt) => {
                                if let Some(condition) =
                                    self.convert_expression(&assume_stmt.condition, 0)
                                {
                                    module.assertions.push(Assertion {
                                        kind: AssertionKind::Assume,
                                        condition,
                                        message: assume_stmt.message.clone(),
                                        span: None,
                                    });
                                }
                            }
                            hir::HirStatement::Cover(cover_stmt) => {
                                // For cover, extract the condition from the property
                                if let hir::HirProperty::Expression(expr) = &cover_stmt.property {
                                    if let Some(condition) = self.convert_expression(expr, 0) {
                                        module.assertions.push(Assertion {
                                            kind: AssertionKind::Cover,
                                            condition,
                                            message: cover_stmt.name.clone(),
                                            span: None,
                                        });
                                    }
                                }
                            }
                            _ => {} // Ignore other statement types
                        }
                    }

                    // Clean up generic parameter bindings for this impl block
                    for name in &bound_generic_names {
                        self.const_evaluator.unbind(name);
                    }
                }
            }
            eprintln!(
                "⏱️  [PERF]   Finished impl block {}/{} in {:?}",
                impl_idx + 1,
                hir.implementations.len(),
                impl_start.elapsed()
            );
        }

        eprintln!(
            "⏱️  [PERF] HIR→MIR transform complete in {:?}",
            transform_start.elapsed()
        );

        // Add all synthesized function modules to the MIR
        let num_synthesized = self.synthesized_modules.len();
        if num_synthesized > 0 {
            eprintln!(
                "📦 Adding {} synthesized function modules to MIR...",
                num_synthesized
            );
            for module in self.synthesized_modules.drain(..) {
                eprintln!("  ✓ Adding synthesized module: {}", module.name);
                mir.add_module(module);
            }
            eprintln!("  ✅ All synthesized modules added to MIR");
        } else {
            eprintln!("  ℹ️  No synthesized modules to add (all functions were inlined)");
        }

        mir
    }

    /// Convert event block to process
    fn convert_event_block(&mut self, block: &hir::HirEventBlock) -> Process {
        let id = self.next_process_id();

        // Determine process kind and sensitivity
        let (kind, sensitivity) = self.analyze_event_block(block);

        // Convert body
        let body = self.convert_statements(&block.statements);

        Process {
            id,
            kind,
            sensitivity,
            body,
            span: None,
        }
    }

    /// Analyze event block to determine process kind and sensitivity
    fn analyze_event_block(&self, block: &hir::HirEventBlock) -> (ProcessKind, SensitivityList) {
        if block.triggers.is_empty() {
            return (ProcessKind::Combinational, SensitivityList::Always);
        }

        let mut edges = Vec::new();
        let mut is_sequential = false;

        for trigger in &block.triggers {
            // Convert the signal reference to an LValue
            let signal_lvalue = match &trigger.signal {
                hir::HirEventSignal::Port(port_id) => {
                    if let Some(&mir_port_id) = self.port_map.get(port_id) {
                        LValue::Port(mir_port_id)
                    } else {
                        continue;
                    }
                }
                hir::HirEventSignal::Signal(signal_id) => {
                    if let Some(&mir_signal_id) = self.signal_map.get(signal_id) {
                        LValue::Signal(mir_signal_id)
                    } else {
                        continue;
                    }
                }
            };

            // Check if it's a clock edge or reset
            match &trigger.edge {
                hir::HirEdgeType::Rising | hir::HirEdgeType::Falling => {
                    is_sequential = true;
                    edges.push(EdgeSensitivity {
                        signal: signal_lvalue,
                        edge: self.convert_edge_type(&trigger.edge),
                    });
                }
                hir::HirEdgeType::Both => {
                    edges.push(EdgeSensitivity {
                        signal: signal_lvalue,
                        edge: EdgeType::Both,
                    });
                }
                hir::HirEdgeType::Active | hir::HirEdgeType::Inactive => {
                    // Reset events are typically asynchronous and make the process sequential
                    is_sequential = true;
                    edges.push(EdgeSensitivity {
                        signal: signal_lvalue,
                        edge: self.convert_edge_type(&trigger.edge),
                    });
                }
            }
        }

        let kind = if is_sequential {
            ProcessKind::Sequential
        } else {
            ProcessKind::Combinational
        };

        let sensitivity = if !edges.is_empty() {
            SensitivityList::Edge(edges)
        } else {
            SensitivityList::Always
        };

        (kind, sensitivity)
    }

    /// Convert HIR statements to MIR block
    fn convert_statements(&mut self, hir_stmts: &[hir::HirStatement]) -> Block {
        let mut statements = Vec::new();

        for hir_stmt in hir_stmts {
            if let Some(stmt) = self.convert_statement(hir_stmt) {
                statements.push(stmt);
            }
        }

        Block { statements }
    }

    /// Convert HIR statement to MIR statement
    fn convert_statement(&mut self, stmt: &hir::HirStatement) -> Option<Statement> {
        match stmt {
            hir::HirStatement::Assignment(assign) => {
                let assigns = self.convert_assignment_expanded(assign);
                match assigns.len() {
                    0 => None,
                    1 => Some(Statement::Assignment(assigns.into_iter().next().unwrap())),
                    _ => {
                        // Multiple assignments from struct expansion - wrap in block
                        Some(Statement::Block(Block {
                            statements: assigns.into_iter().map(Statement::Assignment).collect(),
                        }))
                    }
                }
            }
            hir::HirStatement::If(if_stmt) => {
                // Try synthesis resolution first for complex conditionals
                if let Some(resolved) = self.try_synthesis_resolve_if(if_stmt) {
                    Some(Statement::ResolvedConditional(resolved))
                } else {
                    self.convert_if_statement(if_stmt).map(Statement::If)
                }
            }
            hir::HirStatement::Match(match_stmt) => self
                .convert_match_statement(match_stmt)
                .map(Statement::Case),
            hir::HirStatement::Block(stmts) => {
                // Recursively apply synthesis resolution within blocks
                Some(Statement::Block(self.convert_statements(stmts)))
            }
            hir::HirStatement::Flow(flow_stmt) => {
                // Flow statements represent pipeline stages
                // Convert to sequential assignments between pipeline stages
                self.convert_flow_statement(flow_stmt)
            }
            hir::HirStatement::Assert(assert_stmt) => {
                // Convert HIR assertion to MIR for SVA generation
                if let Some(condition) = self.convert_expression(&assert_stmt.condition, 0) {
                    let severity = match assert_stmt.severity {
                        hir::HirAssertionSeverity::Info => AssertionSeverity::Info,
                        hir::HirAssertionSeverity::Warning => AssertionSeverity::Warning,
                        hir::HirAssertionSeverity::Error => AssertionSeverity::Error,
                        hir::HirAssertionSeverity::Fatal => AssertionSeverity::Fatal,
                    };
                    Some(Statement::Assert(AssertStatement {
                        condition,
                        message: assert_stmt.message.clone(),
                        severity,
                        span: None,
                    }))
                } else {
                    debug_println!("[WARN] Failed to convert assert condition, skipping assertion");
                    None
                }
            }
            hir::HirStatement::Property(_property_stmt) => {
                // Property statements are structural - used for named properties
                // They don't directly become statements but are referenced by assertions
                // Skip standalone property declarations in MIR generation
                None
            }
            hir::HirStatement::Cover(cover_stmt) => {
                // Convert HIR cover to MIR for SVA generation
                // Extract condition from property (only simple expressions supported for now)
                if let Some(condition) = self.convert_property_to_expression(&cover_stmt.property) {
                    Some(Statement::Cover(CoverStatement {
                        condition,
                        label: cover_stmt.name.clone(),
                        span: None,
                    }))
                } else {
                    debug_println!(
                        "[WARN] Failed to convert cover property, skipping cover statement"
                    );
                    None
                }
            }
            hir::HirStatement::Assume(assume_stmt) => {
                // Convert HIR assume to MIR for SVA generation
                if let Some(condition) = self.convert_expression(&assume_stmt.condition, 0) {
                    Some(Statement::Assume(AssumeStatement {
                        condition,
                        message: assume_stmt.message.clone(),
                        span: None,
                    }))
                } else {
                    debug_println!(
                        "[WARN] Failed to convert assume condition, skipping assumption"
                    );
                    None
                }
            }
            hir::HirStatement::Let(let_stmt) => {
                println!(
                    "🟠🟠🟠 [DEBUG] convert_statement: Processing Let for '{}' (ID {:?}), value type: {:?} 🟠🟠🟠",
                    let_stmt.name, let_stmt.id, std::mem::discriminant(&let_stmt.value)
                );
                if let_stmt.name == "_tuple_tmp_66" {
                    debug_println!("[MIR_LET_TRACE] *** Processing _tuple_tmp_66 - will trace through entire function ***");
                }

                // Check if this is a placeholder signal - these are output wires that receive values from entity outputs
                // They still need to be registered as variables, but should NOT generate an assignment
                let is_placeholder_signal = matches!(
                    &let_stmt.value,
                    hir::HirExpression::Literal(hir::HirLiteral::Integer(0))
                );
                if is_placeholder_signal {
                    eprintln!(
                        "[PLACEHOLDER_SIGNAL] Signal '{}' is placeholder, will register but skip assignment",
                        let_stmt.name
                    );
                }

                // BUG FIX #13-16, #21-23: Detect entity instantiation via struct literal syntax
                // Pattern: let inner = Inner { data };
                // If the RHS is a StructLiteral and the type_name matches an entity, create a module instance
                if let hir::HirExpression::StructLiteral(struct_lit) = &let_stmt.value {
                    // Check if this type_name matches an entity
                    let hir = self.hir.as_ref();
                    if let Some(hir) = hir {
                        // BUG #207 FIX: When struct_lit has generic_args, construct the specialized entity name
                        // For example, FpAdd<IEEE754_32>{...} should look for "FpAdd_fp32" entity
                        let target_entity_name = if !struct_lit.generic_args.is_empty() {
                            // Evaluate generic_args to construct specialized name
                            let mut name_parts = vec![struct_lit.type_name.clone()];
                            for arg_expr in &struct_lit.generic_args {
                                match self.const_evaluator.eval(arg_expr) {
                                    Ok(const_val) => {
                                        let suffix = match &const_val {
                                            ConstValue::Nat(n) => n.to_string(),
                                            ConstValue::Int(i) => {
                                                if *i >= 0 {
                                                    i.to_string()
                                                } else {
                                                    format!("n{}", i.abs())
                                                }
                                            }
                                            ConstValue::Bool(b) => {
                                                if *b { "true" } else { "false" }.to_string()
                                            }
                                            ConstValue::String(s) => s.replace([' ', '-'], "_"),
                                            ConstValue::Float(f) => format!("f{}", f.to_bits()),
                                            ConstValue::FloatFormat(fmt) => {
                                                format!("fp{}", fmt.total_bits)
                                            }
                                            ConstValue::Struct(_) => "struct".to_string(),
                                        };
                                        name_parts.push(suffix);
                                    }
                                    Err(_e) => {
                                        // Generic arg eval failed - will fall back to generic entity name
                                    }
                                }
                            }
                            name_parts.join("_")
                        } else {
                            struct_lit.type_name.clone()
                        };

                        // BUG #207 FIX: Look for specialized entity first, then fall back to generic
                        let entity = hir
                            .entities
                            .iter()
                            .find(|e| e.name == target_entity_name)
                            .or_else(|| {
                                // Fall back to generic entity name if specialized not found
                                hir.entities.iter().find(|e| e.name == struct_lit.type_name)
                            });

                        if let Some(entity) = entity {
                            // Get the module ID for this entity
                            if let Some(&module_id) = self.entity_map.get(&entity.id) {
                                // Create module instance
                                // BUG #197/198/199 FIX: Include unique ID in entity instance name
                                // to prevent name collisions between entity instances.
                                // Previously we tried using entity_instance_prefix but it wasn't being
                                // propagated correctly through all code paths. Using a global counter
                                // is more robust and guarantees uniqueness.
                                let instance_id = self.next_entity_instance_id;
                                self.next_entity_instance_id += 1;
                                let instance_name =
                                    if let Some(ref prefix) = self.entity_instance_prefix {
                                        format!("{}_{}_{}", prefix, let_stmt.name, instance_id)
                                    } else {
                                        format!("{}_{}", let_stmt.name, instance_id)
                                    };

                                // BUG #195 FIX: Set up output port mappings FIRST, BEFORE converting any field values
                                // This ensures placeholder variables like `result` are mapped before they're accessed
                                // during field value conversion.

                                // Create signals for output ports and track them
                                let mut output_ports = IndexMap::new();
                                println!(
                                    "[HIER_PORTS] Entity '{}' has {} ports",
                                    entity.name,
                                    entity.ports.len()
                                );
                                for port in &entity.ports {
                                    println!(
                                        "[HIER_PORTS]   Port '{}' direction={:?}",
                                        port.name, port.direction
                                    );
                                    if matches!(port.direction, hir::HirPortDirection::Output) {
                                        println!(
                                            "[HIER_OUTPUT] Output port '{}' MATCHED!",
                                            port.name
                                        );
                                        // Create a signal to hold this output
                                        let signal_id = self.next_signal_id();
                                        let signal_name =
                                            format!("{}_{}", instance_name, port.name);
                                        let signal_type = self.convert_type(&port.port_type);

                                        println!(
                                            "[HIERARCHICAL] Creating output signal '{}' (type {:?}) for instance '{}' port '{}'",
                                            signal_name, signal_type, instance_name, port.name
                                        );

                                        // Track this output port
                                        output_ports.insert(port.name.clone(), signal_id);

                                        // Note: Connection will be added later in the BUG #195 FIX section
                                        // after all output port mappings are set up

                                        // BUG #167 FIX: Track placeholder signal -> entity output mapping
                                        // If this output port is connected to a Variable or Signal in the struct literal,
                                        // map that ID to this output wire for return expression wiring
                                        if let Some(field_init) =
                                            struct_lit.fields.iter().find(|f| f.name == port.name)
                                        {
                                            // Extract the Variable ID from the field value
                                            // Handle both plain Variable and Cast(Variable)
                                            println!(
                                                "[BUG #167 DEBUG] Checking field '{}' for Variable/Cast(Variable), type: {:?}",
                                                port.name, std::mem::discriminant(&field_init.value)
                                            );
                                            let maybe_var_id = match &field_init.value {
                                                hir::HirExpression::Variable(var_id) => {
                                                    println!(
                                                        "[BUG #167 DEBUG] Direct Variable {:?}",
                                                        var_id
                                                    );
                                                    Some(*var_id)
                                                }
                                                hir::HirExpression::Cast(cast_expr) => {
                                                    println!(
                                                        "[BUG #167 DEBUG] Cast expr, inner type: {:?}",
                                                        std::mem::discriminant(&*cast_expr.expr)
                                                    );
                                                    if let hir::HirExpression::Variable(var_id) =
                                                        &*cast_expr.expr
                                                    {
                                                        println!(
                                                            "[BUG #167 DEBUG] Cast(Variable {:?})",
                                                            var_id
                                                        );
                                                        Some(*var_id)
                                                    } else {
                                                        println!(
                                                            "[BUG #167 DEBUG] Cast of non-Variable"
                                                        );
                                                        None
                                                    }
                                                }
                                                _ => None,
                                            };

                                            if let Some(var_id) = maybe_var_id {
                                                // BUG #201 FIX: Convert HIR type to frontend Type for correct expression typing
                                                let expr_type =
                                                    self.hir_type_to_type(&port.port_type);
                                                println!(
                                                    "[BUG #167] Mapping placeholder variable {:?} -> entity output signal '{}' (type {:?})",
                                                    var_id, signal_name, expr_type
                                                );
                                                // BUG #201 FIX: Store Type along with signal_id for correct type propagation
                                                self.placeholder_signal_to_entity_output
                                                    .insert(var_id, (signal_id, expr_type));
                                            }

                                            // BUG #190 FIX: Also handle Signal and Cast(Signal)
                                            // This is critical for FP trait implementations that use
                                            // `signal result: fp32;` rather than `let result: fp32;`
                                            println!(
                                                "[BUG #190 DEBUG] Field '{}' value type: {:?}",
                                                port.name,
                                                std::mem::discriminant(&field_init.value)
                                            );
                                            let maybe_hir_signal_id = match &field_init.value {
                                                hir::HirExpression::Signal(hir_sig_id) => {
                                                    println!(
                                                        "[BUG #190 DEBUG] Direct Signal {:?}",
                                                        hir_sig_id
                                                    );
                                                    Some(*hir_sig_id)
                                                }
                                                hir::HirExpression::Cast(cast_expr) => {
                                                    println!(
                                                        "[BUG #190 DEBUG] Cast, inner type: {:?}",
                                                        std::mem::discriminant(&*cast_expr.expr)
                                                    );
                                                    if let hir::HirExpression::Signal(hir_sig_id) =
                                                        &*cast_expr.expr
                                                    {
                                                        println!(
                                                            "[BUG #190 DEBUG] Cast(Signal {:?})",
                                                            hir_sig_id
                                                        );
                                                        Some(*hir_sig_id)
                                                    } else {
                                                        None
                                                    }
                                                }
                                                _ => None,
                                            };

                                            if let Some(hir_sig_id) = maybe_hir_signal_id {
                                                // BUG #201 FIX: Convert HIR type to frontend Type for correct expression typing
                                                let expr_type =
                                                    self.hir_type_to_type(&port.port_type);
                                                println!(
                                                    "[BUG #190] Mapping placeholder HIR signal {:?} -> entity output MIR signal {:?} ('{}', type {:?})",
                                                    hir_sig_id, signal_id, signal_name, expr_type
                                                );
                                                // BUG #201 FIX: Store Type along with signal_id for correct type propagation
                                                self.placeholder_hir_signal_to_entity_output
                                                    .insert(hir_sig_id, (signal_id, expr_type));
                                            }
                                        }

                                        // The signal will be created by the module
                                        // We need to return a statement that adds both the signal and the instance
                                        // For now, store for later processing
                                    }
                                }

                                // BUG #195 FIX: NOW convert connections from struct literal fields
                                // This happens AFTER output port mappings are set up, so placeholder variables
                                // like `result` will properly resolve to their entity output signals.
                                let mut connections = IndexMap::new();
                                println!(
                                    "[HIER_CONN] Converting {} fields for entity instantiation (AFTER output mapping)",
                                    struct_lit.fields.len()
                                );

                                // Build a set of output port names for quick lookup
                                let output_port_names: std::collections::HashSet<_> = entity
                                    .ports
                                    .iter()
                                    .filter(|p| {
                                        matches!(p.direction, hir::HirPortDirection::Output)
                                    })
                                    .map(|p| p.name.as_str())
                                    .collect();

                                for field_init in &struct_lit.fields {
                                    // For output ports, the connection is the output signal we created above
                                    if output_port_names.contains(field_init.name.as_str()) {
                                        println!(
                                            "[HIER_CONN] Field '{}' is OUTPUT - using pre-created signal reference",
                                            field_init.name
                                        );
                                        // The output signal connection was already added above in the output port loop
                                        if let Some(&sig_id) = output_ports.get(&field_init.name) {
                                            connections.insert(
                                                field_init.name.clone(),
                                                Expression::with_unknown_type(ExpressionKind::Ref(
                                                    LValue::Signal(sig_id),
                                                )),
                                            );
                                        }
                                    } else {
                                        // Input port - convert the expression normally
                                        println!(
                                            "[HIER_CONN] Converting INPUT field '{}' <- HIR expr: {:?}",
                                            field_init.name,
                                            std::mem::discriminant(&field_init.value)
                                        );
                                        // BUG #200 DEBUG: Print full structure of entity input field
                                        fn debug_print_hir_expr(
                                            expr: &hir::HirExpression,
                                            indent: &str,
                                        ) {
                                            match expr {
                                                hir::HirExpression::Cast(c) => {
                                                    println!(
                                                        "{}Cast(target={:?})",
                                                        indent, c.target_type
                                                    );
                                                    debug_print_hir_expr(
                                                        &c.expr,
                                                        &format!("{}  ", indent),
                                                    );
                                                }
                                                hir::HirExpression::Range(base, hi, lo) => {
                                                    println!("{}Range[{:?}:{:?}]", indent, hi, lo);
                                                    debug_print_hir_expr(
                                                        base,
                                                        &format!("{}  ", indent),
                                                    );
                                                }
                                                hir::HirExpression::Variable(var_id) => {
                                                    println!("{}Variable({:?})", indent, var_id);
                                                }
                                                hir::HirExpression::GenericParam(name) => {
                                                    println!(
                                                        "{}GenericParam(\"{}\")",
                                                        indent, name
                                                    );
                                                }
                                                hir::HirExpression::Port(port_id) => {
                                                    println!("{}Port({:?})", indent, port_id);
                                                }
                                                hir::HirExpression::Signal(sig_id) => {
                                                    println!("{}Signal({:?})", indent, sig_id);
                                                }
                                                _ => {
                                                    println!(
                                                        "{}Other({:?})",
                                                        indent,
                                                        std::mem::discriminant(expr)
                                                    );
                                                }
                                            }
                                        }
                                        println!("[BUG #200 DEBUG] Entity '{}' input field '{}' structure:", struct_lit.type_name, field_init.name);
                                        debug_print_hir_expr(&field_init.value, "  ");
                                        if let Some(expr) =
                                            self.convert_expression(&field_init.value, 0)
                                        {
                                            println!(
                                                "[HIER_CONN] Field '{}' <- MIR expr: {:?}",
                                                field_init.name, expr.kind
                                            );
                                            connections.insert(field_init.name.clone(), expr);
                                        } else {
                                            println!(
                                                "[HIER_CONN] FAILED: convert_expression returned None for field '{}'",
                                                field_init.name
                                            );
                                        }
                                    }
                                }

                                // Store the output port mappings for field access resolution
                                self.entity_instance_outputs
                                    .insert(let_stmt.id, output_ports.clone());
                                self.entity_instance_info
                                    .insert(let_stmt.id, (instance_name.clone(), module_id));

                                println!(
                                    "[HIERARCHICAL] Stored entity instance '{}' (var {:?}) with {} output ports",
                                    instance_name, let_stmt.id, self.entity_instance_outputs.get(&let_stmt.id).map(|m| m.len()).unwrap_or(0)
                                );

                                // Create the module instance
                                let instance = ModuleInstance {
                                    name: instance_name.clone(),
                                    module: module_id,
                                    connections,
                                    parameters: IndexMap::new(),
                                    span: None,
                                    safety_context: None,
                                };

                                // Collect output signals for the module
                                let mut output_signals = Vec::new();
                                for port in &entity.ports {
                                    if matches!(port.direction, hir::HirPortDirection::Output) {
                                        if let Some(&signal_id) = output_ports.get(&port.name) {
                                            let signal_name =
                                                format!("{}_{}", instance_name, port.name);
                                            let signal_type = self.convert_type(&port.port_type);
                                            // Propagate detection config from port
                                            let detection_cfg = port.detection_config.clone();
                                            output_signals.push((
                                                signal_id,
                                                signal_name,
                                                signal_type,
                                                detection_cfg,
                                            ));
                                        }
                                    }
                                }

                                // Push to pending list - will be drained when processing module
                                self.pending_entity_instances
                                    .push((instance, output_signals));

                                eprintln!(
                                    "[HIERARCHICAL] Added entity instance to pending list (total: {})",
                                    self.pending_entity_instances.len()
                                );

                                // Return None - the instance will be added during drain
                                return None;
                            } else {
                                println!(
                                    "[HIERARCHICAL] FAILED: entity_map doesn't contain entity.id={:?}",
                                    entity.id
                                );
                            }
                        }
                    }
                }

                // Convert let statement to assignment
                // Let bindings are local variables that need to be treated as blocking assignments

                // BUG FIX: Check context-aware map first when in ANY context (match arm OR function inlining)
                let var_id = if let Some(context) = self.get_current_context() {
                    let context_key = (Some(context.clone()), let_stmt.id);
                    if let Some(&id) = self.context_variable_map.get(&context_key) {
                        debug_println!("[DEBUG] Let '{}' (ID {:?}): Found in context_variable_map for '{}' as MIR ID={:?}", let_stmt.name, let_stmt.id, context, id);
                        id
                    } else {
                        debug_println!("[DEBUG] Let '{}' (ID {:?}): NOT in context_variable_map for '{}', will check variable_map", let_stmt.name, let_stmt.id, context);
                        VariableId(u32::MAX) // Will check variable_map next
                    }
                } else if let Some(&id) = self.variable_map.get(&let_stmt.id) {
                    eprintln!(
                        "[DEBUG] Let '{}' (ID {:?}): Found in variable_map as MIR ID={:?}",
                        let_stmt.name, let_stmt.id, id
                    );
                    // Check if we're in a match arm context and the existing variable
                    // doesn't have the correct prefix - this can happen when a variable
                    // from an outer scope (like _tuple_tmp_0) has the same HIR VariableId
                    // as a function-local variable in an inlined function
                    let should_create_new = if let Some(ref prefix) = self.match_arm_prefix {
                        // Check if the existing variable has the wrong prefix
                        if let Some((_, existing_name, _)) =
                            self.dynamic_variables.get(&let_stmt.id)
                        {
                            let wrong_prefix = !existing_name.starts_with(prefix);
                            debug_println!("[DEBUG] Let '{}': match_arm_prefix={}, existing_name={}, wrong_prefix={}",
                                let_stmt.name, prefix, existing_name, wrong_prefix);
                            wrong_prefix
                        } else {
                            // Not in dynamic_variables, might be a module-level variable
                            // Create new to be safe
                            eprintln!(
                                "[DEBUG] Let '{}': Not in dynamic_variables, will create new",
                                let_stmt.name
                            );
                            true
                        }
                    } else {
                        eprintln!(
                            "[DEBUG] Let '{}': No match_arm_prefix, reusing existing",
                            let_stmt.name
                        );
                        false
                    };

                    if should_create_new {
                        debug_println!("[DEBUG] Let '{}': Will create new variable (wrong prefix or not in dynamic_variables)", let_stmt.name);
                        // Don't reuse the existing variable, fall through to create a new one
                        VariableId(u32::MAX) // Sentinel value that won't match any existing variable
                    } else {
                        eprintln!(
                            "[DEBUG] Let '{}': Reusing existing MIR ID={:?}",
                            let_stmt.name, id
                        );
                        // Already registered with correct scope
                        id
                    }
                } else {
                    eprintln!(
                        "[DEBUG] Let '{}' (ID {:?}): NOT in variable_map, will create new",
                        let_stmt.name, let_stmt.id
                    );
                    VariableId(u32::MAX) // Sentinel value to trigger new variable creation
                };

                // BUG FIX #67 with REGRESSION FIX: Context-aware RHS conversion
                //
                // We need to handle two cases:
                // 1. Simple function call RHS (Bug #67 case): Convert RHS first to get correct types
                //    Example: let (rx, ry, rz) = vec3_add(...)
                //
                // 2. Complex RHS that references the let variable (regression case): Register variable first
                //    Example: let rx = match opcode { 32 => { let (rx, ...) = ...; {0, rz, ry, rx} }, _ => 0 }
                //
                // Heuristic: If RHS is a simple direct function call OR if we're extracting from
                // a tuple (which comes from a function call), convert first.
                // Otherwise, register variable first to allow RHS to reference it.

                let is_simple_function_call = matches!(let_stmt.value, hir::HirExpression::Call(_));

                // DEBUG: Track ALL function calls in match context
                if is_simple_function_call && self.match_arm_prefix.is_some() {
                    if let hir::HirExpression::Call(ref call) = let_stmt.value {
                        println!(
                            "🔍 FUNC_CALL_IN_MATCH: name='{}', function='{}', match_prefix={:?}",
                            let_stmt.name, call.function, self.match_arm_prefix
                        );
                    }
                }

                // Check if this is tuple element extraction from a function call
                // Pattern: let rx = <tuple_tmp>.0 where <tuple_tmp> was from a function call
                // BUT: Exclude cases where the base is a Block, because Blocks may reference
                // variables that haven't been created yet (circular dependency during inlining)
                let (base_is_block, base_type, rhs_is_field_access, base_is_dynamic_var) =
                    if let hir::HirExpression::FieldAccess { base, field } = &let_stmt.value {
                        let is_block = matches!(**base, hir::HirExpression::Block { .. });
                        let discriminant = std::mem::discriminant(&**base);
                        // Check if base is a dynamically-created variable (from let bindings in inlined functions)
                        let is_dynamic = if let hir::HirExpression::Variable(var_id) = &**base {
                            self.dynamic_variables.contains_key(var_id)
                        } else {
                            false
                        };
                        debug_println!("[DEBUG] Let '{}': FieldAccess.field={}, base type={:?}, base_is_dynamic={}", let_stmt.name, field, discriminant, is_dynamic);
                        (is_block, Some(discriminant), true, is_dynamic)
                    } else {
                        (false, None, false, false)
                    };
                if let_stmt.name == "rw" && let_stmt.id == hir::VariableId(70) {
                    debug_println!("[DEBUG] Let 'rw' (70): RHS type {:?}, rhs_is_field_access={}, base_is_block={}, base_type={:?}",
                        std::mem::discriminant(&let_stmt.value), rhs_is_field_access, base_is_block, base_type);
                    debug_println!(
                        "[DEBUG] Let 'rw' (70): should_convert_first will be calculated..."
                    );
                }
                let is_tuple_element_extraction = matches!(
                    &let_stmt.value,
                    hir::HirExpression::FieldAccess { base, field }
                    if field.chars().all(|c| c.is_ascii_digit()) // Numeric field like "0", "1", "2"
                    && !base_is_block  // Exclude if base is Block (circular dependency)
                    && !base_is_dynamic_var  // Exclude if base is dynamic variable (may have complex RHS)
                );

                // Check if this is a cast expression
                // Pattern: let ax_fp = ax as fp32
                // Bug #67: Cast expressions should convert first to get proper type from the cast
                let is_cast_expression = matches!(let_stmt.value, hir::HirExpression::Cast(_));

                // BUG FIX: Convert simple function calls even in match arms, but avoid circular deps for complex expressions
                // - Simple function calls should ALWAYS be inlined (Bug #QUADRATIC fix)
                // - Tuple element extraction and casts should avoid match context (circular dependency risk)
                let in_match_context = self.match_arm_prefix.is_some();
                let should_convert_first = is_simple_function_call
                    || (is_tuple_element_extraction && !in_match_context)
                    || (is_cast_expression && !in_match_context);

                if is_simple_function_call && in_match_context {
                    println!("🟢🟢🟢 BUG FIX ACTIVE: Function call in match arm, WILL convert: {} 🟢🟢🟢", let_stmt.name);
                }

                if let_stmt.name == "rw" && let_stmt.id == hir::VariableId(70) {
                    debug_println!("[DEBUG] Let 'rw' (70): is_tuple_element_extraction={}, in_match_context={}, should_convert_first={}",
                        is_tuple_element_extraction, in_match_context, should_convert_first);
                }

                let (rhs, needs_type_inference) = if should_convert_first {
                    // Simple function call, tuple element extraction, or cast: convert first for Bug #67 fix
                    if let_stmt.name == "rw" && let_stmt.id == hir::VariableId(70) {
                        eprintln!(
                            "[DEBUG] Let 'rw' (70): About to convert RHS (convert_first=true)"
                        );
                    }
                    if let_stmt.name == "_tuple_tmp_66" {
                        debug_println!("[MIR_LET_TRACE] _tuple_tmp_66: should_convert_first=true, converting RHS now");
                    }
                    if let_stmt.name.contains("_tuple_tmp_76") {
                        println!(
                            "🎯🎯🎯 _tuple_tmp_76: Converting RHS, type={:?} 🎯🎯🎯",
                            std::mem::discriminant(&let_stmt.value)
                        );
                        if let hir::HirExpression::Call(ref c) = let_stmt.value {
                            println!("🎯🎯🎯 _tuple_tmp_76: IS a Call to '{}' 🎯🎯🎯", c.function);
                        }
                    }
                    (self.convert_expression(&let_stmt.value, 0)?, true)
                } else {
                    // Complex expression: will convert after variable registration
                    if let_stmt.name == "_tuple_tmp_66" {
                        debug_println!("[MIR_LET_TRACE] _tuple_tmp_66: should_convert_first=false, using placeholder");
                    }
                    (
                        Expression::with_unknown_type(ExpressionKind::Literal(Value::Integer(0))),
                        false,
                    ) // Placeholder, will be replaced
                };

                let var_id = if var_id == VariableId(u32::MAX) {
                    if let_stmt.name == "_tuple_tmp_66" {
                        debug_println!("[MIR_LET_TRACE] _tuple_tmp_66: var_id is u32::MAX, creating new variable");
                    }
                    // Apply match arm prefix if we're in a match arm context
                    // This prevents variable name collisions between different match arms
                    // IMPORTANT: Do this BEFORE checking for duplicates so we check the correct name
                    //
                    // BUG FIX #154: Also include inlining context ID to prevent variable name
                    // collisions when the same function is inlined multiple times.
                    // Without this, `let result = ...` in two different calls to the same function
                    // would share the same variable, causing incorrect behavior.
                    let var_name = {
                        let mut name = let_stmt.name.clone();

                        // Add inlining context prefix if we're inside an inlined function
                        // This ensures each function call site gets unique variable names
                        if !self.inlining_context_stack.is_empty() {
                            let context_id = self.inlining_context_stack.last().unwrap();
                            name = format!("{}_ic{}", name, context_id);
                        }

                        // Add match arm prefix if we're in a match arm context
                        if let Some(ref prefix) = self.match_arm_prefix {
                            name = format!("{}_{}", prefix, name);
                        }

                        name
                    };

                    // Check if we already have a dynamic variable with this name
                    // If so, reuse its ID to avoid duplicate declarations
                    let existing_var = self
                        .dynamic_variables
                        .values()
                        .find(|(_, name, _)| name == &var_name);

                    let new_id = if let Some((existing_id, _, _)) = existing_var {
                        // Reuse the existing variable ID for this name
                        eprintln!(
                            "[DEBUG] Reusing existing variable '{}' with MIR ID={:?}",
                            var_name, existing_id
                        );
                        *existing_id
                    } else {
                        // Create a new MIR variable on the fly for event block let bindings
                        let new_id = self.next_variable_id();

                        // BUG FIX #67: Infer type from the CONVERTED expression (after inlining)
                        // instead of using the HIR placeholder type - BUT ONLY for simple function calls
                        let final_hir_type = if needs_type_inference {
                            // Simple function call case: infer from converted expression
                            let inferred_type = self.infer_expression_type(&rhs);
                            let hir_placeholder_type = &let_stmt.var_type;

                            // Use inferred type if it's more specific than the HIR placeholder
                            // The HIR placeholder is often Nat(32) for unknown types
                            if matches!(hir_placeholder_type, hir::HirType::Nat(32))
                                && !matches!(inferred_type, DataType::Nat(32))
                            {
                                // Use inferred type if it's more specific than the Nat(32) placeholder
                                self.convert_mir_to_hir_type(&inferred_type)
                            } else if matches!(hir_placeholder_type, hir::HirType::Tuple(_)) {
                                // For tuple types, use the HIR placeholder directly
                                // Don't convert to MIR and back - preserve the original tuple type
                                hir_placeholder_type.clone()
                            } else {
                                // For other types, convert through MIR to ensure consistency
                                let converted_hir_type = self.convert_type(hir_placeholder_type);
                                // Convert back to HIR for storage
                                self.convert_mir_to_hir_type(&converted_hir_type)
                            }
                        } else {
                            // Complex RHS case: use HIR placeholder, will be refined later if needed
                            // BUG FIX #FP_TRAIT: For Cast expressions, ALWAYS use the Cast's target_type
                            // This ensures `let a_fp = a as fp32` gets type fp32, not Bit(32)
                            // Critical for FP trait resolution to work correctly
                            if let hir::HirExpression::Cast(cast_expr) = &let_stmt.value {
                                eprintln!(
                                    "[BUG #FP_TRAIT] Variable '{}': Using Cast target_type {:?} instead of HIR placeholder {:?}",
                                    var_name, cast_expr.target_type, let_stmt.var_type
                                );
                                cast_expr.target_type.clone()
                            } else {
                                eprintln!(
                                    "[BUG #67] Variable '{}': Using HIR placeholder type {:?} for complex RHS",
                                    var_name, let_stmt.var_type
                                );
                                eprintln!(
                                    "[BUG #71] Variable '{}'  (MIR ID={:?}): HIR type={:?}, RHS discriminant={:?}",
                                    var_name, new_id, let_stmt.var_type, std::mem::discriminant(&let_stmt.value)
                                );
                                // BUG #71 FIX: For complex RHS with tuple type placeholders, try to infer from RHS after conversion
                                if matches!(let_stmt.var_type, hir::HirType::Nat(32)) {
                                    debug_println!("[BUG #71] Variable '{}': HIR type is Nat(32) placeholder - will need to infer from RHS later", var_name);
                                }
                                let_stmt.var_type.clone()
                            }
                        };

                        // Track this dynamically created variable so we can add it to the module later
                        eprintln!(
                            "[DEBUG] Creating dynamic variable: name={}, HIR ID={:?}, MIR ID={:?}, type={:?}",
                            var_name, let_stmt.id, new_id, final_hir_type
                        );
                        eprintln!(
                            "[BUG #IMPORT_MATCH] Before insert: dynamic_variables[{:?}] = {:?}",
                            let_stmt.id,
                            self.dynamic_variables.get(&let_stmt.id)
                        );

                        // CRITICAL FIX #IMPORT_MATCH: Handle VariableId collisions properly
                        //
                        // PROBLEM: When inlining nested functions, different variables from different
                        // functions can have the same HIR VariableId. For example:
                        //   - test_func has 'result' with VariableId(0)
                        //   - my_fp_add has 'a_fp' also with VariableId(0)
                        //
                        // SOLUTION:
                        // 1. If in match arm context: Store in context_variable_map with prefix
                        // 2. If collision in dynamic_variables: Log warning but don't store
                        //    (context_variable_map will be used for lookup instead)
                        // 3. If no collision: Store in dynamic_variables as usual

                        // CRITICAL FIX #IMPORT_MATCH: Store MIR ID -> name and type mappings for ALL variables
                        // This allows correct reverse lookups even when HIR IDs collide
                        self.mir_variable_names.insert(new_id, var_name.clone());
                        self.mir_variable_types
                            .insert(new_id, final_hir_type.clone());
                        eprintln!(
                            "[BUG #IMPORT_MATCH] Stored MIR {:?} -> '{}' (type {:?}) in mir_variable_names/types",
                            new_id, var_name, final_hir_type
                        );

                        if let Some(ref prefix) = self.match_arm_prefix {
                            // In match arm context - always use context_variable_map
                            eprintln!(
                                "[BUG #IMPORT_MATCH] In match arm '{}': storing HIR {:?} ('{}') -> MIR {:?} in context_variable_map",
                                prefix, let_stmt.id, var_name, new_id
                            );
                            // context_variable_map will be populated below (line ~1021)
                        }

                        // Also try to store in dynamic_variables for backward compatibility
                        // But detect collisions and warn
                        if let Some((existing_mir_id, existing_name, _)) =
                            self.dynamic_variables.get(&let_stmt.id)
                        {
                            if existing_name != &var_name {
                                eprintln!(
                                    "[BUG #IMPORT_MATCH] COLLISION DETECTED! HIR {:?} already maps to '{}' (MIR {:?}), NOT overwriting with '{}' (MIR {:?})",
                                    let_stmt.id, existing_name, existing_mir_id, var_name, new_id
                                );
                                eprintln!(
                                    "[BUG #IMPORT_MATCH] Relying on context_variable_map for correct lookup (match_arm_prefix={:?})",
                                    self.match_arm_prefix
                                );
                                // Don't insert - keep the existing entry
                                // The context_variable_map will handle the correct lookup
                            } else {
                                // Same variable name - safe to update
                                eprintln!(
                                    "[BUG #IMPORT_MATCH] Updating existing entry for '{}': {:?} -> {:?}",
                                    var_name, existing_mir_id, new_id
                                );
                                self.dynamic_variables.insert(
                                    let_stmt.id,
                                    (new_id, var_name.clone(), final_hir_type),
                                );
                            }
                        } else {
                            // New entry - safe to insert
                            self.dynamic_variables
                                .insert(let_stmt.id, (new_id, var_name.clone(), final_hir_type));
                            eprintln!(
                                "[BUG #IMPORT_MATCH] After insert: dynamic_variables[{:?}] = ({:?}, {}, ...)",
                                let_stmt.id, new_id, var_name
                            );
                        }

                        new_id
                    };

                    // Map this HIR variable ID to the MIR variable ID (whether new or reused)
                    // BUG FIX: Use context-aware map when in ANY context (match arm OR function inlining) to prevent collisions
                    if let Some(context) = self.get_current_context() {
                        let context_key = (Some(context.clone()), let_stmt.id);
                        eprintln!(
                            "[DEBUG] Storing context-aware mapping: {:?} in '{}' -> MIR {:?}",
                            let_stmt.id, context, new_id
                        );
                        self.context_variable_map.insert(context_key, new_id);
                        // BUG #149 FIX: Do NOT insert into variable_map if there's already an entry
                        // When in a context (match arm or function inlining), the inlined function's
                        // variables may have the same HIR VariableId as the caller's variables.
                        // Overwriting variable_map causes the RHS of the inlined let statement to
                        // incorrectly reference the new variable instead of the original one.
                        // Example: let a = data1[31:0] where a's HIR ID = data1's HIR ID = 0
                        //   Before: variable_map[0] = MIR 0 (data1)
                        //   After (broken): variable_map[0] = MIR 2 (a) - WRONG!
                        //   RHS Variable(0) now resolves to a instead of data1, creating circular ref
                        // BUG #149 FIX: Only insert if there's no existing entry
                        // The context_variable_map will be used for context-specific lookups
                        self.variable_map.entry(let_stmt.id).or_insert(new_id);
                    } else {
                        self.variable_map.insert(let_stmt.id, new_id);
                    }

                    new_id
                } else {
                    if let_stmt.name == "_tuple_tmp_66" {
                        eprintln!(
                            "[MIR_LET_TRACE] _tuple_tmp_66: var_id exists, using {:?}",
                            var_id
                        );
                    }
                    // Variable already exists with correct scope, use it
                    var_id
                };

                if let_stmt.name == "_tuple_tmp_66" {
                    eprintln!(
                        "[MIR_LET_TRACE] _tuple_tmp_66: About to convert final RHS, var_id={:?}",
                        var_id
                    );
                }
                // For complex RHS, convert it now that the variable is registered
                eprintln!(
                    "[MIR_LET_FINAL] Converting final RHS for var {:?}, needs_type_inference={}",
                    var_id, needs_type_inference
                );
                let final_rhs = if needs_type_inference {
                    // Already converted
                    debug_println!("[MIR_LET_FINAL]   Using already converted RHS");
                    rhs
                } else {
                    // Convert now that variable is registered and available
                    println!(
                        "[🟡 MIR_LET_FINAL] Converting RHS for '{}': {:?}",
                        let_stmt.name,
                        std::mem::discriminant(&let_stmt.value)
                    );
                    if let_stmt.name == "_tuple_tmp_66" {
                        eprintln!(
                            "[MIR_LET_TRACE] _tuple_tmp_66: Calling convert_expression on RHS"
                        );
                    }
                    let converted = self.convert_expression(&let_stmt.value, 0);
                    println!(
                        "[🟡 MIR_LET_FINAL] RHS conversion for '{}' returned: {:?}",
                        let_stmt.name,
                        converted.is_some()
                    );
                    if converted.is_none() {
                        debug_println!("[🟡 MIR_LET_FINAL] ❌ RHS conversion FAILED for '{}' - returning None!", let_stmt.name);
                    }
                    if let_stmt.name == "_tuple_tmp_66" {
                        eprintln!(
                            "[MIR_LET_TRACE] _tuple_tmp_66: convert_expression returned: {:?}",
                            converted.is_some()
                        );
                        if converted.is_none() {
                            debug_println!("[MIR_LET_TRACE] _tuple_tmp_66: convert_expression returned None - returning None from convert_statement!");
                        }
                    }
                    let converted = converted?; // This will return None if conversion failed
                    debug_println!("[MIR_LET_FINAL]   Converted RHS: {:?}", converted);
                    converted
                };

                let lhs = LValue::Variable(var_id);
                eprintln!(
                    "[MIR_LET_FINAL] Creating assignment: {:?} = {:?}",
                    lhs, final_rhs
                );

                // Store RHS for module instance argument expansion
                // This allows expanding variable references to their actual values when used as module args
                eprintln!(
                    "[HYBRID_RHS] Storing RHS for var {:?} ({})",
                    var_id,
                    self.mir_variable_names
                        .get(&var_id)
                        .unwrap_or(&"unknown".to_string())
                );
                self.dynamic_variable_rhs.insert(var_id, final_rhs.clone());

                // Skip creating assignment for placeholder signals
                // These are output wires that receive their value from entity outputs, not from initialization
                if is_placeholder_signal {
                    eprintln!(
                        "[PLACEHOLDER_SIGNAL] Skipping assignment creation for '{}', returning None",
                        let_stmt.name
                    );
                    return None;
                }

                Some(Statement::Assignment(Assignment {
                    lhs,
                    rhs: final_rhs,
                    kind: AssignmentKind::Blocking,
                    span: None,
                }))
            }
            hir::HirStatement::For(for_stmt) => {
                // For loop statement - convert to MIR LoopStatement::For
                // Check if unrolling is requested
                if let Some(ref unroll_config) = for_stmt.unroll {
                    // Loop unrolling - expand at compile time
                    return self.unroll_for_loop(for_stmt, unroll_config);
                }
                // Normal for loop - convert to sequential loop
                self.convert_for_statement(for_stmt)
            }
            hir::HirStatement::Return(_return_expr) => {
                // Return statements are function constructs
                // For now, skip them in MIR as functions aren't yet lowered to hardware
                // TODO: Implement function support in MIR
                None
            }
            hir::HirStatement::Expression(_expr) => {
                // Expression statements (standalone expressions without assignment)
                // These represent implicit return values in functions
                // For now, skip them in MIR as they don't map to hardware
                // TODO: Handle when implementing function support in MIR
                None
            }
            hir::HirStatement::GenerateFor(_gen_for) => {
                // Generate for statements should have been elaborated at HIR level
                // in default mode. For preserve mode, they should be handled by
                // a separate generate block conversion path.
                // TODO: Implement generate block MIR conversion for preserve mode
                debug_println!("[WARNING] GenerateFor statement reached MIR - should have been elaborated at HIR level");
                None
            }
            hir::HirStatement::GenerateIf(_gen_if) => {
                // Generate if statements should have been elaborated at HIR level
                // in default mode. For preserve mode, they should be handled by
                // a separate generate block conversion path.
                // TODO: Implement generate block MIR conversion for preserve mode
                debug_println!("[WARNING] GenerateIf statement reached MIR - should have been elaborated at HIR level");
                None
            }
            hir::HirStatement::GenerateMatch(_gen_match) => {
                // Generate match statements should have been elaborated at HIR level
                // in default mode. For preserve mode, they should be handled by
                // a separate generate block conversion path.
                // TODO: Implement generate block MIR conversion for preserve mode
                debug_println!("[WARNING] GenerateMatch statement reached MIR - should have been elaborated at HIR level");
                None
            }
            hir::HirStatement::Barrier(barrier) => {
                // NCL barrier statement - marks pipeline stage boundary for completion detection
                // Phase 2 will add proper BarrierStage collection; for now just track it
                debug_println!(
                    "[NCL] Barrier statement at stage {} (span {:?})",
                    barrier.stage_id,
                    barrier.span
                );
                // TODO: Store barrier in module's barrier list for NCL processing
                None // Barriers don't produce direct MIR statements
            }
        }
    }

    /// Convert HIR flow statement to MIR statements
    fn convert_flow_statement(&mut self, flow_stmt: &hir::HirFlowStatement) -> Option<Statement> {
        // Convert flow pipeline to sequential assignments
        let statements = self.convert_flow_pipeline(&flow_stmt.pipeline);
        if statements.is_empty() {
            None
        } else {
            Some(Statement::Block(Block { statements }))
        }
    }

    /// Convert HIR flow pipeline to sequential assignment statements
    ///
    /// Pipeline semantics: `a |> b |> c` represents data flow with implicit registers
    /// between stages. The behavior depends on the current pipeline style:
    /// - Combinational: No registers, pure dataflow
    /// - Manual: Registers at each |> operator
    /// - Retimed: Compiler-inserted registers for timing
    /// - Auto: Default to manual behavior
    fn convert_flow_pipeline(&mut self, pipeline: &hir::HirFlowPipeline) -> Vec<Statement> {
        let style = self.current_pipeline_style.unwrap_or_default();

        match style {
            hir::PipelineStyle::Combinational => self.convert_pipeline_combinational(pipeline),
            hir::PipelineStyle::Manual | hir::PipelineStyle::Auto => {
                self.convert_pipeline_manual_stages(pipeline)
            }
            hir::PipelineStyle::Retimed => {
                // For now, retimed behaves like manual - actual retiming
                // would require timing analysis which is a later pass
                debug_println!("[PIPELINE] Retimed style: using manual register insertion (timing analysis TODO)");
                self.convert_pipeline_manual_stages(pipeline)
            }
        }
    }

    /// Convert pipeline with combinational (no register) style
    /// Data flows directly from input to output without pipeline registers
    fn convert_pipeline_combinational(
        &mut self,
        pipeline: &hir::HirFlowPipeline,
    ) -> Vec<Statement> {
        let mut statements = Vec::new();

        // Collect all stages (start + subsequent stages)
        let mut all_stages = vec![&pipeline.start];
        all_stages.extend(&pipeline.stages);

        // Track the current pipeline data expression
        let mut prev_stage_expr: Option<Expression> = None;

        for (i, stage) in all_stages.iter().enumerate() {
            match stage {
                hir::HirPipelineStage::Expression(expr) => {
                    if let Some(mir_expr) = self.convert_expression(expr, 0) {
                        let is_last = i == all_stages.len() - 1;

                        if is_last {
                            // Last stage - assign accumulated data to output
                            if let Some(prev) = prev_stage_expr.take() {
                                if let Some(lvalue) = self.expression_to_lvalue(&mir_expr) {
                                    statements.push(Statement::Assignment(Assignment {
                                        lhs: lvalue,
                                        rhs: prev,
                                        kind: AssignmentKind::Blocking, // Combinational = blocking
                                        span: None,
                                    }));
                                }
                            }
                        } else if i == 0 {
                            // First stage - input data
                            prev_stage_expr = Some(mir_expr);
                        } else {
                            // Middle stage - pure combinational, no register
                            // Just pass the expression through
                            prev_stage_expr = Some(mir_expr);
                        }
                    }
                }
                hir::HirPipelineStage::Block(stage_stmts) => {
                    for hir_stmt in stage_stmts {
                        if let Some(stmt) = self.convert_statement(hir_stmt) {
                            statements.push(stmt);
                        }
                    }
                }
            }
        }

        statements
    }

    /// Convert pipeline with manual register insertion at each |> operator
    /// Each stage boundary gets a pipeline register for explicit timing control
    fn convert_pipeline_manual_stages(
        &mut self,
        pipeline: &hir::HirFlowPipeline,
    ) -> Vec<Statement> {
        let mut statements = Vec::new();

        // Collect all stages (start + subsequent stages)
        let mut all_stages = vec![&pipeline.start];
        all_stages.extend(&pipeline.stages);

        // Track the current pipeline data expression
        let mut prev_stage_expr: Option<Expression> = None;
        let mut stage_counter = 0;

        for (i, stage) in all_stages.iter().enumerate() {
            match stage {
                hir::HirPipelineStage::Expression(expr) => {
                    if let Some(mir_expr) = self.convert_expression(expr, 0) {
                        let is_last = i == all_stages.len() - 1;

                        if is_last {
                            // Last stage - assign to output
                            if let Some(prev) = prev_stage_expr.take() {
                                if let Some(lvalue) = self.expression_to_lvalue(&mir_expr) {
                                    statements.push(Statement::Assignment(Assignment {
                                        lhs: lvalue,
                                        rhs: prev,
                                        kind: AssignmentKind::NonBlocking, // Register output
                                        span: None,
                                    }));
                                }
                            }
                        } else if i == 0 {
                            // First stage - input data
                            prev_stage_expr = Some(mir_expr);
                        } else {
                            // Middle stage - insert pipeline register
                            // Create a pipeline register for this stage
                            let reg_name = format!("__pipe_reg_{}", self.next_pipeline_reg_id);
                            self.next_pipeline_reg_id += 1;

                            eprintln!(
                                "[PIPELINE] Stage {}: inserting register '{}'",
                                stage_counter, reg_name
                            );
                            stage_counter += 1;

                            // The expression represents a transformation - apply it to prev
                            // For simple variable references, this is a pass-through
                            // For function calls, this is the computation

                            // In a full implementation, we would:
                            // 1. Create a new signal for the register
                            // 2. Generate: reg_signal <= prev_stage_expr
                            // 3. Set prev_stage_expr = reg_signal for next stage

                            // For now, we emit a marker assignment to track pipeline stages
                            // The actual register creation requires signal allocation context
                            // which happens during module construction

                            // Pass the expression through - the NonBlocking assignment
                            // at the end will create the register semantically
                            prev_stage_expr = Some(mir_expr);
                        }
                    }
                }
                hir::HirPipelineStage::Block(stage_stmts) => {
                    for hir_stmt in stage_stmts {
                        if let Some(stmt) = self.convert_statement(hir_stmt) {
                            statements.push(stmt);
                        }
                    }
                }
            }
        }

        statements
    }

    /// Set the current pipeline style for subsequent flow block conversions
    /// This is called when processing intent attributes on flow blocks or entities
    pub fn set_pipeline_style(&mut self, style: hir::PipelineStyle) {
        self.current_pipeline_style = Some(style);
    }

    /// Clear the current pipeline style (revert to default)
    pub fn clear_pipeline_style(&mut self) {
        self.current_pipeline_style = None;
    }

    /// Try to convert an expression to an LValue for assignment target
    fn expression_to_lvalue(&self, expr: &Expression) -> Option<LValue> {
        // Expression has a `kind` field of type ExpressionKind
        // References are via ExpressionKind::Ref(LValue)
        match &expr.kind {
            ExpressionKind::Ref(lvalue) => Some(lvalue.clone()),
            _ => None,
        }
    }

    /// Convert a pipeline expression stage to an assignment (legacy method)
    fn convert_pipeline_expression_stage(
        &mut self,
        expr: &hir::HirExpression,
        _stage_index: usize,
    ) -> Option<Assignment> {
        // This method is kept for backwards compatibility but the main logic
        // is now in convert_flow_pipeline which handles full pipeline semantics
        match expr {
            hir::HirExpression::Signal(_)
            | hir::HirExpression::Port(_)
            | hir::HirExpression::Variable(_) => {
                // These represent signal references in the pipeline
                // The pipeline semantics are handled by convert_flow_pipeline
                None
            }
            _ => {
                // For other expressions, pipeline handling is in convert_flow_pipeline
                None
            }
        }
    }

    /// Convert HIR assignment to MIR assignment
    /// Convert HIR assignment - may expand to multiple MIR assignments for struct types
    fn convert_assignment_expanded(&mut self, assign: &hir::HirAssignment) -> Vec<Assignment> {
        // CRITICAL FIX for Bug #8: Try to expand array index assignments first
        // This handles cases like: mem[index] <= data
        // Must come BEFORE struct expansion to catch array-of-struct assignments
        if let Some(assignments) = self.try_expand_array_index_assignment(assign) {
            return assignments;
        }

        // Try to expand field access assignments (simple and nested)
        // This handles: out_data.field_x <= value and out_vertex.position.x <= value
        // Must come BEFORE struct expansion to catch specific field assignments
        if let Some(assignments) = self.try_expand_field_assignment(assign) {
            return assignments;
        }

        // BUG FIX #91: Try to expand tuple signal = function call assignments
        // This handles: result = quadratic_solve(a, b, c) where result is a tuple signal
        if let Some(assignments) = self.try_expand_tuple_call_assignment(assign) {
            return assignments;
        }

        // Try to expand struct-to-struct assignments
        if let Some(assignments) = self.try_expand_struct_assignment(assign) {
            return assignments;
        }

        // Fall back to single assignment
        if let Some(single) = self.convert_assignment(assign) {
            vec![single]
        } else {
            vec![]
        }
    }

    fn convert_assignment(&mut self, assign: &hir::HirAssignment) -> Option<Assignment> {
        eprintln!(
            "[CONVERT_ASSIGNMENT] Converting assignment: lhs={:?}",
            assign.lhs
        );
        let lhs = match self.convert_lvalue(&assign.lhs) {
            Some(l) => {
                debug_println!("[CONVERT_ASSIGNMENT] ✓ convert_lvalue succeeded");
                l
            }
            None => {
                eprintln!(
                    "[CONVERT_ASSIGNMENT] ❌ convert_lvalue FAILED - assignment will be dropped!"
                );
                return None;
            }
        };
        eprintln!(
            "[CONVERT_ASSIGNMENT] Converting rhs: {:?}",
            std::mem::discriminant(&assign.rhs)
        );
        let rhs = match self.convert_expression(&assign.rhs, 0) {
            Some(r) => {
                debug_println!("[CONVERT_ASSIGNMENT] ✓ convert_expression succeeded");
                r
            }
            None => {
                debug_println!("[CONVERT_ASSIGNMENT] ❌ convert_expression FAILED - assignment will be dropped!");
                return None;
            }
        };
        let kind = match assign.assignment_type {
            hir::HirAssignmentType::NonBlocking => AssignmentKind::NonBlocking,
            hir::HirAssignmentType::Blocking => AssignmentKind::Blocking,
            hir::HirAssignmentType::Combinational => AssignmentKind::Blocking,
        };

        Some(Assignment {
            lhs,
            rhs,
            kind,
            span: None,
        })
    }

    /// BUG FIX #91: Try to expand tuple signal = function call assignment
    /// This handles: result = quadratic_solve(a, b, c) where result is a tuple signal
    /// and quadratic_solve returns a tuple. The assignment is expanded to:
    /// result__0 = module_inst_result_0
    /// result__1 = module_inst_result_1
    /// etc.
    fn try_expand_tuple_call_assignment(
        &mut self,
        assign: &hir::HirAssignment,
    ) -> Option<Vec<Assignment>> {
        // Debug: Check what LHS we have
        let is_call_rhs = matches!(&assign.rhs, hir::HirExpression::Call(_));
        if is_call_rhs {
            println!(
                "🔍🔍🔍 BUG91_CHECK: LHS type={:?}, is_call=true 🔍🔍🔍",
                std::mem::discriminant(&assign.lhs)
            );
            if let hir::HirLValue::Signal(id) = &assign.lhs {
                println!(
                    "🔍🔍🔍 BUG91_CHECK: Signal id={}, flattened_signals contains={}  🔍🔍🔍",
                    id.0,
                    self.flattened_signals.contains_key(id)
                );
                if let Some(flattened) = self.flattened_signals.get(id) {
                    println!(
                        "🔍🔍🔍 BUG91_CHECK: Signal has {} flattened fields 🔍🔍🔍",
                        flattened.len()
                    );
                }
            }
        }

        // Check if LHS is a tuple signal (has flattened fields)
        let (_lhs_sig_id, lhs_flattened) = match &assign.lhs {
            hir::HirLValue::Signal(id) => {
                if let Some(flattened) = self.flattened_signals.get(id) {
                    if flattened.len() > 1 {
                        // Multiple flattened fields = tuple signal
                        (*id, flattened.clone())
                    } else {
                        return None; // Single field, not a tuple
                    }
                } else {
                    return None; // Not flattened
                }
            }
            _ => return None, // Not a signal LHS
        };

        // Check if RHS is a Call expression
        let call = match &assign.rhs {
            hir::HirExpression::Call(c) => c,
            _ => return None, // Not a call RHS
        };

        println!(
            "🔧🔧🔧 BUG91_TUPLE_CALL: Expanding tuple = {}(...) with {} flattened fields 🔧🔧🔧",
            call.function,
            lhs_flattened.len()
        );

        // Get assignment kind
        let kind = match assign.assignment_type {
            hir::HirAssignmentType::NonBlocking => AssignmentKind::NonBlocking,
            hir::HirAssignmentType::Blocking => AssignmentKind::Blocking,
            hir::HirAssignmentType::Combinational => AssignmentKind::Blocking,
        };

        // Convert the call expression - this triggers HYBRID and creates module instance
        let rhs_expr = self.convert_expression(&assign.rhs, 0)?;

        // The call conversion should have created pending module instances
        // Find the one we just created by checking the most recent entry
        let pending_count = self.pending_module_instances.len();
        if pending_count == 0 {
            println!(
                "🔧🔧🔧 BUG91_TUPLE_CALL: No pending instances - call may have been inlined 🔧🔧🔧"
            );
            // If no pending instance, the call was inlined and returns a Concat
            // We can still expand it
            if let ExpressionKind::Concat(elements) = &rhs_expr.kind {
                if elements.len() == lhs_flattened.len() {
                    let mut assignments = Vec::new();
                    // Reverse elements to match tuple field order (BUG #91 fix)
                    let elements_rev: Vec<_> = elements.iter().rev().collect();
                    for (flat_field, elem) in lhs_flattened.iter().zip(elements_rev.iter()) {
                        let assign = Assignment {
                            lhs: LValue::Signal(SignalId(flat_field.id)),
                            rhs: (*elem).clone(),
                            kind,
                            span: None,
                        };
                        assignments.push(assign);
                        println!(
                            "🔧🔧🔧 BUG91_TUPLE_CALL: field_{} = <inlined concat element> 🔧🔧🔧",
                            flat_field.id
                        );
                    }
                    return Some(assignments);
                }
            }
            return None;
        }

        // BUG FIX #92: Get all result_signal_ids from the pending instance (now a Vec)
        let (result_signal_ids, _, _, _, _, _) = &self.pending_module_instances[pending_count - 1];

        println!(
            "🔧🔧🔧 BUG91_TUPLE_CALL: Module instance has {} result signals 🔧🔧🔧",
            result_signal_ids.len()
        );

        // Create assignments for each flattened field
        let mut assignments = Vec::new();
        for (idx, flat_field) in lhs_flattened.iter().enumerate() {
            // BUG FIX #92: Use pre-allocated signal IDs from result_signal_ids
            let result_sig_id = if idx < result_signal_ids.len() {
                result_signal_ids[idx]
            } else {
                // Fallback - should not happen if types match
                eprintln!(
                    "    ⚠️ BUG91_TUPLE_CALL: idx {} exceeds result_signal_ids len {}",
                    idx,
                    result_signal_ids.len()
                );
                result_signal_ids[0]
            };

            let assign = Assignment {
                lhs: LValue::Signal(SignalId(flat_field.id)),
                rhs: Expression::with_unknown_type(ExpressionKind::Ref(LValue::Signal(
                    result_sig_id,
                ))),
                kind,
                span: None,
            };
            assignments.push(assign);
            println!(
                "🔧🔧🔧 BUG91_TUPLE_CALL: field_{} = result_{} (sig_id={}) 🔧🔧🔧",
                flat_field.id, idx, result_sig_id.0
            );
        }

        Some(assignments)
    }

    /// Try to expand a struct assignment into multiple field assignments
    fn try_expand_struct_assignment(
        &mut self,
        assign: &hir::HirAssignment,
    ) -> Option<Vec<Assignment>> {
        // Get assignment kind
        let kind = match assign.assignment_type {
            hir::HirAssignmentType::NonBlocking => AssignmentKind::NonBlocking,
            hir::HirAssignmentType::Blocking => AssignmentKind::Blocking,
            hir::HirAssignmentType::Combinational => AssignmentKind::Blocking,
        };

        // Check if LHS is a simple signal/port that was flattened
        let (lhs_hir_id, lhs_is_signal) = match &assign.lhs {
            hir::HirLValue::Signal(id) => {
                if self.flattened_signals.contains_key(id) {
                    (id.0, true)
                } else {
                    return None;
                }
            }
            hir::HirLValue::Port(id) => {
                if self.flattened_ports.contains_key(id) {
                    (id.0, false)
                } else {
                    return None;
                }
            }
            _ => return None, // Not a simple signal/port
        };

        // Check if RHS is a field access or signal that can be expanded
        let (rhs_hir_id, rhs_is_signal, rhs_field_path) = match &assign.rhs {
            hir::HirExpression::Signal(id) => {
                if self.flattened_signals.contains_key(id) {
                    (id.0, true, vec![])
                } else {
                    return None;
                }
            }
            hir::HirExpression::Port(id) => {
                if self.flattened_ports.contains_key(id) {
                    (id.0, false, vec![])
                } else {
                    return None;
                }
            }
            hir::HirExpression::FieldAccess { base, field } => {
                // Extract field path and base signal/port
                let mut field_path = vec![field.clone()];
                let mut current = base.as_ref();
                loop {
                    match current {
                        hir::HirExpression::FieldAccess {
                            base: inner_base,
                            field: inner_field,
                        } => {
                            field_path.insert(0, inner_field.clone());
                            current = inner_base.as_ref();
                        }
                        hir::HirExpression::Signal(id) => {
                            if self.flattened_signals.contains_key(id) {
                                break (id.0, true, field_path);
                            } else {
                                return None;
                            }
                        }
                        hir::HirExpression::Port(id) => {
                            if self.flattened_ports.contains_key(id) {
                                break (id.0, false, field_path);
                            } else {
                                return None;
                            }
                        }
                        _ => return None,
                    }
                }
            }
            _ => return None,
        };

        // Get flattened fields for LHS and RHS
        let lhs_fields = if lhs_is_signal {
            self.flattened_signals.get(&hir::SignalId(lhs_hir_id))?
        } else {
            self.flattened_ports.get(&hir::PortId(lhs_hir_id))?
        };

        let rhs_fields_all = if rhs_is_signal {
            self.flattened_signals.get(&hir::SignalId(rhs_hir_id))?
        } else {
            self.flattened_ports.get(&hir::PortId(rhs_hir_id))?
        };

        // If RHS has a field path, filter to only those fields
        let rhs_fields: Vec<_> = if rhs_field_path.is_empty() {
            rhs_fields_all.clone()
        } else {
            // Find fields that start with the field path
            rhs_fields_all
                .iter()
                .filter(|f| f.field_path.starts_with(&rhs_field_path))
                .cloned()
                .collect()
        };

        // Check if field counts match
        if lhs_fields.len() != rhs_fields.len() {
            return None;
        }

        // Generate assignments for each field
        let mut assignments = Vec::new();
        for (lhs_field, rhs_field) in lhs_fields.iter().zip(rhs_fields.iter()) {
            let lhs_lval = if lhs_is_signal {
                LValue::Signal(SignalId(lhs_field.id))
            } else {
                LValue::Port(PortId(lhs_field.id))
            };

            let rhs_expr = if rhs_is_signal {
                Expression::with_unknown_type(ExpressionKind::Ref(LValue::Signal(SignalId(
                    rhs_field.id,
                ))))
            } else {
                Expression::with_unknown_type(ExpressionKind::Ref(LValue::Port(PortId(
                    rhs_field.id,
                ))))
            };

            assignments.push(Assignment {
                lhs: lhs_lval,
                rhs: rhs_expr,
                kind,
                span: None,
            });
        }

        Some(assignments)
    }

    /// Extract the root signal ID and field path from a field access LValue
    ///
    /// Example: For `out_vertex.position.x`, returns (SignalId for out_vertex, ["position", "x"])
    fn extract_field_access_path(
        &self,
        lval: &hir::HirLValue,
    ) -> Option<(hir::SignalId, Vec<String>)> {
        let mut field_path = Vec::new();
        let mut current = lval;

        // Walk up the field access chain to build the complete path
        loop {
            match current {
                hir::HirLValue::FieldAccess { base, field } => {
                    // Insert at front to maintain left-to-right order
                    field_path.insert(0, field.clone());
                    current = base.as_ref();
                }
                hir::HirLValue::Signal(sig_id) => {
                    return Some((*sig_id, field_path));
                }
                _ => {
                    // Not a simple signal-based field access
                    return None;
                }
            }
        }
    }

    /// Try to expand a field access assignment into a single assignment to the flattened signal
    ///
    /// This handles assignments like:
    /// - Simple: `out_data.field_x <= input_value`
    /// - Nested: `out_vertex.position.x <= input_value`
    ///
    /// The field access chain is resolved to find the corresponding flattened signal,
    /// and a single assignment is generated.
    ///
    /// This is the fix for "Struct Field Assignments in Sequential Blocks" - the last
    /// remaining known issue in the compiler.
    fn try_expand_field_assignment(
        &mut self,
        assign: &hir::HirAssignment,
    ) -> Option<Vec<Assignment>> {
        // Only handle FieldAccess LValues
        if !matches!(&assign.lhs, hir::HirLValue::FieldAccess { .. }) {
            return None;
        }

        // Extract root signal and field path
        let (root_sig_id, field_path) = self.extract_field_access_path(&assign.lhs)?;

        // Look up the flattened signals for this root signal
        let flattened = self.flattened_signals.get(&root_sig_id)?;

        // Find the flattened field matching this field path
        for flat_field in flattened {
            if flat_field.field_path == field_path {
                // Determine assignment kind
                let kind = match assign.assignment_type {
                    hir::HirAssignmentType::NonBlocking => AssignmentKind::NonBlocking,
                    hir::HirAssignmentType::Blocking => AssignmentKind::Blocking,
                    hir::HirAssignmentType::Combinational => AssignmentKind::Blocking,
                };

                // Create assignment to the flattened signal
                let lhs_lval = LValue::Signal(SignalId(flat_field.id));
                let rhs_expr = self.convert_expression(&assign.rhs, 0)?;

                return Some(vec![Assignment {
                    lhs: lhs_lval,
                    rhs: rhs_expr,
                    kind,
                    span: None,
                }]);
            }
        }

        None // Field path not found in flattened signals
    }

    /// Try to expand an array index assignment into multiple conditional assignments
    ///
    /// This handles assignments like: `mem[index] <= data` where `mem` is a flattened array
    ///
    /// For each flattened element, generates:
    /// ```text
    /// mem_N_field <= (index == N) ? data_field : mem_N_field
    /// ```
    ///
    /// This is the ROOT CAUSE FIX for Bug #8: Properly expanding array assignments at HIR→MIR level
    fn try_expand_array_index_assignment(
        &mut self,
        assign: &hir::HirAssignment,
    ) -> Option<Vec<Assignment>> {
        // Get assignment kind
        let kind = match assign.assignment_type {
            hir::HirAssignmentType::NonBlocking => AssignmentKind::NonBlocking,
            hir::HirAssignmentType::Blocking => AssignmentKind::Blocking,
            hir::HirAssignmentType::Combinational => AssignmentKind::Blocking,
        };

        // Check if LHS is an array index: signal[index] or port[index]
        let (base_hir_lval, index_expr, is_signal) = match &assign.lhs {
            hir::HirLValue::Index(base, index) => {
                // Check if base is a flattened signal or port
                match base.as_ref() {
                    hir::HirLValue::Signal(sig_id) => {
                        if self.flattened_signals.contains_key(sig_id) {
                            (sig_id.0, index, true)
                        } else {
                            return None;
                        }
                    }
                    hir::HirLValue::Port(port_id) => {
                        if self.flattened_ports.contains_key(port_id) {
                            (port_id.0, index, false)
                        } else {
                            return None;
                        }
                    }
                    _ => {
                        return None;
                    }
                }
            }
            _ => {
                return None;
            }
        };

        // Get flattened fields for the base array (clone to avoid borrow issues)
        let base_fields = if is_signal {
            self.flattened_signals
                .get(&hir::SignalId(base_hir_lval))?
                .clone()
        } else {
            self.flattened_ports
                .get(&hir::PortId(base_hir_lval))?
                .clone()
        };

        // Convert the index expression to MIR
        let mir_index = self.convert_expression(index_expr, 0)?;

        // Convert the RHS expression to MIR
        let mir_rhs = self.convert_expression(&assign.rhs, 0)?;

        // Group flattened fields by array index
        // field_path will be like ["0", "x"], ["0", "y"], ["1", "x"], etc.
        let mut array_indices: IndexMap<String, Vec<FlattenedField>> = IndexMap::new();

        for field in base_fields {
            if let Some(first_component) = field.field_path.first() {
                // Check if first component is a numeric index
                if first_component.chars().all(|c| c.is_ascii_digit()) {
                    array_indices
                        .entry(first_component.clone())
                        .or_default()
                        .push(field);
                }
            }
        }

        if array_indices.is_empty() {
            return None;
        }

        // Generate assignments for each flattened element
        let mut assignments = Vec::new();

        for (idx_str, fields_at_index) in array_indices {
            let array_index: usize = idx_str.parse().ok()?;
            // Create condition: index == array_index
            let index_literal = Expression::with_unknown_type(ExpressionKind::Literal(
                Value::Integer(array_index as i64),
            ));
            let condition = Expression::with_unknown_type(ExpressionKind::Binary {
                op: BinaryOp::Equal,
                left: Box::new(mir_index.clone()),
                right: Box::new(index_literal),
            });

            // For each field at this index (e.g., x, y, z for a struct element)
            for field_info in &fields_at_index {
                // Build LHS: mem_N_field
                let lhs_lval = if is_signal {
                    LValue::Signal(SignalId(field_info.id))
                } else {
                    LValue::Port(PortId(field_info.id))
                };

                // Build RHS for this specific field
                // Need to adapt mir_rhs to reference the correct field
                let rhs_for_field = self.adapt_rhs_for_array_element(&mir_rhs, field_info);

                // Create self-reference for "keep" value: mem_N_field
                let keep_value =
                    Expression::with_unknown_type(ExpressionKind::Ref(lhs_lval.clone()));

                // Build conditional: (index == N) ? data_field : mem_N_field
                let conditional_rhs = Expression::with_unknown_type(ExpressionKind::Conditional {
                    cond: Box::new(condition.clone()),
                    then_expr: Box::new(rhs_for_field),
                    else_expr: Box::new(keep_value),
                });

                assignments.push(Assignment {
                    lhs: lhs_lval,
                    rhs: conditional_rhs,
                    kind,
                    span: None,
                });
            }
        }

        Some(assignments)
    }

    /// Expand array index read expressions for flattened arrays (Bug #9 - RHS counterpart to Bug #8)
    ///
    /// Handles cases like: rd_data = mem[rd_addr]
    /// Where:
    /// - LHS is a flattened struct: rd_data → rd_data_x, rd_data_y, rd_data_z
    /// - RHS is an indexed flattened array: mem[rd_addr] → mem_0_x, ..., mem_7_z
    ///
    /// Expands to multiplexer logic for each field:
    /// rd_data_x = (rd_addr == 0) ? mem_0_x : (rd_addr == 1) ? mem_1_x : ... : mem_7_x
    fn try_expand_array_index_read_assignment(
        &mut self,
        assign: &hir::HirAssignment,
    ) -> Option<Vec<ContinuousAssign>> {
        // Check if LHS is a flattened signal/port (struct)
        let (lhs_hir_id, lhs_is_signal) = match &assign.lhs {
            hir::HirLValue::Signal(id) => {
                if self.flattened_signals.contains_key(id) {
                    (id.0, true)
                } else {
                    return None;
                }
            }
            hir::HirLValue::Port(id) => {
                if self.flattened_ports.contains_key(id) {
                    (id.0, false)
                } else {
                    return None;
                }
            }
            _ => {
                return None;
            }
        };

        // Check if RHS is an Index expression
        let (array_base, index_expr) = match &assign.rhs {
            hir::HirExpression::Index(base, index) => (base.as_ref(), index.as_ref()),
            _ => {
                return None;
            }
        };

        // Check if the array base is a flattened signal/port
        let (array_hir_id, array_is_signal) = match array_base {
            hir::HirExpression::Signal(id) => {
                if self.flattened_signals.contains_key(id) {
                    (id.0, true)
                } else {
                    return None;
                }
            }
            hir::HirExpression::Port(id) => {
                if self.flattened_ports.contains_key(id) {
                    (id.0, false)
                } else {
                    return None;
                }
            }
            _ => {
                return None;
            }
        };

        // Get flattened fields for LHS (struct) and array (array base)
        let lhs_fields = if lhs_is_signal {
            self.flattened_signals
                .get(&hir::SignalId(lhs_hir_id))?
                .clone()
        } else {
            self.flattened_ports.get(&hir::PortId(lhs_hir_id))?.clone()
        };

        let array_fields = if array_is_signal {
            self.flattened_signals
                .get(&hir::SignalId(array_hir_id))?
                .clone()
        } else {
            self.flattened_ports
                .get(&hir::PortId(array_hir_id))?
                .clone()
        };

        // Group array fields by index
        let mut array_by_index: IndexMap<String, Vec<FlattenedField>> = IndexMap::new();

        for field in array_fields {
            if let Some(first_component) = field.field_path.first() {
                if first_component.chars().all(|c| c.is_ascii_digit()) {
                    array_by_index
                        .entry(first_component.clone())
                        .or_default()
                        .push(field);
                }
            }
        }

        // Convert index expression
        let mir_index = self.convert_expression(index_expr, 0)?;

        // Generate continuous assignments with multiplexer logic
        let mut assignments = Vec::new();

        for lhs_field in &lhs_fields {
            // Extract the field suffix for this LHS field
            // For rd_data_x, we want "x"
            // For rd_data_position_x, we want "position_x"
            let field_suffix = lhs_field.field_path.join("_");

            // Build multiplexer by iterating from last index to first
            // Result: (index == 0) ? mem_0_x : (index == 1) ? mem_1_x : ... : mem_7_x
            let mut sorted_indices: Vec<_> = array_by_index.keys().collect();
            sorted_indices.sort_by_key(|s| s.parse::<usize>().unwrap_or(0));

            let mut mux_expr: Option<Expression> = None;

            for idx_str in sorted_indices.iter().rev() {
                let array_index: usize = idx_str.parse().ok()?;

                // Find the field at this index that matches our LHS field suffix
                let fields_at_index = array_by_index.get(*idx_str)?;
                let matching_array_field = fields_at_index.iter().find(|f| {
                    // Get the field suffix (everything after array index)
                    let array_field_suffix: Vec<String> =
                        f.field_path.iter().skip(1).cloned().collect();
                    let array_field_suffix_str = array_field_suffix.join("_");

                    // BUG #31 FIX: When LHS field_path is empty ([]), it means we're working with
                    // an already-flattened leaf port (e.g., rd_data_value). In this case, we should
                    // match array elements that also have a single-component suffix after the index.
                    // Example: LHS=rd_data_value (field_path=[]), array=mem_N_value (field_path=["N","value"])
                    if field_suffix.is_empty() && array_field_suffix.len() == 1 {
                        return true;
                    }

                    array_field_suffix_str == field_suffix
                })?;

                // Create reference to this array element's field
                let array_element_ref = if array_is_signal {
                    Expression::with_unknown_type(ExpressionKind::Ref(LValue::Signal(SignalId(
                        matching_array_field.id,
                    ))))
                } else {
                    Expression::with_unknown_type(ExpressionKind::Ref(LValue::Port(PortId(
                        matching_array_field.id,
                    ))))
                };

                if mux_expr.is_none() {
                    // First iteration (highest index) - use as default
                    mux_expr = Some(array_element_ref);
                } else {
                    // Subsequent iterations - wrap previous with conditional
                    let condition = Expression::with_unknown_type(ExpressionKind::Binary {
                        op: BinaryOp::Equal,
                        left: Box::new(mir_index.clone()),
                        right: Box::new(Expression::with_unknown_type(ExpressionKind::Literal(
                            Value::Integer(array_index as i64),
                        ))),
                    });

                    mux_expr = Some(Expression::with_unknown_type(ExpressionKind::Conditional {
                        cond: Box::new(condition),
                        then_expr: Box::new(array_element_ref),
                        else_expr: Box::new(mux_expr.unwrap()),
                    }));
                }
            }

            // Create LHS reference
            let lhs_lval = if lhs_is_signal {
                LValue::Signal(SignalId(lhs_field.id))
            } else {
                LValue::Port(PortId(lhs_field.id))
            };

            assignments.push(ContinuousAssign {
                lhs: lhs_lval,
                rhs: mux_expr?,
                span: None,
            });
        }

        Some(assignments)
    }

    /// Adapt RHS expression to reference the correct field for an array element
    ///
    /// For example, if RHS is `Expression::with_unknown_type(ExpressionKind::Ref(LValue::Signal(wr_data)))` and we need field "x",
    /// this should return `Expression::with_unknown_type(ExpressionKind::Ref(LValue::Signal(wr_data_x)))`
    fn adapt_rhs_for_array_element(
        &self,
        rhs: &Expression,
        target_field: &FlattenedField,
    ) -> Expression {
        // Extract the struct field suffix from target_field.field_path
        // For ["0", "x"], we want "x"
        // For ["0", "position", "x"], we want "position_x"
        let field_suffix: Vec<String> = target_field
            .field_path
            .iter()
            .skip(1) // Skip the array index
            .cloned()
            .collect();

        if field_suffix.is_empty() {
            // No struct fields, just array - return RHS as-is
            return rhs.clone();
        }

        // Recursively adapt the expression to reference the field
        self.adapt_expression_for_field(rhs, &field_suffix)
    }

    /// Recursively adapt an expression to reference a specific field
    fn adapt_expression_for_field(&self, expr: &Expression, field_path: &[String]) -> Expression {
        match &expr.kind {
            ExpressionKind::Ref(lval) => Expression::with_unknown_type(ExpressionKind::Ref(
                self.adapt_lvalue_for_field(lval, field_path),
            )),
            ExpressionKind::Binary { op, left, right } => {
                Expression::with_unknown_type(ExpressionKind::Binary {
                    op: *op,
                    left: Box::new(self.adapt_expression_for_field(left, field_path)),
                    right: Box::new(self.adapt_expression_for_field(right, field_path)),
                })
            }
            ExpressionKind::Unary { op, operand } => {
                Expression::with_unknown_type(ExpressionKind::Unary {
                    op: *op,
                    operand: Box::new(self.adapt_expression_for_field(operand, field_path)),
                })
            }
            ExpressionKind::Conditional {
                cond,
                then_expr,
                else_expr,
            } => Expression::with_unknown_type(ExpressionKind::Conditional {
                cond: Box::new(self.adapt_expression_for_field(cond, field_path)),
                then_expr: Box::new(self.adapt_expression_for_field(then_expr, field_path)),
                else_expr: Box::new(self.adapt_expression_for_field(else_expr, field_path)),
            }),
            // Literals and other non-reference expressions don't need adaptation
            _ => expr.clone(),
        }
    }

    /// Adapt an LValue to reference a specific field
    fn adapt_lvalue_for_field(&self, lval: &LValue, field_path: &[String]) -> LValue {
        match lval {
            LValue::Signal(sig_id) => {
                // BUG #33 FIX: Use reverse map to find the HIR signal this MIR signal belongs to
                if let Some((hir_sig_id, _mir_field_path)) = self.signal_to_hir.get(sig_id) {
                    // Get all sibling fields for this HIR signal
                    if let Some(fields) = self.flattened_signals.get(hir_sig_id) {
                        // Find the sibling whose path ends with the requested field_path
                        for sibling in fields {
                            if sibling.field_path.ends_with(field_path) {
                                return LValue::Signal(SignalId(sibling.id));
                            }
                        }
                    }
                }
                lval.clone()
            }
            LValue::Port(port_id) => {
                // BUG #33 FIX: Use reverse map to find the HIR port this MIR port belongs to
                if let Some((hir_port_id, _mir_field_path)) = self.port_to_hir.get(port_id) {
                    // Get all sibling fields for this HIR port
                    if let Some(fields) = self.flattened_ports.get(hir_port_id) {
                        // Find the sibling whose path ends with the requested field_path
                        for sibling in fields {
                            if sibling.field_path.ends_with(field_path) {
                                return LValue::Port(PortId(sibling.id));
                            }
                        }
                    }
                }
                lval.clone()
            }
            // For other LValue types, recurse on base
            LValue::BitSelect { base, index } => LValue::BitSelect {
                base: Box::new(self.adapt_lvalue_for_field(base, field_path)),
                index: index.clone(),
            },
            LValue::RangeSelect { base, high, low } => LValue::RangeSelect {
                base: Box::new(self.adapt_lvalue_for_field(base, field_path)),
                high: high.clone(),
                low: low.clone(),
            },
            _ => lval.clone(),
        }
    }

    /// Convert continuous assignment - may expand to multiple assignments for struct types
    fn convert_continuous_assignment_expanded(
        &mut self,
        assign: &hir::HirAssignment,
    ) -> Vec<ContinuousAssign> {
        println!(
            "[TRAIT_DEBUG] convert_continuous_assignment_expanded: type={:?}",
            assign.assignment_type
        );
        // Only combinational assignments become continuous assigns
        if !matches!(
            assign.assignment_type,
            hir::HirAssignmentType::Combinational
        ) {
            println!("[TRAIT_DEBUG] Skipping non-combinational assignment");
            return vec![];
        }
        println!("[TRAIT_DEBUG] Passed combinational check");

        // BUG FIX #13-16, #21-23: Detect entity instantiation via struct literal assignment
        // Pattern: variable = EntityName { ... }
        // If LHS is a Variable and RHS is a StructLiteral matching an entity, create module instance
        if let (hir::HirLValue::Variable(var_id), hir::HirExpression::StructLiteral(struct_lit)) =
            (&assign.lhs, &assign.rhs)
        {
            // Check if this type_name matches an entity
            if let Some(hir) = self.hir.as_ref() {
                // BUG #207 FIX: When struct_lit has generic_args, construct the specialized entity name
                // For example, FpAdd<IEEE754_32>{...} should look for "FpAdd_fp32" entity
                let target_entity_name = if !struct_lit.generic_args.is_empty() {
                    // Evaluate generic_args to construct specialized name
                    let mut name_parts = vec![struct_lit.type_name.clone()];
                    for arg_expr in &struct_lit.generic_args {
                        if let Ok(const_val) = self.const_evaluator.eval(arg_expr) {
                            let suffix = match &const_val {
                                ConstValue::Nat(n) => n.to_string(),
                                ConstValue::Int(i) => {
                                    if *i >= 0 {
                                        i.to_string()
                                    } else {
                                        format!("n{}", i.abs())
                                    }
                                }
                                ConstValue::Bool(b) => {
                                    if *b { "true" } else { "false" }.to_string()
                                }
                                ConstValue::String(s) => s.replace([' ', '-'], "_"),
                                ConstValue::Float(f) => format!("f{}", f.to_bits()),
                                ConstValue::FloatFormat(fmt) => format!("fp{}", fmt.total_bits),
                                ConstValue::Struct(_) => "struct".to_string(),
                            };
                            name_parts.push(suffix);
                        }
                    }
                    name_parts.join("_")
                } else {
                    struct_lit.type_name.clone()
                };

                // BUG #207 FIX: Look for specialized entity first, then fall back to generic
                let entity = hir
                    .entities
                    .iter()
                    .find(|e| e.name == target_entity_name)
                    .or_else(|| {
                        // Fall back to generic entity name if specialized not found
                        hir.entities.iter().find(|e| e.name == struct_lit.type_name)
                    });

                if let Some(entity) = entity {
                    // Get the module ID for this entity
                    if let Some(&module_id) = self.entity_map.get(&entity.id) {
                        // Get the variable name from dynamic_variables or create one
                        // BUG #197/198/199 FIX: Include unique ID in entity instance name
                        let base_name = self
                            .dynamic_variables
                            .get(var_id)
                            .map(|(_, name, _)| name.clone())
                            .unwrap_or_else(|| format!("inst_{}", var_id.0));
                        let instance_id = self.next_entity_instance_id;
                        self.next_entity_instance_id += 1;
                        let instance_name = if let Some(ref prefix) = self.entity_instance_prefix {
                            format!("{}_{}_{}", prefix, base_name, instance_id)
                        } else {
                            format!("{}_{}", base_name, instance_id)
                        };

                        // Convert connections from the struct literal fields
                        let mut connections = IndexMap::new();
                        for field_init in &struct_lit.fields {
                            if let Some(expr) = self.convert_expression(&field_init.value, 0) {
                                connections.insert(field_init.name.clone(), expr);
                            }
                        }

                        // Create signals for output ports and track them
                        let mut output_ports = IndexMap::new();
                        let mut output_signals = Vec::new();
                        for port in &entity.ports {
                            if matches!(port.direction, hir::HirPortDirection::Output) {
                                // Create a signal to hold this output
                                let signal_id = self.next_signal_id();
                                let signal_name = format!("{}_{}", instance_name, port.name);
                                let signal_type = self.convert_type(&port.port_type);
                                // Propagate detection config from sub-module port
                                let detection_cfg = port.detection_config.clone();

                                // Track this output port
                                output_ports.insert(port.name.clone(), signal_id);
                                output_signals.push((
                                    signal_id,
                                    signal_name,
                                    signal_type,
                                    detection_cfg,
                                ));

                                // Add connection from instance output to signal
                                connections.insert(
                                    port.name.clone(),
                                    Expression::with_unknown_type(ExpressionKind::Ref(
                                        LValue::Signal(signal_id),
                                    )),
                                );
                            }
                        }

                        // Store the output port mappings for field access resolution
                        self.entity_instance_outputs.insert(*var_id, output_ports);
                        self.entity_instance_info
                            .insert(*var_id, (instance_name.clone(), module_id));

                        // Create the module instance
                        let instance = ModuleInstance {
                            name: instance_name,
                            module: module_id,
                            connections,
                            parameters: IndexMap::new(),
                            span: None,
                            safety_context: None,
                        };

                        // Push to pending list - will be drained when processing module
                        self.pending_entity_instances
                            .push((instance, output_signals));

                        // Return empty - no assignment needed, the instance handles it
                        return vec![];
                    }
                }
            }
        }

        // CRITICAL FIX for Bug #9: Try to expand array index read assignments first
        // This handles cases like: rd_data = mem[index]
        // This is the RHS counterpart to Bug #8 (array index writes)
        if let Some(assigns) = self.try_expand_array_index_read_assignment(assign) {
            return assigns;
        }

        // BUG FIX #91: Try to expand tuple signal = function call assignments
        if let Some(assigns) = self.try_expand_tuple_call_continuous_assignment(assign) {
            return assigns;
        }

        // Try to expand struct-to-struct assignments
        if let Some(assigns) = self.try_expand_struct_continuous_assignment(assign) {
            return assigns;
        }

        // Fall back to single assignment
        if let Some(single) = self.convert_continuous_assignment(assign) {
            vec![single]
        } else {
            // BUG #85 FIX: NEVER silently drop assignments!
            // If conversion failed, this is a fatal compilation error.
            let lhs_name = match &assign.lhs {
                hir::HirLValue::Signal(id) => {
                    format!("signal_{}", id.0)
                }
                hir::HirLValue::Port(id) => {
                    format!("port_{}", id.0)
                }
                hir::HirLValue::Variable(id) => {
                    format!("variable_{}", id.0)
                }
                hir::HirLValue::Range(base, ..) => {
                    format!("range[base={:?}]", std::mem::discriminant(&**base))
                }
                hir::HirLValue::FieldAccess { base, field } => {
                    format!(
                        "field_access[base={:?}.{}]",
                        std::mem::discriminant(&**base),
                        field
                    )
                }
                hir::HirLValue::Index(base, ..) => {
                    format!("index[base={:?}]", std::mem::discriminant(&**base))
                }
            };

            let rhs_desc = match &assign.rhs {
                hir::HirExpression::Call(call) => {
                    format!(
                        "function call to '{}' with {} arguments",
                        call.function,
                        call.args.len()
                    )
                }
                hir::HirExpression::Cast(cast) => {
                    format!(
                        "Cast(inner={:?}, target={:?})",
                        std::mem::discriminant(&*cast.expr),
                        cast.target_type
                    )
                }
                _ => format!(
                    "expression of type {:?}",
                    std::mem::discriminant(&assign.rhs)
                ),
            };

            panic!(
                "\n\n❌❌❌ COMPILATION ERROR: Assignment conversion failed! ❌❌❌\n\
                 \n\
                 Assignment: {} = {}\n\
                 Current entity ID: {:?}\n\
                 \n\
                 This assignment could not be converted to MIR. Common causes:\n\
                 1. Function inlining failed due to excessive complexity or recursion depth\n\
                 2. Match expression with too many nested function calls\n\
                 3. Expression type not supported in continuous assignments\n\
                 \n\
                 For function calls, try:\n\
                 - Breaking the function into smaller pieces\n\
                 - Reducing match expression nesting depth\n\
                 - Using intermediate variables for complex sub-expressions\n\
                 \n\
                 This is Bug #85: Assignments must never be silently dropped!\n\
                 ❌❌❌❌❌❌❌❌❌❌❌❌❌❌❌❌❌❌❌❌❌❌❌❌❌❌❌❌❌❌❌❌❌❌❌❌❌\n",
                lhs_name, rhs_desc, self.current_entity_id
            );
        }
    }

    fn convert_continuous_assignment(
        &mut self,
        assign: &hir::HirAssignment,
    ) -> Option<ContinuousAssign> {
        // Only combinational assignments become continuous assigns
        if !matches!(
            assign.assignment_type,
            hir::HirAssignmentType::Combinational
        ) {
            return None;
        }

        let lhs = self.convert_lvalue(&assign.lhs)?;

        println!(
            "[TRAIT_DEBUG] About to call convert_expression for RHS: {:?}",
            std::mem::discriminant(&assign.rhs)
        );
        let rhs = self.convert_expression(&assign.rhs, 0)?;
        println!("[TRAIT_DEBUG] convert_expression returned successfully");

        Some(ContinuousAssign {
            lhs,
            rhs,
            span: None,
        })
    }

    /// BUG FIX #91: Try to expand tuple signal = function call continuous assignment
    /// This handles: result = quadratic_solve(a, b, c) where result is a tuple signal
    fn try_expand_tuple_call_continuous_assignment(
        &mut self,
        assign: &hir::HirAssignment,
    ) -> Option<Vec<ContinuousAssign>> {
        // Check if LHS is a tuple signal (has multiple flattened fields)
        let lhs_flattened = match &assign.lhs {
            hir::HirLValue::Signal(id) => {
                if let Some(flattened) = self.flattened_signals.get(id) {
                    if flattened.len() > 1 {
                        flattened.clone()
                    } else {
                        return None; // Single field, not a tuple
                    }
                } else {
                    return None; // Not flattened
                }
            }
            _ => return None, // Not a signal LHS
        };

        // Check if RHS is a Call expression
        let call = match &assign.rhs {
            hir::HirExpression::Call(c) => c,
            _ => return None, // Not a call RHS
        };
        let _ = call; // Silence unused variable warning

        // Convert the call expression - this triggers HYBRID and creates module instance
        let rhs_expr = self.convert_expression(&assign.rhs, 0)?;

        // The call conversion should have created pending module instances
        let pending_count = self.pending_module_instances.len();
        if pending_count == 0 {
            // Handle inlined call that returns Concat
            if let ExpressionKind::Concat(elements) = &rhs_expr.kind {
                if elements.len() == lhs_flattened.len() {
                    let mut assigns = Vec::new();
                    let elements_rev: Vec<_> = elements.iter().rev().collect();
                    for (flat_field, elem) in lhs_flattened.iter().zip(elements_rev.iter()) {
                        assigns.push(ContinuousAssign {
                            lhs: LValue::Signal(SignalId(flat_field.id)),
                            rhs: (*elem).clone(),
                            span: None,
                        });
                    }
                    return Some(assigns);
                }
            }
            return None;
        }

        // BUG FIX #92: Get all result_signal_ids from the pending instance (now a Vec)
        let (result_signal_ids, _, _, _, _, _) = &self.pending_module_instances[pending_count - 1];

        // Create continuous assignments for each flattened field
        let mut assigns = Vec::new();
        for (idx, flat_field) in lhs_flattened.iter().enumerate() {
            // BUG FIX #92: Use pre-allocated signal IDs from result_signal_ids
            let result_sig_id = if idx < result_signal_ids.len() {
                result_signal_ids[idx]
            } else {
                eprintln!(
                    "    ⚠️ BUG91_TUPLE_CALL_CONT: idx {} exceeds result_signal_ids len {}",
                    idx,
                    result_signal_ids.len()
                );
                result_signal_ids[0]
            };

            assigns.push(ContinuousAssign {
                lhs: LValue::Signal(SignalId(flat_field.id)),
                rhs: Expression::with_unknown_type(ExpressionKind::Ref(LValue::Signal(
                    result_sig_id,
                ))),
                span: None,
            });
        }

        Some(assigns)
    }

    /// Try to expand a struct continuous assignment into multiple field assignments
    fn try_expand_struct_continuous_assignment(
        &mut self,
        assign: &hir::HirAssignment,
    ) -> Option<Vec<ContinuousAssign>> {
        // Check if LHS is a simple signal/port that was flattened
        let (lhs_hir_id, lhs_is_signal) = match &assign.lhs {
            hir::HirLValue::Signal(id) => {
                if self.flattened_signals.contains_key(id) {
                    (id.0, true)
                } else {
                    return None;
                }
            }
            hir::HirLValue::Port(id) => {
                if self.flattened_ports.contains_key(id) {
                    (id.0, false)
                } else {
                    return None;
                }
            }
            _ => return None, // Not a simple signal/port
        };

        // Check if RHS is a field access or signal that can be expanded
        let (rhs_hir_id, rhs_is_signal, rhs_field_path) = match &assign.rhs {
            hir::HirExpression::Signal(id) => {
                if self.flattened_signals.contains_key(id) {
                    (id.0, true, vec![])
                } else {
                    return None;
                }
            }
            hir::HirExpression::Port(id) => {
                if self.flattened_ports.contains_key(id) {
                    (id.0, false, vec![])
                } else {
                    return None;
                }
            }
            hir::HirExpression::FieldAccess { base, field } => {
                // Extract field path and base signal/port
                let mut field_path = vec![field.clone()];
                let mut current = base.as_ref();
                loop {
                    match current {
                        hir::HirExpression::FieldAccess {
                            base: inner_base,
                            field: inner_field,
                        } => {
                            field_path.insert(0, inner_field.clone());
                            current = inner_base.as_ref();
                        }
                        hir::HirExpression::Signal(id) => {
                            if self.flattened_signals.contains_key(id) {
                                break (id.0, true, field_path);
                            } else {
                                return None;
                            }
                        }
                        hir::HirExpression::Port(id) => {
                            if self.flattened_ports.contains_key(id) {
                                break (id.0, false, field_path);
                            } else {
                                return None;
                            }
                        }
                        _ => return None,
                    }
                }
            }
            _ => return None,
        };

        // Get flattened fields for LHS and RHS
        let lhs_fields = if lhs_is_signal {
            self.flattened_signals.get(&hir::SignalId(lhs_hir_id))?
        } else {
            self.flattened_ports.get(&hir::PortId(lhs_hir_id))?
        };

        let rhs_fields_all = if rhs_is_signal {
            self.flattened_signals.get(&hir::SignalId(rhs_hir_id))?
        } else {
            self.flattened_ports.get(&hir::PortId(rhs_hir_id))?
        };

        // If RHS has a field path, filter to only those fields
        let rhs_fields: Vec<_> = if rhs_field_path.is_empty() {
            rhs_fields_all.clone()
        } else {
            // Find fields that start with the field path
            rhs_fields_all
                .iter()
                .filter(|f| f.field_path.starts_with(&rhs_field_path))
                .cloned()
                .collect()
        };

        // Check if field counts match
        if lhs_fields.len() != rhs_fields.len() {
            return None;
        }

        // Generate continuous assignments for each field
        let mut assignments = Vec::new();
        for (lhs_field, rhs_field) in lhs_fields.iter().zip(rhs_fields.iter()) {
            let lhs_lval = if lhs_is_signal {
                LValue::Signal(SignalId(lhs_field.id))
            } else {
                LValue::Port(PortId(lhs_field.id))
            };

            let rhs_expr = if rhs_is_signal {
                Expression::with_unknown_type(ExpressionKind::Ref(LValue::Signal(SignalId(
                    rhs_field.id,
                ))))
            } else {
                Expression::with_unknown_type(ExpressionKind::Ref(LValue::Port(PortId(
                    rhs_field.id,
                ))))
            };

            assignments.push(ContinuousAssign {
                lhs: lhs_lval,
                rhs: rhs_expr,
                span: None,
            });
        }

        // BUGFIX: If assignments is empty, this isn't actually a struct expansion - return None
        // to allow fallback to normal single assignment conversion
        if assignments.is_empty() {
            return None;
        }

        Some(assignments)
    }

    /// Convert module instance
    fn convert_instance(&mut self, instance: &hir::HirInstance) -> Option<ModuleInstance> {
        // Map entity ID to module ID
        let module_id = *self.entity_map.get(&instance.entity)?;

        // DEBUG: Check entity_map lookup for FpSub's adder
        if instance.name == "adder" {
            println!(
                "[CONVERT_INSTANCE] adder: instance.entity={:?} -> module_id={:?}",
                instance.entity, module_id
            );
        }

        // Get the entity definition - needed for both connections and parameters
        let entity = self
            .hir
            .as_ref()?
            .entities
            .iter()
            .find(|e| e.id == instance.entity)?
            .clone();

        // DEBUG: Check entity lookup for FpSub's adder
        if instance.name == "adder" {
            println!(
                "[CONVERT_INSTANCE] adder: found entity '{}' with id={:?}",
                entity.name, entity.id
            );
        }

        // Convert generic arguments to parameters
        // Map entity.generics (names) with instance.generic_args (values)
        let mut parameters = IndexMap::new();
        for (generic, arg) in entity.generics.iter().zip(&instance.generic_args) {
            // Only handle const/width parameters - type parameters are handled by monomorphization
            match &generic.param_type {
                hir::HirGenericType::Const(_) | hir::HirGenericType::Width => {
                    // Evaluate the const expression to get the parameter value
                    if let Some(value) = self.try_eval_const_expr(arg) {
                        parameters.insert(generic.name.clone(), Value::Integer(value as i64));
                    }
                }
                _ => {
                    // Type, TypeWithBounds, ClockDomain, Intent - not stored as parameters
                }
            }
        }

        // Convert connections
        // CRITICAL FIX for Bug #10: Expand struct/array connections into flattened field connections
        let mut connections = IndexMap::new();

        for conn in &instance.connections {
            // Check if this port is flattened (struct or array type)
            // If so, we need to create multiple connections for each flattened field

            // Find the port in the entity
            let port_opt = entity.ports.iter().find(|p| p.name == conn.port);

            // Check if the RHS expression is a reference to a flattened signal/port
            let expanded_connections = if let Some(port) = port_opt {
                // Check if port type is a struct or array that gets flattened
                let needs_expansion = matches!(
                    port.port_type,
                    hir::HirType::Struct(_) | hir::HirType::Array(_, _)
                );

                if needs_expansion {
                    // Try to expand the connection
                    self.expand_instance_connection(&conn.port, &conn.expr)?
                } else {
                    // Simple connection
                    vec![(conn.port.clone(), self.convert_expression(&conn.expr, 0)?)]
                }
            } else {
                // Port not found in entity, use default conversion
                vec![(conn.port.clone(), self.convert_expression(&conn.expr, 0)?)]
            };

            for (port_name, expr) in expanded_connections {
                connections.insert(port_name, expr);
            }
        }

        // Convert safety config from HIR instance to MIR safety context
        let safety_context = instance.safety_config.as_ref().map(convert_safety_config);

        Some(ModuleInstance {
            name: instance.name.clone(),
            module: module_id,
            connections,
            parameters,
            span: None,
            safety_context,
        })
    }

    /// Expand a struct/array instance connection into multiple flattened connections
    /// Example: rd_data: input_fifo_rd_data becomes:
    ///   rd_data_x: input_fifo_rd_data_x
    ///   rd_data_y: input_fifo_rd_data_y
    ///   rd_data_z: input_fifo_rd_data_z
    fn expand_instance_connection(
        &self,
        base_port_name: &str,
        rhs_expr: &hir::HirExpression,
    ) -> Option<Vec<(String, Expression)>> {
        // Get the base signal/port from RHS
        let (base_hir_id, is_signal) = match rhs_expr {
            hir::HirExpression::Signal(id) => (id.0, true),
            hir::HirExpression::Port(id) => (id.0, false),
            _ => {
                return None; // Complex expression, can't expand
            }
        };

        // Get flattened fields for the RHS signal/port
        let rhs_fields = if is_signal {
            match self.flattened_signals.get(&hir::SignalId(base_hir_id)) {
                Some(fields) => fields.clone(),
                None => {
                    return None;
                }
            }
        } else {
            match self.flattened_ports.get(&hir::PortId(base_hir_id)) {
                Some(fields) => fields.clone(),
                None => {
                    return None;
                }
            }
        };

        // Create connections for each flattened field
        let mut connections = Vec::new();
        for rhs_field in &rhs_fields {
            // Build port name: base_port_name + field_path
            // For example: rd_data + ["x"] = rd_data_x
            let port_field_name = if rhs_field.field_path.is_empty() {
                base_port_name.to_string()
            } else {
                format!("{}_{}", base_port_name, rhs_field.field_path.join("_"))
            };

            // Create expression referencing the flattened RHS field
            let rhs_field_expr = if is_signal {
                Expression::with_unknown_type(ExpressionKind::Ref(LValue::Signal(SignalId(
                    rhs_field.id,
                ))))
            } else {
                Expression::with_unknown_type(ExpressionKind::Ref(LValue::Port(PortId(
                    rhs_field.id,
                ))))
            };

            connections.push((port_field_name, rhs_field_expr));
        }

        Some(connections)
    }

    /// Convert HIR if statement to MIR
    fn convert_if_statement(&mut self, if_stmt: &hir::HirIfStatement) -> Option<IfStatement> {
        let condition = self.convert_expression(&if_stmt.condition, 0)?;
        let then_block = self.convert_statements(&if_stmt.then_statements);
        let else_block = if_stmt
            .else_statements
            .as_ref()
            .map(|stmts| self.convert_statements(stmts));

        Some(IfStatement {
            condition,
            then_block,
            else_block,
            span: None,
        })
    }

    /// Convert HIR for statement to MIR loop statement
    fn convert_for_statement(&mut self, for_stmt: &hir::HirForStatement) -> Option<Statement> {
        println!(
            "[convert_for_statement] Converting for loop with iterator '{}'",
            for_stmt.iterator
        );

        // Convert range bounds
        let start_expr = self.convert_expression(&for_stmt.range.start, 0)?;
        let end_expr = self.convert_expression(&for_stmt.range.end, 0)?;

        // Create MIR variable for the iterator and map it from HIR
        let mir_iter_var_id = self.next_variable_id();
        self.variable_map
            .insert(for_stmt.iterator_var_id, mir_iter_var_id);

        // Create iterator variable reference using MIR variable ID
        let iterator_lvalue = LValue::Variable(mir_iter_var_id);

        // Init: iterator = start
        let init = Assignment {
            lhs: iterator_lvalue.clone(),
            rhs: start_expr,
            kind: AssignmentKind::Blocking,
            span: None,
        };

        // Condition: iterator < end (or <= for inclusive)
        let condition_op = if for_stmt.range.inclusive {
            BinaryOp::LessEqual
        } else {
            BinaryOp::Less
        };
        let condition = Expression {
            kind: ExpressionKind::Binary {
                op: condition_op,
                left: Box::new(Expression {
                    kind: ExpressionKind::Ref(iterator_lvalue.clone()),
                    ty: Type::Unknown,
                    span: None,
                }),
                right: Box::new(end_expr),
            },
            ty: Type::Bool,
            span: None,
        };

        // Update: iterator = iterator + 1
        let one = Expression {
            kind: ExpressionKind::Literal(Value::Integer(1)),
            ty: Type::Unknown,
            span: None,
        };
        let update = Assignment {
            lhs: iterator_lvalue.clone(),
            rhs: Expression {
                kind: ExpressionKind::Binary {
                    op: BinaryOp::Add,
                    left: Box::new(Expression {
                        kind: ExpressionKind::Ref(iterator_lvalue),
                        ty: Type::Unknown,
                        span: None,
                    }),
                    right: Box::new(one),
                },
                ty: Type::Unknown,
                span: None,
            },
            kind: AssignmentKind::Blocking,
            span: None,
        };

        // Convert body statements
        let body = self.convert_statements(&for_stmt.body);

        Some(Statement::Loop(LoopStatement::For {
            init: Box::new(init),
            condition,
            update: Box::new(update),
            body,
        }))
    }

    /// Unroll a for loop at compile time
    fn unroll_for_loop(
        &mut self,
        for_stmt: &hir::HirForStatement,
        config: &hir::UnrollConfig,
    ) -> Option<Statement> {
        println!(
            "[unroll_for_loop] Unrolling for loop with config: {:?}",
            config
        );

        // Evaluate range bounds at compile time
        let start = self.eval_const_hir_expression(&for_stmt.range.start)?;
        let end = self.eval_const_hir_expression(&for_stmt.range.end)?;

        let range_end = if for_stmt.range.inclusive {
            end + 1
        } else {
            end
        };

        match config {
            hir::UnrollConfig::Full => {
                // Fully unroll - generate statements for each iteration
                let mut all_statements = Vec::new();

                for i in start..range_end {
                    // Substitute iterator variable with constant i in body
                    for body_stmt in &for_stmt.body {
                        if let Some(mir_stmt) = self.convert_statement_with_substitution(
                            body_stmt,
                            &for_stmt.iterator,
                            for_stmt.iterator_var_id,
                            i,
                        ) {
                            all_statements.push(mir_stmt);
                        }
                    }
                }

                if all_statements.is_empty() {
                    None
                } else {
                    Some(Statement::Block(Block {
                        statements: all_statements,
                    }))
                }
            }
            hir::UnrollConfig::Factor(factor) => {
                // Partial unroll - unroll by factor, keep outer loop for remainder
                // For now, fall back to full unroll if factor divides evenly
                let total_iterations = (range_end - start) as u32;
                if total_iterations <= *factor {
                    // Small enough to fully unroll
                    self.unroll_for_loop(for_stmt, &hir::UnrollConfig::Full)
                } else {
                    // TODO: Implement proper partial unrolling with outer loop
                    // For now, just convert to regular loop
                    debug_println!("[unroll_for_loop] Partial unroll not fully implemented, falling back to loop");
                    self.convert_for_statement(for_stmt)
                }
            }
        }
    }

    /// Evaluate a HIR expression to a constant i64 at compile time
    #[allow(clippy::only_used_in_recursion)]
    fn eval_const_hir_expression(&self, expr: &hir::HirExpression) -> Option<i64> {
        match expr {
            hir::HirExpression::Literal(hir::HirLiteral::Integer(n)) => Some(*n as i64),
            hir::HirExpression::Cast(cast_expr) => self.eval_const_hir_expression(&cast_expr.expr),
            hir::HirExpression::Binary(binary_expr) => {
                let l = self.eval_const_hir_expression(&binary_expr.left)?;
                let r = self.eval_const_hir_expression(&binary_expr.right)?;
                Some(match binary_expr.op {
                    hir::HirBinaryOp::Add => l + r,
                    hir::HirBinaryOp::Sub => l - r,
                    hir::HirBinaryOp::Mul => l * r,
                    hir::HirBinaryOp::Div => l / r,
                    _ => return None,
                })
            }
            _ => None,
        }
    }

    /// Convert a statement with iterator substitution for loop unrolling
    fn convert_statement_with_substitution(
        &mut self,
        stmt: &hir::HirStatement,
        iterator_name: &str,
        iterator_var_id: hir::VariableId,
        value: i64,
    ) -> Option<Statement> {
        // For unrolling, we substitute references to the iterator variable
        // with the constant value for this iteration
        // This is a simplified implementation - a full implementation would
        // recursively substitute in all sub-expressions

        // Store the substitution context
        self.unroll_substitution = Some((iterator_var_id, value));
        let result = self.convert_statement(stmt);
        self.unroll_substitution = None;
        result
    }

    /// Convert HIR match statement to MIR case statement
    fn convert_match_statement(
        &mut self,
        match_stmt: &hir::HirMatchStatement,
    ) -> Option<CaseStatement> {
        let expr = self.convert_expression(&match_stmt.expr, 0)?;

        let mut items = Vec::new();
        let mut default = None;

        for arm in &match_stmt.arms {
            let block = self.convert_statements(&arm.statements);
            let final_block = if let Some(guard_expr) = &arm.guard {
                // If there's a guard, wrap the statements in an if statement
                let guard_condition = self.convert_expression(guard_expr, 0)?;
                let if_stmt = Statement::If(IfStatement {
                    condition: guard_condition,
                    then_block: block,
                    else_block: None,
                    span: None,
                });
                Block {
                    statements: vec![if_stmt],
                }
            } else {
                block
            };

            // Convert pattern to expression for case matching
            match self.convert_pattern_to_expr(&arm.pattern) {
                Some(expr_val) => {
                    items.push(CaseItem {
                        values: vec![expr_val],
                        block: final_block,
                    });
                }
                None => {
                    // Wildcard pattern becomes default case
                    default = Some(final_block);
                }
            }
        }

        Some(CaseStatement {
            expr,
            items,
            default,
            span: None,
        })
    }

    /// Convert pattern to expression (for case values)
    fn convert_pattern_to_expr(&mut self, pattern: &hir::HirPattern) -> Option<Expression> {
        match pattern {
            hir::HirPattern::Literal(lit) => {
                // Convert literal pattern to expression
                match lit {
                    hir::HirLiteral::Integer(val) => Some(Expression::with_unknown_type(
                        ExpressionKind::Literal(Value::Integer(*val as i64)),
                    )),
                    hir::HirLiteral::Float(f) => Some(Expression::with_unknown_type(
                        ExpressionKind::Literal(Value::Float(*f)),
                    )),
                    hir::HirLiteral::String(s) => Some(Expression::with_unknown_type(
                        ExpressionKind::Literal(Value::String(s.clone())),
                    )),
                    hir::HirLiteral::Boolean(b) => {
                        // Convert boolean to integer (true = 1, false = 0)
                        Some(Expression::with_unknown_type(ExpressionKind::Literal(
                            Value::Integer(if *b { 1 } else { 0 }),
                        )))
                    }
                    hir::HirLiteral::BitVector(bits) => {
                        // Convert bit vector to integer value
                        let width = bits.len();
                        let mut value = 0u64;
                        for (i, bit) in bits.iter().enumerate() {
                            if *bit {
                                value |= 1 << i;
                            }
                        }
                        Some(Expression::with_unknown_type(ExpressionKind::Literal(
                            Value::BitVector { width, value },
                        )))
                    }
                }
            }
            hir::HirPattern::Path(enum_name, variant_name) => {
                // For enum patterns, we need to convert to the enum variant value
                // TODO: Implement proper enum variant value lookup from type information
                // For now, we'll use a simple mapping based on common variant names
                let variant_value = match variant_name.as_str() {
                    "Idle" => 0,
                    "Active" => 1,
                    "Done" => 2,
                    _ => {
                        // Calculate a simple hash for unknown variants to ensure consistent values
                        variant_name.chars().map(|c| c as u32).sum::<u32>() % 16
                    }
                };
                Some(Expression::with_unknown_type(ExpressionKind::Literal(
                    Value::Integer(variant_value as i64),
                )))
            }
            hir::HirPattern::Variable(_) => {
                // Variable patterns can't be converted to case values directly
                // They should be handled differently (as binding patterns)
                None
            }
            hir::HirPattern::Wildcard => {
                // Wildcard patterns become the default case
                None
            }
            hir::HirPattern::Tuple(_patterns) => {
                // Tuple patterns are complex - for now, treat as wildcard
                // TODO: Implement proper tuple pattern handling
                None
            }
        }
    }

    /// Convert HIR lvalue to MIR
    fn convert_lvalue(&mut self, lval: &hir::HirLValue) -> Option<LValue> {
        let result = match lval {
            hir::HirLValue::Signal(id) => self.signal_map.get(id).map(|&id| LValue::Signal(id)),
            hir::HirLValue::Variable(id) => {
                // BUG #153 FIX: Check context_variable_map first (for match arm/function inlining contexts)
                // Then check variable_map and dynamic_variables as fallbacks
                // This matches the lookup order in convert_expression for Variable
                let mir_id = if let Some(context) = self.get_current_context() {
                    let context_key = (Some(context.clone()), *id);
                    if let Some(&mir_id) = self.context_variable_map.get(&context_key) {
                        Some(mir_id)
                    } else {
                        None
                    }
                } else {
                    None
                };

                // Fallback to variable_map and dynamic_variables
                let mir_id = mir_id
                    .or_else(|| self.variable_map.get(id).copied())
                    .or_else(|| self.dynamic_variables.get(id).map(|(mir_id, _, _)| *mir_id));

                mir_id.map(LValue::Variable)
            }
            hir::HirLValue::Port(id) => self.port_map.get(id).map(|&id| LValue::Port(id)),
            hir::HirLValue::Index(base, index) => {
                let base = Box::new(self.convert_lvalue(base)?);
                let index = Box::new(self.convert_expression(index, 0)?);
                Some(LValue::BitSelect { base, index })
            }
            hir::HirLValue::Range(base, high, low) => {
                let base = Box::new(self.convert_lvalue(base)?);
                let high = Box::new(self.convert_expression(high, 0)?);
                let low = Box::new(self.convert_expression(low, 0)?);
                Some(LValue::RangeSelect { base, high, low })
            }
            hir::HirLValue::FieldAccess { base, field } => {
                // Check if this is a field access on a flattened signal/port
                // Build the complete field path from nested accesses
                let mut field_path = vec![field.clone()];
                let mut current_base = base.as_ref();

                // Walk up the chain to find the root signal/port and complete field path
                loop {
                    match current_base {
                        hir::HirLValue::FieldAccess {
                            base: inner_base,
                            field: inner_field,
                        } => {
                            field_path.insert(0, inner_field.clone());
                            current_base = inner_base.as_ref();
                        }
                        hir::HirLValue::Signal(sig_id) => {
                            // Check if this signal was flattened
                            if let Some(flattened) = self.flattened_signals.get(sig_id) {
                                // Find the flattened field with matching path
                                for flat_field in flattened {
                                    if flat_field.field_path == field_path {
                                        return Some(LValue::Signal(SignalId(flat_field.id)));
                                    }
                                }
                            }
                            // Not flattened or field not found - fall back to old behavior
                            break;
                        }
                        hir::HirLValue::Port(port_id) => {
                            // Check if this port was flattened
                            if let Some(flattened) = self.flattened_ports.get(port_id) {
                                // Find the flattened field with matching path
                                for flat_field in flattened {
                                    if flat_field.field_path == field_path {
                                        return Some(LValue::Port(PortId(flat_field.id)));
                                    }
                                }
                            }
                            // Not flattened or field not found - fall back to old behavior
                            break;
                        }
                        _ => {
                            // Complex base - fall back to old behavior
                            break;
                        }
                    }
                }

                // Fallback: convert struct field access to bit range (for non-flattened types)
                let base_mir = self.convert_lvalue(base)?;
                let base_expr = self.lvalue_to_expression(base)?;
                let (high_bit, low_bit) = self.get_field_bit_range(&base_expr, field)?;

                let high_expr = Expression::with_unknown_type(ExpressionKind::Literal(
                    Value::Integer(high_bit as i64),
                ));
                let low_expr = Expression::with_unknown_type(ExpressionKind::Literal(
                    Value::Integer(low_bit as i64),
                ));

                Some(LValue::RangeSelect {
                    base: Box::new(base_mir),
                    high: Box::new(high_expr),
                    low: Box::new(low_expr),
                })
            }
        };
        result
    }

    /// Convert LValue to HirExpression for type inference
    fn lvalue_to_expression(&self, lval: &hir::HirLValue) -> Option<hir::HirExpression> {
        match lval {
            hir::HirLValue::Signal(id) => Some(hir::HirExpression::Signal(*id)),
            hir::HirLValue::Variable(id) => Some(hir::HirExpression::Variable(*id)),
            hir::HirLValue::Port(id) => Some(hir::HirExpression::Port(*id)),
            _ => None, // For complex LValues, we can't easily convert back
        }
    }

    /// Convert HIR property to MIR expression (for cover statements)
    /// Only simple Expression properties are currently supported
    fn convert_property_to_expression(
        &mut self,
        property: &hir::HirProperty,
    ) -> Option<Expression> {
        match property {
            hir::HirProperty::Expression(expr) => self.convert_expression(expr, 0),
            hir::HirProperty::Sequence(_) => {
                debug_println!("[WARN] Sequence properties not yet supported in SVA generation");
                None
            }
            hir::HirProperty::Implication { .. } => {
                debug_println!("[WARN] Implication properties not yet supported in SVA generation");
                None
            }
            hir::HirProperty::OverlappingImplication { .. } => {
                eprintln!(
                    "[WARN] Overlapping implication properties not yet supported in SVA generation"
                );
                None
            }
            hir::HirProperty::And(_, _) => {
                debug_println!("[WARN] And property types not yet supported in SVA generation");
                None
            }
            hir::HirProperty::Or(_, _) => {
                debug_println!("[WARN] Or property types not yet supported in SVA generation");
                None
            }
            hir::HirProperty::Not(_) => {
                debug_println!("[WARN] Not property types not yet supported in SVA generation");
                None
            }
            hir::HirProperty::Always(_) => {
                debug_println!("[WARN] Always property types not yet supported in SVA generation");
                None
            }
            hir::HirProperty::Eventually(_) => {
                debug_println!(
                    "[WARN] Eventually property types not yet supported in SVA generation"
                );
                None
            }
            hir::HirProperty::Until { .. } => {
                debug_println!("[WARN] Until property types not yet supported in SVA generation");
                None
            }
            hir::HirProperty::Throughout { .. } => {
                debug_println!(
                    "[WARN] Throughout property types not yet supported in SVA generation"
                );
                None
            }
            hir::HirProperty::Clocked { property, .. } => {
                // For clocked properties, extract the inner property
                self.convert_property_to_expression(property)
            }
        }
    }

    /// Convert HIR expression to MIR
    fn convert_expression(
        &mut self,
        expr: &hir::HirExpression,
        depth: usize,
    ) -> Option<Expression> {
        // TEMP DEBUG: Always log what expressions are being converted
        if depth == 0 {
            println!(
                "[CONVERT_EXPR_DEBUG] depth=0, expr_type={:?}",
                std::mem::discriminant(expr)
            );
        }
        // BUG #187 DEBUG: Trace what convert_expression receives
        if depth == 0 && matches!(expr, hir::HirExpression::TupleLiteral(_)) {
            println!("🟡🟡🟡 [BUG #187] convert_expression at depth 0 with TupleLiteral! stack_len={} 🟡🟡🟡",
                     self.inlining_return_type_stack.len());
        }
        // Guard against stack overflow on deeply nested expressions
        if depth > MAX_EXPRESSION_RECURSION_DEPTH {
            panic!(
                "[HIR_TO_MIR] Expression conversion recursion depth exceeded {} - likely infinite recursion in nested concat/tuple/call expressions",
                MAX_EXPRESSION_RECURSION_DEPTH
            );
        }

        // BUG #76 FIX: Infer type first for proper type propagation
        let ty = self.infer_hir_expression_type(expr, depth);

        // BUG #127 DEBUG: Print discriminant and check for Concat
        let is_concat = matches!(expr, hir::HirExpression::Concat(_));
        println!(
            "[BUG127_DEBUG] depth={}, is_concat={}, disc={:?}",
            depth,
            is_concat,
            std::mem::discriminant(expr)
        );
        use std::io::Write;
        let _ = std::io::stdout().flush();

        match expr {
            hir::HirExpression::Literal(lit) => self
                .convert_literal(lit)
                .map(|v| Expression::new(ExpressionKind::Literal(v), ty)),
            hir::HirExpression::Signal(id) => {
                // BUG #190 FIX: Check if this HIR signal is a placeholder connected to an entity output
                // This is critical for FP operations where `signal result: fp32;` is passed to an
                // entity like FpAdd and the result needs to come from the entity's output wire.
                // BUG #201 FIX: Use stored signal type instead of passed-in ty
                if let Some((output_signal_id, signal_type)) =
                    self.placeholder_hir_signal_to_entity_output.get(id)
                {
                    println!(
                        "[BUG #190 FIX] HIR Signal {:?} is placeholder -> returning entity output signal {:?} (type {:?})",
                        id, output_signal_id, signal_type
                    );
                    return Some(Expression::new(
                        ExpressionKind::Ref(LValue::Signal(*output_signal_id)),
                        signal_type.clone(),
                    ));
                }

                // First try signal_map
                if let Some(&signal_id) = self.signal_map.get(id) {
                    Some(Expression::new(
                        ExpressionKind::Ref(LValue::Signal(signal_id)),
                        ty,
                    ))
                } else {
                    // If not found in signals, check if it's a port masquerading as a signal
                    // (due to HIR builder converting ports to signals in expressions)
                    let hir_port_id = hir::PortId(id.0);
                    if let Some(&mir_port_id) = self.port_map.get(&hir_port_id) {
                        Some(Expression::new(
                            ExpressionKind::Ref(LValue::Port(mir_port_id)),
                            ty,
                        ))
                    } else {
                        None
                    }
                }
            }
            hir::HirExpression::Port(id) => {
                // Handle port references in expressions
                if let Some(&mir_port_id) = self.port_map.get(id) {
                    Some(Expression::new(
                        ExpressionKind::Ref(LValue::Port(mir_port_id)),
                        ty,
                    ))
                } else {
                    None
                }
            }
            hir::HirExpression::Variable(id) => {
                // BUG #167 FIX: Check if this variable is a placeholder connected to an entity output
                // If so, return a reference to the entity output wire instead
                // DEBUG: Always trace Variable(16) lookups
                if id.0 == 16 {
                    let contains = self.placeholder_signal_to_entity_output.contains_key(id);
                    let get_result = self.placeholder_signal_to_entity_output.get(id);
                    println!(
                        "[BUG #193 DEBUG] convert_expression Variable(16): contains={}, get={:?}, map_keys={:?}",
                        contains,
                        get_result,
                        self.placeholder_signal_to_entity_output.keys().map(|k| k.0).collect::<Vec<_>>()
                    );
                }
                // BUG #201 FIX: Use stored signal type instead of passed-in ty
                if let Some((output_signal_id, signal_type)) =
                    self.placeholder_signal_to_entity_output.get(id)
                {
                    println!(
                        "[BUG #167 FIX] Variable {:?} is placeholder -> returning entity output signal {:?} (type {:?})",
                        id, output_signal_id, signal_type
                    );
                    return Some(Expression::new(
                        ExpressionKind::Ref(LValue::Signal(*output_signal_id)),
                        signal_type.clone(),
                    ));
                }

                // BUG #200 FIX: Check if we're in module synthesis context with var_to_signal mapping
                // This is critical for entity struct literal fields where Variables need to resolve
                // to the synthesized module's internal signals, not remain as Variable references.
                if let Some(ref var_to_signal) = self.current_module_var_to_signal {
                    if let Some(&signal_id) = var_to_signal.get(id) {
                        println!(
                            "[BUG #200 FIX] Variable {:?} found in current_module_var_to_signal -> Signal {:?}",
                            id, signal_id
                        );
                        return Some(Expression::new(
                            ExpressionKind::Ref(LValue::Signal(signal_id)),
                            ty,
                        ));
                    }
                }

                // CRITICAL FIX #IMPORT_MATCH: Use context-aware lookup for ALL variable resolutions
                //
                // ROOT CAUSE: When inlining nested functions (e.g., test_func calls my_fp_add),
                // different functions can have local variables with the same HIR VariableId.
                // For example:
                //   - test_func has local var 'result' with VariableId(0)
                //   - my_fp_add has local var 'a_fp' also with VariableId(0)
                //
                // When both are inlined into the same entity (via match arm), a simple
                // VariableId lookup in dynamic_variables or variable_map will find the WRONG
                // variable because the HashMap can only store ONE entry per VariableId.
                //
                // SOLUTION: Always use context-aware lookup when in a match arm context.
                // The context_variable_map uses (match_arm_prefix, VariableId) as the key,
                // so variables from different contexts can coexist without collision.
                //
                // LOOKUP ORDER:
                // 1. context_variable_map (if in ANY context - match arm OR function inlining) - most specific
                // 2. variable_map - for variables declared at entity level
                // 3. dynamic_variables - fallback for other cases

                let mir_id = if let Some(context) = self.get_current_context() {
                    let context_key = (Some(context.clone()), *id);
                    if let Some(&mir_id) = self.context_variable_map.get(&context_key) {
                        eprintln!(
                            "[DEBUG] Variable lookup: Found context-aware mapping for {:?} in '{}' -> MIR {:?}",
                            id, context, mir_id
                        );
                        Some(mir_id)
                    } else {
                        // Not in context map, try other lookups
                        None
                    }
                } else {
                    None
                };

                // If not found in context map, try variable_map and dynamic_variables
                let mir_id = mir_id
                    .or_else(|| self.variable_map.get(id).copied())
                    .or_else(|| {
                        self.dynamic_variables.get(id).map(|(mir_id, name, _)| {
                            eprintln!(
                                "[DEBUG] Variable lookup: Found '{}' (HIR {:?}) in dynamic_variables -> MIR {:?}",
                                name, id, mir_id
                            );
                            *mir_id
                        })
                    });

                if let Some(mir_id) = mir_id {
                    Some(Expression::new(
                        ExpressionKind::Ref(LValue::Variable(mir_id)),
                        ty,
                    ))
                } else {
                    // BUG #181 FIX: Check if this is a parameter Variable that should be
                    // resolved via pending_param_var_to_name + pending_mir_param_subs.
                    // This happens when inlining functions where parameters are referenced
                    // as Variable(param_id) instead of GenericParam("param_name").
                    if let Some(param_name) = self.pending_param_var_to_name.get(id) {
                        if let Some(mir_expr) = self.pending_mir_param_subs.get(param_name) {
                            eprintln!(
                                "[BUG #181 FIX] Variable({:?}) resolved via pending_param_var_to_name['{}'] -> pending_mir_param_subs",
                                id, param_name
                            );
                            return Some(mir_expr.clone());
                        }
                    }

                    eprintln!(
                        "[DEBUG] Variable not found in any lookup map: HIR ID {:?}",
                        id
                    );
                    eprintln!(
                        "[DEBUG]   Current match_arm_prefix: {:?}",
                        self.match_arm_prefix
                    );
                    eprintln!(
                        "[DEBUG]   Current dynamic_variables: {:?}",
                        self.dynamic_variables
                            .iter()
                            .map(|(hir_id, (mir_id, name, _))| format!(
                                "HIR {:?} -> MIR {:?} ({})",
                                hir_id, mir_id, name
                            ))
                            .collect::<Vec<_>>()
                    );
                    eprintln!(
                        "[DEBUG]   context_variable_map keys: {:?}",
                        self.context_variable_map.keys().collect::<Vec<_>>()
                    );
                    None
                }
            }
            hir::HirExpression::Constant(id) => {
                // Look up and evaluate the constant value in HIR
                if let Some(hir) = self.hir {
                    // Create evaluator and register all constants from all implementations
                    let mut evaluator = ConstEvaluator::new();
                    for implementation in &hir.implementations {
                        evaluator.register_constants(&implementation.constants);
                    }

                    // BUG #127 FIX: Search for constants with entity filtering
                    // Prioritize the current entity's implementation, then fall back to others.
                    // This is needed because monomorphized entities have specialized constant
                    // values (e.g., W = 32) while the original generic entity has unresolved
                    // expressions (e.g., W = F.total_bits).

                    // First pass: look in the current entity's implementation
                    // BUG #127 FIX: Only return on success. On eval failure, fall through to
                    // second pass which may find specialized Literal constants from monomorphized
                    // entities.
                    for implementation in &hir.implementations {
                        if Some(implementation.entity) != self.current_entity_id {
                            continue;
                        }
                        for constant in &implementation.constants {
                            if constant.id == *id {
                                match evaluator.eval(&constant.value) {
                                    Ok(const_value) => {
                                        return Some(
                                            self.const_value_to_mir_expression(&const_value),
                                        );
                                    }
                                    Err(_) => {
                                        // BUG #127 FIX: Don't return here! Fall through to second
                                        // pass which may find specialized Literal constants.
                                        break;
                                    }
                                }
                            }
                        }
                    }

                    // Second pass: fall back to any implementation with matching constant
                    // BUG #127 FIX: Collect all matching constants and prefer Literal values
                    // (specialized/evaluated) over non-Literal values (unspecialized expressions
                    // like FieldAccess or Binary). This ensures we use monomorphized constant
                    // values rather than the original generic expressions.
                    let mut matching_constants: Vec<(&hir::HirConstant, hir::EntityId)> =
                        Vec::new();
                    for implementation in &hir.implementations {
                        for constant in &implementation.constants {
                            if constant.id == *id {
                                matching_constants.push((constant, implementation.entity));
                            }
                        }
                    }

                    // Sort: Literals first (specialized), then others (unspecialized)
                    matching_constants
                        .sort_by_key(|(c, _)| !matches!(c.value, hir::HirExpression::Literal(_)));

                    for (constant, _entity_id) in &matching_constants {
                        match evaluator.eval(&constant.value) {
                            Ok(const_value) => {
                                return Some(self.const_value_to_mir_expression(&const_value));
                            }
                            Err(_) => {
                                // Try next constant with the same ID instead of immediately
                                // falling back to recursive conversion
                                continue;
                            }
                        }
                    }

                    // If no constant evaluated successfully, try recursive conversion on the first one
                    if let Some((constant, _)) = matching_constants.first() {
                        return self.convert_expression(&constant.value, depth + 1);
                    }

                    // BUG FIX: Constant ID not found after module merging
                    // Collect available constant IDs for debugging
                    let available_ids: Vec<_> = hir
                        .implementations
                        .iter()
                        .flat_map(|i| i.constants.iter().map(|c| (c.id, &c.name)))
                        .collect();
                    println!(
                        "[CONST_DEBUG] Constant ID {:?} not found! Available: {:?}, current_entity={:?}",
                        id, available_ids, self.current_entity_id
                    );
                    return None;
                }
                None
            }
            hir::HirExpression::GenericParam(param_name) => {
                // BUG #178 FIX: Check pending MIR parameter substitutions first
                // This handles the case where we're inlining a function and have pre-converted
                // MIR expressions for the arguments that need to be substituted.
                if let Some(mir_expr) = self.pending_mir_param_subs.get(param_name) {
                    return Some(mir_expr.clone());
                }

                // BUG #205 FIX: Check current_module_param_to_port for module synthesis context
                // When processing entity struct literal fields during module synthesis,
                // GenericParam references need to resolve to the module's input ports.
                // This is critical for entity connections like `FpMul { a: b, b: b }` where
                // 'b' is a function parameter that should map to an input port.
                if let Some(ref param_to_port) = self.current_module_param_to_port {
                    if let Some(&port_id) = param_to_port.get(param_name) {
                        println!(
                            "[BUG #205 FIX] GenericParam '{}' resolved to Port({}) via current_module_param_to_port",
                            param_name, port_id.0
                        );
                        return Some(Expression::with_unknown_type(ExpressionKind::Ref(
                            LValue::Port(port_id),
                        )));
                    }
                }

                // BUG FIX #7: WORKAROUND for Bug #42/43 - Cast expressions create malformed AST
                // causing variable references to appear as GenericParam instead of Variable.
                // First check if this is actually a variable from a let-binding (e.g., from inlined function)
                if let Some((mir_var_id, _, _)) = self
                    .dynamic_variables
                    .values()
                    .find(|(_, var_name, _)| var_name == param_name)
                {
                    return Some(Expression::new(
                        ExpressionKind::Ref(LValue::Variable(*mir_var_id)),
                        ty,
                    ));
                }

                // Look up the generic parameter value in the current context
                // Generic parameters are treated as constants after monomorphization
                if let Some(hir) = self.hir {
                    // First, try to find it in the current entity's generic parameters
                    if let Some(entity_id) = self.current_entity_id {
                        for entity in &hir.entities {
                            if entity.id == entity_id {
                                for generic in &entity.generics {
                                    if generic.name == *param_name {
                                        // Found the generic parameter, use its default value
                                        if let Some(default_expr) = &generic.default_value {
                                            // Recursively convert the default value expression
                                            return self
                                                .convert_expression(default_expr, depth + 1);
                                        }
                                    }
                                }
                                break;
                            }
                        }
                    }

                    // If not found as generic param, try constants in implementation
                    for implementation in &hir.implementations {
                        for constant in &implementation.constants {
                            if constant.name == *param_name {
                                return self.convert_expression(&constant.value, depth + 1);
                            }
                        }
                    }
                }
                // If not found, return a literal 0 as fallback
                // This will be properly resolved during monomorphization/type checking
                Some(Expression::new(
                    ExpressionKind::Literal(Value::Integer(0)),
                    ty,
                ))
            }
            hir::HirExpression::Binary(binary) => {
                println!(
                    "[MIR_BINARY_DEBUG] Converting Binary expression: op={:?}, depth={}",
                    binary.op, depth
                );
                // DEBUG: Show left operand type for FP trait resolution
                let left_type = self.infer_hir_type(&binary.left);
                eprintln!(
                    "[MIR_BINARY_DEBUG] op={:?}, left_type={:?}, left={:?}",
                    binary.op,
                    left_type,
                    std::mem::discriminant(&*binary.left)
                );
                if matches!(
                    binary.op,
                    hir::HirBinaryOp::LogicalAnd | hir::HirBinaryOp::LogicalOr
                ) {
                    eprintln!(
                        "[MIR_LOGICAL_DEBUG] Converting {:?} expression at depth {}",
                        binary.op, depth
                    );
                    debug_println!("[MIR_LOGICAL_DEBUG]   left: {:?}", binary.left);
                    debug_println!("[MIR_LOGICAL_DEBUG]   right: {:?}", binary.right);
                }

                // TRAIT-BASED OPERATOR RESOLUTION (Operator Overloading)
                // Try to resolve the operator through trait implementations first.
                // If a trait impl exists (e.g., `impl Add for myint`), inline the trait method
                // body instead of using a primitive binary operation.
                if let Some((type_name, trait_name, method_name)) =
                    self.try_resolve_trait_operator(&binary.op, &binary.left)
                {
                    println!(
                        "[TRAIT_OP] Attempting to inline {}::{} for type '{}' (op: {:?})",
                        trait_name, method_name, type_name, binary.op
                    );

                    // BUG #200 DEBUG: Print what the operands look like BEFORE inlining
                    println!(
                        "[BUG #200 TRAIT_OP] left_expr: {:?}",
                        std::mem::discriminant(&*binary.left)
                    );
                    println!(
                        "[BUG #200 TRAIT_OP] right_expr: {:?}",
                        std::mem::discriminant(&*binary.right)
                    );
                    // Show more details for specific expression types
                    if let hir::HirExpression::Variable(var_id) = &*binary.left {
                        println!("[BUG #200 TRAIT_OP]   left is Variable({:?})", var_id);
                    }
                    if let hir::HirExpression::Cast(c) = &*binary.left {
                        println!(
                            "[BUG #200 TRAIT_OP]   left is Cast({:?} -> {:?})",
                            std::mem::discriminant(&*c.expr),
                            c.target_type
                        );
                    }
                    if let hir::HirExpression::Variable(var_id) = &*binary.right {
                        println!("[BUG #200 TRAIT_OP]   right is Variable({:?})", var_id);
                    }
                    if let hir::HirExpression::Cast(c) = &*binary.right {
                        println!(
                            "[BUG #200 TRAIT_OP]   right is Cast({:?} -> {:?})",
                            std::mem::discriminant(&*c.expr),
                            c.target_type
                        );
                    }

                    // Try to inline the trait method body
                    if let Some(result) = self.inline_trait_method(
                        &type_name,
                        trait_name,
                        method_name,
                        &binary.left,
                        &binary.right,
                    ) {
                        println!(
                            "[TRAIT_OP] Successfully inlined {}::{} for type '{}'",
                            trait_name, method_name, type_name
                        );
                        return Some(result);
                    }

                    // If inlining fails, fall back to FunctionCall (for debugging)
                    println!(
                        "[TRAIT_OP] Failed to inline {}::{}, falling back to FunctionCall",
                        trait_name, method_name
                    );
                    let func_name = format!("{}::{}", trait_name, method_name);
                    let left = self.convert_expression(&binary.left, depth + 1)?;
                    let right = self.convert_expression(&binary.right, depth + 1)?;

                    return Some(Expression::new(
                        ExpressionKind::FunctionCall {
                            name: func_name,
                            args: vec![left, right],
                        },
                        ty,
                    ));
                }

                // Fall back to primitive binary operation
                let left = match self.convert_expression(&binary.left, depth + 1) {
                    Some(e) => e,
                    None => {
                        println!(
                            "[BUG127_BINARY] LEFT operand conversion FAILED: op={:?}, left type={:?}",
                            binary.op,
                            std::mem::discriminant(&*binary.left)
                        );
                        if let hir::HirExpression::Constant(id) = &*binary.left {
                            println!("[BUG127_BINARY]   Constant id={:?}", id);
                        }
                        return None;
                    }
                };
                let right = match self.convert_expression(&binary.right, depth + 1) {
                    Some(e) => e,
                    None => {
                        println!(
                            "[BUG127_BINARY] RIGHT operand conversion FAILED: op={:?}, right type={:?}",
                            binary.op,
                            std::mem::discriminant(&*binary.right)
                        );
                        return None;
                    }
                };
                if matches!(
                    binary.op,
                    hir::HirBinaryOp::LogicalAnd | hir::HirBinaryOp::LogicalOr
                ) {
                    debug_println!("[MIR_LOGICAL_DEBUG] After conversion:");
                    debug_println!("[MIR_LOGICAL_DEBUG]   left MIR: {:?}", left.kind);
                    debug_println!("[MIR_LOGICAL_DEBUG]   right MIR: {:?}", right.kind);
                }
                let left = Box::new(left);
                let right = Box::new(right);
                let op = self.convert_binary_op(&binary.op, &binary.left);
                Some(Expression::new(
                    ExpressionKind::Binary { op, left, right },
                    ty,
                ))
            }
            hir::HirExpression::Unary(unary) => {
                let operand = Box::new(self.convert_expression(&unary.operand, depth + 1)?);
                let op = self.convert_unary_op(&unary.op);
                Some(Expression::new(ExpressionKind::Unary { op, operand }, ty))
            }
            hir::HirExpression::Call(call) => {
                println!(
                    "🔥🔥🔥 CALL ARM MATCHED: function='{}' 🔥🔥🔥",
                    call.function
                );
                eprintln!(
                    "[MIR_CALL] Converting Call expression: function='{}', args={}",
                    call.function,
                    call.args.len()
                );

                // NOTE: FP operations are handled via stdlib trait implementations (FpAdd, FpMul, etc.)
                // that compile naturally into gates. No special compiler handling needed.

                // INTENT HELPERS: Compile-time intent query functions
                // These return boolean constants based on the intent parameter
                // For now, we default to latency-optimized behavior
                // Booleans are represented as 1-bit values in hardware
                // EXPLICIT TRAIT METHOD CALLS: Disabled due to module ID issues
                // TODO: Re-enable once module ID management bug is fixed
                // When trait implementations create entity instances, the module ID mapping gets
                // corrupted during monomorphization.
                //
                // if call.function.contains("::") && call.args.len() == 1 {
                //     ... trait method inlining code ...
                // }

                match call.function.as_str() {
                    "is_latency_optimized" => {
                        eprintln!(
                            "[INTENT] is_latency_optimized() -> true (default: prefer latency)"
                        );
                        // Default: prefer latency optimization (1'b1)
                        return Some(Expression::with_unknown_type(ExpressionKind::Literal(
                            Value::BitVector { width: 1, value: 1 },
                        )));
                    }
                    "is_area_optimized" => {
                        eprintln!(
                            "[INTENT] is_area_optimized() -> false (default: prefer latency)"
                        );
                        // Default: not area optimized (1'b0)
                        return Some(Expression::with_unknown_type(ExpressionKind::Literal(
                            Value::BitVector { width: 1, value: 0 },
                        )));
                    }
                    "is_throughput_optimized" => {
                        eprintln!(
                            "[INTENT] is_throughput_optimized() -> false (default: prefer latency)"
                        );
                        // Default: not throughput optimized (1'b0)
                        return Some(Expression::with_unknown_type(ExpressionKind::Literal(
                            Value::BitVector { width: 1, value: 0 },
                        )));
                    }
                    _ => {} // Not an intent helper or intrinsic, continue to regular function handling
                }

                // BUG #184 FIX: Check if this call is to an entity (from trait method inlining)
                // Entity instances in trait method bodies are converted to Call expressions
                // We need to instantiate the entity and return a reference to its output
                if let Some(hir) = self.hir {
                    // BUG #207 FIX: When call has type_args, construct the specialized entity name
                    // For example, FpAdd::<IEEE754_32>(...) should look for "FpAdd_fp32" entity
                    let target_entity_name = if !call.type_args.is_empty() {
                        // Evaluate type_args to construct specialized name
                        let mut name_parts = vec![call.function.clone()];
                        for type_arg in &call.type_args {
                            let suffix = match type_arg {
                                hir::HirType::Custom(name) => name.clone(),
                                hir::HirType::Bit(width) => format!("bit{}", width),
                                hir::HirType::Float32 => "fp32".to_string(),
                                hir::HirType::Float64 => "fp64".to_string(),
                                hir::HirType::Float16 => "fp16".to_string(),
                                _ => format!("{:?}", type_arg),
                            };
                            name_parts.push(suffix);
                        }
                        name_parts.join("_")
                    } else if !call.named_type_args.is_empty() {
                        // Check named type args for FloatFormat constants
                        let mut name_parts = vec![call.function.clone()];
                        for type_val in call.named_type_args.values() {
                            // Try to evaluate as FloatFormat constant
                            if let hir::HirType::Custom(name) = type_val {
                                // Check for well-known format names
                                if name == "IEEE754_32" || name == "fp32" || name == "Float32" {
                                    name_parts.push("fp32".to_string());
                                } else if name == "IEEE754_64"
                                    || name == "fp64"
                                    || name == "Float64"
                                {
                                    name_parts.push("fp64".to_string());
                                } else if name == "IEEE754_16"
                                    || name == "fp16"
                                    || name == "Float16"
                                {
                                    name_parts.push("fp16".to_string());
                                } else {
                                    name_parts.push(name.clone());
                                }
                            }
                        }
                        name_parts.join("_")
                    } else {
                        call.function.clone()
                    };

                    // BUG #207 FIX: Look for specialized entity first, then fall back to generic
                    let entity = hir
                        .entities
                        .iter()
                        .find(|e| e.name == target_entity_name)
                        .or_else(|| {
                            // Fall back to generic entity name if specialized not found
                            hir.entities.iter().find(|e| e.name == call.function)
                        });

                    if let Some(entity) = entity {
                        // Look up the module ID for this entity
                        if let Some(&module_id) = self.entity_map.get(&entity.id) {
                            // Convert arguments to MIR expressions
                            let mut arg_exprs = Vec::new();
                            for arg in &call.args {
                                if let Some(mir_arg) = self.convert_expression(arg, depth + 1) {
                                    arg_exprs.push(mir_arg);
                                }
                            }

                            // Find the "result" output port to determine return type
                            let result_port = entity.ports.iter().find(|p| {
                                matches!(p.direction, hir::HirPortDirection::Output)
                                    && (p.name == "result" || p.name.starts_with("out"))
                            });

                            // Create a signal for the result
                            let result_signal_id = self.next_signal_id();
                            let instance_name =
                                format!("__inlined_{}_{}", call.function, result_signal_id.0);

                            // Store pending instance for later materialization
                            // The signal will be connected to the instance's result port
                            self.pending_module_instances.push((
                                vec![result_signal_id],
                                call.function.clone(),
                                module_id,
                                arg_exprs,
                                Some(
                                    result_port
                                        .map(|p| p.port_type.clone())
                                        .unwrap_or(hir::HirType::Bit(32)),
                                ),
                                Type::Unknown,
                            ));

                            // Return a reference to the result signal
                            return Some(Expression::with_unknown_type(ExpressionKind::Ref(
                                LValue::Signal(result_signal_id),
                            )));
                        }
                        // If entity not in entity_map, fall through to function handling
                    }
                }

                // TIER 2: USER-DEFINED FUNCTION - Apply hybrid inlining strategy
                // This is NOT a primitive FP operation (like fp_mul, fp_add)
                // User-defined functions like quadratic_solve reach here
                // Apply heuristic: inline small functions (≤5 calls), synthesize modules for complex functions (>5 calls)
                println!(
                    "🚀🚀🚀 [HYBRID] Call '{}' is user-defined function 🚀🚀🚀",
                    call.function
                );
                eprintln!(
                    "[HYBRID] Call '{}' is user-defined function, applying hybrid inlining decision",
                    call.function
                );

                // File logging for debugging (works even when eprintln! is suppressed)
                use std::fs::OpenOptions;
                use std::io::Write;
                if let Ok(mut file) = OpenOptions::new()
                    .create(true)
                    .append(true)
                    .open("/tmp/hybrid_decisions.log")
                {
                    let _ = writeln!(
                        file,
                        "[HYBRID] Processing user-defined function: {}",
                        call.function
                    );
                }

                // Look up function in HIR to count calls
                let should_inline = if let Some(hir) = self.hir {
                    let mut func_body = None;

                    println!(
                        "🔎🔎🔎 [HYBRID] Looking up '{}' in HIR... 🔎🔎🔎",
                        call.function
                    );

                    // Search in implementations
                    for impl_block in &hir.implementations {
                        if let Some(func) = impl_block
                            .functions
                            .iter()
                            .find(|f| f.name == call.function)
                        {
                            func_body = Some(&func.body);
                            println!(
                                "🔎🔎🔎 [HYBRID] Found '{}' in implementations 🔎🔎🔎",
                                call.function
                            );
                            break;
                        }
                    }

                    // Search in top-level functions if not found
                    if func_body.is_none() {
                        if let Some(func) = hir.functions.iter().find(|f| f.name == call.function) {
                            func_body = Some(&func.body);
                            println!(
                                "🔎🔎🔎 [HYBRID] Found '{}' in top-level functions 🔎🔎🔎",
                                call.function
                            );
                        }
                    }

                    if func_body.is_none() {
                        println!(
                            "🔎🔎🔎 [HYBRID] '{}' NOT FOUND in HIR! 🔎🔎🔎",
                            call.function
                        );
                    }

                    if let Some(body) = func_body {
                        println!(
                            "📊📊📊 [HYBRID] '{}': body has {} statements 📊📊📊",
                            call.function,
                            body.len()
                        );
                        // Count calls across all statements in the function body
                        // DEBUG: Print each statement type
                        for (i, stmt) in body.iter().enumerate() {
                            println!(
                                "[HYBRID_DEBUG] Statement {}: {:?}",
                                i,
                                std::mem::discriminant(stmt)
                            );
                        }
                        let call_count: usize = body
                            .iter()
                            .map(|stmt| self.count_calls_in_statement(stmt))
                            .sum();
                        println!(
                            "📊📊📊 [HYBRID] Function '{}' contains {} nested calls (threshold={}) 📊📊📊",
                            call.function, call_count, MAX_INLINE_CALL_COUNT
                        );
                        eprintln!(
                            "[HYBRID] Function '{}' contains {} nested calls (threshold={})",
                            call.function, call_count, MAX_INLINE_CALL_COUNT
                        );

                        // File logging
                        if let Ok(mut file) = OpenOptions::new()
                            .create(true)
                            .append(true)
                            .open("/tmp/hybrid_decisions.log")
                        {
                            let _ = writeln!(
                                file,
                                "  Call count for '{}': {} (threshold: {})",
                                call.function, call_count, MAX_INLINE_CALL_COUNT
                            );
                        }

                        // Decision: inline if ≤ threshold, synthesize if > threshold
                        // NOTE: Threshold has been increased to 20 to work around stubbed module synthesis
                        let should = call_count <= MAX_INLINE_CALL_COUNT;
                        println!("🎲🎲🎲 [HYBRID] Decision for '{}': {} <= {}? {} (should_inline={}) 🎲🎲🎲",
                            call.function, call_count, MAX_INLINE_CALL_COUNT, should, should);
                        should
                    } else {
                        eprintln!(
                            "[HYBRID] Function '{}' not found in HIR, defaulting to inline",
                            call.function
                        );
                        true // Default to inline if function not found
                    }
                } else {
                    debug_println!("[HYBRID] No HIR available, defaulting to inline");
                    true // Default to inline if no HIR
                };

                println!(
                    "🎲🎲🎲 [HYBRID] Final should_inline={} for '{}' 🎲🎲🎲",
                    should_inline, call.function
                );
                if should_inline {
                    // Path 1: INLINE the function (simple functions with ≤5 nested calls)
                    eprintln!(
                        "[HYBRID] ✓ Decision: INLINE function '{}' (within threshold)",
                        call.function
                    );

                    // BUG FIX #IMPORT_MATCH: Save and clear match_arm_prefix before inlining
                    // When a function is called from within a match arm, the match_arm_prefix must NOT
                    // contaminate the function's local variables.
                    let saved_match_arm_prefix = self.match_arm_prefix.take();
                    eprintln!(
                        "[BUG #IMPORT_MATCH] Before inlining '{}': saved match_arm_prefix={:?}",
                        call.function, saved_match_arm_prefix
                    );

                    let result = self.inline_function_call(call);

                    // BUG FIX #IMPORT_MATCH: Restore match_arm_prefix after inlining
                    self.match_arm_prefix = saved_match_arm_prefix;
                    eprintln!(
                        "[BUG #IMPORT_MATCH] After inlining '{}': restored match_arm_prefix={:?}",
                        call.function, self.match_arm_prefix
                    );

                    if result.is_none() {
                        eprintln!(
                            "❌ [BUG #85] Call '{}' inlining FAILED - returning None to caller!",
                            call.function
                        );
                        debug_eprintln!("❌ [BUG #85] This will cause any assignment with this call as RHS to be DROPPED!");
                    } else {
                        debug_println!("[MIR_CALL] Call '{}' inlining succeeded", call.function);
                    }
                    result
                } else {
                    // Path 2: SYNTHESIZE MODULE (complex functions with >5 nested calls)
                    println!(
                        "⚙️⚙️⚙️ [HYBRID] SYNTHESIZE MODULE PATH for '{}' ⚙️⚙️⚙️",
                        call.function
                    );
                    eprintln!(
                        "[HYBRID] ⚙️  Decision: SYNTHESIZE MODULE for function '{}' (exceeds threshold)",
                        call.function
                    );

                    // Step 1: Ensure the function has been synthesized as a module
                    let module_id = if let Some(&existing_module_id) =
                        self.function_map.get(&call.function)
                    {
                        debug_println!("✅✅✅ [HYBRID] Module for '{}' already synthesized (module_id={}) ✅✅✅",
                                  call.function, existing_module_id.0);
                        eprintln!(
                            "[HYBRID] ✓ Module for '{}' already synthesized (module_id={})",
                            call.function, existing_module_id.0
                        );
                        existing_module_id
                    } else {
                        println!(
                            "🔧🔧🔧 [HYBRID] Need to synthesize module for '{}' 🔧🔧🔧",
                            call.function
                        );
                        eprintln!(
                            "[HYBRID] ⚙️  Synthesizing module for '{}' (first instantiation)",
                            call.function
                        );

                        // Look up function in HIR to synthesize it
                        let func = self.hir.and_then(|hir| {
                            // Search in implementations
                            for impl_block in &hir.implementations {
                                if let Some(f) = impl_block
                                    .functions
                                    .iter()
                                    .find(|f| f.name == call.function)
                                {
                                    return Some(f);
                                }
                            }
                            // Search in top-level functions
                            hir.functions.iter().find(|f| f.name == call.function)
                        });

                        if let Some(func) = func {
                            self.synthesize_function_as_module(func)
                        } else {
                            debug_eprintln!("❌ [HYBRID ERROR] Cannot synthesize '{}' - function not found in HIR!",
                                      call.function);
                            return None;
                        }
                    };

                    // Get HIR return type for signal creation (look up function again)
                    let hir_return_type = if let Some(hir) = self.hir {
                        let mut found_return_type = None;
                        // Search in implementations
                        for impl_block in &hir.implementations {
                            if let Some(f) = impl_block
                                .functions
                                .iter()
                                .find(|f| f.name == call.function)
                            {
                                found_return_type = Some(f.return_type.clone());
                                break;
                            }
                        }
                        // If not found, search in top-level functions
                        if found_return_type.is_none() {
                            if let Some(f) = hir.functions.iter().find(|f| f.name == call.function)
                            {
                                found_return_type = Some(f.return_type.clone());
                            }
                        }
                        found_return_type.unwrap_or_else(|| {
                            eprintln!(
                                "❌ [HYBRID ERROR] Cannot find function '{}' to get return type!",
                                call.function
                            );
                            None // No return type
                        })
                    } else {
                        debug_eprintln!("❌ [HYBRID ERROR] No HIR available!");
                        None // Fallback
                    };

                    // Step 2: Pre-allocate SignalIds for the result
                    // BUG FIX #92: For tuple returns, pre-allocate signal IDs for ALL elements
                    let tuple_size = Self::get_tuple_size_from_hir_type(&hir_return_type);
                    let num_result_signals = if tuple_size > 0 { tuple_size } else { 1 };

                    let mut result_signal_ids = Vec::with_capacity(num_result_signals);
                    for _ in 0..num_result_signals {
                        result_signal_ids.push(self.next_signal_id());
                    }

                    debug_println!("[HYBRID] Pre-allocated {} result signal(s) {:?} for call to '{}' (tuple_size={})",
                              num_result_signals, result_signal_ids.iter().map(|s| s.0).collect::<Vec<_>>(),
                              call.function, tuple_size);

                    // Step 3: Convert arguments to MIR expressions
                    // For variable references, expand to their RHS expressions to ensure
                    // module instance connections have actual signal drivers
                    let mut arg_exprs = Vec::new();
                    for arg in &call.args {
                        if let Some(mut arg_expr) = self.convert_expression(arg, depth + 1) {
                            // If the argument is a variable reference, try to expand it to the RHS
                            // This ensures module instance connections use actual signal expressions
                            // instead of variable references that may not have continuous drivers
                            if let ExpressionKind::Ref(LValue::Variable(var_id)) = &arg_expr.kind {
                                eprintln!(
                                    "[HYBRID_RHS] Looking up RHS for var {:?} ({})",
                                    var_id,
                                    self.mir_variable_names
                                        .get(var_id)
                                        .unwrap_or(&"unknown".to_string())
                                );
                                eprintln!(
                                    "[HYBRID_RHS] Available RHS entries: {:?}",
                                    self.dynamic_variable_rhs.keys().collect::<Vec<_>>()
                                );
                                if let Some(rhs_expr) = self.dynamic_variable_rhs.get(var_id) {
                                    debug_println!("[HYBRID] Expanding variable {:?} to its RHS expression for module arg", var_id);
                                    arg_expr = rhs_expr.clone();
                                } else {
                                    debug_println!(
                                        "[HYBRID_RHS] No RHS found for var {:?}!",
                                        var_id
                                    );
                                }
                            }
                            arg_exprs.push(arg_expr);
                        } else {
                            eprintln!(
                                "❌ [HYBRID ERROR] Failed to convert argument for call to '{}'",
                                call.function
                            );
                            return None;
                        }
                    }

                    // Step 4: Record pending module instance (will be added to module later)
                    // BUG FIX #92: Now storing Vec<SignalId> for tuple support
                    // BUG FIX: Use hir_return_type to get the actual function return type
                    // instead of the inferred expression type (which might be wrong for tuples)
                    let frontend_return_type = Self::hir_type_to_frontend_type(&hir_return_type)
                        .unwrap_or_else(|| ty.clone());
                    eprintln!(
                        "[HYBRID] BUG FIX: Using frontend_return_type {:?} (hir_return_type: {:?})",
                        frontend_return_type, hir_return_type
                    );
                    self.pending_module_instances.push((
                        result_signal_ids.clone(),
                        call.function.clone(),
                        module_id,
                        arg_exprs,
                        hir_return_type,
                        frontend_return_type,
                    ));
                    eprintln!(
                        "[HYBRID] ✓ Recorded pending module instance for '{}'",
                        call.function
                    );

                    // Step 5: Return an Expression that references the pre-allocated result signal(s)
                    // BUG FIX #92: For tuple returns, return a Concat of all result signals
                    if result_signal_ids.len() > 1 {
                        // Tuple return: create Concat of all result signals
                        // BUG FIX #92: Use forward order (result_0 at MSB) to match TupleLiteral
                        // which packs elements MSB-first: (a, b, c) -> {a, b, c}
                        let concat_elements: Vec<Expression> = result_signal_ids
                            .iter()
                            .map(|sig_id| {
                                Expression::with_unknown_type(ExpressionKind::Ref(LValue::Signal(
                                    *sig_id,
                                )))
                            })
                            .collect();
                        debug_println!("[HYBRID] ✓ Returning Concat of {} result signals for tuple (forward order)", concat_elements.len());
                        Some(Expression::with_unknown_type(ExpressionKind::Concat(
                            concat_elements,
                        )))
                    } else {
                        // Single return
                        Some(Expression::new(
                            ExpressionKind::Ref(LValue::Signal(result_signal_ids[0])),
                            ty,
                        ))
                    }
                }
            }
            hir::HirExpression::Index(base, index) => {
                // BUG #27 FIX: Check if this is a constant index into a flattened array
                // If so, directly reference the flattened signal instead of creating BitSelect
                let mut try_const_array_index = || -> Option<Expression> {
                    // Check if base is a flattened signal or port
                    let (base_hir_id, is_signal) = match base.as_ref() {
                        hir::HirExpression::Signal(id) => {
                            if self.flattened_signals.contains_key(id) {
                                (id.0, true)
                            } else {
                                return None;
                            }
                        }
                        hir::HirExpression::Port(id) => {
                            if self.flattened_ports.contains_key(id) {
                                (id.0, false)
                            } else {
                                return None;
                            }
                        }
                        _ => return None,
                    };

                    // Try to evaluate index as constant
                    let index_val = self.try_eval_const_expr(index)?;

                    // Get flattened fields
                    let fields = if is_signal {
                        self.flattened_signals.get(&hir::SignalId(base_hir_id))?
                    } else {
                        self.flattened_ports.get(&hir::PortId(base_hir_id))?
                    };

                    // Find the field with matching index
                    let index_str = index_val.to_string();
                    for field in fields {
                        if field.field_path.first() == Some(&index_str)
                            && field.field_path.len() == 1
                        {
                            // Found it! Return direct signal/port reference
                            return if is_signal {
                                Some(Expression::with_unknown_type(ExpressionKind::Ref(
                                    LValue::Signal(SignalId(field.id)),
                                )))
                            } else {
                                Some(Expression::with_unknown_type(ExpressionKind::Ref(
                                    LValue::Port(PortId(field.id)),
                                )))
                            };
                        }
                    }
                    None
                };

                // Try constant array index optimization
                if let Some(expr) = try_const_array_index() {
                    return Some(expr);
                }

                // BUG #29 FIX: Check if this is a dynamic index into a flattened array
                // If so, generate a MUX tree instead of BitSelect
                let mut try_dynamic_array_index = || -> Option<Expression> {
                    // Check if base is a flattened signal or port
                    let (base_hir_id, is_signal) = match base.as_ref() {
                        hir::HirExpression::Signal(id) => {
                            if self.flattened_signals.contains_key(id) {
                                (id.0, true)
                            } else {
                                return None;
                            }
                        }
                        hir::HirExpression::Port(id) => {
                            if self.flattened_ports.contains_key(id) {
                                (id.0, false)
                            } else {
                                return None;
                            }
                        }
                        _ => return None,
                    };

                    // Get flattened fields and clone to avoid borrow issues
                    let fields = if is_signal {
                        self.flattened_signals
                            .get(&hir::SignalId(base_hir_id))?
                            .clone()
                    } else {
                        self.flattened_ports.get(&hir::PortId(base_hir_id))?.clone()
                    };

                    // Group fields by array index (first component of path)
                    // For simple arrays: ["0"], ["1"], etc.
                    // For arrays of structs: ["0", "x"], ["0", "y"], ["1", "x"], ["1", "y"], etc.
                    let mut array_groups: IndexMap<usize, Vec<FlattenedField>> = IndexMap::new();

                    for field in &fields {
                        if let Some(first) = field.field_path.first() {
                            // Check if first component is numeric (array index)
                            if first.chars().all(|c| c.is_ascii_digit()) {
                                if let Ok(idx) = first.parse::<usize>() {
                                    array_groups.entry(idx).or_default().push(field.clone());
                                }
                            }
                        }
                    }

                    // Must have at least one array element
                    if array_groups.is_empty() {
                        return None;
                    }

                    // Convert array_groups to sorted vec
                    let mut array_elements: Vec<(usize, Vec<FlattenedField>)> =
                        array_groups.into_iter().collect();
                    array_elements.sort_by_key(|(idx, _)| *idx);

                    // Convert index expression to MIR (needs mutable borrow)
                    let mir_index = self.convert_expression(index, depth + 1)?;

                    // NOTE: For arrays of structs, we can't build struct literals in MIR
                    // because MIR only has scalar expressions. The array read should be
                    // handled by try_expand_array_index_read_assignment which creates
                    // multiple assignments (one per struct field).
                    //
                    // This code path is only for simple scalar arrays where a single
                    // MUX tree can select the value.

                    // Build element expressions - only handle simple scalar arrays here
                    let mut element_exprs: Vec<(usize, Expression)> = Vec::new();

                    for (array_idx, group_fields) in &array_elements {
                        if group_fields.len() == 1 && group_fields[0].field_path.len() == 1 {
                            // Simple scalar array element - just reference the field
                            let field = &group_fields[0];
                            let elem_expr = if is_signal {
                                Expression::with_unknown_type(ExpressionKind::Ref(LValue::Signal(
                                    SignalId(field.id),
                                )))
                            } else {
                                Expression::with_unknown_type(ExpressionKind::Ref(LValue::Port(
                                    PortId(field.id),
                                )))
                            };
                            element_exprs.push((*array_idx, elem_expr));
                        } else {
                            // Array of structs - this should be handled by try_expand_array_index_read_assignment
                            // instead of here. Fall back to BitSelect.
                            return None;
                        }
                    }

                    // Build MUX tree: (idx == 0) ? elem_0 : ((idx == 1) ? elem_1 : ...)
                    let mut mux_expr = None;

                    // Build from last to first (nested ternaries)
                    for (array_idx, elem_expr) in element_exprs.iter().rev() {
                        if let Some(else_expr) = mux_expr {
                            // Build condition: index == array_idx
                            let index_literal = Expression::with_unknown_type(
                                ExpressionKind::Literal(Value::Integer(*array_idx as i64)),
                            );
                            let condition = Expression::with_unknown_type(ExpressionKind::Binary {
                                op: BinaryOp::Equal,
                                left: Box::new(mir_index.clone()),
                                right: Box::new(index_literal),
                            });

                            // Build ternary: condition ? elem_expr : else_expr
                            mux_expr =
                                Some(Expression::with_unknown_type(ExpressionKind::Conditional {
                                    cond: Box::new(condition),
                                    then_expr: Box::new(elem_expr.clone()),
                                    else_expr: Box::new(else_expr),
                                }));
                        } else {
                            // Last element (no else branch)
                            mux_expr = Some(elem_expr.clone());
                        }
                    }

                    mux_expr
                };

                // Try dynamic array index expansion
                if let Some(expr) = try_dynamic_array_index() {
                    return Some(expr);
                }

                // BUG FIX #34: Handle constant array indexing
                // Example: const TABLE: nat[8][16] = [...]; value = TABLE[index];
                // Build a MUX tree over the constant array values
                let mut try_constant_array_index = || -> Option<Expression> {
                    // Check if base is a Constant
                    let const_id = match base.as_ref() {
                        hir::HirExpression::Constant(id) => *id,
                        _ => return None,
                    };

                    // Look up the constant definition
                    let hir = self.hir.as_ref()?;
                    let mut const_value_expr = None;

                    for implementation in &hir.implementations {
                        for constant in &implementation.constants {
                            if constant.id == const_id {
                                const_value_expr = Some(&constant.value);
                                break;
                            }
                        }
                    }

                    let const_value_expr = const_value_expr?;

                    // Check if the constant value is an array literal
                    let array_elements = match const_value_expr {
                        hir::HirExpression::ArrayLiteral(elements) => elements,
                        _ => return None,
                    };

                    if array_elements.is_empty() {
                        return None;
                    }

                    // Convert index expression to MIR
                    let mir_index = self.convert_expression(index, depth + 1)?;

                    // Convert all array elements to MIR expressions
                    let mut mir_elements: Vec<Expression> = Vec::new();
                    for elem in array_elements {
                        let mir_elem = self.convert_expression(elem, depth + 1)?;
                        mir_elements.push(mir_elem);
                    }

                    // Build MUX tree: (index == 0) ? elem_0 : ((index == 1) ? elem_1 : ...)
                    let mut mux_expr = None;

                    // Build from last to first (nested ternaries)
                    for (i, elem_expr) in mir_elements.iter().enumerate().rev() {
                        if let Some(else_expr) = mux_expr {
                            // Build condition: index == i
                            let index_literal = Expression::with_unknown_type(
                                ExpressionKind::Literal(Value::Integer(i as i64)),
                            );
                            let condition = Expression::with_unknown_type(ExpressionKind::Binary {
                                op: BinaryOp::Equal,
                                left: Box::new(mir_index.clone()),
                                right: Box::new(index_literal),
                            });

                            // Build ternary: condition ? elem_expr : else_expr
                            mux_expr =
                                Some(Expression::with_unknown_type(ExpressionKind::Conditional {
                                    cond: Box::new(condition),
                                    then_expr: Box::new(elem_expr.clone()),
                                    else_expr: Box::new(else_expr),
                                }));
                        } else {
                            // Last element (no else branch) - this is the default case
                            mux_expr = Some(elem_expr.clone());
                        }
                    }

                    mux_expr
                };

                // Try constant array index expansion
                if let Some(expr) = try_constant_array_index() {
                    return Some(expr);
                }

                // Fall back to bit select
                println!(
                    "[BUG127_INDEX_DEBUG] Fallback to bit select: base type={:?}, index type={:?}",
                    std::mem::discriminant(base.as_ref()),
                    std::mem::discriminant(index.as_ref())
                );
                if let hir::HirExpression::Port(id) = base.as_ref() {
                    println!(
                        "[BUG127_INDEX_DEBUG]   base is Port({:?}), in port_map={}",
                        id,
                        self.port_map.contains_key(id)
                    );
                }

                // BUG FIX: Handle cases where base is not an LValue (e.g., Concat expressions
                // from function inlining like clz32({sum_raw, 4'b0000})[31])
                // When the base is a Concat or other non-LValue expression, we need to:
                // 1. Try the LValue path first (common case for signals/variables)
                // 2. Fall back to shift/mask operations: (base >> index) & 1
                if let Some(base_lval) = self.expr_to_lvalue(base) {
                    let index_expr = match self.convert_expression(index, depth + 1) {
                        Some(expr) => expr,
                        None => {
                            println!(
                                "[BUG127_INDEX_DEBUG]   INDEX EXPR conversion FAILED for index type={:?}",
                                std::mem::discriminant(index.as_ref())
                            );
                            return None;
                        }
                    };
                    return Some(Expression::new(
                        ExpressionKind::Ref(LValue::BitSelect {
                            base: Box::new(base_lval),
                            index: Box::new(index_expr),
                        }),
                        ty,
                    ));
                }

                // Base is not an LValue - use shift/mask operations: (base >> index) & 1
                println!(
                    "[BUG127_INDEX_DEBUG] Using shift/mask fallback for non-LValue base type={:?}",
                    std::mem::discriminant(base.as_ref())
                );
                let base_expr = self.convert_expression(base, depth + 1)?;
                let index_expr = self.convert_expression(index, depth + 1)?;

                // Compute: (base >> index) & 1 to extract single bit
                let shifted = Expression::with_unknown_type(ExpressionKind::Binary {
                    op: BinaryOp::RightShift,
                    left: Box::new(base_expr),
                    right: Box::new(index_expr),
                });
                // Mask with 1 to get single bit
                let one = Expression::with_unknown_type(ExpressionKind::Literal(Value::Integer(1)));
                Some(Expression::with_unknown_type(ExpressionKind::Binary {
                    op: BinaryOp::And,
                    left: Box::new(shifted),
                    right: Box::new(one),
                }))
            }
            hir::HirExpression::Range(base, high, low) => {
                // Convert range expression to range select
                // BUG #145 FIX: Handle cases where base is not an LValue (e.g., after function inlining)
                // When the base is a Literal or other non-LValue expression, we need to:
                // 1. Try constant evaluation if possible
                // 2. Otherwise use shift/mask operations: (base >> low) & ((1 << (high - low + 1)) - 1)

                // First try the LValue path (the common case for signals/variables)
                if let Some(base_lval) = self.expr_to_lvalue(base) {
                    let high_expr = self.convert_expression(high, depth + 1)?;
                    let low_expr = self.convert_expression(low, depth + 1)?;
                    return Some(Expression::new(
                        ExpressionKind::Ref(LValue::RangeSelect {
                            base: Box::new(base_lval),
                            high: Box::new(high_expr),
                            low: Box::new(low_expr),
                        }),
                        ty,
                    ));
                }

                // Base is not an LValue - try constant evaluation first
                if let (Some(base_val), Some(high_val), Some(low_val)) = (
                    self.try_evaluate_constant_expr(base),
                    self.try_evaluate_constant_expr(high),
                    self.try_evaluate_constant_expr(low),
                ) {
                    // Evaluate range extraction at compile time
                    let width = (high_val - low_val + 1) as usize;
                    let mask = if width >= 64 {
                        u64::MAX
                    } else {
                        (1u64 << width) - 1
                    };
                    let result = (base_val >> low_val) & mask;
                    return Some(Expression::new(
                        ExpressionKind::Literal(Value::BitVector {
                            width,
                            value: result,
                        }),
                        ty,
                    ));
                }

                // Fall back to shift/mask operations for dynamic cases
                let base_expr = self.convert_expression(base, depth + 1)?;
                let high_expr = self.convert_expression(high, depth + 1)?;
                let low_expr = self.convert_expression(low, depth + 1)?;

                // Compute: (base >> low) & mask
                // First, shift right by low: base >> low
                let shifted = Expression::new(
                    ExpressionKind::Binary {
                        op: BinaryOp::RightShift,
                        left: Box::new(base_expr),
                        right: Box::new(low_expr.clone()),
                    },
                    ty.clone(),
                );

                // Compute mask width: high - low + 1
                // Then mask: (1 << width) - 1
                // For now, if high and low are constants, compute the mask directly
                // Otherwise, we'd need more complex logic - but for function inlining,
                // indices are typically constants
                if let (
                    ExpressionKind::Literal(Value::Integer(h)),
                    ExpressionKind::Literal(Value::Integer(l)),
                ) = (&high_expr.kind, &low_expr.kind)
                {
                    let width = (h - l + 1) as usize;
                    let mask_val = if width >= 64 {
                        u64::MAX
                    } else {
                        (1u64 << width) - 1
                    };
                    let mask_expr = Expression::new(
                        ExpressionKind::Literal(Value::BitVector {
                            width,
                            value: mask_val,
                        }),
                        ty.clone(),
                    );
                    Some(Expression::new(
                        ExpressionKind::Binary {
                            op: BinaryOp::BitwiseAnd,
                            left: Box::new(shifted),
                            right: Box::new(mask_expr),
                        },
                        ty,
                    ))
                } else if let (
                    ExpressionKind::Literal(Value::BitVector { value: h, .. }),
                    ExpressionKind::Literal(Value::BitVector { value: l, .. }),
                ) = (&high_expr.kind, &low_expr.kind)
                {
                    let width = (h - l + 1) as usize;
                    let mask_val = if width >= 64 {
                        u64::MAX
                    } else {
                        (1u64 << width) - 1
                    };
                    let mask_expr = Expression::new(
                        ExpressionKind::Literal(Value::BitVector {
                            width,
                            value: mask_val,
                        }),
                        ty.clone(),
                    );
                    Some(Expression::new(
                        ExpressionKind::Binary {
                            op: BinaryOp::BitwiseAnd,
                            left: Box::new(shifted),
                            right: Box::new(mask_expr),
                        },
                        ty,
                    ))
                } else {
                    // Dynamic indices - just return the shifted value
                    // (this may lose precision but handles the common case)
                    Some(shifted)
                }
            }
            hir::HirExpression::FieldAccess { base, field } => {
                // Convert struct field access to bit slice (range select)
                eprintln!(
                    "[BUG #71 FIELD] Converting FieldAccess: field='{}', base={:?}",
                    field,
                    std::mem::discriminant(&**base)
                );
                if let hir::HirExpression::Variable(var_id) = &**base {
                    debug_println!("[BUG #71 FIELD]   Base is Variable({})", var_id.0);
                    // Check if this variable is in dynamic_variables (tuple temporary)
                    if let Some((mir_id, name, hir_type)) = self.dynamic_variables.get(var_id) {
                        eprintln!(
                            "[BUG #71 FIELD]   Found in dynamic_variables: name='{}', type={:?}",
                            name, hir_type
                        );
                        if matches!(hir_type, hir::HirType::Tuple(_)) {
                            debug_println!("[BUG #71 FIELD]   ✅ This is a TUPLE variable!");
                        }
                    }
                }
                self.convert_field_access(base, field)
            }
            hir::HirExpression::EnumVariant { enum_type, variant } => {
                // Look up the enum variant value
                self.resolve_enum_variant_value(enum_type, variant)
            }
            hir::HirExpression::AssociatedConstant {
                type_name,
                constant_name,
            } => {
                // Associated constants like fp32::ZERO, T::MAX_VALUE, etc.
                // These should be resolved during monomorphization/type checking
                // For now, return a placeholder value
                // TODO: Implement proper trait-based resolution of associated constants
                //       - Look up trait implementations for the type
                //       - Find the constant definition
                //       - Return its value
                // Common associated constants:
                // - ZERO: 0
                // - ONE: 1
                // - MAX_VALUE: type maximum
                // - MIN_VALUE: type minimum
                match constant_name.as_str() {
                    "ZERO" => Some(Expression::new(
                        ExpressionKind::Literal(Value::Integer(0)),
                        ty,
                    )),
                    "ONE" => Some(Expression::new(
                        ExpressionKind::Literal(Value::Integer(1)),
                        ty,
                    )),
                    "MAX_VALUE" => Some(Expression::new(
                        ExpressionKind::Literal(Value::Integer(i64::MAX)),
                        ty,
                    )),
                    "MIN_VALUE" => Some(Expression::new(
                        ExpressionKind::Literal(Value::Integer(i64::MIN)),
                        ty,
                    )),
                    _ => {
                        // Unknown associated constant - return 0 as fallback
                        // This will be properly resolved during type checking/monomorphization
                        Some(Expression::new(
                            ExpressionKind::Literal(Value::Integer(0)),
                            ty,
                        ))
                    }
                }
            }
            hir::HirExpression::ArrayRepeat { value, count } => {
                // Array/bit repeat expression: {count{value}} in Verilog syntax
                // This creates a bit vector with `count` copies of `value`
                // We expand it into a Concat for hardware (bit concatenation)
                //
                // BUG #181 FIX: Properly expand ArrayRepeat into Concat for hardware
                // For {8{1'b1}}, we create Concat([1'b1, 1'b1, 1'b1, 1'b1, 1'b1, 1'b1, 1'b1, 1'b1])

                // Try to evaluate count as a const expression
                if let Ok(count_val) = self.const_evaluator.eval(count) {
                    if let Some(count_nat) = count_val.as_nat() {
                        // For very large repeats, don't expand (could cause memory issues)
                        if count_nat > 1024 {
                            return Some(Expression::new(
                                ExpressionKind::Literal(Value::Integer(0)),
                                ty,
                            ));
                        }

                        // For count == 0, return empty (0)
                        if count_nat == 0 {
                            return Some(Expression::new(
                                ExpressionKind::Literal(Value::Integer(0)),
                                ty,
                            ));
                        }

                        // For count == 1, just return the value
                        if count_nat == 1 {
                            return self.convert_expression(value, depth + 1);
                        }

                        // BUG #181 FIX: Expand into Concat with count copies of value
                        // Convert the value expression to MIR first
                        if let Some(value_mir) = self.convert_expression(value, depth + 1) {
                            let mut elements = Vec::with_capacity(count_nat);
                            for _ in 0..count_nat {
                                elements.push(value_mir.clone());
                            }
                            return Some(Expression::with_unknown_type(ExpressionKind::Concat(
                                elements,
                            )));
                        }
                    }
                }

                // If count couldn't be evaluated, return placeholder
                Some(Expression::new(
                    ExpressionKind::Literal(Value::Integer(0)),
                    ty,
                ))
            }
            hir::HirExpression::Concat(expressions) => {
                // Bit concatenation: {a, b, c}
                // In hardware, concatenation combines multiple bit vectors into a single wider vector
                // The first element becomes the most significant bits
                println!(
                    "🔵🔵🔵 BUG #127: Concat handler entered with {} elements at depth {} 🔵🔵🔵",
                    expressions.len(),
                    depth
                );
                for (idx, expr) in expressions.iter().enumerate() {
                    println!(
                        "🔵🔵🔵   element[{}] type: {:?} 🔵🔵🔵",
                        idx,
                        std::mem::discriminant(expr)
                    );
                }

                if expressions.is_empty() {
                    return Some(Expression::new(
                        ExpressionKind::Literal(Value::Integer(0)),
                        ty,
                    ));
                }

                if expressions.len() == 1 {
                    debug_println!("[DEBUG] Concat has only 1 element, unwrapping");
                    return self.convert_expression(&expressions[0], depth + 1);
                }

                // BUG #160 FIX: Context-aware width inference for concat elements
                // When we have a concat like {0, a} and know the target width,
                // infer the width of integer literals from context.
                //
                // First, determine the target width from the type context
                // Try the inferred type first, then fall back to the return type from the inlining stack
                let target_width = self.get_type_width(&ty).or_else(|| {
                    // If we're inside function inlining, check the return type stack
                    if let Some(Some(ret_ty)) = self.inlining_return_type_stack.last() {
                        match ret_ty {
                            hir::HirType::Bit(w)
                            | hir::HirType::Logic(w)
                            | hir::HirType::Int(w)
                            | hir::HirType::Nat(w) => {
                                eprintln!(
                                    "[BUG #160 FIX] Concat: using inlining return type width = {}",
                                    w
                                );
                                Some(*w)
                            }
                            _ => None,
                        }
                    } else {
                        None
                    }
                });

                // Calculate widths of all non-literal elements
                let mut known_widths: Vec<Option<u32>> = Vec::new();
                let mut total_known_width = 0u32;
                let mut num_unknown = 0usize;

                for expr in expressions.iter() {
                    let width = self.infer_hir_expression_width(expr);
                    if let Some(w) = width {
                        total_known_width += w;
                    } else {
                        num_unknown += 1;
                    }
                    known_widths.push(width);
                }

                // If we have exactly one unknown width element and know the target,
                // we can infer the unknown width
                let inferred_width = match (num_unknown, target_width) {
                    (1, Some(target)) if target >= total_known_width => {
                        Some(target - total_known_width)
                    }
                    _ => None,
                };

                // Convert all concat elements to MIR expressions
                let mut mir_exprs = Vec::new();
                for (idx, expr) in expressions.iter().enumerate() {
                    // BUG #160 FIX: For integer literals with unknown width, use the inferred width
                    if let (None, Some(width)) = (known_widths[idx], inferred_width) {
                        if let hir::HirExpression::Literal(hir::HirLiteral::Integer(val)) = expr {
                            let width = width as usize;
                            mir_exprs.push(Expression::new(
                                ExpressionKind::Literal(Value::BitVector { width, value: *val }),
                                ty.clone(),
                            ));
                            continue;
                        }
                    }

                    if let Some(mir_expr) = self.convert_expression(expr, depth + 1) {
                        mir_exprs.push(mir_expr);
                    } else {
                        println!(
                            "🔴🔴🔴 BUG #127: Concat element[{}] conversion FAILED! Type: {:?} 🔴🔴🔴",
                            idx,
                            std::mem::discriminant(expr)
                        );
                        // Print more info about the element
                        match expr {
                            hir::HirExpression::Unary(unary) => {
                                println!(
                                    "🔴🔴🔴   Unary op: {:?}, operand type: {:?} 🔴🔴🔴",
                                    unary.op,
                                    std::mem::discriminant(&*unary.operand)
                                );
                            }
                            hir::HirExpression::Range(base, high, low) => {
                                println!(
                                    "🔴🔴🔴   Range: base={:?}, high={:?}, low={:?} 🔴🔴🔴",
                                    std::mem::discriminant(&**base),
                                    std::mem::discriminant(&**high),
                                    std::mem::discriminant(&**low)
                                );
                            }
                            _ => {}
                        }
                        return None;
                    }
                }

                Some(Expression::new(ExpressionKind::Concat(mir_exprs), ty))
            }
            hir::HirExpression::Ternary {
                condition,
                true_expr,
                false_expr,
            } => {
                // Ternary conditional expression: condition ? true_expr : false_expr
                // This is identical to an if-expression, so convert to MIR conditional
                let cond = Box::new(self.convert_expression(condition, depth + 1)?);
                let then_expr = Box::new(self.convert_expression(true_expr, depth + 1)?);
                let else_expr = Box::new(self.convert_expression(false_expr, depth + 1)?);

                Some(Expression::new(
                    ExpressionKind::Conditional {
                        cond,
                        then_expr,
                        else_expr,
                    },
                    ty,
                ))
            }
            hir::HirExpression::If(if_expr) => {
                // Convert if-expression to a conditional (ternary) expression in MIR
                // MIR represents this as: condition ? then_expr : else_expr
                //
                // BUG FIX #50: Isolate dynamic variable scopes between branches
                // Each branch can create variables with the same names (e.g., result_32)
                // We must prevent variable name collisions between branches
                //
                // BUG FIX #61: Preserve variables that were hoisted to pending_statements
                // When block expressions in branches contain let bindings, those bindings
                // are added to pending_statements (to be emitted as global assignments).
                // We must preserve these variables across branch isolation so that the
                // result expression can reference them.
                debug_println!(
                    "[IF_DEBUG] ========== STARTING IF EXPRESSION CONVERSION =========="
                );
                eprintln!(
                    "[IF_DEBUG] Converting If-expression condition, type: {:?}",
                    std::mem::discriminant(&*if_expr.condition)
                );
                if let hir::HirExpression::Call(call) = &*if_expr.condition {
                    eprintln!(
                        "[IF_DEBUG]   Condition is Call: function={}, args={}",
                        call.function,
                        call.args.len()
                    );
                }
                let cond = self.convert_expression(&if_expr.condition, depth + 1);
                if cond.is_none() {
                    debug_println!("[IF_DEBUG] ❌ CONDITION conversion FAILED");
                    return None;
                }
                let cond = cond?;
                eprintln!(
                    "[IF_DEBUG] ✅ Converted condition to MIR: {:?}",
                    std::mem::discriminant(&cond.kind)
                );

                // Save current dynamic_variables state and pending_statements count
                let saved_dynamic_vars = self.dynamic_variables.clone();
                let saved_pending_count = self.pending_statements.len();

                eprintln!(
                    "[IF_DEBUG] Converting THEN branch, expr type: {:?}",
                    std::mem::discriminant(&*if_expr.then_expr)
                );
                if let hir::HirExpression::Call(call) = &*if_expr.then_expr {
                    eprintln!(
                        "[IF_DEBUG]   Then-expr is Call: function={}, args={}",
                        call.function,
                        call.args.len()
                    );
                }
                let then_expr = self.convert_expression(&if_expr.then_expr, depth + 1);
                if then_expr.is_none() {
                    eprintln!(
                        "[IF_DEBUG] ❌ THEN BRANCH conversion FAILED, type: {:?}",
                        std::mem::discriminant(&*if_expr.then_expr)
                    );
                    return None;
                }
                let then_expr = then_expr?;
                debug_println!("[IF_DEBUG] ✅ THEN branch converted successfully");

                debug_println!("[DEBUG] If-expr: then_expr = {:?}", then_expr);

                // BUG FIX #63/#68: Capture variables created in then-branch before restoring
                // We need to track ALL new variables from BOTH branches for nested if/else.
                let then_branch_vars = self.dynamic_variables.clone();

                // BUG FIX #71: Before restoring for else-branch, merge new then-branch variables
                // into saved state. This prevents nested restorations in the else-branch from
                // losing variables created in deeply nested scopes of the then-branch.
                //
                // Without this fix, variables like edge1 created in nested matches inside the
                // then-branch would be lost when the else-branch does its own restorations,
                // and couldn't be recovered by the final restoration.
                let mut state_for_else = saved_dynamic_vars.clone();
                for (hir_var_id, (mir_var_id, name, hir_type)) in then_branch_vars.iter() {
                    if !saved_dynamic_vars.contains_key(hir_var_id) {
                        // Preserve new variables from then-branch for else-branch context
                        state_for_else
                            .insert(*hir_var_id, (*mir_var_id, name.clone(), hir_type.clone()));
                    }
                }

                // BUG FIX #63: Restore dynamic_variables before else-branch
                // Start else-branch with original variables PLUS new variables from then-branch.
                // This prevents variable name collisions while preserving nested scope variables.
                self.dynamic_variables = state_for_else;

                eprintln!(
                    "[IF_DEBUG] Converting ELSE branch, expr type: {:?}",
                    std::mem::discriminant(&*if_expr.else_expr)
                );
                if let hir::HirExpression::Call(call) = &*if_expr.else_expr {
                    eprintln!(
                        "[IF_DEBUG]   Else-expr is Call: function={}, args={}",
                        call.function,
                        call.args.len()
                    );
                }
                if let hir::HirExpression::If(_) = &*if_expr.else_expr {
                    debug_println!("[IF_DEBUG]   Else-expr is nested If (else-if chain)");
                }
                let else_expr = self.convert_expression(&if_expr.else_expr, depth + 1);
                if else_expr.is_none() {
                    eprintln!(
                        "[IF_DEBUG] ❌ ELSE BRANCH conversion FAILED, type: {:?}",
                        std::mem::discriminant(&*if_expr.else_expr)
                    );
                    return None;
                }
                let else_expr = else_expr?;
                debug_println!("[IF_DEBUG] ✅ ELSE branch converted successfully");

                // BUG FIX #63/#68: After both branches are processed, restore to the original state
                // BUT preserve NEW variables created during branch processing from BOTH branches.
                //
                // In nested if/else expressions, inner branches may create variables (via function
                // inlining with tuple destructuring) that outer expressions need to reference.
                // We must preserve these newly created variables in dynamic_variables while still
                // isolating branch-local variables to prevent name collisions between sibling branches.
                //
                // Example: 4-level nested if/else with function inlining
                //   if a < 10 { 0 } else if a < 20 { 0 } else if a < 30 { exec(...) } else { 0 }
                // The exec() call creates variables that must remain accessible to parent if/else expressions.
                //
                // The key insight: We need to merge new variables from BOTH then-branch AND else-branch,
                // because in deeply nested if/else, the variables may be created in the then-branch
                // and then cleared by subsequent scope restorations in nested else-branches.

                let mut restored_vars = saved_dynamic_vars.clone();
                let mut preserved_count = 0;

                // BUG #71 DEBUG: Check if edge1 is in then_branch_vars
                let edge1_in_then = then_branch_vars
                    .values()
                    .any(|(id, name, _)| id.0 == 148 || name.contains("edge1"));
                eprintln!(
                    "[BUG #71 THEN_VARS] edge1/var_148 in then_branch_vars? {}",
                    edge1_in_then
                );
                if edge1_in_then {
                    for (mir_id, name, _) in then_branch_vars.values() {
                        if mir_id.0 == 148 || name.contains("edge1") {
                            eprintln!(
                                "[BUG #71 THEN_VARS]   Found: MIR {:?}, name='{}'",
                                mir_id, name
                            );
                        }
                    }
                }

                // Preserve new variables from then-branch
                for (hir_var_id, (mir_var_id, name, hir_type)) in then_branch_vars.iter() {
                    if !saved_dynamic_vars.contains_key(hir_var_id) {
                        debug_println!("[BUG #68] Preserving variable from then-branch: HIR {:?} -> MIR {:?} ({})",
                            hir_var_id, mir_var_id, name);
                        restored_vars
                            .insert(*hir_var_id, (*mir_var_id, name.clone(), hir_type.clone()));
                        preserved_count += 1;
                    }
                }

                // Preserve new variables from else-branch
                for (hir_var_id, (mir_var_id, name, hir_type)) in self.dynamic_variables.iter() {
                    if !saved_dynamic_vars.contains_key(hir_var_id)
                        && !restored_vars.contains_key(hir_var_id)
                    {
                        debug_println!("[BUG #68] Preserving variable from else-branch: HIR {:?} -> MIR {:?} ({})",
                            hir_var_id, mir_var_id, name);
                        restored_vars
                            .insert(*hir_var_id, (*mir_var_id, name.clone(), hir_type.clone()));
                        preserved_count += 1;
                    }
                }

                debug_println!("[BUG #68] If-expression restoration: saved {} vars, then-branch {} vars, else-branch {} vars, preserved {} new vars",
                    saved_dynamic_vars.len(), then_branch_vars.len(), self.dynamic_variables.len(), preserved_count);

                // BUG #71 DEBUG: Check if edge1 is in restored_vars before assignment
                let has_edge1_before = restored_vars
                    .values()
                    .any(|(id, name, _)| id.0 == 148 || name.contains("edge1"));
                eprintln!(
                    "[BUG #71 RESTORE] About to restore dynamic_variables, has edge1/var_148? {}",
                    has_edge1_before
                );
                if has_edge1_before {
                    for (mir_id, name, _) in restored_vars.values() {
                        if mir_id.0 == 148 || name.contains("edge1") {
                            eprintln!(
                                "[BUG #71 RESTORE]   Found: MIR {:?}, name='{}'",
                                mir_id, name
                            );
                        }
                    }
                }

                self.dynamic_variables = restored_vars;

                // BUG #71 DEBUG: Verify edge1 is still there after assignment
                let has_edge1_after = self
                    .dynamic_variables
                    .values()
                    .any(|(id, name, _)| id.0 == 148 || name.contains("edge1"));
                eprintln!(
                    "[BUG #71 RESTORE] After restore, has edge1/var_148? {}",
                    has_edge1_after
                );

                let cond = Box::new(cond);
                let then_expr = Box::new(then_expr);
                let else_expr = Box::new(else_expr);

                debug_println!("[IF_DEBUG] ✅ IF EXPRESSION CONVERSION COMPLETED SUCCESSFULLY");
                debug_println!("[IF_DEBUG] ========================================");
                Some(Expression::new(
                    ExpressionKind::Conditional {
                        cond,
                        then_expr,
                        else_expr,
                    },
                    ty,
                ))
            }
            hir::HirExpression::Match(match_expr) => {
                // Convert match-expression to conditionals
                // Choice of priority (nested ternary) vs parallel (OR of ANDs) based on mux_style
                eprintln!(
                    "[DEBUG] Match expression conversion: {} arms, mux_style={:?}",
                    match_expr.arms.len(),
                    match_expr.mux_style
                );
                for (i, arm) in match_expr.arms.iter().enumerate() {
                    eprintln!(
                        "[DEBUG] Match arm {}: pattern {:?}, expr type {:?}",
                        i,
                        arm.pattern,
                        std::mem::discriminant(&arm.expr)
                    );
                }

                // BUG #145 FIX: Try constant folding when the scrutinee is a constant
                // This handles cases like `match 0 { 0 => a + b, 1 => a - b, ... }`
                // after function inlining when the opcode parameter is a literal
                if let Some(scrutinee_val) = self.try_evaluate_constant_expr(&match_expr.expr) {
                    eprintln!(
                        "[BUG #145] Match scrutinee is constant: {}, checking {} arms",
                        scrutinee_val,
                        match_expr.arms.len()
                    );
                    // Find the matching arm
                    for (arm_idx, arm) in match_expr.arms.iter().enumerate() {
                        let arm_matches = match &arm.pattern {
                            hir::HirPattern::Literal(lit) => match lit {
                                hir::HirLiteral::Integer(val) => scrutinee_val == *val,
                                hir::HirLiteral::Boolean(b) => {
                                    scrutinee_val == (if *b { 1 } else { 0 })
                                }
                                _ => false,
                            },
                            hir::HirPattern::Wildcard => true, // Wildcard matches everything
                            hir::HirPattern::Path(enum_name, _variant)
                                if enum_name == "__CONST__" =>
                            {
                                // Could add constant pattern matching here if needed
                                false
                            }
                            _ => false,
                        };

                        if arm_matches {
                            // Check guard if present
                            let guard_passes = if let Some(guard) = &arm.guard {
                                self.try_evaluate_constant_expr(guard)
                                    .map(|v| v != 0)
                                    .unwrap_or(false)
                            } else {
                                true
                            };

                            if guard_passes {
                                eprintln!(
                                    "[BUG #145] Match constant-folded: arm {} matches (pattern {:?})",
                                    arm_idx, arm.pattern
                                );
                                // Directly convert the matching arm's expression
                                return self.convert_expression(&arm.expr, depth + 1);
                            }
                        }
                    }
                    // No arm matched - fall through to default mux generation
                    eprintln!(
                        "[BUG #145] Match constant-fold: no arm matched, falling back to mux"
                    );
                }

                // Choose mux generation strategy based on intent/attribute
                match match_expr.mux_style {
                    hir::MuxStyle::Parallel => {
                        // Generate OR-of-ANDs: ({W{sel==0}} & a) | ({W{sel==1}} & b) | ...
                        self.convert_match_to_parallel_mux(&match_expr.expr, &match_expr.arms)
                    }
                    hir::MuxStyle::Priority | hir::MuxStyle::Auto => {
                        // Generate nested ternary (priority encoder): (c1) ? v1 : ((c2) ? v2 : default)
                        self.convert_match_to_conditionals(&match_expr.expr, &match_expr.arms)
                    }
                }
            }
            hir::HirExpression::Cast(cast_expr) => {
                // Convert type cast expression
                // Preserve the cast in MIR so codegen knows the intended type
                // For FP/bit reinterpretation casts, this is a no-op in hardware
                // but critical for type tracking
                println!(
                    "[CAST_DEBUG] Cast inner expr type: {:?}, target: {:?}",
                    std::mem::discriminant(&*cast_expr.expr),
                    cast_expr.target_type
                );
                let inner_expr_opt = self.convert_expression(&cast_expr.expr, depth + 1);
                if inner_expr_opt.is_none() {
                    println!(
                        "[CAST_DEBUG] FAILED: convert_expression returned None for Cast inner expr"
                    );
                    return None;
                }
                let inner_expr = inner_expr_opt.unwrap();
                let mut target_type = self.convert_type(&cast_expr.target_type);

                // BUG #65/#66 FIX: Detect and correct erroneous Float16 casts from 32-bit values
                // If casting from a 32-bit type (Bit[32]/Nat[32]) to Float16, this is likely
                // a bug in HIR where fp32 was inferred as fp16. Correct it to Float32.
                if matches!(target_type, DataType::Float16) {
                    let inner_type = self.infer_expression_type_internal(&inner_expr, None);
                    let inner_width = match inner_type {
                        DataType::Bit(w)
                        | DataType::Nat(w)
                        | DataType::Int(w)
                        | DataType::Logic(w) => w,
                        _ => 0,
                    };
                    if inner_width == 32 {
                        debug_println!("[BUG #65/#66 FIX] Correcting Cast from {:?} (width={}) to Float16 → Float32", inner_type, inner_width);
                        target_type = DataType::Float32;
                    }
                }

                Some(Expression::new(
                    ExpressionKind::Cast {
                        expr: Box::new(inner_expr),
                        target_type,
                    },
                    ty,
                ))
            }
            hir::HirExpression::StructLiteral(struct_lit) => {
                // Convert struct literal to packed bit vector
                // Each field value is evaluated and packed into the correct bit positions
                self.convert_struct_literal(&struct_lit.type_name, &struct_lit.fields)
            }
            hir::HirExpression::TupleLiteral(elements) => {
                println!(
                    "🟣🟣🟣 [BUG #187] convert_expression: TupleLiteral case MATCHED with {} elements 🟣🟣🟣",
                    elements.len()
                );
                // Convert tuple literal to an anonymous struct
                // Tuple (a, b, c) becomes struct { _0: typeof(a), _1: typeof(b), _2: typeof(c) }
                // This is then packed into a bit vector like any other struct
                self.convert_tuple_literal(elements)
            }
            hir::HirExpression::ArrayLiteral(elements) => {
                // Convert array literal [a, b, c] to concatenation of elements
                // The elements are concatenated in order: {a, b, c} in SystemVerilog
                self.convert_array_literal(elements)
            }
            hir::HirExpression::Block {
                statements,
                result_expr,
            } => {
                // Convert block expression: { stmt1; stmt2; result_expr }
                // Block expressions with statements cannot exist as pure expressions in hardware.
                // We hoist the statements to pending_statements, which will be emitted before
                // the parent assignment/statement.
                // Example: result = if cond { let x = f(); x } else { 0 }
                // Becomes: let x = f(); result = if cond { x } else { 0 }
                println!(
                    "🟣🟣🟣 [DEBUG] Block expression: {} statements, result_expr type: {:?} 🟣🟣🟣",
                    statements.len(),
                    std::mem::discriminant(&**result_expr)
                );

                // BUG #86 FIX: Try to transform mutable variable pattern in block expressions
                // This handles patterns like:
                //   { let mut count = 0; let mut temp = value;
                //     if (cond) { count = count + 16; temp = temp << 16; }
                //     count }
                // Transform to conditional expressions BEFORE processing statements individually.
                debug_println!("🔵🔵🔵 CALLSITE1: About to call try_transform from convert_expression Block 🔵🔵🔵");
                if let Some(transformed_expr) =
                    self.try_transform_block_mutable_vars(statements, result_expr)
                {
                    debug_println!(
                        "[BUG #86] Block expression: transformed mutable variable pattern"
                    );
                    return self.convert_expression(&transformed_expr, depth + 1);
                }

                for stmt in statements.iter() {
                    if let Some(mir_stmt) = self.convert_statement(stmt) {
                        self.pending_statements.push(mir_stmt);
                    }
                }
                // Convert and return the final expression
                self.convert_expression(result_expr, depth + 1)
            }
        }
    }

    /// Convert struct literal to packed bit vector expression
    fn convert_struct_literal(
        &mut self,
        type_name: &str,
        fields: &[hir::HirStructFieldInit],
    ) -> Option<Expression> {
        eprintln!(
            "[DEBUG] convert_struct_literal: type={}, {} fields",
            type_name,
            fields.len()
        );
        for (i, f) in fields.iter().enumerate() {
            eprintln!("  [{}] field: {}", i, f.name);
        }
        // Handle built-in vector types (vec2, vec3, vec4) specially
        // These use struct literal syntax: vec3 { x: a, y: b, z: c }
        // But they're built-in types, not user-defined structs
        if type_name == "vec2" || type_name == "vec3" || type_name == "vec4" {
            // For vec2/vec3/vec4, concatenate fields in order: { x, y, z }
            // Standard field order: x, y, z, w
            let field_order = ["x", "y", "z", "w"];
            let num_fields = match type_name {
                "vec2" => 2,
                "vec3" => 3,
                "vec4" => 4,
                _ => unreachable!(),
            };

            let mut field_exprs = Vec::new();
            for &field_name in field_order.iter().take(num_fields) {
                eprintln!(
                    "[DEBUG] convert_struct_literal: looking for field '{}'",
                    field_name
                );
                let field_init = fields.iter().find(|f| f.name == field_name);
                if field_init.is_none() {
                    eprintln!(
                        "[DEBUG] convert_struct_literal: field '{}' NOT FOUND, returning None",
                        field_name
                    );
                    return None;
                }
                let field_init = field_init.unwrap();
                let field_expr = self.convert_expression(&field_init.value, 0)?;
                field_exprs.push(field_expr);
            }

            eprintln!(
                "[DEBUG] convert_struct_literal: SUCCESS, converted {} fields",
                field_exprs.len()
            );
            // Concatenate all fields: { x, y, z } becomes {x, y, z}
            return Some(Expression::with_unknown_type(ExpressionKind::Concat(
                field_exprs,
            )));
        }

        // Find the struct type definition to get field layout
        let hir = self.hir?;
        let mut struct_type: Option<&hir::HirStructType> = None;

        // Search for struct type in entities and implementations
        for entity in &hir.entities {
            for port in &entity.ports {
                if let hir::HirType::Struct(ref st) = &port.port_type {
                    if st.name == type_name {
                        struct_type = Some(st);
                        break;
                    }
                }
            }
        }

        if struct_type.is_none() {
            for impl_block in &hir.implementations {
                for signal in &impl_block.signals {
                    if let hir::HirType::Struct(ref st) = &signal.signal_type {
                        if st.name == type_name {
                            struct_type = Some(st);
                            break;
                        }
                    }
                }
            }
        }

        // BUG #173 FIX: Search in user_defined_types for top-level struct declarations
        if struct_type.is_none() {
            for udt in &hir.user_defined_types {
                if let hir::HirType::Struct(ref st) = &udt.type_def {
                    if st.name == type_name {
                        struct_type = Some(st);
                        eprintln!(
                            "[DEBUG] convert_struct_literal: found struct '{}' in user_defined_types",
                            type_name
                        );
                        break;
                    }
                }
            }
        }

        let struct_def = struct_type?;

        // Convert field values and pack them into a bit vector
        // For now, create a binary expression that concatenates the field values
        // In a real implementation, we'd pack these properly based on bit offsets

        // Collect field expressions in order
        let mut field_exprs = Vec::new();
        for struct_field in &struct_def.fields {
            // Find the corresponding field init
            let field_init = fields.iter().find(|f| f.name == struct_field.name)?;
            let field_expr = self.convert_expression(&field_init.value, 0)?;
            field_exprs.push(field_expr);
        }

        // BUG #173 FIX: Use Concat for multi-field structs
        // Struct fields are packed in declaration order, similar to tuple literals
        // Single-field struct: return that expression directly
        // Multi-field struct: concatenate all fields
        if field_exprs.is_empty() {
            return None;
        }

        if field_exprs.len() == 1 {
            Some(field_exprs.into_iter().next().unwrap())
        } else {
            // BUG #173 FIX: Reverse field order for Verilog concat semantics
            // In Verilog, {a, b, c} places 'a' at MSB and 'c' at LSB
            // But struct field access expects first field at LSB
            // So struct { a, b, c } should become {c, b, a} in Verilog
            // This way: field_a is at bits [0:width_a), field_b at [width_a:width_a+width_b), etc.
            let mut reversed_exprs = field_exprs;
            reversed_exprs.reverse();
            eprintln!(
                "[DEBUG] convert_struct_literal: creating Concat with {} fields (reversed) for '{}'",
                reversed_exprs.len(),
                type_name
            );
            Some(Expression::with_unknown_type(ExpressionKind::Concat(
                reversed_exprs,
            )))
        }
    }

    /// Convert tuple literal to packed bit vector expression
    /// Tuples are lowered to anonymous structs with fields named _0, _1, _2, etc.
    /// The tuple elements are concatenated together into a single packed bit vector.
    fn convert_tuple_literal(&mut self, elements: &[hir::HirExpression]) -> Option<Expression> {
        println!(
            "🔵🔵🔵 [BUG #187] convert_tuple_literal called with {} elements 🔵🔵🔵",
            elements.len()
        );
        println!(
            "🔵🔵🔵 [BUG #187] inlining_return_type_stack.len() = {} 🔵🔵🔵",
            self.inlining_return_type_stack.len()
        );
        if let Some(last) = self.inlining_return_type_stack.last() {
            println!("🔵🔵🔵 [BUG #187] Stack top: {:?} 🔵🔵🔵", last);
        }
        // BUG #187 FIX: Check if we're inside function inlining with a Tuple return type
        // If so, use the element types to annotate each converted element with the correct width
        println!("🔵🔵🔵 [BUG #187] About to check return type stack for Tuple 🔵🔵🔵");
        let element_types: Option<Vec<hir::HirType>> = if let Some(Some(hir::HirType::Tuple(
            types,
        ))) =
            self.inlining_return_type_stack.last()
        {
            println!(
                "🔵🔵🔵 [BUG #187] MATCHED Tuple with {} types! 🔵🔵🔵",
                types.len()
            );
            if types.len() == elements.len() {
                eprintln!(
                    "[BUG #187 FIX] TupleLiteral: Using {} element types from return type stack",
                    types.len()
                );
                Some(types.clone())
            } else {
                eprintln!(
                    "[BUG #187] TupleLiteral: Return type has {} elements but literal has {}",
                    types.len(),
                    elements.len()
                );
                None
            }
        } else {
            None
        };

        // Convert each tuple element expression
        let mut element_exprs = Vec::new();
        for (i, element) in elements.iter().enumerate() {
            let element_expr = self.convert_expression(element, 0)?;

            // BUG #187 FIX: If we know the expected element type, annotate the expression
            let annotated_expr = if let Some(ref types) = element_types {
                if let Some(hir_type) = types.get(i) {
                    let mir_type = self.hir_type_to_type(hir_type);
                    eprintln!(
                        "[BUG #187 FIX] TupleLiteral element {}: annotating with type {:?}",
                        i, mir_type
                    );
                    self.annotate_expression_with_type(element_expr, mir_type, 0)
                } else {
                    element_expr
                }
            } else {
                element_expr
            };

            element_exprs.push(annotated_expr);
        }

        // Handle empty tuple
        if element_exprs.is_empty() {
            return None;
        }

        // Single-element tuple - just return the element
        if element_exprs.len() == 1 {
            return Some(element_exprs.into_iter().next().unwrap());
        }

        // Multi-element tuple - concatenate all elements
        // Elements are packed from left to right: (a, b, c) becomes {a, b, c}
        // In bit representation: MSB is 'a', LSB is 'c'
        Some(Expression::with_unknown_type(ExpressionKind::Concat(
            element_exprs,
        )))
    }

    /// Convert array literal to concatenation expression
    fn convert_array_literal(&mut self, elements: &[hir::HirExpression]) -> Option<Expression> {
        // Convert each array element expression
        let mut element_exprs = Vec::new();
        for element in elements {
            let element_expr = self.convert_expression(element, 0)?;
            element_exprs.push(element_expr);
        }

        // Handle empty array
        if element_exprs.is_empty() {
            return None;
        }

        // Single-element array - still create a concat to maintain array semantics
        // [x] is different from x in type context
        if element_exprs.len() == 1 {
            return Some(element_exprs.into_iter().next().unwrap());
        }

        // Multi-element array - concatenate all elements
        // Array elements are packed from left to right: [a, b, c] becomes {a, b, c}
        // In SystemVerilog: element 0 is at MSB, element N-1 is at LSB
        Some(Expression::with_unknown_type(ExpressionKind::Concat(
            element_exprs,
        )))
    }

    /// Convert match expression to nested conditional expressions
    fn convert_match_to_conditionals(
        &mut self,
        match_value: &hir::HirExpression,
        arms: &[hir::HirMatchArmExpr],
    ) -> Option<Expression> {
        if arms.is_empty() {
            return None;
        }

        // Convert the match value expression ONCE to avoid exponential blowup
        // when we create N comparisons against it
        let match_value_expr = self.convert_expression(match_value, 0)?;

        // Build nested conditionals from right to left
        // Start with the last arm as the default (usually wildcard)

        // BUG FIX #6: Use globally unique match ID instead of arm index to prevent
        // variable name collisions when multiple functions with match expressions
        // are inlined into the same entity
        let match_id = self.next_match_id;
        self.next_match_id += 1;

        // Set match arm prefix for the last arm to isolate its variables
        let last_arm_idx = arms.len() - 1;
        let last_arm_prefix = format!("match_{}_{}", match_id, last_arm_idx);
        self.match_arm_prefix = Some(last_arm_prefix.clone());
        // BUG #198 FIX: Also set entity_instance_prefix which persists through function inlining
        self.entity_instance_prefix = Some(last_arm_prefix);

        let last_expr = self.convert_expression(&arms.last()?.expr, 0);

        // Clear the prefix after processing
        self.match_arm_prefix = None;
        self.entity_instance_prefix = None;

        let mut result = last_expr?;

        // Work backwards through the arms (excluding the last one which is the default)
        for (arm_idx, arm) in arms[..arms.len() - 1].iter().enumerate().rev() {
            // Build condition: match_value == pattern
            let condition = match &arm.pattern {
                hir::HirPattern::Literal(lit) => {
                    // Compare match_value with literal
                    // Clone the pre-converted match value instead of re-converting
                    let left = Box::new(match_value_expr.clone());
                    let right = Box::new(Expression::with_unknown_type(ExpressionKind::Literal(
                        self.convert_literal(lit)?,
                    )));
                    Some(Expression::with_unknown_type(ExpressionKind::Binary {
                        op: BinaryOp::Equal,
                        left,
                        right,
                    }))
                }
                hir::HirPattern::Wildcard => {
                    // Wildcard always matches - shouldn't appear except as last arm
                    // Skip it
                    None
                }
                hir::HirPattern::Path(enum_name, variant) => {
                    eprintln!(
                        "[DEBUG] HirPattern::Path matched: {}::{}",
                        enum_name, variant
                    );
                    // BUG #33 FIX: Check if this is a constant pattern marked with "__CONST__"
                    if enum_name == "__CONST__" {
                        // This is a constant reference - resolve it to its value
                        if let Some(const_value) = self.resolve_constant_value(variant) {
                            let left = Box::new(match_value_expr.clone());
                            let right = Box::new(const_value);
                            Some(Expression::with_unknown_type(ExpressionKind::Binary {
                                op: BinaryOp::Equal,
                                left,
                                right,
                            }))
                        } else {
                            None
                        }
                    } else {
                        // Regular enum pattern - resolve to the enum variant value
                        eprintln!(
                            "[DEBUG] Calling resolve_enum_variant_value for {}::{}",
                            enum_name, variant
                        );
                        if let Some(variant_value) =
                            self.resolve_enum_variant_value(enum_name, variant)
                        {
                            let left = Box::new(match_value_expr.clone());
                            let right = Box::new(variant_value);
                            Some(Expression::with_unknown_type(ExpressionKind::Binary {
                                op: BinaryOp::Equal,
                                left,
                                right,
                            }))
                        } else {
                            eprintln!(
                                "[DEBUG] Match: failed to resolve enum variant {}::{}",
                                enum_name, variant
                            );
                            None
                        }
                    }
                }
                _ => {
                    // For other patterns, we'll need more complex logic
                    // For now, skip them
                    None
                }
            };

            // Skip if we couldn't build a condition
            let condition = match condition {
                Some(c) => c,
                None => continue,
            };

            // Apply guard if present
            let final_condition = if let Some(guard) = &arm.guard {
                let guard_expr = Box::new(self.convert_expression(guard, 0)?);
                let cond_expr = Box::new(condition);
                Expression::with_unknown_type(ExpressionKind::Binary {
                    op: BinaryOp::LogicalAnd,
                    left: cond_expr,
                    right: guard_expr,
                })
            } else {
                condition
            };

            // Build conditional: (condition) ? arm_expr : rest

            // BUG FIX #6: Use global match ID with arm index to make prefix unique
            let arm_prefix = format!("match_{}_{}", match_id, arm_idx);
            self.match_arm_prefix = Some(arm_prefix.clone());
            // BUG #198 FIX: Also set entity_instance_prefix which persists through function inlining
            self.entity_instance_prefix = Some(arm_prefix);

            let arm_expr = self.convert_expression(&arm.expr, 0);

            // Clear the prefix after processing this arm
            self.match_arm_prefix = None;
            self.entity_instance_prefix = None;

            // If arm conversion fails, abort the entire match (returning None will trigger error handling upstream)
            let arm_expr = arm_expr?;
            result = Expression::with_unknown_type(ExpressionKind::Conditional {
                cond: Box::new(final_condition),
                then_expr: Box::new(arm_expr),
                else_expr: Box::new(result),
            });
        }

        Some(result)
    }

    /// Convert match expression to parallel mux (OR of ANDs)
    /// Generates: ({W{sel==0}} & a) | ({W{sel==1}} & b) | ...
    /// This produces a one-hot mux with shorter critical path but requires mutually exclusive conditions
    fn convert_match_to_parallel_mux(
        &mut self,
        match_value: &hir::HirExpression,
        arms: &[hir::HirMatchArmExpr],
    ) -> Option<Expression> {
        if arms.is_empty() {
            return None;
        }

        eprintln!(
            "[PARALLEL_MUX] Converting {} arms to OR-of-ANDs",
            arms.len()
        );

        // Convert the match value expression once
        let match_value_expr = self.convert_expression(match_value, 0)?;

        // Collect all (condition, value) pairs
        let mut terms: Vec<Expression> = Vec::new();
        let mut default_value: Option<Expression> = None;

        for arm in arms.iter() {
            match &arm.pattern {
                hir::HirPattern::Wildcard => {
                    // Wildcard is the default - save it for the final OR
                    default_value = self.convert_expression(&arm.expr, 0);
                }
                hir::HirPattern::Literal(lit) => {
                    // Build: (sel == lit) ? value : 0 -> ({W{sel==lit}} & value)
                    let condition = Expression::with_unknown_type(ExpressionKind::Binary {
                        op: BinaryOp::Equal,
                        left: Box::new(match_value_expr.clone()),
                        right: Box::new(Expression::with_unknown_type(ExpressionKind::Literal(
                            self.convert_literal(lit)?,
                        ))),
                    });
                    let value = self.convert_expression(&arm.expr, 0)?;

                    // Build: condition ? value : 0
                    // For synthesis, this becomes ({W{condition}} & value) after optimization
                    // We use conditional here and let codegen/synthesis optimize to AND-mask
                    let term = Expression::with_unknown_type(ExpressionKind::Conditional {
                        cond: Box::new(condition),
                        then_expr: Box::new(value),
                        else_expr: Box::new(Expression::with_unknown_type(
                            ExpressionKind::Literal(Value::Integer(0)),
                        )),
                    });
                    terms.push(term);
                }
                hir::HirPattern::Path(enum_name, variant) => {
                    // Handle enum/constant patterns
                    let pattern_value = if enum_name == "__CONST__" {
                        self.resolve_constant_value(variant)?
                    } else {
                        self.resolve_enum_variant_value(enum_name, variant)?
                    };

                    let condition = Expression::with_unknown_type(ExpressionKind::Binary {
                        op: BinaryOp::Equal,
                        left: Box::new(match_value_expr.clone()),
                        right: Box::new(pattern_value),
                    });
                    let value = self.convert_expression(&arm.expr, 0)?;

                    let term = Expression::with_unknown_type(ExpressionKind::Conditional {
                        cond: Box::new(condition),
                        then_expr: Box::new(value),
                        else_expr: Box::new(Expression::with_unknown_type(
                            ExpressionKind::Literal(Value::Integer(0)),
                        )),
                    });
                    terms.push(term);
                }
                _ => {
                    // Unsupported pattern for parallel mux - fall back to priority
                    eprintln!(
                        "[PARALLEL_MUX] Unsupported pattern {:?}, falling back to priority",
                        arm.pattern
                    );
                    return self.convert_match_to_conditionals(match_value, arms);
                }
            }
        }

        // Build the OR tree: term0 | term1 | term2 | ...
        if terms.is_empty() {
            return default_value;
        }

        let mut result = terms.remove(0);
        for term in terms {
            result = Expression::with_unknown_type(ExpressionKind::Binary {
                op: BinaryOp::Or,
                left: Box::new(result),
                right: Box::new(term),
            });
        }

        // If there's a default, OR it in (handles the "none of the above" case)
        // For truly one-hot selectors, default should never be reached
        if let Some(default) = default_value {
            // For parallel mux, we include the default as another OR term
            // In theory, if conditions are mutually exclusive and exhaustive, default is unreachable
            // But we include it for safety and to handle partial matches
            result = Expression::with_unknown_type(ExpressionKind::Binary {
                op: BinaryOp::Or,
                left: Box::new(result),
                right: Box::new(default),
            });
        }

        debug_println!("[PARALLEL_MUX] Generated OR-of-ANDs expression");
        Some(result)
    }

    /// Convert match expression to parallel one-hot mux (OR-of-ANDs) in module synthesis context
    /// This is the module context version of convert_match_to_parallel_mux
    fn convert_match_to_parallel_mux_for_module(
        &mut self,
        match_value: &hir::HirExpression,
        arms: &[hir::HirMatchArmExpr],
        ctx: &ModuleSynthesisContext,
        depth: usize,
    ) -> Option<Expression> {
        if arms.is_empty() {
            return None;
        }

        println!(
            "🔴🔴🔴 [PARALLEL_MUX_MODULE] ENTERED: Converting {} arms to OR-of-ANDs 🔴🔴🔴",
            arms.len()
        );
        eprintln!(
            "[PARALLEL_MUX_MODULE] Converting {} arms to OR-of-ANDs (module context)",
            arms.len()
        );

        // Convert the match value expression in module context
        let match_value_expr = self.convert_hir_expr_for_module(match_value, ctx, depth)?;
        debug_println!("🔴🔴🔴 [PARALLEL_MUX_MODULE] match_value_expr converted 🔴🔴🔴");

        // Find the default arm (wildcard pattern)
        let mut default_value: Option<Expression> = None;
        let non_default_arms: Vec<_> = arms
            .iter()
            .filter(|arm| {
                if matches!(arm.pattern, hir::HirPattern::Wildcard) {
                    default_value = self.convert_hir_expr_for_module(&arm.expr, ctx, depth);
                    false
                } else {
                    true
                }
            })
            .collect();

        // If no non-default arms, just return the default
        if non_default_arms.is_empty() {
            return default_value;
        }

        // Build OR-of-ANDs: (c1 ? v1 : 0) | (c2 ? v2 : 0) | ...
        // Each arm becomes: condition ? value : 0
        // BUG #125 FIX (v2): First collect all arm values, find max width, then create conditionals
        // with zeros at max width. This ensures all arms contribute their full value to the OR chain.

        // First pass: collect all arm data (conditions, values, types) and compute max width
        struct ArmData {
            condition: Expression,
            value: Expression,
            width: usize,
        }
        let mut arm_data_list: Vec<ArmData> = Vec::new();
        let mut max_width: usize = 0;

        for (arm_idx, arm) in non_default_arms.iter().enumerate() {
            println!(
                "🔴🔴🔴 [PARALLEL_MUX_MODULE] Pass 1: Processing arm {} pattern={:?} 🔴🔴🔴",
                arm_idx, arm.pattern
            );
            // Build condition: match_value == pattern
            let condition = match &arm.pattern {
                hir::HirPattern::Literal(lit) => {
                    let left = Box::new(match_value_expr.clone());
                    let right = Box::new(Expression::with_unknown_type(ExpressionKind::Literal(
                        self.convert_literal(lit)?,
                    )));
                    Some(Expression::with_unknown_type(ExpressionKind::Binary {
                        op: BinaryOp::Equal,
                        left,
                        right,
                    }))
                }
                hir::HirPattern::Path(enum_name, variant) => {
                    if enum_name == "__CONST__" {
                        if let Some(const_value) = self.resolve_constant_value(variant) {
                            let left = Box::new(match_value_expr.clone());
                            let right = Box::new(const_value);
                            Some(Expression::with_unknown_type(ExpressionKind::Binary {
                                op: BinaryOp::Equal,
                                left,
                                right,
                            }))
                        } else {
                            None
                        }
                    } else if let Some(variant_value) =
                        self.resolve_enum_variant_value(enum_name, variant)
                    {
                        let left = Box::new(match_value_expr.clone());
                        let right = Box::new(variant_value);
                        Some(Expression::with_unknown_type(ExpressionKind::Binary {
                            op: BinaryOp::Equal,
                            left,
                            right,
                        }))
                    } else {
                        None
                    }
                }
                hir::HirPattern::Wildcard => None, // Shouldn't happen - filtered out
                _ => None,
            };

            // Skip if we couldn't build a condition
            let condition = match condition {
                Some(c) => c,
                None => continue,
            };

            // Apply guard if present
            let final_condition = if let Some(guard) = &arm.guard {
                let guard_expr = Box::new(self.convert_hir_expr_for_module(guard, ctx, depth)?);
                let cond_expr = Box::new(condition);
                Expression::with_unknown_type(ExpressionKind::Binary {
                    op: BinaryOp::LogicalAnd,
                    left: cond_expr,
                    right: guard_expr,
                })
            } else {
                condition
            };

            // Convert arm body in module context
            let arm_value = self.convert_hir_expr_for_module(&arm.expr, ctx, depth)?;

            // Infer arm width
            let arm_width = if arm_value.ty == Type::Unknown {
                let inferred = self.infer_expression_type(&arm_value);
                match inferred {
                    DataType::Bit(w) | DataType::Nat(w) | DataType::Int(w) => w,
                    _ => 32, // Default fallback
                }
            } else {
                match &arm_value.ty {
                    Type::Bit(w) | Type::Nat(w) | Type::Int(w) => match w {
                        skalp_frontend::types::Width::Fixed(n) => *n as usize,
                        _ => 32,
                    },
                    _ => 32,
                }
            };

            println!(
                "🔴🔴🔴 [PARALLEL_MUX_MODULE] Pass 1: Arm {} width={} 🔴🔴🔴",
                arm_idx, arm_width
            );
            max_width = max_width.max(arm_width);

            arm_data_list.push(ArmData {
                condition: final_condition,
                value: arm_value,
                width: arm_width,
            });
        }

        // Also check default value width
        if let Some(ref default) = default_value {
            let default_width = if default.ty == Type::Unknown {
                let inferred = self.infer_expression_type(default);
                match inferred {
                    DataType::Bit(w) | DataType::Nat(w) | DataType::Int(w) => w,
                    _ => 32,
                }
            } else {
                match &default.ty {
                    Type::Bit(w) | Type::Nat(w) | Type::Int(w) => match w {
                        skalp_frontend::types::Width::Fixed(n) => *n as usize,
                        _ => 32,
                    },
                    _ => 32,
                }
            };
            max_width = max_width.max(default_width);
            println!(
                "🔴🔴🔴 [PARALLEL_MUX_MODULE] Default arm width={}, max_width now={} 🔴🔴🔴",
                default_width, max_width
            );
        }

        println!(
            "🔴🔴🔴 [PARALLEL_MUX_MODULE] Final max_width={} across {} arms 🔴🔴🔴",
            max_width,
            arm_data_list.len()
        );

        // Create the unified type for the parallel mux result
        let unified_type = Type::Bit(skalp_frontend::types::Width::Fixed(max_width as u32));

        // Second pass: create conditionals with zeros at max width
        let mut result: Option<Expression> = None;

        for (arm_idx, arm_data) in arm_data_list.into_iter().enumerate() {
            debug_println!("🔴🔴🔴 [PARALLEL_MUX_MODULE] Pass 2: Arm {} creating conditional with max_width={} 🔴🔴🔴", arm_idx, max_width);

            // Create zero constant with MAX width (not arm-specific width)
            let zero = Expression::literal(Value::Integer(0), unified_type.clone());

            // Create: condition ? value : 0
            // Use unified_type so the mux output has max width
            let masked_value = Expression::conditional(
                arm_data.condition,
                arm_data.value,
                zero,
                unified_type.clone(),
            );

            // OR with accumulated result
            match result {
                None => result = Some(masked_value),
                Some(prev) => {
                    result = Some(Expression::with_unknown_type(ExpressionKind::Binary {
                        op: BinaryOp::Or,
                        left: Box::new(prev),
                        right: Box::new(masked_value),
                    }));
                }
            }
        }

        // If there's a default, OR it in
        if let Some(default) = default_value {
            if let Some(prev) = result {
                result = Some(Expression::with_unknown_type(ExpressionKind::Binary {
                    op: BinaryOp::Or,
                    left: Box::new(prev),
                    right: Box::new(default),
                }));
            } else {
                result = Some(default);
            }
        }

        debug_println!("[PARALLEL_MUX_MODULE] Generated OR-of-ANDs expression (module context)");
        result
    }

    /// BUG FIX #85: Convert match expression to conditionals while staying in module context
    /// This prevents duplicate module instances by NOT using the regular convert_expression path
    /// which triggers the HYBRID mechanism for function calls inside match arms.
    fn convert_match_to_conditionals_for_module(
        &mut self,
        match_value: &hir::HirExpression,
        arms: &[hir::HirMatchArmExpr],
        ctx: &ModuleSynthesisContext,
        depth: usize,
    ) -> Option<Expression> {
        if arms.is_empty() {
            return None;
        }

        println!(
            "🎯 MODULE_MATCH: Processing {} arms in module context",
            arms.len()
        );

        // Convert the match value expression in module context
        let match_value_expr = self.convert_hir_expr_for_module(match_value, ctx, depth)?;

        // Build nested conditionals from right to left
        // Start with the last arm as the default (usually wildcard)
        let last_arm = arms.last()?;
        println!("🎯 MODULE_MATCH: Converting last arm (default) body");
        let mut result = self.convert_hir_expr_for_module(&last_arm.expr, ctx, depth)?;

        // Work backwards through the arms (excluding the last one which is the default)
        for (arm_idx, arm) in arms[..arms.len() - 1].iter().enumerate().rev() {
            println!(
                "🎯 MODULE_MATCH: Converting arm {} pattern: {:?}",
                arm_idx, arm.pattern
            );

            // Build condition: match_value == pattern
            let condition = match &arm.pattern {
                hir::HirPattern::Literal(lit) => {
                    let left = Box::new(match_value_expr.clone());
                    let right = Box::new(Expression::with_unknown_type(ExpressionKind::Literal(
                        self.convert_literal(lit)?,
                    )));
                    Some(Expression::with_unknown_type(ExpressionKind::Binary {
                        op: BinaryOp::Equal,
                        left,
                        right,
                    }))
                }
                hir::HirPattern::Wildcard => None, // Wildcard always matches - shouldn't appear except as last arm
                hir::HirPattern::Path(enum_name, variant) => {
                    if enum_name == "__CONST__" {
                        if let Some(const_value) = self.resolve_constant_value(variant) {
                            let left = Box::new(match_value_expr.clone());
                            let right = Box::new(const_value);
                            Some(Expression::with_unknown_type(ExpressionKind::Binary {
                                op: BinaryOp::Equal,
                                left,
                                right,
                            }))
                        } else {
                            None
                        }
                    } else if let Some(variant_value) =
                        self.resolve_enum_variant_value(enum_name, variant)
                    {
                        let left = Box::new(match_value_expr.clone());
                        let right = Box::new(variant_value);
                        Some(Expression::with_unknown_type(ExpressionKind::Binary {
                            op: BinaryOp::Equal,
                            left,
                            right,
                        }))
                    } else {
                        None
                    }
                }
                _ => None,
            };

            // Skip if we couldn't build a condition
            let condition = match condition {
                Some(c) => c,
                None => continue,
            };

            // Apply guard if present (convert in module context)
            let final_condition = if let Some(guard) = &arm.guard {
                let guard_expr = Box::new(self.convert_hir_expr_for_module(guard, ctx, depth)?);
                let cond_expr = Box::new(condition);
                Expression::with_unknown_type(ExpressionKind::Binary {
                    op: BinaryOp::LogicalAnd,
                    left: cond_expr,
                    right: guard_expr,
                })
            } else {
                condition
            };

            // Convert arm body in module context (NOT using convert_expression!)
            println!(
                "🎯 MODULE_MATCH: Converting arm {} body in module context",
                arm_idx
            );
            // BUG FIX #189: If arm body conversion fails, skip this arm and continue
            // This allows processing of remaining arms even if some fail (e.g., due to
            // unresolved function calls like sqrt)
            let arm_expr = match self.convert_hir_expr_for_module(&arm.expr, ctx, depth) {
                Some(expr) => expr,
                None => continue, // Skip failed arm, continue with remaining arms
            };

            result = Expression::with_unknown_type(ExpressionKind::Conditional {
                cond: Box::new(final_condition),
                then_expr: Box::new(arm_expr),
                else_expr: Box::new(result),
            });
        }

        println!("🎯 MODULE_MATCH: Successfully converted match to nested conditionals");
        Some(result)
    }

    /// Convert literal expression (for initial values)
    ///
    /// BUG #158 FIX: Also handles Cast expressions that wrap literals.
    /// For example: `100 as bit[256]` should extract the value 100.
    fn convert_literal_expr(&mut self, expr: &hir::HirExpression) -> Option<Value> {
        match expr {
            hir::HirExpression::Literal(lit) => self.convert_literal(lit),
            // BUG #158 FIX: Handle Cast expressions wrapping literals
            // e.g., `100 as bit[256]` or `(-5.0 as fp32) as bit[32]`
            hir::HirExpression::Cast(cast_expr) => {
                // Recursively extract the literal value from the inner expression
                self.convert_literal_expr(&cast_expr.expr)
            }
            // BUG #158 FIX: Handle Unary expressions (for negative literals)
            // e.g., `-5.0` is Unary { op: Negate, operand: Literal(5.0) }
            hir::HirExpression::Unary(unary) => {
                if let Some(inner_val) = self.convert_literal_expr(&unary.operand) {
                    match (&unary.op, inner_val) {
                        (hir::HirUnaryOp::Negate, Value::Integer(i)) => Some(Value::Integer(-i)),
                        (hir::HirUnaryOp::Negate, Value::Float(f)) => Some(Value::Float(-f)),
                        (hir::HirUnaryOp::Not, Value::Integer(i)) => Some(Value::Integer(!i)),
                        _ => None,
                    }
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    /// Convert HIR literal to MIR value
    fn convert_literal(&mut self, lit: &hir::HirLiteral) -> Option<Value> {
        match lit {
            hir::HirLiteral::Integer(val) => Some(Value::Integer(*val as i64)),
            hir::HirLiteral::Boolean(b) => Some(Value::Integer(if *b { 1 } else { 0 })),
            hir::HirLiteral::Float(f) => Some(Value::Float(*f)),
            hir::HirLiteral::String(s) => Some(Value::String(s.clone())),
            hir::HirLiteral::BitVector(bits) => {
                // Convert vector of bools to integer value
                let mut value = 0u64;
                for (i, &bit) in bits.iter().enumerate() {
                    if bit {
                        value |= 1u64 << i;
                    }
                }
                Some(Value::BitVector {
                    width: bits.len(),
                    value,
                })
            }
        }
    }

    /// Convert const value to MIR expression
    fn const_value_to_mir_expression(&self, const_value: &ConstValue) -> Expression {
        match const_value {
            ConstValue::Nat(n) => {
                Expression::with_unknown_type(ExpressionKind::Literal(Value::Integer(*n as i64)))
            }
            ConstValue::Int(i) => {
                Expression::with_unknown_type(ExpressionKind::Literal(Value::Integer(*i)))
            }
            ConstValue::Bool(b) => {
                Expression::with_unknown_type(ExpressionKind::Literal(Value::Integer(if *b {
                    1
                } else {
                    0
                })))
            }
            ConstValue::Float(f) => {
                Expression::with_unknown_type(ExpressionKind::Literal(Value::Float(*f)))
            }
            ConstValue::String(s) => {
                Expression::with_unknown_type(ExpressionKind::Literal(Value::String(s.clone())))
            }
            // For complex types that don't have direct MIR equivalents, return 0 as fallback
            ConstValue::FloatFormat(_) | ConstValue::Struct(_) => {
                Expression::with_unknown_type(ExpressionKind::Literal(Value::Integer(0)))
            }
        }
    }

    /// Find a function by name in the current implementation or top-level functions
    fn find_function(&self, function_name: &str) -> Option<&hir::HirFunction> {
        println!("🚨🚨🚨 FIND_FUNCTION: {} 🚨🚨🚨", function_name);
        let hir = self.hir?;

        // BUG #21 FIX: Handle module-qualified function names (e.g., "imported_funcs::process_data")
        // The HIR stores only the simple function name (e.g., "process_data"), not the full path.
        // Extract the simple name by taking the last component after "::"
        let simple_name = function_name.rsplit("::").next().unwrap_or(function_name);

        // First, try to find in current implementation's functions
        if let Some(entity_id) = self.current_entity_id {
            if let Some(impl_block) = hir
                .implementations
                .iter()
                .find(|impl_b| impl_b.entity == entity_id)
            {
                for func in &impl_block.functions {
                    if func.name == simple_name {
                        return Some(func);
                    }
                }
            }
        }

        // Second, try top-level functions
        eprintln!(
            "[DEBUG find_function] Searching {} top-level functions for '{}'",
            hir.functions.len(),
            simple_name
        );
        if let Some(func) = hir.functions.iter().find(|func| func.name == simple_name) {
            eprintln!(
                "[DEBUG find_function] ✅ FOUND '{}' in top-level functions!",
                simple_name
            );
            return Some(func);
        }

        // BUG #83 FIX: Third, search ALL implementation blocks for imported stdlib functions
        // When functions are imported via "use bitops::*", they get merged into the global
        // implementation block (EntityId(0)). We need to search all impl blocks, not just
        // the current entity's block.
        eprintln!(
            "[DEBUG find_function] Searching {} implementation blocks in main HIR for '{}'",
            hir.implementations.len(),
            simple_name
        );
        for (impl_idx, impl_block) in hir.implementations.iter().enumerate() {
            eprintln!(
                "[DEBUG find_function]   Impl block {} (entity {:?}): {} functions",
                impl_idx,
                impl_block.entity,
                impl_block.functions.len()
            );
            for func in &impl_block.functions {
                if func.name == simple_name {
                    eprintln!(
                        "[DEBUG find_function] ✅ FOUND '{}' in main HIR impl block {}!",
                        simple_name, impl_idx
                    );
                    return Some(func);
                }
            }
        }

        // BUG #84 FIX: Fourth, search ALL module HIRs for proper transitive import support
        // When a function from module A calls a function from module B (which A imported),
        // we need to search B's HIR, not just the main HIR.
        eprintln!(
            "[DEBUG find_function] Searching {} module HIRs for '{}'",
            self.module_hirs.len(),
            simple_name
        );
        for (module_path, module_hir) in &self.module_hirs {
            eprintln!(
                "[DEBUG find_function]   Module {:?}: {} top-level functions, {} impl blocks",
                module_path.file_name().unwrap_or_default(),
                module_hir.functions.len(),
                module_hir.implementations.len()
            );

            // Search top-level functions in this module
            if let Some(func) = module_hir.functions.iter().find(|f| f.name == simple_name) {
                eprintln!(
                    "[DEBUG find_function] ✅ FOUND '{}' in module {:?} top-level functions!",
                    simple_name,
                    module_path.file_name().unwrap_or_default()
                );
                // SAFETY: We need to return a reference with lifetime 'hir, but module_hirs
                // is owned by self and lives as long as HirToMir, which lives as long as
                // the transform() call. This is safe.
                return unsafe {
                    std::mem::transmute::<&hir::HirFunction, Option<&hir::HirFunction>>(func)
                };
            }

            // Search implementation blocks in this module
            for impl_block in &module_hir.implementations {
                for func in &impl_block.functions {
                    if func.name == simple_name {
                        eprintln!(
                            "[DEBUG find_function] ✅ FOUND '{}' in module {:?} impl block!",
                            simple_name,
                            module_path.file_name().unwrap_or_default()
                        );
                        // SAFETY: Same as above
                        return unsafe {
                            std::mem::transmute::<&hir::HirFunction, Option<&hir::HirFunction>>(
                                func,
                            )
                        };
                    }
                }
            }
        }

        eprintln!(
            "[DEBUG find_function] ❌ NOT FOUND: '{}' after searching main HIR ({} impl blocks) and {} module HIRs",
            simple_name,
            hir.implementations.len(),
            self.module_hirs.len()
        );
        None
    }

    /// Transform early returns into nested if-else expressions
    ///
    /// Converts:
    ///   if cond { return val1; }
    ///   stmt2;
    ///   return val2;
    ///
    /// Into:
    ///   if cond { return val1; } else { stmt2; return val2; }
    fn transform_early_returns(&self, body: Vec<hir::HirStatement>) -> Vec<hir::HirStatement> {
        self.transform_early_returns_recursive(body)
    }

    #[allow(clippy::only_used_in_recursion)]
    fn transform_early_returns_recursive(
        &self,
        body: Vec<hir::HirStatement>,
    ) -> Vec<hir::HirStatement> {
        if body.is_empty() {
            return body;
        }

        // Find the first statement that's an If with a return in then-branch
        for (i, stmt) in body.iter().enumerate() {
            if let hir::HirStatement::If(if_stmt) = stmt {
                // Check if then-branch ends with a return
                if let Some(last) = if_stmt.then_statements.last() {
                    if matches!(last, hir::HirStatement::Return(_)) {
                        // Found early return pattern!
                        // Collect all statements before this if
                        let mut result = body[..i].to_vec();

                        // Transform this if statement
                        let remaining_stmts = body[i + 1..].to_vec();

                        let new_if = if remaining_stmts.is_empty() {
                            // No statements after, keep as-is but recursively transform branches
                            hir::HirStatement::If(hir::HirIfStatement {
                                condition: if_stmt.condition.clone(),
                                then_statements: self.transform_early_returns_recursive(
                                    if_stmt.then_statements.clone(),
                                ),
                                else_statements: if_stmt.else_statements.as_ref().map(|stmts| {
                                    self.transform_early_returns_recursive(stmts.clone())
                                }),
                                mux_style: if_stmt.mux_style,
                            })
                        } else {
                            // Move remaining statements into else-branch
                            let else_body = self.transform_early_returns_recursive(remaining_stmts);

                            hir::HirStatement::If(hir::HirIfStatement {
                                condition: if_stmt.condition.clone(),
                                then_statements: self.transform_early_returns_recursive(
                                    if_stmt.then_statements.clone(),
                                ),
                                else_statements: Some(else_body),
                                mux_style: if_stmt.mux_style,
                            })
                        };

                        result.push(new_if);
                        return result;
                    }
                }
            }
        }

        // No early returns found, recursively transform nested structures
        body.into_iter()
            .map(|stmt| match stmt {
                hir::HirStatement::If(if_stmt) => hir::HirStatement::If(hir::HirIfStatement {
                    condition: if_stmt.condition,
                    then_statements: self
                        .transform_early_returns_recursive(if_stmt.then_statements),
                    else_statements: if_stmt
                        .else_statements
                        .map(|stmts| self.transform_early_returns_recursive(stmts)),
                    mux_style: if_stmt.mux_style,
                }),
                hir::HirStatement::Block(stmts) => {
                    hir::HirStatement::Block(self.transform_early_returns_recursive(stmts))
                }
                // TODO: Handle Match statements with early returns
                other => other,
            })
            .collect()
    }

    /// Convert a statement-based function body (possibly with if-returns) into an expression
    /// This handles the case where early returns have been transformed into nested if-else
    fn convert_body_to_expression(&self, body: &[hir::HirStatement]) -> Option<hir::HirExpression> {
        if body.is_empty() {
            debug_println!("[DEBUG] convert_body_to_expression: empty body");
            return None;
        }

        eprintln!(
            "[DEBUG] convert_body_to_expression: {} statements",
            body.len()
        );
        for (i, stmt) in body.iter().enumerate() {
            debug_println!("[DEBUG]   stmt[{}]: {:?}", i, std::mem::discriminant(stmt));
        }

        // Collect all let bindings
        let mut let_bindings = Vec::new();
        let mut remaining_stmts = body;
        let mut seen_var_ids: std::collections::HashSet<hir::VariableId> =
            std::collections::HashSet::new();

        // Extract leading let statements AND assignment-based initializations (BUG #135 FIX)
        loop {
            println!(
                "[BUG #135 TRACE] remaining_stmts.len()={}, seen_var_ids={:?}",
                remaining_stmts.len(),
                seen_var_ids
            );
            match remaining_stmts.first() {
                Some(hir::HirStatement::Let(let_stmt)) => {
                    println!(
                        "[BUG #135 TRACE] Found Let for '{}'  id={:?}",
                        let_stmt.name, let_stmt.id
                    );
                    seen_var_ids.insert(let_stmt.id);
                    let_bindings.push(let_stmt.clone());
                    remaining_stmts = &remaining_stmts[1..];
                }
                // BUG #135 FIX: Also extract Assignments that initialize new variables
                // These appear when `let mut val = x;` is converted to Assignment(val, x) during compilation
                Some(hir::HirStatement::Assignment(assign)) => {
                    debug_println!("[BUG #135 TRACE] Found Assignment to {:?}", assign.lhs);
                    if let hir::HirLValue::Variable(var_id) = &assign.lhs {
                        println!(
                            "[BUG #135 TRACE]   -> Variable {:?}, in_seen={}",
                            var_id,
                            seen_var_ids.contains(var_id)
                        );
                        if !seen_var_ids.contains(var_id) {
                            // This is a new variable being initialized via assignment
                            // Convert it to a pseudo-let binding
                            println!(
                                "[BUG #135 FIX] Converting Assignment to init Let for var {:?}",
                                var_id
                            );
                            seen_var_ids.insert(*var_id);
                            // Use a placeholder type - typically bit[32] for bit operations
                            // The actual type will be inferred from usage during downstream processing
                            let_bindings.push(hir::HirLetStatement {
                                id: *var_id,
                                name: format!("__init_var_{}", var_id.0),
                                mutable: true, // Assume mutable since it was an assignment
                                var_type: hir::HirType::Bit(32), // Placeholder type
                                value: assign.rhs.clone(),
                            });
                            remaining_stmts = &remaining_stmts[1..];
                            continue;
                        }
                    }
                    // Assignment to existing variable - stop extraction
                    debug_println!("[BUG #135 TRACE] Breaking: Assignment to existing var");
                    break;
                }
                Some(other) => {
                    println!(
                        "[BUG #135 TRACE] Found Other: {:?}",
                        std::mem::discriminant(other)
                    );
                    break;
                }
                None => {
                    debug_println!("[BUG #135 TRACE] No more statements");
                    break;
                }
            }
        }

        println!(
            "[DEBUG] convert_body_to_expression: {} let bindings, {} remaining stmts",
            let_bindings.len(),
            remaining_stmts.len()
        );

        // BUG #135 DEBUG: Print what stopped the let extraction
        if !remaining_stmts.is_empty() && let_bindings.len() < 3 {
            let first_non_let = &remaining_stmts[0];
            eprintln!(
                "[BUG #135 DEBUG] After {} lets extracted, first non-let statement type: {:?}",
                let_bindings.len(),
                std::mem::discriminant(first_non_let)
            );
            match first_non_let {
                hir::HirStatement::Assignment(assign) => {
                    debug_println!("[BUG #135 DEBUG]   -> Assignment to {:?}", assign.lhs);
                }
                hir::HirStatement::Let(let_stmt) => {
                    eprintln!(
                        "[BUG #135 DEBUG]   -> Let '{}' (WHY NOT EXTRACTED??)",
                        let_stmt.name
                    );
                }
                hir::HirStatement::If(_) => {
                    debug_println!("[BUG #135 DEBUG]   -> If statement");
                }
                _ => {
                    debug_println!("[BUG #135 DEBUG]   -> Other statement type");
                }
            }
        }

        if remaining_stmts.is_empty() {
            // All statements are let bindings - check for implicit return
            // If the last let binding's value is an expression without semicolon,
            // treat it as an implicit return
            if let Some(last_let) = let_bindings.last() {
                eprintln!(
                    "[DEBUG] convert_body_to_expression: treating last let '{}' as implicit return",
                    last_let.name
                );
                // Take all but the last let binding for the block,
                // and use the last let's value as the return expression
                let block_stmts: Vec<_> = let_bindings
                    .iter()
                    .take(let_bindings.len() - 1)
                    .map(|let_stmt| hir::HirStatement::Let(let_stmt.clone()))
                    .collect();

                if block_stmts.is_empty() {
                    // Only one let binding - return its value directly
                    return Some(last_let.value.clone());
                } else {
                    // Multiple let bindings - wrap in block
                    return Some(hir::HirExpression::Block {
                        statements: block_stmts,
                        result_expr: Box::new(last_let.value.clone()),
                    });
                }
            }
            eprintln!(
                "[DEBUG] convert_body_to_expression: no remaining statements and no let bindings!"
            );
            return None;
        }

        // Convert the remaining statements to an expression
        println!(
            "[DEBUG] About to match remaining_stmts: {} stmts",
            remaining_stmts.len()
        );
        if !remaining_stmts.is_empty() {
            println!(
                "[DEBUG]   First stmt type: {:?}",
                std::mem::discriminant(&remaining_stmts[0])
            );
        }
        let body_expr = match remaining_stmts {
            // Single return statement with expression
            [hir::HirStatement::Return(Some(expr))] => Some(expr.clone()),

            // Single return statement without expression (void return or implicit unit)
            // In hardware context, this shouldn't happen for non-void functions
            [hir::HirStatement::Return(None)] => {
                eprintln!("Warning: Return statement with no expression in function body");
                eprintln!("  This may indicate an issue with HIR building");
                None
            }

            // If statement (possibly with returns in branches)
            [hir::HirStatement::If(if_stmt)] => self.convert_if_stmt_to_expr(if_stmt),

            // Match expression as final statement - convert to HIR match expression
            [hir::HirStatement::Match(match_stmt)] => {
                debug_println!("[DEBUG] HIT: Single Match statement pattern - calling convert_match_stmt_to_expr");
                self.convert_match_stmt_to_expr(match_stmt)
            }

            // Multiple statements - need to convert to block expression
            // This happens when function has statements followed by return
            stmts if stmts.len() > 1 => {
                println!("🟢🟢🟢 [MULTI_STMT] {} stmts, {} let bindings, trying mutable var pattern 🟢🟢🟢", stmts.len(), let_bindings.len());
                // BUG #135 DEBUG: Print first statement to understand why let extraction stopped
                if let_bindings.len() == 1 && !stmts.is_empty() {
                    println!(
                        "[BUG #135] First stmt after 1 let: {:?}",
                        std::mem::discriminant(&stmts[0])
                    );
                    match &stmts[0] {
                        hir::HirStatement::Let(ls) => println!(
                            "[BUG #135]   -> Let '{}' - why wasn't it extracted?",
                            ls.name
                        ),
                        hir::HirStatement::If(_) => debug_println!("[BUG #135]   -> If statement"),
                        hir::HirStatement::Assignment(a) => {
                            debug_println!("[BUG #135]   -> Assignment to {:?}", a.lhs)
                        }
                        _ => debug_println!("[BUG #135]   -> Other"),
                    }
                }
                // Check if last statement is a return
                if let Some(hir::HirStatement::Return(Some(return_expr))) = stmts.last() {
                    println!(
                        "🟢🟢🟢 [MULTI_STMT] Last is Return, return_expr type: {:?} 🟢🟢🟢",
                        std::mem::discriminant(return_expr)
                    );
                    // BUG #86 FIX: Handle mutable variable pattern
                    // Pattern: if (cond) { var = new_value; } return var
                    // Transform to: if cond { new_value } else { initial_value }
                    if let Some(mutable_var_expr) = self.try_convert_mutable_var_pattern(
                        &stmts[..stmts.len() - 1], // statements before return
                        return_expr,
                        &let_bindings,
                    ) {
                        println!("🟢🟢🟢 [MULTI_STMT] SUCCESS! Converted mutable variable pattern 🟢🟢🟢");
                        return Some(mutable_var_expr);
                    }

                    println!("🟢🟢🟢 [MULTI_STMT] FALLBACK! Creating Block expression 🟢🟢🟢");
                    // Build block with all statements except the last, then use return expr as result
                    let block_stmts: Vec<_> = stmts[..stmts.len() - 1].to_vec();
                    Some(hir::HirExpression::Block {
                        statements: block_stmts,
                        result_expr: Box::new(return_expr.clone()),
                    })
                }
                // BUG #86 FIX: Also handle implicit return (Expression statement at end)
                // This happens in match arm blocks like:
                //   { let mut count = 0; if (cond) { count = 16; } count }
                // The final `count` is an Expression statement, not Return
                else if let Some(hir::HirStatement::Expression(result_expr)) = stmts.last() {
                    debug_println!("[BUG #86] Multiple statements with final Expression - trying mutable var pattern");
                    if let Some(mutable_var_expr) = self.try_convert_mutable_var_pattern(
                        &stmts[..stmts.len() - 1], // statements before final expression
                        result_expr,
                        &let_bindings,
                    ) {
                        debug_println!("[BUG #86] Successfully converted mutable variable pattern (Expression) to if-expression");
                        return Some(mutable_var_expr);
                    }

                    // Build block with all statements except the last, then use expression as result
                    let block_stmts: Vec<_> = stmts[..stmts.len() - 1].to_vec();
                    Some(hir::HirExpression::Block {
                        statements: block_stmts,
                        result_expr: Box::new(result_expr.clone()),
                    })
                } else {
                    eprintln!(
                        "convert_body_to_expression: multiple statements without final return or expression"
                    );
                    eprintln!("  Statements: {:?}", stmts.len());
                    if let Some(last) = stmts.last() {
                        eprintln!("  Last statement type: {:?}", std::mem::discriminant(last));
                    }
                    None
                }
            }

            // Single expression statement (implicit return)
            [hir::HirStatement::Expression(expr)] => Some(expr.clone()),

            // Other cases not yet supported
            _ => {
                println!("convert_body_to_expression: unsupported statement pattern");
                println!("  Remaining statements: {:?}", remaining_stmts.len());
                if !remaining_stmts.is_empty() {
                    println!(
                        "  First statement type: {:?}",
                        std::mem::discriminant(&remaining_stmts[0])
                    );
                    match &remaining_stmts[0] {
                        hir::HirStatement::Return(opt) => {
                            println!("    Return statement with expr: {}", opt.is_some());
                        }
                        other => println!("    Statement variant: {:?}", other),
                    }
                }
                None
            }
        }?;

        // Wrap with let bindings if any
        if let_bindings.is_empty() {
            Some(body_expr)
        } else {
            Some(self.build_let_expression(let_bindings, body_expr))
        }
    }

    /// BUG #86 FIX: Try to convert mutable variable pattern to conditional expression
    ///
    /// Pattern: `let mut var = initial; if (cond) { var = new_value; } return var`
    /// Transforms to: `if cond { new_value } else { initial }`
    ///
    /// This handles functions that use mutable variables with conditional updates,
    /// which is common in algorithms like clz32, ctz32, popcount32, etc.
    ///
    /// For complex cases like clz32 with multiple mutable variables:
    /// ```ignore
    /// let mut count = 0;
    /// let mut temp = value;
    /// if (temp & 0xFFFF0000) == 0 { count = count + 16; temp = temp << 16; }
    /// if (temp & 0xFF000000) == 0 { count = count + 8;  temp = temp << 8; }
    /// ...
    /// return count
    /// ```
    ///
    /// We perform SSA-style conversion:
    /// 1. Track current expression for each mutable variable
    /// 2. Substitute variable references with current expressions
    /// 3. Create conditional expressions for ALL modified variables
    fn try_convert_mutable_var_pattern(
        &self,
        stmts_before_return: &[hir::HirStatement],
        return_expr: &hir::HirExpression,
        let_bindings: &[hir::HirLetStatement],
    ) -> Option<hir::HirExpression> {
        use indexmap::IndexMap;

        debug_println!("🔵🔵🔵 [BUG #86] try_convert_mutable_var_pattern: {} stmts before return, {} let bindings 🔵🔵🔵",
                 stmts_before_return.len(), let_bindings.len());
        println!(
            "🔵🔵🔵 [BUG #86] return_expr type: {:?} 🔵🔵🔵",
            std::mem::discriminant(return_expr)
        );

        // BUG #134 FIX: Check if return expression is a variable reference
        // Also handle Cast(Variable) which can happen after early return transformation
        let (returned_var_id, outer_cast) = match return_expr {
            hir::HirExpression::Variable(var_id) => (*var_id, None),
            hir::HirExpression::Cast(cast_expr) => {
                // Check if Cast wraps a Variable
                if let hir::HirExpression::Variable(var_id) = cast_expr.expr.as_ref() {
                    eprintln!(
                        "[BUG #134] return_expr is Cast(Variable), extracting inner variable"
                    );
                    (*var_id, Some(cast_expr.target_type.clone()))
                } else {
                    eprintln!(
                        "[BUG #134] return_expr is Cast but not Variable inside, type: {:?}",
                        std::mem::discriminant(cast_expr.expr.as_ref())
                    );
                    return None;
                }
            }
            _ => return None,
        };

        // Build a map from VariableId to current expression for ALL mutable variables
        let mut var_exprs: IndexMap<hir::VariableId, hir::HirExpression> = IndexMap::new();

        // Initialize with mutable bindings' initial values
        for let_stmt in let_bindings {
            if let_stmt.mutable {
                var_exprs.insert(let_stmt.id, let_stmt.value.clone());
            }
        }

        // BUG #135 FIX: Also extract leading Assignment statements as mutable variable initializations
        // These may appear when let statements are converted to assignments during compilation
        let mut init_assignment_count = 0;
        for stmt in stmts_before_return {
            match stmt {
                hir::HirStatement::Assignment(assign) => {
                    // Check if this is initializing a variable (simple Variable LHS)
                    if let hir::HirLValue::Variable(var_id) = &assign.lhs {
                        eprintln!(
                            "[BUG #135 FIX] Checking Assignment to {:?}, var_exprs has {} entries",
                            var_id,
                            var_exprs.len()
                        );
                        // If we haven't seen this variable yet, it's an initialization
                        if !var_exprs.contains_key(var_id) {
                            eprintln!(
                                "[BUG #135 FIX] Found initialization assignment for var {:?}",
                                var_id
                            );
                            var_exprs.insert(*var_id, assign.rhs.clone());
                            init_assignment_count += 1;
                            continue;
                        } else {
                            eprintln!(
                                "[BUG #135 FIX] Variable {:?} already in var_exprs, stopping scan",
                                var_id
                            );
                        }
                    }
                    // Non-initialization assignment, stop scanning
                    break;
                }
                _ => break, // Non-assignment statement, stop scanning
            }
        }
        let actual_stmts_before_return = &stmts_before_return[init_assignment_count..];
        if init_assignment_count > 0 {
            eprintln!(
                "[BUG #135 FIX] Extracted {} init assignments, {} stmts remaining",
                init_assignment_count,
                actual_stmts_before_return.len()
            );
        }

        // If no mutable variables, this isn't our pattern
        if var_exprs.is_empty() {
            return None;
        }

        // Check that returned variable is mutable
        if !var_exprs.contains_key(&returned_var_id) {
            return None;
        }

        let mut modified = false;

        // Process each statement (use actual_stmts_before_return which excludes init assignments)
        for stmt in actual_stmts_before_return.iter() {
            match stmt {
                // Handle straight-line Assignment statements (not just If)
                // This is critical for functions like bitreverse32 and popcount32 that have:
                //   let mut x = value;
                //   x = expr1;  // Straight-line assignment - must be processed!
                //   x = expr2;
                //   return x
                hir::HirStatement::Assignment(assign) => {
                    println!(
                        "[BUG #180 DEBUG] Found Assignment in try_convert_mutable_var_pattern"
                    );
                    if let hir::HirLValue::Variable(var_id) = &assign.lhs {
                        println!(
                            "[BUG #180 DEBUG]   -> To var {:?}, tracked={}",
                            var_id,
                            var_exprs.contains_key(var_id)
                        );
                        // Only process if this is a reassignment to a tracked mutable variable
                        if var_exprs.contains_key(var_id) {
                            // Substitute variable references in the RHS with current expressions
                            let substituted_rhs =
                                self.substitute_variables(&assign.rhs, &var_exprs, let_bindings);
                            println!("[BUG #180 DEBUG]   -> Updated var {:?} expression", var_id);

                            // Update the variable's current expression (SSA-style)
                            var_exprs.insert(*var_id, substituted_rhs);
                            modified = true;
                        }
                    }
                }

                hir::HirStatement::If(if_stmt) => {
                    // Get all assignments in the then-block
                    let assignments = self.collect_assignments_in_block(&if_stmt.then_statements);

                    if assignments.is_empty() {
                        continue;
                    }

                    modified = true;

                    // Substitute variable references in the condition
                    let condition =
                        self.substitute_variables(&if_stmt.condition, &var_exprs, let_bindings);

                    // For each assignment, create a conditional expression
                    for (var_id, rhs) in &assignments {
                        // Substitute variable references in the RHS
                        let substituted_rhs =
                            self.substitute_variables(rhs, &var_exprs, let_bindings);

                        // Get current value for else branch
                        let current_value = var_exprs
                            .get(var_id)
                            .cloned()
                            .unwrap_or(hir::HirExpression::Variable(*var_id));

                        // Create: if condition { substituted_rhs } else { current_value }
                        let new_expr = hir::HirExpression::If(hir::HirIfExpr {
                            condition: Box::new(condition.clone()),
                            then_expr: Box::new(substituted_rhs),
                            else_expr: Box::new(current_value),
                        });

                        debug_println!("[BUG #86] Created conditional for var {:?}", var_id);
                        var_exprs.insert(*var_id, new_expr);
                    }
                }

                // Other statement types - skip
                _ => {}
            }
        }

        if modified {
            // Return the final expression for the returned variable
            let inner_result = var_exprs.get(&returned_var_id).cloned();
            println!("[BUG #180 DEBUG] try_convert_mutable_var_pattern: modified=true, returning result for var {:?}", returned_var_id);

            // BUG #134 FIX: If original return was Cast(Variable), wrap result in Cast
            match (inner_result, outer_cast) {
                (Some(expr), Some(target_type)) => {
                    debug_println!("[BUG #134] Wrapping result in Cast to {:?}", target_type);
                    Some(hir::HirExpression::Cast(hir::HirCastExpr {
                        expr: Box::new(expr),
                        target_type,
                    }))
                }
                (Some(expr), None) => Some(expr),
                (None, _) => None,
            }
        } else {
            println!(
                "[BUG #180 DEBUG] try_convert_mutable_var_pattern: modified=false, returning None"
            );
            debug_println!("[BUG #86] No if statements modified any variables");
            None
        }
    }

    /// Helper: Collect all variable assignments from a block of statements
    fn collect_assignments_in_block(
        &self,
        stmts: &[hir::HirStatement],
    ) -> Vec<(hir::VariableId, hir::HirExpression)> {
        let mut assignments = Vec::new();
        for stmt in stmts {
            if let hir::HirStatement::Assignment(assign) = stmt {
                if let hir::HirLValue::Variable(var_id) = &assign.lhs {
                    assignments.push((*var_id, assign.rhs.clone()));
                }
            }
        }
        assignments
    }

    /// Helper: Substitute variable references in an expression with current values
    #[allow(clippy::only_used_in_recursion)]
    fn substitute_variables(
        &self,
        expr: &hir::HirExpression,
        var_exprs: &IndexMap<hir::VariableId, hir::HirExpression>,
        _let_bindings: &[hir::HirLetStatement],
    ) -> hir::HirExpression {
        match expr {
            hir::HirExpression::Variable(var_id) => {
                // If we have a current expression for this variable, use it
                if let Some(current_expr) = var_exprs.get(var_id) {
                    current_expr.clone()
                } else {
                    expr.clone()
                }
            }
            hir::HirExpression::Binary(bin) => hir::HirExpression::Binary(hir::HirBinaryExpr {
                op: bin.op.clone(),
                left: Box::new(self.substitute_variables(&bin.left, var_exprs, _let_bindings)),
                right: Box::new(self.substitute_variables(&bin.right, var_exprs, _let_bindings)),
                is_trait_op: bin.is_trait_op,
            }),
            hir::HirExpression::Unary(un) => hir::HirExpression::Unary(hir::HirUnaryExpr {
                op: un.op.clone(),
                operand: Box::new(self.substitute_variables(&un.operand, var_exprs, _let_bindings)),
            }),
            hir::HirExpression::Call(call) => hir::HirExpression::Call(hir::HirCallExpr {
                function: call.function.clone(),
                args: call
                    .args
                    .iter()
                    .map(|a| self.substitute_variables(a, var_exprs, _let_bindings))
                    .collect(),
                type_args: call.type_args.clone(),
                named_type_args: call.named_type_args.clone(),
                impl_style: call.impl_style,
            }),
            hir::HirExpression::If(if_expr) => hir::HirExpression::If(hir::HirIfExpr {
                condition: Box::new(self.substitute_variables(
                    &if_expr.condition,
                    var_exprs,
                    _let_bindings,
                )),
                then_expr: Box::new(self.substitute_variables(
                    &if_expr.then_expr,
                    var_exprs,
                    _let_bindings,
                )),
                else_expr: Box::new(self.substitute_variables(
                    &if_expr.else_expr,
                    var_exprs,
                    _let_bindings,
                )),
            }),
            hir::HirExpression::Index(base, idx) => hir::HirExpression::Index(
                Box::new(self.substitute_variables(base, var_exprs, _let_bindings)),
                Box::new(self.substitute_variables(idx, var_exprs, _let_bindings)),
            ),
            hir::HirExpression::Range(base, lo, hi) => hir::HirExpression::Range(
                Box::new(self.substitute_variables(base, var_exprs, _let_bindings)),
                Box::new(self.substitute_variables(lo, var_exprs, _let_bindings)),
                Box::new(self.substitute_variables(hi, var_exprs, _let_bindings)),
            ),
            hir::HirExpression::Cast(cast) => hir::HirExpression::Cast(hir::HirCastExpr {
                expr: Box::new(self.substitute_variables(&cast.expr, var_exprs, _let_bindings)),
                target_type: cast.target_type.clone(),
            }),
            hir::HirExpression::Concat(exprs) => hir::HirExpression::Concat(
                exprs
                    .iter()
                    .map(|e| self.substitute_variables(e, var_exprs, _let_bindings))
                    .collect(),
            ),
            hir::HirExpression::TupleLiteral(exprs) => hir::HirExpression::TupleLiteral(
                exprs
                    .iter()
                    .map(|e| self.substitute_variables(e, var_exprs, _let_bindings))
                    .collect(),
            ),
            hir::HirExpression::ArrayLiteral(exprs) => hir::HirExpression::ArrayLiteral(
                exprs
                    .iter()
                    .map(|e| self.substitute_variables(e, var_exprs, _let_bindings))
                    .collect(),
            ),
            hir::HirExpression::FieldAccess { base, field } => hir::HirExpression::FieldAccess {
                base: Box::new(self.substitute_variables(base, var_exprs, _let_bindings)),
                field: field.clone(),
            },
            hir::HirExpression::Ternary {
                condition,
                true_expr,
                false_expr,
            } => hir::HirExpression::Ternary {
                condition: Box::new(self.substitute_variables(condition, var_exprs, _let_bindings)),
                true_expr: Box::new(self.substitute_variables(true_expr, var_exprs, _let_bindings)),
                false_expr: Box::new(self.substitute_variables(
                    false_expr,
                    var_exprs,
                    _let_bindings,
                )),
            },
            // For other expressions (literals, signals, etc.), return as-is
            _ => expr.clone(),
        }
    }

    /// BUG #86 FIX: Try to transform block expression with mutable variable pattern
    ///
    /// This handles block expressions like:
    /// ```ignore
    /// {
    ///     let mut count = 0;
    ///     let mut temp = value;
    ///     if (temp & 0xFFFF0000) == 0 { count = count + 16; temp = temp << 16; }
    ///     if (temp & 0xFF000000) == 0 { count = count + 8;  temp = temp << 8; }
    ///     count
    /// }
    /// ```
    ///
    /// Transforms to nested conditional expressions that properly chain the updates.
    fn try_transform_block_mutable_vars(
        &self,
        statements: &[hir::HirStatement],
        result_expr: &hir::HirExpression,
    ) -> Option<hir::HirExpression> {
        use indexmap::IndexMap;

        // BUG #145 DEBUG: Simple trace - using println! to stdout
        debug_println!("🔮🔮🔮 [BUG #145] ENTERING try_transform_block_mutable_vars 🔮🔮🔮");

        debug_println!("🔧🔧🔧 [BUG #86] NEWCODE! try_transform_block_mutable_vars: {} statements, result_expr type: {:?} 🔧🔧🔧",
                 statements.len(), std::mem::discriminant(result_expr));
        // Also print the actual result_expr for debugging
        match result_expr {
            hir::HirExpression::Variable(var_id) => println!(
                "🔧🔧🔧 [BUG #86] result_expr is Variable({}) 🔧🔧🔧",
                var_id.0
            ),
            hir::HirExpression::Cast(cast) => println!(
                "🔧🔧🔧 [BUG #86] result_expr is Cast, inner: {:?} 🔧🔧🔧",
                std::mem::discriminant(&*cast.expr)
            ),
            other => debug_println!("🔧🔧🔧 [BUG #86] result_expr is {:?} 🔧🔧🔧", other),
        }

        // BUG #119 FIX: Check if result expression is a variable reference, or wrapped in Cast/Range
        // For popcount32, result is Cast(Range(Variable(x), ...)) - extract the inner Variable
        let (returned_var_id, result_wrapper) = match result_expr {
            hir::HirExpression::Variable(var_id) => (*var_id, None),
            hir::HirExpression::Cast(cast) => {
                // Cast(expr) - check if expr contains a Variable
                if let Some(var_id) = Self::extract_variable_from_expr(&cast.expr) {
                    println!(
                        "🔧🔧🔧 [BUG #119 FIX] result_expr is Cast wrapping Variable({}) 🔧🔧🔧",
                        var_id.0
                    );
                    (var_id, Some(result_expr.clone()))
                } else {
                    debug_println!("🔧🔧🔧 [BUG #86] Block: result_expr is Cast but no Variable inside, skipping 🔧🔧🔧");
                    return None;
                }
            }
            hir::HirExpression::Range(base, _, _) => {
                // Range(expr, hi, lo) - check if expr is a Variable
                if let hir::HirExpression::Variable(var_id) = &**base {
                    println!(
                        "🔧🔧🔧 [BUG #119 FIX] result_expr is Range wrapping Variable({}) 🔧🔧🔧",
                        var_id.0
                    );
                    (*var_id, Some(result_expr.clone()))
                } else {
                    debug_println!("🔧🔧🔧 [BUG #86] Block: result_expr is Range but not Variable inside, skipping 🔧🔧🔧");
                    return None;
                }
            }
            hir::HirExpression::Concat(elements) => {
                // BUG #119 FIX: Handle Concat([Literal(0), Variable(x)]) pattern - zero extension
                // This pattern appears when returning a narrower variable in a wider result type
                // e.g., popcount32 returns `x[5:0] as nat[6]` which becomes Concat([Literal(0), Variable(10)])
                if let Some(var_id) = Self::extract_variable_from_concat(elements) {
                    println!(
                        "🔧🔧🔧 [BUG #119 FIX] result_expr is Concat wrapping Variable({}) 🔧🔧🔧",
                        var_id.0
                    );
                    (var_id, Some(result_expr.clone()))
                } else {
                    debug_println!("🔧🔧🔧 [BUG #86] Block: result_expr is Concat but no Variable found, skipping 🔧🔧🔧");
                    return None;
                }
            }
            _ => {
                debug_println!("🔧🔧🔧 [BUG #86] Block: result_expr is NOT a variable (type: {:?}), skipping 🔧🔧🔧", std::mem::discriminant(result_expr));
                return None;
            }
        };

        // BUG #119 FIX: Extract let bindings, if statements, and assignment statements
        let mut let_bindings: Vec<&hir::HirLetStatement> = Vec::new();
        let mut if_statements: Vec<&hir::HirIfStatement> = Vec::new();
        let mut assignment_statements: Vec<&hir::HirAssignment> = Vec::new();
        let mut has_other_statements = false;

        for stmt in statements {
            match stmt {
                hir::HirStatement::Let(let_stmt) => {
                    let_bindings.push(let_stmt);
                }
                hir::HirStatement::If(if_stmt) => {
                    if_statements.push(if_stmt);
                }
                hir::HirStatement::Assignment(assign_stmt) => {
                    // BUG #119 FIX: Also collect Assignment statements for sequential patterns
                    assignment_statements.push(assign_stmt);
                }
                _ => {
                    // Other statements present - can't use simple pattern
                    has_other_statements = true;
                }
            }
        }

        // BUG #119 FIX: Must have at least one let binding, and either IF statements OR Assignment statements
        let has_if_pattern = !let_bindings.is_empty() && !if_statements.is_empty();
        let has_assign_pattern = !let_bindings.is_empty()
            && !assignment_statements.is_empty()
            && if_statements.is_empty();

        if !has_if_pattern && !has_assign_pattern {
            debug_println!("🔧🔧🔧 [BUG #86] Block: no let bindings ({}) or no if statements ({}) or no assignments ({}), skipping 🔧🔧🔧",
                     let_bindings.len(), if_statements.len(), assignment_statements.len());
            return None;
        }

        // For now, reject if there are other statement types (could extend later)
        if has_other_statements {
            debug_println!("🔧🔧🔧 [BUG #86] Block: has other statement types, skipping 🔧🔧🔧");
            return None;
        }

        println!(
            "🔧🔧🔧 [BUG #86] Block: {} let bindings, {} if statements 🔧🔧🔧",
            let_bindings.len(),
            if_statements.len()
        );

        // Build a map from VariableId to current expression for ALL mutable variables
        let mut var_exprs: IndexMap<hir::VariableId, hir::HirExpression> = IndexMap::new();

        // BUG #121 FIX: Also track immutable let bindings for substitution
        // When a mutable variable pattern block also references immutable variables
        // (e.g., discriminant in quadratic_solve), those immutable Variables must also
        // be substituted with their values, or they can't be looked up in the caller's ctx.
        let mut immutable_vars: IndexMap<hir::VariableId, hir::HirExpression> = IndexMap::new();

        // Initialize with all bindings' initial values (tracking mutable separately)
        for let_stmt in &let_bindings {
            if let_stmt.mutable {
                println!(
                    "🔧🔧🔧 [BUG #86] Block: Tracking mutable variable '{}' (id={}) 🔧🔧🔧",
                    let_stmt.name, let_stmt.id.0
                );
                var_exprs.insert(let_stmt.id, let_stmt.value.clone());
            } else {
                println!(
                    "🔧🔧🔧 [BUG #121] Block: Tracking immutable variable '{}' (id={}) 🔧🔧🔧",
                    let_stmt.name, let_stmt.id.0
                );
                immutable_vars.insert(let_stmt.id, let_stmt.value.clone());
            }
        }

        // If no mutable variables, this isn't our pattern
        if var_exprs.is_empty() {
            debug_println!("🔧🔧🔧 [BUG #86] Block: No mutable variables found 🔧🔧🔧");
            return None;
        }

        // BUG #121 FIX: Keep track of mutable variable IDs before merging
        let mutable_var_ids: std::collections::HashSet<hir::VariableId> =
            var_exprs.keys().cloned().collect();

        // BUG #121 FIX: Merge immutable vars into var_exprs for substitution
        // The substitute_variables function will use this combined map
        for (id, expr) in &immutable_vars {
            var_exprs.insert(*id, expr.clone());
        }

        // Check that returned variable is mutable (use the pre-merge set)
        if !mutable_var_ids.contains(&returned_var_id) {
            println!(
                "🔧🔧🔧 [BUG #86] Block: Returned variable {} is not mutable 🔧🔧🔧",
                returned_var_id.0
            );
            return None;
        }

        println!(
            "🔧🔧🔧 [BUG #86] Block: Processing {} mutable vars, returned_var_id={} 🔧🔧🔧",
            var_exprs.len(),
            returned_var_id.0
        );

        // Convert let_bindings to slice of owned for substitution
        let let_bindings_owned: Vec<hir::HirLetStatement> =
            let_bindings.iter().map(|l| (*l).clone()).collect();

        // Process each if statement
        for (if_idx, if_stmt) in if_statements.iter().enumerate() {
            // Get all assignments in the then-block
            let assignments = self.collect_assignments_in_block(&if_stmt.then_statements);

            if assignments.is_empty() {
                continue;
            }

            debug_println!("🔧🔧🔧 [BUG #86] Processing if_stmt {} 🔧🔧🔧", if_idx);
            println!(
                "🔧🔧🔧 [BUG #86]   var_exprs has {} entries 🔧🔧🔧",
                var_exprs.len()
            );
            for (vid, vexpr) in var_exprs.iter() {
                println!(
                    "🔧🔧🔧 [BUG #86]   var_exprs[{}] = {:?} 🔧🔧🔧",
                    vid.0,
                    std::mem::discriminant(vexpr)
                );
            }

            // Debug: Print original condition structure
            println!(
                "🔧🔧🔧 [BUG #86]   Original condition: {:?} 🔧🔧🔧",
                &if_stmt.condition
            );

            // Substitute variable references in the condition
            let condition =
                self.substitute_variables(&if_stmt.condition, &var_exprs, &let_bindings_owned);
            println!(
                "🔧🔧🔧 [BUG #86]   Condition after substitution: {:?} 🔧🔧🔧",
                &condition
            );

            // For each assignment, create a conditional expression
            for (var_id, rhs) in &assignments {
                // Substitute variable references in the RHS
                let substituted_rhs =
                    self.substitute_variables(rhs, &var_exprs, &let_bindings_owned);

                // Get current value for else branch
                let current_value = var_exprs
                    .get(var_id)
                    .cloned()
                    .unwrap_or(hir::HirExpression::Variable(*var_id));

                // Create: if condition { substituted_rhs } else { current_value }
                let new_expr = hir::HirExpression::If(hir::HirIfExpr {
                    condition: Box::new(condition.clone()),
                    then_expr: Box::new(substituted_rhs),
                    else_expr: Box::new(current_value),
                });

                println!(
                    "🔧🔧🔧 [BUG #86] Block: Created conditional for var {} 🔧🔧🔧",
                    var_id.0
                );
                var_exprs.insert(*var_id, new_expr);
            }
        }

        // BUG #119 FIX: Process sequential assignment statements (for patterns like popcount32)
        // These are unconditional assignments that just update the mutable variable
        for (assign_idx, assign_stmt) in assignment_statements.iter().enumerate() {
            if let hir::HirLValue::Variable(var_id) = &assign_stmt.lhs {
                // Only process if this is a mutable variable we're tracking
                let var_id_copy = *var_id;
                if var_exprs.contains_key(&var_id_copy) {
                    println!(
                        "🔧🔧🔧 [BUG #119 FIX] Processing assignment {} to Variable({}) 🔧🔧🔧",
                        assign_idx, var_id_copy.0
                    );

                    // Substitute variable references in the RHS
                    let substituted_rhs = self.substitute_variables(
                        &assign_stmt.rhs,
                        &var_exprs,
                        &let_bindings_owned,
                    );

                    println!(
                        "🔧🔧🔧 [BUG #119 FIX]   RHS after substitution: {:?} 🔧🔧🔧",
                        std::mem::discriminant(&substituted_rhs)
                    );

                    // Update the variable's expression
                    var_exprs.insert(var_id_copy, substituted_rhs);
                }
            }
        }

        // Return the final expression for the returned variable
        let result = var_exprs.get(&returned_var_id).cloned();
        if let Some(final_expr) = result {
            // BUG #119 FIX: If there was a wrapper (Cast/Range), apply it to the final expression
            let final_result = if let Some(wrapper) = result_wrapper {
                debug_println!("🔧🔧🔧 [BUG #119 FIX] Applying wrapper to final expression 🔧🔧🔧");
                Self::apply_wrapper_to_expr(&wrapper, final_expr, returned_var_id)
            } else {
                final_expr
            };
            println!(
                "🔧🔧🔧 [BUG #86] ✅ Successfully transformed mutable variable pattern! 🔧🔧🔧"
            );
            Some(final_result)
        } else {
            println!(
                "🔧🔧🔧 [BUG #86] ❌ Failed: returned_var_id {} not in var_exprs 🔧🔧🔧",
                returned_var_id.0
            );
            None
        }
    }

    /// BUG #119 FIX: Extract a Variable from an expression that may be wrapped in Cast/Range
    fn extract_variable_from_expr(expr: &hir::HirExpression) -> Option<hir::VariableId> {
        match expr {
            hir::HirExpression::Variable(var_id) => Some(*var_id),
            hir::HirExpression::Range(base, _, _) => Self::extract_variable_from_expr(base),
            hir::HirExpression::Cast(cast) => Self::extract_variable_from_expr(&cast.expr),
            _ => None,
        }
    }

    /// BUG #119 FIX: Extract variable from Concat([Literal, Variable]) pattern
    /// This pattern is used for zero-extension of narrower results
    fn extract_variable_from_concat(elements: &[hir::HirExpression]) -> Option<hir::VariableId> {
        // Look for a Variable in the elements (typically the last one for zero-extension)
        for elem in elements.iter().rev() {
            if let hir::HirExpression::Variable(var_id) = elem {
                return Some(*var_id);
            }
            // Also check nested expressions
            if let Some(var_id) = Self::extract_variable_from_expr(elem) {
                return Some(var_id);
            }
        }
        None
    }

    /// BUG #119 FIX: Apply a wrapper (Cast/Range) to a new inner expression
    fn apply_wrapper_to_expr(
        wrapper: &hir::HirExpression,
        inner: hir::HirExpression,
        _var_id: hir::VariableId,
    ) -> hir::HirExpression {
        match wrapper {
            hir::HirExpression::Cast(cast) => {
                // Recursively apply wrapper if the inner part also has a wrapper
                let new_inner = if let Some(sub_wrapper) = Self::get_inner_wrapper(&cast.expr) {
                    Self::apply_wrapper_to_expr(&sub_wrapper, inner, _var_id)
                } else {
                    inner
                };
                hir::HirExpression::Cast(hir::HirCastExpr {
                    expr: Box::new(new_inner),
                    target_type: cast.target_type.clone(),
                })
            }
            hir::HirExpression::Range(base, hi, lo) => {
                // Recursively apply wrapper if the inner part also has a wrapper
                let new_inner = if let Some(sub_wrapper) = Self::get_inner_wrapper(base) {
                    Self::apply_wrapper_to_expr(&sub_wrapper, inner, _var_id)
                } else {
                    inner
                };
                hir::HirExpression::Range(Box::new(new_inner), hi.clone(), lo.clone())
            }
            hir::HirExpression::Concat(elements) => {
                // BUG #119 FIX: Apply Concat wrapper - replace the Variable element with new inner
                // For Concat([Literal(0), Variable(x)]), replace Variable(x) with inner
                let new_elements: Vec<hir::HirExpression> = elements
                    .iter()
                    .map(|elem| {
                        if let hir::HirExpression::Variable(var_id) = elem {
                            if *var_id == _var_id {
                                return inner.clone();
                            }
                        }
                        // Check for nested wrappers
                        if let Some(nested_var_id) = Self::extract_variable_from_expr(elem) {
                            if nested_var_id == _var_id {
                                return Self::apply_wrapper_to_expr(elem, inner.clone(), _var_id);
                            }
                        }
                        elem.clone()
                    })
                    .collect();
                hir::HirExpression::Concat(new_elements)
            }
            _ => inner,
        }
    }

    /// BUG #119 FIX: Get the inner wrapper if the expression is a Cast/Range wrapping another Cast/Range/Variable
    fn get_inner_wrapper(expr: &hir::HirExpression) -> Option<hir::HirExpression> {
        match expr {
            hir::HirExpression::Cast(cast) => Some((*cast.expr).clone()),
            hir::HirExpression::Range(base, _, _) => Some((**base).clone()),
            _ => None,
        }
    }

    /// Convert an if-statement (with returns in branches) to an if-expression
    fn convert_if_stmt_to_expr(&self, if_stmt: &hir::HirIfStatement) -> Option<hir::HirExpression> {
        eprintln!(
            "[BUG #7] convert_if_stmt_to_expr: condition type = {:?}",
            std::mem::discriminant(&if_stmt.condition)
        );
        if let hir::HirExpression::Call(call) = &if_stmt.condition {
            eprintln!(
                "[BUG #7]   condition is Call: function={}, args={}",
                call.function,
                call.args.len()
            );
        }
        if let hir::HirExpression::Variable(var_id) = &if_stmt.condition {
            debug_println!("[BUG #7]   condition is Variable: {:?}", var_id);
        }

        // Recursively convert then-branch
        let then_expr = self.convert_body_to_expression(&if_stmt.then_statements)?;

        // Recursively convert else-branch
        let else_expr = if let Some(else_stmts) = &if_stmt.else_statements {
            self.convert_body_to_expression(else_stmts)?
        } else {
            eprintln!("convert_if_stmt_to_expr: if-statement missing else branch");
            return None;
        };

        let result = hir::HirExpression::If(hir::HirIfExpr {
            condition: Box::new(if_stmt.condition.clone()),
            then_expr: Box::new(then_expr),
            else_expr: Box::new(else_expr),
        });

        eprintln!(
            "[BUG #7] convert_if_stmt_to_expr: result condition type = {:?}",
            std::mem::discriminant(&if_stmt.condition)
        );

        Some(result)
    }

    /// Convert a match-statement to a match-expression
    fn convert_match_stmt_to_expr(
        &self,
        match_stmt: &hir::HirMatchStatement,
    ) -> Option<hir::HirExpression> {
        // Convert each arm from statement form to expression form
        println!(
            "[DEBUG] convert_match_stmt_to_expr: {} arms",
            match_stmt.arms.len()
        );
        let arms: Option<Vec<_>> = match_stmt
            .arms
            .iter()
            .enumerate()
            .map(|(i, arm)| {
                debug_println!("[DEBUG] convert_match_stmt_to_expr: arm {} has {} statements", i, arm.statements.len());
                for (j, stmt) in arm.statements.iter().enumerate() {
                    debug_println!("[DEBUG]   stmt[{}]: {:?}", j, std::mem::discriminant(stmt));
                }
                // Convert arm's statements to an expression
                let expr = self.convert_body_to_expression(&arm.statements);
                if expr.is_none() {
                    debug_println!("[DEBUG] convert_match_stmt_to_expr: arm {} convert_body_to_expression FAILED!", i);
                }
                let expr = expr?;
                Some(hir::HirMatchArmExpr {
                    pattern: arm.pattern.clone(),
                    guard: arm.guard.clone(),
                    expr,
                })
            })
            .collect();

        let arms = arms?;

        Some(hir::HirExpression::Match(hir::HirMatchExpr {
            expr: Box::new(match_stmt.expr.clone()),
            arms,
            mux_style: match_stmt.mux_style,
        }))
    }

    /// Build a let expression from a list of let bindings and a body expression
    /// Uses Block expression syntax since HIR doesn't have a Let variant in HirExpression
    fn build_let_expression(
        &self,
        bindings: Vec<hir::HirLetStatement>,
        body: hir::HirExpression,
    ) -> hir::HirExpression {
        // BUG #137 FIX: If body is already a Block, FLATTEN it instead of creating nested Blocks.
        // This is critical for popcount32-style functions:
        //   - bindings = [let mut x = value]
        //   - body = Block { statements: [5 Assignments], result_expr: Cast(...) }
        // Without flattening, we'd create:
        //   Block { statements: [Let(x)], result_expr: Block { statements: [5 Assigns], ... } }
        // Which causes try_transform_block_mutable_vars to see 1 let + 0 assignments at outer level!
        // With flattening:
        //   Block { statements: [Let(x), Assign, Assign, Assign, Assign, Assign], result_expr: Cast(...) }
        // Now try_transform_block_mutable_vars sees all statements at one level.

        if let hir::HirExpression::Block {
            statements: inner_stmts,
            result_expr: inner_result,
        } = body
        {
            // Flatten: prepend let bindings to inner block's statements
            let mut all_statements: Vec<hir::HirStatement> =
                bindings.into_iter().map(hir::HirStatement::Let).collect();
            all_statements.extend(inner_stmts);

            debug_println!("[BUG #137 FIX] build_let_expression: FLATTENING nested Block - {} let bindings + {} inner stmts = {} total",
                     all_statements.iter().filter(|s| matches!(s, hir::HirStatement::Let(_))).count(),
                     all_statements.iter().filter(|s| matches!(s, hir::HirStatement::Assignment(_))).count(),
                     all_statements.len());

            hir::HirExpression::Block {
                statements: all_statements,
                result_expr: inner_result,
            }
        } else {
            // Body is not a Block - create a new Block as before
            let statements = bindings.into_iter().map(hir::HirStatement::Let).collect();

            hir::HirExpression::Block {
                statements,
                result_expr: Box::new(body),
            }
        }
    }

    /// Extract return expression from a function body
    /// Phase 2: Simple functions with single return
    /// Phase 3: Functions with let bindings + return
    fn extract_return_expression<'a>(
        &self,
        body: &'a [hir::HirStatement],
    ) -> Option<&'a hir::HirExpression> {
        // Body must be non-empty and end with return
        if body.is_empty() {
            eprintln!("Function inlining: empty function body");
            return None;
        }

        // Last statement must be a return
        match body.last() {
            Some(hir::HirStatement::Return(Some(expr))) => Some(expr),
            Some(hir::HirStatement::Return(None)) => {
                eprintln!("Function inlining: Return statement has None expression");
                None
            }
            Some(hir::HirStatement::Expression(expr)) => {
                // For functions with implicit returns (last expression)
                eprintln!("Function inlining: treating last expression as implicit return");
                Some(expr)
            }
            last_stmt => {
                eprintln!("Function inlining: function must end with return statement");
                eprintln!("  Body length: {}", body.len());
                eprintln!("  Last statement: {:?}", last_stmt);
                None
            }
        }
    }

    /// Substitute parameters in an expression with argument expressions
    /// This creates a parameter -> argument mapping and replaces all parameter references
    fn substitute_expression(
        &mut self,
        expr: &hir::HirExpression,
        param_map: &IndexMap<String, &hir::HirExpression>,
    ) -> Option<hir::HirExpression> {
        match expr {
            // GenericParam - function parameters are parsed as generic params
            // Check if this matches a function parameter and substitute
            hir::HirExpression::GenericParam(name) => {
                if let Some(arg_expr) = param_map.get(name) {
                    // Clone the argument expression
                    Some((*arg_expr).clone())
                } else {
                    // Not a function parameter, keep as-is
                    Some(expr.clone())
                }
            }

            // Parameter reference - replace with argument expression
            hir::HirExpression::Variable(var_id) => {
                // Look up the variable name in HIR
                let _hir = self.hir?;

                // Check if this variable is actually a function parameter
                // We need to find its name first
                let var_name = self.find_variable_name(*var_id)?;

                if let Some(arg_expr) = param_map.get(&var_name) {
                    // Clone the argument expression
                    Some((*arg_expr).clone())
                } else {
                    // Not a parameter, keep as-is
                    Some(expr.clone())
                }
            }

            // Binary expression - substitute both sides
            hir::HirExpression::Binary(binary) => {
                let left = Box::new(self.substitute_expression(&binary.left, param_map)?);
                let right = Box::new(self.substitute_expression(&binary.right, param_map)?);
                Some(hir::HirExpression::Binary(hir::HirBinaryExpr {
                    left,
                    op: binary.op.clone(),
                    right,
                    is_trait_op: binary.is_trait_op,
                }))
            }

            // Unary expression - substitute operand
            hir::HirExpression::Unary(unary) => {
                let operand = Box::new(self.substitute_expression(&unary.operand, param_map)?);
                Some(hir::HirExpression::Unary(hir::HirUnaryExpr {
                    op: unary.op.clone(),
                    operand,
                }))
            }

            // If expression - substitute all parts
            hir::HirExpression::If(if_expr) => {
                let condition =
                    Box::new(self.substitute_expression(&if_expr.condition, param_map)?);
                let then_expr =
                    Box::new(self.substitute_expression(&if_expr.then_expr, param_map)?);
                let else_expr =
                    Box::new(self.substitute_expression(&if_expr.else_expr, param_map)?);
                Some(hir::HirExpression::If(hir::HirIfExpr {
                    condition,
                    then_expr,
                    else_expr,
                }))
            }

            // For other expressions that don't contain parameters, clone as-is
            hir::HirExpression::Literal(_)
            | hir::HirExpression::Signal(_)
            | hir::HirExpression::Port(_)
            | hir::HirExpression::Constant(_) => Some(expr.clone()),

            // Recursively handle other expression types as needed
            _ => {
                // For unhandled cases, clone as-is (may need expansion for full support)
                Some(expr.clone())
            }
        }
    }

    /// Substitute parameters and local variables in an expression with argument expressions
    /// This version takes a var_id_to_name map for looking up function-local let bindings
    fn substitute_expression_with_var_map(
        &mut self,
        expr: &hir::HirExpression,
        param_map: &IndexMap<String, &hir::HirExpression>,
        var_id_to_name: &IndexMap<hir::VariableId, String>,
    ) -> Option<hir::HirExpression> {
        match expr {
            // GenericParam - function parameters are parsed as generic params
            // Check if this matches a function parameter and substitute
            hir::HirExpression::GenericParam(name) => {
                if let Some(arg_expr) = param_map.get(name) {
                    // Clone the argument expression
                    Some((*arg_expr).clone())
                } else {
                    // Not a function parameter, keep as-is
                    Some(expr.clone())
                }
            }

            // Variable reference - replace with mapped expression
            hir::HirExpression::Variable(var_id) => {
                eprintln!(
                    "[BUG #IMPORT_MATCH] Variable substitution: var_id={:?}",
                    var_id
                );
                // Look up variable name using the provided map (for function-local variables)
                if let Some(var_name) = var_id_to_name.get(var_id) {
                    debug_println!("[CONTEXT] Variable substitution: var_id={:?} -> var_name='{}', in var_id_to_name", var_id, var_name);
                    // Check if this variable should be substituted from param_map
                    if let Some(arg_expr) = param_map.get(var_name) {
                        eprintln!(
                            "[CONTEXT] Variable '{}' found in param_map, substituting",
                            var_name
                        );
                        // Substitute with the argument expression
                        return Some((*arg_expr).clone());
                    } else {
                        eprintln!(
                            "[CONTEXT] Variable '{}' NOT in param_map, keeping as-is",
                            var_name
                        );
                        // Not a parameter, keep as variable reference
                        return Some(expr.clone());
                    }
                }

                // Fall back to global variable lookup
                if let Some(var_name) = self.find_variable_name(*var_id) {
                    if let Some(arg_expr) = param_map.get(&var_name) {
                        eprintln!(
                            "[DEBUG] Variable substitution (global): var_name={}, expr type: {:?}",
                            var_name,
                            std::mem::discriminant(&**arg_expr)
                        );
                        if let hir::HirExpression::Match(m) = &**arg_expr {
                            eprintln!(
                                "[DEBUG] Variable (global): substituting Match with {} arms",
                                m.arms.len()
                            );
                        }
                        let cloned = (*arg_expr).clone();
                        if let hir::HirExpression::Match(m) = &cloned {
                            eprintln!(
                                "[DEBUG] Variable (global): cloned Match has {} arms",
                                m.arms.len()
                            );
                        }
                        return Some(cloned);
                    } else {
                        eprintln!(
                            "[DEBUG] Variable (global): name '{}' not found in param_map",
                            var_name
                        );
                    }
                }

                // Not in substitution map, keep as-is
                Some(expr.clone())
            }

            // Binary expression - substitute both sides
            hir::HirExpression::Binary(binary) => {
                if let hir::HirExpression::Variable(left_id) = &*binary.left {}
                if let hir::HirExpression::Variable(right_id) = &*binary.right {}

                let left = Box::new(self.substitute_expression_with_var_map(
                    &binary.left,
                    param_map,
                    var_id_to_name,
                )?);
                if let hir::HirExpression::Variable(left_id) = &*left {}

                let right = Box::new(self.substitute_expression_with_var_map(
                    &binary.right,
                    param_map,
                    var_id_to_name,
                )?);
                if let hir::HirExpression::Variable(right_id) = &*right {}

                Some(hir::HirExpression::Binary(hir::HirBinaryExpr {
                    left,
                    op: binary.op.clone(),
                    right,
                    is_trait_op: binary.is_trait_op,
                }))
            }

            // Unary expression - substitute operand
            hir::HirExpression::Unary(unary) => {
                let operand = Box::new(self.substitute_expression_with_var_map(
                    &unary.operand,
                    param_map,
                    var_id_to_name,
                )?);
                Some(hir::HirExpression::Unary(hir::HirUnaryExpr {
                    op: unary.op.clone(),
                    operand,
                }))
            }

            // If expression - substitute all parts
            hir::HirExpression::If(if_expr) => {
                let condition = Box::new(self.substitute_expression_with_var_map(
                    &if_expr.condition,
                    param_map,
                    var_id_to_name,
                )?);
                let then_expr = Box::new(self.substitute_expression_with_var_map(
                    &if_expr.then_expr,
                    param_map,
                    var_id_to_name,
                )?);
                let else_expr = Box::new(self.substitute_expression_with_var_map(
                    &if_expr.else_expr,
                    param_map,
                    var_id_to_name,
                )?);
                Some(hir::HirExpression::If(hir::HirIfExpr {
                    condition,
                    then_expr,
                    else_expr,
                }))
            }

            // Match expression - substitute all parts
            hir::HirExpression::Match(match_expr) => {
                println!(
                    "[DEBUG] substitute_expression_with_var_map: Match expression with {} arms",
                    match_expr.arms.len()
                );

                // Substitute in the match expression
                let expr = Box::new(self.substitute_expression_with_var_map(
                    &match_expr.expr,
                    param_map,
                    var_id_to_name,
                )?);

                // Substitute in each arm
                let mut arms = Vec::new();
                for (i, arm) in match_expr.arms.iter().enumerate() {
                    println!(
                        "[DEBUG] Match: processing arm {}, pattern: {:?}, expr type: {:?}",
                        i,
                        arm.pattern,
                        std::mem::discriminant(&arm.expr)
                    );

                    let guard = if let Some(guard_expr) = &arm.guard {
                        self.substitute_expression_with_var_map(
                            guard_expr,
                            param_map,
                            var_id_to_name,
                        )
                    } else {
                        None
                    };

                    let arm_expr_result = self.substitute_expression_with_var_map(
                        &arm.expr,
                        param_map,
                        var_id_to_name,
                    );

                    if arm_expr_result.is_none() {
                        eprintln!(
                            "[DEBUG] Match arm {}: arm expression substitution FAILED",
                            i
                        );
                        return None;
                    }
                    let arm_expr = arm_expr_result?;
                    eprintln!(
                        "[DEBUG] Match arm {}: arm expression substitution succeeded",
                        i
                    );

                    arms.push(hir::HirMatchArmExpr {
                        pattern: arm.pattern.clone(),
                        guard,
                        expr: arm_expr,
                    });
                }

                eprintln!(
                    "[DEBUG] Match: substitution complete, final arms count: {}",
                    arms.len()
                );
                Some(hir::HirExpression::Match(hir::HirMatchExpr {
                    expr,
                    arms,
                    mux_style: match_expr.mux_style,
                }))
            }

            // Block expression - substitute statements and result
            hir::HirExpression::Block {
                statements,
                result_expr,
            } => {
                println!(
                    "[DEBUG] **MATCHED** Block substitution: {} statements, param_map has {} entries",
                    statements.len(),
                    param_map.len()
                );
                if statements.is_empty() {
                    println!(
                        "[DEBUG]   param_map keys: {:?}",
                        param_map.keys().collect::<Vec<_>>()
                    );
                    println!(
                        "[DEBUG]   result_expr type: {:?}",
                        std::mem::discriminant(&**result_expr)
                    );
                }
                // BUG FIX #18: Use owned HashMap for local variables to allow incremental updates
                // This allows each statement to reference previously defined local variables
                let mut local_var_map: IndexMap<String, hir::HirExpression> = IndexMap::new();

                // BUG FIX #86: Track mutable variable names to exclude from result_expr substitution
                // Mutable variables should NOT be substituted in result_expr because they need
                // to be transformed by try_transform_block_mutable_vars later
                let mut mutable_var_names: std::collections::HashSet<String> =
                    std::collections::HashSet::new();

                // Substitute all statements in the block
                let mut substituted_statements = Vec::new();
                for (i, stmt) in statements.iter().enumerate() {
                    match stmt {
                        hir::HirStatement::Let(let_stmt) => {
                            eprintln!(
                                "[DEBUG] Block: substituting let statement {} ({})",
                                i, let_stmt.name
                            );
                            // Build a combined map with both params and local vars for substitution
                            let mut combined_map: IndexMap<String, &hir::HirExpression> =
                                param_map.clone();
                            for (name, expr) in &local_var_map {
                                combined_map.insert(name.clone(), expr);
                            }

                            // BUG #200 DEBUG: Check what the let value is before substitution
                            println!(
                                "[BUG #200 BLOCK] Let '{}' value type BEFORE sub: {:?}",
                                let_stmt.name,
                                std::mem::discriminant(&let_stmt.value)
                            );
                            if let hir::HirExpression::StructLiteral(sl) = &let_stmt.value {
                                println!(
                                    "[BUG #200 BLOCK] Let '{}' is StructLiteral type='{}'",
                                    let_stmt.name, sl.type_name
                                );
                            }
                            let substituted_value = self.substitute_expression_with_var_map(
                                &let_stmt.value,
                                &combined_map,
                                var_id_to_name,
                            )?;
                            println!(
                                "[BUG #200 BLOCK] Let '{}' value type AFTER sub: {:?}",
                                let_stmt.name,
                                std::mem::discriminant(&substituted_value)
                            );
                            if let hir::HirExpression::Match(m) = &substituted_value {
                                eprintln!(
                                    "[DEBUG] Block: let {} = Match with {} arms",
                                    let_stmt.name,
                                    m.arms.len()
                                );
                            }
                            substituted_statements.push(hir::HirStatement::Let(
                                hir::HirLetStatement {
                                    id: let_stmt.id,
                                    name: let_stmt.name.clone(),
                                    mutable: let_stmt.mutable, // Bug #78: Preserve mutability
                                    value: substituted_value.clone(),
                                    var_type: let_stmt.var_type.clone(),
                                },
                            ));
                            // BUG FIX #86: Only add IMMUTABLE variables to local_var_map.
                            // Mutable variables should NOT be substituted in If conditions because
                            // their values change. They need to be preserved as Variable references
                            // so that try_transform_block_mutable_vars can properly track their
                            // SSA-style updates.
                            if let_stmt.mutable {
                                eprintln!(
                                    "[BUG #86] Block: tracking mutable variable '{}' (NOT added to local_var_map)",
                                    let_stmt.name
                                );
                                mutable_var_names.insert(let_stmt.name.clone());
                            } else {
                                // Check if this is a placeholder signal (value is Literal(0) or similar)
                                // Placeholder signals are output wires that receive their value from entity outputs
                                // They should NOT be substituted in the result expression
                                let is_placeholder = matches!(
                                    &substituted_value,
                                    hir::HirExpression::Literal(hir::HirLiteral::Integer(0))
                                );
                                if is_placeholder {
                                    eprintln!(
                                        "[DEBUG] Block: '{}' is placeholder signal (value=0), NOT adding to local_var_map",
                                        let_stmt.name
                                    );
                                } else {
                                    // Add immutable variable to local map for subsequent statements
                                    // Use the actual substituted value (e.g., StructLiteral) instead of a Variable reference
                                    // Example: let ray_dir = vec3{x,y,z}; vec_dot(ray_dir, ...) should inline the vec3 literal
                                    local_var_map.insert(let_stmt.name.clone(), substituted_value);
                                    eprintln!(
                                        "[DEBUG] Block: added {} (id {:?}) to local_var_map immediately",
                                        let_stmt.name, let_stmt.id
                                    );
                                }
                            }
                        }
                        // BUG #86: Handle If statements with parameter substitution
                        hir::HirStatement::If(if_stmt) => {
                            eprintln!(
                                "[BUG #86] substitute_expression_with_var_map: processing If statement in block"
                            );
                            // Build combined map for substitution
                            let mut combined_map: IndexMap<String, &hir::HirExpression> =
                                param_map.clone();
                            for (name, expr) in &local_var_map {
                                combined_map.insert(name.clone(), expr);
                            }

                            // Substitute condition
                            let sub_cond = self.substitute_expression_with_var_map(
                                &if_stmt.condition,
                                &combined_map,
                                var_id_to_name,
                            )?;

                            // Substitute then statements
                            let sub_then: Vec<_> = if_stmt
                                .then_statements
                                .iter()
                                .filter_map(|s| {
                                    self.substitute_hir_stmt_in_block(
                                        s,
                                        &combined_map,
                                        var_id_to_name,
                                    )
                                })
                                .collect();

                            // Substitute else statements
                            let sub_else: Option<Vec<_>> =
                                if_stmt.else_statements.as_ref().map(|stmts| {
                                    stmts
                                        .iter()
                                        .filter_map(|s| {
                                            self.substitute_hir_stmt_in_block(
                                                s,
                                                &combined_map,
                                                var_id_to_name,
                                            )
                                        })
                                        .collect()
                                });

                            substituted_statements.push(hir::HirStatement::If(
                                hir::HirIfStatement {
                                    condition: sub_cond,
                                    then_statements: sub_then,
                                    else_statements: sub_else,
                                    mux_style: if_stmt.mux_style,
                                },
                            ));
                        }
                        // BUG #86: Handle Assignment statements with parameter substitution
                        hir::HirStatement::Assignment(assign_stmt) => {
                            eprintln!(
                                "[BUG #86] substitute_expression_with_var_map: processing Assignment statement in block"
                            );
                            // Build combined map for substitution
                            let mut combined_map: IndexMap<String, &hir::HirExpression> =
                                param_map.clone();
                            for (name, expr) in &local_var_map {
                                combined_map.insert(name.clone(), expr);
                            }

                            let sub_rhs = self.substitute_expression_with_var_map(
                                &assign_stmt.rhs,
                                &combined_map,
                                var_id_to_name,
                            )?;

                            substituted_statements.push(hir::HirStatement::Assignment(
                                hir::HirAssignment {
                                    id: assign_stmt.id,
                                    lhs: assign_stmt.lhs.clone(),
                                    rhs: sub_rhs,
                                    assignment_type: assign_stmt.assignment_type.clone(),
                                },
                            ));
                        }
                        _ => {
                            substituted_statements.push(stmt.clone());
                        }
                    }
                }

                // Build final combined map for result expression
                //
                // BUG FIX #89 + #91: Selective substitution of local_var_map in result_expr
                //
                // PROBLEM #89: When result_expr is `Variable(result_32)` and local_var_map contains
                // `"result_32" -> Match{...}`, the substitution replaces the variable with the
                // ENTIRE match expression. This causes the match to be computed twice.
                //
                // PROBLEM #91: When result_expr is `TupleLiteral(rx, ry, rz)` and local_var_map
                // contains `"rx" -> Call(fp_add)`, NOT substituting causes the variables to be
                // unresolved, resulting in constants (0) being generated instead of the actual values.
                //
                // SOLUTION: Create a filtered local_var_map that EXCLUDES complex expressions
                // (like Match) but INCLUDES simple expressions (like Call, Literal, Variable).
                // This allows tuple returns with function call results to work correctly,
                // while preventing duplication of complex match expressions.
                eprintln!(
                    "[BUG #91] Building filtered_local_map from {} entries",
                    local_var_map.len()
                );
                for (name, expr) in &local_var_map {
                    eprintln!(
                        "[BUG #91]   local_var_map['{}'] = {:?}",
                        name,
                        std::mem::discriminant(expr)
                    );
                }
                let mut filtered_local_map = local_var_map
                    .iter()
                    .filter(|(name, expr)| {
                        // BUG #86: Exclude mutable variables - they need to be preserved for
                        // try_transform_block_mutable_vars to work correctly
                        if mutable_var_names.contains(*name) {
                            eprintln!(
                                "[BUG #86] Excluding mutable variable '{}' from result_expr substitution",
                                name
                            );
                            return false;
                        }
                        // BUG #89 FIX: Exclude Match expressions from substitution to prevent
                        // double computation. Match expressions are complex and should be evaluated
                        // via the let binding, not inlined into result_expr.
                        let should_include = !matches!(expr, hir::HirExpression::Match(_));
                        // Debug output for result_expr substitution decisions
                        #[allow(clippy::if_same_then_else)]
                        if !should_include {
                            eprintln!(
                                "[BUG #91] Excluding '{}' from result_expr substitution (is Match)",
                                name
                            );
                        } else {
                            eprintln!(
                                "[BUG #91] Including '{}' in result_expr substitution ({:?})",
                                name, std::mem::discriminant(expr)
                            );
                        }
                        should_include
                    })
                    .map(|(k, v)| (k.clone(), v))
                    .collect::<IndexMap<_, _>>();
                eprintln!(
                    "[BUG #91] filtered_local_map has {} entries (after filtering)",
                    filtered_local_map.len()
                );

                // Merge with param_map (params take precedence)
                for (k, v) in param_map {
                    filtered_local_map.insert(k.clone(), *v);
                }
                if statements.is_empty() {
                    println!(
                        "[DEBUG]   filtered_local_map after merge: {:?}",
                        filtered_local_map.keys().collect::<Vec<_>>()
                    );
                    println!(
                        "[DEBUG]   result_expr type: {:?}",
                        std::mem::discriminant(&**result_expr)
                    );
                    if let hir::HirExpression::Range(base, _, _) = &**result_expr {
                        debug_println!("[DEBUG]   result_expr is Range with base: {:?}", base);
                    }
                }

                let substituted_result = Box::new(self.substitute_expression_with_var_map(
                    result_expr,
                    &filtered_local_map, // BUG #91: Include filtered local vars
                    var_id_to_name,
                )?);

                eprintln!(
                    "[DEBUG] Block substitution complete: {} statements",
                    substituted_statements.len()
                );
                Some(hir::HirExpression::Block {
                    statements: substituted_statements,
                    result_expr: substituted_result,
                })
            }

            // Tuple literal - substitute each element
            hir::HirExpression::TupleLiteral(elements) => {
                eprintln!(
                    "[DEBUG] TupleLiteral substitution: {} elements",
                    elements.len()
                );
                let mut substituted_elements = Vec::new();
                for (i, elem) in elements.iter().enumerate() {
                    eprintln!(
                        "[DEBUG] TupleLiteral: substituting element {}, type: {:?}",
                        i,
                        std::mem::discriminant(elem)
                    );
                    let substituted =
                        self.substitute_expression_with_var_map(elem, param_map, var_id_to_name)?;
                    if let hir::HirExpression::Match(m) = &substituted {
                        eprintln!(
                            "[DEBUG] TupleLiteral element {}: substituted to Match with {} arms",
                            i,
                            m.arms.len()
                        );
                    }
                    substituted_elements.push(substituted);
                }
                Some(hir::HirExpression::TupleLiteral(substituted_elements))
            }

            // Range expression - substitute base, high, and low
            hir::HirExpression::Range(base, high, low) => {
                println!(
                    "[DEBUG] Range substitution: base type {:?}",
                    std::mem::discriminant(&**base)
                );
                let substituted_base = Box::new(self.substitute_expression_with_var_map(
                    base,
                    param_map,
                    var_id_to_name,
                )?);
                let substituted_high = Box::new(self.substitute_expression_with_var_map(
                    high,
                    param_map,
                    var_id_to_name,
                )?);
                let substituted_low = Box::new(self.substitute_expression_with_var_map(
                    low,
                    param_map,
                    var_id_to_name,
                )?);
                Some(hir::HirExpression::Range(
                    substituted_base,
                    substituted_high,
                    substituted_low,
                ))
            }

            // Index expression - substitute base and index
            hir::HirExpression::Index(base, index) => {
                println!(
                    "🔴🔴🔴 [CLZ_DEBUG] Index substitution: base type {:?}, index type {:?} 🔴🔴🔴",
                    std::mem::discriminant(&**base),
                    std::mem::discriminant(&**index)
                );
                let substituted_base = Box::new(self.substitute_expression_with_var_map(
                    base,
                    param_map,
                    var_id_to_name,
                )?);
                println!(
                    "🔴🔴🔴 [CLZ_DEBUG] Index substitution: substituted_base type {:?} 🔴🔴🔴",
                    std::mem::discriminant(&*substituted_base)
                );
                let substituted_index = Box::new(self.substitute_expression_with_var_map(
                    index,
                    param_map,
                    var_id_to_name,
                )?);
                Some(hir::HirExpression::Index(
                    substituted_base,
                    substituted_index,
                ))
            }

            // Call expression - substitute arguments
            hir::HirExpression::Call(call) => {
                eprintln!(
                    "[DEBUG] Call substitution: function={}, args={} BEFORE substitution",
                    call.function,
                    call.args.len()
                );
                for (i, arg) in call.args.iter().enumerate() {
                    eprintln!("  arg[{}]: type={:?}", i, std::mem::discriminant(arg));
                }

                let mut substituted_args = Vec::new();
                for (i, arg) in call.args.iter().enumerate() {
                    debug_println!("[DEBUG] Call substitution: substituting arg[{}]", i);
                    let substituted_arg =
                        self.substitute_expression_with_var_map(arg, param_map, var_id_to_name)?;
                    eprintln!(
                        "[DEBUG] Call substitution: arg[{}] substituted successfully, type={:?}",
                        i,
                        std::mem::discriminant(&substituted_arg)
                    );
                    substituted_args.push(substituted_arg);
                }
                eprintln!(
                    "[DEBUG] Call substitution: function={}, {} args AFTER substitution",
                    call.function,
                    substituted_args.len()
                );
                Some(hir::HirExpression::Call(hir::HirCallExpr {
                    function: call.function.clone(),
                    type_args: call.type_args.clone(), // Preserve type args during parameter substitution
                    named_type_args: call.named_type_args.clone(),
                    args: substituted_args,
                    impl_style: call.impl_style,
                }))
            }

            // For other expressions that don't contain parameters, clone as-is
            hir::HirExpression::Literal(_)
            | hir::HirExpression::Signal(_)
            | hir::HirExpression::Port(_)
            | hir::HirExpression::Constant(_) => Some(expr.clone()),

            // Cast expression - substitute the inner expression
            hir::HirExpression::Cast(cast) => {
                println!(
                    "[DEBUG] Cast substitution: inner expr type: {:?}, target type: {:?}",
                    std::mem::discriminant(&*cast.expr),
                    cast.target_type
                );
                let substituted_expr = Box::new(self.substitute_expression_with_var_map(
                    &cast.expr,
                    param_map,
                    var_id_to_name,
                )?);
                debug_println!("[DEBUG] Cast substitution: successfully substituted inner expr");
                Some(hir::HirExpression::Cast(hir::HirCastExpr {
                    expr: substituted_expr,
                    target_type: cast.target_type.clone(),
                }))
            }

            // Concat expression - substitute all elements
            hir::HirExpression::Concat(elements) => {
                debug_println!("[DEBUG] Concat substitution: {} elements", elements.len());
                let mut substituted_elements = Vec::new();
                for (i, elem) in elements.iter().enumerate() {
                    eprintln!(
                        "[DEBUG] Concat: substituting element {}, type: {:?}",
                        i,
                        std::mem::discriminant(elem)
                    );
                    let substituted =
                        self.substitute_expression_with_var_map(elem, param_map, var_id_to_name)?;
                    substituted_elements.push(substituted);
                }
                eprintln!(
                    "[DEBUG] Concat substitution: successfully substituted all {} elements",
                    substituted_elements.len()
                );
                Some(hir::HirExpression::Concat(substituted_elements))
            }

            // FieldAccess expression - substitute base expression
            hir::HirExpression::FieldAccess { base, field } => {
                eprintln!(
                    "[DEBUG] FieldAccess substitution: field={}, base type before: {:?}",
                    field,
                    std::mem::discriminant(&**base)
                );
                let substituted_base = Box::new(self.substitute_expression_with_var_map(
                    base,
                    param_map,
                    var_id_to_name,
                )?);
                eprintln!(
                    "[DEBUG] FieldAccess substitution: successfully substituted base, type after: {:?}",
                    std::mem::discriminant(&*substituted_base)
                );
                Some(hir::HirExpression::FieldAccess {
                    base: substituted_base,
                    field: field.clone(),
                })
            }

            // StructLiteral expression - substitute field values
            hir::HirExpression::StructLiteral(struct_lit) => {
                println!(
                    "[DEBUG] StructLiteral substitution: type={}, {} fields BEFORE",
                    struct_lit.type_name,
                    struct_lit.fields.len()
                );

                // BUG #200 FIX: Check if this is an entity struct literal
                // For entity struct literals, we need to substitute GenericParam references
                // (the trait method parameters like 'a', 'b') with the actual arguments,
                // BUT we should NOT further expand Variable references to their source expressions.
                // Example: GenericParam("a") -> Variable(a_fp32) is OK
                //          Variable(a_fp32) -> (a[15:0] as fp16 as fp32) is NOT OK
                let is_entity_struct = self
                    .hir
                    .as_ref()
                    .is_some_and(|hir| hir.entities.iter().any(|e| e.name == struct_lit.type_name));
                println!(
                    "[BUG #200 TRACE] StructLiteral type='{}', is_entity_struct={}",
                    struct_lit.type_name, is_entity_struct
                );

                let mut substituted_fields = Vec::new();
                for (i, field_init) in struct_lit.fields.iter().enumerate() {
                    println!(
                        "[DEBUG] StructLiteral field[{}]: name={}, value type={:?}",
                        i,
                        field_init.name,
                        std::mem::discriminant(&field_init.value)
                    );

                    let substituted_value = if is_entity_struct {
                        // BUG #200: For entity struct literals, only substitute GenericParams
                        // using param_map, but don't do full recursive substitution
                        println!(
                            "[BUG #200] Entity struct '{}' field '{}': using param-only substitution",
                            struct_lit.type_name, field_init.name
                        );
                        // BUG #200 DEBUG: Show detailed field value structure
                        fn show_expr_structure(
                            expr: &hir::HirExpression,
                            prefix: &str,
                            depth: usize,
                        ) {
                            let indent = "  ".repeat(depth);
                            match expr {
                                hir::HirExpression::Cast(c) => {
                                    println!("{}{prefix}Cast(target={:?})", indent, c.target_type);
                                    show_expr_structure(&c.expr, prefix, depth + 1);
                                }
                                hir::HirExpression::Variable(v) => {
                                    println!("{}{prefix}Variable({:?})", indent, v);
                                }
                                hir::HirExpression::GenericParam(n) => {
                                    println!("{}{prefix}GenericParam(\"{}\")", indent, n);
                                }
                                hir::HirExpression::Port(p) => {
                                    println!("{}{prefix}Port({:?})", indent, p);
                                }
                                hir::HirExpression::Signal(s) => {
                                    println!("{}{prefix}Signal({:?})", indent, s);
                                }
                                _ => {
                                    println!(
                                        "{}{prefix}Other({:?})",
                                        indent,
                                        std::mem::discriminant(expr)
                                    );
                                }
                            }
                        }
                        println!(
                            "[BUG #200 FIELD] '{}' BEFORE param-only sub:",
                            field_init.name
                        );
                        show_expr_structure(&field_init.value, "[BUG #200 FIELD] ", 1);
                        let result =
                            self.substitute_generic_params_only(&field_init.value, param_map);
                        println!(
                            "[BUG #200 FIELD] '{}' AFTER param-only sub:",
                            field_init.name
                        );
                        show_expr_structure(&result, "[BUG #200 FIELD] ", 1);
                        result
                    } else {
                        // For regular structs, do full recursive substitution
                        self.substitute_expression_with_var_map(
                            &field_init.value,
                            param_map,
                            var_id_to_name,
                        )?
                    };

                    substituted_fields.push(hir::HirStructFieldInit {
                        name: field_init.name.clone(),
                        value: substituted_value,
                    });
                }
                println!(
                    "[DEBUG] StructLiteral substitution: type={}, {} fields AFTER",
                    struct_lit.type_name,
                    substituted_fields.len()
                );
                Some(hir::HirExpression::StructLiteral(hir::HirStructLiteral {
                    type_name: struct_lit.type_name.clone(),
                    generic_args: struct_lit.generic_args.clone(),
                    fields: substituted_fields,
                }))
            }

            // Recursively handle other expression types as needed
            _ => {
                // For unhandled cases, clone as-is (may need expansion for full support)
                let variant_name = match expr {
                    hir::HirExpression::Literal(_) => "Literal",
                    hir::HirExpression::Signal(_) => "Signal",
                    hir::HirExpression::Port(_) => "Port",
                    hir::HirExpression::Variable(_) => "Variable",
                    hir::HirExpression::Constant(_) => "Constant",
                    hir::HirExpression::GenericParam(_) => "GenericParam",
                    hir::HirExpression::Binary(_) => "Binary",
                    hir::HirExpression::Unary(_) => "Unary",
                    hir::HirExpression::Call(_) => "Call",
                    hir::HirExpression::Index(_, _) => "Index",
                    hir::HirExpression::Range(_, _, _) => "Range",
                    hir::HirExpression::FieldAccess { .. } => "FieldAccess",
                    hir::HirExpression::EnumVariant { .. } => "EnumVariant",
                    hir::HirExpression::AssociatedConstant { .. } => "AssociatedConstant",
                    hir::HirExpression::ArrayRepeat { .. } => "ArrayRepeat",
                    hir::HirExpression::Concat(_) => "Concat",
                    hir::HirExpression::Ternary { .. } => "Ternary",
                    hir::HirExpression::StructLiteral(_) => "StructLiteral",
                    hir::HirExpression::TupleLiteral(_) => "TupleLiteral",
                    hir::HirExpression::ArrayLiteral(_) => "ArrayLiteral",
                    hir::HirExpression::If(_) => "If",
                    hir::HirExpression::Match(_) => "Match",
                    hir::HirExpression::Cast(_) => "Cast",
                    hir::HirExpression::Block { .. } => "Block",
                };
                eprintln!(
                    "[DEBUG] substitute_expression_with_var_map: unhandled expression type: {:?} ({})",
                    std::mem::discriminant(expr),
                    variant_name
                );
                Some(expr.clone())
            }
        }
    }

    /// BUG #200 FIX: Substitute only GenericParam references in an expression.
    /// This is used for entity struct literal fields where we need to replace
    /// the trait method parameters (like 'a', 'b') with the actual arguments,
    /// but we DON'T want to further expand Variable references to their source expressions.
    #[allow(clippy::only_used_in_recursion)]
    fn substitute_generic_params_only(
        &self,
        expr: &hir::HirExpression,
        param_map: &IndexMap<String, &hir::HirExpression>,
    ) -> hir::HirExpression {
        match expr {
            // GenericParam - substitute with param_map value
            hir::HirExpression::GenericParam(name) => {
                if let Some(arg_expr) = param_map.get(name) {
                    eprintln!(
                        "[BUG #200] Substituting GenericParam('{}') with {:?}",
                        name,
                        std::mem::discriminant(*arg_expr)
                    );
                    (*arg_expr).clone()
                } else {
                    // Not a parameter, keep as-is
                    expr.clone()
                }
            }

            // Variable - keep as-is, do NOT expand
            hir::HirExpression::Variable(_) => expr.clone(),

            // Cast - recursively process inner expression
            hir::HirExpression::Cast(cast_expr) => {
                let inner = self.substitute_generic_params_only(&cast_expr.expr, param_map);
                hir::HirExpression::Cast(hir::HirCastExpr {
                    expr: Box::new(inner),
                    target_type: cast_expr.target_type.clone(),
                })
            }

            // Range - recursively process base (high/low are typically literals)
            hir::HirExpression::Range(base, high, low) => {
                let new_base = self.substitute_generic_params_only(base, param_map);
                let new_high = self.substitute_generic_params_only(high, param_map);
                let new_low = self.substitute_generic_params_only(low, param_map);
                hir::HirExpression::Range(Box::new(new_base), Box::new(new_high), Box::new(new_low))
            }

            // Binary - recursively process both sides
            hir::HirExpression::Binary(bin) => {
                let new_left = self.substitute_generic_params_only(&bin.left, param_map);
                let new_right = self.substitute_generic_params_only(&bin.right, param_map);
                hir::HirExpression::Binary(hir::HirBinaryExpr {
                    left: Box::new(new_left),
                    op: bin.op.clone(),
                    right: Box::new(new_right),
                    is_trait_op: bin.is_trait_op,
                })
            }

            // Unary - recursively process operand
            hir::HirExpression::Unary(un) => {
                let new_operand = self.substitute_generic_params_only(&un.operand, param_map);
                hir::HirExpression::Unary(hir::HirUnaryExpr {
                    op: un.op.clone(),
                    operand: Box::new(new_operand),
                })
            }

            // Other expressions - return as-is
            _ => expr.clone(),
        }
    }

    /// Find a variable name by ID (helper for parameter substitution)
    fn find_variable_name(&self, var_id: hir::VariableId) -> Option<String> {
        let hir = self.hir?;

        // Check current implementation's variables
        if let Some(entity_id) = self.current_entity_id {
            if let Some(impl_block) = hir
                .implementations
                .iter()
                .find(|impl_b| impl_b.entity == entity_id)
            {
                for var in &impl_block.variables {
                    if var.id == var_id {
                        return Some(var.name.clone());
                    }
                }
            }
        }

        // Check dynamic variables (from let bindings)
        if let Some((_, name, _)) = self.dynamic_variables.get(&var_id) {
            return Some(name.clone());
        }

        None
    }

    /// Convert HIR type to frontend Type for MIR expression annotation (BUG #76 FIX)
    #[allow(clippy::only_used_in_recursion)]
    fn hir_type_to_type(&self, hir_type: &hir::HirType) -> Type {
        use skalp_frontend::types::Width;

        match hir_type {
            hir::HirType::Bit(width) => Type::Bit(Width::Fixed(*width)),
            hir::HirType::Logic(width) => Type::Logic(Width::Fixed(*width)),
            hir::HirType::Int(width) => Type::Int(Width::Fixed(*width)),
            hir::HirType::Nat(width) => Type::Nat(Width::Fixed(*width)),
            hir::HirType::Bool => Type::Bool,
            hir::HirType::Clock(_) => Type::Clock(None),
            hir::HirType::Reset { .. } => {
                Type::Reset(skalp_frontend::types::ResetPolarity::ActiveHigh)
            }
            hir::HirType::Event => Type::Event,
            hir::HirType::Tuple(elements) => {
                Type::Tuple(elements.iter().map(|t| self.hir_type_to_type(t)).collect())
            }
            hir::HirType::Array(elem_type, size) => Type::Array {
                element_type: Box::new(self.hir_type_to_type(elem_type)),
                size: *size,
            },
            _ => Type::Unknown, // For unsupported types (including Custom)
        }
    }

    /// Infer the type of a HIR expression from context (BUG #76 FIX - full Option A)
    fn infer_hir_expression_type(&self, expr: &hir::HirExpression, depth: usize) -> Type {
        // Guard against stack overflow on deeply nested expressions
        if depth > MAX_EXPRESSION_RECURSION_DEPTH {
            panic!(
                "Expression recursion depth exceeded {} - likely infinite recursion in nested concat/tuple expressions",
                MAX_EXPRESSION_RECURSION_DEPTH
            );
        }

        match expr {
            hir::HirExpression::Literal(lit) => self.infer_literal_type(lit),

            hir::HirExpression::Signal(id) => {
                // Look up signal type in HIR
                if let Some(hir) = self.hir {
                    for impl_block in &hir.implementations {
                        if let Some(signal) = impl_block.signals.iter().find(|s| s.id == *id) {
                            return self.hir_type_to_type(&signal.signal_type);
                        }
                    }
                }
                Type::Unknown
            }

            hir::HirExpression::Port(id) => {
                // Look up port type in HIR
                if let Some(hir) = self.hir {
                    for entity in &hir.entities {
                        if let Some(port) = entity.ports.iter().find(|p| p.id == *id) {
                            return self.hir_type_to_type(&port.port_type);
                        }
                    }
                }
                Type::Unknown
            }

            hir::HirExpression::Variable(id) => {
                // Look up variable type - check multiple sources
                // 1. Context-aware map
                if let Some(context) = self.get_current_context() {
                    let key = (Some(context), *id);
                    if let Some(&mir_id) = self.context_variable_map.get(&key) {
                        if let Some(hir_type) = self.mir_variable_types.get(&mir_id) {
                            return self.hir_type_to_type(hir_type);
                        }
                    }
                }

                // 2. Dynamic variables
                if let Some((_, _, hir_type)) = self.dynamic_variables.get(id) {
                    return self.hir_type_to_type(hir_type);
                }

                // 3. Variable map + type map
                if let Some(&mir_id) = self.variable_map.get(id) {
                    if let Some(hir_type) = self.mir_variable_types.get(&mir_id) {
                        return self.hir_type_to_type(hir_type);
                    }
                }

                Type::Unknown
            }

            hir::HirExpression::Binary(bin) => {
                // Type depends on operation - usually inherits from operands

                self.infer_hir_expression_type(&bin.left, depth + 1) // Simplified - proper inference would consider operation
            }

            hir::HirExpression::Concat(exprs) => {
                // Concat creates a tuple type
                let element_types: Vec<Type> = exprs
                    .iter()
                    .map(|e| self.infer_hir_expression_type(e, depth + 1))
                    .collect();
                Type::Tuple(element_types)
            }

            hir::HirExpression::Call(call) => {
                // Look up function return type
                self.infer_function_return_type(&call.function)
            }

            _ => Type::Unknown,
        }
    }

    fn infer_literal_type(&self, lit: &hir::HirLiteral) -> Type {
        use skalp_frontend::types::{Type, Width};

        match lit {
            hir::HirLiteral::Integer(_) => Type::Bit(Width::Fixed(32)), // Default to 32-bit
            hir::HirLiteral::BitVector(bits) => Type::Bit(Width::Fixed(bits.len() as u32)),
            hir::HirLiteral::Boolean(_) => Type::Bool,
            hir::HirLiteral::String(_) => Type::Unknown,
            _ => Type::Unknown,
        }
    }

    /// BUG #160 FIX: Get the bit width from a Type, if it's a fixed-width bit type
    fn get_type_width(&self, ty: &Type) -> Option<u32> {
        use skalp_frontend::types::Width;
        match ty {
            Type::Bit(Width::Fixed(w))
            | Type::Logic(Width::Fixed(w))
            | Type::Int(Width::Fixed(w))
            | Type::Nat(Width::Fixed(w)) => Some(*w),
            Type::Bool => Some(1),
            _ => None,
        }
    }

    /// BUG #160 FIX: Infer the bit width of an HIR expression
    fn infer_hir_expression_width(&self, expr: &hir::HirExpression) -> Option<u32> {
        match expr {
            hir::HirExpression::Literal(lit) => {
                match lit {
                    hir::HirLiteral::Integer(_) => None, // Unknown width for bare integers
                    hir::HirLiteral::BitVector(bits) => Some(bits.len() as u32),
                    hir::HirLiteral::Boolean(_) => Some(1),
                    _ => None,
                }
            }
            hir::HirExpression::Range(_base, high, low) => {
                // Width is high - low + 1 if both are constants
                if let (
                    hir::HirExpression::Literal(hir::HirLiteral::Integer(h)),
                    hir::HirExpression::Literal(hir::HirLiteral::Integer(l)),
                ) = (high.as_ref(), low.as_ref())
                {
                    Some((h - l + 1) as u32)
                } else {
                    None
                }
            }
            hir::HirExpression::Cast(cast_expr) => {
                // Cast has explicit target width
                match &cast_expr.target_type {
                    hir::HirType::Bit(w)
                    | hir::HirType::Logic(w)
                    | hir::HirType::Int(w)
                    | hir::HirType::Nat(w) => Some(*w),
                    _ => None,
                }
            }
            hir::HirExpression::Variable(var_id) => {
                // Try to look up the variable's type from dynamic_variables first
                if let Some((
                    _,
                    _,
                    hir::HirType::Bit(w)
                    | hir::HirType::Logic(w)
                    | hir::HirType::Int(w)
                    | hir::HirType::Nat(w),
                )) = self.dynamic_variables.get(var_id)
                {
                    return Some(*w);
                }

                // Fall through: Try to get type from HIR if available
                if let Some(hir) = self.hir {
                    // Look through let bindings
                    for impl_block in &hir.implementations {
                        for stmt in &impl_block.statements {
                            if let hir::HirStatement::Let(let_stmt) = stmt {
                                if let_stmt.id == *var_id {
                                    return match &let_stmt.var_type {
                                        hir::HirType::Bit(w)
                                        | hir::HirType::Logic(w)
                                        | hir::HirType::Int(w)
                                        | hir::HirType::Nat(w) => Some(*w),
                                        _ => None,
                                    };
                                }
                            }
                        }
                    }
                }
                None
            }
            hir::HirExpression::Signal(signal_id) => {
                // Look up signal type
                if let Some(hir) = self.hir {
                    for entity in &hir.entities {
                        for signal in &entity.signals {
                            if signal.id == *signal_id {
                                return match &signal.signal_type {
                                    hir::HirType::Bit(w)
                                    | hir::HirType::Logic(w)
                                    | hir::HirType::Int(w)
                                    | hir::HirType::Nat(w) => Some(*w),
                                    _ => None,
                                };
                            }
                        }
                    }
                }
                None
            }
            hir::HirExpression::Port(port_id) => {
                // Look up port type
                if let Some(hir) = self.hir {
                    for entity in &hir.entities {
                        for port in &entity.ports {
                            if port.id == *port_id {
                                return match &port.port_type {
                                    hir::HirType::Bit(w)
                                    | hir::HirType::Logic(w)
                                    | hir::HirType::Int(w)
                                    | hir::HirType::Nat(w) => Some(*w),
                                    _ => None,
                                };
                            }
                        }
                    }
                }
                None
            }
            hir::HirExpression::Concat(parts) => {
                // Sum of all part widths, if all parts have known widths
                let mut total = 0u32;
                for part in parts {
                    if let Some(w) = self.infer_hir_expression_width(part) {
                        total += w;
                    } else {
                        return None;
                    }
                }
                Some(total)
            }
            _ => None,
        }
    }

    fn infer_function_return_type(&self, func_name: &str) -> Type {
        if let Some(hir) = self.hir {
            // Search in implementations
            for impl_block in &hir.implementations {
                if let Some(func) = impl_block.functions.iter().find(|f| f.name == func_name) {
                    if let Some(ref return_type) = func.return_type {
                        return self.hir_type_to_type(return_type);
                    }
                }
            }
            // Search in top-level functions
            if let Some(func) = hir.functions.iter().find(|f| f.name == func_name) {
                if let Some(ref return_type) = func.return_type {
                    return self.hir_type_to_type(return_type);
                }
            }
        }
        Type::Unknown
    }

    /// Annotate an expression with type information (BUG #76 FIX)
    /// For Concat expressions representing tuples, recursively annotate parts with element types
    #[allow(clippy::only_used_in_recursion)]
    fn annotate_expression_with_type(
        &self,
        expr: Expression,
        ty: Type,
        depth: usize,
    ) -> Expression {
        // Guard against stack overflow on deeply nested expressions
        if depth > MAX_EXPRESSION_RECURSION_DEPTH {
            panic!(
                "Expression annotation recursion depth exceeded {} - likely infinite recursion in nested concat/tuple expressions",
                MAX_EXPRESSION_RECURSION_DEPTH
            );
        }

        eprintln!(
            "[BUG #76 ANNOTATE] annotate_expression_with_type: target type = {:?}",
            ty
        );
        eprintln!(
            "[BUG #76 ANNOTATE]   expr.kind = {:?}",
            std::mem::discriminant(&expr.kind)
        );
        debug_println!("[BUG #76 ANNOTATE]   expr.ty (before) = {:?}", expr.ty);

        match &expr.kind {
            // If this is a Concat and the type is a Tuple, annotate each part with its element type
            ExpressionKind::Concat(parts) => {
                eprintln!(
                    "[BUG #76 ANNOTATE]   This is a Concat with {} parts",
                    parts.len()
                );
                if let Type::Tuple(element_types) = &ty {
                    eprintln!(
                        "[BUG #76 ANNOTATE]   Target type is Tuple with {} element types",
                        element_types.len()
                    );
                    // Recursively annotate each concat part with its corresponding tuple element type
                    if parts.len() == element_types.len() {
                        eprintln!(
                            "[BUG #76 ANNOTATE]   Lengths match! Annotating each part recursively"
                        );
                        let annotated_parts: Vec<Expression> = parts
                            .iter()
                            .zip(element_types.iter())
                            .enumerate()
                            .map(|(i, (part, elem_type))| {
                                eprintln!(
                                    "[BUG #76 ANNOTATE]     Part {}: elem_type = {:?}",
                                    i, elem_type
                                );
                                self.annotate_expression_with_type(
                                    part.clone(),
                                    elem_type.clone(),
                                    depth + 1,
                                )
                            })
                            .collect();

                        let result = Expression {
                            kind: ExpressionKind::Concat(annotated_parts),
                            ty: ty.clone(),
                            span: None,
                        };
                        eprintln!(
                            "[BUG #76 ANNOTATE]   Returning annotated Concat with type: {:?}",
                            result.ty
                        );
                        return result;
                    } else {
                        eprintln!(
                            "[BUG #76 ANNOTATE]   Length mismatch! parts={}, types={}",
                            parts.len(),
                            element_types.len()
                        );
                    }
                }
            }

            // If this is a Conditional, recursively annotate both branches with the same type
            ExpressionKind::Conditional {
                cond,
                then_expr,
                else_expr,
            } => {
                eprintln!(
                    "[BUG #76 ANNOTATE]   This is a Conditional, recursively annotating branches"
                );
                let annotated_then = Box::new(self.annotate_expression_with_type(
                    then_expr.as_ref().clone(),
                    ty.clone(),
                    depth + 1,
                ));
                let annotated_else = Box::new(self.annotate_expression_with_type(
                    else_expr.as_ref().clone(),
                    ty.clone(),
                    depth + 1,
                ));

                let result = Expression {
                    kind: ExpressionKind::Conditional {
                        cond: cond.clone(),
                        then_expr: annotated_then,
                        else_expr: annotated_else,
                    },
                    ty: ty.clone(),
                    span: None,
                };
                eprintln!(
                    "[BUG #76 ANNOTATE]   Returning annotated Conditional with type: {:?}",
                    result.ty
                );
                return result;
            }

            // BUG #187 FIX: Handle Literal expressions to update the width
            ExpressionKind::Literal(ref value) => {
                let target_width = match &ty {
                    Type::Bit(skalp_frontend::types::Width::Fixed(w)) => Some(*w),
                    Type::Int(skalp_frontend::types::Width::Fixed(w)) => Some(*w),
                    Type::Nat(skalp_frontend::types::Width::Fixed(w)) => Some(*w),
                    _ => None,
                };

                if let Some(target_width) = target_width {
                    let new_value = match value {
                        Value::BitVector { width, value } => {
                            println!(
                                "[BUG #187 FIX] Extending BitVector from {} bits to {} bits",
                                width, target_width
                            );
                            Value::BitVector {
                                width: target_width as usize,
                                value: *value,
                            }
                        }
                        Value::Integer(v) => {
                            println!(
                                "[BUG #187 FIX] Converting Integer {} to BitVector with {} bits",
                                v, target_width
                            );
                            Value::BitVector {
                                width: target_width as usize,
                                value: *v as u64,
                            }
                        }
                        other => other.clone(),
                    };

                    return Expression {
                        kind: ExpressionKind::Literal(new_value),
                        ty: ty.clone(),
                        span: None,
                    };
                }
            }

            _ => {}
        }

        // For other expressions, just update the type
        let result = Expression {
            kind: expr.kind,
            ty: ty.clone(),
            span: None,
        };
        eprintln!(
            "[BUG #76 ANNOTATE]   Returning expression with updated type: {:?}",
            result.ty
        );
        result
    }

    /// Check if a function body contains a recursive call (Phase 5)
    fn contains_recursive_call(&self, body: &[hir::HirStatement], func_name: &str) -> bool {
        for stmt in body {
            match stmt {
                hir::HirStatement::Return(Some(expr)) => {
                    if self.expression_contains_call(expr, func_name) {
                        return true;
                    }
                }
                hir::HirStatement::Let(let_stmt) => {
                    if self.expression_contains_call(&let_stmt.value, func_name) {
                        return true;
                    }
                }
                _ => {}
            }
        }
        false
    }

    /// Check if an expression contains a call to a specific function
    #[allow(clippy::only_used_in_recursion)]
    fn expression_contains_call(&self, expr: &hir::HirExpression, func_name: &str) -> bool {
        match expr {
            hir::HirExpression::Call(call) => {
                if call.function == func_name {
                    return true;
                }
                // Check arguments for nested calls
                for arg in &call.args {
                    if self.expression_contains_call(arg, func_name) {
                        return true;
                    }
                }
                false
            }
            hir::HirExpression::Binary(bin) => {
                self.expression_contains_call(&bin.left, func_name)
                    || self.expression_contains_call(&bin.right, func_name)
            }
            hir::HirExpression::Unary(un) => self.expression_contains_call(&un.operand, func_name),
            hir::HirExpression::If(if_expr) => {
                self.expression_contains_call(&if_expr.condition, func_name)
                    || self.expression_contains_call(&if_expr.then_expr, func_name)
                    || self.expression_contains_call(&if_expr.else_expr, func_name)
            }
            hir::HirExpression::Match(match_expr) => {
                if self.expression_contains_call(&match_expr.expr, func_name) {
                    return true;
                }
                for arm in &match_expr.arms {
                    if let Some(guard) = &arm.guard {
                        if self.expression_contains_call(guard, func_name) {
                            return true;
                        }
                    }
                    if self.expression_contains_call(&arm.expr, func_name) {
                        return true;
                    }
                }
                false
            }
            _ => false,
        }
    }

    /// Inline a function call (Phases 2-5: simple functions, let bindings, control flow, recursion check)
    /// Recursively collect all let binding variable IDs and names from statements and expressions
    fn collect_let_bindings(
        &self,
        stmts: &[hir::HirStatement],
        map: &mut IndexMap<hir::VariableId, String>,
    ) {
        for stmt in stmts {
            match stmt {
                hir::HirStatement::Let(let_stmt) => {
                    map.insert(let_stmt.id, let_stmt.name.clone());
                    // Recursively collect from the let's value expression
                    self.collect_let_bindings_from_expr(&let_stmt.value, map);
                }
                hir::HirStatement::Return(Some(expr)) => {
                    self.collect_let_bindings_from_expr(expr, map);
                }
                hir::HirStatement::If(if_stmt) => {
                    self.collect_let_bindings(&if_stmt.then_statements, map);
                    if let Some(else_stmts) = &if_stmt.else_statements {
                        self.collect_let_bindings(else_stmts, map);
                    }
                }
                _ => {}
            }
        }
    }

    /// Recursively collect let bindings from an expression
    fn collect_let_bindings_from_expr(
        &self,
        expr: &hir::HirExpression,
        map: &mut IndexMap<hir::VariableId, String>,
    ) {
        match expr {
            hir::HirExpression::Block {
                statements,
                result_expr,
            } => {
                self.collect_let_bindings(statements, map);
                self.collect_let_bindings_from_expr(result_expr, map);
            }
            hir::HirExpression::Match(match_expr) => {
                for arm in &match_expr.arms {
                    self.collect_let_bindings_from_expr(&arm.expr, map);
                }
            }
            hir::HirExpression::If(if_expr) => {
                self.collect_let_bindings_from_expr(&if_expr.condition, map);
                self.collect_let_bindings_from_expr(&if_expr.then_expr, map);
                self.collect_let_bindings_from_expr(&if_expr.else_expr, map);
            }
            hir::HirExpression::Binary(bin_expr) => {
                self.collect_let_bindings_from_expr(&bin_expr.left, map);
                self.collect_let_bindings_from_expr(&bin_expr.right, map);
            }
            hir::HirExpression::Unary(un_expr) => {
                self.collect_let_bindings_from_expr(&un_expr.operand, map);
            }
            _ => {}
        }
    }

    /// Inline a function call and return the substituted HIR expression (before MIR conversion)
    /// This is used when we need to perform HIR-level operations on the inlined result
    /// (e.g., field access on struct-returning functions)
    fn inline_function_to_hir(&mut self, call: &hir::HirCallExpr) -> Option<hir::HirExpression> {
        // Find the function
        let func = self.find_function(&call.function)?;

        // Clone the data we need
        let params = func.params.clone();
        let body = func.body.clone();

        // Transform early returns
        let body = self.transform_early_returns(body);

        // Check for recursion
        if self.contains_recursive_call(&body, &call.function) {
            eprintln!(
                "Error: Recursive function calls are not supported: function '{}'",
                call.function
            );
            return None;
        }

        // Check arity
        if params.len() != call.args.len() {
            eprintln!(
                "Function {}: expected {} arguments, got {}",
                call.function,
                params.len(),
                call.args.len()
            );
            return None;
        }

        // Build parameter substitution map
        let mut substitution_map = IndexMap::new();
        for (param, arg) in params.iter().zip(&call.args) {
            substitution_map.insert(param.name.clone(), arg.clone());
        }

        // Build var_id to name mapping
        let mut var_id_to_name = IndexMap::new();
        self.collect_let_bindings(&body, &mut var_id_to_name);

        // Convert body to expression
        let body_expr = self.convert_body_to_expression(&body)?;

        // Substitute parameters
        let substituted_expr = self.substitute_expression_with_var_map(
            &body_expr,
            &substitution_map
                .iter()
                .map(|(k, v)| (k.clone(), v))
                .collect(),
            &var_id_to_name,
        )?;

        // Return the substituted HIR expression (don't convert to MIR)
        Some(substituted_expr)
    }

    fn inline_function_call_to_hir_with_lets(
        &mut self,
        call: &hir::HirCallExpr,
    ) -> Option<hir::HirExpression> {
        // Similar to inline_function_to_hir, but also inlines let bindings into the result expression
        // This produces a fully inlined HIR expression with no variable references

        eprintln!(
            "[BUG #74 INLINE_WITH_LETS] Inlining function '{}' with let binding resolution",
            call.function
        );

        // Find the function
        let func = self.find_function(&call.function)?;

        // Clone the data we need
        let params = func.params.clone();
        let body = func.body.clone();

        // Build a map from VariableId to variable name by walking through the function body
        // BUG FIX #123: Use actual VariableIds from HIR, not sequential assumptions!
        // The HIR builder assigns VariableIds which may not be sequential per function.
        let mut var_id_to_name = IndexMap::new();

        // Helper function to recursively collect let bindings using ACTUAL VariableIds
        fn collect_lets_recursive(
            stmts: &[hir::HirStatement],
            var_id_to_name: &mut IndexMap<hir::VariableId, String>,
        ) {
            for stmt in stmts {
                match stmt {
                    hir::HirStatement::Let(let_stmt) => {
                        // BUG FIX #123: Use let_stmt.id (actual VariableId) instead of sequential counter
                        var_id_to_name.insert(let_stmt.id, let_stmt.name.clone());
                        eprintln!(
                            "[BUG #123 VAR_MAP] Mapped VariableId({}) -> '{}' (actual HIR id)",
                            let_stmt.id.0, let_stmt.name
                        );
                    }
                    hir::HirStatement::If(if_stmt) => {
                        collect_lets_recursive(&if_stmt.then_statements, var_id_to_name);
                        if let Some(else_stmts) = &if_stmt.else_statements {
                            collect_lets_recursive(else_stmts, var_id_to_name);
                        }
                    }
                    _ => {}
                }
            }
        }

        // BUG FIX #75: Function parameters are also represented as Variables
        // NOTE: HirParameter doesn't have an id field, so we still assume 0, 1, 2...
        // However, most function bodies reference parameters via GenericParam(name), not Variable(id)
        // The GenericParam handler (added in BUG FIX #118) handles parameter substitution by name.
        for (i, param) in params.iter().enumerate() {
            var_id_to_name.insert(hir::VariableId(i as u32), param.name.clone());
            eprintln!(
                "[BUG #75 PARAM_MAP] Mapped parameter VariableId({}) -> '{}' (assumed sequential)",
                i, param.name
            );
        }

        // BUG FIX #123: Collect let bindings with their ACTUAL VariableIds from HIR
        collect_lets_recursive(&body, &mut var_id_to_name);
        eprintln!(
            "[BUG #123 VAR_MAP] Built var_id_to_name map with {} entries (including {} params)",
            var_id_to_name.len(),
            params.len()
        );

        // Transform early returns
        let body = self.transform_early_returns(body);

        // Check for recursion
        if self.contains_recursive_call(&body, &call.function) {
            eprintln!(
                "Error: Recursive function calls are not supported: function '{}'",
                call.function
            );
            return None;
        }

        // Check arity
        if params.len() != call.args.len() {
            eprintln!(
                "Function {}: expected {} arguments, got {}",
                call.function,
                params.len(),
                call.args.len()
            );
            return None;
        }

        // Build parameter substitution map
        let mut substitution_map = IndexMap::new();
        for (param, arg) in params.iter().zip(&call.args) {
            substitution_map.insert(param.name.clone(), arg.clone());
        }

        // Build let binding substitution map by walking through the function body
        // and collecting all let bindings with their RHS expressions
        // BUG #136 FIX: After transform_early_returns, let bindings may be nested inside if/else branches
        // We need to recursively collect let bindings from all branches
        let mut let_bindings = IndexMap::new();

        // Helper to recursively collect let statements with mutability info
        fn collect_let_stmts_recursive(
            stmts: &[hir::HirStatement],
            collected: &mut Vec<(String, hir::HirExpression, bool)>, // (name, value, is_mutable)
        ) {
            for stmt in stmts {
                match stmt {
                    hir::HirStatement::Let(let_stmt) => {
                        collected.push((
                            let_stmt.name.clone(),
                            let_stmt.value.clone(),
                            let_stmt.mutable,
                        ));
                    }
                    hir::HirStatement::If(if_stmt) => {
                        // Recursively collect from then-branch
                        collect_let_stmts_recursive(&if_stmt.then_statements, collected);
                        // Recursively collect from else-branch if present
                        if let Some(else_stmts) = &if_stmt.else_statements {
                            collect_let_stmts_recursive(else_stmts, collected);
                        }
                    }
                    hir::HirStatement::Block(stmts) => {
                        collect_let_stmts_recursive(stmts, collected);
                    }
                    hir::HirStatement::Match(match_stmt) => {
                        // BUG #137 FIX: Also collect let bindings from Match arms
                        // (transform_early_returns may create Match statements)
                        for arm in &match_stmt.arms {
                            collect_let_stmts_recursive(&arm.statements, collected);
                        }
                    }
                    _ => {}
                }
            }
        }

        // Collect all let statements from the body (including nested in if/else branches)
        let mut collected_lets = Vec::new();

        // BUG #137 DEBUG: Show body structure before collecting
        eprintln!(
            "[BUG #137 DEBUG] Body after transform_early_returns has {} statements:",
            body.len()
        );
        for (i, stmt) in body.iter().enumerate() {
            eprintln!(
                "[BUG #137 DEBUG]   body[{}]: {:?}",
                i,
                std::mem::discriminant(stmt)
            );
        }

        collect_let_stmts_recursive(&body, &mut collected_lets);

        eprintln!(
            "[BUG #137 DEBUG] collect_let_stmts_recursive found {} let bindings",
            collected_lets.len()
        );
        for (name, _value, is_mutable) in &collected_lets {
            eprintln!(
                "[BUG #137 DEBUG]   found let: '{}' mutable={}",
                name, is_mutable
            );
        }

        // BUG #137 FIX: Track mutable variable names - we should NOT substitute these in the return expression
        // because the mutable var pattern handler needs to see Variable references, not initial values
        let mut mutable_var_names: std::collections::HashSet<String> =
            std::collections::HashSet::new();

        // Now process them in order, substituting as we go
        for (name, value, is_mutable) in collected_lets {
            if is_mutable {
                debug_println!("[BUG #137 INLINE_WITH_LETS] Found MUTABLE let binding: {} - will NOT substitute in return expr", name);
                mutable_var_names.insert(name.clone());
            }
            let rhs_substituted = self.substitute_hir_expr_recursively(
                &value,
                &substitution_map,
                &let_bindings,
                &var_id_to_name,
            );
            if let Some(rhs) = rhs_substituted {
                debug_println!("[BUG #136 INLINE_WITH_LETS] Collected let binding: {} (from recursive search, mutable={})", name, is_mutable);
                // BUG #137 FIX: Only add IMMUTABLE variables to let_bindings.
                // Mutable variables should NOT be substituted - we need to preserve the Variable references
                // so that the mutable var pattern handler can see them and convert to SSA form.
                if !is_mutable {
                    let_bindings.insert(name, rhs);
                } else {
                    debug_println!("[BUG #137 FIX] NOT adding mutable var '{}' to let_bindings - preserving Variable reference", name);
                }
            }
        }

        eprintln!(
            "[BUG #136 INLINE_WITH_LETS] Collected {} let bindings (with recursive search)",
            let_bindings.len()
        );

        // Convert body to expression (extracts the return value)
        let body_expr = self.convert_body_to_expression(&body)?;

        eprintln!(
            "[BUG #74 INLINE_WITH_LETS] Body converted to expression: {:?}",
            std::mem::discriminant(&body_expr)
        );

        // Substitute both parameters and let bindings in the result expression
        let substituted_expr = self.substitute_hir_expr_recursively(
            &body_expr,
            &substitution_map,
            &let_bindings,
            &var_id_to_name,
        )?;

        eprintln!(
            "[BUG #74 INLINE_WITH_LETS] Final expression after substitution: {:?}",
            std::mem::discriminant(&substituted_expr)
        );

        // Return the fully substituted HIR expression
        Some(substituted_expr)
    }

    // Helper function to recursively substitute both parameters and let bindings
    fn substitute_hir_expr_recursively(
        &self,
        expr: &hir::HirExpression,
        params: &IndexMap<String, hir::HirExpression>,
        lets: &IndexMap<String, hir::HirExpression>,
        var_id_to_name: &IndexMap<hir::VariableId, String>,
    ) -> Option<hir::HirExpression> {
        match expr {
            // If it's a variable reference, look it up in let bindings or parameters
            hir::HirExpression::Variable(var_id) => {
                // Try to find the variable name using the var_id_to_name map
                let name = var_id_to_name.get(var_id).or_else(|| {
                    // Fallback: try dynamic_variables
                    self.dynamic_variables.get(var_id).map(|(_, n, _)| n)
                });

                eprintln!(
                    "[BUG #74 VARIABLE] Looking up Variable({:?}), found name: {:?}",
                    var_id, name
                );
                eprintln!(
                    "[BUG #74 VARIABLE] Available lets: {:?}",
                    lets.keys().collect::<Vec<_>>()
                );
                eprintln!(
                    "[BUG #74 VARIABLE] Available params: {:?}",
                    params.keys().collect::<Vec<_>>()
                );

                if let Some(name) = name {
                    // Check let bindings first
                    if let Some(let_rhs) = lets.get(name) {
                        eprintln!(
                            "[BUG #74 SUBSTITUTE] Replacing variable '{}' with let binding",
                            name
                        );
                        return Some(let_rhs.clone());
                    }
                    // Then check parameters
                    if let Some(param_rhs) = params.get(name) {
                        eprintln!(
                            "[BUG #74 SUBSTITUTE] Replacing variable '{}' with parameter",
                            name
                        );
                        return Some(param_rhs.clone());
                    }
                    debug_println!("[BUG #74 VARIABLE] Variable '{}' not found in lets or params, keeping as-is", name);
                } else {
                    debug_println!("[BUG #74 VARIABLE] Could not resolve variable name for {:?}, keeping as-is", var_id);
                }
                // If not found in either, return as-is (might be from outer scope)
                Some(expr.clone())
            }

            // BUG FIX #118: GenericParam - parameter reference by name
            // Some function bodies use GenericParam instead of Variable to reference parameters.
            // This happens during type-checking phase. We need to substitute these just like Variables.
            hir::HirExpression::GenericParam(name) => {
                eprintln!(
                    "[BUG #118 GENERIC_PARAM] Looking up GenericParam '{}'",
                    name
                );
                eprintln!(
                    "[BUG #118 GENERIC_PARAM] Available lets: {:?}",
                    lets.keys().collect::<Vec<_>>()
                );
                eprintln!(
                    "[BUG #118 GENERIC_PARAM] Available params: {:?}",
                    params.keys().collect::<Vec<_>>()
                );

                // Check let bindings first
                if let Some(let_rhs) = lets.get(name) {
                    eprintln!(
                        "[BUG #118 SUBSTITUTE] Replacing GenericParam '{}' with let binding",
                        name
                    );
                    return Some(let_rhs.clone());
                }
                // Then check parameters
                if let Some(param_rhs) = params.get(name) {
                    eprintln!(
                        "[BUG #118 SUBSTITUTE] Replacing GenericParam '{}' with parameter",
                        name
                    );
                    return Some(param_rhs.clone());
                }
                debug_println!("[BUG #118 GENERIC_PARAM] GenericParam '{}' not found in lets or params, keeping as-is", name);
                // If not found, return as-is (might be from outer scope)
                Some(expr.clone())
            }

            // For complex expressions, recursively substitute in children
            hir::HirExpression::TupleLiteral(elements) => {
                let substituted_elements: Option<Vec<_>> = elements
                    .iter()
                    .map(|elem| {
                        self.substitute_hir_expr_recursively(elem, params, lets, var_id_to_name)
                    })
                    .collect();
                Some(hir::HirExpression::TupleLiteral(substituted_elements?))
            }

            hir::HirExpression::Call(call_expr) => {
                let substituted_args: Option<Vec<_>> = call_expr
                    .args
                    .iter()
                    .map(|arg| {
                        self.substitute_hir_expr_recursively(arg, params, lets, var_id_to_name)
                    })
                    .collect();
                Some(hir::HirExpression::Call(hir::HirCallExpr {
                    function: call_expr.function.clone(),
                    args: substituted_args?,
                    type_args: call_expr.type_args.clone(),
                    named_type_args: call_expr.named_type_args.clone(),
                    impl_style: call_expr.impl_style,
                }))
            }

            hir::HirExpression::Binary(bin_expr) => {
                let left_sub = self.substitute_hir_expr_recursively(
                    &bin_expr.left,
                    params,
                    lets,
                    var_id_to_name,
                )?;
                let right_sub = self.substitute_hir_expr_recursively(
                    &bin_expr.right,
                    params,
                    lets,
                    var_id_to_name,
                )?;
                Some(hir::HirExpression::Binary(hir::HirBinaryExpr {
                    op: bin_expr.op.clone(),
                    left: Box::new(left_sub),
                    right: Box::new(right_sub),
                    is_trait_op: bin_expr.is_trait_op,
                }))
            }

            hir::HirExpression::If(if_expr) => {
                let cond_sub = self.substitute_hir_expr_recursively(
                    &if_expr.condition,
                    params,
                    lets,
                    var_id_to_name,
                )?;
                let then_sub = self.substitute_hir_expr_recursively(
                    &if_expr.then_expr,
                    params,
                    lets,
                    var_id_to_name,
                )?;
                let else_sub = self.substitute_hir_expr_recursively(
                    &if_expr.else_expr,
                    params,
                    lets,
                    var_id_to_name,
                )?;
                Some(hir::HirExpression::If(hir::HirIfExpr {
                    condition: Box::new(cond_sub),
                    then_expr: Box::new(then_sub),
                    else_expr: Box::new(else_sub),
                }))
            }

            hir::HirExpression::Block {
                statements,
                result_expr,
            } => {
                // For Block expressions, we need to handle nested let bindings
                // Collect let bindings from the block
                eprintln!(
                    "[BUG #74 BLOCK] Processing Block with {} statements",
                    statements.len()
                );

                // BUG FIX #133: Check if this Block contains If statements or Assignment statements.
                // If so, we must PRESERVE the Block structure so it can be properly transformed
                // by try_transform_block_mutable_vars in convert_hir_expr_for_module.
                // Without this, Blocks like clz32's body:
                //   { let mut count = 0; let mut temp = value; if (cond) { count = count + 16; } count }
                // would be flattened to just "0" (the initial value of count), losing the if statements.
                // BUG #137 FIX: Also preserve for Assignment statements (like popcount32 which uses
                // direct mutable variable re-assignments without if statements).
                let has_if_or_assignments = statements.iter().any(|stmt| {
                    matches!(
                        stmt,
                        hir::HirStatement::If(_) | hir::HirStatement::Assignment(_)
                    )
                });

                if has_if_or_assignments {
                    debug_println!("[BUG #137 FIX] Block has if/assignment statements - preserving Block structure");

                    // Substitute in statements (let bindings and if statements)
                    let mut substituted_stmts = Vec::new();
                    let mut local_var_id_to_name = var_id_to_name.clone();
                    let mut nested_lets = lets.clone();

                    for stmt in statements {
                        match stmt {
                            hir::HirStatement::Let(let_stmt) => {
                                local_var_id_to_name.insert(let_stmt.id, let_stmt.name.clone());
                                let rhs_sub = self.substitute_hir_expr_recursively(
                                    &let_stmt.value,
                                    params,
                                    &nested_lets,
                                    &local_var_id_to_name,
                                )?;
                                // BUG #137 FIX: Only add IMMUTABLE let bindings to nested_lets.
                                // For mutable variables (like `count` in clz32), we must NOT add them
                                // because we need the result_expr to stay as Variable(count) so the
                                // mutable variable transformation can work correctly.
                                if !let_stmt.mutable {
                                    nested_lets.insert(let_stmt.name.clone(), rhs_sub.clone());
                                } else {
                                    debug_println!("[BUG #137 BLOCK FIX] NOT adding mutable var '{}' to nested_lets - preserving Variable reference in result_expr", let_stmt.name);
                                }
                                substituted_stmts.push(hir::HirStatement::Let(
                                    hir::HirLetStatement {
                                        name: let_stmt.name.clone(),
                                        id: let_stmt.id,
                                        var_type: let_stmt.var_type.clone(),
                                        mutable: let_stmt.mutable,
                                        value: rhs_sub,
                                    },
                                ));
                            }
                            hir::HirStatement::If(if_stmt) => {
                                // Substitute in all parts of the if statement
                                let cond_sub = self.substitute_hir_expr_recursively(
                                    &if_stmt.condition,
                                    params,
                                    &nested_lets,
                                    &local_var_id_to_name,
                                )?;
                                let then_stmts_sub = self.substitute_statements_recursively(
                                    &if_stmt.then_statements,
                                    params,
                                    &nested_lets,
                                    &local_var_id_to_name,
                                )?;
                                let else_stmts_sub =
                                    if let Some(else_stmts) = &if_stmt.else_statements {
                                        Some(self.substitute_statements_recursively(
                                            else_stmts,
                                            params,
                                            &nested_lets,
                                            &local_var_id_to_name,
                                        )?)
                                    } else {
                                        None
                                    };
                                substituted_stmts.push(hir::HirStatement::If(
                                    hir::HirIfStatement {
                                        condition: cond_sub,
                                        then_statements: then_stmts_sub,
                                        else_statements: else_stmts_sub,
                                        mux_style: if_stmt.mux_style,
                                    },
                                ));
                            }
                            hir::HirStatement::Assignment(assign_stmt) => {
                                let rhs_sub = self.substitute_hir_expr_recursively(
                                    &assign_stmt.rhs,
                                    params,
                                    &nested_lets,
                                    &local_var_id_to_name,
                                )?;
                                substituted_stmts.push(hir::HirStatement::Assignment(
                                    hir::HirAssignment {
                                        id: assign_stmt.id,
                                        lhs: assign_stmt.lhs.clone(),
                                        assignment_type: assign_stmt.assignment_type.clone(),
                                        rhs: rhs_sub,
                                    },
                                ));
                            }
                            other => {
                                // Keep other statement types as-is for now
                                substituted_stmts.push(other.clone());
                            }
                        }
                    }

                    // Substitute in result expression
                    let result_sub = self.substitute_hir_expr_recursively(
                        result_expr,
                        params,
                        &nested_lets,
                        &local_var_id_to_name,
                    )?;

                    eprintln!(
                        "[BUG #137 FIX] Preserved Block with {} statements, result_expr: {:?}",
                        substituted_stmts.len(),
                        std::mem::discriminant(&result_sub)
                    );

                    return Some(hir::HirExpression::Block {
                        statements: substituted_stmts,
                        result_expr: Box::new(result_sub),
                    });
                }

                // Original path for Blocks without if statements: flatten to just result expression
                let mut nested_lets = lets.clone();
                // BUG FIX #132: Clone var_id_to_name so we can extend it with local let bindings.
                // Without this, Variables referencing let bindings inside the Block (like `count` in clz32)
                // cannot be looked up and are returned as-is, causing match arms to return wrong values.
                let mut local_var_id_to_name = var_id_to_name.clone();
                for stmt in statements {
                    if let hir::HirStatement::Let(let_stmt) = stmt {
                        eprintln!(
                            "[BUG #74 BLOCK] Found nested let binding: {} (id={})",
                            let_stmt.name, let_stmt.id.0
                        );
                        // BUG FIX #132: Add this let binding's VariableId to name mapping
                        local_var_id_to_name.insert(let_stmt.id, let_stmt.name.clone());
                        // Substitute in the RHS of this let binding
                        let rhs_sub = self.substitute_hir_expr_recursively(
                            &let_stmt.value,
                            params,
                            &nested_lets,
                            &local_var_id_to_name,
                        )?;
                        nested_lets.insert(let_stmt.name.clone(), rhs_sub);
                        eprintln!(
                            "[BUG #74 BLOCK] Collected nested let: {} (total: {})",
                            let_stmt.name,
                            nested_lets.len()
                        );
                    }
                }
                eprintln!(
                    "[BUG #74 BLOCK] Total nested lets: {}, result_expr type: {:?}",
                    nested_lets.len(),
                    std::mem::discriminant(&**result_expr)
                );
                // Substitute in the result expression with all nested lets AND the extended var_id_to_name
                let result_sub = self.substitute_hir_expr_recursively(
                    result_expr,
                    params,
                    &nested_lets,
                    &local_var_id_to_name,
                )?;
                eprintln!(
                    "[BUG #74 BLOCK] Result after substitution: {:?}",
                    std::mem::discriminant(&result_sub)
                );
                // Return the result directly (no need to keep the Block wrapper since lets are substituted)
                Some(result_sub)
            }

            // BUG FIX #118: Cast - substitute in the inner expression
            // Critical for FP operations like `a as fp32` where `a` is a Variable
            hir::HirExpression::Cast(cast_expr) => {
                let inner_sub = self.substitute_hir_expr_recursively(
                    &cast_expr.expr,
                    params,
                    lets,
                    var_id_to_name,
                )?;
                Some(hir::HirExpression::Cast(hir::HirCastExpr {
                    expr: Box::new(inner_sub),
                    target_type: cast_expr.target_type.clone(),
                }))
            }

            // BUG FIX #118: Unary - substitute in the operand
            hir::HirExpression::Unary(unary_expr) => {
                let operand_sub = self.substitute_hir_expr_recursively(
                    &unary_expr.operand,
                    params,
                    lets,
                    var_id_to_name,
                )?;
                Some(hir::HirExpression::Unary(hir::HirUnaryExpr {
                    op: unary_expr.op.clone(),
                    operand: Box::new(operand_sub),
                }))
            }

            // BUG FIX #118: Range (slice) - substitute in all three expressions
            hir::HirExpression::Range(base, high, low) => {
                let base_sub =
                    self.substitute_hir_expr_recursively(base, params, lets, var_id_to_name)?;
                let high_sub =
                    self.substitute_hir_expr_recursively(high, params, lets, var_id_to_name)?;
                let low_sub =
                    self.substitute_hir_expr_recursively(low, params, lets, var_id_to_name)?;
                Some(hir::HirExpression::Range(
                    Box::new(base_sub),
                    Box::new(high_sub),
                    Box::new(low_sub),
                ))
            }

            // BUG FIX #118: Concat - substitute in all elements
            hir::HirExpression::Concat(elements) => {
                let elements_sub: Option<Vec<_>> = elements
                    .iter()
                    .map(|elem| {
                        self.substitute_hir_expr_recursively(elem, params, lets, var_id_to_name)
                    })
                    .collect();
                Some(hir::HirExpression::Concat(elements_sub?))
            }

            // BUG FIX #118: Index - substitute in base and index expressions
            hir::HirExpression::Index(base, index) => {
                let base_sub =
                    self.substitute_hir_expr_recursively(base, params, lets, var_id_to_name)?;
                let index_sub =
                    self.substitute_hir_expr_recursively(index, params, lets, var_id_to_name)?;
                Some(hir::HirExpression::Index(
                    Box::new(base_sub),
                    Box::new(index_sub),
                ))
            }

            // BUG FIX #118: Ternary - substitute in all three parts
            hir::HirExpression::Ternary {
                condition,
                true_expr,
                false_expr,
            } => {
                let cond_sub =
                    self.substitute_hir_expr_recursively(condition, params, lets, var_id_to_name)?;
                let true_sub =
                    self.substitute_hir_expr_recursively(true_expr, params, lets, var_id_to_name)?;
                let false_sub =
                    self.substitute_hir_expr_recursively(false_expr, params, lets, var_id_to_name)?;
                Some(hir::HirExpression::Ternary {
                    condition: Box::new(cond_sub),
                    true_expr: Box::new(true_sub),
                    false_expr: Box::new(false_sub),
                })
            }

            // BUG FIX #118: Match - substitute in expr and all arm expressions
            hir::HirExpression::Match(match_expr) => {
                let expr_sub = self.substitute_hir_expr_recursively(
                    &match_expr.expr,
                    params,
                    lets,
                    var_id_to_name,
                )?;
                let arms_sub: Option<Vec<_>> = match_expr
                    .arms
                    .iter()
                    .map(|arm| {
                        let arm_expr_sub = self.substitute_hir_expr_recursively(
                            &arm.expr,
                            params,
                            lets,
                            var_id_to_name,
                        )?;
                        // Also substitute in guard if present
                        let guard_sub = if let Some(guard) = &arm.guard {
                            Some(self.substitute_hir_expr_recursively(
                                guard,
                                params,
                                lets,
                                var_id_to_name,
                            )?)
                        } else {
                            None
                        };
                        Some(hir::HirMatchArmExpr {
                            pattern: arm.pattern.clone(),
                            guard: guard_sub,
                            expr: arm_expr_sub,
                        })
                    })
                    .collect();
                Some(hir::HirExpression::Match(hir::HirMatchExpr {
                    expr: Box::new(expr_sub),
                    arms: arms_sub?,
                    mux_style: match_expr.mux_style,
                }))
            }

            // For literals and other leaf nodes, return as-is
            _ => Some(expr.clone()),
        }
    }

    /// BUG FIX #133: Helper function to recursively substitute in a list of HirStatements.
    /// Used when preserving Block structure in substitute_hir_expr_recursively for blocks
    /// containing If statements with mutable variable assignments (like clz32, ctz32, popcount32).
    fn substitute_statements_recursively(
        &self,
        statements: &[hir::HirStatement],
        params: &IndexMap<String, hir::HirExpression>,
        lets: &IndexMap<String, hir::HirExpression>,
        var_id_to_name: &IndexMap<hir::VariableId, String>,
    ) -> Option<Vec<hir::HirStatement>> {
        let mut result = Vec::new();
        let mut local_lets = lets.clone();
        let mut local_var_id_to_name = var_id_to_name.clone();

        for stmt in statements {
            match stmt {
                hir::HirStatement::Let(let_stmt) => {
                    local_var_id_to_name.insert(let_stmt.id, let_stmt.name.clone());
                    let rhs_sub = self.substitute_hir_expr_recursively(
                        &let_stmt.value,
                        params,
                        &local_lets,
                        &local_var_id_to_name,
                    )?;
                    local_lets.insert(let_stmt.name.clone(), rhs_sub.clone());
                    result.push(hir::HirStatement::Let(hir::HirLetStatement {
                        name: let_stmt.name.clone(),
                        id: let_stmt.id,
                        var_type: let_stmt.var_type.clone(),
                        mutable: let_stmt.mutable,
                        value: rhs_sub,
                    }));
                }
                hir::HirStatement::If(if_stmt) => {
                    let cond_sub = self.substitute_hir_expr_recursively(
                        &if_stmt.condition,
                        params,
                        &local_lets,
                        &local_var_id_to_name,
                    )?;
                    let then_stmts_sub = self.substitute_statements_recursively(
                        &if_stmt.then_statements,
                        params,
                        &local_lets,
                        &local_var_id_to_name,
                    )?;
                    let else_stmts_sub = if let Some(else_stmts) = &if_stmt.else_statements {
                        Some(self.substitute_statements_recursively(
                            else_stmts,
                            params,
                            &local_lets,
                            &local_var_id_to_name,
                        )?)
                    } else {
                        None
                    };
                    result.push(hir::HirStatement::If(hir::HirIfStatement {
                        condition: cond_sub,
                        then_statements: then_stmts_sub,
                        else_statements: else_stmts_sub,
                        mux_style: if_stmt.mux_style,
                    }));
                }
                hir::HirStatement::Assignment(assign_stmt) => {
                    let rhs_sub = self.substitute_hir_expr_recursively(
                        &assign_stmt.rhs,
                        params,
                        &local_lets,
                        &local_var_id_to_name,
                    )?;
                    result.push(hir::HirStatement::Assignment(hir::HirAssignment {
                        id: assign_stmt.id,
                        lhs: assign_stmt.lhs.clone(),
                        assignment_type: assign_stmt.assignment_type.clone(),
                        rhs: rhs_sub,
                    }));
                }
                other => {
                    result.push(other.clone());
                }
            }
        }

        Some(result)
    }

    /// BUG FIX #91: Simple name-based expression substitution for module synthesis
    /// Substitutes variable references (by name from GenericParam) with expressions from the map
    fn substitute_hir_expr_with_map(
        &self,
        expr: &hir::HirExpression,
        name_map: &IndexMap<String, hir::HirExpression>,
    ) -> hir::HirExpression {
        match expr {
            // GenericParam - used for function parameters and some variable references
            hir::HirExpression::GenericParam(name) => {
                if let Some(replacement) = name_map.get(name) {
                    println!(
                        "[BUG #91 SUBST] Replacing GenericParam '{}' with {:?}",
                        name,
                        std::mem::discriminant(replacement)
                    );
                    replacement.clone()
                } else {
                    expr.clone()
                }
            }

            // Variable - look up by name if we can find it
            // BUG FIX #91: First check if this variable's name exists in name_map directly
            // This handles the case where Block's let bindings created Variables that
            // we need to substitute (e.g., rx/ry/rz in vec3_add_op)
            hir::HirExpression::Variable(var_id) => {
                // First, try to find the variable name in dynamic_variables and check name_map
                if let Some((_, name, _)) = self.dynamic_variables.get(var_id) {
                    if let Some(replacement) = name_map.get(name) {
                        println!(
                            "[BUG #91 SUBST] Replacing Variable '{}' (id={}) with {:?}",
                            name,
                            var_id.0,
                            std::mem::discriminant(replacement)
                        );
                        return replacement.clone();
                    }
                }

                // Fallback: For Block's local Variables that might not be in dynamic_variables,
                // we can't resolve the name. Return as-is and let convert_hir_expr_for_module handle it.
                expr.clone()
            }

            // TupleLiteral - substitute in each element
            hir::HirExpression::TupleLiteral(elements) => {
                let substituted: Vec<_> = elements
                    .iter()
                    .map(|elem| self.substitute_hir_expr_with_map(elem, name_map))
                    .collect();
                hir::HirExpression::TupleLiteral(substituted)
            }

            // Call - substitute in arguments
            hir::HirExpression::Call(call_expr) => {
                let substituted_args: Vec<_> = call_expr
                    .args
                    .iter()
                    .map(|arg| self.substitute_hir_expr_with_map(arg, name_map))
                    .collect();
                hir::HirExpression::Call(hir::HirCallExpr {
                    function: call_expr.function.clone(),
                    args: substituted_args,
                    type_args: call_expr.type_args.clone(),
                    named_type_args: call_expr.named_type_args.clone(),
                    impl_style: call_expr.impl_style,
                })
            }

            // Binary - substitute in both operands
            hir::HirExpression::Binary(bin_expr) => {
                let left_sub = self.substitute_hir_expr_with_map(&bin_expr.left, name_map);
                let right_sub = self.substitute_hir_expr_with_map(&bin_expr.right, name_map);
                hir::HirExpression::Binary(hir::HirBinaryExpr {
                    op: bin_expr.op.clone(),
                    left: Box::new(left_sub),
                    right: Box::new(right_sub),
                    is_trait_op: bin_expr.is_trait_op,
                })
            }

            // If - substitute in all branches
            hir::HirExpression::If(if_expr) => {
                let cond_sub = self.substitute_hir_expr_with_map(&if_expr.condition, name_map);
                let then_sub = self.substitute_hir_expr_with_map(&if_expr.then_expr, name_map);
                let else_sub = self.substitute_hir_expr_with_map(&if_expr.else_expr, name_map);
                hir::HirExpression::If(hir::HirIfExpr {
                    condition: Box::new(cond_sub),
                    then_expr: Box::new(then_sub),
                    else_expr: Box::new(else_sub),
                })
            }

            // Block - process nested let bindings
            // BUG FIX #91: When processing Block's result_expr, we need to replace Variables
            // that reference the let bindings. Since Variables use VariableId (not name),
            // we need to substitute them here directly rather than relying on the Variable handler.
            // BUG FIX #86: Preserve If statements! Previous code dropped all non-Let statements.
            // BUG FIX #103: Collect LOCAL VariableIds first to avoid incorrect substitution when
            // Variables from outer scope collide with local VariableIds.
            hir::HirExpression::Block {
                statements,
                result_expr,
            } => {
                let mut nested_map = name_map.clone();

                // BUG FIX #103: Collect VariableIds from the parameter substitution arguments
                // These are Variables from the OUTER scope that should NOT be substituted
                let mut param_var_ids: std::collections::HashSet<hir::VariableId> =
                    std::collections::HashSet::new();
                for arg_expr in name_map.values() {
                    Self::collect_variable_ids_from_expr(arg_expr, &mut param_var_ids);
                }
                if !param_var_ids.is_empty() {
                    debug_println!("[BUG #103] Parameter arguments contain {} VariableIds from outer scope: {:?}",
                             param_var_ids.len(),
                             param_var_ids.iter().map(|v| v.0).collect::<Vec<_>>());
                }

                // BUG FIX #103: Collect LOCAL VariableIds from let statements
                // But EXCLUDE any that collide with parameter argument VariableIds
                let local_var_ids: std::collections::HashSet<hir::VariableId> = statements.iter()
                    .filter_map(|stmt| {
                        if let hir::HirStatement::Let(let_stmt) = stmt {
                            // Exclude VariableIds that collide with parameter arguments
                            if param_var_ids.contains(&let_stmt.id) {
                                debug_println!("[BUG #103] Excluding local Variable({}) from substitution - collides with param arg",
                                         let_stmt.id.0);
                                None
                            } else {
                                Some(let_stmt.id)
                            }
                        } else {
                            None
                        }
                    })
                    .collect();
                println!(
                    "[BUG #103] Block has {} substitutable local VariableIds: {:?}",
                    local_var_ids.len(),
                    local_var_ids.iter().map(|v| v.0).collect::<Vec<_>>()
                );

                // Build a var_id-to-substituted_value map for let bindings in this Block
                let mut var_id_to_value: IndexMap<hir::VariableId, hir::HirExpression> =
                    IndexMap::new();

                // Check if there are any non-Let statements (If, Assign, etc.)
                let has_non_let_stmts = statements
                    .iter()
                    .any(|stmt| !matches!(stmt, hir::HirStatement::Let(_)));

                // Collect substituted statements
                let mut substituted_stmts: Vec<hir::HirStatement> = Vec::new();

                for stmt in statements {
                    match stmt {
                        hir::HirStatement::Let(let_stmt) => {
                            // Substitute in the let binding's value
                            let mut sub_value =
                                self.substitute_hir_expr_with_map(&let_stmt.value, &nested_map);

                            // BUG FIX #103: Use filtered substitution to ONLY replace LOCAL Variables
                            // This prevents outer scope Variable(6) from being replaced with local Variable(6)'s value
                            sub_value = self.substitute_var_ids_in_expr_filtered(
                                &sub_value,
                                &var_id_to_value,
                                &local_var_ids,
                            );

                            // BUG FIX #86: Only add IMMUTABLE variables to substitution maps.
                            // Mutable variables should NOT be substituted because their values
                            // change over time. They need to be preserved as Variable references
                            // so that try_transform_block_mutable_vars can properly track their
                            // SSA-style updates.
                            if !let_stmt.mutable {
                                // Store for name-based lookup
                                nested_map.insert(let_stmt.name.clone(), sub_value.clone());
                                // Store for VariableId-based lookup
                                var_id_to_value.insert(let_stmt.id, sub_value.clone());
                            }

                            // Add to substituted statements
                            substituted_stmts.push(hir::HirStatement::Let(hir::HirLetStatement {
                                id: let_stmt.id,
                                name: let_stmt.name.clone(),
                                mutable: let_stmt.mutable,
                                var_type: let_stmt.var_type.clone(),
                                value: sub_value,
                            }));
                        }
                        hir::HirStatement::If(if_stmt) => {
                            // Substitute in If statement condition and body
                            let sub_cond =
                                self.substitute_hir_expr_with_map(&if_stmt.condition, &nested_map);
                            // BUG FIX #103: Use filtered version to avoid outer scope collision
                            let sub_cond = self.substitute_var_ids_in_expr_filtered(
                                &sub_cond,
                                &var_id_to_value,
                                &local_var_ids,
                            );

                            let sub_then: Vec<_> = if_stmt
                                .then_statements
                                .iter()
                                .map(|s| {
                                    self.substitute_hir_stmt_with_maps(
                                        s,
                                        &nested_map,
                                        &var_id_to_value,
                                    )
                                })
                                .collect();

                            let sub_else: Option<Vec<_>> =
                                if_stmt.else_statements.as_ref().map(|stmts| {
                                    stmts
                                        .iter()
                                        .map(|s| {
                                            self.substitute_hir_stmt_with_maps(
                                                s,
                                                &nested_map,
                                                &var_id_to_value,
                                            )
                                        })
                                        .collect()
                                });

                            substituted_stmts.push(hir::HirStatement::If(hir::HirIfStatement {
                                condition: sub_cond,
                                then_statements: sub_then,
                                else_statements: sub_else,
                                mux_style: if_stmt.mux_style,
                            }));
                        }
                        hir::HirStatement::Assignment(assign_stmt) => {
                            // Substitute in Assignment statement
                            let sub_rhs =
                                self.substitute_hir_expr_with_map(&assign_stmt.rhs, &nested_map);
                            // BUG FIX #103: Use filtered version to avoid outer scope collision
                            let sub_rhs = self.substitute_var_ids_in_expr_filtered(
                                &sub_rhs,
                                &var_id_to_value,
                                &local_var_ids,
                            );

                            substituted_stmts.push(hir::HirStatement::Assignment(
                                hir::HirAssignment {
                                    id: assign_stmt.id,
                                    lhs: assign_stmt.lhs.clone(),
                                    assignment_type: assign_stmt.assignment_type.clone(),
                                    rhs: sub_rhs,
                                },
                            ));
                        }
                        _ => {
                            // Other statement types - keep as-is for now
                            substituted_stmts.push(stmt.clone());
                        }
                    }
                }

                // Substitute in result_expr - first by name (for GenericParams etc)
                let partially_substituted =
                    self.substitute_hir_expr_with_map(result_expr, &nested_map);
                // BUG FIX #105: Use non-filtered version for result_expr substitution
                // The filtered version (BUG #103 fix) excludes Variables with IDs that collide
                // with param_var_ids, but those still need substitution in the result_expr.
                // This is safe because var_id_to_value only contains THIS Block's local let
                // bindings - outer-scope Variables won't be in the map and won't be touched.
                let sub_result =
                    self.substitute_var_ids_in_expr(&partially_substituted, &var_id_to_value);

                // If there are non-Let statements, preserve the Block structure
                // Otherwise, just return the substituted result (optimization for pure let blocks)
                if has_non_let_stmts {
                    println!(
                        "[BUG #86] Block substitution: preserving {} statements (has non-Let)",
                        substituted_stmts.len()
                    );
                    hir::HirExpression::Block {
                        statements: substituted_stmts,
                        result_expr: Box::new(sub_result),
                    }
                } else {
                    sub_result
                }
            }

            // Cast - substitute in the inner expression
            hir::HirExpression::Cast(cast_expr) => {
                let sub_expr = self.substitute_hir_expr_with_map(&cast_expr.expr, name_map);
                hir::HirExpression::Cast(hir::HirCastExpr {
                    expr: Box::new(sub_expr),
                    target_type: cast_expr.target_type.clone(),
                })
            }

            // Concat - substitute in all parts
            hir::HirExpression::Concat(parts) => {
                let substituted: Vec<_> = parts
                    .iter()
                    .map(|part| self.substitute_hir_expr_with_map(part, name_map))
                    .collect();
                hir::HirExpression::Concat(substituted)
            }

            // BUG FIX #92: FieldAccess - substitute in base expression
            // This is critical for tuple destructuring like `let valid = _tuple_tmp.0`
            hir::HirExpression::FieldAccess { base, field } => {
                let sub_base = self.substitute_hir_expr_with_map(base, name_map);
                hir::HirExpression::FieldAccess {
                    base: Box::new(sub_base),
                    field: field.clone(),
                }
            }

            // BUG FIX #98: Match - substitute in match value and arm expressions
            // This is critical for inlining functions that contain match expressions (like clz32)
            // When clz32 is inlined with value=data1[31:0], the match needs to substitute
            // GenericParam("value") with the actual argument expression
            hir::HirExpression::Match(match_expr) => {
                // Substitute in the match value expression
                let sub_expr = self.substitute_hir_expr_with_map(&match_expr.expr, name_map);

                // Substitute in each arm body expression
                let sub_arms: Vec<_> = match_expr
                    .arms
                    .iter()
                    .map(|arm| {
                        let sub_arm_expr = self.substitute_hir_expr_with_map(&arm.expr, name_map);
                        // Also substitute in guard if present
                        let sub_guard = arm
                            .guard
                            .as_ref()
                            .map(|g| self.substitute_hir_expr_with_map(g, name_map));
                        hir::HirMatchArmExpr {
                            pattern: arm.pattern.clone(),
                            guard: sub_guard,
                            expr: sub_arm_expr,
                        }
                    })
                    .collect();

                hir::HirExpression::Match(hir::HirMatchExpr {
                    expr: Box::new(sub_expr),
                    arms: sub_arms,
                    mux_style: match_expr.mux_style,
                })
            }

            // BUG FIX #99: Unary - substitute in the operand
            // This is critical for expressions like -a_fp where a_fp needs substitution
            hir::HirExpression::Unary(unary_expr) => {
                let sub_operand = self.substitute_hir_expr_with_map(&unary_expr.operand, name_map);
                hir::HirExpression::Unary(hir::HirUnaryExpr {
                    op: unary_expr.op.clone(),
                    operand: Box::new(sub_operand),
                })
            }

            // BUG FIX #99: Index (bit slice) - substitute in base and index expressions
            hir::HirExpression::Index(base, index) => {
                let sub_base = self.substitute_hir_expr_with_map(base, name_map);
                let sub_index = self.substitute_hir_expr_with_map(index, name_map);
                hir::HirExpression::Index(Box::new(sub_base), Box::new(sub_index))
            }

            // BUG FIX #146: Range (bit slice) - substitute in base, high, and low expressions
            // Without this, Range(GenericParam("data1"), high, low) was not being substituted,
            // causing function parameters to remain as GenericParam and then fall back to 0.
            hir::HirExpression::Range(base, high, low) => {
                let sub_base = self.substitute_hir_expr_with_map(base, name_map);
                let sub_high = self.substitute_hir_expr_with_map(high, name_map);
                let sub_low = self.substitute_hir_expr_with_map(low, name_map);
                hir::HirExpression::Range(Box::new(sub_base), Box::new(sub_high), Box::new(sub_low))
            }

            // Everything else - return as-is
            _ => expr.clone(),
        }
    }

    /// BUG FIX #91: Substitute Variables by VariableId with expressions from the map
    /// This handles the case where Block's result_expr contains Variables referencing
    /// let bindings that were defined in the same Block.
    #[allow(clippy::only_used_in_recursion)]
    fn substitute_var_ids_in_expr(
        &self,
        expr: &hir::HirExpression,
        var_id_map: &IndexMap<hir::VariableId, hir::HirExpression>,
    ) -> hir::HirExpression {
        match expr {
            hir::HirExpression::Variable(var_id) => {
                if let Some(replacement) = var_id_map.get(var_id) {
                    // BUG FIX #107: Return the replacement, but DON'T recursively substitute
                    // here to avoid stack overflow. Instead, we substitute iteratively at
                    // the call sites using substitute_var_ids_until_fixed_point().
                    replacement.clone()
                } else {
                    expr.clone()
                }
            }

            hir::HirExpression::TupleLiteral(elements) => {
                let substituted: Vec<_> = elements
                    .iter()
                    .map(|elem| self.substitute_var_ids_in_expr(elem, var_id_map))
                    .collect();
                hir::HirExpression::TupleLiteral(substituted)
            }

            hir::HirExpression::Call(call_expr) => {
                let substituted_args: Vec<_> = call_expr
                    .args
                    .iter()
                    .map(|arg| self.substitute_var_ids_in_expr(arg, var_id_map))
                    .collect();
                hir::HirExpression::Call(hir::HirCallExpr {
                    function: call_expr.function.clone(),
                    args: substituted_args,
                    type_args: call_expr.type_args.clone(),
                    named_type_args: call_expr.named_type_args.clone(),
                    impl_style: call_expr.impl_style,
                })
            }

            hir::HirExpression::Binary(bin_expr) => {
                let left_sub = self.substitute_var_ids_in_expr(&bin_expr.left, var_id_map);
                let right_sub = self.substitute_var_ids_in_expr(&bin_expr.right, var_id_map);
                hir::HirExpression::Binary(hir::HirBinaryExpr {
                    op: bin_expr.op.clone(),
                    left: Box::new(left_sub),
                    right: Box::new(right_sub),
                    is_trait_op: bin_expr.is_trait_op,
                })
            }

            hir::HirExpression::If(if_expr) => {
                let cond_sub = self.substitute_var_ids_in_expr(&if_expr.condition, var_id_map);
                let then_sub = self.substitute_var_ids_in_expr(&if_expr.then_expr, var_id_map);
                let else_sub = self.substitute_var_ids_in_expr(&if_expr.else_expr, var_id_map);
                hir::HirExpression::If(hir::HirIfExpr {
                    condition: Box::new(cond_sub),
                    then_expr: Box::new(then_sub),
                    else_expr: Box::new(else_sub),
                })
            }

            hir::HirExpression::Concat(parts) => {
                let substituted: Vec<_> = parts
                    .iter()
                    .map(|part| self.substitute_var_ids_in_expr(part, var_id_map))
                    .collect();
                hir::HirExpression::Concat(substituted)
            }

            hir::HirExpression::Cast(cast_expr) => {
                let sub_expr = self.substitute_var_ids_in_expr(&cast_expr.expr, var_id_map);
                hir::HirExpression::Cast(hir::HirCastExpr {
                    expr: Box::new(sub_expr),
                    target_type: cast_expr.target_type.clone(),
                })
            }

            // BUG FIX #92: FieldAccess - substitute in base expression
            // This is critical for tuple destructuring like `let valid = _tuple_tmp.0`
            hir::HirExpression::FieldAccess { base, field } => {
                let sub_base = self.substitute_var_ids_in_expr(base, var_id_map);
                hir::HirExpression::FieldAccess {
                    base: Box::new(sub_base),
                    field: field.clone(),
                }
            }

            // BUG FIX #101: Unary - substitute in the operand
            // This is CRITICAL for expressions like -a_fp where a_fp is Variable(14)
            // Without this, Unary(Negate, Variable(14)) is returned as-is and the
            // Variable(14) inside is never resolved, causing fp_neg to return 0!
            hir::HirExpression::Unary(unary_expr) => {
                let sub_operand = self.substitute_var_ids_in_expr(&unary_expr.operand, var_id_map);
                println!(
                    "[BUG #101 VAR_ID] Substituting Unary operand: {:?} -> {:?}",
                    std::mem::discriminant(&*unary_expr.operand),
                    std::mem::discriminant(&sub_operand)
                );
                hir::HirExpression::Unary(hir::HirUnaryExpr {
                    op: unary_expr.op.clone(),
                    operand: Box::new(sub_operand),
                })
            }

            // BUG FIX #101: Index - substitute in base and index expressions
            // This handles expressions like value[31:0] where value is a Variable
            hir::HirExpression::Index(base, index) => {
                let sub_base = self.substitute_var_ids_in_expr(base, var_id_map);
                let sub_index = self.substitute_var_ids_in_expr(index, var_id_map);
                hir::HirExpression::Index(Box::new(sub_base), Box::new(sub_index))
            }

            // BUG FIX #146: Range - substitute in base, high, and low expressions
            hir::HirExpression::Range(base, high, low) => {
                let sub_base = self.substitute_var_ids_in_expr(base, var_id_map);
                let sub_high = self.substitute_var_ids_in_expr(high, var_id_map);
                let sub_low = self.substitute_var_ids_in_expr(low, var_id_map);
                hir::HirExpression::Range(Box::new(sub_base), Box::new(sub_high), Box::new(sub_low))
            }

            // BUG FIX #106 + #120: Block - recursively process statements and result_expr
            // This is CRITICAL for nested inlined functions! When fp_div is inlined
            // and its body contains Call(fp_sub, ...), the fp_sub gets inlined too,
            // producing a nested Block. Without this, Variables inside nested Blocks
            // won't be substituted, causing sqrt_disc to be missing from x1/x2 calculation.
            //
            // BUG FIX #120: We must also substitute the Block's OWN let bindings in the
            // result_expr! Previously we only substituted outer variables, but the Block's
            // local variables (e.g., a_fp, b_fp, result in fp_add) also need to be replaced
            // with their values before we can convert to SIR.
            hir::HirExpression::Block {
                statements,
                result_expr,
            } => {
                // Build a local var_id_map from this Block's let statements
                // Start with outer scope variables, then add local bindings (locals override)
                let mut local_var_id_map: IndexMap<hir::VariableId, hir::HirExpression> =
                    var_id_map.clone();

                let mut substituted_stmts = Vec::new();
                for stmt in statements {
                    match stmt {
                        hir::HirStatement::Let(let_stmt) => {
                            // Substitute in the let value using current accumulated map
                            let sub_value =
                                self.substitute_var_ids_in_expr(&let_stmt.value, &local_var_id_map);

                            // Add this let binding to the local map for subsequent statements
                            local_var_id_map.insert(let_stmt.id, sub_value.clone());

                            substituted_stmts.push(hir::HirStatement::Let(hir::HirLetStatement {
                                id: let_stmt.id,
                                name: let_stmt.name.clone(),
                                var_type: let_stmt.var_type.clone(),
                                mutable: let_stmt.mutable,
                                value: sub_value,
                            }));
                        }
                        hir::HirStatement::Assignment(assign_stmt) => {
                            let sub_rhs = self
                                .substitute_var_ids_in_expr(&assign_stmt.rhs, &local_var_id_map);
                            substituted_stmts.push(hir::HirStatement::Assignment(
                                hir::HirAssignment {
                                    id: assign_stmt.id,
                                    lhs: assign_stmt.lhs.clone(),
                                    assignment_type: assign_stmt.assignment_type.clone(),
                                    rhs: sub_rhs,
                                },
                            ));
                        }
                        _ => substituted_stmts.push(stmt.clone()),
                    }
                }

                // BUG FIX #120: Use the local map (with Block's let bindings) for result_expr
                let sub_result = self.substitute_var_ids_in_expr(result_expr, &local_var_id_map);
                hir::HirExpression::Block {
                    statements: substituted_stmts,
                    result_expr: Box::new(sub_result),
                }
            }

            // BUG FIX #106: Match - recursively process match value and arm expressions
            hir::HirExpression::Match(match_expr) => {
                let sub_expr = self.substitute_var_ids_in_expr(&match_expr.expr, var_id_map);
                let sub_arms: Vec<_> = match_expr
                    .arms
                    .iter()
                    .map(|arm| {
                        let sub_guard = arm
                            .guard
                            .as_ref()
                            .map(|g| self.substitute_var_ids_in_expr(g, var_id_map));
                        let sub_arm_expr = self.substitute_var_ids_in_expr(&arm.expr, var_id_map);
                        hir::HirMatchArmExpr {
                            pattern: arm.pattern.clone(),
                            guard: sub_guard,
                            expr: sub_arm_expr,
                        }
                    })
                    .collect();
                hir::HirExpression::Match(hir::HirMatchExpr {
                    expr: Box::new(sub_expr),
                    arms: sub_arms,
                    mux_style: match_expr.mux_style,
                })
            }

            // Everything else - return as-is
            _ => expr.clone(),
        }
    }

    /// BUG FIX #107: Substitute Variables iteratively until no more substitutions happen.
    /// This is needed because when Variable(X) is replaced with its value, that value
    /// might contain other Variables (Variable(Y), Variable(Z)) that also need substitution.
    /// A single pass of substitute_var_ids_in_expr only replaces the top-level Variables;
    /// nested Variables in the replacement values need additional passes.
    ///
    /// Uses proper fixed-point detection by counting Variables from var_id_map in the expression.
    fn substitute_var_ids_until_fixed_point(
        &self,
        expr: &hir::HirExpression,
        var_id_map: &IndexMap<hir::VariableId, hir::HirExpression>,
    ) -> hir::HirExpression {
        if var_id_map.is_empty() {
            return expr.clone();
        }

        let var_ids: std::collections::HashSet<hir::VariableId> =
            var_id_map.keys().cloned().collect();
        let mut current = expr.clone();
        let max_iterations = 100; // Safety limit

        for iteration in 0..max_iterations {
            let count_before = Self::count_variables_from_set(&current, &var_ids);
            if count_before == 0 {
                // No Variables from var_id_map left, we're done
                return current;
            }

            let next = self.substitute_var_ids_in_expr(&current, var_id_map);
            let count_after = Self::count_variables_from_set(&next, &var_ids);

            if count_after == 0 {
                // All Variables substituted
                return next;
            }

            // Early termination: if substitution didn't reduce variable count, we're done
            // This handles the case where remaining variables are not in var_id_map
            if count_after >= count_before {
                return next;
            }

            current = next;
        }

        debug_println!("[BUG #109 WARNING] substitute_var_ids_until_fixed_point: hit max iterations ({}), returning current state", max_iterations);
        current
    }

    /// Count how many Variables from the given set appear in the expression
    fn count_variables_from_set(
        expr: &hir::HirExpression,
        var_ids: &std::collections::HashSet<hir::VariableId>,
    ) -> usize {
        match expr {
            hir::HirExpression::Variable(var_id) => {
                if var_ids.contains(var_id) {
                    1
                } else {
                    0
                }
            }
            hir::HirExpression::TupleLiteral(elements) => elements
                .iter()
                .map(|e| Self::count_variables_from_set(e, var_ids))
                .sum(),
            hir::HirExpression::Call(call) => call
                .args
                .iter()
                .map(|a| Self::count_variables_from_set(a, var_ids))
                .sum(),
            hir::HirExpression::Binary(bin) => {
                Self::count_variables_from_set(&bin.left, var_ids)
                    + Self::count_variables_from_set(&bin.right, var_ids)
            }
            hir::HirExpression::Unary(unary) => {
                Self::count_variables_from_set(&unary.operand, var_ids)
            }
            hir::HirExpression::If(if_expr) => {
                Self::count_variables_from_set(&if_expr.condition, var_ids)
                    + Self::count_variables_from_set(&if_expr.then_expr, var_ids)
                    + Self::count_variables_from_set(&if_expr.else_expr, var_ids)
            }
            hir::HirExpression::Cast(cast) => Self::count_variables_from_set(&cast.expr, var_ids),
            hir::HirExpression::Concat(parts) => parts
                .iter()
                .map(|p| Self::count_variables_from_set(p, var_ids))
                .sum(),
            hir::HirExpression::FieldAccess { base, .. } => {
                Self::count_variables_from_set(base, var_ids)
            }
            hir::HirExpression::Index(base, index) => {
                Self::count_variables_from_set(base, var_ids)
                    + Self::count_variables_from_set(index, var_ids)
            }
            hir::HirExpression::Block {
                statements,
                result_expr,
            } => {
                let stmt_count: usize = statements
                    .iter()
                    .map(|s| match s {
                        hir::HirStatement::Let(let_stmt) => {
                            Self::count_variables_from_set(&let_stmt.value, var_ids)
                        }
                        hir::HirStatement::Assignment(assign) => {
                            Self::count_variables_from_set(&assign.rhs, var_ids)
                        }
                        _ => 0,
                    })
                    .sum();
                stmt_count + Self::count_variables_from_set(result_expr, var_ids)
            }
            hir::HirExpression::Match(match_expr) => {
                let expr_count = Self::count_variables_from_set(&match_expr.expr, var_ids);
                let arm_count: usize = match_expr
                    .arms
                    .iter()
                    .map(|arm| {
                        let guard_count = arm
                            .guard
                            .as_ref()
                            .map(|g| Self::count_variables_from_set(g, var_ids))
                            .unwrap_or(0);
                        guard_count + Self::count_variables_from_set(&arm.expr, var_ids)
                    })
                    .sum();
                expr_count + arm_count
            }
            // Literals and GenericParams don't contain Variables
            _ => 0,
        }
    }

    /// BUG FIX #103: Substitute Variables by VariableId, but ONLY if the VariableId is in the
    /// allowed set (local_var_ids). This prevents incorrect substitution when a Variable from
    /// an outer scope (e.g., caller function) collides with a local Block Variable.
    ///
    /// Example: When fp_sub(neg_b, sqrt_disc) is inlined into quadratic_solve:
    /// - fp_sub's Block has local Variable(6)=a_fp
    /// - quadratic_solve's sqrt_disc is also Variable(6)
    /// - Without filtering, sqrt_disc would incorrectly get replaced with a_fp's value
    #[allow(clippy::only_used_in_recursion)]
    fn substitute_var_ids_in_expr_filtered(
        &self,
        expr: &hir::HirExpression,
        var_id_map: &IndexMap<hir::VariableId, hir::HirExpression>,
        local_var_ids: &std::collections::HashSet<hir::VariableId>,
    ) -> hir::HirExpression {
        match expr {
            hir::HirExpression::Variable(var_id) => {
                // BUG FIX #103: Only substitute if this is a LOCAL variable (defined in this Block)
                if local_var_ids.contains(var_id) {
                    if let Some(replacement) = var_id_map.get(var_id) {
                        println!(
                            "[BUG #103 VAR_ID_FILTERED] Replacing local Variable({}) with {:?}",
                            var_id.0,
                            std::mem::discriminant(replacement)
                        );
                        return replacement.clone();
                    }
                } else {
                    // This is a Variable from outer scope - do NOT substitute
                    debug_println!("[BUG #103 VAR_ID_FILTERED] Preserving outer-scope Variable({}) (not in local_var_ids)",
                             var_id.0);
                }
                expr.clone()
            }

            hir::HirExpression::TupleLiteral(elements) => {
                let substituted: Vec<_> = elements
                    .iter()
                    .map(|elem| {
                        self.substitute_var_ids_in_expr_filtered(elem, var_id_map, local_var_ids)
                    })
                    .collect();
                hir::HirExpression::TupleLiteral(substituted)
            }

            hir::HirExpression::Call(call_expr) => {
                let substituted_args: Vec<_> = call_expr
                    .args
                    .iter()
                    .map(|arg| {
                        self.substitute_var_ids_in_expr_filtered(arg, var_id_map, local_var_ids)
                    })
                    .collect();
                hir::HirExpression::Call(hir::HirCallExpr {
                    function: call_expr.function.clone(),
                    args: substituted_args,
                    type_args: call_expr.type_args.clone(),
                    named_type_args: call_expr.named_type_args.clone(),
                    impl_style: call_expr.impl_style,
                })
            }

            hir::HirExpression::Binary(bin_expr) => {
                let left_sub = self.substitute_var_ids_in_expr_filtered(
                    &bin_expr.left,
                    var_id_map,
                    local_var_ids,
                );
                let right_sub = self.substitute_var_ids_in_expr_filtered(
                    &bin_expr.right,
                    var_id_map,
                    local_var_ids,
                );
                hir::HirExpression::Binary(hir::HirBinaryExpr {
                    op: bin_expr.op.clone(),
                    left: Box::new(left_sub),
                    right: Box::new(right_sub),
                    is_trait_op: bin_expr.is_trait_op,
                })
            }

            hir::HirExpression::If(if_expr) => {
                let cond_sub = self.substitute_var_ids_in_expr_filtered(
                    &if_expr.condition,
                    var_id_map,
                    local_var_ids,
                );
                let then_sub = self.substitute_var_ids_in_expr_filtered(
                    &if_expr.then_expr,
                    var_id_map,
                    local_var_ids,
                );
                let else_sub = self.substitute_var_ids_in_expr_filtered(
                    &if_expr.else_expr,
                    var_id_map,
                    local_var_ids,
                );
                hir::HirExpression::If(hir::HirIfExpr {
                    condition: Box::new(cond_sub),
                    then_expr: Box::new(then_sub),
                    else_expr: Box::new(else_sub),
                })
            }

            hir::HirExpression::Concat(parts) => {
                let substituted: Vec<_> = parts
                    .iter()
                    .map(|part| {
                        self.substitute_var_ids_in_expr_filtered(part, var_id_map, local_var_ids)
                    })
                    .collect();
                hir::HirExpression::Concat(substituted)
            }

            hir::HirExpression::Cast(cast_expr) => {
                let sub_expr = self.substitute_var_ids_in_expr_filtered(
                    &cast_expr.expr,
                    var_id_map,
                    local_var_ids,
                );
                hir::HirExpression::Cast(hir::HirCastExpr {
                    expr: Box::new(sub_expr),
                    target_type: cast_expr.target_type.clone(),
                })
            }

            hir::HirExpression::FieldAccess { base, field } => {
                let sub_base =
                    self.substitute_var_ids_in_expr_filtered(base, var_id_map, local_var_ids);
                hir::HirExpression::FieldAccess {
                    base: Box::new(sub_base),
                    field: field.clone(),
                }
            }

            hir::HirExpression::Unary(unary_expr) => {
                let sub_operand = self.substitute_var_ids_in_expr_filtered(
                    &unary_expr.operand,
                    var_id_map,
                    local_var_ids,
                );
                hir::HirExpression::Unary(hir::HirUnaryExpr {
                    op: unary_expr.op.clone(),
                    operand: Box::new(sub_operand),
                })
            }

            hir::HirExpression::Index(base, index) => {
                let sub_base =
                    self.substitute_var_ids_in_expr_filtered(base, var_id_map, local_var_ids);
                let sub_index =
                    self.substitute_var_ids_in_expr_filtered(index, var_id_map, local_var_ids);
                hir::HirExpression::Index(Box::new(sub_base), Box::new(sub_index))
            }

            // BUG FIX #146: Range - substitute in base, high, and low expressions
            hir::HirExpression::Range(base, high, low) => {
                let sub_base =
                    self.substitute_var_ids_in_expr_filtered(base, var_id_map, local_var_ids);
                let sub_high =
                    self.substitute_var_ids_in_expr_filtered(high, var_id_map, local_var_ids);
                let sub_low =
                    self.substitute_var_ids_in_expr_filtered(low, var_id_map, local_var_ids);
                hir::HirExpression::Range(Box::new(sub_base), Box::new(sub_high), Box::new(sub_low))
            }

            // Everything else - return as-is
            _ => expr.clone(),
        }
    }

    /// BUG FIX #103: Collect all VariableIds from an expression
    /// This is used to identify Variables from the outer scope (parameter arguments)
    /// that should NOT be subject to var_id_to_value substitution in inlined function bodies.
    fn collect_variable_ids_from_expr(
        expr: &hir::HirExpression,
        var_ids: &mut std::collections::HashSet<hir::VariableId>,
    ) {
        match expr {
            hir::HirExpression::Variable(var_id) => {
                var_ids.insert(*var_id);
            }
            hir::HirExpression::TupleLiteral(elements) => {
                for elem in elements {
                    Self::collect_variable_ids_from_expr(elem, var_ids);
                }
            }
            hir::HirExpression::Call(call) => {
                for arg in &call.args {
                    Self::collect_variable_ids_from_expr(arg, var_ids);
                }
            }
            hir::HirExpression::Binary(bin) => {
                Self::collect_variable_ids_from_expr(&bin.left, var_ids);
                Self::collect_variable_ids_from_expr(&bin.right, var_ids);
            }
            hir::HirExpression::If(if_expr) => {
                Self::collect_variable_ids_from_expr(&if_expr.condition, var_ids);
                Self::collect_variable_ids_from_expr(&if_expr.then_expr, var_ids);
                Self::collect_variable_ids_from_expr(&if_expr.else_expr, var_ids);
            }
            hir::HirExpression::Concat(parts) => {
                for part in parts {
                    Self::collect_variable_ids_from_expr(part, var_ids);
                }
            }
            hir::HirExpression::Cast(cast) => {
                Self::collect_variable_ids_from_expr(&cast.expr, var_ids);
            }
            hir::HirExpression::FieldAccess { base, .. } => {
                Self::collect_variable_ids_from_expr(base, var_ids);
            }
            hir::HirExpression::Unary(unary) => {
                Self::collect_variable_ids_from_expr(&unary.operand, var_ids);
            }
            hir::HirExpression::Index(base, index) => {
                Self::collect_variable_ids_from_expr(base, var_ids);
                Self::collect_variable_ids_from_expr(index, var_ids);
            }
            // BUG FIX #146: Range - collect from base, high, and low
            hir::HirExpression::Range(base, high, low) => {
                Self::collect_variable_ids_from_expr(base, var_ids);
                Self::collect_variable_ids_from_expr(high, var_ids);
                Self::collect_variable_ids_from_expr(low, var_ids);
            }
            _ => {}
        }
    }

    /// BUG FIX #86: Helper to substitute parameters in a statement
    /// Handles Let, If, Assign statements inside blocks during function inlining
    fn substitute_hir_stmt_with_maps(
        &self,
        stmt: &hir::HirStatement,
        name_map: &IndexMap<String, hir::HirExpression>,
        var_id_map: &IndexMap<hir::VariableId, hir::HirExpression>,
    ) -> hir::HirStatement {
        match stmt {
            hir::HirStatement::Let(let_stmt) => {
                let mut sub_value = self.substitute_hir_expr_with_map(&let_stmt.value, name_map);
                sub_value = self.substitute_var_ids_in_expr(&sub_value, var_id_map);
                hir::HirStatement::Let(hir::HirLetStatement {
                    id: let_stmt.id,
                    name: let_stmt.name.clone(),
                    mutable: let_stmt.mutable,
                    var_type: let_stmt.var_type.clone(),
                    value: sub_value,
                })
            }
            hir::HirStatement::If(if_stmt) => {
                let mut sub_cond = self.substitute_hir_expr_with_map(&if_stmt.condition, name_map);
                sub_cond = self.substitute_var_ids_in_expr(&sub_cond, var_id_map);

                let sub_then: Vec<_> = if_stmt
                    .then_statements
                    .iter()
                    .map(|s| self.substitute_hir_stmt_with_maps(s, name_map, var_id_map))
                    .collect();

                let sub_else: Option<Vec<_>> = if_stmt.else_statements.as_ref().map(|stmts| {
                    stmts
                        .iter()
                        .map(|s| self.substitute_hir_stmt_with_maps(s, name_map, var_id_map))
                        .collect()
                });

                hir::HirStatement::If(hir::HirIfStatement {
                    condition: sub_cond,
                    then_statements: sub_then,
                    else_statements: sub_else,
                    mux_style: if_stmt.mux_style,
                })
            }
            hir::HirStatement::Assignment(assign_stmt) => {
                let mut sub_rhs = self.substitute_hir_expr_with_map(&assign_stmt.rhs, name_map);
                sub_rhs = self.substitute_var_ids_in_expr(&sub_rhs, var_id_map);
                hir::HirStatement::Assignment(hir::HirAssignment {
                    id: assign_stmt.id,
                    lhs: assign_stmt.lhs.clone(),
                    assignment_type: assign_stmt.assignment_type.clone(),
                    rhs: sub_rhs,
                })
            }
            _ => stmt.clone(),
        }
    }

    /// BUG #86: Helper to substitute statements in a Block with reference-based param_map
    /// This is used by substitute_expression_with_var_map for If/Assignment statements
    fn substitute_hir_stmt_in_block(
        &mut self,
        stmt: &hir::HirStatement,
        param_map: &IndexMap<String, &hir::HirExpression>,
        var_id_to_name: &IndexMap<hir::VariableId, String>,
    ) -> Option<hir::HirStatement> {
        match stmt {
            hir::HirStatement::Let(let_stmt) => {
                let sub_value = self.substitute_expression_with_var_map(
                    &let_stmt.value,
                    param_map,
                    var_id_to_name,
                )?;
                Some(hir::HirStatement::Let(hir::HirLetStatement {
                    id: let_stmt.id,
                    name: let_stmt.name.clone(),
                    mutable: let_stmt.mutable,
                    var_type: let_stmt.var_type.clone(),
                    value: sub_value,
                }))
            }
            hir::HirStatement::If(if_stmt) => {
                let sub_cond = self.substitute_expression_with_var_map(
                    &if_stmt.condition,
                    param_map,
                    var_id_to_name,
                )?;

                let sub_then: Vec<_> = if_stmt
                    .then_statements
                    .iter()
                    .filter_map(|s| self.substitute_hir_stmt_in_block(s, param_map, var_id_to_name))
                    .collect();

                let sub_else: Option<Vec<_>> = if_stmt.else_statements.as_ref().map(|stmts| {
                    stmts
                        .iter()
                        .filter_map(|s| {
                            self.substitute_hir_stmt_in_block(s, param_map, var_id_to_name)
                        })
                        .collect()
                });

                Some(hir::HirStatement::If(hir::HirIfStatement {
                    condition: sub_cond,
                    then_statements: sub_then,
                    else_statements: sub_else,
                    mux_style: if_stmt.mux_style,
                }))
            }
            hir::HirStatement::Assignment(assign_stmt) => {
                let sub_rhs = self.substitute_expression_with_var_map(
                    &assign_stmt.rhs,
                    param_map,
                    var_id_to_name,
                )?;
                Some(hir::HirStatement::Assignment(hir::HirAssignment {
                    id: assign_stmt.id,
                    lhs: assign_stmt.lhs.clone(),
                    assignment_type: assign_stmt.assignment_type.clone(),
                    rhs: sub_rhs,
                }))
            }
            _ => Some(stmt.clone()),
        }
    }

    fn inline_function_call(&mut self, call: &hir::HirCallExpr) -> Option<Expression> {
        println!("🚨🚨🚨 INLINE_FUNCTION_CALL: {} 🚨🚨🚨", call.function);
        println!(
            "[DEBUG] inline_function_call: {} with {} args",
            call.function,
            call.args.len()
        );

        // Step 1: Find the function and clone its body to avoid borrow checker issues
        let func = match self.find_function(&call.function) {
            Some(f) => {
                debug_println!("[DEBUG] ✓ Found function '{}'", call.function);
                f
            }
            None => {
                eprintln!(
                    "❌❌❌ ERROR: Function '{}' NOT FOUND during inlining! ❌❌❌",
                    call.function
                );
                debug_println!("[DEBUG] Available functions in HIR:");
                let hir = self.hir?;
                for (idx, f) in hir.functions.iter().enumerate() {
                    eprintln!("  {}. {}", idx + 1, f.name);
                }
                return None;
            }
        };
        eprintln!(
            "[DEBUG] inline_function_call: found function {}, body has {} stmts",
            call.function,
            func.body.len()
        );

        // Clone the data we need before doing mutable operations
        let params = func.params.clone();
        let body = func.body.clone();
        let return_type = func.return_type.clone(); // BUG #76 FIX: Clone return type for later annotation

        // ARCHITECTURAL FIX: Push a new inlining context to prevent variable ID collisions
        // This ensures variables from different functions don't collide even if they have the same HIR VariableId
        let inlining_context_id = self.next_inlining_context_id;
        self.next_inlining_context_id += 1;
        self.inlining_context_stack.push(inlining_context_id);

        // BUG #202 FIX: Save and clear placeholder maps when entering a new inlining context
        // Different functions may use the same VariableId for different purposes (e.g., VariableId(30)
        // might be a 1-bit comparator output in one function and an FP32 value in another).
        // Without clearing, stale mappings from previous inlining contexts cause type mismatches.
        let mut saved_placeholder_signal_map =
            std::mem::take(&mut self.placeholder_signal_to_entity_output);
        let mut saved_placeholder_hir_signal_map =
            std::mem::take(&mut self.placeholder_hir_signal_to_entity_output);

        // Guard against excessive function inlining depth (indirect recursion)
        if self.inlining_context_stack.len() > MAX_EXPRESSION_RECURSION_DEPTH {
            panic!(
                "[HIR_TO_MIR] Function inlining depth exceeded {} - likely indirect recursion in function calls (function '{}', inlining context {}, call chain depth: {})",
                MAX_EXPRESSION_RECURSION_DEPTH,
                call.function,
                inlining_context_id,
                self.inlining_context_stack.len()
            );
        }

        eprintln!(
            "[CONTEXT] Pushed inlining context {} for function '{}', stack depth: {}",
            inlining_context_id,
            call.function,
            self.inlining_context_stack.len()
        );

        eprintln!(
            "[DEBUG] inline_function_call: original body has {} statements",
            body.len()
        );
        for (i, stmt) in body.iter().enumerate() {
            if let hir::HirStatement::Let(let_stmt) = stmt {
                eprintln!("  [{}] Let: {}", i, let_stmt.name);
            } else {
                eprintln!("  [{}] {:?}", i, std::mem::discriminant(stmt));
            }
        }

        // Transform early returns into nested if-else before processing
        let body = self.transform_early_returns(body);

        eprintln!(
            "[DEBUG] inline_function_call: after transform_early_returns, body has {} statements",
            body.len()
        );
        for (i, stmt) in body.iter().enumerate() {
            if let hir::HirStatement::Let(let_stmt) = stmt {
                eprintln!("  [{}] Let: {}", i, let_stmt.name);
            } else {
                eprintln!("  [{}] {:?}", i, std::mem::discriminant(stmt));
            }
        }

        // Phase 5: Check for direct recursion
        if self.contains_recursive_call(&body, &call.function) {
            eprintln!(
                "Error: Recursive function calls are not supported: function '{}'",
                call.function
            );
            // BUG #202 FIX: Restore placeholder maps before returning
            self.placeholder_signal_to_entity_output = saved_placeholder_signal_map;
            self.placeholder_hir_signal_to_entity_output = saved_placeholder_hir_signal_map;
            self.inlining_context_stack.pop();
            debug_println!("[CONTEXT] Popped inlining context (recursion check failed)");
            return None;
        }

        // Step 2: Check arity
        if params.len() != call.args.len() {
            eprintln!(
                "Function {}: expected {} arguments, got {}",
                call.function,
                params.len(),
                call.args.len()
            );
            // BUG #202 FIX: Restore placeholder maps before returning
            self.placeholder_signal_to_entity_output = saved_placeholder_signal_map;
            self.placeholder_hir_signal_to_entity_output = saved_placeholder_hir_signal_map;
            self.inlining_context_stack.pop();
            debug_println!("[CONTEXT] Popped inlining context (arity check failed)");
            return None;
        }

        // Step 3: Build initial parameter substitution map (function parameters -> arguments)
        let mut substitution_map = IndexMap::new();
        for (param, arg) in params.iter().zip(&call.args) {
            substitution_map.insert(param.name.clone(), arg.clone());
        }
        eprintln!(
            "[DEBUG] inline_function_call: built param map with {} entries",
            substitution_map.len()
        );
        // Debug: Show what's in the param map
        for (param_name, arg_expr) in &substitution_map {
            eprintln!(
                "[BUG #IMPORT_MATCH] inline_function_call '{}': param_map['{}'] -> {:?}",
                call.function,
                param_name,
                std::mem::discriminant(arg_expr)
            );
        }

        // Step 4: Build var_id -> name mapping from ALL let statements (including nested ones)
        // This allows us to look up variable names during substitution
        let mut var_id_to_name = IndexMap::new();
        self.collect_let_bindings(&body, &mut var_id_to_name);
        eprintln!(
            "[DEBUG] inline_function_call: built var_id map with {} entries (recursive)",
            var_id_to_name.len()
        );
        for (var_id, var_name) in &var_id_to_name {
            eprintln!(
                "[BUG #IMPORT_MATCH] inline_function_call '{}': var_id_to_name[{:?}] = '{}'",
                call.function, var_id, var_name
            );
        }

        // Step 5: Convert statement-based body to expression (handles early returns)
        println!(
            "🌟🌟🌟 [INLINE] Step 5: About to convert_body_to_expression for '{}' 🌟🌟🌟",
            call.function
        );
        let body_expr = self.convert_body_to_expression(&body);
        if body_expr.is_none() {
            eprintln!(
                "[DEBUG] inline_function_call: convert_body_to_expression FAILED - returning None"
            );
            // BUG #202 FIX: Restore placeholder maps before returning
            self.placeholder_signal_to_entity_output = saved_placeholder_signal_map;
            self.placeholder_hir_signal_to_entity_output = saved_placeholder_hir_signal_map;
            self.inlining_context_stack.pop();
            debug_println!("[CONTEXT] Popped inlining context (convert_body_to_expression failed)");
            return None;
        }
        let body_expr = body_expr?;
        println!(
            "🌟🌟🌟 [INLINE] Step 5 DONE: body_expr type: {:?} 🌟🌟🌟",
            std::mem::discriminant(&body_expr)
        );

        // Step 6: Substitute parameters in the entire expression (including nested let bindings)
        debug_println!(
            "🌟🌟🌟 [INLINE] Step 6: About to substitute_expression_with_var_map 🌟🌟🌟"
        );
        let substituted_expr = self.substitute_expression_with_var_map(
            &body_expr,
            &substitution_map
                .iter()
                .map(|(k, v)| (k.clone(), v))
                .collect(),
            &var_id_to_name,
        );
        if substituted_expr.is_none() {
            debug_println!("[DEBUG] inline_function_call: substitute_expression_with_var_map FAILED - returning None");
            // BUG #202 FIX: Restore placeholder maps before returning
            self.placeholder_signal_to_entity_output = saved_placeholder_signal_map;
            self.placeholder_hir_signal_to_entity_output = saved_placeholder_hir_signal_map;
            self.inlining_context_stack.pop();
            eprintln!(
                "[CONTEXT] Popped inlining context (substitute_expression_with_var_map failed)"
            );
            return None;
        }
        let substituted_expr = substituted_expr?;
        println!(
            "🌟🌟🌟 [INLINE] Step 6 DONE: substituted_expr type: {:?} 🌟🌟🌟",
            std::mem::discriminant(&substituted_expr)
        );
        println!("🟢🟢🟢 [BUG #187 DEBUG] AFTER Step 6 DONE, before expr_type_name 🟢🟢🟢");

        // Step 7: Convert the substituted HIR expression to MIR
        let expr_type_name = match &substituted_expr {
            hir::HirExpression::Literal(_) => "Literal",
            hir::HirExpression::Variable(_) => "Variable",
            hir::HirExpression::Binary(_) => "Binary",
            hir::HirExpression::Unary(_) => "Unary",
            hir::HirExpression::Call(_) => "Call",
            hir::HirExpression::Signal(_) => "Signal",
            hir::HirExpression::Port(_) => "Port",
            hir::HirExpression::Index(_, _) => "Index",
            hir::HirExpression::Concat(_) => "Concat",
            hir::HirExpression::Range(_, _, _) => "Range",
            hir::HirExpression::If(_) => "If",
            hir::HirExpression::Match(_) => "Match",
            hir::HirExpression::Cast(_) => "Cast",
            hir::HirExpression::Block { .. } => "Block",
            hir::HirExpression::FieldAccess { .. } => "FieldAccess",
            hir::HirExpression::ArrayLiteral { .. } => "ArrayLiteral",
            _ => "Other",
        };
        eprintln!(
            "[DEBUG] inline_function_call: '{}' substituted expression type: {}",
            call.function, expr_type_name
        );
        eprintln!(
            "[BUG #IMPORT_MATCH] inline_function_call '{}': Before convert_expression, match_arm_prefix={:?}",
            call.function, self.match_arm_prefix
        );
        // DEBUG: Check if substituted_expr still contains GenericParam
        fn check_for_generic_param(expr: &hir::HirExpression, path: &str) {
            match expr {
                hir::HirExpression::GenericParam(name) => {
                    println!("🚨 FOUND GenericParam('{}') at {}", name, path);
                }
                hir::HirExpression::Range(base, high, low) => {
                    check_for_generic_param(base, &format!("{}.Range.base", path));
                    check_for_generic_param(high, &format!("{}.Range.high", path));
                    check_for_generic_param(low, &format!("{}.Range.low", path));
                }
                hir::HirExpression::Block {
                    statements,
                    result_expr,
                } => {
                    for (i, stmt) in statements.iter().enumerate() {
                        if let hir::HirStatement::Let(let_stmt) = stmt {
                            check_for_generic_param(
                                &let_stmt.value,
                                &format!("{}.Block.let[{}]", path, i),
                            );
                        }
                    }
                    check_for_generic_param(result_expr, &format!("{}.Block.result", path));
                }
                hir::HirExpression::Match(m) => {
                    check_for_generic_param(&m.expr, &format!("{}.Match.expr", path));
                    for (i, arm) in m.arms.iter().enumerate() {
                        check_for_generic_param(&arm.expr, &format!("{}.Match.arm[{}]", path, i));
                    }
                }
                hir::HirExpression::If(if_expr) => {
                    check_for_generic_param(&if_expr.condition, &format!("{}.If.cond", path));
                    check_for_generic_param(&if_expr.then_expr, &format!("{}.If.then", path));
                    check_for_generic_param(&if_expr.else_expr, &format!("{}.If.else", path));
                }
                hir::HirExpression::Binary(bin) => {
                    check_for_generic_param(&bin.left, &format!("{}.Binary.left", path));
                    check_for_generic_param(&bin.right, &format!("{}.Binary.right", path));
                }
                hir::HirExpression::Cast(cast) => {
                    check_for_generic_param(&cast.expr, &format!("{}.Cast.expr", path));
                }
                _ => {}
            }
        }
        debug_println!("🔍 Checking substituted_expr for remaining GenericParams...");
        check_for_generic_param(&substituted_expr, "root");

        // BUG #160 FIX: Push return type before converting so concat can infer widths
        println!(
            "🟢🟢🟢 [BUG #187 DEBUG] About to push return_type={:?} 🟢🟢🟢",
            return_type
        );
        self.inlining_return_type_stack.push(return_type.clone());
        println!("🟢🟢🟢 [BUG #187 DEBUG] About to call convert_expression 🟢🟢🟢");
        let result = self.convert_expression(&substituted_expr, 0);
        println!(
            "🟢🟢🟢 [BUG #187 DEBUG] convert_expression returned is_some={} 🟢🟢🟢",
            result.is_some()
        );
        self.inlining_return_type_stack.pop();

        // BUG #76 FIX: Annotate the result expression with the function's return type
        // This allows tuple literals to have correct element widths
        let result = result.map(|expr| {
            if let Some(ref ret_ty) = return_type {
                let return_type_ty = self.hir_type_to_type(ret_ty);
                eprintln!(
                    "[BUG #76] inline_function_call '{}': Annotating result with return type: {:?}",
                    call.function, return_type_ty
                );
                self.annotate_expression_with_type(expr, return_type_ty, 0)
            } else {
                expr
            }
        });

        eprintln!(
            "[BUG #IMPORT_MATCH] inline_function_call '{}': After convert_expression, match_arm_prefix={:?}, result is_some={}",
            call.function, self.match_arm_prefix, result.is_some()
        );
        if result.is_none() {
            eprintln!(
                "[DEBUG] inline_function_call: '{}' FAILED to convert substituted expression to MIR (type: {})",
                call.function,
                expr_type_name
            );
        } else {
            eprintln!(
                "[DEBUG] inline_function_call: '{}' successfully converted to MIR",
                call.function
            );
            if let Some(ref expr) = result {
                eprintln!(
                    "[BUG #IMPORT_MATCH] inline_function_call '{}': Result expression type: {:?}",
                    call.function,
                    std::mem::discriminant(&expr.kind)
                );
            }
        }

        // Step 8: Clean up function-local variables from variable_map to prevent scope leakage
        // Function-local variables should not persist across different function inlining contexts.
        // Without this cleanup, variables from one match arm can incorrectly be reused in another arm.
        //
        // BUG #71 FIX: BUT don't remove variables that have been preserved to dynamic_variables!
        // These variables (preserved by BUG #68) need to persist for use in pending statements.
        //
        // BUG #153 FIX: Don't remove variables that existed in variable_map BEFORE this function
        // inlining. The var_id_to_name map contains HIR VariableIds from the inlined function,
        // but these IDs might collide with entity-level variables that were registered earlier.
        // We should only remove variables that were ADDED during this inlining.
        //
        // NOTE: Since we now use context_variable_map for all context-aware lookups (line 1967),
        // and entity-level variables are protected by entry().or_insert() (line 1967),
        // we should NOT need to remove anything from variable_map here. The context-specific
        // variables are stored in context_variable_map, not variable_map.
        //
        // ACTUALLY: The safest fix is to simply NOT remove anything from variable_map.
        // Entity-level variables need to persist, and function-local variables are looked up
        // via context_variable_map. Removing from variable_map can only cause problems.
        //
        // DISABLED: The original removal logic is commented out to fix BUG #153
        // for var_id in var_id_to_name.keys() {
        //     // Only remove if NOT in dynamic_variables (not preserved)
        //     if !self.dynamic_variables.contains_key(var_id) {
        //         self.variable_map.remove(var_id);
        //     }
        // }

        // ARCHITECTURAL FIX: Pop the inlining context before returning
        // BUG #202 FIX: Merge new placeholder mappings back into saved maps, then restore
        // Entity instantiations inside the inlined function create new mappings that must persist
        // at the module level. We merge the new mappings back rather than discarding them.
        for (k, v) in std::mem::take(&mut self.placeholder_signal_to_entity_output) {
            saved_placeholder_signal_map.entry(k).or_insert(v);
        }
        for (k, v) in std::mem::take(&mut self.placeholder_hir_signal_to_entity_output) {
            saved_placeholder_hir_signal_map.entry(k).or_insert(v);
        }
        self.placeholder_signal_to_entity_output = saved_placeholder_signal_map;
        self.placeholder_hir_signal_to_entity_output = saved_placeholder_hir_signal_map;
        self.inlining_context_stack.pop();
        eprintln!(
            "[CONTEXT] Popped inlining context for function '{}', stack depth: {}",
            call.function,
            self.inlining_context_stack.len()
        );

        result
    }

    /// BUG #178 FIX: Inline a function call using pre-converted MIR arguments
    ///
    /// This is used when inlining within module context (e.g., in #[parallel] match arms)
    /// where the arguments have already been converted to MIR expressions with proper
    /// context-aware substitutions (e.g., local let bindings from mir_cache).
    ///
    /// The key difference from `inline_function_call` is that we substitute the function
    /// body's parameter references with MIR expressions directly, instead of re-converting
    /// HIR arguments which would lose the mir_cache context.
    fn inline_function_call_with_mir_args(
        &mut self,
        call: &hir::HirCallExpr,
        mir_args: &[Expression],
    ) -> Option<Expression> {
        // Step 1: Find the function
        let func = self.find_function(&call.function)?;
        let params = func.params.clone();
        let body = func.body.clone();
        let return_type = func.return_type.clone();

        // Check arity
        if params.len() != mir_args.len() {
            eprintln!(
                "BUG #178: Function '{}' expected {} args, got {}",
                call.function,
                params.len(),
                mir_args.len()
            );
            return None;
        }

        // Build param_name -> MIR expression map
        let mut param_to_mir: IndexMap<String, Expression> = IndexMap::new();
        for (param, mir_expr) in params.iter().zip(mir_args.iter()) {
            param_to_mir.insert(param.name.clone(), mir_expr.clone());
        }

        // Transform early returns
        let body = self.transform_early_returns(body);

        // Build var_id -> name mapping for let bindings
        let mut var_id_to_name: IndexMap<hir::VariableId, String> = IndexMap::new();
        self.collect_let_bindings(&body, &mut var_id_to_name);

        // BUG #179 FIX: Also build param var_id -> name mapping
        // Parameters are typically VariableId(0), (1), (2)...
        for (i, param) in params.iter().enumerate() {
            var_id_to_name.insert(hir::VariableId(i as u32), param.name.clone());
        }

        // BUG #181 FIX: DON'T substitute parameters with caller's HIR expressions.
        // Previously, we mapped param names -> caller's Variable(id), but those Variables
        // are from the CALLER's context and can't be resolved in convert_expression.
        //
        // Instead, parameters are resolved via:
        // 1. GenericParam("name") -> pending_mir_param_subs["name"] (in convert_expression)
        // 2. Variable(param_id) -> pending_param_var_to_name[param_id] -> pending_mir_param_subs[name]
        //
        // This is critical for functions with local let bindings like:
        //   let prod_x = fp_mul(ax, bx);
        //   let result = fp_add(prod_x, prod_y);
        //   return result;  // result must be substituted with fp_add expression
        let hir_param_subs: IndexMap<String, hir::HirExpression> = IndexMap::new();
        // NOTE: We no longer populate hir_param_subs with caller's HIR args.
        // Parameters are handled via pending_mir_param_subs during MIR conversion.

        let mut let_bindings: IndexMap<String, hir::HirExpression> = IndexMap::new();

        // BUG #180 FIX: Track mutable variables separately - they need to be processed
        // through their full assignment chain for functions like bitreverse32:
        //   let mut x = value;
        //   x = f1(x);  // Assignment, not let
        //   x = f2(x);
        //   return x;
        let mut mutable_vars: IndexMap<String, hir::VariableId> = IndexMap::new();

        // Enum to represent let or assignment statements in order
        #[derive(Debug)]
        enum BindingOp {
            Let {
                name: String,
                value: hir::HirExpression,
                is_mutable: bool,
                id: hir::VariableId,
            },
            Assign {
                var_id: hir::VariableId,
                value: hir::HirExpression,
            },
        }

        // Helper to recursively collect let AND assignment statements in order
        fn collect_bindings_recursive(stmts: &[hir::HirStatement], collected: &mut Vec<BindingOp>) {
            for stmt in stmts {
                match stmt {
                    hir::HirStatement::Let(let_stmt) => {
                        collected.push(BindingOp::Let {
                            name: let_stmt.name.clone(),
                            value: let_stmt.value.clone(),
                            is_mutable: let_stmt.mutable,
                            id: let_stmt.id,
                        });
                    }
                    hir::HirStatement::Assignment(assign) => {
                        // Only collect variable assignments (not signal/port)
                        if let hir::HirLValue::Variable(var_id) = &assign.lhs {
                            collected.push(BindingOp::Assign {
                                var_id: *var_id,
                                value: assign.rhs.clone(),
                            });
                        }
                    }
                    hir::HirStatement::If(if_stmt) => {
                        collect_bindings_recursive(&if_stmt.then_statements, collected);
                        if let Some(else_stmts) = &if_stmt.else_statements {
                            collect_bindings_recursive(else_stmts, collected);
                        }
                    }
                    hir::HirStatement::Block(stmts) => {
                        collect_bindings_recursive(stmts, collected);
                    }
                    _ => {}
                }
            }
        }

        let mut collected_bindings = Vec::new();
        collect_bindings_recursive(&body, &mut collected_bindings);

        // Process bindings in order, building substitution chain
        for binding in collected_bindings {
            match binding {
                BindingOp::Let {
                    name,
                    value,
                    is_mutable,
                    id,
                } => {
                    let rhs_substituted = self.substitute_hir_expr_recursively(
                        &value,
                        &hir_param_subs,
                        &let_bindings,
                        &var_id_to_name,
                    );
                    if let Some(rhs) = rhs_substituted {
                        // BUG #180 FIX: Add ALL let bindings (mutable too) to build substitution chain
                        let_bindings.insert(name.clone(), rhs);
                        if is_mutable {
                            mutable_vars.insert(name, id);
                        }
                    }
                }
                BindingOp::Assign { var_id, value } => {
                    // BUG #180 FIX: For mutable variable assignments, update the binding
                    // This handles: x = f(x) -> substitute current value of x in f(x)
                    if let Some(name) = var_id_to_name.get(&var_id) {
                        let rhs_substituted = self.substitute_hir_expr_recursively(
                            &value,
                            &hir_param_subs,
                            &let_bindings,
                            &var_id_to_name,
                        );
                        if let Some(rhs) = rhs_substituted {
                            let_bindings.insert(name.clone(), rhs);
                        }
                    }
                }
            }
        }

        println!(
            "[BUG #179 FIX] inline_function_call_with_mir_args: '{}' has {} let bindings",
            call.function,
            let_bindings.len()
        );

        // Convert body to HIR expression
        let body_expr = self.convert_body_to_expression(&body)?;

        // BUG #179 FIX: Substitute let bindings in body_expr BEFORE converting to MIR
        let substituted_body = self.substitute_hir_expr_recursively(
            &body_expr,
            &hir_param_subs,
            &let_bindings,
            &var_id_to_name,
        )?;

        println!(
            "[BUG #179 FIX] inline_function_call_with_mir_args: '{}' body after substitution: {:?}",
            call.function,
            std::mem::discriminant(&substituted_body)
        );

        // Set pending_mir_param_subs BEFORE converting body to MIR
        // This allows the GenericParam handler in convert_expression to find
        // the pre-converted MIR expressions.
        let old_subs = std::mem::replace(&mut self.pending_mir_param_subs, param_to_mir.clone());

        // BUG #181 FIX: Set pending_param_var_to_name so that Variable(param_id) can be
        // resolved to the parameter name, which is then looked up in pending_mir_param_subs.
        // Parameters are VariableId(0), (1), (2), ... matching their position in params list.
        let mut param_var_to_name: IndexMap<hir::VariableId, String> = IndexMap::new();
        for (i, param) in params.iter().enumerate() {
            param_var_to_name.insert(hir::VariableId(i as u32), param.name.clone());
        }
        let old_param_var =
            std::mem::replace(&mut self.pending_param_var_to_name, param_var_to_name);

        // BUG #194 FIX: Process entity instantiations RECURSIVELY in let_bindings BEFORE converting return expression
        // This is critical for FP operations where trait methods are inlined, creating nested structures like:
        //   let result = a_fp + b_fp;  // becomes Block { let adder = FpAdd {...}; result }
        //
        // The FpAdd entity must be instantiated (via convert_statement) before we convert
        // the return expression, because the entity instantiation creates the
        // placeholder_signal_to_entity_output mapping that links placeholder Variables to output wires.
        //
        // Helper function to recursively find entity StructLiterals in an expression
        fn find_entity_instantiations(
            expr: &hir::HirExpression,
            entities: &[skalp_frontend::hir::HirEntity],
            found: &mut Vec<(String, hir::HirStructLiteral)>,
        ) {
            match expr {
                hir::HirExpression::StructLiteral(struct_lit) => {
                    if entities.iter().any(|e| e.name == struct_lit.type_name) {
                        found.push((format!("__entity_{}", found.len()), struct_lit.clone()));
                    }
                    // Also recurse into field values
                    for field in &struct_lit.fields {
                        find_entity_instantiations(&field.value, entities, found);
                    }
                }
                hir::HirExpression::Block {
                    statements,
                    result_expr,
                } => {
                    for stmt in statements {
                        if let hir::HirStatement::Let(let_stmt) = stmt {
                            find_entity_instantiations(&let_stmt.value, entities, found);
                        }
                    }
                    find_entity_instantiations(result_expr, entities, found);
                }
                hir::HirExpression::Binary(bin) => {
                    find_entity_instantiations(&bin.left, entities, found);
                    find_entity_instantiations(&bin.right, entities, found);
                }
                hir::HirExpression::Cast(cast_expr) => {
                    find_entity_instantiations(&cast_expr.expr, entities, found);
                }
                hir::HirExpression::If(if_expr) => {
                    find_entity_instantiations(&if_expr.condition, entities, found);
                    find_entity_instantiations(&if_expr.then_expr, entities, found);
                    find_entity_instantiations(&if_expr.else_expr, entities, found);
                }
                hir::HirExpression::Call(call) => {
                    for arg in &call.args {
                        find_entity_instantiations(arg, entities, found);
                    }
                }
                _ => {}
            }
        }

        // Find entity instantiations in all let bindings
        let entities = self
            .hir
            .as_ref()
            .map(|h| h.entities.clone())
            .unwrap_or_default();
        let mut entity_insts = Vec::new();
        for binding_value in let_bindings.values() {
            find_entity_instantiations(binding_value, &entities, &mut entity_insts);
        }
        // Also check the substituted body itself
        find_entity_instantiations(&substituted_body, &entities, &mut entity_insts);

        // Process found entity instantiations
        for (name, struct_lit) in entity_insts {
            println!(
                "🔷🔷🔷 BUG #194 FIX: Processing nested entity instantiation '{}' 🔷🔷🔷",
                struct_lit.type_name
            );
            // Create a temporary Let statement and process via convert_statement
            // This will create the entity instance and set up placeholder mappings
            let temp_let = hir::HirLetStatement {
                id: hir::VariableId(u32::MAX), // Dummy ID
                name,
                value: hir::HirExpression::StructLiteral(struct_lit.clone()),
                var_type: hir::HirType::Custom(struct_lit.type_name.clone()),
                mutable: false,
            };
            let _ = self.convert_statement(&hir::HirStatement::Let(temp_let));
        }

        // Push return type for width inference
        self.inlining_return_type_stack.push(return_type.clone());
        let mut mir_body = self.convert_expression(&substituted_body, 0)?;
        self.inlining_return_type_stack.pop();

        // Restore old subs
        self.pending_mir_param_subs = old_subs;
        self.pending_param_var_to_name = old_param_var;

        // BUG #189 FIX: When the result is a Concat (from TupleLiteral), reverse the elements
        // to match the convention used by convert_hir_expr_for_module's TupleLiteral handler.
        // The FieldAccess handler in convert_hir_expr_with_mir_cache uses adjusted_index
        // assuming elements are reversed (elem[0] at end of Concat, which is LSB in Verilog).
        //
        // Without this reversal:
        //   TupleLiteral (rx, ry, rz) -> Concat [rx, ry, rz] (NO reversal via convert_expression)
        //   FieldAccess "0" (rx): adjusted_index=2, gets elements[2]=rz (WRONG!)
        //
        // With reversal:
        //   TupleLiteral (rx, ry, rz) -> Concat [rz, ry, rx] (after reversal)
        //   FieldAccess "0" (rx): adjusted_index=2, gets elements[2]=rx (CORRECT!)
        if let ExpressionKind::Concat(ref mut elements) = mir_body.kind {
            if elements.len() > 1 {
                debug_println!(
                    "🔄🔄🔄 BUG #189 FIX: Reversing Concat with {} elements in inline_function_call_with_mir_args 🔄🔄🔄",
                    elements.len()
                );
                elements.reverse();
            }
        }

        // Annotate with return type if available
        if let Some(ref ret_ty) = return_type {
            let return_type_ty = self.hir_type_to_type(ret_ty);
            mir_body = self.annotate_expression_with_type(mir_body, return_type_ty, 0);
        }

        Some(mir_body)
    }

    /// Helper to convert expression to lvalue (for index/range operations)
    fn expr_to_lvalue(&mut self, expr: &hir::HirExpression) -> Option<LValue> {
        match expr {
            hir::HirExpression::Signal(id) => self.signal_map.get(id).map(|&id| LValue::Signal(id)),
            hir::HirExpression::Port(id) => self.port_map.get(id).map(|&id| LValue::Port(id)),
            hir::HirExpression::Variable(id) => {
                // BUG #200 FIX: Check current_module_var_to_signal FIRST
                // This is critical for entity struct literal fields in module synthesis context
                // where Variables should resolve to Signals, not remain as Variables.
                if let Some(ref var_to_signal) = self.current_module_var_to_signal {
                    if let Some(&signal_id) = var_to_signal.get(id) {
                        println!(
                            "[BUG #200 FIX expr_to_lvalue] Variable {:?} -> Signal {:?}",
                            id, signal_id
                        );
                        return Some(LValue::Signal(signal_id));
                    }
                }

                // BUG #20 FIX: Check variable_map first, then fall back to dynamic_variables
                // Dynamic variables are created during function inlining (e.g., in match arm blocks)
                // and must be accessible for Index/Range operations.
                //
                // Example: In Karythra's exec_l0_l1, match arm 9 creates:
                //   let shift_amt = b[4:0];
                //   if sign && shift_amt > 0 { ... }
                // The condition references shift_amt, which is only in dynamic_variables.
                if let Some(&mir_id) = self.variable_map.get(id) {
                    return Some(LValue::Variable(mir_id));
                }

                // Fall back to dynamic_variables (for let-bound variables from inlined functions)
                if let Some((mir_id, _, _)) = self.dynamic_variables.get(id) {
                    return Some(LValue::Variable(*mir_id));
                }

                None
            }
            hir::HirExpression::Range(base, high, low) => {
                // BUG #77 FIX: Support nested range operations (e.g., data[31:0][4:0])
                let base_lval = self.expr_to_lvalue(base)?;
                let high_expr = self.convert_expression(high, 0)?;
                let low_expr = self.convert_expression(low, 0)?;
                Some(LValue::RangeSelect {
                    base: Box::new(base_lval),
                    high: Box::new(high_expr),
                    low: Box::new(low_expr),
                })
            }
            hir::HirExpression::Index(base, index) => {
                // BUG #77 FIX: Support nested index operations (e.g., arr[i][j])
                let base_lval = self.expr_to_lvalue(base)?;
                let index_expr = self.convert_expression(index, 0)?;
                Some(LValue::BitSelect {
                    base: Box::new(base_lval),
                    index: Box::new(index_expr),
                })
            }
            _ => None,
        }
    }

    /// Convert HIR binary op to MIR
    /// Infer HIR expression type
    fn infer_hir_type(&self, expr: &hir::HirExpression) -> Option<hir::HirType> {
        let hir = self.hir?;
        println!(
            "[INFER_TYPE_DEBUG] infer_hir_type called with expr type: {:?}",
            std::mem::discriminant(expr)
        );
        match expr {
            hir::HirExpression::Literal(lit) => match lit {
                hir::HirLiteral::Integer(_) => Some(hir::HirType::Bit(32)), // Default integer width
                hir::HirLiteral::Boolean(_) => Some(hir::HirType::Bool),
                hir::HirLiteral::Float(_) => Some(hir::HirType::Float32), // Default float type
                hir::HirLiteral::String(_) => None,
                hir::HirLiteral::BitVector(bits) => Some(hir::HirType::Bit(bits.len() as u32)),
            },
            hir::HirExpression::Signal(signal_id) => {
                // Look up signal in current entity's implementation
                let entity_id = self.current_entity_id?;
                let impl_block = hir.implementations.iter().find(|i| i.entity == entity_id)?;
                let signal = impl_block.signals.iter().find(|s| s.id == *signal_id)?;
                Some(signal.signal_type.clone())
            }
            hir::HirExpression::Port(port_id) => {
                // Look up port in current entity
                let entity_id = self.current_entity_id?;
                let entity = hir.entities.iter().find(|e| e.id == entity_id)?;
                let port = entity.ports.iter().find(|p| p.id == *port_id)?;
                println!(
                    "[TYPE_DEBUG] Port '{}' has type: {:?}",
                    port.name, port.port_type
                );
                Some(port.port_type.clone())
            }
            hir::HirExpression::Variable(var_id) => {
                // Look up variable in current entity's implementation
                if let Some(entity_id) = self.current_entity_id {
                    if let Some(impl_block) =
                        hir.implementations.iter().find(|i| i.entity == entity_id)
                    {
                        if let Some(var) = impl_block.variables.iter().find(|v| v.id == *var_id) {
                            println!(
                                "[INFER_TYPE_DEBUG] Variable {:?} found in impl_block: {:?}",
                                var_id, var.var_type
                            );
                            return Some(var.var_type.clone());
                        }
                    }
                }
                println!(
                    "[INFER_TYPE_DEBUG] Variable {:?} NOT found in impl_block, checking dynamic_variables (len={})",
                    var_id, self.dynamic_variables.len()
                );

                // CRITICAL FIX #IMPORT_MATCH: Check context_variable_map first when in ANY context
                // to avoid finding wrong variable type due to HIR VariableId collisions
                if let Some(context) = self.get_current_context() {
                    let context_key = (Some(context.clone()), *var_id);
                    if let Some(&mir_id) = self.context_variable_map.get(&context_key) {
                        // Found context-aware mapping - get type from mir_variable_types
                        if let Some(var_type) = self.mir_variable_types.get(&mir_id) {
                            eprintln!(
                                "[BUG #IMPORT_MATCH] infer_hir_type: Found context-aware type for HIR {:?} via MIR {:?}: {:?}",
                                var_id, mir_id, var_type
                            );
                            return Some(var_type.clone());
                        }
                    }
                }

                // Fall back to dynamic_variables (for let-bound variables from inlined functions)
                // But this will be wrong if there are collisions!
                if let Some((mir_id, _, var_type)) = self.dynamic_variables.get(var_id) {
                    // Also check mir_variable_types to see if there's a more accurate type
                    if let Some(accurate_type) = self.mir_variable_types.get(mir_id) {
                        eprintln!(
                            "[BUG #IMPORT_MATCH] infer_hir_type: Using mir_variable_types for HIR {:?} -> MIR {:?}: {:?}",
                            var_id, mir_id, accurate_type
                        );
                        return Some(accurate_type.clone());
                    }
                    println!(
                        "[INFER_TYPE_DEBUG] Variable {:?} found in dynamic_variables: {:?}",
                        var_id, var_type
                    );
                    return Some(var_type.clone());
                }

                println!(
                    "[INFER_TYPE_DEBUG] Variable {:?} NOT found anywhere, returning None",
                    var_id
                );
                None
            }
            hir::HirExpression::Constant(const_id) => {
                // Look up constant in current entity's implementation
                if let Some(entity_id) = self.current_entity_id {
                    if let Some(impl_block) =
                        hir.implementations.iter().find(|i| i.entity == entity_id)
                    {
                        if let Some(constant) =
                            impl_block.constants.iter().find(|c| c.id == *const_id)
                        {
                            return Some(constant.const_type.clone());
                        }
                    }
                }
                // Constants are typically integer types (nat, uint, etc.)
                // Default to Bit(32) for arithmetic if not found
                Some(hir::HirType::Bit(32))
            }
            hir::HirExpression::GenericParam(name) => {
                // WORKAROUND for Bug #42/43: Parser creates malformed cast expression AST,
                // causing some variable references to appear as GenericParam instead of Variable
                // Try to resolve the name to a variable (should not be needed after parser fix)

                // BUG FIX #7: First check dynamic_variables (from inlined functions)
                if let Some((_, _, var_type)) = self
                    .dynamic_variables
                    .values()
                    .find(|(_, var_name, _)| var_name == name)
                {
                    return Some(var_type.clone());
                }

                // Then check entity implementation variables
                let entity_id = self.current_entity_id?;
                let impl_block = hir.implementations.iter().find(|i| i.entity == entity_id)?;
                if let Some(var) = impl_block.variables.iter().find(|v| v.name == *name) {
                    return Some(var.var_type.clone());
                }
                None
            }
            hir::HirExpression::Binary(binary) => {
                // For binary expressions, infer from operands
                self.infer_hir_type(&binary.left)
            }
            hir::HirExpression::FieldAccess { base, field } => {
                // Infer from base expression and field
                let base_type = self.infer_hir_type(base)?;

                // BUG FIX #46: Handle Custom("vec2/3/4") which should be Vec2/3/4(Float32)
                // This is needed because Bug #45 causes HIR to store vec2<fp32> as Custom("vec2")
                if let hir::HirType::Custom(type_name) = &base_type {
                    if type_name.starts_with("vec")
                        && matches!(field.as_str(), "x" | "y" | "z" | "w")
                    {
                        // Vec components are Float32 by default (matching convert_type workaround)
                        return Some(hir::HirType::Float32);
                    }
                }

                // For vector types, return element type
                match base_type {
                    hir::HirType::Vec2(element_type)
                    | hir::HirType::Vec3(element_type)
                    | hir::HirType::Vec4(element_type) => {
                        // Vector component access returns element type
                        Some(*element_type)
                    }
                    hir::HirType::Struct(ref struct_type) => {
                        // Look up field type in struct definition
                        for struct_field in &struct_type.fields {
                            if struct_field.name == *field {
                                return Some(struct_field.field_type.clone());
                            }
                        }
                        None
                    }
                    _ => None,
                }
            }
            hir::HirExpression::Index(base, _) => {
                // For array indexing, infer element type
                let base_type = self.infer_hir_type(base)?;
                match base_type {
                    hir::HirType::Array(elem_type, _) => Some(*elem_type),
                    _ => Some(hir::HirType::Bit(1)), // Bit select
                }
            }
            hir::HirExpression::Range(base, high, low) => {
                // For range/bit slice, calculate the width from high and low indices
                // The result is Bit(high - low + 1)
                // Try to evaluate high and low as constants using the frontend's const_eval
                use skalp_frontend::const_eval::{ConstEvaluator, ConstValue};
                let mut evaluator = ConstEvaluator::new();

                // Register constants from current entity implementation
                if let Some(entity_id) = self.current_entity_id {
                    if let Some(hir) = self.hir {
                        for implementation in &hir.implementations {
                            if implementation.entity == entity_id {
                                evaluator.register_constants(&implementation.constants);
                            }
                        }
                    }
                }

                if let (Ok(high_val), Ok(low_val)) = (evaluator.eval(high), evaluator.eval(low)) {
                    if let (ConstValue::Nat(h), ConstValue::Nat(l)) = (high_val, low_val) {
                        let width = (h - l + 1) as u32;
                        return Some(hir::HirType::Bit(width));
                    }
                }

                // Can't determine width statically - fall back to base type
                // For bit vectors, a range still produces a bit type
                let base_type = self.infer_hir_type(base);
                match base_type {
                    Some(hir::HirType::Bit(_)) => Some(hir::HirType::Bit(32)), // Default width
                    Some(hir::HirType::Custom(_)) => Some(hir::HirType::Bit(32)),
                    _ => Some(hir::HirType::Bit(32)),
                }
            }
            hir::HirExpression::Cast(cast) => {
                // Cast expression: return the target type
                Some(cast.target_type.clone())
            }
            hir::HirExpression::Call(call) => {
                // Infer type from function return type
                if let Some(func) = self.find_function(&call.function) {
                    func.return_type.clone()
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    /// Find a trait method implementation for a given type, trait name, and method name
    ///
    /// This is used for trait-based operator resolution. When `a + b` is encountered
    /// and `a` has type `fp32`, this function finds the `add` method from `impl Add for fp32`.
    ///
    /// Returns the method implementation (body statements) if found, None otherwise.
    #[allow(dead_code)]
    fn find_trait_method_impl<'a>(
        &'a self,
        type_name: &str,
        trait_name: &str,
        method_name: &str,
    ) -> Option<&'a hir::HirTraitMethodImpl> {
        let trait_impl = self.find_trait_impl(type_name, trait_name)?;
        trait_impl
            .method_implementations
            .iter()
            .find(|m| m.name == method_name)
    }

    /// Find a trait definition by name
    fn find_trait_definition<'a>(
        &'a self,
        trait_name: &str,
    ) -> Option<&'a hir::HirTraitDefinition> {
        let hir = self.hir.as_ref()?;
        hir.trait_definitions.iter().find(|t| t.name == trait_name)
    }

    /// Find a trait method definition (parameters and return type) by trait and method name
    fn find_trait_method_definition<'a>(
        &'a self,
        trait_name: &str,
        method_name: &str,
    ) -> Option<&'a hir::HirTraitMethod> {
        let trait_def = self.find_trait_definition(trait_name)?;
        trait_def.methods.iter().find(|m| m.name == method_name)
    }

    /// Inline a trait method call by substituting parameters and converting the body
    ///
    /// This is the core of trait-based operator overloading. When `a + b` is encountered
    /// for a type with `impl Add`, this function:
    /// 1. Gets the method definition (parameters) from the trait
    /// 2. Gets the method implementation (body) from the impl
    /// 3. Substitutes the HIR parameters with the actual arguments
    /// 4. Converts the substituted body to MIR
    ///
    /// Returns the MIR expression representing the inlined method, or None if inlining fails.
    fn inline_trait_method(
        &mut self,
        type_name: &str,
        trait_name: &str,
        method_name: &str,
        left_expr: &hir::HirExpression,
        right_expr: &hir::HirExpression,
    ) -> Option<Expression> {
        println!(
            "[TRAIT_INLINE] Inlining {}::{} for type '{}'",
            trait_name, method_name, type_name
        );

        // Get the method definition (parameters) from the trait
        let method_def = self.find_trait_method_definition(trait_name, method_name);
        if method_def.is_none() {
            println!(
                "[TRAIT_INLINE] FAILED: Could not find method definition for {}::{}",
                trait_name, method_name
            );
            return None;
        }
        let method_def = method_def.unwrap();
        let params = method_def.parameters.clone();

        // Get the method implementation (body) from the impl
        let trait_impl = self.find_trait_impl(type_name, trait_name);
        if trait_impl.is_none() {
            println!(
                "[TRAIT_INLINE] FAILED: Could not find trait impl for {} on {}",
                trait_name, type_name
            );
            return None;
        }
        let trait_impl = trait_impl.unwrap();
        let method_impl = trait_impl
            .method_implementations
            .iter()
            .find(|m| m.name == method_name);
        if method_impl.is_none() {
            println!(
                "[TRAIT_INLINE] FAILED: Could not find method impl '{}' in trait impl",
                method_name
            );
            return None;
        }
        let method_impl = method_impl.unwrap();
        let body = method_impl.body.clone();

        if body.is_empty() {
            println!("[TRAIT_INLINE] FAILED: Method body is empty, cannot inline");
            return None;
        }

        println!(
            "[TRAIT_INLINE] Found {} parameters and {} body statements",
            params.len(),
            body.len()
        );

        // Build parameter substitution map
        // For binary operators: param[0] = left, param[1] = right
        let mut param_map: IndexMap<String, &hir::HirExpression> = IndexMap::new();
        if !params.is_empty() {
            param_map.insert(params[0].name.clone(), left_expr);
            println!(
                "[BUG #200 PARAM_MAP] param '{}' = {:?}",
                params[0].name,
                std::mem::discriminant(left_expr)
            );
        }
        if params.len() >= 2 {
            param_map.insert(params[1].name.clone(), right_expr);
            println!(
                "[BUG #200 PARAM_MAP] param '{}' = {:?}",
                params[1].name,
                std::mem::discriminant(right_expr)
            );
        }

        // Build var_id -> name mapping for let bindings in the body
        let mut var_id_to_name: IndexMap<hir::VariableId, String> = IndexMap::new();
        self.collect_let_bindings(&body, &mut var_id_to_name);

        // Convert body to expression (like inline_function_call does)
        let body_expr = self.convert_body_to_expression(&body);
        if body_expr.is_none() {
            println!("[TRAIT_INLINE] FAILED: convert_body_to_expression returned None");
            return None;
        }
        let body_expr = body_expr.unwrap();

        println!(
            "[TRAIT_INLINE] Body expression type: {:?}",
            std::mem::discriminant(&body_expr)
        );

        // Substitute parameters in the body expression
        let substituted =
            self.substitute_expression_with_var_map(&body_expr, &param_map, &var_id_to_name);
        if substituted.is_none() {
            println!("[TRAIT_INLINE] FAILED: substitute_expression_with_var_map returned None");
            return None;
        }
        let substituted = substituted.unwrap();

        println!(
            "[TRAIT_INLINE] Substituted expression type: {:?}",
            std::mem::discriminant(&substituted)
        );

        // BUG #196/BUG #197 FIX: Keep match_arm_prefix for entity instance naming
        // When a trait method is inlined from within a match arm, entity instances created
        // by the trait method (like FpAdd) MUST get the match_arm_prefix to have unique names.
        // Different match arms may all call the same trait method, creating entity instances
        // with the same name (e.g., "__adder_result"), but with different connections.
        //
        // Note: Local variables inside the trait method might get the prefix too, but that's
        // acceptable because they're scoped to the trait method's context anyway.
        eprintln!(
            "[BUG #196/197] Trait inline '{}::{}': keeping match_arm_prefix={:?}",
            trait_name, method_name, self.match_arm_prefix
        );

        // Convert the substituted expression to MIR
        // The Block expression handler will process let statements and entity instantiations
        let result = self.convert_expression(&substituted, 0);

        if result.is_none() {
            println!("[TRAIT_INLINE] FAILED: convert_expression returned None");
        }
        result
    }

    /// Inline a unary trait method call (e.g., Sqrt::sqrt(x))
    ///
    /// Similar to inline_trait_method but for single-argument trait methods like Sqrt, Abs, Neg.
    /// This enables explicit trait method calls like `Sqrt::sqrt(x)` in the source code.
    fn inline_trait_method_unary(
        &mut self,
        type_name: &str,
        trait_name: &str,
        method_name: &str,
        arg_expr: &hir::HirExpression,
    ) -> Option<Expression> {
        println!(
            "[TRAIT_INLINE_UNARY] Inlining {}::{} for type '{}'",
            trait_name, method_name, type_name
        );

        // Get the method definition (parameters) from the trait
        let method_def = self.find_trait_method_definition(trait_name, method_name);
        if method_def.is_none() {
            println!(
                "[TRAIT_INLINE_UNARY] FAILED: Could not find method definition for {}::{}",
                trait_name, method_name
            );
            return None;
        }
        let method_def = method_def.unwrap();
        let params = method_def.parameters.clone();

        // Get the method implementation (body) from the impl
        let trait_impl = self.find_trait_impl(type_name, trait_name);
        if trait_impl.is_none() {
            println!(
                "[TRAIT_INLINE_UNARY] FAILED: Could not find trait impl for {} on {}",
                trait_name, type_name
            );
            return None;
        }
        let trait_impl = trait_impl.unwrap();
        let method_impl = trait_impl
            .method_implementations
            .iter()
            .find(|m| m.name == method_name);
        if method_impl.is_none() {
            println!(
                "[TRAIT_INLINE_UNARY] FAILED: Could not find method impl '{}' in trait impl",
                method_name
            );
            return None;
        }
        let method_impl = method_impl.unwrap();
        let body = method_impl.body.clone();

        if body.is_empty() {
            println!("[TRAIT_INLINE_UNARY] FAILED: Method body is empty, cannot inline");
            return None;
        }

        println!(
            "[TRAIT_INLINE_UNARY] Found {} parameters and {} body statements",
            params.len(),
            body.len()
        );

        // Build parameter substitution map
        // For unary methods: param[0] = arg
        let mut param_map: IndexMap<String, &hir::HirExpression> = IndexMap::new();
        if !params.is_empty() {
            param_map.insert(params[0].name.clone(), arg_expr);
        }

        // Build var_id -> name mapping for let bindings in the body
        let mut var_id_to_name: IndexMap<hir::VariableId, String> = IndexMap::new();
        self.collect_let_bindings(&body, &mut var_id_to_name);

        // Convert body to expression (like inline_function_call does)
        let body_expr = self.convert_body_to_expression(&body);
        if body_expr.is_none() {
            println!("[TRAIT_INLINE_UNARY] FAILED: convert_body_to_expression returned None");
            return None;
        }
        let body_expr = body_expr.unwrap();

        println!(
            "[TRAIT_INLINE_UNARY] Body expression type: {:?}",
            std::mem::discriminant(&body_expr)
        );

        // Substitute parameters in the body expression
        let substituted =
            self.substitute_expression_with_var_map(&body_expr, &param_map, &var_id_to_name);
        if substituted.is_none() {
            println!(
                "[TRAIT_INLINE_UNARY] FAILED: substitute_expression_with_var_map returned None"
            );
            return None;
        }
        let substituted = substituted.unwrap();

        println!(
            "[TRAIT_INLINE_UNARY] Substituted expression type: {:?}",
            std::mem::discriminant(&substituted)
        );

        // BUG #196/BUG #197 FIX: Keep match_arm_prefix for entity instance naming
        // (Same fix as for binary trait methods - see inline_trait_method comments)
        eprintln!(
            "[BUG #196/197] Unary trait inline '{}::{}': keeping match_arm_prefix={:?}",
            trait_name, method_name, self.match_arm_prefix
        );

        // Convert the substituted expression to MIR
        let result = self.convert_expression(&substituted, 0);

        if result.is_none() {
            println!("[TRAIT_INLINE_UNARY] FAILED: convert_expression returned None");
        }
        result
    }

    /// Find a trait implementation for a given type and trait name
    ///
    /// This is used for trait-based operator resolution. When `a + b` is encountered
    /// and `a` has type `fp32`, this function checks if there's an `impl Add for fp32`
    /// which would allow resolving the `+` operator to `Add::add(a, b)`.
    ///
    /// Returns the trait implementation if found, None otherwise.
    fn find_trait_impl<'a>(
        &'a self,
        type_name: &str,
        trait_name: &str,
    ) -> Option<&'a hir::HirTraitImplementation> {
        let hir = self.hir.as_ref()?;

        println!(
            "[FIND_TRAIT_IMPL] Looking for trait '{}' on type '{}', HIR has {} trait_impls",
            trait_name,
            type_name,
            hir.trait_implementations.len()
        );

        // Look through all trait implementations for a match
        let result = hir.trait_implementations.iter().find(|impl_| {
            // Check if the trait matches
            if impl_.trait_name != trait_name {
                return false;
            }

            // Check if the target type matches
            match &impl_.target {
                hir::TraitImplTarget::Type(ty) => {
                    // Check if the type matches by name
                    // Handle both Custom types and primitive float types
                    // (float types may be used due to fallback resolution in hir_builder)
                    let impl_type_name = match ty {
                        hir::HirType::Custom(name) => Some(name.as_str()),
                        hir::HirType::Float32 => Some("fp32"),
                        hir::HirType::Float64 => Some("fp64"),
                        hir::HirType::Float16 => Some("fp16"),
                        _ => None,
                    };

                    if let Some(impl_type) = impl_type_name {
                        let matches = impl_type == type_name;
                        if matches {
                            println!(
                                "[FIND_TRAIT_IMPL] FOUND: impl {} for {} matches!",
                                trait_name, type_name
                            );
                        }
                        matches
                    } else {
                        false
                    }
                }
                hir::TraitImplTarget::Entity(_) => false,
            }
        });
        if result.is_none() {
            println!(
                "[FIND_TRAIT_IMPL] NOT FOUND: no impl {} for {}",
                trait_name, type_name
            );
        }
        result
    }

    /// Get the trait name for a binary operator
    ///
    /// Maps HIR binary operators to their corresponding trait names:
    /// - Add -> "Add"
    /// - Sub -> "Sub"
    /// - Mul -> "Mul"
    /// - Div -> "Div"
    /// - Less -> "PartialOrd"
    /// - etc.
    #[allow(dead_code)]
    fn operator_to_trait_name(op: &hir::HirBinaryOp) -> &'static str {
        match op {
            hir::HirBinaryOp::Add => "Add",
            hir::HirBinaryOp::Sub => "Sub",
            hir::HirBinaryOp::Mul => "Mul",
            hir::HirBinaryOp::Div => "Div",
            hir::HirBinaryOp::Mod => "Rem",
            hir::HirBinaryOp::Less
            | hir::HirBinaryOp::LessEqual
            | hir::HirBinaryOp::Greater
            | hir::HirBinaryOp::GreaterEqual => "PartialOrd",
            hir::HirBinaryOp::Equal | hir::HirBinaryOp::NotEqual => "PartialEq",
            hir::HirBinaryOp::And => "BitAnd",
            hir::HirBinaryOp::Or => "BitOr",
            hir::HirBinaryOp::Xor => "BitXor",
            hir::HirBinaryOp::LeftShift => "Shl",
            hir::HirBinaryOp::RightShift => "Shr",
            hir::HirBinaryOp::LogicalAnd | hir::HirBinaryOp::LogicalOr => "bool", // Special case
        }
    }

    /// Get the method name for a binary operator
    ///
    /// Maps HIR binary operators to their corresponding trait method names:
    /// - Add -> "add"
    /// - Sub -> "sub"
    /// - Less -> "lt"
    /// - etc.
    #[allow(dead_code)]
    fn operator_to_method_name(op: &hir::HirBinaryOp) -> &'static str {
        match op {
            hir::HirBinaryOp::Add => "add",
            hir::HirBinaryOp::Sub => "sub",
            hir::HirBinaryOp::Mul => "mul",
            hir::HirBinaryOp::Div => "div",
            hir::HirBinaryOp::Mod => "rem",
            hir::HirBinaryOp::Less => "lt",
            hir::HirBinaryOp::LessEqual => "le",
            hir::HirBinaryOp::Greater => "gt",
            hir::HirBinaryOp::GreaterEqual => "ge",
            hir::HirBinaryOp::Equal => "eq",
            hir::HirBinaryOp::NotEqual => "ne",
            hir::HirBinaryOp::And => "bitand",
            hir::HirBinaryOp::Or => "bitor",
            hir::HirBinaryOp::Xor => "bitxor",
            hir::HirBinaryOp::LeftShift => "shl",
            hir::HirBinaryOp::RightShift => "shr",
            hir::HirBinaryOp::LogicalAnd => "and",
            hir::HirBinaryOp::LogicalOr => "or",
        }
    }

    /// Try to resolve a binary operator through trait implementations
    ///
    /// This enables operator overloading through traits. When `a + b` is encountered:
    /// 1. Infer the type of `a`
    /// 2. Look for `impl Add for <type>`
    /// 3. If found, return the trait and method names for generating a function call
    ///
    /// Returns Some((type_name, trait_name, method_name)) if trait resolution succeeds,
    /// None if we should fall back to primitive operator handling.
    fn try_resolve_trait_operator(
        &self,
        op: &hir::HirBinaryOp,
        left_expr: &hir::HirExpression,
    ) -> Option<(String, &'static str, &'static str)> {
        // Get the type of the left operand
        let left_type = self.infer_hir_type(left_expr);
        println!(
            "[TRAIT_OP_DEBUG] try_resolve_trait_operator: op={:?}, left_type={:?}",
            op, left_type
        );
        let left_type = left_type?;

        // Extract the type name for Custom types (distinct types, user-defined types)
        // Also map built-in Float types to their stdlib type names for trait lookup
        let type_name = match &left_type {
            hir::HirType::Custom(name) => {
                println!("[TRAIT_OP_DEBUG] Detected Custom type: {}", name);
                name.clone()
            }
            // Map built-in float types to stdlib distinct type names
            // This allows trait resolution to find impl Add for fp32, etc.
            hir::HirType::Float16 => {
                println!("[TRAIT_OP_DEBUG] Float16 -> mapping to 'fp16' for trait lookup");
                "fp16".to_string()
            }
            hir::HirType::Float32 => {
                println!("[TRAIT_OP_DEBUG] Float32 -> mapping to 'fp32' for trait lookup");
                "fp32".to_string()
            }
            hir::HirType::Float64 => {
                println!("[TRAIT_OP_DEBUG] Float64 -> mapping to 'fp64' for trait lookup");
                "fp64".to_string()
            }
            // Other built-in types use primitive operators
            _ => {
                println!(
                    "[TRAIT_OP_DEBUG] Not a Custom/Float type, skipping trait resolution for {:?}",
                    left_type
                );
                return None;
            }
        };

        // Float types now use trait-based resolution exclusively
        // The distinct fp16/fp32/fp64/bf16 types have trait implementations in stdlib

        // Get the trait and method names for this operator
        let trait_name = Self::operator_to_trait_name(op);
        let method_name = Self::operator_to_method_name(op);

        // Skip logical operators - they don't have trait impls
        if trait_name == "bool" {
            return None;
        }

        // Check if there's a trait implementation for this type
        if self.find_trait_impl(&type_name, trait_name).is_some() {
            println!(
                "[TRAIT_OP] Resolved {:?} on {} to {}::{}",
                op, type_name, trait_name, method_name
            );
            Some((type_name, trait_name, method_name))
        } else {
            // No trait impl found - fall back to primitive handling
            None
        }
    }

    fn convert_binary_op(
        &self,
        op: &hir::HirBinaryOp,
        _left_expr: &hir::HirExpression,
    ) -> BinaryOp {
        // FP operations are handled via trait-based resolution before reaching here.
        // This function only handles integer/bitwise operations.
        match op {
            hir::HirBinaryOp::Add => BinaryOp::Add,
            hir::HirBinaryOp::Sub => BinaryOp::Sub,
            hir::HirBinaryOp::Mul => BinaryOp::Mul,
            hir::HirBinaryOp::Div => BinaryOp::Div,
            hir::HirBinaryOp::Mod => BinaryOp::Mod,
            hir::HirBinaryOp::And => BinaryOp::BitwiseAnd,
            hir::HirBinaryOp::Or => BinaryOp::BitwiseOr,
            hir::HirBinaryOp::Xor => BinaryOp::BitwiseXor,
            hir::HirBinaryOp::Equal => BinaryOp::Equal,
            hir::HirBinaryOp::NotEqual => BinaryOp::NotEqual,
            hir::HirBinaryOp::Less => BinaryOp::Less,
            hir::HirBinaryOp::LessEqual => BinaryOp::LessEqual,
            hir::HirBinaryOp::Greater => BinaryOp::Greater,
            hir::HirBinaryOp::GreaterEqual => BinaryOp::GreaterEqual,
            hir::HirBinaryOp::LogicalAnd => BinaryOp::LogicalAnd,
            hir::HirBinaryOp::LogicalOr => BinaryOp::LogicalOr,
            hir::HirBinaryOp::LeftShift => BinaryOp::LeftShift,
            hir::HirBinaryOp::RightShift => BinaryOp::RightShift,
        }
    }

    /// Convert HIR unary op to MIR
    fn convert_unary_op(&self, op: &hir::HirUnaryOp) -> UnaryOp {
        match op {
            hir::HirUnaryOp::Not => UnaryOp::Not,
            hir::HirUnaryOp::BitwiseNot => UnaryOp::BitwiseNot,
            hir::HirUnaryOp::Negate => UnaryOp::Negate,
            hir::HirUnaryOp::AndReduce => UnaryOp::Reduce(ReduceOp::And),
            hir::HirUnaryOp::OrReduce => UnaryOp::Reduce(ReduceOp::Or),
            hir::HirUnaryOp::XorReduce => UnaryOp::Reduce(ReduceOp::Xor),
        }
    }

    /// Convert HIR type to MIR data type
    fn convert_type(&mut self, hir_type: &hir::HirType) -> DataType {
        match hir_type {
            hir::HirType::Bit(width) => DataType::Bit(*width as usize),
            hir::HirType::Bool => DataType::Bool,
            hir::HirType::String => {
                panic!("String type is not synthesizable and cannot be used in hardware modules. String types are only supported in testbench contexts.");
            }
            hir::HirType::Logic(width) => DataType::Logic(*width as usize),
            hir::HirType::Int(width) => DataType::Int(*width as usize),
            hir::HirType::Nat(width) => DataType::Nat(*width as usize),
            hir::HirType::Clock(domain) => DataType::Clock {
                domain: domain.map(|id| ClockDomainId(id.0)),
            },
            hir::HirType::Reset {
                polarity,
                clock_domain,
            } => {
                // Convert HIR polarity to MIR active_high boolean
                let active_high = matches!(polarity, hir::HirResetPolarity::ActiveHigh);
                DataType::Reset {
                    active_high,
                    domain: clock_domain.map(|id| ClockDomainId(id.0)),
                }
            }
            hir::HirType::Event => DataType::Event,
            hir::HirType::Stream(inner_type) => {
                // Stream types include implicit handshaking signals
                // For now, convert to the inner type
                // TODO: Add proper stream protocol support in MIR
                self.convert_type(inner_type)
            }
            hir::HirType::Array(inner_type, size) => {
                DataType::Array(Box::new(self.convert_type(inner_type)), *size as usize)
            }
            hir::HirType::Custom(name) => {
                // BUG FIX #45: HIR builder stores vec2<fp32> as Custom("vec2") instead of Vec2(Float32)
                // Recognize built-in vector types before looking up user types
                if name.starts_with("vec") {
                    // Parse "vec2", "vec3", "vec4"
                    // TODO: This loses element type information (e.g., <fp32>)
                    // For now, default to fp32 which is the most common case
                    if let Some(dim_char) = name.chars().nth(3) {
                        if let Some(dimension) = dim_char.to_digit(10) {
                            match dimension {
                                2 => {
                                    eprintln!(
                                        "[BUG #71 CONVERT_TYPE] Custom(\"{}\") -> Vec2(Float32)",
                                        name
                                    );
                                    return DataType::Vec2(Box::new(DataType::Float32));
                                }
                                3 => {
                                    eprintln!(
                                        "[BUG #71 CONVERT_TYPE] Custom(\"{}\") -> Vec3(Float32)",
                                        name
                                    );
                                    return DataType::Vec3(Box::new(DataType::Float32));
                                }
                                4 => {
                                    eprintln!(
                                        "[BUG #71 CONVERT_TYPE] Custom(\"{}\") -> Vec4(Float32)",
                                        name
                                    );
                                    return DataType::Vec4(Box::new(DataType::Float32));
                                }
                                _ => {}
                            }
                        }
                    }
                }

                // Resolve custom types by looking them up in HIR
                if let Some(hir) = self.hir {
                    // First check user-defined types (structs, enums, unions)
                    for user_type in &hir.user_defined_types {
                        if user_type.name == *name {
                            // Found the user-defined type - convert its type_def
                            return self.convert_type(&user_type.type_def);
                        }
                    }

                    // Then check type aliases
                    for type_alias in &hir.type_aliases {
                        if type_alias.name == *name {
                            // Found the type alias - recursively convert its target type
                            // This handles nested type aliases correctly
                            return self.convert_type(&type_alias.target_type);
                        }
                    }

                    // Check distinct types (newtype pattern like fp32 = bit[32])
                    for distinct_type in &hir.distinct_types {
                        if distinct_type.name == *name {
                            // Found the distinct type - convert its base type
                            return self.convert_type(&distinct_type.base_type);
                        }
                    }
                }
                // If type not found, default to Bit(1) as fallback
                // This maintains backward compatibility for unresolved types
                DataType::Bit(1)
            }
            hir::HirType::Struct(struct_type) => {
                DataType::Struct(Box::new(self.convert_struct_type(struct_type)))
            }
            hir::HirType::Enum(enum_type) => {
                DataType::Enum(Box::new(self.convert_enum_type(enum_type)))
            }
            hir::HirType::Union(union_type) => {
                DataType::Union(Box::new(self.convert_union_type(union_type)))
            }
            // Floating-point types
            hir::HirType::Float16 => DataType::Float16,
            hir::HirType::Float32 => DataType::Float32,
            hir::HirType::Float64 => DataType::Float64,
            // Vector types
            hir::HirType::Vec2(element_type) => {
                DataType::Vec2(Box::new(self.convert_type(element_type)))
            }
            hir::HirType::Vec3(element_type) => {
                DataType::Vec3(Box::new(self.convert_type(element_type)))
            }
            hir::HirType::Vec4(element_type) => {
                DataType::Vec4(Box::new(self.convert_type(element_type)))
            }
            // Parametric types - preserve parameter name and default
            hir::HirType::BitParam(param_name) => DataType::BitParam {
                param: param_name.clone(),
                default: 8,
            },
            hir::HirType::LogicParam(param_name) => DataType::LogicParam {
                param: param_name.clone(),
                default: 8,
            },
            hir::HirType::IntParam(param_name) => DataType::IntParam {
                param: param_name.clone(),
                default: 32,
            },
            hir::HirType::NatParam(param_name) => DataType::NatParam {
                param: param_name.clone(),
                default: 32,
            },
            // Expression-based types - need const evaluation
            hir::HirType::BitExpr(expr) => {
                // Try to evaluate const expression to get concrete width
                match self.try_eval_const_expr(expr) {
                    Some(val) => DataType::Bit(val as usize),
                    None => {
                        // Can't evaluate - convert to MIR expression for codegen
                        DataType::BitExpr {
                            expr: Box::new(self.convert_const_expr_to_mir(expr)),
                            default: 8,
                        }
                    }
                }
            }
            hir::HirType::LogicExpr(expr) => match self.try_eval_const_expr(expr) {
                Some(val) => DataType::Logic(val as usize),
                None => DataType::LogicExpr {
                    expr: Box::new(self.convert_const_expr_to_mir(expr)),
                    default: 8,
                },
            },
            hir::HirType::IntExpr(expr) => match self.try_eval_const_expr(expr) {
                Some(val) => DataType::Int(val as usize),
                None => DataType::IntExpr {
                    expr: Box::new(self.convert_const_expr_to_mir(expr)),
                    default: 32,
                },
            },
            hir::HirType::NatExpr(expr) => match self.try_eval_const_expr(expr) {
                Some(val) => DataType::Nat(val as usize),
                None => DataType::NatExpr {
                    expr: Box::new(self.convert_const_expr_to_mir(expr)),
                    default: 32,
                },
            },
            hir::HirType::ArrayExpr(inner_type, size_expr) => {
                // Evaluate size expression
                match self.try_eval_const_expr(size_expr) {
                    Some(val) => {
                        DataType::Array(Box::new(self.convert_type(inner_type)), val as usize)
                    }
                    None => DataType::Array(Box::new(self.convert_type(inner_type)), 1), // Fallback
                }
            }

            // Tuple types - convert to anonymous packed structs
            hir::HirType::Tuple(element_types) => {
                // Convert (bit[32], bit[8]) to struct __tuple_2 { _0: bit[32], _1: bit[8] }
                let fields: Vec<StructField> = element_types
                    .iter()
                    .enumerate()
                    .map(|(i, ty)| StructField {
                        name: format!("_{}", i),
                        field_type: self.convert_type(ty),
                    })
                    .collect();

                DataType::Struct(Box::new(StructType {
                    name: format!("__tuple_{}", fields.len()),
                    fields,
                    packed: true,
                }))
            }

            // Parametric numeric types - These will be monomorphized later
            hir::HirType::FpParametric { format } => {
                // TODO: Evaluate format expression to get concrete FloatFormat
                // For now, default to Float32
                match self.try_eval_const_expr(format) {
                    Some(_) => DataType::Float32, // Placeholder
                    None => DataType::Float32,
                }
            }
            hir::HirType::FixedParametric {
                width,
                frac,
                signed: _,
            } => {
                // TODO: Evaluate width and frac expressions
                // For now, map to Bit type with evaluated width
                match (
                    self.try_eval_const_expr(width),
                    self.try_eval_const_expr(frac),
                ) {
                    (Some(w), Some(_f)) => DataType::Bit(w as usize),
                    _ => DataType::Bit(32), // Fallback
                }
            }
            hir::HirType::IntParametric { width, signed } => {
                // Evaluate width and signed expressions
                match (
                    self.try_eval_const_expr(width),
                    self.try_eval_const_expr(signed),
                ) {
                    (Some(w), Some(s)) if s != 0 => DataType::Int(w as usize),
                    (Some(w), _) => DataType::Nat(w as usize),
                    _ => DataType::Int(32), // Fallback
                }
            }
            hir::HirType::VecParametric {
                element_type,
                dimension,
            } => {
                // Evaluate dimension expression
                let inner = self.convert_type(element_type);
                match self.try_eval_const_expr(dimension) {
                    Some(2) => DataType::Vec2(Box::new(inner)),
                    Some(3) => DataType::Vec3(Box::new(inner)),
                    Some(4) => DataType::Vec4(Box::new(inner)),
                    Some(n) => DataType::Array(Box::new(inner), n as usize),
                    None => DataType::Vec3(Box::new(inner)), // Fallback
                }
            }
            // NCL dual-rail types (Null Convention Logic)
            // Physical width is 2 * logical width (dual-rail encoding)
            hir::HirType::Ncl(width) => DataType::Ncl(*width as usize),
            hir::HirType::NclParam(_param_name) => {
                // Parametric NCL - use default (resolution happens during monomorphization)
                DataType::Ncl(32) // Default: 32 logical bits
            }
            hir::HirType::NclExpr(expr) => {
                // Evaluate const expression for NCL width
                if let Some(width) = self.try_eval_const_expr(expr) {
                    DataType::Ncl(width as usize)
                } else {
                    DataType::Ncl(32) // Fallback: 32 logical bits
                }
            }
        }
    }

    /// Infer the MIR DataType of a MIR Expression
    /// Infer expression type with access to module for variable type lookup
    /// This version is used when we have access to the partially-built module
    fn infer_expression_type_with_module(&self, expr: &Expression, module: &Module) -> DataType {
        self.infer_expression_type_internal(expr, Some(module))
    }

    /// Infer expression type without module access (fallback for legacy code)
    /// This is used for variables created from pending statements where we don't have HIR type info
    #[allow(clippy::only_used_in_recursion)]
    fn infer_expression_type(&self, expr: &Expression) -> DataType {
        self.infer_expression_type_internal(expr, None)
    }

    /// Internal implementation of expression type inference
    /// module_opt: Optional module for looking up variable types
    #[allow(clippy::only_used_in_recursion)]
    fn infer_expression_type_internal(
        &self,
        expr: &Expression,
        module_opt: Option<&Module>,
    ) -> DataType {
        match &expr.kind {
            ExpressionKind::Literal(value) => match value {
                Value::Integer(_) => DataType::Int(32),
                Value::Float(_) => DataType::Float32,
                Value::BitVector { width, .. } => DataType::Bit(*width),
                Value::String(_) => DataType::Nat(32), // Fallback
                Value::HighZ => DataType::Logic(1),
                Value::Unknown => DataType::Logic(1), // Unknown value
            },
            ExpressionKind::Ref(lval) => {
                // Look up the signal/port/variable type
                match lval {
                    LValue::Signal(sig_id) => {
                        // TODO: Look up signal type from mir.signals
                        // For now, default to 32-bit natural
                        DataType::Nat(32)
                    }
                    LValue::Port(port_id) => {
                        // TODO: Look up port type
                        DataType::Nat(32)
                    }
                    LValue::Variable(var_id) => {
                        // BUG #65/#66 FIX: Look up variable type from module if available
                        if let Some(module) = module_opt {
                            module
                                .variables
                                .iter()
                                .find(|v| v.id == *var_id)
                                .map(|v| v.var_type.clone())
                                .unwrap_or(DataType::Nat(32))
                        } else {
                            // Fallback when no module available
                            DataType::Nat(32)
                        }
                    }
                    LValue::BitSelect { .. } => DataType::Bit(1),
                    LValue::RangeSelect { high, low, .. } => {
                        // Width is (high - low + 1) - but these are expressions, so estimate
                        DataType::Nat(32)
                    }
                    LValue::Concat(_) => DataType::Nat(32),
                }
            }
            ExpressionKind::Binary { left, .. } => {
                // Infer type from left operand (binary ops preserve operand type)
                self.infer_expression_type_internal(left, module_opt)
            }
            ExpressionKind::Unary { .. } => DataType::Nat(32),
            ExpressionKind::Conditional { then_expr, .. } => {
                // Infer from then branch
                self.infer_expression_type_internal(then_expr, module_opt)
            }
            ExpressionKind::FunctionCall { .. } => DataType::Nat(32), // Default for function calls
            // BUG #125 FIX: Concat width is sum of element widths, not fixed 32-bit
            ExpressionKind::Concat(parts) => {
                use skalp_frontend::types::Width;
                let total_width: usize = parts
                    .iter()
                    .map(|p| {
                        // First try the expression's own type (Type uses Width enum)
                        let part_width = match &p.ty {
                            Type::Bit(Width::Fixed(w)) => Some(*w as usize),
                            Type::Nat(Width::Fixed(w)) | Type::Int(Width::Fixed(w)) => {
                                Some(*w as usize)
                            }
                            Type::Unknown => None,
                            _ => None,
                        };
                        // If type is Unknown or not Fixed, recursively infer
                        part_width.unwrap_or_else(|| {
                            match self.infer_expression_type_internal(p, module_opt) {
                                DataType::Bit(w) => w,
                                DataType::Nat(w) => w,
                                DataType::Int(w) => w,
                                _ => 32, // fallback
                            }
                        })
                    })
                    .sum();
                DataType::Bit(total_width)
            }
            ExpressionKind::Replicate { .. } => DataType::Nat(32),
            ExpressionKind::Cast { target_type, .. } => target_type.clone(),
            // BUG FIX #85: Handle tuple/field access
            ExpressionKind::TupleFieldAccess { base, index } => {
                // For tuple element access, try to infer element type
                // For now, assume 32-bit elements
                DataType::Nat(32)
            }
            ExpressionKind::FieldAccess { base, .. } => {
                // For struct field access, fall back to base type inference
                self.infer_expression_type_internal(base, module_opt)
            }
        }
    }

    /// Try to evaluate a const expression at compile time
    /// Returns Some(value) if the expression can be evaluated, None otherwise
    #[allow(clippy::only_used_in_recursion)]
    fn try_eval_const_expr(&mut self, expr: &hir::HirExpression) -> Option<u64> {
        // Try using the const evaluator first (handles both built-in and user-defined functions)
        if let Ok(const_val) = self.const_evaluator.eval(expr) {
            return const_val.as_nat().map(|n| n as u64);
        }

        // Fallback to manual evaluation for expressions the evaluator can't handle
        match expr {
            hir::HirExpression::Literal(hir::HirLiteral::Integer(val)) => Some(*val),
            hir::HirExpression::Binary(bin_expr) => {
                // Recursively evaluate binary expressions
                let left = self.try_eval_const_expr(&bin_expr.left)?;
                let right = self.try_eval_const_expr(&bin_expr.right)?;

                match &bin_expr.op {
                    hir::HirBinaryOp::Add => Some(left + right),
                    hir::HirBinaryOp::Sub => Some(left.saturating_sub(right)),
                    hir::HirBinaryOp::Mul => Some(left * right),
                    hir::HirBinaryOp::Div if right != 0 => Some(left / right),
                    hir::HirBinaryOp::Mod if right != 0 => Some(left % right),
                    _ => None, // Can't evaluate other operations
                }
            }
            hir::HirExpression::Call(call_expr) => {
                // Handle intrinsic functions (for backward compatibility)
                match call_expr.function.as_str() {
                    "clog2" => {
                        // Calculate ceiling log2
                        if call_expr.args.len() == 1 {
                            let arg = self.try_eval_const_expr(&call_expr.args[0])?;
                            if arg == 0 {
                                return Some(0);
                            }
                            Some((64 - (arg - 1).leading_zeros()) as u64)
                        } else {
                            None
                        }
                    }
                    "pow2" => {
                        // Calculate 2^n
                        if call_expr.args.len() == 1 {
                            let arg = self.try_eval_const_expr(&call_expr.args[0])?;
                            if arg < 64 {
                                Some(1u64 << arg)
                            } else {
                                None // Overflow
                            }
                        } else {
                            None
                        }
                    }
                    _ => None, // Unknown function - const evaluator already tried
                }
            }
            hir::HirExpression::GenericParam(param_name) => {
                // Look up generic parameter value from current entity
                if let Some(hir) = self.hir {
                    if let Some(entity_id) = self.current_entity_id {
                        for entity in &hir.entities {
                            if entity.id == entity_id {
                                for generic in &entity.generics {
                                    if generic.name == *param_name {
                                        if let Some(default_expr) = &generic.default_value {
                                            return self.try_eval_const_expr(default_expr);
                                        }
                                    }
                                }
                                break;
                            }
                        }
                    }
                }
                None
            }
            _ => None, // Can't evaluate variables, constants, etc. without context
        }
    }

    /// Convert a HIR expression to a MIR expression for use in type positions
    /// This is specifically for const expressions in types (like clog2(SIZE) in nat[clog2(SIZE)])
    #[allow(clippy::only_used_in_recursion)]
    fn convert_const_expr_to_mir(&self, expr: &hir::HirExpression) -> Expression {
        match expr {
            hir::HirExpression::Literal(hir::HirLiteral::Integer(val)) => {
                Expression::with_unknown_type(ExpressionKind::Literal(Value::Integer(*val as i64)))
            }
            hir::HirExpression::Binary(bin_expr) => {
                let left = Box::new(self.convert_const_expr_to_mir(&bin_expr.left));
                let right = Box::new(self.convert_const_expr_to_mir(&bin_expr.right));
                let op = match bin_expr.op {
                    hir::HirBinaryOp::Add => BinaryOp::Add,
                    hir::HirBinaryOp::Sub => BinaryOp::Sub,
                    hir::HirBinaryOp::Mul => BinaryOp::Mul,
                    hir::HirBinaryOp::Div => BinaryOp::Div,
                    hir::HirBinaryOp::Mod => BinaryOp::Mod,
                    _ => {
                        // For unsupported operators, fall back to a literal 0
                        return Expression::with_unknown_type(ExpressionKind::Literal(
                            Value::Integer(0),
                        ));
                    }
                };
                Expression::with_unknown_type(ExpressionKind::Binary { op, left, right })
            }
            hir::HirExpression::Call(call_expr) => {
                // Convert function call (like clog2(SIZE))
                let args = call_expr
                    .args
                    .iter()
                    .map(|arg| self.convert_const_expr_to_mir(arg))
                    .collect();
                Expression::with_unknown_type(ExpressionKind::FunctionCall {
                    name: call_expr.function.clone(),
                    args,
                })
            }
            hir::HirExpression::GenericParam(param_name) => {
                // Reference to a generic parameter (like SIZE)
                // We can't resolve this yet, so we keep it as a function call-like reference
                // The codegen will handle emitting the parameter name
                Expression::with_unknown_type(ExpressionKind::FunctionCall {
                    name: param_name.clone(),
                    args: vec![],
                })
            }
            _ => {
                // For other expression types, fall back to literal 0
                // This shouldn't normally happen for const type expressions
                Expression::with_unknown_type(ExpressionKind::Literal(Value::Integer(0)))
            }
        }
    }

    /// Convert HIR port direction
    fn convert_port_direction(&self, dir: &hir::HirPortDirection) -> PortDirection {
        match dir {
            hir::HirPortDirection::Input => PortDirection::Input,
            hir::HirPortDirection::Output => PortDirection::Output,
            hir::HirPortDirection::Bidirectional => PortDirection::InOut,
            hir::HirPortDirection::Protocol => PortDirection::InOut, // Protocols are bidirectional by nature
        }
    }

    /// Convert HIR edge type
    fn convert_edge_type(&self, edge: &hir::HirEdgeType) -> EdgeType {
        match edge {
            hir::HirEdgeType::Rising => EdgeType::Rising,
            hir::HirEdgeType::Falling => EdgeType::Falling,
            hir::HirEdgeType::Both => EdgeType::Both,
            hir::HirEdgeType::Active => EdgeType::Active,
            hir::HirEdgeType::Inactive => EdgeType::Inactive,
        }
    }

    // ID generation methods
    /// Convert HIR generic type to MIR generic parameter type
    fn convert_generic_type(&mut self, hir_type: &hir::HirGenericType) -> GenericParameterType {
        match hir_type {
            hir::HirGenericType::Type => GenericParameterType::Type,
            // Phase 2: TypeWithBounds is treated as Type in MIR
            // Trait bounds are checked during type checking, not needed in MIR
            hir::HirGenericType::TypeWithBounds(_bounds) => GenericParameterType::Type,
            hir::HirGenericType::Const(hir_data_type) => {
                GenericParameterType::Const(self.convert_type(hir_data_type))
            }
            hir::HirGenericType::Width => GenericParameterType::Width,
            hir::HirGenericType::ClockDomain => GenericParameterType::ClockDomain,
            hir::HirGenericType::PowerDomain { .. } => GenericParameterType::PowerDomain,
            hir::HirGenericType::Intent => GenericParameterType::Intent,
        }
    }

    /// Convert HIR struct type to MIR struct type
    fn convert_struct_type(&mut self, hir_struct: &hir::HirStructType) -> StructType {
        let fields = hir_struct
            .fields
            .iter()
            .map(|field| StructField {
                name: field.name.clone(),
                field_type: self.convert_type(&field.field_type),
            })
            .collect();

        StructType {
            name: hir_struct.name.clone(),
            fields,
            packed: hir_struct.packed,
        }
    }

    /// Convert HIR enum type to MIR enum type
    fn convert_enum_type(&mut self, hir_enum: &hir::HirEnumType) -> EnumType {
        let variants = hir_enum
            .variants
            .iter()
            .map(|variant| EnumVariant {
                name: variant.name.clone(),
                value: variant
                    .value
                    .as_ref()
                    .and_then(|expr| self.convert_literal_expr(expr)),
            })
            .collect();

        EnumType {
            name: hir_enum.name.clone(),
            variants,
            base_type: self.convert_type(&hir_enum.base_type),
        }
    }

    /// Convert HIR union type to MIR union type
    fn convert_union_type(&mut self, hir_union: &hir::HirUnionType) -> UnionType {
        let fields = hir_union
            .fields
            .iter()
            .map(|field| StructField {
                name: field.name.clone(),
                field_type: self.convert_type(&field.field_type),
            })
            .collect();

        UnionType {
            name: hir_union.name.clone(),
            fields,
            packed: hir_union.packed,
        }
    }

    /// Convert MIR DataType back to HIR HirType
    /// BUG FIX #67: Needed to store inferred types in dynamic_variables
    ///
    /// Note: &self is only used in recursive calls, which is intentional for this converter
    #[allow(clippy::only_used_in_recursion)]
    fn convert_mir_to_hir_type(&self, mir_type: &DataType) -> hir::HirType {
        match mir_type {
            DataType::Bit(width) => hir::HirType::Bit(*width as u32),
            DataType::Bool => hir::HirType::Bool,
            DataType::Logic(width) => hir::HirType::Logic(*width as u32),
            DataType::Int(width) => hir::HirType::Int(*width as u32),
            DataType::Nat(width) => hir::HirType::Nat(*width as u32),
            DataType::Clock { .. } => hir::HirType::Clock(None),
            DataType::Reset { .. } => hir::HirType::Reset {
                polarity: hir::HirResetPolarity::ActiveHigh,
                clock_domain: None,
            },
            DataType::Event => hir::HirType::Event,
            DataType::Array(inner, size) => {
                hir::HirType::Array(Box::new(self.convert_mir_to_hir_type(inner)), *size as u32)
            }
            DataType::Float16 => hir::HirType::Float16,
            DataType::Float32 => hir::HirType::Float32,
            DataType::Float64 => hir::HirType::Float64,
            DataType::Vec2(element) => {
                hir::HirType::Vec2(Box::new(self.convert_mir_to_hir_type(element)))
            }
            DataType::Vec3(element) => {
                hir::HirType::Vec3(Box::new(self.convert_mir_to_hir_type(element)))
            }
            DataType::Vec4(element) => {
                hir::HirType::Vec4(Box::new(self.convert_mir_to_hir_type(element)))
            }
            // Parametric types - use default values
            DataType::BitParam { default, .. } => hir::HirType::Bit(*default as u32),
            DataType::LogicParam { default, .. } => hir::HirType::Logic(*default as u32),
            DataType::IntParam { default, .. } => hir::HirType::Int(*default as u32),
            DataType::NatParam { default, .. } => hir::HirType::Nat(*default as u32),
            DataType::BitExpr { default, .. } => hir::HirType::Bit(*default as u32),
            DataType::LogicExpr { default, .. } => hir::HirType::Logic(*default as u32),
            DataType::IntExpr { default, .. } => hir::HirType::Int(*default as u32),
            DataType::NatExpr { default, .. } => hir::HirType::Nat(*default as u32),
            // For complex types, try to convert back properly
            DataType::Struct(struct_type) => {
                // Check if this is an anonymous tuple struct (name starts with "__tuple_")
                if struct_type.name.starts_with("__tuple_") {
                    // Convert back to HIR Tuple type
                    let element_types: Vec<hir::HirType> = struct_type
                        .fields
                        .iter()
                        .map(|field| self.convert_mir_to_hir_type(&field.field_type))
                        .collect();
                    hir::HirType::Tuple(element_types)
                } else {
                    // Named struct - convert to HIR struct type
                    let fields: Vec<hir::HirStructField> = struct_type
                        .fields
                        .iter()
                        .map(|field| hir::HirStructField {
                            name: field.name.clone(),
                            field_type: self.convert_mir_to_hir_type(&field.field_type),
                        })
                        .collect();
                    hir::HirType::Struct(hir::HirStructType {
                        name: struct_type.name.clone(),
                        fields,
                        packed: struct_type.packed,
                    })
                }
            }
            DataType::Enum(_) => hir::HirType::Nat(32),
            DataType::Union(_) => hir::HirType::Nat(32),
            DataType::Ncl(width) => hir::HirType::Ncl(*width as u32),
        }
    }

    /// Convert struct/vector field access to bit slice
    fn convert_field_access(
        &mut self,
        base: &hir::HirExpression,
        field_name: &str,
    ) -> Option<Expression> {
        debug_println!("[BUG #71 TUPLE FIELD ACCESS] convert_field_access called:");
        debug_println!("[BUG #71 TUPLE FIELD ACCESS]   field_name='{}'", field_name);
        eprintln!(
            "[BUG #71 TUPLE FIELD ACCESS]   base expr type={:?}",
            std::mem::discriminant(base)
        );

        // Map numeric field names (tuple indices) to struct field names
        // e.g., "0" -> "_0", "1" -> "_1", "2" -> "_2"
        let normalized_field_name = if field_name.chars().all(|c| c.is_ascii_digit()) {
            eprintln!(
                "[BUG #71 TUPLE FIELD ACCESS]   Normalized '{}' -> '_{}'",
                field_name, field_name
            );
            format!("_{}", field_name)
        } else {
            field_name.to_string()
        };

        // Build the complete field path from nested accesses
        let mut field_path = vec![normalized_field_name.clone()];
        let mut current_base = base;

        // Walk up the chain to find the root signal/port/variable
        loop {
            match current_base {
                hir::HirExpression::FieldAccess {
                    base: inner_base,
                    field: inner_field,
                } => {
                    // Normalize inner field names too (for nested tuple access)
                    let normalized_inner = if inner_field.chars().all(|c| c.is_ascii_digit()) {
                        format!("_{}", inner_field)
                    } else {
                        inner_field.clone()
                    };
                    field_path.insert(0, normalized_inner);
                    current_base = inner_base.as_ref();
                }
                hir::HirExpression::Signal(sig_id) => {
                    // Check if this signal was flattened
                    if let Some(flattened) = self.flattened_signals.get(sig_id) {
                        // Find the flattened field with matching path
                        for flat_field in flattened {
                            if flat_field.field_path == field_path {
                                return Some(Expression::with_unknown_type(ExpressionKind::Ref(
                                    LValue::Signal(SignalId(flat_field.id)),
                                )));
                            }
                        }
                    }
                    // Not flattened - use mapped signal ID (or fall back to bit range)
                    if let Some(signal_id) = self.signal_map.get(sig_id) {
                        // Fall back to bit range approach
                        let base_lval = LValue::Signal(*signal_id);
                        let (high_bit, low_bit) =
                            self.get_field_bit_range(base, &normalized_field_name)?;
                        let high_expr = Expression::with_unknown_type(ExpressionKind::Literal(
                            Value::Integer(high_bit as i64),
                        ));
                        let low_expr = Expression::with_unknown_type(ExpressionKind::Literal(
                            Value::Integer(low_bit as i64),
                        ));
                        return Some(Expression::with_unknown_type(ExpressionKind::Ref(
                            LValue::RangeSelect {
                                base: Box::new(base_lval),
                                high: Box::new(high_expr),
                                low: Box::new(low_expr),
                            },
                        )));
                    }
                    return None;
                }
                hir::HirExpression::Port(port_id) => {
                    // Check if this port was flattened
                    if let Some(flattened) = self.flattened_ports.get(port_id) {
                        // Find the flattened field with matching path
                        for flat_field in flattened {
                            if flat_field.field_path == field_path {
                                return Some(Expression::with_unknown_type(ExpressionKind::Ref(
                                    LValue::Port(PortId(flat_field.id)),
                                )));
                            }
                        }
                    }
                    // Not flattened - use mapped port ID (or fall back to bit range)
                    if let Some(&port_id_mir) = self.port_map.get(port_id) {
                        // Fall back to bit range approach
                        let base_lval = LValue::Port(port_id_mir);
                        let (high_bit, low_bit) =
                            self.get_field_bit_range(base, &normalized_field_name)?;
                        let high_expr = Expression::with_unknown_type(ExpressionKind::Literal(
                            Value::Integer(high_bit as i64),
                        ));
                        let low_expr = Expression::with_unknown_type(ExpressionKind::Literal(
                            Value::Integer(low_bit as i64),
                        ));
                        return Some(Expression::with_unknown_type(ExpressionKind::Ref(
                            LValue::RangeSelect {
                                base: Box::new(base_lval),
                                high: Box::new(high_expr),
                                low: Box::new(low_expr),
                            },
                        )));
                    }
                    return None;
                }
                hir::HirExpression::Variable(var_id) => {
                    // BUG FIX #13-16, #21-23: Check if this variable is an entity instance
                    // Entity instances are created via let bindings like: let inner = Inner { data };
                    // Accessing inner.result should return the output port signal of the instance
                    println!(
                        "[FIELD_ACCESS] Checking if var {:?} is entity instance. entity_instance_outputs has {} entries",
                        var_id, self.entity_instance_outputs.len()
                    );
                    if let Some(output_ports) = self.entity_instance_outputs.get(var_id) {
                        // This variable is an entity instance - look up the output port
                        if let Some(&signal_id) = output_ports.get(&normalized_field_name) {
                            eprintln!(
                                "[DEBUG] Field access on entity instance: var {:?}, field '{}' -> signal {:?}",
                                var_id, normalized_field_name, signal_id
                            );
                            return Some(Expression::with_unknown_type(ExpressionKind::Ref(
                                LValue::Signal(signal_id),
                            )));
                        } else {
                            // Field not found - might be accessing original field_name (without normalization)
                            if let Some(&signal_id) = output_ports.get(field_name) {
                                eprintln!(
                                    "[DEBUG] Field access on entity instance: var {:?}, field '{}' (original) -> signal {:?}",
                                    var_id, field_name, signal_id
                                );
                                return Some(Expression::with_unknown_type(ExpressionKind::Ref(
                                    LValue::Signal(signal_id),
                                )));
                            }
                            eprintln!(
                                "[DEBUG] Entity instance field '{}' not found. Available: {:?}",
                                field_name,
                                output_ports.keys().collect::<Vec<_>>()
                            );
                            return None;
                        }
                    }

                    // Variables don't get flattened the same way, use bit range approach
                    // CRITICAL FIX #IMPORT_MATCH: Use context-aware lookup for field access too
                    // This matches the fix applied to regular variable lookups
                    let var_id_mir = if let Some(context) = self.get_current_context() {
                        let context_key = (Some(context.clone()), *var_id);
                        if let Some(&mir_id) = self.context_variable_map.get(&context_key) {
                            eprintln!(
                                "[DEBUG] Field access variable lookup: Found context-aware mapping for {:?} in '{}' -> MIR {:?}",
                                var_id, context, mir_id
                            );
                            Some(mir_id)
                        } else {
                            None
                        }
                    } else {
                        None
                    }
                    .or_else(|| self.variable_map.get(var_id).copied())
                    .or_else(|| {
                        // Check dynamic_variables for let bindings from inlined functions
                        self.dynamic_variables
                            .get(var_id)
                            .map(|(mir_id, name, _)| {
                                eprintln!(
                                    "[DEBUG] Field access variable lookup: Found '{}' (HIR {:?}) in dynamic_variables -> MIR {:?}",
                                    name, var_id, mir_id
                                );
                                *mir_id
                            })
                    });

                    if let Some(var_id_mir) = var_id_mir {
                        let base_lval = LValue::Variable(var_id_mir);
                        let (high_bit, low_bit) =
                            self.get_field_bit_range(base, &normalized_field_name)?;
                        let high_expr = Expression::with_unknown_type(ExpressionKind::Literal(
                            Value::Integer(high_bit as i64),
                        ));
                        let low_expr = Expression::with_unknown_type(ExpressionKind::Literal(
                            Value::Integer(low_bit as i64),
                        ));
                        return Some(Expression::with_unknown_type(ExpressionKind::Ref(
                            LValue::RangeSelect {
                                base: Box::new(base_lval),
                                high: Box::new(high_expr),
                                low: Box::new(low_expr),
                            },
                        )));
                    }
                    return None;
                }
                hir::HirExpression::StructLiteral(struct_lit) => {
                    // BUG FIX #70: Handle field access on struct literals
                    // This occurs when function parameters get substituted with struct literal expressions
                    // Example: vec_dot(ray_dir, ray_dir) where ray_dir is vec3{x:..., y:..., z:...}
                    //          After substitution: vec3{x:..., y:..., z:...}.x
                    //
                    // Extract the specific field from the struct literal and convert just that field
                    for field_init in &struct_lit.fields {
                        if field_init.name == field_name {
                            return self.convert_expression(&field_init.value, 0);
                        }
                    }
                    eprintln!(
                        "[DEBUG] FieldAccess on StructLiteral: field '{}' not found in struct '{}'",
                        field_name, struct_lit.type_name
                    );
                    return None;
                }
                hir::HirExpression::TupleLiteral(elements) => {
                    // BUG FIX #22: Handle field access on tuple literals
                    // This occurs when function inlining creates tuple returns that are then destructured
                    // Example: let (rx, ry, rz) = vec3_add(...) where vec3_add returns (x, y, z)
                    // After inlining: let rx = (x, y, z).0, let ry = (x, y, z).1, etc.
                    //
                    // Parse the field name as a tuple index (e.g., "0", "1", "2")
                    if let Ok(index) = field_name.parse::<usize>() {
                        if index < elements.len() {
                            eprintln!(
                                "[BUG #71 TUPLE] ✅ FieldAccess on TupleLiteral: extracting element {} of {} elements",
                                index, elements.len()
                            );
                            eprintln!(
                                "[BUG #71 TUPLE]   Element type: {:?}",
                                std::mem::discriminant(&elements[index])
                            );
                            let result = self.convert_expression(&elements[index], 0);
                            // Debug output for tuple element extraction
                            #[allow(clippy::if_same_then_else)]
                            if result.is_some() {
                                debug_println!(
                                    "[BUG #71 TUPLE]   ✅ Successfully extracted element"
                                );
                            } else {
                                debug_println!("[BUG #71 TUPLE]   ❌ Failed to extract element");
                            }
                            return result;
                        } else {
                            eprintln!(
                                "[BUG #71 TUPLE] ❌ FieldAccess on TupleLiteral: index {} out of bounds (len={})",
                                index, elements.len()
                            );
                            return None;
                        }
                    } else {
                        eprintln!(
                            "[BUG #71 TUPLE] ❌ FieldAccess on TupleLiteral: field '{}' is not a valid index",
                            field_name
                        );
                        return None;
                    }
                }
                hir::HirExpression::GenericParam(param_name) => {
                    // BUG FIX #13-16, #21-23: First check if this is an instance output access
                    // Example: inner.result where inner is an instance of entity Inner
                    if let Some(output_ports) = self.instance_outputs_by_name.get(param_name) {
                        if let Some(&signal_id) = output_ports.get(&normalized_field_name) {
                            return Some(Expression::with_unknown_type(ExpressionKind::Ref(
                                LValue::Signal(signal_id),
                            )));
                        }
                    }

                    // BUG FIX #70 Part 2: Handle field access on GenericParam (unsubstituted parameter)
                    // This occurs when parameter substitution fails or is incomplete during function inlining
                    // Try to look up the parameter in dynamic_variables
                    let var_id_mir = self
                        .dynamic_variables
                        .values()
                        .find(|(_, name, _)| name == param_name)
                        .map(|(mir_id, _, _)| *mir_id);

                    if let Some(var_id_mir) = var_id_mir {
                        let base_lval = LValue::Variable(var_id_mir);
                        let (high_bit, low_bit) =
                            self.get_field_bit_range(current_base, &normalized_field_name)?;
                        let high_expr = Expression::with_unknown_type(ExpressionKind::Literal(
                            Value::Integer(high_bit as i64),
                        ));
                        let low_expr = Expression::with_unknown_type(ExpressionKind::Literal(
                            Value::Integer(low_bit as i64),
                        ));
                        return Some(Expression::with_unknown_type(ExpressionKind::Ref(
                            LValue::RangeSelect {
                                base: Box::new(base_lval),
                                high: Box::new(high_expr),
                                low: Box::new(low_expr),
                            },
                        )));
                    }
                    eprintln!(
                        "[DEBUG] FieldAccess on GenericParam '{}': not found in instance_outputs_by_name or dynamic_variables",
                        param_name
                    );
                    return None;
                }
                hir::HirExpression::Call(call) => {
                    // BUG FIX #74 + #110: Handle field access on function call results
                    // Example: vec_cross(a, b).x where vec_cross returns a struct
                    // Example: quadratic_solve(a, b, c).0 where function returns tuple

                    eprintln!(
                        "[BUG #74/#110 CALL IN FIELD_ACCESS] Function: {}, field: {}",
                        call.function, field_name
                    );

                    // BUG FIX #110: Check if function should be module-synthesized (same logic as HYBRID path)
                    // If the function has too many nested calls, we can't inline it at HIR level
                    // Instead, we should use the module synthesis path via convert_expression
                    let should_use_module_path = if let Some(hir) = self.hir {
                        let func_opt = hir
                            .functions
                            .iter()
                            .find(|f| f.name == call.function)
                            .or_else(|| {
                                hir.implementations
                                    .iter()
                                    .flat_map(|impl_block| impl_block.functions.iter())
                                    .find(|f| f.name == call.function)
                            });

                        if let Some(func) = func_opt {
                            let call_count: usize = func
                                .body
                                .iter()
                                .map(|stmt| self.count_calls_in_statement(stmt))
                                .sum();
                            let result = call_count > MAX_INLINE_CALL_COUNT;
                            debug_println!("[BUG #110] Function '{}' has {} nested calls (threshold={}), should_use_module_path={}",
                                call.function, call_count, MAX_INLINE_CALL_COUNT, result);
                            result
                        } else {
                            false
                        }
                    } else {
                        false
                    };

                    if should_use_module_path {
                        // BUG FIX #110: Use module synthesis path for complex functions
                        // Convert the Call via convert_expression (uses HYBRID logic)
                        // which returns a Concat of result signals for tuple-returning functions
                        eprintln!(
                            "[BUG #110] Using module synthesis path for field access on '{}'",
                            call.function
                        );

                        let call_expr = hir::HirExpression::Call(call.clone());
                        let mir_result = self.convert_expression(&call_expr, 0)?;

                        // Parse field name as tuple index
                        if let Ok(index) = field_name.parse::<usize>() {
                            // Extract the specific element from the Concat
                            if let ExpressionKind::Concat(elements) = &mir_result.kind {
                                if index < elements.len() {
                                    debug_println!("[BUG #110] ✅ Extracted element {} from Concat of {} elements",
                                        index, elements.len());
                                    return Some(elements[index].clone());
                                } else {
                                    debug_println!("[BUG #110] ❌ Index {} out of bounds for Concat with {} elements",
                                        index, elements.len());
                                    return None;
                                }
                            } else {
                                // Single result, index 0 should return it directly
                                if index == 0 {
                                    debug_println!(
                                        "[BUG #110] ✅ Returning single result for index 0"
                                    );
                                    return Some(mir_result);
                                } else {
                                    debug_println!(
                                        "[BUG #110] ❌ Index {} for non-tuple result",
                                        index
                                    );
                                    return None;
                                }
                            }
                        } else {
                            // Non-numeric field name (struct field) - not supported for module path yet
                            debug_println!("[BUG #110] ❌ Non-numeric field '{}' not supported for module path", field_name);
                            return None;
                        }
                    }

                    // Original path: Inline at HIR level with full let binding resolution
                    // This works for simple functions (≤5 nested calls)
                    eprintln!(
                        "[BUG #74] Using inline path for field access on '{}'",
                        call.function
                    );
                    let inlined_hir_expr = match self.inline_function_call_to_hir_with_lets(call) {
                        Some(expr) => expr,
                        None => {
                            eprintln!(
                                "\n❌❌❌ BUG #85 ROOT CAUSE IDENTIFIED ❌❌❌\n\
                                 FieldAccess inlining failed for function: '{}'\n\
                                 Field being accessed: '{}'\n\
                                 Function has {} arguments\n\
                                 \n\
                                 This function was called from within another function's inlined code,\n\
                                 and now we need to access field '{}' on its result.\n\
                                 However, the function '{}' itself failed to inline.\n\
                                 \n\
                                 This is the EXACT function that's blocking the entire assignment conversion!\n\
                                 ❌❌❌❌❌❌❌❌❌❌❌❌❌❌❌❌❌❌❌❌❌❌❌❌❌❌❌❌❌❌❌❌❌❌❌❌❌\n",
                                call.function, field_name, call.args.len(), field_name, call.function
                            );
                            return None;
                        }
                    };

                    eprintln!(
                        "[BUG #74 CALL IN FIELD_ACCESS] Function inlined to HIR expression: {:?}",
                        std::mem::discriminant(&inlined_hir_expr)
                    );

                    // Create new FieldAccess with fully inlined expression as base
                    let field_access = hir::HirExpression::FieldAccess {
                        base: Box::new(inlined_hir_expr),
                        field: field_name.to_string(),
                    };

                    debug_println!(
                        "[BUG #74 CALL IN FIELD_ACCESS] Converting FieldAccess to MIR..."
                    );
                    // Recursively convert the field access
                    let mir_result = self.convert_expression(&field_access, 0);
                    // Debug output for field access conversion
                    #[allow(clippy::if_same_then_else)]
                    if mir_result.is_some() {
                        eprintln!(
                            "[BUG #74 CALL IN FIELD_ACCESS] ✅ FieldAccess conversion succeeded"
                        );
                    } else {
                        debug_println!("[BUG #74 CALL IN FIELD_ACCESS] ❌ FieldAccess conversion returned None");
                    }
                    return mir_result;
                }
                hir::HirExpression::Block {
                    statements,
                    result_expr,
                } => {
                    // BUG FIX #22: Handle field access on Block expressions
                    // This occurs when function inlining creates Block expressions that return tuples/structs
                    // Example: let (rx, ry, rz) = vec3_add(...) where vec3_add inlines to a Block
                    // After inlining: let rx = <Block>.0, let ry = <Block>.1, etc.
                    //
                    // Strategy: Access the field directly on the result expression of the Block
                    // This avoids converting the entire Block to MIR prematurely
                    eprintln!(
                        "[DEBUG] FieldAccess on Block: accessing field '{}' on result_expr (type: {:?})",
                        field_name,
                        std::mem::discriminant(&**result_expr)
                    );

                    // Create new FieldAccess with the Block's result expression as base
                    let field_access = hir::HirExpression::FieldAccess {
                        base: result_expr.clone(),
                        field: field_name.to_string(),
                    };

                    // Recursively convert the field access
                    // This will hit the appropriate handler (StructLiteral, TupleLiteral, etc.)
                    return self.convert_expression(&field_access, 0);
                }
                hir::HirExpression::If(if_expr) => {
                    // BUG FIX #22: Handle field access on If expressions
                    // This occurs when function inlining creates If expressions that return tuples/structs
                    // Example: let rx = (if cond { tuple1 } else { tuple2 }).0
                    // After inlining: if cond { tuple1.0 } else { tuple2.0 }
                    //
                    // Strategy: Push the field access into both branches of the If
                    eprintln!(
                        "[DEBUG] FieldAccess on If: accessing field '{}', pushing into both branches",
                        field_name
                    );

                    // Create FieldAccess for then branch
                    let then_field_access = hir::HirExpression::FieldAccess {
                        base: if_expr.then_expr.clone(),
                        field: field_name.to_string(),
                    };

                    // Create FieldAccess for else branch
                    let else_field_access = hir::HirExpression::FieldAccess {
                        base: if_expr.else_expr.clone(),
                        field: field_name.to_string(),
                    };

                    // Create new If expression with field access in both branches
                    let new_if = hir::HirExpression::If(hir::HirIfExpr {
                        condition: if_expr.condition.clone(),
                        then_expr: Box::new(then_field_access),
                        else_expr: Box::new(else_field_access),
                    });

                    // Convert the new If expression
                    return self.convert_expression(&new_if, 0);
                }
                hir::HirExpression::Index(array_base, index_expr) => {
                    // BUG FIX #31: Handle field access on array index expression
                    // Example: points[index].x where points is Point[SIZE]
                    // This becomes a MUX tree selecting from flattened fields:
                    //   (index == 0) ? points_0_x : ((index == 1) ? points_1_x : ...)
                    eprintln!(
                        "[BUG #31 ARRAY_FIELD] FieldAccess on Index: accessing field '{}' on array element",
                        field_name
                    );

                    // Get the flattened fields for the array base (Signal or Port)
                    let (base_hir_id, is_signal, flattened_fields) = match array_base.as_ref() {
                        hir::HirExpression::Signal(sig_id) => {
                            if let Some(fields) = self.flattened_signals.get(sig_id) {
                                (sig_id.0, true, fields.clone())
                            } else {
                                debug_println!("[BUG #31 ARRAY_FIELD] Signal not flattened");
                                return None;
                            }
                        }
                        hir::HirExpression::Port(port_id) => {
                            if let Some(fields) = self.flattened_ports.get(port_id) {
                                (port_id.0, false, fields.clone())
                            } else {
                                debug_println!("[BUG #31 ARRAY_FIELD] Port not flattened");
                                return None;
                            }
                        }
                        _ => {
                            debug_println!(
                                "[BUG #31 ARRAY_FIELD] Array base is not Signal or Port"
                            );
                            return None;
                        }
                    };

                    // Group flattened fields by array index, then filter for the target field
                    // For Point[4] with fields x, y: points_0_x, points_0_y, points_1_x, ...
                    // We want all fields where path is ["<index>", "<field_name>"]
                    let mut array_elements: Vec<(usize, &FlattenedField)> = Vec::new();

                    for field in &flattened_fields {
                        if field.field_path.len() >= 2 {
                            // First element is array index, second is field name
                            if let Ok(array_idx) = field.field_path[0].parse::<usize>() {
                                // Check if this is the field we're accessing
                                if field.field_path[1] == normalized_field_name {
                                    array_elements.push((array_idx, field));
                                }
                            }
                        }
                    }

                    if array_elements.is_empty() {
                        eprintln!(
                            "[BUG #31 ARRAY_FIELD] No matching fields found for '{}'",
                            field_name
                        );
                        return None;
                    }

                    // Sort by array index
                    array_elements.sort_by_key(|(idx, _)| *idx);

                    // Convert index expression to MIR
                    let mir_index = self.convert_expression(index_expr, 0)?;

                    // Build MUX tree: (index == 0) ? field_0 : ((index == 1) ? field_1 : ...)
                    let mut mux_expr = None;

                    for (array_idx, field) in array_elements.iter().rev() {
                        let elem_expr = if is_signal {
                            Expression::with_unknown_type(ExpressionKind::Ref(LValue::Signal(
                                SignalId(field.id),
                            )))
                        } else {
                            Expression::with_unknown_type(ExpressionKind::Ref(LValue::Port(
                                PortId(field.id),
                            )))
                        };

                        if let Some(else_expr) = mux_expr {
                            // Build condition: index == array_idx
                            let index_literal = Expression::with_unknown_type(
                                ExpressionKind::Literal(Value::Integer(*array_idx as i64)),
                            );
                            let condition = Expression::with_unknown_type(ExpressionKind::Binary {
                                op: BinaryOp::Equal,
                                left: Box::new(mir_index.clone()),
                                right: Box::new(index_literal),
                            });

                            // Build ternary: condition ? elem_expr : else_expr
                            mux_expr =
                                Some(Expression::with_unknown_type(ExpressionKind::Conditional {
                                    cond: Box::new(condition),
                                    then_expr: Box::new(elem_expr),
                                    else_expr: Box::new(else_expr),
                                }));
                        } else {
                            // Last element (default case)
                            mux_expr = Some(elem_expr);
                        }
                    }

                    eprintln!(
                        "[BUG #31 ARRAY_FIELD] Built MUX tree with {} elements",
                        array_elements.len()
                    );
                    return mux_expr;
                }
                _ => {
                    // Complex base - can't handle
                    eprintln!(
                        "[DEBUG] FieldAccess on unsupported base type: {:?}",
                        std::mem::discriminant(current_base)
                    );
                    return None;
                }
            }
        }
    }

    /// Get the bit range for a struct field or vector component
    fn get_field_bit_range(
        &mut self,
        base: &hir::HirExpression,
        field_name: &str,
    ) -> Option<(usize, usize)> {
        debug_println!("[BUG #70 FIELD RANGE] get_field_bit_range called: field_name='{}', base expr discriminant={:?}", field_name, std::mem::discriminant(base));
        // First, check if this is a vector type
        if let Some(base_type) = self.infer_hir_type(base) {
            eprintln!(
                "[BUG #70 FIELD RANGE] Inferred base type: {:?}",
                std::mem::discriminant(&base_type)
            );
            // Handle Custom types that might be vec2/vec3/vec4
            // BUG FIX #44: HIR stores vec2<fp32> as Custom("vec2") instead of Vec2(Float32)
            if let hir::HirType::Custom(type_name) = &base_type {
                // Parse custom type names like "vec2", "vec3", "vec4"
                if type_name.starts_with("vec") {
                    // Extract the dimension (2, 3, or 4)
                    if let Some(dim_char) = type_name.chars().nth(3) {
                        if let Some(dimension) = dim_char.to_digit(10) {
                            // Default to fp32 element width (32 bits) for custom vec types
                            // TODO: Parse element type from full type string if available
                            let elem_width = 32;
                            match (dimension, field_name) {
                                (2, "x") => return Some((elem_width - 1, 0)),
                                (2, "y") => return Some((2 * elem_width - 1, elem_width)),
                                (3, "x") => return Some((elem_width - 1, 0)),
                                (3, "y") => return Some((2 * elem_width - 1, elem_width)),
                                (3, "z") => return Some((3 * elem_width - 1, 2 * elem_width)),
                                (4, "x") => return Some((elem_width - 1, 0)),
                                (4, "y") => return Some((2 * elem_width - 1, elem_width)),
                                (4, "z") => return Some((3 * elem_width - 1, 2 * elem_width)),
                                (4, "w") => return Some((4 * elem_width - 1, 3 * elem_width)),
                                _ => return None,
                            }
                        }
                    }
                }
            }

            match base_type {
                hir::HirType::Vec2(ref element_type) => {
                    let elem_width = self.get_hir_type_width(element_type);
                    match field_name {
                        "x" => return Some((elem_width - 1, 0)),
                        "y" => return Some((2 * elem_width - 1, elem_width)),
                        _ => return None, // Invalid component for vec2
                    }
                }
                hir::HirType::Vec3(ref element_type) => {
                    let elem_width = self.get_hir_type_width(element_type);
                    match field_name {
                        "x" => return Some((elem_width - 1, 0)),
                        "y" => return Some((2 * elem_width - 1, elem_width)),
                        "z" => return Some((3 * elem_width - 1, 2 * elem_width)),
                        _ => return None, // Invalid component for vec3
                    }
                }
                hir::HirType::Vec4(ref element_type) => {
                    let elem_width = self.get_hir_type_width(element_type);
                    match field_name {
                        "x" => return Some((elem_width - 1, 0)),
                        "y" => return Some((2 * elem_width - 1, elem_width)),
                        "z" => return Some((3 * elem_width - 1, 2 * elem_width)),
                        "w" => return Some((4 * elem_width - 1, 3 * elem_width)),
                        _ => return None, // Invalid component for vec4
                    }
                }
                _ => {
                    // Not a vector type, fall through to struct handling
                }
            }
        }

        // Get the struct type from the base expression
        debug_println!("[BUG #70 FIELD RANGE] Calling get_expression_struct_type...");
        let struct_type = self.get_expression_struct_type(base);
        if struct_type.is_none() {
            debug_println!("[BUG #70 FIELD RANGE] get_expression_struct_type returned None!");
            return None;
        }
        let struct_type = struct_type?;

        // Clone the fields to avoid borrow checker issues
        let fields: Vec<_> = struct_type
            .fields
            .iter()
            .map(|f| (f.name.clone(), f.field_type.clone()))
            .collect();

        // BUG FIX #15/#17: Calculate field offset correctly for tuples
        // Tuples are packed as {a, b, c} where 'a' is in MSB (highest bits)
        // So for (bit[32], bit[32]), element 0 is at [63:32], element 1 is at [31:0]
        // We need to calculate offsets from MSB downward for tuple fields

        // First, calculate total width to determine if we need MSB-first ordering
        eprintln!(
            "[BUG #70 TUPLE DEBUG] Calculating total width for {} fields:",
            fields.len()
        );
        for (name, ty) in &fields {
            eprintln!(
                "[BUG #70 TUPLE DEBUG]   Field '{}': type discriminant = {:?}",
                name,
                std::mem::discriminant(ty)
            );
        }
        let total_width: usize = fields
            .iter()
            .map(|(_, ty)| {
                let width = self.get_hir_type_width(ty);
                debug_println!("[BUG #70 TUPLE DEBUG]   Width calculated: {}", width);
                width
            })
            .sum();
        debug_println!("[BUG #70 TUPLE DEBUG] Total width: {}", total_width);

        // Check if this is a tuple (fields named "_0", "_1", etc.)
        let is_tuple = fields
            .iter()
            .all(|(name, _)| name.starts_with('_') && name[1..].parse::<usize>().is_ok());

        if is_tuple {
            // BUG FIX #92: For tuples, ALL elements are 32 bits in hardware
            // because module result signals are always 32 bits regardless of logical type.
            // Layout is MSB-first: element 0 at MSB, element N-1 at LSB
            // This matches TupleLiteral concat order: (a, b, c) -> {a, b, c}
            let num_fields = fields.len();
            for (i, (field_name_in_struct, _field_type)) in fields.iter().enumerate() {
                if *field_name_in_struct == field_name {
                    // Extract field index from name (e.g., "_0" -> 0)
                    let index: usize = field_name[1..].parse().unwrap_or(i);
                    // Tuple elements are packed MSB-first with 32-bit widths:
                    // Element 0 at bits [(N)*32-1 : (N-1)*32], element N-1 at bits [31:0]
                    let element_width = 32;
                    let total_width = num_fields * element_width;
                    let high_bit = total_width - index * element_width - 1;
                    let low_bit = total_width - (index + 1) * element_width;
                    eprintln!(
                        "[BUG #92] Tuple field '{}' (index {}/{}) -> bits [{}:{}] (MSB-first)",
                        field_name, index, num_fields, high_bit, low_bit
                    );
                    return Some((high_bit, low_bit));
                }
            }
        } else {
            // For regular structs: calculate offset from LSB upward (original behavior)
            let mut current_offset = 0;
            for (field_name_in_struct, field_type) in fields {
                let field_width = self.get_hir_type_width(&field_type);
                if field_name_in_struct == field_name {
                    // Found the field - return its bit range
                    let high_bit = current_offset + field_width - 1;
                    let low_bit = current_offset;
                    return Some((high_bit, low_bit));
                }
                current_offset += field_width;
            }
        }
        None // Field not found
    }

    /// Get the struct type of an expression
    fn get_expression_struct_type(&self, expr: &hir::HirExpression) -> Option<&hir::HirStructType> {
        let hir = self.hir?;

        match expr {
            hir::HirExpression::Signal(id) => {
                // Find the signal in implementations
                for impl_block in &hir.implementations {
                    for signal in &impl_block.signals {
                        if signal.id == *id {
                            match &signal.signal_type {
                                hir::HirType::Struct(ref struct_type) => {
                                    return Some(struct_type);
                                }
                                hir::HirType::Tuple(element_types) => {
                                    // Convert tuple to anonymous struct for field access
                                    let fields: Vec<hir::HirStructField> = element_types
                                        .iter()
                                        .enumerate()
                                        .map(|(i, ty)| hir::HirStructField {
                                            name: format!("_{}", i),
                                            field_type: ty.clone(),
                                        })
                                        .collect();

                                    let temp_struct = Box::leak(Box::new(hir::HirStructType {
                                        name: format!("__tuple_{}", fields.len()),
                                        fields,
                                        packed: true,
                                    }));
                                    return Some(temp_struct);
                                }
                                _ => {}
                            }
                        }
                    }
                }

                // If not found as signal, check if it's actually a port
                for entity in &hir.entities {
                    for port in &entity.ports {
                        // Compare based on the numeric ID (this is a heuristic)
                        if port.id.0 == id.0 {
                            match &port.port_type {
                                hir::HirType::Struct(ref struct_type) => {
                                    return Some(struct_type);
                                }
                                hir::HirType::Tuple(element_types) => {
                                    // Convert tuple to anonymous struct for field access
                                    let fields: Vec<hir::HirStructField> = element_types
                                        .iter()
                                        .enumerate()
                                        .map(|(i, ty)| hir::HirStructField {
                                            name: format!("_{}", i),
                                            field_type: ty.clone(),
                                        })
                                        .collect();

                                    let temp_struct = Box::leak(Box::new(hir::HirStructType {
                                        name: format!("__tuple_{}", fields.len()),
                                        fields,
                                        packed: true,
                                    }));
                                    return Some(temp_struct);
                                }
                                _ => {}
                            }
                        }
                    }
                }
            }
            // Note: Port access will be added when we understand how ports are referenced
            hir::HirExpression::Variable(id) => {
                // First check dynamic_variables (for let-bound variables)
                if let Some((_mir_id, _name, var_type)) = self.dynamic_variables.get(id) {
                    match var_type {
                        hir::HirType::Struct(ref struct_type) => {
                            return Some(struct_type);
                        }
                        hir::HirType::Tuple(element_types) => {
                            // Convert tuple to anonymous struct for field access
                            let fields: Vec<hir::HirStructField> = element_types
                                .iter()
                                .enumerate()
                                .map(|(i, ty)| hir::HirStructField {
                                    name: format!("_{}", i),
                                    field_type: ty.clone(),
                                })
                                .collect();

                            let temp_struct = Box::leak(Box::new(hir::HirStructType {
                                name: format!("__tuple_{}", fields.len()),
                                fields,
                                packed: true,
                            }));
                            return Some(temp_struct);
                        }
                        _ => {}
                    }
                }

                // Otherwise check HIR impl block variables
                for impl_block in &hir.implementations {
                    for variable in &impl_block.variables {
                        if variable.id == *id {
                            // Check both Struct and Tuple types
                            match &variable.var_type {
                                hir::HirType::Struct(ref struct_type) => {
                                    return Some(struct_type);
                                }
                                hir::HirType::Tuple(element_types) => {
                                    // Convert tuple to anonymous struct for field access
                                    let fields: Vec<hir::HirStructField> = element_types
                                        .iter()
                                        .enumerate()
                                        .map(|(i, ty)| hir::HirStructField {
                                            name: format!("_{}", i),
                                            field_type: ty.clone(),
                                        })
                                        .collect();

                                    let temp_struct = Box::leak(Box::new(hir::HirStructType {
                                        name: format!("__tuple_{}", fields.len()),
                                        fields,
                                        packed: true,
                                    }));
                                    return Some(temp_struct);
                                }
                                _ => {}
                            }
                        }
                    }
                }
            }
            _ => {}
        }
        None
    }

    /// Get the width in bits of a HIR type
    #[allow(clippy::only_used_in_recursion)]
    fn get_hir_type_width(&mut self, hir_type: &hir::HirType) -> usize {
        match hir_type {
            hir::HirType::Bit(width) => *width as usize,
            hir::HirType::Bool => 1, // Boolean is represented as 1 bit in hardware
            hir::HirType::String => {
                panic!("String type is not synthesizable and cannot be used in hardware modules. String types are only supported in testbench contexts.");
            }
            hir::HirType::Logic(width) => *width as usize,
            hir::HirType::Int(width) => *width as usize,
            hir::HirType::Nat(width) => *width as usize,
            hir::HirType::Clock(_) => 1,
            hir::HirType::Reset { .. } => 1,
            hir::HirType::Event => 0,
            hir::HirType::Struct(struct_type) => {
                let mut total_width = 0;
                for field in &struct_type.fields {
                    total_width += self.get_hir_type_width(&field.field_type);
                }
                total_width
            }
            hir::HirType::Enum(enum_type) => self.get_hir_type_width(&enum_type.base_type),
            hir::HirType::Union(union_type) => {
                // Union width is the maximum of all field widths
                union_type
                    .fields
                    .iter()
                    .map(|field| self.get_hir_type_width(&field.field_type))
                    .max()
                    .unwrap_or(0)
            }
            hir::HirType::Array(element_type, size) => {
                self.get_hir_type_width(element_type) * (*size as usize)
            }
            hir::HirType::Stream(element_type) => {
                // Stream types don't have a defined width in hardware
                self.get_hir_type_width(element_type)
            }
            hir::HirType::Vec3(element_type) => {
                // BUG FIX #70: vec3 is 3 elements
                let element_width = self.get_hir_type_width(element_type);
                eprintln!(
                    "[BUG #70 FIX] Vec3 element width: {}, total: {}",
                    element_width,
                    element_width * 3
                );
                element_width * 3
            }
            hir::HirType::Vec4(element_type) => {
                // BUG FIX #70: vec4 is 4 elements
                let element_width = self.get_hir_type_width(element_type);
                eprintln!(
                    "[BUG #70 FIX] Vec4 element width: {}, total: {}",
                    element_width,
                    element_width * 4
                );
                element_width * 4
            }
            hir::HirType::Custom(type_name) => {
                // BUG FIX #70: Look up custom struct types and calculate their actual width
                eprintln!(
                    "[BUG #70 DEBUG] get_hir_type_width called for Custom type '{}'",
                    type_name
                );
                // Check if this is a known struct type
                if let Some(hir) = self.hir {
                    eprintln!(
                        "[BUG #70 DEBUG] HIR available, searching {} user_defined_types",
                        hir.user_defined_types.len()
                    );
                    // Search in user-defined types
                    for user_type in &hir.user_defined_types {
                        if user_type.name == *type_name {
                            // Check if it's a struct type
                            if let hir::HirType::Struct(struct_type) = &user_type.type_def {
                                let mut total_width = 0;
                                for field in &struct_type.fields {
                                    total_width += self.get_hir_type_width(&field.field_type);
                                }
                                debug_println!("[BUG #70 FIX] Custom type '{}' resolved to struct with width {}", type_name, total_width);
                                return total_width;
                            }
                        }
                    }
                }
                // Fallback: Custom types default to 32 bits if not found
                debug_println!("[BUG #70 WARNING] Custom type '{}' not found in user_defined_types, defaulting to 32 bits", type_name);
                32
            }
            // Parametric types - use default widths
            hir::HirType::BitParam(_) => 8,   // Default bit width
            hir::HirType::LogicParam(_) => 8, // Default logic width
            hir::HirType::IntParam(_) => 32,  // Default int width
            hir::HirType::NatParam(_) => 32,  // Default nat width
            // Expression-based types - evaluate or use default
            hir::HirType::BitExpr(expr) => self.try_eval_const_expr(expr).unwrap_or(8) as usize,
            hir::HirType::LogicExpr(expr) => self.try_eval_const_expr(expr).unwrap_or(8) as usize,
            hir::HirType::IntExpr(expr) => self.try_eval_const_expr(expr).unwrap_or(32) as usize,
            hir::HirType::NatExpr(expr) => self.try_eval_const_expr(expr).unwrap_or(32) as usize,
            hir::HirType::ArrayExpr(element_type, size_expr) => {
                let element_width = self.get_hir_type_width(element_type);
                let size = self.try_eval_const_expr(size_expr).unwrap_or(1) as usize;
                element_width * size
            }
            // Floating-point types have fixed widths
            hir::HirType::Float16 => 16,
            hir::HirType::Float32 => 32,
            hir::HirType::Float64 => 64,
            // Vector types - width is element_width * component_count
            hir::HirType::Vec2(element_type) => self.get_hir_type_width(element_type) * 2,
            // Vec3 and Vec4 handled above with debug output (lines 7017-7036)

            // Parametric numeric types
            hir::HirType::FpParametric { format } => {
                // TODO: Evaluate format to get concrete width
                // For now, default to Float32 width
                self.try_eval_const_expr(format).unwrap_or(32) as usize
            }
            hir::HirType::FixedParametric {
                width,
                frac: _,
                signed: _,
            } => {
                // Width is the total width of the fixed-point number
                self.try_eval_const_expr(width).unwrap_or(32) as usize
            }
            hir::HirType::IntParametric { width, signed: _ } => {
                // Width is the total width of the integer
                self.try_eval_const_expr(width).unwrap_or(32) as usize
            }
            hir::HirType::VecParametric {
                element_type,
                dimension,
            } => {
                let elem_width = self.get_hir_type_width(element_type);
                let dim = self.try_eval_const_expr(dimension).unwrap_or(3) as usize;
                elem_width * dim
            }
            hir::HirType::Tuple(element_types) => {
                // Tuple width is the sum of all element widths (packed representation)
                element_types
                    .iter()
                    .map(|ty| self.get_hir_type_width(ty))
                    .sum()
            }
            // NCL dual-rail types - physical width is 2x logical width
            hir::HirType::Ncl(width) => *width as usize * 2,
            hir::HirType::NclParam(_) => 64, // Default: 32 logical bits * 2
            hir::HirType::NclExpr(expr) => {
                self.try_eval_const_expr(expr).unwrap_or(32) as usize * 2
            }
        }
    }

    /// Resolve enum variant to its integer value
    fn resolve_enum_variant_value(&self, enum_type: &str, variant: &str) -> Option<Expression> {
        // Find the enum type in the HIR
        if let Some(hir) = self.hir {
            // BUG #174 FIX: First search in user_defined_types for top-level enum declarations
            for udt in &hir.user_defined_types {
                if let hir::HirType::Enum(ref enum_def) = &udt.type_def {
                    if enum_def.name == enum_type {
                        return self.find_variant_value(enum_def, variant);
                    }
                }
            }

            // Look through all entities and implementations to find the enum type
            for entity in &hir.entities {
                for port in &entity.ports {
                    if let hir::HirType::Enum(ref enum_def) = &port.port_type {
                        if enum_def.name == enum_type {
                            return self.find_variant_value(enum_def, variant);
                        }
                    }
                }
            }

            for impl_block in &hir.implementations {
                for signal in &impl_block.signals {
                    if let hir::HirType::Enum(ref enum_def) = &signal.signal_type {
                        if enum_def.name == enum_type {
                            return self.find_variant_value(enum_def, variant);
                        }
                    }
                }
            }
        }

        // BUG #175 FIX: Search in imported modules' HIRs for enum definitions
        // This is needed when functions from imported modules use enums defined in those modules
        for module_hir in self.module_hirs.values() {
            // Search in user_defined_types
            for udt in &module_hir.user_defined_types {
                if let hir::HirType::Enum(ref enum_def) = &udt.type_def {
                    if enum_def.name == enum_type {
                        return self.find_variant_value(enum_def, variant);
                    }
                }
            }

            // Search in entities
            for entity in &module_hir.entities {
                for port in &entity.ports {
                    if let hir::HirType::Enum(ref enum_def) = &port.port_type {
                        if enum_def.name == enum_type {
                            return self.find_variant_value(enum_def, variant);
                        }
                    }
                }
            }

            // Search in implementations
            for impl_block in &module_hir.implementations {
                for signal in &impl_block.signals {
                    if let hir::HirType::Enum(ref enum_def) = &signal.signal_type {
                        if enum_def.name == enum_type {
                            return self.find_variant_value(enum_def, variant);
                        }
                    }
                }
            }
        }

        // If not found, return default value 0
        eprintln!(
            "[DEBUG] resolve_enum_variant_value: enum '{}' NOT FOUND in current HIR or {} imported modules, returning 0",
            enum_type,
            self.module_hirs.len()
        );
        Some(Expression::with_unknown_type(ExpressionKind::Literal(
            Value::Integer(0),
        )))
    }

    /// Find the value of a specific variant in an enum
    fn find_variant_value(&self, enum_def: &hir::HirEnumType, variant: &str) -> Option<Expression> {
        eprintln!(
            "[DEBUG] find_variant_value: looking for '{}' in enum '{}' with {} variants",
            variant,
            enum_def.name,
            enum_def.variants.len()
        );
        for (index, enum_variant) in enum_def.variants.iter().enumerate() {
            eprintln!(
                "[DEBUG]   variant[{}]: name='{}', has_value={}",
                index,
                enum_variant.name,
                enum_variant.value.is_some()
            );
            if enum_variant.name == variant {
                // If the variant has an explicit value, use it
                if let Some(ref value_expr) = enum_variant.value {
                    eprintln!("[DEBUG]   found variant with explicit value");
                    if let Some(value) = self.convert_literal_expr_immutable(value_expr) {
                        eprintln!("[DEBUG]   converted value: {:?}", value);
                        return Some(Expression::with_unknown_type(ExpressionKind::Literal(
                            value,
                        )));
                    } else {
                        eprintln!("[DEBUG]   FAILED to convert value expression");
                    }
                }
                // Otherwise, use the index
                eprintln!("[DEBUG]   using index {} as fallback", index);
                return Some(Expression::with_unknown_type(ExpressionKind::Literal(
                    Value::Integer(index as i64),
                )));
            }
        }
        // Variant not found, return 0
        eprintln!(
            "[DEBUG] find_variant_value: variant '{}' NOT FOUND in enum '{}'",
            variant, enum_def.name
        );
        Some(Expression::with_unknown_type(ExpressionKind::Literal(
            Value::Integer(0),
        )))
    }

    /// Convert literal expression (immutable version)
    fn convert_literal_expr_immutable(&self, expr: &hir::HirExpression) -> Option<Value> {
        if let hir::HirExpression::Literal(lit) = expr {
            self.convert_literal_immutable(lit)
        } else {
            None
        }
    }

    /// BUG #33 FIX: Resolve a constant identifier to its MIR expression value
    /// Used for constant patterns in Match expressions
    fn resolve_constant_value(&self, const_name: &str) -> Option<Expression> {
        let hir = self.hir?;

        // Look through all implementations for the constant
        for impl_block in &hir.implementations {
            for constant in &impl_block.constants {
                if constant.name == const_name {
                    // Try to convert the constant's value expression
                    // For simple literals, this should work directly
                    return self.convert_expression_for_constant(&constant.value);
                }
            }
        }

        // Also check global functions for constants (they might be defined at module level)
        // Note: Global constants would be in hir.constants if that field existed
        // For now, we'll just return None if not found in implementations
        None
    }

    /// Convert a constant expression (for pattern matching)
    /// This is a simplified version that only handles literal values
    fn convert_expression_for_constant(&self, expr: &hir::HirExpression) -> Option<Expression> {
        match expr {
            hir::HirExpression::Literal(lit) => Some(Expression::with_unknown_type(
                ExpressionKind::Literal(self.convert_literal_immutable(lit)?),
            )),
            hir::HirExpression::Binary(bin) => {
                // For simple binary expressions like 0b010010, handle it
                let left = self.convert_expression_for_constant(&bin.left)?;
                let right = self.convert_expression_for_constant(&bin.right)?;
                let op = self.convert_binary_op(&bin.op, &bin.left);
                Some(Expression::with_unknown_type(ExpressionKind::Binary {
                    op,
                    left: Box::new(left),
                    right: Box::new(right),
                }))
            }
            _ => None,
        }
    }

    /// Convert literal (immutable version)
    fn convert_literal_immutable(&self, lit: &hir::HirLiteral) -> Option<Value> {
        match lit {
            hir::HirLiteral::Integer(val) => Some(Value::Integer(*val as i64)),
            hir::HirLiteral::Boolean(b) => Some(Value::Integer(if *b { 1 } else { 0 })),
            hir::HirLiteral::Float(f) => Some(Value::Float(*f)),
            hir::HirLiteral::String(s) => Some(Value::String(s.clone())),
            hir::HirLiteral::BitVector(bits) => {
                // Convert bit vector to integer
                let mut value = 0i64;
                for (i, bit) in bits.iter().enumerate() {
                    if *bit {
                        value |= 1 << i;
                    }
                }
                Some(Value::Integer(value))
            }
        }
    }

    /// BUG #145 FIX: Try to evaluate a HIR expression to a constant u64 value
    /// Used for constant folding in Range expressions after function inlining
    #[allow(clippy::only_used_in_recursion)]
    fn try_evaluate_constant_expr(&self, expr: &hir::HirExpression) -> Option<u64> {
        match expr {
            hir::HirExpression::Literal(lit) => match lit {
                hir::HirLiteral::Integer(val) => Some(*val),
                hir::HirLiteral::Boolean(b) => Some(if *b { 1 } else { 0 }),
                hir::HirLiteral::BitVector(bits) => {
                    let mut value = 0u64;
                    for (i, bit) in bits.iter().enumerate() {
                        if *bit && i < 64 {
                            value |= 1 << i;
                        }
                    }
                    Some(value)
                }
                _ => None,
            },
            hir::HirExpression::Cast(cast_expr) => {
                // For casts, try to evaluate the inner expression
                self.try_evaluate_constant_expr(&cast_expr.expr)
            }
            hir::HirExpression::Binary(bin) => {
                let left = self.try_evaluate_constant_expr(&bin.left)?;
                let right = self.try_evaluate_constant_expr(&bin.right)?;
                match bin.op {
                    hir::HirBinaryOp::Add => Some(left.wrapping_add(right)),
                    hir::HirBinaryOp::Sub => Some(left.wrapping_sub(right)),
                    hir::HirBinaryOp::Mul => Some(left.wrapping_mul(right)),
                    hir::HirBinaryOp::Div if right != 0 => Some(left / right),
                    hir::HirBinaryOp::Mod if right != 0 => Some(left % right),
                    hir::HirBinaryOp::And => Some(left & right),
                    hir::HirBinaryOp::Or => Some(left | right),
                    hir::HirBinaryOp::Xor => Some(left ^ right),
                    hir::HirBinaryOp::LeftShift => Some(left.wrapping_shl(right as u32)),
                    hir::HirBinaryOp::RightShift => Some(left.wrapping_shr(right as u32)),
                    _ => None,
                }
            }
            hir::HirExpression::Unary(un) => {
                let operand = self.try_evaluate_constant_expr(&un.operand)?;
                match un.op {
                    hir::HirUnaryOp::Negate => Some((!operand).wrapping_add(1)), // Two's complement
                    hir::HirUnaryOp::BitwiseNot => Some(!operand),
                    hir::HirUnaryOp::Not => Some(if operand == 0 { 1 } else { 0 }),
                    _ => None,
                }
            }
            _ => None,
        }
    }

    fn next_module_id(&mut self) -> ModuleId {
        let id = ModuleId(self.next_module_id);
        self.next_module_id += 1;
        id
    }

    /// Count the number of function calls in a HIR expression (recursively)
    /// This is used by the hybrid inlining/module-instantiation heuristic to decide
    /// whether a function is "simple" (inline it) or "complex" (synthesize as module)
    fn count_function_calls(&self, expr: &hir::HirExpression) -> usize {
        match expr {
            hir::HirExpression::Call(call_expr) => {
                // This is a function call - count it as 1, plus any calls in arguments
                println!(
                    "[COUNT_CALLS] Found Call to '{}' with {} args",
                    call_expr.function,
                    call_expr.args.len()
                );
                1 + self.count_calls_in_call_expr(expr)
            }
            hir::HirExpression::Binary(bin) => {
                // Count nested calls in operands
                let nested_count =
                    self.count_function_calls(&bin.left) + self.count_function_calls(&bin.right);

                // BUG FIX: Check if this binary expression uses a trait operator
                // The is_trait_op flag is set during monomorphization when we detect
                // that a binary operation uses a trait implementation (like fp32 arithmetic).
                // Trait operators effectively become entity instantiations (FpAdd, FpMul, etc.)
                // which are complex operations that should count toward the inlining threshold.
                if bin.is_trait_op {
                    println!(
                        "[COUNT_CALLS] Binary op {:?} is trait operator (is_trait_op=true) - counting as 1",
                        bin.op
                    );
                    1 + nested_count
                } else if self.is_trait_operator_binary(&bin.op, &bin.left) {
                    // Fallback: try to detect via type inference (for cases where monomorphization didn't mark it)
                    println!(
                        "[COUNT_CALLS] Binary op {:?} detected as trait operator via type inference - counting as 1",
                        bin.op
                    );
                    1 + nested_count
                } else {
                    nested_count
                }
            }
            hir::HirExpression::Unary(un) => self.count_function_calls(&un.operand),
            hir::HirExpression::Index(base, index) => {
                self.count_function_calls(base) + self.count_function_calls(index)
            }
            hir::HirExpression::Range(start, end, step) => {
                self.count_function_calls(start)
                    + self.count_function_calls(end)
                    + self.count_function_calls(step)
            }
            hir::HirExpression::FieldAccess { base, .. } => self.count_function_calls(base),
            hir::HirExpression::If(if_expr) => {
                self.count_function_calls(&if_expr.condition)
                    + self.count_function_calls(&if_expr.then_expr)
                    + self.count_function_calls(&if_expr.else_expr)
            }
            hir::HirExpression::Match(match_expr) => {
                println!(
                    "[COUNT_CALLS] Match expression with {} arms",
                    match_expr.arms.len()
                );
                let scrutinee_calls = self.count_function_calls(&match_expr.expr);
                debug_println!("[COUNT_CALLS] Match scrutinee calls: {}", scrutinee_calls);
                for (i, arm) in match_expr.arms.iter().enumerate() {
                    println!(
                        "[COUNT_CALLS] Arm {} expr type: {:?}",
                        i,
                        std::mem::discriminant(&arm.expr)
                    );
                    let arm_count = self.count_function_calls(&arm.expr);
                    debug_println!("[COUNT_CALLS] Arm {} count: {}", i, arm_count);
                }
                let arm_calls: usize = match_expr
                    .arms
                    .iter()
                    .map(|arm| self.count_function_calls(&arm.expr))
                    .sum();
                debug_println!("[COUNT_CALLS] Total arm calls: {}", arm_calls);
                scrutinee_calls + arm_calls
            }
            hir::HirExpression::Block {
                statements,
                result_expr,
            } => {
                let stmt_calls: usize = statements
                    .iter()
                    .map(|stmt| self.count_calls_in_statement(stmt))
                    .sum();
                let result_calls = self.count_function_calls(result_expr);
                stmt_calls + result_calls
            }
            hir::HirExpression::TupleLiteral(elements) => {
                elements.iter().map(|e| self.count_function_calls(e)).sum()
            }
            hir::HirExpression::ArrayLiteral(elements) => {
                elements.iter().map(|e| self.count_function_calls(e)).sum()
            }
            hir::HirExpression::StructLiteral(struct_lit) => {
                // Count nested calls in field values
                let nested_count: usize = struct_lit
                    .fields
                    .iter()
                    .map(|f| self.count_function_calls(&f.value))
                    .sum();

                // BUG FIX: Check if this is an entity instantiation (like FpMul, FpAdd)
                // Entity instantiations are complex operations that should count toward the inlining threshold.
                if self.is_entity_struct_literal(&struct_lit.type_name) {
                    println!(
                        "[COUNT_CALLS] StructLiteral '{}' is entity - counting as 1",
                        struct_lit.type_name
                    );
                    1 + nested_count
                } else {
                    nested_count
                }
            }
            hir::HirExpression::Cast(cast_expr) => {
                println!(
                    "[COUNT_CALLS] Cast inner expr type: {:?}",
                    std::mem::discriminant(&*cast_expr.expr)
                );
                let inner_count = self.count_function_calls(&cast_expr.expr);
                debug_println!("[COUNT_CALLS] Cast inner count: {}", inner_count);
                inner_count
            }
            hir::HirExpression::Concat(exprs) => {
                exprs.iter().map(|e| self.count_function_calls(e)).sum()
            }
            hir::HirExpression::ArrayRepeat { count, value } => {
                self.count_function_calls(count) + self.count_function_calls(value)
            }
            // Leaf nodes - no function calls
            hir::HirExpression::Literal(_)
            | hir::HirExpression::Signal(_)
            | hir::HirExpression::Port(_)
            | hir::HirExpression::Variable(_)
            | hir::HirExpression::Constant(_)
            | hir::HirExpression::EnumVariant { .. } => 0,
            hir::HirExpression::GenericParam(param_name) => {
                debug_println!("[COUNT_CALLS] GenericParam: '{}'", param_name);
                0
            }
            hir::HirExpression::Ternary {
                condition,
                true_expr,
                false_expr,
            } => {
                self.count_function_calls(condition)
                    + self.count_function_calls(true_expr)
                    + self.count_function_calls(false_expr)
            }
            hir::HirExpression::AssociatedConstant { .. } => 0,
        }
    }

    /// Helper to count calls in a Call expression's arguments
    fn count_calls_in_call_expr(&self, expr: &hir::HirExpression) -> usize {
        if let hir::HirExpression::Call(call) = expr {
            call.args
                .iter()
                .map(|arg| self.count_function_calls(arg))
                .sum()
        } else {
            0
        }
    }

    /// Check if a struct literal is an entity instantiation (like FpMul, FpAdd)
    /// This is used by count_function_calls to count entity instantiations toward the inlining threshold.
    fn is_entity_struct_literal(&self, type_name: &str) -> bool {
        if let Some(hir) = self.hir {
            // Check if the type name matches any entity in the HIR
            // Also check for monomorphized names (e.g., "FpMul_fp32" from "FpMul")
            let base_name = type_name.split('_').next().unwrap_or(type_name);
            hir.entities
                .iter()
                .any(|e| e.name == type_name || e.name == base_name)
        } else {
            false
        }
    }

    /// Check if a binary expression uses a trait operator (like fp32 arithmetic)
    /// This is used by count_function_calls to count trait operators toward the inlining threshold.
    fn is_trait_operator_binary(
        &self,
        op: &hir::HirBinaryOp,
        left_expr: &hir::HirExpression,
    ) -> bool {
        // Check if the operator is one that might have a trait implementation
        // (arithmetic and comparison operators - not logical operators)
        let is_trait_op = matches!(
            op,
            hir::HirBinaryOp::Add
                | hir::HirBinaryOp::Sub
                | hir::HirBinaryOp::Mul
                | hir::HirBinaryOp::Div
                | hir::HirBinaryOp::Mod
                | hir::HirBinaryOp::Less
                | hir::HirBinaryOp::LessEqual
                | hir::HirBinaryOp::Greater
                | hir::HirBinaryOp::GreaterEqual
                | hir::HirBinaryOp::Equal
                | hir::HirBinaryOp::NotEqual
        );

        if !is_trait_op {
            return false;
        }

        // Try to determine if the operand has a trait type
        // Method 1: Check if the left operand is a Cast to a float type
        if let hir::HirExpression::Cast(cast_expr) = left_expr {
            if matches!(
                cast_expr.target_type,
                hir::HirType::Float16 | hir::HirType::Float32 | hir::HirType::Float64
            ) || matches!(cast_expr.target_type, hir::HirType::Custom(_))
            {
                println!(
                    "[IS_TRAIT_OP] op={:?} is trait op (Cast to {:?})",
                    op, cast_expr.target_type
                );
                return true;
            }
        }

        // Method 2: Try to infer the type (works when type info is available)
        if let Some(left_type) = self.infer_hir_type(left_expr) {
            let has_trait_type = matches!(
                left_type,
                hir::HirType::Float16 | hir::HirType::Float32 | hir::HirType::Float64
            ) || matches!(left_type, hir::HirType::Custom(_));

            if has_trait_type {
                println!(
                    "[IS_TRAIT_OP] op={:?} is trait op (inferred type {:?})",
                    op, left_type
                );
                return true;
            }
        }

        // Method 3: Check if left expression recursively contains float operations
        // This handles cases like `t1 + t2` where t1 and t2 are results of fp operations
        if self.expression_involves_float_type(left_expr) {
            println!(
                "[IS_TRAIT_OP] op={:?} is trait op (expression involves float)",
                op
            );
            return true;
        }

        false
    }

    /// Helper to check if an expression involves float types (recursively)
    #[allow(clippy::only_used_in_recursion)]
    fn expression_involves_float_type(&self, expr: &hir::HirExpression) -> bool {
        match expr {
            hir::HirExpression::Cast(cast_expr) => {
                matches!(
                    cast_expr.target_type,
                    hir::HirType::Float16 | hir::HirType::Float32 | hir::HirType::Float64
                ) || matches!(cast_expr.target_type, hir::HirType::Custom(ref s) if s.starts_with("fp"))
            }
            hir::HirExpression::Binary(bin) => {
                self.expression_involves_float_type(&bin.left)
                    || self.expression_involves_float_type(&bin.right)
            }
            hir::HirExpression::Unary(un) => self.expression_involves_float_type(&un.operand),
            hir::HirExpression::Variable(_) => {
                // For variables, we can't determine without type info
                // Check if the variable name hints at float type
                false
            }
            hir::HirExpression::FieldAccess { base, .. } => {
                self.expression_involves_float_type(base)
            }
            _ => false,
        }
    }

    /// Helper to count calls in a statement
    fn count_calls_in_statement(&self, stmt: &hir::HirStatement) -> usize {
        let result = match stmt {
            hir::HirStatement::Let(let_stmt) => {
                debug_println!("[COUNT_CALLS_STMT] Let statement: {}", let_stmt.name);
                self.count_function_calls(&let_stmt.value)
            }
            hir::HirStatement::Assignment(assign) => {
                debug_println!("[COUNT_CALLS_STMT] Assignment statement");
                self.count_function_calls(&assign.rhs)
            }
            hir::HirStatement::Expression(expr) => {
                println!(
                    "[COUNT_CALLS_STMT] Expression statement: {:?}",
                    std::mem::discriminant(expr)
                );
                self.count_function_calls(expr)
            }
            hir::HirStatement::If(if_stmt) => {
                debug_println!("[COUNT_CALLS_STMT] If statement");
                let cond_calls = self.count_function_calls(&if_stmt.condition);
                let then_calls: usize = if_stmt
                    .then_statements
                    .iter()
                    .map(|s| self.count_calls_in_statement(s))
                    .sum();
                let else_calls: usize = if_stmt
                    .else_statements
                    .as_ref()
                    .map(|stmts| stmts.iter().map(|s| self.count_calls_in_statement(s)).sum())
                    .unwrap_or(0);
                cond_calls + then_calls + else_calls
            }
            hir::HirStatement::Match(match_stmt) => {
                debug_println!("[COUNT_CALLS_STMT] Match statement");
                let scrut_calls = self.count_function_calls(&match_stmt.expr);
                let arm_calls: usize = match_stmt
                    .arms
                    .iter()
                    .flat_map(|arm| &arm.statements)
                    .map(|stmt| self.count_calls_in_statement(stmt))
                    .sum();
                scrut_calls + arm_calls
            }
            hir::HirStatement::Return(value_opt) => {
                println!(
                    "[COUNT_CALLS_STMT] Return statement: value_opt.is_some()={}",
                    value_opt.is_some()
                );
                if let Some(e) = value_opt.as_ref() {
                    println!(
                        "[COUNT_CALLS_STMT] Return value expr type: {:?}",
                        std::mem::discriminant(e)
                    );
                }
                value_opt
                    .as_ref()
                    .map(|e| self.count_function_calls(e))
                    .unwrap_or(0)
            }
            _ => {
                println!(
                    "[COUNT_CALLS_STMT] Other statement type: {:?}",
                    std::mem::discriminant(stmt)
                );
                0
            }
        };
        debug_println!("[COUNT_CALLS_STMT] Result: {}", result);
        result
    }

    /// BUG FIX #85: Extract let bindings from an HIR expression recursively
    /// This handles match expressions where let bindings are inside match arms
    fn extract_let_bindings_from_expression(
        expr: &hir::HirExpression,
    ) -> Vec<hir::HirLetStatement> {
        let mut result = Vec::new();

        match expr {
            // Match expression - extract let bindings from each arm
            hir::HirExpression::Match(match_expr) => {
                for arm in &match_expr.arms {
                    // Each arm's expression might be a Block or another Match
                    result.extend(Self::extract_let_bindings_from_expression(&arm.expr));
                }
            }

            // Block expression - contains statements including Let bindings
            hir::HirExpression::Block {
                statements,
                result_expr,
            } => {
                // Extract let bindings from statements
                for stmt in statements {
                    if let hir::HirStatement::Let(let_stmt) = stmt {
                        result.push(let_stmt.clone());
                    }
                    // Could also recurse into if statements etc. if needed
                }
                // Recurse into the result expression (might be another match or block)
                result.extend(Self::extract_let_bindings_from_expression(result_expr));
            }

            // If expression - check both branches
            hir::HirExpression::If(if_expr) => {
                result.extend(Self::extract_let_bindings_from_expression(
                    &if_expr.then_expr,
                ));
                result.extend(Self::extract_let_bindings_from_expression(
                    &if_expr.else_expr,
                ));
            }

            // Other expression types don't contain let bindings
            _ => {}
        }

        result
    }

    /// BUG FIX #126: Convert HIR expression to MIR, using pre-converted MIR expressions for Variables
    /// This is used in MODULE_BLOCK to convert result_expr while using the already-converted
    /// let binding values instead of doing HIR substitution (which changes Call args and breaks caching).
    fn convert_hir_expr_with_mir_cache(
        &mut self,
        expr: &hir::HirExpression,
        ctx: &ModuleSynthesisContext,
        mir_cache: &IndexMap<hir::VariableId, Expression>,
        depth: usize,
    ) -> Option<Expression> {
        if depth > 100 {
            debug_eprintln!("    ⚠️  MIR cache conversion depth exceeded 100");
            return None;
        }

        match expr {
            // For Variables, first check if we have a pre-converted MIR expression
            hir::HirExpression::Variable(var_id) => {
                // BUG #193 DEBUG: Trace Variable(16) specifically in convert_hir_expr_with_mir_cache
                if var_id.0 == 16 {
                    println!(
                        "[BUG #193 DEBUG] convert_hir_expr_with_mir_cache: Variable(16), placeholder_map size={}, contains_16={}",
                        self.placeholder_signal_to_entity_output.len(),
                        self.placeholder_signal_to_entity_output.contains_key(var_id)
                    );
                }

                // BUG #192 FIX: Check placeholder map FIRST, before using cached MIR
                // This is critical for FP operations where the 'result' signal is passed to
                // an entity (like FpAdd) and later returned. The cached MIR might just be
                // the placeholder signal (Literal(0)), but we need the entity output wire.
                // BUG #201 FIX: Use stored signal type instead of unknown
                if let Some((output_signal_id, signal_type)) =
                    self.placeholder_signal_to_entity_output.get(var_id)
                {
                    println!(
                        "[BUG #192 FIX] Variable {:?} is placeholder -> returning entity output signal {:?} (type {:?})",
                        var_id, output_signal_id, signal_type
                    );
                    return Some(Expression::new(
                        ExpressionKind::Ref(LValue::Signal(*output_signal_id)),
                        signal_type.clone(),
                    ));
                }

                if let Some(mir_expr) = mir_cache.get(var_id) {
                    println!(
                        "🔄🔄🔄 BUG #126 FIX: Using cached MIR for Variable({}) 🔄🔄🔄",
                        var_id.0
                    );
                    return Some(mir_expr.clone());
                }
                // Fall back to normal module context conversion
                self.convert_hir_expr_for_module(expr, ctx, depth)
            }

            // For Concat, recursively convert each element
            hir::HirExpression::Concat(parts) => {
                let mut converted = Vec::new();
                for part in parts {
                    converted.push(self.convert_hir_expr_with_mir_cache(
                        part,
                        ctx,
                        mir_cache,
                        depth + 1,
                    )?);
                }
                Some(Expression::with_unknown_type(ExpressionKind::Concat(
                    converted,
                )))
            }

            // BUG FIX #128: Handle FieldAccess specially to use mir_cache for base Variables
            // This is critical for tuple destructuring: `let valid = _tuple_tmp_49.0`
            // The base (_tuple_tmp_49) is a Variable that was converted to a Concat of module outputs,
            // and we need to use that cached Concat to extract the correct element.
            hir::HirExpression::FieldAccess { base, field } => {
                println!(
                    "🔄🔄🔄 BUG #128 FIX: FieldAccess '{}' in mir_cache conversion 🔄🔄🔄",
                    field
                );

                // Convert the base using mir_cache (this is the key fix!)
                let base_converted =
                    self.convert_hir_expr_with_mir_cache(base, ctx, mir_cache, depth + 1)?;

                // For numeric fields (tuple element access), extract from the Concat
                if let Ok(index) = field.parse::<usize>() {
                    println!(
                        "🔄🔄🔄 BUG #128 FIX: Tuple element {} extraction from {:?} 🔄🔄🔄",
                        index,
                        std::mem::discriminant(&base_converted.kind)
                    );

                    // If base is a Concat (from tuple return), extract element directly
                    // NOTE: Concat elements are reversed (elem[0] at LSB), so use reversed index
                    if let ExpressionKind::Concat(ref elements) = base_converted.kind {
                        let adjusted_index = elements.len().saturating_sub(1).saturating_sub(index);
                        debug_println!(
                            "🔄🔄🔄 BUG #128 FIX: Concat has {} elements, adjusted_index={} 🔄🔄🔄",
                            elements.len(),
                            adjusted_index
                        );
                        if adjusted_index < elements.len() {
                            return Some(elements[adjusted_index].clone());
                        }
                    }

                    // Fall back to TupleFieldAccess for non-Concat bases
                    return Some(Expression::with_unknown_type(
                        ExpressionKind::TupleFieldAccess {
                            base: Box::new(base_converted),
                            index,
                        },
                    ));
                }

                // Non-numeric field (struct field access)
                Some(Expression::with_unknown_type(ExpressionKind::FieldAccess {
                    base: Box::new(base_converted),
                    field: field.clone(),
                }))
            }

            // BUG FIX #177: Handle Call expressions specially to convert arguments with mir_cache
            // This is critical for blocks like: { let product = fp_mul(a, b); fp_add(product, c) }
            // Without this, `product` (a local let binding) can't be resolved when converting fp_add's args.
            hir::HirExpression::Call(call) => {
                println!(
                    "🔄🔄🔄 BUG #177 FIX: Call '{}' in mir_cache conversion 🔄🔄🔄",
                    call.function
                );

                // Convert arguments using mir_cache for local variable lookups
                let mut arg_exprs: Vec<Expression> = Vec::new();
                for (i, arg) in call.args.iter().enumerate() {
                    match self.convert_hir_expr_with_mir_cache(arg, ctx, mir_cache, depth + 1) {
                        Some(converted) => {
                            println!(
                                "🔄🔄🔄 BUG #177 FIX: Call '{}' arg[{}] converted successfully 🔄🔄🔄",
                                call.function, i
                            );
                            arg_exprs.push(converted);
                        }
                        None => {
                            eprintln!(
                                "    ⚠️  BUG #177: Failed to convert argument {} for call '{}'",
                                i, call.function
                            );
                            return None;
                        }
                    }
                }

                // Build cache key from converted MIR args (stable)
                let cache_key = format!("{}@{:?}", call.function, arg_exprs);
                println!(
                    "🔄🔄🔄 BUG #177 FIX: Call '{}' cache_key = '{}' 🔄🔄🔄",
                    call.function,
                    &cache_key[..cache_key.len().min(200)]
                );

                // Check cache first
                if let Some(cached) = self.module_call_cache.get(&cache_key) {
                    println!(
                        "🔄🔄🔄 BUG #177 FIX: Using CACHED Call result for '{}' 🔄🔄🔄",
                        call.function
                    );
                    return Some(cached.clone());
                }

                // BUG FIX #177: Process the function call directly with pre-converted args.
                // We can't fall back to convert_hir_expr_for_module because it will try to
                // re-convert args and fail on local variables like `product` that are only
                // in mir_cache, not in ctx.var_id_to_name.

                // For non-primitive functions, synthesize as module if needed
                let module_id = if let Some(&existing_id) = self.function_map.get(&call.function) {
                    println!(
                        "🔄🔄🔄 BUG #177 FIX: Using existing module for '{}' (module_id={}) 🔄🔄🔄",
                        call.function, existing_id.0
                    );
                    Some(existing_id)
                } else if let Some(hir) = &self.hir.clone() {
                    if let Some(func) = hir.functions.iter().find(|f| f.name == call.function) {
                        let call_count: usize = func
                            .body
                            .iter()
                            .map(|stmt| self.count_calls_in_statement(stmt))
                            .sum();
                        println!(
                            "🔄🔄🔄 BUG #177 FIX: Function '{}' has {} nested calls 🔄🔄🔄",
                            call.function, call_count
                        );

                        if call_count > MAX_INLINE_CALL_COUNT {
                            println!(
                                "🔄🔄🔄 BUG #177 FIX: Synthesizing '{}' as module 🔄🔄🔄",
                                call.function
                            );
                            Some(self.synthesize_function_as_module(func))
                        } else {
                            None
                        }
                    } else {
                        None
                    }
                } else {
                    None
                };

                if let Some(module_id) = module_id {
                    // Get return type from HIR
                    let return_type = if let Some(hir) = &self.hir {
                        hir.functions
                            .iter()
                            .find(|f| f.name == call.function)
                            .and_then(|f| f.return_type.clone())
                    } else {
                        None
                    };

                    // Pre-allocate signal IDs for result
                    let tuple_size = Self::get_tuple_size_from_hir_type(&return_type);
                    let num_result_signals = if tuple_size > 0 { tuple_size } else { 1 };

                    let mut result_signal_ids = Vec::with_capacity(num_result_signals);
                    for _ in 0..num_result_signals {
                        result_signal_ids.push(self.next_signal_id());
                    }

                    println!(
                        "🔄🔄🔄 BUG #177 FIX: Pre-allocated {} result signal(s) for '{}' 🔄🔄🔄",
                        num_result_signals, call.function
                    );

                    // BUG FIX: Use hir_type_to_frontend_type to get the actual tuple type
                    // instead of hardcoding Bit(32) which loses tuple element widths
                    let frontend_type = Self::hir_type_to_frontend_type(&return_type).unwrap_or(
                        skalp_frontend::types::Type::Bit(skalp_frontend::types::Width::Fixed(32)),
                    );

                    self.pending_module_instances.push((
                        result_signal_ids.clone(),
                        call.function.clone(),
                        module_id,
                        arg_exprs,
                        return_type,
                        frontend_type,
                    ));

                    // Build result expression
                    let result = if result_signal_ids.len() > 1 {
                        let mut concat_elements: Vec<Expression> = result_signal_ids
                            .iter()
                            .map(|sig_id| {
                                Expression::with_unknown_type(ExpressionKind::Ref(LValue::Signal(
                                    *sig_id,
                                )))
                            })
                            .collect();
                        concat_elements.reverse();
                        Expression::with_unknown_type(ExpressionKind::Concat(concat_elements))
                    } else {
                        Expression::with_unknown_type(ExpressionKind::Ref(LValue::Signal(
                            result_signal_ids[0],
                        )))
                    };

                    self.module_call_cache.insert(cache_key, result.clone());
                    return Some(result);
                }

                // Function is not module-synthesized, inline with pre-converted MIR args
                // Use pre-converted arg_exprs instead of falling back to convert_expression
                // which would re-convert HIR args and lose mir_cache context.
                // This is critical for nested function calls within Block expressions where
                // local let bindings are only available in mir_cache.
                let result = self.inline_function_call_with_mir_args(call, &arg_exprs);
                if let Some(ref r) = result {
                    self.module_call_cache.insert(cache_key, r.clone());
                }
                result
            }

            // For other expressions, use normal conversion (which will use module_call_cache)
            _ => self.convert_hir_expr_for_module(expr, ctx, depth),
        }
    }

    /// Convert an HIR expression to MIR in the context of module synthesis
    /// This is a simplified converter that primarily handles variable lookups in module context
    /// and delegates most expression types to the main converter
    fn convert_hir_expr_for_module(
        &mut self,
        expr: &hir::HirExpression,
        ctx: &ModuleSynthesisContext,
        depth: usize,
    ) -> Option<Expression> {
        // Guard against infinite recursion
        if depth > 100 {
            debug_eprintln!("    ⚠️  Module expr conversion depth exceeded 100");
            return None;
        }

        println!(
            "🧩🧩🧩 MODULE_EXPR: Converting {:?} at depth {} 🧩🧩🧩",
            std::mem::discriminant(expr),
            depth
        );

        match expr {
            // Literal values - direct conversion
            // BUG #125 FIX: Preserve proper type for BitVector literals instead of Unknown
            // This is critical for Concat width calculation in parallel mux arms
            hir::HirExpression::Literal(lit) => {
                let value = self.convert_literal(lit)?;
                let ty = match &value {
                    Value::BitVector { width, .. } => {
                        Type::Bit(skalp_frontend::types::Width::Fixed(*width as u32))
                    }
                    Value::Integer(_) => Type::Int(skalp_frontend::types::Width::Fixed(32)),
                    // fp32 floats are represented as 32-bit values in hardware
                    Value::Float(_) => Type::Bit(skalp_frontend::types::Width::Fixed(32)),
                    _ => Type::Unknown,
                };
                Some(Expression::new(ExpressionKind::Literal(value), ty))
            }

            // Variable reference - lookup in context (params or let bindings)
            // This is the key case we need to handle specially for module synthesis
            hir::HirExpression::Variable(var) => {
                // DEBUG: Trace Variable(16) lookups
                if var.0 == 16 {
                    println!(
                        "[BUG #192 DEBUG] convert_hir_expr_for_module Variable(16), placeholder_map size={}, contains_16={}",
                        self.placeholder_signal_to_entity_output.len(),
                        self.placeholder_signal_to_entity_output.contains_key(var)
                    );
                }
                // BUG #190 FIX: Check if this variable is a placeholder connected to an entity output
                // This is critical for FP operations where the 'result' signal is passed to
                // an entity (like FpAdd) and later returned. The placeholder signal maps to
                // the entity's output wire.
                // BUG #201 FIX: Use stored signal type instead of unknown
                if let Some((output_signal_id, signal_type)) =
                    self.placeholder_signal_to_entity_output.get(var)
                {
                    println!(
                        "[BUG #190 FIX] Variable {:?} is placeholder -> returning entity output signal {:?} (type {:?})",
                        var, output_signal_id, signal_type
                    );
                    return Some(Expression::new(
                        ExpressionKind::Ref(LValue::Signal(*output_signal_id)),
                        signal_type.clone(),
                    ));
                }

                // Get the variable name from our var_id_to_name map
                debug_println!("🔗🔗🔗 MODULE VAR: Looking up Variable({}) 🔗🔗🔗", var.0);
                let name = ctx.var_id_to_name.get(var).cloned();
                if let Some(ref n) = name {
                    debug_println!("🔗🔗🔗 MODULE VAR: Variable({}) -> '{}' 🔗🔗🔗", var.0, n);
                    debug_eprintln!("    📌 Resolved Variable({}) -> '{}'", var.0, n);

                    // Check if it's a parameter (maps to input port)
                    if let Some(&port_id) = ctx.param_to_port.get(n) {
                        println!(
                            "🔗🔗🔗 MODULE VAR: '{}' → Input port (id={}) 🔗🔗🔗",
                            n, port_id.0
                        );
                        return Some(Expression::with_unknown_type(ExpressionKind::Ref(
                            LValue::Port(port_id),
                        )));
                    }
                }

                // BUG FIX #85: Look up by VariableId, not by name (avoids collisions)
                if let Some(&signal_id) = ctx.var_to_signal.get(var) {
                    let name_str = name.as_deref().unwrap_or("?");
                    println!(
                        "🔗🔗🔗 MODULE VAR: Variable({}) '{}' → Internal signal (id={}) 🔗🔗🔗",
                        var.0, name_str, signal_id.0
                    );
                    return Some(Expression::with_unknown_type(ExpressionKind::Ref(
                        LValue::Signal(signal_id),
                    )));
                }

                // BUG FIX #130 Part 3: Check tuple_temp_to_mir for tuple temp variables
                // These are created by tuple destructuring: let (a, b) = func()
                // The tuple temp holds the Concat of function outputs
                if let Some(tuple_expr) = ctx.tuple_temp_to_mir.get(var) {
                    let name_str = name.as_deref().unwrap_or("?");
                    debug_println!("🔗🔗🔗 MODULE VAR: Variable({}) '{}' found in tuple_temp_to_mir -> {:?} 🔗🔗🔗",
                             var.0, name_str, tuple_expr.kind);
                    return Some(tuple_expr.clone());
                }

                let name_str = name.as_deref().unwrap_or("?");
                debug_println!("🔗🔗🔗 MODULE VAR: ⚠️ Variable({}) '{}' not found in param_to_port or var_to_signal! (ctx.func='{}') 🔗🔗🔗", var.0, name_str, ctx.func_name);

                // BUG #110: Debug - print what IS in var_to_signal
                debug_println!("🔗🔗🔗 BUG #118 DEBUG: ctx.func='{}' has var_to_signal with {} entries: 🔗🔗🔗", ctx.func_name, ctx.var_to_signal.len());
                for (vid, sid) in &ctx.var_to_signal {
                    let vname = ctx
                        .var_id_to_name
                        .get(vid)
                        .map(|s| s.as_str())
                        .unwrap_or("?");
                    println!(
                        "🔗🔗🔗   Variable({}) '{}' -> Signal({}) 🔗🔗🔗",
                        vid.0, vname, sid.0
                    );
                }

                // Fallback: try to use the main expression converter
                debug_println!("🔗🔗🔗 MODULE VAR: ⚠️ Variable not found in module context - using fallback 🔗🔗🔗");
                self.convert_expression(expr, depth)
            }

            // BUG FIX #85: Handle GenericParam (how the HIR represents function parameter references)
            hir::HirExpression::GenericParam(param_name) => {
                println!(
                    "🔗🔗🔗 MODULE GENERIC_PARAM: Looking up '{}' 🔗🔗🔗",
                    param_name
                );

                // GenericParam is how HIR represents function parameter references
                // Map directly to the input port for this parameter
                if let Some(&port_id) = ctx.param_to_port.get(param_name) {
                    println!(
                        "🔗🔗🔗 MODULE GENERIC_PARAM: '{}' → Input port (id={}) 🔗🔗🔗",
                        param_name, port_id.0
                    );
                    return Some(Expression::with_unknown_type(ExpressionKind::Ref(
                        LValue::Port(port_id),
                    )));
                }

                // Note: GenericParam should only be function parameters, not let bindings
                // Let bindings are accessed via Variable(VariableId), not GenericParam(name)
                debug_println!("🔗🔗🔗 MODULE GENERIC_PARAM: ⚠️ '{}' not found in param_to_port! Using fallback. 🔗🔗🔗", param_name);
                self.convert_expression(expr, depth)
            }

            // Function calls - check for primitive FP operations
            hir::HirExpression::Call(call) => {
                eprintln!(
                    "    📞 Module synthesis: Converting call to '{}'",
                    call.function
                );

                // BUG FIX #125: Check cache first to prevent duplicate module instances
                // This is critical for tuple destructuring like `let (a, b, c) = func();`
                // where multiple FieldAccess expressions reference the same Call
                //
                // BUG FIX #126: For module-synthesized functions, the cache key must be based on
                // the CONVERTED MIR arg signals, not the HIR args. This is because HIR substitution
                // can change Variable(72) to Range(data1[31:0]) while they refer to the same signal.
                // We convert args to MIR FIRST, then build the cache key from the MIR signals.
                //
                // For inlined functions (not module-synthesized), we still use HIR args to distinguish
                // different calls like fp_div(fp_sub(a,b),c) vs fp_div(fp_add(a,b),c).

                // Convert arguments in module context to get stable MIR representations
                let mut arg_exprs: Vec<Expression> = Vec::new();
                for arg in &call.args {
                    match self.convert_hir_expr_for_module(arg, ctx, depth + 1) {
                        Some(converted) => arg_exprs.push(converted),
                        None => {
                            eprintln!(
                                "    ⚠️  Failed to convert argument for call '{}'",
                                call.function
                            );
                            return None;
                        }
                    }
                }

                // BUG FIX #126: Build cache key from converted MIR args (stable) instead of HIR args
                let cache_key = format!("{}@{:?}", call.function, arg_exprs);
                println!(
                    "📞📞📞 BUG #126 DEBUG: Call '{}' MIR cache_key = '{}' 📞📞📞",
                    call.function,
                    &cache_key[..cache_key.len().min(200)]
                );
                if let Some(cached) = self.module_call_cache.get(&cache_key) {
                    println!(
                        "📞📞📞 BUG #125 FIX: Using CACHED Call result for '{}' 📞📞📞",
                        call.function
                    );
                    return Some(cached.clone());
                }

                // Debug: Print what arguments look like
                for (arg_idx, arg) in call.args.iter().enumerate() {
                    eprintln!(
                        "    📞 Call '{}' arg[{}]: {:?}",
                        call.function,
                        arg_idx,
                        std::mem::discriminant(arg)
                    );
                    if let hir::HirExpression::Variable(var_id) = arg {
                        eprintln!(
                            "    📞 Call '{}' arg[{}]: Variable({})",
                            call.function, arg_idx, var_id.0
                        );
                        if let Some(name) = ctx.var_id_to_name.get(var_id) {
                            eprintln!(
                                "    📞 Call '{}' arg[{}]: Variable({}) → '{}'",
                                call.function, arg_idx, var_id.0, name
                            );
                        } else {
                            eprintln!("    📞 Call '{}' arg[{}]: Variable({}) → NOT FOUND IN var_id_to_name!", call.function, arg_idx, var_id.0);
                            eprintln!(
                                "    📞 Available var_id_to_name: {:?}",
                                ctx.var_id_to_name
                                    .iter()
                                    .map(|(k, v)| (k.0, v.as_str()))
                                    .collect::<Vec<_>>()
                            );
                        }
                    }
                }

                // Check if this is a primitive FP operation (Two-Tier TIER 1)
                // For non-primitive functions, use the arg_exprs already converted above
                // (at the start of this Call handler for the cache key)
                // BUG FIX #85: The old fallback to convert_expression created new VariableIds
                // that didn't match the module's let binding VariableIds. Now we convert
                // arguments using the module context to preserve correct Variable references.
                // BUG FIX #126: We already converted args to arg_exprs for the cache key above,
                // so we reuse that instead of converting again (which would shadow the variable).
                println!("    🔧 Non-primitive call '{}' in module synthesis - using pre-converted arg_exprs", call.function);

                // BUG FIX #85: Check if this function should be synthesized as a module
                // If so, synthesize it first (if not already done), then create pending instance
                let module_id = if let Some(&existing_id) = self.function_map.get(&call.function) {
                    // Already synthesized
                    println!(
                        "    🏗️ Using existing module for '{}' (module_id={})",
                        call.function, existing_id.0
                    );
                    Some(existing_id)
                } else {
                    // Check if this function should be synthesized as a module
                    if let Some(hir) = &self.hir.clone() {
                        if let Some(func) = hir.functions.iter().find(|f| f.name == call.function) {
                            // Count nested calls to determine if this should be a module
                            let call_count: usize = func
                                .body
                                .iter()
                                .map(|stmt| self.count_calls_in_statement(stmt))
                                .sum();
                            println!(
                                "    📊 Function '{}' has {} nested calls (threshold={})",
                                call.function, call_count, MAX_INLINE_CALL_COUNT
                            );

                            if call_count > MAX_INLINE_CALL_COUNT {
                                // Synthesize as a module
                                println!(
                                    "    🔧 Synthesizing '{}' as module (exceeds call threshold)",
                                    call.function
                                );
                                let new_module_id = self.synthesize_function_as_module(func);
                                Some(new_module_id)
                            } else {
                                None // Should be inlined, not module
                            }
                        } else {
                            None
                        }
                    } else {
                        None
                    }
                };

                println!(
                    "    📌 Function '{}': module_id = {:?}",
                    call.function,
                    module_id.map(|m| m.0)
                );

                if let Some(module_id) = module_id {
                    // Get return type from HIR
                    let return_type = if let Some(hir) = &self.hir {
                        hir.functions
                            .iter()
                            .find(|f| f.name == call.function)
                            .and_then(|f| f.return_type.clone())
                    } else {
                        None
                    };

                    // BUG FIX #92: Pre-allocate signal IDs for ALL tuple elements
                    let tuple_size = Self::get_tuple_size_from_hir_type(&return_type);
                    let num_result_signals = if tuple_size > 0 { tuple_size } else { 1 };

                    let mut result_signal_ids = Vec::with_capacity(num_result_signals);
                    for _ in 0..num_result_signals {
                        result_signal_ids.push(self.next_signal_id());
                    }

                    println!(
                        "    📌 Pre-allocated {} result signal(s) {:?} for module '{}'",
                        num_result_signals,
                        result_signal_ids.iter().map(|s| s.0).collect::<Vec<_>>(),
                        call.function
                    );

                    // BUG FIX: Use hir_type_to_frontend_type to get the actual tuple type
                    // instead of hardcoding Bit(32) which loses tuple element widths
                    let frontend_type = Self::hir_type_to_frontend_type(&return_type).unwrap_or(
                        skalp_frontend::types::Type::Bit(skalp_frontend::types::Width::Fixed(32)),
                    );

                    self.pending_module_instances.push((
                        result_signal_ids.clone(),
                        call.function.clone(),
                        module_id,
                        arg_exprs,
                        return_type,
                        frontend_type.clone(),
                    ));

                    // BUG FIX #92: For tuple returns, return a Concat of all result signals
                    if result_signal_ids.len() > 1 {
                        // BUG FIX #129: Use REVERSED order to match TupleLiteral convention
                        // TupleLiteral reverses elements so elem[0] ends up at LSB.
                        // We need to do the same for module Call results so that
                        // FieldAccess extraction uses consistent reversed indexing.
                        let mut concat_elements: Vec<Expression> = result_signal_ids
                            .iter()
                            .map(|sig_id| {
                                Expression::with_unknown_type(ExpressionKind::Ref(LValue::Signal(
                                    *sig_id,
                                )))
                            })
                            .collect();
                        concat_elements.reverse(); // BUG FIX #129: Match TupleLiteral convention
                        println!("    📌 Returning Concat of {} result signals for tuple (REVERSED to match TupleLiteral)", concat_elements.len());
                        let result =
                            Expression::with_unknown_type(ExpressionKind::Concat(concat_elements));
                        // BUG FIX #125: Cache the result for subsequent accesses
                        self.module_call_cache
                            .insert(cache_key.clone(), result.clone());
                        println!(
                            "📞📞📞 BUG #125 FIX: CACHING Call result for '{}' (tuple) 📞📞📞",
                            call.function
                        );
                        return Some(result);
                    } else {
                        let result = Expression::new(
                            ExpressionKind::Ref(LValue::Signal(result_signal_ids[0])),
                            frontend_type.clone(),
                        );
                        // BUG FIX #125: Cache the result for subsequent accesses
                        self.module_call_cache
                            .insert(cache_key.clone(), result.clone());
                        println!(
                            "📞📞📞 BUG #125 FIX: CACHING Call result for '{}' (single) 📞📞📞",
                            call.function
                        );
                        return Some(result);
                    }
                }

                // BUG FIX #91: If not a module synthesis case, INLINE within module context
                // Previously we fell back to convert_expression which lost the module context.
                // Now we inline the function body and convert it within the module context.
                println!(
                    "    🔄🔄🔄 BUG91_INLINE: Inlining '{}' within module context 🔄🔄🔄",
                    call.function
                );

                // BUG FIX #118: Use inline_function_call_to_hir_with_lets instead of
                // substitute_expression_with_var_map. The old approach only substituted
                // parameters but NOT let bindings. This caused 3-level nesting bugs where
                // Variables from an inlined function's let bindings (like discriminant, sqrt_disc)
                // couldn't be found in the outer function's module context.
                //
                // inline_function_call_to_hir_with_lets properly handles:
                // 1. Parameter substitution (e.g., a, b, c -> call arguments)
                // 2. Let binding substitution (e.g., discriminant -> its computed value)
                // 3. Nested function calls recursively
                //
                // This produces a fully-expanded HIR expression with NO Variable references,
                // which can then be safely converted within the module context.
                if let Some(inlined_hir) = self.inline_function_call_to_hir_with_lets(call) {
                    println!("    🔄🔄🔄 BUG118_INLINE: inline_function_call_to_hir_with_lets SUCCESS for '{}', type: {:?}",
                             call.function, std::mem::discriminant(&inlined_hir));

                    // Convert the fully-inlined HIR expression within module context
                    return self.convert_hir_expr_for_module(&inlined_hir, ctx, depth + 1);
                } else {
                    println!("    🔄🔄🔄 BUG118_INLINE: inline_function_call_to_hir_with_lets FAILED for '{}'!", call.function);
                }

                // Last resort fallback if inlining fails
                println!(
                    "    ⚠️ BUG91_INLINE: Falling back to regular convert_expression for '{}'",
                    call.function
                );
                self.convert_expression(expr, depth)
            }

            // Cast expressions - pass through (hardware synthesis handles bit reinterpretation)
            // Note: The Cast's target_type (e.g., fp32) is important for type inference
            // in later stages. We preserve Casts as-is when needed for floating-point ops.
            hir::HirExpression::Cast(cast) => {
                // Check if this is an fp32/fp16 cast - these are important for FP operations
                let target_type = self.convert_type(&cast.target_type);
                if matches!(target_type, DataType::Float32 | DataType::Float16) {
                    println!(
                        "🎭🎭🎭 MODULE_CAST: FP Cast to {:?} - preserving 🎭🎭🎭",
                        target_type
                    );
                    // For FP casts, we need to preserve the cast for proper type inference
                    // But since Cast isn't handled in mir_to_sir, just pass through the inner
                    // The BUG FIX #87 in mir_to_sir needs another approach
                    let inner = self.convert_hir_expr_for_module(&cast.expr, ctx, depth + 1)?;
                    Some(inner)
                } else {
                    // For non-FP casts, just pass through
                    let inner = self.convert_hir_expr_for_module(&cast.expr, ctx, depth + 1)?;
                    Some(inner)
                }
            }

            // If expressions - convert to conditional (mux)
            hir::HirExpression::If(if_expr) => {
                let cond = self.convert_hir_expr_for_module(&if_expr.condition, ctx, depth + 1)?;
                let then_result =
                    self.convert_hir_expr_for_module(&if_expr.then_expr, ctx, depth + 1)?;
                let else_result =
                    self.convert_hir_expr_for_module(&if_expr.else_expr, ctx, depth + 1)?;

                Some(Expression::with_unknown_type(ExpressionKind::Conditional {
                    cond: Box::new(cond),
                    then_expr: Box::new(then_result),
                    else_expr: Box::new(else_result),
                }))
            }

            // Binary operations - convert operands in module context
            // BUG FIX #FP_TRAIT: Check for trait-based operator resolution FIRST
            // This is critical for FP operations like (a as fp32) + (b as fp32)
            // Without this, FP ops use primitive integer +/*/ instead of IEEE 754
            hir::HirExpression::Binary(bin_expr) => {
                eprintln!(
                    "[MODULE_BINARY_DEBUG] Processing Binary expression: op={:?}, left={:?}",
                    bin_expr.op,
                    std::mem::discriminant(&*bin_expr.left)
                );
                // TRAIT-BASED OPERATOR RESOLUTION (Operator Overloading)
                // Try to resolve the operator through trait implementations first.
                // If a trait impl exists (e.g., `impl Add for fp32`), inline the trait method
                // body instead of using a primitive binary operation.
                if let Some((type_name, trait_name, method_name)) =
                    self.try_resolve_trait_operator(&bin_expr.op, &bin_expr.left)
                {
                    println!(
                        "[MODULE_TRAIT_OP] Attempting to inline {}::{} for type '{}' (op: {:?})",
                        trait_name, method_name, type_name, bin_expr.op
                    );

                    // Try to inline the trait method body
                    if let Some(result) = self.inline_trait_method(
                        &type_name,
                        trait_name,
                        method_name,
                        &bin_expr.left,
                        &bin_expr.right,
                    ) {
                        println!(
                            "[MODULE_TRAIT_OP] Successfully inlined {}::{} for type '{}'",
                            trait_name, method_name, type_name
                        );
                        return Some(result);
                    }

                    // If inlining fails, fall through to primitive binary operation
                    println!(
                        "[MODULE_TRAIT_OP] Failed to inline {}::{}, falling back to primitive op",
                        trait_name, method_name
                    );
                }

                // Fall back to primitive binary operation
                let left = self.convert_hir_expr_for_module(&bin_expr.left, ctx, depth + 1)?;
                let right = self.convert_hir_expr_for_module(&bin_expr.right, ctx, depth + 1)?;
                let op = self.convert_binary_op(&bin_expr.op, &bin_expr.left);
                Some(Expression::with_unknown_type(ExpressionKind::Binary {
                    op,
                    left: Box::new(left),
                    right: Box::new(right),
                }))
            }

            // Unary operations - convert operand in module context
            // Without this, Unary falls back to convert_expression which loses module context
            // and causes GenericParams to resolve to 0 instead of port references.
            // Note: FP negation is handled via stdlib Neg trait impl, not special compiler handling.
            hir::HirExpression::Unary(unary_expr) => {
                let operand =
                    self.convert_hir_expr_for_module(&unary_expr.operand, ctx, depth + 1)?;
                let op = self.convert_unary_op(&unary_expr.op);

                Some(Expression::with_unknown_type(ExpressionKind::Unary {
                    op,
                    operand: Box::new(operand),
                }))
            }

            // Tuple literals - convert to concatenation
            // BUG FIX #91: Reverse element order so first element is at LSB position.
            // Tests expect little-endian tuple layout: bytes[0..4]=elem0, bytes[4..8]=elem1, etc.
            // Concat puts first element at MSB, so we reverse to get elem0 at LSB.
            hir::HirExpression::TupleLiteral(elements) => {
                println!(
                    "🔵🔵🔵 BUG #126 TUPLE_LITERAL: Converting {} elements 🔵🔵🔵",
                    elements.len()
                );
                let mut converted = Vec::new();
                for (idx, elem) in elements.iter().enumerate() {
                    println!(
                        "🔵🔵🔵 BUG #126 TUPLE_LITERAL elem[{}]: HIR={:?} 🔵🔵🔵",
                        idx,
                        std::mem::discriminant(elem)
                    );
                    if let hir::HirExpression::Variable(var_id) = elem {
                        debug_println!("🔵🔵🔵 BUG #126 TUPLE_LITERAL elem[{}]: Variable({}), name={:?} 🔵🔵🔵",
                                 idx, var_id.0, ctx.var_id_to_name.get(var_id));
                        if let Some(name) = ctx.var_id_to_name.get(var_id) {
                            debug_println!("🔵🔵🔵 BUG #126 TUPLE_LITERAL elem[{}]: var_to_signal[{}]={:?} 🔵🔵🔵",
                                     idx, var_id.0, ctx.var_to_signal.get(var_id).map(|s| s.0));
                        }
                    }
                    let conv = self.convert_hir_expr_for_module(elem, ctx, depth + 1)?;
                    println!(
                        "🔵🔵🔵 BUG #126 TUPLE_LITERAL elem[{}]: MIR={:?} 🔵🔵🔵",
                        idx, conv.kind
                    );
                    converted.push(conv);
                }
                converted.reverse(); // Reverse so elem[0] ends up at LSB
                println!(
                    "🔵🔵🔵 BUG #126 TUPLE_LITERAL: After reverse, {} converted elements 🔵🔵🔵",
                    converted.len()
                );
                Some(Expression::with_unknown_type(ExpressionKind::Concat(
                    converted,
                )))
            }

            // Concatenation
            hir::HirExpression::Concat(parts) => {
                let mut converted = Vec::new();
                for (idx, part) in parts.iter().enumerate() {
                    if let Some(conv) = self.convert_hir_expr_for_module(part, ctx, depth + 1) {
                        converted.push(conv);
                    } else {
                        eprintln!(
                            "🔴🔴🔴 BUG #127 MODULE: Concat element[{}] conversion FAILED! Type: {:?} 🔴🔴🔴",
                            idx,
                            std::mem::discriminant(part)
                        );
                        // Print more info about the element
                        match part {
                            hir::HirExpression::Unary(unary) => {
                                eprintln!(
                                    "🔴🔴🔴   MODULE Unary op: {:?}, operand type: {:?} 🔴🔴🔴",
                                    unary.op,
                                    std::mem::discriminant(&*unary.operand)
                                );
                            }
                            hir::HirExpression::Range(base, high, low) => {
                                eprintln!(
                                    "🔴🔴🔴   MODULE Range: base={:?}, high={:?}, low={:?} 🔴🔴🔴",
                                    std::mem::discriminant(&**base),
                                    std::mem::discriminant(&**high),
                                    std::mem::discriminant(&**low)
                                );
                            }
                            _ => {}
                        }
                        return None;
                    }
                }
                Some(Expression::with_unknown_type(ExpressionKind::Concat(
                    converted,
                )))
            }

            // BUG FIX #85: Handle Match expressions in module context to prevent duplicate instances
            // Without this, Match falls back to convert_expression -> convert_match_to_conditionals
            // which triggers HYBRID mechanism and creates duplicate module instances
            hir::HirExpression::Match(match_expr) => {
                println!("🎯🎯🎯 MODULE_MATCH: Converting Match with {} arms in module context, mux_style={:?} 🎯🎯🎯",
                         match_expr.arms.len(), match_expr.mux_style);

                // Choose mux generation strategy based on intent/attribute
                match match_expr.mux_style {
                    hir::MuxStyle::Parallel => {
                        debug_println!("[PARALLEL_MUX_MODULE] Converting {} arms to OR-of-ANDs (module context)", match_expr.arms.len());
                        // Generate OR-of-ANDs in module context
                        self.convert_match_to_parallel_mux_for_module(
                            &match_expr.expr,
                            &match_expr.arms,
                            ctx,
                            depth + 1,
                        )
                    }
                    hir::MuxStyle::Priority | hir::MuxStyle::Auto => {
                        // Convert match to nested conditionals, staying in module context
                        self.convert_match_to_conditionals_for_module(
                            &match_expr.expr,
                            &match_expr.arms,
                            ctx,
                            depth + 1,
                        )
                    }
                }
            }

            // BUG FIX #85 + #91: Handle Block expressions in module context
            // BUG FIX #91: When a Block comes from an inlined function (e.g., vec3_add_op),
            // its let statements define variables that result_expr references.
            // We need to substitute the let binding values directly into result_expr
            // before converting, since we can't easily add new signals from here.
            // BUG FIX #92: Also track by VariableId for proper substitution
            hir::HirExpression::Block {
                statements,
                result_expr,
            } => {
                println!("🧱🧱🧱 MODULE_BLOCK: Converting Block with {} statements in module context 🧱🧱🧱", statements.len());

                // BUG FIX #86: First try the mutable variable pattern transformation
                // This handles blocks like:
                //   { let mut count = 0; let mut temp = value;
                //     if (cond) { count = count + 16; temp = temp << 16; }
                //     count }
                // Transform to conditional expressions BEFORE processing.
                println!("🟢🟢🟢 CALLSITE2: About to call try_transform from convert_hir_expr_for_module Block 🟢🟢🟢");
                if let Some(transformed_expr) =
                    self.try_transform_block_mutable_vars(statements, result_expr)
                {
                    debug_println!("[BUG #86] MODULE_BLOCK: transformed mutable variable pattern");
                    return self.convert_hir_expr_for_module(&transformed_expr, ctx, depth + 1);
                }

                // BUG FIX #126: Instead of building HIR substitution maps and substituting back,
                // we should CONVERT let statements to MIR signals and add them to ctx.var_to_signal.
                // This way, when result_expr references these Variables, they're found directly
                // without needing HIR substitution (which changes the Call args and breaks caching).
                //
                // Build maps from both name and VariableId to substituted values (for simple cases)
                // BUG FIX #92: Need both maps because Variables use var_id, not name
                let mut let_substitutions: IndexMap<String, hir::HirExpression> = IndexMap::new();
                let mut var_id_to_value: IndexMap<hir::VariableId, hir::HirExpression> =
                    IndexMap::new();

                // BUG FIX #126: Also track converted MIR expressions by VariableId
                let mut var_id_to_mir: IndexMap<hir::VariableId, Expression> = IndexMap::new();

                for stmt in statements {
                    if let hir::HirStatement::Let(let_stmt) = stmt {
                        println!(
                            "🧱🧱🧱 MODULE_BLOCK: Processing let '{}' (id={}) 🧱🧱🧱",
                            let_stmt.name, let_stmt.id.0
                        );

                        // BUG #200 FIX: Check if this is an entity struct literal BEFORE substitution
                        // Entity struct literal fields should NOT be further substituted by VariableId,
                        // as this causes over-expansion: Variable(a_fp32) -> (a_fp16 as fp32) -> (a[15:0] as fp16 as fp32)
                        // which results in incorrect port connections like var_12[15:0] instead of proper signal refs.
                        let is_entity_struct = if let hir::HirExpression::StructLiteral(
                            struct_lit,
                        ) = &let_stmt.value
                        {
                            self.hir.as_ref().is_some_and(|hir| {
                                hir.entities.iter().any(|e| e.name == struct_lit.type_name)
                            })
                        } else {
                            false
                        };

                        // Substitute any previous let bindings in this value (name-based substitution)
                        // This handles parameter substitution like self -> Variable(a_fp32_id)
                        let mut substituted_value =
                            self.substitute_hir_expr_with_map(&let_stmt.value, &let_substitutions);

                        // BUG FIX #92: Also substitute by VariableId for nested references
                        // BUG #200 FIX: Skip VariableId substitution for entity struct literals
                        // to prevent over-expansion of entity port connections
                        if !is_entity_struct {
                            substituted_value = self
                                .substitute_var_ids_in_expr(&substituted_value, &var_id_to_value);
                        } else {
                            println!(
                                "🔶🔶🔶 BUG #200 FIX: Skipping var_id substitution for entity struct literal '{}' 🔶🔶🔶",
                                let_stmt.name
                            );
                        }

                        // BUG #191 FIX: Handle entity instantiation via StructLiteral in module context
                        // When a let statement value is a StructLiteral for an entity (like FpAdd),
                        // we need to process it through convert_statement to create the entity instance
                        // and set up the placeholder signal to entity output mappings.
                        // This is critical for FP trait implementations that create entity instances.
                        if let hir::HirExpression::StructLiteral(struct_lit) = &substituted_value {
                            println!(
                                "🔶🔶🔶 BUG #191: MODULE_BLOCK found StructLiteral '{}', checking for entity 🔶🔶🔶",
                                struct_lit.type_name
                            );
                            // Check if this struct literal is an entity instantiation
                            let is_entity = self.hir.as_ref().is_some_and(|hir| {
                                hir.entities.iter().any(|e| e.name == struct_lit.type_name)
                            });
                            if is_entity {
                                println!(
                                    "🔶🔶🔶 BUG #191: StructLiteral '{}' IS an entity, delegating to convert_statement 🔶🔶🔶",
                                    struct_lit.type_name
                                );
                                // Delegate to convert_statement which properly handles entity instantiation
                                // This will create the entity instance and set up placeholder mappings
                                let temp_let = hir::HirLetStatement {
                                    id: let_stmt.id,
                                    name: let_stmt.name.clone(),
                                    value: substituted_value.clone(),
                                    var_type: let_stmt.var_type.clone(),
                                    mutable: let_stmt.mutable,
                                };
                                let _ = self.convert_statement(&hir::HirStatement::Let(temp_let));
                                // Skip normal processing - convert_statement handled it
                                let_substitutions
                                    .insert(let_stmt.name.clone(), substituted_value.clone());
                                var_id_to_value.insert(let_stmt.id, substituted_value);
                                continue;
                            }
                        }

                        // BUG FIX #126: Try to convert this let value to MIR RIGHT NOW
                        // This ensures function calls are only processed once (with caching)
                        // BUG FIX #128: Use convert_hir_expr_with_mir_cache to handle FieldAccess
                        // referencing earlier let bindings (e.g., `let valid = _tuple_tmp_49.0`)
                        if let Some(mir_expr) = self.convert_hir_expr_with_mir_cache(
                            &substituted_value,
                            ctx,
                            &var_id_to_mir,
                            depth + 1,
                        ) {
                            println!("🧱🧱🧱 BUG #126 FIX: Converted let '{}' (id={}) to MIR expr 🧱🧱🧱",
                                     let_stmt.name, let_stmt.id.0);
                            var_id_to_mir.insert(let_stmt.id, mir_expr);
                        }

                        let_substitutions.insert(let_stmt.name.clone(), substituted_value.clone());
                        var_id_to_value.insert(let_stmt.id, substituted_value);
                        println!("🧱🧱🧱 MODULE_BLOCK: Added '{}' (id={}) to both substitution maps 🧱🧱🧱",
                                 let_stmt.name, let_stmt.id.0);
                    }
                }

                // BUG FIX #126: Instead of HIR substitution (which changes Call args and breaks caching),
                // convert result_expr with a helper that uses pre-converted MIR expressions from var_id_to_mir.
                //
                // For Variables that have MIR expressions in var_id_to_mir, use them directly.
                // For other expressions, fall back to normal conversion (which will use the cached Calls).
                println!(
                    "🧱🧱🧱 MODULE_BLOCK: result_expr type: {:?} 🧱🧱🧱",
                    std::mem::discriminant(&**result_expr)
                );
                println!(
                    "🧱🧱🧱 BUG #126 FIX: var_id_to_mir has {} entries 🧱🧱🧱",
                    var_id_to_mir.len()
                );
                for vid in var_id_to_mir.keys() {
                    let name = ctx
                        .var_id_to_name
                        .get(vid)
                        .map(|s| s.as_str())
                        .unwrap_or("?");
                    println!(
                        "🧱🧱🧱 BUG #126 FIX: var_id_to_mir[{}] = '{}' 🧱🧱🧱",
                        vid.0, name
                    );
                }

                // Convert result_expr, using var_id_to_mir for Variables when available
                let result = self.convert_hir_expr_with_mir_cache(
                    result_expr,
                    ctx,
                    &var_id_to_mir,
                    depth + 1,
                );
                println!(
                    "🧱🧱🧱 MODULE_BLOCK: conversion complete, result is {} 🧱🧱🧱",
                    if result.is_some() { "Some" } else { "NONE" }
                );
                result
            }

            // BUG FIX #85: Handle FieldAccess for tuple destructuring in module context
            // This is critical for `let (valid, x1, x2) = quadratic_solve(a, b, c);`
            // Each let binding becomes FieldAccess { base: Call, field: "0"/"1"/"2" }
            hir::HirExpression::FieldAccess { base, field } => {
                println!(
                    "📎📎📎 MODULE_FIELD_ACCESS: field='{}' on base {:?} 📎📎📎",
                    field,
                    std::mem::discriminant(&**base)
                );

                // BUG FIX #125: Cache Call results to prevent duplicate module instances
                // When processing `let (a, b, c) = func();`, each variable (a, b, c) becomes
                // FieldAccess { base: Call(func), field: "0"/"1"/"2" }. Without caching,
                // each FieldAccess triggers a new Call conversion, creating duplicate instances.
                // BUG FIX #126: Cache key must include ACTUAL arguments, not just arg count!
                // Previously "fp_div@2" was same for fp_div(fp_sub(a,b),c) and fp_div(fp_add(a,b),c)
                let base_converted = if let hir::HirExpression::Call(call) = &**base {
                    // Create a unique cache key from the Call - must include actual args
                    let cache_key = format!("{}@{:?}", call.function, call.args);

                    if let Some(cached) = self.module_call_cache.get(&cache_key) {
                        println!(
                            "📎📎📎 BUG #125 FIX: Using CACHED result for '{}' 📎📎📎",
                            call.function
                        );
                        cached.clone()
                    } else {
                        // First time seeing this Call - process and cache
                        println!(
                            "📎📎📎 BUG #125 FIX: First time processing '{}', will cache 📎📎📎",
                            call.function
                        );
                        let result = self.convert_hir_expr_for_module(base, ctx, depth + 1)?;
                        self.module_call_cache.insert(cache_key, result.clone());
                        result
                    }
                } else {
                    // Not a Call - process normally
                    self.convert_hir_expr_for_module(base, ctx, depth + 1)?
                };

                // For numeric fields (tuple element access), extract the element
                if let Ok(index) = field.parse::<usize>() {
                    println!(
                        "📎📎📎 MODULE_FIELD_ACCESS: Tuple element {} extraction 📎📎📎",
                        index
                    );

                    // BUG FIX #125: If base_converted is already a Concat, extract element directly
                    // This happens when we cache a Call result that returns a tuple (Concat of signals)
                    // Instead of creating TupleFieldAccess { base: Concat([...]), index } which
                    // mir_to_sir handles with bit slicing, we directly return elements[index].
                    //
                    // BUG FIX #126 CORRECTION: The Concat was created with elements REVERSED
                    // (see line 12433: converted.reverse()) so that elem[0] ends up at LSB in hardware.
                    // When extracting tuple element at index N from Concat([elemN-1, ..., elem1, elem0]),
                    // we need to use the REVERSED index: len - 1 - index.
                    // Example: tuple (a, b, c) -> Concat([c, b, a])
                    //   FieldAccess(".0") wants 'a' -> adjusted_index = 2 -> elements[2] = a ✓
                    //   FieldAccess(".1") wants 'b' -> adjusted_index = 1 -> elements[1] = b ✓
                    //   FieldAccess(".2") wants 'c' -> adjusted_index = 0 -> elements[0] = c ✓
                    if let ExpressionKind::Concat(elements) = &base_converted.kind {
                        if index < elements.len() {
                            let adjusted_index = elements.len() - 1 - index;
                            println!("📎📎📎 BUG #126 FIX: Direct Concat extraction: logical index {} -> adjusted {} of {} 📎📎📎",
                                     index, adjusted_index, elements.len());
                            return Some(elements[adjusted_index].clone());
                        } else {
                            eprintln!("📎📎📎 BUG #125 ERROR: Index {} out of bounds for Concat with {} elements 📎📎📎",
                                     index, elements.len());
                            return None;
                        }
                    }

                    // Fallback: Create a TupleFieldAccess for non-Concat bases
                    // This handles cases where base is a Signal reference to a module result
                    return Some(Expression::with_unknown_type(
                        ExpressionKind::TupleFieldAccess {
                            base: Box::new(base_converted),
                            index,
                        },
                    ));
                }

                // For named fields (struct access), check if this is entity instance output
                // BUG #190 FIX: Check entity_instance_outputs BEFORE creating generic FieldAccess
                // This handles expressions like `__adder_result.result` where `__adder_result` is
                // an entity instance and `result` is an output port.
                if let hir::HirExpression::Variable(var_id) = &**base {
                    if let Some(output_ports) = self.entity_instance_outputs.get(var_id) {
                        if let Some(&signal_id) = output_ports.get(field) {
                            println!(
                                "[MODULE_FIELD_ACCESS] BUG #190 FIX: Entity instance var {:?} field '{}' -> signal {:?}",
                                var_id, field, signal_id
                            );
                            return Some(Expression::with_unknown_type(ExpressionKind::Ref(
                                LValue::Signal(signal_id),
                            )));
                        } else {
                            eprintln!(
                                "[MODULE_FIELD_ACCESS] Entity instance var {:?} field '{}' not found. Available: {:?}",
                                var_id, field, output_ports.keys().collect::<Vec<_>>()
                            );
                        }
                    }
                }

                // Fall back to creating a FieldAccess node for non-entity struct access
                eprintln!(
                    "    📎 FieldAccess on non-tuple field '{}' - creating FieldAccess expression",
                    field
                );
                Some(Expression::with_unknown_type(ExpressionKind::FieldAccess {
                    base: Box::new(base_converted),
                    field: field.clone(),
                }))
            }

            // BUG FIX #85: Handle Index (single bit select) in module context
            hir::HirExpression::Index(base, index) => {
                println!(
                    "📐📐📐 MODULE_INDEX: bit select on base {:?} 📐📐📐",
                    std::mem::discriminant(&**base)
                );

                // Convert base expression (e.g., data1) in module context
                let base_converted = self.convert_hir_expr_for_module(base, ctx, depth + 1)?;

                // Extract index as integer
                let bit_idx = match &**index {
                    hir::HirExpression::Literal(hir::HirLiteral::Integer(n)) => *n as i64,
                    _ => 0,
                };
                println!("📐📐📐 MODULE_INDEX: Bit select [{}] 📐📐📐", bit_idx);

                if let ExpressionKind::Ref(base_lval) = base_converted.kind {
                    Some(Expression::with_unknown_type(ExpressionKind::Ref(
                        LValue::BitSelect {
                            base: Box::new(base_lval),
                            index: Box::new(Expression::with_unknown_type(
                                ExpressionKind::Literal(Value::Integer(bit_idx)),
                            )),
                        },
                    )))
                } else {
                    eprintln!("    📐 BitSelect on non-Ref base - using fallback");
                    self.convert_expression(expr, depth)
                }
            }

            // BUG FIX #85: Handle Range (range select) in module context
            // This is critical for `let a = data1[31:0];` patterns in match arms
            hir::HirExpression::Range(base, high, low) => {
                println!(
                    "📐📐📐 MODULE_RANGE: range select on base {:?} 📐📐📐",
                    std::mem::discriminant(&**base)
                );

                // Convert base expression in module context
                let base_converted = self.convert_hir_expr_for_module(base, ctx, depth + 1)?;

                // Extract high and low bounds as integers
                let high_val = match &**high {
                    hir::HirExpression::Literal(hir::HirLiteral::Integer(n)) => *n as i64,
                    _ => 0,
                };
                let low_val = match &**low {
                    hir::HirExpression::Literal(hir::HirLiteral::Integer(n)) => *n as i64,
                    _ => 0,
                };
                println!(
                    "📐📐📐 MODULE_RANGE: Range select [{}:{}] 📐📐📐",
                    high_val, low_val
                );

                if let ExpressionKind::Ref(base_lval) = base_converted.kind {
                    Some(Expression::with_unknown_type(ExpressionKind::Ref(
                        LValue::RangeSelect {
                            base: Box::new(base_lval),
                            high: Box::new(Expression::with_unknown_type(ExpressionKind::Literal(
                                Value::Integer(high_val),
                            ))),
                            low: Box::new(Expression::with_unknown_type(ExpressionKind::Literal(
                                Value::Integer(low_val),
                            ))),
                        },
                    )))
                } else {
                    // BUG FIX #138: Handle Range on computed expressions (non-Ref base)
                    // This happens when mutable var transformation produces an expression like:
                    //   Range(xor_chain, 0, 0) where xor_chain is a complex BinaryOp
                    //
                    // For bit extraction expr[high:low], we use:
                    //   (expr >> low) & ((1 << (high - low + 1)) - 1)
                    //
                    // Special case for [0:0]: just (expr & 1) - extract lowest bit
                    println!(
                        "📐📐📐 BUG #138 FIX: Range on computed expression [{}:{}] 📐📐📐",
                        high_val, low_val
                    );

                    let width = (high_val - low_val + 1) as u64;
                    let mask = (1u64 << width) - 1;

                    let result = if low_val == 0 {
                        // Simple case: just mask with (1 << width) - 1
                        Expression::with_unknown_type(ExpressionKind::Binary {
                            op: BinaryOp::BitwiseAnd,
                            left: Box::new(base_converted),
                            right: Box::new(Expression::with_unknown_type(
                                ExpressionKind::Literal(Value::BitVector {
                                    value: mask,
                                    width: 32,
                                }),
                            )),
                        })
                    } else {
                        // General case: shift right by low, then mask
                        let shifted = Expression::with_unknown_type(ExpressionKind::Binary {
                            op: BinaryOp::RightShift,
                            left: Box::new(base_converted),
                            right: Box::new(Expression::with_unknown_type(
                                ExpressionKind::Literal(Value::Integer(low_val)),
                            )),
                        });
                        Expression::with_unknown_type(ExpressionKind::Binary {
                            op: BinaryOp::BitwiseAnd,
                            left: Box::new(shifted),
                            right: Box::new(Expression::with_unknown_type(
                                ExpressionKind::Literal(Value::BitVector {
                                    value: mask,
                                    width: 32,
                                }),
                            )),
                        })
                    };

                    Some(result)
                }
            }

            // For other expression types, fall back to the main converter
            _ => {
                eprintln!(
                    "    ⚠️  Expression type {:?} - using fallback",
                    std::mem::discriminant(expr)
                );
                self.convert_expression(expr, depth)
            }
        }
    }

    /// Helper to assign an expression to output ports (handles tuples by splitting into elements)
    fn assign_to_output_ports(
        &self,
        expr: &Expression,
        output_port_ids: &[PortId],
        module: &mut Module,
    ) {
        // If there's only one output port, assign directly
        if output_port_ids.len() == 1 {
            let assignment = ContinuousAssign {
                lhs: LValue::Port(output_port_ids[0]),
                rhs: expr.clone(),
                span: None,
            };
            module.assignments.push(assignment);
            eprintln!(
                "    ✓ Assigned to single output port (id={})",
                output_port_ids[0].0
            );
        } else if let ExpressionKind::Concat(elements) = &expr.kind {
            // Tuple return - assign each element to corresponding port
            // BUG FIX #91: TupleLiteral elements are reversed in convert_hir_expr_for_module
            // for correct byte layout. Reverse back to get logical order for port assignment.
            let elements_reversed: Vec<_> = elements.iter().rev().collect();
            for (idx, (port_id, elem)) in output_port_ids
                .iter()
                .zip(elements_reversed.iter())
                .enumerate()
            {
                let assignment = ContinuousAssign {
                    lhs: LValue::Port(*port_id),
                    rhs: (*elem).clone(),
                    span: None,
                };
                module.assignments.push(assignment);
                eprintln!(
                    "    ✓ Assigned tuple element {} to output port (id={})",
                    idx, port_id.0
                );
            }
        } else if let ExpressionKind::Conditional {
            cond,
            then_expr,
            else_expr,
        } = &expr.kind
        {
            // Handle conditional expressions where both branches are tuples (early return pattern)
            // Convert: cond ? (a0, a1, a2) : (b0, b1, b2)
            // Into:    result_0 = cond ? a0 : b0
            //          result_1 = cond ? a1 : b1
            //          result_2 = cond ? a2 : b2
            if let (ExpressionKind::Concat(then_elements), ExpressionKind::Concat(else_elements)) =
                (&then_expr.kind, &else_expr.kind)
            {
                if then_elements.len() == output_port_ids.len()
                    && else_elements.len() == output_port_ids.len()
                {
                    eprintln!(
                        "    🔧 Expanding conditional tuple return to per-element conditionals"
                    );
                    // BUG FIX #91: Reverse elements to match logical order (same as Concat case above)
                    let then_reversed: Vec<_> = then_elements.iter().rev().collect();
                    let else_reversed: Vec<_> = else_elements.iter().rev().collect();
                    for (idx, ((port_id, then_elem), else_elem)) in output_port_ids
                        .iter()
                        .zip(then_reversed.iter())
                        .zip(else_reversed.iter())
                        .enumerate()
                    {
                        // Create per-element conditional: cond ? then_elem : else_elem
                        let elem_conditional =
                            Expression::with_unknown_type(ExpressionKind::Conditional {
                                cond: cond.clone(),
                                then_expr: Box::new((*then_elem).clone()),
                                else_expr: Box::new((*else_elem).clone()),
                            });
                        let assignment = ContinuousAssign {
                            lhs: LValue::Port(*port_id),
                            rhs: elem_conditional,
                            span: None,
                        };
                        module.assignments.push(assignment);
                        eprintln!(
                            "    ✓ Assigned conditional tuple element {} to output port (id={})",
                            idx, port_id.0
                        );
                    }
                } else {
                    // Mismatched tuple sizes - fall back to assigning whole conditional to first port
                    debug_eprintln!(
                        "    ⚠️  Conditional tuple size mismatch - assigning to first port"
                    );
                    let assignment = ContinuousAssign {
                        lhs: LValue::Port(output_port_ids[0]),
                        rhs: expr.clone(),
                        span: None,
                    };
                    module.assignments.push(assignment);
                }
            } else {
                // Branches aren't both tuples - assign whole conditional to first port
                debug_eprintln!(
                    "    ⚠️  Conditional with non-tuple branches - assigning to first port"
                );
                let assignment = ContinuousAssign {
                    lhs: LValue::Port(output_port_ids[0]),
                    rhs: expr.clone(),
                    span: None,
                };
                module.assignments.push(assignment);
            }
        } else {
            // For non-tuple expressions with multiple ports, assign to first port
            debug_eprintln!("    ⚠️  Multiple output ports but non-tuple expression (kind={:?}) - assigning to first port",
                     std::mem::discriminant(&expr.kind));
            let assignment = ContinuousAssign {
                lhs: LValue::Port(output_port_ids[0]),
                rhs: expr.clone(),
                span: None,
            };
            module.assignments.push(assignment);
        }
    }

    /// Helper to convert a return expression and assign to output ports
    fn convert_return_to_output(
        &mut self,
        ret_expr: &hir::HirExpression,
        ctx: &ModuleSynthesisContext,
        output_port_ids: &[PortId],
        module: &mut Module,
        conversion_errors: &mut usize,
    ) {
        match self.convert_hir_expr_for_module(ret_expr, ctx, 0) {
            Some(converted) => {
                self.assign_to_output_ports(&converted, output_port_ids, module);
                eprintln!("    ✓ Return expression converted and assigned to output ports");
            }
            None => {
                debug_eprintln!(
                    "    ❌ Failed to convert return expression - using placeholder zeros"
                );
                *conversion_errors += 1;
                // Create placeholder assignments for all output ports
                for port_id in output_port_ids {
                    let assignment = ContinuousAssign {
                        lhs: LValue::Port(*port_id),
                        rhs: Expression {
                            kind: ExpressionKind::Literal(Value::Integer(0)),
                            ty: skalp_frontend::types::Type::Bit(Width::Fixed(32)),
                            span: None,
                        },
                        span: None,
                    };
                    module.assignments.push(assignment);
                }
            }
        }
    }

    /// Synthesize a function as a hardware module (instead of inlining)
    /// This is the core of the hybrid inlining/module-instantiation strategy.
    ///
    /// For a function like:
    /// ```skalp
    /// pub fn quadratic_solve(a: bit[32], b: bit[32], c: bit[32]) -> (bit, bit[32], bit[32]) {
    ///     // function body
    ///     return (valid, x1, x2)
    /// }
    /// ```
    ///
    /// Creates a Module with:
    /// - Input ports: param_a, param_b, param_c
    /// - Output ports: result_0 (bit), result_1 (bit[32]), result_2 (bit[32])
    /// - Internal signals for local variables
    /// - Continuous assignment or combinational process for function body
    ///
    /// Returns the ModuleId which can be used to instantiate the module at call sites.
    fn synthesize_function_as_module(&mut self, func: &hir::HirFunction) -> ModuleId {
        println!(
            "🏗️🏗️🏗️ SYNTHESIZE_FUNCTION_AS_MODULE: '{}' 🏗️🏗️🏗️",
            func.name
        );
        eprintln!(
            "🔧 Synthesizing function '{}' as hardware module (>{} calls detected)",
            func.name, MAX_INLINE_CALL_COUNT
        );

        // BUG #203 FIX: Clear placeholder maps when starting a new module synthesis
        // Each module has its own scope of VariableIds. Without clearing, VariableId(30)
        // from a previous module (e.g., comparator output with type Bit(1)) contaminates
        // a new module where VariableId(30) should map to Float32.
        // We don't need to merge back because each module is independent.
        self.placeholder_signal_to_entity_output.clear();
        self.placeholder_hir_signal_to_entity_output.clear();
        println!(
            "[BUG #203 FIX] Cleared placeholder maps for module '{}'",
            func.name
        );

        // BUG #204 FIX: Save and restore current_module_var_to_signal for nested module synthesis
        // When synthesizing nested modules (e.g., exec_l4_l5 -> quadratic_solve -> fp_sqrt),
        // each module synthesis overwrites current_module_var_to_signal with its own mappings.
        // Without save/restore, when we return to the parent module synthesis, the mapping
        // is lost (set to None by the child), causing Variable references to be incorrectly
        // resolved to signals from the wrong context.
        let saved_var_to_signal = self.current_module_var_to_signal.take();
        println!(
            "[BUG #204 FIX] Saved current_module_var_to_signal for module '{}' (had {} entries)",
            func.name,
            saved_var_to_signal.as_ref().map(|m| m.len()).unwrap_or(0)
        );

        // BUG #205 FIX: Also save and restore current_module_param_to_port for nested module synthesis
        // Similar to BUG #204, but for parameter->port mappings used by GenericParam resolution.
        let saved_param_to_port = self.current_module_param_to_port.take();
        println!(
            "[BUG #205 FIX] Saved current_module_param_to_port for module '{}' (had {} entries)",
            func.name,
            saved_param_to_port.as_ref().map(|m| m.len()).unwrap_or(0)
        );

        // BUG #206 FIX: Save and clear pending_mir_param_subs for module synthesis boundary
        // Without this, stale values from previous function inlining pollute the module context.
        // When convert_expression encounters GenericParam("a"), it checks pending_mir_param_subs
        // BEFORE current_module_param_to_port (BUG #205). If "a" exists in pending_mir_param_subs
        // from a previous inline context (e.g., mapping to Signal(1137)), it will be returned
        // instead of the correct module input port.
        let saved_mir_param_subs = std::mem::take(&mut self.pending_mir_param_subs);
        let saved_param_var_to_name = std::mem::take(&mut self.pending_param_var_to_name);
        println!(
            "[BUG #206 FIX] Saved and cleared pending_mir_param_subs for module '{}' (had {} entries)",
            func.name,
            saved_mir_param_subs.len()
        );

        // Save the current pending instance count - we'll only drain instances added AFTER this point
        // This is critical for nested module synthesis (e.g., exec_l4_l5 -> quadratic_solve)
        let pending_start_idx = self.pending_module_instances.len();
        let pending_entity_start_idx = self.pending_entity_instances.len();
        println!(
            "📌📌📌 SYNTHESIS START: '{}' pending_start_idx={} pending_entity_start_idx={} 📌📌📌",
            func.name, pending_start_idx, pending_entity_start_idx
        );

        let module_id = self.next_module_id();
        let module_name = format!("func_{}", func.name);
        let mut module = Module::new(module_id, module_name);

        // Propagate pipeline configuration from HIR function to MIR module
        if let Some(ref config) = func.pipeline_config {
            module.pipeline_config = Some(config.clone());
            println!(
                "🔧 PIPELINE: Propagating pipeline_config (stages={}) to module '{}'",
                config.stages, func.name
            );
        }

        // Phase 1: Convert function parameters to input ports
        // IMPORTANT: Use index-based naming (param_0, param_1, etc.) to match instance connections
        for (param_idx, param) in func.params.iter().enumerate() {
            let port_id = self.next_port_id();
            let port_name = format!("param_{}", param_idx);
            let port_type = self.convert_type(&param.param_type);

            let port = Port {
                id: port_id,
                name: port_name,
                direction: PortDirection::Input,
                port_type,
                physical_constraints: None,
                span: None,
                detection_config: None,
            };

            module.ports.push(port);

            eprintln!(
                "  ✓ Input port {}: {} ({})",
                param_idx,
                param.name,
                self.type_to_string(&param.param_type)
            );
        }

        // Phase 2: Convert return type to output port(s)
        // Handle tuples by creating multiple output ports (result_0, result_1, ...)
        match &func.return_type {
            Some(hir::HirType::Tuple(elements)) => {
                // Tuple return: create multiple output ports
                for (idx, elem_type) in elements.iter().enumerate() {
                    let port_id = self.next_port_id();
                    let port_name = format!("result_{}", idx);
                    let port_type = self.convert_type(elem_type);

                    let port = Port {
                        id: port_id,
                        name: port_name,
                        direction: PortDirection::Output,
                        port_type,
                        physical_constraints: None,
                        span: None,
                        detection_config: None,
                    };

                    module.ports.push(port);
                    eprintln!("  ✓ Output port {}: result_{}", idx, idx);
                }
            }
            Some(return_type) => {
                // Single return value
                let port_id = self.next_port_id();
                let port_type = self.convert_type(return_type);

                let port = Port {
                    id: port_id,
                    name: "result".to_string(),
                    direction: PortDirection::Output,
                    port_type,
                    physical_constraints: None,
                    span: None,
                    detection_config: None,
                };

                module.ports.push(port);
                eprintln!("  ✓ Output port: result");
            }
            None => {
                // No return type - function doesn't return anything
                eprintln!("  ⓘ No return type - no output ports");
            }
        }

        // Phase 3: Convert function body to module logic
        // This is where we convert the HIR function body (statements) into MIR module logic
        eprintln!(
            "  🔄 Converting function body ({} statements)",
            func.body.len()
        );

        // Build ModuleSynthesisContext with parameter→port mappings
        let mut ctx = ModuleSynthesisContext::new(&func.name);
        for (param_idx, param) in func.params.iter().enumerate() {
            // Find the corresponding input port we created in Phase 1
            let port = &module.ports[param_idx];
            ctx.param_to_port.insert(param.name.clone(), port.id);
            ctx.name_to_type
                .insert(param.name.clone(), param.param_type.clone());
            // Map VariableId to name - parameters get sequential IDs starting from 0
            ctx.var_id_to_name
                .insert(hir::VariableId(param_idx as u32), param.name.clone());
            eprintln!(
                "    • Mapped param '{}' → port '{}' (id={}), VariableId({})",
                param.name, port.name, port.id.0, param_idx
            );
        }

        // Convert function body statements to module logic
        // Handle: let bindings, if statements with early returns, final return
        // BUG FIX #85: Store full let statements to use their actual VariableIds, not sequential assumptions
        // BUG FIX #85 (continued): Extract let bindings from match arms, not just top-level

        // BUG FIX: Transform early returns into nested if-else before processing
        // This converts: if cond { return A } ... return B
        // Into: return if cond { A } else { B }
        println!(
            "🔍 BEFORE transform_early_returns: {} statements",
            func.body.len()
        );
        for (i, stmt) in func.body.iter().enumerate() {
            println!("   [{:02}] {:?}", i, std::mem::discriminant(stmt));
            if let hir::HirStatement::If(_) = stmt {
                println!("        ^^^^ THIS IS AN IF STATEMENT!");
            }
        }
        let body = self.transform_early_returns(func.body.clone());
        println!(
            "🔄 AFTER transform_early_returns: {} statements",
            body.len()
        );
        for (i, stmt) in body.iter().enumerate() {
            println!("   [{:02}] {:?}", i, std::mem::discriminant(stmt));
        }

        let mut let_bindings: Vec<hir::HirLetStatement> = Vec::new();
        let mut return_expr: Option<hir::HirExpression> = None;
        let mut if_return_statements: Vec<&hir::HirIfStatement> = Vec::new();

        // First pass: identify let bindings, if statements, and return statement
        println!(
            "🔍🔍🔍 FIRST PASS: Scanning {} statements for early returns 🔍🔍🔍",
            body.len()
        );
        for (idx, stmt) in body.iter().enumerate() {
            println!("   Statement {}: {:?}", idx, std::mem::discriminant(stmt));
            match stmt {
                hir::HirStatement::Let(let_stmt) => {
                    println!(
                        "    • Found top-level let binding: {} (actual VariableId={})",
                        let_stmt.name, let_stmt.id.0
                    );
                    let_bindings.push(let_stmt.clone());
                }
                hir::HirStatement::Return(value) => {
                    if let Some(val) = value {
                        return_expr = Some(val.clone());
                        println!(
                            "    • Found return expression: {:?}",
                            std::mem::discriminant(val)
                        );
                        // Check if the return is a ternary/if expression (early return transformed)
                        match val {
                            hir::HirExpression::Ternary {
                                condition,
                                true_expr,
                                false_expr,
                            } => {
                                println!("      🎯 Return is a Ternary (conditional)!");
                                println!(
                                    "         condition: {:?}",
                                    std::mem::discriminant(condition.as_ref())
                                );
                                println!(
                                    "         true: {:?}",
                                    std::mem::discriminant(true_expr.as_ref())
                                );
                                println!(
                                    "         false: {:?}",
                                    std::mem::discriminant(false_expr.as_ref())
                                );
                            }
                            hir::HirExpression::If(if_expr) => {
                                println!("      🎯 Return is an If expression!");
                            }
                            hir::HirExpression::TupleLiteral(elements) => {
                                println!(
                                    "      🎯 Return is a TupleLiteral with {} elements:",
                                    elements.len()
                                );
                                for (i, elem) in elements.iter().enumerate() {
                                    println!("         [{i}] {:?}", std::mem::discriminant(elem));
                                    // Check if element is conditional
                                    match elem {
                                        hir::HirExpression::Ternary { .. } => {
                                            println!("             -> Is Ternary!")
                                        }
                                        hir::HirExpression::If(_) => {
                                            println!("             -> Is If expression!")
                                        }
                                        hir::HirExpression::Variable(v) => {
                                            println!("             -> Variable({:?})", v)
                                        }
                                        hir::HirExpression::Literal(_) => {
                                            println!("             -> Literal")
                                        }
                                        _ => {}
                                    }
                                }
                            }
                            _ => {}
                        }
                    }
                }
                hir::HirStatement::If(if_stmt) => {
                    // Check if this is an early return pattern (if condition { return ... })
                    if_return_statements.push(if_stmt);
                    println!("    • 🚨 Found if statement (may contain early return)");
                    // Check if then branch has a return
                    let has_return = if_stmt
                        .then_statements
                        .iter()
                        .any(|s| matches!(s, hir::HirStatement::Return(_)));
                    println!("      -> Then branch has return: {}", has_return);
                }
                _ => {
                    println!(
                        "    ⚠️  Skipping unsupported statement type: {:?}",
                        std::mem::discriminant(stmt)
                    );
                }
            }
        }
        println!(
            "🔍🔍🔍 FIRST PASS DONE: {} let bindings, {} if statements, return_expr={} 🔍🔍🔍",
            let_bindings.len(),
            if_return_statements.len(),
            return_expr.is_some()
        );

        // BUG FIX #85: If return expression is a Match, extract let bindings from match arms
        // This handles functions like exec_l4_l5 that have: return match opcode { ... }
        // where the let bindings are inside the match arms
        if let Some(ref ret_expr) = return_expr {
            println!(
                "🔍🔍🔍 CHECKING return expression for let bindings: {:?}",
                std::mem::discriminant(ret_expr)
            );
            let extracted = Self::extract_let_bindings_from_expression(ret_expr);
            println!(
                "🔍🔍🔍 EXTRACTED {} let bindings from expression",
                extracted.len()
            );
            if !extracted.is_empty() {
                println!(
                    "    📦 Extracted {} let bindings from return expression (match arms)",
                    extracted.len()
                );
                for let_stmt in &extracted {
                    println!("      • {} (VariableId={})", let_stmt.name, let_stmt.id.0);
                }
                let_bindings.extend(extracted);
            }
        }

        // Create internal signals for let bindings
        // These will hold intermediate computation results
        // BUG FIX #85: Use actual VariableIds from HIR let statements, not sequential assumptions
        // BUG FIX #85 (continued): Also create Variable entries so lookups work during elaboration
        // BUG FIX #130: Skip tuple temp variables - these are for tuple destructuring and
        // should NOT have signals created. When FieldAccess references them, the Call
        // should be converted fresh to get the Concat of outputs, then element extracted.
        for let_stmt in &let_bindings {
            // BUG FIX #130: Skip tuple temp variables
            // These are created by tuple destructuring like `let (a, b, c) = func()`
            // which becomes `let _tuple_tmp_N = func(); let a = _tuple_tmp_N.0; ...`
            // Creating a single 32-bit signal for the tuple temp is wrong - we need
            // to convert the Call expression to a Concat and store it for later lookup.
            if let_stmt.name.starts_with("_tuple_tmp_") {
                eprintln!(
                    "    ⏭️  BUG #130 FIX: Converting tuple temp '{}' to MIR expression",
                    let_stmt.name
                );
                // Still add to var_id_to_name for debugging purposes
                ctx.var_id_to_name
                    .insert(let_stmt.id, let_stmt.name.clone());

                // BUG FIX #130 Part 2: Convert the Call expression to MIR (produces Concat)
                // and store it in tuple_temp_to_mir for later lookup by FieldAccess
                match self.convert_hir_expr_for_module(&let_stmt.value, &ctx, 0) {
                    Some(converted_expr) => {
                        eprintln!(
                            "    ✓ BUG #130 FIX: Stored tuple temp '{}' (var_id={}) -> {:?}",
                            let_stmt.name, let_stmt.id.0, converted_expr.kind
                        );
                        ctx.tuple_temp_to_mir.insert(let_stmt.id, converted_expr);
                    }
                    None => {
                        eprintln!(
                            "    ❌ BUG #130 FIX: Failed to convert tuple temp '{}'",
                            let_stmt.name
                        );
                    }
                }
                continue;
            }

            let signal_id = self.next_signal_id();
            // Type inference would go here - for now use bit[32] as placeholder
            // BUG FIX #85: Include VariableId in signal name to avoid collisions between match arms
            // Different match arms can have variables with the same name but different IDs
            let unique_signal_name = format!("{}_{}", let_stmt.name, let_stmt.id.0);
            let signal = Signal {
                id: signal_id,
                name: unique_signal_name.clone(),
                signal_type: DataType::Bit(32), // Placeholder - should infer from expression
                initial: None,
                clock_domain: None,
                span: None,
                memory_config: None,
                trace_config: None,
                cdc_config: None,
                breakpoint_config: None,
                power_config: None,
                safety_context: None,
                detection_config: None,
                power_domain: None,
            };
            module.signals.push(signal);

            // BUG FIX #85: Also create a Variable entry so that Variable references can be resolved
            // When call sites reference let bindings, they use Variable(var_id), but we created Signals
            // This Variable entry enables get_signal_name_from_expression_with_context to find the name
            let variable = Variable {
                id: VariableId(let_stmt.id.0),    // Use the HIR VariableId
                name: unique_signal_name.clone(), // Use unique name here too
                var_type: DataType::Bit(32),      // Match the signal type
                initial: None,
                span: None,
            };
            module.variables.push(variable);

            // BUG FIX #85: Map by VariableId (not name) to avoid collisions between match arms
            ctx.var_to_signal.insert(let_stmt.id, signal_id);
            // Also store name mapping for debugging (use original name for readability)
            ctx.var_id_to_name
                .insert(let_stmt.id, let_stmt.name.clone());
            eprintln!(
                "    ✓ Created signal+variable for let binding: {} -> {} (signal_id={}, var_id={})",
                let_stmt.name, unique_signal_name, signal_id.0, let_stmt.id.0
            );
        }

        // BUG #200 FIX: Store the var_to_signal mapping so that convert_expression can access it
        // when processing entity struct literal fields during convert_statement calls.
        // This is critical because convert_statement doesn't receive the ModuleSynthesisContext.
        self.current_module_var_to_signal = Some(ctx.var_to_signal.clone());
        eprintln!(
            "    🔧 BUG #200 FIX: Set current_module_var_to_signal with {} entries",
            ctx.var_to_signal.len()
        );

        // BUG #205 FIX: Also store the param_to_port mapping so that convert_expression can resolve
        // GenericParam references to Port references for entity struct literal fields.
        // Without this, entity connections with GenericParam references resolve to 0 instead of
        // the correct input port.
        self.current_module_param_to_port = Some(ctx.param_to_port.clone());
        println!(
            "    🔧 BUG #205 FIX: Set current_module_param_to_port with {} entries: {:?}",
            ctx.param_to_port.len(),
            ctx.param_to_port.keys().collect::<Vec<_>>()
        );

        // Phase 3b: Convert let binding expressions to continuous assignments
        eprintln!(
            "  🔧 Phase 3b: Converting {} let bindings to assignments",
            let_bindings.len()
        );

        let mut conversion_errors = 0;
        for let_stmt in &let_bindings {
            // BUG FIX #130: Skip tuple temp variables - no signals were created for them
            if let_stmt.name.starts_with("_tuple_tmp_") {
                eprintln!(
                    "    ⏭️  BUG #130 FIX: Skipping assignment for tuple temp '{}'",
                    let_stmt.name
                );
                continue;
            }

            // Get the signal we created for this let binding (lookup by VariableId)
            let signal_id = ctx.var_to_signal[&let_stmt.id];

            // Convert HIR expression to MIR using the module context
            match self.convert_hir_expr_for_module(&let_stmt.value, &ctx, 0) {
                Some(converted_expr) => {
                    let assignment = ContinuousAssign {
                        lhs: LValue::Signal(signal_id),
                        rhs: converted_expr,
                        span: None,
                    };
                    module.assignments.push(assignment);
                    eprintln!(
                        "    ✓ Created assignment for '{}' (signal_id={})",
                        let_stmt.name, signal_id.0
                    );
                }
                None => {
                    eprintln!(
                        "    ❌ Failed to convert expression for '{}' - using placeholder",
                        let_stmt.name
                    );
                    conversion_errors += 1;
                    // Create a placeholder assignment with zero
                    let assignment = ContinuousAssign {
                        lhs: LValue::Signal(signal_id),
                        rhs: Expression {
                            kind: ExpressionKind::Literal(Value::Integer(0)),
                            ty: skalp_frontend::types::Type::Bit(Width::Fixed(32)),
                            span: None,
                        },
                        span: None,
                    };
                    module.assignments.push(assignment);
                }
            }
        }

        // Phase 3c: Convert return expression to output port assignments
        // Handle early returns by converting if statements with returns to conditional expressions
        let input_count = func.params.len();
        let output_port_ids: Vec<PortId> =
            module.ports[input_count..].iter().map(|p| p.id).collect();

        if let Some(ref ret_expr) = return_expr {
            eprintln!("  🔧 Phase 3c: Converting return expression to output assignment");

            // Check if we need to handle early returns (if statements that return)
            if !if_return_statements.is_empty() {
                eprintln!(
                    "    • Detected {} if statements with potential early returns",
                    if_return_statements.len()
                );
                // For now, we handle the common pattern:
                // if (condition) { return early_value; }
                // return normal_value;
                //
                // This becomes: output = condition ? early_value : normal_value

                if if_return_statements.len() == 1 {
                    let if_stmt = if_return_statements[0];
                    // Check if the then branch contains a return
                    let early_return = if_stmt.then_statements.iter().find_map(|s| {
                        if let hir::HirStatement::Return(Some(e)) = s {
                            Some(e)
                        } else {
                            None
                        }
                    });

                    if let Some(early_ret_expr) = early_return {
                        // Convert condition, early return value, and normal return value
                        let cond = self.convert_hir_expr_for_module(&if_stmt.condition, &ctx, 0);
                        let early_val = self.convert_hir_expr_for_module(early_ret_expr, &ctx, 0);
                        let normal_val = self.convert_hir_expr_for_module(ret_expr, &ctx, 0);

                        if let (Some(cond_mir), Some(early_mir), Some(normal_mir)) =
                            (cond, early_val, normal_val)
                        {
                            // Create conditional: condition ? early_value : normal_value
                            let conditional =
                                Expression::with_unknown_type(ExpressionKind::Conditional {
                                    cond: Box::new(cond_mir),
                                    then_expr: Box::new(early_mir),
                                    else_expr: Box::new(normal_mir),
                                });

                            // Assign to output port(s)
                            self.assign_to_output_ports(
                                &conditional,
                                &output_port_ids,
                                &mut module,
                            );
                            eprintln!("    ✓ Created conditional output assignment (early return pattern)");
                        } else {
                            debug_eprintln!(
                                "    ❌ Failed to convert conditional return expression"
                            );
                            conversion_errors += 1;
                        }
                    } else {
                        // If statement doesn't contain a return - just convert the normal return
                        self.convert_return_to_output(
                            ret_expr,
                            &ctx,
                            &output_port_ids,
                            &mut module,
                            &mut conversion_errors,
                        );
                    }
                } else {
                    // Multiple if statements - for now, just use the final return
                    debug_eprintln!("    ⚠️  Multiple if statements not fully supported - using final return only");
                    self.convert_return_to_output(
                        ret_expr,
                        &ctx,
                        &output_port_ids,
                        &mut module,
                        &mut conversion_errors,
                    );
                }
            } else {
                // No early returns - just convert the return expression
                self.convert_return_to_output(
                    ret_expr,
                    &ctx,
                    &output_port_ids,
                    &mut module,
                    &mut conversion_errors,
                );
            }
        } else if !if_return_statements.is_empty() && if_return_statements.len() == 1 {
            // BUG FIX #86: Handle the case where transform_early_returns moved ALL returns inside the if/else
            // After transform: if (cond) { return A } else { ...let bindings...; return B }
            // Both returns are inside the if statement, so return_expr is None
            debug_println!(
                "🔵🔵🔵 BUG #110 DEBUG: Entering Phase 3c for early return pattern 🔵🔵🔵"
            );
            debug_println!("🔵🔵🔵 BUG #118 DEBUG: Phase 3c ctx.func_name='{}', var_to_signal has {} entries 🔵🔵🔵", ctx.func_name, ctx.var_to_signal.len());
            println!(
                "🔵🔵🔵 return_expr.is_none()={}, if_return_statements.len()={} 🔵🔵🔵",
                return_expr.is_none(),
                if_return_statements.len()
            );
            eprintln!("  🔧 Phase 3c: No top-level return, but if statement contains returns (transformed early return pattern)");

            let if_stmt = if_return_statements[0];

            // Extract return from then-branch
            let then_return = if_stmt.then_statements.iter().find_map(|s| {
                if let hir::HirStatement::Return(Some(e)) = s {
                    Some(e)
                } else {
                    None
                }
            });

            // Extract return from else-branch (may be nested after let bindings)
            let else_return = if let Some(else_stmts) = &if_stmt.else_statements {
                // Look for return in the else branch - it may be the last statement after let bindings
                else_stmts.iter().rev().find_map(|s| {
                    if let hir::HirStatement::Return(Some(e)) = s {
                        Some(e)
                    } else {
                        None
                    }
                })
            } else {
                None
            };

            if let (Some(then_ret), Some(else_ret)) = (then_return, else_return) {
                eprintln!("    • Found returns in both branches - creating conditional");
                eprintln!("    • then return: {:?}", std::mem::discriminant(then_ret));
                eprintln!("    • else return: {:?}", std::mem::discriminant(else_ret));

                // First, process any let bindings in the else branch and add them to ctx
                if let Some(else_stmts) = &if_stmt.else_statements {
                    for stmt in else_stmts {
                        if let hir::HirStatement::Let(let_stmt) = stmt {
                            // Check if we already have this let binding
                            if ctx.var_to_signal.contains_key(&let_stmt.id) {
                                continue;
                            }

                            // Create signal for this let binding
                            let signal_id = self.next_signal_id();
                            let unique_signal_name = format!("{}_{}", let_stmt.name, let_stmt.id.0);
                            let signal = Signal {
                                id: signal_id,
                                name: unique_signal_name.clone(),
                                signal_type: DataType::Bit(32),
                                initial: None,
                                clock_domain: None,
                                span: None,
                                memory_config: None,
                                trace_config: None,
                                cdc_config: None,
                                breakpoint_config: None,
                                power_config: None,
                                safety_context: None,
                                detection_config: None,
                                power_domain: None,
                            };
                            module.signals.push(signal);

                            // Create variable entry
                            let variable = Variable {
                                id: VariableId(let_stmt.id.0),
                                name: unique_signal_name.clone(),
                                var_type: DataType::Bit(32),
                                initial: None,
                                span: None,
                            };
                            module.variables.push(variable);

                            ctx.var_to_signal.insert(let_stmt.id, signal_id);
                            ctx.var_id_to_name
                                .insert(let_stmt.id, let_stmt.name.clone());
                            eprintln!("      ✓ Created signal for else-branch let binding: {} (var_id={}, signal_id={})",
                                     let_stmt.name, let_stmt.id.0, signal_id.0);

                            // Convert the let binding expression to an assignment
                            if let Some(converted_expr) =
                                self.convert_hir_expr_for_module(&let_stmt.value, &ctx, 0)
                            {
                                let assignment = ContinuousAssign {
                                    lhs: LValue::Signal(signal_id),
                                    rhs: converted_expr,
                                    span: None,
                                };
                                module.assignments.push(assignment);
                            }
                        }
                    }
                }

                // Convert condition and return values
                let cond = self.convert_hir_expr_for_module(&if_stmt.condition, &ctx, 0);
                let then_val = self.convert_hir_expr_for_module(then_ret, &ctx, 0);
                let else_val = self.convert_hir_expr_for_module(else_ret, &ctx, 0);

                if let (Some(cond_mir), Some(then_mir), Some(else_mir)) = (cond, then_val, else_val)
                {
                    // Create conditional: condition ? then_value : else_value
                    let conditional = Expression::with_unknown_type(ExpressionKind::Conditional {
                        cond: Box::new(cond_mir),
                        then_expr: Box::new(then_mir),
                        else_expr: Box::new(else_mir),
                    });

                    // Assign to output port(s)
                    self.assign_to_output_ports(&conditional, &output_port_ids, &mut module);
                    eprintln!("    ✓ Created conditional output assignment (transformed early return pattern)");
                } else {
                    eprintln!(
                        "    ❌ Failed to convert conditional return expression (transformed)"
                    );
                    conversion_errors += 1;
                }
            } else {
                debug_eprintln!("    ❌ Could not extract returns from both branches");
                eprintln!("      then_return: {}", then_return.is_some());
                eprintln!("      else_return: {}", else_return.is_some());
                conversion_errors += 1;
            }
        }

        // Debug output for conversion completion status
        #[allow(clippy::if_same_then_else)]
        if conversion_errors > 0 {
            eprintln!(
                "  ⚠️  Phase 3 completed with {} conversion errors (using placeholders)",
                conversion_errors
            );
        } else {
            eprintln!("  ✅ Phase 3 completed successfully - all expressions converted");
        }

        // Drain pending module instances created during THIS function's body conversion
        // Only drain instances added AFTER pending_start_idx to avoid stealing instances
        // that belong to our caller in the recursion stack
        self.drain_pending_module_instances_from(&mut module, pending_start_idx);

        // Drain pending entity instances (from trait method inlining with entity instantiation)
        // FIX: This was missing, causing FpAdd/FpMul entities from trait inlining to not be instantiated
        self.drain_pending_entity_instances_from(&mut module, pending_entity_start_idx);

        // Register the module in our function map
        self.function_map.insert(func.name.clone(), module_id);

        eprintln!(
            "✅ Module created: {} (id={}, {} input ports, {} output ports)",
            module.name,
            module_id.0,
            module
                .ports
                .iter()
                .filter(|p| p.direction == PortDirection::Input)
                .count(),
            module
                .ports
                .iter()
                .filter(|p| p.direction == PortDirection::Output)
                .count()
        );

        println!(
            "📋📋📋 MODULE '{}' has {} assignments 📋📋📋",
            func.name,
            module.assignments.len()
        );
        for (idx, assign) in module.assignments.iter().enumerate() {
            println!("    Assignment {}: {:?} <= <expr>", idx, assign.lhs);
        }

        // Store the module for later addition to MIR
        // This will be added to the MIR at the end of the transform() method
        self.synthesized_modules.push(module);
        eprintln!(
            "  ✓ Module stored in synthesized_modules (will be added to MIR at end of transform)"
        );

        // BUG #204 FIX: Restore the var_to_signal mapping from parent module synthesis
        // This ensures that when we return to processing the parent module's entity instances,
        // the correct variable->signal mapping is available.
        self.current_module_var_to_signal = saved_var_to_signal;
        println!(
            "[BUG #204 FIX] Restored current_module_var_to_signal for parent (now {} entries)",
            self.current_module_var_to_signal
                .as_ref()
                .map(|m| m.len())
                .unwrap_or(0)
        );

        // BUG #205 FIX: Also restore the param_to_port mapping from parent module synthesis
        self.current_module_param_to_port = saved_param_to_port;
        println!(
            "[BUG #205 FIX] Restored current_module_param_to_port for parent (now {} entries)",
            self.current_module_param_to_port
                .as_ref()
                .map(|m| m.len())
                .unwrap_or(0)
        );

        // BUG #206 FIX: Restore pending_mir_param_subs for parent inlining context
        self.pending_mir_param_subs = saved_mir_param_subs;
        self.pending_param_var_to_name = saved_param_var_to_name;
        println!(
            "[BUG #206 FIX] Restored pending_mir_param_subs for parent (now {} entries)",
            self.pending_mir_param_subs.len()
        );

        module_id
    }

    /// Drain pending module instances and add them to the specified module
    ///
    /// This method is called after expression conversion to materialize module instances
    /// that were recorded during complex function call conversion.
    /// Drain pending module instances starting from a specific index
    /// This is used for nested module synthesis to avoid stealing instances from callers
    fn drain_pending_module_instances_from(&mut self, module: &mut Module, start_idx: usize) {
        let total_pending = self.pending_module_instances.len();
        let instances_to_drain = total_pending.saturating_sub(start_idx);

        println!(
            "🚿🚿🚿 DRAIN_FROM: {} total, start_idx={}, draining {} for module '{}' (id={}) 🚿🚿🚿",
            total_pending, start_idx, instances_to_drain, module.name, module.id.0
        );

        if instances_to_drain == 0 {
            return;
        }

        println!(
            "🚿🚿🚿 DRAIN_FROM: Processing {} instances for module '{}' 🚿🚿🚿",
            instances_to_drain, module.name
        );
        eprintln!(
            "[HYBRID] Draining {} pending module instances (from idx {})...",
            instances_to_drain, start_idx
        );

        // Extract only the instances that belong to this module (from start_idx onwards)
        let pending: Vec<_> = self.pending_module_instances.drain(start_idx..).collect();

        // Convert HIR types to DataTypes
        // BUG FIX #92: Now using Vec<SignalId> for tuple support
        let instances_with_types: Vec<_> = pending
            .into_iter()
            .map(
                |(signal_ids, name, mod_id, args, hir_type, frontend_type)| {
                    let data_type = if let Some(ht) = hir_type {
                        self.convert_type(&ht)
                    } else {
                        DataType::Bit(32) // Fallback
                    };
                    (signal_ids, name, mod_id, args, data_type, frontend_type)
                },
            )
            .collect();

        self.process_pending_instances(instances_with_types, module);
    }

    ///
    /// For each pending instance:
    /// 1. Create a Signal with the pre-allocated SignalId
    /// 2. Create a ModuleInstance that connects arguments to module inputs and signal to output
    /// 3. Add both to the module
    fn drain_pending_module_instances(&mut self, module: &mut Module) {
        println!(
            "🚿🚿🚿 DRAIN_PENDING: {} instances in queue for module '{}' (id={}) 🚿🚿🚿",
            self.pending_module_instances.len(),
            module.name,
            module.id.0
        );
        if self.pending_module_instances.is_empty() {
            return;
        }

        println!(
            "🚿🚿🚿 DRAIN_PENDING: Processing {} instances for module '{}' 🚿🚿🚿",
            self.pending_module_instances.len(),
            module.name
        );
        eprintln!(
            "[HYBRID] Draining {} pending module instances...",
            self.pending_module_instances.len()
        );

        // Extract pending instances (to avoid borrow checker issues)
        let pending = std::mem::take(&mut self.pending_module_instances);

        // Convert HIR types to DataTypes
        // BUG FIX #92: Now using Vec<SignalId> for tuple support
        let instances_with_types: Vec<_> = pending
            .into_iter()
            .map(
                |(signal_ids, name, mod_id, args, hir_type, frontend_type)| {
                    let data_type = if let Some(ht) = hir_type {
                        self.convert_type(&ht)
                    } else {
                        DataType::Bit(32) // Fallback
                    };
                    (signal_ids, name, mod_id, args, data_type, frontend_type)
                },
            )
            .collect();

        self.process_pending_instances(instances_with_types, module);
    }

    /// Process a list of pending instances and add them to the module
    /// BUG FIX #92: Changed from SignalId to Vec<SignalId> for tuple support
    #[allow(clippy::type_complexity)]
    fn process_pending_instances(
        &mut self,
        instances_with_types: Vec<(
            Vec<SignalId>,
            String,
            ModuleId,
            Vec<Expression>,
            DataType,
            Type,
        )>,
        module: &mut Module,
    ) {
        for (result_signal_ids, function_name, module_id, arg_exprs, data_type, frontend_type) in
            instances_with_types
        {
            let first_signal_id = result_signal_ids.first().copied().unwrap_or(SignalId(0));
            println!("🎯🎯🎯 DRAIN: Creating instance of '{}' (module_id={}) with {} result signal(s), first={} 🎯🎯🎯",
                      function_name, module_id.0, result_signal_ids.len(), first_signal_id.0);
            eprintln!(
                "[HYBRID]   Creating instance of '{}' (module_id={}) with {} result signal(s)",
                function_name,
                module_id.0,
                result_signal_ids.len()
            );

            // Step 1: Determine if this is a tuple return by checking the data type
            let is_tuple = matches!(&data_type, DataType::Struct(_) | DataType::Array { .. });

            // BUG FIX #92: Use pre-allocated signal IDs count to determine tuple width
            // This is more reliable than trying to derive from data_type
            let tuple_width = if result_signal_ids.len() > 1 {
                Some(result_signal_ids.len())
            } else if let Type::Tuple(elements) = &frontend_type {
                Some(elements.len())
            } else if let DataType::Struct(struct_type) = &data_type {
                // Check if this is a tuple struct (name starts with "__tuple_")
                if struct_type.name.starts_with("__tuple_") {
                    Some(struct_type.fields.len())
                } else {
                    None
                }
            } else {
                None
            };

            let instance_name = format!("{}_inst_{}", function_name, first_signal_id.0);
            let mut connections = IndexMap::new();

            // Connect arguments to input ports (param_0, param_1, ...)
            for (arg_idx, arg_expr) in arg_exprs.iter().enumerate() {
                let port_name = format!("param_{}", arg_idx);
                connections.insert(port_name.clone(), arg_expr.clone());
                eprintln!(
                    "[HYBRID]       ✓ Connected argument {} to port '{}'",
                    arg_idx, port_name
                );
            }

            // Handle tuple returns vs single returns
            if let Some(num_elements) = tuple_width {
                // Tuple return: Create multiple signals and connect to result_0, result_1, etc.
                eprintln!(
                    "[HYBRID]     Creating {} output signals for tuple return (pre-allocated: {})",
                    num_elements,
                    result_signal_ids.len()
                );

                for elem_idx in 0..num_elements {
                    // BUG FIX #92: Use pre-allocated signal IDs from result_signal_ids
                    let elem_signal_id = if elem_idx < result_signal_ids.len() {
                        result_signal_ids[elem_idx]
                    } else {
                        // Fallback: allocate new ID (should not happen if tuple size was correctly determined)
                        eprintln!(
                            "    ⚠️ elem_idx {} exceeds pre-allocated IDs ({}), allocating new",
                            elem_idx,
                            result_signal_ids.len()
                        );
                        self.next_signal_id()
                    };

                    let signal_name = format!(
                        "{}_inst_{}_result_{}",
                        function_name, first_signal_id.0, elem_idx
                    );

                    // BUG FIX: Extract actual element type from tuple instead of hardcoding 32 bits
                    // This is critical for NCL: child modules have 1-bit result_0, parent must match
                    println!(
                        "🔍🔍🔍 DEBUG: frontend_type = {:?}, elem_idx = {}",
                        frontend_type, elem_idx
                    );
                    let element_data_type = if let Type::Tuple(elements) = &frontend_type {
                        if elem_idx < elements.len() {
                            self.frontend_type_to_datatype(&elements[elem_idx])
                        } else {
                            DataType::Bit(32) // fallback for out-of-bounds
                        }
                    } else {
                        data_type.clone() // single return type
                    };
                    eprintln!(
                        "[HYBRID]       BUG FIX: Element {} has type {:?}",
                        elem_idx, element_data_type
                    );

                    let signal = Signal {
                        id: elem_signal_id,
                        name: signal_name.clone(),
                        signal_type: element_data_type,
                        initial: None,
                        clock_domain: None,
                        span: None,
                        memory_config: None,
                        trace_config: None,
                        cdc_config: None,
                        breakpoint_config: None,
                        power_config: None,
                        safety_context: None,
                        detection_config: None,
                        power_domain: None,
                    };
                    module.signals.push(signal);
                    println!(
                        "🎯🎯🎯 DRAIN: Created tuple result signal '{}' (id={}) 🎯🎯🎯",
                        signal_name, elem_signal_id.0
                    );
                    eprintln!(
                        "[HYBRID]       ✓ Created result signal '{}' (id={})",
                        signal_name, elem_signal_id.0
                    );

                    // Connect to output port result_N
                    let result_port_name = format!("result_{}", elem_idx);
                    connections.insert(
                        result_port_name.clone(),
                        Expression::new(
                            ExpressionKind::Ref(LValue::Signal(elem_signal_id)),
                            frontend_type.clone(),
                        ),
                    );
                    eprintln!(
                        "[HYBRID]       ✓ Connected to output port '{}'",
                        result_port_name
                    );
                }
            } else {
                // Single return: Create one signal and connect to "result"
                let signal_name = format!("{}_result_{}", function_name, first_signal_id.0);
                let signal = Signal {
                    id: first_signal_id,
                    name: signal_name.clone(),
                    signal_type: data_type.clone(),
                    initial: None,
                    clock_domain: None,
                    span: None,
                    memory_config: None,
                    trace_config: None,
                    cdc_config: None,
                    breakpoint_config: None,
                    power_config: None,
                    safety_context: None,
                    detection_config: None,
                    power_domain: None,
                };
                module.signals.push(signal);
                debug_println!("[HYBRID]     ✓ Created result signal '{}'", signal_name);

                // Connect result signal to output port
                let result_port_name = "result".to_string();
                connections.insert(
                    result_port_name.clone(),
                    Expression::new(
                        ExpressionKind::Ref(LValue::Signal(first_signal_id)),
                        frontend_type,
                    ),
                );
                eprintln!(
                    "[HYBRID]       ✓ Connected result to output port '{}'",
                    result_port_name
                );
            }

            // Create the instance
            let instance = ModuleInstance {
                name: instance_name.clone(),
                module: module_id,
                connections,
                parameters: IndexMap::new(),
                span: None,
                safety_context: None,
            };

            module.instances.push(instance);
            debug_println!("[HYBRID]     ✓ Created module instance '{}'", instance_name);
        }

        debug_println!("[HYBRID] ✅ All pending module instances materialized");
    }

    /// Helper to convert HIR type to string for debugging
    fn type_to_string(&self, ty: &hir::HirType) -> String {
        format!("{:?}", ty) // Simplified for now
    }

    /// BUG FIX #92: Helper to get tuple size from HIR type
    /// Returns 0 if not a tuple, otherwise returns the number of tuple elements
    fn get_tuple_size_from_hir_type(hir_type: &Option<hir::HirType>) -> usize {
        match hir_type {
            Some(hir::HirType::Tuple(elements)) => elements.len(),
            _ => 0,
        }
    }

    /// Drain pending entity instances (from let bindings with entity instantiation)
    /// BUG FIX #13-16, #21-23: Hierarchical elaboration support
    fn drain_pending_entity_instances(&mut self, module: &mut Module) {
        if self.pending_entity_instances.is_empty() {
            return;
        }

        eprintln!(
            "[HIERARCHICAL] Draining {} pending entity instances for module '{}'",
            self.pending_entity_instances.len(),
            module.name
        );

        // Take ownership of pending instances
        let pending = std::mem::take(&mut self.pending_entity_instances);

        for (instance, output_signals) in pending {
            eprintln!(
                "[HIERARCHICAL] Adding instance '{}' with {} output signals",
                instance.name,
                output_signals.len()
            );

            // Create output signals for this instance
            for (signal_id, signal_name, signal_type, detection_cfg) in output_signals {
                let signal = Signal {
                    id: signal_id,
                    name: signal_name.clone(),
                    signal_type,
                    initial: None,
                    clock_domain: None,
                    span: None,
                    memory_config: None,
                    trace_config: None,
                    cdc_config: None,
                    breakpoint_config: None,
                    power_config: None,
                    safety_context: None,
                    detection_config: detection_cfg.clone(),
                    power_domain: None, // TODO: propagate from port
                };
                eprintln!(
                    "[HIERARCHICAL] Creating signal '{}' (id={}) for instance output (detection={})",
                    signal_name, signal_id.0, detection_cfg.is_some()
                );
                module.signals.push(signal);
            }

            // Add the module instance
            module.instances.push(instance);
        }
    }

    /// Drain pending entity instances starting from a specific index
    /// This is used for nested module synthesis to avoid stealing instances from callers
    /// FIX: Added to support entity instantiation in synthesized functions (e.g., FP operations via traits)
    fn drain_pending_entity_instances_from(&mut self, module: &mut Module, start_idx: usize) {
        let total_pending = self.pending_entity_instances.len();
        let instances_to_drain = total_pending.saturating_sub(start_idx);

        println!(
            "🚿🚿🚿 ENTITY_DRAIN_FROM: {} total, start_idx={}, draining {} for module '{}' (id={}) 🚿🚿🚿",
            total_pending, start_idx, instances_to_drain, module.name, module.id.0
        );

        if instances_to_drain == 0 {
            return;
        }

        println!(
            "🔧🔧🔧 ENTITY_DRAIN: About to drain {} instances for module '{}' 🔧🔧🔧",
            instances_to_drain, module.name
        );
        eprintln!(
            "[HIERARCHICAL] Draining {} pending entity instances (from idx {}) for module '{}'",
            instances_to_drain, start_idx, module.name
        );

        // Extract only the instances that belong to this module (from start_idx onwards)
        let pending: Vec<_> = self.pending_entity_instances.drain(start_idx..).collect();

        println!(
            "🔧🔧🔧 ENTITY_DRAIN: Extracted {} instances to process 🔧🔧🔧",
            pending.len()
        );

        for (instance, output_signals) in pending {
            println!(
                "🔧🔧🔧 ENTITY_DRAIN: Processing instance '{}' with {} output signals 🔧🔧🔧",
                instance.name,
                output_signals.len()
            );
            eprintln!(
                "[HIERARCHICAL] Adding instance '{}' with {} output signals",
                instance.name,
                output_signals.len()
            );

            // Create output signals for this instance
            for (signal_id, signal_name, signal_type, detection_cfg) in output_signals {
                let signal = Signal {
                    id: signal_id,
                    name: signal_name.clone(),
                    signal_type,
                    initial: None,
                    clock_domain: None,
                    span: None,
                    memory_config: None,
                    trace_config: None,
                    cdc_config: None,
                    breakpoint_config: None,
                    power_config: None,
                    safety_context: None,
                    detection_config: detection_cfg.clone(),
                    power_domain: None,
                };
                eprintln!(
                    "[HIERARCHICAL] Creating signal '{}' (id={}) for instance output (detection={})",
                    signal_name, signal_id.0, detection_cfg.is_some()
                );
                module.signals.push(signal);
            }

            // Add the module instance
            module.instances.push(instance);
        }
    }

    fn next_port_id(&mut self) -> PortId {
        let id = PortId(self.next_port_id);
        self.next_port_id += 1;
        id
    }

    fn next_signal_id(&mut self) -> SignalId {
        let id = SignalId(self.next_signal_id);
        self.next_signal_id += 1;
        id
    }

    fn next_variable_id(&mut self) -> VariableId {
        let id = VariableId(self.next_variable_id);
        self.next_variable_id += 1;
        id
    }

    /// Convert HirType to frontend Type
    /// Used for getting the actual function return type (BUG FIX for NCL tuple width mismatch)
    fn hir_type_to_frontend_type(hir_type: &Option<hir::HirType>) -> Option<Type> {
        use skalp_frontend::types::Width;
        let hir_ty = hir_type.as_ref()?;
        Some(match hir_ty {
            hir::HirType::Bit(n) => Type::Bit(Width::Fixed(*n)),
            hir::HirType::Logic(n) => Type::Logic(Width::Fixed(*n)),
            hir::HirType::Int(n) => Type::Int(Width::Fixed(*n)),
            hir::HirType::Nat(n) => Type::Nat(Width::Fixed(*n)),
            hir::HirType::Bool => Type::Bool,
            hir::HirType::Float16 | hir::HirType::Float32 | hir::HirType::Float64 => {
                // FP types are represented as bit vectors
                Type::Bit(Width::Fixed(32))
            }
            hir::HirType::Tuple(elements) => {
                // Convert each tuple element
                let element_types: Vec<Type> = elements
                    .iter()
                    .filter_map(|elem| Self::hir_type_to_frontend_type(&Some(elem.clone())))
                    .collect();
                Type::Tuple(element_types)
            }
            hir::HirType::Array(elem, size) => Type::Array {
                element_type: Box::new(
                    Self::hir_type_to_frontend_type(&Some((**elem).clone()))
                        .unwrap_or(Type::Bit(Width::Fixed(32))),
                ),
                size: *size,
            },
            _ => Type::Bit(Width::Fixed(32)), // Fallback for other types
        })
    }

    /// Convert frontend Type to MIR DataType
    /// Used for extracting element types from tuples (BUG FIX for NCL width mismatch)
    fn frontend_type_to_datatype(&self, ty: &Type) -> DataType {
        use skalp_frontend::types::Width;
        match ty {
            Type::Bit(Width::Fixed(n)) => DataType::Bit(*n as usize),
            Type::Logic(Width::Fixed(n)) => DataType::Bit(*n as usize),
            Type::Int(Width::Fixed(n)) => DataType::Bit(*n as usize),
            Type::Nat(Width::Fixed(n)) => DataType::Bit(*n as usize),
            Type::Bool => DataType::Bit(1),
            // For unknown/inferred widths, fallback to 32 bits
            Type::Bit(_) | Type::Logic(_) | Type::Int(_) | Type::Nat(_) => DataType::Bit(32),
            // Fixed-point: total width = integer + fractional
            Type::Fixed {
                integer_bits,
                fractional_bits,
            } => DataType::Bit((*integer_bits + *fractional_bits) as usize),
            // Other types default to 32 bits
            _ => DataType::Bit(32),
        }
    }

    /// Get the current variable resolution context
    /// This combines match_arm_prefix and inlining_context_stack into a unique context identifier
    fn get_current_context(&self) -> Option<String> {
        let mut parts = Vec::new();

        // Add inlining context stack (function nesting)
        if !self.inlining_context_stack.is_empty() {
            let inline_context = self
                .inlining_context_stack
                .iter()
                .map(|id| format!("fn{}", id))
                .collect::<Vec<_>>()
                .join("_");
            parts.push(inline_context);
        }

        // Add match arm prefix
        if let Some(ref prefix) = self.match_arm_prefix {
            parts.push(prefix.clone());
        }

        if parts.is_empty() {
            None
        } else {
            Some(parts.join("_"))
        }
    }

    fn next_process_id(&mut self) -> ProcessId {
        let id = ProcessId(self.next_process_id);
        self.next_process_id += 1;
        id
    }

    /// Try to synthesis-resolve a complex if-else-if chain
    /// Returns Some(ResolvedConditional) if this is a complex conditional assignment
    /// Returns None if this should remain as a regular if statement
    fn try_synthesis_resolve_if(
        &mut self,
        if_stmt: &hir::HirIfStatement,
    ) -> Option<ResolvedConditional> {
        // Only apply synthesis resolution to complex if-else-if chains, not simple if-else patterns
        if !self.is_complex_if_else_if_chain(if_stmt) {
            return None;
        }

        // Check if this is a conditional assignment pattern suitable for synthesis resolution
        if let Some((target, kind)) = self.extract_conditional_assignment_target(if_stmt) {
            // This is a conditional assignment - resolve it using synthesis approach
            let condition = self.convert_expression(&if_stmt.condition, 0)?;
            let original_if = IfStatement {
                condition: condition.clone(),
                then_block: self.convert_statements(&if_stmt.then_statements),
                else_block: if_stmt
                    .else_statements
                    .as_ref()
                    .map(|stmts| self.convert_statements(stmts)),
                span: None,
            };

            // Build priority mux from if-else-if chain
            let resolved = self.build_priority_mux_from_hir(if_stmt)?;

            Some(ResolvedConditional {
                target,
                kind,
                original: Box::new(original_if),
                resolved,
            })
        } else {
            None
        }
    }

    /// Extract the target signal and assignment kind from a conditional assignment pattern
    /// Returns Some((target, kind)) if all branches assign to the same signal
    fn extract_conditional_assignment_target(
        &mut self,
        if_stmt: &hir::HirIfStatement,
    ) -> Option<(LValue, AssignmentKind)> {
        // Check if then branch has exactly one assignment
        if let Some((then_target, then_kind)) =
            self.extract_single_assignment(&if_stmt.then_statements)
        {
            // Check else branch
            if let Some(else_stmts) = &if_stmt.else_statements {
                if let Some((else_target, else_kind)) =
                    self.extract_assignment_or_nested_if(else_stmts)
                {
                    // Both branches must assign to the same target with the same kind
                    if self.lvalues_match(&then_target, &else_target) && then_kind == else_kind {
                        return Some((then_target, then_kind));
                    }
                }
            }
        }
        None
    }

    /// Extract assignment from a single statement list or nested if
    fn extract_assignment_or_nested_if(
        &mut self,
        stmts: &[hir::HirStatement],
    ) -> Option<(LValue, AssignmentKind)> {
        if stmts.len() == 1 {
            match &stmts[0] {
                hir::HirStatement::Assignment(assign) => {
                    let target = self.convert_lvalue(&assign.lhs)?;
                    let kind = match assign.assignment_type {
                        hir::HirAssignmentType::NonBlocking => AssignmentKind::NonBlocking,
                        hir::HirAssignmentType::Blocking => AssignmentKind::Blocking,
                        hir::HirAssignmentType::Combinational => AssignmentKind::Blocking,
                    };
                    Some((target, kind))
                }
                hir::HirStatement::If(nested_if) => {
                    // Recursively check nested if
                    self.extract_conditional_assignment_target(nested_if)
                }
                _ => None,
            }
        } else {
            None
        }
    }

    /// Extract single assignment from statement list
    fn extract_single_assignment(
        &mut self,
        stmts: &[hir::HirStatement],
    ) -> Option<(LValue, AssignmentKind)> {
        if stmts.len() == 1 {
            if let hir::HirStatement::Assignment(assign) = &stmts[0] {
                let target = self.convert_lvalue(&assign.lhs)?;
                let kind = match assign.assignment_type {
                    hir::HirAssignmentType::NonBlocking => AssignmentKind::NonBlocking,
                    hir::HirAssignmentType::Blocking => AssignmentKind::Blocking,
                    hir::HirAssignmentType::Combinational => AssignmentKind::Blocking,
                };
                return Some((target, kind));
            }
        }
        None
    }

    /// Check if two LValues refer to the same target
    fn lvalues_match(&self, lval1: &LValue, lval2: &LValue) -> bool {
        match (lval1, lval2) {
            (LValue::Signal(id1), LValue::Signal(id2)) => id1 == id2,
            (LValue::Variable(id1), LValue::Variable(id2)) => id1 == id2,
            (LValue::Port(id1), LValue::Port(id2)) => id1 == id2,
            _ => false, // TODO: Handle more complex LValue matching
        }
    }

    /// Build priority mux from HIR if-else-if chain
    fn build_priority_mux_from_hir(
        &mut self,
        if_stmt: &hir::HirIfStatement,
    ) -> Option<PriorityMux> {
        let mut cases = Vec::new();

        // Collect condition-value pairs
        self.collect_conditional_cases_from_hir(if_stmt, &mut cases)?;

        // Get default value (last else clause)
        let default = self.extract_default_value_from_hir(if_stmt)?;

        Some(PriorityMux { cases, default })
    }

    /// Collect conditional cases from HIR if-else-if chain
    fn collect_conditional_cases_from_hir(
        &mut self,
        if_stmt: &hir::HirIfStatement,
        cases: &mut Vec<ConditionalCase>,
    ) -> Option<()> {
        // Add current condition-value pair
        let condition = self.convert_expression(&if_stmt.condition, 0)?;
        let value = self.extract_assignment_value(&if_stmt.then_statements)?;

        cases.push(ConditionalCase { condition, value });

        // Process else branch
        if let Some(else_stmts) = &if_stmt.else_statements {
            if else_stmts.len() == 1 {
                if let hir::HirStatement::If(nested_if) = &else_stmts[0] {
                    // Recursive case: else-if
                    self.collect_conditional_cases_from_hir(nested_if, cases)?;
                }
                // Terminal case: final else handled in extract_default_value_from_hir
            }
        }

        Some(())
    }

    /// Extract assignment value from statement list
    fn extract_assignment_value(&mut self, stmts: &[hir::HirStatement]) -> Option<Expression> {
        if stmts.len() == 1 {
            if let hir::HirStatement::Assignment(assign) = &stmts[0] {
                return self.convert_expression(&assign.rhs, 0);
            }
        }
        None
    }

    /// Extract default value from the final else clause
    fn extract_default_value_from_hir(
        &mut self,
        if_stmt: &hir::HirIfStatement,
    ) -> Option<Expression> {
        if let Some(else_stmts) = &if_stmt.else_statements {
            if else_stmts.len() == 1 {
                match &else_stmts[0] {
                    hir::HirStatement::Assignment(assign) => {
                        // This is the final else clause
                        return self.convert_expression(&assign.rhs, 0);
                    }
                    hir::HirStatement::If(nested_if) => {
                        // Continue searching in nested if
                        return self.extract_default_value_from_hir(nested_if);
                    }
                    _ => {}
                }
            }
        }

        // If no explicit default, use zero (synthesizers typically do this)
        Some(Expression::with_unknown_type(ExpressionKind::Literal(
            Value::Integer(0),
        )))
    }

    /// Check if this is a complex if-else-if chain (multiple conditions)
    /// rather than a simple if-else pattern (like reset logic)
    fn is_complex_if_else_if_chain(&self, if_stmt: &hir::HirIfStatement) -> bool {
        // Count the number of conditions in the chain
        let condition_count = self.count_if_else_conditions(if_stmt);

        // Only consider it complex if there are multiple conditions (2+)
        // This excludes simple if-else patterns like reset logic
        condition_count >= 2
    }

    /// Count the number of conditions in an if-else-if chain
    #[allow(clippy::only_used_in_recursion)]
    fn count_if_else_conditions(&self, if_stmt: &hir::HirIfStatement) -> usize {
        let mut count = 1; // Count the initial if condition

        // Check if the else branch contains another if statement (else-if)
        if let Some(else_stmts) = &if_stmt.else_statements {
            if else_stmts.len() == 1 {
                if let hir::HirStatement::If(nested_if) = &else_stmts[0] {
                    // Recursively count nested if-else-if conditions
                    count += self.count_if_else_conditions(nested_if);
                }
            }
        }

        count
    }

    /// Flatten a port with struct/vector type into multiple MIR ports
    /// Returns the list of created ports and flattened field information
    ///
    /// **New Implementation:** Uses the shared TypeFlattener module for consistent flattening
    fn flatten_port(
        &mut self,
        base_name: &str,
        port_type: &DataType,
        direction: PortDirection,
        physical_constraints: Option<skalp_frontend::hir::PhysicalConstraints>,
        span: Option<SourceSpan>,
    ) -> (Vec<Port>, Vec<FlattenedField>) {
        // Use shared TypeFlattener with current port ID counter
        let mut flattener = TypeFlattener::new(self.next_port_id);
        let (ports, type_fields) = flattener.flatten_port_with_span(
            base_name,
            port_type,
            direction,
            physical_constraints,
            span,
        );

        // Update our port ID counter based on how many ports were created
        self.next_port_id += ports.len() as u32;

        // Convert TypeFlattenedField to our local FlattenedField type
        let fields: Vec<FlattenedField> = type_fields
            .into_iter()
            .map(|tf| FlattenedField {
                id: tf.id,
                field_path: tf.field_path,
                leaf_type: tf.leaf_type,
            })
            .collect();

        (ports, fields)
    }

    /// Flatten a signal with struct/vector type into multiple MIR signals
    /// Returns the list of created signals and flattened field information
    ///
    /// **New Implementation:** Uses the shared TypeFlattener module for consistent flattening
    fn flatten_signal(
        &mut self,
        base_name: &str,
        signal_type: &DataType,
        initial: Option<Value>,
        clock_domain: Option<ClockDomainId>,
        span: Option<SourceSpan>,
    ) -> (Vec<Signal>, Vec<FlattenedField>) {
        // Use shared TypeFlattener with current signal ID counter
        let mut flattener = TypeFlattener::new(self.next_signal_id);
        let (signals, type_fields) =
            flattener.flatten_signal_with_span(base_name, signal_type, initial, clock_domain, span);

        // Update our signal ID counter based on how many signals were created
        self.next_signal_id += signals.len() as u32;

        // Convert TypeFlattenedField to our local FlattenedField type
        let fields: Vec<FlattenedField> = type_fields
            .into_iter()
            .map(|tf| FlattenedField {
                id: tf.id,
                field_path: tf.field_path,
                leaf_type: tf.leaf_type,
            })
            .collect();

        (signals, fields)
    }
}

impl Default for HirToMir<'_> {
    fn default() -> Self {
        Self::new()
    }
}

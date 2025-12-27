//! HIR - High-level Intermediate Representation
//!
//! Converts the AST into a simplified IR suitable for further processing

use crate::ast::SourceFile;
use crate::safety_attributes::ModuleSafetyDefinitions;
use crate::span::SourceSpan;
use serde::{Deserialize, Serialize};

// ============================================================================
// Detection Signal Configuration (ISO 26262 Safety Analysis)
// ============================================================================

/// Detection mode for safety mechanism signals
/// Determines when the detection is active and how it contributes to metrics
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, Default)]
pub enum DetectionMode {
    /// Continuous runtime detection - contributes to SPFM
    /// Faults are detected immediately during operation
    #[default]
    Continuous,
    /// Boot-time detection (e.g., BIST) - contributes to LFM only
    /// Faults are detected at power-on before operation begins
    Boot,
    /// Periodic detection - contributes to LFM with interval factor
    /// Faults are detected within the specified interval
    Periodic,
    /// On-demand detection - requires software trigger
    /// Does not automatically contribute to metrics
    OnDemand,
}

/// Configuration for detection signals
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct DetectionConfig {
    /// When this detection is active
    pub mode: DetectionMode,
    /// For Periodic mode: detection interval in milliseconds
    /// Used for PMHF calculation with time-dependent coverage
    #[serde(skip_serializing_if = "Option::is_none")]
    pub interval_ms: Option<u32>,
    /// If true, this detection signal MUST be in an always-on power domain
    /// Use for watchdogs, voltage monitors, and other mechanisms that must
    /// survive power domain failures. Verification will warn if the signal
    /// is placed in a switchable domain.
    #[serde(default, skip_serializing_if = "std::ops::Not::not")]
    pub requires_always_on: bool,
}

// ============================================================================
// Power Domain Configuration (ISO 26262 CCF Analysis)
// ============================================================================

/// Configuration for power domain assignment
/// Used for Common Cause Failure (CCF) analysis - power supply failures
/// affect all cells in a domain simultaneously
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct PowerDomainConfig {
    /// Power domain name (e.g., "vdd_core", "vdd_io", "vdd_analog")
    pub domain_name: String,
    /// Optional nominal voltage specification (V)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub voltage: Option<f64>,
    /// True if this is an always-on domain (cannot be power-gated)
    #[serde(default)]
    pub is_always_on: bool,
}

// ============================================================================
// SEooC (Safety Element out of Context) Configuration
// ============================================================================

/// SEooC configuration for an entity
/// Per ISO 26262-10:9, a SEooC is developed without full knowledge of the
/// integrating system. It declares assumptions about external safety mechanisms
/// that the integrator must provide.
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct SeoocConfig {
    /// Target ASIL level for this SEooC
    pub target_asil: String, // "A", "B", "C", "D", "QM"
    /// Optional rationale for SEooC status
    #[serde(skip_serializing_if = "Option::is_none")]
    pub rationale: Option<String>,
    /// Assumed external mechanisms (entity-level, NOT port-level)
    /// The response path (interrupt, reset, shutdown) is outside SEooC boundary
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub assumed_mechanisms: Vec<AssumedMechanismConfig>,
}

/// Assumed external mechanism - a requirement on the integrating system
/// NOT tied to any specific port - the response path is outside this SEooC
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AssumedMechanismConfig {
    /// Unique identifier for this assumed mechanism (for traceability)
    pub id: String,
    /// Type of mechanism (voltage_monitor, watchdog, clock_monitor, etc.)
    pub mechanism_type: String,
    /// Fault types this external mechanism is assumed to detect
    /// e.g., ["VoltageDropout", "GroundBounce"] or ["StuckAt0", "StuckAt1"]
    pub covers: Vec<String>,
    /// Human-readable description
    #[serde(skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,
}

/// High-level Intermediate Representation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Hir {
    /// Module name
    pub name: String,
    /// Entities in this HIR
    pub entities: Vec<HirEntity>,
    /// Implementations
    pub implementations: Vec<HirImplementation>,
    /// Protocols
    pub protocols: Vec<HirProtocol>,
    /// Intents
    pub intents: Vec<HirIntent>,
    /// Requirements
    pub requirements: Vec<HirRequirement>,
    /// Trait definitions
    pub trait_definitions: Vec<HirTraitDefinition>,
    /// Trait implementations
    pub trait_implementations: Vec<HirTraitImplementation>,
    /// Type aliases
    pub type_aliases: Vec<HirTypeAlias>,
    /// User-defined types (struct, enum, union definitions)
    pub user_defined_types: Vec<HirUserDefinedType>,
    /// Global physical constraints
    pub global_constraints: Vec<GlobalConstraint>,
    /// Module declarations
    pub modules: Vec<HirModule>,
    /// Use statements (imports)
    pub imports: Vec<HirImport>,
    /// Top-level functions (including const functions)
    pub functions: Vec<HirFunction>,
    /// Module-level safety definitions (safety goals, mechanisms, HSI)
    /// ISO 26262 compliance support
    #[serde(default, skip_serializing_if = "ModuleSafetyDefinitions::is_empty")]
    pub safety_definitions: ModuleSafetyDefinitions,
}

/// Entity in HIR
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HirEntity {
    /// Entity identifier
    pub id: EntityId,
    /// Entity name
    pub name: String,
    /// Visibility
    pub visibility: HirVisibility,
    /// Ports
    pub ports: Vec<HirPort>,
    /// Generic parameters
    pub generics: Vec<HirGeneric>,
    /// Clock domain parameters
    pub clock_domains: Vec<HirClockDomain>,
    /// Continuous assignments in entity body (for direct signal assignments)
    pub assignments: Vec<HirAssignment>,
    /// Signals declared in entity body
    pub signals: Vec<HirSignal>,
    /// Source location span (for error reporting)
    pub span: Option<SourceSpan>,
    /// Pipeline configuration (from #[pipeline(stages=N)] attribute)
    pub pipeline_config: Option<PipelineConfig>,
    /// Vendor IP configuration (from #[xilinx_ip], #[intel_ip], etc. attributes)
    pub vendor_ip_config: Option<VendorIpConfig>,
    /// Power domain declarations (mirrors clock_domains pattern)
    pub power_domains: Vec<HirPowerDomain>,
    /// Power domain configuration for CCF analysis (from #[power_domain("name")] attribute)
    /// When set on entity, all ports/signals inherit this domain unless overridden
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub power_domain_config: Option<PowerDomainConfig>,
    /// Safety mechanism configuration (from #[safety_mechanism(...)] attribute)
    /// When present, indicates this entity is a reusable safety mechanism.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub safety_mechanism_config: Option<SafetyMechanismConfig>,
    /// SEooC (Safety Element out of Context) configuration
    /// When present, this entity is developed without full system knowledge.
    /// Skalp will calculate required DC for external mechanisms.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub seooc_config: Option<SeoocConfig>,
    /// Compiled IP configuration (from #[compiled_ip("path.skb")] attribute)
    /// When present, this entity's implementation comes from a pre-compiled .skb file.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub compiled_ip_config: Option<CompiledIpConfig>,
}

/// Safety mechanism configuration for entities (from #[safety_mechanism(...)] attribute)
/// Marks an entity as a reusable safety mechanism component in the stdlib.
///
/// Example:
/// ```skalp
/// #[safety_mechanism(type: crc, dc: 99.0)]
/// entity Crc8Checker<'clk> { ... }
/// ```
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct SafetyMechanismConfig {
    /// Mechanism type (crc, ecc, tmr, lockstep, watchdog, etc.)
    pub mechanism_type: Option<String>,
    /// Default diagnostic coverage (percentage)
    pub dc: Option<f64>,
    /// Default latent coverage (percentage)
    pub lc: Option<f64>,
    /// Description of the safety mechanism
    pub description: Option<String>,
}

impl SafetyMechanismConfig {
    /// Create an empty safety mechanism config
    pub fn new() -> Self {
        Self::default()
    }

    /// Create a config with type and DC
    pub fn with_type_and_dc(mechanism_type: &str, dc: f64) -> Self {
        Self {
            mechanism_type: Some(mechanism_type.to_string()),
            dc: Some(dc),
            ..Default::default()
        }
    }

    /// Check if this config is populated
    pub fn is_populated(&self) -> bool {
        self.mechanism_type.is_some() || self.dc.is_some() || self.lc.is_some()
    }
}

/// Implementation in HIR
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HirImplementation {
    /// Target entity
    pub entity: EntityId,
    /// Signals
    pub signals: Vec<HirSignal>,
    /// Variables
    pub variables: Vec<HirVariable>,
    /// Constants
    pub constants: Vec<HirConstant>,
    /// Functions
    pub functions: Vec<HirFunction>,
    /// Event blocks
    pub event_blocks: Vec<HirEventBlock>,
    /// Assignments
    pub assignments: Vec<HirAssignment>,
    /// Module instances
    pub instances: Vec<HirInstance>,
    /// Coverage groups
    pub covergroups: Vec<HirCovergroup>,
    /// Formal verification blocks
    pub formal_blocks: Vec<HirFormalBlock>,
    /// Standalone statements (assertions, etc.) in impl blocks
    pub statements: Vec<HirStatement>,
}

/// Module instance in HIR
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HirInstance {
    /// Instance identifier
    pub id: InstanceId,
    /// Instance name
    pub name: String,
    /// Entity to instantiate
    pub entity: EntityId,
    /// Positional generic arguments (for monomorphization)
    pub generic_args: Vec<HirExpression>,
    /// Named generic arguments (parameter_name -> expression)
    /// Supports syntax like `Entity<WIDTH: 32, DEPTH: 16>`
    #[serde(default)]
    pub named_generic_args: std::collections::HashMap<String, HirExpression>,
    /// Port connections
    pub connections: Vec<HirConnection>,
    /// Safety configuration (from #[implements(...)] attribute)
    /// When present, indicates this instance implements a safety mechanism from a safety_goal.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub safety_config: Option<SafetyConfig>,
}

/// Named generic argument (for clearer code)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HirNamedGenericArg {
    /// Parameter name (e.g., "WIDTH", "DEPTH")
    pub name: String,
    /// Argument expression (e.g., `32`, `fp32`)
    pub expr: HirExpression,
}

/// Port connection in HIR
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HirConnection {
    /// Port name
    pub port: String,
    /// Connected expression
    pub expr: HirExpression,
}

/// Port in HIR
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HirPort {
    /// Port identifier
    pub id: PortId,
    /// Port name
    pub name: String,
    /// Port direction
    pub direction: HirPortDirection,
    /// Port type
    pub port_type: HirType,
    /// Physical constraints (pin mapping, I/O characteristics)
    pub physical_constraints: Option<PhysicalConstraints>,
    /// Detection signal configuration (for safety analysis)
    /// Set via #[detection_signal] or #[detection_signal(mode = "...")] attribute
    /// None = not a detection signal, Some = detection signal with config
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub detection_config: Option<DetectionConfig>,
    /// Power domain configuration (for CCF analysis)
    /// Set via #[power_domain("name")] attribute
    /// None = default domain, Some = explicit domain assignment
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub power_domain_config: Option<PowerDomainConfig>,
    /// Isolation cell configuration
    /// Set via #[isolation(clamp = 0|1|hold)] attribute on output ports
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub isolation_config: Option<IsolationConfig>,
    /// Retention flip-flop configuration
    /// Set via #[retention] attribute on state registers
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub retention_config: Option<RetentionConfig>,
}

impl HirPort {
    /// Check if this port is a detection signal (any mode)
    /// Backward compatibility helper
    pub fn is_detection_signal(&self) -> bool {
        self.detection_config.is_some()
    }

    /// Get the detection mode if this is a detection signal
    pub fn detection_mode(&self) -> Option<DetectionMode> {
        self.detection_config.as_ref().map(|c| c.mode)
    }
}

/// Port direction in HIR
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum HirPortDirection {
    Input,
    Output,
    Bidirectional,
    Protocol,
}

/// Signal in HIR
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HirSignal {
    /// Signal identifier
    pub id: SignalId,
    /// Signal name
    pub name: String,
    /// Signal type
    pub signal_type: HirType,
    /// Initial value
    pub initial_value: Option<HirExpression>,
    /// Clock domain this signal belongs to (inferred from assignments)
    pub clock_domain: Option<ClockDomainId>,
    /// Source location span (for error reporting)
    pub span: Option<SourceSpan>,
    /// Memory configuration (from #[memory(depth=N)] attribute)
    /// When present, signal represents a memory array that should be
    /// synthesized as BRAM/SRAM rather than discrete registers.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub memory_config: Option<MemoryConfig>,
    /// Trace configuration (from #[trace] attribute)
    /// When present, signal should be auto-exported to simulation traces/waveforms.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub trace_config: Option<TraceConfig>,
    /// CDC configuration (from #[cdc] attribute)
    /// When present, signal crosses clock domains and needs synchronization.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub cdc_config: Option<CdcConfig>,
    /// Breakpoint configuration (from #[breakpoint] attribute)
    /// When present, generates SVA assertions for debugging.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub breakpoint_config: Option<BreakpointConfig>,
    /// Power intent configuration (from #[retention], #[isolation], #[pdc], #[level_shift] attributes)
    /// When present, specifies power domain crossing, retention, or isolation requirements.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub power_config: Option<PowerConfig>,
    /// Safety configuration (from #[implements(...)] attribute)
    /// When present, indicates this signal implements a safety mechanism from a safety_goal.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub safety_config: Option<SafetyConfig>,
    /// Power domain assignment (from signal<'domain> lifetime syntax)
    /// When present, indicates which power domain this signal belongs to.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub power_domain: Option<String>,
}

/// Variable in HIR
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HirVariable {
    /// Variable identifier
    pub id: VariableId,
    /// Variable name
    pub name: String,
    /// Variable type
    pub var_type: HirType,
    /// Initial value
    pub initial_value: Option<HirExpression>,
    /// Source location for error reporting
    #[serde(skip_serializing_if = "Option::is_none")]
    pub span: Option<crate::SourceSpan>,
}

/// Constant in HIR
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HirConstant {
    /// Constant identifier
    pub id: ConstantId,
    /// Constant name
    pub name: String,
    /// Constant type
    pub const_type: HirType,
    /// Constant value
    pub value: HirExpression,
}

/// Function declaration inside impl block
///
/// Represents combinational logic functions that can be called from
/// sequential blocks, combinational assignments, or other functions.
///
/// Const functions can be evaluated at compile time.
///
/// Generic functions (Phase 1) support parametric polymorphism.
/// During HIR→MIR transformation, generic functions are monomorphized
/// (specialized for each set of type arguments used).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HirFunction {
    /// Function identifier
    pub id: FunctionId,
    /// Whether this is a const function
    pub is_const: bool,
    /// Function name
    pub name: String,
    /// Generic parameters (Phase 1: type and const parameters)
    pub generics: Vec<HirGeneric>,
    /// Function parameters
    pub params: Vec<HirParameter>,
    /// Return type (None for void functions)
    pub return_type: Option<HirType>,
    /// Function body
    pub body: Vec<HirStatement>,
    /// Source location span (for error reporting)
    pub span: Option<SourceSpan>,
    /// Pipeline configuration from `#[pipeline(stages=N)]` attribute
    #[serde(skip_serializing_if = "Option::is_none")]
    pub pipeline_config: Option<PipelineConfig>,
}

/// Event block in HIR
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HirEventBlock {
    /// Block identifier
    pub id: BlockId,
    /// Event triggers
    pub triggers: Vec<HirEventTrigger>,
    /// Statements
    pub statements: Vec<HirStatement>,
}

/// Event trigger in HIR
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HirEventTrigger {
    /// Signal being monitored (can be a port or signal)
    pub signal: HirEventSignal,
    /// Edge type
    pub edge: HirEdgeType,
}

/// Signal reference in event trigger
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum HirEventSignal {
    /// Port reference
    Port(PortId),
    /// Signal reference
    Signal(SignalId),
}

/// Edge types in HIR
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum HirEdgeType {
    Rising,
    Falling,
    Both,
    Active,   // For reset.active (active level)
    Inactive, // For reset.inactive (inactive level)
}

/// Reset polarity in HIR
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
pub enum HirResetPolarity {
    ActiveHigh,
    ActiveLow,
}

/// Assignment in HIR
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HirAssignment {
    /// Assignment identifier
    pub id: AssignmentId,
    /// Left-hand side
    pub lhs: HirLValue,
    /// Assignment type
    pub assignment_type: HirAssignmentType,
    /// Right-hand side
    pub rhs: HirExpression,
}

/// Assignment types in HIR
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum HirAssignmentType {
    Blocking,
    NonBlocking,
    Combinational,
}

/// Left-hand side value in HIR
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum HirLValue {
    Signal(SignalId),
    Variable(VariableId),
    Port(PortId),
    Index(Box<HirLValue>, HirExpression),
    Range(Box<HirLValue>, HirExpression, HirExpression),
    FieldAccess { base: Box<HirLValue>, field: String },
}

/// Statements in HIR
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum HirStatement {
    Assignment(HirAssignment),
    If(HirIfStatement),
    Match(HirMatchStatement),
    Flow(HirFlowStatement),
    Block(Vec<HirStatement>),
    Assert(HirAssertStatement),
    Assume(HirAssumeStatement),
    Property(HirPropertyStatement),
    Cover(HirCoverStatement),
    Let(HirLetStatement),
    Return(Option<HirExpression>),
    Expression(HirExpression),
    For(HirForStatement),
    GenerateFor(HirGenerateFor),
    GenerateIf(HirGenerateIf),
    GenerateMatch(HirGenerateMatch),
}

/// If statement in HIR
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HirIfStatement {
    /// Condition
    pub condition: HirExpression,
    /// Then statements
    pub then_statements: Vec<HirStatement>,
    /// Else statements
    pub else_statements: Option<Vec<HirStatement>>,
    /// Mux style hint from intent/attribute (defaults to Priority)
    #[serde(default)]
    pub mux_style: MuxStyle,
}

/// For loop statement in HIR
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HirForStatement {
    /// Unique identifier for this for loop
    pub id: ForLoopId,
    /// Loop iterator variable name
    pub iterator: String,
    /// Iterator variable ID (for use in body)
    pub iterator_var_id: VariableId,
    /// Range to iterate over
    pub range: HirRange,
    /// Loop body statements
    pub body: Vec<HirStatement>,
    /// Unroll configuration (None = sequential loop)
    pub unroll: Option<UnrollConfig>,
}

/// Range expression in HIR (for `start..end` or `start..=end`)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HirRange {
    /// Start of range (inclusive)
    pub start: HirExpression,
    /// End of range
    pub end: HirExpression,
    /// Whether the end is inclusive (..= vs ..)
    pub inclusive: bool,
    /// Optional step value for generate-for loops
    pub step: Option<Box<HirExpression>>,
}

/// Loop unrolling configuration
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum UnrollConfig {
    /// Fully unroll the loop
    Full,
    /// Unroll by a specific factor
    Factor(u32),
}

/// Pipeline configuration for automatic pipeline register insertion
///
/// Specifies how a function/entity should be pipelined. Used with `#[pipeline(...)]` attribute.
///
/// # Compilation Flow
///
/// This config propagates through the entire compilation pipeline:
///
/// - **HIR/MIR**: `stages` is used to insert pipeline registers at IR level
/// - **SIR** (simulation): Uses `stages` for accurate cycle-level latency simulation
/// - **LIR** (synthesis): Uses `target_freq` for timing-driven optimization after
///   technology mapping, when real timing information is available
///
/// # Usage
///
/// ```text
/// #[pipeline(stages=3)]                           // Fixed 3 pipeline stages
/// #[pipeline(stages=4, target_freq=100_000_000)]  // 4 stages, verify 100MHz target in LIR
/// #[pipeline(stages=2, auto_balance=true)]        // 2 stages, auto-balance in LIR
/// ```
///
/// # Design Notes
///
/// - `stages`: Acted upon at HIR/MIR/SIR level (technology-independent)
/// - `target_freq`: Deferred to LIR where actual timing is known post-tech-mapping
/// - `auto_balance`: Hint for LIR retiming passes to optimize stage boundaries
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct PipelineConfig {
    /// Number of pipeline stages to insert (used at HIR/MIR/SIR level)
    pub stages: u32,
    /// Target clock frequency in Hz - used by LIR for timing verification/optimization
    /// after technology mapping when real timing information is available
    pub target_freq: Option<u64>,
    /// Enable automatic balancing of pipeline stages (hint for LIR retiming)
    pub auto_balance: bool,
}

impl PipelineConfig {
    /// Create a pipeline config with specified number of stages
    pub fn with_stages(stages: u32) -> Self {
        Self {
            stages,
            target_freq: None,
            auto_balance: false,
        }
    }
}

/// Memory configuration for RAM/ROM inference
///
/// Specifies that a signal should be synthesized as a memory block (BRAM/SRAM/ROM).
/// Used with `#[memory(...)]` attribute on signal declarations.
///
/// # Usage
///
/// ```text
/// #[memory(depth=1024, width=64)]
/// signal mem: bit[64][1024];  // Infers single-port BRAM
///
/// #[memory(depth=256, width=32, ports=2)]
/// signal dual_mem: bit[32][256];  // Infers dual-port BRAM
///
/// #[memory(depth=128, width=16, style=distributed)]
/// signal small_mem: bit[16][128];  // Infers distributed RAM (LUT-based)
/// ```
///
/// # Memory Types
///
/// - `block`: Block RAM (BRAM) - default for larger memories
/// - `distributed`: Distributed RAM (LUT-based) - for smaller memories
/// - `ultra`: UltraRAM (Xilinx) - for very large memories
/// - `register`: Register file - for small, high-port-count memories
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct MemoryConfig {
    /// Memory depth (number of entries)
    pub depth: u32,
    /// Data width in bits (can be inferred from signal type)
    pub width: Option<u32>,
    /// Number of read/write ports (default: 1)
    pub ports: u32,
    /// Memory style hint for synthesis
    pub style: MemoryStyle,
    /// Read latency in cycles (default: 1 for BRAM)
    pub read_latency: u32,
    /// Whether memory is read-only (ROM)
    pub read_only: bool,
}

/// Memory implementation style hints for synthesis tools
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Serialize, Deserialize)]
pub enum MemoryStyle {
    /// Automatic inference (default) - let synthesis tool decide
    #[default]
    Auto,
    /// Block RAM (BRAM)
    Block,
    /// Distributed RAM (LUT-based)
    Distributed,
    /// UltraRAM (Xilinx-specific)
    Ultra,
    /// Register file
    Register,
}

impl MemoryConfig {
    /// Create a basic memory config with specified depth
    pub fn with_depth(depth: u32) -> Self {
        Self {
            depth,
            width: None,
            ports: 1,
            style: MemoryStyle::Auto,
            read_latency: 1,
            read_only: false,
        }
    }

    /// Create a memory config with depth and width
    pub fn with_depth_and_width(depth: u32, width: u32) -> Self {
        Self {
            depth,
            width: Some(width),
            ports: 1,
            style: MemoryStyle::Auto,
            read_latency: 1,
            read_only: false,
        }
    }
}

/// Trace configuration for debug/waveform export
///
/// Specifies that a signal should be automatically included in simulation
/// traces and waveform dumps. Used with `#[trace]` attribute on signal declarations.
///
/// # Usage
///
/// ```text
/// #[trace]
/// signal debug_state: bit[8];  // Auto-exported to VCD/waveform
///
/// #[trace(group = "control")]
/// signal fsm_state: bit[4];  // Grouped trace signal
///
/// #[trace(radix = hex)]
/// signal data_bus: bit[32];  // Display as hex in waveform viewer
/// ```
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct TraceConfig {
    /// Optional group name for organizing signals in waveform viewers
    pub group: Option<String>,
    /// Display radix hint for waveform viewers
    pub radix: TraceRadix,
    /// Optional custom display name (defaults to signal name)
    pub display_name: Option<String>,
}

/// Display radix for traced signals in waveform viewers
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Serialize, Deserialize)]
pub enum TraceRadix {
    /// Binary display (default)
    #[default]
    Binary,
    /// Hexadecimal display
    Hex,
    /// Decimal display (unsigned)
    Unsigned,
    /// Decimal display (signed)
    Signed,
    /// ASCII character display
    Ascii,
}

impl TraceConfig {
    /// Create a basic trace config (all defaults)
    pub fn new() -> Self {
        Self::default()
    }

    /// Create a trace config with a group name
    pub fn with_group(group: impl Into<String>) -> Self {
        Self {
            group: Some(group.into()),
            ..Default::default()
        }
    }
}

/// Breakpoint configuration for debug assertions
///
/// Generates SystemVerilog assertions (SVA) that can trigger simulation
/// breakpoints or coverage events when conditions are met.
///
/// # Example
/// ```skalp
/// // Simple breakpoint on signal value
/// #[breakpoint]
/// signal error_flag: bit;
///
/// // Conditional breakpoint with expression
/// #[breakpoint(condition = "count > 100")]
/// signal overflow_counter: bit[8];
///
/// // Named breakpoint for identification
/// #[breakpoint(name = "FSM_ERROR", condition = "state == 0xF")]
/// signal fsm_state: bit[4];
///
/// // Breakpoint with message
/// #[breakpoint(message = "Unexpected reset")]
/// signal async_reset: bit;
/// ```
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct BreakpointConfig {
    /// Optional condition expression (Skalp expression syntax)
    /// If None, breakpoint triggers when signal changes
    pub condition: Option<String>,
    /// Optional name for identifying this breakpoint in waveforms/logs
    pub name: Option<String>,
    /// Optional message to display when breakpoint triggers
    pub message: Option<String>,
    /// Whether this is an error breakpoint (simulation should stop)
    pub is_error: bool,
}

impl BreakpointConfig {
    /// Create a basic breakpoint config (triggers on any change)
    pub fn new() -> Self {
        Self::default()
    }

    /// Create a breakpoint with a condition
    pub fn with_condition(condition: impl Into<String>) -> Self {
        Self {
            condition: Some(condition.into()),
            ..Default::default()
        }
    }

    /// Create a named breakpoint with a condition
    pub fn named(name: impl Into<String>, condition: impl Into<String>) -> Self {
        Self {
            name: Some(name.into()),
            condition: Some(condition.into()),
            ..Default::default()
        }
    }
}

/// Clock Domain Crossing (CDC) configuration
///
/// Specifies synchronizer configuration for signals crossing clock domains.
/// Works with the lifetime-based clock domain system (`'domain` syntax).
///
/// # Integration with Clock Domain Lifetimes
///
/// SKALP uses Rust-style lifetimes for compile-time CDC checking:
/// ```text
/// signal data: logic<'fast>[32]   // Signal in 'fast domain
/// signal sync: logic<'slow>[32]   // Signal in 'slow domain
/// ```
///
/// The `#[cdc]` attribute specifies HOW to synchronize when crossing:
/// ```text
/// // Type system detects the crossing; attribute configures synchronizer
/// #[cdc(sync_stages = 2, from = 'fast, to = 'slow)]
/// signal cross_domain: logic<'slow>[8];
/// ```
///
/// # Usage Examples
///
/// ```text
/// // Basic 2-stage synchronizer (default)
/// #[cdc]
/// signal async_input: bit;
///
/// // 3-stage for higher MTBF
/// #[cdc(sync_stages = 3)]
/// signal metastable_input: bit;
///
/// // With explicit domain annotation (references lifetime names)
/// #[cdc(from = 'src, to = 'dst, sync_stages = 2)]
/// signal cross_domain: logic<'dst>[8];
///
/// // Gray code synchronizer for multi-bit values
/// #[cdc(cdc_type = gray, sync_stages = 2)]
/// signal counter_sync: bit[8];
///
/// // Pulse synchronizer for edge detection
/// #[cdc(cdc_type = pulse)]
/// signal trigger_sync: bit;
/// ```
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct CdcConfig {
    /// Number of synchronizer flip-flop stages (default: 2)
    pub sync_stages: u32,
    /// Source clock domain lifetime name (e.g., "fast" from 'fast)
    /// References the lifetime annotation on the source signal's type
    pub from_domain: Option<String>,
    /// Destination clock domain lifetime name (e.g., "slow" from 'slow)
    /// References the lifetime annotation on the destination signal's type
    pub to_domain: Option<String>,
    /// CDC synchronization pattern type
    pub cdc_type: CdcType,
}

/// Type of CDC synchronization pattern
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Serialize, Deserialize)]
pub enum CdcType {
    /// Simple 2-FF synchronizer (default) - for single-bit signals
    #[default]
    TwoFF,
    /// Gray code synchronizer - for multi-bit counters
    Gray,
    /// Handshake synchronizer - for control signals
    Handshake,
    /// Pulse synchronizer - for edge detection across domains
    Pulse,
    /// FIFO-based synchronizer - for data buses
    AsyncFifo,
}

impl CdcConfig {
    /// Create a basic CDC config with default 2 stages
    pub fn new() -> Self {
        Self {
            sync_stages: 2,
            ..Default::default()
        }
    }

    /// Create a CDC config with specified stages
    pub fn with_stages(stages: u32) -> Self {
        Self {
            sync_stages: stages,
            ..Default::default()
        }
    }
}

/// Vendor IP configuration for wrapping vendor-specific IP cores
///
/// Used to mark an entity as a wrapper for vendor IP (Xilinx, Intel, etc.)
/// The entity's ports map directly to the IP's ports.
///
/// # Example
/// ```skalp
/// // Basic Xilinx FIFO wrapper
/// #[xilinx_ip("xpm_fifo_sync")]
/// entity SyncFifo<WIDTH: 32, DEPTH: 512> {
///     in  wr_clk: clock,
///     in  wr_en: bit,
///     in  din: bit[WIDTH],
///     out full: bit,
///     in  rd_en: bit,
///     out dout: bit[WIDTH],
///     out empty: bit,
/// }
///
/// // With custom parameters
/// #[xilinx_ip(name = "xpm_memory_spram", library = "xpm")]
/// entity SinglePortRam<WIDTH: 32, DEPTH: 1024> { ... }
///
/// // Intel/Altera IP
/// #[intel_ip("altera_fifo")]
/// entity AlteraFifo { ... }
/// ```
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct VendorIpConfig {
    /// IP core name (e.g., "xpm_fifo_sync", "altera_fifo")
    pub ip_name: String,
    /// Vendor (Xilinx, Intel, Lattice, etc.)
    pub vendor: VendorType,
    /// Optional library name (e.g., "xpm" for Xilinx Parameterized Macros)
    pub library: Option<String>,
    /// Optional version constraint
    pub version: Option<String>,
    /// Whether to generate a black-box module or instantiate directly
    pub black_box: bool,
    /// Additional vendor-specific parameters as key-value pairs
    pub parameters: Vec<(String, String)>,
}

/// Supported FPGA vendors
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Serialize, Deserialize)]
pub enum VendorType {
    /// Xilinx (AMD) FPGAs
    #[default]
    Xilinx,
    /// Intel (Altera) FPGAs
    Intel,
    /// Lattice FPGAs
    Lattice,
    /// Generic vendor (black-box)
    Generic,
}

impl VendorIpConfig {
    /// Create a Xilinx IP config
    pub fn xilinx(ip_name: impl Into<String>) -> Self {
        Self {
            ip_name: ip_name.into(),
            vendor: VendorType::Xilinx,
            ..Default::default()
        }
    }

    /// Create an Intel IP config
    pub fn intel(ip_name: impl Into<String>) -> Self {
        Self {
            ip_name: ip_name.into(),
            vendor: VendorType::Intel,
            ..Default::default()
        }
    }

    /// Create a generic black-box IP config
    pub fn generic(ip_name: impl Into<String>) -> Self {
        Self {
            ip_name: ip_name.into(),
            vendor: VendorType::Generic,
            black_box: true,
            ..Default::default()
        }
    }
}

// ============================================================================
// Compiled IP Configuration
// ============================================================================

/// Configuration for compiled IP entities (from #[compiled_ip("path.skb")] attribute)
///
/// When present on an entity, indicates that the entity's implementation
/// comes from a pre-compiled .skb file rather than SKALP source code.
///
/// # Example
///
/// ```skalp
/// #[compiled_ip("lib/dsp_block.skb")]
/// entity DspBlock {
///     in  clk: clock,
///     in  data_in: bit<32>,
///     out data_out: bit<48>
/// }
/// ```
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct CompiledIpConfig {
    /// Path to the .skb file (relative to the .skh file location)
    pub skb_path: String,
    /// Whether the .skb file is encrypted
    pub encrypted: bool,
    /// Key ID for decryption (if encrypted, for key management)
    pub key_id: Option<String>,
}

impl CompiledIpConfig {
    /// Create a new compiled IP config
    pub fn new(skb_path: impl Into<String>) -> Self {
        Self {
            skb_path: skb_path.into(),
            encrypted: false,
            key_id: None,
        }
    }

    /// Create an encrypted compiled IP config
    pub fn encrypted(skb_path: impl Into<String>, key_id: Option<String>) -> Self {
        Self {
            skb_path: skb_path.into(),
            encrypted: true,
            key_id,
        }
    }
}

// ============================================================================
// Power Intent Configuration
// ============================================================================

/// Power domain identifier (mirrors ClockDomainId pattern)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct PowerDomainId(pub u32);

/// Power domain type - specifies power management characteristics
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Serialize, Deserialize)]
pub enum PowerDomainType {
    /// Always-on domain - cannot be power-gated
    #[default]
    AlwaysOn,
    /// Switchable domain - can be fully powered off
    Switchable,
    /// Retention domain - retains state during power-down
    Retention,
}

/// Power domain in HIR (mirrors HirClockDomain pattern)
///
/// Power domains use Rust-style lifetimes like clock domains:
/// ```text
/// power_domains {
///     'always_on: AlwaysOn,
///     'core: Switchable { supply: VDD_CORE },
///     'mem: Retention { supply: VDD_MEM },
/// }
/// ```
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HirPowerDomain {
    /// Power domain identifier
    pub id: PowerDomainId,
    /// Power domain name (e.g., "core" from lifetime 'core)
    pub name: String,
    /// Domain type (AlwaysOn, Switchable, Retention)
    pub domain_type: PowerDomainType,
    /// Supply network name (e.g., VDD_CORE)
    pub supply: Option<String>,
    /// Voltage in millivolts (e.g., 900 for 0.9V)
    pub voltage_mv: Option<u32>,
}

/// Signal power configuration (from #[retention], #[isolation], #[pdc] attributes)
///
/// # Usage Examples
/// ```text
/// // Retention - state preserved during power-down
/// #[retention]
/// signal saved_state<'mem>: bit[32]
///
/// // Isolation - explicit clamp value for domain crossing
/// #[isolation(clamp = low, enable = iso_en)]
/// signal isolated_data<'io>: bit[32]
///
/// // Power domain crossing (mirrors CDC syntax)
/// #[pdc(from = 'core, to = 'io, isolation = clamp_low)]
/// signal cross_domain: bit[32]
///
/// // Level shifter for voltage domain crossing
/// #[level_shift(from = 'core, to = 'io)]
/// signal shifted_signal: bit[16]
/// ```
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct PowerConfig {
    /// Power domain this signal belongs to (resolved from lifetime)
    pub domain: Option<PowerDomainId>,
    /// Power domain name (from #[power('domain)] or #[pdc(from = 'domain)])
    /// Used for safety analysis before ID resolution
    pub domain_name: Option<String>,
    /// Retention configuration
    pub retention: Option<RetentionConfig>,
    /// Isolation configuration
    pub isolation: Option<IsolationConfig>,
    /// Level shifter configuration
    pub level_shift: Option<LevelShiftConfig>,
}

/// Retention configuration for signals in retention domains
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct RetentionConfig {
    /// Retention strategy
    pub strategy: RetentionStrategy,
    /// Save signal name (optional, auto-generated if not specified)
    pub save_signal: Option<String>,
    /// Restore signal name (optional, auto-generated if not specified)
    pub restore_signal: Option<String>,
}

/// Retention strategy for preserving state during power-down
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Serialize, Deserialize)]
pub enum RetentionStrategy {
    /// Compiler chooses optimal strategy
    #[default]
    Auto,
    /// Balloon latch technique
    BalloonLatch,
    /// Shadow register technique
    ShadowRegister,
}

/// Isolation configuration for cross-domain signals
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct IsolationConfig {
    /// Isolation clamp value
    pub clamp: IsolationClamp,
    /// Isolation enable signal name (optional)
    pub enable_signal: Option<String>,
    /// Isolation enable polarity (true = active high)
    pub active_high: bool,
}

/// Isolation clamp type for powered-down domain outputs
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Serialize, Deserialize)]
pub enum IsolationClamp {
    /// Clamp output to 0
    #[default]
    Low,
    /// Clamp output to 1
    High,
    /// Hold last value (latch)
    Latch,
}

/// Level shifter configuration for voltage domain crossings
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct LevelShiftConfig {
    /// Source domain name (from lifetime like 'core)
    pub from_domain: Option<String>,
    /// Destination domain name (from lifetime like 'io)
    pub to_domain: Option<String>,
    /// Level shifter type
    pub shifter_type: LevelShifterType,
}

/// Level shifter type for voltage domain crossings
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Serialize, Deserialize)]
pub enum LevelShifterType {
    /// Compiler infers direction based on voltage specs
    #[default]
    Auto,
    /// Low voltage to high voltage shifter
    LowToHigh,
    /// High voltage to low voltage shifter
    HighToLow,
}

impl PowerConfig {
    /// Create an empty power config
    pub fn new() -> Self {
        Self::default()
    }

    /// Check if this config has any power intent specified
    pub fn has_power_intent(&self) -> bool {
        self.domain.is_some()
            || self.retention.is_some()
            || self.isolation.is_some()
            || self.level_shift.is_some()
    }
}

impl RetentionConfig {
    /// Create a basic retention config with auto strategy
    pub fn new() -> Self {
        Self::default()
    }

    /// Create a retention config with specified strategy
    pub fn with_strategy(strategy: RetentionStrategy) -> Self {
        Self {
            strategy,
            ..Default::default()
        }
    }
}

impl IsolationConfig {
    /// Create an isolation config with low clamp
    pub fn clamp_low() -> Self {
        Self {
            clamp: IsolationClamp::Low,
            active_high: true,
            ..Default::default()
        }
    }

    /// Create an isolation config with high clamp
    pub fn clamp_high() -> Self {
        Self {
            clamp: IsolationClamp::High,
            active_high: true,
            ..Default::default()
        }
    }

    /// Create an isolation config with latch behavior
    pub fn latch() -> Self {
        Self {
            clamp: IsolationClamp::Latch,
            active_high: true,
            ..Default::default()
        }
    }
}

// ============================================================================
// Safety Configuration (ISO 26262 Support)
// ============================================================================

/// Safety configuration for signals and instances (from #[implements(...)] attribute)
/// When present, indicates this element implements a safety mechanism from a safety_goal.
///
/// Example:
/// ```skalp
/// #[implements(BrakingSafety::SensorVoting)]
/// signal voted_pressure: bit[12] = median(pressure_a, pressure_b, pressure_c);
///
/// #[implements(BrakingSafety::DataIntegrity)]
/// let crc = Crc8Checker { ... }
/// ```
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct SafetyConfig {
    /// Safety goal name being implemented (e.g., "BrakingSafety")
    pub goal: Option<String>,
    /// Mechanism name within the goal (e.g., "SensorVoting", "DataIntegrity")
    pub mechanism: Option<String>,
    /// Full implements path (e.g., "BrakingSafety::SensorVoting")
    pub implements_path: Option<String>,
    /// ASIL level override (if specified)
    pub asil_override: Option<String>,
    /// Custom diagnostic coverage override (measured value)
    pub dc_override: Option<f64>,
    /// Custom latent coverage override (measured value)
    pub lc_override: Option<f64>,
}

impl SafetyConfig {
    /// Create an empty safety config
    pub fn new() -> Self {
        Self::default()
    }

    /// Create a safety config with implements path
    pub fn implements(path: &str) -> Self {
        let parts: Vec<&str> = path.split("::").collect();
        let (goal, mechanism) = if parts.len() >= 2 {
            (Some(parts[0].to_string()), Some(parts[1..].join("::")))
        } else {
            (Some(path.to_string()), None)
        };

        Self {
            goal,
            mechanism,
            implements_path: Some(path.to_string()),
            ..Default::default()
        }
    }

    /// Check if this config has any safety annotation
    pub fn has_safety_annotation(&self) -> bool {
        self.implements_path.is_some() || self.goal.is_some()
    }

    /// Get the full implements path
    pub fn get_implements_path(&self) -> Option<&str> {
        self.implements_path.as_deref()
    }
}

/// Generate block mode - controls elaboration behavior
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Serialize, Deserialize)]
pub enum GenerateMode {
    /// Elaborate mode (default): Expand generate blocks at compile time
    #[default]
    Elaborate,
    /// Preserve mode: Keep generate blocks for Verilog output
    Preserve,
}

/// Generate for statement in HIR
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HirGenerateFor {
    /// Iterator variable name
    pub iterator: String,
    /// Iterator variable ID
    pub iterator_var_id: VariableId,
    /// Range to iterate over
    pub range: HirRange,
    /// Body items (signals, instances, event blocks, etc.)
    pub body: HirGenerateBody,
    /// Generate mode (elaborate or preserve)
    pub mode: GenerateMode,
}

/// Generate if statement in HIR
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HirGenerateIf {
    /// Condition expression (must be compile-time constant for elaborate mode)
    pub condition: HirExpression,
    /// Then body items
    pub then_body: HirGenerateBody,
    /// Else body items (optional)
    pub else_body: Option<HirGenerateBody>,
    /// Generate mode (elaborate or preserve)
    pub mode: GenerateMode,
}

/// Generate match statement in HIR
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HirGenerateMatch {
    /// Expression being matched (must be compile-time constant for elaborate mode)
    pub expr: HirExpression,
    /// Match arms
    pub arms: Vec<HirGenerateArm>,
    /// Generate mode (elaborate or preserve)
    pub mode: GenerateMode,
}

/// Generate match arm in HIR
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HirGenerateArm {
    /// Pattern
    pub pattern: HirPattern,
    /// Body items
    pub body: HirGenerateBody,
}

/// Body of a generate block containing declarations and statements
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct HirGenerateBody {
    /// Signals declared in the generate block
    pub signals: Vec<HirSignal>,
    /// Variables declared in the generate block
    pub variables: Vec<HirVariable>,
    /// Constants declared in the generate block
    pub constants: Vec<HirConstant>,
    /// Child instances declared in the generate block
    pub instances: Vec<HirInstance>,
    /// Event blocks (on rising/falling edge)
    pub event_blocks: Vec<HirEventBlock>,
    /// Continuous assignments (using HirAssignment)
    pub assignments: Vec<HirAssignment>,
    /// Nested generate statements
    pub generate_stmts: Vec<HirStatement>,
}

/// Let statement in HIR - local variable binding
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HirLetStatement {
    /// Variable identifier
    pub id: VariableId,
    /// Variable name
    pub name: String,
    /// Whether the binding is mutable (let mut)
    pub mutable: bool,
    /// Variable type
    pub var_type: HirType,
    /// Initializer expression
    pub value: HirExpression,
}

/// Match statement in HIR
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HirMatchStatement {
    /// Expression being matched
    pub expr: HirExpression,
    /// Match arms
    pub arms: Vec<HirMatchArm>,
    /// Mux style hint from intent/attribute (defaults to Priority)
    #[serde(default)]
    pub mux_style: MuxStyle,
}

/// Match arm in HIR
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HirMatchArm {
    /// Pattern
    pub pattern: HirPattern,
    /// Optional guard expression (for `pattern if condition` syntax)
    pub guard: Option<HirExpression>,
    /// Statements
    pub statements: Vec<HirStatement>,
}

/// Flow statement in HIR
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HirFlowStatement {
    /// Pipeline stages
    pub pipeline: HirFlowPipeline,
}

/// Flow pipeline in HIR
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HirFlowPipeline {
    /// Starting stage
    pub start: HirPipelineStage,
    /// Subsequent stages
    pub stages: Vec<HirPipelineStage>,
}

/// Pipeline stage in HIR
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum HirPipelineStage {
    /// Expression stage
    Expression(HirExpression),
    /// Block stage with statements
    Block(Vec<HirStatement>),
}

/// Expressions in HIR
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum HirExpression {
    Literal(HirLiteral),
    Signal(SignalId),
    Port(PortId),
    Variable(VariableId),
    Constant(ConstantId),
    GenericParam(String), // Generic parameter reference (e.g., WIDTH, DEPTH)
    Binary(HirBinaryExpr),
    Unary(HirUnaryExpr),
    Call(HirCallExpr),
    Index(Box<HirExpression>, Box<HirExpression>),
    Range(Box<HirExpression>, Box<HirExpression>, Box<HirExpression>),
    FieldAccess {
        base: Box<HirExpression>,
        field: String,
    },
    EnumVariant {
        enum_type: String,
        variant: String,
    },
    AssociatedConstant {
        type_name: String,
        constant_name: String,
    },
    ArrayRepeat {
        value: Box<HirExpression>,
        count: Box<HirExpression>,
    },
    Concat(Vec<HirExpression>),
    Ternary {
        condition: Box<HirExpression>,
        true_expr: Box<HirExpression>,
        false_expr: Box<HirExpression>,
    },
    StructLiteral(HirStructLiteral),
    /// Tuple literal expression
    /// Example: (result, valid, error_code)
    TupleLiteral(Vec<HirExpression>),
    /// Array literal expression
    /// Example: [1, 2, 3, 4]
    ArrayLiteral(Vec<HirExpression>),
    If(HirIfExpr),
    Match(HirMatchExpr),
    Cast(HirCastExpr),
    /// Block expression with statements and a final expression value
    /// Example: { let x = 10; x + 5 } evaluates to 15
    Block {
        statements: Vec<HirStatement>,
        result_expr: Box<HirExpression>,
    },
}

/// Type cast expression in HIR
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HirCastExpr {
    /// Expression to cast
    pub expr: Box<HirExpression>,
    /// Target type
    pub target_type: HirType,
}

/// Match expression in HIR
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HirMatchExpr {
    /// Expression to match on
    pub expr: Box<HirExpression>,
    /// Match arms
    pub arms: Vec<HirMatchArmExpr>,
    /// Mux style hint from intent/attribute (defaults to Priority)
    #[serde(default)]
    pub mux_style: MuxStyle,
}

/// Match arm for match expressions (contains expressions, not statements)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HirMatchArmExpr {
    /// Pattern
    pub pattern: HirPattern,
    /// Optional guard expression
    pub guard: Option<HirExpression>,
    /// Arm expression (value of this arm)
    pub expr: HirExpression,
}

/// Literals in HIR
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum HirLiteral {
    Integer(u64),
    Boolean(bool),
    Float(f64),
    String(String),
    BitVector(Vec<bool>),
}

/// Binary expression in HIR
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HirBinaryExpr {
    /// Left operand
    pub left: Box<HirExpression>,
    /// Operator
    pub op: HirBinaryOp,
    /// Right operand
    pub right: Box<HirExpression>,
}

/// Binary operators in HIR
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum HirBinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    And,
    Or,
    Xor,
    Equal,
    NotEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    LogicalAnd,
    LogicalOr,
    LeftShift,
    RightShift,
}

/// Unary expression in HIR
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HirUnaryExpr {
    /// Operator
    pub op: HirUnaryOp,
    /// Operand
    pub operand: Box<HirExpression>,
}

/// Unary operators in HIR
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum HirUnaryOp {
    Not,
    Negate,
    BitwiseNot,
    /// Reduction AND (&array) - ANDs all bits together
    AndReduce,
    /// Reduction OR (|array) - ORs all bits together
    OrReduce,
    /// Reduction XOR (^array) - XORs all bits together
    XorReduce,
}

/// Function call in HIR
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HirCallExpr {
    /// Function name
    pub function: String,
    /// Type arguments for generic functions (Phase 1)
    /// Example: func::<T, U>(args) has type_args = [T, U]
    pub type_args: Vec<HirType>,
    /// Named type arguments for generic functions
    /// Example: func::<W: 32>(args) has named_type_args = {"W": Nat(32)}
    pub named_type_args: std::collections::HashMap<String, HirType>,
    /// Arguments
    pub args: Vec<HirExpression>,
    /// Implementation style hint from `#[impl_style::parallel]` attribute
    /// Controls which implementation variant is selected for this call
    pub impl_style: ImplStyle,
}

/// Struct literal in HIR
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HirStructLiteral {
    /// Struct type name
    pub type_name: String,
    /// Field initializations
    pub fields: Vec<HirStructFieldInit>,
}

/// Struct field initialization in HIR
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HirStructFieldInit {
    /// Field name
    pub name: String,
    /// Field value
    pub value: HirExpression,
}

/// If expression in HIR
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HirIfExpr {
    /// Condition
    pub condition: Box<HirExpression>,
    /// Then expression
    pub then_expr: Box<HirExpression>,
    /// Else expression
    pub else_expr: Box<HirExpression>,
}

/// Types in HIR
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum HirType {
    Bit(u32),
    Bool,   // Boolean type (true/false) - distinct from Bit
    String, // String type for testbench messages and debugging (non-synthesizable)
    Logic(u32),
    Int(u32),
    Nat(u32),
    Clock(Option<ClockDomainId>), // Clock with optional domain
    Reset {
        polarity: HirResetPolarity,
        clock_domain: Option<ClockDomainId>,
    }, // Reset with polarity and optional domain
    Event,
    Stream(Box<HirType>), // Stream<T> for streaming data with handshaking
    Array(Box<HirType>, u32),
    Custom(String),
    Struct(HirStructType),
    Enum(Box<HirEnumType>),
    Union(HirUnionType),
    // Parameterized types for generics
    BitParam(String),   // bit[WIDTH] where WIDTH is a parameter
    LogicParam(String), // logic[WIDTH] where WIDTH is a parameter
    IntParam(String),   // int[WIDTH] where WIDTH is a parameter
    NatParam(String),   // nat[WIDTH] where WIDTH is a parameter
    // Types with const expressions (e.g., bit<SIZE + 1>, nat<clog2(DEPTH)>)
    BitExpr(Box<HirExpression>),
    LogicExpr(Box<HirExpression>),
    IntExpr(Box<HirExpression>),
    NatExpr(Box<HirExpression>),
    ArrayExpr(Box<HirType>, Box<HirExpression>), // array<T, SIZE_EXPR>
    // IEEE 754 floating-point types
    Float16, // IEEE 754 half precision (16-bit)
    Float32, // IEEE 754 single precision (32-bit)
    Float64, // IEEE 754 double precision (64-bit)
    // Vector types (SIMD-style packed vectors)
    Vec2(Box<HirType>), // 2-component vector (x, y)
    Vec3(Box<HirType>), // 3-component vector (x, y, z)
    Vec4(Box<HirType>), // 4-component vector (x, y, z, w)

    // Parametric numeric types (from unified type system)
    /// Parametric floating-point type: fp<const F: FloatFormat>
    /// Example: fp<IEEE754_32>, fp<BFLOAT16>
    FpParametric {
        format: Box<HirExpression>, // Const expression evaluating to FloatFormat
    },

    /// Parametric fixed-point type: fixed<const WIDTH: nat, const FRAC: nat, const SIGNED: bool>
    /// Example: fixed<32, 16, true> for Q16.16 fixed-point
    FixedParametric {
        width: Box<HirExpression>,
        frac: Box<HirExpression>,
        signed: Box<HirExpression>,
    },

    /// Parametric integer type: int<const WIDTH: nat, const SIGNED: bool>
    /// Example: int<32, true> for i32, int<64, false> for u64
    IntParametric {
        width: Box<HirExpression>,
        signed: Box<HirExpression>,
    },

    /// Parametric vector type: vec<T, const N: nat>
    /// Example: vec<fp32, 3> for vec3<fp32>
    VecParametric {
        element_type: Box<HirType>,
        dimension: Box<HirExpression>,
    },

    /// Tuple type for multiple return values
    /// Example: (bit[32], bit) for returning (result, valid)
    /// Lowered to anonymous structs during MIR generation
    Tuple(Vec<HirType>),
}

/// Patterns in HIR
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum HirPattern {
    Literal(HirLiteral),
    Variable(String),
    Wildcard,
    Tuple(Vec<HirPattern>),
    Path(String, String), // enum_name, variant_name for enum matching
}

/// Generic parameter in HIR
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HirGeneric {
    /// Parameter name
    pub name: String,
    /// Parameter type
    pub param_type: HirGenericType,
    /// Default value (for type parameters like WIDTH: nat = 8)
    pub default_value: Option<HirExpression>,
}

/// Generic parameter types
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum HirGenericType {
    Type,
    /// Type parameter with trait bounds (e.g., T: FloatingPoint + Numeric)
    /// Phase 2: Enables bounded polymorphism for generic functions
    TypeWithBounds(Vec<String>), // trait bound names
    Const(HirType),
    Width,
    ClockDomain, // Clock domain lifetime parameter (from clock<'clk> types)
    /// Power domain lifetime parameter (from entity<'core, 'aon> declarations)
    /// First parameter is default if only one, otherwise signals must specify
    PowerDomain {
        /// True if this is the default power domain for the entity
        is_default: bool,
    },
    Intent, // Intent parameter for HLS optimization
}

/// Clock domain in HIR
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HirClockDomain {
    /// Clock domain identifier
    pub id: ClockDomainId,
    /// Clock domain name (e.g., 'clk)
    pub name: String,
}

/// Struct type in HIR
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HirStructType {
    /// Struct name
    pub name: String,
    /// Struct fields
    pub fields: Vec<HirStructField>,
    /// Packing mode
    pub packed: bool,
}

/// Struct field in HIR
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HirStructField {
    /// Field name
    pub name: String,
    /// Field type
    pub field_type: HirType,
}

/// Enum type in HIR
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HirEnumType {
    /// Enum name
    pub name: String,
    /// Enum variants
    pub variants: Vec<HirEnumVariant>,
    /// Base type for enum values
    pub base_type: Box<HirType>,
}

/// Enum variant in HIR
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HirEnumVariant {
    /// Variant name
    pub name: String,
    /// Variant value (optional, for explicit discriminants)
    pub value: Option<HirExpression>,
    /// Associated data types for tuple variants (e.g., Transfer(bit[3], bit[8]))
    pub associated_data: Option<Vec<HirType>>,
}

/// Union type in HIR
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HirUnionType {
    /// Union name
    pub name: String,
    /// Union fields
    pub fields: Vec<HirStructField>,
    /// Packing mode
    pub packed: bool,
}

/// Protocol in HIR
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HirProtocol {
    /// Protocol identifier
    pub id: ProtocolId,
    /// Protocol name
    pub name: String,
    /// Protocol signals
    pub signals: Vec<HirProtocolSignal>,
}

/// Protocol signal in HIR
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HirProtocolSignal {
    /// Signal name
    pub name: String,
    /// Signal direction
    pub direction: HirProtocolDirection,
    /// Signal type
    pub signal_type: HirType,
}

/// Protocol signal directions (same as port directions)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum HirProtocolDirection {
    In,
    Out,
}

/// Intent in HIR
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HirIntent {
    /// Intent identifier
    pub id: IntentId,
    /// Intent name
    pub name: String,
    /// Intent description
    pub description: String,
    /// Intent constraints
    pub constraints: Vec<HirIntentConstraint>,
}

/// Intent constraint in HIR
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HirIntentConstraint {
    /// Constraint type
    pub constraint_type: HirConstraintType,
    /// Constraint expression
    pub expr: HirExpression,
}

/// Types of intent constraints
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum HirConstraintType {
    Timing,
    Power,
    Area,
    Performance,
}

/// Mux style for conditionals and match expressions
/// This determines how conditions are synthesized to hardware muxes
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Serialize, Deserialize)]
pub enum MuxStyle {
    /// Priority encoder - cascaded ternary, handles overlapping conditions (default)
    /// Generated as: (c1) ? v1 : ((c2) ? v2 : default)
    #[default]
    Priority,
    /// Parallel one-hot mux - assumes mutually exclusive conditions, shorter critical path
    /// Generated as: ({W{sel==0}} & a) | ({W{sel==1}} & b) | ...
    Parallel,
    /// Compiler analyzes conditions and chooses optimal style
    Auto,
}

/// Pipeline synthesis style - controls register insertion strategy
/// Used with flow { ... |> ... } syntax for pipeline stage control
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Serialize, Deserialize)]
pub enum PipelineStyle {
    /// Compiler decides based on timing analysis (default)
    #[default]
    Auto,
    /// Fully combinational - no pipeline registers inserted
    Combinational,
    /// Explicit manual stages - user controls register placement via |> operator
    Manual,
    /// Auto-retiming - compiler inserts registers for target frequency
    Retimed,
}

/// Implementation style - controls which implementation variant is selected for
/// functions that have multiple valid implementations with different area/performance tradeoffs.
/// Applied to function calls to select implementation at the call site.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Serialize, Deserialize)]
pub enum ImplStyle {
    /// Compiler decides based on context and timing constraints (default)
    #[default]
    Auto,
    /// Fully unrolled single-cycle implementation (more area, lower latency)
    /// E.g., popcount32 expands to 32-input OR tree
    Parallel,
    /// Parallel prefix tree implementation (balanced area/latency)
    /// E.g., popcount32 uses adder tree reduction
    Tree,
    /// Multi-cycle iterative implementation (minimal area, higher latency)
    /// E.g., popcount32 generates FSM with counter
    Sequential,
}

/// Requirement in HIR
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HirRequirement {
    /// Requirement identifier
    pub id: RequirementId,
    /// Requirement name
    pub name: String,
    /// Requirement description
    pub description: String,
    /// Verification method
    pub verification: HirVerificationMethod,
}

/// Verification methods for requirements
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum HirVerificationMethod {
    Simulation,
    FormalVerification,
    Testing,
    Review,
}

// ID types for strong typing
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct EntityId(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct PortId(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct SignalId(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct VariableId(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct ConstantId(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct FunctionId(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct BlockId(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct AssignmentId(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct ProtocolId(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct IntentId(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct RequirementId(pub u32);

/// Instance identifier
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct InstanceId(pub u32);

/// Clock domain identifier
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct ClockDomainId(pub u32);

/// Assertion identifier
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct AssertionId(pub u32);

/// Property identifier
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct PropertyId(pub u32);

/// Cover identifier
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct CoverId(pub u32);

/// For loop identifier
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct ForLoopId(pub u32);

/// HIR builder for converting AST to HIR
pub struct HirBuilder {
    /// Next entity ID
    next_entity_id: u32,
    /// Next port ID
    next_port_id: u32,
    /// Next signal ID
    next_signal_id: u32,
    /// Next variable ID
    next_variable_id: u32,
    /// Next constant ID
    next_constant_id: u32,
    /// Next function ID
    next_function_id: u32,
    /// Next block ID
    next_block_id: u32,
    /// Next assignment ID
    next_assignment_id: u32,
    /// Next protocol ID
    next_protocol_id: u32,
    /// Next intent ID
    next_intent_id: u32,
    /// Next requirement ID
    next_requirement_id: u32,
    /// Next instance ID
    next_instance_id: u32,
    /// Next clock domain ID
    next_clock_domain_id: u32,
    /// Next assertion ID
    next_assertion_id: u32,
    /// Next property ID
    next_property_id: u32,
    /// Next cover ID
    next_cover_id: u32,
    /// Next module ID
    next_module_id: u32,
    /// Next import ID
    next_import_id: u32,
    /// Next for loop ID
    next_for_loop_id: u32,
}

impl HirBuilder {
    /// Create a new HIR builder
    pub fn new() -> Self {
        Self {
            next_entity_id: 0,
            next_port_id: 0,
            next_signal_id: 0,
            next_variable_id: 0,
            next_constant_id: 0,
            next_function_id: 0,
            next_block_id: 0,
            next_assignment_id: 0,
            next_protocol_id: 0,
            next_intent_id: 0,
            next_requirement_id: 0,
            next_instance_id: 0,
            next_clock_domain_id: 0,
            next_assertion_id: 0,
            next_property_id: 0,
            next_cover_id: 0,
            next_module_id: 0,
            next_import_id: 0,
            next_for_loop_id: 0,
        }
    }

    /// Generate a new for loop ID
    pub fn next_for_loop_id(&mut self) -> ForLoopId {
        let id = ForLoopId(self.next_for_loop_id);
        self.next_for_loop_id += 1;
        id
    }

    /// Build HIR from AST
    pub fn build(&mut self, _source_file: &SourceFile) -> Hir {
        // Stub implementation - will be expanded in Week 4
        Hir {
            name: "main".to_string(),
            entities: Vec::new(),
            implementations: Vec::new(),
            protocols: Vec::new(),
            intents: Vec::new(),
            requirements: Vec::new(),
            trait_definitions: Vec::new(),
            trait_implementations: Vec::new(),
            type_aliases: Vec::new(),
            user_defined_types: Vec::new(),
            global_constraints: Vec::new(),
            modules: Vec::new(),
            imports: Vec::new(),
            functions: Vec::new(),
            safety_definitions: ModuleSafetyDefinitions::default(),
        }
    }

    /// Generate next entity ID
    fn next_entity_id(&mut self) -> EntityId {
        let id = EntityId(self.next_entity_id);
        self.next_entity_id += 1;
        id
    }

    /// Generate next port ID
    fn next_port_id(&mut self) -> PortId {
        let id = PortId(self.next_port_id);
        self.next_port_id += 1;
        id
    }

    /// Generate next signal ID
    fn next_signal_id(&mut self) -> SignalId {
        let id = SignalId(self.next_signal_id);
        self.next_signal_id += 1;
        id
    }

    /// Generate next function ID
    fn next_function_id(&mut self) -> FunctionId {
        let id = FunctionId(self.next_function_id);
        self.next_function_id += 1;
        id
    }

    /// Generate next instance ID
    fn next_instance_id(&mut self) -> InstanceId {
        let id = InstanceId(self.next_instance_id);
        self.next_instance_id += 1;
        id
    }

    /// Generate next clock domain ID
    fn next_clock_domain_id(&mut self) -> ClockDomainId {
        let id = ClockDomainId(self.next_clock_domain_id);
        self.next_clock_domain_id += 1;
        id
    }

    /// Generate next assertion ID
    fn next_assertion_id(&mut self) -> AssertionId {
        let id = AssertionId(self.next_assertion_id);
        self.next_assertion_id += 1;
        id
    }

    /// Generate next property ID
    fn next_property_id(&mut self) -> PropertyId {
        let id = PropertyId(self.next_property_id);
        self.next_property_id += 1;
        id
    }

    /// Generate next cover ID
    fn next_cover_id(&mut self) -> CoverId {
        let id = CoverId(self.next_cover_id);
        self.next_cover_id += 1;
        id
    }
}

impl Default for HirBuilder {
    fn default() -> Self {
        Self::new()
    }
}

impl Hir {
    /// Create a new empty HIR
    pub fn new(name: String) -> Self {
        Self {
            name,
            entities: Vec::new(),
            implementations: Vec::new(),
            protocols: Vec::new(),
            intents: Vec::new(),
            requirements: Vec::new(),
            trait_definitions: Vec::new(),
            trait_implementations: Vec::new(),
            type_aliases: Vec::new(),
            user_defined_types: Vec::new(),
            global_constraints: Vec::new(),
            modules: Vec::new(),
            imports: Vec::new(),
            functions: Vec::new(),
            safety_definitions: ModuleSafetyDefinitions::default(),
        }
    }
}

/// Trait definition in HIR
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HirTraitDefinition {
    /// Trait name
    pub name: String,
    /// Generic parameters
    pub generics: Vec<HirGeneric>,
    /// Trait methods
    pub methods: Vec<HirTraitMethod>,
    /// Associated types
    pub associated_types: Vec<HirTraitAssociatedType>,
    /// Associated constants
    pub associated_constants: Vec<HirTraitAssociatedConst>,
}

/// Target for trait implementation (entity or type)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TraitImplTarget {
    /// Implementation for an entity
    Entity(EntityId),
    /// Implementation for a type (e.g., impl Trait for nat[32])
    Type(HirType),
}

/// Trait implementation in HIR
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HirTraitImplementation {
    /// Trait name being implemented
    pub trait_name: String,
    /// Target (entity or type)
    pub target: TraitImplTarget,
    /// Method implementations
    pub method_implementations: Vec<HirTraitMethodImpl>,
    /// Type implementations
    pub type_implementations: Vec<HirTraitTypeImpl>,
    /// Constant implementations
    pub const_implementations: Vec<HirTraitConstImpl>,
}

/// Trait method definition
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HirTraitMethod {
    /// Method name
    pub name: String,
    /// Method parameters
    pub parameters: Vec<HirParameter>,
    /// Return type
    pub return_type: Option<HirType>,
    /// Default implementation
    pub default_implementation: Option<Vec<HirStatement>>,
}

/// Method parameter
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HirParameter {
    /// Parameter name
    pub name: String,
    /// Parameter type
    pub param_type: HirType,
    /// Default value
    pub default_value: Option<HirExpression>,
}

/// Trait associated type
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HirTraitAssociatedType {
    /// Type name
    pub name: String,
    /// Type bounds
    pub bounds: Vec<String>,
    /// Default type
    pub default_type: Option<HirType>,
}

/// Trait associated constant
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HirTraitAssociatedConst {
    /// Constant name
    pub name: String,
    /// Constant type
    pub const_type: HirType,
    /// Default value
    pub default_value: Option<HirExpression>,
}

/// Trait method implementation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HirTraitMethodImpl {
    /// Method name
    pub name: String,
    /// Implementation body
    pub body: Vec<HirStatement>,
}

/// Trait type implementation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HirTraitTypeImpl {
    /// Type name
    pub name: String,
    /// Type implementation
    pub implementation: HirType,
}

/// Trait constant implementation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HirTraitConstImpl {
    /// Constant name
    pub name: String,
    /// Constant value
    pub value: HirExpression,
}

/// Type alias in HIR
///
/// Represents a named alias for an existing type.
///
/// Example:
/// ```skalp
/// pub type Vec3 = vec3<fp32>;
/// pub type Matrix4x4 = [[fp32; 4]; 4];
/// ```
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HirTypeAlias {
    /// Type alias name
    pub name: String,
    /// Visibility
    pub visibility: HirVisibility,
    /// Generic parameters
    pub generics: Vec<HirGeneric>,
    /// Target type (what this alias resolves to)
    pub target_type: HirType,
}

/// User-defined type definition in HIR
///
/// Represents a struct, enum, or union type definition that can be imported.
///
/// Example:
/// ```skalp
/// pub struct Vec3 {
///     pub x: fp32,
///     pub y: fp32,
///     pub z: fp32
/// }
/// ```
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HirUserDefinedType {
    /// Type name
    pub name: String,
    /// Visibility
    pub visibility: HirVisibility,
    /// The actual type definition
    pub type_def: HirType,
}

/// Immediate assertion statement in HIR
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HirAssertStatement {
    /// Assertion identifier
    pub id: AssertionId,
    /// Condition to assert
    pub condition: HirExpression,
    /// Optional message for assertion failure
    pub message: Option<String>,
    /// Assertion severity level
    pub severity: HirAssertionSeverity,
}

/// Simple assume statement in HIR (verification assumptions)
/// Used for assume!(condition) macro-style assumptions
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HirAssumeStatement {
    /// Assumption identifier
    pub id: AssertionId,
    /// Condition to assume
    pub condition: HirExpression,
    /// Optional message for assumption
    pub message: Option<String>,
}

/// Property statement in HIR (concurrent assertions)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HirPropertyStatement {
    /// Property identifier
    pub id: PropertyId,
    /// Property name
    pub name: String,
    /// Property definition
    pub property: HirProperty,
    /// Clock expression for property evaluation
    pub clock: Option<HirExpression>,
    /// Optional disable condition
    pub disable: Option<HirExpression>,
}

/// Cover statement in HIR
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HirCoverStatement {
    /// Cover identifier
    pub id: CoverId,
    /// Property to cover
    pub property: HirProperty,
    /// Optional cover name
    pub name: Option<String>,
}

/// Property definition in HIR
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum HirProperty {
    /// Simple expression property
    Expression(HirExpression),
    /// Sequence property
    Sequence(HirSequence),
    /// Implication property (antecedent |-> consequent)
    Implication {
        antecedent: Box<HirProperty>,
        consequent: Box<HirProperty>,
    },
    /// Overlapping implication (antecedent |=> consequent)
    OverlappingImplication {
        antecedent: Box<HirProperty>,
        consequent: Box<HirProperty>,
    },
    /// Logical AND of properties
    And(Box<HirProperty>, Box<HirProperty>),
    /// Logical OR of properties
    Or(Box<HirProperty>, Box<HirProperty>),
    /// Logical NOT of property
    Not(Box<HirProperty>),
    /// Always property (always p)
    Always(Box<HirProperty>),
    /// Eventually property (eventually p)
    Eventually(Box<HirProperty>),
    /// Until property (p until q)
    Until {
        left: Box<HirProperty>,
        right: Box<HirProperty>,
        strong: bool, // true for strong until, false for weak
    },
    /// Throughout property (p throughout q)
    Throughout {
        left: Box<HirProperty>,
        right: Box<HirProperty>,
    },
    /// Clocked property with edge
    Clocked {
        clock_edge: HirClockEdge,
        property: Box<HirProperty>,
    },
}

/// Sequence definition in HIR
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HirSequence {
    /// Sequence elements
    pub elements: Vec<HirSequenceElement>,
}

/// Sequence element in HIR
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum HirSequenceElement {
    /// Boolean expression
    Expression(HirExpression),
    /// Delay (##n)
    Delay(u32),
    /// Variable delay (##[m:n])
    DelayRange(u32, u32),
    /// Repetition ([*n])
    Repetition(Box<HirSequenceElement>, u32),
    /// Variable repetition ([*m:n])
    RepetitionRange(Box<HirSequenceElement>, u32, u32),
    /// Consecutive repetition ([+n])
    ConsecutiveRepetition(Box<HirSequenceElement>, u32),
    /// Variable consecutive repetition ([+m:n])
    ConsecutiveRepetitionRange(Box<HirSequenceElement>, u32, u32),
    /// Goto repetition ([=n])
    GotoRepetition(Box<HirSequenceElement>, u32),
    /// Variable goto repetition ([=m:n])
    GotoRepetitionRange(Box<HirSequenceElement>, u32, u32),
    /// Sequence concatenation
    Concatenation(Vec<HirSequenceElement>),
    /// Sequence intersection
    Intersection(Box<HirSequenceElement>, Box<HirSequenceElement>),
    /// Sequence union
    Union(Box<HirSequenceElement>, Box<HirSequenceElement>),
}

/// Assertion severity levels
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum HirAssertionSeverity {
    Info,
    Warning,
    Error,
    Fatal,
}

/// Clock edge specification for assertions
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum HirClockEdge {
    /// Positive edge (posedge)
    Posedge(HirExpression),
    /// Negative edge (negedge)
    Negedge(HirExpression),
    /// Any edge
    Edge(HirExpression),
}

/// Coverage group definition in HIR
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HirCovergroup {
    /// Covergroup identifier
    pub id: CovergroupId,
    /// Covergroup name
    pub name: String,
    /// Sampling event (optional)
    pub sampling_event: Option<HirClockEdge>,
    /// Coverage points
    pub coverpoints: Vec<HirCoverpoint>,
    /// Coverage crosses
    pub crosses: Vec<HirCross>,
}

/// Coverage point definition in HIR
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HirCoverpoint {
    /// Coverpoint identifier
    pub id: CoverpointId,
    /// Coverpoint name
    pub name: String,
    /// Expression to cover
    pub expression: HirExpression,
    /// Bins definition
    pub bins: Vec<HirBins>,
}

/// Bins definition in HIR
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HirBins {
    /// Bins identifier
    pub id: BinsId,
    /// Bins name
    pub name: String,
    /// Bins type
    pub bins_type: HirBinsType,
    /// Value ranges or expressions
    pub values: Vec<HirBinsValue>,
}

/// Types of bins in HIR
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum HirBinsType {
    /// Regular bins for coverage
    Regular,
    /// Ignore bins (not counted in coverage)
    Ignore,
    /// Illegal bins (cause error if hit)
    Illegal,
}

/// Bins value specification in HIR
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum HirBinsValue {
    /// Single value
    Value(HirExpression),
    /// Range of values [min, max]
    Range(HirExpression, HirExpression),
    /// Set of discrete values
    Set(Vec<HirExpression>),
    /// Default bin (covers all other values)
    Default,
}

/// Coverage cross definition in HIR
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HirCross {
    /// Cross identifier
    pub id: CrossId,
    /// Cross name
    pub name: String,
    /// List of coverpoints to cross
    pub coverpoints: Vec<CoverpointId>,
    /// Optional bins for cross coverage
    pub bins: Vec<HirBins>,
}

/// Unique identifiers for coverage constructs
pub type CovergroupId = u32;
pub type CoverpointId = u32;
pub type BinsId = u32;
pub type CrossId = u32;

/// Formal verification property definition in HIR
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HirFormalProperty {
    /// Property identifier
    pub id: FormalPropertyId,
    /// Property name
    pub name: String,
    /// Property type
    pub property_type: HirFormalPropertyType,
    /// Property expression
    pub property: HirProperty,
    /// Verification bounds (for bounded model checking)
    pub bounds: Option<HirVerificationBounds>,
    /// Clock domain for this property
    pub clock_domain: Option<HirClockEdge>,
}

/// Types of formal verification properties
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum HirFormalPropertyType {
    /// Safety property (something bad never happens)
    Safety,
    /// Liveness property (something good eventually happens)
    Liveness,
    /// Invariant property (always true)
    Invariant,
    /// Bounded property (true within bounds)
    Bounded,
}

/// Verification bounds for bounded model checking
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HirVerificationBounds {
    /// Minimum depth for verification
    pub min_depth: Option<u32>,
    /// Maximum depth for verification
    pub max_depth: u32,
    /// Step size for incremental verification
    pub step_size: Option<u32>,
}

/// Formal verification block in HIR
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HirFormalBlock {
    /// Block identifier
    pub id: FormalBlockId,
    /// Block name
    pub name: Option<String>,
    /// Formal properties in this block
    pub properties: Vec<HirFormalProperty>,
    /// Assumptions for this verification context
    pub assumptions: Vec<HirProperty>,
    /// Verification engine configuration
    pub config: HirVerificationConfig,
}

/// Verification engine configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HirVerificationConfig {
    /// Target verification engine
    pub engine: HirVerificationEngine,
    /// Timeout for verification (in seconds)
    pub timeout: Option<u32>,
    /// Memory limit for verification (in MB)
    pub memory_limit: Option<u32>,
    /// Additional engine-specific options
    pub options: Vec<(String, String)>,
}

/// Supported verification engines
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum HirVerificationEngine {
    /// Built-in bounded model checker
    BuiltinBMC,
    /// Induction-based prover
    Induction,
    /// SAT-based model checker
    SATSolver,
    /// SMT-based verification
    SMTSolver,
    /// External tool integration
    External(String),
}

/// Verification result from formal verification
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HirVerificationResult {
    /// Property that was verified
    pub property_id: FormalPropertyId,
    /// Verification status
    pub status: HirVerificationStatus,
    /// Verification statistics
    pub statistics: HirVerificationStatistics,
    /// Counterexample (if property failed)
    pub counterexample: Option<HirCounterexample>,
}

/// Verification status
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum HirVerificationStatus {
    /// Property is proven true
    Proven,
    /// Property is disproven (counterexample found)
    Disproven,
    /// Verification timed out
    Timeout,
    /// Verification failed due to error
    Error(String),
    /// Verification is inconclusive
    Unknown,
}

/// Verification statistics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HirVerificationStatistics {
    /// Time taken for verification (in seconds)
    pub verification_time: f64,
    /// Memory used for verification (in MB)
    pub memory_used: u32,
    /// Number of solver calls
    pub solver_calls: u32,
    /// Maximum depth reached
    pub max_depth_reached: u32,
}

/// Counterexample for failed properties
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HirCounterexample {
    /// Length of the counterexample trace
    pub length: u32,
    /// Signal values at each step
    pub trace: Vec<HirTraceStep>,
}

/// Single step in a counterexample trace
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HirTraceStep {
    /// Step number
    pub step: u32,
    /// Signal assignments at this step
    pub assignments: Vec<(String, HirExpression)>,
}

/// Unique identifiers for formal verification constructs
pub type FormalPropertyId = u32;
pub type FormalBlockId = u32;

// ============================================================================
// Physical Constraints
// ============================================================================

/// Physical constraints for FPGA pin mapping and I/O configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PhysicalConstraints {
    /// Pin location assignment
    pub pin_location: Option<PinLocation>,
    /// I/O electrical standard (e.g., "LVCMOS33", "LVDS_25")
    pub io_standard: Option<String>,
    /// Drive strength
    pub drive_strength: Option<DriveStrength>,
    /// Slew rate
    pub slew_rate: Option<SlewRate>,
    /// Termination/pull resistor
    pub termination: Option<Termination>,
    /// Schmitt trigger enable
    pub schmitt_trigger: Option<bool>,
    /// I/O bank assignment
    pub bank: Option<u32>,
    /// Differential termination (for LVDS, etc.)
    pub diff_term: Option<bool>,
    /// I/O pad type for ASIC designs
    pub pad_type: Option<PadType>,
    /// Specific pad cell to use (overrides automatic selection)
    pub pad_cell: Option<String>,
    /// LDO configuration (only valid when pad_type is PowerLdo)
    pub ldo_config: Option<LdoConfig>,
}

/// Pin location specification
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum PinLocation {
    /// Single pin (e.g., "A1")
    Single(String),
    /// Multiple pins for a bus (e.g., ["A1", "A2", "A3"])
    Multiple(Vec<String>),
    /// Differential pair
    Differential {
        /// Positive pin
        positive: String,
        /// Negative pin
        negative: String,
    },
}

/// Drive strength specification
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum DriveStrength {
    /// 4mA drive strength
    Ma4,
    /// 8mA drive strength
    Ma8,
    /// 12mA drive strength
    Ma12,
    /// 16mA drive strength
    Ma16,
}

/// Slew rate specification
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum SlewRate {
    /// Fast slew rate
    Fast,
    /// Slow slew rate
    Slow,
    /// Medium slew rate
    Medium,
}

/// Termination/pull resistor specification
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum Termination {
    /// No termination
    None,
    /// Pull-up resistor
    PullUp,
    /// Pull-down resistor
    PullDown,
    /// Keeper (weak latch)
    Keeper,
}

/// I/O Pad type specification for ASIC designs
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum PadType {
    /// Input pad (external → core)
    Input,
    /// Output pad (core → external)
    Output,
    /// Bidirectional pad with output enable
    Bidirectional,
    /// Clock input pad with low-jitter characteristics
    Clock,
    /// Power pad (VDD) with ESD protection
    Power,
    /// Ground pad (VSS) with ESD protection
    Ground,
    /// Analog pass-through pad
    Analog,
    /// Power pad with integrated LDO voltage regulator
    PowerLdo,
}

/// LDO (Low Dropout Regulator) configuration for integrated voltage regulation
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct LdoConfig {
    /// Input voltage in millivolts
    pub input_voltage_mv: u32,
    /// Output (regulated) voltage in millivolts
    pub output_voltage_mv: u32,
    /// Maximum current capability in milliamps
    pub max_current_ma: u32,
}

/// Global constraint for device-wide settings
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum GlobalConstraint {
    /// Device selection constraint
    Device(DeviceConstraint),
    /// Bank voltage and I/O standard constraint
    Bank(BankConstraint),
    /// Floorplan constraint
    Floorplan(FloorplanConstraint),
    /// Global I/O defaults
    IoDefaults(IoDefaults),
}

/// Device selection constraint
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DeviceConstraint {
    /// Device name (e.g., "iCE40HX8K-CT256")
    pub device_name: String,
}

/// Bank voltage and I/O standard constraint
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BankConstraint {
    /// Bank identifier
    pub bank_id: u32,
    /// Bank voltage (e.g., "3.3V", "1.8V")
    pub voltage: Option<String>,
    /// Default I/O standard for the bank
    pub io_standard: Option<String>,
}

/// Floorplan constraint
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FloorplanConstraint {
    /// Placement regions
    pub regions: Vec<RegionConstraint>,
}

/// Placement region constraint
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RegionConstraint {
    /// Region name
    pub name: String,
    /// Physical area (x1, y1, x2, y2)
    pub area: (u32, u32, u32, u32),
    /// Instances to place in this region
    pub instances: Vec<String>,
    /// Whether this is a boundary region
    pub boundary: bool,
    /// Keep instances together
    pub keep_together: bool,
}

/// Global I/O defaults
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct IoDefaults {
    /// Default I/O standard
    pub io_standard: Option<String>,
    /// Default drive strength
    pub drive_strength: Option<DriveStrength>,
    /// Default slew rate
    pub slew_rate: Option<SlewRate>,
    /// Default termination
    pub termination: Option<Termination>,
}

/// Module declaration in HIR
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HirModule {
    /// Module identifier
    pub id: ModuleId,
    /// Module name
    pub name: String,
    /// Visibility
    pub visibility: HirVisibility,
    /// Items in the module (if inline)
    pub items: Option<Vec<HirModuleItem>>,
}

/// Items that can appear in a module
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum HirModuleItem {
    Entity(Box<HirEntity>),
    Module(HirModule),
    Import(HirImport),
    TraitDef(HirTraitDefinition),
}

/// Use statement (import) in HIR
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HirImport {
    /// Import identifier
    pub id: ImportId,
    /// Visibility (pub use re-exports)
    pub visibility: HirVisibility,
    /// Path being imported
    pub path: HirImportPath,
}

/// Path in an import statement
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum HirImportPath {
    /// Simple path: use foo::bar::Baz;
    Simple { segments: Vec<String> },
    /// Renamed import: use foo::bar::Baz as Qux;
    Renamed {
        segments: Vec<String>,
        alias: String,
    },
    /// Glob import: use foo::bar::*;
    Glob { segments: Vec<String> },
    /// Nested imports: use foo::bar::{Baz, Qux};
    Nested {
        prefix: Vec<String>,
        paths: Vec<HirImportPath>,
    },
}

/// Visibility in HIR
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
pub enum HirVisibility {
    /// Public - visible everywhere
    Public,
    /// Crate-local - visible within the crate
    Crate,
    /// Super-local - visible to parent module
    Super,
    /// Private - only visible in the same module (default)
    Private,
}

/// Module identifier
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct ModuleId(pub u32);

/// Import identifier
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct ImportId(pub u32);

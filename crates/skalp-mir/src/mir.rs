//! MIR - Mid-level Intermediate Representation
//!
//! This represents hardware designs at a level suitable for:
//! - SystemVerilog code generation
//! - Optimization passes
//! - Simulation preparation
//!
//! MIR is lower-level than HIR but still hardware-agnostic

use crate::Type;
use serde::{Deserialize, Serialize};
use skalp_frontend::hir::DetectionConfig;
use skalp_frontend::safety_attributes::ModuleSafetyDefinitions;
use skalp_frontend::SourceSpan;
use std::collections::HashMap;

// ============================================================================
// Safety Context Types (ISO 26262 Support)
// ============================================================================

/// Safety context for signals, modules, and instances
/// Tracks which safety goal(s) a hardware element implements
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct SafetyContext {
    /// Safety goal this element implements (if any)
    pub implementing_goal: Option<String>,
    /// If true, this element is part of a safety mechanism
    pub is_sm_signal: bool,
    /// Name of the safety mechanism this element belongs to
    pub mechanism_name: Option<String>,
    /// Diagnostic coverage override (measured value)
    pub dc_override: Option<f64>,
    /// Latent coverage override (measured value)
    pub lc_override: Option<f64>,
}

impl SafetyContext {
    /// Create a new empty safety context
    pub fn new() -> Self {
        Self::default()
    }

    /// Create a context for a goal implementation
    pub fn for_goal(goal_id: impl Into<String>) -> Self {
        Self {
            implementing_goal: Some(goal_id.into()),
            ..Default::default()
        }
    }

    /// Create a context for a safety mechanism
    pub fn for_mechanism(mechanism_name: impl Into<String>, goal_id: Option<String>) -> Self {
        Self {
            implementing_goal: goal_id,
            is_sm_signal: true,
            mechanism_name: Some(mechanism_name.into()),
            ..Default::default()
        }
    }

    /// Check if this context has any safety annotation
    pub fn has_safety_annotation(&self) -> bool {
        self.implementing_goal.is_some() || self.is_sm_signal || self.mechanism_name.is_some()
    }
}

/// Mid-level Intermediate Representation for a design
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Mir {
    /// Design name
    pub name: String,
    /// Top-level modules
    pub modules: Vec<Module>,
    /// Module-level safety definitions (safety goals, mechanisms, HSI)
    /// Propagated from HIR for downstream analysis
    #[serde(default, skip_serializing_if = "ModuleSafetyDefinitions::is_empty")]
    pub safety_definitions: ModuleSafetyDefinitions,
}

/// A hardware module (corresponds to entity in SKALP)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Module {
    /// Module identifier
    pub id: ModuleId,
    /// Module name
    pub name: String,
    /// Generic parameters
    pub parameters: Vec<GenericParameter>,
    /// Input/output ports
    pub ports: Vec<Port>,
    /// Internal signals
    pub signals: Vec<Signal>,
    /// Variables (for procedural blocks)
    pub variables: Vec<Variable>,
    /// Process blocks (always blocks in Verilog)
    pub processes: Vec<Process>,
    /// Continuous assignments
    pub assignments: Vec<ContinuousAssign>,
    /// Module instances (hierarchy)
    pub instances: Vec<ModuleInstance>,
    /// Clock domains in this module
    pub clock_domains: Vec<ClockDomain>,
    /// Generate blocks (for #[preserve_generate] mode)
    pub generate_blocks: Vec<GenerateBlock>,
    /// Formal verification assertions (assert!, assume!, cover!)
    pub assertions: Vec<Assertion>,
    /// Source location for error reporting
    #[serde(skip_serializing_if = "Option::is_none")]
    pub span: Option<SourceSpan>,
    /// Pipeline configuration from `#[pipeline(stages=N)]` attribute
    #[serde(skip_serializing_if = "Option::is_none")]
    pub pipeline_config: Option<skalp_frontend::hir::PipelineConfig>,
    /// Vendor IP configuration from `#[xilinx_ip]`, `#[intel_ip]`, etc. attributes
    #[serde(skip_serializing_if = "Option::is_none")]
    pub vendor_ip_config: Option<skalp_frontend::hir::VendorIpConfig>,
    /// Compiled IP configuration from `#[compiled_ip("path.skb")]` attribute
    /// When set, this module is a blackbox that will be loaded from a pre-compiled binary
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub compiled_ip_config: Option<skalp_frontend::hir::CompiledIpConfig>,
    /// Power domain declarations (mirrors clock_domains pattern)
    pub power_domains: Vec<skalp_frontend::hir::HirPowerDomain>,
    /// Power domain configuration for CCF analysis (from #[power_domain("name")] attribute)
    /// When set, all primitives in this module belong to this power domain
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub power_domain_config: Option<skalp_frontend::hir::PowerDomainConfig>,
    /// Safety context (from #[implements(...)] or #[safety_mechanism(...)] attribute)
    /// Indicates which safety goal/mechanism this module implements
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub safety_context: Option<SafetyContext>,
    /// Whether this is an async (NCL) module
    /// NCL modules have no clocks and use dual-rail encoding
    #[serde(default)]
    pub is_async: bool,
    /// NCL barrier stages for completion detection
    /// Each barrier defines a point where all signals must transition through NULL
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub barriers: Vec<BarrierStage>,
}

/// NCL barrier stage for completion detection
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BarrierStage {
    /// Unique identifier for this stage
    pub stage_id: u32,
    /// Signals that contribute to this stage's completion
    pub signals: Vec<SignalId>,
}

/// Module identifier
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct ModuleId(pub u32);

/// Port of a module
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Port {
    /// Port identifier
    pub id: PortId,
    /// Port name
    pub name: String,
    /// Port direction
    pub direction: PortDirection,
    /// Port type
    pub port_type: DataType,
    /// Physical constraints (from HIR)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub physical_constraints: Option<skalp_frontend::hir::PhysicalConstraints>,
    /// Source location for error reporting
    #[serde(skip_serializing_if = "Option::is_none")]
    pub span: Option<SourceSpan>,
    /// Detection signal configuration (for safety analysis)
    /// Set via #[detection_signal] or #[detection_signal(mode = "boot")] attribute
    #[serde(skip_serializing_if = "Option::is_none")]
    pub detection_config: Option<DetectionConfig>,
}

impl Port {
    /// Returns true if this port is a detection signal
    pub fn is_detection_signal(&self) -> bool {
        self.detection_config.is_some()
    }
}

/// Port identifier
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct PortId(pub u32);

/// Port direction
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum PortDirection {
    Input,
    Output,
    InOut,
}

/// Internal signal in a module
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Signal {
    /// Signal identifier
    pub id: SignalId,
    /// Signal name
    pub name: String,
    /// Signal type
    pub signal_type: DataType,
    /// Initial value (if any)
    pub initial: Option<Value>,
    /// Clock domain this signal belongs to (for CDC analysis)
    pub clock_domain: Option<ClockDomainId>,
    /// Source location for error reporting
    #[serde(skip_serializing_if = "Option::is_none")]
    pub span: Option<SourceSpan>,
    /// Memory configuration (from #[memory(depth=N)] attribute)
    /// When present, signal represents a memory array that should be
    /// synthesized as BRAM/SRAM rather than discrete registers.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub memory_config: Option<skalp_frontend::hir::MemoryConfig>,
    /// Trace configuration (from #[trace] attribute)
    /// When present, signal should be auto-exported to simulation traces/waveforms.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub trace_config: Option<skalp_frontend::hir::TraceConfig>,
    /// CDC configuration (from #[cdc] attribute)
    /// When present, signal crosses clock domains and needs synchronization.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub cdc_config: Option<skalp_frontend::hir::CdcConfig>,
    /// Breakpoint configuration (from #[breakpoint] attribute)
    /// When present, generates SVA assertions for debugging.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub breakpoint_config: Option<skalp_frontend::hir::BreakpointConfig>,
    /// Power intent configuration (from #[retention], #[isolation], #[pdc], #[level_shift] attributes)
    /// When present, specifies power domain crossing, retention, or isolation requirements.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub power_config: Option<skalp_frontend::hir::PowerConfig>,
    /// Safety context (from #[implements(...)] attribute on signal)
    /// Indicates which safety goal/mechanism this signal implements
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub safety_context: Option<SafetyContext>,
    /// Detection signal configuration (for safety analysis)
    /// Propagated from sub-module output ports marked with #[detection_signal]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub detection_config: Option<DetectionConfig>,
    /// Power domain name for CCF (Common Cause Failure) analysis
    /// Set via #[power_domain("name")] attribute
    /// Used to identify cells sharing a common power supply
    #[serde(skip_serializing_if = "Option::is_none")]
    pub power_domain: Option<String>,
}

impl Signal {
    /// Returns true if this signal is a detection signal
    pub fn is_detection_signal(&self) -> bool {
        self.detection_config.is_some()
    }
}

/// Signal identifier
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct SignalId(pub u32);

/// Variable (for procedural context)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Variable {
    /// Variable identifier
    pub id: VariableId,
    /// Variable name
    pub name: String,
    /// Variable type
    pub var_type: DataType,
    /// Initial value
    pub initial: Option<Value>,
    /// Source location for error reporting
    #[serde(skip_serializing_if = "Option::is_none")]
    pub span: Option<SourceSpan>,
}

/// Variable identifier
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct VariableId(pub u32);

/// Data types in MIR
///
/// # MIR Type Invariants (Post-HIR→MIR Transformation)
///
/// After HIR→MIR transformation is complete, **ports and signals must be synthesis-friendly**.
/// Most composite types (Struct, Vec2/Vec3/Vec4, Enum, Union) are **flattened** into
/// individual scalar signals/ports during the HIR→MIR transformation.
///
/// **Exception:** Arrays of scalar types (e.g., `[bit[32]; 16]`) are **preserved** as arrays
/// to allow the synthesis tool to choose the optimal implementation (MUX tree, distributed RAM,
/// block RAM, etc.). This separation of concerns improves synthesis flexibility.
///
/// ## Flattening Rules
///
/// - **Struct types**: Always flattened into individual fields with names like `base_field1_field2`
/// - **Vec2/Vec3/Vec4**: Always flattened into components `base_x`, `base_y`, `base_z`, `base_w`
/// - **Arrays of scalars**: **PRESERVED** as array types (e.g., `reg [31:0] mem [0:15]` in SystemVerilog)
/// - **Arrays of composites**: Flattened into indexed elements `base_0_field`, `base_1_field`, etc.
/// - **Enums**: Converted to their base type representation
/// - **Unions**: Flattened to their largest field width
///
/// ## Array Preservation Strategy
///
/// Arrays of scalar types are intentionally preserved to:
/// 1. Let synthesis tools choose optimal implementation (MUX vs. memory)
/// 2. Support functional simulation with array indexing
/// 3. Enable gate-level simulation of actual synthesized structures
/// 4. Avoid forcing a specific hardware realization
///
/// Example:
/// ```systemverilog
/// // Preserved array (synthesis tool decides implementation):
/// reg [31:0] memory [0:15];
/// assign rd_data = memory[rd_ptr];  // Can become MUX, LUTRAM, BRAM, etc.
/// ```
///
/// ## Validation
///
/// The `skalp_mir::mir_validation::validate_mir()` function enforces this invariant.
/// It is automatically called after HIR→MIR transformation in `lower_to_mir()`.
/// If disallowed composite types appear in ports/signals after transformation, validation
/// will panic with a detailed error message indicating a bug in `hir_to_mir.rs`.
///
/// ## Type Width Calculation
///
/// Use `skalp_mir::type_width::get_type_width()` for consistent width calculation.
/// This function will panic if struct/enum/union types are passed to it, ensuring bugs are
/// caught early.
///
/// ## Example
///
/// ```ignore
/// // HIR (before transformation):
/// port vertex: Vertex;  // Vertex is a struct { position: Vec3<f32>, color: bit[32] }
/// port memory: [bit[32]; 16];  // Array of scalars
///
/// // MIR (after transformation):
/// port vertex_position_x: Float32;  // Struct flattened
/// port vertex_position_y: Float32;
/// port vertex_position_z: Float32;
/// port vertex_color: Bit(32);
/// port memory: Array(Bit(32), 16);  // Array preserved!
/// ```
///
/// ## See Also
///
/// - `skalp_mir::type_flattening` - Struct/vector/array flattening utilities
/// - `skalp_mir::type_width` - Type width calculation utilities
/// - `skalp_mir::signal_naming` - Naming convention utilities
/// - `skalp_mir::mir_validation` - MIR invariant validation
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum DataType {
    /// Bit vector (synthesis-friendly)
    Bit(usize),
    /// Boolean type - true/false (distinct from single-bit hardware signals)
    Bool,
    /// Logic vector (4-state for simulation)
    Logic(usize),
    /// Signed integer
    Int(usize),
    /// Unsigned natural
    Nat(usize),
    /// Clock signal with optional domain
    Clock { domain: Option<ClockDomainId> },
    /// Reset signal with optional domain
    Reset {
        active_high: bool,
        domain: Option<ClockDomainId>,
    },
    /// Event type
    Event,
    /// Struct type
    Struct(Box<StructType>),
    /// Enum type
    Enum(Box<EnumType>),
    /// Union type
    Union(Box<UnionType>),
    /// Array type
    Array(Box<DataType>, usize),
    /// Bit vector with parametric width (simple parameter name)
    BitParam { param: String, default: usize },
    /// Logic vector with parametric width (simple parameter name)
    LogicParam { param: String, default: usize },
    /// Signed integer with parametric width (simple parameter name)
    IntParam { param: String, default: usize },
    /// Unsigned natural with parametric width (simple parameter name)
    NatParam { param: String, default: usize },
    /// Bit vector with expression-based width (e.g., clog2(SIZE))
    BitExpr {
        expr: Box<Expression>,
        default: usize,
    },
    /// Logic vector with expression-based width
    LogicExpr {
        expr: Box<Expression>,
        default: usize,
    },
    /// Signed integer with expression-based width
    IntExpr {
        expr: Box<Expression>,
        default: usize,
    },
    /// Unsigned natural with expression-based width
    NatExpr {
        expr: Box<Expression>,
        default: usize,
    },
    /// IEEE 754 half precision (16-bit)
    Float16,
    /// IEEE 754 single precision (32-bit)
    Float32,
    /// IEEE 754 double precision (64-bit)
    Float64,
    /// 2-component vector type (packed struct with x, y fields)
    Vec2(Box<DataType>),
    /// 3-component vector type (packed struct with x, y, z fields)
    Vec3(Box<DataType>),
    /// 4-component vector type (packed struct with x, y, z, w fields)
    Vec4(Box<DataType>),
    /// NCL (Null Convention Logic) dual-rail type
    /// Physical width is 2 * logical width (dual-rail encoding)
    Ncl(usize),
}

/// Clock domain identifier in MIR
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct ClockDomainId(pub u32);

/// Clock domain information in MIR
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ClockDomain {
    /// Domain identifier
    pub id: ClockDomainId,
    /// Domain name
    pub name: String,
    /// Clock signal for this domain
    pub clock_signal: Option<SignalId>,
    /// Reset signal for this domain
    pub reset_signal: Option<SignalId>,
}

/// Process block (maps to always block)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Process {
    /// Process identifier
    pub id: ProcessId,
    /// Process kind
    pub kind: ProcessKind,
    /// Sensitivity list
    pub sensitivity: SensitivityList,
    /// Process body
    pub body: Block,
    /// Source location for error reporting
    #[serde(skip_serializing_if = "Option::is_none")]
    pub span: Option<SourceSpan>,
}

/// Process identifier
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct ProcessId(pub u32);

/// Kind of process
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum ProcessKind {
    /// Sequential logic (always_ff)
    Sequential,
    /// Combinational logic (always_comb)
    Combinational,
    /// General process (always)
    General,
    /// NCL async process (self-timed, no clock)
    Async,
}

/// Sensitivity list for a process
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum SensitivityList {
    /// Edge-triggered (posedge/negedge)
    Edge(Vec<EdgeSensitivity>),
    /// Level-sensitive (combinational)
    Level(Vec<LValue>),
    /// Always (continuous)
    Always,
}

/// Edge sensitivity
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EdgeSensitivity {
    /// Signal to monitor
    pub signal: LValue,
    /// Edge type
    pub edge: EdgeType,
}

/// Edge type
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum EdgeType {
    Rising,
    Falling,
    Both,
    Active,   // For reset active level
    Inactive, // For reset inactive level
}

/// Block of statements
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Block {
    /// Statements in the block
    pub statements: Vec<Statement>,
}

/// Statement in a process
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Statement {
    /// Assignment statement
    Assignment(Assignment),
    /// Conditional statement
    If(IfStatement),
    /// Case/match statement
    Case(CaseStatement),
    /// Block of statements
    Block(Block),
    /// Loop statement
    Loop(LoopStatement),
    /// Synthesis-resolved conditional assignment (replaces complex if-else-if chains)
    ResolvedConditional(ResolvedConditional),
    /// Assertion statement (generates SVA assert)
    Assert(AssertStatement),
    /// Assume statement (generates SVA assume)
    Assume(AssumeStatement),
    /// Cover statement (generates SVA cover)
    Cover(CoverStatement),
}

/// Assignment in a process
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Assignment {
    /// Left-hand side
    pub lhs: LValue,
    /// Right-hand side expression
    pub rhs: Expression,
    /// Assignment kind
    pub kind: AssignmentKind,
    /// Source location for error reporting
    #[serde(skip_serializing_if = "Option::is_none")]
    pub span: Option<SourceSpan>,
}

/// Assignment kind
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum AssignmentKind {
    /// Non-blocking assignment (<=)
    NonBlocking,
    /// Blocking assignment (:= or =)
    Blocking,
}

/// Left-hand value (assignable)
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum LValue {
    /// Port reference
    Port(PortId),
    /// Signal reference
    Signal(SignalId),
    /// Variable reference
    Variable(VariableId),
    /// Bit selection
    BitSelect {
        base: Box<LValue>,
        index: Box<Expression>,
    },
    /// Range selection
    RangeSelect {
        base: Box<LValue>,
        high: Box<Expression>,
        low: Box<Expression>,
    },
    /// Concatenation
    Concat(Vec<LValue>),
}

/// Expression (right-hand side) with type information
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Expression {
    /// The kind/variant of this expression
    pub kind: ExpressionKind,
    /// The type of this expression
    #[serde(skip, default = "default_unknown_type")]
    pub ty: Type,
    /// Source location for error reporting
    #[serde(skip_serializing_if = "Option::is_none")]
    pub span: Option<SourceSpan>,
}

/// Default function for serde deserialization of Type field
fn default_unknown_type() -> Type {
    Type::Unknown
}

/// Expression kinds (variants)
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum ExpressionKind {
    /// Literal value
    Literal(Value),
    /// LValue reference
    Ref(LValue),
    /// Binary operation
    Binary {
        op: BinaryOp,
        left: Box<Expression>,
        right: Box<Expression>,
    },
    /// Unary operation
    Unary {
        op: UnaryOp,
        operand: Box<Expression>,
    },
    /// Conditional expression (ternary)
    Conditional {
        cond: Box<Expression>,
        then_expr: Box<Expression>,
        else_expr: Box<Expression>,
    },
    /// Concatenation
    Concat(Vec<Expression>),
    /// Replication
    Replicate {
        count: Box<Expression>,
        value: Box<Expression>,
    },
    /// Function call
    FunctionCall { name: String, args: Vec<Expression> },
    /// Type cast (bitwise reinterpretation for FP/bit conversions)
    Cast {
        expr: Box<Expression>,
        target_type: DataType,
    },
    /// Tuple field/element access (for destructuring)
    /// BUG FIX #85: Enables `let (a, b, c) = func()` pattern in module synthesis
    TupleFieldAccess { base: Box<Expression>, index: usize },
    /// Named field access on structs
    FieldAccess {
        base: Box<Expression>,
        field: String,
    },
}

impl Expression {
    /// Create a new expression with the given kind and type
    pub fn new(kind: ExpressionKind, ty: Type) -> Self {
        Self {
            kind,
            ty,
            span: None,
        }
    }

    /// Create a new expression with the given kind, type, and span
    pub fn new_with_span(kind: ExpressionKind, ty: Type, span: Option<SourceSpan>) -> Self {
        Self { kind, ty, span }
    }

    /// Create an expression with Unknown type (TEMPORARY: for migration to typed expressions)
    /// TODO(Bug #76): Replace all uses with properly typed constructors
    pub fn with_unknown_type(kind: ExpressionKind) -> Self {
        Self {
            kind,
            ty: Type::Unknown,
            span: None,
        }
    }

    /// Create a literal expression
    pub fn literal(value: Value, ty: Type) -> Self {
        Self::new(ExpressionKind::Literal(value), ty)
    }

    /// Create an LValue reference expression
    pub fn lvalue_ref(lvalue: LValue, ty: Type) -> Self {
        Self::new(ExpressionKind::Ref(lvalue), ty)
    }

    /// Create a binary operation expression
    pub fn binary(op: BinaryOp, left: Expression, right: Expression, ty: Type) -> Self {
        Self::new(
            ExpressionKind::Binary {
                op,
                left: Box::new(left),
                right: Box::new(right),
            },
            ty,
        )
    }

    /// Create a unary operation expression
    pub fn unary(op: UnaryOp, operand: Expression, ty: Type) -> Self {
        Self::new(
            ExpressionKind::Unary {
                op,
                operand: Box::new(operand),
            },
            ty,
        )
    }

    /// Create a conditional (ternary) expression
    pub fn conditional(
        cond: Expression,
        then_expr: Expression,
        else_expr: Expression,
        ty: Type,
    ) -> Self {
        Self::new(
            ExpressionKind::Conditional {
                cond: Box::new(cond),
                then_expr: Box::new(then_expr),
                else_expr: Box::new(else_expr),
            },
            ty,
        )
    }

    /// Create a concatenation expression
    pub fn concat(exprs: Vec<Expression>, ty: Type) -> Self {
        Self::new(ExpressionKind::Concat(exprs), ty)
    }

    /// Create a replication expression
    pub fn replicate(count: Expression, value: Expression, ty: Type) -> Self {
        Self::new(
            ExpressionKind::Replicate {
                count: Box::new(count),
                value: Box::new(value),
            },
            ty,
        )
    }

    /// Create a function call expression
    pub fn function_call(name: String, args: Vec<Expression>, ty: Type) -> Self {
        Self::new(ExpressionKind::FunctionCall { name, args }, ty)
    }

    /// Create a cast expression
    pub fn cast(expr: Expression, target_type: DataType, ty: Type) -> Self {
        Self::new(
            ExpressionKind::Cast {
                expr: Box::new(expr),
                target_type,
            },
            ty,
        )
    }
}

/// Literal value
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Value {
    /// Integer literal
    Integer(i64),
    /// Floating-point literal
    Float(f64),
    /// Bit vector literal
    BitVector { width: usize, value: u64 },
    /// String literal
    String(String),
    /// High impedance
    HighZ,
    /// Unknown/undefined
    Unknown,
}

// Custom Eq implementation for Value
// We use bitwise comparison for floats to maintain Eq semantics
impl Eq for Value {}

impl Value {
    /// Compare floats using bitwise equality (required for Eq)
    pub fn eq_float(a: f64, b: f64) -> bool {
        a.to_bits() == b.to_bits()
    }
}

/// Binary operators
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum BinaryOp {
    // Arithmetic
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    // Logical
    And,
    Or,
    Xor,
    // Comparison
    Equal,
    NotEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    // Bitwise
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    // Shift
    LeftShift,
    RightShift,
    // Logical (boolean)
    LogicalAnd,
    LogicalOr,
}

/// Unary operators
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum UnaryOp {
    Not,
    BitwiseNot,
    Negate,
    Reduce(ReduceOp),
}

/// Reduction operators
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum ReduceOp {
    And,
    Or,
    Xor,
    Nand,
    Nor,
    Xnor,
}

/// If statement
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct IfStatement {
    /// Condition
    pub condition: Expression,
    /// Then branch
    pub then_block: Block,
    /// Else branch (optional)
    pub else_block: Option<Block>,
    /// Source location for error reporting
    #[serde(skip_serializing_if = "Option::is_none")]
    pub span: Option<SourceSpan>,
}

/// Case statement
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CaseStatement {
    /// Expression to match
    pub expr: Expression,
    /// Case items
    pub items: Vec<CaseItem>,
    /// Default case
    pub default: Option<Block>,
    /// Source location for error reporting
    #[serde(skip_serializing_if = "Option::is_none")]
    pub span: Option<SourceSpan>,
}

/// Case item
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CaseItem {
    /// Values to match
    pub values: Vec<Expression>,
    /// Block to execute
    pub block: Block,
}

/// Loop statement
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum LoopStatement {
    /// For loop
    For {
        init: Box<Assignment>,
        condition: Expression,
        update: Box<Assignment>,
        body: Block,
    },
    /// While loop
    While { condition: Expression, body: Block },
}

/// Continuous assignment (outside processes)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ContinuousAssign {
    /// Left-hand side
    pub lhs: LValue,
    /// Right-hand side
    pub rhs: Expression,
    /// Source location for error reporting
    #[serde(skip_serializing_if = "Option::is_none")]
    pub span: Option<SourceSpan>,
}

/// Generic parameter definition in MIR
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GenericParameter {
    /// Parameter name
    pub name: String,
    /// Parameter type
    pub param_type: GenericParameterType,
    /// Default value (if any)
    pub default: Option<Value>,
}

/// Types of generic parameters
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum GenericParameterType {
    /// Type parameter
    Type,
    /// Constant parameter with specified type
    Const(DataType),
    /// Width parameter (integer)
    Width,
    /// Clock domain parameter
    ClockDomain,
    /// Power domain parameter (from entity<'core, 'aon> declarations)
    PowerDomain,
    /// Intent parameter for HLS optimization
    Intent,
}

/// Struct type definition in MIR
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct StructType {
    /// Struct name
    pub name: String,
    /// Struct fields
    pub fields: Vec<StructField>,
    /// Whether this is a packed struct (affects layout)
    pub packed: bool,
}

/// Struct field in MIR
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct StructField {
    /// Field name
    pub name: String,
    /// Field type
    pub field_type: DataType,
}

/// Enum type definition in MIR
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct EnumType {
    /// Enum name
    pub name: String,
    /// Enum variants
    pub variants: Vec<EnumVariant>,
    /// Base type for enum values
    pub base_type: DataType,
}

/// Enum variant in MIR
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct EnumVariant {
    /// Variant name
    pub name: String,
    /// Variant value (optional, auto-assigned if None)
    pub value: Option<Value>,
}

/// Union type definition in MIR
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct UnionType {
    /// Union name
    pub name: String,
    /// Union fields (all share the same memory)
    pub fields: Vec<StructField>,
    /// Whether this is a packed union
    pub packed: bool,
}

/// Module instance (hierarchy)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ModuleInstance {
    /// Instance name
    pub name: String,
    /// Module to instantiate
    pub module: ModuleId,
    /// Port connections
    pub connections: HashMap<String, Expression>,
    /// Generic/parameter overrides
    pub parameters: HashMap<String, Value>,
    /// Source location for error reporting
    #[serde(skip_serializing_if = "Option::is_none")]
    pub span: Option<SourceSpan>,
    /// Safety context (from #[implements(...)] attribute on instance)
    /// Indicates which safety goal/mechanism this instance implements
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub safety_context: Option<SafetyContext>,
}

/// Synthesis-resolved conditional assignment
/// Contains both original form (for analysis tools) and resolved form (for SIR/LIR generation)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ResolvedConditional {
    /// Target signal being assigned
    pub target: LValue,
    /// Assignment kind (blocking/non-blocking)
    pub kind: AssignmentKind,
    /// Original if-else-if chain (preserved for analysis tools)
    pub original: Box<IfStatement>,
    /// Synthesis-resolved priority mux tree
    pub resolved: PriorityMux,
}

/// Priority-encoded multiplexer tree (synthesis-resolved form of if-else-if)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PriorityMux {
    /// Ordered list of condition-value pairs (highest priority first)
    pub cases: Vec<ConditionalCase>,
    /// Default value (when no conditions match)
    pub default: Expression,
}

/// Parallel one-hot multiplexer (for mutually exclusive conditions)
/// Generates: (sel==0 & a) | (sel==1 & b) | (sel==2 & c) | ...
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ParallelMux {
    /// Selector expression (typically the match scrutinee)
    pub selector: Expression,
    /// List of (match_value, result) pairs - must be mutually exclusive
    pub cases: Vec<ParallelCase>,
    /// Default value (when no cases match) - optional for full coverage
    pub default: Option<Expression>,
    /// Bit width of result
    pub result_width: usize,
}

/// A single case in a parallel mux
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ParallelCase {
    /// Value to compare selector against
    pub match_value: Expression,
    /// Result value when selector matches
    pub value: Expression,
}

/// Unified mux type that can be either priority or parallel
#[derive(Debug, Clone, Serialize, Deserialize)]
#[allow(clippy::large_enum_variant)]
pub enum MuxTree {
    /// Priority encoder - cascaded ternary (default, safe for overlapping conditions)
    Priority(PriorityMux),
    /// Parallel one-hot - OR of AND terms (faster, requires mutually exclusive conditions)
    Parallel(ParallelMux),
}

/// Formal verification assertion (assert!, assume!, cover!)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Assertion {
    /// Type of assertion
    pub kind: AssertionKind,
    /// Condition expression to check
    pub condition: Expression,
    /// Optional message for assertion failure
    pub message: Option<String>,
    /// Source location for error reporting
    #[serde(skip_serializing_if = "Option::is_none")]
    pub span: Option<SourceSpan>,
}

/// Kind of formal verification assertion
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum AssertionKind {
    /// Assert - property must hold (checked by simulation/formal)
    Assert,
    /// Assume - constraint on inputs (for formal verification)
    Assume,
    /// Cover - functional coverage point
    Cover,
}

/// A single condition-value pair in a priority mux
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ConditionalCase {
    /// Condition expression
    pub condition: Expression,
    /// Value to select when condition is true
    pub value: Expression,
}

// ============================================================================
// Formal Verification Types (SVA Generation)
// ============================================================================

/// Assertion statement for formal verification
/// Generates SVA: `assert property (@(posedge clk) condition) else $error("message");`
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AssertStatement {
    /// Condition to assert
    pub condition: Expression,
    /// Optional message for assertion failure
    pub message: Option<String>,
    /// Severity level (maps to $info, $warning, $error, $fatal)
    pub severity: AssertionSeverity,
    /// Source location for error reporting
    #[serde(skip_serializing_if = "Option::is_none")]
    pub span: Option<SourceSpan>,
}

/// Assume statement for formal verification
/// Generates SVA: `assume property (@(posedge clk) condition);`
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AssumeStatement {
    /// Condition to assume
    pub condition: Expression,
    /// Optional message
    pub message: Option<String>,
    /// Source location for error reporting
    #[serde(skip_serializing_if = "Option::is_none")]
    pub span: Option<SourceSpan>,
}

/// Cover statement for formal verification
/// Generates SVA: `cover property (@(posedge clk) condition);`
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CoverStatement {
    /// Condition to cover
    pub condition: Expression,
    /// Optional label for the cover property
    pub label: Option<String>,
    /// Source location for error reporting
    #[serde(skip_serializing_if = "Option::is_none")]
    pub span: Option<SourceSpan>,
}

/// Assertion severity level
/// Maps to SystemVerilog severity functions
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum AssertionSeverity {
    /// $info() - informational message
    Info,
    /// $warning() - warning message
    Warning,
    /// $error() - error message (default)
    Error,
    /// $fatal() - fatal error, stops simulation
    Fatal,
}

// ============================================================================
// Generate Block Types (for #[preserve_generate] mode)
// ============================================================================

/// Generate block identifier
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct GenerateBlockId(pub u32);

/// HDL generate block (preserved for SystemVerilog output)
///
/// When `#[preserve_generate]` attribute is used, generate constructs are
/// preserved in MIR and emitted as SystemVerilog generate blocks rather
/// than being elaborated at compile time.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GenerateBlock {
    /// Generate block identifier
    pub id: GenerateBlockId,
    /// Optional block label (for named generate blocks)
    pub label: Option<String>,
    /// The kind of generate block
    pub kind: GenerateBlockKind,
    /// Source location for error reporting
    #[serde(skip_serializing_if = "Option::is_none")]
    pub span: Option<SourceSpan>,
}

/// Kind of generate block
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum GenerateBlockKind {
    /// For-generate loop: `for (genvar i = 0; i < N; i++)`
    For(GenerateFor),
    /// If-generate conditional: `if (CONDITION)`
    If(GenerateIf),
    /// Case-generate (from match): `case (EXPR)`
    Case(GenerateCase),
}

/// For-generate loop construct
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GenerateFor {
    /// Genvar name
    pub genvar: String,
    /// Start value expression
    pub start: Expression,
    /// End value expression (exclusive)
    pub end: Expression,
    /// Step value (defaults to 1)
    pub step: i64,
    /// Body to replicate
    pub body: GenerateBody,
}

/// If-generate conditional construct
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GenerateIf {
    /// Condition expression (must be constant at elaboration time)
    pub condition: Expression,
    /// Then branch
    pub then_body: GenerateBody,
    /// Optional else branch
    pub else_body: Option<GenerateBody>,
}

/// Case-generate construct (from match expressions)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GenerateCase {
    /// Selector expression
    pub selector: Expression,
    /// Case arms
    pub arms: Vec<GenerateCaseArm>,
    /// Default arm (optional)
    pub default: Option<GenerateBody>,
}

/// A single arm in a case-generate
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GenerateCaseArm {
    /// Pattern value(s) to match
    pub patterns: Vec<Expression>,
    /// Body for this arm
    pub body: GenerateBody,
}

/// Body of a generate block - can contain various module items
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GenerateBody {
    /// Signals declared in this generate scope
    pub signals: Vec<Signal>,
    /// Process blocks
    pub processes: Vec<Process>,
    /// Continuous assignments
    pub assignments: Vec<ContinuousAssign>,
    /// Module instances
    pub instances: Vec<ModuleInstance>,
    /// Nested generate blocks
    pub nested_generates: Vec<GenerateBlock>,
}

impl GenerateBody {
    /// Create an empty generate body
    pub fn new() -> Self {
        Self {
            signals: Vec::new(),
            processes: Vec::new(),
            assignments: Vec::new(),
            instances: Vec::new(),
            nested_generates: Vec::new(),
        }
    }
}

impl Default for GenerateBody {
    fn default() -> Self {
        Self::new()
    }
}

impl Mir {
    /// Create a new MIR
    pub fn new(name: String) -> Self {
        Self {
            name,
            modules: Vec::new(),
            safety_definitions: ModuleSafetyDefinitions::default(),
        }
    }

    /// Add a module to the MIR
    pub fn add_module(&mut self, module: Module) {
        self.modules.push(module);
    }
}

impl Module {
    /// Create a new module
    pub fn new(id: ModuleId, name: String) -> Self {
        Self {
            id,
            name,
            parameters: Vec::new(),
            ports: Vec::new(),
            signals: Vec::new(),
            variables: Vec::new(),
            processes: Vec::new(),
            assignments: Vec::new(),
            instances: Vec::new(),
            clock_domains: Vec::new(),
            generate_blocks: Vec::new(),
            assertions: Vec::new(),
            span: None,
            pipeline_config: None,
            vendor_ip_config: None,
            compiled_ip_config: None,
            power_domains: Vec::new(),
            power_domain_config: None,
            safety_context: None,
            is_async: false,
            barriers: Vec::new(),
        }
    }
}

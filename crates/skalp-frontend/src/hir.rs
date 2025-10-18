//! HIR - High-level Intermediate Representation
//!
//! Converts the AST into a simplified IR suitable for further processing

use crate::ast::SourceFile;
use serde::{Deserialize, Serialize};

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
    /// Generic arguments (for monomorphization)
    pub generic_args: Vec<HirExpression>,
    /// Port connections
    pub connections: Vec<HirConnection>,
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
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HirFunction {
    /// Function identifier
    pub id: FunctionId,
    /// Whether this is a const function
    pub is_const: bool,
    /// Function name
    pub name: String,
    /// Function parameters
    pub params: Vec<HirParameter>,
    /// Return type (None for void functions)
    pub return_type: Option<HirType>,
    /// Function body
    pub body: Vec<HirStatement>,
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
    Property(HirPropertyStatement),
    Cover(HirCoverStatement),
    Let(HirLetStatement),
    Return(Option<HirExpression>),
    Expression(HirExpression),
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
}

/// Let statement in HIR - local variable binding
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HirLetStatement {
    /// Variable identifier
    pub id: VariableId,
    /// Variable name
    pub name: String,
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
    /// Arguments
    pub args: Vec<HirExpression>,
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
    Bool, // Boolean type (true/false) - distinct from Bit
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
    Const(HirType),
    Width,
    ClockDomain, // Clock domain lifetime parameter
    Intent,      // Intent parameter for HLS optimization
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
        }
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

/// Trait implementation in HIR
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HirTraitImplementation {
    /// Trait name being implemented
    pub trait_name: String,
    /// Target entity
    pub target_entity: EntityId,
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
    Entity(HirEntity),
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

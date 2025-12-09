//! Const Expression Evaluator
//!
//! Evaluates compile-time constant expressions for monomorphization

use crate::hir::{
    ConstantId, HirBinaryOp, HirExpression, HirFunction, HirLiteral, HirStatement, HirUnaryOp,
};
use std::collections::HashMap;

/// Const expression evaluation result
#[derive(Debug, Clone)]
pub enum ConstValue {
    /// Natural number (usize)
    Nat(usize),
    /// Integer
    Int(i64),
    /// Boolean
    Bool(bool),
    /// String
    String(String),
    /// Floating-point
    Float(f64),
    /// Float format descriptor
    FloatFormat(FloatFormatValue),
    /// Struct value (field name -> value)
    Struct(HashMap<String, ConstValue>),
}

/// Float format constant value
#[derive(Debug, Clone)]
pub struct FloatFormatValue {
    pub total_bits: usize,
    pub exponent_bits: usize,
    pub mantissa_bits: usize,
    pub bias: i64,
    pub name: String,
}

// Manual PartialEq for ConstValue (to handle Float)
impl PartialEq for ConstValue {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (ConstValue::Nat(a), ConstValue::Nat(b)) => a == b,
            (ConstValue::Int(a), ConstValue::Int(b)) => a == b,
            (ConstValue::Bool(a), ConstValue::Bool(b)) => a == b,
            (ConstValue::String(a), ConstValue::String(b)) => a == b,
            (ConstValue::Float(a), ConstValue::Float(b)) => a.to_bits() == b.to_bits(),
            (ConstValue::FloatFormat(a), ConstValue::FloatFormat(b)) => a == b,
            (ConstValue::Struct(a), ConstValue::Struct(b)) => {
                // Compare struct fields (simplified)
                a.len() == b.len()
            }
            _ => false,
        }
    }
}

impl Eq for ConstValue {}

impl PartialEq for FloatFormatValue {
    fn eq(&self, other: &Self) -> bool {
        self.total_bits == other.total_bits
            && self.exponent_bits == other.exponent_bits
            && self.mantissa_bits == other.mantissa_bits
            && self.bias == other.bias
    }
}

impl Eq for FloatFormatValue {}

impl ConstValue {
    /// Convert to usize (for array sizes, etc.)
    pub fn as_nat(&self) -> Option<usize> {
        match self {
            ConstValue::Nat(n) => Some(*n),
            ConstValue::Int(i) if *i >= 0 => Some(*i as usize),
            _ => None,
        }
    }

    /// Convert to i64
    pub fn as_int(&self) -> Option<i64> {
        match self {
            ConstValue::Int(i) => Some(*i),
            ConstValue::Nat(n) => Some(*n as i64),
            _ => None,
        }
    }

    /// Convert to bool
    pub fn as_bool(&self) -> Option<bool> {
        match self {
            ConstValue::Bool(b) => Some(*b),
            _ => None,
        }
    }

    /// Convert to string
    pub fn as_string(&self) -> Option<&str> {
        match self {
            ConstValue::String(s) => Some(s),
            _ => None,
        }
    }
}

/// Built-in const function
type BuiltinConstFn = fn(&[ConstValue]) -> Result<ConstValue, EvalError>;

/// Const expression evaluator
pub struct ConstEvaluator {
    /// Symbol table for const bindings (parameter name -> value)
    const_bindings: HashMap<String, ConstValue>,
    /// Built-in const functions
    builtin_fns: HashMap<String, BuiltinConstFn>,
    /// User-defined const functions
    user_fns: HashMap<String, HirFunction>,
    /// Constant definitions (ID -> value expression)
    constants: HashMap<ConstantId, HirExpression>,
    /// Recursion depth (for preventing infinite recursion)
    recursion_depth: usize,
}

/// Evaluation error
#[derive(Debug, Clone)]
pub enum EvalError {
    /// Expression is not constant
    NotConstant(String),
    /// Undefined symbol
    UndefinedSymbol(String),
    /// Type mismatch
    TypeMismatch(String),
    /// Division by zero
    DivisionByZero,
    /// Function not found
    FunctionNotFound(String),
    /// Invalid argument count
    InvalidArgCount { expected: usize, got: usize },
    /// Field not found
    FieldNotFound(String),
    /// Recursion limit exceeded
    RecursionLimitExceeded,
    /// Non-const function cannot be evaluated at compile time
    NonConstFunction(String),
}

impl ConstEvaluator {
    /// Create a new const evaluator
    pub fn new() -> Self {
        let mut builtin_fns: HashMap<String, BuiltinConstFn> = HashMap::new();

        // Register built-in functions
        // Logarithmic & Exponential
        builtin_fns.insert("clog2".to_string(), builtin_clog2 as BuiltinConstFn);
        builtin_fns.insert("pow2".to_string(), builtin_pow2 as BuiltinConstFn);
        builtin_fns.insert("pow".to_string(), builtin_pow as BuiltinConstFn);

        // Bit Manipulation
        builtin_fns.insert("popcount".to_string(), builtin_popcount as BuiltinConstFn);
        builtin_fns.insert("clz".to_string(), builtin_clz as BuiltinConstFn);
        builtin_fns.insert("ctz".to_string(), builtin_ctz as BuiltinConstFn);
        builtin_fns.insert(
            "reverse_bits".to_string(),
            builtin_reverse_bits as BuiltinConstFn,
        );
        builtin_fns.insert(
            "is_power_of_2".to_string(),
            builtin_is_power_of_2 as BuiltinConstFn,
        );

        // Arithmetic
        builtin_fns.insert("max".to_string(), builtin_max as BuiltinConstFn);
        builtin_fns.insert("min".to_string(), builtin_min as BuiltinConstFn);
        builtin_fns.insert("abs".to_string(), builtin_abs as BuiltinConstFn);
        builtin_fns.insert("gcd".to_string(), builtin_gcd as BuiltinConstFn);
        builtin_fns.insert("lcm".to_string(), builtin_lcm as BuiltinConstFn);

        // Gray Code
        builtin_fns.insert(
            "gray_encode".to_string(),
            builtin_gray_encode as BuiltinConstFn,
        );
        builtin_fns.insert(
            "gray_decode".to_string(),
            builtin_gray_decode as BuiltinConstFn,
        );

        Self {
            const_bindings: HashMap::new(),
            builtin_fns,
            user_fns: HashMap::new(),
            constants: HashMap::new(),
            recursion_depth: 0,
        }
    }

    /// Bind a const parameter to a value
    pub fn bind(&mut self, name: String, value: ConstValue) {
        self.const_bindings.insert(name, value);
    }

    /// Bind multiple const parameters
    pub fn bind_all(&mut self, bindings: HashMap<String, ConstValue>) {
        self.const_bindings.extend(bindings);
    }

    /// Unbind a const parameter
    pub fn unbind(&mut self, name: &str) {
        self.const_bindings.remove(name);
    }

    /// Register a user-defined const function
    pub fn register_function(&mut self, func: HirFunction) {
        if func.is_const {
            self.user_fns.insert(func.name.clone(), func);
        }
    }

    /// Register multiple user-defined const functions
    pub fn register_functions(&mut self, funcs: &[HirFunction]) {
        for func in funcs {
            self.register_function(func.clone());
        }
    }

    /// Register a constant definition
    pub fn register_constant(&mut self, id: ConstantId, value_expr: HirExpression) {
        self.constants.insert(id, value_expr);
    }

    /// Register multiple constant definitions
    pub fn register_constants(&mut self, consts: &[crate::hir::HirConstant]) {
        for const_decl in consts {
            self.constants
                .insert(const_decl.id, const_decl.value.clone());
        }
    }

    /// Evaluate a const expression
    pub fn eval(&mut self, expr: &HirExpression) -> Result<ConstValue, EvalError> {
        // Check recursion depth
        const MAX_RECURSION_DEPTH: usize = 100;
        if self.recursion_depth >= MAX_RECURSION_DEPTH {
            return Err(EvalError::RecursionLimitExceeded);
        }
        match expr {
            // Literals are trivially constant
            HirExpression::Literal(lit) => self.eval_literal(lit),

            // Generic parameter reference - look up in bindings
            HirExpression::GenericParam(name) => self
                .const_bindings
                .get(name)
                .cloned()
                .ok_or_else(|| EvalError::UndefinedSymbol(name.clone())),

            // Constant reference - look up constant value and recursively evaluate
            HirExpression::Constant(id) => {
                if let Some(value_expr) = self.constants.get(id).cloned() {
                    // Recursively evaluate the constant's value expression
                    self.eval(&value_expr)
                } else {
                    Err(EvalError::UndefinedSymbol(format!(
                        "Constant ID {:?} not found",
                        id
                    )))
                }
            }

            // Binary operations
            HirExpression::Binary(bin) => self.eval_binary(bin),

            // Unary operations
            HirExpression::Unary(un) => self.eval_unary(un),

            // Function calls
            HirExpression::Call(call) => {
                let fn_name = &call.function;

                // Check built-in functions first
                if let Some(&builtin) = self.builtin_fns.get(fn_name) {
                    // Evaluate arguments
                    let args: Result<Vec<_>, _> =
                        call.args.iter().map(|arg| self.eval(arg)).collect();
                    let args = args?;
                    builtin(&args)
                } else if let Some(user_fn) = self.user_fns.get(fn_name).cloned() {
                    // User-defined const function
                    if !user_fn.is_const {
                        return Err(EvalError::NonConstFunction(fn_name.clone()));
                    }

                    // Check argument count
                    if call.args.len() != user_fn.params.len() {
                        return Err(EvalError::InvalidArgCount {
                            expected: user_fn.params.len(),
                            got: call.args.len(),
                        });
                    }

                    // Evaluate user-defined function
                    self.eval_user_function(&user_fn, &call.args)
                } else {
                    Err(EvalError::FunctionNotFound(fn_name.clone()))
                }
            }

            // Field access (e.g., F.total_bits)
            HirExpression::FieldAccess { base, field } => {
                let base_val = self.eval(base)?;
                match base_val {
                    ConstValue::FloatFormat(ref fmt) => match field.as_str() {
                        "total_bits" => Ok(ConstValue::Nat(fmt.total_bits)),
                        "exponent_bits" => Ok(ConstValue::Nat(fmt.exponent_bits)),
                        "mantissa_bits" => Ok(ConstValue::Nat(fmt.mantissa_bits)),
                        "bias" => Ok(ConstValue::Int(fmt.bias)),
                        "name" => Ok(ConstValue::String(fmt.name.clone())),
                        _ => Err(EvalError::FieldNotFound(field.clone())),
                    },
                    ConstValue::Struct(fields) => fields
                        .get(field)
                        .cloned()
                        .ok_or_else(|| EvalError::FieldNotFound(field.clone())),
                    _ => Err(EvalError::TypeMismatch(format!(
                        "Cannot access field '{}' on {:?}",
                        field, base_val
                    ))),
                }
            }

            // If expression (must be const-evaluable)
            HirExpression::If(if_expr) => {
                let cond = self.eval(&if_expr.condition)?;
                match cond {
                    ConstValue::Bool(true) => self.eval(&if_expr.then_expr),
                    ConstValue::Bool(false) => self.eval(&if_expr.else_expr),
                    _ => Err(EvalError::TypeMismatch(
                        "if condition must be boolean".to_string(),
                    )),
                }
            }

            // Other expressions are not constant
            _ => Err(EvalError::NotConstant(format!(
                "Expression {:?} is not constant",
                expr
            ))),
        }
    }

    /// Evaluate a user-defined const function
    fn eval_user_function(
        &mut self,
        func: &HirFunction,
        args: &[HirExpression],
    ) -> Result<ConstValue, EvalError> {
        // Increment recursion depth
        self.recursion_depth += 1;

        // Save current bindings (for restoring later)
        let saved_bindings = self.const_bindings.clone();

        // Bind parameters to argument values
        for (param, arg_expr) in func.params.iter().zip(args.iter()) {
            let arg_val = self.eval(arg_expr)?;
            self.const_bindings.insert(param.name.clone(), arg_val);
        }

        // Evaluate function body
        let result = self.eval_function_body(&func.body);

        // Restore bindings and decrement recursion depth
        self.const_bindings = saved_bindings;
        self.recursion_depth -= 1;

        result
    }

    /// Evaluate function body and extract return value
    fn eval_function_body(&mut self, body: &[HirStatement]) -> Result<ConstValue, EvalError> {
        // Look for a return statement or expression statement
        for stmt in body {
            match stmt {
                HirStatement::Return(Some(expr)) => {
                    return self.eval(expr);
                }
                HirStatement::Return(None) => {
                    return Err(EvalError::TypeMismatch(
                        "Const function must return a value".to_string(),
                    ));
                }
                HirStatement::Expression(expr) => {
                    // Last expression is the return value (Rust-style)
                    return self.eval(expr);
                }
                HirStatement::If(if_stmt) => {
                    // Evaluate if condition
                    let cond = self.eval(&if_stmt.condition)?;
                    match cond {
                        ConstValue::Bool(true) => {
                            return self.eval_function_body(&if_stmt.then_statements);
                        }
                        ConstValue::Bool(false) => {
                            if let Some(ref else_stmts) = if_stmt.else_statements {
                                return self.eval_function_body(else_stmts);
                            }
                        }
                        _ => {
                            return Err(EvalError::TypeMismatch(
                                "If condition must be boolean".to_string(),
                            ));
                        }
                    }
                }
                HirStatement::Let(let_stmt) => {
                    // Evaluate let binding
                    let val = self.eval(&let_stmt.value)?;
                    self.const_bindings.insert(let_stmt.name.clone(), val);
                }
                _ => {
                    // Other statements cannot appear in const functions
                    return Err(EvalError::NotConstant(
                        "Statement not allowed in const function".to_string(),
                    ));
                }
            }
        }

        Err(EvalError::TypeMismatch(
            "Const function must have a return value".to_string(),
        ))
    }

    /// Evaluate a literal
    fn eval_literal(&self, lit: &HirLiteral) -> Result<ConstValue, EvalError> {
        match lit {
            HirLiteral::Integer(n) => Ok(ConstValue::Nat(*n as usize)),
            HirLiteral::Boolean(b) => Ok(ConstValue::Bool(*b)),
            HirLiteral::Float(f) => Ok(ConstValue::Float(*f)),
            HirLiteral::String(s) => Ok(ConstValue::String(s.clone())),
            _ => Err(EvalError::NotConstant("Complex literal".to_string())),
        }
    }

    /// Evaluate binary operation
    fn eval_binary(&mut self, bin: &crate::hir::HirBinaryExpr) -> Result<ConstValue, EvalError> {
        let left = self.eval(&bin.left)?;
        let right = self.eval(&bin.right)?;

        match bin.op {
            HirBinaryOp::Add => match (left, right) {
                (ConstValue::Nat(a), ConstValue::Nat(b)) => Ok(ConstValue::Nat(a + b)),
                (ConstValue::Int(a), ConstValue::Int(b)) => Ok(ConstValue::Int(a + b)),
                (ConstValue::Float(a), ConstValue::Float(b)) => Ok(ConstValue::Float(a + b)),
                _ => Err(EvalError::TypeMismatch(
                    "Cannot add these types".to_string(),
                )),
            },

            HirBinaryOp::Sub => match (left, right) {
                (ConstValue::Nat(a), ConstValue::Nat(b)) if a >= b => Ok(ConstValue::Nat(a - b)),
                (ConstValue::Nat(a), ConstValue::Nat(b)) => {
                    Ok(ConstValue::Int(a as i64 - b as i64))
                }
                (ConstValue::Int(a), ConstValue::Int(b)) => Ok(ConstValue::Int(a - b)),
                (ConstValue::Float(a), ConstValue::Float(b)) => Ok(ConstValue::Float(a - b)),
                _ => Err(EvalError::TypeMismatch(
                    "Cannot subtract these types".to_string(),
                )),
            },

            HirBinaryOp::Mul => match (left, right) {
                (ConstValue::Nat(a), ConstValue::Nat(b)) => Ok(ConstValue::Nat(a * b)),
                (ConstValue::Int(a), ConstValue::Int(b)) => Ok(ConstValue::Int(a * b)),
                (ConstValue::Float(a), ConstValue::Float(b)) => Ok(ConstValue::Float(a * b)),
                _ => Err(EvalError::TypeMismatch(
                    "Cannot multiply these types".to_string(),
                )),
            },

            HirBinaryOp::Div => match (left, right) {
                (ConstValue::Nat(a), ConstValue::Nat(b)) if b != 0 => Ok(ConstValue::Nat(a / b)),
                (ConstValue::Int(a), ConstValue::Int(b)) if b != 0 => Ok(ConstValue::Int(a / b)),
                (ConstValue::Float(a), ConstValue::Float(b)) => Ok(ConstValue::Float(a / b)),
                (_, ConstValue::Nat(0)) | (_, ConstValue::Int(0)) => Err(EvalError::DivisionByZero),
                _ => Err(EvalError::TypeMismatch(
                    "Cannot divide these types".to_string(),
                )),
            },

            HirBinaryOp::Mod => match (left, right) {
                (ConstValue::Nat(a), ConstValue::Nat(b)) if b != 0 => Ok(ConstValue::Nat(a % b)),
                (ConstValue::Int(a), ConstValue::Int(b)) if b != 0 => Ok(ConstValue::Int(a % b)),
                (_, ConstValue::Nat(0)) | (_, ConstValue::Int(0)) => Err(EvalError::DivisionByZero),
                _ => Err(EvalError::TypeMismatch(
                    "Cannot modulo these types".to_string(),
                )),
            },

            HirBinaryOp::Equal => Ok(ConstValue::Bool(left == right)),
            HirBinaryOp::NotEqual => Ok(ConstValue::Bool(left != right)),

            HirBinaryOp::Less => match (left, right) {
                (ConstValue::Nat(a), ConstValue::Nat(b)) => Ok(ConstValue::Bool(a < b)),
                (ConstValue::Int(a), ConstValue::Int(b)) => Ok(ConstValue::Bool(a < b)),
                (ConstValue::Float(a), ConstValue::Float(b)) => Ok(ConstValue::Bool(a < b)),
                _ => Err(EvalError::TypeMismatch(
                    "Cannot compare these types".to_string(),
                )),
            },

            HirBinaryOp::LessEqual => match (left, right) {
                (ConstValue::Nat(a), ConstValue::Nat(b)) => Ok(ConstValue::Bool(a <= b)),
                (ConstValue::Int(a), ConstValue::Int(b)) => Ok(ConstValue::Bool(a <= b)),
                (ConstValue::Float(a), ConstValue::Float(b)) => Ok(ConstValue::Bool(a <= b)),
                _ => Err(EvalError::TypeMismatch(
                    "Cannot compare these types".to_string(),
                )),
            },

            HirBinaryOp::Greater => match (left, right) {
                (ConstValue::Nat(a), ConstValue::Nat(b)) => Ok(ConstValue::Bool(a > b)),
                (ConstValue::Int(a), ConstValue::Int(b)) => Ok(ConstValue::Bool(a > b)),
                (ConstValue::Float(a), ConstValue::Float(b)) => Ok(ConstValue::Bool(a > b)),
                _ => Err(EvalError::TypeMismatch(
                    "Cannot compare these types".to_string(),
                )),
            },

            HirBinaryOp::GreaterEqual => match (left, right) {
                (ConstValue::Nat(a), ConstValue::Nat(b)) => Ok(ConstValue::Bool(a >= b)),
                (ConstValue::Int(a), ConstValue::Int(b)) => Ok(ConstValue::Bool(a >= b)),
                (ConstValue::Float(a), ConstValue::Float(b)) => Ok(ConstValue::Bool(a >= b)),
                _ => Err(EvalError::TypeMismatch(
                    "Cannot compare these types".to_string(),
                )),
            },

            HirBinaryOp::LogicalAnd => match (left, right) {
                (ConstValue::Bool(a), ConstValue::Bool(b)) => Ok(ConstValue::Bool(a && b)),
                _ => Err(EvalError::TypeMismatch(
                    "Logical AND requires booleans".to_string(),
                )),
            },

            HirBinaryOp::LogicalOr => match (left, right) {
                (ConstValue::Bool(a), ConstValue::Bool(b)) => Ok(ConstValue::Bool(a || b)),
                _ => Err(EvalError::TypeMismatch(
                    "Logical OR requires booleans".to_string(),
                )),
            },

            HirBinaryOp::LeftShift => match (left, right) {
                (ConstValue::Nat(a), ConstValue::Nat(b)) => Ok(ConstValue::Nat(a << b)),
                (ConstValue::Int(a), ConstValue::Nat(b)) => Ok(ConstValue::Int(a << b)),
                _ => Err(EvalError::TypeMismatch(
                    "Cannot shift these types".to_string(),
                )),
            },

            HirBinaryOp::RightShift => match (left, right) {
                (ConstValue::Nat(a), ConstValue::Nat(b)) => Ok(ConstValue::Nat(a >> b)),
                (ConstValue::Int(a), ConstValue::Nat(b)) => Ok(ConstValue::Int(a >> b)),
                _ => Err(EvalError::TypeMismatch(
                    "Cannot shift these types".to_string(),
                )),
            },

            HirBinaryOp::And => match (left, right) {
                (ConstValue::Nat(a), ConstValue::Nat(b)) => Ok(ConstValue::Nat(a & b)),
                (ConstValue::Int(a), ConstValue::Int(b)) => Ok(ConstValue::Int(a & b)),
                _ => Err(EvalError::TypeMismatch(
                    "Bitwise AND requires integers".to_string(),
                )),
            },

            HirBinaryOp::Or => match (left, right) {
                (ConstValue::Nat(a), ConstValue::Nat(b)) => Ok(ConstValue::Nat(a | b)),
                (ConstValue::Int(a), ConstValue::Int(b)) => Ok(ConstValue::Int(a | b)),
                _ => Err(EvalError::TypeMismatch(
                    "Bitwise OR requires integers".to_string(),
                )),
            },

            HirBinaryOp::Xor => match (left, right) {
                (ConstValue::Nat(a), ConstValue::Nat(b)) => Ok(ConstValue::Nat(a ^ b)),
                (ConstValue::Int(a), ConstValue::Int(b)) => Ok(ConstValue::Int(a ^ b)),
                _ => Err(EvalError::TypeMismatch(
                    "Bitwise XOR requires integers".to_string(),
                )),
            },
        }
    }

    /// Evaluate unary operation
    fn eval_unary(&mut self, un: &crate::hir::HirUnaryExpr) -> Result<ConstValue, EvalError> {
        let operand = self.eval(&un.operand)?;

        match un.op {
            HirUnaryOp::Negate => match operand {
                ConstValue::Int(n) => Ok(ConstValue::Int(-n)),
                ConstValue::Nat(n) => Ok(ConstValue::Int(-(n as i64))),
                ConstValue::Float(f) => Ok(ConstValue::Float(-f)),
                _ => Err(EvalError::TypeMismatch(
                    "Cannot negate this type".to_string(),
                )),
            },

            HirUnaryOp::Not => match operand {
                ConstValue::Bool(b) => Ok(ConstValue::Bool(!b)),
                _ => Err(EvalError::TypeMismatch(
                    "Logical NOT requires boolean".to_string(),
                )),
            },

            HirUnaryOp::BitwiseNot => match operand {
                ConstValue::Nat(n) => Ok(ConstValue::Nat(!n)),
                ConstValue::Int(n) => Ok(ConstValue::Int(!n)),
                _ => Err(EvalError::TypeMismatch(
                    "Bitwise NOT requires integer".to_string(),
                )),
            },

            // Reduction operators cannot be evaluated at compile time
            // They require runtime array values
            HirUnaryOp::AndReduce | HirUnaryOp::OrReduce | HirUnaryOp::XorReduce => {
                Err(EvalError::NotConstant(
                    "Reduction operators cannot be evaluated at compile time".to_string(),
                ))
            }
        }
    }
}

// ============================================================================
// Built-in Const Functions
// ============================================================================

/// Ceiling log base 2
fn builtin_clog2(args: &[ConstValue]) -> Result<ConstValue, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::InvalidArgCount {
            expected: 1,
            got: args.len(),
        });
    }

    let n = args[0].as_nat().ok_or_else(|| {
        EvalError::TypeMismatch("clog2 requires natural number argument".to_string())
    })?;

    let result = if n <= 1 {
        0
    } else {
        (n - 1).ilog2() as usize + 1
    };

    Ok(ConstValue::Nat(result))
}

/// Check if power of 2
fn builtin_is_power_of_2(args: &[ConstValue]) -> Result<ConstValue, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::InvalidArgCount {
            expected: 1,
            got: args.len(),
        });
    }

    let n = args[0].as_nat().ok_or_else(|| {
        EvalError::TypeMismatch("is_power_of_2 requires natural number argument".to_string())
    })?;

    let result = n > 0 && (n & (n - 1)) == 0;
    Ok(ConstValue::Bool(result))
}

/// Maximum of two numbers
fn builtin_max(args: &[ConstValue]) -> Result<ConstValue, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::InvalidArgCount {
            expected: 2,
            got: args.len(),
        });
    }

    match (&args[0], &args[1]) {
        (ConstValue::Nat(a), ConstValue::Nat(b)) => Ok(ConstValue::Nat(*a.max(b))),
        (ConstValue::Int(a), ConstValue::Int(b)) => Ok(ConstValue::Int(*a.max(b))),
        (ConstValue::Float(a), ConstValue::Float(b)) => Ok(ConstValue::Float(a.max(*b))),
        _ => Err(EvalError::TypeMismatch(
            "max requires matching numeric types".to_string(),
        )),
    }
}

/// Minimum of two numbers
fn builtin_min(args: &[ConstValue]) -> Result<ConstValue, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::InvalidArgCount {
            expected: 2,
            got: args.len(),
        });
    }

    match (&args[0], &args[1]) {
        (ConstValue::Nat(a), ConstValue::Nat(b)) => Ok(ConstValue::Nat(*a.min(b))),
        (ConstValue::Int(a), ConstValue::Int(b)) => Ok(ConstValue::Int(*a.min(b))),
        (ConstValue::Float(a), ConstValue::Float(b)) => Ok(ConstValue::Float(a.min(*b))),
        _ => Err(EvalError::TypeMismatch(
            "min requires matching numeric types".to_string(),
        )),
    }
}

/// Absolute value
fn builtin_abs(args: &[ConstValue]) -> Result<ConstValue, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::InvalidArgCount {
            expected: 1,
            got: args.len(),
        });
    }

    match &args[0] {
        ConstValue::Int(n) => Ok(ConstValue::Int(n.abs())),
        ConstValue::Nat(n) => Ok(ConstValue::Nat(*n)),
        ConstValue::Float(f) => Ok(ConstValue::Float(f.abs())),
        _ => Err(EvalError::TypeMismatch(
            "abs requires numeric argument".to_string(),
        )),
    }
}

/// Power of 2: compute 2^x
fn builtin_pow2(args: &[ConstValue]) -> Result<ConstValue, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::InvalidArgCount {
            expected: 1,
            got: args.len(),
        });
    }

    let exp = args[0].as_nat().ok_or_else(|| {
        EvalError::TypeMismatch("pow2 requires natural number argument".to_string())
    })?;

    if exp >= usize::BITS as usize {
        return Err(EvalError::TypeMismatch(format!(
            "pow2 exponent {} is too large (max {})",
            exp,
            usize::BITS - 1
        )));
    }

    Ok(ConstValue::Nat(1usize << exp))
}

/// General power: compute base^exp
fn builtin_pow(args: &[ConstValue]) -> Result<ConstValue, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::InvalidArgCount {
            expected: 2,
            got: args.len(),
        });
    }

    let base = args[0]
        .as_nat()
        .ok_or_else(|| EvalError::TypeMismatch("pow requires natural number base".to_string()))?;

    let exp = args[1].as_nat().ok_or_else(|| {
        EvalError::TypeMismatch("pow requires natural number exponent".to_string())
    })?;

    // Use checked_pow to detect overflow
    match base.checked_pow(exp as u32) {
        Some(result) => Ok(ConstValue::Nat(result)),
        None => Err(EvalError::TypeMismatch(format!(
            "pow overflow: {}^{}",
            base, exp
        ))),
    }
}

/// Population count: count number of 1 bits
fn builtin_popcount(args: &[ConstValue]) -> Result<ConstValue, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::InvalidArgCount {
            expected: 1,
            got: args.len(),
        });
    }

    let n = args[0].as_nat().ok_or_else(|| {
        EvalError::TypeMismatch("popcount requires natural number argument".to_string())
    })?;

    Ok(ConstValue::Nat(n.count_ones() as usize))
}

/// Count leading zeros
fn builtin_clz(args: &[ConstValue]) -> Result<ConstValue, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::InvalidArgCount {
            expected: 2,
            got: args.len(),
        });
    }

    let n = args[0].as_nat().ok_or_else(|| {
        EvalError::TypeMismatch("clz requires natural number argument".to_string())
    })?;

    let width = args[1]
        .as_nat()
        .ok_or_else(|| EvalError::TypeMismatch("clz requires width argument".to_string()))?;

    if width > usize::BITS as usize {
        return Err(EvalError::TypeMismatch(format!(
            "clz width {} exceeds maximum {}",
            width,
            usize::BITS
        )));
    }

    // Count leading zeros within the specified width
    let leading_zeros = if n == 0 {
        width
    } else {
        let total_clz = n.leading_zeros() as usize;
        let bits_used = usize::BITS as usize;
        if total_clz >= bits_used - width {
            width - (bits_used - total_clz)
        } else {
            0
        }
    };

    Ok(ConstValue::Nat(leading_zeros))
}

/// Count trailing zeros
fn builtin_ctz(args: &[ConstValue]) -> Result<ConstValue, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::InvalidArgCount {
            expected: 1,
            got: args.len(),
        });
    }

    let n = args[0].as_nat().ok_or_else(|| {
        EvalError::TypeMismatch("ctz requires natural number argument".to_string())
    })?;

    let trailing_zeros = if n == 0 {
        usize::BITS as usize
    } else {
        n.trailing_zeros() as usize
    };

    Ok(ConstValue::Nat(trailing_zeros))
}

/// Reverse bits within a specified width
fn builtin_reverse_bits(args: &[ConstValue]) -> Result<ConstValue, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::InvalidArgCount {
            expected: 2,
            got: args.len(),
        });
    }

    let n = args[0].as_nat().ok_or_else(|| {
        EvalError::TypeMismatch("reverse_bits requires natural number argument".to_string())
    })?;

    let width = args[1].as_nat().ok_or_else(|| {
        EvalError::TypeMismatch("reverse_bits requires width argument".to_string())
    })?;

    if width > usize::BITS as usize {
        return Err(EvalError::TypeMismatch(format!(
            "reverse_bits width {} exceeds maximum {}",
            width,
            usize::BITS
        )));
    }

    // Reverse bits algorithmically
    let mut result = 0usize;
    let mut temp = n;
    for _ in 0..width {
        result = (result << 1) | (temp & 1);
        temp >>= 1;
    }

    Ok(ConstValue::Nat(result))
}

/// Greatest common divisor (Euclidean algorithm)
fn builtin_gcd(args: &[ConstValue]) -> Result<ConstValue, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::InvalidArgCount {
            expected: 2,
            got: args.len(),
        });
    }

    let mut a = args[0].as_nat().ok_or_else(|| {
        EvalError::TypeMismatch("gcd requires natural number arguments".to_string())
    })?;

    let mut b = args[1].as_nat().ok_or_else(|| {
        EvalError::TypeMismatch("gcd requires natural number arguments".to_string())
    })?;

    // Euclidean algorithm
    while b != 0 {
        let temp = b;
        b = a % b;
        a = temp;
    }

    Ok(ConstValue::Nat(a))
}

/// Least common multiple
fn builtin_lcm(args: &[ConstValue]) -> Result<ConstValue, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::InvalidArgCount {
            expected: 2,
            got: args.len(),
        });
    }

    let a = args[0].as_nat().ok_or_else(|| {
        EvalError::TypeMismatch("lcm requires natural number arguments".to_string())
    })?;

    let b = args[1].as_nat().ok_or_else(|| {
        EvalError::TypeMismatch("lcm requires natural number arguments".to_string())
    })?;

    if a == 0 || b == 0 {
        return Ok(ConstValue::Nat(0));
    }

    // LCM = (a * b) / GCD(a, b)
    // Compute GCD inline to avoid recursion
    let mut gcd_a = a;
    let mut gcd_b = b;
    while gcd_b != 0 {
        let temp = gcd_b;
        gcd_b = gcd_a % gcd_b;
        gcd_a = temp;
    }
    let gcd = gcd_a;

    // Use checked arithmetic to detect overflow
    match (a / gcd).checked_mul(b) {
        Some(result) => Ok(ConstValue::Nat(result)),
        None => Err(EvalError::TypeMismatch(format!(
            "lcm overflow: lcm({}, {})",
            a, b
        ))),
    }
}

/// Gray code encoding
fn builtin_gray_encode(args: &[ConstValue]) -> Result<ConstValue, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::InvalidArgCount {
            expected: 1,
            got: args.len(),
        });
    }

    let n = args[0].as_nat().ok_or_else(|| {
        EvalError::TypeMismatch("gray_encode requires natural number argument".to_string())
    })?;

    // Gray code: G = B XOR (B >> 1)
    Ok(ConstValue::Nat(n ^ (n >> 1)))
}

/// Gray code decoding
fn builtin_gray_decode(args: &[ConstValue]) -> Result<ConstValue, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::InvalidArgCount {
            expected: 1,
            got: args.len(),
        });
    }

    let gray = args[0].as_nat().ok_or_else(|| {
        EvalError::TypeMismatch("gray_decode requires natural number argument".to_string())
    })?;

    // Decode gray code by XORing with itself shifted right repeatedly
    let mut binary = gray;
    let mut shift = 1;
    while shift < usize::BITS as usize {
        binary ^= gray >> shift;
        shift <<= 1;
    }

    Ok(ConstValue::Nat(binary))
}

impl Default for ConstEvaluator {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::hir::{HirBinaryExpr, HirBinaryOp, HirExpression, HirLiteral};

    #[test]
    fn test_eval_literal() {
        let mut eval = ConstEvaluator::new();

        let expr = HirExpression::Literal(HirLiteral::Integer(42));
        assert_eq!(eval.eval(&expr).unwrap(), ConstValue::Nat(42));

        let expr = HirExpression::Literal(HirLiteral::Boolean(true));
        assert_eq!(eval.eval(&expr).unwrap(), ConstValue::Bool(true));
    }

    #[test]
    fn test_eval_arithmetic() {
        let mut eval = ConstEvaluator::new();

        // 2 + 3
        let expr = HirExpression::Binary(HirBinaryExpr {
            op: HirBinaryOp::Add,
            left: Box::new(HirExpression::Literal(HirLiteral::Integer(2))),
            right: Box::new(HirExpression::Literal(HirLiteral::Integer(3))),
        });
        assert_eq!(eval.eval(&expr).unwrap(), ConstValue::Nat(5));

        // 10 * 5
        let expr = HirExpression::Binary(HirBinaryExpr {
            op: HirBinaryOp::Mul,
            left: Box::new(HirExpression::Literal(HirLiteral::Integer(10))),
            right: Box::new(HirExpression::Literal(HirLiteral::Integer(5))),
        });
        assert_eq!(eval.eval(&expr).unwrap(), ConstValue::Nat(50));
    }

    #[test]
    fn test_clog2() {
        let mut eval = ConstEvaluator::new();

        // clog2(1024) = 10
        let expr = HirExpression::Call(crate::hir::HirCallExpr {
            function: "clog2".to_string(),
            type_args: vec![],
            named_type_args: std::collections::HashMap::new(),
            args: vec![HirExpression::Literal(HirLiteral::Integer(1024))],
            impl_style: crate::hir::ImplStyle::default(),
        });
        assert_eq!(eval.eval(&expr).unwrap(), ConstValue::Nat(10));

        // clog2(1) = 0
        let expr = HirExpression::Call(crate::hir::HirCallExpr {
            function: "clog2".to_string(),
            type_args: vec![],
            named_type_args: std::collections::HashMap::new(),
            args: vec![HirExpression::Literal(HirLiteral::Integer(1))],
            impl_style: crate::hir::ImplStyle::default(),
        });
        assert_eq!(eval.eval(&expr).unwrap(), ConstValue::Nat(0));
    }

    #[test]
    fn test_generic_param() {
        let mut eval = ConstEvaluator::new();
        eval.bind("N".to_string(), ConstValue::Nat(8));

        // N + 1 = 9
        let expr = HirExpression::Binary(HirBinaryExpr {
            op: HirBinaryOp::Add,
            left: Box::new(HirExpression::GenericParam("N".to_string())),
            right: Box::new(HirExpression::Literal(HirLiteral::Integer(1))),
        });
        assert_eq!(eval.eval(&expr).unwrap(), ConstValue::Nat(9));
    }

    #[test]
    fn test_field_access() {
        let mut eval = ConstEvaluator::new();

        // Create FloatFormat value
        let ieee754_32 = ConstValue::FloatFormat(FloatFormatValue {
            total_bits: 32,
            exponent_bits: 8,
            mantissa_bits: 23,
            bias: 127,
            name: "IEEE754-single".to_string(),
        });

        eval.bind("F".to_string(), ieee754_32);

        // F.total_bits = 32
        let expr = HirExpression::FieldAccess {
            base: Box::new(HirExpression::GenericParam("F".to_string())),
            field: "total_bits".to_string(),
        };
        assert_eq!(eval.eval(&expr).unwrap(), ConstValue::Nat(32));
    }
}

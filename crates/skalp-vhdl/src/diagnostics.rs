use thiserror::Error;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VhdlSeverity {
    Error,
    Warning,
}

#[derive(Debug, Clone)]
pub struct VhdlError {
    pub kind: VhdlErrorKind,
    pub message: String,
    pub position: usize,
    /// End byte offset (equals `position` for point errors)
    pub end_position: usize,
    pub severity: VhdlSeverity,
}

#[derive(Debug, Clone, Error)]
pub enum VhdlErrorKind {
    #[error("lexer error")]
    LexerError,
    #[error("parse error")]
    ParseError,
    #[error("unsynthesizable construct: {0}")]
    Unsynthesizable(String),
    #[error("type error: {0}")]
    TypeError(String),
    #[error("unknown type: {0}")]
    UnknownType(String),
    #[error("lowering error: {0}")]
    LoweringError(String),
    #[error("unresolved name: {0}")]
    UnresolvedName(String),
}

impl std::fmt::Display for VhdlError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.position == self.end_position {
            write!(f, "{} at byte {}", self.message, self.position)
        } else {
            write!(
                f,
                "{} at bytes {}..{}",
                self.message, self.position, self.end_position
            )
        }
    }
}

impl std::error::Error for VhdlError {}

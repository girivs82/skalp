//! Constraint solving for test generation

#[derive(Debug, Clone)]
pub struct Constraint {
    pub name: String,
    pub expression: String,
}

pub struct ConstraintSolver {
    constraints: Vec<Constraint>,
}

impl ConstraintSolver {
    pub fn new() -> Self {
        Self {
            constraints: Vec::new(),
        }
    }

    pub fn add_constraint(&mut self, constraint: Constraint) {
        self.constraints.push(constraint);
    }
}

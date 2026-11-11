//! Write-port analysis for memory inference.
//!
//! An array lowered as a memory gets a single write port: one address, one
//! data word and one enable per clock edge. That holds only when no two writes
//! to different entries can happen in the same cycle. Writes in the arms of
//! one `if`/`else` or `match` exclude each other; writes in sibling statements
//! do not (`if we0 { mem[a0] = d0 }  if we1 { mem[a1] = d1 }` writes both
//! entries when both enables are high), and neither do writes in different
//! clocked blocks or in loop bodies.

use skalp_frontend::hir::{HirEventBlock, HirExpression, HirLValue, HirStatement, SignalId};
use std::collections::{BTreeSet, HashSet};

/// The entry one write site addresses: the text of its index expression.
/// Two sites with the same key in one clocked block write the same entry
/// (signals and ports hold their value for the whole block); a key never
/// matches another when it can change between the two sites.
type EntryKey = String;

/// The arrays among `arrays` that a clocked block in `blocks` can write at two
/// different entries in one cycle.
pub fn arrays_written_concurrently(
    blocks: &[&HirEventBlock],
    arrays: &HashSet<SignalId>,
) -> HashSet<SignalId> {
    let mut indexed = HashSet::new();
    for block in blocks {
        collect_indexed_targets(&block.statements, &mut indexed);
    }
    let mut result = HashSet::new();
    for &array in arrays.intersection(&indexed) {
        let mut sites = WriteSites::new(array);
        let mut alternatives = Alternatives::none_written();
        for (index, block) in blocks.iter().enumerate() {
            let block_alternatives = sites.statements(&block.statements, &format!("b{index}"));
            alternatives = alternatives.then(&block_alternatives);
            if alternatives.concurrent {
                break;
            }
        }
        if alternatives.concurrent {
            result.insert(array);
        }
    }
    result
}

/// The sets of entries a piece of code can write in one cycle, one set per
/// way through it. Each set holds at most one entry: as soon as a way writes
/// two different entries the array is concurrent and nothing more is needed.
#[derive(Clone, Debug)]
struct Alternatives {
    ways: BTreeSet<Option<EntryKey>>,
    concurrent: bool,
}

impl Alternatives {
    fn none_written() -> Self {
        Self {
            ways: BTreeSet::from([None]),
            concurrent: false,
        }
    }

    fn written(key: EntryKey) -> Self {
        Self {
            ways: BTreeSet::from([Some(key)]),
            concurrent: false,
        }
    }

    fn concurrent() -> Self {
        Self {
            ways: BTreeSet::new(),
            concurrent: true,
        }
    }

    /// Code that runs `self` and then `next`
    fn then(&self, next: &Alternatives) -> Alternatives {
        if self.concurrent || next.concurrent {
            return Alternatives::concurrent();
        }
        let mut ways = BTreeSet::new();
        for first in &self.ways {
            for second in &next.ways {
                match (first, second) {
                    (Some(a), Some(b)) if a != b => return Alternatives::concurrent(),
                    (Some(a), _) | (None, Some(a)) => {
                        ways.insert(Some(a.clone()));
                    }
                    (None, None) => {
                        ways.insert(None);
                    }
                }
            }
        }
        Alternatives {
            ways,
            concurrent: false,
        }
    }

    /// Code that runs either `self` or `other`
    fn or(&self, other: &Alternatives) -> Alternatives {
        if self.concurrent || other.concurrent {
            return Alternatives::concurrent();
        }
        Alternatives {
            ways: self.ways.union(&other.ways).cloned().collect(),
            concurrent: false,
        }
    }

    fn writes(&self) -> bool {
        self.concurrent || self.ways.iter().any(Option::is_some)
    }
}

struct WriteSites {
    array: SignalId,
    /// Counter that makes keys unique where an index can change between sites
    unique: usize,
}

impl WriteSites {
    fn new(array: SignalId) -> Self {
        Self { array, unique: 0 }
    }

    fn statements(&mut self, statements: &[HirStatement], scope: &str) -> Alternatives {
        let mut alternatives = Alternatives::none_written();
        for statement in statements {
            alternatives = alternatives.then(&self.statement(statement, scope));
            if alternatives.concurrent {
                break;
            }
        }
        alternatives
    }

    fn statement(&mut self, statement: &HirStatement, scope: &str) -> Alternatives {
        match statement {
            HirStatement::Assignment(assign) => match self.entry_written(&assign.lhs, scope) {
                Some(key) => Alternatives::written(key),
                None => Alternatives::none_written(),
            },
            HirStatement::If(if_stmt) => {
                let then = self.statements(&if_stmt.then_statements, scope);
                let otherwise = match &if_stmt.else_statements {
                    Some(statements) => self.statements(statements, scope),
                    None => Alternatives::none_written(),
                };
                then.or(&otherwise)
            }
            HirStatement::Match(match_stmt) => {
                // Arms are tried in order and at most one runs
                let mut alternatives = Alternatives::none_written();
                for arm in &match_stmt.arms {
                    let arm_alternatives = self.statements(&arm.statements, scope);
                    alternatives = alternatives.or(&arm_alternatives);
                }
                alternatives
            }
            HirStatement::Block(statements) => self.statements(statements, scope),
            // A loop body runs once per iteration, each time with its own index
            HirStatement::For(for_stmt) => self.repeated(&for_stmt.body, scope),
            HirStatement::While(while_stmt) => self.repeated(&while_stmt.body, scope),
            HirStatement::GenerateFor(generate) => {
                self.repeated(&generate.body.generate_stmts, scope)
            }
            HirStatement::GenerateIf(generate) => {
                let then = self.statements(&generate.then_body.generate_stmts, scope);
                let otherwise = match &generate.else_body {
                    Some(body) => self.statements(&body.generate_stmts, scope),
                    None => Alternatives::none_written(),
                };
                then.or(&otherwise)
            }
            HirStatement::GenerateMatch(generate) => {
                let mut alternatives = Alternatives::none_written();
                for arm in &generate.arms {
                    let arm_alternatives = self.statements(&arm.body.generate_stmts, scope);
                    alternatives = alternatives.or(&arm_alternatives);
                }
                alternatives
            }
            _ => Alternatives::none_written(),
        }
    }

    fn repeated(&mut self, body: &[HirStatement], scope: &str) -> Alternatives {
        let once = self.statements(body, scope);
        if once.writes() {
            Alternatives::concurrent()
        } else {
            once
        }
    }

    /// The entry of `self.array` that an assignment to `lhs` writes, if any
    fn entry_written(&mut self, lhs: &HirLValue, scope: &str) -> Option<EntryKey> {
        match lhs {
            HirLValue::Signal(id) if *id == self.array => {
                // The whole array at once: every entry
                self.unique += 1;
                Some(format!("{scope}:whole#{}", self.unique))
            }
            HirLValue::Index(base, index) if matches!(**base, HirLValue::Signal(id) if id == self.array) => {
                if index_is_stable(index) {
                    Some(format!("{scope}:{index:?}"))
                } else {
                    self.unique += 1;
                    Some(format!("{scope}:#{}", self.unique))
                }
            }
            HirLValue::Index(base, _) | HirLValue::Range(base, _, _) => {
                self.entry_written(base, scope)
            }
            HirLValue::FieldAccess { base, .. } => self.entry_written(base, scope),
            _ => None,
        }
    }
}

/// Signals some statement in `statements` writes at an index
fn collect_indexed_targets(statements: &[HirStatement], out: &mut HashSet<SignalId>) {
    for statement in statements {
        match statement {
            HirStatement::Assignment(assign) => {
                let mut lhs = &assign.lhs;
                loop {
                    match lhs {
                        HirLValue::Index(base, _) | HirLValue::Range(base, _, _) => {
                            if let HirLValue::Signal(id) = **base {
                                out.insert(id);
                            }
                            lhs = base;
                        }
                        HirLValue::FieldAccess { base, .. } => lhs = base,
                        _ => break,
                    }
                }
            }
            HirStatement::If(if_stmt) => {
                collect_indexed_targets(&if_stmt.then_statements, out);
                if let Some(statements) = &if_stmt.else_statements {
                    collect_indexed_targets(statements, out);
                }
            }
            HirStatement::Match(match_stmt) => {
                for arm in &match_stmt.arms {
                    collect_indexed_targets(&arm.statements, out);
                }
            }
            HirStatement::Block(statements) => collect_indexed_targets(statements, out),
            HirStatement::For(for_stmt) => collect_indexed_targets(&for_stmt.body, out),
            HirStatement::While(while_stmt) => collect_indexed_targets(&while_stmt.body, out),
            HirStatement::GenerateFor(generate) => {
                collect_indexed_targets(&generate.body.generate_stmts, out)
            }
            HirStatement::GenerateIf(generate) => {
                collect_indexed_targets(&generate.then_body.generate_stmts, out);
                if let Some(body) = &generate.else_body {
                    collect_indexed_targets(&body.generate_stmts, out);
                }
            }
            HirStatement::GenerateMatch(generate) => {
                for arm in &generate.arms {
                    collect_indexed_targets(&arm.body.generate_stmts, out);
                }
            }
            _ => {}
        }
    }
}

/// Whether an index expression has one value throughout a clocked block:
/// it reads only signals, ports, constants and literals, which a clocked
/// block does not change until the edge. Variables are assigned in place.
fn index_is_stable(index: &HirExpression) -> bool {
    match index {
        HirExpression::Literal(_)
        | HirExpression::Signal(_)
        | HirExpression::Port(_)
        | HirExpression::Constant(_)
        | HirExpression::GenericParam(_)
        | HirExpression::EnumVariant { .. }
        | HirExpression::AssociatedConstant { .. } => true,
        HirExpression::Binary(binary) => {
            index_is_stable(&binary.left) && index_is_stable(&binary.right)
        }
        HirExpression::Unary(unary) => index_is_stable(&unary.operand),
        HirExpression::Index(base, bit) => index_is_stable(base) && index_is_stable(bit),
        HirExpression::Range(base, high, low) => {
            index_is_stable(base) && index_is_stable(high) && index_is_stable(low)
        }
        HirExpression::FieldAccess { base, .. } => index_is_stable(base),
        HirExpression::Concat(parts) => parts.iter().all(index_is_stable),
        HirExpression::Ternary {
            condition,
            true_expr,
            false_expr,
        } => {
            index_is_stable(condition) && index_is_stable(true_expr) && index_is_stable(false_expr)
        }
        _ => false,
    }
}

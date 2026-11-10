//! A top module that imports only an entity from another file
//! (`mod regfile_core; use regfile_core::RegfileCore;`).
//!
//! - The entity's implementation calls free functions of its own file. The
//!   merged design must bring them along: the gate-level path failed with
//!   "`port = Call('write_mask', 2 args)` could not be lowered" unless every
//!   helper was imported by name too.
//! - Signal ids are only unique within a file. The top's counter has the same
//!   id as the core's 64x32 register file, and type inference took the first
//!   implementation that had the id, so `cnt + 1` became a 2048-bit add and
//!   the behavioral C++ did not compile.

use skalp_frontend::types::{Type, Width};
use skalp_mir::{ExpressionKind, LValue, Statement};
use skalp_testing::testbench::*;

fn top_path() -> String {
    format!(
        "{}/tests/fixtures/imported_core/imported_core_top.sk",
        env!("CARGO_MANIFEST_DIR")
    )
}

const TOP: &str = "ImportedCoreTop";

/// Write two registers, read one back, and run the counter for 300 cycles
/// (it saturates at 255).
async fn exercise(tb: &mut Testbench) {
    tb.set("we", 1u8)
        .set("waddr", 3u8)
        .set("wdata", 0x0123_4567_89ab_cdefu64)
        .set("size", 1u8)
        .set("off", 2u8)
        .set("a", 0x12u8)
        .set("b", 0x34u8)
        .set("s", 1u8)
        .set("raddr", 3u8);
    tb.clock(1).await;
    tb.set("waddr", 4u8).set("wdata", 0xffu64).set("s", 0u8);
    tb.clock(1).await;
    tb.set("we", 0u8);
    tb.clock(1).await;
    assert_eq!(tb.get_u64("rdata").await, 0x0123_4567_89ab_cdef);
    assert_eq!(tb.get_u64("mask").await, 0b1100);
    assert_eq!(tb.get_u64("q").await, 0x34);
    tb.set("raddr", 4u8).set("size", 0u8).set("off", 3u8);
    tb.clock(297).await;
    assert_eq!(tb.get_u64("rdata").await, 0xff);
    assert_eq!(tb.get_u64("mask").await, 0b1000);
    assert_eq!(tb.get_u64("count").await, 255);
}

#[tokio::test]
async fn test_imported_entity_behavioral() {
    let mut tb = Testbench::with_top_module(&top_path(), TOP).await.unwrap();
    exercise(&mut tb).await;
}

#[tokio::test]
async fn test_imported_entity_gate_level_generic() {
    let mut tb = Testbench::gate_level_with_top_and_library(&top_path(), TOP, "generic_asic")
        .await
        .unwrap();
    exercise(&mut tb).await;
}

#[tokio::test]
async fn test_imported_entity_gate_level_nexus() {
    let mut tb = Testbench::gate_level_with_top_and_library(&top_path(), TOP, "nexus")
        .await
        .unwrap();
    exercise(&mut tb).await;
}

/// The counter's increment keeps the counter's own type.
#[test]
fn test_imported_entity_signal_types_stay_in_their_module() {
    let path = top_path();
    let ctx =
        skalp_frontend::parse_and_build_compilation_context(std::path::Path::new(&path)).unwrap();
    let mir = skalp_mir::MirCompiler::new()
        .compile_to_mir_with_modules(&ctx.main_hir, &ctx.module_hirs)
        .unwrap();
    let top = mir.modules.iter().find(|m| m.name == TOP).unwrap();
    let cnt = top.signals.iter().find(|s| s.name == "cnt").unwrap().id;

    fn find_increment(stmts: &[Statement], cnt: skalp_mir::SignalId) -> Option<Type> {
        for stmt in stmts {
            match stmt {
                Statement::Assignment(a) if matches!(a.lhs, LValue::Signal(id) if id == cnt) => {
                    if let ExpressionKind::Binary { left, .. } = &a.rhs.kind {
                        return Some(left.ty.clone());
                    }
                }
                Statement::If(i) => {
                    if let Some(t) = find_increment(&i.then_block.statements, cnt) {
                        return Some(t);
                    }
                }
                _ => {}
            }
        }
        None
    }

    let ty = top
        .processes
        .iter()
        .find_map(|p| find_increment(&p.body.statements, cnt))
        .expect("cnt = cnt + 1 not found");
    assert!(
        matches!(ty, Type::Bit(Width::Fixed(8))),
        "cnt has type {ty:?} in its own increment"
    );
}

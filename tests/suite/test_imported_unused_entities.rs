//! A module whose file defines entities the top does not import.
//!
//! When the module is rebuilt with the imported entities available, the
//! imported ones keep the ids the merge gave them and the unused ones get
//! fresh ids. Fresh ids used to start again at 0, so an unused entity could
//! share an id with an imported one, and its implementation was then taken as
//! the imported entity's: `Inc` got `IncTwice`'s body and the build failed
//! with "in `Inc`: instance `i2` could not be lowered".

use skalp_testing::testbench::*;

fn top_path() -> String {
    format!(
        "{}/tests/fixtures/imported_unused/imported_unused_top.sk",
        env!("CARGO_MANIFEST_DIR")
    )
}

const TOP: &str = "ImportedUnusedTop";

async fn exercise(tb: &mut Testbench) {
    for x in [0u8, 1, 7, 100, 200, 255] {
        tb.set("x", x);
        tb.clock(1).await;
        assert_eq!(tb.get_u64("inc").await, x.wrapping_add(1) as u64, "x = {x}");
        assert_eq!(tb.get_u64("dbl").await, x.wrapping_mul(2) as u64, "x = {x}");
    }
}

#[test]
fn test_imported_entities_keep_their_implementations() {
    let ctx =
        skalp_frontend::parse_and_build_compilation_context(std::path::Path::new(&top_path()))
            .unwrap();
    let hir = &ctx.main_hir;
    for name in ["Inc", "Dbl"] {
        let entity = hir.entities.iter().find(|e| e.name == name).unwrap();
        assert_eq!(
            hir.entities.iter().filter(|e| e.id == entity.id).count(),
            1,
            "`{name}` shares its id with another entity"
        );
        let body = hir
            .implementations
            .iter()
            .find(|i| i.entity == entity.id)
            .unwrap();
        assert!(
            body.instances.is_empty(),
            "`{name}` was given another entity's instances: {:?}",
            body.instances.iter().map(|i| &i.name).collect::<Vec<_>>()
        );
    }
}

#[tokio::test]
async fn test_imported_entities_behavioral() {
    let mut tb = Testbench::with_top_module(&top_path(), TOP).await.unwrap();
    exercise(&mut tb).await;
}

#[tokio::test]
async fn test_imported_entities_gate_level() {
    let mut tb = Testbench::gate_level_with_top_and_library(&top_path(), TOP, "nexus")
        .await
        .unwrap();
    exercise(&mut tb).await;
}

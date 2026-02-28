/// Integration tests for the VHDL frontend.
///
/// Tests 1-3 exercise the full pipeline: VHDL -> HIR -> MIR -> SIR -> simulation
/// Tests 4-5 verify HIR structure from parse + lower without simulation

// ========================================================================
// Parse-only tests (VHDL -> HIR, no simulation)
// ========================================================================

#[test]
fn test_vhdl_counter_parse_only() {
    let source = std::fs::read_to_string(
        std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("examples/vhdl/counter.vhd"),
    )
    .unwrap();

    let hir = skalp_vhdl::parse_vhdl_source(&source, None).unwrap();

    // Entity
    assert_eq!(hir.entities.len(), 1);
    assert_eq!(hir.entities[0].name, "Counter");
    assert_eq!(hir.entities[0].ports.len(), 4);

    // Architecture lowered to implementation
    assert_eq!(hir.implementations.len(), 1);
    let imp = &hir.implementations[0];

    // At least one signal (count_reg)
    assert!(
        imp.signals.len() >= 1,
        "expected at least 1 signal, got {}",
        imp.signals.len()
    );

    // At least one event block (the clocked process)
    assert!(
        imp.event_blocks.len() >= 1,
        "expected event blocks, got {}",
        imp.event_blocks.len()
    );

    // At least one concurrent assignment (count <= count_reg)
    assert!(
        imp.assignments.len() >= 1,
        "expected assignments, got {}",
        imp.assignments.len()
    );
}

#[test]
fn test_vhdl_mux4_parse_only() {
    let source = std::fs::read_to_string(
        std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("examples/vhdl/mux4.vhd"),
    )
    .unwrap();

    let hir = skalp_vhdl::parse_vhdl_source(&source, None).unwrap();

    assert_eq!(hir.entities.len(), 1);
    assert_eq!(hir.entities[0].name, "Mux4");
    assert_eq!(hir.entities[0].ports.len(), 6);

    assert_eq!(hir.implementations.len(), 1);
    let imp = &hir.implementations[0];

    // Combinational process -> event block with empty triggers
    assert!(imp.event_blocks.len() >= 1);
    let eb = &imp.event_blocks[0];
    assert!(
        eb.triggers.is_empty(),
        "combinational process should have empty triggers"
    );
}

#[test]
fn test_vhdl_case_insensitive() {
    let source = r#"
ENTITY MyDesign IS
    PORT (
        CLK : IN std_logic;
        RST : IN std_logic
    );
END ENTITY MyDesign;
"#;
    let hir = skalp_vhdl::parse_vhdl_source(source, None).unwrap();

    // "MyDesign" -> lexer lowercases to "mydesign" -> PascalCase: "Mydesign"
    assert_eq!(hir.entities[0].name, "Mydesign");
    assert_eq!(hir.entities[0].ports.len(), 2);
}

// ========================================================================
// Full pipeline tests (VHDL -> HIR -> MIR -> SIR -> simulation)
// ========================================================================

use skalp_testing::testbench::*;

#[tokio::test]
async fn test_vhdl_counter_counts_up() {
    let mut tb = Testbench::new("examples/vhdl/counter.vhd")
        .await
        .unwrap();

    // Reset
    tb.set("rst", 1u8).set("en", 0u8);
    tb.clock(2).await;
    tb.expect("count", 0u8).await;

    // Release reset, enable counting
    tb.set("rst", 0u8).set("en", 1u8);
    for expected in 1u8..=5 {
        tb.clock(1).await;
        tb.expect("count", expected).await;
    }

    tb.export_waveform("build/test_vhdl_counter_counts_up.skw.gz")
        .ok();
}

#[tokio::test]
async fn test_vhdl_counter_reset_clears() {
    let mut tb = Testbench::new("examples/vhdl/counter.vhd")
        .await
        .unwrap();

    // Count up to 10
    tb.set("rst", 0u8).set("en", 1u8);
    tb.clock(10).await;
    tb.expect("count", 10u8).await;

    // Assert reset — count should clear
    tb.set("rst", 1u8);
    tb.clock(1).await;
    tb.expect("count", 0u8).await;

    // Release reset, verify counting resumes from 0
    tb.set("rst", 0u8);
    tb.clock(1).await;
    tb.expect("count", 1u8).await;

    tb.export_waveform("build/test_vhdl_counter_reset_clears.skw.gz")
        .ok();
}

#[tokio::test]
async fn test_vhdl_mux4_selects_correctly() {
    let mut tb = Testbench::new("examples/vhdl/mux4.vhd")
        .await
        .unwrap();

    // Set four input channels to distinct values
    tb.set("a", 0x11u8)
        .set("b", 0x22u8)
        .set("c", 0x33u8)
        .set("d", 0x44u8);

    // sel = "00" -> y = a
    tb.set("sel", 0b00u8);
    tb.clock(1).await;
    tb.expect("y", 0x11u8).await;

    // sel = "01" -> y = b
    tb.set("sel", 0b01u8);
    tb.clock(1).await;
    tb.expect("y", 0x22u8).await;

    // sel = "10" -> y = c
    tb.set("sel", 0b10u8);
    tb.clock(1).await;
    tb.expect("y", 0x33u8).await;

    // sel = "11" -> y = d (others branch)
    tb.set("sel", 0b11u8);
    tb.clock(1).await;
    tb.expect("y", 0x44u8).await;

    tb.export_waveform("build/test_vhdl_mux4_selects.skw.gz")
        .ok();
}

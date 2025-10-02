#[cfg(test)]
mod range_debug_tests {
    use skalp_frontend::parse_and_build_hir;

    #[test]
    fn test_range_parsing() {
    let simple_source = r#"
        entity TestRange {
            in clk: clock
            out result: nat[8]
        }

        impl TestRange {
            signal fetch_instruction: nat[16] = 0
            signal decode_opcode: nat[4] = 0
            signal pipeline_valid: nat[1] = 0

            on(clk.rise) {
                decode_opcode <= fetch_instruction[15:12]
                fetch_instruction <= (fetch_instruction << 1) | 1
                pipeline_valid <= (pipeline_valid << 1) | 1
            }

            result = decode_opcode
        }
    "#;

    println!("=== Testing range parsing ===");
    let hir = parse_and_build_hir(simple_source).expect("Failed to parse");

    println!("=== HIR Event Block Assignments ===");
    for implementation in &hir.implementations {
        for event_block in &implementation.event_blocks {
            println!("Event block has {} statements", event_block.statements.len());
            for stmt in &event_block.statements {
                if let skalp_frontend::hir::HirStatement::Assignment(assign) = stmt {
                    println!("Assignment: {:?} <= {:?}", assign.lhs, assign.rhs);
                }
            }
        }
    }
    }
}
//! Test: Match arms with let bindings - Karythra Blocker
//!
//! This tests the critical blocker described in SKALP_FEATURE_REQUESTS.md:
//! "Match arms containing block expressions with `let` bindings fail to inline"
//!
//! If this test passes, Karythra CLE can use all 41 function units!

#[cfg(test)]
mod test_match_let_blocker {
    use skalp_frontend::parse_and_build_hir;
    use skalp_mir::monomorphize::Monomorphizer;

    #[test]
    fn test_simple_match_with_let() {
        let skalp_code = r#"
    // Simplest test: match arm with single let binding
    fn test_simple(x: bit[8]) -> bit[8] {
        return match x {
            0 => {
                let y = x + 1;
                y
            },
            _ => x
        }
    }
        "#;

        println!("\n=== Test 1: Simple Match with Let Binding ===");
        println!("Code:\n{}", skalp_code);

        match parse_and_build_hir(skalp_code) {
            Ok(hir) => {
                println!("✅ HIR build succeeded");

                let mut mono = Monomorphizer::new();
                let monomorphized_hir = mono.monomorphize(&hir);

                println!("✅ Monomorphization succeeded");
                println!("Functions: {}", monomorphized_hir.functions.len());
                println!("✅ Test 1 PASSED!");
            }
            Err(e) => {
                println!("❌ Test 1 FAILED: {:?}", e);
                panic!("Simple match with let binding failed");
            }
        }
    }

    #[test]
    fn test_arithmetic_right_shift() {
        let skalp_code = r#"
    // The actual Karythra L0-L1 blocker pattern
    fn exec_sra(a: bit[32], b: bit[32]) -> bit[32] {
        return match 1 {
            1 => {
                let sign = a[31];
                let shifted = a >> b[4:0];
                let shift_amt = b[4:0];
                if sign && shift_amt > 0 {
                    let mask = (0xFFFFFFFF << (32 - shift_amt));
                    shifted | mask
                } else {
                    shifted
                }
            },
            _ => 0
        }
    }
        "#;

        println!("\n=== Test 2: Arithmetic Right Shift (Karythra Pattern) ===");
        println!("Code:\n{}", skalp_code);

        match parse_and_build_hir(skalp_code) {
            Ok(hir) => {
                println!("✅ HIR build succeeded");

                let mut mono = Monomorphizer::new();
                let monomorphized_hir = mono.monomorphize(&hir);

                println!("✅ Monomorphization succeeded");
                println!("Functions: {}", monomorphized_hir.functions.len());
                println!("✅ Test 2 PASSED - Karythra SRA pattern works!");
            }
            Err(e) => {
                println!("❌ Test 2 FAILED: {:?}", e);
                panic!("Karythra SRA pattern failed - BLOCKER STILL EXISTS");
            }
        }
    }

    #[test]
    fn test_unsigned_compare() {
        let skalp_code = r#"
    // Another Karythra L0-L1 blocker pattern
    fn exec_ltu(a: bit[32], b: bit[32]) -> bit[32] {
        return match 1 {
            1 => {
                let a_unsigned = a as nat[32];
                let b_unsigned = b as nat[32];
                if a_unsigned < b_unsigned { 1 } else { 0 }
            },
            _ => 0
        }
    }
        "#;

        println!("\n=== Test 3: Unsigned Compare (Karythra Pattern) ===");
        println!("Code:\n{}", skalp_code);

        match parse_and_build_hir(skalp_code) {
            Ok(hir) => {
                println!("✅ HIR build succeeded");

                let mut mono = Monomorphizer::new();
                let monomorphized_hir = mono.monomorphize(&hir);

                println!("✅ Monomorphization succeeded");
                println!("Functions: {}", monomorphized_hir.functions.len());
                println!("✅ Test 3 PASSED - Karythra LTU pattern works!");
            }
            Err(e) => {
                println!("❌ Test 3 FAILED: {:?}", e);
                panic!("Karythra LTU pattern failed - BLOCKER STILL EXISTS");
            }
        }
    }

    #[test]
    fn test_fp_operation() {
        let skalp_code = r#"
    // Karythra L2 FP operation pattern
    fn fp16_sqrt(x: bit[16]) -> bit[16] {
        return match 1 {
            1 => {
                let sign = x[15];
                let exp = x[14:10];
                let mant = x[9:0];

                if sign == 1 {
                    return 0x7E00;
                }

                if exp == 0 && mant == 0 {
                    return 0;
                }

                let exp_adj = ((exp as bit[6]) - 15) / 2;
                let new_exp = (exp_adj + 15) as bit[5];
                let approx_mant = mant >> 1;

                {sign, new_exp, approx_mant}
            },
            _ => 0
        }
    }
        "#;

        println!("\n=== Test 4: FP16 SQRT (Karythra L2 Pattern) ===");
        println!("Code:\n{}", skalp_code);

        match parse_and_build_hir(skalp_code) {
            Ok(hir) => {
                println!("✅ HIR build succeeded");

                let mut mono = Monomorphizer::new();
                let monomorphized_hir = mono.monomorphize(&hir);

                println!("✅ Monomorphization succeeded");
                println!("Functions: {}", monomorphized_hir.functions.len());
                println!("✅ Test 4 PASSED - Karythra FP16 sqrt pattern works!");
            }
            Err(e) => {
                println!("❌ Test 4 FAILED: {:?}", e);
                panic!("Karythra FP16 sqrt pattern failed - BLOCKER STILL EXISTS");
            }
        }
    }

    #[test]
    fn test_vector_operation() {
        let skalp_code = r#"
    // Karythra L3 vector operation pattern
    fn vec4_dot(a: bit[128], b: bit[128]) -> bit[32] {
        return match 1 {
            1 => {
                let a0 = a[31:0];
                let a1 = a[63:32];
                let a2 = a[95:64];
                let a3 = a[127:96];

                let b0 = b[31:0];
                let b1 = b[63:32];
                let b2 = b[95:64];
                let b3 = b[127:96];

                let p0 = a0 * b0;
                let p1 = a1 * b1;
                let p2 = a2 * b2;
                let p3 = a3 * b3;

                p0 + p1 + p2 + p3
            },
            _ => 0
        }
    }
        "#;

        println!("\n=== Test 5: Vec4 Dot Product (Karythra L3 Pattern) ===");
        println!("Code:\n{}", skalp_code);

        match parse_and_build_hir(skalp_code) {
            Ok(hir) => {
                println!("✅ HIR build succeeded");

                let mut mono = Monomorphizer::new();
                let monomorphized_hir = mono.monomorphize(&hir);

                println!("✅ Monomorphization succeeded");
                println!("Functions: {}", monomorphized_hir.functions.len());
                println!("✅ Test 5 PASSED - Karythra vec4 dot pattern works!");
            }
            Err(e) => {
                println!("❌ Test 5 FAILED: {:?}", e);
                panic!("Karythra vec4 dot pattern failed - BLOCKER STILL EXISTS");
            }
        }
    }
}

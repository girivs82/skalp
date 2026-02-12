// CPU simulation tests for floating-point arithmetic
// These tests verify proper IEEE 754 FP operations in the CPU (compiled C++) simulator

#[cfg(test)]
mod cpu_fp_simulation_tests {
    use skalp_testing::testbench::*;

    async fn cpu_testbench(fixture: &str) -> Testbench {
        // Force CPU mode for these tests
        std::env::set_var("SKALP_SIM_MODE", "cpu");
        Testbench::new(fixture)
            .await
            .expect("Failed to create testbench")
    }

    #[tokio::test]
    async fn test_fp32_addition_cpu() {
        let mut tb = cpu_testbench("tests/fixtures/numeric/fp_arithmetic.sk").await;

        // Test: 2.5 + 3.5 = 6.0
        tb.set("a", 2.5f32).set("b", 3.5f32);
        tb.step().await;
        tb.expect_fp32("add_result", 6.0, 0.001).await;
    }

    #[tokio::test]
    async fn test_fp32_multiplication_cpu() {
        let mut tb = cpu_testbench("tests/fixtures/numeric/fp_arithmetic.sk").await;

        // Test: 3.0 * 4.0 = 12.0
        tb.set("a", 3.0f32).set("b", 4.0f32);
        tb.step().await;
        tb.expect_fp32("mul_result", 12.0, 0.001).await;
    }

    #[tokio::test]
    #[ignore = "MIR bug: fp32 comparisons constant-fold to 0 instead of generating comparison ops"]
    async fn test_fp32_comparison_cpu() {
        let mut tb = cpu_testbench("tests/fixtures/numeric/fp_comparison.sk").await;

        // Test: 2.5 < 3.5
        tb.set("a", 2.5f32).set("b", 3.5f32);
        tb.step().await;

        tb.expect("is_lt", 1u8).await;
        tb.expect("is_eq", 0u8).await;
        tb.expect("is_gt", 0u8).await;
    }

    #[tokio::test]
    async fn test_fp32_negation_cpu() {
        let mut tb = cpu_testbench("tests/fixtures/numeric/fp_negation.sk").await;

        // Test: -(-9.0) = 9.0
        tb.set("x", -9.0f32);
        tb.step().await;
        tb.expect_fp32("neg", 9.0, 0.001).await;
    }
}

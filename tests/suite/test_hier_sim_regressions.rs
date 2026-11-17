//! Behavioural-simulation regressions found building a multi-level design
//! (controller + PHY model + memory model, each with child instances):
//!
//! - an impl's `on(clk.rise)` bound to a same-named port of the entity
//!   declared last, so its process lost its clock and became combinational
//! - `a == b + c + d` parsed as `(a == b + c) + d`: chains of
//!   same-precedence operators were built as siblings, not left-nested
//! - a function call as an instance port connection was dropped
//! - `Child<P, 7>` inside `Parent<P>` built a `Child` with `A` unbound
//! - a clock output driven by a child's register was reported as a CDC
//! - `a[i]` as an operand of an operator lowered to a library entity read
//!   one bit of the array instead of the element
//! - `let x = ~f(a)`: a call as a unary operand failed to build and the
//!   `let` vanished, leaving `x` undefined (and `-a.m(1)` became `-a`); a
//!   `let` that cannot be built is now an error at the `let`
//! - `{i, 1'b1}` passed to a function and indexed there (`k[0]`) selected a
//!   part of the concatenation (`i`) instead of a bit, whenever the index
//!   was below the number of parts
//! - a call too wide to inline (made a module instance) in an entity with
//!   two specialisations: the second reused the first's cached result
//!   signal, which it does not have, and read 0

use skalp_testing::testbench::Testbench;

fn fixture(name: &str) -> String {
    format!(
        "{}/tests/fixtures/hier_regressions/{}",
        env!("CARGO_MANIFEST_DIR"),
        name
    )
}

#[tokio::test]
async fn impl_after_other_entity_keeps_its_clock() {
    let mut tb = Testbench::behavioral_with_top(&fixture("impl_after_other_entity.sk"), "Outer")
        .await
        .expect("build");
    tb.clock(5).await;
    assert_eq!(tb.get_u32("count").await, 5, "Outer's own register");
    assert_eq!(tb.get_u32("inner_count").await, 10, "Inner's register");
}

#[test]
fn impl_after_other_entity_process_is_sequential() {
    use skalp_mir::ProcessKind;
    let path = fixture("impl_after_other_entity.sk");
    let source = std::fs::read_to_string(&path).expect("read fixture");
    let hir = skalp_frontend::parse_and_build_hir(&source).expect("parse");
    let mir = skalp_mir::MirCompiler::new()
        .compile_to_mir(&hir)
        .expect("mir");
    for name in ["Outer", "Inner"] {
        let m = mir
            .modules
            .iter()
            .find(|m| m.name == name)
            .unwrap_or_else(|| panic!("module {name}"));
        assert!(
            m.processes
                .iter()
                .all(|p| p.kind == ProcessKind::Sequential),
            "{name}: every on(clk.rise) process is sequential"
        );
    }
}

#[tokio::test]
async fn operator_chains_nest_left() {
    let mut tb = Testbench::behavioral_with_top(&fixture("operator_chains.sk"), "OpChains")
        .await
        .expect("build");
    let cases: [(u8, u8, u8, u8); 6] = [
        (12, 3, 4, 5),
        (40, 10, 2, 7),
        (9, 1, 2, 6),
        (255, 17, 33, 1),
        (16, 2, 4, 9),
        (7, 7, 0, 0),
    ];
    for (a, b, c, d) in cases {
        tb.set("a", a).set("b", b).set("c", c).set("d", d);
        tb.step().await;
        let w = |x: u32| x & 0xff;
        let sub_chain = w(a as u32)
            .wrapping_sub(b as u32)
            .wrapping_sub(c as u32)
            .wrapping_add(d as u32)
            & 0xff;
        let eq_sum = (a as u32) == w(b as u32 + c as u32 + d as u32);
        let ge_diff = w(a as u32 + 1) >= w((b as u32).wrapping_sub(c as u32).wrapping_sub(1));
        let mask = ((a | b | c) & d & 15) as u32;
        let mul_eq = (a as u32) == w(b as u32 * 2 * c as u32);
        let logic_mix = (a & 1 == 1 && b & 1 == 1 && c & 1 == 1) || d & 1 == 1;
        let ctx = format!("a={a} b={b} c={c} d={d}");
        assert_eq!(tb.get_u32("sub_chain").await, sub_chain, "{ctx}");
        assert_eq!(tb.get_u32("eq_sum").await, eq_sum as u32, "{ctx}");
        assert_eq!(tb.get_u32("ge_diff").await, ge_diff as u32, "{ctx}");
        assert_eq!(tb.get_u32("mask").await, mask, "{ctx}");
        assert_eq!(tb.get_u32("mul_eq").await, mul_eq as u32, "{ctx}");
        assert_eq!(tb.get_u32("logic_mix").await, logic_mix as u32, "{ctx}");
    }
}

#[tokio::test]
async fn function_call_as_port_connection() {
    let mut tb = Testbench::behavioral_with_top(&fixture("instance_call_connection.sk"), "Top")
        .await
        .expect("build");
    for i in [0u8, 5, 0xA, 0xF] {
        tb.set("i", i);
        tb.step().await;
        assert_eq!(tb.get_u32("o").await, ((i as u32) << 4) | i as u32, "i={i}");
    }
}

#[tokio::test]
async fn generic_forwarded_to_child() {
    let mut tb = Testbench::behavioral_with_top(&fixture("generic_forwarding.sk"), "GfTop")
        .await
        .expect("build");
    tb.step().await;
    assert_eq!(tb.get_u32("o").await, 27, "20 + 7");
    assert_eq!(tb.get_u32("o_default").await, 12, "5 + 7");
}

#[tokio::test]
async fn child_generated_clock_drives_child() {
    // Builds without a CDC error; the divided clock runs the counter
    let mut tb = Testbench::behavioral_with_top(&fixture("generated_clock.sk"), "GcTop")
        .await
        .expect("build");
    tb.clock(40).await;
    let n = tb.get_u32("n").await;
    assert!((9..=11).contains(&n), "one count per 4 clocks, got {n}");
}

#[tokio::test]
async fn array_element_operand_with_library_operators() {
    let stdlib = format!("{}/crates/skalp-stdlib", env!("CARGO_MANIFEST_DIR"));
    std::env::set_var("SKALP_STDLIB_PATH", &stdlib);
    let mut tb = Testbench::behavioral_with_top(&fixture("array_index_operand.sk"), "AiTop")
        .await
        .expect("build");
    let vals = [0x1234_5678u32, 0x0000_00FF, 0xFFFF_FFF0, 0x8000_0001];
    for v in vals {
        tb.set("d", v);
        tb.clock(1).await;
    }
    for (i, v) in vals.iter().enumerate() {
        tb.set("i", i as u8);
        tb.step().await;
        assert_eq!(tb.get_u32("o").await, v.wrapping_add(5), "a[{i}] + 5");
    }
}

#[tokio::test]
async fn unary_operator_on_a_call() {
    let mut tb = Testbench::behavioral_with_top(&fixture("unary_call_operand.sk"), "NotCall")
        .await
        .expect("build");
    for a in [0u8, 1] {
        tb.set("a", a);
        tb.step().await;
        let f = (a as u32 + 0x35) & 0xff;
        assert_eq!(tb.get_u32("p").await, f, "f(a), a={a}");
        assert_eq!(tb.get_u32("o").await, !f & 0xff, "~f(a), a={a}");
        assert_eq!(tb.get_u32("q").await, !f & 7, "~f(a)[2:0], a={a}");
    }
}

#[tokio::test]
async fn bit_index_of_a_concatenation_argument() {
    let mut tb = Testbench::behavioral_with_top(&fixture("concat_argument_bit.sk"), "ConcatArg")
        .await
        .expect("build");
    for i in 0..8u32 {
        for n in 0..4u32 {
            tb.set("i", i as u8).set("n", n as u8);
            tb.step().await;
            let k = (i << 1) | 1;
            assert_eq!(tb.get_u32("lo1").await, 1, "{{i, 1}}[0], i={i}");
            assert_eq!(tb.get_u32("lo0").await, 0, "{{i, 0}}[0], i={i}");
            assert_eq!(tb.get_u32("b1").await, i & 1, "{{i, 1}}[1], i={i}");
            assert_eq!(
                tb.get_u32("dyn").await,
                (k >> n) & 1,
                "{{i, 1}}[{n}], i={i}"
            );
        }
    }
}

#[tokio::test]
async fn wide_call_in_each_specialisation() {
    let mut tb =
        Testbench::behavioral_with_top(&fixture("wide_call_in_specialisation.sk"), "WideTop")
            .await
            .expect("build");
    tb.step().await;
    for p in ["a", "b"] {
        assert_eq!(
            tb.get_u32(&format!("{p}_lo")).await,
            0x5463_7281,
            "{p}: low word"
        );
        assert_eq!(
            tb.get_u32(&format!("{p}_hi")).await,
            0x0918_2736,
            "{p}: high word"
        );
    }
    assert_eq!(tb.get_u32("b_g").await, 200, "b is the specialisation");
}

#[test]
fn let_that_cannot_be_built_is_an_error() {
    let source = "entity E {\n    in a: bit\n    out o: bit[8]\n}\n\nimpl E {\n    let x: bit[8] = Nowhere::X\n    o = x\n}\n";
    let err = match skalp_frontend::parse_and_build_hir(source) {
        Ok(_) => panic!("a let naming an unknown enum must not build"),
        Err(e) => format!("{e:?}"),
    };
    assert!(
        err.contains("cannot build the value of `let x`"),
        "error names the let: {err}"
    );
}

//! The compiled CPU kernel of a large design puts its combinational logic
//! into parts, functions of a bounded size, and the fields of its buffer
//! structs into groups (anonymous member structs), so that compile time
//! grows with the size of the design rather than faster (a gate-level
//! netlist's kernel took over an hour). A kernel in parts must simulate
//! exactly as the kernel in one piece does, stepped and batched.

use skalp_frontend::parse_and_build_hir;
use skalp_mir::MirCompiler;
use skalp_sim::compiled_cpu_runtime::CompiledCpuRuntime;
use skalp_sim::simulator::SimulationRuntime;
use skalp_sir::codegen::CppBackend;
use skalp_sir::{convert_mir_to_sir_with_hierarchy, SirModule};

const SOURCE: &str = r#"
entity Parts {
    in clk: clock
    in rst: bit
    in en: bit
    in a: bit[2]
    in d: bit[8]
    out count: bit[8]
    out acc: bit[16]
    out q: bit[8]
    out mix: bit[8]
}

impl Parts {
    signal cnt: bit[8] = 0
    signal sum: bit[16] = 0
    signal sh: bit[8] = 0
    signal mem: [bit[8]; 4]
    on(clk.rise) {
        if rst {
            cnt = 0
            sum = 0
        } else {
            if en {
                cnt = cnt + 1
            }
            sum = sum + {8'b0, d ^ cnt}
            mem[a] = d + cnt
            sh = {sh[6:0], d[0] ^ cnt[7]}
        }
    }
    count = cnt
    acc = sum
    q = mem[a]
    mix = (cnt & d) | sh
}
"#;

const OUTPUTS: [&str; 4] = ["count", "acc", "q", "mix"];

fn module() -> SirModule {
    let hir = parse_and_build_hir(SOURCE).expect("parse");
    let mir = MirCompiler::new().compile(&hir).expect("MIR");
    let top = mir
        .modules
        .iter()
        .find(|m| m.name == "Parts")
        .expect("Parts")
        .clone();
    convert_mir_to_sir_with_hierarchy(&mir, &top)
}

/// The kernel's name of port `name`
fn internal(sir: &SirModule, name: &str) -> String {
    sir.name_registry.resolve(name).unwrap_or(name).to_string()
}

async fn outputs(sir: &SirModule, rt: &CompiledCpuRuntime) -> Vec<Vec<u8>> {
    let mut values = Vec::new();
    for name in OUTPUTS {
        values.push(rt.get_output(&internal(sir, name)).await.expect(name));
    }
    values
}

async fn set(sir: &SirModule, rt: &mut CompiledCpuRuntime, name: &str, value: &[u8]) {
    rt.set_input(&internal(sir, name), value).await.expect(name);
}

/// Drive both runtimes with the same inputs for one clock cycle (two
/// steps), from a pseudo-random sequence
async fn cycle(sir: &SirModule, runtimes: &mut [&mut CompiledCpuRuntime], n: u32) {
    let x = n.wrapping_mul(2654435761).rotate_left(7);
    let inputs = [
        ("rst", vec![(n < 2) as u8]),
        ("en", vec![(x & 3 != 0) as u8]),
        ("a", vec![((x >> 3) & 3) as u8]),
        ("d", vec![(x >> 8) as u8]),
    ];
    for rt in runtimes.iter_mut() {
        for (name, value) in &inputs {
            set(sir, rt, name, value).await;
        }
    }
    for clk in [0u8, 1] {
        for rt in runtimes.iter_mut() {
            set(sir, rt, "clk", &[clk]).await;
            rt.step().await.expect("step");
        }
    }
}

#[test]
fn small_parts_split_the_logic_and_the_structs() {
    let sir = module();
    let whole = CppBackend::generate(&sir);
    assert!(!whole.contains("combinational_eval_part_"));
    assert!(!whole.contains("sequential_update_in_place"));

    assert!(!whole.contains("    struct {\n"));

    let parts = CppBackend::generate_in_parts(&sir, 3, 2);
    let signals = &parts[parts.find("struct Signals {").expect("Signals")..];
    let signals = &signals[..signals.find("\n};").expect("end of Signals")];
    assert!(
        signals.matches("    struct {\n").count() > 1,
        "Signals fields in groups:\n{signals}"
    );
    assert!(parts.contains("static SKALP_NOINLINE void combinational_eval_part_0("));
    assert!(parts.contains("static SKALP_NOINLINE void combinational_eval_part_1("));
    assert!(parts.contains("combinational_eval_part_1(inputs, registers, signals);"));
    assert!(parts.contains("static SKALP_NOINLINE void sequential_update_in_place("));
}

#[tokio::test]
async fn kernel_in_parts_simulates_as_one_function() {
    let sir = module();
    let mut whole = CompiledCpuRuntime::new(&sir).expect("kernel in one function");
    let mut parts = CompiledCpuRuntime::with_kernel_parts(&sir, 3, 2).expect("kernel in parts");
    whole.initialize(&sir).await.expect("init");
    parts.initialize(&sir).await.expect("init");

    // Stepped: every output after every cycle
    for n in 0..200 {
        cycle(&sir, &mut [&mut whole, &mut parts], n).await;
        assert_eq!(
            outputs(&sir, &whole).await,
            outputs(&sir, &parts).await,
            "outputs after cycle {n}"
        );
    }
    assert_ne!(
        outputs(&sir, &whole).await[1],
        vec![0, 0],
        "accumulator moved"
    );

    // Batched: the kernel in parts runs its own batched loop
    for rt in [&mut whole, &mut parts] {
        set(&sir, rt, "rst", &[0]).await;
        set(&sir, rt, "en", &[1]).await;
        set(&sir, rt, "d", &[0x5a]).await;
        let clk = internal(&sir, "clk");
        assert!(rt.run_batched_cycles(37, Some(&clk)), "batched run");
    }
    assert_eq!(
        outputs(&sir, &whole).await,
        outputs(&sir, &parts).await,
        "after batched run"
    );
}

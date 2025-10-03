use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion};
use skalp_frontend::parse_and_build_hir;
use skalp_mir::{MirCompiler, OptimizationLevel};
use skalp_sim::{SimulationConfig, Simulator};
use skalp_sir::convert_mir_to_sir;
use tokio::runtime::Runtime;

const COUNTER_SOURCE: &str = r#"
entity Counter {
    in clk: clock
    in rst: reset
    out count: nat[8]
}

impl Counter {
    signal counter: nat[8] = 0

    on(clk.rise) {
        if (rst) {
            counter <= 0
        } else {
            counter <= counter + 1
        }
    }

    count = counter
}
"#;

async fn benchmark_simulation(use_gpu: bool, cycles: u64) -> u64 {
    // Parse and compile
    let hir = parse_and_build_hir(COUNTER_SOURCE).unwrap();
    let compiler = MirCompiler::new().with_optimization_level(OptimizationLevel::Basic);
    let mir = compiler.compile_to_mir(&hir).unwrap();
    let sir = convert_mir_to_sir(&mir.modules[0]);

    // Create simulator
    let config = SimulationConfig {
        use_gpu,
        max_cycles: cycles,
        timeout_ms: 60000,
        capture_waveforms: false, // Disable for benchmarking
        parallel_threads: 4,
    };

    let mut simulator = Simulator::new(config).await.unwrap();
    simulator.load_module(&sir).await.unwrap();

    // Initialize inputs
    simulator.set_input("rst", vec![0]).await.unwrap();
    simulator.set_input("clk", vec![0]).await.unwrap();

    // Run simulation
    for i in 0..cycles {
        simulator
            .set_input("clk", vec![(i % 2) as u8])
            .await
            .unwrap();
        simulator.step_simulation().await.unwrap();
    }

    cycles
}

fn bench_gpu_vs_cpu(c: &mut Criterion) {
    let rt = Runtime::new().unwrap();
    let mut group = c.benchmark_group("gpu_vs_cpu_real");

    for cycles in [100, 500, 1000].iter() {
        group.bench_with_input(BenchmarkId::new("CPU", cycles), cycles, |b, &cycles| {
            b.iter(|| rt.block_on(async { benchmark_simulation(false, cycles).await }));
        });

        group.bench_with_input(BenchmarkId::new("GPU", cycles), cycles, |b, &cycles| {
            b.iter(|| rt.block_on(async { benchmark_simulation(true, cycles).await }));
        });
    }

    group.finish();
}

criterion_group!(benches, bench_gpu_vs_cpu);
criterion_main!(benches);

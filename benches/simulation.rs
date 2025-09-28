//! Simulation performance benchmarks

use criterion::{black_box, criterion_group, criterion_main, Criterion, BenchmarkId, Throughput};
use std::collections::BinaryHeap;

fn benchmark_gpu_vs_cpu(c: &mut Criterion) {
    let mut group = c.benchmark_group("gpu_vs_cpu_simulation");

    let design_sizes = vec![
        ("small", 100),    // 100 gates
        ("medium", 500),   // 500 gates
        ("large", 1000),   // 1K gates
    ];

    for (name, size) in design_sizes {
        group.throughput(Throughput::Elements(size as u64));

        // CPU simulation
        group.bench_with_input(
            BenchmarkId::new("cpu", name),
            &size,
            |b, &size| {
                b.iter(|| {
                    // Simulate CPU-based evaluation
                    let mut state = vec![0u32; size];
                    for cycle in 0..100 {
                        for i in 0..size {
                            state[i] = black_box((state[i] + cycle) % 256);
                        }
                    }
                    black_box(state[0])
                });
            },
        );

        // GPU simulation (simulated with parallel chunks)
        group.bench_with_input(
            BenchmarkId::new("gpu_simulated", name),
            &size,
            |b, &size| {
                b.iter(|| {
                    // Simulate GPU parallel processing
                    let chunks = size / 32; // Simulate 32-thread warps
                    let mut results = vec![0u32; chunks];

                    for cycle in 0..100 {
                        // Parallel evaluation (simulated)
                        for i in 0..chunks {
                            results[i] = black_box((i as u32 * cycle) % 256);
                        }
                    }
                    black_box(results[0])
                });
            },
        );
    }

    group.finish();
}

fn benchmark_event_scheduling(c: &mut Criterion) {
    c.bench_function("event_queue_1000", |b| {
        b.iter(|| {
            // Simulate event-driven simulation with a priority queue
            let mut events = BinaryHeap::new();

            // Add 1000 events
            for i in 0..1000 {
                events.push(black_box(1000 - i)); // Reverse order for max heap
            }

            // Process 500 events
            let mut processed = 0;
            while processed < 500 {
                if let Some(_event) = events.pop() {
                    processed += 1;
                } else {
                    break;
                }
            }

            black_box(processed)
        });
    });
}

fn benchmark_signal_updates(c: &mut Criterion) {
    let mut group = c.benchmark_group("signal_updates");

    let signal_counts = vec![100, 500, 1000];

    for count in signal_counts {
        group.bench_with_input(
            BenchmarkId::from_parameter(count),
            &count,
            |b, &count| {
                b.iter(|| {
                    // Simulate signal update propagation
                    let mut signals = vec![0u64; count];
                    let mut changed = vec![false; count];

                    // Initial changes
                    for i in 0..10 {
                        signals[i] = i as u64;
                        changed[i] = true;
                    }

                    // Propagate changes
                    let mut iterations = 0;
                    let mut any_changed = true;

                    while any_changed && iterations < 10 {
                        any_changed = false;
                        for i in 1..count {
                            if changed[i - 1] {
                                signals[i] = black_box(signals[i - 1] + 1);
                                changed[i] = true;
                                any_changed = true;
                            }
                        }
                        iterations += 1;
                    }

                    black_box(signals[count - 1])
                });
            },
        );
    }

    group.finish();
}

fn benchmark_coverage_overhead(c: &mut Criterion) {
    let mut group = c.benchmark_group("coverage");

    group.bench_function("without_coverage", |b| {
        b.iter(|| {
            // Simulate execution without coverage
            let mut state = 0u64;
            for i in 0..1000 {
                state = black_box(state.wrapping_add(i));
            }
            state
        });
    });

    group.bench_function("with_coverage", |b| {
        b.iter(|| {
            // Simulate execution with coverage tracking
            let mut state = 0u64;
            let mut coverage = vec![false; 1000];

            for i in 0..1000 {
                state = black_box(state.wrapping_add(i));
                coverage[i as usize] = true; // Track statement coverage
            }

            // Calculate coverage percentage
            let covered = coverage.iter().filter(|&&x| x).count();
            black_box((state, covered))
        });
    });

    group.finish();
}

criterion_group!(
    benches,
    benchmark_gpu_vs_cpu,
    benchmark_event_scheduling,
    benchmark_signal_updates,
    benchmark_coverage_overhead
);

criterion_main!(benches);
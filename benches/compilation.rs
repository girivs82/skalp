//! Compilation performance benchmarks

use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion};
use std::time::Duration;

fn benchmark_lexing(c: &mut Criterion) {
    let sources = vec![
        ("small", include_str!("../examples/counter.sk")),
        ("medium", include_str!("../examples/fifo.sk")),
    ];

    let mut group = c.benchmark_group("lexing");

    for (name, source) in sources {
        group.bench_with_input(BenchmarkId::from_parameter(name), &source, |b, source| {
            use skalp_frontend::lexer::Lexer;
            b.iter(|| {
                let mut lexer = Lexer::new(source);
                let tokens = lexer.tokenize();
                black_box(tokens.len())
            });
        });
    }
    group.finish();
}

fn benchmark_parsing(c: &mut Criterion) {
    let sources = vec![
        ("counter", include_str!("../examples/counter.sk")),
        ("fifo", include_str!("../examples/fifo.sk")),
    ];

    let mut group = c.benchmark_group("parsing");

    for (name, source) in sources {
        group.bench_with_input(BenchmarkId::from_parameter(name), &source, |b, source| {
            use skalp_frontend::parser::Parser;
            b.iter(|| {
                let mut parser = Parser::new(source);
                let ast = parser.parse();
                black_box(ast.is_ok())
            });
        });
    }
    group.finish();
}

fn benchmark_optimization_passes(c: &mut Criterion) {
    let mut group = c.benchmark_group("optimizations");

    group.bench_function("constant_folding", |b| {
        b.iter(|| {
            // Simulate constant folding
            let mut values = vec![1, 2, 3, 4, 5];
            for _ in 0..100 {
                values[0] = black_box((values[1] + values[2]) * 2);
                values[1] = black_box(values[0] / 2);
            }
            black_box(values[0])
        });
    });

    group.bench_function("dead_code_elimination", |b| {
        b.iter(|| {
            // Simulate dead code elimination
            let mut live_blocks = vec![true; 1000];
            let mut removed = 0;
            for i in 0..1000 {
                if i % 3 == 0 {
                    live_blocks[i] = false;
                    removed += 1;
                }
            }
            black_box(removed)
        });
    });

    group.finish();
}

fn benchmark_end_to_end(c: &mut Criterion) {
    let sources = vec![
        ("counter", include_str!("../examples/counter.sk")),
        ("fifo", include_str!("../examples/fifo.sk")),
    ];

    let mut group = c.benchmark_group("end_to_end");

    for (name, source) in sources {
        group.bench_with_input(BenchmarkId::from_parameter(name), &source, |b, source| {
            use skalp_frontend::parser::Parser;
            b.iter(|| {
                // Parse the source
                let mut parser = Parser::new(source);
                let result = parser.parse();

                // Simulate rest of compilation pipeline
                if result.is_ok() {
                    // Simulate HIR generation
                    for _ in 0..10 {
                        black_box(format!("node_{}", 1));
                    }

                    // Simulate code generation
                    let mut output = String::new();
                    for i in 0..20 {
                        output.push_str(&format!("wire sig_{};\n", i));
                    }
                    black_box(output.len())
                } else {
                    black_box(0)
                }
            });
        });
    }

    group.finish();
}

criterion_group!(
    benches,
    benchmark_lexing,
    benchmark_parsing,
    benchmark_optimization_passes,
    benchmark_end_to_end
);

criterion_main!(benches);

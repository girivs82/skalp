# SKALP API Reference

Complete API documentation for the SKALP compiler and tools.

## Table of Contents

1. [Compiler API](#compiler-api)
2. [Frontend API](#frontend-api)
3. [MIR API](#mir-api)
4. [Code Generation API](#code-generation-api)
5. [Simulation API](#simulation-api)
6. [Verification API](#verification-api)
7. [Language Server API](#language-server-api)

---

## Compiler API

### Main Compiler Interface

```rust
use skalp::Compiler;
use skalp::CompilerOptions;

// Create compiler with options
let options = CompilerOptions {
    target: Target::SystemVerilog,
    optimization_level: OptLevel::O2,
    output_dir: PathBuf::from("output"),
    verbose: true,
};

let mut compiler = Compiler::new(options);

// Compile a design
compiler.compile_file("design.sk")?;

// Or compile from string
compiler.compile_str(source_code)?;
```

### Compiler Options

```rust
pub struct CompilerOptions {
    /// Target output format
    pub target: Target,

    /// Optimization level (O0, O1, O2, O3)
    pub optimization_level: OptLevel,

    /// Output directory
    pub output_dir: PathBuf,

    /// Enable verbose output
    pub verbose: bool,

    /// Enable debug information
    pub debug_info: bool,

    /// Additional include paths
    pub include_paths: Vec<PathBuf>,
}

pub enum Target {
    SystemVerilog,
    VHDL,
    FPGA(FPGATarget),
    ASIC(ASICTarget),
    Simulation,
}
```

---

## Frontend API

### Lexer

```rust
use skalp_frontend::lexer::Lexer;

let source = "entity Counter { ... }";
let lexer = Lexer::new(source);
let tokens = lexer.tokenize();

for token in tokens {
    println!("{:?}: {}", token.kind, token.text);
}
```

### Parser

```rust
use skalp_frontend::parser::Parser;

let parser = Parser::new(tokens);
let ast = parser.parse()?;

// Access AST nodes
match ast {
    AstNode::Entity(entity) => {
        println!("Entity: {}", entity.name);
    }
    _ => {}
}
```

### HIR Builder

```rust
use skalp_frontend::hir_builder::HirBuilder;

let mut builder = HirBuilder::new();
let hir = builder.build(ast)?;

// Access HIR elements
for entity in &hir.entities {
    println!("Entity: {} with {} ports",
             entity.name, entity.ports.len());
}
```

### Type System

```rust
use skalp_frontend::types::{Type, TypeChecker};

let mut checker = TypeChecker::new();

// Check type compatibility
let t1 = Type::Bit(8);
let t2 = Type::Bit(8);
assert!(checker.compatible(&t1, &t2));

// Type inference
let inferred = checker.infer_type(expression)?;
```

---

## MIR API

### MIR Transformer

```rust
use skalp_mir::transformer::HirToMir;

let mut transformer = HirToMir::new();
let mir = transformer.transform(&hir)?;

// Access MIR modules
for module in &mir.modules {
    println!("Module: {} with {} signals",
             module.name, module.signals.len());
}
```

### MIR Optimization

```rust
use skalp_mir::optimizer::{MirOptimizer, OptimizationPass};

let mut optimizer = MirOptimizer::new();

// Add optimization passes
optimizer.add_pass(OptimizationPass::ConstantFolding);
optimizer.add_pass(OptimizationPass::DeadCodeElimination);
optimizer.add_pass(OptimizationPass::CommonSubexpressionElimination);

// Run optimizations
let optimized_mir = optimizer.optimize(mir)?;
```

### CDC Analysis

```rust
use skalp_mir::cdc::{CdcAnalyzer, CdcViolation};

let analyzer = CdcAnalyzer::new();
let violations = analyzer.analyze(&mir);

for violation in violations {
    println!("CDC Violation: {:?} at {}",
             violation.violation_type,
             violation.location.module_name);
}
```

---

## Code Generation API

### SystemVerilog Generator

```rust
use skalp_codegen::SystemVerilogGenerator;

let mut generator = SystemVerilogGenerator::new();

// Configure generator
generator.set_indent_width(4);
generator.set_use_always_ff(true);

// Generate code
let verilog_code = generator.generate(&mir)?;

// Write to file
generator.write_to_file(&mir, "output.sv")?;
```

### VHDL Generator

```rust
use skalp_codegen::VhdlGenerator;

let mut generator = VhdlGenerator::new();
generator.set_standard(VhdlStandard::VHDL2008);

let vhdl_code = generator.generate(&mir)?;
```

---

## Simulation API

### Simulation Engine

```rust
use skalp_sim::{SimulationEngine, SimulationConfig};

let config = SimulationConfig {
    use_gpu: true,
    max_cycles: 10000,
    timeout: Duration::from_secs(60),
    waveform_output: Some("waves.vcd"),
};

let mut engine = SimulationEngine::new(config);

// Load design
engine.load_sir(sir)?;

// Run simulation
engine.run()?;

// Get results
let cycles = engine.get_cycle_count();
let coverage = engine.get_coverage_report();
```

### Async Testbench

```rust
use skalp_sim::testbench::AsyncTestbench;

let mut tb = AsyncTestbench::new("my_test");

// Define test
tb.add_test("basic_test", async {
    // Reset
    tb.set_signal("rst", 1).await;
    tb.wait_cycles(2).await;
    tb.set_signal("rst", 0).await;

    // Stimulus
    tb.set_signal("data", 0xFF).await;
    tb.wait_cycles(10).await;

    // Check
    let result = tb.get_signal("output").await;
    assert_eq!(result, 0x55);
});

// Run tests
tb.run_all().await?;
```

### GPU Simulation (macOS)

```rust
use skalp_sim::gpu::MetalSimulator;

let mut simulator = MetalSimulator::new()?;

// Generate and compile shader
let shader = simulator.generate_shader(&sir)?;
simulator.compile_shader(shader)?;

// Run on GPU
let results = simulator.simulate(1000)?;
```

---

## Verification API

### Assertions

```rust
use skalp_verify::assertions::{Assertion, PropertyBuilder};

// Create property
let property = PropertyBuilder::new()
    .name("req_ack_property")
    .sequence("req |-> ##[1:3] ack")
    .build();

// Add to design
verifier.add_assertion(property);

// Check assertions
let results = verifier.check_assertions()?;
```

### Coverage

```rust
use skalp_verify::coverage::{CoverageCollector, CoverageType};

let mut collector = CoverageCollector::new();

// Enable coverage types
collector.enable_coverage(CoverageType::Statement);
collector.enable_coverage(CoverageType::Branch);
collector.enable_coverage(CoverageType::FSM);
collector.enable_coverage(CoverageType::Toggle);

// Run and collect
engine.run_with_coverage(&mut collector)?;

// Generate report
let report = collector.generate_report();
println!("Coverage: {:.2}%", report.total_coverage_percentage());
```

### Formal Verification

```rust
use skalp_verify::formal::{FormalVerifier, SolverType};

let mut verifier = FormalVerifier::new(SolverType::Z3);

// Add constraints
verifier.add_constraint("counter < 256");
verifier.add_invariant("!overflow || counter == 0");

// Verify
let proof_result = verifier.verify(&mir)?;

match proof_result {
    ProofResult::Proven => println!("Property proven!"),
    ProofResult::CounterExample(cex) => {
        println!("Counter-example found: {:?}", cex);
    }
    ProofResult::Unknown => println!("Could not prove or disprove"),
}
```

---

## Language Server API

### LSP Server

```rust
use skalp_lsp::create_lsp_service;
use tower_lsp::Server;

// Create LSP service
let (service, socket) = create_lsp_service();

// Run server
Server::new(stdin, stdout, socket)
    .serve(service)
    .await;
```

### Custom Language Features

```rust
use skalp_lsp::{SkalpLanguageServer, DocumentState};

impl SkalpLanguageServer {
    /// Add custom completion provider
    pub fn add_completion_provider<F>(&mut self, provider: F)
    where
        F: Fn(&DocumentState, Position) -> Vec<CompletionItem>
    {
        // Implementation
    }

    /// Add custom diagnostic analyzer
    pub fn add_diagnostic_analyzer<F>(&mut self, analyzer: F)
    where
        F: Fn(&str) -> Vec<Diagnostic>
    {
        // Implementation
    }
}
```

---

## Standard Library API

### Using Standard Components

```rust
use skalp_stdlib::{get_components, COUNTER_SK, FIFO_SK};

// Get all available components
let components = get_components();
for (name, source) in components {
    println!("Component: {}", name);
}

// Use specific component
compiler.add_library_component("counter", COUNTER_SK);
compiler.add_library_component("fifo", FIFO_SK);
```

---

## Error Handling

### Error Types

```rust
use skalp::{SkalpError, Result};

fn compile_design() -> Result<()> {
    let compiler = Compiler::new(options);

    match compiler.compile_file("design.sk") {
        Ok(_) => println!("Success!"),
        Err(SkalpError::ParseError(e)) => {
            eprintln!("Parse error: {} at line {}", e.message, e.line);
        }
        Err(SkalpError::TypeError(e)) => {
            eprintln!("Type error: {}", e);
        }
        Err(e) => {
            eprintln!("Error: {}", e);
        }
    }

    Ok(())
}
```

### Diagnostic Reporting

```rust
use skalp::diagnostics::{Diagnostic, DiagnosticLevel};

let diagnostic = Diagnostic {
    level: DiagnosticLevel::Error,
    message: "Clock domain crossing detected".to_string(),
    location: Location {
        file: "design.sk",
        line: 42,
        column: 15,
    },
    suggestion: Some("Use a synchronizer".to_string()),
};

diagnostic.report();
```

---

## Command Line Interface

### CLI Builder

```rust
use skalp::cli::{CliApp, Command};

let app = CliApp::new()
    .version("1.0.0")
    .author("SKALP Team")
    .about("SKALP Hardware Synthesis Compiler");

// Add commands
app.add_command(Command::Compile)
   .add_command(Command::Simulate)
   .add_command(Command::Verify);

// Parse and run
let matches = app.get_matches();
app.run(matches)?;
```

---

## Examples

### Complete Compilation Pipeline

```rust
use skalp::prelude::*;

fn compile_pipeline(source: &str) -> Result<String> {
    // Frontend
    let lexer = Lexer::new(source);
    let tokens = lexer.tokenize();
    let parser = Parser::new(tokens);
    let ast = parser.parse()?;

    // HIR
    let mut hir_builder = HirBuilder::new();
    let hir = hir_builder.build(ast)?;

    // Type checking
    let mut type_checker = TypeChecker::new();
    type_checker.check(&hir)?;

    // MIR
    let mut transformer = HirToMir::new();
    let mir = transformer.transform(&hir)?;

    // Optimization
    let mut optimizer = MirOptimizer::new();
    let optimized = optimizer.optimize(mir)?;

    // Code generation
    let mut generator = SystemVerilogGenerator::new();
    let output = generator.generate(&optimized)?;

    Ok(output)
}
```

### Custom Backend

```rust
use skalp_backends::{Backend, BackendResult};

struct MyCustomBackend;

#[async_trait::async_trait]
impl Backend for MyCustomBackend {
    async fn synthesize(
        &self,
        lir: &LirDesign,
        config: &SynthesisConfig,
    ) -> BackendResult<SynthesisResults> {
        // Custom synthesis implementation
        Ok(SynthesisResults {
            gates: lir.gates.len(),
            area: 1000.0,
            power: 50.0,
            timing: TimingReport::default(),
        })
    }
}
```

---

## Performance Tips

1. **Use batch operations** when processing multiple files
2. **Enable parallel compilation** with `rayon` features
3. **Cache parsed results** for incremental compilation
4. **Use GPU simulation** for large designs
5. **Profile with `cargo bench` using provided benchmarks

---

## Version Compatibility

| SKALP Version | API Version | Rust Version |
|---------------|-------------|--------------|
| 0.1.x         | 1.0         | 1.70+        |
| 0.2.x         | 2.0         | 1.75+        |

---

For more examples and detailed usage, see the `examples/` directory in the repository.
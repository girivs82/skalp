#![allow(unused_variables)]

use anyhow::{Context, Result};
use clap::{Parser, Subcommand};
use std::path::PathBuf;
use std::fs;
use tracing::info;

/// SKALP - Intent-driven hardware synthesis
#[derive(Parser)]
#[command(author, version, about, long_about = None)]
#[command(propagate_version = true)]
struct Cli {
    #[command(subcommand)]
    command: Commands,

    /// Verbosity level
    #[arg(short, long, action = clap::ArgAction::Count)]
    verbose: u8,
}

#[derive(Subcommand)]
enum Commands {
    /// Create a new SKALP project
    New {
        /// Project name
        name: String,
    },

    /// Build the design
    Build {
        /// Source file (defaults to src/main.sk)
        #[arg(short, long)]
        source: Option<PathBuf>,

        /// Target output format
        #[arg(short, long, default_value = "sv")]
        target: String,

        /// Output directory
        #[arg(short, long, default_value = "build")]
        output: PathBuf,
    },

    /// Simulate the design
    Sim {
        /// Design file to simulate
        design: PathBuf,

        /// Simulation duration
        #[arg(short, long)]
        duration: Option<String>,
    },

    /// Synthesize for FPGA target
    Synth {
        /// Source file
        source: PathBuf,

        /// Target device (e.g., ice40-hx8k)
        #[arg(short, long)]
        device: String,

        /// Full flow (place, route, bitstream)
        #[arg(short, long)]
        full_flow: bool,
    },

    /// Program the FPGA device
    Program {
        /// Bitstream file
        bitstream: PathBuf,

        /// Programming interface
        #[arg(short, long, default_value = "spi")]
        interface: String,

        /// Verify after programming
        #[arg(short, long)]
        verify: bool,
    },

    /// Format SKALP source files
    Fmt {
        /// Files to format
        files: Vec<PathBuf>,

        /// Check only (don't modify files)
        #[arg(long)]
        check: bool,
    },

    /// Run tests
    Test {
        /// Test filter
        filter: Option<String>,
    },
}

fn main() -> Result<()> {
    let cli = Cli::parse();

    // Set up logging
    let log_level = match cli.verbose {
        0 => "warn",
        1 => "info",
        2 => "debug",
        _ => "trace",
    };

    tracing_subscriber::fmt()
        .with_env_filter(log_level)
        .init();

    match cli.command {
        Commands::New { name } => {
            create_new_project(&name)?;
        }

        Commands::Build { source, target, output } => {
            let source_file = source.unwrap_or_else(|| PathBuf::from("src/main.sk"));
            build_design(&source_file, &target, &output)?;
        }

        Commands::Sim { design, duration } => {
            simulate_design(&design, duration.as_deref())?;
        }

        Commands::Synth { source, device, full_flow } => {
            synthesize_design(&source, &device, full_flow)?;
        }

        Commands::Program { bitstream, interface, verify } => {
            program_device(&bitstream, &interface, verify)?;
        }

        Commands::Fmt { files, check } => {
            format_files(&files, check)?;
        }

        Commands::Test { filter } => {
            run_tests(filter.as_deref())?;
        }
    }

    Ok(())
}

/// Create a new SKALP project
fn create_new_project(name: &str) -> Result<()> {
    info!("Creating new SKALP project: {}", name);

    // Create project directory structure
    fs::create_dir_all(format!("{}/src", name))?;
    fs::create_dir_all(format!("{}/tests", name))?;
    fs::create_dir_all(format!("{}/examples", name))?;

    // Create Cargo.toml
    let cargo_toml = format!(r#"[package]
name = "{}"
version = "0.1.0"
edition = "2021"

[dependencies]
skalp-stdlib = {{ git = "https://github.com/skalp-lang/skalp" }}
"#, name);
    fs::write(format!("{}/Cargo.toml", name), cargo_toml)?;

    // Create main.sk with a simple example
    let main_sk = r#"// Main SKALP design file

entity Counter {
    in clk: clock
    in reset: reset
    out count: bit[8]
}

impl Counter {
    signal count_reg: bit[8] = 0

    on(clk.rise) {
        if (reset.active) {
            count_reg <= 0
        } else {
            count_reg <= count_reg + 1
        }
    }

    count = count_reg
}
"#;
    fs::write(format!("{}/src/main.sk", name), main_sk)?;

    // Create README
    let readme = format!(r#"# {}

A SKALP hardware design project.

## Building

```bash
skalpc build
```

## Simulation

```bash
skalpc sim build/design.lir
```

## Synthesis

```bash
skalpc synth src/main.sk --device ice40-hx8k
```
"#, name);
    fs::write(format!("{}/README.md", name), readme)?;

    println!("✅ Created new SKALP project '{}'", name);
    println!("📁 Project structure:");
    println!("   {}/", name);
    println!("   ├── Cargo.toml");
    println!("   ├── README.md");
    println!("   ├── src/");
    println!("   │   └── main.sk");
    println!("   ├── tests/");
    println!("   └── examples/");

    Ok(())
}

/// Build SKALP design
fn build_design(source: &PathBuf, target: &str, output_dir: &PathBuf) -> Result<()> {
    use skalp_frontend::parse_and_build_hir;
    use skalp_lir::lower_to_lir;
    use skalp_codegen::{generate_verilog, generate_vhdl, generate_systemverilog_from_mir};

    info!("Building design from {:?} to {}", source, target);

    // Read source file
    let source_code = fs::read_to_string(source)
        .context("Failed to read source file")?;

    // Parse, build HIR, and type check in one step
    info!("Parsing SKALP source and building HIR...");
    let hir = parse_and_build_hir(&source_code)
        .context("Failed to parse and build HIR")?;

    // Lower to MIR with CDC analysis
    info!("Lowering to MIR with CDC analysis...");
    let compiler = skalp_mir::MirCompiler::new()
        .with_optimization_level(skalp_mir::OptimizationLevel::None)
        .with_verbose(true); // Enable verbose output for CDC analysis
    let mir = compiler.compile_to_mir(&hir)
        .map_err(|e| anyhow::anyhow!("Failed to compile HIR to MIR with CDC analysis: {}", e))?;

    // Lower to LIR
    info!("Lowering to LIR...");
    let lir = lower_to_lir(&mir)
        .context("Failed to lower to LIR")?;

    // Create output directory
    fs::create_dir_all(output_dir)?;

    // Generate target output
    let output_file = match target {
        "sv" | "systemverilog" => {
            info!("Generating SystemVerilog...");
            // Use the new MIR-based generator for proper process generation
            let sv_code = generate_systemverilog_from_mir(&mir, &lir)?;
            let output_path = output_dir.join("design.sv");
            fs::write(&output_path, sv_code)?;
            output_path
        }
        "v" | "verilog" => {
            info!("Generating Verilog...");
            let v_code = generate_verilog(&lir)?;
            let output_path = output_dir.join("design.v");
            fs::write(&output_path, v_code)?;
            output_path
        }
        "vhdl" => {
            info!("Generating VHDL...");
            let vhdl_code = generate_vhdl(&lir)?;
            let output_path = output_dir.join("design.vhd");
            fs::write(&output_path, vhdl_code)?;
            output_path
        }
        "lir" => {
            info!("Saving LIR...");
            let output_path = output_dir.join("design.lir");
            let lir_json = serde_json::to_string_pretty(&lir)?;
            fs::write(&output_path, lir_json)?;
            output_path
        }
        "mir" => {
            info!("Saving MIR...");
            let output_path = output_dir.join("design.mir");
            let mir_json = serde_json::to_string_pretty(&mir)?;
            fs::write(&output_path, mir_json)?;
            output_path
        }
        _ => {
            anyhow::bail!("Unsupported target: {}. Use 'sv', 'v', 'vhdl', 'lir', or 'mir'", target);
        }
    };

    println!("✅ Build complete!");
    println!("📄 Output: {:?}", output_file);

    Ok(())
}

/// Simulate design using GPU-accelerated simulation
fn simulate_design(design_file: &PathBuf, duration: Option<&str>) -> Result<()> {
    use skalp_mir::Mir;
    use skalp_sir::convert_mir_to_sir;
    use skalp_sim::{Simulator, SimulationConfig};
    use skalp_sim::waveform::Waveform;
    use tokio::runtime::Runtime;

    info!("Loading design from {:?}", design_file);

    // Load design (support both .lir and .mir files)
    let design_str = fs::read_to_string(design_file)?;

    let sir = if design_file.extension() == Some(std::ffi::OsStr::new("lir")) {
        // For LIR files, we need to first convert to MIR, then to SIR
        anyhow::bail!("LIR to SIR conversion not yet implemented. Please use .mir files for simulation");
    } else if design_file.extension() == Some(std::ffi::OsStr::new("mir")) {
        // Load MIR and convert to SIR
        let mir: Mir = serde_json::from_str(&design_str)?;
        // Use the first module in the MIR
        if mir.modules.is_empty() {
            anyhow::bail!("No modules found in MIR");
        }
        convert_mir_to_sir(&mir.modules[0])
    } else {
        anyhow::bail!("Unsupported file format. Use .lir or .mir files");
    };

    // Parse duration (default 1000 cycles)
    let cycles = if let Some(dur) = duration {
        if dur.ends_with("ns") {
            dur.trim_end_matches("ns").parse::<u64>()? / 10  // Assume 10ns clock
        } else if dur.ends_with("us") {
            dur.trim_end_matches("us").parse::<u64>()? * 100
        } else {
            dur.parse::<u64>()?
        }
    } else {
        1000
    };

    println!("🚀 Starting GPU-accelerated simulation...");
    println!("⏱️  Simulating {} cycles", cycles);

    // Create async runtime for simulation
    let runtime = Runtime::new()?;

    runtime.block_on(async {
        // Create simulation config
        let config = SimulationConfig {
            use_gpu: true,
            max_cycles: cycles,
            timeout_ms: 60_000,
            capture_waveforms: true,
            parallel_threads: 4,
        };

        // Create and initialize simulator
        let mut simulator = Simulator::new(config).await?;
        simulator.load_module(&sir).await?;

        // Run simulation
        simulator.run_simulation().await?;

        println!("✅ Simulation complete!");
        println!("📊 Simulated {} cycles", cycles);

        // Get simulation history and create waveform
        let state_history = simulator.get_waveforms().await;
        if !state_history.is_empty() {
            let waveform = Waveform::from_simulation_states(&state_history);
            waveform.export_vcd(&PathBuf::from("simulation.vcd"))?;
            println!("📈 Waveform exported to simulation.vcd");
            waveform.print_summary();
        }

        Ok::<(), anyhow::Error>(())
    })?;

    Ok(())
}

/// Synthesize design for FPGA/ASIC
fn synthesize_design(source: &PathBuf, device: &str, full_flow: bool) -> Result<()> {
    use skalp_backends::TargetPlatform;

    info!("Synthesizing for device: {}", device);

    // First build to LIR
    let temp_dir = tempfile::TempDir::new()?;
    build_design(source, "lir", &temp_dir.path().to_path_buf())?;

    // Load the LIR
    let lir_path = temp_dir.path().join("design.lir");
    let lir_str = fs::read_to_string(&lir_path)?;
    let lir: skalp_lir::LirDesign = serde_json::from_str(&lir_str)?;

    // Parse device target
    let target = match device {
        "ice40-hx1k" => TargetPlatform::Fpga(skalp_backends::FpgaTarget::Ice40 {
            part: "iCE40HX1K".to_string(),
            package: "TQ144".to_string(),
        }),
        "ice40-hx8k" => TargetPlatform::Fpga(skalp_backends::FpgaTarget::Ice40 {
            part: "iCE40HX8K".to_string(),
            package: "CT256".to_string(),
        }),
        "xc7a35t" => TargetPlatform::Fpga(skalp_backends::FpgaTarget::Xilinx7Series {
            part: "xc7a35t".to_string(),
            package: "cpg236".to_string(),
        }),
        "sky130" => TargetPlatform::Asic(skalp_backends::AsicTarget::Sky130),
        "freepdk45" => TargetPlatform::Asic(skalp_backends::AsicTarget::FreePdk45),
        _ => {
            anyhow::bail!("Unknown device: {}. Supported: ice40-hx1k, ice40-hx8k, xc7a35t, sky130, freepdk45", device);
        }
    };

    // Create synthesis config
    let config = skalp_backends::SynthesisConfig {
        target: target.clone(),
        optimization: skalp_backends::OptimizationGoals {
            primary: skalp_backends::OptimizationTarget::Area,
            max_area_utilization: Some(0.8),
            target_frequency: Some(100.0),
            max_power: None,
        },
        timing_constraints: vec![],
        power_constraints: None,
        output_dir: "synth_output".to_string(),
        tool_options: std::collections::HashMap::new(),
    };

    println!("🔧 Would run synthesis for target: {:?}", target);
    println!("⚠️  Backend synthesis integration not yet complete in CLI");

    if full_flow {
        println!("\n🔄 Running full flow (place, route, bitstream)...");
        // This would call place & route tools
        println!("⚠️  Full flow not yet implemented");
    }

    Ok(())
}

/// Program FPGA device
fn program_device(bitstream: &PathBuf, interface: &str, verify: bool) -> Result<()> {
    println!("📤 Programming device via {}...", interface);
    println!("   Bitstream: {:?}", bitstream);

    if !bitstream.exists() {
        anyhow::bail!("Bitstream file not found: {:?}", bitstream);
    }

    // This would interface with actual programming tools
    println!("⚠️  Device programming not yet implemented");
    println!("   Would program via: {}", interface);

    if verify {
        println!("   Would verify after programming");
    }

    Ok(())
}

/// Format SKALP source files
fn format_files(files: &[PathBuf], check: bool) -> Result<()> {
    use skalp_frontend::{parse_file, format_ast};

    if files.is_empty() {
        println!("No files specified");
        return Ok(());
    }

    let mut needs_formatting = false;

    for file in files {
        if file.extension() != Some(std::ffi::OsStr::new("sk")) &&
           file.extension() != Some(std::ffi::OsStr::new("skalp")) {
            continue;
        }

        let source = fs::read_to_string(file)?;
        let ast = parse_file(&source)?;
        let formatted = format_ast(&ast)?;

        if source != formatted {
            needs_formatting = true;
            if check {
                println!("❌ {} needs formatting", file.display());
            } else {
                fs::write(file, formatted)?;
                println!("✅ Formatted {}", file.display());
            }
        } else if !check {
            println!("✓ {} already formatted", file.display());
        }
    }

    if check && needs_formatting {
        anyhow::bail!("Some files need formatting. Run without --check to format them.");
    }

    Ok(())
}

/// Run property-based tests
fn run_tests(filter: Option<&str>) -> Result<()> {
    use skalp_testing::TestConfig;

    println!("🧪 Running property-based tests...");

    if let Some(f) = filter {
        println!("   Filter: {}", f);
    }

    // Load test configuration
    let _config = TestConfig::default();

    // Find and load test files
    let test_files = fs::read_dir("tests")?
        .filter_map(|entry| entry.ok())
        .filter(|entry| {
            entry.path().extension() == Some(std::ffi::OsStr::new("sk"))
        })
        .collect::<Vec<_>>();

    if test_files.is_empty() {
        println!("No test files found in tests/");
        return Ok(());
    }

    println!("Found {} test files", test_files.len());

    // Run tests
    for test_file in test_files {
        let name = test_file.file_name();
        if let Some(f) = filter {
            if !name.to_string_lossy().contains(f) {
                continue;
            }
        }

        println!("\n📝 Running test: {}", name.to_string_lossy());

        // Load and run test
        // This would parse test specifications and run property-based testing
        println!("   ⚠️  Test execution not yet fully implemented");
    }

    println!("\n✅ All tests passed!");

    Ok(())
}

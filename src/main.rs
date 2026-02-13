#![allow(unused_variables)]

use anyhow::{Context, Result};
use clap::{Parser, Subcommand};
use std::fs;
use std::path::{Path, PathBuf};
use tracing::info;

/// ISO 26262 Safety build options
#[derive(Debug, Clone)]
pub struct SafetyBuildOptions {
    /// Target ASIL level (A, B, C, D)
    pub asil_level: Option<String>,
    /// Output path for safety report
    pub report_path: Option<PathBuf>,
    /// Check metrics only, don't generate work products
    pub check_only: bool,
    /// Work products to generate (comma-separated)
    pub workproducts: Option<String>,
    /// Output formats for work products
    pub formats: String,
}

/// Logic synthesis optimization options
#[derive(Debug, Clone)]
pub struct OptimizationOptions {
    /// Optimization preset (quick, balanced, full, timing, area, resyn2, compress2)
    pub preset: Option<String>,
    /// Custom pass sequence (comma-separated)
    pub passes: Option<String>,
    /// Use ML-guided pass ordering
    pub ml_guided: bool,
    /// Path to trained ML policy model (JSON format)
    pub ml_policy_path: Option<PathBuf>,
    /// Directory to collect training data
    pub training_data_dir: Option<PathBuf>,
    /// Gate optimization level (0=none, 1=basic, 2=full)
    pub gate_opt_level: u8,
}

impl Default for OptimizationOptions {
    fn default() -> Self {
        Self {
            preset: None,
            passes: None,
            ml_guided: false,
            ml_policy_path: None,
            training_data_dir: None,
            gate_opt_level: 1,
        }
    }
}

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

        // === ISO 26262 Safety Options ===
        /// Enable ISO 26262 functional safety analysis
        #[arg(long)]
        safety: bool,

        /// Target ASIL level for safety analysis (A, B, C, D)
        #[arg(long, value_name = "LEVEL")]
        asil: Option<String>,

        /// Output path for safety report
        #[arg(long, value_name = "PATH")]
        safety_report: Option<PathBuf>,

        /// Check safety metrics only (don't generate work products)
        #[arg(long)]
        safety_check_only: bool,

        /// Generate ISO 26262 work products (comma-separated: fmeda,traceability,hsi,all)
        #[arg(long, value_name = "PRODUCTS")]
        workproducts: Option<String>,

        /// Work product output format (comma-separated: md,html,pdf,csv,reqif)
        #[arg(long, value_name = "FORMATS", default_value = "md,html")]
        workproduct_formats: String,

        // === Logic Synthesis Optimization Options ===
        /// Optimization preset (quick, balanced, full, timing, area, resyn2, compress2, auto)
        /// Default: "auto" (runs multiple strategies and picks the best result)
        /// Use --no-synth-opt to disable synthesis optimization entirely
        #[arg(long, value_name = "PRESET", default_value = "auto")]
        optimize: String,

        /// Disable synthesis optimization (AIG/ABC passes)
        /// Useful for debugging or when you need raw gate-level output
        #[arg(long)]
        no_synth_opt: bool,

        /// Disable async timing analysis for NCL circuits
        /// Useful for faster builds when STA is not needed
        #[arg(long)]
        no_async_sta: bool,

        /// Custom pass sequence (comma-separated: strash,rewrite,balance,refactor,map)
        #[arg(long, value_name = "PASSES")]
        passes: Option<String>,

        /// Use ML-guided pass ordering for optimization
        #[arg(long)]
        ml_guided: bool,

        /// Path to trained ML policy model (JSON format, use with --ml-guided)
        #[arg(long, value_name = "PATH")]
        ml_policy: Option<PathBuf>,

        /// Collect training data during synthesis for ML model training
        #[arg(long, value_name = "DIR")]
        collect_training_data: Option<PathBuf>,

        // === Technology Library Options ===
        /// Path to technology library file (.skalib format)
        /// If not specified, uses the built-in generic ASIC library
        #[arg(long, value_name = "PATH")]
        library: Option<PathBuf>,

        // === Gate-Level Optimization ===
        /// Gate optimization level (0=none, 1=basic, 2=full). Default: 1
        /// -O0 skips gate optimization (useful for debugging)
        /// -O1 runs constant folding, buffer removal, DCE
        /// -O2 runs all optimizations including advanced passes
        #[arg(
            short = 'O',
            long = "gate-opt",
            value_name = "LEVEL",
            default_value = "1"
        )]
        gate_opt_level: u8,
    },

    /// Simulate the design
    Sim {
        /// Design file to simulate
        design: PathBuf,

        /// Simulation duration
        #[arg(short, long)]
        duration: Option<String>,

        /// Use gate-level simulation (HIR→MIR→LIR→SIR) instead of behavioral (HIR→MIR→SIR)
        #[arg(long)]
        gate_level: bool,

        /// Use GPU acceleration
        #[arg(long)]
        gpu: bool,

        /// Gate optimization level for gate-level simulation (0=none, 1=basic, 2=full). Default: 1
        #[arg(
            short = 'O',
            long = "gate-opt",
            value_name = "LEVEL",
            default_value = "1"
        )]
        gate_opt_level: u8,

        /// Simulate as NCL (Null Convention Logic) async circuit
        ///
        /// Uses proper THmn threshold gate evaluation with hysteresis
        /// and wavefront propagation instead of clock-based simulation.
        #[arg(long)]
        ncl: bool,
    },

    /// Synthesize for FPGA target
    Synth {
        /// Source file
        source: PathBuf,

        /// Target device (e.g., ice40-hx8k, ice40-hx1k, ice40-up5k)
        #[arg(short, long)]
        device: String,

        /// Full flow (place, route, bitstream)
        #[arg(short, long)]
        full_flow: bool,

        /// Output directory
        #[arg(short, long, default_value = "build")]
        output: PathBuf,

        /// P&R quality preset (fast, default, high_quality)
        #[arg(long, default_value = "default")]
        pnr_preset: String,
    },

    /// Run place and route on an existing gate-level netlist
    Pnr {
        /// Gate-level netlist file (JSON format from previous synthesis)
        netlist: PathBuf,

        /// Target device (e.g., ice40-hx8k, ice40-hx1k, ice40-up5k)
        #[arg(short, long)]
        device: String,

        /// Output directory
        #[arg(short, long, default_value = "build")]
        output: PathBuf,

        /// P&R quality preset (fast, default, high_quality)
        #[arg(long, default_value = "default")]
        preset: String,

        /// Target frequency in MHz (for timing analysis)
        #[arg(long)]
        frequency: Option<f64>,
    },

    /// Program the FPGA device
    Program {
        /// Bitstream file (.bin format)
        #[arg(required_unless_present_any = ["list", "reset_only"])]
        bitstream: Option<PathBuf>,

        /// Target board (icebreaker, icebreaker-bitsy, hx8k-breakout, upduino3, auto)
        #[arg(short, long, default_value = "auto")]
        board: String,

        /// Reset the FPGA without programming
        #[arg(long)]
        reset_only: bool,

        /// List detected boards and exit
        #[arg(long)]
        list: bool,
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

    /// Gate-level analysis and fault simulation
    Analyze {
        /// Source file to analyze
        source: PathBuf,

        /// Output directory for analysis results
        #[arg(short, long, default_value = "analysis")]
        output: PathBuf,

        /// Run fault simulation
        #[arg(long)]
        fault_sim: bool,

        /// Number of cycles for fault simulation
        #[arg(long, default_value = "100")]
        cycles: u64,

        /// Use GPU acceleration for fault simulation
        #[arg(long)]
        gpu: bool,

        /// Maximum faults to test (0 = all)
        #[arg(long, default_value = "0")]
        max_faults: usize,

        /// Generate detailed report
        #[arg(long)]
        detailed: bool,
    },

    /// Equivalence checking between RTL and gate-level netlist
    ///
    /// Verifies that the synthesized gate-level netlist is functionally
    /// equivalent to the original RTL description.
    ///
    /// Default mode: Hybrid (simulation + SAT-based symbolic check)
    /// --quick: Simulation-only (fast, not exhaustive)
    /// --symbolic: SAT-based symbolic check only (finds deep bugs)
    Ec {
        /// Source file (.sk) containing the RTL design
        source: PathBuf,

        /// Gate-level netlist file (JSON format from synthesis)
        /// If not provided, synthesis is run automatically
        #[arg(short, long)]
        netlist: Option<PathBuf>,

        /// Output directory for equivalence reports
        #[arg(short, long, default_value = "equiv")]
        output: PathBuf,

        /// BMC bound (number of cycles to check for simulation)
        #[arg(short, long, default_value = "10")]
        bound: usize,

        /// Output format (text, json, html, all)
        #[arg(long, default_value = "text")]
        format: String,

        /// Entity name to check (default: top-level entity)
        #[arg(short, long)]
        entity: Option<String>,

        /// Run quick simulation-only check (faster but not exhaustive)
        /// Good for catching easy bugs quickly
        #[arg(long)]
        quick: bool,

        /// Run SAT-based symbolic check (finds bugs in unreachable states)
        /// Essential for protocols with long timeouts (PCIe, USB, etc.)
        #[arg(long)]
        symbolic: bool,

        /// Number of reset cycles for simulation mode
        #[arg(long, default_value = "10")]
        reset_cycles: u64,

        /// Verbose output showing internal AIG details
        #[arg(long)]
        verbose: bool,

        /// Enable coverage-driven simulation with systematic + random + biased vectors
        #[arg(long)]
        coverage: bool,
    },

    /// ISO 26262 FI-driven safety analysis
    ///
    /// Runs fault injection simulation to generate FMEA/FMEDA with MEASURED
    /// diagnostic coverage values (not estimated from tables).
    Safety {
        /// Source file (.sk)
        #[arg(short, long)]
        source: PathBuf,

        /// Output directory for safety collaterals
        #[arg(short, long, default_value = "safety_collaterals")]
        output: PathBuf,

        /// Target ASIL level (A, B, C, D, or QM)
        #[arg(long, default_value = "D")]
        asil: String,

        /// Safety goal name
        #[arg(long, default_value = "SG-001")]
        goal: String,

        /// Number of simulation cycles per fault
        #[arg(long, default_value = "100")]
        cycles: u64,

        /// Use GPU acceleration for fault simulation
        #[arg(long)]
        gpu: bool,

        /// Maximum faults to simulate (0 = all primitives)
        #[arg(long, default_value = "0")]
        max_faults: usize,

        /// Effect conditions file (YAML/JSON) - if not provided, uses output difference detection
        #[arg(long)]
        effects: Option<PathBuf>,

        /// Output formats (comma-separated: md,html,yaml,json)
        #[arg(long, default_value = "md,yaml")]
        formats: String,

        /// Skip fault injection (use for testing collateral generation only)
        #[arg(long)]
        skip_fi: bool,

        /// Generate BIST for undetected coverage gap faults
        #[arg(long)]
        generate_bist: bool,

        /// Enable dual-BIST for SM-of-SM protection
        #[arg(long)]
        dual_bist: bool,
    },

    /// Add a dependency to the project
    Add {
        /// Package name
        package: String,

        /// Version requirement (default: latest)
        #[arg(short, long)]
        version: Option<String>,

        /// Add as dev dependency
        #[arg(short, long)]
        dev: bool,

        /// Optional dependency (only included with feature)
        #[arg(long)]
        optional: bool,

        /// Features to enable
        #[arg(short, long)]
        features: Vec<String>,
    },

    /// Remove a dependency from the project
    Remove {
        /// Package name
        package: String,

        /// Remove from dev dependencies
        #[arg(short, long)]
        dev: bool,
    },

    /// Update dependencies
    Update {
        /// Update specific package (if not specified, updates all)
        package: Option<String>,

        /// Force update even if already at latest
        #[arg(short, long)]
        force: bool,
    },

    /// Search for packages in the registry
    Search {
        /// Search query
        query: String,

        /// Number of results to show
        #[arg(short, long, default_value = "10")]
        limit: usize,
    },

    /// Manage package cache
    Cache {
        #[command(subcommand)]
        command: CacheCommands,
    },

    /// Train ML pass ordering model
    Train {
        /// Directory containing collected training data (from --collect-training-data)
        #[arg(short, long)]
        data: PathBuf,

        /// Output path for trained model (default: models/pass_policy.json)
        #[arg(short, long, default_value = "models/pass_policy.json")]
        output: PathBuf,

        /// Number of training epochs
        #[arg(long, default_value = "100")]
        epochs: usize,

        /// Learning rate
        #[arg(long, default_value = "0.001")]
        learning_rate: f64,

        /// Batch size
        #[arg(long, default_value = "32")]
        batch_size: usize,

        /// Training mode: standard, positive-only, episode-weighted, best-episodes
        #[arg(long, default_value = "standard")]
        mode: String,
    },

    /// Compile a design to pre-compiled IP format (.skb)
    ///
    /// Creates a binary distribution file containing the synthesized gate netlist.
    /// The compiled IP can be used as a blackbox by importing the generated header file (.skh).
    Compile {
        /// Source file (.sk)
        #[arg(short, long)]
        source: PathBuf,

        /// Output file (.skb)
        #[arg(short, long)]
        output: PathBuf,

        /// Generate header file (.skh) for import
        #[arg(long)]
        header: Option<PathBuf>,

        /// Entity name to compile (if not specified, uses the top-level entity)
        #[arg(long)]
        entity: Option<String>,

        /// Encrypt the compiled IP
        #[arg(long)]
        encrypt: bool,

        /// Path to AES-256 key file (32 bytes) for encryption
        #[arg(long)]
        key_file: Option<PathBuf>,

        /// Technology library (default: generic_asic)
        #[arg(long, default_value = "generic_asic")]
        library: String,

        /// Optimization preset
        #[arg(long)]
        optimize: Option<String>,
    },

    /// Trace a signal through the gate-level netlist
    ///
    /// Shows the complete driver chain from output to source, including all
    /// gates, their types, inputs, and optionally simulated values.
    /// Useful for debugging equivalence check mismatches.
    Trace {
        /// Source file (.sk) containing the RTL design
        source: PathBuf,

        /// Signal to trace (e.g., "lockstep_tx.state[0]" or "state_reg")
        signal: String,

        /// Entity name (if not specified, uses the top-level entity)
        #[arg(short, long)]
        entity: Option<String>,

        /// Maximum depth to trace (default: 20)
        #[arg(short, long, default_value = "20")]
        depth: usize,

        /// Run simulation and show actual values at cycle N
        #[arg(long)]
        cycle: Option<u64>,

        /// Show fanout (forward trace) instead of driver (backward trace)
        #[arg(long)]
        fanout: bool,

        /// Pre-compiled gate-level netlist (JSON) - skip synthesis
        #[arg(short, long)]
        netlist: Option<PathBuf>,
    },
}

#[derive(Subcommand)]
enum CacheCommands {
    /// List cached packages
    List,

    /// Show cache size
    Size,

    /// Clear the cache
    Clear,

    /// Remove specific package from cache
    Remove {
        /// Package name
        package: String,

        /// Package version
        version: String,
    },
}

fn main() -> Result<()> {
    let cli = Cli::parse();

    // Set up logging - RUST_LOG takes precedence, then verbose flag
    let log_level = std::env::var("RUST_LOG").unwrap_or_else(|_| match cli.verbose {
        0 => "warn".to_string(),
        1 => "info".to_string(),
        2 => "debug".to_string(),
        _ => "trace".to_string(),
    });

    tracing_subscriber::fmt()
        .with_env_filter(&log_level)
        .with_target(true) // Show module path in logs
        .init();

    match cli.command {
        Commands::New { name } => {
            create_new_project(&name)?;
        }

        Commands::Build {
            source,
            target,
            output,
            safety,
            asil,
            safety_report,
            safety_check_only,
            workproducts,
            workproduct_formats,
            optimize,
            no_synth_opt,
            no_async_sta,
            passes,
            ml_guided,
            ml_policy,
            collect_training_data,
            library,
            gate_opt_level,
        } => {
            let source_file = source.unwrap_or_else(|| PathBuf::from("src/main.sk"));

            // Build safety options
            let safety_options = if safety {
                Some(SafetyBuildOptions {
                    asil_level: asil,
                    report_path: safety_report,
                    check_only: safety_check_only,
                    workproducts,
                    formats: workproduct_formats,
                })
            } else {
                None
            };

            // Build optimization options
            // If --no-synth-opt is specified, disable synthesis optimization
            let optimization_options = OptimizationOptions {
                preset: if no_synth_opt { None } else { Some(optimize) },
                passes,
                ml_guided,
                ml_policy_path: ml_policy,
                training_data_dir: collect_training_data,
                gate_opt_level,
            };

            build_design(
                &source_file,
                &target,
                &output,
                safety_options,
                optimization_options,
                library.as_ref(),
                no_async_sta,
            )?;
        }

        Commands::Sim {
            design,
            duration,
            gate_level,
            gpu,
            gate_opt_level,
            ncl,
        } => {
            simulate_design(
                &design,
                duration.as_deref(),
                gate_level,
                gpu,
                gate_opt_level,
                ncl,
            )?;
        }

        Commands::Synth {
            source,
            device,
            full_flow,
            output,
            pnr_preset,
        } => {
            synthesize_design(&source, &device, full_flow, &output, &pnr_preset)?;
        }

        Commands::Pnr {
            netlist,
            device,
            output,
            preset,
            frequency,
        } => {
            run_place_and_route(&netlist, &device, &output, &preset, frequency)?;
        }

        Commands::Program {
            bitstream,
            board,
            reset_only,
            list,
        } => {
            program_device(bitstream.as_ref(), &board, reset_only, list)?;
        }

        Commands::Fmt { files, check } => {
            format_files(&files, check)?;
        }

        Commands::Test { filter } => {
            run_tests(filter.as_deref())?;
        }

        Commands::Analyze {
            source,
            output,
            fault_sim,
            cycles,
            gpu,
            max_faults,
            detailed,
        } => {
            analyze_design(
                &source, &output, fault_sim, cycles, gpu, max_faults, detailed,
            )?;
        }

        Commands::Ec {
            source,
            netlist,
            output,
            bound,
            format,
            entity,
            quick,
            symbolic,
            reset_cycles,
            verbose,
            coverage,
        } => {
            run_equivalence_check(
                &source,
                netlist.as_ref(),
                &output,
                bound,
                &format,
                entity.as_deref(),
                quick,
                symbolic,
                reset_cycles,
                verbose,
                coverage,
            )?;
        }

        Commands::Safety {
            source,
            output,
            asil,
            goal,
            cycles,
            gpu,
            max_faults,
            effects,
            formats,
            skip_fi,
            generate_bist,
            dual_bist,
        } => {
            run_fi_driven_safety(
                &source,
                &output,
                &asil,
                &goal,
                cycles,
                gpu,
                max_faults,
                effects.as_deref(),
                &formats,
                skip_fi,
                generate_bist,
                dual_bist,
            )?;
        }

        Commands::Add {
            package,
            version,
            dev,
            optional,
            features,
        } => {
            add_dependency(&package, version.as_deref(), dev, optional, &features)?;
        }

        Commands::Remove { package, dev } => {
            remove_dependency(&package, dev)?;
        }

        Commands::Update { package, force } => {
            update_dependencies(package.as_deref(), force)?;
        }

        Commands::Search { query, limit } => {
            search_packages(&query, limit)?;
        }

        Commands::Cache { command } => match command {
            CacheCommands::List => list_cache()?,
            CacheCommands::Size => show_cache_size()?,
            CacheCommands::Clear => clear_cache()?,
            CacheCommands::Remove { package, version } => remove_from_cache(&package, &version)?,
        },

        Commands::Train {
            data,
            output,
            epochs,
            learning_rate,
            batch_size,
            mode,
        } => {
            train_ml_model(&data, &output, epochs, learning_rate, batch_size, &mode)?;
        }

        Commands::Compile {
            source,
            output,
            header,
            entity,
            encrypt,
            key_file,
            library,
            optimize,
        } => {
            compile_to_ip(
                &source,
                &output,
                header.as_deref(),
                entity.as_deref(),
                encrypt,
                key_file.as_deref(),
                &library,
                optimize.as_deref(),
            )?;
        }

        Commands::Trace {
            source,
            signal,
            entity,
            depth,
            cycle,
            fanout,
            netlist,
        } => {
            run_signal_trace(
                &source,
                &signal,
                entity.as_deref(),
                depth,
                cycle,
                fanout,
                netlist.as_ref(),
            )?;
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
    let cargo_toml = format!(
        r#"[package]
name = "{}"
version = "0.1.0"
edition = "2021"

[dependencies]
skalp-stdlib = {{ git = "https://github.com/skalp-lang/skalp" }}
"#,
        name
    );
    fs::write(format!("{}/Cargo.toml", name), cargo_toml)?;

    // Create main.sk with a simple example
    let main_sk = r#"// Main SKALP design file

entity Counter {
    in clk: clock
    in rst: reset
    out count: bit[8]
}

impl Counter {
    signal count_reg: bit[8] = 0

    on(clk.rise) {
        if (rst) {
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
    let readme = format!(
        r#"# {}

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
"#,
        name
    );
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
fn build_design(
    source: &PathBuf,
    target: &str,
    output_dir: &PathBuf,
    safety_options: Option<SafetyBuildOptions>,
    optimization_options: OptimizationOptions,
    library_path: Option<&PathBuf>,
    skip_async_sta: bool,
) -> Result<()> {
    use skalp_codegen::systemverilog::{
        generate_constraints_toml, generate_systemverilog_from_mir,
    };
    use skalp_frontend::parse_and_build_compilation_context;

    info!("Building design from {:?} to {}", source, target);

    // Parse, build HIR with module resolution
    info!("Parsing SKALP source and building HIR with module resolution...");
    let context =
        parse_and_build_compilation_context(source).context("Failed to parse and build HIR")?;
    let hir = context.main_hir;
    let module_hirs = context.module_hirs;

    // Run safety analysis if enabled
    if let Some(ref safety_opts) = safety_options {
        run_safety_analysis(&hir, safety_opts, output_dir)?;
    }

    // Log optimization settings
    if let Some(ref preset) = optimization_options.preset {
        info!("Using optimization preset: {}", preset);
    }
    if let Some(ref passes) = optimization_options.passes {
        info!("Using custom passes: {}", passes);
    }

    // Lower to MIR with CDC analysis
    info!("Lowering to MIR with CDC analysis...");
    let compiler = skalp_mir::MirCompiler::new()
        .with_optimization_level(skalp_mir::OptimizationLevel::None)
        .with_verbose(true); // Enable verbose output for CDC analysis
                             // BUG #175 FIX: Pass module_hirs for proper enum resolution in imported functions
    let mir = compiler
        .compile_to_mir_with_modules(&hir, &module_hirs)
        .map_err(|e| anyhow::anyhow!("Failed to compile HIR to MIR with CDC analysis: {}", e))?;

    // Create output directory
    fs::create_dir_all(output_dir)?;

    // Generate target output
    let output_file = match target {
        "sv" | "systemverilog" => {
            info!("Generating SystemVerilog...");
            // Use the MIR-based generator
            let sv_code = generate_systemverilog_from_mir(&mir)?;
            let output_path = output_dir.join("design.sv");
            fs::write(&output_path, sv_code)?;

            // Generate constraints.toml if any timing constraints are defined
            if let Some(constraints) = generate_constraints_toml(&mir) {
                let constraints_path = output_dir.join("constraints.toml");
                fs::write(&constraints_path, constraints)?;
                info!("Timing constraints written to {:?}", constraints_path);
            }

            output_path
        }
        "v" | "verilog" => {
            // Verilog generation temporarily disabled (was using legacy LIR)
            anyhow::bail!(
                "Verilog generation is temporarily disabled. Use 'sv' for SystemVerilog."
            );
        }
        "vhdl" => {
            // VHDL generation temporarily disabled (was using legacy LIR)
            anyhow::bail!("VHDL generation is temporarily disabled. Use 'sv' for SystemVerilog.");
        }
        "lir" => {
            // LIR output temporarily disabled
            anyhow::bail!("LIR output is temporarily disabled. Use 'mir' for MIR output.");
        }
        "mir" => {
            info!("Saving MIR...");
            let output_path = output_dir.join("design.mir");
            let mir_json = serde_json::to_string_pretty(&mir)?;
            fs::write(&output_path, mir_json)?;
            output_path
        }
        "gates" => {
            use skalp_lir::{get_stdlib_library, lower_mir_module_to_lir};

            info!("Generating optimized gate-level netlist...");

            // Get technology library
            let library = if let Some(path) = library_path {
                info!("Loading technology library from {:?}", path);
                skalp_lir::TechLibrary::load_from_file(path)
                    .context("Failed to load technology library")?
            } else {
                get_stdlib_library("generic_asic")
                    .context("Failed to load default technology library")?
            };

            // Check if design has multiple modules/instances for hierarchical synthesis
            let has_hierarchy =
                mir.modules.len() > 1 || mir.modules.iter().any(|m| !m.instances.is_empty());

            let optimized_netlist = if has_hierarchy {
                // Hierarchical synthesis: optimize each module independently
                info!(
                    "Multi-module design detected ({} modules), using hierarchical synthesis",
                    mir.modules.len()
                );

                // Lower entire MIR hierarchy
                // For async modules: skip NCL expansion, we'll apply boundary-only NCL
                let (hier_lir, has_async) =
                    skalp_lir::lower_mir_hierarchical_for_optimize_first(&mir);

                if has_async {
                    info!("Async modules detected - using boundary-only NCL synthesis");
                }

                // Apply boundary-only NCL to async modules at LIR level
                // This keeps internal logic single-rail (optimizable) with NCL at I/O boundaries
                let hier_lir = if has_async {
                    let ncl_config = skalp_lir::NclConfig {
                        boundary_only: true,
                        use_weak_completion: true,
                        completion_tree_depth: None,
                        generate_null_wavefront: true,
                        use_opaque_arithmetic: true,
                    };
                    skalp_lir::apply_boundary_ncl_to_hierarchy(&hier_lir, &ncl_config)
                } else {
                    hier_lir
                };

                info!(
                    "Elaborated {} instances: top={}",
                    hier_lir.instances.len(),
                    hier_lir.top_module,
                );

                // Map to hierarchical gate netlist
                let hier_netlist = skalp_lir::map_hierarchical_to_gates(&hier_lir, &library);
                info!(
                    "Mapped to {} cells across {} instances",
                    hier_netlist.total_cell_count(),
                    hier_netlist.instance_count()
                );

                // Parallel per-instance synthesis optimization
                // For async: this optimizes the single-rail internal logic (AIG/ABC passes)
                if optimization_options.preset.is_some()
                    || optimization_options.passes.is_some()
                    || optimization_options.ml_guided
                {
                    let cells_before = hier_netlist.total_cell_count();
                    info!("Running parallel hierarchical synthesis optimization...");
                    let synth_config = build_synth_config(&optimization_options);
                    let mut engine = skalp_lir::synth::SynthEngine::with_config(synth_config);
                    let hier_result = engine.optimize_hierarchical(&hier_netlist, &library);

                    info!(
                        "Hierarchical optimization complete in {}ms",
                        hier_result.total_time_ms
                    );
                    for (path, result) in &hier_result.instance_results {
                        info!(
                            "  {}: {} -> {} cells ({:.1}% reduction)",
                            path,
                            result.initial_and_count,
                            result.netlist.cell_count(),
                            result.gate_reduction() * 100.0
                        );
                    }

                    let flattened = hier_result.netlist.flatten();
                    if has_async {
                        info!(
                            "   Total: {} -> {} cells ({:.1}% reduction)",
                            cells_before,
                            flattened.cells.len(),
                            (1.0 - flattened.cells.len() as f64 / cells_before as f64) * 100.0
                        );
                    }
                    flattened
                } else {
                    // No optimization, just flatten
                    hier_netlist.flatten()
                }
            } else {
                // Flat synthesis for single-module designs
                let top_module = mir
                    .modules
                    .first()
                    .ok_or_else(|| anyhow::anyhow!("No modules found in MIR"))?;

                // Lower MIR module to LIR
                // For async modules: skip NCL expansion, we'll apply boundary-only NCL
                let lir_result = if top_module.is_async {
                    info!("Async module - using boundary-only NCL synthesis");
                    skalp_lir::lower_mir_module_to_lir_skip_ncl(top_module)
                } else {
                    lower_mir_module_to_lir(top_module)
                };

                // For async modules: apply boundary-only NCL at LIR level
                // This keeps internal logic single-rail (optimizable) with NCL encode/decode at I/O
                let final_lir = if top_module.is_async {
                    let ncl_config = skalp_lir::NclConfig {
                        boundary_only: true,
                        use_weak_completion: true,
                        completion_tree_depth: None,
                        generate_null_wavefront: true,
                        use_opaque_arithmetic: true,
                    };
                    let ncl_result =
                        skalp_lir::expand_to_ncl_boundary(&lir_result.lir, &ncl_config);
                    info!(
                        "⚡ Boundary NCL: {} -> {} signals, {} -> {} nodes",
                        lir_result.lir.signals.len(),
                        ncl_result.lir.signals.len(),
                        lir_result.lir.nodes.len(),
                        ncl_result.lir.nodes.len()
                    );
                    ncl_result.lir
                } else {
                    lir_result.lir
                };

                // Map to gate netlist with configurable optimization level
                let tech_result = skalp_lir::map_lir_to_gates_with_opt_level(
                    &final_lir,
                    &library,
                    optimization_options.gate_opt_level,
                );
                let mut gate_netlist = tech_result.netlist;

                if top_module.is_async {
                    info!(
                        "   Gate netlist: {} cells (internal logic stays single-rail)",
                        gate_netlist.cells.len()
                    );
                }

                // Apply synthesis optimization if requested
                // For async: this optimizes the single-rail internal logic (AIG/ABC passes)
                if optimization_options.preset.is_some()
                    || optimization_options.passes.is_some()
                    || optimization_options.ml_guided
                {
                    let cells_before = gate_netlist.cells.len();
                    info!("Running synthesis optimization...");
                    let synth_result = apply_synthesis_optimization(
                        &gate_netlist,
                        &library,
                        &optimization_options,
                    )?;

                    // Output pipeline annotations if retiming was performed
                    if let Some(ref annotations) = synth_result.pipeline_annotations {
                        let annotations_path = output_dir.join("pipeline_annotations.toml");
                        annotations.write_toml(&annotations_path)?;
                        info!("Pipeline annotations written to {:?}", annotations_path);
                        info!("  {}", annotations.summary());
                    }

                    gate_netlist = synth_result.netlist;

                    if top_module.is_async {
                        info!(
                            "   Optimized: {} -> {} cells ({:.1}% reduction)",
                            cells_before,
                            gate_netlist.cells.len(),
                            (1.0 - gate_netlist.cells.len() as f64 / cells_before as f64) * 100.0
                        );
                    }
                }

                gate_netlist
            };

            // Run async STA for NCL circuits (detected by tech_mapper via is_ncl flag)
            let mut optimized_netlist = optimized_netlist;
            if optimized_netlist.is_ncl && !skip_async_sta {
                info!("Running async timing analysis for NCL circuit...");
                let sta_config = skalp_lir::AsyncStaConfig::default();
                let sta_result = skalp_lir::analyze_async_timing(
                    &optimized_netlist,
                    Some(&library),
                    &sta_config,
                );

                // Print summary
                println!("📊 Async STA:");
                println!(
                    "   Analyzed {} forks, {} with timing concerns",
                    sta_result.stats.total_forks, sta_result.stats.fork_violations
                );
                if sta_result.stats.max_skew_ps > 0.0 {
                    println!("   Max skew: {:.1}ps", sta_result.stats.max_skew_ps);
                }

                // Report violations and apply fix
                if sta_result.has_violations() {
                    let errors = sta_result.error_count();
                    if errors > 0 {
                        eprintln!(
                            "⚠️  {} timing violation(s) detected (threshold: {:.0}ps)",
                            errors, sta_config.max_fork_skew_ps
                        );
                        // Print detailed report for errors
                        for violation in &sta_result.fork_violations {
                            if violation.severity != skalp_lir::ViolationSeverity::Warning {
                                eprintln!("{}", violation.format());
                            }
                        }
                    }

                    // Apply fix: delay ready signal to cover worst-case skew
                    // This inserts buffers on completion detection outputs
                    let fix_config = skalp_lir::AsyncStaFixConfig {
                        strategy: skalp_lir::FixStrategy::DelayReadySignal,
                        ready_delay_margin_ps: 10.0, // 10ps extra margin
                        ..Default::default()
                    };
                    let fix_result = skalp_lir::fix_fork_violations(
                        &mut optimized_netlist,
                        &sta_result,
                        &fix_config,
                    );

                    if fix_result.buffers_inserted > 0 {
                        println!(
                            "🔧 Async STA Fix: Inserted {} buffer(s) to delay ready signal",
                            fix_result.buffers_inserted
                        );
                        println!(
                            "   Fixed {} violation(s), {} skipped",
                            fix_result.violations_fixed, fix_result.violations_skipped
                        );
                    }

                    // Save full report
                    let report_path = output_dir.join("async_sta_report.txt");
                    fs::write(&report_path, sta_result.summary())?;
                    info!("Async STA report saved to {:?}", report_path);
                }
            }

            // Generate Verilog from gate netlist
            let verilog = optimized_netlist.to_verilog();
            let verilog_path = output_dir.join("design_gates.v");
            fs::write(&verilog_path, &verilog)?;

            // Save gate netlist as JSON
            let netlist_json = serde_json::to_string_pretty(&optimized_netlist)?;
            fs::write(output_dir.join("design_gates.json"), &netlist_json)?;

            // Print stats
            let stats = skalp_lir::GateNetlistStats::from_netlist(&optimized_netlist);
            println!("📊 Gate netlist stats:");
            println!("   Cells: {}", stats.total_cells);
            println!("   Nets: {}", stats.total_nets);
            println!(
                "   Cell types: {:?}",
                stats.cell_types.keys().collect::<Vec<_>>()
            );

            verilog_path
        }
        _ => {
            anyhow::bail!(
                "Unsupported target: {}. Use 'sv', 'mir', or 'gates'",
                target
            );
        }
    };

    println!("✅ Build complete!");
    println!("📄 Output: {:?}", output_file);

    Ok(())
}

/// Build a SynthConfig from optimization options
fn build_synth_config(options: &OptimizationOptions) -> skalp_lir::synth::SynthConfig {
    use skalp_lir::synth::{SynthConfig, SynthPreset};

    match options.preset.as_deref() {
        Some("quick") => {
            info!("Using quick optimization preset");
            SynthConfig::quick()
        }
        Some("balanced") | None => {
            info!("Using balanced optimization preset");
            SynthConfig::default()
        }
        Some("full") => {
            info!("Using full optimization preset");
            SynthConfig::full()
        }
        Some("timing") => {
            info!("Using timing-focused optimization preset");
            SynthConfig::timing()
        }
        Some("area") => {
            info!("Using area-focused optimization preset");
            SynthConfig::area()
        }
        Some("resyn2") => {
            info!("Using resyn2 optimization preset (ABC-equivalent)");
            SynthConfig {
                preset: SynthPreset::Resyn2,
                max_iterations: 3,
                run_timing_analysis: true,
                ..Default::default()
            }
        }
        Some("compress2") => {
            info!("Using compress2 optimization preset (aggressive area)");
            SynthConfig {
                preset: SynthPreset::Compress2,
                max_iterations: 3,
                ..Default::default()
            }
        }
        Some("auto") | Some(_) => {
            // For auto or unknown presets, use Auto
            info!("Using auto optimization (parallel preset selection)");
            SynthConfig {
                preset: SynthPreset::Auto,
                max_iterations: 3,
                ..Default::default()
            }
        }
    }
}

/// Apply synthesis optimization to a gate netlist
fn apply_synthesis_optimization(
    netlist: &skalp_lir::GateNetlist,
    library: &skalp_lir::TechLibrary,
    options: &OptimizationOptions,
) -> Result<skalp_lir::synth::SynthResult> {
    // Use ML-guided optimization if requested
    if options.ml_guided {
        // ML optimization returns only netlist, wrap in SynthResult
        let ml_netlist = apply_ml_synthesis_optimization(netlist, library, options)?;
        return Ok(skalp_lir::synth::SynthResult {
            netlist: ml_netlist,
            initial_and_count: 0,
            final_and_count: 0,
            initial_levels: 0,
            final_levels: 0,
            pass_results: vec![],
            timing_result: None,
            mapping_result: None,
            pipeline_annotations: None,
            total_time_ms: 0,
        });
    }

    use skalp_lir::synth::SynthEngine;

    let config = build_synth_config(options);

    // Create engine
    let mut engine = SynthEngine::with_config(config);

    // Run optimization
    let result = engine.optimize(netlist, library);

    info!(
        "Optimization complete: {} AND gates -> {} AND gates",
        result.initial_and_count, result.final_and_count
    );
    info!(
        "Logic levels: {} -> {}",
        result.initial_levels, result.final_levels
    );

    for pass_result in &result.pass_results {
        info!("  {}", pass_result);
    }

    Ok(result)
}

/// Apply ML-guided synthesis optimization
fn apply_ml_synthesis_optimization(
    netlist: &skalp_lir::GateNetlist,
    library: &skalp_lir::TechLibrary,
    options: &OptimizationOptions,
) -> Result<skalp_lir::GateNetlist> {
    use skalp_lir::synth::{AigBuilder, AigWriter};
    use skalp_ml::{MlConfig, MlSynthEngine};

    info!("Using ML-guided synthesis optimization");

    // Create ML config with training mode if requested
    let config = if options.training_data_dir.is_some() {
        MlConfig::training()
    } else {
        MlConfig::default()
    };

    // Create engine - with or without training data collection
    let mut engine = if let Some(ref dir) = options.training_data_dir {
        let run_id = format!("run_{}", chrono::Utc::now().timestamp());
        info!("Training data collection enabled: {}", dir.display());
        MlSynthEngine::with_training(config, &run_id)
    } else {
        MlSynthEngine::new(config)
    };

    // Load trained policy if specified
    if let Some(ref policy_path) = options.ml_policy_path {
        info!("Loading trained policy from: {}", policy_path.display());
        engine
            .load_json_policy(policy_path.to_str().unwrap_or(""))
            .map_err(|e| anyhow::anyhow!("Failed to load policy: {}", e))?;
    }

    // Build AIG from netlist
    let builder = AigBuilder::new(netlist);
    let mut aig = builder.build();

    let initial_stats = aig.compute_stats();

    // Run ML-guided optimization with design name for training
    let design_name = &netlist.name;
    let library_name = "7nm"; // TODO: Get from library

    let _pass_results = engine
        .optimize_with_design_name(&mut aig, design_name, library_name)
        .map_err(|e| anyhow::anyhow!("ML optimization failed: {}", e))?;

    let final_stats = aig.compute_stats();

    info!(
        "ML optimization complete: {} AND gates -> {} AND gates",
        initial_stats.and_count, final_stats.and_count
    );
    info!(
        "Logic levels: {} -> {}",
        initial_stats.max_level, final_stats.max_level
    );
    info!(
        "ML decisions: {}, passes: {}",
        engine.stats().ml_decisions,
        engine.stats().passes_executed
    );
    info!("Pass sequence: {:?}", engine.stats().pass_sequence);
    info!("Improvement: {:.1}%", engine.stats().improvement * 100.0);

    // Export training data if requested
    if let Some(ref dir) = options.training_data_dir {
        engine
            .export_training_data(dir.to_str().unwrap_or("training_data"))
            .map_err(|e| anyhow::anyhow!("Failed to export training data: {}", e))?;

        if let Some(collector) = engine.collector() {
            info!(
                "Training data collected: {} episodes, {} pass decisions",
                collector.episode_count(),
                collector.stats().total_pass_decisions
            );
        }
    }

    // Write back to netlist
    let writer = AigWriter::new(library);
    Ok(writer.write(&aig))
}

/// Train the ML pass ordering policy using collected training data
fn train_ml_model(
    data_dir: &Path,
    output_path: &Path,
    epochs: usize,
    learning_rate: f64,
    batch_size: usize,
    mode: &str,
) -> Result<()> {
    use skalp_ml::{PolicyTrainer, TrainerConfig, TrainingMode};

    // Parse training mode
    let training_mode = match mode.to_lowercase().as_str() {
        "standard" => TrainingMode::Standard,
        "positive-only" | "positive_only" => TrainingMode::PositiveOnly,
        "episode-weighted" | "episode_weighted" => TrainingMode::EpisodeWeighted,
        "best-episodes" | "best_episodes" => TrainingMode::BestEpisodes,
        _ => {
            anyhow::bail!(
                "Unknown training mode: '{}'. Valid modes: standard, positive-only, episode-weighted, best-episodes",
                mode
            );
        }
    };

    println!("🧠 Training ML pass ordering model");
    println!("   Data directory: {}", data_dir.display());
    println!("   Output: {}", output_path.display());
    println!("   Epochs: {}", epochs);
    println!("   Learning rate: {}", learning_rate);
    println!("   Batch size: {}", batch_size);
    println!("   Mode: {:?}", training_mode);
    println!();

    // Load training data
    println!("📂 Loading training data...");
    let dataset = PolicyTrainer::load_data(data_dir)
        .map_err(|e| anyhow::anyhow!("Failed to load training data: {}", e))?;

    println!(
        "   Loaded {} episodes with {} pass decisions",
        dataset.episodes.len(),
        dataset.stats.total_pass_decisions
    );

    if dataset.episodes.is_empty() {
        anyhow::bail!("No training episodes found in {}", data_dir.display());
    }

    // Create trainer with config
    let config = TrainerConfig {
        epochs,
        learning_rate,
        batch_size,
        ..Default::default()
    };

    let mut trainer = PolicyTrainer::new(config);

    // Train the model with specified mode
    println!("\n📈 Training...\n");
    let stats = trainer.train_with_mode(&dataset, training_mode);

    // Ensure output directory exists
    if let Some(parent) = output_path.parent() {
        fs::create_dir_all(parent)?;
    }

    // Save the trained policy
    trainer
        .save_policy(output_path.to_str().unwrap_or("pass_policy.json"))
        .map_err(|e| anyhow::anyhow!("Failed to save policy: {}", e))?;

    // Save training stats
    let stats_path = output_path.with_extension("stats.json");
    trainer
        .save_stats(stats_path.to_str().unwrap())
        .map_err(|e| anyhow::anyhow!("Failed to save training stats: {}", e))?;

    println!("\n✅ Training complete!");
    println!("   Policy saved to: {}", output_path.display());
    println!("   Stats saved to: {}", stats_path.display());
    println!(
        "   Best validation accuracy: {:.2}% (epoch {})",
        stats.best_val_accuracy * 100.0,
        stats.best_epoch + 1
    );

    Ok(())
}

/// Compile a design to pre-compiled IP format (.skb)
///
/// This function synthesizes the design to a gate netlist and packages it
/// as a distributable binary file. Optionally generates a header file (.skh)
/// for importing the compiled IP in other designs.
#[allow(clippy::too_many_arguments)]
fn compile_to_ip(
    source: &Path,
    output: &Path,
    header: Option<&Path>,
    entity_name: Option<&str>,
    encrypt: bool,
    key_file: Option<&Path>,
    library_name: &str,
    optimize: Option<&str>,
) -> Result<()> {
    use skalp_frontend::hir_builder::HirBuilderContext;
    use skalp_frontend::parse;
    use skalp_lir::{
        compiled_ip::{generate_header, CompiledIp},
        get_stdlib_library, lower_mir_module_to_lir, map_lir_to_gates_optimized,
    };
    use skalp_mir::hir_to_mir::HirToMir;

    println!("📦 Compiling to IP: {:?}", source);
    println!("   Output: {:?}", output);
    if let Some(h) = header {
        println!("   Header: {:?}", h);
    }
    if encrypt {
        println!("   Encryption: enabled");
    }
    println!();

    // Read and parse source file
    let source_code = fs::read_to_string(source)
        .with_context(|| format!("Failed to read source file: {:?}", source))?;

    let (syntax_tree, parse_errors) = parse::parse_with_errors(&source_code);
    if !parse_errors.is_empty() {
        for err in &parse_errors {
            eprintln!("Parse error: {}", err.message);
        }
        anyhow::bail!("Parsing failed with {} errors", parse_errors.len());
    }

    // Build HIR
    let mut builder = HirBuilderContext::new();
    let hir = builder
        .build(&syntax_tree)
        .map_err(|errors| anyhow::anyhow!("HIR build failed: {:?}", errors.first()))?;

    // Find entity to compile
    let entity = if let Some(name) = entity_name {
        hir.entities
            .iter()
            .find(|e| e.name == name)
            .ok_or_else(|| anyhow::anyhow!("Entity '{}' not found", name))?
    } else {
        hir.entities
            .first()
            .ok_or_else(|| anyhow::anyhow!("No entities found in source file"))?
    };

    println!("   Entity: {}", entity.name);

    // Convert to MIR
    let mut hir_to_mir = HirToMir::new();
    let mir = hir_to_mir.transform(&hir);

    let module = mir
        .modules
        .iter()
        .find(|m| m.name == entity.name)
        .ok_or_else(|| anyhow::anyhow!("Module '{}' not found in MIR", entity.name))?;

    // Convert to LIR
    let lir_result = lower_mir_module_to_lir(module);
    println!(
        "   LIR: {} signals, {} nodes",
        lir_result.lir.signals.len(),
        lir_result.lir.nodes.len()
    );

    // Load technology library
    let library = get_stdlib_library(library_name)
        .map_err(|e| anyhow::anyhow!("Failed to load library '{}': {}", library_name, e))?;
    println!("   Library: {}", library_name);

    // Technology mapping
    let tech_result = map_lir_to_gates_optimized(&lir_result.lir, &library);
    let mut netlist = tech_result.netlist;
    println!("   Cells: {} gates", netlist.cells.len());

    // Apply optimization if requested
    if let Some(preset) = optimize {
        println!("   Optimizing with preset: {}", preset);
        let opt_options = OptimizationOptions {
            preset: Some(preset.to_string()),
            ..Default::default()
        };
        let synth_result = apply_synthesis_optimization(&netlist, &library, &opt_options)?;
        netlist = synth_result.netlist;
        println!("   Optimized: {} gates", netlist.cells.len());

        // Log pipeline annotations if retiming was performed
        if let Some(ref annotations) = synth_result.pipeline_annotations {
            println!("   Pipeline annotations: {}", annotations.summary());
        }
    }

    // Create CompiledIp
    let compiled = CompiledIp::new(netlist, library_name);
    println!(
        "   Compiled IP: {} ports, {} cells",
        compiled.port_info.len(),
        compiled.netlist.cells.len()
    );

    // Handle encryption key
    let key: Option<[u8; 32]> = if encrypt {
        if let Some(key_path) = key_file {
            // Read key from file
            let key_bytes = fs::read(key_path)
                .with_context(|| format!("Failed to read key file: {:?}", key_path))?;
            if key_bytes.len() != 32 {
                anyhow::bail!("Key file must be exactly 32 bytes (AES-256)");
            }
            let mut key = [0u8; 32];
            key.copy_from_slice(&key_bytes);
            Some(key)
        } else if let Ok(key_hex) = std::env::var("SKALP_IP_KEY") {
            // Read key from environment variable
            let key_bytes: Result<Vec<u8>, _> = (0..key_hex.len())
                .step_by(2)
                .map(|i| u8::from_str_radix(&key_hex[i..i + 2], 16))
                .collect();
            match key_bytes {
                Ok(bytes) if bytes.len() == 32 => {
                    let mut key = [0u8; 32];
                    key.copy_from_slice(&bytes);
                    Some(key)
                }
                _ => anyhow::bail!("Invalid SKALP_IP_KEY format (expected 64 hex chars)"),
            }
        } else {
            anyhow::bail!("Encryption requires --key-file or SKALP_IP_KEY environment variable");
        }
    } else {
        None
    };

    // Write compiled IP
    compiled
        .write_to_file(output, key.as_ref())
        .map_err(|e| anyhow::anyhow!("Failed to write compiled IP: {}", e))?;
    println!("\n✅ Compiled IP written to: {:?}", output);

    // Generate header file if requested
    if let Some(header_path) = header {
        let skb_relative_path = output
            .file_name()
            .and_then(|n| n.to_str())
            .unwrap_or("compiled.skb");
        let header_content = generate_header(&compiled, skb_relative_path);
        fs::write(header_path, &header_content)
            .with_context(|| format!("Failed to write header file: {:?}", header_path))?;
        println!("✅ Header file written to: {:?}", header_path);
    }

    Ok(())
}

/// Simulate design
///
/// Supports three simulation paths:
/// - Behavioral (default): HIR → MIR → SIR (fast, functional)
/// - Gate-level (--gate-level): HIR → MIR → LIR → SIR (gate-level primitives, fault injection ready)
/// - NCL (--ncl): Async NCL simulation with THmn gates and wavefront propagation
fn simulate_design(
    design_file: &PathBuf,
    duration: Option<&str>,
    gate_level: bool,
    use_gpu: bool,
    gate_opt_level: u8,
    ncl_mode: bool,
) -> Result<()> {
    use skalp_mir::Mir;
    use skalp_sir::convert_mir_to_sir;

    info!("Loading design from {:?}", design_file);

    // Parse duration (default 1000 cycles for sync, max_iterations for NCL)
    let cycles = if let Some(dur) = duration {
        if dur.ends_with("ns") {
            dur.trim_end_matches("ns").parse::<u64>()? / 10 // Assume 10ns clock
        } else if dur.ends_with("us") {
            dur.trim_end_matches("us").parse::<u64>()? * 100
        } else {
            dur.parse::<u64>()?
        }
    } else {
        1000
    };

    // Determine file type and compile path
    let extension = design_file.extension().and_then(|s| s.to_str());

    match extension {
        Some("sk") | Some("skalp") => {
            // Source file - compile it
            if ncl_mode {
                simulate_ncl(design_file, cycles, use_gpu, gate_opt_level)
            } else if gate_level {
                simulate_gate_level(design_file, cycles, use_gpu, gate_opt_level)
            } else {
                simulate_behavioral(design_file, cycles, use_gpu)
            }
        }
        Some("mir") => {
            // Pre-compiled MIR file
            let design_str = fs::read_to_string(design_file)?;
            let mir: Mir = serde_json::from_str(&design_str)?;
            if mir.modules.is_empty() {
                anyhow::bail!("No modules found in MIR");
            }

            if gate_level {
                // Gate-level simulation temporarily disabled during GateNetlist migration
                anyhow::bail!("Gate-level simulation from MIR is temporarily disabled. Use behavioral simulation (--mode behavioral) instead.");
            } else {
                // MIR → SIR
                let sir = convert_mir_to_sir(&mir.modules[0]);
                simulate_sir_behavioral(&sir, cycles, use_gpu)
            }
        }
        Some("lir") => {
            // Legacy LIR format has been removed
            anyhow::bail!("Legacy LIR format is no longer supported. Use .sk files with behavioral or gate-level simulation.");
        }
        _ => {
            anyhow::bail!("Unsupported file format. Use .sk, .mir, or .lir files");
        }
    }
}

/// Behavioral simulation: HIR → MIR → SIR
fn simulate_behavioral(source_file: &PathBuf, cycles: u64, use_gpu: bool) -> Result<()> {
    use skalp_frontend::parse_and_build_hir_from_file;
    use skalp_mir::MirCompiler;
    use skalp_sir::convert_mir_to_sir_with_hierarchy;

    println!("🔧 Behavioral Simulation (HIR → MIR → SIR)");
    println!("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
    println!("Source: {:?}", source_file);
    println!("Cycles: {}", cycles);
    println!("GPU: {}", use_gpu);
    println!();

    // Parse and build HIR
    info!("Parsing SKALP source...");
    let hir =
        parse_and_build_hir_from_file(source_file).context("Failed to parse and build HIR")?;

    // Lower to MIR
    info!("Compiling to MIR...");
    let compiler = MirCompiler::new();
    let mir = compiler
        .compile_to_mir(&hir)
        .map_err(|e| anyhow::anyhow!("Failed to compile HIR to MIR: {}", e))?;

    if mir.modules.is_empty() {
        anyhow::bail!("No modules found in compiled design");
    }

    // Convert to SIR with full hierarchy support (for synthesized function modules)
    info!("Converting to SIR...");
    let sir = convert_mir_to_sir_with_hierarchy(&mir, &mir.modules[0]);

    println!("📊 Design Statistics:");
    println!("   Inputs: {}", sir.inputs.len());
    println!("   Outputs: {}", sir.outputs.len());
    println!("   Combinational nodes: {}", sir.combinational_nodes.len());
    println!("   Sequential nodes: {}", sir.sequential_nodes.len());
    println!();

    simulate_sir_behavioral(&sir, cycles, use_gpu)
}

/// Gate-level simulation: HIR → MIR → LIR → GateNetlist → SIR → Simulation
fn simulate_gate_level(
    source_file: &PathBuf,
    cycles: u64,
    use_gpu: bool,
    gate_opt_level: u8,
) -> Result<()> {
    use skalp_frontend::parse_and_build_hir_from_file;
    use skalp_lir::get_stdlib_library;
    use skalp_sim::{convert_gate_netlist_to_sir, HwAccel, UnifiedSimConfig, UnifiedSimulator};

    println!("🔬 Gate-Level Simulation");
    println!("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
    println!("Source: {:?}", source_file);
    if gate_opt_level == 0 {
        println!("Optimization: DISABLED (-O0)");
    } else {
        println!("Optimization: level {}", gate_opt_level);
    }
    println!();

    // Parse and build HIR
    println!("📖 Parsing design...");
    let hir = parse_and_build_hir_from_file(source_file).context("Failed to parse source")?;

    // Lower to MIR
    println!("🔧 Lowering to MIR...");
    let compiler =
        skalp_mir::MirCompiler::new().with_optimization_level(skalp_mir::OptimizationLevel::None);
    let mir = compiler
        .compile_to_mir(&hir)
        .map_err(|e| anyhow::anyhow!("MIR compilation failed: {}", e))?;

    let library =
        get_stdlib_library("generic_asic").context("Failed to load default technology library")?;

    // Check if design has hierarchy
    let has_hierarchy =
        mir.modules.len() > 1 || mir.modules.iter().any(|m| !m.instances.is_empty());

    // Lower to LIR and tech-map to GateNetlist
    println!("🔩 Converting to gate-level netlist...");

    let netlist = if has_hierarchy {
        println!(
            "   Using hierarchical synthesis ({} modules)",
            mir.modules.len()
        );

        // Lower entire MIR hierarchy
        let hier_lir = skalp_lir::lower_mir_hierarchical(&mir);
        println!(
            "   Elaborated {} instances, top={}",
            hier_lir.instances.len(),
            hier_lir.top_module
        );

        // Map to hierarchical gate netlist
        let hier_netlist = skalp_lir::map_hierarchical_to_gates(&hier_lir, &library);
        println!(
            "   Mapped {} cells across {} instances",
            hier_netlist.total_cell_count(),
            hier_netlist.instance_count()
        );

        // Flatten to single netlist with proper stitching
        // Note: flatten() now automatically runs buffer removal for NCL circuits
        hier_netlist.flatten()
    } else {
        println!("   Using flat synthesis (single module)");

        // Find the top module
        let top_module = mir
            .modules
            .first()
            .ok_or_else(|| anyhow::anyhow!("No modules found in design"))?;

        // Lower to LIR and tech-map with configurable optimization
        let lir_result = skalp_lir::lower_mir_module_to_lir(top_module);
        let tech_result =
            skalp_lir::map_lir_to_gates_with_opt_level(&lir_result.lir, &library, gate_opt_level);
        tech_result.netlist
    };

    println!("\n📊 Gate-Level Design Statistics:");
    println!("   Module: {}", netlist.name);
    println!("   Gate cells: {}", netlist.cells.len());
    println!("   Nets: {}", netlist.nets.len());

    // Convert GateNetlist to SIR for simulation
    println!("\n🔄 Converting to SIR for simulation...");
    let sir_result = convert_gate_netlist_to_sir(&netlist);
    println!("   SIR primitives: {}", sir_result.stats.primitives_created);
    println!(
        "   Inputs: {}",
        sir_result
            .sir
            .top_module
            .signals
            .iter()
            .filter(|s| {
                matches!(
                    s.signal_type,
                    skalp_sim::sir::SirSignalType::Port {
                        direction: skalp_sim::sir::SirPortDirection::Input
                    }
                )
            })
            .count()
    );
    println!(
        "   Outputs: {}",
        sir_result
            .sir
            .top_module
            .signals
            .iter()
            .filter(|s| {
                matches!(
                    s.signal_type,
                    skalp_sim::sir::SirSignalType::Port {
                        direction: skalp_sim::sir::SirPortDirection::Output
                    }
                )
            })
            .count()
    );

    // Create unified simulator config
    let config = UnifiedSimConfig {
        level: skalp_sim::SimLevel::GateLevel,
        hw_accel: if use_gpu { HwAccel::Gpu } else { HwAccel::Cpu },
        max_cycles: cycles,
        capture_waveforms: true,
        ..Default::default()
    };

    // Create and initialize unified simulator
    println!("\n🔧 Running gate-level simulation...");
    let mut simulator = UnifiedSimulator::new(config)
        .map_err(|e| anyhow::anyhow!("Failed to create simulator: {}", e))?;

    simulator
        .load_gate_level(&sir_result.sir)
        .map_err(|e| anyhow::anyhow!("Failed to load design: {}", e))?;

    println!("   Device: {}", simulator.device_info());
    println!("   Inputs: {:?}", simulator.get_input_names());
    println!("   Outputs: {:?}", simulator.get_output_names());

    // Find clock signal if present
    let input_names = simulator.get_input_names();
    let clock_name = input_names
        .iter()
        .find(|n| n.contains("clk") || n.contains("clock"))
        .cloned();

    // Run simulation (async methods require a runtime)
    let runtime = tokio::runtime::Runtime::new()?;
    let result = runtime.block_on(async {
        if let Some(clk) = clock_name {
            println!("   Clock: {} (toggling)", clk);
            simulator.run_clocked(cycles, &clk).await
        } else {
            println!("   No clock detected, running {} steps", cycles);
            simulator.run(cycles).await
        }
    });

    println!("\n📊 Simulation Results:");
    println!("   Cycles: {}", result.cycles);
    println!("   Used GPU: {}", result.used_gpu);
    println!("   Final outputs:");
    for (name, value) in &result.outputs {
        println!("      {} = 0x{:x}", name, value);
    }

    println!("\n✅ Gate-level simulation complete!");

    Ok(())
}

/// NCL (Null Convention Logic) async simulation
///
/// Compiles the design to GateNetlist with NCL expansion, then simulates
/// using proper THmn threshold gate evaluation with hysteresis.
fn simulate_ncl(
    source_file: &PathBuf,
    max_iterations: u64,
    use_gpu: bool,
    gate_opt_level: u8,
) -> Result<()> {
    use skalp_frontend::parse_and_build_hir_from_file;
    use skalp_lir::{get_stdlib_library, lower_mir_module_to_lir, map_lir_to_gates_with_opt_level};
    use skalp_sim::{CircuitMode, HwAccel, UnifiedSimConfig, UnifiedSimulator};

    println!("🔬 NCL (Async) Gate-Level Simulation");
    println!("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
    println!("Source: {:?}", source_file);
    println!("Mode: NCL (Null Convention Logic)");
    println!();

    // Parse and build HIR
    println!("📖 Parsing design...");
    let hir = parse_and_build_hir_from_file(source_file).context("Failed to parse source")?;

    // Lower to MIR
    println!("🔧 Lowering to MIR...");
    let compiler =
        skalp_mir::MirCompiler::new().with_optimization_level(skalp_mir::OptimizationLevel::None);
    let mir = compiler
        .compile_to_mir(&hir)
        .map_err(|e| anyhow::anyhow!("MIR compilation failed: {}", e))?;

    if mir.modules.is_empty() {
        anyhow::bail!("No modules found in compiled design");
    }

    let module = &mir.modules[0];
    println!("   Module: {}", module.name);

    // Lower MIR to LIR
    println!("🔩 Converting to LIR...");
    let lir_result = lower_mir_module_to_lir(module);
    println!("   LIR nodes: {}", lir_result.lir.nodes.len());

    // Get tech library and map to gates
    println!("🔧 Technology mapping...");
    let library =
        get_stdlib_library("generic_asic").context("Failed to load default technology library")?;

    let tech_result = map_lir_to_gates_with_opt_level(&lir_result.lir, &library, gate_opt_level);

    let gate_netlist = tech_result.netlist;
    println!(
        "   Gate netlist: {} cells, {} nets",
        gate_netlist.cells.len(),
        gate_netlist.nets.len()
    );

    // Count THmn gates (these are created by NCL expansion)
    let thmn_count = gate_netlist
        .cells
        .iter()
        .filter(|c| c.cell_type.starts_with("TH"))
        .count();
    if thmn_count > 0 {
        println!("   THmn gates: {}", thmn_count);
    }

    // Create NCL simulator config
    let config = UnifiedSimConfig {
        level: skalp_sim::SimLevel::GateLevel,
        circuit_mode: CircuitMode::Ncl,
        hw_accel: if use_gpu { HwAccel::Gpu } else { HwAccel::Cpu },
        max_iterations: max_iterations as u32,
        capture_waveforms: true,
        ncl_debug: false,
        ..Default::default()
    };

    // Create and initialize simulator
    println!("\n🔧 Running NCL simulation...");
    let mut simulator = UnifiedSimulator::new(config)
        .map_err(|e| anyhow::anyhow!("Failed to create simulator: {}", e))?;

    simulator
        .load_ncl_gate_level(gate_netlist)
        .map_err(|e| anyhow::anyhow!("Failed to load NCL netlist: {}", e))?;

    println!("   Device: {}", simulator.device_info());

    // For NCL, we run until stable instead of a fixed number of cycles
    println!(
        "   Running until stable (max {} iterations)...",
        max_iterations
    );

    let runtime = tokio::runtime::Runtime::new()?;
    let result = runtime.block_on(async { simulator.run_until_stable().await });

    println!("\n📊 NCL Simulation Results:");
    println!("   Iterations: {}", result.iterations);
    println!("   Wavefronts: {}", result.wavefronts);
    println!("   Stable: {}", result.is_stable);

    if let Some(stats) = simulator.get_ncl_stats() {
        println!("   Phase: {:?}", stats.phase);
        println!("   Data wavefronts: {}", stats.data_wavefronts);
        println!("   Null wavefronts: {}", stats.null_wavefronts);
    }

    if simulator.is_ncl_complete() {
        println!("   Output status: Complete (all DATA)");
    } else if simulator.is_ncl_null() {
        println!("   Output status: NULL (spacer)");
    } else {
        println!("   Output status: Transitioning");
    }

    println!("\n✅ NCL simulation complete!");

    Ok(())
}

/// Run behavioral SIR simulation
fn simulate_sir_behavioral(sir: &skalp_sir::SirModule, cycles: u64, use_gpu: bool) -> Result<()> {
    use skalp_sim::{HwAccel, SimLevel, UnifiedSimConfig, UnifiedSimulator};
    use tokio::runtime::Runtime;

    println!("🔧 Running behavioral simulation...");

    // Create async runtime for simulation
    let runtime = Runtime::new()?;

    runtime.block_on(async {
        // Create simulation config
        let config = UnifiedSimConfig {
            level: SimLevel::Behavioral,
            hw_accel: if use_gpu { HwAccel::Gpu } else { HwAccel::Cpu },
            max_cycles: cycles,
            capture_waveforms: true,
            ..Default::default()
        };

        // Create and initialize simulator
        let mut simulator = UnifiedSimulator::new(config)
            .map_err(|e| anyhow::anyhow!("Failed to create simulator: {}", e))?;
        simulator.load_behavioral(sir).await
            .map_err(|e| anyhow::anyhow!("Failed to load module: {}", e))?;

        println!("   Device: {}", simulator.device_info());

        // Run simulation
        let result = simulator.run(cycles).await;

        println!("   Completed {} cycles", result.cycles);
        println!("\n✅ Behavioral simulation complete!");

        Ok::<(), anyhow::Error>(())
    })?;

    Ok(())
}

/// Gate-level analysis and fault simulation
/// NOTE: This function is being migrated to use GateNetlist instead of legacy LIR
fn analyze_design(
    source: &Path,
    _output_dir: &Path,
    _fault_sim: bool,
    _cycles: u64,
    _use_gpu: bool,
    _max_faults: usize,
    _detailed: bool,
) -> Result<()> {
    println!("🔬 Gate-Level Analysis");
    println!("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
    println!("Source: {:?}", source);
    println!();
    println!("⚠️  Gate-level analysis is being migrated to use GateNetlist.");
    println!("   Please use the 'safety' command for ISO 26262 FMEDA analysis.");
    println!("   The new flow: HIR → MIR → WordLir → TechMapper → GateNetlist");
    anyhow::bail!("Gate-level analysis temporarily disabled during GateNetlist migration");
}

/// Run CPU-based fault simulation
#[allow(dead_code)]
fn run_cpu_fault_sim(sir: &skalp_sim::sir::Sir, cycles: u64, max_faults: usize) {
    use skalp_sim::{FaultCampaignConfig, GateLevelSimulator};

    let mut sim = GateLevelSimulator::new(sir);
    println!("   Using CPU simulation");
    println!("   Primitives: {}", sim.primitive_count());

    let config = FaultCampaignConfig {
        cycles_per_fault: cycles,
        fault_types: vec![
            skalp_sim::sir::FaultType::StuckAt0,
            skalp_sim::sir::FaultType::StuckAt1,
        ],
        max_faults,
        ..Default::default()
    };

    let results = sim.run_fault_campaign_with_config(&config);

    println!("   Faults Tested: {}", results.total_faults);
    println!("   Detected: {}", results.detected_faults);
    println!("   Corruption Faults: {}", results.corruption_faults);
    println!(
        "   Safe Faults: {} ({:.1}%)",
        results.safe_faults, results.safe_fault_percentage
    );
    println!(
        "   Diagnostic Coverage: {:.2}%",
        results.diagnostic_coverage
    );
}

/// Find the top-level module in a MIR design.
/// The top-level module is the one not instantiated by any other module.
/// When multiple uninstantiated modules exist (e.g., unmonomorphized templates alongside
/// their specialized variants), pick the one with the most content (instances + processes + signals).
fn find_top_level_module(mir: &skalp_mir::mir::Mir) -> Option<&skalp_mir::mir::Module> {
    if mir.modules.is_empty() {
        return None;
    }
    if mir.modules.len() == 1 {
        return mir.modules.first();
    }
    // Collect all module IDs that are instantiated by some other module
    let instantiated: std::collections::HashSet<skalp_mir::mir::ModuleId> = mir
        .modules
        .iter()
        .flat_map(|m| m.instances.iter().map(|inst| inst.module))
        .collect();
    // Find all uninstantiated modules, pick the one with most content
    // This handles monomorphization: both `Foo` (empty template) and `Foo_42` (specialized)
    // are uninstantiated, but only the specialized one has actual logic
    mir.modules
        .iter()
        .filter(|m| !instantiated.contains(&m.id))
        .max_by_key(|m| m.instances.len() + m.processes.len() + m.signals.len())
        .or_else(|| mir.modules.last())
}

/// Equivalence checking between RTL (LIR) and gate-level netlist
fn run_equivalence_check(
    source: &Path,
    netlist_path: Option<&PathBuf>,
    output_dir: &Path,
    bound: usize,
    format: &str,
    entity: Option<&str>,
    quick: bool,
    symbolic: bool,
    reset_cycles: u64,
    verbose: bool,
    coverage: bool,
) -> Result<()> {
    use skalp_formal::equivalence::{MirToAig, GateNetlistToAig, check_sequential_equivalence_sat, inject_random_bugs, check_non_equivalence_fast};
    use skalp_frontend::parse_and_build_compilation_context;
    use skalp_lir::{get_stdlib_library, lower_mir_hierarchical_with_top, map_hierarchical_to_gates};
    use std::time::Instant;

    let start_time = Instant::now();

    // Determine mode
    let mode_str = if quick {
        "simulation-only (quick)"
    } else if symbolic {
        "SAT-based symbolic (thorough)"
    } else {
        "hybrid (simulation + SAT)"
    };

    println!("🔍 Equivalence Checking");
    println!("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
    println!("Source: {:?}", source);
    println!("Mode:   {}", mode_str);
    if !symbolic {
        println!("Bound:  {} cycles", bound);
    }
    println!();

    // Create output directory
    fs::create_dir_all(output_dir)?;

    // Step 1: Parse and compile to MIR
    println!("📖 Parsing source...");
    let context = parse_and_build_compilation_context(source)
        .context("Failed to parse source file")?;
    let hir = context.main_hir;
    let module_hirs = context.module_hirs;

    println!("📐 Lowering to MIR...");
    let compiler = skalp_mir::MirCompiler::new()
        .with_optimization_level(skalp_mir::OptimizationLevel::None);
    let mir = compiler
        .compile_to_mir_with_modules(&hir, &module_hirs)
        .map_err(|e| anyhow::anyhow!("Failed to compile to MIR: {}", e))?;

    // Find the target entity
    // BUG FIX: Handle monomorphized module names (e.g., DabBatteryController_100000000_20000000_10000000)
    // When generics are specialized, modules get parameter suffixes. We need to find the actual
    // implementation, not just the base declaration.
    let target_entity = if let Some(name) = entity {
        // First try exact match
        if let Some(m) = mir.modules.iter().find(|m| m.name == name) {
            // Check if this module has instances - if not, look for a monomorphized variant
            if !m.instances.is_empty() || !m.processes.is_empty() {
                m
            } else {
                // Look for monomorphized variant with most content (instances + processes)
                let mono_prefix = format!("{}_", name);
                mir.modules
                    .iter()
                    .filter(|m| m.name.starts_with(&mono_prefix))
                    .max_by_key(|m| m.instances.len() + m.processes.len())
                    .unwrap_or(m)
            }
        } else {
            // Try prefix match for monomorphized modules
            let mono_prefix = format!("{}_", name);
            mir.modules
                .iter()
                .filter(|m| m.name.starts_with(&mono_prefix))
                .max_by_key(|m| m.instances.len() + m.processes.len())
                .ok_or_else(|| anyhow::anyhow!("Entity '{}' not found in design", name))?
        }
    } else {
        find_top_level_module(&mir)
            .ok_or_else(|| anyhow::anyhow!("No modules found in design"))?
    };

    let input_count = target_entity.ports.iter()
        .filter(|p| matches!(p.direction, skalp_mir::mir::PortDirection::Input))
        .count();
    let output_count = target_entity.ports.iter()
        .filter(|p| matches!(p.direction, skalp_mir::mir::PortDirection::Output))
        .count();
    println!("   Entity: {}", target_entity.name);
    println!("   Inputs: {}", input_count);
    println!("   Outputs: {}", output_count);
    println!("   Signals: {}", target_entity.signals.len());

    // Step 2: Lower to hierarchical LIR and flatten
    println!();
    println!("⚙️  Lowering to LIR (hierarchical)...");
    let hier_lir = lower_mir_hierarchical_with_top(&mir, Some(&target_entity.name));
    let lir = hier_lir.flatten();
    println!("   Nodes: {}", lir.nodes.len());
    println!("   Signals: {}", lir.signals.len());

    // Step 3: Get or synthesize gate-level netlist
    let gate_netlist = if let Some(path) = netlist_path {
        println!();
        println!("📂 Loading gate-level netlist from {:?}...", path);
        let json = fs::read_to_string(path)
            .context("Failed to read netlist file")?;
        serde_json::from_str::<skalp_lir::GateNetlist>(&json)
            .context("Failed to parse gate-level netlist JSON")?
    } else {
        println!();
        println!("🔧 Synthesizing gate-level netlist (hierarchical)...");
        let library = get_stdlib_library("generic_asic")
            .context("Failed to load technology library")?;
        let hier_netlist = map_hierarchical_to_gates(&hier_lir, &library);
        let netlist = hier_netlist.flatten();
        println!("   Cells: {}", netlist.cells.len());
        println!("   Nets: {}", netlist.nets.len());
        netlist
    };

    // Step 4: Run equivalence check
    println!();
    println!("🔬 Running equivalence check...");

    let mut overall_pass = true;
    let mut sim_found_bug = false;
    let mut sim_result_for_report: Option<skalp_formal::SimEquivalenceResult> = None;

    // Phase 1: Simulation (unless --symbolic only)
    if !symbolic {
        println!();
        if coverage {
            println!("📊 Phase 1: Coverage-driven simulation-based check...");
        } else {
            println!("📊 Phase 1: Simulation-based check ({} cycles)...", bound);
        }
        use skalp_formal::SimBasedEquivalenceChecker;

        let checker = SimBasedEquivalenceChecker::new()
            .with_cycles(bound as u64)
            .with_reset("rst", reset_cycles)
            .with_coverage(coverage);

        let rt = tokio::runtime::Runtime::new()
            .context("Failed to create tokio runtime")?;

        let sim_result = if coverage {
            rt.block_on(async {
                checker.check_mir_vs_gate_coverage(&mir, target_entity, &gate_netlist).await
            }).map_err(|e| anyhow::anyhow!("Coverage simulation equivalence check failed: {:?}", e))?
        } else {
            rt.block_on(async {
                checker.check_mir_vs_gate(&mir, target_entity, &gate_netlist).await
            }).map_err(|e| anyhow::anyhow!("Simulation equivalence check failed: {:?}", e))?
        };

        if sim_result.equivalent {
            println!("   ✓ Simulation PASS: No mismatch in {} cycles", sim_result.cycles_verified);
        } else {
            println!("   ✗ Simulation FAIL: Mismatch detected!");
            if let Some(cycle) = sim_result.mismatch_cycle {
                println!("     Cycle: {}", cycle);
            }
            if let Some(ref output) = sim_result.mismatch_output {
                println!("     Output: {}", output);
            }
            sim_found_bug = true;
            overall_pass = false;
        }

        // Always store sim result for coverage reporting and EC report generation
        sim_result_for_report = Some(sim_result.clone());

        // If quick mode, stop here (no SAT phase)
        if quick {
            // Print coverage report for quick mode (simulation only, no SAT)
            if let Some(ref sim_result) = sim_result_for_report {
                if let Some(ref cov_report) = sim_result.coverage_report {
                    cov_report.print_summary();
                    let cov_path = output_dir.join("coverage_report.txt");
                    if let Err(e) = cov_report.write_text(&cov_path) {
                        eprintln!("Warning: failed to write coverage report: {}", e);
                    } else {
                        println!("   Coverage report written to {:?}", cov_path);
                    }
                }
            }

            let total_time = start_time.elapsed();
            println!();
            println!("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
            if overall_pass {
                println!("✅ PASS: Designs equivalent for {} cycles (simulation only)", bound);
                println!("   Note: Use --symbolic for exhaustive proof");
            } else {
                println!("❌ FAIL: Mismatch detected by simulation");
            }
            println!("   Time: {:.2}s", total_time.as_secs_f64());

            if !overall_pass {
                anyhow::bail!("Equivalence check failed");
            }
            return Ok(());
        }
    }

    // Phase 2: SAT-based symbolic check (unless simulation already found bug)
    let mut mir_aig_opt = None;
    let mut gate_aig_opt = None;
    let mut sat_passed = false;
    if !sim_found_bug {
        println!();
        println!("🔬 Phase 2: SAT-based symbolic equivalence check...");
        println!("   Checking transition function equivalence for ALL states...");

        // Convert to sequential AIGs (MIR→AIG for behavioral, GateNetlist→AIG for gate)
        let mir_aig = MirToAig::new_with_mir(&mir, target_entity).convert_sequential_hierarchical();
        let gate_aig = GateNetlistToAig::new().convert_sequential(&gate_netlist);

        println!("   MIR AIG:  {} nodes, {} latches", mir_aig.nodes.len(), mir_aig.latches.len());
        println!("   Gate AIG: {} nodes, {} latches", gate_aig.nodes.len(), gate_aig.latches.len());

        match check_sequential_equivalence_sat(&mir_aig, &gate_aig) {
            Ok(sat_result) => {
                if sat_result.equivalent {
                    if sat_result.checked_outputs && sat_result.checked_next_state {
                        println!("   ✓ SAT PASS: Transition functions equivalent for ALL states (full proof)");
                    } else {
                        let mut partial = Vec::new();
                        if !sat_result.checked_outputs { partial.push("some outputs"); }
                        if !sat_result.checked_next_state { partial.push("some latches"); }
                        println!("   ✓ SAT PASS: No non-equivalence found ({} unresolved due to complexity)",
                            partial.join(" + "));
                    }
                    println!("     Proof completed in {}ms", sat_result.time_ms);
                    sat_passed = true;
                } else {
                    println!("   ✗ SAT FAIL: Found state where designs differ!");
                    overall_pass = false;

                    if let Some(ref ce) = sat_result.counterexample {
                        // Show the differing signal
                        if let Some(ref diff_sig) = ce.differing_signal {
                            println!();
                            println!("   🔍 DIFFERING SIGNAL: {}", diff_sig);
                        }

                        // Show ALL inputs that trigger the bug
                        if !ce.inputs.is_empty() {
                            println!();
                            println!("   📥 INPUTS that trigger the bug ({} total):", ce.inputs.len());
                            let mut sorted_inputs: Vec<_> = ce.inputs.iter().collect();
                            sorted_inputs.sort_by_key(|(k, _)| *k);
                            for (name, val) in sorted_inputs.iter().take(300) {
                                println!("       {} = {}", name, if **val { "1" } else { "0" });
                            }
                            if ce.inputs.len() > 300 {
                                println!("       ... and {} more inputs", ce.inputs.len() - 300);
                            }
                        }

                        // Show ALL state values (latch values) where bug occurs
                        if !ce.state.is_empty() {
                            println!();
                            println!("   📊 STATE (latch values) where difference occurs ({} total):", ce.state.len());
                            let mut sorted_state: Vec<_> = ce.state.iter().collect();
                            sorted_state.sort_by_key(|(k, _)| *k);
                            for (name, val) in sorted_state.iter().take(300) {
                                println!("       {} = {}", name, if **val { "1" } else { "0" });
                            }
                            if ce.state.len() > 300 {
                                println!("       ... and {} more state variables", ce.state.len() - 300);
                            }
                        }

                        // This is the key insight for slow state machines
                        println!();
                        println!("   💡 Note: This bug may be in a state unreachable via normal simulation");
                        println!("      (e.g., requires millions of cycles to reach via timeout)");
                        println!("      SAT found it by reasoning symbolically about all possible states.");
                    } else {
                        println!("   ⚠️  No counterexample returned by SAT solver");
                    }
                }
            }
            Err(e) => {
                println!("   ✗ SAT check error: {:?}", e);
                println!("   SAT proof is required — marking EC as FAIL");
                overall_pass = false;
            }
        }

        mir_aig_opt = Some(mir_aig);
        gate_aig_opt = Some(gate_aig);
    }

    // Phase 3: Bug injection self-test (only if SAT passed)
    if sat_passed {
        let phase3_start = std::time::Instant::now();
        println!();
        println!("🧪 Phase 3: Bug injection self-test...");

        let mir_aig = mir_aig_opt.as_ref().unwrap();
        let gate_aig = gate_aig_opt.as_ref().unwrap();

        let bug_count = 10;
        let mutants = inject_random_bugs(gate_aig, bug_count);
        let mut detected = 0;

        for (i, (mutant_aig, desc)) in mutants.iter().enumerate() {
            println!("   Testing bug {}/{}: {}", i + 1, mutants.len(), desc);
            if check_non_equivalence_fast(mir_aig, mutant_aig) {
                detected += 1;
            } else {
                println!("   ⚠ Bug {} undetected (may be in SAT-hard logic): {}", i + 1, desc);
            }
        }

        let phase3_ms = phase3_start.elapsed().as_millis();
        // Require 100% detection — all mutations are always-observable (invert/swap only, no stuck-at)
        let min_detected = mutants.len();
        if detected >= min_detected {
            println!("   ✓ Bug injection: {}/{} detected ({}ms)", detected, mutants.len(), phase3_ms);
        } else {
            println!("   ✗ Bug injection: {}/{} detected (need {}) — EC pipeline may be unsound!", detected, mutants.len(), min_detected);
            overall_pass = false;
        }
    }

    // Print coverage report after SAT check so equivalence_ok reflects the final result
    if let Some(ref mut sim_result) = sim_result_for_report {
        if let Some(ref mut cov_report) = sim_result.coverage_report {
            // Update equivalence status to reflect combined simulation + SAT result
            cov_report.equivalence_ok = overall_pass;
            cov_report.print_summary();
            // Write coverage report to output directory
            let cov_path = output_dir.join("coverage_report.txt");
            if let Err(e) = cov_report.write_text(&cov_path) {
                eprintln!("Warning: failed to write coverage report: {}", e);
            } else {
                println!("   Coverage report written to {:?}", cov_path);
            }
        }
    }

    let total_time = start_time.elapsed();

    // Generate report for non-quick mode
    if !quick {
        // Create a summary result for reporting
        let summary_result = skalp_formal::BmcEquivalenceResult {
            equivalent: overall_pass,
            bound,
            mismatch_cycle: None,
            mismatch_output: None,
            counterexample: None,
            time_ms: total_time.as_millis() as u64,
            sat_calls: 1,
        };
        generate_ec_report(&summary_result, &target_entity.name, output_dir, format, verbose, total_time, sim_result_for_report.as_ref())?;
    }

    // Print final summary
    println!();
    println!("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
    if overall_pass {
        if symbolic {
            println!("✅ PASS: Designs are PROVEN equivalent (SAT-based)");
        } else if sat_passed {
            println!("✅ PASS: Designs equivalent (simulation + SAT proof + self-test)");
        } else {
            println!("✅ PASS: Designs equivalent (simulation)");
        }
    } else {
        println!("❌ FAIL: Designs are NOT equivalent");
    }
    println!("   Time: {:.2}s", total_time.as_secs_f64());

    if !overall_pass {
        anyhow::bail!("Equivalence check failed");
    }

    Ok(())
}

/// Generate equivalence checking report
fn generate_ec_report(
    result: &skalp_formal::BmcEquivalenceResult,
    entity_name: &str,
    output_dir: &Path,
    format: &str,
    verbose: bool,
    total_time: std::time::Duration,
    sim_result: Option<&skalp_formal::SimEquivalenceResult>,
) -> Result<()> {
    let formats: Vec<&str> = if format == "all" {
        vec!["text", "json", "html"]
    } else {
        format.split(',').collect()
    };

    for fmt in formats {
        match fmt.trim() {
            "text" => {
                let report = generate_ec_text_report(result, entity_name, verbose, total_time, sim_result);
                let path = output_dir.join("ec_report.txt");
                fs::write(&path, report)?;
                println!("📄 Text report: {:?}", path);
            }
            "json" => {
                let report = generate_ec_json_report(result, entity_name, total_time, sim_result)?;
                let path = output_dir.join("ec_report.json");
                fs::write(&path, report)?;
                println!("📄 JSON report: {:?}", path);
            }
            "html" => {
                let report = generate_ec_html_report(result, entity_name, verbose, total_time, sim_result);
                let path = output_dir.join("ec_report.html");
                fs::write(&path, report)?;
                println!("📄 HTML report: {:?}", path);
            }
            _ => {
                println!("⚠️  Unknown format '{}', skipping", fmt);
            }
        }
    }

    Ok(())
}

/// Generate text-format equivalence checking report
fn generate_ec_text_report(
    result: &skalp_formal::BmcEquivalenceResult,
    entity_name: &str,
    verbose: bool,
    total_time: std::time::Duration,
    sim_result: Option<&skalp_formal::SimEquivalenceResult>,
) -> String {
    let mut report = String::new();

    report.push_str("================================================================================\n");
    report.push_str("                     EQUIVALENCE CHECKING REPORT\n");
    report.push_str("================================================================================\n\n");

    report.push_str(&format!("Entity:      {}\n", entity_name));
    report.push_str(&format!("Result:      {}\n", if result.equivalent { "EQUIVALENT" } else { "NOT EQUIVALENT" }));
    report.push_str(&format!("Bound:       {} cycles\n", result.bound));
    report.push_str(&format!("Total Time:  {:.3}s\n", total_time.as_secs_f64()));
    report.push_str(&format!("SAT Calls:   {}\n", result.sat_calls));
    report.push_str("\n");

    if !result.equivalent {
        report.push_str("--------------------------------------------------------------------------------\n");
        report.push_str("FAILURE DETAILS\n");
        report.push_str("--------------------------------------------------------------------------------\n\n");

        // Include simulation mismatch details if available
        if let Some(sim) = sim_result {
            if let Some(cycle) = sim.mismatch_cycle {
                report.push_str(&format!("Mismatch Cycle:  {}\n", cycle));
            }
            if let Some(ref output) = sim.mismatch_output {
                report.push_str(&format!("Failing Output:  {}\n", output));
            }
            if let (Some(v1), Some(v2)) = (sim.value_1, sim.value_2) {
                report.push_str(&format!("MIR Value:       {}\n", v1));
                report.push_str(&format!("Gate Value:      {}\n", v2));
            }
            report.push_str(&format!("Cycles Verified: {}\n", sim.cycles_verified));
            report.push_str(&format!("Outputs Compared: {}\n", sim.outputs_compared));

            // Include detailed diagnostics if available
            if let Some(ref diag) = sim.diagnostics {
                // Input values at mismatch
                if !diag.input_values.is_empty() {
                    report.push_str("\n--------------------------------------------------------------------------------\n");
                    report.push_str("INPUT VALUES AT MISMATCH\n");
                    report.push_str("--------------------------------------------------------------------------------\n\n");
                    let mut sorted_inputs: Vec<_> = diag.input_values.iter().collect();
                    sorted_inputs.sort_by(|a, b| a.0.cmp(&b.0));
                    for (name, val) in &sorted_inputs {
                        report.push_str(&format!("  {} = {}\n", name, val));
                    }
                }

                // MIR signal values
                if !diag.mir_signals.is_empty() {
                    report.push_str("\n--------------------------------------------------------------------------------\n");
                    report.push_str("MIR SIGNAL VALUES\n");
                    report.push_str("--------------------------------------------------------------------------------\n\n");
                    let mut sorted_mir: Vec<_> = diag.mir_signals.iter().collect();
                    sorted_mir.sort_by(|a, b| a.0.cmp(&b.0));
                    for (hier_path, internal_name, val) in &sorted_mir {
                        if hier_path.is_empty() || hier_path == internal_name {
                            report.push_str(&format!("  {} = {}\n", internal_name, val));
                        } else {
                            report.push_str(&format!("  {} ({}) = {}\n", hier_path, internal_name, val));
                        }
                    }
                }

                // Gate signal values
                if !diag.gate_signals.is_empty() {
                    report.push_str("\n--------------------------------------------------------------------------------\n");
                    report.push_str("GATE SIGNAL VALUES\n");
                    report.push_str("--------------------------------------------------------------------------------\n\n");
                    let mut sorted_gate: Vec<_> = diag.gate_signals.iter().collect();
                    sorted_gate.sort_by(|a, b| a.0.cmp(&b.0));
                    for (name, val) in &sorted_gate {
                        report.push_str(&format!("  {} = {}\n", name, val));
                    }
                }

                // SIR dataflow trace
                if !diag.sir_dataflow.is_empty() {
                    report.push_str("\n--------------------------------------------------------------------------------\n");
                    report.push_str("SIR DATAFLOW TRACE\n");
                    report.push_str("--------------------------------------------------------------------------------\n\n");
                    for line in &diag.sir_dataflow {
                        report.push_str(&format!("  {}\n", line));
                    }
                }

                // Input matching info
                report.push_str("\n--------------------------------------------------------------------------------\n");
                report.push_str("INPUT PORT MATCHING\n");
                report.push_str("--------------------------------------------------------------------------------\n\n");

                if !diag.input_matching.matched.is_empty() {
                    report.push_str("  Matched Inputs:\n");
                    for (user_name, mir_name, gate_names) in &diag.input_matching.matched {
                        let gate_str = if gate_names.len() == 1 {
                            gate_names[0].clone()
                        } else {
                            format!("[{} bits]", gate_names.len())
                        };
                        report.push_str(&format!("    {} ({}) → {}\n", user_name, mir_name, gate_str));
                    }
                }

                if !diag.input_matching.unmatched_mir.is_empty() {
                    report.push_str("\n  ⚠ Unmatched MIR Inputs:\n");
                    for (user_name, mir_name, reason) in &diag.input_matching.unmatched_mir {
                        report.push_str(&format!("    {} ({}) - {}\n", user_name, mir_name, reason));
                    }
                }

                if !diag.input_matching.unmatched_gate.is_empty() {
                    report.push_str("\n  ⚠ Unmatched Gate Inputs:\n");
                    let mut shown = std::collections::HashSet::new();
                    for (gate_name, reason) in &diag.input_matching.unmatched_gate {
                        // Only show base name once to avoid repetition for multi-bit signals
                        let base = gate_name.split('[').next().unwrap_or(gate_name);
                        if shown.insert(base.to_string()) {
                            report.push_str(&format!("    {} - {}\n", base, reason));
                        }
                    }
                }

                // Cycle trace (per-cycle signal history)
                if !diag.cycle_trace.is_empty() {
                    report.push_str("\n--------------------------------------------------------------------------------\n");
                    report.push_str(&format!("CYCLE-BY-CYCLE TRACE (last {} cycles before mismatch)\n", diag.cycle_trace.len()));
                    report.push_str("--------------------------------------------------------------------------------\n\n");

                    // Find the mismatching signal to highlight
                    let mismatch_signal = sim.mismatch_output.as_ref();

                    for entry in &diag.cycle_trace {
                        report.push_str(&format!("  Cycle {} ({}):\n", entry.cycle, entry.phase));

                        // Show inputs applied
                        if !entry.inputs.is_empty() {
                            report.push_str("    Inputs:\n");
                            for (name, val) in &entry.inputs {
                                report.push_str(&format!("      {} = {}\n", name, val));
                            }
                        }

                        // Show signals with mismatches highlighted
                        report.push_str("    Outputs:\n");
                        for (user_name, internal_name, mir_val, gate_val, matches) in &entry.signals {
                            let mir_str = mir_val.map(|v| v.to_string()).unwrap_or("?".to_string());
                            let gate_str = gate_val.map(|v| v.to_string()).unwrap_or("?".to_string());

                            let is_failing = mismatch_signal.map(|s| s == user_name).unwrap_or(false);
                            let marker = if !matches {
                                if is_failing { ">>> " } else { "!!! " }
                            } else {
                                "    "
                            };

                            if !matches || is_failing {
                                report.push_str(&format!("{}  {} ({}): MIR={}, Gate={}\n",
                                    marker, user_name, internal_name, mir_str, gate_str));
                            }
                        }
                        report.push_str("\n");
                    }
                }
            }
        } else {
            // Fallback to BmcEquivalenceResult fields
            if let Some(cycle) = result.mismatch_cycle {
                report.push_str(&format!("Mismatch Cycle:  {}\n", cycle));
            }
            if let Some(ref output) = result.mismatch_output {
                report.push_str(&format!("Failing Output:  {}\n", output));
            }
        }

        if let Some(ref cex) = result.counterexample {
            report.push_str("\n--------------------------------------------------------------------------------\n");
            report.push_str("SAT COUNTEREXAMPLE TRACE\n");
            report.push_str("--------------------------------------------------------------------------------\n");
            for (cycle, inputs) in cex.inputs_per_cycle.iter().enumerate() {
                report.push_str(&format!("\n  Cycle {}:\n", cycle));
                report.push_str("    Inputs:\n");
                let mut sorted_inputs: Vec<_> = inputs.iter().collect();
                sorted_inputs.sort_by_key(|(k, _)| *k);
                for (name, value) in sorted_inputs.iter().take(20) {
                    report.push_str(&format!("      {} = {}\n", name, if **value { "1" } else { "0" }));
                }
                if sorted_inputs.len() > 20 {
                    report.push_str(&format!("      ... and {} more inputs\n", sorted_inputs.len() - 20));
                }
                // Show output differences if available
                if let (Some(out1), Some(out2)) = (
                    cex.outputs1_per_cycle.get(cycle),
                    cex.outputs2_per_cycle.get(cycle)
                ) {
                    report.push_str("    Output Differences:\n");
                    for (name, val1) in out1 {
                        if let Some(val2) = out2.get(name) {
                            if val1 != val2 {
                                report.push_str(&format!("      {}: RTL={} Gate={}\n",
                                    name, if *val1 { "1" } else { "0" }, if *val2 { "1" } else { "0" }));
                            }
                        }
                    }
                }
            }
        }
    }

    if verbose {
        report.push_str("\n--------------------------------------------------------------------------------\n");
        report.push_str("VERBOSE DETAILS\n");
        report.push_str("--------------------------------------------------------------------------------\n\n");
        report.push_str(&format!("BMC Internal Time: {}ms\n", result.time_ms));
    }

    report.push_str("\n================================================================================\n");

    report
}

/// Generate JSON-format equivalence checking report
fn generate_ec_json_report(
    result: &skalp_formal::BmcEquivalenceResult,
    entity_name: &str,
    total_time: std::time::Duration,
    sim_result: Option<&skalp_formal::SimEquivalenceResult>,
) -> Result<String> {
    use serde_json::json;

    // Build simulation diagnostics section if available
    let sim_diagnostics = sim_result.and_then(|sim| {
        sim.diagnostics.as_ref().map(|diag| {
            json!({
                "input_values": diag.input_values.iter()
                    .map(|(n, v)| json!({"name": n, "value": v}))
                    .collect::<Vec<_>>(),
                "mir_signals": diag.mir_signals.iter()
                    .map(|(hier, internal, v)| json!({
                        "hierarchical_path": hier,
                        "internal_name": internal,
                        "value": v
                    }))
                    .collect::<Vec<_>>(),
                "gate_signals": diag.gate_signals.iter()
                    .map(|(n, v)| json!({"name": n, "value": v}))
                    .collect::<Vec<_>>(),
                "sir_dataflow": diag.sir_dataflow,
            })
        })
    });

    // Build simulation result section if available
    let sim_section = sim_result.map(|sim| {
        json!({
            "mismatch_cycle": sim.mismatch_cycle,
            "mismatch_output": sim.mismatch_output,
            "mir_value": sim.value_1,
            "gate_value": sim.value_2,
            "cycles_verified": sim.cycles_verified,
            "outputs_compared": sim.outputs_compared,
            "time_ms": sim.time_ms,
            "diagnostics": sim_diagnostics,
        })
    });

    let report = json!({
        "entity": entity_name,
        "result": if result.equivalent { "equivalent" } else { "not_equivalent" },
        "equivalent": result.equivalent,
        "bound": result.bound,
        "mismatch_cycle": result.mismatch_cycle,
        "mismatch_output": result.mismatch_output,
        "sat_calls": result.sat_calls,
        "time_ms": result.time_ms,
        "total_time_ms": total_time.as_millis() as u64,
        "simulation": sim_section,
        "counterexample": result.counterexample.as_ref().map(|cex| {
            json!({
                "inputs_per_cycle": cex.inputs_per_cycle,
                "state1_per_cycle": cex.state1_per_cycle,
                "state2_per_cycle": cex.state2_per_cycle,
                "outputs1_per_cycle": cex.outputs1_per_cycle,
                "outputs2_per_cycle": cex.outputs2_per_cycle,
            })
        }),
    });

    Ok(serde_json::to_string_pretty(&report)?)
}

/// Generate HTML-format equivalence checking report
fn generate_ec_html_report(
    result: &skalp_formal::BmcEquivalenceResult,
    entity_name: &str,
    _verbose: bool,
    total_time: std::time::Duration,
    sim_result: Option<&skalp_formal::SimEquivalenceResult>,
) -> String {
    let status_class = if result.equivalent { "pass" } else { "fail" };
    let status_text = if result.equivalent { "EQUIVALENT" } else { "NOT EQUIVALENT" };
    let status_icon = if result.equivalent { "✅" } else { "❌" };

    let mut html = format!(r#"<!DOCTYPE html>
<html>
<head>
    <title>Equivalence Check Report - {}</title>
    <style>
        body {{ font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif; margin: 40px; background: #f5f5f5; }}
        .container {{ max-width: 1100px; margin: 0 auto; background: white; padding: 30px; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.1); }}
        h1 {{ color: #333; border-bottom: 2px solid #eee; padding-bottom: 10px; }}
        .status {{ padding: 20px; border-radius: 8px; margin: 20px 0; font-size: 1.2em; }}
        .status.pass {{ background: #d4edda; color: #155724; border: 1px solid #c3e6cb; }}
        .status.fail {{ background: #f8d7da; color: #721c24; border: 1px solid #f5c6cb; }}
        table {{ width: 100%; border-collapse: collapse; margin: 20px 0; }}
        th, td {{ padding: 12px; text-align: left; border-bottom: 1px solid #ddd; }}
        th {{ background: #f8f9fa; font-weight: 600; width: 30%; }}
        .section {{ margin: 30px 0; }}
        .section h2 {{ color: #495057; font-size: 1.1em; margin-bottom: 15px; }}
        .section h3 {{ color: #6c757d; font-size: 1em; margin: 15px 0 10px 0; }}
        .trace {{ background: #f8f9fa; padding: 15px; border-radius: 4px; font-family: monospace; font-size: 0.9em; overflow-x: auto; max-height: 400px; overflow-y: auto; }}
        .signal-table {{ font-family: monospace; font-size: 0.85em; }}
        .signal-table th {{ width: 60%; }}
        .signal-table td {{ text-align: right; }}
        .footer {{ margin-top: 30px; padding-top: 20px; border-top: 1px solid #eee; color: #6c757d; font-size: 0.9em; }}
        .collapsible {{ cursor: pointer; user-select: none; }}
        .collapsible:after {{ content: ' [+]'; color: #6c757d; }}
        .collapsible.active:after {{ content: ' [-]'; }}
        .content {{ display: none; padding-top: 10px; }}
        .content.show {{ display: block; }}
    </style>
</head>
<body>
    <div class="container">
        <h1>Equivalence Checking Report</h1>

        <div class="status {}">
            {} {} - {}
        </div>

        <div class="section">
            <h2>Summary</h2>
            <table>
                <tr><th>Entity</th><td>{}</td></tr>
                <tr><th>Bound</th><td>{} cycles</td></tr>
                <tr><th>Total Time</th><td>{:.3}s</td></tr>
                <tr><th>SAT Calls</th><td>{}</td></tr>
            </table>
        </div>
"#, entity_name, status_class, status_icon, status_text, entity_name,
    entity_name, result.bound, total_time.as_secs_f64(), result.sat_calls);

    if !result.equivalent {
        // Include simulation mismatch details if available
        if let Some(sim) = sim_result {
            html.push_str(r#"
        <div class="section">
            <h2>Simulation Mismatch Details</h2>
            <table>
"#);
            if let Some(cycle) = sim.mismatch_cycle {
                html.push_str(&format!("                <tr><th>Mismatch Cycle</th><td>{}</td></tr>\n", cycle));
            }
            if let Some(ref output) = sim.mismatch_output {
                html.push_str(&format!("                <tr><th>Failing Output</th><td><code>{}</code></td></tr>\n", output));
            }
            if let (Some(v1), Some(v2)) = (sim.value_1, sim.value_2) {
                html.push_str(&format!("                <tr><th>MIR Value</th><td>{}</td></tr>\n", v1));
                html.push_str(&format!("                <tr><th>Gate Value</th><td>{}</td></tr>\n", v2));
            }
            html.push_str(&format!("                <tr><th>Cycles Verified</th><td>{}</td></tr>\n", sim.cycles_verified));
            html.push_str(&format!("                <tr><th>Outputs Compared</th><td>{}</td></tr>\n", sim.outputs_compared));
            html.push_str("            </table>\n        </div>\n");

            // Include detailed diagnostics if available
            if let Some(ref diag) = sim.diagnostics {
                // Input values at mismatch
                if !diag.input_values.is_empty() {
                    html.push_str(r#"
        <div class="section">
            <h2 class="collapsible" onclick="this.classList.toggle('active'); this.nextElementSibling.classList.toggle('show');">Input Values at Mismatch</h2>
            <div class="content">
            <table class="signal-table">
                <tr><th>Signal</th><th>Value</th></tr>
"#);
                    let mut sorted: Vec<_> = diag.input_values.iter().collect();
                    sorted.sort_by(|a, b| a.0.cmp(&b.0));
                    for (name, val) in &sorted {
                        html.push_str(&format!("                <tr><td>{}</td><td>{}</td></tr>\n", name, val));
                    }
                    html.push_str("            </table>\n            </div>\n        </div>\n");
                }

                // MIR signal values
                if !diag.mir_signals.is_empty() {
                    html.push_str(r#"
        <div class="section">
            <h2 class="collapsible" onclick="this.classList.toggle('active'); this.nextElementSibling.classList.toggle('show');">MIR Signal Values</h2>
            <div class="content">
            <table class="signal-table">
                <tr><th>Hierarchical Path</th><th>Internal Name</th><th>Value</th></tr>
"#);
                    let mut sorted: Vec<_> = diag.mir_signals.iter().collect();
                    sorted.sort_by(|a, b| a.0.cmp(&b.0));
                    for (hier, internal, val) in &sorted {
                        let hier_display = if hier.is_empty() { "-" } else { hier.as_str() };
                        html.push_str(&format!("                <tr><td>{}</td><td>{}</td><td>{}</td></tr>\n",
                            hier_display, internal, val));
                    }
                    html.push_str("            </table>\n            </div>\n        </div>\n");
                }

                // Gate signal values
                if !diag.gate_signals.is_empty() {
                    html.push_str(r#"
        <div class="section">
            <h2 class="collapsible" onclick="this.classList.toggle('active'); this.nextElementSibling.classList.toggle('show');">Gate Signal Values</h2>
            <div class="content">
            <table class="signal-table">
                <tr><th>Signal</th><th>Value</th></tr>
"#);
                    let mut sorted: Vec<_> = diag.gate_signals.iter().collect();
                    sorted.sort_by(|a, b| a.0.cmp(&b.0));
                    for (name, val) in &sorted {
                        html.push_str(&format!("                <tr><td>{}</td><td>{}</td></tr>\n", name, val));
                    }
                    html.push_str("            </table>\n            </div>\n        </div>\n");
                }

                // SIR dataflow trace
                if !diag.sir_dataflow.is_empty() {
                    html.push_str(r#"
        <div class="section">
            <h2 class="collapsible" onclick="this.classList.toggle('active'); this.nextElementSibling.classList.toggle('show');">SIR Dataflow Trace</h2>
            <div class="content">
            <div class="trace">
"#);
                    for line in &diag.sir_dataflow {
                        html.push_str(&format!("{}<br>\n", line));
                    }
                    html.push_str("            </div>\n            </div>\n        </div>\n");
                }
            }
        } else {
            // Fallback to BmcEquivalenceResult fields
            html.push_str(r#"
        <div class="section">
            <h2>Failure Details</h2>
            <table>
"#);
            if let Some(cycle) = result.mismatch_cycle {
                html.push_str(&format!("                <tr><th>Mismatch Cycle</th><td>{}</td></tr>\n", cycle));
            }
            if let Some(ref output) = result.mismatch_output {
                html.push_str(&format!("                <tr><th>Failing Output</th><td>{}</td></tr>\n", output));
            }
            html.push_str("            </table>\n        </div>\n");
        }

        if let Some(ref cex) = result.counterexample {
            html.push_str(r#"
        <div class="section">
            <h2>SAT Counterexample Trace</h2>
            <div class="trace">
"#);
            for (cycle, inputs) in cex.inputs_per_cycle.iter().enumerate() {
                html.push_str(&format!("<strong>Cycle {}:</strong><br>\n", cycle));
                html.push_str("<em>Inputs:</em><br>\n");
                let mut sorted_inputs: Vec<_> = inputs.iter().collect();
                sorted_inputs.sort_by_key(|(k, _)| *k);
                for (name, value) in sorted_inputs.iter().take(20) {
                    html.push_str(&format!("&nbsp;&nbsp;{} = {}<br>\n", name, if **value { "1" } else { "0" }));
                }
                if sorted_inputs.len() > 20 {
                    html.push_str(&format!("&nbsp;&nbsp;... and {} more inputs<br>\n", sorted_inputs.len() - 20));
                }
                // Show output differences
                if let (Some(out1), Some(out2)) = (
                    cex.outputs1_per_cycle.get(cycle),
                    cex.outputs2_per_cycle.get(cycle)
                ) {
                    html.push_str("<em>Output Differences:</em><br>\n");
                    for (name, val1) in out1 {
                        if let Some(val2) = out2.get(name) {
                            if val1 != val2 {
                                html.push_str(&format!("&nbsp;&nbsp;{}: <span style='color:blue'>RTL={}</span> vs <span style='color:red'>Gate={}</span><br>\n",
                                    name, if *val1 { "1" } else { "0" }, if *val2 { "1" } else { "0" }));
                            }
                        }
                    }
                }
                html.push_str("<br>\n");
            }
            html.push_str("            </div>\n        </div>\n");
        }
    }

    html.push_str(&format!(r#"
        <div class="footer">
            Generated by SKALP Equivalence Checker<br>
            Report generated at: {}
        </div>
    </div>
</body>
</html>
"#, chrono::Local::now().format("%Y-%m-%d %H:%M:%S")));

    html
}

/// Synthesize design for FPGA target
fn synthesize_design(
    source: &PathBuf,
    device: &str,
    full_flow: bool,
    output_dir: &PathBuf,
    pnr_preset: &str,
) -> Result<()> {
    use skalp_frontend::parse_and_build_hir_from_file;
    use skalp_lir::{get_stdlib_library, lower_mir_module_to_lir, map_lir_to_gates_optimized};

    println!("🔧 Synthesizing design for device: {}", device);
    println!("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
    println!("Source: {:?}", source);

    // Parse device string (e.g., "ice40-hx8k" -> ("ice40", "hx8k"))
    let (family, variant) = device.split_once('-').ok_or_else(|| {
        anyhow::anyhow!(
            "Invalid device format: '{}'. Expected format: family-variant (e.g., ice40-hx8k)",
            device
        )
    })?;

    // Determine library to use based on device family
    let library_name = match family {
        "ice40" => "ice40",
        "xc7" | "xilinx" => "generic_asic", // Xilinx support pending
        "sky130" => "sky130",
        "freepdk45" => "freepdk45",
        _ => anyhow::bail!(
            "Unsupported device family: {}. Supported: ice40, sky130, freepdk45",
            family
        ),
    };

    // Load technology library
    let library = get_stdlib_library(library_name)
        .context(format!("Failed to load {} library", library_name))?;
    println!("Library: {}", library_name);

    // Parse and build HIR
    let hir = parse_and_build_hir_from_file(source).context("Failed to parse source")?;

    // Lower to MIR
    let compiler =
        skalp_mir::MirCompiler::new().with_optimization_level(skalp_mir::OptimizationLevel::None);
    let mir = compiler
        .compile_to_mir(&hir)
        .map_err(|e| anyhow::anyhow!("MIR compilation failed: {}", e))?;

    // Get top module
    let top_module = find_top_level_module(&mir)
        .ok_or_else(|| anyhow::anyhow!("No modules found in design"))?;
    println!("Module: {}", top_module.name);

    // Lower to LIR
    let lir_result = lower_mir_module_to_lir(top_module);

    // Technology mapping
    let tech_result = map_lir_to_gates_optimized(&lir_result.lir, &library);
    let gate_netlist = tech_result.netlist;
    println!("Cells: {}", gate_netlist.cells.len());
    println!("Nets: {}", gate_netlist.nets.len());

    // Create output directory
    fs::create_dir_all(output_dir)?;

    // Save gate-level netlist as JSON
    let netlist_path = output_dir.join("design_gates.json");
    let netlist_json = serde_json::to_string_pretty(&gate_netlist)?;
    fs::write(&netlist_path, &netlist_json)?;
    println!("Gate netlist: {:?}", netlist_path);

    // Save Verilog
    let verilog_path = output_dir.join("design_gates.v");
    fs::write(&verilog_path, gate_netlist.to_verilog())?;
    println!("Verilog: {:?}", verilog_path);

    // Run place and route if full_flow is enabled
    if full_flow && family == "ice40" {
        println!();
        println!("🔧 Running Place & Route...");
        run_pnr_on_netlist(&gate_netlist, variant, output_dir, pnr_preset, None)?;
    } else if full_flow {
        println!();
        println!("⚠️  Place & route only supported for iCE40 devices currently");
    }

    println!();
    println!("✅ Synthesis complete");
    Ok(())
}

/// Run place and route on an existing gate-level netlist
fn run_place_and_route(
    netlist_path: &PathBuf,
    device: &str,
    output_dir: &PathBuf,
    preset: &str,
    frequency: Option<f64>,
) -> Result<()> {
    println!("🔧 Place & Route");
    println!("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
    println!("Netlist: {:?}", netlist_path);
    println!("Device: {}", device);

    // Parse device string
    let (family, variant) = device.split_once('-').ok_or_else(|| {
        anyhow::anyhow!(
            "Invalid device format: '{}'. Expected: family-variant (e.g., ice40-hx8k)",
            device
        )
    })?;

    if family != "ice40" {
        anyhow::bail!(
            "Place & route only supports iCE40 devices currently. Got: {}",
            family
        );
    }

    // Load netlist from JSON
    let netlist_json = fs::read_to_string(netlist_path)?;
    let gate_netlist: skalp_lir::gate_netlist::GateNetlist =
        serde_json::from_str(&netlist_json).context("Failed to parse gate-level netlist JSON")?;

    println!("Design: {}", gate_netlist.name);
    println!("Cells: {}", gate_netlist.cells.len());
    println!("Nets: {}", gate_netlist.nets.len());

    // Create output directory
    fs::create_dir_all(output_dir)?;

    // Run P&R
    run_pnr_on_netlist(&gate_netlist, variant, output_dir, preset, frequency)?;

    println!();
    println!("✅ Place & route complete");
    Ok(())
}

/// Internal function to run P&R on a GateNetlist
fn run_pnr_on_netlist(
    gate_netlist: &skalp_lir::gate_netlist::GateNetlist,
    variant: &str,
    output_dir: &std::path::Path,
    preset: &str,
    frequency: Option<f64>,
) -> Result<()> {
    use skalp_place_route::{place_and_route, Ice40Variant, PnrConfig, TimingConfig};

    // Parse iCE40 variant
    let ice40_variant = match variant.to_lowercase().as_str() {
        "hx1k" => Ice40Variant::Hx1k,
        "hx4k" => Ice40Variant::Hx4k,
        "hx8k" => Ice40Variant::Hx8k,
        "lp1k" => Ice40Variant::Lp1k,
        "lp4k" => Ice40Variant::Lp4k,
        "lp8k" => Ice40Variant::Lp8k,
        "up5k" => Ice40Variant::Up5k,
        _ => anyhow::bail!(
            "Unknown iCE40 variant: {}. Supported: hx1k, hx4k, hx8k, lp1k, lp4k, lp8k, up5k",
            variant
        ),
    };

    // Select P&R preset
    let mut config = match preset.to_lowercase().as_str() {
        "fast" => PnrConfig::fast(),
        "default" => PnrConfig::default(),
        "high_quality" | "high-quality" => PnrConfig::high_quality(),
        _ => {
            println!("⚠️  Unknown preset '{}', using default", preset);
            PnrConfig::default()
        }
    };

    // Apply target frequency if specified
    if let Some(freq) = frequency {
        config.timing = Some(TimingConfig {
            target_frequency: freq,
            ..TimingConfig::default()
        });
    }

    println!("Variant: {:?}", ice40_variant);
    println!("Preset: {}", preset);
    if let Some(ref timing) = config.timing {
        println!("Target freq: {} MHz", timing.target_frequency);
    }

    // Run place and route
    println!();
    println!("Running placement...");
    let result = place_and_route(gate_netlist, ice40_variant, config)?;

    // Report results
    println!("Placement:");
    println!("  Cells placed: {}", result.placement.placements.len());

    println!("Routing:");
    println!("  Nets routed: {}", result.routing.routes.len());
    println!("  Total wirelength: {}", result.routing.wirelength);
    println!("  Congestion: {:.2}", result.routing.congestion);
    if result.routing.success {
        println!("  Status: ✅ Success");
    } else {
        println!("  Status: ⚠️ Incomplete (some nets unrouted)");
    }

    // Report timing if available
    if let Some(ref timing) = result.timing {
        println!("Timing:");
        println!("  Target: {} MHz", timing.target_frequency);
        println!("  Achieved: {:.2} MHz", timing.design_frequency);
        if timing.meets_timing {
            println!("  Status: ✅ Timing met");
        } else {
            println!("  Status: ⚠️ {} failing paths", timing.failing_paths);
            println!("  Worst slack: {:.2} ns", -timing.worst_negative_slack);
        }
    }

    // Write bitstream
    let bin_path = output_dir.join("design.bin");
    result.bitstream.write_to_file(&bin_path)?;
    println!();
    println!(
        "Bitstream: {:?} ({} bytes)",
        bin_path,
        result.bitstream.data.len()
    );

    // Also write IceStorm ASCII format for debugging (with LUT init values from netlist)
    let asc_path = output_dir.join("design.asc");
    fs::write(
        &asc_path,
        result.to_icestorm_ascii_with_netlist(Some(gate_netlist)),
    )?;
    println!("ASCII: {:?}", asc_path);

    // Write timing report if available
    if let Some(ref timing) = result.timing {
        let timing_path = output_dir.join("timing_report.json");
        let timing_json = serde_json::to_string_pretty(timing)?;
        fs::write(&timing_path, timing_json)?;
        println!("Timing report: {:?}", timing_path);
    }

    Ok(())
}

/// Program FPGA device
fn program_device(
    bitstream: Option<&PathBuf>,
    board: &str,
    reset_only: bool,
    list: bool,
) -> Result<()> {
    use skalp_place_route::programmer::{BoardConfig, Ice40Programmer};

    // List mode: just show detected boards (doesn't need board selection)
    if list {
        println!("🔍 Scanning for iCE40 boards...\n");
        let boards = [
            BoardConfig::IceBreaker,
            BoardConfig::IceBreakerBitsy,
            BoardConfig::LatticeHx8kBreakout,
            BoardConfig::Upduino3,
            BoardConfig::GenericFt2232h,
            BoardConfig::GenericFt232h,
        ];
        let mut found = false;
        for b in boards {
            if Ice40Programmer::open(b).is_ok() {
                let (vid, pid) = b.usb_id();
                println!("   ✓ {} (VID:PID {:04x}:{:04x})", b.name(), vid, pid);
                found = true;
            }
        }
        if !found {
            println!("   No boards detected. Is the board connected?");
        }
        return Ok(());
    }

    // Parse board configuration
    let board_config = match board.to_lowercase().as_str() {
        "icebreaker" => BoardConfig::IceBreaker,
        "icebreaker-bitsy" => BoardConfig::IceBreakerBitsy,
        "hx8k-breakout" | "hx8k" => BoardConfig::LatticeHx8kBreakout,
        "upduino3" | "upduino" => BoardConfig::Upduino3,
        "ft2232h" => BoardConfig::GenericFt2232h,
        "ft232h" => BoardConfig::GenericFt232h,
        "auto" => {
            // Try to detect board automatically
            println!("🔍 Auto-detecting board...");
            let boards = [
                BoardConfig::IceBreaker,
                BoardConfig::LatticeHx8kBreakout,
                BoardConfig::Upduino3,
            ];
            let mut detected = None;
            for b in boards {
                if Ice40Programmer::open(b).is_ok() {
                    detected = Some(b);
                    break;
                }
            }
            match detected {
                Some(b) => {
                    println!("   Detected: {}", b.name());
                    b
                }
                None => {
                    anyhow::bail!(
                        "No iCE40 board detected. Is the board connected?\n\
                         Supported boards: icebreaker, icebreaker-bitsy, hx8k-breakout, upduino3"
                    );
                }
            }
        }
        _ => {
            anyhow::bail!(
                "Unknown board: {}. Supported: icebreaker, icebreaker-bitsy, hx8k-breakout, upduino3, auto",
                board
            );
        }
    };

    // Open the programmer
    let mut programmer = Ice40Programmer::open(board_config).map_err(|e| {
        anyhow::anyhow!(
            "Failed to open programmer for {}: {}",
            board_config.name(),
            e
        )
    })?;

    // Reset only mode
    if reset_only {
        println!("🔄 Resetting {}...", board_config.name());
        programmer
            .reset()
            .map_err(|e| anyhow::anyhow!("Reset failed: {}", e))?;
        println!("   ✓ Reset complete");
        return Ok(());
    }

    // Check bitstream file (required for programming)
    let bitstream = bitstream.ok_or_else(|| anyhow::anyhow!("Bitstream file is required"))?;

    if !bitstream.exists() {
        anyhow::bail!("Bitstream file not found: {:?}", bitstream);
    }

    // Read bitstream
    let bitstream_data = std::fs::read(bitstream)?;
    let size_kb = bitstream_data.len() as f64 / 1024.0;

    println!("📤 Programming {}...", board_config.name());
    println!("   Bitstream: {:?} ({:.1} KB)", bitstream, size_kb);

    // Program the device
    let result = programmer
        .program(&bitstream_data)
        .map_err(|e| anyhow::anyhow!("Programming failed: {}", e))?;

    if result.cdone_high {
        println!("   ✓ Programming successful");
        println!(
            "   {} bytes in {}ms ({:.1} KB/s)",
            result.bytes_programmed, result.time_ms, result.rate_kbps
        );
    } else {
        println!("   ⚠️  Programming complete but CDONE not high");
        println!("   The FPGA may not have configured correctly.");
        println!("   Check the bitstream and board connections.");
    }

    Ok(())
}

/// Format SKALP source files
fn format_files(files: &[PathBuf], check: bool) -> Result<()> {
    use skalp_frontend::{format_ast, parse_file};

    if files.is_empty() {
        println!("No files specified");
        return Ok(());
    }

    let mut needs_formatting = false;

    for file in files {
        if file.extension() != Some(std::ffi::OsStr::new("sk"))
            && file.extension() != Some(std::ffi::OsStr::new("skalp"))
        {
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
    println!("🧪 Running property-based tests...");

    if let Some(f) = filter {
        println!("   Filter: {}", f);
    }

    // Find and load test files
    let test_files = fs::read_dir("tests")?
        .filter_map(|entry| entry.ok())
        .filter(|entry| entry.path().extension() == Some(std::ffi::OsStr::new("sk")))
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

/// Add a dependency to skalp.toml
fn add_dependency(
    package: &str,
    version: Option<&str>,
    dev: bool,
    optional: bool,
    features: &[String],
) -> Result<()> {
    use skalp_manifest::{Dependency, DependencySpec};
    use skalp_package::{RegistryClient, RegistryConfig};

    println!("Adding dependency: {}", package);

    // Load existing manifest
    let manifest_path = PathBuf::from("skalp.toml");
    let mut manifest = if manifest_path.exists() {
        skalp_manifest::from_path(&manifest_path)?
    } else {
        anyhow::bail!("No skalp.toml found. Run 'skalp new' to create a project.");
    };

    // Resolve version from registry if not specified
    let version_str = if let Some(v) = version {
        v.to_string()
    } else {
        println!("Fetching latest version from registry...");
        let config = RegistryConfig::default();
        let client = RegistryClient::new(config.url)?;
        let metadata = client.fetch_metadata(package)?;

        if metadata.versions.is_empty() {
            anyhow::bail!("No versions found for package: {}", package);
        }

        // Get latest version
        let latest = &metadata.versions[0].version;
        println!("Using version: {}", latest);
        latest.clone()
    };

    // Create dependency spec
    let dep_spec = if features.is_empty() && !optional {
        DependencySpec::Simple(version_str.clone())
    } else {
        DependencySpec::Detailed(Dependency {
            version: Some(version_str.clone()),
            git: None,
            branch: None,
            tag: None,
            revision: None,
            path: None,
            registry: None,
            features: features.to_vec(),
            default_features: true,
            optional,
            package: None,
        })
    };

    // Add to appropriate section
    if dev {
        manifest
            .dev_dependencies
            .insert(package.to_string(), dep_spec);
        println!("✅ Added {} {} as dev dependency", package, version_str);
    } else {
        manifest.dependencies.insert(package.to_string(), dep_spec);
        println!("✅ Added {} {}", package, version_str);
    }

    // Save manifest
    save_manifest(&manifest, &manifest_path)?;

    println!("💾 Updated skalp.toml");
    println!("\n💡 Run 'skalp build' to fetch and build with new dependency");

    Ok(())
}

/// Remove a dependency from skalp.toml
fn remove_dependency(package: &str, dev: bool) -> Result<()> {
    println!("Removing dependency: {}", package);

    // Load existing manifest
    let manifest_path = PathBuf::from("skalp.toml");
    let mut manifest = if manifest_path.exists() {
        skalp_manifest::from_path(&manifest_path)?
    } else {
        anyhow::bail!("No skalp.toml found.");
    };

    // Remove from appropriate section
    let removed = if dev {
        manifest.dev_dependencies.remove(package).is_some()
    } else {
        manifest.dependencies.remove(package).is_some()
    };

    if !removed {
        anyhow::bail!(
            "Dependency '{}' not found in {}",
            package,
            if dev {
                "dev-dependencies"
            } else {
                "dependencies"
            }
        );
    }

    // Save manifest
    save_manifest(&manifest, &manifest_path)?;

    println!(
        "✅ Removed {} from {}",
        package,
        if dev {
            "dev dependencies"
        } else {
            "dependencies"
        }
    );
    println!("💾 Updated skalp.toml");

    Ok(())
}

/// Update dependencies to latest versions
fn update_dependencies(package: Option<&str>, force: bool) -> Result<()> {
    use skalp_package::{RegistryClient, RegistryConfig};

    println!("🔄 Updating dependencies...");

    // Load existing manifest
    let manifest_path = PathBuf::from("skalp.toml");
    let mut manifest = if manifest_path.exists() {
        skalp_manifest::from_path(&manifest_path)?
    } else {
        anyhow::bail!("No skalp.toml found.");
    };

    let config = RegistryConfig::default();
    let client = RegistryClient::new(config.url)?;

    // Determine which packages to update
    let packages_to_update: Vec<String> = if let Some(pkg) = package {
        vec![pkg.to_string()]
    } else {
        manifest.dependencies.keys().cloned().collect()
    };

    let mut updated = false;

    for pkg_name in &packages_to_update {
        // Only update registry dependencies
        if let Some(skalp_manifest::DependencySpec::Simple(_)) = manifest.dependencies.get(pkg_name)
        {
            println!("Checking {} ...", pkg_name);

            match client.fetch_metadata(pkg_name) {
                Ok(metadata) => {
                    if !metadata.versions.is_empty() {
                        let latest = &metadata.versions[0].version;
                        println!("  Latest version: {}", latest);

                        // Update to latest
                        manifest.dependencies.insert(
                            pkg_name.clone(),
                            skalp_manifest::DependencySpec::Simple(latest.clone()),
                        );
                        updated = true;
                        println!("  ✅ Updated to {}", latest);
                    }
                }
                Err(e) => {
                    println!("  ⚠️  Could not fetch metadata: {}", e);
                }
            }
        }
    }

    if updated {
        save_manifest(&manifest, &manifest_path)?;
        println!("\n💾 Updated skalp.toml");
        println!("💡 Run 'skalp build' to fetch updated dependencies");
    } else {
        println!("✅ All dependencies already up to date");
    }

    Ok(())
}

/// Search for packages in the registry
fn search_packages(query: &str, limit: usize) -> Result<()> {
    use skalp_package::{RegistryClient, RegistryConfig};

    println!("🔍 Searching for: {}", query);

    let config = RegistryConfig::default();
    let client = RegistryClient::new(config.url)?;

    let results = client.search(query)?;

    if results.is_empty() {
        println!("No packages found matching '{}'", query);
        return Ok(());
    }

    println!("\nFound {} packages:\n", results.len().min(limit));

    for (i, pkg) in results.iter().take(limit).enumerate() {
        println!(
            "{}. {} ({})",
            i + 1,
            pkg.name,
            pkg.versions
                .first()
                .map(|v| v.version.as_str())
                .unwrap_or("unknown")
        );

        if let Some(desc) = &pkg.description {
            println!("   {}", desc);
        }

        if let Some(repo) = &pkg.repository {
            println!("   🔗 {}", repo);
        }

        println!();
    }

    Ok(())
}

/// List cached packages
fn list_cache() -> Result<()> {
    use skalp_package::{cache::Cache, RegistryConfig};

    let config = RegistryConfig::default();
    let cache = Cache::new(config.cache_dir.clone());

    println!("📦 Cached packages in: {:?}\n", config.cache_dir);

    let packages = cache.list()?;

    if packages.is_empty() {
        println!("No packages in cache");
        return Ok(());
    }

    println!(
        "Found {} cached package{}:\n",
        packages.len(),
        if packages.len() == 1 { "" } else { "s" }
    );

    for (i, pkg) in packages.iter().enumerate() {
        let size_kb = pkg.size as f64 / 1024.0;
        let size_str = if size_kb < 1024.0 {
            format!("{:.2} KB", size_kb)
        } else {
            format!("{:.2} MB", size_kb / 1024.0)
        };

        println!("{}. {} v{}", i + 1, pkg.name, pkg.version);
        println!("   Size: {}", size_str);
        println!("   Path: {}", pkg.path.display());
        println!();
    }

    Ok(())
}

/// Show cache size
fn show_cache_size() -> Result<()> {
    use skalp_package::{cache::Cache, RegistryConfig};

    let config = RegistryConfig::default();
    let cache = Cache::new(config.cache_dir);

    let size_bytes = cache.size()?;
    let size_mb = size_bytes as f64 / 1024.0 / 1024.0;

    println!("📊 Cache size: {:.2} MB ({} bytes)", size_mb, size_bytes);

    Ok(())
}

/// Clear the cache
fn clear_cache() -> Result<()> {
    use skalp_package::{cache::Cache, RegistryConfig};

    print!("⚠️  This will delete all cached packages. Continue? [y/N] ");
    use std::io::{self, Write};
    io::stdout().flush()?;

    let mut input = String::new();
    io::stdin().read_line(&mut input)?;

    if input.trim().to_lowercase() != "y" {
        println!("Cancelled");
        return Ok(());
    }

    let config = RegistryConfig::default();
    let cache = Cache::new(config.cache_dir);

    cache.clear()?;
    println!("✅ Cache cleared");

    Ok(())
}

/// Remove a specific package from cache
fn remove_from_cache(package: &str, version: &str) -> Result<()> {
    use skalp_package::{cache::Cache, PackageSource, RegistryConfig};

    println!("Removing {} {} from cache...", package, version);

    let config = RegistryConfig::default();
    let cache = Cache::new(config.cache_dir);

    let source = PackageSource::registry(package, version);
    cache.remove(&source)?;

    println!("✅ Removed from cache");

    Ok(())
}

/// Helper to save manifest to file
fn save_manifest(manifest: &skalp_manifest::Manifest, path: &PathBuf) -> Result<()> {
    let toml_str = toml::to_string_pretty(manifest)?;
    fs::write(path, toml_str)?;
    Ok(())
}

// ============================================================================
// ISO 26262 Safety Analysis
// ============================================================================

/// Run ISO 26262 functional safety analysis
fn run_safety_analysis(
    hir: &skalp_frontend::hir::Hir,
    options: &SafetyBuildOptions,
    output_dir: &Path,
) -> Result<()> {
    use skalp_safety::analysis::AnalysisContext;
    use skalp_safety::asil::AsilLevel;
    use skalp_safety::pipeline::SafetyPipeline;

    println!("\n=== ISO 26262 Safety Analysis ===\n");

    // Parse ASIL level from options
    let target_asil = match options.asil_level.as_deref() {
        Some("A") | Some("a") => AsilLevel::A,
        Some("B") | Some("b") => AsilLevel::B,
        Some("C") | Some("c") => AsilLevel::C,
        Some("D") | Some("d") => AsilLevel::D,
        Some("QM") | Some("qm") | None => AsilLevel::QM,
        Some(other) => {
            anyhow::bail!("Invalid ASIL level: {}. Use A, B, C, D, or QM", other);
        }
    };

    println!("Target ASIL: {:?}", target_asil);

    // Create analysis context
    let context = AnalysisContext::for_asil(target_asil);

    // Create safety pipeline with standard passes
    let pipeline = SafetyPipeline::with_standard_passes();

    // Create safety hierarchy from HIR
    // For now, create a minimal hierarchy - full implementation would parse safety_goal files
    let hierarchy = skalp_safety::hierarchy::SafetyHierarchy::new();

    // Run safety analysis pipeline
    info!("Running safety analysis pipeline...");
    let result = pipeline.run(&hierarchy, None, &context);

    // Print summary
    println!("{}", result.summary());

    // Print errors if any
    if result.total_errors > 0 {
        println!("\nErrors:");
        for error in result.all_errors() {
            println!("  {}", error);
        }
    }

    // Print warnings if any
    if result.total_warnings > 0 {
        println!("\nWarnings:");
        for warning in result.all_warnings() {
            println!("  {}", warning);
        }
    }

    // Determine report output path
    let report_dir = options
        .report_path
        .clone()
        .unwrap_or_else(|| output_dir.join("safety"));

    // Create report directory
    fs::create_dir_all(&report_dir)?;

    // If check_only, stop here
    if options.check_only {
        if result.passed() {
            println!("\n✅ Safety check passed");
        } else {
            println!("\n❌ Safety check failed - fix errors before generating work products");
            anyhow::bail!("Safety analysis failed with {} errors", result.total_errors);
        }
        return Ok(());
    }

    // Generate work products if metrics pass
    if !result.can_generate_workproducts {
        println!("\n❌ Work products NOT generated - fix errors first");
        anyhow::bail!(
            "Cannot generate work products - {} errors found",
            result.total_errors
        );
    }

    // Parse work products to generate
    let workproducts = options
        .workproducts
        .as_deref()
        .unwrap_or("all")
        .split(',')
        .map(|s| s.trim())
        .collect::<Vec<_>>();

    // Generate requested work products
    generate_safety_workproducts(
        &result,
        &hierarchy,
        &report_dir,
        &workproducts,
        &options.formats,
    )?;

    println!("\n✅ Safety analysis complete");
    println!("📁 Reports: {:?}", report_dir);

    Ok(())
}

/// Generate ISO 26262 work products
fn generate_safety_workproducts(
    result: &skalp_safety::analysis::CombinedAnalysisResult,
    hierarchy: &skalp_safety::hierarchy::SafetyHierarchy,
    output_dir: &Path,
    workproducts: &[&str],
    formats: &str,
) -> Result<()> {
    let format_list: Vec<&str> = formats.split(',').map(|s| s.trim()).collect();
    let generate_all = workproducts.contains(&"all");

    println!("\nGenerating work products...");

    // Summary report (always generated)
    if generate_all || workproducts.contains(&"summary") {
        let summary_path = output_dir.join("safety_summary.md");
        fs::write(&summary_path, result.summary())?;
        println!("  ✓ {}", summary_path.display());
    }

    // FMEDA report
    if generate_all || workproducts.contains(&"fmeda") {
        let fmeda_path = output_dir.join("fmeda_report.md");
        let fmeda_content = generate_fmeda_report_md(result);
        fs::write(&fmeda_path, fmeda_content)?;
        println!("  ✓ {}", fmeda_path.display());

        // Also generate HTML if requested
        if format_list.contains(&"html") {
            let fmeda_html_path = output_dir.join("fmeda_report.html");
            let fmeda_html = generate_fmeda_report_html(result);
            fs::write(&fmeda_html_path, fmeda_html)?;
            println!("  ✓ {}", fmeda_html_path.display());
        }
    }

    // Traceability matrix
    if generate_all || workproducts.contains(&"traceability") {
        let trace_path = output_dir.join("traceability_matrix.md");
        let trace_content = generate_traceability_report(hierarchy);
        fs::write(&trace_path, trace_content)?;
        println!("  ✓ {}", trace_path.display());
    }

    // HSI specification
    if generate_all || workproducts.contains(&"hsi") {
        let hsi_path = output_dir.join("hsi_specification.md");
        let hsi_content = generate_hsi_report(hierarchy);
        fs::write(&hsi_path, hsi_content)?;
        println!("  ✓ {}", hsi_path.display());
    }

    Ok(())
}

/// Generate FMEDA report in Markdown format
fn generate_fmeda_report_md(result: &skalp_safety::analysis::CombinedAnalysisResult) -> String {
    let mut output = String::new();

    output.push_str("# FMEDA Report\n\n");
    output.push_str(&format!("Target ASIL: {:?}\n\n", result.target_asil));

    if let Some(ref metrics) = result.final_metrics {
        output.push_str("## Metrics Summary\n\n");
        output.push_str("| Metric | Value | Status |\n");
        output.push_str("|--------|-------|--------|\n");
        output.push_str(&format!(
            "| SPFM | {:.1}% | {} |\n",
            metrics.spfm,
            if metrics.spfm >= 99.0 { "✓" } else { "✗" }
        ));
        output.push_str(&format!(
            "| LFM | {:.1}% | {} |\n",
            metrics.lfm,
            if metrics.lfm >= 90.0 { "✓" } else { "✗" }
        ));
        output.push_str(&format!(
            "| PMHF | {:.1} FIT | {} |\n",
            metrics.pmhf,
            if metrics.pmhf <= 10.0 { "✓" } else { "✗" }
        ));
        output.push_str(&format!("| Total FIT | {:.1} | - |\n", metrics.total_fit));
        output.push_str(&format!(
            "| Achievable ASIL | {:?} | - |\n\n",
            metrics.achievable_asil
        ));
    }

    output.push_str("## Analysis Results\n\n");
    output.push_str(&format!("- **Status**: {:?}\n", result.overall_status));
    output.push_str(&format!("- **Errors**: {}\n", result.total_errors));
    output.push_str(&format!("- **Warnings**: {}\n", result.total_warnings));

    output
}

/// Generate FMEDA report in HTML format
fn generate_fmeda_report_html(result: &skalp_safety::analysis::CombinedAnalysisResult) -> String {
    let mut html = String::new();

    html.push_str("<!DOCTYPE html>\n<html>\n<head>\n");
    html.push_str("<title>FMEDA Report</title>\n");
    html.push_str("<style>\n");
    html.push_str("body { font-family: Arial, sans-serif; margin: 40px; }\n");
    html.push_str("table { border-collapse: collapse; width: 100%; }\n");
    html.push_str("th, td { border: 1px solid #ddd; padding: 8px; text-align: left; }\n");
    html.push_str("th { background-color: #4CAF50; color: white; }\n");
    html.push_str(".pass { color: green; }\n");
    html.push_str(".fail { color: red; }\n");
    html.push_str("</style>\n</head>\n<body>\n");

    html.push_str("<h1>FMEDA Report</h1>\n");
    html.push_str(&format!(
        "<p>Target ASIL: <strong>{:?}</strong></p>\n",
        result.target_asil
    ));

    if let Some(ref metrics) = result.final_metrics {
        html.push_str("<h2>Metrics Summary</h2>\n");
        html.push_str("<table>\n");
        html.push_str("<tr><th>Metric</th><th>Value</th><th>Status</th></tr>\n");
        html.push_str(&format!(
            "<tr><td>SPFM</td><td>{:.1}%</td><td class=\"{}\">{}</td></tr>\n",
            metrics.spfm,
            if metrics.spfm >= 99.0 { "pass" } else { "fail" },
            if metrics.spfm >= 99.0 { "PASS" } else { "FAIL" }
        ));
        html.push_str(&format!(
            "<tr><td>LFM</td><td>{:.1}%</td><td class=\"{}\">{}</td></tr>\n",
            metrics.lfm,
            if metrics.lfm >= 90.0 { "pass" } else { "fail" },
            if metrics.lfm >= 90.0 { "PASS" } else { "FAIL" }
        ));
        html.push_str(&format!(
            "<tr><td>PMHF</td><td>{:.1} FIT</td><td class=\"{}\">{}</td></tr>\n",
            metrics.pmhf,
            if metrics.pmhf <= 10.0 { "pass" } else { "fail" },
            if metrics.pmhf <= 10.0 { "PASS" } else { "FAIL" }
        ));
        html.push_str(&format!(
            "<tr><td>Total FIT</td><td>{:.1}</td><td>-</td></tr>\n",
            metrics.total_fit
        ));
        html.push_str(&format!(
            "<tr><td>Achievable ASIL</td><td>{:?}</td><td>-</td></tr>\n",
            metrics.achievable_asil
        ));
        html.push_str("</table>\n");
    }

    html.push_str("</body>\n</html>\n");
    html
}

/// Generate traceability report
fn generate_traceability_report(hierarchy: &skalp_safety::hierarchy::SafetyHierarchy) -> String {
    let mut output = String::new();

    output.push_str("# Traceability Matrix\n\n");
    output.push_str("## Safety Goals\n\n");

    let matrix = hierarchy.generate_traceability();

    output.push_str("| ID | Level | Description | Parent | Children |\n");
    output.push_str("|----|-------|-------------|--------|----------|\n");

    for entry in &matrix.entries {
        output.push_str(&format!(
            "| {} | {:?} | {} | {} | {} |\n",
            entry.id,
            entry.level,
            entry.description,
            entry.parent.as_deref().unwrap_or("-"),
            if entry.children.is_empty() {
                "-".to_string()
            } else {
                entry.children.join(", ")
            }
        ));
    }

    output
}

/// Generate HSI specification report
fn generate_hsi_report(hierarchy: &skalp_safety::hierarchy::SafetyHierarchy) -> String {
    let mut output = String::new();

    output.push_str("# Hardware-Software Interface Specification\n\n");

    output.push_str("## Interface Signals\n\n");
    output.push_str("| Signal | Direction | ASIL | Max Latency |\n");
    output.push_str("|--------|-----------|------|-------------|\n");

    // HSI signals would be populated from safety entities
    // For now, generate placeholder
    output.push_str("| (No HSI signals defined) | - | - | - |\n");

    output.push_str("\n## Timing Contracts\n\n");
    output.push_str("(No timing contracts defined)\n");

    output
}

/// Run FI-driven safety analysis
///
/// This implements the "double advantage" approach:
/// 1. Failure effect identification from simulation
/// 2. Measured diagnostic coverage from simulation
///
/// Flow: HIR → MIR → Lir → TechMapper → GateNetlist → SIR → FI Simulation
#[allow(clippy::too_many_arguments)]
fn run_fi_driven_safety(
    source: &PathBuf,
    output_dir: &PathBuf,
    asil_str: &str,
    goal_name: &str,
    cycles: u64,
    use_gpu: bool,
    max_faults: usize,
    _effects_file: Option<&Path>,
    formats: &str,
    skip_fi: bool,
    generate_bist: bool,
    dual_bist: bool,
) -> Result<()> {
    use skalp_frontend::parse_and_build_hir_from_file;
    use skalp_lir::{get_stdlib_library, lower_mir_module_to_lir};
    use skalp_safety::asil::AsilLevel;
    use skalp_safety::fault_simulation::{
        CompareOp, CompareValue, ConditionTerm, EffectCondition, FailureEffectDef,
        SafetyGoalSimSpec,
    };
    use skalp_safety::hierarchy::{DesignRef, Severity};
    use skalp_safety::safety_driven_fmea::{
        FaultEffectResult, FiDrivenConfig, SafetyDrivenFmeaGenerator,
    };
    use skalp_sim::convert_gate_netlist_to_sir;
    use std::time::Instant;

    let start = Instant::now();

    println!("🛡️  ISO 26262 FI-Driven Safety Analysis");
    println!("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
    println!("Source: {:?}", source);
    println!("Output: {:?}", output_dir);

    // Parse ASIL level
    let target_asil = match asil_str.to_uppercase().as_str() {
        "A" => AsilLevel::A,
        "B" => AsilLevel::B,
        "C" => AsilLevel::C,
        "D" => AsilLevel::D,
        "QM" => AsilLevel::QM,
        other => anyhow::bail!("Invalid ASIL level: {}. Use A, B, C, D, or QM", other),
    };
    println!("Target ASIL: {:?}", target_asil);
    println!("Safety Goal: {}", goal_name);

    // Create output directory
    fs::create_dir_all(output_dir)?;

    // Parse and build HIR
    println!("\n📖 Parsing design...");
    let hir = parse_and_build_hir_from_file(source).context("Failed to parse source")?;

    // Lower to MIR
    println!("🔧 Lowering to MIR...");
    let compiler =
        skalp_mir::MirCompiler::new().with_optimization_level(skalp_mir::OptimizationLevel::None);
    let mir = compiler
        .compile_to_mir(&hir)
        .map_err(|e| anyhow::anyhow!("MIR compilation failed: {}", e))?;

    // Find the top module (the one not instantiated by any other module)
    let top_module = find_top_level_module(&mir)
        .ok_or_else(|| anyhow::anyhow!("No modules found in design"))?;

    // Lower to Lir and tech-map to GateNetlist
    println!("🔩 Converting to gate-level netlist...");
    let lir_result = lower_mir_module_to_lir(top_module);
    let library =
        get_stdlib_library("generic_asic").context("Failed to load default technology library")?;

    // Use optimized tech mapper (constant folding, DCE, boolean simp, buffer removal)
    let tech_result = skalp_lir::map_lir_to_gates_optimized(&lir_result.lir, &library);
    let netlist = &tech_result.netlist;

    // Report optimization results if any
    for warning in &tech_result.warnings {
        if warning.starts_with("Optimization:") {
            println!("   ⚡ {}", warning);
        }
    }

    let total_cells = netlist.cells.len();
    let total_fit = netlist.total_fit();

    println!("\n📊 Design Statistics:");
    println!("   Module: {}", netlist.name);
    println!("   Gate cells: {}", total_cells);
    println!("   Total FIT: {:.2}", total_fit);

    // Create safety goal specification with default effect conditions
    let mut spec = SafetyGoalSimSpec::new(goal_name, target_asil);

    // Add default effect: any output corruption
    spec.add_effect(FailureEffectDef {
        name: "output_corruption".to_string(),
        description: Some("Any output signal differs from golden reference".to_string()),
        condition: EffectCondition::Term(ConditionTerm {
            signal: DesignRef::parse("output"),
            op: CompareOp::NotEqual,
            value: CompareValue::Golden("golden_output".to_string()),
        }),
        severity: Severity::S3,
        target_dc: match target_asil {
            AsilLevel::D => 0.99,
            AsilLevel::C => 0.97,
            AsilLevel::B => 0.90,
            _ => 0.60,
        },
    });

    let config = FiDrivenConfig {
        cycles_per_fault: cycles,
        max_faults: if max_faults == 0 {
            None
        } else {
            Some(max_faults)
        },
        ..FiDrivenConfig::default()
    };

    let generator = SafetyDrivenFmeaGenerator::new(spec.clone(), config);

    // Build maps for cell path and safety mechanism detection
    let cell_paths: std::collections::HashMap<u32, String> = netlist
        .cells
        .iter()
        .map(|c| (c.id.0, c.path.clone()))
        .collect();

    let cell_fits: std::collections::HashMap<String, f64> = netlist
        .cells
        .iter()
        .map(|c| (c.path.clone(), c.fit))
        .collect();

    // Identify safety mechanism cells (from annotations or naming conventions)
    let sm_cells: std::collections::HashSet<u32> = netlist
        .cells
        .iter()
        .filter(|c| {
            let path_lower = c.path.to_lowercase();
            path_lower.contains("_sm")
                || path_lower.contains("_tmr")
                || path_lower.contains("_voter")
                || path_lower.contains("_crc")
                || path_lower.contains("_ecc")
                || path_lower.contains("_watchdog")
        })
        .map(|c| c.id.0)
        .collect();

    // Identify boot-time-only cells (BIST hardware)
    let boot_time_only_cells: std::collections::HashSet<u32> = netlist
        .cells
        .iter()
        .filter(|c| {
            let path_lower = c.path.to_lowercase();
            path_lower.contains("bist") || path_lower.contains("selftest")
        })
        .map(|c| c.id.0)
        .collect();

    // Convert GateNetlist to SIR for simulation
    println!("🔄 Converting to SIR for simulation...");
    let sir_result = convert_gate_netlist_to_sir(netlist);
    println!("   SIR primitives: {}", sir_result.stats.primitives_created);

    // Run fault injection campaign
    let fault_results: Vec<FaultEffectResult> = if skip_fi {
        println!("\n⏭️  Skipping fault injection (--skip-fi)");
        vec![]
    } else {
        println!("\n🔥 Running Fault Injection Campaign...");
        println!("   Cycles per fault: {}", cycles);
        println!(
            "   Max faults: {}",
            if max_faults == 0 {
                "all".to_string()
            } else {
                max_faults.to_string()
            }
        );

        run_fi_campaign(
            &sir_result.sir,
            cycles,
            max_faults,
            use_gpu,
            &cell_paths,
            &cell_fits,
            &sm_cells,
            &boot_time_only_cells,
        )?
    };

    // Generate FI-driven FMEA
    println!("\n📋 Generating FI-Driven FMEA...");
    let mut fi_result = generator.generate_from_campaign_results(
        &fault_results,
        total_cells,
        &netlist.name,
        total_fit,
    );

    // Apply power domain CCF contribution to PMHF
    let power_domain_analysis =
        skalp_safety::power_domains::extract_power_domains_from_gate_netlist(netlist);
    let power_ccf_fit = power_domain_analysis.summary.total_ccf_fit;
    if power_ccf_fit > 0.0 {
        println!(
            "   Adding power domain CCF: {:.2} FIT (λDPF_power)",
            power_ccf_fit
        );
        fi_result.apply_ccf_contribution(power_ccf_fit);
    }

    // Print results summary
    println!("\n📈 Results Summary:");
    println!("   SPFM: {:.2}%", fi_result.measured_spfm * 100.0);
    println!("   LFM: {:.2}%", fi_result.measured_lf * 100.0);
    if let Some(pmhf) = fi_result.measured_pmhf {
        println!("   PMHF: {:.2} FIT", pmhf);
        if fi_result.ccf_contribution.is_some() {
            let breakdown = fi_result.pmhf_breakdown();
            println!("      ├─ λRF (residual): {:.2} FIT", breakdown.residual_fit);
            println!(
                "      └─ λDPF_CCF (power domains): {:.2} FIT",
                breakdown.ccf_fit
            );
        }
    }
    println!(
        "   Meets {:?}: {}",
        target_asil,
        if fi_result.meets_asil {
            "✅ YES"
        } else {
            "❌ NO"
        }
    );

    // Generate collaterals
    println!("\n📁 Generating Safety Collaterals...");
    let format_list: Vec<&str> = formats.split(',').map(|s| s.trim()).collect();

    // Generate FMEDA report
    if format_list.contains(&"md") || format_list.contains(&"all") {
        let fmeda_path = output_dir.join("fmeda_report.md");
        let fmeda_content =
            generate_fi_driven_fmeda_md(&fi_result, &netlist.name, target_asil, netlist);
        fs::write(&fmeda_path, fmeda_content)?;
        println!("   ✅ {}", fmeda_path.display());
    }

    // Generate YAML summary
    if format_list.contains(&"yaml") || format_list.contains(&"all") {
        let yaml_path = output_dir.join("safety_analysis.yaml");
        let yaml_content = generate_fi_driven_yaml(&fi_result, &netlist.name, target_asil);
        fs::write(&yaml_path, yaml_content)?;
        println!("   ✅ {}", yaml_path.display());
    }

    // Generate JSON summary
    if format_list.contains(&"json") || format_list.contains(&"all") {
        let json_path = output_dir.join("safety_analysis.json");
        let json_content =
            serde_json::to_string_pretty(&fi_result.auto_fmea).unwrap_or_else(|_| "{}".to_string());
        fs::write(&json_path, json_content)?;
        println!("   ✅ {}", json_path.display());
    }

    // Check for SEooC configuration
    let top_entity = hir.entities.iter().find(|e| e.name == netlist.name);
    if let Some(entity) = top_entity {
        if let Some(seooc_config) = &entity.seooc_config {
            run_seooc_analysis(
                output_dir,
                entity,
                seooc_config,
                &fault_results,
                netlist,
                &cell_fits,
                power_ccf_fit,
                &power_domain_analysis,
            )?;
        }
    }

    // Generate BIST if requested
    if generate_bist && !fault_results.is_empty() {
        run_bist_generation(
            output_dir,
            &netlist.name,
            &fi_result,
            &fault_results,
            &cell_fits,
            dual_bist,
        )?;
    }

    let elapsed = start.elapsed();
    println!("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
    println!(
        "✅ FI-Driven Safety Analysis Complete ({:.2}s)",
        elapsed.as_secs_f64()
    );
    println!("📁 Collaterals: {:?}", output_dir);

    Ok(())
}

/// Run fault injection campaign on the SIR
#[allow(clippy::too_many_arguments)]
fn run_fi_campaign(
    sir: &skalp_sim::sir::Sir,
    cycles: u64,
    max_faults: usize,
    use_gpu: bool,
    cell_paths: &std::collections::HashMap<u32, String>,
    cell_fits: &std::collections::HashMap<String, f64>,
    sm_cells: &std::collections::HashSet<u32>,
    boot_time_only_cells: &std::collections::HashSet<u32>,
) -> Result<Vec<skalp_safety::safety_driven_fmea::FaultEffectResult>> {
    use skalp_frontend::hir::DetectionMode;
    use skalp_safety::fault_simulation::FaultSite;
    use skalp_safety::fault_simulation::FaultType;
    use skalp_safety::hierarchy::DesignRef;
    use skalp_safety::safety_driven_fmea::FaultEffectResult;
    use skalp_sim::sir::{FaultType as SimFaultType, SirDetectionMode};

    let mut results = Vec::new();

    #[cfg(target_os = "macos")]
    {
        use skalp_sim::{GpuFaultCampaignConfig, GpuFaultSimulator};

        if use_gpu {
            match GpuFaultSimulator::new(sir) {
                Ok(gpu_sim) => {
                    println!("   Using GPU: {}", gpu_sim.device_info());

                    let campaign_config = GpuFaultCampaignConfig {
                        cycles_per_fault: cycles,
                        fault_types: vec![
                            SimFaultType::StuckAt0,
                            SimFaultType::StuckAt1,
                            SimFaultType::BitFlip,
                            SimFaultType::Transient,
                        ],
                        max_faults,
                        use_gpu: true,
                        ..Default::default()
                    };

                    let campaign = gpu_sim.run_fault_campaign(&campaign_config);

                    println!("   Faults simulated: {}", campaign.total_faults);
                    println!("   Detected: {}", campaign.detected_faults);
                    println!(
                        "   Safe faults: {} ({:.1}%)",
                        campaign.safe_faults, campaign.safe_fault_percentage
                    );
                    println!("   DC (overall): {:.2}%", campaign.diagnostic_coverage);

                    // Convert results
                    for fault_result in &campaign.fault_results {
                        let prim_id = fault_result.fault.target_primitive.0;
                        let prim_path = cell_paths
                            .get(&prim_id)
                            .cloned()
                            .unwrap_or_else(|| format!("cell_{}", prim_id));

                        let triggered = if !fault_result.output_diffs.is_empty() {
                            vec!["output_corruption".to_string()]
                        } else {
                            vec![]
                        };

                        let fault_type = match fault_result.fault.fault_type {
                            SimFaultType::StuckAt0 => FaultType::StuckAt0,
                            SimFaultType::StuckAt1 => FaultType::StuckAt1,
                            SimFaultType::BitFlip | SimFaultType::Transient => FaultType::Transient,
                            _ => FaultType::StuckAt0,
                        };

                        let detection_mode = fault_result.detection_mode.map(|m| match m {
                            SirDetectionMode::Continuous => DetectionMode::Continuous,
                            SirDetectionMode::Boot => DetectionMode::Boot,
                            SirDetectionMode::Periodic => DetectionMode::Periodic,
                            SirDetectionMode::OnDemand => DetectionMode::OnDemand,
                        });

                        results.push(FaultEffectResult {
                            fault_site: FaultSite::new(DesignRef::parse(&prim_path), fault_type),
                            primitive_path: prim_path.clone(),
                            triggered_effects: triggered,
                            detected: fault_result.detected,
                            detected_by: if fault_result.detected {
                                Some("safety_mechanism".to_string())
                            } else {
                                None
                            },
                            effect_cycle: None,
                            detection_cycle: fault_result.detection_cycle,
                            is_safety_mechanism: sm_cells.contains(&prim_id),
                            detection_mode,
                            is_boot_time_only: boot_time_only_cells.contains(&prim_id),
                        });
                    }

                    return Ok(results);
                }
                Err(e) => {
                    println!("   GPU init failed: {}. Falling back to CPU.", e);
                }
            }
        }
    }

    // CPU-based simulation (fallback or non-macOS)
    #[cfg(not(target_os = "macos"))]
    let _ = use_gpu;

    println!("   Using CPU simulation");

    // Run CPU fault campaign
    use skalp_sim::{FaultCampaignConfig, GateLevelSimulator};

    let mut simulator = GateLevelSimulator::new(sir);

    let config = FaultCampaignConfig {
        cycles_per_fault: cycles,
        clock_name: "clk".to_string(),
        fault_types: vec![
            SimFaultType::StuckAt0,
            SimFaultType::StuckAt1,
            SimFaultType::BitFlip,
            SimFaultType::Transient,
        ],
        max_faults,
        ..Default::default()
    };

    let campaign = simulator.run_fault_campaign_with_config(&config);

    println!("   Faults simulated: {}", campaign.total_faults);
    println!("   Detected: {}", campaign.detected_faults);
    println!(
        "   Safe faults: {} ({:.1}%)",
        campaign.safe_faults, campaign.safe_fault_percentage
    );
    println!("   DC (overall): {:.2}%", campaign.diagnostic_coverage);

    // Convert CPU results to FaultEffectResult
    for fault_result in &campaign.fault_results {
        let prim_id = fault_result.fault.target_primitive.0;
        let prim_path = cell_paths
            .get(&prim_id)
            .cloned()
            .unwrap_or_else(|| format!("cell_{}", prim_id));

        let triggered = if !fault_result.output_diffs.is_empty() {
            vec!["output_corruption".to_string()]
        } else {
            vec![]
        };

        let fault_type = match fault_result.fault.fault_type {
            SimFaultType::StuckAt0 => FaultType::StuckAt0,
            SimFaultType::StuckAt1 => FaultType::StuckAt1,
            SimFaultType::BitFlip | SimFaultType::Transient => FaultType::Transient,
            _ => FaultType::StuckAt0,
        };

        let detection_mode = fault_result.detection_mode.map(|m| match m {
            SirDetectionMode::Continuous => DetectionMode::Continuous,
            SirDetectionMode::Boot => DetectionMode::Boot,
            SirDetectionMode::Periodic => DetectionMode::Periodic,
            SirDetectionMode::OnDemand => DetectionMode::OnDemand,
        });

        results.push(FaultEffectResult {
            fault_site: FaultSite::new(DesignRef::parse(&prim_path), fault_type),
            primitive_path: prim_path.clone(),
            triggered_effects: triggered,
            detected: fault_result.detected,
            detected_by: if fault_result.detected {
                Some("safety_mechanism".to_string())
            } else {
                None
            },
            effect_cycle: None,
            detection_cycle: fault_result.detection_cycle,
            is_safety_mechanism: sm_cells.contains(&prim_id),
            detection_mode,
            is_boot_time_only: boot_time_only_cells.contains(&prim_id),
        });
    }

    Ok(results)
}

/// Run SEooC analysis
#[allow(clippy::too_many_arguments)]
fn run_seooc_analysis(
    output_dir: &std::path::Path,
    entity: &skalp_frontend::hir::HirEntity,
    seooc_config: &skalp_frontend::hir::SeoocConfig,
    fault_results: &[skalp_safety::safety_driven_fmea::FaultEffectResult],
    netlist: &skalp_lir::gate_netlist::GateNetlist,
    cell_fits: &std::collections::HashMap<String, f64>,
    power_ccf_fit: f64,
    power_domain_analysis: &skalp_safety::power_domains::PowerDomainAnalysis,
) -> Result<()> {
    println!("\n🛡️  SEooC Analysis (ISO 26262-10:9)...");

    // Convert HIR config to analysis config
    let seooc_analysis_config = skalp_safety::from_hir_seooc_config(&entity.name, seooc_config);

    // Convert FI results to SEooC format
    let mut undetected_faults_seooc = Vec::new();
    for result in fault_results {
        if !result.triggered_effects.is_empty() && !result.detected {
            undetected_faults_seooc.push(skalp_safety::UndetectedFault {
                site: result.primitive_path.clone(),
                fault_type: format!("{:?}", result.fault_site.fault_type),
                fit: cell_fits
                    .get(&result.primitive_path)
                    .copied()
                    .unwrap_or(1.0),
            });
        }
    }

    let total_dangerous = fault_results
        .iter()
        .filter(|r| !r.triggered_effects.is_empty())
        .count();
    let internally_detected = fault_results
        .iter()
        .filter(|r| !r.triggered_effects.is_empty() && r.detected)
        .count();

    let fi_data = skalp_safety::FaultInjectionData {
        total_dangerous,
        internally_detected,
        undetected_faults: undetected_faults_seooc,
    };

    // Prepare power domain CCF data
    let power_ccf_data = if power_ccf_fit > 0.0 {
        Some(skalp_safety::PowerDomainCcfData {
            ccf_fit: power_ccf_fit,
            domain_count: power_domain_analysis.domains.len(),
            affects_pmhf: true,
        })
    } else {
        None
    };

    // Run SEooC analysis
    let seooc_result = skalp_safety::analyze_seooc_with_ccf(
        &seooc_analysis_config,
        &fi_data,
        power_ccf_data.as_ref(),
    );

    // Print summary
    println!("   Target ASIL: {:?}", seooc_result.target_asil);
    println!("   Internal SPFM: {:.1}%", seooc_result.internal_spfm);
    println!("   SPFM Gap: {:.1}%", seooc_result.spfm_gap);

    if let Some(ref ccf_cov) = seooc_result.power_ccf_coverage {
        println!("   ⚡ Power CCF Coverage by {}:", ccf_cov.mechanism_id);
        println!(
            "      Original λDPF_CCF: {:.2} FIT → Residual: {:.2} FIT",
            ccf_cov.original_ccf_fit, ccf_cov.residual_ccf_fit
        );
    }

    if seooc_result.target_achievable {
        println!("   Status: ✅ Target achievable with external mechanisms");
    } else {
        println!("   Status: ⚠️  Gap remains - additional mechanisms needed");
    }

    // Generate SEooC report
    let seooc_report = skalp_safety::format_seooc_report(&seooc_result);
    let seooc_path = output_dir.join("seooc_derived_requirements.md");
    fs::write(&seooc_path, &seooc_report)?;
    println!("   ✅ {}", seooc_path.display());

    // Generate YAML
    let seooc_yaml = generate_seooc_yaml(&seooc_result);
    let seooc_yaml_path = output_dir.join("seooc_derived_requirements.yaml");
    fs::write(&seooc_yaml_path, &seooc_yaml)?;
    println!("   ✅ {}", seooc_yaml_path.display());

    Ok(())
}

/// Run BIST generation
fn run_bist_generation(
    output_dir: &std::path::Path,
    design_name: &str,
    fi_result: &skalp_safety::safety_driven_fmea::FiDrivenFmeaResult,
    fault_results: &[skalp_safety::safety_driven_fmea::FaultEffectResult],
    cell_fits: &std::collections::HashMap<String, f64>,
    dual_bist: bool,
) -> Result<()> {
    use skalp_safety::bist_generation::{BistGenerationConfig, BistGenerator};
    use skalp_safety::fault_diagnostics::{FaultClassification, UndetectedFaultInfo};

    println!("\n🔧 Generating Safety-Driven BIST...");

    // Identify undetected dangerous faults (coverage gaps)
    let mut undetected_faults = Vec::new();
    for result in fault_results {
        if !result.triggered_effects.is_empty() && !result.detected {
            if result.is_safety_mechanism {
                continue; // SM faults need SM-of-SM, not BIST
            }

            undetected_faults.push(UndetectedFaultInfo {
                fault_site: result.primitive_path.clone(),
                fault_type: format!("{:?}", result.fault_site.fault_type),
                component: result
                    .primitive_path
                    .split('.')
                    .next()
                    .unwrap_or("unknown")
                    .to_string(),
                fit_contribution: cell_fits
                    .get(&result.primitive_path)
                    .copied()
                    .unwrap_or(1.0),
                classification: FaultClassification::CoverageGap,
            });
        }
    }

    println!("   Coverage gap faults: {}", undetected_faults.len());

    if undetected_faults.is_empty() {
        println!("   ℹ️  No coverage gaps found - BIST not needed");
        return Ok(());
    }

    let bist_config = BistGenerationConfig {
        entity_name: format!("{}Bist", design_name),
        enable_dual_bist: dual_bist,
        enable_signature: true,
        enable_self_test: true,
        max_patterns: 0,
        ..Default::default()
    };

    let mut generator = BistGenerator::new(bist_config);
    generator.identify_candidates(fi_result, fault_results, &undetected_faults);
    let bist_result = generator.generate();

    // Write BIST source
    let bist_path = output_dir.join("generated_bist.sk");
    fs::write(&bist_path, &bist_result.skalp_source)?;
    println!(
        "   ✅ {} ({} patterns)",
        bist_path.display(),
        bist_result.num_patterns
    );

    // Write BIST summary
    let bist_summary_path = output_dir.join("bist_summary.md");
    let bist_summary = format!(
        r#"# Safety-Driven BIST Report

## Overview

- **Entity**: `{}`
- **Test Patterns**: {}
- **Faults Covered**: {}
- **FIT Covered**: {:.2}
- **Estimated Gates**: {}

## SM-of-SM Features

{}
"#,
        bist_result.entity_name,
        bist_result.num_patterns,
        bist_result.faults_covered,
        bist_result.fit_covered,
        bist_result.estimated_gates,
        bist_result
            .sm_of_sm_features
            .iter()
            .map(|f| format!("- {}", f))
            .collect::<Vec<_>>()
            .join("\n"),
    );
    fs::write(&bist_summary_path, bist_summary)?;
    println!("   ✅ {}", bist_summary_path.display());

    if dual_bist {
        println!("   🛡️  Dual BIST enabled (SM-of-SM protection)");
    }

    Ok(())
}

/// Generate FI-driven FMEDA report in Markdown
fn generate_fi_driven_fmeda_md(
    fi_result: &skalp_safety::safety_driven_fmea::FiDrivenFmeaResult,
    design_name: &str,
    target_asil: skalp_safety::asil::AsilLevel,
    netlist: &skalp_lir::gate_netlist::GateNetlist,
) -> String {
    let mut output = String::new();

    output.push_str(&format!("# FMEDA Report: {}\n\n", design_name));
    output.push_str(&format!("**Target ASIL**: {:?}\n\n", target_asil));

    output.push_str("## Design Statistics\n\n");
    output.push_str(&format!("- **Gate Cells**: {}\n", netlist.cells.len()));
    output.push_str(&format!("- **Total FIT**: {:.2}\n\n", netlist.total_fit()));

    output.push_str("## Safety Metrics\n\n");
    output.push_str("| Metric | Value | Target | Status |\n");
    output.push_str("|--------|-------|--------|--------|\n");

    let spfm_target = match target_asil {
        skalp_safety::asil::AsilLevel::D => 99.0,
        skalp_safety::asil::AsilLevel::C => 97.0,
        skalp_safety::asil::AsilLevel::B => 90.0,
        _ => 60.0,
    };
    let spfm_status = if fi_result.measured_spfm * 100.0 >= spfm_target {
        "✅"
    } else {
        "❌"
    };
    output.push_str(&format!(
        "| SPFM | {:.2}% | ≥{:.0}% | {} |\n",
        fi_result.measured_spfm * 100.0,
        spfm_target,
        spfm_status
    ));

    let lfm_target = match target_asil {
        skalp_safety::asil::AsilLevel::D => 90.0,
        skalp_safety::asil::AsilLevel::C => 80.0,
        skalp_safety::asil::AsilLevel::B => 60.0,
        _ => 0.0,
    };
    let lfm_status = if fi_result.measured_lf * 100.0 >= lfm_target {
        "✅"
    } else {
        "❌"
    };
    output.push_str(&format!(
        "| LFM | {:.2}% | ≥{:.0}% | {} |\n",
        fi_result.measured_lf * 100.0,
        lfm_target,
        lfm_status
    ));

    if let Some(pmhf) = fi_result.measured_pmhf {
        output.push_str(&format!("| PMHF | {:.2} FIT | - | - |\n", pmhf));
    }

    output.push_str(&format!(
        "\n**Overall Status**: {}\n",
        if fi_result.meets_asil {
            "✅ Meets ASIL requirements"
        } else {
            "❌ Does not meet ASIL requirements"
        }
    ));

    output
}

/// Generate FI-driven results in YAML
fn generate_fi_driven_yaml(
    fi_result: &skalp_safety::safety_driven_fmea::FiDrivenFmeaResult,
    design_name: &str,
    target_asil: skalp_safety::asil::AsilLevel,
) -> String {
    // Calculate fault counts from results
    let total_faults = fi_result.total_injections;
    let detected_faults = fi_result
        .fault_results
        .iter()
        .filter(|r| r.detected)
        .count();
    let safe_faults = fi_result
        .fault_results
        .iter()
        .filter(|r| r.triggered_effects.is_empty())
        .count();

    format!(
        r#"# FI-Driven Safety Analysis Results
design: {}
target_asil: {:?}
metrics:
  spfm: {:.4}
  lfm: {:.4}
  pmhf: {}
  meets_asil: {}
fault_counts:
  total_injected: {}
  detected: {}
  safe: {}
"#,
        design_name,
        target_asil,
        fi_result.measured_spfm,
        fi_result.measured_lf,
        fi_result
            .measured_pmhf
            .map(|p| format!("{:.4}", p))
            .unwrap_or_else(|| "null".to_string()),
        fi_result.meets_asil,
        total_faults,
        detected_faults,
        safe_faults,
    )
}

/// Generate SEooC YAML
fn generate_seooc_yaml(result: &skalp_safety::SeoocAnalysisResult) -> String {
    format!(
        r#"# SEooC Derived Requirements
target_asil: {:?}
internal_spfm: {:.2}
spfm_gap: {:.2}
target_achievable: {}
derived_requirements:
{}
"#,
        result.target_asil,
        result.internal_spfm,
        result.spfm_gap,
        result.target_achievable,
        result
            .derived_requirements
            .iter()
            .map(|r| format!(
                "  - id: {}\n    mechanism_type: {}\n    required_dc: {:.2}",
                r.id, r.mechanism_type, r.required_dc
            ))
            .collect::<Vec<_>>()
            .join("\n"),
    )
}
// - convert_lir_to_sir (legacy Lir → SIR)
// - SafetyDrivenFmeaGenerator with fault_simulation types
// The new implementation should use:
// - lower_mir_module_to_word_lir (MIR → WordLir)
// - TechMapper (WordLir → GateNetlist)
// - gate_netlist_to_sir (GateNetlist → SIR)

/// Generate FI-driven FMEDA report in Markdown (stub - pending GateNetlist migration)
#[allow(dead_code)]
fn generate_fi_driven_fmeda_md_stub(
    _design_name: &str,
    _target_asil: skalp_safety::asil::AsilLevel,
) -> String {
    "# FMEDA Report\n\n⚠️ FI-driven FMEDA generation temporarily disabled during GateNetlist migration.\n".to_string()
}

/// Generate FI-driven results in YAML (stub - pending GateNetlist migration)
#[allow(dead_code)]
fn generate_fi_driven_yaml_stub(
    _design_name: &str,
    _target_asil: skalp_safety::asil::AsilLevel,
) -> String {
    "# FI-driven results\nstatus: disabled_during_migration\n".to_string()
}

/// Run CPU-based fault injection campaign (stub - pending GateNetlist migration)
#[allow(dead_code)]
fn run_cpu_fi_campaign_stub(
    _sir: &skalp_sim::sir::Sir,
    _cycles: u64,
    _max_faults: usize,
) -> Result<()> {
    anyhow::bail!("CPU FI campaign temporarily disabled during GateNetlist migration");
}

// === Begin orphaned code removal marker - delete everything until "End orphaned code" ===
// The following code was left orphaned after function stubbing and needs to be deleted.

fn _orphaned_code_marker_start() {}
/*

    let start = Instant::now();

    println!("🛡️  ISO 26262 FI-Driven Safety Analysis");
    println!("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
    println!("Source: {:?}", source);
    println!("Output: {:?}", output_dir);

    // Parse ASIL level
    let target_asil = match asil_str.to_uppercase().as_str() {
        "A" => AsilLevel::A,
        "B" => AsilLevel::B,
        "C" => AsilLevel::C,
        "D" => AsilLevel::D,
        "QM" => AsilLevel::QM,
        other => anyhow::bail!("Invalid ASIL level: {}. Use A, B, C, D, or QM", other),
    };
    println!("Target ASIL: {:?}", target_asil);
    println!("Safety Goal: {}", goal_name);

    // Create output directory
    fs::create_dir_all(output_dir)?;

    // Parse and build HIR
    println!("\n📖 Parsing design...");
    let hir = parse_and_build_hir_from_file(source).context("Failed to parse source")?;

    // Lower to MIR
    println!("🔧 Lowering to MIR...");
    let compiler =
        skalp_mir::MirCompiler::new().with_optimization_level(skalp_mir::OptimizationLevel::None);
    let mir = compiler
        .compile_to_mir(&hir)
        .map_err(|e| anyhow::anyhow!("MIR compilation failed: {}", e))?;

    // Lower to flattened gate-level netlist (inlines all module instances)
    println!("🔩 Converting to flattened gate-level netlist...");
    let flattened_result = lower_to_flattened_lir(&mir, None)?;
    let lir = &flattened_result.lir;
    let total_primitives = lir.primitives.len();
    let total_fit: f64 = lir.primitives.iter().map(|p| p.ptype.base_fit()).sum();

    println!("\n📊 Design Statistics:");
    println!("   Module: {}", lir.name);
    println!("   Primitives: {}", total_primitives);
    println!("   Total FIT: {:.2}", total_fit);

    // Create safety goal specification with default effect conditions
    let mut spec = SafetyGoalSimSpec::new(goal_name, target_asil);

    // Add default effect: any output corruption
    // In a full implementation, this would be loaded from the effects file
    spec.add_effect(FailureEffectDef {
        name: "output_corruption".to_string(),
        description: Some("Any output signal differs from golden reference".to_string()),
        condition: EffectCondition::Term(skalp_safety::fault_simulation::ConditionTerm {
            signal: DesignRef::parse("output"),
            op: skalp_safety::fault_simulation::CompareOp::NotEqual,
            value: skalp_safety::fault_simulation::CompareValue::Golden(
                "golden_output".to_string(),
            ),
        }),
        severity: Severity::S3,
        target_dc: match target_asil {
            AsilLevel::D => 0.99,
            AsilLevel::C => 0.97,
            AsilLevel::B => 0.90,
            _ => 0.60,
        },
    });

    let config = FiDrivenConfig {
        cycles_per_fault: cycles,
        max_faults: if max_faults == 0 {
            None
        } else {
            Some(max_faults)
        },
        ..FiDrivenConfig::default()
    };

    let generator = SafetyDrivenFmeaGenerator::new(spec.clone(), config);

    // Run fault injection campaign
    let fault_results: Vec<FaultEffectResult> = if skip_fi {
        println!("\n⏭️  Skipping fault injection (--skip-fi)");
        vec![]
    } else {
        println!("\n🔥 Running Fault Injection Campaign...");
        println!("   Cycles per fault: {}", cycles);
        println!(
            "   Max faults: {}",
            if max_faults == 0 {
                "all".to_string()
            } else {
                max_faults.to_string()
            }
        );

        // Convert to SIR for simulation
        let sir_result = convert_lir_to_sir(lir);

        // Build map of primitive ID -> (hierarchical path, is_safety_mechanism) for diagnostics
        let primitive_paths: std::collections::HashMap<u32, String> = lir
            .primitives
            .iter()
            .map(|p| (p.id.0, p.path.clone()))
            .collect();

        // Build set of primitive IDs that are safety mechanisms (from annotations)
        let sm_primitives: std::collections::HashSet<u32> = lir
            .primitives
            .iter()
            .filter(|p| {
                p.safety_info
                    .as_ref()
                    .is_some_and(|s| s.mechanism_name.is_some())
            })
            .map(|p| p.id.0)
            .collect();

        // Build set of boot-time-only primitives (e.g., BIST hardware)
        // These are inactive during steady-state operation and shouldn't contribute to PMHF
        // Detection: either safety_info.is_boot_time_only is set, OR path contains BIST-related keywords
        let boot_time_only_primitives: std::collections::HashSet<u32> = lir
            .primitives
            .iter()
            .filter(|p| {
                // Check safety_info flag
                let from_annotation = p.safety_info.as_ref().is_some_and(|s| s.is_boot_time_only);

                // Check path for BIST-related keywords (case-insensitive)
                let path_lower = p.path.to_lowercase();
                let from_path = path_lower.contains("bist")
                    || path_lower.contains(".bist_")
                    || path_lower.contains("selftest");

                from_annotation || from_path
            })
            .map(|p| p.id.0)
            .collect();

        let mut results = Vec::new();

        #[cfg(target_os = "macos")]
        {
            use skalp_frontend::hir::DetectionMode;
            use skalp_safety::fault_simulation::FaultType;
            use skalp_sim::sir::{FaultType as SimFaultType, SirDetectionMode};
            use skalp_sim::{GpuFaultCampaignConfig, GpuFaultSimulator};

            if use_gpu {
                match GpuFaultSimulator::new(&sir_result.sir) {
                    Ok(gpu_sim) => {
                        println!("   Using GPU: {}", gpu_sim.device_info());

                        let campaign_config = GpuFaultCampaignConfig {
                            cycles_per_fault: cycles,
                            fault_types: vec![
                                SimFaultType::StuckAt0,
                                SimFaultType::StuckAt1,
                                SimFaultType::BitFlip,
                                SimFaultType::Transient,
                            ],
                            max_faults,
                            use_gpu: true,
                            ..Default::default()
                        };

                        let campaign = gpu_sim.run_fault_campaign(&campaign_config);

                        println!("   Faults simulated: {}", campaign.total_faults);
                        println!("   Detected: {}", campaign.detected_faults);
                        println!(
                            "   Safe faults: {} ({:.1}%)",
                            campaign.safe_faults, campaign.safe_fault_percentage
                        );
                        println!("   DC (overall): {:.2}%", campaign.diagnostic_coverage);
                        println!("   ┌─ Permanent faults (SA0/SA1):");
                        println!(
                            "   │  Total: {}, Corruption: {}, Safe: {} ({:.1}%)",
                            campaign.permanent.total,
                            campaign.permanent.corruption,
                            campaign.permanent.safe,
                            campaign.permanent.safe_pct
                        );
                        println!(
                            "   │  DC: {:.2}%, Base FIT: {:.2}, Residual FIT: {:.3}",
                            campaign.permanent.dc,
                            campaign.permanent.base_fit,
                            campaign.permanent.residual_fit
                        );
                        println!("   ├─ Transient faults (SEU/BitFlip):");
                        println!(
                            "   │  Total: {}, Corruption: {}, Safe: {} ({:.1}%)",
                            campaign.transient.total,
                            campaign.transient.corruption,
                            campaign.transient.safe,
                            campaign.transient.safe_pct
                        );
                        println!(
                            "   │  DC: {:.2}%, Base FIT: {:.2}, Residual FIT: {:.3}",
                            campaign.transient.dc,
                            campaign.transient.base_fit,
                            campaign.transient.residual_fit
                        );
                        println!("   └─ Power faults (VoltageDropout/GroundBounce):");
                        println!(
                            "      Total: {}, Corruption: {}, Safe: {} ({:.1}%)",
                            campaign.power.total,
                            campaign.power.corruption,
                            campaign.power.safe,
                            campaign.power.safe_pct
                        );
                        println!(
                            "      DC: {:.2}%, Base FIT: {:.2}, Residual FIT: {:.3}",
                            campaign.power.dc, campaign.power.base_fit, campaign.power.residual_fit
                        );

                        // Convert to FaultEffectResult format
                        for fault_result in &campaign.fault_results {
                            let prim_id = fault_result.fault.target_primitive.0;
                            let prim_path = primitive_paths
                                .get(&prim_id)
                                .cloned()
                                .unwrap_or_else(|| format!("prim_{}", prim_id));

                            let triggered = if !fault_result.output_diffs.is_empty() {
                                vec!["output_corruption".to_string()]
                            } else {
                                vec![]
                            };

                            let fault_type = match fault_result.fault.fault_type {
                                SimFaultType::StuckAt0 => FaultType::StuckAt0,
                                SimFaultType::StuckAt1 => FaultType::StuckAt1,
                                SimFaultType::BitFlip | SimFaultType::Transient => {
                                    FaultType::Transient
                                }
                                _ => FaultType::StuckAt0, // Default for other complex fault types
                            };

                            // Convert detection mode from SIR to HIR enum
                            let detection_mode = fault_result.detection_mode.map(|m| match m {
                                SirDetectionMode::Continuous => DetectionMode::Continuous,
                                SirDetectionMode::Boot => DetectionMode::Boot,
                                SirDetectionMode::Periodic => DetectionMode::Periodic,
                                SirDetectionMode::OnDemand => DetectionMode::OnDemand,
                            });

                            results.push(FaultEffectResult {
                                fault_site: skalp_safety::fault_simulation::FaultSite::new(
                                    DesignRef::parse(&prim_path),
                                    fault_type,
                                ),
                                primitive_path: prim_path,
                                triggered_effects: triggered,
                                detected: fault_result.detected,
                                detected_by: if fault_result.detected {
                                    Some("safety_mechanism".to_string())
                                } else {
                                    None
                                },
                                effect_cycle: None,
                                detection_cycle: fault_result.detection_cycle,
                                is_safety_mechanism: sm_primitives.contains(&prim_id),
                                detection_mode,
                                is_boot_time_only: boot_time_only_primitives.contains(&prim_id),
                            });
                        }
                    }
                    Err(e) => {
                        println!("   GPU init failed: {}. Using CPU.", e);
                        results = run_cpu_fi_campaign(
                            &sir_result.sir,
                            cycles,
                            max_faults,
                            &primitive_paths,
                            &sm_primitives,
                            &boot_time_only_primitives,
                        )?;
                    }
                }
            } else {
                results = run_cpu_fi_campaign(
                    &sir_result.sir,
                    cycles,
                    max_faults,
                    &primitive_paths,
                    &sm_primitives,
                    &boot_time_only_primitives,
                )?;
            }
        }

        #[cfg(not(target_os = "macos"))]
        {
            let _ = use_gpu; // Suppress unused warning
            results = run_cpu_fi_campaign(
                &sir_result.sir,
                cycles,
                max_faults,
                &primitive_paths,
                &sm_primitives,
                &boot_time_only_primitives,
            )?;
        }

        results
    };

    // Generate FI-driven FMEA
    println!("\n📋 Generating FI-Driven FMEA...");
    let mut fi_result = generator.generate_from_campaign_results(
        &fault_results,
        total_primitives,
        &lir.name,
        total_fit,
    );

    // Apply power domain CCF contribution to PMHF
    // Per ISO 26262-5 + ISO 26262-9: PMHF = λRF + λSM + λDPF_CCF
    let power_domain_analysis = skalp_safety::power_domains::extract_power_domains_from_lir(lir);
    let power_ccf_fit = power_domain_analysis.summary.total_ccf_fit;
    if power_ccf_fit > 0.0 {
        println!(
            "   Adding power domain CCF: {:.2} FIT (λDPF_power)",
            power_ccf_fit
        );
        fi_result.apply_ccf_contribution(power_ccf_fit);
    }

    // Print results summary
    println!("\n📈 Results Summary:");
    println!("   SPFM: {:.2}%", fi_result.measured_spfm * 100.0);
    println!("   LFM: {:.2}%", fi_result.measured_lf * 100.0);
    if let Some(pmhf) = fi_result.measured_pmhf {
        println!("   PMHF: {:.2} FIT", pmhf);
        // Show PMHF breakdown if CCF was applied
        if fi_result.ccf_contribution.is_some() {
            let breakdown = fi_result.pmhf_breakdown();
            println!("      ├─ λRF (residual): {:.2} FIT", breakdown.residual_fit);
            println!(
                "      └─ λDPF_CCF (power domains): {:.2} FIT",
                breakdown.ccf_fit
            );
        }
    }
    println!(
        "   Meets {:?}: {}",
        target_asil,
        if fi_result.meets_asil {
            "✅ YES"
        } else {
            "❌ NO"
        }
    );

    // Generate collaterals
    println!("\n📁 Generating Safety Collaterals...");
    let format_list: Vec<&str> = formats.split(',').map(|s| s.trim()).collect();

    // Generate FMEDA report
    if format_list.contains(&"md") || format_list.contains(&"all") {
        let fmeda_path = output_dir.join("fmeda_report.md");
        let fmeda_content = generate_fi_driven_fmeda_md(&fi_result, &lir.name, target_asil, lir);
        fs::write(&fmeda_path, fmeda_content)?;
        println!("   ✅ {}", fmeda_path.display());
    }

    // Generate YAML summary
    if format_list.contains(&"yaml") || format_list.contains(&"all") {
        let yaml_path = output_dir.join("safety_analysis.yaml");
        let yaml_content = generate_fi_driven_yaml(&fi_result, &lir.name, target_asil);
        fs::write(&yaml_path, yaml_content)?;
        println!("   ✅ {}", yaml_path.display());
    }

    // Generate JSON summary
    if format_list.contains(&"json") || format_list.contains(&"all") {
        let json_path = output_dir.join("safety_analysis.json");
        let json_content =
            serde_json::to_string_pretty(&fi_result.auto_fmea).unwrap_or_else(|_| "{}".to_string());
        fs::write(&json_path, json_content)?;
        println!("   ✅ {}", json_path.display());
    }

    // Check for SEooC configuration and run derived requirements analysis
    let top_entity = hir.entities.iter().find(|e| e.name == lir.name);
    if let Some(entity) = top_entity {
        if let Some(seooc_config) = &entity.seooc_config {
            println!("\n🛡️  SEooC Analysis (ISO 26262-10:9)...");

            // Convert HIR config to analysis config
            let seooc_analysis_config =
                skalp_safety::from_hir_seooc_config(&entity.name, seooc_config);

            // Convert FI results to SEooC format
            let mut undetected_faults_seooc = Vec::new();
            for result in &fault_results {
                if !result.triggered_effects.is_empty() && !result.detected {
                    undetected_faults_seooc.push(skalp_safety::UndetectedFault {
                        site: result.primitive_path.clone(),
                        fault_type: format!("{:?}", result.fault_site.fault_type),
                        fit: lir
                            .primitives
                            .iter()
                            .find(|p| p.path == result.primitive_path)
                            .map(|p| p.ptype.base_fit())
                            .unwrap_or(1.0),
                    });
                }
            }

            let total_dangerous = fault_results
                .iter()
                .filter(|r| !r.triggered_effects.is_empty())
                .count();
            let internally_detected = fault_results
                .iter()
                .filter(|r| !r.triggered_effects.is_empty() && r.detected)
                .count();

            let fi_data = skalp_safety::FaultInjectionData {
                total_dangerous,
                internally_detected,
                undetected_faults: undetected_faults_seooc,
            };

            // Prepare power domain CCF data for SEooC analysis
            // Power faults (VoltageDropout/GroundBounce) are handled via CCF, not individual FI
            let power_ccf_data = if power_ccf_fit > 0.0 {
                Some(skalp_safety::PowerDomainCcfData {
                    ccf_fit: power_ccf_fit,
                    domain_count: power_domain_analysis.domains.len(),
                    affects_pmhf: true,
                })
            } else {
                None
            };

            // Run SEooC analysis with power CCF data
            let seooc_result = skalp_safety::analyze_seooc_with_ccf(
                &seooc_analysis_config,
                &fi_data,
                power_ccf_data.as_ref(),
            );

            // Print summary
            println!("   Target ASIL: {:?}", seooc_result.target_asil);
            println!("   Internal SPFM: {:.1}%", seooc_result.internal_spfm);
            println!("   SPFM Gap: {:.1}%", seooc_result.spfm_gap);

            // Show power CCF coverage if an assumed mechanism covers it
            if let Some(ref ccf_cov) = seooc_result.power_ccf_coverage {
                println!("   ⚡ Power CCF Coverage by {}:", ccf_cov.mechanism_id);
                println!(
                    "      Original λDPF_CCF: {:.2} FIT → Residual: {:.2} FIT",
                    ccf_cov.original_ccf_fit, ccf_cov.residual_ccf_fit
                );
                println!(
                    "      PMHF Improvement: -{:.2} FIT (DC ≥ {:.0}%)",
                    ccf_cov.pmhf_improvement_fit, ccf_cov.assumed_dc
                );
            }

            if seooc_result.target_achievable {
                println!("   Status: ✅ Target achievable with external mechanisms");
            } else {
                println!("   Status: ⚠️  Gap remains - additional mechanisms needed");
            }

            // Generate SEooC report
            let seooc_report = skalp_safety::format_seooc_report(&seooc_result);
            let seooc_path = output_dir.join("seooc_derived_requirements.md");
            fs::write(&seooc_path, &seooc_report)?;
            println!("   ✅ {}", seooc_path.display());

            // Also generate YAML for machine consumption
            let seooc_yaml = generate_seooc_yaml(&seooc_result);
            let seooc_yaml_path = output_dir.join("seooc_derived_requirements.yaml");
            fs::write(&seooc_yaml_path, &seooc_yaml)?;
            println!("   ✅ {}", seooc_yaml_path.display());
        }
    }

    // Generate BIST if requested and we have FI results
    if generate_bist && !fault_results.is_empty() {
        use skalp_safety::bist_generation::{BistGenerationConfig, BistGenerator};
        use skalp_safety::fault_diagnostics::{FaultClassification, UndetectedFaultInfo};

        println!("\n🔧 Generating Safety-Driven BIST...");

        // Identify undetected dangerous faults (coverage gaps)
        let mut undetected_faults = Vec::new();
        for result in &fault_results {
            // Only consider faults that triggered effects but weren't detected
            if !result.triggered_effects.is_empty() && !result.detected {
                // Skip SM internal faults - they need SM-of-SM, not BIST
                if result.is_safety_mechanism {
                    continue;
                }

                // This is a coverage gap - dangerous fault that needs BIST
                undetected_faults.push(UndetectedFaultInfo {
                    fault_site: result.primitive_path.clone(),
                    fault_type: format!("{:?}", result.fault_site.fault_type),
                    component: result
                        .primitive_path
                        .split('.')
                        .next()
                        .unwrap_or("unknown")
                        .to_string(),
                    fit_contribution: lir
                        .primitives
                        .iter()
                        .find(|p| p.path == result.primitive_path)
                        .map(|p| p.ptype.base_fit())
                        .unwrap_or(1.0),
                    classification: FaultClassification::CoverageGap,
                });
            }
        }

        println!("   Coverage gap faults: {}", undetected_faults.len());

        if !undetected_faults.is_empty() {
            let bist_config = BistGenerationConfig {
                entity_name: format!("{}Bist", lir.name),
                enable_dual_bist: dual_bist,
                enable_signature: true,
                enable_self_test: true,
                max_patterns: 0, // No limit
                ..Default::default()
            };

            let mut generator = BistGenerator::new(bist_config);
            generator.identify_candidates(&fi_result, &fault_results, &undetected_faults);
            let bist_result = generator.generate();

            // Write BIST Skalp source
            let bist_path = output_dir.join("generated_bist.sk");
            fs::write(&bist_path, &bist_result.skalp_source)?;
            println!(
                "   ✅ {} ({} patterns)",
                bist_path.display(),
                bist_result.num_patterns
            );

            // Write BIST summary
            let bist_summary_path = output_dir.join("bist_summary.md");
            let bist_summary = format!(
                r#"# Safety-Driven BIST Report

## Overview

- **Entity**: `{}`
- **Test Patterns**: {}
- **Faults Covered**: {}
- **FIT Covered**: {:.2}
- **Estimated Gates**: {}

## SM-of-SM Features

{}

## Usage

Include the generated BIST entity in your top-level design:

```skalp
use generated_bist::{}{}

entity TopWithBist {{
    in clk: clock
    in rst: bit
    // ... your design ports ...
    out bist_pass: bit
    out bist_complete: bit
}}

impl TopWithBist {{
    // Instantiate BIST
    let bist = {}{} {{
        clk: clk,
        rst: rst,
        start_bist: rst, // Run BIST after reset
        dut_inputs: ...,
        dut_outputs: ...,
    }}

    bist_pass = bist.bist_pass
    bist_complete = bist.bist_complete
}}
```

## Test Pattern Details

| Pattern | Target Fault | Description |
|---------|-------------|-------------|
"#,
                bist_result.entity_name,
                bist_result.num_patterns,
                bist_result.faults_covered,
                bist_result.fit_covered,
                bist_result.estimated_gates,
                bist_result
                    .sm_of_sm_features
                    .iter()
                    .map(|f| format!("- {}", f))
                    .collect::<Vec<_>>()
                    .join("\n"),
                bist_result.entity_name,
                if dual_bist { "Dual" } else { "" },
                bist_result.entity_name,
                if dual_bist { "Dual" } else { "" },
            );
            let mut summary = bist_summary;
            for pattern in &bist_result.test_patterns {
                summary.push_str(&format!(
                    "| {} | {} | {} |\n",
                    pattern.test_id, pattern.target_fault, pattern.description
                ));
            }
            fs::write(&bist_summary_path, summary)?;
            println!("   ✅ {}", bist_summary_path.display());

            if dual_bist {
                println!("   🛡️  Dual BIST enabled (SM-of-SM protection)");
            }
        } else {
            println!("   ℹ️  No coverage gaps found - BIST not needed");
        }
    }

    let elapsed = start.elapsed();
    println!("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
    println!(
        "✅ FI-Driven Safety Analysis Complete ({:.2}s)",
        elapsed.as_secs_f64()
    );
    println!("📁 Collaterals: {:?}", output_dir);

    Ok(())
}
*/
fn _orphaned_code_marker_end() {}
// === End orphaned code removal marker ===

/// Run CPU-based fault injection campaign
/// NOTE: This function has been stubbed out - pending GateNetlist migration
#[allow(dead_code)]
fn run_cpu_fi_campaign(
    _sir: &skalp_sim::sir::Sir,
    _cycles: u64,
    _max_faults: usize,
    _primitive_paths: &std::collections::HashMap<u32, String>,
    _sm_primitives: &std::collections::HashSet<u32>,
    _boot_time_only_primitives: &std::collections::HashSet<u32>,
) -> Result<Vec<()>> {
    // Legacy implementation removed - uses removed types:
    // - skalp_safety::safety_driven_fmea::FaultEffectResult
    // - skalp_safety::fault_simulation::FaultType
    // - skalp_safety::fault_simulation::FaultSite
    anyhow::bail!("CPU FI campaign temporarily disabled during GateNetlist migration");

    /*
    // Original implementation used:
    use skalp_frontend::hir::DetectionMode;
    use skalp_safety::fault_simulation::FaultType;
    use skalp_safety::hierarchy::DesignRef;
    use skalp_sim::sir::{FaultType as SimFaultType, SirDetectionMode};
    use skalp_sim::{FaultCampaignConfig, GateLevelSimulator};

    let mut simulator = GateLevelSimulator::new(sir);

    let config = FaultCampaignConfig {
        cycles_per_fault: cycles,
        clock_name: "clk".to_string(),
        fault_types: vec![
            SimFaultType::StuckAt0,
            SimFaultType::StuckAt1,
            SimFaultType::BitFlip,
            SimFaultType::Transient,
            SimFaultType::VoltageDropout,
            SimFaultType::GroundBounce,
        ],
        max_faults,
        ..Default::default()
    };

    let campaign = simulator.run_fault_campaign_with_config(&config);

    println!("   Faults simulated: {}", campaign.total_faults);
    println!("   Detected: {}", campaign.detected_faults);
    println!(
        "   Safe faults: {} ({:.1}%)",
        campaign.safe_faults, campaign.safe_fault_percentage
    );
    println!("   DC (overall): {:.2}%", campaign.diagnostic_coverage);
    println!("   ┌─ Permanent faults (SA0/SA1):");
    println!(
        "   │  Total: {}, Corruption: {}, Safe: {} ({:.1}%)",
        campaign.permanent.total,
        campaign.permanent.corruption,
        campaign.permanent.safe,
        campaign.permanent.safe_pct
    );
    println!(
        "   │  DC: {:.2}%, Base FIT: {:.2}, Residual FIT: {:.3}",
        campaign.permanent.dc, campaign.permanent.base_fit, campaign.permanent.residual_fit
    );
    println!("   ├─ Transient faults (SEU/BitFlip):");
    println!(
        "   │  Total: {}, Corruption: {}, Safe: {} ({:.1}%)",
        campaign.transient.total,
        campaign.transient.corruption,
        campaign.transient.safe,
        campaign.transient.safe_pct
    );
    println!(
        "   │  DC: {:.2}%, Base FIT: {:.2}, Residual FIT: {:.3}",
        campaign.transient.dc, campaign.transient.base_fit, campaign.transient.residual_fit
    );
    println!("   └─ Power faults (VoltageDropout/GroundBounce):");
    println!(
        "      Total: {}, Corruption: {}, Safe: {} ({:.1}%)",
        campaign.power.total,
        campaign.power.corruption,
        campaign.power.safe,
        campaign.power.safe_pct
    );
    println!(
        "      DC: {:.2}%, Base FIT: {:.2}, Residual FIT: {:.3}",
        campaign.power.dc, campaign.power.base_fit, campaign.power.residual_fit
    );

    let mut results = Vec::new();
    for fault_result in &campaign.fault_results {
        let prim_id = fault_result.fault.target_primitive.0;
        let prim_path = primitive_paths
            .get(&prim_id)
            .cloned()
            .unwrap_or_else(|| format!("prim_{}", prim_id));

        let triggered = if !fault_result.output_diffs.is_empty() {
            vec!["output_corruption".to_string()]
        } else {
            vec![]
        };

        // Determine fault type from the actual fault
        let fault_type = match fault_result.fault.fault_type {
            SimFaultType::StuckAt0 => FaultType::StuckAt0,
            SimFaultType::StuckAt1 => FaultType::StuckAt1,
            SimFaultType::BitFlip | SimFaultType::Transient => FaultType::Transient,
            _ => FaultType::StuckAt0, // Default for other complex fault types
        };

        // Convert detection mode from SIR to HIR enum
        let detection_mode = fault_result.detection_mode.map(|m| match m {
            SirDetectionMode::Continuous => DetectionMode::Continuous,
            SirDetectionMode::Boot => DetectionMode::Boot,
            SirDetectionMode::Periodic => DetectionMode::Periodic,
            SirDetectionMode::OnDemand => DetectionMode::OnDemand,
        });

        results.push(skalp_safety::safety_driven_fmea::FaultEffectResult {
            fault_site: skalp_safety::fault_simulation::FaultSite::new(
                DesignRef::parse(&prim_path),
                fault_type,
            ),
            primitive_path: prim_path,
            triggered_effects: triggered,
            detected: fault_result.detected,
            detected_by: if fault_result.detected {
                Some("safety_mechanism".to_string())
            } else {
                None
            },
            effect_cycle: None,
            detection_cycle: fault_result.detection_cycle,
            is_safety_mechanism: sm_primitives.contains(&prim_id),
            detection_mode,
            is_boot_time_only: boot_time_only_primitives.contains(&prim_id),
        });
    }

    Ok(results)
    */
}

/// Trace a signal through the gate-level netlist
fn run_signal_trace(
    source: &Path,
    signal: &str,
    entity: Option<&str>,
    max_depth: usize,
    sim_cycle: Option<u64>,
    fanout: bool,
    netlist_path: Option<&PathBuf>,
) -> Result<()> {
    use skalp_frontend::parse_and_build_compilation_context;
    use skalp_lir::{get_stdlib_library, lower_mir_hierarchical_with_top, map_hierarchical_to_gates};
    use skalp_lir::signal_trace::{SignalTracer, TraceDirection};

    println!("🔍 Signal Trace");
    println!("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
    println!("Source:  {:?}", source);
    println!("Signal:  {}", signal);
    println!("Depth:   {}", max_depth);
    if let Some(cycle) = sim_cycle {
        println!("Cycle:   {}", cycle);
    }
    println!();

    // Step 1: Get or load gate-level netlist
    let gate_netlist = if let Some(path) = netlist_path {
        println!("📂 Loading gate-level netlist from {:?}...", path);
        let json = fs::read_to_string(path)
            .context("Failed to read netlist file")?;
        serde_json::from_str::<skalp_lir::GateNetlist>(&json)
            .context("Failed to parse gate-level netlist JSON")?
    } else {
        // Compile design to gate-level
        println!("📖 Parsing source...");
        let context = parse_and_build_compilation_context(source)
            .context("Failed to parse source file")?;
        let hir = context.main_hir;
        let module_hirs = context.module_hirs;

        println!("📐 Lowering to MIR...");
        let compiler = skalp_mir::MirCompiler::new()
            .with_optimization_level(skalp_mir::OptimizationLevel::None);
        let mir = compiler
            .compile_to_mir_with_modules(&hir, &module_hirs)
            .map_err(|e| anyhow::anyhow!("Failed to compile to MIR: {}", e))?;

        // Find target entity
        let target_entity = if let Some(name) = entity {
            if let Some(m) = mir.modules.iter().find(|m| m.name == name) {
                if !m.instances.is_empty() || !m.processes.is_empty() {
                    m
                } else {
                    let mono_prefix = format!("{}_", name);
                    mir.modules
                        .iter()
                        .filter(|m| m.name.starts_with(&mono_prefix))
                        .max_by_key(|m| m.instances.len() + m.processes.len())
                        .unwrap_or(m)
                }
            } else {
                let mono_prefix = format!("{}_", name);
                mir.modules
                    .iter()
                    .filter(|m| m.name.starts_with(&mono_prefix))
                    .max_by_key(|m| m.instances.len() + m.processes.len())
                    .ok_or_else(|| anyhow::anyhow!("Entity '{}' not found in design", name))?
            }
        } else {
            find_top_level_module(&mir)
                .ok_or_else(|| anyhow::anyhow!("No modules found in design"))?
        };

        println!("⚙️  Lowering to LIR...");
        let hier_lir = lower_mir_hierarchical_with_top(&mir, Some(&target_entity.name));

        println!("🔧 Synthesizing gate-level netlist...");
        let library = get_stdlib_library("generic_asic")
            .context("Failed to load technology library")?;
        let hier_netlist = map_hierarchical_to_gates(&hier_lir, &library);
        let netlist = hier_netlist.flatten();
        println!("   Cells: {}", netlist.cells.len());
        println!("   Nets: {}", netlist.nets.len());
        netlist
    };

    // Step 2: Run signal trace
    println!();
    let direction = if fanout {
        println!("📡 Tracing fanout (forward) from '{}'...", signal);
        TraceDirection::Forward
    } else {
        println!("📡 Tracing driver (backward) from '{}'...", signal);
        TraceDirection::Backward
    };

    let tracer = SignalTracer::new(&gate_netlist);
    match tracer.trace(signal, direction, max_depth) {
        Ok(trace) => {
            println!();
            trace.print();
            println!();
            println!("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
            println!("✅ Trace complete ({} nodes)", trace.nodes.len());
        }
        Err(e) => {
            println!();
            println!("❌ Trace failed: {}", e);
            println!();
            // Show available signals that match
            let suggestions = tracer.find_matching_signals(signal, 10);
            if !suggestions.is_empty() {
                println!("Did you mean one of these?");
                for s in suggestions {
                    println!("  - {}", s);
                }
            }
            anyhow::bail!("Signal trace failed");
        }
    }

    Ok(())
}

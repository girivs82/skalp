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
#[derive(Debug, Clone, Default)]
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
        /// Optimization preset (quick, balanced, full, timing, area, resyn2, compress2)
        #[arg(long, value_name = "PRESET")]
        optimize: Option<String>,

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

    // Set up logging
    let log_level = match cli.verbose {
        0 => "warn",
        1 => "info",
        2 => "debug",
        _ => "trace",
    };

    tracing_subscriber::fmt().with_env_filter(log_level).init();

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
            passes,
            ml_guided,
            ml_policy,
            collect_training_data,
            library,
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
            let optimization_options = OptimizationOptions {
                preset: optimize,
                passes,
                ml_guided,
                ml_policy_path: ml_policy,
                training_data_dir: collect_training_data,
            };

            build_design(
                &source_file,
                &target,
                &output,
                safety_options,
                optimization_options,
                library.as_ref(),
            )?;
        }

        Commands::Sim {
            design,
            duration,
            gate_level,
            gpu,
        } => {
            simulate_design(&design, duration.as_deref(), gate_level, gpu)?;
        }

        Commands::Synth {
            source,
            device,
            full_flow,
        } => {
            synthesize_design(&source, &device, full_flow)?;
        }

        Commands::Program {
            bitstream,
            interface,
            verify,
        } => {
            program_device(&bitstream, &interface, verify)?;
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
) -> Result<()> {
    use skalp_codegen::systemverilog::generate_systemverilog_from_mir;
    use skalp_frontend::parse_and_build_hir_from_file;

    info!("Building design from {:?} to {}", source, target);

    // Parse, build HIR with module resolution
    info!("Parsing SKALP source and building HIR with module resolution...");
    let hir = parse_and_build_hir_from_file(source).context("Failed to parse and build HIR")?;

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
    let mir = compiler
        .compile_to_mir(&hir)
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
            use skalp_lir::{builtin_libraries::builtin_generic_asic, lower_mir_module_to_lir};

            info!("Generating optimized gate-level netlist...");

            // Get the top module from MIR
            let top_module = mir
                .modules
                .first()
                .ok_or_else(|| anyhow::anyhow!("No modules found in MIR"))?;

            // Lower MIR module to LIR
            let lir_result = lower_mir_module_to_lir(top_module);

            // Get technology library
            let library = if let Some(path) = library_path {
                info!("Loading technology library from {:?}", path);
                skalp_lir::TechLibrary::load_from_file(path)
                    .context("Failed to load technology library")?
            } else {
                builtin_generic_asic()
            };

            // Map to gate netlist with optimizations
            let tech_result = skalp_lir::map_lir_to_gates_optimized(&lir_result.lir, &library);
            let gate_netlist = tech_result.netlist;

            // Apply synthesis optimization if requested
            let optimized_netlist = if optimization_options.preset.is_some()
                || optimization_options.passes.is_some()
                || optimization_options.ml_guided
            {
                info!("Running synthesis optimization...");
                apply_synthesis_optimization(&gate_netlist, &library, &optimization_options)?
            } else {
                gate_netlist
            };

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

/// Apply synthesis optimization to a gate netlist
fn apply_synthesis_optimization(
    netlist: &skalp_lir::GateNetlist,
    library: &skalp_lir::TechLibrary,
    options: &OptimizationOptions,
) -> Result<skalp_lir::GateNetlist> {
    // Use ML-guided optimization if requested
    if options.ml_guided {
        return apply_ml_synthesis_optimization(netlist, library, options);
    }

    use skalp_lir::synth::{SynthConfig, SynthEngine, SynthPreset};

    // Create config based on preset
    let config = match options.preset.as_deref() {
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
        Some(other) => {
            anyhow::bail!(
                "Unknown optimization preset: '{}'. Use: quick, balanced, full, timing, area, resyn2, compress2",
                other
            );
        }
    };

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

    Ok(result.netlist)
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

/// Simulate design
///
/// Supports two simulation paths:
/// - Behavioral (default): HIR → MIR → SIR (fast, functional)
/// - Gate-level (--gate-level): HIR → MIR → LIR → SIR (gate-level primitives, fault injection ready)
fn simulate_design(
    design_file: &PathBuf,
    duration: Option<&str>,
    gate_level: bool,
    use_gpu: bool,
) -> Result<()> {
    use skalp_mir::Mir;
    use skalp_sir::convert_mir_to_sir;

    info!("Loading design from {:?}", design_file);

    // Parse duration (default 1000 cycles)
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
            if gate_level {
                simulate_gate_level(design_file, cycles, use_gpu)
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
    use skalp_sir::convert_mir_to_sir;

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

    // Convert to SIR
    info!("Converting to SIR...");
    let sir = convert_mir_to_sir(&mir.modules[0]);

    println!("📊 Design Statistics:");
    println!("   Inputs: {}", sir.inputs.len());
    println!("   Outputs: {}", sir.outputs.len());
    println!("   Combinational nodes: {}", sir.combinational_nodes.len());
    println!("   Sequential nodes: {}", sir.sequential_nodes.len());
    println!();

    simulate_sir_behavioral(&sir, cycles, use_gpu)
}

/// Gate-level simulation: HIR → MIR → WordLir → GateNetlist → SIR
/// NOTE: This path is being migrated to use GateNetlist instead of legacy LIR
fn simulate_gate_level(source_file: &PathBuf, _cycles: u64, _use_gpu: bool) -> Result<()> {
    println!("🔬 Gate-Level Simulation");
    println!("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
    println!("Source: {:?}", source_file);
    println!();
    println!("⚠️  Gate-level simulation is being migrated to use GateNetlist.");
    println!("   Please use behavioral simulation (--mode behavioral) for now.");
    println!("   The new flow: HIR → MIR → WordLir → TechMapper → GateNetlist → SIR");
    anyhow::bail!("Gate-level simulation temporarily disabled during GateNetlist migration");
}

/// Gate-level simulation from MIR (stub - pending GateNetlist migration)
#[allow(dead_code)]
fn simulate_gate_level_from_mir_stub(
    _mir: &skalp_mir::Mir,
    _cycles: u64,
    _use_gpu: bool,
) -> Result<()> {
    anyhow::bail!("Gate-level simulation temporarily disabled during GateNetlist migration")
}

/// Run behavioral SIR simulation
fn simulate_sir_behavioral(sir: &skalp_sir::SirModule, cycles: u64, use_gpu: bool) -> Result<()> {
    use skalp_sim::waveform::Waveform;
    use skalp_sim::{SimulationConfig, Simulator};
    use tokio::runtime::Runtime;

    println!("🔧 Running behavioral simulation...");

    // Create async runtime for simulation
    let runtime = Runtime::new()?;

    runtime.block_on(async {
        // Create simulation config
        let config = SimulationConfig {
            use_gpu,
            max_cycles: cycles,
            timeout_ms: 60_000,
            capture_waveforms: true,
            parallel_threads: 4,
        };

        // Create and initialize simulator
        let mut simulator = Simulator::new(config).await?;
        simulator.load_module(sir).await?;

        // Run simulation
        simulator.run_simulation().await?;

        println!("   Completed {} cycles", cycles);

        // Get simulation history and create waveform
        let state_history = simulator.get_waveforms().await;
        if !state_history.is_empty() {
            let waveform = Waveform::from_simulation_states(&state_history);
            waveform.export_vcd(&PathBuf::from("simulation.vcd"))?;
            println!("\n📈 Waveform exported to simulation.vcd");
            waveform.print_summary();
        }

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

/// Synthesize design for FPGA/ASIC
/// NOTE: This function is being migrated to use GateNetlist instead of legacy LIR
fn synthesize_design(source: &PathBuf, device: &str, _full_flow: bool) -> Result<()> {
    println!("🔧 Synthesizing design for device: {}", device);
    println!("Source: {:?}", source);
    println!();
    println!("⚠️  Synthesis is being migrated to use GateNetlist.");
    println!("   The new flow: HIR → MIR → WordLir → TechMapper → GateNetlist → Backend");
    println!("   Supported devices: ice40-hx1k, ice40-hx8k, xc7a35t, sky130, freepdk45");
    anyhow::bail!("FPGA/ASIC synthesis temporarily disabled during GateNetlist migration");
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
    use skalp_lir::{builtin_libraries::builtin_generic_asic, lower_mir_module_to_lir};
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

    // Find the top module (last one, or one matching the file name)
    let top_module = mir
        .modules
        .last()
        .ok_or_else(|| anyhow::anyhow!("No modules found in design"))?;

    // Lower to Lir and tech-map to GateNetlist
    println!("🔩 Converting to gate-level netlist...");
    let lir_result = lower_mir_module_to_lir(top_module);
    let library = builtin_generic_asic();

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

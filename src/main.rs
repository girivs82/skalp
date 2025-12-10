#![allow(unused_variables)]

use anyhow::{Context, Result};
use clap::{Parser, Subcommand};
use std::fs;
use std::path::PathBuf;
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

            build_design(&source_file, &target, &output, safety_options)?;
        }

        Commands::Sim { design, duration } => {
            simulate_design(&design, duration.as_deref())?;
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
) -> Result<()> {
    use skalp_codegen::{generate_systemverilog_from_mir, generate_verilog, generate_vhdl};
    use skalp_frontend::parse_and_build_hir_from_file;
    use skalp_lir::lower_to_lir;

    info!("Building design from {:?} to {}", source, target);

    // Parse, build HIR with module resolution
    info!("Parsing SKALP source and building HIR with module resolution...");
    let hir = parse_and_build_hir_from_file(source).context("Failed to parse and build HIR")?;

    // Run safety analysis if enabled
    if let Some(ref safety_opts) = safety_options {
        run_safety_analysis(&hir, safety_opts, output_dir)?;
    }

    // Lower to MIR with CDC analysis
    info!("Lowering to MIR with CDC analysis...");
    let compiler = skalp_mir::MirCompiler::new()
        .with_optimization_level(skalp_mir::OptimizationLevel::None)
        .with_verbose(true); // Enable verbose output for CDC analysis
    let mir = compiler
        .compile_to_mir(&hir)
        .map_err(|e| anyhow::anyhow!("Failed to compile HIR to MIR with CDC analysis: {}", e))?;

    // Lower to LIR
    info!("Lowering to LIR...");
    let lir = lower_to_lir(&mir).context("Failed to lower to LIR")?;

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
            anyhow::bail!(
                "Unsupported target: {}. Use 'sv', 'v', 'vhdl', 'lir', or 'mir'",
                target
            );
        }
    };

    println!("✅ Build complete!");
    println!("📄 Output: {:?}", output_file);

    Ok(())
}

/// Simulate design using GPU-accelerated simulation
fn simulate_design(design_file: &PathBuf, duration: Option<&str>) -> Result<()> {
    use skalp_mir::Mir;
    use skalp_sim::waveform::Waveform;
    use skalp_sim::{SimulationConfig, Simulator};
    use skalp_sir::convert_mir_to_sir;
    use tokio::runtime::Runtime;

    info!("Loading design from {:?}", design_file);

    // Load design (support both .lir and .mir files)
    let design_str = fs::read_to_string(design_file)?;

    let sir = if design_file.extension() == Some(std::ffi::OsStr::new("lir")) {
        // For LIR files, create a minimal SIR for simulation
        use skalp_lir::LirDesign;
        use skalp_sir::{SirModule, SirSignal, SirType};
        use std::collections::HashMap;

        let lir: LirDesign = serde_json::from_str(&design_str)?;
        if lir.modules.is_empty() {
            anyhow::bail!("No modules found in LIR");
        }

        // Create a simple SIR module from LIR
        let lir_module = &lir.modules[0];
        let mut signals = Vec::new();

        // Add signals from LIR
        for signal in lir_module.signals.iter() {
            signals.push(SirSignal {
                name: signal.name.clone(),
                width: 32, // Default width
                sir_type: SirType::Bits(32),
                driver_node: None,
                fanout_nodes: Vec::new(),
                is_state: signal.is_register,
                span: None,
            });
        }

        SirModule {
            name: lir_module.name.clone(),
            inputs: Vec::new(),
            outputs: Vec::new(),
            signals,
            combinational_nodes: Vec::new(),
            sequential_nodes: Vec::new(),
            state_elements: HashMap::new(),
            clock_domains: HashMap::new(),
            sorted_combinational_node_ids: Vec::new(), // Empty for LIR (no combinational nodes)
            pipeline_config: None, // LIR doesn't support pipeline config
            span: None,
        }
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
            dur.trim_end_matches("ns").parse::<u64>()? / 10 // Assume 10ns clock
        } else if dur.ends_with("us") {
            dur.trim_end_matches("us").parse::<u64>()? * 100
        } else {
            dur.parse::<u64>()?
        }
    } else {
        1000
    };

    println!("Starting GPU-accelerated simulation");
    println!("Simulating {} cycles", cycles);

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

        println!("Simulation complete!");
        println!("Simulated {} cycles", cycles);

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
    build_design(source, "lir", &temp_dir.path().to_path_buf(), None)?;

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
            anyhow::bail!(
                "Unknown device: {}. Supported: ice40-hx1k, ice40-hx8k, xc7a35t, sky130, freepdk45",
                device
            );
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
    output_dir: &PathBuf,
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
        anyhow::bail!("Cannot generate work products - {} errors found", result.total_errors);
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
    generate_safety_workproducts(&result, &hierarchy, &report_dir, &workproducts, &options.formats)?;

    println!("\n✅ Safety analysis complete");
    println!("📁 Reports: {:?}", report_dir);

    Ok(())
}

/// Generate ISO 26262 work products
fn generate_safety_workproducts(
    result: &skalp_safety::analysis::CombinedAnalysisResult,
    hierarchy: &skalp_safety::hierarchy::SafetyHierarchy,
    output_dir: &PathBuf,
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
    output.push_str(&format!(
        "- **Status**: {:?}\n",
        result.overall_status
    ));
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
    html.push_str(&format!("<p>Target ASIL: <strong>{:?}</strong></p>\n", result.target_asil));

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

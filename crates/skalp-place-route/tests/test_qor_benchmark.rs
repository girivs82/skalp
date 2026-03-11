//! QoR Benchmark Framework — skalp native vs Yosys+nextpnr
//!
//! Compares quality-of-results between skalp's native iCE40 flow and the
//! industry-standard open-source flow (Yosys + nextpnr). Skalp source is the
//! single source of truth: the native flow compiles directly, the reference
//! flow generates SystemVerilog via HIR codegen and feeds it to external tools.
//!
//! Run:
//!   cargo test --test test_qor_benchmark -- test_qor_designs_compile       # smoke (no tools needed)
//!   cargo test --test test_qor_benchmark -- test_qor_counter8 --ignored --nocapture  # single design
//!   cargo test --test test_qor_benchmark -- test_qor_benchmark_suite --ignored --nocapture  # full suite

use skalp_frontend::parse_and_build_hir;
use skalp_hir_codegen::generate_systemverilog_files;
use skalp_lir::{get_stdlib_library, lower_mir_module_to_lir_with_bram, synthesize_balanced};
use skalp_mir::MirCompiler;
use skalp_place_route::packing::CellPacker;
use skalp_place_route::{
    place_and_route, Ice40Variant, Placer, PlacerConfig, PnrConfig, Router, RouterConfig,
    TimingConfig,
};
use std::path::Path;
use std::process::Command;
use std::time::{Duration, Instant};

// ===== Data structures =====

#[allow(dead_code)]
struct BenchmarkDesign {
    name: &'static str,
    source: &'static str,
    top_module: &'static str,
}

#[derive(Debug, Default)]
struct FlowResult {
    success: bool,
    error: Option<String>,
    luts: Option<usize>,
    dffs: Option<usize>,
    carries: Option<usize>,
    ios: Option<usize>,
    total_lcs: Option<usize>,
    wirelength: Option<usize>,
    fmax_mhz: Option<f64>,
    bitstream_bytes: Option<usize>,
    compile_time: Duration,
    // PnR-specific metrics
    placement_wirelength: Option<u64>,
    placement_cost: Option<f64>,
    utilization: Option<f64>,
    congestion: Option<f64>,
    routing_iterations: Option<usize>,
    routing_success: Option<bool>,
    worst_slack_ns: Option<f64>,
    critical_path_stages: Option<usize>,
    place_time: Option<Duration>,
    route_time: Option<Duration>,
    timing_time: Option<Duration>,
}

struct QorComparison {
    design_name: String,
    skalp: FlowResult,
    reference: FlowResult,
}

// ===== Benchmark designs =====

const COUNTER8_SK: &str = r#"
entity Counter8 {
    in clk: clock
    out count: bit[8]
}
impl Counter8 {
    signal cnt: bit[8] = 0
    on(clk.rise) {
        cnt = cnt + 1
    }
    count = cnt
}
"#;

const SHIFTREG8_SK: &str = r#"
entity ShiftReg8 {
    in clk: clock
    in data_in: bit
    out data_out: bit
}
impl ShiftReg8 {
    signal sr: bit[8] = 0
    on(clk.rise) {
        sr = {sr[6:0], data_in}
    }
    data_out = sr[7]
}
"#;

const ALU_8BIT_SK: &str = include_str!("../../../examples/real_world/08_alu/alu.sk");
const UART_TX_SK: &str = include_str!("../../../examples/real_world/02_uart_tx/uart_tx.sk");
const REGFILE_SK: &str = include_str!("../../../examples/real_world/07_register_file/regfile.sk");
const COUNTER16_SK: &str = r#"
entity Counter16 {
    in clk: clock
    out count: bit[16]
}
impl Counter16 {
    signal cnt: bit[16] = 0
    on(clk.rise) {
        cnt = cnt + 1
    }
    count = cnt
}
"#;
const SPI_MASTER_SK: &str = include_str!("../../../examples/spi_master.sk");

fn benchmark_designs() -> Vec<BenchmarkDesign> {
    vec![
        BenchmarkDesign {
            name: "Counter8",
            source: COUNTER8_SK,
            top_module: "Counter8",
        },
        BenchmarkDesign {
            name: "ShiftReg8",
            source: SHIFTREG8_SK,
            top_module: "ShiftReg8",
        },
        BenchmarkDesign {
            name: "ALU (8-bit)",
            source: ALU_8BIT_SK,
            top_module: "ALU",
        },
        BenchmarkDesign {
            name: "UART TX",
            source: UART_TX_SK,
            top_module: "UartTx",
        },
        BenchmarkDesign {
            name: "RegisterFile",
            source: REGFILE_SK,
            top_module: "RegisterFile",
        },
        BenchmarkDesign {
            name: "Counter16",
            source: COUNTER16_SK,
            top_module: "Counter16",
        },
        BenchmarkDesign {
            name: "SPI Master",
            source: SPI_MASTER_SK,
            top_module: "SpiMaster",
        },
    ]
}

// ===== Tool helpers =====

fn yosys_path() -> &'static str {
    "/opt/homebrew/bin/yosys"
}

fn nextpnr_path() -> &'static str {
    if Path::new("/Users/girivs/.local/bin/nextpnr-ice40").exists() {
        "/Users/girivs/.local/bin/nextpnr-ice40"
    } else {
        "nextpnr-ice40"
    }
}

fn icetime_path() -> &'static str {
    if Path::new("/Users/girivs/.local/bin/icetime").exists() {
        "/Users/girivs/.local/bin/icetime"
    } else {
        "icetime"
    }
}

fn icepack_path() -> &'static str {
    if Path::new("/Users/girivs/.local/bin/icepack").exists() {
        "/Users/girivs/.local/bin/icepack"
    } else {
        "icepack"
    }
}

fn tools_available() -> bool {
    Command::new(yosys_path()).arg("--version").output().is_ok()
        && Command::new(nextpnr_path())
            .arg("--version")
            .output()
            .is_ok()
        && Command::new(icetime_path())
            .arg("--version")
            .output()
            .is_ok()
        && Command::new(icepack_path())
            .arg("--version")
            .output()
            .is_ok()
}

// ===== skalp native flow =====

fn run_skalp_flow(design: &BenchmarkDesign) -> FlowResult {
    let start = Instant::now();
    let source = design.source;

    // Wrap the entire flow in catch_unwind to handle panics (e.g., overflow in synthesis)
    let inner_result = std::panic::catch_unwind(|| {
        let mut result = FlowResult::default();

        // Parse and build HIR
        let hir = match parse_and_build_hir(source) {
            Ok(h) => h,
            Err(e) => {
                result.error = Some(format!("Parse failed: {}", e));
                return result;
            }
        };

        // Compile to MIR
        let mir = match MirCompiler::new().compile_to_mir(&hir) {
            Ok(m) => m,
            Err(e) => {
                result.error = Some(format!("MIR compilation failed: {}", e));
                return result;
            }
        };

        // Lower to LIR (use last module = top module)
        let module = match mir.modules.last() {
            Some(m) => m,
            None => {
                result.error = Some("No modules in MIR".to_string());
                return result;
            }
        };
        let lir_result = lower_mir_module_to_lir_with_bram(module);

        // Synthesize
        let library = match get_stdlib_library("ice40") {
            Ok(l) => l,
            Err(e) => {
                result.error = Some(format!("Failed to load ice40 library: {:?}", e));
                return result;
            }
        };
        let synth = synthesize_balanced(&lir_result.lir, &library);

        // Pack — get packing stats
        let mut packer = CellPacker::new(&synth.netlist);
        let packing = packer.pack();
        let stats = &packing.stats;

        result.luts = Some(stats.lut_only + stats.lut_dff);
        result.dffs = Some(stats.dff_only + stats.lut_dff);
        result.carries = Some(stats.carry_only);
        result.ios = Some(stats.io_cells);
        result.total_lcs = Some(stats.total_lcs);

        // Place and route (HX8K, with timing) — capture detailed PnR metrics
        let config = PnrConfig {
            timing: Some(TimingConfig::default()),
            ..PnrConfig::default()
        };

        // Run P&R with stage-level timing
        let t_pnr_start = Instant::now();
        match place_and_route(&synth.netlist, Ice40Variant::Hx8k, config) {
            Ok(pnr) => {
                let _t_pnr = t_pnr_start.elapsed();

                // Routing metrics
                result.wirelength = Some(pnr.routing.wirelength as usize);
                result.congestion = Some(pnr.routing.congestion);
                result.routing_iterations = Some(pnr.routing.iterations);
                result.routing_success = Some(pnr.routing.success);

                // Placement metrics
                result.placement_wirelength = Some(pnr.placement.wirelength);
                result.placement_cost = Some(pnr.placement.cost);
                result.utilization = Some(pnr.placement.utilization);

                // Timing metrics
                if let Some(ref timing) = pnr.timing {
                    result.fmax_mhz = Some(timing.design_frequency);
                    result.worst_slack_ns = Some(timing.worst_negative_slack);
                    result.critical_path_stages = Some(
                        timing.critical_paths.first()
                            .map(|p| p.path.len())
                            .unwrap_or(0)
                    );
                }

                result.bitstream_bytes = Some(pnr.bitstream.data.len());
                result.success = true;
            }
            Err(e) => {
                result.error = Some(format!("P&R failed: {}", e));
            }
        }

        result
    });

    let mut result = match inner_result {
        Ok(r) => r,
        Err(e) => {
            let msg = if let Some(s) = e.downcast_ref::<String>() {
                s.clone()
            } else if let Some(s) = e.downcast_ref::<&str>() {
                s.to_string()
            } else {
                "unknown panic".to_string()
            };
            FlowResult {
                error: Some(format!("Panic: {}", msg)),
                ..Default::default()
            }
        }
    };

    result.compile_time = start.elapsed();
    result
}

// ===== Reference flow (Yosys + nextpnr) =====

fn run_reference_flow(design: &BenchmarkDesign) -> FlowResult {
    let start = Instant::now();
    let mut result = FlowResult::default();

    // Parse HIR and generate SystemVerilog
    let hir = match parse_and_build_hir(design.source) {
        Ok(h) => h,
        Err(e) => {
            result.error = Some(format!("Parse failed: {}", e));
            result.compile_time = start.elapsed();
            return result;
        }
    };

    let sv_files = match generate_systemverilog_files(&hir) {
        Ok(f) => f,
        Err(e) => {
            result.error = Some(format!("SV codegen failed: {}", e));
            result.compile_time = start.elapsed();
            return result;
        }
    };

    if sv_files.is_empty() {
        result.error = Some("SV codegen produced no files".to_string());
        result.compile_time = start.elapsed();
        return result;
    }

    // Use the entity name from the last generated file, falling back to design.top_module
    let top_name = sv_files
        .last()
        .map(|f| f.name.clone())
        .unwrap_or_else(|| design.top_module.to_string());

    // Combine all SV into a single string
    let sv_combined: String = sv_files.iter().map(|f| f.code.as_str()).collect::<Vec<_>>().join("\n");

    let dir = match tempfile::tempdir() {
        Ok(d) => d,
        Err(e) => {
            result.error = Some(format!("Failed to create tempdir: {}", e));
            result.compile_time = start.elapsed();
            return result;
        }
    };
    let sv_path = dir.path().join("design.sv");
    let json_path = dir.path().join("design.json");
    let asc_path = dir.path().join("design.asc");
    let bin_path = dir.path().join("design.bin");

    if std::fs::write(&sv_path, &sv_combined).is_err() {
        result.error = Some("Failed to write SV file".to_string());
        result.compile_time = start.elapsed();
        return result;
    }

    // Yosys: synthesis + stat
    let yosys_cmd = format!(
        "read_verilog -sv {}; synth_ice40 -top {} -json {}; stat",
        sv_path.display(),
        top_name,
        json_path.display()
    );
    let yosys_out = match Command::new(yosys_path()).args(["-p", &yosys_cmd]).output() {
        Ok(o) => o,
        Err(e) => {
            result.error = Some(format!("Failed to run yosys: {}", e));
            result.compile_time = start.elapsed();
            return result;
        }
    };

    if !yosys_out.status.success() {
        result.error = Some(format!(
            "Yosys failed: {}",
            String::from_utf8_lossy(&yosys_out.stderr)
        ));
        result.compile_time = start.elapsed();
        return result;
    }

    // Parse Yosys stat output
    let yosys_stdout = String::from_utf8_lossy(&yosys_out.stdout);
    parse_yosys_stats(&yosys_stdout, &mut result);

    // nextpnr: place and route
    let nextpnr_out = match Command::new(nextpnr_path())
        .args([
            "--hx8k",
            "--json",
            json_path.to_str().unwrap(),
            "--asc",
            asc_path.to_str().unwrap(),
            "--seed",
            "42",
            "--pcf-allow-unconstrained",
        ])
        .output()
    {
        Ok(o) => o,
        Err(e) => {
            result.error = Some(format!("Failed to run nextpnr: {}", e));
            result.compile_time = start.elapsed();
            return result;
        }
    };

    if !nextpnr_out.status.success() {
        result.error = Some(format!(
            "nextpnr failed: {}",
            String::from_utf8_lossy(&nextpnr_out.stderr)
        ));
        result.compile_time = start.elapsed();
        // Still return Yosys stats
        return result;
    }

    // Parse nextpnr Fmax from stderr
    let nextpnr_stderr = String::from_utf8_lossy(&nextpnr_out.stderr);
    parse_nextpnr_fmax(&nextpnr_stderr, &mut result);

    // icetime: post-route timing
    if let Ok(icetime_out) = Command::new(icetime_path())
        .args(["-d", "hx8k", asc_path.to_str().unwrap()])
        .output()
    {
        let icetime_stdout = String::from_utf8_lossy(&icetime_out.stdout);
        parse_icetime_fmax(&icetime_stdout, &mut result);
    }

    // icepack: bitstream size
    if let Ok(icepack_out) = Command::new(icepack_path())
        .args([asc_path.to_str().unwrap(), bin_path.to_str().unwrap()])
        .output()
    {
        if icepack_out.status.success() {
            if let Ok(meta) = std::fs::metadata(&bin_path) {
                result.bitstream_bytes = Some(meta.len() as usize);
            }
        }
    }

    result.success = true;
    result.compile_time = start.elapsed();
    result
}

// ===== Output parsing helpers =====

fn parse_yosys_stats(stdout: &str, result: &mut FlowResult) {
    // Yosys stat format: "        8   SB_DFF" (count first, then cell name)
    // There may be multiple "Printing statistics" sections; we want the last one.
    // We scan for cell lines containing "SB_" after the last "Printing statistics".
    let mut luts = 0usize;
    let mut dffs = 0usize;
    let mut carries = 0usize;
    let mut ios = 0usize;

    // Find the last occurrence of "Printing statistics"
    let last_stats_pos = stdout.rfind("Printing statistics");
    let search_region = match last_stats_pos {
        Some(pos) => &stdout[pos..],
        None => return,
    };

    for line in search_region.lines() {
        let trimmed = line.trim();

        if let Some(n) = extract_cell_count(trimmed, "SB_LUT4") {
            luts += n;
        }
        // Match all SB_DFF variants (SB_DFF, SB_DFFE, SB_DFFR, SB_DFFS, etc.)
        if trimmed.contains("SB_DFF") {
            if let Some(n) = extract_leading_count(trimmed) {
                dffs += n;
            }
        }
        if let Some(n) = extract_cell_count(trimmed, "SB_CARRY") {
            carries += n;
        }
        if let Some(n) = extract_cell_count(trimmed, "SB_IO") {
            ios += n;
        }
    }

    if luts > 0 { result.luts = Some(luts); }
    if dffs > 0 { result.dffs = Some(dffs); }
    if carries > 0 { result.carries = Some(carries); }
    if ios > 0 { result.ios = Some(ios); }
}

fn extract_cell_count(line: &str, cell_name: &str) -> Option<usize> {
    // Yosys stat format: "        8   SB_LUT4" (count, then cell name)
    if !line.contains(cell_name) {
        return None;
    }
    extract_leading_count(line)
}

fn extract_leading_count(line: &str) -> Option<usize> {
    // First whitespace-separated token is the count
    line.split_whitespace().next()?.parse().ok()
}

fn parse_nextpnr_fmax(stderr: &str, result: &mut FlowResult) {
    // nextpnr outputs: "Max frequency for clock 'clk': XXX.XX MHz (PASS at YYY.YY MHz)"
    for line in stderr.lines() {
        if line.contains("Max frequency for clock") {
            if let Some(start) = line.find(": ") {
                let after = &line[start + 2..];
                if let Some(end) = after.find(" MHz") {
                    if let Ok(freq) = after[..end].trim().parse::<f64>() {
                        // Use the highest Fmax if there are multiple clocks
                        match result.fmax_mhz {
                            Some(existing) if existing > freq => {}
                            _ => result.fmax_mhz = Some(freq),
                        }
                    }
                }
            }
        }
    }
}

fn parse_icetime_fmax(stdout: &str, result: &mut FlowResult) {
    // icetime output: "Total path delay: X.XX ns (Y.YY MHz)"
    for line in stdout.lines() {
        if line.contains("MHz") {
            if let Some(start) = line.rfind('(') {
                let inside = &line[start + 1..];
                if let Some(end) = inside.find(" MHz") {
                    if let Ok(freq) = inside[..end].trim().parse::<f64>() {
                        // Prefer icetime Fmax over nextpnr's estimate
                        result.fmax_mhz = Some(freq);
                    }
                }
            }
        }
    }
}

// ===== Full comparison =====

fn run_comparison(design: &BenchmarkDesign) -> QorComparison {
    let skalp = run_skalp_flow(design);
    let reference = if tools_available() {
        run_reference_flow(design)
    } else {
        FlowResult {
            error: Some("External tools not available".to_string()),
            ..Default::default()
        }
    };

    QorComparison {
        design_name: design.name.to_string(),
        skalp,
        reference,
    }
}

fn print_single_comparison(cmp: &QorComparison) {
    println!("\n=== {} ===", cmp.design_name);
    println!("{:<20} {:>10} {:>10}", "", "skalp", "reference");
    println!("{}", "-".repeat(42));

    fn fmt_opt(v: Option<usize>) -> String {
        v.map(|n| format!("{}", n)).unwrap_or_else(|| "-".to_string())
    }
    fn fmt_opt_f64(v: Option<f64>) -> String {
        v.map(|f| format!("{:.1}", f)).unwrap_or_else(|| "-".to_string())
    }

    println!("{:<20} {:>10} {:>10}", "LUTs", fmt_opt(cmp.skalp.luts), fmt_opt(cmp.reference.luts));
    println!("{:<20} {:>10} {:>10}", "DFFs", fmt_opt(cmp.skalp.dffs), fmt_opt(cmp.reference.dffs));
    println!("{:<20} {:>10} {:>10}", "Carries", fmt_opt(cmp.skalp.carries), fmt_opt(cmp.reference.carries));
    println!("{:<20} {:>10} {:>10}", "I/Os", fmt_opt(cmp.skalp.ios), fmt_opt(cmp.reference.ios));
    println!("{:<20} {:>10} {:>10}", "Total LCs", fmt_opt(cmp.skalp.total_lcs), fmt_opt(cmp.reference.total_lcs));
    println!("{:<20} {:>10} {:>10}", "Wirelength", fmt_opt(cmp.skalp.wirelength), fmt_opt(cmp.reference.wirelength));
    println!("{:<20} {:>10} {:>10}", "Fmax (MHz)", fmt_opt_f64(cmp.skalp.fmax_mhz), fmt_opt_f64(cmp.reference.fmax_mhz));
    println!("{:<20} {:>10} {:>10}", "Bitstream (B)", fmt_opt(cmp.skalp.bitstream_bytes), fmt_opt(cmp.reference.bitstream_bytes));
    println!("{:<20} {:>10.2?} {:>10.2?}", "Compile time", cmp.skalp.compile_time, cmp.reference.compile_time);

    // PnR detail metrics
    if cmp.skalp.congestion.is_some() {
        println!();
        println!("  PnR Details:");
        println!("    Placement WL:    {}", cmp.skalp.placement_wirelength.map(|w| format!("{}", w)).unwrap_or("-".into()));
        println!("    Placement cost:  {}", cmp.skalp.placement_cost.map(|c| format!("{:.1}", c)).unwrap_or("-".into()));
        println!("    Utilization:     {}", cmp.skalp.utilization.map(|u| format!("{:.1}%", u * 100.0)).unwrap_or("-".into()));
        println!("    Congestion:      {}", fmt_opt_f64(cmp.skalp.congestion));
        println!("    Route iters:     {}", cmp.skalp.routing_iterations.map(|i| format!("{}", i)).unwrap_or("-".into()));
        println!("    Route success:   {}", cmp.skalp.routing_success.map(|s| format!("{}", s)).unwrap_or("-".into()));
        println!("    Worst slack:     {}", cmp.skalp.worst_slack_ns.map(|s| format!("{:.2} ns", s)).unwrap_or("-".into()));
        println!("    Crit path depth: {}", cmp.skalp.critical_path_stages.map(|n| format!("{}", n)).unwrap_or("-".into()));
    }

    if let Some(ref e) = cmp.skalp.error {
        println!("  skalp error: {}", e);
    }
    if let Some(ref e) = cmp.reference.error {
        println!("  reference error: {}", e);
    }
}

fn print_suite_summary(comparisons: &[QorComparison]) {
    println!("\n============================================================");
    println!("         SKALP QoR Benchmark (iCE40 HX8K)");
    println!("============================================================\n");

    // Resource table
    println!(
        "{:<20} | {:>5} {:>5} {:>5} | {:>5} {:>5} {:>5} | {:>5} {:>5}",
        "", "LUTs", "DFFs", "Carry", "LUTs", "DFFs", "Carry", "LUTs", "DFFs"
    );
    println!(
        "{:<20} | {:>17} | {:>17} | {:>11}",
        "Design", "skalp native", "yosys+nextpnr", "ratio (S/R)"
    );
    println!("{}+{}+{}+{}", "-".repeat(20), "-".repeat(18), "-".repeat(18), "-".repeat(12));

    let mut lut_ratios = Vec::new();
    let mut fmax_ratios = Vec::new();
    let mut both_pass = 0;
    let total = comparisons.len();

    for cmp in comparisons {
        let sl = cmp.skalp.luts.map(|n| format!("{:>5}", n)).unwrap_or_else(|| "    -".to_string());
        let sd = cmp.skalp.dffs.map(|n| format!("{:>5}", n)).unwrap_or_else(|| "    -".to_string());
        let sc = cmp.skalp.carries.map(|n| format!("{:>5}", n)).unwrap_or_else(|| "    -".to_string());
        let rl = cmp.reference.luts.map(|n| format!("{:>5}", n)).unwrap_or_else(|| "    -".to_string());
        let rd = cmp.reference.dffs.map(|n| format!("{:>5}", n)).unwrap_or_else(|| "    -".to_string());
        let rc = cmp.reference.carries.map(|n| format!("{:>5}", n)).unwrap_or_else(|| "    -".to_string());

        let lut_ratio = match (cmp.skalp.luts, cmp.reference.luts) {
            (Some(s), Some(r)) if r > 0 => {
                let ratio = s as f64 / r as f64;
                lut_ratios.push(ratio);
                format!("{:>5.2}", ratio)
            }
            _ => "    -".to_string(),
        };
        let dff_ratio = match (cmp.skalp.dffs, cmp.reference.dffs) {
            (Some(s), Some(r)) if r > 0 => format!("{:>5.2}", s as f64 / r as f64),
            _ => "    -".to_string(),
        };

        println!(
            "{:<20} | {} {} {} | {} {} {} | {} {}",
            cmp.design_name, sl, sd, sc, rl, rd, rc, lut_ratio, dff_ratio
        );

        if cmp.skalp.success && cmp.reference.success {
            both_pass += 1;
        }
    }

    // Timing table
    println!();
    println!(
        "{:<20} | {:>10} {:>10} | {:>10} {:>10}",
        "", "skalp", "ref", "skalp", "ref"
    );
    println!(
        "{:<20} | {:>21} | {:>21}",
        "Design", "Fmax (MHz)", "Bitstream (bytes)"
    );
    println!("{}+{}+{}", "-".repeat(20), "-".repeat(22), "-".repeat(22));

    for cmp in comparisons {
        let sf = cmp.skalp.fmax_mhz.map(|f| format!("{:>10.1}", f)).unwrap_or_else(|| "         -".to_string());
        let rf = cmp.reference.fmax_mhz.map(|f| format!("{:>10.1}", f)).unwrap_or_else(|| "         -".to_string());
        let sb = cmp.skalp.bitstream_bytes.map(|n| format!("{:>10}", n)).unwrap_or_else(|| "         -".to_string());
        let rb = cmp.reference.bitstream_bytes.map(|n| format!("{:>10}", n)).unwrap_or_else(|| "         -".to_string());

        if let (Some(s), Some(r)) = (cmp.skalp.fmax_mhz, cmp.reference.fmax_mhz) {
            if r > 0.0 {
                fmax_ratios.push(s / r);
            }
        }

        println!("{:<20} | {} {} | {} {}", cmp.design_name, sf, rf, sb, rb);
    }

    // PnR quality table
    println!();
    println!(
        "{:<20} | {:>6} {:>7} {:>5} {:>5} {:>7} {:>8} {:>6}",
        "Design", "PlcWL", "RteWL", "Cong", "Iter", "Util%", "Slack", "CritD"
    );
    println!("{}+{}", "-".repeat(20), "-".repeat(52));

    for cmp in comparisons {
        let plc_wl = cmp.skalp.placement_wirelength
            .map(|w| format!("{:>6}", w)).unwrap_or_else(|| "     -".to_string());
        let rte_wl = cmp.skalp.wirelength
            .map(|w| format!("{:>7}", w)).unwrap_or_else(|| "      -".to_string());
        let cong = cmp.skalp.congestion
            .map(|c| format!("{:>5.2}", c)).unwrap_or_else(|| "    -".to_string());
        let iter = cmp.skalp.routing_iterations
            .map(|i| format!("{:>5}", i)).unwrap_or_else(|| "    -".to_string());
        let util = cmp.skalp.utilization
            .map(|u| format!("{:>7.1}", u * 100.0)).unwrap_or_else(|| "      -".to_string());
        let slack = cmp.skalp.worst_slack_ns
            .map(|s| format!("{:>8.2}", s)).unwrap_or_else(|| "       -".to_string());
        let crit_d = cmp.skalp.critical_path_stages
            .map(|n| format!("{:>6}", n)).unwrap_or_else(|| "     -".to_string());

        println!(
            "{:<20} | {} {} {} {} {} {} {}",
            cmp.design_name, plc_wl, rte_wl, cong, iter, util, slack, crit_d
        );
    }

    // Summary statistics
    println!();
    if !lut_ratios.is_empty() {
        let geo_mean_lut = geometric_mean(&lut_ratios);
        println!("Geometric mean LUT ratio:  {:.2}x", geo_mean_lut);
    }
    // Fmax geomean: only include designs with DFFs (sequential logic)
    // Purely combinational designs have meaningless Fmax
    let seq_fmax_ratios: Vec<f64> = comparisons.iter().filter_map(|cmp| {
        let has_dffs = cmp.skalp.dffs.map(|d| d > 0).unwrap_or(false);
        if !has_dffs { return None; }
        match (cmp.skalp.fmax_mhz, cmp.reference.fmax_mhz) {
            (Some(s), Some(r)) if r > 0.0 => Some(s / r),
            _ => None,
        }
    }).collect();
    if !seq_fmax_ratios.is_empty() {
        let geo_mean_fmax = geometric_mean(&seq_fmax_ratios);
        println!("Geometric mean Fmax ratio: {:.2}x (sequential designs only, n={})", geo_mean_fmax, seq_fmax_ratios.len());
    }
    // Wirelength stats
    let wl_ratios: Vec<f64> = comparisons.iter().filter_map(|cmp| {
        match (cmp.skalp.placement_wirelength, cmp.skalp.wirelength) {
            (Some(p), Some(r)) if p > 0 => Some(r as f64 / p as f64),
            _ => None,
        }
    }).collect();
    if !wl_ratios.is_empty() {
        let avg_ratio = wl_ratios.iter().sum::<f64>() / wl_ratios.len() as f64;
        println!("Avg route/place WL ratio:  {:.1}x (lower = better routing efficiency)", avg_ratio);
    }
    println!("Designs passing both flows: {}/{}", both_pass, total);
}

fn geometric_mean(values: &[f64]) -> f64 {
    if values.is_empty() {
        return 0.0;
    }
    let log_sum: f64 = values.iter().map(|v| v.ln()).sum();
    (log_sum / values.len() as f64).exp()
}

// ===== Smoke test (non-ignored) =====

/// Verifies all benchmark designs compile through skalp parse → MIR → LIR → synthesis.
/// Does NOT run P&R (which is slow); P&R is tested in the individual #[ignore] tests.
#[test]
fn test_qor_designs_compile() {
    let mut failures = Vec::new();

    for design in benchmark_designs() {
        let name = design.name;
        let source = design.source;

        let result = std::panic::catch_unwind(|| {
            let hir = parse_and_build_hir(source).expect("parse failed");
            let mir = MirCompiler::new()
                .compile_to_mir(&hir)
                .expect("MIR failed");
            assert!(!mir.modules.is_empty(), "no modules in MIR");

            let module = mir.modules.last().unwrap();
            let lir_result = lower_mir_module_to_lir_with_bram(module);
            let library = get_stdlib_library("ice40").expect("ice40 library");
            let synth = synthesize_balanced(&lir_result.lir, &library);
            assert!(!synth.netlist.cells.is_empty(), "synthesis produced no cells");
        });

        match result {
            Ok(_) => println!("  {} ... ok", name),
            Err(e) => {
                let msg = if let Some(s) = e.downcast_ref::<String>() {
                    s.clone()
                } else if let Some(s) = e.downcast_ref::<&str>() {
                    s.to_string()
                } else {
                    "unknown panic".to_string()
                };
                println!("  {} ... FAILED: {}", name, msg);
                failures.push((name.to_string(), msg));
            }
        }
    }

    assert!(
        failures.is_empty(),
        "The following designs failed: {:?}",
        failures
    );
}

// ===== Individual design benchmarks =====

#[test]
#[ignore]
fn test_qor_counter8() {
    let designs = benchmark_designs();
    let design = &designs[0];
    let cmp = run_comparison(design);
    print_single_comparison(&cmp);
}

#[test]
#[ignore]
fn test_qor_shiftreg8() {
    let designs = benchmark_designs();
    let design = &designs[1];
    let cmp = run_comparison(design);
    print_single_comparison(&cmp);
}

#[test]
#[ignore]
fn test_qor_alu_8bit() {
    let designs = benchmark_designs();
    let design = &designs[2];
    let cmp = run_comparison(design);
    print_single_comparison(&cmp);
}

#[test]
#[ignore]
fn test_qor_uart_tx() {
    let designs = benchmark_designs();
    let design = &designs[3];
    let cmp = run_comparison(design);
    print_single_comparison(&cmp);
}

#[test]
#[ignore]
fn test_qor_regfile() {
    let designs = benchmark_designs();
    let design = &designs[4];
    let cmp = run_comparison(design);
    print_single_comparison(&cmp);
}

#[test]
#[ignore]
fn test_qor_counter16() {
    let designs = benchmark_designs();
    let design = &designs[5];
    let cmp = run_comparison(design);
    print_single_comparison(&cmp);
}

#[test]
#[ignore]
fn test_qor_spi_master() {
    let designs = benchmark_designs();
    let design = &designs[6];
    let cmp = run_comparison(design);
    print_single_comparison(&cmp);
}

// ===== Performance profiling =====

/// Profile each stage of the skalp flow to identify bottlenecks.
/// Breaks P&R into placement and routing to pinpoint scaling issues.
#[test]
#[ignore]
fn test_qor_profile_stages() {
    println!(
        "\n{:<20} {:>8} {:>8} {:>8} {:>10} {:>10} {:>10}  {:<}",
        "Design", "Parse", "MIR", "LIR", "Synth", "Place", "Route", "Netlist"
    );
    println!("{}", "-".repeat(100));

    for design in benchmark_designs() {
        let t0 = Instant::now();
        let hir = match parse_and_build_hir(design.source) {
            Ok(h) => h,
            Err(e) => {
                println!("{:<20} PARSE FAILED: {}", design.name, e);
                continue;
            }
        };
        let t_parse = t0.elapsed();

        let t1 = Instant::now();
        let mir = match MirCompiler::new().compile_to_mir(&hir) {
            Ok(m) => m,
            Err(e) => {
                println!("{:<20} {:>8.2?} MIR FAILED: {}", design.name, t_parse, e);
                continue;
            }
        };
        let t_mir = t1.elapsed();

        let module = mir.modules.last().unwrap();

        let t2 = Instant::now();
        let lir_result = lower_mir_module_to_lir_with_bram(module);
        let t_lir = t2.elapsed();

        let library = get_stdlib_library("ice40").expect("ice40 library");

        let t3 = Instant::now();
        let synth_result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            synthesize_balanced(&lir_result.lir, &library)
        }));
        let t_synth = t3.elapsed();

        let synth = match synth_result {
            Ok(s) => s,
            Err(_) => {
                println!(
                    "{:<20} {:>8.2?} {:>8.2?} {:>8.2?} {:>10.2?} (SYNTH PANIC)",
                    design.name, t_parse, t_mir, t_lir, t_synth
                );
                continue;
            }
        };

        let netlist_cells = synth.netlist.cells.len();
        let netlist_nets = synth.netlist.nets.len();

        // Packing (needed to detect carry chains)
        let mut packer = CellPacker::new(&synth.netlist);
        let packing = packer.pack();
        let lcs = packing.stats.total_lcs;

        // Placement only
        let device = skalp_place_route::Ice40Device::new(Ice40Variant::Hx8k);
        let placer_config = PlacerConfig {
            max_iterations: 1000,
            initial_temperature: 50.0,
            ..Default::default()
        };
        let t_place_start = Instant::now();
        let mut placer = Placer::new(placer_config, device.clone());
        let placement = if packing.carry_chains.is_empty() {
            placer.place(&synth.netlist)
        } else {
            placer.place_with_carry_chains(&synth.netlist, &packing.carry_chains)
        };
        let t_place = t_place_start.elapsed();

        let placement = match placement {
            Ok(p) => p,
            Err(e) => {
                println!(
                    "{:<20} {:>8.2?} {:>8.2?} {:>8.2?} {:>10.2?} {:>10.2?} PLACE ERR  cells={} nets={} lcs={}",
                    design.name, t_parse, t_mir, t_lir, t_synth, t_place,
                    netlist_cells, netlist_nets, lcs,
                );
                println!("  Placement error: {}", e);
                continue;
            }
        };

        // Routing only
        let t_route_start = Instant::now();
        let mut router = Router::new(RouterConfig::default(), device);
        let route_result = router.route(&synth.netlist, &placement);
        let t_route = t_route_start.elapsed();

        let route_status = match &route_result {
            Ok(_) => "ok".to_string(),
            Err(e) => format!("ERR: {}", e),
        };

        println!(
            "{:<20} {:>8.2?} {:>8.2?} {:>8.2?} {:>10.2?} {:>10.2?} {:>10.2?}  cells={} nets={} lcs={} route={}",
            design.name, t_parse, t_mir, t_lir, t_synth, t_place, t_route,
            netlist_cells, netlist_nets, lcs, route_status,
        );
    }
}

// ===== Full benchmark suite =====

#[test]
#[ignore]
fn test_qor_benchmark_suite() {
    if !tools_available() {
        println!("SKIP: external tools not available (yosys, nextpnr-ice40, icetime, icepack)");
        return;
    }

    let designs = benchmark_designs();
    let comparisons: Vec<QorComparison> = designs.iter().map(|d| run_comparison(d)).collect();

    print_suite_summary(&comparisons);

    // Soft assert: at least half the designs pass both flows
    let both_pass = comparisons
        .iter()
        .filter(|c| c.skalp.success && c.reference.success)
        .count();
    assert!(
        both_pass >= designs.len() / 2,
        "Only {}/{} designs passed both flows",
        both_pass,
        designs.len()
    );
}

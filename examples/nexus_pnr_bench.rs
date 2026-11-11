//! Place (and optionally route) a design on the real Nexus device data,
//! reporting how long each stage takes and how good the placement is.
//!
//! cargo run --release --features nexus-real --example nexus_pnr_bench -- design.sk Top [--route]
//!
//! The synthesized netlist is cached next to the source as
//! `<source>.<preset>.netlist.json`. `PRESET=compress2` (etc.) picks the
//! synthesis preset (default `quick`); `PATHS=n` prints the n worst paths
//! after routing.

use skalp_lir::GateNetlist;
use skalp_place_route::device::nexus::prjoxide_load::{find_database, load_tilegrid};
use skalp_place_route::device::nexus::{prjoxide_graph::ClockRegions, sites, NexusDevice};
use skalp_place_route::{NexusVariant, PnrConfig};
use std::time::Instant;

fn synthesize(path: &std::path::Path, top_name: &str) -> GateNetlist {
    let preset_name = std::env::var("PRESET").unwrap_or_else(|_| "quick".into());
    let preset = match preset_name.as_str() {
        "balanced" => skalp_lir::synth::SynthPreset::Balanced,
        "full" => skalp_lir::synth::SynthPreset::Full,
        "timing" => skalp_lir::synth::SynthPreset::Timing,
        "area" => skalp_lir::synth::SynthPreset::Area,
        "resyn2" => skalp_lir::synth::SynthPreset::Resyn2,
        "compress2" => skalp_lir::synth::SynthPreset::Compress2,
        "auto" => skalp_lir::synth::SynthPreset::Auto,
        _ => skalp_lir::synth::SynthPreset::Quick,
    };
    let flat = if std::env::var("FLAT").is_ok_and(|v| v == "1") {
        ".flat"
    } else {
        ""
    };
    let cache = path.with_extension(format!("{preset_name}{flat}.netlist.json"));
    if let Ok(text) = std::fs::read_to_string(&cache) {
        return serde_json::from_str(&text).unwrap();
    }
    let ctx = skalp_frontend::parse_and_build_compilation_context(path).unwrap();
    let mir = skalp_mir::MirCompiler::new()
        .with_optimization_level(skalp_mir::OptimizationLevel::None)
        .compile_to_mir_with_modules(&ctx.main_hir, &ctx.module_hirs)
        .unwrap();
    let (hier, _) = skalp_lir::lower_mir_hierarchical_for_optimize_first(&mir);
    let library = skalp_lir::get_stdlib_library("nexus").unwrap();
    // FLAT=1: synthesize the flattened design in one piece instead of one
    // module at a time (logic can then be optimized across module boundaries).
    let mut netlist = if !flat.is_empty() {
        skalp_lir::synthesize(&hier.flatten(), &library, preset).netlist
    } else {
        skalp_lir::synthesize_hierarchical(&hier, &library, preset).flatten_for_library(&library)
    };
    let top = mir.modules.iter().find(|m| m.name == top_name).unwrap();
    // Hierarchical synthesis names the top's nets `top.<port>`; flat
    // synthesis keeps the port names.
    let prefix = if flat.is_empty() { "top." } else { "" };
    let constraints: indexmap::IndexMap<_, _> = top
        .ports
        .iter()
        .filter_map(|p| {
            Some((
                format!("{prefix}{}", p.name),
                p.physical_constraints.clone()?,
            ))
        })
        .collect();
    // Like the CLI, drop ports hierarchical synthesis promoted for child instances
    let names: Vec<String> = netlist.nets.iter().map(|n| n.name.clone()).collect();
    let is_real_port = |net: &skalp_lir::GateNetId| {
        let name = &names[net.0 as usize];
        let base = name.split('[').next().unwrap_or(name);
        let port = base.strip_prefix("top.").unwrap_or(base);
        top.ports.iter().any(|p| p.name == port)
    };
    netlist.inputs.retain(is_real_port);
    netlist.outputs.retain(is_real_port);
    skalp_lir::insert_io_buffers(&mut netlist, &library, &constraints);
    std::fs::write(&cache, serde_json::to_string(&netlist).unwrap()).unwrap();
    netlist
}

fn main() {
    tracing_subscriber::fmt()
        .with_env_filter(
            tracing_subscriber::EnvFilter::try_from_default_env().unwrap_or_else(|_| "info".into()),
        )
        .init();
    let args: Vec<String> = std::env::args().collect();
    let source = std::path::PathBuf::from(&args[1]);
    let top = &args[2];
    let route = args.iter().any(|a| a == "--route");
    let db = find_database().expect("prjoxide database");

    let t = Instant::now();
    let netlist = synthesize(&source, top);
    println!(
        "netlist: {} cells, {} nets ({:.1} s)",
        netlist.cells.len(),
        netlist.nets.len(),
        t.elapsed().as_secs_f64()
    );

    let t = Instant::now();
    if route {
        let mut config = PnrConfig::default();
        // Experiment knobs: PLACER_ITERS, TIMING_WEIGHT, SEED
        if let Some(v) = std::env::var("SEED").ok().and_then(|v| v.parse().ok()) {
            config.placer.seed = v;
        }
        if let Some(v) = std::env::var("PLACER_ITERS")
            .ok()
            .and_then(|v| v.parse().ok())
        {
            config.placer.max_iterations = v;
        }
        if let Some(v) = std::env::var("TIMING_WEIGHT")
            .ok()
            .and_then(|v| v.parse().ok())
        {
            config.placer.timing_weight = v;
        }
        let result = skalp_place_route::pnr::place_and_route_nexus_real(
            &netlist,
            NexusVariant::Lfcpnx100,
            config,
            &db,
        );
        match result {
            Ok(r) => {
                println!(
                    "place+route: success {} in {} iterations, {} unrouted, wirelength {}, {:.1} MHz ({:.1} s)",
                    r.routing.success,
                    r.routing.iterations,
                    r.routing.unrouted_connections,
                    r.routing.wirelength,
                    r.timing.as_ref().map_or(0.0, |t| t.design_frequency),
                    t.elapsed().as_secs_f64()
                );
                if let Some(timing) = &r.timing {
                    print_paths(timing);
                }
            }
            Err(e) => println!(
                "place+route failed: {e} ({:.1} s)",
                t.elapsed().as_secs_f64()
            ),
        }
        return;
    }

    let variant = NexusVariant::Lfcpnx100;
    let tiles = load_tilegrid(&db, variant).unwrap();
    let clocks =
        ClockRegions::load(&db, variant.prjoxide_family(), variant.prjoxide_device()).unwrap();
    let region = sites::design_region(&tiles, &clocks, &[], netlist.cells.len(), 0, 0);
    println!("region {:?}", region);
    let device =
        NexusDevice::from_prjoxide_region(variant, &db, region.window, Some(region.logic_rows))
            .unwrap();
    println!("device loaded ({:.1} s)", t.elapsed().as_secs_f64());
    let netlist = skalp_place_route::bitstream::prepare::drive_constant_inputs(&netlist, &device);
    let logic = skalp_place_route::bitstream::prepare::logic_only_netlist(&netlist);
    let mut packer = skalp_place_route::CellPacker::new(&logic);
    let packing = packer.pack();
    let t = Instant::now();
    let mut placer = skalp_place_route::Placer::new(PnrConfig::default().placer, device);
    let placement = placer
        .place_with_carry_chains(&logic, &packing.carry_chains)
        .unwrap();
    println!("placement: {:.1} s", t.elapsed().as_secs_f64());
    skalp_place_route::Placer::<NexusDevice>::log_placement_stats(&placement, &logic);
}

/// Print the worst paths, one line per net: arrival, routing into the driver,
/// delay through it, driver type and tile.
fn print_paths(timing: &skalp_place_route::timing::TimingReport) {
    let count = std::env::var("PATHS")
        .ok()
        .and_then(|v| v.parse().ok())
        .unwrap_or(1);
    for path in timing.critical_paths.iter().take(count) {
        let route: f64 = path.steps.iter().map(|s| s.route_in).sum();
        let cells: f64 = path.steps.iter().map(|s| s.cell_delay).sum();
        println!(
            "\npath {:.3} ns ({} nets, cells {:.3} ns, routing {:.3} ns) -> {}",
            path.delay,
            path.steps.len(),
            cells,
            route,
            path.destination
        );
        for step in &path.steps {
            println!(
                "  {:8.3} +{:6.3} +{:6.3}  {:12} {:>10}  {}",
                step.arrival,
                step.route_in,
                step.cell_delay,
                step.cell_type,
                step.tile
                    .map_or(String::new(), |(x, y)| format!("({x},{y})")),
                step.net
            );
        }
    }
}

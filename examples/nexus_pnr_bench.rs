//! Place (and optionally route) a design on the real Nexus device data,
//! reporting how long each stage takes and how good the placement is.
//!
//! cargo run --release --features nexus-real --example nexus_pnr_bench -- design.sk Top [--route]
//!
//! The synthesized netlist is cached next to the source as `<source>.netlist.json`.

use skalp_lir::GateNetlist;
use skalp_place_route::device::nexus::prjoxide_load::{find_database, load_tilegrid};
use skalp_place_route::device::nexus::{prjoxide_graph::ClockRegions, sites, NexusDevice};
use skalp_place_route::{NexusVariant, PnrConfig};
use std::time::Instant;

fn synthesize(path: &std::path::Path, top_name: &str) -> GateNetlist {
    let cache = path.with_extension("netlist.json");
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
    let mut netlist =
        skalp_lir::synthesize_hierarchical(&hier, &library, skalp_lir::synth::SynthPreset::Quick)
            .flatten();
    let top = mir.modules.iter().find(|m| m.name == top_name).unwrap();
    let constraints: indexmap::IndexMap<_, _> = top
        .ports
        .iter()
        .filter_map(|p| Some((format!("top.{}", p.name), p.physical_constraints.clone()?)))
        .collect();
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
        let result = skalp_place_route::pnr::place_and_route_nexus_real(
            &netlist,
            NexusVariant::Lfcpnx100,
            PnrConfig::default(),
            &db,
        );
        match result {
            Ok(r) => println!(
                "place+route: success {} in {} iterations, {} unrouted, wirelength {}, {:.1} MHz ({:.1} s)",
                r.routing.success,
                r.routing.iterations,
                r.routing.unrouted_connections,
                r.routing.wirelength,
                r.timing.as_ref().map_or(0.0, |t| t.design_frequency),
                t.elapsed().as_secs_f64()
            ),
            Err(e) => println!("place+route failed: {e} ({:.1} s)", t.elapsed().as_secs_f64()),
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

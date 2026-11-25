//! Register replication after placement: a register driving many loads
//! spread over the die is copied, each copy driving the loads near it,
//! without changing what the design does

use skalp_lir::gate_netlist::GateNetlist;
use skalp_place_route::placer::control_sets::{control_set_conflicts, control_sets};
use skalp_place_route::{
    CellPacker, NexusDevice, NexusVariant, Placer, PlacerConfig, ReplicationConfig,
    REPLICA_OF_PARAM,
};
use skalp_testing::testbench::Testbench;

const TOP: &str = "FanoutReplicationCheck";

fn fixture_path(name: &str) -> String {
    format!("{}/tests/fixtures/{}", env!("CARGO_MANIFEST_DIR"), name)
}

/// What replication did to the fixture
#[derive(Default, Debug)]
struct Outcome {
    /// Loads of each driver replicated
    replicated: Vec<usize>,
    copies: usize,
    slack_before_ns: f64,
    slack_after_ns: f64,
}

/// Place the netlist on a Nexus device and replicate its registers against
/// a 1 ns clock, then check the result: the copies are placed on BELs of
/// their own, control groups keep one control set, the loads are shared out
/// between the original and its copies, and every copy names its original
fn place_and_replicate(netlist: GateNetlist, outcome: &std::sync::Mutex<Outcome>) -> GateNetlist {
    let device = NexusDevice::new(NexusVariant::Lifcl40);
    let config = PlacerConfig {
        replication: ReplicationConfig {
            min_fanout: 32,
            max_fanout: 32,
            ..ReplicationConfig::default()
        },
        ..PlacerConfig::default()
    };
    let mut placer = Placer::new(config, device.clone());
    let packing = CellPacker::new(&netlist).pack();
    let placement = if packing.carry_chains.is_empty() {
        placer.place(&netlist)
    } else {
        placer.place_with_carry_chains(&netlist, &packing.carry_chains)
    }
    .expect("placement");
    let found = placer
        .replicate_high_fanout(&netlist, &placement, |_| 1.0)
        .expect("replication")
        .expect("nothing was replicated");
    let out = &found.netlist;

    // Every cell that was placed stays where it was
    for cell in &netlist.cells {
        assert_eq!(
            placement.get(cell.id),
            found.placement.get(cell.id),
            "{} moved",
            cell.path
        );
    }
    let mut owner = std::collections::HashMap::new();
    for (cell, loc) in found.placement.cells() {
        if let Some(other) = owner.insert((loc.tile_x, loc.tile_y, loc.bel_index), *cell) {
            panic!(
                "{} and {} share a BEL",
                out.cells[other.0 as usize].path, out.cells[cell.0 as usize].path
            );
        }
    }
    let conflicts = control_set_conflicts(&device, &found.placement, &control_sets(out));
    assert!(conflicts.is_empty(), "control sets mixed: {conflicts:?}");

    let copies: Vec<_> = out.cells[netlist.cells.len()..].iter().collect();
    assert_eq!(
        copies.len(),
        found.drivers.iter().map(|r| r.copies).sum::<usize>()
    );
    for copy in &copies {
        let original = copy
            .parameters
            .get(REPLICA_OF_PARAM)
            .unwrap_or_else(|| panic!("{} does not name its original", copy.path));
        let source = netlist
            .cells
            .iter()
            .find(|c| &c.path == original)
            .expect("the original is in the netlist");
        // Each input carries what the original's does: the same net, or
        // one driven by a copy of the same cell
        let origin = |net: skalp_lir::gate_netlist::GateNetId| {
            let driver = out.nets[net.0 as usize].driver?;
            let cell = &out.cells[driver.0 as usize];
            Some(
                cell.parameters
                    .get(REPLICA_OF_PARAM)
                    .unwrap_or(&cell.path)
                    .clone(),
            )
        };
        assert_eq!(copy.inputs.len(), source.inputs.len());
        for (a, b) in copy.inputs.iter().zip(&source.inputs) {
            assert!(
                a == b || origin(*a) == origin(*b),
                "{} reads other inputs",
                copy.path
            );
        }
        assert_eq!(copy.clock, source.clock);
        assert_eq!(copy.cell_type, source.cell_type);
        assert!(
            found.placement.get(copy.id).is_some(),
            "{} unplaced",
            copy.path
        );
    }
    // The loads are split between the original and its copies, at least
    // in halves (a few more can come along with a control group moved
    // whole), none lost or added
    for r in &found.drivers {
        let drivers = out
            .cells
            .iter()
            .filter(|c| c.path == r.path || c.parameters.get(REPLICA_OF_PARAM) == Some(&r.path));
        let mut total = 0;
        for d in drivers {
            let loads = out
                .cells
                .iter()
                .map(|c| c.inputs.iter().filter(|&&n| n == d.outputs[0]).count())
                .sum::<usize>();
            assert!(
                loads <= r.loads / 2 + 8,
                "{} drives {loads} of {} loads",
                d.path,
                r.loads
            );
            total += loads;
        }
        assert_eq!(total, r.loads, "{}: loads lost or added", r.path);
        assert!(r.slack_after_ns > r.slack_before_ns);
    }
    let mut o = outcome.lock().unwrap();
    o.replicated = found.drivers.iter().map(|r| r.loads).collect();
    o.copies = copies.len();
    o.slack_before_ns = found.drivers[0].slack_before_ns;
    o.slack_after_ns = found.drivers[0].slack_after_ns;
    drop(o);
    found.netlist
}

/// The write enable is split into copies near its loads, each copy placed
/// legally, and the design does the same thing cycle for cycle
#[tokio::test]
async fn test_high_fanout_register_is_replicated_and_behaves_the_same() {
    let path = fixture_path("fanout_replication_check.sk");
    let outcome = std::sync::Mutex::new(Outcome::default());
    let pass = |netlist| place_and_replicate(netlist, &outcome);
    let mut replicated = Testbench::gate_level_with_netlist_pass(&path, TOP, "nexus", &pass)
        .await
        .unwrap();
    let mut plain = Testbench::gate_level_with_top_and_library(&path, TOP, "nexus")
        .await
        .unwrap();

    let outcome = outcome.into_inner().unwrap();
    assert!(
        outcome.replicated.iter().any(|&loads| loads >= 384),
        "the write enable was not replicated: {outcome:?}"
    );
    assert!(outcome.copies >= 1, "no copies");
    assert!(
        outcome.slack_after_ns > outcome.slack_before_ns,
        "{outcome:?}"
    );

    let mut changed = 0;
    let mut last = None;
    for cycle in 0..300 {
        replicated.clock(1).await;
        plain.clock(1).await;
        let want = plain.get_u64("sig").await;
        let got = replicated.get_u64("sig").await;
        assert_eq!(got, want, "cycle {cycle}: {got:#06x} vs {want:#06x}");
        if last.is_some_and(|l| l != want) {
            changed += 1;
        }
        last = Some(want);
    }
    assert!(changed > 100, "the output hardly changed ({changed} times)");
}

/// Replication off, or a clock every path meets, leaves the netlist alone
#[test]
fn test_replication_only_when_paths_fail() {
    let ctx = skalp_frontend::parse_and_build_compilation_context(std::path::Path::new(
        &fixture_path("fanout_replication_check.sk"),
    ))
    .unwrap();
    let mut mir = skalp_mir::MirCompiler::new()
        .compile_to_mir_with_modules(&ctx.main_hir, &ctx.module_hirs)
        .unwrap();
    assert!(mir.retain_reachable_from(TOP));
    let (hier, _) = skalp_lir::lower_mir_hierarchical_for_optimize_first(&mir);
    let library = skalp_lir::get_stdlib_library("nexus").unwrap();
    let netlist = skalp_lir::synthesize_hierarchical(
        &hier,
        &library,
        skalp_lir::synth::SynthPreset::Balanced,
    )
    .flatten();

    let device = NexusDevice::new(NexusVariant::Lifcl40);
    let mut placer = Placer::new(PlacerConfig::default(), device.clone());
    let placement = placer.place(&netlist).expect("placement");
    assert!(
        placer
            .replicate_high_fanout(&netlist, &placement, |_| 1000.0)
            .unwrap()
            .is_none(),
        "replicated with every path meeting a 1 MHz clock"
    );
    let mut off = PlacerConfig::default();
    off.replication.enabled = false;
    let placer = Placer::new(off, device);
    assert!(placer
        .replicate_high_fanout(&netlist, &placement, |_| 1.0)
        .unwrap()
        .is_none());
}

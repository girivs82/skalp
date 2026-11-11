//! iCE40 block RAM in every width: the router connects a memory's data to the
//! pins the block's mode uses, and the bitstream sets that mode.
//!
//! `SB_RAM40_4K` keeps 16 physical data pins in every mode (yosys
//! `cells_sim.v`): 512x8 uses the even pins, 1024x4 pins 1, 5, 9 and 13, and
//! 2048x2 pins 3 and 11. The mode bits are `RamConfig.CBIT_0/1` (write) and
//! `CBIT_2/3` (read) in the block's top tile (nextpnr `ice40/bitstream.cc`).

use skalp_lir::gate_netlist::GateNetlist;
use skalp_lir::tech_library::CellFunction;
use skalp_place_route::{place_and_route, Device, Ice40Device, Ice40Variant, PnrConfig, TileType};

/// A memory `width` bits wide, as deep as one block holds, written and read
/// from counters so every RAM pin is driven by logic and routed.
fn memory_source(width: usize) -> String {
    let depth = 4096 / width;
    let addr = depth.trailing_zeros();
    format!(
        "entity Mem {{
    in clk: clock
    out q: bit[{width}]
}}
impl Mem {{
    #[memory(depth = {depth})]
    signal mem: bit[{width}][{depth}]
    signal wa: bit[{addr}] = 0
    signal ra: bit[{addr}] = 0
    signal wd: bit[{width}] = 0
    signal rd: bit[{width}] = 0
    signal acc: bit[{width}] = 0
    on(clk.rise) {{
        mem[wa] = wd
        rd = mem[ra]
        wa = wa + 1
        ra = ra + 3
        wd = wd + 5
        acc = acc ^ rd
    }}
    q = acc
}}
"
    )
}

fn synthesize(width: usize) -> GateNetlist {
    let dir = std::env::temp_dir().join(format!("skalp_ice40_bram_{}_{width}", std::process::id()));
    std::fs::create_dir_all(&dir).unwrap();
    let path = dir.join("mem.sk");
    std::fs::write(&path, memory_source(width)).unwrap();
    let ctx = skalp_frontend::parse_and_build_compilation_context(&path).unwrap();
    let mir = skalp_mir::MirCompiler::new()
        .with_optimization_level(skalp_mir::OptimizationLevel::None)
        .compile_to_mir_with_modules(&ctx.main_hir, &ctx.module_hirs)
        .unwrap();
    let (hier, _) = skalp_lir::lower_mir_hierarchical_for_optimize_first(&mir);
    let library = skalp_lir::get_stdlib_library("ice40").unwrap();
    skalp_lir::synthesize_hierarchical(&hier, &library, skalp_lir::synth::SynthPreset::Quick)
        .flatten()
}

/// The `RamConfig.CBIT_n` bits of the `.ramt_tile x y` section of `asc`.
fn mode_bits(asc: &str, x: u32, y: u32, positions: &[(usize, usize)]) -> Vec<bool> {
    let header = format!(".ramt_tile {x} {y}\n");
    let start = asc
        .find(&header)
        .expect("the block's top tile is configured")
        + header.len();
    let rows: Vec<&str> = asc[start..].lines().take(16).collect();
    positions
        .iter()
        .map(|&(row, col)| rows[row].as_bytes()[col] == b'1')
        .collect()
}

#[test]
fn block_ram_data_lands_on_the_pins_of_its_mode() {
    for (width, mode, pins) in [
        (16usize, 0u8, (0..16).collect::<Vec<usize>>()),
        (8, 1, vec![0, 2, 4, 6, 8, 10, 12, 14]),
        (4, 2, vec![1, 5, 9, 13]),
        (2, 3, vec![3, 11]),
    ] {
        let netlist = synthesize(width);
        let rams: Vec<_> = netlist
            .cells
            .iter()
            .filter(|c| c.function == Some(CellFunction::Ram))
            .collect();
        assert_eq!(rams.len(), 1, "{width}-bit memory takes one block");
        // Every cell's id is its index, and every cell input is driven.
        for (index, cell) in netlist.cells.iter().enumerate() {
            assert_eq!(
                cell.id.0 as usize, index,
                "{width}-bit: id of {}",
                cell.path
            );
        }
        let driven: std::collections::HashSet<_> = netlist
            .cells
            .iter()
            .flat_map(|c| c.outputs.iter().copied())
            .chain(netlist.inputs.iter().copied())
            .collect();
        for cell in &netlist.cells {
            for net in &cell.inputs {
                assert!(
                    driven.contains(net),
                    "{width}-bit: input {} of {} is driven",
                    netlist.nets[net.0 as usize].name,
                    cell.path
                );
            }
        }
        let ram = rams[0];
        let ports = ram.ram_ports().unwrap();
        assert_eq!(ports.data_width(), width);
        assert!(
            !ram.parameters.contains_key("READ_MODE"),
            "the mapper records the width, not a mode"
        );

        for variant in [Ice40Variant::Hx1k, Ice40Variant::Hx8k] {
            let result = place_and_route(&netlist, variant, PnrConfig::fast())
                .unwrap_or_else(|e| panic!("{width}-bit memory on {variant:?}: {e}"));
            let device = Ice40Device::new(variant);
            let loc = result.placement.placements[&ram.id];
            let (x, y) = (loc.tile_x, loc.tile_y);
            // The block spans a bottom tile and the top tile above it; its pins
            // are split between the two (and the next block has the same names).
            let top = match Device::tile_at(&device, x, y).map(|t| t.tile_type()) {
                Some(TileType::RamTop) => y,
                Some(TileType::RamBottom) => y + 1,
                other => panic!("{width}-bit memory placed on {other:?}"),
            };
            let pin_wire = |name: &str| {
                device
                    .bel_pin_wire(x, top, name)
                    .or_else(|| device.bel_pin_wire(x, top - 1, name))
                    .unwrap_or_else(|| panic!("no {name} in the block at ({x}, {top})"))
            };
            let route = |net| {
                result
                    .routing
                    .routes
                    .get(&net)
                    .unwrap_or_else(|| panic!("{variant:?} {width}-bit: net {net:?} is routed"))
            };

            for (bit, &pin) in pins.iter().enumerate() {
                let wd = ram.inputs[ports.write_data.start + bit];
                let wire = pin_wire(&format!("ram/WDATA_{pin}"));
                assert!(
                    route(wd).sinks.contains(&wire),
                    "{variant:?} {width}-bit: write data bit {bit} reaches WDATA_{pin}"
                );
                let q = ram.outputs[bit];
                assert_eq!(
                    route(q).source,
                    pin_wire(&format!("ram/RDATA_{pin}")),
                    "{variant:?} {width}-bit: read data bit {bit} comes from RDATA_{pin}"
                );
            }
            for bit in 0..ports.read_addr.len() {
                let ra = ram.inputs[ports.read_addr.start + bit];
                let wire = pin_wire(&format!("ram/RADDR_{bit}"));
                assert!(route(ra).sinks.contains(&wire), "RADDR_{bit}");
            }
            // The active-low write mask is left unconnected (0: write every bit).
            let masks: Vec<_> = (0..16)
                .map(|i| pin_wire(&format!("ram/MASK_{i}")))
                .collect();
            for route in result.routing.routes.values() {
                assert!(
                    !route.sinks.iter().any(|s| masks.contains(s)),
                    "{variant:?} {width}-bit: nothing drives MASK"
                );
            }

            // Mode bits in the top tile: CBIT_0/1 write mode, CBIT_2/3 read mode.
            let asc = result.to_icestorm_ascii_with_netlist(Some(&netlist));
            // RamConfig.CBIT_0..3 are B1[7], B0[7], B3[7], B2[7] on every device.
            let positions = [(1, 7), (0, 7), (3, 7), (2, 7)];
            let bits = mode_bits(&asc, x, top, &positions);
            let expected = [mode & 1 != 0, mode & 2 != 0, mode & 1 != 0, mode & 2 != 0];
            assert_eq!(bits, expected, "{variant:?} {width}-bit mode bits");
        }
    }
}

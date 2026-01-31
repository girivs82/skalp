# Native iCE40 Place & Route Architecture

## Overview

This document describes the architecture for implementing native iCE40 FPGA place and route in Rust, eliminating the need for external tools like nextpnr while maintaining compatibility with the IceStorm bitstream format.

## Goals

1. **Native Rust Implementation**: No external tool dependencies for P&R
2. **IceStorm Compatibility**: Generate bitstreams compatible with `icepack`/`iceprog`
3. **Performance**: Leverage Rust's performance and parallelism (rayon)
4. **Integration**: Seamless integration with existing SKALP synthesis pipeline
5. **Extensibility**: Architecture that can be extended to ECP5 and other FPGAs

## Architecture Diagram

```
┌─────────────────────────────────────────────────────────────────────┐
│                         SKALP Synthesis                              │
│  Source → HIR → MIR → LIR → GateNetlist (with iCE40 cells)          │
└─────────────────────────────────────────────────────────────────────┘
                                   │
                                   ▼
┌─────────────────────────────────────────────────────────────────────┐
│                    skalp-place-route crate                          │
├─────────────────────────────────────────────────────────────────────┤
│                                                                      │
│  ┌──────────────┐    ┌──────────────┐    ┌──────────────┐          │
│  │   Device     │    │   Placer     │    │   Router     │          │
│  │   Database   │───▶│              │───▶│              │          │
│  └──────────────┘    └──────────────┘    └──────────────┘          │
│         │                   │                   │                   │
│         ▼                   ▼                   ▼                   │
│  ┌──────────────┐    ┌──────────────┐    ┌──────────────┐          │
│  │  ChipDB      │    │  Placement   │    │  Routing     │          │
│  │  (ice40)     │    │  Result      │    │  Result      │          │
│  └──────────────┘    └──────────────┘    └──────────────┘          │
│                                                 │                   │
│                                                 ▼                   │
│                             ┌──────────────────────────────┐        │
│                             │   Bitstream Generator        │        │
│                             │   (IceStorm ASCII/Binary)    │        │
│                             └──────────────────────────────┘        │
│                                          │                          │
└──────────────────────────────────────────│──────────────────────────┘
                                           ▼
                                    .asc / .bin file
                                           │
                                           ▼
                                    ┌──────────────┐
                                    │  iceprog     │
                                    │  (external)  │
                                    └──────────────┘
```

## Module Structure

```
crates/skalp-place-route/
├── src/
│   ├── lib.rs              # Public API
│   ├── device/
│   │   ├── mod.rs          # Device trait and common types
│   │   ├── ice40/
│   │   │   ├── mod.rs      # iCE40 device implementation
│   │   │   ├── chipdb.rs   # ChipDB parser/loader
│   │   │   ├── tiles.rs    # Tile definitions (LOGIC, IO, RAM, etc.)
│   │   │   ├── bels.rs     # Basic Elements (LUT, FF, CARRY, etc.)
│   │   │   ├── pips.rs     # Programmable Interconnect Points
│   │   │   └── routing.rs  # Routing resource graph
│   │   └── ecp5/           # Future: ECP5 support
│   ├── placer/
│   │   ├── mod.rs          # Placer trait and common types
│   │   ├── analytical.rs   # Analytical placement (quadratic)
│   │   ├── annealing.rs    # Simulated annealing refinement
│   │   ├── legalization.rs # Legalize placement to valid sites
│   │   └── timing.rs       # Timing-driven placement
│   ├── router/
│   │   ├── mod.rs          # Router trait and common types
│   │   ├── pathfinder.rs   # PathFinder negotiated congestion
│   │   ├── astar.rs        # A* shortest path
│   │   ├── timing.rs       # Timing-driven routing
│   │   └── rip_up.rs       # Rip-up and reroute
│   ├── bitstream/
│   │   ├── mod.rs          # Bitstream generation trait
│   │   ├── icestorm.rs     # IceStorm ASCII/binary format
│   │   └── cram.rs         # Configuration RAM manipulation
│   ├── timing/
│   │   ├── mod.rs          # Static timing analysis
│   │   ├── delays.rs       # Delay models
│   │   └── constraints.rs  # SDC-like timing constraints
│   └── pnr.rs              # Top-level P&R orchestration
└── chipdb/
    ├── ice40-hx1k.bin      # Pre-compiled chip database
    ├── ice40-hx8k.bin
    ├── ice40-lp1k.bin
    └── ice40-up5k.bin
```

## Core Data Structures

### Device Database

```rust
/// iCE40 chip database - describes the FPGA architecture
pub struct Ice40ChipDb {
    /// Device variant (HX1K, HX8K, LP1K, UP5K, etc.)
    pub device: Ice40Device,

    /// Grid dimensions
    pub width: u32,
    pub height: u32,

    /// Tile definitions indexed by (x, y)
    pub tiles: Vec<Vec<Tile>>,

    /// Global routing resources
    pub globals: Vec<GlobalNet>,

    /// Package pin mappings
    pub packages: HashMap<String, PackagePins>,
}

#[derive(Clone, Debug)]
pub enum Ice40Device {
    Hx1k,
    Hx4k,
    Hx8k,
    Lp1k,
    Lp4k,
    Lp8k,
    Up5k,
}

/// A tile in the FPGA fabric
pub struct Tile {
    pub tile_type: TileType,
    pub x: u32,
    pub y: u32,

    /// Basic Elements in this tile
    pub bels: Vec<Bel>,

    /// Local routing muxes
    pub local_muxes: Vec<LocalMux>,

    /// Connections to neighbors
    pub pips: Vec<Pip>,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum TileType {
    Logic,      // PLB with 8 LUTs
    Io,         // I/O tile
    RamT,       // RAM tile (top)
    RamB,       // RAM tile (bottom)
    Dsp,        // DSP tile (UP5K only)
    Ipcon,      // IP connection tile
    Empty,      // No resources
}

/// Basic Element - a primitive that can be placed
pub struct Bel {
    pub bel_type: BelType,
    pub name: String,
    pub pins: Vec<BelPin>,
}

#[derive(Clone, Copy, Debug)]
pub enum BelType {
    Lut4,
    Dff,
    Carry,
    RamSlice,
    IoCell,
    GlobalBuffer,
}

/// Programmable Interconnect Point - a routing switch
pub struct Pip {
    pub src_wire: WireId,
    pub dst_wire: WireId,
    pub delay: f32,  // picoseconds
    pub configurable: bool,
}
```

### Placement

```rust
/// Cell placement information
pub struct CellPlacement {
    /// Map from cell ID to placement location
    pub placements: HashMap<CellId, PlacementLoc>,

    /// Placement metrics
    pub wirelength: u64,
    pub timing_score: f64,
    pub utilization: f64,
}

pub struct PlacementLoc {
    pub tile_x: u32,
    pub tile_y: u32,
    pub bel_index: usize,
    pub bel_type: BelType,
}

/// Placer configuration
pub struct PlacerConfig {
    pub algorithm: PlacementAlgorithm,
    pub timing_driven: bool,
    pub seed: u64,
    pub effort: PlacementEffort,
}

pub enum PlacementAlgorithm {
    /// Fast analytical placement using quadratic wirelength
    Analytical,
    /// Simulated annealing for quality
    SimulatedAnnealing {
        initial_temp: f64,
        cooling_rate: f64
    },
    /// Analytical + SA refinement (default)
    AnalyticalWithRefinement,
}
```

### Routing

```rust
/// Routing result
pub struct RoutingResult {
    /// Routes for each net
    pub routes: HashMap<NetId, Route>,

    /// Routing metrics
    pub total_wirelength: u64,
    pub max_congestion: u32,
    pub timing_met: bool,
}

/// A routed net
pub struct Route {
    pub net_id: NetId,
    /// Sequence of wires used
    pub wires: Vec<WireId>,
    /// PIPs (switches) enabled
    pub pips: Vec<PipId>,
}

/// Router configuration
pub struct RouterConfig {
    pub algorithm: RoutingAlgorithm,
    pub timing_driven: bool,
    pub max_iterations: u32,
    pub congestion_factor: f64,
}

pub enum RoutingAlgorithm {
    /// PathFinder with A* (default, good quality)
    PathFinderAStar,
    /// Simple maze routing (fast but lower quality)
    MazeRouting,
    /// Timing-driven with slack allocation
    TimingDriven,
}
```

### Bitstream

```rust
/// Bitstream data ready for device programming
pub struct Bitstream {
    pub device: Ice40Device,
    pub format: BitstreamFormat,
    pub data: Vec<u8>,
    pub cram: ConfigRam,
}

pub enum BitstreamFormat {
    /// IceStorm ASCII format (.asc) - human readable
    IceStormAscii,
    /// IceStorm binary format (.bin) - for programming
    IceStormBinary,
}

/// Configuration RAM - the actual bits programmed to the device
pub struct ConfigRam {
    /// CRAM organized by bank
    pub banks: Vec<CramBank>,
}

pub struct CramBank {
    pub bank_num: u8,
    pub rows: Vec<CramRow>,
}
```

## Algorithm Details

### Placement Algorithm

The default placement uses a two-phase approach:

**Phase 1: Analytical Placement**
1. Build connectivity hypergraph from netlist
2. Formulate quadratic wirelength minimization problem
3. Solve using conjugate gradient method
4. Spread cells to reduce overlap

**Phase 2: Simulated Annealing Refinement**
1. Start from analytical solution
2. Define moves: swap cells, move to empty BEL
3. Cost function: wirelength + timing + congestion
4. Adaptive temperature schedule
5. Parallel evaluation using rayon

```rust
impl Placer {
    pub fn place(&self, netlist: &GateNetlist, device: &Ice40ChipDb) -> PlacementResult {
        // Phase 1: Analytical placement
        let initial = self.analytical_place(netlist, device);

        // Phase 2: Legalize to valid BEL sites
        let legalized = self.legalize(initial, device);

        // Phase 3: Simulated annealing refinement
        if self.config.effort >= PlacementEffort::Medium {
            self.simulated_annealing(legalized, netlist, device)
        } else {
            legalized
        }
    }
}
```

### Routing Algorithm

PathFinder with A* search:

1. **Global Routing**: Coarse route through routing regions
2. **Detailed Routing**: Find exact wire paths using A*
3. **Negotiated Congestion**:
   - Route all nets
   - Identify congested resources
   - Increase cost of congested resources
   - Rip-up and reroute
   - Iterate until no congestion

```rust
impl Router {
    pub fn route(
        &self,
        netlist: &GateNetlist,
        placement: &CellPlacement,
        device: &Ice40ChipDb
    ) -> RoutingResult {
        let mut result = RoutingResult::new();
        let mut congestion = CongestionMap::new(device);

        for iteration in 0..self.config.max_iterations {
            // Route all nets
            for net in netlist.nets() {
                let route = self.route_net(net, placement, device, &congestion);
                result.routes.insert(net.id, route);
            }

            // Check congestion
            congestion.update(&result);
            if congestion.max() <= 1 {
                break; // No congestion, done
            }

            // Rip up congested nets and increase costs
            self.rip_up_congested(&mut result, &congestion);
            congestion.increase_history_costs();
        }

        result
    }
}
```

### Bitstream Generation

Generate IceStorm-compatible bitstream:

```rust
impl BitstreamGenerator {
    pub fn generate(
        &self,
        device: &Ice40ChipDb,
        placement: &CellPlacement,
        routing: &RoutingResult,
    ) -> Bitstream {
        let mut cram = ConfigRam::new(device);

        // Configure logic tiles (LUTs, FFs)
        for (cell_id, loc) in &placement.placements {
            self.configure_cell(&mut cram, cell_id, loc, device);
        }

        // Configure routing (PIPs)
        for route in routing.routes.values() {
            for pip in &route.pips {
                self.configure_pip(&mut cram, pip, device);
            }
        }

        // Configure I/O cells
        self.configure_io(&mut cram, device);

        // Generate output format
        match self.config.format {
            BitstreamFormat::IceStormAscii => self.to_ascii(&cram, device),
            BitstreamFormat::IceStormBinary => self.to_binary(&cram, device),
        }
    }
}
```

## ChipDB Loading

The chip database can be loaded from:
1. **Embedded binary** - Compiled into the binary for common devices
2. **IceStorm chipdb** - Parse from IceStorm's `.txt` files
3. **Custom binary format** - Fast loading with serde

```rust
impl Ice40ChipDb {
    /// Load from embedded database
    pub fn load(device: Ice40Device) -> Result<Self> {
        match device {
            Ice40Device::Hx1k => Self::from_bytes(include_bytes!("../chipdb/ice40-hx1k.bin")),
            Ice40Device::Hx8k => Self::from_bytes(include_bytes!("../chipdb/ice40-hx8k.bin")),
            // ...
        }
    }

    /// Parse from IceStorm chipdb text file
    pub fn from_icestorm(path: &Path) -> Result<Self> {
        let content = std::fs::read_to_string(path)?;
        Self::parse_icestorm_chipdb(&content)
    }
}
```

## Public API

```rust
// High-level API for the common case
pub fn place_and_route(
    netlist: &GateNetlist,
    device: Ice40Device,
    config: PnrConfig,
) -> Result<PnrResult> {
    let chipdb = Ice40ChipDb::load(device)?;

    let placer = Placer::new(config.placer);
    let placement = placer.place(netlist, &chipdb)?;

    let router = Router::new(config.router);
    let routing = router.route(netlist, &placement, &chipdb)?;

    let generator = BitstreamGenerator::new(config.bitstream);
    let bitstream = generator.generate(&chipdb, &placement, &routing)?;

    Ok(PnrResult {
        placement,
        routing,
        bitstream,
        timing: TimingAnalyzer::analyze(&placement, &routing, &chipdb),
    })
}

// Example usage
let netlist = map_lir_to_gates(&lir, &ice40_lib).netlist;
let result = place_and_route(&netlist, Ice40Device::Hx8k, PnrConfig::default())?;
std::fs::write("output.bin", &result.bitstream.data)?;
// Then: iceprog output.bin
```

## Integration with Existing Code

### From test_native_place_route.rs

The existing test file already defines much of this API. Key integration points:

1. **Device**: `Device::ice40_hx8k()` already exists
2. **Placer**: `Placer::new(config).place(netlist, device)` pattern
3. **Router**: `Router::new(config).route(netlist, placement, device)` pattern
4. **Bitstream**: `BitstreamGenerator::new(config).generate(placement, routing)` pattern

### From skalp-asic

The ASIC P&R code in `skalp-asic` can be adapted:
- `placement.rs` - Analytical and SA algorithms
- `routing.rs` - PathFinder routing
- `timing.rs` - STA framework

## Implementation Phases

### Phase 1: Device Database (1-2 weeks)
- [ ] Parse IceStorm chipdb format
- [ ] Define Rust data structures
- [ ] Create binary serialization format
- [ ] Embed common devices (HX1K, HX8K, UP5K)

### Phase 2: Basic Placement (2-3 weeks)
- [ ] Random placement (baseline)
- [ ] Analytical placement
- [ ] Legalization
- [ ] Basic simulated annealing

### Phase 3: Basic Routing (2-3 weeks)
- [ ] Build routing graph from chipdb
- [ ] A* shortest path routing
- [ ] PathFinder congestion negotiation
- [ ] Handle global nets (clocks, resets)

### Phase 4: Bitstream Generation (1-2 weeks)
- [ ] IceStorm ASCII format
- [ ] IceStorm binary format
- [ ] Verify against known-good bitstreams

### Phase 5: Timing & Optimization (2-3 weeks)
- [ ] Static timing analysis
- [ ] Timing-driven placement
- [ ] Timing-driven routing
- [ ] Iterative timing closure

### Phase 6: Polish & Testing (1-2 weeks)
- [ ] Comprehensive test suite
- [ ] Performance optimization (rayon parallelism)
- [ ] Documentation
- [ ] Integration with SKALP CLI

## Testing Strategy

1. **Unit Tests**: Each module (chipdb parsing, placement moves, routing paths)
2. **Integration Tests**: Full P&R flow on small designs
3. **Golden Tests**: Compare bitstreams against nextpnr output
4. **Regression Tests**: Ensure designs that work keep working
5. **Performance Tests**: Track runtime and quality metrics

## Dependencies

```toml
[dependencies]
skalp-lir = { path = "../skalp-lir" }
petgraph = "0.6"           # Graph algorithms
indexmap = "2.0"           # Ordered hash maps
rayon = "1.7"              # Parallelism
thiserror = "1.0"          # Error handling
serde = { version = "1.0", features = ["derive"] }  # Serialization
bincode = "1.3"            # Binary serialization for chipdb
```

## References

- [Project IceStorm](http://www.clifford.at/icestorm/) - Reverse-engineered iCE40 docs
- [nextpnr](https://github.com/YosysHQ/nextpnr) - Reference implementation
- [VPR](https://verilogtorouting.org/) - Academic FPGA P&R
- [PathFinder Algorithm](https://dl.acm.org/doi/10.1145/217474.217550) - Original paper

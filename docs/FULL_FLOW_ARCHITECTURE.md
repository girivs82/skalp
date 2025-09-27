# SKALP Full Flow Architecture for Open Targets

## Overview

For open FPGA architectures with documented bitstream formats, SKALP can provide a complete compilation flow from source code to device programming. This eliminates the need for vendor tools and provides a fully integrated, intent-preserving flow from conception to silicon.

## Supported Open Targets

### Currently Planned
- **Lattice iCE40**: Fully documented, simple architecture
- **Lattice ECP5**: More complex but documented
- **Gowin LittleBee**: Growing documentation
- **Future**: Any FPGA with open bitstream documentation

## Extended Pipeline

```
SKALP Source (.sk)
       ↓
    [Frontend]
       ↓
    HIR (High-level IR)
       ↓
    [Architecture Selection]
       ↓
    MIR (Mid-level IR)
       ↓
    [Target Lowering]
       ↓
    LIR (Low-level IR)
       ↓
═══════════════════════════
    [Full Flow Targets]
═══════════════════════════
       ↓
    Physical Netlist
       ↓
    [Placement]
       ↓
    Placed Design
       ↓
    [Routing]
       ↓
    Routed Design
       ↓
    [Bitstream Generation]
       ↓
    Bitstream (.bin)
       ↓
    [Device Programming]
       ↓
    Running Hardware!
```

## Clean Room Implementation

All implementations are based solely on:
- Published datasheets and documentation
- Publicly available technical reference manuals
- Open bitstream format specifications
- No proprietary tool code or reverse engineering

## Physical Design Stages

### Technology Mapping

Converts logical netlist to target-specific primitives:

```rust
struct TechnologyMapper {
    target: TargetDevice,

    fn map_logic(&self, logic: &LogicFunction) -> Primitive {
        match self.target {
            TargetDevice::ICE40 => {
                // Map to 4-LUTs
                self.map_to_lut4(logic)
            },
            TargetDevice::ECP5 => {
                // Map to 4-LUTs with MUX2
                self.map_to_lut4_mux2(logic)
            },
        }
    }

    fn map_memory(&self, mem: &Memory) -> Primitive {
        // Map to block RAM or distributed RAM
        if mem.size() > self.target.lutram_threshold() {
            self.map_to_bram(mem)
        } else {
            self.map_to_lutram(mem)
        }
    }

    fn map_arithmetic(&self, arith: &Arithmetic) -> Primitive {
        // Map to DSP blocks or LUTs+carry
        if self.has_dsp() && arith.width() >= 8 {
            self.map_to_dsp(arith)
        } else {
            self.map_to_carry_chain(arith)
        }
    }
}
```

### Placement

Assigns physical locations to primitives:

```rust
struct Placer {
    algorithm: PlacementAlgorithm,

    fn place(&self, netlist: &PhysicalNetlist) -> Placement {
        // Global placement
        let initial = match self.algorithm {
            PlacementAlgorithm::Analytical => {
                self.analytical_placement(netlist)
            },
            PlacementAlgorithm::SimulatedAnnealing => {
                self.simulated_annealing(netlist)
            },
            PlacementAlgorithm::ForceDirected => {
                self.force_directed(netlist)
            },
        };

        // Legalization
        let legal = self.legalize(initial);

        // Detailed placement
        let detailed = self.detail_placement(legal);

        // Timing-driven optimization
        self.optimize_timing(detailed)
    }

    fn analytical_placement(&self, netlist: &PhysicalNetlist) -> RawPlacement {
        // Minimize quadratic wirelength
        let matrix = self.build_connectivity_matrix(netlist);
        let solution = self.solve_quadratic(matrix);
        self.map_to_positions(solution)
    }

    fn legalize(&self, placement: RawPlacement) -> LegalPlacement {
        // Assign to valid sites
        for cell in placement.cells() {
            let nearest_site = self.find_nearest_legal_site(cell);
            cell.assign_to(nearest_site);
        }

        // Resolve overlaps
        self.remove_overlaps(placement)
    }
}
```

### Routing

Creates physical connections between placed cells:

```rust
struct Router {
    algorithm: RoutingAlgorithm,
    graph: RoutingGraph,

    fn route(&self, placement: &Placement) -> Routing {
        let mut routing = Routing::new();

        // Route in order of criticality
        let nets = self.order_by_criticality(placement.nets());

        for net in nets {
            let route = match self.algorithm {
                RoutingAlgorithm::PathFinder => {
                    self.pathfinder_route(net)
                },
                RoutingAlgorithm::AStar => {
                    self.astar_route(net)
                },
                RoutingAlgorithm::Maze => {
                    self.maze_route(net)
                },
            };

            routing.add_route(net, route);
        }

        // Verify and optimize
        self.verify_drc(routing);
        self.optimize_timing(routing);

        routing
    }

    fn pathfinder_route(&self, net: &Net) -> Route {
        // Negotiated congestion routing
        let mut costs = self.initial_costs();

        loop {
            let path = self.find_path(net, costs);

            if !self.has_conflicts(path) {
                return path;
            }

            // Update costs based on congestion
            self.update_costs(&mut costs, path);
        }
    }
}
```

### Bitstream Generation

Converts routed design to configuration bits:

```rust
struct BitstreamGenerator {
    device: DeviceInfo,

    fn generate(&self, routing: &Routing) -> Bitstream {
        let mut bitstream = Bitstream::new(self.device.size());

        // Configure each tile
        for tile in self.device.tiles() {
            let config = self.configure_tile(tile, routing);
            bitstream.set_tile_bits(tile.address(), config);
        }

        // Add CRC and headers
        bitstream.add_crc();
        bitstream.add_header(self.device.id());

        bitstream
    }

    fn configure_tile(&self, tile: &Tile, routing: &Routing) -> TileConfig {
        let mut config = TileConfig::default();

        // Configure LUTs
        for lut in tile.luts() {
            if let Some(function) = routing.lut_function(lut) {
                config.set_lut_init(lut.index, function.truth_table());
            }
        }

        // Configure routing muxes
        for mux in tile.muxes() {
            if let Some(source) = routing.mux_source(mux) {
                config.set_mux_select(mux.id, source);
            }
        }

        // Configure I/O standards
        if let Some(io) = tile.io_block() {
            config.set_io_standard(routing.io_config(io));
        }

        config
    }
}
```

## Timing Analysis with Real Data

```rust
struct PostRouteTimingAnalysis {
    fn analyze(&self, routing: &Routing) -> DetailedTimingReport {
        let mut report = DetailedTimingReport::new();

        for path in routing.timing_paths() {
            let delay = self.compute_path_delay(path, routing);

            // Now we have REAL delays
            let logic_delay = path.cells()
                .map(|c| self.cell_delay(c))
                .sum();

            let routing_delay = path.nets()
                .map(|n| self.wire_delay(n, routing))
                .sum();

            report.add_path(PathReport {
                start: path.start(),
                end: path.end(),
                logic_delay,      // Actual cell delays
                routing_delay,    // Actual wire delays
                total: logic_delay + routing_delay,
                required: path.constraint(),
                slack: path.constraint() - (logic_delay + routing_delay),
            });
        }

        report
    }

    fn wire_delay(&self, net: &Net, routing: &Routing) -> Time {
        // Real RC delay calculation
        let segments = routing.wire_segments(net);

        let r = segments.iter()
            .map(|s| s.resistance())
            .sum();

        let c = segments.iter()
            .map(|s| s.capacitance())
            .sum();

        // Elmore delay model
        r * c / 2.0
    }
}
```

## Device Programming

```rust
struct DeviceProgrammer {
    interface: ProgrammingInterface,

    fn program(&self, bitstream: &Bitstream) -> Result<()> {
        match self.interface {
            ProgrammingInterface::SPI => {
                self.spi_program(bitstream)
            },
            ProgrammingInterface::JTAG => {
                self.jtag_program(bitstream)
            },
            ProgrammingInterface::USB => {
                self.usb_program(bitstream)
            },
        }
    }

    fn verify(&self, bitstream: &Bitstream) -> Result<()> {
        let readback = self.readback()?;
        if readback == bitstream {
            Ok(())
        } else {
            Err(VerificationError::Mismatch)
        }
    }
}
```

## Integration with SKALP

### Command Line Interface

```bash
# Full flow compilation
skalp compile design.sk --target ice40-hx8k --full-flow

# With specific options
skalp compile design.sk \
    --target ice40-hx8k \
    --placer analytical \
    --router pathfinder \
    --timing-driven \
    --output design.bin

# Program device
skalp program design.bin --interface spi --verify

# Complete flow with programming
skalp run design.sk --target ice40-hx8k --device /dev/ttyUSB0
```

### Configuration

```toml
# skalp.toml
[target.ice40-hx8k]
device = "iCE40HX8K"
package = "CT256"

[placement]
algorithm = "analytical"
effort = "high"
seed = 42

[routing]
algorithm = "pathfinder"
iterations = 50
timing_driven = true

[bitstream]
compress = true
add_crc = true
```

## Benefits of Full Flow

### 1. Complete Control
- Every optimization decision is visible
- Intent guides all stages
- No black box tools

### 2. Rapid Iteration
```bash
# Change source
edit design.sk

# Recompile to hardware in seconds
skalp run design.sk  # Compile, place, route, program

# See results immediately
```

### 3. Advanced Optimizations
- Cross-boundary optimization
- Intent-aware placement
- Source-level timing closure
- Power optimization from source

### 4. Deterministic Results
- Same source → same bitstream
- Reproducible builds
- Version control friendly

### 5. Custom Architectures
- Support experimental FPGAs
- Academic architectures
- Domain-specific devices

## Future Extensions

### Multi-die Support
- Chiplet-based designs
- Die-to-die routing
- Cross-die timing

### Advanced Algorithms
- Machine learning placement
- Parallel routing
- Incremental compilation

### Cloud Compilation
- Distributed place & route
- Shared optimization results
- Hardware acceleration

---

*With full flow support, SKALP becomes a true silicon compiler - from intent to implementation in one seamless flow.*
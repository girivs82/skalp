# Pin Mapping Integration Plan

## Current Status

### What Exists ✅
- **Constraint Infrastructure** (`skalp-backends/src/constraints.rs`)
  - `ConstraintManager` with `PinConstraint` structures
  - Parsing support for PCF, XDC, SDC formats
  - Generation methods: `generate_pcf()`, `generate_xdc()`, `generate_sdc()`

- **Backend Integration** (`skalp-backends/src/fpga/ice40.rs`)
  - `FpgaConfig.pin_file` field exists
  - `nextpnr-ice40` called with `--pcf` flag when pin_file is provided

- **Native Place & Route** (`skalp-place-route/src/`)
  - Complete native placement for iCE40, ECP5, VTR, OpenFPGA
  - Complete native routing
  - Bitstream generation for multiple formats

### What's Missing ❌
1. No connection between `ConstraintManager` → constraint file → synthesis tools
2. Pin constraints not extracted from design source
3. Bitstream generator uses hardcoded I/O defaults instead of actual pin assignments
4. No language syntax for inline physical constraints

## Implementation Plan

### Phase 0: Language Specification Update ✅ COMPLETE

**Update `docs/LANGUAGE_SPECIFICATION.md` to define physical constraint syntax.**

**Status:** ✅ Completed Jan 9, 2025

This must be done FIRST before any implementation to ensure:
- Clear specification of the feature
- Agreement on syntax and semantics
- Reference for implementation and testing
- Documentation for users

**Add new section: "5.X Physical Constraints"**

**Files to modify:**
- `docs/LANGUAGE_SPECIFICATION.md` - Add complete section on physical constraints

**Section content should include:**
1. Motivation and rationale
2. Inline constraint syntax for entity ports
3. Global constraint blocks
4. Supported constraint types per FPGA family
5. Constraint precedence rules (inline vs external)
6. Validation rules
7. Complete examples

**Example spec section:**

```markdown
## 5.X Physical Constraints

### 5.X.1 Overview

Physical constraints specify how logical signals map to physical FPGA resources. Unlike traditional HDLs where constraints live in separate vendor-specific files, SKALP allows constraints to be specified inline with port declarations while still supporting external constraint files for compatibility.

### 5.X.2 Inline Port Constraints

Physical constraints can be attached to entity ports using the `@` syntax:

// ... (include syntax examples)
```

### Phase 1: Language Syntax Extension ✅ COMPLETE

**Implement parsing for the physical constraint syntax defined in the spec:**

**Status:** ✅ Completed Jan 9, 2025

```rust
entity Counter {
    // Pin constraints inline with port declarations
    in clk: clock @ {
        pin: "A1",
        io_standard: "LVCMOS33"
    }

    in rst: reset @ {
        pin: "B2",
        io_standard: "LVCMOS33",
        pull: up
    }

    out count: nat[8] @ {
        pins: ["C1", "C2", "C3", "C4", "D1", "D2", "D3", "D4"],
        io_standard: "LVCMOS33",
        drive: 8mA,
        slew: fast
    }
}

// Optional: Global constraint block for device and floorplan
constraint physical {
    device: "iCE40HX8K-CT256"

    floorplan {
        region "fast_logic" {
            area: (10, 10, 30, 30)
            instances: [counter_reg, adder_logic]
        }
    }
}
```

**Grammar additions:**

```
port_declaration ::=
    direction identifier ':' type ['@' clock_domain] [physical_constraint_block]

physical_constraint_block ::=
    '{' constraint_pair { ',' constraint_pair } '}'

constraint_pair ::=
    | 'pin' ':' string_literal
    | 'pins' ':' '[' string_literal { ',' string_literal } ']'
    | 'io_standard' ':' identifier
    | 'drive' ':' drive_strength
    | 'slew' ':' ('fast' | 'slow')
    | 'pull' ':' ('up' | 'down' | 'none')
    | 'diff_pair' ':' bool

drive_strength ::= integer 'mA'

constraint_block ::=
    'constraint' identifier '{' constraint_statement* '}'
```

**Files to modify:**
- `crates/skalp-frontend/src/parse.rs` - Add parsing for physical constraint blocks
- `crates/skalp-frontend/src/hir.rs` - Add `PhysicalConstraints` to `HirPort`
- `docs/LANGUAGE_SPECIFICATION.md` - Document new syntax

### Phase 2: Frontend Integration ✅ COMPLETE

**Status:** ✅ Completed Jan 9, 2025

**Extract constraints during HIR/MIR building:**

```rust
// In crates/skalp-frontend/src/hir.rs
#[derive(Debug, Clone)]
pub struct HirPort {
    pub name: String,
    pub direction: PortDirection,
    pub typ: HirType,
    pub clock_domain: Option<String>,
    pub physical_constraints: Option<PhysicalConstraints>,  // NEW
}

#[derive(Debug, Clone)]
pub struct PhysicalConstraints {
    pub pin_location: Option<PinLocation>,
    pub io_standard: Option<String>,
    pub drive_strength: Option<DriveStrength>,
    pub slew_rate: Option<SlewRate>,
    pub termination: Option<Termination>,
}

#[derive(Debug, Clone)]
pub enum PinLocation {
    Single(String),           // "A1"
    Multiple(Vec<String>),    // ["A1", "A2", "A3"]
}

#[derive(Debug, Clone)]
pub enum DriveStrength {
    Ma4, Ma8, Ma12, Ma16
}

#[derive(Debug, Clone)]
pub enum SlewRate {
    Fast, Slow
}

#[derive(Debug, Clone)]
pub enum Termination {
    None, PullUp, PullDown, Keeper
}
```

**Files to modify:**
- `crates/skalp-frontend/src/hir.rs` - Add constraint types
- `crates/skalp-frontend/src/hir_builder.rs` - Parse and attach constraints
- `crates/skalp-mir/src/mir.rs` - Propagate constraints through MIR
- `crates/skalp-lir/src/lib.rs` - Propagate to LIR ports

### Phase 3: Backend Integration - Native Tools

**Integrate constraints into native place & route:**

```rust
// In crates/skalp-place-route/src/placer.rs
impl Placer {
    /// Apply pin constraints before placement
    pub fn apply_pin_constraints(
        &mut self,
        constraints: &HashMap<String, PinConstraint>
    ) -> Result<(), PlacementError> {
        for (signal_name, constraint) in constraints {
            // Find corresponding I/O tile for the pin location
            if let Some(io_tile) = self.device.find_io_tile(&constraint.location) {
                // Pre-assign this signal to the specific I/O location
                self.fixed_placements.insert(
                    signal_name.clone(),
                    io_tile.position
                );

                // Store I/O configuration for bitstream generation
                self.io_configurations.insert(
                    signal_name.clone(),
                    IoConfig {
                        location: constraint.location.clone(),
                        io_standard: constraint.io_standard.clone(),
                        drive_strength: constraint.drive_strength.clone(),
                        slew_rate: constraint.slew_rate.clone(),
                        termination: constraint.termination.clone(),
                    }
                );
            } else {
                return Err(PlacementError::InvalidPinLocation(
                    constraint.location.clone()
                ));
            }
        }
        Ok(())
    }
}

// In crates/skalp-place-route/src/device.rs
impl Device {
    /// Find I/O tile by pin name (e.g., "A1", "B2")
    pub fn find_io_tile(&self, pin_name: &str) -> Option<&IoTile> {
        self.io_tiles.iter().find(|tile| {
            tile.pins.iter().any(|pin| pin.name == pin_name)
        })
    }
}
```

**Files to modify:**
- `crates/skalp-place-route/src/placer.rs` - Add constraint application
- `crates/skalp-place-route/src/device.rs` - Add pin lookup methods
- `crates/skalp-place-route/src/bitstream.rs` - Use actual I/O configs

### Phase 4: Backend Integration - External Tools

**Generate constraint files and pass to tools:**

```rust
// In crates/skalp-backends/src/fpga.rs
impl FpgaBackend {
    async fn synthesize(
        &self,
        lir: &LirDesign,
        config: &SynthesisConfig
    ) -> BackendResult<SynthesisResults> {
        // Extract pin constraints from LIR
        let pin_constraints = self.extract_pin_constraints_from_lir(lir);

        // Create ConstraintManager and populate
        let mut constraint_manager = ConstraintManager::new();
        for (signal_name, phys_constraint) in pin_constraints {
            constraint_manager.add_pin_constraint(PinConstraint {
                signal_name: signal_name.clone(),
                location: phys_constraint.pin_location.to_string(),
                io_standard: phys_constraint.io_standard,
                drive_strength: phys_constraint.drive_strength.map(|d| d.to_string()),
                slew_rate: phys_constraint.slew_rate,
                termination: phys_constraint.termination,
            });
        }

        // Add timing constraints
        for timing_constraint in &config.timing_constraints {
            constraint_manager.add_timing_constraint(timing_constraint.clone());
        }

        // Generate appropriate constraint file format
        let constraint_format = match self.target {
            FpgaTarget::Ice40 { .. } => ConstraintFormat::Pcf,
            FpgaTarget::Xilinx7Series { .. } => ConstraintFormat::Xdc,
            FpgaTarget::IntelStratix { .. } => ConstraintFormat::Sdc,
            _ => ConstraintFormat::Sdc, // Default
        };

        // Generate and write constraint file
        let constraint_content = constraint_manager.generate_constraints(constraint_format);
        let constraint_path = Path::new(&config.output_dir)
            .join(format!("design.{}", constraint_format.extension()));
        tokio::fs::write(&constraint_path, constraint_content).await?;

        // Update fpga_config with constraint file path
        let mut fpga_config = self.config.clone();
        fpga_config.pin_file = Some(constraint_path.to_string_lossy().to_string());

        // Continue with synthesis...
        let verilog = self.lir_to_verilog(lir).await?;

        // Call tool-specific synthesis with constraints
        match self.target {
            FpgaTarget::Ice40 { ref part, ref package } => {
                ice40::synthesize_ice40(
                    &verilog,
                    part,
                    package,
                    Path::new(&config.output_dir),
                    &fpga_config
                ).await
            }
            // ... other targets
        }
    }

    fn extract_pin_constraints_from_lir(
        &self,
        lir: &LirDesign
    ) -> HashMap<String, PhysicalConstraints> {
        let mut constraints = HashMap::new();

        // Extract from top-level module ports
        if let Some(top_module) = lir.modules.first() {
            for signal in &top_module.signals {
                if signal.is_port() {
                    if let Some(ref phys_constraint) = signal.physical_constraints {
                        constraints.insert(signal.name.clone(), phys_constraint.clone());
                    }
                }
            }
        }

        constraints
    }
}

// Helper for constraint format extensions
impl ConstraintFormat {
    pub fn extension(&self) -> &str {
        match self {
            ConstraintFormat::Pcf => "pcf",
            ConstraintFormat::Xdc => "xdc",
            ConstraintFormat::Sdc => "sdc",
            ConstraintFormat::Skalp => "skalp.json",
        }
    }
}
```

**Files to modify:**
- `crates/skalp-backends/src/fpga.rs` - Add constraint extraction and file generation
- `crates/skalp-backends/src/fpga/ice40.rs` - Use constraint file
- `crates/skalp-backends/src/fpga/xilinx.rs` - Use constraint file
- `crates/skalp-backends/src/constraints.rs` - Add helper methods

### Phase 5: Bitstream Generation Enhancement

**Use actual pin assignments instead of hardcoded defaults:**

```rust
// In crates/skalp-place-route/src/bitstream.rs
impl BitstreamGenerator {
    fn generate_ice40_ascii(
        &self,
        placement: &PlacementResult,
        routing: &RoutingResult,
        io_configs: &HashMap<String, IoConfig>,  // NEW parameter
    ) -> Result<Bitstream, BitstreamError> {
        // ... existing logic ...

        // I/O configuration with actual constraints
        bitstream_text.push_str("\n.io_tiles\n");

        for (signal_name, io_config) in io_configs {
            if let Some(io_tile) = self.device.find_io_tile(&io_config.location) {
                let (x, y) = io_tile.position;

                // Direction based on signal type
                let direction = if signal_name.starts_with("in_") {
                    "input"
                } else {
                    "output"
                };

                bitstream_text.push_str(&format!(
                    ".io {} {} {}\n",
                    x, y, direction
                ));

                // I/O standard from constraints, not hardcoded
                if let Some(ref io_standard) = io_config.io_standard {
                    bitstream_text.push_str(&format!(
                        ".io_standard {}\n",
                        io_standard
                    ));
                } else {
                    bitstream_text.push_str(".io_standard LVCMOS33\n"); // fallback
                }

                // Drive strength
                if let Some(ref drive) = io_config.drive_strength {
                    bitstream_text.push_str(&format!(
                        ".drive_strength {}\n",
                        drive
                    ));
                }

                // Slew rate
                if let Some(ref slew) = io_config.slew_rate {
                    bitstream_text.push_str(&format!(
                        ".slew_rate {:?}\n",
                        slew
                    ));
                }

                // Termination
                if let Some(ref term) = io_config.termination {
                    bitstream_text.push_str(&format!(
                        ".termination {:?}\n",
                        term
                    ));
                }
            }
        }

        // ... rest of implementation ...
    }
}
```

**Files to modify:**
- `crates/skalp-place-route/src/bitstream.rs` - Use actual I/O configurations
- `crates/skalp-place-route/src/placer.rs` - Return I/O configs with placement

### Phase 6: CLI Integration

**Add command-line options for constraints:**

```rust
// In src/main.rs
#[derive(Parser)]
pub struct BuildArgs {
    /// Input SKALP source file
    pub input: PathBuf,

    /// Output directory
    #[arg(short, long, default_value = "./build")]
    pub output: PathBuf,

    /// Target device
    #[arg(short, long)]
    pub device: Option<String>,

    /// External constraint file (PCF/XDC/SDC)
    #[arg(short = 'c', long)]
    pub constraints: Option<PathBuf>,

    /// Use native place & route (don't call external tools)
    #[arg(long)]
    pub native: bool,
}

async fn build_command(args: BuildArgs) -> Result<()> {
    // ... parse source ...

    // Load external constraints if provided
    let mut constraint_manager = ConstraintManager::new();
    if let Some(constraint_file) = args.constraints {
        let format = detect_constraint_format(&constraint_file)?;
        constraint_manager.load_from_file(
            constraint_file.to_str().unwrap(),
            format
        ).await?;
    }

    // Merge with inline constraints from source
    // (inline takes precedence over external)

    // Continue with synthesis...
}
```

**Files to modify:**
- `src/main.rs` - Add CLI options
- Add constraint format detection helper

### Phase 7: Testing

**Add comprehensive tests:**

```rust
// In tests/test_pin_mapping_integration.rs
#[test]
fn test_inline_pin_constraints_parsing() {
    let source = r#"
        entity Led {
            in clk: clock @ { pin: "A1", io_standard: "LVCMOS33" }
            out led: bit @ { pin: "B2", drive: 8mA, slew: fast }
        }
    "#;

    let hir = parse_and_build_hir(source).unwrap();
    assert!(hir.entities[0].ports[0].physical_constraints.is_some());
}

#[test]
fn test_native_place_with_pin_constraints() {
    let device = Device::ice40_hx1k();
    let design = create_test_design_with_pins();

    let mut placer = Placer::new(PlacerConfig::default(), device);

    let pin_constraints = extract_pin_constraints(&design);
    placer.apply_pin_constraints(&pin_constraints).unwrap();

    let result = placer.place(&design).unwrap();

    // Verify constrained pins are in fixed locations
    assert_eq!(result.placements.get("clk"), Some(&(0, 0))); // A1 location
}

#[tokio::test]
async fn test_constraint_file_generation() {
    let lir = create_test_lir_with_constraints();
    let backend = FpgaBackend::new(/* ... */);

    let config = SynthesisConfig {
        output_dir: "/tmp/test_constraints".to_string(),
        // ...
    };

    backend.synthesize(&lir, &config).await.unwrap();

    // Verify PCF file was created
    let pcf_path = Path::new("/tmp/test_constraints/design.pcf");
    assert!(pcf_path.exists());

    // Verify contents
    let content = fs::read_to_string(pcf_path).unwrap();
    assert!(content.contains("set_io clk A1"));
}

#[test]
fn test_bitstream_with_actual_io_config() {
    let device = Device::ice40_hx1k();
    let placement = create_test_placement();
    let routing = create_test_routing();

    let mut io_configs = HashMap::new();
    io_configs.insert("clk".to_string(), IoConfig {
        location: "A1".to_string(),
        io_standard: Some("LVCMOS33".to_string()),
        drive_strength: None,
        slew_rate: Some(SlewRate::Fast),
        termination: None,
    });

    let generator = BitstreamGenerator::new(device);
    let bitstream = generator.generate_with_io_config(
        &placement,
        &routing,
        &io_configs
    ).unwrap();

    let content = String::from_utf8_lossy(&bitstream.data);
    assert!(content.contains(".io_standard LVCMOS33"));
    assert!(!content.contains("HARDCODED")); // Ensure no hardcoded values
}
```

**Files to create:**
- `tests/test_pin_mapping_integration.rs` - Integration tests
- `tests/golden_files/pin_constraints/` - Golden file tests for parsing

## Validation Checklist

- [ ] Can parse inline pin constraint syntax
- [ ] Constraints propagate from HIR → MIR → LIR
- [ ] Native placer respects pin constraints
- [ ] External tools receive generated constraint files
- [ ] Bitstream uses actual I/O configurations
- [ ] CLI can load external constraint files
- [ ] Inline constraints override external constraints
- [ ] Error messages for invalid pin names
- [ ] Error messages for pin conflicts
- [ ] Device-specific validation (e.g., iCE40 vs ECP5 pin names)
- [ ] Documentation updated in language spec
- [ ] Examples added showing pin constraints
- [ ] All tests pass including CI

## Example Usage After Implementation

```bash
# Using inline constraints only
skalp build led_blink.sk --device iCE40HX8K-CT256

# Using external constraint file
skalp build counter.sk --device iCE40HX8K --constraints board.pcf

# Using native place & route with inline constraints
skalp build fifo.sk --device iCE40HX8K --native

# Generate constraint file from design
skalp extract-constraints design.sk --format pcf -o design.pcf
```

## Timeline Estimate

- **Phase 0 (Spec)**: ✅ COMPLETE (1 day)
- **Phase 1 (Parsing)**: ✅ COMPLETE (Jan 9, 2025)
- **Phase 2 (Frontend)**: ⏳ NEXT - 2-3 days
- **Phase 3 (Native)**: 3-4 days
- **Phase 4 (External)**: 2-3 days
- **Phase 5 (Bitstream)**: 2-3 days
- **Phase 6 (CLI)**: 1 day
- **Phase 7 (Testing)**: 2-3 days

**Total: ~16-22 days** for complete implementation

**Note**: Phase 0 is critical and blocks all other phases. The spec defines what we're building.

## Priority

**HIGH** - This is a critical missing piece that prevents the toolchain from being usable for real FPGA designs. Without pin mapping:
- Can't program actual hardware
- Can't integrate with development boards
- Can't meet board-level constraints
- Native toolchain is incomplete

## References

- Existing timing constraint syntax: `docs/LANGUAGE_SPECIFICATION.md` section 4.5
- Constraint manager: `crates/skalp-backends/src/constraints.rs`
- Native place & route: `crates/skalp-place-route/src/`
- Test examples: `tests/test_native_place_route.rs`

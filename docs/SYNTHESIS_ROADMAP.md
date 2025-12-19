# Synthesis Engine Roadmap

**Status:** Core infrastructure complete, rewrite pass needs gate implementations to be functional.

---

## Recent Updates (December 2025)

### NPN Database Complete
- All **222 NPN equivalence classes** for 4-input functions are now enumerated
- Fixed critical mask bug: 4-input functions need 16-bit masks, not 4-bit
- Dynamic enumeration via `NpnDatabase::enumerate_all_classes()`

### ABC-Style Pass Sequences
- Added `SynthPreset::Resyn2` matching ABC's proven sequence:
  ```
  balance; rewrite; refactor; balance; rewrite; rewrite -z; balance; refactor -z; rewrite -z; balance
  ```
- Zero-cost modes: `rewrite_z` and `refactor_z` (accept gain >= 0)

### What's Working
- Infrastructure for cut enumeration and NPN canonicalization
- Pass sequencing with convergence detection
- Structural hashing, constant propagation, DCE, balancing

### What's Not Yet Working
- **Rewrite/Refactor don't reduce gate count** - the NPN database entries don't have gate implementations
- Need to either precompute optimal gate sequences or synthesize from truth tables

---

## Current Capabilities

### Optimization Passes
- **strash**: Structural hashing (merge identical subgraphs) ✅
- **const_prop**: Constant propagation ✅
- **dce**: Dead code elimination ✅
- **balance**: Tree balancing (reduce logic depth) ✅
- **rewrite**: 4-cut AIG rewriting ⚠️ (infrastructure only, needs gate implementations)
- **refactor**: Cone refactoring ⚠️ (infrastructure only)
- **fraig**: SAT sweeping (functional equivalence)
- **retiming**: Register retiming
- **buffer_opt**: Fanout-aware buffer insertion

### Technology Mapping
- Cut-based mapping with priority cuts
- Delay-optimal mapping
- Cell sizing (X1/X2/X4 drive strengths)

### ML Integration
- Pass ordering via learned policy
- GNN-based cut selection (heuristic fallback)
- Architecture advisor for datapath

---

## Remaining Work (High Priority)

### 1. NPN Gate Implementations
**File:** `crates/skalp-lir/src/synth/npn.rs`

The rewrite pass needs actual gate implementations for each NPN class.

**Options:**
1. **Precompute** - Generate optimal gate sequences offline for all 222 classes
2. **Synthesize on-demand** - Use truth table synthesis at runtime

```rust
// Example: Precomputed implementation for XOR (0x6666)
// XOR = (!a & b) | (a & !b) = !(!(!a & b) & !(a & !b))
// In AIG: 3 gates with inverters
self.add(0x6666, 3, 2, vec![(0, 3), (1, 2), (9, 11)]);
```

---

## Remaining Work (Medium Priority)

### 1. Area Recovery Mapper
**File:** `crates/skalp-lir/src/synth/mapping/area_mapper.rs`

Area recovery pass that trades delay slack for area reduction on non-critical paths.

```rust
// Pseudocode
pub struct AreaMapper {
    slack_threshold: f64,  // Minimum slack to consider for area recovery
}

impl AreaMapper {
    pub fn recover_area(&self, aig: &mut Aig, timing: &StaResult) {
        for node in aig.nodes_with_positive_slack(timing) {
            // Try smaller/slower cells
            // Re-run STA to verify timing still met
        }
    }
}
```

### 2. Multiplier Optimization
**File:** `crates/skalp-lir/src/synth/datapath/mult_opt.rs`

Multiplier architecture selection and generation.

```rust
pub enum MultiplierArchitecture {
    Array,        // Simple, predictable area/delay
    Wallace,      // Fast, uses 3:2 compressors
    Dadda,        // Variant of Wallace, slightly less area
    Booth,        // Radix-4 Booth encoding for signed
}

pub fn generate_multiplier(
    width: usize,
    signed: bool,
    arch: MultiplierArchitecture,
) -> Aig {
    // Generate multiplier AIG based on architecture
}
```

---

## Optional Enhancements (Low Priority)

### 1. Training Data Collection
**Purpose:** Build dataset for ML model training

```rust
// Add to MlSynthEngine
pub struct TrainingDataCollector {
    episodes: Vec<SynthesisEpisode>,
}

pub struct SynthesisEpisode {
    initial_features: AigFeatures,
    actions: Vec<PassAction>,
    rewards: Vec<f64>,
    final_qor: QualityOfResult,
}

impl MlSynthEngine {
    pub fn collect_training_data(&mut self, aig: &mut Aig) -> SynthesisEpisode {
        // Run synthesis while logging state-action-reward tuples
    }

    pub fn export_training_data(&self, path: &str) -> Result<()> {
        // Export to JSON/Parquet for Python training
    }
}
```

### 2. Online Learning
**Purpose:** Improve policy based on synthesis outcomes

```rust
pub struct OnlineLearner {
    replay_buffer: Vec<Experience>,
    learning_rate: f64,
    update_frequency: usize,
}

impl OnlineLearner {
    pub fn update_policy(&mut self, episode: &SynthesisEpisode) {
        // Simple policy gradient update
        // Or: batch updates with PPO
    }
}
```

### 3. PPO Trainer (Rust)
**File:** `crates/skalp-ml/src/pass_rl/trainer.rs`

Full PPO implementation for on-device training.

```rust
pub struct PpoTrainer {
    policy: PolicyNetwork,
    value_fn: ValueNetwork,
    clip_epsilon: f64,
    gamma: f64,
    gae_lambda: f64,
}

impl PpoTrainer {
    pub fn train_step(&mut self, batch: &[Experience]) -> TrainStats {
        // Compute advantages with GAE
        // Update policy with clipped objective
        // Update value function
    }
}
```

### 4. Additional Datapath Architectures

```rust
// Dividers
pub enum DividerArchitecture {
    Restoring,
    NonRestoring,
    SRT,           // Sweeney-Robertson-Tocher
    Newton,        // Newton-Raphson iterative
}

// Shifters (already have barrel/log)
pub enum ShifterArchitecture {
    Barrel,        // O(1) delay, O(n log n) area
    Logarithmic,   // O(log n) delay, O(n log n) area
    Funnel,        // For wide shifts
}

// Comparators
pub enum ComparatorArchitecture {
    Ripple,        // Simple, O(n) delay
    Tree,          // O(log n) delay
    Parallel,      // Parallel prefix
}
```

### 5. Power Optimization Integration

```rust
pub struct PowerOptimizer {
    enable_clock_gating: bool,
    enable_power_gating: bool,
    activity_threshold: f64,
}

impl PowerOptimizer {
    pub fn insert_clock_gating(&self, netlist: &mut GateNetlist) {
        // Identify registers with enable conditions
        // Insert ICG (integrated clock gating) cells
    }

    pub fn analyze_switching_activity(&self, aig: &Aig) -> ActivityMap {
        // Estimate toggle rates from truth tables
    }
}
```

### 6. Formal Equivalence Checking

```rust
pub struct EquivalenceChecker {
    solver: SatSolver,
}

impl EquivalenceChecker {
    pub fn check(&self, aig1: &Aig, aig2: &Aig) -> EquivResult {
        // Miter construction
        // SAT solving
    }
}
```

---

## Training Pipeline (Python)

For production ML models, a Python training pipeline would be needed:

```
training/
├── collect_data.py      # Run skalp on design corpus, collect episodes
├── preprocess.py        # Convert to tensors, normalize features
├── train_policy.py      # PPO training with PyTorch
├── train_gnn.py         # GNN training for cut selection
├── export_onnx.py       # Export to ONNX for Rust inference
└── evaluate.py          # Benchmark against heuristics
```

---

## Priority Matrix

| Enhancement | Impact | Effort | Priority |
|-------------|--------|--------|----------|
| Area mapper | High | Medium | P1 |
| Multiplier opt | High | Medium | P1 |
| Training collection | Medium | Low | P2 |
| Online learning | Medium | Medium | P2 |
| PPO trainer | Low | High | P3 |
| Power optimization | High | High | P2 |
| Formal EC | Medium | High | P3 |

---

## Notes

- The current heuristic-based ML works well for most designs
- Training infrastructure is only needed if pursuing learned optimization
- Power optimization should integrate with existing power intent attributes
- Formal EC is useful for validating optimization correctness

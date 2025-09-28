//! GPU Simulator
//!
//! Main simulation engine that coordinates GPU execution

use crate::sir::*;
use crate::state::SimulationState;
use crate::event::EventQueue;
use std::collections::HashMap;
use bitvec::prelude::*;

/// Simulation result for a single run
#[derive(Debug, Clone)]
pub struct SimulationResult {
    /// Final simulation state
    pub final_state: SimulationState,
    /// Total cycles simulated
    pub cycles_executed: u64,
    /// Simulation statistics
    pub stats: SimulationStats,
}

/// Simulation statistics
#[derive(Debug, Clone, Default)]
pub struct SimulationStats {
    /// Total combinational evaluations
    pub comb_evaluations: u64,
    /// Total sequential evaluations
    pub seq_evaluations: u64,
    /// Time spent in combinational logic (ns)
    pub comb_time_ns: u64,
    /// Time spent in sequential logic (ns)
    pub seq_time_ns: u64,
    /// GPU kernel launches (if using GPU)
    pub gpu_kernel_launches: u32,
}

/// GPU-accelerated simulator
pub struct Simulator {
    /// The SIR being simulated
    sir: Sir,
    /// Current simulation state
    state: SimulationState,
    /// Event queue for delta-cycle simulation
    event_queue: EventQueue,
    /// Combinational evaluation cache
    comb_cache: HashMap<CombBlockId, BitVec>,
    /// Simulation statistics
    stats: SimulationStats,
}

impl Simulator {
    /// Create a new simulator for the given SIR
    pub fn new(sir: Sir) -> Self {
        let mut state = SimulationState::new(sir.top_module.signals.len());

        // Initialize signal values
        for signal in &sir.top_module.signals {
            if let Some(ref initial) = signal.initial_value {
                state.set_signal_value(signal.id, initial.clone());
            } else {
                // Default to zero
                let zero_value = bitvec![0; signal.width];
                state.set_signal_value(signal.id, zero_value);
            }
        }

        Self {
            sir,
            state,
            event_queue: EventQueue::new(),
            comb_cache: HashMap::new(),
            stats: SimulationStats::default(),
        }
    }

    /// Run simulation for a number of cycles
    pub async fn run(&mut self, cycles: u64) -> Result<SimulationResult, Box<dyn std::error::Error>> {
        println!("Starting simulation for {} cycles", cycles);

        let start_time = std::time::Instant::now();

        for cycle in 0..cycles {
            self.simulate_cycle().await?;

            // Print progress every 10000 cycles
            if cycle % 10000 == 0 && cycle > 0 {
                println!("Completed {} cycles", cycle);
            }
        }

        let total_time = start_time.elapsed();
        println!("Simulation completed in {:.2}ms", total_time.as_secs_f64() * 1000.0);

        Ok(SimulationResult {
            final_state: self.state.clone(),
            cycles_executed: cycles,
            stats: self.stats.clone(),
        })
    }

    /// Simulate a single cycle
    async fn simulate_cycle(&mut self) -> Result<(), Box<dyn std::error::Error>> {
        // Phase 1: Evaluate all combinational logic
        self.evaluate_combinational_logic().await?;

        // Phase 2: Process clock edges (sequential logic)
        self.process_clock_edges().await?;

        // Phase 3: Process any delta-cycle events
        self.process_delta_events().await?;

        Ok(())
    }

    /// Evaluate all combinational logic blocks
    async fn evaluate_combinational_logic(&mut self) -> Result<(), Box<dyn std::error::Error>> {
        let start_time = std::time::Instant::now();

        // Get dependency-ordered list of combinational blocks
        let dependency_graph = self.sir.get_comb_dependency_graph();

        // Use topological sort to get evaluation order
        use petgraph::algo::toposort;
        let topo_order = toposort(&dependency_graph, None)
            .map_err(|_| "Combinational loop detected in design")?;

        // Evaluate blocks in dependency order
        for node_index in topo_order {
            if let Some(block_id) = dependency_graph.node_weight(node_index) {
                self.evaluate_comb_block(*block_id).await?;
            }
        }

        self.stats.comb_time_ns += start_time.elapsed().as_nanos() as u64;
        self.stats.comb_evaluations += self.sir.top_module.comb_blocks.len() as u64;

        Ok(())
    }

    /// Evaluate a single combinational block
    async fn evaluate_comb_block(&mut self, block_id: CombBlockId) -> Result<(), Box<dyn std::error::Error>> {
        // Find the block and clone operations to avoid borrow conflicts
        let (operations, outputs, inputs_changed) = {
            let block = self.sir.top_module.comb_blocks.iter()
                .find(|b| b.id == block_id)
                .ok_or("Combinational block not found")?;

            // Check if any inputs have changed (for optimization)
            let mut inputs_changed = false;
            for &input_signal in &block.inputs {
                if self.state.has_signal_changed(input_signal) {
                    inputs_changed = true;
                    break;
                }
            }

            (block.operations.clone(), block.outputs.clone(), inputs_changed)
        };

        // Only evaluate if inputs changed or first evaluation
        if inputs_changed || !self.comb_cache.contains_key(&block_id) {
            // Execute all operations in the block
            for operation in &operations {
                self.execute_operation(operation)?;
            }

            // Cache the evaluation
            if let Some(&output_signal) = outputs.first() {
                if let Some(value) = self.state.get_signal_value(output_signal) {
                    self.comb_cache.insert(block_id, value.clone());
                }
            }
        }

        Ok(())
    }

    /// Process clock edges for sequential logic
    async fn process_clock_edges(&mut self) -> Result<(), Box<dyn std::error::Error>> {
        let start_time = std::time::Instant::now();

        // Find all clock signals and check for edges
        let mut active_clocks = Vec::new();

        for signal in &self.sir.top_module.signals {
            if let SirSignalType::Register { clock, .. } = &signal.signal_type {
                if self.state.has_clock_edge(*clock) {
                    active_clocks.push(*clock);
                }
            }
        }

        // Clone sequential blocks to avoid borrow conflicts
        let seq_blocks = self.sir.top_module.seq_blocks.clone();

        // Process sequential blocks triggered by active clocks
        for seq_block in &seq_blocks {
            if active_clocks.contains(&seq_block.clock) {
                self.evaluate_seq_block(seq_block).await?;
            }
        }

        self.stats.seq_time_ns += start_time.elapsed().as_nanos() as u64;
        self.stats.seq_evaluations += active_clocks.len() as u64;

        Ok(())
    }

    /// Evaluate a sequential block
    async fn evaluate_seq_block(&mut self, block: &SequentialBlock) -> Result<(), Box<dyn std::error::Error>> {
        // Check reset condition
        if let Some(ref reset_spec) = block.reset {
            let reset_active = self.is_reset_active(reset_spec)?;
            if reset_active {
                // Apply reset values to all registers in this block
                for &register_id in &block.registers {
                    if let Some(signal) = self.sir.top_module.signals.iter().find(|s| s.id == register_id) {
                        let reset_value = signal.initial_value.clone()
                            .unwrap_or_else(|| bitvec![0; signal.width]);
                        self.state.set_signal_value(register_id, reset_value);
                    }
                }
                return Ok(());
            }
        }

        // Execute all operations in the sequential block
        for operation in &block.operations {
            self.execute_operation(operation)?;
        }

        Ok(())
    }

    /// Execute a single SIR operation
    fn execute_operation(&mut self, operation: &SirOperation) -> Result<(), Box<dyn std::error::Error>> {
        match operation {
            SirOperation::Assign { target, source } => {
                let value = self.evaluate_expression(source)?;
                self.state.set_signal_value(*target, value);
            }
            SirOperation::ConditionalAssign { condition, target, source } => {
                let cond_value = self.evaluate_expression(condition)?;
                if self.is_true(&cond_value) {
                    let value = self.evaluate_expression(source)?;
                    self.state.set_signal_value(*target, value);
                }
            }
            SirOperation::Case { selector, cases, default } => {
                let selector_value = self.evaluate_expression(selector)?;

                let mut matched = false;
                for case_item in cases {
                    for case_value_expr in &case_item.values {
                        let case_value = self.evaluate_expression(case_value_expr)?;
                        if selector_value == case_value {
                            // Clone operations to avoid recursive async
                            let case_ops = case_item.operations.clone();
                            for case_op in &case_ops {
                                self.execute_operation(case_op)?;
                            }
                            matched = true;
                            break;
                        }
                    }
                    if matched { break; }
                }

                // Execute default case if no match
                if !matched {
                    if let Some(ref default_ops) = default {
                        let default_ops_clone = default_ops.clone();
                        for default_op in &default_ops_clone {
                            self.execute_operation(default_op)?;
                        }
                    }
                }
            }
        }

        Ok(())
    }

    /// Evaluate a SIR expression
    fn evaluate_expression(&self, expr: &SirExpression) -> Result<BitVec, Box<dyn std::error::Error>> {
        match expr {
            SirExpression::Signal(signal_id) => {
                self.state.get_signal_value(*signal_id)
                    .ok_or_else(|| format!("Signal {:?} not found", signal_id).into())
            }
            SirExpression::Constant(value) => Ok(value.clone()),
            SirExpression::Binary { op, left, right } => {
                let left_val = self.evaluate_expression(left)?;
                let right_val = self.evaluate_expression(right)?;
                self.evaluate_binary_op(op, &left_val, &right_val)
            }
            SirExpression::Unary { op, operand } => {
                let operand_val = self.evaluate_expression(operand)?;
                self.evaluate_unary_op(op, &operand_val)
            }
            SirExpression::BitSelect { signal, index } => {
                let signal_val = self.state.get_signal_value(*signal)
                    .ok_or_else(|| format!("Signal {:?} not found", signal))?;
                let index_val = self.evaluate_expression(index)?;
                let index_int = self.bitvec_to_usize(&index_val)?;

                if index_int < signal_val.len() {
                    let mut result = BitVec::new();
                    result.push(signal_val[index_int]);
                    Ok(result)
                } else {
                    Ok(bitvec![0; 1])
                }
            }
            SirExpression::RangeSelect { signal, high, low } => {
                let signal_val = self.state.get_signal_value(*signal)
                    .ok_or_else(|| format!("Signal {:?} not found", signal))?;
                let high_val = self.evaluate_expression(high)?;
                let low_val = self.evaluate_expression(low)?;
                let high_int = self.bitvec_to_usize(&high_val)?;
                let low_int = self.bitvec_to_usize(&low_val)?;

                if high_int < signal_val.len() && low_int <= high_int {
                    let range = low_int..=high_int;
                    let mut result = BitVec::new();
                    for i in range {
                        result.push(signal_val[i]);
                    }
                    Ok(result)
                } else {
                    Ok(BitVec::new())
                }
            }
            SirExpression::Concat(exprs) => {
                let mut result = BitVec::new();
                for expr in exprs {
                    let val = self.evaluate_expression(expr)?;
                    result.extend(val);
                }
                Ok(result)
            }
            SirExpression::Replicate { count, value } => {
                let count_val = self.evaluate_expression(count)?;
                let value_val = self.evaluate_expression(value)?;
                let count_int = self.bitvec_to_usize(&count_val)?;

                let mut result = BitVec::new();
                for _ in 0..count_int {
                    result.extend(value_val.clone());
                }
                Ok(result)
            }
        }
    }

    /// Process delta-cycle events
    async fn process_delta_events(&mut self) -> Result<(), Box<dyn std::error::Error>> {
        while let Some(event) = self.event_queue.pop_next_event() {
            // Process the event
            self.state.apply_event(&event)?;
        }
        Ok(())
    }

    /// Check if reset is active
    fn is_reset_active(&self, reset_spec: &ResetSpec) -> Result<bool, Box<dyn std::error::Error>> {
        let reset_value = self.state.get_signal_value(reset_spec.signal)
            .ok_or("Reset signal not found")?;

        let reset_bit = reset_value.first().map(|b| *b).unwrap_or(false);
        Ok(if reset_spec.active_high { reset_bit } else { !reset_bit })
    }

    /// Check if a bit vector represents true
    fn is_true(&self, value: &BitVec) -> bool {
        value.any()
    }

    /// Convert bitvec to usize for indexing
    fn bitvec_to_usize(&self, value: &BitVec) -> Result<usize, Box<dyn std::error::Error>> {
        let mut result = 0usize;
        for (i, bit) in value.iter().enumerate() {
            if *bit {
                result |= 1 << i;
            }
        }
        Ok(result)
    }

    /// Evaluate binary operation
    fn evaluate_binary_op(&self, op: &BinaryOp, left: &BitVec, right: &BitVec) -> Result<BitVec, Box<dyn std::error::Error>> {
        // Convert to integers for arithmetic operations
        let left_int = self.bitvec_to_u64(left)?;
        let right_int = self.bitvec_to_u64(right)?;

        let result_int = match op {
            BinaryOp::Add => left_int.wrapping_add(right_int),
            BinaryOp::Sub => left_int.wrapping_sub(right_int),
            BinaryOp::Mul => left_int.wrapping_mul(right_int),
            BinaryOp::Div => if right_int != 0 { left_int / right_int } else { 0 },
            BinaryOp::Mod => if right_int != 0 { left_int % right_int } else { 0 },
            BinaryOp::Equal => if left_int == right_int { 1 } else { 0 },
            BinaryOp::NotEqual => if left_int != right_int { 1 } else { 0 },
            BinaryOp::Less => if left_int < right_int { 1 } else { 0 },
            BinaryOp::LessEqual => if left_int <= right_int { 1 } else { 0 },
            BinaryOp::Greater => if left_int > right_int { 1 } else { 0 },
            BinaryOp::GreaterEqual => if left_int >= right_int { 1 } else { 0 },
            BinaryOp::LeftShift => left_int << (right_int & 63), // Limit shift amount
            BinaryOp::RightShift => left_int >> (right_int & 63),
            BinaryOp::BitwiseAnd => left_int & right_int,
            BinaryOp::BitwiseOr => left_int | right_int,
            BinaryOp::BitwiseXor => left_int ^ right_int,
            BinaryOp::And => if left_int != 0 && right_int != 0 { 1 } else { 0 },
            BinaryOp::Or => if left_int != 0 || right_int != 0 { 1 } else { 0 },
            BinaryOp::Xor => if (left_int != 0) ^ (right_int != 0) { 1 } else { 0 },
        };

        Ok(self.u64_to_bitvec(result_int, std::cmp::max(left.len(), right.len())))
    }

    /// Evaluate unary operation
    fn evaluate_unary_op(&self, op: &UnaryOp, operand: &BitVec) -> Result<BitVec, Box<dyn std::error::Error>> {
        match op {
            UnaryOp::Not => {
                let operand_int = self.bitvec_to_u64(operand)?;
                let result = if operand_int == 0 { 1 } else { 0 };
                Ok(self.u64_to_bitvec(result, 1))
            }
            UnaryOp::BitwiseNot => {
                let mut result = operand.clone();
                result.iter_mut().for_each(|mut b| b.set(!*b));
                Ok(result)
            }
            UnaryOp::Negate => {
                let operand_int = self.bitvec_to_u64(operand)?;
                let result = (!operand_int).wrapping_add(1);
                Ok(self.u64_to_bitvec(result, operand.len()))
            }
            UnaryOp::Reduce(reduce_op) => {
                let result = match reduce_op {
                    ReduceOp::And => operand.all(),
                    ReduceOp::Or => operand.any(),
                    ReduceOp::Xor => operand.iter().fold(false, |acc, b| acc ^ *b),
                    ReduceOp::Nand => !operand.all(),
                    ReduceOp::Nor => !operand.any(),
                    ReduceOp::Xnor => !operand.iter().fold(false, |acc, b| acc ^ *b),
                };
                Ok(if result { bitvec![1] } else { bitvec![0] })
            }
        }
    }

    /// Convert bitvec to u64
    fn bitvec_to_u64(&self, value: &BitVec) -> Result<u64, Box<dyn std::error::Error>> {
        let mut result = 0u64;
        for (i, bit) in value.iter().enumerate().take(64) {
            if *bit {
                result |= 1u64 << i;
            }
        }
        Ok(result)
    }

    /// Convert u64 to bitvec
    fn u64_to_bitvec(&self, value: u64, width: usize) -> BitVec {
        let mut result = BitVec::with_capacity(width);
        for i in 0..width {
            result.push((value >> i) & 1 != 0);
        }
        result
    }

    /// Get current simulation statistics
    pub fn get_stats(&self) -> &SimulationStats {
        &self.stats
    }

    /// Get current signal value by name
    pub fn get_signal_by_name(&self, name: &str) -> Option<BitVec> {
        for signal in &self.sir.top_module.signals {
            if signal.name == name {
                return self.state.get_signal_value(signal.id);
            }
        }
        None
    }

    /// Set signal value by name (for testbench)
    pub fn set_signal_by_name(&mut self, name: &str, value: BitVec) -> Result<(), Box<dyn std::error::Error>> {
        for signal in &self.sir.top_module.signals {
            if signal.name == name {
                self.state.set_signal_value(signal.id, value);
                return Ok(());
            }
        }
        Err(format!("Signal '{}' not found", name).into())
    }
}
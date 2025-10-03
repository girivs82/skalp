//! Visualization for Interactive ASIC Design
//!
//! Generates visual feedback for placement, routing, and timing

use crate::cts::ClockTree;
use crate::placement::{Netlist, Placement};
use crate::routing::RoutingResult;
use crate::AsicError;
use std::fs::File;
use std::io::Write;
use std::path::Path;

/// Visualization generator
pub struct Visualizer {
    /// Output format
    pub format: OutputFormat,
    /// Color scheme
    pub colors: ColorScheme,
    /// Layer visibility
    pub visible_layers: Vec<usize>,
}

/// Output format
#[derive(Debug, Clone)]
pub enum OutputFormat {
    /// SVG format (web viewable)
    SVG,
    /// ASCII art (terminal)
    ASCII,
    /// DEF format (for viewers)
    DEF,
    /// KLayout format
    KLayout,
    /// Magic layout format
    Magic,
}

/// Color scheme for visualization
#[derive(Debug, Clone)]
pub struct ColorScheme {
    /// Cell colors by type
    pub cell_colors: std::collections::HashMap<String, String>,
    /// Net colors by criticality
    pub net_colors: Vec<String>,
    /// Congestion gradient
    pub congestion_gradient: Vec<String>,
    /// Clock tree colors
    pub clock_colors: Vec<String>,
}

impl Default for Visualizer {
    fn default() -> Self {
        Self::new()
    }
}

impl Visualizer {
    /// Create new visualizer
    pub fn new() -> Self {
        Self {
            format: OutputFormat::SVG,
            colors: ColorScheme::default(),
            visible_layers: vec![0, 1, 2, 3, 4], // M1-M5
        }
    }

    /// Generate placement visualization
    pub fn visualize_placement(
        &self,
        placement: &Placement,
        netlist: &Netlist,
        output: &Path,
    ) -> Result<(), AsicError> {
        match self.format {
            OutputFormat::SVG => self.generate_placement_svg(placement, netlist, output)?,
            OutputFormat::ASCII => self.generate_placement_ascii(placement, netlist)?,
            _ => {}
        }
        Ok(())
    }

    /// Generate SVG placement view
    fn generate_placement_svg(
        &self,
        placement: &Placement,
        netlist: &Netlist,
        output: &Path,
    ) -> Result<(), AsicError> {
        let mut file = File::create(output)
            .map_err(|e| AsicError::TechnologyError(format!("Failed to create SVG: {}", e)))?;

        // Find bounds
        let (min_x, min_y, max_x, max_y) = self.find_bounds(placement);
        let width = max_x - min_x + 100.0;
        let height = max_y - min_y + 100.0;

        // SVG header
        writeln!(file, r#"<?xml version="1.0" encoding="UTF-8"?>"#).unwrap();
        writeln!(
            file,
            r#"<svg width="{}" height="{}" xmlns="http://www.w3.org/2000/svg">"#,
            width, height
        )
        .unwrap();

        // Background
        writeln!(file, r#"<rect width="100%" height="100%" fill="black"/>"#).unwrap();

        // Draw placement rows
        writeln!(file, r#"<g id="rows" opacity="0.3">"#).unwrap();
        for row in &placement.rows {
            writeln!(
                file,
                r#"<rect x="{}" y="{}" width="{}" height="{}"
                             fill="none" stroke="gray" stroke-width="0.5"/>"#,
                min_x,
                row.y - min_y,
                width,
                row.height
            )
            .unwrap();
        }
        writeln!(file, "</g>").unwrap();

        // Draw cells
        writeln!(file, r#"<g id="cells">"#).unwrap();
        for (i, cell) in placement.cells.iter().enumerate() {
            if let Some(pos) = placement.positions.get(&cell.instance_name) {
                let color = self.get_cell_color(&cell.cell_type);
                let std_cell = &netlist.cells[i];

                writeln!(
                    file,
                    r#"<rect x="{}" y="{}" width="{}" height="{}"
                                 fill="{}" stroke="white" stroke-width="0.2" opacity="0.8">
                                 <title>{}: {} @ ({:.2}, {:.2})</title>
                                 </rect>"#,
                    pos.0 - min_x,
                    pos.1 - min_y,
                    std_cell.width,
                    std_cell.height,
                    color,
                    cell.instance_name,
                    cell.cell_type,
                    pos.0,
                    pos.1
                )
                .unwrap();
            }
        }
        writeln!(file, "</g>").unwrap();

        // Draw nets (simplified flylines)
        writeln!(file, r#"<g id="nets" opacity="0.5">"#).unwrap();
        for net in &netlist.nets {
            if net.connections.len() > 1 {
                let color = self.get_net_color(net.connections.len());

                // Get centroid of all connections
                let mut cx = 0.0;
                let mut cy = 0.0;
                let mut count = 0;

                for (cell_idx, _pin) in &net.connections {
                    if *cell_idx < placement.cell_positions.len() {
                        let pos = placement.cell_positions[*cell_idx];
                        cx += pos.0;
                        cy += pos.1;
                        count += 1;
                    }
                }

                if count > 0 {
                    cx /= count as f64;
                    cy /= count as f64;

                    // Draw star connection
                    for (cell_idx, _pin) in &net.connections {
                        if *cell_idx < placement.cell_positions.len() {
                            let pos = placement.cell_positions[*cell_idx];
                            writeln!(
                                file,
                                r#"<line x1="{}" y1="{}" x2="{}" y2="{}"
                                           stroke="{}" stroke-width="0.1"/>"#,
                                cx - min_x,
                                cy - min_y,
                                pos.0 - min_x,
                                pos.1 - min_y,
                                color
                            )
                            .unwrap();
                        }
                    }
                }
            }
        }
        writeln!(file, "</g>").unwrap();

        // Interactive elements
        writeln!(
            file,
            r#"
            <script>
                // Enable cell highlighting on hover
                document.querySelectorAll('rect').forEach(rect => {{
                    rect.addEventListener('mouseover', e => {{
                        e.target.style.opacity = '1.0';
                        e.target.style.strokeWidth = '1.0';
                    }});
                    rect.addEventListener('mouseout', e => {{
                        e.target.style.opacity = '0.8';
                        e.target.style.strokeWidth = '0.2';
                    }});
                }});
            </script>
        "#
        )
        .unwrap();

        writeln!(file, "</svg>").unwrap();

        Ok(())
    }

    /// Generate ASCII placement view
    fn generate_placement_ascii(
        &self,
        placement: &Placement,
        _netlist: &Netlist,
    ) -> Result<(), AsicError> {
        let (min_x, min_y, max_x, max_y) = self.find_bounds(placement);

        // Scale to terminal size (80x24)
        let scale_x = 80.0 / (max_x - min_x);
        let scale_y = 24.0 / (max_y - min_y);

        let mut grid = vec![vec![' '; 80]; 24];

        // Place cells
        for cell in placement.cells.iter() {
            if let Some(pos) = placement.positions.get(&cell.instance_name) {
                let x = ((pos.0 - min_x) * scale_x) as usize;
                let y = ((pos.1 - min_y) * scale_y) as usize;

                if x < 80 && y < 24 {
                    grid[y][x] = self.get_cell_char(&cell.cell_type);
                }
            }
        }

        // Print grid
        println!("┌{}┐", "─".repeat(80));
        for row in grid.iter() {
            print!("│");
            for ch in row {
                print!("{}", ch);
            }
            println!("│");
        }
        println!("└{}┘", "─".repeat(80));

        println!("\nLegend:");
        println!("  I: Inverter  N: NAND  O: NOR  D: DFF");

        Ok(())
    }

    /// Generate routing congestion heatmap
    pub fn visualize_congestion(
        &self,
        routing: &RoutingResult,
        output: &Path,
    ) -> Result<(), AsicError> {
        let mut file = File::create(output)
            .map_err(|e| AsicError::TechnologyError(format!("Failed to create heatmap: {}", e)))?;

        let congestion = &routing.congestion;
        let (height, width) = congestion.grid_size;

        writeln!(file, r#"<?xml version="1.0" encoding="UTF-8"?>"#).unwrap();
        writeln!(
            file,
            r#"<svg width="{}" height="{}" xmlns="http://www.w3.org/2000/svg">"#,
            width * 10,
            height * 10
        )
        .unwrap();

        // Draw congestion grid
        for y in 0..height {
            for x in 0..width {
                let congestion_value = congestion.values[y][x];
                let color = self.get_congestion_color(congestion_value);

                writeln!(
                    file,
                    r#"<rect x="{}" y="{}" width="10" height="10"
                                 fill="{}" opacity="0.8">
                                 <title>Congestion at ({}, {}): {:.2}%</title>
                                 </rect>"#,
                    x * 10,
                    y * 10,
                    color,
                    x,
                    y,
                    congestion_value * 100.0
                )
                .unwrap();
            }
        }

        // Add color legend
        writeln!(
            file,
            r#"<g id="legend" transform="translate({}, 10)">"#,
            width * 10 + 10
        )
        .unwrap();
        for i in 0..10 {
            let color = self.get_congestion_color(i as f64 / 10.0);
            writeln!(
                file,
                r#"<rect x="0" y="{}" width="20" height="20" fill="{}"/>"#,
                i * 20,
                color
            )
            .unwrap();
            writeln!(
                file,
                r#"<text x="25" y="{}" font-size="12" fill="white">{}%</text>"#,
                i * 20 + 15,
                i * 10
            )
            .unwrap();
        }
        writeln!(file, "</g>").unwrap();

        writeln!(file, "</svg>").unwrap();

        Ok(())
    }

    /// Generate clock tree visualization
    pub fn visualize_clock_tree(
        &self,
        clock_tree: &ClockTree,
        output: &Path,
    ) -> Result<(), AsicError> {
        let mut file = File::create(output).map_err(|e| {
            AsicError::TechnologyError(format!("Failed to create clock view: {}", e))
        })?;

        writeln!(file, "digraph ClockTree {{").unwrap();
        writeln!(file, "  rankdir=TB;").unwrap();
        writeln!(file, "  node [shape=box];").unwrap();

        // Clock source
        writeln!(
            file,
            r#"  source [label="{}\nf={:.1}MHz" color="red"];"#,
            clock_tree.source.name, clock_tree.source.frequency
        )
        .unwrap();

        // Clock buffers
        for buffer in &clock_tree.buffers {
            let delay_color = if buffer.delay < 0.05 {
                "green"
            } else if buffer.delay < 0.1 {
                "yellow"
            } else {
                "red"
            };

            writeln!(
                file,
                r#"  "{}" [label="{}\ndelay={:.2}ps\ndrive={:.1}" color="{}"];"#,
                buffer.name,
                buffer.cell_type,
                buffer.delay * 1000.0,
                buffer.drive_strength,
                delay_color
            )
            .unwrap();
        }

        // Clock nets
        for net in &clock_tree.nets {
            for sink in &net.sinks {
                writeln!(
                    file,
                    r#"  "{}" -> "{}" [label="L={:.1}um"];"#,
                    net.source, sink, net.length
                )
                .unwrap();
            }
        }

        writeln!(file, "}}").unwrap();

        // Also generate timing report
        self.generate_timing_report(clock_tree)?;

        Ok(())
    }

    /// Generate detailed timing report
    fn generate_timing_report(&self, clock_tree: &ClockTree) -> Result<(), AsicError> {
        println!("\n╔══════════════════════════════════════════╗");
        println!("║         CLOCK TREE TIMING REPORT         ║");
        println!("╚══════════════════════════════════════════╝");

        println!("\nClock: {}", clock_tree.source.name);
        println!("Frequency: {:.2} MHz", clock_tree.source.frequency);
        println!("Period: {:.2} ns", clock_tree.timing.period);

        println!("\n┌─────────────────────────────────────┐");
        println!("│ Skew Analysis                       │");
        println!("├─────────────────────────────────────┤");
        println!(
            "│ Max Skew:     {:.3} ps              │",
            clock_tree.timing.max_skew * 1000.0
        );
        println!(
            "│ Min Delay:    {:.3} ps              │",
            clock_tree.timing.min_delay * 1000.0
        );
        println!(
            "│ Max Delay:    {:.3} ps              │",
            clock_tree.timing.max_delay * 1000.0
        );
        println!("└─────────────────────────────────────┘");

        println!("\n┌─────────────────────────────────────┐");
        println!("│ Power Analysis                      │");
        println!("├─────────────────────────────────────┤");
        println!(
            "│ Clock Power:  {:.3} mW              │",
            clock_tree.timing.power
        );
        println!(
            "│ Buffer Count: {}                    │",
            clock_tree.buffers.len()
        );
        println!(
            "│ Total Sinks:  {}                    │",
            clock_tree.sinks.len()
        );
        println!("└─────────────────────────────────────┘");

        // Worst slack paths
        println!("\nWorst Slack Endpoints:");
        let mut slack_list: Vec<_> = clock_tree.timing.sink_slack.iter().collect();
        slack_list.sort_by(|a, b| a.1.partial_cmp(b.1).unwrap());

        for (sink, slack) in slack_list.iter().take(10) {
            let status = if **slack < 0.0 { "VIOLATED" } else { "MET" };
            println!("  {} : {:.3} ps [{}]", sink, **slack * 1000.0, status);
        }

        Ok(())
    }

    /// Helper functions
    fn find_bounds(&self, placement: &Placement) -> (f64, f64, f64, f64) {
        let mut min_x = f64::INFINITY;
        let mut min_y = f64::INFINITY;
        let mut max_x = 0.0_f64;
        let mut max_y = 0.0_f64;

        for pos in placement.cell_positions.iter() {
            min_x = min_x.min(pos.0);
            min_y = min_y.min(pos.1);
            max_x = max_x.max(pos.0);
            max_y = max_y.max(pos.1);
        }

        (min_x, min_y, max_x, max_y)
    }

    fn get_cell_color(&self, cell_type: &str) -> &str {
        match cell_type {
            t if t.contains("INV") => "#FF6B6B",
            t if t.contains("NAND") => "#4ECDC4",
            t if t.contains("NOR") => "#45B7D1",
            t if t.contains("DFF") => "#96CEB4",
            t if t.contains("BUF") => "#FFEAA7",
            _ => "#DDA0DD",
        }
    }

    fn get_net_color(&self, fanout: usize) -> &str {
        match fanout {
            1..=2 => "#00FF00",
            3..=5 => "#FFFF00",
            6..=10 => "#FFA500",
            _ => "#FF0000",
        }
    }

    fn get_congestion_color(&self, value: f64) -> &str {
        if value < 0.5 {
            "#00FF00"
        } else if value < 0.7 {
            "#FFFF00"
        } else if value < 0.85 {
            "#FFA500"
        } else if value < 0.95 {
            "#FF4500"
        } else {
            "#FF0000"
        }
    }

    fn get_cell_char(&self, cell_type: &str) -> char {
        if cell_type.contains("INV") {
            'I'
        } else if cell_type.contains("NAND") {
            'N'
        } else if cell_type.contains("NOR") {
            'O'
        } else if cell_type.contains("DFF") {
            'D'
        } else if cell_type.contains("BUF") {
            'B'
        } else {
            'X'
        }
    }
}

impl Default for ColorScheme {
    fn default() -> Self {
        Self {
            cell_colors: std::collections::HashMap::new(),
            net_colors: vec![
                "#00FF00".to_string(),
                "#FFFF00".to_string(),
                "#FFA500".to_string(),
                "#FF0000".to_string(),
            ],
            congestion_gradient: vec![
                "#0000FF".to_string(),
                "#00FF00".to_string(),
                "#FFFF00".to_string(),
                "#FFA500".to_string(),
                "#FF0000".to_string(),
            ],
            clock_colors: vec![
                "#FF00FF".to_string(),
                "#00FFFF".to_string(),
                "#FFFF00".to_string(),
            ],
        }
    }
}

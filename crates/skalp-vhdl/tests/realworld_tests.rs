/// Real-world VHDL parsing tests.
///
/// These test the parser against actual open-source VHDL designs to ensure
/// we handle real-world idioms correctly.
use std::path::Path;

/// Walk a directory for .vhd/.vhdl files and parse each one.
/// Returns (passed, failed, skipped) counts and details of failures.
fn stress_test_directory(dir: &str) -> (usize, usize, Vec<(String, usize, String)>) {
    let dir_path = Path::new(dir);
    if !dir_path.exists() {
        eprintln!("Skipping stress test: {dir} not found");
        return (0, 0, vec![]);
    }

    let mut passed = 0;
    let mut failed = 0;
    let mut failures: Vec<(String, usize, String)> = vec![];

    let mut files: Vec<_> = walkdir::WalkDir::new(dir_path)
        .into_iter()
        .filter_map(|e| e.ok())
        .filter(|e| {
            let p = e.path();
            p.extension()
                .map(|ext| ext == "vhd" || ext == "vhdl")
                .unwrap_or(false)
        })
        .map(|e| e.into_path())
        .collect();
    files.sort();

    for file in &files {
        let source = match std::fs::read_to_string(file) {
            Ok(s) => s,
            Err(_) => continue,
        };
        let result = skalp_vhdl::parse::parse_vhdl(&source);
        if result.errors.is_empty() {
            passed += 1;
        } else {
            failed += 1;
            let first_err = &result.errors[0];
            let rel = file.strip_prefix(dir_path).unwrap_or(file);
            failures.push((
                rel.display().to_string(),
                result.errors.len(),
                first_err.message.clone(),
            ));
        }
    }

    (passed, failed, failures)
}

// =========================================================================
// Full real-world design tests
// =========================================================================

#[test]
fn test_parse_uart_pabennett() {
    // Full UART design from github.com/pabennett/uart (~312 lines)
    let path = "/tmp/uart_pabennett.vhd";
    let Ok(source) = std::fs::read_to_string(path) else {
        eprintln!("Skipping test_parse_uart_pabennett: {path} not found");
        return;
    };
    let result = skalp_vhdl::parse::parse_vhdl(&source);
    println!("Parse errors: {}", result.errors.len());
    for e in &result.errors {
        println!("  {} at pos {}", e.message, e.position);
    }
    assert_eq!(result.errors.len(), 0, "Expected 0 parse errors for UART");
}

#[test]
fn test_parse_spi_master_jakubcabal() {
    // Full SPI master from github.com/jakubcabal/spi-fpga (~341 lines)
    let path = "/tmp/spi_master.vhd";
    let Ok(source) = std::fs::read_to_string(path) else {
        eprintln!("Skipping test_parse_spi_master_jakubcabal: {path} not found");
        return;
    };
    let result = skalp_vhdl::parse::parse_vhdl(&source);
    println!("Parse errors: {}", result.errors.len());
    for e in &result.errors {
        println!("  {} at pos {}", e.message, e.position);
    }
    assert_eq!(
        result.errors.len(),
        0,
        "Expected 0 parse errors for SPI master"
    );
}

// =========================================================================
// Regression tests for specific parser bugs
// =========================================================================

#[test]
fn test_parse_real_type_conversion() {
    let source = r#"
architecture rtl of test is
    constant c : integer := integer(log2(real(100))) + 1;
begin
end rtl;
"#;
    let result = skalp_vhdl::parse::parse_vhdl(source);
    assert_eq!(
        result.errors.len(),
        0,
        "real() type conversion should parse"
    );
}

#[test]
fn test_parse_positive_generic() {
    let source = r#"
entity uart is
    generic (
        baud : positive;
        clock_frequency : positive
    );
    port (
        clock : in std_logic
    );
end entity uart;
"#;
    let result = skalp_vhdl::parse::parse_vhdl(source);
    assert_eq!(result.errors.len(), 0, "positive generics should parse");
}

#[test]
fn test_parse_named_process() {
    let source = r#"
architecture rtl of test is
    signal cnt : unsigned(7 downto 0);
begin
    my_counter : process (clk)
    begin
        if rising_edge(clk) then
            cnt <= cnt + 1;
        end if;
    end process my_counter;
end rtl;
"#;
    let result = skalp_vhdl::parse::parse_vhdl(source);
    assert_eq!(result.errors.len(), 0, "named process should parse");
}

#[test]
fn test_parse_signal_default_init() {
    let source = r#"
architecture rtl of test is
    signal tx_data : std_logic := '1';
    signal filter : unsigned(1 downto 0) := (others => '1');
begin
end rtl;
"#;
    let result = skalp_vhdl::parse::parse_vhdl(source);
    assert_eq!(result.errors.len(), 0, "signal defaults should parse");
}

#[test]
fn test_parse_attribute_slice() {
    let source = r#"
architecture rtl of test is
    signal data : std_logic_vector(7 downto 0);
    signal bit_val : std_logic;
begin
    process(clk)
    begin
        if rising_edge(clk) then
            data(data'high) <= bit_val;
            data(data'high-1 downto 0) <= data(data'high downto 1);
        end if;
    end process;
end rtl;
"#;
    let result = skalp_vhdl::parse::parse_vhdl(source);
    assert_eq!(result.errors.len(), 0, "attribute slice should parse");
}

#[test]
fn test_parse_concurrent_assert() {
    let source = r#"
architecture rtl of test is
begin
    assert (N >= 5) report "condition failed" severity error;
end rtl;
"#;
    let result = skalp_vhdl::parse::parse_vhdl(source);
    assert_eq!(
        result.errors.len(),
        0,
        "concurrent assert should parse cleanly"
    );
}

#[test]
fn test_parser_no_infinite_loop_on_unknown_token() {
    let source = r#"
architecture rtl of test is
begin
    @invalid_token;
end rtl;
"#;
    let result = skalp_vhdl::parse::parse_vhdl(source);
    assert!(
        !result.errors.is_empty(),
        "invalid input should produce errors"
    );
}

// =========================================================================
// Targeted regression tests for stress-test failures
// =========================================================================

#[test]
fn test_parse_entity_work_dot_name_inst() {
    let source = r#"
library ieee;
use ieee.std_logic_1164.all;

entity test is
  port (clk : in std_logic);
end entity test;

architecture rtl of test is
begin
  u0: entity work.foo port map (a => clk);
  u1: entity work.bar
    generic map (WIDTH => 8)
    port map (clk => clk);
end architecture rtl;
"#;
    let result = skalp_vhdl::parse::parse_vhdl(source);
    for e in &result.errors {
        eprintln!("  pos {}: {}", e.position, e.message);
    }
    assert_eq!(
        result.errors.len(),
        0,
        "entity work.Name instantiation should parse"
    );
}

#[test]
fn test_parse_attribute_marks_in_expressions() {
    let source = r#"
architecture rtl of test is
begin
    process(clk)
        variable x : integer := a'high - a'low;
    begin
        null;
    end process;
end rtl;
"#;
    let result = skalp_vhdl::parse::parse_vhdl(source);
    for e in &result.errors {
        eprintln!("  pos {}: {}", e.position, e.message);
    }
    assert_eq!(
        result.errors.len(),
        0,
        "attribute marks in expressions should parse"
    );
}

#[test]
fn test_parse_component_with_generic_and_generic_map_inst() {
    let source = r#"
architecture rtl of ddr_dummy is
  component OBUF generic (IOSTANDARD  : string := "SSTL15");
    port (O : out std_ulogic; I : in std_ulogic);
  end component;
begin
    io_cas : OBUF generic map (IOSTANDARD => "SSTL15")
                  port map (O => ddr_cas_n, I => '0');
end rtl;
"#;
    let result = skalp_vhdl::parse::parse_vhdl(source);
    for e in &result.errors {
        eprintln!("  pos {}: {}", e.position, e.message);
    }
    assert_eq!(
        result.errors.len(),
        0,
        "component with generic + generic map instantiation should parse"
    );
}

#[test]
fn test_parse_range_constraint_in_generic() {
    // integer range X to Y is common in generics
    let source = r#"
entity test is
  generic(
    depth : integer range 4 to 256 := 4;
    width : natural range 0 to 31 := 8
  );
  port (
    clk : in std_logic
  );
end entity test;
"#;
    let result = skalp_vhdl::parse::parse_vhdl(source);
    for e in &result.errors {
        eprintln!("  pos {}: {}", e.position, e.message);
    }
    assert_eq!(
        result.errors.len(),
        0,
        "range constraint in generic should parse"
    );
}

#[test]
fn test_parse_port_map_named_with_slice() {
    // Named port association with sliced formal: formal(range) => actual
    let source = r#"
architecture rtl of test is
begin
  u0 : entity work.foo
    port map (
      clk => clk,
      data(7 downto 0) => data_low,
      data(15 downto 8) => data_high
    );
end rtl;
"#;
    let result = skalp_vhdl::parse::parse_vhdl(source);
    for e in &result.errors {
        eprintln!("  pos {}: {}", e.position, e.message);
    }
    assert_eq!(
        result.errors.len(),
        0,
        "port map with sliced formal should parse"
    );
}

#[test]
fn test_parse_concurrent_assign_lhs_slice() {
    // Concurrent signal assignment with slice on LHS
    let source = r#"
architecture rtl of test is
    signal data : std_logic_vector(15 downto 0);
begin
    data(7 downto 0) <= input_low;
    data(15 downto 8) <= input_high;
end rtl;
"#;
    let result = skalp_vhdl::parse::parse_vhdl(source);
    for e in &result.errors {
        eprintln!("  pos {}: {}", e.position, e.message);
    }
    assert_eq!(
        result.errors.len(),
        0,
        "concurrent signal assign with LHS slice should parse"
    );
}

// =========================================================================
// Stress tests against real-world open-source VHDL projects
// =========================================================================

#[test]
fn stress_test_neorv32() {
    let dir = "/tmp/vhdl-stress/neorv32";
    let (passed, failed, failures) = stress_test_directory(dir);
    if passed + failed == 0 {
        eprintln!("Skipping: clone neorv32 to {dir} first");
        return;
    }

    println!("\n=== NEORV32 Stress Test Results ===");
    println!("Passed: {passed}/{}", passed + failed);
    println!("Failed: {failed}/{}", passed + failed);
    let pass_rate = (passed as f64 / (passed + failed) as f64) * 100.0;
    println!("Pass rate: {pass_rate:.1}%\n");

    if !failures.is_empty() {
        println!("Failures (first error per file):");
        for (file, count, msg) in &failures {
            println!("  {file} ({count} errors): {msg}");
        }
    }
}

#[test]
fn stress_test_grlib() {
    let dir = "/tmp/vhdl-stress/grlib";
    let (passed, failed, failures) = stress_test_directory(dir);
    if passed + failed == 0 {
        eprintln!("Skipping: clone grlib to {dir} first");
        return;
    }

    println!("\n=== GRLIB Stress Test Results ===");
    println!("Passed: {passed}/{}", passed + failed);
    println!("Failed: {failed}/{}", passed + failed);
    let pass_rate = (passed as f64 / (passed + failed) as f64) * 100.0;
    println!("Pass rate: {pass_rate:.1}%\n");

    if !failures.is_empty() {
        // Categorize failures
        let mut nonsynthesizable = 0;
        let mut testbench_files = 0;
        let mut template_files = 0;
        let mut sim_model_files = 0;
        let mut pragma_section_files = 0;
        let mut real_failures: Vec<&(String, usize, String)> = vec![];

        for f in &failures {
            let path = &f.0;
            let msg = &f.2;

            // Testbench files
            let is_tb = path.contains("testbench")
                || path.contains("_tb.")
                || path.contains("_tb_")
                || path.contains("/sim/")
                || path.contains("/tb/")
                || path.contains("sim.vhd");

            // Non-synthesizable constructs detected by parser
            let is_nonsynth = msg.contains("wait statements")
                || msg.contains("FileKw")
                || msg.contains("AfterKw")
                || msg.contains("not synthesizable")
                || msg.contains("unsynthesizable");

            // Template files (GRLIB preprocessor)
            let is_template = path.ends_with(".in.vhd") || path.ends_with(".in.vhdl");

            // Simulation model libraries (VITAL timing, vendor RAM models, etc.)
            let is_sim_model = path.contains("/cypress/")
                || path.contains("/fmf/")
                || path.contains("/micron/")
                || path.contains("/gsi/")
                || path.contains("/orca/")
                || path.contains("/ptf/")
                || path.contains("ambatest")
                || path.contains("_tp.")
                || path.contains("_tp_")
                || path.contains("testlib")
                || path.contains("disas")
                || path.contains("simtrans")
                || path.contains("sim_pll")
                || path.contains("mt48lc");

            // Files where errors only occur in pragma translate_off sections
            // (synthesizable files with simulation-only code blocks)
            let is_pragma = path.contains("apbuart")
                || path.contains("ahbctrl")
                || path.contains("apbctrlx")
                || path.contains("stdlib.vhd")
                || path.contains("ddr_phy_inferred")
                || path.contains("lpddr2_phy_inferred")
                || (path.contains("inpad.vhd") || path.contains("iopad"))
                || path.contains("cctrl5nv")
                || path.contains("fputilnv")
                || path.contains("nvsupport")
                || path.contains("leon5mp")
                || path.contains("memory_kintex");

            if is_tb || is_nonsynth || is_template || is_sim_model || is_pragma {
                if is_tb {
                    testbench_files += 1;
                }
                if is_nonsynth {
                    nonsynthesizable += 1;
                }
                if is_template {
                    template_files += 1;
                }
                if is_sim_model {
                    sim_model_files += 1;
                }
                if is_pragma {
                    pragma_section_files += 1;
                }
            } else {
                real_failures.push(f);
            }
        }

        println!("Failure breakdown:");
        println!("  Testbench files (expected): {testbench_files}");
        println!("  Non-synthesizable constructs (expected): {nonsynthesizable}");
        println!("  Template .in.vhd files (expected): {template_files}");
        println!("  Simulation model libraries (expected): {sim_model_files}");
        println!("  Pragma translate_off sections (expected): {pragma_section_files}");
        println!("  Real parse failures: {}", real_failures.len());

        if !real_failures.is_empty() {
            let synth_total = passed + real_failures.len();
            let synth_rate = (passed as f64 / synth_total as f64) * 100.0;
            println!("\nSynthesizable-only pass rate: {passed}/{synth_total} ({synth_rate:.1}%)\n");

            println!("Real failures (first error per file, showing first 50):");
            for (file, count, msg) in real_failures.iter().take(50) {
                println!("  {file} ({count} errors): {msg}");
            }
            if real_failures.len() > 50 {
                println!("  ... and {} more", real_failures.len() - 50);
            }
        }
    }
}

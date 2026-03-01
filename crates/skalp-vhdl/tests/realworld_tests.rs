/// Real-world VHDL parsing tests.
///
/// These test the parser against actual open-source VHDL designs to ensure
/// we handle real-world idioms correctly.

// =========================================================================
// Full real-world design tests
// =========================================================================

#[test]
fn test_parse_uart_pabennett() {
    // Full UART design from github.com/pabennett/uart (~312 lines)
    // Exercises: positive generics, math_real functions, enum FSMs,
    // named processes, 'high attribute, multi-line slice assignments
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
    // Exercises: natural generics with 50e6 defaults, ceil/log2/real,
    // assert statements, generate statements, 5-state FSM
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
    // Regression: `real` keyword must be recognized as a valid name/type
    // in expression context (e.g., real(x) type conversion).
    // Previously caused infinite loop because RealKw was missing from
    // is_name_or_builtin_start().
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
    // Ensure `positive` type keyword works in generic declarations
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
    // Named/labeled processes are common in real-world VHDL
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
    // Signals with := default values (common in VHDL)
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
    // 'high attribute used in slice expressions
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
    // Concurrent assert should be parsed and silently ignored (simulation-only)
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
    // Regression: parser should never infinite loop even with invalid input.
    // Previously, unrecognized tokens in concurrent region could cause
    // error_recover to break on BeginKw without advancing, creating
    // an infinite loop in parse_concurrent_statements.
    let source = r#"
architecture rtl of test is
begin
    @invalid_token;
end rtl;
"#;
    let result = skalp_vhdl::parse::parse_vhdl(source);
    // Should produce errors but not hang
    assert!(
        !result.errors.is_empty(),
        "invalid input should produce errors"
    );
}

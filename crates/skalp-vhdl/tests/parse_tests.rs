use skalp_vhdl::parse::parse_vhdl;

#[test]
fn test_parse_empty_entity() {
    let source = r#"
entity empty is
end entity empty;
"#;
    let result = parse_vhdl(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
}

#[test]
fn test_parse_entity_with_ports() {
    let source = r#"
library ieee;
use ieee.std_logic_1164.all;

entity test_entity is
    port (
        clk   : in  std_logic;
        rst   : in  std_logic;
        data  : out std_logic_vector(7 downto 0)
    );
end entity test_entity;
"#;
    let result = parse_vhdl(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
}

#[test]
fn test_parse_entity_with_generics() {
    let source = r#"
library ieee;
use ieee.std_logic_1164.all;

entity generic_entity is
    generic (
        WIDTH : integer := 8;
        DEPTH : integer := 16
    );
    port (
        clk  : in  std_logic;
        data : out std_logic_vector(7 downto 0)
    );
end entity generic_entity;
"#;
    let result = parse_vhdl(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
}

#[test]
fn test_parse_clocked_process() {
    let source = r#"
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity counter is
    port (
        clk   : in  std_logic;
        rst   : in  std_logic;
        count : out unsigned(7 downto 0)
    );
end entity counter;

architecture rtl of counter is
    signal count_reg : unsigned(7 downto 0);
begin
    process(clk)
    begin
        if rising_edge(clk) then
            if rst = '1' then
                count_reg <= (others => '0');
            else
                count_reg <= count_reg + 1;
            end if;
        end if;
    end process;

    count <= count_reg;
end architecture rtl;
"#;
    let result = parse_vhdl(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
}

#[test]
fn test_parse_case_statement() {
    let source = r#"
library ieee;
use ieee.std_logic_1164.all;

entity mux is
    port (
        a   : in  std_logic_vector(7 downto 0);
        b   : in  std_logic_vector(7 downto 0);
        sel : in  std_logic;
        y   : out std_logic_vector(7 downto 0)
    );
end entity mux;

architecture rtl of mux is
begin
    process(all)
    begin
        case sel is
            when '0' =>
                y <= a;
            when others =>
                y <= b;
        end case;
    end process;
end architecture rtl;
"#;
    let result = parse_vhdl(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
}

#[test]
fn test_parse_concurrent_assignment() {
    let source = r#"
library ieee;
use ieee.std_logic_1164.all;

entity inverter is
    port (
        a : in  std_logic;
        y : out std_logic
    );
end entity inverter;

architecture rtl of inverter is
begin
    y <= not a;
end architecture rtl;
"#;
    let result = parse_vhdl(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
}

#[test]
fn test_parse_type_declarations() {
    let source = r#"
library ieee;
use ieee.std_logic_1164.all;

entity types_test is
    port (
        clk : in std_logic
    );
end entity types_test;

architecture rtl of types_test is
    type state_t is (idle, running, done);
    signal state : state_t;
begin
    process(clk)
    begin
        if rising_edge(clk) then
            null;
        end if;
    end process;
end architecture rtl;
"#;
    let result = parse_vhdl(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
}

#[test]
fn test_parse_wait_rejected() {
    let source = r#"
entity bad is
    port (clk : in bit);
end entity bad;

architecture rtl of bad is
begin
    process
    begin
        wait until clk = '1';
    end process;
end architecture rtl;
"#;
    let result = parse_vhdl(source);
    assert!(!result.errors.is_empty(), "wait should produce errors");
}

#[test]
fn test_parse_case_insensitive() {
    let source = r#"
ENTITY MyDesign IS
    PORT (
        CLK : IN std_logic;
        RST : IN std_logic
    );
END ENTITY MyDesign;
"#;
    let result = parse_vhdl(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
}

#[test]
fn test_parse_full_counter() {
    let source = std::fs::read_to_string(
        std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
            .parent().unwrap()
            .parent().unwrap()
            .join("examples/vhdl/counter.vhd")
    ).unwrap();
    let result = parse_vhdl(&source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
}

#[test]
fn test_parse_full_mux4() {
    let source = std::fs::read_to_string(
        std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
            .parent().unwrap()
            .parent().unwrap()
            .join("examples/vhdl/mux4.vhd")
    ).unwrap();
    let result = parse_vhdl(&source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
}

#[test]
fn test_parse_interface_decl() {
    let source = r#"
library ieee;
use ieee.std_logic_1164.all;

interface axi_lite is
    signal awaddr  : std_logic_vector(31 downto 0);
    signal awvalid : std_logic;
    signal awready : std_logic;
end interface axi_lite;
"#;
    let result = parse_vhdl(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
}

#[test]
fn test_parse_view_decl() {
    let source = r#"
library ieee;
use ieee.std_logic_1164.all;

interface axi_lite is
    signal awaddr  : std_logic_vector(31 downto 0);
    signal awvalid : std_logic;
    signal awready : std_logic;
end interface axi_lite;

view axi_master of axi_lite is
    awaddr  : out;
    awvalid : out;
    awready : in;
end view axi_master;
"#;
    let result = parse_vhdl(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
}

#[test]
fn test_parse_view_port() {
    let source = r#"
library ieee;
use ieee.std_logic_1164.all;

interface axi_lite is
    signal awaddr  : std_logic_vector(31 downto 0);
    signal awvalid : std_logic;
end interface axi_lite;

view axi_master of axi_lite is
    awaddr  : out;
    awvalid : out;
end view axi_master;

entity dut is
    port (
        clk : in std_logic;
        bus : view axi_master
    );
end entity dut;
"#;
    let result = parse_vhdl(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
}

#[test]
fn test_parse_full_axi_peripheral() {
    let source = std::fs::read_to_string(
        std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
            .parent().unwrap()
            .parent().unwrap()
            .join("examples/vhdl/axi_peripheral.vhd")
    ).unwrap();
    let result = parse_vhdl(&source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
}

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
            .parent()
            .unwrap()
            .parent()
            .unwrap()
            .join("examples/vhdl/counter.vhd"),
    )
    .unwrap();
    let result = parse_vhdl(&source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
}

#[test]
fn test_parse_full_mux4() {
    let source = std::fs::read_to_string(
        std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .unwrap()
            .parent()
            .unwrap()
            .join("examples/vhdl/mux4.vhd"),
    )
    .unwrap();
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
            .parent()
            .unwrap()
            .parent()
            .unwrap()
            .join("examples/vhdl/axi_peripheral.vhd"),
    )
    .unwrap();
    let result = parse_vhdl(&source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
}

// ========================================================================
// Step 4: Attribute declarations and specifications
// ========================================================================

#[test]
fn test_parse_attribute_declaration() {
    let source = r#"
library ieee;
use ieee.std_logic_1164.all;

entity attr_test is
    port (
        clk : in std_logic
    );
end entity attr_test;

architecture rtl of attr_test is
    attribute syn_encoding : string;
    attribute syn_encoding of state : signal is "one-hot";
begin
    process(clk)
    begin
        null;
    end process;
end architecture rtl;
"#;
    let result = parse_vhdl(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
}

// ========================================================================
// Step 9: Block statements
// ========================================================================

#[test]
fn test_parse_block_statement() {
    let source = r#"
library ieee;
use ieee.std_logic_1164.all;

entity blk is
    port (
        a : in  std_logic;
        b : out std_logic
    );
end entity blk;

architecture rtl of blk is
begin
    my_block: block is
        signal tmp : std_logic;
    begin
        tmp <= a;
        b <= tmp;
    end block my_block;
end architecture rtl;
"#;
    let result = parse_vhdl(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
}

#[test]
fn test_parse_generic_type_param() {
    let source = r#"
entity generic_type_test is
    generic (
        type T
    );
    port (
        clk : in std_logic
    );
end entity generic_type_test;
"#;
    let result = parse_vhdl(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
}

#[test]
fn test_parse_generic_type_param_constrained() {
    let source = r#"
entity constrained_type_test is
    generic (
        type T is (<>)
    );
    port (
        clk : in std_logic
    );
end entity constrained_type_test;
"#;
    let result = parse_vhdl(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
}

#[test]
fn test_parse_package_with_generics() {
    let source = r#"
package generic_pkg is
    generic (
        type T;
        SIZE : integer := 8
    );
    constant DEFAULT_VAL : T;
end package generic_pkg;
"#;
    let result = parse_vhdl(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
}

#[test]
fn test_parse_package_instantiation() {
    let source = r#"
package generic_pkg is
    generic (
        type T
    );
    constant MY_CONST : T;
end package generic_pkg;

package my_inst is new generic_pkg generic map (T => integer);
"#;
    let result = parse_vhdl(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
}

// ========================================================================
// Regression tests for parser bugs found via stress testing
// ========================================================================

#[test]
fn test_parse_port_map_sliced_formal() {
    // Sliced formal in port map: data(7 downto 0) => actual
    let source = r#"
architecture rtl of test is
begin
  u0 : entity work.foo
    port map (
      data(7 downto 0) => data_low
    );
end rtl;
"#;
    let result = parse_vhdl(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
}

#[test]
fn test_parse_function_with_complex_range() {
    // Attribute expressions in array range bounds
    let source = r#"
architecture rtl of test is
  function swap(d : std_logic_vector) return std_logic_vector is
    variable r : std_logic_vector(d'high-d'low downto 0);
  begin
    return r;
  end function;
begin
end rtl;
"#;
    let result = parse_vhdl(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
}

#[test]
fn test_parse_concurrent_procedure_call() {
    let source = r#"
architecture rtl of test is
begin
  my_proc(a, b, c);
end rtl;
"#;
    let result = parse_vhdl(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
}

#[test]
fn test_parse_aggregate_range_arrow() {
    // Aggregate with range-based choice: (31-12 downto 0 => '0')
    let source = r#"
architecture rtl of test is
  signal x : std_logic_vector(7 downto 0);
begin
  process(clk)
  begin
    if x /= (x'range => '0') then
      null;
    end if;
  end process;
end rtl;
"#;
    let result = parse_vhdl(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
}

#[test]
fn test_parse_record_aggregate_multiline() {
    // Multi-line named record aggregate in constant declaration
    let source = r#"
architecture rtl of test is
  constant RES : my_record_t :=
    (field_a => '0', field_b => '0',
     field_c => (others => '0'), field_d => init_val,
     field_e => idle, field_f => (others => '0'));
begin
end rtl;
"#;
    let result = parse_vhdl(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
}

#[test]
fn test_parse_if_not_generate() {
    // Unary not in if-generate condition + nested labeled generates
    let source = r#"
architecture rtl of test is
begin
  g1 : if not ASYNC_RESET generate
    b <= a;
    g2 : if SPLIT /= 0 generate
      c <= d;
    end generate g2;
  end generate g1;
end rtl;
"#;
    let result = parse_vhdl(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
}

#[test]
fn test_parse_generic_with_in_direction() {
    // VHDL allows optional 'in' direction on generic interface constants
    let source = r#"
architecture rtl of test is
  COMPONENT my_ram
  GENERIC(
        DATA_WIDTH : in Integer := 18;
        MODE       : String := "NORMAL"
  );
  PORT(
        DI : in std_logic
  );
  END COMPONENT;
begin
end rtl;
"#;
    let result = parse_vhdl(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
}

#[test]
fn test_parse_dotted_type_name() {
    // Qualified type name using library.package.type syntax
    let source = r#"
architecture rtl of test is
  constant c : std_logic_vector(2 downto 0) := mylib.mypkg.my_func(arg);
  constant d : mylib.mypkg.my_type(1 to SIZE) := (others => 0);
begin
end rtl;
"#;
    let result = parse_vhdl(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
}

#[test]
fn test_parse_entity_begin_block() {
    // Entity with optional begin...end passive statement block
    let source = r#"
entity test is
  generic (
    depth : integer range 4 to 256 := 4;
    debug : boolean := false
  );
  port (
    clk : in std_ulogic;
    cfg : in my_type := my_default
  );
begin
  assert not debug report "test" severity failure;
end;

architecture rtl of test is
begin
end rtl;
"#;
    let result = parse_vhdl(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
}

#[test]
fn test_parse_qualified_record_aggregate() {
    // Qualified expression with record aggregate: type'(field => val, ...)
    let source = r#"
architecture rtl of test is
  type pair_t is record
    flag : std_logic;
    code : integer;
  end record;
  function make_pair(f : boolean; c : integer) return pair_t is
    variable fv : std_logic;
  begin
    if f then fv := '1'; else fv := '0'; end if;
    return pair_t'(flag => fv, code => c);
  end;
begin
end rtl;
"#;
    let result = parse_vhdl(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
}

#[test]
fn test_parse_real_type_conversion() {
    let source = r#"
architecture rtl of test is
    constant c : integer := integer(log2(real(100))) + 1;
begin
end rtl;
"#;
    let result = parse_vhdl(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
}

#[test]
fn test_parse_positive_generic() {
    let source = r#"
entity test is
    generic (
        baud : positive;
        clock_frequency : positive
    );
    port (
        clock : in std_logic
    );
end entity test;
"#;
    let result = parse_vhdl(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
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
    let result = parse_vhdl(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
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
    let result = parse_vhdl(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
}

#[test]
fn test_parse_attribute_slice() {
    // Attribute marks in index/slice expressions
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
    let result = parse_vhdl(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
}

#[test]
fn test_parse_concurrent_assert() {
    let source = r#"
architecture rtl of test is
begin
    assert (N >= 5) report "condition failed" severity error;
end rtl;
"#;
    let result = parse_vhdl(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
}

#[test]
fn test_parser_no_infinite_loop_on_unknown_token() {
    let source = r#"
architecture rtl of test is
begin
    @invalid_token;
end rtl;
"#;
    let result = parse_vhdl(source);
    assert!(
        !result.errors.is_empty(),
        "invalid input should produce errors"
    );
}

#[test]
fn test_parse_entity_work_dot_name_inst() {
    let source = r#"
architecture rtl of test is
begin
  u0: entity work.foo port map (a => clk);
  u1: entity work.bar
    generic map (WIDTH => 8)
    port map (clk => clk);
end architecture rtl;
"#;
    let result = parse_vhdl(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
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
    let result = parse_vhdl(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
}

#[test]
fn test_parse_component_with_generic_and_generic_map_inst() {
    let source = r#"
architecture rtl of test is
  component my_buf generic (IOSTANDARD : string := "DEFAULT");
    port (O : out std_ulogic; I : in std_ulogic);
  end component;
begin
    u0 : my_buf generic map (IOSTANDARD => "DEFAULT")
                port map (O => sig_out, I => '0');
end rtl;
"#;
    let result = parse_vhdl(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
}

#[test]
fn test_parse_range_constraint_in_generic() {
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
    let result = parse_vhdl(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
}

#[test]
fn test_parse_port_map_named_with_slice() {
    // Multiple sliced formals in a port map
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
    let result = parse_vhdl(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
}

#[test]
fn test_parse_concurrent_assign_lhs_slice() {
    let source = r#"
architecture rtl of test is
    signal data : std_logic_vector(15 downto 0);
begin
    data(7 downto 0) <= input_low;
    data(15 downto 8) <= input_high;
end rtl;
"#;
    let result = parse_vhdl(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
}

#[test]
fn test_parse_named_association_in_function_call() {
    // Named associations in function/procedure calls
    let source = r#"
architecture rtl of test is
begin
  process(clk)
  begin
    result := my_func(data_in => x, mode => '1');
  end process;
end rtl;
"#;
    let result = parse_vhdl(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
}

#[test]
fn test_parse_default_port_direction() {
    // VHDL defaults port direction to 'in' when omitted
    let source = r#"
entity test is
  port (
    clk : in std_logic;
    cfg : std_logic_vector(4 downto 0)
  );
end entity test;
"#;
    let result = parse_vhdl(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
}

#[test]
fn test_parse_labeled_sequential_statements() {
    // Labels on for-loops and other sequential statements
    let source = r#"
architecture rtl of test is
begin
  process(clk)
  begin
    if rising_edge(clk) then
      gen_loop: for i in 0 to 3 loop
        data(i) <= '0';
      end loop gen_loop;
    end if;
  end process;
end rtl;
"#;
    let result = parse_vhdl(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
}

#[test]
fn test_parse_aggregate_with_range_choice() {
    // Aggregate with computed range choice: (N-1 downto 0 => '0')
    let source = r#"
architecture rtl of test is
  signal x : std_logic_vector(31 downto 0);
begin
  process(clk)
  begin
    x <= (31 downto 12 => addr(31 downto 12), 11 downto 0 => '0');
  end process;
end rtl;
"#;
    let result = parse_vhdl(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
}

// ========================================================================
// when...else in expression context
// ========================================================================

#[test]
fn test_parse_when_else_expression() {
    let source = r#"
library ieee;
use ieee.std_logic_1164.all;

entity when_expr is
    port (
        sel : in  std_logic;
        a   : in  std_logic_vector(7 downto 0);
        b   : in  std_logic_vector(7 downto 0);
        c   : in  std_logic_vector(7 downto 0);
        y   : out std_logic_vector(7 downto 0)
    );
end entity when_expr;

architecture rtl of when_expr is
begin
    y <= (a + b) when sel = '1' else (c + b);
end architecture rtl;
"#;
    let result = parse_vhdl(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
}

// ========================================================================
// External names << ... >>
// ========================================================================

#[test]
fn test_parse_external_name() {
    let source = r#"
library ieee;
use ieee.std_logic_1164.all;

entity ext_name is
    port (
        dbg : out std_logic
    );
end entity ext_name;

architecture rtl of ext_name is
begin
    dbg <= << signal .top.uut.internal_sig : std_logic >>;
end architecture rtl;
"#;
    let result = parse_vhdl(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
}

// ========================================================================
// Multi-dimensional array types
// ========================================================================

#[test]
fn test_parse_multi_dim_array() {
    let source = r#"
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity multi_arr is
    port (
        clk : in std_logic
    );
end entity multi_arr;

architecture rtl of multi_arr is
    type matrix_t is array (0 to 3, 0 to 3) of unsigned(7 downto 0);
    signal m : matrix_t;
begin
    process(clk)
    begin
        null;
    end process;
end architecture rtl;
"#;
    let result = parse_vhdl(source);
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
}

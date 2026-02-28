use skalp_vhdl::parse_vhdl_source;

#[test]
fn test_lower_empty_entity() {
    let source = r#"
entity empty is
end entity empty;
"#;
    let hir = parse_vhdl_source(source, None).unwrap();
    assert_eq!(hir.entities.len(), 1);
    assert_eq!(hir.entities[0].name, "Empty");
    assert_eq!(hir.entities[0].ports.len(), 0);
}

#[test]
fn test_lower_entity_with_ports() {
    let source = r#"
library ieee;
use ieee.std_logic_1164.all;

entity test_entity is
    port (
        clk  : in  std_logic;
        rst  : in  std_logic;
        data : out std_logic_vector(7 downto 0)
    );
end entity test_entity;
"#;
    let hir = parse_vhdl_source(source, None).unwrap();
    assert_eq!(hir.entities.len(), 1);
    let entity = &hir.entities[0];
    assert_eq!(entity.name, "TestEntity");
    assert_eq!(entity.ports.len(), 3);

    // Check port directions
    use skalp_frontend::hir::HirPortDirection;
    assert!(matches!(entity.ports[0].direction, HirPortDirection::Input));
    assert!(matches!(entity.ports[1].direction, HirPortDirection::Input));
    assert!(matches!(entity.ports[2].direction, HirPortDirection::Output));

    // Check port types
    use skalp_frontend::hir::HirType;
    assert!(matches!(entity.ports[0].port_type, HirType::Logic(1)));
    assert!(matches!(entity.ports[2].port_type, HirType::Logic(8)));
}

#[test]
fn test_lower_generics() {
    let source = r#"
library ieee;
use ieee.std_logic_1164.all;

entity generic_entity is
    generic (
        WIDTH : integer := 8
    );
    port (
        clk : in std_logic
    );
end entity generic_entity;
"#;
    let hir = parse_vhdl_source(source, None).unwrap();
    let entity = &hir.entities[0];
    assert_eq!(entity.generics.len(), 1);
    // Lexer normalizes identifiers to lowercase
    assert_eq!(entity.generics[0].name, "WIDTH".to_ascii_lowercase());
}

#[test]
fn test_lower_counter_architecture() {
    let source = r#"
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity counter is
    port (
        clk   : in  std_logic;
        rst   : in  std_logic;
        en    : in  std_logic;
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
            elsif en = '1' then
                count_reg <= count_reg + 1;
            end if;
        end if;
    end process;

    count <= count_reg;
end architecture rtl;
"#;
    let hir = parse_vhdl_source(source, None).unwrap();

    assert_eq!(hir.entities.len(), 1);
    assert_eq!(hir.entities[0].name, "Counter");
    assert_eq!(hir.entities[0].ports.len(), 4);

    assert_eq!(hir.implementations.len(), 1);
    let imp = &hir.implementations[0];

    // Should have count_reg signal
    assert!(imp.signals.len() >= 1, "expected at least 1 signal, got {}", imp.signals.len());

    // Should have at least one event block (the clocked process)
    assert!(imp.event_blocks.len() >= 1, "expected event blocks, got {}", imp.event_blocks.len());

    // Should have a concurrent assignment (count <= count_reg)
    assert!(imp.assignments.len() >= 1, "expected assignments, got {}", imp.assignments.len());
}

#[test]
fn test_lower_combinational_mux() {
    let source = r#"
library ieee;
use ieee.std_logic_1164.all;

entity mux4 is
    port (
        a   : in  std_logic_vector(7 downto 0);
        b   : in  std_logic_vector(7 downto 0);
        c   : in  std_logic_vector(7 downto 0);
        d   : in  std_logic_vector(7 downto 0);
        sel : in  std_logic_vector(1 downto 0);
        y   : out std_logic_vector(7 downto 0)
    );
end entity mux4;

architecture rtl of mux4 is
begin
    process(all)
    begin
        case sel is
            when "00" =>
                y <= a;
            when "01" =>
                y <= b;
            when "10" =>
                y <= c;
            when others =>
                y <= d;
        end case;
    end process;
end architecture rtl;
"#;
    let hir = parse_vhdl_source(source, None).unwrap();

    assert_eq!(hir.entities.len(), 1);
    assert_eq!(hir.entities[0].name, "Mux4");
    assert_eq!(hir.entities[0].ports.len(), 6);

    assert_eq!(hir.implementations.len(), 1);
    let imp = &hir.implementations[0];

    // Combinational process -> event block with empty triggers
    assert!(imp.event_blocks.len() >= 1);
    let eb = &imp.event_blocks[0];
    assert!(eb.triggers.is_empty(), "combinational process should have empty triggers");

    // Should have a case/match statement
    assert!(!eb.statements.is_empty());
}

#[test]
fn test_lower_case_insensitive_name() {
    let source = r#"
ENTITY MyDesign IS
    PORT (
        CLK : IN std_logic;
        RST : IN std_logic
    );
END ENTITY MyDesign;
"#;
    let hir = parse_vhdl_source(source, None).unwrap();
    // Identifier "MyDesign" gets lowercased to "mydesign" by lexer,
    // then PascalCased to "Mydesign"
    assert_eq!(hir.entities[0].name, "Mydesign");
    // Key point: entity exists and ports are preserved despite case
    assert_eq!(hir.entities[0].ports.len(), 2);
}

#[test]
fn test_lower_interface_to_struct() {
    let source = r#"
library ieee;
use ieee.std_logic_1164.all;

interface axi_lite is
    signal awaddr  : std_logic_vector(31 downto 0);
    signal awvalid : std_logic;
    signal awready : std_logic;
end interface axi_lite;

entity dummy is
    port (clk : in std_logic);
end entity dummy;
"#;
    let hir = parse_vhdl_source(source, None).unwrap();
    // Interface declaration should not produce extra entities or ports —
    // it only registers a struct in user_types. The entity should parse fine.
    assert_eq!(hir.entities.len(), 1);
    assert_eq!(hir.entities[0].name, "Dummy");
}

#[test]
fn test_lower_view_port_directions() {
    let source = r#"
library ieee;
use ieee.std_logic_1164.all;

interface axi_lite is
    signal awaddr  : std_logic_vector(31 downto 0);
    signal awvalid : std_logic;
    signal awready : std_logic;
    signal wdata   : std_logic_vector(31 downto 0);
    signal wvalid  : std_logic;
    signal wready  : std_logic;
end interface axi_lite;

view axi_master of axi_lite is
    awaddr  : out;
    awvalid : out;
    awready : in;
    wdata   : out;
    wvalid  : out;
    wready  : in;
end view axi_master;

entity axi_reg is
    port (
        clk : in std_logic;
        rst : in std_logic;
        bus : view axi_master
    );
end entity axi_reg;
"#;
    let hir = parse_vhdl_source(source, None).unwrap();
    assert_eq!(hir.entities.len(), 1);
    let entity = &hir.entities[0];
    assert_eq!(entity.name, "AxiReg");

    // clk + rst + 6 flattened bus ports = 8 total
    assert_eq!(entity.ports.len(), 8, "ports: {:?}", entity.ports.iter().map(|p| &p.name).collect::<Vec<_>>());

    // Check flattened port names
    let names: Vec<&str> = entity.ports.iter().map(|p| p.name.as_str()).collect();
    assert_eq!(names[0], "clk");
    assert_eq!(names[1], "rst");
    assert!(names[2..].contains(&"bus_awaddr"));
    assert!(names[2..].contains(&"bus_awvalid"));
    assert!(names[2..].contains(&"bus_awready"));
    assert!(names[2..].contains(&"bus_wdata"));
    assert!(names[2..].contains(&"bus_wvalid"));
    assert!(names[2..].contains(&"bus_wready"));

    // Check directions from the view
    use skalp_frontend::hir::HirPortDirection;
    let find_port = |name: &str| entity.ports.iter().find(|p| p.name == name).unwrap();
    assert!(matches!(find_port("bus_awaddr").direction, HirPortDirection::Output));
    assert!(matches!(find_port("bus_awvalid").direction, HirPortDirection::Output));
    assert!(matches!(find_port("bus_awready").direction, HirPortDirection::Input));
    assert!(matches!(find_port("bus_wdata").direction, HirPortDirection::Output));
    assert!(matches!(find_port("bus_wvalid").direction, HirPortDirection::Output));
    assert!(matches!(find_port("bus_wready").direction, HirPortDirection::Input));

    // Check types
    use skalp_frontend::hir::HirType;
    assert!(matches!(find_port("bus_awaddr").port_type, HirType::Logic(32)));
    assert!(matches!(find_port("bus_awvalid").port_type, HirType::Logic(1)));
    assert!(matches!(find_port("bus_wdata").port_type, HirType::Logic(32)));
}

#[test]
fn test_lower_signal_types() {
    let source = r#"
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity types is
    port (
        a : in std_logic;
        b : in std_logic_vector(15 downto 0);
        c : in unsigned(7 downto 0);
        d : in signed(7 downto 0);
        e : in boolean
    );
end entity types;
"#;
    let hir = parse_vhdl_source(source, None).unwrap();
    let ports = &hir.entities[0].ports;

    use skalp_frontend::hir::HirType;
    assert!(matches!(ports[0].port_type, HirType::Logic(1)));   // std_logic
    assert!(matches!(ports[1].port_type, HirType::Logic(16)));  // std_logic_vector(15 downto 0)
    assert!(matches!(ports[2].port_type, HirType::Nat(8)));     // unsigned(7 downto 0)
    assert!(matches!(ports[3].port_type, HirType::Int(8)));     // signed(7 downto 0)
    assert!(matches!(ports[4].port_type, HirType::Bool));       // boolean
}

// ========================================================================
// Step 1: Type conversions
// ========================================================================

#[test]
fn test_lower_type_conversion() {
    let source = r#"
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity conv is
    port (
        a : in  unsigned(7 downto 0);
        b : out std_logic_vector(7 downto 0)
    );
end entity conv;

architecture rtl of conv is
begin
    b <= std_logic_vector(a);
end architecture rtl;
"#;
    let hir = parse_vhdl_source(source, None).unwrap();
    let imp = &hir.implementations[0];

    // The concurrent assignment b <= std_logic_vector(a) should NOT produce Literal(0)
    assert!(!imp.assignments.is_empty(), "expected at least 1 assignment");
    use skalp_frontend::hir::{HirExpression, HirLiteral};
    let rhs = &imp.assignments[0].rhs;
    assert!(
        !matches!(rhs, HirExpression::Literal(HirLiteral::Integer(0))),
        "type conversion should not produce Literal(0), got {:?}",
        rhs
    );
}

// ========================================================================
// Step 2: Subtype declarations
// ========================================================================

#[test]
fn test_lower_subtype_decl() {
    let source = r#"
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity sub is
    port (
        clk : in std_logic
    );
end entity sub;

architecture rtl of sub is
    subtype byte_t is unsigned(7 downto 0);
    signal data : byte_t;
begin
    process(clk)
    begin
        data <= (others => '0');
    end process;
end architecture rtl;
"#;
    let hir = parse_vhdl_source(source, None).unwrap();
    let imp = &hir.implementations[0];

    // data signal should exist and be Nat(8) (unsigned 8-bit)
    assert!(!imp.signals.is_empty(), "expected at least 1 signal");
    use skalp_frontend::hir::HirType;
    assert!(
        matches!(imp.signals[0].signal_type, HirType::Nat(8)),
        "subtype byte_t should resolve to Nat(8), got {:?}",
        imp.signals[0].signal_type
    );
}

// ========================================================================
// Step 3: Alias declarations
// ========================================================================

#[test]
fn test_lower_alias() {
    let source = r#"
library ieee;
use ieee.std_logic_1164.all;

entity alias_test is
    port (
        data_in  : in  std_logic_vector(7 downto 0);
        data_out : out std_logic_vector(7 downto 0)
    );
end entity alias_test;

architecture rtl of alias_test is
    alias din is data_in;
begin
    data_out <= din;
end architecture rtl;
"#;
    let hir = parse_vhdl_source(source, None).unwrap();
    let imp = &hir.implementations[0];

    // Concurrent assignment data_out <= din should resolve din to data_in's port
    assert!(!imp.assignments.is_empty(), "expected at least 1 assignment");
    use skalp_frontend::hir::HirExpression;
    let rhs = &imp.assignments[0].rhs;
    assert!(
        matches!(rhs, HirExpression::Port(_)),
        "alias 'din' should resolve to a Port, got {:?}",
        rhs
    );
}

// ========================================================================
// Step 5: Function lowering
// ========================================================================

#[test]
fn test_lower_function_in_architecture() {
    let source = r#"
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity func_test is
    port (
        a : in  unsigned(7 downto 0);
        b : in  unsigned(7 downto 0);
        y : out unsigned(7 downto 0)
    );
end entity func_test;

architecture rtl of func_test is
    function max_val(x : unsigned(7 downto 0); y : unsigned(7 downto 0)) return unsigned is
    begin
        if x > y then
            return x;
        else
            return y;
        end if;
    end function max_val;
begin
    y <= max_val(a, b);
end architecture rtl;
"#;
    let hir = parse_vhdl_source(source, None).unwrap();
    let imp = &hir.implementations[0];

    assert_eq!(imp.functions.len(), 1, "expected 1 function, got {}", imp.functions.len());
    assert_eq!(imp.functions[0].name, "max_val");
    assert_eq!(imp.functions[0].params.len(), 2, "expected 2 params");
    assert!(imp.functions[0].return_type.is_some(), "expected return type");
    assert!(!imp.functions[0].body.is_empty(), "expected function body");
}

// ========================================================================
// Step 6: Package lowering
// ========================================================================

#[test]
fn test_lower_package_with_types() {
    let source = r#"
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package my_pkg is
    type state_t is (idle, run, done);
    constant MAX_COUNT : integer := 255;
end package my_pkg;

entity pkg_user is
    port (
        clk : in std_logic
    );
end entity pkg_user;

architecture rtl of pkg_user is
    signal state : state_t;
begin
    process(clk)
    begin
        null;
    end process;
end architecture rtl;
"#;
    let hir = parse_vhdl_source(source, None).unwrap();
    assert_eq!(hir.entities.len(), 1);
    assert_eq!(hir.entities[0].name, "PkgUser");
}

// ========================================================================
// Step 7: Generate statement lowering
// ========================================================================

#[test]
fn test_lower_for_generate() {
    let source = r#"
library ieee;
use ieee.std_logic_1164.all;

entity gen_test is
    port (
        a : in  std_logic_vector(3 downto 0);
        b : out std_logic_vector(3 downto 0)
    );
end entity gen_test;

architecture rtl of gen_test is
begin
    gen_inv: for i in 0 to 3 generate
        b(i) <= not a(i);
    end generate gen_inv;
end architecture rtl;
"#;
    let hir = parse_vhdl_source(source, None).unwrap();
    let imp = &hir.implementations[0];

    use skalp_frontend::hir::HirStatement;
    assert!(
        !imp.statements.is_empty(),
        "expected generate statements, got empty"
    );
    assert!(
        matches!(imp.statements[0], HirStatement::GenerateFor(_)),
        "expected GenerateFor, got {:?}",
        imp.statements[0]
    );
}

#[test]
fn test_lower_if_generate() {
    let source = r#"
library ieee;
use ieee.std_logic_1164.all;

entity ig_test is
    generic (
        USE_REG : boolean := true
    );
    port (
        a : in  std_logic;
        b : out std_logic
    );
end entity ig_test;

architecture rtl of ig_test is
begin
    gen_if: if USE_REG generate
        b <= a;
    end generate gen_if;
end architecture rtl;
"#;
    let hir = parse_vhdl_source(source, None).unwrap();
    let imp = &hir.implementations[0];

    use skalp_frontend::hir::HirStatement;
    assert!(
        !imp.statements.is_empty(),
        "expected generate statements, got empty"
    );
    assert!(
        matches!(imp.statements[0], HirStatement::GenerateIf(_)),
        "expected GenerateIf, got {:?}",
        imp.statements[0]
    );
}

// ========================================================================
// Step 8: While loop lowering
// ========================================================================

#[test]
fn test_lower_while_loop() {
    let source = r#"
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity wl is
    port (
        clk : in  std_logic;
        val : out unsigned(7 downto 0)
    );
end entity wl;

architecture rtl of wl is
    signal count : unsigned(7 downto 0);
begin
    process(clk)
        variable i : integer := 0;
    begin
        if rising_edge(clk) then
            while i < 10 loop
                i := i + 1;
            end loop;
        end if;
    end process;
end architecture rtl;
"#;
    let hir = parse_vhdl_source(source, None).unwrap();
    let imp = &hir.implementations[0];

    // Should have at least one event block with statements
    assert!(!imp.event_blocks.is_empty());
    // The while loop is lowered as a bounded for loop with a guard
    use skalp_frontend::hir::HirStatement;
    fn find_for_stmt(stmts: &[HirStatement]) -> bool {
        for s in stmts {
            match s {
                HirStatement::For(_) => return true,
                HirStatement::If(ref ifs) => {
                    if find_for_stmt(&ifs.then_statements) {
                        return true;
                    }
                    if let Some(ref else_s) = ifs.else_statements {
                        if find_for_stmt(else_s) {
                            return true;
                        }
                    }
                }
                _ => {}
            }
        }
        false
    }
    assert!(
        find_for_stmt(&imp.event_blocks[0].statements),
        "while loop should be lowered as a for loop"
    );
}

// ========================================================================
// Step 9: Block statement lowering
// ========================================================================

#[test]
fn test_lower_block_statement() {
    let source = r#"
library ieee;
use ieee.std_logic_1164.all;

entity blk_test is
    port (
        a : in  std_logic;
        b : out std_logic
    );
end entity blk_test;

architecture rtl of blk_test is
begin
    my_block: block is
        signal internal : std_logic;
    begin
        internal <= a;
        b <= internal;
    end block my_block;
end architecture rtl;
"#;
    let hir = parse_vhdl_source(source, None).unwrap();
    let imp = &hir.implementations[0];

    // Block signals should be flattened into the architecture
    assert!(
        !imp.signals.is_empty(),
        "block signal 'internal' should be in architecture signals"
    );
    // Block assignments should be flattened too
    assert!(
        imp.assignments.len() >= 2,
        "expected at least 2 assignments from block, got {}",
        imp.assignments.len()
    );
}

// ========================================================================
// Phase 2: Generic type parameters
// ========================================================================

#[test]
fn test_lower_generic_type_param() {
    let source = r#"
entity type_param_test is
    generic (
        type T
    );
    port (
        clk : in std_logic
    );
end entity type_param_test;
"#;
    let hir = parse_vhdl_source(source, None).unwrap();
    let entity = &hir.entities[0];
    assert_eq!(entity.generics.len(), 1);
    assert_eq!(entity.generics[0].name, "t");
    use skalp_frontend::hir::HirGenericType;
    assert!(
        matches!(entity.generics[0].param_type, HirGenericType::Type),
        "expected HirGenericType::Type, got {:?}",
        entity.generics[0].param_type
    );
}

// ========================================================================
// Phase 2: Qualified name resolution
// ========================================================================

#[test]
fn test_lower_qualified_name() {
    let source = r#"
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package my_pkg is
    constant MY_CONST : integer := 42;
end package my_pkg;

entity qual_test is
    port (
        clk : in  std_logic;
        val : out unsigned(7 downto 0)
    );
end entity qual_test;

architecture rtl of qual_test is
begin
    val <= to_unsigned(my_pkg.MY_CONST, 8);
end architecture rtl;
"#;
    let hir = parse_vhdl_source(source, None).unwrap();
    let imp = &hir.implementations[0];

    // The assignment should resolve my_pkg.MY_CONST to a constant reference
    assert!(!imp.assignments.is_empty(), "expected at least 1 assignment");
}

// ========================================================================
// Phase 2: Package instantiation with generic substitution
// ========================================================================

#[test]
fn test_lower_package_instantiation_substitution() {
    let source = r#"
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package generic_pkg is
    generic (
        type T
    );
    constant PKG_CONST : T;
end package generic_pkg;

package my_inst is new generic_pkg generic map (T => unsigned(7 downto 0));

entity inst_test is
    port (
        clk : in std_logic
    );
end entity inst_test;
"#;
    let hir = parse_vhdl_source(source, None).unwrap();
    // The package should parse and lower without errors
    assert_eq!(hir.entities.len(), 1);
    assert_eq!(hir.entities[0].name, "InstTest");
}

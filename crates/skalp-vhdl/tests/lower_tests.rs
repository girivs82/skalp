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

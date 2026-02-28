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

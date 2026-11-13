//! Board declarations and binding ports to their pins.

use skalp_frontend::hir::{BoardPinKind, PinLocation, Termination};
use skalp_frontend::parse_and_build_compilation_context_from_source;

const BOARD: &str = r#"
board DemoBoard {
    device: "LFCPNX-100"
    out led: bit[4] @ { pins: ["A1", "A2", "A3", "A4"], io_standard: "LVCMOS33" }
    in button: bit[2] @ { pins: ["B1", "B2"], io_standard: "LVCMOS33", pull: up }
    in clk_25: clock @ { pin: "C1", io_standard: "LVCMOS33" }
    inout gpio: bit[2] @ { pins: ["D1", "D2"], io_standard: "LVCMOS18H" }
    reserved dram: bit[2] @ { pins: ["E1", "E2"], connected: "DRAM address" }
}
"#;

fn build(design: &str) -> anyhow::Result<skalp_frontend::CompilationContext> {
    let dir = std::env::temp_dir().join(format!("skalp_board_{}", std::process::id()));
    std::fs::create_dir_all(&dir).unwrap();
    let path = dir.join("design.sk");
    parse_and_build_compilation_context_from_source(&path, format!("{BOARD}\n{design}"))
}

fn error_of(design: &str) -> String {
    match build(design) {
        Ok(_) => panic!("the design should be refused"),
        Err(e) => format!("{e:#}"),
    }
}

fn port_pins(ctx: &skalp_frontend::CompilationContext, entity: &str, port: &str) -> Vec<String> {
    let entity = ctx
        .main_hir
        .entities
        .iter()
        .find(|e| e.name == entity)
        .unwrap();
    let port = entity.ports.iter().find(|p| p.name == port).unwrap();
    match port
        .physical_constraints
        .as_ref()
        .unwrap()
        .pin_location
        .as_ref()
        .unwrap()
    {
        PinLocation::Single(pin) => vec![pin.clone()],
        PinLocation::Multiple(pins) => pins.clone(),
        other => panic!("{other:?}"),
    }
}

#[test]
fn a_board_declares_its_pins() {
    let ctx = build("").unwrap();
    let board = &ctx.main_hir.boards[0];
    assert_eq!(board.name, "DemoBoard");
    assert_eq!(board.device.as_deref(), Some("LFCPNX-100"));
    let kinds: Vec<_> = board
        .pins
        .iter()
        .map(|p| (p.name.as_str(), p.kind, p.width))
        .collect();
    assert_eq!(
        kinds,
        vec![
            ("led", BoardPinKind::Output, 4),
            ("button", BoardPinKind::Input, 2),
            ("clk_25", BoardPinKind::Input, 1),
            ("gpio", BoardPinKind::Inout, 2),
            ("dram", BoardPinKind::Reserved, 2),
        ]
    );
    assert_eq!(board.pins[4].connected.as_deref(), Some("DRAM address"));
}

#[test]
fn bound_ports_take_the_boards_pins_and_settings() {
    let ctx = build(
        r#"
entity Top {
    in clk: clock @ DemoBoard.clk_25
    in buttons: bit[2] @ DemoBoard.button
    out lights: bit[2] @ DemoBoard.led[3:2]
    out first: bit @ DemoBoard.led[0]
}
impl Top {
    lights = buttons
    first = buttons[0]
}
"#,
    )
    .unwrap();
    assert_eq!(port_pins(&ctx, "Top", "clk"), ["C1"]);
    assert_eq!(port_pins(&ctx, "Top", "lights"), ["A3", "A4"]);
    assert_eq!(port_pins(&ctx, "Top", "first"), ["A1"]);
    let top = &ctx
        .main_hir
        .entities
        .iter()
        .find(|e| e.name == "Top")
        .unwrap();
    let buttons = top.ports.iter().find(|p| p.name == "buttons").unwrap();
    let c = buttons.physical_constraints.as_ref().unwrap();
    assert_eq!(c.io_standard.as_deref(), Some("LVCMOS33"));
    assert!(matches!(c.termination, Some(Termination::PullUp)));
}

#[test]
fn an_output_on_an_input_pin_is_refused() {
    let error = error_of("entity Top { out o: bit[2] @ DemoBoard.button }\nimpl Top { o = 0 }");
    assert!(error.contains("Top.o"), "{error}");
    assert!(error.contains("an input to the FPGA"), "{error}");
}

#[test]
fn an_input_on_an_output_pin_is_refused() {
    let error = error_of(
        "entity Top { in i: bit[4] @ DemoBoard.led\n out o: bit[2] @ DemoBoard.gpio }\nimpl Top { o = i[1:0] }",
    );
    assert!(error.contains("Top.i"), "{error}");
}

#[test]
fn a_reserved_pin_is_refused() {
    let error = error_of("entity Top { in i: bit[2] @ DemoBoard.dram }\nimpl Top { }");
    assert!(error.contains("DRAM address"), "{error}");
}

#[test]
fn a_width_mismatch_is_refused() {
    let error = error_of("entity Top { out o: bit[3] @ DemoBoard.led }\nimpl Top { o = 0 }");
    assert!(error.contains("3 bit(s)"), "{error}");
}

#[test]
fn raw_pins_of_a_design_on_a_board_are_checked_too() {
    let error = error_of(
        r#"
entity Top {
    in clk: clock @ DemoBoard.clk_25
    out bad: bit @ { pin: "B1", io_standard: "LVCMOS33" }
    out dram: bit @ { pin: "E2", io_standard: "LVCMOS33" }
    out volts: bit @ { pin: "D1", io_standard: "LVCMOS33" }
}
impl Top { bad = 0
 dram = 0
 volts = 0 }
"#,
    );
    assert!(error.contains("Top.bad: pin B1"), "{error}");
    assert!(error.contains("Top.dram: pin E2"), "{error}");
    assert!(error.contains("runs at LVCMOS18H"), "{error}");
}

#[test]
fn a_pin_used_twice_is_refused() {
    let error = error_of(
        "entity Top { out a: bit @ DemoBoard.led[0]\n out b: bit @ DemoBoard.led[0] }\nimpl Top { a = 0\n b = 0 }",
    );
    assert!(error.contains("already used"), "{error}");
}

#[test]
fn an_unknown_board_or_pin_is_named() {
    let error = error_of("entity Top { out o: bit @ Nowhere.led }\nimpl Top { o = 0 }");
    assert!(error.contains("no board named 'Nowhere'"), "{error}");
    let error = error_of("entity Top { out o: bit @ DemoBoard.lamp }\nimpl Top { o = 0 }");
    assert!(error.contains("no pin 'lamp'"), "{error}");
}

#[test]
fn a_design_without_a_board_is_not_checked() {
    build("entity Top { out o: bit @ { pin: \"B1\" } }\nimpl Top { o = 0 }").unwrap();
}

#[test]
fn the_versa_reference_board_binds() {
    let versa = std::fs::read_to_string(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/../skalp-stdlib/skalp/boards.sk"
    ))
    .unwrap();
    let design = r#"
entity Blink {
    in clk: clock @ CertusProVersa.clk_125
    in sw: bit[4] @ CertusProVersa.switch[3:0]
    out led: bit[8] @ CertusProVersa.led
}
impl Blink {
    led = {sw, sw}
}
"#;
    let dir = std::env::temp_dir().join(format!("skalp_versa_{}", std::process::id()));
    std::fs::create_dir_all(&dir).unwrap();
    let ctx = parse_and_build_compilation_context_from_source(
        &dir.join("blink.sk"),
        format!("{versa}\n{design}"),
    )
    .unwrap();
    let board = ctx
        .main_hir
        .boards
        .iter()
        .find(|b| b.name == "CertusProVersa")
        .unwrap();
    assert!(board.pins.iter().any(|p| p.kind == BoardPinKind::Reserved));
    assert_eq!(port_pins(&ctx, "Blink", "clk"), ["P24"]);
    assert_eq!(
        port_pins(&ctx, "Blink", "sw"),
        ["AA23", "AB22", "AC22", "AA22"]
    );
    assert_eq!(port_pins(&ctx, "Blink", "led")[0], "R5");

    // The PROGRAMN pin restarts configuration: no design may use it
    let error = match parse_and_build_compilation_context_from_source(
        &dir.join("bad.sk"),
        format!("{versa}\nentity Bad {{ in p: bit @ CertusProVersa.programn }}\nimpl Bad {{ }}"),
    ) {
        Ok(_) => panic!("PROGRAMN should be refused"),
        Err(e) => format!("{e:#}"),
    };
    assert!(error.contains("PROGRAMN"), "{error}");
}

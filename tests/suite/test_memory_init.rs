//! Memories that power up holding contents, given by `#[memory(init =
//! "file.hex")]` or an array literal on the signal. The fixture has a block
//! RAM ROM loaded from a hex file, a RAM preloaded by an array literal and
//! then written, and a small read-only table. Each is read back in behavioural
//! simulation and at gate level on every target library, and the contents
//! reach the Nexus block RAM's configuration.

use skalp_lir::{CellFunction, GateNetlist};
use skalp_testing::Testbench;

const FIXTURE: &str = "memory_init.sk";

fn fixture_path(name: &str) -> String {
    format!("{}/tests/fixtures/{}", env!("CARGO_MANIFEST_DIR"), name)
}

/// The ROM's words, as `memory_init_rom.hex` lists them: 200 words, the rest zero.
fn rom_word(address: u64) -> u64 {
    if address < 200 {
        ((address * 0x9E37) ^ 0x5A5A) & 0xFFFF
    } else {
        0
    }
}

/// The RAM's words, as the array literal lists them.
fn ram_word(address: u64) -> u64 {
    (address * 0x111 + 0x0A5) & 0xFFF
}

const TABLE: [u64; 4] = [0x11, 0x22, 0x44, 0x88];

fn synthesize(library: &str) -> GateNetlist {
    let path = std::path::PathBuf::from(fixture_path(FIXTURE));
    let ctx = skalp_frontend::parse_and_build_compilation_context(&path).unwrap();
    let mir = skalp_mir::MirCompiler::new()
        .with_optimization_level(skalp_mir::OptimizationLevel::None)
        .compile_to_mir_with_modules(&ctx.main_hir, &ctx.module_hirs)
        .unwrap();
    let (hier, _) = skalp_lir::lower_mir_hierarchical_for_optimize_first(&mir);
    let library = skalp_lir::get_stdlib_library(library).unwrap();
    skalp_lir::synthesize_hierarchical(&hier, &library, skalp_lir::synth::SynthPreset::Balanced)
        .flatten()
}

async fn check_contents(mut tb: Testbench, what: &str) {
    tb.set("we", 0u8).set("wa", 0u8).set("wd", 0u16);
    tb.set("addr", 0u8).set("ta", 0u8);
    for address in 0..256u64 {
        tb.set("addr", address as u8);
        tb.set("wa", (address % 16) as u8);
        tb.set("ta", (address % 4) as u8);
        tb.clock(1).await;
        assert_eq!(
            tb.get_u64("rom_q").await,
            rom_word(address),
            "{what}: rom[{address}]"
        );
        assert_eq!(
            tb.get_u64("ram_q").await,
            ram_word(address % 16),
            "{what}: ram[{}]",
            address % 16
        );
        assert_eq!(
            tb.get_u64("table_q").await,
            TABLE[(address % 4) as usize],
            "{what}: table[{}]",
            address % 4
        );
    }
    // The preloaded RAM still takes writes
    tb.set("we", 1u8).set("wa", 7u8).set("wd", 0xABCu16);
    tb.clock(1).await;
    tb.set("we", 0u8);
    tb.clock(1).await;
    assert_eq!(tb.get_u64("ram_q").await, 0xABC, "{what}: ram[7] written");
    tb.set("wa", 8u8);
    tb.clock(1).await;
    assert_eq!(
        tb.get_u64("ram_q").await,
        ram_word(8),
        "{what}: ram[8] kept"
    );
}

#[tokio::test]
async fn preloaded_memories_behavioral() {
    let tb = Testbench::with_top_module(&fixture_path(FIXTURE), "MemInit")
        .await
        .unwrap();
    check_contents(tb, "behavioral").await;
}

#[tokio::test]
async fn preloaded_memories_gate_level() {
    for library in ["nexus", "ecp5", "ice40", "generic_asic"] {
        let tb =
            Testbench::gate_level_with_top_and_library(&fixture_path(FIXTURE), "MemInit", library)
                .await
                .unwrap();
        check_contents(tb, library).await;
    }
}

#[test]
fn block_ram_cells_carry_the_rom_contents() {
    let net = synthesize("nexus");
    let rams: Vec<_> = net
        .cells
        .iter()
        .filter(|c| c.function == Some(CellFunction::Ram))
        .collect();
    assert_eq!(rams.len(), 1, "the 256 x 16 ROM takes one block RAM");
    let words = rams[0].memory_init().expect("the block RAM has contents");
    assert_eq!(words.len(), 200, "trailing zero words are left out");
    for (address, &word) in words.iter().enumerate() {
        assert_eq!(word, rom_word(address as u64), "word {address}");
    }
}

#[test]
fn contents_that_do_not_fit_are_errors() {
    let dir = std::env::temp_dir().join(format!("skalp_meminit_err_{}", std::process::id()));
    std::fs::create_dir_all(&dir).unwrap();
    let check = |source: &str, expected: &str| {
        let path = dir.join("design.sk");
        std::fs::write(&path, source).unwrap();
        let result = skalp_frontend::parse_and_build_compilation_context(&path).and_then(|ctx| {
            skalp_mir::MirCompiler::new()
                .compile_to_mir_with_modules(&ctx.main_hir, &ctx.module_hirs)
                .map(|_| ())
                .map_err(|e| anyhow::anyhow!(e))
        });
        let error = format!("{:?}", result.expect_err(expected));
        assert!(
            error.contains(expected),
            "expected `{expected}` in: {error}"
        );
    };
    let design = |attr: &str, init: &str| {
        format!(
            "entity M {{\n in clk: clock\n in a: bit[2]\n out q: bit[4]\n}}\n\
             impl M {{\n #[memory({attr})]\n signal m: bit[4][4]{init}\n \
             q = m[a]\n}}\n"
        )
    };
    std::fs::write(dir.join("big.hex"), "1 2 3 4 5\n").unwrap();
    std::fs::write(dir.join("wide.hex"), "1 2 1f\n").unwrap();
    check(
        &design("depth = 4, init = \"missing.hex\"", ""),
        "missing.hex",
    );
    check(&design("depth = 4, init = \"big.hex\"", ""), "5 words");
    check(
        &design("depth = 4, init = \"wide.hex\"", ""),
        "does not fit in 4 bits",
    );
    check(
        &design("depth = 4", " = [1, 2, 3, 4, 5]"),
        "5 initial words",
    );
    check(
        &design("depth = 4", " = [1, 0x10]"),
        "does not fit in 4 bits",
    );
    check(
        &design("depth = 4, init = \"wide.hex\"", " = [1]"),
        "both an init file and an array literal",
    );
    std::fs::remove_dir_all(&dir).unwrap();
}

#[test]
fn systemverilog_declares_the_contents() {
    let path = std::path::PathBuf::from(fixture_path(FIXTURE));
    let ctx = skalp_frontend::parse_and_build_compilation_context(&path).unwrap();
    let mir = skalp_mir::MirCompiler::new()
        .compile_to_mir_with_modules(&ctx.main_hir, &ctx.module_hirs)
        .unwrap();
    let sv = skalp_codegen::generate_systemverilog_from_mir(&mir).unwrap();
    // The ROM gives 200 words, so the other 56 start at zero
    assert!(
        sv.contains("for (int i = 200; i < 256; i++) rom[i] = '0;"),
        "{sv}"
    );
    assert!(
        sv.contains(&format!("rom[1] = 16'h{:x};", rom_word(1))),
        "{sv}"
    );
    assert!(
        sv.contains(&format!("ram[15] = 12'h{:x};", ram_word(15))),
        "{sv}"
    );
    assert!(sv.contains("tbl[3] = 8'h88;"), "{sv}");
}

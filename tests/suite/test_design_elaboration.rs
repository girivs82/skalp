//! Every flow must elaborate the same design: the same top-level module, the
//! same source text, and one driver per register.

use skalp_frontend::{
    parse_and_build_compilation_context, parse_and_build_compilation_context_from_source,
};
use skalp_mir::{Mir, MirCompiler, OptimizationLevel};

fn compile(context: &skalp_frontend::CompilationContext) -> Mir {
    MirCompiler::new()
        .with_optimization_level(OptimizationLevel::None)
        .compile_to_mir_with_modules(&context.main_hir, &context.module_hirs)
        .unwrap()
}

fn write(test: &str, source: &str) -> std::path::PathBuf {
    let dir = std::env::temp_dir().join(format!("skalp_elab_{test}_{}", std::process::id()));
    std::fs::create_dir_all(&dir).unwrap();
    let path = dir.join("design.sk");
    std::fs::write(&path, source).unwrap();
    path
}

/// The real top instantiates a generic core, which is defined after it and
/// holds more logic than it does.
const GENERIC_CORE_AFTER_TOP: &str = r#"
entity Top {
    in clk: clock
    out o: bit[8]
}
impl Top {
    inst core = Core<8> { clk: clk }
    o = core.o
}

entity Core<const N: nat = 4> {
    in clk: clock
    out o: bit[N]
}
impl Core {
    signal a: bit[N] = 0
    signal b: bit[N] = 0
    signal c: bit[N] = 0
    on(clk.rise) {
        a = a + 1
        b = b + a
        c = c ^ b
    }
    o = c
}
"#;

#[test]
fn every_flow_chooses_the_same_top_module() {
    let path = write("top", GENERIC_CORE_AFTER_TOP);
    let mir = compile(&parse_and_build_compilation_context(&path).unwrap());
    assert_eq!(mir.top_module().unwrap().name, "Top");
    assert_eq!(skalp_lir::lower_mir_hierarchical(&mir).top_module, "Top");
    assert_eq!(
        skalp_lir::lower_mir_hierarchical_for_optimize_first(&mir)
            .0
            .top_module,
        "Top"
    );
}

#[test]
fn an_unused_generic_entity_is_never_the_top() {
    // Only the default-parameter copy of the generic core exists in the
    // design, and it holds more logic than the real top.
    let unused = GENERIC_CORE_AFTER_TOP.replace(
        "    inst core = Core<8> { clk: clk }\n    o = core.o\n",
        "    o = 0\n",
    );
    let path = write("unused", &unused);
    let mir = compile(&parse_and_build_compilation_context(&path).unwrap());
    assert_eq!(mir.top_module().unwrap().name, "Top");
    assert_eq!(skalp_lir::lower_mir_hierarchical(&mir).top_module, "Top");
    assert_eq!(
        skalp_lir::lower_mir_hierarchical_for_optimize_first(&mir)
            .0
            .top_module,
        "Top"
    );
}

#[test]
fn a_design_built_from_edited_source_keeps_its_edits() {
    let original = r#"
entity Inner {
    in clk: clock
    out o: bit
}
impl Inner {
    signal r: bit = 0
    on(clk.rise) {
        r = !r
    }
    o = r
}
entity Outer {
    in clk: clock
    out o: bit
}
impl Outer {
    o = 0
}
"#;
    let path = write("edited", original);
    let edited = original.replace(
        "    o = 0\n",
        "    inst inner = Inner { clk: clk }\n    o = inner.o\n",
    );
    let context = parse_and_build_compilation_context_from_source(&path, edited).unwrap();
    let mir = compile(&context);
    let outer = mir.modules.iter().find(|m| m.name == "Outer").unwrap();
    assert_eq!(
        outer.instances.len(),
        1,
        "the instance added in the edited text, not the file on disk, is built"
    );
}

#[test]
fn a_register_assigned_before_a_match_has_one_driver() {
    let source = r#"
entity Reader {
    in clk: clock
    in addr: bit[4]
    in data: bit[8]
    out o: bit[8]
}
impl Reader {
    signal mem: [bit[8]; 16]
    signal read_data: bit[8] = 0
    signal state: nat[2] = 0
    on(clk.rise) {
        read_data = mem[addr]
        match state {
            0 => {
                mem[addr] = data
                state = 1
            }
            1 => {
                state = 0
            }
            _ => {
                state = 0
            }
        }
    }
    o = read_data
}
"#;
    let path = write("driver", source);
    let mir = compile(&parse_and_build_compilation_context(&path).unwrap());
    let module = mir.top_module().unwrap();
    let lir = skalp_lir::lower_mir_module_to_lir(module).lir;
    let mut drivers = std::collections::HashMap::new();
    for node in &lir.nodes {
        *drivers.entry(node.output).or_insert(0) += 1;
    }
    let several: Vec<&str> = drivers
        .iter()
        .filter(|(_, &count)| count > 1)
        .map(|(signal, _)| lir.signals[signal.0 as usize].name.as_str())
        .collect();
    assert!(
        several.is_empty(),
        "signals with several drivers: {several:?}"
    );
}

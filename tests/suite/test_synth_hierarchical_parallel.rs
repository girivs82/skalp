//! Hierarchical synthesis synthesizes each distinct module once, in
//! parallel, and can take modules from an on-disk cache. Each instance's
//! netlist must be exactly what synthesizing its module on its own gives,
//! and a build from the cache must equal a fresh one.

use skalp_frontend::parse_and_build_hir;
use skalp_lir::synth::SynthPreset;
use skalp_lir::synth_cache::SynthCache;
use skalp_lir::{
    get_stdlib_library, lower_mir_hierarchical, synthesize, synthesize_hierarchical,
    synthesize_hierarchical_cached,
};
use skalp_mir::MirCompiler;

/// Three copies of one counter, one of another, and a parent combining them
const SOURCE: &str = r#"
entity Count {
    in clk: clock
    in en: bit
    out q: bit[8]
}
impl Count {
    signal c: bit[8] = 0
    on(clk.rise) {
        if (en == 1) {
            c = c + 8'h01
        }
    }
    q = c
}

entity Shift {
    in clk: clock
    in d: bit
    out q: bit[8]
}
impl Shift {
    signal s: bit[8] = 0
    on(clk.rise) {
        s = {s[6:0], d}
    }
    q = s
}

entity Top {
    in clk: clock
    in en: bit
    out y: bit[8]
}
impl Top {
    inst a = Count { clk: clk, en: en }
    inst b = Count { clk: clk, en: en }
    inst c = Count { clk: clk, en: en }
    inst s = Shift { clk: clk, d: en }
    y = a.q ^ b.q ^ c.q ^ s.q
}
"#;

fn hier_lir() -> skalp_lir::mir_to_lir::HierarchicalMirToLirResult {
    let hir = parse_and_build_hir(SOURCE).expect("parse");
    let mir = MirCompiler::new().compile(&hir).expect("MIR");
    lower_mir_hierarchical(&mir)
}

fn json<T: serde::Serialize>(v: &T) -> String {
    serde_json::to_string(v).unwrap()
}

#[test]
fn each_instance_matches_its_module_synthesized_alone() {
    let hier = hier_lir();
    let library = get_stdlib_library("generic_asic").unwrap();
    let preset = SynthPreset::Balanced;
    let result = synthesize_hierarchical(&hier, &library, preset);
    let copies = hier
        .instances
        .values()
        .filter(|i| i.module_name == "Count")
        .count();
    assert_eq!(copies, 3, "three copies of the counter");
    // Leaf instances: a parent's own ports grow by the signals its children
    // use (promoted before synthesis), so only leaves synthesize their LIR as
    // it stands
    for (path, inst) in hier.instances.iter().filter(|(_, i)| i.children.is_empty()) {
        let alone = synthesize(&inst.lir_result.lir, &library, preset).netlist;
        let got = &result.instances[path].netlist;
        assert_eq!(
            json(got),
            json(&alone),
            "instance {path} differs from its module synthesized alone"
        );
    }
}

#[test]
fn cached_build_equals_a_fresh_one() {
    let hier = hier_lir();
    let library = get_stdlib_library("generic_asic").unwrap();
    let preset = SynthPreset::Balanced;
    let dir = tempfile::tempdir().unwrap();
    let cache = SynthCache::with_tool_hash(dir.path(), blake3::hash(b"this test")).unwrap();

    let fresh = synthesize_hierarchical(&hier, &library, preset);
    let first = synthesize_hierarchical_cached(&hier, &library, preset, Some(&cache));
    let entries = std::fs::read_dir(dir.path()).unwrap().count();
    // one entry per distinct module: the three counters share one
    let distinct: std::collections::HashSet<&str> = hier
        .instances
        .values()
        .map(|i| i.module_name.as_str())
        .collect();
    assert_eq!(entries, distinct.len());
    let second = synthesize_hierarchical_cached(&hier, &library, preset, Some(&cache));
    assert_eq!(
        std::fs::read_dir(dir.path()).unwrap().count(),
        entries,
        "a cached build adds nothing"
    );
    assert_eq!(json(&first), json(&fresh));
    assert_eq!(json(&second), json(&fresh));
}

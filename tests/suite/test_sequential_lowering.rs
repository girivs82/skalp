//! Sequential blocks must mean the same thing in behavioral simulation and
//! in synthesized gates: registers start at their declared initial values, a
//! match arm takes the last assignment that applies to each target (through
//! nested if/else-if chains), an arm that does not assign a target leaves it
//! at its value before the match, and the default arm only applies when no
//! listed arm matches.

use skalp_testing::testbench::Testbench;

/// Write `source` to a fresh file for one test and return its path.
fn design(test: &str, source: &str) -> String {
    let dir = std::env::temp_dir().join(format!("skalp_seq_{test}_{}", std::process::id()));
    std::fs::create_dir_all(&dir).unwrap();
    let path = dir.join("design.sk");
    std::fs::write(&path, source).unwrap();
    path.to_string_lossy().into_owned()
}

/// Run `cycles` clocks and record the named outputs after each one, in both
/// behavioral and gate-level simulation, which must agree.
async fn run_both(
    test: &str,
    source: &str,
    top: &str,
    outputs: &[&str],
    cycles: usize,
) -> Vec<Vec<u64>> {
    let path = design(test, source);
    let mut traces = Vec::new();
    for gate_level in [false, true] {
        let mut tb = if gate_level {
            Testbench::gate_level_with_top_and_library(&path, top, "nexus")
                .await
                .unwrap()
        } else {
            Testbench::with_top_module(&path, top).await.unwrap()
        };
        let mut trace = Vec::new();
        for _ in 0..cycles {
            tb.clock(1).await;
            let mut row = Vec::new();
            for name in outputs {
                row.push(tb.get_u64(name).await);
            }
            trace.push(row);
        }
        traces.push(trace);
    }
    assert_eq!(
        traces[0], traces[1],
        "behavioral and gate-level simulation disagree on {outputs:?}"
    );
    traces.swap_remove(0)
}

const INITIAL_VALUES: &str = r#"
entity Child {
    in clk: clock
    out o: bit[10]
}
impl Child {
    signal frame: bit[10] = 1023
    on(clk.rise) {
        frame = {1'b0, frame[9:1]}
    }
    o = frame
}
entity Top {
    in clk: clock
    out o: bit[10]
    out t: bit[10]
}
impl Top {
    signal value: bit[10] = 600
    on(clk.rise) {
        value = value + 1
    }
    t = value
    inst c = Child { clk: clk }
    o = c.o
}
"#;

#[tokio::test]
async fn registers_start_at_their_initial_values() {
    let trace = run_both("init", INITIAL_VALUES, "Top", &["t", "o"], 3).await;
    assert_eq!(
        trace,
        vec![vec![601, 511], vec![602, 255], vec![603, 127]],
        "a register starts at its declared value, in the top and in a child"
    );
}

/// A chain of if/else-if inside a match arm, with a default arm that
/// assigns only `state`.
const CHAIN_IN_ARM: &str = r#"
entity C {
    in clk: clock
    out s: bit[3]
    out f: bit[8]
}
impl C {
    signal state: nat[3] = 0
    signal h: nat[2] = 0
    signal frame: bit[8] = 0
    on(clk.rise) {
        match state {
            0 => {
                state = 1
            }
            1 => {
                if h == 0 {
                    frame = 8'h11
                } else if h == 1 {
                    frame = 8'h22
                } else {
                    frame = 8'h33
                }
                h = h + 1
                if h == 2 {
                    state = 2
                }
            }
            _ => {
                state = 2
            }
        }
    }
    s = state
    f = frame
}
"#;

#[tokio::test]
async fn else_if_chain_in_a_match_arm_and_default_arm() {
    let trace = run_both("chain", CHAIN_IN_ARM, "C", &["s", "f"], 6).await;
    assert_eq!(
        trace,
        vec![
            vec![1, 0x00],
            vec![1, 0x11],
            vec![1, 0x22],
            vec![2, 0x33],
            vec![2, 0x33],
            vec![2, 0x33],
        ]
    );
}

/// `frame` is assigned by an if before the match and again inside an arm;
/// the arm's assignment wins when it applies. The default arm assigns
/// `frame`, which must not leak into arms that leave it alone.
const REPEATED_TARGET: &str = r#"
entity Shifter {
    in clk: clock
    out s: bit[3]
    out f: bit[8]
}
impl Shifter {
    signal state: nat[3] = 0
    signal h: nat[2] = 0
    signal frame: bit[8] = 0
    signal left: nat[3] = 0
    on(clk.rise) {
        if left != 0 {
            frame = {1'b0, frame[7:1]}
            left = left - 1
        }
        match state {
            0 => {
                state = 1
            }
            1 => {
                if left == 0 {
                    if h == 0 {
                        frame = 8'h81
                    } else {
                        frame = 8'h82
                    }
                    left = 2
                    h = h + 1
                    if h == 1 {
                        state = 2
                    }
                }
            }
            2 => {
                state = 2
            }
            _ => {
                frame = 8'hFF
                state = 0
            }
        }
    }
    s = state
    f = frame
}
entity Wrapper {
    in clk: clock
    out s: bit[3]
    out f: bit[8]
}
impl Wrapper {
    inst inner = Shifter { clk: clk }
    s = inner.s
    f = inner.f
}
"#;

#[tokio::test]
async fn repeated_target_and_default_arm_at_top_level() {
    let trace = run_both("repeat_top", REPEATED_TARGET, "Shifter", &["s", "f"], 8).await;
    let expected = vec![
        vec![1, 0x00],
        vec![1, 0x81],
        vec![1, 0x40],
        vec![1, 0x20],
        vec![2, 0x82],
        vec![2, 0x41],
        vec![2, 0x20],
        vec![2, 0x20],
    ];
    assert_eq!(trace, expected);
}

#[tokio::test]
async fn repeated_target_and_default_arm_in_a_child_instance() {
    let trace = run_both("repeat_child", REPEATED_TARGET, "Wrapper", &["s", "f"], 8).await;
    let expected = vec![
        vec![1, 0x00],
        vec![1, 0x81],
        vec![1, 0x40],
        vec![1, 0x20],
        vec![2, 0x82],
        vec![2, 0x41],
        vec![2, 0x20],
        vec![2, 0x20],
    ];
    assert_eq!(trace, expected);
}

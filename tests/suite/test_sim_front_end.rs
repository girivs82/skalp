//! `skalp sim` drives every simulator with the same stimulus options: the
//! behavioural and gate-level cycle simulators from a source file, and the
//! timed simulator from a place-and-route build directory. All three must
//! agree on what a counter does.

use std::path::{Path, PathBuf};
use std::process::Command;
use tempfile::TempDir;

const COUNTER: &str = r#"entity Counter {
    in clk: clock
    in rst: reset
    in en: bit
    out count: bit[8]
}
impl Counter {
    signal c: bit[8] = 0
    on(clk.rise) {
        if rst {
            c = 0
        } else if en {
            c = c + 1
        }
    }
    count = c
}
"#;

fn skalp_binary() -> PathBuf {
    PathBuf::from(env!("CARGO_BIN_EXE_skalp"))
}

fn skalp(dir: &Path, args: &[&str]) -> String {
    let output = Command::new(skalp_binary())
        .args(args)
        .current_dir(dir)
        .output()
        .expect("skalp runs");
    let stdout = String::from_utf8_lossy(&output.stdout).into_owned();
    assert!(
        output.status.success(),
        "skalp {args:?} failed:\n{stdout}\n{}",
        String::from_utf8_lossy(&output.stderr)
    );
    stdout
}

/// The values a watched signal took, in order, from the report's change
/// lines (`<time> ns  <value>`) that follow its `<name> (N changes):` line
fn watched_values(report: &str, name: &str) -> Vec<String> {
    let mut lines = report
        .lines()
        .map(str::trim)
        .skip_while(|line| !line.starts_with(&format!("{name} (")));
    lines
        .next()
        .unwrap_or_else(|| panic!("no {name} in:\n{report}"));
    lines
        .map_while(|line| line.split_once(" ns  ").map(|(_, v)| v.to_string()))
        .collect()
}

fn counter_dir() -> TempDir {
    let dir = TempDir::new().unwrap();
    std::fs::write(dir.path().join("counter.sk"), COUNTER).unwrap();
    dir
}

#[test]
fn cycle_simulators_take_the_same_stimulus() {
    let dir = counter_dir();
    for level in [None, Some("--gate-level")] {
        let mut args = vec!["sim", "counter.sk"];
        args.extend(level);
        args.extend([
            "--set", "rst=0", "--set", "en=1", "--watch", "count", "--until", "count=3", "-d", "10",
        ]);
        let report = skalp(dir.path(), &args);
        assert!(
            report.contains("Stopped: count=3 in cycle 3"),
            "{level:?}:\n{report}"
        );
        assert_eq!(
            watched_values(&report, "count"),
            ["0x1", "0x2", "0x3"],
            "{level:?}"
        );
    }
}

#[test]
fn timed_simulation_of_a_build_takes_the_same_stimulus() {
    let dir = counter_dir();
    skalp(
        dir.path(),
        &["synth", "counter.sk", "-d", "ice40-hx8k", "-f", "-o", "out"],
    );
    let report = skalp(
        dir.path(),
        &[
            "sim", "out", "--clock", "clk=20", "--set", "rst=0", "--set", "en=1", "--watch",
            "count", "--until", "count=5", "-d", "2us",
        ],
    );
    assert!(report.contains("Stopped: count=5"), "{report}");
    assert!(report.contains("no setup or hold violations"), "{report}");
    assert_eq!(
        watched_values(&report, "count"),
        ["0x1", "0x2", "0x3", "0x4", "0x5"]
    );
}

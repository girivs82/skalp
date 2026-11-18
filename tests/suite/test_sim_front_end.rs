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

/// Sends "Hi\r\n" over and over at a quarter of the clock rate (8N1)
const SENDER: &str = r#"entity Sender {
    in clk: clock
    out tx: bit
}
impl Sender {
    signal baud: bit[2] = 0
    signal bitn: bit[4] = 0
    signal chn: bit[2] = 0
    signal txr: bit = 1
    signal ch: bit[8]
    ch = if chn == 0 { 8'h48 } else if chn == 1 { 8'h69 } else if chn == 2 { 8'h0D } else { 8'h0A }
    signal frame: bit[10]
    frame = {1'b1, ch, 1'b0}
    on(clk.rise) {
        baud = baud + 1
        if baud == 3 {
            txr = frame[bitn]
            if bitn == 9 {
                bitn = 0
                chn = chn + 1
            } else {
                bitn = bitn + 1
            }
        }
    }
    tx = txr
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

/// The text a UART line sends, decoded the same way by every simulator
#[test]
fn simulators_decode_a_uart_line() {
    let dir = TempDir::new().unwrap();
    std::fs::write(dir.path().join("sender.sk"), SENDER).unwrap();
    // 20 ns clock, 4 clocks a bit: 12.5 Mbaud, a line (4 characters) in 3.2 us
    let uart = ["--uart", "tx=12500000", "--uart-lines", "2"];
    let lines = |report: &str| -> Vec<String> {
        report
            .lines()
            .filter_map(|line| line.trim().split_once(" ns  ").map(|(_, t)| t.to_string()))
            .collect()
    };
    for level in [None, Some("--gate-level")] {
        let mut args = vec!["sim", "sender.sk", "--clock", "clk=20ns", "-d", "1000"];
        args.extend(level);
        args.extend(uart);
        let report = skalp(dir.path(), &args);
        assert!(report.contains("Stopped: 2 UART lines"), "{report}");
        assert_eq!(lines(&report), ["Hi", "Hi"], "{report}");
    }
    skalp(
        dir.path(),
        &["synth", "sender.sk", "-d", "ice40-hx8k", "-f", "-o", "out"],
    );
    let mut args = vec!["sim", "out", "--clock", "clk=20", "-d", "100us"];
    args.extend(uart);
    let report = skalp(dir.path(), &args);
    assert_eq!(lines(&report), ["Hi", "Hi"], "{report}");
    // Stopped once the second line was in (before 3 lines, 9.6 us), not at 100 us
    let stopped = report
        .lines()
        .find_map(|line| line.strip_prefix("Stopped: 2 UART lines at "))
        .unwrap_or_else(|| panic!("{report}"));
    let ns: f64 = stopped.trim_end_matches(" ns").parse().unwrap();
    assert!(ns < 9600.0, "stopped at {ns} ns");
}

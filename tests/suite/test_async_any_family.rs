//! Self-timed blocks with an arbiter on a device family with no measured
//! settle time: the build sizes every wait from the assumed figures, says
//! that it is not signed off, checks each matched delay with routed delays
//! (lengthening any that routing made too short), and the routed design
//! works in timed simulation.

use std::path::{Path, PathBuf};
use std::process::Command;
use tempfile::TempDir;

/// Four clients take turns at one server through a `match arbitrate`
const SHARED_SERVER: &str = "tests/fixtures/ncl_streams_shared_server.sk";

fn skalp(dir: &Path, args: &[&str]) -> String {
    let output = Command::new(PathBuf::from(env!("CARGO_BIN_EXE_skalp")))
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

#[test]
fn arbitrating_blocks_build_and_run_on_a_family_without_measurements() {
    let dir = TempDir::new().unwrap();
    let source = Path::new(env!("CARGO_MANIFEST_DIR")).join(SHARED_SERVER);
    let build = skalp(
        dir.path(),
        &[
            "synth",
            source.to_str().unwrap(),
            "-d",
            "ice40-hx8k",
            "-f",
            "-o",
            "out",
        ],
    );
    assert!(
        build.contains("no measured settle time"),
        "no warning that the waits are not signed off:\n{build}"
    );
    // The final report: every matched delay checked with routed delays, the
    // arbiter's wait among them, none short
    let report = &build[build.rfind("Timing:").expect("a timing report")..];
    assert!(report.contains("(arbiter): ✅"), "{report}");
    assert!(report.contains("(step decision): ✅"), "{report}");
    assert!(!report.contains("short by"), "{report}");

    // Each client gets all its replies with the routed delays
    let sim = skalp(
        dir.path(),
        &[
            "sim", "out", "--clock", "clk=40", "--set", "run=1", "--watch", "n3", "--until",
            "n3=32", "-d", "2ms",
        ],
    );
    assert!(sim.contains("Stopped: n3=32"), "{sim}");
    assert!(sim.contains("no setup or hold violations"), "{sim}");
}

/// A settle-time measurement build (`examples/settle_time`) cuts every
/// decision wait to the length it asks for, and says it is one
#[test]
fn a_measurement_build_cuts_decision_waits_to_its_length() {
    let dir = TempDir::new().unwrap();
    let source = Path::new(env!("CARGO_MANIFEST_DIR")).join("examples/settle_time/settle_race.sk");
    let build = skalp(
        dir.path(),
        &[
            "synth",
            source.to_str().unwrap(),
            "-d",
            "ice40-hx8k",
            "-f",
            "--settle-wait",
            "1.5ns",
            "-o",
            "out",
        ],
    );
    assert!(build.contains("Settle-time measurement build"), "{build}");
    assert!(!build.contains("no measured settle time"), "{build}");
    let report = &build[build.rfind("Timing:").expect("a timing report")..];
    let decisions: Vec<&str> = report
        .lines()
        .filter(|l| l.contains("(step decision)"))
        .collect();
    assert_eq!(decisions.len(), 2, "{report}");
    for line in decisions {
        assert!(line.contains("data 1.50 ns"), "{line}");
        assert!(line.contains("✅"), "{line}");
    }
}

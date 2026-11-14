//! Turning waveform capture off keeps a long simulation's memory constant

use skalp_testing::testbench::Testbench;

const COUNTER: &str = "examples/counter.sk";

#[tokio::test]
async fn capture_can_be_turned_off() {
    let mut tb = Testbench::behavioral(COUNTER)
        .await
        .expect("build testbench");
    tb.set("rst", 0u8);
    tb.clock(10).await;
    assert!(
        tb.export_waveform(std::env::temp_dir().join("skalp_capture_on.skw.gz"))
            .is_ok(),
        "capture is on by default"
    );

    tb.set_capture_waveforms(false);
    tb.clock(10).await;
    assert!(
        tb.export_waveform(std::env::temp_dir().join("skalp_capture_off.skw.gz"))
            .is_err(),
        "nothing kept once capture is off"
    );
    // The design still runs: 20 rising edges since the counter left reset
    assert_eq!(tb.get_u32("count").await, 20);

    tb.set_capture_waveforms(true);
    tb.clock(1).await;
    assert!(
        tb.export_waveform(std::env::temp_dir().join("skalp_capture_again.skw.gz"))
            .is_ok(),
        "capture resumes"
    );
}

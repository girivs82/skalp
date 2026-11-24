//! What the LPDDR4 controller's VREF(DQ) training should find against the
//! channel model's receiver window, for the LPDDR4 simulation tests.

/// MR14 as the fixtures' controllers have it (range 1, code 50: 42 % of VDDQ)
pub const MR14: u64 = 0x72;
/// The controller's default sweep: codes 0 to 50 in steps of 2
pub const CODES: std::ops::RangeInclusive<u64> = 0..=50;
pub const STEP: usize = 2;

/// The training's outcome
#[derive(Debug, PartialEq, Eq)]
pub struct Vref {
    /// MR14 written at the end
    pub mr14: u64,
    /// one bit per code swept, the last code in bit 0
    pub map: u64,
    /// {last, first} code of the longest passing run (0 if none)
    pub win: u64,
    /// codes swept (one MR14 write each, plus the final one)
    pub n_codes: u64,
    /// no code passed (training reports a failure)
    pub fail: bool,
}

/// The model takes writes at MR14 levels `ok_lo..=ok_hi` (range 1: 55 + code);
/// training takes the middle of the first longest passing run.
pub fn expected(ok_lo: u64, ok_hi: u64) -> Vref {
    let mut map = 0u64;
    let (mut run_start, mut run_len) = (0u64, 0u64);
    let (mut best_start, mut best_end, mut best_len) = (0u64, 0u64, 0u64);
    let mut n_codes = 0;
    for c in CODES.step_by(STEP) {
        let level = 55 + c;
        let pass = (ok_lo..=ok_hi).contains(&level);
        map = (map << 1) | pass as u64;
        n_codes += 1;
        if pass {
            if run_len == 0 {
                run_start = c;
            }
            run_len += 1;
            if run_len > best_len {
                (best_start, best_end, best_len) = (run_start, c, run_len);
            }
        } else {
            run_len = 0;
        }
    }
    let fail = best_len == 0;
    let mr14 = if fail {
        MR14
    } else {
        (MR14 & 0xC0) | ((best_start + best_end) >> 1)
    };
    Vref {
        mr14,
        map,
        win: (best_end << 6) | best_start,
        n_codes,
        fail,
    }
}

#[test]
fn lpddr4_vref_expected_window() {
    // all pass: middle of 0..=50
    let all = expected(0, 255);
    assert_eq!(all.mr14, 0x40 | 25);
    assert_eq!(all.map, (1 << 26) - 1);
    assert_eq!(all.n_codes, 26);
    // levels 70..=95: codes 15..=40, swept 16..=40
    let w = expected(70, 95);
    assert_eq!(w.mr14, 0x40 | 28);
    assert_eq!(w.win, (40 << 6) | 16);
    // nothing passes: the generic's MR14 stays
    let none = expected(0, 10);
    assert!(none.fail);
    assert_eq!(none.mr14, MR14);
}

//! What the LPDDR4 controller's VREF(DQ) training should find against the
//! channel model's receiver window, for the LPDDR4 simulation tests.

/// MR14 as the fixtures' controllers have it (range 1, code 50: 42 % of VDDQ)
pub const MR14: u64 = 0x72;
/// The controller's default sweep: levels (0.4 % of VDDQ) 25 to 105 in steps of 2, across
/// both MR14 ranges
pub const LEVELS: std::ops::RangeInclusive<u64> = 25..=105;
pub const STEP: usize = 2;

/// MR14 for a level: range 0 (code = level - 25) up to level 75, range 1 (code = level - 55)
/// above
pub fn mr14_of(level: u64) -> u64 {
    if level <= 75 {
        level - 25
    } else {
        0x40 | (level - 55)
    }
}

/// The training's outcome
#[derive(Debug, PartialEq, Eq)]
pub struct Vref {
    /// MR14 written at the end
    pub mr14: u64,
    /// one bit per level swept, the last level in bit 0
    pub map: u64,
    /// {last, first} level of the longest passing run (7 bits each, 0 if none)
    pub win: u64,
    /// levels swept (one MR14 write each, plus the final one)
    pub n_codes: u64,
    /// no code passed (training reports a failure)
    pub fail: bool,
}

/// The model takes writes at MR14 levels `ok_lo..=ok_hi`; training takes the
/// middle of the first longest passing run.
pub fn expected(ok_lo: u64, ok_hi: u64) -> Vref {
    let mut map = 0u64;
    let (mut run_start, mut run_len) = (0u64, 0u64);
    let (mut best_start, mut best_end, mut best_len) = (0u64, 0u64, 0u64);
    let mut n_codes = 0;
    for c in LEVELS.step_by(STEP) {
        let level = c;
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
        (MR14 & 0x80) | mr14_of((best_start + best_end) >> 1)
    };
    Vref {
        mr14,
        map,
        win: (best_end << 7) | best_start,
        n_codes,
        fail,
    }
}

#[test]
fn lpddr4_vref_expected_window() {
    // all pass: middle of 25..=105, level 65 (range 0, code 40)
    let all = expected(0, 255);
    assert_eq!(all.mr14, 40);
    assert_eq!(all.map, (1 << 41) - 1);
    assert_eq!(all.n_codes, 41);
    // levels 70..=95, swept 71..=95: middle 83 (range 1, code 28)
    let w = expected(70, 95);
    assert_eq!(w.mr14, 0x40 | 28);
    assert_eq!(w.win, (95 << 7) | 71);
    // nothing passes: the generic's MR14 stays
    let none = expected(0, 20);
    assert!(none.fail);
    assert_eq!(none.mr14, MR14);
}

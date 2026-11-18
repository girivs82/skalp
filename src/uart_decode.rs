//! Decoding a UART transmit line (8N1, least significant bit first) from
//! the changes of a simulated net, so simulation reports the same text a
//! board sends over its serial port.

/// One decoded character
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct UartChar {
    /// When its start bit began, in ps
    pub start_ps: i64,
    pub byte: u8,
    /// The stop bit was high
    pub framed: bool,
}

/// A streaming decoder: give it the line's level changes in time order
/// ([`Self::edge`]), and how far time has got ([`Self::advance_to`]).
/// Each character is sampled at the middle of its bits, as a receiver
/// would. An unknown level counts as high (idle).
#[derive(Debug, Clone)]
pub struct UartDecoder {
    bit_ps: i64,
    level: bool,
    /// The frame being received: when its start bit began, and the next
    /// sample to take (0 = start bit check, 1..=8 = data bits, 9 = stop)
    frame: Option<(i64, u32)>,
    byte: u8,
    chars: Vec<UartChar>,
}

impl UartDecoder {
    /// A decoder for `baud` bits a second, with the line at `level` now
    pub fn new(baud: u64, level: bool) -> Self {
        Self {
            bit_ps: (1_000_000_000_000 / baud.max(1)) as i64,
            level,
            frame: None,
            byte: 0,
            chars: Vec::new(),
        }
    }

    /// The line changed to `level` at `time_ps`
    pub fn edge(&mut self, time_ps: i64, level: bool) {
        self.advance_to(time_ps);
        if self.frame.is_none() && self.level && !level {
            self.frame = Some((time_ps, 0));
            self.byte = 0;
        }
        self.level = level;
    }

    /// Take the samples due before `time_ps`, with the line as it is now
    pub fn advance_to(&mut self, time_ps: i64) {
        while let Some((start, sample)) = self.frame {
            // The middle of bit `sample` (the start bit is bit 0)
            let at = start + self.bit_ps * i64::from(sample) + self.bit_ps / 2;
            if at >= time_ps {
                break;
            }
            match sample {
                // A start bit that is high by its middle was a glitch
                0 if self.level => self.frame = None,
                0 => self.frame = Some((start, 1)),
                1..=8 => {
                    self.byte |= u8::from(self.level) << (sample - 1);
                    self.frame = Some((start, sample + 1));
                }
                _ => {
                    self.chars.push(UartChar {
                        start_ps: start,
                        byte: self.byte,
                        framed: self.level,
                    });
                    self.frame = None;
                }
            }
        }
    }

    /// When the next sample of a character arriving is due, if one is
    pub fn next_sample(&self) -> Option<i64> {
        self.frame
            .map(|(start, sample)| start + self.bit_ps * i64::from(sample) + self.bit_ps / 2)
    }

    /// The characters received so far
    pub fn chars(&self) -> &[UartChar] {
        &self.chars
    }

    /// How many complete lines (ending in a line feed) have come
    pub fn lines(&self) -> usize {
        self.chars.iter().filter(|c| c.byte == b'\n').count()
    }
}

/// Characters as text lines: each with the time its first character
/// began, without the line ending; a last line without one is included.
/// Bytes that are not printable show as `\xNN`, and a character with a
/// low stop bit is marked `\!`.
pub fn text_lines(chars: &[UartChar]) -> Vec<(i64, String)> {
    let mut lines = Vec::new();
    let mut current: Option<(i64, String)> = None;
    for c in chars {
        let (_, text) = current.get_or_insert_with(|| (c.start_ps, String::new()));
        if !c.framed {
            text.push_str("\\!");
        }
        match c.byte {
            b'\n' => lines.extend(current.take()),
            b'\r' => {}
            b if b == b' ' || b.is_ascii_graphic() => text.push(b as char),
            b => text.push_str(&format!("\\x{b:02x}")),
        }
    }
    lines.extend(current);
    lines
}

/// Bytes read from a serial port as text lines, as [`text_lines`] shows
/// them (without times)
pub fn byte_lines(bytes: &[u8]) -> Vec<String> {
    let chars: Vec<UartChar> = bytes
        .iter()
        .map(|&byte| UartChar {
            start_ps: 0,
            byte,
            framed: true,
        })
        .collect();
    text_lines(&chars).into_iter().map(|(_, l)| l).collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    const BAUD: u64 = 1_000_000;
    const BIT: i64 = 1_000_000;

    /// The level changes of `bytes` sent back to back from `start`, with
    /// `gap` idle bits between characters; returns (changes, end time)
    fn send(bytes: &[u8], start: i64, gap: i64) -> (Vec<(i64, bool)>, i64) {
        let mut levels = Vec::new();
        let mut t = start;
        for &b in bytes {
            levels.push((t, false));
            for k in 0..8 {
                levels.push((t + BIT * (k + 1), (b >> k) & 1 == 1));
            }
            levels.push((t + BIT * 9, true));
            t += BIT * (10 + gap);
        }
        // Keep only the changes
        let mut changes: Vec<(i64, bool)> = Vec::new();
        let mut last = true;
        for (t, l) in levels {
            if l != last {
                changes.push((t, l));
                last = l;
            }
        }
        (changes, t)
    }

    fn decode(changes: &[(i64, bool)], end: i64) -> UartDecoder {
        let mut d = UartDecoder::new(BAUD, true);
        for &(t, l) in changes {
            d.edge(t, l);
        }
        d.advance_to(end);
        d
    }

    #[test]
    fn decodes_back_to_back_characters() {
        let text = b"0a1b2c3d ffffffff\r\n";
        let (changes, end) = send(text, 5 * BIT, 0);
        let d = decode(&changes, end);
        let bytes: Vec<u8> = d.chars().iter().map(|c| c.byte).collect();
        assert_eq!(bytes, text);
        assert!(d.chars().iter().all(|c| c.framed));
        assert_eq!(d.chars()[0].start_ps, 5 * BIT);
        assert_eq!(d.lines(), 1);
        assert_eq!(
            text_lines(d.chars()),
            vec![(5 * BIT, "0a1b2c3d ffffffff".to_string())]
        );
    }

    #[test]
    fn tolerates_a_baud_rate_a_few_percent_off() {
        // Bits 3% long: the last sample (stop, 9.5 bits) is 0.29 bit late
        let text = b"U\x00\xff";
        let (changes, end) = send(text, 0, 1);
        let stretched: Vec<(i64, bool)> =
            changes.iter().map(|&(t, l)| (t + t * 3 / 100, l)).collect();
        let d = decode(&stretched, end * 2);
        let bytes: Vec<u8> = d.chars().iter().map(|c| c.byte).collect();
        assert_eq!(bytes, text);
    }

    #[test]
    fn ignores_a_short_low_glitch() {
        let mut changes = vec![(BIT, false), (BIT + BIT / 4, true)];
        let (rest, end) = send(b"A", 3 * BIT, 0);
        changes.extend(rest);
        let d = decode(&changes, end);
        let bytes: Vec<u8> = d.chars().iter().map(|c| c.byte).collect();
        assert_eq!(bytes, b"A");
    }

    #[test]
    fn marks_a_missing_stop_bit() {
        // A break: the line held low for longer than a character
        let d = decode(&[(0, false), (20 * BIT, true)], 30 * BIT);
        assert_eq!(d.chars().len(), 1);
        assert_eq!(d.chars()[0].byte, 0);
        assert!(!d.chars()[0].framed);
        assert_eq!(text_lines(d.chars()), vec![(0, "\\!\\x00".to_string())]);
    }

    #[test]
    fn says_when_the_next_sample_is_due() {
        let mut d = UartDecoder::new(BAUD, true);
        assert_eq!(d.next_sample(), None);
        d.edge(10 * BIT, false);
        assert_eq!(d.next_sample(), Some(10 * BIT + BIT / 2));
        // A line held low: every sample comes without another change
        while let Some(t) = d.next_sample() {
            d.advance_to(t + 1);
        }
        assert_eq!(d.chars().len(), 1);
        assert_eq!(d.chars()[0].start_ps, 10 * BIT);
    }

    #[test]
    fn leaves_a_character_still_arriving() {
        let (changes, end) = send(b"AB", 0, 0);
        let d = decode(&changes, end - 3 * BIT);
        assert_eq!(d.chars().len(), 1);
    }

    #[test]
    fn splits_bytes_into_lines() {
        assert_eq!(
            byte_lines(b"one\r\ntwo\nthr\x01"),
            vec!["one", "two", "thr\\x01"]
        );
    }
}

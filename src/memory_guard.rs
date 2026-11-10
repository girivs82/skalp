//! A memory budget for the whole `skalp` process.
//!
//! Place and route on a large device can need several gigabytes. Left alone,
//! a run that grows past the machine's memory makes the host swap until it is
//! unusable. A background thread samples the process's memory footprint and
//! stops the run with a clear error once it passes the budget, so the host
//! stays responsive.
//!
//! The budget defaults to half of physical memory. `--max-memory` or the
//! `SKALP_MAX_MEMORY` environment variable set it explicitly, in bytes or
//! with a `K`, `M`, `G` or `T` suffix (`12G`), or as a percentage of physical
//! memory (`60%`). `0` turns the guard off.

use std::thread;
use std::time::Duration;

/// How often the footprint is sampled.
const SAMPLE_INTERVAL: Duration = Duration::from_millis(250);

/// Fraction of physical memory used when no budget is given.
const DEFAULT_FRACTION: f64 = 0.5;

/// Exit status of a run stopped by the guard.
pub const EXIT_OUT_OF_BUDGET: i32 = 3;

/// Parse a budget: bytes, a size with a K/M/G/T suffix, or a percentage of
/// `physical` bytes.
pub fn parse_budget(text: &str, physical: u64) -> Result<u64, String> {
    let text = text.trim();
    if let Some(percent) = text.strip_suffix('%') {
        let percent: f64 = percent
            .trim()
            .parse()
            .map_err(|_| format!("'{text}' is not a percentage"))?;
        if !(0.0..=100.0).contains(&percent) {
            return Err(format!("'{text}' is not between 0% and 100%"));
        }
        return Ok((physical as f64 * percent / 100.0) as u64);
    }
    let upper = text.to_ascii_uppercase();
    let upper = upper.strip_suffix('B').unwrap_or(&upper);
    let (digits, scale) = match upper.chars().last() {
        Some('K') => (&upper[..upper.len() - 1], 1u64 << 10),
        Some('M') => (&upper[..upper.len() - 1], 1 << 20),
        Some('G') => (&upper[..upper.len() - 1], 1 << 30),
        Some('T') => (&upper[..upper.len() - 1], 1 << 40),
        _ => (upper, 1),
    };
    let value: f64 = digits
        .trim()
        .parse()
        .map_err(|_| format!("'{text}' is not a memory size (for example 12G or 60%)"))?;
    if value < 0.0 {
        return Err(format!("'{text}' is negative"));
    }
    Ok((value * scale as f64) as u64)
}

/// Human-readable size.
pub fn format_bytes(bytes: u64) -> String {
    const GIB: f64 = (1u64 << 30) as f64;
    const MIB: f64 = (1u64 << 20) as f64;
    if bytes as f64 >= GIB {
        format!("{:.1} GB", bytes as f64 / GIB)
    } else {
        format!("{:.0} MB", bytes as f64 / MIB)
    }
}

/// Start the guard. `requested` is the `--max-memory` value, if given.
/// Returns the budget in bytes, or `None` when the guard is off.
pub fn start(requested: Option<&str>) -> Result<Option<u64>, String> {
    let Some(physical) = physical_memory() else {
        // Without the machine's size there is no sensible default.
        return Ok(None);
    };
    let from_env = std::env::var("SKALP_MAX_MEMORY").ok();
    let budget = match requested.or(from_env.as_deref()) {
        Some(text) => parse_budget(text, physical)?,
        None => (physical as f64 * DEFAULT_FRACTION) as u64,
    };
    if budget == 0 || current_footprint().is_none() {
        return Ok(None);
    }
    thread::Builder::new()
        .name("skalp-memory-guard".to_string())
        .spawn(move || watch(budget, physical))
        .map_err(|e| format!("cannot start the memory guard: {e}"))?;
    Ok(Some(budget))
}

fn watch(budget: u64, physical: u64) {
    loop {
        if let Some(used) = current_footprint() {
            if used > budget {
                eprintln!(
                    "\nerror: skalp stopped because it was using {} of memory, over its budget of {} \
                     ({}% of this machine's {}).\n\
                     The run was stopped before it could exhaust the machine's memory. To allow \
                     more, pass --max-memory (for example --max-memory 75%) or set \
                     SKALP_MAX_MEMORY; 0 turns the limit off.",
                    format_bytes(used),
                    format_bytes(budget),
                    budget * 100 / physical.max(1),
                    format_bytes(physical),
                );
                std::process::exit(EXIT_OUT_OF_BUDGET);
            }
        }
        thread::sleep(SAMPLE_INTERVAL);
    }
}

/// Physical memory of the machine, in bytes.
pub fn physical_memory() -> Option<u64> {
    #[cfg(target_os = "macos")]
    {
        let mut size: u64 = 0;
        let mut len = std::mem::size_of::<u64>();
        // SAFETY: hw.memsize is a 64-bit integer; `size` and `len` describe
        // a buffer of exactly that size.
        let status = unsafe {
            libc::sysctlbyname(
                c"hw.memsize".as_ptr(),
                (&mut size as *mut u64).cast(),
                &mut len,
                std::ptr::null_mut(),
                0,
            )
        };
        (status == 0 && size > 0).then_some(size)
    }
    #[cfg(target_os = "linux")]
    {
        // SAFETY: sysconf has no preconditions.
        let (pages, page_size) = unsafe {
            (
                libc::sysconf(libc::_SC_PHYS_PAGES),
                libc::sysconf(libc::_SC_PAGESIZE),
            )
        };
        (pages > 0 && page_size > 0).then(|| pages as u64 * page_size as u64)
    }
    #[cfg(not(any(target_os = "macos", target_os = "linux")))]
    {
        None
    }
}

/// Memory the process is charged for, in bytes.
///
/// On macOS this is the physical footprint, which (unlike resident size)
/// also counts memory the system has compressed or swapped out, so a run
/// cannot hide its growth behind memory compression.
pub fn current_footprint() -> Option<u64> {
    #[cfg(target_os = "macos")]
    {
        let mut info = std::mem::MaybeUninit::<libc::rusage_info_v2>::zeroed();
        // SAFETY: RUSAGE_INFO_V2 fills a rusage_info_v2, which `info` holds.
        let status = unsafe {
            libc::proc_pid_rusage(
                std::process::id() as libc::c_int,
                libc::RUSAGE_INFO_V2,
                info.as_mut_ptr().cast(),
            )
        };
        if status != 0 {
            return None;
        }
        // SAFETY: the call succeeded, so the structure is initialised.
        Some(unsafe { info.assume_init() }.ri_phys_footprint)
    }
    #[cfg(target_os = "linux")]
    {
        // Resident plus swapped-out memory, from the kernel's own accounting.
        let status = std::fs::read_to_string("/proc/self/status").ok()?;
        let field = |name: &str| -> Option<u64> {
            let line = status.lines().find(|l| l.starts_with(name))?;
            let kb: u64 = line.split_whitespace().nth(1)?.parse().ok()?;
            Some(kb * 1024)
        };
        Some(field("VmRSS:")? + field("VmSwap:").unwrap_or(0))
    }
    #[cfg(not(any(target_os = "macos", target_os = "linux")))]
    {
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const GIB: u64 = 1 << 30;

    #[test]
    fn budgets_parse_sizes_and_percentages() {
        assert_eq!(parse_budget("12G", 32 * GIB), Ok(12 * GIB));
        assert_eq!(parse_budget("12gb", 32 * GIB), Ok(12 * GIB));
        assert_eq!(parse_budget("512M", 32 * GIB), Ok(512 << 20));
        assert_eq!(parse_budget("1.5G", 32 * GIB), Ok(3 * GIB / 2));
        assert_eq!(parse_budget("4096", 32 * GIB), Ok(4096));
        assert_eq!(parse_budget("50%", 32 * GIB), Ok(16 * GIB));
        assert_eq!(parse_budget("0", 32 * GIB), Ok(0));
        assert!(parse_budget("lots", 32 * GIB).is_err());
        assert!(parse_budget("150%", 32 * GIB).is_err());
    }

    #[test]
    fn this_machine_reports_its_memory() {
        if cfg!(any(target_os = "macos", target_os = "linux")) {
            let physical = physical_memory().expect("physical memory");
            let used = current_footprint().expect("footprint");
            assert!(used > 0 && used < physical);
        }
    }
}

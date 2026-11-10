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
//!
//! It works on macOS, Linux, Windows and the other Unix systems; see
//! [`current_footprint`] for what each one measures.

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
    let from_env = std::env::var("SKALP_MAX_MEMORY").ok();
    let requested = requested.or(from_env.as_deref());
    let physical = physical_memory();
    let budget = match (requested, physical) {
        (Some(text), Some(physical)) => parse_budget(text, physical)?,
        (Some(text), None) if !text.trim().ends_with('%') => parse_budget(text, 0)?,
        (Some(text), None) => {
            return Err(format!(
                "'{text}' is a share of physical memory, which this system does not report; \
                 give a size such as 12G"
            ))
        }
        (None, Some(physical)) => (physical as f64 * DEFAULT_FRACTION) as u64,
        (None, None) => {
            eprintln!(
                "warning: this system does not report its physical memory, so skalp has no \
                 default memory limit; pass --max-memory to set one"
            );
            return Ok(None);
        }
    };
    if budget == 0 {
        return Ok(None);
    }
    if current_footprint().is_none() {
        eprintln!(
            "warning: skalp cannot measure its own memory use on this system, so no memory \
             limit is enforced"
        );
        return Ok(None);
    }
    thread::Builder::new()
        .name("skalp-memory-guard".to_string())
        .spawn(move || watch(budget, physical))
        .map_err(|e| format!("cannot start the memory guard: {e}"))?;
    Ok(Some(budget))
}

fn watch(budget: u64, physical: Option<u64>) {
    loop {
        if let Some(used) = current_footprint() {
            if used > budget {
                let share = physical
                    .map(|p| {
                        format!(
                            " ({}% of this machine's {})",
                            budget * 100 / p.max(1),
                            format_bytes(p)
                        )
                    })
                    .unwrap_or_default();
                eprintln!(
                    "\nerror: skalp stopped because it was using {} of memory, over its budget \
                     of {}{share}.\n\
                     The run was stopped before it could exhaust the machine's memory. To allow \
                     more, pass --max-memory (for example --max-memory 75%) or set \
                     SKALP_MAX_MEMORY; 0 turns the limit off.",
                    format_bytes(used),
                    format_bytes(budget),
                );
                std::process::exit(EXIT_OUT_OF_BUDGET);
            }
        }
        thread::sleep(SAMPLE_INTERVAL);
    }
}

/// Physical memory of the machine, in bytes.
pub fn physical_memory() -> Option<u64> {
    platform::physical_memory()
}

/// Memory the process is charged for, in bytes.
///
/// Each platform reports the measure that includes memory the system has
/// compressed or paged out, so a run cannot hide its growth from the guard:
/// the physical footprint on macOS, resident plus swapped memory on Linux,
/// and the private commit charge on Windows. Other Unix systems report the
/// peak resident size, which never undercounts what the run has reached.
pub fn current_footprint() -> Option<u64> {
    platform::current_footprint()
}

#[cfg(target_os = "macos")]
mod platform {
    pub fn physical_memory() -> Option<u64> {
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

    pub fn current_footprint() -> Option<u64> {
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
}

#[cfg(target_os = "linux")]
mod platform {
    pub fn physical_memory() -> Option<u64> {
        super::unix_physical_memory()
    }

    pub fn current_footprint() -> Option<u64> {
        let status = std::fs::read_to_string("/proc/self/status").ok()?;
        let field = |name: &str| -> Option<u64> {
            let line = status.lines().find(|l| l.starts_with(name))?;
            let kb: u64 = line.split_whitespace().nth(1)?.parse().ok()?;
            Some(kb * 1024)
        };
        Some(field("VmRSS:")? + field("VmSwap:").unwrap_or(0))
    }
}

#[cfg(windows)]
mod platform {
    use windows_sys::Win32::System::ProcessStatus::{
        GetProcessMemoryInfo, PROCESS_MEMORY_COUNTERS, PROCESS_MEMORY_COUNTERS_EX,
    };
    use windows_sys::Win32::System::SystemInformation::{GlobalMemoryStatusEx, MEMORYSTATUSEX};
    use windows_sys::Win32::System::Threading::GetCurrentProcess;

    pub fn physical_memory() -> Option<u64> {
        // SAFETY: MEMORYSTATUSEX is plain data; dwLength is set as the API requires.
        let mut status: MEMORYSTATUSEX = unsafe { std::mem::zeroed() };
        status.dwLength = std::mem::size_of::<MEMORYSTATUSEX>() as u32;
        // SAFETY: `status` is a valid, correctly sized MEMORYSTATUSEX.
        let ok = unsafe { GlobalMemoryStatusEx(&mut status) };
        (ok != 0 && status.ullTotalPhys > 0).then_some(status.ullTotalPhys)
    }

    pub fn current_footprint() -> Option<u64> {
        // SAFETY: the counters are plain data, filled in by the call below.
        let mut counters: PROCESS_MEMORY_COUNTERS_EX = unsafe { std::mem::zeroed() };
        let size = std::mem::size_of::<PROCESS_MEMORY_COUNTERS_EX>() as u32;
        counters.cb = size;
        // SAFETY: GetCurrentProcess returns a pseudo-handle that needs no
        // closing; `counters` is `size` bytes, as `cb` says.
        let ok = unsafe {
            GetProcessMemoryInfo(
                GetCurrentProcess(),
                (&mut counters as *mut PROCESS_MEMORY_COUNTERS_EX)
                    .cast::<PROCESS_MEMORY_COUNTERS>(),
                size,
            )
        };
        // PrivateUsage is the commit charge: memory the process owns,
        // whether it is in RAM or in the page file.
        (ok != 0).then_some(counters.PrivateUsage as u64)
    }
}

#[cfg(all(unix, not(any(target_os = "macos", target_os = "linux"))))]
mod platform {
    pub fn physical_memory() -> Option<u64> {
        super::unix_physical_memory()
    }

    pub fn current_footprint() -> Option<u64> {
        let mut usage = std::mem::MaybeUninit::<libc::rusage>::zeroed();
        // SAFETY: getrusage fills the rusage that `usage` holds.
        if unsafe { libc::getrusage(libc::RUSAGE_SELF, usage.as_mut_ptr()) } != 0 {
            return None;
        }
        // SAFETY: the call succeeded, so the structure is initialised.
        let peak_kb = unsafe { usage.assume_init() }.ru_maxrss;
        u64::try_from(peak_kb).ok().map(|kb| kb * 1024)
    }
}

#[cfg(not(any(unix, windows)))]
mod platform {
    pub fn physical_memory() -> Option<u64> {
        None
    }

    pub fn current_footprint() -> Option<u64> {
        None
    }
}

/// Physical memory from `sysconf`, available on Linux and the BSDs.
#[cfg(all(unix, not(target_os = "macos")))]
fn unix_physical_memory() -> Option<u64> {
    // SAFETY: sysconf has no preconditions.
    let (pages, page_size) = unsafe {
        (
            libc::sysconf(libc::_SC_PHYS_PAGES),
            libc::sysconf(libc::_SC_PAGESIZE),
        )
    };
    (pages > 0 && page_size > 0).then(|| pages as u64 * page_size as u64)
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
        if cfg!(any(unix, windows)) {
            let physical = physical_memory().expect("physical memory");
            let used = current_footprint().expect("footprint");
            assert!(used > 0 && used < physical);
        }
    }
}

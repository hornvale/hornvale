//! Where this client keeps state between runs — the one module that owns the
//! answer, so nothing else has to guess.
//!
//! Two callers need it and they need the SAME directory: the startup frame
//! ([`crate::overture`]) reads the previous run's per-phase timings from it to
//! size its bar, and the world cache writes generated worlds into it. Before
//! this module the client had never written anything to disk at all.
//!
//! **std-only, deliberately.** There is no `dirs` crate here and there is not
//! going to be one: `clients/game/bin` carries crossterm and signal-hook and
//! the campaign adds nothing. XDG's rule is three lines.
//!
//! **Nothing here is in the determinism path.** A world is a seed plus a
//! ledger and is re-derivable from the seed alone (decision 0055: the repo
//! boundary is the determinism boundary, and `clients/` is outside it). A
//! missing, unreadable, or stale state directory must therefore always
//! degrade to *doing the work again*, never to an error — which is why every
//! function here returns an [`Option`] or a plain [`std::io::Result`] the
//! caller is expected to shrug off.
//!
//! # Testing
//!
//! The resolution rule is a pure function ([`resolve`]) over three borrowed
//! strings, and the tests exercise THAT rather than the environment. This is
//! not a stylistic preference: a test that set `HOME` to probe the fallback
//! would be mutating process-global state shared with every other test in a
//! threaded binary, and a test that read the real `HOME` would be one bug away
//! from writing into the developer's actual home directory. Neither happens
//! here — no test in this crate touches the environment or the real state
//! directory, and every on-disk function has an explicit-path twin
//! (`*_at`/`*_to`/`*_from`) for a test to point at a temporary directory.

use std::path::{Path, PathBuf};

/// The environment variable that overrides the resolved state directory
/// outright.
///
/// It exists for two audiences at once, and both want the same behaviour: a
/// user relocating the client's cache, and a TEST that must never write to the
/// real one. Set it to an ABSOLUTE path; a relative one is refused (see
/// [`resolve`]).
pub const STATE_DIR_ENV: &str = "HORNVALE_GAME_STATE_DIR";

/// The directory name appended to the resolved cache root.
const APP_DIR: &str = "hornvale";

/// The pure resolution rule, over the three environment values that decide it
/// — so it can be tested without touching the environment at all.
///
/// In order:
///
/// 1. `override_dir` ([`STATE_DIR_ENV`]) verbatim, if set, non-empty and
///    ABSOLUTE;
/// 2. `xdg_cache` (`XDG_CACHE_HOME`) `/hornvale`, if set, non-empty and
///    absolute;
/// 3. `home` (`HOME`) `/.cache/hornvale`, if set, non-empty and absolute;
/// 4. otherwise [`None`] — this client has nowhere to keep state, and every
///    caller degrades to redoing the work.
///
/// **A relative path is REFUSED, not resolved against the current directory.**
/// That is the shape of a real, recorded footgun in this repo: the census
/// runner documented `HV_CENSUS_WORKTREE=canonical`, a bare relative name,
/// and it created an untracked directory *inside the repository* that a `git
/// clean -fdx` would delete (decision 0146). A cache root is exactly the same
/// hazard, so a relative value yields [`None`] here and the caller behaves as
/// though nothing were configured — the honest, lossless degradation.
pub fn resolve(
    override_dir: Option<&str>,
    xdg_cache: Option<&str>,
    home: Option<&str>,
) -> Option<PathBuf> {
    fn absolute(value: Option<&str>) -> Option<&Path> {
        let path = Path::new(value?.trim());
        if path.as_os_str().is_empty() || !path.is_absolute() {
            return None;
        }
        Some(path)
    }

    if let Some(dir) = absolute(override_dir) {
        return Some(dir.to_path_buf());
    }
    if let Some(cache) = absolute(xdg_cache) {
        return Some(cache.join(APP_DIR));
    }
    absolute(home).map(|home| home.join(".cache").join(APP_DIR))
}

/// Read one environment variable as a `String`, or `None` when it is unset or
/// not UTF-8. A non-UTF-8 cache root is treated as unset for the same reason a
/// relative one is: better no cache than a surprising one.
fn var(name: &str) -> Option<String> {
    std::env::var(name).ok()
}

/// This client's state directory, per [`resolve`]. Does NOT create it — see
/// [`ensure`].
pub fn state_dir() -> Option<PathBuf> {
    let (over, xdg, home) = (var(STATE_DIR_ENV), var("XDG_CACHE_HOME"), var("HOME"));
    resolve(over.as_deref(), xdg.as_deref(), home.as_deref())
}

/// The path a named state file would occupy, whether or not it exists.
pub fn state_path(name: &str) -> Option<PathBuf> {
    Some(state_dir()?.join(name))
}

/// The state directory, created if absent. `Ok(None)` means there is nowhere
/// to keep state (rule 4 of [`resolve`]) — a legitimate configuration, not an
/// error; `Err` means there IS somewhere and it could not be created.
pub fn ensure() -> std::io::Result<Option<PathBuf>> {
    let Some(dir) = state_dir() else {
        return Ok(None);
    };
    std::fs::create_dir_all(&dir)?;
    Ok(Some(dir))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn the_override_wins_over_both_xdg_and_home() {
        // Non-vacuity: all THREE are set to distinguishable absolute paths, so
        // this cannot pass by the other two being absent — the failure mode a
        // one-variable test would miss entirely.
        let got = resolve(Some("/tmp/over"), Some("/tmp/xdg"), Some("/tmp/home"));
        assert_eq!(got, Some(PathBuf::from("/tmp/over")));
    }

    #[test]
    fn xdg_wins_over_home_and_gains_the_app_directory() {
        let got = resolve(None, Some("/tmp/xdg"), Some("/tmp/home"));
        assert_eq!(got, Some(PathBuf::from("/tmp/xdg/hornvale")));
    }

    #[test]
    fn home_is_the_last_resort_and_lands_under_dot_cache() {
        let got = resolve(None, None, Some("/tmp/home"));
        assert_eq!(got, Some(PathBuf::from("/tmp/home/.cache/hornvale")));
    }

    #[test]
    fn nothing_set_means_nowhere_to_keep_state() {
        assert_eq!(resolve(None, None, None), None);
    }

    #[test]
    fn a_relative_override_is_refused_and_falls_through() {
        // Decision 0146's footgun. `canonical` must NOT become `./canonical`;
        // it must be ignored so the next rule answers. Asserting the FALL-
        // THROUGH rather than just `None` is what makes this discriminate: an
        // implementation that returned `None` outright on a relative override
        // would also pass a bare `is_none()` check while losing the XDG value
        // the user really did set.
        let got = resolve(Some("canonical"), Some("/tmp/xdg"), None);
        assert_eq!(got, Some(PathBuf::from("/tmp/xdg/hornvale")));
    }

    #[test]
    fn an_empty_or_whitespace_value_counts_as_unset() {
        assert_eq!(
            resolve(Some(""), Some("   "), Some("/tmp/home")),
            Some(PathBuf::from("/tmp/home/.cache/hornvale"))
        );
    }
}

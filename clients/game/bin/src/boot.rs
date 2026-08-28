//! The boot seam: getting from a parsed seed/target to a running
//! [`Driver`], with the terminal's restore-before-report ordering pulled
//! out where it can be tested without a real terminal.
//!
//! `main.rs`'s `run` opens the terminal *before* attempting genesis (The
//! Overture, Task 1), so every later view has a live screen from the first
//! frame rather than a blank one while genesis runs. That inversion has a
//! cost: if genesis or possession then fails, the terminal is already in
//! raw mode on the alternate screen, so it must be restored *before* the
//! failure is printed, or the user reads an error message mangled by the
//! alternate screen (or invisible on it entirely). [`TermHandle`] and
//! [`start_and_report`] make that ordering an explicit, testable seam
//! rather than something only [`crate::term::Term`]'s `Drop` backstop
//! happens to get right.

use crate::driver::{Driver, DriverError};

/// What [`start_and_report`] needs from a terminal: a way to restore it,
/// and a way to mark that an error is about to be reported. Abstracted so
/// the ordering between the two can be exercised by a recording double
/// rather than a real terminal (which needs a real tty and enables raw
/// mode as a side effect of existing).
pub trait TermHandle {
    /// Restore the terminal to its pre-open state. Idempotent — the real
    /// terminal's own [`Drop`] still runs afterward as the unconditional
    /// backstop; this call makes the restore happen explicitly, ahead of
    /// any error text, rather than merely hoping `Drop` gets there first.
    fn restore(&self);

    /// Mark that `message` is about to be reported to the user. The real
    /// terminal does nothing here — producing and printing the text is
    /// the caller's job (`main`'s `eprintln!`) — but a double can record
    /// the call to prove it happens only after [`TermHandle::restore`].
    fn report(&self, message: &str);
}

/// Build the driver, restoring `term` and marking the error reported
/// before handing it back if `start` fails.
///
/// `start` is a thunk rather than a bare `(seed, target)` pair so a
/// failure can be injected directly in a test instead of needing a real
/// seed whose genesis or possession fails. `SkyPins`/`TerrainPins` default
/// to no constraints, so the only failure `Driver::start(seed, target)`
/// can realistically hit with them is `DriverError::Possess(VesselError::
/// NoSettlement)` — and that has become rare across ordinary seeds (see
/// `windows/vessel/tests/suite/session_snapshot.rs`'s own settlement-free
/// fixture, which reaches for `BuildDepth::Terrain` rather than a seed
/// found by luck, precisely because one is no longer easy to find).
/// Searching seed space here would be slow, non-deterministic in what it
/// actually tests, and still only exercise the same `NoSettlement` path a
/// direct injection reaches for free.
pub fn start_and_report<T: TermHandle>(
    term: &T,
    start: impl FnOnce() -> Result<Driver, DriverError>,
) -> Result<Driver, String> {
    start().map_err(|e| {
        term.restore();
        let message = e.to_string();
        term.report(&message);
        message
    })
}

//! The boot seam's acceptance test: a genesis/possession failure still
//! leaves the terminal restored before the error is reported.
//!
//! The Overture, Task 1 inverts `main.rs`'s `run` so `Term::open()`
//! precedes `Driver::start` — every later view needs a live screen from
//! the first frame, not a blank one while genesis runs. That ordering has
//! a cost: once the terminal opens first, a failure below it must restore
//! the terminal *before* printing, rather than getting that for free from
//! never having opened it at all. This test drives `boot::start_and_report`
//! (the seam that owns that ordering) with a recording double rather than a
//! real terminal, so the property is checked rather than hoped for.
//!
//! **No seed was used to reach a real failure.** `SkyPins`/`TerrainPins`
//! default to no constraints, so `GenesisError`'s only two variants
//! (`InvalidPin`/`UnsatisfiablePin`, `domains/astronomy/src/pins.rs`) can
//! never fire on the unpinned path `Driver::start` takes — the only
//! failure a default `Driver::start(seed, target)` call can realistically
//! hit is `DriverError::Possess(VesselError::NoSettlement)`, and per
//! `windows/vessel/tests/suite/session_snapshot.rs`'s own comment, that
//! has become rare enough across ordinary seeds that campaign now reaches
//! for a settlement-free-by-construction fixture (`BuildDepth::Terrain`)
//! rather than a seed found by luck. Brute-forcing seed space here would
//! be slow, would still only ever land on that same `NoSettlement` variant,
//! and would make the test's failure or success depend on worldgen's
//! current settlement rate rather than on the ordering under test — so the
//! failure is injected directly at the seam, using that same real variant.

use hornvale_game::boot::{self, TermHandle};
use hornvale_game::driver::DriverError;
use hornvale_vessel::VesselError;
use std::cell::RefCell;

/// One call `RecordingTerm` observed, in the order it was made.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Event {
    /// `TermHandle::restore` was called.
    Restore,
    /// `TermHandle::report` was called.
    Report,
}

/// A `TermHandle` double that records the ORDER of `restore`/`report`
/// calls rather than merely whether either happened, so
/// `restored_before_reporting` is a real question about ordering.
#[derive(Default)]
struct RecordingTerm {
    events: RefCell<Vec<Event>>,
}

impl TermHandle for RecordingTerm {
    fn restore(&self) {
        self.events.borrow_mut().push(Event::Restore);
    }

    fn report(&self, _message: &str) {
        self.events.borrow_mut().push(Event::Report);
    }
}

impl RecordingTerm {
    /// `true` only if BOTH a `restore` and a `report` were recorded, and
    /// the `restore` came strictly before the `report` — vacuously `false`
    /// if either event never fired, so this cannot be satisfied by an
    /// implementation that skips one of the two calls.
    fn restored_before_reporting(&self) -> bool {
        let events = self.events.borrow();
        let restore_idx = events.iter().position(|e| *e == Event::Restore);
        let report_idx = events.iter().position(|e| *e == Event::Report);
        matches!((restore_idx, report_idx), (Some(r), Some(rep)) if r < rep)
    }
}

/// A genesis/possession failure injected directly at the seam (see the
/// module doc for why no real seed is used): a real, documented failure
/// mode of the pipeline `Driver::start` runs, constructed without paying
/// for an actual world build.
fn failing_genesis() -> Result<hornvale_game::driver::Driver, DriverError> {
    Err(DriverError::Possess(VesselError::NoSettlement))
}

#[test]
fn a_failing_genesis_still_leaves_the_terminal_restored() {
    let term = RecordingTerm::default();
    let err = boot::start_and_report(&term, failing_genesis);
    assert!(
        err.is_err(),
        "the test's premise: the injected start must fail"
    );
    assert!(
        term.restored_before_reporting(),
        "the terminal was not restored before the error was reported"
    );
}

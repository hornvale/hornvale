//! Searching for a world that exercises a precondition, instead of pinning one.
//!
//! **Why this module exists.** The Sighting's evidence originally rested on an
//! accident of the seed-42 world: after one tick, a creature happened to be
//! co-located in the chamber the possession enters, and standing somewhere the
//! shadowcast could reach. Five tests and one client fixture were written
//! against that. The Tense then reseeded the flagship — the settlement went
//! `Goodogododaga` → `Googo`, the structure went from two chambers to four —
//! and seed 42 stopped being a world that exercises the feature at all. Sight
//! still worked; the *fixture world* had moved out from under the tests.
//!
//! Re-pointing at some other single seed would reproduce that fragility
//! exactly, one campaign later. So the tests that need "a creature is drawn on
//! the chamber plan" now **search** for such a world, the same way
//! `lattice::anchor_cells`'s property batteries sweep `0u64..64` rather than
//! asserting over one fixture.
//!
//! **The search is loud in both directions.** It returns the FIRST seed whose
//! world satisfies the caller's predicate — 19 of the first 24 seeds do, so in
//! practice it costs one or two world builds — and it PANICS, naming the range
//! and the predicate, when none does. A sweep that quietly found nothing and
//! let its test pass would be strictly worse than the hardcoded seed it
//! replaces: the loud precondition assertions are what caught The Tense's
//! reseed in the first place.

#![allow(dead_code)]

use hornvale_kernel::{Seed, World};
use hornvale_vessel::{PlanMark, PossessOpts, Session, SpatialChannel};

/// The seeds searched. Wide enough that "no world in here draws a creature" is
/// a real finding about the sim rather than about the sample, and cheap in
/// practice because the search stops at its first hit.
pub const SIGHT_SEEDS: std::ops::Range<u64> = 0..64;

/// A world built at `seed`, or `None` if this seed has no world to build.
pub fn build(seed: u64) -> Option<World> {
    hornvale_worldgen::build_world(
        Seed(seed),
        &Default::default(),
        hornvale_worldgen::SkyChoice::Generated,
        &Default::default(),
        &Default::default(),
    )
    .ok()
}

/// The marks a live session's snapshot draws on its chamber plan.
///
/// Panics if the session is not in the chamber band, because every caller has
/// just walked it there and a `Walk` here would mean `enter` silently failed.
pub fn marks_of(session: &Session<'_>) -> Vec<PlanMark> {
    match session
        .snapshot()
        .expect("a live session snapshots")
        .spatial
    {
        SpatialChannel::Chamber { plan } => plan.marks,
        SpatialChannel::Walk { .. } => panic!("expected the chamber band"),
    }
}

/// Whether the possession is in the chamber band — i.e. `enter` found a
/// structure to enter. Read off the wire tag rather than a private field,
/// because that is all an integration test can see.
pub fn is_inside(session: &Session<'_>) -> bool {
    matches!(
        session
            .snapshot()
            .expect("a live session snapshots")
            .spatial,
        SpatialChannel::Chamber { .. }
    )
}

/// One tick in and one `enter` deep — the script every sight test walks.
///
/// The `wait` is load-bearing: the within-room `Occupancy` is populated by
/// `DriveMovements::step_with_occupancy`, which only runs on a tick, so before
/// the first `wait` no creature has a fine-layer anchor and the embedding has
/// nothing to place.
pub fn step_inside(session: &mut Session<'_>) {
    session.handle("wait");
    session.handle("enter");
}

/// The first seed in [`SIGHT_SEEDS`] whose fresh possession satisfies `pred` —
/// with the world it was built from.
///
/// `pred` receives the session as `Session::start` returns it, having taken no
/// turns, so a caller whose precondition spans the walk band AND the chamber
/// band (the doorway-parity test does) can walk it itself rather than being
/// handed a session already indoors.
///
/// `what` names the property being searched for and appears in the panic
/// message, so a search that comes up empty says what the sim stopped doing
/// rather than merely that a test failed.
pub fn world_where(what: &str, pred: impl Fn(&mut Session<'_>) -> bool) -> (u64, World) {
    for seed in SIGHT_SEEDS {
        let Some(world) = build(seed) else { continue };
        let hit = {
            let Ok((mut session, _)) = Session::start(&world, &PossessOpts::default()) else {
                continue;
            };
            pred(&mut session)
        };
        if hit {
            return (seed, world);
        }
    }
    panic!(
        "no seed in {SIGHT_SEEDS:?} produces a world where {what} — the search \
         found nothing, so nothing below could be tested. This is a finding \
         about the sim, not a flaky fixture: either the feature regressed or \
         every world in the range stopped exercising it."
    );
}

/// A world whose opening chamber, one tick in, draws at least one creature on
/// its plan — the precondition The Sighting's evidence rests on.
pub fn world_that_draws_a_creature() -> (u64, World) {
    world_where("a creature is drawn on the entered chamber's plan", |s| {
        step_inside(s);
        is_inside(s) && !marks_of(s).is_empty()
    })
}

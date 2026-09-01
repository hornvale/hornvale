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

use hornvale_kernel::{EntityId, Seed, World};
use hornvale_vessel::{PlanMark, PossessOpts, Session, SpatialChannel, Turn};

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
        SpatialChannel::Walk { .. } | SpatialChannel::Underground { .. } => {
            panic!("expected the chamber band")
        }
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

/// Place `who` at the possession, and walk further in until the chamber it
/// stands in actually DRAWS the mark. The caller has already entered.
///
/// **Entering alone stopped being enough at The Pavement, and the cause
/// is the epoch rather than anything about sight.** A structure is drawn from
/// its own room's seed and the cube-sphere mesh moved every room address, so
/// the flagship's ENTRANCE chamber is drawn from a different seed than it was.
/// Measured on seed 42: its five room anchors resolve to three cells and none
/// of them lies inside the shadowcast, while one chamber further in has two
/// lit and one unlit. Every fixture that entered and then asserted a mark was
/// reading a chamber that draws none, and each said so in its own words —
/// *"the placed companion was chosen BECAUSE it draws a mark"*, *"'x' must be
/// drawn on the plan indoors"*.
///
/// Walking until the property holds is the same discipline as
/// [`world_where`] one function down: ask for the property, do not pin the
/// place that happened to have it. It panics rather than returning a chamber
/// that draws nothing, for the reason that module doc gives — a search that
/// quietly found nothing is worse than the hardcoded fixture it replaces.
pub fn deepen_until_the_plan_draws(session: &mut Session<'_>, who: EntityId) {
    assert!(
        is_inside(session),
        "the possession is not indoors, so nothing below is tested"
    );
    // `MAX_CHAMBERS` is 4, so four steps is one more than any structure has;
    // the loop stops on the far-end reply rather than on the count.
    for _ in 0..4 {
        session.place_creature_at_me(who);
        if !marks_of(session).is_empty() {
            return;
        }
        let reply = match session.handle("enter further in") {
            Turn::Out(t) | Turn::Released(t) => t,
        };
        assert!(
            reply.starts_with("[chamber "),
            "no chamber of this structure draws a placed creature on its plan, so \
             nothing below is tested: {reply}"
        );
    }
    panic!("the structure ran past MAX_CHAMBERS without the plan ever drawing");
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

//! The Minute — a held body's acts are minuted (spec
//! `docs/superpowers/specs/2026-09-03-the-minute-design.md`, §4).
//!
//! P1 and P2 are the campaign's preregistered measurements, frozen before
//! the code. P7 (Task 4) joins this file. Everything here reads public API;
//! the in-module tests in `session.rs` hold the halves that need the ledger.

use hornvale_kernel::{Seed, World};
use hornvale_vessel::liveness::{AffectLabel, Mode};
use hornvale_vessel::{PossessOpts, Session};

fn world_at(seed: u64) -> World {
    if seed == 42 {
        return hornvale_worldgen::seed_42_world();
    }
    hornvale_worldgen::build_world(
        Seed(seed),
        &Default::default(),
        hornvale_worldgen::SkyChoice::Generated,
        &Default::default(),
        &Default::default(),
    )
    .expect("the seed builds")
}

/// How many committed facts with `predicate` name the driven body as
/// subject. Read through the ledger's serialized form because the ledger
/// itself is private to the session; `session_ledger_json` is the
/// determinism accessor and serializes `facts` as an array of `Fact`s.
pub fn driven_facts_named(session: &Session<'_>, predicate: &str) -> usize {
    let json: serde_json::Value =
        serde_json::from_str(&session.session_ledger_json()).expect("a ledger is JSON");
    let me = serde_json::to_value(session.agent_entity()).expect("an id serializes");
    json["facts"]
        .as_array()
        .expect("a ledger has a facts array")
        .iter()
        .filter(|f| f["subject"] == me && f["predicate"] == predicate)
        .count()
}

fn possessed(seed: u64) -> (World, ()) {
    (world_at(seed), ())
}

/// P1 — seed 42, the minuted drink. Before Task 2 (measured 2026-09-03): the
/// walk emitted 29 facts in 40 days, `drank` on every tick from the second,
/// and 0 reached the ledger while the felt state read `Content`. After: the
/// drinks are on the ledger and the felt state is unchanged — it was right,
/// the ledger was wrong.
///
/// RED BEFORE TASK 2: `seed 42's held body must have its drinks minuted`
/// (left 0).
#[test]
fn p1_a_held_bodys_drinks_reach_the_ledger_and_its_felt_state_stands() {
    let (world, ()) = possessed(42);
    let (mut session, _) = Session::start(&world, &PossessOpts::default()).expect("starts");
    let _ = session.handle("!possess");
    assert!(session.possessor().is_some(), "possession must be open");
    for _ in 0..8 {
        let _ = session.handle("!wait 5");
    }
    assert!(
        driven_facts_named(&session, "drank") >= 7,
        "seed 42's held body must have its drinks minuted: got {}",
        driven_facts_named(&session, "drank")
    );
    assert_eq!(session.driven_mode(), Some(Mode::Idle));
    assert_eq!(
        session.driven_affect(),
        Some(AffectLabel::Content),
        "the felt state was already right; only the ledger moves"
    );
}

/// P2 — seed 7, progress accumulates. Before Task 2: the walk sought water
/// for 14 then 15 rooms, restarted from the origin every tick, and the body
/// read `Helpless` from day 20 with 0 `drank`. The mechanism half (the
/// column moves on the first acting wait) is asserted unconditionally. The
/// PREDICTION half — at least one `drank` by day 36 — is the preregistered
/// bet, and the plan's decision rule for a red result is in Task 2.
///
/// RED BEFORE TASK 2 at the first assertion: the column does not move.
#[test]
fn p2_a_held_bodys_walk_accumulates_across_ticks() {
    let (world, ()) = possessed(7);
    let (mut session, _) = Session::start(&world, &PossessOpts::default()).expect("starts");
    let _ = session.handle("!possess");
    assert!(session.possessor().is_some(), "possession must be open");
    let _ = session.handle("!wait 1");
    let origin = session.position();
    let _ = session.handle("!wait 5");
    assert_ne!(
        session.position(),
        origin,
        "the held body's first seeking wait must move the column"
    );
    for _ in 0..6 {
        let _ = session.handle("!wait 5");
    }
    assert!(
        driven_facts_named(&session, "drank") >= 1,
        "PREREGISTERED PREDICTION (spec §4 P2): a walk that resumes reaches water \
         a walk that restarts could not; got 0 drank by day 36 — a red here is \
         the null finding, see the plan's Task 2 decision rule"
    );
}

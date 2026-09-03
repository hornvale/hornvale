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
    let world = world_at(42);
    let (mut session, _) = Session::start(&world, &PossessOpts::default()).expect("starts");
    let _ = session.handle("!possess");
    assert!(session.possessor().is_some(), "possession must be open");
    for _ in 0..8 {
        let _ = session.handle("!wait 5");
    }
    let drank = driven_facts_named(&session, "drank");
    assert!(
        drank >= 7,
        "seed 42's held body must have its drinks minuted: got {drank}"
    );
    assert_eq!(
        session.driven_mode(),
        Some(Mode::Idle),
        "the held body's mode settles to Idle at seed 42"
    );
    assert_eq!(
        session.driven_affect(),
        Some(AffectLabel::Content),
        "the felt state was already right; only the ledger moves"
    );
}

/// P2 — seed 7, progress accumulates. Before Task 2: the walk sought water
/// for 14 then 15 rooms, restarted from the origin every tick, and the body
/// read `Helpless` from day 20 with 0 `drank`. The mechanism half (the
/// column moves on the first acting wait) is asserted unconditionally and is
/// GREEN after Task 2: the held body's first seeking wait moves the column.
///
/// **The PREDICTION half is THE NULL (spec §4 P2), measured 2026-09-03 after
/// Task 2's fix:** at least one `drank` by day 36 was the preregistered bet;
/// this seed reached day 36 with `drank == 0` and `driven_affect() ==
/// Some(Helpless)`. The mechanism repair (P1: `drank == 7` at seed 42) does
/// not, by itself, guarantee this seed's walk reaches water in the window
/// measured — resuming instead of restarting narrows the search but seed 7's
/// walk apparently does not narrow it enough in 36 days. Not retuned: the
/// plan's decision rule is to record the measurement, not chase the
/// prediction.
///
/// **The window the tree pins is NARROWER than the one preregistered, and
/// deliberately.** The preregistration framed the null at day 40; the
/// assertion below measures day 36 — the seventh `!wait 5` after the opening
/// `!wait 1` — because that is where this script's loop ends. The day-40
/// zero was observed too, in the campaign close's probe log, and is not
/// asserted here. So the tripwire in the tree and the preregistered window
/// are different quantities; both read zero.
///
/// RED BEFORE TASK 2 at the first assertion: the column does not move.
#[test]
fn p2_a_held_bodys_walk_accumulates_across_ticks() {
    let world = world_at(7);
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
    assert_eq!(
        driven_facts_named(&session, "drank"),
        0,
        "THE NULL (spec §4 P2): the preregistered prediction was at least one \
         drank by day 36; measured 0 drank, driven_affect() Some(Helpless) — \
         a walk that resumes still does not reach water at seed 7 in this window"
    );
}

/// P7 — the line. Seed 7's first seeking wait names the move and does not
/// count the body among the stirred; seed 42's FIRST wait names its
/// stationary, resting minute (measured: at seed 42 the population itself
/// DOES stir on that tick, so the line reads "Time passes. You sense
/// movement…" rather than the alternate "Time passes; the world keeps its
/// shape…" — either wording still ends in the held body's own "The will
/// that holds you rests.", which is the branch this half of P7 pins); seed
/// 42's second wait names the drink; a free body's line carries no minutes.
#[test]
fn p7_the_wait_line_minutes_the_held_bodys_acts() {
    let world = world_at(7);
    let (mut session, _) = Session::start(&world, &PossessOpts::default()).expect("starts");
    let _ = session.handle("!possess");
    let _ = session.handle("!wait 1");
    let hornvale_vessel::Turn::Out(line) = session.handle("!wait 5") else {
        panic!("wait narrates")
    };
    assert!(
        line.contains("walks this body elsewhere"),
        "seed 7's first seeking wait must name the move: {line:?}"
    );
    assert!(
        !line.contains("stirred") && !line.contains("You watch") && !line.contains("You notice"),
        "a room change suppresses the arrival/departure comparison: {line:?}"
    );

    let world = world_at(42);
    let (mut session, _) = Session::start(&world, &PossessOpts::default()).expect("starts");
    let _ = session.handle("!possess");
    let hornvale_vessel::Turn::Out(first_line) = session.handle("!wait 5") else {
        panic!("wait narrates")
    };
    assert!(
        first_line.ends_with("The will that holds you rests."),
        "seed 42's first wait must still name a Holding driven walk, however \
         the population's own comings and goings are worded that tick \
         (measured: the population DOES stir here, so the line begins \
         \"Time passes. You sense movement\" rather than the stationary \
         \"the world keeps its shape\" — either way the minute suffix is the \
         thing this assertion pins): {first_line:?}"
    );
    let hornvale_vessel::Turn::Out(line) = session.handle("!wait 5") else {
        panic!("wait narrates")
    };
    assert!(
        line.contains("drinks"),
        "seed 42's second wait must name the drink: {line:?}"
    );

    let (mut free, _) = Session::start(&world, &PossessOpts::default()).expect("starts");
    let _ = free.handle("!wait 5");
    let hornvale_vessel::Turn::Out(line) = free.handle("!wait 5") else {
        panic!("wait narrates")
    };
    assert!(
        !line.contains("The will that holds you"),
        "a free body has no minutes: {line:?}"
    );
}

//! The Rack's roster tests: what the struct of arrays guarantees a caller
//! that only ever sees a live `Session`.
//!
//! **Not in the commit gate today.** A new vessel test is invisible to
//! `gate-commit` until the next green chamber job rewrites
//! `docs/timings/subfloor-roster.tsv` (that file's own header says so); this
//! module runs in the stage gate from its first commit.
//!
//! **Task 2 is byte-identical, and the proof lives next door.** The claim
//! that collapsing four parallel fields into one roster changed no observable
//! output is carried by `session_snapshot.rs`'s `v2_bytes_are_pinned`
//! (`tests/fixtures/session-seed-42.json`, run WITHOUT `REBASELINE=1`) and by
//! `the_roll.rs`'s existing order and append batteries — all of which pass
//! unchanged. This module adds what those cannot see: the columns the rack
//! introduces, which nothing in Task 2 renders.

use hornvale_vessel::{PossessOpts, PossessTarget, Session, WorldContext};

/// Seed 42, the world every session below is started over.
fn world() -> hornvale_kernel::World {
    hornvale_worldgen::build_world(
        hornvale_kernel::Seed(42),
        &Default::default(),
        hornvale_worldgen::SkyChoice::Generated,
        &Default::default(),
        &Default::default(),
    )
    .expect("seed 42 builds")
}

/// At turn 0 every slot's `position` column holds the body's own `home` — the
/// seed `Roster::push` writes, and (before any `agent-at` fact is committed)
/// exactly what `liveness::agent_position` independently folds for that body.
///
/// **This is the VIEW ≡ SCAN invariant at its first instant** (spec §3.4):
/// the column and the ledger fold must agree at every read, and turn 0 is the
/// one moment where the agreement is entirely the seed's doing. Task 3 makes
/// the tick keep it true; this pins the starting point it has to keep. The
/// driven body's own agreement is checked against `Session::position`, which
/// IS the ledger fold — the only one of the two readings this crate exposes
/// to an integration test.
///
/// MUTATION THIS MUST FAIL AGAINST: seed `position` with `body.resource`
/// instead of `body.home` in `Roster::push`
/// (`self.position.push(body.resource.clone());`).
#[test]
fn at_turn_zero_every_slot_stands_at_home() {
    let world = world();
    let (session, _) = Session::start(&world, &PossessOpts::default()).expect("starts");
    let roster = session.roster();
    assert!(
        roster.len() > 1,
        "seed 42's flagship derives a roster worth checking, got {}",
        roster.len()
    );
    assert_eq!(
        roster.positions().len(),
        roster.len(),
        "the position column has one entry per body"
    );
    for (i, body) in roster.bodies().iter().enumerate() {
        assert_eq!(
            roster.positions()[i],
            body.home,
            "slot {i} ({}) stands at its home",
            body.label
        );
    }
    assert_eq!(
        roster.positions()[roster.driven().0],
        session.position(),
        "the driven slot's column agrees with the ledger's own fold"
    );
}

/// Every column the roster holds is the same length, on a real derived
/// session rather than a hand-built fixture — the alignment `roster.rs`'s own
/// unit test proves for `push` in isolation, checked here against the append
/// sites a live `Session` actually uses (`start_held`'s settled cast and its
/// herds). `slot_of` answers every one of them.
///
/// MUTATION THIS MUST FAIL AGAINST: drop the `self.felt.push(felt);` line
/// from `Roster::push` and push it twice in `Roster::new`'s place — see the
/// report; the recorded red is `felts()` reading 0 against the 71 every other
/// column reads.
#[test]
fn a_live_sessions_columns_are_aligned() {
    let world = world();
    let (session, _) = Session::start(&world, &PossessOpts::default()).expect("starts");
    let roster = session.roster();
    let n = roster.len();
    assert!(n > 1, "seed 42's flagship derives more than one body");
    assert_eq!(roster.bodies().len(), n, "bodies");
    assert_eq!(roster.keys().len(), n, "keys");
    assert_eq!(roster.positions().len(), n, "positions");
    assert_eq!(roster.felts().len(), n, "felts");
    assert_eq!(roster.on_roll().len(), n, "on_roll");
    for (i, body) in roster.bodies().iter().enumerate() {
        assert_eq!(
            roster.slot_of(body.entity).map(|slot| slot.0),
            Some(i),
            "slot {i} ({}) answers with its own index",
            body.label
        );
    }
}

/// The driven slot names the driven body when it is NOT slot `0` — the case
/// a flagship possession cannot exercise, since `derive_npcs` hoists the
/// flagship's own body to the front and every default session drives slot 0.
///
/// A `PossessTarget::Creature` session over a body chosen from the middle of
/// the roster is what makes the assertion non-vacuous: `driven()` must be
/// that body's slot, `driven_body()` must be that body, and the mask entry
/// there must be forced on (spec §3.8: the body you are is always advanced).
///
/// MUTATION THIS MUST FAIL AGAINST: return `Slot(0)` from `Roster::driven()`.
/// Verified: `driven_body()` then reports the flagship's own body where this
/// test expects the creature it possessed.
#[test]
fn a_non_zero_driven_slot_names_its_own_body() {
    let world = world();
    let ctx = WorldContext::build(&world).expect("seed 42 builds a context");
    let (flagship, _) = Session::start_in(&ctx, &PossessOpts::default()).expect("starts");
    // A body from the middle of the roster, so neither the front nor the back
    // could pass by accident.
    let target = flagship.bodies()[flagship.bodies().len() / 2].clone();
    assert_ne!(
        target.entity,
        flagship.driven_body().entity,
        "the chosen body must not be the flagship's own, or this proves nothing"
    );
    let (session, _) = Session::start_in(
        &ctx,
        &PossessOpts {
            target: PossessTarget::Creature(target.entity),
            ..Default::default()
        },
    )
    .expect("a derived creature is possessable");
    let roster = session.roster();
    let driven = roster.driven();
    assert_ne!(driven.0, 0, "the chosen body is not slot 0");
    assert_eq!(
        roster.slot_of(target.entity),
        Some(driven),
        "the reverse index and the driven slot agree"
    );
    assert_eq!(
        roster.bodies()[driven.0].entity,
        target.entity,
        "the driven slot indexes the possessed body"
    );
    assert_eq!(
        session.driven_body().entity,
        target.entity,
        "and so does the session's own accessor"
    );
    assert!(
        roster.on_roll()[driven.0],
        "the driven body is always on the roll"
    );
}

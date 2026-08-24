//! The Hand, Task 5: the arc's acceptance test. GOAP becomes
//! `DefaultController`, player input becomes a second `Controller`
//! implementation of the same trait, and arbitration runs for every body
//! — the driven one included, which is the co-present decision (spec
//! §2.3) made mechanical.

use crate::body_fields::seed_42;
use hornvale_vessel::liveness::Mode;
use hornvale_vessel::{PossessOpts, PossessTarget, Session};

/// The Bridle's Arc II acceptance test: swap controllers and both paths
/// still produce acts. If either special-cases the other this fails.
#[test]
fn a_creature_on_player_input_and_a_body_on_goap_both_act() {
    let (world, _ctx) = seed_42();

    // A body driven by GOAP: give the player's controller nothing to say and
    // confirm the body still acts on its own drives over several ticks.
    let (mut s, _) = Session::start(&world, &PossessOpts::default()).unwrap();
    let before = s.committed_fact_count();
    s.handle("!wait 5");
    assert!(
        s.committed_fact_count() > before,
        "the world acts while the player says nothing"
    );

    // A creature on player input: drive body 1 explicitly and confirm ITS
    // entity is the subject of a committed act.
    let second = s.bodies()[1].entity;
    let (mut t, _) = Session::start(
        &world,
        &PossessOpts {
            target: PossessTarget::Creature(second),
            ..Default::default()
        },
    )
    .unwrap();
    let who = t.agent_entity();
    t.handle("go n");
    assert!(
        t.committed_agent_at_count_for(who) > 0,
        "a player-driven creature commits its OWN agent-at"
    );
}

/// Co-present (spec §2.3): the host has feelings while you ride it. This is
/// the mechanical content of that decision, and nothing else observes it
/// yet — so if this test is deleted the decision silently stops holding.
#[test]
fn a_driven_body_still_arbitrates_its_own_drives() {
    let (world, _ctx) = seed_42();
    let (mut s, _) = Session::start(&world, &PossessOpts::default()).unwrap();
    s.handle("!wait 30");
    let mode = s.driven_mode().expect("a driven body has a mode");
    assert!(
        matches!(mode, Mode::Pursuing(_) | Mode::Homing | Mode::Idle),
        "the driven body's own arbitration produced a mode: {mode:?}"
    );
}

/// The mechanical content of spec §5.2's "commits on `Do`, nothing on
/// `Hold`" argument: a driven body's own arbitration runs every `!wait` (the
/// test above), but nothing it decides ever reaches the ledger while the
/// player has queued nothing — the body waits ON THE PLAYER, not on its own
/// drives. This is the assertion the other two in this file cannot make:
/// both pass whether or not the driven body's own walk is wired in at all
/// (see the Task 5 fix-round report's first mutation proof), because neither
/// reads the DRIVEN body's own committed facts across a `!wait`. This one
/// does, and DOES redden under "the driven body removed from the loop"
/// (mutation proof 2 in the report).
///
/// **It does NOT redden under "swap the driven body's controller for
/// `DefaultController`", and that was verified empirically, not assumed.**
/// Seed 42's flagship body, walked solo (band-of-one — see
/// `DriveMovements::step_one_with_controller`'s own doc for why), reaches
/// `Mode::Pursuing(Fatigue)` with a BLOCKED proposal on its very first
/// arbitration this session ever runs, so `resolution.intent` is already
/// `Hold` before any controller is asked — `DefaultController` and
/// `PlayerController` are indistinguishable here because arbitration itself
/// never proposes a `Do` for this specific body at this specific starting
/// state. The genuine, deterministic proof that `DefaultController` is a
/// pass-through and `PlayerController` overrides a real `Do` is
/// `liveness::tests::a_default_controller_passes_through_and_a_player_controller_holds`,
/// which constructs a body it KNOWS wants to act rather than relying on
/// seed 42's own population to happen to want one.
#[test]
fn a_driven_body_does_not_act_on_its_own_drives_while_the_player_says_nothing() {
    let (world, _ctx) = seed_42();
    let (mut s, _) = Session::start(&world, &PossessOpts::default()).unwrap();
    let who = s.agent_entity();
    let before = s.committed_fact_count_for(who);
    // Long enough that seed 42's flagship body (fix round 1's mutation probe:
    // `Pursuing(Fatigue)` inside 200 days) would actually reach `Rest` under
    // real GOAP — `committed_agent_at_count_for` alone cannot see a `Rest`
    // (it never moves), so this reads every predicate the body could emit.
    s.handle("!wait 200");
    assert_eq!(
        s.committed_fact_count_for(who),
        before,
        "a driven body must not act on its own GOAP (any predicate — agent-at, \
         drank, rested, eaten) while the player says nothing (spec 2.3/5.2: it \
         waits on the player, PlayerController answers Hold)"
    );
}

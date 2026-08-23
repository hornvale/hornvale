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

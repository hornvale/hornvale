//! The Hand, Task 3: the session holds ONE roster, and the driven body is a
//! member of it rather than a twin of one (Task 2's proof made this safe).
//!
//! Task 4 extends this: possession can select any creature in the roster,
//! not only the flagship — the metaplan's "a creature on player-input" acceptance
//! test needs no new mechanism, only a way to say which one (`PossessTarget::Creature`).

use crate::body_fields::seed_42;
use hornvale_vessel::{PossessOpts, PossessTarget, Session};

#[test]
fn the_driven_body_is_a_member_of_the_roster_not_a_twin_of_one() {
    let (world, _ctx) = seed_42();
    let (session, _) =
        hornvale_vessel::Session::start(&world, &hornvale_vessel::PossessOpts::default())
            .expect("possession starts");

    let driven = session.driven_body();
    let matches: Vec<_> = session
        .bodies()
        .iter()
        .filter(|b| b.entity == driven.entity)
        .collect();
    assert_eq!(
        matches.len(),
        1,
        "the driven body appears exactly once in the roster"
    );

    // And no OTHER body shares its home and species — which is what the
    // pre-Hand duplicate looked like from the outside.
    let twins = session
        .bodies()
        .iter()
        .filter(|b| b.entity != driven.entity)
        .filter(|b| b.home == driven.home && b.species == driven.species)
        .count();
    assert_eq!(twins, 0, "no twin of the driven body stands in its home");
}

#[test]
fn the_driven_bodys_identity_is_its_creature_entity() {
    let (world, _ctx) = seed_42();
    let (session, _) = hornvale_vessel::Session::start(&world, &Default::default()).unwrap();
    assert_eq!(
        session.agent_entity(),
        session.driven_body().entity,
        "identity is the creature's, not a separate minted id"
    );
}

#[test]
fn any_creature_in_the_roster_can_be_driven() {
    let (world, _ctx) = seed_42();
    let (a, _) = Session::start(&world, &PossessOpts::default()).unwrap();
    assert!(
        a.bodies().len() >= 2,
        "precondition: seed 42 derives 2+ bodies"
    );
    let second = a.bodies()[1].entity;
    let flagship_home = a.driven_body().home.clone();
    drop(a);

    let (b, _) = Session::start(
        &world,
        &PossessOpts {
            target: PossessTarget::Creature(second),
            ..Default::default()
        },
    )
    .expect("possessing a named creature starts");

    assert_eq!(b.agent_entity(), second, "the second creature is driven");
    assert_ne!(
        b.driven_body().home,
        flagship_home,
        "and it is not the flagship — otherwise this test proves nothing"
    );
}

#[test]
fn possessing_a_creature_does_not_renumber_other_bodies_handles() {
    let (world, _ctx) = seed_42();
    let (flagship, _) = Session::start(&world, &PossessOpts::default()).unwrap();
    assert!(
        flagship.bodies().len() >= 4,
        "precondition: need a FOURTH body. Driving the second body (as the \
         sibling test above does) cannot distinguish an index-preserving \
         exclude from a two-slot swap — swapping adjacent slots 0 and 1 \
         happens to produce the same visible order as excluding index 1."
    );
    let flagship_label = flagship.driven_body().label.clone();
    let fourth = flagship.bodies()[3].entity;
    drop(flagship);

    let (b, _) = Session::start(
        &world,
        &PossessOpts {
            target: PossessTarget::Creature(fourth),
            ..Default::default()
        },
    )
    .expect("possessing the fourth roster member starts");

    // The flagship was never part of this choice, and `ordered_for_derivation`
    // hoisted it to the roster's own first slot before anyone was ever driven.
    // Its handle among "other bodies" (`!npcs`/`why`/`provoke`'s 1-based
    // numbering) must still be #1. A front-slot SWAP (the mechanism spec
    // review rejected) would instead move whichever body the fourth one
    // displaced there, silently renumbering the flagship's own handle
    // depending on which creature the player happened to choose — a
    // regression `any_creature_in_the_roster_can_be_driven` cannot see, since
    // it drives the SECOND body, where a swap and an exclude agree by
    // coincidence.
    let labels = b.npc_labels();
    assert_eq!(
        labels[0], flagship_label,
        "the flagship keeps handle #1 among the other bodies when a LATER \
         roster member is driven instead of it"
    );
}

#[test]
fn possessing_an_entity_absent_from_the_roster_fails_loudly() {
    let (world, _ctx) = seed_42();
    // An id no derivation in this session could ever mint (the ledger's ids
    // start at 1 and this session derives a small, bounded roster) — the
    // absent-entity case generation never guesses at.
    let absent = hornvale_kernel::EntityId::new(u64::MAX).unwrap();
    // `Session` does not implement `Debug` (it borrows a live world), so
    // `Result::expect_err`/`unwrap_err` (both `T: Debug`-bound) cannot be
    // used here — match the `Result` by hand instead.
    match Session::start(
        &world,
        &PossessOpts {
            target: PossessTarget::Creature(absent),
            ..Default::default()
        },
    ) {
        Ok(_) => panic!("an entity outside the derived roster must fail, not silently fall back"),
        Err(e) => assert_eq!(e, hornvale_vessel::VesselError::NoSuchCreature(absent)),
    }
}

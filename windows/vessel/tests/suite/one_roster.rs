//! The Hand, Task 3: the session holds ONE roster, and the driven body is a
//! member of it rather than a twin of one (Task 2's proof made this safe).

use crate::body_fields::seed_42;

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

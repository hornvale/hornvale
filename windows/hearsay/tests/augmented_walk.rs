//! Spec §5.1, §5.3, §5.5: the clock removes holders, the contact edge adds
//! them, and a claim reaches a people no tree route connects.

mod common;

use common::{
    a_holder_that_died_before_the_event, eid, two_peoples_joined_by_a_later_raid,
    two_peoples_joined_by_an_earlier_raid,
};
use hornvale_astronomy::units::StdDays;
use hornvale_hearsay::accumulate::Accumulation;
use hornvale_hearsay::clock::Clock;
use hornvale_hearsay::contact::{Contact, contact_of};
use hornvale_hearsay::derive::variants_about_accumulating;
use hornvale_hearsay::durations::PeopleDurations;
use hornvale_hearsay::ladder::PeopleLadders;
use hornvale_hearsay::lineage::lineage_of;
use hornvale_hearsay::transmission::{Transmission, Walk};
use hornvale_kernel::ledger::{EntityId, Ledger};
use std::collections::BTreeSet;

fn durations_for(peoples: &[&str]) -> PeopleDurations {
    let mut d = PeopleDurations::default();
    for p in peoples {
        d.insert(
            p,
            Some(StdDays::new(50.0).expect("positive")),
            Some(StdDays::new(150.0).expect("positive")),
        );
    }
    d
}

fn holders(led: &Ledger, policy: Transmission, subject: EntityId) -> BTreeSet<EntityId> {
    let lin = lineage_of(led);
    let graph = contact_of(led);
    let durations = durations_for(&["human", "kobold"]);
    let ladders = PeopleLadders::of(led, &durations);
    let walk = Walk {
        ledger: led,
        lineage: &lin,
        contact: &graph,
        policy,
    };
    variants_about_accumulating(
        &walk,
        &ladders,
        &durations,
        Accumulation::Additive,
        subject,
        hornvale_history::OCC_ENDED,
    )
    .into_iter()
    .map(|c| c.holder)
    .collect()
}

fn with(policy: Transmission, f: impl FnOnce(&mut Transmission)) -> Transmission {
    let mut p = policy;
    f(&mut p);
    p
}

#[test]
fn the_clock_removes_a_holder_that_died_before_the_event() {
    let led = a_holder_that_died_before_the_event();
    let off = holders(&led, Transmission::AS_SHIPPED, eid(1));
    let on = holders(
        &led,
        with(Transmission::AS_SHIPPED, |p| p.clock = Clock::Alive),
        eid(1),
    );

    assert!(
        off.contains(&eid(3)),
        "control: today's model DOES give the dead community the claim -- \
         if this fails the fixture is wrong, not the clock"
    );
    assert!(!on.contains(&eid(3)), "the clock refuses it");
    assert!(
        on.is_subset(&off),
        "the clock is strictly removing: {on:?} must be a subset of {off:?}"
    );
}

/// Spec §6.2's H1 mechanism, which the arm assertion above does not reach: the
/// clock removes strictly MORE than the holders that fail it, because refusing
/// a step also orphans everything below that step. 4 never ended, so it passes
/// the aliveness test on its own account — but its only route to the event runs
/// through the dead 3, and a claim nobody could tell it is a claim it does not
/// hold.
#[test]
fn refusing_a_step_orphans_the_line_below_it() {
    let led = a_holder_that_died_before_the_event();
    let off = holders(&led, Transmission::AS_SHIPPED, eid(1));
    let on = holders(
        &led,
        with(Transmission::AS_SHIPPED, |p| p.clock = Clock::Alive),
        eid(1),
    );

    assert!(
        off.contains(&eid(4)),
        "control: 4 holds the claim today, through 3"
    );
    assert!(
        !on.contains(&eid(4)),
        "4 itself is alive at the event, but its only teller is not: {on:?}"
    );
    assert_eq!(
        on,
        BTreeSet::from([eid(1)]),
        "the witness alone survives the clock on this fixture"
    );
}

#[test]
fn the_contact_edge_carries_a_claim_across_a_people_boundary() {
    let led = two_peoples_joined_by_a_later_raid();
    let descent = holders(&led, Transmission::AS_SHIPPED, eid(1));
    let contact = holders(
        &led,
        with(Transmission::AS_SHIPPED, |p| {
            p.contact = Contact::WithRaidSeam
        }),
        eid(1),
    );

    assert!(
        descent.is_subset(&contact),
        "contact is strictly adding: {descent:?} must be a subset of {contact:?}"
    );
    assert!(
        contact.len() > descent.len(),
        "the raid seam must reach somebody new; descent {} vs contact {}",
        descent.len(),
        contact.len()
    );
}

/// Spec §5.5. 5 and 6 are a separate root and its child; no founding route
/// connects either to 1, and neither witnessed 1's ending. Only the seam can
/// carry the account there.
#[test]
fn a_claim_reaches_a_people_that_no_tree_route_connects() {
    let led = two_peoples_joined_by_a_later_raid();
    let contact = holders(
        &led,
        with(Transmission::AS_SHIPPED, |p| {
            p.contact = Contact::WithRaidSeam
        }),
        eid(1),
    );
    assert!(
        contact.contains(&eid(6)),
        "the raider's child holds the victim's account: {contact:?}"
    );
    assert!(contact.contains(&eid(2)), "the human line still holds it");
}

/// Spec §5.3, condition 1: a meeting cannot carry news of something that has
/// not happened. The fixture is
/// [`two_peoples_joined_by_a_later_raid`]'s twin with the two days swapped —
/// the raid is stamped day 500 and the event under test is day 900 — so the
/// ONLY thing separating them is the contact-day comparison. Delete the
/// `>= event_day` filter in `derive.rs` and this test goes red while every
/// other test in the crate stays green (Task 5 step 6, measured).
#[test]
fn the_seam_refuses_a_raid_that_predates_the_event() {
    let led = two_peoples_joined_by_an_earlier_raid();
    let descent = holders(&led, Transmission::AS_SHIPPED, eid(1));
    let contact = holders(
        &led,
        with(Transmission::AS_SHIPPED, |p| {
            p.contact = Contact::WithRaidSeam
        }),
        eid(1),
    );

    assert_eq!(
        descent,
        BTreeSet::from([eid(1), eid(2), eid(3)]),
        "control: the human line holds it by descent"
    );
    assert_eq!(
        contact, descent,
        "an earlier raid adds nobody: {contact:?} must equal {descent:?}"
    );
    assert!(
        !contact.contains(&eid(5)) && !contact.contains(&eid(6)),
        "the kobolds met 3 four hundred days before 1 ended: {contact:?}"
    );
}

/// The walk must TERMINATE on a cyclic graph. `3 <-> 5` plus descent gives a
/// cycle the moment contact is on; a walk that re-expands a node at an equal
/// width would not drain. Spec §5.5's termination argument rests on
/// `Accumulation::step` being non-decreasing, pinned by
/// `tests/accumulate.rs::every_rule_is_non_decreasing`.
#[test]
fn the_augmented_walk_terminates_on_a_cycle() {
    let led = two_peoples_joined_by_a_later_raid();
    for rule in Accumulation::ALL {
        let lin = lineage_of(&led);
        let graph = contact_of(&led);
        let durations = durations_for(&["human", "kobold"]);
        let ladders = PeopleLadders::of(&led, &durations);
        let walk = Walk {
            ledger: &led,
            lineage: &lin,
            contact: &graph,
            policy: with(Transmission::AS_SHIPPED, |p| {
                p.contact = Contact::WithRaidSeam
            }),
        };
        let held = variants_about_accumulating(
            &walk,
            &ladders,
            &durations,
            rule,
            eid(1),
            hornvale_history::OCC_ENDED,
        );
        assert!(!held.is_empty(), "{rule:?}: the walk produced nothing");
        assert!(
            held.windows(2).all(|w| w[0].holder <= w[1].holder),
            "{rule:?}: results must stay ascending by holder"
        );
    }
}

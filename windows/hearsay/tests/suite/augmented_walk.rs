//! Spec §5.1, §5.3, §5.5: the clock removes holders, the contact edge adds
//! them, and a claim reaches a people no tree route connects.

use crate::common;

use common::{
    a_holder_that_died_before_the_event, a_round_trip_through_another_people, eid,
    two_peoples_joined_by_a_later_raid, two_peoples_joined_by_a_raid_on_the_event_day,
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
use hornvale_kernel::Claim;
use hornvale_kernel::ledger::{EntityId, Ledger, Value};
use hornvale_kernel::provenance::Provenance;
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

fn tellings(led: &Ledger, policy: Transmission, subject: EntityId) -> Vec<Claim> {
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
}

/// The one telling `holder` retained, or `None` when it holds nothing.
fn telling_of(
    led: &Ledger,
    policy: Transmission,
    subject: EntityId,
    holder: EntityId,
) -> Option<Claim> {
    tellings(led, policy, subject)
        .into_iter()
        .find(|c| c.holder == holder)
}

/// Every telling as `(holder, hops, rung)` — the shape a holder-set assertion
/// is structurally blind to.
fn shape(led: &Ledger, policy: Transmission, subject: EntityId) -> Vec<(u64, u32, u8)> {
    tellings(led, policy, subject)
        .into_iter()
        .map(|c| (c.holder.get(), c.hops, c.precision.rung()))
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

/// A holder set cannot see WHICH telling arrived, and a route-selection
/// regression moves the telling without moving the set. This pins the whole
/// walk on the contact fixture as `(holder, hops, rung)`.
///
/// The widths are re-derivable by hand against the human ladder (day 1.0,
/// moon 41.7, generation 50, lifespan 150, year 372.4) with every generation
/// 50 std days: base 1.0, then `1->2` spans 4.0 (width 5), `2->3` 4.0 (9),
/// `3->5` 6.0 (15) and `5->6` 22.0 (37). 37 is still short of the moon rung,
/// so the seam does NOT coarsen anybody here — the account crosses two peoples
/// and four hops still remembering day 500 exactly. Stating that as an
/// assertion is the point: it is the fact `a_round_trip_through_another_people`
/// then contrasts with, where the same seam DOES cost a rung.
#[test]
fn a_contact_carried_claim_arrives_with_the_seams_own_hop_count() {
    let led = two_peoples_joined_by_a_later_raid();
    let policy = with(Transmission::AS_SHIPPED, |p| {
        p.contact = Contact::WithRaidSeam
    });

    assert_eq!(
        shape(&led, policy, eid(1)),
        vec![(1, 0, 0), (2, 1, 0), (3, 2, 0), (5, 3, 0), (6, 4, 0)],
        "the seam route 1->2->3->5->6 must land hop by hop"
    );

    let six = telling_of(&led, policy, eid(1), eid(6)).expect("6 holds it");
    assert_ne!(
        six.grade,
        Provenance::Witnessed,
        "6 was not there; it heard about it"
    );
    assert_eq!(
        six.object,
        Value::Number(500.0),
        "four hops of accumulated width (37.0) still fit inside the day rung"
    );
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

/// Spec §5.3, condition 1, **at its boundary**: the condition is `e <= c`, so
/// a meeting on the event's OWN day carries the news.
///
/// The pair above straddles the comparison — 500 against 900, and 1000
/// against 500 — but never lands on it, so `derive.rs`'s `*day >= event` and
/// a wrong `*day > event` agreed on every fixture in the crate and the whole
/// suite stayed green under either (measured). This fixture puts the raid on
/// day 500 and the event on day 500, where they disagree: `>=` admits the
/// kobolds and `>` orphans them back to the descent set below.
///
/// The clock's identical boundary was pinned from the start
/// (`tests/clock.rs::ending_on_the_event_day_is_admitted`); the seam's was
/// not, and the asymmetry was the gap.
#[test]
fn the_seam_admits_a_raid_on_the_event_day() {
    let led = two_peoples_joined_by_a_raid_on_the_event_day();
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
        "control: the human line holds it by descent, exactly as in the pair above"
    );
    assert_eq!(
        contact,
        BTreeSet::from([eid(1), eid(2), eid(3), eid(5), eid(6)]),
        "a raid ON the event day is a meeting that can carry the news: {contact:?}"
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

/// Spec §5.5 and the §8 Definition of Done: **an account that leaves its
/// lineage, crosses a people boundary, and RETURNS to a descendant of its own
/// witness — arriving damaged, by a route no tree admits.**
///
/// The route is `1 -> 2 -> 5 -> 6 -> 4`: out of the human line at the raid on
/// 2, down the kobold line, and back into the human line at the raid on 6. 4
/// is a descendant of the witness 1 (through the dead 3), so the account comes
/// home — to a community that could not have been told it by any ancestor.
///
/// **Why this is not vacuous, which is the whole difficulty.** The returning
/// route is WIDER than the tree route to the same 4, so wherever the tree
/// route is open it wins the ordering and 4 keeps the tree's telling
/// — the seam would deliver nothing and an "is 4 a holder" assertion would
/// pass on the tree's work. `Clock::Alive` is what makes the seam the only
/// teller: 3 ended 200 days before the event, so the clock refuses it and
/// orphans 4 from the tree entirely. The `Descent` control below is that
/// claim, measured: under the same clock and no seam, 4 holds NOTHING.
///
/// **Arriving damaged**, positively asserted rather than assumed: the tree
/// telling is 5 hops at rung 0 remembering day 500 exactly; the returned
/// telling is 4 hops at rung 1 (the moon) remembering day 458.7. Width, by
/// hand: base 1.0, `1->2` 4.0 (5.0), `2->5` 2.0 (7.0), `5->6` 22.0 (29.0),
/// `6->4` 16.0 (45.0) — 45.0 clears the moon rung at 41.7, and
/// `floor(500 / 41.7) * 41.7 = 458.7`. The people boundary is crossed twice
/// and the rung is still resolved against the ORIGINATING witness's human
/// ladder (`KNOW-teller-ladder-at-emit`).
#[test]
fn an_account_returns_to_its_own_lineage_by_a_route_no_tree_admits() {
    let led = a_round_trip_through_another_people();
    let clock_only = with(Transmission::AS_SHIPPED, |p| p.clock = Clock::Alive);
    let round_trip = with(Transmission::AS_SHIPPED, |p| {
        p.clock = Clock::Alive;
        p.contact = Contact::WithRaidSeam;
    });

    // 4 descends from the witness, so this really is a return home and not a
    // journey to a stranger.
    let lin = lineage_of(&led);
    assert!(
        lin.is_ancestor(eid(1), eid(4)),
        "4 must be a descendant of the witness for this to be a ROUND trip"
    );

    // The control that makes the arm mean something: with the tree route
    // blocked and no seam, the account does not reach 4 at all.
    let no_seam = holders(&led, clock_only, eid(1));
    assert_eq!(
        no_seam,
        BTreeSet::from([eid(1), eid(2)]),
        "control: the clock orphans the whole line under the dead 3, so no tree \
         route to 4 survives and any 4 below arrived by the seam"
    );

    let returned = telling_of(&led, round_trip, eid(1), eid(4))
        .expect("the account comes home to 4 by way of the kobolds");
    assert_eq!(
        returned.hops, 4,
        "the seam route is four hops (1->2->5->6->4); the tree's five are refused"
    );
    assert_eq!(
        returned.precision.rung(),
        1,
        "it comes back a rung coarser than it left — 45.0 days of width clears \
         the moon at 41.7"
    );
    match returned.object {
        Value::Number(day) => assert!(
            (day - 458.7).abs() < 1e-6,
            "the returned account misremembers the day: got {day}, expected 458.7"
        ),
        other => panic!("the remembered day must stay a Number, got {other:?}"),
    }
    assert_ne!(
        returned.grade,
        Provenance::Witnessed,
        "a claim that travelled four hops is not witnessed"
    );

    // And the whole policy matrix, so each arm's contribution is separable.
    assert_eq!(
        holders(&led, round_trip, eid(1)),
        BTreeSet::from([eid(1), eid(2), eid(4), eid(5), eid(6)]),
        "clock + seam: the human line minus the dead 3, plus both kobolds"
    );
    assert_eq!(
        holders(&led, Transmission::AS_SHIPPED, eid(1)),
        BTreeSet::from([eid(1), eid(2), eid(3), eid(4), eid(7), eid(8), eid(10)]),
        "control: today's model reaches 4 the ordinary way, down the tree"
    );
}

/// The other half of Important 2, and the assertion a wrong-route regression
/// actually trips: where BOTH routes to 4 are open, the walk must keep the
/// least-corrupted one.
///
/// Under `Clock::Off` the tree route `1 -> 3 -> 7 -> 8 -> 10 -> 4` (FIVE hops,
/// width 9.0, rung 0, day 500) and the seam route `1 -> 2 -> 5 -> 6 -> 4`
/// (FOUR hops, width 45.0, rung 1, day 458.7) both reach 4. The holder SET is
/// identical either way, so only the telling can tell them apart — which is
/// why the holder-set assertions elsewhere in this file cannot see a
/// route-selection regression and these can.
///
/// **The two routes disagree about which is better, and that is the point.**
/// The seam route is SHORTER; the tree route is NARROWER. Campaign 3's rule is
/// that width decides and hops are only a tie-break, so the tree route must
/// win — and a walk that ordered by hop count (campaign 2's rule, and the
/// regression this fixture exists to catch) would keep the seam's telling and
/// redden all three assertions below. Measured, not assumed: blanking the
/// key's width component turns exactly this test red and nothing else.
#[test]
fn the_tree_route_wins_wherever_it_is_open() {
    let led = a_round_trip_through_another_people();
    let seam = with(Transmission::AS_SHIPPED, |p| {
        p.contact = Contact::WithRaidSeam
    });

    let four = telling_of(&led, seam, eid(1), eid(4)).expect("4 holds it by both routes");
    assert_eq!(
        four.hops, 5,
        "the tree route is LONGER in hops and must still win, because it is narrower"
    );
    assert_eq!(
        four.precision.rung(),
        0,
        "the tree telling never accumulates past the day rung (width 9.0)"
    );
    assert_eq!(
        four.object,
        Value::Number(500.0),
        "the tree telling remembers the day exactly; the seam telling does not"
    );

    // Substrate: the seam really is available here, so the assertions above
    // are a CHOICE the walk made and not the only option it had.
    assert!(
        holders(&led, seam, eid(1)).is_superset(&BTreeSet::from([eid(5), eid(6)])),
        "the kobolds must be reachable, or there was no rival route to reject"
    );
}

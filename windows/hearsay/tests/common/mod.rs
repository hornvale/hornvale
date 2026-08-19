//! Hand-built ledgers for the hearsay tests: these are milliseconds and pin
//! the shape exactly.
//!
//! Shared by all three test files (Tasks 4 and 5 declare `mod common;` rather
//! than copying it); not every helper is used by every consuming test
//! binary, so this module allows dead code the same way
//! `windows/vessel/tests/common/mod.rs` does.
#![allow(dead_code)]

use hornvale_kernel::ledger::{EntityId, Fact, Ledger, Value};
use hornvale_kernel::registry::ConceptRegistry;

/// `EntityId` from a small integer.
pub fn eid(n: u64) -> EntityId {
    EntityId::new(n).expect("nonzero")
}

/// A ledger holding one `occ-founded-from` per entry: `Some(parent)` is a
/// `Founding::From` edge, `None` is a `Founding::Genesis` root (Number-valued,
/// a site id — NOT an ancestor).
pub fn ledger_with(chain: &[(u64, Option<u64>)]) -> Ledger {
    let mut reg = ConceptRegistry::default();
    reg.register_predicate(hornvale_history::OCC_FOUNDED_FROM, true, "founding")
        .expect("register");
    let mut led = Ledger::default();
    for (child, parent) in chain {
        let object = match parent {
            Some(p) => Value::Entity(eid(*p)),
            None => Value::Number(7449.0),
        };
        led.commit(
            Fact {
                subject: eid(*child),
                predicate: hornvale_history::OCC_FOUNDED_FROM.to_string(),
                object,
                place: None,
                day: None,
                provenance: "test".to_string(),
            },
            &reg,
        )
        .expect("commit");
    }
    led
}

/// Commit one extra fact onto an existing ledger, registering its predicate.
pub fn put(led: &mut Ledger, subject: u64, predicate: &str, object: Value) {
    let mut reg = ConceptRegistry::default();
    reg.register_predicate(predicate, true, "test predicate")
        .expect("register");
    led.commit(
        Fact {
            subject: eid(subject),
            predicate: predicate.to_string(),
            object,
            place: None,
            day: None,
            provenance: "test".to_string(),
        },
        &reg,
    )
    .expect("commit");
}

/// Commit one extra fact onto an existing ledger, registering its predicate
/// as NON-FUNCTIONAL — unlike [`put`], which registers functional. Several
/// facts (e.g. one `moon-period-std` per moon) can then land on one subject
/// without the registry rejecting the second as a contradiction.
pub fn put_on(led: &mut Ledger, subject: u64, predicate: &str, object: Value) {
    let mut reg = ConceptRegistry::default();
    reg.register_predicate(predicate, false, "test predicate")
        .expect("register");
    led.commit(
        Fact {
            subject: eid(subject),
            predicate: predicate.to_string(),
            object,
            place: None,
            day: None,
            provenance: "test".to_string(),
        },
        &reg,
    )
    .expect("commit");
}

/// A single descent chain of six occupations (1 -> 2 -> 3 -> 4 -> 5 -> 6),
/// each `occ-people` `"human"`, with widening (roughly doubling)
/// `occ-founded` gaps, an `occ-ended` on the root, and a real sky (day
/// length, one moon, a year — the same trio `tests/derive.rs`'s stance-
/// boundary tests commit on entity 9) so a [`hornvale_hearsay::ladder::PrecisionLadder`]
/// has astronomical rungs regardless of which social durations a caller
/// supplies on top.
///
/// Built for Task 5's accumulating derivation, which needs founding days and
/// a people on every occupation to produce any generational span at all.
/// `ledger_with` alone cannot serve it: it commits only `occ-founded-from`,
/// so [`hornvale_hearsay::amplitude::gen_span`] would read no founding day
/// anywhere and return `0.0` on every step, leaving every claim at
/// `Precision::FINEST` no matter what the accumulation rule does with it.
pub fn chain_with_foundings() -> Ledger {
    let mut led = ledger_with(&[
        (1, None),
        (2, Some(1)),
        (3, Some(2)),
        (4, Some(3)),
        (5, Some(4)),
        (6, Some(5)),
    ]);
    for (occ, day) in [
        (1, 0.0),
        (2, 200.0),
        (3, 600.0),
        (4, 1400.0),
        (5, 3000.0),
        (6, 6200.0),
    ] {
        put(
            &mut led,
            occ,
            hornvale_history::OCC_FOUNDED,
            Value::Number(day),
        );
    }
    for occ in [1u64, 2, 3, 4, 5, 6] {
        put(
            &mut led,
            occ,
            hornvale_history::OCC_PEOPLE,
            Value::Text("human".to_string()),
        );
    }
    put(
        &mut led,
        1,
        hornvale_history::OCC_ENDED,
        Value::Number(6300.0),
    );
    put_on(
        &mut led,
        9,
        hornvale_astronomy::facts::DAY_LENGTH_STD,
        Value::Number(1.0),
    );
    put_on(
        &mut led,
        9,
        hornvale_astronomy::facts::MOON_PERIOD_STD,
        Value::Number(41.7),
    );
    put_on(
        &mut led,
        9,
        hornvale_astronomy::facts::YEAR_LENGTH_STD,
        Value::Number(372.4),
    );
    led
}

/// Three occupations (1 -> 2 -> 3) where 2 is founded on EXACTLY the day 1
/// ends, so `witnesses_of` (spec §6.1: survivors who fled and refounded)
/// makes both 1 and 2 witnesses of `occ-ended` — and 3, founded later from 2,
/// is then reachable by two genuinely different paths: the long route
/// `1 -> 2 -> 3` (2 hops, through witness 1) and the short route `2 -> 3` (1
/// hop, directly from witness 2). Round-review fix round 1: the existing
/// `chain_with_foundings` is a single unbranched chain, so
/// `variants_about_accumulating`'s multi-path merge (`derive.rs`'s
/// `best.get(&d)` match) was never exercised by any Task 5 test — every
/// descendant had exactly one path to it. This fixture is a NEW function
/// beside it rather than a modification, because Task 5's committed tests
/// pin `chain_with_foundings`'s exact day values.
///
/// The founding days are chosen so the two routes' accumulated widths land
/// on DIFFERENT rungs under `Accumulation::Additive` with a 50-day
/// generation and the same day/moon/year sky as `chain_with_foundings`
/// (rungs ascending: day=1, moon=41.7, generation=50, year=372.4): the long
/// route's extra `1 -> 2` step (span 2.0) pushes its final width to 43.0,
/// past the moon rung (41.7) to rung 1; the short route's single `2 -> 3`
/// step alone lands at 41.0, short of the moon rung, at rung 0. If both
/// routes gave the same width or landed on the same rung, a merge bug that
/// kept the wrong one would be undetectable.
pub fn chain_with_a_survivor_shortcut() -> Ledger {
    let mut led = ledger_with(&[(1, None), (2, Some(1)), (3, Some(2))]);
    for (occ, day) in [(1, 0.0), (2, 100.0), (3, 2100.0)] {
        put(
            &mut led,
            occ,
            hornvale_history::OCC_FOUNDED,
            Value::Number(day),
        );
    }
    for occ in [1u64, 2, 3] {
        put(
            &mut led,
            occ,
            hornvale_history::OCC_PEOPLE,
            Value::Text("human".to_string()),
        );
    }
    // 1 ends on day 100, the same day 2 was founded -- 2 is a survivor, not
    // an inheritor, so it becomes a second witness.
    put(
        &mut led,
        1,
        hornvale_history::OCC_ENDED,
        Value::Number(100.0),
    );
    put_on(
        &mut led,
        9,
        hornvale_astronomy::facts::DAY_LENGTH_STD,
        Value::Number(1.0),
    );
    put_on(
        &mut led,
        9,
        hornvale_astronomy::facts::MOON_PERIOD_STD,
        Value::Number(41.7),
    );
    put_on(
        &mut led,
        9,
        hornvale_astronomy::facts::YEAR_LENGTH_STD,
        Value::Number(372.4),
    );
    led
}

/// Two peoples joined only by a raid, built so a claim can travel to a people
/// that DID NOT WITNESS the event it is about.
///
/// **The shape is load-bearing and a naive version of it does not work.**
/// [`hornvale_hearsay::derive::witnesses_of`] already seeds an ending's named
/// attacker as a hop-0 witness, so for the raid's OWN ending the raider's line
/// holds the account under plain descent and a contact edge adds nothing. That
/// is not a bug — it is spec §3.3's measured fact that every foreign-attacker
/// ending already reaches two peoples. What no tree can do is carry an account
/// of an event the other people were absent for. So the claim under test is
/// the ending of `1`, which only the human line witnessed, and the seam is a
/// LATER raid on its descendant.
///
/// - 1 (human) root, founded day 0, **ended day 500 with NO `occ-ended-by`** —
///   the event under test; witness set is `{1}` alone.
/// - 2 (human) child of 1, founded day 200.
/// - 3 (human) child of 2, founded day 400, **ended day 1000, raided by 5** —
///   this is the contact, and it is not the event under test.
/// - 5 (kobold) root, founded day 100 — the raider, never ends.
/// - 6 (kobold) child of 5, founded day 1200.
///
/// Under `Contact::Descent` the account of 1's ending reaches `{1, 2, 3}` and
/// stops at the people boundary. Under `Contact::WithRaidSeam` the edge
/// `3 <-> 5` (stamped day 1000, which is `>=` the event's day 500) carries it
/// to `{1, 2, 3, 5, 6}`. Neither 5 nor 6 has any founding route to 1.
///
/// Note 2 is founded on day 200 and 3 on day 400, NEITHER equal to 1's ending
/// day of 500, so `witnesses_of`'s survivor rule does not fire and they are
/// inheritors rather than co-witnesses. That is deliberate: a survivor would
/// hold at hop 0 and mask the walk under test.
pub fn two_peoples_joined_by_a_later_raid() -> Ledger {
    two_peoples_joined_by_a_raid(500.0, 1000.0)
}

/// The same two peoples, joined by a raid that happened BEFORE the event under
/// test — the negative control for spec §5.3's contact-day condition.
///
/// `1` ends on day 900 and the raid that puts `3` in contact with `5` is
/// stamped day 500, so the meeting cannot carry news of something that had not
/// happened yet. Under `Contact::WithRaidSeam` the holders must therefore be
/// exactly what descent gives, and `5`/`6` must stay out. Without the
/// `>= event_day` condition on the seam this fixture is indistinguishable from
/// [`two_peoples_joined_by_a_later_raid`], which is precisely what
/// `tests/augmented_walk.rs` pins.
///
/// `3` ends on day 500, before `1`'s ending on day 900, and still inherits the
/// claim — that is the same 1.19% `Clock::Off` admits everywhere and is not
/// what this fixture is about.
pub fn two_peoples_joined_by_an_earlier_raid() -> Ledger {
    two_peoples_joined_by_a_raid(900.0, 500.0)
}

/// The same two peoples again, joined by a raid stamped EXACTLY the event's
/// own day — the seam's day condition **at its boundary**.
///
/// Spec §5.3 writes the condition as `e <= c`, so a meeting on the day the
/// event happened is admitted. `derive.rs`'s `tellable` implements that as
/// `*day >= event`, and the boundary is the one place `>=` and `>` disagree:
/// the pair
/// [`two_peoples_joined_by_a_later_raid`] / [`two_peoples_joined_by_an_earlier_raid`]
/// straddles the comparison but never lands on it, so the crate was green
/// under either operator (measured). The clock's identical boundary already
/// had `tests/clock.rs::ending_on_the_event_day_is_admitted`; this is the
/// seam's missing counterpart.
pub fn two_peoples_joined_by_a_raid_on_the_event_day() -> Ledger {
    two_peoples_joined_by_a_raid(500.0, 500.0)
}

/// The shared body of the two contact fixtures: `1`'s ending falls on
/// `event_day` and the raid on its descendant `3` falls on `raid_day`, so the
/// only thing that varies between them is the order of those two days.
fn two_peoples_joined_by_a_raid(event_day: f64, raid_day: f64) -> Ledger {
    let mut led = ledger_with(&[
        (1, None),
        (2, Some(1)),
        (3, Some(2)),
        (5, None),
        (6, Some(5)),
    ]);
    for (occ, day) in [(1, 0.0), (2, 200.0), (3, 400.0), (5, 100.0), (6, 1200.0)] {
        put(
            &mut led,
            occ,
            hornvale_history::OCC_FOUNDED,
            Value::Number(day),
        );
    }
    for (occ, people) in [
        (1, "human"),
        (2, "human"),
        (3, "human"),
        (5, "kobold"),
        (6, "kobold"),
    ] {
        put(
            &mut led,
            occ,
            hornvale_history::OCC_PEOPLE,
            Value::Text(people.to_string()),
        );
    }
    // The event under test: 1 ends with no named attacker, so its witness set
    // is itself alone and no raider is seeded at hop 0.
    put(
        &mut led,
        1,
        hornvale_history::OCC_ENDED,
        Value::Number(event_day),
    );
    // The contact: a raid on 1's descendant, by another people.
    put(
        &mut led,
        3,
        hornvale_history::OCC_ENDED,
        Value::Number(raid_day),
    );
    put(
        &mut led,
        3,
        hornvale_history::OCC_ENDED_BY,
        Value::Entity(eid(5)),
    );
    put_on(
        &mut led,
        9,
        hornvale_astronomy::facts::DAY_LENGTH_STD,
        Value::Number(1.0),
    );
    put_on(
        &mut led,
        9,
        hornvale_astronomy::facts::MOON_PERIOD_STD,
        Value::Number(41.7),
    );
    put_on(
        &mut led,
        9,
        hornvale_astronomy::facts::YEAR_LENGTH_STD,
        Value::Number(372.4),
    );
    led
}

/// A dead community: 3 is founded from 1 on day 10 and ends on day 20, long
/// before 1's own ending on day 1000. Under `Clock::Off` it inherits a claim
/// about an event 980 days after it ceased to exist; under `Clock::Alive` it
/// does not. Spec §3.5 measured 1,959 such holders on the real substrate.
///
/// 4 is founded from 3 on day 15 and never ends, so it passes the aliveness
/// test on its own account and its ONLY route to the event runs through the
/// dead 3. It is the fixture's half that exercises orphaning (spec §6.2's
/// H1): refusing a step removes the hearer *and* everything below it.
pub fn a_holder_that_died_before_the_event() -> Ledger {
    let mut led = ledger_with(&[(1, None), (3, Some(1)), (4, Some(3))]);
    for (occ, day) in [(1, 0.0), (3, 10.0), (4, 15.0)] {
        put(
            &mut led,
            occ,
            hornvale_history::OCC_FOUNDED,
            Value::Number(day),
        );
    }
    for occ in [1u64, 3, 4] {
        put(
            &mut led,
            occ,
            hornvale_history::OCC_PEOPLE,
            Value::Text("human".to_string()),
        );
    }
    put(
        &mut led,
        3,
        hornvale_history::OCC_ENDED,
        Value::Number(20.0),
    );
    put(
        &mut led,
        1,
        hornvale_history::OCC_ENDED,
        Value::Number(1000.0),
    );
    put_on(
        &mut led,
        9,
        hornvale_astronomy::facts::DAY_LENGTH_STD,
        Value::Number(1.0),
    );
    put_on(
        &mut led,
        9,
        hornvale_astronomy::facts::MOON_PERIOD_STD,
        Value::Number(41.7),
    );
    put_on(
        &mut led,
        9,
        hornvale_astronomy::facts::YEAR_LENGTH_STD,
        Value::Number(372.4),
    );
    led
}

/// A ROUND TRIP: an account that leaves its lineage, crosses a people
/// boundary, and comes back to a descendant of its own witness by a route no
/// tree admits (spec §5.5, and the §8 Definition of Done).
///
/// **The blocked tree route is the load-bearing half, and without it the
/// fixture proves nothing.** A returning contact route is always longer and
/// wider than the tree route to the same descendant, so under any policy that
/// leaves the tree route open the tree route WINS the ordering and the
/// returning claim is discarded — a test that only checked "4 holds it" would
/// pass while measuring the tree. So `3` — 4's only parent — is dead before
/// the event, and the round trip is asserted under `Clock::Alive`, where the
/// clock refuses `3` and orphans `4` from the tree entirely.
///
/// - 1 (human) root, founded day 0, **ended day 500 with NO `occ-ended-by`** —
///   the event under test; witness set is `{1}` alone.
/// - 2 (human) child of 1, founded day 200, **ended day 1000, raided by 5** —
///   the outbound seam.
/// - 3 (human) child of 1, founded day 100, **ended day 300** — dead 200 days
///   BEFORE the event, so `Clock::Alive` refuses it.
/// - 7, 8, 10 (human) a close-founded line under 3 (days 150, 200, 250).
/// - 4 (human) child of 10, founded day 400, never ends — alive at the event on
///   its own account, reachable on the tree only through the dead 3.
/// - 5 (kobold) root, founded day 100.
/// - 6 (kobold) child of 5, founded day 1200, **ended day 2000, raided by 4** —
///   the inbound seam, back into the human line.
///
/// **The tree line under 3 is deliberately LONG AND NARROW, and that is a
/// second load-bearing choice.** Its four foundings are 50 days apart — one
/// human generation each — so the tree route to 4 is FIVE hops but only 9.0
/// days of accumulated width, against the seam's FOUR hops and 45.0. The two
/// routes therefore disagree about which is better depending on whether you
/// order by width or by hop count, which is exactly the distinction campaign 3
/// introduced ("hop count alone no longer orders two tellings"). A walk that
/// ordered by hops would keep the seam's telling at 4 and
/// `tests/augmented_walk.rs::the_tree_route_wins_wherever_it_is_open` would go
/// red — measured, not assumed.
///
/// Four policies, four distinct holder sets — which is what makes each arm's
/// contribution separable:
///
/// | clock | contact | holders | why |
/// |---|---|---|---|
/// | `Off` | `Descent` | `{1, 2, 3, 4, 7, 8, 10}` | today's model: the whole human line |
/// | `Off` | `WithRaidSeam` | + `{5, 6}` | both peoples, but 4 keeps its TREE telling |
/// | `Alive` | `Descent` | `{1, 2}` | 3 is refused and the line below it is orphaned |
/// | `Alive` | `WithRaidSeam` | `{1, 2, 4, 5, 6}` | 4 is back, and only the seam could have told it |
///
/// The round trip is `1 -> 2 -> 5 -> 6 -> 4`: out of the human line at the
/// raid on 2, down the kobold line, and back at the raid on 6 — four hops,
/// against the tree's five, and it arrives at a coarser rung remembering a
/// different day.
pub fn a_round_trip_through_another_people() -> Ledger {
    let mut led = ledger_with(&[
        (1, None),
        (2, Some(1)),
        (3, Some(1)),
        (7, Some(3)),
        (8, Some(7)),
        (10, Some(8)),
        (4, Some(10)),
        (5, None),
        (6, Some(5)),
    ]);
    for (occ, day) in [
        (1, 0.0),
        (2, 200.0),
        (3, 100.0),
        (7, 150.0),
        (8, 200.0),
        (10, 250.0),
        (4, 400.0),
        (5, 100.0),
        (6, 1200.0),
    ] {
        put(
            &mut led,
            occ,
            hornvale_history::OCC_FOUNDED,
            Value::Number(day),
        );
    }
    for (occ, people) in [
        (1, "human"),
        (2, "human"),
        (3, "human"),
        (7, "human"),
        (8, "human"),
        (10, "human"),
        (4, "human"),
        (5, "kobold"),
        (6, "kobold"),
    ] {
        put(
            &mut led,
            occ,
            hornvale_history::OCC_PEOPLE,
            Value::Text(people.to_string()),
        );
    }
    // The event under test.
    put(
        &mut led,
        1,
        hornvale_history::OCC_ENDED,
        Value::Number(500.0),
    );
    // The outbound seam: the kobolds raid 2 long after the event.
    put(
        &mut led,
        2,
        hornvale_history::OCC_ENDED,
        Value::Number(1000.0),
    );
    put(
        &mut led,
        2,
        hornvale_history::OCC_ENDED_BY,
        Value::Entity(eid(5)),
    );
    // The blocked tree route: 3 is already gone when 1 ends.
    put(
        &mut led,
        3,
        hornvale_history::OCC_ENDED,
        Value::Number(300.0),
    );
    // The inbound seam: 4 raids the kobolds' 6, and hears about its own
    // ancestor's ending from them.
    put(
        &mut led,
        6,
        hornvale_history::OCC_ENDED,
        Value::Number(2000.0),
    );
    put(
        &mut led,
        6,
        hornvale_history::OCC_ENDED_BY,
        Value::Entity(eid(4)),
    );
    put_on(
        &mut led,
        9,
        hornvale_astronomy::facts::DAY_LENGTH_STD,
        Value::Number(1.0),
    );
    put_on(
        &mut led,
        9,
        hornvale_astronomy::facts::MOON_PERIOD_STD,
        Value::Number(41.7),
    );
    put_on(
        &mut led,
        9,
        hornvale_astronomy::facts::YEAR_LENGTH_STD,
        Value::Number(372.4),
    );
    led
}

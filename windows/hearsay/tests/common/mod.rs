//! Hand-built ledgers for the hearsay tests: these are milliseconds and pin
//! the shape exactly.
//!
//! Shared by all three test files (Tasks 4 and 5 declare `mod common;` rather
//! than copying it); not every helper is used by every consuming test
//! binary, so this module allows dead code the same way
//! `windows/vessel/tests/common/mod.rs` does.
#![allow(dead_code)]

use hornvale_astronomy::units::StdDays;
use hornvale_hearsay::accumulate::{Accumulation, precision_at};
use hornvale_hearsay::amplitude::gen_span;
use hornvale_hearsay::contact::{Contact, ContactGraph, contact_of};
use hornvale_hearsay::derive::witnesses_of;
use hornvale_hearsay::durations::PeopleDurations;
use hornvale_hearsay::ladder::{PeopleLadders, PrecisionLadder};
use hornvale_hearsay::lineage::{Lineage, lineage_of};
use hornvale_kernel::Claim;
use hornvale_kernel::Precision;
use hornvale_kernel::ledger::{EntityId, Fact, Ledger, Value};
use hornvale_kernel::registry::ConceptRegistry;
use std::collections::{BTreeMap, BTreeSet};

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

/// A world whose sky puts four rungs where a fraction of a day of accumulated
/// width is visible as a change of rung. Committed by
/// [`two_peoples_with_raid_count`] and by nothing else.
///
/// The rungs it produces, ascending (`PrecisionLadder::with_social` sorts by
/// actual length, so the social rungs interleave):
/// `0: day 1.0 | 1: 3.5 | 2: 9.8 | 3: 10.1 | 4: 10.6 | 5: generation 50 |
///  6: lifespan 150 | 7: year 372.4`.
///
/// The four "moons" are a hand-built instrument, not a claim about any real
/// sky: spec §5.1's crossing penalty is `span(FINEST) / (1 + edges)`, which is
/// at most one day wide, and a ladder whose next rung after the day is 41.7
/// days (the shape every other fixture here uses) cannot resolve it at all.
/// Each rung earns its place against a specific way the penalty could be
/// wrong:
///
/// - **9.8, 10.1, 10.6** straddle the width at holder `5` for 19, 3 and 1 raid
///   edges (9.75, 9.95, 10.2 days against 9.7 unpenalised), which is what makes
///   the rung a strictly decreasing function of contact.
/// - **3.5** straddles the width at holder `2` (3.0 days) with one full rung of
///   penalty added (4.0). Holder `2` is reached by a step WITHIN one people, so
///   this rung is what notices a penalty charged on a same-people step. It was
///   added after measurement: with the ladder's second rung at 5.0 instead,
///   deleting the same-people guard in `derive.rs::crossing_penalty` left
///   `tests/crossing.rs::a_step_within_one_people_pays_nothing` GREEN — the
///   guard read as tested and was not.
fn commit_a_four_moon_sky(led: &mut Ledger) {
    put_on(
        led,
        9,
        hornvale_astronomy::facts::DAY_LENGTH_STD,
        Value::Number(1.0),
    );
    for period in [3.5, 9.8, 10.1, 10.6] {
        put_on(
            led,
            9,
            hornvale_astronomy::facts::MOON_PERIOD_STD,
            Value::Number(period),
        );
    }
    put_on(
        led,
        9,
        hornvale_astronomy::facts::YEAR_LENGTH_STD,
        Value::Number(372.4),
    );
}

/// Two peoples joined by a raid seam, with **the number of raid edges between
/// those two peoples** as the only parameter — the fixture family spec §5.1's
/// crossing penalty is measured against.
///
/// The transmission shape is [`two_peoples_joined_by_a_later_raid`]'s, for the
/// reason recorded there: the event under test is `1`'s ending, which only the
/// human line witnessed, and the seam is a LATER raid on its descendant, so
/// the account genuinely has to cross a people boundary to reach a kobold.
///
/// - 1 (human) root, founded day 0, **ended day 503 with no `occ-ended-by`** —
///   the event; witness set is `{1}` alone.
/// - 2 (human) child of 1, founded day 100.
/// - 3 (human) child of 2, founded day 250, **ended day 1000, raided by 5** —
///   the seam, and the FIRST of the `raids` human/kobold edges.
/// - 5 (kobold) root, founded day 435 — the raider.
/// - 6 (kobold) child of 5, founded day 1000.
///
/// **`raids` cannot be zero, and that is a fact about the substrate rather
/// than about this helper.** A step whose teller and hearer are different
/// peoples is always a seam edge (fission never crosses a people boundary —
/// see `windows/hearsay/src/amplitude.rs`), and a seam edge between peoples
/// `a` and `b` is itself counted in `edges_between(a, b)`. So a crossing that
/// happens at all has at least one edge under it, and the cheapest reachable
/// crossing costs `span(FINEST) / 2`, never the full rung. The full-rung case
/// has its own fixture, [`a_people_boundary_no_raid_has_ever_crossed`].
///
/// The extra `raids - 1` edges are raids on human occupations `21..` by the
/// kobold `90`, all of them founding-tree ROOTS that no telling ever reaches:
/// they move `edges_between("human", "kobold")` and nothing else. That the
/// walk is otherwise untouched is not assumed — `tests/crossing.rs` asserts it
/// directly, by requiring identical output across every member of the family
/// under [`hornvale_hearsay::transmission::Crossing::Free`].
///
/// The founding days are chosen so the accumulated width at `5` — the first
/// holder across the seam — lands at 9.7 days before any penalty, with the
/// four-moon ladder's rungs at 9.8, 10.1 and 10.6 straddling the penalty at
/// 19, 3 and 1 edges. Every margin is at least 0.05 days, which is fourteen
/// orders of magnitude above the `f64` error in sums this small.
fn two_peoples_with_raid_count(raids: u64) -> Ledger {
    assert!(
        raids >= 1,
        "the seam itself is a human/kobold raid, so the pair always has at \
         least one edge -- see this function's doc comment"
    );
    let mut chain: Vec<(u64, Option<u64>)> = vec![
        (1, None),
        (2, Some(1)),
        (3, Some(2)),
        (5, None),
        (6, Some(5)),
    ];
    for i in 1..raids {
        chain.push((20 + i, None));
    }
    if raids > 1 {
        chain.push((90, None));
    }
    let mut led = ledger_with(&chain);

    for (occ, day) in [(1, 0.0), (2, 100.0), (3, 250.0), (5, 435.0), (6, 1000.0)] {
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
    // is itself alone. Day 503 rather than a round number so that snapping it
    // to different rungs yields different remembered days.
    put(
        &mut led,
        1,
        hornvale_history::OCC_ENDED,
        Value::Number(503.0),
    );
    // The seam: a raid on 1's descendant, by the other people, on a day after
    // the event (spec §5.3's contact-day condition).
    put(
        &mut led,
        3,
        hornvale_history::OCC_ENDED,
        Value::Number(1000.0),
    );
    put(
        &mut led,
        3,
        hornvale_history::OCC_ENDED_BY,
        Value::Entity(eid(5)),
    );

    // The remaining human/kobold edges, between occupations no telling reaches.
    if raids > 1 {
        put(
            &mut led,
            90,
            hornvale_history::OCC_PEOPLE,
            Value::Text("kobold".to_string()),
        );
        put(
            &mut led,
            90,
            hornvale_history::OCC_FOUNDED,
            Value::Number(0.0),
        );
        for i in 1..raids {
            let victim = 20 + i;
            put(
                &mut led,
                victim,
                hornvale_history::OCC_PEOPLE,
                Value::Text("human".to_string()),
            );
            put(
                &mut led,
                victim,
                hornvale_history::OCC_FOUNDED,
                Value::Number(0.0),
            );
            put(
                &mut led,
                victim,
                hornvale_history::OCC_ENDED,
                Value::Number(600.0 + i as f64),
            );
            put(
                &mut led,
                victim,
                hornvale_history::OCC_ENDED_BY,
                Value::Entity(eid(90)),
            );
        }
    }

    commit_a_four_moon_sky(&mut led);
    led
}

/// Near-strangers: the human line and the kobolds have met exactly once, and
/// that once is the seam the account crosses. The most expensive crossing the
/// bake can actually produce.
pub fn two_peoples_joined_by_one_raid() -> Ledger {
    two_peoples_with_raid_count(1)
}

/// The same two peoples, having met three times.
pub fn two_peoples_joined_by_three_raids() -> Ledger {
    two_peoples_with_raid_count(3)
}

/// The same two peoples again, now old adversaries: nineteen recorded raids,
/// so spec §5.1's penalty is a twentieth of what a single meeting costs.
pub fn two_peoples_joined_by_nineteen_raids() -> Ledger {
    two_peoples_with_raid_count(19)
}

/// A people boundary that NO raid has ever crossed — the `edges_between == 0`
/// branch of spec §5.1's `1 + edges` denominator, and the only fixture here
/// that reaches it.
///
/// **Constructed, and not producible by the bake.** It puts the boundary on a
/// DESCENT edge (`2` is human, its child `3` is drow), which
/// `windows/hearsay/src/amplitude.rs` records as never happening in real data
/// — zero of 780 typed edges. It exists because the zero denominator is
/// otherwise unreachable and is still worth pinning two things about: that a
/// pair with no shared history pays the full finest rung, and that the `1 +`
/// is load-bearing. Drop it and the division yields `+inf`, which
/// `Accumulation::step` refuses as non-finite and passes the width through
/// UNCHANGED — so the most expensive crossing in the model would silently
/// become the only free one.
///
/// One moon at 6.5 days, so the accumulated width at `3` — 6.0 without the
/// penalty, 7.0 with it — straddles a rung.
pub fn a_people_boundary_no_raid_has_ever_crossed() -> Ledger {
    let mut led = ledger_with(&[(1, None), (2, Some(1)), (3, Some(2))]);
    for (occ, day) in [(1, 0.0), (2, 100.0), (3, 250.0)] {
        put(
            &mut led,
            occ,
            hornvale_history::OCC_FOUNDED,
            Value::Number(day),
        );
    }
    for (occ, people) in [(1, "human"), (2, "human"), (3, "drow")] {
        put(
            &mut led,
            occ,
            hornvale_history::OCC_PEOPLE,
            Value::Text(people.to_string()),
        );
    }
    put(
        &mut led,
        1,
        hornvale_history::OCC_ENDED,
        Value::Number(503.0),
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
        Value::Number(6.5),
    );
    put_on(
        &mut led,
        9,
        hornvale_astronomy::facts::YEAR_LENGTH_STD,
        Value::Number(372.4),
    );
    led
}

// ===========================================================================
// THE TOUCHSTONE CONTROL MACHINERY.
//
// Extracted verbatim from `touchstone_controls_probe.rs` (Task 1, itself
// ported from `probe_tiebreak_rules.rs`) so both that probe and
// `touchstone_readout.rs` (Task 4) share ONE copy of the enumeration/selection
// model rather than each carrying its own. The per-step arithmetic is
// `derive.rs`'s verbatim (same `gen_span`, `Accumulation::step`,
// `precision_at`, cumulative `ladder.apply`); the shipped ordering key is the
// selection each rule tie-breaks against. Nothing here is new logic — the move
// is byte-preserving, and `touchstone_controls_probe`'s frozen numbers are the
// control that proves it.
// ===========================================================================

/// The 12-seed panel: a strict prefix of the census panel and of every earlier
/// Myth campaign's readout panel, so nothing here samples a population an
/// earlier instrument did not.
pub const PANEL: [u64; 12] = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11];

/// The predicate every claim here is about — only an ending has parties beyond
/// its subject, so it is the only predicate a transmission graph exists over.
pub const PREDICATE: &str = hornvale_history::OCC_ENDED;

/// Node expansions one ending's enumeration may spend before it is declared
/// capped — the backstop `probe_tiebreak_rules.rs` carries, so a future world
/// with a denser seam fails loudly rather than slowly.
pub const MAX_EXPANSIONS: u64 = 4_000_000;

/// Route length one ending's enumeration may reach before it is declared
/// capped.
pub const MAX_DEPTH: u32 = 2_048;

/// The accumulation rule the divergence aggregate is defined under, and the one
/// this machinery measures the positive control against — matching
/// `parley_readout.rs`'s frozen §6.5 measure.
pub const RULE: Accumulation = Accumulation::Multiplicative;

// ---------------------------------------------------------------------------
// THE SELECTION RULES. Ported verbatim from `probe_tiebreak_rules.rs`.
// ---------------------------------------------------------------------------

/// Which telling a holder keeps, out of every telling that reached it. All are
/// people-blind and stateless; what separates them is only which feature wins.
///
/// Arm A of the positive control is the shipped walk itself (today's rule,
/// smallest final width → fewest hops → witness), so this enum carries only the
/// ALTERNATIVE rules — arm-B candidates. `least_damage_key`/`primacy_key` below
/// are the shipped ordering, used as each alternative's own tie-break exactly as
/// in `probe_tiebreak_rules.rs`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Selection {
    /// The telling that arrived first: fewest hops, damage only as a tie-break.
    Primacy,
    /// The remembered value the most distinct tellings agree on, one vote per
    /// arriving telling.
    Frequency,
    /// The same vote, one per distinct WITNESS rather than per arriving telling.
    FrequencyByWitness,
    /// The telling that arrived last: most hops, then least damage. Well-defined
    /// here only because routes are restricted to simple paths.
    Recency,
}

impl Selection {
    /// The alternatives to today's rule — arm-B candidates for the positive
    /// control, in a fixed order so the readout's rows are stable.
    pub const ALTERNATIVES: [Selection; 4] = [
        Selection::Primacy,
        Selection::Frequency,
        Selection::FrequencyByWitness,
        Selection::Recency,
    ];

    /// This rule's short name, used as a readout row label.
    pub fn label(self) -> &'static str {
        match self {
            Selection::Primacy => "primacy",
            Selection::Frequency => "frequency",
            Selection::FrequencyByWitness => "frequency-w",
            Selection::Recency => "recency",
        }
    }
}

/// One telling that reached a holder, deduplicated to its observable content.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Telling {
    /// Accumulated damage width at this holder, as `f64::to_bits`.
    pub width_bits: u64,
    /// Retellings between the originating witness and this holder.
    pub hops: u32,
    /// The ORIGINATING witness. Fixes the ladder for the whole route.
    pub witness: EntityId,
    /// The remembered day, as `f64::to_bits`.
    pub day_bits: u64,
    /// The rung this holder remembers the day at.
    pub rung: u8,
    /// Seam traversals on the route that delivered this telling.
    pub crossings: u32,
    /// How many distinct simple routes delivered exactly this telling.
    pub routes: u32,
}

impl Telling {
    /// The shipped ordering key: smallest width, then fewest hops, then
    /// smallest witness.
    pub fn least_damage_key(&self) -> (u64, u32, EntityId) {
        (self.width_bits, self.hops, self.witness)
    }

    /// Primacy's key: fewest hops, then smallest width, then smallest witness.
    pub fn primacy_key(&self) -> (u32, u64, EntityId) {
        (self.hops, self.width_bits, self.witness)
    }
}

/// Apply one selection rule to one holder's candidate set. `cands` is non-empty
/// and ascending by [`Telling::least_damage_key`].
pub fn select(rule: Selection, cands: &[Telling]) -> Telling {
    let least = |xs: &[Telling]| {
        *xs.iter()
            .min_by_key(|t| t.least_damage_key())
            .expect("a holder's candidate set is never empty")
    };
    match rule {
        Selection::Primacy => *cands
            .iter()
            .min_by_key(|t| t.primacy_key())
            .expect("a holder's candidate set is never empty"),
        Selection::Recency => *cands
            .iter()
            .max_by_key(|t| (t.hops, std::cmp::Reverse(t.least_damage_key())))
            .expect("a holder's candidate set is never empty"),
        Selection::Frequency | Selection::FrequencyByWitness => {
            let mut votes: BTreeMap<u64, usize> = BTreeMap::new();
            if rule == Selection::Frequency {
                for t in cands {
                    *votes.entry(t.day_bits).or_default() += t.routes as usize;
                }
            } else {
                let mut per_witness: BTreeMap<EntityId, Telling> = BTreeMap::new();
                for t in cands {
                    per_witness
                        .entry(t.witness)
                        .and_modify(|b| {
                            if t.least_damage_key() < b.least_damage_key() {
                                *b = *t;
                            }
                        })
                        .or_insert(*t);
                }
                for t in per_witness.values() {
                    *votes.entry(t.day_bits).or_default() += 1;
                }
            }
            let best_day = votes
                .iter()
                .map(|(day, n)| {
                    let rep = least(
                        &cands
                            .iter()
                            .copied()
                            .filter(|t| t.day_bits == *day)
                            .collect::<Vec<_>>(),
                    );
                    (*n, rep)
                })
                .max_by(|a, b| {
                    a.0.cmp(&b.0)
                        .then(b.1.least_damage_key().cmp(&a.1.least_damage_key()))
                })
                .expect("votes is non-empty when cands is");
            best_day.1
        }
    }
}

// ---------------------------------------------------------------------------
// THE ENUMERATOR. Ported verbatim from `probe_tiebreak_rules.rs`.
// ---------------------------------------------------------------------------

/// Everything one ending's enumeration needs, assembled once per world.
pub struct Enumerator<'a> {
    /// The committed ledger.
    pub led: &'a Ledger,
    /// The founding tree.
    pub lineage: &'a Lineage,
    /// The SHIPPED raid seam.
    pub contact: &'a ContactGraph,
    /// Per-people generation and lifespan, in std days.
    pub durations: &'a PeopleDurations,
}

/// One ending's whole candidate set, plus whether it is trustworthy.
#[derive(Default)]
pub struct Candidates {
    /// holder -> every telling that reached it, ascending by the shipped key.
    pub per_holder: BTreeMap<EntityId, Vec<Telling>>,
    /// Whether a bound bound.
    pub capped: bool,
}

/// The mutable half of one enumeration.
struct Accum {
    /// (holder, telling) -> route count.
    seen: BTreeMap<EntityId, BTreeMap<Telling, u32>>,
    /// Expansions spent so far.
    expansions: u64,
    /// Whether a bound bound.
    capped: bool,
}

/// One node's state on the route currently being walked.
#[derive(Clone, Copy)]
struct Step {
    /// Who currently holds the telling.
    node: EntityId,
    /// Accumulated damage width at full precision.
    width: f64,
    /// The day as this node remembers it.
    day: f64,
    /// The rung `day` currently sits at.
    rung: Precision,
    /// Seam traversals behind this node.
    crossings: u32,
    /// Retellings behind this node.
    hops: u32,
}

impl<'a> Enumerator<'a> {
    /// Who `node` may tell under `arm`. `derive.rs`'s private `tellable`, minus
    /// the clock (every arm here runs `Clock::Off`).
    fn tellable(
        &self,
        arm: Contact,
        node: EntityId,
        event_day: Option<f64>,
    ) -> Vec<(EntityId, bool)> {
        let mut out: Vec<(EntityId, bool)> = self
            .lineage
            .children_of(node)
            .iter()
            .map(|c| (*c, false))
            .collect();
        if arm == Contact::WithRaidSeam {
            for (peer, day) in self.contact.peers_of(node) {
                if event_day.is_none_or(|event| *day >= event) {
                    out.push((*peer, true));
                }
            }
        }
        out.sort();
        out.dedup_by_key(|(id, _)| *id);
        out
    }

    /// Every telling that reaches every holder, over simple paths from every
    /// witness. The per-step arithmetic is the shipped relaxation's, verbatim.
    pub fn enumerate(
        &self,
        ladders: &PeopleLadders,
        rule: Accumulation,
        arm: Contact,
        subject: EntityId,
    ) -> Candidates {
        let led = self.led;
        let Some(Value::Number(event)) = led.value_of(subject, PREDICATE) else {
            return Candidates::default();
        };
        let event = *event;
        let witnesses = witnesses_of(led, self.lineage, subject, PREDICATE);
        let witness_set: BTreeSet<EntityId> = witnesses.iter().copied().collect();

        let mut acc = Accum {
            seen: BTreeMap::new(),
            expansions: 0,
            capped: false,
        };
        for w in &witnesses {
            let people = match led.value_of(*w, hornvale_history::OCC_PEOPLE) {
                Some(Value::Text(p)) => p.clone(),
                _ => String::new(),
            };
            let ladder = ladders.for_people(&people);
            let width = ladder
                .span(Precision::FINEST)
                .map(|days| days.get())
                .unwrap_or(0.0);
            let mut visited: BTreeSet<EntityId> = BTreeSet::new();
            visited.insert(*w);
            self.descend(
                &mut acc,
                &witness_set,
                ladder,
                rule,
                arm,
                Some(event),
                *w,
                Step {
                    node: *w,
                    width,
                    day: event,
                    rung: Precision::FINEST,
                    hops: 0,
                    crossings: 0,
                },
                &mut visited,
            );
        }

        let mut per_holder: BTreeMap<EntityId, Vec<Telling>> = BTreeMap::new();
        for (holder, rows) in acc.seen {
            let mut v: Vec<Telling> = rows
                .into_iter()
                .map(|(mut t, routes)| {
                    t.routes = routes;
                    t
                })
                .collect();
            v.sort();
            per_holder.insert(holder, v);
        }
        Candidates {
            per_holder,
            capped: acc.capped,
        }
    }

    /// Walk every simple path out of `step.node`.
    #[allow(clippy::too_many_arguments)]
    fn descend(
        &self,
        acc: &mut Accum,
        witness_set: &BTreeSet<EntityId>,
        ladder: &PrecisionLadder,
        rule: Accumulation,
        arm: Contact,
        event_day: Option<f64>,
        witness: EntityId,
        step: Step,
        visited: &mut BTreeSet<EntityId>,
    ) {
        acc.expansions += 1;
        if acc.expansions > MAX_EXPANSIONS {
            acc.capped = true;
            return;
        }
        let telling = Telling {
            width_bits: step.width.to_bits(),
            hops: step.hops,
            witness,
            day_bits: step.day.to_bits(),
            rung: step.rung.rung(),
            crossings: step.crossings,
            routes: 0,
        };
        *acc.seen
            .entry(step.node)
            .or_default()
            .entry(telling)
            .or_default() += 1;

        if step.hops >= MAX_DEPTH {
            acc.capped = true;
            return;
        }
        for (hearer, is_seam) in self.tellable(arm, step.node, event_day) {
            if witness_set.contains(&hearer) {
                continue; // a witness is never demoted to an inheritor
            }
            if visited.contains(&hearer) {
                continue; // simple paths only
            }
            let next_width = rule.step(
                step.width,
                gen_span(self.led, self.durations, step.node, hearer),
            );
            let rung = precision_at(ladder, next_width);
            let next = Step {
                node: hearer,
                width: next_width,
                day: ladder.apply(rung, step.day),
                rung,
                hops: step.hops + 1,
                crossings: step.crossings + u32::from(is_seam),
            };
            visited.insert(hearer);
            self.descend(
                acc,
                witness_set,
                ladder,
                rule,
                arm,
                event_day,
                witness,
                next,
                visited,
            );
            visited.remove(&hearer);
            if acc.expansions > MAX_EXPANSIONS {
                return;
            }
        }
    }
}

// ---------------------------------------------------------------------------
// World reading. Ported from `probe_tiebreak_rules.rs::read_world`.
// ---------------------------------------------------------------------------

/// One ending, with the two peoples the seam puts on either side of it.
pub struct Ending {
    /// The occupation that ended.
    pub subject: EntityId,
    /// The subject's own people, empty when it names none.
    pub people: String,
    /// The people of its `occ-ended-by` attacker, when it names one.
    pub attacker_people: Option<String>,
}

impl Ending {
    /// Whether the attacker is of another people — the cross-people population.
    pub fn is_foreign(&self) -> bool {
        self.attacker_people
            .as_ref()
            .is_some_and(|ap| !ap.is_empty() && !self.people.is_empty() && *ap != self.people)
    }
}

/// Every ending in a ledger, ascending by subject.
pub fn endings_of(led: &Ledger) -> Vec<Ending> {
    let mut out = Vec::new();
    for fact in led.find(PREDICATE) {
        let subject = fact.subject;
        if !matches!(
            led.value_of(subject, hornvale_history::OCC_ENDED),
            Some(Value::Number(_))
        ) {
            continue;
        }
        let people = match led.value_of(subject, hornvale_history::OCC_PEOPLE) {
            Some(Value::Text(p)) => p.clone(),
            _ => String::new(),
        };
        let attacker_people = match led.value_of(subject, hornvale_history::OCC_ENDED_BY) {
            Some(Value::Entity(a)) => match led.value_of(*a, hornvale_history::OCC_PEOPLE) {
                Some(Value::Text(p)) => Some(p.clone()),
                _ => None,
            },
            _ => None,
        };
        out.push(Ending {
            subject,
            people,
            attacker_people,
        });
    }
    out.sort_by_key(|e| e.subject);
    out.dedup_by_key(|e| e.subject);
    out
}

/// One world, assembled once and shared by every arm.
pub struct WorldRead {
    /// The founding tree.
    pub lineage: Lineage,
    /// The shipped raid seam.
    pub contact: ContactGraph,
    /// Per-people generation and lifespan, in std days.
    pub durations: PeopleDurations,
    /// One ladder per people.
    pub ladders: PeopleLadders,
    /// Which people each occupation belongs to.
    pub people_of: BTreeMap<EntityId, String>,
    /// Every ending in the world, ascending by subject.
    pub endings: Vec<Ending>,
}

/// Assemble one world. `None` when it offers no year rung — reported as skipped.
pub fn read_world(
    led: &Ledger,
    components: &hornvale_worldgen::WorldComponents,
) -> Option<WorldRead> {
    let lineage = lineage_of(led);
    let astronomical = PrecisionLadder::of(led);
    let year_days = astronomical
        .labels()
        .iter()
        .position(|label| *label == "year")
        .and_then(|i| astronomical.span(Precision(i as u8)))
        .map(|span| span.get())?;

    let mut people_of: BTreeMap<EntityId, String> = BTreeMap::new();
    for occ in lineage.all() {
        if let Some(Value::Text(people)) = led.value_of(occ, hornvale_history::OCC_PEOPLE) {
            people_of.insert(occ, people.clone());
        }
    }

    let mut durations = PeopleDurations::default();
    let named: BTreeSet<&String> = people_of.values().collect();
    for people in named {
        let Some(bio) = components.biosphere.get_by_label(people) else {
            continue;
        };
        let life = hornvale_species::life_history(bio.mass, bio.metabolic_class, bio.schedule);
        let to_days = |years: hornvale_kernel::Years| StdDays::new(years.get() * year_days).ok();
        durations.insert(
            people,
            life.generation_length.and_then(to_days),
            life.lifespan.and_then(to_days),
        );
    }

    let ladders = PeopleLadders::of(led, &durations);
    Some(WorldRead {
        lineage,
        contact: contact_of(led),
        durations,
        ladders,
        people_of,
        endings: endings_of(led),
    })
}

/// Build one panel world at full depth.
pub fn build(seed: u64) -> hornvale_kernel::World {
    hornvale_worldgen::build_world(
        hornvale_kernel::Seed(seed),
        &hornvale_astronomy::SkyPins::default(),
        hornvale_worldgen::SkyChoice::Generated,
        &hornvale_terrain::TerrainPins::default(),
        &hornvale_worldgen::SettlementPins::default(),
    )
    .expect("panel seed builds")
}

/// `n` as a percentage of `d`, or `0.0` when `d` is zero.
pub fn pct(n: usize, d: usize) -> f64 {
    100.0 * n as f64 / d.max(1) as f64
}

/// The remembered days each named people holds, under one selection's picks.
pub fn by_people(
    people_of: &BTreeMap<EntityId, String>,
    held: &BTreeMap<EntityId, u64>,
) -> BTreeMap<String, BTreeSet<u64>> {
    let mut out: BTreeMap<String, BTreeSet<u64>> = BTreeMap::new();
    for (holder, day_bits) in held {
        let Some(p) = people_of.get(holder) else {
            continue;
        };
        out.entry(p.clone()).or_default().insert(*day_bits);
    }
    out
}

/// The remembered day a claim carries, as bits. `None` for a non-`Number`
/// object, which an ending never has.
pub fn day_bits_of(c: &Claim) -> Option<u64> {
    match &c.object {
        Value::Number(day) => Some(day.to_bits()),
        _ => None,
    }
}

/// Whether every occupation in `chain` shares one `occ-people` value — the
/// structural identification of the NEGATIVE control's sub-population.
///
/// Reads the people directly (`derive::people_of` is private): an unlabelled
/// occupation is the single nameless people `""`, so two of them compare EQUAL,
/// mirroring `derive.rs`.
pub fn people_homogeneous(led: &Ledger, chain: &[EntityId]) -> bool {
    let people = |occ: EntityId| match led.value_of(occ, hornvale_history::OCC_PEOPLE) {
        Some(Value::Text(p)) => p.clone(),
        _ => String::new(),
    };
    let mut it = chain.iter().map(|o| people(*o));
    match it.next() {
        None => true,
        Some(first) => it.all(|p| p == first),
    }
}

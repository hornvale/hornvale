mod common;

use common::{chain_with_a_survivor_shortcut, chain_with_foundings, eid, ledger_with, put, put_on};
use hornvale_astronomy::units::StdDays;
use hornvale_hearsay::contact::contact_of;
use hornvale_hearsay::derive::{claims_about, variants_about, witnesses_of};
use hornvale_hearsay::ladder::PrecisionLadder;
use hornvale_hearsay::lineage::lineage_of;
use hornvale_hearsay::transmission::{Transmission, Walk};
use hornvale_hearsay::{divergent_witnesses, echo_ratio};
use hornvale_kernel::Precision;
use hornvale_kernel::ledger::{Ledger, Value};
use hornvale_kernel::provenance::Provenance;

/// 1 (root) ends on day 100; 2 founded from 1; 3 founded from 2. A second,
/// unrelated lineage (10 root, 11 founded from 10) exists so
/// `an_unrelated_lineage_holds_nothing_about_it` has a real cross-lineage
/// holder to prove absent, rather than an entity id nothing ever committed.
fn world() -> Ledger {
    let mut led = ledger_with(&[
        (1, None),
        (2, Some(1)),
        (3, Some(2)),
        (10, None),
        (11, Some(10)),
    ]);
    put(
        &mut led,
        1,
        hornvale_history::OCC_ENDED,
        Value::Number(100.0),
    );
    led
}

#[test]
fn the_subject_of_an_event_witnessed_it() {
    let led = world();
    let lin = lineage_of(&led);
    let claims = claims_about(&led, &lin, eid(1), hornvale_history::OCC_ENDED);
    let own = claims
        .iter()
        .find(|c| c.holder == eid(1))
        .expect("self holds it");
    assert_eq!(own.grade, Provenance::Witnessed);
    assert_eq!(own.hops, 0);
}

#[test]
fn descendants_inherit_it_taught_with_growing_hops() {
    let led = world();
    let lin = lineage_of(&led);
    let claims = claims_about(&led, &lin, eid(1), hornvale_history::OCC_ENDED);
    let two = claims
        .iter()
        .find(|c| c.holder == eid(2))
        .expect("2 holds it");
    let three = claims
        .iter()
        .find(|c| c.holder == eid(3))
        .expect("3 holds it");
    assert_eq!(two.grade, Provenance::Taught);
    assert_eq!(two.hops, 1);
    assert_eq!(three.grade, Provenance::Taught);
    assert_eq!(three.hops, 2);
}

#[test]
fn content_is_carried_unchanged() {
    let led = world();
    let lin = lineage_of(&led);
    for c in claims_about(&led, &lin, eid(1), hornvale_history::OCC_ENDED) {
        assert_eq!(c.object, Value::Number(100.0), "distortion is campaign 2");
        assert_eq!(c.subject, eid(1));
    }
}

#[test]
fn an_unrelated_lineage_holds_nothing_about_it() {
    let led = world();
    let lin = lineage_of(&led);
    let claims = claims_about(&led, &lin, eid(1), hornvale_history::OCC_ENDED);
    let holders: Vec<_> = claims.iter().map(|c| c.holder).collect();
    // The real lineage (1's own event, inherited by its descendants 2 and 3)
    // is present...
    assert!(holders.contains(&eid(1)));
    assert!(holders.contains(&eid(2)));
    assert!(holders.contains(&eid(3)));
    // ...and the second, genuinely unrelated lineage rooted at 10 is not —
    // 10 and 11 are real, committed entities that never descend from 1.
    assert!(!holders.contains(&eid(10)));
    assert!(!holders.contains(&eid(11)));
}

/// A raid: village 1 (founded day 0) is ended on day 100 by village 50.
/// Survivors found 2 and 3 on day 100 — they were there. Village 4 was
/// founded from 1 back on day 10 and was elsewhere when it happened.
/// Village 51 is founded from the attacker 50, later. Village 6 is founded
/// from survivor-witness 2, later still: it is reachable from witness 1 at
/// hops=2 (through 2) and from witness 2 at hops=1 directly — the nearer
/// telling, from 2, is the one that must win.
fn raid() -> Ledger {
    let mut led = ledger_with(&[
        (1, None),
        (50, None),
        (2, Some(1)),
        (3, Some(1)),
        (4, Some(1)),
        (51, Some(50)),
        (6, Some(2)),
    ]);
    for (occ, day) in [
        (1, 0.0),
        (50, 0.0),
        (2, 100.0),
        (3, 100.0),
        (4, 10.0),
        (51, 200.0),
        (6, 150.0),
    ] {
        put(
            &mut led,
            occ,
            hornvale_history::OCC_FOUNDED,
            Value::Number(day),
        );
    }
    put(
        &mut led,
        1,
        hornvale_history::OCC_ENDED,
        Value::Number(100.0),
    );
    put(
        &mut led,
        1,
        hornvale_history::OCC_ENDED_BY,
        Value::Entity(eid(50)),
    );
    led
}

#[test]
fn the_victim_the_survivors_and_the_attacker_all_witnessed_it() {
    let led = raid();
    let lin = lineage_of(&led);
    let w = witnesses_of(&led, &lin, eid(1), hornvale_history::OCC_ENDED);
    assert_eq!(w, vec![eid(1), eid(2), eid(3), eid(50)]);
}

#[test]
fn a_child_founded_before_the_ending_was_elsewhere() {
    let led = raid();
    let lin = lineage_of(&led);
    let w = witnesses_of(&led, &lin, eid(1), hornvale_history::OCC_ENDED);
    assert!(
        !w.contains(&eid(4)),
        "4 was founded on day 10, not at the ending"
    );
}

#[test]
fn a_survivor_is_a_witness_not_an_inheritor() {
    // The trap: 2 is a DESCENDANT of 1 and would otherwise inherit at hops 1.
    // It saw the raid. Witness wins over inheritance.
    let led = raid();
    let lin = lineage_of(&led);
    let claims = claims_about(&led, &lin, eid(1), hornvale_history::OCC_ENDED);
    let two = claims
        .iter()
        .find(|c| c.holder == eid(2))
        .expect("2 holds it");
    assert_eq!(two.grade, Provenance::Witnessed);
    assert_eq!(two.hops, 0);
}

#[test]
fn the_ordinary_child_inherits_at_one_hop() {
    let led = raid();
    let lin = lineage_of(&led);
    let claims = claims_about(&led, &lin, eid(1), hornvale_history::OCC_ENDED);
    let four = claims
        .iter()
        .find(|c| c.holder == eid(4))
        .expect("4 holds it");
    assert_eq!(four.grade, Provenance::Taught);
    assert_eq!(four.hops, 1);
}

#[test]
fn the_attackers_line_holds_it_too() {
    let led = raid();
    let lin = lineage_of(&led);
    let claims = claims_about(&led, &lin, eid(1), hornvale_history::OCC_ENDED);
    let fifty = claims
        .iter()
        .find(|c| c.holder == eid(50))
        .expect("50 holds it");
    let fifty_one = claims
        .iter()
        .find(|c| c.holder == eid(51))
        .expect("51 holds it");
    assert_eq!(fifty.grade, Provenance::Witnessed);
    assert_eq!(fifty.hops, 0);
    assert_eq!(fifty_one.grade, Provenance::Taught);
    assert_eq!(fifty_one.hops, 1);
}

#[test]
fn a_holder_reachable_from_two_witnesses_takes_the_nearer_one() {
    // 6 is founded from survivor-witness 2, so it is reachable from witness 2
    // at hops=1 directly, AND from witness 1 at hops=2 (1 -> 2 -> 6, since 2
    // is also 1's descendant). The nearer telling — hops=1, from 2 — must be
    // the one held, regardless of which witness's walk reaches 6 first.
    let led = raid();
    let lin = lineage_of(&led);
    let claims = claims_about(&led, &lin, eid(1), hornvale_history::OCC_ENDED);
    let six = claims
        .iter()
        .find(|c| c.holder == eid(6))
        .expect("6 holds it");
    assert_eq!(six.grade, Provenance::Taught);
    assert_eq!(six.hops, 1);
}

#[test]
fn a_non_ending_predicate_has_only_its_subject_as_witness() {
    let led = raid();
    let lin = lineage_of(&led);
    let w = witnesses_of(&led, &lin, eid(1), hornvale_history::OCC_FOUNDED);
    assert_eq!(w, vec![eid(1)], "only an ending has other parties");
}

#[test]
fn echo_ratio_is_witnesses_over_holders() {
    let led = raid();
    let lin = lineage_of(&led);
    // Witnesses: 1, 2, 3, 50. Holders: those plus 4, 6, 51 = 7.
    let holders = claims_about(&led, &lin, eid(1), hornvale_history::OCC_ENDED).len();
    let witnesses = witnesses_of(&led, &lin, eid(1), hornvale_history::OCC_ENDED).len();
    assert_eq!((witnesses, holders), (4, 7));
    let r = echo_ratio(&led, &lin, eid(1), hornvale_history::OCC_ENDED).expect("qualifies");
    assert!((r - 4.0 / 7.0).abs() < 1e-12, "got {r}");
}

#[test]
fn echo_ratio_is_absent_below_three_holders() {
    // A lone root with an ending and no descendants: one holder, no ratio.
    let mut led = ledger_with(&[(1, None)]);
    put(
        &mut led,
        1,
        hornvale_history::OCC_FOUNDED,
        Value::Number(0.0),
    );
    put(&mut led, 1, hornvale_history::OCC_ENDED, Value::Number(5.0));
    let lin = lineage_of(&led);
    assert_eq!(
        echo_ratio(&led, &lin, eid(1), hornvale_history::OCC_ENDED),
        None
    );
}

#[test]
fn divergent_witnesses_drops_a_witness_descended_from_another() {
    let led = raid();
    let lin = lineage_of(&led);
    let w = witnesses_of(&led, &lin, eid(1), hornvale_history::OCC_ENDED);
    // 2 and 3 descend from 1, which is also a witness; 50 does not.
    let d = divergent_witnesses(&lin, &w);
    assert_eq!(d, vec![eid(1), eid(50)]);
}

#[test]
fn two_unrelated_witnesses_are_both_divergent() {
    let led = ledger_with(&[(1, None), (50, None)]);
    let lin = lineage_of(&led);
    assert_eq!(
        divergent_witnesses(&lin, &[eid(1), eid(50)]),
        vec![eid(1), eid(50)]
    );
}

#[test]
fn a_chain_of_one_stance_carries_the_day_unchanged() {
    // 1 ends; 2 and 3 descend from it. No attacker, so every holder in the
    // subtree is VictimLine -- one stance throughout, nothing lossy.
    let mut led = ledger_with(&[(2, Some(1)), (3, Some(2))]);
    put(
        &mut led,
        1,
        hornvale_history::OCC_ENDED,
        Value::Number(745.0),
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
    let lin = lineage_of(&led);
    let graph = contact_of(&led);
    let walk = Walk {
        ledger: &led,
        lineage: &lin,
        contact: &graph,
        policy: Transmission::AS_SHIPPED,
    };
    let ladder = PrecisionLadder::of(&led);
    let vs = variants_about(&walk, &ladder, eid(1), hornvale_history::OCC_ENDED);
    assert!(!vs.is_empty(), "the subtree holds the claim");
    for v in &vs {
        assert_eq!(v.precision, Precision::FINEST, "no stance change: {v:?}");
        assert_eq!(v.object, Value::Number(745.0));
    }
}

#[test]
fn crossing_a_stance_boundary_coarsens_the_day() {
    // 4 raided 1, so 4 is Perpetrator and 1's line is VictimLine. A retelling
    // from 4 into that line crosses stances and must lose a rung.
    let mut led = ledger_with(&[(2, Some(1)), (4, None), (5, Some(4))]);
    put(
        &mut led,
        1,
        hornvale_history::OCC_ENDED,
        Value::Number(745.0),
    );
    put(
        &mut led,
        1,
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
    let lin = lineage_of(&led);
    let graph = contact_of(&led);
    let walk = Walk {
        ledger: &led,
        lineage: &lin,
        contact: &graph,
        policy: Transmission::AS_SHIPPED,
    };
    let ladder = PrecisionLadder::of(&led);
    let vs = variants_about(&walk, &ladder, eid(1), hornvale_history::OCC_ENDED);
    // 5 descends from the perpetrator 4, so its telling crossed a stance
    // boundary and MUST have lost a rung. Asserted unconditionally: an
    // earlier draft guarded this behind `if let Some(v) = five && v.precision
    // != FINEST`, which passes silently if 5 stops holding the claim at all
    // or stops being coarsened -- exactly the two regressions this test is
    // named to catch.
    let v = vs
        .iter()
        .find(|v| v.holder == eid(5))
        .expect("5 descends from the perpetrator and must hold the claim");
    assert_ne!(
        v.precision,
        Precision::FINEST,
        "crossing a stance boundary must coarsen: {v:?}"
    );
    // And content must agree with the rung it claims -- not a hand-computed
    // number, which would pin the ladder's arithmetic rather than the walk's.
    assert_eq!(
        v.object,
        Value::Number(ladder.apply(v.precision, 745.0)),
        "a coarsened claim's object must match its own rung"
    );
    assert_ne!(
        v.object,
        Value::Number(745.0),
        "and it must actually differ from the truth it descends from"
    );
}

#[test]
fn precision_and_object_never_disagree_anywhere() {
    // The invariant that matters across the whole population: every holder's
    // object is SOME ancestor-value snapped to that holder's own rung. A
    // claim whose precision says "year" but whose object carries a day is
    // incoherent regardless of which path produced it.
    let mut led = ledger_with(&[(2, Some(1)), (3, Some(2)), (4, None), (5, Some(4))]);
    put(
        &mut led,
        1,
        hornvale_history::OCC_ENDED,
        Value::Number(745.0),
    );
    put(
        &mut led,
        1,
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
    let lin = lineage_of(&led);
    let graph = contact_of(&led);
    let walk = Walk {
        ledger: &led,
        lineage: &lin,
        contact: &graph,
        policy: Transmission::AS_SHIPPED,
    };
    let ladder = PrecisionLadder::of(&led);
    for v in variants_about(&walk, &ladder, eid(1), hornvale_history::OCC_ENDED) {
        let Value::Number(d) = v.object else {
            panic!("occ-ended is Number-valued")
        };
        assert_eq!(
            d,
            ladder.apply(v.precision, d),
            "{:?} holds {d} at rung {:?}, which is not snapped to that rung",
            v.holder,
            v.precision
        );
    }
}

#[test]
fn an_empty_ladder_leaves_every_claim_at_finest() {
    // A world with no committed sky has no rungs, so a lossy step has nowhere
    // to descend. It must be a no-op, not a panic and not a silent change.
    let mut led = ledger_with(&[(2, Some(1)), (4, None)]);
    put(
        &mut led,
        1,
        hornvale_history::OCC_ENDED,
        Value::Number(745.0),
    );
    put(
        &mut led,
        1,
        hornvale_history::OCC_ENDED_BY,
        Value::Entity(eid(4)),
    );
    let lin = lineage_of(&led);
    let graph = contact_of(&led);
    let walk = Walk {
        ledger: &led,
        lineage: &lin,
        contact: &graph,
        policy: Transmission::AS_SHIPPED,
    };
    let ladder = PrecisionLadder::of(&led);
    assert!(ladder.is_empty());
    for v in variants_about(&walk, &ladder, eid(1), hornvale_history::OCC_ENDED) {
        assert_eq!(v.precision, Precision::FINEST);
        assert_eq!(v.object, Value::Number(745.0));
    }
}

#[test]
fn witnesses_hold_first_hand_and_are_never_demoted() {
    // Campaign 1's rule, preserved: a survivor saw the raid; it does not
    // merely hear about it from the village it fled.
    let mut led = ledger_with(&[(2, Some(1))]);
    put(
        &mut led,
        1,
        hornvale_history::OCC_ENDED,
        Value::Number(745.0),
    );
    put(
        &mut led,
        2,
        hornvale_history::OCC_FOUNDED,
        Value::Number(745.0),
    );
    let lin = lineage_of(&led);
    let graph = contact_of(&led);
    let walk = Walk {
        ledger: &led,
        lineage: &lin,
        contact: &graph,
        policy: Transmission::AS_SHIPPED,
    };
    let ladder = PrecisionLadder::of(&led);
    let vs = variants_about(&walk, &ladder, eid(1), hornvale_history::OCC_ENDED);
    for w in [eid(1), eid(2)] {
        let v = vs.iter().find(|v| v.holder == w).expect("witness holds");
        assert_eq!(v.hops, 0, "{w:?} is a witness");
        assert_eq!(v.grade, Provenance::Witnessed);
        assert_eq!(v.precision, Precision::FINEST);
    }
}

#[test]
fn accumulating_distortion_can_pass_more_than_one_rung() {
    // A chain deep enough that campaign 2's boundary model would stop at one
    // rung. The property under test is that the accumulating model does not.
    let led = chain_with_foundings();
    let lin = lineage_of(&led);
    let graph = contact_of(&led);
    let walk = Walk {
        ledger: &led,
        lineage: &lin,
        contact: &graph,
        policy: Transmission::AS_SHIPPED,
    };
    let mut d = hornvale_hearsay::durations::PeopleDurations::default();
    d.insert(
        "human",
        Some(StdDays::new(50.0).expect("positive")),
        Some(StdDays::new(500.0).expect("positive")),
    );
    let ladders = hornvale_hearsay::ladder::PeopleLadders::of(&led, &d);

    let out = hornvale_hearsay::derive::variants_about_accumulating(
        &walk,
        &ladders,
        &d,
        hornvale_hearsay::accumulate::Accumulation::Additive,
        eid(1),
        hornvale_history::OCC_ENDED,
    );
    let deepest = out
        .iter()
        .map(|c| c.precision.rung())
        .max()
        .expect("claims");
    assert!(
        deepest >= 2,
        "expected multi-rung accumulation, got {deepest}"
    );
}

#[test]
fn every_rule_preserves_precision_rank_monotonicity_along_a_path() {
    // On a SINGLE chain, hop count orders the holders, so precision must be
    // non-decreasing in hops. Asserting `rung() >= 0` would be vacuous —
    // `rung()` returns u8 — and a vacuous guard is the exact failure campaign
    // 2's retrospective is about.
    let led = chain_with_foundings();
    let lin = lineage_of(&led);
    let graph = contact_of(&led);
    let walk = Walk {
        ledger: &led,
        lineage: &lin,
        contact: &graph,
        policy: Transmission::AS_SHIPPED,
    };
    let mut d = hornvale_hearsay::durations::PeopleDurations::default();
    d.insert("human", Some(StdDays::new(50.0).expect("positive")), None);
    let ladders = hornvale_hearsay::ladder::PeopleLadders::of(&led, &d);

    for rule in hornvale_hearsay::accumulate::Accumulation::ALL {
        let mut out = hornvale_hearsay::derive::variants_about_accumulating(
            &walk,
            &ladders,
            &d,
            rule,
            eid(1),
            hornvale_history::OCC_ENDED,
        );
        out.sort_by_key(|c| c.hops);
        let mut seen = 0u8;
        for c in &out {
            assert!(
                c.precision.rung() >= seen,
                "{}: precision went FINER at hop {} ({} < {})",
                rule.label(),
                c.hops,
                c.precision.rung(),
                seen
            );
            seen = c.precision.rung();
        }
        // Positive control: the assertion above is only meaningful if the
        // precision actually moves. If nothing coarsens, this test proves
        // nothing about monotonicity.
        assert!(
            seen > 0,
            "{}: no coarsening happened at all — the test is vacuous",
            rule.label()
        );
    }
}

#[test]
fn a_holder_reachable_by_two_paths_keeps_the_least_corrupted_telling() {
    // 2 is a survivor of 1's ending, so both 1 and 2 are witnesses; 3, founded
    // later from 2, is reachable both the long way (1 -> 2 -> 3, 2 hops) and
    // the short way (2 -> 3, 1 hop, directly from witness 2). The short route
    // skips the 1 -> 2 step entirely, so its accumulated width is strictly
    // smaller -- it must be the telling 3 ends up holding.
    let led = chain_with_a_survivor_shortcut();
    let lin = lineage_of(&led);
    let graph = contact_of(&led);
    let walk = Walk {
        ledger: &led,
        lineage: &lin,
        contact: &graph,
        policy: Transmission::AS_SHIPPED,
    };
    let mut d = hornvale_hearsay::durations::PeopleDurations::default();
    d.insert("human", Some(StdDays::new(50.0).expect("positive")), None);
    let ladders = hornvale_hearsay::ladder::PeopleLadders::of(&led, &d);

    let out = hornvale_hearsay::derive::variants_about_accumulating(
        &walk,
        &ladders,
        &d,
        hornvale_hearsay::accumulate::Accumulation::Additive,
        eid(1),
        hornvale_history::OCC_ENDED,
    );
    let three = out
        .iter()
        .find(|c| c.holder == eid(3))
        .expect("3 is reachable and must hold a claim");
    assert_eq!(
        three.hops, 1,
        "the short direct route from witness 2 must win, not the long route through 1"
    );
    assert_eq!(
        three.precision.rung(),
        0,
        "the short route's width (41.0) stays inside the day rung; the long \
         route's width (43.0) would cross into the moon rung"
    );
}

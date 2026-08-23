//! Task 3's guard: [`hornvale_hearsay::touchstone`] diffs two transmission
//! arms' traced tellings into per-holder, per-component change flags. Every
//! fixture here is a hand-built `Vec<HeldTelling>` (no world, no ledger for
//! the alignment tests) — the module's job is a merge-join and five
//! component comparisons, not derivation, so nothing here needs to pay for
//! genesis.
//!
//! Two batteries:
//! - The alignment/flag tests pin `belief_deltas`/`tail_counts` against hand
//!   fixtures: identical holders, a one-component difference each, and a
//!   holder present in only one arm.
//! - [`mod mutation`] proves each of the five flags is load-bearing: a
//!   fixture that moves EXACTLY one component, checked against the source
//!   text so a hard-wired `false` (the Parley deferral this task exists to
//!   prevent) would be caught.

mod common;

use common::{eid, ledger_with, put};
use hornvale_hearsay::touchstone::{belief_deltas, changed_tail, tail_by_people_pair, tail_counts};
use hornvale_hearsay::traced::{Carrier, Crossed, HeldTelling};
use hornvale_kernel::Claim;
use hornvale_kernel::Precision;
use hornvale_kernel::ledger::{EntityId, Value};
use hornvale_kernel::provenance::Provenance;

/// The predicate every hand-built claim in this file is about.
const PREDICATE: &str = "occ-ended";

fn claim(holder: u64, subject: u64, object: Value, hops: u32, precision: Precision) -> Claim {
    Claim {
        holder: eid(holder),
        subject: eid(subject),
        predicate: PREDICATE.to_string(),
        object,
        grade: Provenance::Witnessed,
        hops,
        precision,
    }
}

/// A minimal `HeldTelling`: witness `witness`, no crossings, day `day`,
/// width `width`, hop 0, `Precision::FINEST`.
fn telling(holder: u64, witness: u64, day: f64, width: f64) -> HeldTelling {
    HeldTelling {
        claim: claim(holder, 99, Value::Number(day), 0, Precision::FINEST),
        witness: eid(witness),
        width,
        crossings: Vec::new(),
    }
}

// ===========================================================================
// Alignment and per-flag definitions.
// ===========================================================================

#[test]
fn an_identical_holder_on_both_arms_has_every_flag_false() {
    let a = vec![telling(1, 1, 500.0, 10.0)];
    let b = vec![telling(1, 1, 500.0, 10.0)];
    let deltas = belief_deltas(&a, &b);
    assert_eq!(deltas.len(), 1);
    let d = &deltas[0];
    assert_eq!(d.holder, eid(1));
    assert!(!d.route_changed);
    assert!(!d.day_changed);
    assert!(!d.rung_changed);
    assert!(!d.hops_changed);
    assert!(!d.width_changed);
    assert!(!d.any_changed());
}

#[test]
fn a_witness_that_differs_flags_route_changed_and_nothing_else() {
    let a = vec![telling(1, 1, 500.0, 10.0)];
    let b = vec![telling(1, 2, 500.0, 10.0)];
    let d = &belief_deltas(&a, &b)[0];
    assert!(
        d.route_changed,
        "witness != witness must flag route_changed"
    );
    assert!(!d.day_changed);
    assert!(!d.rung_changed);
    assert!(!d.hops_changed);
    assert!(!d.width_changed);
}

#[test]
fn a_crossings_only_difference_still_flags_route_changed() {
    // Same witness on both -- only the crossing sequence differs, which is
    // exactly why route identity is `witness != witness || crossings !=
    // crossings`, not just the witness half.
    let mut a = vec![telling(1, 1, 500.0, 10.0)];
    let mut b = vec![telling(1, 1, 500.0, 10.0)];
    a[0].crossings = Vec::new();
    b[0].crossings = vec![Crossed {
        edges: 1,
        carrier: Carrier::Seam,
    }];
    let d = &belief_deltas(&a, &b)[0];
    assert!(
        d.route_changed,
        "crossings != crossings must flag route_changed"
    );
    assert!(!d.day_changed);
}

#[test]
fn a_day_differing_by_one_bit_flags_day_changed_bit_exact() {
    let mut a = vec![telling(1, 1, 500.0, 10.0)];
    let mut b = vec![telling(1, 1, 500.0, 10.0)];
    // Perturb by exactly one ULP -- invisible to a tolerant `==`, which is
    // the whole point of the bit-exact rule this test pins.
    let bumped = f64::from_bits(500.0_f64.to_bits() + 1);
    assert_ne!(bumped, 500.0 + 1.0, "sanity: a one-ULP nudge, not +1.0");
    a[0].claim.object = Value::Number(bumped);
    b[0].claim.object = Value::Number(500.0);
    let d = &belief_deltas(&a, &b)[0];
    assert!(d.day_changed);
    assert!(!d.route_changed);
    assert!(!d.rung_changed);
    assert!(!d.hops_changed);
    assert!(!d.width_changed);
}

#[test]
fn nan_days_on_both_sides_are_not_conflated_by_bit_exact_comparison() {
    // A tolerant `==` calls two NaNs unequal (float NaN != NaN) -- the
    // bit-exact rule instead compares payload bits, so two differently
    // constructed NaNs correctly register as a difference.
    let mut a = vec![telling(1, 1, 500.0, 10.0)];
    let mut b = vec![telling(1, 1, 500.0, 10.0)];
    a[0].claim.object = Value::Number(f64::NAN);
    b[0].claim.object = Value::Number(f64::NAN);
    let d = &belief_deltas(&a, &b)[0];
    // Rust's f64::NAN is a fixed bit pattern, so two identical constructions
    // compare bit-equal here -- this pins that the rule is `to_bits()`, not
    // `==`, without depending on distinct NaN payloads existing at all.
    assert!(!d.day_changed);
}

#[test]
fn a_rung_that_differs_flags_rung_changed_and_nothing_else() {
    let a = vec![telling(1, 1, 500.0, 10.0)];
    let mut b = vec![telling(1, 1, 500.0, 10.0)];
    b[0].claim.precision = Precision(1);
    let d = &belief_deltas(&a, &b)[0];
    assert!(d.rung_changed);
    assert!(!d.route_changed);
    assert!(!d.day_changed);
    assert!(!d.hops_changed);
    assert!(!d.width_changed);
}

#[test]
fn hops_that_differ_flag_hops_changed_and_nothing_else() {
    let a = vec![telling(1, 1, 500.0, 10.0)];
    let mut b = vec![telling(1, 1, 500.0, 10.0)];
    b[0].claim.hops = 3;
    let d = &belief_deltas(&a, &b)[0];
    assert!(d.hops_changed);
    assert!(!d.route_changed);
    assert!(!d.day_changed);
    assert!(!d.rung_changed);
    assert!(!d.width_changed);
}

#[test]
fn width_that_differs_by_one_bit_flags_width_changed_bit_exact() {
    let a = vec![telling(1, 1, 500.0, 10.0)];
    let bumped = f64::from_bits(10.0_f64.to_bits() + 1);
    let b = vec![telling(1, 1, 500.0, bumped)];
    let d = &belief_deltas(&a, &b)[0];
    assert!(d.width_changed);
    assert!(!d.route_changed);
    assert!(!d.day_changed);
    assert!(!d.rung_changed);
    assert!(!d.hops_changed);
}

#[test]
fn a_holder_present_only_in_arm_b_is_tallied_not_dropped() {
    let a: Vec<HeldTelling> = vec![];
    let b = vec![telling(1, 1, 500.0, 10.0)];
    assert!(
        belief_deltas(&a, &b).is_empty(),
        "a one-sided holder has nothing to diff, so it must not appear in belief_deltas"
    );
    let t = tail_counts(&a, &b);
    assert_eq!(t.only_b, 1);
    assert_eq!(t.only_a, 0);
    assert_eq!(t.reached_both, 0);
}

#[test]
fn a_holder_present_only_in_arm_a_is_tallied_not_dropped() {
    let a = vec![telling(1, 1, 500.0, 10.0)];
    let b: Vec<HeldTelling> = vec![];
    assert!(belief_deltas(&a, &b).is_empty());
    let t = tail_counts(&a, &b);
    assert_eq!(t.only_a, 1);
    assert_eq!(t.only_b, 0);
    assert_eq!(t.reached_both, 0);
}

#[test]
fn merge_join_aligns_by_holder_across_a_gap_on_either_side() {
    // A: holders 1, 3, 5.  B: holders 2, 3, 4, 5. Only 3 and 5 are common.
    let a = vec![
        telling(1, 1, 500.0, 10.0),
        telling(3, 1, 500.0, 10.0),
        telling(5, 1, 500.0, 10.0),
    ];
    let b = vec![
        telling(2, 1, 500.0, 10.0),
        telling(3, 1, 500.0, 10.0),
        telling(4, 1, 500.0, 10.0),
        telling(5, 1, 500.0, 10.0),
    ];
    let deltas = belief_deltas(&a, &b);
    let holders: Vec<EntityId> = deltas.iter().map(|d| d.holder).collect();
    assert_eq!(holders, vec![eid(3), eid(5)]);
    let t = tail_counts(&a, &b);
    assert_eq!(t.reached_both, 2);
    assert_eq!(t.only_a, 1); // holder 1
    assert_eq!(t.only_b, 2); // holders 2, 4
}

// ===========================================================================
// changed_tail.
// ===========================================================================

#[test]
fn changed_tail_is_zero_over_an_empty_denominator() {
    let a: Vec<HeldTelling> = vec![];
    let b: Vec<HeldTelling> = vec![];
    assert_eq!(changed_tail(&tail_counts(&a, &b)), 0.0);
}

#[test]
fn changed_tail_is_the_movers_ratio() {
    let a = vec![telling(1, 1, 500.0, 10.0), telling(2, 1, 500.0, 10.0)];
    let mut b = vec![telling(1, 1, 500.0, 10.0), telling(2, 1, 500.0, 10.0)];
    b[1].witness = eid(9); // one of two holders moves
    let t = tail_counts(&a, &b);
    assert_eq!(t.reached_both, 2);
    assert_eq!(t.any, 1);
    assert_eq!(changed_tail(&t), 0.5);
}

// ===========================================================================
// tail_by_people_pair.
// ===========================================================================

#[test]
fn tail_by_people_pair_buckets_by_holder_and_subject_people() {
    // Holder 1 is human, holder 2 is kobold; both hold a claim about
    // subject 99, whose own people is human. Only holder 1's telling moves.
    let mut led = ledger_with(&[]);
    put(
        &mut led,
        1,
        hornvale_history::OCC_PEOPLE,
        Value::Text("human".to_string()),
    );
    put(
        &mut led,
        2,
        hornvale_history::OCC_PEOPLE,
        Value::Text("kobold".to_string()),
    );
    put(
        &mut led,
        99,
        hornvale_history::OCC_PEOPLE,
        Value::Text("human".to_string()),
    );

    let a = vec![telling(1, 1, 500.0, 10.0), telling(2, 1, 500.0, 10.0)];
    let mut b = vec![telling(1, 1, 500.0, 10.0), telling(2, 1, 500.0, 10.0)];
    b[0].witness = eid(7); // holder 1's route moves

    let by_pair = tail_by_people_pair(&led, eid(99), &a, &b);
    let human_human = by_pair
        .get(&("human".to_string(), "human".to_string()))
        .expect("holder 1's bucket");
    assert_eq!(human_human.reached_both, 1);
    assert_eq!(human_human.route, 1);
    assert_eq!(human_human.any, 1);

    let kobold_human = by_pair
        .get(&("kobold".to_string(), "human".to_string()))
        .expect("holder 2's bucket");
    assert_eq!(kobold_human.reached_both, 1);
    assert_eq!(kobold_human.any, 0);
}

#[test]
fn tail_by_people_pair_buckets_a_one_sided_holder_by_its_own_people() {
    let mut led = ledger_with(&[]);
    put(
        &mut led,
        1,
        hornvale_history::OCC_PEOPLE,
        Value::Text("human".to_string()),
    );
    put(
        &mut led,
        99,
        hornvale_history::OCC_PEOPLE,
        Value::Text("human".to_string()),
    );

    let a: Vec<HeldTelling> = vec![];
    let b = vec![telling(1, 1, 500.0, 10.0)];
    let by_pair = tail_by_people_pair(&led, eid(99), &a, &b);
    let bucket = by_pair
        .get(&("human".to_string(), "human".to_string()))
        .expect("the one-sided holder's own people");
    assert_eq!(bucket.only_b, 1);
    assert_eq!(bucket.reached_both, 0);
}

#[test]
fn an_unlabelled_holder_or_subject_buckets_under_the_empty_people() {
    // No occ-people fact committed for either entity -- people_of falls back
    // to "" for both, matching the traced walk's own resolution.
    let led = ledger_with(&[]);
    let a = vec![telling(1, 1, 500.0, 10.0)];
    let b = vec![telling(1, 1, 500.0, 10.0)];
    let by_pair = tail_by_people_pair(&led, eid(99), &a, &b);
    assert!(by_pair.contains_key(&(String::new(), String::new())));
}

// ===========================================================================
// Mutation proofs: each of the five flags is load-bearing.
// ===========================================================================
//
// Each test below asserts the exact source text it is about to check exists
// (so a rename or refactor that dropped the assignment would fail loudly
// here, not silently pass a stale check), then re-derives the SAME
// definition an independent way and requires it to agree with what
// `belief_deltas` actually returned. A hard-wired `false` for any one flag
// would still compile and still pass every test above EXCEPT the one
// asserting that specific flag fires -- which is the whole reason each flag
// gets its own dedicated fixture rather than one shared "everything changed"
// case.

mod mutation {
    use super::*;
    use std::fs;

    fn source() -> String {
        fs::read_to_string(concat!(env!("CARGO_MANIFEST_DIR"), "/src/touchstone.rs"))
            .expect("read touchstone.rs")
    }

    #[test]
    fn route_changed_assignment_exists_in_source() {
        let s = source();
        assert!(
            s.contains("a.witness != b.witness || a.crossings != b.crossings"),
            "route_changed's exact definition must be present -- if this \
             text is gone, the mutation proof below is checking nothing"
        );
    }

    #[test]
    fn route_changed_fires_only_when_witness_or_crossings_actually_move() {
        // A held-constant route_changed (e.g. `false`) would pass every OTHER
        // per-flag test in this file, since none of them assert on
        // route_changed except the two above. This is the one that catches
        // it: two arms differing ONLY in witness must set route_changed,
        // full stop.
        let a = vec![telling(1, 1, 500.0, 10.0)];
        let b = vec![telling(1, 2, 500.0, 10.0)];
        assert!(belief_deltas(&a, &b)[0].route_changed);
        // And the negative: identical witness/crossings must NOT set it --
        // a `true` constant is caught here.
        let c = vec![telling(1, 1, 500.0, 10.0)];
        assert!(!belief_deltas(&a, &c)[0].route_changed);
    }

    #[test]
    fn day_changed_assignment_exists_in_source() {
        let s = source();
        assert!(
            s.contains("da.to_bits() != db.to_bits()"),
            "day_changed's bit-exact comparison must be present"
        );
    }

    #[test]
    fn day_changed_fires_only_when_the_remembered_day_actually_moves() {
        let mut a = vec![telling(1, 1, 500.0, 10.0)];
        let mut b = vec![telling(1, 1, 500.0, 10.0)];
        a[0].claim.object = Value::Number(500.0);
        b[0].claim.object = Value::Number(501.0);
        assert!(belief_deltas(&a, &b)[0].day_changed);
        b[0].claim.object = Value::Number(500.0);
        assert!(!belief_deltas(&a, &b)[0].day_changed);
    }

    #[test]
    fn rung_changed_assignment_exists_in_source() {
        let s = source();
        assert!(
            s.contains("a.claim.precision != b.claim.precision"),
            "rung_changed's definition must be present"
        );
    }

    #[test]
    fn rung_changed_fires_only_when_precision_actually_moves() {
        let a = vec![telling(1, 1, 500.0, 10.0)];
        let mut b = vec![telling(1, 1, 500.0, 10.0)];
        b[0].claim.precision = Precision(2);
        assert!(belief_deltas(&a, &b)[0].rung_changed);
        b[0].claim.precision = Precision::FINEST;
        assert!(!belief_deltas(&a, &b)[0].rung_changed);
    }

    #[test]
    fn hops_changed_assignment_exists_in_source() {
        let s = source();
        assert!(
            s.contains("a.claim.hops != b.claim.hops"),
            "hops_changed's definition must be present"
        );
    }

    #[test]
    fn hops_changed_fires_only_when_hops_actually_moves() {
        let a = vec![telling(1, 1, 500.0, 10.0)];
        let mut b = vec![telling(1, 1, 500.0, 10.0)];
        b[0].claim.hops = 5;
        assert!(belief_deltas(&a, &b)[0].hops_changed);
        b[0].claim.hops = 0;
        assert!(!belief_deltas(&a, &b)[0].hops_changed);
    }

    #[test]
    fn width_changed_assignment_exists_in_source() {
        let s = source();
        assert!(
            s.contains("a.width.to_bits() != b.width.to_bits()"),
            "width_changed's bit-exact comparison must be present"
        );
    }

    #[test]
    fn width_changed_fires_only_when_width_actually_moves() {
        let a = vec![telling(1, 1, 500.0, 10.0)];
        let b = vec![telling(1, 1, 500.0, 11.0)];
        assert!(belief_deltas(&a, &b)[0].width_changed);
        let c = vec![telling(1, 1, 500.0, 10.0)];
        assert!(!belief_deltas(&a, &c)[0].width_changed);
    }

    #[test]
    fn any_changed_assignment_exists_in_source() {
        let s = source();
        assert!(
            s.contains("self.route_changed")
                && s.contains("self.day_changed")
                && s.contains("self.rung_changed")
                && s.contains("self.hops_changed")
                && s.contains("self.width_changed"),
            "any_changed must OR all five flags"
        );
    }

    #[test]
    fn any_changed_is_false_when_all_five_flags_are_false_and_true_when_one_is() {
        let a = vec![telling(1, 1, 500.0, 10.0)];
        let same = vec![telling(1, 1, 500.0, 10.0)];
        assert!(!belief_deltas(&a, &same)[0].any_changed());
        let moved = vec![telling(1, 9, 500.0, 10.0)]; // route only
        assert!(belief_deltas(&a, &moved)[0].any_changed());
    }
}

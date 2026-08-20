//! The Cupel, Task 1 — the penalty parameter is live.
//!
//! `traced_variants_with_penalty` generalizes the traced walk over
//! [`PenaltyModel`] so a later readout can re-walk a crossing under a
//! constant-denominator arm the shipped `Crossing` enum cannot express. The
//! property this test pins: **the penalty parameter actually feeds the
//! width-first ordering key** — a constant denominator `D != 1 + edges` must
//! move at least one holder's accumulated width relative to the derived arm,
//! on a fixture where a claim genuinely crosses a people boundary. It does
//! not assert which holder or how many; that would pin an incidental
//! transcribed number rather than the property under test (spec discipline:
//! "studies are data, metrics are code" applies here in miniature — the
//! fixture and the arms are data, the merge-join is the code that reads
//! them).

use crate::common;

use common::{PREDICATE, RULE, eid, two_peoples_joined_by_one_raid};
use hornvale_astronomy::units::StdDays;
use hornvale_hearsay::contact::{Contact, contact_of};
use hornvale_hearsay::durations::PeopleDurations;
use hornvale_hearsay::ladder::PeopleLadders;
use hornvale_hearsay::lineage::lineage_of;
use hornvale_hearsay::traced::{HeldTelling, PenaltyModel, traced_variants_with_penalty};
use hornvale_hearsay::transmission::{Crossing, Transmission, Walk};

/// One generation length and lifespan for each of the fixture's two
/// peoples — `homogeneous_durations` in `touchstone_controls_probe.rs`'s
/// shape, widened to cover both `"human"` and `"kobold"` since this fixture
/// (unlike that one) genuinely crosses a people boundary.
fn two_peoples_durations() -> PeopleDurations {
    let mut d = PeopleDurations::default();
    for people in ["human", "kobold"] {
        d.insert(
            people,
            Some(StdDays::new(50.0).expect("positive")),
            Some(StdDays::new(150.0).expect("positive")),
        );
    }
    d
}

/// A constant denominator that cannot equal `1 + edges` on this fixture
/// (`edges == 1`, so `1 + edges == 2`) — chosen well clear of it so the
/// magnitude the two arms compute is not a near-miss of floating-point
/// rounding.
const CONSTANT_DENOMINATOR: f64 = 4.0;

/// The penalty parameter must be live: re-walking under
/// [`PenaltyModel::ConstantDenominator`] with a divisor other than
/// `1 + edges` must move at least one holder's accumulated width relative to
/// [`PenaltyModel::Derived`], on a fixture where an account genuinely
/// crosses a people boundary (`two_peoples_joined_by_one_raid`, edges = 1).
#[test]
fn cupel_penalty_constant_denominator_moves_a_holders_width() {
    let led = two_peoples_joined_by_one_raid();
    let lineage = lineage_of(&led);
    let contact = contact_of(&led);
    let durations = two_peoples_durations();
    let ladders = PeopleLadders::of(&led, &durations);
    let walk = Walk {
        ledger: &led,
        lineage: &lineage,
        contact: &contact,
        policy: Transmission {
            contact: Contact::WithRaidSeam,
            crossing: Crossing::ContactWeighted,
            ..Transmission::AS_SHIPPED
        },
    };
    let subject = eid(1);

    let derived = traced_variants_with_penalty(
        &walk,
        &ladders,
        &durations,
        RULE,
        subject,
        PREDICATE,
        PenaltyModel::Derived,
    );
    let constant = traced_variants_with_penalty(
        &walk,
        &ladders,
        &durations,
        RULE,
        subject,
        PREDICATE,
        PenaltyModel::ConstantDenominator(CONSTANT_DENOMINATOR),
    );

    // Anti-vacuity: both arms must actually reach holders, and the same set
    // of holders, or the merge-join below could hold trivially.
    assert!(
        !derived.is_empty(),
        "control: the derived arm must reach holders"
    );
    assert_eq!(
        holder_ids(&derived),
        holder_ids(&constant),
        "control: both arms walk the same fixture and must reach the same holders"
    );

    assert!(
        widths_differ(&derived, &constant),
        "the penalty parameter must be live: a constant denominator D != 1+edges \
         must move at least one holder's width relative to the derived arm -- \
         derived={derived:?} constant={constant:?}"
    );
}

fn holder_ids(tellings: &[HeldTelling]) -> Vec<u64> {
    let mut ids: Vec<u64> = tellings.iter().map(|t| t.claim.holder.get()).collect();
    ids.sort_unstable();
    ids
}

/// Merge-join two `Vec<HeldTelling>` by `claim.holder` (both ascending, since
/// `traced_variants_with_penalty` builds its result from a `BTreeMap`'s
/// `into_values()`) and report whether any holder's `width.to_bits()`
/// differs between them.
fn widths_differ(a: &[HeldTelling], b: &[HeldTelling]) -> bool {
    let mut ai = a.iter().peekable();
    let mut bi = b.iter().peekable();
    loop {
        match (ai.peek(), bi.peek()) {
            (Some(x), Some(y)) => {
                let (xh, yh) = (x.claim.holder.get(), y.claim.holder.get());
                match xh.cmp(&yh) {
                    std::cmp::Ordering::Equal => {
                        if x.width.to_bits() != y.width.to_bits() {
                            return true;
                        }
                        ai.next();
                        bi.next();
                    }
                    std::cmp::Ordering::Less => {
                        ai.next();
                    }
                    std::cmp::Ordering::Greater => {
                        bi.next();
                    }
                }
            }
            _ => return false,
        }
    }
}

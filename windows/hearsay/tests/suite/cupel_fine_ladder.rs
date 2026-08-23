//! The Cupel, Task 2 — the fine-ladder day-channel liveness control.
//!
//! The real panel's day-null (measured separately, §3) says the derived and
//! constant-denominator crossing penalties agree on every holder's
//! REMEMBERED DAY across the census panel. That could mean the day channel
//! is genuinely blind to the penalty parameter, or it could mean the
//! panel's ladders are too coarse for a difference of a fraction of a
//! generation to ever cross a rung boundary. This is the control that tells
//! the two apart: a fast, hand-built fixture whose ladder is engineered so a
//! crossing's magnitude change CAN move a remembered rung, proving the
//! instrument itself is not blind — so the panel's null is read as ladder
//! coarseness, not instrument blindness.
//!
//! **The paired, self-verifying property** (this is what makes the test
//! honest rather than merely asserting a number that happened to come out
//! of one run):
//!
//! 1. With `ConstantDenominator(D)` for a `D != 1 + edges`, at least one
//!    holder's remembered rung (`claim.precision`) differs from the
//!    `Derived` arm's.
//! 2. With `ConstantDenominator((1 + edges) as f64)` — the constant that
//!    reproduces the derived magnitude on this single-crossing fixture — the
//!    two arms are BIT-IDENTICAL on every holder (`claim.object`,
//!    `claim.precision`, `width`, `witness`, `crossings`).
//!
//! Part 2 is the non-vacuity proof for part 1: if the instrument could not
//! see the crossing's magnitude at all, part 2 could not hold either (there
//! would be nothing for `D == 1 + edges` to "reproduce"). Together they rule
//! out both failure modes a naive single assertion would miss: an
//! instrument that always disagrees (part 2 would fail) and an instrument
//! that always agrees regardless of `D` (part 1 would fail).
//!
//! **Why the crossing moves the GENERATION/LIFESPAN rung (50/150 days) and
//! not the four fine moon rungs (9.8/10.1/10.6) the fixture's own doc
//! comment describes.** `two_peoples_with_raid_count`'s doc comment
//! engineers those three moons against an ADDITIVE accumulation of the
//! route's per-step spans — under that rule the width at holder `5` really
//! does land at 9.7 days before any penalty (`1.0 -> 1+2=3.0 -> 3+3=6.0 ->
//! 6+3.7=9.7`, one step per generation-span). This control instead runs
//! under `rule = common::RULE` (`Accumulation::Multiplicative`), per the
//! controller's resolution, matching the readout this control exists to
//! back. Multiplicative accumulation compounds those same three steps
//! instead of summing them (`1.0 -> 1*(1+2)=3.0 -> 3*(1+3)=12.0`), so by the
//! time the walk reaches holder `3` — the last same-people node before the
//! crossing — the accumulated width is already 12.0 days, past all four
//! moon rungs. The crossing step's own span can only ever ADD 3.7-to-a-lot
//! more days before the multiplicative step, so the post-crossing width at
//! holder `5` is bounded below by `12.0 * (1 + 3.7) = 56.4` — already inside
//! the generation rung (50 days) and unable to fall back below it whatever
//! `D` is. What a constant denominator CAN still move, empirically (see
//! `scratch_explore`'s trace in the task report), is whether that width
//! clears the NEXT rung up, the 150-day lifespan: `D = 0.1` (`unit()/D =
//! 10.0`, comfortably inside the `(0.0395, 0.1282)` window that lands width
//! `176.4` inside `[150, 372.4)`) moves holder `5` from `Precision(5)`
//! (generation) to `Precision(6)` (lifespan) — a genuine rung change, just
//! not the one the fixture's prose anticipated for an additive world. The
//! property under test — "the day channel discriminates when the ladder is
//! fine enough" — holds either way: the lifespan rung is still a rung of
//! this world's own ladder, engineered from the same committed sky and
//! social facts as the moon rungs are.
//!
//! **Why `claim.precision`, not `claim.object`, is the asserted signal.**
//! At holder `5`, `claim.object` (the remembered day) is `450.0` under
//! EVERY `D` tried, including the one that moves `claim.precision`: the
//! previous holder's remembered day (`498.2`, already coarsened to the
//! 10.6-day rung at holder `3`) happens to be an exact multiple of both 50
//! and 150 once floored (`floor(498.2/50)*50 == floor(498.2/150)*150 ==
//! 450.0`), so the remembered NUMBER is a coincidental tie across the two
//! rungs that changing `D` moves between. `claim.precision` — which rung
//! was actually resolved, independent of whether that rung's rounding
//! happens to coincide with a neighbour's — is what the brief calls "the
//! cleanest signal", and this fixture is a live demonstration of exactly
//! why: an object-only assertion would have reported no difference and
//! silently missed the discrimination that occurred.

use crate::common;
use common::{PREDICATE, RULE, eid, two_peoples_joined_by_one_raid};
use hornvale_astronomy::units::StdDays;
use hornvale_hearsay::contact::{Contact, contact_of};
use hornvale_hearsay::durations::PeopleDurations;
use hornvale_hearsay::ladder::PeopleLadders;
use hornvale_hearsay::lineage::lineage_of;
use hornvale_hearsay::traced::{HeldTelling, PenaltyModel, traced_variants_with_penalty};
use hornvale_hearsay::transmission::{Crossing, Transmission, Walk};
use hornvale_kernel::ledger::Value;

/// One generation length and lifespan for each of the fixture's two
/// peoples. `hornvale_hearsay::durations::homogeneous_durations` lives in a
/// heavy binary this fast suite cannot reach, so this is built by hand —
/// the same numbers `cupel_penalty.rs`'s `two_peoples_durations` uses.
fn two_peoples_durations() -> PeopleDurations {
    let mut durs = PeopleDurations::default();
    for people in ["human", "kobold"] {
        durs.insert(
            people,
            Some(StdDays::new(50.0).expect("positive")),
            Some(StdDays::new(150.0).expect("positive")),
        );
    }
    durs
}

/// A constant denominator chosen so `unit()/D` pushes the accumulated width
/// at the fixture's first cross-people holder past the NEXT rung boundary
/// up (the 150-day lifespan rung) from where the derived arm lands (the
/// 50-day generation rung) — see this module's doc comment for the interval
/// this must fall in (`(0.0395, 0.1282)`) and why the fine moon rungs the
/// fixture was built around are unreachable under `Accumulation::
/// Multiplicative`. Found empirically per the task brief: 4.0, 6.0 and 1.5
/// (all inside the generation rung alongside the derived arm) moved
/// nothing; searching further out found this window.
const OFF_DENOMINATOR: f64 = 0.1;

/// The day channel discriminates `Derived` from a constant denominator when
/// the ladder is fine enough to notice the difference — see this module's
/// doc comment for the paired property and why it is self-verifying.
#[test]
fn cupel_fine_ladder_day_channel_discriminates_when_the_ladder_is_fine_enough() {
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

    // Anti-vacuity: the derived arm must actually reach holders, and at
    // least one of them must genuinely have crossed the people boundary --
    // otherwise `edges` below has nothing to be read from.
    assert!(
        !derived.is_empty(),
        "control: the derived arm must reach holders"
    );
    let edges = crossing_edges(&derived);
    let identity_denominator = (1 + edges) as f64;
    assert_ne!(
        OFF_DENOMINATOR, identity_denominator,
        "control: OFF_DENOMINATOR must differ from 1+edges on this fixture, \
         or part 1 below would vacuously agree with part 2"
    );

    // Part 1: an off-identity constant denominator moves at least one
    // holder's remembered rung relative to the derived arm.
    let off = traced_variants_with_penalty(
        &walk,
        &ladders,
        &durations,
        RULE,
        subject,
        PREDICATE,
        PenaltyModel::ConstantDenominator(OFF_DENOMINATOR),
    );
    assert_eq!(
        holder_ids(&derived),
        holder_ids(&off),
        "control: both arms walk the same fixture and must reach the same holders"
    );
    assert!(
        precisions_differ(&derived, &off),
        "the day channel must discriminate on a fine ladder: a constant \
         denominator D != 1+edges must move at least one holder's \
         remembered rung relative to the derived arm -- derived={derived:?} \
         off={off:?}"
    );

    // Part 2: the constant that reproduces the derived magnitude on this
    // single-crossing fixture reproduces every belief, bit for bit -- the
    // non-vacuity proof for part 1.
    let identity = traced_variants_with_penalty(
        &walk,
        &ladders,
        &durations,
        RULE,
        subject,
        PREDICATE,
        PenaltyModel::ConstantDenominator(identity_denominator),
    );
    assert_eq!(
        holder_ids(&derived),
        holder_ids(&identity),
        "control: both arms walk the same fixture and must reach the same holders"
    );
    assert!(
        bit_identical(&derived, &identity),
        "ConstantDenominator(1+edges) must reproduce the derived arm bit \
         for bit on every holder -- derived={derived:?} identity={identity:?}"
    );
}

/// `edges` off the fixture's own crossing, as the brief requires, rather
/// than assumed from the fixture's doc comment. Every crossing on this
/// single-raid fixture carries the same `edges` (there is only one raid
/// edge to price), so the first one found is authoritative.
fn crossing_edges(tellings: &[HeldTelling]) -> u32 {
    tellings
        .iter()
        .flat_map(|t| t.crossings.iter())
        .map(|c| c.edges)
        .next()
        .expect(
            "the fixture is engineered so at least one holder's route \
             crosses the raid seam -- see two_peoples_joined_by_one_raid's \
             doc comment",
        )
}

fn holder_ids(tellings: &[HeldTelling]) -> Vec<u64> {
    let mut ids: Vec<u64> = tellings.iter().map(|t| t.claim.holder.get()).collect();
    ids.sort_unstable();
    ids
}

/// Merge-join two `Vec<HeldTelling>` by `claim.holder` (both ascending,
/// since `traced_variants_with_penalty` builds its result from a
/// `BTreeMap`'s `into_values()`) and report whether any holder's
/// remembered rung differs.
fn precisions_differ(a: &[HeldTelling], b: &[HeldTelling]) -> bool {
    merge_any(a, b, |x, y| x.claim.precision != y.claim.precision)
}

/// Merge-join two `Vec<HeldTelling>` and report whether EVERY holder
/// matches bit for bit: `claim.object` (via `to_bits()` on the `Number`
/// variant a day is always stored as), `claim.precision`, `width` (via
/// `to_bits()`), `witness`, and `crossings`.
fn bit_identical(a: &[HeldTelling], b: &[HeldTelling]) -> bool {
    a.len() == b.len() && !merge_any(a, b, |x, y| !holder_bit_identical(x, y))
}

fn holder_bit_identical(x: &HeldTelling, y: &HeldTelling) -> bool {
    x.claim.precision == y.claim.precision
        && object_bits(&x.claim.object) == object_bits(&y.claim.object)
        && x.width.to_bits() == y.width.to_bits()
        && x.witness == y.witness
        && x.crossings == y.crossings
}

/// The remembered day's bit pattern -- every claim this walk produces
/// carries a `Number` object (see `traced_variants_with_penalty`, which
/// only ever rewrites the `Number` arm and passes any other variant
/// through unchanged; this fixture's event is a day, so it is always the
/// former).
fn object_bits(v: &Value) -> u64 {
    match v {
        Value::Number(n) => n.to_bits(),
        other => panic!("expected a Number object for a day, got {other:?}"),
    }
}

/// Walk both slices in holder order and report whether `pred` fires on any
/// matched pair. A holder present in only one slice is skipped rather than
/// treated as a difference -- the callers above already assert the holder
/// sets are equal before reaching here, so this only ever walks matched
/// pairs.
fn merge_any(
    a: &[HeldTelling],
    b: &[HeldTelling],
    pred: impl Fn(&HeldTelling, &HeldTelling) -> bool,
) -> bool {
    let mut ai = a.iter().peekable();
    let mut bi = b.iter().peekable();
    loop {
        match (ai.peek(), bi.peek()) {
            (Some(x), Some(y)) => {
                let (xh, yh) = (x.claim.holder.get(), y.claim.holder.get());
                match xh.cmp(&yh) {
                    std::cmp::Ordering::Equal => {
                        if pred(x, y) {
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

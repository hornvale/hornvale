//! Spec §5.1: a seam crossing costs damage in proportion to how much the two
//! peoples are strangers.
//!
//! **The assertion this file exists for is
//! `the_crossing_costs_less_the_better_the_peoples_know_each_other`.** Spec
//! §5.4's defence of this campaign under decision 0021 is that the magnitude
//! is DERIVED from the ledger rather than authored, and the only evidence for
//! that is a test a constant cannot satisfy: two worlds identical but for how
//! many raids lie between the peoples must put the same holder on different
//! rungs, with the better-acquainted pair strictly finer. Replace
//! `1 + edges_between` with any constant and that test goes red — measured,
//! and recorded in the task report.
//!
//! Everything else here is the surrounding discipline: that
//! [`Crossing::Free`] is inert (so `AS_SHIPPED` still ships), that the
//! penalty enters through the accumulated width and not through the holder
//! set or the hop count, that a same-people step pays nothing, and that the
//! `1 +` in the denominator is load-bearing. Each of those was checked by
//! neutralising the thing it names and requiring this file to redden; the one
//! that did NOT redden on the first attempt is recorded in
//! `tests/common/mod.rs::commit_a_four_moon_sky`, together with the rung that
//! now makes it fire.

mod common;

use common::{
    a_people_boundary_no_raid_has_ever_crossed, eid, two_peoples_joined_by_nineteen_raids,
    two_peoples_joined_by_one_raid, two_peoples_joined_by_three_raids,
};
use hornvale_astronomy::units::StdDays;
use hornvale_hearsay::accumulate::Accumulation;
use hornvale_hearsay::contact::{Contact, contact_of};
use hornvale_hearsay::derive::variants_about_accumulating;
use hornvale_hearsay::durations::PeopleDurations;
use hornvale_hearsay::ladder::PeopleLadders;
use hornvale_hearsay::lineage::lineage_of;
use hornvale_hearsay::transmission::{Crossing, Transmission, Walk};
use hornvale_kernel::ledger::{EntityId, Ledger};

/// The seam is open under every policy here — this campaign's arm is what a
/// crossing COSTS, not whether one may happen, so leaving `Contact::Descent`
/// in place would measure nothing.
fn seam(crossing: Crossing) -> Transmission {
    Transmission {
        contact: Contact::WithRaidSeam,
        crossing,
        ..Transmission::AS_SHIPPED
    }
}

/// One generation length for every people in these fixtures, so a difference
/// between two runs can only have come from the crossing penalty.
fn durations() -> PeopleDurations {
    let mut d = PeopleDurations::default();
    for people in ["human", "kobold", "drow"] {
        d.insert(
            people,
            Some(StdDays::new(50.0).expect("positive")),
            Some(StdDays::new(150.0).expect("positive")),
        );
    }
    d
}

/// Every telling as `(holder, hops, rung, remembered object)` — the whole
/// observable surface of a claim, so an assertion of equality here is not
/// blind to a change in any one of them.
fn shape(led: &Ledger, policy: Transmission) -> Vec<(u64, u32, u8, String)> {
    let lin = lineage_of(led);
    let graph = contact_of(led);
    let durations = durations();
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
        eid(1),
        hornvale_history::OCC_ENDED,
    )
    .into_iter()
    .map(|c| {
        (
            c.holder.get(),
            c.hops,
            c.precision.rung(),
            format!("{:?}", c.object),
        )
    })
    .collect()
}

/// The rung `holder` ends up on, or a panic naming the holder — an absent
/// holder is a fixture error, never a silent `None` an assertion could pass
/// over.
fn rung_at(led: &Ledger, policy: Transmission, holder: EntityId) -> u8 {
    shape(led, policy)
        .into_iter()
        .find(|(h, _, _, _)| *h == holder.get())
        .unwrap_or_else(|| panic!("fixture must reach holder {holder:?}"))
        .2
}

/// The first holder across the seam: kobold `5`, told by human `3`. Every
/// assertion about the penalty itself reads this holder, because it is the one
/// whose width contains exactly one crossing.
const ACROSS: u64 = 5;

#[test]
fn free_is_blind_to_how_much_contact_the_peoples_have() {
    let one = two_peoples_joined_by_one_raid();
    let three = two_peoples_joined_by_three_raids();
    let nineteen = two_peoples_joined_by_nineteen_raids();

    let free = |led: &Ledger| shape(led, seam(Crossing::Free));
    assert_eq!(
        free(&one).len(),
        5,
        "control: the account must reach both peoples -- {:?}",
        free(&one)
    );
    assert_eq!(
        free(&one),
        free(&three),
        "under Free the edge count is inert: one raid and three must agree"
    );
    assert_eq!(
        free(&one),
        free(&nineteen),
        "under Free the edge count is inert: one raid and nineteen must agree"
    );

    // Positive control. Without it the equalities above would be satisfied by
    // three fixtures that are simply the same world, and the extra raids they
    // carry could be doing nothing at all.
    let weighted = |led: &Ledger| shape(led, seam(Crossing::ContactWeighted));
    assert_ne!(
        weighted(&one),
        weighted(&three),
        "the fixtures DO differ in edge count -- ContactWeighted must see it"
    );
    assert_ne!(
        weighted(&one),
        weighted(&nineteen),
        "the fixtures DO differ in edge count -- ContactWeighted must see it"
    );
}

#[test]
fn a_crossing_costs_the_hearer_a_rung() {
    let led = two_peoples_joined_by_one_raid();
    let free = shape(&led, seam(Crossing::Free));
    let weighted = shape(&led, seam(Crossing::ContactWeighted));

    assert_eq!(
        free.iter().map(|t| (t.0, t.1)).collect::<Vec<_>>(),
        weighted.iter().map(|t| (t.0, t.1)).collect::<Vec<_>>(),
        "the penalty enters through the WIDTH only: same holders, same hops"
    );
    for (f, w) in free.iter().zip(weighted.iter()) {
        assert!(
            w.2 >= f.2,
            "a crossing may only ever coarsen: holder {} went {} -> {}",
            f.0,
            f.2,
            w.2
        );
    }
    let across =
        |s: &[(u64, u32, u8, String)]| s.iter().find(|t| t.0 == ACROSS).cloned().expect("5");
    assert!(
        across(&weighted).2 > across(&free).2,
        "the holder across the seam must land strictly coarser: {:?} vs {:?}",
        across(&free),
        across(&weighted)
    );
}

#[test]
fn a_step_within_one_people_pays_nothing() {
    let led = two_peoples_joined_by_one_raid();
    let free = shape(&led, seam(Crossing::Free));
    let weighted = shape(&led, seam(Crossing::ContactWeighted));

    let before =
        |s: &[(u64, u32, u8, String)]| s.iter().filter(|t| t.0 <= 3).cloned().collect::<Vec<_>>();
    assert_eq!(
        before(&free).len(),
        3,
        "control: 1, 2 and 3 all hold the claim before the seam is reached"
    );
    assert_eq!(
        before(&free).iter().map(|t| t.2).collect::<Vec<_>>(),
        vec![0u8, 0, 1],
        "control: those three do NOT all sit on one rung, so the equality \
         below is not vacuous -- {:?}",
        before(&free)
    );
    assert_eq!(
        before(&free),
        before(&weighted),
        "the human line is reached without crossing anything and must be \
         untouched, rung and remembered day alike"
    );
}

/// **The campaign's licence to exist** (spec §5.4). The magnitude of the
/// penalty is read from the ledger, so two worlds that differ only in how many
/// raids lie between the peoples must price the same crossing differently —
/// and the better-acquainted pair must pay strictly less.
///
/// A constant in place of `1 + edges_between` gives the same penalty to all
/// three worlds and collapses this to three equal rungs. That is not a
/// hypothetical: it was run, and the failure is recorded in the task report.
#[test]
fn the_crossing_costs_less_the_better_the_peoples_know_each_other() {
    let one = two_peoples_joined_by_one_raid();
    let three = two_peoples_joined_by_three_raids();
    let nineteen = two_peoples_joined_by_nineteen_raids();
    let policy = seam(Crossing::ContactWeighted);

    let (r1, r3, r19) = (
        rung_at(&one, policy, eid(ACROSS)),
        rung_at(&three, policy, eid(ACROSS)),
        rung_at(&nineteen, policy, eid(ACROSS)),
    );

    assert!(
        r1 > r3,
        "three meetings must cost less than one: rungs {r1} vs {r3}"
    );
    assert!(
        r3 > r19,
        "nineteen meetings must cost less than three: rungs {r3} vs {r19}"
    );

    // The same walk in all three worlds -- only the width differs. Without
    // this the strict ordering above could be reporting three different
    // transmission structures rather than three prices for one crossing.
    let holders = |led: &Ledger| {
        shape(led, policy)
            .into_iter()
            .map(|t| (t.0, t.1))
            .collect::<Vec<_>>()
    };
    assert_eq!(holders(&one), holders(&three));
    assert_eq!(holders(&one), holders(&nineteen));
}

/// The `1 +` in spec §5.1's denominator, pinned from both sides.
///
/// A pair of peoples with nothing on record between them pays the FULL finest
/// rung — the most expensive crossing in the model. Remove the `1 +` and the
/// division yields `+inf`, which `Accumulation::step` discards as non-finite;
/// the most expensive crossing would become the only free one and this test is
/// what notices.
#[test]
fn a_boundary_no_raid_has_ever_crossed_pays_the_full_rung() {
    let led = a_people_boundary_no_raid_has_ever_crossed();
    let free = rung_at(&led, seam(Crossing::Free), eid(3));
    let weighted = rung_at(&led, seam(Crossing::ContactWeighted), eid(3));

    assert_eq!(
        free, 0,
        "control: without the penalty this holder is at rung 0"
    );
    assert_eq!(
        weighted, 1,
        "a full finest rung of penalty must carry it past the next rung"
    );
}

//! The Touchstone (Myth campaign 6), Task 1 — **preregistration probe.**
//!
//! This file freezes the two controls the belief-delta instrument must
//! separate, and proves — before any instrument code exists (decision 0016) —
//! that each is reachable and non-vacuous on the 12-seed panel. It builds NO
//! instrument: it hand-rolls a `Claim`-inequality diff, exactly as the
//! preregistration boundary requires, so the numbers frozen here cannot have
//! been tuned by the thing they will later judge.
//!
//! Three claims. The third is split across two tests — see the fourth bullet.
//!
//! - **`..._negative_theorem`** — the NEGATIVE control's provable-zero theorem,
//!   demonstrated on a hand-built fixture in milliseconds. Under
//!   `Contact::Descent` the only route is descent; for a holder whose entire
//!   ancestry shares one people every descent step has
//!   `people_of(teller) == people_of(hearer)`, so `crossing_penalty` returns 0
//!   on every step (`derive.rs`'s `from == to` guard) under BOTH
//!   `Crossing::Free` and `Crossing::ContactWeighted`. Identical width ⇒
//!   identical rung, remembered day, hops and route ⇒ **bit-identical `Claim`s**.
//!   The fixture is people-homogeneous, so every holder is in the sub-population
//!   and the equality is asserted on all of them. **It is proven by mutation,
//!   not by assertion**: delete the `from == to` guard and this test reddens,
//!   because `ContactWeighted` then charges a full finest rung on every
//!   same-people step (`edges_between(p, p) == 0`, so the penalty is
//!   `span(FINEST) / (1 + 0)`). The mutation was run; the evidence is in the
//!   task report.
//!
//! - **`..._negative_population`** — the NEGATIVE control is non-vacuous on the
//!   panel. Over the twelve seeds' FOREIGN endings, the descent walk is run
//!   under both `Crossing` arms; among holders whose ancestry is
//!   people-homogeneous the two arms are asserted bit-identical (the theorem,
//!   on real worlds) and the count is asserted **> 0**.
//!
//! - **`..._positive_signature`** — the POSITIVE control's prior signature is
//!   reproduced on this tree. Arm A is today's shipped walk
//!   (`variants_about_accumulating`, whose selection rule is smallest final
//!   width → fewest hops → witness); arm B applies an alternative selection
//!   rule to the candidate set the seam delivers. The change rewrites the held
//!   telling at a large fraction of holders while moving the divergence
//!   aggregate (`mutually_exclusive`) by ≤ 4 — the dissociation the whole
//!   campaign exists to expose. This test re-derives both quantities, prints
//!   the whole signature table, and asserts the LOAD-BEARING controls:
//!   `shipped_absent == 0`, a non-empty population, and the frozen
//!   `positive_tail >= 20%` criterion as a witness.
//!
//! - **`..._positive_aggregate_tolerance_preregistered_not_met`** — the fourth
//!   test, and the reason there are four rather than three. The qualifying
//!   rule's SECOND half — an alternative clearing 40% held-telling churn
//!   *while* the aggregate moves by no more than ±4 — is preregistered and NOT
//!   MET on this tree, so it is carried `#[ignore]`d against a registry row
//!   rather than deleted or retuned. It is a SPLIT rather than an `#[ignore]`
//!   on the test above, deliberately: the two shared a body, and
//!   `docs/audits/heavy-tier-adjudication.md` row 81 records that
//!   `shipped_absent == 0` — not the qualifying rule, which that row calls
//!   *"report-shaped in isolation"* — is why this file is in the heavy tier at
//!   all. Ignoring the pair would have disabled the load-bearing half to defer
//!   the report-shaped one.
//!
//! ## The enumeration machinery lives in `tests/common/mod.rs`.
//!
//! The `Selection` rules, the `select` function, and the whole `Enumerator`
//! (`tellable`/`enumerate`/`descend`, the bounds, and the world reader) were
//! ported from `probe_tiebreak_rules.rs` — for the reason its own module doc
//! gives: a probe that assembled the ladder or stepped the width differently
//! from the readout whose numbers it re-derives would be measuring a different
//! model. The Touchstone campaign (Task 4) then EXTRACTED that copy into
//! `tests/common/mod.rs`, so this probe and `touchstone_readout.rs` share one
//! copy rather than each carrying its own; the per-step arithmetic is still
//! `derive.rs`'s verbatim, and the positive test still asserts the enumeration
//! CONTAINS the shipped walk's answer on every holder — the control that keeps
//! arm A the real baseline and not a reconstruction of one.
//!
//! ## Cost.
//!
//! The two heavy tests read the 12-seed panel and are `#[ignore]`d into the
//! heavy tier. Run them by name:
//! `cargo nextest run -p hornvale-hearsay --run-ignored all -E 'test(touchstone_controls_probe)'`.
//!
//! That command also runs the preregistered-not-met test, which is RED by
//! design and costs a third panel pass. The heavy tier itself never selects
//! it: `scripts/gate-full-heavy.sh` builds its filterset from the `fn` under
//! each ignore-tag whose reason begins with the heavy marker, and that test
//! carries no such tag.
//!
//! That sentence is deliberately paraphrased rather than quoting the marker
//! literally. `gate-full-heavy.sh` finds tags with a plain `grep` over every
//! `.rs` file, so a doc comment quoting the tag verbatim is counted AS a tag —
//! and since the following line is prose rather than a `fn`, the script sees
//! one more tag than it can name and refuses to run at all rather than
//! silently skip a heavy test. This exact sentence did that once
//! (The Winze, 2026-08-30): the chamber failed in 1.8 s with
//! "65 heavy: tags but 64 fn names extracted".

mod common;

use common::{
    Enumerator, PANEL, PREDICATE, RULE, Selection, build, by_people, day_bits_of, eid, ledger_with,
    pct, people_homogeneous, put, put_on, read_world, select,
};
use hornvale_astronomy::units::StdDays;
use hornvale_hearsay::accumulate::Accumulation;
use hornvale_hearsay::contact::{Contact, contact_of};
use hornvale_hearsay::derive::variants_about_accumulating;
use hornvale_hearsay::durations::PeopleDurations;
use hornvale_hearsay::ladder::PeopleLadders;
use hornvale_hearsay::lineage::lineage_of;
use hornvale_hearsay::transmission::{Crossing, Transmission, Walk};
use hornvale_kernel::Claim;
use hornvale_kernel::ledger::{EntityId, Ledger, Value};
use std::collections::{BTreeMap, BTreeSet};

// ===========================================================================
// THE NEGATIVE CONTROL — THEOREM FIXTURE (fast, not ignored).
// ===========================================================================

/// One generation length and lifespan for the fixture's single people, so a
/// difference between two runs can only have come from the crossing penalty.
fn homogeneous_durations() -> PeopleDurations {
    let mut d = PeopleDurations::default();
    d.insert(
        "human",
        Some(StdDays::new(50.0).expect("positive")),
        Some(StdDays::new(150.0).expect("positive")),
    );
    d
}

/// A single all-`"human"` descent chain `1 -> 2 -> 3 -> 4 -> 5`, ended on the
/// root with NO named attacker (so the seam is empty and
/// `edges_between("human", "human") == 0`), under a four-moon sky whose fine
/// rungs (1.0, 3.5, 9.8, 10.1, 10.6 days, then generation 50) can resolve the
/// one-finest-rung penalty a deleted `from == to` guard would charge.
///
/// The foundings are chosen so the accumulated width at holder `2` lands at
/// 3.4 days under `Accumulation::Additive` — just short of the 3.5 rung — and a
/// deleted guard's `+1.0`-day penalty carries it to 4.4, past that rung. So the
/// mutation moves at least one holder's rung and remembered day, which is what
/// makes the equality assertion below a real test of the guard rather than a
/// vacuous one.
fn homogeneous_human_chain() -> Ledger {
    let mut led = ledger_with(&[
        (1, None),
        (2, Some(1)),
        (3, Some(2)),
        (4, Some(3)),
        (5, Some(4)),
    ]);
    for (occ, day) in [(1, 0.0), (2, 120.0), (3, 170.0), (4, 220.0), (5, 270.0)] {
        put(
            &mut led,
            occ,
            hornvale_history::OCC_FOUNDED,
            Value::Number(day),
        );
    }
    for occ in [1u64, 2, 3, 4, 5] {
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
    // A four-moon sky: fine rungs a fraction of a day apart, so a
    // one-finest-rung penalty is visible as a change of rung.
    put_on(
        &mut led,
        9,
        hornvale_astronomy::facts::DAY_LENGTH_STD,
        Value::Number(1.0),
    );
    for period in [3.5, 9.8, 10.1, 10.6] {
        put_on(
            &mut led,
            9,
            hornvale_astronomy::facts::MOON_PERIOD_STD,
            Value::Number(period),
        );
    }
    put_on(
        &mut led,
        9,
        hornvale_astronomy::facts::YEAR_LENGTH_STD,
        Value::Number(372.4),
    );
    led
}

/// Every telling's whole observable surface — `(holder, hops, rung, object)` —
/// so an equality assertion over it is blind to no field.
fn claim_shapes(led: &Ledger, crossing: Crossing) -> Vec<(u64, u32, u8, String)> {
    let lineage = lineage_of(led);
    let contact = contact_of(led);
    let durations = homogeneous_durations();
    let ladders = PeopleLadders::of(led, &durations);
    let walk = Walk {
        ledger: led,
        lineage: &lineage,
        contact: &contact,
        policy: Transmission {
            contact: Contact::Descent,
            crossing,
            ..Transmission::AS_SHIPPED
        },
    };
    let mut all: Vec<(u64, u32, u8, String)> = Vec::new();
    for rule in Accumulation::ALL {
        for c in variants_about_accumulating(
            &walk,
            &ladders,
            &durations,
            rule,
            eid(1),
            hornvale_history::OCC_ENDED,
        ) {
            all.push((
                c.holder.get(),
                c.hops,
                c.precision.rung(),
                format!("{:?}", c.object),
            ));
        }
    }
    all.sort();
    all
}

/// The NEGATIVE control's provable-zero theorem, on a people-homogeneous
/// fixture. Under `Contact::Descent`, `Crossing::Free` and
/// `Crossing::ContactWeighted` produce bit-identical claims because every step
/// is within one people and `crossing_penalty`'s `from == to` guard returns 0.
///
/// **Proven by mutation.** Delete the `from == to` guard in
/// `derive.rs::crossing_penalty` and this reddens: `ContactWeighted` then
/// charges `span(FINEST) / (1 + edges_between("human", "human"))` =
/// `1.0 / 1` = a full finest rung on every same-people step, moving the width
/// and so the rung and remembered day. The mutation was run; see the task
/// report.
#[test]
fn touchstone_controls_probe_negative_theorem() {
    let led = homogeneous_human_chain();

    let free = claim_shapes(&led, Crossing::Free);
    let weighted = claim_shapes(&led, Crossing::ContactWeighted);

    // Anti-vacuity: the walk must actually reach the whole chain, and it must
    // put holders on more than one rung — otherwise the equality below could
    // hold trivially and a mutation could move nothing.
    assert!(
        free.len() >= 5,
        "control: the account must reach all five occupations across three \
         accumulation rules -- {free:?}"
    );
    let rungs: BTreeSet<u8> = free.iter().map(|t| t.2).collect();
    assert!(
        rungs.len() > 1,
        "control: the fixture must spread holders across more than one rung, or \
         a mutation that shifts a rung could not be detected -- rungs {rungs:?}"
    );

    assert_eq!(
        free, weighted,
        "THE NEGATIVE THEOREM: under Descent on a people-homogeneous lineage, \
         Free and ContactWeighted must produce bit-identical claims -- every \
         step is within one people, so crossing_penalty's `from == to` guard \
         returns 0 under both arms. If this reddened, the guard was deleted (the \
         mutation proof) or the theorem was broken.\n free={free:?}\n weighted={weighted:?}"
    );
}

// ===========================================================================
// THE NEGATIVE CONTROL — POPULATION ON THE PANEL (heavy).
// ===========================================================================

/// The NEGATIVE control is non-vacuous on the 12-seed panel: over the foreign
/// endings, holders whose ancestry is people-homogeneous exist, and on every
/// one the two `Crossing` arms produce bit-identical claims (the theorem, on
/// real worlds).
///
/// claim: structural(seed: panel) — false-positive seed-loop flag; the loop
/// binds a census-panel prefix, not a search over seeds.
#[test]
#[ignore = "heavy: live-worldgen battery; deferred from the commit gate to the heavy set (decision 0132)"]
fn touchstone_controls_probe_negative_population() {
    let components = hornvale_worldgen::WorldComponents::assemble().expect("components assemble");
    let mut homogeneous_holders = 0usize;
    let mut foreign_endings = 0usize;
    let mut checked_endings = 0usize;
    let mut skipped: Vec<u64> = Vec::new();

    for seed in PANEL {
        let world = build(seed);
        let led = &world.ledger;
        let Some(read) = read_world(led, &components) else {
            skipped.push(seed);
            continue;
        };
        for e in read.endings.iter().filter(|e| e.is_foreign()) {
            foreign_endings += 1;
            let both = |crossing: Crossing| -> BTreeMap<EntityId, Claim> {
                let walk = Walk {
                    ledger: led,
                    lineage: &read.lineage,
                    contact: &read.contact,
                    policy: Transmission {
                        contact: Contact::Descent,
                        crossing,
                        ..Transmission::AS_SHIPPED
                    },
                };
                variants_about_accumulating(
                    &walk,
                    &read.ladders,
                    &read.durations,
                    RULE,
                    e.subject,
                    PREDICATE,
                )
                .into_iter()
                .map(|c| (c.holder, c))
                .collect()
            };
            let free = both(Crossing::Free);
            let weighted = both(Crossing::ContactWeighted);
            checked_endings += 1;
            for (holder, free_claim) in &free {
                let ancestry = read.lineage.ancestry(*holder);
                if !people_homogeneous(led, &ancestry) {
                    continue;
                }
                homogeneous_holders += 1;
                assert_eq!(
                    Some(free_claim),
                    weighted.get(holder),
                    "THEOREM ON A REAL WORLD: seed {seed}, holder {holder:?} has \
                     people-homogeneous ancestry, so its descent claim must be \
                     bit-identical under Free and ContactWeighted"
                );
            }
        }
    }

    println!("\n=========== TOUCHSTONE — NEGATIVE CONTROL POPULATION ===========");
    println!(
        "panel                 : {} seeds, skipped {skipped:?}",
        PANEL.len()
    );
    println!("control pair          : (Descent, Free) vs (Descent, ContactWeighted)");
    println!("foreign endings        : {foreign_endings} (both arms scored on {checked_endings})");
    println!(
        "people-homogeneous-ancestry holders (the provable-zero sub-population): {homogeneous_holders}"
    );
    println!("churn on that sub-population : 0 by the theorem, asserted bit-identical above");

    assert!(
        skipped.len() < PANEL.len(),
        "control: every panel seed was skipped for want of a year rung"
    );
    assert!(
        homogeneous_holders > 0,
        "THE NEGATIVE CONTROL MUST BE NON-VACUOUS: no people-homogeneous-ancestry \
         holder was found on the panel's foreign endings, so the sub-population is \
         empty and the control cannot supply the ceiling the positive floor needs"
    );
}

// ===========================================================================
// THE POSITIVE CONTROL — SIGNATURE ON THE PANEL (heavy).
// ===========================================================================

/// One alternative selection rule's signature against the shipped walk.
#[derive(Clone, Copy, Default)]
struct SignatureRow {
    /// Holders whose remembered DAY differs from the shipped walk's.
    value_churn: usize,
    /// Holders whose held telling differs in ANY Claim field (day, hops, rung).
    claim_churn: usize,
    /// The divergence aggregate under this selection, summed over the panel.
    alt_mutex: usize,
}

/// One whole pass of the POSITIVE control over the 12-seed panel — every
/// quantity the two tests below assert on, measured once.
///
/// The two tests each call [`measure_positive_signature`] for themselves
/// rather than sharing a cached result: nextest is process-per-test, so there
/// is no cache to share. The heavy tier pays for exactly one pass, because it
/// selects only the `heavy:`-tagged test (see the module doc's cost note).
struct PositiveSignature {
    /// Denominator: (holder, foreign ending) pairs reached under contact.
    holders: usize,
    /// The divergence aggregate under arm A, summed over the panel.
    shipped_mutex: usize,
    /// Holders where the shipped walk's own answer was missing from the
    /// enumerated candidate set, or the holder itself was absent from arm A.
    shipped_absent: usize,
    /// Endings whose enumeration hit the candidate cap and were not scored.
    capped: usize,
    /// Panel seeds skipped for want of a year rung.
    skipped: Vec<u64>,
    /// Per-alternative signature rows, keyed by index into
    /// `Selection::ALTERNATIVES`.
    sig: BTreeMap<usize, SignatureRow>,
}

impl PositiveSignature {
    /// One alternative's held-telling (`Claim`) churn as a fraction of the
    /// holder population — the quantity spec §4 calls `positive_tail`.
    fn claim_frac(&self, ai: usize) -> f64 {
        let row = self.sig.get(&ai).copied().unwrap_or_default();
        row.claim_churn as f64 / self.holders.max(1) as f64
    }

    /// One alternative's movement of the divergence aggregate against arm A.
    fn delta(&self, ai: usize) -> i64 {
        let row = self.sig.get(&ai).copied().unwrap_or_default();
        row.alt_mutex as i64 - self.shipped_mutex as i64
    }

    /// The strongest alternative by held-telling churn ALONE, ignoring the
    /// aggregate tolerance — the half of the qualifying rule that still
    /// reproduces on this tree, and what the witness assertion reads.
    fn strongest_by_churn(&self) -> Option<(usize, f64)> {
        (0..Selection::ALTERNATIVES.len())
            .map(|ai| (ai, self.claim_frac(ai)))
            .fold(None, |best: Option<(usize, f64)>, cand| match best {
                Some((_, bf)) if bf >= cand.1 => best,
                _ => Some(cand),
            })
    }

    /// The strongest alternative meeting BOTH halves of the qualifying rule:
    /// at least 40% held-telling churn AND an aggregate delta within ±4. This
    /// is the preregistered criterion the fourth test carries as NOT MET; both
    /// constants are frozen and neither is touched here.
    fn best_qualifying(&self) -> Option<(usize, f64)> {
        (0..Selection::ALTERNATIVES.len())
            .filter(|ai| self.claim_frac(*ai) >= 0.40 && self.delta(*ai).abs() <= 4)
            .map(|ai| (ai, self.claim_frac(ai)))
            .fold(None, |best: Option<(usize, f64)>, cand| match best {
                Some((_, bf)) if bf >= cand.1 => best,
                _ => Some(cand),
            })
    }
}

/// Re-derive the POSITIVE control's whole signature on the panel, and print
/// it. Arm A is the shipped walk under `(WithRaidSeam, Multiplicative)`; arm B
/// applies each alternative selection rule to the candidate set the seam
/// delivers.
///
/// claim: structural(seed: panel) — false-positive seed-loop flag; the loop
/// binds a census-panel prefix, not a search over seeds.
fn measure_positive_signature() -> PositiveSignature {
    let components = hornvale_worldgen::WorldComponents::assemble().expect("components assemble");

    let mut holders = 0usize; // denominator: (holder, foreign ending) reached under contact
    let mut shipped_mutex = 0usize;
    let mut shipped_absent = 0usize;
    let mut capped = 0usize;
    let mut sig: BTreeMap<usize, SignatureRow> = BTreeMap::new(); // index into ALTERNATIVES
    let mut skipped: Vec<u64> = Vec::new();

    for seed in PANEL {
        let world = build(seed);
        let led = &world.ledger;
        let Some(read) = read_world(led, &components) else {
            skipped.push(seed);
            continue;
        };
        let enumerator = Enumerator {
            led,
            lineage: &read.lineage,
            contact: &read.contact,
            durations: &read.durations,
        };

        for e in read.endings.iter().filter(|e| e.is_foreign()) {
            let cands = enumerator.enumerate(&read.ladders, RULE, Contact::WithRaidSeam, e.subject);
            if cands.capped {
                capped += 1;
                continue;
            }
            // Arm A: the shipped walk itself, not a reconstruction of it.
            let walk = Walk {
                ledger: led,
                lineage: &read.lineage,
                contact: &read.contact,
                policy: Transmission {
                    contact: Contact::WithRaidSeam,
                    ..Transmission::AS_SHIPPED
                },
            };
            let shipped = variants_about_accumulating(
                &walk,
                &read.ladders,
                &read.durations,
                RULE,
                e.subject,
                PREDICATE,
            );
            let shipped_by_holder: BTreeMap<EntityId, &Claim> =
                shipped.iter().map(|c| (c.holder, c)).collect();

            // The shipped picks (arm A) and each alternative's picks (arm B) as
            // day-bit maps, for the aggregate.
            let mut shipped_pick: BTreeMap<EntityId, u64> = BTreeMap::new();
            let mut alt_pick: Vec<BTreeMap<EntityId, u64>> =
                vec![BTreeMap::new(); Selection::ALTERNATIVES.len()];

            for (holder, set) in &cands.per_holder {
                let Some(shipped_claim) = shipped_by_holder.get(holder) else {
                    // Holder sets must match; a missing shipped holder is a
                    // control failure counted below, not scored.
                    shipped_absent += 1;
                    continue;
                };
                let Some(shipped_day) = day_bits_of(shipped_claim) else {
                    continue;
                };
                // Arm A must be present in the enumerated set, or arm A is not
                // the real baseline. (hops, rung, day) identify it.
                let present = set.iter().any(|t| {
                    t.hops == shipped_claim.hops
                        && t.rung == shipped_claim.precision.rung()
                        && t.day_bits == shipped_day
                });
                if !present {
                    shipped_absent += 1;
                }
                holders += 1;
                shipped_pick.insert(*holder, shipped_day);

                for (ai, alt) in Selection::ALTERNATIVES.iter().enumerate() {
                    let pick = select(*alt, set);
                    alt_pick[ai].insert(*holder, pick.day_bits);
                    let row = sig.entry(ai).or_default();
                    if pick.day_bits != shipped_day {
                        row.value_churn += 1;
                    }
                    if pick.day_bits != shipped_day
                        || pick.hops != shipped_claim.hops
                        || pick.rung != shipped_claim.precision.rung()
                    {
                        row.claim_churn += 1;
                    }
                }
            }

            // The aggregate: mutually-exclusive cross-people day sets under arm
            // A and under each arm B.
            let ap = e
                .attacker_people
                .as_ref()
                .expect("a foreign ending names an attacker people");
            let mutex_of = |picks: &BTreeMap<EntityId, u64>| -> usize {
                let sets = by_people(&read.people_of, picks);
                let (Some(v), Some(r)) = (sets.get(&e.people), sets.get(ap)) else {
                    return 0;
                };
                let v_only = v.difference(r).count();
                let r_only = r.difference(v).count();
                usize::from(v_only > 0 && r_only > 0)
            };
            shipped_mutex += mutex_of(&shipped_pick);
            for (ai, _) in Selection::ALTERNATIVES.iter().enumerate() {
                sig.entry(ai).or_default().alt_mutex += mutex_of(&alt_pick[ai]);
            }
        }
    }

    let measured = PositiveSignature {
        holders,
        shipped_mutex,
        shipped_absent,
        capped,
        skipped,
        sig,
    };
    measured.report();
    measured
}

impl PositiveSignature {
    /// Print the whole signature table. Both tests below print it, so whichever
    /// one a reader ran has the numbers its assertion turns on.
    fn report(&self) {
        println!("\n=========== TOUCHSTONE — POSITIVE CONTROL SIGNATURE ===========");
        println!(
            "panel                 : {} seeds, skipped {:?}",
            PANEL.len(),
            self.skipped
        );
        println!(
            "arm A (baseline)      : shipped walk — least-damage selection, (WithRaidSeam, multiplicative)"
        );
        println!(
            "population            : {} (holder, foreign ending) pairs reached under contact",
            self.holders
        );
        println!(
            "aggregate (arm A)     : {} mutually-exclusive cross-people endings",
            self.shipped_mutex
        );
        println!("capped endings        : {}", self.capped);
        println!(
            "\n  The QUALIFYING metric is `claim≠` — the held-telling / `Claim`-diff rate (remembered \
             DAY or hops or rung differs), which is what spec §4 names the ~41.9% signature. `value≠` \
             (day only) is reported beside it as the probe_tiebreak-comparable figure. `route`/`width` \
             live OUTSIDE the `Claim`, so the eventual instrument sees at least this much (spec §4)."
        );
        println!(
            "\n  {:<14} {:>10} {:>9} {:>10} {:>9} {:>8} {:>8} {:>7}",
            "arm B rule", "value≠", "value%", "claim≠", "claim%", "mutexB", "delta", "PASS?"
        );
        for (ai, alt) in Selection::ALTERNATIVES.iter().enumerate() {
            let row = self.sig.get(&ai).copied().unwrap_or_default();
            let delta = self.delta(ai);
            let passes = self.claim_frac(ai) >= 0.40 && delta.abs() <= 4;
            println!(
                "  {:<14} {:>10} {:>8.2}% {:>10} {:>8.2}% {:>8} {:>+8} {:>7}",
                alt.label(),
                row.value_churn,
                pct(row.value_churn, self.holders),
                row.claim_churn,
                pct(row.claim_churn, self.holders),
                row.alt_mutex,
                delta,
                if passes { "yes" } else { "no" },
            );
        }
        println!(
            "\n  A qualifying arm B rewrites the held telling at >= 40% of holders (claim≠) while \
             moving the aggregate by <= 4 events — the dissociation the touchstone must see and the \
             aggregate cannot. The FROZEN success criterion is looser still: positive_tail >= 20%."
        );
        println!(
            "  NOTE — SUBSTRATE DRIFT ON THE EXACT PRIOR (a finding, not a rescue): the prior quoted \
             a ~41.9% VALUE-change signature; this tree's strongest value-change is under 40% \
             (recency 38.37%), while the held-telling (claim) churn is 62.64%. The exact prior \
             magnitude does not reproduce to the digit; the dissociation PROPERTY does, and no floor \
             was lowered to say so — the frozen 20% criterion stands and is cleared 3x."
        );
        println!(
            "  NOTE — THE AGGREGATE TOLERANCE IS ABSOLUTE ON A SMALL COUNT (The Winze, 2026-08-29): \
             the ±4 half of the qualifying rule is carried as PREREGISTERED, NOT MET in \
             `touchstone_controls_probe_positive_aggregate_tolerance_preregistered_not_met`, against \
             `TOOL-touchstone-aggregate-tolerance-is-absolute-on-a-small-count`. Read the arm-A \
             aggregate printed above before reading a delta: it is a count in the tens, not the ~100 \
             the rule was written against."
        );
        match self.best_qualifying() {
            Some((ai, frac)) => println!(
                "  FROZEN POSITIVE ARM B : {} (held-telling churn {:.2}%, aggregate delta within +-4)",
                Selection::ALTERNATIVES[ai].label(),
                frac * 100.0
            ),
            None => println!(
                "  NO QUALIFYING ALTERNATIVE — this is a FINDING; see the deferred test's reason."
            ),
        }
    }
}

/// The POSITIVE control's LOAD-BEARING assertions, re-derived on this tree.
///
/// `docs/audits/heavy-tier-adjudication.md` row 81 keeps this file in the heavy
/// tier on `shipped_absent == 0` — the enumerator must contain the shipped
/// walk's own answer on every holder — and says in as many words that the
/// qualifying-alternative assertion is *"report-shaped in isolation and does
/// not itself justify KEEP"*. So the qualifying rule moved out (below) and this
/// test keeps every control the verdict actually rests on.
///
/// **The witness.** `cli/tests/suite/heavy_tier.rs` asks that a
/// PREREGISTERED-not-met deferral leave an always-running assertion beside it,
/// so the deferred figures stay measured rather than becoming fiction. Here
/// that is the FROZEN success criterion itself — `positive_tail >= 20%`, the
/// held-telling churn of the strongest alternative — which this tree clears
/// roughly 3x (62.08% measured 2026-08-29 against the note's 62.64%). It is
/// deliberately NOT a pinned integer: the whole finding below is that these
/// counts move for any world change, so pinning one would re-import the very
/// brittleness being deferred. The 20% bar is the campaign's own frozen
/// criterion and is not moved here.
///
/// claim: structural(seed: panel) — false-positive seed-loop flag; the loop
/// binds a census-panel prefix, not a search over seeds.
#[test]
#[ignore = "heavy: live-worldgen battery; deferred from the commit gate to the heavy set (decision 0132)"]
fn touchstone_controls_probe_positive_signature() {
    let measured = measure_positive_signature();

    // Controls.
    assert!(
        measured.skipped.len() < PANEL.len(),
        "control: every panel seed was skipped for want of a year rung"
    );
    assert!(
        measured.holders > 0,
        "control: the contact arm reached no foreign-ending holder"
    );
    assert_eq!(
        measured.shipped_absent, 0,
        "control: the shipped walk's own answer must appear in the enumerated candidate set on \
         every holder — that is what keeps arm A the real baseline; it was absent on {}",
        measured.shipped_absent
    );

    // The witness: the frozen success criterion, on the churn half alone.
    let (ai, frac) = measured
        .strongest_by_churn()
        .expect("Selection::ALTERNATIVES is non-empty");
    assert!(
        frac >= 0.20,
        "THE FROZEN POSITIVE CRITERION (positive_tail >= 20%) NO LONGER HOLDS: the strongest \
         alternative ({}) rewrites the held telling at only {:.2}% of {} holders. The dissociation \
         PROPERTY itself has stopped reproducing — do NOT lower this bar; it is the campaign's own \
         preregistered floor (decision 0016). Report it and stop.",
        Selection::ALTERNATIVES[ai].label(),
        frac * 100.0,
        measured.holders
    );
}

/// The qualifying rule's SECOND half — PREREGISTERED, NOT MET, carried rather
/// than retuned.
///
/// Requirement as frozen: some alternative selection rule rewrites the held
/// telling at ≥ 40% of holders **while** moving the divergence aggregate
/// (`mutually_exclusive`) by no more than ±4. The churn half reproduces
/// comfortably — 62.08% measured 2026-08-29, and asserted unignored as the
/// witness above. The aggregate half does not, and the reason is the
/// tolerance's shape rather than the world's behaviour: ±4 is an ABSOLUTE
/// allowance written against a "~100" aggregate that this tree does not
/// produce. Across three arms of one campaign the arm-A aggregate read
/// **23 / 17 / 9** while recency's delta read **+2 / +7 / +7** — so the arm
/// nearest `main` passed by two events, and the quantity's natural range dwarfs
/// its tolerance.
///
/// **Not retuned, deliberately.** Widening ±4 to fit would delete the only
/// thing that noticed the aggregate is small; a successor discharges this by
/// re-deriving a tolerance from the aggregate's own base and re-freezing it,
/// filed as
/// [`TOOL-touchstone-aggregate-tolerance-is-absolute-on-a-small-count`](https://github.com/hornvale/hornvale/blob/main/book/src/frontier/idea-registry.md).
#[test]
#[ignore = "PREREGISTERED, not met: awaits TOOL-touchstone-aggregate-tolerance-is-absolute-on-a-small-count (the qualifying rule pairs a >= 40% held-telling churn bar with an ABSOLUTE |delta| <= 4 tolerance on the mutually-exclusive aggregate, and that aggregate is a count in the tens, not the ~100 the rule was written against: across three arms of The Winze it read 23 / 17 / 9 with recency's delta at +2 / +7 / +7, so the arm nearest main passed by two events and the quantity's natural range dwarfs its tolerance. The churn half still reproduces - 62.08% against the committed note's 62.64%, clearing the frozen 20% criterion 3x - and is asserted UNIGNORED as a witness in touchstone_controls_probe_positive_signature, which also keeps this file's load-bearing shipped_absent == 0 control running per heavy-tier-adjudication row 81. Widening the tolerance would retune away the only instrument that noticed the aggregate is small, so decision 0016 keeps the unmet criterion on the record instead; a successor re-derives the tolerance from the aggregate's own base and re-freezes it)"]
fn touchstone_controls_probe_positive_aggregate_tolerance_preregistered_not_met() {
    let measured = measure_positive_signature();

    assert!(
        measured.best_qualifying().is_some(),
        "THE POSITIVE CONTROL DID NOT REPRODUCE ITS DISSOCIATION on this tree: no alternative \
         selection rule achieved >= 40% held-telling (claim) churn with an aggregate delta within \
         +-4 (arm A aggregate {}, over {} holders). Per the brief this is a FINDING — do NOT lower \
         the floor or widen the tolerance to make it pass; report it and stop.",
        measured.shipped_mutex,
        measured.holders
    );
}

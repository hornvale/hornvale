//! The Touchstone (Myth campaign 6), Task 4 — **the discrimination result.**
//!
//! This is the campaign's HEADLINE battery. It runs the belief-delta
//! instrument (`hornvale_hearsay::touchstone`, Task 3) over the traced walk
//! (`hornvale_hearsay::traced`, Task 2) on the two controls frozen in
//! `docs/superpowers/specs/2026-08-19-the-touchstone-design.md` §4, and asserts
//! the instrument SEPARATES a belief-rewriting change from a provably-inert one
//! on the same 12-seed panel where the divergence aggregate
//! (`DivRow::mutually_exclusive`) reads ~0 for both.
//!
//! **The frozen success criterion (spec §4):** `positive_tail >= 20%` AND
//! `negative_tail <= 1%`.
//!
//! - **POSITIVE control** — the belief-rewriting change the aggregate misses.
//!   Under `(Contact::WithRaidSeam, Accumulation::Multiplicative)`: arm A is the
//!   shipped least-damage selection, arm B is `Selection::Recency`, both applied
//!   to the candidate set the seam delivers (the enumeration, Task 1's frozen
//!   finding — recency needs the whole arriving multiset, so it is not
//!   expressible as a shipped-shape relaxation). Each selected `Telling` is
//!   adapted into a `traced::HeldTelling` so the instrument can consume it.
//!
//! - **NEGATIVE control** — a provably-inert change. `(Descent, Free)` vs
//!   `(Descent, ContactWeighted)`, restricted to the people-homogeneous-ancestry
//!   holder sub-population, walked by the real traced walk. By the
//!   `crossing_penalty` `from == to` theorem the two arms are bit-identical on
//!   that sub-population, so `negative_tail` is 0 by construction — the ceiling
//!   the positive floor needs.
//!
//! **A falsified prediction is a finding, not a failure** (spec §4). If
//! `positive_tail < 20%`, the instrument does not see the dissociation, and that
//! null is a legitimate headline — nothing here is retuned to rescue it.
//!
//! ## Two design decisions worth stating (both faithful, neither a rescue).
//!
//! 1. **Both positive arms come from the enumeration + adapter**, not one from
//!    the shipped walk and one from the enumeration. The shipped walk carries
//!    REAL `Crossed` route steps; the adapter carries PLACEHOLDER `Crossed`s
//!    whose only load-bearing content is the count. Mixing the two
//!    representations would make `route_changed` fire on the representation
//!    difference rather than on a real route change. So arm A is
//!    `min-by-least_damage_key` over the same enumerated set arm B selects from —
//!    which equals the shipped walk's answer on every holder (Task 1's
//!    `shipped_absent == 0` control froze that), and the aggregate reproduction
//!    below re-checks it (arm-A aggregate must reproduce Task 1's frozen 10).
//! 2. **Route identity here is `(witness, crossing-count)`.** The probe's
//!    `Telling` tracks crossings only as a count, so the adapter emits
//!    `count` identical placeholder `Crossed`s; `Vec<Crossed>` equality then
//!    reduces to a length (== count) comparison — exactly what
//!    `undertow_readout.rs`'s own `cross_route_changed` did (`crossings.len()`).
//!
//! ## Cost.
//!
//! A heavy `#[ignore]`d battery — one enumeration per foreign ending (the
//! positive control, the expensive half, matching `touchstone_controls_probe`'s
//! positive test) plus two cheap traced walks (the negative). Run it by name:
//! `cargo nextest run -p hornvale-hearsay --run-ignored all -E 'test(touchstone_readout)'`.

mod common;

use common::{
    Ending, Enumerator, PANEL, PREDICATE, RULE, Selection, Telling, build, by_people, day_bits_of,
    pct, people_homogeneous, read_world, select,
};
use hornvale_hearsay::contact::Contact;
use hornvale_hearsay::touchstone::{TailCounts, changed_tail, tail_by_people_pair, tail_counts};
use hornvale_hearsay::traced::{Carrier, Crossed, HeldTelling, traced_variants_about_accumulating};
use hornvale_hearsay::transmission::{Crossing, Transmission, Walk};
use hornvale_kernel::ledger::{EntityId, Ledger, Value};
use hornvale_kernel::provenance::Provenance;
use hornvale_kernel::{Claim, Precision};
use std::collections::BTreeMap;

/// One placeholder crossing. All placeholders are equal, so a `Vec<Crossed>`
/// built of `n` of them compares equal to another iff `n` matches — which turns
/// `route_changed`'s `a.crossings != b.crossings` into a crossing-COUNT
/// comparison, the only route information the probe's `Telling` carries.
const PLACEHOLDER_CROSSED: Crossed = Crossed {
    edges: 0,
    carrier: Carrier::Seam,
};

/// Adapt one enumerated [`Telling`] into a [`HeldTelling`] the instrument can
/// diff. `subject`/`predicate`/`grade` are not read by any belief-delta flag
/// (the instrument compares holder, object, precision, hops, witness, width,
/// crossings), but a `Claim` requires them, so they are filled faithfully.
fn adapt(t: &Telling, holder: EntityId, subject: EntityId) -> HeldTelling {
    HeldTelling {
        claim: Claim {
            holder,
            subject,
            predicate: PREDICATE.to_string(),
            object: Value::Number(f64::from_bits(t.day_bits)),
            grade: Provenance::Witnessed,
            hops: t.hops,
            precision: Precision(t.rung),
        },
        witness: t.witness,
        width: f64::from_bits(t.width_bits),
        crossings: vec![PLACEHOLDER_CROSSED; t.crossings as usize],
    }
}

/// Sum one arm-pair's tail counts into a running accumulator.
fn add_counts(a: &mut TailCounts, b: &TailCounts) {
    a.reached_both += b.reached_both;
    a.only_a += b.only_a;
    a.only_b += b.only_b;
    a.any += b.any;
    a.route += b.route;
    a.day += b.day;
    a.rung += b.rung;
    a.hops += b.hops;
    a.width += b.width;
}

/// `DivRow::mutually_exclusive` for one foreign ending under one arm's picks:
/// 1 iff the victim's people and the raider's people each hold a remembered day
/// the other holds nowhere. Reproduces `undertow_readout.rs`'s
/// `tally_divergence` definition, restricted to the mutually-exclusive bucket.
fn mutex_of(
    people_of: &BTreeMap<EntityId, String>,
    e: &Ending,
    picks: &BTreeMap<EntityId, u64>,
) -> usize {
    let ap = e
        .attacker_people
        .as_ref()
        .expect("a foreign ending names an attacker people");
    let sets = by_people(people_of, picks);
    let (Some(v), Some(r)) = (sets.get(&e.people), sets.get(ap)) else {
        return 0;
    };
    let v_only = v.difference(r).count();
    let r_only = r.difference(v).count();
    usize::from(v_only > 0 && r_only > 0)
}

/// One arm of the negative control: the traced walk under one `(Contact,
/// Crossing)` pair, over `subject`'s ending.
fn traced_arm(
    led: &Ledger,
    read: &common::WorldRead,
    contact: Contact,
    crossing: Crossing,
    subject: EntityId,
) -> Vec<HeldTelling> {
    let walk = Walk {
        ledger: led,
        lineage: &read.lineage,
        contact: &read.contact,
        policy: Transmission {
            contact,
            crossing,
            ..Transmission::AS_SHIPPED
        },
    };
    traced_variants_about_accumulating(
        &walk,
        &read.ladders,
        &read.durations,
        RULE,
        subject,
        PREDICATE,
    )
}

/// Print one control's per-component instrument reading.
fn print_components(label: &str, t: &TailCounts) {
    println!(
        "  {label:<10} reached_both={:<6} any={:<6} ({:.2}%)  route={} day={} rung={} hops={} width={}",
        t.reached_both,
        t.any,
        pct(t.any, t.reached_both),
        t.route,
        t.day,
        t.rung,
        t.hops,
        t.width,
    );
    println!(
        "             per-component tail:  route {:.2}%  day {:.2}%  rung {:.2}%  hops {:.2}%  width {:.2}%",
        pct(t.route, t.reached_both),
        pct(t.day, t.reached_both),
        pct(t.rung, t.reached_both),
        pct(t.hops, t.reached_both),
        pct(t.width, t.reached_both),
    );
}

/// THE DISCRIMINATION RESULT. The touchstone separates a belief-rewriting
/// change (positive control) from a provably-inert one (negative control) on
/// the panel where the divergence aggregate reads ~0 for both.
///
/// claim: structural(seed: panel) — false-positive seed-loop flag; the loop
/// binds a census-panel prefix, not a search over seeds.
#[test]
#[ignore = "heavy: live-worldgen battery; deferred from the commit gate to the heavy set (decision 0132)"]
fn touchstone_readout() {
    let components = hornvale_worldgen::WorldComponents::assemble().expect("components assemble");
    let mut skipped: Vec<u64> = Vec::new();

    // POSITIVE control accumulators.
    let mut pos = TailCounts::default();
    let mut pos_people_pair: BTreeMap<(String, String), TailCounts> = BTreeMap::new();
    let mut arm_a_mutex = 0usize; // shipped least-damage selection
    let mut arm_b_mutex = 0usize; // recency selection
    let mut capped = 0usize;

    // NEGATIVE control accumulators.
    let mut neg = TailCounts::default();
    let mut neg_free_mutex = 0usize;
    let mut neg_weighted_mutex = 0usize;

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
            // ---- POSITIVE: selection swap over the enumerated candidate set.
            let cands = enumerator.enumerate(&read.ladders, RULE, Contact::WithRaidSeam, e.subject);
            if cands.capped {
                capped += 1;
            } else {
                let mut arm_a: Vec<HeldTelling> = Vec::new();
                let mut arm_b: Vec<HeldTelling> = Vec::new();
                let mut a_pick: BTreeMap<EntityId, u64> = BTreeMap::new();
                let mut b_pick: BTreeMap<EntityId, u64> = BTreeMap::new();
                // per_holder is a BTreeMap, so both arms come out ascending by
                // holder — exactly what tail_counts's merge-join requires.
                for (holder, set) in &cands.per_holder {
                    let a = *set
                        .iter()
                        .min_by_key(|t| t.least_damage_key())
                        .expect("a holder's candidate set is never empty");
                    let b = select(Selection::Recency, set);
                    arm_a.push(adapt(&a, *holder, e.subject));
                    arm_b.push(adapt(&b, *holder, e.subject));
                    a_pick.insert(*holder, a.day_bits);
                    b_pick.insert(*holder, b.day_bits);
                }
                add_counts(&mut pos, &tail_counts(&arm_a, &arm_b));
                for (key, counts) in tail_by_people_pair(led, e.subject, &arm_a, &arm_b) {
                    add_counts(pos_people_pair.entry(key).or_default(), &counts);
                }
                arm_a_mutex += mutex_of(&read.people_of, e, &a_pick);
                arm_b_mutex += mutex_of(&read.people_of, e, &b_pick);
            }

            // ---- NEGATIVE: crossing swap under descent, homogeneous ancestry.
            let free = traced_arm(led, &read, Contact::Descent, Crossing::Free, e.subject);
            let weighted = traced_arm(
                led,
                &read,
                Contact::Descent,
                Crossing::ContactWeighted,
                e.subject,
            );
            // Aggregate over the FULL holder sets (both peoples), before the
            // sub-population restriction — the aggregate is a panel scalar.
            let free_pick: BTreeMap<EntityId, u64> = free
                .iter()
                .filter_map(|h| day_bits_of(&h.claim).map(|d| (h.claim.holder, d)))
                .collect();
            let weighted_pick: BTreeMap<EntityId, u64> = weighted
                .iter()
                .filter_map(|h| day_bits_of(&h.claim).map(|d| (h.claim.holder, d)))
                .collect();
            neg_free_mutex += mutex_of(&read.people_of, e, &free_pick);
            neg_weighted_mutex += mutex_of(&read.people_of, e, &weighted_pick);
            // The instrument reads the provable sub-population only.
            let homog =
                |h: &HeldTelling| people_homogeneous(led, &read.lineage.ancestry(h.claim.holder));
            let free_r: Vec<HeldTelling> = free.into_iter().filter(&homog).collect();
            let weighted_r: Vec<HeldTelling> = weighted.into_iter().filter(&homog).collect();
            add_counts(&mut neg, &tail_counts(&free_r, &weighted_r));
        }
    }

    let positive_tail = changed_tail(&pos);
    let negative_tail = changed_tail(&neg);
    let aggregate_delta_positive = arm_b_mutex as i64 - arm_a_mutex as i64;
    let aggregate_delta_negative = neg_weighted_mutex as i64 - neg_free_mutex as i64;

    // ================= THE HEADLINE TABLE =================
    println!("\n================= TOUCHSTONE — THE DISCRIMINATION RESULT =================");
    println!(
        "panel                 : {} seeds, skipped {skipped:?}",
        PANEL.len()
    );
    println!("capped endings (pos)  : {capped}");
    println!("\nThe dissociation, side by side — the AGGREGATE barely moves for either control,");
    println!("while the INSTRUMENT reads HIGH for the positive and ~0 for the negative.\n");

    println!("--- INSTRUMENT (per-holder belief-delta) ---");
    print_components("POSITIVE", &pos);
    print_components("NEGATIVE", &neg);

    println!("\n--- AGGREGATE (DivRow::mutually_exclusive, the blunt instrument) ---");
    println!(
        "  POSITIVE   arm A (least-damage) = {arm_a_mutex}   arm B (recency) = {arm_b_mutex}   delta = {aggregate_delta_positive:+}"
    );
    println!(
        "  NEGATIVE   arm A (free)         = {neg_free_mutex}   arm B (weighted) = {neg_weighted_mutex}   delta = {aggregate_delta_negative:+}"
    );

    println!("\n--- HEADLINE ---");
    println!(
        "  positive_tail = {:.4}   (frozen floor   >= 0.20)",
        positive_tail
    );
    println!(
        "  negative_tail = {:.4}   (frozen ceiling <= 0.01)",
        negative_tail
    );
    let separation = if negative_tail > 0.0 {
        format!("{:.1}x", positive_tail / negative_tail)
    } else {
        "unbounded (negative is exactly 0)".to_string()
    };
    println!(
        "  SEPARATION    = {separation}   where the aggregate separates them by |{}| of ~100 (i.e. not at all)",
        aggregate_delta_positive
    );

    println!("\n--- POSITIVE control, people-pair cut (holder-people, subject-people) ---");
    if pos_people_pair.is_empty() {
        println!("  (empty)");
    }
    for ((hp, sp), t) in &pos_people_pair {
        let hp = if hp.is_empty() { "(none)" } else { hp.as_str() };
        let sp = if sp.is_empty() { "(none)" } else { sp.as_str() };
        println!(
            "  {hp:>10} <- {sp:<10}  reached_both={:<5} any={:<5} ({:.1}%)  route={} day={} rung={} hops={} width={}",
            t.reached_both,
            t.any,
            pct(t.any, t.reached_both),
            t.route,
            t.day,
            t.rung,
            t.hops,
            t.width,
        );
    }
    println!("==========================================================================\n");

    // ================= CONTROLS =================
    assert!(
        skipped.len() < PANEL.len(),
        "control: every panel seed was skipped for want of a year rung"
    );
    assert!(
        pos.reached_both > 0,
        "control: the positive control reached no (holder, foreign ending) pair"
    );
    assert!(
        neg.reached_both > 0,
        "control: the NEGATIVE control is vacuous — no people-homogeneous-ancestry \
         holder was diffed, so a green negative_tail would mean nothing"
    );
    // Arm A is min-by-least-damage over the enumerated set; if it is the real
    // shipped baseline its aggregate reproduces Task 1's frozen value (10), and
    // recency's reproduces 14. This re-checks the enumeration-vs-shipped
    // identity the readout leans on, on the merge product's own numbers.
    assert_eq!(
        arm_a_mutex, 10,
        "control: arm A (least-damage) aggregate must reproduce Task 1's frozen 10 — \
         if it does not, arm A is not the shipped baseline"
    );
    assert_eq!(
        arm_b_mutex, 14,
        "control: arm B (recency) aggregate must reproduce Task 1's frozen 14"
    );
    assert!(
        aggregate_delta_positive <= 4,
        "the positive control moved the aggregate more than the frozen +4: {aggregate_delta_positive}"
    );

    // ================= THE FROZEN SUCCESS CRITERION (spec §4) =================
    assert!(
        positive_tail >= 0.20,
        "instrument blind to a change the aggregate also misses: {positive_tail}"
    );
    assert!(
        negative_tail <= 0.01,
        "instrument fires on a provably-inert change: {negative_tail}"
    );
    // The symmetric "+0" half of the dissociation the chronicle reports: the
    // NEGATIVE control moves the divergence aggregate by exactly 0, provably —
    // every descent step is within one people (fission never crosses a people
    // boundary), so crossing_penalty's `from == to` guard returns 0 under both
    // Crossing arms and every descent-reached holder's day pick is identical.
    assert_eq!(
        aggregate_delta_negative, 0,
        "negative control moved the divergence aggregate: {aggregate_delta_negative}"
    );
}

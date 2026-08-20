//! The Cupel — Task 3, **the heavy readout battery** (the campaign's headline
//! deliverable).
//!
//! Runs three penalty arms — `Free`, `ConstantDenominator(D_panel)`,
//! `Derived` (`hornvale_hearsay::traced::PenaltyModel`, Task 1) — over the
//! panel's foreign endings through the belief-delta instrument
//! (`hornvale_hearsay::touchstone`, ready before this campaign started), and
//! reports the day (primary), route (co-primary, over the discriminating
//! holders), and width (mechanism) tails between arms.
//!
//! The shape is two passes, because `D` is panel-wide and worldgen is the
//! whole cost. Pass 1 builds and reads every seed exactly once, runs the
//! DERIVED arm on its foreign endings, and stores the built
//! [`hornvale_kernel::World`] + [`common::WorldRead`] + the derived arm's
//! `Vec<HeldTelling>` per ending — plus every derived-arm crossing's edge
//! count, panel-wide. Only after every seed's derived arm has run can
//! `D_panel` be computed (`1.0 / mean(1.0/(1.0+edges))` over every realized
//! crossing). Pass 2 then iterates the STORED worlds — never rebuilding —
//! and runs `Free` and `ConstantDenominator(D_panel)`, diffing each against
//! the stored derived tellings.
//!
//! The primary denominator is precise: `S`, the set of holders whose
//! DERIVED-arm winning route has a non-empty `crossings` vector, is computed
//! once per ending and used to filter every arm's `Vec<HeldTelling>` before
//! `tail_counts` — a same-people holder pays zero under every magnitude and
//! would only dilute the rate if left in.
//!
//! Revised Step 6 (controller ruling, pre-unblinding): the frozen §4 upper
//! bound on the day tail (`<= 5%`) assumed the same route wins in both arms,
//! but a constant penalty can change which route WINS — a route change can
//! move a rung in a way the bound never covered. So the day tail is PRINTED
//! against the band, read by a human, never hard-asserted. What IS asserted:
//! the §5 negative control, the primary denominator's non-vacuity, `D_panel`
//! finite and `> 1.0`, width non-vacuity (both arm-pairs' width tails `> 0`),
//! and a loose `< 0.50` gross-breakage tripwire that is explicitly NOT the
//! hypothesis.
//!
//! Run it by name:
//! `cargo nextest run -p hornvale-hearsay --run-ignored all -E 'test(cupel_readout)'`

mod common;

use common::{PANEL, PREDICATE, RULE, build, pct, read_world};
use hornvale_hearsay::contact::Contact;
use hornvale_hearsay::touchstone::{TailCounts, changed_tail, tail_counts};
use hornvale_hearsay::traced::{HeldTelling, PenaltyModel, traced_variants_with_penalty};
use hornvale_hearsay::transmission::{Transmission, Walk};
use hornvale_kernel::ledger::EntityId;
use std::collections::{BTreeMap, BTreeSet};

/// Sum one arm-pair's tail counts into a running accumulator (copied verbatim
/// from `touchstone_readout.rs`'s helper of the same name).
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

/// `n / d` as a bare fraction (not a percentage), `0.0` when `d == 0` — used
/// where an assertion needs the raw ratio rather than `pct`'s `0..100` form.
fn frac(n: usize, d: usize) -> f64 {
    if d == 0 { 0.0 } else { n as f64 / d as f64 }
}

/// `D = 1.0 / mean(1.0 / (1.0 + edges))` over a set of realized crossings —
/// the matched-mean denominator the derived arm's magnitude is discounted by.
/// Panics on an empty slice; every call site asserts non-emptiness first.
fn derive_d(edges: &[u32]) -> f64 {
    let mean_inv_discount: f64 =
        edges.iter().map(|&e| 1.0 / (1.0 + e as f64)).sum::<f64>() / edges.len() as f64;
    1.0 / mean_inv_discount
}

/// Keep only the tellings whose holder is in `keep`, preserving the input's
/// ascending-by-holder order (a plain filter never reorders).
fn filter_by_holder(v: &[HeldTelling], keep: &BTreeSet<EntityId>) -> Vec<HeldTelling> {
    v.iter()
        .filter(|t| keep.contains(&t.claim.holder))
        .cloned()
        .collect()
}

/// One world, built and read exactly once, with the derived arm's tellings
/// already computed for every foreign ending.
struct StoredSeed {
    /// The built world (owns the ledger every arm re-walks in pass 2).
    world: hornvale_kernel::World,
    /// The world's read-side context (lineage, contact graph, ladders, …).
    read: common::WorldRead,
    /// Foreign-ending subject -> the DERIVED arm's held tellings.
    derived: BTreeMap<EntityId, Vec<HeldTelling>>,
}

/// One row of the route co-primary enumeration (Step 4): a holder whose
/// derived-arm route is a discriminating one, with whether its route changed
/// under the constant-denominator arm.
#[derive(Clone)]
struct RouteRow {
    /// Which panel seed this holder belongs to.
    seed: u64,
    /// The foreign ending's subject.
    subject: EntityId,
    /// The holder.
    holder: EntityId,
    /// The derived-arm winning route's crossing edge-counts, in order.
    edge_counts: Vec<u32>,
    /// Whether the route (witness or crossings) differs under the
    /// constant-denominator arm.
    route_changed: bool,
}

/// THE CUPEL — the belief-ladder measurement's headline result.
///
/// claim: structural(seed: panel) — false-positive seed-loop flag; the loop
/// binds a census-panel prefix, not a search over seeds.
#[test]
#[ignore = "heavy: live-worldgen battery; deferred from the commit gate to the heavy set (decision 0132)"]
fn cupel_readout() {
    let components = hornvale_worldgen::WorldComponents::assemble().expect("components assemble");

    // ================= PASS 1: build + read + the DERIVED arm =================
    let mut skipped: Vec<u64> = Vec::new();
    let mut stored: Vec<StoredSeed> = Vec::new();
    let mut crossing_edges: Vec<u32> = Vec::new(); // panel-wide, for D_panel

    for seed in PANEL {
        let world = build(seed);
        let led = &world.ledger;
        let Some(read) = read_world(led, &components) else {
            skipped.push(seed);
            continue;
        };
        let mut derived: BTreeMap<EntityId, Vec<HeldTelling>> = BTreeMap::new();
        for e in read.endings.iter().filter(|e| e.is_foreign()) {
            let walk = Walk {
                ledger: led,
                lineage: &read.lineage,
                contact: &read.contact,
                policy: Transmission {
                    contact: Contact::WithRaidSeam,
                    ..Transmission::AS_SHIPPED
                },
            };
            let tellings = traced_variants_with_penalty(
                &walk,
                &read.ladders,
                &read.durations,
                RULE,
                e.subject,
                PREDICATE,
                PenaltyModel::Derived,
            );
            for t in &tellings {
                for c in &t.crossings {
                    crossing_edges.push(c.edges);
                }
            }
            derived.insert(e.subject, tellings);
        }
        stored.push(StoredSeed {
            world,
            read,
            derived,
        });
    }

    assert!(
        skipped.len() < PANEL.len(),
        "control: every panel seed was skipped for want of a year rung"
    );

    // Step 1 anti-vacuity: the derived arm reaches > 0 cross-people holders.
    let cross_people_holders: usize = stored
        .iter()
        .flat_map(|s| s.derived.values())
        .flatten()
        .filter(|t| !t.crossings.is_empty())
        .count();
    assert!(
        cross_people_holders > 0,
        "control: the derived arm reached no cross-people holder across the panel"
    );

    // ================= Step 2: re-derive D (panel-wide, and per-world) =================
    assert!(
        !crossing_edges.is_empty(),
        "control: the derived arm realized no crossing to derive D from"
    );
    let d_panel = derive_d(&crossing_edges);
    println!(
        "D_panel (re-derived on this tree) = {d_panel:.4}   (frozen §3 value: 4.7881, printed for comparison — not hard-coded)"
    );
    assert!(
        d_panel.is_finite() && d_panel > 1.0,
        "control: D_panel must be finite and > 1.0 (every crossing has edges >= 1, \
         so the mean discount is <= 0.5, so D >= 2): {d_panel}"
    );

    // ================= PASS 2: the Free and Constant(D_panel) arms, from the STORED worlds =================
    let mut dc = TailCounts::default(); // Derived vs Constant(D_panel) -- THE HEADLINE
    let mut cf = TailCounts::default(); // Constant(D_panel) vs Free
    let mut df = TailCounts::default(); // Derived vs Free -- context
    let mut dc_world = TailCounts::default(); // Derived vs Constant(D_world) -- robustness, printed not asserted
    let mut neg = TailCounts::default(); // Step 5: the negative control
    let mut footnote_flips = 0usize; // same-people in derived, cross-people in constant

    let mut multi_edge_rows: Vec<RouteRow> = Vec::new(); // >= 2 DISTINCT edge counts
    let mut multi_crossing_rows: Vec<RouteRow> = Vec::new(); // >= 2 crossings

    let mut world_d_report: Vec<(u64, f64)> = Vec::new();

    for s in &stored {
        let seed = s.world.seed.0;
        let led = &s.world.ledger;

        let world_edges: Vec<u32> = s
            .derived
            .values()
            .flatten()
            .flat_map(|t| t.crossings.iter().map(|c| c.edges))
            .collect();
        let d_world = if world_edges.is_empty() {
            f64::NAN
        } else {
            derive_d(&world_edges)
        };
        world_d_report.push((seed, d_world));

        for (subject, derived_tellings) in &s.derived {
            if derived_tellings.is_empty() {
                continue;
            }
            let walk = Walk {
                ledger: led,
                lineage: &s.read.lineage,
                contact: &s.read.contact,
                policy: Transmission {
                    contact: Contact::WithRaidSeam,
                    ..Transmission::AS_SHIPPED
                },
            };
            let free_tellings = traced_variants_with_penalty(
                &walk,
                &s.read.ladders,
                &s.read.durations,
                RULE,
                *subject,
                PREDICATE,
                PenaltyModel::Free,
            );
            let constant_tellings = traced_variants_with_penalty(
                &walk,
                &s.read.ladders,
                &s.read.durations,
                RULE,
                *subject,
                PREDICATE,
                PenaltyModel::ConstantDenominator(d_panel),
            );

            // S: holders whose DERIVED-arm winning route has non-empty crossings.
            let s_set: BTreeSet<EntityId> = derived_tellings
                .iter()
                .filter(|t| !t.crossings.is_empty())
                .map(|t| t.claim.holder)
                .collect();

            let derived_s = filter_by_holder(derived_tellings, &s_set);
            let constant_s = filter_by_holder(&constant_tellings, &s_set);
            let free_s = filter_by_holder(&free_tellings, &s_set);

            add_counts(&mut dc, &tail_counts(&derived_s, &constant_s));
            add_counts(&mut cf, &tail_counts(&constant_s, &free_s));
            add_counts(&mut df, &tail_counts(&derived_s, &free_s));

            if d_world.is_finite() {
                let constant_world_tellings = traced_variants_with_penalty(
                    &walk,
                    &s.read.ladders,
                    &s.read.durations,
                    RULE,
                    *subject,
                    PREDICATE,
                    PenaltyModel::ConstantDenominator(d_world),
                );
                let constant_world_s = filter_by_holder(&constant_world_tellings, &s_set);
                add_counts(&mut dc_world, &tail_counts(&derived_s, &constant_world_s));
            }

            // ---- Step 5: the negative control -- derived-arm same-people holders.
            let neg_set: BTreeSet<EntityId> = derived_tellings
                .iter()
                .filter(|t| t.crossings.is_empty())
                .map(|t| t.claim.holder)
                .collect();
            let derived_neg = filter_by_holder(derived_tellings, &neg_set);
            let constant_neg = filter_by_holder(&constant_tellings, &neg_set);
            add_counts(&mut neg, &tail_counts(&derived_neg, &constant_neg));

            // ---- footnote: same-people in derived, but cross-people in constant.
            let constant_by_holder: BTreeMap<EntityId, &HeldTelling> = constant_tellings
                .iter()
                .map(|t| (t.claim.holder, t))
                .collect();
            for holder in &neg_set {
                if constant_by_holder
                    .get(holder)
                    .is_some_and(|ct| !ct.crossings.is_empty())
                {
                    footnote_flips += 1;
                }
            }

            // ---- Step 4: the route co-primary, holder-resolved (an enumeration).
            for t in derived_tellings.iter().filter(|t| !t.crossings.is_empty()) {
                let edge_counts: Vec<u32> = t.crossings.iter().map(|c| c.edges).collect();
                let distinct: BTreeSet<u32> = edge_counts.iter().copied().collect();
                let n_crossings = edge_counts.len();
                if distinct.len() < 2 && n_crossings < 2 {
                    continue;
                }
                let route_changed = constant_by_holder
                    .get(&t.claim.holder)
                    .is_some_and(|ct| ct.witness != t.witness || ct.crossings != t.crossings);
                let row = RouteRow {
                    seed,
                    subject: *subject,
                    holder: t.claim.holder,
                    edge_counts,
                    route_changed,
                };
                if distinct.len() >= 2 {
                    multi_edge_rows.push(row.clone());
                }
                if n_crossings >= 2 {
                    multi_crossing_rows.push(row);
                }
            }
        }
    }

    // ================= THE HEADLINE TABLE =================
    println!("\n================= THE CUPEL — THE READOUT =================");
    println!(
        "panel                  : {} seeds, skipped {skipped:?}",
        PANEL.len()
    );
    println!("D_panel                : {d_panel:.4}");
    println!("D_world per seed (robustness, printed only):");
    for (seed, d) in &world_d_report {
        if d.is_finite() {
            println!("  seed {seed:>3}: D_world = {d:.4}");
        } else {
            println!("  seed {seed:>3}: D_world = n/a (this world realized no crossing)");
        }
    }

    let day_tail = frac(dc.day, dc.reached_both);
    let day_tail_world = frac(dc_world.day, dc_world.reached_both);

    println!("\n--- Derived vs Constant(D_panel), S = derived-arm crossers (THE HEADLINE) ---");
    println!(
        "  day    tail = {}/{} ({:.2}%)   <- PRIMARY",
        dc.day,
        dc.reached_both,
        pct(dc.day, dc.reached_both)
    );
    println!(
        "  width  tail = {}/{} ({:.2}%)   <- mechanism",
        dc.width,
        dc.reached_both,
        pct(dc.width, dc.reached_both)
    );
    println!(
        "  route  tail = {}/{} ({:.2}%)   <- co-primary (holder-resolved below, not a rate assertion)",
        dc.route,
        dc.reached_both,
        pct(dc.route, dc.reached_both)
    );
    println!(
        "  rung   tail = {}/{} ({:.2}%)",
        dc.rung,
        dc.reached_both,
        pct(dc.rung, dc.reached_both)
    );
    println!(
        "  hops   tail = {}/{} ({:.2}%)",
        dc.hops,
        dc.reached_both,
        pct(dc.hops, dc.reached_both)
    );
    println!(
        "  any    tail = {}/{} ({:.2}%)   only_a={} only_b={}",
        dc.any,
        dc.reached_both,
        pct(dc.any, dc.reached_both),
        dc.only_a,
        dc.only_b
    );

    println!("\n--- Constant(D_panel) vs Free, S = derived-arm crossers ---");
    println!(
        "  day    tail = {}/{} ({:.2}%)",
        cf.day,
        cf.reached_both,
        pct(cf.day, cf.reached_both)
    );
    println!(
        "  width  tail = {}/{} ({:.2}%)",
        cf.width,
        cf.reached_both,
        pct(cf.width, cf.reached_both)
    );
    println!(
        "  route  tail = {}/{} ({:.2}%)",
        cf.route,
        cf.reached_both,
        pct(cf.route, cf.reached_both)
    );

    println!("\n--- Derived vs Free (context), S = derived-arm crossers ---");
    println!(
        "  day    tail = {}/{} ({:.2}%)",
        df.day,
        df.reached_both,
        pct(df.day, df.reached_both)
    );
    println!(
        "  width  tail = {}/{} ({:.2}%)",
        df.width,
        df.reached_both,
        pct(df.width, df.reached_both)
    );
    println!(
        "  route  tail = {}/{} ({:.2}%)",
        df.route,
        df.reached_both,
        pct(df.route, df.reached_both)
    );

    println!(
        "\n--- ROBUSTNESS: Derived vs Constant(D_world), S = derived-arm crossers (printed, not asserted) ---"
    );
    println!(
        "  day    tail = {}/{} ({:.2}%)   (primary, D_panel: {:.2}%)",
        dc_world.day,
        dc_world.reached_both,
        pct(dc_world.day, dc_world.reached_both),
        pct(dc.day, dc.reached_both)
    );

    println!("\n--- FOOTNOTE (not in any denominator) ---");
    println!(
        "  same-people-in-derived but cross-people-in-constant flips = {footnote_flips} (expected ~0)"
    );

    println!(
        "\n--- ROUTE CO-PRIMARY (Step 4): >= 2 DISTINCT edge counts (the ~15 discriminating holders) ---"
    );
    if multi_edge_rows.is_empty() {
        println!("  (none)");
    }
    for r in &multi_edge_rows {
        println!(
            "  seed={:>3} subject={:>6} holder={:>6} edges={:?} route_changed={}",
            r.seed,
            r.subject.get(),
            r.holder.get(),
            r.edge_counts,
            r.route_changed
        );
    }
    println!("\n--- ROUTE CO-PRIMARY (Step 4): >= 2 crossings (the ~38) ---");
    if multi_crossing_rows.is_empty() {
        println!("  (none)");
    }
    for r in &multi_crossing_rows {
        println!(
            "  seed={:>3} subject={:>6} holder={:>6} edges={:?} route_changed={}",
            r.seed,
            r.subject.get(),
            r.holder.get(),
            r.edge_counts,
            r.route_changed
        );
    }

    println!("\n--- NEGATIVE CONTROL (Step 5): derived-arm same-people holders ---");
    let negative_tail = changed_tail(&neg);
    println!(
        "  sub-population = {} holders   negative_tail = {:.6}",
        neg.reached_both, negative_tail
    );

    println!("\n--- READING RULE (§4 bands; printed, NOT an assertion — see revised Step 6) ---");
    if day_tail <= 0.05 {
        println!(
            "  day tail {:.2}% <= 5% -> DECORATIVE on the day channel",
            day_tail * 100.0
        );
    } else {
        println!(
            "  day tail {:.2}% > 5% -> a route/width decomposition is needed to attribute this",
            day_tail * 100.0
        );
    }
    println!(
        "  (robustness arm D_world day tail: {:.2}%)",
        day_tail_world * 100.0
    );
    println!("==============================================================\n");

    // ================= ASSERTIONS (revised Step 6: controls and non-vacuity, NOT the day tail) =================
    assert!(
        neg.reached_both > 0,
        "control: the negative control sub-population (derived-arm same-people holders) is empty"
    );
    assert_eq!(
        negative_tail, 0.0,
        "instrument fires on a provably-inert change (a same-people route pays zero under \
         every PenaltyModel arm): negative_tail = {negative_tail}"
    );
    assert!(
        dc.reached_both > 0,
        "control: the primary denominator S (derived-arm crossers) is empty"
    );
    assert!(
        d_panel.is_finite() && d_panel > 1.0,
        "control: D_panel must be finite and > 1.0: {d_panel}"
    );
    assert!(
        dc.width > 0,
        "width non-vacuity: the Derived-vs-Constant width tail must be > 0 (the two \
         magnitudes must genuinely differ somewhere): {}",
        dc.width
    );
    assert!(
        cf.width > 0,
        "width non-vacuity: the Constant-vs-Free width tail must be > 0 (the constant \
         penalty must be genuinely non-zero somewhere): {}",
        cf.width
    );
    assert!(
        day_tail < 0.50,
        "a gross-breakage tripwire against substrate movement or a wiring bug, NOT the \
         hypothesis -- the real reading is the §4 band above plus the route decomposition: \
         day_tail = {day_tail}"
    );
}

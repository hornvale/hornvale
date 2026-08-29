//! The Cupel (Myth campaign 7), Task 0 — **substrate probe, grounds the
//! freeze.**
//!
//! Before any hypothesis is frozen (decision 0016; the Undertow's "measure the
//! substrate before freezing" rule), this probe answers whether the campaign is
//! even viable, and supplies the numbers the spec's frozen constant `D` and
//! preregistered day-channel criterion are set from. It builds NO instrument
//! and needs no new library code: it runs the SHIPPED derived arm
//! (`traced_variants_about_accumulating` under `(WithRaidSeam, ContactWeighted,
//! Multiplicative)`) over the 12-seed panel's foreign endings and reads each
//! winning route's realized crossings out of `HeldTelling::crossings`.
//!
//! The question the campaign lives or dies on: **does `edges_between` vary
//! enough across real winning-route crossings that a derived
//! `span(FINEST)/(1+edges)` penalty and a matched-mean CONSTANT-denominator
//! penalty could ever produce different beliefs?** If nearly every crossing
//! sits at one edge count, derived ≈ constant is the EXPECTED null (the Undertow
//! measured 41.6% at a single edge count), not a finding — and this probe says
//! so cheaply, before a campaign is spent building the readout.
//!
//! Five readings, each named so the spec can cite it:
//!
//! - **P1 — the edge-count distribution** over all winning-route crossings.
//!   Distinct values, the modal fraction (the Undertow's concentration figure),
//!   min/max. VIABILITY: more than one distinct edge count, modal fraction well
//!   under 100%.
//! - **P2 — the discriminating population** (the power analysis). Cross-people
//!   holders (>=1 crossing), and the sharper cut: holders whose winning route
//!   crosses at >=2 DIFFERENT edge counts. A null is only informative if this
//!   population is large.
//! - **P3 — the implied constant.** The per-crossing mean of `1/(1+edges)`
//!   panel-wide and per world — that mean IS `1/D` for the matched-mean
//!   constant-denominator control — plus its dispersion (how far individual
//!   crossings sit from the mean; large dispersion = room for the derivation to
//!   matter).
//! - **P4 — `unit` variation.** `span(FINEST)` per people per world, to see
//!   whether the finest rung is constant within a world (it should be: the
//!   world's shortest astronomical period) and how it ranges across worlds —
//!   which decides panel-wide vs per-world `D`.
//! - **P5 — a day-channel upper bound (approximation).** Of cross-people
//!   holders, the fraction sitting within one `unit` (the largest a crossing
//!   penalty can be) of a rung boundary under the derived arm — an UPPER BOUND
//!   on how many holders any penalty change could move to a different remembered
//!   day. Approximate because the constant arm's own widths (Task 1) will
//!   differ; it bounds the day channel from above, cheaply.
//!
//! ## Cost.
//!
//! A heavy `#[ignore]`d battery: one derived traced walk per foreign ending on
//! the 12-seed panel, the same build cost as `touchstone_readout`'s negative
//! control. Run it by name:
//! `cargo nextest run -p hornvale-hearsay --run-ignored all -E 'test(cupel_substrate_probe)'`.

mod common;

use common::{PANEL, PREDICATE, RULE, build, pct, read_world};
use hornvale_hearsay::contact::Contact;
use hornvale_hearsay::traced::traced_variants_about_accumulating;
use hornvale_hearsay::transmission::{Crossing, Transmission, Walk};
use hornvale_kernel::Precision;
use std::collections::BTreeMap;

/// `1/(1+edges)` — the derived discount factor, the thing a constant denominator
/// would flatten. Unit-free, so the whole edge/discount analysis needs no ladder.
fn discount(edges: u32) -> f64 {
    1.0 / (1.0 + edges as f64)
}

/// Mean and population standard deviation of a slice, or `(0.0, 0.0)` when empty.
fn mean_sd(xs: &[f64]) -> (f64, f64) {
    if xs.is_empty() {
        return (0.0, 0.0);
    }
    let n = xs.len() as f64;
    let mean = xs.iter().sum::<f64>() / n;
    let var = xs.iter().map(|x| (x - mean) * (x - mean)).sum::<f64>() / n;
    (mean, var.sqrt())
}

/// THE SUBSTRATE PROBE. Reads the derived arm's realized crossings across the
/// panel and reports the five numbers the freeze rests on.
///
/// claim: structural(seed: panel) — false-positive seed-loop flag; the loop
/// binds a census-panel prefix, not a search over seeds.
#[test]
#[ignore = "probe: preregistration/viability probe informing the crossing-penalty freeze; run by hand (Myth campaign 7, The Cupel, Task 0, answered its question; demoted by The Governor 2026-08-28)"]
fn cupel_substrate_probe() {
    let components = hornvale_worldgen::WorldComponents::assemble().expect("components assemble");
    let mut skipped: Vec<u64> = Vec::new();

    // P1: every winning-route crossing's edge count, panel-wide.
    let mut edge_hist: BTreeMap<u32, usize> = BTreeMap::new();
    // P3: every crossing's discount 1/(1+edges), panel-wide (per-crossing), and
    // per-world means for the per-world D and unit-variation read.
    let mut all_discounts: Vec<f64> = Vec::new();
    // P2: per-holder crossing structure, over cross-people holders panel-wide.
    let mut cross_people_holders = 0usize; // >=1 crossing on the winning route
    let mut multi_crossing_holders = 0usize; // >=2 crossings
    let mut discriminating_holders = 0usize; // >=2 DISTINCT edge counts
    let mut n_crossings_hist: BTreeMap<usize, usize> = BTreeMap::new();
    // P4: per-world finest span per people.
    let mut world_finest: Vec<(u64, Vec<(String, f64)>)> = Vec::new();
    // P5: day-channel upper bound. The remembered day changes only if the two
    // arms' widths straddle a rung boundary, and |W_derived - W_constant| is
    // bounded per crossing by `unit` (both penalties are in [0, unit]; derived
    // is <= unit/2 since edges >= 1, constant = unit/D). So a holder's day can
    // differ between the arms ONLY if its derived width sits within (crossings *
    // unit) of the NEAREST rung boundary — up OR down. Checking only the upward
    // gap (an earlier draft) undercounts; this checks both.
    let mut p5_denom = 0usize; // cross-people holders scored
    let mut p5_within_budget = 0usize; // nearest boundary within (n_crossings * unit)
    let mut p5_within_half = 0usize; // tighter: within (n_crossings * unit/2)

    for seed in PANEL {
        let world = build(seed);
        let led = &world.ledger;
        let Some(read) = read_world(led, &components) else {
            skipped.push(seed);
            continue;
        };

        // P4: finest span for each named people this world.
        let mut finest_rows: Vec<(String, f64)> = Vec::new();
        {
            let mut peoples: Vec<String> = read.people_of.values().cloned().collect();
            peoples.sort();
            peoples.dedup();
            for p in peoples {
                let ladder = read.ladders.for_people(&p);
                let finest = ladder
                    .span(Precision::FINEST)
                    .map(|d| d.get())
                    .unwrap_or(0.0);
                finest_rows.push((p, finest));
            }
        }
        world_finest.push((seed, finest_rows));

        let mut world_discounts: Vec<f64> = Vec::new();

        for e in read.endings.iter().filter(|e| e.is_foreign()) {
            let walk = Walk {
                ledger: led,
                lineage: &read.lineage,
                contact: &read.contact,
                policy: Transmission {
                    contact: Contact::WithRaidSeam,
                    crossing: Crossing::ContactWeighted,
                    ..Transmission::AS_SHIPPED
                },
            };
            let held = traced_variants_about_accumulating(
                &walk,
                &read.ladders,
                &read.durations,
                RULE,
                e.subject,
                PREDICATE,
            );

            for h in &held {
                if h.crossings.is_empty() {
                    continue; // same-people holder: pays zero under any magnitude
                }
                cross_people_holders += 1;
                let n = h.crossings.len();
                *n_crossings_hist.entry(n).or_default() += 1;
                if n >= 2 {
                    multi_crossing_holders += 1;
                }
                let distinct: std::collections::BTreeSet<u32> =
                    h.crossings.iter().map(|c| c.edges).collect();
                if distinct.len() >= 2 {
                    discriminating_holders += 1;
                }
                for c in &h.crossings {
                    *edge_hist.entry(c.edges).or_default() += 1;
                    let d = discount(c.edges);
                    all_discounts.push(d);
                    world_discounts.push(d);
                }

                // P5: distance from this holder's derived width to the NEAREST
                // rung boundary (up or down), against the width-shift budget
                // (n_crossings * unit). The ladder is the originating witness's
                // people's.
                let witness_people = read.people_of.get(&h.witness).cloned().unwrap_or_default();
                let ladder = read.ladders.for_people(&witness_people);
                let unit = ladder
                    .span(Precision::FINEST)
                    .map(|d| d.get())
                    .unwrap_or(0.0);
                let cur = hornvale_hearsay::accumulate::precision_at(ladder, h.width);
                let cur_span = ladder.span(cur).map(|d| d.get()).unwrap_or(0.0);
                let up_gap = ladder
                    .span(Precision(cur.rung() + 1))
                    .map(|d| d.get() - h.width)
                    .unwrap_or(f64::INFINITY); // saturated at the coarsest rung
                let down_gap = h.width - cur_span; // >= 0
                let nearest = up_gap.min(down_gap);
                let budget = n as f64 * unit;
                p5_denom += 1;
                if nearest <= budget {
                    p5_within_budget += 1;
                }
                if nearest <= budget * 0.5 {
                    p5_within_half += 1;
                }
            }
        }

        let (wmean, _) = mean_sd(&world_discounts);
        // Stash the per-world mean into world_finest's seed row via a parallel
        // print below; recomputed there from all_discounts is panel-wide, so
        // keep the per-world figure here.
        println!(
            "  seed {seed:>2}: crossings={:<5} per-world mean 1/(1+edges)={:.4} (=> D={:.3})",
            world_discounts.len(),
            wmean,
            if wmean > 0.0 { 1.0 / wmean } else { 0.0 },
        );
    }

    // ============================ THE READINGS ============================
    println!("\n================= THE CUPEL — SUBSTRATE PROBE =================");
    println!(
        "panel                 : {} seeds, skipped {skipped:?}",
        PANEL.len()
    );

    // P1 — edge-count distribution.
    let total_crossings: usize = edge_hist.values().sum();
    let modal = edge_hist.values().copied().max().unwrap_or(0);
    println!("\n--- P1: edge-count distribution over winning-route crossings ---");
    println!("  total crossings       : {total_crossings}");
    println!("  distinct edge counts  : {}", edge_hist.len());
    println!(
        "  modal fraction        : {:.2}%  (the Undertow saw ~41.6% at one edge count)",
        pct(modal, total_crossings)
    );
    println!("  histogram (edges -> count -> discount 1/(1+edges)):");
    for (edges, count) in &edge_hist {
        println!(
            "    edges={edges:<4} n={count:<6} ({:>5.2}%)  discount={:.4}",
            pct(*count, total_crossings),
            discount(*edges),
        );
    }

    // P2 — discriminating population (power).
    println!("\n--- P2: discriminating population (the power analysis) ---");
    println!("  cross-people holders (>=1 crossing)       : {cross_people_holders}");
    println!("  multi-crossing holders (>=2 crossings)    : {multi_crossing_holders}");
    println!(
        "  DISCRIMINATING (>=2 distinct edge counts) : {discriminating_holders}  <- a null is only informative if this is large"
    );
    println!("  crossings-per-holder histogram:");
    for (n, count) in &n_crossings_hist {
        println!("    {n} crossing(s): {count} holders");
    }

    // P3 — implied constant and dispersion.
    let (mean_d, sd_d) = mean_sd(&all_discounts);
    println!("\n--- P3: the matched-mean constant (per-crossing weighting) ---");
    println!(
        "  panel-wide mean 1/(1+edges) = {mean_d:.6}  =>  D = {:.4}",
        if mean_d > 0.0 { 1.0 / mean_d } else { 0.0 }
    );
    println!("  dispersion sd = {sd_d:.6}  (0 => every crossing identical => certain null)");
    if let (Some(lo), Some(hi)) = (
        all_discounts.iter().cloned().reduce(f64::min),
        all_discounts.iter().cloned().reduce(f64::max),
    ) {
        println!("  discount range = [{lo:.4}, {hi:.4}]");
    }

    // P4 — unit variation.
    println!("\n--- P4: unit = span(FINEST) per people per world ---");
    let mut world_units: Vec<f64> = Vec::new();
    for (seed, rows) in &world_finest {
        let uniq: std::collections::BTreeSet<u64> = rows.iter().map(|(_, f)| f.to_bits()).collect();
        let shown: Vec<String> = rows.iter().map(|(p, f)| format!("{p}={f:.3}")).collect();
        for (_, f) in rows {
            world_units.push(*f);
        }
        println!(
            "  seed {seed:>2}: {} distinct finest span(s){}  [{}]",
            uniq.len(),
            if uniq.len() == 1 {
                " (constant in-world)"
            } else {
                " (VARIES in-world!)"
            },
            shown.join(", "),
        );
    }
    if let (Some(lo), Some(hi)) = (
        world_units.iter().cloned().reduce(f64::min),
        world_units.iter().cloned().reduce(f64::max),
    ) {
        println!("  finest span across all worlds/peoples: [{lo:.3}, {hi:.3}] days");
    }

    // P5 — day-channel upper bound.
    println!("\n--- P5: day-channel upper bound (nearest rung boundary, derived-arm widths) ---");
    println!(
        "  within (n_crossings * unit)   of a boundary: {p5_within_budget} / {p5_denom} ({:.3}%)  <- valid UPPER bound on day_changed",
        pct(p5_within_budget, p5_denom)
    );
    println!(
        "  within (n_crossings * unit/2) of a boundary: {p5_within_half} / {p5_denom} ({:.3}%)  <- tighter (derived penalty <= unit/2)",
        pct(p5_within_half, p5_denom)
    );
    println!(
        "  (ONLY these holders could have their remembered DAY differ between derived and a matched constant;"
    );
    println!("   the rest are quantized onto the same rung regardless of the penalty magnitude.)");
    println!("==============================================================\n");

    // ===========================  ANTI-VACUITY  ===========================
    assert!(
        skipped.len() < PANEL.len(),
        "control: every panel seed was skipped for want of a year rung"
    );
    assert!(
        total_crossings > 0,
        "control: the derived arm produced no winning-route crossings on the panel — \
         the probe measured nothing"
    );
    // NOT a hypothesis assertion — this probe informs the freeze, it does not
    // test it. The one structural claim worth failing on: the panel must offer
    // SOME edge-count variation, or the whole campaign is the expected null and
    // the spec should say so before any readout is built. Printed loudly above;
    // asserted here so a degenerate substrate reddens rather than passing quietly.
    assert!(
        edge_hist.len() >= 2,
        "VIABILITY: only one distinct edge count across the panel's winning-route \
         crossings — derived and a matched constant are identical BY CONSTRUCTION, \
         so the campaign's answer is the expected null with no readout needed. \
         This is a FINDING; record it in the spec and stop, do not build the readout."
    );
}

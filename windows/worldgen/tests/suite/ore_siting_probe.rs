//! ORE SITING — settling The Winze Task 2's two open questions (spec §B.6).
//!
//! Changes no production code. Task 1 (`ore_separation_probe.rs`) killed the
//! reclassification design; spec §B.3 replaces it with a mine FOUNDED as a
//! daughter on a second objective inside `Bake::grow`'s existing site choice.
//! That design leaves two numbers open and this probe measures both:
//!
//! 1. **The ore scoring function must discriminate.** Prospectivity is a
//!    near-constant floor (75% of seed 42's land inside a 0.0067-wide band),
//!    so a naive `max_by(prospectivity)` over a settlement's ~6 neighbours
//!    would tie and the lowest-`Vertex` tie-break would silently become the
//!    siting rule. This measures the distinct-score count of the *candidate
//!    neighbour set*, not of the whole map.
//! 2. **How often an expansion is a working.** The population the rate
//!    applies to: how many expansions a world makes, and what share of them
//!    have any ore-bearing candidate at all.
//!
//! The candidate set here is a PROXY: the land neighbours of an occupied
//! vertex in the geosphere adjacency, where the live path scans the era
//! graph's traversable neighbours and filters to vacant-and-feedable. Both
//! filters only shrink the set, so a qualifying-set size measured here is an
//! upper bound on the live one — which is the safe direction for the tie
//! question (a set of size <= 1 cannot tie at all).
//!
//! # What this probe measures, 2026-08-29 (seeds 42 / 7 / 1234)
//!
//! ```text
//! land >= 0.24                     5.11% / 9.64% / 11.31%
//! land >= 0.30                     5.11% / 9.56% / 11.31%   <- a plateau, not a knife edge
//! occupied vertices sampled          416  /   314  /   130
//! neighbour sets that are ONE score     0  /     0  /     0   (a total tie is never the case)
//! ARGMAX of the UNFILTERED set ties  40.9% / 32.2% / 29.2%   <- the naive max_by(prospectivity)
//! ARGMAX of the set filtered >= 0.24  0.0% /  0.0% /  0.0%
//! sites with a candidate >= 0.24      2.6% /  6.4% /  16.2%
//! ```
//!
//! # AND WHAT IT IS WRONG ABOUT, WHICH IS THE MORE USEFUL HALF
//!
//! The proxy over-estimates the live qualifying population by roughly 30x on
//! seed 42, and the reason is not the two filters this header names. It is the
//! sampling frame: this probe samples SITES, one per occupied vertex of the
//! finished world, while the ore objective is offered a candidate set once per
//! `Bake::grow` DAUGHTER THROW — a different population, distributed over the
//! whole history and concentrated wherever communities are comfortable enough
//! to expand, which is not where ore is.
//!
//! Measured directly, by instrumenting `Bake::grow` on the branch (a temporary
//! `eprintln!` of every throw's candidate set, removed before commit), against
//! the implementation that landed:
//!
//! ```text
//!                                            seed 42   seed 7   seed 1234
//! daughter throws                                466      292          23
//!   ... with at least one vacant candidate       400      261          21
//! best vacant candidate's prospectivity, p50  0.0603   0.0616      0.0626
//!                                          p90  0.0894   0.1980      0.4919
//!                                          max  0.5154   0.6627      0.5163
//! throws with a vacant candidate >= 0.24           4       23           4
//! ```
//!
//! **Task 1's finding transmits one hop.** Settlements are under-represented
//! in high-ore ground, and so are their immediate neighbourhoods: for half of
//! all daughter throws the *richest* vacant neighbour is at the field's barren
//! floor. That, and not the rate, is what bounds the mine population of the
//! §B.3 design — see `mines_exist.rs` for what it comes to.
//!
//! **These are the numbers of the WORLD THE OBJECTIVE PRODUCES, not of the
//! world before it**, and the difference is not small: measured against the
//! pre-campaign bake the same instrument read 489 / 420 / 26 throws with
//! 1 / 9 / 3 qualifying. A world that founds workings expands differently, so
//! the population the rate applies to is itself moved by the rate. Quoting the
//! pre-campaign figures as if they described the shipped path would be the
//! ordinary version of that mistake.
#![allow(clippy::disallowed_methods)]

use hornvale_astronomy::SkyPins;
use hornvale_kernel::Seed;
use hornvale_terrain::TerrainPins;
use hornvale_worldgen::{SettlementPins, build_world, occupations_by_vertex, terrain_of};

/// Seeds the campaign preregisters on.
const SEEDS: [u64; 3] = [42, 7, 1234];

/// Nearest-rank percentile of an ascending-sorted slice.
fn pct(sorted: &[f64], q: f64) -> f64 {
    if sorted.is_empty() {
        return f64::NAN;
    }
    let i = ((sorted.len() - 1) as f64 * q).round() as usize;
    sorted[i]
}

/// claim: readout(off-gate, heavy:, prints only) — the candidate-neighbour
/// prospectivity structure a `Function::Mine` founding would score on.
#[test]
#[ignore = "probe: The Winze Task 2's two open questions (spec B.6); run by hand"]
fn ore_siting_probe() {
    // Structural cut points. `lithology::prospectivity` is
    // `0.6*setting + 0.3*unrest + 0.1*grade`, and `setting` is a step
    // function of the boundary: 0.1 off one, 0.4 at a continental collision,
    // 0.5 at a rift/ridge, 0.7 at an arc/coastal range. So 0.06 is the
    // absolute floor and 0.24 is exactly where "on a plate boundary" begins.
    // 0.06 is the field's absolute floor, so a `0.0` cut IS the naive
    // `max_by(prospectivity)` over the whole neighbour set — the reading the
    // plan warns would let the lowest-`Vertex` tie-break decide the siting.
    let cuts = [0.0_f64, 0.10, 0.12, 0.15, 0.20, 0.24, 0.30];
    for seed_value in SEEDS {
        let seed = Seed(seed_value);
        let world = build_world(
            seed,
            &SkyPins::default(),
            &TerrainPins::default(),
            &SettlementPins::default(),
        )
        .expect("probe seed builds");
        let terrain = terrain_of(&world).expect("terrain");
        let geo = terrain.geosphere();

        let mut land: Vec<f64> = geo
            .vertices()
            .filter(|&c| !terrain.is_ocean(c))
            .map(|c| terrain.prospectivity_at(c))
            .collect();
        land.sort_by(f64::total_cmp);
        println!("\n== seed {seed_value} ==  land vertices {}", land.len());
        println!(
            "  land prospectivity: min {:.4} p50 {:.4} p75 {:.4} p90 {:.4} p95 {:.4} p99 {:.4} max {:.4}",
            land.first().copied().unwrap_or(f64::NAN),
            pct(&land, 0.50),
            pct(&land, 0.75),
            pct(&land, 0.90),
            pct(&land, 0.95),
            pct(&land, 0.99),
            land.last().copied().unwrap_or(f64::NAN),
        );
        for c in cuts {
            let n = land.iter().filter(|&&v| v >= c).count();
            println!(
                "    land >= {c:.2}: {n} / {} ({:.2}%)",
                land.len(),
                n as f64 / land.len().max(1) as f64 * 100.0
            );
        }

        // The candidate-neighbour structure, one sample per occupied vertex.
        let by_vertex = occupations_by_vertex(&world);
        let mut n_sites = 0usize;
        let mut distinct_hist: Vec<usize> = vec![0; 12];
        let mut nbr_hist: Vec<usize> = vec![0; 12];
        // Per cut: how many sites have >=1 qualifying neighbour, and the size
        // distribution of that qualifying set.
        let mut qual_any = vec![0usize; cuts.len()];
        let mut qual_size_hist: Vec<Vec<usize>> = vec![vec![0; 12]; cuts.len()];
        let mut qual_ties = vec![0usize; cuts.len()];
        for &vertex in by_vertex.keys() {
            if terrain.is_ocean(vertex) {
                continue;
            }
            let nbrs: Vec<hornvale_kernel::Vertex> = geo
                .neighbors(vertex)
                .iter()
                .copied()
                .filter(|&n| !terrain.is_ocean(n))
                .collect();
            if nbrs.is_empty() {
                continue;
            }
            n_sites += 1;
            let mut scores: Vec<f64> = nbrs.iter().map(|&n| terrain.prospectivity_at(n)).collect();
            nbr_hist[scores.len().min(11)] += 1;
            scores.sort_by(f64::total_cmp);
            let mut d = scores.clone();
            d.dedup_by(|a, b| a == b);
            distinct_hist[d.len().min(11)] += 1;
            for (ci, &c) in cuts.iter().enumerate() {
                let q: Vec<f64> = scores.iter().copied().filter(|&v| v >= c).collect();
                if !q.is_empty() {
                    qual_any[ci] += 1;
                    qual_size_hist[ci][q.len().min(11)] += 1;
                    // Does the ARGMAX tie? Count how many qualifying
                    // candidates carry the maximum score.
                    let max = q.iter().copied().fold(f64::NEG_INFINITY, f64::max);
                    if q.iter().filter(|&&v| v == max).count() > 1 {
                        qual_ties[ci] += 1;
                    }
                }
            }
        }
        println!("  occupied land vertices sampled: {n_sites}");
        println!("  neighbour-set SIZE histogram:     {nbr_hist:?}");
        println!("  neighbour-set DISTINCT-SCORE histogram: {distinct_hist:?}");
        let all_tie = distinct_hist[1];
        println!(
            "    sites whose whole neighbour set is ONE score (a total tie): {all_tie} / {n_sites} ({:.1}%)",
            all_tie as f64 / n_sites.max(1) as f64 * 100.0
        );
        for (ci, &c) in cuts.iter().enumerate() {
            println!(
                "  cut {c:.2}: {} / {n_sites} sites have a qualifying neighbour ({:.1}%); qualifying-set size hist {:?}; argmax TIES {} ({:.1}% of qualifying sites)",
                qual_any[ci],
                qual_any[ci] as f64 / n_sites.max(1) as f64 * 100.0,
                qual_size_hist[ci],
                qual_ties[ci],
                qual_ties[ci] as f64 / qual_any[ci].max(1) as f64 * 100.0,
            );
        }

        // The population the rate applies to: total occupations, and how
        // many of them were founded as an expansion of an existing one.
        let total: usize = by_vertex.values().map(|v| v.len()).sum();
        let expansions = by_vertex
            .values()
            .flatten()
            .filter(|r| matches!(r.founded_from, hornvale_history::record::Founding::From(_)))
            .count();
        println!(
            "  total occupations in this world: {total}; founded as an EXPANSION of another: {expansions} ({:.1}%)",
            expansions as f64 / total.max(1) as f64 * 100.0
        );
    }
}

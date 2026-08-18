//! THE UNDERWORLD, Task 1: what does the rock column look like in KELVIN?
//!
//! Measurement only. The delve ladder (spec §4.1) places its rungs at
//! temperature offsets above the surface datum rather than at round metres,
//! so the rung table cannot be authored until the distribution of
//! (depth, gradient) over cave-bearing cells is known. This probe prints it.
//!
//! It asserts nothing. Every check is a build/lookup `expect`; the result is
//! the printed table. Recorded into the module doc when it has been run.
//!
//! ## Measured, 2026-08-16, seeds 42 / 7 / 1234 — BEFORE Task 1b
//!
//! Depth is `top_depth_m(deepest_band)`, a band index. Two occupied classes;
//! the three middle buckets hold 1–25 caves out of 874–1681 between them. This
//! reading is what falsified spec §4.1's premise and occasioned §4.0.
//!
//! **Kept deliberately.** The pair of tables is this campaign's evidence and
//! deleting the first one destroys it.
//!
//! ```text
//! seed 42: caves=874 bands(Reg,Cov,Bas,Roo,Und)=[0, 176, 490, 208, 0]
//!   gradient K/km  p10=22.238 p50=24.419 p90=26.136
//!   deltaT p10 = 0.000 K
//!   deltaT p25 = 0.000 K
//!   deltaT p50 = 0.005 K
//!   deltaT p75 = 2.477 K
//!   deltaT p90 = 364.274 K
//!   deltaT p99 = 450.034 K
//!   [  0.0,   2.0) K : 655
//!   [  2.0,  10.0) K : 6
//!   [ 10.0,  25.0) K : 4
//!   [ 25.0,  50.0) K : 1
//!   [ 50.0,   inf) K : 208
//! seed 7: caves=1681 bands(Reg,Cov,Bas,Roo,Und)=[0, 754, 289, 638, 0]
//!   gradient K/km  p10=22.760 p50=25.004 p90=27.603
//!   deltaT p10 = 0.000 K
//!   deltaT p25 = 0.000 K
//!   deltaT p50 = 0.032 K
//!   deltaT p75 = 319.610 K
//!   deltaT p90 = 385.385 K
//!   deltaT p99 = 489.552 K
//!   [  0.0,   2.0) K : 998
//!   [  2.0,  10.0) K : 24
//!   [ 10.0,  25.0) K : 16
//!   [ 25.0,  50.0) K : 4
//!   [ 50.0,   inf) K : 639
//! seed 1234: caves=1266 bands(Reg,Cov,Bas,Roo,Und)=[0, 220, 602, 444, 0]
//!   gradient K/km  p10=21.795 p50=23.082 p90=27.780
//!   deltaT p10 = 0.000 K
//!   deltaT p25 = 0.000 K
//!   deltaT p50 = 0.014 K
//!   deltaT p75 = 286.452 K
//!   deltaT p90 = 366.190 K
//!   deltaT p99 = 457.014 K
//!   [  0.0,   2.0) K : 786
//!   [  2.0,  10.0) K : 25
//!   [ 10.0,  25.0) K : 9
//!   [ 25.0,  50.0) K : 2
//!   [ 50.0,   inf) K : 444
//! ```
//!
//! ## Measured, 2026-08-16, seeds 42 / 7 / 1234 — AFTER Task 1b
//!
//! Depth is now `Cave::depth_reach_m`, a budget in metres derived purely from
//! lithology and the column (`hornvale_terrain::cave_depth`). Same seeds, same
//! probe, same buckets; the only thing that moved is the depth coordinate.
//!
//! **The three middle buckets go from 1–25 caves to 53–639.** The parenthesised
//! `band-derived` figure on each line is the in-run control described at the
//! call site — the band-top coordinate applied to the NEW band assignment, not
//! the pre-1b reading — and it still puts 832–1500 caves in `[0, 2) K`, which
//! is the point: a band top is not a depth quantity even when the band is
//! right.
//!
//! Two things to carry into §4.1's rung table rather than read as defects:
//! `deepest_band` no longer reaches `Roots` on any cave (a metre budget capped
//! at 3 km cannot, when `Roots` starts near 14 km), and `[50, ∞) K` grew rather
//! than shrank, because fault voids in competent rock are genuinely deep.
//!
//! ```text
//! seed 42: caves=874 bands(Reg,Cov,Bas,Roo,Und)=[0, 19, 855, 0, 0]
//!   gradient K/km  p10=22.238 p50=24.419 p90=26.136
//!   reach m        p10=200.0 p25=399.5 p50=483.5 p75=1502.0 p90=2271.9 p99=2398.9
//!   deltaT p10 = 5.581 K
//!   deltaT p25 = 9.973 K
//!   deltaT p50 = 10.735 K
//!   deltaT p75 = 39.063 K
//!   deltaT p90 = 55.044 K
//!   deltaT p99 = 57.974 K
//!   [  0.0,   2.0) K : 77  (band-derived: 832)
//!   [  2.0,  10.0) K : 149  (band-derived: 39)
//!   [ 10.0,  25.0) K : 381  (band-derived: 3)
//!   [ 25.0,  50.0) K : 53  (band-derived: 0)
//!   [ 50.0,   inf) K : 214  (band-derived: 0)
//! seed 7: caves=1681 bands(Reg,Cov,Bas,Roo,Und)=[0, 46, 1635, 0, 0]
//!   gradient K/km  p10=22.760 p50=25.004 p90=27.603
//!   reach m        p10=215.3 p25=227.9 p50=1408.6 p75=2246.6 p90=2474.3 p99=2601.2
//!   deltaT p10 = 5.870 K
//!   deltaT p25 = 6.091 K
//!   deltaT p50 = 33.266 K
//!   deltaT p75 = 54.750 K
//!   deltaT p90 = 56.723 K
//!   deltaT p99 = 59.621 K
//!   [  0.0,   2.0) K : 84  (band-derived: 1500)
//!   [  2.0,  10.0) K : 639  (band-derived: 131)
//!   [ 10.0,  25.0) K : 81  (band-derived: 48)
//!   [ 25.0,  50.0) K : 150  (band-derived: 2)
//!   [ 50.0,   inf) K : 727  (band-derived: 0)
//! seed 1234: caves=1266 bands(Reg,Cov,Bas,Roo,Und)=[0, 13, 1253, 0, 0]
//!   gradient K/km  p10=21.795 p50=23.082 p90=27.780
//!   reach m        p10=201.7 p25=461.1 p50=1201.2 p75=2311.7 p90=2694.0 p99=2702.6
//!   deltaT p10 = 5.765 K
//!   deltaT p25 = 10.487 K
//!   deltaT p50 = 26.963 K
//!   deltaT p75 = 55.902 K
//!   deltaT p90 = 58.882 K
//!   deltaT p99 = 60.622 K
//!   [  0.0,   2.0) K : 91  (band-derived: 1117)
//!   [  2.0,  10.0) K : 162  (band-derived: 110)
//!   [ 10.0,  25.0) K : 348  (band-derived: 36)
//!   [ 25.0,  50.0) K : 129  (band-derived: 3)
//!   [ 50.0,   inf) K : 536  (band-derived: 0)
//! ```
//!
//! Test fixture (decision 0092): calls the composition-root entry points
//! directly, the sanctioned posture for this crate's live-worldgen batteries.
#![allow(clippy::disallowed_methods)]

use hornvale_astronomy::SkyPins;
use hornvale_terrain::{BandKind, TerrainPins};
use hornvale_worldgen::{
    BuildDepth, SettlementPins, SkyChoice, WorldComponents, build_world_to_with_artifacts,
};

/// Seeds this campaign preregisters on (spec §5).
const SEEDS: [u64; 3] = [42, 7, 1234];

/// Percentile of an ascending slice.
fn pct(sorted: &[f64], q: f64) -> f64 {
    if sorted.is_empty() {
        return f64::NAN;
    }
    sorted[((sorted.len() - 1) as f64 * q).round() as usize]
}

/// claim: readout(off-gate, heavy:, prints only, no assertion) — the joint
/// distribution of cave depth and geothermal gradient over cave-bearing land
/// cells, expressed as ΔT above the surface datum. The input to spec §4.1's
/// rung table; not a gate on any value.
#[test]
#[ignore = "heavy: live-worldgen battery; deferred from the commit gate to the heavy set (decision 0132)"]
fn how_hot_is_a_cave() {
    // The sanctioned test-fixture posture (decision 0092), copied from
    // `deep_realm_substrate.rs::measure_one` — seven arguments including the
    // assembled components, returning `BuildArtifacts` rather than a tuple.
    let wc = WorldComponents::assemble().expect("canonical registries are well-formed");
    for seed_value in SEEDS {
        let seed = hornvale_kernel::Seed(seed_value);
        let artifacts = build_world_to_with_artifacts(
            seed,
            &SkyPins::default(),
            SkyChoice::Generated,
            &TerrainPins::default(),
            &SettlementPins::default(),
            &wc,
            BuildDepth::Terrain,
        )
        .expect("probe seed builds");
        let terrain = artifacts
            .terrain
            .expect("terrain is Some at BuildDepth::Terrain");
        let geo = terrain.geosphere();

        let mut dt_samples: Vec<f64> = Vec::new();
        let mut dt_band_samples: Vec<f64> = Vec::new();
        let mut reaches: Vec<f64> = Vec::new();
        let mut gradients: Vec<f64> = Vec::new();
        let mut band_hist = [0usize; 5];

        for cell in geo.cells() {
            if terrain.is_ocean(cell) {
                continue;
            }
            let Some(cave) = terrain.cave_at(cell) else {
                continue;
            };
            let gradient = terrain.geothermal_gradient_at(cell).get();
            gradients.push(gradient);
            let band_index = match cave.deepest_band {
                BandKind::Regolith => 0,
                BandKind::Cover => 1,
                BandKind::Basement => 2,
                BandKind::Roots => 3,
                BandKind::Underneath => 4,
            };
            band_hist[band_index] += 1;
            // ΔT at the cave's deepest reach: its depth budget in metres
            // (Task 1b), converted to km, times this cell's gradient.
            reaches.push(cave.depth_reach_m);
            dt_samples.push(gradient * (cave.depth_reach_m / 1000.0));
            // The band-top coordinate, kept as an in-run control: the column's
            // own top-depth for the deepest band. **This is NOT the pre-1b
            // reading** — `deepest_band` itself moved in Task 1b, so this is
            // the old coordinate applied to the new band assignment. What it
            // shows is the narrower, still-live claim: a band top is not a
            // depth quantity even when the band is right. The pre-1b table in
            // this module's doc is the actual before-reading.
            let column = terrain.column_at(cell);
            dt_band_samples.push(gradient * (column.bands[band_index].top_depth_m / 1000.0));
        }

        dt_samples.sort_by(f64::total_cmp);
        dt_band_samples.sort_by(f64::total_cmp);
        reaches.sort_by(f64::total_cmp);
        gradients.sort_by(f64::total_cmp);
        println!(
            "seed {seed_value}: caves={} bands(Reg,Cov,Bas,Roo,Und)={band_hist:?}",
            dt_samples.len()
        );
        println!(
            "  gradient K/km  p10={:.3} p50={:.3} p90={:.3}",
            pct(&gradients, 0.10),
            pct(&gradients, 0.50),
            pct(&gradients, 0.90)
        );
        println!(
            "  reach m        p10={:.1} p25={:.1} p50={:.1} p75={:.1} p90={:.1} p99={:.1}",
            pct(&reaches, 0.10),
            pct(&reaches, 0.25),
            pct(&reaches, 0.50),
            pct(&reaches, 0.75),
            pct(&reaches, 0.90),
            pct(&reaches, 0.99)
        );
        for q in [0.10, 0.25, 0.50, 0.75, 0.90, 0.99] {
            println!(
                "  deltaT p{:>2.0} = {:.3} K",
                q * 100.0,
                pct(&dt_samples, q)
            );
        }
        // How many cave cells fall in each candidate band of the spec's
        // ILLUSTRATIVE table. Printed so the real boundaries can be chosen
        // against a distribution rather than against the illustration.
        for (lo, hi) in [(0.0, 2.0), (2.0, 10.0), (10.0, 25.0), (25.0, 50.0)] {
            let n = dt_samples.iter().filter(|d| **d >= lo && **d < hi).count();
            let was = dt_band_samples
                .iter()
                .filter(|d| **d >= lo && **d < hi)
                .count();
            println!("  [{lo:>5.1}, {hi:>5.1}) K : {n}  (band-derived: {was})");
        }
        let over = dt_samples.iter().filter(|d| **d >= 50.0).count();
        let over_band = dt_band_samples.iter().filter(|d| **d >= 50.0).count();
        println!("  [ 50.0,   inf) K : {over}  (band-derived: {over_band})");
    }
}

/// Candidate rung boundaries the fine re-bin below measures the local mass
/// around. Includes the shipped table's interior edges and a spread of
/// alternatives, so "is this edge in a spike?" is answered for the shipped
/// choice and for what one would move it to. **10.0 is kept in the list after
/// the ladder stopped using it** — it is the edge this probe condemned, and a
/// reader should be able to re-measure the condemnation, not take it on
/// trust.
const CANDIDATE_EDGES_K: [f64; 14] = [
    1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 8.0, 10.0, 12.0, 15.0, 20.0, 25.0, 35.0, 50.0,
];

/// claim: readout(off-gate, heavy:, prints only, no assertion) — the FINE
/// structure of the same ΔT distribution `how_hot_is_a_cave` bins coarsely.
///
/// Occasioned by review: the coarse table cannot tell an edge that separates
/// two populations from one that sits inside a mode, and
/// `cave_depth_reach_m` has hard clamps (`LAVATUBE_CEILING_M = 200.0`,
/// `CAVE_REACH_CEILING_M = 3000.0`) that put **atoms** in the reach
/// distribution, which ΔT inherits scaled by the gradient. An edge inside an
/// atom's smear is the least stable place an edge can be: a small move
/// migrates a large fraction of the population across it.
///
/// Three readouts per seed:
///
/// 1. the most common exact reach values, which is where the clamps show;
/// 2. a 1 K histogram of ΔT, printed as `lo:count` for non-empty bins only;
/// 3. for each [`CANDIDATE_EDGES_K`], the share of caves within ±0.5 K of it
///    — the direct answer to "does this edge cut a spike?"
///
/// ## Measured, 2026-08-17, seeds 42 / 7 / 1234
///
/// ```text
/// seed 42: caves=874
///   reach atoms: 483.5m x202 (23.1%), 425.8m x87 (10.0%), 407.8m x65 (7.4%),
///                2145.7m x53 (6.1%), 2271.9m x51 (5.8%), 2117.4m x50 (5.7%)
///   deltaT 1K bins: 0:77 5:37 6:94 9:18 10:349 15:12 16:6 17:7 24:7 25:1 27:3
///                   33:3 34:8 35:20 36:10 37:2 38:1 39:5 50:13 51:32 52:10
///                   53:26 54:45 55:35 56:19 57:25 58:7 61:2
///   mass +/-0.5K: 1K:5.0% 2K:0.0% 3K:0.0% 4K:0.0% 5K:0.2% 6K:14.8% 8K:0.0%
///                 10K:19.6% 12K:0.0% 15K:0.6% 20K:0.0% 25K:0.6% 35K:2.1% 50K:0.0%
/// seed 7: caves=1681
///   reach atoms: 2474.3m x193 (11.5%), 215.3m x170 (10.1%), 250.8m x131 (7.8%),
///                2232.4m x125 (7.4%), 2260.4m x121 (7.2%), 2230.4m x101 (6.0%)
///   deltaT 1K bins: 0:72 1:12 5:272 6:327 9:40 10:13 14:17 15:21 16:5 22:1
///                   23:9 24:15 25:7 31:1 32:21 33:22 34:19 35:12 36:5 37:11
///                   38:5 39:11 40:3 41:2 48:15 49:16 50:15 51:91 52:69 53:69
///                   54:97 55:145 56:106 57:70 58:32 59:28 60:3 65:1 67:1
///   mass +/-0.5K: 1K:2.7% 2K:0.0% 3K:0.0% 4K:0.0% 5K:0.3% 6K:35.0% 8K:0.0%
///                 10K:1.8% 12K:0.0% 15K:1.1% 20K:0.0% 25K:1.1% 35K:0.8% 50K:0.4%
/// seed 1234: caves=1266
///   reach atoms: 478.2m x194 (15.3%), 2039.1m x147 (11.6%), 2694.0m x120 (9.5%),
///                2702.6m x120 (9.5%), 461.1m x41 (3.2%), 1871.4m x34 (2.7%)
///   deltaT 1K bins: 0:81 1:10 5:121 6:23 9:18 10:284 14:5 15:17 16:25 17:6
///                   23:4 24:7 25:7 26:26 27:3 32:5 33:9 34:9 35:9 36:9 37:20
///                   38:3 40:1 41:10 42:11 43:2 49:5 50:37 51:53 52:45 53:56
///                   54:12 55:24 56:64 57:59 58:73 59:77 60:30 61:1 65:2 66:2 68:1
///   mass +/-0.5K: 1K:3.9% 2K:0.2% 3K:0.0% 4K:0.0% 5K:1.4% 6K:9.3% 8K:0.0%
///                 10K:6.5% 12K:0.0% 15K:1.6% 20K:0.0% 25K:0.2% 35K:0.9% 50K:1.5%
/// ```
///
/// **What it showed, and what moved because of it.** The reach atoms are the
/// mechanism: seed 42's single fattest reach value covers 23.1% of its caves,
/// and the gradient spans only 1.27x, so ΔT is a row of spikes rather than a
/// spread. Nine of the ~70 1 K bins hold everything.
///
/// The 1 K bin ranges EMPTY on all three seeds — the valleys an edge should
/// sit in — are `[2,5)`, `[7,9)`, `[11,14)`, `[18,22)`, `[28,31)`, `[44,48)`.
///
/// `DEEPS_TOP_K` was 10.0, which is none of those: bin `[10,11)` alone holds
/// 39.9% of seed 42, and 19.6% of that seed lies within ±0.5 K of the edge. It
/// moved to **8.0**, inside `[7,9)`, measuring 0.0% on every seed. The other
/// three interior edges were left alone — 2.0 is already inside `[2,5)`, 25.0
/// is sparse rather than modal (0.2–1.1%), and 50.0 is authored and frozen
/// (and, as the 48/49 bins show, the least stable of the four — an accepted
/// cost of it not being a measured quantity). See
/// `domains/terrain/src/delve.rs`'s constants for each decision in full.
#[test]
#[ignore = "heavy: live-worldgen battery; deferred from the commit gate to the heavy set (decision 0132)"]
fn how_lumpy_is_the_delta_t_distribution() {
    let wc = WorldComponents::assemble().expect("canonical registries are well-formed");
    for seed_value in SEEDS {
        let seed = hornvale_kernel::Seed(seed_value);
        let artifacts = build_world_to_with_artifacts(
            seed,
            &SkyPins::default(),
            SkyChoice::Generated,
            &TerrainPins::default(),
            &SettlementPins::default(),
            &wc,
            BuildDepth::Terrain,
        )
        .expect("probe seed builds");
        let terrain = artifacts
            .terrain
            .expect("terrain is Some at BuildDepth::Terrain");
        let geo = terrain.geosphere();

        let mut dt: Vec<f64> = Vec::new();
        let mut reaches: Vec<f64> = Vec::new();
        for cell in geo.cells() {
            if terrain.is_ocean(cell) {
                continue;
            }
            let Some(cave) = terrain.cave_at(cell) else {
                continue;
            };
            let gradient = terrain.geothermal_gradient_at(cell).get();
            reaches.push(cave.depth_reach_m);
            dt.push(gradient * (cave.depth_reach_m / 1000.0));
        }
        assert!(!dt.is_empty(), "seed {seed_value} has no caves");
        let n = dt.len();
        dt.sort_by(f64::total_cmp);

        // 1. Reach atoms. Bucket to 0.1 m so float noise does not split an
        // exact clamp into neighbours, then report the fattest buckets.
        let mut reach_counts: std::collections::BTreeMap<i64, usize> =
            std::collections::BTreeMap::new();
        for r in &reaches {
            *reach_counts.entry((r * 10.0).round() as i64).or_default() += 1;
        }
        let mut atoms: Vec<(i64, usize)> = reach_counts.into_iter().collect();
        atoms.sort_by(|a, b| b.1.cmp(&a.1).then(a.0.cmp(&b.0)));
        let atom_line: Vec<String> = atoms
            .iter()
            .take(6)
            .map(|&(tenths, c)| {
                format!(
                    "{:.1}m x{c} ({:.1}%)",
                    tenths as f64 / 10.0,
                    100.0 * c as f64 / n as f64
                )
            })
            .collect();
        println!("seed {seed_value}: caves={n}");
        println!("  reach atoms: {}", atom_line.join(", "));

        // 2. The 1 K histogram, non-empty bins only.
        let mut hist: std::collections::BTreeMap<i64, usize> = std::collections::BTreeMap::new();
        for d in &dt {
            *hist.entry(d.floor() as i64).or_default() += 1;
        }
        let bins: Vec<String> = hist.iter().map(|(k, v)| format!("{k}:{v}")).collect();
        println!("  deltaT 1K bins: {}", bins.join(" "));

        // 3. Mass within +/-0.5 K of each candidate edge.
        let edge_line: Vec<String> = CANDIDATE_EDGES_K
            .iter()
            .map(|&e| {
                let m = dt.iter().filter(|d| (**d - e).abs() <= 0.5).count();
                format!("{e:.0}K:{:.1}%", 100.0 * m as f64 / n as f64)
            })
            .collect();
        println!("  mass within +/-0.5K of edge: {}", edge_line.join(" "));
    }
}

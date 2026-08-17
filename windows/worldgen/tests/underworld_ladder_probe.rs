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
//! ## Measured, 2026-08-16, seeds 42 / 7 / 1234
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
            band_hist[match cave.deepest_band {
                BandKind::Regolith => 0,
                BandKind::Cover => 1,
                BandKind::Basement => 2,
                BandKind::Roots => 3,
                BandKind::Underneath => 4,
            }] += 1;
            // ΔT at the cave's deepest reach: the column's own top-depth for
            // that band, converted to km, times this cell's gradient.
            let column = terrain.column_at(cell);
            let depth_m = column.bands[match cave.deepest_band {
                BandKind::Regolith => 0,
                BandKind::Cover => 1,
                BandKind::Basement => 2,
                BandKind::Roots => 3,
                BandKind::Underneath => 4,
            }]
            .top_depth_m;
            dt_samples.push(gradient * (depth_m / 1000.0));
        }

        dt_samples.sort_by(f64::total_cmp);
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
            println!("  [{lo:>5.1}, {hi:>5.1}) K : {n}");
        }
        let over = dt_samples.iter().filter(|d| **d >= 50.0).count();
        println!("  [ 50.0,   inf) K : {over}");
    }
}

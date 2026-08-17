//! THE UNDERWORLD, Task 3: how deep does the water table sit, and is it lumpy?
//!
//! Two jobs, deliberately in one file so the same code path produces both.
//!
//! 1. **H3's statistic** (spec §5): the share of cave-bearing columns that are
//!    *wholly phreatic*. Preregistered bounds — neither under 5% nor over 95%.
//! 2. **The distribution's shape**, which H3 alone cannot see. A water table
//!    that passes H3 while taking three distinct values is a trap for Task 5,
//!    which derives chamber moisture from it. This campaign has already been
//!    bitten twice by exactly that: `cave_depth_reach_m`'s clamps put 23.1% of
//!    seed 42's caves on one reach value, and the delve ladder's `Deeps`
//!    boundary had to be moved off the mode it had landed on.
//!
//! **Wholly phreatic is `water_table_depth_m == 0.0`** — the table at or above
//! the ground, so every depth below the surface is flooded and the column has
//! no walkable window at all. That is the only reading of "wholly" that is not
//! vacuous: a cave column spans the surface down to its reach, so any strictly
//! positive table leaves a vadose slice at the top. The floor is a physical
//! state (a spring, a marsh, a lake — see `hornvale_terrain::water_table`), so
//! this counts drowned columns rather than an artifact of a rail.
//!
//! ## Measured, 2026-08-17, seeds 42 / 7 / 1234
//!
//! **H3 holds on every seed** — 29.9% / 45.5% / 42.5% wholly phreatic, well
//! inside `[5%, 95%]`.
//!
//! **The distribution is not three atoms.** 491–586 distinct values at 0.1 m
//! resolution over 874–1681 columns. The only large atom is `0.0` itself, which
//! *is* H3's statistic rather than an artifact; the fattest **non-zero** value
//! holds 3.7% of a seed at worst. For contrast, one reach value holds 23.1% of
//! seed 42 in the cave-depth budget this campaign had to work around.
//!
//! **The inputs carry one atom the derivation inherits:** 16.4–18.6% of
//! cave-bearing columns sit at `height_asl_m == 0.0` exactly, because the
//! carve's marine trim pins cells to sea level. That is why `RELIEF_SOFT_M`
//! is not the negligible smoothing constant it looks like — see its doc.
//!
//! **The reading that is not H3.** The walkable share of a cave column has a
//! median of 0.002–0.049, and no `Underdeep` or `Sunless` rung is dry in any
//! of the three worlds. Reported, not asserted: nothing preregistered a
//! per-rung criterion, and inventing one after seeing this would be exactly
//! the move decision 0016 exists to prevent.
//!
//! ```text
//! seed 42: cave columns=874
//!   H3 wholly phreatic: 261/874 = 29.9%
//!   sumped (cave bottom below the table)=758 (86.7%)  wholly vadose=116 (13.3%)
//!   table m   p10=0.0 p25=0.0 p50=23.5 p75=248.4 p90=359.6 p99=440.6 max=501.5
//!   table m (undrowned only, n=613) p10=7.3 p50=115.0 p90=384.6
//!   distinct 0.1 m values=491
//!   atoms: 0.0m x261 (29.9%), 23.5m x32 (3.7%), 2.7m x9 (1.0%), 12.5m x9 (1.0%),
//!          6.7m x6 (0.7%), 3.0m x5 (0.6%)
//!   input atom: height_asl_m == 0.0 on 163 columns (18.6%)
//!   inputs: height p10=0 p50=597 p90=2123 | drainage p10=1 p50=2 p90=11
//!         | porosity min=0.055 p10=0.056 p50=0.781 p90=0.819 max=0.819
//!   reach m   p10=200 p50=483 p90=2272
//!   walkable share of the cave column  p10=0.000 p25=0.000 p50=0.049 p75=0.742 p90=1.000
//!   rung Undercroft: reached by 874 columns, dry at 874 (100.0%)
//!   rung Shallows: reached by 797 columns, dry at 315 (39.5%)
//!   rung Deeps: reached by 666 columns, dry at 90 (13.5%)
//!   rung Underdeep: reached by 267 columns, dry at 0 (0.0%)
//!   rung Sunless: reached by 214 columns, dry at 0 (0.0%)
//! seed 7: cave columns=1681
//!   H3 wholly phreatic: 765/1681 = 45.5%
//!   sumped (cave bottom below the table)=1492 (88.8%)  wholly vadose=189 (11.2%)
//!   table m   p10=0.0 p25=0.0 p50=4.6 p75=43.2 p90=224.7 p99=333.4 max=422.0
//!   table m (undrowned only, n=916) p10=4.9 p50=28.8 p90=271.9
//!   distinct 0.1 m values=586
//!   atoms: 0.0m x765 (45.5%), 19.3m x22 (1.3%), 15.6m x21 (1.2%), 8.3m x20 (1.2%),
//!          17.0m x16 (1.0%), 4.6m x14 (0.8%)
//!   input atom: height_asl_m == 0.0 on 277 columns (16.5%)
//!   inputs: height p10=0 p50=708 p90=2727 | drainage p10=1 p50=2 p90=15
//!         | porosity min=0.052 p10=0.054 p50=0.374 p90=0.791 max=0.805
//!   reach m   p10=215 p50=1409 p90=2474
//!   walkable share of the cave column  p10=0.000 p25=0.000 p50=0.002 p75=0.123 p90=1.000
//!   rung Undercroft: reached by 1681 columns, dry at 1681 (100.0%)
//!   rung Shallows: reached by 1597 columns, dry at 334 (20.9%)
//!   rung Deeps: reached by 998 columns, dry at 0 (0.0%)
//!   rung Underdeep: reached by 877 columns, dry at 0 (0.0%)
//!   rung Sunless: reached by 727 columns, dry at 0 (0.0%)
//! seed 1234: cave columns=1266
//!   H3 wholly phreatic: 538/1266 = 42.5%
//!   sumped (cave bottom below the table)=1186 (93.7%)  wholly vadose=80 (6.3%)
//!   table m   p10=0.0 p25=0.0 p50=5.6 p75=71.0 p90=264.0 p99=418.8 max=470.3
//!   table m (undrowned only, n=728) p10=4.7 p50=46.4 p90=327.5
//!   distinct 0.1 m values=523
//!   atoms: 0.0m x538 (42.5%), 23.3m x23 (1.8%), 7.9m x11 (0.9%), 2.5m x9 (0.7%),
//!          6.9m x9 (0.7%), 7.4m x8 (0.6%)
//!   input atom: height_asl_m == 0.0 on 207 columns (16.4%)
//!   inputs: height p10=0 p50=722 p90=2298 | drainage p10=1 p50=2 p90=13
//!         | porosity min=0.051 p10=0.051 p50=0.379 p90=0.817 max=0.818
//!   reach m   p10=202 p50=1201 p90=2694
//!   walkable share of the cave column  p10=0.000 p25=0.000 p50=0.003 p75=0.172 p90=0.808
//!   rung Undercroft: reached by 1266 columns, dry at 1266 (100.0%)
//!   rung Shallows: reached by 1175 columns, dry at 271 (23.1%)
//!   rung Deeps: reached by 1031 columns, dry at 48 (4.7%)
//!   rung Underdeep: reached by 665 columns, dry at 0 (0.0%)
//!   rung Sunless: reached by 536 columns, dry at 0 (0.0%)
//! ```
#![allow(clippy::disallowed_methods)]

use hornvale_astronomy::SkyPins;
use hornvale_terrain::{
    DelveRung, TerrainPins, delta_t_range_of, is_phreatic, rungs, water_table_depth_m,
};
use hornvale_worldgen::{
    BuildDepth, SettlementPins, SkyChoice, WorldComponents, build_world_to_with_artifacts,
};

/// Seeds this campaign preregisters on (spec §5). Same three the delve
/// ladder's probe uses, so the two readouts describe the same worlds.
const SEEDS: [u64; 3] = [42, 7, 1234];

/// H3's lower bound: fewer than this share of cave-bearing columns being
/// wholly phreatic falsifies it (spec §5, frozen before any code).
const H3_MIN_DROWNED: f64 = 0.05;

/// H3's upper bound, same provenance.
const H3_MAX_DROWNED: f64 = 0.95;

/// Percentile of an ascending slice.
fn pct(sorted: &[f64], q: f64) -> f64 {
    if sorted.is_empty() {
        return f64::NAN;
    }
    sorted[((sorted.len() - 1) as f64 * q).round() as usize]
}

/// The fattest exact values in a sample, bucketed to 0.1 of a unit so float
/// noise cannot split one spike into neighbours. The same instrument
/// `how_lumpy_is_the_delta_t_distribution` uses on cave reaches, pointed at
/// the water table instead.
fn atoms(values: &[f64], take: usize) -> Vec<String> {
    let n = values.len();
    let mut counts: std::collections::BTreeMap<i64, usize> = std::collections::BTreeMap::new();
    for v in values {
        *counts.entry((v * 10.0).round() as i64).or_default() += 1;
    }
    let mut ranked: Vec<(i64, usize)> = counts.into_iter().collect();
    ranked.sort_by(|a, b| b.1.cmp(&a.1).then(a.0.cmp(&b.0)));
    ranked
        .iter()
        .take(take)
        .map(|&(tenths, c)| {
            format!(
                "{:.1}m x{c} ({:.1}%)",
                tenths as f64 / 10.0,
                100.0 * c as f64 / n as f64
            )
        })
        .collect()
}

/// claim: readout(off-gate, heavy:, prints the distribution, asserts only the
/// preregistered H3 bounds) — the water table over every cave-bearing land
/// cell of the three preregistered seeds.
#[test]
#[ignore = "heavy: live-worldgen battery; deferred from the commit gate to the heavy set (decision 0132)"]
fn the_water_table_is_not_degenerate() {
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
        let sea = terrain.sea_level().get();

        let mut tables: Vec<f64> = Vec::new();
        let mut heights: Vec<f64> = Vec::new();
        let mut drainages: Vec<f64> = Vec::new();
        let mut porosities: Vec<f64> = Vec::new();
        let mut reaches: Vec<f64> = Vec::new();
        // The share of each cave column that is walkable: the vadose slice
        // divided by the cave's own reach. The "habitable window per column
        // that is not the same everywhere" §4.2 exists to produce, measured
        // as a distribution rather than asserted as a design intent.
        let mut vadose_fraction: Vec<f64> = Vec::new();
        // Per habitation rung: (columns whose cave reaches that rung's top,
        // of those, columns where the rung's top is above the table). The
        // consequence for the thing that consumes this — a rung the cave
        // reaches but the table drowns is a sump, not a place to live.
        let mut rung_reached = [0usize; 5];
        let mut rung_vadose = [0usize; 5];
        // Columns whose cave bottoms out below the table — the sump
        // population §4.2 exists to produce.
        let mut sumped = 0usize;
        // Columns whose whole cave is above the table: dry to the bottom.
        let mut wholly_vadose = 0usize;

        for cell in geo.cells() {
            if terrain.is_ocean(cell) {
                continue;
            }
            let Some(cave) = terrain.cave_at(cell) else {
                continue;
            };
            let height_asl_m = terrain.elevation_at(cell).get() - sea;
            let drainage = terrain.drainage_at(cell);
            let porosity = terrain.material_at(cell).porosity;
            let table = water_table_depth_m(drainage, porosity, height_asl_m);

            if is_phreatic(cave.depth_reach_m, table) {
                sumped += 1;
            } else {
                wholly_vadose += 1;
            }
            let gradient = terrain.geothermal_gradient_at(cell).get();
            for (index, rung) in rungs()
                .iter()
                .filter(|r| **r != DelveRung::Surface)
                .enumerate()
            {
                // The rung's own top, the depth `chamber.rs` places a chamber
                // at: its ΔT floor divided by this cell's gradient.
                let top_m = 1000.0 * delta_t_range_of(*rung).0 / gradient;
                if top_m > cave.depth_reach_m {
                    continue;
                }
                rung_reached[index] += 1;
                if !is_phreatic(top_m, table) {
                    rung_vadose[index] += 1;
                }
            }

            if cave.depth_reach_m > 0.0 {
                vadose_fraction.push((table.min(cave.depth_reach_m)) / cave.depth_reach_m);
            }
            tables.push(table);
            heights.push(height_asl_m);
            drainages.push(drainage);
            porosities.push(porosity);
            reaches.push(cave.depth_reach_m);
        }

        let n = tables.len();
        assert!(n > 0, "seed {seed_value} has no caves");
        let drowned = tables.iter().filter(|t| **t == 0.0).count();
        let drowned_share = drowned as f64 / n as f64;
        let mut dry: Vec<f64> = tables.iter().copied().filter(|t| *t > 0.0).collect();

        let atom_line = atoms(&tables, 6);
        let distinct = {
            let mut seen: std::collections::BTreeSet<i64> = std::collections::BTreeSet::new();
            for t in &tables {
                seen.insert((t * 10.0).round() as i64);
            }
            seen.len()
        };

        tables.sort_by(f64::total_cmp);
        dry.sort_by(f64::total_cmp);
        heights.sort_by(f64::total_cmp);
        drainages.sort_by(f64::total_cmp);
        porosities.sort_by(f64::total_cmp);
        reaches.sort_by(f64::total_cmp);
        vadose_fraction.sort_by(f64::total_cmp);

        println!("seed {seed_value}: cave columns={n}");
        println!(
            "  H3 wholly phreatic: {drowned}/{n} = {:.1}%",
            100.0 * drowned_share
        );
        println!(
            "  sumped (cave bottom below the table)={sumped} ({:.1}%)  wholly vadose={wholly_vadose} ({:.1}%)",
            100.0 * sumped as f64 / n as f64,
            100.0 * wholly_vadose as f64 / n as f64
        );
        println!(
            "  table m   p10={:.1} p25={:.1} p50={:.1} p75={:.1} p90={:.1} p99={:.1} max={:.1}",
            pct(&tables, 0.10),
            pct(&tables, 0.25),
            pct(&tables, 0.50),
            pct(&tables, 0.75),
            pct(&tables, 0.90),
            pct(&tables, 0.99),
            pct(&tables, 1.00)
        );
        println!(
            "  table m (undrowned only, n={}) p10={:.1} p50={:.1} p90={:.1}",
            dry.len(),
            pct(&dry, 0.10),
            pct(&dry, 0.50),
            pct(&dry, 0.90)
        );
        println!("  distinct 0.1 m values={distinct}");
        println!("  atoms: {}", atom_line.join(", "));
        // The one atom the INPUTS carry: the carve's marine trim pins cells to
        // exactly sea level, so a share of land sits at height_asl_m == 0.0.
        // That population's table depth is proportional to RELIEF_SOFT_M,
        // which is why that constant is not the free parameter it looks like.
        let at_sea_level = heights.iter().filter(|h| **h == 0.0).count();
        println!(
            "  input atom: height_asl_m == 0.0 on {at_sea_level} columns ({:.1}%)",
            100.0 * at_sea_level as f64 / n as f64
        );
        println!(
            "  inputs: height p10={:.0} p50={:.0} p90={:.0} | drainage p10={:.0} p50={:.0} p90={:.0} | porosity min={:.3} p10={:.3} p50={:.3} p90={:.3} max={:.3}",
            pct(&heights, 0.10),
            pct(&heights, 0.50),
            pct(&heights, 0.90),
            pct(&drainages, 0.10),
            pct(&drainages, 0.50),
            pct(&drainages, 0.90),
            pct(&porosities, 0.00),
            pct(&porosities, 0.10),
            pct(&porosities, 0.50),
            pct(&porosities, 0.90),
            pct(&porosities, 1.00)
        );
        println!(
            "  reach m   p10={:.0} p50={:.0} p90={:.0}",
            pct(&reaches, 0.10),
            pct(&reaches, 0.50),
            pct(&reaches, 0.90)
        );
        println!(
            "  walkable share of the cave column  p10={:.3} p25={:.3} p50={:.3} p75={:.3} p90={:.3}",
            pct(&vadose_fraction, 0.10),
            pct(&vadose_fraction, 0.25),
            pct(&vadose_fraction, 0.50),
            pct(&vadose_fraction, 0.75),
            pct(&vadose_fraction, 0.90)
        );

        for (index, rung) in rungs()
            .iter()
            .filter(|r| **r != DelveRung::Surface)
            .enumerate()
        {
            let reached = rung_reached[index];
            let vadose = rung_vadose[index];
            let share = if reached == 0 {
                f64::NAN
            } else {
                100.0 * vadose as f64 / reached as f64
            };
            println!(
                "  rung {rung:?}: reached by {reached} columns, dry at {vadose} ({share:.1}%)"
            );
        }

        assert!(
            (H3_MIN_DROWNED..=H3_MAX_DROWNED).contains(&drowned_share),
            "H3 falsified on seed {seed_value}: {:.1}% of cave columns wholly phreatic, \
             outside [{:.0}%, {:.0}%]",
            100.0 * drowned_share,
            100.0 * H3_MIN_DROWNED,
            100.0 * H3_MAX_DROWNED
        );
    }
}

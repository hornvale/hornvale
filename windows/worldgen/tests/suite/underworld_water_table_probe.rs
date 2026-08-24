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
//! ## Measured, 2026-08-17, seeds 42 / 7 / 1234 — AFTER spec §4.2.1
//!
//! Taken after the relief recalibration (§4.2.1 clause 1) and the drainage rule
//! (clause 2) landed. The pre-correction reading is kept below as the
//! before-arm; deleting it would destroy the evidence that the correction
//! did not buy H3 at the cost of anything else.
//!
//! **H3 holds on every seed** — 31.9% / 43.6% / 41.5% wholly phreatic, well
//! inside `[5%, 95%]`, and essentially unmoved by the recalibration
//! (29.9 / 45.5 / 42.5 before). That is the result the correction had to
//! survive and did.
//!
//! **The distribution is not three atoms, and got finer.** 527–757 distinct
//! values at 0.1 m over 874–1681 columns (was 491–586). The only large atom is
//! `0.0`, which *is* H3's statistic rather than an artifact; the fattest
//! **non-zero** value holds 3.7% of a seed at worst, unchanged. For contrast,
//! one reach value holds 23.1% of seed 42 in the cave-depth budget.
//!
//! **The inputs carry one atom the derivation inherits:** 16.4–18.6% of
//! cave-bearing columns sit at `height_asl_m == 0.0` exactly, because the
//! carve's marine trim pins cells to sea level.
//!
//! **Rung dryness, looked at only after the two above** (spec §4.2.1 requires
//! that order, and the calibration was committed before this probe was run so
//! the order is a fact about the history):
//!
//! ```text
//! rung         before §4.2.1        after §4.2.1
//! Undercroft   100 /100 /100 %      100  /100  /100  %   <- an IDENTITY, see below
//! Shallows      39.5/20.9/23.1      41.8 / 24.9/ 26.1
//! Deeps         13.5/ 0.0/ 4.7      29.9 /  1.7/ 14.4
//! Underdeep      0.0/ 0.0/ 0.0       0.0 /  0.0/  0.0
//! Nadir          0.0/ 0.0/ 0.0       0.0 /  0.0/  0.0
//! ```
//!
//! **THE `Undercroft` ROW IS AN IDENTITY, NOT A MEASUREMENT, AND IT CANNOT
//! COME BACK ANY OTHER VALUE.** A rung is judged at its own top;
//! `Undercroft`'s ΔT range begins at 0 K, so its top is `0.0` m in every
//! column under every gradient; `earth_table_depth_m` ends `.max(0.0)` and so
//! the table is never negative; and [`is_phreatic`] is a **strict**
//! `depth > table`, which makes even a drowned column's `0.0` read vadose. So
//! `is_phreatic(0.0, table)` is `false` for every table this crate can
//! produce, in every possible world — the 100% is arithmetic, and it is
//! invariant to the calibration whose before/after these two columns exist to
//! show. Reading it as evidence that the shallowest rung *happens* to be dry
//! is the mistake: it is the first row of both columns because it is the same
//! tautology on both sides.
//!
//! This row was cited as evidence for a shipped design rule for the length of
//! this campaign (`delve_seating::seat_at`, now corrected), and the reason it
//! survived is that it is printed beside four rows that ARE measurements and
//! looks like a fifth. `delve_seating::RungSeat::works` carries the
//! consequence for consumers: a works count over a seating is a count over
//! ranks 1–4 only.
//!
//! `Deeps` opened; **`Underdeep` and `Nadir` did not, and no scale constant
//! can open them** — see `UNDERWORLD_DRYNESS_GAIN`'s doc for the 1×–16× sweep
//! and the reason. The reason is in this readout: the columns reaching
//! `Nadir` have a median porosity of **0.056** against population medians of
//! 0.781 / 0.374 / 0.379, because reach rises with `induration` and porosity
//! falls with it. Deep caves are in rock that cannot shed water.
//!
//! **What does open them is the drainage rule** (clause 2), exercised here
//! through the shipped `is_sump`: a `Made` chamber is dry regardless of the
//! table, recovering **100% of every reached `Underdeep` and `Nadir`
//! column**. `Made` now HAS a writer — Task 8's
//! `delve_seating::made_chambers` — but no shipped path constructs the
//! override map it writes into, so no world a player can reach carries the
//! variant. `dry-if-made` therefore still measures the size of the population
//! the rule *can* reach, not chambers in any world. (This block said "nothing
//! writes `Made` yet" after the writer landed.)
//!
//! Per-rung figures are REPORTED, never asserted: nothing preregistered a
//! per-rung criterion, and inventing one after seeing this is the move
//! decision 0016 exists to prevent.
//!
//! ```text
//! seed 42: cave columns=874
//!   H3 wholly phreatic: 279/874 = 31.9%
//!   sumped (cave bottom below the table)=599 (68.5%)  wholly vadose=275 (31.5%)
//!   table m   p10=0.0 p25=0.0 p50=49.3 p75=408.7 p90=756.4 p99=1022.4 max=1361.9
//!   table m (undrowned only, n=595) p10=14.1 p50=284.0 p90=843.1
//!   distinct 0.1 m values=527
//!   atoms: 0.0m x279 (31.9%), 16.3m x32 (3.7%), 9.6m x5 (0.6%), 11.6m x5 (0.6%), 11.9m x5 (0.6%), 10.0m x4 (0.5%)
//!   input atom: height_asl_m == 0.0 on 163 columns (18.6%)
//!   inputs: height p10=0 p50=597 p90=2123 | drainage p10=1 p50=2 p90=11 | porosity min=0.055 p10=0.056 p50=0.781 p90=0.819 max=0.819
//!   reach m   p10=200 p50=483 p90=2272
//!   porosity of Nadir-reaching columns (n=214) p10=0.055 p50=0.056 p90=0.381  vs ALL p50=0.781
//!   walkable share of the cave column  p10=0.000 p25=0.000 p50=0.041 p75=1.000 p90=1.000
//!   rung Undercroft: reached by 874 columns, dry at 874 (100.0%), dry-if-made 874 (+0 the drainage rule can recover)
//!   rung Shallows: reached by 797 columns, dry at 333 (41.8%), dry-if-made 797 (+464 the drainage rule can recover)
//!   rung Deeps: reached by 666 columns, dry at 199 (29.9%), dry-if-made 666 (+467 the drainage rule can recover)
//!   rung Underdeep: reached by 267 columns, dry at 0 (0.0%), dry-if-made 267 (+267 the drainage rule can recover)
//!   rung Nadir: reached by 214 columns, dry at 0 (0.0%), dry-if-made 214 (+214 the drainage rule can recover)
//! seed 7: cave columns=1681
//!   H3 wholly phreatic: 733/1681 = 43.6%
//!   sumped (cave bottom below the table)=1390 (82.7%)  wholly vadose=291 (17.3%)
//!   table m   p10=0.0 p25=0.0 p50=8.8 p75=98.6 p90=379.2 p99=651.9 max=1070.5
//!   table m (undrowned only, n=948) p10=8.7 p50=67.7 p90=489.7
//!   distinct 0.1 m values=757
//!   atoms: 0.0m x733 (43.6%), 7.1m x22 (1.3%), 11.4m x22 (1.3%), 8.7m x16 (1.0%), 5.2m x5 (0.3%), 11.0m x5 (0.3%)
//!   input atom: height_asl_m == 0.0 on 277 columns (16.5%)
//!   inputs: height p10=0 p50=708 p90=2727 | drainage p10=1 p50=2 p90=15 | porosity min=0.052 p10=0.054 p50=0.374 p90=0.791 max=0.805
//!   reach m   p10=215 p50=1409 p90=2474
//!   porosity of Nadir-reaching columns (n=727) p10=0.054 p50=0.056 p90=0.056  vs ALL p50=0.374
//!   walkable share of the cave column  p10=0.000 p25=0.000 p50=0.009 p75=0.172 p90=1.000
//!   rung Undercroft: reached by 1681 columns, dry at 1681 (100.0%), dry-if-made 1681 (+0 the drainage rule can recover)
//!   rung Shallows: reached by 1597 columns, dry at 398 (24.9%), dry-if-made 1597 (+1199 the drainage rule can recover)
//!   rung Deeps: reached by 998 columns, dry at 17 (1.7%), dry-if-made 998 (+981 the drainage rule can recover)
//!   rung Underdeep: reached by 877 columns, dry at 0 (0.0%), dry-if-made 877 (+877 the drainage rule can recover)
//!   rung Nadir: reached by 727 columns, dry at 0 (0.0%), dry-if-made 727 (+727 the drainage rule can recover)
//! seed 1234: cave columns=1266
//!   H3 wholly phreatic: 525/1266 = 41.5%
//!   sumped (cave bottom below the table)=1057 (83.5%)  wholly vadose=209 (16.5%)
//!   table m   p10=0.0 p25=0.0 p50=16.0 p75=144.0 p90=484.9 p99=870.7 max=1078.5
//!   table m (undrowned only, n=741) p10=13.3 p50=82.9 p90=609.5
//!   distinct 0.1 m values=618
//!   atoms: 0.0m x525 (41.5%), 16.0m x23 (1.8%), 15.1m x8 (0.6%), 5.8m x5 (0.4%), 5.2m x4 (0.3%), 13.5m x4 (0.3%)
//!   input atom: height_asl_m == 0.0 on 207 columns (16.4%)
//!   inputs: height p10=0 p50=722 p90=2298 | drainage p10=1 p50=2 p90=13 | porosity min=0.051 p10=0.051 p50=0.379 p90=0.817 max=0.818
//!   reach m   p10=202 p50=1201 p90=2694
//!   porosity of Nadir-reaching columns (n=536) p10=0.051 p50=0.056 p90=0.377  vs ALL p50=0.379
//!   walkable share of the cave column  p10=0.000 p25=0.000 p50=0.011 p75=0.338 p90=1.000
//!   rung Undercroft: reached by 1266 columns, dry at 1266 (100.0%), dry-if-made 1266 (+0 the drainage rule can recover)
//!   rung Shallows: reached by 1175 columns, dry at 307 (26.1%), dry-if-made 1175 (+868 the drainage rule can recover)
//!   rung Deeps: reached by 1031 columns, dry at 148 (14.4%), dry-if-made 1031 (+883 the drainage rule can recover)
//!   rung Underdeep: reached by 665 columns, dry at 0 (0.0%), dry-if-made 665 (+665 the drainage rule can recover)
//!   rung Nadir: reached by 536 columns, dry at 0 (0.0%), dry-if-made 536 (+536 the drainage rule can recover)
//! ```
//!
//! ## The before-arm: measured 2026-08-17, BEFORE spec §4.2.1
//!
//! Retained deliberately. `RELIEF_HALF_M` was 800 m and there was one metre
//! scale at 200 m.
//!
//! ```text
//! seed 42:   H3 261/874 = 29.9%   table p50=23.5 p90=359.6 max=501.5   distinct=491
//!            walkable share p50=0.049   rungs dry 100/39.5/13.5/0.0/0.0 %
//! seed 7:    H3 765/1681 = 45.5%  table p50= 4.6 p90=224.7 max=422.0   distinct=586
//!            walkable share p50=0.002   rungs dry 100/20.9/ 0.0/0.0/0.0 %
//! seed 1234: H3 538/1266 = 42.5%  table p50= 5.6 p90=264.0 max=470.3   distinct=523
//!            walkable share p50=0.003   rungs dry 100/23.1/ 4.7/0.0/0.0 %
//! ```
//!
//! Test fixture (decision 0092): calls the composition-root entry points
//! directly, the sanctioned posture for this crate's live-worldgen batteries.
#![allow(clippy::disallowed_methods)]

use hornvale_astronomy::SkyPins;
use hornvale_kernel::Band;
use hornvale_terrain::{
    ARABIKA_POROSITY, TerrainPins, delta_t_range_of, earth_table_depth_m, is_phreatic, rungs,
    water_table_depth_m,
};
use hornvale_worldgen::chamber::{ChamberOrigin, is_sump};
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
        // The same rungs judged through the SHIPPED drainage rule with a
        // `Made` origin (spec §4.2.1, clause 2), so the probe exercises
        // `is_sump` rather than restating it. `made_chambers` writes `Made`
        // now, but nothing in the shipped path constructs the override map it
        // writes into, so this still measures the SIZE OF THE POPULATION the
        // rule can reach — not a count of chambers in any world.
        let mut rung_dry_if_made = [0usize; 5];
        // WHY the gain saturates: `cave_depth_reach_m` rises with `induration`
        // while `assemble_material` builds porosity with a `(1 - induration)`
        // term, so a deep-reaching cave sits in rock that sheds water badly.
        // Measured rather than inferred from the two source lines.
        let mut porosity_deep: Vec<f64> = Vec::new();
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
            for (index, rung) in rungs().iter().filter(|r| **r != Band::Surface).enumerate() {
                // The rung's own top, the depth `chamber.rs` places a chamber
                // at: its ΔT floor divided by this cell's gradient.
                let top_m = 1000.0 * delta_t_range_of(*rung).0 / gradient;
                if top_m > cave.depth_reach_m {
                    continue;
                }
                rung_reached[index] += 1;
                if !is_sump(ChamberOrigin::Made, top_m, table) {
                    rung_dry_if_made[index] += 1;
                }
                if !is_sump(ChamberOrigin::Found, top_m, table) {
                    rung_vadose[index] += 1;
                }
                // Index 4 is `Nadir`, the deepest rung. A column that reaches
                // it is a deep-reaching cave; its porosity is the quantity the
                // anti-correlation claim is about.
                if index == 4 {
                    porosity_deep.push(porosity);
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
        porosity_deep.sort_by(f64::total_cmp);

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
            "  porosity of Nadir-reaching columns (n={}) p10={:.3} p50={:.3} p90={:.3}  vs ALL p50={:.3}",
            porosity_deep.len(),
            pct(&porosity_deep, 0.10),
            pct(&porosity_deep, 0.50),
            pct(&porosity_deep, 0.90),
            pct(&porosities, 0.50)
        );
        println!(
            "  walkable share of the cave column  p10={:.3} p25={:.3} p50={:.3} p75={:.3} p90={:.3}",
            pct(&vadose_fraction, 0.10),
            pct(&vadose_fraction, 0.25),
            pct(&vadose_fraction, 0.50),
            pct(&vadose_fraction, 0.75),
            pct(&vadose_fraction, 0.90)
        );

        for (index, rung) in rungs().iter().filter(|r| **r != Band::Surface).enumerate() {
            let reached = rung_reached[index];
            let vadose = rung_vadose[index];
            let share = if reached == 0 {
                f64::NAN
            } else {
                100.0 * vadose as f64 / reached as f64
            };
            let made = rung_dry_if_made[index];
            println!(
                "  rung {rung:?}: reached by {reached} columns, dry at {vadose} ({share:.1}%), \
                 dry-if-made {made} (+{} the drainage rule can recover)",
                made - vadose
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

/// The gains [`hornvale_terrain`]'s `UNDERWORLD_DRYNESS_GAIN` doc tabulates.
/// `1.0` is the shipped value and doubles as this arm's positive control: its
/// row must reproduce `the_water_table_is_not_degenerate`'s numbers exactly,
/// because at gain 1 the swept expression IS the shipped function.
const SWEPT_GAINS: [f64; 6] = [1.0, 2.0, 3.0, 4.0, 8.0, 16.0];

/// How far the calibration coordinate may drift from the model's measured
/// porosity ceiling before the vadose datum needs re-solving. Wide enough that
/// ordinary noise in the lithology pipeline does not cry wolf, tight enough
/// that a real change to the induration/porosity coupling — which is what would
/// silently decalibrate `DRAWDOWN_SCALE_M` — cannot pass.
const POROSITY_CEILING_TOLERANCE: f64 = 0.03;

/// claim: readout(off-gate, heavy:, prints the sweep, asserts only that the
/// gain cannot move H3) — regenerates the six-row table in
/// `hornvale_terrain::water_table`'s `UNDERWORLD_DRYNESS_GAIN` doc, which is the
/// entire evidence for shipping the gain at Earth.
///
/// **This arm exists because that table was not reproducible from the tree.**
/// It was produced by hand-editing a private constant six times and
/// transcribing the output, in a task that had already caught one sweep drafted
/// from estimate and one probe edit silently defeated by `cargo fmt`. The record
/// could not distinguish a real row from a typo. It can now: the sweep is one
/// run, and `earth_table_depth_m` is `pub` for exactly this.
///
/// The swept quantity is `gain * earth_table_depth_m(..)`, which is the shipped
/// `water_table_depth_m`'s definition, so the gain-1 row is a positive control
/// rather than a separate computation — it is asserted against the shipped
/// function cell by cell.
#[test]
#[ignore = "heavy: live-worldgen battery; deferred from the commit gate to the heavy set (decision 0132)"]
fn how_far_does_the_dryness_gain_reach() {
    let wc = WorldComponents::assemble().expect("canonical registries are well-formed");
    println!("gain    Deeps            Underdeep      Nadir          H3");
    // [gain][seed] for each reported statistic.
    let mut deeps = vec![vec![]; SWEPT_GAINS.len()];
    let mut underdeep = vec![vec![]; SWEPT_GAINS.len()];
    let mut nadir = vec![vec![]; SWEPT_GAINS.len()];
    let mut h3 = vec![vec![]; SWEPT_GAINS.len()];

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

        // One pass over the world, then every gain evaluated off the same
        // cached columns — the world build is the expensive part and it must
        // not be paid six times.
        // (drainage, porosity, height_asl_m, metres per kelvin, cave reach).
        // Gathered in ONE pass so a column and its reach cannot come apart —
        // an earlier draft collected the reaches in a second traversal and
        // guarded the pairing with a length assert, which is a weaker thing
        // than not having two traversals.
        let mut columns: Vec<(f64, f64, f64, f64, f64)> = Vec::new();
        let mut ceiling = f64::MIN;
        for cell in geo.cells() {
            if terrain.is_ocean(cell) {
                continue;
            }
            let Some(cave) = terrain.cave_at(cell) else {
                continue;
            };
            let porosity = terrain.material_at(cell).porosity;
            ceiling = ceiling.max(porosity);
            columns.push((
                terrain.drainage_at(cell),
                porosity,
                terrain.elevation_at(cell).get() - sea,
                1000.0 / terrain.geothermal_gradient_at(cell).get(),
                cave.depth_reach_m,
            ));
        }

        for (gi, gain) in SWEPT_GAINS.iter().enumerate() {
            let mut drowned = 0usize;
            // Rung floors in ΔT: Deeps, Underdeep, Nadir.
            let mut dry = [0usize; 3];
            let mut reached = [0usize; 3];
            for &(q, p, h, m_per_k, reach) in columns.iter() {
                let table = gain * earth_table_depth_m(q, p, h);
                if *gain == 1.0 {
                    assert_eq!(
                        table,
                        water_table_depth_m(q, p, h),
                        "the gain-1 control diverged from the shipped function"
                    );
                }
                if table == 0.0 {
                    drowned += 1;
                }
                for (ri, rung) in [Band::Deeps, Band::Underdeep, Band::Nadir]
                    .iter()
                    .enumerate()
                {
                    let top_m = delta_t_range_of(*rung).0 * m_per_k;
                    if top_m > reach {
                        continue;
                    }
                    reached[ri] += 1;
                    if !is_phreatic(top_m, table) {
                        dry[ri] += 1;
                    }
                }
            }
            let share = |a: usize, b: usize| {
                if b == 0 {
                    f64::NAN
                } else {
                    100.0 * a as f64 / b as f64
                }
            };
            deeps[gi].push(share(dry[0], reached[0]));
            underdeep[gi].push(share(dry[1], reached[1]));
            nadir[gi].push(share(dry[2], reached[2]));
            h3[gi].push(100.0 * drowned as f64 / columns.len() as f64);
        }

        // Item 5: the one calibration coordinate read off the MODEL rather than
        // off Earth. If the porosity ceiling has moved, the vadose datum is
        // being instantiated at a rock this model no longer produces and
        // DRAWDOWN_SCALE_M is quietly decalibrated.
        assert!(
            (ceiling - ARABIKA_POROSITY).abs() <= POROSITY_CEILING_TOLERANCE,
            "seed {seed_value}: the model's porosity ceiling is {ceiling:.3}, but the vadose \
             datum is instantiated at ARABIKA_POROSITY = {ARABIKA_POROSITY}. Re-solve \
             DRAWDOWN_SCALE_M against the new ceiling."
        );
    }

    let row = |v: &Vec<f64>| {
        v.iter()
            .map(|x| format!("{x:.1}"))
            .collect::<Vec<_>>()
            .join("/")
    };
    for (gi, gain) in SWEPT_GAINS.iter().enumerate() {
        println!(
            "{gain:>4.1}    {:<16} {:<14} {:<14} {}",
            row(&deeps[gi]),
            row(&underdeep[gi]),
            row(&nadir[gi]),
            row(&h3[gi])
        );
    }

    // The structural claim the gain's doc rests on, checked on live worlds
    // rather than only in the algebra: scaling a floored quantity cannot move
    // anyone into or out of the drowned set.
    for gi in 1..SWEPT_GAINS.len() {
        assert_eq!(
            h3[gi], h3[0],
            "the gain moved H3 between {} and {}",
            SWEPT_GAINS[0], SWEPT_GAINS[gi]
        );
    }
}

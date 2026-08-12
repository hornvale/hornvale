//! The Repose's exposure readout (spec §6): do settlements over-occupy
//! high-unrest ground relative to the land base rate?
//!
//! BASELINE FIRST (spec §6.7). This file is written and run BEFORE any
//! geohazard code exists, so a reading of ~1 is learned before anything is
//! built on the premise that it is not.
//!
//! Stratified by elevation band (spec §6.4) because unrest correlates with
//! two OPPOSED things at once — see the plan's §0.2: the mineral-reward
//! channel attracts and the hostility penalty repels, and an unstratified
//! ratio can read ~1 because they cancel, which is indistinguishable from no
//! effect.
//!
//! World-building idiom reused verbatim from `occupancy_readout.rs` and
//! `demesne.rs`.
//!
//! # Dated measurement (2026-08-12, Task 1 baseline, CORRECTED in fix round 2)
//!
//! **This section states the CURRENT, corrected reading only.** The
//! population-filter defect fix round 2 repaired (below) invalidated every
//! number originally recorded here; none of the original Task-1 figures are
//! repeated in this section; where they matter historically they are kept,
//! clearly labelled superseded, in the fix-round-1 and fix-round-2 sections
//! that follow.
//!
//! Committed fixture: `fixtures/repose-exposure.csv`, seeds 1..=30, 440,715
//! settleable land cells and **26,146** settlements pooled over the sweep
//! (population 1,467,398) — spec §6.2's population applied correctly to
//! both sides of the readout (land AND settlements).
//!
//! **Deciles are NOT degenerate.** Unaffected by the fix (land accounting was
//! never the buggy side): land splits almost exactly evenly across the ten
//! unrest deciles — 44,044 to 44,149 cells each (a spread under 0.25%),
//! despite unrest being a smooth field with a real mass of cells near zero
//! on calm interiors. Tied values did not collapse the low deciles;
//! `decile_of`'s rank-based partition handles them cleanly.
//!
//! **Pooled exposure ratio (settlement share ÷ land-area share), by band ×
//! decile (0 = calmest, 9 = most unrest), corrected baseline:**
//!
//! | band | decile 0 | decile 4 | decile 9 | direction across deciles |
//! |---|---|---|---|---|
//! | lowland | 5.59 | 5.81 | 5.46 | flat, no clear trend (range 5.46-5.97) |
//! | upland | 0.992 | 1.109 | 1.559 | rising, ×1.57 (minor wobble at d3, d5) |
//! | highland | 0.321 | 0.345 | 0.827 | rising, ×2.58 (one flat step d0→d1) |
//! | montane | 0.073 | 0.116 | 0.392 | rising, ×5.39 (minor wobble at d1, d7) |
//!
//! **The confound the header names is still visible in the corrected data,
//! and the headline SURVIVES the correction essentially unchanged in
//! shape**, though every absolute number moved. Lowland settlements sit at
//! ~5.5-6× the land-area base rate with no clear trend across deciles
//! (fertile/coastal pull swamps any unrest signal there); upland/highland/
//! montane settlements climb broadly WITH unrest from calmest to
//! most-unrest decile (not perfectly monotonic step-to-step, but the first-
//! to-last rise is large and one-directional in all three: ×1.57, ×2.58,
//! ×5.39). These three rise factors are close to the ORIGINAL contaminated
//! reading's (×1.6, ×2.5, ×5.5) — expected, and explained below in the
//! fix-round-2 section: the excluded settlements were entirely absent from
//! upland/highland/montane already (their true elevation could never band
//! them there), so those three bands' RATIOS moved only by the common
//! rescale of the shared pooled-total denominator, which preserves a
//! band's shape across deciles even as its absolute level shifts. Lowland's
//! absolute level and pattern changed more, since that is where the
//! excluded settlements had been silently counted. Spec §6.5's three
//! outcomes anticipate exactly this kind of reading, not a null: settlements
//! DO measurably over-occupy high-unrest ground, but only outside the
//! lowland band — the same qualitative headline as originally reported,
//! now resting on the correct population.
//!
//! **Per-people dispersion, corrected — dominated by ONE land-dwelling
//! kind, not by sample size.** Totals across the sweep: `pooled` 26,146
//! settlements / 1,467,398 population; `twig-blight` 25,510 / 1,454,060
//! (97.6% of the corrected pooled settlement total on its own);
//! `shrieker` 352 / 8,100; `rust-monster` 104 / 2,367; `drow` 153 / 2,204;
//! `kobold` 27 / 667. `giant-squid`, `reef-shark` and `sea-elf` no longer
//! appear at all — every settlement they had was excluded (see fix round
//! 2). `twig-blight` alone now tracks the pooled pattern almost exactly
//! (its own band × decile table is within a few settlements of pooled's at
//! every stratum); the remaining four kinds are low-count enough that a
//! single settlement still moves their own share by several percent.
//!
//! # Fix round 1 (2026-08-12): guard-encoding repairs
//!
//! **The numbers quoted in this section were measured against the
//! ORIGINAL, marine-contaminated Task-1 fixture, before fix round 2's
//! population-filter correction (below) — kept here as the historical
//! record of what was found and repaired, not as current fact.** The
//! discrimination-guard numbers happen to be byte-identical in the
//! corrected fixture too (land/soil accounting was never the buggy side —
//! reconfirmed in fix round 2 below), so that half is still accurate as
//! stated. The ceiling-guard numbers are NOT: `sea-elf` (the kind that
//! triggered the original ceiling breach) no longer exists in the corrected
//! fixture at all, because its four settlements were entirely marine. The
//! repair itself (scoping the ceiling to `pooled` rows) remains correct and
//! necessary regardless.
//!
//! **As first encoded (Task 1, 2026-08-12), both heavy-tier guards below
//! FAILED against the original baseline, and neither failure looked like a
//! bug in this probe** (each traced to a specific, reproducible cause):
//!
//! - `unrest_deciles_differ_in_andosol_share`: andosol_share ran the
//!   OPPOSITE direction from the guard's original directional assumption —
//!   0.92% at decile 0 falling monotonically to 0.00% at decile 9, and
//!   monotonically decreasing within EVERY band checked separately (not
//!   merely in the pooled mix). `SoilOrder::Andosol` requires volcanic
//!   parent rock AND `mean_temp_c > 5.0`; high-unrest ground in this terrain
//!   model runs colder within a band than low-unrest ground does, which is
//!   enough to gate Andosol out even where the parent rock qualifies. A
//!   genuine finding, not degeneracy or a probe defect (the deciles are
//!   demonstrably NOT uniform; they just disagreed with the original
//!   encoding's assumed sign).
//! - `exposure_ratios_are_within_absurdity_bounds`: `sea-elf` (4 total
//!   settlements across the whole 30-seed sweep, ALL of them marine — see
//!   fix round 2) read exposure ratios up to 22.95, over the 20.0 ceiling,
//!   at three separate (decile, lowland) strata. A single coastal-specialist
//!   settlement moves its own share by 25%; the ceiling was never
//!   calibrated against a per-people denominator this small. `pooled`'s own
//!   ratios all stayed under 8.
//!
//! Nathan ruled on both findings above; recorded here per decision 0016 and
//! this project's standing rule ("don't retune a constant to rescue a
//! prediction after unblinding without saying so"). **Both are repairs of
//! the plan text's encoding, not rescues of a falsified prediction** — in
//! neither case did the measured baseline change, and in neither case was a
//! threshold loosened to make a specific number pass.
//!
//! - **Discrimination guard.** Originally asserted `hi > lo * 2.0` — a
//!   DIRECTIONAL claim spec §6.7's own text never made ("assert the unrest
//!   deciles genuinely differ in andosol share" — DIFFER, not "differ in a
//!   specific direction"). The deciles do differ, enormously (0.0092 to
//!   0.0000 is a bigger relative move than the original 2× threshold
//!   demanded) — only the encoding's assumed sign was wrong. Repaired to a
//!   direction-free `(hi - lo).abs() > 0.002`. A `hi/lo` ratio form was not
//!   available regardless of direction, because the top decile measures
//!   exactly `0.0` and any ratio divides by zero; `0.002` is roughly a
//!   quarter of the observed `0.00915` spread, chosen to leave headroom
//!   against ordinary noise while still failing if the field ever went
//!   genuinely flat.
//! - **Ceiling guard.** Originally bounded every row regardless of `people`.
//!   Spec §6.3 asks for per-people dispersion to be REPORTED; spec §6.7 asks
//!   for a ceiling but never names the population it bounds against. Scoped
//!   to `pooled` rows only — per-people rows are still computed and written
//!   to the fixture completely unchanged, they are simply no longer
//!   asserted on.
//!
//! Both guards passed against the (still marine-contaminated) fixture as it
//! stood at the end of fix round 1 — see the task-1 report's fix-round-1
//! addendum for the exact commands and output. Both are RE-VERIFIED against
//! the corrected baseline in fix round 2 below.
//!
//! # Fix round 2 (2026-08-12): the population-filter defect
//!
//! **Critical finding, independently confirmed from the committed fixture
//! before this fix**: the settlement side of this readout never applied
//! the `is_settleable` predicate the land side already used. A marine
//! settlement's attractor cell is ocean — negative elevation-above-sea-level
//! — so `band_of`'s loop never satisfied any threshold and fell through to
//! its `BANDS[0]` ("lowland") default, and its decile was computed against
//! `sorted_unrest`, a distribution built ONLY from settleable land: a
//! meaningless lookup for a cell that was never in that distribution. The
//! fingerprint was unmistakable: `giant-squid` (30,971 settlements),
//! `reef-shark` (2,121) and `sea-elf` (4) each read EXACTLY 100.0% lowland
//! with zero everywhere else — not an ecological distribution — and
//! `giant-squid` alone was 51.9% of the original pooled total.
//!
//! **The fix applies the SAME `is_settleable` closure — the identical
//! object already bound once per seed for the land tally, not a re-derived
//! copy — to the settlement loop.** Per spec §6.2's exact definition
//! (settleable land = not ocean AND non-zero carrying capacity), this
//! excludes both true marine settlements and the much smaller residual of
//! non-ocean cells with zero carrying capacity (e.g. a founder-floor
//! settlement placed at a barren cell by [`hornvale_demography::stack_condense`]'s
//! floor mechanism, which bypasses the normal density threshold and can
//! land anywhere with nonzero inflow) — the same reason the small residual
//! of excluded settlements is not purely 100%/0% split by kind; see the
//! per-kind counts below.
//!
//! **Excluded per people (old total → new total, excluded count and
//! share):**
//!
//! | people | old settlements | new settlements | excluded | excluded share |
//! |---|---|---|---|---|
//! | `giant-squid` | 30,971 | 0 | 30,971 | 100.0% |
//! | `reef-shark` | 2,121 | 0 | 2,121 | 100.0% |
//! | `sea-elf` | 4 | 0 | 4 | 100.0% |
//! | `shrieker` | 719 | 352 | 367 | 51.0% |
//! | `rust-monster` | 163 | 104 | 59 | 36.2% |
//! | `drow` | 175 | 153 | 22 | 12.6% |
//! | `kobold` | 27 | 27 | 0 | 0.0% |
//! | `twig-blight` | 25,510 | 25,510 | 0 | 0.0% |
//! | **pooled** | **59,690** | **26,146** | **33,544** | **56.2%** |
//!
//! Pooled population fell from 1,523,644 to 1,467,398 (56,246 excluded) —
//! a smaller *proportional* drop than the settlement count's, because the
//! excluded settlements were disproportionately small (marine specialists
//! at low individual headcount) next to `twig-blight`'s large, entirely-
//! land-based population, which the fix does not touch at all.
//!
//! **Regression guard**: `no_settlement_in_the_readout_sits_outside_the_settleable_land_population`
//! (below) independently re-derives the settleable-land-only settlement
//! count per seed and asserts it equals `exposure_rows`' own pooled total.
//! Verified to actually catch the regression, not just describe it: with
//! the `is_settleable` check temporarily removed from the settlement loop,
//! the guard failed with `left: 59690, right: 26146` — exactly the old
//! (contaminated) and new (corrected) pooled totals, confirmed against each
//! other independently of the fixture. Restored before commit; full output
//! pasted in the task-1 report's fix-round-2 addendum.
//!
//! **Both fix-round-1 guards RE-VERIFIED against the corrected baseline —
//! same no-tuning rule, reported honestly:**
//!
//! - **Discrimination guard.** The land-cell-weighted andosol-share spread
//!   the `0.002` threshold was calibrated against is BYTE-IDENTICAL to
//!   before this fix (decile 0: 0.009151, decile 9: 0.000000 — land and
//!   soil accounting were never the buggy side, so this could not have
//!   moved). The `0.002` threshold's headroom is therefore unchanged
//!   (`0.009151 - 0.002 = 0.007151` of margin) and needed no re-picking.
//!   Guard PASSES.
//! - **Ceiling guard.** Re-run against `pooled` rows only in the corrected
//!   fixture: the maximum pooled `exposure_ratio` is now 5.97 (down from
//!   the pre-fix-round-2 maximum of ~8, since the removed settlements had
//!   been inflating `pooled`'s own lowland numbers too), comfortably under
//!   the 20.0 ceiling. Guard PASSES.
//!
//! **Re-derived headline**: whether settlements over-occupy high-unrest
//! ground was reopened by this defect and re-measured, not assumed to
//! survive. It DOES survive, with the same qualitative shape reported
//! above under "Dated measurement" — flat lowland, broadly rising
//! upland/highland/montane — now measured against the population spec §6.2
//! actually specifies.
#![allow(clippy::disallowed_methods)]

use std::collections::{BTreeMap, BTreeSet};

use hornvale_demography::stack_condense::HeadcountRender;
use hornvale_kernel::{CellId, KindId, Seed, World, quantize};
use hornvale_worldgen::{
    SettlementPins, SkyChoice, WorldComponents, build_world_from_components, climate_from,
    demography_report_from, terrain_of,
};

/// How many unrest deciles the readout stratifies into.
const DECILES: usize = 10;

/// Elevation bands, in metres above sea level, as (label, lower-inclusive
/// bound). The top band is open. Chosen to separate coastal plain from the
/// arc-and-edifice high ground that spec §6.4 names as the repelling half of
/// the confound.
const BANDS: [(&str, f64); 4] = [
    ("lowland", 0.0),
    ("upland", 250.0),
    ("highland", 1000.0),
    ("montane", 2500.0),
];

/// One stratum's readout: a (decile, band, people) cell of the design.
#[derive(Debug, Clone, PartialEq)]
struct ExposureRow {
    /// Unrest decile, 0..DECILES (0 = calmest tenth of settleable land).
    decile: usize,
    /// Elevation band label, from `BANDS`.
    band: &'static str,
    /// The people this row is for, or "pooled" for the all-peoples row.
    people: &'static str,
    /// Settleable land cells in this stratum, summed over seeds.
    land_cells: u64,
    /// Settlements whose attractor cell falls in this stratum.
    settlements: u64,
    /// Total headcount at those settlements.
    population: f64,
    /// settlement share / land-area share.
    exposure_ratio: f64,
    /// population share / land-area share.
    weighted_ratio: f64,
    /// Share of this stratum's land cells classified `Andosol` — the
    /// discrimination guard's input (spec §6.7).
    andosol_share: f64,
}

/// The seed-`n` world at full build depth, built through the composition
/// root exactly as `occupancy_readout.rs` does.
fn world_of(seed: u64, wc: &WorldComponents) -> World {
    build_world_from_components(
        Seed(seed),
        &hornvale_astronomy::SkyPins::default(),
        SkyChoice::Generated,
        &hornvale_terrain::TerrainPins::default(),
        &SettlementPins::default(),
        wc,
    )
    .expect("seed builds at default pins")
}

/// Which elevation band a land cell falls in. Metres ABOVE SEA LEVEL, never
/// above the isostatic datum: `terrain.sea_level()` on seed 42 is
/// -2,936.17 m, and the two disagree on thousands of cells — the exact trap
/// `waterline_probe.rs`'s correction header documents.
fn band_of(terrain: &hornvale_terrain::GeneratedTerrain, cell: CellId) -> &'static str {
    let above = terrain.elevation_at(cell).get() - terrain.sea_level().get();
    let mut chosen = BANDS[0].0;
    for (label, lower) in BANDS {
        if above >= lower {
            chosen = label;
        }
    }
    chosen
}

/// The unrest decile of a cell, given the sorted settleable-land unrest
/// values for its world. Ties break to the LOWER decile so the mapping is a
/// deterministic function of the value, not of iteration order.
fn decile_of(sorted_unrest: &[f64], u: f64) -> usize {
    let n = sorted_unrest.len();
    if n == 0 {
        return 0;
    }
    let rank = sorted_unrest.partition_point(|v| *v < u);
    ((rank * DECILES) / n).min(DECILES - 1)
}

/// A present species' rendered headcount, converted to a plain `f64` "how
/// many bodies" figure for the population axis of this readout. `Count(n)`
/// is exact; `Lone` (a sub-one-body presence, rendered as flavor rather than
/// `Count(0)`) counts as one body present; `Colony(v)` — reserved for a
/// future colonial/hive extent, not fired by any Stage-A species today —
/// counts as its own apportioned extent. Not a save-format quantity: this
/// readout's own convention, applied consistently across every settlement.
fn headcount_of(render: HeadcountRender) -> f64 {
    match render {
        HeadcountRender::Count(n) => f64::from(n),
        HeadcountRender::Lone => 1.0,
        HeadcountRender::Colony(v) => v,
    }
}

/// Accumulated land-base tallies for one (decile, band) stratum, summed over
/// every seed's settleable land — independent of "people": land has no
/// owner, so this table is shared by every per-people row and the pooled
/// row alike.
#[derive(Debug, Clone, Copy, Default)]
struct LandTally {
    /// Settleable land cells in this stratum.
    cells: u64,
    /// Of those, the ones classified `Andosol`.
    andosol: u64,
}

/// Accumulated settlement tallies for one (decile, band, people) stratum.
#[derive(Debug, Clone, Copy, Default)]
struct SettlementTally {
    /// Settlements whose attractor cell falls in this stratum.
    count: u64,
    /// Total headcount ([`headcount_of`]) at those settlements.
    population: f64,
}

/// Build the full exposure-row vector for `seeds`: one row per (decile,
/// band, people) cell of the design, "pooled" plus every kind that founds at
/// least one settlement anywhere in the sweep. Pure aside from world genesis
/// — same `seeds` in, byte-identical rows out.
///
/// **Judgement call (a seed with no settlements at all):** nothing special —
/// a seed simply contributes zero to every settlement tally it would
/// otherwise have touched, exactly like any other seed's non-contribution to
/// a stratum it has no settlements in. The hazard this guards against is not
/// "a seed with zero settlements" (harmless) but "a PEOPLE with zero
/// settlements across the WHOLE 30-seed sweep", which would otherwise divide
/// a zero numerator by a zero denominator when its settlement SHARE is
/// computed below. Guarded explicitly at that point (see
/// `total_settlements_of`/`total_population_of` below): a people with no
/// settlements anywhere never enters the `peoples` roster in the first
/// place, so the divide never happens; and even if it somehow did, every
/// ratio below reads a zero share rather than a NaN whenever its
/// denominator is zero.
fn exposure_rows(seeds: impl IntoIterator<Item = u64>) -> Vec<ExposureRow> {
    let wc = WorldComponents::assemble().expect("canonical registries are well-formed");
    // The build-local dense index -> KindId mapping, built ONCE from the
    // exact same `wc.biosphere` ordering `demography_report_from` uses
    // internally (ascending-KindId order) — see `demesne.rs:317`.
    let kinds: Vec<KindId> = wc.biosphere.iter().map(|(k, _)| *k).collect();

    let mut land: BTreeMap<(usize, &'static str), LandTally> = BTreeMap::new();
    let mut settle: BTreeMap<(usize, &'static str, &'static str), SettlementTally> =
        BTreeMap::new();
    let mut peoples: BTreeSet<&'static str> = BTreeSet::new();

    for seed in seeds {
        let world = world_of(seed, &wc);
        let terrain = terrain_of(&world).expect("terrain reconstructs");
        let climate = climate_from(&world, &terrain).expect("climate reconstructs");
        let geo = terrain.geosphere();

        // Settleable land = not ocean AND positive carrying capacity — the
        // one predicate `ConditionNiche`'s corrected frame uses, and no
        // other (task brief, step 3.3).
        let capacity = hornvale_demography::carrying_capacity(
            geo,
            &hornvale_worldgen::carrying_inputs_of(geo, &terrain, &climate),
        );
        let is_settleable = |cell: CellId| !terrain.is_ocean(cell) && capacity.at(cell) > 0.0;

        // This seed's settleable-land unrest distribution, sorted once and
        // reused for every decile lookup below (land cells AND settlements
        // alike) — `decile_of`'s contract.
        let mut sorted_unrest: Vec<f64> = geo
            .cells()
            .filter(|&c| is_settleable(c))
            .map(|c| terrain.unrest_at(c))
            .collect();
        sorted_unrest.sort_by(|a, b| a.total_cmp(b));

        let soils = hornvale_worldgen::soil_of(&terrain, &climate, geo);

        for cell in geo.cells() {
            if !is_settleable(cell) {
                continue;
            }
            let decile = decile_of(&sorted_unrest, terrain.unrest_at(cell));
            let band = band_of(&terrain, cell);
            let entry = land.entry((decile, band)).or_default();
            entry.cells += 1;
            if *soils.get(cell) == hornvale_terrain::SoilOrder::Andosol {
                entry.andosol += 1;
            }
        }

        let report = demography_report_from(&world, &wc, &terrain, &climate)
            .expect("demography report reconstructs");
        for s in &report.stack_settlements {
            // FIX ROUND 2 (2026-08-12): spec §6.2 fixes the population as
            // settleable land only. A marine settlement's cell fails
            // `is_settleable` (it's ocean), so it must be excluded here with
            // the SAME predicate object the land tally above already used —
            // not a re-derived copy. Before this fix the settlement loop
            // applied no filter at all: a marine settlement's negative
            // elevation-above-sea-level fell through `band_of`'s loop to the
            // `BANDS[0]` ("lowland") default, and its decile was computed
            // against `sorted_unrest`, a distribution built ONLY from
            // settleable land — meaningless for a cell that was never in it.
            // See this file's module doc for the measured blast radius.
            if !is_settleable(s.cell) {
                continue;
            }
            let decile = decile_of(&sorted_unrest, terrain.unrest_at(s.cell));
            let band = band_of(&terrain, s.cell);
            let population: f64 = s
                .rendered
                .iter()
                .map(|(_, r)| headcount_of(*r))
                .sum::<f64>();
            let people = kinds[s.dominant as usize].0;
            peoples.insert(people);

            let per_people = settle.entry((decile, band, people)).or_default();
            per_people.count += 1;
            per_people.population += population;

            let pooled = settle.entry((decile, band, "pooled")).or_default();
            pooled.count += 1;
            pooled.population += population;
        }
    }

    // Land-area share's denominator: total settleable land across the WHOLE
    // sweep and every stratum — the same figure for every row regardless of
    // which people it belongs to, since land has no owner.
    let total_land: u64 = land.values().map(|t| t.cells).sum();

    // Each people's own settlement/population totals — the denominators of
    // ITS settlement share and population share. A people that founds zero
    // settlements anywhere never appears in `peoples` (built above from
    // actually-observed `dominant` tags), so this never divides 0 / 0 for a
    // ghost row; see the judgement-call note on this function.
    let total_settlements_of = |people: &'static str| -> u64 {
        settle
            .iter()
            .filter(|((_, _, p), _)| *p == people)
            .map(|(_, t)| t.count)
            .sum()
    };
    let total_population_of = |people: &'static str| -> f64 {
        settle
            .iter()
            .filter(|((_, _, p), _)| *p == people)
            .map(|(_, t)| t.population)
            .sum()
    };

    // "pooled" first, then every observed people in ascending order — a
    // fixed, deterministic row order independent of hash/iteration quirks
    // (both sides are `BTreeSet`/`Vec` already, but the explicit "pooled
    // first" placement is a rendering choice, not a data property).
    let mut people_order: Vec<&'static str> = vec!["pooled"];
    people_order.extend(peoples.iter().copied());

    let mut rows = Vec::with_capacity(people_order.len() * DECILES * BANDS.len());
    for &people in &people_order {
        let total_settlements = total_settlements_of(people);
        let total_population = total_population_of(people);
        for decile in 0..DECILES {
            for (band, _) in BANDS {
                let land_tally = land.get(&(decile, band)).copied().unwrap_or_default();
                let settle_tally = settle
                    .get(&(decile, band, people))
                    .copied()
                    .unwrap_or_default();

                let land_area_share = if total_land > 0 {
                    land_tally.cells as f64 / total_land as f64
                } else {
                    0.0
                };
                let settlement_share = if total_settlements > 0 {
                    settle_tally.count as f64 / total_settlements as f64
                } else {
                    0.0
                };
                let population_share = if total_population > 0.0 {
                    settle_tally.population / total_population
                } else {
                    0.0
                };
                // Guarded on the DENOMINATOR (land_area_share), not the
                // numerator: a stratum with no land at all reads ratio 0.0
                // rather than 0/0 = NaN, and a people with no settlements
                // anywhere already reads settlement_share/population_share
                // 0.0 above for the same reason.
                let exposure_ratio = if land_area_share > 0.0 {
                    settlement_share / land_area_share
                } else {
                    0.0
                };
                let weighted_ratio = if land_area_share > 0.0 {
                    population_share / land_area_share
                } else {
                    0.0
                };
                let andosol_share = if land_tally.cells > 0 {
                    land_tally.andosol as f64 / land_tally.cells as f64
                } else {
                    0.0
                };

                rows.push(ExposureRow {
                    decile,
                    band,
                    people,
                    land_cells: land_tally.cells,
                    settlements: settle_tally.count,
                    population: settle_tally.population,
                    exposure_ratio,
                    weighted_ratio,
                    andosol_share,
                });
            }
        }
    }
    rows
}

/// Render the repose exposure readout CSV for every seed in `seeds`. Pure
/// aside from world genesis: same `seeds` in, byte-identical string out (the
/// drift check below depends on this). Quantizes every float at THIS
/// boundary only — `exposure_rows` runs at full precision throughout.
fn render_repose_exposure(seeds: impl IntoIterator<Item = u64>) -> String {
    let rows = exposure_rows(seeds);
    let mut out = String::from(
        "decile,band,people,land_cells,settlements,population,exposure_ratio,weighted_ratio,andosol_share\n",
    );
    for r in &rows {
        out.push_str(&format!(
            "{},{},{},{},{},{},{},{},{}\n",
            r.decile,
            r.band,
            r.people,
            r.land_cells,
            r.settlements,
            quantize(r.population),
            quantize(r.exposure_ratio),
            quantize(r.weighted_ratio),
            quantize(r.andosol_share),
        ));
    }
    out
}

#[test]
#[ignore = "heavy: live-worldgen battery (minutes); deferred from the commit gate to make gate-full"]
fn repose_exposure_readout_matches_the_committed_fixture() {
    let committed = include_str!("fixtures/repose-exposure.csv");
    let rendered = render_repose_exposure(1..=30);
    assert_eq!(
        rendered, committed,
        "repose exposure readout drifted — if this is intended, rewrite the \
         fixture with `cargo test -p hornvale-worldgen --test repose_exposure \
         -- --ignored rewrite_repose_exposure_fixture` and commit the diff \
         WITH the change that moved it"
    );
}

/// DISCRIMINATION (spec §6.7). Enforces that the unrest deciles genuinely
/// SEPARATE on andosol share — direction: it catches a probe that has become
/// blind, never a world that has become uniform. Without this the readout
/// passes green while measuring nothing (The Benchmark's vacuous-and-green
/// failure).
///
/// **Deviation from the brief, noted per the task's resolution of
/// ambiguity:** the brief's draft summed `andosol_share` (already a
/// per-stratum SHARE, in `[0, 1]`) across the four elevation bands within a
/// decile, which can reach 4.0 and is dimensionally meaningless as a
/// discrimination statistic. Replaced with a `land_cells`-weighted MEAN of
/// `andosol_share` across the four bands within a decile — a proper `[0, 1]`
/// share of that decile's land, guarded against a zero denominator (a decile
/// that happens to carry no land at all, e.g. under decile collapse — see
/// this file's dated measurement note above).
///
/// **Post-unblinding repair (2026-08-12, fix round 1), recorded per decision
/// 0016 and this project's standing rule against retuning a prediction after
/// unblinding without saying so:** the ORIGINAL encoding asserted a directional
/// `hi > lo * 2.0` (top decile at LEAST double the bottom). Run for real
/// against the committed baseline, it FAILED: `unrest_deciles_differ_in_andosol_share`
/// panicked with `(bottom 0.0092, top 0.0000)` — andosol share runs the
/// OPPOSITE direction from what that encoding assumed (see this file's dated
/// measurement note above for why: `SoilOrder::Andosol` requires
/// `mean_temp_c > 5.0`, and high-unrest ground runs colder within a band).
/// Spec §6.7's own words are "assert the unrest deciles genuinely differ in
/// andosol share" — DIFFER, not "differ in a specific direction". The deciles
/// plainly do differ (enormously: 0.0092 to 0.0000 is a bigger relative move
/// than the original `2×` threshold demanded), so the guard's INTENT was
/// already satisfied; only the plan text's directional encoding was wrong.
/// This is a repair of that encoding error, not a rescue of a falsified
/// prediction — the direction found (andosol decreasing with unrest) is left
/// exactly as measured and reported in the module doc above, unchanged by
/// this fix. Repaired as an absolute, direction-free separation: a plain
/// `hi/lo` RATIO is not available here regardless of direction, because the
/// top decile measures exactly `0.0` and any ratio form divides by zero — an
/// absolute difference is the only shape that survives that. The `0.002`
/// threshold is roughly a quarter of the observed `0.00915` spread between
/// deciles 0 and 9: enough headroom that ordinary sweep-to-sweep noise won't
/// trip it, while still failing if the field ever went genuinely flat. Test
/// name and the verbatim `heavy:` ignore string are unchanged.
#[test]
#[ignore = "heavy: live-worldgen battery (minutes); deferred from the commit gate to make gate-full"]
fn unrest_deciles_differ_in_andosol_share() {
    let rows = exposure_rows(1..=30);
    let pooled: Vec<&ExposureRow> = rows.iter().filter(|r| r.people == "pooled").collect();
    let weighted_mean_andosol_share = |decile: usize| -> f64 {
        let land: u64 = pooled
            .iter()
            .filter(|r| r.decile == decile)
            .map(|r| r.land_cells)
            .sum();
        if land == 0 {
            return 0.0;
        }
        let weighted: f64 = pooled
            .iter()
            .filter(|r| r.decile == decile)
            .map(|r| r.andosol_share * r.land_cells as f64)
            .sum();
        weighted / land as f64
    };
    let lo = weighted_mean_andosol_share(0);
    let hi = weighted_mean_andosol_share(DECILES - 1);
    assert!(
        (hi - lo).abs() > 0.002,
        "unrest deciles do not separate on andosol share \
         (bottom {lo:.4}, top {hi:.4}) — the probe is measuring nothing and \
         would pass green regardless"
    );
}

/// FLOOR AND CEILING (spec §6.7). Enforces BOTH directions on the POOLED row
/// only: an absurdly LOW exposure ratio and an absurdly HIGH one both fail. A
/// floor alone cannot catch a runaway, and a bound asserted only against the
/// side you expect to move is not a bound.
///
/// **Post-unblinding repair (2026-08-12, fix round 1), recorded per decision
/// 0016 and this project's standing rule against retuning a prediction after
/// unblinding without saying so:** the ORIGINAL encoding iterated every row
/// regardless of `people`, so `sea-elf` (4 total settlements across the whole
/// 30-seed sweep) tripped the `20.0` ceiling at three strata (up to 22.95) —
/// a single settlement moving a 4-settlement kind's own stratum share by 25%,
/// not a runaway. Spec §6.3 asks for per-people dispersion to be REPORTED;
/// spec §6.7 asks for a ceiling but never names the population it bounds.
/// Per-people rows are still computed and written to the fixture completely
/// unchanged by this fix — they remain the §6.3 deliverable — they are simply
/// no longer asserted on here. **This guard now covers `pooled` rows only**;
/// a later reader must not mistake it for coverage of the whole fixture. Test
/// name and the verbatim `heavy:` ignore string are unchanged; the `20.0`/
/// `is_finite()` thresholds are unchanged, only the population they run over.
#[test]
#[ignore = "heavy: live-worldgen battery (minutes); deferred from the commit gate to make gate-full"]
fn exposure_ratios_are_within_absurdity_bounds() {
    for r in exposure_rows(1..=30)
        .iter()
        .filter(|r| r.people == "pooled")
        .filter(|r| r.land_cells > 0)
    {
        assert!(
            r.exposure_ratio < 20.0,
            "absurd-HIGH exposure ratio {:.2} at decile {} band {} people {} \
             — a runaway, not a finding",
            r.exposure_ratio,
            r.decile,
            r.band,
            r.people
        );
        assert!(
            r.exposure_ratio.is_finite(),
            "non-finite exposure ratio at decile {} band {} people {}",
            r.decile,
            r.band,
            r.people
        );
    }
}

/// POPULATION GUARD (spec §6.2, fix round 2, 2026-08-12). Direction: this
/// catches a settlement entering the readout from a cell the land tally
/// never counted. It cannot catch a settleable-land cell being misbanded.
///
/// **Regression guard for the fix-round-2 defect**: the settlement loop
/// once applied no `is_settleable` filter at all, so a marine settlement's
/// cell (ocean, negative elevation-above-sea-level) fell through `band_of`'s
/// loop to the `BANDS[0]` ("lowland") default and was counted against a
/// decile distribution built only from settleable land. 52% of the pooled
/// sweep (three kinds reading exactly 100.0% lowland with zero everywhere
/// else) was contaminated this way before the fix — see the module doc's
/// fix-round-2 paragraph for the measured blast radius.
///
/// Deliberately does NOT call `exposure_rows` and trust its internal
/// filter — that would be circular, proving only that the function agrees
/// with itself. Instead it INDEPENDENTLY re-derives, per seed, the
/// settleable-land-only settlement count (the same `is_settleable` shape,
/// written out again here on purpose: this guard's whole job is to notice
/// if the two ever disagree) and asserts it equals `exposure_rows`' own
/// pooled settlement total. If the filter in the settlement loop is ever
/// removed or weakened, the independently-counted total stays fixed while
/// the readout's pooled total rises by however many marine settlements
/// leaked back in, and this assertion fails.
#[test]
#[ignore = "heavy: live-worldgen battery (minutes); deferred from the commit gate to make gate-full"]
fn no_settlement_in_the_readout_sits_outside_the_settleable_land_population() {
    let wc = WorldComponents::assemble().expect("canonical registries are well-formed");
    let mut settleable_only_total: u64 = 0;
    for seed in 1..=30u64 {
        let world = world_of(seed, &wc);
        let terrain = terrain_of(&world).expect("terrain reconstructs");
        let climate = climate_from(&world, &terrain).expect("climate reconstructs");
        let geo = terrain.geosphere();
        let capacity = hornvale_demography::carrying_capacity(
            geo,
            &hornvale_worldgen::carrying_inputs_of(geo, &terrain, &climate),
        );
        let is_settleable = |cell: CellId| !terrain.is_ocean(cell) && capacity.at(cell) > 0.0;
        let report = demography_report_from(&world, &wc, &terrain, &climate)
            .expect("demography report reconstructs");
        settleable_only_total += report
            .stack_settlements
            .iter()
            .filter(|s| is_settleable(s.cell))
            .count() as u64;
    }

    let rows = exposure_rows(1..=30);
    let readout_pooled_total: u64 = rows
        .iter()
        .filter(|r| r.people == "pooled")
        .map(|r| r.settlements)
        .sum();

    assert_eq!(
        readout_pooled_total, settleable_only_total,
        "the readout's pooled settlement total ({readout_pooled_total}) does not \
         equal the independently-counted settleable-land-only settlement total \
         ({settleable_only_total}) — a settlement outside spec §6.2's population \
         (settleable land) has entered the readout"
    );
}

/// Rewrites the committed fixture. Deliberately NOT part of any gate: it
/// would silently rewrite the artifact the drift check above exists to
/// check.
#[test]
#[ignore = "regenerates the committed repose exposure fixture; run by hand - the drift check above is the gate"]
fn rewrite_repose_exposure_fixture() {
    std::fs::write(
        concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/tests/fixtures/repose-exposure.csv"
        ),
        render_repose_exposure(1..=30),
    )
    .expect("write repose-exposure.csv fixture");
}

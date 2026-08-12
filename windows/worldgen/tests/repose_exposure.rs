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
//! # Dated measurement (2026-08-12, Task 1 baseline, CORRECTED in fix round 2,
//! band labels RELABELLED in fix round 3)
//!
//! **This section states the CURRENT, corrected reading only.** The
//! population-filter defect fix round 2 repaired (below) invalidated every
//! number originally recorded here; none of the original Task-1 figures are
//! repeated in this section; where they matter historically they are kept,
//! clearly labelled superseded, in the fix-round-1 and fix-round-2 sections
//! that follow. Fix round 3 renamed the four elevation-band labels (see
//! `BANDS`'s doc comment and the land-share table just below) — a pure
//! relabelling that moved no number; the fix-round-3 section at the end of
//! this doc proves that byte-for-byte.
//!
//! Committed fixture: `fixtures/repose-exposure.csv`, seeds 1..=30, 440,715
//! settleable land cells and **26,146** settlements pooled over the sweep
//! (population 1,467,398) — spec §6.2's population applied correctly to
//! both sides of the readout (land AND settlements).
//!
//! **The band labels are datum-neutral metre ranges, and land share by band
//! is the context every reader needs before reading the table below.**
//! `elevation_at` is relative to an isostatic reference datum, not to sea
//! level: sea level itself sits far below zero on that datum
//! (`terrain.sea_level()` on seed 42 is -2,936.17 m — `waterline_probe.rs`'s
//! correction header). So height-above-sea-level on ordinary continental
//! land routinely reaches several kilometres, and an absolute 2500 m cut
//! captures nearly half of all settleable land — not a mountain fringe:
//!
//! | band | land cells | land share | settlement share |
//! |---|---|---|---|
//! | `0-250m` | 50,792 | 11.5% | 65.5% |
//! | `250-1000m` | 55,764 | 12.7% | 14.4% |
//! | `1000-2500m` | 140,937 | 32.0% | 13.0% |
//! | `2500m+` | 193,222 | 43.8% | 7.0% |
//!
//! `2500m+` is the LARGEST band by land area of the four, at 43.8% — a
//! sentence like "the effect is strongest in mountain terrain" would be a
//! claim about mountains this data does not support. The stratification
//! itself is unaffected by any of this: it still separates land
//! monotonically by elevation, exactly what spec §6.4 asks for as confound
//! control. What was wrong, and is fixed as of this section, is only that
//! the ORIGINAL labels (`lowland`/`upland`/`highland`/`montane`) imported an
//! Earth intuition this world's datum does not support.
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
//! | `0-250m` | 5.59 | 5.81 | 5.46 | flat, no clear trend (range 5.46-5.97) |
//! | `250-1000m` | 0.992 | 1.109 | 1.559 | rising, ×1.57 (minor wobble at d3, d5) |
//! | `1000-2500m` | 0.321 | 0.345 | 0.827 | rising, ×2.58 (one flat step d0→d1) |
//! | `2500m+` | 0.073 | 0.116 | 0.392 | rising, ×5.39 (minor wobble at d1, d7) |
//!
//! **The confound the header names is still visible in the corrected data,
//! and the headline SURVIVES the correction essentially unchanged in
//! shape**, though every absolute number moved. `0-250m` settlements sit at
//! ~5.5-6× the land-area base rate with no clear trend across deciles
//! (fertile/coastal pull swamps any unrest signal there); the three higher
//! bands climb broadly WITH unrest from calmest to most-unrest decile (not
//! perfectly monotonic step-to-step, but the first-to-last rise is large
//! and one-directional in all three: ×1.57, ×2.58, ×5.39). These three rise
//! factors are close to the ORIGINAL contaminated reading's (×1.6, ×2.5,
//! ×5.5) — expected, and explained below in the fix-round-2 section: the
//! excluded settlements were entirely absent from the three higher bands
//! already (their true elevation could never band them there), so those
//! three bands' RATIOS moved only by the common rescale of the shared
//! pooled-total denominator, which preserves a band's shape across deciles
//! even as its absolute level shifts. `0-250m`'s absolute level and pattern
//! changed more, since that is where the excluded settlements had been
//! silently counted. Spec §6.5's three outcomes anticipate exactly this
//! kind of reading, not a null: settlements DO measurably over-occupy
//! high-unrest ground, but only above the coastal fringe — the same
//! qualitative headline as originally reported, now resting on the correct
//! population.
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
//! every stratum, and it is the only one of the five surviving kinds present
//! in all four bands); the remaining four kinds are low-count enough that a
//! single settlement still moves their own share by several percent.
//!
//! **Those four low-count kinds sit at EXACTLY 100% of the `2500m+` band —
//! and this is a real preference, not a repeat of the fix-round-2 artifact.**
//! `kobold` (n=27), `rust-monster` (n=104), `drow` (n=153) and `shrieker`
//! (n=352) each place every one of their settlements in `2500m+` and none
//! anywhere else. Two things distinguish this from the marine-default
//! artifact fix round 2 removed: `band_of`'s fallback is the FIRST band
//! (`0-250m`), never the last, so a default-branch bug would read as
//! spurious `0-250m` concentration, not `2500m+` — the opposite band from
//! what these four kinds show; and the probability of landing 100% in
//! `2500m+` by chance is vanishingly small given that band's own 43.8% land
//! share — for `kobold`'s smallest sample, `0.438^27 ≈ 2.1×10⁻¹⁰` (the other
//! three kinds' probabilities are smaller still, by many more orders of
//! magnitude, since they have larger n). This reads as a genuine niche
//! preference for high ground, not an artifact of either defect this file
//! has already found and fixed.
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
//!   at three separate (decile, `0-250m`) strata. A single coastal-specialist
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
//! its `BANDS[0]` (the `0-250m` band) default, and its decile was computed
//! against `sorted_unrest`, a distribution built ONLY from settleable land: a
//! meaningless lookup for a cell that was never in that distribution. The
//! fingerprint was unmistakable: `giant-squid` (30,971 settlements),
//! `reef-shark` (2,121) and `sea-elf` (4) each read EXACTLY 100.0% `0-250m`
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
//!   been inflating `pooled`'s own `0-250m` numbers too), comfortably under
//!   the 20.0 ceiling. Guard PASSES.
//!
//! **Re-derived headline**: whether settlements over-occupy high-unrest
//! ground was reopened by this defect and re-measured, not assumed to
//! survive. It DOES survive, with the same qualitative shape reported
//! above under "Dated measurement" — flat `0-250m`, broadly rising through
//! the three higher bands — now measured against the population spec §6.2
//! actually specifies.
//!
//! # Dated measurement (2026-08-12, Task 2: the three counterfactual arms)
//!
//! Task 2 ablates the channels by which unrest could reach siting, through
//! `hornvale_worldgen::ChannelMask`. Task 1's baseline is unchanged by any of
//! it — `ChannelMask::NONE` is an IEEE-754 no-op, and the committed fixture
//! above is byte-identical after the threading.
//!
//! **Instrument sensitivity (the calibration a later reader needs before
//! judging any future null).** Attractor-cell symmetric difference against
//! the unmasked baseline, 30 seeds, 59,690 settlements total of which 26,146
//! are on settleable land (that figure matching Task 1's corrected pooled
//! total exactly, independently re-derived):
//!
//! | arm | moved (all) | moved (settleable land) | share of the 26,146 |
//! |---|---|---|---|
//! | A — hostility penalty ablated | 3,042 | 3,036 | 11.6% |
//! | B — mineral unrest term ablated | 430 | 399 | 1.5% |
//!
//! Both positive controls fire, so the harness is NOT blind and arm C's null
//! is decidable. Arm C is green: no siting-path source reads a soil order or
//! a soil fertility.
//!
//! **A movement count cannot attribute, so the gradient was re-taken under
//! each arm** (`which_channel_carries_the_exposure_gradient`). Pooled
//! exposure ratio, decile 0 → decile 9 rise factor, denominators
//! byte-identical across arms by construction:
//!
//! | band | base | arm A (hostility off) | arm B (mineral unrest off) |
//! |---|---|---|---|
//! | `0-250m` | ×0.978 | ×1.109 | ×0.976 |
//! | `250-1000m` | ×1.572 | ×1.880 | ×1.554 |
//! | `1000-2500m` | ×2.578 | ×2.751 | ×2.506 |
//! | `2500m+` | ×5.395 | ×5.836 | ×5.125 |
//!
//! **The headline, and it refutes the task's own going-in prediction.** Task
//! 1 established that soil never reaches siting and that andosol is
//! anti-correlated with unrest, and concluded that the mineral/prospectivity
//! channel was therefore "the only remaining candidate explanation" for the
//! rising exposure ratio. Measured, it is not the explanation:
//!
//! - **Arm B moves the gradient in the predicted DIRECTION but nowhere near
//!   the required MAGNITUDE.** Ablating the unrest term inside prospectivity
//!   lowers the rise in all four bands, so the mineral reward does contribute
//!   positively — but in the strongest band it removes ×0.270 of a ×4.395
//!   excess over unity, about 6% of the effect. If this channel were the
//!   cause, ablating it would collapse the rise toward ×1. It does not move
//!   it out of its own significant figures.
//! - **Arm A runs the OTHER way: the hostility penalty is a brake on the
//!   effect, not its source.** Ablating it RAISES the rise in every band
//!   (×5.395 → ×5.836 in `2500m+`, ×1.572 → ×1.880 in `250-1000m`), which is
//!   the physically correct sign — removing a term that repels settlement
//!   from high-unrest ground lets more settlement onto it. The channel that
//!   moves the most settlements (11.6% of them, 7.6× arm B's count) is
//!   therefore the one that OPPOSES the measured effect.
//! - **So most of Task 1's gradient is carried by neither channel.** With the
//!   attracting channel worth ~6% and the other channel pushing the opposite
//!   way, the residual is the bulk of it: unrest CO-VARIES with whatever
//!   actually sites settlements rather than causing the siting through either
//!   of its two direct wires. Identifying that confound is not in Task 2's
//!   scope and is not guessed at here.
//!
//! **The caveat that bounds arm B, and it is a roster fact, not a physics
//! one.** Only two shipped kinds weight the `MINERAL` axis at all — `xorn`
//! (subterranean) and `rust-monster` — and both are pure-`MINERAL`
//! (`ResourceVector::new(&[(MINERAL, 1.0)])`, `domains/species/src/lib.rs`).
//! `rust-monster` founds 104 of the 26,146 settleable-land settlements
//! (0.4%); `twig-blight` alone is 97.6% and takes nothing from that axis.
//! Arm B's small effect therefore measures how little the CURRENT ROSTER
//! reaches through the mineral channel, and must not be read as "the mineral
//! channel is intrinsically weak". A roster with a mineral-weighted people in
//! it would have to re-take this reading — the same shelf life spec §6.6
//! declares for arm C.
#![allow(clippy::disallowed_methods)]

use std::collections::{BTreeMap, BTreeSet};

use hornvale_demography::stack_condense::HeadcountRender;
use hornvale_kernel::{CellId, KindId, Seed, World, quantize};
use hornvale_worldgen::{
    ChannelMask, SettlementPins, SkyChoice, WorldComponents, build_world_from_components,
    climate_from, demography_report_from, demography_report_from_masked, terrain_of,
};

/// How many unrest deciles the readout stratifies into.
const DECILES: usize = 10;

/// Elevation bands, in metres above sea level, as (label, lower-inclusive
/// bound). The top band is open. Chosen to separate the coastal fringe from
/// the higher ground that spec §6.4 names as the repelling half of the
/// confound.
///
/// **Labels are explicit, datum-neutral metre ranges (fix round 3,
/// 2026-08-12), not terrain names.** They originally read `lowland`/
/// `upland`/`highland`/`montane` — Earth-intuitive names this world's
/// numbers do not support: `elevation_at` is relative to an isostatic
/// datum, not sea level, and sea level itself sits far below zero on that
/// datum (`terrain.sea_level()` on seed 42 is -2,936.17 m — see
/// `waterline_probe.rs`'s correction header). So height-above-sea-level
/// routinely runs to several kilometres on ordinary continental land, and
/// the top band (`2500m+`, née "montane") turns out to hold 43.8% of all
/// settleable land — the LARGEST of the four bands, not a mountain fringe.
/// See this file's module doc for the full land-share table and its
/// implications. The stratification itself — separating land monotonically
/// by elevation, spec §6.4's confound control — is unchanged: only the
/// labels moved, never the thresholds, order, or semantics. Renaming is
/// PROVEN not to move a number in the task-1 report's fix-round-3 addendum
/// (a byte-for-byte diff of every non-label column, before vs. after).
const BANDS: [(&str, f64); 4] = [
    ("0-250m", 0.0),
    ("250-1000m", 250.0),
    ("1000-2500m", 1000.0),
    ("2500m+", 2500.0),
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
    exposure_rows_masked(seeds, ChannelMask::NONE)
}

/// [`exposure_rows`] under a channel ablation (Task 2). At
/// [`ChannelMask::NONE`] this IS `exposure_rows` — the delegation above is
/// the only caller that path has, so the committed fixture is what pins it.
///
/// **Only the SETTLEMENT side moves under a mask.** The land tally, the
/// unrest deciles, the elevation bands and the `is_settleable` population are
/// all derived from terrain and climate through the UNMASKED
/// `carrying_inputs_of` below, so every arm is scored against a denominator
/// byte-identical to the baseline's. That is deliberate: an arm that moved
/// its own denominator could not be attributed, since a ratio would then
/// shift for two reasons at once and neither could be separated.
fn exposure_rows_masked(
    seeds: impl IntoIterator<Item = u64>,
    mask: ChannelMask,
) -> Vec<ExposureRow> {
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

        let report = demography_report_from_masked(&world, &wc, &terrain, &climate, mask)
            .expect("demography report reconstructs");
        for s in &report.stack_settlements {
            // FIX ROUND 2 (2026-08-12): spec §6.2 fixes the population as
            // settleable land only. A marine settlement's cell fails
            // `is_settleable` (it's ocean), so it must be excluded here with
            // the SAME predicate object the land tally above already used —
            // not a re-derived copy. Before this fix the settlement loop
            // applied no filter at all: a marine settlement's negative
            // elevation-above-sea-level fell through `band_of`'s loop to the
            // `BANDS[0]` (the `0-250m` band) default, and its decile was computed
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
/// loop to the `BANDS[0]` (the `0-250m` band) default and was counted
/// against a decile distribution built only from settleable land. 52% of the
/// pooled sweep (three kinds reading exactly 100.0% `0-250m` with zero
/// everywhere else) was contaminated this way before the fix — see the module doc's
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
///
/// claim: structural(seed: 1..=30) — an exact count identity between two
/// independent derivations of spec §6.2's population over one fixed sweep,
/// not a per-seed property and not a search for an instance.
///
/// **This tag was MISSING when the test landed in Task 1**, and
/// `cli/tests/claim_shape.rs` is a default-deny workspace lint, so the branch
/// was red on `make gate` from that commit until Task 2 found it. Recorded
/// rather than quietly fixed: a crate-scoped green
/// (`cargo test -p hornvale-worldgen`) cannot see this lint, because the
/// enforcement tests live in `cli/`.
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

/// The mask's identity element is an IEEE-754 no-op at both of its
/// application points. Direction: this catches the `NONE` path computing
/// anything other than the formula it replaced; it cannot catch the
/// arithmetic being wrong in a way the ORIGINAL formula was also wrong in.
///
/// Bit-level, not approximate: `to_bits()` equality, because "close enough"
/// is exactly the class of drift the determinism contract forbids.
///
/// **Deviation from the task brief's draft, and why the draft could not
/// work.** The brief compared `per_species_suitability(…, ChannelMask::NONE)`
/// against a `suitability_fields_unmasked(…)` sibling. After the threading
/// there IS no unmasked sibling: `per_species_suitability` *delegates* to
/// `per_species_suitability_masked(…, NONE)`, so a comparison of the two is a
/// comparison of one code path against itself — VACUOUS, and green no matter
/// what the mask does. The same objection kills the equivalent comparison at
/// the report level (`demography_report_from` likewise delegates). A
/// self-comparison is precisely the evidence shape this project has been
/// burned by before, so it is not shipped here.
///
/// What is shipped instead is an INDEPENDENT recomputation. Each of the two
/// application points has exactly one pre-mask formula, and the test writes
/// that formula out again, by hand, from the terrain — then asserts the
/// shipped `NONE` path agrees bit-for-bit over every cell of three worlds:
///
/// - `hostility` — `carrying_inputs_of` (which is `carrying_inputs_at` at
///   `NONE`) must equal `terrain.unrest_at(cell).clamp(0.0, 1.0)`.
/// - `MINERAL` supply — `mineral_supply_field` (which is
///   `mineral_supply_field_masked` at `NONE`) must equal
///   `0.0` at sea and `terrain.prospectivity_at(c) * scale` on land.
///
/// **The CHAIN-level identity is carried by a different, older guard**, and
/// deliberately: `repose_exposure_readout_matches_the_committed_fixture`
/// above re-renders 26,146 settlements over 30 seeds through
/// `demography_report_from` — and that fixture was authored in Task 1, before
/// any mask existed. If any rung of the four-deep threading delegated with
/// something other than the identity, or dropped the mask, that fixture
/// drifts. A pre-existing golden authored against the pre-mask code is a
/// stronger statement about the whole pipeline than any assertion this test
/// could make about it, and it costs nothing extra.
///
/// claim: structural(seed: [1, 42, 30]) — bit-identity of two arithmetics
/// over three named worlds, every cell of each. Three seeds, not thirty:
/// a bit-identity that holds on every cell of three whole globes and fails on
/// a fourth would be a different defect (a seed-dependent code path) than
/// anything this seam can express.
#[test]
#[ignore = "heavy: live-worldgen battery (minutes); deferred from the commit gate to make gate-full"]
fn channel_mask_none_is_bit_identical_to_the_unmasked_path() {
    let wc = WorldComponents::assemble().expect("components assemble");
    for seed in [1u64, 42, 30] {
        let world = world_of(seed, &wc);
        let terrain = terrain_of(&world).expect("terrain reconstructs");
        let climate = climate_from(&world, &terrain).expect("climate reconstructs");
        let geo = terrain.geosphere();

        // Application point 1: the unrest hostility penalty.
        let inputs = hornvale_worldgen::carrying_inputs_of(geo, &terrain, &climate);
        for cell in geo.cells() {
            let expected = terrain.unrest_at(cell).clamp(0.0, 1.0);
            assert_eq!(
                inputs.get(cell).hostility.to_bits(),
                expected.to_bits(),
                "seed {seed}, cell {cell:?}: hostility at ChannelMask::NONE diverged \
                 from the formula it replaced"
            );
        }

        // Application point 2: the unrest term inside mineral prospectivity.
        let scale = 1.0_f64;
        let mineral = hornvale_worldgen::mineral_supply_field(geo, &terrain, scale);
        for cell in geo.cells() {
            let expected = if terrain.is_ocean(cell) {
                0.0
            } else {
                terrain.prospectivity_at(cell) * scale
            };
            assert_eq!(
                mineral.get(cell).to_bits(),
                expected.to_bits(),
                "seed {seed}, cell {cell:?}: mineral supply at ChannelMask::NONE \
                 diverged from the formula it replaced"
            );
        }
    }
}

/// One seed's settlement attractor cells under one channel mask. Per-seed
/// (never pooled across seeds) so that two different seeds' identical cell
/// indices cannot cancel in the symmetric difference; the caller sums the
/// per-seed differences.
///
/// Takes an ALREADY-BUILT world/terrain/climate: the three arms differ only
/// in the mask, and the world they are read against is the same one — genesis
/// is by far the expensive half, so building it once per seed and taking
/// three reports off it costs a third of what three independent 30-seed
/// sweeps would, for byte-identical results (the report is pure over the
/// committed world; see [`demography_report_from`]'s doc).
fn attractor_cells_of(
    world: &hornvale_kernel::World,
    wc: &WorldComponents,
    terrain: &hornvale_terrain::GeneratedTerrain,
    climate: &hornvale_climate::GeneratedClimate,
    mask: ChannelMask,
) -> BTreeSet<CellId> {
    demography_report_from_masked(world, wc, terrain, climate, mask)
        .expect("demography report reconstructs")
        .stack_settlements
        .iter()
        .map(|s| s.cell)
        .collect()
}

/// The counterfactual arm (spec §6.6, amended by plan §0.1/§0.2).
///
/// Direction: arms A and B are POSITIVE CONTROLS — they must MOVE siting, and
/// a green here means the harness can detect movement. Arm C is the null
/// under test. C alone would be an empty diff with no positive control, which
/// is exactly the evidence shape that has misled this project before.
///
/// **On the arm-C `include_str!` grep:** it is a coarse instrument. It sees
/// four whole demography files and nine named worldgen functions, matches
/// four spellings, and would miss a soil term reaching siting through a
/// helper in a file it does not include, through a re-exported alias, or
/// through a value passed in from a caller that read the soil itself. It is
/// NOT a proof of absence. It is a tripwire on the specific wiring this
/// arm's null depends on, and the `assert!(moved_a > 0)` /
/// `assert!(moved_b > 0)` positive controls are what carry the real
/// evidential weight.
///
/// **Arm C runs FIRST in the body even though it is the last arm
/// logically.** It is a static scan costing microseconds, while the controls
/// cost a 30-seed sweep; and if the null is stale, the sweep's numbers are
/// being read against a claim that no longer holds, so there is nothing to
/// measure. Failing before the expensive half is the honest order.
///
/// # Dated measurement (2026-08-12, Task 2)
///
/// See the module doc's Task-2 section for the measured `moved_a` /
/// `moved_b` counts — the calibration of this instrument's sensitivity, which
/// a later reader needs before judging any future null it reports.
///
/// claim: readout(seed: 1..=30, off-gate heavy:) — reports how many
/// settlements each ablation moves over one fixed sweep, and asserts only
/// that the counts are non-zero (the positive controls). Not a rate: no
/// threshold on the counts is claimed, precisely because none was
/// preregistered and inventing one after unblinding would be a rescue.
#[test]
#[ignore = "heavy: live-worldgen battery (minutes); deferred from the commit gate to make gate-full"]
fn the_counterfactual_arms_separate_a_true_null_from_a_wiring_gap() {
    // ARM C, FIRST because it is free and because a stale null makes the
    // expensive half unreadable. Soil fertility has no application point in
    // the siting path, so the arm is a CONNECTIVITY assertion: no siting-path
    // code reads a soil order or a soil fertility. If this ever fails,
    // someone wired The Ground into siting and this probe's null is stale —
    // which is precisely the shelf life spec §6.6 declares.
    //
    // Clause 1: the demography domain, where the whole siting ARITHMETIC
    // lives (carrying capacity, the coexistence pack, both condensations).
    // Whole files, because the correct reading here is that the domain does
    // not know soil exists at all.
    let demography_siting = [
        include_str!("../../../domains/demography/src/carrying_capacity.rs"),
        include_str!("../../../domains/demography/src/coexist.rs"),
        include_str!("../../../domains/demography/src/condense.rs"),
        include_str!("../../../domains/demography/src/stack_condense.rs"),
    ];
    for src in demography_siting {
        for spelling in SOIL_SPELLINGS {
            assert!(
                !src.contains(spelling),
                "a demography siting source now mentions `{spelling}` — The \
                 Ground has been wired into the siting arithmetic, and this \
                 probe's arm-C null is STALE. Re-take the reading and rewrite \
                 this assertion."
            );
        }
    }

    // Clause 2: the composition root's siting chain, function by function.
    // NOT whole-file: `windows/worldgen/src/lib.rs` is a ~13k-line god file
    // that ALREADY contains `classify_soil(` — inside `soil_of`, which Task 1
    // established is called from five places, NONE of them in the siting
    // path. A whole-file grep here is therefore red on arrival and proves
    // nothing; the arm has to name the functions whose output actually
    // reaches K.
    let worldgen = include_str!("../src/lib.rs");
    for signature in SITING_CHAIN {
        let body = body_of(worldgen, signature);
        for spelling in SOIL_SPELLINGS {
            assert!(
                !body.contains(spelling),
                "the siting-chain function `{signature}` now mentions \
                 `{spelling}` — The Ground has been wired into siting, and \
                 this probe's arm-C null is STALE. Re-take the reading and \
                 rewrite this assertion."
            );
        }
    }

    let wc = WorldComponents::assemble().expect("components assemble");
    let arm_a_mask = ChannelMask {
        hostility: true,
        ..ChannelMask::NONE
    };
    let arm_b_mask = ChannelMask {
        mineral_unrest: true,
        ..ChannelMask::NONE
    };

    let mut base_all = 0usize;
    let mut base_settleable = 0usize;
    let mut moved_a_all = 0usize;
    let mut moved_b_all = 0usize;
    let mut moved_a_settleable = 0usize;
    let mut moved_b_settleable = 0usize;
    for seed in 1..=30u64 {
        let world = world_of(seed, &wc);
        let terrain = terrain_of(&world).expect("terrain reconstructs");
        let climate = climate_from(&world, &terrain).expect("climate reconstructs");
        let geo = terrain.geosphere();

        // THE POPULATION UNDER MEASUREMENT, defined ONCE per seed from the
        // UNMASKED world and applied identically to every arm. Two reasons it
        // is not re-derived per arm: an arm moves carrying capacity, so an
        // arm-local predicate would move the population and the settlements
        // at the same time, and no count could then be attributed to either;
        // and this is the same `is_settleable` spec §6.2 fixes for the
        // exposure readout above, so the arms are calibrating the instrument
        // on the population that readout actually measures.
        let capacity = hornvale_demography::carrying_capacity(
            geo,
            &hornvale_worldgen::carrying_inputs_of(geo, &terrain, &climate),
        );
        let is_settleable = |cell: &CellId| !terrain.is_ocean(*cell) && capacity.at(*cell) > 0.0;

        let base = attractor_cells_of(&world, &wc, &terrain, &climate, ChannelMask::NONE);
        let a = attractor_cells_of(&world, &wc, &terrain, &climate, arm_a_mask);
        let b = attractor_cells_of(&world, &wc, &terrain, &climate, arm_b_mask);

        base_all += base.len();
        base_settleable += base.iter().filter(|c| is_settleable(c)).count();
        moved_a_all += base.symmetric_difference(&a).count();
        moved_b_all += base.symmetric_difference(&b).count();
        // SAME predicate on BOTH sides of the difference — the fix-round-2
        // lesson from this file's module doc, applied to a set difference
        // rather than a ratio: filtering one side only would count every
        // marine settlement as "moved".
        moved_a_settleable += base
            .symmetric_difference(&a)
            .filter(|c| is_settleable(c))
            .count();
        moved_b_settleable += base
            .symmetric_difference(&b)
            .filter(|c| is_settleable(c))
            .count();
    }
    println!(
        "REPOSE ARMS: base attractor cells {base_all} (settleable {base_settleable}); \
         moved_a {moved_a_all} (settleable {moved_a_settleable}); \
         moved_b {moved_b_all} (settleable {moved_b_settleable})"
    );

    // POSITIVE CONTROLS, asserted on the SETTLEABLE-LAND population — the one
    // the exposure readout above measures, and therefore the one arm C's null
    // is a null about. Movement among marine attractor cells would prove the
    // mask does something, but not that it does something where the effect
    // under investigation lives. If either of these is zero, the harness is
    // blind and arm C's null above means nothing.
    assert!(
        moved_a_settleable > 0,
        "arm A (hostility ablated) moved NO settleable-land settlement across \
         30 seeds — the ablation harness cannot see movement in the population \
         spec §6.2 measures, so arm C proves nothing"
    );
    assert!(
        moved_b_settleable > 0,
        "arm B (mineral unrest ablated) moved NO settleable-land settlement \
         across 30 seeds — the ablation harness cannot see movement in the \
         population spec §6.2 measures, so arm C proves nothing"
    );
}

/// The spellings arm C treats as "a soil term has reached here". Coarse by
/// construction — see [`the_counterfactual_arms_separate_a_true_null_from_a_wiring_gap`]'s
/// doc for what it cannot see.
const SOIL_SPELLINGS: [&str; 4] = [
    "classify_soil",
    "SoilOrder",
    "soil_of(",
    "terrain::fertility(",
];

/// Every function in `windows/worldgen/src/lib.rs` whose output reaches a
/// species' per-cell K, and therefore reaches settlement condensation: the
/// four rungs of The Repose's mask threading plus the five supply/substrate
/// fields `per_species_suitability_masked` reads. Named by their exact
/// signature line so [`body_of`] fails loudly rather than silently scanning
/// nothing if one is renamed.
const SITING_CHAIN: [&str; 9] = [
    "pub(crate) fn demography_report_with_beta_from(",
    "pub fn per_species_suitability_masked(",
    "pub fn carrying_inputs_at(",
    "pub fn mineral_supply_field_masked(",
    "pub fn substrate_field(",
    "pub fn forage_supply_field(",
    "pub fn prey_supply_field(",
    "pub fn detritus_supply_field(",
    "pub fn marine_forage_supply_field(",
];

/// The source text of one top-level function in a rustfmt-formatted file:
/// from its signature line to the first line that is exactly `}` at column
/// zero, which is where rustfmt closes a top-level item and nowhere else
/// inside one. Panics if the signature is absent — a renamed function must
/// fail this probe loudly, not silently scan an empty string.
fn body_of(src: &str, signature: &str) -> String {
    let start = src
        .find(signature)
        .unwrap_or_else(|| panic!("siting-chain function `{signature}` not found in lib.rs"));
    let rest = &src[start..];
    let end = rest
        .find("\n}\n")
        .unwrap_or_else(|| panic!("no top-level close brace after `{signature}`"));
    rest[..end].to_string()
}

/// ATTRIBUTION (Task 2). Which channel carries the exposure gradient Task 1
/// measured — the question a movement COUNT cannot answer.
///
/// **Why this test exists and the arms test is not enough.** The arms above
/// count how many settlements MOVED under each ablation. A count is
/// direction-free: it says the harness can see the channel, not which way the
/// channel pushes, and certainly not which channel produces the rising
/// exposure ratio. Attributing on a movement count would be this project's
/// named top failure mode — the right measurement against the wrong claim.
/// So this readout re-takes Task 1's actual statistic (pooled exposure ratio
/// by band, decile 0 vs decile 9) under each ablation and reports how the
/// RISE FACTOR moves.
///
/// Denominators are byte-identical across arms by construction — see
/// [`exposure_rows_masked`] — so a change in a rise factor can only come from
/// settlements relocating.
///
/// **This test asserts only that the instrument is not blind**, never a
/// direction or a magnitude. The measured numbers go in the module doc, where
/// a reader can weigh them; encoding a directional expectation here after
/// unblinding would be a rescue, and this campaign publishes what it finds.
#[test]
#[ignore = "heavy: live-worldgen battery (minutes); deferred from the commit gate to make gate-full"]
fn which_channel_carries_the_exposure_gradient() {
    let arm_a_mask = ChannelMask {
        hostility: true,
        ..ChannelMask::NONE
    };
    let arm_b_mask = ChannelMask {
        mineral_unrest: true,
        ..ChannelMask::NONE
    };

    let arms: [(&str, Vec<ExposureRow>); 3] = [
        ("base", exposure_rows_masked(1..=30, ChannelMask::NONE)),
        ("armA-hostility", exposure_rows_masked(1..=30, arm_a_mask)),
        ("armB-mineral", exposure_rows_masked(1..=30, arm_b_mask)),
    ];

    let ratio_at = |rows: &[ExposureRow], band: &str, decile: usize| -> f64 {
        rows.iter()
            .find(|r| r.people == "pooled" && r.band == band && r.decile == decile)
            .map(|r| r.exposure_ratio)
            .unwrap_or(0.0)
    };
    for (label, rows) in &arms {
        for (band, _) in BANDS {
            let lo = ratio_at(rows, band, 0);
            let hi = ratio_at(rows, band, DECILES - 1);
            let rise = if lo > 0.0 { hi / lo } else { f64::NAN };
            println!(
                "REPOSE ATTRIBUTION: arm {label:14} band {band:10} d0 {lo:.4} \
                 d9 {hi:.4} rise x{rise:.3}"
            );
        }
    }

    // BLINDNESS GUARD, and nothing more: each arm must actually change the
    // pooled settlement distribution, or the table above is three copies of
    // one reading and says nothing about attribution. Direction and magnitude
    // are deliberately unasserted (see this test's doc).
    let base_settlements: Vec<u64> = arms[0]
        .1
        .iter()
        .filter(|r| r.people == "pooled")
        .map(|r| r.settlements)
        .collect();
    for (label, rows) in arms.iter().skip(1) {
        let arm_settlements: Vec<u64> = rows
            .iter()
            .filter(|r| r.people == "pooled")
            .map(|r| r.settlements)
            .collect();
        assert_ne!(
            arm_settlements, base_settlements,
            "arm {label} left the pooled settlement distribution untouched — \
             the attribution table is measuring one reading three times"
        );
    }
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

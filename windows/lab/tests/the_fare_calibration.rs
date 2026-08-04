//! The Fare's preregistered measurement (spec `docs/superpowers/specs/
//! 2026-08-04-the-fare-design.md`, frozen 2026-08-04): The Mire measured
//! **passability** through a threshold on the connection graph and found a
//! null. This asks the same question on **cost**, routed over a weathered
//! traversal-cost field rather than over the weathered graph.
//!
//! ## Why a heavy-tier calibration test and not lab metrics
//!
//! Nine studies declare `"metrics": "all"` and `windows/lab/src/study.rs:225`
//! resolves that to the whole registry, unfiltered. A registered metric that
//! routes per world would run on every census world and drift nine studies'
//! committed `rows.csv`. Same reasoning as `the_mire_calibration.rs`.
//!
//! ## Why the transform lives here rather than in `windows/worldgen`
//!
//! Promoting it would create a `pub` item with no production consumer — the
//! exact defect (`MAP-weather-gating-is-unconsumed`) this campaign exposes in
//! The Mire. Spec §9 makes the production change a non-goal.
//!
//! ## The sampling frame (spec §4a, project owner's ruling 2026-08-04)
//!
//! F1/F2/F3 are measured between **deterministically sampled land cells**,
//! not settlement pairs — settlements sit in six to twelve tight carpets on
//! high-capacity river basins, and weather bites hardest on marginal ground
//! (boggy lowland, snow-loaded upland) that is exactly where settlements are
//! not. The settlement-pair routing this file measured before this ruling
//! survives as a labelled **secondary** readout (`PILOT-SETTLEMENT` lines),
//! never the headline. See `the_fares_pilot`'s doc comment for the geographic
//! frame's construction.

use hornvale_astronomy::SkyPins;
use hornvale_climate::provider::GeneratedClimate;
use hornvale_climate::snowpack::DEFAULT_SNOWPACK;
use hornvale_climate::substrate::SubstrateField;
use hornvale_climate::wetness::{DEFAULT_WETNESS, receptivity};
use hornvale_kernel::math::acos;
use hornvale_kernel::{CellId, CellMap, Geosphere, Seed, Value};
use hornvale_terrain::TerrainPins;
use hornvale_topology::{CostSweep, least_cost_from};
use hornvale_worldgen::graph_derive::weather_conductance_factor;
use hornvale_worldgen::{
    BASE_COST, BuildDepth, SettlementPins, SkyChoice, WorldComponents,
    build_world_to_with_artifacts, traversal_cost,
};
use std::collections::BTreeSet;

/// AUTHORED (spec §5a), **not calibrated**: the floor
/// `weather_conductance_factor`'s output is clamped to before inversion.
///
/// The factor can be exactly `0.0` — its penalties sum past 1.0
/// (`MUD_PENALTY 0.6 + SNOW_PENALTY 0.7`) and it clamps to `[0,1]`. Unfloored,
/// `1/f` diverges. Mapping that to `u64::MAX` would be worse than wrong: it
/// would reintroduce the passability threshold The Mire already measured, and
/// `least_cost` returns `None` for an unreachable pair, so weather-impassable
/// cells would silently drop the hardest pairs from the sample and bias F1
/// toward those that stayed connected.
///
/// At `0.25` the cap is `4x` flat ground, binding only in the combined
/// mud-plus-snow extreme. Ordinary cases sit inside it: saturated unfrozen
/// (`f = 0.4`) gives `2.5x`, fully snowed (`f = 0.3`) gives `3.3x`.
const WEATHER_FACTOR_FLOOR: f64 = 0.25;

/// The additive weather surcharge, in the same integer cost units as
/// [`BASE_COST`]. AUTHORED per spec §5a; anchored on the tabletop convention
/// that difficult terrain costs double movement, so `1/f` is the
/// movement-rate multiplier and `f = 0.5` yields exactly `+BASE_COST`.
///
/// **Additive, not multiplicative, and that is load-bearing.** The field is
/// `BASE_COST + slope_term` with the slope term reaching the thousands on an
/// escarpment. A multiplier would make weather's absolute contribution scale
/// with relief — largest on mountains, smallest on the flat routes travellers
/// actually use — which is a terrain effect wearing a weather costume.
fn weather_surcharge(factor: f64) -> u64 {
    let f = factor.clamp(WEATHER_FACTOR_FLOOR, 1.0);
    (BASE_COST as f64 * (1.0 / f - 1.0)).round() as u64
}

mod transform {
    use super::*;

    #[test]
    fn dry_ground_pays_nothing() {
        assert_eq!(weather_surcharge(1.0), 0);
    }

    #[test]
    fn difficult_terrain_doubles_flat_ground() {
        // The authored anchor: f = 0.5 is a halved movement rate, so the
        // surcharge equals BASE_COST exactly and flat ground costs double.
        assert_eq!(weather_surcharge(0.5), BASE_COST);
    }

    #[test]
    fn the_floor_binds_and_weather_is_never_impassable() {
        // THE KEYSTONE. `weather_conductance_factor` really does return
        // exactly 0.0 for a saturated, snowed, unfrozen cell. Remove the
        // clamp in `weather_surcharge` and `1.0 / 0.0` is `inf`, which
        // saturates to `u64::MAX` under Rust's `as` cast — turning a muddy
        // cell into an impassable one and silently dropping pairs from the
        // sample. This test must go red under that mutation.
        let at_zero = weather_surcharge(0.0);
        assert_ne!(
            at_zero,
            u64::MAX,
            "weather must never make a cell impassable"
        );
        assert_eq!(at_zero, weather_surcharge(WEATHER_FACTOR_FLOOR));
        // 4x flat ground: BASE_COST * (1/0.25 - 1) == BASE_COST * 3.
        assert_eq!(at_zero, BASE_COST * 3);
    }

    #[test]
    fn no_factor_in_the_unit_interval_yields_an_impassable_cost() {
        for i in 0..=100 {
            let f = i as f64 / 100.0;
            assert_ne!(
                weather_surcharge(f),
                u64::MAX,
                "factor {f} produced u64::MAX"
            );
        }
    }

    #[test]
    fn surcharge_is_monotone_non_increasing_in_the_factor() {
        let mut previous = u64::MAX;
        for i in 0..=100 {
            let f = i as f64 / 100.0;
            let s = weather_surcharge(f);
            assert!(
                s <= previous,
                "factor {f} raised the surcharge to {s} from {previous}"
            );
            previous = s;
        }
    }
}

/// Days sampled across one converged annual trajectory. Matches The Mire's
/// 12 so F3's latitude comparison is like-for-like.
const SAMPLE_DAYS: usize = 12;

/// The pilot's seeds. Deliberately small: its job is to measure cost and set
/// the floors, not to answer F1-F4. Spec §6.
const PILOT_SEEDS: std::ops::RangeInclusive<u64> = 1..=5;

/// How many reachable settlement pairs F2's re-routing check and the
/// redundancy control sample per seed — a deterministic stride over the
/// ordered reachable-pair list, same idiom as `the_mire_calibration.rs`'s
/// `H3_SAMPLE_STRIDE_TARGET`. The redundancy control needs a blocked
/// re-sweep PER PAIR (a fresh `CellMap` plus a fresh `least_cost_from`), so
/// running it over every reachable pair (up to ~62k for one pilot seed)
/// would dominate the pilot's cost; F2 is measured over the same sampled
/// subset rather than exhaustively, so the two numbers are read together
/// over one population instead of two differently-sized ones.
const PATH_SAMPLE_STRIDE_TARGET: usize = 200;

/// Target separations (degrees) the geographic frame sweeps — one `PILOT`
/// line per (seed, band), never a single chosen band. The campaign's central
/// mechanism claim is that path cost is a SUM along a route, so a longer
/// route crosses more terrain and more distinct weather: F1 and F2 should
/// therefore RISE with separation. Reporting one band would measure an
/// effect; reporting the curve across two orders of magnitude tests the
/// mechanism — a flat curve falsifies it rather than merely restating a
/// null. Coordinator-specified set, spec §4a ("the separation band... set
/// from the pilot").
const SEPARATION_BANDS_DEG: &[f64] = &[5.0, 10.0, 20.0, 40.0, 80.0];

/// How many land cells (`!Biome::is_marine()`, read off `dry != u64::MAX` —
/// the same test `traversal_cost` already encodes, so no separate biome
/// field is needed anywhere in this file) the geographic frame draws as
/// landmarks: a deterministic stride across the whole land roster, never
/// random — same idiom as [`PATH_SAMPLE_STRIDE_TARGET`]/
/// `the_mire_calibration.rs`'s `H3_SAMPLE_STRIDE_TARGET`. Every landmark
/// acts as a SOURCE for every band in [`SEPARATION_BANDS_DEG`], paired with
/// whichever OTHER landmark's actual angular separation is closest to that
/// band's target — so the day-sweep cost stays `landmarks × SAMPLE_DAYS`
/// regardless of how many bands are swept: one sweep per source per day
/// serves every band that source is used for.
const GEO_LANDMARK_STRIDE_TARGET: usize = 200;

/// The stride [`GEO_LANDMARK_STRIDE_TARGET`]'s doc comment describes.
fn geo_landmark_stride(land_cell_count: usize) -> usize {
    (land_cell_count / GEO_LANDMARK_STRIDE_TARGET).max(1)
}

/// Great-circle angular separation between two cells, in DEGREES. Never
/// converted to a physical distance — this sim defines no planetary radius
/// (spec §4a), so degrees (or [`Geosphere::hops_between`]'s hop count) is
/// the only honest unit. Cells sit on the unit sphere (`Geosphere::
/// position`), so the separation is just the angle between their two
/// position vectors — no latitude/longitude wraparound edge case near a
/// pole or the dateline the way a haversine-on-coordinates formula would
/// carry.
fn angular_separation_deg(geo: &Geosphere, a: CellId, b: CellId) -> f64 {
    let pa = geo.position(a);
    let pb = geo.position(b);
    let dot = pa[0] * pb[0] + pa[1] * pb[1] + pa[2] * pb[2];
    acos(dot.clamp(-1.0, 1.0)).to_degrees()
}

/// Among `landmarks` (excluding `landmarks[src_idx]` itself), the one whose
/// angular separation from the source is closest to `target_deg`, and that
/// achieved separation. Ties (equal `|diff|`) break on the lower `CellId` —
/// a pure function of the mesh and the target, never of iteration order,
/// same discipline `CostSweep`'s tie-break follows. Returns `(destination,
/// actual_separation_deg)`.
fn nearest_at_separation(
    geo: &Geosphere,
    landmarks: &[CellId],
    src_idx: usize,
    target_deg: f64,
) -> (CellId, f64) {
    let src = landmarks[src_idx];
    let mut best: Option<(f64, CellId)> = None;
    for (j, &candidate) in landmarks.iter().enumerate() {
        if j == src_idx {
            continue;
        }
        let diff = (angular_separation_deg(geo, src, candidate) - target_deg).abs();
        best = Some(match best {
            None => (diff, candidate),
            Some((best_diff, best_cell)) => {
                if diff.total_cmp(&best_diff).is_lt()
                    || (diff.total_cmp(&best_diff).is_eq() && candidate < best_cell)
                {
                    (diff, candidate)
                } else {
                    (best_diff, best_cell)
                }
            }
        });
    }
    let (_, dst) = best.expect("landmarks must hold at least 2 cells");
    (dst, angular_separation_deg(geo, src, dst))
}

/// One built world's cached readout surface, computed exactly once per world.
///
/// Deliberately carries no `ungated: ConnectionGraph` field. F4 (the only
/// readout that would have read the graph, `defensibility`, or the history
/// bake) is dropped from this campaign entirely (spec §4, project owner's
/// ruling) — nothing here touches any of them, and re-adding the field would
/// resurrect a `dead_code` trap this file already paid down once.
struct WorldSample {
    /// The mesh, kept for neighbour and coordinate reads.
    geo: Geosphere,
    /// The **dry** traversal-cost field — the one production plans over.
    dry: CellMap<u64>,
    /// Every settlement's cell, ascending and deduplicated.
    settlements: Vec<CellId>,
    /// The converged annual period, standard days.
    year_length: f64,
    /// Surface wetness's converged annual trajectory, every cell.
    wetness: SubstrateField,
    /// Snowpack's converged annual trajectory, every cell.
    snow: SubstrateField,
    /// The reconstructed climate, for `is_frozen_at` reads.
    climate: GeneratedClimate,
}

/// Build one world to `BuildDepth::Settlements` and assemble everything
/// [`WorldSample`] caches. Panics loudly if a seed fails to build — a silent
/// skip would quietly shrink the preregistered population.
///
/// Uses `build_world_to_with_artifacts` (which hands back the terrain and
/// climate the build already produced) rather than re-deriving either a
/// second time. Same caching discipline as `the_mire_calibration.rs`.
fn build_sample(seed: u64, wc: &WorldComponents) -> WorldSample {
    let artifacts = build_world_to_with_artifacts(
        Seed(seed),
        &SkyPins::default(),
        SkyChoice::Generated,
        &TerrainPins::default(),
        &SettlementPins::default(),
        wc,
        BuildDepth::Settlements,
    )
    .unwrap_or_else(|e| panic!("seed {seed} failed to build to BuildDepth::Settlements: {e:?}"));

    let world = artifacts.world;
    let terrain = artifacts
        .terrain
        .expect("BuildDepth::Settlements builds terrain");
    let climate = artifacts
        .climate
        .expect("BuildDepth::Settlements builds climate");

    let geo = terrain.geosphere().clone();
    let elevation = &terrain.globe().elevation;
    let biome = climate.biome_map();

    let mut settlements: Vec<CellId> = hornvale_settlement::all_settlements(&world)
        .iter()
        .map(
            |s| match world.ledger.value_of(s.id, hornvale_settlement::CELL_ID) {
                Some(Value::Number(n)) => CellId(*n as u32),
                _ => panic!("settlement {} has no cell-id fact", s.id.0),
            },
        )
        .collect();
    settlements.sort();
    settlements.dedup();

    let dry = traversal_cost(&geo, elevation, &biome);
    let (wetness, snow) =
        SubstrateField::compute_pair(&climate, &DEFAULT_WETNESS, &DEFAULT_SNOWPACK);
    let year_length = climate.year_length_std();

    WorldSample {
        geo,
        dry,
        settlements,
        year_length,
        wetness,
        snow,
        climate,
    }
}

/// The dry traversal-cost field plus this day's weather surcharge, per cell.
///
/// Reads exactly the substrate state `graph_derive`'s own `factor_at` closure
/// reads, so the cost instrument and the conductance instrument agree on the
/// world and differ only in transform. Marine cells stay `u64::MAX` untouched:
/// weather never creates or removes impassability (spec §5a).
fn weathered_cost(sample: &WorldSample, day: f64) -> CellMap<u64> {
    CellMap::from_fn(&sample.geo, |cell| {
        let base = *sample.dry.get(cell);
        if base == u64::MAX {
            return u64::MAX;
        }
        let factor = weather_conductance_factor(
            receptivity(
                sample.wetness.at(cell, day),
                DEFAULT_WETNESS.field_capacity_mm,
            ),
            sample.snow.at(cell, day),
            sample.climate.is_frozen_at(cell, day),
        );
        base.saturating_add(weather_surcharge(factor))
    })
}

/// The median of a nonempty population: the middle value for an odd-length
/// sample, the average of the two middle values for an even-length one.
/// Matches `the_mire_calibration.rs`'s `median()` convention. A bare
/// `sorted[len / 2]` is an *upper*-median on an even-length sample, which
/// silently differs from this whenever the pair count is even (4 of the 5
/// pilot seeds, in practice) — worth conforming exactly since Task 4 freezes
/// floors from these numbers.
fn median(values: &[f64]) -> f64 {
    assert!(!values.is_empty(), "median of an empty population");
    let mut sorted = values.to_vec();
    sorted.sort_by(f64::total_cmp);
    let n = sorted.len();
    if n % 2 == 1 {
        sorted[n / 2]
    } else {
        (sorted[n / 2 - 1] + sorted[n / 2]) / 2.0
    }
}

/// The stride F2/the redundancy control walk the reachable-pair list at: at
/// most every pair, but no finer than needed to land roughly
/// [`PATH_SAMPLE_STRIDE_TARGET`] samples across the whole reachable-pair
/// population. Same shape as `the_mire_calibration.rs`'s `h3_stride`.
fn path_sample_stride(pair_count: usize) -> usize {
    (pair_count / PATH_SAMPLE_STRIDE_TARGET).max(1)
}

mod weathering {
    use super::*;

    #[test]
    #[ignore = "heavy: builds a live world (tens of seconds); deferred from the commit gate"]
    fn weathering_raises_cost_somewhere_and_never_makes_a_cell_impassable() {
        // THE KEYSTONE for this task. Two failure modes it must catch: a
        // weathered field that is byte-identical to the dry one (the
        // substrate never reaching the cost field at all — the latent-
        // mechanism failure The Mire's own Task 6 test guarded against), and
        // a weathered field that turns a passable cell impassable (which
        // would silently drop pairs from F1's sample).
        let wc = WorldComponents::assemble().expect("canonical registries are well-formed");
        let sample = build_sample(1, &wc);
        let day = sample.year_length / 4.0;
        let wet = weathered_cost(&sample, day);

        let mut raised = 0usize;
        for cell in sample.geo.cells() {
            let dry = *sample.dry.get(cell);
            let w = *wet.get(cell);
            assert!(
                w >= dry,
                "weathering lowered cost at {cell:?}: {dry} -> {w}"
            );
            if dry == u64::MAX {
                assert_eq!(
                    w,
                    u64::MAX,
                    "a marine cell stopped being marine at {cell:?}"
                );
            } else {
                assert_ne!(w, u64::MAX, "weathering made {cell:?} impassable");
                if w > dry {
                    raised += 1;
                }
            }
        }
        assert!(
            raised > 0,
            "weathering raised no cell's cost in the whole world"
        );
    }

    #[test]
    #[ignore = "heavy: the pilot builds 5 live worlds and routes over them (minutes); spec §6"]
    fn the_fares_pilot() {
        // TWO independent readouts per seed, each on its own labelled PILOT
        // line, per spec §4a's re-basing (project owner's ruling
        // 2026-08-04):
        //
        // PRIMARY — `PILOT` lines, one per (seed, band): the geographic
        // land-cell frame. Landmarks are a deterministic stride over land
        // cells (`GEO_LANDMARK_STRIDE_TARGET`); every landmark pairs with
        // whichever other landmark's actual angular separation is closest
        // to each of `SEPARATION_BANDS_DEG`'s targets
        // (`nearest_at_separation`). F1/F2 are reported SEPARATELY per band
        // rather than at one chosen separation, because the campaign's
        // mechanism claim (path cost is a SUM along a route, so a longer
        // route crosses more terrain and more distinct weather) predicts
        // BOTH should rise with separation — the curve tests the mechanism,
        // a single point only measures an effect.
        //
        // SECONDARY — `PILOT-SETTLEMENT` lines, one per seed: the original
        // settlement-pair frame, unchanged in every statistic, kept because
        // it is where the forward prediction about The Keeping's
        // re-placement belongs (spec §6a) — never the headline, and
        // prefixed distinctly so the two frames can never be confused in
        // the readout.
        //
        // Both frames share: the two-pass memory discipline (costs-only
        // pass 1, sampled paths-only pass 2), F2's path-identity comparison
        // at each pair's own argmin/argmax day (never a global day), the
        // redundancy control with adjacent pairs excluded and counted
        // (`adjacent_pairs`/`redundancy_sample`), `no_alt_count` counting
        // only genuine no-alternative cases among non-adjacent pairs,
        // `total_cmp` float ordering throughout, and the sibling `median()`
        // convention. `reroute_frac`'s denominator KEEPS adjacent pairs in
        // both frames — they structurally cannot re-route, so including
        // them is conservative, never inflationary (see the comment at each
        // frame's F2 call site).
        let wc = WorldComponents::assemble().expect("canonical registries are well-formed");
        for seed in PILOT_SEEDS {
            let sample = build_sample(seed, &wc);

            // ============ PRIMARY: geographic land-cell frame (§4a) ============
            let land_cells: Vec<CellId> = sample
                .geo
                .cells()
                .filter(|&c| *sample.dry.get(c) != u64::MAX)
                .collect();
            let geo_stride = geo_landmark_stride(land_cells.len());
            let landmarks: Vec<CellId> = land_cells.iter().step_by(geo_stride).copied().collect();
            let landmark_count = landmarks.len();
            let band_count = SEPARATION_BANDS_DEG.len();

            // The dry sweep is day-invariant, so compute it once per
            // landmark and reuse it for the swing's normalization and (via
            // `path_to`) the redundancy control's best path.
            let landmark_dry_sweeps: Vec<CostSweep> = landmarks
                .iter()
                .map(|&src| least_cost_from(&sample.geo, &sample.dry, src))
                .collect();

            // Each landmark pairs with the OTHER landmark whose actual
            // separation is closest to each band's target. `pair_dst`/
            // `pair_sep` are flat, indexed `i * band_count + k`.
            let mut pair_dst: Vec<CellId> = Vec::with_capacity(landmark_count * band_count);
            let mut pair_sep: Vec<f64> = Vec::with_capacity(landmark_count * band_count);
            for i in 0..landmark_count {
                for &target in SEPARATION_BANDS_DEG {
                    let (dst, actual) = nearest_at_separation(&sample.geo, &landmarks, i, target);
                    pair_dst.push(dst);
                    pair_sep.push(actual);
                }
            }

            // Pass 1 (costs only): per (landmark, band), track the min/max
            // weathered cost across SAMPLE_DAYS and the DAY INDEX each
            // extremum occurred at. One sweep per landmark per day serves
            // EVERY band that landmark sources — day-sweep cost stays
            // landmark_count x SAMPLE_DAYS regardless of band_count. Both
            // the day's field and each landmark's sweep drop at the end of
            // their scope, same discipline as the settlement frame below.
            let mut geo_min = vec![f64::INFINITY; landmark_count * band_count];
            let mut geo_max = vec![f64::NEG_INFINITY; landmark_count * band_count];
            let mut geo_argmin_day = vec![0usize; landmark_count * band_count];
            let mut geo_argmax_day = vec![0usize; landmark_count * band_count];
            for day_idx in 0..SAMPLE_DAYS {
                let day = day_idx as f64 * sample.year_length / SAMPLE_DAYS as f64;
                let wet = weathered_cost(&sample, day);
                for (i, &src) in landmarks.iter().enumerate() {
                    let d_wet = least_cost_from(&sample.geo, &wet, src);
                    for k in 0..band_count {
                        let idx = i * band_count + k;
                        if let Some(c) = d_wet.cost_to(pair_dst[idx]) {
                            let c = c as f64;
                            if c.total_cmp(&geo_min[idx]).is_lt() {
                                geo_min[idx] = c;
                                geo_argmin_day[idx] = day_idx;
                            }
                            if c.total_cmp(&geo_max[idx]).is_gt() {
                                geo_max[idx] = c;
                                geo_argmax_day[idx] = day_idx;
                            }
                        }
                    }
                }
            }

            // Per band: F1's swings, plus the reachable-landmark population
            // (dry cost Some and > 0, geo_min finite) pass 2 walks.
            let mut geo_swings_by_band: Vec<Vec<f64>> = vec![Vec::new(); band_count];
            let mut geo_reachable_by_band: Vec<Vec<usize>> = vec![Vec::new(); band_count];
            for (i, landmark_dry_sweep) in landmark_dry_sweeps.iter().enumerate() {
                for k in 0..band_count {
                    let idx = i * band_count + k;
                    if !geo_min[idx].is_finite() {
                        continue;
                    }
                    let dst = pair_dst[idx];
                    if let Some(a) = landmark_dry_sweep.cost_to(dst)
                        && a > 0
                    {
                        geo_swings_by_band[k].push((geo_max[idx] - geo_min[idx]) / a as f64);
                        geo_reachable_by_band[k].push(i);
                    }
                }
            }

            // Pass 2 (paths): F2's re-routing fraction and the redundancy
            // control, per band, over EVERY reachable landmark pair in that
            // band — no further stride needed, GEO_LANDMARK_STRIDE_TARGET
            // already bounds the population.
            for (k, (reachable, mut swings)) in geo_reachable_by_band
                .into_iter()
                .zip(geo_swings_by_band)
                .enumerate()
            {
                let target = SEPARATION_BANDS_DEG[k];
                let mut rerouted = 0usize;
                let mut adjacent_pairs = 0usize;
                let mut redundancy_sample_count = 0usize;
                let mut redundancy_ratios: Vec<f64> = Vec::new();
                let mut no_alternative = 0usize;
                for &i in &reachable {
                    let idx = i * band_count + k;
                    let src = landmarks[i];
                    let dst = pair_dst[idx];

                    // F2: path identity at this pair's own cheapest vs
                    // costliest sampled day. Adjacent pairs are
                    // DELIBERATELY KEPT in this denominator (see the
                    // settlement frame's matching comment below for why —
                    // same reasoning, same rule, both frames).
                    let day_a =
                        geo_argmin_day[idx] as f64 * sample.year_length / SAMPLE_DAYS as f64;
                    let day_b =
                        geo_argmax_day[idx] as f64 * sample.year_length / SAMPLE_DAYS as f64;
                    let field_a = weathered_cost(&sample, day_a);
                    let field_b = weathered_cost(&sample, day_b);
                    let path_a = least_cost_from(&sample.geo, &field_a, src).path_to(dst);
                    let path_b = least_cost_from(&sample.geo, &field_b, src).path_to(dst);
                    assert!(
                        path_a.is_some() && path_b.is_some(),
                        "seed {seed} band {target} landmark {i} lost reachability \
                         under weathering (the keystone's guarantee that weathering \
                         never makes a passable cell impassable should preclude this)"
                    );
                    if path_a != path_b {
                        rerouted += 1;
                    }

                    // Redundancy control: on the DRY field, the best path's
                    // interior blocked, re-swept. Adjacent pairs (empty
                    // interior) are counted and EXCLUDED — see the
                    // settlement frame's matching comment below.
                    let best_cost = landmark_dry_sweeps[i]
                        .cost_to(dst)
                        .expect("geo_reachable only holds pairs with a finite dry cost");
                    let best_path = landmark_dry_sweeps[i]
                        .path_to(dst)
                        .expect("a finite dry cost implies a dry path");
                    if best_path.len() == 2 {
                        adjacent_pairs += 1;
                        continue;
                    }
                    redundancy_sample_count += 1;
                    let blocked: BTreeSet<CellId> =
                        best_path[1..best_path.len() - 1].iter().copied().collect();
                    let scratch = CellMap::from_fn(&sample.geo, |c| {
                        if blocked.contains(&c) {
                            u64::MAX
                        } else {
                            *sample.dry.get(c)
                        }
                    });
                    match least_cost_from(&sample.geo, &scratch, src).cost_to(dst) {
                        Some(second_best) => {
                            redundancy_ratios.push(second_best as f64 / best_cost as f64);
                        }
                        None => no_alternative += 1,
                    }
                }
                assert_eq!(
                    redundancy_sample_count,
                    redundancy_ratios.len() + no_alternative,
                    "seed {seed} band {target}: every non-adjacent pair must land in \
                     exactly one of redundancy_ratios or no_alternative"
                );

                swings.sort_by(f64::total_cmp);
                let median_swing = if swings.is_empty() {
                    f64::NAN
                } else {
                    median(&swings)
                };
                let max_swing = swings.last().copied().unwrap_or(f64::NAN);

                redundancy_ratios.sort_by(f64::total_cmp);
                let median_redundancy = if redundancy_ratios.is_empty() {
                    f64::NAN
                } else {
                    median(&redundancy_ratios)
                };
                let max_redundancy = redundancy_ratios.last().copied().unwrap_or(f64::NAN);
                let reroute_frac = if reachable.is_empty() {
                    f64::NAN
                } else {
                    rerouted as f64 / reachable.len() as f64
                };

                let mut seps: Vec<f64> = reachable
                    .iter()
                    .map(|&i| pair_sep[i * band_count + k])
                    .collect();
                seps.sort_by(f64::total_cmp);
                let sep_actual_median = if seps.is_empty() {
                    f64::NAN
                } else {
                    median(&seps)
                };

                // Per-(seed, band) progress: The Mire lost an hour of
                // wall-clock to a run with no visible progress and nothing
                // recoverable.
                println!(
                    "PILOT seed={seed} band_deg={target:.1} landmarks={landmark_count} \
                     geo_pairs={} sep_actual_median_deg={sep_actual_median:.3} \
                     median_swing={median_swing:.6} max_swing={max_swing:.6} \
                     reroute_frac={reroute_frac:.6} adjacent_pairs={adjacent_pairs} \
                     redundancy_sample={redundancy_sample_count} \
                     median_redundancy={median_redundancy:.6} max_redundancy={max_redundancy:.6} \
                     no_alt_count={no_alternative}",
                    reachable.len()
                );
            }

            // ============ SECONDARY: settlement-pair frame (perishable; §4a, §6a) ============
            let n = sample.settlements.len();

            // The dry sweep is day-invariant, so compute it once per source
            // and reuse it for the markup control, the swing, and (via
            // `path_to`) the redundancy control's best path.
            let dry_sweeps: Vec<CostSweep> = sample
                .settlements
                .iter()
                .map(|&src| least_cost_from(&sample.geo, &sample.dry, src))
                .collect();

            // --- Control: fixed-day markup at year_length / 4.0 ---
            let markup_day = sample.year_length / 4.0;
            let wet_markup = weathered_cost(&sample, markup_day);
            let mut markups: Vec<f64> = Vec::new();
            for (i, &src) in sample.settlements.iter().enumerate() {
                let d_wet = least_cost_from(&sample.geo, &wet_markup, src);
                for &dst in &sample.settlements {
                    if dst == src {
                        continue;
                    }
                    if let (Some(a), Some(b)) = (dry_sweeps[i].cost_to(dst), d_wet.cost_to(dst))
                        && a > 0
                    {
                        markups.push((b as f64 - a as f64) / a as f64);
                    }
                }
            }
            markups.sort_by(f64::total_cmp);
            let median_markup = if markups.is_empty() {
                f64::NAN
            } else {
                median(&markups)
            };
            let max_markup = markups.last().copied().unwrap_or(f64::NAN);

            // --- Pass 1 (costs only): F1's per-pair (max - min) weathered
            // cost across SAMPLE_DAYS, plus the DAY INDEX each extremum
            // occurred at (pass 2 needs this to know which two days to
            // re-sweep for F2's path comparison). Weathered field built once
            // per day, every settlement swept against that one field, both
            // dropped at the end of the day's scope — never all
            // SAMPLE_DAYS x settlements sweeps held at once, only O(n^2)
            // scalars.
            let mut min_cost = vec![f64::INFINITY; n * n];
            let mut max_cost = vec![f64::NEG_INFINITY; n * n];
            let mut argmin_day = vec![0usize; n * n];
            let mut argmax_day = vec![0usize; n * n];
            for day_idx in 0..SAMPLE_DAYS {
                let day = day_idx as f64 * sample.year_length / SAMPLE_DAYS as f64;
                let wet = weathered_cost(&sample, day);
                for (i, &src) in sample.settlements.iter().enumerate() {
                    let d_wet = least_cost_from(&sample.geo, &wet, src);
                    for (j, &dst) in sample.settlements.iter().enumerate() {
                        if i == j {
                            continue;
                        }
                        if let Some(c) = d_wet.cost_to(dst) {
                            let c = c as f64;
                            let idx = i * n + j;
                            if c.total_cmp(&min_cost[idx]).is_lt() {
                                min_cost[idx] = c;
                                argmin_day[idx] = day_idx;
                            }
                            if c.total_cmp(&max_cost[idx]).is_gt() {
                                max_cost[idx] = c;
                                argmax_day[idx] = day_idx;
                            }
                        }
                    }
                }
            }

            let mut swings: Vec<f64> = Vec::new();
            let mut reachable_pairs: Vec<(usize, usize)> = Vec::new();
            for (i, dry_sweep) in dry_sweeps.iter().enumerate() {
                for (j, &dst) in sample.settlements.iter().enumerate() {
                    if i == j {
                        continue;
                    }
                    let idx = i * n + j;
                    if !min_cost[idx].is_finite() {
                        continue;
                    }
                    if let Some(a) = dry_sweep.cost_to(dst)
                        && a > 0
                    {
                        swings.push((max_cost[idx] - min_cost[idx]) / a as f64);
                        reachable_pairs.push((i, j));
                    }
                }
            }
            swings.sort_by(f64::total_cmp);
            let median_swing = if swings.is_empty() {
                f64::NAN
            } else {
                median(&swings)
            };
            let max_swing = swings.last().copied().unwrap_or(f64::NAN);

            // --- Pass 2 (paths, sampled): F2's re-routing fraction and the
            // redundancy control, over a deterministic stride sample of
            // `reachable_pairs`.
            let stride = path_sample_stride(reachable_pairs.len());
            let mut path_sample_count = 0usize;
            let mut rerouted = 0usize;
            let mut adjacent_pairs = 0usize;
            let mut redundancy_sample_count = 0usize;
            let mut redundancy_ratios: Vec<f64> = Vec::new();
            let mut no_alternative = 0usize;
            for &(i, j) in reachable_pairs.iter().step_by(stride) {
                path_sample_count += 1;
                let src = sample.settlements[i];
                let dst = sample.settlements[j];
                let idx = i * n + j;

                // F2: path identity at this pair's own cheapest vs costliest
                // sampled day. Re-sweeps at exactly those two days — never
                // all SAMPLE_DAYS again. Adjacent pairs (see the redundancy
                // control below) are DELIBERATELY KEPT in this denominator,
                // unlike the redundancy control, which excludes them: an
                // adjacent pair structurally cannot re-route (there is only
                // ever the one edge, weathered or not, so `path_a == path_b`
                // always), so counting it here makes `reroute_frac`
                // conservative — a slight undercount, never an inflation.
                // Do not "fix" this by excluding them too.
                let day_a = argmin_day[idx] as f64 * sample.year_length / SAMPLE_DAYS as f64;
                let day_b = argmax_day[idx] as f64 * sample.year_length / SAMPLE_DAYS as f64;
                let field_a = weathered_cost(&sample, day_a);
                let field_b = weathered_cost(&sample, day_b);
                let path_a = least_cost_from(&sample.geo, &field_a, src).path_to(dst);
                let path_b = least_cost_from(&sample.geo, &field_b, src).path_to(dst);
                assert!(
                    path_a.is_some() && path_b.is_some(),
                    "seed {seed} pair ({i},{j}) lost reachability under weathering \
                     (the keystone's guarantee that weathering never makes a \
                     passable cell impassable should preclude this)"
                );
                if path_a != path_b {
                    rerouted += 1;
                }

                // Redundancy control: on the DRY field, the best path's
                // interior cells (everything but the two endpoints) blocked
                // to u64::MAX in a scratch field, re-swept. A directly
                // adjacent pair's best path has NO interior to block — the
                // re-sweep would trivially reproduce the same path and read
                // exactly 1.0, indistinguishable from a genuine equal-cost
                // parallel corridor — so it is counted and EXCLUDED here,
                // never folded into the ratio distribution or into
                // `no_alternative` (it was never tested, so it is neither
                // "has an alternative" nor "has none").
                let best_cost = dry_sweeps[i]
                    .cost_to(dst)
                    .expect("reachable_pairs only holds pairs with a finite dry cost");
                let best_path = dry_sweeps[i]
                    .path_to(dst)
                    .expect("a finite dry cost implies a dry path");
                if best_path.len() == 2 {
                    adjacent_pairs += 1;
                    continue;
                }
                redundancy_sample_count += 1;
                let blocked: BTreeSet<CellId> =
                    best_path[1..best_path.len() - 1].iter().copied().collect();
                let scratch = CellMap::from_fn(&sample.geo, |c| {
                    if blocked.contains(&c) {
                        u64::MAX
                    } else {
                        *sample.dry.get(c)
                    }
                });
                match least_cost_from(&sample.geo, &scratch, src).cost_to(dst) {
                    Some(second_best) => {
                        redundancy_ratios.push(second_best as f64 / best_cost as f64);
                    }
                    None => no_alternative += 1,
                }
            }
            assert_eq!(
                redundancy_sample_count,
                redundancy_ratios.len() + no_alternative,
                "seed {seed}: every non-adjacent sampled pair must land in \
                 exactly one of redundancy_ratios or no_alternative"
            );
            redundancy_ratios.sort_by(f64::total_cmp);
            let median_redundancy = if redundancy_ratios.is_empty() {
                f64::NAN
            } else {
                median(&redundancy_ratios)
            };
            let max_redundancy = redundancy_ratios.last().copied().unwrap_or(f64::NAN);
            let reroute_frac = if path_sample_count == 0 {
                f64::NAN
            } else {
                rerouted as f64 / path_sample_count as f64
            };

            // Per-seed progress: The Mire lost an hour of wall-clock to a run
            // with no visible progress and nothing recoverable.
            println!(
                "PILOT-SETTLEMENT seed={seed} settlements={n} pairs={} median_swing={median_swing:.6} max_swing={max_swing:.6} median_markup={median_markup:.6} max_markup={max_markup:.6} path_sample={path_sample_count} reroute_frac={reroute_frac:.6} adjacent_pairs={adjacent_pairs} redundancy_sample={redundancy_sample_count} median_redundancy={median_redundancy:.6} max_redundancy={max_redundancy:.6} no_alt_count={no_alternative}",
                swings.len()
            );
        }
    }
}

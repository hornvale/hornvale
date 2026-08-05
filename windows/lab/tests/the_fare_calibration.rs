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

/// A percentile of an ALREADY-ASCENDING-SORTED (`total_cmp`) slice, using
/// the NEAREST-RANK convention (never interpolated):
/// `rank = ceil(p / 100 * n)`, clamped to `[1, n]`, 1-indexed then converted
/// to a 0-index. Exploratory-only (E1/E2/E3, spec §6b's post-hoc addendum) —
/// distinct from, and never used by, the frozen [`median`] helper F1-F4 use
/// (which averages the two middle values on an even-length sample), so this
/// function's own `p=50.0` can differ slightly from `median()`'s result on
/// an even-length population. `p=100.0` returns the same element `.last()`
/// on a sorted slice would.
fn percentile(sorted_ascending: &[f64], p: f64) -> f64 {
    assert!(
        !sorted_ascending.is_empty(),
        "percentile of an empty population"
    );
    let n = sorted_ascending.len();
    let rank = ((p / 100.0) * n as f64).ceil() as usize;
    let idx = rank.clamp(1, n) - 1;
    sorted_ascending[idx]
}

/// The stride F2/the redundancy control walk the reachable-pair list at: at
/// most every pair, but no finer than needed to land roughly
/// [`PATH_SAMPLE_STRIDE_TARGET`] samples across the whole reachable-pair
/// population. Same shape as `the_mire_calibration.rs`'s `h3_stride`.
fn path_sample_stride(pair_count: usize) -> usize {
    (pair_count / PATH_SAMPLE_STRIDE_TARGET).max(1)
}

// ============================================================================
// Task 5: the preregistered readout at full population (spec §6b, THE
// FREEZE, commit a6e28e5e — frozen before this run, and the sole authority
// for every number below).
// ============================================================================

/// The population Task 5's preregistered readout runs over (spec §6b):
/// matches The Mire's 200 seeds so F3's latitude-ordering comparison is
/// like-for-like.
const PREREGISTERED_SEEDS: std::ops::RangeInclusive<u64> = 1..=200;

/// `|latitude|` band edges in degrees: equatorial, temperate, polar.
/// Redeclared VERBATIM from `the_mire_calibration.rs:151` — test crates do
/// not share constants, per spec §6b's explicit F3 instruction.
const LAT_BANDS: [(f64, f64); 3] = [(0.0, 30.0), (30.0, 60.0), (60.0, 90.0)];

/// F1's frozen floor (spec §6b): pooled median seasonal cost swing at the
/// 40-degree separation band (index [`HEADLINE_BAND_IDX`] into
/// [`SEPARATION_BANDS_DEG`]). AUTHORED, anchored to §5a's own doubling
/// scale — NOT to The Mire's passability metric, an earlier working note's
/// withdrawn comparison ("they share a decimal point and nothing else").
/// **The pilot measured 0.30–0.67% pooled across bands, so this floor is
/// EXPECTED TO FAIL, clearly rather than marginally — recorded in the spec
/// before this run precisely so the failure cannot look like a floor chosen
/// to be cleared. Do not raise, lower, or retune this constant after seeing
/// the 200-seed result; a falsified floor is this campaign's finding.**
const F1_FLOOR_AT_40_DEG: f64 = 0.05;

/// F1's FALSIFIED pooled median seasonal cost swing at the 40-degree band,
/// pinned as a WITNESS (never a claim) after [`F1_FLOOR_AT_40_DEG`]'s
/// preregistered assertion failed exactly as spec §6b predicted. This is
/// NOT a new floor to clear — it is the measured value itself, frozen so a
/// FUTURE change that moves it reddens this test, rather than the test
/// staying green forever on a hypothesis everyone already knows failed.
///
/// Measured post-merge (commit `c2707a36`, absorbing The Keeping's step B —
/// `feat(demography)!: decompose CarryingInput.habitable into is_land` —
/// and 0103 step A1) at the full 200-seed population, and BYTE-IDENTICAL
/// (all six printed decimals) to the pre-merge measurement recorded in
/// `.superpowers/sdd/2026-08-04-the-fare/readout.md`. This confirms step
/// B's settlement-placement change reaches the settlement-frame secondary
/// (which moved) but not the geographic frame F1 reads (which did not).
const F1_PINNED_MEDIAN_SWING_AT_40_DEG: f64 = 0.003698;

/// The relative tolerance around [`F1_PINNED_MEDIAN_SWING_AT_40_DEG`] the
/// pin allows before reddening.
///
/// Chosen at 15%, reasoned from a measurement actually taken rather than a
/// round number picked in the abstract: the SAME post-merge run that
/// re-confirmed F1's geographic-frame value byte-identical also re-measured
/// the settlement-frame secondary, which moved 1.48% (`median_swing`) and
/// 2.09% (`max_swing`) across a merge that changed settlement placement but
/// touched nothing this pin reads. 15% is roughly 7-10x that observed
/// sibling-statistic drift — wide enough to absorb ordinary incidental
/// change elsewhere in the pipeline (a settlement-placement tweak, a minor
/// unrelated refactor's floating-point reordering) without false-alarming,
/// while still tight enough that a REAL shift to the weather-cost mechanism
/// would very likely clear it: F1's own preregistered floor sat at 0.05,
/// more than 13x the pinned value, so any change large enough to threaten
/// that floor's territory would blow through a 15% band by a wide margin
/// long before reaching it.
const F1_PIN_TOLERANCE_FRAC: f64 = 0.15;

/// F2's frozen floor (spec §6b): pooled re-routing fraction at the
/// 40-degree separation band. Pilot: 16.27% pooled, well above this floor.
const F2_FLOOR_AT_40_DEG: f64 = 0.10;

/// F2's redundancy band (spec §6b): a pair counts as having an alternative
/// when its second-best substantially-disjoint dry path costs at most this
/// multiple of the best. **Reporting-only in this test** — the frozen text's
/// only concrete instruction about F2's own denominator is that adjacent
/// pairs stay in it ("retained in F2's denominator, per §4a"); nothing in
/// §6b says a pair beyond this band is removed from F2's denominator too, so
/// this test does not filter F2 by it. It instead adds one new diagnostic
/// count, `beyond_redundancy_band` (pairs with a finite ratio > 2.0, on top
/// of the already-existing `no_alt_count` for pairs with NO alternative at
/// all), so the "2.0× admits the ordinary case" framing in §6b is visible in
/// the readout even though it gates nothing. Flagged in the task report as a
/// judgment call given the ambiguity, so it can be corrected if wrong.
const REDUNDANCY_BAND: f64 = 2.0;

/// Index into [`SEPARATION_BANDS_DEG`] of the frozen headline band (40
/// degrees) F1 and F2's floors are measured at.
const HEADLINE_BAND_IDX: usize = 3;

/// How many of [`SEPARATION_BANDS_DEG`]'s leading entries feed F1, F2, F3,
/// and F-mono. The 80-degree band (index 4) is measured and reported
/// exactly like every other band but excluded from every hypothesis per
/// spec §6b: three of five pilot seeds yielded 0, 2, and 7 pairs there.
const HYPOTHESIS_BAND_COUNT: usize = 4;

/// F3's latitude-band index for `lat_abs` (already `.abs()`d by the
/// caller), using [`LAT_BANDS`]' boundaries with the LAST band inclusive of
/// its upper bound (90°) — same idiom `the_mire_calibration.rs`'s own
/// `land_by_band` construction uses (`lat >= lo && (lat < hi ||
/// is_last_band)`), so a cell at exactly the pole is not dropped by every
/// band's exclusive-upper-bound test.
fn lat_band_index(lat_abs: f64) -> usize {
    for (idx, &(lo, hi)) in LAT_BANDS.iter().enumerate() {
        let is_last_band = idx == LAT_BANDS.len() - 1;
        if lat_abs >= lo && (lat_abs < hi || is_last_band) {
            return idx;
        }
    }
    unreachable!("latitude {lat_abs} (already abs) outside every LAT_BANDS bucket")
}

/// One seed's full preregistered readout: every quantity
/// `the_fares_preregistered_readout` needs, computed exactly once per world
/// (same caching discipline as `the_fares_pilot` and `the_mire_calibration.rs`).
struct FullSeedReadout {
    /// Per [`SEPARATION_BANDS_DEG`] index: this seed's median weathered-cost
    /// swing among reachable geographic pairs at that band (F1's per-seed
    /// value). `f64::NAN` if this seed had no reachable pair at that band
    /// (mirrors the pilot's fallback; `seed=5 band=80°` in the pilot hit
    /// this exactly).
    geo_median_swing: [f64; 5],
    /// Per band: this seed's re-routing fraction among reachable geographic
    /// pairs at that band (F2's per-seed value). Adjacent pairs stay in the
    /// denominator (see the comment at the F2 call site in
    /// `build_full_readout` for why).
    geo_reroute_frac: [f64; 5],
    /// Per band: how many reachable geographic pairs this seed contributed
    /// at that band — diagnostic, matches the pilot's `geo_pairs`.
    geo_pairs: [usize; 5],
    /// Per band: this seed's median redundancy ratio (adjacent pairs
    /// excluded, per fix round 3 / spec §6b).
    geo_median_redundancy: [f64; 5],
    /// Per band: this seed's max redundancy ratio.
    geo_max_redundancy: [f64; 5],
    /// Per band: how many of this band's reachable pairs were directly
    /// adjacent (excluded from the redundancy statistic, retained in F2's
    /// denominator).
    geo_adjacent_pairs: [usize; 5],
    /// Per band: the surviving redundancy-tested population
    /// (`geo_pairs - geo_adjacent_pairs`).
    geo_redundancy_sample: [usize; 5],
    /// Per band: pairs with NO alternative at all after blocking the best
    /// path.
    geo_no_alt_count: [usize; 5],
    /// Per band: among the redundancy-tested (non-adjacent) pairs, how many
    /// have a finite ratio exceeding [`REDUNDANCY_BAND`] (2.0×) — reporting
    /// only, per that constant's doc comment; does not affect F2.
    geo_beyond_redundancy_band: [usize; 5],
    /// F3: this seed's median swing, POOLED ACROSS THE FOUR INCLUDED
    /// SEPARATION BANDS (`SEPARATION_BANDS_DEG[..HYPOTHESIS_BAND_COUNT]`,
    /// i.e. 5°/10°/20°/40° — the 80° band is excluded here too, per §6b),
    /// split by [`LAT_BANDS`] on the swing's SOURCE landmark cell's
    /// `|latitude|`. `None` if this seed contributed no reachable pair to
    /// that latitude band across all four included separation bands.
    /// Bucketing on the SOURCE cell (rather than the destination, or some
    /// midpoint) is a judgment call the task report names explicitly: F3's
    /// instruction says "partitioned on `geo.coord(c).latitude.abs()`"
    /// without specifying which cell `c` is for a two-endpoint pair.
    f3_band_swings: [Option<f64>; 3],
    /// Secondary settlement-pair frame (§4a, §6a): this seed's median swing.
    settlement_median_swing: f64,
    /// Secondary frame: this seed's max swing.
    settlement_max_swing: f64,
    /// Secondary frame: this seed's re-routing fraction.
    settlement_reroute_frac: f64,
    /// Secondary frame: this seed's median redundancy ratio (adjacent pairs
    /// excluded, same rule as the geographic frame).
    settlement_median_redundancy: f64,
    /// Secondary frame: this seed's max redundancy ratio.
    settlement_max_redundancy: f64,
    /// Secondary frame: how many of this seed's sampled settlement pairs
    /// were directly adjacent (excluded from the redundancy statistic).
    settlement_adjacent_pairs: usize,
    /// Secondary frame: the surviving redundancy-tested population
    /// (`path_sample - adjacent_pairs`).
    settlement_redundancy_sample: usize,
    /// Secondary frame: pairs with NO alternative at all after blocking the
    /// best path (a stronger condition than `beyond_redundancy_band`).
    settlement_no_alt_count: usize,
    /// Secondary frame: the realised stride-sampled pair count F2/the
    /// redundancy control ran over.
    settlement_path_sample: usize,
    /// Secondary frame: this seed's settlement count.
    settlement_settlements: usize,
    /// Secondary frame: this seed's exhaustive reachable-pair count (F1's
    /// own population, before the pass-2 stride).
    settlement_pairs: usize,
}

/// Builds one seed's [`FullSeedReadout`]. Mirrors `the_fares_pilot`'s
/// per-seed body (which is left untouched per the task instruction — this
/// is a deliberate, disclosed duplication rather than a refactor of an
/// already-committed, already-reported test) but aggregates into a struct
/// instead of printing, and additionally buckets F1's swings by latitude
/// for F3.
fn build_full_readout(seed: u64, wc: &WorldComponents) -> FullSeedReadout {
    let sample = build_sample(seed, wc);

    // ---------- Geographic frame (primary) ----------
    let land_cells: Vec<CellId> = sample
        .geo
        .cells()
        .filter(|&c| *sample.dry.get(c) != u64::MAX)
        .collect();
    let geo_stride = geo_landmark_stride(land_cells.len());
    let landmarks: Vec<CellId> = land_cells.iter().step_by(geo_stride).copied().collect();
    let landmark_count = landmarks.len();
    let band_count = SEPARATION_BANDS_DEG.len();

    let landmark_dry_sweeps: Vec<CostSweep> = landmarks
        .iter()
        .map(|&src| least_cost_from(&sample.geo, &sample.dry, src))
        .collect();

    let mut pair_dst: Vec<CellId> = Vec::with_capacity(landmark_count * band_count);
    for i in 0..landmark_count {
        for &target in SEPARATION_BANDS_DEG {
            let (dst, _actual) = nearest_at_separation(&sample.geo, &landmarks, i, target);
            pair_dst.push(dst);
        }
    }

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

    let mut geo_median_swing = [f64::NAN; 5];
    let mut geo_reroute_frac = [f64::NAN; 5];
    let mut geo_pairs = [0usize; 5];
    let mut geo_median_redundancy = [f64::NAN; 5];
    let mut geo_max_redundancy = [f64::NAN; 5];
    let mut geo_adjacent_pairs = [0usize; 5];
    let mut geo_redundancy_sample = [0usize; 5];
    let mut geo_no_alt_count = [0usize; 5];
    let mut geo_beyond_redundancy_band = [0usize; 5];
    let mut f3_lat_swings: [Vec<f64>; 3] = Default::default();

    for (k, (reachable, swings)) in geo_reachable_by_band
        .into_iter()
        .zip(geo_swings_by_band)
        .enumerate()
    {
        geo_pairs[k] = reachable.len();

        // F3: bucket this band's swings by the SOURCE landmark's |latitude|
        // — only for the four bands that feed the hypotheses (§6b excludes
        // 80° from F3 the same as F1/F2/F-mono).
        if k < HYPOTHESIS_BAND_COUNT {
            for (&i, &s) in reachable.iter().zip(swings.iter()) {
                let lat_abs = sample.geo.coord(landmarks[i]).latitude.abs();
                f3_lat_swings[lat_band_index(lat_abs)].push(s);
            }
        }

        if !swings.is_empty() {
            let mut sorted = swings;
            sorted.sort_by(f64::total_cmp);
            geo_median_swing[k] = median(&sorted);
        }

        // F2 + the redundancy control, over every reachable pair in this
        // band (GEO_LANDMARK_STRIDE_TARGET already bounds the population,
        // no further stride needed — matches the pilot exactly).
        let mut rerouted = 0usize;
        let mut adjacent = 0usize;
        let mut redundancy_sample = 0usize;
        let mut redundancy_ratios: Vec<f64> = Vec::new();
        let mut no_alt = 0usize;
        let mut beyond_band = 0usize;
        for &i in &reachable {
            let idx = i * band_count + k;
            let src = landmarks[i];
            let dst = pair_dst[idx];

            // F2: path identity at this pair's own cheapest vs costliest
            // sampled day. Adjacent pairs are DELIBERATELY KEPT in this
            // denominator — same rule the settlement frame below follows,
            // and the pilot (fix round 3) established: an adjacent pair
            // structurally cannot re-route (only one edge exists), so
            // keeping it makes reroute_frac conservative, never inflated.
            // Do not exclude it here to "match" the redundancy control's
            // exclusion, which is unrelated.
            let day_a = geo_argmin_day[idx] as f64 * sample.year_length / SAMPLE_DAYS as f64;
            let day_b = geo_argmax_day[idx] as f64 * sample.year_length / SAMPLE_DAYS as f64;
            let field_a = weathered_cost(&sample, day_a);
            let field_b = weathered_cost(&sample, day_b);
            let path_a = least_cost_from(&sample.geo, &field_a, src).path_to(dst);
            let path_b = least_cost_from(&sample.geo, &field_b, src).path_to(dst);
            assert!(
                path_a.is_some() && path_b.is_some(),
                "seed {seed} geo band {k} landmark {i} lost reachability under weathering \
                 (the keystone's guarantee that weathering never makes a passable cell \
                 impassable should preclude this)"
            );
            if path_a != path_b {
                rerouted += 1;
            }

            // Redundancy control: on the DRY field, the best path's
            // interior blocked, re-swept. Adjacent pairs (empty interior)
            // counted and EXCLUDED, per fix round 3.
            let best_cost = landmark_dry_sweeps[i]
                .cost_to(dst)
                .expect("geo_reachable only holds pairs with a finite dry cost");
            let best_path = landmark_dry_sweeps[i]
                .path_to(dst)
                .expect("a finite dry cost implies a dry path");
            if best_path.len() == 2 {
                adjacent += 1;
                continue;
            }
            redundancy_sample += 1;
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
                    let ratio = second_best as f64 / best_cost as f64;
                    if ratio.total_cmp(&REDUNDANCY_BAND).is_gt() {
                        beyond_band += 1;
                    }
                    redundancy_ratios.push(ratio);
                }
                None => no_alt += 1,
            }
        }
        assert_eq!(
            redundancy_sample,
            redundancy_ratios.len() + no_alt,
            "seed {seed} geo band {k}: every non-adjacent pair must land in exactly one \
             of redundancy_ratios or no_alt"
        );
        geo_reroute_frac[k] = if reachable.is_empty() {
            f64::NAN
        } else {
            rerouted as f64 / reachable.len() as f64
        };
        geo_adjacent_pairs[k] = adjacent;
        geo_redundancy_sample[k] = redundancy_sample;
        geo_no_alt_count[k] = no_alt;
        geo_beyond_redundancy_band[k] = beyond_band;
        if !redundancy_ratios.is_empty() {
            geo_median_redundancy[k] = median(&redundancy_ratios);
            redundancy_ratios.sort_by(f64::total_cmp);
            geo_max_redundancy[k] = redundancy_ratios.last().copied().unwrap_or(f64::NAN);
        }
    }

    let f3_band_swings: [Option<f64>; 3] = std::array::from_fn(|b| {
        if f3_lat_swings[b].is_empty() {
            None
        } else {
            Some(median(&f3_lat_swings[b]))
        }
    });

    // ---------- Settlement-pair frame (secondary; §4a, §6a) ----------
    let n = sample.settlements.len();
    let dry_sweeps: Vec<CostSweep> = sample
        .settlements
        .iter()
        .map(|&src| least_cost_from(&sample.geo, &sample.dry, src))
        .collect();

    let mut settlement_min = vec![f64::INFINITY; n * n];
    let mut settlement_max = vec![f64::NEG_INFINITY; n * n];
    let mut settlement_argmin_day = vec![0usize; n * n];
    let mut settlement_argmax_day = vec![0usize; n * n];
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
                    if c.total_cmp(&settlement_min[idx]).is_lt() {
                        settlement_min[idx] = c;
                        settlement_argmin_day[idx] = day_idx;
                    }
                    if c.total_cmp(&settlement_max[idx]).is_gt() {
                        settlement_max[idx] = c;
                        settlement_argmax_day[idx] = day_idx;
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
            if !settlement_min[idx].is_finite() {
                continue;
            }
            if let Some(a) = dry_sweep.cost_to(dst)
                && a > 0
            {
                swings.push((settlement_max[idx] - settlement_min[idx]) / a as f64);
                reachable_pairs.push((i, j));
            }
        }
    }
    let settlement_pairs = swings.len();
    let settlement_median_swing = if swings.is_empty() {
        f64::NAN
    } else {
        median(&swings)
    };
    swings.sort_by(f64::total_cmp);
    let settlement_max_swing = swings.last().copied().unwrap_or(f64::NAN);

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

        let day_a = settlement_argmin_day[idx] as f64 * sample.year_length / SAMPLE_DAYS as f64;
        let day_b = settlement_argmax_day[idx] as f64 * sample.year_length / SAMPLE_DAYS as f64;
        let field_a = weathered_cost(&sample, day_a);
        let field_b = weathered_cost(&sample, day_b);
        let path_a = least_cost_from(&sample.geo, &field_a, src).path_to(dst);
        let path_b = least_cost_from(&sample.geo, &field_b, src).path_to(dst);
        assert!(
            path_a.is_some() && path_b.is_some(),
            "seed {seed} settlement pair ({i},{j}) lost reachability under weathering"
        );
        if path_a != path_b {
            rerouted += 1;
        }

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
        let blocked: BTreeSet<CellId> = best_path[1..best_path.len() - 1].iter().copied().collect();
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
        "seed {seed} settlement frame: every non-adjacent sampled pair must land in \
         exactly one of redundancy_ratios or no_alternative"
    );
    let settlement_median_redundancy = if redundancy_ratios.is_empty() {
        f64::NAN
    } else {
        median(&redundancy_ratios)
    };
    redundancy_ratios.sort_by(f64::total_cmp);
    let settlement_max_redundancy = redundancy_ratios.last().copied().unwrap_or(f64::NAN);
    let settlement_reroute_frac = if path_sample_count == 0 {
        f64::NAN
    } else {
        rerouted as f64 / path_sample_count as f64
    };

    FullSeedReadout {
        geo_median_swing,
        geo_reroute_frac,
        geo_pairs,
        geo_median_redundancy,
        geo_max_redundancy,
        geo_adjacent_pairs,
        geo_redundancy_sample,
        geo_no_alt_count,
        geo_beyond_redundancy_band,
        f3_band_swings,
        settlement_median_swing,
        settlement_max_swing,
        settlement_reroute_frac,
        settlement_median_redundancy,
        settlement_max_redundancy,
        settlement_adjacent_pairs: adjacent_pairs,
        settlement_redundancy_sample: redundancy_sample_count,
        settlement_no_alt_count: no_alternative,
        settlement_path_sample: path_sample_count,
        settlement_settlements: n,
        settlement_pairs,
    }
}

// ============================================================================
// EXPLORATORY (post-hoc, NOT preregistered) — dispatched after F1's
// preregistered falsification, at the project owner's request. NO floors,
// NO pass/fail assertions. F1/F2/F3/F-mono (above) and spec §6b are
// UNCHANGED and UNTOUCHED by anything below; this section is a fully
// independent, duplicate reconstruction of the same deterministic
// geographic frame `build_full_readout` builds (same seed -> byte-identical
// landmarks/pairs, since `nearest_at_separation`/`geo_landmark_stride` are
// pure functions of the mesh), so E1's population is provably the same one
// F1's pooled median summarizes — without importing or calling
// `build_full_readout` itself, so there is zero risk of this work altering
// F1-F4's own computation.
// ============================================================================

/// One seed's exploratory readout: per band, the RAW (un-aggregated)
/// per-pair populations E1/E2/E3 are computed from.
struct ExploratorySeedReadout {
    /// Per band: this seed's raw RE-PLANNED per-pair seasonal swings — F1's
    /// own quantity, independently re-derived and left un-aggregated (E1's
    /// population).
    e1_swings_by_band: [Vec<f64>; 5],
    /// Per band: this seed's raw COMMITTED-ROUTE (fixed dry-optimal path,
    /// never re-planned) per-pair seasonal swings (E2's population).
    e2_swings_by_band: [Vec<f64>; 5],
    /// Per band: this seed's per-pair worst-surcharge-cell fraction, on
    /// that pair's own committed-route costliest sampled day (E3's
    /// population).
    e3_fracs_by_band: [Vec<f64>; 5],
}

/// Builds one seed's [`ExploratorySeedReadout`]. Reconstructs land cells,
/// landmarks, and per-band pair destinations EXACTLY as `build_full_readout`
/// does (deterministic, so byte-identical for a given seed) but computes
/// E1 (F1 re-derived, raw), E2 (the committed-route/fixed-path swing), and
/// E3 (the worst-cell surcharge fraction) instead of F1-F4's own
/// aggregates. Does not call, import from, or share mutable state with
/// `build_full_readout` — a disclosed duplication, chosen so this
/// exploratory work cannot alter F1-F4's frozen computation even by
/// accident.
fn build_exploratory_readout(seed: u64, wc: &WorldComponents) -> ExploratorySeedReadout {
    let sample = build_sample(seed, wc);

    let land_cells: Vec<CellId> = sample
        .geo
        .cells()
        .filter(|&c| *sample.dry.get(c) != u64::MAX)
        .collect();
    let geo_stride = geo_landmark_stride(land_cells.len());
    let landmarks: Vec<CellId> = land_cells.iter().step_by(geo_stride).copied().collect();
    let landmark_count = landmarks.len();
    let band_count = SEPARATION_BANDS_DEG.len();

    let landmark_dry_sweeps: Vec<CostSweep> = landmarks
        .iter()
        .map(|&src| least_cost_from(&sample.geo, &sample.dry, src))
        .collect();

    let mut pair_dst: Vec<CellId> = Vec::with_capacity(landmark_count * band_count);
    for i in 0..landmark_count {
        for &target in SEPARATION_BANDS_DEG {
            let (dst, _actual) = nearest_at_separation(&sample.geo, &landmarks, i, target);
            pair_dst.push(dst);
        }
    }

    // E2's FIXED path per pair — the dry-optimal path, computed ONCE here
    // and never re-planned across the day loop below. `None` if
    // unreachable on the dry field (matches every other frame's
    // reachability test in this file).
    let pair_path: Vec<Option<Vec<CellId>>> = (0..landmark_count * band_count)
        .map(|idx| {
            let i = idx / band_count;
            landmark_dry_sweeps[i].path_to(pair_dst[idx])
        })
        .collect();
    let pair_dry_cost: Vec<Option<u64>> = (0..landmark_count * band_count)
        .map(|idx| {
            let i = idx / band_count;
            landmark_dry_sweeps[i].cost_to(pair_dst[idx])
        })
        .collect();

    // All SAMPLE_DAYS weathered fields, retained rather than rebuilt per
    // pair or per E3 lookup — SAMPLE_DAYS (12) x cell_count u64s is a few
    // MB, trivial, and this is what makes E3's "look up an arbitrary day's
    // field for the worst-cell surcharge" cheap instead of a rebuild storm.
    let weathered_fields: Vec<CellMap<u64>> = (0..SAMPLE_DAYS)
        .map(|day_idx| {
            let day = day_idx as f64 * sample.year_length / SAMPLE_DAYS as f64;
            weathered_cost(&sample, day)
        })
        .collect();

    let mut e1_min = vec![f64::INFINITY; landmark_count * band_count];
    let mut e1_max = vec![f64::NEG_INFINITY; landmark_count * band_count];
    let mut e2_min = vec![f64::INFINITY; landmark_count * band_count];
    let mut e2_max = vec![f64::NEG_INFINITY; landmark_count * band_count];
    let mut e2_argmax_day = vec![0usize; landmark_count * band_count];

    for (day_idx, wet) in weathered_fields.iter().enumerate() {
        // E1: F1's own quantity, independently re-derived — a re-planned
        // least-cost sweep from each landmark against this day's field,
        // exactly what `build_full_readout`'s pass 1 does, just left
        // un-aggregated (per-pair, not reduced to a per-seed median) so E1
        // can report the population's tail.
        for (i, &src) in landmarks.iter().enumerate() {
            let d_wet = least_cost_from(&sample.geo, wet, src);
            for k in 0..band_count {
                let idx = i * band_count + k;
                if let Some(c) = d_wet.cost_to(pair_dst[idx]) {
                    let c = c as f64;
                    if c.total_cmp(&e1_min[idx]).is_lt() {
                        e1_min[idx] = c;
                    }
                    if c.total_cmp(&e1_max[idx]).is_gt() {
                        e1_max[idx] = c;
                    }
                }
            }
        }
        // E2: the FIXED dry-optimal path's cost under THIS day's weathered
        // field — NO re-planning, no sweep, just a sum along the already-
        // known path, excluding the source cell, exactly as `least_cost`
        // totals a path (`route.rs`'s `for cell in actions { total +=
        // cost.get(cell) }`, which also excludes the start). Cheaper than
        // E1/F1: O(path length) per pair per day, not a full Dijkstra sweep.
        for (idx, path) in pair_path.iter().enumerate() {
            if let Some(path) = path {
                let total: u64 = path[1..]
                    .iter()
                    .fold(0u64, |acc, &cell| acc.saturating_add(*wet.get(cell)));
                let c = total as f64;
                if c.total_cmp(&e2_min[idx]).is_lt() {
                    e2_min[idx] = c;
                }
                if c.total_cmp(&e2_max[idx]).is_gt() {
                    e2_max[idx] = c;
                    e2_argmax_day[idx] = day_idx;
                }
            }
        }
    }

    let mut readout = ExploratorySeedReadout {
        e1_swings_by_band: Default::default(),
        e2_swings_by_band: Default::default(),
        e3_fracs_by_band: Default::default(),
    };

    for i in 0..landmark_count {
        for k in 0..band_count {
            let idx = i * band_count + k;
            let Some(dry_cost) = pair_dry_cost[idx] else {
                continue;
            };
            if dry_cost == 0 || !e1_min[idx].is_finite() {
                continue;
            }
            readout.e1_swings_by_band[k].push((e1_max[idx] - e1_min[idx]) / dry_cost as f64);
            readout.e2_swings_by_band[k].push((e2_max[idx] - e2_min[idx]) / dry_cost as f64);

            // E3: on the committed route's own costliest sampled day (E2's
            // argmax for THIS pair — the day the fixed path was priciest
            // under weather, continuing E2's "committed route" framing
            // rather than F1's re-planned argmax day), the path cell with
            // the largest weather surcharge, reported as a fraction of that
            // cell's OWN dry cost. Well-defined for every reachable pair:
            // §5a's WEATHER_FACTOR_FLOOR guarantees no cell's weathered cost
            // is u64::MAX, so the subtraction below never underflows a real
            // (non-defensive) case.
            let path = pair_path[idx]
                .as_ref()
                .expect("a finite dry cost implies a dry path");
            let costliest_day = &weathered_fields[e2_argmax_day[idx]];
            let mut worst_frac = f64::NEG_INFINITY;
            for &cell in &path[1..] {
                let dry_here = *sample.dry.get(cell);
                if dry_here == 0 {
                    continue; // defensive only: BASE_COST=10 floors every land cell above 0
                }
                let surcharge = (*costliest_day.get(cell)).saturating_sub(dry_here);
                let frac = surcharge as f64 / dry_here as f64;
                if frac.total_cmp(&worst_frac).is_gt() {
                    worst_frac = frac;
                }
            }
            if worst_frac.is_finite() {
                readout.e3_fracs_by_band[k].push(worst_frac);
            }
        }
    }

    readout
}

mod weathering {
    use super::*;

    #[test]
    #[ignore = "heavy: live-worldgen battery (minutes); deferred from the commit gate to make gate-full"]
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
    #[ignore = "heavy: live-worldgen battery (minutes); deferred from the commit gate to make gate-full"]
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

    #[test]
    #[ignore = "heavy: live-worldgen battery (minutes); deferred from the commit gate to make gate-full"]
    fn the_fares_preregistered_readout() {
        // THE FREEZE (spec §6b, commit a6e28e5e — frozen before this run,
        // the sole authority for every floor and boundary below). Four
        // hypotheses, all at the geographic (primary) frame unless noted:
        //
        // F1 (§6b): pooled median seasonal cost swing at the 40° band
        //           (SEPARATION_BANDS_DEG[HEADLINE_BAND_IDX]) >= 0.05.
        //           EXPECTED TO FAIL -- the pilot measured 0.30-0.67%
        //           pooled across bands, and that expectation is frozen in
        //           the spec BEFORE this run so the failure cannot look
        //           like a floor chosen to be cleared. Do not weaken this
        //           floor, do not invert the assertion, do not #[ignore]
        //           the failing case, and do not "fix" it by touching
        //           WEATHER_FACTOR_FLOOR, the surcharge formula,
        //           MUD_PENALTY, SNOW_PENALTY, or either substrate default.
        //           A falsified preregistered hypothesis is this
        //           campaign's finding, not a bug to chase.
        // F2 (§6b): pooled re-routing fraction at the 40° band >= 0.10.
        //           Pilot: 16.27% pooled -- expected to pass comfortably.
        // F3 (§6b, unchanged from the original spec): the latitude
        //           ordering (equatorial > temperate > polar), on
        //           LAT_BANDS's boundaries (redeclared verbatim from
        //           the_mire_calibration.rs:151), pooled across the four
        //           included separation bands, compared against The
        //           Mire's measured equatorial 0.0224 > temperate 0.0021 >
        //           polar 0.0000. No numeric floor -- the ORDERING is the
        //           claim.
        // F-mono (§6b, added at freeze time, labelled PILOT-SUGGESTED not
        //           pre-held): pooled F1 and pooled F2 are each
        //           non-decreasing across the four included bands
        //           (5/10/20/40 degrees). POOLED ONLY -- §6b records that
        //           per-seed monotonicity is already known to be
        //           imperfect (F1 was strictly monotonic in only 2 of 5
        //           pilot seeds, and dropped ~22% at the top band in seed
        //           4), so this is not asserted per seed.
        //
        // The 80° band (index 4) is measured and reported exactly like
        // every other band but excluded from all four hypotheses (§6b:
        // three of five pilot seeds yielded 0, 2, and 7 pairs there).
        //
        // "Pooled" follows the_mire_calibration.rs's own H1 convention:
        // one statistic per seed, then the MEDIAN of those 200 values --
        // never a raw pool of every underlying pair across every seed, and
        // never an unweighted mean (which is what this campaign's earlier
        // 5-seed pilot report used informally; the frozen hypotheses use
        // the established median-of-per-seed-statistics convention
        // instead, for consistency with The Mire's own H1/H2).
        let wc = WorldComponents::assemble().expect("canonical registries are well-formed");
        let mut readouts: Vec<FullSeedReadout> = Vec::with_capacity(200);
        for seed in PREREGISTERED_SEEDS {
            readouts.push(build_full_readout(seed, &wc));
            // Per-seed progress: The Mire (and this campaign's own pilot
            // rounds) lost real wall-clock to runs with no visible progress
            // and nothing recoverable. Mandatory per spec §7 / the task
            // brief.
            eprintln!("PROGRESS seed={seed}/200 done");
        }

        let band_count = SEPARATION_BANDS_DEG.len();

        // Pooled F1/F2, per band: median across seeds of each seed's
        // per-band statistic, skipping seeds that had no reachable pair at
        // that band (NaN).
        let pooled_f1: Vec<f64> = (0..band_count)
            .map(|k| {
                let vals: Vec<f64> = readouts
                    .iter()
                    .map(|r| r.geo_median_swing[k])
                    .filter(|v| !v.is_nan())
                    .collect();
                if vals.is_empty() {
                    f64::NAN
                } else {
                    median(&vals)
                }
            })
            .collect();
        let pooled_f2: Vec<f64> = (0..band_count)
            .map(|k| {
                let vals: Vec<f64> = readouts
                    .iter()
                    .map(|r| r.geo_reroute_frac[k])
                    .filter(|v| !v.is_nan())
                    .collect();
                if vals.is_empty() {
                    f64::NAN
                } else {
                    median(&vals)
                }
            })
            .collect();
        let total_geo_pairs: Vec<usize> = (0..band_count)
            .map(|k| readouts.iter().map(|r| r.geo_pairs[k]).sum())
            .collect();

        // Redundancy diagnostics, per band: pooled ratio (median across
        // seeds of each seed's median), totals for the count fields.
        let pooled_redundancy: Vec<f64> = (0..band_count)
            .map(|k| {
                let vals: Vec<f64> = readouts
                    .iter()
                    .map(|r| r.geo_median_redundancy[k])
                    .filter(|v| !v.is_nan())
                    .collect();
                if vals.is_empty() {
                    f64::NAN
                } else {
                    median(&vals)
                }
            })
            .collect();
        let pooled_max_redundancy: Vec<f64> = (0..band_count)
            .map(|k| {
                let vals: Vec<f64> = readouts
                    .iter()
                    .map(|r| r.geo_max_redundancy[k])
                    .filter(|v| !v.is_nan())
                    .collect();
                if vals.is_empty() {
                    f64::NAN
                } else {
                    median(&vals)
                }
            })
            .collect();
        let total_adjacent: Vec<usize> = (0..band_count)
            .map(|k| readouts.iter().map(|r| r.geo_adjacent_pairs[k]).sum())
            .collect();
        let total_redundancy_sample: Vec<usize> = (0..band_count)
            .map(|k| readouts.iter().map(|r| r.geo_redundancy_sample[k]).sum())
            .collect();
        let total_no_alt: Vec<usize> = (0..band_count)
            .map(|k| readouts.iter().map(|r| r.geo_no_alt_count[k]).sum())
            .collect();
        let total_beyond_band: Vec<usize> = (0..band_count)
            .map(|k| {
                readouts
                    .iter()
                    .map(|r| r.geo_beyond_redundancy_band[k])
                    .sum()
            })
            .collect();

        // F3: pooled median swing per latitude band (median across seeds
        // of each seed's per-latitude-band median, pooled over the four
        // included separation bands within each seed already).
        let f3_pooled: [f64; 3] = std::array::from_fn(|b| {
            let vals: Vec<f64> = readouts
                .iter()
                .filter_map(|r| r.f3_band_swings[b])
                .collect();
            if vals.is_empty() {
                f64::NAN
            } else {
                median(&vals)
            }
        });

        // Secondary settlement-pair frame: pooled median swing/reroute,
        // reported alongside (never a hypothesis).
        let settlement_swings: Vec<f64> = readouts
            .iter()
            .map(|r| r.settlement_median_swing)
            .filter(|v| !v.is_nan())
            .collect();
        let pooled_settlement_swing = if settlement_swings.is_empty() {
            f64::NAN
        } else {
            median(&settlement_swings)
        };
        let settlement_max_swings: Vec<f64> = readouts
            .iter()
            .map(|r| r.settlement_max_swing)
            .filter(|v| !v.is_nan())
            .collect();
        let pooled_settlement_max_swing = if settlement_max_swings.is_empty() {
            f64::NAN
        } else {
            median(&settlement_max_swings)
        };
        let settlement_reroutes: Vec<f64> = readouts
            .iter()
            .map(|r| r.settlement_reroute_frac)
            .filter(|v| !v.is_nan())
            .collect();
        let pooled_settlement_reroute = if settlement_reroutes.is_empty() {
            f64::NAN
        } else {
            median(&settlement_reroutes)
        };
        let settlement_median_redundancies: Vec<f64> = readouts
            .iter()
            .map(|r| r.settlement_median_redundancy)
            .filter(|v| !v.is_nan())
            .collect();
        let pooled_settlement_median_redundancy = if settlement_median_redundancies.is_empty() {
            f64::NAN
        } else {
            median(&settlement_median_redundancies)
        };
        let settlement_max_redundancies: Vec<f64> = readouts
            .iter()
            .map(|r| r.settlement_max_redundancy)
            .filter(|v| !v.is_nan())
            .collect();
        let pooled_settlement_max_redundancy = if settlement_max_redundancies.is_empty() {
            f64::NAN
        } else {
            median(&settlement_max_redundancies)
        };
        let total_settlements: usize = readouts.iter().map(|r| r.settlement_settlements).sum();
        let total_settlement_pairs: usize = readouts.iter().map(|r| r.settlement_pairs).sum();
        let total_settlement_adjacent: usize =
            readouts.iter().map(|r| r.settlement_adjacent_pairs).sum();
        let total_settlement_redundancy_sample: usize = readouts
            .iter()
            .map(|r| r.settlement_redundancy_sample)
            .sum();
        let total_settlement_no_alt: usize =
            readouts.iter().map(|r| r.settlement_no_alt_count).sum();
        let total_settlement_path_sample: usize =
            readouts.iter().map(|r| r.settlement_path_sample).sum();

        // ---------------- The readout block ----------------
        eprintln!("=== The Fare: preregistered readout (200 seeds) ===");
        eprintln!(
            "F1 floor = {F1_FLOOR_AT_40_DEG} at band_deg={:.1} (spec §6b, EXPECTED TO FAIL)",
            SEPARATION_BANDS_DEG[HEADLINE_BAND_IDX]
        );
        eprintln!(
            "F2 floor = {F2_FLOOR_AT_40_DEG} at band_deg={:.1} (spec §6b)",
            SEPARATION_BANDS_DEG[HEADLINE_BAND_IDX]
        );
        eprintln!("redundancy band = {REDUNDANCY_BAND}x (spec §6b, reporting-only in this test)");
        for k in 0..SEPARATION_BANDS_DEG.len() {
            let hypothesis_note = if k < HYPOTHESIS_BAND_COUNT {
                ""
            } else {
                " (excluded from all hypotheses, §6b)"
            };
            eprintln!(
                "band_deg={:.1}{hypothesis_note}: pooled_F1(median_swing)={:.6} \
                 pooled_F2(reroute_frac)={:.6} total_geo_pairs={} \
                 pooled_median_redundancy={:.6} pooled_max_redundancy={:.6} \
                 total_adjacent={} total_redundancy_sample={} total_no_alt={} \
                 total_beyond_{REDUNDANCY_BAND}x={}",
                SEPARATION_BANDS_DEG[k],
                pooled_f1[k],
                pooled_f2[k],
                total_geo_pairs[k],
                pooled_redundancy[k],
                pooled_max_redundancy[k],
                total_adjacent[k],
                total_redundancy_sample[k],
                total_no_alt[k],
                total_beyond_band[k],
            );
        }
        eprintln!(
            "F3 (pooled across 5/10/20/40 deg bands): equatorial={:.6} temperate={:.6} \
             polar={:.6} (The Mire: equatorial 0.0224 > temperate 0.0021 > polar 0.0000)",
            f3_pooled[0], f3_pooled[1], f3_pooled[2]
        );
        eprintln!(
            "F-mono: pooled_F1 by band = {:?}",
            &pooled_f1[..HYPOTHESIS_BAND_COUNT]
        );
        eprintln!(
            "F-mono: pooled_F2 by band = {:?}",
            &pooled_f2[..HYPOTHESIS_BAND_COUNT]
        );
        eprintln!(
            "PILOT-SETTLEMENT (secondary, {total_settlements} settlements pooled, \
             {total_settlement_pairs} pairs pooled, {total_settlement_path_sample} \
             path-sampled): pooled_median_swing={pooled_settlement_swing:.6} \
             pooled_max_swing={pooled_settlement_max_swing:.6} \
             pooled_reroute_frac={pooled_settlement_reroute:.6} \
             pooled_median_redundancy={pooled_settlement_median_redundancy:.6} \
             pooled_max_redundancy={pooled_settlement_max_redundancy:.6} \
             total_adjacent={total_settlement_adjacent} \
             total_redundancy_sample={total_settlement_redundancy_sample} \
             total_no_alt={total_settlement_no_alt}"
        );

        // ---------------- F1: PINNED AS A FALSIFIED WITNESS ----------------
        // F1's original preregistered claim was `>= F1_FLOOR_AT_40_DEG` (0.05,
        // spec §6b). It failed as predicted -- the pilot measured 0.30-0.67%
        // pooled across bands, an order of magnitude below a floor anchored to
        // §5a's own doubling scale -- and that failure is this campaign's
        // finding, not a bug. Post-falsification, at the project owner's
        // direction, this assertion is converted from a CLAIM (a floor that
        // must be cleared) to a WITNESS (a pin on the measured falsification,
        // so the test reddens only if the number MOVES, never because the
        // already-falsified hypothesis stays falsified). See
        // F1_PINNED_MEDIAN_SWING_AT_40_DEG's doc comment for the pinned value
        // and F1_PIN_TOLERANCE_FRAC's for the tolerance and its reasoning.
        let f1_measured = pooled_f1[HEADLINE_BAND_IDX];
        let f1_tolerance = F1_PINNED_MEDIAN_SWING_AT_40_DEG * F1_PIN_TOLERANCE_FRAC;
        let f1_tolerance_pct = F1_PIN_TOLERANCE_FRAC * 100.0;
        assert!(
            (f1_measured - F1_PINNED_MEDIAN_SWING_AT_40_DEG).abs() <= f1_tolerance,
            "F1 PIN (not a floor): pooled median seasonal cost swing at the \
             40-degree band was {f1_measured:.6}, outside the pinned witness \
             {F1_PINNED_MEDIAN_SWING_AT_40_DEG} +/- {f1_tolerance:.6} \
             ({f1_tolerance_pct:.0}%). THIS PINS A FALSIFIED HYPOTHESIS AS A \
             WITNESS, NOT A CLAIM: F1's original preregistered floor (>= \
             {F1_FLOOR_AT_40_DEG}, spec §6b) failed as predicted before this pin \
             existed -- weather's cost effect on a re-planned route, even pooled \
             over a full year and 200 worlds, is real but an order of magnitude \
             below a floor anchored to §5a's own doubling scale, and THAT FAILURE \
             IS THIS CAMPAIGN'S FINDING. A red assertion here means the MEASURED \
             NUMBER MOVED away from its pinned value -- something upstream \
             changed (terrain, climate, settlement placement, the weather-cost \
             transform, or the sampling frame itself) -- it does NOT mean the \
             falsified hypothesis was rescued, and it must never be read that \
             way. Investigate what moved before touching this pin; do not widen \
             the tolerance to silence a genuine drift, and do not treat a GREEN \
             result here as F1 having passed."
        );

        // ---------------- F2 ----------------
        let f2_measured = pooled_f2[HEADLINE_BAND_IDX];
        assert!(
            f2_measured >= F2_FLOOR_AT_40_DEG,
            "F2 floor: pooled re-routing fraction at the 40-degree band was \
             {f2_measured:.6}, short of the preregistered floor {F2_FLOOR_AT_40_DEG} -- \
             a real finding, not a test bug: weather may not produce enough spatial \
             structure relative to terrain to change which road is cheapest at \
             population scale, even though the 5-seed pilot measured 16.27% pooled."
        );

        // ---------------- F3: the ordering is the claim, no numeric floor ----------------
        let f3_ordering_holds = f3_pooled[0] > f3_pooled[1] && f3_pooled[1] > f3_pooled[2];
        eprintln!(
            "F3 verdict: {} (measured equatorial={:.6} temperate={:.6} polar={:.6})",
            if f3_ordering_holds {
                "HOLDS"
            } else {
                "FALSIFIED"
            },
            f3_pooled[0],
            f3_pooled[1],
            f3_pooled[2]
        );
        assert!(
            f3_ordering_holds,
            "F3 (the latitude ordering): measured equatorial={:.6} temperate={:.6} \
             polar={:.6} does not reproduce The Mire's equatorial > temperate > polar \
             ordering on the cost instrument -- a real finding about whether the \
             polar zero was a property of the world or of The Mire's threshold, not \
             a test bug. §5a's mechanism (a permanently frozen cell has constant \
             conductance, hence constant cost) predicted this ordering SHOULD survive \
             a better instrument; if it does not, that prediction is falsified and \
             belongs in the chronicle as such.",
            f3_pooled[0], f3_pooled[1], f3_pooled[2]
        );

        // ---------------- F-mono: POOLED ONLY, both F1 and F2 ----------------
        for w in pooled_f1[..HYPOTHESIS_BAND_COUNT].windows(2) {
            assert!(
                w[0].total_cmp(&w[1]).is_le(),
                "F-mono (F1 half) falsified: pooled median swing did not rise \
                 monotonically across the included separation bands -- pooled_F1 by \
                 band = {:?}. This was PILOT-SUGGESTED (the 5-seed pilot found F1 \
                 rising x2.21 from 5 to 40 degrees), stated as a pooled claim BECAUSE \
                 per-seed monotonicity was already known to be imperfect (strictly \
                 monotonic in only 2 of 5 pilot seeds). A falsified pooled trend at \
                 200 seeds is a genuine finding about the mechanism, not a bug.",
                &pooled_f1[..HYPOTHESIS_BAND_COUNT]
            );
        }
        for w in pooled_f2[..HYPOTHESIS_BAND_COUNT].windows(2) {
            assert!(
                w[0].total_cmp(&w[1]).is_le(),
                "F-mono (F2 half) falsified: pooled re-routing fraction did not rise \
                 monotonically across the included separation bands -- pooled_F2 by \
                 band = {:?}. Same PILOT-SUGGESTED, pooled-only status as the F1 half \
                 above.",
                &pooled_f2[..HYPOTHESIS_BAND_COUNT]
            );
        }
    }

    #[test]
    #[ignore = "heavy: live-worldgen battery (minutes); deferred from the commit gate to make gate-full"]
    fn the_fares_exploratory_readout() {
        // EXPLORATORY, POST-HOC, NOT PREREGISTERED. Dispatched after F1's
        // preregistered falsification (pooled median swing 0.0037 against
        // the 0.05 floor), at the project owner's request: real weather is
        // catastrophic (the Donner Party), so a median-only readout cannot
        // see a catastrophe, and F1 measures the cost of a PERFECTLY
        // RE-PLANNED route, not a route committed to in advance. Neither
        // objection changes F1/F2/F3/F-mono's frozen verdicts (spec §6b)
        // -- this whole test is additional evidence gathered AFTER
        // unblinding, carries NO floor and NO pass/fail assertion, and must
        // never be retrofitted as a preregistered hypothesis. It does not
        // call or touch `build_full_readout`/the four hypotheses' own code
        // at all (see `build_exploratory_readout`'s doc comment).
        //
        // Three statistics, all over the SAME geographic population F1
        // uses (byte-identical landmarks/pairs for a given seed, since the
        // construction is a pure function of the mesh):
        //
        // E1 -- the tail of F1. Per band (including 80, unlike F1-F4):
        // pooled p50/p90/p99/max of the RAW per-pair re-planned seasonal
        // swing (F1's own quantity, un-aggregated). "Pooled" here means
        // every reachable pair's swing across all 200 seeds, concatenated
        // THEN percentiled -- NOT median-of-per-seed-medians (F1's own
        // convention) -- because the whole point is to see the tail a
        // per-seed-then-cross-seed median would smooth away.
        //
        // E2 -- the committed-route cost (the Donner number). Per band:
        // for each pair, the DRY-OPTIMAL PATH computed ONCE, then that
        // SAME FIXED path's cost evaluated under each sampled day's
        // weathered field (summed over the path's cells, excluding the
        // source) -- no re-planning. Same p50/p90/p99/max tail as E1, plus
        // E2/F1 (the ratio of committed-route to re-planned swing, at each
        // percentile) -- the value of foresight.
        //
        // E3 -- the worst cell on the route. For each pair's fixed path, on
        // that pair's own committed-route costliest sampled day, the cell
        // with the largest weather surcharge, reported as a fraction of
        // THAT CELL's own dry cost. Pooled p50/p90/p99/max, both overall
        // (every band's pairs combined, the number the task literally
        // asked for) and per band (a free bonus from the same data).
        //
        // Percentile convention: NEAREST-RANK (see `percentile`'s doc
        // comment), not interpolated.
        let wc = WorldComponents::assemble().expect("canonical registries are well-formed");
        let band_count = SEPARATION_BANDS_DEG.len();
        let mut e1_pooled: [Vec<f64>; 5] = Default::default();
        let mut e2_pooled: [Vec<f64>; 5] = Default::default();
        let mut e3_pooled: [Vec<f64>; 5] = Default::default();

        for seed in PREREGISTERED_SEEDS {
            let r = build_exploratory_readout(seed, &wc);
            for k in 0..band_count {
                e1_pooled[k].extend(r.e1_swings_by_band[k].iter().copied());
                e2_pooled[k].extend(r.e2_swings_by_band[k].iter().copied());
                e3_pooled[k].extend(r.e3_fracs_by_band[k].iter().copied());
            }
            // Per-seed progress: mandatory for any run expected to exceed a
            // few minutes (spec §7 / the task brief) -- this run is
            // expected to take about as long as the preregistered readout.
            eprintln!("PROGRESS seed={seed}/200 done (exploratory)");
        }

        eprintln!("=== The Fare: EXPLORATORY readout (post-hoc, NOT preregistered; 200 seeds) ===");
        eprintln!(
            "No floors, no pass/fail assertions below. F1/F2/F3/F-mono's frozen verdicts \
             (spec §6b) are unchanged by anything in this test."
        );

        let mut e1_p50_by_band = [f64::NAN; 5];
        let mut e2_p50_by_band = [f64::NAN; 5];

        for k in 0..band_count {
            let mut e1 = e1_pooled[k].clone();
            e1.sort_by(f64::total_cmp);
            let mut e2 = e2_pooled[k].clone();
            e2.sort_by(f64::total_cmp);

            if e1.is_empty() || e2.is_empty() {
                eprintln!(
                    "band_deg={:.1}: no reachable pairs (n_e1={}, n_e2={})",
                    SEPARATION_BANDS_DEG[k],
                    e1.len(),
                    e2.len()
                );
                continue;
            }

            let (e1_p50, e1_p90, e1_p99, e1_max) = (
                percentile(&e1, 50.0),
                percentile(&e1, 90.0),
                percentile(&e1, 99.0),
                percentile(&e1, 100.0),
            );
            let (e2_p50, e2_p90, e2_p99, e2_max) = (
                percentile(&e2, 50.0),
                percentile(&e2, 90.0),
                percentile(&e2, 99.0),
                percentile(&e2, 100.0),
            );
            e1_p50_by_band[k] = e1_p50;
            e2_p50_by_band[k] = e2_p50;

            eprintln!(
                "band_deg={:.1}: E1(re-planned, n={}) p50={e1_p50:.6} p90={e1_p90:.6} \
                 p99={e1_p99:.6} max={e1_max:.6}",
                SEPARATION_BANDS_DEG[k],
                e1.len()
            );
            eprintln!(
                "band_deg={:.1}: E2(committed-route, n={}) p50={e2_p50:.6} p90={e2_p90:.6} \
                 p99={e2_p99:.6} max={e2_max:.6}",
                SEPARATION_BANDS_DEG[k],
                e2.len()
            );

            let ratio_at = |a: f64, b: f64| -> String {
                if b == 0.0 {
                    "undefined (F1 percentile is zero)".to_string()
                } else {
                    format!("{:.6}", a / b)
                }
            };
            eprintln!(
                "band_deg={:.1}: E2/F1 (value of foresight) at p50={} p90={} max={}",
                SEPARATION_BANDS_DEG[k],
                ratio_at(e2_p50, e1_p50),
                ratio_at(e2_p90, e1_p90),
                ratio_at(e2_max, e1_max),
            );
        }

        for k in 0..band_count {
            let mut e3 = e3_pooled[k].clone();
            e3.sort_by(f64::total_cmp);
            if e3.is_empty() {
                eprintln!(
                    "band_deg={:.1}: E3 no reachable pairs",
                    SEPARATION_BANDS_DEG[k]
                );
                continue;
            }
            eprintln!(
                "band_deg={:.1}: E3(worst-cell surcharge fraction, n={}) p50={:.6} p90={:.6} \
                 p99={:.6} max={:.6}",
                SEPARATION_BANDS_DEG[k],
                e3.len(),
                percentile(&e3, 50.0),
                percentile(&e3, 90.0),
                percentile(&e3, 99.0),
                percentile(&e3, 100.0),
            );
        }

        let mut e3_overall: Vec<f64> = e3_pooled.iter().flatten().copied().collect();
        e3_overall.sort_by(f64::total_cmp);
        if e3_overall.is_empty() {
            eprintln!("E3 OVERALL (all bands pooled): no reachable pairs");
        } else {
            eprintln!(
                "E3 OVERALL (all bands pooled, n={}): p50={:.6} p90={:.6} p99={:.6} max={:.6}",
                e3_overall.len(),
                percentile(&e3_overall, 50.0),
                percentile(&e3_overall, 90.0),
                percentile(&e3_overall, 99.0),
                percentile(&e3_overall, 100.0),
            );
        }

        eprintln!("E1 p50 by band = {:?}", &e1_p50_by_band[..band_count]);
        eprintln!("E2 p50 by band = {:?}", &e2_p50_by_band[..band_count]);

        // NO assertions on any measured value above -- exploratory, no
        // floors, no pass/fail. This test's only failure mode is a panic
        // from malformed data (e.g. `percentile` on an empty slice, guarded
        // above by the `is_empty()` checks), never a comparison to a
        // preregistered threshold.
    }
}

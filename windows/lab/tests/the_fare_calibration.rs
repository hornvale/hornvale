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

use hornvale_astronomy::SkyPins;
use hornvale_climate::provider::GeneratedClimate;
use hornvale_climate::snowpack::DEFAULT_SNOWPACK;
use hornvale_climate::substrate::SubstrateField;
use hornvale_climate::wetness::{DEFAULT_WETNESS, receptivity};
use hornvale_kernel::{CellId, CellMap, Geosphere, Seed, Value};
use hornvale_terrain::TerrainPins;
use hornvale_topology::least_cost_from;
use hornvale_worldgen::graph_derive::weather_conductance_factor;
use hornvale_worldgen::{
    BASE_COST, BuildDepth, SettlementPins, SkyChoice, WorldComponents,
    build_world_to_with_artifacts, traversal_cost,
};

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

/// One built world's cached readout surface, computed exactly once per world.
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
        // Measures TWO distinct quantities per seed, both labelled on the
        // PILOT line:
        //
        // - `median_swing`/`max_swing`: the spec's F1 quantity (§4) — each
        //   pair's own (max - min) of weathered path cost ACROSS the
        //   SAMPLE_DAYS sampled days, expressed as a fraction of that pair's
        //   dry cost. This is what Task 4 must set floors from, since it is
        //   what Task 5 measures.
        // - `median_markup`/`max_markup`: a fixed-day dry-vs-weathered
        //   markup at a single day (`year_length / 4.0`), kept as a control
        //   statistic. This is what the first version of this pilot
        //   measured; it answers a structurally different question (a
        //   single day's markup, not a within-year amplitude) and must not
        //   be the thing floors are calibrated from.
        let wc = WorldComponents::assemble().expect("canonical registries are well-formed");
        for seed in PILOT_SEEDS {
            let sample = build_sample(seed, &wc);
            let n = sample.settlements.len();

            // The dry sweep is day-invariant, so compute it once per source
            // and reuse it for both the markup control and the swing.
            let dry_sweeps: Vec<CellMap<Option<u64>>> = sample
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
                    if let (Some(a), Some(b)) = (*dry_sweeps[i].get(dst), *d_wet.get(dst))
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

            // --- F1's quantity: each pair's (max - min) weathered cost
            // across SAMPLE_DAYS, over its dry cost. Weathered field is
            // built once per day (not once per source per day) and every
            // settlement is swept from that one field.
            let mut min_cost = vec![f64::INFINITY; n * n];
            let mut max_cost = vec![f64::NEG_INFINITY; n * n];
            for day_idx in 0..SAMPLE_DAYS {
                let day = day_idx as f64 * sample.year_length / SAMPLE_DAYS as f64;
                let wet = weathered_cost(&sample, day);
                for (i, &src) in sample.settlements.iter().enumerate() {
                    let d_wet = least_cost_from(&sample.geo, &wet, src);
                    for (j, &dst) in sample.settlements.iter().enumerate() {
                        if i == j {
                            continue;
                        }
                        if let Some(c) = *d_wet.get(dst) {
                            let c = c as f64;
                            let idx = i * n + j;
                            if c.total_cmp(&min_cost[idx]).is_lt() {
                                min_cost[idx] = c;
                            }
                            if c.total_cmp(&max_cost[idx]).is_gt() {
                                max_cost[idx] = c;
                            }
                        }
                    }
                }
            }

            let mut swings: Vec<f64> = Vec::new();
            for (i, _) in sample.settlements.iter().enumerate() {
                for (j, &dst) in sample.settlements.iter().enumerate() {
                    if i == j {
                        continue;
                    }
                    let idx = i * n + j;
                    if !min_cost[idx].is_finite() {
                        continue;
                    }
                    if let Some(a) = *dry_sweeps[i].get(dst)
                        && a > 0
                    {
                        swings.push((max_cost[idx] - min_cost[idx]) / a as f64);
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

            // Per-seed progress: The Mire lost an hour of wall-clock to a run
            // with no visible progress and nothing recoverable.
            println!(
                "PILOT seed={seed} settlements={n} pairs={} median_swing={median_swing:.6} max_swing={max_swing:.6} median_markup={median_markup:.6} max_markup={max_markup:.6}",
                swings.len()
            );
        }
    }
}

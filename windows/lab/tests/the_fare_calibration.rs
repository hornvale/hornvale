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

use hornvale_worldgen::BASE_COST;

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

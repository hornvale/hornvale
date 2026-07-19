//! Drawn weather (The Firmament) — a sampled synoptic weather-state field and
//! its cloud face. Weather is a pure function of `(cell, day, seed)`: the
//! climate fields set a per-cell storm *propensity*, and a time-smooth drifting
//! Fbm phase picks the instantaneous state. Nothing integrates forward (the
//! Lorenz guard-rail); no per-day state is stored. Level 0 — an observation
//! read that changes nothing a biome reads.

use crate::streams::WEATHER_PHASE;
use hornvale_kernel::Seed;
use hornvale_kernel::noise::Fbm;

/// The synoptic state of a cell's sky on a day — the rungs of a small state
/// machine, ordered from calm to violent. `Cirrus` is a separate high overlay
/// (see [`cirrus_present`]), not a rung here.
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum WeatherState {
    /// No low cloud; a clear sky.
    Clear = 0,
    /// Scattered fair-weather cumulus.
    Fair = 1,
    /// A stable overcast stratus deck.
    Overcast = 2,
    /// Sustained rain from a nimbostratus deck.
    Rain = 3,
    /// A deep convective storm (cumulonimbus).
    Storm = 4,
}

/// The kind of cloud a sky wears — the pure projection of its [`WeatherState`]
/// (plus the high cirrus overlay). "Realistic clouds" is this typing; there is
/// no separate cloud model.
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum CloudType {
    /// A cloudless sky.
    None = 0,
    /// Puffy fair-weather cumulus.
    Cumulus = 1,
    /// A flat overcast stratus sheet.
    Stratus = 2,
    /// A thick rain-bearing nimbostratus deck.
    Nimbostratus = 3,
    /// A towering convective cumulonimbus.
    Cumulonimbus = 4,
    /// High, thin cirrus (a storm precursor over an otherwise clear sky).
    Cirrus = 5,
}

/// How much drifting-Fbm swing the propensity is modulated by — the fast
/// weather's amplitude over the slow climatological center.
const PHASE_AMPLITUDE: f64 = 0.55;
/// Octaves of the weather Fbm (a little texture, not fine detail).
const WEATHER_OCTAVES: u32 = 3;
/// Degrees of longitude the weather pattern drifts per standard day (weather
/// systems track roughly west-to-east); small so day-to-day is smooth.
const DRIFT_DEG_PER_DAY: f64 = 6.0;
/// Spatial scale (degrees per Fbm unit) — larger cells of weather read as
/// coherent systems rather than per-cell noise.
const SPATIAL_SCALE_DEG: f64 = 40.0;

/// The climatological storm *propensity* of a cell, `[0,1]` — the slow prior the
/// fast phase is centred on. Storm-prone where the air is moist, rising, warm,
/// and near its evaporative source; clear-prone where dry, subsiding, cool, and
/// interior. A pure blend of fields already computed — no draws.
/// type-audit: bare-ok(ratio: moisture), bare-ok(flag: rising), bare-ok(diagnostic-value: mean_temp_c), bare-ok(flag: ocean_adjacent), bare-ok(ratio: return)
pub fn storm_propensity(
    moisture: f64,
    rising: bool,
    mean_temp_c: f64,
    ocean_adjacent: bool,
) -> f64 {
    let m = moisture.clamp(0.0, 1.0);
    // Warmth term: convective vigor rises with temperature, saturating; near-
    // freezing air barely convects. Normalized 0 (<=0 C) .. 1 (>=30 C).
    let warmth = (mean_temp_c / 30.0).clamp(0.0, 1.0);
    let uplift = if rising { 1.0 } else { 0.4 };
    let source = if ocean_adjacent { 1.0 } else { 0.75 };
    // Moisture and warmth are the dominant, multiplicative drivers (you need
    // both water and heat to build a storm); uplift and source modulate.
    (m * warmth * uplift * source).clamp(0.0, 1.0)
}

/// The drifting weather-phase at a place and day, `[-1,1]`. A single static Fbm
/// sampled in a frame that drifts with time: `x = (longitude - day*DRIFT)`, so
/// the pattern appears to move and day-to-day is smooth, while space is smooth
/// because longitude/latitude are continuous. Deterministic in `(seed, lon,
/// lat, day)`; nothing integrates.
/// type-audit: bare-ok(diagnostic-value: longitude_deg), bare-ok(diagnostic-value: latitude_deg), bare-ok(diagnostic-value: day), bare-ok(diagnostic-value: return)
pub fn weather_phase(weather_seed: Seed, longitude_deg: f64, latitude_deg: f64, day: f64) -> f64 {
    let fbm = Fbm::new(weather_seed, WEATHER_OCTAVES);
    let x = (longitude_deg - day * DRIFT_DEG_PER_DAY) / SPATIAL_SCALE_DEG;
    let y = latitude_deg / SPATIAL_SCALE_DEG;
    // Fbm::sample returns [0,1); center it to [-1,1].
    (fbm.sample(x, y) * 2.0 - 1.0).clamp(-1.0, 1.0)
}

/// Classify the sky: combine the slow propensity with the fast phase into an
/// intensity, then threshold into the five rungs. Monotone in both inputs, so
/// the state ladder never skips erratically and — because the phase is smooth in
/// time — day-to-day motion is between adjacent rungs.
/// type-audit: bare-ok(ratio: propensity), bare-ok(diagnostic-value: phase)
pub fn weather_state(propensity: f64, phase: f64) -> WeatherState {
    let intensity = propensity.clamp(0.0, 1.0) + PHASE_AMPLITUDE * phase;
    // Thresholds over the reachable intensity range [-0.55, 1.55].
    if intensity < 0.15 {
        WeatherState::Clear
    } else if intensity < 0.45 {
        WeatherState::Fair
    } else if intensity < 0.75 {
        WeatherState::Overcast
    } else if intensity < 1.05 {
        WeatherState::Rain
    } else {
        WeatherState::Storm
    }
}

/// Whether high, thin cirrus is present — the leading edge of an approaching
/// system: a moderately storm-prone cell whose phase is *rising toward* activity
/// but has not yet built a low deck. A diagnostic overlay, independent of the
/// low-cloud rung.
/// type-audit: bare-ok(ratio: propensity), bare-ok(diagnostic-value: phase), bare-ok(flag: return)
pub fn cirrus_present(propensity: f64, phase: f64) -> bool {
    // Precursor band: some propensity, phase in the "building" range where the
    // state is still Clear/Fair (see weather_state thresholds).
    propensity > 0.25 && (0.0..0.3).contains(&phase)
}

/// The cloud a sky wears — a total projection of its state, with cirrus filling
/// in only over an otherwise cloudless (`Clear`) sky.
/// type-audit: bare-ok(flag: cirrus)
pub fn cloud_type(state: WeatherState, cirrus: bool) -> CloudType {
    match state {
        WeatherState::Clear => {
            if cirrus {
                CloudType::Cirrus
            } else {
                CloudType::None
            }
        }
        WeatherState::Fair => CloudType::Cumulus,
        WeatherState::Overcast => CloudType::Stratus,
        WeatherState::Rain => CloudType::Nimbostratus,
        WeatherState::Storm => CloudType::Cumulonimbus,
    }
}

/// The weather-phase seed derived from a world seed (the save-format label
/// [`WEATHER_PHASE`]). Independent of every other draw — climate is otherwise
/// seed-free.
/// type-audit: bare-ok(constructor-edge: seed)
pub fn weather_seed(seed: Seed) -> Seed {
    seed.derive(WEATHER_PHASE)
}

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_kernel::Seed;

    // Propensity tracks the fields: a wet, rising, warm, ocean-adjacent cell is
    // more storm-prone than a dry, subsiding, cool, interior one. This isolates
    // the mechanism — flip any driver and the ordering breaks.
    #[test]
    fn propensity_tracks_the_fields() {
        let stormy = storm_propensity(0.9, true, 28.0, true);
        let calm = storm_propensity(0.1, false, 2.0, false);
        assert!(
            stormy > calm,
            "wet/rising/warm/coastal must out-propensity dry/subsiding/cool/interior: {stormy} vs {calm}"
        );
        assert!((0.0..=1.0).contains(&stormy) && (0.0..=1.0).contains(&calm));
    }

    // The state ladder is monotone in the combined intensity: raising the phase
    // (holding propensity) can only move the state up the ladder, never skip
    // erratically. Sampling a sweep must be non-decreasing.
    #[test]
    fn state_is_monotone_in_phase() {
        let p = 0.5;
        let mut prev = weather_state(p, -1.0);
        for i in -9..=10 {
            let phase = f64::from(i) / 10.0;
            let s = weather_state(p, phase);
            assert!(
                s >= prev,
                "state must not decrease as phase rises: {prev:?} -> {s:?} at phase {phase}"
            );
            prev = s;
        }
    }

    // Temporal smoothness, measured on the MECHANISM and self-calibrating (no
    // magic threshold): consecutive days are far more alike than distant days.
    // If the phase were discontinuous in time (e.g. a huge drift decorrelating
    // adjacent days), adjacent-day deltas would be as large as far-apart-day
    // deltas and the ratio would approach 1 — this asserts it stays well below.
    // A naive "non-adjacent state jumps are rare" test is VACUOUS here: at a
    // mid propensity the intensity never swings far enough in a day to skip two
    // rungs regardless of smoothness, so it passes even for a decorrelated
    // phase. The delta-ratio measures the actual smoothness the design claims.
    #[test]
    fn weather_is_temporally_smooth() {
        let seed = Seed(42).derive(WEATHER_PHASE_TEST);
        for &(lon, lat) in &[(12.0_f64, 34.0_f64), (100.0, -20.0), (-50.0, 5.0)] {
            let mut adjacent = 0.0_f64;
            let mut far = 0.0_f64;
            let n = 600;
            for d in 0..n {
                let day = f64::from(d);
                let a = weather_phase(seed, lon, lat, day);
                let a_next = weather_phase(seed, lon, lat, day + 1.0);
                // 300 days apart ~ fully decorrelated (weeks-scale systems).
                let a_far = weather_phase(seed, lon, lat, day + 300.0);
                adjacent += (a_next - a).abs();
                far += (a_far - a).abs();
            }
            let ratio = adjacent / far;
            assert!(
                ratio < 0.5,
                "phase not smooth in time at ({lon},{lat}): adjacent-day delta \
                 {adjacent:.3} vs far-day delta {far:.3}, ratio {ratio:.3} \
                 (a discontinuous phase gives ~1.0)"
            );
        }
    }

    // Temporal VARIETY: over a season a cell must actually experience DIFFERENT
    // weather, not sit frozen in one state. Guards the opposite failure of
    // smoothness — a phase amplitude too small (or a dead noise) would leave the
    // sky static, which is smooth but wrong. A mid-propensity cell visits at
    // least three distinct states across a year.
    #[test]
    fn weather_varies_over_a_season() {
        let seed = Seed(42).derive(WEATHER_PHASE_TEST);
        let (lon, lat) = (12.0, 34.0);
        let p = 0.55;
        let mut states = std::collections::BTreeSet::new();
        for d in 0..400 {
            states.insert(weather_state(p, weather_phase(seed, lon, lat, f64::from(d))) as u8);
        }
        assert!(
            states.len() >= 3,
            "weather is too static — only {} distinct states over 400 days: {states:?}",
            states.len()
        );
    }

    // Cloud type is the state's own face — the projection is total and 1:1 on
    // the primary states; cirrus overlays only where there is no low deck.
    #[test]
    fn cloud_type_projects_the_state() {
        assert_eq!(cloud_type(WeatherState::Fair, false), CloudType::Cumulus);
        assert_eq!(
            cloud_type(WeatherState::Overcast, false),
            CloudType::Stratus
        );
        assert_eq!(
            cloud_type(WeatherState::Rain, false),
            CloudType::Nimbostratus
        );
        assert_eq!(
            cloud_type(WeatherState::Storm, false),
            CloudType::Cumulonimbus
        );
        assert_eq!(cloud_type(WeatherState::Clear, false), CloudType::None);
        // Cirrus fills in only over an otherwise cloudless sky.
        assert_eq!(cloud_type(WeatherState::Clear, true), CloudType::Cirrus);
        assert_eq!(
            cloud_type(WeatherState::Storm, true),
            CloudType::Cumulonimbus
        );
    }

    // Determinism: same (seed, place, day) -> same phase, exactly, on rebuild.
    #[test]
    fn weather_phase_is_deterministic() {
        let seed = Seed(7).derive(WEATHER_PHASE_TEST);
        let a = weather_phase(seed, 100.0, -20.0, 173.0);
        let b = weather_phase(seed, 100.0, -20.0, 173.0);
        assert_eq!(
            a.to_bits(),
            b.to_bits(),
            "identical inputs must give bit-identical phase"
        );
        assert!((-1.0..=1.0).contains(&a), "phase in range: {a}");
    }

    const WEATHER_PHASE_TEST: &str = "climate/weather/phase/v1";
}

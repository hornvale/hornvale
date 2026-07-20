//! The tier-1 climate provider: temperature, moisture, and the derived biome
//! and habitability fields over the shared Geosphere. Consumes an elevation
//! `CellMap` and scalar stellar inputs — never a terrain or astronomy type —
//! so climate stays kernel-only (spec §3). Recomputed on demand, never
//! serialized.

use crate::biome::{self, Biome, SeafloorFeature};
use crate::circulation::{
    RotationRegime, band_count_for, band_index, is_rising_band, prevailing_wind,
};
use crate::currents::ocean_current_field;
use crate::habitability;
use crate::moisture::{moisture_field, upwind_neighbor};
use crate::precipitation::{
    PrecipRegime, cloud_fraction, precip_mm_yr, precip_regime, snow_fraction,
};
use crate::temperature::{
    continentality, diurnal_amplitude_field, mean_temperature, temperature_at,
};
use crate::weather::{CloudType, WeatherState};
use crate::{AMBIENT, COLD, HEAT, RAIN, SNOW};
use hornvale_kernel::{
    CellId, CellMap, Geosphere, NearestCellIndex, ObserverContext, PhenomenaSource, Phenomenon,
    Precipitation, ReferenceElevation, Seed, Temperature, Venue,
};

/// The inputs the composition root supplies to build a climate (all bare
/// kernel types or climate-owned types — no terrain/astronomy imports).
/// type-audit: pending(wave-2)
pub struct ClimateInputs<'a> {
    /// The shared globe mesh.
    pub geosphere: &'a Geosphere,
    /// Elevation per cell, meters.
    pub elevation: &'a CellMap<ReferenceElevation>,
    /// Sea level, meters.
    pub sea_level: ReferenceElevation,
    /// Seafloor tectonic features per cell (mapped from terrain boundaries).
    pub seafloor: &'a CellMap<SeafloorFeature>,
    /// Stellar insolation relative to Earth (L / d², solar units / AU²).
    pub insolation: f64,
    /// Axial tilt, degrees.
    pub obliquity_deg: f64,
    /// The rotation regime.
    pub regime: RotationRegime,
    /// Year length in standard days (for time-varying temperature).
    pub year_length_std: f64,
    /// The orbital phase offset at epoch, in turns (`0.0..1.0`) — the same
    /// value `Calendar::year_phase` reads off the astronomy forcing.
    /// type-audit: bare-ok(ratio)
    pub year_phase_offset: f64,
    /// The world seed — derives the weather-phase noise (The Firmament). Climate
    /// is otherwise seed-free; this perturbs no existing draw.
    /// type-audit: bare-ok(constructor-edge: seed)
    pub seed: Seed,
}

/// The tier-1 climate: derived temperature/moisture/biome/habitability over
/// the globe. Owns its mesh so every query and CellMap agree on the cell
/// space. Recomputed on demand, never serialized.
#[derive(Debug, Clone)]
pub struct GeneratedClimate {
    geosphere: Geosphere,
    /// Latitude-bucketed cell index, built once at construction so `phenomena`
    /// resolves an observer's cell without rebuilding it (O(cells)) per call.
    nearest_cell: NearestCellIndex,
    elevation: CellMap<ReferenceElevation>,
    sea_level: ReferenceElevation,
    mean_temp: CellMap<Temperature>,
    moisture: CellMap<f64>,
    diurnal_amp: CellMap<f64>,
    precip: CellMap<Precipitation>,
    snow_fraction: CellMap<f64>,
    precip_regime: CellMap<PrecipRegime>,
    cloud_fraction: CellMap<f64>,
    weather_propensity: CellMap<f64>,
    weather_seed: Seed,
    current: CellMap<[f64; 3]>,
    biome: CellMap<Biome>,
    habitability: CellMap<bool>,
    band_count: Option<u32>,
    obliquity_deg: f64,
    year_length_std: f64,
    year_phase_offset: f64,
    insolation: f64,
    regime: RotationRegime,
}

/// Whether an ocean cell has a coastal upwelling: it borders land and the
/// prevailing wind carries surface water offshore (wind points away from the
/// mean direction to its land neighbors).
fn is_upwelling(
    geo: &Geosphere,
    elevation: &CellMap<ReferenceElevation>,
    sea_level: ReferenceElevation,
    cell: CellId,
    bands: Option<u32>,
) -> bool {
    if *elevation.get(cell) >= sea_level {
        return false;
    }
    let Some(bands) = bands else { return false };
    let p = geo.position(cell);
    // Mean direction toward land neighbors.
    let mut toward = [0.0, 0.0, 0.0];
    let mut land = 0;
    for &n in geo.neighbors(cell) {
        if *elevation.get(n) >= sea_level {
            let np = geo.position(n);
            toward = [
                toward[0] + np[0] - p[0],
                toward[1] + np[1] - p[1],
                toward[2] + np[2] - p[2],
            ];
            land += 1;
        }
    }
    if land == 0 {
        return false;
    }
    let wind = prevailing_wind(geo, cell, bands);
    // Offshore wind: wind opposes the toward-land direction.
    wind[0] * toward[0] + wind[1] * toward[1] + wind[2] * toward[2] < 0.0
}

/// The local along-wind terrain rise at `cell`, meters: the elevation gained
/// over the single immediate upwind hop (clamped to non-negative — downhill
/// contributes no orographic uplift). The same signal
/// `moisture::carried_water` sinks moisture on, recomputed here as a
/// diagnostic (feeds only `cloud_fraction_at`, nothing else). `None` bands
/// (tidally locked: no meaningful prevailing-wind direction) yield `0.0`.
fn local_uplift_m(
    geo: &Geosphere,
    elevation: &CellMap<ReferenceElevation>,
    cell: CellId,
    bands: Option<u32>,
) -> f64 {
    let Some(bands) = bands else { return 0.0 };
    let wind = prevailing_wind(geo, cell, bands);
    let Some(upwind) = upwind_neighbor(geo, cell, wind) else {
        return 0.0;
    };
    (*elevation.get(cell) - *elevation.get(upwind)).max(0.0)
}

impl GeneratedClimate {
    /// Derive the full climate from inputs.
    pub fn generate(inputs: &ClimateInputs) -> GeneratedClimate {
        let geo = inputs.geosphere;
        let band_count = band_count_for(&inputs.regime);
        let mean_temp = mean_temperature(
            geo,
            inputs.elevation,
            inputs.sea_level,
            inputs.insolation,
            &inputs.regime,
        );
        let moisture = moisture_field(geo, inputs.elevation, inputs.sea_level, &inputs.regime);
        let diurnal_amp =
            diurnal_amplitude_field(geo, inputs.elevation, inputs.sea_level, &moisture);
        let precip = CellMap::from_fn(geo, |cell| precip_mm_yr(*moisture.get(cell)));
        let snow_frac = CellMap::from_fn(geo, |cell| snow_fraction(mean_temp.get(cell).get()));
        let precip_regime_field = CellMap::from_fn(geo, |cell| {
            let band = band_count
                .map(|bands| band_index(geo.coord(cell).latitude, bands))
                .unwrap_or(0);
            let cont = continentality(geo, inputs.elevation, inputs.sea_level, cell);
            let hemisphere_sign = geo.coord(cell).latitude.signum();
            precip_regime(band, cont, hemisphere_sign)
        });
        let cloud_frac = CellMap::from_fn(geo, |cell| {
            let band = band_count
                .map(|bands| band_index(geo.coord(cell).latitude, bands))
                .unwrap_or(0);
            let rising = is_rising_band(band);
            let uplift = local_uplift_m(geo, inputs.elevation, cell, band_count);
            cloud_fraction(*moisture.get(cell), uplift, rising)
        });
        let sea_level = inputs.sea_level;
        let is_ocean = |cell: CellId| *inputs.elevation.get(cell) < sea_level;
        let weather_propensity = CellMap::from_fn(geo, |cell| {
            let band = band_count
                .map(|bands| band_index(geo.coord(cell).latitude, bands))
                .unwrap_or(0);
            let rising = is_rising_band(band);
            let mean_c = mean_temp.get(cell).get();
            let ocean_adjacent = geo.neighbors(cell).iter().any(|&n| is_ocean(n)) || is_ocean(cell);
            crate::weather::storm_propensity(*moisture.get(cell), rising, mean_c, ocean_adjacent)
        });
        let weather_seed = crate::weather::weather_seed(inputs.seed);
        let current = ocean_current_field(geo, &is_ocean, band_count);
        let biome = CellMap::from_fn(geo, |cell| {
            let temp = *mean_temp.get(cell);
            let upwell = is_upwelling(geo, inputs.elevation, inputs.sea_level, cell, band_count);
            biome::classify(
                temp,
                *moisture.get(cell),
                temp, // SST proxy = surface annual-mean temperature
                *inputs.elevation.get(cell),
                inputs.sea_level,
                geo.coord(cell).latitude,
                *inputs.seafloor.get(cell),
                upwell,
            )
        });
        let habitability = habitability::habitability_map(
            geo,
            inputs.elevation,
            &mean_temp,
            &moisture,
            inputs.sea_level,
        );
        GeneratedClimate {
            geosphere: geo.clone(),
            nearest_cell: NearestCellIndex::new(geo),
            elevation: inputs.elevation.clone(),
            sea_level: inputs.sea_level,
            mean_temp,
            moisture,
            diurnal_amp,
            precip,
            snow_fraction: snow_frac,
            precip_regime: precip_regime_field,
            cloud_fraction: cloud_frac,
            weather_propensity,
            weather_seed,
            current,
            biome,
            habitability,
            band_count,
            obliquity_deg: inputs.obliquity_deg,
            year_length_std: inputs.year_length_std,
            year_phase_offset: inputs.year_phase_offset,
            insolation: inputs.insolation,
            regime: inputs.regime,
        }
    }

    /// The mesh this climate is computed over.
    pub fn geosphere(&self) -> &Geosphere {
        &self.geosphere
    }
    /// Circulation bands per hemisphere; `None` when tidally locked.
    /// type-audit: bare-ok(count)
    pub fn band_count(&self) -> Option<u32> {
        self.band_count
    }
    /// The rotation regime.
    pub fn regime(&self) -> RotationRegime {
        self.regime
    }
    /// Whether this world is tidally locked — the true source, kept
    /// separate from `band_count()` even though locked worlds also have no
    /// bands, so callers don't conflate "no circulation bands" with "locked".
    /// type-audit: bare-ok(flag)
    pub fn is_locked(&self) -> bool {
        matches!(self.regime, RotationRegime::Locked)
    }
    /// Annual-mean temperature at a cell, °C.
    pub fn mean_temperature_at(&self, cell: CellId) -> Temperature {
        *self.mean_temp.get(cell)
    }
    /// Temperature at a cell on a given day, °C (mean plus the seasonal term).
    /// type-audit: pending(wave-2: day)
    pub fn temperature_at(&self, cell: CellId, day: f64) -> Temperature {
        temperature_at(
            &self.mean_temp,
            &self.diurnal_amp,
            &self.geosphere,
            &self.elevation,
            self.sea_level,
            self.obliquity_deg,
            self.insolation,
            self.year_length_std,
            self.year_phase_offset,
            &self.regime,
            cell,
            day,
        )
    }
    /// The seasonal period this climate's temperature swing is phased on,
    /// in standard days (the orbital year; a default on constant-sun worlds).
    /// type-audit: bare-ok(diagnostic-value: return)
    pub fn year_length_std(&self) -> f64 {
        self.year_length_std
    }

    /// The orbital phase offset at epoch, in turns, this climate was built
    /// with — the seasonal phase `temperature_at` will need to evaluate the
    /// time-varying term against a moving substellar point (Task 2/3).
    /// type-audit: bare-ok(ratio: return)
    pub fn year_phase_offset(&self) -> f64 {
        self.year_phase_offset
    }

    /// The stellar insolation relative to Earth this climate was built with
    /// (`L / d²`, solar units / AU²) — retained (not just consumed at
    /// generation) so the locked-libration temperature evaluator can
    /// recompute the `S^{1/4}` scale as the substellar point moves (Task 3).
    /// type-audit: bare-ok(diagnostic-value: return)
    pub fn insolation(&self) -> f64 {
        self.insolation
    }

    /// This climate's axial tilt, degrees — retained alongside
    /// `insolation()`/`year_length_std()`/`year_phase_offset()` so the
    /// locked-libration temperature evaluator can recompute the substellar
    /// latitude as the year turns (Task 3, The Wandering Sun).
    /// type-audit: bare-ok(diagnostic-value: return)
    pub fn obliquity_deg(&self) -> f64 {
        self.obliquity_deg
    }

    /// The hemisphere-signed seasonal half-swing at a cell, °C: the
    /// coefficient of the seasonal sinusoid, `amplitude × sign(latitude)`.
    /// Positive north, negative south, exactly `0.0` when locked, when the
    /// world has no year, or at zero obliquity — matching `temperature_at`,
    /// which this factors: `temperature_at(cell, day) == mean + swing ·
    /// sin(τ · frac(day / year_length_std))`.
    /// type-audit: bare-ok(diagnostic-value: return)
    pub fn seasonal_swing_at(&self, cell: CellId) -> f64 {
        match self.regime {
            RotationRegime::Locked => 0.0,
            RotationRegime::Spinning { .. } => {
                if self.year_length_std <= 0.0 || self.obliquity_deg == 0.0 {
                    return 0.0;
                }
                let amp = crate::temperature::seasonal_amplitude(
                    &self.geosphere,
                    &self.elevation,
                    self.sea_level,
                    self.obliquity_deg,
                    cell,
                );
                let hemi = self.geosphere.coord(cell).latitude.signum();
                amp * hemi
            }
        }
    }

    /// Moisture at a cell, `[0, 1]`.
    /// type-audit: bare-ok(ratio)
    pub fn moisture_at(&self, cell: CellId) -> f64 {
        *self.moisture.get(cell)
    }
    /// Annual precipitation at a cell, mm/yr — the moisture field mapped
    /// into an Earth-ranged total (see [`crate::precipitation::precip_mm_yr`]).
    pub fn precip_at(&self, cell: CellId) -> Precipitation {
        *self.precip.get(cell)
    }
    /// The fraction of precipitation falling as snow at a cell, `[0, 1]`,
    /// derived from annual-mean temperature (see
    /// [`crate::precipitation::snow_fraction`]).
    /// type-audit: bare-ok(ratio)
    pub fn snow_fraction_at(&self, cell: CellId) -> f64 {
        *self.snow_fraction.get(cell)
    }
    /// The seasonal precipitation regime at a cell (see
    /// [`crate::precipitation::precip_regime`]).
    pub fn regime_at(&self, cell: CellId) -> PrecipRegime {
        *self.precip_regime.get(cell)
    }
    /// Diagnostic cloud fraction at a cell, `[0, 1]` (see
    /// [`crate::precipitation::cloud_fraction`]). **Feeds nothing** — no
    /// insolation or temperature term reads this back; it is a readable
    /// field only.
    /// type-audit: bare-ok(ratio)
    pub fn cloud_fraction_at(&self, cell: CellId) -> f64 {
        *self.cloud_fraction.get(cell)
    }
    /// The synoptic weather state at a cell on a day (The Firmament) — sampled,
    /// never integrated (the Lorenz guard-rail). Level 0: an observation read.
    /// type-audit: pending(wave-2: day)
    pub fn weather_at(&self, cell: CellId, day: f64) -> WeatherState {
        let coord = self.geosphere.coord(cell);
        let phase =
            crate::weather::weather_phase(self.weather_seed, coord.longitude, coord.latitude, day);
        crate::weather::weather_state(*self.weather_propensity.get(cell), phase)
    }
    /// The cloud type worn at a cell on a day — the projection of
    /// [`Self::weather_at`].
    /// type-audit: pending(wave-2: day)
    pub fn cloud_type_at(&self, cell: CellId, day: f64) -> CloudType {
        let coord = self.geosphere.coord(cell);
        let prop = *self.weather_propensity.get(cell);
        let phase =
            crate::weather::weather_phase(self.weather_seed, coord.longitude, coord.latitude, day);
        let state = crate::weather::weather_state(prop, phase);
        let cirrus = crate::weather::cirrus_present(prop, phase);
        crate::weather::cloud_type(state, cirrus)
    }
    /// The climatological storm propensity at a cell, `[0,1]` (the slow prior).
    /// type-audit: bare-ok(ratio: return)
    pub fn storm_propensity_at(&self, cell: CellId) -> f64 {
        *self.weather_propensity.get(cell)
    }
    /// The precomputed diurnal half-range amplitude at a cell, °C: the
    /// coefficient `temperature_at` scales its diurnal waveform by. Zero has
    /// no special meaning here (unlike `seasonal_swing_at`, which is exactly
    /// zero when locked) — the amplitude is always computed, but only the
    /// `Spinning` branch of `temperature_at` ever applies it.
    /// type-audit: bare-ok(diagnostic-value: return)
    pub fn diurnal_amp_at(&self, cell: CellId) -> f64 {
        *self.diurnal_amp.get(cell)
    }
    /// The precomputed ocean surface-current vector at a cell (a unit-sphere
    /// tangent vector, zero over land and zero everywhere when locked — see
    /// [`crate::currents::ocean_current`]).
    /// type-audit: bare-ok(diagnostic-value: return)
    pub fn current_at(&self, cell: CellId) -> [f64; 3] {
        *self.current.get(cell)
    }
    /// The biome at a cell.
    pub fn biome_at(&self, cell: CellId) -> Biome {
        *self.biome.get(cell)
    }
    /// The full biome field (a clone of the derived map).
    pub fn biome_map(&self) -> CellMap<Biome> {
        self.biome.clone()
    }
    /// The per-cell habitability mask.
    /// type-audit: bare-ok(flag)
    pub fn habitability(&self) -> &CellMap<bool> {
        &self.habitability
    }
    /// The fraction of cells that are habitable.
    /// type-audit: bare-ok(ratio)
    pub fn habitable_fraction(&self) -> f64 {
        habitability::habitable_fraction(&self.habitability)
    }
}

// Felt weather decouples TWO thresholds (The Elements, Stage 2, G3-refined):
// the world is felt BROADLY (a narrow EMISSION band, so mild-but-imperfect
// climes emit a low-salience standing condition), but weather-gods stay RARE
// (a gentle salience SLOPE, so only a genuinely brutal clime crosses the 0.25
// pantheon FLOOR). Mild = felt sub-floor; brutal = deified.

/// The comfortable temperate midpoint, °C: roughly Earth's global mean
/// annual surface temperature. Felt heat/cold is the deviation from here.
const TEMPERATE_BASELINE_C: f64 = 14.0;
/// EMISSION threshold, °C: a cell whose annual mean deviates by at least this
/// from [`TEMPERATE_BASELINE_C`] emits a felt heat/cold standing condition.
/// Narrow, so mild climes (e.g. an 18 °C settlement, 4 °C off) ARE felt.
const TEMP_EMIT_MARGIN_C: f64 = 2.0;
/// Felt-temperature salience per °C of deviation beyond the emission margin, a
/// gentle slope (G3): crossing the 0.25 pantheon FLOOR (deification) needs a
/// genuinely brutal ~±15 °C from baseline — `0.019 × (15 − 2) = 0.247`, which
/// `round2`s to 0.25 (mean ≥ ~29 °C or ≤ ~−1 °C). A mild 4 °C deviation is
/// felt at only `round2(0.019 × 2) = 0.04` — well sub-floor.
const TEMP_SALIENCE_PER_C: f64 = 0.019;
/// Below this annual-mean temperature, precipitation on a wet cell falls as
/// snow rather than rain (water's freezing point).
const FREEZING_C: f64 = 0.0;
/// EMISSION threshold for moisture: a cell at or above this ("moderately wet",
/// not only near-saturated) emits a felt rain/snow standing condition. Below
/// it, no precipitation phenomenon (dryness is deferred). Narrow, so the world
/// is felt broadly.
const WET_EMIT_THRESHOLD: f64 = 0.5;
/// Precipitation salience per unit of moisture beyond the emission threshold, a
/// gentle slope: crossing the 0.25 FLOOR needs genuinely extreme wetness —
/// `0.5 × (1.0 − 0.5) = 0.25`, so only a near-saturated cell (moisture ≈ 1.0)
/// deifies. A moderately-wet 0.75 cell is felt at only
/// `round2(0.5 × 0.25) = 0.13` — sub-floor.
const MOISTURE_SALIENCE_PER_UNIT: f64 = 0.5;
/// Ceiling on any standing climate phenomenon's salience: felt weather never
/// outranks the sky's headline bodies.
const MAX_CLIMATE_SALIENCE: f64 = 0.9;

/// Round to two decimals — the salience precision every phenomena source
/// emits at (matches astronomy's emitter and `observe`'s lens arithmetic).
fn round2(x: f64) -> f64 {
    (x * 100.0).round() / 100.0
}

/// The tier-1 climate as a phenomena source: felt, standing conditions that
/// vary with place (spec §3.1.6). Pure function of `(self, ctx)`. A
/// position-blind observation (`ctx.position == None`) yields only the
/// inherited AMBIENT claim — felt weather needs a place. Refines, never
/// contradicts, the tier-0 [`UniformClimate`] (decision 0039): the AMBIENT
/// phenomenon is emitted byte-identically, with heat/cold/rain/snow added
/// beneath it. Emission is BROAD (a narrow deviation band, so the world is
/// felt widely) but salience is a GENTLE slope (so only a genuinely brutal
/// clime crosses the 0.25 pantheon floor and mints a weather-god).
impl PhenomenaSource for GeneratedClimate {
    fn phenomena(&self, ctx: &ObserverContext) -> Vec<Phenomenon> {
        // Tier-0's ambient claim still holds — emit it unchanged first, then
        // refine with the felt structure beneath it.
        let mut out = vec![Phenomenon {
            kind: AMBIENT.to_string(),
            description: "warm, still, unchanging air".to_string(),
            period_days: None,
            salience: 0.15,
            venue: Venue::Ambient,
        }];

        let Some(coord) = ctx.position else {
            return out;
        };
        let cell = self
            .nearest_cell
            .nearest(&self.geosphere, coord.latitude, coord.longitude);

        // Felt temperature: deviation from the temperate baseline. Warm past
        // the emission margin → heat; cold past it → cold; within the narrow
        // band → neither. Salience is a gentle slope so mild climes are felt
        // sub-floor and only a brutal one deifies.
        let mean = self.mean_temperature_at(cell).get();
        let deviation = mean - TEMPERATE_BASELINE_C;
        let temp_salience = round2(
            (TEMP_SALIENCE_PER_C * (deviation.abs() - TEMP_EMIT_MARGIN_C))
                .clamp(0.0, MAX_CLIMATE_SALIENCE),
        );
        if deviation >= TEMP_EMIT_MARGIN_C {
            out.push(Phenomenon {
                kind: HEAT.to_string(),
                description: "oppressive heat".to_string(),
                period_days: None,
                salience: temp_salience,
                venue: Venue::Ambient,
            });
        } else if deviation <= -TEMP_EMIT_MARGIN_C {
            out.push(Phenomenon {
                kind: COLD.to_string(),
                description: "biting cold".to_string(),
                period_days: None,
                salience: temp_salience,
                venue: Venue::Ambient,
            });
        }

        // Felt precipitation: moderately-wet cells and up. Snow when frozen,
        // else rain. Gentle slope: felt sub-floor unless genuinely extreme.
        // Dryness/drought is deferred (Stage 2 scope).
        let moisture = self.moisture_at(cell);
        if moisture >= WET_EMIT_THRESHOLD {
            let salience = round2(
                (MOISTURE_SALIENCE_PER_UNIT * (moisture - WET_EMIT_THRESHOLD))
                    .clamp(0.0, MAX_CLIMATE_SALIENCE),
            );
            let (kind, description) = if mean <= FREEZING_C {
                (SNOW, "falling snow")
            } else {
                (RAIN, "falling rain")
            };
            out.push(Phenomenon {
                kind: kind.to_string(),
                description: description.to_string(),
                period_days: None,
                salience,
                venue: Venue::Ambient,
            });
        }

        out
    }
}

/// Headline numbers of a climate, for the almanac and the Lab.
/// type-audit: bare-ok(count: band_count), bare-ok(ratio: habitable_fraction), bare-ok(count: land_biome_count), bare-ok(count: marine_biome_count)
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ClimateSummary {
    /// Circulation bands per hemisphere (`None` when locked).
    pub band_count: Option<u32>,
    /// Fraction of cells that are habitable.
    pub habitable_fraction: f64,
    /// Distinct land biomes present.
    pub land_biome_count: usize,
    /// Distinct marine biomes present.
    pub marine_biome_count: usize,
}

/// Summarize a climate's headline numbers (deterministic; ascending order).
pub fn summarize(climate: &GeneratedClimate) -> ClimateSummary {
    let mut land: Vec<&'static str> = Vec::new();
    let mut marine: Vec<&'static str> = Vec::new();
    for (_, b) in climate.biome.iter() {
        let list = if b.is_marine() {
            &mut marine
        } else {
            &mut land
        };
        if !list.contains(&b.name()) {
            list.push(b.name());
        }
    }
    ClimateSummary {
        band_count: climate.band_count,
        habitable_fraction: climate.habitable_fraction(),
        land_biome_count: land.len(),
        marine_biome_count: marine.len(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_kernel::Geosphere;

    fn inputs<'a>(
        geo: &'a Geosphere,
        elev: &'a CellMap<ReferenceElevation>,
        sea: &'a CellMap<SeafloorFeature>,
        regime: RotationRegime,
    ) -> ClimateInputs<'a> {
        ClimateInputs {
            geosphere: geo,
            elevation: elev,
            sea_level: ReferenceElevation::new(0.0).unwrap(),
            seafloor: sea,
            insolation: 1.0,
            obliquity_deg: 23.5,
            regime,
            year_length_std: 365.25,
            year_phase_offset: 0.0,
            seed: Seed(1),
        }
    }

    #[test]
    fn provider_answers_every_query_and_is_deterministic() {
        let geo = Geosphere::new(4);
        let elev = CellMap::from_fn(&geo, |c| {
            let m = if geo.position(c)[2] > 0.0 {
                300.0
            } else {
                -1000.0
            };
            ReferenceElevation::new(m).unwrap()
        });
        let sea = CellMap::from_fn(&geo, |_| SeafloorFeature::None);
        let regime = RotationRegime::Spinning { day_std: 1.0 };
        let a = GeneratedClimate::generate(&inputs(&geo, &elev, &sea, regime));
        let b = GeneratedClimate::generate(&inputs(&geo, &elev, &sea, regime));
        assert_eq!(a.band_count(), Some(3));
        assert_eq!(a.biome_map(), b.biome_map());
        assert!((0.0..=1.0).contains(&a.habitable_fraction()));
        // biome_map and biome_at agree.
        for c in geo.cells().take(50) {
            assert_eq!(*a.biome_map().get(c), a.biome_at(c));
        }
    }

    #[test]
    fn locked_provider_has_no_band_count() {
        let geo = Geosphere::new(3);
        let elev = CellMap::from_fn(&geo, |_| ReferenceElevation::new(200.0).unwrap());
        let sea = CellMap::from_fn(&geo, |_| SeafloorFeature::None);
        let c = GeneratedClimate::generate(&inputs(&geo, &elev, &sea, RotationRegime::Locked));
        assert_eq!(c.band_count(), None);
    }

    #[test]
    fn documented_evaluator_restates_temperature_at_exactly() {
        use hornvale_kernel::math;
        let geo = Geosphere::new(4);
        let elev = CellMap::from_fn(&geo, |_| ReferenceElevation::new(200.0).unwrap());
        let sea = CellMap::from_fn(&geo, |_| SeafloorFeature::None);
        let regime = RotationRegime::Spinning { day_std: 1.0 };
        let RotationRegime::Spinning { day_std } = regime else {
            unreachable!("this test builds a spinning regime")
        };
        let climate = GeneratedClimate::generate(&inputs(&geo, &elev, &sea, regime));
        let period = climate.year_length_std();
        assert!(period > 0.0);
        // The documented client evaluator, restated in Rust with the SAME libm sin
        // and the SAME frac. Must equal temperature_at to the last bit.
        for cell in geo.cells() {
            let mean = climate.mean_temperature_at(cell).get();
            let swing = climate.seasonal_swing_at(cell);
            let diurnal_amp = climate.diurnal_amp_at(cell);
            for &day in &[0.0_f64, 30.0, 91.3, 182.6, 300.0, 365.25, 800.0] {
                let phase = (day / period).rem_euclid(1.0);
                let diurnal = crate::diurnal::diurnal_anomaly(
                    diurnal_amp,
                    geo.coord(cell).latitude,
                    geo.coord(cell).longitude,
                    climate.obliquity_deg(),
                    phase,
                    day.rem_euclid(1.0),
                    day_std,
                )
                .get();
                let documented = mean + swing * math::sin(std::f64::consts::TAU * phase) + diurnal;
                let actual = climate.temperature_at(cell, day).get();
                assert_eq!(
                    documented.to_bits(),
                    actual.to_bits(),
                    "cell {} day {day}: documented {documented} != temperature_at {actual}",
                    cell.0
                );
            }
        }
    }

    #[test]
    fn seasonal_swing_is_zero_when_locked() {
        let geo = Geosphere::new(3);
        let elev = CellMap::from_fn(&geo, |_| ReferenceElevation::new(200.0).unwrap());
        let sea = CellMap::from_fn(&geo, |_| SeafloorFeature::None);
        let climate =
            GeneratedClimate::generate(&inputs(&geo, &elev, &sea, RotationRegime::Locked));
        for cell in geo.cells() {
            assert_eq!(climate.seasonal_swing_at(cell), 0.0);
        }
    }

    #[test]
    fn ocean_cells_map_to_marine_biomes() {
        let geo = Geosphere::new(4);
        let elev = CellMap::from_fn(&geo, |c| {
            let m = if geo.position(c)[2] > 0.0 {
                300.0
            } else {
                -3000.0
            };
            ReferenceElevation::new(m).unwrap()
        });
        let sea = CellMap::from_fn(&geo, |_| SeafloorFeature::None);
        let c = GeneratedClimate::generate(&inputs(
            &geo,
            &elev,
            &sea,
            RotationRegime::Spinning { day_std: 1.0 },
        ));
        for cell in geo.cells() {
            let marine = elev.get(cell).get() < 0.0;
            assert_eq!(
                c.biome_at(cell).is_marine(),
                marine,
                "cell {} biome/ocean mismatch",
                cell.0
            );
        }
    }

    #[test]
    fn climate_carries_the_year_phase_offset() {
        // Build a climate with a known offset and confirm it is retained for
        // the time-varying temperature phase (regression guard for the plumbing).
        let geo = Geosphere::new(2);
        let elevation = CellMap::from_fn(&geo, |_| ReferenceElevation::new(0.0).unwrap());
        let seafloor = CellMap::from_fn(&geo, |_| SeafloorFeature::None);
        let climate = GeneratedClimate::generate(&ClimateInputs {
            geosphere: &geo,
            elevation: &elevation,
            sea_level: ReferenceElevation::new(0.0).unwrap(),
            seafloor: &seafloor,
            insolation: 1.0,
            obliquity_deg: 23.5,
            regime: RotationRegime::Spinning { day_std: 1.0 },
            year_length_std: 360.0,
            year_phase_offset: 0.2,
            seed: Seed(2),
        });
        assert_eq!(climate.year_phase_offset(), 0.2);
    }

    // --- The felt-weather emitter (The Elements, Stage 2). ---

    use hornvale_kernel::{EntityId, GeoCoord, ObserverContext, PhenomenaSource, WorldTime};

    /// A high-insolation spinning climate whose cells span a wide range of
    /// mean temperatures and moistures — enough to exercise every emitter arm.
    fn varied_climate() -> GeneratedClimate {
        let geo = Geosphere::new(5);
        let elev = CellMap::from_fn(&geo, |c| {
            let m = if geo.position(c)[2] > 0.0 {
                200.0
            } else {
                -2000.0
            };
            ReferenceElevation::new(m).unwrap()
        });
        let sea = CellMap::from_fn(&geo, |_| SeafloorFeature::None);
        GeneratedClimate::generate(&ClimateInputs {
            insolation: 1.6,
            ..inputs(&geo, &elev, &sea, RotationRegime::Spinning { day_std: 1.0 })
        })
    }

    fn observe_at(climate: &GeneratedClimate, coord: GeoCoord) -> Vec<Phenomenon> {
        climate.phenomena(&ObserverContext::at_position(
            EntityId::new(1).unwrap(),
            WorldTime { day: 3.0 },
            coord,
        ))
    }

    #[test]
    fn emitter_is_pure() {
        let climate = varied_climate();
        let coord = climate.geosphere().coord(CellId(7));
        assert_eq!(observe_at(&climate, coord), observe_at(&climate, coord));
    }

    #[test]
    fn ambient_is_always_emitted_and_unrefined() {
        // Tier refinement (0039): the tier-0 ambient claim survives at every
        // vantage — position-blind (no cell) and placed alike, byte-identical
        // to UniformClimate's.
        let climate = varied_climate();
        let blind = climate.phenomena(&ObserverContext::at(
            EntityId::new(1).unwrap(),
            WorldTime { day: 3.0 },
        ));
        assert_eq!(
            blind.len(),
            1,
            "position-blind yields only the ambient claim"
        );
        let expected = crate::UniformClimate.phenomena(&ObserverContext::at(
            EntityId::new(1).unwrap(),
            WorldTime { day: 3.0 },
        ));
        assert_eq!(blind, expected, "ambient is byte-identical to tier 0");
        for cell in climate.geosphere().cells() {
            let out = observe_at(&climate, climate.geosphere().coord(cell));
            assert!(
                out.iter().any(|p| p.kind == AMBIENT),
                "every placed vantage keeps the ambient claim"
            );
        }
    }

    #[test]
    fn felt_weather_matches_the_documented_thresholds() {
        // For every cell, the emitter's heat/cold/rain/snow arms must agree
        // with the documented EMISSION rule computed from the same fields — a
        // strong correctness+purity check without forcing a specific extreme.
        let climate = varied_climate();
        for cell in climate.geosphere().cells() {
            let out = observe_at(&climate, climate.geosphere().coord(cell));
            let mean = climate.mean_temperature_at(cell).get();
            let deviation = mean - TEMPERATE_BASELINE_C;
            let has_heat = out.iter().any(|p| p.kind == HEAT);
            let has_cold = out.iter().any(|p| p.kind == COLD);
            assert_eq!(has_heat, deviation >= TEMP_EMIT_MARGIN_C, "cell {}", cell.0);
            assert_eq!(
                has_cold,
                deviation <= -TEMP_EMIT_MARGIN_C,
                "cell {}",
                cell.0
            );

            let moisture = climate.moisture_at(cell);
            let has_snow = out.iter().any(|p| p.kind == SNOW);
            let has_rain = out.iter().any(|p| p.kind == RAIN);
            let wet = moisture >= WET_EMIT_THRESHOLD;
            assert_eq!(has_snow, wet && mean <= FREEZING_C, "cell {}", cell.0);
            assert_eq!(has_rain, wet && mean > FREEZING_C, "cell {}", cell.0);

            // Salience never exceeds the ceiling, and every weather phenomenon
            // is a standing (aperiodic) ambient condition.
            for p in &out {
                assert!(p.salience <= MAX_CLIMATE_SALIENCE + 1e-9);
                if p.kind != AMBIENT {
                    assert_eq!(p.period_days, None);
                    assert_eq!(p.venue, Venue::Ambient);
                }
            }
        }
    }

    #[test]
    fn mild_climes_are_felt_but_never_deified() {
        // The two decoupled thresholds: BROAD emission (a cell within the
        // narrow ±2 °C band mints nothing; just past it IS felt) but a RARE
        // floor (crossing 0.25 needs a genuinely brutal deviation).
        let climate = varied_climate();
        let mut saw_felt_subfloor = false;
        for cell in climate.geosphere().cells() {
            let out = observe_at(&climate, climate.geosphere().coord(cell));
            let deviation = climate.mean_temperature_at(cell).get() - TEMPERATE_BASELINE_C;
            let felt = out.iter().find(|p| p.kind == HEAT || p.kind == COLD);

            // Emission tracks the narrow band exactly.
            if deviation.abs() < TEMP_EMIT_MARGIN_C {
                assert!(
                    felt.is_none(),
                    "cell {} within the ±2 °C band should mint nothing",
                    cell.0
                );
            } else {
                assert!(
                    felt.is_some(),
                    "cell {} past the emission margin should be felt",
                    cell.0
                );
            }

            // A mild clime (felt, but well short of brutal) stays sub-floor.
            if let Some(p) = felt {
                if deviation.abs() < 10.0 {
                    assert!(
                        p.salience < 0.25,
                        "cell {} at {deviation:.1} °C deviation should be felt sub-floor, \
                         got salience {}",
                        cell.0,
                        p.salience
                    );
                    saw_felt_subfloor = true;
                }
                // Crossing the deity FLOOR needs a genuinely brutal deviation.
                // The raw slope crosses 0.25 at 15 °C (`0.019 × (15 − 2) =
                // 0.247` → round2 0.25); round2 pulls the effective boundary
                // down to ~14.89 °C (raw 0.245 → 0.25), no further.
                if p.salience >= 0.25 {
                    assert!(
                        deviation.abs() >= 14.88,
                        "cell {} crossed the deity floor at only {deviation:.2} °C deviation",
                        cell.0
                    );
                }
            }
        }
        assert!(
            saw_felt_subfloor,
            "the varied climate must contain a mild-but-felt cell (the whole point)"
        );
    }

    // Coast tangency: an ocean cell adjacent to land has its into-land
    // component suppressed — the current runs ALONG the coast, not into it.
    // Build a real world with a coastline (the same land/ocean split
    // `ocean_cells_map_to_marine_biomes` uses), find an ocean cell with a
    // land neighbor, and assert the current's component toward the mean
    // land-neighbor direction is at most a small tolerance. The toward-land
    // direction is computed independently here (tangent-projected, normalized
    // mean of neighbor-minus-self over land neighbors) so this is a genuine
    // check on `current_at`'s output, not a restatement of the production
    // formula.
    #[test]
    fn coastal_current_does_not_flow_into_land() {
        let geo = Geosphere::new(5);
        let sea_level = ReferenceElevation::new(0.0).unwrap();
        let elev = CellMap::from_fn(&geo, |c| {
            let m = if geo.position(c)[2] > 0.0 {
                300.0
            } else {
                -1000.0
            };
            ReferenceElevation::new(m).unwrap()
        });
        let sea = CellMap::from_fn(&geo, |_| SeafloorFeature::None);
        let regime = RotationRegime::Spinning { day_std: 1.0 };
        let climate = GeneratedClimate::generate(&inputs(&geo, &elev, &sea, regime));
        let is_ocean = |c: CellId| *elev.get(c) < sea_level;

        let mut checked = false;
        for cell in geo.cells() {
            if !is_ocean(cell) {
                continue;
            }
            let pos = geo.position(cell);
            let len = (pos[0] * pos[0] + pos[1] * pos[1] + pos[2] * pos[2]).sqrt();
            let up = [pos[0] / len, pos[1] / len, pos[2] / len];

            let mut toward_land_sum = [0.0, 0.0, 0.0];
            for &n in geo.neighbors(cell) {
                if is_ocean(n) {
                    continue;
                }
                let npos = geo.position(n);
                let dir = [npos[0] - pos[0], npos[1] - pos[1], npos[2] - pos[2]];
                let radial = dir[0] * up[0] + dir[1] * up[1] + dir[2] * up[2];
                let tangent = [
                    dir[0] - radial * up[0],
                    dir[1] - radial * up[1],
                    dir[2] - radial * up[2],
                ];
                toward_land_sum = [
                    toward_land_sum[0] + tangent[0],
                    toward_land_sum[1] + tangent[1],
                    toward_land_sum[2] + tangent[2],
                ];
            }
            let tl_len = (toward_land_sum[0] * toward_land_sum[0]
                + toward_land_sum[1] * toward_land_sum[1]
                + toward_land_sum[2] * toward_land_sum[2])
                .sqrt();
            if tl_len < 1e-9 {
                // No land neighbor (or a degenerate cancellation) — skip.
                continue;
            }
            let toward_land = [
                toward_land_sum[0] / tl_len,
                toward_land_sum[1] / tl_len,
                toward_land_sum[2] / tl_len,
            ];
            let current = climate.current_at(cell);
            let into_land = current[0] * toward_land[0]
                + current[1] * toward_land[1]
                + current[2] * toward_land[2];
            assert!(
                into_land <= 1e-6,
                "cell {} current flows into land: into_land={into_land}",
                cell.0
            );
            checked = true;
        }
        assert!(
            checked,
            "expected at least one ocean cell with a land neighbor"
        );
    }

    // --- Drawn weather (The Firmament, Task 2 wiring). ---

    #[test]
    fn weather_is_defined_and_deterministic_over_the_globe() {
        let climate = varied_climate();
        for cell in climate.geosphere().cells().take(64) {
            let w1 = climate.weather_at(cell, 100.0);
            let w2 = climate.weather_at(cell, 100.0);
            assert_eq!(w1, w2);
            let c = climate.cloud_type_at(cell, 100.0);
            // Storm cells wear cumulonimbus; clear cells wear none-or-cirrus.
            match w1 {
                WeatherState::Storm => assert_eq!(c, CloudType::Cumulonimbus),
                WeatherState::Clear => assert!(matches!(c, CloudType::None | CloudType::Cirrus)),
                _ => {}
            }
        }
    }
}

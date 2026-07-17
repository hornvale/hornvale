//! The tier-1 climate provider: temperature, moisture, and the derived biome
//! and habitability fields over the shared Geosphere. Consumes an elevation
//! `CellMap` and scalar stellar inputs — never a terrain or astronomy type —
//! so climate stays kernel-only (spec §3). Recomputed on demand, never
//! serialized.

use crate::biome::{self, Biome, SeafloorFeature};
use crate::circulation::{RotationRegime, band_count_for, prevailing_wind};
use crate::habitability;
use crate::moisture::moisture_field;
use crate::temperature::{mean_temperature, temperature_at};
use hornvale_kernel::{CellId, CellMap, Geosphere, ReferenceElevation, Temperature};

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
}

/// The tier-1 climate: derived temperature/moisture/biome/habitability over
/// the globe. Owns its mesh so every query and CellMap agree on the cell
/// space. Recomputed on demand, never serialized.
#[derive(Debug, Clone)]
pub struct GeneratedClimate {
    geosphere: Geosphere,
    elevation: CellMap<ReferenceElevation>,
    sea_level: ReferenceElevation,
    mean_temp: CellMap<Temperature>,
    moisture: CellMap<f64>,
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
            elevation: inputs.elevation.clone(),
            sea_level: inputs.sea_level,
            mean_temp,
            moisture,
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
    /// Annual-mean temperature at a cell, °C.
    pub fn mean_temperature_at(&self, cell: CellId) -> Temperature {
        *self.mean_temp.get(cell)
    }
    /// Temperature at a cell on a given day, °C (mean plus the seasonal term).
    /// type-audit: pending(wave-2: day)
    pub fn temperature_at(&self, cell: CellId, day: f64) -> Temperature {
        temperature_at(
            &self.mean_temp,
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
        let climate = GeneratedClimate::generate(&inputs(&geo, &elev, &sea, regime));
        let period = climate.year_length_std();
        assert!(period > 0.0);
        // The documented client evaluator, restated in Rust with the SAME libm sin
        // and the SAME frac. Must equal temperature_at to the last bit.
        for cell in geo.cells() {
            let mean = climate.mean_temperature_at(cell).get();
            let swing = climate.seasonal_swing_at(cell);
            for &day in &[0.0_f64, 30.0, 91.3, 182.6, 300.0, 365.25, 800.0] {
                let phase = (day / period).rem_euclid(1.0);
                let documented = mean + swing * math::sin(std::f64::consts::TAU * phase);
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
        });
        assert_eq!(climate.year_phase_offset(), 0.2);
    }
}

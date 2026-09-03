//! The authored seasonal harvest curve (The Granary T1): a pure,
//! deterministic multiplier in `[0, AMPLITUDE_MAX]` giving a settlement's
//! food production as a function of day-of-year, keyed by latitude and
//! coarse biome class.
//!
//! PHASE PRECEDENT — this module authors no independent latitude or season
//! physics. It follows the annual phase convention of
//! `domains/climate/src/temperature.rs` for spinning worlds:
//! `year_phase = day / DAYS_PER_YEAR` with
//! `sub_lat = obliquity * sin(TAU * year_phase)`, so phase 0 is the
//! northern spring equinox (declination rising through zero) and phase
//! 0.25 is the northern summer solstice. NOTE this assumes the climate
//! module's per-world `year_phase_offset` is zero; at a non-zero offset
//! temperature.rs's phase 0 is not the spring equinox, so consuming that
//! offset here is deferred to bake wiring. The hemisphere sign comes from
//! the latitude's signum, exactly as temperature.rs's hemispheric
//! asymmetry term does. The curve peaks a fixed fraction of a year after
//! that hemisphere's solstice (`PEAK_AFTER_SOLSTICE`), i.e. at harvest.
//! Latitude itself enters only through its sign here — the authored
//! amplitude is per biome class, not per degree.

use hornvale_culture::BiomeClass;
use hornvale_kernel::math;
use hornvale_kernel::units::Years;

/// One Julian year in standard days (the kernel's canonical year).
/// plumb: pending(wave-1)
const DAYS_PER_YEAR: f64 = Years::DAYS_PER_YEAR;

/// The ceiling on any authored amplitude; every table entry is in
/// `[0, AMPLITUDE_MAX]`.
/// type-audit: bare-ok(ratio: AMPLITUDE_MAX)
/// plumb: pending(wave-1)
pub const AMPLITUDE_MAX: f64 = 1.0;

/// The fraction of a year after a hemisphere's summer solstice at which the
/// curve peaks — harvest, roughly 6 weeks past peak sun, when stores ripen.
/// The solstice as an annual phase (see the module doc): the northern
/// summer / southern winter turning point.
/// plumb: pending(wave-1)
const SOLSTICE_PHASE: f64 = 0.25;

/// The fraction of a year after a hemisphere's summer solstice at which the
/// curve peaks — harvest, roughly 6 weeks past peak sun, when stores ripen.
/// This puts the peak at phase 0.35 in the north and 0.85 in the south.
/// plumb: pending(wave-1)
const PEAK_AFTER_SOLSTICE: f64 = 0.10;

/// Half the year the curve spends above zero (the growing half); the other
/// half — winter through early spring — is exactly zero (living off stores).
/// plumb: pending(wave-1)
const GROWING_HALF: f64 = 0.5;

/// A validating latitude in degrees, `-90 ..= 90`.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct LatDeg(f64);

impl LatDeg {
    /// Validating constructor: rejects non-finite values and values outside
    /// `[-90, 90]`.
    /// type-audit: bare-ok(diagnostic-value: value)
    pub fn new(value: f64) -> Result<Self, LatError> {
        if !value.is_finite() || !(-90.0..=90.0).contains(&value) {
            return Err(LatError { value });
        }
        Ok(Self(value))
    }

    /// The raw value in degrees.
    /// type-audit: bare-ok(diagnostic-value: return)
    pub fn degrees(self) -> f64 {
        self.0
    }
}

/// A latitude outside the physical range `[-90, 90]` degrees (or non-finite).
/// type-audit: bare-ok(diagnostic-value: value)
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct LatError {
    /// The rejected value.
    pub value: f64,
}

/// Authored seasonal production multiplier in `[0, AMPLITUDE_MAX]`: pure,
/// periodic with period one year, near-zero through winter, single annual
/// peak at harvest, falling to living-off-stores afterwards.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Curve {
    latitude: LatDeg,
    biome: BiomeClass,
}

impl Curve {
    /// Author a curve for a latitude and biome class.
    pub fn new(latitude: LatDeg, biome: BiomeClass) -> Self {
        Self { latitude, biome }
    }

    /// The production multiplier at a day of year (any finite `f64`; the
    /// curve is periodic with period [`DAYS_PER_YEAR`]). Day 0 shares the
    /// climate module's annual-phase epoch: the northern spring equinox.
    /// Latitude 0 takes the northern convention (the hemispheres are
    /// distinguished only by the latitude's sign).
    /// type-audit: bare-ok(diagnostic-value: day_of_year), bare-ok(ratio: return)
    pub fn at(&self, day_of_year: f64) -> f64 {
        debug_assert!(day_of_year.is_finite());
        let amp = self.amplitude();
        let rel = (day_of_year.rem_euclid(DAYS_PER_YEAR) - self.peak_phase() * DAYS_PER_YEAR)
            .rem_euclid(DAYS_PER_YEAR)
            / DAYS_PER_YEAR;
        if rel >= GROWING_HALF {
            return 0.0;
        }
        let c = math::cos(std::f64::consts::PI * rel);
        amp * c * c
    }

    /// The day of year at which the curve peaks (harvest).
    /// type-audit: bare-ok(diagnostic-value: return)
    pub fn peak_day_of_year(&self) -> f64 {
        self.peak_phase() * DAYS_PER_YEAR
    }

    /// The authored per-biome-class production swing. Small authored table,
    /// one line of justification each:
    /// - Forest: deep soil and moisture carry a strong but shade-damped
    ///   swing.
    /// - Grassland: the grain belt — the widest single-harvest swing.
    /// - Arid: sparse, drought-spread production barely seasons.
    /// - Cold: a short intense season against a long frozen floor.
    /// - Barren: ice, alpine, and marine margins hardly farm at all.
    ///
    /// type-audit: bare-ok(ratio: return)
    pub fn amplitude(&self) -> f64 {
        amplitude_for(self.biome)
    }

    /// This hemisphere's peak as an annual phase in `[0, 1)` — the
    /// solstice phase plus the post-solstice lag, offset by half a year in
    /// the southern hemisphere. Latitude 0 takes the northern convention.
    fn peak_phase(&self) -> f64 {
        let base = SOLSTICE_PHASE + PEAK_AFTER_SOLSTICE;
        if self.latitude.degrees() < 0.0 {
            (base + 0.5).rem_euclid(1.0)
        } else {
            base
        }
    }
}

/// The authored amplitude table, one entry per [`BiomeClass`] variant.
/// type-audit: bare-ok(ratio: return)
fn amplitude_for(biome: BiomeClass) -> f64 {
    match biome {
        BiomeClass::Forest => 0.55,
        BiomeClass::Grassland => 0.70,
        BiomeClass::Arid => 0.25,
        BiomeClass::Cold => 0.35,
        BiomeClass::Barren => 0.05,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn curve(lat: f64, biome: BiomeClass) -> Curve {
        Curve::new(LatDeg::new(lat).unwrap(), biome)
    }

    #[test]
    fn near_zero_in_winter_and_single_peak_at_harvest() {
        let c = curve(45.0, BiomeClass::Grassland);
        // Winter: half a year after the peak the curve is exactly off.
        let winter = c.at(c.peak_day_of_year() + DAYS_PER_YEAR * 0.5);
        assert_eq!(winter, 0.0);
        // Single peak: sweep two full years with a non-dividing step count
        // (4000 steps over 2×DAYS_PER_YEAR never lands exactly on the
        // solstice/peak boundary, unlike a 3650-step year grid) and count
        // STRICT local maxima — one per full period swept.
        let peak = c.peak_day_of_year();
        const PERIODS: f64 = 2.0;
        let steps = 4000_usize;
        let span = PERIODS * DAYS_PER_YEAR;
        let mut best = (f64::NEG_INFINITY, 0.0_f64);
        let mut maxima = 0_usize;
        let values: Vec<f64> = (0..=steps)
            .map(|step| c.at(step as f64 * span / steps as f64))
            .collect();
        for i in 1..values.len().saturating_sub(1) {
            if values[i] > values[i - 1] && values[i] > values[i + 1] {
                maxima += 1;
            }
            if values[i] > best.0 {
                best = (values[i], i as f64 * span / steps as f64);
            }
        }
        // A periodic curve has one peak per period; two periods must
        // therefore carry exactly two strict maxima and no more.
        assert_eq!(maxima, PERIODS as usize, "one strict maximum per period");
        assert!(
            (best.1.rem_euclid(DAYS_PER_YEAR) - peak).abs() < DAYS_PER_YEAR * 0.5 / steps as f64,
            "sampled peak {best:?} vs authored {peak}"
        );
        assert!(
            (best.0 - c.amplitude()).abs() < 1e-6,
            "sampled maximum {} vs authored amplitude {}",
            best.0,
            c.amplitude()
        );
    }

    #[test]
    fn southern_hemisphere_is_half_a_year_out_of_phase() {
        let north = curve(45.0, BiomeClass::Forest);
        let south = curve(-45.0, BiomeClass::Forest);
        let n = north.peak_day_of_year();
        let s = south.peak_day_of_year();
        let expected_south = (n + DAYS_PER_YEAR * 0.5) % DAYS_PER_YEAR;
        assert_eq!(s, expected_south);
        // And the waveforms mirror fully: south at d is north at d shifted
        // half a year, pointwise across a sweep.
        for step in 0..=1000 {
            let d = step as f64 * DAYS_PER_YEAR / 1000.0;
            let mirrored = (d + DAYS_PER_YEAR * 0.5) % DAYS_PER_YEAR;
            assert!(
                (north.at(d) - south.at(mirrored)).abs() < 1e-12,
                "at day {d}: {} vs {}",
                north.at(d),
                south.at(mirrored)
            );
        }
    }

    #[test]
    fn periodic_over_one_year() {
        let c = curve(-30.0, BiomeClass::Cold);
        for step in 0..73 {
            let d = step as f64 * 5.0;
            assert_eq!(c.at(d), c.at(d + DAYS_PER_YEAR));
            assert_eq!(c.at(d), c.at(d + 3.0 * DAYS_PER_YEAR));
        }
    }

    #[test]
    fn amplitude_table_covers_every_variant_in_range() {
        let all = [
            (BiomeClass::Forest, 0.55),
            (BiomeClass::Grassland, 0.70),
            (BiomeClass::Arid, 0.25),
            (BiomeClass::Cold, 0.35),
            (BiomeClass::Barren, 0.05),
        ];
        for (biome, expected) in all {
            let c = curve(45.0, biome);
            assert_eq!(c.amplitude(), expected, "{biome:?}");
            assert!((0.0..=AMPLITUDE_MAX).contains(&c.amplitude()));
        }
    }

    #[test]
    fn lat_deg_rejects_out_of_range() {
        assert!(LatDeg::new(90.0).is_ok());
        assert!(LatDeg::new(-90.0).is_ok());
        assert!(LatDeg::new(90.1).is_err());
        assert!(LatDeg::new(-90.1).is_err());
        assert!(LatDeg::new(f64::NAN).is_err());
        assert!(LatDeg::new(f64::INFINITY).is_err());
    }
}

//! The authored seasonal harvest curve (The Granary T1): a pure,
//! deterministic multiplier in `[0, AMPLITUDE_MAX]` giving a settlement's
//! food production as a function of day-of-year, keyed by latitude and
//! coarse biome class.
//!
//! PHASE PRECEDENT — this module authors no independent latitude or season
//! physics. The annual phase convention is the one
//! `domains/climate/src/temperature.rs` already fixed for spinning worlds:
//! `year_phase = day / DAYS_PER_YEAR` with
//! `sub_lat = obliquity * sin(TAU * year_phase)`, so phase 0 is the
//! northern spring equinox (declination rising through zero) and phase
//! 0.25 is the northern summer solstice. The hemisphere sign comes from
//! the latitude's signum, exactly as temperature.rs's hemispheric
//! asymmetry term does. The curve peaks a fixed fraction of a year after
//! that hemisphere's solstice (`PEAK_AFTER_SOLSTICE`), i.e. at harvest.
//! Latitude itself enters only through its sign here — the authored
//! amplitude is per biome class, not per degree.

use hornvale_culture::BiomeClass;
use hornvale_kernel::math;
use hornvale_kernel::units::Years;

/// One Julian year in standard days (the kernel's canonical year).
const DAYS_PER_YEAR: f64 = Years::DAYS_PER_YEAR;

/// The ceiling on any authored amplitude; every table entry is in
/// `[0, AMPLITUDE_MAX]`.
/// type-audit: bare-ok(ratio: AMPLITUDE_MAX)
pub const AMPLITUDE_MAX: f64 = 1.0;

/// The fraction of a year after a hemisphere's summer solstice at which the
/// curve peaks — harvest, roughly 6 weeks past peak sun, when stores ripen.
/// Phase 0.25 is the solstice (see the module doc), so this puts the peak
/// at phase 0.35 in the north and 0.85 in the south.
const PEAK_AFTER_SOLSTICE: f64 = 0.10;

/// Half the year the curve spends above zero (the growing half); the other
/// half — winter through early spring — is exactly zero (living off stores).
const GROWING_HALF: f64 = 0.5;

/// A validating latitude in degrees, `-90 ..= 90`.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct LatDeg(f64);

impl LatDeg {
    /// Validating constructor: rejects non-finite values outside
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
    /// type-audit: bare-ok(diagnostic-value: day_of_year), bare-ok(ratio: return)
    pub fn at(&self, day_of_year: f64) -> f64 {
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
    /// climate-module solstice phase plus the post-solstice lag, offset by
    /// half a year in the southern hemisphere.
    fn peak_phase(&self) -> f64 {
        let base = 0.25 + PEAK_AFTER_SOLSTICE;
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
        // Single peak: the sampled maximum lands on peak_day_of_year, and
        // the neighbourhood around it is unimodal.
        let peak = c.peak_day_of_year();
        let mut best = (f64::NEG_INFINITY, 0.0_f64);
        let mut maxima = 0_usize;
        let mut prev = f64::NEG_INFINITY;
        let mut rising = true;
        for step in 0..=3650 {
            let d = step as f64 * DAYS_PER_YEAR / 3650.0;
            let v = c.at(d);
            if v > best.0 {
                best = (v, d);
            }
            if v < prev && rising {
                rising = false;
                maxima += 1;
            } else if v > prev && !rising {
                panic!("curve rises again after falling: more than one peak");
            }
            prev = v;
        }
        assert_eq!(maxima, 1);
        assert!(
            (best.1 - peak).abs() < DAYS_PER_YEAR / 3650.0,
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
        // And the waveforms mirror: north at n equals south at s.
        assert_eq!(north.at(n), south.at(s));
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

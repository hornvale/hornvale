//! The calendar layer: translate absolute standard days into a world's own
//! cycles (spec §6, two clocks). Worlds without a cycle have calendars
//! without that column — truthfully.

use crate::anchor::Rotation;
use crate::system::StarSystem;
use crate::units::StdDays;

#[cfg(test)]
mod tests {
    use super::*;
    use crate::pins::{MoonsPin, RotationPin, SkyPins};
    use crate::system::generate;
    use crate::units::Degrees;
    use hornvale_kernel::Seed;

    fn spinning_system() -> StarSystem {
        let pins = SkyPins {
            rotation: Some(RotationPin::PeriodHours(24.0)),
            moons: Some(MoonsPin::exact(1).unwrap()),
            ..SkyPins::default()
        };
        generate(Seed(42), &pins).unwrap().system
    }

    fn locked_system() -> StarSystem {
        let pins = SkyPins {
            rotation: Some(RotationPin::Locked),
            ..SkyPins::default()
        };
        generate(Seed(42), &pins).unwrap().system
    }

    #[test]
    fn local_days_advance_with_absolute_time() {
        let cal = calendar_of(&spinning_system());
        assert_eq!(cal.day_length().unwrap().get(), 1.0);
        let (index, fraction) = cal.local_day(StdDays::new(2.25).unwrap()).unwrap();
        assert_eq!(index, 2);
        // The genesis day-phase offset (SKY-4) shifts where in the day t=0
        // falls, so the expected fraction is 0.25 plus that offset.
        let expected = (0.25 + cal.forcing.day_phase_offset).fract();
        assert!((fraction - expected).abs() < 1e-12);
    }

    #[test]
    fn locked_worlds_have_no_local_day_and_no_daylight_cycle() {
        let cal = calendar_of(&locked_system());
        assert!(cal.day_length().is_none());
        assert!(cal.local_day(StdDays::new(5.0).unwrap()).is_none());
        assert!(cal.daylight_fraction(StdDays::new(5.0).unwrap()).is_none());
        assert!(cal.is_daylight(StdDays::new(5.0).unwrap()).is_none());
    }

    #[test]
    fn daylight_follows_the_declared_sinusoid() {
        let pins = SkyPins {
            rotation: Some(RotationPin::PeriodHours(24.0)),
            moons: Some(MoonsPin::exact(1).unwrap()),
            forcing: Some(crate::pins::ForcingPin::Zero),
            ..SkyPins::default()
        };
        let system = generate(Seed(42), &pins).unwrap().system;
        let cal = calendar_of(&system);
        let year = cal.year_length().get();
        let obliquity = system.anchor.obliquity.get();
        // Midsummer is year phase 0.25; the genesis year-phase offset
        // (SKY-4) shifts which absolute day that falls on, so solve for it
        // rather than assuming t=0 is the start of the year.
        let t_frac = (0.25 - cal.forcing.year_phase_offset).rem_euclid(1.0);
        let t = StdDays::new(t_frac * year).unwrap();
        let expected = 0.5 + (obliquity / 90.0) * 0.5;
        assert!((cal.daylight_fraction(t).unwrap() - expected).abs() < 1e-9);
    }

    #[test]
    fn zero_obliquity_means_no_seasons_and_flat_daylight() {
        let pins = SkyPins {
            rotation: Some(RotationPin::PeriodHours(24.0)),
            obliquity: Some(Degrees::new(0.0).unwrap()),
            forcing: Some(crate::pins::ForcingPin::Zero),
            ..SkyPins::default()
        };
        let cal = calendar_of(&generate(Seed(42), &pins).unwrap().system);
        let t = StdDays::new(100.0).unwrap();
        assert!(cal.season_phase(t).is_none());
        assert_eq!(cal.daylight_fraction(t).unwrap(), 0.5);
    }

    #[test]
    fn t0_daylight_matches_the_pre_forcing_value() {
        // At t=0 the obliquity term equals ε₀ (anchored). With no eccentricity
        // the daylight at year-phase 0 is 0.5 (equinoctial), unchanged.
        let pins = SkyPins {
            rotation: Some(RotationPin::PeriodHours(24.0)),
            obliquity: Some(Degrees::new(0.0).unwrap()),
            forcing: Some(crate::pins::ForcingPin::Zero),
            ..SkyPins::default()
        };
        let cal = calendar_of(&generate(Seed(42), &pins).unwrap().system);
        assert_eq!(
            cal.daylight_fraction(StdDays::new(0.0).unwrap()).unwrap(),
            0.5
        );
    }

    #[test]
    fn zero_tilt_world_still_has_a_year_from_eccentricity() {
        // SKY-2 isolated: obliquity mean AND wobble amplitude are both zero, so
        // obliquity_at(t) == 0 for all t; the ONLY seasonal driver is
        // eccentricity. If the apsidal term were dropped, season_phase would be
        // None and daylight would be flat — this test would fail.
        let forcing = crate::forcing::OrbitalForcing {
            obliquity_mean: 0.0,
            obliquity_amp: 0.0,
            obliquity_phase: 0.0,
            ecc_mean: 0.02,
            ecc_amp: 0.0,
            ecc_phase: 0.0,
            precession_phase: 0.0,
            year_phase_offset: 0.0,
            day_phase_offset: 0.0,
            moon_phase_offsets: Vec::new(),
        };
        let cal = Calendar {
            day: Some(StdDays::new(1.0).unwrap()),
            year: StdDays::new(365.25).unwrap(),
            forcing,
            moon_periods: Vec::new(),
        };
        // Obliquity is identically zero, so any season/daylight variation is
        // purely apsidal (eccentricity-driven).
        assert_eq!(cal.forcing.obliquity_at(123.0), 0.0);
        assert!(
            cal.season_phase(StdDays::new(50.0).unwrap()).is_some(),
            "eccentricity gives even a zero-tilt world a year (SKY-2)"
        );
        // Daylight is not flat across the year: the apsidal term makes it vary.
        let spring = cal
            .daylight_fraction(StdDays::new(365.25 * 0.25).unwrap())
            .unwrap();
        let autumn = cal
            .daylight_fraction(StdDays::new(365.25 * 0.75).unwrap())
            .unwrap();
        assert!(
            (spring - autumn).abs() > 1e-6,
            "apsidal daylight must vary over the year"
        );
    }

    #[test]
    fn moon_phase_and_months_derive_from_kepler_periods() {
        let system = spinning_system();
        let cal = calendar_of(&system);
        let period = system.moons[0].period.get();
        let t0 = StdDays::new(0.0).unwrap();
        let t = StdDays::new(period * 1.5).unwrap();
        // Half a period advances the phase by 0.5, modulo wraparound — the
        // offset shifts where day 0 sits but not how fast the phase moves.
        let advance =
            (cal.moon_phase(t, 0).unwrap() - cal.moon_phase(t0, 0).unwrap()).rem_euclid(1.0);
        assert!((advance - 0.5).abs() < 1e-9);
        let months = cal.months_per_year(0).unwrap();
        assert!((months - cal.year_length().get() / period).abs() < 1e-12);
        assert!(cal.moon_phase(t, 5).is_none());
    }

    #[test]
    fn genesis_day_zero_is_not_a_grand_alignment() {
        let cal = calendar_of(&spinning_system());
        // At least one of year/day/moon phase is non-zero at t=0.
        let y = cal.year_phase(StdDays::new(0.0).unwrap());
        let (_, dfrac) = cal.local_day(StdDays::new(0.0).unwrap()).unwrap();
        let m = cal.moon_phase(StdDays::new(0.0).unwrap(), 0).unwrap_or(0.0);
        assert!(
            y != 0.0 || dfrac != 0.0 || m != 0.0,
            "day 0 is still a grand alignment"
        );
    }

    #[test]
    fn equatorial_daylight_is_a_flat_half_all_year() {
        let cal = calendar_of(&spinning_system());
        let year = cal.year_length().get();
        for k in 0..24 {
            let t = StdDays::new(k as f64 * year / 24.0).unwrap();
            let f = cal.daylight_fraction_at(t, 0.0).unwrap();
            assert!((f - 0.5).abs() < 1e-9, "equator not flat at t={t:?}: {f}");
        }
    }

    #[test]
    fn high_latitude_reaches_polar_day_and_night() {
        // A tilted, spinning world with the forcing zeroed (constant obliquity,
        // no drift): latitude 85° hits polar day near midsummer and polar night
        // near midwinter. The year-phase offset (Plan 1) shifts where in `t`
        // the solstices fall, so scan the year for the extremes rather than
        // assuming they sit at 0.25/0.75 of it.
        let pins = SkyPins {
            rotation: Some(RotationPin::PeriodHours(24.0)),
            obliquity: Some(Degrees::new(23.5).unwrap()),
            forcing: Some(crate::pins::ForcingPin::Zero),
            ..SkyPins::default()
        };
        let cal = calendar_of(&generate(Seed(42), &pins).unwrap().system);
        let year = cal.year_length().get();
        let (mut max, mut min) = (0.0_f64, 1.0_f64);
        for k in 0..365 {
            let t = StdDays::new(k as f64 * year / 365.0).unwrap();
            let f = cal.daylight_fraction_at(t, 85.0).unwrap();
            max = max.max(f);
            min = min.min(f);
        }
        assert!(max > 0.999, "expected polar day at 85°, got {max}");
        assert!(min < 0.001, "expected polar night at 85°, got {min}");
    }

    #[test]
    fn a_locked_world_has_no_latitude_daylight() {
        let cal = calendar_of(&locked_system());
        assert!(
            cal.daylight_fraction_at(StdDays::new(0.0).unwrap(), 45.0)
                .is_none()
        );
    }
}

/// A world's cycles, derived once from its star system.
#[derive(Debug, Clone, PartialEq)]
pub struct Calendar {
    day: Option<StdDays>,
    year: StdDays,
    moon_periods: Vec<StdDays>,
    forcing: crate::forcing::OrbitalForcing,
}

/// Derive the calendar from a generated system.
pub fn calendar_of(system: &StarSystem) -> Calendar {
    let day = match system.anchor.rotation {
        Rotation::Spinning { day } => Some(day),
        Rotation::Locked => None,
    };
    Calendar {
        day,
        year: system.anchor.year,
        moon_periods: system.moons.iter().map(|m| m.period).collect(),
        forcing: system.forcing.clone(),
    }
}

impl Calendar {
    /// Length of one local day, if the world has one.
    pub fn day_length(&self) -> Option<StdDays> {
        self.day
    }
    /// Length of the year in standard days.
    pub fn year_length(&self) -> StdDays {
        self.year
    }
    /// Local day index and fraction at absolute time `t`.
    pub fn local_day(&self, t: StdDays) -> Option<(u64, f64)> {
        let day = self.day?;
        let local = t.0 / day.0;
        let fraction = (local.fract() + self.forcing.day_phase_offset).fract();
        Some((local as u64, fraction))
    }
    /// Fraction of the year elapsed at `t`.
    pub fn year_phase(&self, t: StdDays) -> f64 {
        (t.0 / self.year.0 + self.forcing.year_phase_offset).fract()
    }
    /// Seasonal phase; present when either driver (tilt or eccentricity) acts.
    pub fn season_phase(&self, t: StdDays) -> Option<f64> {
        let obliquity = self.forcing.obliquity_at(t.0);
        let ecc = self.forcing.eccentricity_at(t.0);
        if obliquity == 0.0 && ecc == 0.0 {
            return None;
        }
        Some(self.year_phase(t))
    }
    /// Daylight fraction: the tilt sinusoid (time-varying obliquity) plus a
    /// smaller apsidal term from eccentricity (a tilt-independent driver).
    /// Absent on a tidally locked world.
    pub fn daylight_fraction(&self, t: StdDays) -> Option<f64> {
        self.day?;
        let obliquity = self.forcing.obliquity_at(t.0);
        let ecc = self.forcing.eccentricity_at(t.0);
        let phase = self.year_phase(t);
        let tilt_term = (obliquity / 90.0) * 0.5 * (std::f64::consts::TAU * phase).sin();
        let apsidal_term = ecc * 0.5 * (std::f64::consts::TAU * phase).sin();
        Some((0.5 + tilt_term + apsidal_term).clamp(0.0, 1.0))
    }
    /// Daylight fraction of the local day at a latitude (SKY-8): the standard
    /// sunrise equation. ~0.5 flat at the equator, running to 1.0/0.0 (polar
    /// day/night) toward the poles. Reads the time-varying obliquity, so a
    /// world's daylight geometry drifts with its axial tilt. `None` on a
    /// tidally locked world, which has no day/night cycle.
    pub fn daylight_fraction_at(&self, t: StdDays, latitude: f64) -> Option<f64> {
        self.day?;
        // Solar declination: the sub-solar latitude oscillates over the year,
        // its amplitude the (time-varying) obliquity.
        let obliquity = self.forcing.obliquity_at(t.0).to_radians();
        let declination = obliquity * (std::f64::consts::TAU * self.year_phase(t)).sin();
        let phi = latitude.to_radians();
        // cos H0 = −tan φ · tan δ; the clamp yields polar day (−1 → H0 = π,
        // fraction 1) and polar night (1 → H0 = 0, fraction 0) past the polar
        // circles.
        let cos_h0 = (-phi.tan() * declination.tan()).clamp(-1.0, 1.0);
        Some(cos_h0.acos() / std::f64::consts::PI)
    }
    /// Is it daylight at `t`? Daylight is a centered window of the local day.
    pub fn is_daylight(&self, t: StdDays) -> Option<bool> {
        let fraction = self.local_day(t)?.1;
        let f = self.daylight_fraction(t)?;
        Some(fraction > (1.0 - f) / 2.0 && fraction < (1.0 + f) / 2.0)
    }
    /// Phase of moon `index` at `t`, if that moon exists.
    pub fn moon_phase(&self, t: StdDays, index: usize) -> Option<f64> {
        let period = self.moon_periods.get(index)?;
        let offset = self
            .forcing
            .moon_phase_offsets
            .get(index)
            .copied()
            .unwrap_or(0.0);
        Some((t.0 / period.0 + offset).fract())
    }
    /// How many of moon `index`'s cycles fit in a year.
    pub fn months_per_year(&self, index: usize) -> Option<f64> {
        let period = self.moon_periods.get(index)?;
        Some(self.year.0 / period.0)
    }
}

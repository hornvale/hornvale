//! The calendar layer: translate absolute standard days into a world's own
//! cycles (spec §6, two clocks). Worlds without a cycle have calendars
//! without that column — truthfully.

use crate::anchor::Rotation;
use crate::system::StarSystem;
use crate::units::{Degrees, StdDays};

#[cfg(test)]
mod tests {
    use super::*;
    use crate::pins::{MoonsPin, RotationPin, SkyPins};
    use crate::system::generate;
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
        let system = spinning_system();
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
            ..SkyPins::default()
        };
        let cal = calendar_of(&generate(Seed(42), &pins).unwrap().system);
        let t = StdDays::new(100.0).unwrap();
        assert!(cal.season_phase(t).is_none());
        assert_eq!(cal.daylight_fraction(t).unwrap(), 0.5);
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
}

/// A world's cycles, derived once from its star system.
#[derive(Debug, Clone, PartialEq)]
pub struct Calendar {
    day: Option<StdDays>,
    year: StdDays,
    obliquity: Degrees,
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
        obliquity: system.anchor.obliquity,
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
    /// Seasonal phase, absent on a world without axial tilt.
    pub fn season_phase(&self, t: StdDays) -> Option<f64> {
        if self.obliquity.0 == 0.0 {
            return None;
        }
        Some(self.year_phase(t))
    }
    /// Daylight fraction of the local day: the model card's sinusoid.
    /// Absent on a tidally locked world.
    pub fn daylight_fraction(&self, t: StdDays) -> Option<f64> {
        self.day?;
        Some(
            0.5 + (self.obliquity.0 / 90.0)
                * 0.5
                * (std::f64::consts::TAU * self.year_phase(t)).sin(),
        )
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

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

    /// A minimal calendar with one moon at the given sidereal period, in a
    /// year of the given length (both in standard days) — for exercising
    /// the sidereal/synodic conversion at exact, hand-checkable values.
    fn calendar_with_moon(sidereal_days: f64, year_days: f64) -> Calendar {
        Calendar {
            day: None,
            year: StdDays::new(year_days).unwrap(),
            obliquity: Degrees::new(0.0).unwrap(),
            moon_periods: vec![StdDays::new(sidereal_days).unwrap()],
        }
    }

    #[test]
    fn local_days_advance_with_absolute_time() {
        let cal = calendar_of(&spinning_system());
        assert_eq!(cal.day_length().unwrap().get(), 1.0);
        let (index, fraction) = cal.local_day(StdDays::new(2.25).unwrap()).unwrap();
        assert_eq!(index, 2);
        assert!((fraction - 0.25).abs() < 1e-12);
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
        // Midsummer (year phase 0.25): maximum daylight.
        let t = StdDays::new(0.25 * year).unwrap();
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
    fn moon_phase_and_months_derive_from_synodic_periods() {
        let system = spinning_system();
        let cal = calendar_of(&system);
        let synodic = cal.synodic_month(0).unwrap().get();
        let t = StdDays::new(synodic * 1.5).unwrap();
        assert!((cal.moon_phase(t, 0).unwrap() - 0.5).abs() < 1e-9);
        let months = cal.months_per_year(0).unwrap();
        assert!((months - cal.year_length().get() / synodic).abs() < 1e-12);
        assert!(cal.moon_phase(t, 5).is_none());
    }

    /// Luna-like check: 27.32 d sidereal in a 365.25 d year → 29.53 d synodic.
    #[test]
    fn synodic_month_matches_the_luna_check_value() {
        let cal = calendar_with_moon(27.32, 365.25);
        let synodic = cal.synodic_month(0).unwrap().0;
        assert!((synodic - 29.5306).abs() < 0.01, "got {synodic}");
        assert!((cal.months_per_year(0).unwrap() - 365.25 / synodic).abs() < 1e-12);
    }

    /// Illumination phase cycles on the synodic period, not the sidereal.
    #[test]
    fn moon_phase_cycles_on_the_synodic_period() {
        let cal = calendar_with_moon(27.32, 365.25);
        let synodic = cal.synodic_month(0).unwrap().0;
        assert!((cal.moon_phase(StdDays(synodic * 0.5), 0).unwrap() - 0.5).abs() < 1e-9);
        assert!(cal.moon_phase(StdDays(synodic), 0).unwrap() < 1e-9);
    }

    /// A sidereal period at or beyond the year is degenerate: no synodic cycle.
    #[test]
    fn synodic_month_guards_the_degenerate_case() {
        let cal = calendar_with_moon(400.0, 365.25);
        assert!(cal.synodic_month(0).is_none());
        assert!(cal.moon_phase(StdDays(1.0), 0).is_none());
        assert!(cal.months_per_year(0).is_none());
    }
}

/// A world's cycles, derived once from its star system.
#[derive(Debug, Clone, PartialEq)]
pub struct Calendar {
    day: Option<StdDays>,
    year: StdDays,
    obliquity: Degrees,
    moon_periods: Vec<StdDays>,
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
        Some((local as u64, local.fract()))
    }
    /// Fraction of the year elapsed at `t`.
    pub fn year_phase(&self, t: StdDays) -> f64 {
        (t.0 / self.year.0).fract()
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
    /// The synodic month of moon `index` — the illumination cycle seen from
    /// the anchor: `P_syn = P_sid × Y / (Y − P_sid)` (spec §2, fixing
    /// SKY-20). `None` if the moon doesn't exist or `P_sid ≥ Y` (degenerate:
    /// the moon never laps the sun).
    pub fn synodic_month(&self, index: usize) -> Option<StdDays> {
        let sidereal = self.moon_periods.get(index)?;
        if sidereal.0 >= self.year.0 {
            return None;
        }
        Some(StdDays(
            sidereal.0 * self.year.0 / (self.year.0 - sidereal.0),
        ))
    }
    /// Illumination phase of moon `index` at `t` (0 = new, 0.5 = full),
    /// cycling on the synodic month.
    pub fn moon_phase(&self, t: StdDays, index: usize) -> Option<f64> {
        let synodic = self.synodic_month(index)?;
        Some((t.0 / synodic.0).fract())
    }
    /// How many synodic months of moon `index` fit in a year.
    pub fn months_per_year(&self, index: usize) -> Option<f64> {
        let synodic = self.synodic_month(index)?;
        Some(self.year.0 / synodic.0)
    }
}

//! The calendar layer: translate absolute standard days into a world's own
//! cycles (spec §6, two clocks). Worlds without a cycle have calendars
//! without that column — truthfully.

use crate::anchor::Rotation;
use crate::sky_position::{EquatorialCoord, ecliptic_of, equatorial_at};
use crate::system::StarSystem;
use crate::units::StdDays;
use hornvale_kernel::math;

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

    /// A minimal calendar with one moon at the given sidereal period, in a
    /// year of the given length (both in standard days) — for exercising
    /// the sidereal/synodic conversion at exact, hand-checkable values.
    /// Forcing is zeroed (no tilt, no drift, no phase offset), so the moon's
    /// illumination phase is a bare synodic cycle with day 0 at phase 0.
    fn calendar_with_moon(sidereal_days: f64, year_days: f64) -> Calendar {
        Calendar {
            day: None,
            year: StdDays::new(year_days).unwrap(),
            moon_periods: vec![StdDays::new(sidereal_days).unwrap()],
            forcing: crate::forcing::OrbitalForcing {
                obliquity_mean: 0.0,
                obliquity_amp: 0.0,
                obliquity_phase: 0.0,
                ecc_mean: 0.0,
                ecc_amp: 0.0,
                ecc_phase: 0.0,
                precession_phase: 0.0,
                year_phase_offset: 0.0,
                day_phase_offset: 0.0,
                moon_phase_offsets: vec![0.0],
            },
            retrograde: false,
        }
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
            retrograde: false,
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
    fn moon_phase_and_months_derive_from_synodic_periods() {
        // SKY-20 (synodic illumination cycle) composed with SKY-4 (genesis
        // phase offset): the phase cycles on the synodic month, and half a
        // synodic period advances it by 0.5 — the offset shifts where day 0
        // sits, not the rate, so assert the advance, not the absolute phase.
        let system = spinning_system();
        let cal = calendar_of(&system);
        let synodic = cal.synodic_month(0).unwrap().get();
        let t0 = StdDays::new(0.0).unwrap();
        let t = StdDays::new(synodic * 1.5).unwrap();
        let advance =
            (cal.moon_phase(t, 0).unwrap() - cal.moon_phase(t0, 0).unwrap()).rem_euclid(1.0);
        assert!((advance - 0.5).abs() < 1e-9);
        let months = cal.months_per_year(0).unwrap();
        assert!((months - cal.year_length().get() / synodic).abs() < 1e-12);
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

    /// SKY-7: the sub-solar latitude swings with the (time-varying)
    /// obliquity over the year — zero at the equinoxes, ±ε at the solstices.
    #[test]
    fn solar_declination_swings_with_the_obliquity() {
        let pins = SkyPins {
            rotation: Some(RotationPin::PeriodHours(24.0)),
            obliquity: Some(Degrees::new(23.5).unwrap()),
            forcing: Some(crate::pins::ForcingPin::Zero),
            ..SkyPins::default()
        };
        let cal = calendar_of(&generate(Seed(42), &pins).unwrap().system);
        let year = cal.year_length().get();
        // The genesis year-phase offset (SKY-4) shifts which absolute day
        // each year phase falls on — solve for it, as the daylight tests do.
        let at_phase = |p: f64| StdDays((p - cal.forcing.year_phase_offset).rem_euclid(1.0) * year);
        assert!(cal.solar_declination(at_phase(0.0)).abs() < 1e-6);
        assert!((cal.solar_declination(at_phase(0.25)) - 23.5).abs() < 1e-6);
        assert!((cal.solar_declination(at_phase(0.75)) + 23.5).abs() < 1e-6);
    }

    /// SKY-7: on a zero-tilt world the equinoctial geometry is exact —
    /// noon at the equator puts the sun at the zenith, latitude subtracts
    /// straight off, and midnight puts it at the nadir.
    #[test]
    fn solar_altitude_runs_zenith_to_nadir_on_a_zero_tilt_equator() {
        let pins = SkyPins {
            rotation: Some(RotationPin::PeriodHours(24.0)),
            obliquity: Some(Degrees::new(0.0).unwrap()),
            forcing: Some(crate::pins::ForcingPin::Zero),
            ..SkyPins::default()
        };
        let cal = calendar_of(&generate(Seed(42), &pins).unwrap().system);
        // The genesis day-phase offset (SKY-4) shifts where in absolute
        // time noon falls — solve for the local-day fraction wanted.
        let at_fraction =
            |f: f64| StdDays(10.0 + (f - cal.forcing.day_phase_offset).rem_euclid(1.0));
        let alt = |f: f64, lat: f64| cal.solar_altitude_at(at_fraction(f), lat).unwrap();
        assert!((alt(0.5, 0.0) - 90.0).abs() < 1e-6, "noon zenith");
        assert!((alt(0.5, 40.0) - 50.0).abs() < 1e-6, "latitude subtracts");
        assert!((alt(0.0, 0.0) + 90.0).abs() < 1e-6, "midnight nadir");
        assert!(alt(0.25, 0.0).abs() < 1e-6, "sunrise on the horizon");
    }

    /// SKY-7 × SKY-22: the sun crosses east → west; a retrograde world
    /// mirrors that, west → east.
    #[test]
    fn solar_azimuth_runs_east_to_west_and_retrograde_mirrors_it() {
        let cal_with_spin = |spin| {
            let pins = SkyPins {
                rotation: Some(RotationPin::PeriodHours(24.0)),
                obliquity: Some(Degrees::new(0.0).unwrap()),
                forcing: Some(crate::pins::ForcingPin::Zero),
                spin: Some(spin),
                ..SkyPins::default()
            };
            calendar_of(&generate(Seed(42), &pins).unwrap().system)
        };
        let pro = cal_with_spin(crate::pins::SpinPin::Prograde);
        // The spin pin draws from its own stream, so both calendars share
        // the same genesis day-phase offset; solve for local fractions.
        let at_fraction =
            |f: f64| StdDays(10.0 + (f - pro.forcing.day_phase_offset).rem_euclid(1.0));
        let morning = pro.solar_azimuth_at(at_fraction(0.3), 0.0).unwrap();
        let evening = pro.solar_azimuth_at(at_fraction(0.7), 0.0).unwrap();
        assert!(
            (45.0..135.0).contains(&morning),
            "morning east, got {morning}"
        );
        assert!(
            (225.0..315.0).contains(&evening),
            "evening west, got {evening}"
        );

        let retro = cal_with_spin(crate::pins::SpinPin::Retrograde);
        let morning = retro.solar_azimuth_at(at_fraction(0.3), 0.0).unwrap();
        assert!(
            (225.0..315.0).contains(&morning),
            "retrograde morning west, got {morning}"
        );
    }

    /// SKY-7: a locked world has no hour angle — no solar position.
    #[test]
    fn locked_worlds_have_no_solar_position() {
        let cal = calendar_of(&locked_system());
        assert!(cal.solar_altitude_at(StdDays(5.0), 30.0).is_none());
        assert!(cal.solar_azimuth_at(StdDays(5.0), 30.0).is_none());
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
    /// (Zero phase offset in `calendar_with_moon`, so day 0 sits at phase 0.)
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

    /// The exact solar position agrees with the shipped small-angle declination
    /// at the equinoxes and solstices, and its RA advances a full turn per year.
    #[test]
    fn solar_equatorial_agrees_at_the_cardinal_phases() {
        let pins = SkyPins {
            rotation: Some(RotationPin::PeriodHours(24.0)),
            obliquity: Some(Degrees::new(23.5).unwrap()),
            forcing: Some(crate::pins::ForcingPin::Zero),
            ..SkyPins::default()
        };
        let cal = calendar_of(&generate(Seed(42), &pins).unwrap().system);
        let year = cal.year_length().get();
        let at_phase = |p: f64| StdDays((p - cal.forcing.year_phase_offset).rem_euclid(1.0) * year);
        assert!(cal.solar_equatorial(at_phase(0.0)).dec_deg.abs() < 1e-6);
        assert!((cal.solar_equatorial(at_phase(0.25)).dec_deg - 23.5).abs() < 1e-6);
        assert!((cal.solar_equatorial(at_phase(0.25)).ra_deg - 90.0).abs() < 1e-6);
        assert!((cal.solar_equatorial(at_phase(0.75)).dec_deg + 23.5).abs() < 1e-6);
    }

    /// SKY-stale-alignments (sky half): precession finally has a reader — a
    /// star's apparent position drifts between epochs kiloyears apart.
    #[test]
    fn star_positions_drift_under_precession() {
        let cal = calendar_of(&spinning_system());
        let genesis = crate::sky_position::EquatorialCoord {
            ra_deg: 10.0,
            dec_deg: 40.0,
        };
        let now = cal.star_equatorial_at(&genesis, StdDays(0.0));
        let later = cal.star_equatorial_at(&genesis, StdDays(crate::forcing::P_PRECESSION / 4.0));
        assert!(
            (now.ra_deg - genesis.ra_deg).abs() < 1e-9,
            "epoch 0 is the genesis frame"
        );
        assert!(
            (later.ra_deg - now.ra_deg).abs() > 1.0,
            "RA must drift over kiloyears"
        );
    }

    /// The shared twilight thresholds (spec §2): Day above the horizon,
    /// Twilight to −12°, Night below. Locked worlds have no band.
    #[test]
    fn sky_band_partitions_the_day_by_solar_altitude() {
        let pins = SkyPins {
            rotation: Some(RotationPin::PeriodHours(24.0)),
            obliquity: Some(Degrees::new(0.0).unwrap()),
            forcing: Some(crate::pins::ForcingPin::Zero),
            ..SkyPins::default()
        };
        let cal = calendar_of(&generate(Seed(42), &pins).unwrap().system);
        let at_fraction =
            |f: f64| StdDays(10.0 + (f - cal.forcing.day_phase_offset).rem_euclid(1.0));
        assert_eq!(cal.sky_band(at_fraction(0.5), 0.0), Some(SkyBand::Day));
        assert_eq!(cal.sky_band(at_fraction(0.0), 0.0), Some(SkyBand::Night));
        // Just past sunset (fraction 0.76 ≈ sun ~3.6° below on a zero-tilt equator).
        assert_eq!(
            cal.sky_band(at_fraction(0.76), 0.0),
            Some(SkyBand::Twilight)
        );
        assert!(
            calendar_of(&locked_system())
                .sky_band(StdDays(5.0), 0.0)
                .is_none()
        );
    }
}

/// The sky's brightness band at a placed moment — the one shared twilight
/// definition (spec §2): heliacal visibility, the morning/evening star, and
/// the prose renderer all read this, none owns it.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SkyBand {
    /// The sun is above the horizon.
    Day,
    /// The sun is below the horizon but within `TWILIGHT_DEPTH_DEG` of it.
    Twilight,
    /// Full dark: the sun is deeper than the twilight band.
    Night,
}

/// How far below the horizon the sun still lights the sky, degrees
/// (model card; the classical astronomical-twilight midpoint).
/// type-audit: pending(wave-1)
pub const TWILIGHT_DEPTH_DEG: f64 = 12.0;

/// A world's cycles, derived once from its star system.
#[derive(Debug, Clone, PartialEq)]
pub struct Calendar {
    day: Option<StdDays>,
    year: StdDays,
    moon_periods: Vec<StdDays>,
    forcing: crate::forcing::OrbitalForcing,
    retrograde: bool,
}

/// Derive the calendar from a generated system.
pub fn calendar_of(system: &StarSystem) -> Calendar {
    let (day, retrograde) = match system.anchor.rotation {
        Rotation::Spinning { day, retrograde } => (Some(day), retrograde),
        Rotation::Locked => (None, false),
    };
    Calendar {
        day,
        year: system.anchor.year,
        moon_periods: system.moons.iter().map(|m| m.period).collect(),
        forcing: system.forcing.clone(),
        retrograde,
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
    /// type-audit: bare-ok(ratio)
    pub fn local_day(&self, t: StdDays) -> Option<(u64, f64)> {
        let day = self.day?;
        let local = t.0 / day.0;
        let fraction = (local.fract() + self.forcing.day_phase_offset).fract();
        Some((local as u64, fraction))
    }
    /// Fraction of the year elapsed at `t`.
    /// type-audit: bare-ok(ratio)
    pub fn year_phase(&self, t: StdDays) -> f64 {
        (t.0 / self.year.0 + self.forcing.year_phase_offset).fract()
    }
    /// Seasonal phase; present when either driver (tilt or eccentricity) acts.
    /// type-audit: bare-ok(ratio)
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
    /// type-audit: bare-ok(ratio)
    pub fn daylight_fraction(&self, t: StdDays) -> Option<f64> {
        self.day?;
        let obliquity = self.forcing.obliquity_at(t.0);
        let ecc = self.forcing.eccentricity_at(t.0);
        let phase = self.year_phase(t);
        let tilt_term = (obliquity / 90.0) * 0.5 * math::sin(std::f64::consts::TAU * phase);
        let apsidal_term = ecc * 0.5 * math::sin(std::f64::consts::TAU * phase);
        Some((0.5 + tilt_term + apsidal_term).clamp(0.0, 1.0))
    }
    /// Daylight fraction of the local day at a latitude (SKY-8): the standard
    /// sunrise equation. ~0.5 flat at the equator, running to 1.0/0.0 (polar
    /// day/night) toward the poles. Reads the time-varying obliquity, so a
    /// world's daylight geometry drifts with its axial tilt. `None` on a
    /// tidally locked world, which has no day/night cycle.
    /// type-audit: pending(wave-1: latitude), bare-ok(ratio: return)
    pub fn daylight_fraction_at(&self, t: StdDays, latitude: f64) -> Option<f64> {
        self.day?;
        // Solar declination: the sub-solar latitude oscillates over the year,
        // its amplitude the (time-varying) obliquity.
        let obliquity = self.forcing.obliquity_at(t.0).to_radians();
        let declination = obliquity * math::sin(std::f64::consts::TAU * self.year_phase(t));
        let phi = latitude.to_radians();
        // cos H0 = −tan φ · tan δ; the clamp yields polar day (−1 → H0 = π,
        // fraction 1) and polar night (1 → H0 = 0, fraction 0) past the polar
        // circles.
        let cos_h0 = (-math::tan(phi) * math::tan(declination)).clamp(-1.0, 1.0);
        Some(math::acos(cos_h0) / std::f64::consts::PI)
    }
    /// The sub-solar latitude at `t`, in degrees (SKY-7): the solar
    /// declination, oscillating over the year with amplitude the
    /// (time-varying) obliquity — the same declination the latitude
    /// daylight model already uses.
    /// type-audit: pending(wave-1)
    pub fn solar_declination(&self, t: StdDays) -> f64 {
        self.forcing.obliquity_at(t.0) * math::sin(std::f64::consts::TAU * self.year_phase(t))
    }

    /// Hour angle (radians, 0 at local solar noon), declination and
    /// latitude (radians) for the solar-position formulas. A retrograde
    /// world (SKY-22) runs its hour angle backward. `None` on a locked
    /// world, which has no hour.
    fn solar_geometry(&self, t: StdDays, latitude: f64) -> Option<(f64, f64, f64)> {
        let fraction = self.local_day(t)?.1;
        let direction = if self.retrograde { -1.0 } else { 1.0 };
        let hour_angle = std::f64::consts::TAU * (fraction - 0.5) * direction;
        Some((
            hour_angle,
            self.solar_declination(t).to_radians(),
            latitude.to_radians(),
        ))
    }

    /// The sun's altitude above the horizon at `t` for an observer at
    /// `latitude`, in degrees (SKY-7): sin a = sin φ sin δ + cos φ cos δ
    /// cos H. Negative below the horizon. `None` on a locked world.
    /// type-audit: pending(wave-1)
    pub fn solar_altitude_at(&self, t: StdDays, latitude: f64) -> Option<f64> {
        let (h, delta, phi) = self.solar_geometry(t, latitude)?;
        Some(
            math::asin(
                math::sin(phi) * math::sin(delta)
                    + math::cos(phi) * math::cos(delta) * math::cos(h),
            )
            .to_degrees(),
        )
    }

    /// The sun's azimuth at `t` for an observer at `latitude`, in degrees
    /// clockwise from north (90 = east) — SKY-7. The sun crosses east to
    /// west; a retrograde world (SKY-22) mirrors the crossing. `None` on
    /// a locked world.
    /// type-audit: pending(wave-1)
    pub fn solar_azimuth_at(&self, t: StdDays, latitude: f64) -> Option<f64> {
        let (h, delta, phi) = self.solar_geometry(t, latitude)?;
        // atan2 form measured from south, westward positive; shift to
        // compass convention.
        let from_south = math::atan2(
            math::sin(h),
            math::cos(h) * math::sin(phi) - math::tan(delta) * math::cos(phi),
        );
        Some((from_south.to_degrees() + 180.0).rem_euclid(360.0))
    }

    /// Is it daylight at `t`? Daylight is a centered window of the local day.
    /// type-audit: bare-ok(flag)
    pub fn is_daylight(&self, t: StdDays) -> Option<bool> {
        let fraction = self.local_day(t)?.1;
        let f = self.daylight_fraction(t)?;
        Some(fraction > (1.0 - f) / 2.0 && fraction < (1.0 + f) / 2.0)
    }
    /// The synodic month of moon `index` — the illumination cycle seen from
    /// the anchor: `P_syn = P_sid × Y / (Y − P_sid)` (spec §2, fixing
    /// SKY-20). `None` if the moon doesn't exist or `P_sid ≥ Y` (degenerate:
    /// the moon never laps the sun).
    /// type-audit: bare-ok(index)
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
    /// cycling on the synodic month (SKY-20) and shifted by the genesis phase
    /// offset (SKY-4) so day 0 is an ordinary day, not a grand alignment.
    /// type-audit: bare-ok(index: index), bare-ok(ratio: return)
    pub fn moon_phase(&self, t: StdDays, index: usize) -> Option<f64> {
        let synodic = self.synodic_month(index)?;
        let offset = self
            .forcing
            .moon_phase_offsets
            .get(index)
            .copied()
            .unwrap_or(0.0);
        Some((t.0 / synodic.0 + offset).fract())
    }
    /// How many synodic months of moon `index` fit in a year.
    /// type-audit: bare-ok(index: index), bare-ok(ratio: return)
    pub fn months_per_year(&self, index: usize) -> Option<f64> {
        let synodic = self.synodic_month(index)?;
        Some(self.year.0 / synodic.0)
    }

    /// Degrees the equinox has precessed since genesis (epoch 0) — the first
    /// reader `precession_at` has ever had.
    /// type-audit: pending(wave-1)
    pub fn precession_offset_deg(&self, t: StdDays) -> f64 {
        (self.forcing.precession_at(t.0) - self.forcing.precession_at(0.0)).to_degrees()
    }

    /// The sun's equatorial position at `t` (exact spherical form; the shipped
    /// small-angle `solar_declination` is the coarse tier of the same object).
    pub fn solar_equatorial(&self, t: StdDays) -> EquatorialCoord {
        let lam = (360.0 * self.year_phase(t)).to_radians();
        let e = self.forcing.obliquity_at(t.0).to_radians();
        EquatorialCoord {
            ra_deg: math::atan2(math::sin(lam) * math::cos(e), math::cos(lam))
                .to_degrees()
                .rem_euclid(360.0),
            dec_deg: math::asin(math::sin(e) * math::sin(lam)).to_degrees(),
        }
    }

    /// A fixed star's apparent equatorial position at `t`: genesis coordinates
    /// through the genesis ecliptic, drifted by precession, re-projected at the
    /// epoch's obliquity.
    pub fn star_equatorial_at(&self, genesis: &EquatorialCoord, t: StdDays) -> EquatorialCoord {
        let ecl = ecliptic_of(genesis, self.forcing.obliquity_at(0.0));
        equatorial_at(
            &ecl,
            self.forcing.obliquity_at(t.0),
            self.precession_offset_deg(t),
        )
    }

    /// The sky band at `t` for an observer at `latitude`; `None` on a
    /// locked world, which has no solar hour.
    /// type-audit: pending(wave-1: latitude)
    pub fn sky_band(&self, t: StdDays, latitude: f64) -> Option<SkyBand> {
        let alt = self.solar_altitude_at(t, latitude)?;
        Some(if alt > 0.0 {
            SkyBand::Day
        } else if alt > -TWILIGHT_DEPTH_DEG {
            SkyBand::Twilight
        } else {
            SkyBand::Night
        })
    }
    /// Whether the world spins retrograde (west to east).
    /// type-audit: bare-ok(flag)
    pub fn is_retrograde(&self) -> bool {
        self.retrograde
    }
}

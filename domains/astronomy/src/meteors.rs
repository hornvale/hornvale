//! Fixed annual debris encounters derived from persistent comet orbits.
#![allow(missing_docs)]

use crate::StarSystem;
use crate::calendar::calendar_of;
use crate::comets::Comet;
use crate::ephemeris::{anchor_orbital_state_at, orbital_state_at};
use crate::units::{Au, Degrees, StdDays, StdInstant};
use hornvale_kernel::math;

/// plumb: pending(wave-1)
const SEASON_FRACTION: f64 = 0.05;

/// type-audit: pending(wave-1: density), pending(wave-1: relative_velocity_au_per_day)
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct DebrisStream {
    pub comet: crate::CometId,
    pub width: Au,
    /// type-audit: pending(wave-1)
    pub density: f64,
    pub radiant: SkyRadiant,
    /// type-audit: pending(wave-1)
    pub relative_velocity_au_per_day: [f64; 3],
    pub epoch: StdInstant,
    pub validity: (StdInstant, StdInstant),
}

/// type-audit: pending(wave-1: lon_deg), pending(wave-1: lat_deg)
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct SkyRadiant {
    /// type-audit: pending(wave-1)
    pub lon_deg: f64,
    /// type-audit: pending(wave-1)
    pub lat_deg: f64,
}

/// type-audit: pending(wave-1: daylight), pending(wave-1: atmospheric_attenuation), pending(wave-1: moonlight)
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct MeteorObservation {
    pub latitude: Degrees,
    pub longitude: Degrees,
    /// type-audit: pending(wave-1)
    pub daylight: f64,
    pub horizon: Degrees,
    /// type-audit: pending(wave-1)
    pub atmospheric_attenuation: f64,
    /// type-audit: pending(wave-1)
    pub moonlight: f64,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MeteorAbsence {
    InvalidInstant,
    InvalidObserver,
    InvalidOrbit,
    InvalidStream,
    OutsideValidity,
    OutsideSeason,
    NoCrossing,
    BelowThreshold,
    BelowHorizon,
    Daylight,
    Atmosphere,
    Moonlight,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MeteorVisibility {
    Visible,
    Absent(MeteorAbsence),
}

/// type-audit: pending(wave-1: relative_velocity_au_per_day), pending(wave-1: rate_per_hour)
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct MeteorShower {
    pub comet: crate::CometId,
    pub peak: StdInstant,
    pub duration: StdDays,
    pub radiant: SkyRadiant,
    pub altitude: Degrees,
    /// type-audit: pending(wave-1)
    pub relative_velocity_au_per_day: [f64; 3],
    /// type-audit: pending(wave-1)
    pub rate_per_hour: f64,
    pub visibility: MeteorVisibility,
}

/// type-audit: pending(wave-1: density)
pub fn debris_stream_from_comet(
    system: &StarSystem,
    comet: &Comet,
    width: Au,
    density: f64,
) -> Result<DebrisStream, MeteorAbsence> {
    if !width.get().is_finite()
        || width.get() <= 0.0
        || width.get() > 1.0
        || !density.is_finite()
        || density < 0.0
    {
        return Err(MeteorAbsence::InvalidStream);
    }
    if !comet.period.0.is_finite()
        || comet.period.0 <= 0.0
        || !comet.semi_major_axis.get().is_finite()
        || comet.semi_major_axis.get() <= 0.0
    {
        return Err(MeteorAbsence::InvalidOrbit);
    }
    if !comet.inclination_deg.is_finite() || !comet.ascending_node_deg.is_finite() {
        return Err(MeteorAbsence::InvalidOrbit);
    }
    let comet_state = orbital_state_at(&comet.orbital_elements(), comet.perihelion_epoch)
        .ok_or(MeteorAbsence::InvalidOrbit)?;
    let anchor_state = anchor_orbital_state_at(
        system.anchor.orbit,
        system.anchor.year,
        &system.forcing,
        comet.perihelion_epoch,
    )
    .ok_or(MeteorAbsence::InvalidOrbit)?;
    let pos = comet.orient_vector(comet_state.position);
    let anchor_radius = system.anchor.orbit.get();
    let position_radius = (pos[0] * pos[0] + pos[1] * pos[1] + pos[2] * pos[2]).sqrt();
    if (position_radius - anchor_radius).abs() > width.get() * 2.0 {
        return Err(MeteorAbsence::NoCrossing);
    }
    let vel = comet.orient_vector(comet_state.velocity_per_day);
    let relative = [
        vel[0] - anchor_state.velocity_per_day[0],
        vel[1] - anchor_state.velocity_per_day[1],
        vel[2],
    ];
    let speed = relative.iter().map(|v| v * v).sum::<f64>().sqrt();
    if !speed.is_finite() || speed <= 0.0 {
        return Err(MeteorAbsence::NoCrossing);
    }
    let radiant = SkyRadiant {
        lon_deg: math::atan2(-relative[1], -relative[0])
            .to_degrees()
            .rem_euclid(360.0),
        lat_deg: math::asin((-relative[2] / speed).clamp(-1.0, 1.0)).to_degrees(),
    };
    let _ = pos;
    Ok(DebrisStream {
        comet: comet.id,
        width,
        density,
        radiant,
        relative_velocity_au_per_day: relative,
        epoch: comet.perihelion_epoch,
        validity: (
            StdInstant(comet.perihelion_epoch.get() - crate::comets::COMET_VALIDITY_DAYS),
            StdInstant(comet.perihelion_epoch.get() + crate::comets::COMET_VALIDITY_DAYS),
        ),
    })
}

pub fn meteor_shower_at(
    system: &StarSystem,
    stream: &DebrisStream,
    instant: StdInstant,
    observer: MeteorObservation,
) -> Result<MeteorShower, MeteorAbsence> {
    if !instant.get().is_finite() {
        return Err(MeteorAbsence::InvalidInstant);
    }
    let finite = [
        observer.latitude.get(),
        observer.longitude.get(),
        observer.horizon.get(),
        observer.daylight,
        observer.atmospheric_attenuation,
        observer.moonlight,
    ]
    .iter()
    .all(|v| v.is_finite());
    if !finite || observer.latitude.get().abs() > 90.0 {
        return Err(MeteorAbsence::InvalidObserver);
    }
    if instant < stream.validity.0 || instant > stream.validity.1 {
        return Err(MeteorAbsence::OutsideValidity);
    }
    let year = system.anchor.year.0;
    if !year.is_finite() || year <= 0.0 {
        return Err(MeteorAbsence::InvalidOrbit);
    }
    let phase = ((instant.get() - stream.epoch.get()).rem_euclid(year)) / year;
    let delta = phase.min(1.0 - phase);
    let peak = StdInstant(instant.get() - delta * year * if phase <= 0.5 { 1.0 } else { -1.0 });
    let hour = (instant.get() / system.anchor.year.0 * std::f64::consts::TAU
        + (observer.longitude.get() - 90.0).to_radians())
    .rem_euclid(std::f64::consts::TAU);
    let altitude = math::asin(
        (math::sin(observer.latitude.get().to_radians())
            * math::sin(stream.radiant.lat_deg.to_radians())
            + math::cos(observer.latitude.get().to_radians())
                * math::cos(stream.radiant.lat_deg.to_radians())
                * math::cos(hour))
        .clamp(-1.0, 1.0),
    )
    .to_degrees();
    let attenuation = (1.0 - observer.atmospheric_attenuation.clamp(0.0, 1.0))
        * (1.0 - observer.moonlight.clamp(0.0, 1.0));
    let base = MeteorShower {
        comet: stream.comet,
        peak,
        duration: StdDays(
            2.0 * stream.width.get()
                / (stream.relative_velocity_au_per_day[0] * stream.relative_velocity_au_per_day[0]
                    + stream.relative_velocity_au_per_day[1]
                        * stream.relative_velocity_au_per_day[1])
                    .sqrt(),
        ),
        radiant: stream.radiant,
        altitude: Degrees(altitude),
        relative_velocity_au_per_day: stream.relative_velocity_au_per_day,
        rate_per_hour: stream.density
            * 100.0
            * math::sin(altitude.to_radians()).max(0.0)
            * attenuation,
        visibility: MeteorVisibility::Visible,
    };
    if delta > SEASON_FRACTION {
        return Err(MeteorAbsence::OutsideSeason);
    }
    let calendar = calendar_of(system);
    let reason = if altitude <= observer.horizon.get() {
        Some(MeteorAbsence::BelowHorizon)
    } else if observer.daylight >= 1.0
        || calendar
            .solar_altitude_at(instant, observer.latitude.get())
            .is_some_and(|sun| sun > 0.0)
    {
        Some(MeteorAbsence::Daylight)
    } else if observer.atmospheric_attenuation >= 1.0 {
        Some(MeteorAbsence::Atmosphere)
    } else if observer.moonlight >= 1.0 {
        Some(MeteorAbsence::Moonlight)
    } else if stream.density <= 0.0 {
        Some(MeteorAbsence::BelowThreshold)
    } else {
        None
    };
    reason.map_or(Ok(base), |reason| {
        Ok(MeteorShower {
            rate_per_hour: 0.0,
            visibility: MeteorVisibility::Absent(reason),
            ..base
        })
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{CometId, Rotation, SkyPins, generate};
    use hornvale_kernel::{Seed, units::TickSpan};

    fn fixture() -> (StarSystem, Comet) {
        let mut system = generate(Seed(42), &SkyPins::default()).unwrap().value;
        system.anchor.orbit = Au(1.0);
        system.anchor.year = StdDays(400.0);
        system.anchor.rotation = Rotation::Spinning {
            day: TickSpan::from_std_days(1.0).unwrap(),
            retrograde: false,
        };
        system.forcing.ecc_mean = 0.0;
        system.forcing.ecc_amp = 0.0;
        system.forcing.year_phase_offset = 0.0;
        system.forcing.day_phase_offset = 0.0;
        system.forcing.obliquity_mean = 0.0;
        system.forcing.obliquity_amp = 0.0;
        // Perihelion at (1,0,0), moving south through the anchor plane.
        // At the crossing v_anchor=(0,tau/400,0), v_stream=(0,0,-tau*sqrt(3)/800).
        let comet = Comet {
            id: CometId(17),
            semi_major_axis: Au(2.0),
            eccentricity: 0.5,
            inclination_deg: 270.0,
            ascending_node_deg: 0.0,
            periapsis_longitude_deg: 0.0,
            perihelion_epoch: StdInstant(0.0),
            period: StdDays(1600.0),
            baseline_activity: 1.0,
            return_variation: 0.3,
            nucleus_radius_km: 2.0,
            albedo: 0.04,
            absolute_magnitude: 8.0,
            activity_distance_exponent: 3.0,
        };
        (system, comet)
    }

    fn observer() -> MeteorObservation {
        MeteorObservation {
            latitude: Degrees(60.0),
            longitude: Degrees(90.0),
            daylight: 0.0,
            horizon: Degrees(0.0),
            atmospheric_attenuation: 0.0,
            moonlight: 0.0,
        }
    }

    fn stream(system: &StarSystem, comet: &Comet) -> DebrisStream {
        debris_stream_from_comet(system, comet, Au(0.02), 100_000.0).unwrap()
    }

    #[test]
    fn recurs_each_anchor_year_without_waiting_for_comet_return() {
        let (system, comet) = fixture();
        let original = system.clone();
        let stream = stream(&system, &comet);
        let first = meteor_shower_at(&system, &stream, StdInstant(0.0), observer()).unwrap();
        assert_eq!(first.visibility, MeteorVisibility::Visible);
        for year in [-3.0, -1.0, 1.0, 11.0] {
            let at = StdInstant(year * 400.0);
            let next = meteor_shower_at(&system, &stream, at, observer()).unwrap();
            assert_eq!(next.radiant, first.radiant);
            assert_eq!(
                next.relative_velocity_au_per_day,
                first.relative_velocity_au_per_day
            );
            assert!((next.rate_per_hour - first.rate_per_hour).abs() < 1e-10);
            assert_eq!(next.peak, at);
            assert_eq!(next.duration, first.duration);
            assert_eq!(Ok(next), meteor_shower_at(&system, &stream, at, observer()));
        }
        assert_eq!(system, original);
    }

    #[test]
    fn radiant_opposes_relative_velocity_including_anchor_motion() {
        let (system, comet) = fixture();
        let result = meteor_shower_at(
            &system,
            &stream(&system, &comet),
            StdInstant(0.0),
            observer(),
        )
        .unwrap();
        assert!(result.relative_velocity_au_per_day[0].abs() < 1e-12);
        assert!(
            (result.relative_velocity_au_per_day[1] + std::f64::consts::TAU / 400.0).abs() < 1e-12
        );
        assert!(
            (result.relative_velocity_au_per_day[2]
                + std::f64::consts::TAU * 3.0_f64.sqrt() / 800.0)
                .abs()
                < 1e-12
        );
        assert!((result.radiant.lon_deg - 90.0).abs() < 1e-10);
        assert!((result.radiant.lat_deg - 40.893394649130904).abs() < 1e-10);
        // A diameter of 0.04 AU crossed at tau/400 AU/day.
        assert!((result.duration.get() - 16.0 / std::f64::consts::TAU).abs() < 1e-10);
    }

    #[test]
    fn hemispheres_and_longitudes_share_encounter_but_change_visibility() {
        let (system, comet) = fixture();
        let stream = stream(&system, &comet);
        let north = meteor_shower_at(&system, &stream, StdInstant(0.0), observer()).unwrap();
        let south = meteor_shower_at(
            &system,
            &stream,
            StdInstant(0.0),
            MeteorObservation {
                latitude: Degrees(-60.0),
                ..observer()
            },
        )
        .unwrap();
        assert_eq!(north.radiant, south.radiant);
        assert_eq!(north.duration, south.duration);
        assert_eq!(
            south.visibility,
            MeteorVisibility::Absent(MeteorAbsence::BelowHorizon)
        );
        assert_eq!(south.rate_per_hour, 0.0);
        let west = meteor_shower_at(
            &system,
            &stream,
            StdInstant(0.0),
            MeteorObservation {
                latitude: Degrees(0.0),
                longitude: Degrees(-90.0),
                ..observer()
            },
        )
        .unwrap();
        assert!(west.altitude.get() < 0.0);
        let wrapped = meteor_shower_at(
            &system,
            &stream,
            StdInstant(0.0),
            MeteorObservation {
                longitude: Degrees(810.0),
                ..observer()
            },
        )
        .unwrap();
        assert_eq!(wrapped, north);
    }

    #[test]
    fn daylight_horizon_atmosphere_and_moonlight_gate_and_attenuate_rates() {
        let (system, comet) = fixture();
        let stream = stream(&system, &comet);
        let clear = meteor_shower_at(&system, &stream, StdInstant(0.0), observer()).unwrap();
        for (observation, reason) in [
            (
                MeteorObservation {
                    daylight: 1.0,
                    ..observer()
                },
                MeteorAbsence::Daylight,
            ),
            (
                MeteorObservation {
                    horizon: Degrees(90.0),
                    ..observer()
                },
                MeteorAbsence::BelowHorizon,
            ),
            (
                MeteorObservation {
                    atmospheric_attenuation: 1.0,
                    ..observer()
                },
                MeteorAbsence::Atmosphere,
            ),
            (
                MeteorObservation {
                    moonlight: 1.0,
                    ..observer()
                },
                MeteorAbsence::Moonlight,
            ),
        ] {
            let hidden = meteor_shower_at(&system, &stream, StdInstant(0.0), observation).unwrap();
            assert_eq!(hidden.visibility, MeteorVisibility::Absent(reason));
            assert_eq!(hidden.rate_per_hour, 0.0);
            assert_eq!(hidden.radiant, clear.radiant);
        }
        let dim = meteor_shower_at(
            &system,
            &stream,
            StdInstant(0.0),
            MeteorObservation {
                atmospheric_attenuation: 0.5,
                moonlight: 0.5,
                ..observer()
            },
        )
        .unwrap();
        assert!((dim.rate_per_hour - clear.rate_per_hour / 4.0).abs() < 1e-10);
    }

    #[test]
    fn local_clock_and_sun_geometry_are_consumed_even_with_clear_inputs() {
        let (mut system, comet) = fixture();
        let stream = stream(&system, &comet);
        system.forcing.day_phase_offset = 0.25;
        let day = meteor_shower_at(&system, &stream, StdInstant(0.0), observer()).unwrap();
        assert_eq!(
            day.visibility,
            MeteorVisibility::Absent(MeteorAbsence::Daylight)
        );
        system.anchor.rotation = Rotation::Locked;
        let locked = meteor_shower_at(&system, &stream, StdInstant(0.0), observer()).unwrap();
        assert!(locked.altitude.get().is_finite());
    }

    #[test]
    fn validity_and_malformed_inputs_return_typed_absence() {
        let (system, comet) = fixture();
        let stream = stream(&system, &comet);
        for at in [-3_652_501.0, 3_652_501.0] {
            assert_eq!(
                meteor_shower_at(&system, &stream, StdInstant(at), observer()),
                Err(MeteorAbsence::OutsideValidity)
            );
        }
        for at in [-3_652_500.0, 3_652_500.0] {
            assert_ne!(
                meteor_shower_at(&system, &stream, StdInstant(at), observer()),
                Err(MeteorAbsence::OutsideValidity)
            );
        }
        for bad in [f64::NAN, f64::INFINITY, f64::NEG_INFINITY] {
            assert_eq!(
                meteor_shower_at(&system, &stream, StdInstant(bad), observer()),
                Err(MeteorAbsence::InvalidInstant)
            );
            for observation in [
                MeteorObservation {
                    latitude: Degrees(bad),
                    ..observer()
                },
                MeteorObservation {
                    longitude: Degrees(bad),
                    ..observer()
                },
                MeteorObservation {
                    horizon: Degrees(bad),
                    ..observer()
                },
                MeteorObservation {
                    daylight: bad,
                    ..observer()
                },
                MeteorObservation {
                    atmospheric_attenuation: bad,
                    ..observer()
                },
                MeteorObservation {
                    moonlight: bad,
                    ..observer()
                },
            ] {
                assert_eq!(
                    meteor_shower_at(&system, &stream, StdInstant(0.0), observation),
                    Err(MeteorAbsence::InvalidObserver)
                );
            }
        }
        for period in [0.0, -1.0, f64::INFINITY, f64::NAN] {
            let mut bad = comet.clone();
            bad.period = StdDays(period);
            assert_eq!(
                debris_stream_from_comet(&system, &bad, Au(0.02), 100.0),
                Err(MeteorAbsence::InvalidOrbit)
            );
        }
        for width in [0.0, -1.0, f64::INFINITY, f64::NAN, 2.0] {
            assert_eq!(
                debris_stream_from_comet(&system, &comet, Au(width), 100.0),
                Err(MeteorAbsence::InvalidStream)
            );
        }
    }

    #[test]
    fn no_crossing_off_season_and_low_density_are_deterministic_absences() {
        let (system, mut comet) = fixture();
        let stream = stream(&system, &comet);
        for _ in 0..2 {
            assert_eq!(
                meteor_shower_at(&system, &stream, StdInstant(200.0), observer()),
                Err(MeteorAbsence::OutsideSeason)
            );
            let empty = debris_stream_from_comet(&system, &comet, Au(0.02), 0.0).unwrap();
            let result = meteor_shower_at(&system, &empty, StdInstant(0.0), observer()).unwrap();
            assert_eq!(
                result.visibility,
                MeteorVisibility::Absent(MeteorAbsence::BelowThreshold)
            );
            assert_eq!(result.rate_per_hour, 0.0);
        }
        comet.semi_major_axis = Au(4.0);
        assert_eq!(
            debris_stream_from_comet(&system, &comet, Au(0.02), 100.0),
            Err(MeteorAbsence::NoCrossing)
        );
    }
}

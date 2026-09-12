//! Fixed annual debris encounters derived from persistent comet orbits.
#![allow(missing_docs)]

use crate::StarSystem;
use crate::calendar::calendar_of;
use crate::comets::Comet;
use crate::ephemeris::{anchor_body_to_frame_at, anchor_orbital_state_at, orbital_state_at};
use crate::sky_position::{EclipticCoord, equatorial_at};
use crate::units::{Au, Degrees, StdDays, StdInstant};
use hornvale_kernel::math;

/// plumb: pending(wave-1)
const CROSSING_SAMPLES: usize = 256;
/// Minimum stream density that can produce a physical shower rate.
/// plumb: pending(wave-1)
const MIN_STREAM_DENSITY: f64 = 0.001;

fn angle_difference(angle: f64, target: f64) -> f64 {
    (angle - target + std::f64::consts::PI).rem_euclid(std::f64::consts::TAU) - std::f64::consts::PI
}

fn comet_nodes(
    comet: &Comet,
    from: f64,
    to: f64,
) -> Option<Vec<(f64, crate::ephemeris::OrbitalState, [f64; 3])>> {
    let elements = comet.orbital_elements();
    let z = |t: f64| {
        let state = orbital_state_at(&elements, StdInstant(t))?;
        Some(comet.orient_vector(state.position)[2])
    };
    let mut left = from;
    let mut left_z = z(left)?;
    let mut nodes = Vec::new();
    for index in 1..=CROSSING_SAMPLES {
        let right = from + (to - from) * index as f64 / CROSSING_SAMPLES as f64;
        let right_z = z(right)?;
        if left_z == 0.0 || left_z.signum() != right_z.signum() {
            let mut lo = left;
            let mut hi = right;
            for _ in 0..64 {
                let mid = (lo + hi) / 2.0;
                let mid_z = z(mid)?;
                if left_z == 0.0 || left_z.signum() != mid_z.signum() {
                    hi = mid;
                } else {
                    lo = mid;
                    left_z = mid_z;
                }
            }
            let t = (lo + hi) / 2.0;
            let state = orbital_state_at(&elements, StdInstant(t))?;
            let position = comet.orient_vector(state.position);
            let longitude = math::atan2(position[1], position[0]);
            nodes.push((longitude, state, position));
        }
        left = right;
        left_z = right_z;
    }
    (!nodes.is_empty()).then_some(nodes)
}

fn anchor_epoch_for_node(
    system: &StarSystem,
    node_longitude: f64,
    node_radius: f64,
    width: f64,
) -> Option<(StdInstant, crate::ephemeris::OrbitalState)> {
    let year = system.anchor.year.get();
    let start = -year / 2.0;
    let state_at = |t: f64| {
        anchor_orbital_state_at(
            system.anchor.orbit,
            system.anchor.year,
            &system.forcing,
            StdInstant(t),
        )
    };
    let mut previous_t = start;
    let previous = state_at(previous_t)?;
    let mut previous_angle = math::atan2(previous.position[1], previous.position[0]);
    for index in 1..=CROSSING_SAMPLES {
        let t = start + year * index as f64 / CROSSING_SAMPLES as f64;
        let current = state_at(t)?;
        let current_angle = math::atan2(current.position[1], current.position[0]);
        let previous_unwrapped = previous_angle;
        let current_unwrapped =
            previous_unwrapped + angle_difference(current_angle, previous_angle);
        let target = node_longitude
            + std::f64::consts::TAU
                * ((previous_unwrapped - node_longitude) / std::f64::consts::TAU).round();
        if target >= previous_unwrapped.min(current_unwrapped)
            && target <= previous_unwrapped.max(current_unwrapped)
        {
            let mut lo = previous_t;
            let mut hi = t;
            let base = previous_angle;
            for _ in 0..64 {
                let mid = (lo + hi) / 2.0;
                let mid_state = state_at(mid)?;
                let mid_angle = base
                    + angle_difference(
                        math::atan2(mid_state.position[1], mid_state.position[0]),
                        base,
                    );
                if (current_unwrapped - previous_unwrapped).is_sign_positive() {
                    if mid_angle < target {
                        lo = mid;
                    } else {
                        hi = mid;
                    }
                } else if mid_angle > target {
                    lo = mid;
                } else {
                    hi = mid;
                }
            }
            let solved_t = (lo + hi) / 2.0;
            let solved = state_at(solved_t)?;
            if (solved.radius - node_radius).abs() <= width * 2.0 {
                return Some((StdInstant(solved_t), solved));
            }
        }
        previous_t = t;
        previous_angle = current_angle;
    }
    None
}

/// plumb: pending(wave-1)
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
    if !comet.eccentricity.is_finite()
        || !(0.0..1.0).contains(&comet.eccentricity)
        || !comet.inclination_deg.is_finite()
        || !comet.ascending_node_deg.is_finite()
        || comet.inclination_deg.abs() > 360.0
        || comet.ascending_node_deg.abs() > 360.0
        || !system.anchor.orbit.get().is_finite()
        || system.anchor.orbit.get() <= 0.0
        || !system.anchor.year.get().is_finite()
        || system.anchor.year.get() <= 0.0
    {
        return Err(MeteorAbsence::InvalidOrbit);
    }
    let comet_epoch = comet.perihelion_epoch.get();
    let nodes = comet_nodes(comet, comet_epoch, comet_epoch + comet.period.get())
        .ok_or(MeteorAbsence::NoCrossing)?;
    let (crossing_epoch, anchor_state, comet_state) = nodes
        .into_iter()
        .filter_map(|(node_longitude, comet_state, node_position)| {
            let node_radius =
                (node_position[0] * node_position[0] + node_position[1] * node_position[1]).sqrt();
            anchor_epoch_for_node(system, node_longitude, node_radius, width.get())
                .map(|(epoch, anchor)| (epoch, anchor, comet_state))
        })
        .min_by(|a, b| {
            a.0.get()
                .abs()
                .total_cmp(&b.0.get().abs())
                .then_with(|| a.0.get().total_cmp(&b.0.get()))
        })
        .ok_or(MeteorAbsence::NoCrossing)?;
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
    Ok(DebrisStream {
        comet: comet.id,
        width,
        density,
        radiant,
        relative_velocity_au_per_day: relative,
        epoch: crossing_epoch,
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
    if !stream.width.get().is_finite()
        || stream.width.get() <= 0.0
        || stream.width.get() > 1.0
        || !stream.density.is_finite()
        || stream.density < 0.0
        || !stream.epoch.get().is_finite()
        || !stream.validity.0.get().is_finite()
        || !stream.validity.1.get().is_finite()
        || stream.validity.0 > stream.validity.1
        || stream
            .relative_velocity_au_per_day
            .iter()
            .any(|v| !v.is_finite())
        || !stream.radiant.lon_deg.is_finite()
        || !stream.radiant.lat_deg.is_finite()
        || stream.radiant.lat_deg.abs() > 90.0
    {
        return Err(MeteorAbsence::InvalidStream);
    }
    if instant < stream.validity.0 || instant > stream.validity.1 {
        return Err(MeteorAbsence::OutsideValidity);
    }
    let year = system.anchor.year.0;
    if !year.is_finite() || year <= 0.0 {
        return Err(MeteorAbsence::InvalidOrbit);
    }
    let duration = 2.0 * stream.width.get()
        / (stream.relative_velocity_au_per_day[0].powi(2)
            + stream.relative_velocity_au_per_day[1].powi(2))
        .sqrt();
    if !duration.is_finite() || duration <= 0.0 {
        return Err(MeteorAbsence::InvalidStream);
    }
    let phase = ((instant.get() - stream.epoch.get()).rem_euclid(year)) / year;
    let signed = if phase <= 0.5 {
        phase * year
    } else {
        (phase - 1.0) * year
    };
    let peak = StdInstant(instant.get() - signed);
    let ecliptic = EclipticCoord {
        lon_deg: stream.radiant.lon_deg,
        lat_deg: stream.radiant.lat_deg,
    };
    let equatorial = equatorial_at(&ecliptic, system.forcing.obliquity_at(instant.get()), 0.0);
    let calendar = calendar_of(system);
    let altitude = if let Some((_, fraction)) = calendar.local_day(instant) {
        let hour = (std::f64::consts::TAU
            * (fraction - 0.5)
            * if calendar.is_retrograde() { -1.0 } else { 1.0 }
            + (observer.longitude.get() - equatorial.ra_deg).to_radians())
        .rem_euclid(std::f64::consts::TAU);
        math::asin(
            (math::sin(observer.latitude.get().to_radians())
                * math::sin(equatorial.dec_deg.to_radians())
                + math::cos(observer.latitude.get().to_radians())
                    * math::cos(equatorial.dec_deg.to_radians())
                    * math::cos(hour))
            .clamp(-1.0, 1.0),
        )
        .to_degrees()
    } else {
        let frame = anchor_body_to_frame_at(system, instant);
        let (lat, lon) = (
            observer.latitude.get().to_radians(),
            observer.longitude.get().to_radians(),
        );
        let dec = equatorial.dec_deg.to_radians();
        let ra = equatorial.ra_deg.to_radians();
        let inertial = [
            math::cos(dec) * math::cos(ra),
            math::cos(dec) * math::sin(ra),
            math::sin(dec),
        ];
        let body = [
            frame[0][0] * inertial[0] + frame[1][0] * inertial[1] + frame[2][0] * inertial[2],
            frame[0][1] * inertial[0] + frame[1][1] * inertial[1] + frame[2][1] * inertial[2],
            frame[0][2] * inertial[0] + frame[1][2] * inertial[1] + frame[2][2] * inertial[2],
        ];
        math::asin(
            (math::cos(lat) * math::cos(lon) * body[0]
                + math::cos(lat) * math::sin(lon) * body[1]
                + math::sin(lat) * body[2])
                .clamp(-1.0, 1.0),
        )
        .to_degrees()
    };
    let attenuation = (1.0 - observer.atmospheric_attenuation.clamp(0.0, 1.0))
        * (1.0 - observer.moonlight.clamp(0.0, 1.0));
    let base = MeteorShower {
        comet: stream.comet,
        peak,
        duration: StdDays(duration),
        radiant: stream.radiant,
        altitude: Degrees(altitude),
        relative_velocity_au_per_day: stream.relative_velocity_au_per_day,
        rate_per_hour: stream.density
            * 100.0
            * math::sin(altitude.to_radians()).max(0.0)
            * attenuation,
        visibility: MeteorVisibility::Visible,
    };
    if !base.altitude.get().is_finite() || !base.rate_per_hour.is_finite() {
        return Err(MeteorAbsence::InvalidStream);
    }
    if signed.abs() > duration / 2.0 {
        return Err(MeteorAbsence::OutsideSeason);
    }
    let solar_altitude = calendar.local_day(instant).map_or(0.0, |(_, fraction)| {
        let sun = calendar.solar_equatorial(instant);
        let hour = (std::f64::consts::TAU
            * (fraction - 0.5)
            * if calendar.is_retrograde() { -1.0 } else { 1.0 }
            + (observer.longitude.get() - sun.ra_deg).to_radians())
        .rem_euclid(std::f64::consts::TAU);
        math::asin(
            (math::sin(observer.latitude.get().to_radians()) * math::sin(sun.dec_deg.to_radians())
                + math::cos(observer.latitude.get().to_radians())
                    * math::cos(sun.dec_deg.to_radians())
                    * math::cos(hour))
            .clamp(-1.0, 1.0),
        )
        .to_degrees()
    });
    let reason = if observer.daylight >= 1.0 || solar_altitude > 1e-12 {
        Some(MeteorAbsence::Daylight)
    } else if observer.atmospheric_attenuation >= 1.0 {
        Some(MeteorAbsence::Atmosphere)
    } else if observer.moonlight >= 1.0 {
        Some(MeteorAbsence::Moonlight)
    } else if altitude <= observer.horizon.get() {
        Some(MeteorAbsence::BelowHorizon)
    } else if stream.density < MIN_STREAM_DENSITY || base.rate_per_hour < 0.01 {
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
    fn crossing_away_from_perihelion_uses_oriented_node_state() {
        let (system, mut comet) = fixture();
        // p=a(1-e²)=1: the nodes, a quarter orbit from periapsis,
        // cross r=1 even though perihelion is only 2/3 AU.
        comet.semi_major_axis = Au(4.0 / 3.0);
        comet.periapsis_longitude_deg = 90.0;
        let stream = stream(&system, &comet);
        assert!(stream.epoch.get().abs() < 1e-9);
        assert!(
            stream
                .relative_velocity_au_per_day
                .iter()
                .all(|v| v.is_finite())
        );
        assert!(stream.relative_velocity_au_per_day[2].abs() > 0.001);
    }

    #[test]
    fn perihelion_radius_match_above_plane_is_not_a_crossing() {
        let (system, mut comet) = fixture();
        comet.periapsis_longitude_deg = 90.0;
        // Perihelion is at z=-1; both plane nodes are at r=1.5.
        assert_eq!(
            debris_stream_from_comet(&system, &comet, Au(0.02), 100.0),
            Err(MeteorAbsence::NoCrossing)
        );
    }

    #[test]
    fn anchor_phase_sets_encounter_date_and_velocity_at_the_node() {
        let (mut system, comet) = fixture();
        system.forcing.year_phase_offset = 0.25;
        let stream = stream(&system, &comet);
        assert!((stream.epoch.get() + 100.0).abs() < 1e-9);
        assert!(stream.relative_velocity_au_per_day[0].abs() < 1e-12);
        assert!(
            (stream.relative_velocity_au_per_day[1] + std::f64::consts::TAU / 400.0).abs() < 1e-12
        );
    }

    #[test]
    fn descending_node_can_supply_the_only_encounter() {
        let (system, mut comet) = fixture();
        comet.semi_major_axis = Au(4.0 / 3.0);
        comet.periapsis_longitude_deg = 90.0;
        let stream = stream(&system, &comet);
        assert!(stream.epoch.get().abs() < 1e-9);
    }

    #[test]
    fn eccentric_anchor_crossing_uses_actual_radius_not_semimajor_axis() {
        let (mut system, mut comet) = fixture();
        system.forcing.ecc_mean = 0.5;
        comet.ascending_node_deg = 90.0;
        comet.periapsis_longitude_deg = 90.0;
        comet.semi_major_axis = Au(1.0);
        let stream = stream(&system, &comet);
        assert!((stream.epoch.get() - 100.0).abs() < 1e-9);
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
        assert!((west.altitude.get() - north.altitude.get()).abs() > 1.0);
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

    #[test]
    fn publicly_constructed_malformed_streams_are_typed_absences() {
        let (system, comet) = fixture();
        let valid = stream(&system, &comet);
        for malformed in [
            DebrisStream {
                width: Au(0.0),
                ..valid
            },
            DebrisStream {
                density: f64::NAN,
                ..valid
            },
            DebrisStream {
                epoch: StdInstant(f64::NAN),
                ..valid
            },
            DebrisStream {
                validity: (StdInstant(1.0), StdInstant(0.0)),
                ..valid
            },
            DebrisStream {
                relative_velocity_au_per_day: [0.0, 0.0, 1.0],
                ..valid
            },
            DebrisStream {
                radiant: SkyRadiant {
                    lon_deg: f64::INFINITY,
                    lat_deg: 0.0,
                },
                ..valid
            },
        ] {
            assert_eq!(
                meteor_shower_at(&system, &malformed, StdInstant(0.0), observer()),
                Err(MeteorAbsence::InvalidStream)
            );
        }
        let sparse = debris_stream_from_comet(&system, &comet, Au(0.02), 0.0001).unwrap();
        let result = meteor_shower_at(&system, &sparse, StdInstant(0.0), observer()).unwrap();
        assert_eq!(
            result.visibility,
            MeteorVisibility::Absent(MeteorAbsence::BelowThreshold)
        );
    }
}

//! Analytic, coplanar stellar and planetary ephemerides.
//!
//! Positions share the scene's planetary plane: anchor angle is tau times
//! `year_phase`. The calendar's solar longitude uses that same phase, so
//! projecting a planet-plane sightline onto its equator requires a half-turn.
//! Alignments are against the orbital center (primary for single/wide,
//! barycenter for close), not transits of either resolved binary disc.

use crate::calendar::{SkyBand, calendar_of};
use crate::{Au, Degrees, StarSystem, StdDays, StdInstant, StellarTopology, Wanderer};
use hornvale_kernel::math;
use std::f64::consts::TAU;

/// Cartesian position in the shared orbital plane, in AU. Components are signed.
/// type-audit: pending(wave-1: x_au), pending(wave-1: y_au)
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct OrbitalPosition {
    /// Coordinate along the genesis phase-zero axis.
    pub x_au: f64,
    /// Coordinate a quarter turn ahead of the phase-zero axis.
    pub y_au: f64,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum OrbitalFrame {
    SystemPlaneAu,
    AnchorPlaneMegameters,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) struct OrbitalValidity {
    pub(crate) from: StdInstant,
    pub(crate) until: StdInstant,
}

impl OrbitalValidity {
    /// type-audit: pending(wave-1)
    /// plumb: pending(wave-1)
    pub(crate) const UNBOUNDED: Self = Self {
        from: StdInstant(f64::NEG_INFINITY),
        until: StdInstant(f64::INFINITY),
    };

    fn contains(self, instant: StdInstant) -> bool {
        instant >= self.from && instant <= self.until
    }
}

/// Internal osculating elements. Distances use the unit named by `frame`.
#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) struct OrbitalElements {
    pub(crate) frame: OrbitalFrame,
    pub(crate) epoch: StdInstant,
    pub(crate) period: StdDays,
    pub(crate) semi_major_axis: f64,
    pub(crate) eccentricity: f64,
    pub(crate) mean_longitude_at_epoch_turns: f64,
    pub(crate) periapsis_longitude_turns: f64,
    pub(crate) validity: OrbitalValidity,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) struct OrbitalState {
    pub(crate) frame: OrbitalFrame,
    pub(crate) position: [f64; 2],
    pub(crate) velocity_per_day: [f64; 2],
    pub(crate) radius: f64,
    pub(crate) semi_major_axis: f64,
    pub(crate) eccentricity: f64,
    pub(crate) mean_longitude_turns: f64,
    pub(crate) mean_anomaly_turns: f64,
    pub(crate) true_anomaly_rad: f64,
    pub(crate) eccentric_anomaly_rad: f64,
    pub(crate) true_longitude_turns: f64,
}

impl OrbitalState {
    /// Shipped scene projection: keep the mean longitude and circular radius.
    /// The exact eccentric position remains `position`; this projection preserves
    /// the existing byte contract without constructing a second orbit.
    fn legacy_circular_position(self) -> OrbitalPosition {
        OrbitalPosition {
            x_au: self.semi_major_axis * math::cos(TAU * self.mean_longitude_turns),
            y_au: self.semi_major_axis * math::sin(TAU * self.mean_longitude_turns),
        }
    }

    /// Shipped eclipse approximation, with periapsis at mean longitude 0.25.
    /// Preserve the original sine and operation order for byte compatibility.
    pub(crate) fn legacy_anchor_radius_ratio(self) -> f64 {
        1.0 - self.eccentricity * math::sin(TAU * self.mean_longitude_turns)
    }
}

fn normalize_turns(turns: f64) -> f64 {
    let phase = turns.rem_euclid(1.0);
    // A tiny negative remainder can round up to exactly one during Euclidean
    // normalization. Zero is the equivalent angle inside the promised interval.
    if phase == 1.0 { 0.0 } else { phase }
}

pub(crate) fn orbital_phase_at(
    period: StdDays,
    epoch: StdInstant,
    phase_at_epoch: f64,
    instant: StdInstant,
) -> f64 {
    let turns = (instant.get() - epoch.get()) / period.get() + phase_at_epoch;
    if period.get().is_finite() && period.get() > 0.0 && turns.is_finite() {
        normalize_turns(turns)
    } else if phase_at_epoch.is_finite() {
        normalize_turns(phase_at_epoch)
    } else {
        0.0
    }
}

pub(crate) fn orbital_state_at(
    elements: &OrbitalElements,
    instant: StdInstant,
) -> Option<OrbitalState> {
    if !elements.validity.contains(instant)
        || !instant.get().is_finite()
        || !elements.epoch.get().is_finite()
        || !elements.period.get().is_finite()
        || elements.period.get() <= 0.0
        || !elements.semi_major_axis.is_finite()
        || elements.semi_major_axis <= 0.0
        || !(0.0..1.0).contains(&elements.eccentricity)
        || !elements.mean_longitude_at_epoch_turns.is_finite()
        || !elements.periapsis_longitude_turns.is_finite()
        || !((instant.get() - elements.epoch.get()) / elements.period.get()
            + elements.mean_longitude_at_epoch_turns)
            .is_finite()
    {
        return None;
    }

    let mean_longitude_turns = orbital_phase_at(
        elements.period,
        elements.epoch,
        elements.mean_longitude_at_epoch_turns,
        instant,
    );
    let mean_anomaly_turns =
        normalize_turns(mean_longitude_turns - elements.periapsis_longitude_turns);
    let mean_anomaly = TAU * mean_anomaly_turns;
    let eccentricity = elements.eccentricity;
    let mut eccentric_anomaly = mean_anomaly;
    for _ in 0..16 {
        eccentric_anomaly -=
            (eccentric_anomaly - eccentricity * math::sin(eccentric_anomaly) - mean_anomaly)
                / (1.0 - eccentricity * math::cos(eccentric_anomaly));
    }

    let cos_e = math::cos(eccentric_anomaly);
    let sin_e = math::sin(eccentric_anomaly);
    let minor_ratio = (1.0 - eccentricity * eccentricity).sqrt();
    let orbital_x = elements.semi_major_axis * (cos_e - eccentricity);
    let orbital_y = elements.semi_major_axis * minor_ratio * sin_e;
    let denominator = 1.0 - eccentricity * cos_e;
    let true_anomaly = math::atan2(minor_ratio * sin_e, cos_e - eccentricity);
    let orientation = TAU * elements.periapsis_longitude_turns;
    let (cos_o, sin_o) = (math::cos(orientation), math::sin(orientation));
    let position = [
        orbital_x * cos_o - orbital_y * sin_o,
        orbital_x * sin_o + orbital_y * cos_o,
    ];
    let speed_scale = TAU * elements.semi_major_axis / elements.period.get() / denominator;
    let velocity_x = -speed_scale * sin_e;
    let velocity_y = speed_scale * minor_ratio * cos_e;

    let state = OrbitalState {
        frame: elements.frame,
        position,
        velocity_per_day: [
            velocity_x * cos_o - velocity_y * sin_o,
            velocity_x * sin_o + velocity_y * cos_o,
        ],
        radius: elements.semi_major_axis * denominator,
        semi_major_axis: elements.semi_major_axis,
        eccentricity,
        mean_longitude_turns,
        mean_anomaly_turns,
        true_anomaly_rad: true_anomaly,
        eccentric_anomaly_rad: eccentric_anomaly,
        true_longitude_turns: normalize_turns(
            true_anomaly / TAU + elements.periapsis_longitude_turns,
        ),
    };
    (state.position.iter().all(|x| x.is_finite())
        && state.velocity_per_day.iter().all(|x| x.is_finite())
        && state.radius.is_finite()
        && state.true_longitude_turns.is_finite())
    .then_some(state)
}

/// The single construction seam for the anchor's osculating elements.
pub(crate) fn anchor_orbital_state_at(
    orbit: Au,
    year: StdDays,
    forcing: &crate::forcing::OrbitalForcing,
    instant: StdInstant,
) -> Option<OrbitalState> {
    orbital_state_at(
        &OrbitalElements {
            frame: OrbitalFrame::SystemPlaneAu,
            epoch: StdInstant(0.0),
            period: year,
            semi_major_axis: orbit.get(),
            eccentricity: forcing.eccentricity_at(instant.get()),
            mean_longitude_at_epoch_turns: forcing.year_phase_offset,
            // The forcing convention puts periapsis at year phase 0.25.
            periapsis_longitude_turns: 0.25,
            validity: OrbitalValidity::UNBOUNDED,
        },
        instant,
    )
}

fn circular_state(
    frame: OrbitalFrame,
    semi_major_axis: f64,
    period: StdDays,
    phase_at_epoch: f64,
    instant: StdInstant,
) -> Option<OrbitalState> {
    orbital_state_at(
        &OrbitalElements {
            frame,
            epoch: StdInstant(0.0),
            period,
            semi_major_axis,
            eccentricity: 0.0,
            mean_longitude_at_epoch_turns: phase_at_epoch,
            periapsis_longitude_turns: 0.0,
            validity: OrbitalValidity::UNBOUNDED,
        },
        instant,
    )
}

/// Anchor in the native circumprimary or barycentric orbital plane, in AU.
/// Uses the legacy circular projection of the shared analytic anchor state.
/// An invalid orbit has the deterministic origin fallback.
pub fn anchor_position_at(system: &StarSystem, t: StdInstant) -> OrbitalPosition {
    anchor_orbital_state_at(system.anchor.orbit, system.anchor.year, &system.forcing, t)
        .map(OrbitalState::legacy_circular_position)
        .unwrap_or(OrbitalPosition {
            x_au: 0.0,
            y_au: 0.0,
        })
}

/// Anchor surface orientation, as columns (longitude zero, longitude 90, north).
///
/// Converts the exact calendar equatorial frame with Rz(pi) Rx(-obliquity),
/// then rotates its prime meridian by solar RA minus native subsolar longitude.
/// The half-turn reconciles calendar solar phase with the center sightline.
/// Retrograde and locked conventions belong to the calendar; a locked world's
/// prime meridian follows the orbital center rather than remaining inertial.
/// type-audit: bare-ok(ratio: return)
pub fn anchor_body_to_frame_at(system: &StarSystem, instant: StdInstant) -> [[f64; 3]; 3] {
    let calendar = calendar_of(system);
    let angle = (calendar.solar_equatorial(instant).ra_deg
        - crate::sub_solar_longitude_deg(&calendar, instant))
    .to_radians();
    let tilt = system.forcing.obliquity_at(instant.get()).to_radians();
    let (c, s) = (math::cos(angle), math::sin(angle));
    let (ce, se) = (math::cos(tilt), math::sin(tilt));
    [
        [-c, -ce * s, -se * s],
        [s, -ce * c, -se * c],
        [0.0, -se, ce],
    ]
}

/// Anchor-centered moon position in megameters in the native system frame.
/// Uses the existing phase, inclination and regressing-node producers, including
/// their retrograde-inclination convention. The calendar-to-system half-turn
/// changes XY signs only. No synodic cycle (or no moon) means no position.
/// type-audit: bare-ok(index: index), pending(wave-1: return)
pub fn moon_position_at(
    system: &StarSystem,
    index: usize,
    instant: StdInstant,
) -> Option<[f64; 3]> {
    let moon = system.moons.get(index)?;
    let calendar = calendar_of(system);
    let state = calendar.moon_orbital_state_at(index, instant)?;
    debug_assert_eq!(state.frame, OrbitalFrame::AnchorPlaneMegameters);
    let longitude = 360.0 * state.true_longitude_turns;
    let latitude = crate::moon_ecliptic_latitude_deg(&calendar, moon, index, instant)?;
    let direction = math::unit_sphere_from_lat_lon(latitude, longitude);
    let radius = moon.distance.get();
    Some([
        -radius * direction[0],
        -radius * direction[1],
        radius * direction[2],
    ])
}

/// Circular orbital phase in turns, normalized for pre-genesis instants too.
/// Invalid periods/times freeze at the normalized epoch phase (zero if invalid).
/// type-audit: bare-ok(ratio: return)
pub fn wanderer_phase_at(wanderer: &Wanderer, instant: StdInstant) -> f64 {
    orbital_phase_at(
        wanderer.period,
        StdInstant(0.0),
        wanderer.phase_offset,
        instant,
    )
}

/// Primary then companion, in the circumprimary or barycentric planetary frame.
/// Invalid binary elements yield no positions.
pub fn stellar_positions_at(system: &StarSystem, instant: StdInstant) -> Vec<OrbitalPosition> {
    let origin = OrbitalPosition {
        x_au: 0.0,
        y_au: 0.0,
    };
    if system.stellar.topology == StellarTopology::Single {
        return vec![origin];
    }
    let Some(companion) = system.stellar.companion.as_ref() else {
        return Vec::new();
    };
    let separation = companion.orbit.semi_major_axis.get();
    if system.stellar.topology == StellarTopology::WideBinary {
        let Some(state) = circular_state(
            OrbitalFrame::SystemPlaneAu,
            separation,
            companion.orbit.period,
            companion.orbit.phase,
            instant,
        ) else {
            return Vec::new();
        };
        vec![
            origin,
            OrbitalPosition {
                x_au: state.position[0],
                y_au: state.position[1],
            },
        ]
    } else {
        let total = system.star.mass.get() + companion.star.mass.get();
        let Some(primary) = circular_state(
            OrbitalFrame::SystemPlaneAu,
            separation * companion.star.mass.get() / total,
            companion.orbit.period,
            companion.orbit.phase,
            instant,
        ) else {
            return Vec::new();
        };
        let Some(secondary) = circular_state(
            OrbitalFrame::SystemPlaneAu,
            separation * system.star.mass.get() / total,
            companion.orbit.period,
            companion.orbit.phase,
            instant,
        ) else {
            return Vec::new();
        };
        vec![
            OrbitalPosition {
                x_au: -primary.position[0],
                y_au: -primary.position[1],
            },
            OrbitalPosition {
                x_au: secondary.position[0],
                y_au: secondary.position[1],
            },
        ]
    }
}

/// Planet position in the same frame as [`stellar_positions_at`].
/// An absent index or invalid orbital elements/time return `None`.
/// type-audit: bare-ok(index: index)
pub fn wanderer_position_at(
    system: &StarSystem,
    index: usize,
    instant: StdInstant,
) -> Option<OrbitalPosition> {
    let w = system.wanderers.get(index)?;
    let state = circular_state(
        OrbitalFrame::SystemPlaneAu,
        w.orbit.get(),
        w.period,
        w.phase_offset,
        instant,
    )?;
    Some(OrbitalPosition {
        x_au: state.position[0],
        y_au: state.position[1],
    })
}

fn relative_position(position: OrbitalPosition, anchor: OrbitalPosition) -> OrbitalPosition {
    OrbitalPosition {
        x_au: position.x_au - anchor.x_au,
        y_au: position.y_au - anchor.y_au,
    }
}

fn longitude(position: OrbitalPosition) -> Degrees {
    Degrees(
        math::atan2(position.y_au, position.x_au)
            .to_degrees()
            .rem_euclid(360.0),
    )
}

/// Apparent longitude from the anchor, in the planetary plane, in `[0, 360)`.
/// Coincident positions have no sightline and return `None`.
/// type-audit: bare-ok(index: index)
pub fn anchor_relative_longitude_at(
    system: &StarSystem,
    index: usize,
    instant: StdInstant,
) -> Option<Degrees> {
    let relative = relative_position(
        wanderer_position_at(system, index, instant)?,
        anchor_position_at(system, instant),
    );
    (relative.x_au != 0.0 || relative.y_au != 0.0).then(|| longitude(relative))
}

/// One star's unattenuated inverse-square flux at the anchor, relative to Earth.
/// type-audit: bare-ok(index: star), bare-ok(ratio: flux_rel)
#[derive(Debug, Clone, PartialEq)]
pub struct StellarLight {
    /// Zero is primary; one is companion.
    pub star: usize,
    /// Anchor-to-star distance.
    pub distance: Au,
    /// Apparent longitude in the planetary plane.
    pub longitude: Degrees,
    /// Luminosity / distance squared, before atmosphere or horizon culling.
    pub flux_rel: f64,
}

/// Resolved illumination, separate from the climate's declared binary envelope.
/// type-audit: bare-ok(ratio: combined_flux_rel)
#[derive(Debug, Clone, PartialEq)]
pub struct StellarIllumination {
    /// Primary then companion, including a wide companion's observable light.
    pub sources: Vec<StellarLight>,
    /// Sum of the unattenuated source fluxes, not a local daylight estimate.
    pub combined_flux_rel: f64,
}

/// Resolve both stars' fluxes at the anchor without changing climate forcing.
pub fn stellar_illumination_at(system: &StarSystem, instant: StdInstant) -> StellarIllumination {
    let anchor = anchor_position_at(system, instant);
    let sources: Vec<_> = stellar_positions_at(system, instant)
        .into_iter()
        .enumerate()
        .map(|(index, position)| {
            let relative = relative_position(position, anchor);
            let squared = relative.x_au * relative.x_au + relative.y_au * relative.y_au;
            let star = if index == 0 {
                &system.star
            } else {
                &system
                    .stellar
                    .companion
                    .as_ref()
                    .expect("companion position has a star")
                    .star
            };
            StellarLight {
                star: index,
                distance: Au(if squared.is_finite() && squared > 0.0 {
                    squared.sqrt()
                } else {
                    0.0
                }),
                longitude: longitude(relative),
                flux_rel: if squared.is_finite() && squared > 0.0 {
                    crate::luminosity_at(star, instant).get() / squared
                } else {
                    0.0
                },
            }
        })
        .collect();
    StellarIllumination {
        combined_flux_rel: sources.iter().map(|s| s.flux_rel).sum(),
        sources,
    }
}

fn synodic_rate(system: &StarSystem, w: &Wanderer) -> f64 {
    1.0 / w.period.get() - 1.0 / system.anchor.year.get()
}

/// Finite recurrence from the actual orbital rates; a stationary alignment has none.
/// type-audit: bare-ok(index: index)
pub fn wanderer_recurrence(system: &StarSystem, index: usize) -> Option<StdDays> {
    let rate = synodic_rate(system, system.wanderers.get(index)?);
    if !rate.is_finite() || rate == 0.0 {
        return None;
    }
    let period = 1.0 / rate.abs();
    period.is_finite().then_some(StdDays(period))
}

/// Dated alignment or a complete pair of stationary points.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum WandererEventKind {
    /// The body lies along the center's apparent longitude.
    Conjunction,
    /// An outer body stands opposite the center in the anchor's sky.
    Opposition,
    /// Apparent longitude reverses at `at` and resumes forward motion at `until`.
    Retrograde {
        /// The second stationary point, included only if it lies within the query.
        until: StdInstant,
    },
}

/// A derived calendar mark; never a persistent world fact.
/// type-audit: bare-ok(index: wanderer)
#[derive(Debug, Clone, PartialEq)]
pub struct WandererEvent {
    /// Index in the system's orbit-sorted wanderers.
    pub wanderer: usize,
    /// Absolute standard time of alignment or the first stationary point.
    pub at: StdInstant,
    /// What the geometry does at this instant.
    pub kind: WandererEventKind,
}

// Enumerate an arithmetic family with no time-step sampling. Stop if the
// instant's floating-point resolution cannot distinguish adjacent recurrences.
fn crossings(rate: f64, offset: f64, target: f64, from: f64, until: f64) -> Vec<f64> {
    let period = 1.0 / rate.abs();
    let phase = (rate * from + offset - target).rem_euclid(1.0);
    let wait = if phase == 0.0 {
        0.0
    } else if rate > 0.0 {
        (1.0 - phase) / rate
    } else {
        phase / -rate
    };
    let first = from + wait;
    let mut out = Vec::new();
    let mut t = first;
    while t < until {
        out.push(t);
        let next = first + out.len() as f64 * period;
        if next <= t {
            break;
        }
        t = next;
    }
    out
}

/// Exact circular alignment dates and complete retrograde intervals in `[from, until)`.
/// A tangent, a zero/nonfinite synodic rate, or either truncated station emits no loop.
/// Binary alignments reference the orbital center; stellar disc encounters are deferred.
pub fn wanderer_events(
    system: &StarSystem,
    from: StdInstant,
    until: StdInstant,
) -> Vec<WandererEvent> {
    let mut events = Vec::new();
    if until <= from {
        return events;
    }
    for (index, w) in system.wanderers.iter().enumerate() {
        if wanderer_recurrence(system, index).is_none() {
            continue;
        }
        let rate = synodic_rate(system, w);
        let offset = w.phase_offset - system.forcing.year_phase_offset;
        for target in [0.0, 0.5] {
            let kind = if target == 0.0 && w.orbit > system.anchor.orbit {
                WandererEventKind::Opposition
            } else {
                WandererEventKind::Conjunction
            };
            for at in crossings(rate, offset, target, from.get(), until.get()) {
                events.push(WandererEvent {
                    wanderer: index,
                    at: StdInstant(at),
                    kind,
                });
            }
        }
        // d(longitude)/dt has numerator r² nw + R² na - rR(nw+na)cos(delta).
        // It is negative around delta=0 only when the station cosine is <1.
        let (r, big_r) = (w.orbit.get(), system.anchor.orbit.get());
        let (nw, na) = (1.0 / w.period.get(), 1.0 / system.anchor.year.get());
        let station_cos = (r * r * nw + big_r * big_r * na) / (r * big_r * (nw + na));
        if !(-1.0..1.0).contains(&station_cos) {
            continue;
        }
        let half_width = math::acos(station_cos) / TAU / rate.abs();
        for center in crossings(rate, offset, 0.0, from.get(), until.get()) {
            let (start, end) = (center - half_width, center + half_width);
            if start >= from.get() && end < until.get() {
                events.push(WandererEvent {
                    wanderer: index,
                    at: StdInstant(start),
                    kind: WandererEventKind::Retrograde {
                        until: StdInstant(end),
                    },
                });
            }
        }
    }
    events.sort_by(|a, b| {
        a.at.get()
            .total_cmp(&b.at.get())
            .then(a.wanderer.cmp(&b.wanderer))
    });
    events
}

/// Observable vocabulary without proper names or cultural interpretation.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum WandererAppearance {
    /// Inner body visible in dawn twilight.
    Morning,
    /// Inner body visible in dusk twilight.
    Evening,
    /// Outer body near opposition.
    Opposition,
    /// Outer body crossing the night sky away from opposition.
    Night,
}

/// A body remains in this result even when glare or the horizon hides it.
/// type-audit: bare-ok(index: wanderer), bare-ok(flag: visible)
#[derive(Debug, Clone, PartialEq)]
pub struct WandererVisibility {
    /// Index in the system's wanderers.
    pub wanderer: usize,
    /// Separation from the orbital center's apparent direction, degrees.
    pub elongation: Degrees,
    /// Geometric altitude; absent without a solar day or a valid latitude.
    pub altitude: Option<Degrees>,
    /// Whether twilight/night, glare and horizon conditions all admit the body.
    pub visible: bool,
    /// Vocabulary only when visible; locked worlds get no morning/evening claim.
    pub appearance: Option<WandererAppearance>,
}

/// Existing night-sky glare cutoff, now applied to actual angular separation.
/// plumb: pending(wave-1)
const GLARE_DEGREES: f64 = 15.0;

/// Derive visibility at the calendar's reference meridian using its twilight band.
/// The same glare cutoff applies to each resolved sun. Altitudes use the current
/// obliquity and spin; a locked anchor has no supplied horizon clock.
/// type-audit: pending(wave-1: latitude)
pub fn wanderer_visibility(
    system: &StarSystem,
    latitude: f64,
    instant: StdInstant,
) -> Vec<WandererVisibility> {
    let calendar = calendar_of(system);
    let band = calendar.sky_band(instant, latitude);
    let anchor = anchor_position_at(system, instant);
    let center_lon = longitude(OrbitalPosition {
        x_au: -anchor.x_au,
        y_au: -anchor.y_au,
    })
    .get();
    let lights = stellar_illumination_at(system, instant);
    let separation = |a: f64, b: f64| ((a - b + 180.0).rem_euclid(360.0) - 180.0).abs();
    system
        .wanderers
        .iter()
        .enumerate()
        .map(|(index, w)| {
            let lon = anchor_relative_longitude_at(system, index, instant);
            let elongation = lon.map_or(0.0, |lon| separation(lon.get(), center_lon));
            let altitude = lon.and_then(|lon| {
                calendar.ecliptic_altitude_at(
                    instant,
                    latitude,
                    Degrees((lon.get() + 180.0).rem_euclid(360.0)),
                )
            });
            let clear_suns = lon.is_some_and(|lon| {
                lights.sources.iter().all(|light| {
                    separation(lon.get(), light.longitude.get()) >= GLARE_DEGREES
                        && calendar
                            .ecliptic_altitude_at(
                                instant,
                                latitude,
                                Degrees((light.longitude.get() + 180.0).rem_euclid(360.0)),
                            )
                            .is_some_and(|a| a.get() <= 0.0)
                })
            });
            let inner = w.orbit < system.anchor.orbit;
            let visible = altitude.is_some_and(|a| a.get() > 0.0)
                && clear_suns
                && matches!(band, Some(SkyBand::Twilight | SkyBand::Night))
                && (!inner || band == Some(SkyBand::Twilight))
                && elongation >= GLARE_DEGREES;
            let appearance = visible.then(|| {
                if inner {
                    if calendar
                        .local_day(instant)
                        .expect("visible requires a solar day")
                        .1
                        < 0.5
                    {
                        WandererAppearance::Morning
                    } else {
                        WandererAppearance::Evening
                    }
                } else if elongation >= 180.0 - GLARE_DEGREES {
                    WandererAppearance::Opposition
                } else {
                    WandererAppearance::Night
                }
            });
            WandererVisibility {
                wanderer: index,
                elongation: Degrees(elongation),
                altitude,
                visible,
                appearance,
            }
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{ForcingPin, RotationPin, SkyPins, StellarTopology, WandererClass, generate};
    use hornvale_kernel::Seed;

    fn system() -> StarSystem {
        let mut s = generate(
            Seed(42),
            &SkyPins {
                topology: Some(StellarTopology::Single),
                rotation: Some(RotationPin::PeriodHours(24.0)),
                obliquity: Some(Degrees(0.0)),
                forcing: Some(ForcingPin::Zero),
                wanderers: Some(0),
                ..SkyPins::default()
            },
        )
        .unwrap()
        .value;
        s.anchor.orbit = Au(1.0);
        s.anchor.year = StdDays(8.0);
        s.forcing.year_phase_offset = 0.0;
        s.forcing.day_phase_offset = 0.0;
        s
    }

    fn body(orbit: f64, period: f64, phase: f64) -> Wanderer {
        Wanderer {
            orbit: Au(orbit),
            period: StdDays(period),
            phase_offset: phase,
            class: WandererClass::Rock,
            albedo: 0.3,
            max_elongation_deg: (orbit < 1.0).then(|| math::asin(orbit).to_degrees()),
            synodic_period: StdDays(1.0 / (1.0 / period - 0.125).abs()),
            apparent_brightness: 1.0,
        }
    }

    fn elements(epoch: f64, eccentricity: f64) -> OrbitalElements {
        OrbitalElements {
            frame: OrbitalFrame::SystemPlaneAu,
            epoch: StdInstant(epoch),
            period: StdDays(8.0),
            semi_major_axis: 2.0,
            eccentricity,
            mean_longitude_at_epoch_turns: 0.25,
            periapsis_longitude_turns: 0.0,
            validity: OrbitalValidity {
                from: StdInstant(-100.0),
                until: StdInstant(100.0),
            },
        }
    }

    #[test]
    fn orbital_evaluator_anchors_mean_longitude_at_the_declared_epoch() {
        let orbit = elements(10.0, 0.0);
        let at_epoch = orbital_state_at(&orbit, StdInstant(10.0)).unwrap();
        let quarter_later = orbital_state_at(&orbit, StdInstant(12.0)).unwrap();

        assert!(at_epoch.position[0].abs() < 1e-12);
        assert!((at_epoch.position[1] - 2.0).abs() < 1e-12);
        assert_eq!(at_epoch.frame, OrbitalFrame::SystemPlaneAu);
        assert!((at_epoch.velocity_per_day[0] + std::f64::consts::PI / 2.0).abs() < 1e-12);
        assert!(at_epoch.velocity_per_day[1].abs() < 1e-12);
        assert!((quarter_later.position[0] + 2.0).abs() < 1e-12);
        assert!(quarter_later.position[1].abs() < 1e-12);
        assert!(orbital_state_at(&orbit, StdInstant(101.0)).is_none());
    }

    #[test]
    fn orbital_evaluator_wraps_negative_instants_before_the_epoch() {
        let orbit = elements(0.0, 0.0);
        let state = orbital_state_at(&orbit, StdInstant(-2.0)).unwrap();

        assert!((state.mean_anomaly_turns - 0.0).abs() < 1e-12);
        assert!((state.position[0] - 2.0).abs() < 1e-12);
        assert!(state.position[1].abs() < 1e-12);
    }

    #[test]
    fn eccentric_and_true_anomaly_describe_the_same_position() {
        let orbit = elements(-3.0, 0.4);
        let state = orbital_state_at(&orbit, StdInstant(-2.0)).unwrap();
        let mean = std::f64::consts::TAU * state.mean_anomaly_turns;
        let eccentric = state.eccentric_anomaly_rad;
        let true_anomaly = state.true_anomaly_rad;

        assert!((eccentric - orbit.eccentricity * math::sin(eccentric) - mean).abs() < 1e-12);
        let denominator = 1.0 - orbit.eccentricity * math::cos(eccentric);
        let expected_cos = (math::cos(eccentric) - orbit.eccentricity) / denominator;
        let expected_sin = (1.0 - orbit.eccentricity * orbit.eccentricity).sqrt()
            * math::sin(eccentric)
            / denominator;
        assert!((math::cos(true_anomaly) - expected_cos).abs() < 1e-12);
        assert!((math::sin(true_anomaly) - expected_sin).abs() < 1e-12);
    }

    #[test]
    fn degenerate_wanderer_periods_freeze_phase_and_have_no_position() {
        for period in [0.0, -1.0, f64::INFINITY, f64::NEG_INFINITY, f64::NAN] {
            let mut s = system();
            s.wanderers.push(body(2.0, period, 0.25));
            assert_eq!(wanderer_phase_at(&s.wanderers[0], StdInstant(-2.0)), 0.25);
            assert_eq!(wanderer_position_at(&s, 0, StdInstant(-2.0)), None);
        }
    }

    #[test]
    fn malformed_circular_elements_have_no_position() {
        let mut s = system();
        s.wanderers.push(body(2.0, 16.0, 0.25));
        for radius in [0.0, -1.0, f64::NAN, f64::INFINITY] {
            s.wanderers[0].orbit = Au(radius);
            assert_eq!(wanderer_position_at(&s, 0, StdInstant(0.0)), None);
        }
        s.wanderers[0].orbit = Au(2.0);
        s.wanderers[0].phase_offset = f64::NAN;
        assert_eq!(wanderer_position_at(&s, 0, StdInstant(0.0)), None);
        assert_eq!(wanderer_phase_at(&s.wanderers[0], StdInstant(0.0)), 0.0);
    }

    #[test]
    fn invalid_binary_orbit_has_no_stellar_positions() {
        for topology in [StellarTopology::WideBinary, StellarTopology::CloseBinary] {
            let mut s = generate(
                Seed(42),
                &SkyPins {
                    topology: Some(topology),
                    ..SkyPins::default()
                },
            )
            .unwrap()
            .value;
            for period in [0.0, -1.0, f64::INFINITY, f64::NAN] {
                s.stellar.companion.as_mut().unwrap().orbit.period = StdDays(period);
                assert!(stellar_positions_at(&s, StdInstant(0.0)).is_empty());
            }
        }
    }

    #[test]
    fn overflowed_orbital_arithmetic_is_absent_and_phase_stays_finite() {
        let mut orbit = elements(0.0, 0.0);
        orbit.semi_major_axis = f64::MAX;
        assert!(orbital_state_at(&orbit, StdInstant(0.0)).is_none());
        orbit.semi_major_axis = 2.0;
        orbit.period = StdDays(f64::MIN_POSITIVE);
        orbit.validity = OrbitalValidity::UNBOUNDED;
        assert!(orbital_state_at(&orbit, StdInstant(f64::MAX)).is_none());
        assert_eq!(
            wanderer_phase_at(&body(2.0, f64::MIN_POSITIVE, 0.25), StdInstant(f64::MAX)),
            0.25
        );
    }

    #[test]
    fn phase_normalizes_zero_negative_and_large_days() {
        let w = body(0.5, 4.0, 0.25);
        for (day, want) in [
            (0.0, 0.25),
            (-2.0, 0.75),
            (4_000_000_001.0, 0.5),
            (-4_000_000_003.0, 0.5),
        ] {
            assert_eq!(wanderer_phase_at(&w, StdInstant(day)), want);
        }
        assert!((0.0..1.0).contains(&wanderer_phase_at(&body(0.5, 4.0, 0.0), StdInstant(-1e-20))));
    }

    #[test]
    fn retrograde_spin_changes_the_twilight_horizon_but_not_event_dates() {
        let mut s = system();
        s.wanderers.push(body(0.5, 2.0, 0.16));
        let dates = wanderer_events(&s, StdInstant(0.0), StdInstant(8.0));
        if let crate::Rotation::Spinning { retrograde, .. } = &mut s.anchor.rotation {
            *retrograde = true;
        }
        assert_eq!(dates, wanderer_events(&s, StdInstant(0.0), StdInstant(8.0)));
        assert!(!wanderer_visibility(&s, 0.0, StdInstant(0.24))[0].visible);
        s.wanderers[0].phase_offset = 0.66;
        assert_eq!(
            wanderer_visibility(&s, 0.0, StdInstant(0.24))[0].appearance,
            Some(WandererAppearance::Morning)
        );
        for latitude in [f64::NAN, 91.0] {
            let seen = wanderer_visibility(&s, latitude, StdInstant(0.24));
            assert!(seen[0].altitude.is_none() && !seen[0].visible);
        }
    }

    #[test]
    fn position_uses_body_phase_and_subtracts_anchor_position() {
        let mut s = system();
        s.wanderers.push(body(2.0, 16.0, 0.25));
        let p = wanderer_position_at(&s, 0, StdInstant(0.0)).unwrap();
        assert!(p.x_au.abs() < 1e-12 && (p.y_au - 2.0).abs() < 1e-12);
        let lon = anchor_relative_longitude_at(&s, 0, StdInstant(0.0))
            .unwrap()
            .get();
        assert!((lon - 116.565051177).abs() < 1e-8);
        assert!(wanderer_position_at(&s, 1, StdInstant(0.0)).is_none());
    }

    #[test]
    fn zero_eccentricity_positions_preserve_the_legacy_circular_results() {
        fn legacy_circular(
            radius: f64,
            period: StdDays,
            offset: f64,
            t: StdInstant,
        ) -> OrbitalPosition {
            let phase = (t.get() / period.get() + offset).rem_euclid(1.0);
            OrbitalPosition {
                x_au: radius * math::cos(TAU * phase),
                y_au: radius * math::sin(TAU * phase),
            }
        }

        let instant = StdInstant(-123.4);
        let mut single = system();
        single.wanderers.push(body(2.0, 16.0, 0.37));
        assert_eq!(
            anchor_position_at(&single, instant),
            legacy_circular(
                single.anchor.orbit.get(),
                single.anchor.year,
                single.forcing.year_phase_offset,
                instant,
            )
        );
        assert_eq!(
            wanderer_position_at(&single, 0, instant).unwrap(),
            legacy_circular(2.0, StdDays(16.0), 0.37, instant)
        );

        let binary = generate(
            Seed(43),
            &SkyPins {
                topology: Some(StellarTopology::CloseBinary),
                ..SkyPins::default()
            },
        )
        .unwrap()
        .value;
        let companion = binary.stellar.companion.as_ref().unwrap();
        let total_mass = binary.star.mass.get() + companion.star.mass.get();
        let expected = [
            legacy_circular(
                -companion.orbit.semi_major_axis.get() * companion.star.mass.get() / total_mass,
                companion.orbit.period,
                companion.orbit.phase,
                instant,
            ),
            legacy_circular(
                companion.orbit.semi_major_axis.get() * binary.star.mass.get() / total_mass,
                companion.orbit.period,
                companion.orbit.phase,
                instant,
            ),
        ];
        assert_eq!(stellar_positions_at(&binary, instant), expected);
    }

    #[test]
    fn inner_elongation_never_exceeds_tangent_bound() {
        let mut s = system();
        s.wanderers.push(body(0.5, 2.0, 0.0));
        for i in 0..120 {
            let seen = wanderer_visibility(&s, 0.0, StdInstant(i as f64 / 10.0));
            assert!(seen[0].elongation.get() <= 30.0 + 1e-10);
        }
    }

    #[test]
    fn outer_opposition_is_above_the_midnight_horizon() {
        let mut s = system();
        s.wanderers.push(body(2.0, 16.0, 0.0));
        let seen = wanderer_visibility(&s, 0.0, StdInstant(0.0));
        assert!((seen[0].elongation.get() - 180.0).abs() < 1e-10);
        assert!(seen[0].visible);
        assert_eq!(seen[0].appearance, Some(WandererAppearance::Opposition));
    }

    #[test]
    fn conjunction_hides_but_does_not_remove_a_body() {
        let mut s = system();
        s.wanderers.push(body(0.5, 2.0, 0.0));
        let seen = wanderer_visibility(&s, 0.0, StdInstant(0.0));
        assert_eq!(seen.len(), 1);
        assert!(!seen[0].visible);
        assert!(seen[0].elongation.get() < 1e-10);
    }

    #[test]
    fn inner_morning_and_evening_require_the_correct_twilight_horizon() {
        let mut s = system();
        // At dawn t=.24, anchor phase .03; a body 90 degrees ahead
        // lies west of the sun and above the eastern horizon.
        s.wanderers.push(body(0.5, 2.0, 0.16));
        let seen = wanderer_visibility(&s, 0.0, StdInstant(0.24));
        assert_eq!(seen[0].appearance, Some(WandererAppearance::Morning));
        assert!(seen[0].visible);
        s.wanderers[0].phase_offset = 0.465; // behind anchor at dusk .76
        let seen = wanderer_visibility(&s, 0.0, StdInstant(0.76));
        assert_eq!(seen[0].appearance, Some(WandererAppearance::Evening));
        assert!(seen[0].visible);
        assert!(!wanderer_visibility(&s, 0.0, StdInstant(0.5))[0].visible);
    }

    #[test]
    fn dated_alignments_and_only_complete_retrograde_intervals() {
        let mut s = system();
        s.wanderers.push(body(2.0, 24.0, 0.0));
        let events = wanderer_events(&s, StdInstant(-6.0), StdInstant(6.0));
        assert!(
            events
                .iter()
                .any(|e| e.kind == WandererEventKind::Opposition && e.at.get().abs() < 1e-10)
        );
        let loop_event = events
            .iter()
            .find(|e| matches!(e.kind, WandererEventKind::Retrograde { .. }))
            .unwrap();
        let WandererEventKind::Retrograde { until } = loop_event.kind else {
            unreachable!()
        };
        assert!(loop_event.at.get() < 0.0 && until.get() > 0.0);
        // Stations bracket real sign changes, independently measured from longitude.
        let slope = |t: f64| {
            let a = anchor_relative_longitude_at(&s, 0, StdInstant(t - 1e-5))
                .unwrap()
                .get();
            let b = anchor_relative_longitude_at(&s, 0, StdInstant(t + 1e-5))
                .unwrap()
                .get();
            (b - a + 180.0).rem_euclid(360.0) - 180.0
        };
        assert!(slope(loop_event.at.get() - 0.01) > 0.0);
        assert!(slope(0.0) < 0.0);
        assert!(slope(until.get() + 0.01) > 0.0);
        for (from, end) in [(0.0, 6.0), (-6.0, 0.0), (0.0, 0.5)] {
            assert!(
                wanderer_events(&s, StdInstant(from), StdInstant(end))
                    .iter()
                    .all(|e| !matches!(e.kind, WandererEventKind::Retrograde { .. }))
            );
        }
        assert!(
            events
                .iter()
                .any(|e| e.kind == WandererEventKind::Conjunction
                    && (e.at.get() + 6.0).abs() < 1e-10)
        );
    }

    #[test]
    fn tangent_and_zero_synodic_rate_are_not_loops_or_finite_recurrences() {
        let mut s = system();
        s.wanderers.push(body(2.0, 16.0, 0.0)); // derivative touches zero at alignment
        assert!(
            wanderer_events(&s, StdInstant(-20.0), StdInstant(20.0))
                .iter()
                .all(|e| !matches!(e.kind, WandererEventKind::Retrograde { .. }))
        );
        s.wanderers[0] = body(2.0, 8.0, 0.0);
        assert!(wanderer_recurrence(&s, 0).is_none());
        assert!(wanderer_events(&s, StdInstant(-20.0), StdInstant(20.0)).is_empty());
    }

    #[test]
    fn single_star_stays_at_origin_and_flux_matches_inverse_square() {
        let s = system();
        assert_eq!(
            stellar_positions_at(&s, StdInstant(-100.0)),
            vec![OrbitalPosition {
                x_au: 0.0,
                y_au: 0.0
            }]
        );
        let light = stellar_illumination_at(&s, StdInstant(0.0));
        assert!((light.combined_flux_rel - s.star.luminosity.get()).abs() < 1e-12);
    }

    #[test]
    fn binary_positions_use_circumprimary_or_barycentric_frame() {
        for topology in [StellarTopology::WideBinary, StellarTopology::CloseBinary] {
            let mut s = generate(
                Seed(42),
                &SkyPins {
                    topology: Some(topology),
                    ..SkyPins::default()
                },
            )
            .unwrap()
            .value;
            s.stellar.companion.as_mut().unwrap().orbit.phase = 0.0;
            let c = s.stellar.companion.as_ref().unwrap();
            let p = stellar_positions_at(&s, StdInstant(0.0));
            assert!((p[1].x_au - p[0].x_au - c.orbit.semi_major_axis.get()).abs() < 1e-12);
            if topology == StellarTopology::WideBinary {
                assert_eq!(p[0].x_au, 0.0);
            } else {
                assert!(
                    (p[0].x_au * s.star.mass.get() + p[1].x_au * c.star.mass.get()).abs() < 1e-12
                );
            }
            let quarter = stellar_positions_at(&s, StdInstant(c.orbit.period.get() / 4.0));
            assert!(quarter[1].x_au.abs() < 1e-12 && quarter[1].y_au > 0.0);
            let light = stellar_illumination_at(&s, StdInstant(0.0));
            assert_eq!(light.sources.len(), 2);
            assert!(
                (light.combined_flux_rel - light.sources.iter().map(|l| l.flux_rel).sum::<f64>())
                    .abs()
                    < 1e-12
            );
        }
    }
}

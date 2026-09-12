//! Dated eclipses (Eclipse Seasons, SKY-6 close-out): node geometry as a
//! function of `WorldTime`. The node longitude is drawn; its regression
//! period, the moon's ecliptic latitude, and every dated event are pure
//! derivations (model card: the lunar-theory leading term).

use crate::calendar::Calendar;
use crate::moons::Moon;
use crate::system::StarSystem;
use crate::units::{StdDays, StdInstant};
use hornvale_kernel::math;

/// One angular-diameter unit (Sol from 1 AU ≈ Luna from Earth) in degrees
/// — the shared scale of `sun_angular_diameter_rel` and a moon's
/// `angular_diameter_rel` (declared approximation: the two units differ
/// by under 1%). Moved here from `provider.rs` (Eclipse Seasons).
/// type-audit: pending(wave-1)
/// plumb: pending(wave-1)
pub const ANGULAR_UNIT_DEG: f64 = 0.53;
/// How far (degrees of lunar ecliptic latitude) past the discs' own touch
/// an eclipse still falls somewhere on the world — the parallax allowance.
/// Calibrated so a Luna–Sol pair at 5.14° inclination eclipses at ~19% of
/// new moons (Earth's ~2.4 solar eclipses a year).
/// type-audit: pending(wave-1)
/// plumb: pending(wave-1)
pub const ECLIPSE_PARALLAX_DEG: f64 = 1.0;
/// The anchor's shadow threshold at the moon, as a fraction of the solar
/// threshold (declared approximation, Luna–Sol-calibrated to ~1.5 umbral
/// lunar eclipses/year — the umbra at Luna's distance is ≈ 2.6 lunar
/// radii). Below 1.0 because the solar threshold carries the
/// anywhere-on-the-world parallax allowance the lunar case doesn't need —
/// the shadow is one shadow for every observer.
/// type-audit: pending(wave-1)
/// plumb: pending(wave-1)
pub const LUNAR_SHADOW_FACTOR: f64 = 0.64;

/// The node threshold (degrees of lunar ecliptic latitude) inside which a
/// new moon eclipses the sun somewhere on the world: half the summed
/// discs plus the parallax allowance.
/// type-audit: pending(wave-1)
pub fn solar_eclipse_threshold_deg(sun_angular_rel: f64, moon_angular_rel: f64) -> f64 {
    ANGULAR_UNIT_DEG * (sun_angular_rel + moon_angular_rel) / 2.0 + ECLIPSE_PARALLAX_DEG
}

/// The fraction of syzygies whose ecliptic latitude falls inside
/// `threshold_deg` for an orbit inclined `inclination_deg`: the exact
/// spherical latitude is β = asin(sin i · sin u), which is sinusoidal in
/// u with amplitude `min(i, 180−i)` (never i itself, past 90°), so the
/// crossing fraction is P = (2/π)·asin(sin threshold / sin i), saturating
/// at 1 once the amplitude covers the whole threshold band. Symmetric
/// under i ↔ 180−i (sin i = sin(180−i)) — a prograde and a retrograde
/// orbit at complementary inclinations eclipse identically, as physics
/// requires. (The statistical twin of the dated scan —
/// `rate_matches_the_dated_scan` in provider.rs holds them together.)
/// type-audit: bare-ok(ratio)
pub fn node_crossing_chance(threshold_deg: f64, inclination_deg: f64) -> f64 {
    let sin_i = math::sin(inclination_deg.to_radians()).max(f64::MIN_POSITIVE);
    let x = (math::sin(threshold_deg.to_radians()) / sin_i).min(1.0);
    (2.0 / std::f64::consts::PI) * math::asin(x)
}

/// Nodal regression period from the lunar-theory leading term (declared
/// approximation, model card): P_node = (4/3)·Y²/(P_sid·cos i). Earth
/// check: ~17.9 yr against the true 18.61.
///
/// **Sign carries direction, not error.** For i < 90° (prograde orbits)
/// cos i > 0 and the nodes regress westward, as coded throughout this
/// module (`node_longitude_at` subtracts the turn fraction). Past i = 90°
/// (retrograde orbits, reachable since the-reckoning's `Formation`
/// epoch — `Capture` moons draw inclination up to 160°) cos i < 0 and
/// this deliberately returns a *negative* period: the orbital-torque
/// sign flips with the orbit's sense, so a retrograde orbit's nodes
/// regress **prograde** (eastward) — real orbital mechanics, not a bug.
/// `node_longitude_at`'s subtraction of a negative turn fraction adds,
/// producing exactly that eastward drift.
///
/// This is why the value is built with the bare tuple constructor
/// instead of `StdDays::new` (which enforces non-negative and would
/// reject it): `StdDays` elsewhere means an absolute or non-negative
/// duration, but here the sign is load-bearing. Numerically safe in
/// every reachable case — `|P_node| = (4/3)·Y²/(P_sid·|cos i|)` is
/// **minimized** at |cos i| = 1 (i.e. near i = 0°/180°; i = 90° is the
/// *safest* case — a polar orbit has no secular nodal precession at
/// all), giving `|P_node| ≥ (4/3)·Y²/P_sid`. `draconic_month` and
/// `eclipse_year` divide by `P_node`, so the danger case is
/// near-cancellation against Y or P_sid; that would need
/// |cos i| = (4/3)·(Y/P_sid) (to cancel against Y) or
/// |cos i| = (4/3)·(Y/P_sid)² (to cancel against P_sid) — both exceed
/// 4/3 > 1 whenever P_sid < Y, so both are **structurally unreachable**
/// (not merely improbable), since |cos i| ≤ 1 always. `P_sid < Y` isn't
/// assumed, it's *enforced*: `generate_moons` in `moons.rs` admits a
/// moon only within `0.4 · hill_radius_mm(..)` of its anchor, and that
/// Hill-sphere cap keeps every admitted moon's orbit — and hence its
/// sidereal period via Kepler's third law — far inside the anchor's own
/// orbit around the star, so `P_sid` never approaches `Y`. Confirmed
/// numerically by `node_regression_period_matches_the_180_minus_i_magnitude`
/// below (which pins the algebra at Luna's fixed Y/P_sid, not the
/// population's margin — the smallest measured margin across a live
/// world sample is 9.19× (`|P_node|/Y` at i≈0.97°), still far from 1).
///
/// The remaining fragility: `Formation::Capture` (The Reckoning) models
/// distant *irregular* satellites — precisely the population that sits
/// closest to the Hill-radius admission cap, i.e. closest to the margin
/// this safety argument depends on. A future campaign that widens how
/// far captured moons are allowed to orbit is what would erode this
/// margin, not a change to this module.
/// type-audit: pending(wave-1: inclination_deg)
pub fn node_regression_period(year: StdDays, sidereal: StdDays, inclination_deg: f64) -> StdDays {
    StdDays((4.0 / 3.0) * year.0 * year.0 / (sidereal.0 * math::cos(inclination_deg.to_radians())))
}

/// The ascending node's ecliptic longitude at `t`, degrees in [0, 360):
/// the genesis draw regressed westward one turn per regression period.
/// type-audit: pending(wave-1)
pub fn node_longitude_at(moon: &Moon, year: StdDays, t: StdInstant) -> f64 {
    let p = node_regression_period(year, moon.period, moon.inclination_deg);
    (moon.node_longitude_deg - 360.0 * t.0 / p.0).rem_euclid(360.0)
}

/// The moon's ecliptic latitude at `t`, degrees: the exact spherical form
/// β = asin(sin i · sin u), with u = L−Ω and L = L_sun + 360·phase reusing
/// the shipped phase machinery. Bounded by ±min(i, 180−i) — the true
/// ceiling for any inclination, including the retrograde range (i > 90°)
/// the small-angle form `i·sin(u)` used to overshoot: at i=160°, u=90° it
/// gives β = 20° = min(160°, 20°), matching a prograde 20° orbit exactly
/// (sin 160° = sin 20°), as physics requires. `None` if the moon has no
/// synodic cycle (degenerate P_sid ≥ Y).
/// type-audit: bare-ok(index: index), pending(wave-1: return)
pub fn moon_ecliptic_latitude_deg(
    calendar: &Calendar,
    moon: &Moon,
    index: usize,
    t: StdInstant,
) -> Option<f64> {
    let l_moon = moon_ecliptic_longitude_deg(calendar, index, t)?;
    let omega = node_longitude_at(moon, calendar.year_length(), t);
    let sin_i = math::sin(moon.inclination_deg.to_radians());
    let sin_u = math::sin((l_moon - omega).to_radians());
    Some(math::asin((sin_i * sin_u).clamp(-1.0, 1.0)).to_degrees())
}

/// The sun's apparent angular diameter (Luna-units) at `t`: the mean
/// orbital value scaled by the existing first-order eccentricity approximation.
/// The apsidal convention keeps perihelion at year phase 0.25. Evaluated at
/// the event, never cached (the tidal-braking seam). The exact orbital radius
/// remains available from `Calendar::anchor_orbital_state_at` for later
/// physical consumers without moving this committed compatibility output.
/// Invalid anchor state yields zero angular diameter (no eclipse cross-section).
/// type-audit: pending(wave-1)
pub fn sun_angular_rel_at(system: &StarSystem, calendar: &Calendar, t: StdInstant) -> f64 {
    let Some(state) = calendar.anchor_orbital_state_at(t) else {
        return 0.0;
    };
    let mean =
        crate::star::sun_angular_diameter_rel(&system.star, crate::Au(state.semi_major_axis));
    mean / state.legacy_anchor_radius_ratio()
}

/// Which body is darkened.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EclipseBody {
    /// A moon crosses the sun at new moon.
    Solar,
    /// The anchor's shadow crosses a full moon.
    Lunar,
}

/// How completely the body is darkened (binary this campaign; partiality
/// grading is explicitly deferred).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EclipseKind {
    /// The covering disc (or shadow) swallows the body whole.
    Total,
    /// The moon's disc is too small: a burning ring remains.
    Annular,
}

/// One dated eclipse: a syzygy whose ecliptic latitude fell inside the
/// node threshold.
/// type-audit: bare-ok(index: moon)
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct EclipseEvent {
    /// The syzygy, in absolute standard days.
    pub day: StdInstant,
    /// Which moon (distance-sorted index into `StarSystem::moons`).
    pub moon: usize,
    /// Solar (new moon) or lunar (full moon).
    pub body: EclipseBody,
    /// Total or annular (kind read at the event's own t — a borderline
    /// moon is total near aphelion and annular near perihelion).
    pub kind: EclipseKind,
}

/// Every dated eclipse in `[from, until]`, day-ascending (moon index as
/// the deterministic tie-break). Syzygies are closed-form — the synodic
/// phase is linear in `t` — so the scan visits each new (and, Task 5,
/// full) moon exactly, no sampling. A moon with no synodic cycle never
/// eclipses. Thresholds and the total/annular kind are evaluated at each
/// syzygy's own time (Task 3), never cached.
pub fn eclipse_events(
    system: &StarSystem,
    calendar: &Calendar,
    from: StdInstant,
    until: StdInstant,
) -> Vec<EclipseEvent> {
    let mut out = Vec::new();
    for (index, moon) in system.moons.iter().enumerate() {
        let Some(synodic) = calendar.synodic_month(index) else {
            continue;
        };
        let Some(phase0) = calendar.moon_phase(StdInstant(0.0), index) else {
            continue;
        };
        // Syzygy k of each family sits at t = (k + half − phase0)·synodic.
        for (half, body) in syzygy_families() {
            let k_min = (from.0 / synodic.0 + phase0 - half).ceil() as i64;
            let k_max = (until.0 / synodic.0 + phase0 - half).floor() as i64;
            for k in k_min..=k_max {
                let t = StdInstant((k as f64 + half - phase0) * synodic.0);
                if t.0 < from.0 || t.0 > until.0 {
                    continue;
                }
                let Some(beta) = moon_ecliptic_latitude_deg(calendar, moon, index, t) else {
                    continue;
                };
                let sun_angular = sun_angular_rel_at(system, calendar, t);
                let theta_solar =
                    solar_eclipse_threshold_deg(sun_angular, moon.angular_diameter_rel);
                let threshold = match body {
                    EclipseBody::Solar => theta_solar,
                    EclipseBody::Lunar => LUNAR_SHADOW_FACTOR * theta_solar,
                };
                if beta.abs() < threshold {
                    let kind = match body {
                        EclipseBody::Lunar => EclipseKind::Total,
                        EclipseBody::Solar if moon.angular_diameter_rel >= sun_angular => {
                            EclipseKind::Total
                        }
                        EclipseBody::Solar => EclipseKind::Annular,
                    };
                    out.push(EclipseEvent {
                        day: t,
                        moon: index,
                        body,
                        kind,
                    });
                }
            }
        }
    }
    out.sort_by(|a, b| a.day.0.total_cmp(&b.day.0).then(a.moon.cmp(&b.moon)));
    out
}

/// The syzygy families the scan walks.
fn syzygy_families() -> Vec<(f64, EclipseBody)> {
    vec![(0.0, EclipseBody::Solar), (0.5, EclipseBody::Lunar)]
}

/// Half-width of the full-omen band, degrees of latitude (declared
/// approximation, model card: real totality bands are ~1° of latitude;
/// ours is slightly generous so a band is findable at room scale).
/// type-audit: pending(wave-1)
/// plumb: pending(wave-1)
pub const TRACK_HALF_WIDTH_DEG: f64 = 2.0;

/// The moon's ecliptic longitude at `t`, degrees in [0, 360): the sun's
/// longitude plus the synodic phase (Eclipse Seasons — the positional
/// ephemeris half transits and standstills will reuse). `None` if the
/// moon has no synodic cycle.
/// type-audit: bare-ok(index: index), pending(wave-1: return)
pub fn moon_ecliptic_longitude_deg(
    calendar: &Calendar,
    index: usize,
    t: StdInstant,
) -> Option<f64> {
    // Preserve the established synodic-phase evaluation order. The lunar
    // orbit is circular in this substrate, so this is the same geometry as
    // `moon_orbital_state_at`, but retaining the legacy expression keeps
    // eclipse thresholds and committed prose byte-stable.
    let phase = calendar.moon_phase(t, index)?;
    let l_sun = 360.0 * calendar.year_phase(t);
    Some((l_sun + 360.0 * phase).rem_euclid(360.0))
}

/// The sub-solar longitude at `t`, degrees in [−180, 180): local noon of
/// the position-blind observer (day fraction 0.5) sits at longitude 0,
/// matching the locked convention (substellar point = prime meridian);
/// the sun sweeps westward on a prograde world, eastward on a retrograde
/// one (SKY-22). A locked world's sun is fixed at 0.
/// type-audit: pending(wave-1)
pub fn sub_solar_longitude_deg(calendar: &Calendar, t: StdInstant) -> f64 {
    let Some((_, fraction)) = calendar.local_day(t) else {
        return 0.0;
    };
    let direction = if calendar.is_retrograde() { 1.0 } else { -1.0 };
    (direction * (fraction - 0.5) * 360.0 + 180.0).rem_euclid(360.0) - 180.0
}

/// A solar eclipse's shadow geometry: a latitude band swept across
/// longitudes as the world turns under the shadow.
/// type-audit: pending(wave-1: center_lat_deg), pending(wave-1: half_width_deg), pending(wave-1: start_lon_deg), pending(wave-1: end_lon_deg), pending(wave-1: duration_days), pending(wave-1: sweep_deg), bare-ok(flag: global_coverage)
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct GroundTrack {
    /// Latitude of the band's center at mid-event, degrees: the sub-solar
    /// latitude displaced poleward by the ecliptic-latitude miss.
    pub center_lat_deg: f64,
    /// Half-width of the full-omen band, degrees of latitude.
    pub half_width_deg: f64,
    /// Sub-solar longitude when the crossing begins, degrees [−180, 180).
    pub start_lon_deg: f64,
    /// Sub-solar longitude when the crossing ends, degrees [−180, 180).
    pub end_lon_deg: f64,
    /// Crossing duration, standard days (the moon's synodic drift across
    /// the combined discs).
    pub duration_days: f64,
    /// Signed, unwrapped surface-longitude sweep during the crossing,
    /// degrees. Positive is eastward and negative is westward; magnitudes
    /// greater than 360 retain every completed turn.
    pub sweep_deg: f64,
    /// Whether the sweep reaches every surface longitude at least once.
    pub global_coverage: bool,
}

/// The ground track of a dated solar eclipse; `None` for a lunar event —
/// the anchor's shadow is one shadow for the whole night side. The
/// latitude mapping is a declared approximation (model card): center =
/// solar declination + (β/θ)·(90° − |declination|), so a central pass
/// tracks the sub-solar latitude and a threshold-grazing one exits at a
/// pole.
pub fn ground_track(
    system: &StarSystem,
    calendar: &Calendar,
    event: &EclipseEvent,
) -> Option<GroundTrack> {
    if event.body == EclipseBody::Lunar {
        return None;
    }
    let moon = &system.moons[event.moon];
    let beta = moon_ecliptic_latitude_deg(calendar, moon, event.moon, event.day)?;
    let sun_angular = sun_angular_rel_at(system, calendar, event.day);
    let theta = solar_eclipse_threshold_deg(sun_angular, moon.angular_diameter_rel);
    let dec = calendar.solar_declination(event.day);
    let center_lat_deg = (dec + (beta / theta) * (90.0 - dec.abs())).clamp(-90.0, 90.0);
    let synodic = calendar.synodic_month(event.moon)?;
    let combined_deg = ANGULAR_UNIT_DEG * (sun_angular + moon.angular_diameter_rel);
    let duration_days = combined_deg / (360.0 / synodic.0);
    let start_lon_deg =
        sub_solar_longitude_deg(calendar, StdInstant(event.day.0 - duration_days / 2.0));
    let sweep_deg = rotation_sweep_deg(calendar, duration_days);
    let end_lon_deg = (start_lon_deg + sweep_deg + 180.0).rem_euclid(360.0) - 180.0;
    Some(GroundTrack {
        center_lat_deg,
        half_width_deg: TRACK_HALF_WIDTH_DEG,
        start_lon_deg,
        end_lon_deg,
        duration_days,
        sweep_deg,
        global_coverage: sweep_deg.abs() >= 360.0,
    })
}

/// What a placed observer sees of a dated solar eclipse.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EclipseSight {
    /// Inside the band under a covering moon: the sun devoured whole.
    WholeSun,
    /// Inside the band under a too-small moon: the burning ring.
    BurningRing,
    /// On the day side but outside the band: the sun is bitten.
    Bitten,
    /// On the night side: nothing.
    Unseen,
}

/// Which half of the world contains an observer during an eclipse.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EclipseSide {
    /// The hemisphere facing the star.
    Day,
    /// The hemisphere facing away from the star.
    Night,
}

/// The event-scale geographic region in which an eclipse is visible.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum EclipseRegion {
    /// A solar eclipse's bounded shadow track.
    GroundTrack(GroundTrack),
    /// The entire night hemisphere for a lunar eclipse.
    NightHemisphere,
}

/// What kind of eclipse visibility applies to one observer.
/// type-audit: bare-ok(flag: Lunar.visible)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EclipseVisibility {
    /// The existing solar sight tier.
    Solar(EclipseSight),
    /// Whether the eclipsed moon is above the observer's horizon.
    Lunar {
        /// True on the night side, false on the day side.
        visible: bool,
    },
}

/// One observer's derived result for one dated eclipse.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct EclipseObserverResult {
    /// Whether the observer is on the day or night side at the event
    /// midpoint. A central solar visibility tier is event-wide and may
    /// describe a local passage away from this midpoint.
    pub side: EclipseSide,
    /// The applicable solar tier or lunar visibility.
    pub visibility: EclipseVisibility,
    /// The event's physical geographic region.
    pub region: EclipseRegion,
}

/// Derive one observer's result for a dated eclipse. Latitude must be finite
/// and lie in `[-90, 90]`; finite longitudes are normalized to the module's
/// `[-180, 180)` convention before the existing sight helpers are applied.
/// `None` reports invalid observer coordinates (or a malformed solar event
/// whose track cannot be derived).
/// type-audit: pending(wave-1: latitude), pending(wave-1: longitude)
pub fn eclipse_observer_result(
    system: &StarSystem,
    calendar: &Calendar,
    event: &EclipseEvent,
    latitude: f64,
    longitude: f64,
) -> Option<EclipseObserverResult> {
    if !latitude.is_finite() || !(-90.0..=90.0).contains(&latitude) {
        return None;
    }
    let longitude = normalize_longitude_deg(longitude)?;
    let side = eclipse_side(calendar, event.day, longitude);
    let (visibility, region) = match event.body {
        EclipseBody::Solar => {
            let track = ground_track(system, calendar, event)?;
            (
                EclipseVisibility::Solar(solar_sight_in_region(
                    event, latitude, longitude, side, track,
                )),
                EclipseRegion::GroundTrack(track),
            )
        }
        EclipseBody::Lunar => (
            EclipseVisibility::Lunar {
                visible: side == EclipseSide::Night,
            },
            EclipseRegion::NightHemisphere,
        ),
    };
    Some(EclipseObserverResult {
        side,
        visibility,
        region,
    })
}

fn normalize_longitude_deg(longitude: f64) -> Option<f64> {
    longitude
        .is_finite()
        .then(|| (longitude + 180.0).rem_euclid(360.0) - 180.0)
}

fn longitude_delta_deg(from: f64, to: f64) -> f64 {
    (to - from + 180.0).rem_euclid(360.0) - 180.0
}

fn eclipse_side(calendar: &Calendar, day: StdInstant, longitude: f64) -> EclipseSide {
    let subsolar = sub_solar_longitude_deg(calendar, day);
    if longitude_delta_deg(subsolar, longitude).abs() < 90.0 {
        EclipseSide::Day
    } else {
        EclipseSide::Night
    }
}

fn rotation_sweep_deg(calendar: &Calendar, duration_days: f64) -> f64 {
    let Some(day_length) = calendar.day_length() else {
        return 0.0;
    };
    let direction = if calendar.is_retrograde() { 1.0 } else { -1.0 };
    direction * duration_days / day_length.0 * 360.0
}

fn longitude_in_track(longitude: f64, track: GroundTrack) -> bool {
    if track.global_coverage {
        return true;
    }
    if longitude_delta_deg(track.start_lon_deg, longitude).abs() <= f64::EPSILON
        || longitude_delta_deg(track.end_lon_deg, longitude).abs() <= f64::EPSILON
    {
        return true;
    }
    if track.sweep_deg.abs() <= f64::EPSILON {
        return longitude_delta_deg(track.start_lon_deg, longitude).abs() <= f64::EPSILON;
    }
    let direction = track.sweep_deg.signum();
    let offset_deg = (direction * (longitude - track.start_lon_deg)).rem_euclid(360.0);
    offset_deg <= track.sweep_deg.abs() + f64::EPSILON
}

fn solar_sight_in_region(
    event: &EclipseEvent,
    latitude: f64,
    longitude: f64,
    side: EclipseSide,
    track: GroundTrack,
) -> EclipseSight {
    if (latitude - track.center_lat_deg).abs() <= track.half_width_deg
        && longitude_in_track(longitude, track)
    {
        match event.kind {
            EclipseKind::Total => EclipseSight::WholeSun,
            EclipseKind::Annular => EclipseSight::BurningRing,
        }
    } else if side == EclipseSide::Night {
        EclipseSight::Unseen
    } else {
        EclipseSight::Bitten
    }
}

/// Which tier of the omen an observer at (`latitude`, `longitude`) sees
/// for a dated solar `event`. Day-side membership is the equatorial
/// half-day approximation: within 90° of the sub-solar longitude
/// (declared; matches the locked-culling geometry).
/// type-audit: pending(wave-1: latitude), pending(wave-1: longitude)
pub fn solar_eclipse_sight(
    system: &StarSystem,
    calendar: &Calendar,
    event: &EclipseEvent,
    latitude: f64,
    longitude: f64,
) -> EclipseSight {
    if !latitude.is_finite() || !(-90.0..=90.0).contains(&latitude) {
        return EclipseSight::Unseen;
    }
    let Some(longitude) = normalize_longitude_deg(longitude) else {
        return EclipseSight::Unseen;
    };
    let Some(track) = ground_track(system, calendar, event) else {
        return EclipseSight::Unseen;
    };
    solar_sight_in_region(
        event,
        latitude,
        longitude,
        eclipse_side(calendar, event.day, longitude),
        track,
    )
}

/// Whether an observer at `longitude` has the full moon in their sky for
/// a dated lunar `event`: the night side, the complement of the solar
/// half-day window.
/// type-audit: pending(wave-1: longitude), bare-ok(flag: return)
pub fn lunar_eclipse_seen(calendar: &Calendar, event: &EclipseEvent, longitude: f64) -> bool {
    normalize_longitude_deg(longitude)
        .is_some_and(|longitude| eclipse_side(calendar, event.day, longitude) == EclipseSide::Night)
}

/// The draconic month — the moon's period relative to its regressing
/// node: frequencies add because the node moves against the motion.
/// type-audit: pending(wave-1: inclination_deg)
pub fn draconic_month(year: StdDays, sidereal: StdDays, inclination_deg: f64) -> StdDays {
    let p_node = node_regression_period(year, sidereal, inclination_deg);
    StdDays(1.0 / (1.0 / sidereal.0 + 1.0 / p_node.0))
}

/// The eclipse year — the sun's return period to the (regressing) node
/// line: 1/(1/Y + 1/P_node). Luna check ≈ 346 days.
pub fn eclipse_year(year: StdDays, node_period: StdDays) -> StdDays {
    StdDays(1.0 / (1.0 / year.0 + 1.0 / node_period.0))
}

/// A near-commensurability between the synodic and draconic months: the
/// world's saros-analog. `node_slip_deg` is the draconic-phase miss per
/// return, in degrees — the drift that ages a series.
/// type-audit: bare-ok(index: synodic_count), bare-ok(index: draconic_count), pending(wave-1: node_slip_deg)
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct EclipseCycle {
    /// Synodic months per return.
    pub synodic_count: u32,
    /// Draconic months per return (the nearest integer).
    pub draconic_count: u32,
    /// The return period, standard days.
    pub period: StdDays,
    /// Node-phase slip per return, degrees.
    pub node_slip_deg: f64,
}

/// The recurrence ladder for one moon and eclipse family.
/// type-audit: bare-ok(index: moon), bare-ok(count: series_returns), pending(wave-1: exeligmos_node_slip_deg), pending(wave-1: exeligmos_surface_longitude_shift_deg), pending(wave-1: parade_days_per_year)
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct EclipseRecurrence {
    /// Distance-sorted moon index.
    pub moon: usize,
    /// Solar or lunar eclipse family.
    pub body: EclipseBody,
    /// The moon's draconic month.
    pub draconic_month: StdDays,
    /// The sun's return to the moon's node line.
    pub eclipse_year: StdDays,
    /// The selected bounded synodic/draconic return.
    pub cycle: EclipseCycle,
    /// Estimated number of returns in the eclipse series.
    pub series_returns: u32,
    /// Estimated duration of the eclipse series.
    pub series_lifetime: StdDays,
    /// Three selected-cycle periods.
    pub exeligmos_period: StdDays,
    /// Node-phase slip accumulated across three returns.
    pub exeligmos_node_slip_deg: f64,
    /// Signed residual surface-longitude shift after three returns, degrees
    /// in `[-180, 180)`. Positive is eastward and negative is westward; zero
    /// is exact terrestrial longitude closure. This rotational closure is
    /// distinct from orbital node-phase slip.
    pub exeligmos_surface_longitude_shift_deg: f64,
    /// Eclipse-season migration through one civil year.
    pub parade_days_per_year: f64,
}

/// Derive recurrence records in moon order, solar then lunar per moon.
pub fn eclipse_recurrences(system: &StarSystem, calendar: &Calendar) -> Vec<EclipseRecurrence> {
    let year = calendar.year_length();
    let mean_sun = crate::star::sun_angular_diameter_rel(&system.star, system.anchor.orbit);
    let mut recurrences = Vec::with_capacity(system.moons.len() * 2);
    for (moon_index, moon) in system.moons.iter().enumerate() {
        let Some(synodic) = calendar.synodic_month(moon_index) else {
            continue;
        };
        let draconic = draconic_month(year, moon.period, moon.inclination_deg);
        let Some(cycle) = best_cycle(synodic, draconic) else {
            continue;
        };
        let node_period = node_regression_period(year, moon.period, moon.inclination_deg);
        let eclipse_year = eclipse_year(year, node_period);
        let solar_threshold = solar_eclipse_threshold_deg(mean_sun, moon.angular_diameter_rel);
        let exeligmos_period = StdDays(3.0 * cycle.period.0);
        let exeligmos_surface_longitude_shift_deg =
            (rotation_sweep_deg(calendar, exeligmos_period.0) + 180.0).rem_euclid(360.0) - 180.0;
        for (_, body) in syzygy_families() {
            let threshold = match body {
                EclipseBody::Solar => solar_threshold,
                EclipseBody::Lunar => LUNAR_SHADOW_FACTOR * solar_threshold,
            };
            let returns = series_returns(&cycle, threshold, moon.inclination_deg);
            recurrences.push(EclipseRecurrence {
                moon: moon_index,
                body,
                draconic_month: draconic,
                eclipse_year,
                cycle,
                series_returns: returns,
                series_lifetime: StdDays(returns as f64 * cycle.period.0),
                exeligmos_period,
                exeligmos_node_slip_deg: 3.0 * cycle.node_slip_deg,
                exeligmos_surface_longitude_shift_deg,
                parade_days_per_year: parade_days_per_year(year, eclipse_year),
            });
        }
    }
    recurrences
}

/// The longest-lived eclipse cycle up to 300 synodic months: the s
/// minimizing the draconic-phase miss (ties broken toward the shorter
/// cycle, deterministically). `None` for degenerate inputs.
pub fn best_cycle(synodic: StdDays, draconic: StdDays) -> Option<EclipseCycle> {
    if !synodic.0.is_finite() || synodic.0 <= 0.0 || !draconic.0.is_finite() || draconic.0 <= 0.0 {
        return None;
    }
    let ratio = synodic.0 / draconic.0;
    let mut best: Option<(f64, u32, u32)> = None;
    for s in 1..=300u32 {
        let x = s as f64 * ratio;
        let d = x.round();
        let miss = (x - d).abs();
        let better = match best {
            None => true,
            Some((b_miss, ..)) => miss < b_miss,
        };
        if better && d >= 1.0 {
            best = Some((miss, s, d as u32));
        }
    }
    let (miss, s, d) = best?;
    Some(EclipseCycle {
        synodic_count: s,
        draconic_count: d,
        period: StdDays(s as f64 * synodic.0),
        node_slip_deg: miss * 360.0,
    })
}

/// How many returns a series survives: the ecliptic-latitude walk per
/// return is the exact spherical form Δβ = asin(sin i · sin slip)
/// (bounded by ±min(i, 180−i), same construction as
/// `moon_ecliptic_latitude_deg`); a series is born grazing one edge of the
/// ±θ window and dies at the other, so it lives ≈ 2θ / Δβ returns.
/// Slipless (or, at retrograde inclinations, edge-saturated) cycles
/// saturate at 10,000. Symmetric under i ↔ 180−i (sin i = sin(180−i)), as
/// physics requires — the small-angle form `i·sin(slip)` this replaces
/// was not, and overshot past 90° the same way the two functions fixed in
/// 8711a5a did.
/// type-audit: pending(wave-1: threshold_deg), pending(wave-1: inclination_deg), bare-ok(index: return)
pub fn series_returns(cycle: &EclipseCycle, threshold_deg: f64, inclination_deg: f64) -> u32 {
    let sin_i = math::sin(inclination_deg.to_radians());
    let slip = cycle.node_slip_deg.to_radians();
    let d_beta = math::asin((sin_i * math::sin(slip)).clamp(-1.0, 1.0))
        .to_degrees()
        .abs();
    if d_beta <= 0.0 {
        return 10_000;
    }
    ((2.0 * threshold_deg / d_beta).floor() as u32).min(10_000)
}

/// How many days per civil year the eclipse seasons migrate backward
/// through the calendar: Y − eclipse-year. Luna check ≈ 19.
/// type-audit: bare-ok(ratio)
pub fn parade_days_per_year(year: StdDays, eclipse_year: StdDays) -> f64 {
    year.0 - eclipse_year.0
}

/// How many distinct integer days in `events` carry events from two or
/// more different moons — the grand-omen coincidence count. Zero for
/// worlds with fewer than two moons, by construction.
/// type-audit: bare-ok(index: return)
pub fn coincidence_days(events: &[EclipseEvent]) -> u32 {
    let mut days: Vec<(i64, usize)> = events
        .iter()
        .map(|e| (e.day.0.floor() as i64, e.moon))
        .collect();
    days.sort();
    days.dedup();
    let mut count = 0u32;
    let mut idx = 0;
    while idx < days.len() {
        let day = days[idx].0;
        let mut moons = 0;
        while idx < days.len() && days[idx].0 == day {
            moons += 1;
            idx += 1;
        }
        if moons >= 2 {
            count += 1;
        }
    }
    count
}

/// Sol + Luna exactly: 1 M☉, 1 AU, 365.25-day year, 24-hour day, zero
/// forcing, one moon at Luna's elements with node at 0°. The calibration
/// fixture Tasks 3–11 reuse — module scope (not inside `mod tests`) so
/// `provider.rs`'s consistency test can reach it as
/// `crate::eclipses::luna_sol()`.
#[cfg(test)]
pub(crate) fn luna_sol() -> (crate::system::StarSystem, Calendar) {
    use crate::calendar::calendar_of;
    use crate::pins::{ForcingPin, MoonsPin, RotationPin, SkyPins};
    use crate::system::generate;
    use crate::units::{Au, LunarMasses, Megameters, SolarLuminosities, SolarMasses};
    use hornvale_kernel::Seed;
    let pins = SkyPins {
        rotation: Some(RotationPin::PeriodHours(24.0)),
        moons: Some(MoonsPin::exact(1).unwrap()),
        forcing: Some(ForcingPin::Zero),
        ..SkyPins::default()
    };
    let mut system = generate(Seed(42), &pins).unwrap().value;
    // Force Sol/Luna numbers onto the generated skeleton.
    system.star.mass = SolarMasses(1.0);
    system.star.luminosity = SolarLuminosities(1.0);
    system.anchor.orbit = Au(1.0);
    system.anchor.year = StdDays(365.25);
    system.moons[0] = Moon {
        mass: LunarMasses(1.0),
        distance: Megameters(384.4),
        period: StdDays(27.32),
        angular_diameter_rel: 1.0,
        tide_rel: 1.0,
        inclination_deg: 5.14,
        node_longitude_deg: 0.0,
        formation: crate::moons::Formation::GiantImpact,
        density: crate::units::GramsPerCm3(3.34),
        age: crate::units::Gyr(4.51),
    };
    let calendar = calendar_of(&system);
    (system, calendar)
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Earth check for the regression period: Y=365.25, P_sid=27.32,
    /// i=5.14° gives ~6540 days (~17.9 yr) against the true 18.61 yr —
    /// the declared approximation's accuracy band.
    #[test]
    fn node_regression_reproduces_the_lunar_magnitude() {
        let p = node_regression_period(StdDays(365.25), StdDays(27.32), 5.14);
        assert!((6000.0..7000.0).contains(&p.0), "P_node {} days", p.0);
    }

    /// The-reckoning regression: a retrograde orbit (i > 90°) and its
    /// prograde mirror (180−i) share the same |P_node| — the physical
    /// torque magnitude only depends on |cos i| — but flip sign, since a
    /// retrograde orbit's nodes precess the opposite way. Neither is
    /// NaN/infinite, and at Luna's fixed Y/P_sid, |P_node| stays far
    /// above both. This pins the *formula's algebra*, not the
    /// population's margin — the structural argument for why
    /// `draconic_month`/`eclipse_year` never see a near-cancellation for
    /// *any* admitted moon (regardless of Y/P_sid) lives in
    /// `node_regression_period`'s own doc comment, tied to the Hill-radius
    /// admission cut in `moons.rs`.
    #[test]
    fn node_regression_period_matches_the_180_minus_i_magnitude() {
        let year = StdDays(365.25);
        let sidereal = StdDays(27.32);
        for i in [10.0, 20.0, 45.0, 60.0, 89.0] {
            let prograde = node_regression_period(year, sidereal, i);
            let retrograde = node_regression_period(year, sidereal, 180.0 - i);
            assert!(prograde.0.is_finite() && retrograde.0.is_finite());
            assert!(
                prograde.0 > 0.0,
                "prograde P_node should be positive: {}",
                prograde.0
            );
            assert!(
                retrograde.0 < 0.0,
                "retrograde P_node should be negative: {}",
                retrograde.0
            );
            assert!(
                (prograde.0 - retrograde.0.abs()).abs() < 1e-6,
                "|P_node| mismatch at i={i}: {} vs {}",
                prograde.0,
                retrograde.0.abs()
            );
            assert!(
                prograde.0.abs() > year.0 && prograde.0.abs() > sidereal.0,
                "P_node magnitude {} should dwarf Y={} and P_sid={}",
                prograde.0,
                year.0,
                sidereal.0
            );
        }
    }

    #[test]
    fn nodes_regress_westward_one_turn_per_period() {
        let moon = test_moon(5.14, 40.0);
        let year = StdDays(365.25);
        let p = node_regression_period(year, moon.period, moon.inclination_deg);
        let start = node_longitude_at(&moon, year, StdInstant(0.0));
        assert_eq!(start, 40.0);
        let quarter = node_longitude_at(&moon, year, StdInstant(p.0 / 4.0));
        assert!(((start - quarter).rem_euclid(360.0) - 90.0).abs() < 1e-6);
        let full = node_longitude_at(&moon, year, StdInstant(p.0));
        assert!((full - start).rem_euclid(360.0) < 1e-6);
    }

    #[test]
    fn ecliptic_latitude_is_bounded_by_the_inclination() {
        let (system, calendar) = super::luna_sol();
        let moon = &system.moons[0];
        for k in 0..500 {
            let t = StdInstant(k as f64 * 13.7);
            let b = moon_ecliptic_latitude_deg(&calendar, moon, 0, t).unwrap();
            assert!(b.abs() <= moon.inclination_deg + 1e-9, "β {b} at t {}", t.0);
        }
    }

    /// The-reckoning regression: past 90° the small-angle form used to
    /// overshoot (claiming |β| up to i, e.g. 160°) — geometrically
    /// impossible since the ecliptic latitude tops out at 90°. The exact
    /// spherical form is bounded by `min(i, 180−i)` instead, and a
    /// retrograde orbit at i reaches exactly the same peak as its
    /// prograde mirror at 180−i (sin i = sin(180−i)).
    #[test]
    fn ecliptic_latitude_is_bounded_by_min_i_and_180_minus_i_for_retrograde_moons() {
        let (mut system, _) = super::luna_sol();
        for i in [20.0, 90.0, 117.0, 160.0] {
            system.moons[0].inclination_deg = i;
            let calendar = crate::calendar::calendar_of(&system);
            let moon = &system.moons[0];
            let cap = i.min(180.0 - i);
            let mut peak: f64 = 0.0;
            for k in 0..500 {
                let t = StdInstant(k as f64 * 13.7);
                let b = moon_ecliptic_latitude_deg(&calendar, moon, 0, t).unwrap();
                assert!(b.abs() <= cap + 1e-6, "i={i}: β {b} exceeds cap {cap}");
                peak = peak.max(b.abs());
            }
            assert!(
                (peak - cap).abs() < 2.0,
                "i={i}: peak {peak} should approach cap {cap}"
            );
        }
    }

    /// The-reckoning regression: `node_crossing_chance` must agree for an
    /// orbit at i and its retrograde mirror at 180−i — the old linear
    /// form gave an 8x difference between i=20° and i=160° at Luna's
    /// threshold; the exact form is identical by construction
    /// (sin i = sin(180−i)).
    #[test]
    fn node_crossing_chance_is_symmetric_under_i_and_180_minus_i() {
        let threshold = 10.66; // Luna–Sol solar threshold, ANGULAR_UNIT_DEG-scaled.
        for i in [5.0, 20.0, 45.0, 60.0, 89.0] {
            let prograde = node_crossing_chance(threshold, i);
            let retrograde = node_crossing_chance(threshold, 180.0 - i);
            assert!(
                (prograde - retrograde).abs() < 1e-9,
                "i={i}: chance {prograde} vs retrograde {retrograde}"
            );
        }
        let at_20 = node_crossing_chance(threshold, 20.0);
        let at_160 = node_crossing_chance(threshold, 160.0);
        assert!(
            (at_20 - at_160).abs() < 1e-9,
            "the review's headline case: chance(20°)={at_20} vs chance(160°)={at_160}"
        );
    }

    /// The degenerate i=0 case (a flat, unbroken orbit) must not divide by
    /// zero: every syzygy crosses the node, so the chance saturates at 1.
    #[test]
    fn node_crossing_chance_handles_a_flat_orbit_without_dividing_by_zero() {
        let chance = node_crossing_chance(10.0, 0.0);
        assert!(chance.is_finite());
        assert!((chance - 1.0).abs() < 1e-6, "flat-orbit chance {chance}");
    }

    /// The-reckoning regression: a third site with the same linear-in-i
    /// defect `node_crossing_chance` and `moon_ecliptic_latitude_deg` had
    /// (8711a5a fixed those two; this one was missed). `series_returns`
    /// must agree for an orbit at i and its retrograde mirror at 180−i —
    /// the old form `i·sin(slip)` gave an 8.75x difference between i=20°
    /// and i=160° at Luna's threshold with a representative node slip
    /// (35 vs 4 returns); the exact spherical form is identical by
    /// construction (sin i = sin(180−i)).
    #[test]
    fn series_returns_is_symmetric_under_i_and_180_minus_i() {
        let cycle = EclipseCycle {
            synodic_count: 1,
            draconic_count: 1,
            period: StdDays(29.53),
            node_slip_deg: 1.72,
        };
        let threshold = 10.66; // Luna–Sol solar threshold, ANGULAR_UNIT_DEG-scaled.
        for i in [5.0, 20.0, 45.0, 60.0, 89.0] {
            let prograde = series_returns(&cycle, threshold, i);
            let retrograde = series_returns(&cycle, threshold, 180.0 - i);
            assert_eq!(
                prograde, retrograde,
                "i={i}: returns {prograde} vs retrograde {retrograde}"
            );
        }
        let at_20 = series_returns(&cycle, threshold, 20.0);
        let at_160 = series_returns(&cycle, threshold, 160.0);
        assert_eq!(
            at_20, at_160,
            "the review's headline case: returns(20°)={at_20} vs returns(160°)={at_160}"
        );
    }

    /// With zero eccentricity the event-time sun is the mean sun exactly.
    #[test]
    fn a_circular_orbit_keeps_the_mean_sun() {
        let (system, calendar) = super::luna_sol();
        let mean = crate::star::sun_angular_diameter_rel(&system.star, system.anchor.orbit);
        for k in 0..12 {
            let t = StdInstant(k as f64 * 30.0);
            assert_eq!(sun_angular_rel_at(&system, &calendar, t), mean);
        }
    }

    /// Sol check: e = 0.0167 swings the apparent sun ±1.7% over the year,
    /// perihelion-largest.
    #[test]
    fn eccentricity_swings_the_sun_size_sol_scale() {
        let (mut system, _) = super::luna_sol();
        system.forcing.ecc_mean = 0.0167;
        system.forcing.ecc_amp = 0.0;
        let calendar = crate::calendar::calendar_of(&system);
        let mean = crate::star::sun_angular_diameter_rel(&system.star, system.anchor.orbit);
        let sizes: Vec<f64> = (0..360)
            .map(|d| sun_angular_rel_at(&system, &calendar, StdInstant(d as f64)))
            .collect();
        let max = sizes.iter().cloned().fold(f64::MIN, f64::max);
        let min = sizes.iter().cloned().fold(f64::MAX, f64::min);
        assert!(
            (max / mean - 1.017).abs() < 2e-3,
            "max ratio {}",
            max / mean
        );
        assert!(
            (min / mean - 0.983).abs() < 2e-3,
            "min ratio {}",
            min / mean
        );
    }

    #[test]
    fn ephemeris_and_eclipse_project_the_same_calendar_anchor_state() {
        let (mut system, _) = super::luna_sol();
        system.forcing.year_phase_offset = 0.125;
        system.forcing.ecc_mean = 0.2;
        system.forcing.ecc_amp = 0.0;
        let calendar = crate::calendar::calendar_of(&system);
        let mean = crate::star::sun_angular_diameter_rel(&system.star, system.anchor.orbit);
        // Periapsis and apoapsis on either side of genesis. The existing
        // scene is circular; the eclipse diameter keeps its first-order scale.
        for (years, phase, radius_ratio, y_sign) in
            [(-0.875, 0.25, 0.8, 1.0), (0.625, 0.75, 1.2, -1.0)]
        {
            let instant = StdInstant(system.anchor.year.get() * years);
            let state = calendar.anchor_orbital_state_at(instant).unwrap();
            let position = crate::ephemeris::anchor_position_at(&system, instant);
            assert!((calendar.year_phase(instant) - phase).abs() < 1e-12);
            assert!((state.radius / system.anchor.orbit.get() - radius_ratio).abs() < 1e-12);
            assert!(position.x_au.abs() < 1e-12);
            assert!((position.y_au / system.anchor.orbit.get() - y_sign).abs() < 1e-12);
            assert!(
                (sun_angular_rel_at(&system, &calendar, instant) / mean - 1.0 / radius_ratio).abs()
                    < 1e-12
            );
        }
        // The supplied calendar owns the evaluated orbital elements. A stale
        // system forcing record must not become a second eclipse geometry.
        system.forcing.ecc_mean = 0.0;
        let instant = StdInstant(system.anchor.year.get() * 0.125);
        assert!((sun_angular_rel_at(&system, &calendar, instant) / mean - 1.25).abs() < 1e-12);
    }

    #[test]
    fn invalid_anchor_state_has_total_ephemeris_and_eclipse_fallbacks() {
        for (period, radius, eccentricity) in [
            (0.0, 1.0, 0.2),
            (f64::INFINITY, 1.0, 0.2),
            (8.0, f64::NAN, 0.2),
            (8.0, 1.0, 1.0),
        ] {
            let (mut system, _) = super::luna_sol();
            system.anchor.year = StdDays(period);
            system.anchor.orbit = crate::Au(radius);
            system.forcing.ecc_mean = eccentricity;
            system.forcing.ecc_amp = 0.0;
            let calendar = crate::calendar_of(&system);
            let instant = StdInstant(0.0);
            assert!(calendar.anchor_orbital_state_at(instant).is_none());
            assert_eq!(
                crate::ephemeris::anchor_position_at(&system, instant),
                crate::OrbitalPosition {
                    x_au: 0.0,
                    y_au: 0.0
                }
            );
            assert_eq!(sun_angular_rel_at(&system, &calendar, instant), 0.0);
        }
    }

    fn test_moon(inclination_deg: f64, node_longitude_deg: f64) -> Moon {
        use crate::units::{GramsPerCm3, Gyr, LunarMasses, Megameters};
        Moon {
            mass: LunarMasses(1.0),
            distance: Megameters(384.4),
            period: StdDays(27.32),
            angular_diameter_rel: 1.0,
            tide_rel: 1.0,
            inclination_deg,
            node_longitude_deg,
            formation: crate::moons::Formation::GiantImpact,
            density: GramsPerCm3(3.34),
            age: Gyr(4.51),
        }
    }

    /// Luna–Sol calibration: the dated scan reproduces Earth's ~2.4 solar
    /// eclipses/year over a 50-year window (the anywhere-on-the-world
    /// count the parallax allowance is calibrated to).
    #[test]
    fn luna_sol_dates_earths_solar_cadence() {
        let (system, calendar) = luna_sol();
        let years = 50.0;
        let events = eclipse_events(
            &system,
            &calendar,
            StdInstant(0.0),
            StdInstant(365.25 * years),
        );
        let solar = events
            .iter()
            .filter(|e| matches!(e.body, EclipseBody::Solar))
            .count() as f64;
        let per_year = solar / years;
        assert!(
            (1.9..=2.9).contains(&per_year),
            "solar eclipses/year {per_year}"
        );
    }

    /// Luna–Sol calibration for the shadow: ~1.5 umbral lunar
    /// eclipses/year. (Fewer than solar — the solar count includes the
    /// anywhere-on-the-world parallax allowance; the lunar one is the
    /// same for every observer on the night side.)
    #[test]
    fn luna_sol_dates_earths_lunar_cadence() {
        let (system, calendar) = luna_sol();
        let years = 50.0;
        let events = eclipse_events(
            &system,
            &calendar,
            StdInstant(0.0),
            StdInstant(365.25 * years),
        );
        let lunar = events
            .iter()
            .filter(|e| matches!(e.body, EclipseBody::Lunar))
            .count() as f64;
        let per_year = lunar / years;
        assert!(
            (1.1..=1.9).contains(&per_year),
            "lunar eclipses/year {per_year}"
        );
    }

    /// Every lunar event sits at a full moon, and every lunar event is
    /// Total (partiality grading is deferred).
    #[test]
    fn lunar_events_fall_at_full_moons() {
        let (system, calendar) = luna_sol();
        let events = eclipse_events(
            &system,
            &calendar,
            StdInstant(0.0),
            StdInstant(365.25 * 20.0),
        );
        let lunar: Vec<_> = events
            .iter()
            .filter(|e| matches!(e.body, EclipseBody::Lunar))
            .collect();
        assert!(!lunar.is_empty());
        for e in lunar {
            let phase = calendar.moon_phase(e.day, e.moon).unwrap();
            assert!((phase - 0.5).abs() < 1e-6, "phase {phase}");
            assert_eq!(e.kind, EclipseKind::Total);
        }
    }

    /// Every dated solar event sits at a new moon and inside the node
    /// threshold — the geometry cannot lie.
    #[test]
    fn solar_events_fall_at_new_moons_inside_the_threshold() {
        let (system, calendar) = luna_sol();
        let events = eclipse_events(
            &system,
            &calendar,
            StdInstant(0.0),
            StdInstant(365.25 * 20.0),
        );
        assert!(!events.is_empty());
        for e in events
            .iter()
            .filter(|e| matches!(e.body, EclipseBody::Solar))
        {
            let moon = &system.moons[e.moon];
            let phase = calendar.moon_phase(e.day, e.moon).unwrap();
            let off_new = phase.min(1.0 - phase);
            assert!(off_new < 1e-6, "phase {phase} at day {}", e.day.0);
            let beta = moon_ecliptic_latitude_deg(&calendar, moon, e.moon, e.day).unwrap();
            let threshold = solar_eclipse_threshold_deg(
                sun_angular_rel_at(&system, &calendar, e.day),
                moon.angular_diameter_rel,
            );
            assert!(beta.abs() < threshold, "β {beta} vs θ {threshold}");
        }
    }

    /// A flat orbit eclipses at every single new moon (SKY-6's shipped
    /// rate limit, now dated).
    #[test]
    fn a_flat_orbit_eclipses_every_new_moon() {
        let (mut system, _) = luna_sol();
        system.moons[0].inclination_deg = 1e-9;
        let calendar = crate::calendar::calendar_of(&system);
        let synodic = calendar.synodic_month(0).unwrap();
        let window = StdInstant(synodic.0 * 24.0);
        let events = eclipse_events(&system, &calendar, StdInstant(0.0), window);
        let solar = events
            .iter()
            .filter(|e| matches!(e.body, EclipseBody::Solar))
            .count();
        assert!((23..=25).contains(&solar), "solar count {solar}");
    }

    /// Eclipse seasons exist: every solar event's sun sits within a
    /// bounded arc of the node line (Luna scale: asin(θ/i) ≈ 17°, so 25°
    /// with slack) — events cluster, they don't smear.
    #[test]
    fn solar_events_cluster_at_the_node_line() {
        let (system, calendar) = luna_sol();
        let moon = &system.moons[0];
        let events = eclipse_events(
            &system,
            &calendar,
            StdInstant(0.0),
            StdInstant(365.25 * 20.0),
        );
        for e in events
            .iter()
            .filter(|e| matches!(e.body, EclipseBody::Solar))
        {
            let l_sun = 360.0 * calendar.year_phase(e.day);
            let omega = node_longitude_at(moon, calendar.year_length(), e.day);
            let arc = (l_sun - omega)
                .rem_euclid(180.0)
                .min(180.0 - (l_sun - omega).rem_euclid(180.0));
            assert!(
                arc < 25.0,
                "sun {arc}° from the node line at day {}",
                e.day.0
            );
        }
    }

    /// A central eclipse (β = 0) tracks the sub-solar latitude; a
    /// threshold-grazing one runs toward a pole.
    #[test]
    fn track_latitude_runs_from_subsolar_to_polar_with_beta() {
        let (system, calendar) = luna_sol();
        let events = eclipse_events(
            &system,
            &calendar,
            StdInstant(0.0),
            StdInstant(365.25 * 30.0),
        );
        let moon = &system.moons[0];
        for e in events
            .iter()
            .filter(|e| matches!(e.body, EclipseBody::Solar))
        {
            let track = ground_track(&system, &calendar, e).unwrap();
            let beta = moon_ecliptic_latitude_deg(&calendar, moon, e.moon, e.day).unwrap();
            let dec = calendar.solar_declination(e.day);
            if beta.abs() < 0.1 {
                assert!((track.center_lat_deg - dec).abs() < 5.0, "central near dec");
            }
            assert!((-90.0..=90.0).contains(&track.center_lat_deg));
        }
    }

    /// Luna check: the shadow crossing lasts hours, not minutes or days.
    #[test]
    fn track_duration_is_hours_luna_scale() {
        let (system, calendar) = luna_sol();
        let events = eclipse_events(
            &system,
            &calendar,
            StdInstant(0.0),
            StdInstant(365.25 * 10.0),
        );
        let solar = events
            .iter()
            .find(|e| matches!(e.body, EclipseBody::Solar))
            .unwrap();
        let track = ground_track(&system, &calendar, solar).unwrap();
        assert!(
            (0.03..0.3).contains(&track.duration_days),
            "duration {} days",
            track.duration_days
        );
    }

    /// Lunar events have no track — every night-side observer sees them.
    #[test]
    fn lunar_events_have_no_track_and_night_visibility() {
        let (system, calendar) = luna_sol();
        let events = eclipse_events(
            &system,
            &calendar,
            StdInstant(0.0),
            StdInstant(365.25 * 10.0),
        );
        let lunar = events
            .iter()
            .find(|e| matches!(e.body, EclipseBody::Lunar))
            .unwrap();
        assert!(ground_track(&system, &calendar, lunar).is_none());
        let ss = sub_solar_longitude_deg(&calendar, lunar.day);
        let night_lon = if ss >= 0.0 { ss - 180.0 } else { ss + 180.0 };
        assert!(lunar_eclipse_seen(&calendar, lunar, night_lon));
        assert!(!lunar_eclipse_seen(&calendar, lunar, ss));
    }

    /// Sight tiers: in-band day-side sees the full omen, off-band day-side
    /// sees a bite, the night side sees nothing.
    #[test]
    fn sight_tiers_partition_the_globe() {
        let (system, calendar) = luna_sol();
        let events = eclipse_events(
            &system,
            &calendar,
            StdInstant(0.0),
            StdInstant(365.25 * 10.0),
        );
        let solar = events
            .iter()
            .find(|e| matches!(e.body, EclipseBody::Solar))
            .unwrap();
        let track = ground_track(&system, &calendar, solar).unwrap();
        let ss = sub_solar_longitude_deg(&calendar, solar.day);
        let in_band = solar_eclipse_sight(&system, &calendar, solar, track.center_lat_deg, ss);
        assert!(matches!(
            in_band,
            EclipseSight::WholeSun | EclipseSight::BurningRing
        ));
        let off_band_lat = if track.center_lat_deg > 0.0 {
            track.center_lat_deg - track.half_width_deg - 20.0
        } else {
            track.center_lat_deg + track.half_width_deg + 20.0
        };
        assert!(matches!(
            solar_eclipse_sight(&system, &calendar, solar, off_band_lat, ss),
            EclipseSight::Bitten
        ));
        let night_lon = if ss >= 0.0 { ss - 180.0 } else { ss + 180.0 };
        assert!(matches!(
            solar_eclipse_sight(&system, &calendar, solar, track.center_lat_deg, night_lon),
            EclipseSight::Unseen
        ));
    }

    /// A one- or two-return value mislabeled as an exeligmos would corrupt
    /// both the recurrence period and the accumulated node-phase drift.
    #[test]
    fn recurrence_summary_makes_the_exeligmos_exactly_three_returns() {
        let (system, calendar) = luna_sol();
        let records = eclipse_recurrences(&system, &calendar);
        assert_eq!(records.len(), 2, "one moon has solar and lunar families");
        for record in records {
            assert_eq!(
                record.series_lifetime,
                StdDays(record.series_returns as f64 * record.cycle.period.0)
            );
            assert_eq!(
                record.exeligmos_period,
                StdDays(3.0 * record.cycle.period.0)
            );
            assert_eq!(
                record.exeligmos_node_slip_deg,
                3.0 * record.cycle.node_slip_deg
            );
            assert!(
                (-21.0..=-20.5).contains(&record.exeligmos_surface_longitude_shift_deg),
                "the Luna fixture's bounded cycle closes about 20.7 degrees west, got {}",
                record.exeligmos_surface_longitude_shift_deg
            );
            assert_ne!(
                record.exeligmos_surface_longitude_shift_deg, record.exeligmos_node_slip_deg,
                "surface-longitude closure and orbital node-phase slip are distinct observables"
            );
        }
    }

    /// The central solar tier belongs only to the swept ground-track arc.
    /// Its endpoints are inclusive, including when the arc crosses the
    /// longitude seam; another day-side longitude receives only a bite.
    #[test]
    fn observer_result_respects_track_edges_and_longitude_wrap() {
        let (system, calendar) = luna_sol();
        let events = eclipse_events(
            &system,
            &calendar,
            StdInstant(0.0),
            StdInstant(365.25 * 100.0),
        );
        let (solar, track) = events
            .iter()
            .filter(|event| event.body == EclipseBody::Solar)
            .find_map(|event| {
                let track = ground_track(&system, &calendar, event)?;
                ((track.start_lon_deg - track.end_lon_deg).abs() > 180.0).then_some((event, track))
            })
            .expect("the Luna fixture crosses the longitude seam within a century");

        for longitude in [track.start_lon_deg, track.end_lon_deg, 180.0, -180.0] {
            let result =
                eclipse_observer_result(&system, &calendar, solar, track.center_lat_deg, longitude)
                    .expect("valid observer");
            assert_eq!(result.side, EclipseSide::Day);
            assert!(matches!(
                result.visibility,
                EclipseVisibility::Solar(EclipseSight::WholeSun | EclipseSight::BurningRing)
            ));
            assert_eq!(result.region, EclipseRegion::GroundTrack(track));
        }

        let subsolar = sub_solar_longitude_deg(&calendar, solar.day);
        let outside_arc = (subsolar + 30.0 + 180.0).rem_euclid(360.0) - 180.0;
        let result =
            eclipse_observer_result(&system, &calendar, solar, track.center_lat_deg, outside_arc)
                .expect("valid observer");
        assert_eq!(result.side, EclipseSide::Day);
        assert_eq!(
            result.visibility,
            EclipseVisibility::Solar(EclipseSight::Bitten)
        );

        let night = eclipse_observer_result(
            &system,
            &calendar,
            solar,
            track.center_lat_deg,
            subsolar + 180.0,
        )
        .expect("valid observer");
        assert_eq!(night.side, EclipseSide::Night);
        assert_eq!(
            night.visibility,
            EclipseVisibility::Solar(EclipseSight::Unseen)
        );
    }

    /// Directed track membership must retain a sweep longer than the
    /// shortest signed arc. A four-hour day can carry the shadow beyond 180°;
    /// a one-hour day carries it through a full turn and covers every
    /// longitude. Track endpoints remain central tiers even when the
    /// midpoint day-side classifier puts an endpoint on the night side.
    #[test]
    fn observer_result_follows_long_directed_sweeps() {
        use crate::anchor::Rotation;
        use hornvale_kernel::units::TickSpan;

        let (mut system, _) = luna_sol();
        system.moons[0].angular_diameter_rel = 2.0;
        let solar_event = |system: &StarSystem, calendar: &Calendar| {
            eclipse_events(system, calendar, StdInstant(0.0), StdInstant(365.25 * 10.0))
                .into_iter()
                .find(|event| event.body == EclipseBody::Solar)
                .expect("the enlarged moon has a solar eclipse")
        };

        system.anchor.rotation = Rotation::Spinning {
            day: TickSpan::from_std_days(1.0 / 6.0).unwrap(),
            retrograde: false,
        };
        let four_hour_calendar = crate::calendar::calendar_of(&system);
        let solar = solar_event(&system, &four_hour_calendar);
        let track = ground_track(&system, &four_hour_calendar, &solar).unwrap();
        assert!(
            track.sweep_deg < -180.0,
            "a prograde four-hour day sweeps west through more than 180 degrees: {}",
            track.sweep_deg
        );
        assert!(!track.global_coverage);
        for longitude in [track.start_lon_deg, track.end_lon_deg] {
            let result = eclipse_observer_result(
                &system,
                &four_hour_calendar,
                &solar,
                track.center_lat_deg,
                longitude,
            )
            .unwrap();
            assert!(matches!(
                result.visibility,
                EclipseVisibility::Solar(EclipseSight::WholeSun | EclipseSight::BurningRing)
            ));
        }

        system.anchor.rotation = Rotation::Spinning {
            day: TickSpan::from_std_days(1.0 / 24.0).unwrap(),
            retrograde: false,
        };
        let one_hour_calendar = crate::calendar::calendar_of(&system);
        let one_hour_track = ground_track(&system, &one_hour_calendar, &solar).unwrap();
        assert!(
            one_hour_track.sweep_deg < -360.0,
            "one-hour sweep: {}",
            one_hour_track.sweep_deg
        );
        assert!(one_hour_track.global_coverage);
        let result = eclipse_observer_result(
            &system,
            &one_hour_calendar,
            &solar,
            one_hour_track.center_lat_deg,
            one_hour_track.start_lon_deg + 137.0,
        )
        .unwrap();
        assert!(matches!(
            result.visibility,
            EclipseVisibility::Solar(EclipseSight::WholeSun | EclipseSight::BurningRing)
        ));
    }

    /// The unified result keeps the lunar night hemisphere distinct from a
    /// solar ground track and reports the boundary as night-side visibility.
    #[test]
    fn observer_result_reports_lunar_night_side_without_a_track() {
        let (system, calendar) = luna_sol();
        let lunar = eclipse_events(
            &system,
            &calendar,
            StdInstant(0.0),
            StdInstant(365.25 * 10.0),
        )
        .into_iter()
        .find(|event| event.body == EclipseBody::Lunar)
        .unwrap();
        let subsolar = sub_solar_longitude_deg(&calendar, lunar.day);

        let day = eclipse_observer_result(&system, &calendar, &lunar, 0.0, subsolar).unwrap();
        assert_eq!(day.side, EclipseSide::Day);
        assert_eq!(day.visibility, EclipseVisibility::Lunar { visible: false });
        assert_eq!(day.region, EclipseRegion::NightHemisphere);

        let terminator =
            eclipse_observer_result(&system, &calendar, &lunar, 90.0, subsolar + 90.0).unwrap();
        assert_eq!(terminator.side, EclipseSide::Night);
        assert_eq!(
            terminator.visibility,
            EclipseVisibility::Lunar { visible: true }
        );
        assert_eq!(terminator.region, EclipseRegion::NightHemisphere);
    }

    /// Latitude poles are valid, values beyond them are not, and longitude
    /// aliases normalize to one observer result.
    #[test]
    fn observer_result_validates_latitude_and_normalizes_longitude() {
        let (system, calendar) = luna_sol();
        let solar = eclipse_events(
            &system,
            &calendar,
            StdInstant(0.0),
            StdInstant(365.25 * 10.0),
        )
        .into_iter()
        .find(|event| event.body == EclipseBody::Solar)
        .unwrap();
        let longitude = sub_solar_longitude_deg(&calendar, solar.day);

        assert!(eclipse_observer_result(&system, &calendar, &solar, 90.0, longitude).is_some());
        assert!(eclipse_observer_result(&system, &calendar, &solar, -90.0, longitude).is_some());
        for latitude in [90.000_001, -90.000_001, f64::NAN, f64::INFINITY] {
            assert!(
                eclipse_observer_result(&system, &calendar, &solar, latitude, longitude).is_none(),
                "latitude {latitude} must be rejected"
            );
        }
        assert!(eclipse_observer_result(&system, &calendar, &solar, 0.0, f64::NAN).is_none());

        let canonical =
            eclipse_observer_result(&system, &calendar, &solar, 0.0, longitude).unwrap();
        let wrapped =
            eclipse_observer_result(&system, &calendar, &solar, 0.0, longitude + 720.0).unwrap();
        assert_eq!(canonical, wrapped);
    }

    /// Reversing the world's spin reverses the ground-track sweep, while
    /// both endpoints remain inside the physical region.
    #[test]
    fn observer_result_follows_retrograde_track_direction() {
        let (mut system, _) = luna_sol();
        let day = match system.anchor.rotation {
            crate::anchor::Rotation::Spinning { day, .. } => day,
            crate::anchor::Rotation::Locked => unreachable!(),
        };
        system.anchor.rotation = crate::anchor::Rotation::Spinning {
            day,
            retrograde: true,
        };
        let calendar = crate::calendar::calendar_of(&system);
        let solar = eclipse_events(
            &system,
            &calendar,
            StdInstant(0.0),
            StdInstant(365.25 * 10.0),
        )
        .into_iter()
        .find(|event| event.body == EclipseBody::Solar)
        .unwrap();
        let track = ground_track(&system, &calendar, &solar).unwrap();
        let sweep = (track.end_lon_deg - track.start_lon_deg + 180.0).rem_euclid(360.0) - 180.0;
        assert!(sweep > 0.0, "retrograde sweep must run eastward: {sweep}");

        for longitude in [track.start_lon_deg, track.end_lon_deg] {
            let result = eclipse_observer_result(
                &system,
                &calendar,
                &solar,
                track.center_lat_deg,
                longitude,
            )
            .unwrap();
            assert!(matches!(
                result.visibility,
                EclipseVisibility::Solar(EclipseSight::WholeSun | EclipseSight::BurningRing)
            ));
        }
    }

    /// A locked world's substellar meridian and shadow do not sweep. The
    /// day side still sees a partial eclipse outside that zero-length track,
    /// while the opposite hemisphere sees the lunar event.
    #[test]
    fn observer_result_keeps_locked_world_regions_static() {
        let (mut system, _) = luna_sol();
        system.anchor.rotation = crate::anchor::Rotation::Locked;
        let calendar = crate::calendar::calendar_of(&system);
        let events = eclipse_events(
            &system,
            &calendar,
            StdInstant(0.0),
            StdInstant(365.25 * 10.0),
        );
        let solar = events
            .iter()
            .find(|event| event.body == EclipseBody::Solar)
            .unwrap();
        let track = ground_track(&system, &calendar, solar).unwrap();
        assert_eq!((track.start_lon_deg, track.end_lon_deg), (0.0, 0.0));

        let on_track =
            eclipse_observer_result(&system, &calendar, solar, track.center_lat_deg, 0.0).unwrap();
        assert!(matches!(
            on_track.visibility,
            EclipseVisibility::Solar(EclipseSight::WholeSun | EclipseSight::BurningRing)
        ));
        let off_track =
            eclipse_observer_result(&system, &calendar, solar, track.center_lat_deg, 45.0).unwrap();
        assert_eq!(off_track.side, EclipseSide::Day);
        assert_eq!(
            off_track.visibility,
            EclipseVisibility::Solar(EclipseSight::Bitten)
        );

        let lunar = events
            .iter()
            .find(|event| event.body == EclipseBody::Lunar)
            .unwrap();
        let night = eclipse_observer_result(&system, &calendar, lunar, -90.0, 180.0).unwrap();
        assert_eq!(night.side, EclipseSide::Night);
        assert_eq!(night.visibility, EclipseVisibility::Lunar { visible: true });
        assert_eq!(night.region, EclipseRegion::NightHemisphere);
    }

    /// Luna check: the draconic month is ~27.21 days.
    #[test]
    fn draconic_month_matches_the_luna_check_value() {
        let d = draconic_month(StdDays(365.25), StdDays(27.32), 5.14);
        assert!((d.0 - 27.21).abs() < 0.05, "draconic {}", d.0);
    }

    /// Luna check: the eclipse year is ~346 days (our approximated node
    /// period gives ~345.9 against the true 346.62).
    #[test]
    fn eclipse_year_matches_the_luna_check_value() {
        let p = node_regression_period(StdDays(365.25), StdDays(27.32), 5.14);
        let ey = eclipse_year(StdDays(365.25), p);
        assert!((340.0..352.0).contains(&ey.0), "eclipse year {}", ey.0);
    }

    /// Fed TRUE Luna periods, the cycle search finds the saros: 223
    /// synodic ≈ 242 draconic ≈ 6585.3 days. (The derived pipeline's own
    /// draconic month differs enough — 17.9 vs 18.61 yr node period —
    /// that a world's best cycle may legitimately be an octon-class one;
    /// this test pins the *search*, the next pins the pipeline.)
    #[test]
    fn the_search_finds_the_true_saros_from_true_inputs() {
        let c = best_cycle(StdDays(29.5306), StdDays(27.2122)).unwrap();
        assert_eq!((c.synodic_count, c.draconic_count), (223, 242));
        assert!((c.period.0 - 6585.3).abs() < 1.0, "period {}", c.period.0);
    }

    /// The derived pipeline always yields *some* long-lived cycle: slip
    /// small enough that the family survives at least ten returns.
    #[test]
    fn luna_sol_pipeline_yields_a_living_cycle() {
        let (system, calendar) = luna_sol();
        let moon = &system.moons[0];
        let synodic = calendar.synodic_month(0).unwrap();
        let d = draconic_month(calendar.year_length(), moon.period, moon.inclination_deg);
        let c = best_cycle(synodic, d).unwrap();
        let sun_angular = crate::star::sun_angular_diameter_rel(&system.star, system.anchor.orbit);
        let theta = solar_eclipse_threshold_deg(sun_angular, moon.angular_diameter_rel);
        let returns = series_returns(&c, theta, moon.inclination_deg);
        assert!(returns >= 10, "returns {returns}");
        // Order-of-magnitude lifetime: centuries to a few millennia.
        let lifetime_years = returns as f64 * c.period.0 / 365.25;
        assert!(
            (50.0..20_000.0).contains(&lifetime_years),
            "lifetime {lifetime_years} yr"
        );
    }

    /// Luna check: the eclipse seasons parade backward through the civil
    /// year at ~19 days/year.
    #[test]
    fn the_parade_matches_the_luna_check_value() {
        let p = node_regression_period(StdDays(365.25), StdDays(27.32), 5.14);
        let ey = eclipse_year(StdDays(365.25), p);
        let parade = parade_days_per_year(StdDays(365.25), ey);
        assert!((15.0..25.0).contains(&parade), "parade {parade}");
    }

    /// Degenerate inputs never yield a cycle: zero, negative, and
    /// non-finite periods are all explicitly None.
    #[test]
    fn best_cycle_rejects_degenerate_inputs() {
        assert!(best_cycle(StdDays(0.0), StdDays(27.2)).is_none());
        assert!(best_cycle(StdDays(29.5), StdDays(-1.0)).is_none());
        assert!(best_cycle(StdDays(f64::NAN), StdDays(27.2)).is_none());
        assert!(best_cycle(StdDays(29.5), StdDays(f64::INFINITY)).is_none());
    }

    /// Coincidence days: same-day events from different moons count once
    /// per day; a single-moon world scores zero.
    #[test]
    fn coincidence_days_needs_two_moons() {
        let (system, calendar) = luna_sol();
        let events = eclipse_events(
            &system,
            &calendar,
            StdInstant(0.0),
            StdInstant(365.25 * 50.0),
        );
        assert_eq!(coincidence_days(&events), 0);
    }
}

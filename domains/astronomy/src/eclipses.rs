//! Dated eclipses (Eclipse Seasons, SKY-6 close-out): node geometry as a
//! function of `WorldTime`. The node longitude is drawn; its regression
//! period, the moon's ecliptic latitude, and every dated event are pure
//! derivations (model card: the lunar-theory leading term).

use crate::calendar::Calendar;
use crate::moons::Moon;
use crate::system::StarSystem;
use crate::units::StdDays;
use hornvale_kernel::math;

/// One angular-diameter unit (Sol from 1 AU ≈ Luna from Earth) in degrees
/// — the shared scale of `sun_angular_diameter_rel` and a moon's
/// `angular_diameter_rel` (declared approximation: the two units differ
/// by under 1%). Moved here from `provider.rs` (Eclipse Seasons).
/// type-audit: pending(wave-1)
pub const ANGULAR_UNIT_DEG: f64 = 0.53;
/// How far (degrees of lunar ecliptic latitude) past the discs' own touch
/// an eclipse still falls somewhere on the world — the parallax allowance.
/// Calibrated so a Luna–Sol pair at 5.14° inclination eclipses at ~19% of
/// new moons (Earth's ~2.4 solar eclipses a year).
/// type-audit: pending(wave-1)
pub const ECLIPSE_PARALLAX_DEG: f64 = 1.0;
/// The anchor's shadow threshold at the moon, as a fraction of the solar
/// threshold (declared approximation, Luna–Sol-calibrated to ~1.5 umbral
/// lunar eclipses/year — the umbra at Luna's distance is ≈ 2.6 lunar
/// radii). Below 1.0 because the solar threshold carries the
/// anywhere-on-the-world parallax allowance the lunar case doesn't need —
/// the shadow is one shadow for every observer.
/// type-audit: pending(wave-1)
pub const LUNAR_SHADOW_FACTOR: f64 = 0.64;

/// The node threshold (degrees of lunar ecliptic latitude) inside which a
/// new moon eclipses the sun somewhere on the world: half the summed
/// discs plus the parallax allowance.
/// type-audit: pending(wave-1)
pub fn solar_eclipse_threshold_deg(sun_angular_rel: f64, moon_angular_rel: f64) -> f64 {
    ANGULAR_UNIT_DEG * (sun_angular_rel + moon_angular_rel) / 2.0 + ECLIPSE_PARALLAX_DEG
}

/// The fraction of syzygies whose ecliptic latitude falls inside
/// `threshold_deg` for an orbit inclined `inclination_deg`: a sinusoidal
/// latitude distribution gives P = (2/π)·asin(threshold / i), saturating
/// at 1 for a flat orbit. (The statistical twin of the dated scan —
/// `rate_matches_the_dated_scan` in provider.rs holds them together.)
/// type-audit: bare-ok(ratio)
pub fn node_crossing_chance(threshold_deg: f64, inclination_deg: f64) -> f64 {
    let x = (threshold_deg / inclination_deg.max(f64::MIN_POSITIVE)).min(1.0);
    (2.0 / std::f64::consts::PI) * math::asin(x)
}

/// Nodal regression period from the lunar-theory leading term (declared
/// approximation, model card): P_node = (4/3)·Y²/(P_sid·cos i). Earth
/// check: ~17.9 yr against the true 18.61.
/// type-audit: pending(wave-1: inclination_deg)
pub fn node_regression_period(year: StdDays, sidereal: StdDays, inclination_deg: f64) -> StdDays {
    StdDays((4.0 / 3.0) * year.0 * year.0 / (sidereal.0 * math::cos(inclination_deg.to_radians())))
}

/// The ascending node's ecliptic longitude at `t`, degrees in [0, 360):
/// the genesis draw regressed westward one turn per regression period.
/// type-audit: pending(wave-1)
pub fn node_longitude_at(moon: &Moon, year: StdDays, t: StdDays) -> f64 {
    let p = node_regression_period(year, moon.period, moon.inclination_deg);
    (moon.node_longitude_deg - 360.0 * t.0 / p.0).rem_euclid(360.0)
}

/// The moon's ecliptic latitude at `t`, degrees (small-angle inclined-orbit
/// form, exact at the syzygies where it is consumed): β = i·sin(L−Ω), with
/// L = L_sun + 360·phase reusing the shipped phase machinery. `None` if
/// the moon has no synodic cycle (degenerate P_sid ≥ Y).
/// type-audit: bare-ok(index: index), pending(wave-1: return)
pub fn moon_ecliptic_latitude_deg(
    calendar: &Calendar,
    moon: &Moon,
    index: usize,
    t: StdDays,
) -> Option<f64> {
    let phase = calendar.moon_phase(t, index)?;
    let l_sun = 360.0 * calendar.year_phase(t);
    let l_moon = l_sun + 360.0 * phase;
    let omega = node_longitude_at(moon, calendar.year_length(), t);
    Some(moon.inclination_deg * math::sin((l_moon - omega).to_radians()))
}

/// The sun's apparent angular diameter (Luna-units) at `t`: the mean
/// orbital value scaled by the instantaneous star distance from the live
/// eccentricity. First-order: r/a = 1 − e·sin(2π·year-phase) — the same
/// perihelion convention the apsidal daylight term already uses
/// (insolation peaks at year phase 0.25). Declared approximation (model
/// card); evaluated at the event, never cached (the tidal-braking seam).
/// type-audit: pending(wave-1)
pub fn sun_angular_rel_at(system: &StarSystem, calendar: &Calendar, t: StdDays) -> f64 {
    let mean = crate::star::sun_angular_diameter_rel(&system.star, system.anchor.orbit);
    let e = system.forcing.eccentricity_at(t.0);
    mean / (1.0 - e * math::sin(std::f64::consts::TAU * calendar.year_phase(t)))
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
    pub day: StdDays,
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
    from: StdDays,
    until: StdDays,
) -> Vec<EclipseEvent> {
    let mut out = Vec::new();
    for (index, moon) in system.moons.iter().enumerate() {
        let Some(synodic) = calendar.synodic_month(index) else {
            continue;
        };
        let Some(phase0) = calendar.moon_phase(StdDays(0.0), index) else {
            continue;
        };
        // Syzygy k of each family sits at t = (k + half − phase0)·synodic.
        for (half, body) in syzygy_families() {
            let k_min = (from.0 / synodic.0 + phase0 - half).ceil() as i64;
            let k_max = (until.0 / synodic.0 + phase0 - half).floor() as i64;
            for k in k_min..=k_max {
                let t = StdDays((k as f64 + half - phase0) * synodic.0);
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
pub const TRACK_HALF_WIDTH_DEG: f64 = 2.0;

/// The moon's ecliptic longitude at `t`, degrees in [0, 360): the sun's
/// longitude plus the synodic phase (Eclipse Seasons — the positional
/// ephemeris half transits and standstills will reuse). `None` if the
/// moon has no synodic cycle.
/// type-audit: bare-ok(index: index), pending(wave-1: return)
pub fn moon_ecliptic_longitude_deg(calendar: &Calendar, index: usize, t: StdDays) -> Option<f64> {
    let phase = calendar.moon_phase(t, index)?;
    Some((360.0 * calendar.year_phase(t) + 360.0 * phase).rem_euclid(360.0))
}

/// The sub-solar longitude at `t`, degrees in [−180, 180): local noon of
/// the position-blind observer (day fraction 0.5) sits at longitude 0,
/// matching the locked convention (substellar point = prime meridian);
/// the sun sweeps westward on a prograde world, eastward on a retrograde
/// one (SKY-22). A locked world's sun is fixed at 0.
/// type-audit: pending(wave-1)
pub fn sub_solar_longitude_deg(calendar: &Calendar, t: StdDays) -> f64 {
    let Some((_, fraction)) = calendar.local_day(t) else {
        return 0.0;
    };
    let direction = if calendar.is_retrograde() { 1.0 } else { -1.0 };
    (direction * (fraction - 0.5) * 360.0 + 180.0).rem_euclid(360.0) - 180.0
}

/// A solar eclipse's shadow geometry: a latitude band swept across
/// longitudes as the world turns under the shadow.
/// type-audit: pending(wave-1: center_lat_deg), pending(wave-1: half_width_deg), pending(wave-1: start_lon_deg), pending(wave-1: end_lon_deg), pending(wave-1: duration_days)
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
        sub_solar_longitude_deg(calendar, StdDays(event.day.0 - duration_days / 2.0));
    let end_lon_deg = sub_solar_longitude_deg(calendar, StdDays(event.day.0 + duration_days / 2.0));
    Some(GroundTrack {
        center_lat_deg,
        half_width_deg: TRACK_HALF_WIDTH_DEG,
        start_lon_deg,
        end_lon_deg,
        duration_days,
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
    let ss = sub_solar_longitude_deg(calendar, event.day);
    let lon_gap = (longitude - ss + 180.0).rem_euclid(360.0) - 180.0;
    if lon_gap.abs() >= 90.0 {
        return EclipseSight::Unseen;
    }
    let Some(track) = ground_track(system, calendar, event) else {
        return EclipseSight::Unseen;
    };
    if (latitude - track.center_lat_deg).abs() <= track.half_width_deg {
        match event.kind {
            EclipseKind::Total => EclipseSight::WholeSun,
            EclipseKind::Annular => EclipseSight::BurningRing,
        }
    } else {
        EclipseSight::Bitten
    }
}

/// Whether an observer at `longitude` has the full moon in their sky for
/// a dated lunar `event`: the night side, the complement of the solar
/// half-day window.
/// type-audit: pending(wave-1: longitude), bare-ok(flag: return)
pub fn lunar_eclipse_seen(calendar: &Calendar, event: &EclipseEvent, longitude: f64) -> bool {
    let ss = sub_solar_longitude_deg(calendar, event.day);
    let lon_gap = (longitude - ss + 180.0).rem_euclid(360.0) - 180.0;
    lon_gap.abs() >= 90.0
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
/// return is i·sin(slip); a series is born grazing one edge of the ±θ
/// window and dies at the other, so it lives ≈ 2θ / Δβ returns. Slipless
/// cycles saturate at 10,000.
/// type-audit: pending(wave-1: threshold_deg), pending(wave-1: inclination_deg), bare-ok(index: return)
pub fn series_returns(cycle: &EclipseCycle, threshold_deg: f64, inclination_deg: f64) -> u32 {
    let d_beta = inclination_deg * math::sin(cycle.node_slip_deg.to_radians()).abs();
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
    let mut system = generate(Seed(42), &pins).unwrap().system;
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

    #[test]
    fn nodes_regress_westward_one_turn_per_period() {
        let moon = test_moon(5.14, 40.0);
        let year = StdDays(365.25);
        let p = node_regression_period(year, moon.period, moon.inclination_deg);
        let start = node_longitude_at(&moon, year, StdDays(0.0));
        assert_eq!(start, 40.0);
        let quarter = node_longitude_at(&moon, year, StdDays(p.0 / 4.0));
        assert!(((start - quarter).rem_euclid(360.0) - 90.0).abs() < 1e-6);
        let full = node_longitude_at(&moon, year, StdDays(p.0));
        assert!((full - start).rem_euclid(360.0) < 1e-6);
    }

    #[test]
    fn ecliptic_latitude_is_bounded_by_the_inclination() {
        let (system, calendar) = super::luna_sol();
        let moon = &system.moons[0];
        for k in 0..500 {
            let t = StdDays(k as f64 * 13.7);
            let b = moon_ecliptic_latitude_deg(&calendar, moon, 0, t).unwrap();
            assert!(b.abs() <= moon.inclination_deg + 1e-9, "β {b} at t {}", t.0);
        }
    }

    /// With zero eccentricity the event-time sun is the mean sun exactly.
    #[test]
    fn a_circular_orbit_keeps_the_mean_sun() {
        let (system, calendar) = super::luna_sol();
        let mean = crate::star::sun_angular_diameter_rel(&system.star, system.anchor.orbit);
        for k in 0..12 {
            let t = StdDays(k as f64 * 30.0);
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
            .map(|d| sun_angular_rel_at(&system, &calendar, StdDays(d as f64)))
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

    fn test_moon(inclination_deg: f64, node_longitude_deg: f64) -> Moon {
        use crate::units::{LunarMasses, Megameters};
        Moon {
            mass: LunarMasses(1.0),
            distance: Megameters(384.4),
            period: StdDays(27.32),
            angular_diameter_rel: 1.0,
            tide_rel: 1.0,
            inclination_deg,
            node_longitude_deg,
            formation: crate::moons::Formation::GiantImpact,
        }
    }

    /// Luna–Sol calibration: the dated scan reproduces Earth's ~2.4 solar
    /// eclipses/year over a 50-year window (the anywhere-on-the-world
    /// count the parallax allowance is calibrated to).
    #[test]
    fn luna_sol_dates_earths_solar_cadence() {
        let (system, calendar) = luna_sol();
        let years = 50.0;
        let events = eclipse_events(&system, &calendar, StdDays(0.0), StdDays(365.25 * years));
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
        let events = eclipse_events(&system, &calendar, StdDays(0.0), StdDays(365.25 * years));
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
        let events = eclipse_events(&system, &calendar, StdDays(0.0), StdDays(365.25 * 20.0));
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
        let events = eclipse_events(&system, &calendar, StdDays(0.0), StdDays(365.25 * 20.0));
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
        let window = StdDays(synodic.0 * 24.0);
        let events = eclipse_events(&system, &calendar, StdDays(0.0), window);
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
        let events = eclipse_events(&system, &calendar, StdDays(0.0), StdDays(365.25 * 20.0));
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
        let events = eclipse_events(&system, &calendar, StdDays(0.0), StdDays(365.25 * 30.0));
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
        let events = eclipse_events(&system, &calendar, StdDays(0.0), StdDays(365.25 * 10.0));
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
        let events = eclipse_events(&system, &calendar, StdDays(0.0), StdDays(365.25 * 10.0));
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
        let events = eclipse_events(&system, &calendar, StdDays(0.0), StdDays(365.25 * 10.0));
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
        let events = eclipse_events(&system, &calendar, StdDays(0.0), StdDays(365.25 * 50.0));
        assert_eq!(coincidence_days(&events), 0);
    }
}

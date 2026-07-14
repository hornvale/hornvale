//! Dated eclipses (Eclipse Seasons, SKY-6 close-out): node geometry as a
//! function of `WorldTime`. The node longitude is drawn; its regression
//! period, the moon's ecliptic latitude, and every dated event are pure
//! derivations (model card: the lunar-theory leading term).

use crate::calendar::Calendar;
use crate::moons::Moon;
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
        }
    }
}

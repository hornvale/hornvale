//! Heliacal risings and settings (spec §7, the Sothic kernel): the day a
//! star first glimmers back into the dawn twilight after a season lost in
//! the sun's glare, and the evening it last shows before the sun swallows
//! it again. The gap between the two is the star's *absence* — the seed of
//! calendars built on a single bright star (a Sothic-cycle instrument).

use crate::calendar::Calendar;
use crate::pins::NeighborClass;
use crate::sky_position::EquatorialCoord;
use crate::system::StarSystem;
use crate::units::StdDays;
use hornvale_kernel::math;

/// Deterministic sample count for the year-long heliacal scan (model card):
/// fixed regardless of day length, so the schedule never drifts with pins.
const SAMPLES: usize = 400;

/// A star's heliacal rising and setting for the year containing the query
/// time `t` — the two edges of its annual absence behind the sun.
/// type-audit: bare-ok(index: neighbor), bare-ok(ratio: rising_frac), bare-ok(ratio: setting_frac)
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct HeliacalPair {
    /// Index into `StarSystem::neighbors`.
    pub neighbor: usize,
    /// Year-phase fraction (`[0,1)` of the year containing `t`) of the
    /// star's heliacal rising: sample index / 400.0 of the scanned year
    /// (that granularity is the documented precision of this instrument).
    pub rising_frac: f64,
    /// Year-phase fraction (`[0,1)` of the year containing `t`) of the
    /// star's heliacal setting, at the same sample-index precision.
    pub setting_frac: f64,
}

impl HeliacalPair {
    /// How much of the year the star spends absent from both dawn and
    /// dusk skies, as a fraction in `[0,1)`: the rising-to-setting gap
    /// wrapped forward, so the star vanishes (setting) then returns
    /// (rising) after this fraction of the year.
    /// type-audit: bare-ok(ratio: return)
    pub fn absence_fraction(&self) -> f64 {
        (self.rising_frac - self.setting_frac).rem_euclid(1.0)
    }
}

/// The sun's minimum depth below the horizon (degrees) for a star of this
/// class to be glimpsed at its own rising or setting (model card): brighter
/// surfaces cut through brighter twilight, so the arcus shrinks with
/// luminosity. All values sit inside the shared `TWILIGHT_DEPTH_DEG` band
/// (`Calendar::sky_band`'s frame, spec §2).
/// type-audit: pending(wave-1)
pub fn arcus_visionis_deg(class: NeighborClass) -> f64 {
    match class {
        NeighborClass::BlueGiant | NeighborClass::RedGiant => 7.0,
        NeighborClass::OrangeGiant | NeighborClass::SunLike => 9.0,
        NeighborClass::WhiteDwarf | NeighborClass::RedDwarf => 11.0,
    }
}

/// The star's rise/set half-arc (radians, `[0, pi]`) at latitude `phi_deg`
/// for declination `dec_deg`: `cos H0 = -tan(phi) * tan(dec)`, clamped to
/// the domain of `acos` (a star beyond the clamp is circumpolar or never
/// rises — the caller filters those out before this is used for a scan).
fn half_arc_radians(phi_deg: f64, dec_deg: f64) -> f64 {
    let phi = phi_deg.to_radians();
    let delta = dec_deg.to_radians();
    let cos_h0 = (-math::tan(phi) * math::tan(delta)).clamp(-1.0, 1.0);
    math::acos(cos_h0)
}

/// The absolute moment (start of the local day containing `t_sample`, plus
/// `fraction` of a local day) — the same day-start recovery `night_sky.rs`
/// uses to place a wrapped local-day fraction back on the absolute
/// timeline.
fn at_local_fraction(
    calendar: &Calendar,
    t_sample: StdDays,
    day_length: f64,
    fraction: f64,
) -> StdDays {
    let day_start =
        t_sample.0 - calendar.local_day(t_sample).map(|(_, f)| f).unwrap_or(0.0) * day_length;
    StdDays(day_start + fraction * day_length)
}

/// All heliacal risings and settings for the year containing `t`, at
/// `latitude` (model card, spec §7). A locked world has no local day, so no
/// star ever rises or sets: empty. A circumpolar or never-rising star
/// (night_sky.rs's sign convention) never crosses the horizon either: also
/// skipped. Everything else is scanned at `SAMPLES` evenly spaced points
/// across the year, walking in sample order: the heliacal rising is the
/// first false-to-true edge of the morning-visibility predicate (the sun
/// at least `arcus_visionis_deg(class)` below the horizon at the star's
/// own rising moment); the heliacal setting is the last true-to-false edge
/// of the evening-visibility predicate at the star's setting moment. A
/// star visible (or invisible) at every sample all year has neither event.
/// type-audit: pending(wave-1: latitude)
pub fn heliacal_events(
    system: &StarSystem,
    calendar: &Calendar,
    latitude: f64,
    t: StdDays,
) -> Vec<HeliacalPair> {
    let Some(day_length) = calendar.day_length().map(|d| d.get()) else {
        return Vec::new();
    };
    let year = calendar.year_length().get();
    let year_start = t.0 - calendar.year_phase(t) * year;

    let mut out = Vec::new();
    for (i, n) in system.neighbors.iter().enumerate() {
        let genesis = EquatorialCoord {
            ra_deg: n.right_ascension,
            dec_deg: n.declination,
        };

        // Circumpolar / never-rises at this latitude (night_sky.rs's
        // sign convention): either way the star never crosses the
        // horizon, so it has no heliacal events. (Same side + extreme =
        // circumpolar; opposite side + extreme = never rises — both are
        // "extreme", so the side doesn't matter for this skip.)
        let ref_pos = calendar.star_equatorial_at(&genesis, t);
        let extreme = ref_pos.dec_deg.abs() > 90.0 - latitude.abs();
        if extreme {
            continue;
        }

        let arcus = arcus_visionis_deg(n.class);
        let mut morning = Vec::with_capacity(SAMPLES);
        let mut evening = Vec::with_capacity(SAMPLES);

        for k in 0..SAMPLES {
            let t_k = StdDays(year_start + (k as f64 / SAMPLES as f64) * year);
            let star_pos = calendar.star_equatorial_at(&genesis, t_k);
            let sun_pos = calendar.solar_equatorial(t_k);
            let transit_fraction =
                (0.5 + (star_pos.ra_deg - sun_pos.ra_deg) / 360.0).rem_euclid(1.0);
            let h0_frac = half_arc_radians(latitude, star_pos.dec_deg) / std::f64::consts::TAU;

            let rise_t = at_local_fraction(calendar, t_k, day_length, transit_fraction - h0_frac);
            let set_t = at_local_fraction(calendar, t_k, day_length, transit_fraction + h0_frac);

            let sun_alt_at_rise = calendar.solar_altitude_at(rise_t, latitude).unwrap_or(0.0);
            let sun_alt_at_set = calendar.solar_altitude_at(set_t, latitude).unwrap_or(0.0);

            morning.push(sun_alt_at_rise <= -arcus);
            evening.push(sun_alt_at_set <= -arcus);
        }

        // Rising: first FALSE -> TRUE edge of morning visibility, walking
        // the year in sample order (no wraparound between the last and
        // first sample — the scan is of one framed year, not a cycle).
        let rising_edge = (1..SAMPLES).find(|&k| !morning[k - 1] && morning[k]);
        // Setting: the LAST TRUE -> FALSE edge of evening visibility — the
        // star's last visible evening before conjunction.
        let setting_edge = (1..SAMPLES).rev().find(|&k| evening[k - 1] && !evening[k]);

        if let (Some(rk), Some(sk)) = (rising_edge, setting_edge) {
            out.push(HeliacalPair {
                neighbor: i,
                rising_frac: rk as f64 / SAMPLES as f64,
                setting_frac: sk as f64 / SAMPLES as f64,
            });
        }
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::calendar::calendar_of;
    use crate::pins::{ForcingPin, RotationPin, SkyPins};
    use crate::system::generate;
    use crate::units::Degrees;
    use hornvale_kernel::Seed;

    /// One bright star + the sun: the instrument's load-bearing kernel.
    fn minimal_sky() -> (StarSystem, Calendar) {
        let pins = SkyPins {
            rotation: Some(RotationPin::PeriodHours(24.0)),
            obliquity: Some(Degrees::new(23.5).unwrap()),
            forcing: Some(ForcingPin::Zero),
            ..SkyPins::default()
        };
        let mut system = generate(Seed(42), &pins).unwrap().system;
        system.neighbors.truncate(1);
        system.neighbors[0].declination = -10.0; // rises and sets at mid-northern latitudes
        system.neighbors[0].right_ascension = 45.0;
        let calendar = calendar_of(&system);
        (system, calendar)
    }

    fn locked_system() -> StarSystem {
        let pins = SkyPins {
            rotation: Some(RotationPin::Locked),
            ..SkyPins::default()
        };
        generate(Seed(42), &pins).unwrap().system
    }

    #[test]
    fn the_minimal_sky_yields_one_heliacal_pair_with_an_absence() {
        let (system, calendar) = minimal_sky();
        let pairs = heliacal_events(&system, &calendar, 35.0, StdDays(0.0));
        assert_eq!(pairs.len(), 1);
        let p = &pairs[0];
        assert!((0.0..1.0).contains(&p.rising_frac) && (0.0..1.0).contains(&p.setting_frac));
        let absence = p.absence_fraction();
        assert!(
            absence > 0.0 && absence < 0.5,
            "the star vanishes, then returns: {absence}"
        );
    }

    #[test]
    fn brighter_classes_surface_in_brighter_twilight() {
        assert!(
            arcus_visionis_deg(NeighborClass::BlueGiant)
                < arcus_visionis_deg(NeighborClass::RedDwarf)
        );
        for class in [NeighborClass::BlueGiant, NeighborClass::RedDwarf] {
            assert!(arcus_visionis_deg(class) < crate::calendar::TWILIGHT_DEPTH_DEG);
        }
    }

    #[test]
    fn locked_worlds_have_no_heliacal_events() {
        let system = locked_system();
        let calendar = calendar_of(&system);
        assert!(heliacal_events(&system, &calendar, 35.0, StdDays(0.0)).is_empty());
    }

    #[test]
    fn circumpolar_stars_have_no_heliacal_events() {
        let (mut system, _) = minimal_sky();
        system.neighbors[0].declination = 88.0;
        let calendar = calendar_of(&system);
        assert!(heliacal_events(&system, &calendar, 60.0, StdDays(0.0)).is_empty());
    }
}

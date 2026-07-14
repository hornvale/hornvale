//! The night sky: which stars are visible to a placed observer at any epoch.
//! Every query is a derived view (spec §2) — committed nowhere, recomputed
//! on demand.

use crate::calendar::{Calendar, SkyBand};
use crate::sky_position::EquatorialCoord;
use crate::system::StarSystem;
use crate::units::StdDays;

/// Which celestial pole a pole star is closest to.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Hemisphere {
    /// North celestial pole.
    North,
    /// South celestial pole.
    South,
}

/// A star serving as the pole star at the current epoch.
/// type-audit: bare-ok(index: neighbor), pending(wave-1: separation_deg)
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct PoleStar {
    /// Index into `StarSystem::neighbors`.
    pub neighbor: usize,
    /// Which pole it marks.
    pub pole: Hemisphere,
    /// How far from the pole, in degrees.
    pub separation_deg: f64,
}

/// The night sky visible to an observer at a location and moment.
/// type-audit: bare-ok(index: visible), bare-ok(index: circumpolar), bare-ok(index: never_rises), bare-ok(flag: wheels_backward), bare-ok(flag: frozen)
#[derive(Debug, Clone, PartialEq)]
pub struct NightSky {
    /// Indices of stars visible tonight (circumpolar + seasonal).
    pub visible: Vec<usize>,
    /// Indices of stars that never set at this latitude.
    pub circumpolar: Vec<usize>,
    /// Indices of stars that never rise at this latitude.
    pub never_rises: Vec<usize>,
    /// The brightest star within the pole-search radius, if any.
    pub pole_star: Option<PoleStar>,
    /// Whether the world spins backward (sun rises in the west).
    pub wheels_backward: bool,
    /// Whether the sky is static (locked world, no day/night cycle).
    pub frozen: bool,
}

/// How many degrees from a celestial pole a star must be to serve as a pole star.
/// type-audit: pending(wave-1)
pub const POLE_STAR_MAX_SEPARATION_DEG: f64 = 10.0;

/// The unified derived view (spec §2): everything a placed observer's night
/// holds, in one query. Committed nowhere — recomputed on demand.
/// type-audit: pending(wave-1: latitude)
pub fn night_sky_at(
    system: &StarSystem,
    calendar: &Calendar,
    latitude: f64,
    t: StdDays,
) -> NightSky {
    let frozen = calendar.day_length().is_none();
    let sun = calendar.solar_equatorial(t);
    let mut circumpolar = Vec::new();
    let mut never_rises = Vec::new();
    let mut visible = Vec::new();
    let mut pole_star: Option<PoleStar> = None;
    for (i, n) in system.neighbors.iter().enumerate() {
        let genesis = EquatorialCoord {
            ra_deg: n.right_ascension,
            dec_deg: n.declination,
        };
        let pos = calendar.star_equatorial_at(&genesis, t);
        let same_side = pos.dec_deg.signum() == latitude.signum() && latitude != 0.0;
        if same_side && pos.dec_deg.abs() > 90.0 - latitude.abs() {
            circumpolar.push(i);
        } else if !same_side && pos.dec_deg.abs() > 90.0 - latitude.abs() {
            never_rises.push(i);
            continue;
        }
        let north_sep = 90.0 - pos.dec_deg;
        let south_sep = 90.0 + pos.dec_deg;
        if pole_star.is_none() {
            if north_sep <= POLE_STAR_MAX_SEPARATION_DEG {
                pole_star = Some(PoleStar {
                    neighbor: i,
                    pole: Hemisphere::North,
                    separation_deg: north_sep,
                });
            } else if south_sep <= POLE_STAR_MAX_SEPARATION_DEG {
                pole_star = Some(PoleStar {
                    neighbor: i,
                    pole: Hemisphere::South,
                    separation_deg: south_sep,
                });
            }
        }
        if circumpolar.contains(&i) {
            visible.push(i);
            continue;
        }
        if frozen {
            visible.push(i);
            continue;
        }
        // Visible tonight = transits in the dark (declared approximation).
        let day_start = t.0
            - calendar.local_day(t).map(|(_, f)| f).unwrap_or(0.0)
                * calendar.day_length().map(|d| d.0).unwrap_or(1.0);
        let transit_fraction = (0.5 + (pos.ra_deg - sun.ra_deg) / 360.0).rem_euclid(1.0);
        let transit_t = StdDays(
            day_start + transit_fraction * calendar.day_length().map(|d| d.0).unwrap_or(1.0),
        );
        if calendar.sky_band(transit_t, latitude) != Some(SkyBand::Day) {
            visible.push(i);
        }
    }
    NightSky {
        visible,
        circumpolar,
        never_rises,
        pole_star,
        wheels_backward: calendar.is_retrograde(),
        frozen,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::calendar::calendar_of;
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
    fn the_equatorial_observer_sees_every_star_across_a_year() {
        let system = spinning_system();
        let cal = calendar_of(&system);
        let year = cal.year_length().get();
        let mut seen: std::collections::BTreeSet<usize> = std::collections::BTreeSet::new();
        for k in 0..48 {
            let sky = night_sky_at(&system, &cal, 0.0, StdDays(k as f64 * year / 48.0));
            assert!(sky.circumpolar.is_empty() && sky.never_rises.is_empty());
            seen.extend(sky.visible.iter().copied());
        }
        assert_eq!(
            seen.len(),
            system.neighbors.len(),
            "a year at the equator shows the whole sky"
        );
    }

    #[test]
    fn the_polar_observer_sees_one_unchanging_hemisphere() {
        let system = spinning_system();
        let cal = calendar_of(&system);
        let sky = night_sky_at(&system, &cal, 90.0, StdDays(0.0));
        assert_eq!(
            sky.circumpolar.len() + sky.never_rises.len(),
            system.neighbors.len(),
            "at the pole every star is circumpolar or invisible"
        );
    }

    #[test]
    fn a_locked_world_has_a_frozen_sky() {
        let system = locked_system();
        let cal = calendar_of(&system);
        let a = night_sky_at(&system, &cal, 30.0, StdDays(0.0));
        let b = night_sky_at(&system, &cal, 30.0, StdDays(5000.0));
        assert!(a.frozen);
        assert_eq!(
            a.visible, b.visible,
            "the locked sky is a map, not a calendar"
        );
    }

    #[test]
    fn seasonal_visibility_actually_varies() {
        let system = spinning_system();
        let cal = calendar_of(&system);
        let year = cal.year_length().get();
        let skies: Vec<Vec<usize>> = (0..12)
            .map(|k| night_sky_at(&system, &cal, 30.0, StdDays(k as f64 * year / 12.0)).visible)
            .collect();
        assert!(
            skies.iter().any(|s| *s != skies[0]),
            "winter stars must differ from summer stars"
        );
    }
}

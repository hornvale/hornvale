//! Deterministic star-chart renders (spec: the star chart, 2026-07-09):
//! a 72×24 equirectangular ASCII chart — RA 0→360° across, dec 90→−90°
//! down, each star plotted as its brightness-rank digit — and timeless
//! moon phase-cycle strips. Same sky, same bytes.

use crate::calendar::Calendar;
use crate::moons::Moon;
use crate::neighborhood::Neighbor;
use crate::provider::size_word;

/// ASCII chart width in characters.
pub const ASCII_WIDTH: usize = 72;
/// ASCII chart height in characters.
pub const ASCII_HEIGHT: usize = 24;

/// One synodic cycle as a 16-column glyph strip: `o` new, `)` waxing,
/// `O` full, `(` waning — the provider's phase-word thresholds sampled
/// at k/16.
const PHASE_STRIP: &str = "oo))))))OO((((oo";

/// Render the fixed night sky as a 72×24 equirectangular ASCII chart.
/// Stars plot as their 1-based brightness-rank digit; on a collision the
/// brighter star's digit wins. The celestial equator is a dashed line.
pub fn chart_ascii(neighbors: &[Neighbor]) -> String {
    let mut grid = vec![vec![' '; ASCII_WIDTH]; ASCII_HEIGHT];
    let equator = ASCII_HEIGHT / 2;
    for (col, cell) in grid[equator].iter_mut().enumerate() {
        if col % 2 == 0 {
            *cell = '-';
        }
    }
    // Dimmest first, so a brighter star's digit overwrites on collision.
    for (index, n) in neighbors.iter().enumerate().rev() {
        let col = ((n.right_ascension / 360.0) * ASCII_WIDTH as f64) as usize;
        let row = (((90.0 - n.declination) / 180.0) * ASCII_HEIGHT as f64) as usize;
        let digit = char::from_digit(index as u32 + 1, 10).expect("≤ 5 neighbors");
        grid[row.min(ASCII_HEIGHT - 1)][col.min(ASCII_WIDTH - 1)] = digit;
    }
    let mut out = String::with_capacity((ASCII_WIDTH + 1) * ASCII_HEIGHT);
    for row in grid {
        out.extend(row);
        out.push('\n');
    }
    out
}

/// One timeless phase-cycle line per moon: the strip, the moon's size
/// word, and its synodic month. Skips (with an honest note) a moon whose
/// synodic cycle is degenerate.
pub fn moon_lines(moons: &[Moon], calendar: &Calendar) -> Vec<String> {
    moons
        .iter()
        .enumerate()
        .map(|(index, moon)| match calendar.synodic_month(index) {
            Some(synodic) => format!(
                "Moon {} ({}): `{}` — one cycle every {:.1} standard days.",
                index + 1,
                size_word(moon.angular_diameter_rel),
                PHASE_STRIP,
                synodic.0
            ),
            None => format!(
                "Moon {} ({}): no phase cycle — its orbit outpaces the year.",
                index + 1,
                size_word(moon.angular_diameter_rel)
            ),
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::anchor::{Anchor, Rotation};
    use crate::calendar::calendar_of;
    use crate::moons::Moon;
    use crate::pins::NeighborClass;
    use crate::star::Star;
    use crate::system::StarSystem;
    use crate::units::{
        Au, Degrees, EarthMasses, HabitableZone, LunarMasses, Megameters, SolarLuminosities,
        SolarMasses, StdDays,
    };

    fn star(dec: f64, ra: f64, brightness: f64) -> Neighbor {
        Neighbor {
            class: NeighborClass::SunLike,
            distance: crate::units::LightYears(10.0),
            apparent_brightness: brightness,
            color: "warm yellow".to_string(),
            declination: dec,
            right_ascension: ra,
        }
    }

    #[test]
    fn chart_is_72x24_with_every_star_digit_present() {
        let stars = vec![
            star(45.0, 30.0, 3.0),
            star(-20.0, 200.0, 2.0),
            star(0.0, 359.0, 1.0),
        ];
        let chart = chart_ascii(&stars);
        assert_eq!(chart.lines().count(), ASCII_HEIGHT);
        for line in chart.lines() {
            assert_eq!(line.chars().count(), ASCII_WIDTH);
        }
        for digit in ["1", "2", "3"] {
            assert!(chart.contains(digit), "missing {digit}");
        }
        assert_eq!(chart, chart_ascii(&stars));
    }

    #[test]
    fn collision_keeps_the_brighter_digit() {
        // Same cell: identical dec/RA. Index 0 is the brighter by contract.
        let stars = vec![star(10.0, 100.0, 5.0), star(10.0, 100.0, 1.0)];
        let chart = chart_ascii(&stars);
        assert!(chart.contains('1'));
        assert!(!chart.contains('2'));
    }

    #[test]
    fn equator_is_dashed() {
        let chart = chart_ascii(&[]);
        let row = chart.lines().nth(ASCII_HEIGHT / 2).unwrap();
        assert!(row.starts_with("- - "));
    }

    /// Hand-build a minimal system with one moon at the given sidereal
    /// period, in a year of the given length (both in standard days) — the
    /// same exact, hand-checkable construction `calendar.rs`'s own tests
    /// use (`calendar_with_moon`), routed through the public `calendar_of`
    /// since that helper's direct field access is private to `calendar.rs`.
    fn system_with_moon(sidereal_days: f64, year_days: f64) -> StarSystem {
        StarSystem {
            star: Star {
                mass: SolarMasses::new(1.0).unwrap(),
                luminosity: SolarLuminosities::new(1.0).unwrap(),
                class_name: "yellow dwarf".to_string(),
                habitable_zone: HabitableZone::new(Au::new(0.9).unwrap(), Au::new(1.4).unwrap())
                    .unwrap(),
            },
            anchor: Anchor {
                mass: EarthMasses::new(1.0).unwrap(),
                orbit: Au::new(1.0).unwrap(),
                year: StdDays::new(year_days).unwrap(),
                rotation: Rotation::Spinning {
                    day: StdDays::new(1.0).unwrap(),
                },
                obliquity: Degrees::new(0.0).unwrap(),
            },
            moons: vec![Moon {
                mass: LunarMasses::new(1.0).unwrap(),
                distance: Megameters::new(384.4).unwrap(),
                period: StdDays::new(sidereal_days).unwrap(),
                angular_diameter_rel: 1.0,
                tide_rel: 1.0,
            }],
            neighbors: vec![],
        }
    }

    #[test]
    fn luna_like_moon_line_shows_the_29_5_day_cycle() {
        let system = system_with_moon(27.32, 365.25);
        let calendar = calendar_of(&system);
        let lines = moon_lines(&system.moons, &calendar);
        assert_eq!(lines.len(), 1);
        assert!(lines[0].contains("29.5"), "got {}", lines[0]);
        assert!(lines[0].contains(PHASE_STRIP));
        assert!(lines[0].contains("full-sized"));
    }

    #[test]
    fn degenerate_synodic_cycle_gets_honest_wording() {
        let system = system_with_moon(400.0, 365.25);
        let calendar = calendar_of(&system);
        let lines = moon_lines(&system.moons, &calendar);
        assert_eq!(lines.len(), 1);
        assert!(lines[0].contains("no phase cycle — its orbit outpaces the year."));
    }
}

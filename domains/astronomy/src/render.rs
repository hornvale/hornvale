//! Deterministic star-chart renders (spec: the star chart, 2026-07-09):
//! a 72×24 equirectangular ASCII chart — RA 0→360° across, dec 90→−90°
//! down, each star plotted as its brightness-rank digit — and timeless
//! moon phase-cycle strips. Same sky, same bytes.

use crate::calendar::Calendar;
use crate::moons::Moon;
use crate::neighborhood::Neighbor;
use crate::pins::NeighborClass;
use crate::provider::size_word;
use hornvale_kernel::math;

/// ASCII chart width in characters.
/// type-audit: bare-ok(render-internal)
pub const ASCII_WIDTH: usize = 72;
/// ASCII chart height in characters.
/// type-audit: bare-ok(render-internal)
pub const ASCII_HEIGHT: usize = 24;

/// One synodic cycle as a 16-column glyph strip: `o` new, `)` waxing,
/// `O` full, `(` waning — the provider's phase-word bands
/// ([`crate::provider::phase_eighth`], centered by SKY-14) sampled at k/16.
const PHASE_STRIP: &str = "o))))))OO((((((o";

/// Render the fixed night sky as a 72×24 equirectangular ASCII chart.
/// Stars plot as their 1-based brightness-rank digit; on a collision the
/// brighter star's digit wins. The celestial equator is a dashed line.
/// type-audit: bare-ok(artifact)
pub fn chart_ascii(neighbors: &[Neighbor]) -> String {
    let mut grid = vec![vec![' '; ASCII_WIDTH]; ASCII_HEIGHT];
    let equator = ASCII_HEIGHT / 2;
    for (col, cell) in grid[equator].iter_mut().enumerate() {
        if col % 2 == 0 {
            *cell = '-';
        }
    }
    // Dimmest first, so a brighter star's digit overwrites on collision.
    for (index, neighbor) in neighbors.iter().enumerate().rev() {
        let col = ((neighbor.right_ascension / 360.0) * ASCII_WIDTH as f64) as usize;
        let row = (((90.0 - neighbor.declination) / 180.0) * ASCII_HEIGHT as f64) as usize;
        let digit =
            char::from_digit(index as u32 + 1, 10).expect("digit glyphs run out past 9 neighbors");
        grid[row.min(ASCII_HEIGHT - 1)][col.min(ASCII_WIDTH - 1)] = digit;
    }
    let mut out = String::with_capacity((ASCII_WIDTH + 1) * ASCII_HEIGHT);
    for row in grid {
        out.extend(row);
        out.push('\n');
    }
    out
}

/// The fixed night sky as a 72×24 ANSI chart: each star its brightness-rank
/// digit, tinted by spectral class; the celestial equator dashed. The colored
/// sibling of [`chart_ascii`]; same layout, same determinism.
/// type-audit: bare-ok(artifact)
pub fn chart_ansi(neighbors: &[Neighbor]) -> String {
    let mut grid: Vec<Vec<(char, &'static str)>> = vec![vec![(' ', ""); ASCII_WIDTH]; ASCII_HEIGHT];
    let equator = ASCII_HEIGHT / 2;
    for (col, cell) in grid[equator].iter_mut().enumerate() {
        if col % 2 == 0 {
            *cell = ('-', "");
        }
    }
    for (index, neighbor) in neighbors.iter().enumerate().rev() {
        let col = ((neighbor.right_ascension / 360.0) * ASCII_WIDTH as f64) as usize;
        let row = (((90.0 - neighbor.declination) / 180.0) * ASCII_HEIGHT as f64) as usize;
        let digit =
            char::from_digit(index as u32 + 1, 10).expect("digit glyphs run out past 9 neighbors");
        grid[row.min(ASCII_HEIGHT - 1)][col.min(ASCII_WIDTH - 1)] =
            (digit, spectral_color(neighbor.class));
    }
    emit_ansi_grid(&grid)
}

/// Emit a `(glyph, sgr)` grid as rows, resetting after every colored cell.
fn emit_ansi_grid(grid: &[Vec<(char, &'static str)>]) -> String {
    let mut out = String::new();
    for row in grid {
        for &(ch, color) in row {
            if color.is_empty() {
                out.push(ch);
            } else {
                out.push_str(color);
                out.push(ch);
                out.push_str("\u{1b}[0m");
            }
        }
        out.push('\n');
    }
    out
}

/// One timeless phase-cycle line per moon: the strip, the moon's size
/// word, and its synodic month. Skips (with an honest note) a moon whose
/// synodic cycle is degenerate.
/// type-audit: bare-ok(artifact)
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

/// Raster chart width in pixels.
/// type-audit: bare-ok(render-internal)
pub const MAP_WIDTH: u32 = 256;
/// Raster chart height in pixels.
/// type-audit: bare-ok(render-internal)
pub const MAP_HEIGHT: u32 = 128;

/// Near-black sky field (spec §4).
const FIELD: [u8; 3] = [10, 14, 28];
/// Ring and rim gray-blue.
const RING: [u8; 3] = [60, 72, 100];
/// Index-digit label color.
const LABEL: [u8; 3] = [180, 195, 220];
/// Disc radius in pixels.
const DISC_RADIUS: f64 = 56.0;
/// North-hemisphere disc center (x, y).
const NORTH_CENTER: (f64, f64) = (64.0, 64.0);
/// South-hemisphere disc center (x, y).
const SOUTH_CENTER: (f64, f64) = (192.0, 64.0);

/// 3×5 pixel digit bitmaps for 1–5, one `u8` of 3 bits per row.
const DIGITS: [[u8; 5]; 5] = [
    [0b010, 0b110, 0b010, 0b010, 0b111],
    [0b110, 0b001, 0b010, 0b100, 0b111],
    [0b111, 0b001, 0b011, 0b001, 0b111],
    [0b101, 0b101, 0b111, 0b001, 0b001],
    [0b111, 0b100, 0b111, 0b001, 0b111],
];

/// Star dot color by spectral class.
fn class_rgb(class: NeighborClass) -> [u8; 3] {
    match class {
        NeighborClass::RedDwarf => [200, 70, 50],
        NeighborClass::SunLike => [255, 214, 120],
        NeighborClass::WhiteDwarf => [235, 235, 245],
        NeighborClass::OrangeGiant => [255, 150, 60],
        NeighborClass::RedGiant => [255, 90, 70],
        NeighborClass::BlueGiant => [160, 200, 255],
    }
}

/// Write one pixel, ignoring out-of-bounds coordinates.
fn set_px(pixels: &mut [u8], x: i64, y: i64, color: [u8; 3]) {
    if x < 0 || y < 0 || x >= i64::from(MAP_WIDTH) || y >= i64::from(MAP_HEIGHT) {
        return;
    }
    let i = (y as usize * MAP_WIDTH as usize + x as usize) * 3;
    pixels[i..i + 3].copy_from_slice(&color);
}

/// Solid rim (dec 0) and dashed rings (dec 30, 60) for one disc.
fn draw_disc(pixels: &mut [u8], center: (f64, f64)) {
    for y in 0..i64::from(MAP_HEIGHT) {
        for x in 0..i64::from(MAP_WIDTH) {
            let (dx, dy) = (x as f64 + 0.5 - center.0, y as f64 + 0.5 - center.1);
            let d = (dx * dx + dy * dy).sqrt();
            if (d - DISC_RADIUS).abs() < 0.6 {
                set_px(pixels, x, y, RING);
                continue;
            }
            for ring in [DISC_RADIUS / 3.0, DISC_RADIUS * 2.0 / 3.0] {
                if (d - ring).abs() < 0.5 {
                    let dash = math::atan2(dy, dx) / std::f64::consts::TAU * 24.0;
                    if dash.rem_euclid(2.0) < 1.0 {
                        set_px(pixels, x, y, RING);
                    }
                }
            }
        }
    }
}

/// Pixel position of a star on its hemisphere's disc: azimuthal
/// equidistant, r = (90 − |δ|)/90 × R; north disc counterclockwise,
/// south clockwise, so RA reads consistently across both.
fn star_xy(n: &Neighbor) -> (f64, f64) {
    let north = n.declination >= 0.0;
    let center = if north { NORTH_CENTER } else { SOUTH_CENTER };
    let r = (90.0 - n.declination.abs()) / 90.0 * DISC_RADIUS;
    let theta = n.right_ascension.to_radians();
    let sy = if north {
        -math::sin(theta)
    } else {
        math::sin(theta)
    };
    (center.0 + r * math::cos(theta), center.1 + r * sy)
}

/// Filled dot of the given radius.
fn draw_dot(pixels: &mut [u8], cx: f64, cy: f64, radius: i64, color: [u8; 3]) {
    let (px, py) = (cx as i64, cy as i64);
    for y in -radius..=radius {
        for x in -radius..=radius {
            if x * x + y * y <= radius * radius {
                set_px(pixels, px + x, py + y, color);
            }
        }
    }
}

/// Stamp digit `n` (1–5) with its top-left at (x, y).
fn draw_digit(pixels: &mut [u8], n: usize, x: i64, y: i64) {
    let glyph = DIGITS[n - 1];
    for (row, bits) in glyph.iter().enumerate() {
        for col in 0..3 {
            if bits >> (2 - col) & 1 == 1 {
                set_px(pixels, x + col, y + row as i64, LABEL);
            }
        }
    }
}

/// Render the fixed night sky as a 256×128 planisphere-pair PNG: north
/// celestial hemisphere left, south right, dots sized by brightness rank
/// and colored by class, each tagged with its rank digit (spec §4). Assumes
/// at most five neighbors — the genesis draw's cap — or digit indexing exhausts
/// the font.
/// type-audit: bare-ok(artifact)
pub fn chart_png(neighbors: &[Neighbor]) -> Vec<u8> {
    let mut pixels = Vec::with_capacity((MAP_WIDTH * MAP_HEIGHT * 3) as usize);
    for _ in 0..MAP_WIDTH * MAP_HEIGHT {
        pixels.extend_from_slice(&FIELD);
    }
    draw_disc(&mut pixels, NORTH_CENTER);
    draw_disc(&mut pixels, SOUTH_CENTER);
    // Dimmest first, so the brighter dot and label win any overlap.
    for (index, neighbor) in neighbors.iter().enumerate().rev() {
        let (x, y) = star_xy(neighbor);
        let radius = match index {
            0 => 3,
            1 | 2 => 2,
            _ => 1,
        };
        draw_dot(&mut pixels, x, y, radius, class_rgb(neighbor.class));
        draw_digit(&mut pixels, index + 1, x as i64 + radius + 2, y as i64 - 2);
    }
    hornvale_kernel::png::encode_rgb(MAP_WIDTH, MAP_HEIGHT, &pixels)
}

/// A star's terminal color from its spectral class — a **render** decision
/// (paint), not domain data: hot O/B blue-white through cool M red. 256-color
/// SGR escape; pair with a `\x1b[0m` reset. Total over `NeighborClass`.
/// type-audit: bare-ok(artifact)
pub fn spectral_color(class: NeighborClass) -> &'static str {
    match class {
        NeighborClass::BlueGiant => "\u{1b}[38;5;39m", // blue-white
        NeighborClass::WhiteDwarf => "\u{1b}[38;5;255m", // white
        NeighborClass::SunLike => "\u{1b}[38;5;220m",  // yellow
        NeighborClass::OrangeGiant => "\u{1b}[38;5;208m", // orange
        NeighborClass::RedGiant => "\u{1b}[38;5;196m", // red
        NeighborClass::RedDwarf => "\u{1b}[38;5;124m", // dim red
    }
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
        Au, Degrees, EarthMasses, Gyr, HabitableZone, LunarMasses, Megameters, SolarLuminosities,
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

    /// The strip is the provider's phase bands sampled at k/16 — recentered
    /// with them by SKY-14, so `O` (full) straddles column 8 (phase 0.5).
    #[test]
    fn phase_strip_glyphs_follow_the_centered_phase_bands() {
        for (k, glyph) in PHASE_STRIP.chars().enumerate() {
            let expected = match crate::provider::phase_eighth(k as f64 / 16.0) {
                0 => 'o',
                1..=3 => ')',
                4 => 'O',
                _ => '(',
            };
            assert_eq!(glyph, expected, "column {k}");
        }
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
                age: Gyr::new(4.5).unwrap(),
            },
            anchor: Anchor {
                mass: EarthMasses::new(1.0).unwrap(),
                orbit: Au::new(1.0).unwrap(),
                year: StdDays::new(year_days).unwrap(),
                rotation: Rotation::Spinning {
                    day: StdDays::new(1.0).unwrap(),
                    retrograde: false,
                },
                obliquity: Degrees::new(0.0).unwrap(),
            },
            moons: vec![Moon {
                mass: LunarMasses::new(1.0).unwrap(),
                distance: Megameters::new(384.4).unwrap(),
                period: StdDays::new(sidereal_days).unwrap(),
                angular_diameter_rel: 1.0,
                tide_rel: 1.0,
                inclination_deg: 5.14,
                node_longitude_deg: 0.0,
                formation: crate::moons::Formation::GiantImpact,
            }],
            neighbors: vec![],
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
            wanderers: vec![],
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

    #[test]
    fn png_is_well_formed_and_byte_deterministic() {
        let stars = vec![star(45.0, 30.0, 3.0), star(-20.0, 200.0, 2.0)];
        let a = chart_png(&stars);
        assert_eq!(a, chart_png(&stars));
        assert!(a.starts_with(&[0x89, b'P', b'N', b'G', 0x0D, 0x0A, 0x1A, 0x0A]));
        assert_eq!(&a[16..20], &MAP_WIDTH.to_be_bytes());
        assert_eq!(&a[20..24], &MAP_HEIGHT.to_be_bytes());
    }

    #[test]
    fn spectral_color_is_total_and_distinct_by_temperature() {
        use crate::pins::NeighborClass;
        // Every variant maps to a non-empty SGR escape.
        for class in [
            NeighborClass::BlueGiant,
            NeighborClass::WhiteDwarf,
            NeighborClass::SunLike,
            NeighborClass::OrangeGiant,
            NeighborClass::RedGiant,
            NeighborClass::RedDwarf,
        ] {
            let c = spectral_color(class);
            assert!(
                c.starts_with("\u{1b}[38;5;") && c.ends_with('m'),
                "{class:?}: {c:?}"
            );
        }
        // Hot and cool ends differ.
        assert_ne!(
            spectral_color(NeighborClass::BlueGiant),
            spectral_color(NeighborClass::RedDwarf)
        );
    }

    #[test]
    fn chart_ansi_is_deterministic_and_tinted() {
        let stars = vec![
            star(45.0, 30.0, 3.0),
            star(-20.0, 200.0, 2.0),
            star(0.0, 359.0, 1.0),
        ];
        let a = chart_ansi(&stars);
        assert_eq!(a, chart_ansi(&stars), "must be deterministic");
        assert!(a.contains("\u{1b}[38;5;"), "stars must be colored");
        assert!(a.contains("\u{1b}[0m"), "colors must reset");
        // Same 72×24 shape as the ASCII chart: 24 newline-terminated rows.
        assert_eq!(a.matches('\n').count(), ASCII_HEIGHT);
    }
}

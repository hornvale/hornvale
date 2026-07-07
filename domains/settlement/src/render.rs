//! Deterministic settlement-mark renders: a 72×24 ASCII overlay and a P6 PPM
//! overlay onto a caller-supplied base image (the biome map, at the
//! composition root). Kernel-only: this module knows nothing of terrain or
//! climate, only bare `(latitude, longitude)` pairs and image bytes, so it
//! stays reachable from `domains/settlement` without violating the
//! kernel-only dependency rule.
//!
//! Same projection as `hornvale_climate::render`: equirectangular, longitude
//! −180→180 across, latitude 90→−90 down.

/// ASCII overlay width in characters.
pub const ASCII_WIDTH: usize = 72;
/// ASCII overlay height in characters.
pub const ASCII_HEIGHT: usize = 24;
/// PPM overlay width in pixels; matches `hornvale_climate::render::MAP_WIDTH`.
pub const PPM_WIDTH: usize = 256;
/// PPM overlay height in pixels; matches `MAP_WIDTH / 2`.
pub const PPM_HEIGHT: usize = 128;

/// Equirectangular pixel coordinates for a (latitude, longitude) pair,
/// clamped into `[0, width)` x `[0, height)`.
fn pixel_for(latitude: f64, longitude: f64, width: usize, height: usize) -> (usize, usize) {
    let px = (((longitude + 180.0) / 360.0) * width as f64) as usize;
    let py = (((90.0 - latitude) / 180.0) * height as f64) as usize;
    (px.min(width - 1), py.min(height - 1))
}

/// Render a fixed 72×24 lon/lat grid of spaces, stamping `o` at each site and
/// `@` at the flagship (stamped last, so it wins any overlap). One newline
/// per row.
pub fn settlement_ascii(sites: &[(f64, f64)], flagship: Option<(f64, f64)>) -> String {
    let mut grid = vec![vec![' '; ASCII_WIDTH]; ASCII_HEIGHT];
    for &(lat, lon) in sites {
        let (px, py) = pixel_for(lat, lon, ASCII_WIDTH, ASCII_HEIGHT);
        grid[py][px] = 'o';
    }
    if let Some((lat, lon)) = flagship {
        let (px, py) = pixel_for(lat, lon, ASCII_WIDTH, ASCII_HEIGHT);
        grid[py][px] = '@';
    }
    let mut out = String::with_capacity((ASCII_WIDTH + 1) * ASCII_HEIGHT);
    for row in grid {
        let line: String = row.into_iter().collect();
        out.push_str(&line);
        out.push('\n');
    }
    out
}

/// The byte offset just past the third `\n` in a P6 PPM header
/// (`P6\n{w} {h}\n255\n`).
fn header_len(base: &[u8]) -> usize {
    let mut count = 0;
    for (i, &b) in base.iter().enumerate() {
        if b == b'\n' {
            count += 1;
            if count == 3 {
                return i + 1;
            }
        }
    }
    base.len()
}

/// Copy `base` (a 256×128 P6 PPM, e.g. `hornvale_climate::render::biome_ppm`)
/// and stamp settlement marks onto it: red (`[255, 0, 0]`) for each site,
/// yellow (`[255, 255, 0]`) for the flagship — stamped last, so it wins any
/// overlap.
pub fn overlay_ppm(base: &[u8], sites: &[(f64, f64)], flagship: Option<(f64, f64)>) -> Vec<u8> {
    let mut out = base.to_vec();
    let offset = header_len(base);
    let mut stamp = |lat: f64, lon: f64, color: [u8; 3]| {
        let (px, py) = pixel_for(lat, lon, PPM_WIDTH, PPM_HEIGHT);
        let start = offset + (py * PPM_WIDTH + px) * 3;
        if start + 3 <= out.len() {
            out[start..start + 3].copy_from_slice(&color);
        }
    };
    for &(lat, lon) in sites {
        stamp(lat, lon, [255, 0, 0]);
    }
    if let Some((lat, lon)) = flagship {
        stamp(lat, lon, [255, 255, 0]);
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    fn base_ppm() -> Vec<u8> {
        let mut out = "P6\n256 128\n255\n".to_string().into_bytes();
        out.extend(vec![0u8; PPM_WIDTH * PPM_HEIGHT * 3]);
        out
    }

    #[test]
    fn ascii_has_right_shape_and_marks() {
        let sites = [(10.0, 20.0), (-30.0, -50.0)];
        let map = settlement_ascii(&sites, Some((10.0, 20.0)));
        assert_eq!(map.lines().count(), ASCII_HEIGHT);
        for line in map.lines() {
            assert_eq!(line.chars().count(), ASCII_WIDTH);
        }
        assert!(map.contains('@'));
        assert!(map.contains('o'));
    }

    #[test]
    fn ascii_is_deterministic() {
        let sites = [(10.0, 20.0), (-30.0, -50.0)];
        let a = settlement_ascii(&sites, Some((10.0, 20.0)));
        let b = settlement_ascii(&sites, Some((10.0, 20.0)));
        assert_eq!(a, b);
    }

    #[test]
    fn ascii_without_flagship_has_no_at_sign() {
        let sites = [(10.0, 20.0)];
        let map = settlement_ascii(&sites, None);
        assert!(!map.contains('@'));
        assert!(map.contains('o'));
    }

    #[test]
    fn overlay_preserves_length_and_changes_bytes() {
        let base = base_ppm();
        let sites = [(10.0, 20.0), (-30.0, -50.0)];
        let out = overlay_ppm(&base, &sites, Some((10.0, 20.0)));
        assert_eq!(out.len(), base.len());
        let diff = out.iter().zip(base.iter()).filter(|(a, b)| a != b).count();
        assert!(diff >= 3, "expected at least one stamped pixel to differ");
    }

    #[test]
    fn flagship_is_stamped_last_and_wins_overlap() {
        let base = base_ppm();
        let out = overlay_ppm(&base, &[(10.0, 20.0)], Some((10.0, 20.0)));
        let offset = header_len(&base);
        let (px, py) = pixel_for(10.0, 20.0, PPM_WIDTH, PPM_HEIGHT);
        let start = offset + (py * PPM_WIDTH + px) * 3;
        assert_eq!(&out[start..start + 3], [255, 255, 0]);
    }
}

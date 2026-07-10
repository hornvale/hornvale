//! Deterministic settlement-mark renders: a 72×24 ASCII overlay and a PNG
//! overlay onto a caller-supplied raw-RGB base image (decision 0018); the
//! module knows nothing of image *containers*, only bare pixels. Kernel-only:
//! this module also knows nothing of terrain or climate, only bare
//! `(latitude, longitude)` pairs and image bytes, so it stays reachable from
//! `domains/settlement` without violating the kernel-only dependency rule.
//!
//! Same projection as `hornvale_climate::render`: equirectangular, longitude
//! −180→180 across, latitude 90→−90 down.

/// ASCII overlay width in characters.
/// type-audit: bare-ok(render-internal)
pub const ASCII_WIDTH: usize = 72;
/// ASCII overlay height in characters.
/// type-audit: bare-ok(render-internal)
pub const ASCII_HEIGHT: usize = 24;
/// Raster overlay width in pixels; matches `hornvale_climate::render::MAP_WIDTH`.
/// type-audit: bare-ok(render-internal)
pub const MAP_WIDTH: usize = 256;
/// Raster overlay height in pixels; `MAP_WIDTH / 2`.
/// type-audit: bare-ok(render-internal)
pub const MAP_HEIGHT: usize = 128;

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
/// type-audit: pending(wave-3: sites), pending(wave-3: flagship), bare-ok(artifact: return)
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

/// Copy `base` (raw 256×128 RGB pixels, row-major, top row first) and stamp
/// settlement marks: red (`[255, 0, 0]`) for each site, yellow
/// (`[255, 255, 0]`) for the flagship — stamped last, so it wins overlap.
fn overlay_pixels(base: &[u8], sites: &[(f64, f64)], flagship: Option<(f64, f64)>) -> Vec<u8> {
    let mut out = base.to_vec();
    let mut stamp = |lat: f64, lon: f64, color: [u8; 3]| {
        let (px, py) = pixel_for(lat, lon, MAP_WIDTH, MAP_HEIGHT);
        let start = (py * MAP_WIDTH + px) * 3;
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

/// Stamp settlement marks onto `base` (raw 256×128 RGB, e.g.
/// `hornvale_climate::render::biome_pixels`) and encode as a PNG
/// (decision 0018).
/// type-audit: bare-ok(artifact: base), pending(wave-3: sites), pending(wave-3: flagship), bare-ok(artifact: return)
pub fn overlay_png(base: &[u8], sites: &[(f64, f64)], flagship: Option<(f64, f64)>) -> Vec<u8> {
    hornvale_kernel::png::encode_rgb(
        MAP_WIDTH as u32,
        MAP_HEIGHT as u32,
        &overlay_pixels(base, sites, flagship),
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    fn base_pixels() -> Vec<u8> {
        vec![0_u8; MAP_WIDTH * MAP_HEIGHT * 3]
    }

    /// The raw-buffer offset of the pixel for (lat, lon).
    fn pixel_offset(lat: f64, lon: f64) -> usize {
        let (px, py) = pixel_for(lat, lon, MAP_WIDTH, MAP_HEIGHT);
        (py * MAP_WIDTH + px) * 3
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
    fn overlay_stamps_sites_red_and_flagship_yellow() {
        let out = overlay_pixels(&base_pixels(), &[(30.0, -60.0)], Some((10.0, 20.0)));
        let site = pixel_offset(30.0, -60.0);
        assert_eq!(&out[site..site + 3], &[255, 0, 0]);
        let flag = pixel_offset(10.0, 20.0);
        assert_eq!(&out[flag..flag + 3], &[255, 255, 0]);
    }

    #[test]
    fn flagship_wins_overlap() {
        let out = overlay_pixels(&base_pixels(), &[(10.0, 20.0)], Some((10.0, 20.0)));
        let at = pixel_offset(10.0, 20.0);
        assert_eq!(&out[at..at + 3], &[255, 255, 0]);
    }

    #[test]
    fn overlay_png_is_well_formed_and_deterministic() {
        let a = overlay_png(&base_pixels(), &[(30.0, -60.0)], Some((10.0, 20.0)));
        assert_eq!(
            a,
            overlay_png(&base_pixels(), &[(30.0, -60.0)], Some((10.0, 20.0)))
        );
        assert!(a.starts_with(&[0x89, b'P', b'N', b'G', 0x0D, 0x0A, 0x1A, 0x0A]));
        assert_eq!(&a[16..20], &(MAP_WIDTH as u32).to_be_bytes());
        assert_eq!(&a[20..24], &(MAP_HEIGHT as u32).to_be_bytes());
    }
}

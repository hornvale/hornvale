//! Atmospheric circulation: the number of banded overturning cells per
//! hemisphere is a heuristic function of rotation period (thermal-Rossby-
//! like), and each band carries a prevailing zonal wind. Prograde by
//! convention (spec §5 model card): equatorial easterly trades, mid-latitude
//! westerlies, polar easterlies. Tidal-lock is a distinct regime with no
//! bands (organized around the substellar point instead).

use hornvale_kernel::{CellId, Geosphere};

/// The rotation regime a climate is built under (climate-owned mirror of the
/// astronomy rotation; mapped at the composition root so climate imports no
/// domain).
/// type-audit: pending(wave-2)
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum RotationRegime {
    /// Ordinary spin with a solar day of this many standard days.
    Spinning {
        /// Day length in standard days.
        day_std: f64,
    },
    /// Tidally locked: organized around the substellar point, no bands.
    Locked,
}

/// The number of circulation cells per hemisphere: `None` when tidally
/// locked; otherwise a step function of the solar day in hours (fast spin →
/// more, narrower bands). Earth's 24 h day yields exactly 3 (the
/// calibration point, spec §10).
/// type-audit: bare-ok(count)
pub fn band_count_for(regime: &RotationRegime) -> Option<u32> {
    match regime {
        RotationRegime::Locked => None,
        RotationRegime::Spinning { day_std } => {
            let hours = day_std * 24.0;
            Some(if hours >= 40.0 {
                1
            } else if hours >= 20.0 {
                3
            } else if hours >= 10.0 {
                5
            } else {
                7
            })
        }
    }
}

/// Which band a latitude falls in, `0` at the equator, increasing poleward.
/// type-audit: bare-ok(count: bands), pending(wave-2: latitude_deg), bare-ok(index: return)
pub fn band_index(latitude_deg: f64, bands: u32) -> u32 {
    let width = 90.0 / f64::from(bands);
    ((latitude_deg.abs() / width) as u32).min(bands - 1)
}

/// Rising (wet) bands are the even-indexed ones (equatorial, and every other
/// belt poleward); odd bands are sinking (dry) horse-latitude/polar belts.
/// type-audit: bare-ok(index: band), bare-ok(flag: return)
pub fn is_rising_band(band: u32) -> bool {
    band.is_multiple_of(2)
}

/// Cross product a × b.
fn cross(a: [f64; 3], b: [f64; 3]) -> [f64; 3] {
    [
        a[1] * b[2] - a[2] * b[1],
        a[2] * b[0] - a[0] * b[2],
        a[0] * b[1] - a[1] * b[0],
    ]
}

/// The unit eastward tangent at a cell: `normalize(ẑ × position)`. Zero at
/// the poles, where east is undefined (their bands carry no rain-shadow
/// tracing). Coordinate convention: latitude = asin(z), longitude = atan2(y, x).
/// type-audit: bare-ok(ratio)
pub fn wind_east_tangent(geo: &Geosphere, cell: CellId) -> [f64; 3] {
    let east = cross([0.0, 0.0, 1.0], geo.position(cell));
    let len = (east[0] * east[0] + east[1] * east[1] + east[2] * east[2]).sqrt();
    if len < 1e-9 {
        [0.0, 0.0, 0.0]
    } else {
        [east[0] / len, east[1] / len, east[2] / len]
    }
}

/// The prevailing wind direction at a cell: the eastward tangent signed by
/// band. Even (rising) bands blow easterly (`-east`, e.g. equatorial trades);
/// odd (sinking) bands blow westerly (`+east`, e.g. mid-latitude westerlies).
/// Zero at the poles.
/// type-audit: bare-ok(count: bands), bare-ok(ratio: return)
pub fn prevailing_wind(geo: &Geosphere, cell: CellId, bands: u32) -> [f64; 3] {
    let east = wind_east_tangent(geo, cell);
    let band = band_index(geo.coord(cell).latitude, bands);
    let sign = if is_rising_band(band) { -1.0 } else { 1.0 };
    [east[0] * sign, east[1] * sign, east[2] * sign]
}

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_kernel::Geosphere;

    #[test]
    fn earth_rotation_yields_three_bands_and_the_step_function_is_monotonic() {
        assert_eq!(
            band_count_for(&RotationRegime::Spinning { day_std: 1.0 }),
            Some(3)
        );
        assert_eq!(
            band_count_for(&RotationRegime::Spinning {
                day_std: 48.0 / 24.0
            }),
            Some(1)
        );
        assert_eq!(
            band_count_for(&RotationRegime::Spinning {
                day_std: 15.0 / 24.0
            }),
            Some(5)
        );
        assert_eq!(
            band_count_for(&RotationRegime::Spinning {
                day_std: 6.0 / 24.0
            }),
            Some(7)
        );
        assert_eq!(band_count_for(&RotationRegime::Locked), None);
    }

    #[test]
    fn band_index_buckets_by_absolute_latitude() {
        assert_eq!(band_index(0.0, 3), 0);
        assert_eq!(band_index(45.0, 3), 1);
        assert_eq!(band_index(-75.0, 3), 2);
        assert_eq!(band_index(90.0, 3), 2); // clamped
        assert!(is_rising_band(0));
        assert!(!is_rising_band(1));
    }

    #[test]
    fn prevailing_wind_is_zonal_tangent_and_reverses_by_band() {
        let geo = Geosphere::new(3);
        // Some equatorial cell has a nonzero eastward tangent orthogonal to +z.
        let cell = geo
            .cells()
            .min_by(|a, b| {
                geo.position(*a)[2]
                    .abs()
                    .total_cmp(&geo.position(*b)[2].abs())
            })
            .unwrap();
        let east = wind_east_tangent(&geo, cell);
        assert!((east[0] * east[0] + east[1] * east[1] + east[2] * east[2]).sqrt() > 0.5);
        // tangent is orthogonal to the local vertical (position)
        let p = geo.position(cell);
        assert!((east[0] * p[0] + east[1] * p[1] + east[2] * p[2]).abs() < 1e-9);
        // easterly at the equator (even band) points opposite the eastward tangent
        let wind = prevailing_wind(&geo, cell, 3);
        assert!(wind[0] * east[0] + wind[1] * east[1] + wind[2] * east[2] < 0.0);
    }
}

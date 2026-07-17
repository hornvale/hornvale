//! Temperature over the globe (canonical °C; the kernel's `Temperature`
//! newtype is the typed boundary — see `hornvale_kernel::Temperature`).
//! Spinning worlds: an insolation baseline that falls with latitude and with
//! elevation (lapse rate), plus a hemisphere-signed seasonal swing set by
//! obliquity and damped near oceans. Tidally locked worlds: temperature is
//! organized around the substellar point (`+x`), hottest there and coldest
//! at the antistellar point. Declared approximations (spec §5 model card):
//! no ocean currents, smooth-sinusoid seasons, prograde-only.

use crate::circulation::RotationRegime;
use hornvale_kernel::math;
use hornvale_kernel::{CellId, CellMap, Geosphere, ReferenceElevation, TempAnomaly, Temperature};

/// Dry-adiabatic-ish lapse rate: °C lost per meter of elevation above sea level.
const LAPSE_C_PER_M: f64 = 6.5 / 1000.0;

/// Continentality: `1.0` fully inland, dropping toward `0.2` as a cell gains
/// ocean neighbors. Damps the seasonal swing (the sea is a thermal buffer).
/// type-audit: bare-ok(ratio: return)
pub fn continentality(
    geo: &Geosphere,
    elevation: &CellMap<ReferenceElevation>,
    sea_level: ReferenceElevation,
    cell: CellId,
) -> f64 {
    let neighbors = geo.neighbors(cell);
    if neighbors.is_empty() {
        return 1.0;
    }
    let ocean = neighbors
        .iter()
        .filter(|n| *elevation.get(**n) < sea_level)
        .count();
    let land_fraction = 1.0 - ocean as f64 / neighbors.len() as f64;
    0.2 + 0.8 * land_fraction
}

/// Annual-mean temperature per cell, °C. Spinning: an insolation baseline
/// (equator warm, poles cold) minus lapse-rate cooling above sea level.
/// Locked: a substellar cosine, hottest at `+x` and floored on the night side.
/// type-audit: pending(wave-2: insolation)
pub fn mean_temperature(
    geo: &Geosphere,
    elevation: &CellMap<ReferenceElevation>,
    sea_level: ReferenceElevation,
    insolation: f64,
    regime: &RotationRegime,
) -> CellMap<Temperature> {
    // Equilibrium temperature scales as S^(1/4).
    let scale = math::powf(insolation.max(0.0), 0.25);
    CellMap::from_fn(geo, |cell| {
        let above = (*elevation.get(cell) - sea_level).max(0.0);
        let lapse = LAPSE_C_PER_M * above;
        let c = match regime {
            RotationRegime::Spinning { .. } => {
                let lat = geo.coord(cell).latitude.to_radians();
                // Blackbody baseline (288 K × S^(1/4)) plus a latitude term of +30 °C at the
                // equator to -30 °C at the pole; endpoints land near +45 °C / -15 °C, area-mean ~15 °C.
                let base_k = 288.0 * scale;
                let lat_term = 30.0 - 60.0 * math::sin(lat) * math::sin(lat);
                (base_k - 273.15) + lat_term - lapse
            }
            RotationRegime::Locked => {
                let p = geo.position(cell);
                let cos_theta = crate::substellar_cosine(p);
                if cos_theta > 0.0 {
                    // Day side: hot at substellar, ~0 °C toward the terminator.
                    (-18.0 + 78.0 * math::powf(cos_theta, 0.3) * scale) - lapse
                } else {
                    // Night side: a deep frozen floor.
                    -60.0 - lapse
                }
            }
        };
        Temperature::new(c).expect("temperature is finite")
    })
}

/// The seasonal half-swing in °C at a cell: proportional to obliquity and to
/// continentality (coastal cells swing less). Zero when obliquity is zero.
/// type-audit: pending(wave-2)
pub fn seasonal_amplitude(
    geo: &Geosphere,
    elevation: &CellMap<ReferenceElevation>,
    sea_level: ReferenceElevation,
    obliquity_deg: f64,
    cell: CellId,
) -> f64 {
    let cont = continentality(geo, elevation, sea_level, cell);
    (obliquity_deg / 90.0) * 25.0 * cont
}

/// Temperature at a cell on a given day: the annual mean plus a
/// hemisphere-signed seasonal sinusoid on the year phase. Locked worlds have
/// no seasonal term (no year phase organizes their fixed day/night).
/// type-audit: pending(wave-2)
#[allow(clippy::too_many_arguments)]
pub fn temperature_at(
    mean: &CellMap<Temperature>,
    geo: &Geosphere,
    elevation: &CellMap<ReferenceElevation>,
    sea_level: ReferenceElevation,
    obliquity_deg: f64,
    year_length_std: f64,
    regime: &RotationRegime,
    cell: CellId,
    day: f64,
) -> Temperature {
    let base = *mean.get(cell);
    match regime {
        RotationRegime::Locked => base,
        RotationRegime::Spinning { .. } => {
            if year_length_std <= 0.0 || obliquity_deg == 0.0 {
                return base;
            }
            let amp = seasonal_amplitude(geo, elevation, sea_level, obliquity_deg, cell);
            let phase = (day / year_length_std).rem_euclid(1.0);
            let hemi = geo.coord(cell).latitude.signum();
            base + TempAnomaly::from_offset_c(amp * hemi * math::sin(std::f64::consts::TAU * phase))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_kernel::Geosphere;

    fn flat_ocean_then_land(
        geo: &Geosphere,
        sea: ReferenceElevation,
    ) -> CellMap<ReferenceElevation> {
        // Half the globe below sea level (x<0), half above — a crude land mask.
        CellMap::from_fn(geo, |c| {
            let m = if geo.position(c)[0] < 0.0 {
                sea.get() - 1000.0
            } else {
                sea.get() + 200.0
            };
            ReferenceElevation::new(m).unwrap()
        })
    }

    #[test]
    fn temperature_falls_with_latitude_on_a_spinning_world() {
        let geo = Geosphere::new(4);
        let elev = CellMap::from_fn(&geo, |_| ReferenceElevation::new(0.0).unwrap());
        let mean = mean_temperature(
            &geo,
            &elev,
            ReferenceElevation::new(0.0).unwrap(),
            1.0,
            &RotationRegime::Spinning { day_std: 1.0 },
        );
        let equator = geo
            .cells()
            .min_by(|a, b| {
                geo.coord(*a)
                    .latitude
                    .abs()
                    .total_cmp(&geo.coord(*b).latitude.abs())
            })
            .unwrap();
        let pole = geo
            .cells()
            .max_by(|a, b| {
                geo.coord(*a)
                    .latitude
                    .abs()
                    .total_cmp(&geo.coord(*b).latitude.abs())
            })
            .unwrap();
        assert!(
            mean.get(equator) > mean.get(pole),
            "equator must be warmer than pole"
        );
    }

    #[test]
    fn temperature_falls_with_altitude() {
        let geo = Geosphere::new(3);
        let low = CellMap::from_fn(&geo, |_| ReferenceElevation::new(0.0).unwrap());
        let high = CellMap::from_fn(&geo, |_| ReferenceElevation::new(3000.0).unwrap());
        let regime = RotationRegime::Spinning { day_std: 1.0 };
        let sea = ReferenceElevation::new(0.0).unwrap();
        let mlow = mean_temperature(&geo, &low, sea, 1.0, &regime);
        let mhigh = mean_temperature(&geo, &high, sea, 1.0, &regime);
        for c in geo.cells() {
            assert!(
                mhigh.get(c) < mlow.get(c),
                "altitude must cool cell {}",
                c.0
            );
        }
    }

    #[test]
    fn locked_world_is_hottest_at_substellar_and_coldest_at_antistellar() {
        let geo = Geosphere::new(4);
        let elev = CellMap::from_fn(&geo, |_| ReferenceElevation::new(0.0).unwrap());
        let mean = mean_temperature(
            &geo,
            &elev,
            ReferenceElevation::new(0.0).unwrap(),
            1.0,
            &RotationRegime::Locked,
        );
        let sub = geo
            .cells()
            .max_by(|a, b| geo.position(*a)[0].total_cmp(&geo.position(*b)[0]))
            .unwrap();
        let anti = geo
            .cells()
            .min_by(|a, b| geo.position(*a)[0].total_cmp(&geo.position(*b)[0]))
            .unwrap();
        assert!(
            *mean.get(sub) > *mean.get(anti) + TempAnomaly::from_offset_c(50.0),
            "substellar must tower over antistellar"
        );
    }

    #[test]
    fn coastal_seasonal_swing_is_smaller_than_continental() {
        let geo = Geosphere::new(4);
        let sea = ReferenceElevation::new(0.0).unwrap();
        let elev = flat_ocean_then_land(&geo, sea);
        // A land cell touching ocean vs a land cell deep inland at similar latitude.
        let coastal = geo
            .cells()
            .find(|c| {
                elev.get(*c) >= &sea
                    && geo.neighbors(*c).iter().any(|n| elev.get(*n) < &sea)
                    && geo.coord(*c).latitude.abs() > 20.0
                    && geo.coord(*c).latitude.abs() < 60.0
            })
            .unwrap();
        let inland = geo
            .cells()
            .find(|c| {
                elev.get(*c) >= &sea
                    && geo.neighbors(*c).iter().all(|n| elev.get(*n) >= &sea)
                    && geo.coord(*c).latitude.abs() > 20.0
                    && geo.coord(*c).latitude.abs() < 60.0
            })
            .unwrap();
        let ac = seasonal_amplitude(&geo, &elev, sea, 23.5, coastal);
        let ai = seasonal_amplitude(&geo, &elev, sea, 23.5, inland);
        assert!(ac < ai, "coastal swing {ac} not smaller than inland {ai}");
    }
}

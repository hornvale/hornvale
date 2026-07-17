//! Moisture over the globe (dimensionless `[0, 1]`, bare f64). Spinning
//! worlds: a base set by the circulation band (rising belts wet, sinking
//! belts dry), raised near oceans, then dried in the lee of mountains by a
//! single upwind trace along the prevailing wind. Locked worlds: wettest near
//! the terminator, dry at the substellar and antistellar points. Declared
//! approximations (spec §5): single-pass rain shadow, no clouds/feedback.

use crate::circulation::{
    RotationRegime, band_count_for, band_index, is_rising_band, prevailing_wind,
};
use hornvale_kernel::{CellId, CellMap, Geosphere, ReferenceElevation};

/// Cells traced upwind when looking for a rain-shadow barrier.
const RAIN_SHADOW_STEPS: usize = 6;
/// Barrier height (m, above the leeward cell) that fully dries it.
const RAIN_SHADOW_SCALE_M: f64 = 3000.0;

/// Ocean-proximity bonus: `+0.3` if the cell itself is ocean-adjacent (or is
/// ocean), tapering to `0.0` fully inland.
fn ocean_bonus(
    geo: &Geosphere,
    elevation: &CellMap<ReferenceElevation>,
    sea_level: ReferenceElevation,
    cell: CellId,
) -> f64 {
    if *elevation.get(cell) < sea_level {
        return 0.3;
    }
    let neighbors = geo.neighbors(cell);
    if neighbors.is_empty() {
        return 0.0;
    }
    let ocean = neighbors
        .iter()
        .filter(|n| *elevation.get(**n) < sea_level)
        .count();
    0.3 * (ocean as f64 / neighbors.len() as f64)
}

/// The rain-shadow drying at a land cell: trace up to `RAIN_SHADOW_STEPS`
/// cells upwind (each step hops to the neighbor most opposite the wind),
/// tracking the highest elevation crossed. A barrier above the cell dries it
/// in proportion to its height over `RAIN_SHADOW_SCALE_M`.
fn rain_shadow(
    geo: &Geosphere,
    elevation: &CellMap<ReferenceElevation>,
    cell: CellId,
    wind: [f64; 3],
) -> f64 {
    if wind == [0.0, 0.0, 0.0] {
        return 0.0;
    }
    let here = *elevation.get(cell);
    let mut current = cell;
    let mut barrier = here;
    for _ in 0..RAIN_SHADOW_STEPS {
        // Upwind neighbor = the one whose displacement is most opposite the wind.
        let cp = geo.position(current);
        let next = geo.neighbors(current).iter().copied().max_by(|a, b| {
            let da = geo.position(*a);
            let db = geo.position(*b);
            let sa = -((da[0] - cp[0]) * wind[0]
                + (da[1] - cp[1]) * wind[1]
                + (da[2] - cp[2]) * wind[2]);
            let sb = -((db[0] - cp[0]) * wind[0]
                + (db[1] - cp[1]) * wind[1]
                + (db[2] - cp[2]) * wind[2]);
            sa.total_cmp(&sb)
        });
        let Some(next) = next else { break };
        if next == current {
            break;
        }
        current = next;
        barrier = barrier.max(*elevation.get(current));
    }
    ((barrier - here) / RAIN_SHADOW_SCALE_M).clamp(0.0, 1.0)
}

/// Moisture per cell, `[0, 1]`. See the module doc for the model.
/// type-audit: bare-ok(ratio: return)
pub fn moisture_field(
    geo: &Geosphere,
    elevation: &CellMap<ReferenceElevation>,
    sea_level: ReferenceElevation,
    regime: &RotationRegime,
) -> CellMap<f64> {
    match band_count_for(regime) {
        None => {
            // Locked: wettest at the terminator (|cos θ| ≈ 0), dry at the poles
            // of the day/night axis.
            CellMap::from_fn(geo, |cell| {
                let p = geo.position(cell);
                let cos_theta = crate::substellar_cosine(p);
                let base = 0.7 * (1.0 - cos_theta.abs());
                (base + ocean_bonus(geo, elevation, sea_level, cell)).clamp(0.0, 1.0)
            })
        }
        Some(bands) => CellMap::from_fn(geo, |cell| {
            let band = band_index(geo.coord(cell).latitude, bands);
            let base = if is_rising_band(band) { 0.6 } else { 0.25 };
            let wind = prevailing_wind(geo, cell, bands);
            let dry = if *elevation.get(cell) >= sea_level {
                rain_shadow(geo, elevation, cell, wind)
            } else {
                0.0
            };
            let raw = base + ocean_bonus(geo, elevation, sea_level, cell) - 0.5 * dry;
            raw.clamp(0.0, 1.0)
        }),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_kernel::Geosphere;

    #[test]
    fn moisture_is_bounded_and_wetter_in_rising_bands() {
        let geo = Geosphere::new(4);
        let elev = CellMap::from_fn(&geo, |_| ReferenceElevation::new(100.0).unwrap()); // all land, flat
        let m = moisture_field(
            &geo,
            &elev,
            ReferenceElevation::new(0.0).unwrap(),
            &RotationRegime::Spinning { day_std: 1.0 },
        );
        for (_, v) in m.iter() {
            assert!((0.0..=1.0).contains(v));
        }
        // Equatorial (band 0, rising) mean vs horse-latitude (band 1, sinking) mean.
        let mean_band = |band: u32| {
            let cells: Vec<f64> = geo
                .cells()
                .filter(|c| band_index(geo.coord(*c).latitude, 3) == band)
                .map(|c| *m.get(c))
                .collect();
            cells.iter().sum::<f64>() / cells.len() as f64
        };
        assert!(
            mean_band(0) > mean_band(1),
            "rising band not wetter than sinking band"
        );
    }

    #[test]
    fn leeward_of_a_ridge_is_drier_than_windward() {
        let geo = Geosphere::new(5);
        let sea = ReferenceElevation::new(0.0).unwrap();
        // A single tall cell; compare its immediate upwind vs downwind neighbor.
        let ridge = geo.cells().nth(2000).unwrap();
        // Build a map via from_fn closure capturing the ridge id.
        let elev = CellMap::from_fn(&geo, |c| {
            ReferenceElevation::new(if c == ridge { 5000.0 } else { 200.0 }).unwrap()
        });
        let regime = RotationRegime::Spinning { day_std: 1.0 };
        let bands = band_count_for(&regime).unwrap();
        let wind = prevailing_wind(&geo, ridge, bands);
        // Windward neighbor: the one most upwind (opposite wind). Leeward: downwind.
        let score = |c: CellId, sign: f64| {
            let d = [
                geo.position(c)[0] - geo.position(ridge)[0],
                geo.position(c)[1] - geo.position(ridge)[1],
                geo.position(c)[2] - geo.position(ridge)[2],
            ];
            sign * (d[0] * wind[0] + d[1] * wind[1] + d[2] * wind[2])
        };
        let windward = *geo
            .neighbors(ridge)
            .iter()
            .max_by(|a, b| score(**a, -1.0).total_cmp(&score(**b, -1.0)))
            .unwrap();
        let leeward = *geo
            .neighbors(ridge)
            .iter()
            .max_by(|a, b| score(**a, 1.0).total_cmp(&score(**b, 1.0)))
            .unwrap();
        let m = moisture_field(&geo, &elev, sea, &regime);
        assert!(
            m.get(leeward) < m.get(windward),
            "leeward {} not drier than windward {}",
            m.get(leeward),
            m.get(windward)
        );
    }

    #[test]
    fn locked_terminator_is_wetter_than_substellar_and_antistellar() {
        let geo = Geosphere::new(4);
        let elev = CellMap::from_fn(&geo, |_| ReferenceElevation::new(100.0).unwrap());
        let m = moisture_field(
            &geo,
            &elev,
            ReferenceElevation::new(0.0).unwrap(),
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
        let term = geo
            .cells()
            .min_by(|a, b| {
                geo.position(*a)[0]
                    .abs()
                    .total_cmp(&geo.position(*b)[0].abs())
            })
            .unwrap();
        assert!(m.get(term) > m.get(sub));
        assert!(m.get(term) > m.get(anti));
    }
}

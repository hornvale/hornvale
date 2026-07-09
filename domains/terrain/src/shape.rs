//! Shape metrics over a generated globe: the quantitative "before
//! photograph" for the terrain overhaul (Campaign 25 spec §2). Pure
//! functions over narrow inputs so tests build synthetic globes cheaply.
//! Discrete estimators (documented per function) are declared
//! approximations; each is deterministic and consistent across metrics.

use hornvale_kernel::{CellMap, Geosphere};

/// Half-width of the shelf band around sea level, meters (Earth's
/// continental shelf lies within ~200 m of the sea surface).
pub const SHELF_BAND_M: f64 = 200.0;

/// Angular distance between two unit vectors, radians.
fn angle(a: [f64; 3], b: [f64; 3]) -> f64 {
    (a[0] * b[0] + a[1] * b[1] + a[2] * b[2])
        .clamp(-1.0, 1.0)
        .acos()
}

/// Shoreline development index `D = L / (2 sqrt(pi A))`: coastline length
/// over the circumference of the circle with the land's area. 1 is
/// maximally compact; fjorded coasts score several times that. Estimators:
/// cell area is the equal-area approximation `4 pi / N`; the shared edge
/// between two neighboring cells is approximated as their center distance
/// over sqrt(3) (the regular-hexagon dual). `None` when the globe has no
/// land or no shoreline.
pub fn shoreline_development(
    geo: &Geosphere,
    elevation: &CellMap<f64>,
    sea_level: f64,
) -> Option<f64> {
    let cell_area = 4.0 * std::f64::consts::PI / geo.cell_count() as f64;
    let mut land_area = 0.0;
    let mut shoreline = 0.0;
    for cell in geo.cells() {
        let land = *elevation.get(cell) >= sea_level;
        if land {
            land_area += cell_area;
        }
        for &neighbor in geo.neighbors(cell) {
            // Each unordered pair once.
            if neighbor.0 <= cell.0 {
                continue;
            }
            let neighbor_land = *elevation.get(neighbor) >= sea_level;
            if land != neighbor_land {
                shoreline += angle(geo.position(cell), geo.position(neighbor)) / 3f64.sqrt();
            }
        }
    }
    if land_area == 0.0 || shoreline == 0.0 {
        return None;
    }
    Some(shoreline / (2.0 * (std::f64::consts::PI * land_area).sqrt()))
}

/// Fraction of all cells within [`SHELF_BAND_M`] of sea level — Earth's
/// hypsometry keeps a populated shelf here; a cliff-coast generator does
/// not.
pub fn shelf_fraction(elevation: &CellMap<f64>, sea_level: f64) -> f64 {
    let within = elevation
        .iter()
        .filter(|(_, e)| (**e - sea_level).abs() <= SHELF_BAND_M)
        .count();
    within as f64 / elevation.len() as f64
}

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_kernel::{CellMap, Geosphere};

    /// Land where the cell's z-coordinate clears `z_min` — a polar cap.
    fn cap_elevation(geo: &Geosphere, z_min: f64) -> CellMap<f64> {
        CellMap::from_fn(geo, |c| {
            if geo.position(c)[2] >= z_min {
                100.0
            } else {
                -100.0
            }
        })
    }

    #[test]
    fn a_compact_cap_scores_near_one_and_stripes_score_higher() {
        let geo = Geosphere::new(3);
        let cap = cap_elevation(&geo, 0.5);
        let d_cap = shoreline_development(&geo, &cap, 0.0).expect("cap has a shoreline");
        assert!((0.6..=1.4).contains(&d_cap), "compact cap D = {d_cap}");
        // Same latitude band, but land only in alternating longitude sectors:
        // far more shoreline for less area.
        let stripes = CellMap::from_fn(&geo, |c| {
            let p = geo.position(c);
            let sector =
                ((p[1].atan2(p[0]) + std::f64::consts::PI) / (std::f64::consts::PI / 6.0)) as i64;
            if p[2] >= 0.5 && sector % 2 == 0 {
                100.0
            } else {
                -100.0
            }
        });
        let d_stripes = shoreline_development(&geo, &stripes, 0.0).expect("stripes shoreline");
        assert!(d_stripes > d_cap, "stripes {d_stripes} <= cap {d_cap}");
    }

    #[test]
    fn worlds_without_a_shoreline_are_absent() {
        let geo = Geosphere::new(2);
        let all_land = CellMap::from_fn(&geo, |_| 100.0);
        let all_ocean = CellMap::from_fn(&geo, |_| -100.0);
        assert_eq!(shoreline_development(&geo, &all_land, 0.0), None);
        assert_eq!(shoreline_development(&geo, &all_ocean, 0.0), None);
    }

    #[test]
    fn shelf_fraction_counts_the_band_around_sea_level() {
        let geo = Geosphere::new(3);
        // Elevation = 1000·z: the ±200 m band is |z| <= 0.2, ~20% of a
        // sphere by area (z is area-uniform).
        let e = CellMap::from_fn(&geo, |c| 1000.0 * geo.position(c)[2]);
        let f = shelf_fraction(&e, 0.0);
        assert!((0.12..=0.28).contains(&f), "shelf fraction {f}");
        let flat = CellMap::from_fn(&geo, |_| 50.0);
        assert_eq!(shelf_fraction(&flat, 0.0), 1.0);
    }
}

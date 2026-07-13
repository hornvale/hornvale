//! Shape metrics over a generated globe: the quantitative "before
//! photograph" for the terrain overhaul (Campaign 25 spec §2). Pure
//! functions over narrow inputs so tests build synthetic globes cheaply.
//! Discrete estimators (documented per function) are declared
//! approximations; each is deterministic and consistent across metrics.

use hornvale_kernel::{CellMap, Geosphere, ReferenceElevation, math};
use std::collections::VecDeque;

/// Half-width of the shelf band around sea level, meters (Earth's
/// continental shelf lies within ~200 m of the sea surface).
/// type-audit: pending(wave-2)
pub const SHELF_BAND_M: f64 = 200.0;

/// Angular distance between two unit vectors, radians.
fn angle(a: [f64; 3], b: [f64; 3]) -> f64 {
    math::acos((a[0] * b[0] + a[1] * b[1] + a[2] * b[2]).clamp(-1.0, 1.0))
}

/// Shoreline development index `D = L / (2 sqrt(pi A))`: coastline length
/// over the circumference of the circle with the land's area. 1 is
/// maximally compact; fjorded coasts score several times that. Estimators:
/// cell area is the equal-area approximation `4 pi / N`; the shared edge
/// between two neighboring cells is approximated as their center distance
/// over sqrt(3) (the regular-hexagon dual). `None` when the globe has no
/// land or no shoreline.
/// type-audit: bare-ok(ratio: return)
pub fn shoreline_development(
    geo: &Geosphere,
    elevation: &CellMap<ReferenceElevation>,
    sea_level: ReferenceElevation,
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
/// type-audit: bare-ok(ratio: return)
pub fn shelf_fraction(
    elevation: &CellMap<ReferenceElevation>,
    sea_level: ReferenceElevation,
) -> f64 {
    let within = elevation
        .iter()
        .filter(|(_, e)| (**e - sea_level).abs() <= SHELF_BAND_M)
        .count();
    within as f64 / elevation.len() as f64
}

/// Ashman's D between the land and ocean elevation populations:
/// `|mean_land - mean_ocean| / sqrt((var_land + var_ocean) / 2)` with
/// population variance. Earth's hypsometry is strongly bimodal (high D).
/// `None` when either population is empty or both are degenerate
/// (zero variance).
/// type-audit: bare-ok(ratio: return)
pub fn hypsometric_bimodality(
    elevation: &CellMap<ReferenceElevation>,
    sea_level: ReferenceElevation,
) -> Option<f64> {
    let mut land = Vec::new();
    let mut ocean = Vec::new();
    for (_, e) in elevation.iter() {
        if *e >= sea_level {
            land.push(e.get());
        } else {
            ocean.push(e.get());
        }
    }
    fn stats(values: &[f64]) -> Option<(f64, f64)> {
        if values.is_empty() {
            return None;
        }
        let n = values.len() as f64;
        let mean = values.iter().sum::<f64>() / n;
        let variance = values.iter().map(|x| (x - mean).powi(2)).sum::<f64>() / n;
        Some((mean, variance))
    }
    let (mean_land, var_land) = stats(&land)?;
    let (mean_ocean, var_ocean) = stats(&ocean)?;
    let denominator = ((var_land + var_ocean) / 2.0).sqrt();
    if denominator <= f64::EPSILON {
        return None;
    }
    Some((mean_land - mean_ocean).abs() / denominator)
}

/// Sizes (cell counts) of connected land components, descending. BFS in
/// ascending cell-id order — fully deterministic. Empty when there is no
/// land.
/// type-audit: bare-ok(count: return)
pub fn land_component_sizes(
    geo: &Geosphere,
    elevation: &CellMap<ReferenceElevation>,
    sea_level: ReferenceElevation,
) -> Vec<usize> {
    let mut visited = vec![false; geo.cell_count()];
    let mut sizes = Vec::new();
    for start in geo.cells() {
        if visited[start.0 as usize] || *elevation.get(start) < sea_level {
            continue;
        }
        visited[start.0 as usize] = true;
        let mut queue = VecDeque::from([start]);
        let mut size = 0usize;
        while let Some(cell) = queue.pop_front() {
            size += 1;
            for &neighbor in geo.neighbors(cell) {
                if !visited[neighbor.0 as usize] && *elevation.get(neighbor) >= sea_level {
                    visited[neighbor.0 as usize] = true;
                    queue.push_back(neighbor);
                }
            }
        }
        sizes.push(size);
    }
    sizes.sort_unstable_by(|a, b| b.cmp(a));
    sizes
}

/// Gini coefficient over nonnegative counts (0 = equal, →1 = concentrated):
/// `G = 2 Σ i·x_i / (n Σ x) − (n+1)/n` over ascending x with 1-based i.
/// `None` for an empty slice or an all-zero total.
/// type-audit: bare-ok(count: counts), bare-ok(ratio: return)
pub fn gini(counts: &[usize]) -> Option<f64> {
    if counts.is_empty() {
        return None;
    }
    let total: usize = counts.iter().sum();
    if total == 0 {
        return None;
    }
    let mut sorted = counts.to_vec();
    sorted.sort_unstable();
    let n = sorted.len() as f64;
    let weighted: f64 = sorted
        .iter()
        .enumerate()
        .map(|(i, x)| (i as f64 + 1.0) * *x as f64)
        .sum();
    Some(2.0 * weighted / (n * total as f64) - (n + 1.0) / n)
}

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_kernel::{CellMap, Geosphere, ReferenceElevation};

    /// Land where the cell's z-coordinate clears `z_min` — a polar cap.
    fn cap_elevation(geo: &Geosphere, z_min: f64) -> CellMap<ReferenceElevation> {
        CellMap::from_fn(geo, |c| {
            let m = if geo.position(c)[2] >= z_min {
                100.0
            } else {
                -100.0
            };
            ReferenceElevation::new(m).unwrap()
        })
    }

    #[test]
    fn a_compact_cap_scores_near_one_and_stripes_score_higher() {
        let geo = Geosphere::new(3);
        let sea = ReferenceElevation::new(0.0).unwrap();
        let cap = cap_elevation(&geo, 0.5);
        let d_cap = shoreline_development(&geo, &cap, sea).expect("cap has a shoreline");
        assert!((0.6..=1.4).contains(&d_cap), "compact cap D = {d_cap}");
        // Same latitude band, but land only in alternating longitude sectors:
        // far more shoreline for less area.
        let stripes = CellMap::from_fn(&geo, |c| {
            let p = geo.position(c);
            let sector = ((math::atan2(p[1], p[0]) + std::f64::consts::PI)
                / (std::f64::consts::PI / 6.0)) as i64;
            let m = if p[2] >= 0.5 && sector % 2 == 0 {
                100.0
            } else {
                -100.0
            };
            ReferenceElevation::new(m).unwrap()
        });
        let d_stripes = shoreline_development(&geo, &stripes, sea).expect("stripes shoreline");
        assert!(d_stripes > d_cap, "stripes {d_stripes} <= cap {d_cap}");
    }

    #[test]
    fn worlds_without_a_shoreline_are_absent() {
        let geo = Geosphere::new(2);
        let sea = ReferenceElevation::new(0.0).unwrap();
        let all_land = CellMap::from_fn(&geo, |_| ReferenceElevation::new(100.0).unwrap());
        let all_ocean = CellMap::from_fn(&geo, |_| ReferenceElevation::new(-100.0).unwrap());
        assert_eq!(shoreline_development(&geo, &all_land, sea), None);
        assert_eq!(shoreline_development(&geo, &all_ocean, sea), None);
    }

    #[test]
    fn shelf_fraction_counts_the_band_around_sea_level() {
        let geo = Geosphere::new(3);
        let sea = ReferenceElevation::new(0.0).unwrap();
        // Elevation = 1000·z: the ±200 m band is |z| <= 0.2, ~20% of a
        // sphere by area (z is area-uniform).
        let e = CellMap::from_fn(&geo, |c| {
            ReferenceElevation::new(1000.0 * geo.position(c)[2]).unwrap()
        });
        let f = shelf_fraction(&e, sea);
        assert!((0.12..=0.28).contains(&f), "shelf fraction {f}");
        let flat = CellMap::from_fn(&geo, |_| ReferenceElevation::new(50.0).unwrap());
        assert_eq!(shelf_fraction(&flat, sea), 1.0);
    }

    #[test]
    fn separated_modes_score_higher_than_a_split_unimodal_field() {
        let geo = Geosphere::new(3);
        let sea = ReferenceElevation::new(0.0).unwrap();
        // Earth-like: two tight modes far apart (tiny within-mode spread so
        // Ashman's denominator is nonzero).
        let bimodal = CellMap::from_fn(&geo, |c| {
            let z = geo.position(c)[2];
            let m = if z >= 0.0 { 400.0 + z } else { -4000.0 + z };
            ReferenceElevation::new(m).unwrap()
        });
        // A single uniform ramp split at sea level.
        let unimodal = CellMap::from_fn(&geo, |c| {
            ReferenceElevation::new(100.0 * geo.position(c)[2]).unwrap()
        });
        let d_bi = hypsometric_bimodality(&bimodal, sea).expect("bimodal");
        let d_uni = hypsometric_bimodality(&unimodal, sea).expect("unimodal");
        assert!(d_bi > 10.0 * d_uni, "bimodal {d_bi} vs unimodal {d_uni}");
        let all_land = CellMap::from_fn(&geo, |_| ReferenceElevation::new(100.0).unwrap());
        assert_eq!(hypsometric_bimodality(&all_land, sea), None);
    }

    #[test]
    fn antipodal_caps_are_two_components_sorted_descending() {
        let geo = Geosphere::new(3);
        let sea = ReferenceElevation::new(0.0).unwrap();
        // North cap bigger than south cap.
        let e = CellMap::from_fn(&geo, |c| {
            let z = geo.position(c)[2];
            let m = if z >= 0.5 || z <= -0.8 { 100.0 } else { -100.0 };
            ReferenceElevation::new(m).unwrap()
        });
        let sizes = land_component_sizes(&geo, &e, sea);
        assert_eq!(sizes.len(), 2, "components: {sizes:?}");
        assert!(sizes[0] > sizes[1]);
        let ocean = CellMap::from_fn(&geo, |_| ReferenceElevation::new(-1.0).unwrap());
        assert!(land_component_sizes(&geo, &ocean, sea).is_empty());
    }

    #[test]
    fn gini_is_zero_for_equal_counts_and_high_for_concentration() {
        assert_eq!(gini(&[5, 5, 5, 5]), Some(0.0));
        let g = gini(&[0, 0, 0, 10]).expect("nonzero total");
        assert!((g - 0.75).abs() < 1e-12, "gini {g}");
        assert_eq!(gini(&[]), None);
        assert_eq!(gini(&[0, 0]), None);
    }
}

//! Habitability: the non-opinionated "where a vale-like place could be" —
//! land, with liquid water available, in a tolerable temperature band
//! (spec §6). It pre-wires the embark seam and yields the Lab's habitable-
//! fraction unknown number (spec §10).

use hornvale_kernel::{CellMap, Geosphere, ReferenceElevation, Temperature};

/// Coldest tolerable annual-mean temperature (°C).
const HABITABLE_MIN_C: f64 = -5.0;
/// Hottest tolerable annual-mean temperature (°C).
const HABITABLE_MAX_C: f64 = 35.0;
/// Aridity floor: below this moisture there is no reliable liquid water.
const HABITABLE_MIN_MOISTURE: f64 = 0.2;

/// Whether a cell could host a vale-like settlement.
/// type-audit: bare-ok(ratio: moisture), bare-ok(flag: return)
pub fn is_habitable(
    temp_c: Temperature,
    moisture: f64,
    elevation_m: ReferenceElevation,
    sea_level_m: ReferenceElevation,
) -> bool {
    let min = Temperature::new(HABITABLE_MIN_C).expect("habitable min is finite");
    let max = Temperature::new(HABITABLE_MAX_C).expect("habitable max is finite");
    elevation_m >= sea_level_m
        && (min..=max).contains(&temp_c)
        && moisture >= HABITABLE_MIN_MOISTURE
}

/// The per-cell habitability mask.
/// type-audit: bare-ok(ratio: moisture), bare-ok(flag: return)
pub fn habitability_map(
    geo: &Geosphere,
    elevation: &CellMap<ReferenceElevation>,
    mean_temp: &CellMap<Temperature>,
    moisture: &CellMap<f64>,
    sea_level: ReferenceElevation,
) -> CellMap<bool> {
    CellMap::from_fn(geo, |cell| {
        is_habitable(
            *mean_temp.get(cell),
            *moisture.get(cell),
            *elevation.get(cell),
            sea_level,
        )
    })
}

/// The fraction of cells that are habitable.
/// type-audit: bare-ok(flag: map), bare-ok(ratio: return)
pub fn habitable_fraction(map: &CellMap<bool>) -> f64 {
    if map.is_empty() {
        return 0.0;
    }
    let count = map.iter().filter(|(_, h)| **h).count();
    count as f64 / map.len() as f64
}

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_kernel::Geosphere;

    /// Test-only helper: a validated `Temperature`.
    fn t(c: f64) -> Temperature {
        Temperature::new(c).unwrap()
    }

    #[test]
    fn ocean_and_extremes_are_uninhabitable_temperate_land_is_habitable() {
        let sea = ReferenceElevation::new(0.0).unwrap();
        assert!(!is_habitable(
            t(20.0),
            0.5,
            ReferenceElevation::new(-100.0).unwrap(),
            sea
        )); // ocean
        assert!(!is_habitable(
            t(50.0),
            0.5,
            ReferenceElevation::new(100.0).unwrap(),
            sea
        )); // too hot
        assert!(!is_habitable(
            t(15.0),
            0.05,
            ReferenceElevation::new(100.0).unwrap(),
            sea
        )); // too dry
        assert!(is_habitable(
            t(15.0),
            0.5,
            ReferenceElevation::new(100.0).unwrap(),
            sea
        ));
    }

    #[test]
    fn fraction_is_between_zero_and_one() {
        let geo = Geosphere::new(3);
        let elev = CellMap::from_fn(&geo, |c| {
            let m = if geo.position(c)[2] > 0.0 {
                200.0
            } else {
                -200.0
            };
            ReferenceElevation::new(m).unwrap()
        });
        let temp = CellMap::from_fn(&geo, |_| t(15.0));
        let moist = CellMap::from_fn(&geo, |_| 0.5);
        let map = habitability_map(
            &geo,
            &elev,
            &temp,
            &moist,
            ReferenceElevation::new(0.0).unwrap(),
        );
        let f = habitable_fraction(&map);
        assert!((0.0..=1.0).contains(&f));
        assert!(f > 0.0, "some northern land should be habitable");
    }
}

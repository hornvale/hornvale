//! Strata: the durable marks a glacial history leaves on the present. Extracted
//! from a series of coarse climate snapshots (`EraClimate`) supplied by the
//! composition root. The full fields live here on the non-serialized
//! `PaleoRecord`; only summaries become facts (see `facts`).

use hornvale_kernel::{CellMap, Geosphere};

/// Land colder than this offset temperature is under ice (°C).
const GLACIATION_THRESHOLD_C: f64 = -3.0;

/// One coarse era's climate fields, all bare kernel types, filled by the
/// composition root after re-running climate at the era's sea level and
/// applying the era's albedo cooling offset to the temperature field.
#[derive(Debug, Clone)]
pub struct EraClimate {
    /// Absolute standard day of the era.
    pub day: f64,
    /// Offset annual-mean temperature per cell (°C), cooling already applied.
    pub temperature: CellMap<f64>,
    /// Habitability per cell under this era's offset climate.
    pub habitable: CellMap<bool>,
    /// Sea level this era (metres): present + eustatic change.
    pub sea_level: f64,
    /// Land fraction under ice this era (for the glacial-maximum summary).
    pub ice_fraction: f64,
}

/// The diagnostic ice mask for one era: land (elevation ≥ that era's sea level)
/// whose offset temperature is below the glaciation threshold.
pub fn glaciated(
    geo: &Geosphere,
    elevation: &CellMap<f64>,
    temperature: &CellMap<f64>,
    sea_level: f64,
) -> CellMap<bool> {
    CellMap::from_fn(geo, |cell| {
        let elev = *elevation.get(cell);
        elev >= sea_level && *temperature.get(cell) < GLACIATION_THRESHOLD_C
    })
}

/// The extracted strata of a world. Non-serialized; re-derived on demand.
#[derive(Debug, Clone)]
pub struct PaleoRecord {
    /// Union of every era's ice mask ("this valley was under ice").
    pub envelope: CellMap<bool>,
    /// The tide-mark band swept by eustatic sea level across eras.
    pub shoreline: CellMap<bool>,
    /// Cells habitable through the glacial maximum.
    pub refugia: CellMap<bool>,
    /// Absolute standard day of peak ice.
    pub glacial_maximum_day: f64,
    /// Land fraction under ice at the maximum.
    pub max_ice_fraction: f64,
}

/// Extract strata from the era series over the present relief.
pub fn extract(
    geo: &Geosphere,
    elevation: &CellMap<f64>,
    present_sea_level: f64,
    eras: &[EraClimate],
) -> PaleoRecord {
    // Sea-level band: cells sometimes shore, sometimes not, across all eras
    // (including the present stand).
    let mut min_sea = present_sea_level;
    let mut max_sea = present_sea_level;
    for e in eras {
        min_sea = min_sea.min(e.sea_level);
        max_sea = max_sea.max(e.sea_level);
    }
    let shoreline = CellMap::from_fn(geo, |cell| {
        let elev = *elevation.get(cell);
        (min_sea..=max_sea).contains(&elev)
    });

    // Ice-extent envelope: OR of every era's diagnostic ice mask.
    let mut envelope = CellMap::from_fn(geo, |_| false);
    for e in eras {
        let mask = glaciated(geo, elevation, &e.temperature, e.sea_level);
        envelope = CellMap::from_fn(geo, |cell| {
            let had = *envelope.get(cell);
            had || *mask.get(cell)
        });
    }

    // Glacial maximum: the era with the greatest ice fraction (ties → earliest
    // day, for determinism).
    let peak = eras.iter().max_by(|a, b| {
        a.ice_fraction
            .total_cmp(&b.ice_fraction)
            .then(b.day.total_cmp(&a.day))
    });
    let (glacial_maximum_day, max_ice_fraction, refugia) = match peak {
        Some(e) => (e.day, e.ice_fraction, e.habitable.clone()),
        None => (0.0, 0.0, CellMap::from_fn(geo, |_| false)),
    };

    PaleoRecord {
        envelope,
        shoreline,
        refugia,
        glacial_maximum_day,
        max_ice_fraction,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_kernel::Geosphere;

    fn era(geo: &Geosphere, day: f64, temp: f64, sea: f64, ice: f64) -> EraClimate {
        EraClimate {
            day,
            temperature: CellMap::from_fn(geo, |_| temp),
            habitable: CellMap::from_fn(geo, |c| geo.coord(c).latitude.abs() < 45.0),
            sea_level: sea,
            ice_fraction: ice,
        }
    }

    #[test]
    fn envelope_unions_cold_eras() {
        let geo = Geosphere::new(3);
        let elev = CellMap::from_fn(&geo, |_| 100.0); // all land
        let eras = vec![
            era(&geo, 0.0, 5.0, 0.0, 0.0),     // warm: no ice
            era(&geo, 1.0, -20.0, -50.0, 0.9), // cold: all ice
        ];
        let rec = extract(&geo, &elev, 0.0, &eras);
        assert!(
            rec.envelope.iter().all(|(_, &b)| b),
            "cold era ices every land cell"
        );
        assert_eq!(rec.max_ice_fraction, 0.9);
        assert_eq!(rec.glacial_maximum_day, 1.0);
    }

    #[test]
    fn shoreline_is_the_swept_band() {
        let geo = Geosphere::new(3);
        // Elevation ramps with latitude so some cells fall in the band.
        let elev = CellMap::from_fn(&geo, |c| geo.coord(c).latitude);
        let eras = vec![era(&geo, 0.0, 5.0, -30.0, 0.0)];
        let rec = extract(&geo, &elev, 0.0, &eras); // band = [-30, 0]
        let any = rec.shoreline.iter().any(|(_, &b)| b);
        assert!(any, "some cells must lie in the [-30,0] sea band");
    }
}

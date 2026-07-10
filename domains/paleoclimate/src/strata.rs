//! Strata: the durable marks a glacial history leaves on the present. Extracted
//! from a series of coarse climate snapshots (`EraClimate`) supplied by the
//! composition root. The full fields live here on the non-serialized
//! `PaleoRecord`; only summaries become facts (see `facts`).

use crate::units::Celsius;
use hornvale_kernel::{CellMap, Geosphere};

/// One coarse era's climate fields, all bare kernel types, filled by the
/// composition root after re-running climate at the era's sea level and
/// applying the era's albedo cooling offset to the temperature field.
#[derive(Debug, Clone)]
pub struct EraClimate {
    /// Absolute standard day of the era.
    pub day: f64,
    /// This era's precomputed ice-ADVANCE mask: land iced this era that is
    /// NOT iced at present (see the composition root's `climate_at_era`).
    /// Advance, not raw glaciation, is what strata preserve — it is what
    /// keeps the null control exact (zero forcing ⇒ every era's glaciation
    /// equals the present's ⇒ zero advance everywhere) regardless of how
    /// cold a world's present poles already run. The single source of
    /// truth: `ice_fraction` and the envelope in `extract` both derive from
    /// this same mask, so they cannot disagree with each other about which
    /// cells advanced.
    pub ice: CellMap<bool>,
    /// Habitability per cell under this era's offset climate.
    pub habitable: CellMap<bool>,
    /// Sea level this era (metres): present + eustatic change.
    pub sea_level: f64,
    /// Land fraction under ice this era (for the glacial-maximum summary).
    pub ice_fraction: f64,
}

/// The diagnostic ice mask for one snapshot: land (elevation ≥ `sea_level`)
/// whose ABSOLUTE temperature is below `freeze`.
///
/// An absolute snowline, not an anomaly threshold (decision 0008 extended):
/// an anomaly-only diagnostic makes glaciation spatially flat, because the
/// only per-era signal is a single global albedo-cooling scalar — every
/// cell's anomaly is identical, so it is either all-iced or none-iced. The
/// world's present temperature field varies by latitude (equator warm, pole
/// cold), so comparing an ABSOLUTE reading against a fixed freezing point
/// lets the same global cooling offset move a latitudinal snowline instead
/// of flipping the whole globe at once. Callers diagnose an era's ice by
/// first adding that era's cooling offset to the present temperature field
/// (`Celsius`'s `Add` impl) and passing the result here; see the
/// composition root's `climate_at_era` for the advance-beyond-present
/// convention this feeds.
///
/// Takes a per-cell [`Celsius`], not a bare `f64` (decision 0008): an
/// earlier version of this function accepted an anomaly and callers twice
/// mixed up the two conventions. `Celsius` and [`crate::units::TempAnomaly`]
/// stay distinct types precisely so that mistake can't happen again.
pub fn glaciated(
    geo: &Geosphere,
    elevation: &CellMap<f64>,
    temperature: &CellMap<Celsius>,
    freeze: Celsius,
    sea_level: f64,
) -> CellMap<bool> {
    CellMap::from_fn(geo, |cell| {
        let elev = *elevation.get(cell);
        elev >= sea_level && temperature.get(cell).get() < freeze.get()
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

/// Extract strata from the era series over the present relief. The
/// ice-extent envelope is the OR-union of each era's precomputed `ice` mask
/// — it never re-diagnoses glaciation from temperature, so it cannot
/// disagree with the era's own `ice_fraction` (both derive from the same
/// mask, computed once by the composition root).
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

    // Ice-extent envelope: OR of every era's precomputed diagnostic ice mask.
    let mut envelope = CellMap::from_fn(geo, |_| false);
    for e in eras {
        envelope = CellMap::from_fn(geo, |cell| {
            let had = *envelope.get(cell);
            had || *e.ice.get(cell)
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

    #[test]
    fn glaciated_ices_land_under_a_cold_field() {
        let geo = Geosphere::new(3);
        let elevation = CellMap::from_fn(&geo, |_| 100.0); // all land
        let temperature = CellMap::from_fn(&geo, |_| Celsius::new(-10.0).unwrap());
        let freeze = Celsius::new(0.0).unwrap();
        let ice = glaciated(&geo, &elevation, &temperature, freeze, 0.0);
        assert!(
            ice.iter().all(|(_, &b)| b),
            "land colder than the freeze threshold everywhere must all be iced"
        );
    }

    #[test]
    fn glaciated_leaves_land_bare_under_a_warm_field() {
        let geo = Geosphere::new(3);
        let elevation = CellMap::from_fn(&geo, |_| 100.0); // all land
        let temperature = CellMap::from_fn(&geo, |_| Celsius::new(20.0).unwrap());
        let freeze = Celsius::new(0.0).unwrap();
        let ice = glaciated(&geo, &elevation, &temperature, freeze, 0.0);
        assert!(
            ice.iter().all(|(_, &b)| !b),
            "land warmer than the freeze threshold everywhere must have no ice"
        );
    }

    #[test]
    fn glaciated_never_ices_ocean_regardless_of_temperature() {
        let geo = Geosphere::new(3);
        let elevation = CellMap::from_fn(&geo, |_| -100.0); // all ocean
        let temperature = CellMap::from_fn(&geo, |_| Celsius::new(-10.0).unwrap());
        let freeze = Celsius::new(0.0).unwrap();
        let ice = glaciated(&geo, &elevation, &temperature, freeze, 0.0);
        assert!(
            ice.iter().all(|(_, &b)| !b),
            "ocean cells are never marked as glaciated land"
        );
    }

    #[test]
    fn glaciated_moves_a_latitudinal_snowline() {
        // A field that varies with latitude produces a spatially structured
        // mask (high latitudes iced, low latitudes bare) — the defect this
        // model fixes: an anomaly-only diagnostic could only ice everything
        // or nothing at once.
        let geo = Geosphere::new(4);
        let elevation = CellMap::from_fn(&geo, |_| 100.0); // all land
        let temperature = CellMap::from_fn(&geo, |c| {
            Celsius::new(30.0 - geo.coord(c).latitude.abs()).unwrap()
        });
        let freeze = Celsius::new(0.0).unwrap();
        let ice = glaciated(&geo, &elevation, &temperature, freeze, 0.0);
        assert!(
            ice.iter().any(|(_, &b)| b),
            "high latitudes must ice under this field"
        );
        assert!(
            ice.iter().any(|(_, &b)| !b),
            "low latitudes must stay bare under this field"
        );
    }

    fn era(geo: &Geosphere, day: f64, ice_all: bool, sea: f64, ice_fraction: f64) -> EraClimate {
        EraClimate {
            day,
            ice: CellMap::from_fn(geo, |_| ice_all),
            habitable: CellMap::from_fn(geo, |c| geo.coord(c).latitude.abs() < 45.0),
            sea_level: sea,
            ice_fraction,
        }
    }

    #[test]
    fn envelope_unions_cold_eras() {
        let geo = Geosphere::new(3);
        let elev = CellMap::from_fn(&geo, |_| 100.0); // all land
        let eras = vec![
            era(&geo, 0.0, false, 0.0, 0.0),  // warm: no ice
            era(&geo, 1.0, true, -50.0, 0.9), // cold: all ice
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
        let eras = vec![era(&geo, 0.0, false, -30.0, 0.0)];
        let rec = extract(&geo, &elev, 0.0, &eras); // band = [-30, 0]
        let any = rec.shoreline.iter().any(|(_, &b)| b);
        assert!(any, "some cells must lie in the [-30,0] sea band");
    }
}

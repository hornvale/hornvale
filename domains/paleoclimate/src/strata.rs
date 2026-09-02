//! Strata: the durable marks a glacial history leaves on the present. Extracted
//! from a series of coarse climate snapshots (`EraClimate`) supplied by the
//! composition root. The full fields live here on the non-serialized
//! `PaleoRecord`; only summaries become facts (see `facts`).

use hornvale_kernel::{Geosphere, ReferenceElevation, Temperature, VertexMap, WorldTime};

/// One coarse era's climate fields, filled by the composition root after
/// re-running climate at the era's sea level and applying the era's albedo
/// cooling offset to the temperature field.
///
/// **`day` is one axis on every producer, since The Hallmark's Task 13, and a
/// typed [`WorldTime`] since Task 15** — the idea registry's
/// `DOM-era-day-axis`, ledger entries #10 (the diagnosis), #15's own record
/// (the source fix), and #17 (the retype). It was not always so, and the
/// history is worth keeping because the field looked healthy the whole time
/// it was wrong: `paleoclimate_from` wrote absolute standard days while
/// `bake_eras` wrote bake YEARS into the same slot, and nothing objected,
/// because the two paths never met at one consumer. Both producers now derive
/// `day` from the identical deep-time expression, converted to `WorldTime`
/// once at the crossing; the history bake's own `[start_year, end_year)`
/// window travels beside the era series as `history_bake::bake`'s
/// `era_years` argument, which is where a bake-side quantity belongs.
/// type-audit: bare-ok(flag: ice), bare-ok(flag: habitable), bare-ok(ratio: ice_fraction)
#[derive(Debug, Clone)]
pub struct EraClimate {
    /// Absolute standard day of the era, on every producer path.
    pub day: WorldTime,
    /// This era's precomputed ice-ADVANCE mask: land iced this era that is
    /// NOT iced at present (see the composition root's `climate_at_era`).
    /// Advance, not raw glaciation, is what strata preserve — it is what
    /// keeps the null control exact (zero forcing ⇒ every era's glaciation
    /// equals the present's ⇒ zero advance everywhere) regardless of how
    /// cold a world's present poles already run. The single source of
    /// truth: `ice_fraction` and the envelope in `extract` both derive from
    /// this same mask, so they cannot disagree with each other about which
    /// vertices advanced.
    pub ice: VertexMap<bool>,
    /// Habitability per vertex under this era's offset climate.
    pub habitable: VertexMap<bool>,
    /// Sea level this era (metres): present + eustatic change.
    pub sea_level: ReferenceElevation,
    /// Land fraction under ice this era (for the glacial-maximum summary).
    pub ice_fraction: f64,
}

/// The diagnostic ice mask for one snapshot: land (elevation ≥ `sea_level`)
/// whose ABSOLUTE temperature is below `freeze`.
///
/// An absolute snowline, not an anomaly threshold (decision 0008 extended):
/// an anomaly-only diagnostic makes glaciation spatially flat, because the
/// only per-era signal is a single global albedo-cooling scalar — every
/// vertex's anomaly is identical, so it is either all-iced or none-iced. The
/// world's present temperature field varies by latitude (equator warm, pole
/// cold), so comparing an ABSOLUTE reading against a fixed freezing point
/// lets the same global cooling offset move a latitudinal snowline instead
/// of flipping the whole globe at once. Callers diagnose an era's ice by
/// first adding that era's cooling offset to the present temperature field
/// (`Temperature`'s `Add` impl) and passing the result here; see the
/// composition root's `climate_at_era` for the advance-beyond-present
/// convention this feeds.
///
/// Takes a per-vertex [`Temperature`], not a bare `f64` (decision 0008): an
/// earlier version of this function accepted an anomaly and callers twice
/// mixed up the two conventions. `Temperature` and [`hornvale_kernel::TempAnomaly`]
/// stay distinct types precisely so that mistake can't happen again.
/// type-audit: bare-ok(flag: return)
pub fn glaciated(
    geo: &Geosphere,
    elevation: &VertexMap<ReferenceElevation>,
    temperature: &VertexMap<Temperature>,
    freeze: Temperature,
    sea_level: ReferenceElevation,
) -> VertexMap<bool> {
    VertexMap::from_fn(geo, |vertex| {
        let elev = *elevation.get(vertex);
        elev >= sea_level && temperature.get(vertex).get() < freeze.get()
    })
}

/// The extracted strata of a world. Non-serialized; re-derived on demand.
///
/// **`glacial_maximum_day` is exactly [`EraClimate::day`]'s axis** — `extract`
/// copies it straight out of the peak era below, and it reaches the ledger
/// from there (`facts::genesis`). That inheritance is why the field could not
/// be typed `WorldTime` while `EraClimate::day` carried two axes; The
/// Hallmark's Task 13 repaired the source and Task 15 did the retype itself.
/// See `DOM-era-day-axis` in the idea registry.
/// type-audit: bare-ok(flag: envelope), bare-ok(flag: shoreline), bare-ok(flag: refugia), bare-ok(ratio: max_ice_fraction)
#[derive(Debug, Clone)]
pub struct PaleoRecord {
    /// Union of every era's ice mask ("this valley was under ice").
    pub envelope: VertexMap<bool>,
    /// The tide-mark band swept by eustatic sea level across eras.
    pub shoreline: VertexMap<bool>,
    /// Vertices habitable through the glacial maximum.
    pub refugia: VertexMap<bool>,
    /// Absolute standard day of peak ice.
    pub glacial_maximum_day: WorldTime,
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
    elevation: &VertexMap<ReferenceElevation>,
    present_sea_level: ReferenceElevation,
    eras: &[EraClimate],
) -> PaleoRecord {
    // Sea-level band: vertices sometimes shore, sometimes not, across all eras
    // (including the present stand).
    let mut min_sea = present_sea_level;
    let mut max_sea = present_sea_level;
    for e in eras {
        min_sea = min_sea.min(e.sea_level);
        max_sea = max_sea.max(e.sea_level);
    }
    let shoreline = VertexMap::from_fn(geo, |vertex| {
        let elev = *elevation.get(vertex);
        (min_sea..=max_sea).contains(&elev)
    });

    // Ice-extent envelope: OR of every era's precomputed diagnostic ice mask.
    let mut envelope = VertexMap::from_fn(geo, |_| false);
    for e in eras {
        envelope = VertexMap::from_fn(geo, |vertex| {
            let had = *envelope.get(vertex);
            had || *e.ice.get(vertex)
        });
    }

    // Glacial maximum: the era with the greatest ice fraction (ties → earliest
    // day, for determinism).
    let peak = eras.iter().max_by(|a, b| {
        a.ice_fraction
            .total_cmp(&b.ice_fraction)
            .then(b.day.cmp(&a.day))
    });
    let (glacial_maximum_day, max_ice_fraction, refugia) = match peak {
        Some(e) => (e.day, e.ice_fraction, e.habitable.clone()),
        None => (WorldTime::GENESIS, 0.0, VertexMap::from_fn(geo, |_| false)),
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

    /// Test-only helper: a validated `ReferenceElevation`.
    fn e(m: f64) -> ReferenceElevation {
        ReferenceElevation::new(m).unwrap()
    }

    #[test]
    fn glaciated_ices_land_under_a_cold_field() {
        let geo = Geosphere::new(3);
        let elevation = VertexMap::from_fn(&geo, |_| e(100.0)); // all land
        let temperature = VertexMap::from_fn(&geo, |_| Temperature::new(-10.0).unwrap());
        let freeze = Temperature::new(0.0).unwrap();
        let ice = glaciated(&geo, &elevation, &temperature, freeze, e(0.0));
        assert!(
            ice.iter().all(|(_, &b)| b),
            "land colder than the freeze threshold everywhere must all be iced"
        );
    }

    #[test]
    fn glaciated_leaves_land_bare_under_a_warm_field() {
        let geo = Geosphere::new(3);
        let elevation = VertexMap::from_fn(&geo, |_| e(100.0)); // all land
        let temperature = VertexMap::from_fn(&geo, |_| Temperature::new(20.0).unwrap());
        let freeze = Temperature::new(0.0).unwrap();
        let ice = glaciated(&geo, &elevation, &temperature, freeze, e(0.0));
        assert!(
            ice.iter().all(|(_, &b)| !b),
            "land warmer than the freeze threshold everywhere must have no ice"
        );
    }

    #[test]
    fn glaciated_never_ices_ocean_regardless_of_temperature() {
        let geo = Geosphere::new(3);
        let elevation = VertexMap::from_fn(&geo, |_| e(-100.0)); // all ocean
        let temperature = VertexMap::from_fn(&geo, |_| Temperature::new(-10.0).unwrap());
        let freeze = Temperature::new(0.0).unwrap();
        let ice = glaciated(&geo, &elevation, &temperature, freeze, e(0.0));
        assert!(
            ice.iter().all(|(_, &b)| !b),
            "ocean vertices are never marked as glaciated land"
        );
    }

    #[test]
    fn glaciated_moves_a_latitudinal_snowline() {
        // A field that varies with latitude produces a spatially structured
        // mask (high latitudes iced, low latitudes bare) — the defect this
        // model fixes: an anomaly-only diagnostic could only ice everything
        // or nothing at once.
        let geo = Geosphere::new(4);
        let elevation = VertexMap::from_fn(&geo, |_| e(100.0)); // all land
        let temperature = VertexMap::from_fn(&geo, |c| {
            Temperature::new(30.0 - geo.coord(c).latitude.abs()).unwrap()
        });
        let freeze = Temperature::new(0.0).unwrap();
        let ice = glaciated(&geo, &elevation, &temperature, freeze, e(0.0));
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
            day: WorldTime::from_std_days(day).expect("test era day within tick range"),
            ice: VertexMap::from_fn(geo, |_| ice_all),
            habitable: VertexMap::from_fn(geo, |c| geo.coord(c).latitude.abs() < 45.0),
            sea_level: e(sea),
            ice_fraction,
        }
    }

    #[test]
    fn envelope_unions_cold_eras() {
        let geo = Geosphere::new(3);
        let elev = VertexMap::from_fn(&geo, |_| e(100.0)); // all land
        let eras = vec![
            era(&geo, 0.0, false, 0.0, 0.0),  // warm: no ice
            era(&geo, 1.0, true, -50.0, 0.9), // cold: all ice
        ];
        let rec = extract(&geo, &elev, e(0.0), &eras);
        assert!(
            rec.envelope.iter().all(|(_, &b)| b),
            "cold era ices every land vertex"
        );
        assert_eq!(rec.max_ice_fraction, 0.9);
        assert_eq!(
            rec.glacial_maximum_day,
            WorldTime::from_std_days(1.0).expect("finite")
        );
    }

    #[test]
    fn shoreline_is_the_swept_band() {
        let geo = Geosphere::new(3);
        // Elevation ramps with latitude so some vertices fall in the band.
        let elev = VertexMap::from_fn(&geo, |c| e(geo.coord(c).latitude));
        let eras = vec![era(&geo, 0.0, false, -30.0, 0.0)];
        let rec = extract(&geo, &elev, e(0.0), &eras); // band = [-30, 0]
        let any = rec.shoreline.iter().any(|(_, &b)| b);
        assert!(any, "some vertices must lie in the [-30,0] sea band");
    }
}

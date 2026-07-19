//! Salt/fresh water classification (The Freshet, DOM-5 first slice): a pure
//! projection over the drainage substrate the globe already computes. No seed
//! draws, no stored state beyond a recomputed-at-genesis `CellMap` — the
//! MAP-39/The Ground shape.

use hornvale_kernel::{CellId, CellMap, Geosphere, ReferenceElevation};

/// The kind of water (if any) at a cell. `Ocean`/`SaltBasin` are salt;
/// `River` is the only drinkable (fresh) class this slice.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum WaterKind {
    /// Below sea level — the salt ocean.
    Ocean,
    /// A terminal endorheic sink — the evaporative salt lake / playa.
    SaltBasin,
    /// Accumulated fresh runoff (a river; includes freshwater through-flow lakes
    /// and endorheic feeder rivers on their way to a salt sink).
    River,
    /// Land with no significant surface water.
    DryLand,
}

impl WaterKind {
    /// Whether this water is drinkable (fresh). Only `River` is fresh this slice.
    /// type-audit: bare-ok(flag: return)
    pub fn is_fresh(self) -> bool {
        matches!(self, WaterKind::River)
    }
}

/// Upstream drainage accumulation at or above which a cell carries fresh flowing
/// water. Reference: `carve::WATERFALL_MIN_DRAINAGE` (80.0); rivers are more
/// common than waterfalls, so this is lower. Tuned (The Freshet T2): a probe
/// of seed-42's land-cell drainage distribution at the canonical globe level
/// put the median land cell at drainage 2 and the 90th percentile at 11, so
/// 20.0 (the initial guess) classified only the top ~4% of land as fresh
/// water — sparse enough that a 400-point deterministic sweep of the sphere
/// (`locale_water_field_varies_and_includes_fresh_water_on_seed_42`) missed
/// every river. 15.0 keeps rivers the minority landform (~6.7% of seed-42's
/// land) while giving that sweep a comfortable margin of hits.
/// type-audit: bare-ok(count)
pub const RIVER_MIN_DRAINAGE: f64 = 15.0;

/// Classify one cell from the bits the globe already derives. Pure and total;
/// precedence Ocean > SaltBasin > River > DryLand. `is_terminal_sink` is
/// `endorheic && no-downhill` (a local minimum with no outlet).
/// type-audit: bare-ok(ratio: elevation_m), bare-ok(ratio: sea_level_m), bare-ok(count: drainage), bare-ok(flag: endorheic), bare-ok(flag: is_terminal_sink)
pub fn classify(
    elevation_m: f64,
    sea_level_m: f64,
    drainage: f64,
    endorheic: bool,
    is_terminal_sink: bool,
) -> WaterKind {
    if elevation_m.total_cmp(&sea_level_m).is_lt() {
        WaterKind::Ocean
    } else if endorheic && is_terminal_sink {
        WaterKind::SaltBasin
    } else if drainage.total_cmp(&RIVER_MIN_DRAINAGE).is_ge() {
        WaterKind::River
    } else {
        WaterKind::DryLand
    }
}

/// Materialize the per-cell classification (recomputed at genesis, never
/// serialized). `downhill[c] == None` marks a local minimum; a terminal salt
/// sink is an endorheic local minimum.
/// type-audit: bare-ok(count: drainage), bare-ok(flag: endorheic)
pub fn water_field(
    geo: &Geosphere,
    elevation: &CellMap<ReferenceElevation>,
    sea_level: ReferenceElevation,
    drainage: &CellMap<f64>,
    endorheic: &CellMap<bool>,
    downhill: &[Option<CellId>],
) -> CellMap<WaterKind> {
    let sea = sea_level.get();
    CellMap::from_fn(geo, |c| {
        let terminal_sink = *endorheic.get(c) && downhill[c.0 as usize].is_none();
        classify(
            elevation.get(c).get(),
            sea,
            *drainage.get(c),
            *endorheic.get(c),
            terminal_sink,
        )
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn ocean_below_sea_level_is_salt() {
        let k = classify(-50.0, 0.0, 0.0, false, false);
        assert_eq!(k, WaterKind::Ocean);
        assert!(!k.is_fresh());
    }

    #[test]
    fn a_high_drainage_exorheic_cell_is_a_fresh_river() {
        let k = classify(100.0, 0.0, RIVER_MIN_DRAINAGE + 1.0, false, false);
        assert_eq!(k, WaterKind::River);
        assert!(k.is_fresh());
    }

    #[test]
    fn an_endorheic_feeder_river_is_still_fresh() {
        // THE CORRECTNESS KEYSTONE: endorheic (drains to an interior basin) but NOT a
        // terminal sink — a river on its way to a salt lake is fresh (the Jordan).
        let k = classify(100.0, 0.0, RIVER_MIN_DRAINAGE + 1.0, true, false);
        assert_eq!(
            k,
            WaterKind::River,
            "an endorheic feeder river must read fresh"
        );
        assert!(k.is_fresh());
    }

    #[test]
    fn the_terminal_endorheic_sink_is_a_salt_basin() {
        // The evaporative salt lake: endorheic AND a terminal sink (local min, no
        // outlet). High drainage accumulates here, but it is salt, not a fresh river.
        let k = classify(90.0, 0.0, RIVER_MIN_DRAINAGE + 999.0, true, true);
        assert_eq!(
            k,
            WaterKind::SaltBasin,
            "the terminal salt sink must read salt"
        );
        assert!(!k.is_fresh());
    }

    #[test]
    fn low_drainage_exorheic_land_is_dry() {
        let k = classify(100.0, 0.0, RIVER_MIN_DRAINAGE - 1.0, false, false);
        assert_eq!(k, WaterKind::DryLand);
        assert!(!k.is_fresh());
    }

    #[test]
    fn classify_orders_ocean_before_salt_basin_before_river() {
        // Precedence: below sea level is Ocean even if flagged endorheic/sink/high-drainage.
        assert_eq!(classify(-1.0, 0.0, 1e9, true, true), WaterKind::Ocean);
        // A terminal sink at/above sea level with huge drainage is SaltBasin, not River.
        assert_eq!(classify(5.0, 0.0, 1e9, true, true), WaterKind::SaltBasin);
    }

    #[test]
    fn water_field_classifies_a_synthetic_globe_deterministically() {
        // A tiny real Geosphere; plant a below-sea cell and a high-drainage cell,
        // build the field twice, assert equal (determinism) and the two cells' kinds.
        let geo = hornvale_kernel::Geosphere::new(2);
        let sea = hornvale_kernel::ReferenceElevation::new(0.0).unwrap();
        let elevation = hornvale_kernel::CellMap::from_fn(&geo, |c| {
            hornvale_kernel::ReferenceElevation::new(if c.0 == 0 { -100.0 } else { 100.0 }).unwrap()
        });
        let drainage = hornvale_kernel::CellMap::from_fn(&geo, |c| {
            if c.0 == 1 {
                RIVER_MIN_DRAINAGE + 1.0
            } else {
                0.0
            }
        });
        let endorheic = hornvale_kernel::CellMap::from_fn(&geo, |_| false);
        let downhill: Vec<Option<hornvale_kernel::CellId>> =
            (0..geo.cell_count()).map(|_| None).collect();
        let a = water_field(&geo, &elevation, sea, &drainage, &endorheic, &downhill);
        let b = water_field(&geo, &elevation, sea, &drainage, &endorheic, &downhill);
        for c in geo.cells() {
            assert_eq!(a.get(c), b.get(c));
        }
        assert_eq!(*a.get(hornvale_kernel::CellId(0)), WaterKind::Ocean);
        // cell 1 is land + high drainage + not-a-sink (downhill None makes it a sink
        // ONLY if endorheic; endorheic is false here) -> River.
        assert_eq!(*a.get(hornvale_kernel::CellId(1)), WaterKind::River);
    }
}

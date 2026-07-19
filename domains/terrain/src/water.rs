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

/// The hop radius over which river proximity falls to zero. Tuned (The
/// Confluence) so carrying capacity spikes ADJACENT to rivers — settlements
/// condense a short walk from fresh water, not necessarily on it.
/// type-audit: bare-ok(count)
pub const RIVER_REACH: u32 = 3;

/// Per-cell proximity to fresh flowing water, in `[0, 1]`: `1.0` on a
/// `WaterKind::River` cell, decaying linearly to `0.0` at `reach` hops. A
/// deterministic multi-source BFS outward from all River cells (frontier
/// processed in `CellId` order — no RNG, no HashMap). The carrying-capacity
/// freshwater term (The Confluence) rides this instead of the smooth
/// drainage/moisture proxy, so condensation pulls towns near rivers.
/// type-audit: bare-ok(count: reach), bare-ok(ratio: return)
pub fn river_proximity(
    geo: &Geosphere,
    water_kind: &CellMap<WaterKind>,
    reach: u32,
) -> CellMap<f64> {
    // hop distance to nearest River, capped at reach+1 (unreached).
    let unreached = reach + 1;
    let mut dist: Vec<u32> = vec![unreached; geo.cell_count()];
    let mut frontier: std::collections::BTreeSet<CellId> = std::collections::BTreeSet::new();
    for c in geo.cells() {
        if matches!(*water_kind.get(c), WaterKind::River) {
            dist[c.0 as usize] = 0;
            frontier.insert(c);
        }
    }
    let mut d = 0u32;
    while d < reach && !frontier.is_empty() {
        let mut next: std::collections::BTreeSet<CellId> = std::collections::BTreeSet::new();
        for c in &frontier {
            for &n in geo.neighbors(*c) {
                if dist[n.0 as usize] > d + 1 {
                    dist[n.0 as usize] = d + 1;
                    next.insert(n);
                }
            }
        }
        frontier = next;
        d += 1;
    }
    CellMap::from_fn(geo, |c| {
        let h = dist[c.0 as usize];
        if h > reach {
            0.0
        } else {
            1.0 - (h as f64) / (reach as f64 + 1.0)
        }
    })
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

    #[test]
    fn river_proximity_is_one_on_a_river_cell_and_decays_with_hops() {
        // A tiny globe; mark one cell River, rest DryLand; proximity is 1.0 on it,
        // strictly decreasing by hop distance, 0.0 beyond reach.
        let geo = hornvale_kernel::Geosphere::new(3);
        let river = hornvale_kernel::CellId(0);
        let wk = hornvale_kernel::CellMap::from_fn(&geo, |c| {
            if c == river {
                WaterKind::River
            } else {
                WaterKind::DryLand
            }
        });
        let prox = river_proximity(&geo, &wk, RIVER_REACH);
        assert_eq!(*prox.get(river), 1.0, "on a river cell");
        // an immediate neighbour is high but < 1
        let nb = geo.neighbors(river)[0];
        assert!(
            *prox.get(nb) > 0.0 && *prox.get(nb) < 1.0,
            "adjacent is high but < 1"
        );
        assert!(
            *prox.get(nb)
                >= *prox.get(
                    geo.neighbors(nb)
                        .iter()
                        .copied()
                        .find(|n| *n != river)
                        .unwrap()
                ),
            "monotone non-increasing with hops"
        );
    }

    #[test]
    fn river_proximity_is_zero_with_no_rivers() {
        let geo = hornvale_kernel::Geosphere::new(3);
        let wk = hornvale_kernel::CellMap::from_fn(&geo, |_| WaterKind::DryLand);
        let prox = river_proximity(&geo, &wk, RIVER_REACH);
        for c in geo.cells() {
            assert_eq!(*prox.get(c), 0.0);
        }
    }

    #[test]
    fn river_proximity_is_deterministic_and_reload_stable() {
        let geo = hornvale_kernel::Geosphere::new(3);
        let wk = hornvale_kernel::CellMap::from_fn(&geo, |c| {
            if c.0 % 7 == 0 {
                WaterKind::River
            } else {
                WaterKind::DryLand
            }
        });
        let a = river_proximity(&geo, &wk, RIVER_REACH);
        let b = river_proximity(&geo, &wk, RIVER_REACH);
        for c in geo.cells() {
            assert_eq!(a.get(c), b.get(c));
        }
    }

    #[test]
    fn river_proximity_seeds_every_river_not_just_the_first() {
        // MULTI-SOURCE (the T1-review coverage catch): two distinct River cells
        // must BOTH read proximity 1.0. A single-source BFS that seeds only the
        // first river leaves the second at < 1.0 (it is >= 1 hop from the only
        // source), so this distinguishes the correct multi-source BFS from a
        // single-source one — which every other test passes.
        let geo = hornvale_kernel::Geosphere::new(4);
        let r1 = hornvale_kernel::CellId(0);
        let r2 = hornvale_kernel::CellId(geo.cell_count() as u32 / 2);
        assert_ne!(r1, r2);
        let wk = hornvale_kernel::CellMap::from_fn(&geo, |c| {
            if c == r1 || c == r2 {
                WaterKind::River
            } else {
                WaterKind::DryLand
            }
        });
        let prox = river_proximity(&geo, &wk, RIVER_REACH);
        assert_eq!(*prox.get(r1), 1.0);
        assert_eq!(
            *prox.get(r2),
            1.0,
            "every river cell is a BFS source, not just the first"
        );
    }
}

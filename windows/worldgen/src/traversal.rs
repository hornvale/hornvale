//! The terrain traversal-cost field (The Connection Graph, Task 3): a
//! per-vertex integer land-travel cost the natural-land-route pathfinder
//! (`hornvale_topology::route::least_cost`, over `hornvale_kernel::astar`)
//! plans over. Lives at the `windows/worldgen` composition root because it
//! reads both terrain (elevation) and climate (biome) -- a domain crate may
//! not depend on a sibling domain.
//!
//! Purely derived from the already-committed elevation and biome fields: no
//! new seed draw, no wall-clock read. Same inputs always produce the same
//! field.

use hornvale_climate::Biome;
use hornvale_kernel::{Geosphere, ReferenceElevation, VertexMap};

/// The flat-ground cost paid to enter any passable (non-ocean) vertex with
/// zero slope to every neighbor -- the floor every other passable vertex's
/// cost is measured above. Coarse-tuned (not census-calibrated): the graph
/// derivation (Task 4) and its consumers only need the relative ordering
/// (flat cheap, steep dear, ocean impassable), so a precise real-world
/// travel-time unit is deferred until a consumer needs one.
/// type-audit: bare-ok(count)
pub const BASE_COST: u64 = 10;

/// Scales a neighbor elevation gap (meters) into the slope term added to
/// [`BASE_COST`]. At `1.0`, the slope term equals the gap in meters, so an
/// ordinary rolling-hill gap (tens of meters) adds a modest surcharge while
/// a real escarpment or peak (hundreds to thousands of meters) dominates the
/// cost -- steep without being literally impassable (unlike ocean, a road
/// can still cross a mountain, just slowly).
/// type-audit: bare-ok(ratio)
const SLOPE_SCALE: f64 = 1.0;

/// Per-vertex integer land-traversal cost: [`BASE_COST`] plus a slope term (the
/// largest absolute elevation gap from a vertex to any one of its neighbors,
/// scaled by [`SLOPE_SCALE`] and truncated to an integer), rising steeply
/// for peaks and escarpments. Ocean vertices (`biome`'s marine classification,
/// `Biome::is_marine`) are impassable to land travel: `u64::MAX`, entered
/// nowhere and skipped by `hornvale_topology::route::VertexRoute` rather than
/// summed as an ordinary (very expensive) step.
///
/// Reuses the already-committed `elevation` and `biome` fields -- this draws
/// no new seed stream and reads no wall clock, so it is deterministic: the
/// same `geo`/`elevation`/`biome` always yield the same field.
///
/// The gap-over-neighbors maximum uses `f64::total_cmp` for its running
/// comparison (never raw `<`/`f64::max`), so the result is total and
/// unambiguous even if a future elevation source ever produced a NaN.
/// type-audit: bare-ok(count: return)
pub fn traversal_cost(
    geo: &Geosphere,
    elevation: &VertexMap<ReferenceElevation>,
    biome: &VertexMap<Biome>,
) -> VertexMap<u64> {
    VertexMap::from_fn(geo, |vertex| {
        if biome.get(vertex).is_marine() {
            return u64::MAX;
        }

        let here = elevation.get(vertex).get();
        let max_gap = geo.neighbors(vertex).iter().fold(0.0_f64, |acc, &n| {
            let gap = (elevation.get(n).get() - here).abs();
            if gap.total_cmp(&acc).is_gt() {
                gap
            } else {
                acc
            }
        });

        BASE_COST.saturating_add((max_gap * SLOPE_SCALE) as u64)
    })
}

/// Per-vertex land-traversal cost at a given sea level: identical to
/// [`traversal_cost`] except a vertex is ocean (`u64::MAX`) iff its elevation is
/// below `sea_level`, rather than by present biome. This is the era-aware cost
/// The Sundering's moving sea plans over — at a glacial low-stand the exposed
/// shelf drops below `u64::MAX` and becomes a passable land bridge.
/// type-audit: bare-ok(count: return)
pub fn traversal_cost_at(
    geo: &Geosphere,
    elevation: &VertexMap<ReferenceElevation>,
    sea_level: ReferenceElevation,
) -> VertexMap<u64> {
    VertexMap::from_fn(geo, |vertex| {
        let here = elevation.get(vertex).get();
        if here < sea_level.get() {
            return u64::MAX;
        }
        let max_gap = geo.neighbors(vertex).iter().fold(0.0_f64, |acc, &n| {
            let gap = (elevation.get(n).get() - here).abs();
            if gap.total_cmp(&acc).is_gt() {
                gap
            } else {
                acc
            }
        });
        BASE_COST.saturating_add((max_gap * SLOPE_SCALE) as u64)
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_kernel::Vertex;

    fn e(m: f64) -> ReferenceElevation {
        ReferenceElevation::new(m).unwrap()
    }

    #[test]
    fn zero_slope_yields_exactly_the_base_cost() {
        let geo = Geosphere::new(0);
        let elevation = VertexMap::from_fn(&geo, |_| e(100.0));
        let biome = VertexMap::from_fn(&geo, |_| Biome::TemperateGrassland);
        let cost = traversal_cost(&geo, &elevation, &biome);
        for vertex in geo.vertices() {
            assert_eq!(*cost.get(vertex), BASE_COST);
        }
    }

    #[test]
    fn every_marine_biome_is_impassable() {
        let geo = Geosphere::new(0);
        let elevation = VertexMap::from_fn(&geo, |_| e(-200.0));
        for &marine in hornvale_climate::biome::ALL
            .iter()
            .filter(|b| b.is_marine())
        {
            let biome = VertexMap::from_fn(&geo, |_| marine);
            let cost = traversal_cost(&geo, &elevation, &biome);
            for vertex in geo.vertices() {
                assert_eq!(*cost.get(vertex), u64::MAX, "{marine:?} must be impassable");
            }
        }
    }

    #[test]
    fn a_lone_high_vertex_costs_more_than_the_flat_base_cost() {
        // Gap is symmetric across an edge, so a peak's immediate neighbor can
        // register the same gap magnitude (the peak is the tallest thing in
        // ITS neighborhood too) -- this asserts the slope term is genuinely
        // added on the peak itself, rather than comparing it to a neighbor
        // that may tie it. The peak-vs-isolated-flat-vertex comparison is
        // covered properly in `windows/worldgen/tests/traversal.rs`, where
        // the flat region is built far enough from the peak to be unaffected
        // by it.
        let geo = Geosphere::new(0);
        let peak = Vertex(0);
        let elevation = VertexMap::from_fn(&geo, |c| if c == peak { e(1000.0) } else { e(0.0) });
        let biome = VertexMap::from_fn(&geo, |_| Biome::TemperateGrassland);
        let cost = traversal_cost(&geo, &elevation, &biome);
        assert!(*cost.get(peak) > BASE_COST);
    }
}

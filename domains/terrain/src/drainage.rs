//! Coarse hydrology: a deterministic flow-accumulation field over the
//! Geosphere. Each land vertex drains to its lowest neighbor; upstream area
//! accumulates downhill. Vertices whose downhill path never reaches the sea are
//! endorheic (interior basins — the banked salt-basin hook, spec C3 §15).
//! Elevation-derived, no seed draws; declared approximations: single
//! lowest-neighbor flow direction (no splitting), unit-area accumulation
//! (no precipitation weighting), no sub-vertex river geometry or lake filling.

use hornvale_kernel::{Geosphere, ReferenceElevation, Vertex, VertexMap};

/// Downhill target per land vertex: the strictly-lowest neighbor (elevations
/// are strictly ordered by C3's per-vertex epsilon, so there is no tie). A
/// vertex with no lower neighbor is a local minimum (a sink). `None` = ocean
/// or local minimum (no outflow). The single owner of this computation:
/// [`drainage_field`] and the carve's sediment router (`carve::route_sediment`)
/// both consume it rather than re-deriving it. `pub` (widened from
/// `pub(crate)`, Sculpting Task 13): the property batteries reconstruct
/// `globe::generate`'s pre-carve pipeline from outside the crate (mirroring
/// `generate_elevation`/`derive_sea_level`, already `pub` for the same
/// reason) to recover the full `CarveDelta` (mass-balance totals)
/// `TectonicGlobe` does not retain, rather than re-deriving this same
/// single-owner logic a second time in a test.
/// type-audit: bare-ok(count: return)
pub fn downhill_targets(
    geo: &Geosphere,
    elevation: &VertexMap<ReferenceElevation>,
    sea_level: ReferenceElevation,
) -> Vec<Option<Vertex>> {
    let n = geo.vertex_count();
    let is_land = |c: Vertex| *elevation.get(c) >= sea_level;
    let mut downhill: Vec<Option<Vertex>> = vec![None; n];
    for c in geo.vertices() {
        if !is_land(c) {
            continue;
        }
        let here = *elevation.get(c);
        let mut best: Option<Vertex> = None;
        let mut best_e = here;
        for &nb in geo.neighbors(c) {
            let e = *elevation.get(nb);
            if e < best_e {
                best_e = e;
                best = Some(nb);
            }
        }
        downhill[c.0 as usize] = best;
    }
    downhill
}

/// Compute the drainage (flow-accumulation) field and the endorheic mask.
/// Returns `(drainage, endorheic)`: `drainage[c]` counts the land vertices
/// upstream of and including `c` (0 on ocean vertices); `endorheic[c]` is true
/// when `c`'s downhill path ends at an interior minimum, never reaching sea.
/// type-audit: bare-ok(count: return)
pub fn drainage_field(
    geo: &Geosphere,
    elevation: &VertexMap<ReferenceElevation>,
    sea_level: ReferenceElevation,
) -> (VertexMap<f64>, VertexMap<bool>) {
    let n = geo.vertex_count();
    let is_land = |c: Vertex| *elevation.get(c) >= sea_level;

    let downhill = downhill_targets(geo, elevation, sea_level);
    let mut reaches_sea = vec![false; n];

    // A land vertex reaches the sea iff following downhill lands on an ocean
    // vertex. Trace each land vertex's path (bounded by n) once, memoizing.
    // Endorheic = land vertex that does NOT reach the sea.
    // 0 = unknown, 1 = reaches sea, 2 = does not.
    let mut state = vec![0u8; n];
    for start in geo.vertices() {
        if !is_land(start) || state[start.0 as usize] != 0 {
            continue;
        }
        let mut path = Vec::new();
        let mut cur = start;
        let verdict;
        loop {
            if !is_land(cur) {
                verdict = 1; // stepped onto ocean
                break;
            }
            if state[cur.0 as usize] != 0 {
                verdict = state[cur.0 as usize];
                break;
            }
            match downhill[cur.0 as usize] {
                None => {
                    verdict = 2; // interior local minimum: endorheic sink
                    break;
                }
                Some(next) => {
                    path.push(cur);
                    cur = next;
                }
            }
        }
        for c in path {
            state[c.0 as usize] = verdict;
        }
    }
    for c in geo.vertices() {
        if is_land(c) {
            reaches_sea[c.0 as usize] = state[c.0 as usize] == 1;
        }
    }

    // Flow accumulation: process land vertices high → low (strict order, total_cmp
    // tie-break by Vertex), each pushing its running total to its downhill
    // neighbor. Ocean vertices stay 0.
    let mut order: Vec<Vertex> = geo.vertices().filter(|c| is_land(*c)).collect();
    order.sort_by(|a, b| {
        elevation
            .get(*b)
            .total_cmp(*elevation.get(*a))
            .then(a.0.cmp(&b.0))
    });
    let mut acc = vec![0.0f64; n];
    for &c in &order {
        acc[c.0 as usize] += 1.0;
        if let Some(next) = downhill[c.0 as usize] {
            acc[next.0 as usize] += acc[c.0 as usize];
        }
    }

    let drainage = VertexMap::from_fn(geo, |c| if is_land(c) { acc[c.0 as usize] } else { 0.0 });
    let endorheic = VertexMap::from_fn(geo, |c| is_land(c) && !reaches_sea[c.0 as usize]);
    (drainage, endorheic)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::globe::generate;
    use crate::pins::TerrainPins;
    use hornvale_kernel::{Geosphere, Seed};

    #[test]
    fn accumulation_conserves_land_area_and_is_deterministic() {
        let geo = Geosphere::new(4);
        let globe = generate(Seed(42), &geo, &TerrainPins::default())
            .unwrap()
            .globe;
        let (a, ea) = drainage_field(&geo, &globe.elevation, globe.sea_level);
        let (b, _eb) = drainage_field(&geo, &globe.elevation, globe.sea_level);
        assert_eq!(a, b, "drainage must be deterministic");
        // Every land vertex drains at least itself; ocean vertices are zero.
        let mut land = 0usize;
        for c in geo.vertices() {
            if *globe.elevation.get(c) >= globe.sea_level {
                land += 1;
                assert!(*a.get(c) >= 1.0, "land vertex {} drains < 1", c.0);
            } else {
                assert_eq!(*a.get(c), 0.0, "ocean vertex {} has drainage", c.0);
            }
        }
        // A sink (endorheic outlet or coastal outlet) collects; the maximum
        // accumulation cannot exceed the land-vertex count.
        let max = a.iter().map(|(_, v)| *v).fold(0.0, f64::max);
        assert!(max <= land as f64, "accumulation {max} exceeds land {land}");
        // Endorheic vertices, if any, are land.
        for (c, e) in ea.iter() {
            if *e {
                assert!(
                    *globe.elevation.get(c) >= globe.sea_level,
                    "endorheic ocean vertex {}",
                    c.0
                );
            }
        }
    }

    #[test]
    fn drainage_flows_downhill_to_a_wetter_outlet() {
        // On a real globe, the single wettest (max-accumulation) land vertex
        // must sit no higher than the mean land elevation — water pools low.
        let geo = Geosphere::new(4);
        let globe = generate(Seed(7), &geo, &TerrainPins::default())
            .unwrap()
            .globe;
        let (d, _e) = drainage_field(&geo, &globe.elevation, globe.sea_level);
        let land: Vec<Vertex> = geo
            .vertices()
            .filter(|c| *globe.elevation.get(*c) >= globe.sea_level)
            .collect();
        let mean: f64 = land
            .iter()
            .map(|c| globe.elevation.get(*c).get())
            .sum::<f64>()
            / land.len() as f64;
        let outlet = land
            .iter()
            .copied()
            .max_by(|a, b| d.get(*a).total_cmp(d.get(*b)))
            .unwrap();
        assert!(
            globe.elevation.get(outlet).get() <= mean,
            "drainage outlet stands above mean land"
        );
    }
}

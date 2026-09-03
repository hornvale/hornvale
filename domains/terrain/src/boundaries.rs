//! Plate boundaries: classify each cross-plate contact from the relative
//! velocity's component along the great-circle direction between the two
//! vertices, and measure every vertex's graph distance to the nearest boundary
//! vertex of its own plate.

use crate::plates::{Plate, dot, norm, normalize, scale, sub, velocity_at};
use hornvale_kernel::{Geosphere, Vertex, VertexMap};
use std::collections::VecDeque;

/// How two plates meet.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BoundaryKind {
    /// Convergent continent–continent: a collision range.
    ContinentalCollision,
    /// Convergent ocean–continent: a coastal range on the continental side,
    /// a trench on the oceanic side.
    CoastalRange,
    /// Convergent ocean–ocean: an island arc on the overriding side, a
    /// trench on the subducting side.
    IslandArc,
    /// Divergent continent–continent: a rift valley.
    ContinentalRift,
    /// Divergent with ocean on either side: a mid-ocean ridge.
    OceanicRidge,
    /// Near-tangential motion: a transform fault (unrest, little relief).
    Transform,
}

/// A vertex's strongest cross-plate contact.
/// type-audit: bare-ok(ratio: magnitude), bare-ok(index: other_plate)
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct VertexBoundary {
    /// The classified kind of the strongest contact.
    pub kind: BoundaryKind,
    /// Absolute closing (or opening) speed of that contact, model units
    /// (0 to 2: two rate-1.0 plates meeting head-on).
    pub magnitude: f64,
    /// The plate on the other side of the contact.
    pub other_plate: u32,
}

/// Fraction of the relative speed the normal component must exceed to count
/// as convergent/divergent rather than transform.
/// plumb: pending(wave-1)
const TRANSFORM_THRESHOLD: f64 = 0.25;

/// Classify the contact between vertex `a` (on `plate_a`) and its neighbor
/// `b` (on `plate_b`). The relative velocity is evaluated at the midpoint;
/// its component along the great-circle direction from `a` toward `b` is
/// the closing speed (positive = converging). Exactly symmetric: swapping
/// `(a, b)` (and its flags) flips both the direction and the relative
/// velocity, so kind and magnitude are bit-identical from either side.
/// Crust epoch (Task 8): continental character is a per-vertex crust flag,
/// not a plate property, so the caller passes each vertex's own flag
/// (`continental_a` for `a`, `continental_b` for `b`) instead of reading
/// `plate.continental`.
/// type-audit: bare-ok(flag: continental_a), bare-ok(flag: continental_b)
pub fn classify_contact(
    geo: &Geosphere,
    a: Vertex,
    b: Vertex,
    plate_a: &Plate,
    plate_b: &Plate,
    continental_a: bool,
    continental_b: bool,
) -> VertexBoundary {
    let pa = geo.position(a);
    let pb = geo.position(b);
    let mid = normalize([pa[0] + pb[0], pa[1] + pb[1], pa[2] + pb[2]]);
    let relative = sub(velocity_at(plate_a, mid), velocity_at(plate_b, mid));
    let speed = norm(relative);
    let chord = sub(pb, pa);
    let toward = normalize(sub(chord, scale(mid, dot(chord, mid))));
    let closing = dot(relative, toward);
    let other_plate = plate_b.id;
    if speed < 1e-12 || closing.abs() < TRANSFORM_THRESHOLD * speed {
        return VertexBoundary {
            kind: BoundaryKind::Transform,
            magnitude: closing.abs(),
            other_plate,
        };
    }
    let kind = if closing > 0.0 {
        match (continental_a, continental_b) {
            (true, true) => BoundaryKind::ContinentalCollision,
            (false, false) => BoundaryKind::IslandArc,
            _ => BoundaryKind::CoastalRange,
        }
    } else {
        match (continental_a, continental_b) {
            (true, true) => BoundaryKind::ContinentalRift,
            _ => BoundaryKind::OceanicRidge,
        }
    };
    VertexBoundary {
        kind,
        magnitude: closing.abs(),
        other_plate,
    }
}

/// Every vertex's strongest boundary contact: among neighbors on other
/// plates, the contact with the greatest magnitude (the first neighbor in
/// ascending order wins ties, via strict `>`). `None` for plate interiors.
/// `continental` is the per-vertex crust flag (Crust epoch, Task 8): each
/// side of a contact is classified by its own vertex's crust, not its
/// plate's identity.
/// type-audit: bare-ok(index: plate_of), bare-ok(flag: continental)
pub fn boundary_field(
    geo: &Geosphere,
    plate_of: &VertexMap<u32>,
    plates: &[Plate],
    continental: &VertexMap<bool>,
) -> VertexMap<Option<VertexBoundary>> {
    VertexMap::from_fn(geo, |vertex| {
        let my_plate = *plate_of.get(vertex);
        let mut best: Option<VertexBoundary> = None;
        for &neighbor in geo.neighbors(vertex) {
            let other = *plate_of.get(neighbor);
            if other == my_plate {
                continue;
            }
            let contact = classify_contact(
                geo,
                vertex,
                neighbor,
                &plates[my_plate as usize],
                &plates[other as usize],
                *continental.get(vertex),
                *continental.get(neighbor),
            );
            let better = match &best {
                None => true,
                Some(current) => contact.magnitude > current.magnitude,
            };
            if better {
                best = Some(contact);
            }
        }
        best
    })
}

/// Graph distance from every vertex to the nearest boundary vertex **of its own
/// plate**, with that boundary vertex attributed as the source. Multi-source
/// BFS: seeds enqueued in ascending vertex order, neighbors visited in
/// ascending order, propagation never crosses a plate boundary — fully
/// deterministic, O(vertices). `None` only for a vertex no same-plate boundary
/// vertex can reach (a fragmented plate at coarse resolution, or a plate with
/// no boundary at all); callers treat that as "no boundary influence".
///
/// **A boundary vertex always seeds itself**, at `(0, vertex)` — the loop above
/// enqueues every boundary vertex with itself as its own source before the BFS
/// runs. `GeneratedTerrain::edifice_source_at`
/// (`domains/terrain/src/provider.rs`, The Repose) depends on exactly this:
/// it treats a volcanic edifice's source contact as itself always being an
/// edifice vertex (same plate, same `arc_side`, same contact, same gate value
/// as whatever query admitted it), which only holds because a boundary vertex
/// is its own zero-distance source here. If this function ever seeded a
/// boundary vertex some other way, that consumer's `.expect()` would start
/// panicking on real terrain; noted here so the invariant has a note at both
/// ends, not just the consumer's.
/// type-audit: bare-ok(index: plate_of), bare-ok(count: return)
pub fn boundary_distance(
    geo: &Geosphere,
    plate_of: &VertexMap<u32>,
    boundaries: &VertexMap<Option<VertexBoundary>>,
) -> VertexMap<Option<(u32, Vertex)>> {
    let mut result: Vec<Option<(u32, Vertex)>> = vec![None; geo.vertex_count()];
    let mut queue = VecDeque::new();
    for vertex in geo.vertices() {
        if boundaries.get(vertex).is_some() {
            result[vertex.0 as usize] = Some((0, vertex));
            queue.push_back(vertex);
        }
    }
    while let Some(vertex) = queue.pop_front() {
        let (distance, source) = result[vertex.0 as usize].expect("queued vertices are labeled");
        let plate = *plate_of.get(vertex);
        for &neighbor in geo.neighbors(vertex) {
            if *plate_of.get(neighbor) != plate {
                continue;
            }
            if result[neighbor.0 as usize].is_none() {
                result[neighbor.0 as usize] = Some((distance + 1, source));
                queue.push_back(neighbor);
            }
        }
    }
    VertexMap::from_fn(geo, |vertex| result[vertex.0 as usize])
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::plates::{Plate, assign_plates};
    use crate::streams;
    use hornvale_kernel::{Geosphere, Seed, VertexMap};

    /// Two hemisphere plates spinning against each other: convergent where
    /// y < 0, divergent where y > 0, transform near x = ±1. Continental
    /// character lives in the crust flag map now, not the plate.
    fn hemisphere_plates(maturity: f64) -> Vec<Plate> {
        vec![
            Plate {
                id: 0,
                seed_position: [0.0, 0.0, 1.0],
                euler_axis: [1.0, 0.0, 0.0],
                rate: 1.0,
                maturity,
                weight: 1.0,
            },
            Plate {
                id: 1,
                seed_position: [0.0, 0.0, -1.0],
                euler_axis: [-1.0, 0.0, 0.0],
                rate: 1.0,
                maturity,
                weight: 1.0,
            },
        ]
    }

    /// Every vertex continental — mirrors the old `continental: true` plates.
    fn all_continental(geo: &Geosphere) -> VertexMap<bool> {
        VertexMap::from_fn(geo, |_| true)
    }

    #[test]
    fn hemisphere_plates_produce_an_equatorial_boundary_of_every_regime() {
        let geo = Geosphere::new(2);
        let plates = hemisphere_plates(0.5);
        let plate_of = assign_plates(&geo, Seed(1).derive(streams::ROOT), &plates);
        let continental = all_continental(&geo);
        let boundaries = boundary_field(&geo, &plate_of, &plates, &continental);
        assert!(boundaries.iter().any(|(_, c)| c.is_some()));
        for (vertex, contact) in boundaries.iter() {
            if contact.is_some() {
                assert!(
                    geo.position(vertex)[2].abs() < 0.5,
                    "boundary vertex {} far from the equator",
                    vertex.0
                );
            }
        }
        let kinds: Vec<BoundaryKind> = boundaries
            .iter()
            .filter_map(|(_, c)| c.as_ref().map(|c| c.kind))
            .collect();
        assert!(kinds.contains(&BoundaryKind::ContinentalCollision));
        assert!(kinds.contains(&BoundaryKind::ContinentalRift));
    }

    #[test]
    fn classification_agrees_from_both_sides() {
        let geo = Geosphere::new(2);
        let plates = hemisphere_plates(0.5);
        let plate_of = assign_plates(&geo, Seed(1).derive(streams::ROOT), &plates);
        for a in geo.vertices() {
            for &b in geo.neighbors(a) {
                let (pa, pb) = (*plate_of.get(a), *plate_of.get(b));
                if pa == pb {
                    continue;
                }
                let ab = classify_contact(
                    &geo,
                    a,
                    b,
                    &plates[pa as usize],
                    &plates[pb as usize],
                    true,
                    true,
                );
                let ba = classify_contact(
                    &geo,
                    b,
                    a,
                    &plates[pb as usize],
                    &plates[pa as usize],
                    true,
                    true,
                );
                assert_eq!(ab.kind, ba.kind, "asymmetric kind {}-{}", a.0, b.0);
                assert_eq!(ab.magnitude, ba.magnitude, "asymmetric magnitude");
            }
        }
    }

    #[test]
    fn classification_uses_vertex_crust_not_plate_identity() {
        // Same two plates, same geometry: continental flags decide the kind.
        let geo = Geosphere::new(2);
        let plates = hemisphere_plates(0.5);
        let plate_of = assign_plates(&geo, Seed(1).derive(streams::ROOT), &plates);
        // Scan for the first adjacent cross-plate vertex pair whose contact is
        // convergent (both-continental classifies as a collision) — the
        // hemisphere plates also produce divergent and transform contacts,
        // which this test isn't exercising.
        let (a, b) = geo
            .vertices()
            .into_iter()
            .find_map(|vertex| {
                let mine = *plate_of.get(vertex);
                geo.neighbors(vertex).iter().find_map(|&n| {
                    let other = *plate_of.get(n);
                    if other == mine {
                        return None;
                    }
                    let probe = classify_contact(
                        &geo,
                        vertex,
                        n,
                        &plates[mine as usize],
                        &plates[other as usize],
                        true,
                        true,
                    );
                    (probe.kind == BoundaryKind::ContinentalCollision).then_some((vertex, n))
                })
            })
            .expect("hemisphere plates have a convergent cross-plate contact");
        let (pa, pb) = (*plate_of.get(a), *plate_of.get(b));
        let cc = classify_contact(
            &geo,
            a,
            b,
            &plates[pa as usize],
            &plates[pb as usize],
            true,
            true,
        );
        assert_eq!(cc.kind, BoundaryKind::ContinentalCollision);
        let oo = classify_contact(
            &geo,
            a,
            b,
            &plates[pa as usize],
            &plates[pb as usize],
            false,
            false,
        );
        assert_eq!(oo.kind, BoundaryKind::IslandArc);
        let mixed = classify_contact(
            &geo,
            a,
            b,
            &plates[pa as usize],
            &plates[pb as usize],
            true,
            false,
        );
        assert_eq!(mixed.kind, BoundaryKind::CoastalRange);
    }

    #[test]
    fn distances_start_at_zero_attribute_a_source_and_grow_by_at_most_one() {
        let geo = Geosphere::new(2);
        let plates = hemisphere_plates(0.5);
        let plate_of = assign_plates(&geo, Seed(1).derive(streams::ROOT), &plates);
        let continental = all_continental(&geo);
        let boundaries = boundary_field(&geo, &plate_of, &plates, &continental);
        let distances = boundary_distance(&geo, &plate_of, &boundaries);
        for (vertex, entry) in distances.iter() {
            let Some((distance, source)) = entry else {
                panic!("vertex {} unreached on a connected hemisphere", vertex.0);
            };
            assert!(
                boundaries.get(*source).is_some(),
                "source {} is not a boundary vertex",
                source.0
            );
            assert_eq!(
                *plate_of.get(*source),
                *plate_of.get(vertex),
                "source crossed a plate"
            );
            if *distance == 0 {
                assert!(boundaries.get(vertex).is_some());
            }
            for &neighbor in geo.neighbors(vertex) {
                if *plate_of.get(neighbor) != *plate_of.get(vertex) {
                    continue;
                }
                let (nd, _) = (*distances.get(neighbor)).expect("same-plate neighbor reached");
                assert!(
                    nd.abs_diff(*distance) <= 1,
                    "distance jumps between {} and {}",
                    vertex.0,
                    neighbor.0
                );
            }
        }
    }
}

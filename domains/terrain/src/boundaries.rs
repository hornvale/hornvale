//! Plate boundaries: classify each cross-plate contact from the relative
//! velocity's component along the great-circle direction between the two
//! cells, and measure every cell's graph distance to the nearest boundary
//! cell of its own plate.

use crate::plates::{Plate, dot, norm, normalize, scale, sub, velocity_at};
use hornvale_kernel::{CellId, CellMap, Geosphere};
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

/// A cell's strongest cross-plate contact.
/// type-audit: bare-ok(ratio: magnitude), bare-ok(index: other_plate)
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct CellBoundary {
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
const TRANSFORM_THRESHOLD: f64 = 0.25;

/// Classify the contact between cell `a` (on `plate_a`) and its neighbor
/// `b` (on `plate_b`). The relative velocity is evaluated at the midpoint;
/// its component along the great-circle direction from `a` toward `b` is
/// the closing speed (positive = converging). Exactly symmetric: swapping
/// `(a, b)` flips both the direction and the relative velocity, so kind and
/// magnitude are bit-identical from either side.
pub fn classify_contact(
    geo: &Geosphere,
    a: CellId,
    b: CellId,
    plate_a: &Plate,
    plate_b: &Plate,
) -> CellBoundary {
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
        return CellBoundary {
            kind: BoundaryKind::Transform,
            magnitude: closing.abs(),
            other_plate,
        };
    }
    let kind = if closing > 0.0 {
        match (plate_a.continental, plate_b.continental) {
            (true, true) => BoundaryKind::ContinentalCollision,
            (false, false) => BoundaryKind::IslandArc,
            _ => BoundaryKind::CoastalRange,
        }
    } else {
        match (plate_a.continental, plate_b.continental) {
            (true, true) => BoundaryKind::ContinentalRift,
            _ => BoundaryKind::OceanicRidge,
        }
    };
    CellBoundary {
        kind,
        magnitude: closing.abs(),
        other_plate,
    }
}

/// Every cell's strongest boundary contact: among neighbors on other
/// plates, the contact with the greatest magnitude (the first neighbor in
/// ascending order wins ties, via strict `>`). `None` for plate interiors.
/// type-audit: bare-ok(index: plate_of)
pub fn boundary_field(
    geo: &Geosphere,
    plate_of: &CellMap<u32>,
    plates: &[Plate],
) -> CellMap<Option<CellBoundary>> {
    CellMap::from_fn(geo, |cell| {
        let my_plate = *plate_of.get(cell);
        let mut best: Option<CellBoundary> = None;
        for &neighbor in geo.neighbors(cell) {
            let other = *plate_of.get(neighbor);
            if other == my_plate {
                continue;
            }
            let contact = classify_contact(
                geo,
                cell,
                neighbor,
                &plates[my_plate as usize],
                &plates[other as usize],
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

/// Graph distance from every cell to the nearest boundary cell **of its own
/// plate**, with that boundary cell attributed as the source. Multi-source
/// BFS: seeds enqueued in ascending cell order, neighbors visited in
/// ascending order, propagation never crosses a plate boundary — fully
/// deterministic, O(cells). `None` only for a cell no same-plate boundary
/// cell can reach (a fragmented plate at coarse resolution, or a plate with
/// no boundary at all); callers treat that as "no boundary influence".
/// type-audit: bare-ok(index: plate_of), bare-ok(count: return)
pub fn boundary_distance(
    geo: &Geosphere,
    plate_of: &CellMap<u32>,
    boundaries: &CellMap<Option<CellBoundary>>,
) -> CellMap<Option<(u32, CellId)>> {
    let mut result: Vec<Option<(u32, CellId)>> = vec![None; geo.cell_count()];
    let mut queue = VecDeque::new();
    for cell in geo.cells() {
        if boundaries.get(cell).is_some() {
            result[cell.0 as usize] = Some((0, cell));
            queue.push_back(cell);
        }
    }
    while let Some(cell) = queue.pop_front() {
        let (distance, source) = result[cell.0 as usize].expect("queued cells are labeled");
        let plate = *plate_of.get(cell);
        for &neighbor in geo.neighbors(cell) {
            if *plate_of.get(neighbor) != plate {
                continue;
            }
            if result[neighbor.0 as usize].is_none() {
                result[neighbor.0 as usize] = Some((distance + 1, source));
                queue.push_back(neighbor);
            }
        }
    }
    CellMap::from_fn(geo, |cell| result[cell.0 as usize])
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::plates::{Plate, assign_plates};
    use hornvale_kernel::Geosphere;

    /// Two continental hemisphere plates spinning against each other:
    /// convergent where y < 0, divergent where y > 0, transform near x = ±1.
    fn hemisphere_plates(maturity: f64) -> Vec<Plate> {
        vec![
            Plate {
                id: 0,
                seed_position: [0.0, 0.0, 1.0],
                continental: true,
                euler_axis: [1.0, 0.0, 0.0],
                rate: 1.0,
                maturity,
            },
            Plate {
                id: 1,
                seed_position: [0.0, 0.0, -1.0],
                continental: true,
                euler_axis: [-1.0, 0.0, 0.0],
                rate: 1.0,
                maturity,
            },
        ]
    }

    #[test]
    fn hemisphere_plates_produce_an_equatorial_boundary_of_every_regime() {
        let geo = Geosphere::new(2);
        let plates = hemisphere_plates(0.5);
        let plate_of = assign_plates(&geo, &plates);
        let boundaries = boundary_field(&geo, &plate_of, &plates);
        assert!(boundaries.iter().any(|(_, c)| c.is_some()));
        for (cell, contact) in boundaries.iter() {
            if contact.is_some() {
                assert!(
                    geo.position(cell)[2].abs() < 0.5,
                    "boundary cell {} far from the equator",
                    cell.0
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
        let plate_of = assign_plates(&geo, &plates);
        for a in geo.cells() {
            for &b in geo.neighbors(a) {
                let (pa, pb) = (*plate_of.get(a), *plate_of.get(b));
                if pa == pb {
                    continue;
                }
                let ab = classify_contact(&geo, a, b, &plates[pa as usize], &plates[pb as usize]);
                let ba = classify_contact(&geo, b, a, &plates[pb as usize], &plates[pa as usize]);
                assert_eq!(ab.kind, ba.kind, "asymmetric kind {}-{}", a.0, b.0);
                assert_eq!(ab.magnitude, ba.magnitude, "asymmetric magnitude");
            }
        }
    }

    #[test]
    fn distances_start_at_zero_attribute_a_source_and_grow_by_at_most_one() {
        let geo = Geosphere::new(2);
        let plates = hemisphere_plates(0.5);
        let plate_of = assign_plates(&geo, &plates);
        let boundaries = boundary_field(&geo, &plate_of, &plates);
        let distances = boundary_distance(&geo, &plate_of, &boundaries);
        for (cell, entry) in distances.iter() {
            let Some((distance, source)) = entry else {
                panic!("cell {} unreached on a connected hemisphere", cell.0);
            };
            assert!(
                boundaries.get(*source).is_some(),
                "source {} is not a boundary cell",
                source.0
            );
            assert_eq!(
                *plate_of.get(*source),
                *plate_of.get(cell),
                "source crossed a plate"
            );
            if *distance == 0 {
                assert!(boundaries.get(cell).is_some());
            }
            for &neighbor in geo.neighbors(cell) {
                if *plate_of.get(neighbor) != *plate_of.get(cell) {
                    continue;
                }
                let (nd, _) = (*distances.get(neighbor)).expect("same-plate neighbor reached");
                assert!(
                    nd.abs_diff(*distance) <= 1,
                    "distance jumps between {} and {}",
                    cell.0,
                    neighbor.0
                );
            }
        }
    }
}

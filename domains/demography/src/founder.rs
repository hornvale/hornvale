//! The founder floor (MAP-22's allocation layer at K=1), migrated from
//! `settlement::placement`: before threshold culling erases a weak people,
//! reserve each species its single strongest attractor so no people is boxed
//! out to zero. Full cross-species competitive exclusion is MAP-22's own
//! campaign; this is only the floor.

use crate::condense::{Condensation, condense};
use hornvale_kernel::{CellMap, Geosphere};

/// Condense each species' K independently, guaranteeing every species its
/// strongest attractor (the founder floor). Returns `(settlement, tag)` pairs,
/// flagship (largest population) first; ties by ascending cell then tag.
/// type-audit: bare-ok(index: per_species), bare-ok(count: threshold), bare-ok(index: return)
pub fn condense_tagged(
    per_species: &[(u32, CellMap<f64>)],
    geo: &Geosphere,
    threshold: f64,
) -> Vec<(Condensation, u32)> {
    let mut out: Vec<(Condensation, u32)> = Vec::new();
    for (tag, k) in per_species {
        let mut nodes = condense(geo, k, threshold);
        if nodes.is_empty() {
            // Founder floor: the single strongest attractor, threshold ignored.
            if let Some(flagship) = condense(geo, k, 0.0).into_iter().next() {
                nodes.push(flagship);
            }
        }
        for n in nodes {
            out.push((n, *tag));
        }
    }
    out.sort_by(|a, b| {
        b.0.population
            .total_cmp(&a.0.population)
            .then(a.0.cell.0.cmp(&b.0.cell.0))
            .then(a.1.cmp(&b.1))
    });
    out
}

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_kernel::{CellId, Geosphere};

    fn peak_at(geo: &Geosphere, cell: u32) -> CellMap<f64> {
        let peak = geo.position(CellId(cell));
        CellMap::from_fn(geo, |c| {
            let p = geo.position(c);
            (p[0] * peak[0] + p[1] * peak[1] + p[2] * peak[2]).max(0.0)
        })
    }

    #[test]
    fn every_tag_keeps_its_strongest_attractor_above_threshold() {
        let geo = Geosphere::new(3);
        // Two species peaking at different cells; a threshold above their
        // catchment totals would drop both without the founder floor.
        let per = vec![(0u32, peak_at(&geo, 0)), (1u32, peak_at(&geo, 40))];
        let placed = condense_tagged(&per, &geo, f64::INFINITY);
        let tags: std::collections::BTreeSet<u32> = placed.iter().map(|(_, t)| *t).collect();
        assert_eq!(
            tags,
            std::collections::BTreeSet::from([0, 1]),
            "every tag founds one settlement"
        );
    }
}

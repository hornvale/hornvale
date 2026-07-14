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
            // Its catchment may fall (far) below a whole person; flooring at 1
            // here — rather than at the emit boundary — keeps "no
            // zero-population settlement" an invariant of the domain itself,
            // not a worldgen-side patch (design spec §5, "no peopleless
            // settlements"). This is the one deliberate, documented exception
            // to conservation (`Σ pop == Σ K`): the founder floor already
            // bypasses the threshold, so it was never conserved-exact.
            if let Some(mut flagship) = condense(geo, k, 0.0).into_iter().next() {
                flagship.population = flagship.population.max(1.0);
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
    use hornvale_kernel::{CellId, CellMap, Geosphere};

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

    #[test]
    fn founder_floor_never_places_a_zero_population_settlement() {
        let geo = Geosphere::new(3);
        // A species with only a trace of carrying capacity at a single cell:
        // its lone attractor's catchment accumulation is far below one
        // person, and every other cell is exactly zero. Without the floor,
        // `.round() as u32` at the emit boundary would commit a
        // population-0 settlement (the wrinkle this test guards).
        let trace_cell = CellId(5);
        let k = CellMap::from_fn(&geo, |c| if c == trace_cell { 1e-6 } else { 0.0 });
        let per = vec![(0u32, k)];
        // A threshold far above the trace K forces the founder-floor path.
        let placed = condense_tagged(&per, &geo, 1.0);
        assert_eq!(
            placed.len(),
            1,
            "the founder floor still places one settlement"
        );
        assert!(
            placed[0].0.population >= 1.0,
            "founder-floor population must never round to zero: {}",
            placed[0].0.population
        );
    }
}

//! Condensation: read discrete settlements off the flow field. An attractor
//! (a flow sink) whose catchment population clears the concentration threshold
//! becomes a settlement; its population is the field integrated over its
//! catchment, conserved against total K by construction.

use crate::flow::flow;
use hornvale_kernel::{CellId, CellMap, Geosphere};

/// A condensed settlement: where it sits and how many people its catchment
/// supports. Population is a readout of the field, never a draw.
/// type-audit: pending(wave-3: position), bare-ok(count: population)
#[derive(Debug, Clone, PartialEq)]
pub struct Condensation {
    /// The attractor cell the settlement sits on.
    pub cell: CellId,
    /// Unit-sphere position.
    pub position: [f64; 3],
    /// Catchment population (= flow accumulation at the attractor).
    pub population: f64,
}

/// Condense settlements from `k`: attractors whose accumulation `>= threshold`,
/// flagship (largest) first. See module docs.
/// type-audit: bare-ok(count: k), bare-ok(count: threshold)
pub fn condense(geo: &Geosphere, k: &CellMap<f64>, threshold: f64) -> Vec<Condensation> {
    let f = flow(geo, k);
    let mut nodes: Vec<Condensation> = geo
        .cells()
        .filter(|c| f.attractor.get(*c).is_some_and(|a| a == *c))
        .map(|c| (c, *f.accumulation.get(c)))
        .filter(|(_, pop)| *pop >= threshold)
        .map(|(cell, population)| Condensation {
            cell,
            position: geo.position(cell),
            population,
        })
        .collect();
    nodes.sort_by(|a, b| {
        b.population
            .total_cmp(&a.population)
            .then(a.cell.0.cmp(&b.cell.0))
    });
    nodes
}

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_kernel::Geosphere;

    fn bump_k(geo: &Geosphere) -> CellMap<f64> {
        let peak = geo.position(CellId(0));
        CellMap::from_fn(geo, |c| {
            let p = geo.position(c);
            (p[0] * peak[0] + p[1] * peak[1] + p[2] * peak[2]).max(0.0)
        })
    }

    #[test]
    fn condensation_conserves_population_against_total_k() {
        let geo = Geosphere::new(3);
        let k = bump_k(&geo);
        let nodes = condense(&geo, &k, 0.0); // threshold 0 → every attractor kept
        let total_k: f64 = geo.cells().map(|c| *k.get(c)).sum();
        let total_pop: f64 = nodes.iter().map(|n| n.population).sum();
        assert!(
            (total_k - total_pop).abs() < 1e-9,
            "Σ pop == Σ K: {total_pop} vs {total_k}"
        );
    }

    #[test]
    fn raising_the_threshold_reduces_the_node_count() {
        let geo = Geosphere::new(3);
        let k = bump_k(&geo);
        let many = condense(&geo, &k, 0.0).len();
        let few = condense(&geo, &k, 5.0).len();
        assert!(
            few <= many,
            "a higher threshold keeps fewer nodes: {few} <= {many}"
        );
    }

    #[test]
    fn nodes_are_sorted_flagship_first() {
        let geo = Geosphere::new(3);
        let nodes = condense(&geo, &bump_k(&geo), 0.0);
        assert!(
            nodes.windows(2).all(|w| w[0].population >= w[1].population),
            "descending population"
        );
    }
}

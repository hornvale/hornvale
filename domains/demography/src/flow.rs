//! Population flow-accumulation over the carrying-capacity field: the terrain
//! `drainage` algorithm with the gradient flipped — people climb the K-gradient
//! as water descends elevation. Each land cell routes to its highest-K
//! neighbour; a cell with no higher neighbour is an attractor. Draws nothing;
//! integer-and-comparison only (K's transcendentals are already spent).

use hornvale_kernel::{CellId, CellMap, Geosphere};

/// The flow field: per-cell accumulated population budget and the attractor
/// (sink) each cell's up-gradient path terminates at.
/// type-audit: bare-ok(count: accumulation)
#[derive(Debug, Clone)]
pub struct Flow {
    /// Accumulated K: `accumulation[c]` = sum of K over all cells whose
    /// up-gradient path passes through `c` (including `c`).
    pub accumulation: CellMap<f64>,
    /// The attractor sink each cell drains to (`None` iff `K(c) == 0`).
    pub attractor: CellMap<Option<CellId>>,
}

/// Compute the flow field. See module docs.
/// type-audit: bare-ok(count: k)
pub fn flow(geo: &Geosphere, k: &CellMap<f64>) -> Flow {
    let n = geo.cell_count();
    // Up-gradient target per cell: strictly-highest-K neighbour, ties to the
    // higher CellId. `None` = zero-K cell or a local maximum (an attractor).
    //
    // The tie-break baseline is the cell's OWN id, not the best neighbour
    // found so far: on an exact K-plateau (e.g. a uniform region), comparing
    // only against the running `best` lets two neighbours with equal K each
    // pick the other (whichever is scanned first "wins" locally), producing
    // a 2-cycle that the memoised path-trace below loops on forever. Seeding
    // `best_id` at `c.0` makes `(K, id)` a strict total order across all
    // cells, so every routed edge strictly increases in that order and no
    // cycle can form.
    let mut up: Vec<Option<CellId>> = vec![None; n];
    for c in geo.cells() {
        if *k.get(c) <= 0.0 {
            continue;
        }
        let here = *k.get(c);
        let mut best: Option<CellId> = None;
        let mut best_k = here;
        let mut best_id = c.0;
        for &nb in geo.neighbors(c) {
            let e = *k.get(nb);
            if e > best_k || (e == best_k && nb.0 > best_id) {
                best_k = e;
                best_id = nb.0;
                best = Some(nb);
            }
        }
        up[c.0 as usize] = best;
    }

    // Terminal attractor per cell: follow `up` to a sink, memoised (bounded n).
    let mut term: Vec<Option<CellId>> = vec![None; n];
    for start in geo.cells() {
        if *k.get(start) <= 0.0 || term[start.0 as usize].is_some() {
            continue;
        }
        let mut path = Vec::new();
        let mut cur = start;
        loop {
            path.push(cur);
            if let Some(t) = term[cur.0 as usize] {
                for p in &path {
                    term[p.0 as usize] = Some(t);
                }
                break;
            }
            match up[cur.0 as usize] {
                None => {
                    // cur is the sink
                    for p in &path {
                        term[p.0 as usize] = Some(cur);
                    }
                    break;
                }
                Some(next) => cur = next,
            }
        }
    }

    // Accumulate: each cell adds its own K to every cell on its up-path.
    let mut acc = vec![0.0f64; n];
    for start in geo.cells() {
        let kv = *k.get(start);
        if kv <= 0.0 {
            continue;
        }
        let mut cur = start;
        loop {
            acc[cur.0 as usize] += kv;
            match up[cur.0 as usize] {
                None => break,
                Some(next) => cur = next,
            }
        }
    }

    let accumulation = CellMap::from_fn(geo, |c| acc[c.0 as usize]);
    let attractor = CellMap::from_fn(geo, |c| term[c.0 as usize]);
    Flow {
        accumulation,
        attractor,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_kernel::{CellId, CellMap, Geosphere};

    #[test]
    fn accumulation_conserves_k_over_attractors() {
        let geo = Geosphere::new(3);
        // A smooth bump: K peaks at cell 0, falls with distance.
        let peak = geo.position(CellId(0));
        let k = CellMap::from_fn(&geo, |c| {
            let p = geo.position(c);
            let dot = p[0] * peak[0] + p[1] * peak[1] + p[2] * peak[2];
            dot.max(0.0) // 0..1, peaks at cell 0
        });
        let f = flow(&geo, &k);
        // Total K equals total accumulation collected at attractor sinks.
        let total_k: f64 = geo.cells().map(|c| *k.get(c)).sum();
        let total_sink: f64 = geo
            .cells()
            .filter(|c| f.attractor.get(*c).is_some_and(|a| a == *c))
            .map(|c| *f.accumulation.get(c))
            .sum();
        assert!(
            (total_k - total_sink).abs() < 1e-9,
            "flow must conserve K: {total_k} vs {total_sink}"
        );
    }

    #[test]
    fn zero_k_cell_has_no_attractor() {
        let geo = Geosphere::new(2);
        let k = CellMap::from_fn(&geo, |c| if c.0 == 0 { 0.0 } else { 1.0 });
        let f = flow(&geo, &k);
        assert!(
            f.attractor.get(CellId(0)).is_none(),
            "a zero-K cell drains nowhere"
        );
    }
}

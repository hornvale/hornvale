//! Derivation of guilds (competitive assemblages) from species' niche vectors.
//!
//! This module computes pairwise competition weights from resource-utilization
//! vectors, which are later consumed by coexistence modeling to derive
//! trophic levels and predation rates.

use hornvale_kernel::ResourceVector;
use std::collections::BTreeMap;

/// Derive pairwise competition weights (`w_ij`) from each species' resource
/// utilization vector via Pianka symmetric niche overlap.
///
/// Returns a complete symmetric matrix: for every ordered pair `(i, j)`
/// (including `i == j` with `w_ii = 1` if the vector is nonzero, `0` if zero),
/// both `(i, j)` and `(j, i)` are present with the same weight.
///
/// # Arguments
/// * `species` - slice of `(species_id, resource_vector)` pairs; ids are
///   iterated in order, so species with duplicate ids are processed but only
///   the last survives in the output (lossily — consider this a validation
///   error in the caller).
///
/// type-audit: bare-ok(index: species), bare-ok(ratio: return)
pub fn guild_overlap(species: &[(u32, ResourceVector)]) -> BTreeMap<(u32, u32), f64> {
    let mut result = BTreeMap::new();

    // Compute overlap between every ordered pair.
    for (id_i, vec_i) in species.iter() {
        for (id_j, vec_j) in species.iter() {
            let weight = vec_i.overlap(vec_j);
            result.insert((*id_i, *id_j), weight);
        }
    }

    result
}

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_kernel::{ANIMAL_PREY, PLANT_FORAGE, ResourceVector};

    #[test]
    fn guild_overlap_is_symmetric_with_unit_diagonal() {
        let sp = vec![
            (0u32, ResourceVector::new(&[(PLANT_FORAGE, 1.0)]).unwrap()),
            (1u32, ResourceVector::new(&[(ANIMAL_PREY, 1.0)]).unwrap()),
        ];
        let w = guild_overlap(&sp);
        assert_eq!(w[&(0, 0)], 1.0);
        assert_eq!(w[&(0, 1)], w[&(1, 0)]);
        assert_eq!(w[&(0, 1)], 0.0, "disjoint niches: no competition");
    }
}

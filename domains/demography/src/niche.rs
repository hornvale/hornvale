//! Derivation of guilds (competitive assemblages) and fractional trophic
//! levels from species' niche vectors.
//!
//! This module computes pairwise competition weights from resource-utilization
//! vectors ([`guild_overlap`]) and the fractional trophic level each species
//! occupies in the food web induced by those same vectors ([`trophic_levels`]).
//! Mass-windowed predation edges (which prey a given predator can actually
//! reach) are a separate, later derivation — this module resolves height in
//! the food web from diet *composition* alone.

use hornvale_kernel::{
    ANIMAL_PREY, DETRITUS, PLANT_FORAGE, ResourceAxis, ResourceVector, v1_basis,
};
use std::collections::{BTreeMap, BTreeSet};

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

/// True if `niche`'s dominant axis (highest weight, ties broken by ascending
/// axis id — [`v1_basis`] order) is [`DETRITUS`]. A detritus-dominant species
/// (a decomposer or scavenger) is off-chain: it is excluded from the
/// [`trophic_levels`] height recursion entirely and pinned at level `1.0` by
/// convention, rather than climbing the food web the way a forager or
/// predator does.
/// type-audit: bare-ok(flag: return)
pub fn is_off_chain(niche: &ResourceVector) -> bool {
    dominant_axis(niche).id == DETRITUS.id
}

/// The axis carrying the highest weight in `niche`. Ties are broken
/// deterministically by ascending axis id ([`v1_basis`] order): only a
/// strictly greater weight displaces the current leader. The zero vector
/// (no recorded niche) resolves to the first basis axis by the same rule.
fn dominant_axis(niche: &ResourceVector) -> ResourceAxis {
    let mut leader = v1_basis()[0];
    let mut leader_weight = niche.weight(leader);
    for axis in &v1_basis()[1..] {
        let weight = niche.weight(*axis);
        if weight.total_cmp(&leader_weight) == std::cmp::Ordering::Greater {
            leader = *axis;
            leader_weight = weight;
        }
    }
    leader
}

/// True if `niche` draws on the biotic axes a heterotroph draws on
/// (`PLANT_FORAGE` and/or `ANIMAL_PREY`) — as opposed to a pure autotroph
/// (`PHOTOSYNTHATE`/`MINERAL` only) or an off-chain detritivore.
fn is_heterotroph(niche: &ResourceVector) -> bool {
    niche.weight(PLANT_FORAGE) > 0.0 || niche.weight(ANIMAL_PREY) > 0.0
}

/// Fractional trophic level per species (`TL_s = 1 + Σ_prey diet_fraction ·
/// TL_prey`), derived from the induced food web implied by each species'
/// niche vector — not from explicit predation edges (that is a separate,
/// mass-windowed derivation).
///
/// Axis resolution:
/// - `PHOTOSYNTHATE`/`MINERAL` (abiotic) are the base of the chain: a
///   supplier level of `0`, so a species eating only these lands at level
///   `1`.
/// - `PLANT_FORAGE` is supplied by producers, which are level `1` by
///   definition — so its contribution always uses the constant supplier
///   level `1`, not a recursively computed value.
/// - `ANIMAL_PREY` is supplied by heterotroph bodies: its supplier level is
///   the mean previous-pass level of the heterotrophs strictly below the
///   eater's own previous-pass level (self-exclusion, so a species never
///   counts its own body as its supply — the "self/cannibal inflation" the
///   spec warns about). When that strict set is empty (most commonly the
///   first pass, where every species is still seeded at the same level),
///   this relaxes to every *other* heterotroph, still self-excluded, so the
///   recursion can bootstrap instead of deadlocking at the seed value.
/// - `DETRITUS` is off-chain: excluded from the height recursion, with the
///   diet renormalized over the remaining axes before weighting. A species
///   whose dominant axis is `DETRITUS` ([`is_off_chain`]) is pinned at level
///   `1.0` by convention rather than participating in the recursion at all.
///
/// The fixpoint is deterministic: every species is seeded at level `1.0`,
/// then all levels are recomputed for `species.len()` passes, each pass
/// reading only the previous pass's levels (Jacobi iteration, never a
/// species' own in-progress value) — enough passes to settle any acyclic
/// food web reachable from a producer-anchored base. A food web with no such
/// anchor (e.g. two species whose entire diet is each other, with no
/// `PLANT_FORAGE`-eating species to break the symmetry) will not reach a
/// true fixed point in bounded passes; the output is still deterministic
/// (same input always yields the same bounded-pass result), just not
/// ecologically converged for that pathological topology.
///
/// type-audit: bare-ok(index: species), bare-ok(ratio: return)
pub fn trophic_levels(species: &[(u32, ResourceVector)]) -> BTreeMap<u32, f64> {
    let heterotrophs: BTreeSet<u32> = species
        .iter()
        .filter(|(_, niche)| is_heterotroph(niche))
        .map(|(id, _)| *id)
        .collect();

    let mut levels: BTreeMap<u32, f64> = species.iter().map(|(id, _)| (*id, 1.0)).collect();

    for _pass in 0..species.len() {
        let prev = levels.clone();
        levels = species
            .iter()
            .map(|(id, niche)| (*id, next_level(*id, niche, species, &prev, &heterotrophs)))
            .collect();
    }

    levels
}

/// One species' next-pass level, given the previous pass's levels for every
/// species (Jacobi update — see [`trophic_levels`]).
fn next_level(
    id: u32,
    niche: &ResourceVector,
    species: &[(u32, ResourceVector)],
    prev: &BTreeMap<u32, f64>,
    heterotrophs: &BTreeSet<u32>,
) -> f64 {
    if is_off_chain(niche) {
        return 1.0;
    }

    // Renormalize the diet over the non-detritus axes (detritus is off-chain
    // and excluded from the height recursion entirely).
    let total_non_detritus: f64 = v1_basis()
        .iter()
        .filter(|axis| axis.id != DETRITUS.id)
        .map(|axis| niche.weight(*axis))
        .sum();
    if total_non_detritus <= 0.0 {
        // No non-detritus diet recorded at all (the zero vector): treat as a
        // base-level producer by convention rather than dividing by zero.
        return 1.0;
    }

    let plant_fraction = niche.weight(PLANT_FORAGE) / total_non_detritus;
    let animal_fraction = niche.weight(ANIMAL_PREY) / total_non_detritus;

    let animal_supplier_level = if animal_fraction > 0.0 {
        animal_prey_supplier_level(id, prev[&id], species, prev, heterotrophs)
    } else {
        0.0
    };

    // Abiotic axes (PHOTOSYNTHATE/MINERAL) supply at level 0, contributing
    // nothing further — the `1.0 +` base already accounts for them.
    1.0 + plant_fraction * 1.0 + animal_fraction * animal_supplier_level
}

/// The supplier level `ANIMAL_PREY` resolves to for `eater_id`: the mean
/// previous-pass level of the heterotrophs strictly below the eater's own
/// previous-pass level, falling back to every other heterotroph
/// (self-excluded) when that strict set is empty. See [`trophic_levels`]'s
/// doc comment for why.
fn animal_prey_supplier_level(
    eater_id: u32,
    eater_prev_level: f64,
    species: &[(u32, ResourceVector)],
    prev: &BTreeMap<u32, f64>,
    heterotrophs: &BTreeSet<u32>,
) -> f64 {
    let other_heterotroph_levels: Vec<f64> = species
        .iter()
        .filter(|(id, _)| *id != eater_id && heterotrophs.contains(id))
        .map(|(id, _)| prev[id])
        .collect();

    let strictly_below: Vec<f64> = other_heterotroph_levels
        .iter()
        .copied()
        .filter(|level| level.total_cmp(&eater_prev_level) == std::cmp::Ordering::Less)
        .collect();

    if !strictly_below.is_empty() {
        return mean(&strictly_below);
    }
    if !other_heterotroph_levels.is_empty() {
        return mean(&other_heterotroph_levels);
    }
    // No other heterotroph anywhere in the pool: nothing to supply the
    // eater's ANIMAL_PREY draw, so treat it as a base-level producer body.
    1.0
}

/// Arithmetic mean of a non-empty slice.
/// type-audit: bare-ok(ratio: return)
fn mean(values: &[f64]) -> f64 {
    values.iter().sum::<f64>() / values.len() as f64
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

    #[test]
    fn trophic_level_places_autotroph_omnivore_and_scavenger() {
        use hornvale_kernel::{ANIMAL_PREY, DETRITUS, PHOTOSYNTHATE, PLANT_FORAGE, ResourceVector};
        let sp = vec![
            (0u32, ResourceVector::new(&[(PHOTOSYNTHATE, 1.0)]).unwrap()), // autotroph
            (1u32, ResourceVector::new(&[(PLANT_FORAGE, 1.0)]).unwrap()),  // herbivore
            (
                2u32,
                ResourceVector::new(&[(PLANT_FORAGE, 0.5), (ANIMAL_PREY, 0.5)]).unwrap(),
            ), // omnivore
            (3u32, ResourceVector::new(&[(ANIMAL_PREY, 1.0)]).unwrap()),   // predator
            (4u32, ResourceVector::new(&[(DETRITUS, 1.0)]).unwrap()),      // scavenger
        ];
        let lv = trophic_levels(&sp);
        assert!((lv[&0] - 1.0).abs() < 1e-9, "autotroph is level 1");
        assert!((lv[&1] - 2.0).abs() < 1e-9, "herbivore is level 2");
        assert!(lv[&2] > 2.0 && lv[&2] < 3.0, "omnivore between integers");
        assert!(lv[&3] > lv[&1], "predator above herbivore");
        assert!(is_off_chain(&sp[4].1), "scavenger is off-chain");
    }
}

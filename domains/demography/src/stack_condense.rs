//! Settlements condensed from the whole-stack density field: tier (total
//! biomass) and composition (species mix) are orthogonal readouts of the same
//! attractor, with each present species' headcount rendered relative to its
//! own body mass rather than a shared population unit. Conceptually replaces
//! [`crate::founder::condense_tagged`]'s per-species condensation with a
//! single mass-weighted pass over [`crate::coexist::CoexistStack`] — the
//! caller wiring (`windows/worldgen`) migrates in a later task.

use crate::condense::{Condensation, condense};
use hornvale_kernel::{CellId, CellMap, Geosphere, Mass};
use std::collections::{BTreeMap, BTreeSet};

/// A present species' headcount, rendered relative to its own body mass
/// rather than a shared population unit — "500 goblins" and "a lone dragon"
/// are both legible outputs of the same field.
/// type-audit: bare-ok(count: Count.0), bare-ok(count: Colony.0)
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum HeadcountRender {
    /// A whole-number headcount of individuals (`>= 1` body).
    Count(u32),
    /// A sub-one-body presence: the species' apportioned biomass in this
    /// settlement is less than one individual's mass. Rendered as flavor
    /// ("a lone dragon lairs here"), never as `Count(0)`.
    Lone,
    /// Reserved for a future colonial/hive extent (e.g. myconid mycelium, an
    /// insect-hive biomass) once a species carries that flag on its
    /// definition. **Not yet fired**: no Stage-A species triggers this
    /// variant, and this module invents no heuristic to fire it early.
    Colony(f64),
}

/// One settlement condensed from the stack: where it sits, its total
/// biomass (tier), its species composition, and each present species'
/// rendered headcount.
/// type-audit: pending(wave-3: position), bare-ok(count: mass_total), bare-ok(index: composition), bare-ok(ratio: composition), bare-ok(index: dominant), bare-ok(index: rendered)
#[derive(Debug, Clone, PartialEq)]
pub struct StackSettlement {
    /// The attractor cell the settlement sits on.
    pub cell: CellId,
    /// Unit-sphere position.
    pub position: [f64; 3],
    /// Total biomass (kilograms) accumulated in the catchment — the tier
    /// axis, independent of which species make it up.
    pub mass_total: f64,
    /// `(species_id, density fraction)` at the attractor cell, sorted by
    /// fraction descending then id ascending — the composition axis,
    /// dominant species first.
    pub composition: Vec<(u32, f64)>,
    /// The top species id by density fraction (`composition[0].0`).
    pub dominant: u32,
    /// `(species_id, rendered headcount)` for every present species, sorted
    /// by species id ascending.
    pub rendered: Vec<(u32, HeadcountRender)>,
}

/// A present species' local density fraction and biomass-share fraction at
/// one attractor cell, plus its own body mass — the inputs [`build_settlement`]
/// turns into `composition` and `rendered`.
struct PresentSpecies {
    id: u32,
    density: f64,
    mass_kg: f64,
}

/// Every species with strictly positive density at `cell`, sorted by id.
fn present_at(
    cell: CellId,
    stack: &crate::coexist::CoexistStack,
    mass_of: &BTreeMap<u32, Mass>,
) -> Vec<PresentSpecies> {
    let mut present: Vec<PresentSpecies> = stack
        .density
        .iter()
        .filter_map(|(id, d)| {
            let density = *d.get(cell);
            if density > 0.0 {
                let mass_kg = mass_of.get(id).map(|m| m.kilograms()).unwrap_or(0.0);
                Some(PresentSpecies {
                    id: *id,
                    density,
                    mass_kg,
                })
            } else {
                None
            }
        })
        .collect();
    present.sort_by_key(|p| p.id);
    present
}

/// Build one [`StackSettlement`] at `cell` given the catchment's total
/// biomass (`mass_total`, already condensed elsewhere): read the local
/// per-species density vector, turn it into a composition (density-fraction)
/// and a rendered headcount (biomass-fraction apportioned against
/// `mass_total`, then divided by each species' own body mass).
fn build_settlement(
    cell: CellId,
    position: [f64; 3],
    mass_total: f64,
    stack: &crate::coexist::CoexistStack,
    mass_of: &BTreeMap<u32, Mass>,
) -> StackSettlement {
    let present = present_at(cell, stack, mass_of);

    // composition: density-fraction shape, independent of mass.
    let density_sum: f64 = present.iter().map(|p| p.density).sum();
    let mut composition: Vec<(u32, f64)> = present
        .iter()
        .map(|p| {
            let frac = if density_sum > 0.0 {
                p.density / density_sum
            } else {
                0.0
            };
            (p.id, frac)
        })
        .collect();
    composition.sort_by(|a, b| b.1.total_cmp(&a.1).then(a.0.cmp(&b.0)));
    let dominant = composition.first().map(|(id, _)| *id).unwrap_or(0);

    // rendered: biomass-fraction shape (density weighted by mass), then
    // apportioned against the catchment's total biomass and divided back by
    // each species' own mass to get a headcount in natural units.
    let biomass_sum: f64 = present.iter().map(|p| p.density * p.mass_kg).sum();
    let mut rendered: Vec<(u32, HeadcountRender)> = present
        .iter()
        .map(|p| {
            let biomass_frac = if biomass_sum > 0.0 {
                (p.density * p.mass_kg) / biomass_sum
            } else {
                0.0
            };
            let species_biomass = biomass_frac * mass_total;
            let head = if p.mass_kg > 0.0 {
                species_biomass / p.mass_kg
            } else {
                0.0
            };
            let render = if head < 1.0 {
                HeadcountRender::Lone
            } else {
                HeadcountRender::Count(head.round() as u32)
            };
            (p.id, render)
        })
        .collect();
    rendered.sort_by_key(|(id, _)| *id);

    StackSettlement {
        cell,
        position,
        mass_total,
        composition,
        dominant,
        rendered,
    }
}

/// Condense settlements from the whole-stack density field: mass-weight
/// every species' density into one field, condense that field's flow
/// attractors ([`crate::condense::condense`]) into tiered settlements, read
/// each attractor's local composition/rendering off the stack, then apply
/// the founder floor (mirroring [`crate::founder::condense_tagged`]) so no
/// species is boxed out to zero representation.
///
/// 1. **Mass field.** `mass_field(cell) = Σ_s density_s(cell) * mass_of[s]`
///    (kilograms) — the tier axis's raw material.
/// 2. **Condense.** `condense(geo, &mass_field, threshold)` reads discrete
///    settlement attractors off that field; each [`Condensation`]'s
///    `population` becomes the settlement's `mass_total`.
/// 3. **Composition + rendering.** Per attractor, [`build_settlement`] reads
///    the local per-species density vector at that one cell (not the
///    catchment) into `composition` (density-fraction shape) and `rendered`
///    (biomass-fraction shape, apportioned against `mass_total`).
/// 4. **Founder floor.** Any species absent from every settlement's
///    `composition` still founds one settlement, at the flagship attractor
///    of its own isolated mass field (`density_s(cell) * mass_of[s]`,
///    condensed with threshold `0.0` — the given `threshold` is deliberately
///    bypassed here, exactly as [`crate::founder::condense_tagged`] bypasses
///    it for its own floor). A species whose density is `0.0` everywhere has
///    no attractor to found and is skipped — there is nothing to represent.
/// 5. **Sort.** Flagship first: `mass_total` descending (`total_cmp`), then
///    ascending cell id.
///
/// Draws nothing from the seed; deterministic (`BTreeMap`/`Vec`/`CellMap`,
/// `total_cmp` throughout, no `HashMap`).
///
/// type-audit: bare-ok(index: mass_of), bare-ok(count: threshold)
pub fn condense_stack(
    geo: &Geosphere,
    stack: &crate::coexist::CoexistStack,
    mass_of: &BTreeMap<u32, Mass>,
    threshold: f64,
) -> Vec<StackSettlement> {
    let mass_field: CellMap<f64> = CellMap::from_fn(geo, |c| {
        stack
            .density
            .iter()
            .map(|(id, d)| {
                let mass_kg = mass_of.get(id).map(|m| m.kilograms()).unwrap_or(0.0);
                d.get(c) * mass_kg
            })
            .sum()
    });

    let nodes: Vec<Condensation> = condense(geo, &mass_field, threshold);
    let mut settlements: Vec<StackSettlement> = nodes
        .iter()
        .map(|n| build_settlement(n.cell, n.position, n.population, stack, mass_of))
        .collect();

    // Founder floor: any species that appears in NO settlement's composition
    // still founds one, at the flagship attractor of its own isolated mass
    // field (threshold ignored, mirroring condense_tagged's floor).
    let covered: BTreeSet<u32> = settlements
        .iter()
        .flat_map(|s| s.composition.iter().map(|(id, _)| *id))
        .collect();

    for (id, density) in &stack.density {
        if covered.contains(id) {
            continue;
        }
        let mass_kg = mass_of.get(id).map(|m| m.kilograms()).unwrap_or(0.0);
        let species_mass_field = CellMap::from_fn(geo, |c| density.get(c) * mass_kg);
        if let Some(flagship) = condense(geo, &species_mass_field, 0.0).into_iter().next() {
            settlements.push(build_settlement(
                flagship.cell,
                flagship.position,
                flagship.population,
                stack,
                mass_of,
            ));
        }
    }

    settlements.sort_by(|a, b| {
        b.mass_total
            .total_cmp(&a.mass_total)
            .then(a.cell.0.cmp(&b.cell.0))
    });

    settlements
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::coexist::CoexistStack;
    use hornvale_kernel::CellId;

    /// Mirrors `founder.rs`'s `peak_at` helper: a dot-product bump peaking at
    /// `cell`, scaled by `scale`, clamped to `0.0` on the far hemisphere.
    fn peak_at(geo: &Geosphere, cell: u32, scale: f64) -> CellMap<f64> {
        let peak = geo.position(CellId(cell));
        CellMap::from_fn(geo, |c| {
            let p = geo.position(c);
            (p[0] * peak[0] + p[1] * peak[1] + p[2] * peak[2]).max(0.0) * scale
        })
    }

    fn zero_pressure(geo: &Geosphere) -> CellMap<f64> {
        CellMap::from_fn(geo, |_| 0.0)
    }

    #[test]
    fn no_524_dragons_and_sub_one_share_renders_lone() {
        let geo = Geosphere::new(3);
        // One dragon-mass species (4000 kg) with a single low-density bump:
        // 0.0005 individuals/cell at its only present cell means the whole
        // catchment's biomass is 0.0005 * 4000 = 2.0 kg -- far under one
        // 4000 kg body, so the render must be `Lone`, never a `Count`.
        let density = CellMap::from_fn(&geo, |c| if c == CellId(0) { 0.0005 } else { 0.0 });
        let stack = CoexistStack {
            density: vec![(0u32, density)],
            emigration_pressure: zero_pressure(&geo),
        };
        let mass_of = BTreeMap::from([(0u32, Mass::new(4000.0).unwrap())]);

        let settlements = condense_stack(&geo, &stack, &mass_of, 0.0);
        assert_eq!(
            settlements.len(),
            1,
            "the single bump condenses one settlement"
        );
        let rendered = &settlements[0].rendered;
        assert_eq!(
            rendered,
            &vec![(0u32, HeadcountRender::Lone)],
            "sub-one-body biomass renders Lone, not a Count: {rendered:?}"
        );
    }

    #[test]
    fn every_species_retains_a_founder_settlement() {
        let geo = Geosphere::new(3);
        // Two peaks at different cells; a threshold above every catchment
        // total forces the main condensation to return nothing, so every
        // settlement in the output must come from the founder floor --
        // mirrors founder.rs's own `f64::INFINITY` test for the same
        // invariant, extended to the stack-composition shape.
        let dominant = peak_at(&geo, 0, 1.0);
        let outcompeted = peak_at(&geo, 300, 0.001);
        let stack = CoexistStack {
            density: vec![(0u32, dominant), (1u32, outcompeted)],
            emigration_pressure: zero_pressure(&geo),
        };
        let mass_of = BTreeMap::from([
            (0u32, Mass::new(40.0).unwrap()),
            (1u32, Mass::new(40.0).unwrap()),
        ]);

        let settlements = condense_stack(&geo, &stack, &mass_of, f64::INFINITY);
        let species_ids: BTreeSet<u32> = settlements
            .iter()
            .flat_map(|s| s.composition.iter().map(|(id, _)| *id))
            .collect();
        assert!(
            species_ids.contains(&1),
            "the outcompeted species still founds a settlement: {species_ids:?}"
        );
    }
}

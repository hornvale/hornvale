//! The overlap-weighted `K^β` coexistence share: the packer's master knob.
//!
//! A single cell's carrying capacity `K_s` per species is not the share that
//! species actually gets once its niche-overlapping competitors are counted:
//! [`cell_share`] normalizes each species' `K_s^β` against the
//! overlap-weighted sum of every co-present species' `K_j^β` (an SINR /
//! multinomial-logit form — each species reads its own denominator, not one
//! shared pool). The competition-temperature exponent `β` is the whole
//! model's dial: `β → 0` spreads capacity broadly across every present
//! species regardless of fitness ("oatmeal"); `β → ∞` collapses it onto the
//! single fittest species per guild (winner-take-all monoculture). Realistic
//! coexistence lives at a finite, calibrated `β` between those extremes.

use crate::footprint::home_range;
use crate::niche::{guild_overlap, predation, trophic_levels};
use hornvale_kernel::math::powf;
use hornvale_kernel::{ANIMAL_PREY, CellMap, Geosphere, Mass, ResourceVector, v1_basis};
use std::collections::BTreeMap;

/// The competition temperature β. AUTHORED placeholder (task A14); task A16
/// calibrates and FREEZES this against the per-cell diversity target. Do not
/// treat as final.
/// type-audit: bare-ok(ratio: BETA)
pub const BETA: f64 = 4.0;
/// The viability floor. AUTHORED placeholder (task A14); frozen with β in A16.
/// type-audit: bare-ok(count: FLOOR)
pub const FLOOR: f64 = 1e-6;

/// The single-cell overlap-weighted coexistence share for every species in
/// `present`.
///
/// `present` is `(species_id, K_s)` — each present species' id and its
/// (species-specific) carrying capacity at this one cell. `overlap` is the
/// [`crate::niche::guild_overlap`] matrix: `overlap[&(i, j)]` is the
/// competition weight `w_ij ∈ [0, 1]` species `i` feels from species `j`
/// (`w_ii = 1`, `0.0` for any pair the matrix omits).
///
/// For each species `s` in `present`:
/// ```text
/// share_s = capacity * K_s^β / ( Σ_j w_sj · K_j^β + floor^β )
/// ```
/// where the sum runs over every `j` in `present` (including `j = s`, since
/// `w_ss = 1`), reading `w_sj` from `overlap` (defaulting to `0.0` if the pair
/// is absent). Every `share_s` that comes out below `floor` is then zeroed —
/// a share too thin to represent a real, persisting population is reported as
/// absence, not as ecological noise.
///
/// Every `^β` power (`K_s^β`, `K_j^β`, `floor^β`) routes through
/// [`hornvale_kernel::math::powf`] — never the inherent `f64::powf` — so the
/// result is identical across platforms (determinism is constitutional; see
/// the module doc). Output is a `BTreeMap` keyed by species id, independent
/// of `present`'s input order.
///
/// type-audit: bare-ok(count: capacity), bare-ok(index: present), bare-ok(count: present), bare-ok(ratio: overlap), bare-ok(ratio: beta), bare-ok(count: floor), bare-ok(count: return)
pub fn cell_share(
    capacity: f64,
    present: &[(u32, f64)],
    overlap: &BTreeMap<(u32, u32), f64>,
    beta: f64,
    floor: f64,
) -> BTreeMap<u32, f64> {
    // Sort by species id so the result is a pure function of the (id, K) SET,
    // independent of caller-supplied order (float addition is non-associative;
    // determinism is constitutional).
    let mut present: Vec<(u32, f64)> = present.to_vec();
    present.sort_by_key(|a| a.0);

    let floor_pow = powf(floor, beta);

    // Precompute K_j^β for every present species once; every species' own
    // denominator reads from this same table rather than recomputing powf
    // per (s, j) pair.
    let powered: BTreeMap<u32, f64> = present
        .iter()
        .map(|(id, k)| (*id, powf(*k, beta)))
        .collect();

    let mut result = BTreeMap::new();
    for (id_s, _k_s) in present.iter() {
        let numerator = capacity * powered[id_s];

        let weighted_sum: f64 = present
            .iter()
            .map(|(id_j, _)| {
                let weight = overlap.get(&(*id_s, *id_j)).copied().unwrap_or(0.0);
                weight * powered[id_j]
            })
            .sum();

        let share = numerator / (weighted_sum + floor_pow);
        let share = if share < floor { 0.0 } else { share };
        result.insert(*id_s, share);
    }

    result
}

// AUTHORED prior (task A9, 2026-07-14): a predator's realized density can be
// at most this fraction of the prey biomass that supports it — an order-of-
// magnitude trophic-efficiency stand-in (real ecological transfer ratios run
// roughly 5-20%; this campaign has no calibration data yet, so the constant
// is a deliberately conservative single authored guess, not a fit). Re-pin
// with a provenance-updated comment once a calibration study exists.
const PREY_SUPPORT_COEFF: f64 = 0.2;

// AUTHORED prior (task A9, 2026-07-14): the top-down shadow multiplier never
// drops below this floor, so an apex predator suppresses its prey without
// ever driving it to true zero (extinction is a settlement-scale event this
// cell-local coupling must not cause on its own).
const SHADOW_FLOOR: f64 = 0.1;

/// Bidirectional trophic coupling over one cell's per-species density
/// [`stack`], applied **highest-level-first**: every predator present in
/// `stack` (i.e. it has a [`crate::niche::predation`] entry) is processed in
/// descending [`crate::niche::trophic_levels`] order (ties broken by id) so
/// that when a mid-chain predator is itself preyed upon by something higher,
/// the apex's shadow has already been applied to it before it, in turn,
/// shadows its own prey — the suppression cascades down the chain in a
/// single pass rather than needing a fixpoint.
///
/// For each predator (in that order), two effects apply in sequence:
/// 1. **Bottom-up cap** — sum the densities of its prey species that are
///    present in `stack`, and cap only the **carnivore-derived fraction** of
///    the predator's own density at `PREY_SUPPORT_COEFF * prey_biomass`. Each
///    predator's density `d` splits into `carnivore_part = cf * d` and
///    `forage_part = (1 - cf) * d`, reading `cf` from `carnivore_frac`
///    (defaulting to `1.0` — an obligate carnivore — for any predator id
///    `carnivore_frac` omits); the post-cap density is
///    `forage_part + carnivore_part.min(PREY_SUPPORT_COEFF * prey_biomass)`.
///    An obligate carnivore (`cf == 1.0`) with no present prey
///    (`prey_biomass == 0`) still collapses to `0.0` — the dormant-apex
///    resting state — but an omnivore (`cf < 1.0`) keeps its uncapped
///    `forage_part`: an animal with no eligible prey survives on what it
///    forages, it does not starve.
/// 2. **Top-down shadow** — using the predator's density *after* the cap
///    above, every present prey species' density is multiplied by
///    `(1 - shadow * predator_density).max(SHADOW_FLOOR)`, so an apex
///    suppresses its prey but never zeroes it outright.
///
/// Predators absent from `predation`, or whose predator id is absent from
/// `stack`, are skipped entirely — this function only touches species
/// actually present in the cell.
///
/// type-audit: bare-ok(index: stack), bare-ok(index: predation), bare-ok(count: stack), bare-ok(index: levels), bare-ok(ratio: levels), bare-ok(index: carnivore_frac), bare-ok(ratio: carnivore_frac), bare-ok(ratio: shadow)
pub fn couple_trophic(
    stack: &mut BTreeMap<u32, f64>,
    predation: &BTreeMap<u32, Vec<u32>>,
    levels: &BTreeMap<u32, f64>,
    carnivore_frac: &BTreeMap<u32, f64>,
    shadow: f64,
) {
    // Highest-level-first: sort the species present in `stack` by their
    // trophic level descending (total_cmp for determinism across platforms),
    // id ascending as the tie-break. Species missing from `levels` sort as
    // if level 0.0 (lowest priority) rather than panicking, since `levels`
    // is expected to cover every species in `stack` but a defensive default
    // keeps this function total.
    let mut ordered: Vec<u32> = stack.keys().copied().collect();
    ordered.sort_by(|a, b| {
        let level_a = levels.get(a).copied().unwrap_or(0.0);
        let level_b = levels.get(b).copied().unwrap_or(0.0);
        level_b.total_cmp(&level_a).then(a.cmp(b))
    });

    for predator_id in ordered {
        let Some(prey_ids) = predation.get(&predator_id) else {
            continue;
        };
        if !stack.contains_key(&predator_id) {
            continue;
        }

        // Bottom-up cap: sum the present prey's densities, then clamp only
        // the CARNIVORE-DERIVED fraction of the predator's own density to
        // PREY_SUPPORT_COEFF of that sum. The forage fraction (1 - cf) is
        // never capped by prey biomass — an omnivore with no eligible prey
        // still survives on what it forages.
        let prey_biomass: f64 = prey_ids.iter().filter_map(|id| stack.get(id)).sum();
        let density = stack[&predator_id];
        let cf = carnivore_frac.get(&predator_id).copied().unwrap_or(1.0);
        let carnivore_part = cf * density;
        let forage_part = (1.0 - cf) * density;
        let capped = forage_part + carnivore_part.min(PREY_SUPPORT_COEFF * prey_biomass);
        stack.insert(predator_id, capped);

        // Top-down shadow: using the just-capped predator density, suppress
        // (never zero) every present prey.
        let predator_density = capped;
        let multiplier = (1.0 - shadow * predator_density).max(SHADOW_FLOOR);
        for prey_id in prey_ids {
            if let Some(prey_density) = stack.get_mut(prey_id) {
                *prey_density *= multiplier;
            }
        }
    }
}

/// Per-species carnivore fraction: the share of a species' sustenance that
/// comes from [`ANIMAL_PREY`], `niche.weight(ANIMAL_PREY) / (sum of every
/// `v1_basis` axis weight)`. An obligate carnivore (only `ANIMAL_PREY`
/// weighted) reads `1.0`; a pure forager or autotroph reads `0.0`; an
/// omnivore lands strictly between. The zero vector (no recorded niche, sum
/// `0.0`) reads `0.0` rather than dividing by zero — "no recorded niche"
/// means no animal-prey dependence to cap.
///
/// type-audit: bare-ok(index: species), bare-ok(ratio: return)
fn carnivore_fraction(species: &[(u32, Mass, ResourceVector)]) -> BTreeMap<u32, f64> {
    species
        .iter()
        .map(|(id, _mass, niche)| {
            let total: f64 = v1_basis().iter().map(|axis| niche.weight(*axis)).sum();
            let frac = if total > 0.0 {
                niche.weight(ANIMAL_PREY) / total
            } else {
                0.0
            };
            (*id, frac)
        })
        .collect()
}

/// The soft-capacity overflow → emigration-pressure field: how hard crowding
/// in one cell pushes population toward its neighbours, the field the
/// history campaign reads for displacement.
///
/// Per cell, the raw overshoot is the logistic excess `max(0, demand -
/// capacity)` — crowding beyond what the cell's carrying capacity supports;
/// a cell at or under capacity contributes nothing. That overshoot field is
/// then **spilled to neighbours by reusing the existing flow hydrology**
/// ([`crate::flow::flow`]) rather than a new diffusion kernel: `flow` was
/// built to route a per-cell quantity up-gradient to its locally-highest
/// neighbour and accumulate it there (originally for population climbing the
/// K-gradient toward attractors); applied to the overshoot field instead of
/// K, the same up-gradient walk routes crowding pressure toward the
/// locally-most-crowded cell in its neighbourhood and accumulates it there,
/// which is exactly the "spill toward the pressure peak" semantics
/// emigration pressure needs — a crowded cell's excess concentrates at (and
/// downstream-accumulates through) local overshoot maxima rather than
/// vanishing at the cell it originated in. The field returned is that
/// [`crate::flow::Flow::accumulation`], not the raw overshoot.
///
/// type-audit: bare-ok(count: demand), bare-ok(count: capacity), bare-ok(count: return)
pub fn emigration_pressure(
    geo: &Geosphere,
    demand: &CellMap<f64>,
    capacity: &CellMap<f64>,
) -> CellMap<f64> {
    let overshoot = CellMap::from_fn(geo, |c| (demand.get(c) - capacity.get(c)).max(0.0));
    crate::flow::flow(geo, &overshoot).accumulation
}

// AUTHORED prior (task A11, 2026-07-14): the top-down shadow strength fed to
// [`couple_trophic`] by the whole-stack packer. No calibration study exists
// yet for this campaign, so 0.3 is a deliberately moderate single authored
// guess — strong enough that an apex predator visibly suppresses its prey
// (unlike `couple_trophic`'s own unit tests, which probe 0.1/0.5 as
// endpoints), but well short of `couple_trophic`'s hard-clamped
// `SHADOW_FLOOR` so ordinary prey never collapses from this coupling alone.
// Re-pin with a provenance-updated comment once a calibration study exists.
const SHADOW: f64 = 0.3;

/// The assembled per-cell density stack for every species over a `Geosphere`:
/// the end-to-end product of [`pack`], and the surface downstream consumers
/// (settlement condensation, migration) read.
///
/// type-audit: bare-ok(index: density), bare-ok(count: density), bare-ok(count: emigration_pressure)
#[derive(Clone, Debug, PartialEq)]
pub struct CoexistStack {
    /// Each species' realized per-cell individual density, tagged by species
    /// id, in the same order as the `species` slice `pack` was called with.
    pub density: Vec<(u32, CellMap<f64>)>,
    /// The soft-capacity overflow field (see [`emigration_pressure`]),
    /// summed across every species' realized density against the pooled
    /// per-cell carrying capacity.
    pub emigration_pressure: CellMap<f64>,
}

/// Assemble the whole per-cell coexistence density stack: the integration
/// keystone wiring together every prior task in this campaign.
///
/// `per_species_k` is each species' carrying-capacity field `K_s` (one
/// `CellMap` per species id, e.g. from [`crate::carrying_capacity`]).
/// `species` is `(species_id, body_mass, niche)` for the same species —
/// the shared, kernel-level shape [`crate::niche::guild_overlap`],
/// [`crate::niche::trophic_levels`], and [`crate::niche::predation`] all
/// project from. `beta` and `floor` pass straight through to [`cell_share`].
///
/// Wiring, in order:
/// 1. **Derive once.** `overlap`, `levels`, `predation`, and
///    `carnivore_frac` ([`carnivore_fraction`]) depend only on `species`'
///    niche vectors and masses — never on a cell — so each is computed
///    exactly once up front, not per cell.
/// 2. **Per cell** (visiting `geo.cells()` in ascending `CellId` order):
///    - `present` is `(species_id, K_s(cell))` for every species with a
///      **strictly positive** `K` at this cell, sorted by id — a species
///      absent from a cell (`K == 0`) contributes nothing to that cell's
///      competition and is dropped before `cell_share` ever sees it.
///    - `capacity` is the **sum of `present`'s K values**: the cell's total
///      carrying capacity, competitively repartitioned among whichever
///      species are actually there, rather than a fixed external cap. This
///      is also reused, unchanged, as the same cell's contribution to the
///      `capacity` field step 3 hands `emigration_pressure`, so both
///      pressure and share read one consistent notion of "how much this
///      cell can support."
///    - `cell_share` turns that `(capacity, present, overlap, beta, floor)`
///      into an overlap-weighted share per present species.
///    - Each share is converted from a capacity count to a **per-cell
///      individual density** by dividing by [`home_range`]: a species with
///      a small home range (many individuals fit in one cell) keeps most of
///      its share as density; a species with a large home range (an
///      individual spans many cells) is scaled down to a fractional
///      per-cell density, since a share of "capacity" is not yet a count of
///      bodies actually standing in this one cell.
///    - [`couple_trophic`] then applies the bottom-up cap / top-down shadow
///      over that cell's density map in place, using the once-derived
///      `predation`/`levels`/`carnivore_frac` and the authored [`SHADOW`]
///      constant. The bottom-up cap only clamps each predator's
///      carnivore-derived density fraction, so an omnivore with no eligible
///      prey still survives on its uncapped forage fraction.
/// 3. **Emigration pressure.** `demand` is each cell's summed post-coupling
///    density across every species; `capacity` is each cell's summed
///    present-species `K` (the same quantity computed in step 2, rebuilt as
///    a field). [`emigration_pressure`] turns `(demand, capacity)` into the
///    spilled crowding-pressure field.
///
/// Draws nothing from the seed: `pack` is a pure function of its arguments,
/// so two calls with identical inputs produce byte-identical output (no
/// `HashMap`, `total_cmp`-safe throughout via the functions it calls).
///
/// # Panics
///
/// `species` must contain an entry for every species id present in
/// `per_species_k` — the internal `masses[&id]` lookup (built from
/// `species`) indexes on every id `per_species_k` supplies, and panics if
/// one is missing. Callers that pass matching sets (worldgen builds both
/// from the same species roster) never hit this; a debug build asserts the
/// precondition explicitly so a mismatch fails loudly at the call site
/// rather than surfacing as an opaque panic deep in the per-cell loop.
///
/// type-audit: bare-ok(index: per_species_k), bare-ok(index: species), bare-ok(ratio: beta), bare-ok(count: floor)
pub fn pack(
    geo: &Geosphere,
    per_species_k: &[(u32, CellMap<f64>)],
    species: &[(u32, Mass, ResourceVector)],
    beta: f64,
    floor: f64,
) -> CoexistStack {
    debug_assert!(
        per_species_k
            .iter()
            .all(|(id, _)| species.iter().any(|(sid, _, _)| sid == id)),
        "pack: every per_species_k id must have a matching species entry \
         (the masses[&id] lookup below panics otherwise)"
    );

    // Derive overlap/levels/predation/carnivore_frac exactly once — they
    // depend only on `species`, never on a cell.
    let projected_niche: Vec<(u32, ResourceVector)> = species
        .iter()
        .map(|(id, _mass, niche)| (*id, niche.clone()))
        .collect();
    let overlap = guild_overlap(&projected_niche);
    let levels = trophic_levels(&projected_niche);
    let predation_edges = predation(species);
    let carnivore_frac = carnivore_fraction(species);
    let masses: BTreeMap<u32, Mass> = species.iter().map(|(id, mass, _)| (*id, *mass)).collect();

    // Per-cell density maps and capacities, indexed by CellId in the same
    // ascending order `Geosphere::cells()` and `CellMap::from_fn` both use,
    // so the later per-species `CellMap` rebuild reads them back correctly.
    let mut per_cell_density: Vec<BTreeMap<u32, f64>> = Vec::new();
    let mut per_cell_capacity: Vec<f64> = Vec::new();

    for cell in geo.cells() {
        let mut present: Vec<(u32, f64)> = per_species_k
            .iter()
            .map(|(id, k)| (*id, *k.get(cell)))
            .filter(|(_, k)| *k > 0.0)
            .collect();
        present.sort_by_key(|(id, _)| *id);

        let capacity: f64 = present.iter().map(|(_, k)| *k).sum();
        let shares = cell_share(capacity, &present, &overlap, beta, floor);

        let mut density: BTreeMap<u32, f64> = shares
            .into_iter()
            .map(|(id, share)| {
                let mass = masses[&id];
                (id, share / home_range(mass))
            })
            .collect();
        couple_trophic(
            &mut density,
            &predation_edges,
            &levels,
            &carnivore_frac,
            SHADOW,
        );

        per_cell_density.push(density);
        per_cell_capacity.push(capacity);
    }

    let density: Vec<(u32, CellMap<f64>)> = species
        .iter()
        .map(|(id, _, _)| {
            let map = CellMap::from_fn(geo, |c| {
                per_cell_density[c.0 as usize]
                    .get(id)
                    .copied()
                    .unwrap_or(0.0)
            });
            (*id, map)
        })
        .collect();

    let demand = CellMap::from_fn(geo, |c| per_cell_density[c.0 as usize].values().sum());
    let capacity_field = CellMap::from_fn(geo, |c| per_cell_capacity[c.0 as usize]);
    let spillover = emigration_pressure(geo, &demand, &capacity_field);

    CoexistStack {
        density,
        emigration_pressure: spillover,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_kernel::CellId;

    #[test]
    fn overflow_is_zero_under_capacity_and_positive_when_crowded() {
        let geo = Geosphere::new(3);
        let cap = CellMap::from_fn(&geo, |_| 1.0);
        let under = CellMap::from_fn(&geo, |_| 0.5);
        let over = CellMap::from_fn(&geo, |c| if c.0 == 0 { 5.0 } else { 0.5 });
        let p_under = emigration_pressure(&geo, &under, &cap);
        let p_over = emigration_pressure(&geo, &over, &cap);
        assert!(
            geo.cells().all(|c| *p_under.get(c) == 0.0),
            "no pressure under capacity"
        );
        assert!(*p_over.get(CellId(0)) > 0.0, "crowding makes pressure");
    }

    #[test]
    fn apex_is_capped_by_prey_and_dormant_without_it() {
        let mut with_prey = BTreeMap::from([(0u32, 5.0), (1u32, 0.02)]); // prey 0, apex 1
        let pred = BTreeMap::from([(1u32, vec![0u32])]);
        let lv = BTreeMap::from([(0u32, 2.0), (1u32, 3.0)]);
        let cf = BTreeMap::from([(1u32, 1.0)]); // obligate carnivore
        couple_trophic(&mut with_prey, &pred, &lv, &cf, 0.1);
        assert!(
            with_prey[&1] <= with_prey[&0],
            "apex density ≤ prey-supported cap"
        );

        let mut no_prey = BTreeMap::from([(1u32, 0.02)]); // apex alone
        couple_trophic(&mut no_prey, &pred, &lv, &cf, 0.1);
        assert!(no_prey[&1] < 1e-6, "dormant apex self-limits with no prey");
    }

    #[test]
    fn couple_trophic_cascade_is_processed_apex_first() {
        // A 3-species predator-of-a-predator cascade: prey P (id 0) <- mid-
        // predator M (id 1) <- apex A (id 2). Trophic level is DESCENDING
        // with id (P=2.0 < M=3.0 < A=4.0), so `BTreeMap` id-ascending
        // iteration order (P, M, A) is the exact REVERSE of the required
        // level-descending processing order (A, M, P). The two existing
        // `couple_trophic` tests each use a single predator/prey pair, so
        // they pass identically whether or not the highest-level-first sort
        // is applied — this test exists to catch a regression to
        // `stack.keys()` / id order, which the trace below shows produces a
        // different (lower) result for P.
        let mut stack = BTreeMap::from([(0u32, 5.0), (1u32, 1.0), (2u32, 1.0)]);
        let predation = BTreeMap::from([(1u32, vec![0u32]), (2u32, vec![1u32])]);
        let levels = BTreeMap::from([(0u32, 2.0), (1u32, 3.0), (2u32, 4.0)]);
        let carnivore_frac = BTreeMap::from([(1u32, 1.0), (2u32, 1.0)]); // obligate carnivores
        let shadow = 0.5;

        couple_trophic(&mut stack, &predation, &levels, &carnivore_frac, shadow);

        // Hand-trace, CORRECT apex-first (level-descending) order:
        //
        // 1. A (id 2, level 4.0) processed first. Its only present prey is
        //    M (id 1), still at its starting density 1.0.
        //      capped_A = min(A=1.0, PREY_SUPPORT_COEFF * prey_biomass(M)=0.2*1.0)
        //               = min(1.0, 0.2) = 0.2
        //    Top-down shadow onto M using A's post-cap density:
        //      mult_A = (1.0 - shadow * 0.2).max(SHADOW_FLOOR)
        //             = (1.0 - 0.1).max(0.1) = 0.9
        //      M's density -> 1.0 * 0.9 = 0.9
        //
        // 2. M (id 1, level 3.0) processed second, now at density 0.9. Its
        //    only present prey is P (id 0), still at 5.0 (untouched so far).
        //      capped_M = min(M=0.9, PREY_SUPPORT_COEFF * prey_biomass(P)=0.2*5.0)
        //               = min(0.9, 1.0) = 0.9
        //    Top-down shadow onto P using M's post-cap density:
        //      mult_M = (1.0 - shadow * 0.9).max(SHADOW_FLOOR)
        //             = (1.0 - 0.45).max(0.1) = 0.55
        //      P's density -> 5.0 * 0.55 = 2.75
        //
        // 3. P (id 0) has no `predation` entry, so it is skipped as a
        //    predator.
        //
        // => expected P density under the CORRECT apex-first order: 2.75.
        // Mirrors the implementation's exact arithmetic sequence (same
        // operators, same operand order) against the real constants, rather
        // than a bare hardcoded literal.
        let capped_a = (1.0_f64).min(PREY_SUPPORT_COEFF * 1.0);
        let mult_a = (1.0 - shadow * capped_a).max(SHADOW_FLOOR);
        let m_after_a = 1.0 * mult_a;
        let capped_m = m_after_a.min(PREY_SUPPORT_COEFF * 5.0);
        let mult_m = (1.0 - shadow * capped_m).max(SHADOW_FLOOR);
        let expected_correct_order = 5.0 * mult_m;

        assert!(
            (stack[&0] - expected_correct_order).abs() < 1e-9,
            "expected P density {expected_correct_order} under apex-first order, got {}",
            stack[&0]
        );

        // For contrast (not asserted against `stack`, just demonstrating
        // the mechanism this test guards): what P's density WOULD be under
        // a WRONG id-ascending order, where M (id 1) is processed before A
        // (id 2) since 1 < 2, and P (id 0) is skipped as a non-predator
        // regardless of position.
        //
        // 1. M processed first, still at density 1.0. Its prey P is still
        //    at 5.0.
        //      capped_M = min(1.0, 0.2 * 5.0) = min(1.0, 1.0) = 1.0
        //      mult_M = (1.0 - 0.5 * 1.0).max(0.1) = 0.5
        //      P's density -> 5.0 * 0.5 = 2.5
        // 2. A processed second: it shadows M (not P), so P is unaffected
        //    by this step.
        //
        // => P's density under the WRONG id-ascending order: 2.5, which
        // differs from the correct 2.75 above — the ordering mechanism this
        // test protects.
        let capped_m_wrong = (1.0_f64).min(PREY_SUPPORT_COEFF * 5.0);
        let mult_m_wrong = (1.0 - shadow * capped_m_wrong).max(SHADOW_FLOOR);
        let expected_wrong_order = 5.0 * mult_m_wrong;

        assert!(
            (expected_correct_order - expected_wrong_order).abs() > 1e-6,
            "test is not order-distinguishing: correct ({expected_correct_order}) and \
             id-order ({expected_wrong_order}) traces agree"
        );
    }

    #[test]
    fn apex_shadow_suppresses_but_never_zeroes_prey() {
        let mut s = BTreeMap::from([(0u32, 5.0), (1u32, 0.02)]);
        let base_prey = s[&0];
        couple_trophic(
            &mut s,
            &BTreeMap::from([(1u32, vec![0u32])]),
            &BTreeMap::from([(0u32, 2.0), (1u32, 3.0)]),
            &BTreeMap::from([(1u32, 1.0)]), // obligate carnivore
            0.5,
        );
        assert!(
            s[&0] < base_prey && s[&0] > 0.0,
            "shadow suppresses, not zeroes"
        );
    }

    #[test]
    fn couple_trophic_omnivore_survives_without_prey() {
        // An omnivore (id 1) is a "predator" (has a `predation` entry) but
        // draws only 40% of its sustenance from ANIMAL_PREY; its listed prey
        // (id 0) is absent from `stack`, so prey_biomass is 0. The old
        // whole-density cap would zero it entirely; the fix caps only the
        // carnivore-derived fraction, so the 60% forage fraction survives
        // uncapped.
        let original_density = 2.0;
        let mut stack = BTreeMap::from([(1u32, original_density)]); // prey 0 absent
        let predation = BTreeMap::from([(1u32, vec![0u32])]);
        let levels = BTreeMap::from([(1u32, 2.0)]);
        let carnivore_frac = BTreeMap::from([(1u32, 0.4)]); // omnivore
        couple_trophic(&mut stack, &predation, &levels, &carnivore_frac, 0.1);

        let expected_forage = 0.6 * original_density;
        assert!(
            stack[&1] > 0.0,
            "omnivore with no eligible prey must not be zeroed"
        );
        assert!(
            (stack[&1] - expected_forage).abs() < 1e-9,
            "expected forage-only density {expected_forage}, got {}",
            stack[&1]
        );
    }

    /// Test helper: full same-guild overlap — `w_ij = 1.0` for every ordered
    /// pair drawn from `ids` (including `i == j`).
    fn full_overlap(ids: &[u32]) -> BTreeMap<(u32, u32), f64> {
        let mut result = BTreeMap::new();
        for &i in ids {
            for &j in ids {
                result.insert((i, j), 1.0);
            }
        }
        result
    }

    #[test]
    fn beta_slides_from_oatmeal_to_monoculture() {
        let overlap = full_overlap(&[0, 1]);
        let present = vec![(0u32, 2.0), (1u32, 1.0)]; // species 0 fitter
        let low = cell_share(1.0, &present, &overlap, 0.1, 1e-6); // β→0: near-equal
        let high = cell_share(1.0, &present, &overlap, 20.0, 1e-6); // β→∞: winner-take-all
        let ratio_low = low[&0] / low[&1];
        assert!(ratio_low < 2.5, "low β shares broadly (oatmeal)");
        assert!(high[&1] < 1e-3 && high[&0] > 0.9, "high β → monoculture");
    }

    #[test]
    fn pack_yields_coexistence_not_exclusion_and_is_deterministic() {
        use hornvale_kernel::{Mass, PLANT_FORAGE, ResourceVector};

        let geo = Geosphere::new(3);
        let k0 = CellMap::from_fn(&geo, |_| 1.0);
        let k1 = CellMap::from_fn(&geo, |_| 0.6);
        let per = vec![(0u32, k0), (1u32, k1)];
        let sp = vec![
            (
                0u32,
                Mass::new(40.0).unwrap(),
                ResourceVector::new(&[(PLANT_FORAGE, 1.0)]).unwrap(),
            ),
            (
                1u32,
                Mass::new(30.0).unwrap(),
                ResourceVector::new(&[(PLANT_FORAGE, 1.0)]).unwrap(),
            ),
        ];
        let a = pack(&geo, &per, &sp, 4.0, 1e-6);
        let b = pack(&geo, &per, &sp, 4.0, 1e-6);
        assert_eq!(a.density, b.density, "byte-identical repack");
        let c = geo.cells().next().unwrap();
        let d0 = a.density.iter().find(|(t, _)| *t == 0).unwrap().1.get(c);
        let d1 = a.density.iter().find(|(t, _)| *t == 1).unwrap().1.get(c);
        assert!(
            *d0 > *d1 && *d1 > 0.0,
            "dominant leads but rival persists (coexistence)"
        );
    }

    #[test]
    fn more_same_guild_competitors_thin_everyone() {
        let two = cell_share(
            1.0,
            &[(0, 1.0), (1, 1.0)],
            &full_overlap(&[0, 1]),
            4.0,
            1e-9,
        );
        let three = cell_share(
            1.0,
            &[(0, 1.0), (1, 1.0), (2, 1.0)],
            &full_overlap(&[0, 1, 2]),
            4.0,
            1e-9,
        );
        assert!(three[&0] < two[&0], "a third competitor thins the first");
    }
}

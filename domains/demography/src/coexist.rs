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

use hornvale_kernel::math::powf;
use std::collections::BTreeMap;

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
///    present in `stack`, and cap the predator's own density at
///    `PREY_SUPPORT_COEFF * prey_biomass`. A predator with no present prey
///    (`prey_biomass == 0`) collapses to `0.0` — the dormant-apex resting
///    state, not a special case to avoid.
/// 2. **Top-down shadow** — using the predator's density *after* the cap
///    above, every present prey species' density is multiplied by
///    `(1 - shadow * predator_density).max(SHADOW_FLOOR)`, so an apex
///    suppresses its prey but never zeroes it outright.
///
/// Predators absent from `predation`, or whose predator id is absent from
/// `stack`, are skipped entirely — this function only touches species
/// actually present in the cell.
///
/// type-audit: bare-ok(index: stack), bare-ok(index: predation), bare-ok(count: stack), bare-ok(index: levels), bare-ok(ratio: levels), bare-ok(ratio: shadow)
pub fn couple_trophic(
    stack: &mut BTreeMap<u32, f64>,
    predation: &BTreeMap<u32, Vec<u32>>,
    levels: &BTreeMap<u32, f64>,
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

        // Bottom-up cap: sum the present prey's densities, then clamp the
        // predator's own density to PREY_SUPPORT_COEFF of that sum. No prey
        // present sums to 0.0, so the predator collapses to 0.0 — dormant.
        let prey_biomass: f64 = prey_ids.iter().filter_map(|id| stack.get(id)).sum();
        let capped = stack[&predator_id].min(PREY_SUPPORT_COEFF * prey_biomass);
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn apex_is_capped_by_prey_and_dormant_without_it() {
        let mut with_prey = BTreeMap::from([(0u32, 5.0), (1u32, 0.02)]); // prey 0, apex 1
        let pred = BTreeMap::from([(1u32, vec![0u32])]);
        let lv = BTreeMap::from([(0u32, 2.0), (1u32, 3.0)]);
        couple_trophic(&mut with_prey, &pred, &lv, 0.1);
        assert!(
            with_prey[&1] <= with_prey[&0],
            "apex density ≤ prey-supported cap"
        );

        let mut no_prey = BTreeMap::from([(1u32, 0.02)]); // apex alone
        couple_trophic(&mut no_prey, &pred, &lv, 0.1);
        assert!(no_prey[&1] < 1e-6, "dormant apex self-limits with no prey");
    }

    #[test]
    fn apex_shadow_suppresses_but_never_zeroes_prey() {
        let mut s = BTreeMap::from([(0u32, 5.0), (1u32, 0.02)]);
        let base_prey = s[&0];
        couple_trophic(
            &mut s,
            &BTreeMap::from([(1u32, vec![0u32])]),
            &BTreeMap::from([(0u32, 2.0), (1u32, 3.0)]),
            0.5,
        );
        assert!(
            s[&0] < base_prey && s[&0] > 0.0,
            "shadow suppresses, not zeroes"
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

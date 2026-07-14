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
        let shadow = 0.5;

        couple_trophic(&mut stack, &predation, &levels, shadow);

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

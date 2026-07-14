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

#[cfg(test)]
mod tests {
    use super::*;

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

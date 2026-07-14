//! The three first-class derived byproducts of the coexistence stack:
//! strife (a balanced-fitness contest measure), wilderness (the unclaimed
//! capacity fraction), and refugia (per-species hostility-shadow strongholds).
//! None of these feed back into `pack`/`cell_share` — they are pure reads off
//! an already-assembled [`crate::coexist::CoexistStack`] and the per-species
//! `K` fields that built it, kept here as a separate module so the packer
//! itself stays free of presentation-layer concerns.

use crate::coexist::CoexistStack;
use hornvale_kernel::{CellId, CellMap, Geosphere};

/// The three derived byproducts of a [`CoexistStack`], all read off the same
/// per-cell density stack and per-species `K` fields.
/// type-audit: bare-ok(count: strife), bare-ok(ratio: wilderness), bare-ok(index: refugia)
pub struct Byproducts {
    /// Contest intensity per cell — the inverse-Herfindahl evenness of the
    /// cell's density vector across present species. `0.0` where no species
    /// is present; approaches the count of present species where they are
    /// evenly matched; approaches `1.0` where one species dominates. See
    /// [`strife`] for the exact formula.
    pub strife: CellMap<f64>,
    /// Unclaimed capacity fraction per cell, in `[0, 1]`: how much of the
    /// cell's summed carrying capacity no species' realized density has
    /// actually claimed — the untamed interstitial marches. See
    /// [`wilderness`] for the exact formula.
    pub wilderness: CellMap<f64>,
    /// Per non-dominant species, its stronghold cells: cells where the
    /// world's normally-dominant species is hostility-excluded (`K_D <
    /// floor`) but this species still persists (`K_s >= floor`). Tagged by
    /// species id; the dominant species itself is omitted (its own refugia
    /// map would be trivially empty — see [`refugia`]'s doc for why).
    pub refugia: Vec<(u32, CellMap<f64>)>,
}

/// Contest intensity at one cell: the inverse Herfindahl index (inverse
/// Simpson index) of `density`'s per-species fractional shares at `cell`.
///
/// `frac_s = density_s(cell) / Σ_j density_j(cell)` for every species `j` in
/// `density` (species absent from the cell contribute `density_s(cell) ==
/// 0.0`, which drops out of the sum on its own — no filtering needed).
/// `strife = 1.0 / Σ_s frac_s^2`, which is `1.0` when a single species holds
/// the entire cell and rises toward the count of present species as they
/// become evenly matched. A cell with zero total density (no species
/// present) reports `0.0` rather than dividing by zero — there is no contest
/// where nobody is contesting.
///
/// Deliberately tracks **balance**, not **magnitude**: a cell where one
/// species density dwarfs another is low-strife regardless of how large the
/// dominant's density is, and a cell where two species are evenly matched is
/// high-strife even at low absolute density. This is why `strife` does not
/// use `hornvale_kernel::math` — squaring and dividing plain `f64` ratios are
/// exact IEEE-754 operations, not transcendentals, so no platform-libm route
/// is needed for determinism.
///
/// type-audit: bare-ok(index: density), bare-ok(count: return)
fn strife_at(density: &[(u32, CellMap<f64>)], cell: CellId) -> f64 {
    let total: f64 = density.iter().map(|(_, d)| *d.get(cell)).sum();
    if total <= 0.0 {
        return 0.0;
    }
    let herfindahl: f64 = density
        .iter()
        .map(|(_, d)| {
            let frac = *d.get(cell) / total;
            frac * frac
        })
        .sum();
    1.0 / herfindahl
}

/// The full strife field: [`strife_at`] evaluated over every cell of `geo`.
///
/// type-audit: bare-ok(index: density), bare-ok(count: return)
pub fn strife(geo: &Geosphere, density: &[(u32, CellMap<f64>)]) -> CellMap<f64> {
    CellMap::from_fn(geo, |c| strife_at(density, c))
}

/// Unclaimed capacity fraction at one cell.
///
/// `capacity = Σ_j K_j(cell)` over `per_species_k`; `claimed = Σ_s
/// density_s(cell)` over `density`. `wilderness = max(0, capacity -
/// claimed) / capacity` when `capacity > 0`, else `0.0` (a cell nobody can
/// live in has no unclaimed wilderness either — it is simply outside the
/// question). The result is clamped to `[0, 1]`: `claimed` is a realized,
/// post-coexistence density that can in principle sit anywhere relative to
/// the raw summed `K` (coupling and home-range scaling do not guarantee
/// `claimed <= capacity`), so the fraction is defensively bounded rather
/// than assumed.
///
/// type-audit: bare-ok(index: per_species_k), bare-ok(index: density), bare-ok(count: return)
fn wilderness_at(
    per_species_k: &[(u32, CellMap<f64>)],
    density: &[(u32, CellMap<f64>)],
    cell: CellId,
) -> f64 {
    let capacity: f64 = per_species_k.iter().map(|(_, k)| *k.get(cell)).sum();
    if capacity <= 0.0 {
        return 0.0;
    }
    let claimed: f64 = density.iter().map(|(_, d)| *d.get(cell)).sum();
    ((capacity - claimed).max(0.0) / capacity).clamp(0.0, 1.0)
}

/// The full wilderness field: [`wilderness_at`] evaluated over every cell of
/// `geo`.
///
/// type-audit: bare-ok(index: per_species_k), bare-ok(index: density), bare-ok(count: return)
pub fn wilderness(
    geo: &Geosphere,
    per_species_k: &[(u32, CellMap<f64>)],
    density: &[(u32, CellMap<f64>)],
) -> CellMap<f64> {
    CellMap::from_fn(geo, |c| wilderness_at(per_species_k, density, c))
}

/// The world's normally-dominant species: the one with the greatest total
/// `K` summed over every cell of `per_species_k`. Deterministic: totals
/// compare via `total_cmp`, ties broken by the lower species id (found by
/// scanning `per_species_k` in its given order, keeping the current best
/// only on a *strict* improvement — the first-seen id among equal totals
/// wins regardless of input order, since `per_species_k` is walked in full
/// either way and every candidate is compared against the same running
/// best).
///
/// Returns `None` for an empty `per_species_k` — there is no dominant
/// species over no species.
///
/// type-audit: bare-ok(index: per_species_k), bare-ok(index: return), bare-ok(count: return)
fn dominant_species(per_species_k: &[(u32, CellMap<f64>)]) -> Option<u32> {
    let mut totals: Vec<(u32, f64)> = per_species_k
        .iter()
        .map(|(id, k)| (*id, k.iter().map(|(_, v)| *v).sum::<f64>()))
        .collect();
    // Sort by id ascending first so the tie-break ("first-seen id among
    // equal totals") is independent of `per_species_k`'s caller-supplied
    // order, not just of iteration order within this function.
    totals.sort_by_key(|(id, _)| *id);

    let mut best: Option<(u32, f64)> = None;
    for (id, total) in totals {
        best = match best {
            None => Some((id, total)),
            Some((best_id, best_total)) => {
                if total.total_cmp(&best_total) == std::cmp::Ordering::Greater {
                    Some((id, total))
                } else {
                    Some((best_id, best_total))
                }
            }
        };
    }
    best.map(|(id, _)| id)
}

/// Per-species refugia: for every species in `per_species_k` other than the
/// world's normally-dominant species `D` (see [`dominant_species`]), the
/// cells where `D` is hostility-excluded (`K_D(cell) < floor`) while the
/// species itself still persists (`K_s(cell) >= floor`) — the weak's
/// strongholds, where the dominant simply cannot reach.
///
/// The marked value at a qualifying cell is the species' realized density
/// there (`density_s(cell)`, falling back to `0.0` if the species is absent
/// from `density` entirely), not a bare `1.0` indicator: a refugium with
/// more individuals in it is a stronger stronghold than a barely-persisting
/// one, and callers that only want presence can threshold `> 0.0` themselves
/// without losing information the other way. Non-qualifying cells are
/// `0.0`.
///
/// The dominant species `D` is omitted from the returned `Vec` entirely
/// (rather than included with a trivially-empty map): `D` never satisfies
/// its own exclusion condition against itself, so its map is `0.0`
/// everywhere by construction, and a map that can never be anything but
/// empty is dead weight for every caller to skip past.
///
/// An empty or single-species `per_species_k` yields an empty `Vec` (no
/// dominant, or the dominant is the only species and there is no "other" to
/// have refugia).
///
/// type-audit: bare-ok(index: per_species_k), bare-ok(index: density), bare-ok(count: floor), bare-ok(index: return)
pub fn refugia(
    geo: &Geosphere,
    per_species_k: &[(u32, CellMap<f64>)],
    density: &[(u32, CellMap<f64>)],
    floor: f64,
) -> Vec<(u32, CellMap<f64>)> {
    let Some(dominant_id) = dominant_species(per_species_k) else {
        return Vec::new();
    };
    let Some((_, k_dominant)) = per_species_k.iter().find(|(id, _)| *id == dominant_id) else {
        return Vec::new();
    };

    per_species_k
        .iter()
        .filter(|(id, _)| *id != dominant_id)
        .map(|(id, k_species)| {
            let own_density = density.iter().find(|(d_id, _)| d_id == id).map(|(_, d)| d);
            let map = CellMap::from_fn(geo, |c| {
                let dominant_excluded = *k_dominant.get(c) < floor;
                let species_persists = *k_species.get(c) >= floor;
                if dominant_excluded && species_persists {
                    own_density.map(|d| *d.get(c)).unwrap_or(0.0)
                } else {
                    0.0
                }
            });
            (*id, map)
        })
        .collect()
}

/// Assemble all three derived byproducts of a [`CoexistStack`] in one call:
/// [`strife`] and [`wilderness`] read `stack.density` against `per_species_k`
/// directly; [`refugia`] additionally needs `floor`, the same
/// hostility-exclusion threshold [`crate::coexist::cell_share`] zeroes shares
/// below.
///
/// type-audit: bare-ok(index: per_species_k), bare-ok(count: floor)
pub fn byproducts(
    geo: &Geosphere,
    stack: &CoexistStack,
    per_species_k: &[(u32, CellMap<f64>)],
    floor: f64,
) -> Byproducts {
    Byproducts {
        strife: strife(geo, &stack.density),
        wilderness: wilderness(geo, per_species_k, &stack.density),
        refugia: refugia(geo, per_species_k, &stack.density, floor),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_kernel::Geosphere;

    #[test]
    fn strife_peaks_at_balanced_fitness_not_at_high_k() {
        let geo = Geosphere::new(3);
        // Cell A (id 0): one dominant + one weak species.
        // Cell B (id 1): two equal species.
        let density0 = CellMap::from_fn(&geo, |c| match c.0 {
            0 => 0.9,
            1 => 0.5,
            _ => 0.0,
        });
        let density1 = CellMap::from_fn(&geo, |c| match c.0 {
            0 => 0.1,
            1 => 0.5,
            _ => 0.0,
        });
        let density = vec![(0u32, density0), (1u32, density1)];
        let field = strife(&geo, &density);

        assert!(
            *field.get(CellId(1)) > *field.get(CellId(0)),
            "balanced cell B ({}) should out-strife dominated cell A ({})",
            field.get(CellId(1)),
            field.get(CellId(0))
        );

        // High-density-but-dominated vs. low-density-but-balanced: strife
        // tracks balance, not magnitude. Cell C (id 2): high density,
        // dominated (50.0 vs 0.5). Cell D (id 3): low density, balanced
        // (0.01 vs 0.01).
        let mixed0 = CellMap::from_fn(&geo, |c| match c.0 {
            2 => 50.0,
            3 => 0.01,
            _ => 0.0,
        });
        let mixed1 = CellMap::from_fn(&geo, |c| match c.0 {
            2 => 0.5,
            3 => 0.01,
            _ => 0.0,
        });
        let mixed = vec![(0u32, mixed0), (1u32, mixed1)];
        let mixed_field = strife(&geo, &mixed);
        assert!(
            *mixed_field.get(CellId(3)) > *mixed_field.get(CellId(2)),
            "low-density balanced cell D ({}) should out-strife high-density dominated cell C ({})",
            mixed_field.get(CellId(3)),
            mixed_field.get(CellId(2))
        );
    }

    #[test]
    fn strife_is_zero_with_no_density() {
        let geo = Geosphere::new(3);
        let density = vec![
            (0u32, CellMap::from_fn(&geo, |_| 0.0)),
            (1u32, CellMap::from_fn(&geo, |_| 0.0)),
        ];
        let field = strife(&geo, &density);
        assert!(
            geo.cells().all(|c| *field.get(c) == 0.0),
            "no species present anywhere means no contest anywhere"
        );
    }

    #[test]
    fn refugia_track_hostility_zeroed_dominants() {
        let geo = Geosphere::new(3);
        let floor = 0.1;
        // D (id 0): dominant everywhere (high K, far larger total than s),
        // except cell 0 where D's K drops below floor (hostility-excluded).
        let k_d = CellMap::from_fn(&geo, |c| if c.0 == 0 { 0.0 } else { 5.0 });
        // s (id 1): low K everywhere, but still >= floor at cell 0.
        let k_s = CellMap::from_fn(&geo, |c| if c.0 == 0 { 0.2 } else { 0.05 });
        let per_species_k = vec![(0u32, k_d), (1u32, k_s)];

        let density_d = CellMap::from_fn(&geo, |c| if c.0 == 0 { 0.0 } else { 5.0 });
        let density_s = CellMap::from_fn(&geo, |c| if c.0 == 0 { 0.2 } else { 0.05 });
        let density = vec![(0u32, density_d), (1u32, density_s)];

        let maps = refugia(&geo, &per_species_k, &density, floor);
        assert_eq!(
            maps.len(),
            1,
            "only the non-dominant species gets a refugia map"
        );
        let (id, map) = &maps[0];
        assert_eq!(*id, 1u32, "s is the only non-dominant species");

        assert!(
            *map.get(CellId(0)) > 0.0,
            "cell 0: D excluded (K<floor), s persists (K>=floor) — a refugium"
        );
        // A cell where D is present (K_D >= floor) must NOT be marked, even
        // though s is also present there.
        assert_eq!(
            *map.get(CellId(1)),
            0.0,
            "cell 1: D is present — not a refugium for s"
        );
    }

    #[test]
    fn refugia_excludes_species_that_do_not_persist() {
        let geo = Geosphere::new(3);
        let floor = 0.1;
        // D excluded at cell 0, but s also below floor there — no refugium.
        let k_d = CellMap::from_fn(&geo, |c| if c.0 == 0 { 0.0 } else { 5.0 });
        let k_s = CellMap::from_fn(&geo, |c| if c.0 == 0 { 0.01 } else { 0.05 });
        let per_species_k = vec![(0u32, k_d), (1u32, k_s)];
        let density = vec![
            (0u32, CellMap::from_fn(&geo, |_| 0.0)),
            (1u32, CellMap::from_fn(&geo, |_| 0.01)),
        ];
        let maps = refugia(&geo, &per_species_k, &density, floor);
        let (_, map) = maps.iter().find(|(id, _)| *id == 1).unwrap();
        assert_eq!(
            *map.get(CellId(0)),
            0.0,
            "s below floor at cell 0 too — not a persisting refugium"
        );
    }

    #[test]
    fn wilderness_is_a_bounded_nonzero_fraction() {
        let geo = Geosphere::new(3);
        let per_species_k = vec![
            (0u32, CellMap::from_fn(&geo, |_| 1.0)),
            (1u32, CellMap::from_fn(&geo, |_| 1.0)),
        ];
        // Claimed density is well under the summed capacity (2.0) at every
        // cell, so wilderness should be positive everywhere and bounded.
        let density = vec![
            (0u32, CellMap::from_fn(&geo, |_| 0.3)),
            (1u32, CellMap::from_fn(&geo, |_| 0.2)),
        ];
        let field = wilderness(&geo, &per_species_k, &density);
        assert!(
            geo.cells().all(|c| (0.0..=1.0).contains(field.get(c))),
            "wilderness must stay within [0, 1] on every cell"
        );
        assert!(
            geo.cells().any(|c| *field.get(c) > 0.0),
            "claimed < capacity everywhere, so wilderness must be positive somewhere"
        );
    }

    #[test]
    fn wilderness_is_zero_with_no_capacity() {
        let geo = Geosphere::new(3);
        let per_species_k = vec![(0u32, CellMap::from_fn(&geo, |_| 0.0))];
        let density = vec![(0u32, CellMap::from_fn(&geo, |_| 0.0))];
        let field = wilderness(&geo, &per_species_k, &density);
        assert!(
            geo.cells().all(|c| *field.get(c) == 0.0),
            "no capacity means no unclaimed wilderness either"
        );
    }

    #[test]
    fn byproducts_assembles_all_three_fields() {
        let geo = Geosphere::new(3);
        let per_species_k = vec![
            (
                0u32,
                CellMap::from_fn(&geo, |c| if c.0 == 0 { 0.0 } else { 5.0 }),
            ),
            (
                1u32,
                CellMap::from_fn(&geo, |c| if c.0 == 0 { 0.2 } else { 0.05 }),
            ),
        ];
        let density = vec![
            (
                0u32,
                CellMap::from_fn(&geo, |c| if c.0 == 0 { 0.0 } else { 5.0 }),
            ),
            (
                1u32,
                CellMap::from_fn(&geo, |c| if c.0 == 0 { 0.2 } else { 0.05 }),
            ),
        ];
        let stack = CoexistStack {
            density,
            emigration_pressure: CellMap::from_fn(&geo, |_| 0.0),
        };
        let result = byproducts(&geo, &stack, &per_species_k, 0.1);
        assert_eq!(result.strife.len(), geo.cells().count());
        assert_eq!(result.wilderness.len(), geo.cells().count());
        assert_eq!(result.refugia.len(), 1, "only species 1 is non-dominant");
    }
}

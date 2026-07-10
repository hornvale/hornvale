//! Field-driven settlement placement: score every habitable cell for
//! suitability, then place a spaced scatter greedily from most to least
//! suitable. The single most-suitable cell is the flagship. Kernel-only:
//! the composition root supplies each cell's bare inputs (habitability,
//! freshwater, coast, temperature, hostility); this module does the social
//! scoring and the geometry.

use hornvale_kernel::CellId;
use std::collections::BTreeSet;

/// The four suitability weights; per-species values are derived at the
/// composition root from the psychology vector (spec §4).
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct SuitabilityWeights {
    /// Weight on freshwater availability.
    pub freshwater: f64,
    /// Weight on coastal access.
    pub coast: f64,
    /// Weight on temperance.
    pub temperance: f64,
    /// Penalty weight on hostility.
    pub hostility: f64,
}

/// The goblin-baseline weights — the pre-species formula, unchanged.
pub const BASELINE_WEIGHTS: SuitabilityWeights = SuitabilityWeights {
    freshwater: 0.45,
    coast: 0.20,
    temperance: 0.35,
    hostility: 0.50,
};

/// The bare per-cell inputs the composition root assembles from terrain and
/// climate. Settlement never imports those domains; it sees only this.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct SiteInput {
    /// The cell this site sits on.
    pub cell: CellId,
    /// Unit-sphere position (for spacing).
    pub position: [f64; 3],
    /// Whether the cell is habitable (land, water, tolerable season).
    pub habitable: bool,
    /// Freshwater availability in `[0, 1]` (drainage/moisture, at root).
    pub freshwater: f64,
    /// Whether the cell borders the ocean.
    pub coastal: bool,
    /// Annual-mean temperature, °C.
    pub temperature_c: f64,
    /// Hostility in `[0, 1]` (aridity, tectonic unrest).
    pub hostility: f64,
}

/// A placed settlement's cell, position, and the suitability that earned it.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Placement {
    /// The settlement's cell.
    pub cell: CellId,
    /// Unit-sphere position.
    pub position: [f64; 3],
    /// The suitability score that placed it, `[0, 1]`.
    pub suitability: f64,
}

/// Suitability under explicit weights; `None` if uninhabitable.
pub fn suitability_weighted(site: &SiteInput, w: &SuitabilityWeights) -> Option<f64> {
    if !site.habitable {
        return None;
    }
    let temperance = (1.0 - (site.temperature_c - 15.0).abs() / 15.0).clamp(0.0, 1.0);
    let coast = if site.coastal { 1.0 } else { 0.0 };
    let raw = w.freshwater * site.freshwater.clamp(0.0, 1.0)
        + w.coast * coast
        + w.temperance * temperance
        - w.hostility * site.hostility.clamp(0.0, 1.0);
    Some(raw.clamp(0.0, 1.0))
}

/// Score a site's suitability for settlement in `[0, 1]`; `None` if the cell
/// is uninhabitable. Watered, coastal, temperate, calm cells score high;
/// dry, inland, extreme, hostile cells score low.
pub fn suitability(site: &SiteInput) -> Option<f64> {
    suitability_weighted(site, &BASELINE_WEIGHTS)
}

/// Dot product of two 3-vectors.
fn dot(a: [f64; 3], b: [f64; 3]) -> f64 {
    a[0] * b[0] + a[1] * b[1] + a[2] * b[2]
}

/// The founder pass (MAP-22's allocation layer at K=1): before the
/// competitive fill, reserve each tag its own single best habitable cell so
/// no tag can be boxed out to zero by another tag's spacing radius. Iterates
/// all (tag, tag's-best-remaining-cell) candidates, repeatedly taking the
/// globally highest-suitability pair — deterministic tie-break on exactly
/// equal suitability: lower tag index, then lower cell id. Founders bypass
/// both `floor` (a people below the quality floor everywhere still gets a
/// flagship) and spacing against each other (spacing among founders would
/// reintroduce the same boxing-out this pass exists to prevent); the
/// competitive pass below still spaces later placements against founders.
fn founder_pass(scored: &[(SiteInput, f64, u32)]) -> (Vec<(Placement, u32)>, BTreeSet<u32>) {
    let mut founders: Vec<(Placement, u32)> = Vec::new();
    let mut reserved_cells: BTreeSet<u32> = BTreeSet::new();
    let mut remaining_tags: BTreeSet<u32> = scored.iter().map(|(_, _, tag)| *tag).collect();

    while !remaining_tags.is_empty() {
        // Each remaining tag's own best not-yet-reserved cell.
        let mut candidates: Vec<(f64, u32, u32)> = Vec::new(); // (score, tag, cell id)
        for &tag in &remaining_tags {
            let mut own: Vec<&(SiteInput, f64, u32)> = scored
                .iter()
                .filter(|(site, _, t)| *t == tag && !reserved_cells.contains(&site.cell.0))
                .collect();
            own.sort_by(|a, b| b.1.total_cmp(&a.1).then(a.0.cell.0.cmp(&b.0.cell.0)));
            if let Some((site, score, _)) = own.first() {
                candidates.push((*score, tag, site.cell.0));
            }
        }
        if candidates.is_empty() {
            break; // no habitable cell remains for any remaining tag
        }
        candidates.sort_by(|a, b| b.0.total_cmp(&a.0).then(a.1.cmp(&b.1)).then(a.2.cmp(&b.2)));
        let (score, tag, cell_id) = candidates[0];
        let site = *scored
            .iter()
            .find(|(s, _, t)| *t == tag && s.cell.0 == cell_id)
            .map(|(s, _, _)| s)
            .expect("selected candidate site must exist in scored");
        founders.push((
            Placement {
                cell: site.cell,
                position: site.position,
                suitability: score,
            },
            tag,
        ));
        reserved_cells.insert(cell_id);
        remaining_tags.remove(&tag);
    }

    (founders, reserved_cells)
}

/// Place a spaced scatter from pre-scored, tagged sites (tag = species index
/// at the root). Runs the founder pass first (each tag reserves its own best
/// cell), then sorts the un-reserved sites by score descending, ties by
/// ascending cell then ascending tag; greedily accepts sites at least
/// `min_separation_dot`-far from EVERY already-placed site (founder or
/// competitive) regardless of tag, at or above `floor`.
pub fn place_tagged(
    scored: &[(SiteInput, f64, u32)],
    min_separation_dot: f64,
    floor: f64,
) -> Vec<(Placement, u32)> {
    let (mut placed, reserved_cells) = founder_pass(scored);

    let mut ranked: Vec<&(SiteInput, f64, u32)> = scored
        .iter()
        .filter(|(site, s, _)| *s >= floor && !reserved_cells.contains(&site.cell.0))
        .collect();
    ranked.sort_by(|a, b| {
        b.1.total_cmp(&a.1)
            .then(a.0.cell.0.cmp(&b.0.cell.0))
            .then(a.2.cmp(&b.2))
    });
    for (site, score, tag) in ranked {
        let too_close = placed
            .iter()
            .any(|(p, _)| dot(p.position, site.position) > min_separation_dot);
        if too_close {
            continue;
        }
        placed.push((
            Placement {
                cell: site.cell,
                position: site.position,
                suitability: *score,
            },
            *tag,
        ));
    }
    placed
}

/// Place a spaced scatter under baseline weights (the original single-people
/// path) — a tag-0 wrapper over `place_tagged`.
pub fn place(sites: &[SiteInput], min_separation_dot: f64, floor: f64) -> Vec<Placement> {
    let scored: Vec<(SiteInput, f64, u32)> = sites
        .iter()
        .filter_map(|s| suitability(s).map(|score| (*s, score, 0u32)))
        .collect();
    place_tagged(&scored, min_separation_dot, floor)
        .into_iter()
        .map(|(p, _)| p)
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    fn site(
        cell: u32,
        pos: [f64; 3],
        habitable: bool,
        fresh: f64,
        coastal: bool,
        temp: f64,
        host: f64,
    ) -> SiteInput {
        SiteInput {
            cell: CellId(cell),
            position: pos,
            habitable,
            freshwater: fresh,
            coastal,
            temperature_c: temp,
            hostility: host,
        }
    }

    #[test]
    fn uninhabitable_scores_none_and_watered_beats_dry() {
        assert!(suitability(&site(0, [1.0, 0.0, 0.0], false, 1.0, true, 15.0, 0.0)).is_none());
        let wet = suitability(&site(1, [1.0, 0.0, 0.0], true, 0.9, true, 15.0, 0.0)).unwrap();
        let dry = suitability(&site(2, [1.0, 0.0, 0.0], true, 0.1, false, 15.0, 0.5)).unwrap();
        assert!(
            wet > dry,
            "watered/coastal/calm ({wet}) must beat dry/inland/hostile ({dry})"
        );
        assert!((0.0..=1.0).contains(&wet) && (0.0..=1.0).contains(&dry));
    }

    #[test]
    fn placement_respects_spacing_floor_and_ranks_flagship_first() {
        // Three near-identical high-suitability cells clustered together plus
        // one far, plus one below the floor.
        let close_a = [1.0, 0.0, 0.0];
        let close_b = [0.9998, 0.02, 0.0]; // ~1.1° from close_a
        let far = [-1.0, 0.0, 0.0];
        let sites = vec![
            site(10, close_a, true, 0.9, true, 15.0, 0.0),
            site(11, close_b, true, 0.8, true, 15.0, 0.0),
            site(12, far, true, 0.85, true, 15.0, 0.0),
            site(13, [0.0, 1.0, 0.0], true, 0.05, false, 40.0, 0.9), // below floor
        ];
        let sep = (12.0_f64.to_radians()).cos();
        let placed = place(&sites, sep, 0.25);
        // close_b is within 12° of close_a and lower suitability → excluded.
        let cells: Vec<u32> = placed.iter().map(|p| p.cell.0).collect();
        assert!(
            cells.contains(&10) && cells.contains(&12),
            "expected the two spaced high cells"
        );
        assert!(
            !cells.contains(&11),
            "clustered lower-suitability cell must be skipped"
        );
        assert!(!cells.contains(&13), "below-floor cell must be skipped");
        // Flagship is the global suitability argmax, placed first.
        assert_eq!(placed[0].cell.0, 10);
        assert!(
            placed
                .windows(2)
                .all(|w| w[0].suitability >= w[1].suitability),
            "not descending"
        );
    }

    #[test]
    fn placement_is_deterministic() {
        let sites = vec![
            site(1, [1.0, 0.0, 0.0], true, 0.7, true, 15.0, 0.1),
            site(2, [0.0, 1.0, 0.0], true, 0.6, false, 12.0, 0.2),
        ];
        let sep = (12.0_f64.to_radians()).cos();
        assert_eq!(place(&sites, sep, 0.25), place(&sites, sep, 0.25));
    }

    #[test]
    fn weighted_suitability_is_identity_at_baseline_weights() {
        let s = site(1, [1.0, 0.0, 0.0], true, 0.7, true, 15.0, 0.2);
        assert_eq!(suitability(&s), suitability_weighted(&s, &BASELINE_WEIGHTS));
    }

    #[test]
    fn place_tagged_with_one_tag_matches_place_exactly() {
        let sites = vec![
            site(10, [1.0, 0.0, 0.0], true, 0.9, true, 15.0, 0.0),
            site(12, [-1.0, 0.0, 0.0], true, 0.85, true, 15.0, 0.0),
            site(13, [0.0, 1.0, 0.0], true, 0.05, false, 40.0, 0.9),
        ];
        let sep = (12.0_f64.to_radians()).cos();
        let scored: Vec<(SiteInput, f64, u32)> = sites
            .iter()
            .filter_map(|s| suitability(s).map(|sc| (*s, sc, 0u32)))
            .collect();
        let tagged: Vec<Placement> = place_tagged(&scored, sep, 0.25)
            .into_iter()
            .map(|(p, _)| p)
            .collect();
        assert_eq!(tagged, place(&sites, sep, 0.25));
    }

    #[test]
    fn founder_floor_places_both_tags_despite_close_spacing() {
        // MAP-22: each tag's own-best cell is a guaranteed founder, so a
        // weaker tag is no longer boxed out to zero by a stronger tag's
        // spacing radius. This supersedes the pre-founder-floor behaviour
        // where only the higher-score site survived the shared spacing pass.
        let close_a = [1.0, 0.0, 0.0];
        let close_b = [0.9998, 0.02, 0.0]; // ~1.1° away
        let a = site(1, close_a, true, 0.9, true, 15.0, 0.0);
        let b = site(2, close_b, true, 0.8, true, 15.0, 0.0);
        let sep = (12.0_f64.to_radians()).cos();
        let placed = place_tagged(&[(a, 0.9, 0), (b, 0.8, 1)], sep, 0.25);
        assert_eq!(
            placed.len(),
            2,
            "each tag reserves its own founder cell regardless of spacing to the other"
        );
        let tags: Vec<u32> = placed.iter().map(|(_, t)| *t).collect();
        assert!(tags.contains(&0) && tags.contains(&1));
    }

    #[test]
    fn competitive_pass_still_enforces_spacing_across_tags_after_founders() {
        // Tag 0's founder is `a`. Tag 1's founder is the distant `far_b`
        // (tag 1's own-best); its second-choice `close_c` is close to `a`
        // and lower-scoring than `far_b`, so once founders are reserved the
        // competitive pass must still exclude `close_c` on cross-tag spacing.
        let a = site(1, [1.0, 0.0, 0.0], true, 0.9, true, 15.0, 0.0);
        let far_b = site(2, [-1.0, 0.0, 0.0], true, 0.95, true, 15.0, 0.0);
        let close_c = site(3, [0.9998, 0.02, 0.0], true, 0.5, true, 15.0, 0.0); // ~1.1° from a
        let sep = (12.0_f64.to_radians()).cos();
        let placed = place_tagged(
            &[(a, 0.9, 0), (far_b, 0.95, 1), (close_c, 0.5, 1)],
            sep,
            0.25,
        );
        let cells: Vec<u32> = placed.iter().map(|(p, _)| p.cell.0).collect();
        assert!(cells.contains(&1), "tag 0's founder must be placed");
        assert!(cells.contains(&2), "tag 1's founder (far_b) must be placed");
        assert!(
            !cells.contains(&3),
            "close_c is not a founder and must lose to cross-tag spacing"
        );
        assert_eq!(placed.len(), 2);
    }

    #[test]
    fn every_tag_gets_at_least_one_placement_when_cells_suffice() {
        // Four tags, four mutually-close cells (all within the spacing
        // radius of each other), each cell the sole candidate for exactly
        // one tag. Without a founder pass the shared spacing radius would
        // let only the single highest-score site survive, boxing the other
        // three tags out to zero (MAP-22's pigeonhole). With the founder
        // pass every tag must appear at least once.
        let cells = [
            site(1, [1.0, 0.0, 0.0], true, 0.9, true, 15.0, 0.0), // 0°
            site(2, [0.9994, 0.0349, 0.0], true, 0.8, true, 15.0, 0.0), // ~2°
            site(3, [0.9976, 0.0698, 0.0], true, 0.7, true, 15.0, 0.0), // ~4°
            site(4, [0.9945, 0.1045, 0.0], true, 0.6, true, 15.0, 0.0), // ~6°
        ];
        let scored: Vec<(SiteInput, f64, u32)> = cells
            .iter()
            .enumerate()
            .map(|(tag, s)| (*s, suitability(s).unwrap(), tag as u32))
            .collect();
        let sep = (12.0_f64.to_radians()).cos();
        let placed = place_tagged(&scored, sep, 0.25);
        let tags: std::collections::BTreeSet<u32> = placed.iter().map(|(_, t)| *t).collect();
        assert_eq!(
            tags,
            std::collections::BTreeSet::from([0, 1, 2, 3]),
            "every tag must be represented when enough habitable cells exist"
        );
    }

    #[test]
    fn founder_pass_resolves_same_cell_collision_by_higher_suitability() {
        // Tag 0 and tag 1 both score highest at cell 1 (tag 0 higher); tag 1
        // also has a lower-scoring, distinct fallback cell 2. Tag 0 keeps
        // cell 1; tag 1 falls back to cell 2.
        let shared = site(1, [1.0, 0.0, 0.0], true, 0.9, true, 15.0, 0.0);
        let fallback = site(2, [-1.0, 0.0, 0.0], true, 0.4, true, 15.0, 0.0);
        let scored = vec![
            (shared, 0.9, 0u32),
            (shared, 0.7, 1u32),
            (fallback, 0.4, 1u32),
        ];
        let placed = place_tagged(&scored, (12.0_f64.to_radians()).cos(), 0.25);
        let by_tag = |t: u32| placed.iter().find(|(_, tag)| *tag == t).unwrap().0.cell.0;
        assert_eq!(
            by_tag(0),
            1,
            "tag 0 keeps the higher-scoring collision cell"
        );
        assert_eq!(by_tag(1), 2, "tag 1 falls back to its next-best cell");
    }

    #[test]
    fn founder_pass_breaks_exact_ties_by_lower_tag_then_lower_cell() {
        // Tag 0 and tag 1 both score exactly 0.7 at the same cell. The tie
        // is broken by lower tag index: tag 0 keeps the cell, tag 1 falls
        // back to its distinct second-choice cell.
        let shared = site(1, [1.0, 0.0, 0.0], true, 0.7, true, 15.0, 0.0);
        let fallback = site(2, [-1.0, 0.0, 0.0], true, 0.5, true, 15.0, 0.0);
        let scored = vec![
            (shared, 0.7, 0u32),
            (shared, 0.7, 1u32),
            (fallback, 0.5, 1u32),
        ];
        let placed = place_tagged(&scored, (12.0_f64.to_radians()).cos(), 0.25);
        let by_tag = |t: u32| placed.iter().find(|(_, tag)| *tag == t).unwrap().0.cell.0;
        assert_eq!(by_tag(0), 1, "lower tag index wins the exact tie");
        assert_eq!(by_tag(1), 2, "tag 1 falls back after losing the tie");
    }

    #[test]
    fn founder_floor_is_deterministic_and_order_independent() {
        let cells = [
            site(1, [1.0, 0.0, 0.0], true, 0.9, true, 15.0, 0.0),
            site(2, [0.9994, 0.0349, 0.0], true, 0.8, true, 15.0, 0.0),
            site(3, [0.9976, 0.0698, 0.0], true, 0.7, true, 15.0, 0.0),
            site(4, [0.9945, 0.1045, 0.0], true, 0.6, true, 15.0, 0.0),
        ];
        let scored: Vec<(SiteInput, f64, u32)> = cells
            .iter()
            .enumerate()
            .map(|(tag, s)| (*s, suitability(s).unwrap(), tag as u32))
            .collect();
        let mut reversed = scored.clone();
        reversed.reverse();
        let sep = (12.0_f64.to_radians()).cos();
        let a = place_tagged(&scored, sep, 0.25);
        let b = place_tagged(&reversed, sep, 0.25);
        assert_eq!(a, b, "result must not depend on input ordering");
        assert_eq!(
            a,
            place_tagged(&scored, sep, 0.25),
            "repeat call must match"
        );
    }
}

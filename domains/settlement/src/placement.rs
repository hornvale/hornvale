//! Field-driven settlement placement: score every habitable cell for
//! suitability, then place a spaced scatter greedily from most to least
//! suitable. The single most-suitable cell is the flagship. Kernel-only:
//! the composition root supplies each cell's bare inputs (habitability,
//! freshwater, coast, temperature, hostility); this module does the social
//! scoring and the geometry.

use hornvale_kernel::CellId;

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
    /// Freshwater availability in `[0, 1]` (drainage/coast/moisture, at root).
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

/// Score a site's suitability for settlement in `[0, 1]`; `None` if the cell
/// is uninhabitable. Watered, coastal, temperate, calm cells score high;
/// dry, inland, extreme, hostile cells score low.
pub fn suitability(site: &SiteInput) -> Option<f64> {
    if !site.habitable {
        return None;
    }
    // Temperance: a triangular preference peaking at 15 °C, zero by 0/30 °C.
    let temperance = (1.0 - (site.temperature_c - 15.0).abs() / 15.0).clamp(0.0, 1.0);
    let coast = if site.coastal { 1.0 } else { 0.0 };
    let raw = 0.45 * site.freshwater.clamp(0.0, 1.0) + 0.20 * coast + 0.35 * temperance
        - 0.5 * site.hostility.clamp(0.0, 1.0);
    Some(raw.clamp(0.0, 1.0))
}

/// Dot product of two 3-vectors.
fn dot(a: [f64; 3], b: [f64; 3]) -> f64 {
    a[0] * b[0] + a[1] * b[1] + a[2] * b[2]
}

/// Place a spaced scatter of settlements. Rank habitable sites by suitability
/// (descending; ties by ascending `CellId`), then greedily accept a site only
/// if it is at least `min_separation_dot`-far (smaller dot = farther) from
/// every already-placed settlement and its suitability is at least `floor`.
/// The first element is the flagship (global suitability argmax).
pub fn place(sites: &[SiteInput], min_separation_dot: f64, floor: f64) -> Vec<Placement> {
    let mut scored: Vec<(SiteInput, f64)> = sites
        .iter()
        .filter_map(|s| suitability(s).map(|score| (*s, score)))
        .filter(|(_, score)| *score >= floor)
        .collect();
    scored.sort_by(|a, b| b.1.total_cmp(&a.1).then(a.0.cell.0.cmp(&b.0.cell.0)));

    let mut placed: Vec<Placement> = Vec::new();
    for (site, score) in scored {
        let too_close = placed
            .iter()
            .any(|p| dot(p.position, site.position) > min_separation_dot);
        if too_close {
            continue;
        }
        placed.push(Placement {
            cell: site.cell,
            position: site.position,
            suitability: score,
        });
    }
    placed
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
}

//! Deterministic composition-root map renders — lenses over worldgen-derived
//! data that a domain's own `render.rs` cannot reach because it needs the
//! ledger (`World`), not just a domain provider. The Vestige's residue lens
//! lives here for exactly that reason: [`crate::vestige::vestiges_at`] reads
//! committed occupation history off `World::ledger` plus terrain, so only
//! the composition root (which already reconstructs both) can render it.
//! Mirrors `hornvale_paleoclimate::render::paleo_png` and
//! `hornvale_settlement::render::overlay_png`'s equirectangular-PNG
//! convention: same world, same bytes — no draws, no mutation.

use crate::terrain_of;
use crate::vestige::{Valence, Vestige, VestigeKind, vestiges_at};
use hornvale_kernel::{CellMap, NearestCellIndex, World};

/// Raster width, pixels (equirectangular → height is half). Matches the
/// paleoclimate/deep-time lens's resolution (`hornvale_paleoclimate::render::
/// MAP_WIDTH`): residue sites are sparse point features (like the terrain
/// Lode lens's caves/deposits), so a coarse raster already shows clusters
/// cleanly, and keeping it coarse keeps [`residue_pixels`]'s per-pixel color
/// lookup cheap regardless of how expensive computing a cell's palimpsest is.
/// type-audit: bare-ok(render-internal)
pub const MAP_WIDTH: u32 = 256;

/// Muted base color for a cell with no residue at all: a flat slate,
/// deliberately low-key so the kind/valence palette below reads clearly
/// against it (the same convention as terrain's `features_pixels` ocean/land
/// base).
/// type-audit: bare-ok(render-internal: return)
const EMPTY_BASE: [u8; 3] = [70, 70, 78];

/// The palette color for a residue site's kind *and* valence: an exhaustive
/// match over `(VestigeKind, Valence)` (10 combinations) so a future variant
/// on either enum without a color fails to compile rather than panicking at
/// render time. Each kind gets its own hue family; within a hue, venerated
/// reads warm and saturated (a still-tended site) while forgotten reads dim
/// and desaturated (only dread remains) — the same remembered-vs-dreaded
/// axis the dread field itself reads.
/// type-audit: bare-ok(render-internal: return)
fn vestige_color(kind: VestigeKind, valence: Valence) -> [u8; 3] {
    match (kind, valence) {
        (VestigeKind::GateScar, Valence::Venerated) => [190, 100, 220],
        (VestigeKind::GateScar, Valence::Forgotten) => [80, 30, 100],
        (VestigeKind::NaturalSeal, Valence::Venerated) => [130, 190, 230],
        (VestigeKind::NaturalSeal, Valence::Forgotten) => [45, 70, 95],
        (VestigeKind::AbandonedDelving, Valence::Venerated) => [230, 190, 90],
        (VestigeKind::AbandonedDelving, Valence::Forgotten) => [95, 80, 40],
        (VestigeKind::BuriedRuin, Valence::Venerated) => [220, 150, 110],
        (VestigeKind::BuriedRuin, Valence::Forgotten) => [85, 55, 45],
        (VestigeKind::SealedVault, Valence::Venerated) => [235, 235, 160],
        (VestigeKind::SealedVault, Valence::Forgotten) => [90, 90, 55],
    }
}

/// The most-dreaded layer in a cell's palimpsest stack, if any: the vestige
/// with the highest `dread` (the one a wanderer would sense most strongly).
/// Ties keep the first-encountered (oldest, per [`vestiges_at`]'s ordering)
/// layer, so the pick is deterministic without depending on `total_cmp`'s own
/// tie behavior.
fn most_dread(stack: &[Vestige]) -> Option<&Vestige> {
    let mut best: Option<&Vestige> = None;
    for v in stack {
        if best.is_none_or(|b| v.dread > b.dread) {
            best = Some(v);
        }
    }
    best
}

/// Raw RGB pixels of the equirectangular residue map: each cell's color is
/// computed once ([`vestige_color`] of its [`most_dread`] layer, or
/// [`EMPTY_BASE`] where the palimpsest is empty) over the geosphere's actual
/// cells, then the pixel grid looks each pixel's nearest cell up in that
/// precomputed table. Computing per-cell first (not per pixel, as
/// `NearestCellIndex`-based lenses elsewhere do) matters here because
/// `vestiges_at` rescans the ledger's committed occupation history on every
/// call — precomputing over cells keeps that cost independent of
/// [`MAP_WIDTH`].
fn residue_pixels(world: &World, terrain: &hornvale_terrain::GeneratedTerrain) -> Vec<u8> {
    let geo = terrain.geosphere();
    let colors = CellMap::from_fn(geo, |cell| {
        match most_dread(&vestiges_at(world, terrain, cell)) {
            Some(v) => vestige_color(v.kind, v.valence),
            None => EMPTY_BASE,
        }
    });
    let (width, height) = (MAP_WIDTH, MAP_WIDTH / 2);
    let index = NearestCellIndex::new(geo);
    let mut out = Vec::with_capacity((width * height * 3) as usize);
    for py in 0..height {
        let latitude = 90.0 - (f64::from(py) + 0.5) / f64::from(height) * 180.0;
        for px in 0..width {
            let longitude = (f64::from(px) + 0.5) / f64::from(width) * 360.0 - 180.0;
            let cell = index.nearest(geo, latitude, longitude);
            out.extend_from_slice(colors.get(cell));
        }
    }
    out
}

/// Render the world's residue palimpsest as an equirectangular PNG lens (The
/// Vestige): each cell's most-dread layer's kind/valence color, a muted base
/// where no residue sits. Same world, same bytes — `vestiges_at` is a pure
/// derived read of committed history and terrain, no draws and no mutation.
/// Reconstructs terrain via [`terrain_of`] (the single construction site)
/// rather than taking a pre-built one, so the signature matches the
/// composition root's other single-`World`-argument derived readouts (e.g.
/// `goblinoid_region_overlap`); a world that fails to reconstruct its own
/// terrain is not a world this lens can be asked to render.
/// type-audit: bare-ok(artifact: return)
pub fn vestige_png(world: &World) -> Vec<u8> {
    let terrain = terrain_of(world).expect("a built world's terrain reconstructs");
    hornvale_kernel::png::encode_rgb(MAP_WIDTH, MAP_WIDTH / 2, &residue_pixels(world, &terrain))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{SettlementPins, SkyChoice, build_world};
    use hornvale_astronomy::SkyPins;
    use hornvale_kernel::Seed;
    use hornvale_terrain::TerrainPins;

    #[test]
    fn vestige_png_is_well_formed_and_byte_deterministic() {
        let world = build_world(
            Seed(42),
            &SkyPins::default(),
            SkyChoice::Generated,
            &TerrainPins::default(),
            &SettlementPins::default(),
        )
        .unwrap();
        let a = vestige_png(&world);
        assert_eq!(a, vestige_png(&world), "same world must render same bytes");
        assert!(
            a.starts_with(&[0x89, b'P', b'N', b'G', 0x0D, 0x0A, 0x1A, 0x0A]),
            "must start with the PNG signature"
        );
        assert_eq!(&a[16..20], &MAP_WIDTH.to_be_bytes(), "IHDR width");
        assert_eq!(&a[20..24], &(MAP_WIDTH / 2).to_be_bytes(), "IHDR height");
    }

    /// Every `(VestigeKind, Valence)` combination, in declaration order —
    /// mirrors `vestige_color`'s match arms so the distinctness test below
    /// can enumerate the full palette without a runtime-panicking lookup.
    const ALL_KIND_VALENCE: [(VestigeKind, Valence); 10] = [
        (VestigeKind::GateScar, Valence::Venerated),
        (VestigeKind::GateScar, Valence::Forgotten),
        (VestigeKind::NaturalSeal, Valence::Venerated),
        (VestigeKind::NaturalSeal, Valence::Forgotten),
        (VestigeKind::AbandonedDelving, Valence::Venerated),
        (VestigeKind::AbandonedDelving, Valence::Forgotten),
        (VestigeKind::BuriedRuin, Valence::Venerated),
        (VestigeKind::BuriedRuin, Valence::Forgotten),
        (VestigeKind::SealedVault, Valence::Venerated),
        (VestigeKind::SealedVault, Valence::Forgotten),
    ];

    #[test]
    fn every_kind_valence_pair_has_a_distinct_palette_color() {
        use std::collections::BTreeSet;
        let colors: BTreeSet<[u8; 3]> = ALL_KIND_VALENCE
            .iter()
            .map(|&(k, v)| vestige_color(k, v))
            .collect();
        assert_eq!(
            colors.len(),
            ALL_KIND_VALENCE.len(),
            "two kind/valence combinations share a palette color"
        );
    }

    #[test]
    fn venerated_and_forgotten_read_distinctly_within_a_kind() {
        for kind in [
            VestigeKind::GateScar,
            VestigeKind::NaturalSeal,
            VestigeKind::AbandonedDelving,
            VestigeKind::BuriedRuin,
            VestigeKind::SealedVault,
        ] {
            assert_ne!(
                vestige_color(kind, Valence::Venerated),
                vestige_color(kind, Valence::Forgotten),
                "{kind:?} must read differently venerated vs forgotten"
            );
        }
    }

    #[test]
    fn most_dread_picks_the_highest_dread_layer_breaking_ties_first() {
        let base = Vestige {
            kind: VestigeKind::BuriedRuin,
            seal_state: crate::vestige::SealState::Breached,
            valence: Valence::Forgotten,
            hazard: crate::vestige::HazardKind::Structural,
            dread: 0.3,
            warning_legibility: 0.0,
            founded_day: Some(0.0),
        };
        let higher = Vestige {
            dread: 0.9,
            kind: VestigeKind::SealedVault,
            ..base
        };
        let tie = Vestige {
            kind: VestigeKind::GateScar,
            ..higher
        };
        assert_eq!(most_dread(&[]), None);
        assert_eq!(most_dread(&[base]).unwrap().kind, VestigeKind::BuriedRuin);
        let stack = [base, higher, tie];
        let picked = most_dread(&stack).unwrap();
        assert_eq!(
            picked.kind,
            VestigeKind::SealedVault,
            "the higher-dread layer wins over a later tie"
        );
    }
}

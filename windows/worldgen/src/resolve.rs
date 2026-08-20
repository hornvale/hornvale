//! Cursor resolution: turn a cell into the name of the most specific
//! feature there. See `docs/superpowers/specs/2026-08-19-the-portolan-
//! design.md` §3 — the terminal client's free-roaming cursor query.
//!
//! This is deliberately a thin function, not a new kind of index: the real
//! work (declaring salience, building `CellId -> Vec<FeatureId>` once at
//! world load) lives in `hornvale_terrain::landscape` — `FeatureClass::
//! salience` and `CellFeatureIndex` — because it needs nothing this crate
//! adds (`domains/terrain` may not depend on `hornvale-language` or
//! `windows/worldgen`, by the kernel -> domains -> windows layering). What
//! belongs here is the one step that DOES need this crate: turning a
//! resolved [`FeatureId`] into a drawn name via [`crate::feature_name`].

use hornvale_kernel::{CellId, Seed};
use hornvale_language::{MorphOptions, Phonology};
use hornvale_terrain::landscape::CellFeatureIndex;

/// The most specific feature's name at `cell`, or `None` if `cell` carries
/// no individuated feature (real terrain below every class's individuation
/// floor — seed 42 measured 377 of 40,962 cells like this, 0.92%).
///
/// `index` must already be built (`CellFeatureIndex::build`, which sorts
/// each cell's stack most-specific-first), so this is an `O(1)` lookup —
/// `index.at(cell).first()` — plus one name draw. The caller supplies the
/// world's seed and one people's phonology/morphology; a different people
/// at the same cell draws a different name from the same [`FeatureId`], the
/// same multi-name shape [`crate::gazetteer_class_entries`] already gives
/// the gazetteer page.
/// type-audit: bare-ok(identifier-text: species), bare-ok(identifier-text: return)
pub fn resolve_at(
    index: &CellFeatureIndex,
    cell: CellId,
    seed: Seed,
    species: &str,
    ph: &Phonology,
    morph: &MorphOptions,
) -> Option<String> {
    let id = *index.at(cell).first()?;
    Some(crate::feature_name(seed, id, species, ph, morph).roman)
}

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_kernel::Geosphere;
    use hornvale_language::{Envelope, ExoticSeg, draw_phonology};
    use hornvale_terrain::{GeneratedTerrain, TerrainPins};

    /// The mesh level `windows/worldgen`'s own gazetteer/volcano tests use
    /// (see `gazetteer.rs`'s `LEVEL` const), so this exercises real,
    /// seed-42-shaped data without paying `GLOBE_LEVEL`'s full cost.
    const LEVEL: u32 = 6;
    const PEOPLE: &str = "aeldrin";

    fn test_phonology() -> (Phonology, MorphOptions) {
        let ph = draw_phonology(
            &Seed(7),
            PEOPLE,
            &Envelope {
                labiality: 1.0,
                vowel_space: 1.0,
                voicing: 1.0,
                sibilance: 1.0,
                voice_loudness: 1.0,
                tonality: 0.0,
                exotic: ExoticSeg::None,
            },
            &hornvale_language::typology::concatenative(),
        );
        let morph = MorphOptions {
            honorifics: false,
            shape_weights: [1.0, 1.0, 1.0],
            shape_beta: 1.0,
        };
        (ph, morph)
    }

    /// A cell inside a real feature's extent resolves to `Some` name; the
    /// name matches what [`crate::feature_name`] draws directly for that
    /// feature's identity, so `resolve_at` is not doing anything to the
    /// name besides the lookup.
    #[test]
    fn a_covered_cell_resolves_to_the_most_specific_features_name() {
        let seed = Seed(42);
        let geo = Geosphere::new(LEVEL);
        let outcome = hornvale_terrain::generate(seed, &geo, &TerrainPins::default())
            .expect("default pins generate");
        let terrain = GeneratedTerrain::new(geo.clone(), outcome);
        let features = crate::gazetteer_features(seed, &geo, &terrain);
        let index = CellFeatureIndex::build(&features);
        let (ph, morph) = test_phonology();

        let covered = features
            .first()
            .and_then(|f| f.extent.iter().next().copied())
            .expect("seed 42 has at least one feature with a nonempty extent");
        let want_id = *index.at(covered).first().expect("covered cell resolves");
        let want = crate::feature_name(seed, want_id, PEOPLE, &ph, &morph).roman;

        assert_eq!(
            resolve_at(&index, covered, seed, PEOPLE, &ph, &morph),
            Some(want)
        );
    }

    /// An empty index (no features at all) resolves every cell to `None`,
    /// not a panic.
    #[test]
    fn an_empty_index_resolves_to_none() {
        let (ph, morph) = test_phonology();
        let index = CellFeatureIndex::build(&[]);
        assert_eq!(
            resolve_at(&index, CellId(0), Seed(42), PEOPLE, &ph, &morph),
            None
        );
    }
}

//! The naming join: every individuated landscape feature, named per people.
//! See `docs/superpowers/specs/2026-08-18-the-gazetteer-design.md`.
//!
//! `domains/terrain::landscape` builds four of the five [`FeatureClass`]
//! traversal classes at genesis time (`GeneratedTerrain::features`); the
//! fifth, [`FeatureClass::Volcano`], cannot live there because `volcano_at`
//! lives in this crate and `domains/terrain` may not depend on it (the
//! kernel -> domains -> windows layering forbids the edge the other
//! direction, and a domain may never depend on another domain either). This
//! module is the composition root that gathers the complete set and joins it
//! to a name.

use hornvale_kernel::{Geosphere, Seed, Vertex};
use hornvale_language::{GeneratedName, MorphOptions, NameKind, Namer, Phonology};
use hornvale_terrain::GeneratedTerrain;
use hornvale_terrain::landscape::{Feature, FeatureClass, FeatureId};
use std::collections::{BTreeMap, BTreeSet};

/// Every individuated feature on `terrain`: the four traversal classes
/// `terrain.features()` already carries, plus the volcanoes gathered here.
///
/// This is the ONLY place a volcano enters a feature set — see the module
/// docs on why `domains/terrain` cannot build this class itself.
pub fn gazetteer_features(seed: Seed, geo: &Geosphere, terrain: &GeneratedTerrain) -> Vec<Feature> {
    let mut features = volcano_features(seed, geo, terrain);
    for class in [
        FeatureClass::Landmass,
        FeatureClass::Sea,
        FeatureClass::SaltLake,
        FeatureClass::River,
    ] {
        features.extend(terrain.features().of(class).iter().cloned());
    }
    features
}

/// Every edifice cell `volcano_at` reports, grouped into one [`Feature`] per
/// [`crate::Volcano::source`] — the identity a cone shares across every cell
/// it occupies (`crate::volcano` module docs). Grouping by the query cell
/// instead of the source would split every multi-cell cone into two
/// features, exactly the mistake `volcano_name`'s signature (taking a
/// `Volcano`, not a `Vertex`) exists to prevent.
fn volcano_features(seed: Seed, geo: &Geosphere, terrain: &GeneratedTerrain) -> Vec<Feature> {
    let mut by_source: BTreeMap<Vertex, BTreeSet<Vertex>> = BTreeMap::new();
    for cell in geo.vertices() {
        if let Some(volcano) = crate::volcano_at(seed, terrain, cell) {
            by_source.entry(volcano.source).or_default().insert(cell);
        }
    }
    by_source
        .into_iter()
        .map(|(source, extent)| Feature {
            id: FeatureId {
                class: FeatureClass::Volcano,
                cell: source,
            },
            anchor: source,
            magnitude: extent.len() as u32,
            extent,
        })
        .collect()
}

/// The naming salt for one feature.
///
/// Injective over `(class, cell)` by construction: `class` occupies the high
/// 32 bits and `cell` (a `u32`) the low 32, so two features collide only if
/// both their class and their cell agree — i.e. only if they are the same
/// feature. Without the class term, a landmass and a river sharing an
/// identity cell would draw the identical name from the identical people
/// (`two_classes_at_one_cell_do_not_share_a_name` pins the collision this
/// prevents). `FeatureClass::Volcano = 0` makes a volcano's salt equal its
/// bare cell id — byte-identical to what `volcano_name` already draws with
/// (`u64::from(volcano.source.0)`), so adopting this scheme moves no volcano
/// name in any world (`a_volcanos_salt_is_its_bare_cell_id`).
/// type-audit: pending(wave-3: return)
pub fn feature_salt(id: FeatureId) -> u64 {
    ((id.class as u64) << 32) | u64::from(id.cell.0)
}

/// What one people calls one feature.
///
/// The gazetteer's counterpart to [`crate::volcano_name`]: a feature has no
/// language of its own, so it has as many names as there are peoples with a
/// word for it, keyed on [`feature_salt`] so no two features of different
/// classes (or different instances of the same class) ever draw the same
/// name from the same people.
/// type-audit: bare-ok(identifier-text: species)
pub fn feature_name(
    seed: Seed,
    id: FeatureId,
    species: &str,
    ph: &Phonology,
    morph: &MorphOptions,
) -> GeneratedName {
    Namer::new(&seed, species, ph).name(NameKind::Landform, feature_salt(id), morph)
}

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_language::{Envelope, ExoticSeg, draw_phonology};
    use hornvale_terrain::TerrainPins;
    use std::collections::BTreeMap;

    /// The mesh level every test here builds at — the same canonical globe
    /// `windows/worldgen/src/volcano.rs`'s tests use, so seed 42's volcano
    /// population is the same population that module measured (360 edifice
    /// cells over ~187 contacts on level 6).
    const LEVEL: u32 = 6;

    fn test_terrain() -> (Geosphere, GeneratedTerrain) {
        let geo = Geosphere::new(LEVEL);
        let outcome = hornvale_terrain::generate(Seed(42), &geo, &TerrainPins::default())
            .expect("default pins generate");
        let terrain = GeneratedTerrain::new(geo.clone(), outcome);
        (geo, terrain)
    }

    /// A phonology and morph shape to name with, mirroring
    /// `windows/worldgen/src/volcano.rs`'s `phonology`/`morph` idiom
    /// (that module's test module, ~lines 245-268). Returned as one pair so
    /// a caller that wants ONE shared phonology across two species — as
    /// `two_peoples_name_one_feature_differently` below must — gets it by
    /// construction rather than by remembering to draw only once.
    fn test_phonology() -> (Phonology, MorphOptions) {
        let ph = draw_phonology(
            &Seed(7),
            "aeldrin",
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

    /// A volcano's two halves are ONE feature. `volcano_at` answers per cell,
    /// so grouping by cell rather than by `source` would split every cone —
    /// the exact mistake `volcano_name`'s signature exists to prevent.
    #[test]
    fn a_volcanos_cells_group_into_one_feature_per_source() {
        let (geo, terrain) = test_terrain();
        for f in gazetteer_features(Seed(42), &geo, &terrain) {
            if f.id.class != FeatureClass::Volcano {
                continue;
            }
            for cell in &f.extent {
                let v = crate::volcano_at(Seed(42), &terrain, *cell)
                    .expect("an edifice cell has a volcano");
                assert_eq!(
                    v.source, f.id.cell,
                    "cell {cell:?} grouped under the wrong source"
                );
            }
        }
    }

    /// THE COLLISION THIS SCHEME EXISTS TO PREVENT. Two features of different
    /// classes sharing an identity cell must not draw the same name from the
    /// same people. Before the class entered the salt they did — silently,
    /// because two features sharing a name is a thing real toponymy does.
    #[test]
    fn two_classes_at_one_cell_do_not_share_a_name() {
        let (ph, morph) = test_phonology();
        let cell = Vertex(1234);
        let a = feature_name(
            Seed(42),
            FeatureId {
                class: FeatureClass::Landmass,
                cell,
            },
            "aeldrin",
            &ph,
            &morph,
        );
        let b = feature_name(
            Seed(42),
            FeatureId {
                class: FeatureClass::River,
                cell,
            },
            "aeldrin",
            &ph,
            &morph,
        );
        assert_ne!(
            a.roman, b.roman,
            "a landmass and a river at cell 1234 share a name"
        );
    }

    /// The salt is injective over (class, cell) — the property the test above
    /// only samples. Exhaustive over a range wide enough to cross the stride.
    #[test]
    fn the_salt_is_injective_over_class_and_cell() {
        let mut seen: BTreeMap<u64, FeatureId> = BTreeMap::new();
        for class in [
            FeatureClass::Volcano,
            FeatureClass::Landmass,
            FeatureClass::Sea,
            FeatureClass::SaltLake,
            FeatureClass::River,
        ] {
            for cell in 0u32..5000 {
                let id = FeatureId {
                    class,
                    cell: Vertex(cell),
                };
                if let Some(prev) = seen.insert(feature_salt(id), id) {
                    panic!("salt collision between {prev:?} and {id:?}");
                }
            }
        }
    }

    /// A volcano's salt is its bare cell id — exactly what `volcano_name`
    /// already draws with, so this scheme moves no volcano name.
    #[test]
    fn a_volcanos_salt_is_its_bare_cell_id() {
        for cell in [0u32, 1, 4095, 99999] {
            let id = FeatureId {
                class: FeatureClass::Volcano,
                cell: Vertex(cell),
            };
            assert_eq!(feature_salt(id), u64::from(cell));
        }
    }

    /// One landform, many names (spec H2), at unit scale. Task 7 measures it
    /// at world scale; this is the control that says the species salt reaches
    /// the draw at all.
    ///
    /// **Mirrors `windows/worldgen/src/volcano.rs`'s
    /// `one_volcano_carries_a_different_name_in_each_language`, for the same
    /// reason that test's own doc comment gives, and this test's control
    /// value depends on preserving it exactly: the two peoples share ONE
    /// `Phonology`** (`test_phonology` draws it once; both calls below reuse
    /// that same value). Giving each species its own phonology would let this
    /// test pass on the phonology's difference alone — in `volcano.rs` a
    /// mutation that discarded `species` and named every people's mountain
    /// off one fixed tongue stayed GREEN under a per-species phonology,
    /// because the two `Phonology` values still differed. Holding the
    /// phonology fixed here leaves `species` as the only thing that moved, so
    /// this is the control Task 7's world-scale H2 measurement cannot itself
    /// be: Task 7 draws real per-species phonologies, so it can never
    /// distinguish "species reaches the draw" from "the tongues differ".
    #[test]
    fn two_peoples_name_one_feature_differently() {
        let (ph, morph) = test_phonology();
        let id = FeatureId {
            class: FeatureClass::Landmass,
            cell: Vertex(77),
        };
        let a = feature_name(Seed(42), id, "aeldrin", &ph, &morph);
        let b = feature_name(Seed(42), id, "khorrun", &ph, &morph);
        assert_ne!(
            a.roman, b.roman,
            "the species salt is not reaching the draw"
        );
    }
}

//! The Swadesh packs: the authored core vocabulary a culture's language
//! draws its root words from. A pack is a flat list of [`PackEntry`]
//! values; `register_concepts` folds every entry not already owned by
//! another domain into the [`ConceptRegistry`] under domain `"language"` —
//! **the pack refers, the owner registers** (astronomy's sun/moon/star/
//! night, terrain's stone/mountain/sea, and so on already have a home; this
//! module lists them so the lexicon can name them, but never re-registers
//! them).
//!
//! Two ladders (Berlin & Kay's basic color term acquisition order) gate how
//! much of [`color_pack`] a culture has lexicalized, keyed by
//! [`PackDepths`] and read with [`in_ladder`]. [`compound_recipe`] is the
//! closed authored table of KNOWS-OF compounds: concepts with no root word
//! of their own, expressed instead as a modifier+head compound built from
//! two rooted concepts (e.g. `sea` = "many water").
#![warn(missing_docs)]

use hornvale_kernel::{ConceptKind, ConceptRegistry, RegistryError};

/// One entry in a vocabulary pack: a concept id, its broad category, a doc,
/// and its rank on whichever acquisition ladder it belongs to (0 for
/// entries outside any ladder — always in the lexicon once the pack is
/// registered).
/// type-audit: bare-ok(identifier-text: concept), bare-ok(prose: doc), bare-ok(count: ladder_rank)
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct PackEntry {
    /// The concept id (the registry key, and the compound-recipe key).
    pub concept: &'static str,
    /// The broad category this concept belongs to.
    pub kind: ConceptKind,
    /// Human-readable description, passed through to the concept registry.
    pub doc: &'static str,
    /// Rank on this entry's acquisition ladder ([`color_pack`]'s hue or
    /// luminance ladder); `0` for unranked entries, which are always in the
    /// lexicon regardless of depth.
    pub ladder_rank: u8,
}

/// The universal stratum: the handful of concepts every culture names
/// first, regardless of climate, biome, or species — water, stone, sky
/// bodies, fire, wind, the ground, a tree, the basic acts of living and
/// dying, one's own name, and the first few counts. Some of these concepts
/// are owned by other domains (`sun`, `moon`, `star`, `night` by astronomy;
/// `stone` by terrain) and are listed here only so the lexicon can refer to
/// them; `register_concepts` skips re-registering them.
pub fn universal_stratum() -> &'static [PackEntry] {
    const KIND: ConceptKind = ConceptKind::Substance;
    &[
        PackEntry {
            concept: "water",
            kind: KIND,
            doc: "the drinkable liquid",
            ladder_rank: 0,
        },
        PackEntry {
            concept: "stone",
            kind: ConceptKind::Substance,
            doc: "rock",
            ladder_rank: 0,
        },
        PackEntry {
            concept: "sun",
            kind: ConceptKind::Celestial,
            doc: "the sun",
            ladder_rank: 0,
        },
        PackEntry {
            concept: "moon",
            kind: ConceptKind::Celestial,
            doc: "a moon",
            ladder_rank: 0,
        },
        PackEntry {
            concept: "star",
            kind: ConceptKind::Celestial,
            doc: "a fixed point of light in the night sky",
            ladder_rank: 0,
        },
        PackEntry {
            concept: "night",
            kind: ConceptKind::Celestial,
            doc: "the dark half of the day-night cycle",
            ladder_rank: 0,
        },
        PackEntry {
            concept: "day",
            kind: ConceptKind::Celestial,
            doc: "the light half of the day-night cycle",
            ladder_rank: 0,
        },
        PackEntry {
            concept: "fire",
            kind: ConceptKind::Substance,
            doc: "flame and heat",
            ladder_rank: 0,
        },
        PackEntry {
            concept: "wind",
            kind: ConceptKind::Substance,
            doc: "moving air",
            ladder_rank: 0,
        },
        PackEntry {
            concept: "earth",
            kind: ConceptKind::Terrain,
            doc: "the ground underfoot",
            ladder_rank: 0,
        },
        PackEntry {
            concept: "tree",
            kind: ConceptKind::Living,
            doc: "a woody plant",
            ladder_rank: 0,
        },
        PackEntry {
            concept: "eat",
            kind: ConceptKind::Quality,
            doc: "to consume food",
            ladder_rank: 0,
        },
        PackEntry {
            concept: "sleep",
            kind: ConceptKind::Quality,
            doc: "to rest unconscious",
            ladder_rank: 0,
        },
        PackEntry {
            concept: "die",
            kind: ConceptKind::Quality,
            doc: "to cease living",
            ladder_rank: 0,
        },
        PackEntry {
            concept: "name",
            kind: ConceptKind::Social,
            doc: "a word that identifies one who bears it",
            ladder_rank: 0,
        },
        PackEntry {
            concept: "one",
            kind: ConceptKind::Quality,
            doc: "the cardinal number 1",
            ladder_rank: 0,
        },
        PackEntry {
            concept: "two",
            kind: ConceptKind::Quality,
            doc: "the cardinal number 2",
            ladder_rank: 0,
        },
        PackEntry {
            concept: "many",
            kind: ConceptKind::Quality,
            doc: "an indefinitely large count",
            ladder_rank: 0,
        },
    ]
}

/// The luminance-ladder concept ids within [`color_pack`], as opposed to
/// the hue-ladder ids. [`in_ladder`] gates these against
/// [`PackDepths::luminance`] rather than [`PackDepths::hue`].
const LUMINANCE_CONCEPTS: &[&str] = &["gloom", "shadow", "starlit"];

/// The color pack: the hue ladder (Berlin & Kay's basic color term
/// acquisition stages — dark/light, then red, then green and yellow, then
/// blue, then brown) and a parallel luminance ladder for the ambient dark
/// (gloom, shadow, starlit) that a culture without much daylight lexicalizes
/// on its own schedule. All entries are [`ConceptKind::Quality`].
pub fn color_pack() -> &'static [PackEntry] {
    const KIND: ConceptKind = ConceptKind::Quality;
    &[
        PackEntry {
            concept: "dark",
            kind: KIND,
            doc: "the color term for black/dark hues",
            ladder_rank: 1,
        },
        PackEntry {
            concept: "light",
            kind: KIND,
            doc: "the color term for white/light hues",
            ladder_rank: 1,
        },
        PackEntry {
            concept: "red",
            kind: KIND,
            doc: "the color term for red",
            ladder_rank: 2,
        },
        PackEntry {
            concept: "green",
            kind: KIND,
            doc: "the color term for green",
            ladder_rank: 3,
        },
        PackEntry {
            concept: "yellow",
            kind: KIND,
            doc: "the color term for yellow",
            ladder_rank: 3,
        },
        PackEntry {
            concept: "blue",
            kind: KIND,
            doc: "the color term for blue",
            ladder_rank: 4,
        },
        PackEntry {
            concept: "brown",
            kind: KIND,
            doc: "the color term for brown",
            ladder_rank: 5,
        },
        PackEntry {
            concept: "gloom",
            kind: KIND,
            doc: "the deepest, starless dark",
            ladder_rank: 1,
        },
        PackEntry {
            concept: "shadow",
            kind: KIND,
            doc: "cast dark, distinct from open gloom",
            ladder_rank: 2,
        },
        PackEntry {
            concept: "starlit",
            kind: KIND,
            doc: "dark faintly lit by stars",
            ladder_rank: 3,
        },
    ]
}

/// The body pack: shared humanoid anatomy, model-carded and banked to
/// BIO-1. All entries are [`ConceptKind::Body`] and unranked.
pub fn body_pack() -> &'static [PackEntry] {
    const KIND: ConceptKind = ConceptKind::Body;
    &[
        PackEntry {
            concept: "eye",
            kind: KIND,
            doc: "the organ of sight",
            ladder_rank: 0,
        },
        PackEntry {
            concept: "mouth",
            kind: KIND,
            doc: "the organ of eating and speech",
            ladder_rank: 0,
        },
        PackEntry {
            concept: "hand",
            kind: KIND,
            doc: "the manipulating limb-end",
            ladder_rank: 0,
        },
        PackEntry {
            concept: "foot",
            kind: KIND,
            doc: "the walking limb-end",
            ladder_rank: 0,
        },
        PackEntry {
            concept: "blood",
            kind: KIND,
            doc: "the circulating fluid of a body",
            ladder_rank: 0,
        },
        PackEntry {
            concept: "bone",
            kind: KIND,
            doc: "the rigid frame of a body",
            ladder_rank: 0,
        },
    ]
}

/// The kin pack: the three relations every kinship system distinguishes,
/// regardless of how it further splits them. All entries are
/// [`ConceptKind::Kin`] and unranked.
pub fn kin_pack() -> &'static [PackEntry] {
    const KIND: ConceptKind = ConceptKind::Kin;
    &[
        PackEntry {
            concept: "parent",
            kind: KIND,
            doc: "one's father or mother",
            ladder_rank: 0,
        },
        PackEntry {
            concept: "child",
            kind: KIND,
            doc: "one's son or daughter",
            ladder_rank: 0,
        },
        PackEntry {
            concept: "sibling",
            kind: KIND,
            doc: "one's brother or sister",
            ladder_rank: 0,
        },
    ]
}

/// Whether `concept` is **core** vocabulary — a member of the always-
/// lexicalized Swadesh strata (the universal stratum, the body pack, the kin
/// pack), where homophony genuinely confuses a reader. The ranked color
/// ladder ([`color_pack`]) and the exposure-gated biome concepts are
/// periphery, where incidental homophony is tolerable. The homophony fix
/// assigns core concepts their proto-roots first — so they win the short,
/// distinct forms — and holds a core root to a minimal-pair distance from
/// every other core root. The split is pack membership, never a doc-string
/// heuristic; the lab's `core-homophony-*` metrics measure against this same
/// definition.
pub fn is_core_concept(concept: &str) -> bool {
    universal_stratum()
        .iter()
        .chain(body_pack())
        .chain(kin_pack())
        .any(|e| e.concept == concept)
}

/// The closed authored recipe table for KNOWS-OF compounds: concepts with
/// no root word of their own in any pack, expressed instead as a
/// `(modifier, head)` compound of two concept ids that *do* have roots
/// (whether in a pack above or owned by another domain). `None` means
/// `concept` cannot compound — it must already be a root, or it is a gap
/// this lexicon cannot yet name.
///
/// `sea` has no Swadesh root (it isn't in [`universal_stratum`]) but is
/// owned by terrain as a distinct concept from `water`; a culture without a
/// dedicated word for it names it "many water". `mountain`, likewise owned
/// by terrain, is named "many stone".
/// type-audit: bare-ok(identifier-text)
pub fn compound_recipe(concept: &str) -> Option<(&'static str, &'static str)> {
    RECIPES
        .iter()
        .find(|(c, _, _)| *c == concept)
        .map(|(_, modifier, head)| (*modifier, *head))
}

/// The authored recipe rows behind [`compound_recipe`]:
/// `(concept, modifier, head)`. One table drives both the lookup and the
/// recipe-closure test, so a new recipe can never silently escape test
/// coverage. Closed and tiny; a linear scan is deterministic and cheap.
const RECIPES: &[(&str, &str, &str)] = &[("sea", "many", "water"), ("mountain", "many", "stone")];

/// Input to [`in_ladder`]: how many acquisition-ladder stages are unlocked,
/// per ladder in [`color_pack`]. Derivation from a culture's perception
/// vector lives in worldgen (Task 8) — this struct is just the input shape.
/// type-audit: bare-ok(count)
#[derive(Clone, Copy, Debug, PartialEq, Eq, Default)]
pub struct PackDepths {
    /// Hue-ladder depth: dark/light (1) through brown (5).
    pub hue: u8,
    /// Luminance-ladder depth: gloom (1) through starlit (3).
    pub luminance: u8,
}

/// Whether `entry` is in the lexicon at `depths`. Unranked entries
/// (`ladder_rank == 0` — the universal stratum, body pack, and kin pack)
/// are always in. Ranked entries from [`color_pack`]'s luminance ladder
/// (gloom/shadow/starlit) gate against [`PackDepths::luminance`]; every
/// other ranked entry (the hue ladder) gates against [`PackDepths::hue`].
/// type-audit: bare-ok(flag)
pub fn in_ladder(entry: &PackEntry, depths: &PackDepths) -> bool {
    if entry.ladder_rank == 0 {
        return true;
    }
    if LUMINANCE_CONCEPTS.contains(&entry.concept) {
        entry.ladder_rank <= depths.luminance
    } else {
        entry.ladder_rank <= depths.hue
    }
}

/// Register every pack entry not already owned by another domain, under
/// domain `"language"`. A concept already present in `registry` — the
/// owning domain got there first (astronomy's sun/moon/star/night,
/// climate's snow/rain/ice and biomes, terrain's stone/mountain/sea,
/// species' goblin-kind/kobold-kind, settlement's home/hearth, religion's
/// god/spirit) — is skipped rather than re-registered, so this function is
/// order-independent: it may run before or after the other domains'
/// `register_concepts` in `register_all` without conflict.
pub fn register_concepts(registry: &mut ConceptRegistry) -> Result<(), RegistryError> {
    let packs = universal_stratum()
        .iter()
        .chain(color_pack())
        .chain(body_pack())
        .chain(kin_pack());
    for entry in packs {
        if registry.concept(entry.concept).is_some() {
            continue;
        }
        registry.register_concept(entry.concept, "language", entry.kind, entry.doc)?;
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn water_is_registered_with_domain_language() {
        let mut r = ConceptRegistry::default();
        register_concepts(&mut r).unwrap();
        let def = r
            .concept("water")
            .unwrap_or_else(|| panic!("water should be registered"));
        assert_eq!(def.domain, "language");
    }

    #[test]
    fn blue_is_gated_by_hue_depth() {
        let blue = color_pack()
            .iter()
            .find(|e| e.concept == "blue")
            .unwrap_or_else(|| panic!("color_pack should contain blue"));
        assert_eq!(blue.ladder_rank, 4);
        assert!(
            !in_ladder(
                blue,
                &PackDepths {
                    hue: 3,
                    luminance: 5
                }
            ),
            "blue (rank 4) should be out at hue depth 3"
        );
        assert!(
            in_ladder(
                blue,
                &PackDepths {
                    hue: 4,
                    luminance: 5
                }
            ),
            "blue (rank 4) should be in at hue depth 4"
        );
    }

    #[test]
    fn every_recipe_ingredient_is_a_registered_concept() {
        let mut r = ConceptRegistry::default();
        register_concepts(&mut r).unwrap();
        for (concept, _, _) in RECIPES {
            let (modifier, head) = compound_recipe(concept)
                .unwrap_or_else(|| panic!("{concept} should have a recipe"));
            assert!(
                r.concept(modifier).is_some(),
                "recipe modifier '{modifier}' for '{concept}' is not a registered concept"
            );
            assert!(
                r.concept(head).is_some(),
                "recipe head '{head}' for '{concept}' is not a registered concept"
            );
        }
    }

    #[test]
    fn registering_after_astronomy_does_not_conflict_on_sun() {
        let mut r = ConceptRegistry::default();
        r.register_concept("sun", "astronomy", ConceptKind::Celestial, "the sun")
            .unwrap();
        register_concepts(&mut r).unwrap();
        assert_eq!(
            r.concept("sun").unwrap().domain,
            "astronomy",
            "language must not re-register a concept another domain already owns"
        );
    }
}

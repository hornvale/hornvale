//! Cross-crate coherence guardrail for the dissolved god-struct: the component
//! registries (possibly in different crates) agree on one entity set and one
//! peopled key-set. The Task-4 transcription this once compared field-by-field
//! against the god-struct is now permanently locked by the seed-42
//! byte-identity oracle; this file keeps only the cross-crate coherence the
//! oracle does not directly witness. Lives in worldgen because only a window
//! may depend on more than one domain.
use std::collections::BTreeMap;

use hornvale_worldgen::components::WorldComponents;

#[test]
fn assemble_holds_every_kind_and_passes_integrity() {
    let wc = WorldComponents::assemble().expect("well-formed roster");
    // biosphere = the canonical entity set (all 16 kinds today).
    assert_eq!(wc.biosphere.len(), 16);
    // Nested capacities (The Eremite): perception ⊆ psyche — the peoples carry
    // both, the three dragons carry a mind but no perception — and psyche ⊆
    // biosphere.
    for k in wc.perception.ids() {
        assert!(wc.psyche.contains(k), "perceiver {k:?} carries a mind");
    }
    assert_eq!(wc.psyche.len(), 7, "four peoples + three minded dragons");
    assert_eq!(wc.perception.len(), 4, "perception is the four peoples");
    for k in wc.psyche.ids() {
        assert!(
            wc.biosphere.contains(k),
            "minded kind {k:?} lacks a biosphere row"
        );
    }
}

#[test]
fn language_speech_registries_cover_exactly_the_peopled_kinds() {
    let wc = WorldComponents::assemble().expect("well-formed roster");
    let art = hornvale_language::articulation_registry();
    let lex = hornvale_language::lexicon_registry();
    // The Solitary Tongue: the three chromatic dragons now speak (a frozen
    // Draconic tongue) though they do not perceive (perception stays the four
    // peoples, deferred). Articulation and lexicon are keyed to exactly the
    // MINDED kinds — the psyche key-set (four peoples + three dragons, 7) —
    // which is now a STRICT SUPERSET of the four-people perception key-set;
    // perception ⊆ articulation holds, articulation == perception no longer
    // does.
    let minded: Vec<_> = wc.psyche.ids().collect();
    assert_eq!(
        art.ids().collect::<Vec<_>>(),
        minded,
        "articulation must key exactly the minded kinds (peoples + dragons)"
    );
    assert_eq!(
        lex.ids().collect::<Vec<_>>(),
        minded,
        "lexicon must key exactly the minded kinds (peoples + dragons)"
    );
    // perception (the four peoples) is a subset of articulation (the seven
    // minded speakers) — a dragon speaks but does not (yet) perceive.
    for kind in wc.perception.ids() {
        assert!(
            art.contains(kind),
            "perceiving kind {kind:?} must also speak (perception ⊆ articulation)"
        );
    }
    // A non-minded kind (ordinary fauna) carries no lexicon.
    for kind in wc.biosphere.ids() {
        if !art.contains(kind) {
            assert!(
                lex.get(kind).is_none(),
                "a non-speaker {kind:?} has no lexicon either"
            );
        }
    }
    // Every family proto belongs to a family with more than one member across
    // the full entity set (a singleton family's proto is itself and is absent
    // from the store); members may be peopled or fauna.
    let proto = hornvale_language::family_proto();
    let family_of = hornvale_species::family_of();
    assert!(!proto.is_empty(), "at least one multi-member family exists");
    for family_kind in proto.ids() {
        let members = family_of
            .iter()
            .filter(|(_, f)| **f == family_kind.0)
            .count();
        assert!(
            members > 1,
            "family proto {family_kind:?} must have more than one member"
        );
    }
}

#[test]
fn every_multi_member_family_has_a_proto() {
    // The converse of the check above: every family label shared by two or
    // more kinds (peopled or fauna) must have a proto vector. Without this,
    // a future edit could add a second member to a fauna-only family and
    // forget its proto — fauna don't speak, so nothing panics, and the
    // daughters would silently become isolated languages instead of
    // inheriting from a shared family.
    let family_of = hornvale_species::family_of();
    let proto = hornvale_language::family_proto();

    let mut counts: BTreeMap<&str, usize> = BTreeMap::new();
    for (_, family) in family_of.iter() {
        *counts.entry(*family).or_insert(0) += 1;
    }

    for (family, count) in &counts {
        if *count >= 2 {
            assert!(
                proto.contains(&hornvale_kernel::KindId(family)),
                "family {family:?} has {count} members but no family_proto entry"
            );
        }
    }
}

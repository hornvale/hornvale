//! Cross-crate coherence guardrail for the dissolved god-struct: the component
//! registries (possibly in different crates) agree on one entity set and one
//! peopled key-set. The Task-4 transcription this once compared field-by-field
//! against the god-struct is now permanently locked by the seed-42
//! byte-identity oracle; this file keeps only the cross-crate coherence the
//! oracle does not directly witness. Lives in worldgen because only a window
//! may depend on more than one domain.
use hornvale_worldgen::components::WorldComponents;

#[test]
fn assemble_holds_every_kind_and_passes_integrity() {
    let wc = WorldComponents::assemble().expect("well-formed roster");
    // biosphere = the canonical entity set (all 16 kinds today).
    assert_eq!(wc.biosphere.len(), 16);
    // peopled cluster is coherent: psyche and perception share one key-set,
    // a subset of biosphere.
    let psy: Vec<_> = wc.psyche.ids().collect();
    let per: Vec<_> = wc.perception.ids().collect();
    assert_eq!(psy, per);
    assert_eq!(wc.psyche.len(), 4);
    for k in wc.psyche.ids() {
        assert!(
            wc.biosphere.contains(k),
            "peopled kind {k:?} lacks a biosphere row"
        );
    }
}

#[test]
fn language_speech_registries_cover_exactly_the_peopled_kinds() {
    let wc = WorldComponents::assemble().expect("well-formed roster");
    let art = hornvale_language::articulation_registry();
    let lex = hornvale_language::lexicon_registry();
    // Articulation and lexicon are keyed to exactly the peopled kinds — the
    // same key-set as psyche — and to nothing else.
    let peopled: Vec<_> = wc.psyche.ids().collect();
    assert_eq!(
        art.ids().collect::<Vec<_>>(),
        peopled,
        "articulation must key exactly the peopled kinds"
    );
    assert_eq!(
        lex.ids().collect::<Vec<_>>(),
        peopled,
        "lexicon must key exactly the peopled kinds"
    );
    // Fauna (biosphere rows without a psyche row) carry neither.
    for kind in wc.biosphere.ids() {
        if !wc.psyche.contains(kind) {
            assert!(
                art.get(kind).is_none(),
                "fauna {kind:?} has no articulation"
            );
            assert!(lex.get(kind).is_none(), "fauna {kind:?} has no lexicon");
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

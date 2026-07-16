//! Cross-crate equivalence guardrail for the SpeciesDef dissolution: the
//! component registries (possibly in different crates) reassemble the same
//! kind data the god-struct held. Lives in worldgen because only a window may
//! depend on more than one domain.
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
fn language_speech_registries_equal_the_god_struct_speech() {
    let god = hornvale_species::registry();
    let art = hornvale_language::articulation_registry();
    let lex = hornvale_language::lexicon_registry();
    for (kind, def) in god.iter() {
        match &def.peopled {
            Some(p) => {
                let a = art.get(kind).expect("peopled kind has articulation");
                // field-by-field equality across the crate boundary
                assert_eq!(a.labiality, p.articulation.labiality, "labiality {kind:?}");
                assert_eq!(a.vowel_space, p.articulation.vowel_space);
                assert_eq!(a.voicing, p.articulation.voicing);
                assert_eq!(a.sibilance, p.articulation.sibilance);
                assert_eq!(a.voice_loudness, p.articulation.voice_loudness);
                assert_eq!(a.tonality, p.articulation.tonality);
                assert_eq!(a.exotic as u8, p.articulation.exotic as u8);
                let l = lex.get(kind).expect("peopled kind has lexicon");
                assert_eq!(l.noun, p.noun, "noun {kind:?}");
                assert_eq!(
                    l.worker_override, p.worker_override,
                    "worker_override {kind:?}"
                );
                assert_eq!(l.warrior, p.warrior, "warrior {kind:?}");
                assert_eq!(l.artisan, p.artisan, "artisan {kind:?}");
                assert_eq!(l.shaman, p.shaman, "shaman {kind:?}");
                assert_eq!(l.top, p.top, "top {kind:?}");
            }
            None => {
                assert!(
                    art.get(kind).is_none(),
                    "fauna {kind:?} has no articulation"
                );
                assert!(lex.get(kind).is_none(), "fauna {kind:?} has no lexicon");
            }
        }
    }
    // family protos: every multi-member family, all 7 articulation fields.
    let proto = hornvale_language::family_proto();
    let families = hornvale_species::family_registry();
    assert_eq!(
        proto.len(),
        families.len(),
        "family_proto and family_registry must have the same entries"
    );
    for (family_kind, species_proto) in families.iter() {
        let lang_proto = proto
            .get(family_kind)
            .unwrap_or_else(|| panic!("family {family_kind:?} has a language proto"));
        assert_eq!(
            lang_proto.labiality, species_proto.labiality,
            "labiality {family_kind:?}"
        );
        assert_eq!(
            lang_proto.vowel_space, species_proto.vowel_space,
            "vowel_space {family_kind:?}"
        );
        assert_eq!(
            lang_proto.voicing, species_proto.voicing,
            "voicing {family_kind:?}"
        );
        assert_eq!(
            lang_proto.sibilance, species_proto.sibilance,
            "sibilance {family_kind:?}"
        );
        assert_eq!(
            lang_proto.voice_loudness, species_proto.voice_loudness,
            "voice_loudness {family_kind:?}"
        );
        assert_eq!(
            lang_proto.tonality, species_proto.tonality,
            "tonality {family_kind:?}"
        );
        assert_eq!(
            lang_proto.exotic as u8, species_proto.exotic as u8,
            "exotic {family_kind:?}"
        );
    }
}

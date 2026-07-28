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
    // biosphere = the canonical entity set (all 29 kinds today — The Vacancy
    // T7 added seven, T8 added five (four marine plus the amphibious giant
    // crocodile), T9 added the gnoll).
    assert_eq!(wc.biosphere.len(), 29);
    // Nested capacities (The Eremite, tightened by The Vigil): perception ⊆
    // psyche — every perceiver is minded — and psyche ⊆ biosphere. Since The
    // Vigil the dragons perceive too, so perception and psyche coincide at
    // eight; the subset assertion is kept (not replaced by equality) because a
    // future non-speaking perceiver — an owl with eyes and no words — must
    // stay expressible.
    for k in wc.perception.ids() {
        assert!(wc.psyche.contains(k), "perceiver {k:?} carries a mind");
    }
    assert_eq!(wc.psyche.len(), 8, "five peoples + three minded dragons");
    assert_eq!(
        wc.perception.len(),
        8,
        "the five peoples + the three dragons perceive (The Vigil)"
    );
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
    // The Solitary Tongue gave the three chromatic dragons a frozen Draconic
    // tongue; The Vigil gave them eyes. Articulation and lexicon are keyed to
    // exactly the MINDED kinds — the psyche key-set (five peoples + three
    // dragons, 8) — and perception now coincides with them.
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
    // The Vigil: `check_integrity` (components.rs) already enforces speech ⊆
    // perception at load time — `WorldComponents::assemble()` above would
    // have failed if any speaker lacked perception, so re-deriving that same
    // subset relation here would just restate an enforced rule and pass
    // vacuously every time `assemble()` succeeds. What `check_integrity`
    // does NOT pin is which eight kinds occupy the roster today — a
    // subset-only invariant is silent on names. Assert the roster fact
    // instead: at THIS commit, perception coincides with articulation
    // exactly, by name — the five settling peoples plus the three chromatic
    // dragons — spelled out so a future non-speaking perceiver (an owl with
    // eyes and no words) reads as a real change to this list, not a passing
    // test that never looked.
    let named_roster: Vec<hornvale_kernel::KindId> = [
        "black-dragon",
        "bugbear",
        "gnoll",
        "goblin",
        "hobgoblin",
        "kobold",
        "red-dragon",
        "white-dragon",
    ]
    .into_iter()
    .map(hornvale_kernel::KindId)
    .collect();
    let perceivers: Vec<_> = wc.perception.ids().copied().collect();
    assert_eq!(
        perceivers, named_roster,
        "perception must key exactly the five peoples + three dragons, by name (The Vigil)"
    );
    assert_eq!(
        art.ids().copied().collect::<Vec<_>>(),
        named_roster,
        "articulation must key the same named roster as perception"
    );
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

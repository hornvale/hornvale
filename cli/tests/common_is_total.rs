//! Common's totality, checked against the REAL registry.
//!
//! `realize_common` is infallible because every registered concept resolves
//! to a Common word. `domains/language` cannot check that itself: layering
//! forbids it from reaching `hornvale-worldgen`, even as a dev-dependency, so
//! its own coverage test runs against a reproduced snapshot of the concept
//! ids — a copy that drifts the moment a domain registers a new one. `cli/`
//! is this workspace's home for cross-cutting enforcement and the one place
//! the fully composed registry is reachable, so the invariant is asserted
//! here, where a new concept with no Common word fails the commit gate.

use hornvale_kernel::ConceptRegistry;
use hornvale_language::CommonVocabulary;
use std::collections::BTreeMap;

/// Every registered concept is **well-formed**: the naming convention derives
/// a word from its id without leaving degenerate whitespace or a leftover
/// separator behind.
///
/// It proves nothing about whether any of them is **well-worded**.
/// `sun-like-star` derives to "sun like star" and passes this check cleanly;
/// only a declared exception (`hornvale_astronomy::common_words`, layered on
/// by `hornvale_worldgen::common_vocabulary`) fixes that, and no mechanical
/// test can find the next one. This is the floor, not the standard.
///
/// It is also a check on the REGISTRY, not on everything `word_for` is ever
/// called with. The render path resolves plenty of strings that are not
/// registered concepts — `is-a` objects, `instance-of` `KindId` labels, a
/// chorus account's carving (`windows/book/src/lib.rs`) — and none of those
/// pass through here. `word_for` is total for any input, so they cannot fail;
/// they simply are not what this test ranges over.
#[test]
fn every_registered_concept_has_a_common_word() {
    let mut registry = ConceptRegistry::default();
    hornvale_worldgen::register_all(&mut registry).expect("the roster registers");
    CommonVocabulary::build(&registry)
        .unwrap_or_else(|e| panic!("the composed registry must be totally sayable in Common: {e}"));
}

/// Group `names` by the Common surface each resolves to, keeping only the
/// surfaces more than one name claims.
fn surface_collisions(vocab: &CommonVocabulary, names: &[String]) -> Vec<String> {
    let mut by_surface: BTreeMap<String, Vec<&String>> = BTreeMap::new();
    for name in names {
        by_surface
            .entry(vocab.word_for(name))
            .or_default()
            .push(name);
    }
    by_surface
        .iter()
        .filter(|(_, ids)| ids.len() > 1)
        .map(|(surface, ids)| format!("{ids:?} all resolve to {surface:?}"))
        .collect()
}

/// No two REGISTERED CONCEPTS share a Common surface.
///
/// `word_for` is **not injective**, and `parse_common` matches text against
/// each candidate's realized surface — so two names with one surface make the
/// parser recover whichever sorts last in the `BTreeSet`, silently. `rerender`
/// re-realizes the wrong id to identical text and the corpus law compares
/// text, so a vessel's `Knowledge` would record the wrong `is-a` value with
/// every existing test still green.
///
/// The clause layer's round-trip property cannot catch that: it enumerates ids
/// one at a time, and two colliding ids produce the same sentence by
/// definition. Hence this assertion, stated directly.
///
/// **This ranges over the registry only, and that is deliberate.** The species
/// roster's `KindId` labels are a second population that reaches a complement
/// set (an `instance-of` object is a `KindId`, not a concept id), and the union
/// of the two collides for all 29 species *by construction*: a species' concept
/// is named `{kind}-kind`, and `word_for` strips exactly that suffix, so
/// `KindId("goblin")` and concept `"goblin-kind"` both resolve to `"goblin"`.
/// That overlap is the naming convention working as designed, not a defect —
/// what keeps the parser correct is that the two never enter one complement
/// set together, which is what
/// [`a_live_parse_context_has_no_surface_collisions`] asserts.
#[test]
fn no_two_registered_concepts_share_a_common_surface() {
    let mut registry = ConceptRegistry::default();
    hornvale_worldgen::register_all(&mut registry).expect("the roster registers");
    let vocab = hornvale_worldgen::common_vocabulary(&registry);
    // Guards the guard: the detector must be able to detect. The `{kind}` /
    // `{kind}-kind` pair below is the real overlap this file's doc describes,
    // so this doubles as executable proof that it exists.
    let known_pair = ["goblin".to_string(), "goblin-kind".to_string()];
    assert_eq!(
        surface_collisions(&vocab, &known_pair).len(),
        1,
        "the collision detector must flag a known-colliding pair, or every \
         assertion below is vacuous"
    );

    let names: Vec<String> = registry.concepts().map(|c| c.name.clone()).collect();
    let collisions = surface_collisions(&vocab, &names);
    assert!(
        collisions.is_empty(),
        "two registered concepts share one Common surface, so parse_common \
         would silently recover the wrong one: {}. Rename one, or declare a \
         distinct word for it.",
        collisions.join("; ")
    );
}

/// The invariant `parse_common` actually depends on: within one **live**
/// `ParseContext`, every complement resolves to a distinct surface.
///
/// This is the assertion that matters, because the collision hazard is real
/// rather than hypothetical — see
/// [`no_two_registered_concepts_share_a_common_surface`]'s note on the
/// `{kind}` / `{kind}-kind` overlap. Today the two populations stay apart:
/// `hornvale_book::parse_context` fills the set from `is-a` objects,
/// `instance-of` `KindId`s, and chorus carvings, and no `-kind` concept id is
/// any of those. A future `is-a` object or carving naming a `-kind` concept
/// would put both halves of a colliding pair in one set, and this test is what
/// would notice.
///
/// One seed suffices: a context's composition is structural (the planet's
/// kind, the placed peoples' kinds, the universal `earth` carving), not
/// seed-varying.
#[test]
fn a_live_parse_context_has_no_surface_collisions() {
    let world = hornvale_worldgen::build_world(
        hornvale_kernel::Seed(42),
        &hornvale_astronomy::SkyPins::default(),
        hornvale_worldgen::SkyChoice::Generated,
        &hornvale_terrain::TerrainPins::default(),
        &hornvale_worldgen::SettlementPins::default(),
    )
    .unwrap_or_else(|e| panic!("seed 42 builds: {e}"));
    let ctx = hornvale_book::parse_context(&world);
    assert!(
        ctx.complements.len() > 1,
        "the complement set must be non-empty for this check to mean anything"
    );
    let names: Vec<String> = ctx.complements.iter().cloned().collect();
    let collisions = surface_collisions(&ctx.vocabulary, &names);
    assert!(
        collisions.is_empty(),
        "one ParseContext holds two complements with the same Common surface, \
         so parse_common recovers the wrong one silently: {}",
        collisions.join("; ")
    );
}

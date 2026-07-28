//! The correspondence reconciliation drift-check (The Correspondence, Stage 4):
//! the teeth behind the manifestation view. Two invariants over a freshly
//! registered world's manifests:
//!
//! 1. **Trial balance foots.** For each of the three correspondence ledgers
//!    (lexeme, percept, cognition), `covered + Σ(void classes)` equals the
//!    registered-concept count. A concept missing from any ledger column — a
//!    concept with no manifest — fails.
//! 2. **Lexeme reconciliation.** Every concept whose manifest declares its
//!    lexeme edge `Present(Lexicalization::Expected)` must actually be
//!    realizable by the language lexicon: its name is either a pack root (one
//!    of the four Swadesh packs) or a compound recipe. An `Expected` no
//!    lexicon path covers is a *broken promise* and fails — the fix is to make
//!    the declaration honest (`Void::Gap`), never to weaken this test.

use hornvale_kernel::{ConceptRegistry, Correspondent, Lexicalization, Manifest};
use hornvale_language::{color_pack, compound_recipe, is_core_concept};
use hornvale_worldgen::register_all;
use std::collections::BTreeSet;

/// A freshly registered registry — the in-memory manifests are populated
/// (they are `#[serde(skip)]`, so they exist only when a world is built, not
/// when one is loaded from JSON).
fn fresh_registry() -> ConceptRegistry {
    let mut registry = ConceptRegistry::default();
    register_all(&mut registry).expect("register_all should register every domain's concepts");
    registry
}

/// The set of concept names the language lexicon can realize: every **core**
/// concept ([`is_core_concept`] — the universal/body/kin Swadesh strata plus
/// The Wearing's toponymic terrain concepts, which win a root the same way
/// once `windows/worldgen::exposure_of` classifies a culture `Steeped` in
/// them, even though they are not pack members); every [`color_pack`] entry
/// (periphery, but still root-eligible once a culture's hue/luminance ladder
/// reaches it — `build_lexicon`'s Steeped pass roots ANY Steeped concept,
/// core or not); and every concept with a KNOWS-OF compound recipe (realized
/// as a modifier+head compound regardless of core status). This is the
/// authoritative "lexicalizable" set the `Expected` declaration promises
/// against — deferring to [`is_core_concept`] for the core half rather than
/// re-deriving universal/body/kin membership locally keeps this test and the
/// library's own core/periphery split from drifting apart.
fn lexicalizable(name: &str) -> bool {
    is_core_concept(name)
        || color_pack().iter().any(|e| e.concept == name)
        || compound_recipe(name).is_some()
}

#[test]
fn every_registered_concept_has_a_manifest() {
    let registry = fresh_registry();
    let concepts: BTreeSet<&str> = registry.concepts().map(|c| c.name.as_str()).collect();
    let manifested: BTreeSet<&str> = registry
        .manifests()
        .map(|m| m.concept.name.as_str())
        .collect();
    assert_eq!(
        concepts, manifested,
        "every registered concept must carry a correspondence manifest — a \
         concept with no manifest is invisible to every ledger column"
    );
    assert!(!concepts.is_empty(), "the roster should register concepts");
}

#[test]
fn trial_balance_foots_on_every_ledger() {
    let registry = fresh_registry();
    let manifests: Vec<&Manifest> = registry.manifests().collect();
    let total = manifests.len();

    // Count covered edges per ledger; the remainder are voids. If every
    // concept lands in exactly one bucket (covered xor voided), covered plus
    // voided foots to the total — the PROC-11 trial balance. (The payload
    // types differ per ledger, so each count is inlined rather than shared.)
    let lex_covered = manifests
        .iter()
        .filter(|m| matches!(m.lexeme, Correspondent::Present(_)))
        .count();
    let lex_void = manifests
        .iter()
        .filter(|m| matches!(m.lexeme, Correspondent::Absent(_)))
        .count();
    assert_eq!(lex_covered + lex_void, total, "lexeme ledger must foot");

    let per_covered = manifests
        .iter()
        .filter(|m| matches!(m.percept, Correspondent::Present(_)))
        .count();
    let per_void = manifests
        .iter()
        .filter(|m| matches!(m.percept, Correspondent::Absent(_)))
        .count();
    assert_eq!(per_covered + per_void, total, "percept ledger must foot");

    let cog_covered = manifests
        .iter()
        .filter(|m| matches!(m.cognition, Correspondent::Present(_)))
        .count();
    let cog_void = manifests
        .iter()
        .filter(|m| matches!(m.cognition, Correspondent::Absent(_)))
        .count();
    assert_eq!(cog_covered + cog_void, total, "cognition ledger must foot");
}

#[test]
fn every_expected_lexeme_is_actually_lexicalizable() {
    let registry = fresh_registry();
    let broken: Vec<&str> = registry
        .manifests()
        .filter(|m| matches!(m.lexeme, Correspondent::Present(Lexicalization::Expected)))
        .map(|m| m.concept.name.as_str())
        .filter(|name| !lexicalizable(name))
        .collect();
    assert!(
        broken.is_empty(),
        "broken lexeme promises — these concepts declare their lexeme \
         `Expected` but no language pack root or compound recipe realizes \
         them: {broken:?}. Fix the DECLARATION (flip to `Void::Gap`) in the \
         owning domain, never this test."
    );
}

#[test]
fn reconciliation_is_non_vacuous() {
    // Guard against the reconciliation passing because nothing declares
    // `Expected`: at least one concept must, so the check above has teeth.
    let registry = fresh_registry();
    let expected_count = registry
        .manifests()
        .filter(|m| matches!(m.lexeme, Correspondent::Present(Lexicalization::Expected)))
        .count();
    assert!(
        expected_count > 0,
        "some concept must declare an `Expected` lexeme, else the \
         reconciliation is vacuous"
    );
}

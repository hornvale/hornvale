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
use hornvale_language::{body_pack, color_pack, compound_recipe, kin_pack, universal_stratum};
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

/// The set of concept names the language lexicon can realize: the four Swadesh
/// pack rosters (each entry is a root word) plus every concept that has a
/// KNOWS-OF compound recipe (realized as a modifier+head compound). This is
/// the authoritative "lexicalizable" set the `Expected` declaration promises
/// against.
fn lexicalizable(name: &str) -> bool {
    thread_local! {
        static PACK_ROOTS: BTreeSet<&'static str> = universal_stratum()
            .iter()
            .chain(color_pack())
            .chain(body_pack())
            .chain(kin_pack())
            .map(|e| e.concept)
            .collect();
    }
    PACK_ROOTS.with(|roots| roots.contains(name)) || compound_recipe(name).is_some()
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

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

/// Every registered concept is **well-formed**: the naming convention derives
/// a word from its id without leaving degenerate whitespace or a leftover
/// separator behind.
///
/// It proves nothing about whether any of them is **well-worded**.
/// `sun-like-star` derives to "sun like star" and passes this check cleanly;
/// only a declared exception (`hornvale_astronomy::common_words`, layered on
/// by `hornvale_worldgen::common_vocabulary`) fixes that, and no mechanical
/// test can find the next one. This is the floor, not the standard.
#[test]
fn every_registered_concept_has_a_common_word() {
    let mut registry = ConceptRegistry::default();
    hornvale_worldgen::register_all(&mut registry).expect("the roster registers");
    CommonVocabulary::build(&registry)
        .unwrap_or_else(|e| panic!("the composed registry must be totally sayable in Common: {e}"));
}

//! The correspondence manifest: the compile-time record that a concept is
//! carried across the three ledgers it must be present in — lexicon
//! (a word), perception (a phenomenon kind), and cognition (a handle) —
//! or is explicitly, reason-bearingly *absent* from one of them
//! (spec: The Correspondence). The `concept` anchor is always present; each
//! of the three correspondence edges is either `Present(payload)` or
//! `Absent(Void)`, and `Void` is closed so an absence must name why.
//!
//! Stage 1 lands the types only — nothing constructs a `Manifest` yet. The
//! edge-payload types (`Lexicalization`, `PerceptKind`, `CognitiveHandle`)
//! are deliberate placeholders whose full semantics arrive in later stages.
//!
//! # Anti-vacuity
//!
//! Two house tripwires guard against a `Manifest` that silently stops
//! covering a ledger:
//! - a destructuring `#[allow(dead_code)]` fn with every field named and no
//!   `..` (the `biome.rs` catalog tripwire applied to a struct), so adding a
//!   ledger edge breaks compilation until every construction site is
//!   revisited; and
//! - a `compile_fail` doctest on [`Manifest`] proving a literal that omits an
//!   edge does not compile. This is the **first `compile_fail` doctest in the
//!   repo**, blessed by idea-registry PROC-13 as the zero-dep (rustdoc-native,
//!   no trybuild) anti-vacuity exhibit.

use crate::registry::ConceptDef;

/// A compile-time record that one concept has been carried across the three
/// correspondence ledgers, or is reason-bearingly absent from one of them.
///
/// The `concept` field is the always-present anchor; `lexeme`, `percept`, and
/// `cognition` are the three correspondence edges. There is no `Default` and
/// no default-able construction: every edge is a required decision.
///
/// # PROC-13 anti-vacuity exhibit
///
/// A `Manifest` literal that omits an edge does not compile. This
/// `compile_fail` doctest is the repo's first such exhibit (PROC-13):
///
/// ```compile_fail
/// use hornvale_kernel::{
///     ConceptDef, ConceptKind, Correspondent, Lexicalization, Manifest, Void,
/// };
///
/// let concept = ConceptDef {
///     name: "water".into(),
///     domain: "language".into(),
///     kind: ConceptKind::Substance,
///     doc: "the drinkable liquid".into(),
/// };
///
/// // Omits the `cognition` edge — this does not compile (E0063).
/// let _m = Manifest {
///     concept,
///     lexeme: Correspondent::Present(Lexicalization::Expected),
///     percept: Correspondent::Absent(Void::Imperceptible("no percept yet")),
/// };
/// ```
#[derive(Clone, Debug, PartialEq)]
pub struct Manifest {
    /// The always-present anchor: the concept this manifest is about.
    pub concept: ConceptDef,
    /// The lexicon edge: a word realizes the concept, or it is absent.
    pub lexeme: Correspondent<Lexicalization, Void>,
    /// The perception edge: a phenomenon kind realizes it, or it is absent.
    pub percept: Correspondent<PerceptKind, Void>,
    /// The cognition edge: a cognitive handle realizes it, or it is absent.
    pub cognition: Correspondent<CognitiveHandle, Void>,
}

/// One correspondence edge: either present with its realized payload `T`, or
/// absent with a reason `V` (which for a `Manifest` edge is always [`Void`]).
#[derive(Clone, Debug, PartialEq)]
pub enum Correspondent<T, V> {
    /// The concept is realized on this edge, carrying its payload.
    Present(T),
    /// The concept is absent on this edge, carrying the reason.
    Absent(V),
}

/// The closed set of reasons a correspondence edge may be absent. Closed on
/// purpose: an absence must always name *why*, and adding a new reason is a
/// reviewed vocabulary change.
/// type-audit: bare-ok(prose: Unnamed.0), bare-ok(prose: Gap.0), bare-ok(prose: Imperceptible.0), bare-ok(identifier-text: Uncognized.pending_wave)
#[derive(Clone, Debug, PartialEq)]
pub enum Void {
    /// No word realizes the concept (the lexicon edge has nothing to say).
    Unnamed(&'static str),
    /// A deliberate hole in coverage, expected to be filled later.
    Gap(&'static str),
    /// The concept is not perceivable (the perception edge cannot realize it).
    Imperceptible(&'static str),
    /// Cognition for the concept is deferred to a named future wave.
    Uncognized {
        /// The wave in which this edge is expected to be realized.
        pending_wave: &'static str,
    },
}

/// The lexicon edge's payload: how the language ledger realizes the concept.
/// type-audit: bare-ok(identifier-text: Word.0)
#[derive(Clone, Debug, PartialEq)]
pub enum Lexicalization {
    /// A declaration that the language ledger is expected to realize the
    /// concept — the word is owed but not yet a concrete owned string.
    Expected,
    /// The concrete owned word that realizes the concept.
    Word(&'static str),
}

/// The perception edge's payload: the registered phenomenon-kind key that
/// realizes the concept.
/// type-audit: bare-ok(identifier-text)
#[derive(Clone, Debug, PartialEq)]
pub struct PerceptKind(pub String);

/// The cognition edge's payload: an opaque handle into the cognition ledger.
/// A unit placeholder for now — the whole cognition column voids in Stage 1;
/// its real semantics arrive in a later stage.
#[derive(Clone, Debug, PartialEq)]
pub struct CognitiveHandle;

/// Compile-time tripwire: adding a ledger edge (a `Manifest` field) breaks
/// this destructuring — every field is named and there is no `..` — forcing
/// every construction site to be revisited. The `biome.rs` catalog tripwire
/// applied to a struct. Never remove.
#[allow(dead_code)]
fn manifest_edges_must_all_be_handled(m: Manifest) {
    let Manifest {
        concept,
        lexeme,
        percept,
        cognition,
    } = m;
    let _ = (concept, lexeme, percept, cognition);
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::registry::ConceptKind;

    fn a_concept() -> ConceptDef {
        ConceptDef {
            name: "water".to_string(),
            domain: "language".to_string(),
            kind: ConceptKind::Substance,
            doc: "the drinkable liquid".to_string(),
        }
    }

    #[test]
    fn fully_present_manifest_exposes_each_edge() {
        let m = Manifest {
            concept: a_concept(),
            lexeme: Correspondent::Present(Lexicalization::Word("water")),
            percept: Correspondent::Present(PerceptKind("wetness".to_string())),
            cognition: Correspondent::Present(CognitiveHandle),
        };
        assert_eq!(m.concept.name, "water");
        assert_eq!(
            m.lexeme,
            Correspondent::Present(Lexicalization::Word("water"))
        );
        assert_eq!(
            m.percept,
            Correspondent::Present(PerceptKind("wetness".to_string()))
        );
        assert_eq!(m.cognition, Correspondent::Present(CognitiveHandle));
    }

    #[test]
    fn lexeme_can_void_unnamed() {
        let m = Manifest {
            concept: a_concept(),
            lexeme: Correspondent::Absent(Void::Unnamed("no word yet")),
            percept: Correspondent::Present(PerceptKind("wetness".to_string())),
            cognition: Correspondent::Present(CognitiveHandle),
        };
        assert_eq!(
            m.lexeme,
            Correspondent::Absent(Void::Unnamed("no word yet"))
        );
    }

    #[test]
    fn lexeme_expected_is_a_declaration_not_a_word() {
        let m = Manifest {
            concept: a_concept(),
            lexeme: Correspondent::Present(Lexicalization::Expected),
            percept: Correspondent::Absent(Void::Gap("filled later")),
            cognition: Correspondent::Absent(Void::Uncognized {
                pending_wave: "wave-2",
            }),
        };
        assert_eq!(m.lexeme, Correspondent::Present(Lexicalization::Expected));
    }

    #[test]
    fn percept_can_void_imperceptible() {
        let m = Manifest {
            concept: a_concept(),
            lexeme: Correspondent::Present(Lexicalization::Expected),
            percept: Correspondent::Absent(Void::Imperceptible("abstract concept")),
            cognition: Correspondent::Present(CognitiveHandle),
        };
        assert_eq!(
            m.percept,
            Correspondent::Absent(Void::Imperceptible("abstract concept"))
        );
    }

    #[test]
    fn cognition_can_void_uncognized_with_pending_wave() {
        let m = Manifest {
            concept: a_concept(),
            lexeme: Correspondent::Present(Lexicalization::Expected),
            percept: Correspondent::Present(PerceptKind("wetness".to_string())),
            cognition: Correspondent::Absent(Void::Uncognized {
                pending_wave: "wave-3",
            }),
        };
        assert_eq!(
            m.cognition,
            Correspondent::Absent(Void::Uncognized {
                pending_wave: "wave-3"
            })
        );
    }
}

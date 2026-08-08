//! The project vocabulary.
//!
//! Predicate names carry **no epoch suffix** (unlike world stream labels,
//! decision 0006 — those version because a rename corrupts every world; these
//! corrupt nothing). But a rename rewrites every fact carrying the predicate,
//! producing a whole-file diff that destroys `git log -p` for that compaction.
//! So: **a predicate rename is its own commit, touching nothing else**
//! (spec §11).

use hornvale_kernel::registry::ConceptRegistry;

/// The v1 project vocabulary.
pub fn project_registry() -> ConceptRegistry {
    let mut r = ConceptRegistry::default();
    for (name, functional, doc) in [
        ("decision-title", true, "a decision's title line"),
        ("decision-status", true, "accepted | proposed | superseded"),
        (
            "superseded-by",
            true,
            "the decision that supersedes this one",
        ),
        (
            "supersession-scope",
            true,
            "which provisions the supersession covers; absent means all of them",
        ),
        ("intends", false, "a rule the project intends to hold"),
        ("provides", false, "a capability a crate provides"),
        (
            "archival",
            true,
            "this document is history, never governing",
        ),
    ] {
        r.register_predicate(name, functional, doc)
            .expect("v1 vocabulary registers cleanly");
    }
    r
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn every_v1_predicate_is_registered_with_the_right_arity() {
        let r = project_registry();
        for (name, functional) in [
            ("decision-title", true),
            ("decision-status", true),
            ("superseded-by", true),
            ("supersession-scope", true),
            ("intends", false),
            ("provides", false),
            ("archival", true),
        ] {
            let def = r
                .predicate(name)
                .unwrap_or_else(|| panic!("{name} must be registered"));
            assert_eq!(def.functional, functional, "{name} arity");
            assert!(!def.doc.is_empty(), "{name} needs a doc");
        }
    }

    #[test]
    fn supersession_scope_is_functional_so_it_compacts() {
        // 0026 is superseded FOR DECISION RECORDS ONLY; its registry-row
        // provision still stands. The scope must be replaceable in place.
        let r = project_registry();
        assert!(
            r.predicate("supersession-scope")
                .expect("registered")
                .functional
        );
    }
}

//! The concept registry: the negotiated vocabulary boundary between
//! domains (spec §3.1.6). The envelope stays dumb; meaning lives here,
//! reviewed at every campaign close.

use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;

/// Definition of a fact predicate. `functional` means a subject may hold
/// at most one distinct object under this predicate — the contradiction
/// rule the ledger enforces.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct PredicateDef {
    /// Predicate name (map key).
    pub name: String,
    /// At most one distinct object per subject.
    pub functional: bool,
    /// Human-readable description.
    pub doc: String,
}

/// Errors from concept registration.
#[derive(Debug)]
pub enum RegistryError {
    /// A concept was re-registered with a different definition.
    ConflictingDefinition {
        /// Name of the concept whose redefinition conflicted.
        name: String,
    },
}

impl std::fmt::Display for RegistryError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RegistryError::ConflictingDefinition { name } => {
                write!(f, "conflicting redefinition of concept '{name}'")
            }
        }
    }
}

impl std::error::Error for RegistryError {}

/// The growing, reviewed vocabulary: predicates and phenomenon kinds.
/// BTreeMaps keep serialization order deterministic.
#[derive(Clone, Debug, Default, Serialize, Deserialize)]
pub struct ConceptRegistry {
    predicates: BTreeMap<String, PredicateDef>,
    phenomenon_kinds: BTreeMap<String, String>,
}

impl ConceptRegistry {
    /// Idempotent for identical definitions; errors on conflict.
    pub fn register_predicate(
        &mut self,
        name: &str,
        functional: bool,
        doc: &str,
    ) -> Result<(), RegistryError> {
        let def = PredicateDef {
            name: name.to_string(),
            functional,
            doc: doc.to_string(),
        };
        match self.predicates.get(name) {
            Some(existing) if *existing == def => Ok(()),
            Some(_) => Err(RegistryError::ConflictingDefinition {
                name: name.to_string(),
            }),
            None => {
                self.predicates.insert(name.to_string(), def);
                Ok(())
            }
        }
    }

    /// Look up a predicate definition by name.
    pub fn predicate(&self, name: &str) -> Option<&PredicateDef> {
        self.predicates.get(name)
    }

    /// Idempotent for identical docs; errors on conflict.
    pub fn register_phenomenon_kind(&mut self, name: &str, doc: &str) -> Result<(), RegistryError> {
        match self.phenomenon_kinds.get(name) {
            Some(existing) if existing == doc => Ok(()),
            Some(_) => Err(RegistryError::ConflictingDefinition {
                name: name.to_string(),
            }),
            None => {
                self.phenomenon_kinds
                    .insert(name.to_string(), doc.to_string());
                Ok(())
            }
        }
    }

    /// Look up a phenomenon kind's doc by name.
    pub fn phenomenon_kind(&self, name: &str) -> Option<&str> {
        self.phenomenon_kinds.get(name).map(String::as_str)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn registered_predicate_is_retrievable() {
        let mut r = ConceptRegistry::default();
        r.register_predicate("name", true, "canonical name of an entity")
            .unwrap();
        let def = r.predicate("name").unwrap();
        assert!(def.functional);
    }

    #[test]
    fn unknown_predicate_is_none() {
        let r = ConceptRegistry::default();
        assert!(r.predicate("nope").is_none());
    }

    #[test]
    fn identical_reregistration_is_idempotent() {
        let mut r = ConceptRegistry::default();
        r.register_predicate("name", true, "canonical name of an entity")
            .unwrap();
        assert!(
            r.register_predicate("name", true, "canonical name of an entity")
                .is_ok()
        );
    }

    #[test]
    fn conflicting_reregistration_is_an_error() {
        let mut r = ConceptRegistry::default();
        r.register_predicate("name", true, "canonical name of an entity")
            .unwrap();
        let err = r.register_predicate("name", false, "canonical name of an entity");
        assert!(matches!(
            err,
            Err(RegistryError::ConflictingDefinition { .. })
        ));
    }

    #[test]
    fn phenomenon_kinds_register_and_conflict_like_predicates() {
        let mut r = ConceptRegistry::default();
        r.register_phenomenon_kind("celestial-body", "a body visible in the sky")
            .unwrap();
        assert_eq!(
            r.phenomenon_kind("celestial-body"),
            Some("a body visible in the sky")
        );
        assert!(
            r.register_phenomenon_kind("celestial-body", "something else")
                .is_err()
        );
    }

    #[test]
    fn registry_serializes_roundtrip() {
        let mut r = ConceptRegistry::default();
        r.register_predicate("name", true, "canonical name of an entity")
            .unwrap();
        let json = serde_json::to_string(&r).unwrap();
        let r2: ConceptRegistry = serde_json::from_str(&json).unwrap();
        assert!(r2.predicate("name").unwrap().functional);
    }
}

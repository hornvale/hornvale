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

/// Broad category a concept belongs to — the coarse sort used when
/// browsing or cross-referencing the vocabulary.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub enum ConceptKind {
    /// A material or stuff (water, iron, cloth).
    Substance,
    /// A living organism or class thereof.
    Living,
    /// A body or feature of the sky.
    Celestial,
    /// A landform or feature of the ground.
    Terrain,
    /// A social role, institution, or relation.
    Social,
    /// A body part or physiological feature.
    Body,
    /// A kinship relation.
    Kin,
    /// An abstract property or attribute.
    Quality,
}

/// Definition of a named concept: the word-level vocabulary entry
/// distinct from predicates and phenomenon kinds.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct ConceptDef {
    /// Concept name (map key).
    pub name: String,
    /// The domain that owns this concept's definition.
    pub domain: String,
    /// The broad category this concept belongs to.
    pub kind: ConceptKind,
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
    /// Pre-Words saves have no `concepts` key; default to empty on load.
    #[serde(default)]
    concepts: BTreeMap<String, ConceptDef>,
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

    /// Iterate over registered predicates, in name order.
    pub fn predicates(&self) -> impl Iterator<Item = &PredicateDef> {
        self.predicates.values()
    }

    /// Iterate over registered phenomenon kinds as (kind, doc), in name order.
    pub fn phenomenon_kinds(&self) -> impl Iterator<Item = (&str, &str)> {
        self.phenomenon_kinds
            .iter()
            .map(|(k, v)| (k.as_str(), v.as_str()))
    }

    /// Idempotent for identical definitions; errors on conflict.
    pub fn register_concept(
        &mut self,
        name: &str,
        domain: &str,
        kind: ConceptKind,
        doc: &str,
    ) -> Result<(), RegistryError> {
        let def = ConceptDef {
            name: name.to_string(),
            domain: domain.to_string(),
            kind,
            doc: doc.to_string(),
        };
        match self.concepts.get(name) {
            Some(existing) if *existing == def => Ok(()),
            Some(_) => Err(RegistryError::ConflictingDefinition {
                name: name.to_string(),
            }),
            None => {
                self.concepts.insert(name.to_string(), def);
                Ok(())
            }
        }
    }

    /// Look up a concept definition by name.
    pub fn concept(&self, name: &str) -> Option<&ConceptDef> {
        self.concepts.get(name)
    }

    /// Iterate over registered concepts, in name order.
    pub fn concepts(&self) -> impl Iterator<Item = &ConceptDef> {
        self.concepts.values()
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

    #[test]
    fn predicates_iterate_in_name_order() {
        let mut r = ConceptRegistry::default();
        r.register_predicate("zeta", false, "z").unwrap();
        r.register_predicate("alpha", true, "a").unwrap();
        let names: Vec<&str> = r.predicates().map(|p| p.name.as_str()).collect();
        assert_eq!(names, vec!["alpha", "zeta"]);
    }

    #[test]
    fn phenomenon_kinds_iterate_in_name_order() {
        let mut r = ConceptRegistry::default();
        r.register_phenomenon_kind("wind", "moving air").unwrap();
        r.register_phenomenon_kind("aurora", "sky lights").unwrap();
        let kinds: Vec<(&str, &str)> = r.phenomenon_kinds().collect();
        assert_eq!(
            kinds,
            vec![("aurora", "sky lights"), ("wind", "moving air")]
        );
    }

    #[test]
    fn registered_concept_is_retrievable() {
        let mut r = ConceptRegistry::default();
        r.register_concept(
            "water",
            "language",
            ConceptKind::Substance,
            "the drinkable liquid",
        )
        .unwrap();
        assert_eq!(r.concept("water").unwrap().kind, ConceptKind::Substance);
    }

    #[test]
    fn old_registry_json_without_concepts_still_loads() {
        // Pre-Words saves serialized a registry with no `concepts` field.
        let json = r#"{"predicates":{},"phenomenon_kinds":{}}"#;
        let r: ConceptRegistry = serde_json::from_str(json).unwrap();
        assert_eq!(r.concepts().count(), 0);
    }

    #[test]
    fn unknown_concept_is_none() {
        let r = ConceptRegistry::default();
        assert!(r.concept("nope").is_none());
    }

    #[test]
    fn identical_concept_reregistration_is_idempotent() {
        let mut r = ConceptRegistry::default();
        r.register_concept(
            "water",
            "language",
            ConceptKind::Substance,
            "the drinkable liquid",
        )
        .unwrap();
        assert!(
            r.register_concept(
                "water",
                "language",
                ConceptKind::Substance,
                "the drinkable liquid"
            )
            .is_ok()
        );
    }

    #[test]
    fn conflicting_concept_reregistration_is_an_error() {
        let mut r = ConceptRegistry::default();
        r.register_concept(
            "water",
            "language",
            ConceptKind::Substance,
            "the drinkable liquid",
        )
        .unwrap();
        let err = r.register_concept(
            "water",
            "language",
            ConceptKind::Living,
            "the drinkable liquid",
        );
        assert!(matches!(
            err,
            Err(RegistryError::ConflictingDefinition { .. })
        ));
    }

    #[test]
    fn concepts_iterate_in_name_order() {
        let mut r = ConceptRegistry::default();
        r.register_concept("zeta", "language", ConceptKind::Quality, "z")
            .unwrap();
        r.register_concept("alpha", "language", ConceptKind::Quality, "a")
            .unwrap();
        let names: Vec<&str> = r.concepts().map(|c| c.name.as_str()).collect();
        assert_eq!(names, vec!["alpha", "zeta"]);
    }

    #[test]
    fn concept_registry_serializes_roundtrip() {
        let mut r = ConceptRegistry::default();
        r.register_concept(
            "water",
            "language",
            ConceptKind::Substance,
            "the drinkable liquid",
        )
        .unwrap();
        let json = serde_json::to_string(&r).unwrap();
        let r2: ConceptRegistry = serde_json::from_str(&json).unwrap();
        assert_eq!(r2.concept("water").unwrap().kind, ConceptKind::Substance);
    }
}

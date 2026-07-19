//! The concept registry: the negotiated vocabulary boundary between
//! domains (spec §3.1.6). The envelope stays dumb; meaning lives here,
//! reviewed at every campaign close.

use crate::manifest::{Correspondent, Manifest, PerceptKind};
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;

/// Definition of a fact predicate. `functional` means a subject may hold
/// at most one distinct object under this predicate — the contradiction
/// rule the ledger enforces.
/// type-audit: bare-ok(identifier-text: name), bare-ok(flag: functional), bare-ok(prose: doc)
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
/// type-audit: bare-ok(identifier-text: name), bare-ok(identifier-text: domain), bare-ok(prose: doc)
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
/// type-audit: bare-ok(identifier-text)
#[derive(Debug)]
pub enum RegistryError {
    /// A concept was re-registered with a different definition.
    ConflictingDefinition {
        /// Name of the concept whose redefinition conflicted.
        name: String,
    },
    /// A manifest's percept edge named a phenomenon kind that is not registered.
    UnregisteredPerceptKind {
        /// Name of the concept whose manifest carried the dangling percept.
        concept: String,
        /// The phenomenon-kind key that was not found in the registry.
        kind: String,
    },
}

impl std::fmt::Display for RegistryError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RegistryError::ConflictingDefinition { name } => {
                write!(f, "conflicting redefinition of concept '{name}'")
            }
            RegistryError::UnregisteredPerceptKind { concept, kind } => {
                write!(
                    f,
                    "manifest for concept '{concept}' names unregistered phenomenon kind '{kind}'"
                )
            }
        }
    }
}

impl std::error::Error for RegistryError {}

/// The growing, reviewed vocabulary: predicates, phenomenon kinds, and
/// concepts. BTreeMaps keep serialization order deterministic.
#[derive(Clone, Debug, Default, Serialize, Deserialize)]
pub struct ConceptRegistry {
    predicates: BTreeMap<String, PredicateDef>,
    phenomenon_kinds: BTreeMap<String, String>,
    /// Pre-Words saves have no `concepts` key; default to empty on load.
    #[serde(default)]
    concepts: BTreeMap<String, ConceptDef>,
    /// Correspondence manifests keyed by concept name. Never serialized: the
    /// concept itself already lives in `concepts`, and the manifest's
    /// lexeme/percept/cognition edges are compile-time coverage records the
    /// in-memory registry carries for the manifestation view, not save-format
    /// state. `#[serde(skip)]` keeps every saved `World` JSON byte-identical.
    #[serde(skip)]
    manifests: BTreeMap<String, Manifest>,
}

impl ConceptRegistry {
    /// Idempotent for identical definitions; errors on conflict.
    /// type-audit: bare-ok(identifier-text: name), bare-ok(flag: functional), bare-ok(prose: doc)
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
    /// type-audit: bare-ok(identifier-text)
    pub fn predicate(&self, name: &str) -> Option<&PredicateDef> {
        self.predicates.get(name)
    }

    /// Idempotent for identical docs; errors on conflict.
    /// type-audit: bare-ok(identifier-text: name), bare-ok(prose: doc)
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
    /// type-audit: bare-ok(identifier-text)
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

    /// Insert a concept anchor with decision-0025 conflict/idempotency
    /// semantics: idempotent on a byte-identical redefinition,
    /// [`RegistryError::ConflictingDefinition`] on a differing one. The single
    /// concept-insert path shared by every registration, so serialized
    /// `concepts` stays byte-identical however a concept was registered.
    fn insert_concept(&mut self, def: ConceptDef) -> Result<(), RegistryError> {
        match self.concepts.get(&def.name) {
            Some(existing) if *existing == def => Ok(()),
            Some(_) => Err(RegistryError::ConflictingDefinition {
                name: def.name.clone(),
            }),
            None => {
                self.concepts.insert(def.name.clone(), def);
                Ok(())
            }
        }
    }

    /// Register a concept together with its correspondence [`Manifest`] — the
    /// only public path to add a concept to the registry.
    ///
    /// The concept anchor is inserted through [`insert_concept`](Self::
    /// insert_concept) (decision 0025): idempotent on a byte-identical
    /// redefinition, [`RegistryError::ConflictingDefinition`] on a differing
    /// one. If the manifest's percept edge names a phenomenon kind, that kind
    /// must already be registered (else
    /// [`RegistryError::UnregisteredPerceptKind`]). The manifest is retained in
    /// memory — never serialized — for the manifestation view.
    pub fn register_manifest(&mut self, manifest: Manifest) -> Result<(), RegistryError> {
        // Validate the percept edge before mutating anything, so an error
        // leaves the registry untouched (fail fast, no partial state).
        if let Correspondent::Present(PerceptKind(kind)) = &manifest.percept
            && self.phenomenon_kind(kind).is_none()
        {
            return Err(RegistryError::UnregisteredPerceptKind {
                concept: manifest.concept.name.clone(),
                kind: kind.clone(),
            });
        }
        self.insert_concept(manifest.concept.clone())?;
        self.manifests
            .insert(manifest.concept.name.clone(), manifest);
        Ok(())
    }

    /// Look up a concept's correspondence manifest by name.
    /// type-audit: bare-ok(identifier-text)
    pub fn manifest(&self, name: &str) -> Option<&Manifest> {
        self.manifests.get(name)
    }

    /// Iterate over registered manifests, in concept-name order.
    pub fn manifests(&self) -> impl Iterator<Item = &Manifest> {
        self.manifests.values()
    }

    /// Look up a concept definition by name.
    /// type-audit: bare-ok(identifier-text)
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
    use crate::manifest::{Lexicalization, Void};

    /// Build a minimal manifest carrying an arbitrary [`ConceptDef`] — the
    /// register_manifest tests only care about the concept anchor, so the
    /// edges void honestly and uniformly.
    fn manifest_of(def: ConceptDef) -> Manifest {
        Manifest {
            lexeme: Correspondent::Present(Lexicalization::Expected),
            percept: Correspondent::Absent(Void::Gap("not emitted as a phenomenon yet")),
            cognition: Correspondent::Absent(Void::Uncognized {
                pending_wave: "wave-cognition",
            }),
            concept: def,
        }
    }

    fn a_concept_def(name: &str, kind: ConceptKind, doc: &str) -> ConceptDef {
        ConceptDef {
            name: name.to_string(),
            domain: "language".to_string(),
            kind,
            doc: doc.to_string(),
        }
    }

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
        r.register_manifest(manifest_of(a_concept_def(
            "water",
            ConceptKind::Substance,
            "the drinkable liquid",
        )))
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
        r.register_manifest(manifest_of(a_concept_def(
            "water",
            ConceptKind::Substance,
            "the drinkable liquid",
        )))
        .unwrap();
        assert!(
            r.register_manifest(manifest_of(a_concept_def(
                "water",
                ConceptKind::Substance,
                "the drinkable liquid",
            )))
            .is_ok()
        );
    }

    #[test]
    fn conflicting_concept_reregistration_is_an_error() {
        let mut r = ConceptRegistry::default();
        r.register_manifest(manifest_of(a_concept_def(
            "water",
            ConceptKind::Substance,
            "the drinkable liquid",
        )))
        .unwrap();
        let err = r.register_manifest(manifest_of(a_concept_def(
            "water",
            ConceptKind::Living,
            "the drinkable liquid",
        )));
        assert!(matches!(
            err,
            Err(RegistryError::ConflictingDefinition { .. })
        ));
    }

    #[test]
    fn concepts_iterate_in_name_order() {
        let mut r = ConceptRegistry::default();
        r.register_manifest(manifest_of(a_concept_def(
            "zeta",
            ConceptKind::Quality,
            "z",
        )))
        .unwrap();
        r.register_manifest(manifest_of(a_concept_def(
            "alpha",
            ConceptKind::Quality,
            "a",
        )))
        .unwrap();
        let names: Vec<&str> = r.concepts().map(|c| c.name.as_str()).collect();
        assert_eq!(names, vec!["alpha", "zeta"]);
    }

    fn a_manifest(name: &str) -> Manifest {
        Manifest {
            concept: ConceptDef {
                name: name.to_string(),
                domain: "language".to_string(),
                kind: ConceptKind::Substance,
                doc: "the drinkable liquid".to_string(),
            },
            lexeme: Correspondent::Present(crate::manifest::Lexicalization::Expected),
            percept: Correspondent::Absent(crate::manifest::Void::Gap("not emitted yet")),
            cognition: Correspondent::Absent(crate::manifest::Void::Uncognized {
                pending_wave: "wave-cognition",
            }),
        }
    }

    #[test]
    fn register_manifest_inserts_the_concept_and_retains_the_manifest() {
        let mut r = ConceptRegistry::default();
        r.register_manifest(a_manifest("water")).unwrap();
        assert_eq!(r.concept("water").unwrap().kind, ConceptKind::Substance);
        assert!(r.manifest("water").is_some());
    }

    #[test]
    fn register_manifest_inserts_the_manifests_conceptdef_verbatim() {
        let expected = ConceptDef {
            name: "water".to_string(),
            domain: "language".to_string(),
            kind: ConceptKind::Substance,
            doc: "the drinkable liquid".to_string(),
        };
        let mut r = ConceptRegistry::default();
        r.register_manifest(a_manifest("water")).unwrap();
        assert_eq!(
            r.concept("water"),
            Some(&expected),
            "register_manifest must insert the manifest's ConceptDef verbatim"
        );
    }

    #[test]
    fn register_manifest_is_idempotent_on_identical_reregistration() {
        let mut r = ConceptRegistry::default();
        r.register_manifest(a_manifest("water")).unwrap();
        assert!(r.register_manifest(a_manifest("water")).is_ok());
    }

    #[test]
    fn register_manifest_conflicts_on_differing_concept() {
        let mut r = ConceptRegistry::default();
        r.register_manifest(a_manifest("water")).unwrap();
        let mut differing = a_manifest("water");
        differing.concept.kind = ConceptKind::Living;
        assert!(matches!(
            r.register_manifest(differing),
            Err(RegistryError::ConflictingDefinition { .. })
        ));
    }

    #[test]
    fn register_manifest_rejects_unregistered_percept_kind() {
        let mut r = ConceptRegistry::default();
        let mut m = a_manifest("water");
        m.percept = Correspondent::Present(PerceptKind("no-such-kind".to_string()));
        let err = r.register_manifest(m);
        assert!(matches!(
            err,
            Err(RegistryError::UnregisteredPerceptKind { .. })
        ));
        // The failed registration left the registry untouched.
        assert!(r.concept("water").is_none());
    }

    #[test]
    fn register_manifest_accepts_registered_percept_kind() {
        let mut r = ConceptRegistry::default();
        r.register_phenomenon_kind("wetness", "the quality of being wet")
            .unwrap();
        let mut m = a_manifest("water");
        m.percept = Correspondent::Present(PerceptKind("wetness".to_string()));
        assert!(r.register_manifest(m).is_ok());
    }

    #[test]
    fn manifests_iterate_in_name_order() {
        let mut r = ConceptRegistry::default();
        r.register_manifest(a_manifest("zeta")).unwrap();
        r.register_manifest(a_manifest("alpha")).unwrap();
        let names: Vec<&str> = r.manifests().map(|m| m.concept.name.as_str()).collect();
        assert_eq!(names, vec!["alpha", "zeta"]);
    }

    #[test]
    fn concept_registry_serializes_roundtrip() {
        let mut r = ConceptRegistry::default();
        r.register_manifest(manifest_of(a_concept_def(
            "water",
            ConceptKind::Substance,
            "the drinkable liquid",
        )))
        .unwrap();
        let json = serde_json::to_string(&r).unwrap();
        let r2: ConceptRegistry = serde_json::from_str(&json).unwrap();
        assert_eq!(r2.concept("water").unwrap().kind, ConceptKind::Substance);
    }
}

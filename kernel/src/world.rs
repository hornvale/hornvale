//! World assembly: seed + registry + ledger. A saved world IS a seed
//! plus a ledger (Constitution §2.3); providers are stateless in tier 0
//! and reconstructed at load by the application.

use crate::ledger::Ledger;
use crate::registry::ConceptRegistry;
use crate::seed::Seed;
use serde::{Deserialize, Serialize};
use std::path::Path;

/// A world is a seed plus everything ever observed about it.
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct World {
    /// The seed that generated this world.
    pub seed: Seed,
    /// The registry of all concepts (predicates, phenomenon kinds, etc.) in this world.
    pub registry: ConceptRegistry,
    /// The ledger of all facts committed to this world.
    pub ledger: Ledger,
}

impl World {
    /// Create an empty world and register kernel-core concepts.
    /// Domains register their own concepts at wiring time.
    pub fn new(seed: Seed) -> World {
        let mut registry = ConceptRegistry::default();
        registry
            .register_predicate("name", true, "canonical name of an entity")
            .expect("core concept registration cannot conflict in an empty registry");
        World {
            seed,
            registry,
            ledger: Ledger::default(),
        }
    }

    /// Deterministic pretty JSON. This string is the save format and the
    /// determinism-test currency: same world → same bytes.
    pub fn to_json(&self) -> String {
        serde_json::to_string_pretty(self).expect("World serialization cannot fail")
    }

    /// Deserialize a world from JSON.
    pub fn from_json(json: &str) -> Result<World, serde_json::Error> {
        serde_json::from_str(json)
    }

    /// Save this world to a JSON file.
    pub fn save(&self, path: &Path) -> std::io::Result<()> {
        std::fs::write(path, self.to_json())
    }

    /// Load a world from a JSON file.
    pub fn load(path: &Path) -> std::io::Result<World> {
        let json = std::fs::read_to_string(path)?;
        World::from_json(&json).map_err(std::io::Error::other)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ledger::{Fact, Value};

    #[test]
    fn new_world_registers_core_concepts() {
        let w = World::new(Seed(42));
        assert!(w.registry.predicate("name").unwrap().functional);
    }

    #[test]
    fn world_json_roundtrips() {
        let mut w = World::new(Seed(42));
        let e = w.ledger.mint_entity();
        w.ledger
            .commit(
                Fact {
                    subject: e,
                    predicate: "name".to_string(),
                    object: Value::Text("Zaggrak".to_string()),
                    place: None,
                    day: None,
                    provenance: "test".to_string(),
                },
                &w.registry,
            )
            .unwrap();
        let w2 = World::from_json(&w.to_json()).unwrap();
        assert_eq!(w2.seed, Seed(42));
        assert_eq!(w2.ledger.len(), 1);
        assert_eq!(
            w2.ledger.value_of(e, "name"),
            Some(&Value::Text("Zaggrak".to_string()))
        );
    }

    #[test]
    fn world_json_is_deterministic() {
        assert_eq!(World::new(Seed(9)).to_json(), World::new(Seed(9)).to_json());
    }

    #[test]
    fn world_saves_and_loads_from_disk() {
        let dir = std::env::temp_dir().join("hornvale-kernel-test");
        std::fs::create_dir_all(&dir).unwrap();
        let path = dir.join("world.json");
        let w = World::new(Seed(42));
        w.save(&path).unwrap();
        let w2 = World::load(&path).unwrap();
        assert_eq!(w2.seed, Seed(42));
        std::fs::remove_file(&path).unwrap();
    }
}

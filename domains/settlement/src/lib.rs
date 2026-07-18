//! Settlement: generated settlements placed on terrain, each committed as
//! its own place entity tagged with cell/coordinates/biome/name/population.
#![warn(missing_docs)]

pub mod genesis;
pub mod render;

pub use genesis::{PlacedSettlement, genesis};

use hornvale_kernel::{
    ConceptDef, ConceptKind, ConceptRegistry, Correspondent, EntityId, Lexicalization, Manifest,
    RegistryError, Value, Void, World,
};

/// Predicate marking an entity as a settlement.
/// type-audit: bare-ok(identifier-text)
pub const IS_SETTLEMENT: &str = "is-settlement";
/// Predicate giving a settlement's population.
/// type-audit: bare-ok(identifier-text)
pub const POPULATION: &str = "population";
/// Predicate key marking an entity a traversable place (owned/registered by
/// terrain; settlement commits facts against the same key for generated
/// cells).
/// type-audit: bare-ok(identifier-text)
pub const IS_PLACE: &str = "is-place";
/// Predicate key giving a place's biome (shared key; see `IS_PLACE`).
/// type-audit: bare-ok(identifier-text)
pub const BIOME: &str = "biome";
/// Predicate: the Geosphere cell id a settlement sits on (functional,
/// Number).
/// type-audit: bare-ok(identifier-text)
pub const CELL_ID: &str = "cell-id";
/// Predicate: latitude of a settlement, degrees (functional, Number).
/// type-audit: bare-ok(identifier-text)
pub const LATITUDE: &str = "latitude";
/// Predicate: longitude of a settlement, degrees (functional, Number).
/// type-audit: bare-ok(identifier-text)
pub const LONGITUDE: &str = "longitude";
/// Predicate: one round-trippable settlement scenario pin string per pinned
/// value (non-functional, Text) — the settlement sibling of terrain's
/// terrain-pin and sky's scenario-pin.
/// type-audit: bare-ok(identifier-text)
pub const SETTLEMENT_PIN: &str = "settlement-pin";

/// Every seed-derivation label (or pattern) this crate uses, with docs.
/// Slash-joined paths document derivation chains; the manifest renders them.
/// `settlement/name` and `settlement/kobold/name` are retired as live
/// generation paths (The Tongues moves settlement-name generation to
/// `hornvale-language`, which owns the real `language/<species>/name/...`
/// derivation labels, since this domain cannot depend on another domain
/// crate — spec §7); `settlement/placement` and `settlement/kobold/population`
/// are retired as live generation paths for the same reason the-gathering
/// (MAP-7) gives (settlement population is now a `demography` catchment
/// readout, drawn from nothing — see `hornvale_demography::condense` — so
/// this crate's placement/population streams draw nothing either). All four
/// stay documented here forever, never renamed (ADR 0006). No new label is
/// minted here for either move: nothing under `settlement/*` derives a name
/// or a population any longer, so publishing phantom `settlement/name/v2` /
/// `settlement/placement/v2` labels would only mislead a reader of the
/// manifest.
/// type-audit: bare-ok(identifier-text)
pub fn stream_labels() -> Vec<(&'static str, &'static str)> {
    vec![
        ("settlement", "root stream for settlement generation"),
        (
            "settlement/name",
            "RETIRED (pre-Tongues): per-settlement generated name, goblin stream. \
             Names now derive under language/<species>/name/settlement. Kept \
             documented for legacy-save continuity; never renamed.",
        ),
        (
            "settlement/placement",
            "RETIRED (the-gathering): per-settlement population against \
             carrying capacity, goblin stream. Population is now the conserved \
             catchment readout of hornvale-demography's flow-condensation, \
             drawing nothing from the seed. Kept documented for legacy-save \
             continuity; never renamed.",
        ),
        (
            "settlement/kobold/name",
            "RETIRED (pre-Tongues): per-settlement generated name, kobold stream \
             (species-qualified; goblin kept settlement/name). Names now derive \
             under language/<species>/name/settlement. Kept documented for \
             legacy-save continuity; never renamed.",
        ),
        (
            "settlement/kobold/population",
            "RETIRED (the-gathering): per-settlement population, kobold \
             stream (species-qualified; goblin kept settlement/placement). \
             Population is now the conserved catchment readout of \
             hornvale-demography's flow-condensation, drawing nothing from the \
             seed. Kept documented for legacy-save continuity; never renamed.",
        ),
    ]
}

/// Register settlement's contribution to the concept registry.
///
/// The home/hearth concepts register through their correspondence [`Manifest`]:
/// each is an everyday nameable social thing, so its lexeme edge declares
/// `Expected`; settlement emits no phenomenon kind for them, so the percept
/// edge is a `Gap`; and cognition voids to the future cognition wave.
pub fn register_concepts(registry: &mut ConceptRegistry) -> Result<(), RegistryError> {
    registry.register_predicate(IS_SETTLEMENT, true, "subject is a settlement")?;
    registry.register_predicate(POPULATION, true, "population of a settlement")?;
    registry.register_predicate(CELL_ID, true, "Geosphere cell id a settlement sits on")?;
    registry.register_predicate(LATITUDE, true, "settlement latitude, degrees")?;
    registry.register_predicate(LONGITUDE, true, "settlement longitude, degrees")?;
    registry.register_predicate(
        SETTLEMENT_PIN,
        false,
        "a settlement scenario pin, round-trippable",
    )?;

    for (name, doc) in [
        ("home", "one's dwelling"),
        ("hearth", "the fire at the center of a home"),
    ] {
        registry.register_manifest(Manifest {
            concept: ConceptDef {
                name: name.to_string(),
                domain: "settlement".to_string(),
                kind: ConceptKind::Social,
                doc: doc.to_string(),
            },
            lexeme: Correspondent::Present(Lexicalization::Expected),
            percept: Correspondent::Absent(Void::Gap("not emitted as a phenomenon yet")),
            cognition: Correspondent::Absent(Void::Uncognized {
                pending_wave: "wave-cognition",
            }),
        })?;
    }
    Ok(())
}

/// Settlement as a registrable unit for the composition-root roster.
/// type-audit: bare-ok(identifier-text: return)
pub struct Settlement;

impl hornvale_kernel::Domain for Settlement {
    fn crate_name(&self) -> &'static str {
        env!("CARGO_PKG_NAME")
    }
    fn register_concepts(
        &self,
        registry: &mut hornvale_kernel::ConceptRegistry,
    ) -> Result<(), hornvale_kernel::RegistryError> {
        crate::register_concepts(registry)
    }
    fn stream_labels(&self) -> Vec<(&'static str, &'static str)> {
        crate::stream_labels()
    }
}

/// A settlement as this domain knows it.
/// type-audit: bare-ok(identifier-text: name), bare-ok(count: population)
#[derive(Debug, Clone, PartialEq)]
pub struct VillageInfo {
    /// The settlement's entity id.
    pub id: EntityId,
    /// The settlement's canonical name.
    pub name: String,
    /// How many live there.
    pub population: u32,
}

/// Every settlement in the world, in commit order (element 0 is the
/// flagship — the first `is-settlement` fact, per settlement genesis).
pub fn all_settlements(world: &World) -> Vec<VillageInfo> {
    world
        .ledger
        .find(IS_SETTLEMENT)
        .map(|f| f.subject)
        .map(|id| {
            let name = world
                .ledger
                .text_of(id, hornvale_kernel::NAME)
                .map(str::to_string)
                .unwrap_or_else(|| format!("settlement {}", id.0));
            let population = match world.ledger.value_of(id, POPULATION) {
                Some(Value::Number(n)) => *n as u32,
                _ => 0,
            };
            VillageInfo {
                id,
                name,
                population,
            }
        })
        .collect()
}

/// The first settlement in the world, if any.
pub fn village_info(world: &World) -> Option<VillageInfo> {
    all_settlements(world).into_iter().next()
}

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_kernel::Seed;

    fn world(seed: u64) -> World {
        let mut w = World::new(Seed(seed));
        register_concepts(&mut w.registry).unwrap();
        // Settlement commits facts against `is-place`/`biome` without owning
        // their registration (terrain does, at the composition root); stand
        // in for that root here, mirroring terrain's exact definitions.
        w.registry
            .register_predicate(IS_PLACE, true, "subject is a traversable place")
            .unwrap();
        w.registry
            .register_predicate(BIOME, true, "biome of a place")
            .unwrap();
        w
    }

    fn flagship(_seed: Seed, salt: u64) -> PlacedSettlement {
        PlacedSettlement {
            cell: salt as u32,
            latitude: 0.0,
            longitude: 0.0,
            biome: "temperate-forest".to_string(),
            // Name generation itself now lives in hornvale-language (spec
            // §7, wired at the composition root); a fixed test name keeps
            // this crate's own tests independent of that domain. Population is
            // now the demography catchment readout (composition root), so this
            // crate's own tests use a fixed count.
            name: "Testville".to_string(),
            population: 100,
        }
    }

    #[test]
    fn genesis_produces_a_named_populated_village() {
        let mut w = world(42);
        let ids = genesis(&mut w, &[flagship(Seed(42), 0)]).unwrap();
        let info = village_info(&w).expect("village exists");
        assert_eq!(info.id, ids[0]);
        assert!(!info.name.is_empty());
        assert!(info.name.chars().next().unwrap().is_uppercase());
        assert!((40..=500).contains(&info.population));
    }

    #[test]
    fn genesis_is_deterministic() {
        let mut a = world(7);
        let mut b = world(7);
        genesis(&mut a, &[flagship(Seed(7), 0)]).unwrap();
        genesis(&mut b, &[flagship(Seed(7), 0)]).unwrap();
        let ia = village_info(&a).unwrap();
        let ib = village_info(&b).unwrap();
        assert_eq!(ia.name, ib.name);
        assert_eq!(ia.population, ib.population);
    }

    #[test]
    fn no_village_means_none() {
        let w = world(1);
        assert!(village_info(&w).is_none());
    }

    #[test]
    fn all_settlements_lists_every_settlement_flagship_first() {
        let mut w = world(42);
        let one = PlacedSettlement {
            name: "First".to_string(),
            ..flagship(Seed(42), 0)
        };
        let two = PlacedSettlement {
            cell: 1,
            name: "Second".to_string(),
            ..flagship(Seed(42), 1)
        };
        let ids = genesis(&mut w, &[one, two]).unwrap();
        let all = all_settlements(&w);
        assert_eq!(all.len(), 2);
        assert_eq!(all[0].id, ids[0]);
        assert_eq!(all[0].name, "First");
        assert_eq!(all[1].name, "Second");
        assert!(all_settlements(&world(1)).is_empty());
    }

    #[test]
    fn stream_labels_declare_every_derivation() {
        let labels: Vec<&str> = stream_labels().iter().map(|(l, _)| *l).collect();
        for expected in ["settlement", "settlement/name", "settlement/placement"] {
            assert!(labels.contains(&expected), "missing {expected}");
        }
        // The move to hornvale-language mints no phantom settlement/* label:
        // nothing under settlement/* derives a name any longer.
        assert!(
            !labels.contains(&"settlement/name/v2"),
            "settlement/name/v2 is a phantom label — real name derivation lives in language/*"
        );
    }

    #[test]
    fn stream_labels_declare_the_kobold_streams() {
        let labels: Vec<&str> = stream_labels().iter().map(|(l, _)| *l).collect();
        for expected in ["settlement/kobold/name", "settlement/kobold/population"] {
            assert!(labels.contains(&expected), "missing {expected}");
        }
    }

    #[test]
    fn concepts_registered() {
        let mut r = ConceptRegistry::default();
        register_concepts(&mut r).unwrap();
        for name in ["home", "hearth"] {
            let c = r
                .concept(name)
                .unwrap_or_else(|| panic!("missing concept {name}"));
            assert_eq!(c.domain, "settlement");
            assert_eq!(c.kind, ConceptKind::Social);
        }
    }
}

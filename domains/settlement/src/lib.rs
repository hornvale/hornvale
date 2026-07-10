//! Settlement: generated settlements placed on terrain, each committed as
//! its own place entity tagged with cell/coordinates/biome/name/population.
#![warn(missing_docs)]

pub mod genesis;
pub mod placement;
pub mod render;

pub use genesis::{PlacedSettlement, genesis};
pub use placement::{
    BASELINE_WEIGHTS, Placement, SiteInput, SuitabilityWeights, place, place_tagged, suitability,
    suitability_weighted,
};

use hornvale_kernel::{ConceptKind, ConceptRegistry, EntityId, RegistryError, Value, World};

/// Predicate marking an entity as a settlement.
pub const IS_SETTLEMENT: &str = "is-settlement";
/// Predicate giving a settlement's population.
pub const POPULATION: &str = "population";
/// Predicate key marking an entity a traversable place (owned/registered by
/// terrain; settlement commits facts against the same key for generated
/// cells).
pub const IS_PLACE: &str = "is-place";
/// Predicate key giving a place's biome (shared key; see `IS_PLACE`).
pub const BIOME: &str = "biome";
/// Predicate: the Geosphere cell id a settlement sits on (functional,
/// Number).
pub const CELL_ID: &str = "cell-id";
/// Predicate: latitude of a settlement, degrees (functional, Number).
pub const LATITUDE: &str = "latitude";
/// Predicate: longitude of a settlement, degrees (functional, Number).
pub const LONGITUDE: &str = "longitude";
/// Predicate: one round-trippable settlement scenario pin string per pinned
/// value (non-functional, Text) — the settlement sibling of terrain's
/// terrain-pin and sky's scenario-pin.
pub const SETTLEMENT_PIN: &str = "settlement-pin";

/// Seed-derivation labels used by this crate. Labels are permanent
/// save-format contracts (spec §3); regeneration uses epoch suffixes.
mod streams {
    /// Root stream label for settlement.
    pub const ROOT: &str = "settlement";
    /// Per-settlement placement draws (population against carrying
    /// capacity).
    pub const PLACEMENT: &str = "placement";
}

/// Every seed-derivation label (or pattern) this crate uses, with docs.
/// Slash-joined paths document derivation chains; the manifest renders them.
/// `settlement/name` and `settlement/kobold/name` are retired as live
/// generation paths (The Tongues moves settlement-name generation to
/// `hornvale-language`, which owns the real `language/<species>/name/...`
/// derivation labels, since this domain cannot depend on another domain
/// crate — spec §7) but stay documented here forever, never renamed (ADR
/// 0006). No new label is minted here for the move: nothing under
/// `settlement/*` derives a name any longer, so publishing a phantom
/// `settlement/name/v2` would only mislead a reader of the manifest.
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
            "per-settlement population against carrying capacity",
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
            "per-settlement population, kobold stream (species-qualified; goblin keeps settlement/placement)",
        ),
    ]
}

/// Register settlement's contribution to the concept registry.
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

    registry.register_concept("home", "settlement", ConceptKind::Social, "one's dwelling")?;
    registry.register_concept(
        "hearth",
        "settlement",
        ConceptKind::Social,
        "the fire at the center of a home",
    )
}

/// A settlement as this domain knows it.
#[derive(Debug, Clone, PartialEq)]
pub struct VillageInfo {
    /// The settlement's entity id.
    pub id: EntityId,
    /// The settlement's canonical name.
    pub name: String,
    /// How many live there.
    pub population: u32,
}

/// Draw a population against a carrying capacity from suitability, seeded per
/// settlement: `floor + (span x suitability)` jittered by a per-salt draw.
pub fn draw_population(seed: hornvale_kernel::Seed, salt: u64, suitability: f64) -> u32 {
    let mut stream = seed
        .derive(streams::ROOT)
        .derive(streams::PLACEMENT)
        .derive(&salt.to_string())
        .stream();
    let base = 40.0 + 460.0 * suitability.clamp(0.0, 1.0); // 40..500 by suitability
    let jitter = 0.75 + 0.5 * stream.next_f64(); // x[0.75, 1.25]
    (base * jitter).round() as u32
}

/// Draw a non-goblin settlement's population from its species-qualified
/// stream (`settlement/<species>/population`), same capacity curve as the
/// goblin draw.
pub fn draw_species_population(
    seed: hornvale_kernel::Seed,
    species: &str,
    salt: u64,
    suitability: f64,
) -> u32 {
    let mut stream = seed
        .derive(streams::ROOT)
        .derive(species)
        .derive("population")
        .derive(&salt.to_string())
        .stream();
    let base = 40.0 + 460.0 * suitability.clamp(0.0, 1.0);
    let jitter = 0.75 + 0.5 * stream.next_f64();
    (base * jitter).round() as u32
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

    fn flagship(seed: Seed, salt: u64) -> PlacedSettlement {
        PlacedSettlement {
            cell: salt as u32,
            latitude: 0.0,
            longitude: 0.0,
            biome: "temperate-forest".to_string(),
            // Name generation itself now lives in hornvale-language (spec
            // §7, wired at the composition root); a fixed test name keeps
            // this crate's own tests independent of that domain.
            name: "Testville".to_string(),
            population: draw_population(seed, salt, 0.5),
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
    fn populations_vary_with_suitability() {
        let low = draw_population(Seed(3), 0, 0.0);
        let high = draw_population(Seed(3), 0, 1.0);
        assert!(high > low);
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
    fn species_population_is_deterministic_and_species_scoped() {
        let a = draw_species_population(Seed(3), "kobold", 5, 0.5);
        let b = draw_species_population(Seed(3), "kobold", 5, 0.5);
        let g = draw_population(Seed(3), 5, 0.5);
        assert_eq!(a, b);
        // Different labeled stream ⇒ (almost surely) different jitter; assert
        // determinism and range, not inequality with the goblin draw.
        assert!((30..=625).contains(&a));
        let _ = g;
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

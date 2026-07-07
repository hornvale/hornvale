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

use hornvale_kernel::{ConceptRegistry, EntityId, RegistryError, Value, World};

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
    /// Per-settlement name generation stream.
    pub const NAME: &str = "name";
    /// Per-settlement placement draws (population against carrying
    /// capacity).
    pub const PLACEMENT: &str = "placement";
}

const SYLLABLES: [&str; 10] = [
    "zag", "gru", "mok", "nar", "bol", "ish", "rak", "ug", "tor", "gna",
];

/// Every seed-derivation label (or pattern) this crate uses, with docs.
/// Slash-joined paths document derivation chains; the manifest renders them.
pub fn stream_labels() -> Vec<(&'static str, &'static str)> {
    vec![
        ("settlement", "root stream for settlement generation"),
        ("settlement/name", "per-settlement generated name"),
        (
            "settlement/placement",
            "per-settlement population against carrying capacity",
        ),
        (
            "settlement/kobold/name",
            "per-settlement generated name, kobold stream (species-qualified; goblin keeps settlement/name)",
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

/// Generate one settlement name deterministically from the world seed and a
/// per-settlement salt (e.g. the cell id), 2-3 capitalized goblin syllables.
pub fn generate_name(seed: hornvale_kernel::Seed, salt: u64) -> String {
    let mut stream = seed
        .derive(streams::ROOT)
        .derive(streams::NAME)
        .derive(&salt.to_string())
        .stream();
    let count = stream.range_u32(2, 3);
    let raw: String = (0..count)
        .map(|_| *stream.pick(&SYLLABLES).expect("SYLLABLES is non-empty"))
        .collect();
    format!("{}{}", raw[..1].to_uppercase(), &raw[1..])
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

/// Generate a settlement name for a non-goblin species from its own labeled
/// stream (`settlement/<species>/name`) and its own syllable pool. Goblin
/// names keep `generate_name` and the legacy label untouched.
pub fn generate_species_name(
    seed: hornvale_kernel::Seed,
    species: &str,
    syllables: &[&str],
    salt: u64,
) -> String {
    let mut stream = seed
        .derive(streams::ROOT)
        .derive(species)
        .derive(streams::NAME)
        .derive(&salt.to_string())
        .stream();
    let count = stream.range_u32(2, 3);
    let raw: String = (0..count)
        .map(|_| *stream.pick(syllables).expect("syllable pool is non-empty"))
        .collect();
    format!("{}{}", raw[..1].to_uppercase(), &raw[1..])
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

/// The first settlement in the world, if any.
pub fn village_info(world: &World) -> Option<VillageInfo> {
    let id = world.ledger.find(IS_SETTLEMENT).next()?.subject;
    let name = world
        .ledger
        .text_of(id, hornvale_kernel::NAME)
        .map(str::to_string)
        .unwrap_or_else(|| format!("settlement {}", id.0));
    let population = match world.ledger.value_of(id, POPULATION) {
        Some(Value::Number(n)) => *n as u32,
        _ => 0,
    };
    Some(VillageInfo {
        id,
        name,
        population,
    })
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
            name: generate_name(seed, salt),
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
    fn names_vary_across_seeds() {
        let names: Vec<String> = (1..=8).map(|s| generate_name(Seed(s), 0)).collect();
        assert!(names.windows(2).any(|p| p[0] != p[1]));
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
    fn stream_labels_declare_every_derivation() {
        let labels: Vec<&str> = stream_labels().iter().map(|(l, _)| *l).collect();
        for expected in ["settlement", "settlement/name", "settlement/placement"] {
            assert!(labels.contains(&expected), "missing {expected}");
        }
    }

    #[test]
    fn species_name_draws_are_disjoint_from_goblin_draws() {
        // Kobold names come from a different labeled stream AND pool; the
        // legacy goblin path must be untouched by their existence.
        let goblin_before = generate_name(Seed(9), 7);
        let kobold = generate_species_name(Seed(9), "kobold", &["zik", "thur", "kra"], 7);
        let goblin_after = generate_name(Seed(9), 7);
        assert_eq!(goblin_before, goblin_after);
        assert_ne!(kobold, goblin_before);
        assert!(kobold.chars().next().unwrap().is_uppercase());
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
}

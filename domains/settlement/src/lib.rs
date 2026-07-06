//! Settlement, tier 0: one goblin village with a generated name and
//! population, placed in a home terrain entity.
#![warn(missing_docs)]

use hornvale_kernel::{
    ConceptRegistry, EntityId, Fact, LedgerError, RegistryError, Stream, Value, World,
    choose_consistent,
};

/// Predicate marking an entity as a settlement.
pub const IS_SETTLEMENT: &str = "is-settlement";
/// Predicate relating a settlement to the place containing it.
pub const LOCATED_IN: &str = "located-in";
/// Predicate giving a settlement's population.
pub const POPULATION: &str = "population";

/// Seed-derivation labels used by this crate. Labels are permanent
/// save-format contracts (spec §3); regeneration uses epoch suffixes.
mod streams {
    /// Root stream label for settlement.
    pub const ROOT: &str = "settlement";
    /// Candidate-name generation stream.
    pub const NAME: &str = "name";
    /// Name-pick offset stream.
    pub const NAME_PICK: &str = "name-pick";
    /// Population draw stream.
    pub const POPULATION: &str = "population";
}

const SYLLABLES: [&str; 10] = [
    "zag", "gru", "mok", "nar", "bol", "ish", "rak", "ug", "tor", "gna",
];

/// Every seed-derivation label (or pattern) this crate uses, with docs.
/// Slash-joined paths document derivation chains; the manifest renders them.
pub fn stream_labels() -> Vec<(&'static str, &'static str)> {
    vec![
        ("settlement", "root stream for settlement generation"),
        ("settlement/name", "candidate village names"),
        ("settlement/name-pick", "which candidate survives refinement"),
        ("settlement/population", "village population draw"),
    ]
}

/// Register settlement's contribution to the concept registry.
pub fn register_concepts(registry: &mut ConceptRegistry) -> Result<(), RegistryError> {
    registry.register_predicate(IS_SETTLEMENT, true, "subject is a settlement")?;
    registry.register_predicate(LOCATED_IN, false, "spatial containment")?;
    registry.register_predicate(POPULATION, true, "population of a settlement")
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
    /// The entity containing this settlement, if recorded.
    pub located_in: Option<EntityId>,
}

/// Generate candidate goblin names: 2-3 syllables, capitalized.
fn candidate_names(stream: &mut Stream) -> Vec<String> {
    (0..4)
        .map(|_| {
            let count = stream.range_u32(2, 3);
            let raw: String = (0..count)
                .map(|_| *stream.pick(&SYLLABLES).expect("SYLLABLES is non-empty"))
                .collect();
            format!("{}{}", raw[..1].to_uppercase(), &raw[1..])
        })
        .collect()
}

fn fact(subject: EntityId, predicate: &str, object: Value, home: EntityId) -> Fact {
    Fact {
        subject,
        predicate: predicate.to_string(),
        object,
        place: Some(home),
        day: Some(0.0),
        provenance: "settlement".to_string(),
    }
}

/// Tier-0 genesis: commit one goblin village in `home`; return its id.
pub fn genesis(world: &mut World, home: EntityId) -> Result<EntityId, LedgerError> {
    let village = world.ledger.mint_entity();

    let candidates = candidate_names(&mut world.seed.derive(streams::ROOT).derive(streams::NAME).stream());
    let name_fact = |n: &String| fact(village, hornvale_kernel::NAME, Value::Text(n.clone()), home);
    let mut pick_stream = world.seed.derive(streams::ROOT).derive(streams::NAME_PICK).stream();
    let idx = choose_consistent(
        &mut pick_stream,
        &world.ledger,
        &world.registry,
        &candidates,
        name_fact,
    )
    .expect("tier 0: at least one of four fresh names must survive an early-world ledger");
    world
        .ledger
        .commit(name_fact(&candidates[idx]), &world.registry)?;

    let population = world
        .seed
        .derive(streams::ROOT)
        .derive(streams::POPULATION)
        .stream()
        .range_u32(40, 80);
    world.ledger.commit(
        fact(village, IS_SETTLEMENT, Value::Flag(true), home),
        &world.registry,
    )?;
    world.ledger.commit(
        fact(village, LOCATED_IN, Value::Entity(home), home),
        &world.registry,
    )?;
    world.ledger.commit(
        fact(
            village,
            POPULATION,
            Value::Number(f64::from(population)),
            home,
        ),
        &world.registry,
    )?;
    Ok(village)
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
    let located_in = match world.ledger.value_of(id, LOCATED_IN) {
        Some(Value::Entity(e)) => Some(*e),
        _ => None,
    };
    Some(VillageInfo {
        id,
        name,
        population,
        located_in,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_kernel::Seed;

    fn world(seed: u64) -> (World, EntityId) {
        let mut w = World::new(Seed(seed));
        register_concepts(&mut w.registry).unwrap();
        let home = w.ledger.mint_entity();
        (w, home)
    }

    #[test]
    fn genesis_produces_a_named_populated_village() {
        let (mut w, home) = world(42);
        let village = genesis(&mut w, home).unwrap();
        let info = village_info(&w).expect("village exists");
        assert_eq!(info.id, village);
        assert!(!info.name.is_empty());
        assert!(info.name.chars().next().unwrap().is_uppercase());
        assert!((40..=80).contains(&info.population));
        assert_eq!(info.located_in, Some(home));
    }

    #[test]
    fn genesis_is_deterministic() {
        let (mut a, home_a) = world(7);
        let (mut b, home_b) = world(7);
        genesis(&mut a, home_a).unwrap();
        genesis(&mut b, home_b).unwrap();
        let ia = village_info(&a).unwrap();
        let ib = village_info(&b).unwrap();
        assert_eq!(ia.name, ib.name);
        assert_eq!(ia.population, ib.population);
    }

    #[test]
    fn names_vary_across_seeds() {
        let names: Vec<String> = (1..=8)
            .map(|s| {
                let (mut w, home) = world(s);
                genesis(&mut w, home).unwrap();
                village_info(&w).unwrap().name
            })
            .collect();
        assert!(names.windows(2).any(|p| p[0] != p[1]));
    }

    #[test]
    fn no_village_means_none() {
        let (w, _home) = world(1);
        assert!(village_info(&w).is_none());
    }

    #[test]
    fn stream_labels_declare_every_derivation() {
        let labels: Vec<&str> = stream_labels().iter().map(|(l, _)| *l).collect();
        for expected in [
            "settlement",
            "settlement/name",
            "settlement/name-pick",
            "settlement/population",
        ] {
            assert!(labels.contains(&expected), "missing {expected}");
        }
    }
}

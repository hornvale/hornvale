//! Species, tier 1: authored species definitions — a closed six-dimension
//! psychology vector, vocabulary stopgaps, and placeholder syllables. Species
//! are data; the social grammar stays code (spec §2). Goblin is the baseline:
//! scalars 0.5, default enum variants; every downstream modulation is the
//! identity function at this vector.
#![warn(missing_docs)]

use std::collections::BTreeMap;

use hornvale_kernel::{ConceptRegistry, EntityId, Fact, LedgerError, RegistryError, Value, World};

/// Predicate: a species entity's name (functional, Text).
pub const SPECIES_NAME: &str = "species-name";
/// Predicate: how a species answers threat, flee 0 ↔ stand 1 (functional, Number).
pub const THREAT_RESPONSE: &str = "species-threat-response";
/// Predicate: how slowly a species decides (functional, Number).
pub const DELIBERATION_LATENCY: &str = "species-deliberation-latency";
/// Predicate: how wide a species draws "us" (functional, Number).
pub const IN_GROUP_RADIUS: &str = "species-in-group-radius";
/// Predicate: how far ahead a species plans (functional, Number).
pub const TIME_HORIZON: &str = "species-time-horizon";
/// Predicate: hierarchic or communal sociality (functional, Text).
pub const SOCIALITY_MODE: &str = "species-sociality-mode";
/// Predicate: what earns standing — rank, knowledge, generosity (functional, Text).
pub const STATUS_BASIS: &str = "species-status-basis";
/// Predicate: the species that peoples a settlement (functional, Text).
pub const PEOPLED_BY: &str = "peopled-by";
/// Predicate: a species' activity cycle — diurnal, nocturnal, crepuscular (functional, Text).
pub const SPECIES_ACTIVITY_CYCLE: &str = "species-activity-cycle";
/// Predicate: how well a species sees at night, 0-1 (functional, Number).
pub const SPECIES_NIGHT_VISION: &str = "species-night-vision";
/// Predicate: how much of a species' attention the sky claims, 0-1 (functional, Number).
pub const SPECIES_SKY_ATTENTION: &str = "species-sky-attention";

/// How a species organizes authority.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Sociality {
    /// Ranked authority under a single head.
    Hierarchic,
    /// Collective authority, consensus-run.
    Communal,
}

/// What earns standing in a species' societies.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum StatusBasis {
    /// Dominance and position.
    Rank,
    /// Craft, lore, and cunning.
    Knowledge,
    /// Provision and largesse.
    Generosity,
}

/// When a species is awake and watching.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ActivityCycle {
    /// Awake by day (the goblin baseline).
    Diurnal,
    /// Awake by night.
    Nocturnal,
    /// Awake at the boundaries (idle this campaign; authored now so a
    /// future species is a data change).
    Crepuscular,
}

/// The closed six-dimension psychology vector (spec §3). Scalars are bare
/// ratios in `[0, 1]` with 0.5 ≡ the goblin baseline; widening the vector
/// requires its own campaign.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct PsychVector {
    /// How a society answers threat: flee 0 ↔ stand 1.
    pub threat_response: f64,
    /// How slowly decisions are made (idle this campaign; banked).
    pub deliberation_latency: f64,
    /// How wide "us" is drawn: insular 0 ↔ expansive 1.
    pub in_group_radius: f64,
    /// How far ahead works are planned: immediate 0 ↔ generational 1.
    pub time_horizon: f64,
    /// Authority shape.
    pub sociality: Sociality,
    /// What earns standing.
    pub status_basis: StatusBasis,
}

/// The closed three-dimension perception vector (spec §4). Scalars are bare
/// ratios in `[0, 1]` with 0.5 ≡ the goblin baseline; widening the vector
/// requires its own campaign. Every dimension is authored — nothing drawn.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct PerceptionVector {
    /// When this species observes.
    pub activity: ActivityCycle,
    /// Night-sky acuity: blind 0 ↔ owl-eyed 1.
    pub night_vision: f64,
    /// Celestial vs. terrestrial attention: earthbound 0 ↔ sky-rapt 1.
    pub sky_attention: f64,
}

/// One authored species: vector, vocabulary stopgaps (deleted by The
/// Tongues), and a placeholder syllable pool for names.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct SpeciesDef {
    /// The species name ("goblin", "kobold").
    pub name: &'static str,
    /// The settlement noun ("village", "warren").
    pub noun: &'static str,
    /// The psychology vector.
    pub psych: PsychVector,
    /// The perception vector.
    pub perception: PerceptionVector,
    /// Worker-role override; `None` = the subsistence worker word.
    pub worker_override: Option<&'static str>,
    /// The warrior-rung word.
    pub warrior: &'static str,
    /// The artisan-rung word.
    pub artisan: &'static str,
    /// The shaman-rung word.
    pub shaman: &'static str,
    /// The top-rung word ("chief", "elders").
    pub top: &'static str,
    /// Placeholder name syllables (goblin's pool stays in settlement).
    pub syllables: &'static [&'static str],
}

/// Kobold placeholder syllables — distinct mouth-feel from the goblin pool.
const KOBOLD_SYLLABLES: [&str; 10] = [
    "zik", "thur", "kra", "ssk", "vex", "mir", "dak", "usz", "pli", "kek",
];

/// The authored species registry, ordered (goblin sorts first). Kobold
/// values are derived from D&D 5E SRD lore — see the species chapter's
/// model card for each derivation.
pub fn registry() -> BTreeMap<&'static str, SpeciesDef> {
    let mut reg = BTreeMap::new();
    reg.insert(
        "goblin",
        SpeciesDef {
            name: "goblin",
            noun: "village",
            psych: PsychVector {
                threat_response: 0.5,
                deliberation_latency: 0.5,
                in_group_radius: 0.5,
                time_horizon: 0.5,
                sociality: Sociality::Hierarchic,
                status_basis: StatusBasis::Rank,
            },
            perception: PerceptionVector {
                activity: ActivityCycle::Diurnal,
                night_vision: 0.5,
                sky_attention: 0.5,
            },
            worker_override: None,
            warrior: "warrior",
            artisan: "artisan",
            shaman: "shaman",
            top: "chief",
            syllables: &[], // goblin names keep settlement's legacy pool
        },
    );
    reg.insert(
        "kobold",
        SpeciesDef {
            name: "kobold",
            noun: "warren",
            psych: PsychVector {
                threat_response: 0.8,
                deliberation_latency: 0.7,
                in_group_radius: 0.2,
                time_horizon: 0.8,
                sociality: Sociality::Communal,
                status_basis: StatusBasis::Knowledge,
            },
            perception: PerceptionVector {
                activity: ActivityCycle::Nocturnal,
                night_vision: 0.9,
                sky_attention: 0.8,
            },
            worker_override: Some("digger"),
            warrior: "warden",
            artisan: "shaper",
            shaman: "keeper",
            top: "elders",
            syllables: &KOBOLD_SYLLABLES,
        },
    );
    reg
}

/// Every seed-derivation label this crate uses (none — species are authored).
pub fn stream_labels() -> Vec<(&'static str, &'static str)> {
    Vec::new()
}

/// Register species' contribution to the concept registry.
pub fn register_concepts(registry: &mut ConceptRegistry) -> Result<(), RegistryError> {
    registry.register_predicate(SPECIES_NAME, true, "a species entity's name")?;
    registry.register_predicate(THREAT_RESPONSE, true, "flee 0 ↔ stand 1")?;
    registry.register_predicate(DELIBERATION_LATENCY, true, "decision slowness, 0-1")?;
    registry.register_predicate(IN_GROUP_RADIUS, true, "how wide 'us' is drawn, 0-1")?;
    registry.register_predicate(TIME_HORIZON, true, "planning depth, 0-1")?;
    registry.register_predicate(SOCIALITY_MODE, true, "hierarchic or communal")?;
    registry.register_predicate(STATUS_BASIS, true, "rank, knowledge, or generosity")?;
    registry.register_predicate(PEOPLED_BY, true, "the species that peoples a settlement")?;
    registry.register_predicate(
        SPECIES_ACTIVITY_CYCLE,
        true,
        "when a species is awake: diurnal, nocturnal, crepuscular",
    )?;
    registry.register_predicate(SPECIES_NIGHT_VISION, true, "night-sky acuity, 0-1")?;
    registry.register_predicate(SPECIES_SKY_ATTENTION, true, "sky vs. ground attention, 0-1")?;
    Ok(())
}

fn fact(subject: EntityId, predicate: &str, object: Value) -> Fact {
    Fact {
        subject,
        predicate: predicate.to_string(),
        object,
        place: None,
        day: Some(0.0),
        provenance: "species".to_string(),
    }
}

/// Mint one entity per species (registry order) and commit its authored
/// vector as facts. Species entities carry facts ONLY under this crate's
/// predicates — never `name` or any pre-existing predicate (the superset
/// contract, spec §8, depends on it).
pub fn genesis(world: &mut World) -> Result<BTreeMap<String, EntityId>, LedgerError> {
    let mut ids = BTreeMap::new();
    for (name, def) in registry() {
        let id = world.ledger.mint_entity();
        let p = def.psych;
        let sociality = match p.sociality {
            Sociality::Hierarchic => "hierarchic",
            Sociality::Communal => "communal",
        };
        let status = match p.status_basis {
            StatusBasis::Rank => "rank",
            StatusBasis::Knowledge => "knowledge",
            StatusBasis::Generosity => "generosity",
        };
        world.ledger.commit(
            fact(id, SPECIES_NAME, Value::Text(name.to_string())),
            &world.registry,
        )?;
        world.ledger.commit(
            fact(id, THREAT_RESPONSE, Value::Number(p.threat_response)),
            &world.registry,
        )?;
        world.ledger.commit(
            fact(
                id,
                DELIBERATION_LATENCY,
                Value::Number(p.deliberation_latency),
            ),
            &world.registry,
        )?;
        world.ledger.commit(
            fact(id, IN_GROUP_RADIUS, Value::Number(p.in_group_radius)),
            &world.registry,
        )?;
        world.ledger.commit(
            fact(id, TIME_HORIZON, Value::Number(p.time_horizon)),
            &world.registry,
        )?;
        world.ledger.commit(
            fact(id, SOCIALITY_MODE, Value::Text(sociality.to_string())),
            &world.registry,
        )?;
        world.ledger.commit(
            fact(id, STATUS_BASIS, Value::Text(status.to_string())),
            &world.registry,
        )?;
        let activity = match def.perception.activity {
            ActivityCycle::Diurnal => "diurnal",
            ActivityCycle::Nocturnal => "nocturnal",
            ActivityCycle::Crepuscular => "crepuscular",
        };
        world.ledger.commit(
            fact(
                id,
                SPECIES_ACTIVITY_CYCLE,
                Value::Text(activity.to_string()),
            ),
            &world.registry,
        )?;
        world.ledger.commit(
            fact(
                id,
                SPECIES_NIGHT_VISION,
                Value::Number(def.perception.night_vision),
            ),
            &world.registry,
        )?;
        world.ledger.commit(
            fact(
                id,
                SPECIES_SKY_ATTENTION,
                Value::Number(def.perception.sky_attention),
            ),
            &world.registry,
        )?;
        ids.insert(name.to_string(), id);
    }
    Ok(ids)
}

/// Commit the `peopled-by` fact linking a settlement to its species.
pub fn people(world: &mut World, settlement: EntityId, species: &str) -> Result<(), LedgerError> {
    world.ledger.commit(
        fact(settlement, PEOPLED_BY, Value::Text(species.to_string())),
        &world.registry,
    )?;
    Ok(())
}

/// The species a settlement is peopled by, if committed.
pub fn species_of(world: &World, settlement: EntityId) -> Option<String> {
    match world.ledger.value_of(settlement, PEOPLED_BY) {
        Some(Value::Text(t)) => Some(t.clone()),
        _ => None,
    }
}

/// The species entity carrying `name`'s authored vector, if genesis ran.
pub fn species_entity(world: &World, name: &str) -> Option<EntityId> {
    world
        .ledger
        .find(SPECIES_NAME)
        .find(|f| matches!(&f.object, Value::Text(t) if t == name))
        .map(|f| f.subject)
}

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_kernel::Seed;

    #[test]
    fn goblin_is_the_baseline_vector() {
        let reg = registry();
        let g = &reg["goblin"].psych;
        for v in [
            g.threat_response,
            g.deliberation_latency,
            g.in_group_radius,
            g.time_horizon,
        ] {
            assert_eq!(v, 0.5, "goblin scalars must sit exactly at baseline");
        }
        assert_eq!(g.sociality, Sociality::Hierarchic);
        assert_eq!(g.status_basis, StatusBasis::Rank);
    }

    #[test]
    fn registry_is_ordered_goblin_first_and_kobold_contrasts() {
        let reg = registry();
        let names: Vec<&str> = reg.keys().copied().collect();
        assert_eq!(names, vec!["goblin", "kobold"]);
        let k = &reg["kobold"].psych;
        assert_eq!(k.sociality, Sociality::Communal);
        assert_eq!(k.status_basis, StatusBasis::Knowledge);
        assert!(k.in_group_radius < 0.5 && k.time_horizon > 0.5 && k.threat_response > 0.5);
        assert_eq!(reg["kobold"].noun, "warren");
        assert_eq!(reg["kobold"].top, "elders");
    }

    #[test]
    fn genesis_commits_vector_facts_and_people_links_settlements() {
        let mut w = World::new(Seed(42));
        register_concepts(&mut w.registry).unwrap();
        let settlement = w.ledger.mint_entity();
        let ids = genesis(&mut w).unwrap();
        assert_eq!(ids.len(), 2);
        people(&mut w, settlement, "kobold").unwrap();
        assert_eq!(species_of(&w, settlement).as_deref(), Some("kobold"));
        // The species entity carries its vector under species predicates.
        let kobold = ids["kobold"];
        assert!(matches!(
            w.ledger.value_of(kobold, THREAT_RESPONSE),
            Some(Value::Number(n)) if *n > 0.5
        ));
        assert_eq!(w.ledger.text_of(kobold, SPECIES_NAME), Some("kobold"));
    }

    #[test]
    fn species_facts_touch_no_pre_existing_predicate() {
        let mut w = World::new(Seed(1));
        register_concepts(&mut w.registry).unwrap();
        genesis(&mut w).unwrap();
        // No species fact may land under the kernel NAME predicate (or any
        // other pre-C1 predicate) — the superset contract depends on it.
        for f in w.ledger.iter() {
            assert!(
                f.predicate.starts_with("species-") || f.predicate == PEOPLED_BY,
                "unexpected predicate {}",
                f.predicate
            );
        }
    }

    #[test]
    fn goblin_perception_is_the_baseline_and_kobold_contrasts() {
        let reg = registry();
        let g = &reg["goblin"].perception;
        assert_eq!(g.activity, ActivityCycle::Diurnal);
        assert_eq!(g.night_vision, 0.5);
        assert_eq!(g.sky_attention, 0.5);
        let k = &reg["kobold"].perception;
        assert_eq!(k.activity, ActivityCycle::Nocturnal);
        assert!(k.night_vision > 0.5 && k.sky_attention > 0.5);
    }

    #[test]
    fn genesis_commits_perception_facts() {
        let mut w = World::new(Seed(42));
        register_concepts(&mut w.registry).unwrap();
        let ids = genesis(&mut w).unwrap();
        let kobold = ids["kobold"];
        assert_eq!(
            w.ledger.text_of(kobold, SPECIES_ACTIVITY_CYCLE),
            Some("nocturnal")
        );
        assert!(matches!(
            w.ledger.value_of(kobold, SPECIES_NIGHT_VISION),
            Some(Value::Number(n)) if *n > 0.5
        ));
        assert_eq!(species_entity(&w, "kobold"), Some(kobold));
    }
}

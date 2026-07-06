//! Religion, tier 0: mythologize the most salient phenomenon. This crate
//! never learns what produced a phenomenon — that ignorance is the trace
//! protocol working (spec §3.1.6).
#![warn(missing_docs)]

use hornvale_kernel::{
    ConceptRegistry, EntityId, Fact, LedgerError, Phenomenon, RegistryError, Value, World,
};

/// Predicate marking an entity as a belief.
pub const IS_BELIEF: &str = "is-belief";
/// Predicate relating a belief to a community that holds it.
pub const HELD_BY: &str = "held-by";
/// Predicate giving a belief's tenet text.
pub const TENET: &str = "tenet";
/// Predicate recording which phenomenon kind a belief mythologizes.
pub const DERIVED_FROM_PHENOMENON: &str = "derived-from-phenomenon";

/// Seed-derivation labels used by this crate (permanent contracts).
mod streams {
    /// Root stream label for religion.
    pub const ROOT: &str = "religion";
    /// Epithet-pick stream.
    pub const EPITHET: &str = "epithet";
}

const ETERNAL_EPITHETS: [&str; 3] = ["the Unblinking Eye", "the Ever-Flame", "the Gold Warden"];
const CYCLIC_EPITHETS: [&str; 3] = ["the Returning One", "the Tidewalker", "the Promised Lamp"];

/// Every seed-derivation label this crate uses, with docs.
pub fn stream_labels() -> Vec<(&'static str, &'static str)> {
    vec![
        ("religion", "root stream for religion generation"),
        ("religion/epithet", "deity epithet pick"),
    ]
}

/// Register religion's contribution to the concept registry.
pub fn register_concepts(registry: &mut ConceptRegistry) -> Result<(), RegistryError> {
    registry.register_predicate(IS_BELIEF, true, "subject is a belief")?;
    registry.register_predicate(HELD_BY, false, "a community holding a belief")?;
    registry.register_predicate(TENET, true, "the tenet text of a belief")?;
    registry.register_predicate(
        DERIVED_FROM_PHENOMENON,
        true,
        "phenomenon kind a belief mythologizes",
    )
}

/// A belief as this domain knows it.
#[derive(Debug, Clone, PartialEq)]
pub struct Belief {
    /// The belief's entity id.
    pub id: EntityId,
    /// The belief's tenet text.
    pub tenet: String,
    /// The phenomenon kind it mythologizes.
    pub source_kind: String,
}

/// Tier-0 genesis: mythologize the most salient phenomenon (the first in
/// the salience-sorted slice). Returns None if there is nothing to revere.
pub fn genesis(
    world: &mut World,
    village: EntityId,
    phenomena: &[Phenomenon],
) -> Result<Option<EntityId>, LedgerError> {
    let Some(top) = phenomena.first() else {
        return Ok(None);
    };
    let mut stream = world
        .seed
        .derive(streams::ROOT)
        .derive(streams::EPITHET)
        .stream();
    let tenet = match top.period_days {
        None => {
            let epithet = *stream.pick(&ETERNAL_EPITHETS).expect("non-empty");
            format!(
                "{epithet} is {}; it has never departed and will never blink.",
                top.description
            )
        }
        Some(period) => {
            let epithet = *stream.pick(&CYCLIC_EPITHETS).expect("non-empty");
            format!(
                "{epithet} departs and returns every {period} days; its absences are mourned \
                 and its returns feasted."
            )
        }
    };

    let belief = world.ledger.mint_entity();
    let fact = |predicate: &str, object: Value| Fact {
        subject: belief,
        predicate: predicate.to_string(),
        object,
        place: None,
        day: Some(0.0),
        provenance: "religion".to_string(),
    };
    world
        .ledger
        .commit(fact(IS_BELIEF, Value::Flag(true)), &world.registry)?;
    world
        .ledger
        .commit(fact(TENET, Value::Text(tenet)), &world.registry)?;
    world
        .ledger
        .commit(fact(HELD_BY, Value::Entity(village)), &world.registry)?;
    world.ledger.commit(
        fact(DERIVED_FROM_PHENOMENON, Value::Text(top.kind.clone())),
        &world.registry,
    )?;
    Ok(Some(belief))
}

/// Every belief in the world, in commit order.
pub fn beliefs_of(world: &World) -> Vec<Belief> {
    world
        .ledger
        .find(IS_BELIEF)
        .map(|f| f.subject)
        .map(|id| Belief {
            id,
            tenet: world
                .ledger
                .text_of(id, TENET)
                .map(str::to_string)
                .unwrap_or_default(),
            source_kind: world
                .ledger
                .text_of(id, DERIVED_FROM_PHENOMENON)
                .map(str::to_string)
                .unwrap_or_default(),
        })
        .collect()
}

/// Explain a belief from its committed provenance: which phenomenon kind
/// it mythologizes and which system asserted it.
pub fn why(world: &World, belief: EntityId) -> Option<String> {
    let source = world.ledger.text_of(belief, DERIVED_FROM_PHENOMENON)?;
    let provenance = world
        .ledger
        .facts_about(belief)
        .find(|f| f.predicate == DERIVED_FROM_PHENOMENON)
        .map(|f| f.provenance.clone())?;
    Some(format!(
        "Derived from the most salient observed phenomenon (kind: {source}); \
         asserted by {provenance}."
    ))
}

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_kernel::Seed;

    fn world(seed: u64) -> (World, EntityId) {
        let mut w = World::new(Seed(seed));
        register_concepts(&mut w.registry).unwrap();
        let village = w.ledger.mint_entity();
        (w, village)
    }

    fn eternal_sun() -> Phenomenon {
        Phenomenon {
            kind: "celestial-body".to_string(),
            description: "a golden sun fixed at zenith".to_string(),
            period_days: None,
            salience: 1.0,
        }
    }

    fn monthly_wanderer() -> Phenomenon {
        Phenomenon {
            kind: "celestial-body".to_string(),
            description: "a pale wanderer".to_string(),
            period_days: Some(30.0),
            salience: 0.8,
        }
    }

    #[test]
    fn an_eternal_phenomenon_yields_an_eternal_tenet() {
        let (mut w, village) = world(42);
        let belief = genesis(&mut w, village, &[eternal_sun()]).unwrap().unwrap();
        let all = beliefs_of(&w);
        assert_eq!(all.len(), 1);
        assert_eq!(all[0].id, belief);
        assert_eq!(all[0].source_kind, "celestial-body");
        assert!(all[0].tenet.contains("never")); // eternal template speaks of changelessness
    }

    #[test]
    fn a_periodic_phenomenon_yields_a_cyclic_tenet() {
        let (mut w, village) = world(42);
        genesis(&mut w, village, &[monthly_wanderer()]).unwrap();
        let tenet = &beliefs_of(&w)[0].tenet;
        assert!(tenet.contains("30"));
        assert!(tenet.contains("return"));
    }

    #[test]
    fn no_phenomena_means_no_religion_yet() {
        let (mut w, village) = world(42);
        assert!(genesis(&mut w, village, &[]).unwrap().is_none());
        assert!(beliefs_of(&w).is_empty());
    }

    #[test]
    fn genesis_is_deterministic_and_seed_sensitive() {
        let tenet_for = |seed| {
            let (mut w, village) = world(seed);
            genesis(&mut w, village, &[eternal_sun()]).unwrap();
            beliefs_of(&w)[0].tenet.clone()
        };
        assert_eq!(tenet_for(7), tenet_for(7));
        let tenets: Vec<String> = (1..=8).map(tenet_for).collect();
        assert!(tenets.windows(2).any(|p| p[0] != p[1]));
    }

    #[test]
    fn why_names_the_source_phenomenon() {
        let (mut w, village) = world(42);
        let belief = genesis(&mut w, village, &[eternal_sun()]).unwrap().unwrap();
        let explanation = why(&w, belief).unwrap();
        assert!(explanation.contains("celestial-body"));
        assert!(explanation.contains("religion"));
    }

    #[test]
    fn stream_labels_declare_every_derivation() {
        let labels: Vec<&str> = stream_labels().iter().map(|(l, _)| *l).collect();
        assert!(labels.contains(&"religion"));
        assert!(labels.contains(&"religion/epithet"));
    }
}

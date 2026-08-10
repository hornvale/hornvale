#![warn(missing_docs)]
//! The person domain: the individuals a people remembers.
//!
//! A person here is the founder of an occupation notable enough that the
//! people who founded it still remembers who did. This crate owns the
//! predicates and commits the facts; it never decides *which* founders are
//! remembered, because that decision needs occupation records and species
//! lifespans, and a domain may reach only the kernel (decision 0002).
//! `windows/worldgen` resolves those and hands over [`PersonSeed`] values.

use hornvale_kernel::{
    ConceptRegistry, EntityId, Fact, LedgerError, Lineage, RegistryError, Value, World,
};
use std::collections::BTreeMap;

/// Marks an entity as an individual person.
/// type-audit: bare-ok(identifier-text)
pub const IS_PERSON: &str = "is-person";
/// The community whose occupation this person founded.
/// type-audit: bare-ok(identifier-text)
pub const PERSON_FOUNDED: &str = "person-founded";
/// The day this person was born, in absolute standard days. **May be
/// negative:** the history record begins at day 0, and the founder of a day-0
/// settlement was already grown when it began, so they were born before the
/// record starts. A negative birth day is honest, not corrupt.
/// type-audit: bare-ok(identifier-text)
pub const PERSON_BORN: &str = "person-born";
/// The day this person died. Absent while they are still alive.
/// type-audit: bare-ok(identifier-text)
pub const PERSON_DIED: &str = "person-died";

/// Register this domain's predicates.
///
/// No concepts are registered: `person` already exists as a *lexical* concept
/// owned by `domains/language` (the autonym root), and re-registering it with a
/// different definition would be a `RegistryError::ConflictingDefinition`.
pub fn register_concepts(registry: &mut ConceptRegistry) -> Result<(), RegistryError> {
    registry.register_predicate(IS_PERSON, true, "this entity is an individual person")?;
    registry.register_predicate(
        PERSON_FOUNDED,
        true,
        "the community whose occupation this person founded",
    )?;
    registry.register_predicate(
        PERSON_BORN,
        true,
        "the day this person was born; negative if before the history record began",
    )?;
    registry.register_predicate(PERSON_DIED, true, "the day this person died")?;
    Ok(())
}

/// One resolved founder, ready to commit.
///
/// Built by the composition root, which alone can see occupation records and
/// species lifespans. Every field is a kernel type so this crate needs no
/// sibling domain.
/// type-audit: waiver(decision-0014: birth_day), waiver(decision-0014: founding_day), waiver(decision-0014: death_day), bare-ok(identifier-text: name)
#[derive(Clone, Debug, PartialEq)]
pub struct PersonSeed {
    /// The community whose occupation this person founded.
    pub community: EntityId,
    /// The name this person is remembered by, drawn at genesis where the
    /// language machinery lives. Committed, not derived at render time.
    pub name: String,
    /// Birth, in absolute standard days. May be NEGATIVE: the history record
    /// begins at day 0 and a founder of a day-0 settlement was already grown,
    /// so they were born before the record starts. That is honest rather than
    /// clamped — `Fact.day` carries a sign and nothing downstream assumes
    /// otherwise.
    pub birth_day: f64,
    /// The day this person founded `community` — the occupation's own
    /// founding day. Distinct from `birth_day` because a newborn founds
    /// nothing.
    pub founding_day: f64,
    /// Death, in absolute standard days. `None` means still alive at `now`.
    pub death_day: Option<f64>,
}

/// A person's day-stamped fact. `place` is the community, so a reader can find
/// a founder from the settlement.
fn fact(subject: EntityId, predicate: &str, object: Value, community: EntityId, day: f64) -> Fact {
    Fact {
        subject,
        predicate: predicate.to_string(),
        object,
        place: Some(community),
        day: Some(day),
        provenance: "person".to_string(),
    }
}

/// Commit one person per seed, in the order given.
///
/// Four facts always — `is-person`, `name`, `person-born`, `person-founded` —
/// plus a fifth when `death_day` is set; the caller decides that, because only
/// the composition root knows `now`. `name` is kernel-core and exempt from the
/// single-writer check, so committing it here is not a violation; several
/// domains already do. Each fact is stamped at the day it became true —
/// `person-born` at `birth_day`, `person-founded` at `founding_day` — so an
/// as-of-day query never sees a newborn as already a founder.
pub fn genesis(world: &mut World, seeds: &[PersonSeed]) -> Result<Vec<EntityId>, LedgerError> {
    let mut ids = Vec::with_capacity(seeds.len());
    // A person's identity derives from the occupation they founded, which is
    // the entity every one of their facts is already `place`d at. The ordinal
    // counts persons *within* that occupation, so promoting a founder cannot
    // move any entity outside their own lineage — the property The Signet
    // exists to give, and the reason this campaign was parked until it landed.
    //
    // Counting rather than hardcoding 0: today `select_founders` picks each
    // occupation at most once, so every community has exactly one person. That
    // is an invariant of the caller, not of this function, and if it ever
    // relaxes a second founder should become a sibling rather than a collision.
    let mut nth_of_community: BTreeMap<EntityId, u16> = BTreeMap::new();
    for s in seeds {
        let ordinal = nth_of_community.entry(s.community).or_insert(0);
        let id = world.ledger.mint_entity(Lineage {
            parent: Some(s.community),
            role: "person",
            ordinal: *ordinal,
        });
        *ordinal += 1;
        world.ledger.commit(
            fact(id, IS_PERSON, Value::Flag(true), s.community, s.birth_day),
            &world.registry,
        )?;
        world.ledger.commit(
            fact(
                id,
                hornvale_kernel::NAME,
                Value::Text(s.name.clone()),
                s.community,
                s.birth_day,
            ),
            &world.registry,
        )?;
        world.ledger.commit(
            fact(
                id,
                PERSON_BORN,
                Value::Number(s.birth_day),
                s.community,
                s.birth_day,
            ),
            &world.registry,
        )?;
        world.ledger.commit(
            fact(
                id,
                PERSON_FOUNDED,
                Value::Entity(s.community),
                s.community,
                s.founding_day,
            ),
            &world.registry,
        )?;
        if let Some(d) = s.death_day {
            world.ledger.commit(
                fact(id, PERSON_DIED, Value::Number(d), s.community, d),
                &world.registry,
            )?;
        }
        ids.push(id);
    }
    Ok(ids)
}

/// The person domain, for the composition root's roster.
#[derive(Debug, Default)]
pub struct Person;

impl hornvale_kernel::Domain for Person {
    fn crate_name(&self) -> &'static str {
        env!("CARGO_PKG_NAME")
    }
    fn register_concepts(
        &self,
        registry: &mut hornvale_kernel::ConceptRegistry,
    ) -> Result<(), hornvale_kernel::RegistryError> {
        crate::register_concepts(registry)
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn concepts_registered() {
        let mut r = hornvale_kernel::ConceptRegistry::default();
        crate::register_concepts(&mut r).expect("registers without conflict");
        crate::register_concepts(&mut r).expect("registration is idempotent");
        let names: Vec<&str> = r.predicates().map(|p| p.name.as_str()).collect();
        for p in [
            crate::IS_PERSON,
            crate::PERSON_FOUNDED,
            crate::PERSON_BORN,
            crate::PERSON_DIED,
        ] {
            assert!(names.contains(&p), "{p} should be registered");
        }
    }

    #[test]
    fn a_living_founder_gets_no_death_fact() {
        let mut world = hornvale_kernel::World::new(hornvale_kernel::Seed(1));
        crate::register_concepts(&mut world.registry).expect("registers");
        let community = world.ledger.mint_entity(hornvale_kernel::test_lineage(0));
        let ids = crate::genesis(
            &mut world,
            &[
                crate::PersonSeed {
                    community,
                    name: "Grokk".to_string(),
                    birth_day: 10.0,
                    founding_day: 30.0,
                    death_day: Some(60.0),
                },
                crate::PersonSeed {
                    community,
                    name: "Vashti".to_string(),
                    birth_day: 20.0,
                    founding_day: 25.0,
                    death_day: None,
                },
            ],
        )
        .expect("commits");
        assert_eq!(ids.len(), 2, "one entity per seed");

        let died: Vec<&hornvale_kernel::Fact> = world.ledger.find(crate::PERSON_DIED).collect();
        assert_eq!(died.len(), 1, "only the dead founder carries a death fact");
        assert_eq!(died[0].subject, ids[0]);

        let founded: Vec<&hornvale_kernel::Fact> =
            world.ledger.find(crate::PERSON_FOUNDED).collect();
        assert_eq!(founded.len(), 2, "every founder carries a founding fact");
        for f in &founded {
            let day = f.day.expect("person-founded carries a day");
            assert_ne!(
                day,
                if f.subject == ids[0] { 10.0 } else { 20.0 },
                "founded is stamped at founding_day, not birth_day"
            );
        }

        let born: Vec<&hornvale_kernel::Fact> = world.ledger.find(crate::PERSON_BORN).collect();
        assert_eq!(born.len(), 2, "every founder carries a birth fact");
    }

    #[test]
    fn a_founder_matures_before_founding_and_the_stamps_say_so() {
        let mut world = hornvale_kernel::World::new(hornvale_kernel::Seed(1));
        crate::register_concepts(&mut world.registry).expect("registers");
        let community = world.ledger.mint_entity(hornvale_kernel::test_lineage(0));
        let ids = crate::genesis(
            &mut world,
            &[crate::PersonSeed {
                community,
                name: "Grokk".to_string(),
                birth_day: -7305.0,
                founding_day: 0.0,
                death_day: Some(10_000.0),
            }],
        )
        .expect("commits");

        let day_of = |p: &str| -> f64 {
            world
                .ledger
                .facts_about(ids[0])
                .find(|f| f.predicate == p)
                .and_then(|f| f.day)
                .expect("every person fact carries a day")
        };
        assert_eq!(
            day_of(crate::PERSON_BORN),
            -7305.0,
            "born before the record"
        );
        assert_eq!(day_of(crate::PERSON_FOUNDED), 0.0, "founded when grown");
        assert_eq!(day_of(crate::PERSON_DIED), 10_000.0, "died at death");
        assert!(
            day_of(crate::PERSON_BORN) < day_of(crate::PERSON_FOUNDED),
            "a newborn founds nothing"
        );
    }
}

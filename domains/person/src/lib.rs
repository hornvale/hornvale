#![warn(missing_docs)]
//! The person domain: the individuals a people remembers.
//!
//! A person here is the founder of an occupation notable enough that the
//! people who founded it still remembers who did. This crate owns the
//! predicates and commits the facts; it never decides *which* founders are
//! remembered, because that decision needs occupation records and species
//! lifespans, and a domain may reach only the kernel (decision 0002).
//! `windows/worldgen` resolves those and hands over [`PersonSeed`] values.

use hornvale_kernel::{ConceptRegistry, EntityId, Fact, LedgerError, RegistryError, Value, World};

/// Marks an entity as an individual person.
/// type-audit: bare-ok(identifier-text)
pub const IS_PERSON: &str = "is-person";
/// The community whose occupation this person founded.
/// type-audit: bare-ok(identifier-text)
pub const PERSON_FOUNDED: &str = "person-founded";
/// The day this person was born, in absolute standard days.
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
    registry.register_predicate(PERSON_BORN, true, "the day this person was born")?;
    registry.register_predicate(PERSON_DIED, true, "the day this person died")?;
    Ok(())
}

/// One resolved founder, ready to commit.
///
/// Built by the composition root, which alone can see occupation records and
/// species lifespans. Every field is a kernel type so this crate needs no
/// sibling domain.
/// type-audit: bare-ok(identifier-text: name), bare-ok(count: birth_day), bare-ok(count: death_day)
#[derive(Clone, Debug, PartialEq)]
pub struct PersonSeed {
    /// The community whose occupation this person founded.
    pub community: EntityId,
    /// The name this person is remembered by, drawn at genesis where the
    /// language machinery lives. Committed, not derived at render time.
    pub name: String,
    /// Birth, in absolute standard days.
    pub birth_day: f64,
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
/// Four facts always — `is-person`, `name`, `person-founded`, `person-born` —
/// plus a fifth when the person has already died. `name` is kernel-core and
/// exempt from the single-writer check, so committing it here is not a
/// violation; several domains already do.
/// A living
/// person is represented by the *absence* of `person-died`: birth is known and
/// death may not have happened, which is the asymmetry the occupation data
/// already carries.
pub fn genesis(world: &mut World, seeds: &[PersonSeed]) -> Result<Vec<EntityId>, LedgerError> {
    let mut ids = Vec::with_capacity(seeds.len());
    for s in seeds {
        let id = world.ledger.mint_entity();
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
                PERSON_FOUNDED,
                Value::Entity(s.community),
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
        let community = world.ledger.mint_entity();
        let ids = crate::genesis(
            &mut world,
            &[
                crate::PersonSeed {
                    community,
                    name: "Grokk".to_string(),
                    birth_day: 10.0,
                    death_day: Some(60.0),
                },
                crate::PersonSeed {
                    community,
                    name: "Vashti".to_string(),
                    birth_day: 20.0,
                    death_day: None,
                },
            ],
        )
        .expect("commits");
        assert_eq!(ids.len(), 2, "one entity per seed");

        let died: Vec<&hornvale_kernel::Fact> = world.ledger.find(crate::PERSON_DIED).collect();
        assert_eq!(died.len(), 1, "only the dead founder carries a death fact");
        assert_eq!(died[0].subject, ids[0]);

        let born: Vec<&hornvale_kernel::Fact> = world.ledger.find(crate::PERSON_BORN).collect();
        assert_eq!(born.len(), 2, "every founder carries a birth fact");
    }
}

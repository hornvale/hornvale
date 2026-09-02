//! Residents (The Roll, spec §3.1): a settlement's `population` persons,
//! minted on demand by lineage into the SESSION ledger — never the world's —
//! and built as bodies from `body_at`'s derivation with the resident's own
//! drawn dials and name. Ordinal 0 is the lineage `derive_npcs` has always
//! minted, so that body's identity is unchanged (decision 0227).

use hornvale_kernel::{Fact, Ledger, Lineage, NAME, Value, World, WorldTime};
use hornvale_locale::LocaleContext;
use hornvale_person::{IS_PERSON, PERSON_BORN};
use hornvale_settlement::VillageInfo;
use hornvale_worldgen::WorldComponents;
use hornvale_worldgen::residents::resident_draws;

use crate::body::Body;
use crate::liveness::body_at;

/// Provenance stamped on every fact this module commits.
const PROVENANCE: &str = "the-roll";

/// Derive `village`'s residents into `ledger`: `population` bodies, ordinal
/// == index, each a person (`is-person`, `person-born`, `name`).
///
/// **The ledger wins: a name or a birth already committed is never
/// redrawn.** `NAME` and `PERSON_BORN` are both functional, and
/// `Ledger::commit`'s functional check compares only `(subject, predicate,
/// object)` — never `provenance`/`place`/`day` — so a *recommit* of either
/// with a value that differs from what is already there (a fresh draw's
/// name against `derive_npcs`'s generic label at ordinal 0; a birth day
/// computed from a *different* `now` than the entity's first derivation
/// used) is a `Contradiction`, not a silent no-op: `commit`'s idempotency
/// dedup (`contains_full`/`naive_contains`) requires the whole [`Fact`] to
/// match byte-for-byte, which a differing object can never do. So a
/// resident's name and birth are each read from the ledger first
/// (`text_of`/`value_of`) and, if already present, are never recommitted —
/// `is-person` is committed only alongside `person-born`, at the same
/// first-derivation moment, so it is skipped under the identical guard.
///
/// Ordinal 0's `EntityId` is the exact lineage `derive_npcs` has always
/// minted (`role: "npc", ordinal: 0`, parented on `village.id`), so a
/// pre-campaign save's flagship possession still resolves the same
/// creature. That save may already carry a NAME fact for that entity from
/// `derive_npcs` (provenance `"the-quickening"`, a generic species label,
/// not a personal name); the same ledger-wins rule above covers it without
/// a special case.
pub fn derive_residents(
    world: &World,
    ctx: &LocaleContext,
    ledger: &mut Ledger,
    wc: &WorldComponents,
    village: &VillageInfo,
    now: WorldTime,
) -> Vec<Body> {
    let species =
        hornvale_species::species_of(world, village.id).unwrap_or_else(|| "goblin".to_string());
    let draws = resident_draws(world, wc, village, &species);
    draws
        .into_iter()
        .map(|draw| {
            let entity = ledger.reuse_or_mint_entity(Lineage {
                parent: Some(village.id),
                role: "npc",
                ordinal: draw.ordinal,
            });
            // The ledger's own name wins: `NAME` is functional, and a
            // resident named in an earlier session (or ordinal 0's
            // pre-campaign `derive_npcs` label) must not be renamed by a
            // fresh draw.
            let existing_name = ledger.text_of(entity, NAME).map(str::to_string);
            let name = existing_name.clone().unwrap_or(draw.name);
            if existing_name.is_none() {
                // `place: None, day: None`, exactly the shape `derive_npcs`
                // commits at `liveness.rs:5900-5912` — NAME is kernel-core
                // and exempt from the single-writer discipline, so more
                // than one caller committing it is not a violation.
                ledger
                    .commit(
                        Fact {
                            subject: entity,
                            predicate: NAME.to_string(),
                            object: Value::Text(name.clone()),
                            place: None,
                            day: None,
                            provenance: PROVENANCE.to_string(),
                        },
                        &world.registry,
                    )
                    .expect("a freshly minted resident's first NAME fact always commits");
            }
            // The ledger's own birth wins too, and for the same reason:
            // `PERSON_BORN` is functional, so a second derivation at a
            // different `now` must not attempt to recommit a different
            // day — a resident's birth is fixed at its FIRST derivation.
            // `is-person` was committed alongside it that first time, so
            // it shares this guard rather than carrying its own.
            if ledger.value_of(entity, PERSON_BORN).is_none() {
                let born = now.as_std_days() - draw.age_days;
                let born_at =
                    WorldTime::from_std_days(born).expect("a resident's birth is a finite day");
                for fact in [
                    Fact {
                        subject: entity,
                        predicate: IS_PERSON.to_string(),
                        object: Value::Flag(true),
                        place: Some(village.id),
                        day: Some(born_at),
                        provenance: PROVENANCE.to_string(),
                    },
                    Fact {
                        subject: entity,
                        predicate: PERSON_BORN.to_string(),
                        object: Value::Number(born),
                        place: Some(village.id),
                        day: Some(born_at),
                        provenance: PROVENANCE.to_string(),
                    },
                ] {
                    ledger.commit(fact, &world.registry).expect(
                        "a resident's identity facts are registered, finite and non-contradicting",
                    );
                }
            }
            let mut body = body_at(world, ctx, village, entity);
            body.label = name;
            body.boldness = draw.mind.threat_response;
            body.deliberation_latency = draw.mind.deliberation_latency;
            body.time_horizon = draw.mind.time_horizon;
            body
        })
        .collect()
}

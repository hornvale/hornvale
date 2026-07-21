//! Commit the baked occupation skeleton to the ledger, and read the present
//! back out of it — the campaign's keystone: **history is the settlement
//! provider**. An occupation still alive at `now` becomes an `is-settlement`
//! fact; a dead one becomes an `is-ruin`. Once committed, the deep past is
//! queryable directly off the ledger's object index (`ruins_of_people`) —
//! nothing here replays the bake to answer a question about it.
//!
//! Determinism: one entity is minted per [`OccupationRecord`], strictly in
//! `records` order, so the same [`History`] always mints the same ids and
//! commits the same facts in the same order. `Ledger::commit` quantizes
//! `Value::Number` objects (and `day`) at the emit boundary; this module
//! never quantizes anything itself.

use crate::{BuildError, History};
use hornvale_history::record::{CauseOfEnd, Ended, Founding, Function, Notability, TechHorizon};
use hornvale_kernel::{CellId, EntityId, Fact, KindId, Value, World};
use std::collections::{BTreeMap, BTreeSet};

/// Build one fact about occupation entity `subject`, day-stamped at `day` —
/// the day this particular fact became true (founding facts pass
/// `record.founded`; end-of-life facts pass `record.ended`, since `Fact.day`
/// means "the day this fact was observed" and an occupation isn't ended
/// until it ends) — self-placed (an occupation is its own place, mirroring
/// `hornvale_settlement::genesis`'s pattern), provenanced to the deep-history
/// bake stream.
fn fact(subject: EntityId, predicate: &str, object: Value, day: f64) -> Fact {
    Fact {
        subject,
        predicate: predicate.to_string(),
        object,
        place: Some(subject),
        day: Some(day),
        provenance: hornvale_history::streams::BAKE.to_string(),
    }
}

/// The stable text label for a tech horizon (round-trippable via `OCC_TECH`).
fn tech_label(t: TechHorizon) -> &'static str {
    match t {
        TechHorizon::Neolithic => "neolithic",
        TechHorizon::Bronze => "bronze",
        TechHorizon::Iron => "iron",
        TechHorizon::Classical => "classical",
    }
}

/// The stable text label for a community's function (round-trippable via
/// `OCC_FUNCTION`).
fn function_label(f: Function) -> &'static str {
    match f {
        Function::Agrarian => "agrarian",
        Function::Mine => "mine",
        Function::Trade => "trade",
        Function::Cult => "cult",
        Function::Fort => "fort",
    }
}

/// The stable text label for why an occupation ended (round-trippable via
/// `OCC_CAUSE`).
fn cause_label(c: CauseOfEnd) -> &'static str {
    match c {
        CauseOfEnd::Famine => "famine",
        CauseOfEnd::Burned => "burned",
        CauseOfEnd::Plague => "plague",
        CauseOfEnd::Fled => "fled",
        CauseOfEnd::Migrated => "migrated",
    }
}

/// The stable text label for an occupation's notability (round-trippable via
/// `OCC_NOTABILITY`).
fn notability_label(n: Notability) -> &'static str {
    match n {
        Notability::Backwater => "backwater",
        Notability::Common => "common",
        Notability::Seat => "seat",
    }
}

/// Resolve a ledger-round-tripped people label back to its canonical
/// `'static` `KindId`, by matching against `hornvale_species`'s biosphere
/// roster — the same "runtime text cannot construct a `KindId` key, so
/// compare content against a known roster" idiom
/// `ComponentStore::get_by_label` documents, just returning the key instead
/// of the component.
fn resolve_people(label: &str) -> Option<KindId> {
    hornvale_species::biosphere_registry()
        .iter()
        .find(|(k, _)| k.0 == label)
        .map(|(k, _)| *k)
}

/// Commit a baked [`History`]'s whole occupation skeleton to `world`'s
/// ledger: one entity per [`OccupationRecord`] (minted in `records` order),
/// tagged `is-occupation` plus its ~11 descriptive facts, `is-ruin` for a
/// dead occupation or `is-settlement`/`population`/`cell-id` for one still
/// alive at `now` — the present is simply the last frame of the committed
/// deep history, not a separate placement.
pub fn emit_history(world: &mut World, h: &History) -> Result<(), BuildError> {
    // Mint one entity per record, strictly in `records` order (determinism:
    // same history ⇒ same ids ⇒ same facts, every time).
    let minted: Vec<EntityId> = h
        .records
        .iter()
        .map(|_| world.ledger.mint_entity())
        .collect();

    // The bake's own (non-ledger) community ids referenced by `founded_from`/
    // `ended_by` are exactly the `community` field of some record in this
    // same history (a raider, or a founding community, is itself always an
    // occupation the bake opened) — map each back to the entity minted for
    // its record.
    let bake_to_ledger: BTreeMap<EntityId, EntityId> = h
        .records
        .iter()
        .zip(minted.iter().copied())
        .map(|(r, e)| (r.community, e))
        .collect();

    for (record, &id) in h.records.iter().zip(minted.iter()) {
        let day = record.founded;
        // End-of-life facts (`OCC_ENDED`, `OCC_CAUSE`, `OCC_ENDED_BY`,
        // `IS_RUIN`) describe events that became true at `record.ended`, not
        // at founding — `Fact.day` means "the day this fact was observed"
        // (see `kernel/src/ledger.rs`), so an as-of-day-N query must not see
        // an occupation as already-ended on its founding day. A still-alive
        // record never commits these, so the `unwrap_or` fallback is inert.
        let end_day = record.ended.unwrap_or(record.founded);
        let mut commit_on = |predicate: &str, object: Value, day: f64| -> Result<(), BuildError> {
            world
                .ledger
                .commit(fact(id, predicate, object, day), &world.registry)?;
            Ok(())
        };

        commit_on(hornvale_history::IS_OCCUPATION, Value::Flag(true), day)?;
        commit_on(
            hornvale_history::OCC_PEOPLE,
            Value::Text(record.people.0.to_string()),
            day,
        )?;
        commit_on(
            hornvale_history::OCC_SITE,
            Value::Number(f64::from(record.site.0)),
            day,
        )?;
        commit_on(
            hornvale_history::OCC_FOUNDED,
            Value::Number(record.founded),
            day,
        )?;
        if let Some(ended) = record.ended {
            commit_on(hornvale_history::OCC_ENDED, Value::Number(ended), end_day)?;
        }
        commit_on(
            hornvale_history::OCC_PEAK,
            Value::Number(f64::from(record.peak_population)),
            day,
        )?;
        commit_on(
            hornvale_history::OCC_TECH,
            Value::Text(tech_label(record.tech).to_string()),
            day,
        )?;
        commit_on(
            hornvale_history::OCC_FUNCTION,
            Value::Text(function_label(record.function).to_string()),
            day,
        )?;
        if let Some(cause) = record.cause {
            commit_on(
                hornvale_history::OCC_CAUSE,
                Value::Text(cause_label(cause).to_string()),
                end_day,
            )?;
        }
        // `ended_by` only means something once an occupation has actually
        // ended; a still-alive record's `Ended::Nature` default is not a
        // claim worth committing.
        if record.ended.is_some() {
            let ended_by = match record.ended_by {
                Ended::Nature => Value::Text("nature".to_string()),
                Ended::By(e) => Value::Entity(
                    *bake_to_ledger
                        .get(&e)
                        .expect("ended-by names a community minted earlier in this history"),
                ),
            };
            commit_on(hornvale_history::OCC_ENDED_BY, ended_by, end_day)?;
        }
        let founded_from = match record.founded_from {
            Founding::Genesis(cell) => Value::Number(f64::from(cell.0)),
            Founding::From(e) => Value::Entity(
                *bake_to_ledger
                    .get(&e)
                    .expect("founded-from names a community minted earlier in this history"),
            ),
        };
        commit_on(hornvale_history::OCC_FOUNDED_FROM, founded_from, day)?;
        commit_on(
            hornvale_history::OCC_NOTABILITY,
            Value::Text(notability_label(record.notability).to_string()),
            day,
        )?;

        if record.is_alive() {
            commit_on(hornvale_settlement::IS_SETTLEMENT, Value::Flag(true), day)?;
            commit_on(
                hornvale_settlement::POPULATION,
                Value::Number(f64::from(record.peak_population)),
                day,
            )?;
            commit_on(
                hornvale_settlement::CELL_ID,
                Value::Number(f64::from(record.site.0)),
                day,
            )?;
        } else {
            commit_on(hornvale_history::IS_RUIN, Value::Flag(true), end_day)?;
        }
    }
    Ok(())
}

/// The dominant people per region: every cell an alive occupation (a
/// committed `is-settlement`) sits on, grouped by that occupation's people.
/// Reads purely off the ledger — the present-as-query the campaign's
/// keystone names.
pub fn territories(world: &World) -> BTreeMap<KindId, BTreeSet<CellId>> {
    let mut map: BTreeMap<KindId, BTreeSet<CellId>> = BTreeMap::new();
    for f in world.ledger.find(hornvale_settlement::IS_SETTLEMENT) {
        let id = f.subject;
        let Some(label) = world.ledger.text_of(id, hornvale_history::OCC_PEOPLE) else {
            continue;
        };
        let Some(people) = resolve_people(label) else {
            continue;
        };
        let Some(Value::Number(cell)) = world.ledger.value_of(id, hornvale_settlement::CELL_ID)
        else {
            continue;
        };
        map.entry(people).or_default().insert(CellId(*cell as u32));
    }
    map
}

/// Every ruin (a dead occupation) a `people` ever held — a direct query
/// against the ledger's object index (`OCC_PEOPLE` narrowed to `IS_RUIN`
/// subjects), proving the deep past is queryable without replaying the bake.
pub fn ruins_of_people(world: &World, people: KindId) -> Vec<EntityId> {
    world
        .ledger
        .query_by_object(&Value::Text(people.0.to_string()))
        .filter(|f| f.predicate == hornvale_history::OCC_PEOPLE)
        .map(|f| f.subject)
        .filter(|&e| {
            world
                .ledger
                .facts_about(e)
                .any(|f| f.predicate == hornvale_history::IS_RUIN)
        })
        .collect()
}

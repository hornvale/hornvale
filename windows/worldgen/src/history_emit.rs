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

use crate::history_bake::BakeId;
use crate::{BuildError, History};
use hornvale_history::record::{
    CauseOfEnd, Ended, Founding, FoundingCoords, Function, Notability, Occupation,
    OccupationRecord, TechHorizon, founding_coords, layer_key,
};
use hornvale_kernel::{EntityId, Fact, KindId, Lineage, Value, Vertex, World, WorldTime};
use std::collections::{BTreeMap, BTreeSet};

/// **The unit boundary (The Ell, spec §2a).** A bake-side YEAR becomes the
/// standard DAY the ledger speaks.
///
/// The history bake reasons in years and is right to — `BakeConfig::start_year`
/// / `end_year` stay years, and so does every `Occupation`/`FoundingCoords`
/// value in this crate. What is constrained is what *crosses into the ledger*,
/// which is days, the same unit `WorldTime` and every other domain's `Fact.day`
/// already carry. Every write in this module that carries a time goes through
/// here, so the crossing is one named seam rather than N inline multiplies.
///
/// Finiteness is not checked: the input is bake output (a `f64` stepped from
/// `start_year` by `epoch_years`), never parsed text, and a finite year times a
/// finite constant is finite. The `WorldTime::from_std_days(...).expect(...)` in [`fact`]
/// is the assertion of that.
/// seam-guard: identity(0) scope(hornvale-worldgen)
///
/// `identity(0)` is exactly "drop the conversion" — the mutation The Ell ran
/// by hand at every crossing, now standing. Scoped narrowly on purpose: a
/// too-narrow scope can still manufacture a false SURVIVOR, never a false
/// GUARDED, so a GUARDED verdict here is true whatever a wider run would say.
/// If this ever reports UNGUARDED, do not reflexively reach for
/// `expect(survives: …)` — that is the failure `conquest_victim`'s
/// declaration made for two campaigns.
///
/// **Widening the scope is not the first thing to try — check the ignore
/// tier first.** The Ballast found the call this seam actually guarded
/// (`windows/worldgen/tests/repose_exposure.rs:1002`) reachable only from
/// that file's `heavy:`-ignored batteries. `run_scope` executes `cargo
/// nextest run -p <scope> --no-fail-fast`, which runs non-ignored tests
/// only — so a mutation whose only witnesses live behind `#[ignore]` reports
/// UNGUARDED at *every* `scope()`, including the widest available one
/// (verified empirically against `scope(hornvale)`; see commit `2fbcc4d7`).
/// Widening scope only helps a mutation some crate's non-ignored tests
/// already reach; it cannot make a heavy-only call site reachable. The fix
/// that actually worked was moving the vulnerable composition into a named
/// production function (`present_frame`) with its own cheap, non-ignored
/// unit test — the general remedy is relocating the call out of
/// `#[ignore]`d code, not widening `scope()`.
/// type-audit: bare-ok(count: year), bare-ok(count: return)
pub fn ledger_day_of_bake_year(year: f64) -> f64 {
    // The crossing quantizes (decision 0033), and must, for two reasons that
    // both post-date the original two-line body:
    //
    // The Escapement (decision 0186) split the ledger's canonicalizations:
    // a committed `Value::Number` object still rounds to 8 significant digits
    // while `Fact.day` became an exact tick count. Every caller commits the
    // result BOTH ways — as an `occ-founded` object and as its own day stamp —
    // so an unquantized crossing let one instant present as two values a
    // half-ULP apart (`155261.6875` as a day, `155261.69` as the object), and
    // any consumer ordering events by `Fact.day` against a founding object
    // read inversions into same-instant pairs: 7 of seed 42's 157 tribute
    // facts predated the very patron whose seating instant they carried.
    // Quantizing here makes the forward map idempotent under commit, so the
    // object and the day agree to the last bit.
    //
    // The Granary's sub-year stamps (`year + phase / PHASES_PER_YEAR`) also
    // ended the old guarantee that bake crossings were coarse enough to
    // survive 8-digit quantization unchanged (`k · 9131.25` days); folding the
    // rounding into the crossing is what keeps the round trip through
    // `bake_year_of_ledger_day` lossless for them.
    hornvale_kernel::quantize(year * hornvale_kernel::Years::DAYS_PER_YEAR)
}

/// The inverse read: a standard DAY off the ledger becomes the bake-side YEAR
/// this crate's `Occupation` records, `FoundingCoords` and present-frame
/// arithmetic are all expressed in.
///
/// Every ledger read of an `occ-founded` / `occ-ended` / `history-now` object
/// that will be *compared with or subtracted from* another year goes through
/// here. Two readers deliberately do not, and both are recorded where they sit:
/// a read whose value is used only for ordering (`lib.rs`'s `predecessor_people`,
/// `layer_key`'s sort) is invariant under this monotone map, and a read that
/// compares two raw ledger values against each other ([`migration_events`])
/// has both sides move together.
///
/// **Losslessness is a property of the bake, not of this function.** The
/// forward map quantizes to 8 significant digits itself (see above), so it is
/// idempotent under `Ledger::commit` and the round trip is exact for any year
/// whose crossing survives that rounding — whole epoch years always did; The
/// Granary's sub-year stamps do because the rounding now happens here, once,
/// rather than at the object boundary alone. `windows/worldgen/tests/history_units.rs`'s
/// `reading_a_founding_back_out_of_the_ledger_is_lossless` asserts that
/// property on a real world, because if it ever stops holding, every founder
/// handle and every flesh seed in every world moves and nothing else says so.
/// **Deliberately NOT registered with `tools/seam-guard`, and the reason is
/// a tool limitation worth knowing.** seam-guard keys a seam by function
/// NAME, and `windows/almanac` defines a function of this same name on
/// purpose — the mirror three paragraphs up, which exists because that window
/// cannot depend on this crate. So a tag here claims the almanac's call sites
/// too, and any single `scope(...)` covers only one of the two crates: the
/// other crate's sites would be neutralised and then checked against tests
/// that cannot see them, reporting FALSE SURVIVORS. That is the same failure
/// `conquest_victim`'s `scope(hornvale-almanac)` made, and registering this
/// seam would manufacture it deliberately. [`ledger_day_of_bake_year`] has a
/// unique name and every call site in this crate, so it IS registered.
/// The crossings here are guarded by hand instead — five of them were found
/// unguarded and closed in The Ell, each watched to fail.
/// type-audit: bare-ok(count: day), bare-ok(count: return)
pub fn bake_year_of_ledger_day(day: f64) -> f64 {
    day / hornvale_kernel::Years::DAYS_PER_YEAR
}

/// Build one fact about occupation entity `subject`, day-stamped at `day` —
/// the day this particular fact became true (founding facts pass
/// `record.founded`; end-of-life facts pass `record.ended`, since `Fact.day`
/// means "the day this fact was observed" and an occupation isn't ended
/// until it ends) — self-placed (an occupation is its own place, mirroring
/// `hornvale_settlement::genesis`'s pattern), provenanced to the deep-history
/// bake stream.
///
/// `day` is a **standard day**: every caller has already crossed
/// [`ledger_day_of_bake_year`]. It derives from `record.core.founded` or
/// `record.core.ended` — deep-history bake output, not stdin/parsed text, so
/// it is finite by construction of the bake it came from, and a finite year
/// scaled by a finite constant stays finite; `.expect()` is sound here.
fn fact(subject: EntityId, predicate: &str, object: Value, day: f64) -> Fact {
    fact_at(subject, predicate, object, day, Some(subject))
}

/// Build a dated fact with an explicit event identity in its serialized place
/// field. Paired epidemic facts use this to share one stable join token while
/// retaining the struck occupation as their subject.
fn fact_at(
    subject: EntityId,
    predicate: &str,
    object: Value,
    day: f64,
    place: Option<EntityId>,
) -> Fact {
    Fact {
        subject,
        predicate: predicate.to_string(),
        object,
        place,
        day: Some(WorldTime::from_std_days(day).expect("history-bake day is finite")),
        provenance: hornvale_history::streams::BAKE.as_str().to_string(),
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
        CauseOfEnd::Breached => "breached",
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
/// deep history, not a separate placement. Finally, one `pays-tribute-to` fact
/// per relation still standing at `now`, so a subordination is legible off the
/// ledger without replaying the bake that imposed it.
pub fn emit_history(world: &mut World, h: &History) -> Result<(), BuildError> {
    // Mint one entity per record, strictly in `records` order (determinism:
    // same history ⇒ same ids ⇒ same facts, every time).
    // An occupation has no ledger entity above it — its site is a `Vertex` and
    // its people a `KindId`, neither of which is an entity — so it roots, and
    // its ordinal is its position in the baked `records` order. Spec P4: a
    // future bake that reorders `records` still moves these ids, and that is
    // the lineage's own churn rather than a residual defect.
    let minted: Vec<EntityId> = h
        .records
        .iter()
        .enumerate()
        .map(|(i, _)| {
            world.ledger.mint_entity(Lineage {
                parent: None,
                role: "occupation",
                ordinal: i as u16,
            })
        })
        .collect();

    // The bake's own (non-ledger) community handles referenced by
    // `founded_from`/`ended_by` are exactly the `community` field of some
    // record in this same history (a raider, or a founding community, is
    // itself always an occupation the bake opened) — map each back to the
    // entity minted for its record.
    let bake_to_ledger: BTreeMap<BakeId, EntityId> = h
        .records
        .iter()
        .zip(minted.iter().copied())
        .map(|(r, e)| (r.community, e))
        .collect();

    let outbreak_entities: Vec<EntityId> = h
        .outbreaks
        .iter()
        .enumerate()
        .map(|(ordinal, event)| {
            let subject = *bake_to_ledger
                .get(&event.occupation)
                .expect("an outbreak names an occupation minted in this history");
            world.ledger.mint_entity(Lineage {
                parent: Some(subject),
                role: "outbreak-event",
                ordinal: ordinal as u16,
            })
        })
        .collect();

    for (record, &id) in h.records.iter().zip(minted.iter()) {
        // The unit boundary: `record.core.founded`/`ended` are bake YEARS;
        // everything below this line is standard DAYS.
        let day = ledger_day_of_bake_year(record.core.founded);
        // End-of-life facts (`OCC_ENDED`, `OCC_CAUSE`, `OCC_ENDED_BY`,
        // `IS_RUIN`) describe events that became true at `record.ended`, not
        // at founding — `Fact.day` means "the day this fact was observed"
        // (see `kernel/src/ledger.rs`), so an as-of-day-N query must not see
        // an occupation as already-ended on its founding day. A still-alive
        // record never commits these, so the `unwrap_or` fallback is inert.
        let end_day = record.core.ended.map_or(day, ledger_day_of_bake_year);
        let mut commit_on = |predicate: &str, object: Value, day: f64| -> Result<(), BuildError> {
            world
                .ledger
                .commit(fact(id, predicate, object, day), &world.registry)?;
            Ok(())
        };

        commit_on(hornvale_history::IS_OCCUPATION, Value::Flag(true), day)?;
        commit_on(
            hornvale_history::OCC_PEOPLE,
            Value::Text(record.core.people.0.to_string()),
            day,
        )?;
        commit_on(
            hornvale_history::OCC_SITE,
            Value::Number(f64::from(record.core.site.0)),
            day,
        )?;
        // The object is the same crossing as the stamp: `occ-founded` and
        // `occ-ended` name days on the ledger's own axis, so a consumer that
        // reads the object and a consumer that reads `Fact.day` see one unit.
        commit_on(hornvale_history::OCC_FOUNDED, Value::Number(day), day)?;
        if record.core.ended.is_some() {
            commit_on(hornvale_history::OCC_ENDED, Value::Number(end_day), end_day)?;
        }
        commit_on(
            hornvale_history::OCC_PEAK,
            Value::Number(f64::from(record.core.peak_population)),
            day,
        )?;
        // Stamped at `end_day`, like `occ-delve-depth`: an integral over the
        // whole tenure only becomes true as the occupation closes, so an
        // alive occupation's still-alive value is committed at its founding
        // day (`end_day` falls back to `day` while alive).
        commit_on(
            hornvale_history::OCC_PERSON_YEARS,
            Value::Number(record.core.person_years),
            end_day,
        )?;
        commit_on(
            hornvale_history::OCC_TECH,
            Value::Text(tech_label(record.core.tech).to_string()),
            day,
        )?;
        commit_on(
            hornvale_history::OCC_FUNCTION,
            Value::Text(function_label(record.core.function).to_string()),
            day,
        )?;
        if let Some(cause) = record.core.cause {
            commit_on(
                hornvale_history::OCC_CAUSE,
                Value::Text(cause_label(cause).to_string()),
                end_day,
            )?;
        }
        // `ended_by` only means something once an occupation has actually
        // ended; a still-alive record's `Ended::Nature` default is not a
        // claim worth committing.
        if record.core.ended.is_some() {
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
            Founding::Genesis(vertex) => Value::Number(f64::from(vertex.0)),
            Founding::From(e) => Value::Entity(
                *bake_to_ledger
                    .get(&e)
                    .expect("founded-from names a community minted earlier in this history"),
            ),
        };
        commit_on(hornvale_history::OCC_FOUNDED_FROM, founded_from, day)?;
        commit_on(
            hornvale_history::OCC_NOTABILITY,
            Value::Text(notability_label(record.core.notability).to_string()),
            day,
        )?;
        // THE WORKING (The Winze, spec §4.2). Committed only where there IS
        // one, for the same reason `occ-ended` and `occ-cause` are conditional
        // above: an absent fact is "no claim", while a committed `0.0` would
        // assert a working that is nought metres deep on every farm in the
        // world — 1,239 of seed 42's 1,240 occupations. The decoder's
        // `unwrap_or(0.0)` is the other half of that reading.
        //
        // **Stamped at the END day, like `occ-cause` and for the same
        // reason.** The depth is an integral over the whole tenure, so it only
        // became true as the occupation closed; an as-of-day-N query must not
        // see a dead delving already at its final depth on the day it was
        // founded.
        //
        // A STILL-ALIVE delving falls through `end_day`'s `unwrap_or` to its
        // founding day, and that stamp IS early — it says a living working
        // was already this deep when it was sunk. Recorded rather than fixed:
        // the honest day would be the bake's `now`, which this function does
        // not receive (`emit_history` takes only the `History`), and a living
        // delving's depth is still moving anyway, so no single day is true of
        // it. Every consumer this campaign writes reads the value, not the
        // stamp.
        if record.core.delve_depth_m > 0.0 {
            commit_on(
                hornvale_history::OCC_DELVE_DEPTH,
                Value::Number(record.core.delve_depth_m),
                end_day,
            )?;
        }

        if record.core.is_alive() {
            commit_on(hornvale_settlement::IS_SETTLEMENT, Value::Flag(true), day)?;
            commit_on(
                hornvale_settlement::POPULATION,
                Value::Number(f64::from(record.core.peak_population)),
                day,
            )?;
            commit_on(
                hornvale_settlement::VERTEX_ID,
                Value::Number(f64::from(record.core.site.0)),
                day,
            )?;
        } else {
            commit_on(hornvale_history::IS_RUIN, Value::Flag(true), end_day)?;
        }
    }

    // Epidemic events are paired by (occupation, day, pathogen). Validate that join key
    // before committing either half so malformed bake output cannot leave a
    // plausible orphan fact in the ledger.
    let mut outbreak_keys = BTreeSet::new();
    for (ordinal, event) in h.outbreaks.iter().enumerate() {
        let subject = *bake_to_ledger
            .get(&event.occupation)
            .expect("an outbreak names an occupation minted in this history");
        let day = ledger_day_of_bake_year(event.year);
        assert!(
            outbreak_keys.insert((subject, day.to_bits(), event.pathogen)),
            "one aggregated outbreak event per occupation, day, and pathogen"
        );
        world.ledger.commit(
            fact_at(
                subject,
                hornvale_epidemiology::STRUCK_BY,
                Value::Text(event.pathogen.0.to_string()),
                day,
                Some(outbreak_entities[ordinal]),
            ),
            &world.registry,
        )?;
        world.ledger.commit(
            fact_at(
                subject,
                hornvale_epidemiology::OUTBREAK_DEATHS,
                Value::Number(event.deaths),
                day,
                Some(outbreak_entities[ordinal]),
            ),
            &world.registry,
        )?;
    }

    // The tribute relations still standing at `now` (spec §4.4). One fact per
    // relation, on the SUBORDINATE's subject, carrying its patron's minted
    // entity — the same `Value::Entity` shape `occ-ended-by` uses, dated by the
    // day the relation was established rather than by `now`, because that is
    // the day it became true. Both parties are alive occupations, so both are
    // in `bake_to_ledger`; the relations arrive in subordinate order, so the
    // commit order is deterministic.
    for rel in &h.tribute {
        let subject = *bake_to_ledger
            .get(&rel.subordinate)
            .expect("a tribute subordinate names a community minted in this history");
        let patron = *bake_to_ledger
            .get(&rel.patron)
            .expect("a tribute patron names a community minted in this history");
        // `ledger_day_of_bake_year` quantizes the crossing (see there for why
        // The Escapement makes that load-bearing): the day below therefore
        // carries the SAME canonical instant as each party's committed founding
        // object, so the invariant this fact must honour — a relation is never
        // dated before either community it names — cannot be inverted by the
        // two surfaces disagreeing about one instant.
        world.ledger.commit(
            fact(
                subject,
                hornvale_history::PAYS_TRIBUTE_TO,
                Value::Entity(patron),
                ledger_day_of_bake_year(rel.since),
            ),
            &world.registry,
        )?;
    }
    Ok(())
}

/// Commit the world-level "now" fact: the bake's `end_year` **in standard
/// days**, on `subject`
/// (the world entity, mirroring how astronomy/terrain commit their own
/// world-scalar genesis facts — see `domains/astronomy/src/facts.rs::fact`).
/// Day-stamped 0.0 like those other world constants: `history-now` is an
/// eternal fact about this world's scenario (fixed by `BakeConfig`), not an
/// event that becomes true partway through the timeline, so it belongs with
/// the other genesis-day scalars rather than at `end_year` itself. Reads back
/// via `windows/almanac::history::present_year`, which now trusts this fact
/// instead of approximating the present as the latest occupation event.
///
/// `now` arrives as a bake YEAR (`BakeConfig::end_year`) and crosses into the
/// ledger as a day here, at the same seam every occupation fact crosses.
/// type-audit: bare-ok(count: now)
pub fn emit_now(world: &mut World, subject: EntityId, now: f64) -> Result<(), BuildError> {
    world.ledger.commit(
        Fact {
            subject,
            predicate: hornvale_history::HISTORY_NOW.to_string(),
            object: Value::Number(ledger_day_of_bake_year(now)),
            place: None,
            day: Some(WorldTime::GENESIS),
            provenance: hornvale_history::streams::BAKE.as_str().to_string(),
        },
        &world.registry,
    )?;
    Ok(())
}

/// The present frame's **year**: the latest founding-or-ending recorded
/// anywhere in the world's deep history — the moment "today" sits at, e.g. for
/// measuring a ruin's age back from. Deterministic (`f64::total_cmp` over
/// ledger numbers). Mirrors `windows/almanac::history::present_year`'s read
/// (that window cannot be depended on here — worldgen is the composition root,
/// so this is the shared, non-almanac-specific home for the read); a future
/// cleanup could have the almanac call this one instead of its private copy.
///
/// **Named for its unit** (The Ell). It was `present_day` and returned the
/// ledger's raw number, which was a year wearing the word "day". The ledger now
/// stores days, so the raw read crosses [`bake_year_of_ledger_day`] and the
/// name says which side of that seam the caller is on: every consumer
/// (`vestige`, the almanac's span and flesh prose) subtracts this from an
/// `Occupation`'s `founded`, which is a bake year.
/// type-audit: bare-ok(count: return)
pub fn present_year(world: &World) -> f64 {
    if let Some(now) = world.ledger.find(hornvale_history::HISTORY_NOW).next()
        && let Value::Number(n) = &now.object
    {
        return bake_year_of_ledger_day(*n);
    }
    // Fallback for a world with no committed `history-now` fact (a save from
    // before T8, or a synthetic Lab world that never ran the composition-root
    // bake): approximate the present as the latest committed occupation
    // event. This UNDERSTATES every ruin's age and tenure by the bake's
    // post-history stretch (the true present is `BakeConfig::end_year`, not
    // the last stochastic draw) — see `emit_now` above.
    let founded = world.ledger.find(hornvale_history::OCC_FOUNDED);
    let ended = world.ledger.find(hornvale_history::OCC_ENDED);
    founded
        .chain(ended)
        .filter_map(|f| match &f.object {
            Value::Number(n) => Some(*n),
            _ => None,
        })
        .max_by(|a, b| a.total_cmp(b))
        // The max is taken on the ledger's own axis (the map is monotone, so
        // which fact wins does not depend on the unit) and crossed once, here.
        .map(bake_year_of_ledger_day)
        .unwrap_or(0.0)
}

/// [`present_year`], crossed forward into the standard **day** every
/// [`WorldTime`] consumer needs — the composition a caller wanting "now" as a
/// day would otherwise hand-write as
/// `WorldTime::from_std_days(ledger_day_of_bake_year(present_year(world)))`.
///
/// That hand-written composition is exactly what sat at
/// `windows/worldgen/tests/repose_exposure.rs`'s TASK 7 call, and it reported
/// `tools/seam-guard`'s `ledger_day_of_bake_year` seam UNGUARDED: the call
/// lived only inside a `heavy:`-ignored battery (`exposure_rows_masked`,
/// reachable from no non-`#[ignore]`d test), so no scoped probe could ever
/// observe a year substituted for a day there — and, contrary to this
/// module's usual advice, **widening `scope(...)` could not have fixed it**.
/// A scope only helps a mutation some crate's *non-ignored* tests can reach;
/// this one only ever ran inside `#[ignore]`d code, at every scope. Naming the
/// crossing here instead moves the one call site somewhere a cheap,
/// non-ignored test can reach it —
/// `present_frame_crosses_the_bake_year_by_days_per_year` in this crate's
/// `tests/history_emit.rs`.
pub fn present_frame(world: &World) -> WorldTime {
    WorldTime::from_std_days(ledger_day_of_bake_year(present_year(world)))
        .expect("a derived present-day crossing is finite")
}

/// Reconstruct every committed occupation from the ledger, in commit order —
/// the shared decoder both this window (a future consumer, e.g. The Vestige)
/// and `windows/almanac`'s prose renderer read history back through. Lifted
/// verbatim from the almanac's private `record_of`/`layers_at` (the working
/// reconstruction that decodes exactly how [`emit_history`] encoded each
/// `OCC_*` fact), so a change to the encoding only ever needs one matching
/// decoder to stay in sync.
pub fn occupation_records(world: &World) -> Vec<OccupationRecord> {
    world
        .ledger
        .find(hornvale_history::IS_OCCUPATION)
        .map(|f| f.subject)
        .filter_map(|id| reconstruct_occupation(world, id))
        .collect()
}

/// Occupations on a vertex, oldest-founded first (the palimpsest layers a
/// site's stratigraphy stacks in). Ordered by [`layer_key`]: material facts
/// only (founded, then ended — a still-living occupation sorts last, then
/// peak population, then the predecessor's founding coordinates) — never by
/// mint order, so a site's stratigraphy is a property of the world, not of
/// the order a bake loop happened to mint its entities in.
pub fn occupations_at(world: &World, vertex: Vertex) -> Vec<OccupationRecord> {
    let all = occupation_records(world);
    let coords = founding_coords_by_id(&all);
    let mut v: Vec<OccupationRecord> = all.into_iter().filter(|o| o.core.site == vertex).collect();
    v.sort_by_key(|r| layer_key(r, parent_coords(r, &coords)));
    v
}

/// Every occupation's founding coordinates, by entity — the lookup
/// [`layer_key`]'s ancestry tail needs. `FoundingCoords<'static>` because
/// `Occupation::people` is a `KindId` wrapping a `&'static str`, so this map
/// borrows nothing from `all` and outlives the scan that built it.
pub(crate) fn founding_coords_by_id(
    all: &[OccupationRecord],
) -> BTreeMap<EntityId, FoundingCoords<'static>> {
    all.iter()
        .map(|o| (o.id, founding_coords(&o.core)))
        .collect()
}

/// The founding coordinates of `r`'s predecessor, if it has one and it is
/// present in `coords`.
pub(crate) fn parent_coords(
    r: &OccupationRecord,
    coords: &BTreeMap<EntityId, FoundingCoords<'static>>,
) -> Option<FoundingCoords<'static>> {
    match r.founded_from {
        Founding::From(e) => coords.get(&e).copied(),
        Founding::Genesis(_) => None,
    }
}

/// Every occupation, grouped by `site`, each vertex's vec ordered
/// oldest-founded-first — the batched sibling of [`occupations_at`]: one
/// [`occupation_records`] scan for the whole world instead of one per vertex.
/// Built for The Vestige's per-world field derivations
/// ([`crate::vestige::vestiges_field`]) and the coming census, where calling
/// `occupations_at` per vertex would rescan the ledger `O(vertices)` times. Each
/// vertex's vec is sorted with the exact same [`layer_key`] `occupations_at`
/// uses, so a vertex's entry here is byte-for-byte identical to what
/// `occupations_at(world, vertex)` would produce. The predecessor-coordinates
/// map is built **once** for the whole world, not per vertex — `layer_key`'s
/// ancestry tail can point at a predecessor on any site, so a per-vertex map
/// would miss it.
pub fn occupations_by_vertex(world: &World) -> BTreeMap<Vertex, Vec<OccupationRecord>> {
    let all = occupation_records(world);
    let coords = founding_coords_by_id(&all);
    let mut by_vertex: BTreeMap<Vertex, Vec<OccupationRecord>> = BTreeMap::new();
    for occ in all {
        by_vertex.entry(occ.core.site).or_default().push(occ);
    }
    for occs in by_vertex.values_mut() {
        occs.sort_by_key(|r| layer_key(r, parent_coords(r, &coords)));
    }
    by_vertex
}

/// Reconstruct the [`OccupationRecord`] an occupation entity's committed facts
/// describe — enough of it to render prose and derive flesh. `deity`/`tongue`
/// are never committed as facts, so they are filled with inert placeholders
/// (`None`); the record's own identity is the entity itself (`id: entity`),
/// matching the almanac's original convention. `None` if the entity is
/// missing a load-bearing fact or names a people outside the biosphere
/// roster.
fn reconstruct_occupation(world: &World, entity: EntityId) -> Option<OccupationRecord> {
    let people_label = world.ledger.text_of(entity, hornvale_history::OCC_PEOPLE)?;
    let people = resolve_people(people_label)?;
    let site = Vertex(occ_number(world, entity, hornvale_history::OCC_SITE)? as u32);
    // The inverse crossing: the ledger stores days, an `Occupation` carries the
    // bake's years. Every key derived from this record downstream
    // (`founder_handle`, `material_key`, `founding_key`, `layer_key`) is keyed
    // on the year form, which is what keeps this epoch confined to the ledger's
    // own numbers instead of renaming every founder in every world.
    let founded =
        bake_year_of_ledger_day(occ_number(world, entity, hornvale_history::OCC_FOUNDED)?);
    let ended = occ_number(world, entity, hornvale_history::OCC_ENDED).map(bake_year_of_ledger_day);
    let peak_population = occ_number(world, entity, hornvale_history::OCC_PEAK)? as u32;
    let tech = parse_tech(world.ledger.text_of(entity, hornvale_history::OCC_TECH)?)?;
    let function = parse_function(
        world
            .ledger
            .text_of(entity, hornvale_history::OCC_FUNCTION)?,
    )?;
    let cause = world
        .ledger
        .text_of(entity, hornvale_history::OCC_CAUSE)
        .and_then(parse_cause);
    let notability = parse_notability(
        world
            .ledger
            .text_of(entity, hornvale_history::OCC_NOTABILITY)?,
    )?;
    // ABSENT MEANS NEVER DUG, not "missing" — the emitter commits
    // `occ-delve-depth` only for an occupation that actually drove a working
    // (The Winze, spec §4.2), so this is a defaulting read and never a
    // `?`-return the way the load-bearing facts above are.
    let delve_depth_m = occ_number(world, entity, hornvale_history::OCC_DELVE_DEPTH).unwrap_or(0.0);
    // ABSENT MEANS "saved before The Lot", never "lived nobody": the emitter
    // commits the fact for every occupation, so a 0.0 here is the signature
    // of a pre-campaign world, which `hornvale_lot` refuses by checking that
    // no occupation in the world carries the predicate at all.
    let person_years = occ_number(world, entity, hornvale_history::OCC_PERSON_YEARS).unwrap_or(0.0);
    let ended_by = match world
        .ledger
        .value_of(entity, hornvale_history::OCC_ENDED_BY)
    {
        Some(Value::Entity(e)) => Ended::By(*e),
        _ => Ended::Nature,
    };
    let founded_from = match world
        .ledger
        .value_of(entity, hornvale_history::OCC_FOUNDED_FROM)
    {
        Some(Value::Entity(e)) => Founding::From(*e),
        Some(Value::Number(vertex)) => Founding::Genesis(Vertex(*vertex as u32)),
        _ => Founding::Genesis(site),
    };

    Some(OccupationRecord {
        core: Occupation {
            people,
            site,
            founded,
            ended,
            peak_population,
            tech,
            function,
            deity: None,
            tongue: None,
            cause,
            notability,
            delve_depth_m,
            person_years,
        },
        id: entity,
        founded_from,
        ended_by,
    })
}

/// A functional `Number` object read back as an `f64`.
/// type-audit: bare-ok(count: return)
fn occ_number(world: &World, entity: EntityId, predicate: &str) -> Option<f64> {
    match world.ledger.value_of(entity, predicate) {
        Some(Value::Number(n)) => Some(*n),
        _ => None,
    }
}

fn parse_tech(label: &str) -> Option<TechHorizon> {
    Some(match label {
        "neolithic" => TechHorizon::Neolithic,
        "bronze" => TechHorizon::Bronze,
        "iron" => TechHorizon::Iron,
        "classical" => TechHorizon::Classical,
        _ => return None,
    })
}

fn parse_function(label: &str) -> Option<Function> {
    Some(match label {
        "agrarian" => Function::Agrarian,
        "mine" => Function::Mine,
        "trade" => Function::Trade,
        "cult" => Function::Cult,
        "fort" => Function::Fort,
        _ => return None,
    })
}

fn parse_cause(label: &str) -> Option<CauseOfEnd> {
    Some(match label {
        "famine" => CauseOfEnd::Famine,
        "burned" => CauseOfEnd::Burned,
        "plague" => CauseOfEnd::Plague,
        "fled" => CauseOfEnd::Fled,
        "migrated" => CauseOfEnd::Migrated,
        // The Winze, spec §4.3. **This half of the codec is not enumerated
        // by the compiler** — it matches on a `&str` and falls through to
        // `None` — so a variant added to `cause_label` above and forgotten
        // here would encode fine and decode as "never ended", on every
        // occupation that ended that way. `history_emit`'s round-trip gates
        // are what actually hold the pair together.
        "breached" => CauseOfEnd::Breached,
        _ => return None,
    })
}

fn parse_notability(label: &str) -> Option<Notability> {
    Some(match label {
        "backwater" => Notability::Backwater,
        "common" => Notability::Common,
        "seat" => Notability::Seat,
        _ => return None,
    })
}

/// The dominant people per region: every vertex an alive occupation (a
/// committed `is-settlement`) sits on, grouped by that occupation's people.
/// Reads purely off the ledger — the present-as-query the campaign's
/// keystone names.
pub fn territories(world: &World) -> BTreeMap<KindId, BTreeSet<Vertex>> {
    let mut map: BTreeMap<KindId, BTreeSet<Vertex>> = BTreeMap::new();
    for f in world.ledger.find(hornvale_settlement::IS_SETTLEMENT) {
        let id = f.subject;
        let Some(label) = world.ledger.text_of(id, hornvale_history::OCC_PEOPLE) else {
            continue;
        };
        let Some(people) = resolve_people(label) else {
            continue;
        };
        let Some(Value::Number(vertex)) = world.ledger.value_of(id, hornvale_settlement::VERTEX_ID)
        else {
            continue;
        };
        map.entry(people)
            .or_default()
            .insert(Vertex(*vertex as u32));
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

/// The four goblinoid peoples the campaign seeds its deep history with — the
/// diversity payoff ([`goblinoid_overlap`]) is measured against exactly these.
pub const GOBLINOIDS: [KindId; 4] = [
    KindId("goblin"),
    KindId("kobold"),
    KindId("hobgoblin"),
    KindId("bugbear"),
];

/// The number of **climate** displacement events the bake resolved, read
/// straight off the ledger: an occupation the paleoclimate evicted from a vertex
/// it turned hostile, which relocated to a vacant refuge rather than starving.
/// This equals `census(bake).migrated` exactly — the contract
/// `windows/worldgen/tests/history_gates.rs`'s
/// `migration_events_counts_climate_displacement_only` asserts — so it
/// recovers the bake's climate-displacement tally without replaying the bake,
/// the present-as-query the campaign is built on.
///
/// **Cause `migrated` alone does not mean climate displacement** (The Tumult).
/// A conquest also closes the *conqueror's* abandoned record with
/// `CauseOfEnd::Migrated`: `Bake::maybe_raid` has it leave its poorer land for
/// the prize, an orderly self-directed move under `Ended::Nature`, before it
/// reopens on the seized vertex. Counting every `occ-cause = migrated` fact
/// therefore folds predation into the climate signal (seed 42: 133 such facts
/// against 58 real climate migrations). The two are kept separate — conflict
/// displacement is `census(bake).raided`/`fled`, and the campaign's cascade
/// histogram measures its size distribution.
///
/// The exclusion is a **query-side fold over the committed records**; nothing
/// new is serialized for it. A conquest-relocation is exactly a `migrated`
/// record that *someone else was driven off by*: `maybe_raid` closes the
/// victim `Fled`, `ended-by` the raider's own just-closed record, in the same
/// year. So a `migrated` record is excluded when some occupation's
/// `occ-ended-by` names it and that occupation's `occ-ended` is the same day.
/// A climate eviction has no such contemporaneous victim — it moves onto
/// *vacant* land (`Bake::nearest_dest`), and the only other producer of an
/// `Ended::By` (a cascade hop in `Bake::relocate`) names the roller's *newly
/// opened* record, whose own ending, if it ever comes, falls in a later year.
/// type-audit: bare-ok(count: return)
pub fn migration_events(world: &World) -> u64 {
    // Every occupation closed with cause `migrated`, against the day it closed.
    let migrated: BTreeMap<EntityId, f64> = world
        .ledger
        .find(hornvale_history::OCC_CAUSE)
        .filter(|f| matches!(&f.object, Value::Text(t) if t == "migrated"))
        .filter_map(|f| {
            match world
                .ledger
                .value_of(f.subject, hornvale_history::OCC_ENDED)
            {
                Some(Value::Number(end)) => Some((f.subject, *end)),
                _ => None,
            }
        })
        .collect();

    // The conquerors among them: a record someone else was driven off by, in
    // the very year it moved. `total_cmp` rather than `==` — both are the same
    // scalar committed through the same quantizing boundary, so they compare
    // exactly, and the project bans bare float equality.
    //
    // Deliberately NOT crossed through `bake_year_of_ledger_day`: this compares
    // two raw ledger objects against *each other*, so both sides moved together
    // when the ledger went to days and the equality is unit-free. Converting
    // would be an identical answer computed twice as expensively.
    let conquerors: BTreeSet<EntityId> = world
        .ledger
        .find(hornvale_history::OCC_ENDED_BY)
        .filter_map(|f| match &f.object {
            Value::Entity(by) => Some((f.subject, *by)),
            _ => None,
        })
        .filter(|(victim, by)| {
            let Some(&moved) = migrated.get(by) else {
                return false;
            };
            matches!(
                world.ledger.value_of(*victim, hornvale_history::OCC_ENDED),
                Some(Value::Number(fell)) if moved.total_cmp(fell).is_eq()
            )
        })
        .map(|(_, by)| by)
        .collect();

    (migrated.len() - conquerors.len()) as u64
}

/// The number of occupations the moving sea's paleoclimate ended by
/// starvation — the collapse signal, mirroring [`migration_events`] exactly
/// (same `OCC_CAUSE` predicate, the sibling `cause_label` for `Famine`).
/// Reads committed `occ-cause` facts; no seed draw, no replay of the bake.
/// Measured on the real seed-42 world: 1 collapse of 151 occupations
/// (≈ 0.0066 share) — the depopulation-ceiling gate
/// (`windows/worldgen/tests/history_sundering.rs`) checks this stays a
/// minority; the moving sea corrects the map's climate, it does not empty it.
/// type-audit: bare-ok(count: return)
pub fn collapse_events(world: &World) -> u64 {
    world
        .ledger
        .find(hornvale_history::OCC_CAUSE)
        .filter(|f| matches!(&f.object, Value::Text(t) if t == "famine"))
        .count() as u64
}

/// One inhabited, sea-isolated landmass: a connected component of the
/// *present* connection graph (conductance ≥ the isolation threshold), with
/// the peoples whose alive occupations sit on it. `peoples` holds the raw
/// `OCC_PEOPLE` label text rather than a resolved `KindId` — the
/// isolation-divergence gate only needs stable people *identity*, and
/// `String` ordering is already deterministic, so this sidesteps needing the
/// `WorldComponents`-based interner a pure ledger-readback helper has no
/// access to (mirrors the "compare content, don't reconstruct a `'static`
/// key" idiom [`resolve_people`] uses one level up).
/// type-audit: bare-ok(identifier-text: peoples)
pub struct Landmass {
    /// The component's vertices.
    pub vertices: BTreeSet<Vertex>,
    /// The raw people-label text of every alive occupation on this landmass.
    pub peoples: BTreeSet<String>,
}

/// The inhabited sea-isolated landmasses of a built world: the present
/// connection graph's connected components, filtered to those holding at
/// least one alive settlement. Purely derived from committed facts plus the
/// graph reconstruction ([`crate::graph_derive::connection_graph_of`]) — the
/// graph itself is never committed, so this replays no bake and draws no
/// seed. The isolation threshold (1e-6) matches the almanac's
/// `ISOLATION_THRESHOLD` (`windows/almanac/src/connections.rs`): "genuinely
/// disconnected", not "technically nonzero". Measured on the real seed-42
/// world: 4 inhabited landmasses over the 4 goblinoid peoples, three of which
/// host only a proper subset (2, 2, and 3 of the 4) — the campaign's headline
/// isolation-predicts-divergence payoff
/// (`windows/worldgen/tests/history_sundering.rs`): a people that could not
/// physically cross the water never diverges into a landmass it never
/// reached.
pub fn sundered_landmasses(world: &World) -> Vec<Landmass> {
    let graph = crate::graph_derive::connection_graph_of(
        world,
        &crate::graph_derive::GraphConfig::default(),
    );

    let mut site_people: BTreeMap<Vertex, String> = BTreeMap::new();
    for s in hornvale_settlement::all_settlements(world) {
        let Some(Value::Number(vertex)) =
            world.ledger.value_of(s.id, hornvale_settlement::VERTEX_ID)
        else {
            continue;
        };
        let Some(label) = world.ledger.text_of(s.id, hornvale_history::OCC_PEOPLE) else {
            continue;
        };
        site_people.insert(Vertex(*vertex as u32), label.to_string());
    }

    graph
        .reachable_regions(1e-6)
        .into_iter()
        .filter_map(|vertices| {
            let peoples: BTreeSet<String> = vertices
                .iter()
                .filter_map(|c| site_people.get(c).cloned())
                .collect();
            if peoples.is_empty() {
                None
            } else {
                Some(Landmass { vertices, peoples })
            }
        })
        .collect()
}

/// How many neighbour rings a people's occupied vertices are dilated by to form
/// its *region of influence* for [`goblinoid_region_overlap`]. One ring — a
/// vertex plus its immediate neighbours — is the natural "territory around a
/// settlement". The raw point-sets ([`territories`]) are structurally disjoint
/// (each vertex hosts at most one alive settlement), so their Jaccard is always
/// 0; dilation is what turns "distinct vertices" into the meaningful "distinct
/// regions" the diversity payoff is really about.
/// type-audit: bare-ok(count)
/// plumb: pending(wave-1)
pub const TERRITORY_DILATION_RINGS: u32 = 1;

/// Mean pairwise Jaccard overlap of the four [`GOBLINOIDS`]' raw territory
/// vertex-sets ([`territories`]). Because each vertex hosts at most one alive
/// settlement, these sets are structurally disjoint, so this is 0.0 on any
/// well-formed world — it is a *disjointness sanity check* (no vertex is
/// double-claimed), NOT the separation metric. For the real spatial-separation
/// measurement use [`goblinoid_region_overlap`]. Cross-platform byte-identical
/// (integer set-cardinality arithmetic only).
/// type-audit: bare-ok(ratio: return)
pub fn goblinoid_overlap(world: &World) -> f64 {
    let terr = territories(world);
    let sets: Vec<BTreeSet<Vertex>> = GOBLINOIDS
        .iter()
        .map(|k| terr.get(k).cloned().unwrap_or_default())
        .collect();
    mean_pairwise_jaccard(&sets)
}

/// Mean pairwise Jaccard overlap of the four [`GOBLINOIDS`]' *regions of
/// influence* — each people's occupied vertices dilated by
/// [`TERRITORY_DILATION_RINGS`] neighbour rings. THIS is the peoples-diversity
/// payoff, measured: 0.0 = fully separated countries, rising toward 1.0 as
/// peoples interleave. Regions that merely abut overlap only along their
/// shared border; genuinely mixed peoples overlap heavily. A people absent
/// from the world contributes an empty region (overlap 0). Reconstructs the
/// world's geosphere (deterministic topology) to walk neighbours, so the value
/// is cross-platform byte-identical (integer set arithmetic only).
/// type-audit: bare-ok(ratio: return)
// Named construction site (decision 0092): reconstructs terrain for its own
// overlap readout.
#[allow(clippy::disallowed_methods)]
pub fn goblinoid_region_overlap(world: &World) -> f64 {
    let terrain = crate::terrain_of(world).expect("a built world's terrain reconstructs");
    let geo = terrain.geosphere();
    let terr = territories(world);
    let sets: Vec<BTreeSet<Vertex>> = GOBLINOIDS
        .iter()
        .map(|k| {
            let base = terr.get(k).cloned().unwrap_or_default();
            let mut region = base;
            for _ in 0..TERRITORY_DILATION_RINGS {
                let mut next = region.clone();
                for &c in &region {
                    for &n in geo.neighbors(c) {
                        next.insert(n);
                    }
                }
                region = next;
            }
            region
        })
        .collect();
    mean_pairwise_jaccard(&sets)
}

/// Mean Jaccard overlap over all unordered pairs of the given vertex-sets
/// (0.0 for an empty pair). Integer set-cardinality arithmetic — deterministic.
fn mean_pairwise_jaccard(sets: &[BTreeSet<Vertex>]) -> f64 {
    let (mut sum, mut pairs) = (0.0, 0.0);
    for i in 0..sets.len() {
        for j in (i + 1)..sets.len() {
            let inter = sets[i].intersection(&sets[j]).count();
            let union = sets[i].union(&sets[j]).count();
            let jaccard = if union == 0 {
                0.0
            } else {
                inter as f64 / union as f64
            };
            sum += jaccard;
            pairs += 1.0;
        }
    }
    if pairs == 0.0 { 0.0 } else { sum / pairs }
}

/// A stratigraphy readout: how deep occupation stacks up per site, and whether
/// depth tracks land quality. A "site" is any vertex that ever held an
/// occupation; its layer count is how many occupations (alive or ruined, over
/// all of deep time) sat on it — a stack ≥ 2 is a re-occupation, the mark of a
/// site worth returning to.
/// type-audit: bare-ok(count: occupied_sites), bare-ok(count: restacked_sites), bare-ok(ratio: restacked_fraction), bare-ok(ratio: depth_capacity_correlation)
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Stratigraphy {
    /// Vertices that ever held at least one occupation.
    pub occupied_sites: u64,
    /// Occupied sites re-occupied at least once (≥ 2 layers — a stack).
    pub restacked_sites: u64,
    /// `restacked_sites / occupied_sites` (0.0 if none) — the fraction of sites
    /// that accreted a stratigraphy. Cross-platform stable (integer division).
    pub restacked_fraction: f64,
    /// Spearman rank correlation between a site's layer count and its mean peak
    /// population — the ledger's own cross-platform-stable capacity signal
    /// (peak population is capacity-limited by the bake's `pressure =
    /// population / eff_capacity`). Positive ⇒ deep stacks sit on productive
    /// land. Only basic arithmetic and IEEE `sqrt` (both cross-platform
    /// deterministic), so this value is drift-check-safe.
    pub depth_capacity_correlation: f64,
}

/// Read the [`Stratigraphy`] off the ledger: group every occupation by its
/// `occ-site` vertex, count layers, and correlate depth against mean peak
/// population. Pure present-as-query — no bake replay, no `HashMap`.
pub fn stratigraphy(world: &World) -> Stratigraphy {
    // vertex -> (layer count, summed peak population).
    let mut by_vertex: BTreeMap<u32, (u64, f64)> = BTreeMap::new();
    for f in world.ledger.find(hornvale_history::IS_OCCUPATION) {
        let id = f.subject;
        let Some(Value::Number(vertex)) = world.ledger.value_of(id, hornvale_history::OCC_SITE)
        else {
            continue;
        };
        let peak = match world.ledger.value_of(id, hornvale_history::OCC_PEAK) {
            Some(Value::Number(p)) => *p,
            _ => 0.0,
        };
        let entry = by_vertex.entry(*vertex as u32).or_insert((0, 0.0));
        entry.0 += 1;
        entry.1 += peak;
    }
    let occupied_sites = by_vertex.len() as u64;
    let restacked_sites = by_vertex.values().filter(|(c, _)| *c >= 2).count() as u64;
    let restacked_fraction = if occupied_sites == 0 {
        0.0
    } else {
        restacked_sites as f64 / occupied_sites as f64
    };
    let depths: Vec<f64> = by_vertex.values().map(|(c, _)| *c as f64).collect();
    let capacities: Vec<f64> = by_vertex
        .values()
        .map(|(c, sum)| *sum / *c as f64)
        .collect();
    let depth_capacity_correlation = rank_correlation(&depths, &capacities);
    Stratigraphy {
        occupied_sites,
        restacked_sites,
        restacked_fraction,
        depth_capacity_correlation,
    }
}

/// Spearman rank correlation of paired samples: rank each axis (average ranks
/// for ties, ordered by `f64::total_cmp`), then Pearson-correlate the ranks.
/// `HashMap`-free and deterministic. Returns 0.0 for fewer than two points or
/// a zero-variance axis.
fn rank_correlation(xs: &[f64], ys: &[f64]) -> f64 {
    let rx = average_ranks(xs);
    let ry = average_ranks(ys);
    pearson(&rx, &ry)
}

/// Fractional ranks (1-based) for `vs`, averaging tied groups — the standard
/// Spearman tie handling. Ordered by `f64::total_cmp` (total & deterministic).
fn average_ranks(vs: &[f64]) -> Vec<f64> {
    let mut idx: Vec<usize> = (0..vs.len()).collect();
    idx.sort_by(|&a, &b| vs[a].total_cmp(&vs[b]));
    let mut ranks = vec![0.0; vs.len()];
    let mut i = 0;
    while i < idx.len() {
        let mut j = i + 1;
        while j < idx.len() && vs[idx[j]] == vs[idx[i]] {
            j += 1;
        }
        // Average of the 1-based ranks (i+1)..=j is ((i+1) + j) / 2.
        let avg = ((i + 1 + j) as f64) / 2.0;
        for &k in &idx[i..j] {
            ranks[k] = avg;
        }
        i = j;
    }
    ranks
}

/// Pearson correlation of two equal-length samples (basic arithmetic and IEEE
/// `sqrt` only — both cross-platform deterministic). 0.0 if degenerate.
fn pearson(xs: &[f64], ys: &[f64]) -> f64 {
    let n = xs.len() as f64;
    if n < 2.0 {
        return 0.0;
    }
    let mx = xs.iter().sum::<f64>() / n;
    let my = ys.iter().sum::<f64>() / n;
    let (mut sxy, mut sxx, mut syy) = (0.0, 0.0, 0.0);
    for (x, y) in xs.iter().zip(ys) {
        sxy += (x - mx) * (y - my);
        sxx += (x - mx) * (x - mx);
        syy += (y - my) * (y - my);
    }
    if sxx == 0.0 || syy == 0.0 {
        0.0
    } else {
        sxy / (sxx * syy).sqrt()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{SettlementPins, build_world};
    use hornvale_astronomy::SkyPins;
    use hornvale_terrain::TerrainPins;

    #[test]
    fn occupation_records_reconstruct_from_the_ledger() {
        let world = build_world(
            hornvale_kernel::Seed(42),
            &SkyPins::default(),
            &TerrainPins::default(),
            &SettlementPins::default(),
        )
        .unwrap();

        let recs = occupation_records(&world);
        assert!(!recs.is_empty(), "seed 42 has occupations");

        // A reconstructed record round-trips its site + lifecycle.
        let r = &recs[0];
        assert!(r.core.founded >= 0.0);

        // `occupations_at` groups by vertex — the same layer must show up
        // among the records at its own site.
        let at = occupations_at(&world, r.core.site);
        assert!(at.iter().any(|o| o.core.founded == r.core.founded));

        // Every returned occupation genuinely sits on the queried vertex, and
        // the layers come back oldest-founded first.
        assert!(at.iter().all(|o| o.core.site == r.core.site));
        assert!(
            at.windows(2)
                .all(|w| w[0].core.founded <= w[1].core.founded),
            "occupations_at must order oldest-founded first"
        );
    }
}

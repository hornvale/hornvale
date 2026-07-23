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
use hornvale_history::record::{
    CauseOfEnd, Ended, Founding, Function, Notability, OccupationRecord, TechHorizon,
};
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

/// Commit the world-level "now" fact: the bake's `end_year`, on `subject`
/// (the world entity, mirroring how astronomy/terrain commit their own
/// world-scalar genesis facts — see `domains/astronomy/src/facts.rs::fact`).
/// Day-stamped 0.0 like those other world constants: `history-now` is an
/// eternal fact about this world's scenario (fixed by `BakeConfig`), not an
/// event that becomes true partway through the timeline, so it belongs with
/// the other genesis-day scalars rather than at `end_year` itself. Reads back
/// via `windows/almanac::history::present_day`, which now trusts this fact
/// instead of approximating the present as the latest occupation event.
/// type-audit: bare-ok(count: now)
pub fn emit_now(world: &mut World, subject: EntityId, now: f64) -> Result<(), BuildError> {
    world.ledger.commit(
        Fact {
            subject,
            predicate: hornvale_history::HISTORY_NOW.to_string(),
            object: Value::Number(now),
            place: None,
            day: Some(0.0),
            provenance: hornvale_history::streams::BAKE.as_str().to_string(),
        },
        &world.registry,
    )?;
    Ok(())
}

/// The present frame's day: the latest founding-or-ending recorded anywhere in
/// the world's deep history — the moment "today" sits at, e.g. for measuring
/// a ruin's age back from. Deterministic (`f64::total_cmp` over ledger
/// numbers). Mirrors `windows/almanac::history::present_day`'s read (that
/// window cannot be depended on here — worldgen is the composition root, so
/// this is the shared, non-almanac-specific home for the read); a future
/// cleanup could have the almanac call this one instead of its private copy.
/// type-audit: bare-ok(count: return)
pub fn present_day(world: &World) -> f64 {
    if let Some(now) = world.ledger.find(hornvale_history::HISTORY_NOW).next()
        && let Value::Number(n) = &now.object
    {
        return *n;
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
        .unwrap_or(0.0)
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

/// Occupations on a cell, oldest-founded first (the palimpsest layers a
/// site's stratigraphy stacks in). Ordered by `founded` via `f64::total_cmp`
/// (total and deterministic), with the occupation's own entity id (carried in
/// [`OccupationRecord::community`], the placeholder `reconstruct_occupation`
/// fills with the minting entity) breaking a same-day tie.
pub fn occupations_at(world: &World, cell: CellId) -> Vec<OccupationRecord> {
    let mut v: Vec<OccupationRecord> = occupation_records(world)
        .into_iter()
        .filter(|o| o.site == cell)
        .collect();
    v.sort_by(|a, b| {
        a.founded
            .total_cmp(&b.founded)
            .then(a.community.0.cmp(&b.community.0))
    });
    v
}

/// Reconstruct the [`OccupationRecord`] an occupation entity's committed facts
/// describe — enough of it to render prose and derive flesh. The fields a
/// derived readout never needs (`community`/`lineage`/`deity`/`tongue`) are
/// filled with inert placeholders (the entity's own id), matching the
/// almanac's original convention. `None` if the entity is missing a
/// load-bearing fact or names a people outside the biosphere roster.
fn reconstruct_occupation(world: &World, entity: EntityId) -> Option<OccupationRecord> {
    let people_label = world.ledger.text_of(entity, hornvale_history::OCC_PEOPLE)?;
    let people = resolve_people(people_label)?;
    let site = CellId(occ_number(world, entity, hornvale_history::OCC_SITE)? as u32);
    let founded = occ_number(world, entity, hornvale_history::OCC_FOUNDED)?;
    let ended = occ_number(world, entity, hornvale_history::OCC_ENDED);
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
        Some(Value::Number(cell)) => Founding::Genesis(CellId(*cell as u32)),
        _ => Founding::Genesis(site),
    };

    Some(OccupationRecord {
        people,
        community: entity,
        lineage: entity,
        site,
        founded,
        ended,
        peak_population,
        tech,
        function,
        deity: None,
        tongue: None,
        cause,
        ended_by,
        founded_from,
        notability,
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

/// The four goblinoid peoples the campaign seeds its deep history with — the
/// diversity payoff ([`goblinoid_overlap`]) is measured against exactly these.
pub const GOBLINOIDS: [KindId; 4] = [
    KindId("goblin"),
    KindId("kobold"),
    KindId("hobgoblin"),
    KindId("bugbear"),
];

/// The number of migration events the bake resolved, read straight off the
/// ledger: every occupation that ended `migrated` — a community relocated off
/// a cell the paleoclimate turned hostile — committed an `occ-cause` fact with
/// that label. This equals `census(bake).migrated` exactly (each migration
/// event closes one occupation record with cause `Migrated`), so it recovers
/// the bake's displacement tally without replaying the bake — the
/// present-as-query the campaign is built on. Migration, not raiding, is the
/// real world's displacement signal (raids ≈ 0 on the ample vacant land of a
/// real paleoclimate; raid-driven displacement is deferred to campaign C3).
/// type-audit: bare-ok(count: return)
pub fn migration_events(world: &World) -> u64 {
    world
        .ledger
        .find(hornvale_history::OCC_CAUSE)
        .filter(|f| matches!(&f.object, Value::Text(t) if t == "migrated"))
        .count() as u64
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
    /// The component's cells.
    pub cells: BTreeSet<CellId>,
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

    let mut site_people: BTreeMap<CellId, String> = BTreeMap::new();
    for s in hornvale_settlement::all_settlements(world) {
        let Some(Value::Number(cell)) = world.ledger.value_of(s.id, hornvale_settlement::CELL_ID)
        else {
            continue;
        };
        let Some(label) = world.ledger.text_of(s.id, hornvale_history::OCC_PEOPLE) else {
            continue;
        };
        site_people.insert(CellId(*cell as u32), label.to_string());
    }

    graph
        .reachable_regions(1e-6)
        .into_iter()
        .filter_map(|cells| {
            let peoples: BTreeSet<String> = cells
                .iter()
                .filter_map(|c| site_people.get(c).cloned())
                .collect();
            if peoples.is_empty() {
                None
            } else {
                Some(Landmass { cells, peoples })
            }
        })
        .collect()
}

/// How many neighbour rings a people's occupied cells are dilated by to form
/// its *region of influence* for [`goblinoid_region_overlap`]. One ring — a
/// cell plus its immediate neighbours — is the natural "territory around a
/// settlement". The raw point-sets ([`territories`]) are structurally disjoint
/// (each cell hosts at most one alive settlement), so their Jaccard is always
/// 0; dilation is what turns "distinct cells" into the meaningful "distinct
/// regions" the diversity payoff is really about.
/// type-audit: bare-ok(count)
pub const TERRITORY_DILATION_RINGS: u32 = 1;

/// Mean pairwise Jaccard overlap of the four [`GOBLINOIDS`]' raw territory
/// cell-sets ([`territories`]). Because each cell hosts at most one alive
/// settlement, these sets are structurally disjoint, so this is 0.0 on any
/// well-formed world — it is a *disjointness sanity check* (no cell is
/// double-claimed), NOT the separation metric. For the real spatial-separation
/// measurement use [`goblinoid_region_overlap`]. Cross-platform byte-identical
/// (integer set-cardinality arithmetic only).
/// type-audit: bare-ok(ratio: return)
pub fn goblinoid_overlap(world: &World) -> f64 {
    let terr = territories(world);
    let sets: Vec<BTreeSet<CellId>> = GOBLINOIDS
        .iter()
        .map(|k| terr.get(k).cloned().unwrap_or_default())
        .collect();
    mean_pairwise_jaccard(&sets)
}

/// Mean pairwise Jaccard overlap of the four [`GOBLINOIDS`]' *regions of
/// influence* — each people's occupied cells dilated by
/// [`TERRITORY_DILATION_RINGS`] neighbour rings. THIS is the peoples-diversity
/// payoff, measured: 0.0 = fully separated countries, rising toward 1.0 as
/// peoples interleave. Regions that merely abut overlap only along their
/// shared border; genuinely mixed peoples overlap heavily. A people absent
/// from the world contributes an empty region (overlap 0). Reconstructs the
/// world's geosphere (deterministic topology) to walk neighbours, so the value
/// is cross-platform byte-identical (integer set arithmetic only).
/// type-audit: bare-ok(ratio: return)
pub fn goblinoid_region_overlap(world: &World) -> f64 {
    let terrain = crate::terrain_of(world).expect("a built world's terrain reconstructs");
    let geo = terrain.geosphere();
    let terr = territories(world);
    let sets: Vec<BTreeSet<CellId>> = GOBLINOIDS
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

/// Mean Jaccard overlap over all unordered pairs of the given cell-sets
/// (0.0 for an empty pair). Integer set-cardinality arithmetic — deterministic.
fn mean_pairwise_jaccard(sets: &[BTreeSet<CellId>]) -> f64 {
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
/// depth tracks land quality. A "site" is any cell that ever held an
/// occupation; its layer count is how many occupations (alive or ruined, over
/// all of deep time) sat on it — a stack ≥ 2 is a re-occupation, the mark of a
/// site worth returning to.
/// type-audit: bare-ok(count: occupied_sites), bare-ok(count: restacked_sites), bare-ok(ratio: restacked_fraction), bare-ok(ratio: depth_capacity_correlation)
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Stratigraphy {
    /// Cells that ever held at least one occupation.
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
/// `occ-site` cell, count layers, and correlate depth against mean peak
/// population. Pure present-as-query — no bake replay, no `HashMap`.
pub fn stratigraphy(world: &World) -> Stratigraphy {
    // cell -> (layer count, summed peak population).
    let mut by_cell: BTreeMap<u32, (u64, f64)> = BTreeMap::new();
    for f in world.ledger.find(hornvale_history::IS_OCCUPATION) {
        let id = f.subject;
        let Some(Value::Number(cell)) = world.ledger.value_of(id, hornvale_history::OCC_SITE)
        else {
            continue;
        };
        let peak = match world.ledger.value_of(id, hornvale_history::OCC_PEAK) {
            Some(Value::Number(p)) => *p,
            _ => 0.0,
        };
        let entry = by_cell.entry(*cell as u32).or_insert((0, 0.0));
        entry.0 += 1;
        entry.1 += peak;
    }
    let occupied_sites = by_cell.len() as u64;
    let restacked_sites = by_cell.values().filter(|(c, _)| *c >= 2).count() as u64;
    let restacked_fraction = if occupied_sites == 0 {
        0.0
    } else {
        restacked_sites as f64 / occupied_sites as f64
    };
    let depths: Vec<f64> = by_cell.values().map(|(c, _)| *c as f64).collect();
    let capacities: Vec<f64> = by_cell.values().map(|(c, sum)| *sum / *c as f64).collect();
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
    use crate::{SettlementPins, SkyChoice, build_world};
    use hornvale_astronomy::SkyPins;
    use hornvale_terrain::TerrainPins;

    #[test]
    fn occupation_records_reconstruct_from_the_ledger() {
        let world = build_world(
            hornvale_kernel::Seed(42),
            &SkyPins::default(),
            SkyChoice::Generated,
            &TerrainPins::default(),
            &SettlementPins::default(),
        )
        .unwrap();

        let recs = occupation_records(&world);
        assert!(!recs.is_empty(), "seed 42 has occupations");

        // A reconstructed record round-trips its site + lifecycle.
        let r = &recs[0];
        assert!(r.founded >= 0.0);

        // `occupations_at` groups by cell — the same layer must show up
        // among the records at its own site.
        let at = occupations_at(&world, r.site);
        assert!(at.iter().any(|o| o.founded == r.founded));

        // Every returned occupation genuinely sits on the queried cell, and
        // the layers come back oldest-founded first.
        assert!(at.iter().all(|o| o.site == r.site));
        assert!(
            at.windows(2).all(|w| w[0].founded <= w[1].founded),
            "occupations_at must order oldest-founded first"
        );
    }
}

//! The flesh derivations must not move when an entity id moves (The Salt).
//!
//! `flesh_seed` re-derives an occupation's [`Occupation`] core from the
//! ledger via `record_of`, which reads nothing for an entity with no
//! committed facts — so a fixture that never touches the ledger would make
//! every entity here collapse onto the same `unwrap_or(0)` fallback
//! regardless of its actual core, passing (and failing to notice a
//! regression) for the wrong reason. `commit_occupation` below commits the
//! exact fact set `record_of` reads (mirroring
//! `windows/worldgen/src/history_emit.rs`'s pattern; that crate itself can't
//! be a dev-dependency here — `windows/worldgen` already depends on
//! `windows/almanac`, so the edge can't run the other way, per
//! `connections_render.rs`'s note), so each fixture entity is a real,
//! independently keyed occupation.

use hornvale_history::flesh::{Departure, residue_of, structures_of};
use hornvale_history::record::{
    CauseOfEnd, Ended, Founding, Function, Notability, Occupation, OccupationRecord, TechHorizon,
};
use hornvale_history::{
    OCC_CAUSE, OCC_ENDED, OCC_FOUNDED, OCC_FOUNDED_FROM, OCC_FUNCTION, OCC_NOTABILITY, OCC_PEAK,
    OCC_PEOPLE, OCC_SITE, OCC_TECH,
};
use hornvale_kernel::{CellId, EntityId, Fact, KindId, Seed, Value, World, WorldTime};

/// The stable text label for a tech horizon (round-trippable via `OCC_TECH`;
/// mirrors `windows/worldgen/src/history_emit.rs`'s private helper of the
/// same name, duplicated here because that crate cannot be a dev-dependency
/// of `hornvale-almanac`).
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

/// Commit exactly the facts `windows/almanac/src/history.rs::record_of`
/// reads, so `flesh_seed(world, id)` reconstructs `core` byte-for-byte and
/// actually exercises `material_key` instead of falling back to a shared
/// `unwrap_or(0)` on a ledger with nothing about `id` in it.
fn commit_occupation(world: &mut World, id: EntityId, core: &Occupation) {
    // The Ell: an `Occupation` carries the bake's YEARS and the ledger speaks
    // standard DAYS, so this fixture must cross at the same boundary
    // `history_emit::ledger_day_of_bake_year` does — otherwise `record_of` would
    // divide a year by 365.25 and reconstruct a *different* core, quietly
    // falsifying this file's whole premise (see the module doc: "reconstructs
    // `core` byte-for-byte"). That premise was prose until now;
    // `the_flesh_seed_round_trips_the_core_it_was_committed_from` below asserts
    // it. `hornvale-worldgen` cannot be a dev-dependency here (it already
    // depends on this crate), so the constant is shared, not the function.
    let cross = |year: f64| year * hornvale_kernel::Years::DAYS_PER_YEAR;
    let day = cross(core.founded);
    let end_day = core.ended.map_or(day, cross);
    let mut commit = |predicate: &str, object: Value, day: f64| {
        world
            .ledger
            .commit(
                Fact {
                    subject: id,
                    predicate: predicate.to_string(),
                    object,
                    place: Some(id),
                    day: Some(WorldTime::new(day).expect("test fixture day is finite")),
                    provenance: "test-fixture".to_string(),
                },
                &world.registry,
            )
            .expect("fixture fact must be committable");
    };

    commit(OCC_PEOPLE, Value::Text(core.people.0.to_string()), day);
    commit(OCC_SITE, Value::Number(f64::from(core.site.0)), day);
    // Objects cross too, exactly as the stamps do — `occ-founded`/`occ-ended`
    // name days on the ledger's own axis.
    commit(OCC_FOUNDED, Value::Number(day), day);
    if core.ended.is_some() {
        commit(OCC_ENDED, Value::Number(end_day), end_day);
    }
    commit(
        OCC_PEAK,
        Value::Number(f64::from(core.peak_population)),
        day,
    );
    commit(
        OCC_TECH,
        Value::Text(tech_label(core.tech).to_string()),
        day,
    );
    commit(
        OCC_FUNCTION,
        Value::Text(function_label(core.function).to_string()),
        day,
    );
    if let Some(cause) = core.cause {
        commit(
            OCC_CAUSE,
            Value::Text(cause_label(cause).to_string()),
            end_day,
        );
    }
    commit(
        OCC_NOTABILITY,
        Value::Text(notability_label(core.notability).to_string()),
        day,
    );
    commit(OCC_FOUNDED_FROM, Value::Number(f64::from(core.site.0)), day);
}

fn dead_core() -> Occupation {
    Occupation {
        people: KindId("gnoll"),
        site: CellId(1400),
        founded: 500.0,
        ended: Some(675.0),
        peak_population: 240,
        tech: TechHorizon::Classical,
        function: Function::Agrarian,
        deity: None,
        tongue: None,
        cause: Some(CauseOfEnd::Famine),
        notability: Notability::Seat,
    }
}

/// Build a fresh world, commit one occupation with `core` under `id`, and
/// hand back the `OccupationRecord` a real reconstruction of it would
/// produce (matching what `flesh_seed`'s own `record_of` call will see).
fn world_with_occupation(id: u64, core: Occupation) -> (World, OccupationRecord) {
    let mut world = World::new(Seed(42));
    hornvale_history::register_concepts(&mut world.registry).expect("register history concepts");
    let entity = EntityId::new(id).expect("nonzero");
    commit_occupation(&mut world, entity, &core);
    let record = OccupationRecord {
        core,
        id: entity,
        founded_from: Founding::Genesis(CellId(1400)),
        ended_by: Ended::Nature,
    };
    (world, record)
}

/// This file's stated premise, asserted instead of claimed.
///
/// The module doc says `commit_occupation` commits "the exact fact set
/// `record_of` reads, so `flesh_seed(world, id)` reconstructs `core`
/// byte-for-byte". Nothing checked that, and The Ell made it briefly false: the
/// ledger moved to days while this fixture still wrote years, so `record_of`
/// divided a year by 365.25 and reconstructed a core the fixture never
/// committed. Every test here still passed, because they all compare two
/// fixtures against *each other* and both were wrong the same way.
///
/// `flesh_seed` (ledger-side, through `record_of`) and `flesh_seed_for`
/// (record-side, straight off the core) are the two producers of one key. This
/// asserts they agree, which is the round trip: the crossing on the way in and
/// the crossing on the way out must compose to the identity, or the almanac
/// derives a different assemblage of structures and residue for a community
/// than `hornvale_history::flesh` derives from the bake's own record.
#[test]
fn the_flesh_seed_round_trips_the_core_it_was_committed_from() {
    // Both arms of `record_of`'s `ended` decode: a ruin (`Some`) and a
    // still-standing community (`None`), so neither crossing is untested.
    let alive = Occupation {
        ended: None,
        cause: None,
        ..dead_core()
    };
    for core in [dead_core(), alive] {
        let (world, record) = world_with_occupation(4242, core.clone());
        assert_eq!(
            hornvale_almanac::history::flesh_seed(&world, record.id),
            hornvale_almanac::history::flesh_seed_for(&world, &core),
            "the ledger-side and record-side flesh seeds disagree — `record_of` \
             did not reconstruct the core that was committed"
        );
    }
}

#[test]
fn both_flesh_derivations_ignore_the_entity_id() {
    let now = 2000.0;
    for shift in [1u64, 1000, 999_999] {
        let (world_a, a) = world_with_occupation(4242, dead_core());
        let (world_b, b) = world_with_occupation(4242 + shift, dead_core());
        let sa = hornvale_almanac::history::flesh_seed(&world_a, a.id);
        let sb = hornvale_almanac::history::flesh_seed(&world_b, b.id);
        assert_eq!(
            structures_of(&a, sa),
            structures_of(&b, sb),
            "structures moved under an id shift of {shift}"
        );
        // The residue half — NOT exercised by the committed gallery artifact,
        // whose showcase layer is still alive and so renders no ruin.
        assert_eq!(
            residue_of(&a, now, sa, Departure::Climate).items,
            residue_of(&b, now, sb, Departure::Climate).items,
            "residue moved under an id shift of {shift}"
        );
    }
}

#[test]
fn the_flesh_seed_still_separates_materially_different_occupations() {
    let a_core = dead_core();
    let mut b_core = dead_core();
    // Perturb a field `structures_of` never reads directly (only
    // `material_key` reads `notability`), so a difference in the result can
    // only come from the derived seed, not from `structures_of` reading the
    // differing field itself. This is deliberate: an earlier version of this
    // test perturbed `peak_population`, which `structures_of` also reads
    // directly, so it stayed green even when `flesh_seed` was mutated to
    // return a constant seed — it was asserting nothing about the seed.
    b_core.notability = Notability::Backwater;

    let (world_a, a) = world_with_occupation(1, a_core);
    let (world_b, b) = world_with_occupation(2, b_core);
    assert_ne!(
        structures_of(&a, hornvale_almanac::history::flesh_seed(&world_a, a.id)),
        structures_of(&b, hornvale_almanac::history::flesh_seed(&world_b, b.id)),
        "a materially different occupation must still differ"
    );
}

/// The Salt (spec D5): `conquest_victim`'s tie-break must not read an entity
/// id's value.
///
/// Its candidate set is never larger than one — measured at 1718 candidate
/// calls across seeds 42/7/1000, maximum set size 1 — so the id was breaking
/// a tie that has never occurred, while still putting an id's value in a
/// render path. The tie-break survives (if a second candidate ever does
/// appear, the choice should be a stated property of the world rather than
/// commit order); only its key changed.
///
/// A source scan rather than a behavioural assertion, deliberately: the
/// behaviour under a second candidate cannot be exercised from a real world,
/// because no real world produces one. What CAN be pinned is the rule.
#[test]
fn the_conquest_tie_break_reads_no_entity_id() {
    let src = include_str!("../../src/history.rs");
    let after = src
        .split("fn conquest_victim")
        .nth(1)
        .expect("conquest_victim exists");
    let body = &after[..after.find("\n}\n").expect("function ends")];

    // Shapes that read an `EntityId` for its VALUE. `min_by_key` itself is
    // NOT banned — the function still breaks its tie, just on material facts.
    for banned in [".0.get()", "u64::from(", "e.get()"] {
        assert!(
            !body.contains(banned),
            "conquest_victim reads an entity id's value ({banned}); \
             the tie-break must key on material facts (The Salt, spec D5)"
        );
    }
    // ...and it must still be breaking the tie on something.
    assert!(
        body.contains("min_by_key"),
        "conquest_victim must still choose deterministically among candidates"
    );
    assert!(
        body.contains("OCC_SITE") && body.contains("OCC_FOUNDED"),
        "the tie-break must key on the victim's own site and founding day"
    );
}

//! Task 4: committing the occupation skeleton to the ledger, present-as-
//! query, and territories. Everything here runs against a hand-built
//! `History` on a fresh test `World` — no real bake, no double-placer
//! conflict (retiring the draft placer is Task 5's job).

use hornvale_history::IS_RUIN;
use hornvale_history::record::{
    CauseOfEnd, Ended, Founding, Function, Notability, OccupationRecord, TechHorizon,
};
use hornvale_kernel::{CellId, EntityId, KindId, Seed, World};
use hornvale_worldgen::{History, emit_history, ruins_of_people, territories};

fn eid(n: u64) -> EntityId {
    EntityId::new(n).unwrap()
}

fn test_world() -> World {
    let mut w = World::new(Seed(42));
    hornvale_history::register_concepts(&mut w.registry).unwrap();
    hornvale_settlement::register_concepts(&mut w.registry).unwrap();
    w
}

/// A record with every "un-set" field filled with a neutral default, so each
/// test case only spells out what it cares about.
fn base_record(community: u64, people: &'static str, site: u32, founded: f64) -> OccupationRecord {
    OccupationRecord {
        people: KindId(people),
        community: eid(community),
        lineage: eid(community),
        site: CellId(site),
        founded,
        ended: None,
        peak_population: 50,
        tech: TechHorizon::Neolithic,
        function: Function::Agrarian,
        deity: None,
        tongue: None,
        cause: None,
        ended_by: Ended::Nature,
        founded_from: Founding::Genesis(CellId(site)),
        notability: Notability::Common,
    }
}

/// Four records: two alive (goblin, kobold), two goblin ruins — one that
/// simply starved (`Ended::Nature`), one that fled a raid by the still-alive
/// goblin community (`Ended::By`, `Founding::From` chained off the OTHER
/// ruin) — exercising every `Value` shape `emit_history` must commit.
fn hand_history() -> History {
    let alive_goblin = base_record(1, "goblin", 0, 0.0);

    let mut starved_goblin = base_record(2, "goblin", 1, 0.0);
    starved_goblin.ended = Some(100.0);
    starved_goblin.cause = Some(CauseOfEnd::Famine);
    starved_goblin.notability = Notability::Backwater;

    let alive_kobold = base_record(3, "kobold", 2, 50.0);

    let mut fled_goblin = base_record(4, "goblin", 3, 10.0);
    fled_goblin.ended = Some(60.0);
    fled_goblin.cause = Some(CauseOfEnd::Fled);
    fled_goblin.ended_by = Ended::By(eid(1)); // raided by the alive goblin community
    fled_goblin.founded_from = Founding::From(eid(2)); // settlers from the starved ruin

    History::new(
        vec![alive_goblin, starved_goblin, alive_kobold, fled_goblin],
        200.0,
    )
}

fn alive_count(h: &History) -> usize {
    h.records.iter().filter(|r| r.is_alive()).count()
}

#[test]
fn the_present_is_the_live_occupations() {
    let mut w = test_world();
    emit_history(&mut w, &hand_history()).unwrap();
    let settlements = hornvale_settlement::all_settlements(&w);
    // Every is-settlement subject is an alive occupation; counts match.
    assert_eq!(settlements.len(), alive_count(&hand_history()));
}

#[test]
fn the_deep_past_is_queryable_not_replayed() {
    let mut w = test_world();
    emit_history(&mut w, &hand_history()).unwrap();
    let goblin_ruins = ruins_of_people(&w, KindId("goblin"));
    assert!(!goblin_ruins.is_empty());
    assert_eq!(goblin_ruins.len(), 2, "both goblin ruins must be found");
    // Provenance points back at the bake for each.
    for e in &goblin_ruins {
        assert!(w.ledger.facts_about(*e).any(|f| f.predicate == IS_RUIN));
    }
    // A kobold query finds no ruins (the kobold occupation is still alive).
    assert!(ruins_of_people(&w, KindId("kobold")).is_empty());
}

#[test]
fn founded_from_and_ended_by_resolve_to_the_right_entities() {
    let mut w = test_world();
    emit_history(&mut w, &hand_history()).unwrap();
    let goblin_ruins = ruins_of_people(&w, KindId("goblin"));
    // The fled ruin's `founded-from` must resolve to the starved ruin's own
    // minted entity, and its `ended-by` to the alive goblin community's —
    // proving the bake-internal ids were translated to ledger ids, not
    // dropped or left dangling.
    let fled = goblin_ruins
        .iter()
        .copied()
        .find(|&e| {
            w.ledger
                .facts_about(e)
                .any(|f| f.predicate == hornvale_history::OCC_CAUSE)
                && matches!(
                    w.ledger.text_of(e, hornvale_history::OCC_CAUSE),
                    Some("fled")
                )
        })
        .expect("the fled ruin must be among the goblin ruins");
    let starved = goblin_ruins
        .iter()
        .copied()
        .find(|&e| e != fled)
        .expect("the starved ruin must also be among the goblin ruins");

    assert_eq!(
        w.ledger
            .value_of(fled, hornvale_history::OCC_FOUNDED_FROM)
            .cloned(),
        Some(hornvale_kernel::Value::Entity(starved))
    );
    let alive_goblin_id = hornvale_settlement::all_settlements(&w)
        .into_iter()
        .find(|s| {
            w.ledger
                .text_of(s.id, hornvale_history::OCC_PEOPLE)
                .is_some_and(|p| p == "goblin")
        })
        .expect("the alive goblin settlement must exist")
        .id;
    assert_eq!(
        w.ledger
            .value_of(fled, hornvale_history::OCC_ENDED_BY)
            .cloned(),
        Some(hornvale_kernel::Value::Entity(alive_goblin_id))
    );
}

#[test]
fn territories_group_alive_occupations_by_people() {
    let mut w = test_world();
    emit_history(&mut w, &hand_history()).unwrap();
    let t = territories(&w);
    assert_eq!(t.get(&KindId("goblin")).unwrap(), &[CellId(0)].into());
    assert_eq!(t.get(&KindId("kobold")).unwrap(), &[CellId(2)].into());
    // Dead occupations never contribute a cell to any territory.
    let all_cells: std::collections::BTreeSet<CellId> = t.values().flatten().copied().collect();
    assert!(!all_cells.contains(&CellId(1)));
    assert!(!all_cells.contains(&CellId(3)));
}

#[test]
fn emit_is_deterministic() {
    let mut a = test_world();
    let mut b = test_world();
    emit_history(&mut a, &hand_history()).unwrap();
    emit_history(&mut b, &hand_history()).unwrap();
    assert_eq!(
        serde_json::to_string(&a.ledger).unwrap(),
        serde_json::to_string(&b.ledger).unwrap(),
        "same history must emit byte-identical ledgers"
    );
}

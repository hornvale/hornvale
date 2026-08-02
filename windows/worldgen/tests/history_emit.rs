//! Task 4: committing the occupation skeleton to the ledger, present-as-
//! query, and territories. Everything here runs against a hand-built
//! `History` on a fresh test `World` — no real bake, no double-placer
//! conflict (retiring the draft placer is Task 5's job).

use hornvale_history::IS_RUIN;
use hornvale_history::record::{
    CauseOfEnd, Ended, Founding, Function, Notability, Occupation, TechHorizon, layer_key,
};
use hornvale_kernel::{CellId, EntityId, KindId, Seed, World};
use hornvale_worldgen::{
    BakeId, BakeOccupation, History, SkyChoice, TributeRelation, build_world, emit_history,
    occupation_records, occupations_at, occupations_by_cell, ruins_of_people, territories,
};

/// A bake-local handle for these hand-built fixtures — every `History` this
/// file constructs is hand-built, standing in for what a real bake would have
/// produced, so its `community`/`lineage`/`founded_from`/`ended_by` handles
/// are `BakeId`s, never `EntityId`s (that translation is `emit_history`'s job).
fn bid(n: u64) -> BakeId {
    BakeId(n)
}

fn test_world() -> World {
    let mut w = World::new(Seed(42));
    hornvale_history::register_concepts(&mut w.registry).unwrap();
    hornvale_settlement::register_concepts(&mut w.registry).unwrap();
    w
}

/// A record with every "un-set" field filled with a neutral default, so each
/// test case only spells out what it cares about.
fn base_record(community: u64, people: &'static str, site: u32, founded: f64) -> BakeOccupation {
    BakeOccupation {
        core: Occupation {
            people: KindId(people),
            site: CellId(site),
            founded,
            ended: None,
            peak_population: 50,
            tech: TechHorizon::Neolithic,
            function: Function::Agrarian,
            deity: None,
            tongue: None,
            cause: None,
            notability: Notability::Common,
        },
        community: bid(community),
        lineage: bid(community),
        founded_from: Founding::Genesis(CellId(site)),
        ended_by: Ended::Nature,
    }
}

/// Four records: two alive (goblin, kobold), two goblin ruins — one that
/// simply starved (`Ended::Nature`), one that fled a raid by the still-alive
/// goblin community (`Ended::By`, `Founding::From` chained off the OTHER
/// ruin) — exercising every `Value` shape `emit_history` must commit.
fn hand_history() -> History {
    let alive_goblin = base_record(1, "goblin", 0, 0.0);

    let mut starved_goblin = base_record(2, "goblin", 1, 0.0);
    starved_goblin.core.ended = Some(100.0);
    starved_goblin.core.cause = Some(CauseOfEnd::Famine);
    starved_goblin.core.notability = Notability::Backwater;

    let alive_kobold = base_record(3, "kobold", 2, 50.0);

    let mut fled_goblin = base_record(4, "goblin", 3, 10.0);
    fled_goblin.core.ended = Some(60.0);
    fled_goblin.core.cause = Some(CauseOfEnd::Fled);
    fled_goblin.ended_by = Ended::By(bid(1)); // raided by the alive goblin community
    fled_goblin.founded_from = Founding::From(bid(2)); // settlers from the starved ruin

    History::new(
        vec![alive_goblin, starved_goblin, alive_kobold, fled_goblin],
        200.0,
    )
}

fn alive_count(h: &History) -> usize {
    h.records.iter().filter(|r| r.core.is_alive()).count()
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
fn a_standing_tribute_relation_is_committed_as_a_dated_entity_fact() {
    // Spec §4.4: the relation lives only inside the bake, and is made legible
    // exactly as an occupation's `ended-by` is — one registered predicate
    // carrying `Value::Entity(patron)` on the SUBORDINATE's subject, dated by
    // the day the relation was established. Nothing about it is a new `Fact`
    // shape, and the direction is load-bearing: a reader must be able to ask
    // "who does this community pay?" and get one answer.
    let mut w = test_world();
    let mut h = hand_history();
    h.tribute = vec![TributeRelation {
        subordinate: bid(3), // the alive kobold community
        patron: bid(1),      // …pays the alive goblin one
        since: 120.0,
    }];
    emit_history(&mut w, &h).unwrap();

    let settlements = hornvale_settlement::all_settlements(&w);
    let of_people = |people: &str| {
        settlements
            .iter()
            .find(|s| {
                w.ledger
                    .text_of(s.id, hornvale_history::OCC_PEOPLE)
                    .is_some_and(|p| p == people)
            })
            .expect("both alive occupations must be settlements")
            .id
    };
    let goblin = of_people("goblin");
    let kobold = of_people("kobold");

    assert_eq!(
        w.ledger
            .value_of(kobold, hornvale_history::PAYS_TRIBUTE_TO)
            .cloned(),
        Some(hornvale_kernel::Value::Entity(goblin)),
        "the subordinate must name its patron's MINTED entity"
    );
    let fact = w
        .ledger
        .facts_about(kobold)
        .find(|f| f.predicate == hornvale_history::PAYS_TRIBUTE_TO)
        .expect("the relation must be committed");
    assert_eq!(
        fact.day,
        Some(120.0),
        "dated by the day the relation was established, not by `now`"
    );
    assert!(
        w.ledger
            .value_of(goblin, hornvale_history::PAYS_TRIBUTE_TO)
            .is_none(),
        "the patron pays nobody: the fact goes on the subordinate alone"
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
fn end_of_life_facts_are_day_stamped_at_ended_not_founded() {
    let mut w = test_world();
    let mut ruin = base_record(1, "goblin", 0, 100.0);
    ruin.core.ended = Some(900.0);
    ruin.core.cause = Some(CauseOfEnd::Burned);
    let h = History::new(vec![ruin], 1000.0);
    emit_history(&mut w, &h).unwrap();

    let ruins = ruins_of_people(&w, KindId("goblin"));
    assert_eq!(ruins.len(), 1);
    let ruin_id = ruins[0];

    // End-of-life facts are stamped at `ended` (900.0), not `founded`
    // (100.0) — the day each of these actually became true.
    let is_ruin = w
        .ledger
        .facts_about(ruin_id)
        .find(|f| f.predicate == IS_RUIN)
        .expect("IS_RUIN must be committed for a dead occupation");
    assert_eq!(is_ruin.day, Some(900.0));

    let occ_ended = w
        .ledger
        .facts_about(ruin_id)
        .find(|f| f.predicate == hornvale_history::OCC_ENDED)
        .expect("OCC_ENDED must be committed for a dead occupation");
    assert_eq!(occ_ended.day, Some(900.0));

    let occ_cause = w
        .ledger
        .facts_about(ruin_id)
        .find(|f| f.predicate == hornvale_history::OCC_CAUSE)
        .expect("OCC_CAUSE must be committed for a dead occupation");
    assert_eq!(occ_cause.day, Some(900.0));

    // Founding facts stay stamped at `founded` (100.0).
    let occ_founded = w
        .ledger
        .facts_about(ruin_id)
        .find(|f| f.predicate == hornvale_history::OCC_FOUNDED)
        .expect("OCC_FOUNDED must be committed");
    assert_eq!(occ_founded.day, Some(100.0));

    let occ_site = w
        .ledger
        .facts_about(ruin_id)
        .find(|f| f.predicate == hornvale_history::OCC_SITE)
        .expect("OCC_SITE must be committed");
    assert_eq!(occ_site.day, Some(100.0));
}

#[test]
fn occupation_records_round_trip_every_committed_field() {
    // Task 1 (The Vestige): `occupation_records`/`occupations_at` are the
    // lifted shared decoder (ported from the almanac's private
    // `record_of`/`layers_at`). This proves it is the true inverse of
    // `emit_history`'s encoder — every field `hand_history` set comes back
    // out exactly, for every `Value` shape the fixture exercises (a plain
    // alive record, a nature-ended ruin, and an `Ended::By`/`Founding::From`
    // chained ruin).
    let mut w = test_world();
    emit_history(&mut w, &hand_history()).unwrap();

    let recs = occupation_records(&w);
    assert_eq!(
        recs.len(),
        4,
        "one reconstructed record per committed occupation"
    );

    let alive_goblin = recs
        .iter()
        .find(|r| r.core.site == CellId(0))
        .expect("alive goblin at cell 0");
    assert_eq!(alive_goblin.core.people, KindId("goblin"));
    assert_eq!(alive_goblin.core.founded, 0.0);
    assert_eq!(alive_goblin.core.ended, None);
    assert_eq!(alive_goblin.core.peak_population, 50);
    assert_eq!(alive_goblin.core.tech, TechHorizon::Neolithic);
    assert_eq!(alive_goblin.core.function, Function::Agrarian);
    assert_eq!(alive_goblin.core.notability, Notability::Common);
    assert_eq!(alive_goblin.core.cause, None);
    assert_eq!(alive_goblin.ended_by, Ended::Nature);
    assert_eq!(alive_goblin.founded_from, Founding::Genesis(CellId(0)));

    let starved_goblin = recs
        .iter()
        .find(|r| r.core.site == CellId(1))
        .expect("starved goblin at cell 1");
    assert_eq!(starved_goblin.core.ended, Some(100.0));
    assert_eq!(starved_goblin.core.cause, Some(CauseOfEnd::Famine));
    assert_eq!(starved_goblin.core.notability, Notability::Backwater);

    let alive_kobold = recs
        .iter()
        .find(|r| r.core.site == CellId(2))
        .expect("alive kobold at cell 2");
    assert_eq!(alive_kobold.core.people, KindId("kobold"));
    assert_eq!(alive_kobold.core.founded, 50.0);

    let fled_goblin = recs
        .iter()
        .find(|r| r.core.site == CellId(3))
        .expect("fled goblin at cell 3");
    assert_eq!(fled_goblin.core.ended, Some(60.0));
    assert_eq!(fled_goblin.core.cause, Some(CauseOfEnd::Fled));
    // The ★ threads: `founded-from` resolves to the starved ruin's own
    // minted entity, `ended-by` to the alive goblin community's — the same
    // resolution `founded_from_and_ended_by_resolve_to_the_right_entities`
    // checks against raw ledger facts, now checked through the decoded
    // `OccupationRecord`.
    assert_eq!(fled_goblin.founded_from, Founding::From(starved_goblin.id));
    assert_eq!(fled_goblin.ended_by, Ended::By(alive_goblin.id));

    // `occupations_at` finds exactly the one occupation at each site (this
    // fixture never restacks a site), and reports it oldest-founded first.
    for r in &recs {
        let at = occupations_at(&w, r.core.site);
        assert_eq!(at.len(), 1);
        assert_eq!(at[0].core.founded, r.core.founded);
    }
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

/// A fresh world with the concepts the material-comparator tests below need
/// committed — same registration `test_world` already does, under the name
/// those tests use.
fn world_with_registry() -> World {
    test_world()
}

/// Commit one hand-built occupation directly into `w`'s ledger via
/// `emit_history` (a one-record `History`, the same committing style
/// `hand_history` uses), returning the entity minted for it. Finds the new
/// entity by set difference against the `is-occupation` subjects already
/// present before the commit, so it works regardless of how many occupations
/// already sit on `w`'s ledger.
fn commit_occupation(
    w: &mut World,
    site: CellId,
    founded: f64,
    ended: Option<f64>,
    peak_population: u32,
) -> EntityId {
    let before: std::collections::BTreeSet<EntityId> = w
        .ledger
        .find(hornvale_history::IS_OCCUPATION)
        .map(|f| f.subject)
        .collect();

    let mut record = base_record(1, "goblin", site.0, founded);
    record.core.ended = ended;
    record.core.peak_population = peak_population;
    let h = History::new(vec![record], ended.unwrap_or(founded) + 1.0);
    emit_history(w, &h).unwrap();

    w.ledger
        .find(hornvale_history::IS_OCCUPATION)
        .map(|f| f.subject)
        .find(|e| !before.contains(e))
        .expect("emit_history must mint exactly one new occupation entity")
}

#[test]
fn same_day_layers_order_by_material_facts_not_mint_order() {
    // Three occupations of one cell, founded the same day. The one that
    // ended FIRST lies deepest. The one still alive (`ended: None`) is the
    // TOP layer, not the bottom — getting that backward inverts the
    // stratigraphy for every site with a survivor. Mint order is
    // deliberately arranged to disagree with BOTH placements, so a
    // mint-order comparator fails every assertion below.
    //
    // Commit order (and why): `none_end` first (so it gets the SMALLEST
    // entity id, even though it must sort LAST materially), `late_end`
    // second, `early_end` last (so it gets the LARGEST id, even though it
    // must sort FIRST materially). Ascending-id order therefore reads
    // none_end, late_end, early_end — backward on every pair. Committing in
    // an order that let mint order agree with material order on any pair
    // would let the old comparator pass that pair by coincidence, and the
    // guard below would never fire.
    let mut w = world_with_registry();
    let none_end = commit_occupation(&mut w, CellId(4), 100.0, None, 20);
    let late_end = commit_occupation(&mut w, CellId(4), 100.0, Some(900.0), 20);
    let early_end = commit_occupation(&mut w, CellId(4), 100.0, Some(150.0), 20);
    assert!(
        none_end.get() < late_end.get() && late_end.get() < early_end.get(),
        "fixture must mint in exactly this (materially-backward) order, or the test proves nothing"
    );

    let layers = occupations_at(&w, CellId(4));
    assert_eq!(layers.len(), 3);
    assert_eq!(
        layers[0].id, early_end,
        "the layer that closed first lies deepest, whatever order it was minted in"
    );
    assert_eq!(
        layers[1].id, late_end,
        "the layer that closed second lies in the middle, whatever order it was minted in"
    );
    assert_eq!(
        layers[2].id, none_end,
        "a still-living occupation is the TOP layer, not the bottom, whatever order it was minted in"
    );
}

#[test]
fn the_layer_comparator_is_total_on_the_live_corpus() {
    // No two distinct occupations of one site may compare Equal. If they do,
    // the sort falls back to input order — the exact failure this campaign
    // removes. Measured before implementation: (founded, ended, peak) alone
    // ties on 6 records in seed 42, which `founded_from` is there to break.
    let mut pairs = 0u64;
    for seed in [42u64, 7, 1000] {
        let w = build_world(
            Seed(seed),
            &Default::default(),
            SkyChoice::Generated,
            &Default::default(),
            &Default::default(),
        )
        .expect("builds");
        for (cell, occs) in occupations_by_cell(&w) {
            for i in 0..occs.len() {
                for j in (i + 1)..occs.len() {
                    pairs += 1;
                    assert_ne!(
                        layer_key(&occs[i]),
                        layer_key(&occs[j]),
                        "seed {seed}, cell {cell:?}: two layers compare equal"
                    );
                }
            }
        }
    }
    assert!(
        pairs > 0,
        "compared zero occupation pairs across seeds 42/7/1000 — this test proves nothing \
         until at least one site restacks (pairs={pairs})"
    );
}

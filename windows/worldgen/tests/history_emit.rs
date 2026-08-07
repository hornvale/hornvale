//! Task 4: committing the occupation skeleton to the ledger, present-as-
//! query, and territories. Everything here runs against a hand-built
//! `History` on a fresh test `World` — no real bake, no double-placer
//! conflict (retiring the draft placer is Task 5's job).

use hornvale_history::IS_RUIN;
use hornvale_history::record::{
    CauseOfEnd, Ended, Founding, FoundingCoords, Function, Notability, Occupation,
    OccupationRecord, TechHorizon, founding_coords, layer_key,
};
use hornvale_kernel::{CellId, EntityId, KindId, Seed, World};
use hornvale_worldgen::{
    BakeId, BakeOccupation, History, SkyChoice, TributeRelation, build_world, emit_history,
    occupation_records, occupations_at, occupations_by_cell, ruins_of_people, territories,
};
use std::collections::BTreeMap;

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

/// A world-wide predecessor lookup, mirroring what
/// `hornvale_worldgen::history_emit`'s two decoders build internally
/// (privately) to resolve `layer_key`'s ancestry tail. Rebuilt here rather
/// than exposed from the crate because the point of this test is to exercise
/// the same *public* contract those decoders offer, with the same
/// world-wide context they use to resolve a predecessor.
fn coords_by_id(all: &[OccupationRecord]) -> BTreeMap<EntityId, FoundingCoords<'static>> {
    all.iter()
        .map(|o| (o.id, founding_coords(&o.core)))
        .collect()
}

/// The founding coordinates of `r`'s predecessor, if it has one and it is
/// present in `coords`.
fn parent_of(
    r: &OccupationRecord,
    coords: &BTreeMap<EntityId, FoundingCoords<'static>>,
) -> Option<FoundingCoords<'static>> {
    match r.founded_from {
        Founding::From(e) => coords.get(&e).copied(),
        Founding::Genesis(_) => None,
    }
}

#[test]
fn distinct_layers_tie_only_on_genuine_material_matches() {
    // Before The Salt, this test asserted the comparator was TOTAL: the
    // fourth key ordered on the predecessor's `EntityId`, which is always
    // unique, so no two distinct occupations at a site could ever compare
    // Equal. The material fourth key (spec D3/D4) drops that guarantee ON
    // PURPOSE: two occupations identical in every material fact -- including
    // their predecessor's founding coordinates -- are SUPPOSED to tie, the
    // same way `material_key` (C1) does (spec D3: "collisions are the
    // correct output, not a defect to be broken").
    //
    // Measured on the live corpus: 5 tying pairs total -- 4 at seed 42 (a
    // same-day hobgoblin founder/flee chain at cell 29352, three
    // occupations each `founded == ended == 1650.0` whose distinct
    // predecessors happen to share identical founding coordinates, plus one
    // more such pair at cell 29653), 0 at seed 7, 1 at seed 1000 (a
    // same-day gnoll chain at cell 6536). A different count means the key's
    // tie conditions changed and needs re-reading, not a bumped number.
    //
    // What this test still asserts, and always will: a tie is never a BUG.
    // Whenever `layer_key` ties for two distinct occupations, their own
    // (founded, ended, peak) and -- when a predecessor resolves -- its
    // founding coordinates are themselves equal, so the key is doing
    // exactly what its definition says, not silently colliding two
    // occupations the world actually distinguishes.
    //
    // The Generalist re-pin (2026-08-03): human joining the coexistence
    // stack redecided deep-history settlement survival at all three seeds,
    // which moved which occupations tie under the material key. Re-measured
    // on the live corpus: 3 tying pairs total -- 0 at seed 42 (the
    // hobgoblin founder/flee chain no longer ties), 1 at seed 7 (newly
    // ties), 2 at seed 1000 (the gnoll chain still ties, plus one more).
    // Re-read, not assumed: this task's own diff touches no code this test
    // exercises (`layer_key`, `legacy_layer_key`, `occupations_by_cell` are
    // all outside `domains/language`), so the movement is entirely a
    // consequence of human's biosphere/niche rows redeciding settlement
    // placement, the same class of collateral this file's own history
    // already documents for The Salt.
    //
    // The Tolerance re-pin (2026-08-04): the raid gate stopped reading a
    // per-people `threat_response` constant and started reading a value drawn
    // per settlement, so which communities raid — and therefore which
    // occupations open, close and restack — moved on every world with
    // settlements. Re-measured on the live corpus: 1 tying pair total -- 0 at
    // seed 42, 0 at seed 7 (the pair that newly tied under The Generalist no
    // longer does), 1 at seed 1000. Re-read rather than assumed, by the same
    // argument as above: Task 4's diff touches `Bake::takes_the_initiative` and
    // its plumbing, not `layer_key`/`legacy_layer_key`/`occupations_by_cell`,
    // so the key's tie CONDITIONS are untouched and only the corpus they run
    // over moved. The per-tie assertions inside the loop — the invariant this
    // test actually defends — all still hold; it is the corroborating count
    // that moved.
    let mut pairs = 0u64;
    let mut ties = 0u64;
    for seed in [42u64, 7, 1000] {
        let w = build_world(
            Seed(seed),
            &Default::default(),
            SkyChoice::Generated,
            &Default::default(),
            &Default::default(),
        )
        .expect("builds");
        let coords = coords_by_id(&occupation_records(&w));
        for (cell, occs) in occupations_by_cell(&w) {
            for i in 0..occs.len() {
                for j in (i + 1)..occs.len() {
                    pairs += 1;
                    let a = &occs[i];
                    let b = &occs[j];
                    let pa = parent_of(a, &coords);
                    let pb = parent_of(b, &coords);
                    if layer_key(a, pa) == layer_key(b, pb) {
                        ties += 1;
                        assert_eq!(
                            (a.core.founded, a.core.ended, a.core.peak_population),
                            (b.core.founded, b.core.ended, b.core.peak_population),
                            "seed {seed}, cell {cell:?}: tie without matching own material facts"
                        );
                        assert_eq!(
                            pa, pb,
                            "seed {seed}, cell {cell:?}: tie without matching predecessor coordinates"
                        );
                    }
                }
            }
        }
    }
    assert!(
        pairs > 0,
        "compared zero occupation pairs across seeds 42/7/1000 — this test proves nothing \
         until at least one site restacks (pairs={pairs})"
    );
    // THE DELVERS, FIRST PASS (C2c, 2026-08-07): 1 -> 0, the count reaching
    // its FLOOR. Five settling peoples redecided deep-history settlement
    // survival at all three seeds and the one surviving tying pair (the
    // seed-1000 gnoll chain) stopped tying: 2688 pairs compared, 0 ties. That
    // was recorded as a COST rather than bumped, because with nothing tying,
    // the per-tie assertions in the loop above — the invariant this test is
    // named for — were vacuous, and the `pairs > 0` guard cannot tell a
    // working key from a key that never ties.
    //
    // THE DELVERS, SECOND PASS (same day): 0 -> 2, and THE WITNESS IS BACK.
    // Correcting the dwarf diets off the `MINERAL` trophic axis onto
    // `DETRITUS` moved three of the five dwarves' competition and redecided
    // survival again. Measured: 3422 pairs, 2 ties, BOTH at seed 1000 — the
    // same world that has carried this witness through every prior re-pin, so
    // the instrument returned to where it has always lived rather than
    // landing somewhere new. The per-tie assertions above are load-bearing
    // again, and nothing had to be chosen to make that true.
    //
    // Worth reading against its sibling: `the_material_fourth_key_barely_
    // moves_the_stratigraphy` stayed at 0/0/0 across this same change. Ties
    // came back; RESTACKING did not. Those are different properties of the
    // key — two occupations comparing equal, versus the fourth key changing
    // an order — and only the first has recovered its witness.
    assert_eq!(
        ties, 2,
        "measured 0 (seed 42) + 0 (seed 7) + 2 (seed 1000) = 2 tying pairs on the live \
         corpus; a different count means the key's tie conditions changed"
    );
}

/// Mirrors `domains/history::record::day_key` (private there) purely so
/// [`legacy_layer_key`] orders `founded`/`ended` the same way the real key
/// does — the point of the comparison below is the FOURTH key, not this one.
fn day_key(x: f64) -> u64 {
    let b = x.to_bits();
    if b >> 63 == 1 { !b } else { b | 1 << 63 }
}

/// The pre-Salt fourth key: two descended layers ordered on the
/// predecessor's raw `EntityId` rather than its founding coordinates. Lives
/// ONLY in this test, to measure the order-change delta The Salt causes —
/// never exported, since reading an id's value for anything but lookup is
/// exactly what this campaign forbids everywhere else (spec D7).
fn legacy_layer_key(r: &OccupationRecord) -> (u64, u8, u64, std::cmp::Reverse<u32>, u8, u64) {
    let founded = day_key(r.core.founded);
    let (ended_rank, ended) = match r.core.ended {
        Some(d) => (0u8, day_key(d)),
        None => (1u8, 0),
    };
    let (from_rank, from) = match r.founded_from {
        Founding::Genesis(c) => (0u8, u64::from(c.0)),
        Founding::From(e) => (1u8, e.get()),
    };
    (
        founded,
        ended_rank,
        ended,
        std::cmp::Reverse(r.core.peak_population),
        from_rank,
        from,
    )
}

/// The Salt (V3): re-keying `layer_key`'s tail off the predecessor's
/// `EntityId` and onto its founding coordinates changes the rendered order at
/// exactly one site across seeds 42, 7 and 1000 -- measured before
/// implementation (spec §4, V3).
///
/// The Generalist re-pin (2026-08-03): human joining the coexistence stack
/// redecided seed 42's deep-history settlement outcome, and a second site
/// there now restacks under the material key — seed 42's count moves 0 -> 1.
/// Seeds 7 and 1000 are unmoved.
///
/// The Tolerance re-pin (2026-08-04): warlikeness became a per-settlement draw
/// instead of a per-species constant, redeciding deep-history settlement
/// survival at all three seeds. Re-measured: 42 -> 0, 7 -> 2, 1000 -> 1. The
/// claim is unchanged and is still the one The Salt froze — the material fourth
/// key barely moves the stratigraphy, three restacking sites across three
/// worlds — only the witness moved with the corpus underneath it.
///
/// The Keeping step B re-pin (2026-08-04, on main): `CarryingInput.habitable`
/// decomposed to `is_land`, opening the arid/very-hot bands to low capacity.
/// Main measured 42 -> 1, 7 -> 6, 1000 -> 0 on ITS side of the fork; seed 7
/// gained the most newly-reachable ground (3,126 cells, 16.4% of its land,
/// against 0.6% on seed 42), so more of its cells carry stacked occupations for
/// the fourth key to reorder.
///
/// MERGE re-pin (2026-08-04, main absorbed into the-tolerance): both changes
/// redecide settlement survival, so the composed counts are neither branch's
/// (0/2/1) nor main's (1/6/0) — RE-MEASURED on the merged tree: **42 -> 0,
/// 7 -> 0, 1000 -> 1**. The CLAIM is unchanged and is still the one The Salt
/// froze: the material fourth key barely moves the stratigraphy — here a single
/// restacking site across three worlds of ~19k land cells each, which is
/// "barely" a fortiori.
///
/// The Tense re-pin (2026-08-05): capacity gained an era axis, redeciding
/// settlement survival once more, and the counts move **0/0/1 -> 0/1/0**. The
/// TOTAL is unchanged at a single restacking site across three worlds of ~19k
/// land cells each, so the claim this test exists for -- the material fourth
/// key barely moves the stratigraphy -- is exactly as true and as "barely".
/// Only the witness moved, from seed 1000 to seed 7.
///
/// NOTE ON THE WITNESS. At 0/1/0 this test is close to degenerate: a dead
/// fourth key would read 0/0/0 and only seed 7 separates the two. It is
/// retained as the BLAST-RADIUS measurement it has always been, not as the
/// mechanism's pin — `same_day_layers_order_by_material_facts_not_mint_order`
/// asserts the fourth key's behaviour directly and fails if it stops working.
///
/// **THE DELVERS (C2c, 2026-08-07): the witness is now GONE — 0/1/0 -> 0/0/0.**
/// The note above named this exact reading as the degenerate one, and five
/// settling peoples redeciding settlement survival is what produced it. On
/// this corpus the measurement can no longer distinguish the material fourth
/// key from a dead one, so it is a green test that proves nothing about the
/// key. It is re-pinned at the measured zeros rather than deleted, because it
/// remains a real BLAST-RADIUS reading (the claim it was frozen for — "the
/// material fourth key barely moves the stratigraphy" — is if anything more
/// true at zero than at one), and because the mechanism is separately and
/// non-vacuously pinned by
/// `same_day_layers_order_by_material_facts_not_mint_order`, which is green.
/// Choosing a new witnessing seed would be a change to the instrument, not a
/// re-pin, and is recorded here rather than made silently.
#[test]
fn the_material_fourth_key_barely_moves_the_stratigraphy() {
    for (seed, expected) in [(42u64, 0usize), (7, 0), (1000, 0)] {
        let w = build_world(
            Seed(seed),
            &Default::default(),
            SkyChoice::Generated,
            &Default::default(),
            &Default::default(),
        )
        .expect("builds");
        let by_cell = occupations_by_cell(&w);
        let changed = by_cell
            .values()
            .filter(|group| group.len() > 1)
            .filter(|group| {
                // Today's order is what `occupations_by_cell` returns (the
                // new, material fourth key). Compare against a re-sort keyed
                // on the PREDECESSOR'S ENTITY ID -- the key this task
                // removes.
                let mut old: Vec<_> = (*group).clone();
                old.sort_by_key(legacy_layer_key);
                old.iter().map(|r| r.id).collect::<Vec<_>>()
                    != group.iter().map(|r| r.id).collect::<Vec<_>>()
            })
            .count();
        assert_eq!(changed, expected, "seed {seed}: order changes");
    }
}

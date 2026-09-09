//! Task 4: committing the occupation skeleton to the ledger, present-as-
//! query, and territories. Everything here runs against a hand-built
//! `History` on a fresh test `World` — no real bake, no double-placer
//! conflict (retiring the draft placer is Task 5's job).

use hornvale_history::IS_RUIN;
use hornvale_history::record::{
    CauseOfEnd, Ended, Founding, FoundingCoords, Function, Notability, Occupation,
    OccupationRecord, TechHorizon, founding_coords, layer_key,
};
use hornvale_kernel::{EntityId, KindId, Seed, Vertex, World, WorldTime};
use hornvale_worldgen::{
    BakeId, BakeOccupation, BuildDepth, History, OutbreakEvent, SettlementPins, TributeRelation,
    WorldComponents, build_world, build_world_to, emit_history, occupation_records, occupations_at,
    occupations_by_vertex, ruins_of_people, territories,
};
use std::collections::BTreeMap;

/// A bake-local handle for these hand-built fixtures — every `History` this
/// file constructs is hand-built, standing in for what a real bake would have
/// produced, so its `community`/`lineage`/`founded_from`/`ended_by` handles
/// are `BakeId`s, never `EntityId`s (that translation is `emit_history`'s job).
fn bid(n: u64) -> BakeId {
    BakeId(n)
}

/// Break caught: the composition root registers the predicates but never runs
/// the phase, or emits one half without its mate on a real deterministic bake.
#[test]
fn seed_42_emits_paired_outbreak_history_and_plague_endings() {
    let components = WorldComponents::assemble().unwrap();
    let world = build_world_to(
        Seed(42),
        &hornvale_astronomy::SkyPins::default(),
        &hornvale_terrain::TerrainPins::default(),
        &SettlementPins::default(),
        &components,
        BuildDepth::Settlements,
    )
    .unwrap();
    let struck: Vec<_> = world.ledger.find("struck-by").collect();
    let deaths: Vec<_> = world.ledger.find("outbreak-deaths").collect();
    assert!(
        !struck.is_empty(),
        "seed 42 must exercise the epidemic phase"
    );
    assert_eq!(struck.len(), deaths.len());
    for event in struck {
        assert!(deaths.iter().any(|death| {
            death.subject == event.subject && death.place == event.place && death.day == event.day
        }));
    }
    assert!(
        world
            .ledger
            .find(hornvale_history::OCC_CAUSE)
            .any(|fact| fact.object == hornvale_kernel::Value::Text("plague".into())),
        "seed 42 must exercise CauseOfEnd::Plague"
    );
}

/// Break caught: either half of an outbreak is omitted, attached to the wrong
/// occupation, or stamped on a different day from its mate.
#[test]
fn outbreak_events_emit_as_a_paired_dated_fact() {
    let mut world = test_world();
    hornvale_epidemiology::register_concepts(&mut world.registry).unwrap();
    let mut history = hand_history();
    history.outbreaks.push(OutbreakEvent {
        occupation: bid(2),
        pathogen: KindId("the-pest"),
        year: 75.0,
        deaths: 12.5,
    });
    history.outbreaks.push(OutbreakEvent {
        occupation: bid(2),
        pathogen: KindId("the-fever"),
        year: 75.0,
        deaths: 3.5,
    });

    emit_history(&mut world, &history).unwrap();
    let struck: Vec<_> = world.ledger.find("struck-by").collect();
    let deaths: Vec<_> = world.ledger.find("outbreak-deaths").collect();
    assert_eq!(struck.len(), 2);
    assert_eq!(deaths.len(), 2);
    let strike_ids: Vec<_> = struck.iter().map(|fact| fact.subject).collect();
    let death_ids: Vec<_> = deaths.iter().map(|fact| fact.subject).collect();
    assert_eq!(strike_ids, death_ids);
    assert_eq!(
        strike_ids
            .iter()
            .collect::<std::collections::BTreeSet<_>>()
            .len(),
        2
    );
    for strike in &struck {
        let death = deaths
            .iter()
            .find(|death| death.subject == strike.subject)
            .expect("each outbreak pair shares its event identity");
        assert_eq!(strike.place, death.place);
        assert_eq!(strike.day, death.day);
        assert_ne!(strike.subject, strike.place.unwrap());
    }
    assert_eq!(
        struck[0].object,
        hornvale_kernel::Value::Text("the-pest".into())
    );
    assert_eq!(deaths[0].object, hornvale_kernel::Value::Number(12.5));

    let bytes = serde_json::to_vec(&world.ledger).unwrap();
    let restored: hornvale_kernel::Ledger = serde_json::from_slice(&bytes).unwrap();
    let restored_struck: Vec<_> = restored.find("struck-by").collect();
    let restored_deaths: Vec<_> = restored.find("outbreak-deaths").collect();
    assert_eq!(restored_struck.len(), 2);
    let restored_strike_ids: Vec<_> = restored_struck.iter().map(|fact| fact.subject).collect();
    let restored_death_ids: Vec<_> = restored_deaths.iter().map(|fact| fact.subject).collect();
    assert_eq!(restored_strike_ids, restored_death_ids);
    assert_eq!(
        restored_strike_ids
            .iter()
            .collect::<std::collections::BTreeSet<_>>()
            .len(),
        2
    );
    assert!(restored_struck.iter().all(|strike| {
        restored_deaths
            .iter()
            .any(|death| death.subject == strike.subject && death.place == strike.place)
    }));
}

fn test_world() -> World {
    let mut w = World::new(Seed(42));
    hornvale_history::register_concepts(&mut w.registry).unwrap();
    hornvale_settlement::register_concepts(&mut w.registry).unwrap();
    w
}

/// Break caught: world composition forgets epidemiology's two dated-event
/// predicates, so an otherwise-correct bake cannot commit either half.
#[test]
fn world_registry_accepts_the_paired_outbreak_predicates() {
    let mut world = World::new(Seed(42));
    hornvale_worldgen::register_all(&mut world.registry).expect("domain roster registers");
    assert!(world.registry.predicate("struck-by").is_some());
    assert!(world.registry.predicate("outbreak-deaths").is_some());
}

/// A record with every "un-set" field filled with a neutral default, so each
/// test case only spells out what it cares about.
fn base_record(community: u64, people: &'static str, site: u32, founded: f64) -> BakeOccupation {
    BakeOccupation {
        core: Occupation {
            people: KindId(people),
            site: Vertex(site),
            founded,
            ended: None,
            peak_population: 50,
            tech: TechHorizon::Neolithic,
            function: Function::Agrarian,
            deity: None,
            tongue: None,
            cause: None,
            notability: Notability::Common,
            delve_depth_m: 0.0,
            person_years: 0.0,
        },
        community: bid(community),
        lineage: bid(community),
        founded_from: Founding::Genesis(Vertex(site)),
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
        // The Ell: `TributeRelation::since` is a bake YEAR and `Fact.day` is a
        // standard DAY, so the stamp is the crossing of the two.
        Some(
            WorldTime::from_std_days(hornvale_worldgen::ledger_day_of_bake_year(120.0))
                .expect("finite")
        ),
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
    assert_eq!(t.get(&KindId("goblin")).unwrap(), &[Vertex(0)].into());
    assert_eq!(t.get(&KindId("kobold")).unwrap(), &[Vertex(2)].into());
    // Dead occupations never contribute a vertex to any territory.
    let all_vertices: std::collections::BTreeSet<Vertex> = t.values().flatten().copied().collect();
    assert!(!all_vertices.contains(&Vertex(1)));
    assert!(!all_vertices.contains(&Vertex(3)));
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

    // End-of-life facts are stamped at `ended` (bake year 900), not `founded`
    // (bake year 100) — the day each of these actually became true.
    //
    // The Ell: the record's years cross into standard days at the emit
    // boundary, so the stamps are those years crossed. Written as the crossing
    // rather than as 328725.0/36525.0 so the two claims stay separable — this
    // test is about WHICH event dates a fact, and the unit is stated, not
    // baked into a literal.
    let ended_day = WorldTime::from_std_days(hornvale_worldgen::ledger_day_of_bake_year(900.0))
        .expect("a bake year crosses to a finite day");
    let founded_day = WorldTime::from_std_days(hornvale_worldgen::ledger_day_of_bake_year(100.0))
        .expect("a bake year crosses to a finite day");

    let is_ruin = w
        .ledger
        .facts_about(ruin_id)
        .find(|f| f.predicate == IS_RUIN)
        .expect("IS_RUIN must be committed for a dead occupation");
    assert_eq!(is_ruin.day, Some(ended_day));

    let occ_ended = w
        .ledger
        .facts_about(ruin_id)
        .find(|f| f.predicate == hornvale_history::OCC_ENDED)
        .expect("OCC_ENDED must be committed for a dead occupation");
    assert_eq!(occ_ended.day, Some(ended_day));

    let occ_cause = w
        .ledger
        .facts_about(ruin_id)
        .find(|f| f.predicate == hornvale_history::OCC_CAUSE)
        .expect("OCC_CAUSE must be committed for a dead occupation");
    assert_eq!(occ_cause.day, Some(ended_day));

    // Founding facts stay stamped at `founded`.
    let occ_founded = w
        .ledger
        .facts_about(ruin_id)
        .find(|f| f.predicate == hornvale_history::OCC_FOUNDED)
        .expect("OCC_FOUNDED must be committed");
    assert_eq!(occ_founded.day, Some(founded_day));

    let occ_site = w
        .ledger
        .facts_about(ruin_id)
        .find(|f| f.predicate == hornvale_history::OCC_SITE)
        .expect("OCC_SITE must be committed");
    assert_eq!(occ_site.day, Some(founded_day));
}

/// `present_year`'s **fallback** arm — the one a world with no committed
/// `history-now` takes (a pre-T8 save, or a synthetic Lab world that never ran
/// the composition-root bake) — reads back in bake years like the primary arm.
///
/// It has to be tested here rather than in `history_units.rs`, because it is
/// only reachable on a world that `emit_now` never touched, and every real
/// world commits `history-now`. `emit_history` alone is exactly that world.
///
/// The arm is a `max` over `occ-founded`/`occ-ended`, which is why it needs its
/// own guard at all: the max is taken on the ledger's day axis and crossed once
/// on the winner, so it shares no code with the primary read and no test of the
/// primary read can reach it. Drop the crossing and every consumer of a
/// bake-less world's present is 365× out, silently.
#[test]
fn the_present_fallback_reads_back_in_years_too() {
    let mut w = test_world();
    let mut ruin = base_record(1, "goblin", 0, 100.0);
    ruin.core.ended = Some(900.0);
    let h = History::new(vec![ruin], 1000.0);
    emit_history(&mut w, &h).unwrap();
    assert!(
        w.ledger
            .find(hornvale_history::HISTORY_NOW)
            .next()
            .is_none(),
        "this fixture must NOT commit history-now, or it tests the primary arm"
    );
    assert_eq!(
        hornvale_worldgen::present_year(&w),
        900.0,
        "with no committed present, the latest occupation event is the present \
         — as a bake YEAR, not as the day it is stored as"
    );
}

/// `present_frame` = `present_year` crossed forward into a standard day
/// (`WorldTime::from_std_days(ledger_day_of_bake_year(present_year(world)))`) — the
/// composition that used to be hand-written at
/// `windows/worldgen/tests/repose_exposure.rs`'s TASK 7 call, reachable only
/// from that file's `heavy:`-ignored batteries. `tools/seam-guard` reported
/// the `ledger_day_of_bake_year` seam UNGUARDED there: the mutated call never
/// ran under a non-ignored test at ANY `scope(...)`, because no non-ignored
/// test reached it at all, not because the scope was too narrow (widening
/// scope only helps a mutation some crate's non-ignored tests already
/// reach). Naming the crossing as its own function moves the call site here,
/// where it costs nothing to test — no bake, just the same hand-built
/// fixture `the_present_fallback_reads_back_in_years_too` already uses.
///
/// The property this pins: **`present_frame` must return the bake year
/// SCALED by `Years::DAYS_PER_YEAR`, not the bare year reinterpreted as a
/// day.** Dropping the crossing (`identity(0)` on `ledger_day_of_bake_year`
/// — exactly the mutation `tools/seam-guard` applies) makes `present_frame`
/// return `WorldTime::from_std_days(900.0)` instead of `WorldTime::from_std_days(900.0 *
/// 365.25)`, which both assertions below catch.
#[test]
fn present_frame_crosses_the_bake_year_by_days_per_year() {
    let mut w = test_world();
    let mut ruin = base_record(1, "goblin", 0, 100.0);
    ruin.core.ended = Some(900.0);
    let h = History::new(vec![ruin], 1000.0);
    emit_history(&mut w, &h).unwrap();

    let year = hornvale_worldgen::present_year(&w);
    assert_eq!(
        year, 900.0,
        "sanity: this fixture's present is bake year 900 via the fallback arm \
         (asserted directly by the_present_fallback_reads_back_in_years_too)"
    );

    let frame = hornvale_worldgen::present_frame(&w);
    let expected = WorldTime::from_std_days(hornvale_worldgen::ledger_day_of_bake_year(900.0))
        .expect("a bake year crosses to a finite day");
    assert_eq!(
        frame, expected,
        "present_frame must cross present_year's bake YEAR into a standard \
         DAY (year * Years::DAYS_PER_YEAR) — a year read back out where a day \
         belongs must fail this assertion"
    );
    // A float-tolerant, unit-legible restatement of the same claim: the day
    // is the year scaled by ~365, not equal to it — a `total_cmp`-adjacent
    // check that does not depend on `WorldTime`'s `PartialEq` alone to carry
    // the finding.
    assert!(
        frame.as_std_days() > year * 300.0,
        "the day ({}) must be the bake year ({year}) scaled by \
         Years::DAYS_PER_YEAR (365.25), not the bare year reinterpreted as a \
         day",
        frame.as_std_days()
    );
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
        .find(|r| r.core.site == Vertex(0))
        .expect("alive goblin at vertex 0");
    assert_eq!(alive_goblin.core.people, KindId("goblin"));
    assert_eq!(alive_goblin.core.founded, 0.0);
    assert_eq!(alive_goblin.core.ended, None);
    assert_eq!(alive_goblin.core.peak_population, 50);
    assert_eq!(alive_goblin.core.tech, TechHorizon::Neolithic);
    assert_eq!(alive_goblin.core.function, Function::Agrarian);
    assert_eq!(alive_goblin.core.notability, Notability::Common);
    assert_eq!(alive_goblin.core.cause, None);
    assert_eq!(alive_goblin.ended_by, Ended::Nature);
    assert_eq!(alive_goblin.founded_from, Founding::Genesis(Vertex(0)));

    let starved_goblin = recs
        .iter()
        .find(|r| r.core.site == Vertex(1))
        .expect("starved goblin at vertex 1");
    assert_eq!(starved_goblin.core.ended, Some(100.0));
    assert_eq!(starved_goblin.core.cause, Some(CauseOfEnd::Famine));
    assert_eq!(starved_goblin.core.notability, Notability::Backwater);

    let alive_kobold = recs
        .iter()
        .find(|r| r.core.site == Vertex(2))
        .expect("alive kobold at vertex 2");
    assert_eq!(alive_kobold.core.people, KindId("kobold"));
    assert_eq!(alive_kobold.core.founded, 50.0);

    let fled_goblin = recs
        .iter()
        .find(|r| r.core.site == Vertex(3))
        .expect("fled goblin at vertex 3");
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

/// One hand-built occupation of `site`, ready to take its place in a
/// multi-record `History`. Its position in that `History`'s `records` is what
/// fixes its entity id: since The Signet an occupation's id derives from
/// `(parent: None, role: "occupation", ordinal: index)`, and siblings of one
/// lineage share their high 48 bits and differ only in the low-16 ordinal — so
/// **records order and ascending-id order are the same order**, which is what
/// lets the test below still arrange a materially-backward mint sequence.
fn an_occupation(
    site: Vertex,
    founded: f64,
    ended: Option<f64>,
    peak_population: u32,
) -> BakeOccupation {
    let mut record = base_record(1, "goblin", site.0, founded);
    record.core.ended = ended;
    record.core.peak_population = peak_population;
    record
}

/// Commit `records` in one `emit_history` call and return the minted entity
/// for each, in the same order.
///
/// One call, not one per record: `emit_history` ordinals its occupations by
/// position in `records`, so calling it twice against the same world would
/// re-derive `ordinal: 0` and trip the mint-time collision assert — correctly,
/// since a world has exactly one baked history. That is a fixture constraint
/// this test now respects rather than a limitation to work around.
fn commit_occupations(w: &mut World, records: Vec<BakeOccupation>) -> Vec<EntityId> {
    let now = records
        .iter()
        .map(|r| r.core.ended.unwrap_or(r.core.founded) + 1.0)
        .fold(0.0_f64, f64::max);
    let h = History::new(records, now);
    emit_history(w, &h).unwrap();
    let mut ids: Vec<EntityId> = w
        .ledger
        .find(hornvale_history::IS_OCCUPATION)
        .map(|f| f.subject)
        .collect();
    // `find` yields commit order, which IS records order here (one entity per
    // record, minted and committed in sequence).
    ids.dedup();
    ids
}

#[test]
fn same_day_layers_order_by_material_facts_not_mint_order() {
    // Three occupations of one vertex, founded the same day. The one that
    // ended FIRST lies deepest. The one still alive (`ended: None`) is the
    // TOP layer, not the bottom — getting that backward inverts the
    // stratigraphy for every site with a survivor. Mint order is
    // deliberately arranged to disagree with BOTH placements, so a
    // mint-order comparator fails every assertion below.
    //
    // Records order (and why): `none_end` first (so it gets the SMALLEST
    // entity id, even though it must sort LAST materially), `late_end`
    // second, `early_end` last (so it gets the LARGEST id, even though it
    // must sort FIRST materially). Ascending-id order therefore reads
    // none_end, late_end, early_end — backward on every pair. An order that
    // let mint order agree with material order on any pair would let the old
    // comparator pass that pair by coincidence, and the guard below would
    // never fire. The `assert!` on the ids is what proves the arrangement
    // actually took.
    let mut w = world_with_registry();
    let ids = commit_occupations(
        &mut w,
        vec![
            an_occupation(Vertex(4), 100.0, None, 20),
            an_occupation(Vertex(4), 100.0, Some(900.0), 20),
            an_occupation(Vertex(4), 100.0, Some(150.0), 20),
        ],
    );
    let (none_end, late_end, early_end) = (ids[0], ids[1], ids[2]);
    assert!(
        none_end.get() < late_end.get() && late_end.get() < early_end.get(),
        "fixture must mint in exactly this (materially-backward) order, or the test proves nothing"
    );

    let layers = occupations_at(&w, Vertex(4));
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

/// claim: structural(seed: [42,7,1000]) — every observed key collision is a
/// genuine material tie, and the live panel exercises at least one collision.
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
    // A tie is never a bug. Whenever `layer_key` ties for two distinct
    // occupations, their own
    // (founded, ended, peak) and -- when a predecessor resolves -- its
    // founding coordinates are themselves equal, so the key is doing
    // exactly what its definition says, not silently colliding two
    // occupations the world actually distinguishes.
    let mut pairs = 0u64;
    let mut ties = 0u64;
    for seed in [42u64, 7, 1000] {
        let w = build_world(
            Seed(seed),
            &Default::default(),
            &Default::default(),
            &Default::default(),
        )
        .expect("builds");
        let coords = coords_by_id(&occupation_records(&w));
        for (vertex, occs) in occupations_by_vertex(&w) {
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
                            "seed {seed}, vertex {vertex:?}: tie without matching own material facts"
                        );
                        assert_eq!(
                            pa, pb,
                            "seed {seed}, vertex {vertex:?}: tie without matching predecessor coordinates"
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
    assert!(
        ties > 0,
        "found no tying pairs on the live corpus over {pairs} compared pairs; the \
         genuine-material-match assertions above did not run"
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
/// `EntityId` and onto its founding coordinates changes the rendered order of
/// at least one live multi-layer site (spec §4, V3).
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
/// gained the most newly-reachable ground (3,126 vertices, 16.4% of its land,
/// against 0.6% on seed 42), so more of its vertices carry stacked occupations for
/// the fourth key to reorder.
///
/// MERGE re-pin (2026-08-04, main absorbed into the-tolerance): both changes
/// redecide settlement survival, so the composed counts are neither branch's
/// (0/2/1) nor main's (1/6/0) — RE-MEASURED on the merged tree: **42 -> 0,
/// 7 -> 0, 1000 -> 1**. The CLAIM is unchanged and is still the one The Salt
/// froze: the material fourth key barely moves the stratigraphy — here a single
/// restacking site across three worlds of ~19k land vertices each, which is
/// "barely" a fortiori.
///
/// The Tense re-pin (2026-08-05): capacity gained an era axis, redeciding
/// settlement survival once more, and the counts move **0/0/1 -> 0/1/0**. The
/// TOTAL is unchanged at a single restacking site across three worlds of ~19k
/// land vertices each, so the claim this test exists for -- the material fourth
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
/// The note above named this exact reading as the degenerate one, and three
/// new settling peoples redeciding settlement survival is what produced it. On
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
///
/// **THE RANGE (task 4, 2026-08-09): the witness is BACK — 0/0/0 -> 1/0/1.**
/// The campaign's first biome-affinity row redecides settlement survival on
/// every seed, and two of the three worlds now carry a restacking site. The
/// note above recorded 0/0/0 as the degenerate reading, unable to tell the
/// material fourth key from a dead one; at 1/0/1 the measurement discriminates
/// again, on two independent worlds rather than one. The CLAIM this test was
/// frozen for is unchanged and still true: two restacking sites across three
/// worlds of ~19k land vertices each is "barely" by any reading.
///
/// **THE RADIATION (C2d, 2026-08-10): the witness is GONE AGAIN — 1/0/1 ->
/// 0/0/0.** Six new settling peoples redecide settlement survival on all three
/// seeds and both restacking sites disappear. This is the second time this
/// quantity has hit zero and the fourth time it has moved on a roster change
/// with no change to `layer_key` itself, so the honest summary is the one the
/// Delvers note already reached: **at 0/0/0 this measurement cannot tell the
/// material fourth key from a dead one**, and it is re-pinned at the measured
/// zeros rather than rescued by choosing a witnessing seed. The mechanism
/// stays separately and non-vacuously pinned by
/// `same_day_layers_order_by_material_facts_not_mint_order`, which is green.
///
/// Read against its sibling: `distinct_layers_tie_only_on_genuine_material_
/// matches` went the OTHER way in this same commit (0 -> 1 tying pairs). Ties
/// came back; restacking went away. Those remain different properties of the
/// key, and a campaign that needs either should widen the seed sweep — three
/// seeds have now produced five different readings between them.
///
/// **THE RADIATION, TASK 6 (C2d, 2026-08-10): the witness is BACK — 0/0/0 ->
/// 1/0/1**, byte-identical to the vector The Range measured, after this
/// campaign moved settlement placement twice more (the task-3 affinity relevel
/// and the founder-collision cut).
///
/// This re-pin was flagged during the campaign's post-census sweep as a
/// **candidate falsification** — "a null that went from *never moves* to
/// *moves on two of three seeds*" — and that reading is **rejected here, with
/// the reason stated rather than the number quietly bumped**, because it
/// mistakes the previous measured value for the claim.
///
/// - The claim this test is named and frozen for is "*the material fourth key
///   BARELY moves the stratigraphy*". Two restacking sites across three worlds
///   of ~19k land vertices each is "barely" by any reading, and the claim is if
///   anything better served at 1/0/1 than at 0/0/0.
/// - "Never moves" was never asserted. The paragraphs above record this
///   quantity reading 0/0/1, 0/1/0, 0/0/0, 1/0/1 and 0/0/0 across five prior
///   measurements — it has been 0, 1 and 2 in total, and this is the sixth
///   reading and the third distinct value. A quantity that has oscillated
///   through the same small range six times is not a null that stopped being
///   null.
/// - The DEGENERATE direction here is downward, not upward. This file already
///   records 0/0/0 as the reading that "cannot tell the material fourth key
///   from a dead one". Going 0 -> 2 restores the measurement's ability to
///   discriminate; it does not remove it.
///
/// So this is a re-pin of a WITNESS, and the claim it witnesses is unmoved.
/// What the six readings do say, and it is worth naming: a three-seed count
/// that lands on 0, 1 or 2 is an existence claim near a threshold in the sense
/// ratified decision 0097 describes, and any campaign that needs this quantity
/// as evidence should widen the sweep rather than read three worlds — which is
/// what the sibling paragraph above has been saying since The Range.
///
/// The per-seed counts are diagnostics, not fixtures: settlement placement
/// changes which live stacks exercise the key without changing the key's
/// contract. The structural witness is that the panel contains multi-layer
/// sites and at least one whose material and legacy orders differ.
///
/// claim: structural(seed: [42,7,1000]) — live material-key order witness.
#[test]
fn material_fourth_key_changes_at_least_one_live_stack() {
    let mut measured: Vec<(u64, usize)> = Vec::new();
    let mut multi_layer_sites = 0usize;
    for seed in [42u64, 7, 1000] {
        let w = build_world(
            Seed(seed),
            &Default::default(),
            &Default::default(),
            &Default::default(),
        )
        .expect("builds");
        let by_vertex = occupations_by_vertex(&w);
        multi_layer_sites += by_vertex.values().filter(|group| group.len() > 1).count();
        let changed = by_vertex
            .values()
            .filter(|group| group.len() > 1)
            .filter(|group| {
                // Today's order is what `occupations_by_vertex` returns (the
                // new, material fourth key). Compare against a re-sort keyed
                // on the PREDECESSOR'S ENTITY ID -- the key this task
                // removes.
                let mut old: Vec<_> = (*group).clone();
                old.sort_by_key(legacy_layer_key);
                old.iter().map(|r| r.id).collect::<Vec<_>>()
                    != group.iter().map(|r| r.id).collect::<Vec<_>>()
            })
            .count();
        measured.push((seed, changed));
    }
    // THE GLASSHOUSE re-pin, Stage B Task 2 (decision 0134): [1, 0, 1] ->
    // [0, 1, 1]. The craton rescale moved every world's coastline, redeciding
    // deep-history settlement survival and so which sites restack at all. The
    // TOTAL is unchanged at 2 and it simply moved seed: 42 lost its one order
    // change and 7 gained one. That is exactly the reading the doc comment
    // above already gives — a three-seed count landing on 0, 1 or 2 is an
    // existence claim near a threshold (decision 0097), and a campaign
    // needing this quantity as evidence should widen the sweep rather than
    // read three worlds. This is a re-pin of a WITNESS; the claim it
    // witnesses (the material fourth key barely moves the stratigraphy) is
    // unmoved and if anything better supported.
    //
    // THE GLASSHOUSE re-pin, Stage B Task 4: [0, 1, 1] -> [3, 0, 0]. The
    // thermostat re-placed every settlement a second time this campaign. The
    // TOTAL moves from 2 to 3 — still "barely" against ~19k land vertices per
    // world — and concentrates entirely on seed 42 this time (7 and 1000 both
    // lose their one restacking site). A three-seed count of 0, 1 or 3 stays
    // the same existence-claim-near-a-threshold reading the paragraphs above
    // already give; the claim this test is frozen for is unmoved.
    // Post-unblinding re-measure, declared per decision 0016.
    //
    // THE GLASSHOUSE re-pin, Stage B Task 5: [3, 0, 0] -> [1, 0, 0]. The
    // latitude profile re-placed settlements a third time this campaign. The
    // TOTAL falls from 3 to 1, so the claim this test is frozen for — that
    // the material fourth key BARELY moves the stratigraphy — is not merely
    // intact but better supported than at any reading since The Delvers: one
    // restacking site in three worlds, against ~19k land vertices each. A re-pin
    // that STRENGTHENS its own claim deserves the same scepticism as one that
    // weakens it, so note what has not changed: this is still a three-seed
    // existence claim near a threshold (decision 0097), the doc comment above
    // still says a campaign needing this quantity as evidence should widen the
    // sweep rather than read three worlds, and 1 is no more a property than 3
    // was. Measured on the tree that has absorbed main (50 commits, The
    // Repose), identical to the pre-absorption figure.
    //
    // THE GLASSHOUSE re-pin, Stage B, k re-decided: [1, 0, 0] -> [1, 2, 1],
    // total 1 -> 4. Setting the thermostat's residual fraction to 0.3 warmed
    // the population and re-placed settlements a FOURTH time this campaign.
    //
    // AND THE PARAGRAPH DIRECTLY ABOVE IS WHY THIS ONE IS WRITTEN DIFFERENTLY.
    // It called the previous reading "better supported than at any reading
    // since The Delvers" because the total had fallen to 1. Two hours later
    // the same quantity read 4. Nothing about the key changed in between —
    // only a constant this test does not measure. That is the file's own
    // standing reading (a coincidence of particular occupation chains, not a
    // property) demonstrating itself against the very comment that had just
    // restated it, and the lesson is narrower and more useful than "re-pin
    // carefully": DO NOT NARRATE A WITNESS'S VALUE. Record what it is, record
    // what moved it, and leave the claim's health to the claim's own evidence.
    // A total of 4 is no more "worse" for `barely moves` than 1 was "better".
    // Both are small against ~19k land vertices per world, and three seeds cannot
    // distinguish 1 from 4 in any case (decision 0097's existence-claim-near-
    // a-threshold reading, which is what the sweep-widening advice above is
    // for). Post-unblinding re-measure, declared per decision 0016.
    //
    // THE UNDERWORLD re-pin, Task 8 (spec §4.6's node-index re-key):
    // [1, 2, 1] -> [0, 0, 0], total 4 -> 0. Taking drow out of the competition
    // for surface vertices re-placed settlements a fifth time and left no site in
    // any of the three worlds where the material fourth key reorders the
    // stratigraphy at all. Recorded, not narrated, per the paragraph above:
    // zero is no more "better" for `barely moves` than 4 was "worse", and
    // three seeds still cannot distinguish 0 from 4 (decision 0097). What it
    // DOES cost is stated plainly — at a total of zero the per-site
    // comparisons this test performs find nothing to compare, so the witness
    // is vacuous at this reading, exactly as the tie-count witness above is at
    // the same commit. Both are recorded as costs rather than rescued by
    // hunting a seed that would keep them busy. Post-unblinding re-measure,
    // declared per decision 0016.
    //
    // THE UNDERWORLD re-pin, Task 9 (the genus join): [0, 0, 0] -> [0, 1, 1],
    // total 0 -> 2. Same campaign, same lever, second pull — `chamber_fit`
    // filtered the underworld corpus on `CaveKind::name()` against genera
    // spelled with a `-cave` suffix, so karst and fracture columns read the
    // genus-blind fallback; repairing it moves drow's seating and re-places
    // settlements a sixth time. Recorded, not narrated: 2 is no more "worse"
    // for `barely moves` than 0 was "better", and three seeds cannot
    // distinguish them (decision 0097). The one thing worth stating is the
    // vacuity note above going the other way — at a total of 2 the per-site
    // comparisons have something to compare again, so the witness is
    // load-bearing at this reading. That recovery is an accident of where the
    // settlements landed, not something this task went looking for.
    // THE GRANARY re-pin (2026-08-25): [0, 1, 1] -> [0, 2, 0], total 2 -> 2.
    // Same lever as every prior reading — re-placed settlements — and the
    // same verdict: three seeds cannot distinguish them (decision 0097).
    //
    // THE WINZE re-pin (Task 2, 2026-08-29): [0, 2, 0] -> [0, 1, 0], total
    // 2 -> 1. `Bake::grow` gained a second siting objective (spec §B.3: an
    // expansion onto ore-bearing ground may be a *working*), which re-places
    // settlements a seventh time — seed 7 carries 13 of the panel's 16 mines,
    // so it is the seed that moved, exactly as it is the seed that has carried
    // every nonzero reading here. Recorded, not narrated: 1 is no more
    // "better" for `barely moves` than 2 was "worse", and three seeds cannot
    // distinguish them (decision 0097). The witness stays load-bearing rather
    // than vacuous at this reading, which is again where it happened to land
    // and not something this task went looking for.
    //
    // THE WINZE T2b re-pin (spec amendment E, 2026-08-29): [0, 1, 0] ->
    // [0, 0, 1], total 1 -> 1. Eighth reading. The moved seed is 1000 this
    // time and seed 7 fell to zero, which is the first time the nonzero
    // reading has sat anywhere but seed 7 — worth recording precisely because
    // the prior entry offered "seed 7 carries most of the mines" as the
    // explanation, and one ring-scan later the same explanation would have
    // predicted the wrong seed. It was a coincidence then too. Three seeds
    // cannot distinguish these readings (decision 0097).
    //
    // THE WINZE T4 re-pin (spec §4.3, 2026-08-29): [0, 0, 1] -> [0, 0, 2],
    // total 1 -> 2. Ninth reading, and the same lever again: the breach hazard
    // ends some workings before the world otherwise would have, which
    // re-places settlements a ninth time. Seed 1000 keeps the whole of the
    // nonzero reading. Recorded, not narrated: 2 is no more "worse" for
    // `barely moves` than 1 was "better", both are tiny against ~19k land
    // vertices per world, and three seeds cannot distinguish them (decision
    // 0097). Seed 42 stays at zero for a reason that is NOT a coincidence
    // this time and is worth having on the record: none of seed 42's sixteen
    // workings breached, so this epoch does not move that world at all
    // (`breach.rs` reports 0 breached on seed 42 across the whole panel).
    assert!(
        multi_layer_sites > 0,
        "found no multi-layer sites across seeds 42/7/1000 — no ordering was exercised"
    );
    let changed_total: usize = measured.iter().map(|(_, changed)| changed).sum();
    assert!(
        changed_total > 0,
        "material and legacy fourth keys produced the same order for every live stack; \
         per-seed changes: {measured:?}"
    );
}

/// claim: structural(seed: 42) — one build, no sweep.
///
/// `person_years` samples the integral at EPOCH ENDS (campaign ledger #12),
/// not continuously, so the invariant is stated on epochs credited, never on
/// raw `tenure`: a raid cascade's intermediate hop can carry strictly
/// positive tenure (it opened at one sub-year raid phase and closed at a
/// later one, still within the same epoch) while surviving to no epoch's end
/// at all, and such an occupation is exactly as uncredited as a same-phase,
/// zero-tenure handoff — both are occupations The Lot's draw would place no
/// birth in, since the draw bins births at year midpoints and an occupation
/// with no credited epoch contains no whole year.
#[test]
fn person_years_matches_epochs_credited_at_their_end() {
    let world = hornvale_worldgen::seed_42_world();
    let now = hornvale_worldgen::present_year(&world);
    let occs = hornvale_worldgen::occupation_records(&world);
    assert!(!occs.is_empty());
    let cfg = hornvale_worldgen::BakeConfig::default_millennia();
    let e = cfg.epoch_years;
    let start = cfg.start_year;

    let mut total_zero_tenure = 0usize;
    let mut total_sub_epoch = 0usize;
    let mut credited_zero_py = 0usize;
    let mut uncredited_nonzero_py = 0usize;
    for o in &occs {
        let tenure = o.core.tenure(now);
        let end = o.core.ended.unwrap_or(now);
        // The checkpoints are the ends of loop-years Y_k = start + k*e, for
        // k = 0.. while Y_k < now; a community is credited at checkpoint k
        // iff it was opened before that epoch closed and had not closed by
        // then. An alive record's `end` is `now` itself, matching the bake's
        // own `while year < end_year` loop bound.
        let credited = (0..)
            .map(|k| start + k as f64 * e)
            .take_while(|y| *y < now)
            .filter(|y| o.core.founded < y + e && end >= y + e)
            .count();
        if tenure == 0.0 {
            total_zero_tenure += 1;
        } else if credited == 0 {
            total_sub_epoch += 1;
        }
        if credited > 0 && o.core.person_years == 0.0 {
            credited_zero_py += 1;
        }
        if credited == 0 && o.core.person_years != 0.0 {
            uncredited_nonzero_py += 1;
        }
        // `peak_population` is `population.round() as u32` (`Bake::touch`) —
        // a nearest-integer snapshot of a continuous quantity — so the raw
        // population any one credited epoch actually accrued can run up to
        // 0.5 above it. Exact for the mechanism otherwise: the once-per-epoch
        // accrual credits `population * e` for precisely `credited` epochs,
        // no more, no less.
        let bound = (f64::from(o.core.peak_population) + 0.5) * credited as f64 * e;
        assert!(
            o.core.person_years <= bound * 1.0001,
            "occupation {} person-years {} exceeds (peak + 0.5) x credited x epoch_years = {bound}",
            o.id.0,
            o.core.person_years
        );
    }
    let uncredited = total_zero_tenure + total_sub_epoch;
    eprintln!(
        "{} occupations, {uncredited} uncredited ({total_zero_tenure} zero-tenure, \
         {total_sub_epoch} sub-epoch)",
        occs.len()
    );
    assert_eq!(
        credited_zero_py, 0,
        "{credited_zero_py} occupations credited at some epoch's end carry zero person-years"
    );
    assert_eq!(
        uncredited_nonzero_py, 0,
        "{uncredited_nonzero_py} occupations credited at no epoch's end carry nonzero person-years"
    );
}

//! The Purview's thesis: map and prose are two grains of ONE lens, joined by
//! attention. If these fail, they are two pipelines wearing one name.

use hornvale_astronomy::SkyPins;
use hornvale_kernel::{Seed, World};
use hornvale_terrain::TerrainPins;
use hornvale_vessel::{PossessOpts, Session, Turn};
use hornvale_worldgen::{SettlementPins, SkyChoice, build_world};

fn world() -> World {
    build_world(
        Seed(42),
        &SkyPins::default(),
        SkyChoice::Generated,
        &TerrainPins::default(),
        &SettlementPins::default(),
    )
    .expect("seed 42 builds")
}

fn out(t: Turn) -> String {
    match t {
        Turn::Out(s) | Turn::Released(s) => s,
    }
}

#[test]
fn examine_accepts_exactly_the_union_of_both_grains() {
    let w = world();
    let (mut session, _) = Session::start(&w, &PossessOpts::default()).unwrap();
    for turn in 0..6 {
        let prose: Vec<String> = session
            .focalized()
            .unwrap()
            .nouns
            .iter()
            .map(|(n, _)| n.to_lowercase())
            .collect();
        let chart: Vec<String> = session
            .purview(0)
            .unwrap()
            .legend
            .iter()
            .map(|e| e.noun.to_lowercase())
            .collect();
        let mut union: Vec<String> = prose.iter().chain(chart.iter()).cloned().collect();
        union.sort();
        union.dedup();
        assert!(
            !union.is_empty(),
            "turn {turn}: a lens that surfaces nothing is no lens"
        );
        for noun in &union {
            let reply = out(session.handle(&format!("examine {noun}")));
            assert!(
                !reply.starts_with("You see no"),
                "turn {turn}: '{noun}' was surfaced by a grain of the lens but examine refused it: {reply}"
            );
            assert!(
                !reply.is_empty(),
                "turn {turn}: '{noun}' resolved to nothing"
            );
        }
        let refused = out(session.handle("examine a-noun-no-grain-surfaced"));
        assert!(
            refused.starts_with("You see no"),
            "turn {turn}: examine must still refuse what no grain surfaced: {refused}"
        );
        let way = session.ways().first().map(|(c, _)| format!("{c:?}"));
        if let Some(way) = way {
            session.handle(&format!("go {way}"));
        }
    }
}

#[test]
fn a_noun_at_both_grains_resolves_to_one_datum() {
    let w = world();
    let (mut session, _) = Session::start(&w, &PossessOpts::default()).unwrap();
    let prose = session.focalized().unwrap();
    let chart = session.purview(0).unwrap();
    let mut shared = 0;
    for (noun, _) in &prose.nouns {
        if chart
            .legend
            .iter()
            .any(|e| e.noun.eq_ignore_ascii_case(noun))
        {
            shared += 1;
            let a = out(session.handle(&format!("examine {noun}")));
            let b = out(session.handle(&format!("examine {}", noun.to_uppercase())));
            assert_eq!(
                a, b,
                "'{noun}' must resolve identically however it is asked"
            );
        }
    }
    // The biome is named by both the prose and the chart's legend, so this is
    // not a vacuous pass.
    assert!(shared > 0, "the two grains must actually overlap");
}

#[test]
fn drawing_the_map_never_moves_the_world() {
    let w = world();
    let (mut session, _) = Session::start(&w, &PossessOpts::default()).unwrap();
    let where_i_stand = session.agent().position.clone();
    let facts = session.committed_agent_at_count();
    for _ in 0..5 {
        session.handle("map");
        session.handle("map out 2");
    }
    assert_eq!(
        session.agent().position,
        where_i_stand,
        "map does not move the agent"
    );
    assert_eq!(
        session.committed_agent_at_count(),
        facts,
        "map commits nothing"
    );
}

#[test]
fn map_out_reaches_a_coarser_rung_and_stops_at_the_bottom() {
    let w = world();
    let (mut session, _) = Session::start(&w, &PossessOpts::default()).unwrap();
    let fine = out(session.handle("map"));
    let coarse = out(session.handle("map out 3"));
    assert!(fine.contains("[lens: terrain"), "{fine}");
    assert!(coarse.contains("[lens: terrain"), "{coarse}");
    assert_ne!(fine, coarse, "a coarser rung shows different ground");
    let absurd = out(session.handle("map out 99"));
    assert!(
        absurd.contains("no coarser") || absurd.contains("[lens: terrain"),
        "an over-large zoom must refuse or clamp, never panic: {absurd}"
    );
}

/// The real bound isn't the walk depth (12 on seed 42) — it's
/// `depth - globe_level`, which is 6 on seed 42. `map out 99` alone doesn't
/// pin this: any refusal at all would pass it, including one that fires far
/// too early or far too late. `map out 7` sits one rung past the real bound,
/// so it's the smallest input that actually exercises the boundary — and it
/// must refuse cleanly, never leak the underlying `VesselError`'s "room is
/// coarser than the canonical grid" wording, which says nothing to a player
/// about zooming.
#[test]
fn map_out_seven_is_just_past_the_real_bound_and_refuses_cleanly() {
    let w = world();
    let (mut session, _) = Session::start(&w, &PossessOpts::default()).unwrap();
    // Just inside the real bound (depth 12 - globe_level 6 = 6): must draw.
    let still_ok = out(session.handle("map out 6"));
    assert!(
        still_ok.contains("[lens: terrain"),
        "rung 6 is the real bound and must still draw: {still_ok}"
    );
    // One rung past the real bound: must refuse in player-facing language,
    // never leak the locale layer's internal "canonical grid" wording.
    let past_bound = out(session.handle("map out 7"));
    assert!(
        past_bound.contains("no coarser"),
        "rung 7 is past the real bound and must refuse: {past_bound}"
    );
    assert!(
        !past_bound.to_lowercase().contains("canonical grid"),
        "the refusal must not leak the locale layer's internal wording: {past_bound}"
    );
}

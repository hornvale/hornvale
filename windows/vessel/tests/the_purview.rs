//! The Purview's thesis: map and prose are two grains of ONE lens, joined by
//! attention. If these fail, they are two pipelines wearing one name.

use hornvale_astronomy::SkyPins;
use hornvale_kernel::{Seed, World, WorldTime};
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
    let mut rooms_visited: Vec<u64> = Vec::new();
    for turn in 0..6 {
        if let Ok(id) = session.agent().position.pack() {
            rooms_visited.push(id.0);
        }
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
    // `session.handle(&format!("go {way}"))`'s result is discarded above, and
    // `ways()` always returns three edges regardless of whether the agent
    // actually moved — so a `go` silently broken into a no-op would leave
    // this whole loop re-examining turn 0's room six times without anything
    // noticing. The walk is known (empirically) to visit 3 distinct rooms
    // over 6 turns, oscillating after the first move; assert only that it
    // moved at all, not a specific count.
    rooms_visited.sort_unstable();
    rooms_visited.dedup();
    assert!(
        rooms_visited.len() > 1,
        "the six-turn walk must visit more than one distinct room \
         (rooms visited: {rooms_visited:?})"
    );
}

#[test]
fn a_noun_at_both_grains_resolves_to_one_datum() {
    let w = world();
    let (mut session, _) = Session::start(&w, &PossessOpts::default()).unwrap();
    let prose = session.focalized().unwrap();
    let chart = session.purview(0).unwrap();
    let mut shared = 0;
    for (noun, prose_datum) in &prose.nouns {
        let Some(chart_entry) = chart
            .legend
            .iter()
            .find(|e| e.noun.eq_ignore_ascii_case(noun))
        else {
            continue;
        };
        shared += 1;
        // Case-insensitive lookup alone (`examine noun` vs `examine NOUN`)
        // would pass even if the join answered from the WRONG grain, as long
        // as it did so consistently. The actual claim under test is that a
        // noun named by both grains resolves to the PROSE catalog's own
        // datum, prose being primary — and since the two grains genuinely
        // carry different text for a shared noun (e.g. the settlement's
        // population line vs. its chart-mint line), this also pins that the
        // chart's datum is NOT what answers.
        let reply = out(session.handle(&format!("examine {noun}")));
        assert_eq!(
            &reply, prose_datum,
            "'{noun}' is named by both grains; examine must answer with the \
             prose catalog's own datum (prose is primary)"
        );
        assert_ne!(
            &reply, &chart_entry.datum,
            "'{noun}' must not resolve to the chart's datum when prose also \
             names it"
        );
        // The case-insensitive lookup itself is still worth pinning.
        let upper = out(session.handle(&format!("examine {}", noun.to_uppercase())));
        assert_eq!(
            reply, upper,
            "'{noun}' must resolve identically however it is asked"
        );
    }
    // The biome is named by both the prose and the chart's legend, so this is
    // not a vacuous pass.
    assert!(shared > 0, "the two grains must actually overlap");

    // `shared > 0` alone would still pass via the regime descriptor (both
    // grains draw it from the same `Locale::regime.descriptor`) even if the
    // biome were never a shared noun — which is exactly the bug The Margin
    // fixed: the chart's legend used to surface the biome's kebab-case slug
    // (`tropical-seasonal-forest`) while the prose surfaced its spaced name
    // (`tropical seasonal forest`), so the campaign's sharpest thesis clause
    // never fired on the most obvious thing on the map. Pin the biome
    // specifically, using the ground-truth `Locale` (day-independent for
    // biome in v1) rather than assuming anything about noun ordering.
    let here_locale = session
        .context()
        .describe(&session.agent().position, WorldTime { day: 0.0 })
        .expect("the observer's own room describes");
    let biome_noun = here_locale.biome;
    let biome_chart_entry = chart
        .legend
        .iter()
        .find(|e| e.noun.eq_ignore_ascii_case(&biome_noun))
        .unwrap_or_else(|| {
            panic!("the biome noun '{biome_noun}' must be a shared noun in the chart's legend")
        });
    let biome_prose_datum = prose
        .nouns
        .iter()
        .find(|(n, _)| n.eq_ignore_ascii_case(&biome_noun))
        .map(|(_, datum)| datum.clone())
        .unwrap_or_else(|| panic!("the biome noun '{biome_noun}' must be a prose noun"));
    let biome_reply = out(session.handle(&format!("examine {biome_noun}")));
    assert_eq!(
        biome_reply, biome_prose_datum,
        "the biome noun must resolve to the prose grain's datum (prose is primary)"
    );
    assert_ne!(
        biome_reply, biome_chart_entry.datum,
        "the biome noun must not resolve to the chart's datum"
    );
}

#[test]
fn drawing_the_map_never_moves_the_world() {
    let w = world();
    let (mut session, _) = Session::start(&w, &PossessOpts::default()).unwrap();
    let where_i_stand = session.agent().position.clone();
    let facts = session.committed_agent_at_count();
    let ledger_before = session.session_ledger_json();
    let knowledge_before = session.knowledge().0.clone();
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
    assert_eq!(
        session.session_ledger_json(),
        ledger_before,
        "map writes nothing to the ledger"
    );
    assert_eq!(
        &session.knowledge().0,
        &knowledge_before,
        "map teaches the session nothing new"
    );
}

/// The `ways on:` footer must name the exits of the cell the chart actually
/// draws, not the walk-depth room the agent stands in — those are different
/// cells once `zoom_out > 0`, and a footer that reports the wrong one is
/// exactly the "picture lies, caption doesn't" failure this campaign's
/// rendering doctrine forbids. Measured directly on seed 42: after one `go`,
/// the room the agent stands in exits NE/NW/S, but the ancestor cell
/// `map out 1` draws from there exits SE/N/SW — a genuine divergence, not a
/// coincidence of ordering, so this test would have failed under the old
/// code (which passed `self.ways()`, the walk-depth exits, unconditionally).
#[test]
fn map_out_names_the_drawn_cells_own_exits_not_the_walk_depths() {
    let w = world();
    let (mut session, _) = Session::start(&w, &PossessOpts::default()).unwrap();
    let way = session
        .ways()
        .first()
        .map(|(c, _)| format!("{c:?}"))
        .expect("the starting room has exits");
    session.handle(&format!("go {way}"));
    let fine_ways: Vec<String> = session
        .ways()
        .iter()
        .map(|(c, _)| format!("{c:?}").to_uppercase())
        .collect();
    assert_eq!(
        fine_ways,
        vec!["NE", "NW", "S"],
        "pin: the fine room's own exits at this point of the seed-42 walk \
         (if world-gen ever changes this, re-measure and update the pin)"
    );
    let coarse = out(session.handle("map out 1"));
    assert!(
        coarse.contains("ways on: SE, N, SW"),
        "the footer must report the DRAWN cell's own exits: {coarse}"
    );
    assert!(
        !coarse.contains("ways on: NE, NW, S"),
        "the footer must not leak the walk-depth room's exits onto a coarser chart: {coarse}"
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

/// `map out 4294967296` (one past `u32::MAX`) IS a number — the request must
/// say so, distinctly from a genuinely non-numeric argument, rather than the
/// misleading "is not a number" a bare `parse::<u32>()` error swallows both
/// cases into.
#[test]
fn map_out_past_u32_names_the_real_problem_not_a_parse_failure() {
    let w = world();
    let (mut session, _) = Session::start(&w, &PossessOpts::default()).unwrap();
    let too_large = out(session.handle("map out 4294967296"));
    assert!(
        !too_large.contains("is not a number"),
        "4294967296 is a number; the refusal must not claim otherwise: {too_large}"
    );
    let non_numeric = out(session.handle("map out banana"));
    assert!(
        non_numeric.contains("is not a number"),
        "a genuinely non-numeric argument must still say so: {non_numeric}"
    );
}

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
        if let Ok(id) = session.position().pack() {
            rooms_visited.push(id.0);
        }
        let prose: Vec<String> = session
            .focalized()
            .unwrap()
            .nouns
            .iter()
            .map(|n| n.display.to_lowercase())
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
    // `ways()` always returns the same edge set regardless of whether the agent
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
    for n in &prose.nouns {
        let noun = &n.display;
        let prose_datum = &n.datum;
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
        .describe(&session.position(), WorldTime::GENESIS)
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
        .find(|n| n.display.eq_ignore_ascii_case(&biome_noun))
        .map(|n| n.datum.clone())
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
    let where_i_stand = session.position();
    let facts = session.committed_agent_at_count();
    let ledger_before = session.session_ledger_json();
    let knowledge_before = session.knowledge().0.clone();
    for _ in 0..5 {
        session.handle("map");
        session.handle("map out 2");
    }
    assert_eq!(
        session.position(),
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

/// The `ways on:` footer must name the exits of the room the chart actually
/// draws, not the walk-depth room the agent stands in — those are different
/// rooms once `zoom_out > 0`, and a footer that reports the wrong one is
/// exactly the "picture lies, caption doesn't" failure this campaign's
/// rendering doctrine forbids.
///
/// # THE OLD FORM OF THIS TEST CANNOT BE RE-PINNED, AND THAT IS THE FINDING
///
/// It used to pin two literal triads — the fine room's `{NE, W, SE}` and the
/// coarse room's `{E, NW, SW}` — and assert they were DISJOINT, which is what
/// made it strong: a footer leaking the walk-depth room's exits onto a coarser
/// chart would fail on every point. Four campaigns re-measured those triads and
/// each time the disjointness survived, because a triangular room had only
/// three of eight compass words and two rooms rarely drew the same three.
///
/// **On the cube-sphere mesh every interior room exits in all eight
/// directions** (`Facet::neighbors` returns 8, and only the 24 cube-corner
/// rooms return 7). So the fine room's exits and the coarse room's exits are
/// now the SAME SET at almost every point of any walk — not because the rungs
/// merged, but because the alphabet stopped distinguishing them. Re-pinning
/// the two literals to `N..NW` twice over would have produced a green test
/// that could no longer fail for the reason it exists.
///
/// So the claim is asserted against the mesh instead of against a second
/// reading of the session: the footer must name **the drawn room's own
/// `Facet::neighbors` rose**, computed here from `position().parent()`, and it
/// must do so at a rung where the drawn room is a genuinely different room. A
/// footer that reported the walk-depth room's exits would still be reporting a
/// DIFFERENT ROOM's exits — the sets happen to coincide today, and this test
/// says so out loud rather than resting on it.
#[test]
fn map_out_names_the_drawn_rooms_own_exits_not_the_walk_depths() {
    let w = world();
    let (mut session, _) = Session::start(&w, &PossessOpts::default()).unwrap();
    let way = session
        .ways()
        .first()
        .map(|(c, _)| format!("{c:?}"))
        .expect("the starting room has exits");
    session.handle(&format!("go {way}"));

    let fine_room = session.position();
    let coarse_room = fine_room.parent().expect("a walk-band room has a parent");
    assert_ne!(
        fine_room, coarse_room,
        "`map out 1` must draw a genuinely different room, or this test compares \
         a room with itself"
    );

    // The rose the mesh gives each room, independently of anything the session
    // says: one compass word per neighbour, in `Compass::all` order. This is
    // `hornvale_locale::heading_rose`'s assignment, which since The Pavement is
    // also the rule `Locale::exits` and `go` both read — so "the drawn room's
    // own exits" has exactly one meaning now.
    let rose_of = |room: &hornvale_kernel::Facet| -> Vec<String> {
        hornvale_locale::Compass::all()
            .into_iter()
            .zip(hornvale_locale::heading_rose(room))
            .filter(|(_, n)| n.is_some())
            .map(|(c, _)| format!("{c:?}").to_uppercase())
            .collect()
    };

    let fine_ways: Vec<String> = session
        .ways()
        .iter()
        .map(|(c, _)| format!("{c:?}").to_uppercase())
        .collect();
    assert_eq!(
        fine_ways,
        rose_of(&fine_room),
        "the walk-band footer names the walk-band room's own rose"
    );

    let coarse = out(session.handle("map out 1"));
    let want = format!("ways on: {}", rose_of(&coarse_room).join(", "));
    assert!(
        coarse.contains(&want),
        "the footer must report the DRAWN room's own exits ({want}): {coarse}"
    );
}

#[test]
fn map_out_reaches_a_coarser_rung_and_stops_at_the_bottom() {
    let w = world();
    let (mut session, _) = Session::start(&w, &PossessOpts::default()).unwrap();
    let fine = out(session.handle("map"));
    let coarse = out(session.handle("map out 3"));
    // Default eyes are `Own` (The Beholding, Task 5): the walk-band chart
    // draws the colour lens, not the plain terrain one.
    assert!(fine.contains("[lens: colour"), "{fine}");
    assert!(coarse.contains("[lens: colour"), "{coarse}");
    assert_ne!(fine, coarse, "a coarser rung shows different ground");
    let absurd = out(session.handle("map out 99"));
    assert!(
        absurd.contains("no coarser") || absurd.contains("[lens: colour"),
        "an over-large zoom must refuse or clamp, never panic: {absurd}"
    );
}

/// The real bound isn't the walk depth — it's `walk_depth - globe_level`,
/// which The Pavement moved from 6 to **7** when the walk band became
/// `globe_level + 7` (spec section 2.3: depth 12 on the cube-sphere is a
/// 2.251 km step, twice the icosphere's, so the band went one level finer).
/// `map out 99` alone doesn't pin this: any refusal at all would pass it,
/// including one that fires far too early or far too late. The rung one past
/// the real bound is the smallest input that actually exercises the boundary —
/// and it must refuse cleanly, never leak the underlying `VesselError`'s "room
/// is coarser than the canonical grid" wording, which says nothing to a player
/// about zooming.
///
/// **Derived, not written down.** The old form spelled the two rungs as the
/// literals `6` and `7`, which is what made this test a casualty of a band
/// change it has no opinion about. It asks the two functions now, and the test
/// name keeps the word "seven" only because renaming a test is a separate act
/// from re-deriving its constant.
#[test]
fn map_out_seven_is_just_past_the_real_bound_and_refuses_cleanly() {
    let w = world();
    let ctx = hornvale_locale::LocaleContext::build(&w).expect("seed 42 builds a context");
    let bound = hornvale_locale::walk_depth(&ctx) - ctx.globe_level();
    let (mut session, _) = Session::start(&w, &PossessOpts::default()).unwrap();
    // Just inside the real bound: must draw.
    let still_ok = out(session.handle(&format!("map out {bound}")));
    assert!(
        // Default eyes are `Own` (The Beholding, Task 5): colour, not terrain.
        still_ok.contains("[lens: colour"),
        "rung {bound} is the real bound and must still draw: {still_ok}"
    );
    // One rung past the real bound: must refuse in player-facing language,
    // never leak the locale layer's internal "canonical grid" wording.
    let past_bound = out(session.handle(&format!("map out {}", bound + 1)));
    assert!(
        past_bound.contains("no coarser"),
        "rung {} is past the real bound and must refuse: {past_bound}",
        bound + 1
    );
    assert!(
        !past_bound.to_lowercase().contains("canonical grid"),
        "the refusal must not leak the locale layer's internal wording: {past_bound}"
    );
}

/// `map out 4294967296` (one past `u32::MAX`) IS a number — the request must
/// say so, distinctly from a genuinely non-numeric argument, rather than the
/// misleading "is not a number" a bare `parse::<u32>()` error swallows both
/// cases into. It must also never state a false bound: an earlier version of
/// this refusal quoted `u32::MAX` (4294967295) as "the chart tops out at"
/// that many rungs, which was not true — the real ceiling is `depth -
/// globe_level` (SEVEN on seed 42 since The Pavement moved the walk band to
/// `globe_level + 7`), enforced separately below. The honest fix
/// saturates the overflowed value and lets that real bound check answer, so
/// this reply must be byte-identical to what the first rung past the real
/// bound already produces — a rung this test now derives rather than writes
/// down, since the literal `7` it used to name is INSIDE the bound today.
#[test]
fn map_out_past_u32_names_the_real_problem_not_a_parse_failure() {
    let w = world();
    let ctx = hornvale_locale::LocaleContext::build(&w).expect("seed 42 builds a context");
    let bound = hornvale_locale::walk_depth(&ctx) - ctx.globe_level();
    let (mut session, _) = Session::start(&w, &PossessOpts::default()).unwrap();
    let too_large = out(session.handle("map out 4294967296"));
    assert!(
        !too_large.contains("is not a number"),
        "4294967296 is a number; the refusal must not claim otherwise: {too_large}"
    );
    assert!(
        !too_large.contains("4294967295") && !too_large.to_lowercase().contains("u32"),
        "the refusal must not state ANY numeric bound — the real bound is \
         {bound}, not u32::MAX: {too_large}"
    );
    let past_real_bound = out(session.handle(&format!("map out {}", bound + 1)));
    assert_eq!(
        too_large, past_real_bound,
        "an overflowed zoom must be refused by the SAME real bound check as an \
         ordinary over-large one, not a bespoke overflow message: {too_large:?} vs {past_real_bound:?}"
    );
    let non_numeric = out(session.handle("map out banana"));
    assert!(
        non_numeric.contains("is not a number"),
        "a genuinely non-numeric argument must still say so: {non_numeric}"
    );
}

/// `map out -1` IS a number too (a negative one) — `u32::from_str` reports a
/// leading `-` as `InvalidDigit`, the same error kind a genuinely non-numeric
/// argument produces, so a naive fix would fold "-1" into the same "is not a
/// number" message. That is false: -1 parses fine as an integer, it merely
/// has no meaning as a rung count. This must be its own honest message,
/// distinct from both the non-numeric case and the real-bound refusal.
#[test]
fn map_out_negative_names_the_real_problem_not_a_parse_failure() {
    let w = world();
    let (mut session, _) = Session::start(&w, &PossessOpts::default()).unwrap();
    let negative = out(session.handle("map out -1"));
    assert!(
        !negative.contains("is not a number"),
        "-1 is a number; the refusal must not claim otherwise: {negative}"
    );
    assert!(
        negative.contains("negative"),
        "the refusal must name the actual problem — a negative zoom: {negative}"
    );
    assert!(
        !negative.contains("no coarser rung") && !negative.contains("coarsest"),
        "a negative zoom must not be confused with the real (positive) bound refusal: {negative}"
    );
}

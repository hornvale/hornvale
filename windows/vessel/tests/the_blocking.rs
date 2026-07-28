//! The Blocking's observable end: a floor plan a player can read, every noun on
//! it examinable, every destination on it reachable by a command.
//!
//! This is `the_purview.rs`'s parity test one band down. That file proved map and
//! prose are two grains of one lens at the locale; this proves the same of the
//! floor plan and the chamber, which is the pane The Panes will later draw with
//! pixels. If it fails, they are two pipelines wearing one name.

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

/// Walk the seed-42 possession into the structure the gallery enters.
///
/// `enter` bare is the same step the committed transcript takes
/// (`book/src/gallery/possession-seed-42.md`), so if this helper stops finding a
/// chamber the gallery has stopped showing one too — which is the defect The
/// Lintel shipped and followup 15 named. Panics loudly rather than skipping:
/// a parity test that silently tests nothing is worse than a red one.
fn inside(session: &mut Session) {
    let reply = out(session.handle("enter"));
    assert!(
        reply.starts_with("[chamber "),
        "the possession did not get indoors, so nothing below is tested: {reply}"
    );
}

/// The rows of a drawn plan: the lines made only of the plan's own alphabet.
/// The header and the legend line carry letters, so they fall out here.
///
/// Counting glyphs over the whole reply is a trap, and it fired the first time
/// this file ran: the legend line spells `+ a doorway`, so `reply.matches('+')`
/// reported one more doorway than the picture draws. The picture is what the
/// parity test is about, so the picture is what gets counted.
fn picture_rows(plan: &str) -> Vec<&str> {
    plan.lines()
        .filter(|l| l.chars().all(|c| "#.+ ".contains(c)) && l.len() > 2)
        .collect()
}

/// How many times `glyph` is DRAWN, legend line excluded.
fn drawn(plan: &str, glyph: char) -> usize {
    picture_rows(plan)
        .iter()
        .map(|r| r.matches(glyph).count())
        .sum()
}

#[test]
fn map_indoors_draws_a_floor_plan() {
    let w = world();
    let (mut session, _) = Session::start(&w, &PossessOpts::default()).unwrap();
    inside(&mut session);
    let plan = out(session.handle("map"));
    // The picture must be a picture: several lines, made of the plan's own
    // alphabet, and wide enough to hold a room.
    let lines = picture_rows(&plan);
    assert!(
        lines.len() >= 4,
        "a floor plan needs more than a few rows to be a plan: {plan}"
    );
    assert!(
        drawn(&plan, '#') > 0 && drawn(&plan, '.') > 0,
        "a plan with no wall or no floor is not a plan: {plan}"
    );
    assert!(
        drawn(&plan, '+') > 0,
        "the seed-42 structure has two chambers, so its plan must show a doorway: {plan}"
    );
    // Rectangular, and that is a claim about the render rather than a tidiness
    // preference: a row short of its neighbours means a cell went undrawn, and an
    // undrawn cell is a hole in a picture the parity test then reads as closed.
    let widths: std::collections::BTreeSet<usize> =
        lines.iter().map(|l| l.chars().count()).collect();
    assert_eq!(
        widths.len(),
        1,
        "every row of a plan must be the same width; got {widths:?} in {plan}"
    );
}

#[test]
fn map_outdoors_still_draws_the_chart() {
    // The band-awareness must not have eaten the locale chart.
    let w = world();
    let (mut session, _) = Session::start(&w, &PossessOpts::default()).unwrap();
    let chart = out(session.handle("map"));
    assert!(
        !chart.is_empty() && !chart.starts_with("[chamber "),
        "outdoors, `map` must still draw the locale chart: {chart}"
    );
    assert!(
        chart.contains("[lens: terrain"),
        "outdoors, `map` must still draw the locale chart: {chart}"
    );
}

#[test]
fn map_out_indoors_refuses_and_names_the_verb_that_fixes_it() {
    // A plan is one building, so there is no coarser rung of it to draw. The
    // refusal must not be silent (an ignored argument is how a player comes to
    // believe they asked for something and got it) and must not fall through to
    // the locale chart either, which would draw the LAND from inside a house.
    let w = world();
    let (mut session, _) = Session::start(&w, &PossessOpts::default()).unwrap();
    inside(&mut session);
    let plan = out(session.handle("map"));
    for line in ["map out", "map out 2"] {
        let refused = out(session.handle(line));
        assert_ne!(
            refused, plan,
            "{line:?} indoors must not silently draw the bare plan"
        );
        assert!(
            !refused.contains("[lens: terrain"),
            "{line:?} indoors must not draw the locale chart: {refused}"
        );
        assert!(
            refused.contains("out"),
            "the refusal must name the verb that fixes it: {refused}"
        );
    }
}

#[test]
fn every_noun_the_plan_depicts_is_examinable() {
    // The parity contract's tested half (spec §6), generalizing
    // `the_purview.rs::examine_accepts_exactly_the_union_of_both_grains`.
    let w = world();
    let (mut session, _) = Session::start(&w, &PossessOpts::default()).unwrap();
    inside(&mut session);
    let nouns = session.plan_legend_nouns();
    assert!(
        !nouns.is_empty(),
        "a plan whose legend names nothing cannot be checked, and a player cannot \
         read it either"
    );
    // The legend must actually be the picture's own alphabet: a legend naming a
    // glyph the render never draws would pass every assertion below while
    // depicting something else entirely.
    let plan = out(session.handle("map"));
    for (glyph, _) in session.plan_legend() {
        assert!(
            plan.contains(glyph),
            "the legend names {glyph:?} but the picture never draws it: {plan}"
        );
    }
    for noun in &nouns {
        let reply = out(session.handle(&format!("examine {noun}")));
        assert!(
            !reply.starts_with("You see no"),
            "'{noun}' is drawn on the plan but examine refused it: {reply}"
        );
        assert!(!reply.is_empty(), "'{noun}' resolved to nothing");
    }
    let refused = out(session.handle("examine a-noun-no-grain-surfaced"));
    assert!(
        refused.starts_with("You see no"),
        "examine must still refuse what nothing depicts, or it accepts everything \
         and the test above proves nothing: {refused}"
    );
}

#[test]
fn every_noun_the_chamber_speaks_of_is_examinable_too() {
    // The other half of the same contract, and the reversal this task owes The
    // Lintel: `look` named a water jar and `examine` denied it. The plan's legend
    // does not name the jar, so walking the legend alone would leave exactly the
    // defect that shipped.
    let w = world();
    let (mut session, _) = Session::start(&w, &PossessOpts::default()).unwrap();
    let shown = out(session.handle("enter"));
    assert!(
        shown.starts_with("[chamber "),
        "the possession did not get indoors: {shown}"
    );
    let nouns = session.chamber_nouns_here();
    assert!(!nouns.is_empty(), "a built chamber's prose names something");
    for noun in &nouns {
        assert!(
            shown.contains(noun.as_str()),
            "the precondition is that `look` NAMED {noun:?}: {shown}"
        );
        let reply = out(session.handle(&format!("examine {noun}")));
        assert!(
            !reply.starts_with("You see no"),
            "`look` named {noun:?} and `examine` denied it, two turns apart: {reply}"
        );
    }
}

#[test]
fn every_destination_the_plan_depicts_is_command_reachable() {
    // A doorway drawn is a promise. The plan draws '+' once per doorway of the
    // WHOLE structure, so the honest comparison is against every aperture the
    // structure names to a player walking it — not against the footer of the one
    // chamber the walk happens to start in, which names at most two ways however
    // many doorways the building has.
    let w = world();
    let (mut session, _) = Session::start(&w, &PossessOpts::default()).unwrap();
    inside(&mut session);
    let plan = out(session.handle("map"));
    let doorways = drawn(&plan, '+');
    // Walk the structure to its far end, counting the apertures the ways-on
    // footer actually advertises. One per drawn doorway, and each one walked
    // through by a command the player could type.
    let mut walked = 0;
    loop {
        let here = out(session.handle("look"));
        let names_deeper = here
            .lines()
            .find(|l| l.starts_with("Ways on:"))
            .map(|l| l.contains("further in"))
            .expect("a chamber rendering always names its ways");
        if !names_deeper {
            break;
        }
        let stepped = out(session.handle("enter further in"));
        assert!(
            stepped.starts_with("[chamber "),
            "the footer advertised a way further in and the command did not take it: {stepped}"
        );
        walked += 1;
        assert!(walked <= 8, "the walk did not terminate");
    }
    assert_eq!(
        doorways, walked,
        "the plan draws {doorways} doorways and the commands walk {walked}: a drawn \
         destination no command reaches is the defect this test exists for"
    );
    assert!(
        doorways > 0,
        "the seed-42 structure is multi-chambered, so a plan with no doorway \
         would make the comparison above vacuous"
    );
}

#[test]
fn drawing_the_plan_never_moves_the_world() {
    // `the_purview.rs::drawing_the_map_never_moves_the_world`, one band down. A
    // FRAME-tier derivation (decision 0069) that committed anything would stop
    // being derived, and re-walking the room would stop being byte-identical.
    let w = world();
    let (mut session, _) = Session::start(&w, &PossessOpts::default()).unwrap();
    inside(&mut session);
    let where_i_stand = session.agent().position.clone();
    let facts = session.committed_agent_at_count();
    let ledger_before = session.session_ledger_json();
    let knowledge_before = session.knowledge().0.clone();
    let first = out(session.handle("map"));
    for _ in 0..4 {
        assert_eq!(
            out(session.handle("map")),
            first,
            "the plan is FRAME-tier: re-deriving it must be byte-identical"
        );
        session.handle("examine a wall");
    }
    assert_eq!(session.agent().position, where_i_stand);
    assert_eq!(
        session.committed_agent_at_count(),
        facts,
        "drawing a plan commits nothing"
    );
    assert_eq!(
        session.session_ledger_json(),
        ledger_before,
        "drawing a plan writes nothing to the ledger"
    );
    assert_eq!(
        &session.knowledge().0,
        &knowledge_before,
        "drawing a plan teaches the session nothing new"
    );
}

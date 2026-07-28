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
///
/// **The alphabet is a maintenance hazard, and Task 5 tripped it.** A row is
/// recognized as a picture row by being made only of the plan's own glyphs, so
/// adding a fourth glyph (`@`, the standing mark) silently dropped the row the
/// mark stands in — every row count and every glyph count went quietly wrong
/// rather than failing. Any glyph the render gains has to be added here in the
/// same commit.
fn picture_rows(plan: &str) -> Vec<&str> {
    plan.lines()
        .filter(|l| l.chars().all(|c| "#.+@ ".contains(c)) && l.len() > 2)
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
fn the_plan_is_one_glyph_per_cell_and_walled_all_round() {
    // Task 4b's reification, checked where a PLAYER meets it. Two claims, and
    // neither could be made before a wall was a cell:
    //
    // - the picture is 1:1 with the lattice, so a 19x10 extent draws 19x10 rather
    //   than Task 4's doubled 39x21. Pinned as numbers here on purpose: the
    //   `(2w+1)` machinery coming back would still satisfy every proportional
    //   assertion in this file, and would only fail against a stated size.
    // - the plan is ENCLOSED. A drawn border of unbroken `#` is what makes the
    //   picture read as a BUILDING rather than as a floating partition diagram,
    //   and it is what roughly a fifth to two fifths of the extent is spent on.
    let w = world();
    let (mut session, _) = Session::start(&w, &PossessOpts::default()).unwrap();
    inside(&mut session);
    let plan = out(session.handle("map"));
    let lines = picture_rows(&plan);
    assert_eq!(
        (lines.len(), lines[0].chars().count()),
        (10, 19),
        "the seed-42 structure has two chambers, whose extent is 19x10, and the \
         render is 1:1: {plan}"
    );
    let last = lines.len() - 1;
    for (y, row) in lines.iter().enumerate() {
        let glyphs: Vec<char> = row.chars().collect();
        let wide = glyphs.len() - 1;
        for (x, g) in glyphs.iter().enumerate() {
            if y == 0 || y == last || x == 0 || x == wide {
                assert_eq!(
                    *g, '#',
                    "the plan's exterior wall is broken at ({x},{y}): {plan}"
                );
            }
        }
    }
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

#[test]
fn go_indoors_moves_one_cell_and_says_where_you_are() {
    let w = world();
    let (mut session, _) = Session::start(&w, &PossessOpts::default()).unwrap();
    inside(&mut session);
    // At least one of the four bearings must be walkable from wherever `enter`
    // lands, or the chamber has no interior to stand in — which
    // `no_chamber_is_degenerate` already forbids at the lattice level.
    let replies: Vec<String> = ["n", "s", "e", "w"]
        .iter()
        .map(|d| out(session.handle(&format!("go {d}"))))
        .collect();
    assert!(
        replies.iter().any(|r| !r.contains("wall")),
        "every bearing from the entry cell is walled: {replies:?}"
    );
    // The plan's own assertion above discriminates only while the word `wall`
    // appears in REFUSALS and nowhere else, which is a live constraint on the
    // success sentence rather than an accident — so the positive form is
    // asserted too. A step either says so or lands in the next chamber.
    assert!(
        replies
            .iter()
            .any(|r| r.starts_with("You step") || r.starts_with("[chamber ")),
        "no bearing produced a step at all: {replies:?}"
    );
    assert!(
        replies.iter().all(|r| !r.starts_with("[room ")),
        "a compass step indoors must not put the possession out of doors: {replies:?}"
    );
}

#[test]
fn a_wall_refuses_with_a_physical_reason() {
    // The Lintel's own standard, from its `enter` work: refuse with a reason
    // drawn from the world, never with a grammar complaint.
    let w = world();
    let (mut session, _) = Session::start(&w, &PossessOpts::default()).unwrap();
    inside(&mut session);
    // Walk one bearing until it refuses — a bounded plan always ends in a wall.
    let mut refusal = String::new();
    for _ in 0..64 {
        let reply = out(session.handle("go n"));
        if reply.contains("wall") {
            refusal = reply;
            break;
        }
    }
    assert!(
        !refusal.is_empty(),
        "walking one bearing 64 times never met a wall: the plan is unbounded"
    );
    assert!(
        !refusal.contains("no north"),
        "the refusal still claims there is no north indoors: {refusal}"
    );
}

#[test]
fn back_stays_refused_indoors() {
    let w = world();
    let (mut session, _) = Session::start(&w, &PossessOpts::default()).unwrap();
    inside(&mut session);
    let reply = out(session.handle("back"));
    assert!(
        reply.contains("Inside"),
        "`back` retraces a WALK-band trail and must still refuse indoors: {reply}"
    );
}

#[test]
fn walking_a_chamber_commits_nothing() {
    // Decision 0069: intra-chamber position is FRAME-tier. The ledger must not
    // grow because someone crossed a room.
    let w = world();
    let (mut session, _) = Session::start(&w, &PossessOpts::default()).unwrap();
    inside(&mut session);
    let before = session.committed_fact_count();
    for _ in 0..8 {
        session.handle("go e");
    }
    assert_eq!(
        session.committed_fact_count(),
        before,
        "walking inside a chamber committed a fact: fine position is never \
         serialized (0069)"
    );
}

#[test]
fn the_plan_marks_where_you_stand_and_the_mark_moves_with_you() {
    // The mark is what makes the plan a plan of where you ARE rather than of the
    // building in the abstract, and it is a CELL position: exactly one glyph, on
    // a cell a mover can stand in, and it moves by one when you step.
    let w = world();
    let (mut session, _) = Session::start(&w, &PossessOpts::default()).unwrap();
    inside(&mut session);
    let before = out(session.handle("map"));
    assert_eq!(
        drawn(&before, '@'),
        1,
        "the plan must mark exactly one standing cell: {before}"
    );
    // The mark must not be drawn OVER a doorway: a plan that hides a drawn
    // destination while you stand in its doorway is a plan that lies about the
    // building, and `every_destination_the_plan_depicts_is_command_reachable`
    // counts those glyphs.
    let doorways = drawn(&before, '+');
    let stepped = out(session.handle("go n"));
    assert!(
        stepped.starts_with("You step") || stepped.starts_with("[chamber "),
        "the entry cell must be able to step north in the seed-42 structure: {stepped}"
    );
    let after = out(session.handle("map"));
    assert_ne!(
        before, after,
        "the possession stepped and the drawn plan did not change, so the mark \
         is not a cell position: {after}"
    );
    assert_eq!(drawn(&after, '@'), 1, "still exactly one mark: {after}");
    assert_eq!(
        drawn(&after, '+'),
        doorways,
        "stepping changed how many doorways the plan draws: {after}"
    );
}

// --- Task 6: differentiation ------------------------------------------------
//
// `CLIENT-language-not-catalogue` binds here (spec §4.3): the substance is which
// roles a brief admits and which patterns complete which. The pattern COUNT
// appears in none of the assertions below, on purpose.

#[test]
fn two_chambers_of_one_structure_do_not_read_alike() {
    // The Lintel's headline was literally true and experientially thin: four
    // doors onto one room (followup 11). This is the assertion that it stopped
    // being thin, made against the SAME structure the gallery walks — which is
    // warm, two-chambered and not a Seat, so differentiation that lived in the
    // hearth patterns or in `notability` would leave this green and unobserved.
    let w = world();
    let (mut session, _) = Session::start(&w, &PossessOpts::default()).unwrap();
    inside(&mut session);
    let first = out(session.handle("look"));
    let stepped = out(session.handle("enter further in"));
    assert!(
        stepped.starts_with("[chamber "),
        "this structure has only one chamber, so the headline cannot be observed \
         here at all: {stepped}"
    );
    let second = out(session.handle("look"));
    let prose = |s: &str| s.lines().nth(1).unwrap_or_default().to_string();
    assert_ne!(
        prose(&first),
        prose(&second),
        "two chambers of one structure still read identically"
    );
}

#[test]
fn a_role_admits_a_different_composition_not_a_bigger_one() {
    // The substance is WHICH patterns complete which, not how many exist. A role
    // whose composition is a superset of another's is a tier list, not a
    // vocabulary — so at least one pair of roles must each hold something the
    // other does not.
    use hornvale_vessel::interior::pattern::{Role, selection_for};
    // The fourth argument is `populous`, which the plan's snippet did not have:
    // the strongbox is population-gated, so the draw needs the flag. Held false
    // here so the pair compared is the ordinary case.
    let a: Vec<&str> = selection_for(Role::Threshold, true, false, false)
        .iter()
        .map(|p| p.name)
        .collect();
    let b: Vec<&str> = selection_for(Role::Store, true, false, false)
        .iter()
        .map(|p| p.name)
        .collect();
    assert!(
        a.iter().any(|n| !b.contains(n)) && b.iter().any(|n| !a.contains(n)),
        "one role's composition is a subset of the other's: {a:?} vs {b:?}"
    );
}

#[test]
fn a_locale_composition_is_untouched_by_the_role_layer() {
    // The load-bearing invariant of this task's DESIGN (ledger #10): whatever the
    // roles do to chambers, the band a creature stands in must be unaffected
    // unless we mean it to be. If this fails, the epoch is real -- which is a
    // finding, not a failure, but it must be a DELIBERATE one.
    use hornvale_vessel::interior::pattern::selection;
    let before = [
        "the-ground",
        "the-threshold",
        "the-alcove",
        "the-water-jar",
        "the-screen",
    ];
    let now: Vec<&str> = selection(true, false).iter().map(|p| p.name).collect();
    assert_eq!(
        now, before,
        "a locale's warm built composition changed, so warmth changed, so \
         committed NPC drive history changed: this IS an epoch"
    );
}

#[test]
fn the_role_table_reads_a_different_room_for_every_role() {
    // THE ROLE TABLE, OBSERVED. The plan shipped a PREDICTED table and said to
    // run it and print the prose before building on it, so the prose is built
    // here and asserted distinct rather than asserted equal to a remembered
    // string. Run with `--nocapture` to read the table.
    use hornvale_vessel::interior::pattern::{EVERY_ROLE, selection_for};
    use hornvale_vessel::interior::{Role, compose};
    use hornvale_vessel::{Brief, describe_chamber};
    let brief = |cold: bool| Brief::from_parts(None, None, None, None, 0, true, cold);
    let mut seen: std::collections::BTreeMap<String, Role> = std::collections::BTreeMap::new();
    for &role in EVERY_ROLE {
        for (cold, populous) in [(false, false), (true, false), (false, true)] {
            let text = describe_chamber(
                &compose(&selection_for(role, true, cold, populous)),
                &brief(cold),
            );
            let tag = if cold { "cold" } else { "warm" };
            let scale = if populous { ", populous" } else { "" };
            let label = format!("{role:?}");
            println!("  {label:<11} {tag}{scale:<10}  {text}");
            if !populous && !cold {
                assert!(
                    seen.insert(text.clone(), role).is_none(),
                    "{role:?} reads exactly as {:?} does, so the role table has a \
                     duplicate row and one of them is decoration: {text}",
                    seen[&text]
                );
            }
        }
    }
}

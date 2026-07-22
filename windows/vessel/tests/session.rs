//! Buffer-driven verb-loop tests — the repl::run pattern.

use hornvale_astronomy::SkyPins;
use hornvale_kernel::{Seed, World, WorldTime};
use hornvale_terrain::TerrainPins;
use hornvale_vessel::{PossessOpts, Session, Turn, run};
use hornvale_worldgen::{SettlementPins, SkyChoice, build_world};

fn seam_world() -> World {
    build_world(
        Seed(42),
        &SkyPins::default(),
        SkyChoice::Generated,
        &TerrainPins::default(),
        &SettlementPins::default(),
    )
    .expect("seed 42 builds")
}

fn opts() -> PossessOpts {
    PossessOpts {
        day: WorldTime { day: 0.0 },
        echo: false,
        wild_agents: true,
    }
}

#[test]
fn possession_opens_with_a_focalized_description() {
    let world = seam_world();
    let (_s, opening) = Session::start(&world, &opts()).unwrap();
    assert!(opening.contains("You stand in"));
    assert!(
        opening.contains("[room "),
        "the opening carries the room id"
    );
}

#[test]
fn go_moves_and_back_retraces() {
    let world = seam_world();
    let (mut s, _) = Session::start(&world, &opts()).unwrap();
    let home = s.agent().position.clone();
    // find a real direction from the current room's ways-on
    let ways = match s.handle("look") {
        Turn::Out(t) => t,
        _ => panic!("look must not release"),
    };
    // Exact, word-boundary token match against the "Ways on: NE, NW, S."
    // line — a substring check (e.g. `ways.contains("N")`) would false-
    // positive "n" against "NE"/"NW" whenever neither bare "N" nor "S" is
    // actually offered.
    let tokens: Vec<String> = ways
        .split([' ', ',', '.'])
        .filter(|t| !t.is_empty())
        .map(str::to_lowercase)
        .collect();
    let dir = ["n", "ne", "e", "se", "s", "sw", "w", "nw"]
        .iter()
        .find(|d| tokens.iter().any(|t| t == *d))
        .copied()
        .expect("some way on");
    match s.handle(&format!("go {dir}")) {
        Turn::Out(t) => assert!(t.contains("[room ")),
        _ => panic!("go must not release"),
    }
    assert_ne!(s.agent().position, home, "go moved");
    s.handle("back");
    assert_eq!(s.agent().position, home, "back retraces");
}

#[test]
fn vertical_exits_refuse_diegetically() {
    let world = seam_world();
    let (mut s, _) = Session::start(&world, &opts()).unwrap();
    let before = s.agent().position.clone();
    let out = match s.handle("exit") {
        Turn::Out(t) => t,
        _ => panic!("exit must not release"),
    };
    assert!(out.contains("grain of the world"), "diegetic refusal");
    assert_eq!(s.agent().position, before, "no movement");
    let out = match s.handle("enter") {
        Turn::Out(t) => t,
        _ => panic!("enter must not release"),
    };
    assert!(out.contains("grain of the world"));
}

#[test]
fn examine_honors_the_contract_and_release_ends() {
    let world = seam_world();
    let (mut s, _) = Session::start(&world, &opts()).unwrap();
    let f = s.focalized().unwrap();
    for (noun, detail) in &f.nouns {
        match s.handle(&format!("examine {noun}")) {
            Turn::Out(t) => assert_eq!(&t, detail, "examine renders the datum"),
            _ => panic!("examine must not release"),
        }
    }
    match s.handle("examine the moon of unreason") {
        Turn::Out(t) => assert!(t.contains("You see no")),
        _ => panic!(),
    }
    assert!(matches!(s.handle("release"), Turn::Released(_)));
}

#[test]
fn wait_advances_the_day_and_moves_the_npc_layer_without_moving_you() {
    // The-quickening (T3): `wait` now runs the NPC layer's tick, so its
    // output narrates motion rather than re-describing the room. The
    // observation day still advances (visible via a follow-up `look`), and
    // the possessed agent itself still never moves — only the session's
    // owned NPC ledger evolves.
    let world = seam_world();
    let (mut s, opening) = Session::start(&world, &opts()).unwrap();
    assert!(opening.contains("day 0"));
    let home = s.agent().position.pack().unwrap().0;
    let out = match s.handle("wait 90") {
        Turn::Out(t) => t,
        _ => panic!("wait must not release"),
    };
    assert!(!out.is_empty(), "wait narrates what happened");
    match s.handle("look") {
        Turn::Out(t) => assert!(t.contains("day 90"), "the observation day moved"),
        _ => panic!("look must not release"),
    }
    assert_eq!(
        s.agent().position.pack().unwrap().0,
        home,
        "waiting does not move the possessed agent"
    );
    match s.handle("wait sideways") {
        Turn::Out(t) => assert!(t.contains("no span of days")),
        _ => panic!(),
    }
    match s.handle("wait inf") {
        Turn::Out(t) => assert!(
            t.contains("no span of days"),
            "non-finite span refused: {t}"
        ),
        _ => panic!(),
    }
}

#[test]
fn knows_grows_as_you_walk() {
    let world = seam_world();
    let (mut s, _) = Session::start(&world, &opts()).unwrap();
    let before = s.knowledge().0.len();
    let ways = match s.handle("look") {
        Turn::Out(t) => t,
        _ => panic!(),
    };
    // Exact, word-boundary token match — see `go_moves_and_back_retraces`'s
    // comment: a substring check false-positives "n" against "NE"/"NW".
    let tokens: Vec<String> = ways
        .split([' ', ',', '.'])
        .filter(|t| !t.is_empty())
        .map(str::to_lowercase)
        .collect();
    let dir = ["n", "ne", "e", "se", "s", "sw", "w", "nw"]
        .iter()
        .find(|d| tokens.iter().any(|t| t == *d))
        .copied()
        .expect("some way on");
    s.handle(&format!("go {dir}"));
    assert!(
        s.knowledge().0.len() > before,
        "walking accumulates knowledge"
    );
}

/// The Vessel Stitch T2: `tell` renamed `write` (G3, total — no alias).
/// Re-pins `tell_absorbs_a_spoken_common_sentence_into_knowledge`
/// (provenance: this test replaces it verbatim, verb and response
/// swapped) and adds the rename's own obligation — `tell` must now fall
/// through to the ordinary unknown-verb response.
#[test]
fn write_is_the_verb_and_the_margin_answers() {
    let world = seam_world();
    let (mut s, _) = Session::start(&world, &opts()).unwrap();
    let volume = hornvale_book::render_volume(&world);
    let line = volume
        .lines
        .first()
        .expect("seed 42 renders at least one line");
    let before = s.knowledge().0.len();
    let out = match s.handle(&format!("write {line}")) {
        Turn::Out(t) => t,
        _ => panic!("write must not release"),
    };
    assert_eq!(
        out, "Written in the margin.",
        "the closed margin response, regardless of how many facts the sentence carried"
    );
    assert!(
        s.knowledge().0.len() > before,
        "writing a fact grows knowledge"
    );
    match s.handle("write") {
        Turn::Out(t) => assert!(t.contains("Write what?")),
        _ => panic!("write with no argument must not release"),
    }
    // The rename is total: no `tell` alias survives.
    match s.handle(&format!("tell {line}")) {
        Turn::Out(t) => assert!(
            t.contains("No verb 'tell'"),
            "tell falls through to the unknown-verb response: {t}"
        ),
        _ => panic!("tell must not release"),
    }
}

/// The Vessel Stitch T2's stitch law (spec §4.1, end to end): a fresh
/// session's `consult` shows the fallback and the day-0 reckoning's empty
/// arm; `write`-ing the moon sentence unlocks the initiated line, whose
/// rendered count is the LEDGER's own value — the mutation arm proves this
/// is not an echo of what was written: even a WRONG written count still
/// unlocks the key, but the printed value never moves (heard ≠ true,
/// printed — spec §1/§8.2).
#[test]
fn the_stitch_law_end_to_end() {
    let world = build_world(
        Seed(1),
        &SkyPins::default(),
        SkyChoice::Generated,
        &TerrainPins::default(),
        &SettlementPins::default(),
    )
    .expect("seed 1 builds");
    let volume = hornvale_book::render_volume(&world);
    let planet_line = volume
        .lines
        .iter()
        .find(|l| l.contains(" is a planet"))
        .expect("seed 1 renders a planet line");
    assert!(
        planet_line.contains("with two moons"),
        "seed 1's planet line carries the two-moon fragment: {planet_line}"
    );

    let (mut s, _) = Session::start(
        &world,
        &PossessOpts {
            day: WorldTime { day: 0.0 },
            echo: false,
            wild_agents: true,
        },
    )
    .unwrap();
    let before = match s.handle("consult") {
        Turn::Out(t) => t,
        _ => panic!("consult must not release"),
    };
    assert!(
        before.contains("The Book holds more for the initiated."),
        "nothing written yet: the fallback answers: {before}"
    );
    assert!(
        before.contains("The sky keeps no dates to number."),
        "day 0's true event count is zero — the empty arm: {before}"
    );

    match s.handle(&format!("write {planet_line}")) {
        Turn::Out(t) => assert_eq!(t, "Written in the margin."),
        _ => panic!("write must not release"),
    }
    let after = match s.handle("consult") {
        Turn::Out(t) => t,
        _ => panic!("consult must not release"),
    };
    assert!(
        // The Book Polish (2026-07-20): re-pinned with its subject. The Living
        // Community epoch (this merge): seed 1's rendered planet name
        // re-derived Vebe -> Xobo under the epoch's re-placement.
        after.contains("Xobo has two moons, as the initiated count."),
        "the ledger's own moon-count, now unlocked: {after}"
    );
    assert!(
        !after.contains("The Book holds more for the initiated."),
        "the fallback no longer applies once something has unlocked: {after}"
    );

    // MUTATION ARM: a fresh session, told a WRONG count, still unlocks the
    // key — but the printed value is the ledger's, never the heard one.
    let (mut wrong, _) = Session::start(
        &world,
        &PossessOpts {
            day: WorldTime { day: 0.0 },
            echo: false,
            wild_agents: true,
        },
    )
    .unwrap();
    let wrong_line = planet_line.replace("with two moons", "with nine moons");
    match wrong.handle(&format!("write {wrong_line}")) {
        Turn::Out(t) => assert_eq!(t, "Written in the margin."),
        _ => panic!("write must not release"),
    }
    let consulted = match wrong.handle("consult") {
        Turn::Out(t) => t,
        _ => panic!("consult must not release"),
    };
    assert!(
        consulted.contains("Xobo has two moons, as the initiated count."),
        "heard 'nine' still renders the ledger's 'two' — heard is not true, printed: {consulted}"
    );
    assert!(
        !consulted.contains("nine"),
        "the wrong heard count never appears in what the Book confirms: {consulted}"
    );
}

/// The Vessel Stitch T2's day law (spec §4.2): `consult`'s heading tracks
/// the session's own day, monotone with play — day 0 at the start, the
/// session's actual (truncated) day after `wait`.
#[test]
fn the_day_law() {
    let world = seam_world();
    let (mut s, _) = Session::start(&world, &opts()).unwrap();
    match s.handle("consult") {
        Turn::Out(t) => assert!(
            t.starts_with("The Reckoning, at day 0."),
            "day-0 heading: {t}"
        ),
        _ => panic!("consult must not release"),
    }
    s.handle("wait 90");
    match s.handle("consult") {
        Turn::Out(t) => assert!(
            t.starts_with("The Reckoning, at day 90."),
            "the heading advances to the session's own day: {t}"
        ),
        _ => panic!("consult must not release"),
    }
}

/// The Vessel Stitch T2's purity law (spec §4.3): `consult` commits
/// nothing — the session's owned ledger is byte-identical before and
/// after, and `consult` never touches `Knowledge` either (only `write`
/// and walking do).
#[test]
fn the_purity_law() {
    let world = seam_world();
    let (mut s, _) = Session::start(&world, &opts()).unwrap();
    let ledger_before = s.session_ledger_json();
    let knowledge_before = s.knowledge().clone();
    s.handle("consult");
    assert_eq!(
        s.session_ledger_json(),
        ledger_before,
        "consult must not commit anything to the ledger"
    );
    assert_eq!(
        *s.knowledge(),
        knowledge_before,
        "consult must not mutate Knowledge"
    );
}

#[test]
fn run_drives_a_script_deterministically() {
    let world = seam_world();
    let script = "look\nwhoami\nknows\nrelease\n";
    let mut out_a = Vec::new();
    let mut out_b = Vec::new();
    run(
        &world,
        PossessOpts {
            day: WorldTime { day: 0.0 },
            echo: true,
            wild_agents: true,
        },
        std::io::Cursor::new(script),
        &mut out_a,
    )
    .unwrap();
    run(
        &world,
        PossessOpts {
            day: WorldTime { day: 0.0 },
            echo: true,
            wild_agents: true,
        },
        std::io::Cursor::new(script),
        &mut out_b,
    )
    .unwrap();
    assert_eq!(out_a, out_b, "byte-identical replays");
    let text = String::from_utf8(out_a).unwrap();
    assert!(text.contains("> look"), "echo mode echoes commands");
    assert!(text.contains("You stand in"));
}

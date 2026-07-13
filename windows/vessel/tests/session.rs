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
    let dir = ["n", "ne", "e", "se", "s", "sw", "w", "nw"]
        .iter()
        .find(|d| {
            ways.to_lowercase().contains(&format!(" {}", d)) || ways.contains(&d.to_uppercase())
        })
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
fn wait_advances_the_frozen_day_without_moving_anything() {
    let world = seam_world();
    let (mut s, opening) = Session::start(&world, &opts()).unwrap();
    assert!(opening.contains("day 0"));
    let home = s.agent().position.pack().unwrap().0;
    let out = match s.handle("wait 90") {
        Turn::Out(t) => t,
        _ => panic!("wait must not release"),
    };
    assert!(out.contains("day 90"), "the observation day moved");
    assert_eq!(
        s.agent().position.pack().unwrap().0,
        home,
        "waiting does not move the agent"
    );
    match s.handle("wait sideways") {
        Turn::Out(t) => assert!(t.contains("no span of days")),
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
    let dir = ["n", "ne", "e", "se", "s", "sw", "w", "nw"]
        .iter()
        .find(|d| ways.contains(&d.to_uppercase()))
        .copied()
        .expect("some way on");
    s.handle(&format!("go {dir}"));
    assert!(
        s.knowledge().0.len() > before,
        "walking accumulates knowledge"
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

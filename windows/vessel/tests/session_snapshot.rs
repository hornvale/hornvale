//! The `vessel/session/v1` byte pin and its tie to the published
//! transcript. This fixture changing is the epoch decision point (The
//! Snapshot spec §9): regenerate deliberately, never casually, with
//! `REBASELINE=1 cargo test -p hornvale-vessel --test session_snapshot`
//! (or `make rebaseline-goldens`), then review the diff as a contract
//! change.

use hornvale_kernel::{Seed, World};
use hornvale_vessel::{PossessOpts, Session, SpatialChannel, snapshot_json};

fn world() -> World {
    hornvale_worldgen::build_world(
        Seed(42),
        &Default::default(),
        hornvale_worldgen::SkyChoice::Generated,
        &Default::default(),
        &Default::default(),
    )
    .expect("seed 42 builds")
}

/// The same script the committed transcript walks, up to its first `go`.
const SCRIPT: &[&str] = &["look", "examine sky", "whoami"];

/// Day **0**, not `PossessOpts::default()`'s noon.
///
/// This is load-bearing for `narration_is_byte_identical_to_the_published_transcript`:
/// the committed transcript is produced by `scripts/regenerate-artifacts.sh`
/// running `hornvale possess --world … --script …` with **no `--day` flag**,
/// and the CLI's `parse_possess_day` defaults to `"0"` (cli/src/main.rs:393).
/// `describe_here` interpolates that day straight into the room header, so the
/// transcript reads `[room 738918402, day 0]`. `PossessOpts::default()` is
/// `day: 0.5` (a deliberate choice so a bare `wait 1` lands at noon again), and
/// using it here would render `day 0.5` and fail the comparison against a
/// transcript that is not wrong — only taken at a different hour.
fn opts() -> PossessOpts {
    PossessOpts {
        day: hornvale_kernel::WorldTime { day: 0.0 },
        echo: false,
        wild_agents: true,
    }
}

fn snapshots(world: &World) -> Vec<String> {
    let (mut session, _) = Session::start(world, &opts()).expect("seed 42 possesses");
    let mut out = vec![snapshot_json(&session.snapshot().unwrap())];
    for line in SCRIPT {
        session.handle(line);
        out.push(snapshot_json(&session.snapshot().unwrap()));
    }
    out
}

#[test]
fn v1_bytes_are_pinned() {
    let world = world();
    let joined = snapshots(&world).join("\n");
    hornvale_kernel::golden::assert_golden(
        std::path::Path::new(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/tests/fixtures/session-seed-42.json"
        )),
        &joined,
        "vessel/session/v1 bytes moved — this is the epoch decision point (The Snapshot \
         spec §9); accept deliberately and review the diff as a contract change",
    );
}

#[test]
fn the_snapshot_sequence_is_deterministic() {
    let a = snapshots(&world());
    let b = snapshots(&world());
    assert_eq!(a, b, "same seed + same script must yield the same bytes");
}

#[test]
fn narration_is_byte_identical_to_the_published_transcript() {
    // The oldest golden in this seam is the committed transcript the book
    // publishes; tying the newest channel to it is the strongest available
    // check. This is drive.mjs's own trick, in Rust.
    let md = std::fs::read_to_string(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/../../book/src/gallery/possession-seed-42.md"
    ))
    .expect("the committed transcript is readable");
    let fence = "```text\n";
    let start = md.find(fence).expect("the transcript has a ```text fence") + fence.len();
    let body = &md[start..];
    let opening = &body[..body.find("\n> ").expect("the transcript has a prompt line")];

    let world = world();
    let (session, _) = Session::start(&world, &opts()).unwrap();
    let snap = session.snapshot().unwrap();
    assert_eq!(
        snap.narration.prose.trim_end(),
        opening.trim_end(),
        "narration.prose must be the transcript's own opening, byte for byte"
    );
}

#[test]
fn the_fixture_is_taken_at_the_transcript_s_own_day() {
    // A guard on the guard: if someone later switches `opts()` to
    // `PossessOpts::default()`, the byte-identity test above starts failing
    // for a reason that looks like worldgen drift but isn't. Pin the intent.
    assert_eq!(
        opts().day.day,
        0.0,
        "the committed transcript is a day-0 recording"
    );
}

#[test]
fn the_embedded_room_carries_its_own_pinned_schema_tag() {
    // `sensed.room` embeds `locale/room/v2` verbatim (spec §3: one schema,
    // one owner). Nothing else in this test file asserts the tag, so a
    // future room epoch would otherwise surface only as an 18 KB fixture
    // diff in `v1_bytes_are_pinned` above — assert it by name instead.
    let world = world();
    let (session, _) = Session::start(&world, &opts()).unwrap();
    let snap = session.snapshot().unwrap();
    assert_eq!(
        snap.sensed.room.schema,
        hornvale_locale::ROOM_SCHEMA,
        "the embedded room's schema tag moved — a room epoch should fail by \
         name here, not as an opaque fixture diff"
    );
}

#[test]
fn a_settlement_free_world_refuses_possession_rather_than_panicking() {
    // A world with no settlement has no flagship to mint and no snapshot to
    // take; the refusal must be the sim's own typed error, not a panic.
    //
    // THE FIXTURE IS CONSTRUCTED, NOT HUNTED. This used to scout `43..80` for a
    // seed that happened to generate no settlement, on the reasoning that
    // hardcoding one seed is fragile because settlement-freeness is a worldgen
    // output that moves. That reasoning was right and the remedy was wrong:
    // scouting is fragile in the same way, just later and more expensively. The
    // Tense made empty worlds rare -- habitability became a relation between a
    // species and a cell instead of a global -10 C snowline, so cold ground is
    // poor rather than forbidden, and seed 1234, which had ZERO survivors for a
    // whole campaign, now carries 36. The scout found nothing in 43..80,
    // widening it to 43..400 meant building 357 full worlds, and neither
    // outcome would have told a reader anything about the refusal path.
    //
    // `BuildDepth::Terrain` gives the fixture directly: terrain and climate are
    // present, so `Session::start`'s derivation succeeds and the error under
    // test is reachable, while the settlement stage never runs -- so the world
    // is settlement-free BY CONSTRUCTION rather than by luck, on every seed,
    // forever, in one build.
    let wc = hornvale_worldgen::WorldComponents::assemble().expect("components assemble");
    let w = hornvale_worldgen::build_world_to(
        Seed(42),
        &Default::default(),
        hornvale_worldgen::SkyChoice::Generated,
        &Default::default(),
        &Default::default(),
        &wc,
        hornvale_worldgen::BuildDepth::Terrain,
    )
    .expect("a terrain-depth world builds");

    let err = Session::start(&w, &PossessOpts::default())
        .err()
        .expect("possession must refuse a settlement-free world, not succeed");
    assert!(
        matches!(err, hornvale_vessel::VesselError::NoSettlement),
        "refused for the wrong reason: {err}"
    );
}

#[test]
fn out_of_doors_the_spatial_channel_is_the_walk_band_chart() {
    let world = world();
    let (session, _) = Session::start(&world, &PossessOpts::default()).unwrap();
    let snap = session.snapshot().expect("a live session snapshots");
    match &snap.spatial {
        SpatialChannel::Walk { chart } => {
            // v2 since The Benchmark: the relief bands measure height above sea
            // level rather than the raw isostatic reading, and the document now
            // carries `sea_level_m` so a consumer can re-derive them. The
            // embedded chart announces its own version, which is why the
            // enclosing `vessel/session/v1` does not move with it.
            assert_eq!(chart.schema, "scene/surrounds/v2");
            assert!(
                !chart.cells.is_empty(),
                "a chart with no cells shows nothing"
            );
        }
        SpatialChannel::Chamber { .. } => {
            panic!("the possession opens out of doors, not inside a building")
        }
    }
}

#[test]
fn inside_a_building_the_spatial_channel_is_the_chamber_plan() {
    let world = world();
    let (mut session, _) = Session::start(&world, &PossessOpts::default()).unwrap();
    session.handle("enter");
    let snap = session.snapshot().expect("a live session snapshots");
    match &snap.spatial {
        SpatialChannel::Chamber { plan } => {
            assert_eq!(plan.schema, "vessel/plan/v1");
            assert_eq!(
                plan.cells.len(),
                (plan.extent.w * plan.extent.h) as usize,
                "the emitted grid must stay total"
            );
        }
        SpatialChannel::Walk { .. } => panic!("`enter` puts the possession inside"),
    }
}

#[test]
fn the_band_tag_is_what_the_client_switches_on() {
    // The client reads `spatial.band` before anything else, so the wire tag
    // is contract and a rename is a v2. Asserted on the BYTES, not the enum:
    // a `#[serde(rename)]` slip is invisible to a match arm.
    let world = world();
    let (mut session, _) = Session::start(&world, &PossessOpts::default()).unwrap();
    let walk = hornvale_vessel::snapshot_json(&session.snapshot().unwrap());
    assert!(
        walk.contains(r#""band":"walk""#),
        "walk tag missing: {walk:.200}"
    );
    session.handle("enter");
    let chamber = hornvale_vessel::snapshot_json(&session.snapshot().unwrap());
    assert!(
        chamber.contains(r#""band":"chamber""#),
        "chamber tag missing: {chamber:.200}"
    );
}

#[test]
fn a_creature_standing_in_the_chamber_reaches_the_plan() {
    // The Sighting, test 1. `wait` before `enter` is load-bearing and is the
    // answer to "why does the committed chamber fixture carry no marks": the
    // within-room `Occupancy` is populated by `DriveMovements::step_with_occupancy`,
    // which only runs on a tick, so before the first `wait` NO creature has a
    // fine-layer position and the embedding has nothing to place. The fixture
    // script is `enter` alone, at turn 1.
    let world = world();
    let (mut session, _) = Session::start(&world, &PossessOpts::default()).unwrap();
    session.handle("wait");
    session.handle("enter");
    let snap = session.snapshot().expect("a live session snapshots");
    let SpatialChannel::Chamber { plan } = &snap.spatial else {
        panic!("`enter` puts the possession inside")
    };
    let marks = plan.marks.clone();
    let extent = plan.extent;
    let you = plan.you;

    assert!(
        !marks.is_empty(),
        "a chamber holding a co-located creature must draw it: present = {:?}",
        snap.sensed.present
    );
    for mark in &marks {
        // The NPC's OWN noun, not a generic one — the join `PlanMark` took the
        // focalizer's shape for.
        assert!(
            snap.sensed.present.iter().any(|p| p.label == mark.noun),
            "mark {mark:?} names no creature `sensed.present` reports"
        );
        // `"agent"`, the SAME word `scene/surrounds/v2` marks this creature with
        // one band up (`purview::AGENT_MARK_KIND`). A second word for one thing
        // would make a client learn two vocabularies to draw one creature.
        assert_eq!(mark.kind, "agent", "a creature's mark says what it is");
        // Inside the extent, and standing on a cell it could stand on: the
        // plan's own grid is total, so a mark outside it would be undrawable.
        assert!(
            mark.x >= extent.x
                && mark.x < extent.x + extent.w
                && mark.y >= extent.y
                && mark.y < extent.y + extent.h,
            "mark {mark:?} is outside the extent {extent:?}"
        );
        assert!(
            !(mark.x == you.x && mark.y == you.y),
            "a creature was drawn in the possession's own cell — §7 rule 5"
        );
    }

    // `PlanMark.datum` promises to be "the datum `examine` prints". Asserted
    // against the verb rather than against a literal, because a literal cannot
    // tell the two apart when only one of them moves (fix round 1: they had
    // already diverged, and no test could see it).
    for mark in &marks {
        let printed = match session.handle(&format!("examine {}", mark.noun)) {
            hornvale_vessel::Turn::Out(t) | hornvale_vessel::Turn::Released(t) => t,
        };
        assert_eq!(
            printed, mark.datum,
            "the mark's datum is not what `examine {}` prints",
            mark.noun
        );
    }
}

/// The committed fixtures the Casement's pane tests decode.
///
/// Byte goldens, refreshed with `REBASELINE=1` like every other golden in
/// this repo. A diff here means the wire shape moved, which is the epoch
/// decision point — never rebaseline to make a red run green without
/// deciding that first.
///
/// **Three fixtures, and the third is The Sighting's.** `…-chamber.json` is
/// taken at turn 1 on the script `enter` alone, so no tick has ever run, the
/// within-room `Occupancy` is still its empty default, and its `marks` array is
/// therefore `[]` — legitimately, not because nothing writes the field. That
/// makes it the wrong fixture to decode a mark from, so `…-chamber-occupied.json`
/// is taken one `wait` earlier and carries a real creature. It is ADDITIVE: the
/// two older fixtures' scripts are untouched, because changing one to gain a mark
/// would have moved `turn`, `day` and `narration` in a file whose whole job is to
/// hold those still.
#[test]
fn the_client_fixtures_are_current() {
    let world = world();
    let (mut session, _) = Session::start(&world, &PossessOpts::default()).unwrap();

    let walk = hornvale_vessel::snapshot_json(&session.snapshot().unwrap());
    session.handle("enter");
    let chamber = hornvale_vessel::snapshot_json(&session.snapshot().unwrap());

    let (mut occupied_session, _) = Session::start(&world, &PossessOpts::default()).unwrap();
    occupied_session.handle("wait");
    occupied_session.handle("enter");
    let occupied = hornvale_vessel::snapshot_json(&occupied_session.snapshot().unwrap());

    for (name, body) in [
        ("snapshot-seed-42-walk.json", walk),
        ("snapshot-seed-42-chamber.json", chamber),
        ("snapshot-seed-42-chamber-occupied.json", occupied),
    ] {
        let path = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("tests/fixtures")
            .join(name);
        if std::env::var("REBASELINE").is_ok() {
            std::fs::write(&path, &body).expect("the fixture directory exists");
            continue;
        }
        let committed = std::fs::read_to_string(&path)
            .unwrap_or_else(|_| panic!("{name} is missing — run with REBASELINE=1"));
        assert_eq!(
            committed, body,
            "{name} drifted: the vessel/session/v1 wire shape moved. Decide \
             whether that is an epoch BEFORE rebaselining."
        );
    }
}

#[test]
fn the_snapshot_stays_a_pure_read() {
    // `Session::snapshot` documents that it never commits and never advances
    // the turn counter. Adding a channel that BUILDS a chart is exactly the
    // change that could break that, so it is asserted rather than assumed.
    let world = world();
    let (mut session, _) = Session::start(&world, &PossessOpts::default()).unwrap();
    session.handle("look");
    let a = hornvale_vessel::snapshot_json(&session.snapshot().unwrap());
    let b = hornvale_vessel::snapshot_json(&session.snapshot().unwrap());
    assert_eq!(
        a, b,
        "two snapshots with no verb between them must be identical"
    );
}

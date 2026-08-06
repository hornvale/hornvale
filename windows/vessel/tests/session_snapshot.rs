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
    // Some worlds generate no settlement at all, so there is no flagship to
    // mint and no snapshot to take; the refusal must be the sim's own
    // error. SCOUTED, never hardcoded: which seeds are settlement-free is a
    // worldgen output that moves, and hardcoding one is exactly the bug that
    // left `make vessel-check` red on main (Task 4 fixes the same mistake in
    // drive.mjs — do not reintroduce it here).
    let refused = (43u64..80).find_map(|seed| {
        let w = hornvale_worldgen::build_world(
            Seed(seed),
            &Default::default(),
            hornvale_worldgen::SkyChoice::Generated,
            &Default::default(),
            &Default::default(),
        )
        .expect("the world builds even with no settlement");
        Session::start(&w, &PossessOpts::default())
            .err()
            .map(|e| (seed, e))
    });
    let (seed, err) = refused.expect("some seed in 43..80 has no settlement");
    assert!(
        matches!(err, hornvale_vessel::VesselError::NoSettlement),
        "seed {seed} refused for the wrong reason: {err}"
    );
}

#[test]
fn out_of_doors_the_spatial_channel_is_the_walk_band_chart() {
    let world = world();
    let (session, _) = Session::start(&world, &PossessOpts::default()).unwrap();
    let snap = session.snapshot().expect("a live session snapshots");
    match &snap.spatial {
        SpatialChannel::Walk { chart } => {
            assert_eq!(chart.schema, "scene/surrounds/v1");
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

/// The committed fixtures the Casement's pane tests decode.
///
/// Byte goldens, refreshed with `REBASELINE=1` like every other golden in
/// this repo. A diff here means the wire shape moved, which is the epoch
/// decision point — never rebaseline to make a red run green without
/// deciding that first.
#[test]
fn the_client_fixtures_are_current() {
    let world = world();
    let (mut session, _) = Session::start(&world, &PossessOpts::default()).unwrap();

    let walk = hornvale_vessel::snapshot_json(&session.snapshot().unwrap());
    session.handle("enter");
    let chamber = hornvale_vessel::snapshot_json(&session.snapshot().unwrap());

    for (name, body) in [
        ("snapshot-seed-42-walk.json", walk),
        ("snapshot-seed-42-chamber.json", chamber),
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

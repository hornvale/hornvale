//! The `vessel/session/v1` byte pin and its tie to the published
//! transcript. This fixture changing is the epoch decision point (The
//! Snapshot spec §9): regenerate deliberately, never casually, with
//! `REBASELINE=1 cargo test -p hornvale-vessel --test session_snapshot`
//! (or `make rebaseline-goldens`), then review the diff as a contract
//! change.

use hornvale_kernel::{Seed, World};
use hornvale_vessel::{PossessOpts, Session, snapshot_json};

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

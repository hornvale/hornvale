//! The `vessel/session/v2` byte pin and its tie to the published
//! transcript. This fixture changing is the epoch decision point (The
//! Snapshot spec §9): regenerate deliberately, never casually, with
//! `REBASELINE=1 cargo test -p hornvale-vessel --test suite -- session_snapshot`
//! (or `make rebaseline-goldens`), then review the diff as a contract
//! change.

use hornvale_kernel::{Seed, World};
use hornvale_vessel::{PossessOpts, Session, SpatialChannel, snapshot_json};

use crate::common;

/// The seed the **client fixture** is taken at.
///
/// A golden cannot sweep — it is one file holding the bytes of one world — so
/// this stays concrete while the tests around it search (see `common/mod.rs`).
/// It is the lowest seed in `common::SIGHT_SEEDS` that draws a creature, and
/// `the_client_fixtures_are_current` asserts that it still does, loudly: if a
/// future reseed moves this world the way The Tense moved seed 42, the fixture
/// says so by name instead of quietly freezing an empty `marks` array.
///
/// That alarm has now fired for real, exactly as designed. Decision 0131's
/// terrain epoch moved seed 1's opening chamber to one with no creature in it,
/// the assert named the problem instead of freezing an empty array, and this
/// was re-pointed 1 → 0 (`common::world_that_draws_a_creature` reports the new
/// lowest qualifying seed). The fixture was renamed to match, because a file
/// called `seed-1` holding seed 0's bytes is the quiet lie this whole
/// arrangement exists to prevent — which means the Casement's
/// `pane_plan_marks_test.ts` had to be re-pointed in the same commit, since it
/// opens the fixture BY NAME.
///
/// **THE GLASSHOUSE, Stage B Task 4: re-pointed 0 → 1.** The thermostat (a
/// damped, greenhouse-forced insolation baseline replacing the fixed 288 K
/// blackbody one) moved seed 0's opening chamber to one with no creature in
/// it — the alarm fired again, exactly as designed, and
/// `common::world_that_draws_a_creature` reports 1 as the new lowest
/// qualifying seed. Renamed to match for the same reason as before.
const OCCUPIED_SEED: u64 = 0;

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
/// `whoami` is sigilled (The Deed, Task 5): the bare form is retired, and
/// this script exists to snapshot a real verb's narration at each step, not
/// the now-retired form's unknown-verb refusal.
const SCRIPT: &[&str] = &["look", "examine sky", "!whoami"];

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
        day: hornvale_kernel::WorldTime::GENESIS,
        echo: false,
        wild_agents: true,
        eyes: hornvale_vessel::eyes::Eyes::Own,
        lens: hornvale_vessel::lens::Lens::Off,
        target: hornvale_vessel::PossessTarget::Flagship,
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
fn v2_bytes_are_pinned() {
    let world = world();
    let joined = snapshots(&world).join("\n");
    hornvale_kernel::golden::assert_golden(
        std::path::Path::new(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/tests/fixtures/session-seed-42.json"
        )),
        &joined,
        "vessel/session/v2 bytes moved — this is the epoch decision point (The Snapshot \
         spec §9); accept deliberately and review the diff as a contract change",
    );
}

#[test]
fn noun_kind_rides_the_wire_additively() {
    // The Lexicon Task 2: `NounEntry` carries a coarse kind for
    // completion-capable clients. Additive on `vessel/session/v2` — serde
    // default keeps older mirrors loading — and rendered as the lowercase tag
    // of `NounKind`. The first noun a fresh snapshot narrates is the biome
    // descriptor, which focalize tags `Place`.
    let world = world();
    let (session, _) = Session::start(&world, &opts()).unwrap();
    let json = snapshot_json(&session.snapshot().unwrap());
    let v: serde_json::Value = serde_json::from_str(&json).expect("a snapshot parses");
    assert_eq!(
        v["narration"]["nouns"][0]["kind"].as_str(),
        Some("place"),
        "nouns must carry their coarse kind on the wire: {json:.200}"
    );
}

#[test]
fn the_schema_tag_is_the_one_every_client_pins() {
    // Asserted on the BYTES and against a literal, not against
    // `SESSION_SCHEMA`: the constant and the wire agreeing is tautological,
    // and the thing that must not move silently is the string three separate
    // consumers compare against by hand (`clients/vessel/src/snapshot.ts`,
    // `clients/vessel/wasm/drive.mjs`, `clients/game/core/tests/schema.rs`).
    let world = world();
    let (session, _) = Session::start(&world, &opts()).unwrap();
    let json = snapshot_json(&session.snapshot().unwrap());
    assert!(
        json.contains(r#""schema":"vessel/session/v2""#),
        "the session schema tag moved; every consumer that pins it by literal \
         must move in the same commit: {json:.120}"
    );
}

/// The largest integer a JavaScript `number` (an IEEE-754 double) holds
/// exactly. Above this, `JSON.parse` silently rounds — two ids can collapse
/// onto one value, which is a correctness bug and not a display one.
const MAX_SAFE_INTEGER: u64 = (1u64 << 53) - 1;

/// Every `"key": <bare integer>` in `json` whose magnitude exceeds
/// [`MAX_SAFE_INTEGER`], as `(key, digits)`.
///
/// A text scan rather than a `serde_json` walk on purpose: parsing turns the
/// very numbers under test into `f64`s and destroys the evidence — the
/// scanner would be asking the question with the answer already rounded off.
/// String state is tracked so a colon or a long run of digits inside
/// narration prose cannot masquerade as a key or a value. The corollary is
/// that it does not descend into the `known` channel's values, which are
/// *escaped* JSON documents carried as strings: a client re-parsing one gets
/// its own numbers back, and nothing in there is an id (they are room ids,
/// which are ~30 bits). If a channel ever carries an escaped id, this scan
/// will not see it.
fn javascript_unsafe_integers(json: &str) -> Vec<(String, String)> {
    let bytes = json.as_bytes();
    let mut found = Vec::new();
    let mut key = String::new();
    let mut i = 0;
    while i < bytes.len() {
        match bytes[i] {
            b'"' => {
                let start = i + 1;
                let mut j = start;
                while j < bytes.len() && bytes[j] != b'"' {
                    j += if bytes[j] == b'\\' { 2 } else { 1 };
                }
                let text = &json[start..j.min(json.len())];
                i = j + 1;
                // A string immediately followed by `:` is an object key.
                if bytes.get(i) == Some(&b':') {
                    key = text.to_string();
                }
            }
            c if c.is_ascii_digit() || c == b'-' => {
                let start = i;
                let mut j = i + usize::from(c == b'-');
                while j < bytes.len() && bytes[j].is_ascii_digit() {
                    j += 1;
                }
                // A float is not the hazard this scan is for: it is already
                // an inexact type on both sides of the wire, and the emit
                // boundary quantizes it (decision 0033).
                let is_float = matches!(bytes.get(j), Some(b'.' | b'e' | b'E'));
                let digits = &json[start..j];
                if !is_float
                    && digits
                        .trim_start_matches('-')
                        .parse::<u64>()
                        .is_ok_and(|v| v > MAX_SAFE_INTEGER)
                {
                    found.push((key.clone(), digits.to_string()));
                }
                i = j.max(start + 1);
            }
            _ => i += 1,
        }
    }
    found
}

#[test]
fn no_emitted_number_can_lose_precision_in_a_javascript_client() {
    // The whole reason `vessel/session/v1` became v2. Swept over every
    // snapshot of the pinned script AND over the chamber band, because the
    // two bands emit different documents and only one of them was ever the
    // fixture a client test happened to read.
    let world = world();
    let mut documents = snapshots(&world);
    let (mut session, _) = Session::start(&world, &PossessOpts::default()).unwrap();
    documents.push(snapshot_json(&session.snapshot().unwrap()));
    session.handle("enter");
    documents.push(snapshot_json(&session.snapshot().unwrap()));

    for json in &documents {
        let wide = javascript_unsafe_integers(json);
        assert!(
            wide.is_empty(),
            "these keys emit bare integers above 2^53, which JSON.parse rounds: \
             {wide:?} — encode them as decimal strings the way `self.agent` and \
             `entity` already are"
        );
    }
}

#[test]
fn the_precision_scan_would_catch_a_regression() {
    // POSITIVE CONTROL. The test above passes by finding nothing, which is
    // exactly the shape a broken scanner also has. So un-quote the ids in a
    // real emitted document — the precise regression a future `#[serde]`
    // slip would reintroduce — and require the scanner to name them.
    let world = world();
    let (session, _) = Session::start(&world, &opts()).unwrap();
    let snap = session.snapshot().unwrap();
    let json = snapshot_json(&snap);

    let ids: Vec<u64> = snap
        .sensed
        .present
        .iter()
        .map(|p| p.entity)
        .chain(snap.social.iter().map(|entry| entry.entity))
        .filter(|id| *id > MAX_SAFE_INTEGER)
        .collect();
    assert!(
        !ids.is_empty(),
        "VACUOUS TEST GUARD: this world emits no entity id above 2^53, so \
         un-quoting them proves nothing about the scanner"
    );

    let mut regressed = json.clone();
    for id in &ids {
        regressed = regressed.replace(&format!(r#""entity":"{id}""#), &format!(r#""entity":{id}"#));
    }
    assert_ne!(
        regressed, json,
        "the mutation must actually change the bytes"
    );

    let wide = javascript_unsafe_integers(&regressed);
    assert!(
        !wide.is_empty(),
        "the scanner found nothing in a document that deliberately carries \
         bare 64-bit ids, so it cannot fail and proves nothing"
    );
    for id in &ids {
        assert!(
            wide.iter()
                .any(|(k, v)| k == "entity" && v == &id.to_string()),
            "the scanner missed the bare id {id} it was mutated to carry: {wide:?}"
        );
    }
}

#[test]
fn entity_ids_are_emitted_as_decimal_strings() {
    // Named fields rather than a generic sweep, so a regression says WHICH
    // channel lost its encoding. `sensed.present` and `social` are the only
    // two `EntityId`-typed fields on this wire; `self.agent` is an `AgentId`
    // and has carried this encoding since The Snapshot.
    let world = world();
    let (session, _) = Session::start(&world, &opts()).unwrap();
    let snap = session.snapshot().unwrap();
    let json = snapshot_json(&snap);

    assert!(
        !snap.sensed.present.is_empty() || !snap.social.is_empty(),
        "VACUOUS TEST GUARD: no entity is emitted at all, so nothing below \
         asserts anything"
    );
    for id in snap
        .sensed
        .present
        .iter()
        .map(|p| p.entity)
        .chain(snap.social.iter().map(|entry| entry.entity))
    {
        assert!(
            json.contains(&format!(r#""entity":"{id}""#)),
            "entity {id} is not emitted as a decimal string: {json:.200}"
        );
    }
    assert!(
        json.contains(r#""agent":""#),
        "`self.agent`'s string encoding is the precedent `entity` copies; if \
         it has gone, the two have diverged"
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
        opts().day.as_std_days(),
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
    // species and a vertex instead of a global -10 C snowline, so cold ground is
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
            // enclosing envelope did not move with it — and, symmetrically,
            // why the envelope's own bump to `vessel/session/v2` (The Signet)
            // does not move the chart.
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
    // fine-layer position and the embedding has nothing to place.
    //
    // The Hand, Task 3: constructed directly through the test seam rather
    // than searched for (see docs/retrospectives/the-hand.md) — `bodies()[1]` is placed once
    // indoors, so it picks up a fine-layer anchor and is drawn.
    let world = world();
    let (mut session, _) = Session::start(&world, &PossessOpts::default()).unwrap();
    common::step_inside(&mut session);
    session.place_creature_at_me(session.bodies()[1].entity);
    let snap = session.snapshot().expect("a live session snapshots");
    let SpatialChannel::Chamber { plan } = &snap.spatial else {
        panic!("`enter` puts the possession inside")
    };
    let marks = plan.marks.clone();
    let extent = plan.extent;
    let you = plan.you;

    assert!(
        !marks.is_empty(),
        "the placed companion was chosen BECAUSE it draws a mark, so an empty \
         plan here means the seam placement and the snapshot disagree: present \
         = {:?}",
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
/// makes it the wrong fixture to decode a mark from, so
/// `snapshot-seed-0-chamber-occupied.json` is taken one `wait` earlier and
/// carries a real creature. It is ADDITIVE: the two older fixtures' scripts are
/// untouched, because changing one to gain a mark would have moved `turn`,
/// `day` and `narration` in a file whose whole job is to hold those still.
///
/// **And the third is taken at a DIFFERENT SEED**, which is the one thing here
/// worth stopping on. Seed 42 is the flagship, and the other two fixtures are
/// its. But seed 42's world no longer puts a creature in the chamber you walk
/// into — The Tense reseeded it — and a fixture whose whole job is to carry a
/// mark cannot be taken from a world that has none. It is named for the seed it
/// is actually taken at, because a fixture named `seed-42` holding seed 1's
/// bytes is the kind of quiet lie a golden exists to prevent.
#[test]
fn the_client_fixtures_are_current() {
    let world = world();
    let (mut session, _) = Session::start(&world, &PossessOpts::default()).unwrap();

    let walk = hornvale_vessel::snapshot_json(&session.snapshot().unwrap());
    session.handle("enter");
    let chamber = hornvale_vessel::snapshot_json(&session.snapshot().unwrap());

    // The Hand, Task 3: the occupied fixture's mark is placed directly
    // through the test seam rather than relied on as a natural property of
    // OCCUPIED_SEED (see docs/retrospectives/the-hand.md) — the possessed-body duplicate
    // this task deletes was the only thing that ever reliably drew one.
    // `clients/vessel/src/pane_plan_marks_test.ts` reads this fixture's mark
    // coordinate FROM the fixture, never pinned, precisely so a regenerated
    // mark (a different creature, a different cell) cannot break it.
    let occupied_world = common::build(OCCUPIED_SEED).expect("the fixture's seed builds");
    let (mut occupied_session, _) =
        Session::start(&occupied_world, &PossessOpts::default()).unwrap();
    common::step_inside(&mut occupied_session);
    occupied_session.place_creature_at_me(occupied_session.bodies()[1].entity);
    assert!(
        !common::marks_of(&occupied_session).is_empty(),
        "seed {OCCUPIED_SEED} has no cell the entered chamber's own shadowcast \
         lights, so `snapshot-seed-{OCCUPIED_SEED}-chamber-occupied.json` cannot \
         carry the mark it exists to carry even through the test seam. Re-point \
         OCCUPIED_SEED at a seed whose opening chamber has a lit cell, rename \
         the fixture to match, and update \
         clients/vessel/src/pane_plan_marks_test.ts's file reference."
    );
    let occupied = hornvale_vessel::snapshot_json(&occupied_session.snapshot().unwrap());

    for (name, body) in [
        ("snapshot-seed-42-walk.json".to_string(), walk),
        ("snapshot-seed-42-chamber.json".to_string(), chamber),
        // Named from the constant, so the file on disk and the seed it was
        // taken at cannot drift apart in a later re-point.
        (
            format!("snapshot-seed-{OCCUPIED_SEED}-chamber-occupied.json"),
            occupied,
        ),
    ] {
        let path = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("tests/fixtures")
            .join(&name);
        if std::env::var("REBASELINE").is_ok() {
            std::fs::write(&path, &body).expect("the fixture directory exists");
            continue;
        }
        let committed = std::fs::read_to_string(&path)
            .unwrap_or_else(|_| panic!("{name} is missing — run with REBASELINE=1"));
        assert_eq!(
            committed, body,
            "{name} drifted: the vessel/session/v2 wire shape moved. Decide \
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

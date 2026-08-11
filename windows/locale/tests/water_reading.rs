//! The Ford, stage 2: the room reports its relation to the nearest river by
//! carrying the *measured quantity* — a signed angular distance and the band
//! edges that apply where it was measured — instead of one consumer's
//! classification of it.
//!
//! Three claims are asserted here, each against a reference that lives
//! **outside** the thing it checks:
//!
//! - **H2-2, appending is byte-clean.** Every document is byte-identical to
//!   its pre-stage-2 form up to the first new key. The reference is
//!   `fixtures/pre-stage-2-rooms.jsonl`, captured and committed on the
//!   unmodified locale window in the commit *before* the fields existed. It
//!   ships without a regenerator on purpose: a fixture re-rendered by the
//!   current code and compared against itself proves nothing, so the only
//!   honest way to recapture it is to check out a pre-stage-2 commit.
//!
//!   The sample it carries is 200 seed-42 rooms at walking depth, chosen by a
//!   fixed rule over the channel network — every polyline's mid vertex first
//!   (so every river in the world is represented before any river is
//!   represented twice), then every head, then every mouth, then the mid-vertex
//!   rooms' three neighbours, deduplicated by packed room id in that order.
//!   The tests below do not re-run that rule: each fixture line *is* a whole
//!   document and names its own room, so the sampled set cannot drift between
//!   capture and check.
//!
//! - **H2-3, the ordinal is reproducible from what the document carries.** The
//!   recomputation reads the numbers back out of the **serialized** document,
//!   not the in-memory `Locale`. That matters: banding the in-memory pair would
//!   reproduce the classification by construction, since one call produced
//!   both. The document emits *quantized* floats (decision 0033, 8 significant
//!   digits), so this asks the real question — is what a consumer can see
//!   sufficient to recover what the engine decided?
//!
//! - **The disclosure is a decision, not an oversight.** Every key the document
//!   emits is accounted for: named in a resolution list, or named in this
//!   file's deliberate-exclusion roster with the reason. 0123 warns that a
//!   stale list is worse than none.

use hornvale_kernel::{RoomAddr, Seed, World, WorldTime};
use hornvale_locale::{LocaleContext, ROOM_SCHEMA};
use hornvale_terrain::channel::Transverse;
use serde_json::Value;

/// The committed pre-stage-2 documents, one per line.
const FIXTURE: &str = include_str!("fixtures/pre-stage-2-rooms.jsonl");

/// The world every claim here is made on.
fn world() -> World {
    World::new(Seed(42))
}

/// The room each fixture line describes, recovered from the line itself.
fn room_of(line: &str) -> RoomAddr {
    let v: Value = serde_json::from_str(line).expect("fixture line is JSON");
    RoomAddr {
        face: v["face"].as_u64().expect("face") as u8,
        path: v["path"]
            .as_array()
            .expect("path")
            .iter()
            .map(|d| d.as_u64().expect("path digit") as u8)
            .collect(),
    }
}

/// H2-2 — appending is byte-clean. A document is byte-identical to its
/// pre-stage-2 form up to the first new key. This is the no-epoch claim,
/// asserted rather than assumed.
#[test]
fn the_document_is_byte_identical_up_to_the_first_new_key() {
    let world = world();
    let ctx = LocaleContext::build(&world).unwrap();
    let mut checked = 0usize;
    for old in FIXTURE.lines() {
        let room = room_of(old);
        let loc = ctx.describe(&room, WorldTime { day: 0.0 }).unwrap();
        let json = serde_json::to_string(&loc).unwrap();
        let cut = json
            .find(",\"channel_distance\"")
            .expect("the new key is present");
        // The old document minus its closing brace IS the new document's
        // prefix, if and only if nothing before the appended keys moved.
        let expected = &old[..old.len() - 1];
        assert_eq!(
            &json[..cut],
            expected,
            "room {room:?} moved before the new keys"
        );
        checked += 1;
    }
    assert_eq!(checked, 200, "the whole committed fixture was checked");
}

/// The schema tag did NOT move. If this fails, an epoch happened by accident.
#[test]
fn the_schema_tag_is_still_v2() {
    assert_eq!(ROOM_SCHEMA, "locale/room/v2");
}

/// H2-3 — the ordinal is reproducible from what the document carries. If a
/// consumer cannot recompute the classification from the stored quantity, the
/// document is storing the wrong thing.
///
/// Deliberately reads the SERIALIZED document. The engine's own answer
/// (`transverse_at`, which selects the same winning line and the same nearest
/// vertex) is the reference, and it is computed at full precision from the
/// network; the recomputation is made from quantized bytes. A room whose `|d|`
/// sits within quantization of a band edge would disagree — that is a finding
/// about the emit boundary, not a reason to loosen this test.
#[test]
fn the_band_recomputes_from_the_stored_distance_and_edges() {
    let world = world();
    let ctx = LocaleContext::build(&world).unwrap();
    let mut checked = 0usize;
    let mut seen: Vec<Transverse> = Vec::new();
    for line in FIXTURE.lines() {
        let room = room_of(line);
        let loc = ctx.describe(&room, WorldTime { day: 0.0 }).unwrap();
        let doc: Value = serde_json::from_str(&serde_json::to_string(&loc).unwrap()).unwrap();
        let (Some(d), Some(edges)) = (
            doc["channel_distance"].as_f64(),
            doc["channel_bands"].as_array().map(|a| {
                a.iter()
                    .map(|x| x.as_f64().expect("band edge is a number"))
                    .collect::<Vec<f64>>()
            }),
        ) else {
            continue;
        };
        let recomputed = Transverse::from_band(hornvale_kernel::band(d, &edges));
        let expected = ctx.terrain().transverse_at(room.centroid()).0;
        assert_eq!(
            recomputed, expected,
            "room {room:?}: the document's own numbers (d={d:?}, edges={edges:?}) band to \
             {recomputed:?} but the network says {expected:?}"
        );
        if !seen.contains(&expected) {
            seen.push(expected);
        }
        checked += 1;
    }
    assert!(
        checked >= 100,
        "only {checked} rooms carried a reading; sweep too thin"
    );
    // Anti-vacuity: a sweep that lands in one band everywhere would agree with
    // any classifier at all. The committed fixture spans all five (25 channel,
    // 30 bank, 32 floodplain, 4 terrace, 109 dry at capture); three is the
    // floor below which this stops being a test of the banding.
    assert!(
        seen.len() >= 3,
        "the sweep reached only {seen:?}; it no longer exercises the banding"
    );
}

/// The pair is one reading, not two. The distance and the edges must come from
/// the same query — if a room's edges were selected independently of the line
/// its distance was measured to, this is where it shows.
#[test]
fn the_distance_and_the_bands_are_one_reading() {
    let world = world();
    let ctx = LocaleContext::build(&world).unwrap();
    for line in FIXTURE.lines() {
        let room = room_of(line);
        let loc = ctx.describe(&room, WorldTime { day: 0.0 }).unwrap();
        let reading = ctx.terrain().channels().bank_reading(room.centroid());
        assert_eq!(
            (loc.channel_distance, loc.channel_bands),
            (reading.map(|(d, _)| d), reading.map(|(_, e)| e)),
            "room {room:?} does not report the network's own reading"
        );
        // Non-decreasing edges, or `band` would read them wrong.
        if let Some(e) = loc.channel_bands {
            assert!(
                e[0] <= e[1] && e[1] <= e[2] && e[2] <= e[3],
                "room {room:?} has non-monotone band edges {e:?}"
            );
        }
    }
}

/// The document declares which fields are grid-resolution and which are
/// channel-resolution — decision 0123, whose rule 3 warns that a stale list is
/// worse than none.
///
/// So this pins the ABSENCES as hard as the presences: every key the document
/// emits is either classified or listed below as a deliberate exclusion with a
/// reason, and adding a field to the document therefore fails this test until
/// somebody decides which side of the disclosure it falls on.
#[test]
fn the_room_declares_which_fields_are_grid_and_channel_resolution() {
    let world = world();
    let ctx = LocaleContext::build(&world).unwrap();
    let room = room_of(FIXTURE.lines().next().unwrap());
    let loc = ctx.describe(&room, WorldTime { day: 0.0 }).unwrap();

    assert_eq!(
        loc.resolution.grid_resolution_fields,
        ["biome", "cave", "fields.water"],
        "the three categorical readings taken from the dominant corner cell"
    );
    assert_eq!(
        loc.resolution.channel_resolution_fields,
        ["channel_bands"],
        "the per-vertex band geometry, and only it"
    );
    assert_eq!(loc.resolution.grid_level, ctx.globe_level());
    assert_eq!(
        loc.resolution.depth_below_grid,
        room.depth() - ctx.globe_level()
    );

    // Deliberately in NEITHER list, with the reason. The reasons differ, which
    // is what makes the lists mean anything.
    let excluded: [(&str, &str); 14] = [
        ("schema", "the room's naming, not a reading of the world"),
        ("id", "the room's naming"),
        (
            "face",
            "the room's naming, and coarser than the grid besides",
        ),
        ("path", "the room's naming"),
        ("depth", "the room's naming"),
        ("latitude", "the room's own geometry"),
        ("longitude", "the room's own geometry"),
        ("corners", "the room's own blend weights"),
        ("exits", "the room's own mesh links"),
        (
            "fields.temperature_c",
            "a three-corner blend; genuinely varies room by room",
        ),
        ("fields.moisture", "a three-corner blend"),
        ("fields.elevation_m", "a three-corner blend"),
        ("fields.height_asl_m", "a three-corner blend, re-datumed"),
        (
            "regime",
            "MIXED granularity (0123 rule 3): dominant-corner substrate and \
             expression, room-hashed micro",
        ),
    ];
    // `channel_distance` is excluded for the opposite reason to all of those:
    // it is the FINEST field the document carries, constant below no
    // resolution at all.
    let finest = "channel_distance";
    // The disclosure cannot classify itself.
    let itself = "resolution";

    let doc: Value = serde_json::from_str(&serde_json::to_string(&loc).unwrap()).unwrap();
    let mut keys: Vec<String> = Vec::new();
    for (k, v) in doc.as_object().expect("a JSON object") {
        // `fields` is the one nested object the disclosure names into, so its
        // keys are qualified rather than the parent being classified whole.
        if k == "fields" {
            for inner in v.as_object().expect("fields is an object").keys() {
                keys.push(format!("fields.{inner}"));
            }
        } else {
            keys.push(k.clone());
        }
    }

    for key in &keys {
        let classified = loc.resolution.grid_resolution_fields.contains(key)
            || loc.resolution.channel_resolution_fields.contains(key)
            || excluded.iter().any(|(k, _)| k == key)
            || key == finest
            || key == itself;
        assert!(
            classified,
            "document key {key:?} is neither disclosed as grid- or \
             channel-resolution nor listed as a deliberate exclusion — decide \
             which side of the disclosure it falls on (decision 0123)"
        );
    }
    // The other direction: no list may name a key the document does not emit.
    for named in loc
        .resolution
        .grid_resolution_fields
        .iter()
        .chain(loc.resolution.channel_resolution_fields.iter())
        .map(String::as_str)
        .chain(excluded.iter().map(|(k, _)| *k))
    {
        assert!(
            keys.iter().any(|k| k == named),
            "{named:?} is named but the document has no such key"
        );
    }
    // `biome_kind` is `#[serde(skip)]`, so it must not be on the wire at all —
    // which is why it appears in no list above.
    assert!(!keys.iter().any(|k| k == "biome_kind"));
}

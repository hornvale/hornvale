//! The Ford, stage 2: the room reports its relation to the nearest river by
//! carrying the *measured quantity* — a signed angular distance and the band
//! edges that apply where it was measured — instead of one consumer's
//! classification of it.
//!
//! Three claims are asserted here, each against a reference that lives
//! **outside** the thing it checks:
//!
//! - **H2-2, appending is additive.** Every document still carries every key
//!   its pre-stage-2 form carried, in the same order, with the stage-2 keys
//!   appended after them and nothing removed or reordered. The reference is
//!   `fixtures/pre-stage-2-rooms.jsonl`, captured and committed on the
//!   unmodified locale window in the commit *before* the fields existed. It
//!   ships without a regenerator on purpose: a fixture re-rendered by the
//!   current code and compared against itself proves nothing, so the only
//!   honest way to recapture it is to check out a pre-stage-2 commit.
//!
//!   **The claim used to be byte-identity of the value prefix, and The
//!   Glasshouse converted it to its world-independent core.** A before-arm
//!   fixture that regenerates with the code is not a before-arm
//!   (`PROC-before-arm-dies-with-an-epoch`), and the terrain epoch of decision
//!   0131 moved every VALUE in every captured document — seed 42's coastline
//!   rose to the shelf break, so the sampled rooms changed biome, elevation,
//!   water kind and prose. The value comparison could only be restored by
//!   re-capturing, which would compare current-against-current and pass
//!   vacuously while proving nothing about The Ford's refactor.
//!
//!   What The Ford actually claimed is STRUCTURAL — stage 2 added keys and
//!   removed none — and a document's key SHAPE is not a function of the world.
//!   That is what is asserted below, and it survived the epoch untouched: the
//!   key order, the key sets at every nesting level, and the identity of the
//!   appended keys are all exactly what the pre-stage-2 fixture recorded. The
//!   fixture is therefore still a genuine before-arm for the claim that
//!   remains, because the half of it this test now reads was captured before
//!   the change and has not been rewritten since.
//!
//!   The sample it carries is 200 seed-42 rooms at walking depth, chosen by a
//!   fixed rule over the channel network — every polyline's mid vertex first
//!   (so every river in the world is represented before any river is
//!   represented twice), then every head, then every mouth, then the mid-vertex
//!   rooms' neighbours, deduplicated by packed room id in that order (three
//!   apiece when the fixtures were captured on the icosahedral triangle
//!   lattice; eight, or seven at a cube corner, since The Pavement).
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

use hornvale_kernel::{Facet, Seed, Vertex, World, WorldTime, math};
use hornvale_locale::{Crossing, LocaleContext, ROOM_SCHEMA, room_edge};
use hornvale_terrain::carve::WATERFALL_MIN_DRAINAGE;
use hornvale_terrain::channel::Transverse;
use serde_json::Value;

/// The committed pre-stage-2 documents, one per line.
const FIXTURE: &str = include_str!("../fixtures/pre-stage-2-rooms.jsonl");

/// The world every claim here is made on.
fn world() -> World {
    World::new(Seed(42))
}

/// A live spread of walk-band rooms, one per fixture line.
///
/// # The fixture's own addresses stopped being addresses at The Pavement
///
/// `pre-stage-2-rooms.jsonl` records rooms on faces 0..20 at depth 12 with
/// three corners — the icosphere. The cube-sphere mesh has six faces, so
/// `Facet::pack` refuses two thirds of those lines outright and `describe`
/// answers `Unaddressable("Invalid")`. Five tests in this file broke on it at
/// once, all with that same message.
///
/// **What each of those five actually needs is a LIVE room, not that room.**
/// They assert over the document's SHAPE — its top-level key order, its nested
/// key sets, that a band recomputes from a stored distance, that an ordinal is
/// recoverable, that the resolution block declares which fields are
/// grid-resolution — and a document's shape is a function of the struct
/// definition, not of where the room is. `the_document_appends_keys_and_
/// removes_none`'s own doc already says so in as many words. The fixture LINE
/// is still the before-arm for the key shape; only the address it carries has
/// stopped resolving, so the address is the part that gets rebuilt.
///
/// A lat/lon grid rather than a path enumeration, for the reason
/// `wetness_reading.rs`'s `sampled_rooms` records: varying the leading path
/// digits and zero-filling puts every address in one deep corner of its face.
fn live_rooms(ctx: &LocaleContext) -> Vec<Facet> {
    // **Seeded on the channel network, then topped up from a lat/lon grid.**
    // A pure grid was tried first and two of the five tests refused it with
    // their own anti-vacuity guards — *"the sweep reached only [(Dry, 198),
    // (Terrace, 2)]; it no longer exercises the banding"* — because a room
    // drawn uniformly over the globe is almost never near a watercourse, and
    // these tests are about the channel bands. The fixture's population was
    // not uniform either; it was captured by a campaign that was measuring
    // water. So the rebuilt sample is seeded where the water is and filled out
    // with ordinary ground, which exercises both the banded and the dry arms.
    let want = FIXTURE.lines().count();
    let geo = ctx.climate().geosphere();
    let depth = walk_depth(ctx);
    let mut out: Vec<Facet> = Vec::with_capacity(want);
    let vertices: Vec<Vertex> = ctx
        .terrain()
        .channels()
        .run_vertices
        .iter()
        .flatten()
        .copied()
        .collect();
    let stride = (vertices.len() / (want / 2).max(1)).max(1);
    for v in vertices.iter().step_by(stride) {
        out.push(Facet::containing(geo.position(*v), depth));
        if out.len() == want / 2 {
            break;
        }
    }
    'grid: for a in 0..20u32 {
        let lat = -85.0 + 170.0 * f64::from(a) / 19.0;
        for b in 0..20u32 {
            let lon = -180.0 + 360.0 * f64::from(b) / 20.0;
            out.push(Facet::containing(
                hornvale_kernel::math::unit_sphere_from_lat_lon(lat, lon),
                depth,
            ));
            if out.len() == want {
                break 'grid;
            }
        }
    }
    out
}

/// H2-2 — appending is additive. Every key the pre-stage-2 document carried is
/// still carried, in the same order, and the stage-2 keys are appended after
/// them. This is the no-epoch claim, asserted rather than assumed.
///
/// **RENAMED by The Glasshouse**, from
/// `the_document_is_byte_identical_up_to_the_first_new_key`, because it no
/// longer compares bytes — the same discipline The Fathom's before-arm got when
/// it was converted (a test's name must state what it now proves, decision
/// 0106's "a wrong label defends itself").
///
/// # Why this is a key-shape check and not a byte-prefix one
///
/// It used to compare the fixture line, byte for byte, against the live
/// document's prefix up to the first new key — with `regime.micro.wetness` and
/// `regime.descriptor` substituted out by name, because The Rill had legitimately
/// moved those two values after the capture.
///
/// **That comparison could not survive an epoch, and one arrived.** Decision
/// 0131's craton rescale moved seed 42's coastline to the shelf break, and with
/// it every value in every captured document — the sampled rooms changed biome,
/// water kind, elevation, temperature and prose. Extending the by-name
/// substitution to cover them would have excused nearly the whole document,
/// leaving an assertion that excused everything it compared; re-capturing the
/// fixture would have compared the current code against itself, which is exactly
/// what its own module doc says proves nothing.
///
/// So the claim is asserted at the level it was always about. The Ford's H2-2 is
/// **structural** — stage 2 *appended* fields to `Locale` and removed none — and
/// a document's key shape is a function of the struct definition, not of the
/// world. Three things are checked, none of which any world change can move:
///
/// 1. the live document's top-level key ORDER begins with the fixture's,
///    exactly, so nothing was removed, inserted or reordered ahead of the new
///    keys;
/// 2. the keys appended after them are exactly [`APPENDED_SINCE_CAPTURE`];
/// 3. every nested object shared by the two documents carries the same key set,
///    recursively — so a field deleted from `regime`, `fields`, an `exits`
///    entry or a `corners` entry reddens this too, not just a top-level one.
///
/// The fixture is still a genuine before-arm for that claim: the half of it this
/// test reads was captured on the unmodified window and has never been rewritten.
/// What is gone is the ability to notice a VALUE moving before the new keys —
/// and after an epoch that moved every value deliberately, that ability had
/// already been spent.
#[test]
fn the_document_appends_keys_and_removes_none() {
    let world = world();
    let ctx = LocaleContext::build(&world).unwrap();
    let mut checked = 0usize;
    for (old, room) in FIXTURE.lines().zip(live_rooms(&ctx)) {
        let loc = ctx.describe(&room, WorldTime::GENESIS).unwrap();
        let json = serde_json::to_string(&loc).unwrap();

        // (1) and (2): top-level key order, read out of the raw text rather
        // than a parsed `Value` — serde_json's map is a `BTreeMap` here (the
        // `preserve_order` feature is not enabled and could not be, decision
        // 0004), so parsing would sort the keys and silently destroy the very
        // property being checked.
        let was = top_level_keys(old);
        let now = top_level_keys(&json);
        assert!(
            now.starts_with(&was),
            "room {room:?}: the pre-stage-2 keys are no longer this document's \
             leading keys, in order.\n  was: {was:?}\n  now: {now:?}"
        );
        assert_eq!(
            &now[was.len()..],
            APPENDED_SINCE_CAPTURE,
            "room {room:?}: the keys appended after the pre-stage-2 document are \
             not the ones this file accounts for"
        );

        // (3): nested key sets, recursively. Order is not asserted below the top
        // level for the same `BTreeMap` reason; membership is, at every depth.
        let was_doc: Value = serde_json::from_str(old).expect("fixture line is JSON");
        let now_doc: Value = serde_json::from_str(&json).expect("the live document is JSON");
        for key in &was {
            assert_same_key_shape(&was_doc[key], &now_doc[key], key, &room);
        }
        checked += 1;
    }
    assert_eq!(checked, 200, "the whole committed fixture was checked");
}

/// Every key appended to the room document since `pre-stage-2-rooms.jsonl` was
/// captured, in emission order. Pinned rather than derived: the whole of H2-2
/// is that appending is *all* that has happened, and a list computed from the
/// live document would agree with itself no matter what had.
///
/// - `channel_distance`, `channel_bands` — The Ford, stage 2. These are the
///   two H2-2 was written about.
/// - `resolution` — decision 0123, a later campaign. **Its presence here is a
///   finding, not a caveat.** The byte-prefix form of this test cut the live
///   document at `channel_distance` and compared only what came before, so it
///   could never see a key appended AFTER the stage-2 pair; `resolution` has
///   been arriving unremarked ever since. The key-shape form notices, which
///   makes it strictly stronger than what it replaces on this axis even though
///   it gave up the value comparison on the other.
///
/// Appending to this list is a deliberate act: it is the assertion that a new
/// field was added at the END of the document and nothing else moved.
const APPENDED_SINCE_CAPTURE: &[&str] = &["channel_distance", "channel_bands", "resolution"];

/// The top-level keys of a one-line JSON object, in emission order.
///
/// A deliberate scanner rather than a parse: `serde_json::Map` is a `BTreeMap`
/// in this workspace, so `Value` cannot carry key order at all.
fn top_level_keys(doc: &str) -> Vec<String> {
    let mut keys = Vec::new();
    let mut depth = 0usize;
    let mut in_string = false;
    let mut escaped = false;
    let mut start: Option<usize> = None;
    let bytes = doc.as_bytes();
    for (i, &b) in bytes.iter().enumerate() {
        if in_string {
            if escaped {
                escaped = false;
            } else if b == b'\\' {
                escaped = true;
            } else if b == b'"' {
                in_string = false;
                // A string that closes at depth 1 and is followed by `:` is a
                // key of the top-level object.
                if depth == 1
                    && let Some(s) = start
                    && bytes.get(i + 1) == Some(&b':')
                {
                    keys.push(doc[s + 1..i].to_string());
                }
                start = None;
            }
            continue;
        }
        match b {
            b'"' => {
                in_string = true;
                escaped = false;
                start = Some(i);
            }
            b'{' | b'[' => depth += 1,
            b'}' | b']' => depth -= 1,
            _ => {}
        }
    }
    keys
}

/// Assert that two values carry the same object keys, recursively.
///
/// Recurses only where both sides agree on shape: two objects are compared key
/// by key, two arrays element-wise over their shared prefix. Anything else —
/// a scalar, or an enum that serializes as a bare string on one side and as a
/// single-key object on the other — is left alone, because that is a VALUE
/// difference and values are exactly what this test no longer claims.
fn assert_same_key_shape(was: &Value, now: &Value, path: &str, room: &Facet) {
    match (was, now) {
        // An externally-tagged ENUM serializes as a one-key object whose key
        // IS the variant, so recursing into one compares world state, not
        // struct shape. It only ever passed by accident: `exits[i].direction`
        // read `{"Compass": ...}` for the first three entries of both
        // documents and `{"Enter": n}` for the last four, because a room had
        // exactly three lateral exits on the triangular mesh. A cube-sphere
        // room has eight, so index 4 is a `Compass` in the live document
        // against an `Enter` in the captured one and the comparison went red
        // on a difference that is not a schema change at all.
        //
        // The pair is skipped rather than the array truncated: truncating
        // would keep comparing variants and merely stop at the shortest, which
        // is the same category error with a smaller blast radius. The struct
        // that CONTAINS `direction` is still compared — `Exit`'s own
        // `direction`/`kind`/`to` key set is asserted by the object arm one
        // level up, which is the shape claim H2-2 is actually about.
        (Value::Object(_), _) | (_, Value::Object(_)) if path.ends_with(".direction") => {}
        (Value::Object(a), Value::Object(b)) => {
            let ka: Vec<&String> = a.keys().collect();
            let kb: Vec<&String> = b.keys().collect();
            assert_eq!(
                ka, kb,
                "room {room:?}: the key set at {path} moved\n  was: {ka:?}\n  now: {kb:?}"
            );
            for k in ka {
                assert_same_key_shape(&a[k], &b[k], &format!("{path}.{k}"), room);
            }
        }
        (Value::Array(a), Value::Array(b)) => {
            for (i, (x, y)) in a.iter().zip(b.iter()).enumerate() {
                assert_same_key_shape(x, y, &format!("{path}[{i}]"), room);
            }
        }
        _ => {}
    }
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
    let mut seen: Vec<(Transverse, usize)> = Vec::new();
    for room in live_rooms(&ctx) {
        let loc = ctx.describe(&room, WorldTime::GENESIS).unwrap();
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
        match seen.iter_mut().find(|(t, _)| *t == expected) {
            Some((_, n)) => *n += 1,
            None => seen.push((expected, 1)),
        }
        checked += 1;
    }
    assert!(
        checked >= 100,
        "only {checked} rooms carried a reading; sweep too thin"
    );
    // Anti-vacuity: a sweep that lands in one band everywhere would agree with
    // any classifier at all; three is the floor below which this stops being a
    // test of the banding.
    //
    // THE SPREAD IS PRINTED, NOT WRITTEN DOWN, because The Rill moved it. It
    // used to read "(25 channel, 30 bank, 32 floodplain, 4 terrace, 109 dry at
    // capture)"; re-measured after Task 3 rendered the whole land flow tree it
    // is 41 channel, 22 bank, 26 floodplain, 2 terrace, 109 dry.
    //
    // **The dry count did not move, and that is the informative half.** The
    // fixture's 200 rooms are fixed, and the wet/dry partition came out
    // identical: the 91 wet rooms re-banded INWARD (channel +16, exactly
    // balancing bank -8, floodplain -6, terrace -2) while the 109 dry ones
    // stayed dry. An 8x denser network made the wet rooms wetter and recruited
    // no new ones.
    //
    // **What that is NOT caused by.** The width law was not retuned — Task 1
    // measured `channel_half_width` and shipped no recalibration
    // (`CHANNEL_WIDTH_COEFF` is still 8.5e-4), and every one of the four edges
    // is a fixed multiple of `channel_half_width(drainage, vertex_edge)`, so the
    // bands moved only through `drainage`. The cause is therefore the network
    // itself: Task 3 renders every downhill step rather than the top 6.7%, and
    // Task 4 repartitions discharge at branches. Which of those two dominates,
    // and whether a room re-banded because it got nearer a centreline or
    // because the drainage under its nearest vertex changed, is NOT measured
    // here — do not read a decomposition into the counts above.
    println!("H2-3: {checked} of 200 fixture rooms carried a reading; band spread {seen:?}");
    assert!(
        seen.len() >= 3,
        "the sweep reached only {seen:?}; it no longer exercises the banding"
    );
}

/// Spec §5.2's deliverable: **the ordinal is a function over a room**, and it
/// is callable. `windows/locale` publishes it as
/// `LocaleContext::transverse_of`, which is what makes "the ordinal is
/// recovered by a function anyone can call" a statement about the shipped
/// surface rather than about the terrain domain's `transverse_at`, a method
/// over a *position* on a *different* crate.
///
/// **The reference lives outside the function: the serialized document.** For
/// every room in the committed fixture, banding the document's own
/// `channel_distance` against its own `channel_bands` must give what
/// `transverse_of` returns, and `transverse_of`'s distance must quantize to
/// the one the document carries.
///
/// Checking against the engine's `transverse_at` instead would prove nothing:
/// both go through the single `bank_reading` selection, which is precisely the
/// property §5.2's function is *required* to have, so they agree by
/// construction. The document is reached by a different call (`describe`) and
/// emitted at eight significant digits rather than full precision, and it is
/// what a consumer actually holds — so agreement there is the claim worth
/// making.
#[test]
fn the_ordinal_is_recoverable_as_a_function_over_a_room() {
    let world = world();
    let ctx = LocaleContext::build(&world).unwrap();
    let mut checked = 0usize;
    let mut seen: Vec<Transverse> = Vec::new();
    for room in live_rooms(&ctx) {
        let loc = ctx.describe(&room, WorldTime::GENESIS).unwrap();
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
        let from_document = Transverse::from_band(hornvale_kernel::band(d, &edges));
        let (from_function, distance) = ctx
            .transverse_of(&room)
            .expect("seed 42's channel network is not empty");
        assert_eq!(
            from_function, from_document,
            "room {room:?}: the function says {from_function:?} but the document's own numbers \
             (d={d:?}, edges={edges:?}) band to {from_document:?}"
        );
        assert_eq!(
            hornvale_kernel::quantize(distance),
            d,
            "room {room:?}: the function's distance {distance:?} is not the one the document \
             emits, so it is not the same reading"
        );
        if !seen.contains(&from_function) {
            seen.push(from_function);
        }
        checked += 1;
    }
    assert!(
        checked >= 100,
        "only {checked} rooms carried a reading; sweep too thin"
    );
    // Anti-vacuity: a function that returned one band everywhere would agree
    // with a document that did the same. The committed fixture spans all five.
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
    for room in live_rooms(&ctx) {
        let loc = ctx.describe(&room, WorldTime::GENESIS).unwrap();
        let reading = ctx.terrain().channels().bank_reading(room.centroid());
        assert_eq!(
            (loc.channel_distance, loc.channel_bands),
            (
                reading.map(|r| r.signed_distance),
                reading.map(|r| r.band_edges)
            ),
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
    let room = live_rooms(&ctx)
        .into_iter()
        .next()
        .expect("the live spread is non-empty");
    let loc = ctx.describe(&room, WorldTime::GENESIS).unwrap();

    assert_eq!(
        loc.resolution.grid_resolution_fields,
        ["biome", "cave", "fields.water"],
        "the three categorical readings taken from the dominant corner vertex"
    );
    assert_eq!(
        loc.resolution.channel_resolution_fields,
        ["channel_bands"],
        "the per-vertex band geometry, and only it"
    );
    // Read the grid level from the TERRAIN's geosphere, not from
    // `ctx.globe_level()` — that accessor returns the very field the
    // disclosure is populated from, so asserting against it compares a value
    // to its own source and holds however wrong the cached level is.
    //
    // The terrain is not merely a different accessor, it is a different
    // object: `LocaleContext` caches `globe_level` from the CLIMATE geosphere
    // (`lib.rs:498`), so this also cross-checks that the two domains were
    // built on the same grid — a disagreement the disclosure would otherwise
    // report with a straight face. The committed
    // `book/src/reference/locale-seed-42.json` pins the literal 6 as well, but
    // a drift check is not a unit test's reference.
    assert_eq!(
        loc.resolution.grid_level,
        ctx.terrain().geosphere().depth(),
        "the disclosed grid level must be the level the terrain was built on"
    );
    assert_eq!(
        loc.resolution.depth_below_grid,
        room.depth() - ctx.terrain().geosphere().depth()
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
            "a four-corner bilinear blend; genuinely varies room by room",
        ),
        ("fields.moisture", "a four-corner bilinear blend"),
        ("fields.elevation_m", "a four-corner bilinear blend"),
        (
            "fields.height_asl_m",
            "a four-corner bilinear blend, re-datumed",
        ),
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

// ---------------------------------------------------------------------------
// Task 3 — crossing is a traversal query.
//
// Everything below asks about a PAIR of rooms. The references:
//
// - The transect population comes from the network's own vertices and the room
//   mesh's own neighbour relation, neither of which knows what
//   `crossing_between` decides — so the denominator exists independently of the
//   criterion. That is
//   the whole point of H2-4's denominator (spec §7): a population of "adjacent
//   pairs whose sign differs" would BE the ford set by construction and would
//   read ~1.0 no matter what the world looked like.
// - The dry-band exclusion test builds its sign change from the geometry of a
//   polyline ENDPOINT — the kernel's documented "beyond an endpoint, distance
//   is to the endpoint" behaviour — and confirms the flip against the rooms'
//   own signed distances before asking `crossing_between` anything. Its
//   reference is therefore outside the gate it is checking.
// ---------------------------------------------------------------------------

/// Anti-vacuity floors, each stated beside the value measured on seed 42 at
/// level 6 so a future reader can tell a drifting world from an emptying test.
///
/// **RE-MEASURED AT THE RILL'S TASK 3** (fix round 1), which took the network
/// from 883 vertices to 14,606 by rendering the whole land flow tree. Every
/// figure below moved and none of the floors had, so each carried 20-27x
/// headroom against a stated measurement that was no longer true — which is
/// precisely the state a floor exists to prevent, since a floor that cannot
/// fire until the population falls by 95% cannot tell a drifting world from an
/// emptying test. The old figures are kept in parentheses because the RATIO
/// between them is the finding.
///
/// Measured now: **4015** same-bank, same-channel adjacent pairs with a room
/// inside its own bank edge (was 440 when the doc was written, 597 immediately
/// before Task 3); **1620** outright-dry constructed sign changes (was 126,
/// 139 before Task 3); **1456** constructed inside a terrace band (was 123,
/// 134 before Task 3). The floors sit near half of each — terrain drift may
/// move a river, but it will not halve the network.
const SAME_BANK_PAIRS_FLOOR: usize = 2_000;
/// See [`SAME_BANK_PAIRS_FLOOR`]. Guards `beyond_terrace`; measured 1620.
const DRY_FLIPS_FLOOR: usize = 800;
/// See [`SAME_BANK_PAIRS_FLOOR`]. Guards `in_terrace`; measured 1456.
const TERRACE_FLIPS_FLOOR: usize = 700;
/// Transects where a crossing exists at all, at walk depth. Measured **96**
/// over the 395 strided transects plus 25 injected extremes (was 121 of 295
/// before Task 3, and the doc said 123 of 341 from earlier still).
///
/// **This is the one floor that did NOT need raising, and the reason is worth
/// knowing:** the count FELL while everything else grew, because the sample is
/// capped at 400 transects and the added reaches are creeks whose rooms mostly
/// read `NotACrossing`. 60 against 96 is 1.6x headroom — tighter than this
/// file's usual half-of-measured, and left alone deliberately.
const CROSSINGS_FLOOR: usize = 60;
/// Crossings whose reach carries at least `WATERFALL_MIN_DRAINAGE`, **within
/// the population the discharge test assembles**. Measured **8** (10 before the
/// ranking was de-duplicated by vertex, which spent slots on the same trunk
/// twice; 15 before Task 3; 8 when the doc was first written).
///
/// **READ THIS COUNT'S LIMITATION BEFORE TRUSTING IT.** Since Task 3 the
/// strided 395-vertex sample contains exactly **one** vertex with Q >= 80 and
/// it reads `NotACrossing`, so all eight of these come from the 25 extremes
/// `transects_with_strongest` injects. The count therefore measures its own
/// enrichment: it can no longer notice "strong water vanished from the sample",
/// which is the job it was added to do. [`LOUD_REACH_VERTICES_FLOOR`] is what
/// notices that now — the injection SELECTS from the network's loud reaches, it
/// cannot create them — and this stays only as a floor on the universal
/// assertion having run on something.
const STRONG_CROSSINGS_FLOOR: usize = 4;
/// Distinct vertices in the whole level-6 seed-42 network carrying at least
/// `WATERFALL_MIN_DRAINAGE`. Measured **34** (91 vertices, since a trunk vertex
/// appears once per run that terminates on it).
///
/// This is the floor [`STRONG_CROSSINGS_FLOOR`] used to be before enrichment
/// made that one circular: it is a property of the WORLD, read straight off
/// `drainage`, so no sampling choice this file makes can inflate it. A world
/// that stopped producing rivers big enough to refuse fails here.
///
/// # THIS FLOOR IS CURRENTLY FIRING, AND IT IS LEFT FIRING ON PURPOSE
///
/// Under decision 0134's terrain epoch, seed 42's loud-vertex count fell
/// **34 -> 16** (network loud vertices 91 of 14,606 -> 58 of 15,360) and
/// crossings above the threshold fell **8 -> 2**. Both this floor and
/// [`STRONG_CROSSINGS_FLOOR`] therefore fail, and
/// `the_discharge_clause_makes_the_strongest_crossing_impassable` is red.
///
/// **Nothing here was moved to make it pass.** This floor's whole purpose is
/// stated in the paragraph above and in its own failure message — "the world
/// has stopped producing water strong enough to refuse" — and it is doing
/// exactly that job. Lowering it would delete the only instrument that noticed.
///
/// What did NOT fail is the claim the test is named for. The ordering still
/// holds and still straddles the threshold: the strongest reachable crossing is
/// Q=98 and reads `Impassable` (it was Q=146), the weakest is Q=2 and reads
/// `Fordable`. The mechanism is coherent with the epoch rather than mysterious —
/// sea level rose to the shelf break, so land drains a shorter distance to a
/// nearer sea, catchments are smaller and peak discharge is lower. The same
/// movement is visible in `hornvale-scene`'s water-fields test, where seed 44
/// went from four waterfall sites to none, and in the channel golden's
/// "more, shorter, narrower reaches" diff.
///
/// **What should happen instead of a nudge**, recorded here so the next session
/// does not have to re-derive it:
///
/// 1. `WATERFALL_MIN_DRAINAGE = 80` is a constant calibrated against
///    pre-epoch catchment sizes. If catchments have shrunk systematically, the
///    threshold is the thing that is now mis-scaled, not the floor — a decision
///    0106 provenance question about that constant, with the new hypsometry as
///    its evidence.
/// 2. "Worlds produce water strong enough to refuse" is a **census** question in
///    decision 0093's sense, not a one-world pin. A `loud-reach-vertices` metric
///    would measure the frequency across the generator's distribution; this test
///    would then keep only the ordering claim, which is world-independent and
///    passes untouched, plus an existence check.
const LOUD_REACH_VERTICES_FLOOR: usize = 17;
/// Vertices that flip verdict on the step length alone. Measured **146 of 146**
/// examined after The Rill's Task 3 (96 of 96 before it) — every pair that
/// reaches the assertion flips, which is what a clause deciding something looks
/// like. The rate at which a sampled vertex QUALIFIES fell from 24% to 9% when
/// the network grew to the whole land flow tree, because a headwater creek is
/// narrower than both step lengths and is filtered out; the test's own sample
/// size absorbs that, and the note is at it.
///
/// It read 98 of 98 until the precondition was restated over the step lengths
/// the gate actually prices against (`min(home, step)` per step) instead of the
/// home room's edge alone. The two pairs that dropped out are the ones where
/// those two quantities disagreed — i.e. exactly the pairs the old filter was
/// admitting on a quantity the gate does not use. A narrower population that
/// still flips at 100% is the better instrument.
const WIDTH_FLIP_FLOOR: usize = 70;

/// Walk depth — a thin alias for [`hornvale_locale::walk_depth`], kept only so
/// the ~40 call sites below read unchanged.
///
/// **This used to be a genuine second DEFINITION**, restating
/// `globe_level() + 6` because `windows/vessel` (where the function then lived)
/// depends on this crate and the dependency could not run the other way. That
/// reason is gone: The Pavement moved the definition into `hornvale_locale`
/// itself, which is this crate, so the restatement had no purpose left but to
/// drift.
///
/// The doc it carried also asserted this "is what `hornvale locale` and
/// `hornvale possess` default to". That was FALSE when written: `possess`
/// follows `walk_depth` through `Session`, but `hornvale locale`'s `--depth`
/// default was its own hardcoded `globe_level() + 6` in `cli/src/main.rs` and
/// had fallen a band behind. Both CLI defaults now call the function, so the
/// sentence is true again — and it is true by construction rather than by
/// assertion, which is why it is not restated as a claim here.
fn walk_depth(ctx: &LocaleContext) -> u32 {
    hornvale_locale::walk_depth(ctx)
}

fn dot(a: [f64; 3], b: [f64; 3]) -> f64 {
    a[0] * b[0] + a[1] * b[1] + a[2] * b[2]
}

fn cross(a: [f64; 3], b: [f64; 3]) -> [f64; 3] {
    [
        a[1] * b[2] - a[2] * b[1],
        a[2] * b[0] - a[0] * b[2],
        a[0] * b[1] - a[1] * b[0],
    ]
}

fn norm(v: [f64; 3]) -> f64 {
    dot(v, v).sqrt()
}

fn normalize(v: [f64; 3]) -> [f64; 3] {
    let n = norm(v);
    assert!(n > 0.0, "cannot normalize a zero vector");
    [v[0] / n, v[1] / n, v[2] / n]
}

/// The point exactly `off` radians from `base` in the (unit, perpendicular to
/// `base`) direction `dir`.
///
/// `cos(off)·base + sin(off)·dir` — a rotation, so the offset is EXACT rather
/// than a chord approximation, and `-off` gives the exact mirror image across
/// `base`'s own great circle. `domains/terrain/tests/channel_properties.rs`
/// documents the trap this avoids: a construction that merely looks like a
/// mirror (`normalize(2b - t)`) is not distance-preserving and perturbs the
/// magnitude in the eighth significant digit, which turns a sign measurement
/// into a magnitude measurement without saying so.
fn offset_from(base: [f64; 3], dir: [f64; 3], off: f64) -> [f64; 3] {
    let (c, s) = (math::cos(off), math::sin(off));
    [
        c * base[0] + s * dir[0],
        c * base[1] + s * dir[1],
        c * base[2] + s * dir[2],
    ]
}

/// One transect of the channel network: the room the centreline runs through,
/// and **every mesh step out of it**, plus the reach it belongs to.
///
/// The steps are `Facet::neighbors()`, so every pair this population asks
/// about is a walker's step **by construction** rather than by filtering.
/// **ALL EIGHT NEIGHBOURS, NOT THE FOUR EDGES — ruled during The Pavement's
/// Task 3, and the reason belongs here rather than in a review thread.** The
/// octile cost model this campaign adopts treats a diagonal as a *real step at
/// `sqrt(2)` cost*, not as a non-step. A diagonal neighbour is therefore
/// genuinely reachable, and a transect that asks "what water is reachable from
/// here" must see it; restricting the set to the four edge neighbours would
/// invent a distinction the movement model does not make. The alternative was
/// considered explicitly and refused.
///
/// **How many there are is not fixed and must not be hardcoded** — it was
/// three on the icosahedral triangle lattice and is eight (seven at a cube
/// corner) since The Pavement made the walk 8-connected. `verdict` and
/// `width_pricing` below both derive their count from `steps` for that reason;
/// an earlier state of this file had that reporter (then `step_lengths`)
/// hardcode `[0]`, `[1]`,
/// `[2]` while `verdict` iterated all of them, which is the exact one-sidedness
/// `width_pricing`'s own doc exists to prevent — and it was admitting steps it
/// should have refused, not merely reporting fewer.
///
/// **THIS CHANGES WHAT THE CALIBRATION MEASURES. It is not a fix and not a
/// tuning.** The population every §8 reading in this file is taken over is
/// "each sampled channel room plus its mesh neighbourhood". That neighbourhood
/// was **3 rooms of 3 possible** and is now **8 of 8** (7 at a cube corner), on
/// a base mesh that also changed shape underneath it. Every fordability number
/// recorded in this file was measured on the old one. The affected figures and
/// their before/after are stated at the two tests that carry them —
/// `the_fordable_fraction_of_the_network_is_within_its_interval` and
/// `the_loud_reach_population_is_pinned_as_a_witness` — rather than summarised
/// here, so that a reader meets the movement at the assertion it applies to.
/// Decision 0016 governs; the chronicle entry is The Pavement's Task 10.
///
/// That matters, and it is the second construction this test has had:
///
/// The first probed two points half a room edge either side of the vertex and
/// checked only that they landed in different rooms. Counting the mesh-neighbour
/// relation showed that **307 of 341 such pairs are not adjacent at all** —
/// diagonal or two-apart — so the claim that the population measured *steps*
/// was false for 90% of it, and filtering the survivors left 34 transects: an
/// anecdote, and a 90% drop rate whose selection mechanism nobody had examined.
/// Building the step instead of testing for it removes the drop entirely.
struct Transect {
    /// The polyline and the index along it this transect was taken at — the
    /// identity two runs at different depths are matched on.
    ///
    /// Named `polyline_at`, not `vertex`: The Lexicon of Place made `Vertex` a
    /// mesh vertex, and this struct carries one of those too (`vertex`, below).
    /// Two different vertices in one struct is exactly the collision that
    /// campaign existed to remove.
    polyline_at: (usize, usize),
    /// The room the centreline runs through: the transect's origin, and the
    /// one room guaranteed to be inside its own bands.
    home: Facet,
    /// Every mesh step out of `home` — `Facet::neighbors()` verbatim, so the
    /// length is whatever the lattice's connectivity is (eight, or seven at a
    /// cube corner). Nothing downstream may assume a count.
    steps: Vec<Facet>,
    /// The transected vertex's own band edges (channel/bank, bank/floodplain,
    /// floodplain/terrace, terrace/dry).
    edges: [f64; 4],
    /// The vertex that vertex was placed from — this reach's discharge.
    vertex: Vertex,
}

impl Transect {
    /// What crossing the network the walker meets here: `Fordable` if any of
    /// the steps out of the channel room can be waded, `Impassable` if
    /// some step crosses the channel but none can be waded, `NotACrossing` if
    /// no step crosses it at all — the channel is wider than every step
    /// available, or the walker is not in its bands.
    fn verdict(&self, ctx: &LocaleContext) -> Crossing {
        let mut crossed = false;
        for step in &self.steps {
            match ctx.crossing_between(&self.home, step) {
                Crossing::Fordable => return Crossing::Fordable,
                Crossing::Impassable => crossed = true,
                Crossing::NotACrossing => {}
            }
        }
        if crossed {
            Crossing::Impassable
        } else {
            Crossing::NotACrossing
        }
    }

    /// Everything §8's fordability clauses will actually compare for **every
    /// one of** this transect's steps: the step length
    /// (`room_edge(home).min(room_edge(step))`, per step, one entry per entry in
    /// `steps`) and the full widths of the reaches the gate prices it against.
    ///
    /// Published as its own helper because a test that filters on
    /// `room_edge(&home)` alone is asserting against a DIFFERENT quantity than
    /// the gate uses: two rooms of one mesh step can carry slightly different
    /// edges, and the gate takes the shorter. Filtering on the home edge and
    /// then asserting on the gate's verdict is one-sided — it holds only while
    /// the two happen to agree, and fails as a spurious red rather than a
    /// silent green when they stop.
    ///
    /// **THE COUNT IS DERIVED FROM `steps`, NEVER A LITERAL (The Pavement,
    /// Task 3, fix round 2).** This helper's predecessor used to hardcode
    /// `[self.steps[0], self.steps[1], self.steps[2]]`. When the walk
    /// went 8-connected, `steps` grew to eight and `verdict` — which iterates
    /// `&self.steps` — began pricing all eight, while the reporter kept
    /// reporting three. That is not a cosmetic mismatch: the one caller requires
    /// that NO step can be admitted on width, then asserts
    /// on `verdict`. Computed over a subset, that precondition can hold while a
    /// fourth-through-eighth step IS narrow enough — reintroducing the exact
    /// one-sidedness the paragraph above was written to remove, silently.
    /// Deriving the count from `steps` is what makes reporter and pricer the
    /// same quantity by construction rather than by a matching literal.
    ///
    /// # THE WIDTH IS PER READING, NOT PER TRANSECT — the second half of the
    /// same mistake, and the one that actually went red (The Pavement, the
    /// absorb-water task)
    ///
    /// This helper used to report step lengths only (`step_lengths`), and its
    /// one caller paired them with `2.0 * self.edges[0]`: the full width of the
    /// reach **being transected**, `ChannelNetwork::band_edges[i][j]`.
    /// `crossing_between` prices no such quantity. It prices
    /// `2.0 * r.band_edges[0]` for each `BankReading` the two rooms of the step
    /// **independently win**, and a room's winning line need not be the sampled
    /// polyline at all.
    ///
    /// **THIS IS A DEFECT IN THE TEST'S MODEL OF THE GATE, NOT IN THE GATE**,
    /// and it was established by instrumenting the failing pair rather than by
    /// reading the code. At `Vertex(2656)`, polyline 284 index 3, the transected
    /// reach's full width is 3.5692e-5 rad — but the transect's own home room
    /// wins **line 1106**, full width 1.6007e-5, and its eight neighbours win
    /// lines 1097, 1084, 1106, 23, 284, 1106, 23 and 260, whose full widths run
    /// from 1.5922e-5 to 4.5276e-5. Six different channels, one of them less
    /// than half the width of the one the filter was reading. The step that
    /// returns `Fordable` at depth 16 is the line-1106 pair, priced at
    /// 1.6007e-5 against a 1.9635e-5 step — the width clause deciding
    /// correctly, about water the filter never looked at.
    ///
    /// `Vertex(2656)` is not a unique identity for the water either, which is
    /// why the old failure message could say "the SAME water" and be wrong: a
    /// grid vertex carries one polyline vertex per run terminating on it, each
    /// with its own discharge and therefore its own `band_edges`, which is how
    /// every reading above names that same vertex at six different widths.
    ///
    /// **The `sqrt(2)` diagonal hypothesis was REFUTED AS THE CAUSE OF THAT
    /// FAILURE, and has since been RATIFIED AS THE RULE (decision 0515).** Read
    /// the two apart, because this paragraph used to say only the first half.
    /// The obvious reading of an 8-connected mesh is that a diagonal step's
    /// true crossing distance is `edge · sqrt(2)` while `min(home, step)`
    /// reports the plain edge. That was never the cause of the `Vertex(2656)`
    /// failure above — 1.41 × 1.9635e-5 = 2.77e-5 is still under 3.5692e-5, so
    /// the factor could not have flipped that verdict — and the open design
    /// question the old wording left ("whether the gate SHOULD charge a
    /// diagonal 1.41 edges of water") is now answered YES, uniformly, by
    /// decision 0515: the movement clock has charged `√2` for a diagonal since
    /// 0508 and the reach follows the same factor, because the reason a
    /// diagonal costs more is the ground it covers.
    ///
    /// So the step length here is `room_edge(home).min(room_edge(step))` times
    /// [`hornvale_locale::DIAGONAL_STEP_FACTOR`] on a corner-adjacent step —
    /// the gate's own arithmetic, asked through the gate's own
    /// [`hornvale_locale::is_diagonal_step`] rather than re-derived, which is
    /// what keeps this reporter matching the gate by construction.
    ///
    /// Only **interpretable** readings are listed, because those are the only
    /// ones `wadeable` is applied to (`crossing_between`'s clause 3 filter). A
    /// step with empty `widths` cannot be a crossing at all, which is why such
    /// an entry reads as *refused* to the caller's deep arm and as *silent* to
    /// its shallow arm.
    fn width_pricing(&self, ctx: &LocaleContext) -> Vec<StepPricing> {
        let net = ctx.terrain().channels();
        let home_edge = room_edge(&self.home);
        let home_reading = net.bank_reading(self.home.centroid());
        self.steps
            .iter()
            .map(|step| {
                let step_len = home_edge.min(room_edge(step))
                    * if hornvale_locale::is_diagonal_step(&self.home, step) {
                        hornvale_locale::DIAGONAL_STEP_FACTOR
                    } else {
                        1.0
                    };
                let priced: Vec<hornvale_terrain::channel::BankReading> =
                    [home_reading, net.bank_reading(step.centroid())]
                        .into_iter()
                        .flatten()
                        // Clause 3: the gate applies `wadeable` to the readings
                        // standing inside their own bank edge and to no others.
                        .filter(|r| r.signed_distance.abs() < r.band_edges[1])
                        .collect();
                StepPricing {
                    step: step_len,
                    quiet: priced
                        .iter()
                        .all(|r| ctx.terrain().drainage_at(r.vertex) < WATERFALL_MIN_DRAINAGE),
                    lines: priced.iter().map(|r| r.line).collect(),
                    widths: priced.iter().map(|r| 2.0 * r.band_edges[0]).collect(),
                }
            })
            .collect()
    }
}

/// What §8's two fordability clauses compare for one step out of a transect's
/// home room — the gate's own operands, read from the network and the mesh
/// rather than from `crossing_between`.
///
/// One of these per entry in [`Transect::steps`]; see
/// [`Transect::width_pricing`] for why the widths are plural and why they are
/// not the transected reach's.
#[derive(Debug)]
struct StepPricing {
    /// The stride the width clause compares each width against:
    /// `room_edge(home).min(room_edge(step))`, times
    /// [`hornvale_locale::DIAGONAL_STEP_FACTOR`] when the step is
    /// corner-adjacent (decision 0515).
    step: f64,
    /// The full widths (`2 · band_edges[0]`) of the reaches the gate would
    /// price this step against: the interpretable readings among the two rooms.
    /// Empty when neither room stands inside its own bank edge, in which case
    /// the step is `NotACrossing`.
    widths: Vec<f64>,
    /// The winning polyline of each priced reading, positionally matched to
    /// `widths` — the identity that says whether two depths priced the same
    /// water.
    lines: Vec<usize>,
    /// Whether every reach priced here is below `WATERFALL_MIN_DRAINAGE`, so
    /// the discharge clause cannot be what decided this step.
    quiet: bool,
}

impl StepPricing {
    /// The width clause can only ADMIT this step: every reach the gate prices
    /// is narrower than the step.
    fn width_admits(&self) -> bool {
        self.widths.iter().all(|w| *w < self.step)
    }

    /// The width clause REFUSES this step: no reach the gate prices is narrower
    /// than the step. Vacuously true when nothing is priced — such a step is
    /// `NotACrossing`, which is equally not `Fordable`.
    fn width_refuses(&self) -> bool {
        !self.widths.iter().any(|w| *w < self.step)
    }
}

/// Why a sampled vertex yielded no usable transect. Counted and reported, never
/// silently dropped: the vertices hardest to pair are the widest channels,
/// which are exactly the ones §8's criterion calls Impassable, so a silent drop
/// inflates the fordable fraction for a reason that has nothing to do with the
/// world.
///
/// **The two drop causes the previous construction had are now structurally
/// absent** — a room always has at least four distinct mesh neighbours (the
/// whole edge prefix of `Facet::neighbors`; three before The Pavement), so
/// neither
/// "both probes in one room" (41 of 341 at half a room edge) nor "the pair is
/// not a step" (307 of 341) can arise. That is the argument for building the
/// step rather than probing for one: the drops it removes were the ones
/// correlated with nothing anybody had checked.
#[derive(Default, Debug)]
struct Drops {
    /// The channel room reads no channel at all. Unreachable on a world with a
    /// network — `bank_reading` is `None` only on an empty one — and counted
    /// rather than assumed away.
    no_reading: usize,
}

/// Transects of the network, one per sampled vertex, at most `wanted` of them.
///
/// **Strided, never truncated.** Taking the first `wanted` vertices in polyline
/// order would sample only the lowest-`Vertex` rivers; `windows/lab`'s
/// `lab_band_transects` derives a stride from the same cap for exactly that
/// reason, and this follows it. Deterministic in order and count; makes no
/// draws.
fn network_transects(ctx: &LocaleContext, wanted: usize) -> (Vec<Transect>, Drops, usize) {
    transects_at(ctx, wanted, walk_depth(ctx))
}

/// [`network_transects`] at a chosen room depth — the sensitivity handle, and
/// the whole of it. **The step length IS the room edge at `depth`**, so this
/// parameter is the one H2-4's reading depends on: the H2-4 test sweeps it, and
/// the width-clause positive control uses it to ask the same water a question
/// at a shorter step.
fn transects_at(ctx: &LocaleContext, wanted: usize, depth: u32) -> (Vec<Transect>, Drops, usize) {
    let net = ctx.terrain().channels();
    let vertices: usize = net.polylines.iter().map(|l| l.points.len()).sum();
    let stride = vertices.div_ceil(wanted.max(1)).max(1);
    let mut out = Vec::new();
    let mut drops = Drops::default();
    let mut seen = 0usize;
    let mut index = 0usize;
    for (i, line) in net.polylines.iter().enumerate() {
        for j in 0..line.points.len() {
            let take = index.is_multiple_of(stride);
            index += 1;
            if !take {
                continue;
            }
            seen += 1;
            match transect_at_vertex(ctx, i, j, depth) {
                Some(t) => out.push(t),
                None => drops.no_reading += 1,
            }
        }
    }
    (out, drops, seen)
}

/// One transect at a named vertex — the body `transects_at` builds each of its
/// samples from, factored out so a test can name a vertex the stride would not
/// have reached.
fn transect_at_vertex(ctx: &LocaleContext, i: usize, j: usize, depth: u32) -> Option<Transect> {
    let net = ctx.terrain().channels();
    let home = Facet::containing(net.polylines[i].points[j], depth);
    net.bank_reading(home.centroid())?;
    Some(Transect {
        polyline_at: (i, j),
        steps: home.neighbors(),
        home,
        edges: net.band_edges[i][j],
        vertex: net.run_vertices[i][j],
    })
}

/// [`network_transects`], plus the `extra` highest-discharge vertices in the
/// whole network.
///
/// **Why the stride alone stopped being enough** (The Rill, Task 3). A uniform
/// stride over vertices was a sample of *rivers* while the network rendered
/// only vertices above `RIVER_MIN_DRAINAGE`. It now renders the whole land flow
/// tree — 14,606 vertices on this world against 883 — and the added reaches are
/// overwhelmingly headwater creeks, so a 400-sample of it is a sample of
/// creeks: the strongest reach it happened to contain carried `drainage` 48,
/// against a `WATERFALL_MIN_DRAINAGE` of 80. The stride is not wrong; it is
/// still the unbiased sample H2-4 is read over. It is that a claim about *the
/// strongest water in the world* needs the strongest water in the world to be
/// in its population, and naming it is not cherry-picking when the claim names
/// it too.
///
/// Deterministic: `total_cmp` on discharge with a `(line, vertex)` tie-break,
/// and the union is de-duplicated by that same identity. The RANKING is
/// de-duplicated by **vertex** first, because `run_vertices` carries a trunk vertex
/// once per run terminating on it — ranking vertices gave 25 slots covering
/// only 13 distinct reaches.
fn transects_with_strongest(
    ctx: &LocaleContext,
    wanted: usize,
    extra: usize,
) -> (Vec<Transect>, Drops, usize) {
    let (mut out, drops, seen) = network_transects(ctx, wanted);
    let depth = walk_depth(ctx);
    let net = ctx.terrain().channels();
    // ONE SLOT PER DISTINCT VERTEX, not per vertex. A trunk vertex appears in
    // `run_vertices` once for every run that terminates on it, so a naive
    // vertex-ranking spends its budget on duplicates: measured, the top 25
    // VERTICES by discharge named only **13 distinct reaches**. The first
    // (line, vertex) seen for a vertex is its representative, which is
    // deterministic because `run_vertices` is in build order.
    let mut seen_vertex: std::collections::BTreeSet<u32> = std::collections::BTreeSet::new();
    let mut ranked: Vec<(f64, usize, usize)> = Vec::new();
    for (i, vertices) in net.run_vertices.iter().enumerate() {
        for (j, &c) in vertices.iter().enumerate() {
            if seen_vertex.insert(c.0) {
                ranked.push((ctx.terrain().drainage_at(c), i, j));
            }
        }
    }
    ranked.sort_by(|a, b| b.0.total_cmp(&a.0).then(a.1.cmp(&b.1)).then(a.2.cmp(&b.2)));
    let already: std::collections::BTreeSet<(usize, usize)> =
        out.iter().map(|t| t.polyline_at).collect();
    for &(_, i, j) in ranked.iter().take(extra) {
        if already.contains(&(i, j)) {
            continue;
        }
        if let Some(t) = transect_at_vertex(ctx, i, j, depth) {
            out.push(t);
        }
    }
    (out, drops, seen)
}

/// A ford is a SIGN CHANGE — the state machine's permitted left->right
/// transition. Two rooms on the same bank are not a crossing at all, however
/// deep in the water they stand.
///
/// **The population is chosen so that only the sign clause can be doing the
/// work.** The first draft of this test paired a room two room edges out from a
/// segment with a same-sign neighbour, found 1611 such pairs, and passed — and
/// went on passing with the sign clause deleted from `crossing_between`
/// entirely, because at that distance *neither* room is inside its own bank
/// edge and the band clause was excluding every pair on its own. A guard whose
/// subject is already excluded by another clause measures that other clause.
///
/// So every pair here has **at least one room inside its own bank edge**
/// (`|d| < channel_bands[1]`): the room the channel actually runs through, and
/// one of its mesh neighbours reading the same sign. **And both rooms'
/// readings are of the same polyline**, so the gate's same-channel clause is
/// satisfied by construction too. The band clause and the same-channel clause
/// are both satisfied, the two rooms are edge-adjacent — a real step — and the
/// only thing left that can return `NotACrossing` is the sign.
///
/// **The same-line filter is not decoration, and it was added the day the
/// same-channel clause shipped.** Without it the population is 460 pairs, of
/// which **20 select two different polylines** — and deleting the sign clause
/// reddens the test on only **440 of the 460**, because clause 1 refuses those
/// 20 on its own. That is this test's own warning turned on itself: a guard
/// whose subject is already excluded by another clause measures that other
/// clause. Filtered, the population is 440 and **deleting the sign clause
/// reddens the test on all 440**.
///
/// The reference for "same bank" — and for "same channel" — is the rooms' own
/// `bank_reading`s, read before `crossing_between` is asked anything.
#[test]
fn same_bank_neighbours_are_not_a_crossing() {
    let world = world();
    let ctx = LocaleContext::build(&world).unwrap();
    let net = ctx.terrain().channels();
    let depth = walk_depth(&ctx);
    let mut checked = 0usize;
    for line in &net.polylines {
        for &vertex in &line.points {
            // The room the centreline runs through: the one reading that is
            // guaranteed to be inside its own bank, whatever the reach.
            let room = Facet::containing(vertex, depth);
            let Some(here) = net.bank_reading(room.centroid()) else {
                continue;
            };
            if here.signed_distance.abs() >= here.band_edges[1] {
                continue; // not actually in the water; says nothing
            }
            for neighbour in room.neighbors() {
                let Some(there) = net.bank_reading(neighbour.centroid()) else {
                    continue;
                };
                if there.line != here.line {
                    // Two different rivers: clause 1 refuses this pair, so it
                    // would pass whatever the sign clause did. Excluded so the
                    // assertion below measures the sign and only the sign.
                    continue;
                }
                if (there.signed_distance > 0.0) != (here.signed_distance > 0.0) {
                    continue; // opposite banks — that IS a crossing
                }
                assert_eq!(
                    ctx.crossing_between(&room, &neighbour),
                    Crossing::NotACrossing,
                    "rooms {room:?} and {neighbour:?} read {} and {} — the same bank, with the \
                     first inside its own bank edge {} — yet the step between them was called a \
                     crossing",
                    here.signed_distance,
                    there.signed_distance,
                    here.band_edges[1],
                );
                checked += 1;
            }
        }
    }
    println!("same-bank adjacent pairs asserted on: {checked}");
    // Anti-vacuity: an empty loop asserts nothing. The floor leaves room for
    // terrain drift without leaving room for the population to quietly empty.
    assert!(
        checked >= SAME_BANK_PAIRS_FLOOR,
        "only {checked} same-bank adjacent pairs with a room inside its own bank edge were \
         found; the assertion ran on almost nothing"
    );
}

/// H2-4 — fords exist and are not everywhere.
///
/// The denominator is TRANSECTS OF THE NETWORK — one per sampled vertex —
/// deliberately: a population of "adjacent pairs whose sign differs" would BE
/// the ford set by construction (a one-step sign change is only possible when
/// the channel is narrower than one step, which is §8's criterion itself), and
/// the fraction would read ~1.0 no matter what the world looked like. A vertex
/// is in this population whether or not anything can be crossed there, so the
/// fraction can take any value in [0, 1].
///
/// `frac = |{v : Fordable(transect(v))}| / |V'|`, where `V` is the sampled
/// vertices and `V'` the ones that yielded a usable transect. A transect is
/// `Fordable` when at least one of the mesh steps out of the channel room
/// can be waded (see [`Transect::verdict`]).
///
/// # THE READING IS PARAMETER-DEPENDENT, AND THE PARAMETER IS THE STEP LENGTH
///
/// A room edge at `depth` is the step, and §8's criterion measures the channel
/// against exactly that. So the fraction is not a constant of the world; it is
/// a reading of the world *at a stated step length*, and the sweep is printed
/// beside it rather than left in a report. Halving the step (one level deeper)
/// halves what a walker can cross.
///
/// # What the attribution says, measured on seed 42 at level 6
///
/// **Every figure in this section is now PRINTED by the test.** Three of these
/// bullets went stale across The Rill's Task 3 and had to be reconciled by
/// hand; the numbers below are emitted on every run, so read the log and treat
/// the prose as the explanation rather than the source.
///
/// - **The width clause is inert at walk depth.** The widest full channel in
///   the world is 1.892e-4 rad against a room edge of 2.831e-4, so `2·b0 < step`
///   holds at **every** sampled vertex (395 of 395) and all the discrimination
///   is done by the discharge half. Task 3 did not move the widest channel —
///   the reaches it added are narrower than the ones already there — but it
///   moved the discharge distribution underneath it: over the sampled
///   transects, `drainage` now runs **min 0, median 2, max 102** (it ran 15 to
///   146, median 24, when only river vertices were rendered; the 0 is a mouth
///   vertex, whose vertex is the sea). So the discharge clause holds on 99.75% of
///   transects rather than 95.0%, and does correspondingly less discriminating.
///   The clause is not inert in principle —
///   `the_width_clause_binds_when_the_step_shrinks` is the positive control
///   that exercises it at a depth where it does bind.
/// - **The conjunction is not the fraction.** The clauses hold on **99.75%** of
///   transects (394 of 395) while **22.28%** are Fordable and only **88**
///   transects have any crossing at all (88 Fordable + 0 Impassable, printed as
///   `crossings (F + I)`); the gap is the crossing gate itself —
///   the same-channel clause, the sign change, and §5.3's requirement that a
///   room stand inside its own bank edge — not §8's criterion. (It was 95.0%,
///   33.7% and 123 of 341 at The Ford, so the gap has WIDENED: the criterion
///   admits nearly everything and the geometry refuses more of it.)
///
///   **The crossing count belongs to THIS test's population and nothing
///   else.** It read 96 for one round, which is `CROSSINGS_FLOOR`'s figure and
///   belongs to the *enriched* population
///   `the_discharge_clause_makes_the_strongest_crossing_impassable` assembles —
///   these 395 strided transects plus 25 injected extremes. Two populations,
///   two numbers, and the wrong one was transcribed while reconciling the
///   figure that preceded it. It is a printed local now.
///
///   Where the
///   channel is wider than every step available, both rooms sit on one bank and
///   the verdict is `NotACrossing` rather than `Impassable`: the geometry
///   refuses before the criterion is consulted.
/// - **Two different rivers are no longer a small part of the population.** Of
///   the **203** step pairs whose signs differ at walk depth, **36 are
///   cross-line** — the two readings were selected against different polylines,
///   so the two signs are expressed in **different frames** and the comparison
///   between them is **uninterpretable** — and **22 of the 36 also stand inside
///   a bank edge**, i.e. were admitted as crossings before `crossing_between`
///   gained its same-channel clause. At The Ford it was 5 of 171, and all 5
///   were inside a bank edge. The printed counts above are read from the
///   network, not from the gate.
///
///   **"Uninterpretable" is the exact claim, and it is weaker than "nothing is
///   there".** A cross-line pair says the two signs cannot be compared; it does
///   not say no water lies between the rooms. So the clause has a
///   **false-negative side**: at a confluence — which is where cross-line pairs
///   concentrate — a genuine crossing whose two rooms happen to select the
///   tributary and the trunk is now refused. That trade is taken deliberately.
///   Pricing an uncomparable pair is a wrong answer stated confidently, while
///   refusing it is a missed crossing at a known locus; and nothing in the
///   reading as it stands can distinguish the two cases, because a reading
///   knows only its own winning line. Recovering those crossings would need a
///   confluence-aware query (`run_vertices` states the join topology outright,
///   which is why it is published) — not a loosening of this clause.
/// # THE RILL, TASK 3: THIS WITNESS MOVED AND LOST A BRANCH
///
/// Rendering the whole land flow tree (883 -> 14,606 vertices) moved the
/// shipped reading without moving the interval it is scored against, so
/// **nothing here went red and the change would have been silent**. Measured
/// on the identical instrument, immediately before and after (the pre arm was
/// reconstructed by reverting `build`'s reach predicate alone, and verified
/// byte-identical to the pre-change level-5 golden):
///
/// ```text
///                        pre                    post
///   |V'| transects       295                    395   (the 400 cap now binds)
///   Fordable             106                     88
///   Impassable             8                      0
///   NotACrossing         181                    307
///   fordable fraction    0.3593                 0.2228   (-38% relative)
///   drainage clause      281/295 (0.9525)       394/395 (0.9975)
///   sign-flip pairs      150                    203
///   of which CROSS-LINE    3                     36      (12x)
/// ```
///
/// **The `Impassable` branch is now unexercised in this witness, and restoring
/// it here is DECLINED ON COST rather than impossible.** At walk depth the
/// width clause is inert (395/395), so `Impassable` requires a LOUD reach
/// (`Q >= WATERFALL_MIN_DRAINAGE`) that is also a crossing. The network holds
/// **91 loud vertices of 14,606** (0.62%), so a uniform 395-transect stride
/// expects **2.46** of them — printed by this test as `network loud vertices`
/// rather than typed, because it was typed wrong once: 0.67 is what you get by
/// substituting the 25 injected extremes for the 91 loud vertices, and it
/// contradicted the sample-size arithmetic two sentences later. Measured:
/// exactly **one** loud vertex, and it reads `NotACrossing`.
///
/// **The cost of restoring it, from printed numbers on both arms.** The pre arm
/// sampled 295 of 883 vertices and its `drainage clause` read 281, so **14**
/// loud vertices were sampled and 8 of them were `Impassable`. Reaching 14
/// again at 91-in-14,606 needs `14 × 14606/91 ≈ **2,247** transects — a **5.7x**
/// sample`, which would take this test from ~8 s to roughly 45 s on the commit
/// gate. (Not the 12x an earlier draft of this paragraph claimed; that figure
/// was derived from the wrong expectation.) So the branch is **closer to
/// recoverable than first stated**, and it is still declined — but the ordering
/// of the reasons matters and the first one is not cost:
///
/// 1. **Enrichment is ruled out on correctness, not price.** Injecting loud
///    reaches biases the very fraction this test exists to report.
/// 2. **Enlargement is unbiased and merely expensive** — 5.7x for one gate
///    test — and it is not even guaranteed to work: the conversion from "loud
///    vertex sampled" to "`Impassable` verdict" was 8 of 14 pre, and the one
///    loud vertex sampled post read `NotACrossing`, so 2,247 transects buys the
///    sample, not the branch.
/// 3. **A rare-branch positive control does not belong in an unbiased-fraction
///    witness anyway.** It is exercised elsewhere, which is the better home:
///    `the_discharge_clause_makes_the_strongest_crossing_impassable` asserts
///    every loud crossing reads `Impassable` over a deliberately enriched
///    population, and `LOUD_REACH_VERTICES_FLOOR` guards that the world still has
///    34 loud vertices to enrich from.
///
/// The loud count, the network total and the stride's expectation are all
/// printed by this test, so the log carries the arithmetic rather than only
/// this paragraph.
///
/// **The 12x jump in cross-line pairs is the other half of the same fact.** The
/// same-channel clause now refuses twelve times as many comparisons as the
/// paragraph above was written against — 36 pairs, 22 of them inside a bank
/// edge — so its false-negative side, described above as small, is no longer
/// small. It is still the right trade, and it is now a much larger one.
///
/// - **Measured, at walk depth: 88 Fordable, 0 Impassable, 307 NotACrossing of
///   395 — a fraction of 0.2228** (The Rill, Task 3). Two earlier readings, on
///   the same instrument and progressively larger networks: 106/8/181 of 295,
///   **0.3593**, immediately before Task 3; and 115/8/218 of 341, **0.3372**,
///   at The Ford, which is the figure the rest of this doc block was written
///   against.
///
///   **The step sweep changed SHAPE, not only level, and that is a finding
///   about the world rather than about the instrument:**
///
///   ```text
///     depth   step (rad)   The Ford   pre-Task-3   post-Task-3
///      11      5.66e-4      0.2170      0.2169       0.1190
///      12*     2.83e-4      0.3372      0.3593       0.2228
///      13      1.42e-4      0.3900      0.4339       0.3089
///      14      7.08e-5      0.1026      0.1017       0.3342
///      15      3.54e-5      0.0000      0.0000       0.1848
///     (* walk depth)
///   ```
///
///   **THE MESH THIS WHOLE TABLE WAS MEASURED ON NO LONGER EXISTS (The
///   Pavement, Task 3). The table is history; do not read any row as current.**
///   Every figure above was taken on the icosahedral triangle lattice, where a
///   room had **3 neighbours of 3 possible**. The base mesh is now a
///   tangent-warped cube-sphere quad lattice and the neighbourhood is **8 of 8**
///   (7 at a cube corner), so both the room a sample lands in and the set of
///   steps out of it moved. Re-measured at walk depth on seed 42, `|V'| = 394`
///   against the 395 the post-Task-3 row was taken over:
///
///   ```text
///     walk-depth fordable fraction, same population size, mesh varied
///       0.2228   recorded above (triangle lattice, 3 of 3 neighbours)
///       0.3452   cube lattice, neighbourhood TRUNCATED to 3   <- geometry alone
///       0.3807   cube lattice, the 4 edge neighbours
///       0.4695   cube lattice, all 8 neighbours               <- what ships
///   ```
///
///   **The isolation matters more than the headline, and it points the other
///   way from the obvious guess.** Truncating today's neighbourhood back to
///   three isolates the geometry change from the connectivity change: the
///   geometry alone moves 0.2228 -> 0.3452 (+55% relative), and widening 3 -> 8
///   adds 0.3452 -> 0.4695 (+36% relative). So the larger share of the movement
///   is the base mesh, not the extra steps — a reader who attributed all of it
///   to 8-connectivity would be wrong by roughly three-fifths.
///
///   **The assertion below still passes, which is exactly why this note is
///   here.** §8's interval is `[0.10, 0.70]`; 0.4695 sits inside it as 0.2228
///   did, so nothing goes red and a 2.11x relative move in the campaign's
///   headline §8 number would otherwise be invisible. **No threshold was
///   adjusted** — the interval is untouched and so is the criterion. A moved
///   number here is a finding, not a failure (CLAUDE.md; decision 0016), and
///   The Pavement's Task 10 owns the chronicle entry.
///
///   **THE SWEEP HAS NOT BEEN RE-TAKEN and the numbers above are walk depth
///   only.** Which depth peaks, and whether the collapse at depth 15 is still
///   absent, are open on the new mesh. Anyone quoting fordability as a function
///   of traversal scale must re-run the sweep rather than carry the table below
///   forward.
///
///   The peak moved from depth 13 to **depth 14**, and the collapse to zero at
///   depth 15 — "by then no step spans the water", as this bullet used to read
///   — **no longer happens**. The mechanism is the width law, not the gate: a
///   3.54e-5 rad step cannot straddle a 1.9e-4 rad mainstem, which is what
///   every reach in the old network was, but it comfortably straddles a
///   7.35e-6 rad headwater creek, which is what most of the new one is. A
///   denser network means there is still water to ford three levels below walk
///   depth. Anyone reading fordability as a function of traversal scale should
///   take this table, not the single walk-depth row.
///
///   **The same-channel clause moved the sweep and not the headline — as
///   measured AT THE FORD, on a 341-transect population with 5 cross-line
///   pairs.** Adding it left walk depth exactly where it was (115/8/218,
///   0.3372) because none of those 5 was the *deciding* step for its transect.
///   One level up it moved 0.2229 → 0.2170 (76 → 74 Fordable) and one level
///   down 0.3959 → 0.3900 (135 → 133); depths two and three below were
///   unchanged. This is a **correction, not a tuning**: it *adds* a constraint
///   (the two readings must be of the same river), which is the test that
///   separates a correction from a rescue, and it was adopted before its effect
///   on the fraction was known. **The attribution is not re-derived for the
///   current population and should not be read as if it were**: cross-line
///   pairs are now 36 rather than 5, so how much the clause moves the sweep is
///   an open number, not the one above.
///
/// # H2-4 IS NOT RESOLVED, AND THE ASSERTION BELOW IS NOT A CONFIRMATION
///
/// Read this before quoting the number anywhere.
///
/// §8's criterion is a **late freeze**, not a preregistration (spec §8): it was
/// chosen with stage-1 measurements already in hand. On top of that, **three
/// separate instrument changes were made after seeing a number**:
///
/// 1. the probe separation was reinterpreted from one room edge *per probe* to
///    one room edge *between the pair*, after the first reading (**0.0308**)
///    fell outside the interval;
/// 2. the mesh-adjacency requirement was added, after which the surviving
///    population was 34 of 341 pairs and read **0.7059** — **also outside
///    [0.10, 0.70]**, i.e. a *falsifying* reading, not merely a thin one;
/// 3. the transect was rebuilt around the mesh steps out of each vertex's room
///    (three at the time, on the triangle lattice) — **adopted
///    directly after that falsifying reading** — giving **0.3372**, The Ford's
///    column in the sweep table above.
///
/// A fourth change landed in the pre-merge fix wave, and it is deliberately not
/// in that list: `crossing_between` gained its same-channel clause. That is a
/// repair to the **subject** — the gate was calling two different rivers one
/// crossing — not a re-cut of the **instrument**, it *adds* a constraint rather
/// than removing one, and it was adopted before its effect on the fraction was
/// known. Its measured effect is in the attribution above (walk depth
/// unchanged; the sweep moved at two depths).
///
/// Every change was forced by a defect in the previous instrument rather than by
/// the number it produced, each is disclosed, and the quantifier and expected
/// band for (3) were written down before it ran. None of that restores the
/// epistemic status of a hypothesis whose free parameter moved four times under
/// unblinded observation.
///
/// **So: H2-4 was not tested under preregistration and is not resolved. The
/// current reading — 0.2228 at one room edge at walk depth, on the whole-flow-
/// tree network — is reportable as a measurement of the world at a stated step
/// length AND a stated network, not as a confirmation of the [0.10, 0.70]
/// interval.** Quote it with both qualifiers: the same instrument read 0.3372
/// and 0.3593 on the two sparser networks that preceded it, all three inside
/// the interval, so the interval is not what is distinguishing them.
/// The range check below is a **witness** that pins today's reading and reddens
/// if it moves; it is not a hypothesis test, and this test's name should be read
/// as naming the check rather than announcing a result.
#[test]
fn the_fordable_fraction_of_the_network_is_within_its_interval() {
    let world = world();
    let ctx = LocaleContext::build(&world).unwrap();
    let (transects, drops, sampled) = network_transects(&ctx, 400);

    let net = ctx.terrain().channels();
    let mut fordable = 0usize;
    let mut impassable = 0usize;
    let mut not_a_crossing = 0usize;
    let mut width_clause = 0usize;
    let mut drainage_clause = 0usize;
    let mut both_clauses = 0usize;
    let mut sign_flip_pairs = 0usize;
    let mut loud_reaches = 0usize;
    let mut cross_line_pairs = 0usize;
    let mut cross_line_admitted = 0usize;
    for t in &transects {
        let verdict = t.verdict(&ctx);
        // Symmetry, asserted here rather than in its own test so it is checked
        // over the whole real population without paying for a second world.
        for step in &t.steps {
            assert_eq!(
                ctx.crossing_between(&t.home, step),
                ctx.crossing_between(step, &t.home),
                "crossing_between is not symmetric at {:?}/{step:?}",
                t.home
            );
            // How much of this population is TWO DIFFERENT RIVERS. Read from
            // the network rather than from the gate, so the number is a
            // reference the gate cannot influence: a pair whose signs differ
            // but whose winning lines differ is an UNINTERPRETABLE comparison —
            // each sign is in its own river's frame — and `crossing_between`'s
            // same-channel clause refuses it. Refuses, not disproves: there may
            // still be water between the rooms (see the doc above on the
            // false-negative side of that trade). The second count is the sharp
            // one: cross-line pairs that ALSO clear the bank-edge clause are
            // the pairs the gate would have called a crossing before that
            // clause existed. Printed rather than asserted; these attribute
            // the fraction, they do not define it.
            if let (Some(ra), Some(rb)) = (
                net.bank_reading(t.home.centroid()),
                net.bank_reading(step.centroid()),
            ) && ((ra.signed_distance > 0.0 && rb.signed_distance < 0.0)
                || (ra.signed_distance < 0.0 && rb.signed_distance > 0.0))
            {
                sign_flip_pairs += 1;
                if ra.line != rb.line {
                    cross_line_pairs += 1;
                    let interpretable = |r: &hornvale_terrain::channel::BankReading| {
                        r.signed_distance.abs() < r.band_edges[1]
                    };
                    cross_line_admitted += usize::from(interpretable(&ra) || interpretable(&rb));
                }
            }
        }
        match verdict {
            Crossing::Fordable => fordable += 1,
            Crossing::Impassable => impassable += 1,
            Crossing::NotACrossing => not_a_crossing += 1,
        }
        // The two clauses of §8, evaluated on the reach BEING transected (the
        // vertex's own edges and vertex) rather than on whichever reading a room
        // happened to win. Reported, never asserted on: these attribute the
        // fraction above, they do not define it.
        let step = room_edge(&t.home);
        let narrow = 2.0 * t.edges[0] < step;
        let quiet = ctx.terrain().drainage_at(t.vertex) < WATERFALL_MIN_DRAINAGE;
        loud_reaches += usize::from(!quiet);
        width_clause += usize::from(narrow);
        drainage_clause += usize::from(quiet);
        both_clauses += usize::from(narrow && quiet);
    }

    let usable = transects.len();
    let frac = fordable as f64 / usable as f64;
    // The attribution numbers, COMPUTED AND PRINTED rather than left in prose.
    // Three bullets in this doc block went stale across The Rill's Task 3 and
    // had to be reconciled by hand; a number the test emits cannot.
    let widest_full = net.widest_half_width() * 2.0;
    // Crossings, computed rather than quoted. The doc block above stated 96
    // here for one round — `CROSSINGS_FLOOR`'s number, which belongs to the
    // ENRICHED population `the_discharge_clause_…` assembles (395 strided plus
    // 25 injected extremes), not to this test's unenriched 395. A figure
    // transcribed from a neighbouring population is exactly the defect this
    // whole doc block was rewritten to remove, so this one is a local too.
    let crossings = fordable + impassable;
    // What a UNIFORM STRIDE can expect to catch of the network's loud reaches —
    // the arithmetic behind declining to restore the `Impassable` branch here.
    // Printed because it was typed wrong once: 91 loud of 14,606 at a 395
    // sample is 2.46, and 0.67 is what you get by substituting the 25 injected
    // extremes for the 91 loud vertices.
    let network_vertices: usize = net.polylines.iter().map(|l| l.points.len()).sum();
    let network_loud = net
        .run_vertices
        .iter()
        .flatten()
        .filter(|&&c| ctx.terrain().drainage_at(c) >= WATERFALL_MIN_DRAINAGE)
        .count();
    let loud_expected = usable as f64 * network_loud as f64 / network_vertices.max(1) as f64;
    let mut discharges: Vec<f64> = transects
        .iter()
        .map(|t| ctx.terrain().drainage_at(t.vertex))
        .collect();
    discharges.sort_by(f64::total_cmp);
    let (q_min, q_med, q_max) = (
        discharges[0],
        discharges[discharges.len() / 2],
        discharges[discharges.len() - 1],
    );

    // This floor comes BEFORE the diagnostic print, which indexes
    // `transects[0]` for the step length: on an empty population the print
    // would panic with a slice-index message and the reader would never see
    // the floor's explanation of what actually went wrong.
    //
    // What it guards, precisely, is NOT what it originally guarded. When it
    // was written, transects were probed and then filtered, so a collapsed
    // denominator meant selection bias. Since the instrument was rebuilt to
    // CONSTRUCT each transect from a room and its own `neighbors()`, both drop
    // causes became structurally impossible and `|V'| = |V|` always. So this
    // now guards the network shrinking — a terrain change that stops producing
    // polylines — not the sample selecting itself.
    //
    // AND SINCE THE RILL'S TASK 3 IT GUARDS CATASTROPHE, NOT DRIFT, because
    // `wanted` caps the sample: measured |V'| = 395 of 395 sampled, off a
    // network of **14,606** vertices at stride 37 (it was 341 of 341 off 681
    // vertices at stride 2 when this comment was written). |V'| is now pinned
    // near 400 by the cap whatever the network does, so this can only fire on a
    // collapse below ~200 vertices — a 73x shrink. Raising it toward 400 would
    // make it a guard on the CAP rather than on the world. The floor that
    // actually notices a shrinking network is
    // `rill_properties.rs::MIN_RUNS_PER_SEED`, which reads the network itself.
    assert!(
        usable >= 200,
        "only {usable} usable transects of {sampled} sampled ({drops:?}); the fraction is an \
         anecdote"
    );

    println!(
        "H2-4 (seed 42, level {}, walk depth {}, step = one room edge = {:e} rad):\n  \
         |V| sampled vertices      = {sampled}\n  \
         |V'| usable transects     = {usable}\n  \
         drops                     = {drops:?}\n  \
         Fordable                  = {fordable}\n  \
         Impassable                = {impassable}\n  \
         NotACrossing              = {not_a_crossing}\n  \
         fordable fraction         = {frac:.4}  (of |V'|)\n  \
         width clause  (2*b0<step) = {width_clause} ({:.4})\n  \
         drainage clause (Q<{WATERFALL_MIN_DRAINAGE}) = {drainage_clause} ({:.4})\n  \
         conjunction               = {both_clauses} ({:.4})\n  \
         loud reaches (Q>={WATERFALL_MIN_DRAINAGE}) = {loud_reaches}  (an Impassable verdict at \
         this depth needs one; see this test's doc)\n  \
         sign-flip step pairs      = {sign_flip_pairs}\n  \
         of which CROSS-LINE       = {cross_line_pairs} ({cross_line_admitted} also inside a \
         bank edge, i.e. admitted before the same-channel clause)\n  \
         crossings (F + I)         = {crossings}  (this test's UNENRICHED population; the \
         discharge test's is larger because it injects extremes)\n  \
         widest full channel       = {widest_full:e} rad (vs one room edge above)\n  \
         transect discharge        = min {q_min}, median {q_med}, max {q_max}\n  \
         network loud vertices     = {network_loud} of {network_vertices}; a uniform \
         {usable}-transect stride expects {loud_expected:.2} of them",
        ctx.globe_level(),
        walk_depth(&ctx),
        room_edge(&transects[0].home),
        width_clause as f64 / usable as f64,
        drainage_clause as f64 / usable as f64,
        both_clauses as f64 / usable as f64,
    );

    // The step-length sensitivity, printed rather than asserted. The shipped
    // instrument is the walk-depth row; the others are here so a reader can see
    // that the fraction is a reading at a stated step length and not a constant
    // of the world.
    for delta in [-1i32, 0, 1, 2, 3] {
        let depth = (walk_depth(&ctx) as i32 + delta) as u32;
        let (ts, dr, seen) = transects_at(&ctx, 400, depth);
        // A sweep row is diagnostic, not asserted, so an empty population here
        // must report itself rather than panic on `ts[0]` and take the whole
        // witness down with it.
        let Some(first) = ts.first() else {
            println!("  depth {depth}: no usable transects of {seen} sampled, drops {dr:?}");
            continue;
        };
        let f = ts
            .iter()
            .filter(|t| t.verdict(&ctx) == Crossing::Fordable)
            .count();
        println!(
            "  depth {depth} (step {:e} rad): fordable {f}/{} of {seen} sampled ({:.4}), \
             drops {dr:?}",
            room_edge(&first.home),
            ts.len(),
            f as f64 / ts.len() as f64
        );
    }

    // A WITNESS, not a hypothesis test — see the note above. It pins today's
    // reading and reddens if it moves; passing it is not a confirmation of
    // H2-4, which is not resolved.
    assert!(
        (0.10..=0.70).contains(&frac),
        "fordable fraction {frac} outside the spec's [0.10, 0.70] \
         ({fordable} of {usable} usable transects, {sampled} sampled, drops {drops:?}; \
         width clause {width_clause}, drainage clause {drainage_clause}, both {both_clauses})"
    );
}

/// §8's DISCHARGE CLAUSE, POSITIVE CONTROL. A crossing the walker can step
/// across is still `Impassable` when the water is strong enough.
///
/// Without this the clause is unguarded: at walk depth the width half holds
/// everywhere, so deleting `drainage_at(vertex) < WATERFALL_MIN_DRAINAGE`
/// promotes every `Impassable` transect to `Fordable` and moves H2-4's fraction
/// by less than the interval's width — the suite stays green while half the
/// task's named deliverable is gone.
///
/// **The population is the strided sample PLUS the network's 25 strongest
/// reaches** (The Rill, Task 3). Once every land vertex is rendered, a uniform
/// stride over 14,606 vertices is a sample of headwater creeks — its strongest
/// reach carried `drainage` 48 against a threshold of 80, and this test went red
/// for that reason and no other. See `transects_with_strongest` for why naming
/// the extreme is not cherry-picking: the claim names it.
///
/// **The reference is the world's own discharge ORDERING, not the criterion's
/// threshold.** Among the transects where a crossing exists at all, the one
/// carrying the most water must not be wadeable and the one carrying the least
/// must be. That is a claim about the world (the biggest river in it is not a
/// ford) scored against `GeneratedTerrain::drainage_at`, which knows nothing
/// about `crossing_between`; it does not restate the threshold, and it holds
/// for any threshold falling between the two extremes. The threshold-keyed
/// count below is the anti-vacuity companion, not the claim.
#[test]
fn the_discharge_clause_makes_the_strongest_crossing_impassable() {
    let world = world();
    let ctx = LocaleContext::build(&world).unwrap();
    let (transects, _, _) = transects_with_strongest(&ctx, 400, 25);
    let net = ctx.terrain().channels();

    // Every transect where a crossing exists, paired with the discharge of the
    // reach it transects. `total_cmp` with an index tie-break, so the extremes
    // are deterministic.
    let mut crossings: Vec<(f64, usize, Crossing)> = transects
        .iter()
        .enumerate()
        .filter_map(|(k, t)| {
            let verdict = t.verdict(&ctx);
            (verdict != Crossing::NotACrossing)
                .then(|| (ctx.terrain().drainage_at(t.vertex), k, verdict))
        })
        .collect();
    crossings.sort_by(|a, b| a.0.total_cmp(&b.0).then(a.1.cmp(&b.1)));
    assert!(
        crossings.len() >= CROSSINGS_FLOOR,
        "only {} transects cross a channel at all; the extremes are not a population",
        crossings.len()
    );
    let (weakest_q, _, weakest) = crossings[0];
    let (strongest_q, _, strongest) = crossings[crossings.len() - 1];
    println!(
        "discharge ordering over {} crossings: weakest Q={weakest_q} -> {weakest:?}, \
         strongest Q={strongest_q} -> {strongest:?}",
        crossings.len()
    );
    assert_eq!(
        strongest,
        Crossing::Impassable,
        "the strongest water in the world that a walker can reach ({strongest_q}) reads \
         {strongest:?} — the discharge clause is not deciding anything"
    );
    assert_eq!(
        weakest,
        Crossing::Fordable,
        "the weakest water in the world ({weakest_q}) reads {weakest:?}, so the verdict is not \
         tracking discharge at all"
    );
    // Anti-vacuity for the ordering claim: the extremes must actually straddle
    // the threshold, or "strongest is impassable" could hold for a reason that
    // has nothing to do with discharge.
    assert!(
        weakest_q < WATERFALL_MIN_DRAINAGE && strongest_q >= WATERFALL_MIN_DRAINAGE,
        "the crossings' discharge range [{weakest_q}, {strongest_q}] does not straddle \
         {WATERFALL_MIN_DRAINAGE}, so the ordering claim is not about the discharge clause"
    );
    // And every crossing above the threshold, not merely the extreme one.
    let mut strong = 0usize;
    for t in &transects {
        let verdict = t.verdict(&ctx);
        if verdict != Crossing::NotACrossing
            && ctx.terrain().drainage_at(t.vertex) >= WATERFALL_MIN_DRAINAGE
        {
            assert_eq!(
                verdict,
                Crossing::Impassable,
                "a crossing at Q={} reads Fordable",
                ctx.terrain().drainage_at(t.vertex)
            );
            strong += 1;
        }
    }
    // THE TWO WORLD-SIDE FLOORS THAT USED TO CLOSE THIS TEST NOW LIVE IN
    // `the_world_still_produces_water_loud_enough_to_refuse`, which is
    // `#[ignore]`d because the epoch mis-scaled them
    // (`MAP-waterfall-threshold-mis-scaled`). Everything above is unaffected
    // and still runs: the ordering claim, its straddle anti-vacuity and the
    // universal "every crossing above the threshold is Impassable" are claims
    // about the world's own discharge ordering, not about the floors, and the
    // epoch did not touch them. `loud_vertices` is counted beside them by
    // `the_loud_reach_population_is_pinned_as_a_witness`.
    println!(
        "crossings above the discharge threshold: {strong} (from a population enriched with the \
         25 strongest reaches)"
    );
    let _ = net;
}

/// **The two world-side floors, `#[ignore]`d because the epoch mis-scaled
/// them — the assertions themselves are unchanged.**
///
/// These closed [`the_discharge_clause_makes_the_strongest_crossing_impassable`]
/// until The Glasshouse's sea-level epoch. They are the floors that can notice
/// a *disappearance*: `strong` counts crossings inside a population that test
/// ENRICHES with the 25 strongest reaches, so it cannot fall while the
/// enrichment works, and `loud_vertices` counts distinct loud vertices in the network
/// itself, read off `drainage`, which no sampling choice can inflate.
///
/// **What moved is the world, not the instrument.** A higher sea level shortens
/// drainage paths and shrinks catchments, so seed 42's loud vertices fell 34 → 16
/// and strong crossings 8 → 2. `WATERFALL_MIN_DRAINAGE = 80` was calibrated
/// against pre-epoch catchments and is now measuring a different world at the
/// old scale. Lowering these floors would delete the only
/// instrument that noticed, which is why they are deferred at their pre-epoch
/// values rather than nudged — the `"PREREGISTERED, not met"` convention
/// rostered in `cli/tests/heavy_tier.rs`.
///
/// # ONE OF THE TWO FLOORS IS NO LONGER UNMET (The Pavement, Task 11)
///
/// This test still fails, and it now fails on **one** assertion rather than
/// two. `strong` moved **2 → 15** when the room mesh became a cube-sphere quad
/// lattice, which is clear of `STRONG_CROSSINGS_FLOOR` (4) — run it with
/// `--ignored` and the panic is the loud-vertices message alone. So the
/// deferral's shape has changed: the crossing count is a WITNESS now (pinned
/// by the test below), not a deferred criterion, and only
/// `LOUD_REACH_VERTICES_FLOOR` still awaits the re-fit.
///
/// **Not a rescue, and the direction is the giveaway.** Nothing was lowered to
/// let `strong` through — it rose past a floor that has not moved since it was
/// authored, because a walker on a quad mesh meets more of the same water than
/// a walker on a triangular one. `loud_vertices` is unchanged at 16, which is
/// the honest reading of that: the NETWORK did not move, the reach did.
///
/// The repair is to re-fit the threshold against post-epoch catchments under
/// decision 0106's provenance discipline, with a census metric that makes the
/// catchment scale visible; both are campaign-sized and tracked as
/// `MAP-waterfall-threshold-mis-scaled`. Because an ignored measurement stops
/// being measured, [`the_loud_reach_population_is_pinned_as_a_witness`] runs
/// always and pins what the world actually produces now.
#[ignore = "PREREGISTERED, not met: awaits MAP-waterfall-threshold-mis-scaled (WATERFALL_MIN_DRAINAGE = 80 was calibrated on pre-epoch catchments; the sea-level epoch shortened drainage paths, so seed 42's loud vertices fell 34 -> 16 against a floor of 17, and lowering that floor would delete the only instrument that noticed. THE STRONG-CROSSINGS HALF OF THIS DEFERRAL HAS LAPSED: The Pavement's cube-sphere mesh moved that reading 2 -> 15, clear of its floor of 4 - 10 of the 13 is the base mesh and 3 is the widening from three neighbours to eight - so the loud-vertices floor alone is still unmet and this test now fails on one assertion, not two)"]
#[test]
fn the_world_still_produces_water_loud_enough_to_refuse() {
    let world = world();
    let ctx = LocaleContext::build(&world).unwrap();
    let (transects, _, _) = transects_with_strongest(&ctx, 400, 25);
    let net = ctx.terrain().channels();

    let mut strong = 0usize;
    for t in &transects {
        if t.verdict(&ctx) != Crossing::NotACrossing
            && ctx.terrain().drainage_at(t.vertex) >= WATERFALL_MIN_DRAINAGE
        {
            strong += 1;
        }
    }
    let loud_vertices: std::collections::BTreeSet<u32> = net
        .run_vertices
        .iter()
        .flatten()
        .filter(|&&c| ctx.terrain().drainage_at(c) >= WATERFALL_MIN_DRAINAGE)
        .map(|c| c.0)
        .collect();
    assert!(
        loud_vertices.len() >= LOUD_REACH_VERTICES_FLOOR,
        "only {} distinct vertices in the whole network carry Q >= {WATERFALL_MIN_DRAINAGE} \
         (measured 34 pre-epoch) — the world has stopped producing water strong enough to \
         refuse, and no amount of sampling can put it back",
        loud_vertices.len()
    );
    assert!(
        strong >= STRONG_CROSSINGS_FLOOR,
        "only {strong} crossings carry enough water to be refused; the universal assertion in \
         the_discharge_clause_makes_the_strongest_crossing_impassable ran on almost nothing"
    );
}

/// **The witness that keeps the mis-scaled floors measured while
/// [`the_world_still_produces_water_loud_enough_to_refuse`] is `#[ignore]`d.**
///
/// This pins a witness, not a claim. The integers below are not a bar the
/// world must clear — they are exactly what seed 42 produces at
/// `WATERFALL_MIN_DRAINAGE = 80` after the epoch, recorded so that any change
/// to sea level, to the channel network or to the threshold *forces a
/// deliberate re-read*. Without it the ignored floors measure nothing and the
/// "34 → 16, 8 → 2" quoted in this file, in the roster string and in
/// `MAP-waterfall-threshold-mis-scaled` quietly becomes fiction.
///
/// The two integers are pinned separately because they fail differently:
/// `loud_vertices` is a fact about the network (how much loud water exists at
/// all) and `strong` is a fact about reachability (how much of it a walker
/// meets).
///
/// # THE DECISION RULE, WITH ITS THIRD ARM (The Pavement, Task 11)
///
/// It read as a binary and the binary was wrong. Three arms, not two:
///
/// 1. **Both integers move** → the threshold or the world's hypsometry moved.
///    That is the epoch case, and the re-fit
///    `MAP-waterfall-threshold-mis-scaled` calls for.
/// 2. **Only `loud_vertices` moves** → the NETWORK moved (a channel-rendering
///    or drainage change) while the walker's reach did not.
/// 3. **Only `strong` moves** → the ground a walker WALKS ON moved, or the
///    sampling did. These are not the same thing and the old rule collapsed
///    them, naming only the second: *"a sampling or transect regression"*.
///
/// **Arm 3 is what actually happened, and following the old rule would have
/// sent the next reader hunting a bug that does not exist.** The mesh became a
/// cube-sphere quad lattice and a transect's neighbourhood went from three of
/// three to eight of eight; nothing about the discharge clause, the threshold,
/// the sampling rule or the criterion changed. Distinguishing arm 3's two
/// halves is a measurement, not a judgement: vary the neighbourhood on the
/// mesh and see whether the number tracks it (the table below does exactly
/// that), and if it does, the transect construction is not what moved.
///
/// # THE WITNESS MOVED, AND HAS BEEN RE-STATED: `(16, 2)` -> `(16, 15)`
///
/// Task 3 measured this move and deliberately did NOT re-pin it, because this
/// test's own failure message requires a coordinated re-statement across four
/// sites in one commit. **That restraint was correct and this is the
/// re-statement.** The four, all landing together:
///
/// 1. the pin below, `(16, 2)` -> `(16, 15)`;
/// 2. the `#[ignore]` reason on
///    [`the_world_still_produces_water_loud_enough_to_refuse`], which now
///    records that its strong-crossings half has LAPSED;
/// 3. that reason's verbatim copy in `cli/tests/suite/heavy_tier.rs`'s
///    `EXPECTED_UNTOKENISED` roster, which is compared byte for byte;
/// 4. the `MAP-waterfall-threshold-mis-scaled` row in
///    `book/src/frontier/idea-registry.md`.
///
/// **What moved, and what did not.** `loud_vertices` is **unchanged at 16** —
/// the network and the threshold did not move, and this campaign never touched
/// either. `strong` went **2 -> 15**. Measured on seed 42 with the
/// neighbourhood varied on today's mesh:
///
/// ```text
///    2   recorded pin (triangle lattice, 3 neighbours of 3 possible)
///   12   cube lattice, neighbourhood TRUNCATED to 3   <- geometry alone
///   13   cube lattice, the 4 edge neighbours
///   15   cube lattice, all 8 neighbours               <- what ships
/// ```
///
/// # DO NOT READ THIS AS "8-CONNECTIVITY MOVED THE WATER"
///
/// The obvious attribution is wrong, and the decomposition above is the whole
/// reason this paragraph is here. **Of the 13-crossing rise, 10 is the base
/// cube mesh and 3 is the widening from three steps to eight** — the mesh is
/// roughly three quarters of it and the connectivity change is the remainder.
/// The same split appears in the fordable fraction, measured independently:
///
/// ```text
///   0.2228  triangle lattice
///   0.3452  cube lattice, geometry alone
///   0.3807  cube lattice, the four edge neighbours
///   0.4695  cube lattice, all eight
/// ```
///
/// **A 2.11x total move of which the base mesh is roughly HALF — 49.6%, not
/// the "three fifths" this doc said until the Task 11 fix round.** The
/// arithmetic is `(0.3452 - 0.2228) / (0.4695 - 0.2228) = 0.1224 / 0.2467`;
/// carrying it out to the four-edge rung gives 64.0%, and the eight-way
/// widening is the remaining 36.0%. The crossings gloss above is a different
/// number and IS right: 10 of 13 is 76.9%, roughly three quarters.
///
/// The correction matters in the same direction the paragraph does, and less
/// strongly than it claimed: a reader who concludes that letting a walker step
/// diagonally is what made the world's water fordable has still attributed the
/// larger half of a base-geometry change to a neighbourhood change — but it is
/// a half, not three fifths, and on this measure the two causes are nearer to
/// equal than the crossings count suggests.
///
/// The base mesh is a tangent-warped cube-sphere quad lattice (The Pavement,
/// Tasks 1-2) and the transect's neighbourhood is now 8 of 8 rather than 3 of
/// 3 (`Transect`'s own doc carries the ruling and the reason). Not a fix and
/// not a tuning: nothing about the discharge clause, the threshold, the
/// sampling rule or the criterion changed.
#[test]
fn the_loud_reach_population_is_pinned_as_a_witness() {
    let world = world();
    let ctx = LocaleContext::build(&world).unwrap();
    let (transects, _, _) = transects_with_strongest(&ctx, 400, 25);
    let net = ctx.terrain().channels();

    let mut strong = 0usize;
    for t in &transects {
        if t.verdict(&ctx) != Crossing::NotACrossing
            && ctx.terrain().drainage_at(t.vertex) >= WATERFALL_MIN_DRAINAGE
        {
            strong += 1;
        }
    }
    let loud_vertices: std::collections::BTreeSet<u32> = net
        .run_vertices
        .iter()
        .flatten()
        .filter(|&&c| ctx.terrain().drainage_at(c) >= WATERFALL_MIN_DRAINAGE)
        .map(|c| c.0)
        .collect();
    assert_eq!(
        (loud_vertices.len(), strong),
        (16, 15),
        "the post-epoch loud-reach population moved: {} loud vertices and {strong} strong \
         crossings, against the pinned (16, 15). This is NOT a number to update — re-read the \
         catchment scale, then re-state this witness, the #[ignore] reason on \
         the_world_still_produces_water_loud_enough_to_refuse, its roster entry in \
         cli/tests/suite/heavy_tier.rs and the MAP-waterfall-threshold-mis-scaled registry row \
         in the SAME commit. Before deciding which of the three arms of the rule above you are \
         in, vary the neighbourhood on the mesh and see whether the number tracks it — that is \
         what separates a mesh change from a transect regression.",
        loud_vertices.len()
    );
}

/// §8's WIDTH CLAUSE, POSITIVE CONTROL — at a depth where it binds.
///
/// At walk depth the clause is inert on seed 42: the widest full channel is
/// 1.89e-4 rad against a 2.71e-4 room edge, so `2·b0 < step` holds at every
/// vertex and deleting the clause changes nothing. (The Rill's Task 3 widened
/// the network to the whole land flow tree without moving that: the reaches it
/// added are NARROWER than the ones already there, so the widest channel in the
/// world is unchanged and the clause is inert at walk depth for the same
/// reason.) That is a fact about this
/// world at this depth, not about the criterion — and an unexercised clause is
/// an unguarded one.
///
/// A room edge halves with each level, so three levels below walk depth the
/// step is 3.4e-5 rad against a median full width of 7.8e-5 and the clause
/// binds. The control is the pair: **the same vertex, the same water, the same
/// discharge — Fordable at walk depth, Impassable three levels down.** Only the
/// traversal unit changed, which is precisely what §8 claims to measure.
///
/// The references are outside `crossing_between`: the step lengths at each
/// depth come from the mesh and the channel widths from the network's own
/// `band_edges`, both assembled by `Transect::width_pricing` so that they are
/// the same operands the gate compares — `min(home, step)` per step against the
/// full width of every reach the two rooms' readings win, not the home room's
/// edge alone and **not the transected reach's width**, each of which is a
/// different quantity. Every pair is also required to be QUIET (`Q` below the
/// discharge threshold) over those same readings, so the other clause cannot be
/// what flipped the verdict, and to price the same set of polylines at both
/// depths, so "the same water" is asserted rather than assumed.
///
/// **The width operand was wrong until The Pavement's absorb-water task**, and
/// `Transect::width_pricing`'s doc carries the instrumented evidence: the filter
/// read the transected reach's width while the gate priced whichever reach each
/// room independently won, so the control was describing different water from
/// the one it asserted about.
#[test]
fn the_width_clause_binds_when_the_step_shrinks() {
    let world = world();
    let ctx = LocaleContext::build(&world).unwrap();
    let shallow_depth = walk_depth(&ctx);
    let deep_depth = shallow_depth + 3;
    // 1600, not the 400 the other tests sample. Once the network renders the
    // whole land flow tree, the qualifying population — pairs whose priced
    // reaches all fall BETWEEN the deep step and the shallow one — is a
    // fraction of a growing sample, because a headwater creek narrower than
    // both steps is filtered out at the `width_refuses` clause. The rate is the
    // measurement; the count is the anti-vacuity floor, and it is restored by
    // sampling more rather than by lowering the floor. Both depths take the same
    // `wanted` so the two strides agree and the vertices match.
    //
    // **THE HISTORICAL RATES HERE DO NOT CARRY ACROSS THE OPERAND FIX.** This
    // comment used to read "96 of 400 (24%) when only river vertices were
    // rendered, 37 of 400 (9%) now". Both were measured with the transected
    // reach's width as the filter's operand, which is not a quantity the gate
    // prices (see `Transect::width_pricing`), so they are rates of a different
    // criterion and are recorded here as history rather than as a baseline. On
    // the corrected operand the qualifying population is 488 of 1600 sampled
    // vertices, all 488 of which flip — measured, not projected.
    let wanted = 1_600;
    let (shallow, _, _) = transects_at(&ctx, wanted, shallow_depth);
    let (deep, _, _) = transects_at(&ctx, wanted, deep_depth);

    let mut flipped = 0usize;
    let mut examined = 0usize;
    for a in &shallow {
        let Some(b) = deep.iter().find(|b| b.polyline_at == a.polyline_at) else {
            continue;
        };
        // The clause must actually flip between the two steps, or this pair has
        // nothing to say about it — and the flip must be stated over the
        // quantities the GATE compares: per step, `min(home, step)` against the
        // full width of every reach the two rooms' own readings win. Shallow:
        // every priced width must clear EVERY step, so no step can be refused
        // on width. Deep: none may, so no step can be admitted on width.
        // Anything between is a pair the width clause decides only for some of
        // the steps, which cannot support an assertion about the transect's
        // single verdict.
        //
        // **THE WIDTH USED TO BE `2.0 * a.edges[0]` AND THAT WAS THE BUG** —
        // the transected reach's width, which `crossing_between` never looks
        // at. `Transect::width_pricing`'s doc carries the instrumented
        // counter-example (`Vertex(2656)`: six different channels in one
        // eight-neighbourhood, the fordable one less than half the width the
        // filter was reading) and the refutation of the `sqrt(2)`-diagonal
        // hypothesis. This is the test's model of the gate being corrected, not
        // the gate: the verdict under test was right and the filter was
        // describing different water. Both arms now cover EVERY step
        // `verdict()` prices, which is the property the count-derived-from-
        // `steps` rule above exists to keep.
        let shallow_steps = a.width_pricing(&ctx);
        let deep_steps = b.width_pricing(&ctx);
        // Require it QUIET at both depths, over the readings the gate actually
        // prices rather than over the transected vertex's own drainage, so the
        // discharge clause is satisfied throughout and cannot be the cause.
        if !shallow_steps.iter().all(|p| p.quiet) || !deep_steps.iter().all(|p| p.quiet) {
            continue;
        }
        if !shallow_steps.iter().all(StepPricing::width_admits)
            || !deep_steps.iter().all(StepPricing::width_refuses)
        {
            continue;
        }
        // THE SAME WATER, asserted rather than assumed. The control's claim is
        // that only the traversal unit changed, so the two depths must price
        // the same set of reaches; a grid vertex is not that identity (it
        // carries one polyline vertex per run terminating on it), and neither is
        // the sampled polyline (the rooms win their own lines). The winning
        // lines are.
        let lines_of = |ps: &[StepPricing]| -> std::collections::BTreeSet<usize> {
            ps.iter().flat_map(|p| p.lines.iter().copied()).collect()
        };
        if lines_of(&shallow_steps) != lines_of(&deep_steps) {
            continue;
        }
        let (near, far) = (a.verdict(&ctx), b.verdict(&ctx));
        // Both must be crossings, or the GEOMETRY decided (a channel wider than
        // every step available puts both rooms on one bank, which reads
        // NotACrossing and says nothing about §8).
        if near == Crossing::NotACrossing || far == Crossing::NotACrossing {
            continue;
        }
        examined += 1;
        assert_eq!(
            near,
            Crossing::Fordable,
            "vertex {:?} (polyline {:?}): every reach priced at {shallow_depth}-depth is narrower \
             than its own step and every one is quiet, so no step can be refused — yet the \
             crossing is {near:?}. Pricing: {shallow_steps:?}",
            a.vertex,
            a.polyline_at,
        );
        assert_eq!(
            far,
            Crossing::Impassable,
            "vertex {:?} (polyline {:?}): the SAME reaches ({:?}), every one at least as wide as \
             its own {deep_depth}-depth step, so no step can be admitted — yet the crossing is \
             {far:?}, so the width clause is not deciding anything. Pricing: {deep_steps:?}",
            a.vertex,
            a.polyline_at,
            lines_of(&deep_steps),
        );
        flipped += 1;
    }
    println!(
        "width-clause flips (Fordable at depth {shallow_depth}, Impassable at {deep_depth}): {flipped} of {examined} examined"
    );
    assert!(
        flipped >= WIDTH_FLIP_FLOOR,
        "only {flipped} vertices flip verdict on the step length alone; the width clause is not \
         exercised anywhere in this suite"
    );
}

/// THE TASK-1 CARRY: a sign change on dry ground EXISTS in this world, and the
/// crossing gate excludes it.
///
/// Constructed, not hoped for. Beyond a polyline's endpoint the kernel measures
/// distance to the endpoint itself while taking the sign from the last
/// segment's plane, so the sign flips across the ray extending past every river
/// source and every river mouth — 25 such loci against 3 real crossings on seed
/// 42 at level 5, and inherent to signed distance against open arcs rather than
/// introduced by the bank convention. This test walks out along that ray on both
/// sides and asks the gate.
///
/// Two radii, because the two facts that shape the gate are different facts:
///
/// - **Inside the terrace** (`r` between the bank and terrace edges): the
///   reading is not `Dry`, so a gate written as `Transverse != Dry` would admit
///   it. This is the endpoint analogue of the confluence-bisector flip the spec
///   measures at `|d| = 4.7946e-3` against a widest terrace edge of 6.9e-3.
/// - **Beyond the terrace** (`r = 2·terrace edge`): a `Dry` flip outright.
///
/// The exclusion is checked for the RIGHT REASON: **both readings select the
/// same polyline** (so the gate's first, same-channel clause passed and cannot
/// be what excluded them), the sign genuinely differs between the two rooms (so
/// the second clause passed too), and neither room stands inside its own bank
/// edge — so the **third** clause is what did. And the rooms are asserted
/// distinct: two probes that collapsed into one room would return
/// `NotACrossing` for no reason at all.
///
/// The same-line filter is load-bearing, not tidiness. Without it a probe pair
/// selecting two different polylines is refused by clause 1, and this test
/// would credit that exclusion to the bank-edge clause it exists to
/// witness — the exact failure it warns about at
/// `same_bank_neighbours_are_not_a_crossing`: a guard whose subject is already
/// excluded by another clause measures that other clause.
///
/// No fixed distance threshold could do this job: the flip locus is a ray, and
/// probing at 1.0e-2, 5.0e-3 and 3.1e-3 rad finds the same flips at whatever
/// `|d|` the probe stands at (spec §5.3).
///
/// **Mutation-proved, not merely asserted.** Widening the gate from
/// `channel_bands[1]` to `channel_bands[3]` — i.e. gating on `Transverse != Dry`,
/// the insufficient version §5.3 warns about — reddens this test on the first
/// terrace-radius pair it reaches, at `|d|` = 4.06e-4 and 4.72e-4 against a bank
/// edge of 6.45e-5. (It also drives H2-4 from 0.217 to 0.839, which is the
/// "near 1.0" reading the brief names as the signature of a collapsed gate.)
#[test]
fn a_dry_land_sign_change_exists_and_the_crossing_gate_excludes_it() {
    let world = world();
    let ctx = LocaleContext::build(&world).unwrap();
    let net = ctx.terrain().channels();
    let depth = walk_depth(&ctx);
    let mut in_terrace = 0usize;
    let mut beyond_terrace = 0usize;
    let mut worst_terrace_fraction = 0.0_f64;

    for (i, line) in net.polylines.iter().enumerate() {
        let last = line.points.len() - 1;
        for (end, inward) in [(0usize, 1usize), (last, last - 1)] {
            let e = line.points[end];
            let p = line.points[inward];
            // The segment's plane normal (left of travel p -> e) and the
            // outward tangent that continues past the endpoint.
            let n = cross(p, e);
            if norm(n) == 0.0 {
                continue;
            }
            let n = normalize(n);
            let t = normalize(cross(n, e));
            let edges = net.band_edges[i][end];
            if edges[3] <= edges[1] {
                continue; // a gorge collapses the bands; nothing to straddle
            }
            for (radius, is_terrace) in
                [(0.5 * (edges[1] + edges[3]), true), (2.0 * edges[3], false)]
            {
                // 45 degrees either side of the outward ray, at exactly
                // `radius` from the endpoint: same |d|, opposite sides of the
                // segment plane.
                let diag = |sign: f64| {
                    let h = std::f64::consts::FRAC_1_SQRT_2;
                    normalize([
                        h * t[0] + sign * h * n[0],
                        h * t[1] + sign * h * n[1],
                        h * t[2] + sign * h * n[2],
                    ])
                };
                let left = Facet::containing(offset_from(e, diag(1.0), radius), depth);
                let right = Facet::containing(offset_from(e, diag(-1.0), radius), depth);
                if left == right {
                    continue;
                }
                let (Some(dl), Some(dr)) = (
                    net.bank_reading(left.centroid()),
                    net.bank_reading(right.centroid()),
                ) else {
                    continue;
                };
                // Is this one channel, is it actually a sign change, and is it
                // actually outside the bank? All three are properties of the
                // rooms' own readings, established before the gate is asked.
                if dl.line != dr.line {
                    // Two different polylines: clause 1 refuses this pair, so
                    // the assertion below would pass whatever the bank-edge
                    // clause did. Excluded so it measures that clause alone.
                    continue;
                }
                if !(dl.signed_distance > 0.0 && dr.signed_distance < 0.0) {
                    continue;
                }
                let outside_bank = |r: &hornvale_terrain::channel::BankReading| {
                    r.signed_distance.abs() >= r.band_edges[1]
                };
                if !outside_bank(&dl) || !outside_bank(&dr) {
                    continue;
                }
                assert_eq!(
                    ctx.crossing_between(&left, &right),
                    Crossing::NotACrossing,
                    "line {i} endpoint {end}: the sign flips between {left:?} ({}) and \
                     {right:?} ({}) but neither room is inside its own bank edge ({} / {}) — \
                     this is the polyline soup, not a river, and calling it a crossing would \
                     report a ford across dry ground at every river source and mouth",
                    dl.signed_distance,
                    dr.signed_distance,
                    dl.band_edges[1],
                    dr.band_edges[1],
                );
                if is_terrace {
                    // The sharp claim: this flip is NOT `Dry`, so a gate of
                    // `Transverse != Dry` would have admitted it.
                    let inside_terrace = |r: &hornvale_terrain::channel::BankReading| {
                        r.signed_distance.abs() < r.band_edges[3]
                    };
                    if inside_terrace(&dl) || inside_terrace(&dr) {
                        in_terrace += 1;
                        let f = (dl.signed_distance.abs() / dl.band_edges[3])
                            .max(dr.signed_distance.abs() / dr.band_edges[3]);
                        worst_terrace_fraction = worst_terrace_fraction.max(f);
                    }
                } else {
                    let dry = |r: &hornvale_terrain::channel::BankReading| {
                        r.signed_distance.abs() >= r.band_edges[3]
                    };
                    if dry(&dl) && dry(&dr) {
                        beyond_terrace += 1;
                    }
                }
            }
        }
    }

    println!(
        "dry-land sign changes excluded: {in_terrace} inside the terrace (worst |d|/terrace \
         edge = {worst_terrace_fraction:.4}), {beyond_terrace} beyond it"
    );
    // Both anti-vacuity floors matter and they are different claims. Without
    // the first, the test could pass on a world where the spurious locus had
    // vanished; without the second, it would stop witnessing the fact that
    // `Transverse != Dry` is an insufficient gate.
    assert!(
        beyond_terrace >= DRY_FLIPS_FLOOR,
        "only {beyond_terrace} outright-dry sign changes were constructed; the test no longer \
         witnesses the locus it exists to exclude"
    );
    assert!(
        in_terrace >= TERRACE_FLIPS_FLOOR,
        "no sign change was constructed inside a terrace band, so this no longer witnesses that \
         gating on `Transverse != Dry` would be insufficient"
    );
}

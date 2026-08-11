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

use hornvale_kernel::{CellId, RoomAddr, Seed, World, WorldTime, math};
use hornvale_locale::{Crossing, LocaleContext, ROOM_SCHEMA, room_edge};
use hornvale_terrain::carve::WATERFALL_MIN_DRAINAGE;
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

// ---------------------------------------------------------------------------
// Task 3 — crossing is a traversal query.
//
// Everything below asks about a PAIR of rooms. The references:
//
// - The transect population comes from the network's own vertices and the room
//   mesh's own edge length, neither of which knows what `crossing_between`
//   decides — so the denominator exists independently of the criterion. That is
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
/// Measured: 460 same-bank adjacent pairs with a room inside its own bank edge,
/// 130 outright-dry constructed sign changes, 127 constructed inside a terrace
/// band. The floors sit near half of each — terrain drift may move a river, but
/// it will not halve the network.
const SAME_BANK_PAIRS_FLOOR: usize = 200;
/// See [`SAME_BANK_PAIRS_FLOOR`]. Measured 130.
const DRY_FLIPS_FLOOR: usize = 60;
/// See [`SAME_BANK_PAIRS_FLOOR`]. Measured 127.
const TERRACE_FLIPS_FLOOR: usize = 60;

/// Walk depth: six refinement levels below the canonical grid.
///
/// This is `hornvale_vessel::walk_depth`'s definition restated, not a fresh
/// choice — `windows/vessel` depends on this crate, so the dependency cannot
/// run the other way — and it is what `hornvale locale` and `hornvale possess`
/// default to.
fn walk_depth(ctx: &LocaleContext) -> u32 {
    ctx.globe_level() + 6
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

/// One transect of the channel network: the two rooms a walker stands in
/// either side of one channel vertex, one **step** apart, plus the reach that
/// vertex belongs to.
struct Transect {
    /// The room on the +normal side of the local travel direction.
    left: RoomAddr,
    /// The room on the -normal side.
    right: RoomAddr,
    /// The transected vertex's own band edges (channel/bank, bank/floodplain,
    /// floodplain/terrace, terrace/dry).
    edges: [f64; 4],
    /// The cell that vertex was placed from — this reach's discharge.
    cell: CellId,
}

/// Why a sampled vertex yielded no usable transect. Counted and reported, never
/// silently dropped: the vertices hardest to pair are the widest channels,
/// which are exactly the ones §8's criterion calls Impassable, so a silent drop
/// inflates the fordable fraction for a reason that has nothing to do with the
/// world.
#[derive(Default, Debug)]
struct Drops {
    /// Coincident vertices: no local travel direction, so no transect normal.
    /// Meander displacement can collapse adjacent vertices, so this is
    /// reachable rather than theoretical.
    degenerate: usize,
    /// Both probes landed in the SAME room, so there is no pair to ask about.
    /// This is a relation between the probe separation and the ROOM, not the
    /// channel — which is why the separation is a room edge and not a fixed
    /// angle. Measured: 0 at the shipped separation, 80 of 681 at half it.
    same_room: usize,
}

/// Transects of the network, one per sampled vertex, at most `wanted` of them.
///
/// **Strided, never truncated.** Taking the first `wanted` vertices in polyline
/// order would sample only the lowest-`CellId` rivers; `windows/lab`'s
/// `lab_band_transects` derives a stride from the same cap for exactly that
/// reason, and this follows it. Deterministic in order and count; makes no
/// draws.
///
/// **The pair straddles the vertex one room edge apart** — each probe half a
/// room edge out, read off the mesh through [`room_edge`] at the room
/// containing the vertex. One room edge is a walker's step, which is both
/// §8's own unit and the only separation at which asking "does this STEP cross
/// the channel" is a question about a step at all. The separation is
/// mesh-derived rather than a fixed angle because a fixed angle is a length
/// scale in disguise, and because the drop that actually bites — both probes in
/// one room — is a relation between the separation and the room.
fn network_transects(ctx: &LocaleContext, wanted: usize) -> (Vec<Transect>, Drops, usize) {
    transects_at_separation(ctx, wanted, 1.0)
}

/// [`network_transects`] with the pair separation scaled by `room_edges` — the
/// sensitivity handle, and the reason the H2-4 test prints a sweep of it.
fn transects_at_separation(
    ctx: &LocaleContext,
    wanted: usize,
    room_edges: f64,
) -> (Vec<Transect>, Drops, usize) {
    let net = ctx.terrain().channels();
    let depth = walk_depth(ctx);
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
            let base = line.points[j];
            // Any neighbouring vertex gives the local travel direction; at an
            // end there is only one, and a transect does not care which way
            // along the channel it points, only that it is perpendicular.
            let along = if j + 1 < line.points.len() {
                line.points[j + 1]
            } else {
                line.points[j - 1]
            };
            let travel = [along[0] - base[0], along[1] - base[1], along[2] - base[2]];
            let side = cross(base, travel);
            if norm(side) == 0.0 {
                drops.degenerate += 1;
                continue;
            }
            let side = normalize(side);
            let half = 0.5 * room_edges * room_edge(&RoomAddr::containing(base, depth));
            let left = RoomAddr::containing(offset_from(base, side, half), depth);
            let right = RoomAddr::containing(offset_from(base, side, -half), depth);
            if left == right {
                drops.same_room += 1;
                continue;
            }
            out.push(Transect {
                left,
                right,
                edges: net.band_edges[i][j],
                cell: net.run_cells[i][j],
            });
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
/// one of its three mesh neighbours reading the same sign. The band clause is
/// satisfied by construction, the two rooms are edge-adjacent — a real step —
/// and the only thing left that can return `NotACrossing` is the sign. Deleting
/// the sign clause reddens this test on all 460 pairs.
///
/// The reference for "same bank" is the rooms' own signed distances from
/// `bank_reading`, read before `crossing_between` is asked anything.
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
            let room = RoomAddr::containing(vertex, depth);
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
/// The denominator is TRANSECTS OF THE NETWORK, deliberately: a population of
/// "adjacent pairs whose sign differs" would BE the ford set by construction (a
/// one-step sign change is only possible when the channel is narrower than one
/// step, which is §8's criterion itself), and the fraction would read ~1.0 no
/// matter what the world looked like.
///
/// `frac = |{v : Fordable(transect(v))}| / |V'|`, where `V` is the sampled
/// vertices and `V'` the ones that yielded a usable pair. Every dropped vertex
/// is counted with its reason and printed; §8's two clauses are reported
/// separately, because a number near the interval's floor is unattributable
/// without them.
///
/// # What the printed attribution says, measured on seed 42 at level 6
///
/// - **The width clause never binds here.** The widest full channel in the
///   world is 1.89e-4 rad against a room edge of 2.71e-4, so `2·b0 < step`
///   holds at **every** vertex. All of §8's discrimination at walk depth is
///   done by its discharge half (`Q < 80`, which 5.0% of sampled vertices
///   fail — the network's discharge runs 15 to 146, median 24). A
///   criterion whose first clause is inert on the world it was frozen for is
///   worth knowing about; it is not inert in principle — a coarser walk depth
///   or a wetter world engages it.
/// - **The conjunction is not the fraction.** The clauses hold on 95.0% of
///   transects while 21.7% are Fordable; the gap is the crossing gate itself —
///   321 of 341 transects show a sign change and 237 of those have neither room
///   inside its own bank edge — not the criterion.
/// - **The reading is separation-dependent, and the sweep is printed so nobody
///   has to rediscover that.** The bank edge is *sub-room* at walk depth —
///   median `b1` = 7.8e-5 rad, 0.29 of a room edge — so whether a pair straddling
///   a channel has either room inside its own bank band depends on how far apart
///   the pair stands. Measured: 0.2133 at half a room edge (with 41 of 341
///   pairs collapsing into one room), **0.2170 at one room edge**, 0.0469 at
///   two, 0.0000 at four — by four both rooms are outside every bank in the
///   world and the gate correctly refuses to call it a crossing.
///   **H2-4 is therefore not a property of the world alone** — it is a property
///   of the world at a stated step length, and the step length has to be the
///   walker's or the number means nothing.
///
/// **Mutation-proved.** Widening the band gate to `Transverse != Dry` drives
/// this fraction from 0.2170 to 0.8387 — the "near 1.0" reading that means the
/// denominator has collapsed back into the criterion. Deleting the sign clause
/// instead leaves it green, which is correct: every transect here straddles the
/// channel, so the sign clause is not what this number measures.
///
/// **This is a late freeze, not a preregistration** (spec §8): the criterion was
/// chosen with stage-1 measurements in hand, so the interval carries less
/// evidential weight than a preregistered one. The number is reported as
/// measured either way — nothing here is tuned to land inside.
#[test]
fn the_fordable_fraction_of_the_network_is_within_its_interval() {
    let world = world();
    let ctx = LocaleContext::build(&world).unwrap();
    let (transects, drops, sampled) = network_transects(&ctx, 400);

    let mut fordable = 0usize;
    let mut impassable = 0usize;
    let mut not_a_crossing = 0usize;
    let mut sign_change = 0usize;
    let mut band_gated = 0usize;
    let mut width_clause = 0usize;
    let mut drainage_clause = 0usize;
    let mut both_clauses = 0usize;
    for t in &transects {
        let crossing = ctx.crossing_between(&t.left, &t.right);
        // Symmetry, asserted here rather than in its own test so it is checked
        // over the whole real population without paying for a second world.
        assert_eq!(
            crossing,
            ctx.crossing_between(&t.right, &t.left),
            "crossing_between is not symmetric at {:?}/{:?}",
            t.left,
            t.right
        );
        match crossing {
            Crossing::Fordable => fordable += 1,
            Crossing::Impassable => impassable += 1,
            Crossing::NotACrossing => not_a_crossing += 1,
        }
        // Where a NotACrossing came from: no sign change at all, or a sign
        // change with neither room inside its own bank edge. Read off the
        // rooms' own readings, not off the gate's internals.
        let net = ctx.terrain().channels();
        if let (Some(dl), Some(dr)) = (
            net.bank_reading(t.left.centroid()),
            net.bank_reading(t.right.centroid()),
        ) && (dl.signed_distance > 0.0) != (dr.signed_distance > 0.0)
        {
            sign_change += 1;
            if dl.signed_distance.abs() >= dl.band_edges[1]
                && dr.signed_distance.abs() >= dr.band_edges[1]
            {
                band_gated += 1;
            }
        }
        // The two clauses of §8, evaluated on the reach BEING transected (the
        // vertex's own edges and cell) rather than on whichever reading the
        // rooms happened to win. Reported, never asserted on: these attribute
        // the fraction above, they do not define it.
        let step = room_edge(&t.left).min(room_edge(&t.right));
        let narrow = 2.0 * t.edges[0] < step;
        let quiet = ctx.terrain().drainage_at(t.cell) < WATERFALL_MIN_DRAINAGE;
        width_clause += usize::from(narrow);
        drainage_clause += usize::from(quiet);
        both_clauses += usize::from(narrow && quiet);
    }

    let usable = transects.len();
    let frac = fordable as f64 / usable as f64;
    println!(
        "H2-4 (seed 42, level {}, walk depth {}):\n  \
         |V| sampled vertices      = {sampled}\n  \
         |V'| usable transects     = {usable}\n  \
         dropped: same room        = {}\n  \
         dropped: degenerate       = {}\n  \
         Fordable                  = {fordable}\n  \
         Impassable                = {impassable}\n  \
         NotACrossing              = {not_a_crossing}  (of which {band_gated} had a sign \
         change but no room inside its own bank edge; {sign_change} sign changes in all)\n  \
         fordable fraction         = {frac:.4}  (of |V'|)\n  \
         width clause  (2*b0<step) = {width_clause} ({:.4})\n  \
         drainage clause (Q<{WATERFALL_MIN_DRAINAGE}) = {drainage_clause} ({:.4})\n  \
         conjunction               = {both_clauses} ({:.4})",
        ctx.globe_level(),
        walk_depth(&ctx),
        drops.same_room,
        drops.degenerate,
        width_clause as f64 / usable as f64,
        drainage_clause as f64 / usable as f64,
        both_clauses as f64 / usable as f64,
    );

    // The separation sensitivity, printed rather than asserted. The shipped
    // instrument is the 1.0 row; the others are here so a reader can see that
    // the fraction is a reading at a stated step length and not a constant of
    // the world.
    for room_edges in [0.5_f64, 1.0, 2.0, 4.0] {
        let (ts, dr, seen) = transects_at_separation(&ctx, 400, room_edges);
        let f = ts
            .iter()
            .filter(|t| ctx.crossing_between(&t.left, &t.right) == Crossing::Fordable)
            .count();
        println!(
            "  separation {room_edges:>4} room edges: fordable {f}/{} of {seen} sampled \
             ({:.4}), drops {dr:?}",
            ts.len(),
            f as f64 / ts.len() as f64
        );
    }

    // Anti-vacuity: a fraction over a handful of transects is an anecdote, and
    // a denominator that collapsed would make any fraction reachable. Measured
    // |V'| = 341 of 341 sampled (681 vertices, stride 2).
    assert!(
        usable >= 200,
        "only {usable} usable transects of {sampled} sampled ({drops:?}); the fraction is an \
         anecdote"
    );
    assert!(
        (0.10..=0.70).contains(&frac),
        "fordable fraction {frac} outside the spec's [0.10, 0.70] \
         ({fordable} of {usable} usable transects, {sampled} sampled, drops {drops:?}; \
         width clause {width_clause}, drainage clause {drainage_clause}, both {both_clauses})"
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
/// The exclusion is checked for the RIGHT REASON: the sign genuinely differs
/// between the two rooms (so the gate's first clause passed and cannot be what
/// excluded them), and neither room stands inside its own bank edge (so the
/// second clause is what did). And the rooms are asserted distinct — two probes
/// that collapsed into one room would return `NotACrossing` for no reason at
/// all.
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
                let left = RoomAddr::containing(offset_from(e, diag(1.0), radius), depth);
                let right = RoomAddr::containing(offset_from(e, diag(-1.0), radius), depth);
                if left == right {
                    continue;
                }
                let (Some(dl), Some(dr)) = (
                    net.bank_reading(left.centroid()),
                    net.bank_reading(right.centroid()),
                ) else {
                    continue;
                };
                // Is this actually a sign change, and is it actually outside
                // the bank? Both are properties of the rooms' own readings,
                // established before the gate is asked.
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

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
/// Measured: 460 same-bank adjacent pairs with a room inside its own bank edge,
/// 130 outright-dry constructed sign changes, 127 constructed inside a terrace
/// band. The floors sit near half of each — terrain drift may move a river, but
/// it will not halve the network.
const SAME_BANK_PAIRS_FLOOR: usize = 200;
/// See [`SAME_BANK_PAIRS_FLOOR`]. Measured 130.
const DRY_FLIPS_FLOOR: usize = 60;
/// See [`SAME_BANK_PAIRS_FLOOR`]. Measured 127.
const TERRACE_FLIPS_FLOOR: usize = 60;
/// Transects where a crossing exists at all, at walk depth. Measured 123 of 341.
const CROSSINGS_FLOOR: usize = 60;
/// Crossings whose reach carries at least `WATERFALL_MIN_DRAINAGE`. Measured 8
/// — the smallest population any assertion here runs on, and the reason the
/// ordering claim rather than this count is the primary reference.
const STRONG_CROSSINGS_FLOOR: usize = 4;
/// Vertices that flip verdict on the step length alone. Measured 98 of 98
/// examined — every pair that reaches the assertion flips, which is what a
/// clause deciding something looks like.
const WIDTH_FLIP_FLOOR: usize = 50;

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

/// One transect of the channel network: the room the centreline runs through,
/// and the **three mesh steps out of it**, plus the reach it belongs to.
///
/// The three steps are `RoomAddr::neighbors()`, so every pair this population
/// asks about is a walker's step **by construction** rather than by filtering.
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
    /// The polyline and vertex this transect was taken at — the identity two
    /// runs at different depths are matched on.
    vertex: (usize, usize),
    /// The room the centreline runs through: the transect's origin, and the
    /// one room guaranteed to be inside its own bands.
    home: RoomAddr,
    /// The three mesh steps out of `home`.
    steps: [RoomAddr; 3],
    /// The transected vertex's own band edges (channel/bank, bank/floodplain,
    /// floodplain/terrace, terrace/dry).
    edges: [f64; 4],
    /// The cell that vertex was placed from — this reach's discharge.
    cell: CellId,
}

impl Transect {
    /// What crossing the network the walker meets here: `Fordable` if any of
    /// the three steps out of the channel room can be waded, `Impassable` if
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
}

/// Why a sampled vertex yielded no usable transect. Counted and reported, never
/// silently dropped: the vertices hardest to pair are the widest channels,
/// which are exactly the ones §8's criterion calls Impassable, so a silent drop
/// inflates the fordable fraction for a reason that has nothing to do with the
/// world.
///
/// **The two drop causes the previous construction had are now structurally
/// absent** — a room always has three distinct mesh neighbours, so neither
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
/// order would sample only the lowest-`CellId` rivers; `windows/lab`'s
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
            let home = RoomAddr::containing(line.points[j], depth);
            if net.bank_reading(home.centroid()).is_none() {
                drops.no_reading += 1;
                continue;
            }
            out.push(Transect {
                vertex: (i, j),
                steps: home.neighbors(),
                home,
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
/// `Fordable` when at least one of the three mesh steps out of the channel room
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
/// - **The width clause is inert at walk depth.** The widest full channel in
///   the world is 1.89e-4 rad against a room edge of 2.71e-4, so `2·b0 < step`
///   holds at every sampled vertex and all the discrimination is done by the
///   discharge half (`Q < 80`; discharge runs 15 to 146, median 24). It is not
///   inert in principle — `the_width_clause_binds_when_the_step_shrinks` is the
///   positive control that exercises it at a depth where it does bind.
/// - **The conjunction is not the fraction.** The clauses hold on 95.0% of
///   transects (324 of 341) while 33.7% are Fordable and only 123 transects
///   have any crossing at all; the gap is the crossing gate itself — the sign
///   change and §5.3's requirement that a room stand inside its own bank edge —
///   not §8's criterion. Where the channel is wider than every step available,
///   both rooms sit on one bank and the verdict is `NotACrossing` rather than
///   `Impassable`: the geometry refuses before the criterion is consulted.
/// - **Measured, at walk depth: 115 Fordable, 8 Impassable, 218 NotACrossing of
///   341 — a fraction of 0.3372.** The step sweep: 0.2229 one level up (a
///   longer step reaches further but puts the walker's rooms outside the bank
///   band), 0.3372 at walk depth, 0.3959 one level down, 0.1026 two, 0.0000
///   three — by then no step spans the water.
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
/// 3. the transect was rebuilt around three mesh steps per vertex — **adopted
///    directly after that falsifying reading** — giving the **0.3372** above.
///
/// Every change was forced by a defect in the previous instrument rather than by
/// the number it produced, each is disclosed, and the quantifier and expected
/// band for (3) were written down before it ran. None of that restores the
/// epistemic status of a hypothesis whose free parameter moved four times under
/// unblinded observation.
///
/// **So: H2-4 was not tested under preregistration and is not resolved. 0.3372
/// at one room edge at walk depth is reportable as a measurement of the world at
/// a stated step length — not as a confirmation of the [0.10, 0.70] interval.**
/// The range check below is a **witness** that pins today's reading and reddens
/// if it moves; it is not a hypothesis test, and this test's name should be read
/// as naming the check rather than announcing a result.
#[test]
fn the_fordable_fraction_of_the_network_is_within_its_interval() {
    let world = world();
    let ctx = LocaleContext::build(&world).unwrap();
    let (transects, drops, sampled) = network_transects(&ctx, 400);

    let mut fordable = 0usize;
    let mut impassable = 0usize;
    let mut not_a_crossing = 0usize;
    let mut width_clause = 0usize;
    let mut drainage_clause = 0usize;
    let mut both_clauses = 0usize;
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
        }
        match verdict {
            Crossing::Fordable => fordable += 1,
            Crossing::Impassable => impassable += 1,
            Crossing::NotACrossing => not_a_crossing += 1,
        }
        // The two clauses of §8, evaluated on the reach BEING transected (the
        // vertex's own edges and cell) rather than on whichever reading a room
        // happened to win. Reported, never asserted on: these attribute the
        // fraction above, they do not define it.
        let step = room_edge(&t.home);
        let narrow = 2.0 * t.edges[0] < step;
        let quiet = ctx.terrain().drainage_at(t.cell) < WATERFALL_MIN_DRAINAGE;
        width_clause += usize::from(narrow);
        drainage_clause += usize::from(quiet);
        both_clauses += usize::from(narrow && quiet);
    }

    let usable = transects.len();
    let frac = fordable as f64 / usable as f64;
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
         conjunction               = {both_clauses} ({:.4})",
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
        let f = ts
            .iter()
            .filter(|t| t.verdict(&ctx) == Crossing::Fordable)
            .count();
        println!(
            "  depth {depth} (step {:e} rad): fordable {f}/{} of {seen} sampled ({:.4}), \
             drops {dr:?}",
            room_edge(&ts[0].home),
            ts.len(),
            f as f64 / ts.len() as f64
        );
    }

    // Anti-vacuity: a fraction over a handful of transects is an anecdote, and
    // a denominator that collapsed would make any fraction reachable. Measured
    // |V'| = 341 of 341 sampled (681 vertices, stride 2), no drops.
    assert!(
        usable >= 200,
        "only {usable} usable transects of {sampled} sampled ({drops:?}); the fraction is an \
         anecdote"
    );
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
/// everywhere, so deleting `drainage_at(cell) < WATERFALL_MIN_DRAINAGE`
/// promotes every `Impassable` transect to `Fordable` and moves H2-4's fraction
/// by less than the interval's width — the suite stays green while half the
/// task's named deliverable is gone.
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
    let (transects, _, _) = network_transects(&ctx, 400);

    // Every transect where a crossing exists, paired with the discharge of the
    // reach it transects. `total_cmp` with an index tie-break, so the extremes
    // are deterministic.
    let mut crossings: Vec<(f64, usize, Crossing)> = transects
        .iter()
        .enumerate()
        .filter_map(|(k, t)| {
            let verdict = t.verdict(&ctx);
            (verdict != Crossing::NotACrossing)
                .then(|| (ctx.terrain().drainage_at(t.cell), k, verdict))
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
            && ctx.terrain().drainage_at(t.cell) >= WATERFALL_MIN_DRAINAGE
        {
            assert_eq!(
                verdict,
                Crossing::Impassable,
                "a crossing at Q={} reads Fordable",
                ctx.terrain().drainage_at(t.cell)
            );
            strong += 1;
        }
    }
    println!("crossings above the discharge threshold: {strong}");
    assert!(
        strong >= STRONG_CROSSINGS_FLOOR,
        "only {strong} crossings carry enough water to be refused; the clause is barely exercised"
    );
}

/// §8's WIDTH CLAUSE, POSITIVE CONTROL — at a depth where it binds.
///
/// At walk depth the clause is inert on seed 42: the widest full channel is
/// 1.89e-4 rad against a 2.71e-4 room edge, so `2·b0 < step` holds at every
/// vertex and deleting the clause changes nothing. That is a fact about this
/// world at this depth, not about the criterion — and an unexercised clause is
/// an unguarded one.
///
/// A room edge halves with each level, so three levels below walk depth the
/// step is 3.4e-5 rad against a median full width of 7.8e-5 and the clause
/// binds. The control is the pair: **the same vertex, the same water, the same
/// discharge — Fordable at walk depth, Impassable three levels down.** Only the
/// traversal unit changed, which is precisely what §8 claims to measure.
///
/// The references are outside `crossing_between`: the room edge at each depth
/// comes from the mesh (`room_edge`), the channel width from the network's own
/// `band_edges`, and every pair is required to be QUIET (`Q` below the
/// discharge threshold) so the other clause cannot be what flipped the verdict.
#[test]
fn the_width_clause_binds_when_the_step_shrinks() {
    let world = world();
    let ctx = LocaleContext::build(&world).unwrap();
    let shallow_depth = walk_depth(&ctx);
    let deep_depth = shallow_depth + 3;
    let (shallow, _, _) = transects_at(&ctx, 400, shallow_depth);
    let (deep, _, _) = transects_at(&ctx, 400, deep_depth);

    let mut flipped = 0usize;
    let mut examined = 0usize;
    for a in &shallow {
        let Some(b) = deep.iter().find(|b| b.vertex == a.vertex) else {
            continue;
        };
        // Same reach, so same discharge; require it QUIET, so the discharge
        // clause is satisfied at both depths and cannot be the cause.
        if ctx.terrain().drainage_at(a.cell) >= WATERFALL_MIN_DRAINAGE {
            continue;
        }
        let wide = 2.0 * a.edges[0];
        // The clause must actually flip between the two steps, or this pair has
        // nothing to say about it.
        if !(wide < room_edge(&a.home) && wide >= room_edge(&b.home)) {
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
            "vertex {:?}: full width {wide:e} is below the {shallow_depth}-depth step {:e} and \
             the water is quiet, yet the crossing is {near:?}",
            a.vertex,
            room_edge(&a.home)
        );
        assert_eq!(
            far,
            Crossing::Impassable,
            "vertex {:?}: the SAME water, full width {wide:e}, is wider than the {deep_depth}-\
             depth step {:e} — yet the crossing is {far:?}, so the width clause is not deciding \
             anything",
            a.vertex,
            room_edge(&b.home)
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

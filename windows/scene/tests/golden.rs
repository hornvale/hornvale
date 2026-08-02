//! The scene/tiles/v1 byte pin: this fixture changing is the epoch
//! decision point (scene-protocol spec §2). Regenerate deliberately, never
//! casually: `REBASELINE=1 cargo test -p hornvale-scene --test golden`
//! (or `make rebaseline-goldens`) rewrites it, then review the diff as a
//! contract change.

use hornvale_scene::{
    TileFields, region_json, render_surrounds_ascii, scene_json, scene_json_selected,
    surrounds_json, surrounds_scene, tiles_region_scene, tiles_scene,
};

// Integration tests can't see #[cfg(test)] helpers, and the public API
// takes no test scaffolding — this 10-line duplicate of the lib tests'
// builder is the cheaper price.
fn world() -> hornvale_kernel::World {
    hornvale_worldgen::build_world(
        hornvale_kernel::Seed(1),
        &Default::default(),
        hornvale_worldgen::SkyChoice::Constant,
        &Default::default(),
        &Default::default(),
    )
    .expect("seed 1 builds")
}

fn seed_1_json() -> String {
    scene_json(&tiles_scene(&world(), 16).unwrap())
}

#[test]
fn v1_bytes_are_pinned() {
    hornvale_kernel::golden::assert_golden(
        std::path::Path::new(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/tests/fixtures/tiles-seed-1-w16.json"
        )),
        &seed_1_json(),
        "scene/tiles/v1 bytes moved — this is the epoch decision point (scene-protocol \
         spec §2); accept deliberately and review the diff as a contract change",
    );
}

// The projection's own pin (The Winnowing; scene-protocol spec §3 item 4).
// The two lib tests either side of it check the projection at `all()` and
// check that an EMITTED field's bytes are unchanged — neither can observe an
// OVER-emission, so with only those a `TileFields::contains` that always
// returned `true` passed the whole suite while defeating the feature. What
// needs pinning is the composed shape: braces, commas, and key order with
// holes in it. A golden does that as bytes rather than as reasoning.
//
// The five selected layers are chosen to put a hole everywhere a hole can go:
// `plate`/`unrest` dropped between the metadata `biome_legend` and `features`,
// the four temperature/current layers dropped before `season_period_days`,
// `moisture` dropped between `circulation_bands` and `locked`, and `drainage`
// dropped between `water_legend` and the trailing `waterfalls`. So the fixture
// covers a hole at the head, in the middle, adjacent to metadata on both
// sides, and at the tail. Seed 1 at width 16 is 128 tiles — gate-cheap.
const PINNED_FIELDS: &[&str] = &["elevation_m", "ocean", "biome", "t_mean_c", "water"];

fn projected_seed_1_json() -> String {
    let scene = tiles_scene(&world(), 16).unwrap();
    let fields = TileFields::only(PINNED_FIELDS).expect("the pinned names are all known layers");
    scene_json_selected(&scene, &fields)
}

#[test]
fn projected_v1_bytes_are_pinned() {
    hornvale_kernel::golden::assert_golden(
        std::path::Path::new(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/tests/fixtures/tiles-seed-1-w16-projected.json"
        )),
        &projected_seed_1_json(),
        "the projected scene/tiles/v1 bytes moved — this pins the composed shape of a \
         partial document (key order with holes in it); if the unselected layers came \
         BACK, the projection has stopped projecting. Accept deliberately, re-run with \
         REBASELINE=1, and review the diff as a contract change",
    );
}

// The golden above would catch an over-emission, but only as "bytes moved".
// This says the property out loud and names the offending layer, so a failure
// is diagnosable without reading a fixture diff.
#[test]
fn the_projection_omits_unselected_layers() {
    let doc = projected_seed_1_json();
    for name in TileFields::ALL_NAMES {
        let key = format!("\"{name}\":");
        let selected = PINNED_FIELDS.contains(name);
        assert_eq!(
            doc.contains(&key),
            selected,
            "layer {name} is {} in the projected document but {} in the selection",
            if doc.contains(&key) {
                "present"
            } else {
                "absent"
            },
            if selected { "selected" } else { "unselected" }
        );
    }
    // Document metadata is never selectable and must survive the projection.
    for key in [
        "\"schema\":",
        "\"seed\":",
        "\"width\":",
        "\"height\":",
        "\"sea_level_m\":",
        "\"biome_legend\":",
        "\"features\":",
        "\"season_period_days\":",
        "\"locked\":",
        "\"water_legend\":",
        "\"waterfalls\":",
    ] {
        assert!(
            doc.contains(key),
            "metadata {key} was dropped by projection"
        );
    }
    // And the projection must actually save bytes — the point of the campaign.
    let full = scene_json(&tiles_scene(&world(), 16).unwrap());
    assert!(
        doc.len() < full.len(),
        "the projected document ({} bytes) is not smaller than the full one ({} bytes)",
        doc.len(),
        full.len()
    );
}

// scene/tiles-region/v1's own pin, the third sibling. Until this existed the
// region path's only in-repo byte evidence was the *mutual* equivalence test
// in the lib (`the_context_path_is_byte_identical_to_the_world_path`), which
// would pass if both paths moved together — an absolute pin is what makes a
// context refactor's byte-identity claim checkable at all. A seed-1 level-3
// patch at 8 samples is 81 nodes: small enough for the commit gate.
fn region_seed_1_json() -> String {
    region_json(&tiles_region_scene(&world(), 0, 3, 0, 0, 8).unwrap())
}

#[test]
fn region_v1_bytes_are_pinned() {
    hornvale_kernel::golden::assert_golden(
        std::path::Path::new(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/tests/fixtures/region-seed-1-f0-l3.json"
        )),
        &region_seed_1_json(),
        "scene/tiles-region/v1 bytes moved — this is the region path's absolute byte pin \
         (the projection is shared byte-for-byte with the orrery's cubeSphere.ts); accept \
         deliberately, re-run with REBASELINE=1, and review the diff as a contract change",
    );
}

// scene/surrounds/v1's own pin, sibling to the tiles pin above. Field order
// is this schema's headline contract (decision 0055: it becomes cross-repo
// once another repo parses it), and until this test existed nothing defended
// it. Radius 4 on a fixed observer guarantees both the "here" cell and
// several "sensed" cells appear, so the pinned bytes give the schema's
// unasserted contract fields (`orientation`, the three legends, and a
// `"sensed"` cell) their first coverage.
fn surrounds_seed_1_json() -> String {
    let w = world();
    // A fixed, arbitrary observer — the equator at the prime meridian — at a
    // shallow depth; this pin only needs a stable address, not a specific
    // biome or a seam. Same lat/lon -> unit-sphere conversion as the lib
    // tests' `observer` helper.
    let observer = hornvale_kernel::RoomAddr::containing(
        hornvale_kernel::math::unit_sphere_from_lat_lon(0.0, 0.0),
        6,
    );
    surrounds_json(
        &surrounds_scene(&w, &observer, 4, hornvale_kernel::WorldTime { day: 0.0 }).unwrap(),
    )
}

#[test]
fn surrounds_v1_bytes_are_pinned() {
    hornvale_kernel::golden::assert_golden(
        std::path::Path::new(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/tests/fixtures/surrounds-seed-1.json"
        )),
        &surrounds_seed_1_json(),
        "scene/surrounds/v1 bytes moved — field order is this schema's headline contract \
         (decision 0055: it becomes cross-repo once another repo parses it); accept \
         deliberately and review the diff as a contract change",
    );
}

// The seed-1 pin above sits in deep ocean: every cell is a single-word
// biome, `marks` is empty everywhere, and the legend has only two entries —
// so it gives no byte coverage to a multi-word biome index or to `Mark`'s
// field order (`noun, kind, datum, salience`), despite field order being
// this schema's headline contract. This second pin sits at seed 42's
// flagship settlement observer instead: a verified 31-cell, all-land,
// radius-4 neighbourhood carrying a settlement mark and the multi-word
// biome "tropical seasonal forest", so both gaps get real byte coverage.
fn seed_42_world() -> hornvale_kernel::World {
    hornvale_worldgen::build_world(
        hornvale_kernel::Seed(42),
        &Default::default(),
        hornvale_worldgen::SkyChoice::Generated,
        &Default::default(),
        &Default::default(),
    )
    .expect("seed 42 builds")
}

// Integration tests can't reach the lib's `pub(crate) place_latlon` helper,
// so this duplicates its handful of lines against the public
// `hornvale_settlement` facts directly — same idiom as `world()`/
// `surrounds_seed_1_json()` above.
fn flagship_latlon(world: &hornvale_kernel::World) -> (f64, f64) {
    let v = hornvale_settlement::village_info(world).expect("seed 42 has a village");
    let lat = match world.ledger.value_of(v.id, hornvale_settlement::LATITUDE) {
        Some(hornvale_kernel::Value::Number(n)) => *n,
        _ => panic!("flagship settlement has no latitude fact"),
    };
    let lon = match world.ledger.value_of(v.id, hornvale_settlement::LONGITUDE) {
        Some(hornvale_kernel::Value::Number(n)) => *n,
        _ => panic!("flagship settlement has no longitude fact"),
    };
    (lat, lon)
}

fn surrounds_seed_42_flagship_json() -> String {
    let w = seed_42_world();
    let ctx = hornvale_locale::LocaleContext::build(&w).unwrap();
    let depth = ctx.globe_level() + 6;
    let (lat, lon) = flagship_latlon(&w);
    let observer = hornvale_kernel::RoomAddr::containing(
        hornvale_kernel::math::unit_sphere_from_lat_lon(lat, lon),
        depth,
    );
    surrounds_json(
        &surrounds_scene(&w, &observer, 4, hornvale_kernel::WorldTime { day: 0.0 }).unwrap(),
    )
}

#[test]
fn surrounds_v1_land_and_mark_bytes_are_pinned() {
    hornvale_kernel::golden::assert_golden(
        std::path::Path::new(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/tests/fixtures/surrounds-seed-42-flagship.json"
        )),
        &surrounds_seed_42_flagship_json(),
        "scene/surrounds/v1 bytes moved (land/mark fixture) — field order is this schema's \
         headline contract (decision 0055: it becomes cross-repo once another repo parses \
         it); accept deliberately and review the diff as a contract change",
    );
}

// The two observers below (The Margin's gallery, `book/src/gallery/
// surrounds-seed-42.md`) are the only charts in the repo that actually show
// ocean meeting land and a face-seam disclosure — the flagship pin above
// sits in a uniform biome and neither the tiles-seed-1 nor the surrounds-
// seed-1 pin ever reaches a seam. The gallery page itself is (correctly)
// excluded from CI's cross-platform drift check, since it's an ASCII
// rendering of the same libm-thresholded classifications the tiles gallery
// exclusion already covers — but that leaves these two renders pinned
// NOWHERE, unlike every other gallery chart. These platform-local byte pins
// restore that protection without reintroducing the cross-platform noise the
// CI exclusion exists to avoid, exactly as `surrounds_v1_bytes_are_pinned`
// above does for the JSON encoding.

/// The observer room a possession's own `map`/`scene surrounds --render
/// ascii` would draw for room 897392747 — face 11, depth 12, half a degree
/// east of the settlement Mjoexaenoenoa (`connections-seed-42.md`), where
/// the 31-cell neighbourhood genuinely splits between ocean and land (the
/// gallery's "A coastline east of Mjoexaenoenoa").
fn coastline_room() -> hornvale_kernel::RoomAddr {
    hornvale_kernel::RoomId(897392747)
        .unpack()
        .expect("897392747 is a valid packed room id (the gallery's coastline observer)")
}

/// The observer room for room 724698318 — face 14, depth 12, latitude -10°/
/// longitude 0° — whose radius-4 neighbourhood reaches across a base-face
/// edge for 12 of its 31 cells (the gallery's "A seam, disclosed").
fn seam_room() -> hornvale_kernel::RoomAddr {
    hornvale_kernel::RoomId(724698318)
        .unpack()
        .expect("724698318 is a valid packed room id (the gallery's seam observer)")
}

/// The `ways on:` footer text, computed exactly as `cmd_scene`'s `--render
/// ascii` path and the possession's own `map` verb both compute it: the
/// observer room's own lateral (`ExitKind::Edge`) exits, read from
/// `hornvale_locale`.
fn ways_on(ctx: &hornvale_locale::LocaleContext, room: &hornvale_kernel::RoomAddr) -> Vec<String> {
    let locale = ctx
        .describe(room, hornvale_kernel::WorldTime { day: 0.0 })
        .expect("the gallery's pinned observers describe cleanly")
        .exits;
    locale
        .iter()
        .filter(|e| e.kind == hornvale_locale::ExitKind::Edge)
        .filter_map(|e| match e.direction {
            hornvale_locale::Direction::Compass(c) => Some(format!("{c:?}").to_uppercase()),
            _ => None,
        })
        .collect()
}

fn surrounds_ascii_coastline() -> String {
    let w = seed_42_world();
    let ctx = hornvale_locale::LocaleContext::build(&w).unwrap();
    let room = coastline_room();
    let scene = surrounds_scene(&w, &room, 4, hornvale_kernel::WorldTime { day: 0.0 }).unwrap();
    render_surrounds_ascii(&scene, "terrain", &ways_on(&ctx, &room))
}

#[test]
fn surrounds_ascii_coastline_bytes_are_pinned() {
    hornvale_kernel::golden::assert_golden(
        std::path::Path::new(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/tests/fixtures/surrounds-ascii-seed-42-coastline.txt"
        )),
        &surrounds_ascii_coastline(),
        "the gallery's coastline observer's ASCII chart bytes moved (The Margin's showcase \
         page, room 897392747) — platform-local pin (host-libm-sensitive biome/water/relief \
         classification, same exposure class as the scene-tiles exclusion); accept \
         deliberately, re-run with REBASELINE=1, and review the diff",
    );
}

fn surrounds_ascii_seam() -> String {
    let w = seed_42_world();
    let ctx = hornvale_locale::LocaleContext::build(&w).unwrap();
    let room = seam_room();
    let scene = surrounds_scene(&w, &room, 4, hornvale_kernel::WorldTime { day: 0.0 }).unwrap();
    render_surrounds_ascii(&scene, "terrain", &ways_on(&ctx, &room))
}

#[test]
fn surrounds_ascii_seam_bytes_are_pinned() {
    hornvale_kernel::golden::assert_golden(
        std::path::Path::new(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/tests/fixtures/surrounds-ascii-seed-42-seam.txt"
        )),
        &surrounds_ascii_seam(),
        "the gallery's seam observer's ASCII chart bytes moved (The Margin's showcase page, \
         room 724698318) — platform-local pin (host-libm-sensitive biome/water/relief \
         classification, same exposure class as the scene-tiles exclusion); accept \
         deliberately, re-run with REBASELINE=1, and review the diff",
    );
}

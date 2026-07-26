//! The scene/tiles/v1 byte pin: this fixture changing is the epoch
//! decision point (scene-protocol spec §2). Regenerate deliberately, never
//! casually: `REBASELINE=1 cargo test -p hornvale-scene --test golden`
//! (or `make rebaseline-goldens`) rewrites it, then review the diff as a
//! contract change.

use hornvale_scene::{scene_json, surrounds_json, surrounds_scene, tiles_scene};

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

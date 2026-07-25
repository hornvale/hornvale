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
    let (la, lo) = (0.0_f64.to_radians(), 0.0_f64.to_radians());
    let observer = hornvale_kernel::RoomAddr::containing(
        [
            hornvale_kernel::math::cos(la) * hornvale_kernel::math::cos(lo),
            hornvale_kernel::math::cos(la) * hornvale_kernel::math::sin(lo),
            hornvale_kernel::math::sin(la),
        ],
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

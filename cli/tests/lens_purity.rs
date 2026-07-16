//! World-identity guard: the seed-42 default world's JSON is a committed
//! fixture. If this test fails, world identity drifted — that is a
//! terrain or sky epoch, OR a species-roster change (a new/changed
//! `hornvale_species::registry` entry re-baselines settlement placement,
//! exposure, and every generated name world-wide — The Branches is the
//! precedent, spec §6), and must be deliberate: regenerate the fixture in
//! the same commit (`REBASELINE=1 cargo test -p hornvale --test
//! lens_purity`, or `make rebaseline-goldens`) and record why in the
//! chronicle. Campaign 25 (The Measured Coast) is contractually lens-only;
//! under it this test must never fail.

use hornvale_kernel::Seed;
use hornvale_worldgen::{SkyChoice, build_world};

#[test]
fn seed_42_world_json_matches_the_committed_fixture() {
    let world = build_world(
        Seed(42),
        &hornvale_astronomy::SkyPins::default(),
        SkyChoice::Generated,
        &hornvale_terrain::TerrainPins::default(),
        &hornvale_worldgen::SettlementPins::default(),
    )
    .expect("seed 42 builds");
    hornvale_kernel::golden::assert_golden(
        std::path::Path::new(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/tests/fixtures/world-seed-42.json"
        )),
        &world.to_json(),
        "world identity drifted from the committed fixture — see this file's module doc",
    );
}

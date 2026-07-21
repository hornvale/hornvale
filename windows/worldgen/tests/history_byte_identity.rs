//! The determinism contract's keystone test (T8): the deep-history skeleton
//! is byte-identical across two builds of the same seed — same seed, same
//! pins ⇒ identical deep history (every occupation record baked and
//! committed), the same committed present (`history-now`), and the same
//! ruins. This is the campaign's whole determinism guarantee stated as one
//! assertion, using the crate's public build API exactly the way
//! `tests/depth.rs` does (no internal replay of the bake).

use hornvale_astronomy::SkyPins;
use hornvale_kernel::Seed;
use hornvale_terrain::TerrainPins;
use hornvale_worldgen::{BuildDepth, SettlementPins, SkyChoice, WorldComponents, build_world_to};

/// Build seed 42 to `BuildDepth::Settlements` — deep enough to include the
/// whole history bake (genesis through the present) and the settlement/ruin
/// facts `emit_history` commits, but no deeper (culture/religion/naming are
/// out of this test's scope).
fn build_settlements() -> hornvale_kernel::World {
    let wc = WorldComponents::assemble().expect("canonical registries are well-formed");
    build_world_to(
        Seed(42),
        &SkyPins::default(),
        SkyChoice::Generated,
        &TerrainPins::default(),
        &SettlementPins::default(),
        &wc,
        BuildDepth::Settlements,
    )
    .expect("seed 42 builds to Settlements")
}

#[test]
fn history_skeleton_is_byte_identical_across_two_builds() {
    let a = build_settlements();
    let b = build_settlements();
    assert_eq!(
        serde_json::to_string(&a.ledger).unwrap(),
        serde_json::to_string(&b.ledger).unwrap(),
        "same seed must bake and commit a byte-identical deep-history skeleton \
         (occupations, the committed present, and ruins) — the determinism contract"
    );
}

#[test]
fn history_now_is_committed_and_matches_the_bake_end_year() {
    // The T8 fix: `history-now` is a committed fact, not an approximation —
    // assert it is present and equals `BakeConfig::default_millennia().end_year`
    // (2000.0), the value `windows/almanac::history::present_day` now reads.
    let w = build_settlements();
    let now = w
        .ledger
        .find(hornvale_history::HISTORY_NOW)
        .next()
        .expect("the composition root commits history-now once per world");
    assert_eq!(
        now.object,
        hornvale_kernel::Value::Number(hornvale_worldgen::BakeConfig::default_millennia().end_year),
        "history-now must equal the bake's end_year"
    );
}

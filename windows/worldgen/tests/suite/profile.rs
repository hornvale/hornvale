//! The build profiler records per-stage spans without altering the build.

use hornvale_astronomy::SkyPins;
use hornvale_kernel::Seed;
use hornvale_terrain::TerrainPins;
use hornvale_worldgen::{BuildProfile, SettlementPins, build_world, profiled};

#[test]
fn profiled_records_the_nine_stages() {
    let (world, profile): (_, BuildProfile) = profiled(|| {
        build_world(
            Seed(42),
            &SkyPins::default(),
            &TerrainPins::default(),
            &SettlementPins::default(),
        )
        .expect("seed 42 builds")
    });
    let labels: Vec<&str> = profile.stages.iter().map(|(l, _)| *l).collect();
    assert_eq!(
        labels,
        vec![
            "astronomy",
            "terrain",
            "climate+settlements",
            "alignments",
            "culture+religion+species",
            "deep-time",
            "planet",
            "peoples",
            // Task 3 (The Particular): the LAST stage, deliberately — see
            // `windows/worldgen/src/lib.rs`'s comment at the `"person"` stage.
            "person",
        ]
    );
    // The build still produced a real world.
    assert!(!world.ledger.is_empty());
}

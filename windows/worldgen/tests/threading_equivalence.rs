//! The Shuttle's equivalence pins: every `_from` readout equals its
//! `_of` wrapper on the same world. One world, one sculpt, all pairs.

use hornvale_astronomy::SkyPins;
use hornvale_kernel::Seed;
use hornvale_terrain::TerrainPins;
use hornvale_worldgen::{SettlementPins, SkyChoice, build_world};

#[test]
fn from_variants_equal_their_of_wrappers() {
    let world = build_world(
        Seed(1),
        &SkyPins::default(),
        SkyChoice::Generated,
        &TerrainPins::default(),
        &SettlementPins::default(),
    )
    .expect("seed 1 builds");
    let terrain = hornvale_worldgen::terrain_of(&world).expect("terrain reconstructs");
    let climate = hornvale_worldgen::climate_from(&world, &terrain).expect("climate derives");
    let at = hornvale_astronomy::StdDays::new(36_525.0).expect("valid day");

    for (kind, _v) in hornvale_worldgen::placed_peoples(&world) {
        assert_eq!(
            format!(
                "{:?}",
                hornvale_worldgen::account_params_of(&world, kind).ok()
            ),
            format!(
                "{:?}",
                hornvale_worldgen::account_params_from(&world, kind, &terrain, &climate).ok()
            ),
            "account_params diverged for {kind}"
        );
        assert_eq!(
            format!("{:?}", hornvale_worldgen::cyclic_beliefs_of(&world, kind)),
            format!(
                "{:?}",
                hornvale_worldgen::cyclic_beliefs_from(&world, kind, &climate)
            ),
            "cyclic_beliefs diverged for {kind}"
        );
        assert_eq!(
            format!("{:?}", hornvale_worldgen::doctrine_of(&world, kind)),
            format!(
                "{:?}",
                hornvale_worldgen::doctrine_from(&world, kind, &terrain, &climate)
            ),
            "doctrine diverged for {kind}"
        );
        assert_eq!(
            hornvale_worldgen::day_schema_of(&world, kind),
            hornvale_worldgen::day_schema_from(&world, kind, &terrain, &climate),
            "day_schema diverged for {kind}"
        );
        for concept in ["sun", "moon", "star", "earth", "person", "river"] {
            assert_eq!(
                hornvale_worldgen::noun_class_of(&world, kind, concept),
                hornvale_worldgen::noun_class_from(&world, kind, concept, &terrain, &climate),
                "noun_class diverged for {kind}/{concept}"
            );
        }
        assert_eq!(
            format!(
                "{:?}",
                hornvale_worldgen::observations_of(&world, kind, at).ok()
            ),
            format!(
                "{:?}",
                hornvale_worldgen::observations_from(&world, kind, at, &terrain, &climate).ok()
            ),
            "observations diverged for {kind}"
        );
        assert_eq!(
            format!("{:?}", hornvale_worldgen::ladder_of(&world, kind, at).ok()),
            format!(
                "{:?}",
                hornvale_worldgen::ladder_from(&world, kind, at, &terrain, &climate).ok()
            ),
            "ladder diverged for {kind}"
        );
    }
}

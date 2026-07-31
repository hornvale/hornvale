//! Determinism pins for the chorus/lexicon/exposure `_from` readout family
//! (post-The-Weir): each `_from` function, called twice over the same
//! already-built terrain/climate, must reproduce byte-identical output — the
//! derived-never-stored contract every readout in this family makes.
//!
//! Before The Weir this file instead asserted each `_of` wrapper equalled its
//! `_from` twin (a guard against a future hand patch forking the two bodies
//! apart). Task 2 deleted every `_of` wrapper in this family — there is
//! nothing left to diverge from — so this file now pins the `_from` behavior
//! directly: two independent calls over the same inputs must agree.
//!
//! Test fixture (decision 0092): calls the sculpt/fit derivation entry
//! points directly to build its own world state, once per test — the
//! sanctioned test-fixture posture the weir's spec carves out.
#![allow(clippy::disallowed_methods)]

use hornvale_astronomy::SkyPins;
use hornvale_kernel::Seed;
use hornvale_terrain::TerrainPins;
use hornvale_worldgen::{SettlementPins, SkyChoice, build_world};

#[test]
fn from_variants_are_pure_over_the_same_world() {
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

    let peoples = hornvale_worldgen::placed_peoples(&world);
    assert!(
        !peoples.is_empty(),
        "seed 1 must place peoples or this test is vacuous"
    );
    let mut saw_a_doctrine = false;

    for (kind, _v) in peoples {
        assert_eq!(
            format!(
                "{:?}",
                hornvale_worldgen::account_params_from(&world, kind, &terrain, &climate).ok()
            ),
            format!(
                "{:?}",
                hornvale_worldgen::account_params_from(&world, kind, &terrain, &climate).ok()
            ),
            "account_params_from must be pure for {kind}"
        );
        assert_eq!(
            format!(
                "{:?}",
                hornvale_worldgen::cyclic_beliefs_from(&world, kind, &climate)
            ),
            format!(
                "{:?}",
                hornvale_worldgen::cyclic_beliefs_from(&world, kind, &climate)
            ),
            "cyclic_beliefs_from must be pure for {kind}"
        );
        let doctrine = hornvale_worldgen::doctrine_from(&world, kind, &terrain, &climate);
        if doctrine.is_some() {
            saw_a_doctrine = true;
        }
        assert_eq!(
            format!("{:?}", doctrine),
            format!(
                "{:?}",
                hornvale_worldgen::doctrine_from(&world, kind, &terrain, &climate)
            ),
            "doctrine_from must be pure for {kind}"
        );
        assert_eq!(
            hornvale_worldgen::day_schema_from(&world, kind, &terrain, &climate),
            hornvale_worldgen::day_schema_from(&world, kind, &terrain, &climate),
            "day_schema_from must be pure for {kind}"
        );
        for concept in ["sun", "moon", "star", "earth", "person", "river"] {
            assert_eq!(
                hornvale_worldgen::noun_class_from(&world, kind, concept, &terrain, &climate),
                hornvale_worldgen::noun_class_from(&world, kind, concept, &terrain, &climate),
                "noun_class_from must be pure for {kind}/{concept}"
            );
        }
        assert_eq!(
            format!(
                "{:?}",
                hornvale_worldgen::observations_from(&world, kind, at, &terrain, &climate).ok()
            ),
            format!(
                "{:?}",
                hornvale_worldgen::observations_from(&world, kind, at, &terrain, &climate).ok()
            ),
            "observations_from must be pure for {kind}"
        );
        assert_eq!(
            format!(
                "{:?}",
                hornvale_worldgen::ladder_from(&world, kind, at, &terrain, &climate).ok()
            ),
            format!(
                "{:?}",
                hornvale_worldgen::ladder_from(&world, kind, at, &terrain, &climate).ok()
            ),
            "ladder_from must be pure for {kind}"
        );
        assert_eq!(
            format!(
                "{:?}",
                hornvale_worldgen::crisis_from(&world, kind, at, &terrain, &climate).ok()
            ),
            format!(
                "{:?}",
                hornvale_worldgen::crisis_from(&world, kind, at, &terrain, &climate).ok()
            ),
            "crisis_from must be pure for {kind}"
        );
    }

    assert!(
        saw_a_doctrine,
        "seed 1 must organize at least one placed people's doctrine or the \
         doctrine/ladder pairs above are vacuous"
    );
}

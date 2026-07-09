//! Exposure derivation at the composition root (Words Task 8):
//! `pack_depths` maps a species' perception vector onto the color-pack
//! ladders, and `exposure_of`/`lexicon_of` classify (and then name) every
//! registered concept for a settled species.
use hornvale_language::{ExposureClass, GapReason, LexEntry};
use hornvale_worldgen::{SettlementPins, SkyChoice, build_world, exposure_of, lexicon_of};

/// The same seed-42, generated-sky, default-pins world the existing
/// two-species integration tests (`species_worlds.rs`) build — both goblin
/// and kobold place settlements on it.
fn world() -> hornvale_kernel::World {
    build_world(
        hornvale_kernel::Seed(42),
        &hornvale_astronomy::SkyPins::default(),
        SkyChoice::Generated,
        &hornvale_terrain::TerrainPins::default(),
        &SettlementPins::default(),
    )
    .unwrap()
}

#[test]
fn goblin_lexicon_has_a_root_for_water_the_universal_concept() {
    let w = world();
    let lex = lexicon_of(&w, "goblin").unwrap();
    match lex.entry("water") {
        Some(LexEntry::Root { .. }) => {}
        other => panic!("expected water to be a Root entry (universal stratum), got {other:?}"),
    }
}

#[test]
fn kobold_blue_is_a_perceptual_gap_and_goblin_blue_is_not() {
    let w = world();
    let goblin = exposure_of(&w, "goblin").unwrap();
    let kobold = exposure_of(&w, "kobold").unwrap();

    match kobold.get("blue") {
        Some(ExposureClass::Unknown {
            reason: GapReason::Perceptual(_),
        }) => {}
        other => panic!("expected kobold's 'blue' to be a Perceptual gap, got {other:?}"),
    }
    assert!(
        !matches!(
            goblin.get("blue"),
            Some(ExposureClass::Unknown {
                reason: GapReason::Perceptual(_)
            })
        ),
        "goblin's 'blue' should not be a Perceptual gap (goblin's hue depth reaches blue); got {:?}",
        goblin.get("blue")
    );
}

#[test]
fn every_unknown_entrys_reason_is_non_empty() {
    let w = world();
    for species in ["goblin", "kobold"] {
        let exposures = exposure_of(&w, species).unwrap();
        for (concept, class) in &exposures {
            if let ExposureClass::Unknown { reason } = class {
                let text = match reason {
                    GapReason::Experiential(s) | GapReason::Perceptual(s) => s,
                };
                assert!(
                    !text.trim().is_empty(),
                    "{species}'s Unknown reason for '{concept}' must be non-empty"
                );
            }
        }
    }
}

#[test]
fn exposure_of_is_pure_across_two_calls() {
    let w = world();
    let a = exposure_of(&w, "goblin").unwrap();
    let b = exposure_of(&w, "goblin").unwrap();
    assert_eq!(a, b, "same world+species must yield identical exposure");
}

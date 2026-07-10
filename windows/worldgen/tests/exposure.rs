//! Exposure derivation at the composition root (Words Task 8):
//! `pack_depths` maps a species' perception vector onto the color-pack
//! ladders, and `exposure_of`/`lexicon_of` classify (and then name) every
//! registered concept for a settled species.
use hornvale_language::{ExposureClass, GapReason, LexEntry};
use hornvale_worldgen::{SettlementPins, SkyChoice, build_world, exposure_of, lexicon_of};

/// The seed-42, generated-sky, default-pins (full four-people roster)
/// world `species_worlds.rs` builds. Since the founder floor (settlement's
/// founder-reservation pass, MAP-22 K=1), all four peoples coexist in this
/// world's shared joint-greedy placement pass (goblin and kobold each win
/// many cells on their own suitability; hobgoblin and bugbear win exactly
/// the one cell the founder floor reserves for them). The coexistence
/// test below uses goblin and kobold, mirroring the pre-Branches
/// two-species test this file descends from.
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
fn each_placed_species_holds_a_root_for_every_placed_species_kind() {
    // Spec §3: "each language will hold its own words for goblin-kind and
    // kobold-kind — endonym and exonym fall out free." Coexistence in one
    // shared world is exposure: both peoples place, so each is Steeped in
    // the other's kind and each lexicon roots both. Since the founder
    // floor, goblin and kobold both coexist in this world (see `world()`'s
    // doc comment) exactly as the pre-Branches two-species world did.
    let w = world();
    let goblin = lexicon_of(&w, "goblin").unwrap();
    let kobold = lexicon_of(&w, "kobold").unwrap();

    let mut romans = Vec::new();
    for (lex, species) in [(&goblin, "goblin"), (&kobold, "kobold")] {
        for concept in ["goblin-kind", "kobold-kind"] {
            match lex.entry(concept) {
                Some(LexEntry::Root { views, .. }) => romans.push(views.roman.clone()),
                other => panic!("{species}'s '{concept}' should be a Root, got {other:?}"),
            }
        }
    }
    // The exonym exists and differs between the two languages: each species
    // draws its word for either kind from its own phonology.
    assert_ne!(
        romans[0], romans[2],
        "goblin and kobold words for goblin-kind should differ"
    );
    assert_ne!(
        romans[1], romans[3],
        "goblin and kobold words for kobold-kind should differ"
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

/// A species the world never placed still gets a total, well-reasoned
/// exposure map: build a goblin-only world (species pin) and query the
/// UNPLACED kobold. Every registered concept classifies exactly once; the
/// experiential core (universal stratum) is Steeped regardless of
/// settlement; geography-derived and coexistence-derived concepts fall to
/// reasoned gaps. Guards the zero-settlement path the seed-42 default
/// world never exercises.
#[test]
fn an_unplaced_species_still_gets_a_total_reasoned_exposure_map() {
    let w = build_world(
        hornvale_kernel::Seed(42),
        &hornvale_astronomy::SkyPins::default(),
        SkyChoice::Generated,
        &hornvale_terrain::TerrainPins::default(),
        &SettlementPins {
            species: Some("goblin".to_string()),
            ..SettlementPins::default()
        },
    )
    .unwrap();

    let exposures = exposure_of(&w, "kobold").unwrap();
    assert_eq!(
        exposures.len(),
        w.registry.concepts().count(),
        "every registered concept must classify exactly once for an unplaced species"
    );
    assert!(
        matches!(
            exposures.get("water"),
            Some(hornvale_language::ExposureClass::Steeped)
        ),
        "the universal stratum is experience every embodied species has, settled or not"
    );
    for (concept, class) in &exposures {
        if let hornvale_language::ExposureClass::Unknown { reason } = class {
            let text = match reason {
                hornvale_language::GapReason::Experiential(s) => s,
                hornvale_language::GapReason::Perceptual(s) => s,
            };
            assert!(
                !text.is_empty(),
                "unplaced-species gap for '{concept}' must carry a reason"
            );
        }
    }
    // The lexicon still assembles over that map without panicking.
    let lex = lexicon_of(&w, "kobold").unwrap();
    assert_eq!(lex.entries().count(), exposures.len());
}

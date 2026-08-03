//! The Vernacular's preregistered instrument: a phenomenon's English
//! description is a rendering, not a contract, so a semantically null reword
//! must move zero committed facts.
//!
//! Before the referent existed, rewording one description in
//! `domains/astronomy/src/provider.rs` from `"a {} moon"` to `"a {} lunar
//! disc"` moved 73 facts on seed 42 — 9 of 48 deity names and 7 of 48
//! epithets — because `phenomenon_concept` dispatched on
//! `description.contains("moon")`. This test is the standing proof that the
//! coupling is gone.
//!
//! It works by mutating the phenomenon list a world was built from, rather
//! than by editing source: every description is replaced with a string that
//! shares no substring with any concept id, and the gloss must be unmoved.

use hornvale_kernel::{Phenomenon, Referent, Seed, World};
use hornvale_worldgen::{
    SettlementPins, SkyChoice, build_world, gloss_concept_of, observed_phenomena,
};

/// Seed 42 at default pins — the same world the gallery almanacs describe.
fn world() -> World {
    build_world(
        Seed(42),
        &hornvale_astronomy::SkyPins::default(),
        SkyChoice::Generated,
        &hornvale_terrain::TerrainPins::default(),
        &SettlementPins::default(),
    )
    .unwrap_or_else(|e| panic!("seed 42 builds: {e}"))
}

/// Every description replaced by prose that names nothing.
fn reworded(phenomena: &[Phenomenon]) -> Vec<Phenomenon> {
    phenomena
        .iter()
        .enumerate()
        .map(|(i, p)| Phenomenon {
            description: format!("an occurrence of the {i}th kind"),
            ..p.clone()
        })
        .collect()
}

#[test]
fn rewording_every_description_leaves_the_gloss_unmoved() {
    let world = world();
    let phenomena =
        observed_phenomena(&world, 0.0).unwrap_or_else(|e| panic!("seed 42 has phenomena: {e}"));

    let before: Vec<Option<&str>> = phenomena.iter().map(gloss_concept_of).collect();
    let after: Vec<Option<&str>> = reworded(&phenomena).iter().map(gloss_concept_of).collect();

    assert_eq!(
        before, after,
        "a null reword moved the gloss — the description is load-bearing again"
    );
    assert!(
        before.iter().any(Option::is_some),
        "the fixture must actually exercise the gloss, not pass vacuously"
    );
}

#[test]
fn a_referent_never_carries_prose() {
    let world = world();
    for p in observed_phenomena(&world, 0.0).expect("phenomena") {
        for key in std::iter::once(&p.referent.concept).chain(p.referent.qualifiers.iter()) {
            assert!(
                !key.contains(' ') && key.chars().all(|c| c.is_ascii_lowercase() || c == '-'),
                "referent key {key:?} is prose, not a registry key"
            );
        }
    }
}

/// The keys a referent names must exist in the registry — otherwise the
/// lexicon can never reach them, which is the whole defect this campaign
/// closes.
#[test]
fn every_referent_key_is_registered() {
    let world = world();
    for p in observed_phenomena(&world, 0.0).expect("phenomena") {
        for key in std::iter::once(&p.referent.concept).chain(p.referent.qualifiers.iter()) {
            assert!(
                world.registry.concept(key).is_some(),
                "referent key {key:?} is not a registered concept"
            );
        }
    }
}

/// Guards the guard: `reworded` must actually change every description, or
/// the first test passes for the wrong reason.
#[test]
fn the_rewording_fixture_changes_every_description() {
    let p = Phenomenon {
        kind: "celestial-body".to_string(),
        referent: Referent::of("moon"),
        description: "a vast moon".to_string(),
        period_days: None,
        salience: 1.0,
        venue: hornvale_kernel::Venue::NightSky,
    };
    let out = reworded(std::slice::from_ref(&p));
    assert_ne!(out[0].description, p.description);
    assert_eq!(out[0].referent, p.referent);
}

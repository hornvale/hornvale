//! The Vernacular's preregistered instrument: prose is not a contract.
//!
//! Before the referent existed, rewording one description in
//! `domains/astronomy/src/provider.rs` from `"a {} moon"` to `"a {} lunar
//! disc"` moved 73 committed facts on seed 42 — 9 of 48 deity names and 7 of
//! 48 epithets — because `phenomenon_concept` dispatched on
//! `description.contains("moon")`.
//!
//! **The headline test that guarded that defect is retired, because the defect
//! is now structurally impossible.** It replaced every phenomenon's
//! description with prose naming nothing and asserted the gloss did not move.
//! `Phenomenon` no longer has a description: a producer cannot know who is
//! looking (`ObserverContext` is `{place, time, lens, position}` by
//! constitutional design, decision 0003), so no stored string could ever have
//! been right, and words are now realized where the speaker is known. There is
//! nothing left to reword, so there is nothing left for a reword-invariance
//! test to say. A type that cannot express the defect is a stronger guarantee
//! than a test that catches it.
//!
//! What survives here is the other half, which is not structural and still
//! needs asserting on a real world: the referent that replaced the prose must
//! itself stay free of prose, and every key it names must be reachable in the
//! concept registry.

use hornvale_kernel::{Seed, World};
use hornvale_worldgen::{SettlementPins, SkyChoice, build_world, observed_phenomena};

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

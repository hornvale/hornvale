//! The Vacancy: no kind may be a ghost.
//!
//! A kind can be authored, load, satisfy every referential-integrity check in
//! `components.rs`, and still have K = 0 on every cell of every world - present
//! in the registry, absent from the world, with no error anywhere. That is not
//! hypothetical: the kobold's elevation optimum once sat at or above the
//! highest land on most seeds, so its documented "exclusive highland
//! stronghold" was unoccupiable and its fit ran ~25x below every other
//! people's everywhere. It shipped, and was found by hand campaigns later
//! (BIO-39).
//!
//! This test is the refusal. It is cheap, it passes today, and it fails the
//! moment a kind is authored into a niche no world can satisfy.
//!
//! World-building idiom reused verbatim from `occupancy_readout.rs`
//! (`hornvale_worldgen::build_world`, `WorldComponents::assemble`,
//! `terrain_of`/`climate_of`/`sky_of` "reconstruct, never store"). The
//! per-species K comes from [`hornvale_worldgen::per_species_suitability`], whose
//! returned `u32` is a **build-local dense index, not identity** (see its doc
//! comment) - it is the position in the `species_biosphere` slice passed in,
//! so the index -> [`hornvale_kernel::KindId`] mapping here is rebuilt fresh,
//! per seed, from that exact same `wc.biosphere.iter()` ordering.
//!
//! Test fixture (decision 0092): calls the sculpt/fit derivation entry
//! points directly to build its own world state, once per test — the
//! sanctioned test-fixture posture the weir's spec carves out.
#![allow(clippy::disallowed_methods)]

use hornvale_astronomy::SkyPins;
use hornvale_kernel::{KindId, Seed};
use hornvale_terrain::TerrainPins;
use hornvale_worldgen::{
    SettlementPins, SkyChoice, WorldComponents, build_world, climate_of, per_species_suitability,
    sky_of, terrain_of,
};
use std::collections::BTreeSet;

/// The viability floor below which a cell's K is ecological noise rather
/// than presence - [`hornvale_demography::FLOOR`], unchanged. Reused
/// identical to `occupancy_readout.rs`'s `VIABILITY_FLOOR`; two different
/// floors would let a kind pass one test and fail the other.
const VIABILITY_FLOOR: f64 = hornvale_demography::FLOOR;

/// The set of kind names viable (K at or above [`VIABILITY_FLOOR`] on at
/// least one cell) on the world built from `seed`. Builds the world and the
/// registries once and checks every kind against it in a single pass,
/// mirroring `occupancy_readout.rs`'s per-seed structure rather than
/// rebuilding the world once per (seed, kind) pair.
fn viable_kinds_on(seed: u64) -> BTreeSet<&'static str> {
    let wc = WorldComponents::assemble().expect("canonical registries are well-formed");
    // The build-local dense index -> KindId mapping, built from the exact
    // same `wc.biosphere` ordering passed to `per_species_suitability` below
    // (ascending-KindId order, per `per_species_suitability`'s doc comment) so
    // the returned `u32` tags resolve to the correct kind.
    let kinds: Vec<KindId> = wc.biosphere.iter().map(|(k, _)| *k).collect();
    let bios: Vec<&hornvale_species::BiosphereTraits> =
        wc.biosphere.iter().map(|(_, b)| b).collect();
    // Same `wc.biosphere` order as `bios`, so the realm slice stays
    // index-aligned — a kind absent from the sparse habitat-realm store
    // defaults to `Surface`.
    let realm: Vec<hornvale_species::HabitatRealm> = wc
        .biosphere
        .iter()
        .map(|(k, _)| {
            wc.habitat_realm
                .get(k)
                .copied()
                .unwrap_or(hornvale_species::HabitatRealm::SURFACE)
        })
        .collect();

    let world = build_world(
        Seed(seed),
        &SkyPins::default(),
        SkyChoice::Generated,
        &TerrainPins::default(),
        &SettlementPins::default(),
    )
    .unwrap_or_else(|e| panic!("seed {seed} failed to build: {e:?}"));

    let terrain = terrain_of(&world).expect("terrain reconstructs");
    let climate = climate_of(&world).expect("climate reconstructs");
    let sky = sky_of(&world).expect("sky reconstructs");
    let geo = terrain.geosphere();
    let system = sky
        .system()
        .unwrap_or_else(|| panic!("seed {seed} has a generated star system"));
    let insolation = hornvale_astronomy::insolation_rel(&system.star, &system.anchor);
    let obliquity = system.anchor.obliquity.get();
    let regime = match system.anchor.rotation {
        hornvale_astronomy::Rotation::Spinning { day, .. } => {
            hornvale_climate::RotationRegime::Spinning { day_std: day.get() }
        }
        hornvale_astronomy::Rotation::Locked => hornvale_climate::RotationRegime::Locked,
    };

    let ks = per_species_suitability(
        geo, &terrain, &climate, obliquity, insolation, &regime, &bios, &realm,
    );

    let mut viable = BTreeSet::new();
    for (tag, k) in &ks {
        let name = kinds[*tag as usize].0;
        if geo.cells().any(|cell| *k.get(cell) >= VIABILITY_FLOOR) {
            viable.insert(name);
        }
    }
    viable
}

#[test]
fn every_kind_is_viable_somewhere() {
    // A small seed set: this is a "somewhere, ever" existence check, not a
    // distributional claim, so a handful of worlds is the right cost. The
    // per-kind DISTRIBUTION is the occupancy readout's job (Task 3).
    let seeds = [1u64, 7, 42, 99];
    let roster: Vec<&'static str> = hornvale_species::biosphere_registry()
        .iter()
        .map(|(k, _)| k.0)
        .collect();

    let viable_anywhere: BTreeSet<&'static str> = seeds
        .iter()
        .flat_map(|seed| viable_kinds_on(*seed))
        .collect();

    let mut void: Vec<&'static str> = roster
        .iter()
        .filter(|name| !viable_anywhere.contains(*name))
        .copied()
        .collect();
    void.sort_unstable();

    assert_eq!(
        void,
        Vec::<&'static str>::new(),
        "every kind must be viable somewhere - a kind appearing here is a \
         ghost. Either the condition niche is authored outside the range any \
         world produces (the BIO-39 class: check the optima against \
         tests/fixtures/occupancy.csv percentiles), or the uptake vector \
         points at supply axes that are zero everywhere the condition terms \
         allow (an aquatic niche before a marine supply axis exists)."
    );
}

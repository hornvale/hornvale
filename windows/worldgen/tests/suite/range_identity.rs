//! THE RANGE: the realm gate must reach world identity, not only the readout.
//!
//! Before this campaign, declaring a PEOPLED kind `Subterranean` — confining it
//! to the ~12% of land holding a cave — moved `per_species_suitability` from
//! 99.49% to 5.62% of land and the committed seed-42 world by ZERO bytes,
//! because `per_species_capacity_at` (what settlement placement consumes) took
//! no realm parameter at all. Measured 2026-08-08; see the spec §1.
#![allow(clippy::disallowed_methods)]

use hornvale_astronomy::SkyPins;
use hornvale_kernel::Seed;
use hornvale_species::HabitatRealm;
use hornvale_terrain::TerrainPins;
use hornvale_worldgen::{
    SettlementPins, SkyChoice, WorldComponents, build_world, climate_of, per_species_capacity,
    sky_of, terrain_of,
};

/// The realm slice must CHANGE a peopled kind's capacity field. This is the
/// rung-5 test: a mechanism that moves a readout and not the world is not a
/// mechanism. Uses `gnoll` — a peopled, settling kind — because the defect is
/// invisible on fauna (no fauna kind competes in settlement genesis).
#[test]
fn a_declared_realm_changes_a_peopled_kinds_capacity_field() {
    let world = build_world(
        Seed(42),
        &SkyPins::default(),
        SkyChoice::Generated,
        &TerrainPins::default(),
        &SettlementPins::default(),
    )
    .expect("seed 42 builds");
    let terrain = terrain_of(&world).unwrap();
    let climate = climate_of(&world).unwrap();
    let sky = sky_of(&world).unwrap();
    let generated = match &sky {
        hornvale_worldgen::Sky::Generated(g) => g,
        _ => panic!("probe expects a generated sky"),
    };
    let system = generated.system();
    let insolation_scalar = hornvale_astronomy::insolation_rel(&system.star, &system.anchor);
    let obliquity_deg = system.anchor.obliquity.get();
    let regime = match system.anchor.rotation {
        hornvale_astronomy::Rotation::Spinning { day, .. } => {
            hornvale_climate::RotationRegime::Spinning { day_std: day.get() }
        }
        hornvale_astronomy::Rotation::Locked => hornvale_climate::RotationRegime::Locked,
    };
    let wc = WorldComponents::assemble().unwrap();
    let geo = terrain.geosphere();

    let names: Vec<&'static str> = wc.biosphere.ids().map(|k| k.0).collect();
    let bio: Vec<&hornvale_species::BiosphereTraits> =
        wc.biosphere.iter().map(|(_, b)| b).collect();
    let tag = names
        .iter()
        .position(|n| *n == "gnoll")
        .expect("gnoll in the biosphere roster");

    let all_surface = vec![HabitatRealm::Surface; bio.len()];
    let mut one_subterranean = all_surface.clone();
    one_subterranean[tag] = HabitatRealm::Subterranean;
    // The Range: the `biome_affinity` registry is NOT empty — `gnoll` and
    // `woolly-mammoth` carry rows since task 4, and gnoll is this test's own
    // subject. So this is a deliberate CONTROL, not a copy of the registry, and
    // here the distinction matters more than anywhere else in this directory.
    //
    // This test's two arms differ in ONE component, the realm slice, so that a
    // difference in capacity is attributable to the realm and to nothing else.
    // Threading the live affinity would apply gnoll's factors identically to
    // both arms and so could not change the verdict — but it would put a second
    // moving mechanism inside a matched pair whose whole value is that only one
    // thing moves. All-`None` is bit-identical to the pre-affinity physics (task
    // 3's `an_absent_affinity_is_bit_identical`), which keeps the pair minimal.
    let affinity: Vec<Option<hornvale_species::BiomeAffinity>> = vec![None; bio.len()];

    let caps = |realm: &[HabitatRealm]| {
        per_species_capacity(
            geo,
            &terrain,
            &climate,
            obliquity_deg,
            insolation_scalar,
            &regime,
            &bio,
            realm,
            &affinity,
        )
        .into_iter()
        .find(|(t, _)| *t == tag as u32)
        .expect("gnoll's capacity map")
        .1
    };

    let surface = caps(&all_surface);
    let confined = caps(&one_subterranean);

    let mut surface_nonzero = 0usize;
    let mut confined_nonzero = 0usize;
    for cell in geo.vertices() {
        if terrain.is_ocean(cell) {
            continue;
        }
        if surface.at(cell) > 0.0 {
            surface_nonzero += 1;
        }
        if confined.at(cell) > 0.0 {
            confined_nonzero += 1;
        }
    }

    assert!(
        confined_nonzero < surface_nonzero / 2,
        "confining a peopled kind to caves must collapse its capacity field: \
         surface {surface_nonzero} land cells vs confined {confined_nonzero}. \
         Equal counts mean the realm gate does not reach the capacity path — \
         the exact defect this campaign exists to repair."
    );
}

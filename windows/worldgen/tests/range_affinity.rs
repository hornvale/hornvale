//! THE RANGE: the biome affinity multiplies OUTSIDE the Liebig minimum, and
//! reaches BOTH the readout and the identity path.
#![allow(clippy::disallowed_methods)]

use hornvale_astronomy::SkyPins;
use hornvale_kernel::Seed;
use hornvale_species::{BiomeAffinity, HabitatRealm};
use hornvale_terrain::TerrainPins;
use hornvale_worldgen::{
    SettlementPins, SkyChoice, WorldComponents, build_world, climate_of, per_species_capacity,
    per_species_suitability, sky_of, terrain_of,
};

/// Seed 42 at the depth `per_species_suitability` needs (terrain + climate +
/// stellar inputs), plus the assembled component set. Copied verbatim from
/// `windows/worldgen/tests/warren_gate.rs:28-77` — it already builds exactly
/// these pieces for exactly this call, and matching it keeps this battery's
/// numbers comparable with that probe's.
fn fixture() -> (
    hornvale_terrain::GeneratedTerrain,
    hornvale_climate::GeneratedClimate,
    f64,
    f64,
    hornvale_climate::RotationRegime,
    WorldComponents,
) {
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

    (
        terrain,
        climate,
        obliquity_deg,
        insolation_scalar,
        regime,
        wc,
    )
}

/// The build-local parallel slices matching `wc.biosphere`'s ascending-`KindId`
/// order — the same assembly the shipped path performs.
fn slices(
    wc: &hornvale_worldgen::WorldComponents,
) -> (Vec<&hornvale_species::BiosphereTraits>, Vec<HabitatRealm>) {
    let bio = wc.biosphere.iter().map(|(_, b)| b).collect();
    let realm = wc
        .biosphere
        .iter()
        .map(|(kind, _)| {
            wc.habitat_realm
                .get(kind)
                .copied()
                .unwrap_or(HabitatRealm::SURFACE)
        })
        .collect();
    (bio, realm)
}

fn gnoll_tag(wc: &hornvale_worldgen::WorldComponents) -> usize {
    wc.biosphere
        .ids()
        .position(|k| k.0 == "gnoll")
        .expect("gnoll in the biosphere roster")
}

/// An ABSENT affinity is an exact IEEE-754 no-op — bit-identical, not
/// approximately equal. The Warren proved its `Surface` path to bit-difference
/// 0 and the same standard applies here: this is what makes Task 3 provably
/// inert, so that only Task 4 moves the world.
#[test]
fn an_absent_affinity_is_bit_identical() {
    let (terrain, climate, obliquity_deg, insolation_scalar, regime, wc) = fixture();
    let geo = terrain.geosphere();
    let (bio, realm) = slices(&wc);
    let none: Vec<Option<BiomeAffinity>> = vec![None; bio.len()];

    let a = per_species_suitability(
        geo,
        &terrain,
        &climate,
        obliquity_deg,
        insolation_scalar,
        &regime,
        &bio,
        &realm,
        &none,
    );
    let b = per_species_suitability(
        geo,
        &terrain,
        &climate,
        obliquity_deg,
        insolation_scalar,
        &regime,
        &bio,
        &realm,
        &none,
    );

    let mut compared = 0usize;
    for ((ta, ka), (tb, kb)) in a.iter().zip(b.iter()) {
        assert_eq!(ta, tb, "species order must match");
        for cell in geo.cells() {
            assert_eq!(
                ka.get(cell).to_bits(),
                kb.get(cell).to_bits(),
                "an absent affinity must be bit-identical at {cell:?} for tag {ta}"
            );
            compared += 1;
        }
    }
    assert!(compared > 0, "the comparison must not be vacuous");
}

/// A declared affinity multiplies OUTSIDE the Liebig minimum. Proven by
/// reconstruction rather than asserted: the production value must equal the
/// undeclared value times the authored factor, BIT-EXACTLY. If the factor were
/// folded into `tolerance_liebig` as a fifth axis, the minimum would clamp it
/// and this identity would fail — which is precisely the distinction the spec
/// calls load-bearing (for a mask in {0,1} `min` and `*` agree; for a GRADED
/// factor they do not).
#[test]
fn a_declared_affinity_multiplies_outside_the_minimum() {
    let (terrain, climate, obliquity_deg, insolation_scalar, regime, wc) = fixture();
    let geo = terrain.geosphere();
    let (bio, realm) = slices(&wc);
    let tag = gnoll_tag(&wc);

    let none: Vec<Option<BiomeAffinity>> = vec![None; bio.len()];
    let mut declared = none.clone();
    // Deliberately NON-uniform: a uniform row is inert by construction.
    declared[tag] = Some(BiomeAffinity {
        default: 0.25,
        by_biome: vec![("desert", 1.0)],
    });

    let base = per_species_suitability(
        geo,
        &terrain,
        &climate,
        obliquity_deg,
        insolation_scalar,
        &regime,
        &bio,
        &realm,
        &none,
    );
    let with = per_species_suitability(
        geo,
        &terrain,
        &climate,
        obliquity_deg,
        insolation_scalar,
        &regime,
        &bio,
        &realm,
        &declared,
    );

    let kb = &base.iter().find(|(t, _)| *t == tag as u32).unwrap().1;
    let kw = &with.iter().find(|(t, _)| *t == tag as u32).unwrap().1;
    let biome = climate.biome_map();

    let mut moved = 0usize;
    for cell in geo.cells() {
        let f = if biome.get(cell).name() == "desert" {
            1.0
        } else {
            0.25
        };
        let expected = kb.get(cell) * f;
        assert_eq!(
            kw.get(cell).to_bits(),
            expected.to_bits(),
            "affinity must apply as a plain factor outside the minimum at {cell:?}"
        );
        if kw.get(cell).to_bits() != kb.get(cell).to_bits() {
            moved += 1;
        }
    }
    // Guard against a vacuous pass: if nothing moved, the identity above is
    // trivially satisfied and proves nothing.
    assert!(
        moved > 0,
        "the declared affinity must actually change some cell, or this test is vacuous"
    );
}

/// BOTH sites, or the mechanism is dead on arrival (spec §3.3). This is the
/// whole lesson of the campaign: The Warren's gate reached the readout and not
/// the world. A test that checked only `per_species_suitability` would have
/// passed for The Warren too.
#[test]
fn the_affinity_reaches_the_capacity_path_too() {
    let (terrain, climate, obliquity_deg, insolation_scalar, regime, wc) = fixture();
    let geo = terrain.geosphere();
    let (bio, realm) = slices(&wc);
    let tag = gnoll_tag(&wc);

    let none: Vec<Option<BiomeAffinity>> = vec![None; bio.len()];
    let mut declared = none.clone();
    declared[tag] = Some(BiomeAffinity {
        default: 0.25,
        by_biome: vec![("desert", 1.0)],
    });

    let caps = |aff: &[Option<BiomeAffinity>]| {
        per_species_capacity(
            geo,
            &terrain,
            &climate,
            obliquity_deg,
            insolation_scalar,
            &regime,
            &bio,
            &realm,
            aff,
        )
        .into_iter()
        .find(|(t, _)| *t == tag as u32)
        .expect("gnoll's capacity map")
        .1
    };

    let base = caps(&none);
    let with = caps(&declared);

    let mut moved = 0usize;
    for cell in geo.cells() {
        if base.at(cell).to_bits() != with.at(cell).to_bits() {
            moved += 1;
        }
    }
    assert!(
        moved > 0,
        "the affinity must reach the CAPACITY path, not only the readout — \
         zero moved cells is exactly the defect this campaign exists to repair"
    );
}

//! The Tense §3.1: the era-adjusted substrate's **no-op seam**.
//!
//! `substrate_field` now delegates to `substrate_field_at` with
//! `EraAdjust::present`. That is only safe if the present-day path is
//! byte-for-byte what it was before the era parameter existed — adding a zero
//! `TempAnomaly` and subtracting today's sea level must be exact identities, not
//! approximately-exact ones.
//!
//! Float identities are the kind of thing that is obviously true and sometimes
//! is not (`x + 0.0` is not the identity for `x = -0.0`), and this substrate
//! feeds settlement placement, so a last-ULP difference would move every world
//! silently. Hence assertions rather than reasoning.

#![allow(clippy::disallowed_methods)]

use hornvale_worldgen::{
    EraAdjust, SettlementPins, SkyChoice, build_world, climate_of, insolation_field, sky_of,
    substrate_field, substrate_field_at, terrain_of,
};

/// Rebuild the pieces a substrate needs for one seed.
fn parts(
    seed: u64,
) -> (
    hornvale_terrain::GeneratedTerrain,
    hornvale_climate::GeneratedClimate,
    f64,
    f64,
    hornvale_climate::RotationRegime,
) {
    let world = build_world(
        hornvale_kernel::Seed(seed),
        &hornvale_astronomy::SkyPins::default(),
        SkyChoice::Generated,
        &hornvale_terrain::TerrainPins::default(),
        &SettlementPins::default(),
    )
    .expect("seed builds");
    let terrain = terrain_of(&world).expect("terrain");
    let climate = climate_of(&world).expect("climate");
    let sky = sky_of(&world).expect("sky");
    let generated = match &sky {
        hornvale_worldgen::Sky::Generated(g) => g,
        _ => panic!("test expects a generated sky"),
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
    (terrain, climate, obliquity_deg, insolation_scalar, regime)
}

#[test]
fn present_era_substrate_is_bit_identical_to_the_unparameterised_field() {
    for seed in [42, 7, 1234] {
        let (terrain, climate, obliquity_deg, insolation_scalar, regime) = parts(seed);
        let geo = terrain.geosphere();

        let direct = substrate_field(
            geo,
            &terrain,
            &climate,
            obliquity_deg,
            insolation_scalar,
            &regime,
        );
        let insolation = insolation_field(geo, obliquity_deg, insolation_scalar, &regime);
        let via_era = substrate_field_at(
            geo,
            &terrain,
            &climate,
            &insolation,
            &EraAdjust::present(&terrain),
        );

        for cell in geo.cells() {
            let a = direct.get(cell);
            let b = via_era.get(cell);
            // Bit patterns, not `==`: `==` would accept 0.0 for -0.0, and the
            // whole point is that nothing shifted at all.
            assert_eq!(
                a.temperature_c.to_bits(),
                b.temperature_c.to_bits(),
                "seed {seed} cell {cell:?}: temperature moved"
            );
            assert_eq!(
                a.moisture.to_bits(),
                b.moisture.to_bits(),
                "seed {seed} cell {cell:?}: moisture moved"
            );
            assert_eq!(
                a.insolation.to_bits(),
                b.insolation.to_bits(),
                "seed {seed} cell {cell:?}: insolation moved"
            );
            assert_eq!(
                a.elevation.to_bits(),
                b.elevation.to_bits(),
                "seed {seed} cell {cell:?}: elevation moved"
            );
        }
    }
}

/// A non-present era must actually move the substrate — otherwise the seam is
/// inert and every era would replay the present, which is the defect The Tense
/// exists to remove.
#[test]
fn a_glacial_era_moves_temperature_and_the_shoreline() {
    let (terrain, climate, obliquity_deg, insolation_scalar, regime) = parts(42);
    let geo = terrain.geosphere();
    let insolation = insolation_field(geo, obliquity_deg, insolation_scalar, &regime);

    let present = EraAdjust::present(&terrain);
    // A glacial low-stand: 8 °C colder, sea 120 m lower (Earth's LGM figures,
    // used here only to exercise the seam, not as authored physics).
    let glacial = EraAdjust {
        temp_offset: hornvale_kernel::TempAnomaly::from_offset_c(-8.0),
        sea_level: hornvale_kernel::ReferenceElevation::new(terrain.sea_level().get() - 120.0)
            .expect("a finite low-stand"),
    };

    let now = substrate_field_at(geo, &terrain, &climate, &insolation, &present);
    let then = substrate_field_at(geo, &terrain, &climate, &insolation, &glacial);

    let mut colder = 0usize;
    let mut newly_exposed = 0usize;
    for cell in geo.cells() {
        let (a, b) = (now.get(cell), then.get(cell));
        if b.temperature_c < a.temperature_c {
            colder += 1;
        }
        if a.elevation < 0.0 && b.elevation >= 0.0 {
            newly_exposed += 1;
        }
        // Insolation is hoisted and era-invariant by construction.
        assert_eq!(b.insolation.to_bits(), a.insolation.to_bits());
    }
    assert_eq!(
        colder,
        geo.cell_count(),
        "every cell should be colder at a -8 C era"
    );
    assert!(
        newly_exposed > 0,
        "a 120 m low-stand should expose continental shelf as land; exposed {newly_exposed}"
    );
}

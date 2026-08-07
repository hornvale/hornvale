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
    EraAdjust, EraInvariantSupply, SettlementPins, SkyChoice, build_world, climate_of,
    insolation_field, per_species_capacity, per_species_capacity_at, sky_of, substrate_field,
    substrate_field_at, terrain_of,
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
                a.height_asl_m.get().to_bits(),
                b.height_asl_m.get().to_bits(),
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
        if a.height_asl_m.get() < 0.0 && b.height_asl_m.get() >= 0.0 {
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

/// The same no-op guarantee one layer up: `per_species_capacity_at` at the
/// present era must reproduce `per_species_capacity` bit-for-bit.
///
/// This is the layer that matters for worlds — capacity is what settlement
/// placement and the whole deep-history bake read — so "close enough" is not a
/// category that exists here.
#[test]
fn present_era_capacity_is_bit_identical_to_the_unparameterised_field() {
    let wc = hornvale_worldgen::components::WorldComponents::assemble().expect("components");
    for seed in [42, 1234] {
        let (terrain, climate, obliquity_deg, insolation_scalar, regime) = parts(seed);
        let geo = terrain.geosphere();
        let biosphere: Vec<&hornvale_species::BiosphereTraits> =
            ["kobold", "goblin", "hobgoblin", "bugbear", "gnoll", "human"]
                .iter()
                .map(|n| {
                    wc.biosphere
                        .get(&hornvale_kernel::KindId(n))
                        .expect("settler has biosphere traits")
                })
                .collect();

        let direct = per_species_capacity(
            geo,
            &terrain,
            &climate,
            obliquity_deg,
            insolation_scalar,
            &regime,
            &biosphere,
        );
        let hoisted = EraInvariantSupply::build(
            geo,
            &terrain,
            &climate,
            obliquity_deg,
            insolation_scalar,
            &regime,
        );
        let via_era = per_species_capacity_at(
            geo,
            &terrain,
            &climate,
            &hoisted,
            &EraAdjust::present(&terrain),
            &biosphere,
        );

        assert_eq!(direct.len(), via_era.len(), "seed {seed}: species count");
        for ((ta, a), (tb, b)) in direct.iter().zip(via_era.iter()) {
            assert_eq!(ta, tb, "seed {seed}: dense index order moved");
            for cell in geo.cells() {
                assert_eq!(
                    a.at(cell).to_bits(),
                    b.at(cell).to_bits(),
                    "seed {seed} species {ta} cell {cell:?}: capacity moved"
                );
            }
        }
    }
}

/// **The ocean-exclusion guard** (The Tense §3.2, step 2).
///
/// The era habitability mask no longer tests "is this cell land" — that load
/// moved to `carrying_inputs_at`, which takes `is_land` against the era's own
/// sea level. So ocean is now excluded *by capacity being zero there*, and
/// nothing else. The spec named this the most likely place for a silent
/// regression, because a leak here puts settlements at sea and no rule test
/// would notice.
///
/// The argument being asserted has two halves:
///
/// 1. **Eustatic change is ≤ 0** (`sea_level_change_m` = `-EUSTATIC_M * volume`,
///    volume ≥ 0 from ice-free). So an era's sea level never exceeds the
///    present's, and ocean-at-era implies ocean-at-present.
/// 2. Therefore the two supply fields still computed against the *present*
///    shoreline — mineral and detritus — are zero on any cell that is sea at any
///    era, and no shipped kind weights `MARINE_FORAGE`. Every supply term is
///    zero, so capacity is zero.
///
/// If the ice model ever admits a high-stand, half 1 fails and drowned
/// present-land keeps non-zero mineral/detritus supply. This test is what turns
/// that from a silent world-corrupting leak into a red build.
#[test]
fn ocean_is_never_settleable_at_any_era() {
    let wc = hornvale_worldgen::components::WorldComponents::assemble().expect("components");
    for seed in [42, 7, 1234] {
        let (terrain, climate, obliquity_deg, insolation_scalar, regime) = parts(seed);
        let geo = terrain.geosphere();
        let biosphere: Vec<&hornvale_species::BiosphereTraits> =
            ["kobold", "goblin", "hobgoblin", "bugbear", "gnoll", "human"]
                .iter()
                .map(|n| {
                    wc.biosphere
                        .get(&hornvale_kernel::KindId(n))
                        .expect("settler has biosphere traits")
                })
                .collect();
        let hoisted = EraInvariantSupply::build(
            geo,
            &terrain,
            &climate,
            obliquity_deg,
            insolation_scalar,
            &regime,
        );

        // Present, and a deep low-stand well past anything the ice model
        // produces — the direction eustatic change is allowed to move.
        for drop_m in [0.0, 60.0, 130.0, 250.0] {
            let adjust = EraAdjust {
                temp_offset: hornvale_kernel::TempAnomaly::from_offset_c(-6.0),
                sea_level: hornvale_kernel::ReferenceElevation::new(
                    terrain.sea_level().get() - drop_m,
                )
                .expect("a finite low-stand"),
            };
            let caps =
                per_species_capacity_at(geo, &terrain, &climate, &hoisted, &adjust, &biosphere);
            for (tag, cap) in &caps {
                for cell in geo.cells() {
                    // "Sea at this era" is the same predicate `carrying_inputs_at`
                    // uses, spelled out here so the test does not depend on the
                    // private helper.
                    if terrain.elevation_at(cell) < adjust.sea_level {
                        assert_eq!(
                            cap.at(cell),
                            0.0,
                            "seed {seed}, low-stand {drop_m} m, species {tag}, cell {cell:?}: \
                             capacity {} on a cell that is SEA this era — ocean exclusion has \
                             leaked, and settlements will appear at sea",
                            cap.at(cell)
                        );
                    }
                }
            }
        }
    }
}

/// Guards half 1 of the argument above directly, at its source: a rising sea
/// would invalidate the reasoning even if today's seeds never exercise it.
#[test]
fn eustatic_change_never_raises_sea_level() {
    for volume in [0.0, 0.1, 0.5, 0.9, 1.0] {
        let change = hornvale_paleoclimate::ice::sea_level_change_m(volume);
        assert!(
            change <= 0.0,
            "ice volume {volume} produced a sea-level change of {change} m; \
             `ocean_is_never_settleable_at_any_era` assumes eustatic change is \
             never positive, and a high-stand breaks it"
        );
    }
}

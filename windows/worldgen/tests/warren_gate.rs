//! THE WARREN: the placement layer's realm question, and its cave gate.
//!
//! `per_species_suitability` used to build ONE surface substrate field and
//! score every kind against it — fauna included, so a cave-dark,
//! near-saturated-damp kind (rust-monster, xorn) was scored against sunlight
//! and rainfall. This battery is the live-path check that a `Subterranean`
//! kind (`hornvale_species::HabitatRealm::Subterranean`) is now scored
//! against `subterranean_substrate` and gated on whether the cell actually
//! holds a cave (spec §3.1).
//!
//! Test fixture (decision 0092): calls the sculpt/fit derivation entry
//! points directly to build its own world state — copied verbatim from
//! `waterline_probe.rs`'s own fixture, so this battery's numbers stay
//! comparable with that probe's.
#![allow(clippy::disallowed_methods)]

use hornvale_astronomy::SkyPins;
use hornvale_kernel::Seed;
use hornvale_species::HabitatRealm;
use hornvale_terrain::TerrainPins;
use hornvale_worldgen::{
    SettlementPins, SkyChoice, WorldComponents, axis_supply, build_world, carrying_inputs_of,
    climate_of, detritus_supply_field, forage_supply_field, marine_forage_supply_field,
    mineral_supply_field, per_species_suitability, prey_supply_field, sky_of, substrate_field,
    terrain_of,
};

/// Seed 42 at the depth `per_species_suitability` needs (terrain + climate +
/// stellar inputs), plus the assembled component set. Copied verbatim from
/// `windows/worldgen/tests/waterline_probe.rs`'s own fixture setup — it
/// already builds exactly these pieces for exactly this call, and matching
/// it keeps this battery's numbers comparable with that probe's.
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

/// The build-local `(names, realm slice)` pair matching `wc.biosphere`'s
/// ascending-`KindId` order, exactly the assembly `demography_report_with_beta_from`
/// does in the shipped path — so this battery exercises the same wiring.
fn realm_slice(wc: &WorldComponents) -> Vec<HabitatRealm> {
    wc.biosphere
        .iter()
        .map(|(kind, _)| {
            wc.habitat_realm
                .get(kind)
                .copied()
                .unwrap_or(HabitatRealm::SURFACE)
        })
        .collect()
}

#[test]
fn a_subterranean_kind_scores_zero_where_there_is_no_cave() {
    // The keystone (spec 3.1): a declared realm is worth nothing without
    // per-cell availability. ~88% of land cells hold no cave; a subterranean
    // kind must draw no capacity from them.
    let (terrain, climate, obliquity_deg, insolation_scalar, regime, wc) = fixture();
    let geo = terrain.geosphere();

    let names: Vec<&'static str> = wc.biosphere.ids().map(|k| k.0).collect();
    let bio: Vec<&hornvale_species::BiosphereTraits> =
        wc.biosphere.iter().map(|(_, b)| b).collect();
    let realm = realm_slice(&wc);

    let ks = per_species_suitability(
        geo,
        &terrain,
        &climate,
        obliquity_deg,
        insolation_scalar,
        &regime,
        &bio,
        &realm,
    );

    let tag = names
        .iter()
        .position(|n| *n == "rust-monster")
        .expect("rust-monster in the biosphere roster") as u32;
    let k = &ks.iter().find(|(t, _)| *t == tag).unwrap().1;

    let mut land_no_cave_nonzero = 0usize;
    let mut land_cave_nonzero = 0usize;
    let mut land_no_cave = 0usize;
    let mut land_cave = 0usize;
    for cell in geo.cells() {
        if terrain.is_ocean(cell) {
            continue;
        }
        let has_cave = terrain.cave_at(cell).is_some();
        let v = *k.get(cell);
        if has_cave {
            land_cave += 1;
            if v > 0.0 {
                land_cave_nonzero += 1;
            }
        } else {
            land_no_cave += 1;
            if v > 0.0 {
                land_no_cave_nonzero += 1;
            }
        }
    }

    assert!(land_no_cave > 0, "seed 42 must have cave-free land cells");
    assert!(land_cave > 0, "seed 42 must have cave-bearing land cells");
    assert_eq!(
        land_no_cave_nonzero, 0,
        "rust-monster must score exactly 0.0 on every cave-free land cell \
         ({land_no_cave} such cells, {land_no_cave_nonzero} scored nonzero)"
    );
    assert!(
        land_cave_nonzero > 0,
        "rust-monster must score > 0.0 on at least one cave-bearing land cell \
         (of {land_cave} such cells, {land_cave_nonzero} scored nonzero)"
    );
}

#[test]
fn a_surface_kind_is_bit_identical_to_the_pre_campaign_arithmetic() {
    // Spec 3.5. Any world movement must be attributable to the two re-homed
    // kinds and nothing else. Score goblin with the realm slice present and
    // compare, by `f64::to_bits`, against a single-substrate reference
    // expression inlined here — the pre-campaign formula, hand-rebuilt from
    // the same public helpers `per_species_suitability` itself calls, rather
    // than a second call into the function under test.
    let (terrain, climate, obliquity_deg, insolation_scalar, regime, wc) = fixture();
    let geo = terrain.geosphere();

    let bio: Vec<&hornvale_species::BiosphereTraits> =
        wc.biosphere.iter().map(|(_, b)| b).collect();
    let realm = realm_slice(&wc);

    let ks = per_species_suitability(
        geo,
        &terrain,
        &climate,
        obliquity_deg,
        insolation_scalar,
        &regime,
        &bio,
        &realm,
    );

    let names: Vec<&'static str> = wc.biosphere.ids().map(|k| k.0).collect();
    let tag = names
        .iter()
        .position(|n| *n == "goblin")
        .expect("goblin in the biosphere roster") as u32;
    let k_live = &ks.iter().find(|(t, _)| *t == tag).unwrap().1;

    let goblin = wc
        .biosphere
        .get_by_label("goblin")
        .expect("goblin has a biosphere row");
    assert_eq!(
        wc.habitat_realm.get(&hornvale_kernel::KindId("goblin")),
        None,
        "goblin must be absent from the sparse habitat-realm store (Surface by default)"
    );

    // The pre-campaign formula: the SAME public per-axis supply helpers and
    // the SAME `substrate_field`, with no realm question and no availability
    // factor — exactly what `per_species_suitability` computed before this
    // task for every kind, surface or not.
    let base_inputs = carrying_inputs_of(geo, &terrain, &climate);
    let base_carrying = hornvale_demography::carrying_capacity(geo, &base_inputs);
    let substrate = substrate_field(
        geo,
        &terrain,
        &climate,
        obliquity_deg,
        insolation_scalar,
        &regime,
    );
    let mineral = mineral_supply_field(geo, &terrain, 1.0);
    let forage = forage_supply_field(geo, base_carrying.as_cell_map());
    let detritus = detritus_supply_field(geo, &terrain);
    let marine = marine_forage_supply_field(geo, &terrain, &climate, 1.0);
    let prey = prey_supply_field(geo, &forage);

    let floor_buf = hornvale_kernel::sovereignty_floor(goblin.mass, goblin.potency);
    let cn = &goblin.condition_niche;

    let mut mismatches = 0usize;
    for cell in geo.cells() {
        let s = substrate.get(cell);
        use hornvale_kernel::{
            ANIMAL_PREY, DETRITUS, MARINE_FORAGE, MINERAL, PHOTOSYNTHATE, PLANT_FORAGE,
        };
        let per_axis = [
            (PHOTOSYNTHATE, base_carrying.at(cell)),
            (PLANT_FORAGE, *forage.get(cell)),
            (MINERAL, *mineral.get(cell)),
            (DETRITUS, *detritus.get(cell)),
            (ANIMAL_PREY, *prey.get(cell)),
            (MARINE_FORAGE, *marine.get(cell)),
        ];
        let supply = axis_supply(&goblin.niche, &per_axis);
        let saturated = supply / (1.0 + supply);
        let k_ref = saturated
            * cn.temperature.eval(s.temperature_c, floor_buf)
            * cn.moisture.eval(s.moisture, floor_buf)
            * cn.insolation.eval(s.insolation, floor_buf)
            * cn.elevation.eval(s.height_asl_m.get(), 0.0);

        let k_live_v = *k_live.get(cell);
        if k_ref.to_bits() != k_live_v.to_bits() {
            mismatches += 1;
        }
    }
    assert_eq!(
        mismatches, 0,
        "goblin's live K must be bit-identical to the pre-campaign single-substrate \
         formula at every cell ({mismatches} mismatches)"
    );
}

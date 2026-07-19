//! BIO-35 Stage 1 (The Demesne, task T1): per-axis resource-supply field
//! builders. These are pure builders only — nothing yet consumes them
//! (`niche_per_species_k` still collapses uptake to a scalar sum; T2 wires
//! these fields into the dot product).

use hornvale_kernel::{CellId, CellMap, Geosphere};

#[test]
fn mineral_supply_tracks_prospectivity_spatially() {
    // On a real seed-42 world, the mineral field peaks where prospectivity
    // peaks and is 0 where prospectivity is 0 — a genuinely SPATIAL field,
    // not a constant.
    let world = hornvale_worldgen::build_world(
        hornvale_kernel::Seed(42),
        &hornvale_astronomy::SkyPins::default(),
        hornvale_worldgen::SkyChoice::Generated,
        &hornvale_terrain::TerrainPins::default(),
        &hornvale_worldgen::SettlementPins::default(),
    )
    .unwrap();

    // Reach the terrain handle the way `niche_per_species_k`'s callers do
    // (`terrain_of`), then its geosphere — the single construction site for
    // the terrain provider on a built world.
    let terrain = hornvale_worldgen::terrain_of(&world).unwrap();
    let geo = terrain.geosphere();

    let scale = 10.0;
    let field = hornvale_worldgen::mineral_supply_field(geo, &terrain, scale);

    // Genuinely spatial: at least two distinct values across cells.
    let mut distinct: Vec<f64> = Vec::new();
    for c in geo.cells() {
        let v = *field.get(c);
        if !distinct.iter().any(|d: &f64| (*d - v).abs() < 1e-12) {
            distinct.push(v);
        }
        if distinct.len() >= 2 {
            break;
        }
    }
    assert!(
        distinct.len() >= 2,
        "mineral supply field must vary across cells, not be a constant"
    );

    // Monotone in prospectivity at two probe cells: whichever cell has
    // higher prospectivity must have a proportionally higher supply value
    // (field = prospectivity * scale, so equality up to float epsilon).
    let probe_a = CellId(0);
    let probe_b = CellId(geo.cells().count() as u32 / 2);
    let prospectivity_a = terrain.prospectivity_at(probe_a);
    let prospectivity_b = terrain.prospectivity_at(probe_b);
    let field_a = *field.get(probe_a);
    let field_b = *field.get(probe_b);
    assert!((field_a - prospectivity_a * scale).abs() < 1e-9);
    assert!((field_b - prospectivity_b * scale).abs() < 1e-9);
    match prospectivity_a.total_cmp(&prospectivity_b) {
        std::cmp::Ordering::Less => assert!(field_a < field_b),
        std::cmp::Ordering::Greater => assert!(field_a > field_b),
        std::cmp::Ordering::Equal => assert!((field_a - field_b).abs() < 1e-9),
    }

    // Bounds: prospectivity is [0,1], so the field is [0, scale].
    for c in geo.cells() {
        let v = *field.get(c);
        assert!(
            (0.0..=scale + 1e-9).contains(&v),
            "mineral supply out of range: {v}"
        );
    }
}

#[test]
fn forage_supply_is_a_fraction_of_base_carrying_and_deterministic() {
    let geo = Geosphere::new(3);
    let base = CellMap::from_fn(&geo, |c| (c.0 as f64) * 0.1);
    let a = hornvale_worldgen::forage_supply_field(&geo, &base);
    let b = hornvale_worldgen::forage_supply_field(&geo, &base);
    for c in geo.cells() {
        assert_eq!(a.get(c), b.get(c));
        assert!(
            *a.get(c) <= *base.get(c),
            "forage is a fraction of primary production"
        );
    }
}

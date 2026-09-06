use hornvale_astronomy::SkyPins;
use hornvale_kernel::Seed;
use hornvale_terrain::TerrainPins;
use hornvale_worldgen::{SettlementPins, bake_era_population_view, build_world};

#[test]
fn population_substrate_is_a_read_only_era_site_view() {
    let world = build_world(
        Seed(1),
        &SkyPins::default(),
        &TerrainPins::default(),
        &SettlementPins::default(),
    )
    .expect("seed builds");
    let view = bake_era_population_view(&world).expect("population view derives");
    let first = view.rows().next().expect("history has a population row");
    assert!(first.population > 0.0);
    assert_eq!(
        view.population_at(first.era_start, first.site),
        first.population
    );
}

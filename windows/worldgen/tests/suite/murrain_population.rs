use hornvale_astronomy::SkyPins;
use hornvale_history::record::Founding;
use hornvale_history::trajectory::{population_at, shape_of};
use hornvale_kernel::Seed;
use hornvale_terrain::TerrainPins;
use hornvale_worldgen::{
    DAUGHTER_POP, GENESIS_POP, SettlementPins, bake_era_graphs, bake_era_population_view,
    bake_era_population_view_from, build_world,
};

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

    let records = hornvale_worldgen::occupation_records(&world);
    let now = hornvale_worldgen::present_year(&world);
    let next_era = view
        .rows()
        .find(|row| row.era_start > first.era_start)
        .map_or(now, |row| row.era_start);
    let midpoint = first.era_start + (next_era - first.era_start) / 2.0;
    let expected: f64 = records
        .iter()
        .filter(|occupation| {
            occupation.core.founded < next_era
                && occupation.core.ended.unwrap_or(now) > first.era_start
                && occupation.core.site == first.site
        })
        .map(|occupation| {
            let opening = match occupation.founded_from {
                Founding::Genesis(_) => GENESIS_POP,
                Founding::From(_) => DAUGHTER_POP,
            };
            let shape = shape_of(
                occupation.core.founded,
                occupation.core.ended.unwrap_or(now),
                occupation.core.peak_population,
                occupation.core.person_years,
                opening,
            );
            population_at(&shape, midpoint)
        })
        .sum();
    assert!((first.population - expected).abs() < 1e-9 * expected.max(1.0));
}

#[test]
fn population_substrate_reuses_prederived_era_boundaries_byte_for_byte() {
    let world = hornvale_worldgen::seed_42_world();
    let graphs = bake_era_graphs(&world).expect("era graphs derive");
    let shared = bake_era_population_view_from(&world, &graphs);
    let standalone = bake_era_population_view(&world).expect("population view derives");
    assert_eq!(shared, standalone);
}

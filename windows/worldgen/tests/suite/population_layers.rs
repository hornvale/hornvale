use hornvale_astronomy::SkyPins;
use hornvale_kernel::Seed;
use hornvale_terrain::TerrainPins;
use hornvale_worldgen::{SettlementPins, build_world, occupation_records, population_census};
use std::collections::BTreeSet;

fn seed_42() -> hornvale_kernel::World {
    build_world(
        Seed(42),
        &SkyPins::default(),
        &TerrainPins::default(),
        &SettlementPins::default(),
    )
    .expect("seed 42 builds")
}

/// Break caught: historical occupation records are silently reduced to the
/// living present, or occupied columns are omitted from either named layer.
#[test]
fn census_distinguishes_historical_and_present_population_layers() {
    let world = seed_42();
    let census = population_census(&world);
    let records = occupation_records(&world);
    let historical_occupations: BTreeSet<_> =
        records.iter().map(|occupation| occupation.id).collect();
    let present_occupations: BTreeSet<_> = records
        .iter()
        .filter(|occupation| occupation.is_alive())
        .map(|occupation| occupation.id)
        .collect();
    let ended_occupations: BTreeSet<_> = records
        .iter()
        .filter(|occupation| !occupation.is_alive())
        .map(|occupation| occupation.id)
        .collect();
    let historical_columns: BTreeSet<_> = records
        .iter()
        .map(|occupation| occupation.core.site)
        .collect();
    let present_columns: BTreeSet<_> = records
        .iter()
        .filter(|occupation| occupation.is_alive())
        .map(|occupation| occupation.core.site)
        .collect();

    assert!(
        !ended_occupations.is_empty(),
        "seed 42 must retain at least one ended occupation"
    );
    assert!(
        census
            .present_occupations
            .is_subset(&census.historical_occupations)
    );
    assert!(census.present_columns.is_subset(&census.historical_columns));
    assert!(present_occupations.is_subset(&historical_occupations));
    assert!(ended_occupations.is_subset(&historical_occupations));
    assert!(present_occupations.is_disjoint(&ended_occupations));
    assert!(present_columns.is_subset(&historical_columns));
    assert_eq!(census.historical_occupations, historical_occupations);
    assert_eq!(census.present_occupations, present_occupations);
    assert_eq!(census.historical_columns, historical_columns);
    assert_eq!(census.present_columns, present_columns);
    assert_eq!(
        census.historical_occupations.len(),
        census.present_occupations.len() + ended_occupations.len(),
        "ended occupations remain represented in the historical layer"
    );
    assert!(
        !census.present_occupations.is_empty(),
        "seed 42 has living occupations"
    );
    assert!(
        census.present_occupations.len() <= census.historical_occupations.len(),
        "present occupations are drawn from historical occupation records"
    );
    assert!(
        !census.historical_columns.is_empty(),
        "seed 42 has historical occupied columns"
    );
    assert!(
        !census.present_columns.is_empty(),
        "seed 42 has present occupied columns"
    );
    assert!(
        census.present_columns.len() <= census.historical_columns.len(),
        "present occupied columns are drawn from historical occupied columns"
    );
}

use hornvale_epidemiology::{critical_community_size, persists};
use hornvale_species::pathogen_registry;

#[test]
fn h_m1_binds_catalogue_rows_and_uses_the_monotonicity_grid() {
    let rows = pathogen_registry();
    let catalogue_ccs = |name: &str| {
        let traits = rows
            .iter()
            .find(|(kind, _)| kind.0 == name)
            .map(|(_, traits)| traits)
            .unwrap_or_else(|| panic!("missing authored pathogen {name}"));
        critical_community_size(
            traits.r0.expect("CCS-bearing row has R0"),
            traits
                .infectious_years
                .expect("CCS-bearing row has infectious period"),
            1.0 / 30.0,
        )
    };
    assert!((3_000.0..=5_000.0).contains(&catalogue_ccs("the-consumption")));
    assert!((200_000.0..=400_000.0).contains(&catalogue_ccs("the-pest")));
    assert!((120_000.0..=200_000.0).contains(&catalogue_ccs("the-pox")));

    let r0_grid = [1.5, 2.0, 3.0, 6.0, 15.0];
    let infectious_years_grid = [0.01, 0.027, 0.038, 0.1, 2.0];
    let birth_rate_grid = [1.0 / 50.0, 1.0 / 40.0, 1.0 / 30.0, 1.0 / 20.0];
    for &r0 in &r0_grid {
        for &infectious_years in &infectious_years_grid {
            for &birth_rate in &birth_rate_grid {
                let base = critical_community_size(r0, infectious_years, birth_rate);
                assert!(critical_community_size(r0 + 0.1, infectious_years, birth_rate) < base);
                assert!(critical_community_size(r0, infectious_years + 0.01, birth_rate) < base);
                assert!(critical_community_size(r0, infectious_years, birth_rate + 0.01) < base);
            }
        }
    }
}

#[test]
fn h_m2_exercises_both_sides_for_each_authored_ccs_row() {
    let rows = pathogen_registry();
    for name in ["the-consumption", "the-pest", "the-pox"] {
        let traits = rows
            .iter()
            .find(|(kind, _)| kind.0 == name)
            .map(|(_, traits)| traits)
            .expect("authored CCS row");
        let ccs = critical_community_size(
            traits.r0.expect("CCS-bearing row has R0"),
            traits
                .infectious_years
                .expect("CCS-bearing row has infectious period"),
            1.0 / 30.0,
        );
        assert!(persists(2.0 * ccs, ccs), "{name} must persist at 2x CCS");
        assert!(!persists(ccs / 2.0, ccs), "{name} must fade at CCS/2");
    }
}

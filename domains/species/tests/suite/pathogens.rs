use hornvale_species::{PathogenClass, pathogen_concept, pathogen_registry};

#[test]
fn pathogen_catalogue_has_five_rows_and_two_epidemic_kinds() {
    let registry = pathogen_registry();
    assert_eq!(registry.len(), 5);
    assert_eq!(
        registry
            .iter()
            .filter(|(_, traits)| matches!(
                traits.class,
                PathogenClass::Zoonotic | PathogenClass::Crowd
            ))
            .count(),
        2
    );
    for name in [
        "the-flux",
        "the-consumption",
        "the-marsh-fever",
        "the-pest",
        "the-pox",
    ] {
        assert!(
            pathogen_concept(name).is_some(),
            "missing concept for {name}"
        );
    }
}

#[test]
fn pathogen_host_weights_cover_all_fifteen_peoples() {
    for (_, traits) in pathogen_registry().iter() {
        assert_eq!(traits.hosts.len(), 15);
        assert!(traits.hosts.iter().all(|(_, weight)| *weight == 1.0));
    }
}

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

/// The host list is exactly the SETTLING roster, and it grows with it.
///
/// THE TIDEMARK takes it from fifteen to twenty, adding the five `Settled`
/// marine peoples. `merfolk` is deliberately absent: it is a people and it
/// forms no fixed place, so admitting it would be the first non-settling row
/// in this table — a modelling decision about whether a nomadic band
/// sustains a crowd disease, which is a real question and not one that
/// campaign measured. See `pathogen_hosts` in `domains/species/src/lib.rs`.
///
/// The name no longer carries the count: a count baked into a test name is
/// how the next campaign inherits a wrong one.
#[test]
fn pathogen_host_weights_cover_the_settling_roster() {
    for (_, traits) in pathogen_registry().iter() {
        assert_eq!(traits.hosts.len(), 20);
        assert!(traits.hosts.iter().all(|(_, weight)| *weight == 1.0));
    }
}

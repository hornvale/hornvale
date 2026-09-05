//! The Warrant (Penstock 7b): the typed, compositional intention.
use hornvale_vessel::liveness::{
    ERRAND_COMFORT, ERRAND_COMPANY, ERRAND_FLIGHT, ERRAND_FORAGE, ERRAND_HOME, ERRAND_REST,
    ERRAND_WATER_BLIND, ERRAND_WATER_KNOWN, errand_predicates,
};

/// Every key is registered with a non-empty doc, and the docs are the eight
/// glosses the renderer will show. A key with an empty doc would render as
/// the bare predicate string in `recount`, which is the failure mode this
/// campaign exists to remove.
#[test]
fn every_errand_predicate_carries_a_distinct_non_empty_doc() {
    let table = errand_predicates();
    assert_eq!(table.len(), 8);
    let mut keys: Vec<&str> = table.iter().map(|(k, _)| *k).collect();
    keys.sort_unstable();
    keys.dedup();
    assert_eq!(keys.len(), 8, "keys are distinct");
    let mut docs: Vec<&str> = table.iter().map(|(_, d)| *d).collect();
    docs.sort_unstable();
    docs.dedup();
    assert_eq!(docs.len(), 8, "docs are distinct");
    for (key, doc) in table {
        assert!(!doc.is_empty(), "{key} has an empty doc");
        assert!(
            key.starts_with("errand/"),
            "{key} is not in the errand namespace"
        );
    }
}

/// SAVE-FORMAT CONTRACT. These eight strings are permanent on-disk keys, the
/// same way `agent-at` is (`liveness.rs`'s
/// `the_agent_at_predicate_spelling_is_a_permanent_on_disk_key`). Do not
/// rebaseline this literal — take an epoch.
#[test]
fn the_errand_predicate_spellings_are_permanent_on_disk_keys() {
    assert_eq!(ERRAND_WATER_KNOWN, "errand/water-known");
    assert_eq!(ERRAND_WATER_BLIND, "errand/water-blind");
    assert_eq!(ERRAND_FORAGE, "errand/forage");
    assert_eq!(ERRAND_COMFORT, "errand/comfort");
    assert_eq!(ERRAND_REST, "errand/rest");
    assert_eq!(ERRAND_FLIGHT, "errand/flight");
    assert_eq!(ERRAND_COMPANY, "errand/company");
    assert_eq!(ERRAND_HOME, "errand/home");
}

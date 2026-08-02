//! The accession register's parity check (The Accession, Stage 3).
//!
//! `EPOCH_COHORTS` is authored, and an authored table has exactly one failure
//! mode: a forgotten row. A concept registered with no cohort entry silently
//! defaults to epoch 0 (`concept_epoch`'s fail-safe), sorts back into the
//! epoch-0 block mid-alphabet, and re-opens the churn the campaign closed —
//! the same silent drift as the `*-kind` roster that went four campaigns
//! without noticing twelve new species.
//!
//! So the table and the registry must agree, in **both** directions: a
//! missing entry re-opens the churn, and a stray entry means a typo'd or
//! renamed concept whose real name is therefore also missing. Neither is
//! reachable by reading the draw path, so it is asserted here.

use hornvale_kernel::ConceptRegistry;
use hornvale_language::EPOCH_COHORTS;
use hornvale_worldgen::register_all;
use std::collections::BTreeSet;

/// Every concept the roster registers.
fn registered() -> BTreeSet<String> {
    let mut registry = ConceptRegistry::default();
    register_all(&mut registry).expect("register_all should register every domain's concepts");
    registry.concepts().map(|c| c.name.clone()).collect()
}

/// Every concept named by any cohort.
fn accessioned() -> BTreeSet<String> {
    EPOCH_COHORTS
        .iter()
        .flat_map(|cohort| cohort.iter())
        .map(|name| (*name).to_string())
        .collect()
}

#[test]
fn every_registered_concept_has_an_accession_epoch() {
    let missing: Vec<String> = registered().difference(&accessioned()).cloned().collect();
    assert!(
        missing.is_empty(),
        "these concepts are registered but appear in no cohort of \
         `hornvale_language::EPOCH_COHORTS`: {missing:?}\n\n\
         They would silently default to epoch 0 and re-open the proto-root \
         churn The Accession closed. The fix is to APPEND them to a new \
         cohort at the end of the table — never to edit an existing cohort, \
         which would re-sort concepts that already have assignments."
    );
}

#[test]
fn every_accessioned_concept_is_actually_registered() {
    let stray: Vec<String> = accessioned().difference(&registered()).cloned().collect();
    assert!(
        stray.is_empty(),
        "these names appear in `EPOCH_COHORTS` but are not registered \
         concepts: {stray:?}\n\n\
         A stray entry is normally a typo or a rename — in which case the \
         concept's real name is missing from the table too, and is silently \
         at epoch 0."
    );
}

/// Anti-vacuity: the two directions above are set differences, and both would
/// pass trivially if the registry were empty.
#[test]
fn the_parity_check_is_over_a_non_empty_roster() {
    let registered = registered();
    assert!(
        registered.len() >= 76,
        "the roster should hold at least the 76 concepts of the epoch-0 \
         cohort; found {}",
        registered.len()
    );
    assert_eq!(
        registered,
        accessioned(),
        "registry and accession register must agree exactly"
    );
}

/// The Wearing: every toponymic concept the campaign added is registered
/// and accessioned. All nineteen — the nine terrain concepts plus the ten
/// relative/evaluative modifiers.
#[test]
fn the_toponymic_concepts_are_registered_and_accessioned() {
    const TOPONYMIC: &[&str] = &[
        "hill", "river", "lake", "valley", "coast", "island", "ford", "marsh", "spring", "high",
        "low", "great", "little", "new", "old", "under", "over", "north", "south",
    ];
    let registered = registered();
    let accessioned = accessioned();
    for concept in TOPONYMIC {
        assert!(registered.contains(*concept), "{concept} is not registered");
        assert!(
            accessioned.contains(*concept),
            "{concept} has no accession epoch"
        );
    }
}

/// The Wearing (Task 4 review, Important 5): every toponymic concept that
/// can ever win a `Root` in `windows/worldgen::exposure_from` must be core —
/// a periphery concept sorts after core inside its shared accession epoch
/// and takes a longer form, which for `hill` and `river` (the
/// highest-frequency morphemes in the name corpus) is exactly backwards.
/// Not all nineteen: `coast` and `lake` are `KnowsOf`-only by construction
/// (a culture can know a shore or a salt basin without living on either),
/// so neither ever reaches the `Steeped` pass that assigns roots — every
/// occurrence is a `Compound` or a `Gap`, never a `Root` — and short-form
/// priority for a concept that can never hold a root only tightens the
/// minimal-pair/merger constraints on every OTHER core root for no benefit.
/// They are periphery instead, alongside `sea`/`mountain` (the same
/// `KnowsOf`-only shape). The ten modifiers are `Steeped` unconditionally
/// (the universal stratum), so all ten are core.
#[test]
fn the_rootable_toponymic_concepts_are_core() {
    const CORE_TOPONYMIC: &[&str] = &[
        "hill", "river", "valley", "island", "ford", "marsh", "spring", "high", "low", "great",
        "little", "new", "old", "under", "over", "north", "south",
    ];
    const PERIPHERY_TOPONYMIC: &[&str] = &["coast", "lake"];
    for concept in CORE_TOPONYMIC {
        assert!(
            hornvale_language::packs::is_core_concept(concept),
            "{concept} is periphery; it must be core to win a short form"
        );
    }
    for concept in PERIPHERY_TOPONYMIC {
        assert!(
            !hornvale_language::packs::is_core_concept(concept),
            "{concept} is KnowsOf-only (never wins a root) and should stay periphery"
        );
    }
}

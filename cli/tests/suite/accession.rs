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
//!
//! # This module IS The Plumb's Task 2 gate
//!
//! Decision 0556 asks that every registry-totality check name its own
//! direction rather than leave it inferred. This file already carries the
//! pair `every_registered_concept_has_an_accession_epoch` (registered ⊆
//! cohorted — the direction whose absence lets a concept silently default to
//! epoch 0) and `every_accessioned_concept_is_actually_registered` (cohorted
//! ⊆ registered — a stray or renamed entry), each doc-commented with its own
//! direction below. Neither half can be repaired by deleting the other:
//! deleting the first re-opens the silent-epoch-0 hole; deleting the second
//! lets a typo sit in the table forever, unioned into the *cohorted* set but
//! resolving to a name no domain has ever registered.
//!
//! When The Plumb's Task 2 brief was written it asked for a **new** file,
//! `cli/tests/suite/accession_coverage.rs`, built by reading
//! `cli/src/concepts.rs` for a registry constructor. Reading `cli/CLAUDE.md`'s
//! own enforcement-test index first would have found this file already
//! documented there as exactly that gate ("Red = you registered a concept and
//! didn't file it"), predating the campaign by two weeks (`git log`:
//! `89bc9fbfc`, 2026-08-19, from The Accession). The Wicket's ledger #27
//! confirms it is not merely present but already load-bearing: Task 5 there
//! found, *by running the suite*, that a newly-registered kind ("brazier")
//! needed a cohort entry, and fixed the data rather than the gate. Building a
//! second copy of this check would create exactly the drift hazard a
//! duplicated rule invites — two gates asserting the same direction that can
//! silently diverge — so Task 2 closes here instead, by bringing this file up
//! to the campaign's own documentation standard.

use hornvale_kernel::ConceptRegistry;
use hornvale_language::EPOCH_COHORTS;
use hornvale_worldgen::register_all;
use std::collections::BTreeSet;

/// Every concept the roster registers.
///
/// Built via `register_all` on a bare registry rather than a full genesis
/// world (`hornvale_worldgen::build_world`), and this is population-
/// equivalent, not a scoped subset: `ConceptRegistry::register_manifest` is
/// the *only* path that inserts a concept (`insert_concept` is private to
/// it), every domain's `register_concepts` is a pure function of static
/// definitions with no dependence on world state or seed, and the only
/// concepts `register_all` does not itself register — `World::new`'s three
/// other kernel-core predicates alongside `NAME_GLOSS` — are registered as
/// *predicates* only and never carry a `Manifest`, so they were never part of
/// `registry.concepts()`'s population under either construction. A full
/// genesis build cannot see a concept this sweep misses.
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

/// **Direction: registered ⊆ cohorted.** The converse (cohorted ⊆
/// registered) is the next test below; `no_concept_appears_in_two_cohorts`
/// (`domains/language/src/accession.rs`) covers a third, orthogonal
/// direction (the table's own internal well-formedness). All three are
/// two-way pairs or standalone invariants — none is repaired by deleting
/// another.
///
/// MUTATION THIS MUST FAIL AGAINST (confirmed 2026-09-02, The Plumb Task 2):
/// `python3 scripts/mutate.py domains/language/src/accession.rs '"brazier"'
/// '"brazier-x"'` — compiles, every other accession test stays green, and
/// this one reddens:
///
/// ```text
/// these concepts are registered but appear in no cohort of
/// `hornvale_language::EPOCH_COHORTS`: ["brazier"]
/// ```
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

/// **Direction: cohorted ⊆ registered.** The converse of the test above.
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
///
/// The floor is **absolute, not relative** (`>= 253`, not `> 0`): a relative
/// check passes on an empty registry (The Wicket catalogued that failure mode
/// twice), where an absolute one pinned to the actual swept population does
/// not. 253 is `registered().len()` as measured 2026-09-02 (The Plumb Task
/// 2) — also `EPOCH_COHORTS`'s total entry count across its 19 cohorts, since
/// `the_parity_check_is_over_a_non_empty_roster` below additionally asserts
/// exact set equality. A future registration wave that grows the roster past
/// 253 moves this number deliberately, in the same commit as the new cohort.
#[test]
fn the_parity_check_is_over_a_non_empty_roster() {
    let registered = registered();
    assert!(
        registered.len() >= 253,
        "the roster should hold at least the 253 concepts registered as of \
         The Plumb (2026-09-02); found {}. If this is a deliberate \
         registration wave, raise the floor in the same commit as the new \
         cohort.",
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

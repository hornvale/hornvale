//! The Warrant (Penstock 7b): the typed, compositional intention.
use crate::common;
use hornvale_vessel::liveness::{
    AGENT_AT, ERRAND_COMFORT, ERRAND_COMPANY, ERRAND_FLIGHT, ERRAND_FORAGE, ERRAND_HOME,
    ERRAND_REST, ERRAND_WATER_BLIND, ERRAND_WATER_KNOWN, errand_predicates,
};
use hornvale_vessel::{PossessOpts, Session};

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

/// The seed and wait count for Task 2's walking harness — `the_kerf.rs`'s
/// `CHEAP_WATER_BELIEF_SEED`/`WITNESS_WAITS` shape, copied rather than
/// imported (each file keeps its own copy of a seed constant, per that
/// file's own doc). Seed 42 commits no `agent-at` fact at all over 90
/// sim-days, which would make either test below vacuous (spec §1).
/// type-audit: bare-ok(index)
const WARRANT_WALK_SEED: u64 = 11;
/// type-audit: bare-ok(count)
const WARRANT_WALK_WAITS: usize = 12;

/// One committed fact, as read back out of the session's own ledger JSON —
/// the same public read `tick_commit_budget.rs` uses for subject and
/// provenance (its own comment names `session_ledger_json` for exactly this
/// purpose), widened here to also carry `predicate` and `day`.
struct ParsedFact {
    /// The fact's subject, as the JSON renders it (`EntityId` is a bare
    /// number at this boundary).
    subject: String,
    /// The predicate name.
    predicate: String,
    /// The committed day, in ticks. Every fact this file reads (`agent-at`
    /// and `errand/*`) always carries one, so a missing `day` reads as
    /// `i64::MIN` rather than panicking — a fact that shape would fail the
    /// coverage assertion honestly instead of aborting the test.
    day: i64,
    /// Free-form provenance string.
    provenance: String,
}

/// One fresh walk's committed facts, parsed out of the session's ledger
/// JSON. `WARRANT_WALK_SEED` over `WARRANT_WALK_WAITS` waits is the cheapest
/// harness the sweep in `the_kerf.rs`'s module doc found that forces a real,
/// multi-step walk — this task builds no new world.
fn walk_facts(seed: u64, waits: usize) -> Vec<ParsedFact> {
    let world = common::build(seed).expect("the pinned seed builds a world");
    let (mut session, _) =
        Session::start(&world, &PossessOpts::default()).expect("the pinned seed starts a session");
    for _ in 0..waits {
        session.handle("wait");
    }
    let doc: serde_json::Value =
        serde_json::from_str(&session.session_ledger_json()).expect("a ledger serializes");
    doc["facts"]
        .as_array()
        .expect("the ledger carries facts")
        .iter()
        .map(|f| ParsedFact {
            subject: f["subject"].to_string(),
            predicate: f["predicate"].as_str().unwrap_or("").to_string(),
            day: f["day"].as_i64().unwrap_or(i64::MIN),
            provenance: f["provenance"].as_str().unwrap_or("").to_string(),
        })
        .collect()
}

/// A creature that walks commits ONE errand fact per errand — a maximal run
/// of constant reason — and never one per step. The synthetic walk harness is
/// used rather than seed 42, which commits no `agent-at` at all (spec §1).
#[test]
fn an_errand_commits_once_and_its_steps_commit_under_it() {
    let facts = walk_facts(WARRANT_WALK_SEED, WARRANT_WALK_WAITS);
    let steps: Vec<&ParsedFact> = facts.iter().filter(|f| f.predicate == AGENT_AT).collect();
    let errands: Vec<&ParsedFact> = facts
        .iter()
        .filter(|f| f.predicate.starts_with("errand/"))
        .collect();
    assert!(
        !steps.is_empty(),
        "the harness must walk, or this test is vacuous"
    );
    assert!(!errands.is_empty(), "a walk commits at least one errand");
    assert!(
        errands.len() < steps.len(),
        "errands ({}) must be sparser than steps ({})",
        errands.len(),
        steps.len()
    );

    // The load-bearing assertion: every step is covered — there is an errand
    // fact for the same subject at or before the step's day.
    for step in &steps {
        let covering = errands
            .iter()
            .rfind(|e| e.subject == step.subject && e.day <= step.day);
        assert!(
            covering.is_some(),
            "step at day {} (subject {}) has no covering errand",
            step.day,
            step.subject
        );
    }
}

/// The count of errand facts equals the count of REASON CHANGES in the trail
/// — this is the losslessness claim of spec §1, asserted mechanically rather
/// than argued. Compared against the provenance runs the steps still carry
/// (Task 2 has not flipped them yet), which is why this test is written NOW
/// and not after Task 3.
#[test]
fn one_errand_fact_per_run_of_constant_step_provenance() {
    let facts = walk_facts(WARRANT_WALK_SEED, WARRANT_WALK_WAITS);
    let mut subjects: Vec<&str> = facts.iter().map(|f| f.subject.as_str()).collect();
    subjects.sort_unstable();
    subjects.dedup();
    assert!(
        !subjects.is_empty(),
        "the harness must derive at least one subject, or this test is vacuous"
    );
    for entity in subjects {
        let provs: Vec<&str> = facts
            .iter()
            .filter(|f| f.subject == entity && f.predicate == AGENT_AT)
            .map(|f| f.provenance.as_str())
            .collect();
        let runs =
            provs.windows(2).filter(|w| w[0] != w[1]).count() + usize::from(!provs.is_empty());
        let errands = facts
            .iter()
            .filter(|f| f.subject == entity && f.predicate.starts_with("errand/"))
            .count();
        assert_eq!(
            errands, runs,
            "entity {entity}: {errands} errand facts against {runs} provenance runs"
        );
    }
}

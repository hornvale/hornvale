//! The Warrant (Penstock 7b): the typed, compositional intention.
use crate::common;
use hornvale_vessel::liveness::{
    AGENT_AT, ERRAND_COMFORT, ERRAND_COMPANY, ERRAND_FLIGHT, ERRAND_FORAGE, ERRAND_HOME,
    ERRAND_PRODUCER, ERRAND_REST, ERRAND_WATER_BLIND, ERRAND_WATER_KNOWN, errand_predicates,
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

/// THE KEY→GLOSS PAIRING, PINNED AS A LITERAL FOR ALL EIGHT (fix round 1).
///
/// **Nothing pinned this for half the errands, and the hole was opened by a
/// deletion that was itself correct.** Until Task 3, `liveness.rs`'s
/// `the_registry_glosses_and_the_live_prose_match_agree_both_ways` compared
/// each `Mode`'s authored prose against its own table entry, pairwise through
/// `errand_key` — its doc says in as many words that a SET comparison would
/// miss a swap, and that once `prose_for` was deleted a swap latent at that
/// moment would become permanent and undetectable. `prose_for` is now gone, so
/// that test went with it, and its own prediction came true.
///
/// **What survived it is thinner than it looks.**
/// `every_mode_maps_to_exactly_one_errand_key` pins `Mode` → key for all
/// eight. `the_errand_predicate_spellings_are_permanent_on_disk_keys` pins the
/// eight key strings. `every_errand_predicate_carries_a_distinct_non_empty_doc`
/// checks only distinctness and non-emptiness — **which a swap satisfies
/// exactly**: exchange two glosses and the keys are still eight, the docs are
/// still eight distinct non-empty strings, and every one of those tests stays
/// green. `every_gloss_and_its_first_day_survives_the_flip` pins the pairing
/// for real, but only for the glosses its four seeds actually produce.
///
/// So this table is the pin, and it is written as a literal for the same
/// reason the spelling test is: a doc string that reaches a reader through
/// `recount` is a published surface, and reading it back out of
/// `errand_predicates()` to compare against itself would assert nothing. Do
/// not rebaseline it — if a gloss should change, change it here and in
/// `errand_predicates` together, deliberately.
#[test]
fn every_errand_key_carries_exactly_its_own_gloss() {
    const PAIRS: [(&str, &str); 8] = [
        (
            ERRAND_WATER_KNOWN,
            "went down to the river it knew (thirst)",
        ),
        (
            ERRAND_WATER_BLIND,
            "wandered, having found no water yet (thirst)",
        ),
        (ERRAND_FORAGE, "foraged toward richer ground (hunger)"),
        (ERRAND_COMFORT, "sought a kinder clime (comfort)"),
        (ERRAND_REST, "turned home, weary, to rest"),
        (ERRAND_FLIGHT, "fled the uncanny ground (fear)"),
        (
            ERRAND_COMPANY,
            "drifted homeward, missing its people (belonging)",
        ),
        (ERRAND_HOME, "walking home (sated)"),
    ];
    let live: std::collections::BTreeMap<&str, &str> = errand_predicates().into_iter().collect();
    assert_eq!(
        live.len(),
        PAIRS.len(),
        "the registry table and this literal must have the same eight keys"
    );
    for (key, gloss) in PAIRS {
        assert_eq!(
            live.get(key),
            Some(&gloss),
            "{key} does not carry its own gloss. A SWAP is the likely mistake and the \
             likeliest one to survive review: it leaves the key count, the doc count and \
             the distinctness of both unchanged, so every other test in this file stays \
             green while the wrong words render on the right key"
        );
    }
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

/// THE EPOCH (The Warrant, Task 3). No committed `agent-at` fact carries
/// authored prose any more: its provenance names the PRODUCER, like every
/// other fact in the repo. The reader-facing words live in the concept
/// registry, on the eight `errand/*` predicates (spec §4.2), where
/// `hornvale_historiography::recount` renders them from
/// `register_predicate`'s doc string.
///
/// **The assertion is exact equality against [`ERRAND_PRODUCER`], not an
/// absence check.** "No provenance contains a parenthetical drive tag" would
/// be satisfied by any new prose that happened to avoid the eight strings,
/// which is the failure this epoch exists to close permanently rather than
/// once.
#[test]
fn no_agent_at_provenance_is_authored_prose() {
    let facts = walk_facts(WARRANT_WALK_SEED, WARRANT_WALK_WAITS);
    let steps: Vec<&ParsedFact> = facts.iter().filter(|f| f.predicate == AGENT_AT).collect();
    assert!(
        !steps.is_empty(),
        "the harness must walk, or this test is vacuous"
    );
    for step in &steps {
        assert_eq!(
            step.provenance, ERRAND_PRODUCER,
            "an agent-at provenance still carries prose: {:?}",
            step.provenance
        );
    }
}

/// The frozen before-images this file's H1 test compares against, one per
/// seed, each captured **while the per-step prose was still live**.
///
/// **THE POPULATION IS SPEC §10's, NOT ONE SEED'S (fix round 1).** §10 freezes
/// H1 over *every resident on seeds 7, 14 and 23 over 12 days*. The first cut
/// of this test ran on seed 11 alone, because Task 2's harness was seed 11 and
/// the before-image it froze was therefore a seed-11 record. That is a reason
/// to go and build the missing before-images, not to narrow the hypothesis —
/// so the other three were captured by building the campaign's merge base
/// `20c0cd375` in a throwaway worktree and reading the live `agent-at` prose
/// runs there. Seed 11 stays in the table beside them, unchanged: it is Task
/// 2's own harness, a fourth independent walk, and deleting it would trade
/// coverage for tidiness.
///
/// **The capture has a positive control, which is what makes the three new
/// files trustworthy.** The same extractor, run against the same merge-base
/// build, reproduced the committed seed-11 fixture entity-for-entity and
/// pair-for-pair (26 entities, 138 run-starts, zero differing lists). An
/// extractor that agrees with a record frozen independently, months of commits
/// earlier, is not silently re-deriving the thing it is supposed to check.
///
/// **None of these is in `docs/generated-paths.txt` and none may ever be
/// regenerated from the CURRENT code.** A re-derivation of the after-side
/// compared against itself would pass unconditionally and prove nothing — the
/// whole content of H1 is that the after-side agrees with a record taken
/// before the change. `windows/vessel/tests/fixtures/` is named in
/// `docs/generated-paths.txt`'s own prose as a directory that must never be
/// listed as an `artifacts` author, for exactly this class of reason.
///
/// Sibling files rather than one nested document, deliberately: the seed-11
/// fixture is frozen, and folding it into a per-seed map would have re-indented
/// every one of its lines. A future capture adds a file; it never edits one.
const GLOSS_FIXTURES: [(u64, &str); 4] = [
    (11, "tests/fixtures/the-warrant-glosses.json"),
    (7, "tests/fixtures/the-warrant-glosses-seed-7.json"),
    (14, "tests/fixtures/the-warrant-glosses-seed-14.json"),
    (23, "tests/fixtures/the-warrant-glosses-seed-23.json"),
];

/// H1, the losslessness claim of spec §1, asserted as EXACT EQUALITY rather
/// than approximation: the sequence of reason-glosses a reader can see, and
/// the day each run of one begins, is identical before and after the flip.
///
/// The "before" side is a frozen fixture — each entity's run-start `(day,
/// provenance)` pairs off the live `agent-at` prose. The "after" side is that
/// entity's `errand/*` facts in commit order, with each predicate resolved
/// through [`errand_predicates`] to the gloss the registry now carries. A run
/// boundary is *defined* as the point where the string changes, so a
/// per-errand fact is lossless by construction — this test is what makes that
/// argument a measurement.
///
/// **THIS TEST REPLACES `one_errand_fact_per_run_of_constant_step_provenance`
/// AND IS STRICTLY STRONGER, WHICH IS WHY THE OLD ONE IS GONE RATHER THAN
/// KEPT.** That test compared errand COUNTS against the runs of live
/// `agent-at` prose in the SAME run — a comparison the flip destroys by
/// construction, since every `agent-at` provenance is now the identical
/// producer string and every entity would read as exactly one run. It could
/// not have survived in any form. What it pinned (one errand per run of
/// constant reason) is a strict weakening of what this pins (the same runs,
/// same order, same days, same glosses, against a record taken before the
/// change) — a count is implied by an equal sequence.
///
/// claim: invariant(forall-seed over [`GLOSS_FIXTURES`] × forall-entity — for
/// each of the four pinned seeds, every entity carrying an errand trail has
/// exactly the gloss sequence and the day sequence that seed's frozen
/// before-image records, and no entity on either side is missing from the
/// other. The seed quantifier is a fixed four-element panel rather than a
/// range, because a before-image can only exist for a seed that was captured
/// while the prose was live)
#[test]
fn every_gloss_and_its_first_day_survives_the_flip() {
    let table: std::collections::BTreeMap<&str, &str> = errand_predicates().into_iter().collect();
    let mut seeds_checked = 0usize;
    let mut entities_checked = 0usize;
    let mut pairs_checked = 0usize;

    for (seed, fixture) in GLOSS_FIXTURES {
        let path = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join(fixture);
        let raw = std::fs::read_to_string(&path)
            .unwrap_or_else(|e| panic!("the frozen before-image must exist at {path:?}: {e}"));
        let doc: serde_json::Value = serde_json::from_str(&raw).expect("the fixture is JSON");
        let expected = doc["entities"]
            .as_object()
            .expect("the fixture carries an entity map");
        assert!(
            !expected.is_empty(),
            "seed {seed}: the frozen before-image is empty, so this test would prove nothing"
        );

        let facts = walk_facts(seed, WARRANT_WALK_WAITS);
        for (entity, pairs) in expected {
            let before: Vec<(i64, String)> = pairs
                .as_array()
                .expect("each entity carries an array of run starts")
                .iter()
                .map(|p| {
                    (
                        p["day"].as_i64().expect("a run start is dated"),
                        p["provenance"]
                            .as_str()
                            .expect("a run start carries its gloss")
                            .to_string(),
                    )
                })
                .collect();
            let after: Vec<(i64, String)> = facts
                .iter()
                .filter(|f| f.subject.as_str() == entity && f.predicate.starts_with("errand/"))
                .map(|f| {
                    (
                        f.day,
                        (*table.get(f.predicate.as_str()).unwrap_or_else(|| {
                            panic!("{} is not a registered errand", f.predicate)
                        }))
                        .to_string(),
                    )
                })
                .collect();
            assert_eq!(
                after, before,
                "seed {seed}, entity {entity}: the errand glosses and their days differ from \
                 the frozen before-image ({fixture}).\n\
                 \n\
                 WHICH FAILURE IS THIS? Two causes produce it and they take OPPOSITE \
                 remedies, so read the rest of this run before touching anything.\n\
                 \n\
                 (1) A PROVENANCE REGRESSION — the errand stream stopped carrying what the \
                 prose carried. Reader-visible information has been lost and the fixture is \
                 the evidence. DO NOT regenerate it; fix the producer.\n\
                 \n\
                 (2) THE WALK ITSELF MOVED — a terrain, drive, arbitration, clock or \
                 pathfinding change made this seed's creatures do something different. The \
                 before-image is then a record of a world that no longer exists, and \
                 re-capturing it IS the correct fix. THE TELL: cause (2) moves the walk's \
                 other pins in the same commit — `liveness.rs`'s `hoist_walk_shape` golden \
                 and `an_errand_commits_once_and_its_steps_commit_under_it` — while cause \
                 (1) leaves the golden's `agent-at` rows and their days untouched and moves \
                 only what a reason is called. If those moved too, you are in case (2).\n\
                 \n\
                 Re-capturing means building a tree where the per-step prose is still live \
                 (this campaign's merge base, 20c0cd375) — never re-deriving from the \
                 current code, which would compare the after-side against itself."
            );
            entities_checked += 1;
            pairs_checked += before.len();
        }

        // The other direction: no entity gained an errand trail the
        // before-image does not know about. A per-entity equality alone
        // cannot see that.
        let mut after_subjects: Vec<&str> = facts
            .iter()
            .filter(|f| f.predicate.starts_with("errand/"))
            .map(|f| f.subject.as_str())
            .collect();
        after_subjects.sort_unstable();
        after_subjects.dedup();
        for s in &after_subjects {
            assert!(
                expected.contains_key(*s),
                "seed {seed}: entity {s} commits errands but is absent from the frozen \
                 before-image"
            );
        }
        assert_eq!(
            after_subjects.len(),
            expected.len(),
            "seed {seed}: the set of entities with an errand trail must match the \
             before-image exactly"
        );
        seeds_checked += 1;
    }

    // The whole test is a loop over fixtures, so a fixture table that shrank
    // — or a seed whose walk went silent — would quietly reduce this to a
    // weaker claim while staying green. Pin the shape of what was compared.
    assert_eq!(
        seeds_checked,
        GLOSS_FIXTURES.len(),
        "every fixture is compared"
    );
    assert_eq!(
        (entities_checked, pairs_checked),
        (252, 1242),
        "the H1 comparison covers 26 + 76 + 58 + 92 entities and 138 + 103 + 81 + 920 \
         run-starts across seeds 11/7/14/23. A drop here means a fixture or a seed's walk \
         went quiet and the equality above got easier, not that anything improved."
    );
}

//! Anchor resolution and decision 0136's four conditions for the
//! `technologies/` family (Task 5).
//!
//! **Every condition here is exercised on a CONSTRUCTED fixture, not just
//! the real corpora.** Neither committed corpus contains a violation of any
//! of the four conditions — the controller's own pre-dispatch measurement
//! found `decision:`, `reason:` and `test:` at zero instances across both
//! files — so a check validated only against real data would be vacuously
//! green. The real corpora are checked too, at the bottom of this file, but
//! that check alone would prove nothing about whether the resolver works.

use hornvale::technologies::{
    Anchor, Corpus, Criterion, Finding, Verdict, audit, audit_family, cross_corpus_ruling_gaps,
    disclosure_gaps, is_chosen, load, meets, render, two_way,
};
use std::path::PathBuf;

fn workspace_root() -> PathBuf {
    std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("workspace root")
        .to_path_buf()
}

fn load_asimov() -> Corpus {
    load(&workspace_root().join("technologies/asimov-1989.technology.json"))
}

fn load_henrich() -> Corpus {
    load(&workspace_root().join("technologies/henrich-2004-extended.technology.json"))
}

/// A one-item corpus with the given verdict and anchor (`""` for none).
/// Almost every test below wants exactly this shape.
fn corpus_with(verdict: &str, anchor: &str) -> Corpus {
    let json = format!(
        r#"{{ "corpus": "fixture", "unit": "technology", "ordered": false,
              "provenance": "fixture", "frozen": "fixture",
              "items": [ {{ "id": "a", "title": "T", "introduces": "tok-a",
                            "presupposes": [], "verdict": "{verdict}",
                            "anchor": "{anchor}" }} ] }}"#
    );
    hornvale::technologies::parse(&json)
}

// --- UNJUSTIFIED -----------------------------------------------------------

/// UNJUSTIFIED: a non-`absent` verdict with no anchor.
///
/// Asserts on `why`, not just the variant: `Anchor::parse("")` returns
/// `None` (`"".split_once(':')` fails) exactly the way an unrecognized
/// prefix does, so deleting the dedicated no-anchor branch and falling
/// through to the unknown-prefix branch keeps this test green while the
/// message silently degrades from "no anchor; a `decision:` anchor is
/// required" to "unrecognized anchor prefix" — a real diagnosability
/// regression 0136 makes part of the decision, not a cosmetic one.
#[test]
fn a_refused_verdict_without_an_anchor_is_unjustified() {
    let f = audit(&corpus_with("refused", ""), &workspace_root());
    match f.as_slice() {
        [Finding::Unjustified { why, .. }] => {
            assert!(
                why.contains("no anchor"),
                "expected the no-anchor branch's message, got: {why}"
            );
            assert!(
                !why.contains("unrecognized anchor prefix"),
                "fell through to the unknown-prefix branch instead of the \
                 dedicated no-anchor one: {why}"
            );
        }
        _ => panic!("expected UNJUSTIFIED, got {f:?}"),
    }
}

/// UNJUSTIFIED also covers the wrong KIND of anchor: `refused` means a
/// decision forbids it, and a path cannot express that.
#[test]
fn a_refused_verdict_anchored_to_a_path_is_unjustified() {
    let f = audit(
        &corpus_with("refused", "path:cli/src/main.rs"),
        &workspace_root(),
    );
    assert!(
        matches!(f.as_slice(), [Finding::Unjustified { .. }]),
        "expected UNJUSTIFIED for a wrong-kind anchor, got {f:?}"
    );
}

/// An `absent` verdict must carry no anchor at all.
#[test]
fn an_absent_verdict_carrying_an_anchor_is_unjustified() {
    let f = audit(&corpus_with("absent", "decision:0070"), &workspace_root());
    assert!(
        matches!(f.as_slice(), [Finding::Unjustified { .. }]),
        "expected UNJUSTIFIED for an anchored `absent`, got {f:?}"
    );
}

/// An unrecognized anchor prefix is UNJUSTIFIED, not a silent pass.
#[test]
fn an_unrecognized_anchor_prefix_is_unjustified() {
    let f = audit(&corpus_with("refused", "wat:nonsense"), &workspace_root());
    assert!(
        matches!(f.as_slice(), [Finding::Unjustified { .. }]),
        "expected UNJUSTIFIED for an unknown prefix, got {f:?}"
    );
}

/// `reason:` requires non-empty prose — the reasonless-`inapplicable` rule,
/// generalized the same way the sibling families generalize it.
#[test]
fn an_inapplicable_verdict_with_an_empty_reason_is_unjustified() {
    let f = audit(&corpus_with("inapplicable", "reason:"), &workspace_root());
    assert!(
        matches!(f.as_slice(), [Finding::Unjustified { .. }]),
        "expected UNJUSTIFIED for an empty reason, got {f:?}"
    );
}

/// The positive control for the reason check above: non-empty prose is
/// clean.
#[test]
fn an_inapplicable_verdict_with_a_reason_is_clean() {
    let f = audit(
        &corpus_with("inapplicable", "reason:the world has no seasons here"),
        &workspace_root(),
    );
    assert!(f.is_empty(), "expected no findings, got {f:?}");
}

// --- DANGLING ----------------------------------------------------------

/// DANGLING: 0014 was superseded, so it is absent from
/// `decisions-in-force.md` by construction (the same fixture the `systems`
/// sibling's own coverage tests use — see `cli/tests/suite/system_coverage.rs`).
#[test]
fn a_refusal_citing_a_superseded_decision_is_dangling() {
    let f = audit(&corpus_with("refused", "decision:0014"), &workspace_root());
    assert!(
        matches!(f.as_slice(), [Finding::Dangling { .. }]),
        "expected DANGLING for superseded 0014, got {f:?}"
    );
}

/// The positive control: a decision that IS in force is clean.
#[test]
fn a_refusal_citing_an_in_force_decision_is_clean() {
    let f = audit(&corpus_with("refused", "decision:0070"), &workspace_root());
    assert!(
        f.is_empty(),
        "0070 is in force; expected no findings, got {f:?}"
    );
}

/// DANGLING for a registry row that does not exist.
#[test]
fn a_deferral_citing_an_unknown_registry_row_is_dangling() {
    let f = audit(
        &corpus_with("deferred", "registry:CLIENT-no-such-row-exists"),
        &workspace_root(),
    );
    assert!(
        matches!(f.as_slice(), [Finding::Dangling { .. }]),
        "expected DANGLING, got {f:?}"
    );
}

/// STALE-DEFERRED, not DANGLING: `CLIENT-action-clock` exists but reads
/// `shipped`, which falsifies "planned, not built".
#[test]
fn a_deferral_against_a_shipped_registry_row_is_stale() {
    let f = audit(
        &corpus_with("deferred", "registry:CLIENT-action-clock"),
        &workspace_root(),
    );
    assert!(
        matches!(f.as_slice(), [Finding::StaleDeferred { .. }]),
        "expected STALE-DEFERRED for a shipped row, got {f:?}"
    );
}

/// DANGLING for a `path:` anchor naming a file that does not exist.
#[test]
fn a_present_verdict_with_a_missing_path_is_dangling() {
    let f = audit(
        &corpus_with("present", "path:does/not/exist.rs"),
        &workspace_root(),
    );
    assert!(
        matches!(f.as_slice(), [Finding::Dangling { .. }]),
        "expected DANGLING for a missing path, got {f:?}"
    );
}

/// DANGLING for a malformed `path:` anchor (escapes the repo root) — never
/// a silent existence check against the wrong file.
#[test]
fn a_present_verdict_with_an_escaping_path_is_dangling() {
    let f = audit(
        &corpus_with("present", "path:../outside-the-repo"),
        &workspace_root(),
    );
    assert!(
        matches!(f.as_slice(), [Finding::Dangling { .. }]),
        "expected DANGLING for an escaping path, got {f:?}"
    );
}

/// The positive control: a `path:` anchor naming a real file is clean.
#[test]
fn a_present_verdict_with_a_real_path_is_clean() {
    let f = audit(
        &corpus_with("present", "path:cli/src/main.rs"),
        &workspace_root(),
    );
    assert!(f.is_empty(), "expected no findings, got {f:?}");
}

/// `test:` is the other mechanism anchor — reused via
/// `crate::systems::RepoFacts::test_resolution` rather than a second
/// boundary scan. `load` is a real, non-`#[ignore]`d `pub fn` defined
/// inside `cli/`, so `test:hornvale::load` resolves.
#[test]
fn a_present_verdict_with_a_resolving_test_anchor_is_clean() {
    let f = audit(
        &corpus_with("present", "test:hornvale::load"),
        &workspace_root(),
    );
    assert!(f.is_empty(), "expected no findings, got {f:?}");
}

/// DANGLING for a `test:` anchor naming a symbol nothing defines.
#[test]
fn a_present_verdict_with_an_unresolving_test_anchor_is_dangling() {
    let f = audit(
        &corpus_with(
            "present",
            "test:hornvale::this_symbol_does_not_exist_anywhere",
        ),
        &workspace_root(),
    );
    assert!(
        matches!(f.as_slice(), [Finding::Dangling { .. }]),
        "expected DANGLING for an unknown symbol, got {f:?}"
    );
}

/// **V2 regression.** `crate::systems::TestResolution::Ignored` is a real
/// variant `RepoFacts::test_resolution` can return, and the sibling's own
/// `boundary_tests` module covers `test_resolution` returning it — but
/// nothing exercised THIS module's `Ignored` arm before this test. Deleting
/// that arm (`=> None` instead of a `Dangling`) is the unanchored-K hazard
/// ledger #18 exists to prevent: an `#[ignore]`d test, which the gate never
/// runs, would silently back a `present`/`unmeasured` verdict.
/// `a_possessed_turn_stays_within_its_ceilings`
/// (`cli/tests/suite/session_cost.rs`) is a real test defined inside the
/// `cli` crate that is currently `#[ignore]`d under the heavy-tier's
/// canonical reason (not spelled out here as a literal attribute line —
/// `heavy_tier.rs`'s own `ignore_reasons` scanner is a naive text scan over
/// every `.rs` file with no awareness of doc comments, and a first draft of
/// this sentence that DID spell it out literally was picked up by that
/// scanner as a second, non-canonical ignore reason and failed
/// `heavy_tier_reason_strings_are_canonical`), so
/// `test:hornvale::a_possessed_turn_stays_within_its_ceilings` resolves to
/// `Ignored`, not `Missing`.
#[test]
fn a_present_verdict_with_an_ignored_test_anchor_is_dangling() {
    let f = audit(
        &corpus_with(
            "present",
            "test:hornvale::a_possessed_turn_stays_within_its_ceilings",
        ),
        &workspace_root(),
    );
    assert!(
        matches!(f.as_slice(), [Finding::Dangling { .. }]),
        "expected DANGLING for an #[ignore]d test anchor, got {f:?}"
    );
}

// --- `unmeasured` carries a mechanism anchor (ledger #18, Step 4b.1) ----

/// `unmeasured` is NOT `regularities::Verdict::Unmeasured` — it requires a
/// mechanism anchor exactly like `present` does, because its reach half
/// already passed. No anchor at all is UNJUSTIFIED, the same as a
/// `present` with no anchor.
///
/// Asserts on `why` for the same reason
/// `a_refused_verdict_without_an_anchor_is_unjustified` does: an empty
/// anchor and an unrecognized prefix both parse to `None`, so the two
/// branches must be told apart by their message, not just their variant.
#[test]
fn an_unmeasured_verdict_without_an_anchor_is_unjustified() {
    let f = audit(&corpus_with("unmeasured", ""), &workspace_root());
    match f.as_slice() {
        [Finding::Unjustified { why, .. }] => {
            assert!(
                why.contains("no anchor"),
                "expected the no-anchor branch's message, got: {why}"
            );
            assert!(
                !why.contains("unrecognized anchor prefix"),
                "fell through to the unknown-prefix branch instead of the \
                 dedicated no-anchor one: {why}"
            );
        }
        _ => panic!("expected UNJUSTIFIED, got {f:?}"),
    }
}

/// The positive control: `unmeasured` with a resolving `path:` anchor is
/// clean — the real shape `henrich-2004-extended` actually uses
/// (`col-copper-smelting`/`col-iron-smelting`, both anchored at
/// `path:domains/history/src/record.rs`).
#[test]
fn an_unmeasured_verdict_with_a_resolving_path_is_clean() {
    let f = audit(
        &corpus_with("unmeasured", "path:domains/history/src/record.rs"),
        &workspace_root(),
    );
    assert!(f.is_empty(), "expected no findings, got {f:?}");
}

/// Two `unmeasured` items may share ONE `path:` anchor — precedented by the
/// real henrich corpus, and the resolver must not require anchors to be
/// distinct across items.
#[test]
fn two_unmeasured_items_may_cite_the_same_path_anchor() {
    let json = r#"{ "corpus": "fixture", "unit": "technology", "ordered": false,
                     "provenance": "fixture", "frozen": "fixture",
                     "items": [
                       { "id": "a", "title": "A", "introduces": "tok-a",
                         "presupposes": [], "verdict": "unmeasured",
                         "anchor": "path:domains/history/src/record.rs" },
                       { "id": "b", "title": "B", "introduces": "tok-b",
                         "presupposes": [], "verdict": "unmeasured",
                         "anchor": "path:domains/history/src/record.rs" }
                     ] }"#;
    let c = hornvale::technologies::parse(json);
    let f = audit(&c, &workspace_root());
    assert!(
        f.is_empty(),
        "two items sharing one path anchor must both resolve, got {f:?}"
    );
}

// --- `doc:` admissibility (mirrors regularity_corpus.rs:192-218) -------

/// `doc:` into generated prose resolves — the Domesday is declared
/// `artifacts` in `docs/generated-paths.txt`.
#[test]
fn a_grown_verdict_anchored_into_generated_prose_is_clean() {
    let f = audit(
        &corpus_with("grown", "doc:book/src/domesday/demography.md"),
        &workspace_root(),
    );
    assert!(f.is_empty(), "expected no findings, got {f:?}");
}

/// **V1 regression (ledger #28).** `GeneratedPaths::has_generator` resolves
/// by LONGEST DECLARED DIRECTORY PREFIX, so a nonexistent file under a real
/// generated directory inherits that directory's `artifacts` author and
/// reads as generated without ever having been written —
/// `book/src/domesday/` is declared `artifacts`
/// (`docs/generated-paths.txt:238`) and this exact file has never existed
/// (confirmed: `ls book/src/domesday/utterly-made-up.md` fails). Before the
/// V1 fix this resolved CLEAN, reproducing the vacuity
/// `regularities::doc_states_the_claim` had already closed for its own
/// family and documented at `cli/src/regularities.rs:956` — the "repointing
/// an item's anchor at `book/src/domesday/climate.md` … and watching the
/// whole suite stay green" experiment. Must be DANGLING, not clean and not
/// UNJUSTIFIED (the anchor's KIND and its declaration are both fine; only
/// its target is not).
#[test]
fn a_grown_verdict_anchored_to_a_nonexistent_page_under_a_declared_directory_is_dangling() {
    let f = audit(
        &corpus_with("grown", "doc:book/src/domesday/utterly-made-up.md"),
        &workspace_root(),
    );
    assert!(
        matches!(f.as_slice(), [Finding::Dangling { .. }]),
        "expected DANGLING for a nonexistent page under a declared directory, got {f:?}"
    );
}

/// The load-bearing direction: `doc:` into HAND-WRITTEN prose is refused,
/// never a silent pass. `book/src/laboratory/overview.md` is declared
/// `none(hand-written prose, never regenerated)`.
#[test]
fn a_flat_verdict_anchored_into_hand_written_prose_is_unjustified() {
    let f = audit(
        &corpus_with("flat", "doc:book/src/laboratory/overview.md"),
        &workspace_root(),
    );
    assert!(
        matches!(f.as_slice(), [Finding::Unjustified { .. }]),
        "expected UNJUSTIFIED for hand-written prose, got {f:?}"
    );
}

/// A `doc:` anchor into a path `docs/generated-paths.txt` says nothing
/// about at all is also refused.
#[test]
fn a_lost_verdict_anchored_into_an_undeclared_path_is_unjustified() {
    let f = audit(
        &corpus_with("lost", "doc:book/src/nothing-here.md"),
        &workspace_root(),
    );
    assert!(
        matches!(f.as_slice(), [Finding::Unjustified { .. }]),
        "expected UNJUSTIFIED for an undeclared path, got {f:?}"
    );
}

/// A `doc:` anchor is the only kind `grown`/`flat`/`lost` accept — a
/// mechanism anchor is the wrong kind of evidence for a measured
/// trajectory claim.
#[test]
fn a_grown_verdict_anchored_to_a_path_is_unjustified() {
    let f = audit(
        &corpus_with("grown", "path:cli/src/main.rs"),
        &workspace_root(),
    );
    assert!(
        matches!(f.as_slice(), [Finding::Unjustified { .. }]),
        "expected UNJUSTIFIED for a wrong-kind anchor, got {f:?}"
    );
}

// --- Anchor parsing ------------------------------------------------------

#[test]
fn anchor_parses_all_six_kinds_and_rejects_the_unknown() {
    assert_eq!(
        Anchor::parse("test:hornvale::load"),
        Some(Anchor::Test("hornvale::load".to_string()))
    );
    assert_eq!(
        Anchor::parse("path:cli/src/main.rs"),
        Some(Anchor::Path("cli/src/main.rs".to_string()))
    );
    assert_eq!(
        Anchor::parse("decision:0070"),
        Some(Anchor::Decision("0070".to_string()))
    );
    assert_eq!(
        Anchor::parse("registry:TOOL-x"),
        Some(Anchor::Registry("TOOL-x".to_string()))
    );
    assert_eq!(
        Anchor::parse("reason:because"),
        Some(Anchor::Reason("because".to_string()))
    );
    assert_eq!(
        Anchor::parse("doc:book/src/domesday/demography.md"),
        Some(Anchor::Doc("book/src/domesday/demography.md".to_string()))
    );
    assert_eq!(Anchor::parse("nonsense"), None);
}

// --- NOVELTY: the `absent` ratchet, both directions ---------------------

/// A corpus id with no baseline recorded raises nothing, regardless of its
/// `absent` count — this is a ratchet against a known reference, not a
/// judgment about whether a corpus is allowed to exist.
#[test]
fn novelty_baseline_is_none_for_an_unbaselined_corpus() {
    assert_eq!(
        hornvale::technologies::novelty_baseline("nonexistent"),
        None
    );
}

/// The two baselines, derived from the committed files
/// (`technologies/asimov-1989.technology.json`'s and
/// `technologies/henrich-2004-extended.technology.json`'s `absent` tallies)
/// rather than guessed — pinned here so a change to either value is a
/// deliberate edit to this test, not a silent drift.
#[test]
fn the_two_baselines_match_the_committed_corpora() {
    assert_eq!(
        hornvale::technologies::novelty_baseline("asimov-1989"),
        Some(35)
    );
    assert_eq!(
        hornvale::technologies::novelty_baseline("henrich-2004-extended"),
        Some(31)
    );
    let asimov_absent = load_asimov()
        .items
        .iter()
        .filter(|i| i.verdict == hornvale::technologies::Verdict::Absent)
        .count();
    let henrich_absent = load_henrich()
        .items
        .iter()
        .filter(|i| i.verdict == hornvale::technologies::Verdict::Absent)
        .count();
    assert_eq!(asimov_absent, 35, "asimov-1989's live absent count moved");
    assert_eq!(
        henrich_absent, 31,
        "henrich-2004-extended's live absent count moved"
    );
}

/// A corpus named `n` items, all `absent`, sharing no tokens (so the
/// lattice is trivially valid — every item is its own root).
fn corpus_with_n_absent_items(corpus_id: &str, n: usize) -> Corpus {
    let items: Vec<String> = (0..n)
        .map(|i| {
            format!(
                r#"{{ "id": "item-{i}", "title": "T{i}", "introduces": "tok-{i}",
                      "presupposes": [], "verdict": "absent", "anchor": "" }}"#
            )
        })
        .collect();
    let json = format!(
        r#"{{ "corpus": "{corpus_id}", "unit": "technology", "ordered": false,
              "provenance": "fixture", "frozen": "fixture",
              "items": [{}] }}"#,
        items.join(",")
    );
    hornvale::technologies::parse(&json)
}

/// NOVELTY fires on a RISE: one more `absent` item than the baseline.
#[test]
fn novelty_fires_when_absent_rises_above_the_asimov_baseline() {
    let c = corpus_with_n_absent_items("asimov-1989", 36);
    let f = audit(&c, &workspace_root());
    assert!(
        matches!(
            f.as_slice(),
            [Finding::Novelty {
                baseline: 35,
                found: 36,
                ..
            }]
        ),
        "expected NOVELTY(35 -> 36), got {f:?}"
    );
}

/// NOVELTY must PASS on a FALL — every correction this campaign has made
/// moved `absent` down, and that is the direction the ratchet must permit,
/// not the one it punishes.
#[test]
fn novelty_passes_when_absent_falls_below_the_asimov_baseline() {
    let c = corpus_with_n_absent_items("asimov-1989", 34);
    let f = audit(&c, &workspace_root());
    assert!(
        f.is_empty(),
        "a falling absent count must not raise NOVELTY, got {f:?}"
    );
}

/// Exactly at the baseline is clean — the ratchet is `>`, not `>=`.
#[test]
fn novelty_passes_when_absent_equals_the_asimov_baseline() {
    let c = corpus_with_n_absent_items("asimov-1989", 35);
    let f = audit(&c, &workspace_root());
    assert!(
        f.is_empty(),
        "expected no findings at the baseline, got {f:?}"
    );
}

/// Both directions again against the SECOND baseline, so the ratchet is not
/// merely correct for one corpus by coincidence.
#[test]
fn novelty_fires_when_absent_rises_above_the_henrich_baseline() {
    let c = corpus_with_n_absent_items("henrich-2004-extended", 32);
    let f = audit(&c, &workspace_root());
    assert!(
        matches!(
            f.as_slice(),
            [Finding::Novelty {
                baseline: 31,
                found: 32,
                ..
            }]
        ),
        "expected NOVELTY(31 -> 32), got {f:?}"
    );
}

#[test]
fn novelty_passes_when_absent_falls_below_the_henrich_baseline() {
    let c = corpus_with_n_absent_items("henrich-2004-extended", 30);
    let f = audit(&c, &workspace_root());
    assert!(
        f.is_empty(),
        "a falling absent count must not raise NOVELTY, got {f:?}"
    );
}

// --- Cross-corpus ruling completeness (Step 4b.2) -----------------------

/// A scratch `technologies/` directory holding exactly the two corpus
/// files given (as `(corpus-id, items-json-array-body)` pairs), so
/// [`cross_corpus_ruling_gaps`] can be exercised against a constructed
/// family rather than the real one. Returns the scratch root; the caller
/// removes it.
///
/// `label` must be unique per CALL SITE, not just per corpus shape. Root
/// `CLAUDE.md` documents nextest as **process-per-test**, which would make
/// this race impossible under that harness — but this file's own tests run
/// under `cargo test -p hornvale --test suite`, i.e. **libtest's own
/// threaded harness, many test functions inside one process**, and that is
/// the harness in which the race actually occurred: caught by this file's
/// first draft, where two tests both naming their scratch dir from `(pid,
/// corpora.len())` alone shared the identical path — two tests with
/// `corpora.len() == 2` read each other's fixtures and failed
/// nondeterministically. The label stays required regardless of which
/// harness eventually runs this suite (nextest included, if this crate ever
/// moves to it): a future reader who checks `CLAUDE.md` and concludes
/// "nextest is process-per-test, so this label is redundant" would be
/// reasoning from the wrong harness for THIS file today.
fn scratch_family(label: &str, corpora: &[(&str, &str)]) -> PathBuf {
    let root = std::env::temp_dir().join(format!(
        "hv-technology-family-{}-{label}",
        std::process::id(),
    ));
    let dir = root.join("technologies");
    std::fs::create_dir_all(&dir).expect("make scratch technologies dir");
    for (id, items_json) in corpora {
        let json = format!(
            r#"{{ "corpus": "{id}", "unit": "technology", "ordered": false,
                  "provenance": "fixture", "frozen": "fixture",
                  "items": [{items_json}] }}"#
        );
        std::fs::write(dir.join(format!("{id}.technology.json")), json)
            .expect("writes a scratch corpus file");
    }
    root
}

/// The violation this rule exists to catch: sibling `b` cites
/// `registry:ROW-x` and corpus `a` never mentions `ROW-x` anywhere — the
/// exact shape `MAP-18`/`BIO-animal-domestication` were before the real
/// corpora's own fix round.
#[test]
fn a_row_a_sibling_cites_and_this_corpus_never_mentions_is_a_gap() {
    let a_items = r#"{ "id": "a1", "title": "A1", "introduces": "tok-a1",
                        "presupposes": [], "verdict": "absent", "anchor": "" }"#;
    let b_items = r#"{ "id": "b1", "title": "B1", "introduces": "tok-b1",
                        "presupposes": [], "verdict": "deferred",
                        "anchor": "registry:ROW-x" }"#;
    let root = scratch_family("never-mentioned", &[("a", a_items), ("b", b_items)]);
    let a = load(&root.join("technologies/a.technology.json"));
    let gaps = cross_corpus_ruling_gaps(&a, &root);
    let _ = std::fs::remove_dir_all(&root);
    assert!(
        matches!(gaps.as_slice(), [Finding::Unjustified { id, .. }] if id == "ROW-x"),
        "expected one gap naming ROW-x, got {gaps:?}"
    );
}

/// The positive control: a refusal written into `provenance` counts as a
/// ruling, exactly the way the real corpora's `MAP-18`/`BIO-animal-
/// domestication` refusals are written today.
#[test]
fn a_row_refused_in_provenance_is_not_a_gap() {
    let a_items = r#"{ "id": "a1", "title": "A1", "introduces": "tok-a1",
                        "presupposes": [], "verdict": "absent", "anchor": "" }"#;
    let b_items = r#"{ "id": "b1", "title": "B1", "introduces": "tok-b1",
                        "presupposes": [], "verdict": "deferred",
                        "anchor": "registry:ROW-x" }"#;
    let root = scratch_family("refused-in-provenance", &[("a", a_items), ("b", b_items)]);
    // Overwrite `a`'s provenance with a refusal naming ROW-x, the way
    // `technologies/CLAUDE.md`'s worked example does.
    let a_json = format!(
        r#"{{ "corpus": "a", "unit": "technology", "ordered": false,
              "provenance": "RULING: ROW-x discharges no demand here.",
              "frozen": "fixture", "items": [{a_items}] }}"#
    );
    std::fs::write(root.join("technologies/a.technology.json"), a_json)
        .expect("overwrite with a refusal");
    let a = load(&root.join("technologies/a.technology.json"));
    let gaps = cross_corpus_ruling_gaps(&a, &root);
    let _ = std::fs::remove_dir_all(&root);
    assert!(
        gaps.is_empty(),
        "a written refusal must satisfy the rule, got {gaps:?}"
    );
}

/// The positive control's other shape: `a` also CITING the row (via its own
/// `registry:` anchor) is a ruling too.
#[test]
fn a_row_also_cited_by_this_corpus_is_not_a_gap() {
    let a_items = r#"{ "id": "a1", "title": "A1", "introduces": "tok-a1",
                        "presupposes": [], "verdict": "deferred",
                        "anchor": "registry:ROW-x" }"#;
    let b_items = r#"{ "id": "b1", "title": "B1", "introduces": "tok-b1",
                        "presupposes": [], "verdict": "deferred",
                        "anchor": "registry:ROW-x" }"#;
    let root = scratch_family("also-cited", &[("a", a_items), ("b", b_items)]);
    let a = load(&root.join("technologies/a.technology.json"));
    let gaps = cross_corpus_ruling_gaps(&a, &root);
    let _ = std::fs::remove_dir_all(&root);
    assert!(
        gaps.is_empty(),
        "a shared citation must satisfy the rule, got {gaps:?}"
    );
}

/// The grep-level floor is TOKEN-bounded, not a bare substring: a corpus
/// mentioning `ROW-x9` must not be read as ruling on `ROW-x`.
#[test]
fn a_mention_of_a_longer_row_id_does_not_satisfy_a_shorter_one() {
    let a_items = r#"{ "id": "a1", "title": "A1", "introduces": "tok-a1",
                        "presupposes": [], "verdict": "absent",
                        "anchor": "" }"#;
    let b_items = r#"{ "id": "b1", "title": "B1", "introduces": "tok-b1",
                        "presupposes": [], "verdict": "deferred",
                        "anchor": "registry:ROW-x" }"#;
    let root = scratch_family("longer-row-id", &[("a", a_items), ("b", b_items)]);
    let a_json = format!(
        r#"{{ "corpus": "a", "unit": "technology", "ordered": false,
              "provenance": "this corpus discusses ROW-x9 and ROW-x12, nothing else.",
              "frozen": "fixture", "items": [{a_items}] }}"#
    );
    std::fs::write(root.join("technologies/a.technology.json"), a_json)
        .expect("overwrite with a near-miss mention");
    let a = load(&root.join("technologies/a.technology.json"));
    let gaps = cross_corpus_ruling_gaps(&a, &root);
    let _ = std::fs::remove_dir_all(&root);
    assert!(
        matches!(gaps.as_slice(), [Finding::Unjustified { id, .. }] if id == "ROW-x"),
        "a longer row id's mention must not satisfy the shorter row's rule, got {gaps:?}"
    );
}

// --- `audit_family`: the one entry point for both halves (V8) ----------

/// `audit_family` must include cross-corpus gaps, not just per-item
/// findings — a future edit that returned only `audit`'s output would
/// silently stop enforcing family law, and nothing else here would catch
/// it. A trivial single-item fixture mentions none of the REAL corpora's
/// registry rows, so against the real workspace root, `audit` alone must
/// stay clean while `audit_family` must not.
#[test]
fn audit_family_includes_cross_corpus_gaps() {
    let c = corpus_with("absent", "");
    let per_item = audit(&c, &workspace_root());
    assert!(
        per_item.is_empty(),
        "sanity: a trivial absent item has no anchor findings, got {per_item:?}"
    );
    let family = audit_family(&c, &workspace_root());
    assert!(
        !family.is_empty(),
        "a fixture mentioning none of the real corpora's registry rows must \
         still surface cross-corpus gaps through audit_family"
    );
}

// --- The criterion, and the two-way trajectory guard (Task 6) ------------

/// Both edges of `MedianInBand` are inclusive — the same arithmetic the
/// sibling `regularities::Criterion::MedianInBand` guards
/// (`cli/tests/suite/regularity_corpus.rs`'s
/// `median_in_band_is_inclusive_at_both_edges`), reproduced for this
/// family.
#[test]
fn median_in_band_is_inclusive_at_both_edges() {
    let v = [0.2, 0.5, 0.8];
    assert!(meets(&Criterion::MedianInBand { lo: 0.5, hi: 0.9 }, &v, 3));
    assert!(meets(&Criterion::MedianInBand { lo: 0.1, hi: 0.5 }, &v, 3));
    assert!(!meets(&Criterion::MedianInBand { lo: 0.6, hi: 0.7 }, &v, 3));
}

/// `FractionInBand` divides by the POPULATION, never by the number of
/// values on hand — this family's own version of the sibling's
/// `fraction_in_band_counts_only_values_inside_it` /
/// `present_on_fraction_measures_against_the_world_count_not_the_value_
/// count` denominator bug. Two peoples report a holding fraction of 0.5
/// each; against a population of 4 (two more peoples reported nothing), the
/// aggregate is 1.0/4 = 0.25, which clears `[0.2, 0.3]`. A denominator bug
/// dividing by `values.len()` == 2 instead would compute 1.0/2 = 0.5, which
/// does not — this is the discriminating case, same as the sibling's.
#[test]
fn fraction_in_band_measures_against_the_population_not_the_value_count() {
    let v = [0.5, 0.5];
    let c = Criterion::FractionInBand { lo: 0.2, hi: 0.3 };
    assert!(
        meets(&c, &v, 4),
        "1.0 summed over a population of 4 is 0.25, inside [0.2, 0.3]"
    );
    assert!(
        !meets(&c, &v, 2),
        "1.0 summed over a population of 2 is 0.5, outside [0.2, 0.3] -- the \
         value-count denominator a bug would use"
    );
}

/// The pathology this family exists to detect (spec §5.2a): a bare boolean
/// cannot see divergence. Today every surviving community is Classical, so
/// a holding fraction of 1.0 must NOT read as success — it is exactly as
/// wrong as 0.0.
#[test]
fn a_universal_holding_fraction_fails_a_divergence_band() {
    let c = Criterion::FractionInBand { lo: 0.15, hi: 0.85 };
    assert!(
        !meets(&c, &[1.0; 10], 10),
        "every people holding it is not divergence"
    );
    assert!(
        !meets(&c, &[0.0; 10], 10),
        "no people holding it is not divergence"
    );
    assert!(meets(&c, &[0.5; 10], 10));
}

/// An empty population never meets a criterion — an honest `false`, never a
/// vacuous `true` (the sibling's `an_empty_population_never_meets_a_
/// criterion`, reproduced for both of this family's criterion kinds).
#[test]
fn an_empty_population_never_meets_a_criterion() {
    assert!(!meets(
        &Criterion::MedianInBand { lo: -1.0, hi: 1.0 },
        &[],
        0
    ));
    assert!(!meets(
        &Criterion::FractionInBand { lo: 0.0, hi: 1.0 },
        &[],
        0
    ));
}

/// 0936's guard is two-way. An implementation reddening only one direction
/// has built half a guard, and the half it skipped is the one that lets a
/// corpus quietly under-report the world.
#[test]
fn the_guard_reddens_in_both_directions() {
    assert!(
        two_way("a", Verdict::Grown, Verdict::Flat).is_some(),
        "a regularity was lost"
    );
    assert!(
        two_way("a", Verdict::Flat, Verdict::Grown).is_some(),
        "stale pessimism"
    );
    assert!(two_way("a", Verdict::Grown, Verdict::Grown).is_none());
}

/// The finding names the item that regressed. This is why campaign ledger
/// #30 authorised widening `two_way`'s signature to take an id: `Regressed`
/// is the verdict that fires when a capability was LOST — the one event
/// this whole campaign exists to make visible — so a finding that could
/// not say which item regressed would fail 0136's diagnosability standard
/// on its own first use.
#[test]
fn the_guard_names_the_item_that_regressed() {
    match two_way("inv-writing", Verdict::Grown, Verdict::Flat) {
        Some(Finding::Regressed {
            id,
            authored,
            computed,
            ..
        }) => {
            assert_eq!(id, "inv-writing");
            assert_eq!(authored, Verdict::Grown);
            assert_eq!(computed, Verdict::Flat);
        }
        other => panic!("expected Regressed naming the item, got {other:?}"),
    }
}

/// `unmeasured` is a lifecycle state, never a coverage verdict, and raises
/// nothing here — which is what makes Task 7's separate tally necessary.
#[test]
fn unmeasured_raises_nothing() {
    assert!(two_way("a", Verdict::Unmeasured, Verdict::Unmeasured).is_none());
}

/// The guard is scoped to the two verdicts a single-snapshot criterion can
/// actually compute. `Lost` names a capability held and then released — a
/// trajectory [`meets`]'s boolean cannot detect — so `Lost` on either side
/// must not trip the guard **yet**; that is the successor campaign's job
/// (which is what makes a computable `Lost` possible in the first place),
/// not a hole in this one.
#[test]
fn lost_does_not_trip_the_guard_yet() {
    assert!(two_way("a", Verdict::Grown, Verdict::Lost).is_none());
    assert!(two_way("a", Verdict::Lost, Verdict::Flat).is_none());
    assert!(two_way("a", Verdict::Lost, Verdict::Lost).is_none());
}

// --- The real corpora, both halves ---------------------------------------

/// The committed corpora must be clean under BOTH checks — the per-item
/// anchor audit (this file's `audit`) and the cross-corpus ruling-
/// completeness rule (`cross_corpus_ruling_gaps`). Task 4 established the
/// same "both halves" precedent for the lattice guards.
#[test]
fn the_real_technology_corpora_have_no_findings() {
    let root = workspace_root();
    for corpus in [load_asimov(), load_henrich()] {
        let anchor_findings = audit(&corpus, &root);
        assert!(
            anchor_findings.is_empty(),
            "{} has anchor findings:\n{anchor_findings:#?}",
            corpus.corpus
        );
        let ruling_gaps = cross_corpus_ruling_gaps(&corpus, &root);
        assert!(
            ruling_gaps.is_empty(),
            "{} has cross-corpus ruling gaps:\n{ruling_gaps:#?}",
            corpus.corpus
        );
    }
}

// --- Task 7: the report's 0095/0136 properties (ledger #33) --------------
//
// Asserted over the RENDERED STRING `render` returns, never over the
// committed `docs/audits/technology-coverage-*.md` file. The sanctioned
// response to a drift-check failure is to regenerate and commit, so a test
// reading the committed artifact would be satisfied by the very
// regeneration that could launder a violation into the golden (ledger #33).
// `the_real_technology_corpora_have_no_findings` above already exercises
// the committed files' CONTENT against live repo facts; these five tests
// exercise the RENDERER's own shape, independent of what happens to be
// committed today.

/// Byte offset of the first ASCII digit at or after `from` in `s`. Panics if
/// none exists — every test below that calls this expects to find one.
fn first_digit_at_or_after(s: &str, from: usize) -> usize {
    s[from..]
        .find(|c: char| c.is_ascii_digit())
        .map(|rel| from + rel)
        .expect("a digit after the given offset")
}

/// Collapse every whitespace run to a single space, so a CONTAINS check on
/// multi-word prose does not depend on exactly where `wrap` (a private,
/// 76-column, word-boundary wrapper) happened to turn a space into a
/// newline. Section headings, bullet lines and table rows are never passed
/// through `wrap`, so the byte-OFFSET comparisons below use the raw string
/// directly; this helper is only for asserting that a phrase appears
/// somewhere, never for asserting where.
fn flatten(s: &str) -> String {
    s.split_whitespace().collect::<Vec<_>>().join(" ")
}

/// Requirement 1 (decision 0095, stated as mechanical rather than
/// aspirational): provenance, the declared bias and the selection rule
/// print BEFORE any number. Asserts an ORDERING — the offsets of the
/// `## Provenance` and `## Reading this report` headings, and of the first
/// digit the `## Tally` section actually prints — not mere presence; a
/// footnote below the tally would also satisfy `report.contains(...)`, and
/// this must not.
#[test]
fn provenance_and_bias_precede_the_tallys_first_number() {
    for (corpus, id) in [
        (load_asimov(), "asimov-1989"),
        (load_henrich(), "henrich-2004-extended"),
    ] {
        let report = render(&corpus, id);
        let provenance = report.find("## Provenance").expect("a provenance section");
        let reading = report
            .find("## Reading this report")
            .expect("a reading-this-report section");
        let tally = report.find("## Tally").expect("a tally section");
        let first_digit = first_digit_at_or_after(&report, tally);
        assert!(
            provenance < reading && reading < tally && tally < first_digit,
            "{id}: expected Provenance < Reading-this-report < Tally < its first \
             printed digit; got provenance={provenance} reading={reading} \
             tally={tally} first_digit={first_digit}"
        );
        // The selection rule is the corpus's OWN declared bias (it lives in
        // `corpus.provenance`, printed verbatim under `## Provenance`), not
        // boilerplate this renderer would print regardless of which corpus
        // loaded. Both real corpora state it verbatim, in exactly these
        // words.
        assert!(
            flatten(&report[..tally]).contains("THE SELECTION RULE"),
            "{id}: the corpus's own selection rule must print before the tally: \
             {report}"
        );
    }
}

/// Requirement 2 (0136's consequence clause): the `present`/`unmeasured`
/// weak-anchor caveat prints ABOVE the tally, never in a footnote. Asserts
/// both that the caveat's own text sits strictly between `## Reading this
/// report` and `## Tally`, and that it does NOT additionally (or only)
/// appear below the tally — a caveat repeated after the score, or moved
/// there, reads exactly like the footnote this property forbids.
#[test]
fn the_weak_anchor_caveat_precedes_the_tally_and_is_not_repeated_below_it() {
    for (corpus, id) in [
        (load_asimov(), "asimov-1989"),
        (load_henrich(), "henrich-2004-extended"),
    ] {
        let report = render(&corpus, id);
        let reading = report
            .find("## Reading this report")
            .expect("a reading-this-report section");
        let tally = report.find("## Tally").expect("a tally section");
        assert!(reading < tally);
        assert!(
            flatten(&report[reading..tally]).contains("only WEAKLY checked"),
            "{id}: the weak-anchor caveat must sit between Reading-this-report \
             and Tally: {report}"
        );
        assert!(
            !flatten(&report[tally..]).contains("only WEAKLY checked"),
            "{id}: the caveat must not also appear after the tally, which is \
             what a footnote would do: {report}"
        );
    }
}

/// Requirement 3: `unmeasured` is reported SEPARATELY from coverage, never
/// folded into a percentage, with its reason beside it. Asserts that the
/// word "unmeasured" never appears inside the `## Tally` section's own body
/// (where the eight coverage-verdict percentages live) — folding it into
/// that same bulleted percentage list is exactly the shape this property
/// forbids — and that the dedicated `## Unmeasured` section states the
/// structural reason whenever the count is nonzero.
#[test]
fn unmeasured_is_reported_separately_from_coverage_with_its_reason_beside_it() {
    for (corpus, id) in [
        (load_asimov(), "asimov-1989"),
        (load_henrich(), "henrich-2004-extended"),
    ] {
        let report = render(&corpus, id);
        let tally = report.find("## Tally").expect("a tally section");
        let unmeasured_heading = report.find("## Unmeasured").expect("an unmeasured section");
        let demand_set = report.find("## Demand set").expect("a demand-set section");
        assert!(tally < unmeasured_heading && unmeasured_heading < demand_set);

        // The Tally section's own prose is allowed to NAME "unmeasured" (it
        // must, in fact, say why the denominator excludes it — leaving that
        // unexplained would itself look like a silent omission). What it
        // must never do is carry an `unmeasured` BULLET in the same list as
        // the eight coverage percentages — that specific shape is the one
        // this property forbids, and checking for the bare word instead
        // would fail on the renderer's own honest explanation of why there
        // is no such bullet.
        let tally_body = &report[tally..unmeasured_heading];
        assert!(
            !tally_body
                .lines()
                .any(|l| l.trim_start().to_lowercase().starts_with("- unmeasured:")),
            "{id}: the Tally section must not carry an `- unmeasured: N (P%)` \
             bullet alongside the eight coverage verdicts — that is exactly \
             what 'never folded into a percentage' forbids: {tally_body}"
        );

        let unmeasured_count = corpus
            .items
            .iter()
            .filter(|i| i.verdict == Verdict::Unmeasured)
            .count();
        let unmeasured_body = &report[unmeasured_heading..demand_set];
        if unmeasured_count == 0 {
            assert!(
                unmeasured_body.contains("None —"),
                "{id}: a zero count must say so plainly: {unmeasured_body}"
            );
        } else {
            let flat_body = flatten(unmeasured_body);
            assert!(
                flat_body.contains("tech_for") && flat_body.contains("monotone"),
                "{id}: a nonzero unmeasured count ({unmeasured_count}) must carry \
                 its structural reason beside it, not just the number: \
                 {unmeasured_body}"
            );
        }
    }
}

/// Requirement 4: BOTH counts that make the finding sayable appear, DERIVED
/// from the corpus rather than hard-coded here — N (every item), K (`reach`:
/// `present` + `unmeasured`, the items Hornvale's mechanism reaches at all)
/// and how many of those K currently read `lost`. A single tally cannot say
/// this (Task 7's brief); this asserts all three numbers land in the same
/// sentence, not scattered where a reader could not connect them.
#[test]
fn the_reach_and_loss_finding_states_both_counts_together() {
    for (corpus, id) in [
        (load_asimov(), "asimov-1989"),
        (load_henrich(), "henrich-2004-extended"),
    ] {
        let report = render(&corpus, id);
        let present = corpus
            .items
            .iter()
            .filter(|i| i.verdict == Verdict::Present)
            .count();
        let unmeasured = corpus
            .items
            .iter()
            .filter(|i| i.verdict == Verdict::Unmeasured)
            .count();
        let lost = corpus
            .items
            .iter()
            .filter(|i| i.verdict == Verdict::Lost)
            .count();
        let reach = present + unmeasured;
        let flat = flatten(&report);

        assert!(
            flat.contains(&format!("of the {} item(s) here", corpus.items.len())),
            "{id}: the N (every item) count must be stated: {report}"
        );
        assert!(
            flat.contains(&format!("mechanism reaches {reach} of them at all")),
            "{id}: the K (reach) count must be derived from the corpus and \
             stated, got reach={reach}: {report}"
        );
        assert!(
            flat.contains(&format!("represent the LOSS of exactly {lost}")),
            "{id}: the lost count must be stated beside K, not alone, got \
             lost={lost}: {report}"
        );

        let finding = flat
            .find("THE FINDING THIS CORPUS MAKES SAYABLE")
            .expect("the finding sentence");
        let reach_pos = flat[finding..]
            .find(&format!("reaches {reach} of them at all"))
            .expect("reach stated inside the finding sentence");
        let lost_pos = flat[finding..]
            .find(&format!("exactly {lost}"))
            .expect("lost stated inside the finding sentence");
        assert!(
            reach_pos < lost_pos,
            "{id}: K must be stated before the loss count it qualifies, in \
             the same sentence: {report}"
        );
    }
}

/// Requirement 5 (The Repertoire's own Critical finding: an artifact
/// "listed seven capabilities the world already had under a heading reading
/// *missing*"). Deliberately narrow, not a word-sieve over every synonym
/// for "backlog" — this campaign has already thrown away two over-broad
/// default-deny lists (ledger #21, #25) — so this checks for the ONE
/// heading shape that is the documented failure, and separately names the
/// heading this renderer actually chose for the actionable output instead.
#[test]
fn no_heading_reads_like_a_missing_capabilities_backlog() {
    for (corpus, id) in [
        (load_asimov(), "asimov-1989"),
        (load_henrich(), "henrich-2004-extended"),
    ] {
        let report = render(&corpus, id);
        let headings: Vec<&str> = report
            .lines()
            .filter(|l| l.starts_with("## "))
            .map(|l| l.trim_start_matches("## ").trim())
            .collect();
        assert!(
            !headings.iter().any(|h| h.eq_ignore_ascii_case("missing")),
            "{id}: a heading reading `Missing` appeared — The Repertoire's own \
             documented failure shape: {headings:?}"
        );
        assert!(
            headings.contains(&"Demand set"),
            "{id}: the actionable output must be headed `## Demand set`, the \
             heading this renderer chose instead of one implying a work \
             queue: {headings:?}"
        );
    }
}

// --- The chosen/inherited disclosure rule (pre-merge fix wave, F1) --------
//
// The rule was ratified by campaign ledger #13's second ruling, written into
// `technologies/CLAUDE.md` ("Task 4's resolver must enforce the chosen rule,
// two-directionally"), and published as a MUST in
// `henrich-2004-extended`'s `provenance` — which the report prints verbatim.
// It was never built (ledger #38). It holds exactly in both committed
// corpora, so `the_real_corpora_satisfy_the_chosen_disclosure_rule` below is
// GREEN ON ARRIVAL and proves nothing on its own; the four constructed
// fixtures here are what actually exercise it, per ledger #27's rule that a
// guard validated only against corpora containing no violation is vacuously
// green.

/// A corpus whose `items` array body is given verbatim, so a test can build
/// a real `presupposes` lattice rather than the one-item shape
/// [`corpus_with`] produces. Parsed through the real parser, so the lattice
/// validation applies here too.
fn lattice_corpus(items: &str) -> Corpus {
    let json = format!(
        r#"{{ "corpus": "fixture", "unit": "technology", "ordered": false,
              "provenance": "fixture", "frozen": "fixture",
              "items": [ {items} ] }}"#
    );
    hornvale::technologies::parse(&json)
}

/// Direction one: a CHOSEN item — no prerequisite anywhere in its closure is
/// `absent` — carrying no `disclosure` must red. An empty `disclosure` column
/// in the report reads as "authored blind", which is false for a verdict
/// reached by searching the repository and is the inversion of the one thing
/// `disclosure` exists to expose.
#[test]
fn a_chosen_item_without_a_disclosure_is_a_finding() {
    let c = lattice_corpus(
        r#"{ "id": "a", "title": "A", "introduces": "tok-a", "presupposes": [],
              "verdict": "deferred", "anchor": "registry:MEM-8" }"#,
    );
    match disclosure_gaps(&c).as_slice() {
        [Finding::Disclosure { id, chosen, why }] => {
            assert_eq!(id, "a");
            assert!(*chosen, "the finding must report the CHOSEN direction");
            assert!(
                why.contains("CHOSEN") && why.contains("no `disclosure`"),
                "the finding must name the direction and the missing field: {why}"
            );
        }
        other => panic!("expected one chosen-without-disclosure finding, got {other:?}"),
    }
}

/// Direction two: an INHERITED item — at least one `absent` prerequisite in
/// its closure — carrying a `disclosure` must red. Over-coverage is a defect
/// too (ledger #13): a refusal that could not have changed the verdict is
/// evidence for a `note`, and a disclosure on a forced verdict is noise that
/// makes the real ones harder to find.
///
/// A one-directional implementation would pass this test while having built
/// half a guard — the same half-built shape decision 0936 refuses for the
/// trajectory axis one family over.
#[test]
fn an_inherited_item_with_a_disclosure_is_a_finding() {
    let c = lattice_corpus(
        r#"{ "id": "a", "title": "A", "introduces": "tok-a", "presupposes": [],
              "verdict": "absent", "anchor": "", "disclosure": "a had read the model" },
            { "id": "b", "title": "B", "introduces": "tok-b", "presupposes": ["a"],
              "verdict": "absent", "anchor": "", "disclosure": "so had b" }"#,
    );
    match disclosure_gaps(&c).as_slice() {
        [Finding::Disclosure { id, chosen, why }] => {
            assert_eq!(id, "b", "only the inherited item may be reported");
            assert!(!*chosen, "the finding must report the INHERITED direction");
            assert!(
                why.contains("INHERITED") && why.contains("`note`"),
                "the finding must name the direction and the repair: {why}"
            );
        }
        other => panic!("expected one inherited-with-disclosure finding, got {other:?}"),
    }
}

/// The compliant shape is clean, in both directions at once — the positive
/// control without which the two tests above could both be satisfied by a
/// function that reports every item. `a` is chosen and discloses; `b`
/// inherits `a`'s `absent` and does not.
#[test]
fn a_compliant_corpus_has_no_disclosure_gaps() {
    let c = lattice_corpus(
        r#"{ "id": "a", "title": "A", "introduces": "tok-a", "presupposes": [],
              "verdict": "absent", "anchor": "", "disclosure": "a had read the model" },
            { "id": "b", "title": "B", "introduces": "tok-b", "presupposes": ["a"],
              "verdict": "absent", "anchor": "" }"#,
    );
    assert!(
        disclosure_gaps(&c).is_empty(),
        "a corpus satisfying the rule in both directions must be clean: {:?}",
        disclosure_gaps(&c)
    );
}

/// **The test that distinguishes the ratified rule from the proxy that cost
/// two fix rounds.** `b` has a prerequisite, so it is NOT a root; that
/// prerequisite is `deferred`, not `absent`, so nothing upstream forces
/// `b`'s verdict and `b` IS chosen. It owes a `disclosure` and has none.
///
/// A **root-keyed** check — `presupposes.is_empty()`, the proxy ledger #13
/// found twice and family law names as the thing this must not be — reports
/// nothing here and passes. `inv-parchment` is this exact shape in the real
/// corpus, and it was this shape at the freeze: roots are a strict subset of
/// the chosen items, so the proxy under-covers from a corpus's first
/// authoring and needs no trigger to do it.
#[test]
fn a_chosen_non_root_is_a_finding_where_a_root_keyed_check_would_pass() {
    let c = lattice_corpus(
        r#"{ "id": "a", "title": "A", "introduces": "tok-a", "presupposes": [],
              "verdict": "deferred", "anchor": "registry:MEM-8",
              "disclosure": "a had read the model" },
            { "id": "b", "title": "B", "introduces": "tok-b", "presupposes": ["a"],
              "verdict": "absent", "anchor": "" }"#,
    );
    assert!(
        is_chosen(&c, "a") && is_chosen(&c, "b"),
        "sanity: both items are chosen — `a` is a root and `b`'s only \
         prerequisite is `deferred`, not `absent`"
    );
    match disclosure_gaps(&c).as_slice() {
        [Finding::Disclosure { id, chosen, .. }] => {
            assert_eq!(id, "b", "the chosen NON-ROOT is the item that owes one");
            assert!(*chosen);
        }
        other => panic!(
            "expected the chosen non-root `b` to be reported; a root-keyed \
             check reports nothing here, which is the defect this test \
             exists for. Got {other:?}"
        ),
    }
}

/// `absent`-ness is read from the whole CLOSURE, not only from the immediate
/// `presupposes` edges. `c` presupposes `b`, whose verdict is `deferred`;
/// the `absent` sits one rung further up, at `a`. A one-level check would
/// call `c` chosen and demand a disclosure it does not owe.
#[test]
fn an_absent_prerequisite_two_rungs_up_still_makes_an_item_inherited() {
    let c = lattice_corpus(
        r#"{ "id": "a", "title": "A", "introduces": "tok-a", "presupposes": [],
              "verdict": "absent", "anchor": "", "disclosure": "a had read the model" },
            { "id": "b", "title": "B", "introduces": "tok-b", "presupposes": ["a"],
              "verdict": "deferred", "anchor": "registry:MEM-8" },
            { "id": "c", "title": "C", "introduces": "tok-c", "presupposes": ["b"],
              "verdict": "absent", "anchor": "" }"#,
    );
    assert!(
        !is_chosen(&c, "c"),
        "`c` inherits `a`'s absent two rungs up"
    );
    assert!(
        disclosure_gaps(&c).is_empty(),
        "neither `b` nor `c` owes a disclosure: {:?}",
        disclosure_gaps(&c)
    );
}

/// The rule must be reachable from the entry point the CLI's `technologies
/// check` actually calls. `audit` alone deliberately skips it (every
/// one-item anchor fixture in this file is chosen-without-disclosure by
/// construction, and would otherwise carry a finding about a field it has
/// nothing to do with), so a check that lived only in `audit` would be
/// enforced by nothing a caller runs — which is the shape of ledger #38's
/// defect reintroduced one layer down.
#[test]
fn audit_family_includes_disclosure_gaps() {
    let c = lattice_corpus(
        r#"{ "id": "a", "title": "A", "introduces": "tok-a", "presupposes": [],
              "verdict": "deferred", "anchor": "registry:MEM-8" }"#,
    );
    let per_item = audit(&c, &workspace_root());
    assert!(
        !per_item
            .iter()
            .any(|f| matches!(f, Finding::Disclosure { .. })),
        "`audit` alone must not carry a disclosure finding, or every \
         single-item fixture in this file gains one: {per_item:?}"
    );
    assert!(
        audit_family(&c, &workspace_root())
            .iter()
            .any(|f| matches!(f, Finding::Disclosure { .. })),
        "the family entry point — what `hornvale technologies check` calls — \
         must enforce the rule"
    );
}

/// The committed corpora satisfy the rule, with a POSITIVE CONTROL so the
/// green is not vacuous: both corpora must contain at least one chosen and
/// at least one inherited item, or "no gaps" would be a statement about an
/// empty population rather than about the rule.
///
/// Asserts the RELATIONSHIP, never the counts. The chosen and disclosed sets
/// move with any re-verdict, and a test literal pinning today's numbers
/// would have to be edited by exactly the campaign whose change it is
/// supposed to scrutinise.
#[test]
fn the_real_corpora_satisfy_the_chosen_disclosure_rule() {
    for corpus in [load_asimov(), load_henrich()] {
        let chosen = corpus
            .items
            .iter()
            .filter(|i| is_chosen(&corpus, &i.id))
            .count();
        assert!(
            chosen > 0 && chosen < corpus.items.len(),
            "{}: the rule must have something to bite on in BOTH directions \
             — {chosen} chosen of {} items",
            corpus.corpus,
            corpus.items.len()
        );
        let gaps = disclosure_gaps(&corpus);
        assert!(
            gaps.is_empty(),
            "{} violates the chosen/inherited disclosure rule:\n{gaps:#?}",
            corpus.corpus
        );
    }
}

// --- `reach` counts every verdict that passed reach (fix wave, F2) --------

/// THE INVARIANT THE OLD DEFINITION VIOLATED: a corpus cannot represent the
/// loss of more items than its mechanism reaches. `reach` was `present +
/// unmeasured`, and under the ratified pipeline (ledger #6, #16) a `grown`,
/// `flat` or `lost` item has passed reach BY DEFINITION — so one `lost` item
/// made the report read "reaches 0 of them at all … and of those 0, it can
/// currently represent the LOSS of exactly 1."
///
/// Asserted over the RENDERED STRING and on a CONSTRUCTED corpus, both
/// deliberately. Neither committed corpus scores a measured verdict (ledger
/// #27: the corpora were frozen against a monotone clock and cannot exercise
/// this), so the sibling test
/// [`the_reach_and_loss_finding_states_both_counts_together`] ranges over
/// data where the bug is invisible — and it recomputes `reach` with
/// production's own formula, so it asserts that the renderer printed the
/// function's output rather than that the definition is right. This one
/// names the invariant instead of the formula.
#[test]
fn the_loss_count_never_exceeds_the_reach_count() {
    let c = lattice_corpus(
        r#"{ "id": "a", "title": "A", "introduces": "tok-a", "presupposes": [],
              "verdict": "lost", "anchor": "doc:book/src/domesday/settlement.md",
              "disclosure": "a had read the model" },
            { "id": "b", "title": "B", "introduces": "tok-b", "presupposes": [],
              "verdict": "grown", "anchor": "doc:book/src/domesday/settlement.md",
              "disclosure": "b had read the model" }"#,
    );
    let flat = flatten(&render(&c, "fixture"));
    let reach = number_after(&flat, "mechanism reaches ");
    let lost = number_after(&flat, "represent the LOSS of exactly ");
    assert!(
        lost <= reach,
        "a corpus cannot represent the loss of more items than it reaches: \
         reach={reach}, lost={lost} in: {flat}"
    );
    assert_eq!(
        reach, 2,
        "both measured items have passed reach: one `lost`, one `grown`"
    );
    assert_eq!(lost, 1);
    assert!(
        flat.contains("(0 `present`, 0 `unmeasured`, 1 `grown`, 0 `flat`, 1 `lost`)"),
        "the breakdown must name every non-zero component, so it sums to the \
         reach figure in front of it: {flat}"
    );
}

/// The first run of ASCII digits immediately following `marker` in `s`,
/// parsed. Panics when the marker is absent or is not followed by a digit —
/// either means the report no longer says what the caller is asserting
/// about, which is a failure and not a zero.
fn number_after(s: &str, marker: &str) -> usize {
    let at = s
        .find(marker)
        .unwrap_or_else(|| panic!("marker {marker:?} not in report: {s}"))
        + marker.len();
    let digits: String = s[at..].chars().take_while(|c| c.is_ascii_digit()).collect();
    assert!(
        !digits.is_empty(),
        "no number follows {marker:?} in report: {s}"
    );
    digits.parse().expect("digits parse")
}

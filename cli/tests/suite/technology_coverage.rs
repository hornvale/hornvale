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
    Anchor, Corpus, Finding, audit, audit_family, cross_corpus_ruling_gaps, load,
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

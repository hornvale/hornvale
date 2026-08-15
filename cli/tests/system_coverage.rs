//! The Compendium's ratchet and anchor discipline.

use std::path::PathBuf;
use std::process::Command;

/// The ratchet: `systems report`'s live output must match the committed
/// artifact byte-for-byte. A moved item means an anchor moved — a decision
/// was superseded, or a registry row shipped — which is a finding, not a
/// formality. Regenerate deliberately with `make rebaseline` and read the
/// diff.
#[test]
fn committed_system_coverage_matches_the_live_report() {
    let root = workspace_root();
    let out = Command::new(env!("CARGO_BIN_EXE_hornvale"))
        .args([
            "systems",
            "--corpus",
            "systems/wolverson-2021.system.json",
            "report",
        ])
        .current_dir(&root)
        .output()
        .expect("runs the binary");
    assert!(out.status.success(), "systems report failed: {out:?}");
    let live = String::from_utf8(out.stdout).expect("utf-8");
    hornvale_kernel::golden::assert_golden(
        &root.join("docs/audits/system-coverage-wolverson-2021.md"),
        &live,
        "the system-coverage report drifted from the committed artifact. An item that \
         changed verdict means an anchor moved — a decision was superseded, or a \
         registry row shipped — which is a finding, not a formality. Regenerate \
         deliberately with `make rebaseline` and read the diff.",
    );
}

/// Regression for a `?`-inside-a-loop bug: `line.strip_prefix(..)?` on a
/// non-matching line propagates that line's `None` out of the WHOLE
/// function, not just past that line, so a naive first draft returned
/// `None` unconditionally — line 1 of every real report is the generated-file
/// banner, which never matches — and `check`'s novelty guard silently never
/// fired. Caught by hand against a scratch corpus before this test existed;
/// this pins the fix.
#[test]
fn committed_absent_count_reads_past_the_banner_line_to_the_tally() {
    let report = "<!-- GENERATED FILE — do not edit. -->\n\n\
                  # System coverage\n\n\
                  ## Tally\n\n\
                  - present: 3 (4%)\n\
                  - refused: 0 (0%)\n\
                  - deferred: 0 (0%)\n\
                  - absent: 71 (96%)\n\
                  - inapplicable: 0 (0%)\n";
    assert_eq!(hornvale::systems::committed_absent_count(report), Some(71));
}

/// `None` — not `Some(0)` — for text with no tally at all, so a caller can
/// tell "nothing to compare against" from "zero absent items".
#[test]
fn committed_absent_count_is_none_without_a_tally_line() {
    assert_eq!(
        hornvale::systems::committed_absent_count("not a report\njust some text\n"),
        None
    );
}

/// `check` must fail specifically on NOVELTY — the live corpus's `absent`
/// count rising above the committed artifact's — with its own regression
/// message, not just the generic drift message. Exercises the real CLI
/// binary against a scratch corpus that adds one more `absent` item on top
/// of the real corpus, so `live_absent` rises by exactly one above the
/// committed artifact's count.
///
/// The expected pair is READ FROM the committed report rather than written
/// here. Task 4 authored the verdicts and took the `absent` count from 74 to
/// 9, which reddened this test on a hard-coded `"74 to 75"` while the guard
/// itself was behaving perfectly — an assertion pinned to a tally it does not
/// test is a false alarm waiting on the next verdict that moves.
#[test]
fn check_fails_on_novelty_when_the_absent_count_rises() {
    let root = workspace_root();
    let json = std::fs::read_to_string(root.join("systems/wolverson-2021.system.json"))
        .expect("reads the real corpus");
    let mut corpus: serde_json::Value = serde_json::from_str(&json).expect("parses as JSON");
    corpus["items"]
        .as_array_mut()
        .expect("items is an array")
        .push(serde_json::json!({
            "id": "99.9",
            "kind": "chapter",
            "title": "A newly absent item",
            "verdict": "absent"
        }));
    let scratch = std::env::temp_dir().join(format!(
        "hv-compendium-novelty-{}.system.json",
        std::process::id()
    ));
    std::fs::write(
        &scratch,
        serde_json::to_string(&corpus).expect("serializes"),
    )
    .expect("writes the scratch corpus");

    let out = Command::new(env!("CARGO_BIN_EXE_hornvale"))
        .args([
            "systems",
            "--corpus",
            scratch.to_str().expect("scratch path is utf-8"),
            "check",
        ])
        .current_dir(&root)
        .output()
        .expect("runs the binary");
    let _ = std::fs::remove_file(&scratch);

    assert!(
        !out.status.success(),
        "check must fail on a rising absent count"
    );
    let stderr = String::from_utf8(out.stderr).expect("utf-8");
    let committed =
        std::fs::read_to_string(root.join("docs/audits/system-coverage-wolverson-2021.md"))
            .expect("the committed report is readable");
    let baseline =
        hornvale::systems::committed_absent_count(&committed).expect("the report carries a tally");
    let expected = format!("{baseline} to {}", baseline + 1);
    assert!(
        stderr.contains("regressed") && stderr.contains(&expected),
        "expected a novelty-specific regression message naming `{expected}`, got: {stderr}"
    );
}

fn workspace_root() -> PathBuf {
    std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("workspace root")
        .to_path_buf()
}

fn load_wolverson() -> hornvale::systems::Corpus {
    let path = workspace_root().join("systems/wolverson-2021.system.json");
    let json = std::fs::read_to_string(&path).expect("corpus is readable");
    hornvale::systems::load(&json).expect("corpus parses")
}

/// The freeze. A corpus's item count is asserted so that changing the
/// catalogue is a deliberate act, never a side effect — the same discipline
/// `tropes/` carries for its situation counts.
#[test]
fn the_wolverson_corpus_is_frozen_at_its_declared_size() {
    let c = load_wolverson();
    assert_eq!(c.items.len(), 74, "the frozen corpus changed size");
    assert!(c.ordered, "Wolverson's chapters are a pedagogical ladder");
    assert_eq!(c.unit, "chapter");
}

/// Provenance is emitted, not documented (decision 0095): a reader cannot
/// reach a score without passing the statement that this is one instrument
/// with a known bias.
#[test]
fn the_corpus_declares_its_provenance_and_freeze() {
    let c = load_wolverson();
    assert!(!c.provenance.is_empty(), "provenance is required");
    assert!(!c.frozen.is_empty(), "the freeze note is required");
}

use hornvale::systems::{Finding, RepoFacts, audit, load};

/// Build a one-item, `ordered: true` corpus with the given verdict and
/// anchor. All 8 existing call sites in this file want exactly that shape,
/// so it keeps the bare two-argument signature; `corpus_with_ordered` below
/// is the narrow sibling for the one test that needs `ordered: false`,
/// rather than threading a bare `true`/`false` through every call here.
fn corpus_with(verdict: &str, anchor: Option<&str>) -> hornvale::systems::Corpus {
    corpus_with_ordered(verdict, anchor, true)
}

/// Same fixture shape as `corpus_with`, with `ordered` as a caller-chosen
/// third argument.
fn corpus_with_ordered(
    verdict: &str,
    anchor: Option<&str>,
    ordered: bool,
) -> hornvale::systems::Corpus {
    let anchor_json = match anchor {
        Some(a) => format!(r#", "anchor": "{a}""#),
        None => String::new(),
    };
    let json = format!(
        r#"{{ "corpus": "fixture", "unit": "chapter", "ordered": {ordered},
              "provenance": "fixture", "frozen": "fixture",
              "items": [ {{ "id": "1.1", "kind": "chapter", "title": "T",
                            "verdict": "{verdict}"{anchor_json} }} ] }}"#
    );
    load(&json).expect("fixture parses")
}

/// Spec §14 (ratified as a decision): an ordinal reading requires a declared
/// ordering, and an unordered catalogue ranked by `id` would manufacture a
/// ladder its source never had. So for `ordered: false`, `render` must omit
/// the `## First unmet` section ENTIRELY — not render it empty, not fall
/// back to id order. Asserts on the header's absence, not just the absence
/// of an item id, so a partially-rendered section still fails this. NetHack,
/// the intended second corpus in `systems::CORPORA`, is exactly this case,
/// and nothing exercised it before this test: `corpus_with` (and the shipped
/// Wolverson corpus) are both `ordered: true`.
#[test]
fn render_omits_first_unmet_entirely_when_the_corpus_is_unordered() {
    let corpus = corpus_with_ordered("absent", None, false);
    let rendered = hornvale::systems::render(&corpus, "fixture.json");
    assert!(
        !rendered.contains("## First unmet"),
        "an unordered corpus must not render a First unmet section at all:\n{rendered}"
    );
}

fn facts() -> RepoFacts {
    RepoFacts::gather(&workspace_root()).expect("repo facts gather")
}

/// UNJUSTIFIED: a non-`absent` verdict with no anchor. The tropes family's
/// reasonless-`inapplicable` rule, generalized.
#[test]
fn a_refused_verdict_without_an_anchor_is_unjustified() {
    let f = audit(&corpus_with("refused", None), &facts());
    assert!(
        matches!(f.as_slice(), [Finding::Unjustified { .. }]),
        "expected UNJUSTIFIED, got {f:?}"
    );
}

/// UNJUSTIFIED also covers the wrong KIND of anchor: `refused` means a
/// decision forbids it, and a path cannot express that.
#[test]
fn a_refused_verdict_anchored_to_a_path_is_unjustified() {
    let f = audit(
        &corpus_with("refused", Some("path:cli/src/main.rs")),
        &facts(),
    );
    assert!(
        matches!(f.as_slice(), [Finding::Unjustified { .. }]),
        "expected UNJUSTIFIED for a wrong-kind anchor, got {f:?}"
    );
}

/// DANGLING: 0014 was superseded by 0126, so it is absent from
/// `decisions-in-force.md` by construction. A refusal citing it has lost its
/// ground and must not read as settled.
#[test]
fn a_refusal_citing_a_superseded_decision_is_dangling() {
    let f = audit(&corpus_with("refused", Some("decision:0014")), &facts());
    assert!(
        matches!(f.as_slice(), [Finding::Dangling { .. }]),
        "expected DANGLING for superseded 0014, got {f:?}"
    );
}

/// A refusal citing a decision that IS in force is clean. The positive
/// control: without it, a resolver that flagged everything would pass the
/// test above (`an-empty-diff-needs-a-positive-control`).
#[test]
fn a_refusal_citing_an_in_force_decision_is_clean() {
    let f = audit(&corpus_with("refused", Some("decision:0070")), &facts());
    assert!(
        f.is_empty(),
        "0070 is in force; expected no findings, got {f:?}"
    );
}

/// DANGLING for a registry row that does not exist.
#[test]
fn a_deferral_citing_an_unknown_registry_row_is_dangling() {
    let f = audit(
        &corpus_with("deferred", Some("registry:CLIENT-no-such-row-exists")),
        &facts(),
    );
    assert!(
        matches!(f.as_slice(), [Finding::Dangling { .. }]),
        "expected DANGLING, got {f:?}"
    );
}

/// STALE-DEFERRED: seam-guard's STALE-DECL, exactly. A one-directional
/// acknowledgement can only ever be satisfied, so it rots; this fails the
/// moment reality catches up. `CLIENT-action-clock` reads `shipped` today.
#[test]
fn a_deferral_against_a_shipped_registry_row_is_stale() {
    let f = audit(
        &corpus_with("deferred", Some("registry:CLIENT-action-clock")),
        &facts(),
    );
    assert!(
        matches!(f.as_slice(), [Finding::StaleDeferred { .. }]),
        "expected STALE-DEFERRED for a shipped row, got {f:?}"
    );
}

/// DANGLING for a `test:` anchor naming a symbol that is a strict PREFIX of
/// a real one, never the real one itself. The bare word `verdict` names no
/// function anywhere in `cli/`, but `verdict_name` does
/// (`cli/src/systems.rs`); a naive substring search for `fn` immediately
/// followed by the symbol would find a match inside that longer definition
/// and wrongly read the anchor as resolved. This is the false-CLEAN a
/// review found: the instrument exists to notice when an anchor stops
/// resolving, and a false-clean means it silently stops noticing.
#[test]
fn a_test_anchor_naming_a_strict_prefix_of_a_real_symbol_is_dangling() {
    let f = audit(
        &corpus_with("present", Some("test:hornvale::verdict")),
        &facts(),
    );
    assert!(
        matches!(f.as_slice(), [Finding::Dangling { .. }]),
        "expected DANGLING for a prefix-only symbol match, got {f:?}"
    );
}

/// An `absent` verdict must carry NO anchor — it is the one verdict that
/// claims nothing, and an anchored `absent` is a miscategorised row.
#[test]
fn an_absent_verdict_carrying_an_anchor_is_unjustified() {
    let f = audit(&corpus_with("absent", Some("decision:0070")), &facts());
    assert!(
        matches!(f.as_slice(), [Finding::Unjustified { .. }]),
        "expected UNJUSTIFIED for an anchored `absent`, got {f:?}"
    );
}

/// The shipped corpus must be clean at all times.
#[test]
fn the_wolverson_corpus_has_no_anchor_findings() {
    let f = audit(&load_wolverson(), &facts());
    assert!(f.is_empty(), "the corpus has anchor findings:\n{f:#?}");
}

/// The empty-file floor: a `decisions-in-force.md` that exists but parses
/// to zero in-force decisions must be a parse failure, not `Ok(empty set)` —
/// the same failure class as the mechanism-anchor prefix bug above, a
/// silent wrong answer from the resolver. Mirrors the corpus's own
/// frozen-count assertion in spirit.
#[test]
fn gather_refuses_a_decisions_file_that_parses_to_zero_entries() {
    let root = std::env::temp_dir().join(format!(
        "hv-compendium-empty-decisions-{}",
        std::process::id()
    ));
    let digest_dir = root.join("docs/digest");
    std::fs::create_dir_all(&digest_dir).expect("make scratch digest dir");
    std::fs::write(
        digest_dir.join("decisions-in-force.md"),
        "# Decisions in force\n\nGENERATED — none matched today.\n",
    )
    .expect("writes an empty decisions file");

    let result = RepoFacts::gather(&root);
    let _ = std::fs::remove_dir_all(&root);

    let err = result.expect_err("an empty in-force set must be an error, not Ok(empty set)");
    assert!(
        err.contains("zero in-force decisions"),
        "expected the empty-parse error to name what happened, got: {err}"
    );
}

/// Same floor, for the registry.
#[test]
fn gather_refuses_a_registry_file_that_parses_to_zero_rows() {
    let root = std::env::temp_dir().join(format!(
        "hv-compendium-empty-registry-{}",
        std::process::id()
    ));
    let digest_dir = root.join("docs/digest");
    let frontier_dir = root.join("book/src/frontier");
    std::fs::create_dir_all(&digest_dir).expect("make scratch digest dir");
    std::fs::create_dir_all(&frontier_dir).expect("make scratch frontier dir");
    std::fs::write(
        digest_dir.join("decisions-in-force.md"),
        "- **0001** A real decision\n",
    )
    .expect("writes a valid decisions file");
    std::fs::write(
        frontier_dir.join("idea-registry.md"),
        "# Idea registry\n\nNo rows today.\n",
    )
    .expect("writes an empty registry file");

    let result = RepoFacts::gather(&root);
    let _ = std::fs::remove_dir_all(&root);

    let err = result.expect_err("an empty registry must be an error, not Ok(empty map)");
    assert!(
        err.contains("zero registry rows"),
        "expected the empty-parse error to name what happened, got: {err}"
    );
}

/// The stamp printed in a `Decision`-anchor `Dangling` finding must move
/// when `decisions-in-force.md`'s content moves — otherwise a "confusing
/// red" (spec §5) stays exactly as confusing across two different decision
/// states. Regression coverage for deriving the stamp from
/// `docs/digest/facts.jsonl` (unrelated to either anchor source) instead of
/// the file the finding actually cites.
#[test]
fn the_decisions_stamp_changes_when_the_decisions_file_changes() {
    let root_a = std::env::temp_dir().join(format!("hv-compendium-stamp-a-{}", std::process::id()));
    let root_b = std::env::temp_dir().join(format!("hv-compendium-stamp-b-{}", std::process::id()));
    for (root, decision_line) in [
        (&root_a, "- **0001** First"),
        (&root_b, "- **0002** Second"),
    ] {
        let digest_dir = root.join("docs/digest");
        let frontier_dir = root.join("book/src/frontier");
        std::fs::create_dir_all(&digest_dir).expect("make scratch digest dir");
        std::fs::create_dir_all(&frontier_dir).expect("make scratch frontier dir");
        std::fs::create_dir_all(root.join("domains")).expect("make scratch domains dir");
        std::fs::create_dir_all(root.join("windows")).expect("make scratch windows dir");
        std::fs::write(
            digest_dir.join("decisions-in-force.md"),
            format!("{decision_line}\n"),
        )
        .expect("writes a decisions file");
        std::fs::write(
            frontier_dir.join("idea-registry.md"),
            "| ID-1 | idea | raw | high | nowhere |\n",
        )
        .expect("writes a registry file");
    }

    let facts_a = RepoFacts::gather(&root_a).expect("gather A");
    let facts_b = RepoFacts::gather(&root_b).expect("gather B");
    let dangling_a = audit(&corpus_with("refused", Some("decision:0099")), &facts_a);
    let dangling_b = audit(&corpus_with("refused", Some("decision:0099")), &facts_b);

    let _ = std::fs::remove_dir_all(&root_a);
    let _ = std::fs::remove_dir_all(&root_b);

    let [Finding::Dangling { why: why_a, .. }] = dangling_a.as_slice() else {
        panic!("expected a single DANGLING finding for A, got {dangling_a:?}");
    };
    let [Finding::Dangling { why: why_b, .. }] = dangling_b.as_slice() else {
        panic!("expected a single DANGLING finding for B, got {dangling_b:?}");
    };
    assert_ne!(
        why_a, why_b,
        "the decisions-in-force stamp did not move when the decisions file changed"
    );
}

/// Build a synthetic repo root with a single registry row (`ID-1`) whose
/// Status cell reads exactly `status`, gather `RepoFacts` from it, and clean
/// up. `tag` keeps concurrent calls (e.g. from different tests in this file)
/// from colliding on the same temp path. Follows the same synthetic-root
/// pattern as `gather_refuses_a_*` and `the_decisions_stamp_changes_*` above.
fn facts_with_registry_status(tag: &str, status: &str) -> RepoFacts {
    let root = std::env::temp_dir().join(format!(
        "hv-compendium-registry-status-{tag}-{}",
        std::process::id()
    ));
    let digest_dir = root.join("docs/digest");
    let frontier_dir = root.join("book/src/frontier");
    std::fs::create_dir_all(&digest_dir).expect("make scratch digest dir");
    std::fs::create_dir_all(&frontier_dir).expect("make scratch frontier dir");
    std::fs::create_dir_all(root.join("domains")).expect("make scratch domains dir");
    std::fs::create_dir_all(root.join("windows")).expect("make scratch windows dir");
    std::fs::write(
        digest_dir.join("decisions-in-force.md"),
        "- **0001** A real decision\n",
    )
    .expect("writes a decisions file");
    std::fs::write(
        frontier_dir.join("idea-registry.md"),
        format!("| ID-1 | idea | {status} | high | nowhere |\n"),
    )
    .expect("writes a registry file");

    let facts = RepoFacts::gather(&root).expect("gather synthetic facts");
    let _ = std::fs::remove_dir_all(&root);
    facts
}

/// STALE-DEFERRED must fire on a QUALIFIED shipped status, not just the bare
/// word. Measured against the real registry (1269 status cells): 167 read
/// exactly `shipped`, but 23 read `shipped (…)` and 16 more are
/// markdown-bolded — the original `status == "shipped"` exact match could
/// never see any of those 39. `CLIENT-action-clock` (the existing fixture's
/// row) happens to be one of the 167 bare ones, so nothing caught this until
/// it was measured on purpose.
#[test]
fn a_deferral_against_a_parenthetically_qualified_shipped_status_is_stale() {
    let facts = facts_with_registry_status("qualified", "shipped (C1)");
    let f = audit(&corpus_with("deferred", Some("registry:ID-1")), &facts);
    assert!(
        matches!(f.as_slice(), [Finding::StaleDeferred { .. }]),
        "expected STALE-DEFERRED for `shipped (C1)`, got {f:?}"
    );
}

/// Same defect, the markdown-bolded shape (16 real rows read `**shipped**`
/// or `**shipped (…)**`).
#[test]
fn a_deferral_against_a_bolded_shipped_status_is_stale() {
    let facts = facts_with_registry_status("bolded", "**shipped**");
    let f = audit(&corpus_with("deferred", Some("registry:ID-1")), &facts);
    assert!(
        matches!(f.as_slice(), [Finding::StaleDeferred { .. }]),
        "expected STALE-DEFERRED for `**shipped**`, got {f:?}"
    );
}

/// The NEGATIVE control the fix must not break: `elaborated (slice-2
/// shipped)` merely MENTIONS "shipped" inside its parenthetical — the row
/// itself is `elaborated`, not shipped, and must read clean. This is what
/// stops the fix for the two tests above from degenerating into a substring
/// search for "shipped" anywhere in the cell.
#[test]
fn a_deferral_against_an_elaborated_status_mentioning_shipped_in_prose_is_clean() {
    let facts = facts_with_registry_status("prose-mention", "elaborated (slice-2 shipped)");
    let f = audit(&corpus_with("deferred", Some("registry:ID-1")), &facts);
    assert!(
        f.is_empty(),
        "elaborated (with 'shipped' merely mentioned in its parenthetical) must not \
         read as STALE-DEFERRED, got {f:?}"
    );
}

/// A row that shipped and was later superseded is still not something to
/// defer against — `normalize_status` strips the trailing `→ <status>`
/// transition, so `shipped → superseded (Goldengrove)` normalizes to
/// `shipped` and SHOULD fire, same as the bare word.
#[test]
fn a_deferral_against_a_shipped_then_superseded_status_is_stale() {
    let facts = facts_with_registry_status("transitioned", "shipped → superseded (Goldengrove)");
    let f = audit(&corpus_with("deferred", Some("registry:ID-1")), &facts);
    assert!(
        matches!(f.as_slice(), [Finding::StaleDeferred { .. }]),
        "expected STALE-DEFERRED for a shipped-then-superseded status, got {f:?}"
    );
}

// --- Task 5: the surplus read, the matrix, and the two honesty fixes ------

/// The surplus read: a subsystem no `present` verdict cites is one this
/// catalogue has no vocabulary for. Derived, never authored — an authored
/// list is the rot the anchor discipline exists to prevent.
///
/// The brief's own worked example uses a bare `|s| …` closure parameter;
/// renamed to `dir` here because `cli/tests/claim_shape.rs`'s seed-loop scan
/// treats a single-letter `s` binding as seed-shaped by convention (it is
/// the idiom used across the tree's real seed loops) and flags any `.any(|s|
/// …)` closure regardless of what `s` actually holds — a false positive
/// against this string, not a seed. The rename is the whole fix: the
/// assertion is unchanged.
#[test]
fn the_surplus_read_names_subsystems_no_chapter_cites() {
    let surplus = hornvale::systems::surplus(&load_wolverson(), &facts());
    assert!(
        surplus.iter().any(|dir| dir.contains("language")),
        "no chapter of a roguelike tutorial asks anything domains/language \
         would answer; expected it in the surplus, got {surplus:?}"
    );
}

/// The positive control the test above needs: a subsystem the real corpus
/// DOES cite (`domains/terrain`, via `test:hornvale-terrain::…` anchors)
/// must NOT appear in the surplus list. Without this, a `surplus` that
/// returned every subsystem unconditionally would still pass the test
/// above.
#[test]
fn the_surplus_read_excludes_a_subsystem_the_corpus_does_cite() {
    let surplus = hornvale::systems::surplus(&load_wolverson(), &facts());
    assert!(
        !surplus.iter().any(|dir| dir == "domains/terrain"),
        "domains/terrain is cited by test:hornvale-terrain::… anchors in the \
         real corpus and must not read as surplus, got {surplus:?}"
    );
}

#[test]
fn committed_system_matrix_matches_the_live_render() {
    let root = workspace_root();
    let out = Command::new(env!("CARGO_BIN_EXE_hornvale"))
        .args(["systems", "matrix"])
        .current_dir(&root)
        .output()
        .expect("runs the binary");
    assert!(out.status.success(), "systems matrix failed: {out:?}");
    let live = String::from_utf8(out.stdout).expect("utf-8");
    hornvale_kernel::golden::assert_golden(
        &root.join("docs/audits/system-matrix.md"),
        &live,
        "the system matrix drifted; regenerate with `make rebaseline`",
    );
}

/// 5b: a `test:` anchor citing a real, `#[ignore]`d test must be DANGLING,
/// not clean. `arcs_are_discrete` (`domains/terrain/tests/carve_properties.rs`)
/// is a real `heavy:`-tiered battery the gate never runs — exactly the shape
/// Task 4's author hit by accident while evidencing item 4.2 and had to
/// decline. Before the 5b fix this anchor read as resolved; this test is
/// the "observe it red" half of that fix.
#[test]
fn a_test_anchor_citing_an_ignored_test_is_dangling() {
    let f = audit(
        &corpus_with("present", Some("test:hornvale-terrain::arcs_are_discrete")),
        &facts(),
    );
    let [Finding::Dangling { why, .. }] = f.as_slice() else {
        panic!("expected DANGLING for an #[ignore]d test, got {f:?}");
    };
    assert!(
        why.contains("ignore"),
        "expected the message to name the ignore, got: {why}"
    );
    assert!(
        why.contains("gate actually runs") && why.contains("weaken the verdict"),
        "expected the message to name both repairs (cite a gate-run test, or \
         weaken the verdict), got: {why}"
    );
}

/// The negative control 5b's fix must not break: an ordinary `#[test]` (no
/// `#[ignore]` governing it) must still resolve clean. Without this, "reject
/// every `test:` anchor" would also pass the test above.
#[test]
fn a_test_anchor_citing_an_ordinary_running_test_is_clean() {
    let f = audit(
        &corpus_with(
            "present",
            Some("test:hornvale-kernel::commit_and_query_roundtrip"),
        ),
        &facts(),
    );
    assert!(
        f.is_empty(),
        "commit_and_query_roundtrip is a plain #[test], not #[ignore]d; expected \
         no findings, got {f:?}"
    );
}

/// 5b's closing requirement: re-verify every real `test:` anchor in the
/// shipped corpus still resolves clean under the new ignore-aware check —
/// not just the wolverson-corpus-has-no-anchor-findings test above (which
/// already covers this), but naming it explicitly as the 26-anchor
/// regression 5b's brief asks for, with a message that would name which
/// anchor broke if one ever does.
#[test]
fn every_real_test_anchor_in_the_corpus_still_resolves_clean() {
    let corpus = load_wolverson();
    let test_anchor_items: Vec<&hornvale::systems::Item> = corpus
        .items
        .iter()
        .filter(|i| i.anchor.as_deref().is_some_and(|a| a.starts_with("test:")))
        .collect();
    assert_eq!(
        test_anchor_items.len(),
        26,
        "expected 26 test: anchors in the frozen corpus, got {}",
        test_anchor_items.len()
    );
    let f = audit(&corpus, &facts());
    assert!(
        f.is_empty(),
        "one or more of the corpus's test: anchors no longer resolves clean \
         under the ignore-aware check — this is a finding about that \
         verdict, not about the check:\n{f:#?}"
    );
}

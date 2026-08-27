//! The sentence corpus is frozen: its entry count is asserted, so growing or
//! trimming it is a deliberate act rather than a drift.
//!
//! This file also carries the corpus's **resolver** (Task 9): for each entry,
//! a demand token is `covered` if this campaign implements a construction for
//! it, `not yet` otherwise. Today that means exactly one token —
//! `classify`, the "X is a Y" construction domains/language realizes through
//! the fact-shaped `Clause` (see `clause.rs`'s `classify_*` tests). Every
//! other demand token names a grammatical capability — tense, negation,
//! questions, embedded clauses, coordination, pronoun reference,
//! existentials, witness lists — this campaign explicitly did not build.
//!
//! **The result is honestly near zero, and that is a trap as much as a
//! finding.** A measurement whose only possible answer is zero cannot tell a
//! working resolver from one hardcoded to return "not yet" unconditionally —
//! the same defect class as a guard that has never gone red. So the resolver
//! carries its own positive control: a synthetic entry, built inline and
//! never touching the frozen corpus file, that demands only `classify` and
//! must resolve as covered.

use std::collections::BTreeMap;
use std::path::{Path, PathBuf};

fn repo_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("cli/ always has a parent")
        .to_path_buf()
}

/// The merchant corpus's frozen entry count. Changing this number is the
/// deliberate act; changing the corpus without it is the drift.
const MERCHANT_ENTRIES: usize = 12;

#[test]
fn the_merchant_corpus_is_frozen_at_its_authored_size() {
    let text = std::fs::read_to_string(repo_root().join("sentences/the-merchant.corpus.json"))
        .expect("the merchant corpus is committed");
    let n = text.matches("\"id\":").count();
    assert_eq!(
        n, MERCHANT_ENTRIES,
        "the corpus moved. If that was deliberate, change MERCHANT_ENTRIES in \
         the same commit and say why in the message; a corpus that drifts \
         under a measurement makes every earlier score incomparable."
    );
}

// ---------------------------------------------------------------------
// Task 9: the resolver
// ---------------------------------------------------------------------

/// Demand tokens this campaign implements a construction for. Extend this
/// list only when a token genuinely gains a construction, in the same commit
/// that moves `MERCHANT_COVERED` — the same discipline `MERCHANT_ENTRIES`
/// enforces on the corpus itself.
const IMPLEMENTED_DEMANDS: &[&str] = &["classify"];

/// One corpus entry, as read from `sentences/*.corpus.json`. Only the fields
/// the resolver and the report need; unrecognized JSON fields (`name`,
/// `provenance` at the document root) are ignored by default.
#[derive(serde::Deserialize)]
struct Entry {
    id: String,
    speaker: String,
    text: String,
    demands: Vec<String>,
}

/// The corpus document's shape: only what the resolver reads.
#[derive(serde::Deserialize)]
struct Corpus {
    entries: Vec<Entry>,
}

fn load_merchant_corpus(root: &Path) -> Corpus {
    let text = std::fs::read_to_string(root.join("sentences/the-merchant.corpus.json"))
        .expect("the merchant corpus is committed");
    serde_json::from_str(&text).expect("the merchant corpus parses as the frozen shape")
}

/// Whether a single demand token has a construction in this campaign.
fn demand_covered(demand: &str) -> bool {
    IMPLEMENTED_DEMANDS.contains(&demand)
}

/// An entry is covered only if it makes at least one demand and every demand
/// it makes is covered. A demand-less entry is malformed data, not a free
/// win — it can never appear in the frozen corpus, but the resolver should
/// not silently call it "covered" if one ever slips in.
fn entry_covered(entry: &Entry) -> bool {
    !entry.demands.is_empty() && entry.demands.iter().all(|d| demand_covered(d))
}

/// The frozen result of resolving the-merchant against this campaign's
/// delivered capability, independent of the prose report below — so a stale
/// report cannot hide a coverage number that moved.
const MERCHANT_COVERED: usize = 0;

#[test]
fn merchant_coverage_is_zero_of_twelve() {
    let root = repo_root();
    let corpus = load_merchant_corpus(&root);
    assert_eq!(corpus.entries.len(), MERCHANT_ENTRIES);

    let covered = corpus.entries.iter().filter(|e| entry_covered(e)).count();
    assert_eq!(
        covered, MERCHANT_COVERED,
        "merchant coverage moved from {MERCHANT_COVERED} to {covered}. If a \
         demand token genuinely gained a construction this campaign, update \
         IMPLEMENTED_DEMANDS and MERCHANT_COVERED together and say why in the \
         chronicle; a coverage number that moves silently is worse than a low \
         one."
    );
}

/// THE POSITIVE CONTROL. Built inline, never touching the frozen corpus
/// file: a synthetic entry demanding only `classify` must resolve as
/// covered. Without this, `merchant_coverage_is_zero_of_twelve` alone passes
/// even if `entry_covered`/`demand_covered` were replaced with a function
/// that always returns `false` — the corpus demands nothing this campaign
/// implements, so a hardcoded "not yet" is indistinguishable from a working
/// resolver on that test alone. This test is designed to fail if that
/// happens: comment out `"classify"` from `IMPLEMENTED_DEMANDS` (or make
/// `entry_covered` return `false` unconditionally) and this is the one test
/// in this file that reds, while `merchant_coverage_is_zero_of_twelve` stays
/// green regardless — which is exactly the blind spot the control exists to
/// close.
#[test]
fn a_classify_only_entry_resolves_as_covered() {
    let synthetic = Entry {
        id: "synthetic-positive-control".to_string(),
        speaker: "player".to_string(),
        text: "(synthetic, never in the frozen corpus)".to_string(),
        demands: vec!["classify".to_string()],
    };
    assert!(
        entry_covered(&synthetic),
        "a demand-only-classify entry must resolve as covered; if this fails, \
         the resolver has regressed to reporting \"not yet\" unconditionally"
    );
}

/// The negative complement: an entry demanding something this campaign did
/// not build must resolve as not-yet, so the positive control above is
/// checking a real branch and not a resolver that always says "covered".
#[test]
fn an_unimplemented_demand_resolves_as_not_yet() {
    let synthetic = Entry {
        id: "synthetic-negative-control".to_string(),
        speaker: "player".to_string(),
        text: "(synthetic, never in the frozen corpus)".to_string(),
        demands: vec!["negation".to_string()],
    };
    assert!(!entry_covered(&synthetic));
}

/// An entry is covered only when EVERY demand it makes is covered — one
/// implemented demand alongside one unimplemented demand must not count.
#[test]
fn a_mixed_entry_needs_every_demand_covered() {
    let synthetic = Entry {
        id: "synthetic-mixed".to_string(),
        speaker: "player".to_string(),
        text: "(synthetic, never in the frozen corpus)".to_string(),
        demands: vec!["classify".to_string(), "negation".to_string()],
    };
    assert!(!entry_covered(&synthetic));
}

/// The committed report path. Deliberately NOT declared BY NAME in
/// `docs/generated-paths.txt`: nothing in `scripts/regenerate-artifacts.sh`
/// writes it (the "regenerate-artifacts.sh writes it" criterion that would
/// earn a by-name entry), so it follows the two-day-old
/// `docs/audits/lexicon-inventory.tsv` precedent instead — rewritten under
/// an env var, with the number that matters guarded in Rust
/// (`MERCHANT_COVERED` above) rather than by diffing this file. **It is not
/// drift-check-vacuous, though** — `docs/generated-paths.txt` declares the
/// whole `docs/audits/` directory, and this file is tracked, so `git diff
/// --exit-code -- docs/audits/` already covers it: an edit here that is not
/// also committed reddens `make rebaseline`'s drift check like any other
/// file under that directory.
const REPORT_PATH: &str = "docs/audits/sentence-coverage.md";

/// Rewrite `docs/audits/sentence-coverage.md` under
/// `HV_SENTENCE_REBASELINE=1`; otherwise a no-op (the covered count is
/// guarded by `merchant_coverage_is_zero_of_twelve` regardless of whether
/// this prose file is current).
#[test]
fn sentence_coverage_report() {
    if std::env::var("HV_SENTENCE_REBASELINE").is_err() {
        return;
    }

    let root = repo_root();
    let corpus = load_merchant_corpus(&root);

    let mut tally: BTreeMap<String, usize> = BTreeMap::new();
    for entry in &corpus.entries {
        for demand in &entry.demands {
            *tally.entry(demand.clone()).or_insert(0) += 1;
        }
    }
    for demand in IMPLEMENTED_DEMANDS {
        tally.entry(demand.to_string()).or_insert(0);
    }

    let covered = corpus.entries.iter().filter(|e| entry_covered(e)).count();
    let not_yet = corpus.entries.len() - covered;

    let mut out = String::new();
    out.push_str(
        "# Sentence coverage — the-merchant\n\n\
         Generated by `cli/tests/suite/sentence_corpus.rs` under \
         `HV_SENTENCE_REBASELINE=1`. This file is not declared BY NAME in \
         `docs/generated-paths.txt` (the `docs/audits/lexicon-inventory.tsv` \
         precedent — nothing regenerates it on its own) — but it IS \
         drift-checked, because that file also declares the whole `docs/audits/` \
         directory this report is tracked inside: `git diff --exit-code -- \
         docs/audits/` catches an unregenerated edit here the same as \
         anywhere else under that path. The covered COUNT is separately \
         guarded, in Rust, against `MERCHANT_COVERED`.\n\n\
         A demand is `covered` only if this campaign implements a \
         construction for it. Today that is `classify` alone (the \"X is a \
         Y\" construction) — every other token names a grammatical \
         capability (tense, negation, questions, embedded clauses, \
         coordination, pronoun reference, existentials, witness lists) this \
         campaign explicitly did not build. A **near-zero score is the \
         expected result**, not a defect; the corpus is the program's map, \
         not this campaign's scorecard.\n\n",
    );

    out.push_str("## the-merchant\n\n");
    out.push_str(&format!("- Total entries: {}\n", corpus.entries.len()));
    out.push_str(&format!("- Covered: {covered}\n"));
    out.push_str(&format!("- Not yet: {not_yet}\n\n"));

    out.push_str("### Per-entry\n\n");
    out.push_str("| id | speaker | text | demands | status |\n");
    out.push_str("|---|---|---|---|---|\n");
    for entry in &corpus.entries {
        let status = if entry_covered(entry) {
            "covered"
        } else {
            "not yet"
        };
        out.push_str(&format!(
            "| {} | {} | {} | {} | {status} |\n",
            entry.id,
            entry.speaker,
            entry.text.replace('|', "\\|"),
            entry.demands.join(", "),
        ));
    }
    out.push('\n');

    out.push_str("### Per-demand tally\n\n");
    out.push_str("| demand | entries | implemented? |\n");
    out.push_str("|---|---|---|\n");
    for (demand, n) in &tally {
        let implemented = if demand_covered(demand) { "yes" } else { "no" };
        out.push_str(&format!("| {demand} | {n} | {implemented} |\n"));
    }

    std::fs::write(root.join(REPORT_PATH), out).expect(REPORT_PATH);
}

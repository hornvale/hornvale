//! The Compendium's ratchet and anchor discipline.

use std::path::PathBuf;

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

/// Build a one-item corpus with the given verdict and anchor.
fn corpus_with(verdict: &str, anchor: Option<&str>) -> hornvale::systems::Corpus {
    let anchor_json = match anchor {
        Some(a) => format!(r#", "anchor": "{a}""#),
        None => String::new(),
    };
    let json = format!(
        r#"{{ "corpus": "fixture", "unit": "chapter", "ordered": true,
              "provenance": "fixture", "frozen": "fixture",
              "items": [ {{ "id": "1.1", "kind": "chapter", "title": "T",
                            "verdict": "{verdict}"{anchor_json} }} ] }}"#
    );
    load(&json).expect("fixture parses")
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

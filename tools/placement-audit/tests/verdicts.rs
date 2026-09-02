//! Integration: the verdict engine judges a real twin-group scan bidirectionally
//! — a current tag draws no finding, a wrong fingerprint draws exactly one
//! `Stale`, and an untagged member draws exactly one `Untagged`.

use placement_audit::verdict::FindingKind;
use placement_audit::{detect, walk};
use std::path::PathBuf;

fn fixture_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/verdicts/domains")
}

fn fixture_src(side: &str) -> PathBuf {
    fixture_root().join(side).join("src/lib.rs")
}

/// The mutation-proved rule (Task 10, Step 5): before asserting a verdict
/// driven by fixture text, assert the fixture actually contains the tag
/// under test. Without this, an edit that silently deleted the tag from the
/// fixture would make every assertion below pass for the wrong reason (an
/// `Untagged` finding looks the same whether the tag was deliberately
/// omitted or accidentally lost).
#[test]
fn fixture_targets_are_present_before_any_verdict_is_asserted() {
    let a = std::fs::read_to_string(fixture_src("a")).unwrap();
    let b = std::fs::read_to_string(fixture_src("b")).unwrap();

    assert!(
        a.contains("placement: deliberate"),
        "FIXTURE TARGET NOT FOUND: domains/a's current tag"
    );
    assert!(
        b.contains("placement: deliberate") && b.contains("shape(000000)"),
        "FIXTURE TARGET NOT FOUND: domains/b's wrong-fingerprint tag"
    );
    assert!(
        b.contains("pub enum Rank"),
        "FIXTURE TARGET NOT FOUND: domains/b's untagged Rank"
    );
    // Rank itself must carry no placement: tag at all.
    let rank_doc_start = b.find("/// The other side of `Grade`").unwrap();
    let rank_section = &b[rank_doc_start..];
    assert!(
        !rank_section.contains("placement:"),
        "FIXTURE TARGET NOT FOUND: Rank must be untagged, but a placement: tag was found nearby"
    );
}

#[test]
fn a_current_tag_draws_no_finding() {
    let crates = walk::scan(&[fixture_src("a"), fixture_src("b")]).unwrap();
    let twins = detect::twins(&crates);
    let findings = placement_audit::verdict::judge(&twins);

    let mood_or_grade_current: Vec<_> = findings
        .iter()
        .filter(|f| f.type_name == "Mood" || f.type_name == "Grade")
        .collect();
    assert!(
        mood_or_grade_current.is_empty(),
        "a tagged-and-current member must draw no finding, got: {mood_or_grade_current:?}"
    );
}

#[test]
fn a_wrong_fingerprint_draws_exactly_one_stale_finding() {
    let crates = walk::scan(&[fixture_src("a"), fixture_src("b")]).unwrap();
    let twins = detect::twins(&crates);
    let findings = placement_audit::verdict::judge(&twins);

    let stale: Vec<_> = findings
        .iter()
        .filter(|f| f.kind == FindingKind::Stale)
        .collect();
    assert_eq!(
        stale.len(),
        1,
        "expected exactly one Stale finding: {stale:?}"
    );
    assert_eq!(stale[0].type_name, "Temper");
    assert!(stale[0].message.contains("000000"));
    assert!(stale[0].message.contains("6e79e6"));
}

#[test]
fn an_untagged_member_draws_exactly_one_untagged_finding() {
    let crates = walk::scan(&[fixture_src("a"), fixture_src("b")]).unwrap();
    let twins = detect::twins(&crates);
    let findings = placement_audit::verdict::judge(&twins);

    let untagged: Vec<_> = findings
        .iter()
        .filter(|f| f.kind == FindingKind::Untagged)
        .collect();
    assert_eq!(
        untagged.len(),
        1,
        "expected exactly one Untagged finding: {untagged:?}"
    );
    assert_eq!(untagged[0].type_name, "Rank");
    assert!(untagged[0].message.contains("Grade"));
}

#[test]
fn the_full_scan_finds_exactly_two_twin_groups_and_two_findings() {
    let crates = walk::scan(&[fixture_src("a"), fixture_src("b")]).unwrap();
    let twins = detect::twins(&crates);
    assert_eq!(twins.len(), 2, "expected two twin groups: {twins:?}");

    let findings = placement_audit::verdict::judge(&twins);
    assert_eq!(
        findings.len(),
        2,
        "expected two findings total: {findings:?}"
    );
}

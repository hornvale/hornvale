//! The freshness reader (`hornvale::attest::attest_report`), tested purely
//! over string literals — see `cli/src/attest.rs`'s module doc for the
//! ledger properties these tests hold it to (no exit code; jobs identified
//! by adjacency, not a job id) and for why the job/phase check is two-
//! directional while the author-freshness check is one-directional.
//!
//! # DIRECTION EACH CHECK ENFORCES
//!
//! - `a_job_missing_an_owed_phase_is_reported` — owed-but-absent: a job
//!   whose rows never carry a phase the roster's `stage` rung owes it.
//! - `a_job_carrying_an_unowed_phase_is_reported` — present-but-unowed, the
//!   MIRROR direction. Together these are the both-directions pair The
//!   Attestation's own thesis requires (§1.1): a reader that only reported
//!   absences would be exactly the one-sided check this campaign exists to
//!   correct, committed by the campaign's own instrument.
//! - `an_author_with_no_canonical_row_is_reported_*` — a declared roster
//!   author with no `sluice:<author>` row anywhere in the ledger is
//!   reported, alongside the most recent BARE `<author>` row if one exists
//!   (legacy/by-hand evidence, offered as context, never as a second
//!   canonical source).
//! - `a_none_row_is_never_reported_as_an_absent_author_and_is_its_own_category`
//!   — TASK 5 CHANGED THE AUTHOR VOCABULARY UNDER THIS TASK. A
//!   `none(<reason>)` declared author means NO author is expected; a reader
//!   built to the original assumption (every author is a roster set name)
//!   would look it up in the ledger, find nothing, and report it as an
//!   absent author — 56 false positives on the real committed file. This
//!   test is the one guard against that regression.
//! - `a_well_formed_tree_reports_nothing` — the negative control: an
//!   internally-consistent ledger, roster and declared list produce an
//!   entirely empty [`Report`] (`declared_none_count` included, since this
//!   scenario declares none).

use hornvale::attest::{Report, attest_report, render_report};

const ROSTER: &str = "\
# comment
name\tgate\twhere\tauthors\tcommand
style\tcommit\tlocal\tno\tmake style-run
gate\tstage\tlane\tno\tmake gate-suite-run
artifacts\tstage\tlane\tyes\tbash scripts/regenerate-artifacts.sh
outboard\tstage\tlane\tyes\tbash scripts/lane-outboard.sh
clients\tstage\tlane\tyes\tmake clients-check-run
heavy\tmerge\tlane\tyes\tbash scripts/gate-full-heavy.sh
census\tcampaign\tlane\tyes\tbash scripts/census-run.sh
seam-guard\tcampaign\tlane\tno\tmake seam-guard
integration\tmerge\tlane\tyes\tbash scripts/sluice-run.sh
";

fn row(when: &str, label: &str) -> String {
    format!("| {when} | {label} | 1.0 | 1.0 | 1.0 | 1.0 | 0 | abc1234 |  | lefford | 40 |\n")
}

#[test]
fn a_job_missing_an_owed_phase_is_reported() {
    // A stage job's rows stop after `gate` — no `sluice:clients` row at
    // all, the exact shape found five times in the real committed ledger
    // (2026-08-23 x3, 2026-08-24, 2026-08-28).
    let timings = format!(
        "{}{}{}",
        row("2026-08-01T00:00:00Z", "sluice:artifacts"),
        row("2026-08-01T00:01:00Z", "sluice:outboard"),
        row("2026-08-01T00:02:00Z", "sluice:gate"),
    );
    let declared = "book/src/gallery/\tartifacts\n";
    let report = attest_report(&timings, ROSTER, declared);

    assert_eq!(report.owed_but_absent.len(), 1);
    assert_eq!(
        report.owed_but_absent[0].phases,
        vec!["clients".to_string()]
    );
    assert_eq!(report.owed_but_absent[0].started_at, "2026-08-01T00:00:00Z");
    assert_eq!(report.owed_but_absent[0].ended_at, "2026-08-01T00:02:00Z");
    assert!(
        report.present_but_unowed.is_empty(),
        "a missing phase must not also read as an unowed one"
    );
}

#[test]
fn a_job_carrying_an_unowed_phase_is_reported() {
    // `seam-guard`'s rung is `campaign` — no stage or merge job owns it, so
    // a `sluice:seam-guard` row inside an otherwise-complete job is the
    // mirror defect: present without being owed. (A real historical
    // instance of exactly this shape sits in the committed ledger from
    // before decision 0148 split seam-guard off `outboard`'s stage-rung
    // dispatch — see the module doc's third numbered blind spot for why
    // that is not read as a defect at the time it was written.)
    let timings = format!(
        "{}{}{}{}{}",
        row("2026-08-01T00:00:00Z", "sluice:artifacts"),
        row("2026-08-01T00:01:00Z", "sluice:outboard"),
        row("2026-08-01T00:02:00Z", "sluice:gate"),
        row("2026-08-01T00:03:00Z", "sluice:clients"),
        row("2026-08-01T00:04:00Z", "sluice:seam-guard"),
    );
    let declared = "book/src/gallery/\tartifacts\n";
    let report = attest_report(&timings, ROSTER, declared);

    assert!(
        report.owed_but_absent.is_empty(),
        "every owed phase is present; nothing should be missing"
    );
    assert_eq!(report.present_but_unowed.len(), 1);
    assert_eq!(
        report.present_but_unowed[0].phases,
        vec!["seam-guard".to_string()]
    );
}

#[test]
fn an_author_with_no_canonical_row_is_reported_with_its_last_bare_row() {
    // census's real evidence is a BARE `census` label: census-run.sh calls
    // `timed.sh census -- ...` directly, never through sluice-run.sh's
    // phase loop, so `sluice:census` never appears in the real ledger at
    // all — a structural fact, not a transient gap.
    let timings = format!(
        "{}{}",
        row("2026-08-01T00:00:00Z", "sluice:artifacts"),
        row("2026-08-10T12:00:00Z", "census"),
    );
    let declared = "book/src/laboratory/generated/the-census/\tcensus\n\
                     book/src/laboratory/generated/census-of-the-meeting/\tcensus\n";
    let report = attest_report(&timings, ROSTER, declared);

    assert_eq!(report.absent_authors.len(), 1);
    assert_eq!(report.absent_authors[0].author, "census");
    assert_eq!(report.absent_authors[0].declared_paths, 2);
    assert_eq!(
        report.absent_authors[0].last_seen,
        Some("2026-08-10T12:00:00Z".to_string())
    );
}

#[test]
fn an_author_never_seen_at_all_is_reported_with_no_last_seen() {
    let timings = row("2026-08-01T00:00:00Z", "sluice:artifacts");
    let declared = "book/src/laboratory/generated/the-history/\theavy\n";
    let report = attest_report(&timings, ROSTER, declared);

    assert_eq!(report.absent_authors.len(), 1);
    assert_eq!(report.absent_authors[0].author, "heavy");
    assert_eq!(report.absent_authors[0].last_seen, None);
}

#[test]
fn a_well_formed_tree_reports_nothing() {
    // Deliberately excludes `census`: census-run.sh always attests itself
    // with a BARE `census` row, never a `sluice:census` one (it runs
    // outside sluice-run.sh's phase loop entirely — see
    // `an_author_with_no_canonical_row_is_reported_with_its_last_bare_row`),
    // so a declared `census` author can never satisfy the strict canonical
    // check this reader applies. That is a real, structural fact about the
    // repository, not a gap in this fixture, and this test's job is to show
    // a tree with nothing wrong — not to paper over that fact.
    let timings = format!(
        "{}{}{}{}{}",
        row("2026-08-01T00:00:00Z", "sluice:artifacts"),
        row("2026-08-01T00:01:00Z", "sluice:outboard"),
        row("2026-08-01T00:02:00Z", "sluice:gate"),
        row("2026-08-01T00:03:00Z", "sluice:clients"),
        row("2026-08-01T00:04:00Z", "sluice:heavy"),
    );
    let declared = "book/src/gallery/\tartifacts\n\
                     book/src/laboratory/generated/the-history/\theavy\n";
    let report = attest_report(&timings, ROSTER, declared);

    assert_eq!(report, Report::default());
    assert!(render_report(&report).contains("No anomalies"));
}

#[test]
fn a_none_row_is_never_reported_as_an_absent_author_and_is_its_own_category() {
    // No ledger evidence for a roster author exists at all, and no roster
    // author appears in `declared` either — only a `none(<reason>)` row
    // does. A reader built to the pre-Task-5 assumption ("every author is a
    // roster set name") would look up `none(...)` in the ledger, find
    // nothing, and report 56 false "absent author" findings on the real
    // committed file. It must report zero, and must count the row in its
    // own category instead.
    let timings = row("2026-08-01T00:00:00Z", "sluice:artifacts");
    let declared = "book/src/gallery/vessel.js\tnone(the Casement client build writes this, \
                     not a roster set)\n";
    let report = attest_report(&timings, ROSTER, declared);

    assert!(
        report.absent_authors.is_empty(),
        "a none(...) row must never surface as an absent author: got {:?}",
        report.absent_authors
    );
    assert_eq!(report.declared_none_count, 1);
}

#[test]
#[should_panic(expected = "PARSE ERROR")]
fn a_reasonless_none_row_is_a_parse_error() {
    let declared = "book/src/gallery/vessel.js\tnone()\n";
    attest_report("", ROSTER, declared);
}

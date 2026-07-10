//! PROC-6 — the result-quieting guard (mechanizes ADR 0016).
//!
//! Preregistered calibrations exist to report an *honest* measured value,
//! "never loosened to fit" (ADR 0016). The recurring near-miss — recorded in
//! two campaign retrospectives (Y2-3 and The Words) — is an implementer's
//! first pass `#[ignore]`-ing a row that fails rather than reporting the
//! failing number. `#[ignore]` is never the fix for an uncomfortable but
//! honest result.
//!
//! This guard turns that standing question — "does this diff quiet a result
//! rather than pin it?" — into a mechanical, default-deny check over the lab's
//! calibration test files. An `#[ignore]` is permitted ONLY when its reason
//! string is *sanctioned*: it names a cost (the ~145s census is the one
//! legitimate case today) or cites a decision/ADR. A bare `#[ignore]`, or one
//! whose reason is a result-quieting excuse ("flaky", "failing", "TODO"),
//! matches no sanctioned class and fails the guard.

use std::fs;
use std::path::{Path, PathBuf};

/// A single `#[ignore]` occurrence that is not sanctioned, with a human-
/// readable explanation of why it was rejected.
#[derive(Debug, PartialEq, Eq)]
struct Violation {
    file: String,
    line: usize,
    detail: String,
}

/// Is `reason` a sanctioned justification for ignoring a preregistered
/// calibration? Sanctioned = names a run cost, or cites a decision/ADR.
/// Everything else (including an empty reason) is denied by default.
fn reason_is_sanctioned(reason: &str) -> bool {
    let r = reason.to_lowercase();
    // Cost: the census / an explicitly expensive or slow run. A "~145s" style
    // timing counts (a digit immediately followed by 's').
    let names_cost = ["census", "expensive", "slow", "seconds", "minutes"]
        .iter()
        .any(|needle| r.contains(needle))
        || has_seconds_timing(&r);
    // Provenance: an explicit decision / ADR reference (a 4-digit run covers
    // the numbered decisions, e.g. "0016").
    let cites_decision =
        r.contains("decision") || r.contains("adr") || has_four_consecutive_digits(&r);
    names_cost || cites_decision
}

/// True if `s` contains a digit immediately followed by 's' (a "~145s" timing).
fn has_seconds_timing(s: &str) -> bool {
    let bytes = s.as_bytes();
    bytes
        .windows(2)
        .any(|w| w[0].is_ascii_digit() && w[1] == b's')
}

/// True if `s` contains four consecutive ASCII digits (a decision number).
fn has_four_consecutive_digits(s: &str) -> bool {
    let digits: Vec<bool> = s.bytes().map(|b| b.is_ascii_digit()).collect();
    digits.windows(4).any(|w| w.iter().all(|&d| d))
}

/// Scan one source file's text for unsanctioned `#[ignore]` attributes.
fn violations_in(file: &str, source: &str) -> Vec<Violation> {
    let mut out = Vec::new();
    for (idx, raw) in source.lines().enumerate() {
        let line = raw.trim();
        if !line.starts_with("#[ignore") {
            continue;
        }
        match ignore_reason(line) {
            None => out.push(Violation {
                file: file.to_string(),
                line: idx + 1,
                detail: "bare `#[ignore]` with no reason string; a preregistered \
                         calibration may be ignored only with a sanctioned reason \
                         (a run cost, or a decision cite) — see ADR 0016"
                    .to_string(),
            }),
            Some(reason) if !reason_is_sanctioned(&reason) => out.push(Violation {
                file: file.to_string(),
                line: idx + 1,
                detail: format!(
                    "`#[ignore]` reason {reason:?} is not sanctioned; ignoring a \
                     preregistered calibration is allowed only to skip a run cost \
                     or under a cited decision — `#[ignore]` is not the fix for a \
                     failing honest result (ADR 0016)"
                ),
            }),
            Some(_) => {}
        }
    }
    out
}

/// Extract the reason string from an `#[ignore = "..."]` line, or `None` for a
/// bare `#[ignore]`. Only the first double-quoted span is read.
fn ignore_reason(line: &str) -> Option<String> {
    let after_eq = line.split_once('=')?.1;
    let start = after_eq.find('"')? + 1;
    let rest = &after_eq[start..];
    let end = rest.find('"')?;
    Some(rest[..end].to_string())
}

/// The lab's calibration test files (any `tests/*calibration*.rs`). CWD at test
/// time is the crate root (`windows/lab`), so `tests/` is the relative anchor.
fn calibration_files() -> Vec<PathBuf> {
    let dir = Path::new("tests");
    let mut files: Vec<PathBuf> = fs::read_dir(dir)
        .expect("read tests/ dir")
        .filter_map(|entry| entry.ok().map(|e| e.path()))
        .filter(|p| {
            let name = p.file_name().and_then(|n| n.to_str()).unwrap_or("");
            name.contains("calibration") && name.ends_with(".rs")
        })
        .collect();
    files.sort();
    files
}

#[test]
fn calibration_files_carry_no_unsanctioned_ignores() {
    let files = calibration_files();
    assert!(
        !files.is_empty(),
        "found no tests/*calibration*.rs to guard — the glob or CWD is wrong"
    );
    let mut violations = Vec::new();
    for path in files {
        let source = fs::read_to_string(&path).expect("read calibration source");
        violations.extend(violations_in(&path.display().to_string(), &source));
    }
    assert!(
        violations.is_empty(),
        "unsanctioned `#[ignore]` on a preregistered calibration (ADR 0016):\n{}",
        violations
            .iter()
            .map(|v| format!("  {}:{} — {}", v.file, v.line, v.detail))
            .collect::<Vec<_>>()
            .join("\n")
    );
}

#[test]
fn the_census_cost_ignore_is_sanctioned() {
    // The one legitimate ignore in the tree today: skipping the ~145s census.
    let sample = r#"#[ignore = "runs the full ~145s census; fixtures are drift-checked in CI"]"#;
    assert!(violations_in("sample.rs", sample).is_empty());
}

#[test]
fn a_decision_cite_is_sanctioned() {
    let sample = r#"#[ignore = "superseded by decision 0016; kept for one release"]"#;
    assert!(violations_in("sample.rs", sample).is_empty());
}

#[test]
fn a_bare_ignore_is_rejected() {
    let violations = violations_in("sample.rs", "    #[ignore]");
    assert_eq!(violations.len(), 1);
    assert!(violations[0].detail.contains("bare"));
}

#[test]
fn a_result_quieting_reason_is_rejected() {
    for excuse in [
        r#"#[ignore = "flaky after the refactor"]"#,
        r#"#[ignore = "failing — investigate later"]"#,
        r#"#[ignore = "TODO: re-enable once the number settles"]"#,
    ] {
        let violations = violations_in("sample.rs", excuse);
        assert_eq!(violations.len(), 1, "should reject: {excuse}");
        assert!(violations[0].detail.contains("not sanctioned"));
    }
}

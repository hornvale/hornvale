//! Byte-golden comparison with an explicit accept path (TOOL-20).
//!
//! Every committed golden fixture in the workspace is compared through
//! [`assert_golden`]; a drift is accepted deliberately by re-running the
//! failing test with `REBASELINE=1` (or `make rebaseline-goldens` for all
//! of them) and then reviewing the resulting `git diff` — a snapshot is a
//! migration, and accepting it is a reviewed migration. Dev/test support
//! only: nothing in any sim or generation path may call this module (the
//! `REBASELINE` environment read is deliberate non-determinism that must
//! never reach world output).
//!
//! Frozen *historical* pins (the `pre-<campaign>` fixtures under
//! `cli/tests/fixtures/`) are NOT goldens: their bytes must never track
//! current code, so they are compared directly and have no accept path.

use std::fmt;
use std::fs;
use std::path::Path;

/// A golden mismatch: what differed, where, and how to accept it.
/// type-audit: bare-ok(prose: message)
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GoldenMismatch {
    /// Human-readable report: fixture path, first divergence, accept instruction.
    pub message: String,
}

impl fmt::Display for GoldenMismatch {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl std::error::Error for GoldenMismatch {}

/// What a golden check did.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum GoldenOutcome {
    /// `actual` matched the committed fixture byte-for-byte.
    Match,
    /// Rebaseline mode rewrote a drifted fixture; review the diff.
    Rewritten,
    /// Rebaseline mode created a missing fixture; review before committing.
    Created,
}

/// The testable, env-free core of [`assert_golden`]: compare `actual`
/// against the fixture at `path`; with `rebaseline` set, accept drift by
/// rewriting (or creating) the fixture instead of failing.
/// type-audit: bare-ok(artifact: actual), bare-ok(flag: rebaseline)
pub fn check_golden(
    path: &Path,
    actual: &str,
    rebaseline: bool,
) -> Result<GoldenOutcome, GoldenMismatch> {
    match fs::read_to_string(path) {
        Ok(expected) if expected == actual => Ok(GoldenOutcome::Match),
        Ok(expected) => {
            if rebaseline {
                write_fixture(path, actual)?;
                Ok(GoldenOutcome::Rewritten)
            } else {
                Err(GoldenMismatch {
                    message: mismatch_report(path, &expected, actual),
                })
            }
        }
        Err(_) => {
            if rebaseline {
                write_fixture(path, actual)?;
                Ok(GoldenOutcome::Created)
            } else {
                Err(GoldenMismatch {
                    message: format!(
                        "golden: no fixture at {} — create it deliberately by re-running \
                         this test with REBASELINE=1, then review and commit the new file",
                        path.display()
                    ),
                })
            }
        }
    }
}

/// Assert `actual` matches the committed golden at `path` byte-for-byte.
///
/// On mismatch, panics with the first diverging line, the caller's
/// `context` (the domain-specific "what drifting means here" guidance),
/// and the accept instruction. Setting the `REBASELINE` environment
/// variable (any value but empty or `0`) accepts instead: the fixture is
/// rewritten and the test passes, leaving the diff for review.
/// type-audit: bare-ok(artifact: actual), bare-ok(prose: context)
pub fn assert_golden(path: &Path, actual: &str, context: &str) {
    let rebaseline = std::env::var_os("REBASELINE").is_some_and(|v| !v.is_empty() && v != "0");
    match check_golden(path, actual, rebaseline) {
        Ok(GoldenOutcome::Match) => {}
        Ok(GoldenOutcome::Rewritten) => eprintln!(
            "golden: REBASELINE rewrote {} — review the diff before committing",
            path.display()
        ),
        Ok(GoldenOutcome::Created) => eprintln!(
            "golden: REBASELINE created {} — review it before committing",
            path.display()
        ),
        Err(mismatch) => panic!("{mismatch}\n{context}"),
    }
}

/// Write an accepted fixture, creating parent directories as needed.
fn write_fixture(path: &Path, actual: &str) -> Result<(), GoldenMismatch> {
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent).map_err(|e| GoldenMismatch {
            message: format!("golden: cannot create {}: {e}", parent.display()),
        })?;
    }
    fs::write(path, actual).map_err(|e| GoldenMismatch {
        message: format!("golden: cannot write {}: {e}", path.display()),
    })
}

/// Render a compact mismatch report: the fixture path, the first diverging
/// line pair (windowed to keep multi-megabyte fixtures out of the panic),
/// byte lengths, and the accept instruction.
fn mismatch_report(path: &Path, expected: &str, actual: &str) -> String {
    let mut line = 1usize;
    let mut expected_lines = expected.lines();
    let mut actual_lines = actual.lines();
    let (exp_line, act_line) = loop {
        match (expected_lines.next(), actual_lines.next()) {
            (Some(e), Some(a)) if e == a => line += 1,
            (e, a) => break (e, a),
        }
    };
    // Both sides have a line at the divergence point: anchor the shown
    // window on their first differing character rather than the line's
    // start. A single-line compact-JSON fixture can carry a very long
    // shared prefix (the same schema tag and leading fields) with the real
    // difference buried deep inside — a head truncation of each line would
    // then print two byte-identical prefixes and tell the reader nothing.
    // When only one side has a line (end of file on the other), there is no
    // shared position to anchor on, so each falls back to a plain head
    // truncation.
    let (show_exp, show_act) = match (exp_line, act_line) {
        (Some(e), Some(a)) => windowed_pair(e, a),
        (e, a) => (show_line(e), show_line(a)),
    };
    format!(
        "golden mismatch: {}\nfirst divergence at line {line}:\n  committed: {show_exp}\n  actual:    {show_act}\n({} vs {} bytes) — accept deliberately: re-run with REBASELINE=1 (or `make rebaseline-goldens`), then review the diff before committing",
        path.display(),
        expected.len(),
        actual.len()
    )
}

/// How many characters of shared prefix to keep before a mismatched line
/// pair's first differing character, so the windowed report still shows the
/// reader their bearings (e.g. the JSON key the differing value sits under)
/// instead of starting mid-value.
const WINDOW_CONTEXT_CHARS: usize = 24;

/// The total width (chars) of the window shown per line once anchored on
/// the first difference — the same budget `truncate`'s old head-only cutoff
/// used, just no longer pinned to the line's start.
const WINDOW_TOTAL_CHARS: usize = 120;

/// Render a line that mismatches at a KNOWN shared position: find that
/// position (the first character where `expected` and `actual` disagree, or
/// where the shorter one runs out), then window both lines around it so the
/// differing region itself — not just an identical shared prefix — actually
/// appears in the report.
fn windowed_pair(expected: &str, actual: &str) -> (String, String) {
    let diverges_at = expected
        .chars()
        .zip(actual.chars())
        .position(|(e, a)| e != a)
        .unwrap_or_else(|| expected.chars().count().min(actual.chars().count()));
    let start = diverges_at.saturating_sub(WINDOW_CONTEXT_CHARS);
    (window(expected, start), window(actual, start))
}

/// The `WINDOW_TOTAL_CHARS`-wide slice of `s` starting at char index
/// `start`, with a leading/trailing `…` wherever the window cuts off real
/// text on that side.
fn window(s: &str, start: usize) -> String {
    let total = s.chars().count();
    let end = total.min(start + WINDOW_TOTAL_CHARS);
    let body: String = s.chars().skip(start).take(end - start).collect();
    format!(
        "{}{body}{}",
        if start > 0 { "…" } else { "" },
        if end < total { "…" } else { "" }
    )
}

/// Render a single line (no shared-position window available: the other
/// side is missing entirely, i.e. one file ran out first) as a plain head
/// truncation to 120 chars — fixtures can be one enormous line of JSON.
fn show_line(l: Option<&str>) -> String {
    match l {
        Some(text) if text.chars().count() > WINDOW_TOTAL_CHARS => {
            let head: String = text.chars().take(WINDOW_TOTAL_CHARS).collect();
            format!("{head}…")
        }
        Some(text) => text.to_string(),
        None => "<end of file>".to_string(),
    }
}

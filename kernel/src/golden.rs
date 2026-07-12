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
/// line pair (truncated to keep multi-megabyte fixtures out of the panic),
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
    let show = |l: Option<&str>| match l {
        Some(text) => truncate(text),
        None => "<end of file>".to_string(),
    };
    format!(
        "golden mismatch: {}\nfirst divergence at line {line}:\n  committed: {}\n  actual:    {}\n({} vs {} bytes) — accept deliberately: re-run with REBASELINE=1 (or `make rebaseline-goldens`), then review the diff before committing",
        path.display(),
        show(exp_line),
        show(act_line),
        expected.len(),
        actual.len()
    )
}

/// Truncate a line to 120 chars for the report (fixtures can be one
/// enormous line of JSON).
fn truncate(s: &str) -> String {
    if s.chars().count() > 120 {
        let head: String = s.chars().take(120).collect();
        format!("{head}…")
    } else {
        s.to_string()
    }
}

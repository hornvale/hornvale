//! Plumb: a sweep that asks an authored numeric constant **which axis it
//! varies along**.
//!
//! The motivating instance is `FATIGUE_RISE = 0.3` — one sleep-debt rate for
//! every species in every world, on the wrong clock. It was found by a human in
//! a design conversation, not by any tool, and nothing was looking for the next
//! one.
//!
//! **The tool does not guess.** Nothing in a constant's syntax distinguishes a
//! physics term from a mis-generalised creature trait, so the output is
//! *coverage*, not *detection*: every numeric constant in the audited scope
//! carrying a declared rung, so that an inherited fixedness becomes a chosen
//! one. That is `tools/type-audit`'s philosophy exactly, and the two tools
//! share a tag grammar deliberately.
//!
//! The verdict vocabulary is a **ladder** ([`tag::Rung`]) — `MAP-one-kind-model`'s
//! three additions seen from the numeric side — so tagging a constant
//! `per-people` today registers it as a consumer for the kind-edge campaign
//! that does not exist yet.
//!
//! ## Re-running a pasted mutation recipe in this crate
//!
//! Every `MUTATION THIS MUST FAIL AGAINST` block below quotes the exact string
//! `scripts/mutate.py` was given. **Pasting the red made several of those
//! strings non-unique**, because the doc comment now contains the anchor
//! verbatim and `mutate.py` refuses with `TARGET NOT UNIQUE (2 occurrences)` —
//! by design, since a pattern matching two sites mutates two things at once.
//! Widen the anchor with a neighbouring line to re-run one. This is a general
//! property of pasted reds in this repository, not a defect of these
//! particular recipes.
#![warn(missing_docs)]

pub mod args;
pub mod audit;
pub mod report;
pub mod tag;
pub mod walk;

use args::{Command, parse_args};

/// The banner `check` prints instead of failing.
///
/// **`check` is deliberately report-only in this campaign's Task 3.** A
/// default-deny gate landing before its backlog is seeded reddens on the entire
/// population on day one and teaches everyone to ignore it — the reasoning
/// `tools/seam-guard` states for its own three-valued design, and the reason
/// `type-audit` has a `pending(wave-N)` class at all. Task 4 seeds the backlog
/// and closes the ratchet, and the exit code below is the single line it flips.
pub const REPORT_ONLY_BANNER: &str = "plumb check is REPORT-ONLY: it never fails yet. The ratchet closes once the \
     backlog is seeded — until then these lines are a worklist, not a gate.";

/// Run the tool with `argv` (without the program name); returns the process
/// exit code (0 = success or report-only findings, 2 = usage or scan error).
pub fn run(args: &[String]) -> i32 {
    match parse_args(args) {
        Ok(Command::Check { paths }) => match walk::scan(&paths) {
            Ok(scan) => {
                let found = audit::findings(&scan);
                for f in &found {
                    println!("{}", f.render());
                }
                let malformed = found.iter().filter(|f| f.malformed).count();
                eprintln!(
                    "{} quantity const(s) swept; {} undeclared, {} malformed",
                    scan.consts.len(),
                    found.len() - malformed,
                    malformed
                );
                eprintln!("{REPORT_ONLY_BANNER}");
                0
            }
            Err(e) => {
                eprintln!("scan error: {e}");
                2
            }
        },
        Ok(Command::Report { paths }) => match walk::scan(&paths) {
            Ok(scan) => {
                print!("{}", report::render_report(&scan));
                0
            }
            Err(e) => {
                eprintln!("scan error: {e}");
                2
            }
        },
        Err(msg) => {
            eprintln!("{msg}");
            2
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn fixture_dir(tag: &str, src: &str) -> std::path::PathBuf {
        let dir = std::env::temp_dir().join(format!("plumb_run_{tag}"));
        let _ = std::fs::remove_dir_all(&dir);
        std::fs::create_dir_all(dir.join("domains/probe/src")).unwrap();
        std::fs::write(dir.join("domains/probe/src/lib.rs"), src).unwrap();
        dir
    }

    /// **This test is the ratchet's off-switch, and Task 4 must delete it.**
    ///
    /// `check` returns 0 even against a tree full of undeclared constants,
    /// because a gate that goes red on day one is a gate everyone learns to
    /// ignore. The property is asserted rather than merely intended, so that
    /// flipping it later is a deliberate edit to a named test instead of a
    /// silent change of behaviour.
    ///
    /// MUTATION THIS MUST FAIL AGAINST (confirmed 2026-09-02, The Plumb Task 3): close the ratchet
    /// early — `0` -> `if found.is_empty() { 0 } else { 1 }` in the `Check` arm.
    ///
    /// ```text
    /// assertion `left == right` failed: check must not fail in this campaign's Task 3
    ///   left: 1
    ///  right: 0
    /// ```
    #[test]
    fn check_is_report_only_and_returns_zero_on_undeclared_constants() {
        let dir = fixture_dir(
            "report_only",
            "/// Undeclared.\nconst A: f64 = 1.0;\n/// Broken.\n/// plumb: universal\nconst B: f64 = 2.0;\n",
        );
        let code = run(&[
            "check".to_string(),
            dir.join("domains").display().to_string(),
        ]);
        std::fs::remove_dir_all(&dir).unwrap();
        assert_eq!(code, 0, "check must not fail in this campaign's Task 3");
    }

    /// A usage error and a scan error still fail, so "report-only" is scoped to
    /// the audit's verdicts and not to the tool's own correctness.
    #[test]
    fn usage_and_scan_errors_still_fail() {
        assert_eq!(run(&["frobnicate".to_string()]), 2);
        assert_eq!(run(&[]), 2);

        let dir = fixture_dir("unparseable", "this is not rust;;;\n");
        let code = run(&[
            "check".to_string(),
            dir.join("domains").display().to_string(),
        ]);
        std::fs::remove_dir_all(&dir).unwrap();
        assert_eq!(code, 2, "an unparseable file must not be swept silently");
    }

    #[test]
    fn report_succeeds_on_an_explicit_root() {
        let dir = fixture_dir(
            "report",
            "/// Ticks.\n/// plumb: universal(the lattice is a kernel constant)\nconst T: i64 = 100_000;\n",
        );
        let code = run(&[
            "report".to_string(),
            dir.join("domains").display().to_string(),
        ]);
        std::fs::remove_dir_all(&dir).unwrap();
        assert_eq!(code, 0);
    }
}

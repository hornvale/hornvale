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

/// Run the tool with `argv` (without the program name); returns the process
/// exit code (0 = every constant carries a well-formed rung, 1 = at least one
/// undeclared or malformed constant, 2 = usage or scan error).
///
/// **`check` closes the ratchet (The Plumb, Task 4).** Task 3 shipped it
/// report-only, deliberately: a default-deny gate landing before its backlog
/// is seeded reddens on the entire population on day one and teaches everyone
/// to ignore it — the reasoning `tools/seam-guard` states for its own
/// three-valued design, and the reason `type-audit` has a `pending(wave-N)`
/// class at all. Task 4 seeded the 681-constant backlog with `pending(wave-1)`
/// tags and judged the contested middle by hand, so the gate can now fail on
/// novelty (an untagged constant) rather than on the whole population.
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
                if found.is_empty() { 0 } else { 1 }
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

    /// **The ratchet is closed (The Plumb, Task 4).** `check` now fails on a
    /// tree carrying even one undeclared or malformed constant — the
    /// property Task 3's `check_is_report_only_and_returns_zero_on_undeclared_
    /// constants` asserted was true only until the backlog was seeded, and
    /// this test replaces it with the opposite assertion so the flip is a
    /// named, deliberate edit rather than a silent behaviour change.
    ///
    /// MUTATION THIS MUST FAIL AGAINST: reopen the ratchet — replace
    /// `if found.is_empty() { 0 } else { 1 }` with a bare `0` in the `Check`
    /// arm.
    ///
    /// ```text
    /// assertion `left == right` failed: an undeclared constant must fail check
    ///   left: 0
    ///  right: 1
    /// ```
    #[test]
    fn check_fails_on_an_undeclared_constant() {
        let dir = fixture_dir("undeclared_fails", "/// Undeclared.\nconst A: f64 = 1.0;\n");
        let code = run(&[
            "check".to_string(),
            dir.join("domains").display().to_string(),
        ]);
        std::fs::remove_dir_all(&dir).unwrap();
        assert_eq!(code, 1, "an undeclared constant must fail check");
    }

    /// A malformed tag fails `check` exactly like an undeclared one — both are
    /// findings, and neither is report-only any more.
    ///
    /// MUTATION THIS MUST FAIL AGAINST: special-case malformed findings out of
    /// the failure path — e.g. `if found.iter().all(|f| f.malformed) { 0 }
    /// else { ... }`.
    ///
    /// ```text
    /// assertion `left == right` failed: a malformed tag must fail check
    ///   left: 0
    ///  right: 1
    /// ```
    #[test]
    fn check_fails_on_a_malformed_tag() {
        let dir = fixture_dir(
            "malformed_fails",
            "/// Broken.\n/// plumb: universal\nconst B: f64 = 2.0;\n",
        );
        let code = run(&[
            "check".to_string(),
            dir.join("domains").display().to_string(),
        ]);
        std::fs::remove_dir_all(&dir).unwrap();
        assert_eq!(code, 1, "a malformed tag must fail check");
    }

    /// A fully-declared tree passes: the gate fails on FINDINGS, not on the
    /// mere existence of a scanned constant.
    #[test]
    fn check_passes_on_a_fully_declared_tree() {
        let dir = fixture_dir(
            "fully_declared",
            "/// Ticks.\n/// plumb: universal(the lattice is a kernel constant)\nconst T: i64 = 100_000;\n",
        );
        let code = run(&[
            "check".to_string(),
            dir.join("domains").display().to_string(),
        ]);
        std::fs::remove_dir_all(&dir).unwrap();
        assert_eq!(code, 0, "a fully-declared tree must pass check");
    }

    /// A usage error and a scan error still fail with `2`, distinct from the
    /// findings failure code `1` — a caller can tell "the tool could not run"
    /// from "the tool ran and found something".
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

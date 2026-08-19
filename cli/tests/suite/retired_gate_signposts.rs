//! The retired gate signposts (`gate`, `ci`, `gate-fast`, `gate-full`,
//! `gate-campaign`) must actually refuse.
//!
//! Nothing else in this repo checked that before this test: a future edit
//! could quietly turn one of these Makefile recipes into a no-op that exits
//! 0, and a caller chaining it (`make gate && deploy`) would proceed as
//! though a gate had actually run — a signpost that exits 0 is worse than no
//! signpost at all. Each target's entire job is to print a redirect message
//! and fail; this pins the "fail" half, which nothing mechanical enforced.
//!
//! **The message must land on STDERR, not stdout.** This is not cosmetic:
//! `gate-campaign`'s first draft routed every `echo` with no redirect at all
//! (the sibling `gate ci gate-fast gate-full:` rule sends every line to
//! `>&2`), so a caller capturing or piping only stderr — a normal shape for
//! error-message tooling — saw nothing, even though the exit code was
//! already correctly non-zero. Checking `output.status.success()` alone did
//! not catch that regression; this test now checks the streams separately so
//! the same defect cannot recur silently.

use std::path::{Path, PathBuf};
use std::process::Command;

fn repo_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("cli/ has a parent")
        .to_path_buf()
}

/// Every retired top-level gate target exits non-zero and prints its
/// redirect message to stderr specifically — never silently, and never only
/// to stdout. Collects every target's failures before asserting, so a red
/// `gate` does not hide whether the other four also broke.
#[test]
fn retired_gate_targets_refuse() {
    let mut failures = Vec::new();
    for target in ["gate", "ci", "gate-fast", "gate-full", "gate-campaign"] {
        let output = Command::new("make")
            .arg(target)
            .current_dir(repo_root())
            .output()
            .unwrap_or_else(|e| panic!("failed to run `make {target}`: {e}"));
        let stdout = String::from_utf8_lossy(&output.stdout).into_owned();
        let stderr = String::from_utf8_lossy(&output.stderr).into_owned();

        if output.status.success() {
            failures.push(format!(
                "`make {target}` exited 0 — a retired signpost that succeeds \
                 is worse than no signpost, because a caller chaining it \
                 would proceed as though the gate it names actually ran"
            ));
        }
        if !stderr.contains("no longer") {
            failures.push(format!(
                "`make {target}`'s refusal did not reach STDERR (it must say \
                 \"no longer\" there, matching its sibling signposts' `>&2`); \
                 stdout was:\n{stdout}\nstderr was:\n{stderr}"
            ));
        }
        if stdout.contains("no longer") {
            failures.push(format!(
                "`make {target}` wrote its refusal to STDOUT — a caller \
                 capturing only stderr (a normal shape for error-message \
                 tooling) would see nothing, exactly the regression this \
                 test exists to catch; stdout was:\n{stdout}"
            ));
        }
    }
    assert!(
        failures.is_empty(),
        "{} retired-signpost failure(s):\n  {}",
        failures.len(),
        failures.join("\n  ")
    );
}

//! The retired gate signposts (`gate`, `ci`, `gate-fast`, `gate-full`,
//! `gate-campaign`) must actually refuse.
//!
//! Nothing else in this repo checked that before this test: a future edit
//! could quietly turn one of these Makefile recipes into a no-op that exits
//! 0, and a caller chaining it (`make gate && deploy`) would proceed as
//! though a gate had actually run — a signpost that exits 0 is worse than no
//! signpost at all. Each target's entire job is to print a redirect message
//! and fail; this pins the "fail" half, which nothing mechanical enforced.

use std::path::{Path, PathBuf};
use std::process::Command;

fn repo_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("cli/ has a parent")
        .to_path_buf()
}

/// Every retired top-level gate target exits non-zero and says so, rather
/// than silently succeeding.
#[test]
fn retired_gate_targets_refuse() {
    for target in ["gate", "ci", "gate-fast", "gate-full", "gate-campaign"] {
        let output = Command::new("make")
            .arg(target)
            .current_dir(repo_root())
            .output()
            .unwrap_or_else(|e| panic!("failed to run `make {target}`: {e}"));
        assert!(
            !output.status.success(),
            "`make {target}` exited 0 — a retired signpost that succeeds is \
             worse than no signpost, because a caller chaining it would \
             proceed as though the gate it names actually ran"
        );
        let combined = format!(
            "{}{}",
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        );
        assert!(
            combined.contains("no longer"),
            "`make {target}`'s refusal no longer says it no longer runs \
             anything (the message changed shape without this test being \
             updated); got:\n{combined}"
        );
    }
}

//! The test-binary ratchet: a new top-level `tests/*.rs` is a new
//! compilation unit, and compilation units are what `gate-commit` actually
//! costs.
//!
//! **Measured, 2026-08-19.** `gate-commit` spends ~0.75% of itself running
//! tests — 6.2 s of an 833 s wall, for 3,070 tests. The cost is the number
//! of *compilation units*: `cargo clippy --workspace` over lib targets costs
//! real 21.3 s / sys 4.6 s, and adding `--all-targets` — which turns every
//! integration-test FILE into its own crate — costs real 62.8 s / sys
//! 162.1 s. The 260 test targets alone were **+157.5 s of kernel time**,
//! roughly 0.6 s each, just to exist.
//!
//! The Crucible consolidated them behind one `tests/suite.rs` per crate and
//! measured −33% wall, −28% sys on the pilot crate. **Nothing stopped the
//! problem re-accreting**: a new `tests/foo.rs` compiles, passes review, and
//! silently adds another unit. 13 had already crept back by the time this
//! guard was written.
//!
//! So this is a **ratchet**, not a wall — the same three-valued shape
//! `tropes check`, the timings baseline and type-audit's `waiver(...)` use.
//! A guard that failed on the mere existence of the 13 would be red on day
//! one and trained-away by day two.
//!
//! **To add a file here deliberately**, append it to
//! `cli/tests/fixtures/top-level-test-binaries.txt` in the same commit and
//! say in the message why the crate's `suite.rs` could not host it. **To
//! remove one**, consolidate it into that crate's `suite/` directory and
//! delete its line — the guard checks both directions, so a stale entry is
//! an error too and the list cannot rot.

use std::collections::BTreeSet;
use std::path::{Path, PathBuf};

/// The frozen roster. Append-with-justification; never edit in bulk.
const FROZEN: &str = include_str!("../fixtures/top-level-test-binaries.txt");

/// Every workspace crate directory, relative to the repo root.
fn crate_dirs(root: &Path) -> Vec<PathBuf> {
    let mut out = vec![root.join("kernel"), root.join("cli")];
    for parent in ["domains", "windows"] {
        let Ok(entries) = std::fs::read_dir(root.join(parent)) else {
            continue;
        };
        let mut kids: Vec<PathBuf> = entries
            .filter_map(Result::ok)
            .map(|e| e.path())
            .filter(|p| p.is_dir())
            .collect();
        kids.sort();
        out.extend(kids);
    }
    out
}

/// Top-level `tests/*.rs` files, excluding each crate's consolidated
/// `suite.rs`, as repo-root-relative slash-separated paths.
fn top_level_test_files(root: &Path) -> BTreeSet<String> {
    let mut found = BTreeSet::new();
    for dir in crate_dirs(root) {
        let Ok(entries) = std::fs::read_dir(dir.join("tests")) else {
            continue;
        };
        for entry in entries.filter_map(Result::ok) {
            let path = entry.path();
            if !path.is_file() || path.extension().is_none_or(|e| e != "rs") {
                continue;
            }
            if path.file_name().is_some_and(|n| n == "suite.rs") {
                continue;
            }
            let rel = path
                .strip_prefix(root)
                .expect("scanned path is under the repo root");
            found.insert(rel.to_string_lossy().replace('\\', "/"));
        }
    }
    found
}

#[test]
fn no_new_top_level_test_binary_appears() {
    let root = Path::new(concat!(env!("CARGO_MANIFEST_DIR"), "/..")); // cli/ -> repo root
    let frozen: BTreeSet<String> = FROZEN
        .lines()
        .map(str::trim)
        .filter(|l| !l.is_empty() && !l.starts_with('#'))
        .map(str::to_string)
        .collect();
    let found = top_level_test_files(root);

    let added: Vec<&String> = found.difference(&frozen).collect();
    assert!(
        added.is_empty(),
        "new top-level integration-test file(s), each a new compilation unit \
         costing ~0.6 s of kernel time on EVERY gate-commit:\n{}\n\nPut the \
         tests in the crate's `tests/suite/` directory and declare them in \
         `tests/suite.rs` with `#[path = \"suite/<stem>.rs\"] mod <stem>;` \
         (a plain `mod x;` will NOT resolve into `suite/`). If a separate \
         binary is genuinely required — a distinct `#![...]` crate attribute, \
         a harness override — append the path to \
         `cli/tests/fixtures/top-level-test-binaries.txt` in the same commit \
         and say why in the message.",
        added
            .iter()
            .map(|p| format!("  {p}"))
            .collect::<Vec<_>>()
            .join("\n")
    );

    let stale: Vec<&String> = frozen.difference(&found).collect();
    assert!(
        stale.is_empty(),
        "the frozen roster names file(s) that no longer exist:\n{}\n\nIf they \
         were consolidated into a crate's `suite/`, delete their lines — this \
         direction is checked so the list cannot rot into a permission slip \
         nobody re-reads.",
        stale
            .iter()
            .map(|p| format!("  {p}"))
            .collect::<Vec<_>>()
            .join("\n")
    );
}

//! Guards the drift-check path list (The Sexton, Task 1).
//!
//! DIRECTION THIS CHECK ENFORCES: every path DECLARED in
//! `docs/generated-paths.txt` has at least one file tracked by git. It is
//! structurally blind to the opposite direction — a generated directory that
//! nobody declared is invisible to it, and always will be.

use std::path::{Path, PathBuf};
use std::process::Command;

/// The repository root, resolved from this crate's manifest directory.
fn repo_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("cli/ always has a parent")
        .to_path_buf()
}

/// The declared paths, with `#` comments and blank lines stripped.
fn declared_paths() -> Vec<String> {
    let text = std::fs::read_to_string(repo_root().join("docs/generated-paths.txt"))
        .expect("docs/generated-paths.txt must exist");
    text.lines()
        .map(str::trim)
        .filter(|l| !l.is_empty() && !l.starts_with('#'))
        .map(str::to_string)
        .collect()
}

#[test]
fn every_declared_generated_path_is_tracked() {
    let root = repo_root();
    let mut vacuous: Vec<String> = Vec::new();

    for path in declared_paths() {
        let out = Command::new("git")
            .arg("-C")
            .arg(&root)
            .args(["ls-files", "--", &path])
            .output()
            .expect("git ls-files must run");
        if out.stdout.is_empty() {
            vacuous.push(path);
        }
    }

    assert!(
        vacuous.is_empty(),
        "these declared generated paths have NO tracked files, so \
         `git diff --exit-code` over them is vacuous and can never fail — \
         `git add` their contents in the commit that introduces them:\n  {}",
        vacuous.join("\n  ")
    );
}

#[test]
fn the_declared_list_is_not_empty() {
    assert!(
        !declared_paths().is_empty(),
        "docs/generated-paths.txt declared nothing — an empty list makes \
         `every_declared_generated_path_is_tracked` vacuously green, which is \
         the exact defect that test exists to prevent, one level up"
    );
}

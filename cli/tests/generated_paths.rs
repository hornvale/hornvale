//! Guards the drift-check path list (The Sexton, Task 1).
//!
//! DIRECTION THIS CHECK ENFORCES: every path DECLARED in
//! `docs/generated-paths.txt` has at least one file tracked by git. It is
//! structurally blind to the opposite direction — a generated directory that
//! nobody declared is invisible to it, and always will be.
//!
//! SECOND DIRECTION, ADDED AFTER REVIEW: no `CLAUDE.md` may carry its own copy
//! of the list. Declaring a file "the single source of truth" does not make it
//! one while four directory guides still enumerate the paths inline — a
//! campaign that follows the prose adds a directory there, never touches the
//! declared list, and the tracked-ness check above silently never covers it.
//! That was not hypothetical: `domains/terrain/CLAUDE.md` had already drifted,
//! restating six paths after `clients/game/core/tests/fixtures/` became the
//! seventh.

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

/// Every tracked `CLAUDE.md`, as (path, contents).
fn claude_md_files() -> Vec<(String, String)> {
    let root = repo_root();
    let out = Command::new("git")
        .arg("-C")
        .arg(&root)
        .args(["ls-files", "--", "CLAUDE.md", "*/CLAUDE.md"])
        .output()
        .expect("git ls-files must run");
    String::from_utf8_lossy(&out.stdout)
        .lines()
        .map(str::trim)
        .filter(|l| !l.is_empty())
        .map(|rel| {
            let text = std::fs::read_to_string(root.join(rel))
                .unwrap_or_else(|e| panic!("reading {rel}: {e}"));
            (rel.to_string(), text)
        })
        .collect()
}

/// No `CLAUDE.md` may restate the declared path list.
///
/// WHAT THIS ENFORCES, EXACTLY: no single line of any `CLAUDE.md` names two or
/// more distinct declared paths. That is the shape a restated list takes — a
/// `git diff --exit-code`/`git diff` line enumerating them — and it is the
/// shape that drifts, because updating `docs/generated-paths.txt` leaves it
/// untouched and nothing complains.
///
/// WHAT IT IS BLIND TO, said plainly:
///   - A list broken across several lines, one path per line. Prose that
///     explains *why* one entry is in the list is legitimate and common, so a
///     one-path-per-line rule would have to distinguish explanation from
///     enumeration, which no textual test can do.
///   - Any restatement outside `CLAUDE.md` — a spec, a retrospective, a doc
///     comment. Those are dated records of what was true when written; the
///     directory guides are the ones read as current instruction, which is why
///     only they are held to this.
///   - Whether the *declared* list is itself complete. Nothing can check that
///     (see this file's header).
///
/// Threshold two, not one, deliberately: a guide naming a single generated
/// directory while explaining it is exactly what these files are for.
#[test]
fn no_claude_md_restates_the_declared_path_list() {
    // Match on the slash-stripped stem so `book/src/gallery` and
    // `book/src/gallery/` both count — the already-drifted copy this test was
    // written for used the unslashed form.
    let stems: Vec<String> = declared_paths()
        .iter()
        .map(|p| p.trim_end_matches('/').to_string())
        .collect();

    let mut offenders: Vec<String> = Vec::new();
    for (path, text) in claude_md_files() {
        for (n, line) in text.lines().enumerate() {
            let named = stems.iter().filter(|s| line.contains(s.as_str())).count();
            if named >= 2 {
                offenders.push(format!("{}:{} names {} declared paths", path, n + 1, named));
            }
        }
    }

    assert!(
        offenders.is_empty(),
        "these lines restate the drift-check path list, which \
         `docs/generated-paths.txt` is the single source of truth for. A second \
         copy drifts the moment a generated directory is added: the prose gets \
         the new path, the declared list does not, and \
         `every_declared_generated_path_is_tracked` never covers it. Replace the \
         inline list with a pointer at docs/generated-paths.txt:\n  {}",
        offenders.join("\n  ")
    );
}

/// The root guide must point at the declared list by name.
///
/// The companion to the test above: deleting an inline list satisfies that one
/// while leaving a reader with no idea where the real list lives, which is how
/// the inline copy got written in the first place.
#[test]
fn the_root_guide_names_the_declared_path_list() {
    let (_, root_guide) = claude_md_files()
        .into_iter()
        .find(|(p, _)| p == "CLAUDE.md")
        .expect("the repository root carries a CLAUDE.md");
    assert!(
        root_guide.contains("docs/generated-paths.txt"),
        "CLAUDE.md's generated-artifact freshness block must name \
         docs/generated-paths.txt as the authoritative path list — otherwise a \
         reader who needs to add a generated directory has nowhere to be sent, \
         and writes a fresh inline list"
    );
}

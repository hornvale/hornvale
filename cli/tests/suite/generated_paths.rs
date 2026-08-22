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
            // The closure parameter is `stem`, never the single letter after
            // `r`. `cli/tests/claim_shape.rs` reads that letter as a seed
            // binding (`seed_shaped`), so the first version of this test read
            // as an untagged seed loop and turned the gate red — and then the
            // comment written to explain THAT read as one too, because
            // `has_seed_closure` is a raw substring scan over the body text
            // and does not skip comments. Hence the circumlocution here: this
            // note cannot spell the offending token it is about.
            let named = stems
                .iter()
                .filter(|stem| line.contains(stem.as_str()))
                .count();
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

/// No generated artifact is routed through a **regenerating** merge driver.
///
/// The ratchet for decision 0160. `.gitattributes` once routed six
/// fully-re-derived documents through `merge=hv-regenerate`, a driver that
/// discarded both sides' text and reran the generator. The premise was sound
/// and the implementation could not deliver it: git invokes a merge driver
/// **before** the merge product exists on disk — `ort` has not yet written the
/// incoming files — so regeneration at that moment measures the wrong tree.
///
/// The failure is not intermittent. Git calls a driver only when **both** sides
/// changed the path, which is exactly when the two sources differ, which is
/// exactly when the working tree is not the merge product. Every invocation
/// emitted ours' answer and silently dropped theirs; the merges where these
/// files came out correct are merges where the driver never ran. Measured on
/// 2026-08-20: the same two commits merged in opposite directions produced two
/// clean, conflict-free, *different* results — one losing six primitives, the
/// other losing an entire crate's row from a default-deny audit.
///
/// **Direction this check enforces:** it fails on the *reappearance* of a
/// regenerating driver attribute. It says nothing about whether the remaining
/// `merge=union` (Tier A) entries are correct — those are a different
/// mechanism with a different failure mode (union is wrong for a *rewrite*,
/// right for an append) and are deliberately out of scope here.
#[test]
fn no_generated_artifact_is_routed_through_a_regenerating_merge_driver() {
    let attributes = std::fs::read_to_string(repo_root().join(".gitattributes"))
        .expect(".gitattributes is tracked at the repository root");

    let offenders: Vec<&str> = attributes
        .lines()
        .filter(|line| !line.trim_start().starts_with('#'))
        .filter(|line| line.contains("merge=hv-regenerate"))
        .collect();

    assert!(
        offenders.is_empty(),
        "a `merge=hv-regenerate` attribute is back in .gitattributes. A merge \
         driver cannot regenerate a merge product, because at the moment git \
         invokes it the merge product does not exist on disk — it will emit \
         whichever side happens to be checked out and silently drop the other. \
         Read docs/decisions/0160-a-generated-artifact-cannot-be-merged-by-\
         regenerating-it.md before re-adding one; the mechanism that works is \
         scripts/hooks/post-merge's advisory to run `make rebaseline`:\n  {}",
        offenders.join("\n  ")
    );
}

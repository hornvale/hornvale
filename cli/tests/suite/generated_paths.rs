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
//!
//! THIRD DIRECTION: every declared path is one its declared AUTHOR actually
//! WRITES. This is the criterion the list exists on, and it is what keeps a
//! BYTE-GOLDEN out of it — see the header of `docs/generated-paths.txt`. A
//! golden is an assertion, not a regenerated input; declaring one would let
//! the chamber's artifacts phase commit accepted drift and silently rebaseline
//! a determinism guarantee. Two sessions proposed exactly that widening on
//! 2026-08-23 before anyone checked which of the two kinds each directory was.
//! Since The Attestation, "the script" is no longer singular: the declared
//! author (`artifacts`, `census`, or `heavy` — the roster's own set names,
//! `scripts/lane-sets.tsv`) picks WHICH source is checked, via
//! `source_for_author` below.
//!
//! WHAT THIS THIRD CHECK CANNOT SEE: it matches the declared path as a literal
//! substring of the author's source file. A future artifact written through a
//! shell variable (`"$out_dir/foo.md"`) would read as undeclared-by-the-author
//! and redden this test even though regeneration does produce it. That is a
//! false positive, so it fails safe — but the fix is to write the path
//! literally in the source, not to weaken this test.
//!
//! FOURTH DIRECTION, FOUND BY THE ATTESTATION: the third check above is
//! satisfied at DIRECTORY granularity while most files beneath stay unwritten
//! by the declared author — see `every_declared_generated_path_is_written_by_
//! its_author`'s own doc comment for the measurement. That is a FALSE
//! NEGATIVE, and it fails *unsafe*, unlike the false positive above: nothing
//! in this file catches a row whose single author value is wrong for most of
//! what it declares.
//!
//! A DECLARED PATH'S AUTHOR MUST ALSO BE ONE THE ROSTER NAMES (Step 2 below,
//! `every_declared_path_names_a_known_author`) — `artifacts`, `census`, or
//! `heavy`, the set names `scripts/lane-sets.tsv` already uses for the same
//! invocations. Declared-implies-attributed; blind to a generated path
//! nobody declared, the same blindness the first direction documents.

use std::path::{Path, PathBuf};
use std::process::Command;

/// The repository root, resolved from this crate's manifest directory.
fn repo_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("cli/ always has a parent")
        .to_path_buf()
}

/// The declared paths, as `(path, author)`, with `#` comments and blank
/// lines stripped. Each row is `path<TAB>author` (docs/generated-paths.txt's
/// own header names the two columns); a row with no tab is a malformed
/// declaration and panics loudly rather than silently losing the author.
fn declared() -> Vec<(String, String)> {
    let text = std::fs::read_to_string(repo_root().join("docs/generated-paths.txt"))
        .expect("docs/generated-paths.txt must exist");
    text.lines()
        .map(str::trim)
        .filter(|l| !l.is_empty() && !l.starts_with('#'))
        .map(|l| {
            let mut fields = l.splitn(2, '\t');
            let path = fields
                .next()
                .unwrap_or_else(|| panic!("empty declared-path line"));
            let author = fields.next().unwrap_or_else(|| {
                panic!(
                    "docs/generated-paths.txt row {path:?} has no <TAB>author column — \
                     every declared path must name its author"
                )
            });
            (path.to_string(), author.to_string())
        })
        .collect()
}

#[test]
fn every_declared_generated_path_is_tracked() {
    let root = repo_root();
    let mut vacuous: Vec<String> = Vec::new();

    for (path, _author) in declared() {
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
        !declared().is_empty(),
        "docs/generated-paths.txt declared nothing — an empty list makes \
         `every_declared_generated_path_is_tracked` vacuously green, which is \
         the exact defect that test exists to prevent, one level up"
    );
}

/// Every declared path names an author, and the author is one we know.
///
/// # Direction this check enforces
///
/// Declared-implies-attributed. Blind to a generated path nobody declared —
/// the same blindness `every_declared_generated_path_is_tracked` documents,
/// and for the same reason: nothing enumerates this repository's generated
/// output independently of this file.
#[test]
fn every_declared_path_names_a_known_author() {
    // The roster's own set names, NOT invented labels: an author name IS a
    // roster set name IS the suffix of a `sluice:<set>` ledger label.
    const KNOWN: &[&str] = &["artifacts", "census", "heavy"];
    let bad: Vec<String> = declared()
        .into_iter()
        .filter(|(_, author)| !KNOWN.contains(&author.as_str()))
        .map(|(p, a)| format!("{p} -> {a:?}"))
        .collect();
    assert!(
        bad.is_empty(),
        "docs/generated-paths.txt rows whose author is missing or unknown \
         (known: {KNOWN:?}). A declared generated path with no author is a \
         claim about this repository that nothing can check:\n  {}",
        bad.join("\n  ")
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
    let stems: Vec<String> = declared()
        .iter()
        .map(|(p, _author)| p.trim_end_matches('/').to_string())
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
/// The ratchet for decision 0166. `.gitattributes` once routed six
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
         Read docs/decisions/0166-a-generated-artifact-cannot-be-merged-by-\
         regenerating-it.md before re-adding one; the mechanism that works is \
         scripts/hooks/post-merge's advisory to run `make rebaseline`:\n  {}",
        offenders.join("\n  ")
    );
}

/// The source file where a given author's writes actually happen, found by
/// tracing each invocation rather than assumed (Task 3, The Attestation).
///
/// - `artifacts` and `census` share ONE literal writer:
///   `scripts/regenerate-artifacts.sh`. `census-run.sh` (the `census` set's
///   own command) does not write a single path itself — confirmed by
///   reading it end to end — it locks, guards the canonical host, and then
///   runs `HV_CENSUS=1 bash scripts/timed.sh census -- bash
///   scripts/regenerate-artifacts.sh` (`scripts/census-run.sh:211`). The two
///   authors are the same PROGRAM gated by one flag, which is exactly why
///   `docs/generated-paths.txt`'s header says the column names an
///   invocation, not a program: pointing both at the same file is not a
///   shortcut, it is the confirmed fact.
/// - `heavy`: `cli/tests/suite/history_battery.rs`. `scripts/gate-full-
///   heavy.sh` (the `heavy` set's own command) contains no literal output
///   path for anything — it discovers every `heavy:`-tagged test generically
///   by grepping for the tag and dispatches the whole roster through
///   nextest, so it cannot serve as a per-path writer check. Of that roster,
///   exactly one test currently writes a committed artifact:
///   `history_gates_full_world_and_cross_seed`, confirmed by the literal
///   `.join("../book/src/laboratory/generated/the-history")` in
///   `cli/tests/suite/history_battery.rs:403` — the same test CLAUDE.md's
///   "Where things run" section names as the tier's one authoring test.
fn source_for_author(author: &str) -> &'static str {
    match author {
        "artifacts" | "census" => "scripts/regenerate-artifacts.sh",
        "heavy" => "cli/tests/suite/history_battery.rs",
        other => panic!(
            "no known writing source recorded for author {other:?} — before declaring a \
             path with this author, find its writing site and add it here (Step 0, Task 3)"
        ),
    }
}

/// Every declared path must be one its declared **author** actually writes.
///
/// The list's whole purpose is "this author's invocation produces it, so a
/// stale copy is a bookkeeping failure that invocation fixes on its own". A
/// path whose declared author never writes it cannot satisfy that: at best
/// its `git diff --exit-code` is permanently empty and the entry is
/// decoration, and at worst — if it is a byte-golden under
/// `kernel/src/golden.rs` — declaring it invites the chamber to commit drift
/// that a human was supposed to review.
///
/// WHAT SUBSTRING MATCHING CANNOT SEE, FIRST DIRECTION (inherited from the
/// single-script version of this check): a future artifact written through a
/// shell variable (`"$out_dir/foo.md"`) would read as undeclared-by-the-
/// author and redden this test even though regeneration does produce it —
/// a false positive, so it fails *safe*.
///
/// SECOND DIRECTION, FOUND BY THIS CAMPAIGN: this check is satisfied at
/// DIRECTORY granularity while most files beneath stay unwritten by the
/// declared author. `docs/generated-path-authors.md` (Task 1) measured 814
/// of 825 tracked files under `book/src/laboratory/` left untouched by a
/// plain `make rebaseline` run — 682 authored only by `census`, 2 only by
/// `heavy`, and the rest hand-written or authored by nothing currently in
/// the tree — while this test, seeing only the row's single declared
/// `artifacts` value, stays green throughout. That is a FALSE NEGATIVE, the
/// opposite direction from the one above, and it fails *unsafe*: neither
/// this check nor `every_declared_path_names_a_known_author` can catch it,
/// because both operate on the row's one declared value, never on the files
/// underneath it.
#[test]
fn every_declared_generated_path_is_written_by_its_author() {
    let root = repo_root();

    let undeclared: Vec<String> = declared()
        .into_iter()
        .filter(|(path, author)| {
            let source_path = source_for_author(author);
            let source = std::fs::read_to_string(root.join(source_path))
                .unwrap_or_else(|e| panic!("reading {source_path}: {e}"));
            !source.contains(path.as_str())
        })
        .map(|(p, a)| format!("{p} (author {a:?})"))
        .collect();

    assert!(
        undeclared.is_empty(),
        "docs/generated-paths.txt declares {} path(s) whose declared author's own source \
         never writes them: {:?}\n\
         The list's criterion is 'this author's invocation produces it', not 'it is a \
         committed fixture'.\n\
         If one of these is a BYTE-GOLDEN (guarded by kernel/src/golden.rs, rebaselined only \
         by `make rebaseline-goldens`), it does not belong here at all: goldens are assertions, \
         and letting the artifacts phase commit their drift would silently accept a determinism \
         change. See the header of docs/generated-paths.txt.",
        undeclared.len(),
        undeclared,
    );
}

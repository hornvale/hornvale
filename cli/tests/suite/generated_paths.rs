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
//! FIFTH DIRECTION, TASK 5'S FIX FOR THE FOURTH: a declared path's author may
//! now be `none(<reason>)` instead of a roster name, declaring that NO
//! roster author writes it — the repo's `waiver(<reason>)` /
//! `expect(survives: <why>)` idiom, not a new invention. A row with a mixed
//! or absent true author is SPLIT into a directory row (true for the files
//! it still covers) plus individual file/subdirectory rows naming the real
//! author of the rest, closing the fourth direction's gap for every case this
//! campaign found. Two things this buys, and one it does not:
//!   - A reasonless `none` (bare `none`, or `none()` with nothing inside the
//!     parens) is a PARSE ERROR, raised by `declared()` itself, not a soft
//!     test failure — the reason is the whole value of the declaration.
//!   - A `none(...)` row that goes STALE — one of the two known author
//!     sources starts literally writing that path — is caught by
//!     `every_declared_generated_path_is_written_by_its_author`, the same
//!     one-directional-declaration rot seam-guard's STALE-DECL verdict
//!     exists to catch.
//!   - It does NOT make the third check's directory-granularity substring
//!     match exact. A directory row's author is still read as "true for
//!     every file under this path NOT more specifically declared elsewhere"
//!     (precedence: longest matching declared path governs a file's true
//!     author) — Task 5 closed every gap it MEASURED, not every gap the
//!     substring check could theoretically miss in the future.
//!
//! SIXTH DIRECTION, FOUND BY REVIEW OF TASK 5 ITSELF: precedence — "the
//! longest matching declared path governs a file's true author" — lived
//! only in prose (this file's own paragraph above, and
//! `docs/generated-paths.txt`'s header) until a reviewer proved it
//! unenforced. Appending
//! `book/src/laboratory/generated/the-census/rows.csv<TAB>artifacts` — a
//! factually wrong, MORE SPECIFIC row overlapping the correct
//! `book/src/laboratory/generated/the-census/<TAB>census` row — passed every
//! test in this file clean, because `every_declared_generated_path_is_
//! written_by_its_author` checks each row against its own author's source
//! in isolation, never against a competing row: `artifacts` and `census`
//! share ONE source (`source_for_author`), so the added row's claim
//! "written by artifacts" and the existing row's claim "written by census"
//! are indistinguishable from source text alone — a direct
//! consequence of `docs/generated-paths.txt`'s own header ("naming the
//! *script* here would collapse exactly the distinction this campaign
//! draws"). `an_overriding_declaration_must_be_measured` and
//! `no_two_declared_rows_tie_for_precedence_with_different_authors` close
//! this the only way available without re-deriving Task 1's census/heavy
//! measurement work inside a test: cross-referencing
//! `docs/generated-path-writes.tsv`, the committed record of what a REAL
//! `make rebaseline` run actually touched. A more-specific row that
//! overrides a less-specific row's author for some file must have its OWN
//! entry there — proof someone ran the regen after adding it, not merely
//! asserted the row — or the override is rejected as unverified. WHAT THIS
//! IS BLIND TO: a TSV entry that EXISTS but does not actually support the
//! currently-declared author (e.g. an author swapped after the last
//! measurement, with the path itself untouched) — this test checks
//! presence, not (again) ground truth; and a same-length tie between two
//! overlapping rows, which the sibling test below refuses outright rather
//! than resolving arbitrarily.
//!
//! A DECLARED PATH'S AUTHOR MUST ALSO BE ONE THE ROSTER NAMES, OR `none(...)`
//! (Step 2 below, `every_declared_path_names_a_known_author`) — `artifacts`, `census`, or
//! `heavy`, the set names `scripts/lane-sets.tsv` already uses for the same
//! invocations. Declared-implies-attributed; blind to a generated path
//! nobody declared, the same blindness the first direction documents.

use std::collections::BTreeSet;
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
///
/// A `none(...)` author is validated HERE, not in a later soft test: a bare
/// `none` or an empty `none()` panics immediately, exactly as a missing tab
/// column does above. This is deliberate, not an inconsistency with the
/// softer `every_declared_path_names_a_known_author` test below — a missing
/// or unrecognised author is a fact worth REPORTING (that test names every
/// offending row in one assertion), but a reasonless `none` is a malformed
/// DECLARATION, the same category of error as a row with no tab at all, and
/// the brief is explicit that it is a parse error: "a reasonless `none` is a
/// PARSE ERROR, exactly as it is for seam-guard".
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
            if author == "none" || author.starts_with("none(") {
                let reason = author
                    .strip_prefix("none(")
                    .and_then(|s| s.strip_suffix(')'))
                    .map(str::trim);
                if !matches!(reason, Some(r) if !r.is_empty()) {
                    panic!(
                        "docs/generated-paths.txt row {path:?} declares `none` with no \
                         reason — a reasonless `none(...)` is a PARSE ERROR, exactly as a \
                         reasonless `waiver(...)` is for type-audit or `expect(survives: ...)` \
                         is for seam-guard. Write `none(<reason>)` naming why no roster author \
                         writes this path."
                    );
                }
            }
            (path.to_string(), author.to_string())
        })
        .collect()
}

/// The reason inside a `none(<reason>)` author value, or `None` if `author`
/// is not the `none(...)` form. Never returns `Some("")` — `declared()`
/// already panics on a reasonless `none` before this function ever sees one,
/// so an empty reason here would mean that guard broke, not that one exists
/// in the file.
fn none_reason(author: &str) -> Option<&str> {
    author
        .strip_prefix("none(")
        .and_then(|s| s.strip_suffix(')'))
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

/// The roster's own set names, NOT invented labels: an author name IS a
/// roster set name IS the suffix of a `sluice:<set>` ledger label.
///
/// THIS WAS THE SIXTH INSTANCE (final review of The Attestation, I1) — a
/// rule restated with no agreement test, minted by decision 0456 and
/// committed in the very file that states it. Proven: deleting the
/// `census` row from `scripts/lane-sets.tsv` left this list naming a set
/// that no longer exists, and the suite stayed green. `KNOWN_IS_A_ROSTER_
/// SUBSET` below is the guard; without it, this constant is exactly the
/// kind of hand-copied duplicate `cli/tests/suite/lane_sets.rs`'s
/// `the_phase_lists_and_the_roster_rungs_agree_both_ways` and
/// `cli/tests/suite/attest.rs`'s
/// `the_conditionally_droppable_set_agrees_with_sluice_phases_sh` already
/// exist to catch elsewhere in this tree.
const KNOWN: &[&str] = &["artifacts", "census", "heavy"];

/// Every declared path names an author, and the author is one we know: a
/// roster set name, or `none(<reason>)` declaring that no roster author
/// writes it (Task 5, The Attestation).
///
/// # Direction this check enforces
///
/// Declared-implies-attributed. Blind to a generated path nobody declared —
/// the same blindness `every_declared_generated_path_is_tracked` documents,
/// and for the same reason: nothing enumerates this repository's generated
/// output independently of this file.
///
/// A malformed `none` (no reason) never reaches this test at all —
/// `declared()` panics on it first, because that is a parse error, not a
/// reporting-worthy fact about the roster. What this test catches is
/// different: an author that is neither a roster name NOR a well-formed
/// `none(...)`, e.g. a typo or an invented label.
#[test]
fn every_declared_path_names_a_known_author() {
    let bad: Vec<String> = declared()
        .into_iter()
        .filter(|(_, author)| !KNOWN.contains(&author.as_str()) && none_reason(author).is_none())
        .map(|(p, a)| format!("{p} -> {a:?}"))
        .collect();
    assert!(
        bad.is_empty(),
        "docs/generated-paths.txt rows whose author is missing or unknown \
         (known: {KNOWN:?}, or `none(<reason>)`). A declared generated path with no \
         recognised author is a claim about this repository that nothing can check:\n  {}",
        bad.join("\n  ")
    );
}

/// `KNOWN` agrees with `scripts/lane-sets.tsv` — the guard for the constant
/// above, modelled on `cli/tests/suite/lane_sets.rs`'s
/// `the_phase_lists_and_the_roster_rungs_agree_both_ways` and
/// `cli/tests/suite/attest.rs`'s
/// `the_conditionally_droppable_set_agrees_with_sluice_phases_sh`: a fact
/// stated once in a TSV and copied by hand into a Rust constant needs an
/// agreement test, not just a comment claiming they match.
///
/// # Direction this check enforces, and the asymmetry that shapes it
///
/// **Every value in `KNOWN` must name a roster row whose `authors` column is
/// `yes`.** That is the one direction that actually holds: `style`,
/// `subfloor`, `gate` and `seam-guard` are rostered sets that author no
/// generated path at all (their `authors` column says `no`), so `KNOWN`
/// naming every roster set — the tempting symmetric check — would be FALSE
/// on the real roster today, not a stricter guard. The reverse also does not
/// hold: `outboard` and `clients` are `authors=yes` rostered sets with no
/// currently-declared path, because nothing in `docs/generated-paths.txt`
/// happens to name them yet — asserting `KNOWN` must equal the full
/// `authors=yes` set would fail on that alone, for no defect. So this is a
/// one-directional subset check: `KNOWN ⊆ {roster rows with authors=yes}`.
///
/// This direction is sufficient to catch the review's reproduction case
/// (deleting the `census` roster row): `"census"` stays in `KNOWN` but no
/// longer names ANY roster row, `authors=yes` or otherwise, so the subset
/// check fails directly — it needs no second assertion for a removed
/// roster set, because "removed" is just "absent from the roster entirely",
/// which the subset already requires against.
///
/// # What this is still blind to
///
/// A roster set gaining `authors=yes` and a real writer, with nobody adding
/// it to `KNOWN` — a future `docs/generated-paths.txt` row naming that
/// author would then fail `every_declared_path_names_a_known_author`
/// (loudly, at the point someone tries to declare the row), not this test.
/// That is the same shape as `every_declared_generated_path_is_tracked`'s
/// own blindness to an undeclared path: nothing here enumerates the
/// roster's *intended* future authors independently of `KNOWN` itself.
#[test]
fn known_authors_agree_with_the_roster() {
    let authors_yes: BTreeSet<String> = crate::lane_sets::roster()
        .into_iter()
        .filter(|(_, _, _, authors, _)| authors == "yes")
        .map(|(name, ..)| name)
        .collect();

    let stale: Vec<&str> = KNOWN
        .iter()
        .copied()
        .filter(|name| !authors_yes.contains(*name))
        .collect();

    assert!(
        stale.is_empty(),
        "cli/tests/suite/generated_paths.rs's KNOWN names {stale:?}, which \
         scripts/lane-sets.tsv either has no row for at all, or rosters with \
         authors=no. KNOWN must be a subset of the roster's authors=yes set \
         names — one of the two files moved without the other. (Roster rows \
         with authors=yes today: {authors_yes:?})"
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
///
/// NESTED STEMS DON'T DOUBLE-COUNT (Task 5, The Attestation). Splitting a
/// directory row into per-file exceptions means a stem like
/// `docs/digest/facts.jsonl` now sits inside an already-declared parent
/// stem, `docs/digest` — so a line naming ONE file (`docs/digest/facts.jsonl
/// is the compacted...`) trivially also "contains" its own parent's stem as
/// a prefix, which would count as 2 distinct paths named by a naive
/// substring tally even though the line mentions exactly one thing. A
/// matched stem that is a proper substring of another ALSO-matched, longer
/// stem on the same line is dropped before counting — it is a fragment of
/// the longer match, not a second distinct reference.
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
            let matched: Vec<&String> = stems
                .iter()
                .filter(|stem| line.contains(stem.as_str()))
                .collect();
            let named = matched
                .iter()
                .filter(|stem| {
                    !matched
                        .iter()
                        .any(|other| other.len() > stem.len() && other.contains(stem.as_str()))
                })
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

/// Every distinct source file a roster author's writes are found in. Used to
/// check a `none(...)` declaration for staleness: a path declared absent
/// must not appear, by either matching rule below, in ANY of these, not just
/// the one source its (nonexistent) roster author would map to.
const KNOWN_SOURCES: &[&str] = &[
    "scripts/regenerate-artifacts.sh",
    "cli/tests/suite/history_battery.rs",
];

/// Whether `source`'s text can be read as writing `path`.
///
/// The baseline rule is the literal substring match this check has always
/// used (see `every_declared_generated_path_is_written_by_its_author`'s own
/// doc comment for what that rule cannot see). Task 5 needed a second rule
/// for one specific, narrow case, found while splitting `book/src/
/// laboratory/` into per-study rows: `scripts/regenerate-artifacts.sh`
/// invokes a lab study as `lab run studies/<name>.study.json`, and the
/// convention that its OUTPUT lands at `book/src/laboratory/generated/
/// <name>/` lives in `windows/lab`'s Rust, not as a literal path in the
/// shell script — exactly the "future artifact written through a shell
/// variable" case the module doc already named as a known false-positive
/// shape. Falling back to the bare STUDY NAME for a path under that one
/// directory convention is a real, narrow, non-vacuous signal, not a general
/// weakening of the check: Task 5 verified that every currently-automated
/// study name (`the-chorus`, `the-census`, `census-of-the-meeting`) appears
/// literally in `scripts/regenerate-artifacts.sh` — via its own `lab run
/// studies/<name>.study.json` call, or (for the two census schemas) the
/// schema-backfill loop's `for study in the-census census-of-the-meeting`
/// list — and that NONE of the nine frozen study names, nor `the-sounding`,
/// appears anywhere in either known source. So the fallback stays a
/// falsifiable ratchet: automate a frozen study again and its name starts
/// appearing, catching a `none(...)` row gone stale; retire an automated
/// study's `lab run` call and its name disappears, catching the row that
/// should have been downgraded to `none(...)` and was not.
fn source_writes_path(path: &str, source: &str) -> bool {
    if source.contains(path) {
        return true;
    }
    if let Some(rest) = path.strip_prefix("book/src/laboratory/generated/") {
        let name = rest.trim_end_matches('/').split('/').next().unwrap_or("");
        if !name.is_empty() && source.contains(name) {
            return true;
        }
    }
    false
}

/// `source` with comment-only lines dropped — lines whose trimmed start is
/// `#` (bash) or `//` (Rust). Good enough for this file's two known sources
/// without a general comment parser; used ONLY for the `none(...)`
/// staleness check below, never for the roster-author check above, which
/// keeps its long-standing (weaker, documented) behaviour of accepting a
/// comment mention as evidence.
///
/// WHY THE TWO CHECKS WANT OPPOSITE LENIENCE: the roster-author check is
/// safe to fool with a mere mention — that direction already fails *safe*
/// (a false "written" reads as fine when it might not be). The staleness
/// check is not: `book/src/gallery/surrounds-seed-42.md` is declared
/// `none(...)` precisely BECAUSE `scripts/regenerate-artifacts.sh:776-777`
/// says, in a comment, that it is hand-authored and never regenerated — the
/// very sentence proving the `none(...)` row correct also contains the
/// path's literal text, which would read as "now written" under the plain
/// rule and falsely redden an accurate declaration.
fn non_comment_text(source: &str) -> String {
    source
        .lines()
        .filter(|line| {
            let trimmed = line.trim_start();
            !(trimmed.starts_with('#') || trimmed.starts_with("//"))
        })
        .collect::<Vec<_>>()
        .join("\n")
}

/// Every declared path must be one its declared **author** actually writes —
/// or, for a `none(<reason>)` row, one that NO known author writes.
///
/// The list's whole purpose is "this author's invocation produces it, so a
/// stale copy is a bookkeeping failure that invocation fixes on its own". A
/// path whose declared author never writes it cannot satisfy that: at best
/// its `git diff --exit-code` is permanently empty and the entry is
/// decoration, and at worst — if it is a byte-golden under
/// `kernel/src/golden.rs` — declaring it invites the chamber to commit drift
/// that a human was supposed to review.
///
/// WHAT MATCHING CANNOT SEE, FIRST DIRECTION (inherited from the
/// single-script version of this check): a future artifact written through a
/// shell variable would read as undeclared-by-the-author and redden this
/// test even though regeneration does produce it — a false positive, so it
/// fails *safe*. `source_writes_path` closes the one instance of this Task 5
/// actually hit (the lab-study output-directory convention); it does not
/// close the shape in general.
///
/// SECOND DIRECTION, FOUND BY THE ATTESTATION AND FIXED BY TASK 5: this
/// check used to be satisfied at DIRECTORY granularity while most files
/// beneath stayed unwritten by the declared author.
/// `docs/generated-path-authors.md` (Task 1) measured 814 of 825 tracked
/// files under the old single `book/src/laboratory/` row left untouched by a
/// plain `make rebaseline` run — 682 authored only by `census`, 2 only by
/// `heavy`, and the rest hand-written or authored by nothing currently in
/// the tree. Task 5 replaced that one row (and four siblings with the same
/// shape) with per-file/per-subdirectory rows naming the TRUE author of
/// each — often `none(...)` — so the row-level check this test performs is
/// now checking something true, not merely something this test could not
/// see through. The remaining, accepted blind spot is narrower: a directory
/// row's author is read as "true for every file under it not more
/// specifically declared elsewhere" (precedence: longest matching declared
/// path wins), which this test — being row-scoped, not file-scoped — cannot
/// verify by itself; `docs/generated-path-authors.md` and
/// `docs/generated-path-writes.tsv` are what Task 5's report checked that
/// precedence claim against, by hand, per row.
///
/// THIRD DIRECTION: a `none(<reason>)` row asserts an ABSENCE, and an
/// absence can go stale exactly the way seam-guard's `expect(survives: ...)`
/// does — the moment some author starts writing the path, the declaration
/// is no longer true and must be upgraded to name that author. This test
/// checks every `none(...)` row against every known source
/// (`KNOWN_SOURCES`), not just the one its (nonexistent) roster author would
/// map to, and fails loudly on the first one that has started being
/// written — the mirror image of the roster-author check just above it.
#[test]
fn every_declared_generated_path_is_written_by_its_author() {
    let root = repo_root();
    let sources: Vec<(&str, String)> = KNOWN_SOURCES
        .iter()
        .map(|&source_path| {
            let text = std::fs::read_to_string(root.join(source_path))
                .unwrap_or_else(|e| panic!("reading {source_path}: {e}"));
            (source_path, text)
        })
        .collect();
    let source_text = |source_path: &str| -> &str {
        &sources
            .iter()
            .find(|(p, _)| *p == source_path)
            .unwrap_or_else(|| panic!("no known source loaded for {source_path:?}"))
            .1
    };

    let non_comment_sources: Vec<String> = sources
        .iter()
        .map(|(_, text)| non_comment_text(text))
        .collect();

    let mut undeclared: Vec<String> = Vec::new();
    let mut stale_none: Vec<String> = Vec::new();

    for (path, author) in declared() {
        if let Some(reason) = none_reason(&author) {
            if non_comment_sources
                .iter()
                .any(|text| source_writes_path(&path, text))
            {
                stale_none.push(format!("{path} (declared none({reason:?}))"));
            }
            continue;
        }
        let source_path = source_for_author(&author);
        if !source_writes_path(&path, source_text(source_path)) {
            undeclared.push(format!("{path} (author {author:?})"));
        }
    }

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

    assert!(
        stale_none.is_empty(),
        "docs/generated-paths.txt declares {} path(s) `none(<reason>)` that a known author's \
         source NOW writes: {:?}\n\
         A `none(...)` declaration is a claim that NO roster author writes this path — the \
         same one-directional-declaration rot seam-guard's STALE-DECL verdict exists to catch. \
         Give the path a real author instead of `none(...)`.",
        stale_none.len(),
        stale_none,
    );
}

/// The measured write-set, `docs/generated-path-writes.tsv` (Task 4/5), as
/// `path -> (written, tracked)`. That file's own header excludes itself
/// (`docs/generated-path-writes.tsv`) from the emitted rows — a
/// self-referential mtime problem documented there and in
/// `docs/generated-paths.txt` — so it is never looked up here either; no
/// declared row needs to find an entry for it.
fn measured_writes() -> std::collections::BTreeMap<String, (u32, u32)> {
    let text = std::fs::read_to_string(repo_root().join("docs/generated-path-writes.tsv"))
        .expect("docs/generated-path-writes.tsv must exist");
    text.lines()
        .map(str::trim)
        .filter(|l| !l.is_empty() && !l.starts_with('#'))
        .map(|l| {
            let mut fields = l.splitn(3, '\t');
            let path = fields
                .next()
                .unwrap_or_else(|| panic!("empty docs/generated-path-writes.tsv line"));
            let written: u32 = fields
                .next()
                .unwrap_or_else(|| {
                    panic!("docs/generated-path-writes.tsv row {path:?} has no written column")
                })
                .parse()
                .unwrap_or_else(|e| {
                    panic!("docs/generated-path-writes.tsv row {path:?} written column: {e}")
                });
            let tracked: u32 = fields
                .next()
                .unwrap_or_else(|| {
                    panic!("docs/generated-path-writes.tsv row {path:?} has no tracked column")
                })
                .parse()
                .unwrap_or_else(|e| {
                    panic!("docs/generated-path-writes.tsv row {path:?} tracked column: {e}")
                });
            (path.to_string(), (written, tracked))
        })
        .collect()
}

/// Every declared row that covers a tracked file (directory prefix, or exact
/// file match).
fn covers(declared_path: &str, file: &str) -> bool {
    match declared_path.strip_suffix('/') {
        Some(_) => file.starts_with(declared_path),
        None => file == declared_path,
    }
}

/// No two declared rows may TIE for precedence over the same file while
/// naming different authors.
///
/// # Direction this check enforces
///
/// Precedence (docs/generated-paths.txt's header, and this file's SIXTH
/// direction above) is "the LONGEST matching declared path governs a
/// file's true author" — a rule with no defined answer when two covering
/// rows are the same length. Today that can only happen if two rows
/// declare the literal same path twice (a straightforward duplicate) or two
/// distinct files/directories of coincidentally equal path length both
/// claim to be the most specific declaration for the same file, which is
/// not possible for two DIFFERENT paths covering the same file (a shorter
/// string cannot be a proper prefix of an equal-length string unless the
/// two are identical) — so in practice this test only ever catches a
/// literal duplicate declaration. It is kept as its own test, rather than
/// folded into `an_overriding_declaration_must_be_measured`, because a tie
/// has no resolvable winner at all: there is nothing to look up in
/// `docs/generated-path-writes.tsv` for a row that does not exist.
///
/// WHAT THIS IS BLIND TO: everything `an_overriding_declaration_must_be_
/// measured` covers — a clean (non-tied) override with no measurement
/// behind it — is out of scope here on purpose; the two tests are
/// complementary, not overlapping.
#[test]
fn no_two_declared_rows_tie_for_precedence_with_different_authors() {
    let root = repo_root();
    let out = Command::new("git")
        .arg("-C")
        .arg(&root)
        .args(["ls-files"])
        .output()
        .expect("git ls-files must run");
    let files: Vec<String> = String::from_utf8_lossy(&out.stdout)
        .lines()
        .map(str::to_string)
        .collect();
    let rows = declared();

    let mut ties: Vec<String> = Vec::new();
    for file in &files {
        let mut covering: Vec<&(String, String)> =
            rows.iter().filter(|(path, _)| covers(path, file)).collect();
        if covering.len() < 2 {
            continue;
        }
        covering.sort_by_key(|(p, _)| std::cmp::Reverse(p.len()));
        let longest_len = covering[0].0.len();
        let winners: Vec<&&(String, String)> = covering
            .iter()
            .filter(|(p, _)| p.len() == longest_len)
            .collect();
        let authors: std::collections::BTreeSet<&str> =
            winners.iter().map(|(_, a)| a.as_str()).collect();
        if winners.len() > 1 && authors.len() > 1 {
            ties.push(format!("{file} -- tied rows: {winners:?}"));
        }
    }

    assert!(
        ties.is_empty(),
        "these files are covered by two declared rows of EQUAL length naming DIFFERENT \
         authors -- precedence (longest match wins) has no defined winner when lengths tie:\n  {}",
        ties.join("\n  ")
    );
}

/// A more-specific declared row that OVERRIDES a less-specific row's author
/// for some file must have been through a real `make rebaseline` measurement
/// before that override is trusted.
///
/// # Direction this check enforces
///
/// Resolves, for every tracked file covered by 2+ declared rows, the
/// governing (longest-match) row. When the covering rows DISAGREE on
/// author, the governing row must have its own entry in
/// `docs/generated-path-writes.tsv` — proof a real regen ran with this row
/// in place, not merely that someone typed a plausible-looking line. This
/// is the check the SIXTH direction above names: it reproduces and fails on
/// appending `book/src/laboratory/generated/the-census/rows.csv<TAB>artifacts`,
/// because that row overrides `book/src/laboratory/generated/the-census/`'s
/// `census` author for `rows.csv` and has no measurement entry.
///
/// WHY THIS AND NOT A GROUND-TRUTH CHECK: `artifacts` and `census` share one
/// literal source file (`scripts/regenerate-artifacts.sh`, gated by
/// `HV_CENSUS`), so no source-text check can ever tell which of the two is
/// true for a given path — that distinction lives in a shell conditional
/// this file does not parse and should not start parsing. Requiring a real
/// measurement is the check this project's own idiom prefers: "studies are
/// data, measurement is code" (decision 0011) applied to this file's own
/// claims about itself.
///
/// WHAT THIS IS BLIND TO, stated once and not repeated per-row: a
/// `docs/generated-path-writes.tsv` entry that EXISTS for the overriding
/// row is trusted at face value — this test does not re-derive whether that
/// entry's `written` count actually supports the CURRENTLY declared author,
/// only that a real run produced a row for this exact path at some point.
/// An author changed on an already-measured path, with the path itself
/// untouched since, would pass here silently. It is also blind to a file
/// with only ONE covering row (no override in play at all) and to the
/// same-length-tie case, which
/// `no_two_declared_rows_tie_for_precedence_with_different_authors` covers
/// instead.
#[test]
fn an_overriding_declaration_must_be_measured() {
    let root = repo_root();
    let out = Command::new("git")
        .arg("-C")
        .arg(&root)
        .args(["ls-files"])
        .output()
        .expect("git ls-files must run");
    let files: Vec<String> = String::from_utf8_lossy(&out.stdout)
        .lines()
        .map(str::to_string)
        .collect();
    let rows = declared();
    let measured = measured_writes();

    let mut unmeasured_overrides: Vec<String> = Vec::new();
    for file in &files {
        let mut covering: Vec<&(String, String)> =
            rows.iter().filter(|(path, _)| covers(path, file)).collect();
        if covering.len() < 2 {
            continue;
        }
        covering.sort_by_key(|(p, _)| std::cmp::Reverse(p.len()));
        let (winning_path, winning_author) = covering[0];
        let disagreement = covering[1..].iter().any(|(_, a)| a != winning_author);
        if disagreement && !measured.contains_key(winning_path) {
            unmeasured_overrides.push(format!(
                "{file} -- governed by {winning_path:?} (author {winning_author:?}), which overrides \
                 a less specific row's different author but has no docs/generated-path-writes.tsv entry"
            ));
        }
    }

    assert!(
        unmeasured_overrides.is_empty(),
        "{} file(s) are governed by a more-specific declared row that OVERRIDES a less-specific \
         row's author, with no docs/generated-path-writes.tsv entry backing the override -- run \
         `make rebaseline` and commit the refreshed write-capture before trusting this declaration:\n  {}",
        unmeasured_overrides.len(),
        unmeasured_overrides.join("\n  ")
    );
}

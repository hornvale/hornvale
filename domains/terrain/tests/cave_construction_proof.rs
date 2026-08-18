//! The guard that makes `Cave`'s compile-fail doctest mean something.
//!
//! `Cave` carries a derived-field invariant (`deepest_band` is a lookup of
//! `depth_reach_m` against the column) that `#[non_exhaustive]` turns into a
//! compiler guarantee: outside `hornvale-terrain`, a struct expression for
//! `Cave` is refused. The natural proof is a `compile_fail` doctest — and a
//! bare `compile_fail` doctest is the single easiest kind of test to leave
//! vacuous, because it passes when something breaks.
//!
//! **rustdoc does not check the reason, and it does not check a pinned error
//! code either.** That was measured on this very block, not assumed: with the
//! violating expression swapped for an undefined identifier (`E0425`) and the
//! annotation left at `compile_fail,E0639`, the doctest still reported `ok`.
//! An error code after the comma is documentation on stable, not an assertion.
//!
//! So the proof is a **differential pair** instead: two doctests on `Cave`
//! whose text is identical except for one line — the control builds through
//! the sanctioned constructor and must compile, the subject uses the forbidden
//! struct expression and must not. Any incidental breakage lands on a shared
//! line, breaks the control, and is caught there.
//!
//! That argument only holds while the two blocks really are identical-but-one,
//! and nothing in rustdoc enforces that. This file does. It re-reads
//! `src/features.rs` and fails unless:
//!
//! 1. exactly one block is marked `control` and exactly one `subject`;
//! 2. the control's fence carries no info string and the subject's carries
//!    exactly `compile_fail` (not `ignore`, not `no_run`, and not an error
//!    code masquerading as a check);
//! 3. after dropping the marker line the two bodies have the same length and
//!    differ at **exactly one** index;
//! 4. that one line is a `Cave` struct expression in the subject and a call to
//!    a sanctioned constructor in the control.
//!
//! Clause 3 catches an unrelated compile error dropped anywhere in the shared
//! body; clause 4 catches one dropped on the differing line itself, which is
//! the mutation that exposed the original defect. Together they leave the
//! construction path as the only difference that can explain the subject's
//! refusal.
//!
//! **What is still not proved.** The pair does not establish *which* error the
//! subject raises — only that it fails for a reason the control does not
//! share, and that the unshared line is a `Cave` struct expression. A
//! compile-error-matching harness (`trybuild`) would close that, and the
//! dependency allowlist forbids it (decision 0004). Clause 4 is also a textual
//! match, not a parse: a line containing `Cave {` could in principle carry a
//! second, unrelated error alongside the struct expression.

use std::path::Path;

/// The marker comment identifying the block that must compile.
const CONTROL_MARKER: &str = "differential-pair: control";
/// The marker comment identifying the block that must not compile.
const SUBJECT_MARKER: &str = "differential-pair: subject";

/// One fenced code block lifted out of a `///` doc comment.
struct DocBlock {
    /// The fence's info string, trimmed (`""` for a bare fence).
    info: String,
    /// The block's body lines, with the `///` prefix stripped.
    lines: Vec<String>,
}

/// Lift every fenced block out of the `///` doc comments in `src`.
fn doc_blocks(src: &str) -> Vec<DocBlock> {
    let mut blocks = Vec::new();
    let mut open: Option<DocBlock> = None;
    for raw in src.lines() {
        let Some(rest) = raw.trim_start().strip_prefix("///") else {
            continue;
        };
        let content = rest.strip_prefix(' ').unwrap_or(rest);
        if let Some(info) = content.strip_prefix("```") {
            match open.take() {
                None => {
                    open = Some(DocBlock {
                        info: info.trim().to_string(),
                        lines: Vec::new(),
                    });
                }
                Some(block) => blocks.push(block),
            }
        } else if let Some(block) = open.as_mut() {
            block.lines.push(content.to_string());
        }
    }
    assert!(
        open.is_none(),
        "unterminated fenced code block in the doc comments"
    );
    blocks
}

/// The one block carrying `marker`, with the marker line removed.
fn take_marked(blocks: &[DocBlock], marker: &str) -> (String, Vec<String>) {
    let matched: Vec<&DocBlock> = blocks
        .iter()
        .filter(|b| b.lines.iter().any(|l| l.contains(marker)))
        .collect();
    assert_eq!(
        matched.len(),
        1,
        "expected exactly one doc block marked `{marker}`, found {}; the \
         differential pair's guard cannot identify its subjects without them",
        matched.len()
    );
    let block = matched[0];
    let body: Vec<String> = block
        .lines
        .iter()
        .filter(|l| !l.contains(marker))
        .cloned()
        .collect();
    (block.info.clone(), body)
}

#[test]
fn the_cave_compile_fail_doctest_is_a_true_differential_pair() {
    let path = Path::new(env!("CARGO_MANIFEST_DIR")).join("src/features.rs");
    let src = std::fs::read_to_string(&path)
        .unwrap_or_else(|e| panic!("cannot read {}: {e}", path.display()));
    let blocks = doc_blocks(&src);

    let (control_info, control) = take_marked(&blocks, CONTROL_MARKER);
    let (subject_info, subject) = take_marked(&blocks, SUBJECT_MARKER);

    // Clause 2 — the fences say what they must, and nothing else. An `ignore`
    // or `no_run` here would quietly stop the pair from being run at all, and
    // an error code after the comma would read as a check rustdoc never makes.
    assert_eq!(
        control_info, "",
        "the control's fence must carry no info string, so rustdoc compiles \
         AND runs it; found `{control_info}`"
    );
    assert_eq!(
        subject_info, "compile_fail",
        "the subject's fence must be exactly `compile_fail`; found \
         `{subject_info}`. rustdoc does not verify an error code written after \
         the comma, so one there would assert nothing while reading as proof"
    );

    // Clause 3 — identical but for one line.
    assert_eq!(
        control.len(),
        subject.len(),
        "the differential pair must be line-for-line comparable: control has \
         {} lines, subject has {}",
        control.len(),
        subject.len()
    );
    let differing: Vec<usize> = (0..control.len())
        .filter(|&i| control[i] != subject[i])
        .collect();
    assert_eq!(
        differing.len(),
        1,
        "the pair must differ on exactly one line, or an incidental compile \
         error in the subject would not break the control; it differs on {:?}: \
         {:#?}",
        differing,
        differing
            .iter()
            .map(|&i| (control[i].clone(), subject[i].clone()))
            .collect::<Vec<_>>()
    );

    // Clause 4 — and that one line is the construct under test. This is the
    // clause that catches an unrelated compile error swapped in for the
    // violation, which a bare `compile_fail` (with or without an error code)
    // accepts silently.
    let i = differing[0];
    let control_line = control[i].trim();
    let subject_line = subject[i].trim();
    assert!(
        subject_line.contains("Cave {"),
        "the subject's unshared line must be the forbidden `Cave` struct \
         expression — that is the whole thing being proved unconstructable. \
         Found: `{subject_line}`"
    );
    assert!(
        control_line.contains("Cave::from_reach(") || control_line.contains("Cave::new("),
        "the control's unshared line must build the same value through a \
         sanctioned constructor, so the pair differs only in construction \
         path. Found: `{control_line}`"
    );
    assert!(
        !control_line.contains("Cave {"),
        "the control must not itself contain a `Cave` struct expression, or \
         the pair would not differ in the construct under test. Found: \
         `{control_line}`"
    );
}

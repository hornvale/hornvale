//! The Repertoire's ratchet. `docs/audits/trope-coverage.md` is a committed
//! artifact; this fails when the live report diverges from it by a single
//! byte.
//!
//! Spec D7 asks for a PER-SITUATION ratchet — "no situation that was
//! stageable becomes unstageable" — because an aggregate percentage lets a
//! corpus pruning read as an improvement. This test is a whole-file byte
//! comparison, which is strictly STRONGER than D7's rule: it cannot admit the
//! failure D7 names, since a situation changing verdict changes bytes. What
//! it gives up is diagnosis. A red result says the report moved, not which
//! situation moved or in which direction, so read the diff before deciding
//! whether the movement was intended. Regenerate deliberately with
//! `make rebaseline`.

use std::process::Command;

/// The workspace root. `cmd_tropes` resolves both the corpus and the committed
/// artifact relative to the working directory, so every test here must set it.
fn workspace_root() -> std::path::PathBuf {
    std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("workspace root")
        .to_path_buf()
}

#[test]
fn committed_trope_coverage_matches_the_live_report() {
    let root = workspace_root();
    let out = Command::new(env!("CARGO_BIN_EXE_hornvale"))
        .args(["tropes", "report"])
        .current_dir(&root)
        .output()
        .expect("runs the binary");
    assert!(out.status.success(), "tropes report failed: {out:?}");
    let live = String::from_utf8(out.stdout).expect("utf-8");
    hornvale_kernel::golden::assert_golden(
        &root.join("docs/audits/trope-coverage.md"),
        &live,
        "the trope-coverage report drifted from the committed artifact. Regenerate \
         deliberately with `make rebaseline` and review the diff — a situation that \
         changed verdict means a predicate the corpus depends on moved, which is a \
         finding, not a formality",
    );
}

/// `check` mode agrees with the committed artifact and stays silent when it
/// does. Spec D7 names `check` as the ratchet; the byte ratchet above is what
/// actually gates, so without this the command itself is never exercised.
#[test]
fn check_mode_agrees_with_the_committed_artifact() {
    let root = workspace_root();
    let out = Command::new(env!("CARGO_BIN_EXE_hornvale"))
        .args(["tropes", "check"])
        .current_dir(&root)
        .output()
        .expect("runs the binary");
    assert!(out.status.success(), "tropes check failed: {out:?}");
    assert!(
        out.stdout.is_empty(),
        "check should be silent on agreement, printed {} bytes",
        out.stdout.len()
    );
}

/// A mode following a flag is still found. Reading only `args[1]` made
/// `tropes --corpus <path> check` print a report and exit 0 — a silent false
/// pass for anything gating on `check`. Nothing else guards that fix.
#[test]
fn check_mode_is_found_after_a_flag() {
    let root = workspace_root();
    let out = Command::new(env!("CARGO_BIN_EXE_hornvale"))
        .args(["tropes", "--corpus", "tropes/polti.trope.json", "check"])
        .current_dir(&root)
        .output()
        .expect("runs the binary");
    assert!(
        out.status.success(),
        "tropes --corpus … check failed: {out:?}"
    );
    assert!(
        out.stdout.is_empty(),
        "the flag form fell through to report and printed {} bytes",
        out.stdout.len()
    );
}

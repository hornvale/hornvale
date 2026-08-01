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

#[test]
fn committed_trope_coverage_matches_the_live_report() {
    let root = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("workspace root")
        .to_path_buf();
    let out = Command::new(env!("CARGO_BIN_EXE_hornvale"))
        .args(["tropes", "report"])
        .current_dir(&root)
        .output()
        .expect("runs the binary");
    assert!(out.status.success(), "tropes report failed: {out:?}");
    let live = String::from_utf8(out.stdout).expect("utf-8");
    let path = root.join("docs/audits/trope-coverage.md");
    if std::env::var("REBASELINE").is_ok() {
        std::fs::write(&path, &live).expect("rebaselines");
        return;
    }
    let committed = std::fs::read_to_string(&path).expect("committed report exists");
    assert_eq!(
        live, committed,
        "trope coverage drifted. If deliberate: REBASELINE=1 cargo test -p hornvale \
         --test trope_coverage, then review the diff and say why in the chronicle."
    );
}

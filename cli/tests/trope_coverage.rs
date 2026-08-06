//! The Repertoire's ratchet. Each corpus has its own committed artifact,
//! `docs/audits/trope-coverage-<corpus-id>.md`; these fail when a corpus's
//! live report diverges from its artifact by a single byte.
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

/// Shared body for `committed_trope_coverage_matches_the_live_report_*`: run
/// `tropes report` against one corpus and compare it to that corpus's own
/// committed artifact.
///
/// A shared helper called from one `#[test]` per corpus — rather than a
/// single test looping over both — keeps each corpus's pass/fail independent
/// and named in CI output; a loop would report only "the test failed" and
/// leave which corpus moved to the panic message alone.
fn assert_committed_matches_live(corpus_path: &str, artifact_stem: &str) {
    let root = workspace_root();
    let out = Command::new(env!("CARGO_BIN_EXE_hornvale"))
        .args(["tropes", "--corpus", corpus_path, "report"])
        .current_dir(&root)
        .output()
        .expect("runs the binary");
    assert!(out.status.success(), "tropes report failed: {out:?}");
    let live = String::from_utf8(out.stdout).expect("utf-8");
    hornvale_kernel::golden::assert_golden(
        &root.join(format!("docs/audits/trope-coverage-{artifact_stem}.md")),
        &live,
        "the trope-coverage report drifted from the committed artifact. Regenerate \
         deliberately with `make rebaseline` and review the diff — a situation that \
         changed verdict means a predicate the corpus depends on moved, which is a \
         finding, not a formality",
    );
}

#[test]
fn committed_trope_coverage_matches_the_live_report_for_polti() {
    assert_committed_matches_live("tropes/polti.trope.json", "polti-1895");
}

#[test]
fn committed_trope_coverage_matches_the_live_report_for_tvtropes_2012() {
    assert_committed_matches_live("tropes/tvtropes-2012.trope.json", "tvtropes-2012");
}

/// Spec invariant: "`hornvale tropes report` with no `--corpus` still
/// defaults to Polti and still reproduces its artifact byte-for-byte, so the
/// rename is the only change visible to an existing caller." Every other
/// test in this file passes `--corpus` explicitly, so without this one
/// nothing exercises `cmd_tropes`'s default (`unwrap_or("tropes/polti.trope.json")`
/// at `cli/src/main.rs`) or the no-mode-flag arm. That default is also what
/// `scripts/regenerate-artifacts.sh:394` relies on for Polti's artifact — if
/// its spelling ever drifted from `tropes/polti.trope.json`, the regenerate
/// script would silently start writing a header the golden test above
/// never actually re-derives through the same code path, and every explicit
/// `--corpus tropes/polti.trope.json` test would stay green throughout.
#[test]
fn bare_tropes_report_still_defaults_to_polti_and_matches_its_artifact() {
    let root = workspace_root();
    let out = Command::new(env!("CARGO_BIN_EXE_hornvale"))
        .args(["tropes", "report"])
        .current_dir(&root)
        .output()
        .expect("runs the binary");
    assert!(out.status.success(), "tropes report failed: {out:?}");
    let live = String::from_utf8(out.stdout).expect("utf-8");
    hornvale_kernel::golden::assert_golden(
        &root.join("docs/audits/trope-coverage-polti-1895.md"),
        &live,
        "the bare `tropes report` invocation (no --corpus) must still default to Polti \
         and reproduce its committed artifact byte-for-byte — if this drifts while the \
         explicit `--corpus tropes/polti.trope.json` test still passes, the default \
         corpus path baked into `cmd_tropes` has moved",
    );
}

/// Shared body for `check_mode_agrees_with_the_committed_artifact_*`: `check`
/// agrees with the committed artifact and stays silent when it does. Spec D7
/// names `check` as the ratchet; the byte ratchet above is what actually
/// gates, so without this the command itself is never exercised.
fn assert_check_agrees(corpus_path: &str) {
    let root = workspace_root();
    let out = Command::new(env!("CARGO_BIN_EXE_hornvale"))
        .args(["tropes", "--corpus", corpus_path, "check"])
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

#[test]
fn check_mode_agrees_with_the_committed_artifact_for_polti() {
    assert_check_agrees("tropes/polti.trope.json");
}

#[test]
fn check_mode_agrees_with_the_committed_artifact_for_tvtropes_2012() {
    assert_check_agrees("tropes/tvtropes-2012.trope.json");
}

/// The `check` half of the same default-corpus invariant covered by
/// `bare_tropes_report_still_defaults_to_polti_and_matches_its_artifact`:
/// the bare `tropes check` (no `--corpus`) must still resolve to Polti and
/// agree with its committed artifact.
#[test]
fn bare_tropes_check_still_defaults_to_polti_and_agrees() {
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

/// `check` must actually FAIL when the report diverges. Every other test here
/// asserts success, so a refactor that made the check arm return `Ok(())`
/// unconditionally would leave all of them green while the command stopped
/// discriminating. Spec §7 asks for the ratchet to be disarmed and shown to
/// redden; without this, that proof exists only in campaign scratch that dies
/// with the worktree.
///
/// The lever is a divergent **corpus**, not a tampered artifact: a corpus that
/// differs from the frozen one renders a different report, which cannot match
/// the committed bytes — so the committed artifact is never written to, and
/// this test cannot leave the tree dirty even if it fails.
///
/// The temp corpus is deliberately given the id `polti-1895` rather than some
/// other id like `divergent`: `artifact_path` derives the committed-artifact
/// path from `corpus.corpus`, so an id with no matching committed file would
/// make `check` fail on a missing file, not on a content mismatch — a weaker
/// test than the one this replaces, and one that would look identical (a red
/// assertion) whichever reason it failed for. Naming the temp corpus
/// `polti-1895` makes it resolve to the real, existing
/// `docs/audits/trope-coverage-polti-1895.md`.
///
/// What actually guarantees the mismatch is the header, not the situations:
/// `render` embeds the invocation path via `regenerate_command(path)`, and
/// the temp file's OS-assigned path (some `/tmp/hv-trope-divergent-<pid>.json`)
/// can never equal `tropes/polti.trope.json` — so line 1 alone diverges from
/// the committed artifact regardless of what the temp corpus's situations
/// say. The deliberately-different situations below add a second, independent
/// source of divergence beneath that header, but the header alone already
/// proves the point.
///
/// Asserts on exit status rather than the drift message's exact wording, so
/// rewording that message does not redden this test — but a bare
/// `!success()` is satisfied by EITHER failure branch in `check`'s arm
/// (content mismatch, or the committed artifact missing entirely), and
/// under path derivation a differently-named temp corpus would hit the
/// latter. The second assertion below pins out that branch by name, so this
/// test fails loudly if it ever starts passing for the wrong reason.
#[test]
fn check_mode_fails_on_a_divergent_corpus() {
    let root = workspace_root();
    // `std::env::temp_dir()`, deliberately not a path built from
    // `CARGO_MANIFEST_DIR` — see `build_path_embedding.rs`. The pid keeps
    // concurrent runs from colliding; nextest is process-per-test.
    let corpus =
        std::env::temp_dir().join(format!("hv-trope-divergent-{}.json", std::process::id()));
    std::fs::write(
        &corpus,
        r#"{"corpus":"polti-1895","provenance":"a deliberately different corpus",
            "frozen":"never","bundles":{},
            "situations":[{"id":"s1","name":"S","actants":{"subject":"someone"},
                           "requires":["predicate:absent"],"excluded_by":[]}]}"#,
    )
    .expect("writes the temp corpus");

    let out = Command::new(env!("CARGO_BIN_EXE_hornvale"))
        .args(["tropes", "--corpus"])
        .arg(&corpus)
        .arg("check")
        .current_dir(&root)
        .output()
        .expect("runs the binary");

    // Clean up before asserting, so a red run still removes the file.
    let _ = std::fs::remove_file(&corpus);

    assert!(
        !out.status.success(),
        "check exited 0 against a corpus that cannot match the committed \
         artifact — the ratchet is not discriminating"
    );
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(
        !stderr.contains("No such file"),
        "check failed because the committed artifact is MISSING, not because its \
         content diverged — this test exists to prove a content mismatch is caught, \
         and a missing-file failure proves nothing about that. stderr: {stderr}"
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

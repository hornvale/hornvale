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
/// nothing exercises `cmd_tropes`'s default (`unwrap_or(tropes::CORPORA[0])`
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

/// A mode following a flag is still found — and found before the flag's value
/// is used. Reading only `args[1]` made `tropes --corpus <path> check` print a
/// report and exit 0, a silent false pass for anything gating on `check`.
///
/// The invocation is `tropes --corpus <a path that does not exist> matrix`,
/// which no other test makes, and which is chosen so this test can fail *by
/// itself*. Pointing it at `--corpus <real corpus> check` — either corpus —
/// made it a byte-identical twin of a `check` agreement test: same argv, same
/// two assertions, so the two could only ever fail together and the
/// flag-parsing fix still had no independent guard. Two things make this form
/// distinct instead:
///
/// - **The discriminator is report-shaped output, not the absence of output.**
///   An empty stdout is what `check` prints on agreement and also what a
///   crashed run prints; a rendered matrix is a shape nothing else in
///   `cmd_tropes` produces. This asserts the bytes are the matrix's, so a
///   fall-through to `report` or `check` cannot satisfy it.
/// - **The corpus is deliberately unreadable.** `cmd_tropes` scans for the
///   mode *before* it reads `--corpus`, precisely so `matrix` — which scores
///   every corpus in `tropes::CORPORA` and has no use for the flag — need not
///   fail because the caller passed a path that does not exist. That ordering
///   is asserted nowhere else, and it is what lets this test redden alone:
///   moving the matrix arm below the corpus read leaves every agreement test
///   and `committed_trope_matrix_matches_the_live_render` (which invokes bare
///   `tropes matrix`) green, and reddens only this one.
///
/// The original regression is still caught: a mode read from a fixed position
/// resolves to `None` here, falls through to `report`, and dies on the
/// unreadable corpus with a non-zero status.
#[test]
fn the_mode_is_found_after_a_flag_and_before_the_flag_is_used() {
    let root = workspace_root();
    let out = Command::new(env!("CARGO_BIN_EXE_hornvale"))
        .args([
            "tropes",
            "--corpus",
            "tropes/no-such-corpus.trope.json",
            "matrix",
        ])
        .current_dir(&root)
        .output()
        .expect("runs the binary");
    assert!(
        out.status.success(),
        "`tropes --corpus <nonexistent> matrix` must not read --corpus at all: the mode \
         is scanned for before the corpus is loaded, so the matrix renders regardless. \
         A non-zero status means the mode after the flag was missed and the run fell \
         through to a corpus-reading arm: {out:?}"
    );
    let stdout = String::from_utf8(out.stdout).expect("utf-8");
    assert!(
        stdout.starts_with("<!-- GENERATED FILE") && stdout.contains("# The trope matrix"),
        "the mode after the flag did not select `matrix`; stdout is not matrix-shaped. \
         First 200 characters: {:?}",
        stdout.chars().take(200).collect::<String>()
    );
    // Anti-vacuity: the marker above is a prefix a truncated render could still
    // carry. A rendered matrix always holds the Columns table it is for.
    assert!(
        !parse_matrix_columns(&stdout).is_empty(),
        "matrix-shaped stdout with no Columns rows — the render is not what this \
         test thinks it is.\n{stdout}"
    );
}

/// The matrix is byte-checked exactly as the columns are.
///
/// The spec asks for the matrix to be drift-checked the way the columns are,
/// and the columns get three layers: this file's golden assertions, `hornvale
/// tropes check`, and CI's regenerate-then-`git diff docs/audits/`. Without
/// this test the matrix had only the third — the slowest, and the one a
/// developer running `cargo test --workspace` never sees.
///
/// This replaces a substring test that asserted the matrix *contained* each
/// corpus id and each denominator. That test could not fail for the reason
/// its own docstring claimed: `contains("409")` is satisfied by the
/// provenance prose and by every `(217/409)` cell, so a matrix rendering
/// `| polti-1895 | 0 of 35 |` would still have passed it. A byte comparison
/// is the same check the columns get, and it cannot be satisfied by
/// coincidence.
///
/// Read a red result the same way: it says the matrix moved, not which figure
/// moved. Regenerate deliberately with `make rebaseline` and read the diff —
/// a share that changed means a corpus's demand changed, which is a finding.
#[test]
fn committed_trope_matrix_matches_the_live_render() {
    let root = workspace_root();
    let out = Command::new(env!("CARGO_BIN_EXE_hornvale"))
        .args(["tropes", "matrix"])
        .current_dir(&root)
        .output()
        .expect("runs the binary");
    assert!(out.status.success(), "tropes matrix failed: {out:?}");
    let live = String::from_utf8(out.stdout).expect("utf-8");
    hornvale_kernel::golden::assert_golden(
        &root.join("docs/audits/trope-matrix.md"),
        &live,
        "the trope matrix drifted from the committed artifact. Regenerate deliberately \
         with `make rebaseline` and review the diff — the matrix summarises both columns, \
         so a figure that moved here without either column moving means the two \
         derivations have come apart, which is exactly what this artifact exists to catch",
    );
}

/// The `## Columns` table's rows, as `(corpus id, the report link's href)`.
///
/// Scoped to that section rather than matched across the whole document: the
/// Demand table below renders a backticked first cell too, and a bundle row
/// must never be counted as a column.
fn parse_matrix_columns(matrix: &str) -> Vec<(String, String)> {
    matrix
        .lines()
        .skip_while(|l| !l.starts_with("## Columns"))
        .skip(1)
        .take_while(|l| !l.starts_with("## "))
        .filter(|l| l.starts_with("| `"))
        .filter_map(|l| {
            let cells: Vec<&str> = l.split('|').map(str::trim).collect();
            let id = cells.get(1)?.trim_matches('`').to_string();
            let href = cells.get(4)?.rsplit_once("](")?.1.strip_suffix(')')?;
            Some((id, href.to_string()))
        })
        .collect()
}

/// Every corpus the matrix declares as a column has a committed report to
/// point at.
///
/// The matrix links each column to `./trope-coverage-<id>.md`. Adding a path
/// to `tropes::CORPORA` without adding a line to
/// `scripts/regenerate-artifacts.sh` renders a column whose link is dead, and
/// nothing else in the workspace would notice: the golden tests only cover the
/// corpora they name, and CI's drift check compares files that exist.
/// `every_matrix_row_carries_its_own_column_s_headline_figures` walks the
/// other direction — committed artifact to matrix row — so a declared corpus
/// with no artifact is precisely the case it cannot see.
///
/// `CORPORA` is reached through the rendered matrix rather than imported.
/// `hornvale` is a binary-only crate, so an integration test cannot name the
/// constant; but the Columns table is rendered *from* it, one row per entry,
/// so parsing those rows out of a live run binds the declared list to the
/// committed artifacts just as tightly. It also binds the **rendered href**
/// rather than re-deriving `artifact_path`'s convention here, so a change to
/// how the link is spelled cannot leave this test checking a path the document
/// no longer contains. A declared corpus whose *file* is missing fails earlier
/// still: `tropes matrix` cannot render at all, and the status assertion
/// catches it.
///
/// This lived in `cli/src/tropes.rs` as a unit test, where reaching the
/// workspace root required `env!("CARGO_MANIFEST_DIR")` inside production
/// source — see `build_path_embedding.rs` for what that costs decision 0090's
/// cross-host oracle. Here the same `env!` is free: this binary never ships.
#[test]
fn every_declared_matrix_column_links_to_a_committed_report() {
    let root = workspace_root();
    let out = Command::new(env!("CARGO_BIN_EXE_hornvale"))
        .args(["tropes", "matrix"])
        .current_dir(&root)
        .output()
        .expect("runs the binary");
    assert!(
        out.status.success(),
        "tropes matrix failed — a corpus declared in CORPORA may be unreadable: {out:?}"
    );
    let matrix = String::from_utf8(out.stdout).expect("utf-8");

    let columns = parse_matrix_columns(&matrix);
    assert!(
        columns.len() >= 2,
        "the matrix's Columns table parsed as {} row(s); two catalogues are declared \
         today, so this test just stopped checking anything. The table's shape moved — \
         fix the parse rather than letting it pass vacuously.\n{matrix}",
        columns.len()
    );

    for (id, href) in &columns {
        let target = root.join("docs/audits").join(href.trim_start_matches("./"));
        assert!(
            target.is_file(),
            "the matrix renders a column for `{id}` linking to `{href}`, but \
             {} does not exist — the committed matrix would carry a dead link. \
             Add a line to scripts/regenerate-artifacts.sh and regenerate.",
            target.display()
        );
    }
}

/// Every corpus in `tropes/` identifies its situations uniquely, and every
/// committed column is one of them.
///
/// `resolve` keys a `BTreeMap` by `id`, so a copy-pasted id silently drops a
/// situation: the report prints a quietly smaller denominator, and every
/// matrix share divides a numerator counted over `corpus.situations` by a
/// denominator of `out.len()` — which is how a duplicate renders a share above
/// 100%. The two hand-written per-corpus tests in `cli/src/tropes.rs` assert
/// uniqueness beside a frozen count and they stay; the count is the
/// deliberate-freeze ratchet. This is the generic backstop, so that a third
/// catalogue is covered the day its file lands rather than waiting for someone
/// to remember to hand-write a third test.
///
/// The sweep is over `tropes/*.trope.json` — a superset of what is declared —
/// and the second assertion is what keeps that from being a weaker check than
/// looping over `CORPORA`: every committed column must be backed by a swept
/// file, so a declared corpus living outside `tropes/` fails loudly here
/// rather than being skipped silently.
#[test]
fn every_corpus_file_identifies_its_situations_uniquely() {
    let root = workspace_root();
    let mut corpus_ids = std::collections::BTreeSet::new();
    let mut files: Vec<std::path::PathBuf> = std::fs::read_dir(root.join("tropes"))
        .expect("tropes/ is readable")
        .map(|e| e.expect("readable directory entry").path())
        .filter(|p| p.to_string_lossy().ends_with(".trope.json"))
        .collect();
    files.sort();
    assert!(
        files.len() >= 2,
        "found {} corpus file(s) under tropes/; two are committed today, so this \
         sweep just stopped checking anything",
        files.len()
    );

    for path in &files {
        let text = std::fs::read_to_string(path).expect("the corpus file is readable");
        let json: serde_json::Value = serde_json::from_str(&text)
            .unwrap_or_else(|e| panic!("{} is not JSON: {e}", path.display()));
        let id = json["corpus"]
            .as_str()
            .unwrap_or_else(|| panic!("{} declares no `corpus` id", path.display()));
        let situations = json["situations"]
            .as_array()
            .unwrap_or_else(|| panic!("{} declares no `situations` array", path.display()));
        assert!(
            !situations.is_empty(),
            "{} declares an empty corpus",
            path.display()
        );
        let mut seen = std::collections::BTreeSet::new();
        for st in situations {
            let sid = st["id"]
                .as_str()
                .unwrap_or_else(|| panic!("{} has a situation with no `id`", path.display()));
            assert!(
                seen.insert(sid),
                "`{id}` declares situation id `{sid}` twice — `resolve` keys a BTreeMap by \
                 id, so one situation would vanish, the report would understate its \
                 denominator, and a matrix share counted over the corpus but divided by \
                 that denominator would render above 100%"
            );
        }
        corpus_ids.insert(id.to_string());
    }

    for entry in std::fs::read_dir(root.join("docs/audits")).expect("docs/audits is readable") {
        let path = entry.expect("readable directory entry").path();
        let Some(name) = path.file_name().and_then(|n| n.to_str()) else {
            continue;
        };
        let Some(id) = name
            .strip_prefix("trope-coverage-")
            .and_then(|n| n.strip_suffix(".md"))
        else {
            continue;
        };
        assert!(
            corpus_ids.contains(id),
            "`docs/audits/{name}` is a committed column for `{id}`, but no file under \
             tropes/ declares that corpus id — so the uniqueness sweep above never \
             covered it. Found: {corpus_ids:?}"
        );
    }
}

/// A report's headline, parsed out of `Stageable {s} of {t} ({i}
/// inapplicable).` as `(stageable, total, inapplicable)`.
///
/// `None` rather than a default on any failure, so a caller can distinguish
/// "the column says different numbers" from "the column no longer says this
/// at all" and fail on both.
fn parse_report_headline(text: &str) -> Option<(usize, usize, usize)> {
    let line = text.lines().find(|l| l.starts_with("Stageable "))?;
    let rest = line.strip_prefix("Stageable ")?;
    let (stageable, rest) = rest.split_once(" of ")?;
    let (total, rest) = rest.split_once(" (")?;
    let inapplicable = rest.strip_suffix(" inapplicable).")?;
    Some((
        stageable.parse().ok()?,
        total.parse().ok()?,
        inapplicable.parse().ok()?,
    ))
}

/// One corpus's row in the matrix's Columns table, as the same triple.
///
/// The row is `| `<id>` | {s} of {t} | {i} | [report](…) |`.
fn parse_matrix_row(matrix: &str, id: &str) -> Option<(usize, usize, usize)> {
    let prefix = format!("| `{id}` |");
    let line = matrix.lines().find(|l| l.starts_with(&prefix))?;
    let cells: Vec<&str> = line.split('|').map(str::trim).collect();
    let (stageable, total) = cells.get(2)?.split_once(" of ")?;
    Some((
        stageable.parse().ok()?,
        total.parse().ok()?,
        cells.get(3)?.parse().ok()?,
    ))
}

/// The spec's named matrix validation: "the matrix's per-column figures equal
/// the per-corpus reports' own."
///
/// The byte check above pins the matrix to *itself*. It cannot see the failure
/// this one exists for, because the two documents are rendered by two callers
/// — `render` and `render_matrix` — and if their figures ever came apart,
/// `make rebaseline` would re-accept every golden in one pass and leave two
/// committed documents stating different numbers with nothing red. So this
/// compares the two derivations against each other and not against a stored
/// expectation.
///
/// Both sides are **parsed**, never transcribed. Writing `0 of 36` here would
/// make this a third place a figure is hand-copied — the very thing the
/// campaign built a generated matrix to avoid — and it would pass unchanged
/// while both real documents moved together. Nothing below asserts what the
/// numbers are; the assertion is only that the two documents agree.
///
/// The columns are read from their committed bytes and the matrix from a live
/// run. Those committed bytes are themselves pinned to live `render` output by
/// the golden tests at the top of this file, so this is still a comparison of
/// two derivations — but the asymmetry means a divergence introduced into
/// `render_matrix` reddens here *before* anyone rebaselines, not only after.
///
/// Columns are discovered by scanning `docs/audits/` rather than listed, so a
/// third catalogue is covered the day its artifact is committed. If that scan
/// or either parse comes up empty the test panics rather than passing
/// vacuously: a reformatted headline must not silently turn this into an
/// assertion about nothing.
#[test]
fn every_matrix_row_carries_its_own_column_s_headline_figures() {
    let root = workspace_root();
    let out = Command::new(env!("CARGO_BIN_EXE_hornvale"))
        .args(["tropes", "matrix"])
        .current_dir(&root)
        .output()
        .expect("runs the binary");
    assert!(out.status.success(), "tropes matrix failed: {out:?}");
    let matrix = String::from_utf8(out.stdout).expect("utf-8");

    let audits = root.join("docs/audits");
    let mut columns: Vec<(String, std::path::PathBuf)> = std::fs::read_dir(&audits)
        .expect("docs/audits is readable")
        .map(|e| e.expect("readable directory entry").path())
        .filter_map(|path| {
            let name = path.file_name()?.to_str()?;
            let id = name
                .strip_prefix("trope-coverage-")?
                .strip_suffix(".md")?
                .to_string();
            Some((id, path))
        })
        .collect();
    columns.sort();
    assert!(
        !columns.is_empty(),
        "no committed column matched docs/audits/trope-coverage-*.md, so this test \
         compared nothing — the artifacts moved or were renamed"
    );

    for (id, path) in &columns {
        let text = std::fs::read_to_string(path).expect("the committed column is readable");
        let column = parse_report_headline(&text).unwrap_or_else(|| {
            panic!(
                "no `Stageable {{s}} of {{t}} ({{i}} inapplicable).` line in {}. The column's \
                 headline was reformatted; update this parse rather than letting the \
                 cross-check quietly stop running",
                path.display()
            )
        });
        let row = parse_matrix_row(&matrix, id).unwrap_or_else(|| {
            panic!(
                "the matrix has no parseable Columns row for `{id}`, but \
                 docs/audits/trope-coverage-{id}.md exists. Either the corpus is committed \
                 without being declared in CORPORA, or the row's shape changed.\n{matrix}"
            )
        });
        assert_eq!(
            column, row,
            "`{id}`: its report says (stageable, total, inapplicable) = {column:?} and the \
             matrix row says {row:?}. `render` and `render_matrix` have come apart — do NOT \
             rebaseline, because rebaselining accepts both documents and preserves the \
             disagreement"
        );
    }
}

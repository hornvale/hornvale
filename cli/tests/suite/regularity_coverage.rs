//! The committed regularity-coverage artifact, and the readings it carries.
//!
//! The drift test is the load-bearing one; the rest pin the four readings
//! Task 2's review established, each against a property a reader could check
//! by hand rather than against a transcribed number.

use hornvale::regularities::{self, Verdict};
use std::path::PathBuf;
use std::process::Command;

fn workspace_root() -> PathBuf {
    std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("workspace root")
        .to_path_buf()
}

fn load_sugarscape() -> regularities::Corpus {
    let path = workspace_root().join(regularities::CORPORA[0]);
    let json = std::fs::read_to_string(&path).expect("corpus file");
    regularities::load(&json).expect("corpus parses")
}

/// Collapse whitespace, so an assertion about the report's PROSE is not
/// broken by where the 76-column wrapper happened to put a newline.
fn flat(s: &str) -> String {
    s.split_whitespace().collect::<Vec<_>>().join(" ")
}

fn census() -> hornvale_lab::domesday::census::Census {
    let dir = workspace_root()
        .join(hornvale_lab::CENSUS_GOLDENS_DIR)
        .join("the-census");
    hornvale_lab::domesday::census::load(&dir).expect("committed census loads")
}

#[test]
fn committed_regularity_coverage_matches_the_live_report() {
    let corpus = load_sugarscape();
    let live = regularities::render(&corpus, &census(), regularities::CORPORA[0]);
    let path = workspace_root().join(regularities::artifact_path(&corpus));
    let committed = std::fs::read_to_string(&path).unwrap_or_default();
    assert_eq!(
        committed,
        live,
        "committed report is stale — regenerate with `{}`",
        // The CORPUS path, not the artifact path: `regenerate_command` prints
        // a `--corpus` argument, and handing it the artifact would print a
        // command asking the resolver to parse its own output as a corpus.
        regularities::regenerate_command(regularities::CORPORA[0])
    );
}

#[test]
fn the_artifact_path_is_derived_from_the_corpus_id() {
    let corpus = load_sugarscape();
    assert_eq!(
        regularities::artifact_path(&corpus),
        "docs/audits/regularity-coverage-sugarscape-1996.md"
    );
}

/// The banner must name a command that regenerates the file it heads. A
/// header naming the artifact path would be a command nobody can run.
#[test]
fn the_regenerate_command_names_the_corpus_not_the_artifact() {
    let cmd = regularities::regenerate_command(regularities::CORPORA[0]);
    assert!(cmd.contains(regularities::CORPORA[0]), "{cmd}");
    assert!(!cmd.contains("docs/audits/"), "{cmd}");
    let corpus = load_sugarscape();
    let report = regularities::render(&corpus, &census(), regularities::CORPORA[0]);
    assert!(report.starts_with(&format!(
        "<!-- GENERATED FILE — do not edit. Regenerate with `{cmd}`. -->"
    )));
}

/// Requirement 1 and 2 together: the independent-claim count is reported
/// beside the item count, and it is STRICTLY BELOW it — the two raid-rate
/// items read near-collinear statistics off the same population, so four
/// measurable items are not four claims.
#[test]
fn the_report_states_fewer_claims_than_measurable_items() {
    let corpus = load_sugarscape();
    let report = regularities::render(&corpus, &census(), regularities::CORPORA[0]);
    let measurable = corpus
        .items
        .iter()
        .filter(|i| i.criterion.is_some() && !i.statistic.is_empty())
        .count();
    let prose = flat(&report);
    assert!(
        prose.contains(&format!("proposes {measurable} measurable item(s)")),
        "the item count must be printed: {report}"
    );
    // The claim count is whatever the census says today. Asserting the
    // PROPERTY (fewer claims than items) rather than a transcribed number
    // keeps this from silently changing meaning at the next census refresh,
    // while still failing if the collinearity merge stops happening.
    let claims: Vec<usize> = (1..measurable)
        .filter(|n| prose.contains(&format!("they make {n} independent claim(s)")))
        .collect();
    assert_eq!(
        claims.len(),
        1,
        "exactly one independent-claim count below {measurable} must be printed: {report}"
    );
}

/// Requirement 2: the correlation is MEASURED on the scored population, not
/// quoted from the metric rustdoc's 12-world scratch probe. A measured pair
/// prints its own paired count, and that count is the census's world count —
/// which a quoted figure could not be.
#[test]
fn the_collinearity_is_measured_on_the_scored_population() {
    let corpus = load_sugarscape();
    let c = census();
    let worlds = c.rows.len();
    let report = regularities::render(&corpus, &c, regularities::CORPORA[0]);
    assert!(
        report.contains(&format!("(n = {worlds})")),
        "a correlation must be reported over the census's own {worlds} worlds: {report}"
    );
    // Scoped to the report's OWN VOICE. The Items table reproduces every
    // note verbatim, and `sug-retaliation-deters`'s note is where the
    // inherited 1.00-1.03 figure is recorded — that is the corpus speaking
    // about its own weakness, which must survive. What must not happen is the
    // REPORT restating it as if it were this population's measurement.
    let voice = &report[..report.find("## Items").expect("an items table")];
    assert!(
        !voice.contains("1.00-1.03") && !voice.contains("1.00–1.03"),
        "the inherited 12-world figure must not be quoted in the report's own \
         voice: {voice}"
    );
}

/// Requirement 3: `absent` splits into roadmap and gap, both halves non-empty,
/// and every roadmap item named with the instrument it declares. A degenerate
/// split (all one side) would read as a distinction while carrying none.
#[test]
fn the_absent_verdict_is_split_into_roadmap_and_gap() {
    let corpus = load_sugarscape();
    let report = regularities::render(&corpus, &census(), regularities::CORPORA[0]);
    let absent: Vec<&regularities::Item> = corpus
        .items
        .iter()
        .filter(|i| i.verdict == Verdict::Absent)
        .collect();
    let roadmap: Vec<&&regularities::Item> = absent
        .iter()
        .filter(|i| i.roadmap_instrument.is_some())
        .collect();
    assert!(
        !roadmap.is_empty() && roadmap.len() < absent.len(),
        "the split must be non-degenerate: {} roadmap of {} absent",
        roadmap.len(),
        absent.len()
    );
    assert!(report.contains(&format!(
        "- roadmap (the item declares the instrument): {}\n",
        roadmap.len()
    )));
    assert!(report.contains(&format!(
        "- gap (the mechanism is missing): {}\n",
        absent.len() - roadmap.len()
    )));
    for item in &roadmap {
        assert!(
            report.contains(&format!("- `{}` —", item.id)),
            "roadmap item {} must be named in the report",
            item.id
        );
        let instrument = item.roadmap_instrument.as_deref().expect("declared");
        assert!(
            report.contains(&format!("  - instrument: {instrument}\n")),
            "roadmap item {} must print the instrument it declares",
            item.id
        );
    }
    // The three the campaign spec names by hand must be on the roadmap side.
    for id in [
        "sug-seasonal-phase-lock",
        "sug-externality-displaces",
        "sug-heterogeneous-landscape",
    ] {
        assert!(
            roadmap.iter().any(|i| i.id == id),
            "{id} must classify as roadmap"
        );
    }
}

/// The classification is DECLARED DATA, not inferred wording. A note may say
/// "the discriminating instrument is …" and still be a gap, or say nothing of
/// the sort and still be roadmap — because the field decides. Pinning that
/// independence is what stops the prose classifier from creeping back.
#[test]
fn the_roadmap_split_is_not_a_function_of_note_wording() {
    let corpus = load_sugarscape();
    let phrase_bearing: Vec<&str> = corpus
        .items
        .iter()
        .filter(|i| i.verdict == Verdict::Absent)
        .filter(|i| i.note.contains("instrument is") || i.note.contains("criterion is known"))
        .map(|i| i.id.as_str())
        .collect();
    let declared: Vec<&str> = corpus
        .items
        .iter()
        .filter(|i| i.roadmap_instrument.is_some())
        .map(|i| i.id.as_str())
        .collect();
    // `sug-spatial-segregation` is the live witness: it names three concrete
    // instruments and carries neither marker phrase, so the two sets differ.
    // If they ever coincide, the test below is the only thing left saying the
    // classification is not prose-derived.
    assert_ne!(
        phrase_bearing, declared,
        "the declared set must not be reproducible from note phrases alone — if it \
         becomes so, the independence of the field from the prose stops being observable"
    );
    assert!(
        declared.contains(&"sug-spatial-segregation"),
        "the item that names instruments without the marker phrase must be roadmap"
    );
}

/// The freeze extends to the new field, in both directions: every `absent`
/// item DECLARES it (explicitly, as a string or a `null`), and no other
/// verdict carries it at all. Presence is read from the raw JSON, because
/// `Option<String>` cannot tell an authored `null` from an omitted key — and
/// an omission is exactly the way an item would slip past the declaration.
#[test]
fn every_absent_item_declares_the_field_and_no_other_item_carries_it() {
    let path = workspace_root().join(regularities::CORPORA[0]);
    let raw: serde_json::Value =
        serde_json::from_str(&std::fs::read_to_string(&path).expect("corpus file"))
            .expect("corpus is JSON");
    let items = raw["items"].as_array().expect("items is an array");
    let mut missing = Vec::new();
    let mut stray = Vec::new();
    for item in items {
        let id = item["id"].as_str().expect("an id");
        let absent = item["verdict"] == serde_json::json!("absent");
        let declared = item.get("roadmap_instrument").is_some();
        if absent && !declared {
            missing.push(id.to_string());
        }
        if !absent && declared {
            stray.push(id.to_string());
        }
    }
    assert!(
        missing.is_empty(),
        "every `absent` item must declare `roadmap_instrument` explicitly (a string, or \
         `null` where no instrument is known): {missing:?}"
    );
    assert!(
        stray.is_empty(),
        "`roadmap_instrument` is meaningful for `absent` only — a `deferred` item's \
         blocker is its registry row, and `refused`/`inapplicable` ask no instrument \
         question: {stray:?}"
    );
}

/// `note` is unparsed again, and this pins it. Rewording any note must not
/// move the roadmap/gap split — the defect the structured field was added to
/// close, where a committed artifact's headline was a function of prose.
#[test]
fn rewording_a_note_cannot_move_the_split() {
    let path = workspace_root().join(regularities::CORPORA[0]);
    let json = std::fs::read_to_string(&path).expect("corpus file");
    let before = regularities::load(&json).expect("parses");
    let split = |c: &regularities::Corpus| {
        c.items
            .iter()
            .filter(|i| i.verdict == Verdict::Absent && i.roadmap_instrument.is_some())
            .count()
    };
    // Strip every marker phrase the retired prose classifier looked for. Under
    // that classifier this collapses the roadmap half to the one item whose
    // note never carried a phrase; under the field it must not move at all.
    let reworded = json
        .replace("instrument is", "thing to build is")
        .replace("criterion is known", "criterion has been worked out");
    let after = regularities::load(&reworded).expect("reworded corpus still parses");
    assert_eq!(
        split(&before),
        split(&after),
        "the roadmap split must not depend on note wording"
    );
    assert!(
        split(&before) > 1,
        "the fixture must be able to detect a collapse"
    );
}

/// Requirement 4: the power caveat is in the report's own text, and it sits
/// ABOVE the tally. A qualification a reader meets after the score is a
/// footnote; this one has to be on the way in.
#[test]
fn the_power_caveat_precedes_the_tally() {
    let corpus = load_sugarscape();
    let report = regularities::render(&corpus, &census(), regularities::CORPORA[0]);
    let power = report.find("## Power").expect("a power section");
    let tally = report.find("## Tally").expect("a tally section");
    assert!(power < tally, "the power reading must precede the tally");
    assert!(
        report.contains("is NOT evidence of reach"),
        "the caveat must be stated, not implied: {report}"
    );
}

/// The instrument disclaimer decision 0095 requires, in the same voice
/// `docs/audits/trope-matrix.md` carries it.
#[test]
fn the_report_opens_with_the_instrument_disclaimer() {
    let corpus = load_sugarscape();
    let report = regularities::render(&corpus, &census(), regularities::CORPORA[0]);
    let reading = report.find("## Reading this report").expect("a preamble");
    let tally = report.find("## Tally").expect("a tally section");
    assert!(reading < tally);
    assert!(report.contains("never a grade"), "{report}");
    assert!(report.contains("decision 0095"), "{report}");
}

/// `emergence_type` is `Option<u8>`, and a `None` is a third answer rather
/// than a missing one: an item where the taxonomy does not apply is excluded
/// from the split, never counted into a type. The two type counts plus the
/// excluded count must therefore exhaust the corpus.
#[test]
fn the_emergence_split_excludes_items_the_taxonomy_does_not_reach() {
    let corpus = load_sugarscape();
    let report = regularities::render(&corpus, &census(), regularities::CORPORA[0]);
    let held = |t: u8| {
        corpus
            .items
            .iter()
            .filter(|i| i.emergence_type == Some(t))
            .count()
    };
    let untyped = corpus
        .items
        .iter()
        .filter(|i| i.emergence_type.is_none())
        .count();
    assert_eq!(
        held(1) + held(2) + untyped,
        corpus.items.len(),
        "the split plus the exclusions must exhaust the corpus"
    );
    assert!(untyped > 0, "this corpus has untyped items to exclude");
    for t in [1u8, 2u8] {
        let grown = corpus
            .items
            .iter()
            .filter(|i| i.emergence_type == Some(t) && i.verdict == Verdict::Grown)
            .count();
        assert!(report.contains(&format!("- type {t}: {} held, {grown} grown\n", held(t))));
    }
    assert!(report.contains(&format!(
        "- taxonomy does not apply: {untyped} (excluded from the split above)\n"
    )));
}

/// `unmeasured` is a lifecycle state, not a coverage verdict, so it is listed
/// by name as well as tallied. When Task 7 measures the corpus this section
/// reports its own emptiness rather than disappearing.
#[test]
fn unmeasured_items_are_listed_separately() {
    let corpus = load_sugarscape();
    let report = regularities::render(&corpus, &census(), regularities::CORPORA[0]);
    let pending: Vec<&str> = corpus
        .items
        .iter()
        .filter(|i| i.verdict == Verdict::Unmeasured)
        .map(|i| i.id.as_str())
        .collect();
    assert!(report.contains("## Unmeasured"));
    if pending.is_empty() {
        assert!(report.contains("None — every item carries a coverage verdict."));
    } else {
        for id in &pending {
            assert!(
                report.contains(&format!("- `{id}` —")),
                "{id} must be listed"
            );
        }
    }
}

/// The artifact is declared in `docs/generated-paths.txt` BY NAME, not only by
/// inheriting `docs/audits/`. `git diff --exit-code` is silently vacuous
/// against a path with no index entry, and a new file dropped into an
/// already-declared directory inherits that vacuity in full.
#[test]
fn the_artifact_is_declared_by_name() {
    let declared = std::fs::read_to_string(workspace_root().join("docs/generated-paths.txt"))
        .expect("generated paths");
    let corpus = load_sugarscape();
    let path = regularities::artifact_path(&corpus);
    assert!(
        declared
            .lines()
            .any(|l| l.split('\t').next() == Some(path.as_str())),
        "{path} must be declared by name in docs/generated-paths.txt"
    );
}

/// The regeneration must live in the script, because the command prints to
/// stdout and the `>` redirect is what writes the file.
#[test]
fn the_regeneration_is_wired_into_the_artifacts_script() {
    let script = std::fs::read_to_string(workspace_root().join("scripts/regenerate-artifacts.sh"))
        .expect("regenerate-artifacts.sh");
    let corpus = load_sugarscape();
    assert!(
        script.contains(&format!(
            "regularities report > {}",
            regularities::artifact_path(&corpus)
        )),
        "the redirect must be in the script, not in the command"
    );
}

// --- The CLI verb, end to end -----------------------------------------------
//
// Everything above exercises `render` as a library call. These run the real
// binary, because the parts most likely to be silently wrong live in
// `cmd_regularities` and nowhere else: the flag-then-mode scan, `check`'s two
// failure arms, `load_census`, and the unknown-mode error. Modelled on
// `cli/tests/suite/system_coverage.rs`, which does the same for the sibling
// verb.

/// A scratch corpus derived from the real one, written to a unique path.
///
/// Named by process id like `system_coverage`'s, so parallel test binaries
/// cannot collide, and removed by its caller after the run.
fn scratch_corpus(tag: &str, edit: impl FnOnce(&mut serde_json::Value)) -> PathBuf {
    let json = std::fs::read_to_string(workspace_root().join(regularities::CORPORA[0]))
        .expect("reads the real corpus");
    let mut corpus: serde_json::Value = serde_json::from_str(&json).expect("parses as JSON");
    edit(&mut corpus);
    let path = std::env::temp_dir().join(format!(
        "hv-regularity-{tag}-{}.regularity.json",
        std::process::id()
    ));
    std::fs::write(&path, serde_json::to_string(&corpus).expect("serializes"))
        .expect("writes the scratch corpus");
    path
}

fn run(args: &[&str]) -> std::process::Output {
    Command::new(env!("CARGO_BIN_EXE_hornvale"))
        .args(args)
        .current_dir(workspace_root())
        .output()
        .expect("runs the binary")
}

/// The committed artifact against the real binary's stdout — the same ratchet
/// the library-level drift test makes, taken through the path `make
/// rebaseline` actually uses. A library test cannot see a broken dispatch arm,
/// a wrong default corpus, or a `load_census` that reads the wrong directory.
#[test]
fn the_binary_report_matches_the_committed_artifact() {
    let out = run(&["regularities", "report"]);
    assert!(out.status.success(), "regularities report failed: {out:?}");
    let live = String::from_utf8(out.stdout).expect("utf-8");
    let corpus = load_sugarscape();
    let committed =
        std::fs::read_to_string(workspace_root().join(regularities::artifact_path(&corpus)))
            .expect("the committed report is readable");
    assert_eq!(
        live, committed,
        "the binary's report drifted from the committed artifact — regenerate with \
         `make rebaseline` and read the diff"
    );
}

/// `check` against the real corpus and the committed artifact: green.
#[test]
fn the_binary_check_passes_against_the_committed_state() {
    let out = run(&["regularities", "check"]);
    assert!(
        out.status.success(),
        "regularities check must pass on a clean tree: {}",
        String::from_utf8_lossy(&out.stderr)
    );
}

/// **The flag-then-mode scan, which is the one this test exists for.** A
/// naive `args.get(1)` reads `--corpus` as the mode, falls into the
/// `report` arm, prints and exits 0 — so `regularities --corpus X check`
/// would be a FALSE PASS for anything gating on `check`. The corpus here is
/// deliberately broken (an `absent` item carrying an anchor, which the audit
/// refuses), so a `check` that really ran must fail.
#[test]
fn a_flag_before_the_mode_still_reaches_check() {
    let scratch = scratch_corpus("flagscan", |c| {
        let items = c["items"].as_array_mut().expect("items is an array");
        for item in items.iter_mut() {
            if item["verdict"] == serde_json::json!("absent") {
                item["anchor"] = serde_json::json!("decision:0135");
                break;
            }
        }
    });
    let out = run(&[
        "regularities",
        "--corpus",
        scratch.to_str().expect("utf-8 path"),
        "check",
    ]);
    let _ = std::fs::remove_file(&scratch);
    assert!(
        !out.status.success(),
        "a `--corpus X check` that silently reported instead would exit 0 — the \
         false-pass shape this scan exists to prevent"
    );
    let stderr = String::from_utf8(out.stderr).expect("utf-8");
    assert!(
        stderr.contains("regularity coverage audit found"),
        "expected the audit-findings arm, got: {stderr}"
    );
}

/// `check`'s OTHER failure arm: the audit is clean but the rendered report no
/// longer matches the committed artifact. Exercised by moving an item's title,
/// which changes the rendered bytes and nothing the audit inspects.
#[test]
fn the_binary_check_fails_on_drift_with_a_clean_audit() {
    let scratch = scratch_corpus("drift", |c| {
        c["items"][0]["title"] = serde_json::json!("A title nobody committed");
    });
    let out = run(&[
        "regularities",
        "--corpus",
        scratch.to_str().expect("utf-8 path"),
        "check",
    ]);
    let _ = std::fs::remove_file(&scratch);
    assert!(!out.status.success(), "check must fail on drift");
    let stderr = String::from_utf8(out.stderr).expect("utf-8");
    assert!(
        stderr.contains("drifted") && stderr.contains("make rebaseline"),
        "expected the drift arm naming its repair, got: {stderr}"
    );
    assert!(
        !stderr.contains("audit found"),
        "a title change must not raise an audit finding: {stderr}"
    );
}

/// An unknown mode is refused by name rather than silently reporting.
#[test]
fn an_unknown_mode_is_refused() {
    let out = run(&["regularities", "matrix"]);
    assert!(
        !out.status.success(),
        "an unimplemented mode must not fall through to `report`"
    );
    let stderr = String::from_utf8(out.stderr).expect("utf-8");
    assert!(
        stderr.contains("unknown mode 'matrix'") && stderr.contains("report|check"),
        "the refusal must name the mode and the alternatives: {stderr}"
    );
}

/// A corpus path that does not exist fails with the path in the message,
/// rather than falling back to the default corpus.
#[test]
fn a_missing_corpus_is_refused_by_name() {
    let out = run(&[
        "regularities",
        "--corpus",
        "regularities/nope.json",
        "report",
    ]);
    assert!(!out.status.success(), "a missing corpus must not succeed");
    let stderr = String::from_utf8(out.stderr).expect("utf-8");
    assert!(
        stderr.contains("regularities/nope.json"),
        "the refusal must name the path it could not read: {stderr}"
    );
}

/// `load_census` resolves the study the CORPUS names, not a hard-coded one.
/// A corpus naming a study with no committed goldens must fail rather than
/// silently scoring against `the-census`.
#[test]
fn the_population_comes_from_the_corpus_not_a_hard_coded_study() {
    let scratch = scratch_corpus("population", |c| {
        c["population"] = serde_json::json!("a-study-that-does-not-exist");
    });
    let out = run(&[
        "regularities",
        "--corpus",
        scratch.to_str().expect("utf-8 path"),
        "report",
    ]);
    let _ = std::fs::remove_file(&scratch);
    assert!(
        !out.status.success(),
        "a corpus naming an absent population must fail, not fall back to `the-census`"
    );
}

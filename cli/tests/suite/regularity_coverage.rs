//! The committed regularity-coverage artifact, and the readings it carries.
//!
//! The drift test is the load-bearing one; the rest pin the four readings
//! Task 2's review established, each against a property a reader could check
//! by hand rather than against a transcribed number.

use hornvale::regularities::{self, Verdict};
use std::path::PathBuf;

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
/// and every roadmap item named. A degenerate split (all one side) would read
/// as a distinction while carrying none.
#[test]
fn the_absent_verdict_is_split_into_roadmap_and_gap() {
    let corpus = load_sugarscape();
    let report = regularities::render(&corpus, &census(), regularities::CORPORA[0]);
    let absent: Vec<&str> = corpus
        .items
        .iter()
        .filter(|i| i.verdict == Verdict::Absent)
        .map(|i| i.id.as_str())
        .collect();
    let roadmap: Vec<&str> = corpus
        .items
        .iter()
        .filter(|i| {
            i.verdict == Verdict::Absent
                && (i.note.contains("instrument is") || i.note.contains("criterion is known"))
        })
        .map(|i| i.id.as_str())
        .collect();
    assert!(
        !roadmap.is_empty() && roadmap.len() < absent.len(),
        "the split must be non-degenerate: {} roadmap of {} absent",
        roadmap.len(),
        absent.len()
    );
    assert!(report.contains(&format!(
        "- roadmap (the note names the instrument): {}\n",
        roadmap.len()
    )));
    assert!(report.contains(&format!(
        "- gap (the mechanism is missing): {}\n",
        absent.len() - roadmap.len()
    )));
    for id in &roadmap {
        assert!(
            report.contains(&format!("- `{id}` —")),
            "roadmap item {id} must be named in the report"
        );
    }
    // The three the campaign spec names by hand must be on the roadmap side —
    // the classifier is read off prose, so this pins that it still reaches
    // them.
    for id in [
        "sug-seasonal-phase-lock",
        "sug-externality-displaces",
        "sug-heterogeneous-landscape",
    ] {
        assert!(roadmap.contains(&id), "{id} must classify as roadmap");
    }
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

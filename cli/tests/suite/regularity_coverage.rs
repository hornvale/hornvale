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

/// The tally reports a CLAIM-LEVEL grown count, and it is strictly below the
/// item-level one.
///
/// `grown: 3` and `3 independent claim(s)` are the same numeral meaning two
/// different things, paragraphs apart, and a reader pairs them into "every
/// claim grew". Two did: the near-collinear raid pair is one claim counted
/// twice in the item tally.
///
/// Asserts the PROPERTY — claim-grown strictly below item-grown, and the
/// claim denominator strictly below the item count — rather than transcribing
/// today's `2 of 3`. A transcribed pair would have to be re-typed at the next
/// census refresh, which is how a derived number quietly becomes a
/// hard-coded one.
#[test]
fn the_tally_reports_grown_by_claim_not_only_by_item() {
    let corpus = load_sugarscape();
    let report = regularities::render(&corpus, &census(), regularities::CORPORA[0]);
    let prose = flat(&report);
    let item_grown = corpus
        .items
        .iter()
        .filter(|i| i.verdict == Verdict::Grown)
        .count();
    let measurable = corpus
        .items
        .iter()
        .filter(|i| i.criterion.is_some() && !i.statistic.is_empty())
        .count();
    let found: Vec<(usize, usize)> = (0..=item_grown)
        .flat_map(|g| (1..measurable).map(move |m| (g, m)))
        .filter(|(g, m)| prose.contains(&format!("{g} of {m} measured claim(s) grew")))
        .collect();
    assert_eq!(
        found.len(),
        1,
        "the tally must state exactly one claim-level grown reading: {report}"
    );
    let (grown, measured) = found[0];
    assert!(
        grown < item_grown,
        "the merge must cost the grown count at least one item ({grown} vs {item_grown}); \
         a claim-level count equal to the item-level one means the merge stopped happening"
    );
    assert!(
        measured < measurable,
        "the claim denominator must be below the measurable-item count ({measured} vs \
         {measurable})"
    );
    assert!(
        prose.contains("Cite this number, not the item tally"),
        "the tally must say which number to cite: {report}"
    );
}

// ---------------------------------------------------------------------------
// The claim-level RULE, on synthetic corpora.
//
// The test above holds the report's number against the committed corpus, and
// that corpus cannot exercise the rule: its one merged claim is all-grown and
// every measurable item is measured, so `all` and `any` agree on it — both
// quantifiers in `grown_claims` survived mutation to `any` at 186/186 tests.
// The rule ("a claim counts as grown only when EVERY item merged into it
// does") is a property of the function, so it is tested on groups built to
// disagree with themselves.
//
// Both statistics below are real census columns correlated at 0.999 over the
// committed population, which is what makes the two items ONE claim. The
// positive control asserts that merge happened rather than assuming it: an
// unmerged pair would read two claims and every assertion here would mean
// something else.
// ---------------------------------------------------------------------------

/// The near-collinear census columns the founding corpus's merged claim is
/// built from, reused here so the fixtures merge for the same reason it does.
const COLLINEAR_A: &str = "raid-victim-rate";
/// The other half of that pair.
const COLLINEAR_B: &str = "raid-initiator-rate";

/// A two-item corpus whose items read [`COLLINEAR_A`] and [`COLLINEAR_B`], so
/// they merge into one claim, with the two authored verdicts chosen by the
/// caller.
///
/// The criterion is present only because `measurable` requires one; nothing
/// here computes a verdict, and `grown_claims` reads the AUTHORED verdict.
fn merged_pair_corpus(left: &str, right: &str) -> regularities::Corpus {
    let json = format!(
        r#"{{"corpus":"t","unit":"regularity","ordered":false,
             "population":"the-census","provenance":"p",
             "frozen":"before first measurement, t",
             "items":[
               {{"id":"l","title":"L","source":"S","emergence_type":2,
                 "statistic":"{COLLINEAR_A}",
                 "criterion":{{"kind":"median-at-most","bound":1e308}},
                 "verdict":"{left}","note":""}},
               {{"id":"r","title":"R","source":"S","emergence_type":2,
                 "statistic":"{COLLINEAR_B}",
                 "criterion":{{"kind":"median-at-most","bound":1e308}},
                 "verdict":"{right}","note":""}}]}}"#
    );
    regularities::load(&json).expect("fixture parses")
}

/// The positive control, and the anti-vacuity witness for the two tests
/// below: the pair really does merge, so a reading of `1 of 1` — not `2 of
/// 2` — is what an all-grown group produces.
#[test]
fn two_collinear_grown_items_are_one_grown_claim() {
    assert_eq!(
        regularities::claim_reading(&merged_pair_corpus("grown", "grown"), &census()),
        (1, 1),
        "the two statistics must merge into one claim, or the fixtures below \
         are measuring something other than the merge rule"
    );
}

/// A merged claim whose two readings DISAGREE is not grown.
///
/// One thing measured twice that answers two different ways is a claim whose
/// instrument disagrees with itself; it must not round up. This is the
/// assertion that reddens when `grown_claims`'s grown quantifier is relaxed
/// from `all` to `any`.
#[test]
fn a_merged_claim_with_one_flat_member_is_not_grown() {
    assert_eq!(
        regularities::claim_reading(&merged_pair_corpus("grown", "flat"), &census()),
        (0, 1),
        "a merged claim counts as grown only when EVERY item merged into it does"
    );
}

/// A merged claim with an UNMEASURED member is not measured at all.
///
/// Its verdict is unknown, not flat, so it belongs in neither the numerator
/// nor the denominator: counting it as measured would publish a claim-level
/// denominator that includes a claim nobody has scored. This is the assertion
/// that reddens when the measured quantifier is relaxed from `all` to `any`.
#[test]
fn a_merged_claim_with_an_unmeasured_member_is_not_counted_at_all() {
    assert_eq!(
        regularities::claim_reading(&merged_pair_corpus("grown", "unmeasured"), &census()),
        (0, 0),
        "an unmeasured member makes the claim's verdict unknown, not flat"
    );
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

/// The roadmap count as the REPORT states it — parsed out of `render`'s own
/// output, never recomputed from the corpus.
///
/// This exists because the first draft of the two guards below recomputed the
/// count from `roadmap_instrument.is_some()` and compared it to itself, which
/// is invariant under every input and could not fail. Routing the assertion
/// through this parser is what puts the production classifier
/// (`names_an_instrument`) inside the loop; a guard that never calls the thing
/// it guards is a restatement wearing a hat.
fn roadmap_count_from_report(report: &str) -> usize {
    let line = report
        .lines()
        .find(|l| l.starts_with("- roadmap ("))
        .expect("the report states a roadmap count");
    line.rsplit_once(": ")
        .expect("the roadmap line carries a count")
        .1
        .trim()
        .parse()
        .expect("the roadmap count is a number")
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

/// The classification is DECLARED DATA, not inferred wording — asserted
/// against what `render` PRINTS, not against a recomputation of the field.
///
/// The distinction is the whole test. Comparing `roadmap_instrument.is_some()`
/// to itself holds for every possible input and for every possible
/// implementation of `names_an_instrument`; comparing the REPORT's count to
/// the declared count fails the moment production classifies by anything else.
/// Verified by mutation: reverting `names_an_instrument` to the retired prose
/// sniffer turns this red.
#[test]
fn the_roadmap_split_is_not_a_function_of_note_wording() {
    let corpus = load_sugarscape();
    let printed = roadmap_count_from_report(&regularities::render(
        &corpus,
        &census(),
        regularities::CORPORA[0],
    ));
    let declared: Vec<&str> = corpus
        .items
        .iter()
        .filter(|i| i.roadmap_instrument.is_some())
        .map(|i| i.id.as_str())
        .collect();
    let phrase_bearing: Vec<&str> = corpus
        .items
        .iter()
        .filter(|i| i.verdict == Verdict::Absent)
        .filter(|i| i.note.contains("instrument is") || i.note.contains("criterion is known"))
        .map(|i| i.id.as_str())
        .collect();
    assert_eq!(
        printed,
        declared.len(),
        "the report must classify by the declared field: it printed {printed}, and \
         {} item(s) declare an instrument",
        declared.len()
    );
    // The two sets must DIFFER, or the assertion above cannot tell a
    // field-driven classifier from a prose-driven one on this corpus.
    // `sug-spatial-segregation` is the live witness: it names three concrete
    // instruments and carries neither marker phrase.
    assert_ne!(
        declared, phrase_bearing,
        "the declared set must not coincide with the prose-marker set — if it does, \
         nothing here can observe which one production used"
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

/// `note` is unparsed again, and this pins it THROUGH `render`.
///
/// **This test was a tautology in fix round 1 and the correction is the point
/// of keeping it.** It defined its own `split()` over
/// `roadmap_instrument.is_some()` and compared that to itself; `is_some()` is
/// invariant under any content-only string substitution that leaves the key
/// present, so `split(before) == split(after)` held for EVERY input and for
/// every implementation of the classifier it claimed to guard. Its comment
/// claimed a positive control, and the control only validated the fixture —
/// the instrument the fixture was wired to was blind. Proven by mutation: with
/// the prose sniffer restored, this test stayed green while four unrelated
/// tests caught the regression.
///
/// It now renders both corpora and compares the counts the REPORTS state.
#[test]
fn rewording_a_note_cannot_move_the_split() {
    let path = workspace_root().join(regularities::CORPORA[0]);
    let json = std::fs::read_to_string(&path).expect("corpus file");
    let c = census();
    let count = |text: &str| {
        let corpus = regularities::load(text).expect("corpus parses");
        roadmap_count_from_report(&regularities::render(&corpus, &c, regularities::CORPORA[0]))
    };
    // Strip every marker phrase the retired prose classifier looked for. Under
    // that classifier this collapses the roadmap half to zero; under the field
    // it must not move at all.
    let reworded = json
        .replace("instrument is", "thing to build is")
        .replace("criterion is known", "criterion has been worked out");
    assert_ne!(
        json, reworded,
        "the rewording must actually change the corpus text, or this proves nothing"
    );
    let before = count(&json);
    let after = count(&reworded);
    assert_eq!(
        before, after,
        "the roadmap split the REPORT states must not depend on note wording: \
         {before} before the rewording, {after} after"
    );
    assert!(
        before > 1,
        "the fixture must be able to detect a collapse to zero or one"
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
    // The one-sided claim is DERIVED, not a hard-coded quantifier. It read
    // "MOSTLY ... ONE-SIDED BOUNDS" while the line above it printed a 2-and-2
    // split — half, not mostly — and nothing would have moved the word when
    // the mix changed again.
    let measurable: Vec<&regularities::Item> = corpus
        .items
        .iter()
        .filter(|i| i.criterion.is_some() && !i.statistic.is_empty())
        .collect();
    let two_sided = measurable
        .iter()
        .filter(|i| {
            matches!(
                i.criterion,
                Some(regularities::Criterion::MedianInBand { .. })
                    | Some(regularities::Criterion::FractionInBandAtLeast { .. })
            )
        })
        .count();
    assert!(
        report.contains(&format!(
            "{} OF THE {} SURVIVING CRITERIA ARE CONSERVATIVE ONE-SIDED BOUNDS",
            measurable.len() - two_sided,
            measurable.len()
        )),
        "the one-sided count must be derived from the criteria, not asserted: {report}"
    );
    for word in ["MOSTLY", "mostly"] {
        assert!(
            !report[..report.find("## Items").expect("an items table")].contains(word),
            "a bare quantifier cannot track a changed mix — print the count instead"
        );
    }
    // Singular/plural against the Frozen block: the corpus declares ONE
    // disclosed exception, so the report must not assert a number of its own.
    let voice = &report[..report.find("## Items").expect("an items table")];
    assert!(
        !voice.contains("with the exceptions the Frozen"),
        "the report must not assert a count of disclosed exceptions the corpus \
         declares as one: {voice}"
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
/// longer matches the committed artifact. Exercised by moving an item's
/// `note`, which changes the rendered bytes and nothing the audit inspects.
///
/// **It moved the TITLE until the final review's doc-anchor fix**, and the
/// title stopped being audit-invisible at that moment: a `doc:` anchor now
/// resolves only when the anchored page states the item's claim, and the
/// title is half of what identifies that claim line. The title mutation
/// therefore raised an audit finding and this test's own second assertion
/// caught it. `note` is the field the module's own documentation calls
/// "never parsed" — the audit reads it nowhere, and the report reproduces it
/// verbatim in the item table — so it is the mutation that isolates the
/// drift arm now.
#[test]
fn the_binary_check_fails_on_drift_with_a_clean_audit() {
    let scratch = scratch_corpus("drift", |c| {
        c["items"][0]["note"] = serde_json::json!("A note nobody committed");
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
        "a note change must not raise an audit finding: {stderr}"
    );
}

/// `measure` is a READ: it exits 0 and leaves the corpus byte-identical.
///
/// The mode exists because the rejected alternative was to interrogate the
/// frozen corpus by MUTATING it (flip each item to `flat`, run `check`, read
/// the findings), so a `measure` that wrote anything at all would have
/// reintroduced the exact hazard it was built to avoid — and would do it
/// silently, since a written corpus still exits 0.
///
/// **THIS TEST COVERS ONLY WRITES OUTSIDE THE PER-ITEM LOOP, and it did not
/// always.** It runs against the REAL corpus, which had four `unmeasured`
/// items when this was written and has none now, because the first
/// measurement scored them all. The loop body therefore never executes here,
/// so a write placed inside it is invisible to this assertion — the test was
/// sound when authored and went vacuous the instant the task it guarded
/// completed. It is kept for the outside-the-loop half it still covers;
/// `measure_names_the_item_its_computed_verdict_and_the_number` carries the
/// same comparison over a corpus that does have an `unmeasured` item, which
/// is the arm where the loop actually runs.
#[test]
fn the_binary_measure_reads_and_writes_nothing() {
    let corpus_path = workspace_root().join(regularities::CORPORA[0]);
    let before = std::fs::read(&corpus_path).expect("reads the corpus");
    let out = run(&["regularities", "measure"]);
    assert!(
        out.status.success(),
        "measure asserts nothing and must exit 0: {}",
        String::from_utf8_lossy(&out.stderr)
    );
    let after = std::fs::read(&corpus_path).expect("reads the corpus");
    assert_eq!(
        before, after,
        "`measure` must not write the corpus — it is a read, not a gate"
    );
}

/// On a corpus with an `unmeasured` item, `measure` names the item, a
/// computed verdict, and the number behind it.
///
/// Built on a scratch corpus rather than the real one, because the real
/// corpus has been measured: every item carries a verdict, so `measure`
/// correctly prints nothing there and a test reading its stdout would assert
/// against an empty string forever.
#[test]
fn measure_names_the_item_its_computed_verdict_and_the_number() {
    let scratch = scratch_corpus("measure", |c| {
        let items = c["items"].as_array_mut().expect("items is an array");
        for item in items.iter_mut() {
            if item["id"] == serde_json::json!("sug-wealth-skew") {
                item["verdict"] = serde_json::json!("unmeasured");
                item.as_object_mut().expect("an object").remove("anchor");
                break;
            }
        }
    });
    let before = std::fs::read(&scratch).expect("reads the scratch corpus");
    let out = run(&[
        "regularities",
        "--corpus",
        scratch.to_str().expect("utf-8 path"),
        "measure",
    ]);
    // THE WRITE-NOTHING GUARD THAT ACTUALLY COVERS THE LOOP. Its sibling
    // `the_binary_measure_reads_and_writes_nothing` byte-compares the REAL
    // corpus, which has no `unmeasured` items any more — so its per-item loop
    // body never runs, and a write placed INSIDE the loop is invisible to it
    // forever. That test was sound the day it was written and went vacuous the
    // instant the first measurement completed. This corpus has an `unmeasured`
    // item by construction, so the loop executes and the comparison is live.
    let after = std::fs::read(&scratch).expect("reads the scratch corpus");
    let _ = std::fs::remove_file(&scratch);
    assert_eq!(
        before, after,
        "`measure` must not write the corpus — it is a read, not a gate, and \
         this is the arm where the per-item loop actually runs"
    );
    assert!(out.status.success(), "measure failed: {out:?}");
    let stdout = String::from_utf8(out.stdout).expect("utf-8");
    assert!(
        stdout.contains("sug-wealth-skew"),
        "measure must name the item: {stdout}"
    );
    assert!(
        stdout.contains("flat") || stdout.contains("grown"),
        "measure must state a computed verdict: {stdout}"
    );
    assert!(
        stdout.contains("median(rank-size-slope)") && stdout.contains("band [-1.2, -0.8]"),
        "measure must show the number the verdict was decided on, and the band it \
         was decided against: {stdout}"
    );
}

/// A measured item is NOT listed by `measure`.
///
/// Not a stylistic choice: a measured item's computed verdict is already
/// gated by `audit`'s two-way `Regressed` comparison, and printing it here
/// would be a second, unasserted answer to a question that already has an
/// asserted one.
#[test]
fn measure_lists_only_unmeasured_items() {
    let out = run(&["regularities", "measure"]);
    assert!(out.status.success(), "measure failed: {out:?}");
    let stdout = String::from_utf8(out.stdout).expect("utf-8");
    let corpus = load_sugarscape();
    assert!(
        corpus
            .items
            .iter()
            .all(|i| i.verdict != regularities::Verdict::Unmeasured),
        "this test's premise is that the real corpus has been measured"
    );
    assert!(
        stdout.trim().is_empty(),
        "a fully measured corpus has nothing for `measure` to report: {stdout}"
    );
}

/// An item whose statistic is not a census column is reported as
/// uncomputable rather than aborting the run — `Census::values` panics on an
/// unknown name, so this is the arm that keeps a retired metric from taking
/// every other item's reading down with it.
#[test]
fn measure_reports_an_uncomputable_item_without_aborting() {
    let scratch = scratch_corpus("uncomputable", |c| {
        let items = c["items"].as_array_mut().expect("items is an array");
        for item in items.iter_mut() {
            if item["id"] == serde_json::json!("sug-wealth-skew") {
                item["verdict"] = serde_json::json!("unmeasured");
                item["statistic"] = serde_json::json!("a-metric-no-census-has");
                item.as_object_mut().expect("an object").remove("anchor");
                break;
            }
        }
    });
    let out = run(&[
        "regularities",
        "--corpus",
        scratch.to_str().expect("utf-8 path"),
        "measure",
    ]);
    let _ = std::fs::remove_file(&scratch);
    assert!(
        out.status.success(),
        "measure asserts nothing, so an uncomputable item must not fail it: {out:?}"
    );
    let stdout = String::from_utf8(out.stdout).expect("utf-8");
    assert!(
        stdout.contains("uncomputable") && stdout.contains("a-metric-no-census-has"),
        "the line must name the item and the column it could not find: {stdout}"
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

// ---------------------------------------------------------------------------
// The two readers of the corpus (The Seedbed, Task 8).
//
// `windows/lab/src/domesday/corpus.rs` reads the same frozen files this
// module's resolver reads, with a minimal view of four fields, because a
// window may not depend on `cli` and the corpus schema does not belong in
// the kernel. That is a duplication kept ON PURPOSE, so decision 0261's
// condition applies: the two readers are held together by an agreement test.
//
// **The test lives here because it CANNOT live in `windows/lab/tests/`.**
// Putting it there means `hornvale-lab` declaring a dev-dependency on
// `hornvale`, and `architecture.rs`'s window check reads `all_deps` — built
// by an unfiltered `deps.iter().map(dep_name)`, every dependency kind — so it
// sees a dev-dependency exactly as it sees a normal one. Only `normal_deps`
// filters on `kind.is_null()`. VERIFIED by adding the edge and running the
// check: `architecture.rs:146` panicked with "window hornvale-lab depends on
// hornvale, which sits above the window layer"; the edge was then removed.
//
// An earlier version of this comment said the opposite — that dev-deps are
// not examined "at all", so the edge would ESCAPE the guard. That came from
// grepping for the literal string `dev-dependencies`, which appears nowhere,
// and concluding the behaviour was absent; it lives in a `collect` that never
// spells the word. The correction is recorded rather than quietly swapped,
// because the wrong version told a reader that dev-dependencies are an
// unpoliced hole in the constitutional layering.
// ---------------------------------------------------------------------------

/// A criterion reduced to its kind name and its frozen parameters, so the
/// two independently-declared enums can be compared without either knowing
/// the other's type. Both `match`es are exhaustive, so a sixth criterion
/// kind added to one reader and not the other fails to compile here.
fn cli_criterion_shape(c: &regularities::Criterion) -> (&'static str, Vec<f64>) {
    use regularities::Criterion as C;
    match c {
        C::MedianInBand { lo, hi } => ("median-in-band", vec![*lo, *hi]),
        C::FractionInBandAtLeast {
            lo,
            hi,
            min_fraction,
        } => ("fraction-in-band-at-least", vec![*lo, *hi, *min_fraction]),
        C::MedianAtLeast { bound } => ("median-at-least", vec![*bound]),
        C::MedianAtMost { bound } => ("median-at-most", vec![*bound]),
        C::PresentOnFraction { min_fraction } => ("present-on-fraction", vec![*min_fraction]),
    }
}

/// The same reduction over the Domesday renderer's own criterion enum.
fn lab_criterion_shape(c: &hornvale_lab::domesday::corpus::Criterion) -> (&'static str, Vec<f64>) {
    use hornvale_lab::domesday::corpus::Criterion as C;
    match c {
        C::MedianInBand { lo, hi } => ("median-in-band", vec![*lo, *hi]),
        C::FractionInBandAtLeast {
            lo,
            hi,
            min_fraction,
        } => ("fraction-in-band-at-least", vec![*lo, *hi, *min_fraction]),
        C::MedianAtLeast { bound } => ("median-at-least", vec![*bound]),
        C::MedianAtMost { bound } => ("median-at-most", vec![*bound]),
        C::PresentOnFraction { min_fraction } => ("present-on-fraction", vec![*min_fraction]),
    }
}

/// One scored item as a plain, comparable tuple: id, statistic, criterion
/// shape, verdict, disclosure.
///
/// The disclosure is in the tuple because it is a field the two readers can
/// now disagree about, and disagreement there is not cosmetic: it decides
/// whether a survey page asserts blindness it cannot support.
type ScoredShape = (
    String,
    String,
    (&'static str, Vec<f64>),
    &'static str,
    Option<String>,
);

/// What the `cli` resolver considers a scored item of the founding corpus.
fn cli_scored() -> Vec<ScoredShape> {
    load_sugarscape()
        .items
        .iter()
        .filter_map(|item| {
            let verdict = match item.verdict {
                Verdict::Grown => "GROWN",
                Verdict::Flat => "FLAT",
                _ => return None,
            };
            let criterion = item.criterion.as_ref()?;
            Some((
                item.id.clone(),
                item.statistic.clone(),
                cli_criterion_shape(criterion),
                verdict,
                item.disclosure.clone(),
            ))
        })
        .collect()
}

/// The founding corpus as the Domesday's own reader sees it.
///
/// The file is opened through an absolute path (a test's working directory
/// is its crate, not the workspace root) while `load` is handed the
/// RELATIVE one, because that string is what the reader records and the
/// page prints — see `the_rendered_corpus_pointer_is_the_relative_path`.
fn lab_corpus() -> hornvale_lab::domesday::corpus::ScoredCorpus {
    let rel = regularities::CORPORA[0];
    let json = std::fs::read_to_string(workspace_root().join(rel)).expect("corpus file");
    hornvale_lab::domesday::corpus::load(&json, rel).expect("the Domesday reader reads the corpus")
}

/// Just its scored items.
fn lab_scored() -> Vec<hornvale_lab::domesday::corpus::ScoredItem> {
    lab_corpus().items
}

/// A scratch corpus read through the Domesday's reader and then deleted.
///
/// The pointer it records is the SCRATCH path, which is exactly right: a
/// page rendered from a scratch corpus should say so. Only the real run
/// records `CORPORA[0]`.
fn read_scratch(path: &std::path::Path) -> Vec<hornvale_lab::domesday::corpus::ScoredCorpus> {
    let json = std::fs::read_to_string(path).expect("scratch corpus file");
    let corpus = hornvale_lab::domesday::corpus::load(&json, &path.display().to_string())
        .expect("scratch corpus reads");
    let _ = std::fs::remove_file(path);
    vec![corpus]
}

/// Decision 0261: the corpus schema is duplicated on purpose, so the
/// duplication carries a two-way agreement test.
///
/// Every id, statistic, criterion and verdict one reader sees, the other
/// must see identically and in the same order. Order is part of the claim:
/// the Domesday renders claim lines in corpus order, and a reader that
/// silently reordered would move a committed artifact.
#[test]
fn the_two_readers_of_the_corpus_agree() {
    let via_cli = cli_scored();
    let via_lab: Vec<ScoredShape> = lab_scored()
        .iter()
        .map(|i| {
            (
                i.id.clone(),
                i.statistic.clone(),
                lab_criterion_shape(&i.criterion),
                i.verdict.shouted(),
                i.disclosure.clone(),
            )
        })
        .collect();
    // Anti-vacuity: two empty vectors are equal, and an agreement test that
    // can be satisfied by both readers seeing nothing asserts nothing.
    assert!(
        !via_cli.is_empty(),
        "the resolver found no scored items, so the comparison below is vacuous"
    );
    assert_eq!(
        via_cli, via_lab,
        "the two readers of the regularity corpus disagree — `cli/src/regularities.rs` \
         and `windows/lab/src/domesday/corpus.rs` parse the same files and must agree \
         on every scored item (decision 0261)"
    );
}

/// The two readers also agree on the NUMBER, not merely on the schema.
///
/// The Domesday prints a measurement beside each verdict, and the resolver
/// decides that verdict from a measurement of its own. If the two ever
/// computed the statistic differently, the survey would publish a number the
/// verdict was not taken on — a sentence that reads as evidence and is not.
/// The median kinds are compared against `regularities::median` and the
/// fraction kinds against the same in-band count `regularities::meets` uses.
#[test]
fn the_two_readers_agree_on_the_measured_number() {
    let census = census();
    let corpus = load_sugarscape();
    let mut compared = 0usize;
    for item in lab_scored() {
        let authored = corpus
            .items
            .iter()
            .find(|i| i.id == item.id)
            .expect("the lab reader invents no items");
        let criterion = authored.criterion.as_ref().expect("a scored item has one");
        let present = regularities::values_of(&census, &authored.statistic);
        let worlds = census.rows.len();
        let expected = match criterion {
            regularities::Criterion::MedianInBand { .. }
            | regularities::Criterion::MedianAtLeast { .. }
            | regularities::Criterion::MedianAtMost { .. } => {
                regularities::median(&present).expect("a present statistic has a median")
            }
            regularities::Criterion::FractionInBandAtLeast { lo, hi, .. } => {
                present.iter().filter(|v| **v >= *lo && **v <= *hi).count() as f64 / worlds as f64
            }
            regularities::Criterion::PresentOnFraction { .. } => {
                present.len() as f64 / worlds as f64
            }
        };
        let measured = item
            .measured(&census)
            .expect("the committed census reports every scored statistic");
        assert_eq!(
            measured.to_bits(),
            expected.to_bits(),
            "{}: the Domesday would print {measured} while the resolver decided on \
             {expected}",
            item.id
        );
        compared += 1;
    }
    assert_eq!(compared, 4, "the founding corpus scores four items");
}

/// The claim line is DERIVED from the corpus record, not transcribed into
/// the renderer.
///
/// The proof is a scratch corpus with one field changed — `sug-wealth-skew`'s
/// recorded verdict flipped from `flat` to `grown` — rendered through the
/// same path the committed page takes. A renderer holding a literal would
/// print the same word both times. Nothing about the census changes between
/// the two renders, so the only input that moved is the one under test.
#[test]
fn flipping_a_recorded_verdict_moves_the_rendered_claim() {
    let census = census();
    let real = vec![lab_corpus()];
    let settlement =
        hornvale_lab::domesday::render::render_domain(&census, "settlement", &[], &real);
    assert!(
        settlement.contains(hornvale_lab::domesday::render::CLAIM_MARKER),
        "the settlement page must carry a claim for this test to say anything"
    );
    assert!(
        settlement.contains("`sug-wealth-skew`"),
        "the item must be named: {settlement}"
    );
    assert!(
        settlement.contains("Predicted median in [-1.2, -0.8]; measured -0.577645. FLAT."),
        "the committed reading moved; see the page: {settlement}"
    );

    let scratch = scratch_corpus("verdict-flip", |c| {
        for item in c["items"]
            .as_array_mut()
            .expect("items is an array")
            .iter_mut()
        {
            if item["id"] == serde_json::json!("sug-wealth-skew") {
                item["verdict"] = serde_json::json!("grown");
            }
        }
    });
    let flipped = read_scratch(&scratch);
    let after = hornvale_lab::domesday::render::render_domain(&census, "settlement", &[], &flipped);
    assert!(
        after.contains("Predicted median in [-1.2, -0.8]; measured -0.577645. GROWN."),
        "the verdict is transcribed, not derived — flipping the corpus record left the \
         page saying the same thing: {after}"
    );
}

/// The measured half is derived too: changing the frozen BAND moves the
/// criterion prose, and the number stays put because it is a fact about the
/// census rather than about the claim.
#[test]
fn tightening_a_band_moves_the_criterion_but_not_the_measurement() {
    let census = census();
    let scratch = scratch_corpus("band-tighten", |c| {
        for item in c["items"]
            .as_array_mut()
            .expect("items is an array")
            .iter_mut()
        {
            if item["id"] == serde_json::json!("sug-wealth-skew") {
                item["criterion"]["lo"] = serde_json::json!(-0.6);
                item["criterion"]["hi"] = serde_json::json!(-0.5);
            }
        }
    });
    let tightened = read_scratch(&scratch);
    let page =
        hornvale_lab::domesday::render::render_domain(&census, "settlement", &[], &tightened);
    assert!(
        page.contains("Predicted median in [-0.6, -0.5]; measured -0.577645."),
        "the criterion prose is assembled from the frozen parameters: {page}"
    );
}

/// The corpus pointer on the COMMITTED page is the repository-relative path,
/// not an absolute one.
///
/// `ScoredCorpus::path` is whatever the caller passed, and it is printed
/// verbatim onto a drift-checked artifact — so a caller that handed the
/// reader an absolute path would commit a machine-dependent page that then
/// drifts for everyone else. This asserts against the committed file rather
/// than a fresh render, because the failure mode is about what the BINARY
/// passes.
#[test]
fn the_rendered_corpus_pointer_is_the_relative_path() {
    let page = std::fs::read_to_string(workspace_root().join("book/src/domesday/settlement.md"))
        .expect("the settlement page");
    assert!(
        page.contains(&format!("`{}`", regularities::CORPORA[0])),
        "the page must point at the corpus by its repository-relative path"
    );
    assert!(
        !page.contains("/Users/") && !page.contains("/home/"),
        "an absolute path reached a committed page"
    );
}

/// The committed pages carry the traceability the claim line promises: what
/// the regularity is, where the source states it, and the corpus behind it.
///
/// Asserted over the real artifact, not a render, so it also holds that the
/// regeneration actually ran.
///
/// **The page is resolved from the ITEM'S OWN ANCHOR, and it used to be
/// resolved from the census column's `domain`.** That made this test look
/// like the thing closing the loop on a `doc:` anchor while its subject was
/// something else entirely: an item repointed at an unrelated generated page
/// left this green, because the page it opened was still the one the renderer
/// writes to. The anchor is the authored claim about where the evidence is,
/// so the anchor is what has to be opened.
///
/// The last assertion keeps the two readings tied together: the anchor must
/// also BE the page the Domesday writes this claim onto, which is what makes
/// an anchor into a generated page a promise the regeneration can keep.
#[test]
fn the_committed_pages_carry_the_regularity_and_its_provenance() {
    let corpus = load_sugarscape();
    let root = workspace_root();
    let census = census();
    let mut checked = 0;
    for item in &corpus.items {
        let Some(regularities::Anchor::Doc(path)) =
            item.anchor.as_deref().and_then(regularities::Anchor::parse)
        else {
            continue;
        };
        let page = std::fs::read_to_string(root.join(&path))
            .unwrap_or_else(|e| panic!("{}: cannot read its anchored page {path}: {e}", item.id));
        assert!(
            page.contains(&item.title),
            "{}: the regularity itself is missing from its anchored page {path}",
            item.id
        );
        assert!(
            page.contains(&item.source),
            "{}: the source citation is missing from its anchored page {path}",
            item.id
        );
        assert!(
            page.contains(&format!(
                "## {}",
                hornvale_lab::domesday::render::CLAIMS_SECTION_TITLE
            )),
            "{path} carries a claim but no gloss saying what one is"
        );
        assert!(
            page.contains(&corpus.provenance),
            "{path} carries a claim but not the corpus provenance behind it"
        );
        let domain = census
            .columns
            .iter()
            .find(|col| col.name == item.statistic)
            .map(|col| col.domain.clone())
            .expect("a scored statistic is a census column");
        assert_eq!(
            path,
            format!("book/src/domesday/{domain}.md"),
            "{}: the anchor names a page the Domesday does not write this claim onto — \
             the survey renders each claim on the page of its own statistic's domain",
            item.id
        );
        checked += 1;
    }
    assert_eq!(
        checked, 4,
        "the founding corpus carries four doc: anchors; a loop that checked none \
         would pass every assertion above"
    );
}

// ---------------------------------------------------------------------------
// The disclosure field (fix round 2).
//
// `sug-wealth-skew` is the founding corpus's one non-blind item. Before this
// field existed, that fact lived only in the item's prose `note`, which no
// consumer parses (deliberately — see `Item::roadmap_instrument`), so it
// reached neither the audit report nor the Domesday. The report deferred to
// the corpus ("does not count the exceptions") and the survey's gloss
// asserted blindness for every claim on the page — false for one of four, on
// the one surface a reader is invited to catch us on.
// ---------------------------------------------------------------------------

/// Exactly one item declares itself not blind, and it is the one whose own
/// frozen note has said so since before the corpus was measured.
///
/// The second assertion is what makes this more than a spelling check: the
/// structured field must agree with the frozen prose it restates. That is
/// the whole legality argument for adding a field after measurement — it
/// carries no information the corpus did not already have.
#[test]
fn the_disclosed_item_is_the_one_its_own_note_discloses() {
    let corpus = load_sugarscape();
    let disclosed: Vec<&str> = corpus
        .items
        .iter()
        .filter(|i| i.disclosure.is_some())
        .map(|i| i.id.as_str())
        .collect();
    assert_eq!(disclosed, vec!["sug-wealth-skew"]);

    for item in &corpus.items {
        let note_says = item.note.contains("NOT A BLIND TEST");
        assert_eq!(
            item.disclosure.is_some(),
            note_says,
            "{}: the structured disclosure and the frozen note disagree. The field is \
             a RESTATEMENT of the note (which is why authoring it after measurement is \
             legal at all), so the two must never diverge — and the field is authored \
             by hand, never parsed from the note, so nothing keeps them together but \
             this assertion.",
            item.id
        );
    }
}

/// The audit report COUNTS and NAMES the exceptions instead of pointing at
/// the corpus.
///
/// Closes the item Task 6's review deferred. The old paragraph said the
/// report "does not count the exceptions, because the corpus is where they
/// are declared", which made a reader open a 45-item JSON file and scan
/// prose to learn that one band was not blind.
#[test]
fn the_power_section_states_the_blindness_exceptions_by_count_and_name() {
    let corpus = load_sugarscape();
    let report = flat(&regularities::render(
        &corpus,
        &census(),
        regularities::CORPORA[0],
    ));
    assert!(
        report.contains("DECLARE THEMSELVES NOT BLIND"),
        "the report must state the exception count: {report}"
    );
    assert!(
        report.contains("1 of the 4 measurable item(s) DECLARE THEMSELVES NOT BLIND"),
        "the count must be derived from the corpus, not asserted"
    );
    assert!(
        report.contains("`sug-wealth-skew`"),
        "the exception must be named, not merely counted"
    );
    assert!(
        !report.contains("does not count the exceptions"),
        "the deferral-to-the-corpus pointer must be gone"
    );
}

/// Flipping the disclosure in a scratch corpus moves BOTH the gloss and the
/// claim line — neither is hard-coded to today's corpus.
///
/// Two directions in one test, because a one-directional check here would
/// pass on a renderer that always printed the disclosed branch:
///
/// - remove the only disclosure and the page states blindness plainly, with
///   no exception named and no disclosure on the claim line;
/// - add one to a second item and the gloss names two.
#[test]
fn the_gloss_and_the_claim_line_follow_the_disclosure_field() {
    let census = census();

    let blind = scratch_corpus("no-disclosure", |c| {
        for item in c["items"]
            .as_array_mut()
            .expect("items is an array")
            .iter_mut()
        {
            if let Some(obj) = item.as_object_mut() {
                obj.remove("disclosure");
            }
        }
    });
    let page = hornvale_lab::domesday::render::render_domain(
        &census,
        "settlement",
        &[],
        &read_scratch(&blind),
    );
    assert!(
        page.contains(
            "Every criterion on this page was authored before its statistic \
                       was looked at."
        ),
        "with no disclosure the page may state blindness plainly: {page}"
    );
    assert!(
        !page.contains("NOT A BLIND TEST"),
        "no item discloses, so no claim line carries a disclosure: {page}"
    );
    assert!(
        !page.contains("declared exception(s)"),
        "nothing to except: {page}"
    );

    let real = vec![lab_corpus()];
    let page = hornvale_lab::domesday::render::render_domain(&census, "settlement", &[], &real);
    assert!(
        page.contains("1 declared exception(s) — `sug-wealth-skew`"),
        "the gloss must name the exception it has: {page}"
    );
    assert!(
        !page.contains("Every criterion on this page was authored before"),
        "the page must not assert uniform blindness while carrying an exception: {page}"
    );
    assert!(
        page.contains("**NOT A BLIND TEST"),
        "the disclosed item's own claim line must carry the disclosure: {page}"
    );
}

/// A page whose claims are all blind says so plainly, and the sentence is
/// the honest one rather than a hedge.
///
/// `society`'s three items carry no disclosure, so this is the negative
/// branch taken over the REAL corpus rather than a scratch one — the
/// derivation is per page, not per corpus.
#[test]
fn a_page_with_no_disclosed_item_states_blindness_plainly() {
    let real = vec![lab_corpus()];
    let page = hornvale_lab::domesday::render::render_domain(&census(), "society", &[], &real);
    assert!(
        page.contains(
            "Every criterion on this page was authored before its statistic \
                       was looked at."
        ),
        "society's three items are all blind: {page}"
    );
    assert!(
        !page.contains("declared exception(s)"),
        "society has no exception to declare: {page}"
    );
}

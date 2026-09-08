//! The Domesday's frozen-claim line (The Seedbed, Task 8).
//!
//! Spec §5's teeth. A stats table is a number nobody can be *wrong* about;
//! the claim line is a sentence a reader outside the program can catch us
//! on. These tests hold three properties, and the third is the one that
//! matters:
//!
//! 1. a scored metric renders its item id, its frozen criterion in prose,
//!    the measured number, and the verdict;
//! 2. a metric no corpus scores gains no claim line at all;
//! 3. **the line is derived, never transcribed** — every part of it comes
//!    from the corpus file or from the committed census at render time, so
//!    changing either moves the page. A literal `FLAT` in the renderer
//!    would go on asserting `FLAT` after a future census made it `grown`,
//!    which is exactly the failure this family exists to prevent.

use hornvale_lab::domesday::census::{Census, Column};
use hornvale_lab::domesday::corpus::{Criterion, ScoredCorpus, ScoredItem, Verdict};
use hornvale_lab::domesday::render::{
    CLAIM_MARKER, CLAIMS_SECTION_ANCHOR, CLAIMS_SECTION_TITLE, claim_line, claim_line_unmeasured,
};
use std::collections::BTreeMap;
use std::path::Path;

/// The founding corpus's repository-relative path — the string `lab
/// domesday` passes and the one that reaches the rendered page.
const CORPUS_REL: &str = "regularities/sugarscape-1996.regularity.json";

/// The founding corpus, with the path spelled the way the CLI spells it.
///
/// The file is opened through an absolute path (a test's working directory
/// is its crate, not the workspace root) while `load` is handed the
/// RELATIVE one, because that is the string the page prints. Deliberately
/// not a `set_current_dir`: nextest gives each test its own process but
/// `cargo test` does not, and a global chdir would race.
fn founding_corpus() -> ScoredCorpus {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .and_then(Path::parent)
        .expect("workspace root");
    let json = std::fs::read_to_string(root.join(CORPUS_REL)).expect("corpus file");
    hornvale_lab::domesday::corpus::load(&json, CORPUS_REL).expect("lab reads corpus")
}

/// A one-column census over `values`, for rendering a page without paying
/// for the committed 1,000-world fixture.
fn one_metric_census(name: &str, domain: &str, values: &[&str]) -> Census {
    Census {
        columns: vec![Column {
            name: name.into(),
            kind: "numeric".into(),
            doc: "A metric, for this test.".into(),
            domain: domain.into(),
            role: "descriptor".into(),
        }],
        rows: values
            .iter()
            .map(|v| BTreeMap::from([(name.to_string(), (*v).to_string())]))
            .collect(),
    }
}

fn wealth_skew_item() -> ScoredItem {
    ScoredItem {
        corpus: "sugarscape-1996".into(),
        id: "sug-wealth-skew".into(),
        title: "Holdings are distributed far more unequally than the endowments that \
                produce them"
            .into(),
        source: "Ch. II, 'Emergence'; Animation II-3".into(),
        statistic: "rank-size-slope".into(),
        // The blind default. `disclosed_wealth_skew_item` is the other arm.
        disclosure: None,
        criterion: Criterion::MedianInBand { lo: -1.2, hi: -0.8 },
        verdict: Verdict::Flat,
    }
}

/// The same item declaring itself not blind.
fn disclosed_wealth_skew_item() -> ScoredItem {
    ScoredItem {
        disclosure: Some("NOT A BLIND TEST, disclosed under decision 0016.".into()),
        ..wealth_skew_item()
    }
}

/// A one-corpus slice carrying `items`, for rendering a page.
fn corpus_of(items: Vec<ScoredItem>) -> Vec<ScoredCorpus> {
    vec![ScoredCorpus {
        corpus: "sugarscape-1996".into(),
        path: "regularities/sugarscape-1996.regularity.json".into(),
        provenance: "Epstein and Axtell, Growing Artificial Societies (1996).".into(),
        items,
    }]
}

#[test]
fn a_scored_metric_renders_its_criterion_verdict_and_measurement() {
    let line = claim_line(&wealth_skew_item(), -0.577645);
    assert!(line.contains("sugarscape-1996"), "{line}");
    assert!(line.contains("sug-wealth-skew"), "{line}");
    assert!(line.contains("median in [-1.2, -0.8]"), "{line}");
    assert!(line.contains("-0.577645"), "{line}");
    assert!(
        line.contains("FLAT"),
        "the verdict is shouted, not buried: {line}"
    );
}

/// The line says WHAT was predicted and WHERE the source says it, not only
/// that something was.
///
/// Two unglossed slugs and a number let a reader check the arithmetic and
/// nothing else; the title and the source citation are what let someone who
/// has never seen this repository go and disagree with the source instead.
#[test]
fn a_claim_states_the_regularity_and_cites_its_source() {
    let line = claim_line(&wealth_skew_item(), -0.577645);
    assert!(
        line.contains("Holdings are distributed far more unequally"),
        "the regularity itself must reach the page: {line}"
    );
    assert!(
        line.contains("Ch. II, 'Emergence'; Animation II-3"),
        "the source citation must reach the page intact: {line}"
    );
}

/// The source citation survives verbatim through a full page render.
///
/// `Animation II-3` is registry-ID-SHAPED — two upper-case letters, a
/// hyphen, a digit — so routing a claim through the same
/// `redact_registry_citations` a metric doc goes through would silently
/// delete it. This is the assertion that would catch that.
#[test]
fn a_registry_shaped_source_citation_is_not_redacted() {
    let census = one_metric_census("rank-size-slope", "settlement", &["-1.0", "-0.9", "-0.8"]);
    let page = hornvale_lab::domesday::render::render_domain(
        &census,
        "settlement",
        &[],
        &corpus_of(vec![wealth_skew_item()]),
    );
    assert!(
        page.contains("Animation II-3"),
        "the citation was mangled on its way to the page: {page}"
    );
}

/// A page carrying a claim also carries the gloss that says what a claim
/// is, the corpus pointer, and that corpus's provenance — and the claim
/// links to it.
#[test]
fn a_page_with_a_claim_carries_the_corpus_it_came_from() {
    let census = one_metric_census("rank-size-slope", "settlement", &["-1.0", "-0.9", "-0.8"]);
    let page = hornvale_lab::domesday::render::render_domain(
        &census,
        "settlement",
        &[],
        &corpus_of(vec![wealth_skew_item()]),
    );
    assert!(
        page.contains(&format!("## {CLAIMS_SECTION_TITLE}")),
        "the gloss section is missing: {page}"
    );
    assert!(
        page.contains(CLAIMS_SECTION_ANCHOR),
        "the claim must link to the gloss: {page}"
    );
    assert!(
        page.contains("`regularities/sugarscape-1996.regularity.json`"),
        "the page must point at the corpus file: {page}"
    );
    assert!(
        page.contains("Growing Artificial Societies"),
        "the corpus provenance must reach the page: {page}"
    );
}

/// The anchor the claim links to is the one mdBook derives from the
/// section heading, so a rename cannot silently break the link.
#[test]
fn the_gloss_anchor_matches_its_heading() {
    let derived = format!("#{}", CLAIMS_SECTION_TITLE.to_lowercase().replace(' ', "-"));
    assert_eq!(derived, CLAIMS_SECTION_ANCHOR);
}

/// Every criterion kind renders prose that names its own frozen parameters.
///
/// The `match` inside `Criterion::prose` is exhaustive, so a sixth kind
/// cannot be added without the compiler demanding a rendering for it; this
/// test holds that each of the five actually *says* its numbers rather than
/// falling back to a kind name.
#[test]
fn every_criterion_kind_states_its_own_numbers() {
    for (criterion, wanted) in [
        (
            Criterion::MedianInBand { lo: -1.2, hi: -0.8 },
            vec!["median", "-1.2", "-0.8"],
        ),
        (
            Criterion::MedianAtLeast { bound: 1.0 },
            vec!["median", "at least", "1"],
        ),
        (
            Criterion::MedianAtMost { bound: 0.5 },
            vec!["median", "at most", "0.5"],
        ),
        (
            Criterion::FractionInBandAtLeast {
                lo: 0.02,
                hi: 0.5,
                min_fraction: 0.5,
            },
            vec!["0.02", "0.5", "worlds"],
        ),
        (
            Criterion::PresentOnFraction { min_fraction: 0.25 },
            vec!["0.25", "worlds"],
        ),
    ] {
        let prose = criterion.prose();
        for want in wanted {
            assert!(
                prose.contains(want),
                "criterion {criterion:?} rendered {prose:?}, which does not state {want:?}"
            );
        }
    }
}

/// The load-bearing negative: the Domesday must not sprout a claim for a
/// metric no frozen corpus scores.
#[test]
fn an_unscored_metric_gains_no_claim_line() {
    let census = one_metric_census("mean-population", "demography", &["1.0", "2.0", "3.0"]);
    let page = hornvale_lab::domesday::render::render_domain(&census, "demography", &[], &[]);
    assert!(
        page.contains("mean-population"),
        "the page must still render the metric: {page}"
    );
    assert!(
        !page.contains(CLAIM_MARKER),
        "no corpus scores it, so no claim: {page}"
    );
}

/// The same negative over the REAL corpus and a genuinely unscored metric,
/// so the property is not an artifact of passing an empty slice.
///
/// `mean-population` is a `demography` column; the founding corpus scores
/// four statistics and none of them is in `demography` at all.
#[test]
fn a_real_corpus_leaves_a_metric_it_does_not_score_unclaimed() {
    let corpus = founding_corpus();
    assert!(
        !corpus.items.is_empty(),
        "anti-vacuity: the corpus scored nothing, so the negative below proves nothing"
    );
    assert!(
        !corpus
            .items
            .iter()
            .any(|i| i.statistic == "mean-population"),
        "this test's premise is that `mean-population` is unscored; it no longer is"
    );
    let census = one_metric_census("mean-population", "demography", &["1.0", "2.0", "3.0"]);
    let page = hornvale_lab::domesday::render::render_domain(&census, "demography", &[], &[corpus]);
    assert!(
        !page.contains(CLAIM_MARKER),
        "the real corpus does not score this metric, so no claim: {page}"
    );
    assert!(
        !page.contains(CLAIMS_SECTION_TITLE),
        "a corpus that scores nothing on this page contributes no provenance either: {page}"
    );
}

/// A scored metric's page carries the claim, and the number in it is the
/// one this census produces — not the one the committed census produces.
///
/// This is the derivation property at the page level: the census here is
/// three hand-written rows whose median is `-0.9`, which is inside the
/// frozen band and nowhere near the committed census's `-0.577645`. A
/// renderer that transcribed the number would print the wrong one.
#[test]
fn the_measured_number_comes_from_the_census_being_rendered() {
    let census = one_metric_census("rank-size-slope", "settlement", &["-1.0", "-0.9", "-0.8"]);
    let page = hornvale_lab::domesday::render::render_domain(
        &census,
        "settlement",
        &[],
        &corpus_of(vec![wealth_skew_item()]),
    );
    assert!(page.contains(CLAIM_MARKER), "{page}");
    assert!(
        page.contains("-0.900000"),
        "the median of THIS census is -0.9: {page}"
    );
    assert!(
        !page.contains("-0.577645"),
        "the committed census's median must not leak in: {page}"
    );
}

/// The verdict comes from the corpus record, so flipping the record flips
/// the page.
///
/// The two renders below differ in exactly one input — the `verdict` field
/// — and the assertion is that the rendered word tracks it in both
/// directions. A literal in the renderer passes neither direction.
#[test]
fn the_verdict_in_the_line_tracks_the_recorded_verdict() {
    let mut item = wealth_skew_item();
    let flat = claim_line(&item, -0.577645);
    item.verdict = Verdict::Grown;
    let grown = claim_line(&item, -0.577645);
    assert!(flat.contains("FLAT") && !flat.contains("GROWN"), "{flat}");
    assert!(
        grown.contains("GROWN") && !grown.contains("FLAT"),
        "{grown}"
    );
}

/// A statistic no world reports says so, rather than printing a number it
/// does not have or falling silent.
#[test]
fn an_absent_statistic_says_so_instead_of_printing_a_number() {
    let line = claim_line_unmeasured(&wealth_skew_item());
    assert!(line.contains("sug-wealth-skew"), "{line}");
    assert!(line.contains("absent"), "{line}");
    assert!(line.contains("FLAT"), "{line}");
}

/// The reader keeps only the items a corpus actually scores, and keeps all
/// four of them.
///
/// A corpus item with a non-measured verdict (`absent`, `refused`,
/// `deferred`, `inapplicable`, `unmeasured`) carries no criterion and makes
/// no falsifiable claim, so it must not reach a page.
#[test]
fn the_reader_keeps_exactly_the_scored_items() {
    let corpus = founding_corpus();
    let ids: Vec<&str> = corpus.items.iter().map(|i| i.id.as_str()).collect();
    assert_eq!(
        ids,
        vec![
            "sug-wealth-skew",
            "sug-predation-is-bounded",
            "sug-retaliation-deters",
            "sug-credit-makes-hierarchy",
        ],
        "the scored subset of the founding corpus moved"
    );
    for item in &corpus.items {
        assert_eq!(item.corpus, "sugarscape-1996");
        assert!(!item.statistic.is_empty(), "{}", item.id);
        assert!(!item.title.is_empty(), "{}", item.id);
        assert!(!item.source.is_empty(), "{}", item.id);
    }
    assert_eq!(corpus.path, CORPUS_REL);
    assert!(
        corpus.provenance.contains("Epstein") && corpus.provenance.contains("Axtell"),
        "the provenance must carry the bibliographic origin: {}",
        corpus.provenance
    );
}

/// A corpus whose items carry unmeasured verdicts yields no scored items,
/// and an unparseable file is an error rather than an empty reading.
///
/// The second half is the one worth having: a reader that failed open to
/// the empty set would silently strip every claim line from the survey and
/// leave a healthy-looking page behind.
#[test]
fn the_reader_fails_loudly_and_filters_quietly() {
    let unscored = r#"{
      "corpus": "test-corpus",
      "provenance": "Nowhere in particular.",
      "items": [
        {"id": "a", "title": "t", "source": "s", "verdict": "absent"},
        {"id": "b", "title": "t", "source": "s", "verdict": "refused",
         "anchor": "decision:0022"},
        {"id": "c", "title": "t", "source": "s", "verdict": "unmeasured"}
      ]
    }"#;
    assert!(
        hornvale_lab::domesday::corpus::load(unscored, "regularities/x.regularity.json")
            .expect("parses")
            .items
            .is_empty(),
        "nothing here is scored"
    );
    assert!(
        hornvale_lab::domesday::corpus::load("{ not json", "regularities/x.regularity.json")
            .is_err(),
        "a malformed corpus must not read as zero scored items"
    );
}

/// A blind item's claim line carries no disclosure, and a disclosed one's
/// carries it in the line itself — not only in the page's closing gloss.
///
/// A reader who scrolls to a metric, reads `FLAT` and moves on must not be
/// able to miss that this particular claim was not a blind test.
#[test]
fn a_disclosed_item_carries_its_disclosure_on_its_own_claim_line() {
    let blind = claim_line(&wealth_skew_item(), -0.577645);
    assert!(
        !blind.contains("NOT A BLIND TEST"),
        "a blind item claims nothing about blindness: {blind}"
    );
    let disclosed = claim_line(&disclosed_wealth_skew_item(), -0.577645);
    assert!(
        disclosed.contains("NOT A BLIND TEST, disclosed under decision 0016."),
        "the disclosure must reach the line itself: {disclosed}"
    );
    assert!(
        disclosed.contains("FLAT"),
        "a disclosed item is still measured and still scored: {disclosed}"
    );
}

/// The absent arm carries the disclosure too.
///
/// Two format strings would have let this one drift; `claim_sentence` is
/// shared precisely so it cannot.
#[test]
fn the_absent_arm_carries_the_disclosure_as_well() {
    let line = claim_line_unmeasured(&disclosed_wealth_skew_item());
    assert!(
        line.contains("absent (no world reported a value)"),
        "{line}"
    );
    assert!(line.contains("NOT A BLIND TEST"), "{line}");
}

/// The gloss is derived from the page's own items in BOTH directions.
///
/// A one-directional check would pass on a renderer hard-coded to either
/// branch, which is exactly the defect this replaces: the old preamble
/// asserted blindness for every claim unconditionally.
#[test]
fn the_gloss_states_blindness_only_when_the_page_can_support_it() {
    let census = one_metric_census("rank-size-slope", "settlement", &["-1.0", "-0.9", "-0.8"]);

    let blind = hornvale_lab::domesday::render::render_domain(
        &census,
        "settlement",
        &[],
        &corpus_of(vec![wealth_skew_item()]),
    );
    assert!(
        blind.contains(
            "Every criterion on this page was authored before its statistic \
                        was looked at."
        ),
        "{blind}"
    );
    assert!(!blind.contains("declared exception(s)"), "{blind}");

    let disclosed = hornvale_lab::domesday::render::render_domain(
        &census,
        "settlement",
        &[],
        &corpus_of(vec![disclosed_wealth_skew_item()]),
    );
    assert!(
        disclosed.contains("1 declared exception(s) — `sug-wealth-skew`"),
        "the gloss must count AND name what the page discloses: {disclosed}"
    );
    assert!(
        !disclosed.contains("Every criterion on this page was authored before"),
        "the page must never assert blindness it cannot support: {disclosed}"
    );
}

/// A disclosure on a metric that lives on ANOTHER page is not this page's
/// exception.
///
/// Naming it here would send a reader hunting for a claim line that is not
/// present. The gloss is derived from what the page SHOWS, not from what the
/// caller loaded.
#[test]
fn the_gloss_counts_only_the_items_this_page_shows() {
    let census = one_metric_census("rank-size-slope", "settlement", &["-1.0", "-0.9", "-0.8"]);
    let elsewhere = ScoredItem {
        id: "sug-elsewhere".into(),
        statistic: "raid-victim-rate".into(),
        disclosure: Some("NOT A BLIND TEST, on another page.".into()),
        ..wealth_skew_item()
    };
    let page = hornvale_lab::domesday::render::render_domain(
        &census,
        "settlement",
        &[],
        &corpus_of(vec![wealth_skew_item(), elsewhere]),
    );
    assert!(
        page.contains(
            "Every criterion on this page was authored before its statistic \
                       was looked at."
        ),
        "the only item this page shows is blind: {page}"
    );
    assert!(
        !page.contains("sug-elsewhere"),
        "an item this page does not show must not be named on it: {page}"
    );
}

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
use hornvale_lab::domesday::corpus::{Criterion, ScoredItem, Verdict};
use hornvale_lab::domesday::render::{CLAIM_MARKER, claim_line, claim_line_unmeasured};
use std::collections::BTreeMap;
use std::path::{Path, PathBuf};

/// The workspace root, from this crate's manifest directory.
fn workspace_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .and_then(Path::parent)
        .expect("workspace root")
        .to_path_buf()
}

/// The founding corpus's path.
fn corpus_path() -> PathBuf {
    workspace_root().join("regularities/sugarscape-1996.regularity.json")
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
        statistic: "rank-size-slope".into(),
        criterion: Criterion::MedianInBand { lo: -1.2, hi: -0.8 },
        verdict: Verdict::Flat,
    }
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
    let scored = hornvale_lab::domesday::corpus::read(&corpus_path()).expect("lab reads corpus");
    assert!(
        !scored.is_empty(),
        "anti-vacuity: the corpus scored nothing, so the negative below proves nothing"
    );
    assert!(
        !scored.iter().any(|i| i.statistic == "mean-population"),
        "this test's premise is that `mean-population` is unscored; it no longer is"
    );
    let census = one_metric_census("mean-population", "demography", &["1.0", "2.0", "3.0"]);
    let page = hornvale_lab::domesday::render::render_domain(&census, "demography", &[], &scored);
    assert!(
        !page.contains(CLAIM_MARKER),
        "the real corpus does not score this metric, so no claim: {page}"
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
        &[wealth_skew_item()],
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
    let scored = hornvale_lab::domesday::corpus::read(&corpus_path()).expect("lab reads corpus");
    let ids: Vec<&str> = scored.iter().map(|i| i.id.as_str()).collect();
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
    for item in &scored {
        assert_eq!(item.corpus, "sugarscape-1996");
        assert!(!item.statistic.is_empty(), "{}", item.id);
    }
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
      "items": [
        {"id": "a", "verdict": "absent"},
        {"id": "b", "verdict": "refused", "anchor": "decision:0022"},
        {"id": "c", "verdict": "unmeasured"}
      ]
    }"#;
    assert!(
        hornvale_lab::domesday::corpus::load(unscored)
            .expect("parses")
            .is_empty(),
        "nothing here is scored"
    );
    assert!(
        hornvale_lab::domesday::corpus::load("{ not json").is_err(),
        "a malformed corpus must not read as zero scored items"
    );
}

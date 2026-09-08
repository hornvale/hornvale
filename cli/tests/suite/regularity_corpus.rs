use hornvale::regularities::{self, Criterion, Verdict};

const FIXTURE: &str = r#"{
  "corpus": "fixture",
  "unit": "regularity",
  "ordered": false,
  "population": "the-census",
  "provenance": "a fixture",
  "frozen": "before first measurement, a fixture",
  "items": [
    { "id": "a", "title": "T", "source": "S", "emergence_type": 2,
      "statistic": "rank-size-slope",
      "criterion": { "kind": "median-in-band", "lo": -1.2, "hi": -0.8 },
      "verdict": "unmeasured", "note": "" }
  ]
}"#;

#[test]
fn a_corpus_parses_its_items_criteria_and_verdicts() {
    let c = regularities::load(FIXTURE).expect("fixture parses");
    assert_eq!(c.corpus, "fixture");
    assert_eq!(c.unit, "regularity");
    assert!(!c.ordered);
    assert_eq!(c.items.len(), 1);
    assert_eq!(c.items[0].verdict, Verdict::Unmeasured);
    assert_eq!(c.items[0].emergence_type, 2);
    assert_eq!(
        c.items[0].criterion,
        Some(Criterion::MedianInBand { lo: -1.2, hi: -0.8 })
    );
}

#[test]
fn an_unknown_verdict_is_a_parse_error() {
    let bad = FIXTURE.replace("\"unmeasured\"", "\"probably\"");
    assert!(
        regularities::load(&bad).is_err(),
        "unknown verdict must not parse"
    );
}

#[test]
fn an_unknown_criterion_kind_is_a_parse_error() {
    let bad = FIXTURE.replace("median-in-band", "vibes");
    assert!(
        regularities::load(&bad).is_err(),
        "unknown criterion must not parse"
    );
}

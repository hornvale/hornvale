use hornvale::regularities::{self, Criterion, Verdict};
use std::path::PathBuf;

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
    assert_eq!(c.items[0].emergence_type, Some(2));
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

/// The freeze. A corpus's item count is asserted so that changing the
/// catalogue is a deliberate act, never a side effect — the same discipline
/// `tropes/` and `systems/` carry for their own counts (decision 0016).
#[test]
fn the_sugarscape_corpus_is_frozen_at_its_declared_size() {
    let c = load_sugarscape();
    assert_eq!(c.items.len(), 45);
}

#[test]
fn the_corpus_declares_its_provenance_and_freeze() {
    let c = load_sugarscape();
    assert!(
        c.provenance.contains("Epstein"),
        "provenance names its source"
    );
    assert!(c.frozen.contains("before first measurement"));
    assert_eq!(c.population, "the-census");
    assert!(!c.ordered, "Sugarscape's rules compose, they do not ladder");
}

#[test]
fn every_item_id_is_unique_and_every_measurable_item_carries_a_criterion() {
    let c = load_sugarscape();
    let mut ids: Vec<&str> = c.items.iter().map(|i| i.id.as_str()).collect();
    ids.sort_unstable();
    let before = ids.len();
    ids.dedup();
    assert_eq!(before, ids.len(), "duplicate item id");
    for item in &c.items {
        let measurable = matches!(
            item.verdict,
            Verdict::Unmeasured | Verdict::Grown | Verdict::Flat
        );
        assert_eq!(
            measurable,
            item.criterion.is_some() && !item.statistic.is_empty(),
            "{}: a measurable verdict needs a statistic and a criterion, and \
             a non-measurable one must carry neither",
            item.id
        );
        // `emergence_type` is nullable so that a model abstraction or a bare
        // micro-rule can decline the source's taxonomy instead of defaulting
        // into it. An item this corpus proposes to MEASURE is asserting a
        // regularity by construction, so it may not decline: without this,
        // nulling the field is a way to smuggle a filler item into the
        // measurable half.
        if measurable {
            assert!(
                item.emergence_type.is_some(),
                "{}: a measurable item asserts a regularity, so it carries an \
                 emergence type",
                item.id
            );
        }
        if let Some(t) = item.emergence_type {
            assert!(
                t == 1 || t == 2,
                "{}: emergence type {t} is outside the source's taxonomy",
                item.id
            );
        }
    }
}

fn load_sugarscape() -> hornvale::regularities::Corpus {
    let path = workspace_root().join(hornvale::regularities::CORPORA[0]);
    let json = std::fs::read_to_string(&path).expect("corpus file");
    hornvale::regularities::load(&json).expect("corpus parses")
}

fn workspace_root() -> PathBuf {
    std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("workspace root")
        .to_path_buf()
}

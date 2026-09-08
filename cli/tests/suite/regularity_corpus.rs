use hornvale::regularities::{self, Anchor, Criterion, GeneratedPaths, Verdict};
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
        // A criterion's discriminating power must be auditable from this file
        // alone, with no data — because a session that has read the census
        // can no longer author a band, so the only reviewer who can improve
        // one is a reader who never sees the numbers. Stating the world in
        // which the regularity is FALSE is what makes that review possible,
        // and three criteria that could not redden on their own negation
        // reached a committed corpus before this was required.
        if measurable {
            assert!(
                item.note.contains("FALSIFYING WORLD"),
                "{}: a measurable item's note must name the world in which \
                 its regularity is false, so its power can be audited \
                 without the census",
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

#[test]
fn a_doc_anchor_into_generated_prose_resolves() {
    let g = GeneratedPaths::read(&workspace_root()).expect("read declarations");
    assert!(
        g.has_generator("book/src/domesday/demography.md"),
        "the Domesday is generated (author `artifacts`) and must anchor"
    );
}

#[test]
fn a_doc_anchor_into_hand_written_prose_is_refused() {
    // This is the load-bearing direction. `book/src/laboratory/overview.md`
    // is declared `none(hand-written prose, never regenerated)`; anchoring a
    // verdict to it would be decision 0330's failure — a declaration that
    // moves the score without moving the world.
    let g = GeneratedPaths::read(&workspace_root()).expect("read declarations");
    assert!(
        !g.has_generator("book/src/laboratory/overview.md"),
        "hand-written prose must never back a verdict"
    );
}

#[test]
fn an_undeclared_path_is_refused() {
    let g = GeneratedPaths::read(&workspace_root()).expect("read declarations");
    assert!(!g.has_generator("book/src/nothing-here.md"));
}

#[test]
fn anchor_parses_the_five_kinds_and_rejects_the_unknown() {
    assert_eq!(
        Anchor::parse("doc:book/src/domesday/demography.md"),
        Some(Anchor::Doc("book/src/domesday/demography.md".to_string()))
    );
    assert_eq!(
        Anchor::parse("decision:0135"),
        Some(Anchor::Decision("0135".into()))
    );
    assert_eq!(
        Anchor::parse("registry:TOOL-x"),
        Some(Anchor::Registry("TOOL-x".into()))
    );
    assert_eq!(
        Anchor::parse("reason:because"),
        Some(Anchor::Reason("because".into()))
    );
    assert_eq!(
        Anchor::parse("path:src/x.rs"),
        None,
        "path: is not admitted by this family"
    );
    assert_eq!(Anchor::parse("nonsense"), None);
}

//! Resolves the `technologies/` corpus: capabilities a people acquires,
//! holds, and loses.
//!
//! The corpus is DATA (`technologies/*.technology.json`) and this file is
//! its RESOLVER (decision 0011). Nothing in `domains/*` or `windows/*`
//! reads a corpus file.

const FIXTURE: &str = r#"{
  "corpus": "fixture",
  "unit": "technology",
  "ordered": false,
  "provenance": "a fixture",
  "frozen": "never",
  "items": [
    { "id": "a", "title": "A", "introduces": "tok-a",
      "presupposes": [], "verdict": "absent", "anchor": "" }
  ]
}"#;

#[test]
fn a_corpus_parses_its_items_and_verdicts() {
    let c = hornvale::technologies::parse(FIXTURE);
    assert_eq!(c.items.len(), 1);
    assert_eq!(c.items[0].verdict, hornvale::technologies::Verdict::Absent);
    assert_eq!(c.items[0].introduces, "tok-a");
}

#[test]
fn an_unknown_verdict_is_a_parse_error() {
    let bad = FIXTURE.replace("\"absent\"", "\"probably\"");
    assert!(std::panic::catch_unwind(|| hornvale::technologies::parse(&bad)).is_err());
}

//! Resolves the `technologies/` corpus: capabilities a people acquires,
//! holds, and loses.
//!
//! The corpus is DATA (`technologies/*.technology.json`) and this file is
//! its RESOLVER (decision 0011). Nothing in `domains/*` or `windows/*`
//! reads a corpus file.

use std::path::PathBuf;

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

/// A clean chain a -> b -> c (c presupposes b, b presupposes a), so the
/// closure from "c" must reach every rung, including c's own `introduces`.
const CHAIN_FIXTURE: &str = r#"{
  "corpus": "fixture",
  "unit": "technology",
  "ordered": false,
  "provenance": "a fixture",
  "frozen": "never",
  "items": [
    { "id": "a", "title": "A", "introduces": "tok-a",
      "presupposes": [], "verdict": "absent", "anchor": "" },
    { "id": "b", "title": "B", "introduces": "tok-b",
      "presupposes": ["a"], "verdict": "absent", "anchor": "" },
    { "id": "c", "title": "C", "introduces": "tok-c",
      "presupposes": ["b"], "verdict": "absent", "anchor": "" }
  ]
}"#;

/// A two-item cycle: a presupposes b, b presupposes a. Neither item is a
/// valid root, so [`hornvale::technologies::parse`] must reject this at
/// parse time rather than let [`hornvale::technologies::derived_demands`]
/// loop forever.
const CYCLE_FIXTURE: &str = r#"{
  "corpus": "fixture",
  "unit": "technology",
  "ordered": false,
  "provenance": "a fixture",
  "frozen": "never",
  "items": [
    { "id": "a", "title": "A", "introduces": "tok-a",
      "presupposes": ["b"], "verdict": "absent", "anchor": "" },
    { "id": "b", "title": "B", "introduces": "tok-b",
      "presupposes": ["a"], "verdict": "absent", "anchor": "" }
  ]
}"#;

/// `a` presupposes `"nope"`, which names no item in this corpus — the
/// dangling reference decision 0386 forbids ("`presupposes` names an item in
/// this corpus and nothing else").
const DANGLING_FIXTURE: &str = r#"{
  "corpus": "fixture",
  "unit": "technology",
  "ordered": false,
  "provenance": "a fixture",
  "frozen": "never",
  "items": [
    { "id": "a", "title": "A", "introduces": "tok-a",
      "presupposes": ["nope"], "verdict": "absent", "anchor": "" }
  ]
}"#;

fn workspace_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("workspace root")
        .to_path_buf()
}

fn load_asimov() -> hornvale::technologies::Corpus {
    hornvale::technologies::load(&workspace_root().join("technologies/asimov-1989.technology.json"))
}

fn load_henrich() -> hornvale::technologies::Corpus {
    hornvale::technologies::load(
        &workspace_root().join("technologies/henrich-2004-extended.technology.json"),
    )
}

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

/// The freeze. A corpus's item count is asserted so that changing the
/// catalogue is a deliberate act, never a side effect — the same discipline
/// `tropes/`, `systems/` and `regularities/` carry (decision 0016).
///
/// Derived (never guessed) via:
///   python3 -c "import json; print(len(json.load(open('technologies/asimov-1989.technology.json'))['items']))"
#[test]
fn the_asimov_corpus_is_frozen_at_its_declared_size() {
    let c = load_asimov();
    assert_eq!(c.items.len(), 41);
}

/// The freeze, for the second corpus. Same discipline, same derivation
/// method as [`the_asimov_corpus_is_frozen_at_its_declared_size`].
#[test]
fn the_collapse_corpus_is_frozen_at_its_declared_size() {
    let c = load_henrich();
    assert_eq!(c.items.len(), 41);
}

/// 0386: the demand set is the transitive closure of `presupposes`,
/// collecting each rung's `introduces`. It is computed on read and never
/// written into the file — materialising it would state one fact twice and
/// need an agreement test whose cheapest repair is deletion (0261).
#[test]
fn demands_are_the_transitive_closure_of_presupposes() {
    let c = hornvale::technologies::parse(CHAIN_FIXTURE);
    let d = hornvale::technologies::derived_demands(&c, "c");
    assert!(d.contains("tok-a"), "closure must reach a grandparent");
    assert!(d.contains("tok-b"));
    assert!(
        d.contains("tok-c"),
        "an item's own introduces is in its demands"
    );
}

#[test]
fn a_cycle_in_the_lattice_is_a_parse_error() {
    assert!(std::panic::catch_unwind(|| { hornvale::technologies::parse(CYCLE_FIXTURE) }).is_err());
}

#[test]
fn presupposes_naming_an_unknown_item_is_a_parse_error() {
    assert!(
        std::panic::catch_unwind(|| { hornvale::technologies::parse(DANGLING_FIXTURE) }).is_err()
    );
}

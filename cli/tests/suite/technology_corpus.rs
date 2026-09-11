//! Resolves the `technologies/` corpus: capabilities a people acquires,
//! holds, and loses.
//!
//! The corpus is DATA (`technologies/*.technology.json`) and this file is
//! its RESOLVER (decision 0011). Nothing in `domains/*` or `windows/*`
//! reads a corpus file.

use std::collections::BTreeSet;
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

/// `d` presupposes both `b` and `c`, and both `b` and `c` presuppose `a` —
/// two paths converge on a shared ancestor. This is the normal shape family
/// law calls a LATTICE rather than a tree, and it is NOT a cycle: Task 4's
/// reviewer verified this by hand with a standalone program (`d -> {b, c}
/// -> a`), but nothing committed held it before this fixture (Task 5's
/// brief, "One addition folded in from Task 4's review").
const DIAMOND_FIXTURE: &str = r#"{
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
      "presupposes": ["a"], "verdict": "absent", "anchor": "" },
    { "id": "d", "title": "D", "introduces": "tok-d",
      "presupposes": ["b", "c"], "verdict": "absent", "anchor": "" }
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

/// A diamond-shaped lattice (`d -> {b, c} -> a`) is NOT a cycle, and its
/// closure is EXACT — neither short (missing the shared ancestor `tok-a`,
/// which a naive "already visited, stop" traversal could drop from one of
/// the two converging paths) nor over-reaching (containing anything beyond
/// the four rungs actually in the lattice). This is the `presupposes`
/// lattice's normal "lattice, not tree" shape (family law), which had no
/// committed test before this one (Task 4's review).
///
/// **What this test does NOT establish, despite an earlier draft's claim:**
/// "`tok-a` appears exactly once" is not a possible failure for
/// [`hornvale::technologies::derived_demands`] to have — it returns a
/// `BTreeSet<String>`, which cannot hold a duplicate by construction. The
/// reviewer traced both DFS orderings by hand and confirmed the real
/// discriminating power is here: a `Mark::Done`-less traversal that treats
/// "already on the stack" the same as "already fully explored" is the
/// concrete bug this fixture would catch, via the exact-set equality below,
/// not via any claim about duplication.
#[test]
fn a_diamond_lattice_parses_and_its_closure_is_exact() {
    let result = std::panic::catch_unwind(|| hornvale::technologies::parse(DIAMOND_FIXTURE));
    assert!(
        result.is_ok(),
        "a diamond (two paths converging on one ancestor) must not be \
         mistaken for a cycle"
    );
    let c = result.expect("checked above");
    let d = hornvale::technologies::derived_demands(&c, "d");
    assert_eq!(
        d,
        BTreeSet::from([
            "tok-a".to_string(),
            "tok-b".to_string(),
            "tok-c".to_string(),
            "tok-d".to_string(),
        ]),
        "the closure must be exactly these four rungs — no shorter (the \
         shared ancestor tok-a dropped by one of the two converging paths) \
         and no longer"
    );
}

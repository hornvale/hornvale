# The Seedbed Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Build `regularities/`, a fourth corpus family that measures whether Hornvale *grows* macro-regularities imported from outside, scored against the committed census.

**Architecture:** A corpus is data (`regularities/<name>.regularity.json`), a resolver is code (`cli/src/regularities.rs`), mirroring `cli/src/systems.rs` shape-for-shape. The resolver reads the committed census through `hornvale_lab::domesday::{census, stats}` — `cli` already depends on `hornvale-lab` — evaluates each item's frozen criterion, and checks that the authored verdict and the computed verdict agree **in both directions**. Verdicts anchor to generated documentation, validated against `docs/generated-paths.txt`.

**Tech Stack:** Rust edition 2024, `serde`/`serde_json` only, `cargo nextest`.

**Spec:** [`docs/superpowers/specs/2026-09-07-the-seedbed-design.md`](../specs/2026-09-07-the-seedbed-design.md)

## Global Constraints

- **Dependencies:** `serde`, `serde_json`, `libm` only. No new crates. The allowlist is `ALLOWED_EXTERNAL` in `cli/tests/suite/architecture.rs`.
- **No `HashMap`/`HashSet`.** `BTreeMap`/`BTreeSet`/`Vec` only, enforced workspace-wide by `clippy.toml`.
- **Every crate sets `#![warn(missing_docs)]`.** Every public item, field and variant gets a one-line doc comment.
- **Every primitive at a `pub` boundary carries a `type-audit:` tag** (`bare-ok(<class>)` / `waiver(<reason>)`). Copy the tag style from `cli/src/systems.rs` — e.g. `/// type-audit: bare-ok(identifier-text: id), bare-ok(prose: note)`.
- **`cargo fmt` is the final step before every commit.** fmt-gate skips are the most common review finding.
- **Layering:** `cli/` may depend on windows and domains. Do not add a dependency in the other direction.
- **The census is read, never run.** No task in this plan runs a census, regenerates one, or moves a golden.

---

### Task 1: Corpus schema and loader

**Files:**
- Create: `cli/src/regularities.rs`
- Modify: `cli/src/lib.rs` (add `pub mod regularities;`)
- Test: `cli/tests/suite/regularity_corpus.rs`, `cli/tests/suite.rs` (add `mod regularity_corpus;`)

**Interfaces:**
- Consumes: nothing.
- Produces: `Verdict`, `Criterion`, `Item`, `Corpus`, `load(&str) -> Result<Corpus, String>`, `pub const CORPORA: &[&str]`.

**Why `unmeasured` exists.** The criterion is the *prediction* and is frozen before measurement (decision 0016); the verdict is the *record* of what measuring found. At freeze time a measurable item is authored `unmeasured`. It is a lifecycle state, **not a coverage verdict** — the report tallies the six verdicts and lists `unmeasured` items separately. Task 2 freezes the corpus before any evaluation code exists, which makes the freeze structural rather than a promise.

- [ ] **Step 1: Write the failing test**

```rust
// cli/tests/suite/regularity_corpus.rs
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
        Some(Criterion::MedianInBand { lo: -1.2, hi: -0.8 }),
        "`Item.criterion` is `Option<Criterion>` — compare against `Some(..)`"
    );
}

#[test]
fn an_unknown_verdict_is_a_parse_error() {
    let bad = FIXTURE.replace("\"unmeasured\"", "\"probably\"");
    assert!(regularities::load(&bad).is_err(), "unknown verdict must not parse");
}

#[test]
fn an_unknown_criterion_kind_is_a_parse_error() {
    let bad = FIXTURE.replace("median-in-band", "vibes");
    assert!(regularities::load(&bad).is_err(), "unknown criterion must not parse");
}
```

- [ ] **Step 2: Run it and watch it fail**

Run: `cargo nextest run -p hornvale --test suite -- regularity_corpus`
Expected: FAIL to compile — `hornvale::regularities` does not exist.

- [ ] **Step 3: Write the module**

```rust
// cli/src/regularities.rs
//! The regularity corpus resolver: does the world GROW this?
//!
//! Sibling to `tropes` (representability), `systems` (implementation) and
//! `sentences` (grammar), and distinguished from all three by its resolution
//! basis — measurement over the committed census (decision 0135). Unlike
//! `systems`, this resolver reads a dataset; unlike `tropes`, it builds no
//! world.
use serde::Deserialize;

/// The declared corpora, in matrix-column order.
/// type-audit: bare-ok(artifact)
pub const CORPORA: &[&str] = &["regularities/sugarscape-1996.regularity.json"];

/// How one imported regularity stands against Hornvale.
///
/// Six verdicts plus one lifecycle state. Decision 0136's
/// refused/deferred/absent triple is preserved intact; `Flat` is this
/// family's addition — *measured, criterion unmet* — which no sibling family
/// can express, because a grammar either parses a sentence or does not.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum Verdict {
    /// Measured; the criterion is met. Cites a `doc:` anchor.
    Grown,
    /// Measured; the criterion is not met. Cites a `doc:` anchor.
    Flat,
    /// Hornvale deliberately will not. Cites a `decision:` anchor.
    Refused,
    /// The statistic cannot be computed yet. Cites a `registry:` anchor.
    Deferred,
    /// Cannot be computed and nobody has registered it. Cites nothing.
    Absent,
    /// About the source model's own abstraction, not a world regularity.
    /// Cites a `reason:` anchor.
    Inapplicable,
    /// Frozen but not yet measured. A lifecycle state, never a coverage
    /// verdict: the report tallies the six above and lists these separately.
    Unmeasured,
}

/// The frozen, falsifiable claim an item makes about the population.
///
/// Selected, never defined (decision 0011): the corpus supplies parameters,
/// the resolver owns the computation. A fifth kind is a code change, a
/// review and a test — deliberately, so "add a criterion shape" is never a
/// data edit.
/// type-audit: bare-ok(ratio: MedianInBand.lo), bare-ok(ratio: MedianInBand.hi), bare-ok(ratio: FractionInBandAtLeast.lo), bare-ok(ratio: FractionInBandAtLeast.hi), bare-ok(ratio: FractionInBandAtLeast.min_fraction), bare-ok(ratio: MedianAtLeast.bound), bare-ok(ratio: MedianAtMost.bound), bare-ok(ratio: PresentOnFraction.min_fraction)
#[derive(Debug, Clone, Copy, PartialEq, Deserialize)]
#[serde(tag = "kind", rename_all = "kebab-case")]
pub enum Criterion {
    /// The statistic's median over the population lies in `[lo, hi]`.
    MedianInBand {
        /// Inclusive lower edge.
        lo: f64,
        /// Inclusive upper edge.
        hi: f64,
    },
    /// At least `min_fraction` of worlds lie in `[lo, hi]`.
    FractionInBandAtLeast {
        /// Inclusive lower edge.
        lo: f64,
        /// Inclusive upper edge.
        hi: f64,
        /// Minimum share of worlds, in `[0, 1]`.
        min_fraction: f64,
    },
    /// The median is at least `bound`.
    MedianAtLeast {
        /// Inclusive lower bound.
        bound: f64,
    },
    /// The median is at most `bound`.
    MedianAtMost {
        /// Inclusive upper bound.
        bound: f64,
    },
    /// The statistic is non-absent on at least `min_fraction` of worlds.
    PresentOnFraction {
        /// Minimum share of worlds, in `[0, 1]`.
        min_fraction: f64,
    },
}

/// One imported regularity as authored in the corpus.
/// type-audit: bare-ok(identifier-text: id), bare-ok(prose: title), bare-ok(prose: source), bare-ok(count: emergence_type), bare-ok(identifier-text: statistic), bare-ok(identifier-text: anchor), bare-ok(prose: note)
#[derive(Debug, Clone, Deserialize)]
pub struct Item {
    /// Corpus-local identifier, e.g. `sug-wealth-skew`.
    pub id: String,
    /// The regularity, stated as the source states it.
    pub title: String,
    /// Where in the source it appears.
    pub source: String,
    /// 1 = the individual property is meaningful but only the collective
    /// exhibits it; 2 = only the collective property is meaningful.
    pub emergence_type: u8,
    /// The census column this item is measured through. Empty when the
    /// verdict is not a measured one.
    #[serde(default)]
    pub statistic: String,
    /// The frozen claim. Absent for non-measurable verdicts.
    #[serde(default)]
    pub criterion: Option<Criterion>,
    /// How it stands against Hornvale.
    pub verdict: Verdict,
    /// The anchor backing the verdict, absent only for `absent` and
    /// `unmeasured`.
    #[serde(default)]
    pub anchor: Option<String>,
    /// One line of human context. Never parsed.
    #[serde(default)]
    pub note: String,
}

/// A frozen, provenance-stamped catalogue of imported regularities.
/// type-audit: bare-ok(identifier-text: corpus), bare-ok(identifier-text: unit), bare-ok(flag: ordered), bare-ok(identifier-text: population), bare-ok(prose: provenance), bare-ok(prose: frozen)
#[derive(Debug, Clone, Deserialize)]
pub struct Corpus {
    /// Corpus identifier, e.g. `sugarscape-1996`.
    pub corpus: String,
    /// What the items are. Always `regularity` today.
    pub unit: String,
    /// Whether the items form a meaningful sequence. Gates ordinal readings
    /// (decision 0135's second schema commitment).
    pub ordered: bool,
    /// The study whose committed rows this corpus scores against.
    pub population: String,
    /// Where this catalogue comes from and what bias it carries.
    pub provenance: String,
    /// Note recording that the freeze preceded measurement.
    pub frozen: String,
    /// The items themselves, in corpus order.
    pub items: Vec<Item>,
}

/// Parse a corpus from JSON.
/// type-audit: bare-ok(artifact: json), bare-ok(prose: return)
pub fn load(json: &str) -> Result<Corpus, String> {
    serde_json::from_str(json).map_err(|e| format!("corpus parse: {e}"))
}
```

Add `pub mod regularities;` to `cli/src/lib.rs` between `pub mod provision;` and `pub mod repl;`.

Register the test module in `cli/tests/suite.rs`. **Every entry there is a two-line pair — an attribute then the `mod` — with no blank line between entries** (measured: 53 `mod` lines, 2 blank lines in the whole file):

```rust
#[path = "suite/regularity_corpus.rs"]
mod regularity_corpus;
```

It sorts between the `provision` pair and the `release_determinism` pair.

- [ ] **Step 4: Run the tests**

Run: `cargo nextest run -p hornvale --test suite -- regularity_corpus`
Expected: 3 passed.

- [ ] **Step 5: fmt, clippy, commit**

```bash
cargo fmt
cargo clippy --workspace --all-targets -- -D warnings
git add cli/src/regularities.rs cli/src/lib.rs cli/tests/suite/regularity_corpus.rs cli/tests/suite.rs
git commit -m "feat(regularities): the corpus schema and loader"
```

---

### Task 2: Author and freeze `sugarscape-1996`

**Files:**
- Create: `regularities/sugarscape-1996.regularity.json`
- Test: `cli/tests/suite/regularity_corpus.rs` (append)

**Interfaces:**
- Consumes: `regularities::{load, Corpus, Verdict}` from Task 1.
- Produces: the frozen corpus every later task scores.

**This task runs BEFORE any evaluation code exists.** That is the point: items are authored blind, so the freeze is structural rather than a promise. Every measurable item is authored `"verdict": "unmeasured"`. **Do not run any statistic against the census during this task.** The one disclosed exception is `sug-wealth-skew`, whose distribution was measured during the brainstorm that motivated the campaign — its `note` must say so verbatim (spec §8).

Author items from the source's Appendix B rule roster and its emergence claims. Each item needs `id`, `title`, `source`, `emergence_type`, and then either a `statistic` + `criterion` + `"verdict": "unmeasured"`, or a non-measurable verdict with its anchor.

- [ ] **Step 1: Write the corpus**

Guidance for each verdict at freeze time:

- A regularity with a census column → `statistic` + `criterion` + `unmeasured`.
- A regularity needing a statistic the census lacks (anything over *time* — migration waves, boom-bust, concentration-over-time) → `deferred`, `anchor: "registry:TOOL-a-regularity-corpus-can-measure-a-trajectory"`.
- A regularity whose mechanism Hornvale deliberately lacks → `refused` with a `decision:` anchor, only if a ratified decision actually says so. If none does, it is `absent`, not `refused`.
- A rule about the source model's own abstraction (lattice geometry, its `Ga` growback parameterization) → `inapplicable` with a `reason:`.
- Nothing else → `absent`, no anchor.

The census columns available (verified present; do not add metrics): `rank-size-slope`, `settlement-count`, `mean-population`, `total-population`, `raid-victim-rate`, `raid-initiator-rate`, `climate-displacement-events`, `tribute-relations-standing`, `granary-raid-phase-concentration`, `cascade-rules-fired-goblin`, `cascade-rules-fired-bugbear`, `plate-size-gini`, `band-count`.

`sug-wealth-skew` is authored exactly so:

```json
{
  "id": "sug-wealth-skew",
  "title": "Holdings are distributed far more unequally than the endowments that produce them",
  "source": "Ch. II, 'Emergence'; Animation II-3",
  "emergence_type": 2,
  "statistic": "rank-size-slope",
  "criterion": { "kind": "median-in-band", "lo": -1.2, "hi": -0.8 },
  "verdict": "unmeasured",
  "note": "NOT A BLIND TEST, disclosed under decision 0016: this statistic's distribution was measured during the brainstorm that motivated The Seedbed, before any corpus existed. Every other item in this corpus was authored before its statistic was looked at."
}
```

- [ ] **Step 2: Write the failing freeze tests**

```rust
#[test]
fn the_sugarscape_corpus_is_frozen_at_its_declared_size() {
    let c = load_sugarscape();
    // Assert the ACTUAL authored count here once the file is written. A
    // corpus's size is asserted so that changing it is a deliberate act
    // (decision 0016); update this number only alongside a re-freeze.
    assert_eq!(c.items.len(), /* authored count */ 0);
}

#[test]
fn the_corpus_declares_its_provenance_and_freeze() {
    let c = load_sugarscape();
    assert!(c.provenance.contains("Epstein"), "provenance names its source");
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
    }
}

fn load_sugarscape() -> hornvale::regularities::Corpus {
    let path = workspace_root().join(hornvale::regularities::CORPORA[0]);
    let json = std::fs::read_to_string(&path).expect("corpus file");
    hornvale::regularities::load(&json).expect("corpus parses")
}
```

Copy `workspace_root()` from `cli/tests/suite/system_coverage.rs:144`.

- [ ] **Step 3: Run, read the real count from the failure, fix the assertion**

Run: `cargo nextest run -p hornvale --test suite -- regularity_corpus`
Expected: the size test fails naming the real count. Put that number in the assertion. Do not adjust the corpus to hit a round number.

- [ ] **Step 4: Run again**

Expected: all pass.

- [ ] **Step 5: Commit**

```bash
cargo fmt
git add regularities/sugarscape-1996.regularity.json cli/tests/suite/regularity_corpus.rs
git commit -m "data(regularities): freeze the sugarscape-1996 corpus before measurement"
```

---

### Task 3: The `doc:` anchor and its generated-paths rule

**Files:**
- Modify: `cli/src/regularities.rs`
- Test: `cli/tests/suite/regularity_corpus.rs` (append)

**Interfaces:**
- Consumes: Task 1's types.
- Produces: `Anchor`, `Anchor::parse(&str) -> Option<Anchor>`, `GeneratedPaths`, `GeneratedPaths::read(&Path) -> Result<GeneratedPaths, String>`, `GeneratedPaths::has_generator(&self, path: &str) -> bool`.

The rule from spec §5: a `doc:` anchor resolves **only** if `docs/generated-paths.txt` gives that path a generator. That file is TSV; column 1 is the path, column 2 names the author. An author of the form `none(...)` means hand-written — refused.

**DO NOT WRITE A NEW PARSER FOR THAT FILE. Controller correction, made after reading the code:** `cli/src/attest.rs:332` already has `parse_declared(text) -> Vec<(String, DeclaredAuthor)>`, which skips comments and blank lines, splits on the tab, and classifies `none` / `none(<reason>)` against a real roster author — exactly this rule. It is stricter than a hand-rolled version: it **panics** on a row with no author column rather than silently skipping it. Its own doc comment records that it already "mirrors `cli/tests/suite/generated_paths.rs`'s `declared()`", so a third copy would be the second duplication of one rule, which is what decision 0261 exists to prevent.

**What to do instead:** make `parse_declared` and `DeclaredAuthor` `pub` in `attest.rs` (adding the doc comments `#![warn(missing_docs)]` will then demand on the enum's variants), and have `regularities` call it. `GeneratedPaths` keeps only the part attest does not do — **directory inheritance**, where a file inherits the author of the longest declared directory prefix ending in `/`. Build the map from `parse_declared`'s output; do not re-read or re-split the file.

The 252 comment lines in that file are why comment-skipping is load-bearing rather than defensive.

- [ ] **Step 1: Write the failing tests**

```rust
use hornvale::regularities::{Anchor, GeneratedPaths};

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
    assert_eq!(Anchor::parse("decision:0135"), Some(Anchor::Decision("0135".into())));
    assert_eq!(Anchor::parse("registry:TOOL-x"), Some(Anchor::Registry("TOOL-x".into())));
    assert_eq!(Anchor::parse("reason:because"), Some(Anchor::Reason("because".into())));
    assert_eq!(Anchor::parse("path:src/x.rs"), None, "path: is not admitted by this family");
    assert_eq!(Anchor::parse("nonsense"), None);
}
```

- [ ] **Step 2: Run and watch them fail**

Run: `cargo nextest run -p hornvale --test suite -- regularity_corpus`
Expected: FAIL to compile — `Anchor` and `GeneratedPaths` do not exist.

- [ ] **Step 3: Implement**

```rust
use std::collections::BTreeMap;
use std::path::Path;

/// A verdict's backing evidence, parsed from its `anchor` string.
///
/// Deliberately NARROWER than `systems::Anchor`: there is no `test:` and no
/// `path:` kind. A regularity is not demonstrated by code existing — it is
/// demonstrated by generated documentation stating the measured claim, which
/// is the only surface a reader outside the program can falsify.
/// type-audit: bare-ok(artifact: Doc.0), bare-ok(identifier-text: Decision.0), bare-ok(identifier-text: Registry.0), bare-ok(prose: Reason.0)
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Anchor {
    /// `doc:<path>` — a generated, drift-checked page. The terminal anchor.
    Doc(String),
    /// `decision:0135` — must be in force.
    Decision(String),
    /// `registry:TOOL-…` — must exist and not read `shipped`.
    Registry(String),
    /// `reason:<prose>` — for `inapplicable` only.
    Reason(String),
}

impl Anchor {
    /// Parse an anchor string. `None` when the prefix is unknown.
    /// type-audit: bare-ok(identifier-text: s)
    pub fn parse(s: &str) -> Option<Anchor> {
        let (kind, rest) = s.split_once(':')?;
        match kind {
            "doc" => Some(Anchor::Doc(rest.to_string())),
            "decision" => Some(Anchor::Decision(rest.to_string())),
            "registry" => Some(Anchor::Registry(rest.to_string())),
            "reason" => Some(Anchor::Reason(rest.to_string())),
            _ => None,
        }
    }
}

/// The declared generated paths and their authors, read from
/// `docs/generated-paths.txt` — the single source of truth for which paths
/// are regenerated and by what.
#[derive(Debug, Clone)]
pub struct GeneratedPaths {
    /// Declared path to whether its author is a real generator (`true`) or a
    /// declared `none(<reason>)` absence (`false`).
    authors: BTreeMap<String, bool>,
}

impl GeneratedPaths {
    /// Read the declarations from a repository root.
    ///
    /// Parsing is delegated to [`crate::attest::parse_declared`] — the
    /// repository's existing reader of this file and of its `none(<reason>)`
    /// convention. This type adds only directory inheritance.
    /// type-audit: bare-ok(artifact: root), bare-ok(prose: return)
    pub fn read(root: &Path) -> Result<GeneratedPaths, String> {
        let path = root.join("docs/generated-paths.txt");
        let text = std::fs::read_to_string(&path)
            .map_err(|e| format!("reading {}: {e}", path.display()))?;
        let authors: BTreeMap<String, bool> = crate::attest::parse_declared(&text)
            .into_iter()
            .map(|(p, author)| (p, matches!(author, crate::attest::DeclaredAuthor::Roster(_))))
            .collect();
        if authors.is_empty() {
            return Err("no declared generated paths".to_string());
        }
        Ok(GeneratedPaths { authors })
    }

    /// Whether this path has a real generator.
    ///
    /// **Direction this enforces:** declared-and-generated ⇒ admissible. It
    /// is blind to a path that is generated but undeclared, which is
    /// `cli/tests/suite/generated_paths.rs`'s job, not this one.
    /// type-audit: bare-ok(artifact: path), bare-ok(flag: return)
    pub fn has_generator(&self, path: &str) -> bool {
        if let Some(generated) = self.authors.get(path) {
            return *generated;
        }
        // A file inherits the LONGEST declared directory prefix's author
        // unless it overrides with a row of its own (handled above).
        self.authors
            .iter()
            .filter(|(decl, _)| decl.ends_with('/') && path.starts_with(decl.as_str()))
            .max_by_key(|(decl, _)| decl.len())
            .map(|(_, generated)| *generated)
            .unwrap_or(false)
    }
}
```

- [ ] **Step 4: Run the tests**

Expected: 4 passed. If `a_doc_anchor_into_hand_written_prose_is_refused` passes only because the path is missing rather than because it is `none(...)`, that is a false green — confirm `book/src/laboratory/overview.md` is present in the file with a `none(` author before accepting.

- [ ] **Step 5: Commit**

```bash
cargo fmt && cargo clippy --workspace --all-targets -- -D warnings
git add cli/src/regularities.rs cli/tests/suite/regularity_corpus.rs
git commit -m "feat(regularities): the doc: anchor, refused into hand-written prose"
```

---

### Task 4: Criterion evaluation

**Files:**
- Modify: `cli/src/regularities.rs`
- Test: `cli/tests/suite/regularity_corpus.rs` (append)

**Interfaces:**
- Consumes: `Criterion` from Task 1.
- Produces: `meets(&Criterion, &[f64], usize) -> bool` (values present, plus total world count for `PresentOnFraction`), and `values_of(&Census, &str) -> Vec<f64>`.

`meets` is pure over a slice so it is testable without any CSV. The census adapter is separate and thin.

- [ ] **Step 1: Write the failing tests**

```rust
use hornvale::regularities::{meets, Criterion};

#[test]
fn median_in_band_is_inclusive_at_both_edges() {
    let v = [-1.2, -1.0, -0.8];
    assert!(meets(&Criterion::MedianInBand { lo: -1.2, hi: -0.8 }, &v, 3));
    assert!(meets(&Criterion::MedianInBand { lo: -1.0, hi: -1.0 }, &v, 3));
    assert!(!meets(&Criterion::MedianInBand { lo: -0.5, hi: 0.0 }, &v, 3));
}

#[test]
fn fraction_in_band_counts_only_values_inside_it() {
    let v = [-1.0, -1.0, -0.5, -0.5];
    let c = Criterion::FractionInBandAtLeast { lo: -1.2, hi: -0.8, min_fraction: 0.5 };
    assert!(meets(&c, &v, 4), "exactly half is at least half");
    let c = Criterion::FractionInBandAtLeast { lo: -1.2, hi: -0.8, min_fraction: 0.75 };
    assert!(!meets(&c, &v, 4));
}

#[test]
fn present_on_fraction_measures_against_the_world_count_not_the_value_count() {
    // The distinction that matters: 2 present values out of 10 worlds is 20%,
    // not 100%. A criterion reading only the present slice would be vacuous.
    let v = [1.0, 2.0];
    assert!(!meets(&Criterion::PresentOnFraction { min_fraction: 0.5 }, &v, 10));
    assert!(meets(&Criterion::PresentOnFraction { min_fraction: 0.5 }, &v, 4));
}

#[test]
fn an_empty_population_never_meets_a_criterion() {
    assert!(!meets(&Criterion::MedianInBand { lo: -1.0, hi: 1.0 }, &[], 0));
    assert!(!meets(&Criterion::MedianAtLeast { bound: 0.0 }, &[], 0));
}

#[test]
fn one_sided_bounds_are_inclusive() {
    let v = [1.0, 2.0, 3.0];
    assert!(meets(&Criterion::MedianAtLeast { bound: 2.0 }, &v, 3));
    assert!(meets(&Criterion::MedianAtMost { bound: 2.0 }, &v, 3));
    assert!(!meets(&Criterion::MedianAtLeast { bound: 2.5 }, &v, 3));
}
```

- [ ] **Step 2: Run and watch them fail**

Expected: FAIL to compile — `meets` does not exist.

- [ ] **Step 3: Implement**

```rust
/// Whether a frozen criterion is met by the present values of a statistic.
///
/// `present` is the statistic's values over worlds that have one; `worlds` is
/// the population size including worlds where the statistic is absent. The
/// two differ, and `PresentOnFraction` is the criterion that cares.
/// type-audit: bare-ok(count: worlds), bare-ok(flag: return)
pub fn meets(criterion: &Criterion, present: &[f64], worlds: usize) -> bool {
    if let Criterion::PresentOnFraction { min_fraction } = criterion {
        if worlds == 0 {
            return false;
        }
        return present.len() as f64 / worlds as f64 >= *min_fraction;
    }
    if present.is_empty() {
        return false;
    }
    let mut sorted = present.to_vec();
    sorted.sort_by(f64::total_cmp);
    let mid = sorted.len() / 2;
    let median = if sorted.len() % 2 == 0 {
        (sorted[mid - 1] + sorted[mid]) / 2.0
    } else {
        sorted[mid]
    };
    match criterion {
        Criterion::MedianInBand { lo, hi } => median >= *lo && median <= *hi,
        Criterion::MedianAtLeast { bound } => median >= *bound,
        Criterion::MedianAtMost { bound } => median <= *bound,
        Criterion::FractionInBandAtLeast { lo, hi, min_fraction } => {
            let inside = present.iter().filter(|v| **v >= *lo && **v <= *hi).count();
            inside as f64 / present.len() as f64 >= *min_fraction
        }
        Criterion::PresentOnFraction { .. } => unreachable!("handled above"),
    }
}

/// The present numeric values of a statistic, in census row order.
///
/// `absent`, empty and unparseable cells are skipped — the same treatment
/// `windows/lab/src/domesday/stats.rs` gives them.
/// type-audit: bare-ok(identifier-text: statistic)
pub fn values_of(census: &hornvale_lab::domesday::census::Census, statistic: &str) -> Vec<f64> {
    census
        .values(statistic)
        .into_iter()
        .filter_map(|cell| cell.parse::<f64>().ok())
        .collect()
}
```

Note `sort_by(f64::total_cmp)` — the workspace bans naive float sorting.

- [ ] **Step 4: Run the tests**

Expected: 5 passed.

- [ ] **Step 5: Commit**

```bash
cargo fmt && cargo clippy --workspace --all-targets -- -D warnings
git add cli/src/regularities.rs cli/tests/suite/regularity_corpus.rs
git commit -m "feat(regularities): criterion evaluation over a census population"
```

---

### Task 5: The audit and the two-way regression guard

**Files:**
- Modify: `cli/src/regularities.rs`
- Test: `cli/tests/suite/regularity_corpus.rs` (append)

**Interfaces:**
- Consumes: everything above.
- Produces: `Finding`, `audit(&Corpus, &Census, &GeneratedPaths, &RepoFacts) -> Vec<Finding>`.

Reuse `systems::RepoFacts` for `decision:` and `registry:` resolution rather than reimplementing it — it is already `pub`.

**The guard is the reason this family exists as more than a report.** Coverage in the sibling families is a ratchet; a grown regularity is emergent and any retune of the history bake can destroy it silently. Both directions are red.

- [ ] **Step 1: Write the failing tests**

```rust
use hornvale::regularities::{audit, Finding};

// Build a one-item corpus with a chosen authored verdict over a chosen
// value set, so both guard directions can be driven deliberately.
fn one_item_corpus(verdict: &str, anchor: &str) -> hornvale::regularities::Corpus {
    let json = format!(
        r#"{{"corpus":"t","unit":"regularity","ordered":false,
             "population":"the-census","provenance":"p",
             "frozen":"before first measurement, t",
             "items":[{{"id":"i","title":"T","source":"S","emergence_type":2,
               "statistic":"rank-size-slope",
               "criterion":{{"kind":"median-in-band","lo":-1.2,"hi":-0.8}},
               "verdict":"{verdict}","anchor":"{anchor}","note":""}}]}}"#
    );
    hornvale::regularities::load(&json).expect("fixture parses")
}

#[test]
fn a_lost_regularity_is_red() {
    // Authored `grown`; the live census median is -0.578, outside the band.
    let c = one_item_corpus("grown", "doc:book/src/domesday/demography.md");
    let findings = audit(&c, &census(), &generated_paths(), &facts());
    assert!(
        findings.iter().any(|f| matches!(f, Finding::Regressed { .. })),
        "an authored `grown` that no longer measures grown must be RED: {findings:?}"
    );
}

#[test]
fn stale_pessimism_is_also_red() {
    // Authored `flat` against a criterion the census DOES meet. A real gain
    // must be claimed deliberately, in a commit that says so.
    let json = r#"{"corpus":"t","unit":"regularity","ordered":false,
      "population":"the-census","provenance":"p",
      "frozen":"before first measurement, t",
      "items":[{"id":"i","title":"T","source":"S","emergence_type":2,
        "statistic":"rank-size-slope",
        "criterion":{"kind":"median-in-band","lo":-1.0,"hi":0.0},
        "verdict":"flat","anchor":"doc:book/src/domesday/demography.md",
        "note":""}]}"#;
    let c = hornvale::regularities::load(json).expect("parses");
    let findings = audit(&c, &census(), &generated_paths(), &facts());
    assert!(
        findings.iter().any(|f| matches!(f, Finding::Regressed { .. })),
        "an authored `flat` that now measures grown must be RED: {findings:?}"
    );
}

#[test]
fn agreement_raises_nothing() {
    let c = one_item_corpus("flat", "doc:book/src/domesday/demography.md");
    let findings = audit(&c, &census(), &generated_paths(), &facts());
    assert!(findings.is_empty(), "authored flat, measures flat: {findings:?}");
}

#[test]
fn a_measured_verdict_anchored_to_hand_written_prose_is_unjustified() {
    let c = one_item_corpus("flat", "doc:book/src/laboratory/overview.md");
    let findings = audit(&c, &census(), &generated_paths(), &facts());
    assert!(findings.iter().any(|f| matches!(f, Finding::Unjustified { .. })));
}

#[test]
fn an_unmeasured_item_raises_nothing_and_needs_no_anchor() {
    let json = r#"{"corpus":"t","unit":"regularity","ordered":false,
      "population":"the-census","provenance":"p",
      "frozen":"before first measurement, t",
      "items":[{"id":"i","title":"T","source":"S","emergence_type":2,
        "statistic":"rank-size-slope",
        "criterion":{"kind":"median-in-band","lo":-1.2,"hi":-0.8},
        "verdict":"unmeasured","note":""}]}"#;
    let c = hornvale::regularities::load(json).expect("parses");
    assert!(audit(&c, &census(), &generated_paths(), &facts()).is_empty());
}
```

Helpers: `census()` loads via `hornvale_lab::domesday::census::load(&workspace_root().join(hornvale_lab::CENSUS_GOLDENS_DIR).join("the-census"))`; `generated_paths()` is `GeneratedPaths::read(&workspace_root()).unwrap()`; `facts()` mirrors `system_coverage.rs:228`.

- [ ] **Step 2: Run and watch each fail**

Expected: FAIL to compile. Then, once `audit` exists but before the guard is written, the two `Regressed` tests must fail on the assertion — **observe that red before implementing the guard**. A red from a compile error proves nothing about an assertion.

- [ ] **Step 3: Implement**

```rust
/// Something wrong with an item, found by re-checking it against live state.
pub enum Finding {
    /// A verdict with no anchor, the wrong kind of anchor, or an anchor into
    /// hand-written prose.
    Unjustified {
        /// The item's corpus-local id.
        id: String,
        /// What is wrong, in a sentence.
        why: String,
    },
    /// The anchor stopped resolving.
    Dangling {
        /// The item's corpus-local id.
        id: String,
        /// The anchor as authored.
        anchor: String,
        /// What would have caused this, and the legitimate repairs.
        why: String,
    },
    /// The authored verdict and the measured verdict disagree — in either
    /// direction. A `grown` that went flat is a lost regularity; a `flat`
    /// that went grown is stale pessimism.
    Regressed {
        /// The item's corpus-local id.
        id: String,
        /// What the corpus claims.
        authored: Verdict,
        /// What the census says today.
        computed: Verdict,
        /// The measured summary that settles it.
        why: String,
    },
}
```

`audit` walks the items: skip `Unmeasured` entirely; for `Grown`/`Flat`, compute the verdict with `meets` and raise `Regressed` on disagreement, and require a `Doc` anchor whose path `has_generator`; for `Refused`/`Deferred`/`Inapplicable`, check the anchor kind matches and resolves, exactly as `systems::audit_item` does; for `Absent`, require no anchor.

- [ ] **Step 4: Run the tests**

Expected: 5 passed, and the two `Regressed` tests were observed red on the assertion first.

- [ ] **Step 5: Measure the guard's cost and place it (spec §6)**

Run: `cargo nextest run -p hornvale --test suite -- regularity_corpus 2>&1 | tail -5`

Read the per-test durations. **Branch table, not a prediction:**

- every test under the sub-floor roster's threshold → add rows to
  `docs/timings/subfloor-roster.tsv`'s successor mechanism by letting a green
  chamber run write them. **Never hand-author a roster row and never mark one
  `DECLARED_ABSENT`** — only a green run may write the roster.
- any test over the threshold → leave it out of the commit gate; it runs in the
  stage gate with the rest of the workspace suite. Say which in the task report.

Do not assume the first branch: the resolver loads a 1,001-row, 290-column CSV.

- [ ] **Step 6: Commit**

```bash
cargo fmt && cargo clippy --workspace --all-targets -- -D warnings
git add cli/src/regularities.rs cli/tests/suite/regularity_corpus.rs
git commit -m "feat(regularities): the two-way regression guard"
```

---

### Task 6: Report, CLI subcommand, committed artifact

**Files:**
- Modify: `cli/src/regularities.rs`, `cli/src/main.rs`, `scripts/regenerate-artifacts.sh`, `docs/generated-paths.txt`
- Create: `docs/audits/regularity-coverage-sugarscape-1996.md`
- Test: `cli/tests/suite/regularity_coverage.rs`, `cli/tests/suite.rs` (register as a two-line `#[path = "suite/regularity_coverage.rs"]` + `mod regularity_coverage;` pair, no blank line — see Task 1)

**Interfaces:**
- Consumes: everything above.
- Produces: `render(&Corpus, &Census, &str) -> String`, `artifact_path(&Corpus) -> String`, `regenerate_command(&str) -> String`, and `hornvale regularities [report|check] [--corpus <PATH>]`.

The report opens with the same instrument disclaimer `docs/audits/trope-matrix.md` carries (decision 0095: a reading through one biased ruler, never a grade), tallies the six verdicts, lists `unmeasured` items separately, and reports the **emergence-type split** — how many type-1 versus type-2 items the corpus holds and how many of each are grown. `emergence_type` is `Option<u8>`: items where the taxonomy does not apply are `null` and must be excluded from the split, never counted as either type.

**Four reporting requirements Task 2's review established. The report is wrong without them:**

1. **Four measurable items, three independent claims.** `sug-retaliation-deters` and `sug-predation-is-bounded` both read raid rates off the same `raid_attribution` fold over the same population and differ only in numerator. Report the independent-claim count beside the item count.
2. **Re-measure that collinearity; do not quote it.** The 1.00–1.03 initiator/victim ratio in the metric's rustdoc comes from a 12-world scratch probe, explicitly *not* the census. It is an inherited diagnosis, so compute the correlation on the population actually being scored and print what you measure.
3. **`absent` is two different things and the report must split them.** The economics and disease items are a real absence of mechanism. But `sug-seasonal-phase-lock`, `sug-externality-displaces` and `sug-heterogeneous-landscape` each have a *named, small, known* right instrument in their notes (a Rayleigh criterion kind; `median-at-least: 1.0`; a Gini over habitable capacity). Those are roadmap, not gap. No verdict value distinguishes them — the notes do.
4. **Report power, not just verdicts.** The surviving criteria are conservative floors authored blind. A near-uniform `grown` sweep across four items is *not* evidence of reach, and the report must say so in its own text rather than leaving a reader to infer it.

- [ ] **Step 1: Write the failing drift test**

```rust
#[test]
fn committed_regularity_coverage_matches_the_live_report() {
    let corpus = load_sugarscape();
    let live = hornvale::regularities::render(&corpus, &census(), hornvale::regularities::CORPORA[0]);
    let path = workspace_root().join(hornvale::regularities::artifact_path(&corpus));
    let committed = std::fs::read_to_string(&path).unwrap_or_default();
    assert_eq!(
        committed, live,
        "committed report is stale — regenerate with `{}`",
        hornvale::regularities::regenerate_command(&hornvale::regularities::artifact_path(&corpus))
    );
}
```

- [ ] **Step 2: Run it and watch it fail**

Expected: FAIL — the committed artifact does not exist.

- [ ] **Step 3: Implement render, wire the subcommand, generate the artifact**

Mirror `cmd_systems` in `cli/src/main.rs:1266`, including its `--corpus` flag handling and its usage lines. Add the regeneration to `scripts/regenerate-artifacts.sh` beside the other `docs/audits/` writers, with the `>` redirect in the script, not in the command.

- [ ] **Step 4: Declare the artifact and check tracked-ness**

`docs/audits/` is already declared with author `artifacts`, so the new file inherits it — but `git diff --exit-code` is **silently vacuous against a path with no index entry**, so the file must be `git add`ed in this same commit or the drift check can never fail. Declare the file by name as well, so the tracked-ness check refuses until it is added:

```
docs/audits/regularity-coverage-sugarscape-1996.md	artifacts
```

- [ ] **Step 5: Run the drift check and the full prose gate**

Run: `make rebaseline && git diff --exit-code -- $(grep -v '^#' docs/generated-paths.txt | grep -v '^$' | cut -f1)`
Expected: empty diff after the artifact is committed.

**Branch table, not a prediction** — after `make rebaseline`:
- only `docs/audits/regularity-coverage-sugarscape-1996.md` moved → commit it here.
- `docs/audits/type-audit-report.md` also moved → expected, this task adds `pub` items; commit it in the same commit.
- anything under `book/src/gallery/` or `book/src/laboratory/generated/` moved → **STOP**. Nothing in this plan may move a world or a census; investigate before committing.

- [ ] **Step 6: Commit**

```bash
cargo fmt && cargo clippy --workspace --all-targets -- -D warnings
git add cli/src/regularities.rs cli/src/main.rs cli/tests/suite/regularity_coverage.rs \
        cli/tests/suite.rs scripts/regenerate-artifacts.sh docs/generated-paths.txt \
        docs/audits/regularity-coverage-sugarscape-1996.md docs/audits/type-audit-report.md
git commit -m "feat(regularities): the coverage report and its committed artifact"
```

---

### Task 7: The first measurement

**Files:**
- Modify: `regularities/sugarscape-1996.regularity.json`, `cli/tests/suite/regularity_corpus.rs`
- Modify: `docs/audits/regularity-coverage-sugarscape-1996.md` (regenerated)

**Interfaces:**
- Consumes: everything above.
- Produces: a corpus with no `unmeasured` items, and verdicts the guard now pins.

This is the moment the frozen predictions meet the data. **Record what is measured. Do not adjust a criterion after seeing its result** — a falsified prediction is a finding, and several campaigns ship the null as the headline (decision 0016).

- [ ] **Step 1: Run the resolver and read the computed verdicts**

Run: `cargo run -p hornvale -- regularities check`
Expected: one `Regressed` finding per `unmeasured` item, each naming the computed verdict and its measured summary.

- [ ] **Step 2: Transcribe each computed verdict into the corpus**

For each item, set `verdict` to what was measured and add the `doc:` anchor. Do not touch `criterion`, `lo`, `hi` or `min_fraction`.

- [ ] **Step 3: Add the no-unmeasured-items ratchet**

```rust
#[test]
fn the_corpus_has_been_measured() {
    let c = load_sugarscape();
    let pending: Vec<&str> = c.items.iter()
        .filter(|i| i.verdict == Verdict::Unmeasured)
        .map(|i| i.id.as_str())
        .collect();
    assert!(pending.is_empty(), "unmeasured after the first run: {pending:?}");
}

#[test]
fn the_falsification_clause_has_not_fired() {
    // Spec §8: if more than half the corpus is `inapplicable`, the finding is
    // that Sugarscape is the wrong first corpus — NOT that Hornvale failed.
    // Report it; do not re-author the corpus to raise the score.
    let c = load_sugarscape();
    let n = c.items.iter().filter(|i| i.verdict == Verdict::Inapplicable).count();
    assert!(
        n * 2 <= c.items.len(),
        "{n} of {} items inapplicable — the falsification clause has fired, and \
         that is a finding to report, not a corpus to re-author",
        c.items.len()
    );
}
```

- [ ] **Step 4: Regenerate and commit**

```bash
cargo run -p hornvale -- regularities check   # expect: no findings
make rebaseline
cargo fmt
git add regularities/sugarscape-1996.regularity.json cli/tests/suite/regularity_corpus.rs \
        docs/audits/regularity-coverage-sugarscape-1996.md
git commit -m "data(regularities): the first measurement — record what the census says"
```

---

### Task 8: The Domesday claim line

**Files:**
- Modify: `windows/lab/src/domesday/render.rs`
- Test: `windows/lab/tests/suite/` (a new module, registered in that crate's suite)
- Modify: `book/src/domesday/*.md` (regenerated)

**Interfaces:**
- Consumes: the frozen corpus, read as data.
- Produces: a criterion-and-verdict line under every metric a corpus scores.

Spec §5's teeth. Today the Domesday prints a stats table nobody can be wrong about; it gains a sentence a reader can catch us on:

```
sugarscape-1996 `sug-wealth-skew`: predicted median in [-1.2, -0.8]
(Zipf/Auerbach rank-size); measured -0.578, 1.7% of worlds in band. FLAT.
```

**Layering — decided here, not deferred to the implementer.** `windows/lab` may not depend on `cli`, and the corpus schema does not belong in the kernel (that crate is the determinism substrate). The resolver stays in `cli/` for family consistency with `tropes`/`systems`, and `windows/lab` reads the corpus with its own minimal structs — reading only the four fields it renders. That is a **deliberate duplication**, so under decision 0261 it carries a two-way agreement test (Step 4 below). Do not add a `cli` dependency to a window.

- [ ] **Step 1: Write the failing tests**

```rust
use hornvale_lab::domesday::render::claim_line;
use hornvale_lab::domesday::corpus::{ScoredItem, Verdict};

#[test]
fn a_scored_metric_renders_its_criterion_verdict_and_measurement() {
    let item = ScoredItem {
        corpus: "sugarscape-1996".into(),
        id: "sug-wealth-skew".into(),
        statistic: "rank-size-slope".into(),
        criterion_prose: "median in [-1.2, -0.8]".into(),
        verdict: Verdict::Flat,
    };
    let line = claim_line(&item, -0.578);
    assert!(line.contains("sug-wealth-skew"), "{line}");
    assert!(line.contains("median in [-1.2, -0.8]"), "{line}");
    assert!(line.contains("-0.578"), "{line}");
    assert!(line.contains("FLAT"), "the verdict is shouted, not buried: {line}");
}

#[test]
fn an_unscored_metric_gains_no_claim_line() {
    // The load-bearing negative: the Domesday must not sprout a claim for a
    // metric no frozen corpus scores.
    let page = render_demography_with(&[]);
    assert!(!page.contains("predicted"), "no corpus scores it, no claim");
}

#[test]
fn the_two_readers_of_the_corpus_agree() {
    // Decision 0261: this schema is duplicated on purpose (cli owns the
    // resolver, lab owns the renderer's minimal view), so the duplication
    // carries a two-way agreement test. Every id and verdict lab reads must
    // match what the cli resolver reads from the same file.
    let via_lab = hornvale_lab::domesday::corpus::read(&corpus_path())
        .expect("lab reads the corpus");
    let json = std::fs::read_to_string(corpus_path()).expect("corpus file");
    let via_cli = hornvale::regularities::load(&json).expect("cli reads the corpus");
    let lab_ids: Vec<&str> = via_lab.iter().map(|i| i.id.as_str()).collect();
    let cli_ids: Vec<&str> = via_cli.items.iter().map(|i| i.id.as_str()).collect();
    assert_eq!(lab_ids, cli_ids, "the two readers disagree about the corpus");
}
```

The third test needs `hornvale` as a `dev-dependency` of `hornvale-lab`. **If that
inverts the layering** (`cli` already depends on `hornvale-lab`, so a dev-dep the
other way is a dev-only cycle cargo permits but `cli/tests/suite/architecture.rs`
may refuse): put this one test in `cli/tests/suite/regularity_coverage.rs`
instead, where both crates are already in scope, and say so in the task report.
Check `architecture.rs` before choosing.

- [ ] **Step 2: Run and watch them fail**

Run: `cargo nextest run -p hornvale-lab --test suite -- domesday`
Expected: FAIL to compile — `claim_line` and `domesday::corpus` do not exist.

- [ ] **Step 3: Implement**

```rust
// windows/lab/src/domesday/corpus.rs — the minimal view, four fields.
/// One scored item, as the Domesday needs to render it.
/// type-audit: bare-ok(identifier-text: corpus), bare-ok(identifier-text: id), bare-ok(identifier-text: statistic), bare-ok(prose: criterion_prose)
pub struct ScoredItem {
    /// Which corpus scored it.
    pub corpus: String,
    /// The item's corpus-local id.
    pub id: String,
    /// The census column it is measured through.
    pub statistic: String,
    /// The frozen criterion, already rendered to prose.
    pub criterion_prose: String,
    /// What measuring found.
    pub verdict: Verdict,
}

/// The claim line printed under a scored metric's stats table.
///
/// The whole point of spec §5: the stats table alone is a number nobody can
/// be wrong about; this is a sentence a reader can catch us on.
/// type-audit: bare-ok(ratio: measured), bare-ok(prose: return)
pub fn claim_line(item: &ScoredItem, measured: f64) -> String {
    format!(
        "{} `{}`: predicted {}; measured {measured}. {}.",
        item.corpus,
        item.id,
        item.criterion_prose,
        match item.verdict {
            Verdict::Grown => "GROWN",
            Verdict::Flat => "FLAT",
        }
    )
}
```

- [ ] **Step 4: Regenerate and check the branch table**

Run: `make rebaseline`
- only `book/src/domesday/` moved → expected; commit.
- `book/src/laboratory/generated/` moved → **STOP**, a census must not move here.

- [ ] **Step 5: Commit**

```bash
cargo fmt && cargo clippy --workspace --all-targets -- -D warnings
git add windows/lab/src/domesday/render.rs windows/lab/tests/suite book/src/domesday
git commit -m "feat(domesday): render the criterion and verdict beside the number"
```

---

## Closing the campaign

Definition of Done adds a chronicle entry (`book/src/chronicle/the-seedbed.md`), a freshness sweep, a retrospective (`docs/retrospectives/the-seedbed.md`), and the `campaign-reconciliation.tsv` row moved from `active` to its close disposition. Use the `closing-a-campaign` skill; do not hand-roll it.

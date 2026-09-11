//! The `technologies/` corpus loader: does a *people* acquire, hold, and
//! lose this capability?
//!
//! Sixth corpus family (decision 0135's basis test; see `technologies/
//! CLAUDE.md` for the family's own law). Closest to `regularities/`
//! (`cli/src/regularities.rs`, the sibling this module's shape mirrors) and
//! distinguished from it by a per-people capability trajectory rather than a
//! macro-statistic over a population, by demands **derived** by transitive
//! closure over `presupposes` (decision 0386) rather than declared, and by
//! the one new verdict `lost`.
//!
//! **The loader, the freeze, and the prerequisite lattice** (Tasks 3-4 of
//! The Kiln). It parses a corpus and fails loudly on malformed input — an
//! unrecognized `verdict` or `criterion.kind` is a parse error naming the
//! offending value, never a silent default, and [`parse`] additionally
//! rejects a `presupposes` edge naming an unknown item or forming a cycle,
//! for the same reason. [`derived_demands`] computes the transitive closure
//! over `presupposes` on read (decision 0386) — never materialised into the
//! file. The item-count freeze itself lives in the test suite
//! (`cli/tests/suite/technology_corpus.rs`), matching the sibling
//! families. Anchor resolution and criterion evaluation are later tasks'
//! work and deliberately do not live here yet: [`Criterion`] is a minimal
//! round-tripping type today, not something this module evaluates.
//!
//! The corpus is DATA (`technologies/*.technology.json`) and this module is
//! its RESOLVER (decision 0011). Nothing in `domains/*` or `windows/*` reads
//! a corpus file.
use serde::Deserialize;
use std::collections::{BTreeMap, BTreeSet};
use std::path::Path;

/// How one imported technology-capability item stands against Hornvale.
///
/// Nine values: decision 0136's `present`/`refused`/`deferred`/`absent`/
/// `inapplicable` five, `regularities/`'s measured `grown`/`flat`/
/// `unmeasured` three, and this family's own addition, `lost` — a capability
/// a people held and released, the one axis no sibling family can express
/// (`technologies/CLAUDE.md`, "`lost` names a scope, and must say which").
#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum Verdict {
    /// Every derived demand is met. Cites a mechanism anchor.
    Present,
    /// Hornvale deliberately will not. Cites a `decision:` anchor.
    Refused,
    /// Planned, not built. Cites a `registry:` anchor.
    Deferred,
    /// Nobody's yet. Cites nothing — the honest red.
    Absent,
    /// The world deliberately lacks a precondition. Cites a `reason:` anchor.
    Inapplicable,
    /// Measured: a people acquires and keeps it. Cites a `doc:` anchor.
    Grown,
    /// Measured: never acquired. Cites a `doc:` anchor.
    Flat,
    /// Measured: acquired, then given up — a people that held the capability
    /// no longer holds it (`technologies/CLAUDE.md`'s scope for this value).
    /// Cites a `doc:` anchor.
    Lost,
    /// Frozen, not yet scored: reach passed, trajectory unscored. A
    /// lifecycle state, never a coverage verdict. Cites a mechanism anchor.
    Unmeasured,
}

/// The frozen, falsifiable claim an item makes about the statistic's
/// distribution across peoples.
///
/// **Minimal on purpose.** `Criterion` is Task 6's to define and evaluate;
/// this shape only needs to round-trip what Tasks 1 and 2 authored, which is
/// exactly one shape in both frozen corpora today
/// (`{"kind": "fraction-in-band", "lo": ..., "hi": ...}`). An unrecognized
/// `kind` is a parse error naming the offending value, via `serde`'s own
/// tagged-enum deserialization — the same mechanism `regularities::Criterion`
/// relies on, not a hand-written check.
/// type-audit: bare-ok(ratio: FractionInBand.lo), bare-ok(ratio: FractionInBand.hi)
#[derive(Debug, Clone, Copy, PartialEq, Deserialize)]
#[serde(tag = "kind", rename_all = "kebab-case")]
pub enum Criterion {
    /// The statistic's value across peoples lies in the inclusive band
    /// `[lo, hi]`.
    FractionInBand {
        /// Inclusive lower edge.
        lo: f64,
        /// Inclusive upper edge.
        hi: f64,
    },
}

/// One capability item as authored in the corpus.
///
/// Carries every field the corpora author (pre-flight ruling F-A): a struct
/// omitting `source`, `statistic`, `criterion`, `contested`, `disclosure` or
/// `note` would silently drop data every later task needs. The five
/// `Option` fields are genuinely optional — an item with no measurable
/// criterion has none, and `contested`/`disclosure` are authored only where
/// they apply.
/// type-audit: bare-ok(identifier-text: id), bare-ok(prose: title), bare-ok(prose: source), bare-ok(identifier-text: introduces), bare-ok(identifier-text: presupposes), bare-ok(identifier-text: anchor), bare-ok(identifier-text: statistic), bare-ok(flag: contested), bare-ok(prose: disclosure), bare-ok(prose: note)
#[derive(Debug, Clone, Deserialize)]
pub struct Item {
    /// Corpus-local identifier, e.g. `inv-writing`.
    pub id: String,
    /// The capability, stated as the source states it.
    pub title: String,
    /// Where in the source this item comes from.
    #[serde(default)]
    pub source: String,
    /// The one demand this item introduces to the derived closure (decision
    /// 0386). Never a list — an item names exactly one.
    pub introduces: String,
    /// The demands (by `id`) this item's own `introduces` token presupposes.
    /// The full demand set is the transitive closure over this edge,
    /// computed on read and never written into the file — this loader
    /// carries the edges only; closure derivation is a later task's.
    pub presupposes: Vec<String>,
    /// How it stands against Hornvale.
    pub verdict: Verdict,
    /// The anchor backing the verdict, as authored — an empty string for
    /// `absent`. Left as raw text here: parsing an anchor's kind and
    /// resolving it against live repo state is Task 5's job, not the
    /// loader's.
    pub anchor: String,
    /// The census-shaped statistic this item is measured through. `None`
    /// where the item carries no measurable claim.
    pub statistic: Option<String>,
    /// The frozen claim. `None` where the item carries no measurable claim.
    pub criterion: Option<Criterion>,
    /// Whether the source itself leaves open whether this capability was
    /// ever held (`technologies/CLAUDE.md`'s "marked rather than resolved").
    /// Present only in `henrich-2004-extended`; absent means the question
    /// does not arise for this item.
    pub contested: Option<bool>,
    /// Why this item's verdict is NOT a blind test, when it is not one — see
    /// `technologies/CLAUDE.md`'s chosen/inherited cut. `None` is the
    /// ordinary case.
    pub disclosure: Option<String>,
    /// One line (or more) of human context. Never parsed.
    pub note: Option<String>,
}

/// A frozen, provenance-stamped catalogue of imported technology-capability
/// items.
/// type-audit: bare-ok(identifier-text: corpus), bare-ok(identifier-text: unit), bare-ok(flag: ordered), bare-ok(prose: provenance), bare-ok(prose: frozen)
#[derive(Debug, Clone, Deserialize)]
pub struct Corpus {
    /// Corpus identifier, e.g. `asimov-1989`.
    pub corpus: String,
    /// What the items are. Always `technology` today.
    pub unit: String,
    /// Whether the items form a meaningful sequence.
    pub ordered: bool,
    /// Where this catalogue comes from and what bias it carries.
    pub provenance: String,
    /// Note recording that the freeze preceded measurement (decision 0016).
    pub frozen: String,
    /// The items themselves, in corpus order.
    pub items: Vec<Item>,
}

/// Parse a corpus from JSON text.
///
/// `&str`, not a path (pre-flight ruling F-B), so a parse test can exercise
/// a fixture string without touching the frozen corpora on disk.
///
/// **Fails loudly, by panicking**, rather than returning a `Result`: this
/// loader has no legitimate caller that would recover from malformed corpus
/// JSON, and an unrecognized `verdict` or `criterion.kind` must name the
/// offending value rather than default silently — which `serde_json`'s own
/// tagged-enum error already does, so the panic message is not hand-rolled.
/// The same standard applies to the `presupposes` lattice: a `presupposes`
/// naming an unknown item, or a cycle among items, is checked here and
/// panics naming the offending id, rather than reaching [`derived_demands`]
/// as a hang or a silently incomplete closure.
/// type-audit: bare-ok(artifact: text)
pub fn parse(text: &str) -> Corpus {
    match serde_json::from_str::<Corpus>(text) {
        Ok(corpus) => {
            validate_lattice(&corpus);
            corpus
        }
        Err(e) => panic!("technology corpus parse: {e}"),
    }
}

/// Check the `presupposes` lattice for the two ways it can be malformed:
/// an edge naming an item not in this corpus (decision 0386: "names an item
/// in this corpus and nothing else"), and a cycle, which would make
/// [`derived_demands`]'s closure loop forever. Panics naming the offending
/// id on either failure; does nothing on a clean lattice.
///
/// Cycle detection is iterative (an explicit stack, not recursion), so a
/// pathological corpus fails with a panic naming the offending id rather
/// than a stack overflow.
fn validate_lattice(corpus: &Corpus) {
    let by_id: BTreeMap<&str, &Item> = corpus.items.iter().map(|i| (i.id.as_str(), i)).collect();

    for item in &corpus.items {
        for p in &item.presupposes {
            if !by_id.contains_key(p.as_str()) {
                panic!(
                    "technology corpus: item {:?} presupposes unknown item {:?}",
                    item.id, p
                );
            }
        }
    }

    #[derive(Clone, Copy, PartialEq)]
    enum Mark {
        /// On the current DFS path — a back-edge to this id is a cycle.
        Visiting,
        /// Fully explored; safe to skip.
        Done,
    }
    let mut marks: BTreeMap<&str, Mark> = BTreeMap::new();

    for &start in by_id.keys() {
        if marks.contains_key(start) {
            continue;
        }
        // Iterative DFS: each stack frame is (id, index of its next
        // `presupposes` edge to visit), standing in for a recursive call's
        // local state.
        let mut stack: Vec<(&str, usize)> = vec![(start, 0)];
        marks.insert(start, Mark::Visiting);
        while let Some((id, edge_idx)) = stack.pop() {
            let item = by_id[id];
            if edge_idx < item.presupposes.len() {
                let next = item.presupposes[edge_idx].as_str();
                stack.push((id, edge_idx + 1));
                match marks.get(next) {
                    Some(Mark::Visiting) => {
                        panic!("technology corpus: cycle in presupposes lattice at {next:?}");
                    }
                    Some(Mark::Done) => {}
                    None => {
                        marks.insert(next, Mark::Visiting);
                        stack.push((next, 0));
                    }
                }
            } else {
                marks.insert(id, Mark::Done);
            }
        }
    }
}

/// The demand set for item `id`: the transitive closure of `presupposes`,
/// collecting each reached item's `introduces` — including `id`'s own, which
/// is why a root's demand set is never empty (decision 0386).
///
/// Computed on read and never written into the corpus (decision 0261: it
/// would state one fact twice). Assumes the corpus already passed
/// [`parse`]'s lattice validation — an unknown id in `presupposes` or a
/// cycle cannot reach this function from a corpus loaded through [`parse`]
/// or [`load`], so no further cycle or dangling-reference handling is done
/// here.
/// type-audit: bare-ok(identifier-text: id), bare-ok(identifier-text: return)
pub fn derived_demands(c: &Corpus, id: &str) -> BTreeSet<String> {
    let by_id: BTreeMap<&str, &Item> = c.items.iter().map(|i| (i.id.as_str(), i)).collect();
    let mut demands = BTreeSet::new();
    let mut seen: BTreeSet<&str> = BTreeSet::new();
    let mut stack: Vec<&str> = vec![id];
    while let Some(current) = stack.pop() {
        if !seen.insert(current) {
            continue;
        }
        if let Some(item) = by_id.get(current) {
            demands.insert(item.introduces.clone());
            for p in &item.presupposes {
                stack.push(p.as_str());
            }
        }
    }
    demands
}

/// Read a corpus file from `path` and delegate to [`parse`].
///
/// Fails loudly on a missing or unreadable file too, naming the path and the
/// io error, for the same reason [`parse`] panics rather than returning a
/// `Result`.
pub fn load(path: &Path) -> Corpus {
    let text = match std::fs::read_to_string(path) {
        Ok(text) => text,
        Err(e) => panic!("reading {}: {e}", path.display()),
    };
    parse(&text)
}

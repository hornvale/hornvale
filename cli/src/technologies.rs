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
//! **This is the loader only** (Task 3 of The Kiln). It parses a corpus and
//! fails loudly on malformed input — an unrecognized `verdict` or
//! `criterion.kind` is a parse error naming the offending value, never a
//! silent default. The freeze assertions (item count, disclosure
//! discipline), the derived-demand closure, anchor resolution and criterion
//! evaluation are later tasks' work and deliberately do not live here yet:
//! [`Criterion`] is a minimal round-tripping type today, not something this
//! module evaluates.
//!
//! The corpus is DATA (`technologies/*.technology.json`) and this module is
//! its RESOLVER (decision 0011). Nothing in `domains/*` or `windows/*` reads
//! a corpus file.
use serde::Deserialize;
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
/// type-audit: bare-ok(artifact: text)
pub fn parse(text: &str) -> Corpus {
    match serde_json::from_str(text) {
        Ok(corpus) => corpus,
        Err(e) => panic!("technology corpus parse: {e}"),
    }
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

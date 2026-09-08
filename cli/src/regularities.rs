//! The regularity corpus resolver: does the world GROW this?
//!
//! Sibling to `tropes` (representability), `systems` (implementation) and
//! `sentences` (grammar), and distinguished from all three by its resolution
//! basis — measurement over the committed census (decision 0135). Unlike
//! `systems`, this resolver reads a dataset; unlike `tropes`, it builds no
//! world.
use serde::Deserialize;
use std::collections::BTreeMap;
use std::path::Path;

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
    /// The source's own emergence taxonomy (Epstein & Axtell, Ch. II
    /// footnote 24): 1 = the individual property is meaningful but only the
    /// collective exhibits it; 2 = only the collective property is
    /// meaningful.
    ///
    /// `None` where the taxonomy does not apply, and that is a third
    /// answer rather than a missing one: a model abstraction (a lattice
    /// geometry, an experimental control) and a bare micro-rule assert no
    /// regularity, so they are neither type. Nullable at the schema rather
    /// than filtered downstream, so that no consumer can count filler into
    /// an emergence tally — a report that summed a defaulted `1` would
    /// publish a plausible number that is a category error.
    ///
    /// An item that DOES assert a regularity carries a 1 or a 2; the
    /// corpus's freeze tests hold that for every measurable item.
    #[serde(default)]
    pub emergence_type: Option<u8>,
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
    /// type-audit: bare-ok(prose: return)
    pub fn read(root: &Path) -> Result<GeneratedPaths, String> {
        let path = root.join("docs/generated-paths.txt");
        let text = std::fs::read_to_string(&path)
            .map_err(|e| format!("reading {}: {e}", path.display()))?;
        let authors: BTreeMap<String, bool> = crate::attest::parse_declared(&text)
            .into_iter()
            .map(|(p, author)| {
                (
                    p,
                    matches!(author, crate::attest::DeclaredAuthor::Roster(_)),
                )
            })
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

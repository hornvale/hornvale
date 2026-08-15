//! The Compendium: score a frozen corpus of game-system capability against
//! Hornvale's own declared state. Sibling family to `tropes/`, never a member
//! of it — see the spec's §3.
//!
//! Unlike `tropes`, this resolver builds **no world**. Every anchor resolves
//! against `docs/digest/decisions-in-force.md`, the idea registry, and the
//! filesystem, so the ratchet costs a few file reads rather than a genesis.

use serde::Deserialize;

/// The declared corpora, in matrix-column order.
/// type-audit: bare-ok(artifact)
pub const CORPORA: &[&str] = &["systems/wolverson-2021.system.json"];

/// How one catalogue item stands against Hornvale.
///
/// Five verdicts, not decision 0095's three. An unmet capability is three
/// different facts — deliberately refused, planned but unbuilt, or a genuine
/// hole — and an instrument that cannot tell them apart reports a deficiency
/// list that is mostly false against a corpus like this one.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum Verdict {
    /// Hornvale does this. Cites a mechanism anchor.
    Present,
    /// Hornvale deliberately will not. Cites a decision anchor.
    Refused,
    /// Planned, not built. Cites a registry anchor.
    Deferred,
    /// A genuine hole. Cites nothing — the honest red.
    Absent,
    /// About the tutorial's toolchain, not a world capability. Cites a reason.
    Inapplicable,
}

/// One catalogue item as authored in the corpus.
/// type-audit: bare-ok(identifier-text: id), bare-ok(identifier-text: kind), bare-ok(prose: title), bare-ok(identifier-text: anchor), bare-ok(prose: note)
#[derive(Debug, Clone, Deserialize)]
pub struct Item {
    /// Corpus-local identifier, e.g. `2.6`.
    pub id: String,
    /// What this row is: `chapter`, `front-matter`, `feature`, `mechanic`.
    pub kind: String,
    /// The item's title as the source gives it.
    pub title: String,
    /// How it stands against Hornvale.
    pub verdict: Verdict,
    /// The anchor backing the verdict, absent only for `absent`.
    #[serde(default)]
    pub anchor: Option<String>,
    /// One line of human context. Never parsed.
    #[serde(default)]
    pub note: String,
}

/// A frozen, provenance-stamped capability corpus.
/// type-audit: bare-ok(identifier-text: corpus), bare-ok(identifier-text: unit), bare-ok(flag: ordered), bare-ok(prose: provenance), bare-ok(prose: frozen)
#[derive(Debug, Clone, Deserialize)]
pub struct Corpus {
    /// Corpus identifier, e.g. `wolverson-2021`.
    pub corpus: String,
    /// What the items are: `chapter`, `feature`, …
    pub unit: String,
    /// Whether the items form a meaningful sequence. Gates ordinal readings.
    pub ordered: bool,
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

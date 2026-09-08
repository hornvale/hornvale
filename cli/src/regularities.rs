//! The regularity corpus resolver: does the world GROW this?
//!
//! Sibling to `tropes` (representability), `systems` (implementation) and
//! `sentences` (grammar), and distinguished from all three by its resolution
//! basis — measurement over the committed census (decision 0135). Unlike
//! `systems`, this resolver reads a dataset; unlike `tropes`, it builds no
//! world.
//!
//! **This is not the only reader of a `regularities/` corpus.**
//! `windows/lab/src/domesday/corpus.rs` parses the same files with a
//! minimal view — the fields a Domesday page prints — because a window may
//! not depend on `cli` and the corpus schema does not belong in the kernel.
//! That is a duplication kept on purpose (decision 0261), so both copies
//! name each other and both are held by
//! `cli/tests/suite/regularity_coverage.rs`'s
//! `the_two_readers_of_the_corpus_agree` and
//! `the_two_readers_agree_on_the_measured_number`. Change [`Criterion`],
//! [`Verdict`] or the fields [`Item`] deserializes, and that module needs
//! the same change.
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
///
/// `windows/lab/src/domesday/corpus.rs` carries a two-valued companion
/// (`Grown`/`Flat` only — the measured pair), for the reason its module doc
/// gives. A new MEASURED verdict added here has to be added there too or it
/// will be dropped from every survey page without a word; the five
/// non-measured values are dropped there deliberately.
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
///
/// **Duplicated on purpose in `windows/lab/src/domesday/corpus.rs`**
/// (decision 0261) — the Domesday renders a criterion in prose and cannot
/// reach this crate, since a window may not depend on `cli`. A kind added
/// here and not there makes the survey silently omit a claim, so the two
/// are held by `the_two_readers_of_the_corpus_agree` in
/// `cli/tests/suite/regularity_coverage.rs`, whose reductions of both enums
/// are exhaustive matches and therefore fail to COMPILE on a one-sided
/// addition. Add the kind in both places, in the same commit.
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
    ///
    /// Measured against the population, not the present slice: a world
    /// where the statistic is absent counts against the claim rather than
    /// being excluded from the vote. Dividing by the present count instead
    /// would let a statistic that is absent almost everywhere score highly
    /// on the handful of worlds where it happens to appear — exactly the
    /// shape a coverage instrument must not reward.
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
/// type-audit: bare-ok(identifier-text: id), bare-ok(prose: title), bare-ok(prose: source), bare-ok(count: emergence_type), bare-ok(identifier-text: statistic), bare-ok(identifier-text: anchor), bare-ok(prose: roadmap_instrument), bare-ok(prose: note)
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
    /// The instrument that would settle this item, where one is known.
    ///
    /// Meaningful for `absent` only, and it is what splits that verdict's two
    /// very different populations: an item nobody can measure because the
    /// mechanism is missing (no economy, no disease model) and an item nobody
    /// has measured because the column has not been written yet. `absent` is
    /// one value and cannot carry that distinction, so it is DECLARED here.
    ///
    /// **A structured field rather than a phrase in `note`, and the reason is
    /// worth stating.** The first draft read this off the note's prose, and
    /// two items with the same content in different words landed in different
    /// buckets — `sug-spatial-segregation` names three concrete instruments
    /// ("nearest-neighbour distance, a join-count, a Moran's I over an
    /// explicit weights matrix") and scored `gap`, while
    /// `sug-heterogeneous-landscape` scored `roadmap` for the same shape of
    /// sentence. Worse, the corpus is FROZEN, so the repair could not be a
    /// re-wording: fixing a classification by editing a note is exactly the
    /// data edit the freeze forbids, and it would have let any future
    /// re-wording move a committed artifact with nothing objecting.
    ///
    /// **What qualifies.** The value names the statistic or criterion that
    /// would DISCRIMINATE this regularity from its negation, at the item's
    /// own grain, over the population `Corpus::population` names. `None`
    /// where no such instrument is known — including two cases that look like
    /// candidates and are not: an instrument that exists but reads the WRONG
    /// GRAIN (a lineage graph between communities cannot settle a claim about
    /// individual genealogy), and a claim that would need a different
    /// POPULATION or a second generator rather than a new column (an
    /// attribution needs an ablation, and a census is a population of worlds
    /// under one physics).
    ///
    /// **It names the instrument; it does not size the work.** Three of the
    /// six declared today need a new census metric or a new criterion kind —
    /// a code change, a review and a test. Identification is not sizing, and
    /// the report says so rather than calling this half "small".
    #[serde(default)]
    pub roadmap_instrument: Option<String>,
    /// One line of human context.
    ///
    /// **Never parsed, and that is now true again.** Task 6's first draft
    /// classified the `absent` split by scanning this text, which made a
    /// committed artifact's headline a function of prose wording with no
    /// freeze test over it; `roadmap_instrument` above took that job. Nothing
    /// in this module reads this field except to reproduce it verbatim in the
    /// report's item table.
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

    /// The declaration covering `path`, if any: `Some(true)` when a roster
    /// set authors it, `Some(false)` when it is declared `none(<reason>)`,
    /// `None` when nothing in the file covers it at all.
    ///
    /// A file inherits the LONGEST declared directory prefix's author unless
    /// it overrides with a row of its own.
    fn declaration(&self, path: &str) -> Option<bool> {
        if let Some(generated) = self.authors.get(path) {
            return Some(*generated);
        }
        self.authors
            .iter()
            .filter(|(decl, _)| decl.ends_with('/') && path.starts_with(decl.as_str()))
            .max_by_key(|(decl, _)| decl.len())
            .map(|(_, generated)| *generated)
    }

    /// Whether this path has a real generator.
    ///
    /// **Direction this enforces:** declared-and-generated ⇒ admissible. It
    /// is blind to a path that is generated but undeclared, which is
    /// `cli/tests/suite/generated_paths.rs`'s job, not this one.
    /// type-audit: bare-ok(artifact: path), bare-ok(flag: return)
    pub fn has_generator(&self, path: &str) -> bool {
        self.declaration(path).unwrap_or(false)
    }

    /// Whether `docs/generated-paths.txt` says anything about this path at
    /// all — whichever way it says it.
    ///
    /// Exists so a rejection can name the RIGHT repair, because the two ways
    /// [`GeneratedPaths::has_generator`] returns `false` want opposite
    /// actions: a path declared `none(<reason>)` is deliberately
    /// hand-written and the verdict must move, whereas an undeclared path
    /// may simply be missing its row. `crate::attest::DeclaredAuthor::None`
    /// discards the `none(...)` reason text, so the reason itself cannot be
    /// quoted here — naming the declaring file and which of the two cases
    /// applies is the repair-bearing half of that message, and it needs no
    /// widening of that variant.
    /// type-audit: bare-ok(artifact: path), bare-ok(flag: return)
    pub fn is_declared(&self, path: &str) -> bool {
        self.declaration(path).is_some()
    }
}

/// Whether a frozen criterion is met by the present values of a statistic.
///
/// `present` is the statistic's values over worlds that have one; `worlds` is
/// the population size including worlds where the statistic is absent. The
/// two differ, and both fraction-based criteria (`FractionInBandAtLeast`,
/// `PresentOnFraction`) divide by `worlds`, never by `present.len()`: an
/// absent world counts against the claim rather than being excluded from
/// the vote, so a statistic absent almost everywhere cannot score highly on
/// the handful of worlds where it happens to appear. A world can also be
/// *present with value 0.0*, which is not the same as absent, so this
/// function never reads absence off the length of `present` alone.
/// type-audit: bare-ok(ratio: present), bare-ok(count: worlds), bare-ok(flag: return)
pub fn meets(criterion: &Criterion, present: &[f64], worlds: usize) -> bool {
    match criterion {
        Criterion::PresentOnFraction { min_fraction } => {
            if worlds == 0 {
                return false;
            }
            present.len() as f64 / worlds as f64 >= *min_fraction
        }
        Criterion::FractionInBandAtLeast {
            lo,
            hi,
            min_fraction,
        } => {
            if worlds == 0 {
                return false;
            }
            let inside = present.iter().filter(|v| **v >= *lo && **v <= *hi).count();
            inside as f64 / worlds as f64 >= *min_fraction
        }
        Criterion::MedianInBand { .. }
        | Criterion::MedianAtLeast { .. }
        | Criterion::MedianAtMost { .. } => {
            let Some(median) = median(present) else {
                return false;
            };
            match criterion {
                Criterion::MedianInBand { lo, hi } => median >= *lo && median <= *hi,
                Criterion::MedianAtLeast { bound } => median >= *bound,
                Criterion::MedianAtMost { bound } => median <= *bound,
                _ => unreachable!("matched above"),
            }
        }
    }
}

/// The median of the present values, or `None` when there are none.
///
/// Extracted from [`meets`] rather than duplicated beside it, because
/// `measure` mode must print the very number the verdict was decided on: two
/// medians computed by two functions could agree for a hundred runs and
/// disagree on the run that matters. Sorted with `total_cmp`, the workspace's
/// only sanctioned float ordering.
/// type-audit: bare-ok(ratio: present), bare-ok(ratio: return)
pub fn median(present: &[f64]) -> Option<f64> {
    if present.is_empty() {
        return None;
    }
    let mut sorted = present.to_vec();
    sorted.sort_by(f64::total_cmp);
    let mid = sorted.len() / 2;
    Some(if sorted.len().is_multiple_of(2) {
        (sorted[mid - 1] + sorted[mid]) / 2.0
    } else {
        sorted[mid]
    })
}

/// The present numeric values of a statistic, in census row order.
///
/// Absent, empty and unparseable readings are skipped — the same treatment
/// `windows/lab/src/domesday/stats.rs` gives them.
/// type-audit: bare-ok(identifier-text: statistic), bare-ok(ratio: return)
pub fn values_of(census: &hornvale_lab::domesday::census::Census, statistic: &str) -> Vec<f64> {
    census
        .values(statistic)
        .into_iter()
        .filter_map(|reading| reading.parse::<f64>().ok())
        .collect()
}

/// What the committed census says about one item's frozen criterion, computed
/// fresh and asserting nothing.
///
/// This is the *read* half of the two-way guard in [`measure`]: the same
/// computation, with the comparison against the authored verdict removed and
/// the numbers behind it kept. `measure` mode prints these; nothing gates on
/// them.
/// type-audit: bare-ok(count: present), bare-ok(count: worlds), bare-ok(prose: summary)
#[derive(Debug, Clone)]
pub struct Measurement {
    /// The verdict the criterion yields against the present census.
    pub verdict: Verdict,
    /// How many worlds reported a numeric value for the statistic.
    pub present: usize,
    /// The population size, including worlds where the statistic is absent.
    pub worlds: usize,
    /// The numbers the verdict was decided on, in one line.
    pub summary: String,
}

/// Score one item's criterion against the census, without comparing it to the
/// authored verdict. `Err` names why the item cannot be scored at all.
///
/// Deliberately separate from [`audit`]: an audit is a gate and returns
/// findings, while this returns a reading. Sharing [`meets`] and [`median`]
/// with the gate is what makes the reading trustworthy — a second
/// implementation could quietly print a number the gate never used.
/// type-audit: bare-ok(prose: return)
pub fn compute(
    item: &Item,
    census: &hornvale_lab::domesday::census::Census,
) -> Result<Measurement, String> {
    let Some(criterion) = item.criterion.as_ref() else {
        return Err(format!(
            "{} carries no criterion, so there is nothing to score against the census",
            item.id
        ));
    };
    // Same reason `measure` guards this: `Census::values` PANICS on a name
    // that is not a column, and a retired metric is an ordinary way for this
    // corpus to decay.
    if !census.has(&item.statistic) {
        return Err(format!(
            "{} is measured through census statistic `{}`, which is not a column of the \
             committed census at all",
            item.id, item.statistic
        ));
    }
    let present = values_of(census, &item.statistic);
    let worlds = census.rows.len();
    let verdict = if meets(criterion, &present, worlds) {
        Verdict::Grown
    } else {
        Verdict::Flat
    };
    let summary = match criterion {
        Criterion::MedianInBand { lo, hi } => format!(
            "median({}) = {}; band [{lo}, {hi}]",
            item.statistic,
            median_text(&present)
        ),
        Criterion::MedianAtLeast { bound } => format!(
            "median({}) = {}; required >= {bound}",
            item.statistic,
            median_text(&present)
        ),
        Criterion::MedianAtMost { bound } => format!(
            "median({}) = {}; required <= {bound}",
            item.statistic,
            median_text(&present)
        ),
        Criterion::FractionInBandAtLeast {
            lo,
            hi,
            min_fraction,
        } => {
            let inside = present.iter().filter(|v| **v >= *lo && **v <= *hi).count();
            format!(
                "{inside} of {worlds} world(s) have {} in [{lo}, {hi}] = {}; required >= \
                 {min_fraction}",
                item.statistic,
                fraction_text(inside, worlds)
            )
        }
        Criterion::PresentOnFraction { min_fraction } => format!(
            "{} present on {} of {worlds} world(s) = {}; required >= {min_fraction}",
            item.statistic,
            present.len(),
            fraction_text(present.len(), worlds)
        ),
    };
    Ok(Measurement {
        verdict,
        present: present.len(),
        worlds,
        summary,
    })
}

/// The median rendered for a human, or `absent` when nothing was present.
///
/// Six decimals, not the census's own emitted precision: this text is read by
/// a person deciding whether a verdict is believable, and is never parsed or
/// committed, so it owes the quantize-at-emit contract nothing.
fn median_text(present: &[f64]) -> String {
    match median(present) {
        Some(m) => format!("{m:.6}"),
        None => "absent (no world reported a value)".to_string(),
    }
}

/// A share rendered for a human. Zero worlds prints as `n/a` rather than a
/// division nobody can read.
fn fraction_text(part: usize, whole: usize) -> String {
    if whole == 0 {
        return "n/a (empty population)".to_string();
    }
    format!("{:.6}", part as f64 / whole as f64)
}

/// Something wrong with an item, found by re-checking it against live state.
///
/// Four variants, and the third is why this family exists as more than a
/// report. The sibling corpora (`tropes/`, `systems/`, `sentences/`) all
/// ratchet — a built capability stays built — so they need only notice
/// evidence that stopped resolving. A *grown* regularity is emergent, and any
/// retune of the history bake can destroy it while every other gate stays
/// green, so this family also compares the authored verdict against the
/// measured one, in BOTH directions.
/// type-audit: bare-ok(identifier-text: Unjustified.id), bare-ok(prose: Unjustified.why), bare-ok(identifier-text: Dangling.id), bare-ok(identifier-text: Dangling.anchor), bare-ok(prose: Dangling.why), bare-ok(identifier-text: Regressed.id), bare-ok(prose: Regressed.why), bare-ok(identifier-text: StaleDeferred.id), bare-ok(identifier-text: StaleDeferred.row), bare-ok(prose: StaleDeferred.why)
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Finding {
    /// A verdict with no anchor, the wrong kind of anchor, or an anchor into
    /// hand-written prose.
    Unjustified {
        /// The item's corpus-local id.
        id: String,
        /// What is wrong, in a sentence.
        why: String,
    },
    /// The anchor — or the census column the verdict is measured through —
    /// stopped resolving.
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
    /// A `deferred` verdict whose registry row now reads a status that
    /// falsifies "planned, not built" (see
    /// [`crate::systems::DEFERRAL_FALSIFYING_STATUSES`]).
    ///
    /// **The consequence is heavier here than in the sibling `systems`
    /// family, which is why this family carries the check despite the cost
    /// of a fourth variant.** There, a stale deferral misreports a
    /// capability. Here it WITHHOLDS AN ITEM FROM MEASUREMENT: the corpus
    /// goes on asserting it cannot measure something it now can, so the item
    /// never re-enters the measurement queue and its verdict is frozen
    /// wrong. A family whose whole premise is that verdicts rot cannot ship
    /// without the rot-detector its sibling already has.
    ///
    /// The exposure is concentrated, not diffuse: every `deferred` item in
    /// the founding corpus cites the SAME registry row, so one row flipping
    /// to `shipped` mis-verdicts a fifth of the corpus in a single move.
    StaleDeferred {
        /// The item's corpus-local id.
        id: String,
        /// The registry row whose question has been settled.
        row: String,
        /// What to do about it.
        why: String,
    },
}

/// Human name of a verdict, for failure text and for `measure`'s output.
///
/// Public so a caller printing a computed verdict spells it the same way the
/// corpus and the audit's failure text do — one spelling, one place.
/// type-audit: bare-ok(identifier-text: return)
pub fn verdict_name(v: Verdict) -> &'static str {
    match v {
        Verdict::Grown => "grown",
        Verdict::Flat => "flat",
        Verdict::Refused => "refused",
        Verdict::Deferred => "deferred",
        Verdict::Absent => "absent",
        Verdict::Inapplicable => "inapplicable",
        Verdict::Unmeasured => "unmeasured",
    }
}

/// The anchor kind a verdict requires, for failure text.
fn expected_anchor_kind(v: Verdict) -> &'static str {
    match v {
        Verdict::Grown | Verdict::Flat => "a `doc:` anchor into generated prose",
        Verdict::Refused => "a `decision:` anchor",
        Verdict::Deferred => "a `registry:` anchor",
        Verdict::Inapplicable => "a `reason:` anchor",
        Verdict::Absent | Verdict::Unmeasured => "no anchor",
    }
}

/// The anchor's own kind, for failure text.
fn anchor_kind_name(a: &Anchor) -> &'static str {
    match a {
        Anchor::Doc(_) => "doc:",
        Anchor::Decision(_) => "decision:",
        Anchor::Registry(_) => "registry:",
        Anchor::Reason(_) => "reason:",
    }
}

/// Whether `a`'s kind is one the verdict `v` permits.
fn anchor_matches_verdict(v: Verdict, a: &Anchor) -> bool {
    matches!(
        (v, a),
        (Verdict::Grown, Anchor::Doc(_))
            | (Verdict::Flat, Anchor::Doc(_))
            | (Verdict::Refused, Anchor::Decision(_))
            | (Verdict::Deferred, Anchor::Registry(_))
            | (Verdict::Inapplicable, Anchor::Reason(_))
    )
}

/// Audit every item against live state: the declared generated paths, the
/// live repo facts, and the committed census. One finding per problem; a
/// clean corpus returns an empty vector.
///
/// `unmeasured` items raise nothing at all — they are frozen but not yet
/// measured, a lifecycle state rather than a coverage verdict, and the
/// report lists them separately.
pub fn audit(
    corpus: &Corpus,
    census: &hornvale_lab::domesday::census::Census,
    generated: &GeneratedPaths,
    facts: &crate::systems::RepoFacts,
) -> Vec<Finding> {
    corpus
        .items
        .iter()
        .filter_map(|item| audit_item(item, census, generated, facts))
        .collect()
}

/// Audit a single item. `None` means clean.
fn audit_item(
    item: &Item,
    census: &hornvale_lab::domesday::census::Census,
    generated: &GeneratedPaths,
    facts: &crate::systems::RepoFacts,
) -> Option<Finding> {
    if item.verdict == Verdict::Unmeasured {
        return None;
    }

    if item.verdict == Verdict::Absent {
        return item.anchor.as_ref().map(|anchor| Finding::Unjustified {
            id: item.id.clone(),
            why: format!(
                "{} has verdict `absent` but carries anchor `{anchor}`. An `absent` \
                 verdict claims nothing and must carry no anchor — remove the anchor, \
                 or change the verdict to the one the anchor actually supports.",
                item.id
            ),
        });
    }

    let Some(anchor_str) = &item.anchor else {
        return Some(Finding::Unjustified {
            id: item.id.clone(),
            why: format!(
                "{} has verdict `{}` but no anchor. Every verdict except `absent` and \
                 `unmeasured` must cite evidence: {} is required. Add an anchor, or \
                 change the verdict to `absent` if there is truly nothing to cite.",
                item.id,
                verdict_name(item.verdict),
                expected_anchor_kind(item.verdict)
            ),
        });
    };

    let Some(anchor) = Anchor::parse(anchor_str) else {
        return Some(Finding::Unjustified {
            id: item.id.clone(),
            why: format!(
                "{} cites `{anchor_str}`, which has an unrecognized anchor prefix. \
                 Expected one of doc:, decision:, registry:, reason:. (`test:` and \
                 `path:` are the sibling `systems` family's kinds and are deliberately \
                 not admitted here: a regularity is not demonstrated by code existing.)",
                item.id
            ),
        });
    };

    if !anchor_matches_verdict(item.verdict, &anchor) {
        return Some(Finding::Unjustified {
            id: item.id.clone(),
            why: format!(
                "{} has verdict `{}`, which requires {}, but its anchor `{anchor_str}` \
                 is a {} anchor.",
                item.id,
                verdict_name(item.verdict),
                expected_anchor_kind(item.verdict),
                anchor_kind_name(&anchor)
            ),
        });
    }

    if let Some(finding) = resolve_anchor(item, anchor_str, &anchor, generated, facts) {
        return Some(finding);
    }

    measure(item, census)
}

/// Verify `anchor` still resolves. `None` means clean.
fn resolve_anchor(
    item: &Item,
    anchor_str: &str,
    anchor: &Anchor,
    generated: &GeneratedPaths,
    facts: &crate::systems::RepoFacts,
) -> Option<Finding> {
    match anchor {
        Anchor::Doc(path) => {
            if generated.has_generator(path) {
                return None;
            }
            // The two ways `has_generator` says no want opposite repairs, so
            // the message names which one applies. It cannot quote the
            // `none(<reason>)` prose — `attest::DeclaredAuthor::None`
            // discards it — and does not need to: the reason is one line of
            // the file this message names.
            let detail = if generated.is_declared(path) {
                "which docs/generated-paths.txt declares `none(<reason>)` — hand-written \
                 prose that no roster set regenerates. Anchoring a measured verdict there \
                 would be decision 0330's failure: a declaration that moves the score \
                 without moving the world. Re-anchor to the generated page that states \
                 the measured claim, or re-verdict this item."
            } else {
                "which docs/generated-paths.txt does not declare at all, so nothing \
                 asserts it is regenerated or drift-checked. Either the page moved (fix \
                 the anchor), or it is generated but undeclared (add its row to \
                 docs/generated-paths.txt, which `cli/tests/suite/generated_paths.rs` \
                 then holds tracked), or it is hand-written and cannot back a verdict."
            };
            Some(Finding::Unjustified {
                id: item.id.clone(),
                why: format!("{} cites doc:{path}, {detail}", item.id),
            })
        }
        Anchor::Decision(d) => {
            if facts.decision_in_force(d) {
                None
            } else {
                Some(Finding::Dangling {
                    id: item.id.clone(),
                    anchor: anchor_str.to_string(),
                    why: format!(
                        "{} cites decision:{d}, which is not in \
                         docs/digest/decisions-in-force.md. A decision leaves that file \
                         when it is wholly superseded. Either re-verdict this item \
                         against the superseding decision, or restore the anchor if the \
                         supersession was partial.",
                        item.id
                    ),
                })
            }
        }
        Anchor::Registry(r) => match facts.registry_status(r) {
            // Status is NORMALIZED by `RepoFacts` at gather time, so
            // `**shipped (C1)**` matches while `elaborated (slice-2 shipped)`
            // correctly does not.
            Some(status) if crate::systems::DEFERRAL_FALSIFYING_STATUSES.contains(&status) => {
                Some(Finding::StaleDeferred {
                    id: item.id.clone(),
                    row: r.clone(),
                    why: format!(
                        "{} defers to registry:{r}, which now reads `{status}` in \
                         book/src/frontier/idea-registry.md. A `deferred` verdict here \
                         claims the statistic CANNOT YET BE COMPUTED; that claim stopped \
                         being true once this row's question was settled. The repair is \
                         not a hand-written re-verdict: move this item back to \
                         `unmeasured` (dropping its anchor) so the next measurement run \
                         computes it and assigns `grown` or `flat` from the census. \
                         Re-verdict to `refused` or `absent` only if the settled row \
                         means the statistic will never be computed after all.",
                        item.id
                    ),
                })
            }
            Some(_) => None,
            None => Some(Finding::Dangling {
                id: item.id.clone(),
                anchor: anchor_str.to_string(),
                why: format!(
                    "{} cites registry:{r}, which does not appear in \
                         book/src/frontier/idea-registry.md. Either the row ID changed \
                         (fix the anchor) or the row was removed (re-verdict this item \
                         against whatever replaced it).",
                    item.id
                ),
            }),
        },
        Anchor::Reason(reason) => {
            if reason.trim().is_empty() {
                Some(Finding::Unjustified {
                    id: item.id.clone(),
                    why: format!(
                        "{} has verdict `inapplicable` but its `reason:` anchor carries \
                         no prose. A reasonless anchor is the same failure a reasonless \
                         `waiver(...)` is for type-audit: state why this item does not \
                         apply.",
                        item.id
                    ),
                })
            } else {
                None
            }
        }
    }
}

/// Recompute a measured item's verdict from the census and compare it to the
/// authored one. `None` means they agree, or the item is not a measured one.
///
/// **The comparison is two-way, and that is the point.** The sibling corpus
/// families ratchet, so a one-directional check suffices for them: a built
/// capability stays built, and only its disappearance is news. A regularity
/// is GROWN rather than built — nothing in the codebase names it, and a
/// retune of the history bake can extinguish it with every other gate still
/// green. So both disagreements are red:
///
/// - authored `grown`, computed `flat` — a regularity was LOST.
/// - authored `flat`, computed `grown` — STALE PESSIMISM. A real gain must be
///   claimed deliberately, in a commit that says so, rather than sitting
///   unreported in a corpus that under-reports the world.
///
/// Reddening only the first direction would be half a guard, and the skipped
/// half is the one that lets the corpus quietly understate what the world
/// does.
fn measure(item: &Item, census: &hornvale_lab::domesday::census::Census) -> Option<Finding> {
    if !matches!(item.verdict, Verdict::Grown | Verdict::Flat) {
        return None;
    }
    let criterion = item.criterion.as_ref()?;

    // `Census::values` PANICS on a name that is not a column, which is the
    // right behaviour for its own callers (a typo is a programming error)
    // and the wrong one here: a census refresh that drops a metric is an
    // ordinary, expected way for this corpus to decay, and an audit that
    // aborts reports nothing about the remaining items.
    if !census.has(&item.statistic) {
        return Some(Finding::Dangling {
            id: item.id.clone(),
            anchor: item.anchor.clone().unwrap_or_default(),
            why: format!(
                "{} is measured through census statistic `{}`, which is not a column of \
                 the committed census at all. Either the metric was renamed (fix the \
                 `statistic` field) or it was retired from the study (re-verdict this \
                 item to `deferred` or `absent`). Note this is NOT the same as a metric \
                 every world declined to report, which is a legitimate measurement.",
                item.id, item.statistic
            ),
        });
    }

    let present = values_of(census, &item.statistic);
    let worlds = census.rows.len();
    let computed = if meets(criterion, &present, worlds) {
        Verdict::Grown
    } else {
        Verdict::Flat
    };
    if computed == item.verdict {
        return None;
    }

    let repair = if item.verdict == Verdict::Grown {
        "A regularity this corpus claims the world GROWS no longer measures grown. That \
         is a finding about the world, not about the corpus: something upstream — most \
         likely a retune of the history bake — stopped producing it, and no other gate \
         would have said so. Investigate the cause before re-verdicting; if the loss is \
         accepted, change the verdict to `flat` in a commit that says why."
    } else {
        "A regularity this corpus records as FLAT now measures grown. Stale pessimism is \
         red for the same reason a loss is: a corpus that under-reports the world is as \
         wrong as one that over-reports it. Promote the verdict to `grown` deliberately, \
         in a commit that claims the gain and names what produced it."
    };
    Some(Finding::Regressed {
        id: item.id.clone(),
        authored: item.verdict,
        computed,
        why: format!(
            "{}: authored `{}`, computed `{}` over {} present value(s) of `{}` across {} \
             world(s). {repair}",
            item.id,
            verdict_name(item.verdict),
            verdict_name(computed),
            present.len(),
            item.statistic,
            worlds
        ),
    })
}

// --- The report -------------------------------------------------------------

/// Wrap prose at 76 columns, preserving word order.
///
/// Duplicated from [`crate::systems::wrap`]'s shape rather than shared: these
/// two renderers author different byte-frozen artifacts, and a shared helper
/// would let a whitespace change in one family silently re-baseline the
/// other's committed report.
fn wrap(text: &str) -> String {
    let mut out = String::new();
    let mut col = 0;
    for word in text.split_whitespace() {
        let w = word.chars().count();
        if col > 0 && col + 1 + w > 76 && !word.starts_with('-') {
            out.push('\n');
            col = 0;
        } else if col > 0 {
            out.push(' ');
            col += 1;
        }
        out.push_str(word);
        col += w;
    }
    out
}

/// `n` of `total` as a whole percent, rounded half up. Integer arithmetic on
/// purpose — this figure lands in a byte-ratcheted artifact, and decision 0033
/// keeps floats away from serialization boundaries.
fn percent(n: usize, total: usize) -> usize {
    if total == 0 {
        0
    } else {
        (n * 200 + total) / (total * 2)
    }
}

/// Where a corpus's committed report lives, derived from the corpus's own
/// identifier so a caller cannot pair the wrong corpus with the wrong
/// artifact.
/// type-audit: bare-ok(identifier-text: return)
pub fn artifact_path(corpus: &Corpus) -> String {
    format!("docs/audits/regularity-coverage-{}.md", corpus.corpus)
}

/// The command that regenerates a report, for the header.
///
/// Takes the **corpus source path** the caller actually resolved, exactly as
/// [`crate::systems::regenerate_command`] does and for the same reason: a
/// derived stem could print a command naming a file that does not exist. It is
/// never the artifact path — passing that would print a command asking the
/// resolver to parse its own output as a corpus.
/// type-audit: bare-ok(identifier-text: path), bare-ok(identifier-text: return)
pub fn regenerate_command(path: &str) -> String {
    format!("hornvale regularities --corpus {path} report")
}

/// Verdict counts across `corpus`, in this family's own verdict order — six
/// coverage verdicts, then `unmeasured`, which is a lifecycle state and is
/// tallied apart from them.
fn tally(corpus: &Corpus) -> [(Verdict, usize); 7] {
    let count = |v: Verdict| corpus.items.iter().filter(|i| i.verdict == v).count();
    [
        (Verdict::Grown, count(Verdict::Grown)),
        (Verdict::Flat, count(Verdict::Flat)),
        (Verdict::Refused, count(Verdict::Refused)),
        (Verdict::Deferred, count(Verdict::Deferred)),
        (Verdict::Absent, count(Verdict::Absent)),
        (Verdict::Inapplicable, count(Verdict::Inapplicable)),
        (Verdict::Unmeasured, count(Verdict::Unmeasured)),
    ]
}

/// The items this corpus proposes to measure: every item carrying both a
/// statistic and a criterion, whatever its verdict.
///
/// Deliberately not "every `grown` item": an `unmeasured` item is measurable
/// and a measured one does not stop being measurable when its criterion fails,
/// so the denominator this report divides by does not move when Task 7's
/// verdicts land.
fn measurable(corpus: &Corpus) -> Vec<&Item> {
    corpus
        .items
        .iter()
        .filter(|i| i.criterion.is_some() && !i.statistic.is_empty())
        .collect()
}

/// The threshold above which two statistics are treated as ONE claim.
///
/// A declared convention, not a tuned value: 0.95 is the ordinary
/// near-collinearity bar. The report prints every measured pair beside it, so
/// a reader can see how far the partition sits from the threshold rather than
/// taking the merge on trust.
const COLLINEARITY_THRESHOLD: f64 = 0.95;

/// Pearson correlation of two census columns over the worlds where BOTH are
/// present, with the pairing done row by row.
///
/// Returns the coefficient and the paired count. `None` when fewer than two
/// worlds pair, or when either column has no variance — in both cases a
/// correlation is undefined rather than zero, and reporting zero would read as
/// "measured, independent" for a pair that was never measured at all.
///
/// [`values_of`] cannot be used here: it drops absent rows independently per
/// column, so two columns with different absence patterns would be paired by
/// position against worlds they do not share.
fn correlation(
    census: &hornvale_lab::domesday::census::Census,
    a: &str,
    b: &str,
) -> Option<(f64, usize)> {
    let mut xs: Vec<f64> = Vec::new();
    let mut ys: Vec<f64> = Vec::new();
    for row in &census.rows {
        let (Some(va), Some(vb)) = (row.get(a), row.get(b)) else {
            continue;
        };
        let (Ok(x), Ok(y)) = (va.parse::<f64>(), vb.parse::<f64>()) else {
            continue;
        };
        xs.push(x);
        ys.push(y);
    }
    let n = xs.len();
    if n < 2 {
        return None;
    }
    let nf = n as f64;
    let mx = xs.iter().sum::<f64>() / nf;
    let my = ys.iter().sum::<f64>() / nf;
    let mut sxy = 0.0;
    let mut sxx = 0.0;
    let mut syy = 0.0;
    for (x, y) in xs.iter().zip(ys.iter()) {
        sxy += (x - mx) * (y - my);
        sxx += (x - mx) * (x - mx);
        syy += (y - my) * (y - my);
    }
    if sxx <= 0.0 || syy <= 0.0 {
        return None;
    }
    Some((sxy / (sxx * syy).sqrt(), n))
}

/// One measured pair of measurable statistics.
struct Pair<'a> {
    /// The first item's id.
    left: &'a str,
    /// The second item's id.
    right: &'a str,
    /// The correlation, when it is defined.
    r: Option<(f64, usize)>,
}

/// Every unordered pair of measurable items, with its measured correlation.
fn pairs<'a>(items: &[&'a Item], census: &hornvale_lab::domesday::census::Census) -> Vec<Pair<'a>> {
    let mut out = Vec::new();
    for (i, a) in items.iter().enumerate() {
        for b in items.iter().skip(i + 1) {
            let r = if census.has(&a.statistic) && census.has(&b.statistic) {
                correlation(census, &a.statistic, &b.statistic)
            } else {
                None
            };
            out.push(Pair {
                left: &a.id,
                right: &b.id,
                r,
            });
        }
    }
    out
}

/// The claim grouping: for each item, the index of the claim it belongs to.
///
/// Two items that read near-collinear statistics off the same population are
/// one claim measured twice, not two corroborations. Union by repeated
/// relaxation over the measured pairs; the item count is tiny and this needs
/// no disjoint-set structure.
///
/// Separated from [`independent_claims`] so the claim-level verdict reading
/// and the claim COUNT are derived from one merge rather than two. A second
/// grouping computed beside this one could disagree with the correlation
/// table the report prints, which is the table a reader would use to check
/// either number.
fn claim_grouping(items: &[&Item], pairs: &[Pair<'_>]) -> Vec<usize> {
    let mut group: Vec<usize> = (0..items.len()).collect();
    let index = |id: &str| items.iter().position(|i| i.id == id);
    let mut changed = true;
    while changed {
        changed = false;
        for p in pairs {
            let Some((r, _)) = p.r else { continue };
            if r.abs() < COLLINEARITY_THRESHOLD {
                continue;
            }
            let (Some(a), Some(b)) = (index(p.left), index(p.right)) else {
                continue;
            };
            let lo = group[a].min(group[b]);
            if group[a] != lo || group[b] != lo {
                group[a] = lo;
                group[b] = lo;
                changed = true;
            }
        }
    }
    group
}

/// How many INDEPENDENT claims the measurable items make.
fn independent_claims(items: &[&Item], pairs: &[Pair<'_>]) -> usize {
    let mut seen = claim_grouping(items, pairs);
    seen.sort_unstable();
    seen.dedup();
    seen.len()
}

/// The claim-level reading: how many independent claims GREW, out of how many
/// have been measured at all.
///
/// A claim counts as grown only when EVERY item merged into it measures
/// `grown`. A merged claim is one thing measured twice, so two readings that
/// disagree do not make it grown — they make it a claim whose instrument
/// disagrees with itself, which must not round up.
///
/// Why the report needs this beside the tally: `grown: 3` and `3 independent
/// claim(s)` are the same numeral meaning two different things, and read
/// together they suggest three claims grew. Two did. The near-collinear pair
/// (r >= the threshold above) is one claim counted twice in the item tally,
/// and nothing in a per-item count can say so.
fn grown_claims(items: &[&Item], pairs: &[Pair<'_>]) -> (usize, usize) {
    let group = claim_grouping(items, pairs);
    let mut ids: Vec<usize> = group.clone();
    ids.sort_unstable();
    ids.dedup();
    let mut grown = 0;
    let mut measured = 0;
    for id in ids {
        let members: Vec<&&Item> = items
            .iter()
            .enumerate()
            .filter(|(i, _)| group[*i] == id)
            .map(|(_, item)| item)
            .collect();
        // A claim is measured only when every item in it is. An unmeasured
        // member makes the claim's verdict unknown, not flat.
        if !members
            .iter()
            .all(|i| matches!(i.verdict, Verdict::Grown | Verdict::Flat))
        {
            continue;
        }
        measured += 1;
        if members.iter().all(|i| i.verdict == Verdict::Grown) {
            grown += 1;
        }
    }
    (grown, measured)
}

/// Whether a criterion constrains the statistic from both sides.
///
/// The power reading turns on this: a one-sided floor is cleared by every
/// world above it, including worlds far past anything the source describes, so
/// a `grown` on a one-sided criterion is a weaker statement than a `grown` on
/// a band.
fn is_two_sided(c: &Criterion) -> bool {
    matches!(
        c,
        Criterion::MedianInBand { .. } | Criterion::FractionInBandAtLeast { .. }
    )
}

/// Whether this item declares the instrument that would settle it.
///
/// Reads the structured [`Item::roadmap_instrument`] field, never the note.
/// The first draft scanned note prose and mis-sorted two items whose notes say
/// the same thing in different words; see that field's doc for why a re-wording
/// could not be the repair.
fn names_an_instrument(item: &Item) -> bool {
    item.roadmap_instrument.is_some()
}

/// A correlation formatted for a committed artifact.
///
/// Quantized before formatting, per decision 0033's quantize-at-emit rule:
/// the coefficient is computed at full precision and rounded once, here, at
/// the only boundary where it becomes bytes.
fn r_text(r: Option<(f64, usize)>) -> String {
    match r {
        Some((r, n)) => format!("{:.3} (n = {n})", hornvale_kernel::quantize(r)),
        None => "undefined".to_string(),
    }
}

/// Escape a free-text corpus field for a markdown table.
fn table_text(s: &str) -> String {
    s.replace('|', "\\|")
}

/// Render the coverage report.
///
/// Order follows decision 0095 — provenance and the instrument's own bias
/// before any number — with this family's two additions ahead of the tally:
/// the independent-claim count, because four measurable items are not four
/// claims, and the power reading, because every surviving criterion is a
/// conservative floor and a reader must pass that sentence before reaching a
/// score.
///
/// `path` is the corpus source path the caller actually resolved, so the
/// regenerate command in the banner names a file that really exists.
/// type-audit: bare-ok(identifier-text: path), bare-ok(prose: return)
pub fn render(
    corpus: &Corpus,
    census: &hornvale_lab::domesday::census::Census,
    path: &str,
) -> String {
    let mut s = String::new();
    s.push_str(&format!(
        "<!-- GENERATED FILE — do not edit. Regenerate with `{}`. -->\n\n",
        regenerate_command(path)
    ));
    s.push_str("# Regularity coverage\n\n## Provenance\n\n");
    s.push_str(&format!("- **Corpus:** `{}`\n", corpus.corpus));
    s.push_str(&format!(
        "- **Population:** `{}`, {} world(s)\n",
        corpus.population,
        census.rows.len()
    ));
    s.push_str(&format!("- **Source:** {}\n", wrap(&corpus.provenance)));
    s.push_str(&format!("- **Frozen:** {}\n", wrap(&corpus.frozen)));

    s.push_str("\n## Reading this report\n\n");
    s.push_str(&wrap(
        "This measures reach against ONE catalogue. It is a reading taken through that \
         catalogue's declared bias (decision 0095), never a grade and never a verdict on \
         the world. Sugarscape is a 1996 lattice model whose roster is roughly half \
         economic, so a large block of this corpus can only ever score `absent` — that is \
         a property of the source's coverage, not a charge against Hornvale. Conversely \
         the source has no terrain, no astronomy, no language and no deep time, so nothing \
         here scores Hornvale's strongest ground.",
    ));
    s.push_str("\n\n");
    s.push_str(&wrap(
        "Every mapping from a source claim to a census column is an ANALOGY: a Sugarscape \
         agent is an individual and a Hornvale settlement is a community. Each item's note \
         states where that analogy is load-bearing, and a `flat` verdict on such an item \
         may be about the analogy rather than about the world.",
    ));
    s.push_str("\n\n");

    let items = measurable(corpus);
    let pairs = pairs(&items, census);
    let claims = independent_claims(&items, &pairs);
    let two_sided = items
        .iter()
        .filter(|i| i.criterion.as_ref().is_some_and(is_two_sided))
        .count();

    // The power reading sits ABOVE the tally deliberately. A tally read first
    // and qualified afterwards is a score; the qualification has to be on the
    // way in.
    s.push_str("## Power\n\n");
    s.push_str(&wrap(&format!(
        "The corpus proposes {} measurable item(s), and they make {} independent claim(s) \
         — see the pairs below. Of those items, {} carry a two-sided criterion and {} \
         carry a one-sided one.",
        items.len(),
        claims,
        two_sided,
        items.len() - two_sided
    )));
    s.push_str("\n\n");
    // The count, not a quantifier. A hard-coded "mostly" contradicted the
    // line printed directly above it the moment the mix was 2 and 2, and
    // nothing would have moved it when the mix changed again.
    s.push_str(&wrap(&format!(
        "{} OF THE {} SURVIVING CRITERIA ARE CONSERVATIVE ONE-SIDED BOUNDS, and this \
         report must not be read as though any of them were a calibrated target. A \
         one-sided criterion says nothing on its unconstrained side: a floor is met by \
         every world above it and a ceiling by every world below it, however far past \
         anything the source describes. It separates a world that does the thing at all \
         from one that does not, and it says nothing about magnitude. So a near-uniform \
         `grown` sweep across these items is NOT evidence of reach. It is evidence that \
         the world clears a small number of low bars that a plausibly-flat world would \
         fail — which is the most this instrument was built to claim, and less than a \
         reader scanning a tally will assume.",
        items.len() - two_sided,
        items.len()
    )));
    s.push_str("\n\n");
    s.push_str(&wrap(
        "The bands were authored ahead of measurement, except where the Frozen \
         declaration above discloses otherwise, by name — this report does not restate \
         the blanket claim, because the corpus itself does not make one, and does not \
         count the exceptions, because the corpus is where they are declared. A two-sided \
         band is the only shape here that can fail at BOTH poles, and it is \
         correspondingly the shape with real discriminating power; the count above says \
         how many of these items carry one. Read the individual notes for which bound is \
         at risk on which item: several say so about themselves, in both directions.",
    ));
    s.push_str("\n\n");
    s.push_str(&wrap(&format!(
        "The claim count is the item count with near-collinear statistics merged: two \
         items reading statistics correlated at |r| >= {COLLINEARITY_THRESHOLD:.2} over \
         the same population are one claim measured twice, not two corroborations. The \
         coefficients below are measured on THIS census, not quoted from any earlier \
         probe."
    )));
    s.push_str("\n\n");

    if pairs.is_empty() {
        s.push_str("No pair of measurable items to compare.\n\n");
    } else {
        s.push_str("| item | item | correlation | merged |\n|---|---|---|---|\n");
        for p in &pairs {
            let merged = match p.r {
                Some((r, _)) if r.abs() >= COLLINEARITY_THRESHOLD => "yes",
                _ => "no",
            };
            s.push_str(&format!(
                "| {} | {} | {} | {merged} |\n",
                p.left,
                p.right,
                r_text(p.r)
            ));
        }
        s.push('\n');
    }

    let counts = tally(corpus);
    let total = corpus.items.len();
    s.push_str("## Tally\n\n");
    s.push_str(&wrap(
        "Six coverage verdicts, then `unmeasured` — which is a lifecycle state, not a \
         coverage verdict: an item frozen but not yet run. It is listed here so the totals \
         add up, and again by name below.",
    ));
    s.push_str("\n\n");
    for (v, n) in counts {
        s.push_str(&format!(
            "- {}: {n} ({}%)\n",
            verdict_name(v),
            percent(n, total)
        ));
    }
    s.push_str(&format!("- **total:** {total}\n"));

    // THE CLAIM-LEVEL READING, derived from the same merge the Power section
    // prints — never a second hard-coded number. Without it the tally's
    // `grown: N` and the Power section's `N independent claim(s)` are the same
    // numeral meaning two different things, paragraphs apart, and a reader
    // pairs them into "every claim grew".
    let (grown_claims_n, measured_claims) = grown_claims(&items, &pairs);
    if measured_claims > 0 {
        s.push('\n');
        s.push_str(&wrap(&format!(
            "By CLAIM rather than by item, merging the near-collinear pairs listed above: \
             {grown_claims_n} of {measured_claims} measured claim(s) grew. A claim counts \
             as grown only when every item merged into it does. Cite this number, not the \
             item tally, when stating what the world grew — the item tally counts a merged \
             pair twice."
        )));
        s.push('\n');
    }

    s.push_str("\n## Unmeasured\n\n");
    let pending: Vec<&Item> = corpus
        .items
        .iter()
        .filter(|i| i.verdict == Verdict::Unmeasured)
        .collect();
    if pending.is_empty() {
        s.push_str("None — every item carries a coverage verdict.\n");
    } else {
        for item in pending {
            s.push_str(&format!("- `{}` — {}\n", item.id, table_text(&item.title)));
        }
    }

    // `absent` is two different things, and no verdict value separates them.
    s.push_str("\n## `absent` splits two ways\n\n");
    s.push_str(&wrap(
        "An `absent` verdict says only that the statistic cannot be computed and nobody \
         has registered it. That covers two very different situations. An item that \
         DECLARES the instrument which would settle it — in the corpus's own \
         `roadmap_instrument` field, not in its prose — is ROADMAP: the work is \
         identified. An item declaring none is a GAP: the mechanism itself is missing, and \
         there is nothing to point a column at.",
    ));
    s.push_str("\n\n");
    s.push_str(&wrap(
        "IDENTIFICATION IS NOT SIZING, and this split must not be read as an estimate. \
         Naming the right instrument says only that somebody knows what to build; several \
         of the items below need a new census metric or a new criterion kind, which is a \
         code change, a review and a test. Each roadmap item's declared instrument is \
         printed with it, so the size can be judged rather than assumed.",
    ));
    s.push_str("\n\n");
    let absent: Vec<&Item> = corpus
        .items
        .iter()
        .filter(|i| i.verdict == Verdict::Absent)
        .collect();
    let roadmap: Vec<&&Item> = absent.iter().filter(|i| names_an_instrument(i)).collect();
    s.push_str(&format!(
        "- roadmap (the item declares the instrument): {}\n",
        roadmap.len()
    ));
    s.push_str(&format!(
        "- gap (the mechanism is missing): {}\n",
        absent.len() - roadmap.len()
    ));
    if !roadmap.is_empty() {
        s.push_str("\nThe roadmap items, each with the instrument it declares:\n\n");
        for item in &roadmap {
            s.push_str(&format!("- `{}` — {}\n", item.id, table_text(&item.title)));
            s.push_str(&format!(
                "  - instrument: {}\n",
                table_text(item.roadmap_instrument.as_deref().unwrap_or(""))
            ));
        }
    }

    // The source's own taxonomy, and the third answer it admits.
    s.push_str("\n## Emergence type\n\n");
    s.push_str(&wrap(
        "The source's taxonomy (Epstein & Axtell, Ch. II footnote 24): type 1 is a \
         property meaningful for an individual but exhibited only by the collective; type \
         2 is a property meaningful only for a collective. An item where the taxonomy does \
         not apply carries neither — a model abstraction and a bare micro-rule assert no \
         regularity — and is EXCLUDED from the split rather than defaulted into a type.",
    ));
    s.push_str("\n\n");
    for t in [1u8, 2u8] {
        let held = corpus
            .items
            .iter()
            .filter(|i| i.emergence_type == Some(t))
            .count();
        let grown = corpus
            .items
            .iter()
            .filter(|i| i.emergence_type == Some(t) && i.verdict == Verdict::Grown)
            .count();
        s.push_str(&format!("- type {t}: {held} held, {grown} grown\n"));
    }
    let untyped = corpus
        .items
        .iter()
        .filter(|i| i.emergence_type.is_none())
        .count();
    s.push_str(&format!(
        "- taxonomy does not apply: {untyped} (excluded from the split above)\n"
    ));

    s.push_str(
        "\n## Items\n\n| id | title | type | verdict | statistic | anchor | note |\n\
         |---|---|---|---|---|---|---|\n",
    );
    for item in &corpus.items {
        s.push_str(&format!(
            "| {} | {} | {} | {} | {} | {} | {} |\n",
            item.id,
            table_text(&item.title),
            item.emergence_type
                .map(|t| t.to_string())
                .unwrap_or_else(|| "—".to_string()),
            verdict_name(item.verdict),
            item.statistic,
            table_text(item.anchor.as_deref().unwrap_or("")),
            table_text(&item.note)
        ));
    }
    s
}

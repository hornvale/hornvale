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
            if present.is_empty() {
                return false;
            }
            let mut sorted = present.to_vec();
            sorted.sort_by(f64::total_cmp);
            let mid = sorted.len() / 2;
            let median = if sorted.len().is_multiple_of(2) {
                (sorted[mid - 1] + sorted[mid]) / 2.0
            } else {
                sorted[mid]
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

/// Human name of a verdict, for failure text.
fn verdict_name(v: Verdict) -> &'static str {
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

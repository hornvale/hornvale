//! The `technologies/` corpus loader: does a *people* acquire, hold, and
//! lose this capability?
//!
//! Sixth corpus family, opened by decision 0986 (which applies decision
//! 0135's basis test and states this family's anchor rules; see
//! `technologies/CLAUDE.md` for the family's own law). Closest to
//! `regularities/` (`cli/src/regularities.rs`, the sibling this module's
//! shape mirrors) and distinguished from it by a per-people capability
//! trajectory rather than a macro-statistic over a population, by demands
//! **derived** by transitive closure over `presupposes` (decision 0386)
//! rather than declared, and by the one new verdict `lost`.
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
//! families.
//!
//! **Anchor resolution and decision 0136's four conditions** ([`audit`],
//! Task 5). Every verdict but `absent` cites an anchor this module
//! re-checks against live repository state, never against another copy of
//! the rule: `decision:`/`registry:` resolve through
//! [`crate::systems::RepoFacts`] (the same in-force-decision and
//! registry-status semantics `systems::audit` uses), `test:`/`path:`
//! resolve through that same `RepoFacts`'s mechanism-anchor machinery
//! (`pub(crate)`-widened for this reuse), and `doc:` resolves through
//! [`crate::regularities::GeneratedPaths`] (generated-and-declared only;
//! hand-written prose is refused). [`audit`] additionally enforces two
//! rules that exist only in prose before this task: `unmeasured` requires a
//! mechanism anchor exactly like `present` does (campaign ledger #18 — its
//! reach half is checkable, even though its trajectory is not), and a
//! registry row any sibling corpus cites must be ruled on — cited, or
//! refused in writing — by every other corpus in the family
//! (`technologies/CLAUDE.md`'s cross-corpus rule).
//!
//! **The criterion evaluator and the two-way trajectory guard** ([`meets`],
//! [`two_way`], Task 6). [`meets`] scores a [`Criterion`] against the
//! per-people values a census would report and the corpus's own population
//! count — never a bare boolean over the world, because "every surviving
//! community holds it" and "half do" must not score identically (the
//! defect this family exists to detect, `technologies/CLAUDE.md`'s "a bare
//! boolean is blind to divergence"). [`two_way`] compares an authored
//! trajectory verdict against what [`meets`] computes today and reddens in
//! *either* direction of disagreement, mirroring
//! [`crate::regularities`]'s own two-way guard (decision 0936) — a report
//! generator wiring the two together, over the real census, is Task 7's
//! work.
//!
//! The corpus is DATA (`technologies/*.technology.json`) and this module is
//! its RESOLVER (decision 0011). Nothing in `domains/*` or `windows/*` reads
//! a corpus file.
use serde::Deserialize;
use std::collections::{BTreeMap, BTreeSet};
use std::path::Path;

/// The two committed corpora' ids — what a caller types
/// (`hornvale technologies report asimov-1989`), never a path. Unlike the
/// sibling families' `CORPORA: &[&str]` (a path list, resolved with
/// `--corpus <path>`), this family's CLI surface takes the corpus by its
/// short id (Task 7's brief states the exact invocation); [`corpus_path`]
/// derives the file path from the id and the fixed `technologies/
/// <id>.technology.json` naming convention every corpus in this directory
/// already follows (see [`cross_corpus_ruling_gaps`], which derives an id
/// from a path the same way in reverse).
/// type-audit: bare-ok(artifact)
pub const CORPORA: &[&str] = &["asimov-1989", "henrich-2004-extended"];

/// Resolve a corpus id (`asimov-1989`) to its file path. `None` for an id not
/// in [`CORPORA`] — a caller should report this as an unknown corpus, never
/// silently fall back to a default.
/// type-audit: bare-ok(identifier-text: id), bare-ok(artifact: return)
pub fn corpus_path(id: &str) -> Option<String> {
    if CORPORA.contains(&id) {
        Some(format!("technologies/{id}.technology.json"))
    } else {
        None
    }
}

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
/// Two kinds, both scored by [`meets`]. `FractionInBand` divides by the
/// **population** (every people the corpus scores, present or not), never
/// by the number of values on hand — the same discipline
/// `regularities::Criterion::FractionInBandAtLeast` and `PresentOnFraction`
/// hold, so a statistic only a handful of peoples happen to report cannot
/// score as though the whole world agreed. `MedianInBand` matches its
/// `regularities` namesake exactly: the median of the present values,
/// inclusive at both edges.
///
/// **The wire tag is pinned explicitly on each variant**, not left to the
/// `rename_all = "kebab-case"` derive. Task 3's review flagged this as a
/// family-wide exposure (`regularities::Criterion` carries the identical
/// latent shape): a derived tag matches today's data by coincidence, and
/// renaming a variant would silently change the wire format both frozen
/// corpora already parse against, with nothing to catch it. Pinning removes
/// that coincidence rather than merely documenting it.
///
/// An unrecognized `kind` is a parse error naming the offending value, via
/// `serde`'s own tagged-enum deserialization — the same mechanism
/// `regularities::Criterion` relies on, not a hand-written check.
/// type-audit: bare-ok(ratio: FractionInBand.lo), bare-ok(ratio: FractionInBand.hi), bare-ok(ratio: MedianInBand.lo), bare-ok(ratio: MedianInBand.hi)
#[derive(Debug, Clone, Copy, PartialEq, Deserialize)]
#[serde(tag = "kind", rename_all = "kebab-case")]
pub enum Criterion {
    /// The statistic is the population-weighted aggregate `sum(values) /
    /// population` (an unreported people contributes zero); the criterion
    /// checks that aggregate against the inclusive band `[lo, hi]`.
    ///
    /// This is the family's defense against the pathology it exists to
    /// detect: "every people holds it" (aggregate `1.0`) and "no people
    /// holds it" (aggregate `0.0`) both fail a divergence band such as
    /// `[0.15, 0.85]`, and only a genuinely mixed world (aggregate `0.5`)
    /// passes.
    ///
    /// **This is NOT a count of how many per-people values individually
    /// fall inside `[lo, hi]`.** That reading is wrong for this criterion
    /// and is ruled out by
    /// `technology_coverage::a_universal_holding_fraction_fails_a_divergence_band`'s
    /// third assertion (`meets(&c, &[0.5; 10], 10)` is `true`): every one
    /// of those ten values already lies inside `[0.15, 0.85]`, so a
    /// count-in-band reading would compute `10/10 = 1.0`, itself outside
    /// the band, and wrongly return `false`. Only the aggregate reading —
    /// `sum([0.5; 10]) / 10 = 0.5`, which IS inside `[0.15, 0.85]` —
    /// produces the `true` that test requires. Naming this explicitly
    /// because the sibling `regularities` criterion this one is modeled on
    /// has a test named `fraction_in_band_counts_only_values_inside_it`,
    /// which is the ruled-out reading's name, not this one's.
    #[serde(rename = "fraction-in-band")]
    FractionInBand {
        /// Inclusive lower edge.
        lo: f64,
        /// Inclusive upper edge.
        hi: f64,
    },
    /// The median of the present per-people values lies in the inclusive
    /// band `[lo, hi]`.
    #[serde(rename = "median-in-band")]
    MedianInBand {
        /// Inclusive lower edge.
        lo: f64,
        /// Inclusive upper edge.
        hi: f64,
    },
}

/// Whether a frozen criterion is met by a set of per-people statistic
/// readings.
///
/// `values` holds one reading per people that reported the statistic;
/// `population` is the total number of peoples this item scores against,
/// **including** any that reported none. The two differ, and
/// `FractionInBand` divides by `population`, never by `values.len()`: a
/// people that never reported contributes zero to the aggregate rather than
/// being excluded from it, so a statistic only a handful of peoples happen
/// to report cannot score as though the whole world agreed (the same
/// discipline `regularities::meets`'s doc comment states for its own
/// fraction criteria).
///
/// An empty population never meets a criterion — an honest `false`, never a
/// vacuous `true`: there is nothing to measure, so nothing can be in-band.
/// type-audit: bare-ok(ratio: values), bare-ok(count: population), bare-ok(flag: return)
pub fn meets(c: &Criterion, values: &[f64], population: usize) -> bool {
    if population == 0 {
        return false;
    }
    match c {
        Criterion::FractionInBand { lo, hi } => {
            let aggregate = values.iter().sum::<f64>() / population as f64;
            aggregate >= *lo && aggregate <= *hi
        }
        Criterion::MedianInBand { lo, hi } => match crate::regularities::median(values) {
            Some(m) => m >= *lo && m <= *hi,
            None => false,
        },
    }
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

/// The items in `id`'s `presupposes` closure, **excluding `id` itself** —
/// that is, its prerequisites, however deep.
///
/// Distinct from [`derived_demands`] and deliberately not built on it:
/// `derived_demands` returns `introduces` TOKENS and includes the item's
/// own, which is right for a demand set and wrong for the chosen/inherited
/// cut ([`is_chosen`]), which has to read each prerequisite's VERDICT and
/// therefore needs item ids. Nothing guarantees an `introduces` token is
/// recoverable to the item that introduced it, so deriving ids back out of
/// a token set would be a second, weaker lattice walk.
///
/// Assumes the corpus passed [`parse`]'s lattice validation, for the same
/// reason [`derived_demands`] does.
fn prerequisite_items(c: &Corpus, id: &str) -> BTreeSet<String> {
    let by_id: BTreeMap<&str, &Item> = c.items.iter().map(|i| (i.id.as_str(), i)).collect();
    let mut out: BTreeSet<String> = BTreeSet::new();
    let mut seen: BTreeSet<&str> = BTreeSet::new();
    let mut stack: Vec<&str> = vec![id];
    while let Some(current) = stack.pop() {
        if !seen.insert(current) {
            continue;
        }
        if current != id {
            out.insert(current.to_string());
        }
        if let Some(item) = by_id.get(current) {
            for p in &item.presupposes {
                stack.push(p.as_str());
            }
        }
    }
    out
}

/// Whether item `id`'s verdict is **chosen** rather than **inherited**
/// (`technologies/CLAUDE.md`'s chosen/inherited cut; campaign ledger #13).
///
/// Chosen means **no prerequisite anywhere in `id`'s closure is `absent`**,
/// so nothing upstream forces the verdict and it rests on a search of the
/// repository by a session that had read the model. Inherited means at
/// least one is, and the weakest-demand rule reads the verdict off that
/// prerequisite rather than off any Hornvale fact.
///
/// **Not "is a root".** Roots (items with no `presupposes` at all) are a
/// STRICT SUBSET of the chosen items, so a root-keyed check under-covers by
/// exactly the items whose prerequisites are all non-`absent` — which can be
/// true from a corpus's first authoring and needs no trigger. That proxy is
/// what ledger #13 found, twice, and what family law names as the thing this
/// function must not be: `inv-parchment` is chosen, is not a root, and was
/// chosen at the freeze.
///
/// An unknown `id` has no prerequisites and therefore reads chosen — an
/// honest answer for a question about an item that is not in the corpus,
/// and unreachable from [`disclosure_gaps`], which only ever asks about
/// items it iterated out of `c`.
/// type-audit: bare-ok(identifier-text: id), bare-ok(flag: return)
pub fn is_chosen(c: &Corpus, id: &str) -> bool {
    let by_id: BTreeMap<&str, &Item> = c.items.iter().map(|i| (i.id.as_str(), i)).collect();
    !prerequisite_items(c, id).iter().any(|p| {
        by_id
            .get(p.as_str())
            .is_some_and(|i| i.verdict == Verdict::Absent)
    })
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

// --- Anchor resolution and decision 0136's four conditions (Task 5) ------

/// A verdict's backing evidence, parsed from its `anchor` string.
///
/// The UNION of the sibling families' anchor vocabularies, not a subset of
/// either: `test:`/`path:` (mechanism, like `systems::Anchor`, for
/// `present` and `unmeasured`) and `doc:` (a generated page, like
/// `regularities::Anchor`, for `grown`/`flat`/`lost`) both appear, because
/// this family's verdict table needs both kinds — a capability Hornvale has
/// built (mechanism) and a capability a people's trajectory was measured to
/// have (a generated document). `decision:`/`registry:`/`reason:` are
/// shared with both siblings.
/// type-audit: bare-ok(identifier-text: Test.0), bare-ok(artifact: Path.0), bare-ok(identifier-text: Decision.0), bare-ok(identifier-text: Registry.0), bare-ok(prose: Reason.0), bare-ok(artifact: Doc.0)
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Anchor {
    /// `test:<crate>::<fn>` — a mechanism anchor. For `present`/`unmeasured`.
    Test(String),
    /// `path:<file>` — the weaker mechanism anchor. For `present`/`unmeasured`.
    Path(String),
    /// `decision:NNNN` — must be in force. For `refused`.
    Decision(String),
    /// `registry:<row-id>` — must exist and not read a falsifying status.
    /// For `deferred`.
    Registry(String),
    /// `reason:<prose>` — must carry non-empty text. For `inapplicable`.
    Reason(String),
    /// `doc:<path>` — a generated, drift-checked page. For
    /// `grown`/`flat`/`lost`.
    Doc(String),
}

impl Anchor {
    /// Parse an anchor string. `None` when the prefix is unknown.
    /// type-audit: bare-ok(identifier-text: s)
    pub fn parse(s: &str) -> Option<Anchor> {
        let (kind, rest) = s.split_once(':')?;
        match kind {
            "test" => Some(Anchor::Test(rest.to_string())),
            "path" => Some(Anchor::Path(rest.to_string())),
            "decision" => Some(Anchor::Decision(rest.to_string())),
            "registry" => Some(Anchor::Registry(rest.to_string())),
            "reason" => Some(Anchor::Reason(rest.to_string())),
            "doc" => Some(Anchor::Doc(rest.to_string())),
            _ => None,
        }
    }
}

/// One thing wrong with an item's verdict and its evidence, or with the
/// corpus as a whole — decision 0136's four conditions, plus this family's
/// own two-way trajectory guard (decision 0936's guard, widened from its
/// `regularities` origin). Every variant carries enough to diagnose it
/// without knowing this instrument exists (the same diagnosability standard
/// `systems::Finding` and `regularities::Finding` carry): this family's two
/// inputs are edited by sessions with no reason to know a resolver reads
/// them.
/// type-audit: bare-ok(identifier-text: Unjustified.id), bare-ok(prose: Unjustified.why), bare-ok(identifier-text: Dangling.id), bare-ok(identifier-text: Dangling.anchor), bare-ok(prose: Dangling.why), bare-ok(identifier-text: StaleDeferred.id), bare-ok(identifier-text: StaleDeferred.row), bare-ok(prose: StaleDeferred.why), bare-ok(identifier-text: Regressed.id), bare-ok(prose: Regressed.why), bare-ok(identifier-text: Disclosure.id), bare-ok(flag: Disclosure.chosen), bare-ok(prose: Disclosure.why), bare-ok(identifier-text: Novelty.corpus), bare-ok(count: Novelty.baseline), bare-ok(count: Novelty.found)
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Finding {
    /// A verdict with no anchor, the wrong kind of anchor, an anchor into
    /// hand-written prose where a generated page is required, or — the
    /// cross-corpus rule — a registry row a sibling corpus cites that this
    /// corpus never rules on.
    Unjustified {
        /// The item's corpus-local id, or (the cross-corpus finding only)
        /// the registry row id that went unruled.
        id: String,
        /// What is wrong, in a sentence.
        why: String,
    },
    /// The anchor stopped resolving against live repository state.
    Dangling {
        /// The item's corpus-local id.
        id: String,
        /// The anchor as authored.
        anchor: String,
        /// What would have caused this, and the two legitimate repairs.
        why: String,
    },
    /// A `deferred` verdict whose registry row now reads a status that
    /// falsifies "planned, not built" (see
    /// [`crate::systems::DEFERRAL_FALSIFYING_STATUSES`]).
    StaleDeferred {
        /// The item's corpus-local id.
        id: String,
        /// The registry row that shipped (or otherwise settled).
        row: String,
        /// What to do about it.
        why: String,
    },
    /// An authored trajectory verdict and the one [`meets`] computes today
    /// disagree — in either direction ([`two_way`]). A `grown` that
    /// computes `flat` is a lost regularity; a `flat` that computes `grown`
    /// is stale pessimism.
    Regressed {
        /// The item's corpus-local id.
        id: String,
        /// What the corpus claims.
        authored: Verdict,
        /// What [`meets`] computes against fresh values today.
        computed: Verdict,
        /// What to do about it.
        why: String,
    },
    /// The chosen/inherited disclosure rule is violated, in either
    /// direction (`technologies/CLAUDE.md`, "Task 4's resolver must enforce
    /// the chosen rule, two-directionally"; campaign ledger #13's second
    /// ruling; `henrich-2004-extended`'s `provenance` states it as a MUST).
    ///
    /// A **separate variant rather than an `Unjustified`**, for three
    /// reasons. (1) The two directions want opposite repairs and the
    /// direction must be machine-readable, not only buried in prose — hence
    /// `chosen`, which a caller can branch on. (2) `Unjustified`'s own `id`
    /// field already carries two meanings (an item id, or a registry row id
    /// for the cross-corpus finding); a third would make the field
    /// unreadable. (3) Adding a variant makes every exhaustive `match` over
    /// `Finding` fail to COMPILE until it handles this case — the CLI's
    /// `technologies check` arm included — so the rule cannot be enforced
    /// by the resolver and silently dropped by its one reporting surface.
    /// That is the compiler doing the enumeration, which this project
    /// prefers to a reviewer doing it.
    Disclosure {
        /// The item's corpus-local id.
        id: String,
        /// Which direction: `true` when the item's verdict is CHOSEN (no
        /// prerequisite anywhere in its closure is `absent`) and it carries
        /// no `disclosure`; `false` when it is INHERITED and carries one.
        chosen: bool,
        /// What is wrong and what to do about it.
        why: String,
    },
    /// The `absent` count rose above its baseline — 0136's one
    /// falsification-by-count guard. `inapplicable`'s tally is deliberately
    /// not ratcheted; only `absent` is.
    Novelty {
        /// The corpus id, e.g. `asimov-1989`.
        corpus: String,
        /// The baseline `absent` count this corpus is held to.
        baseline: usize,
        /// The `absent` count found in the live corpus.
        found: usize,
    },
}

/// Human name of a verdict, for failure text.
fn verdict_name(v: Verdict) -> &'static str {
    match v {
        Verdict::Present => "present",
        Verdict::Refused => "refused",
        Verdict::Deferred => "deferred",
        Verdict::Absent => "absent",
        Verdict::Inapplicable => "inapplicable",
        Verdict::Grown => "grown",
        Verdict::Flat => "flat",
        Verdict::Lost => "lost",
        Verdict::Unmeasured => "unmeasured",
    }
}

/// The two-way trajectory guard (decision 0936, whose two-way rule this
/// family inherits wholesale from its closest sibling `regularities/`):
/// compare an authored trajectory verdict against what [`meets`] computes
/// fresh today, and red on either direction of disagreement. `None` when
/// they agree, or when the pair is not one this guard covers.
///
/// **Two-way, exactly as 0936 states it (its own lines 58-60):**
/// - authored `grown`, computed `flat` — a regularity was lost.
/// - authored `flat`, computed `grown` — stale pessimism; a real gain is
///   claimed deliberately, in a commit that says so, never left for a
///   reader to notice on their own.
///
/// A one-directional guard would be half built, and the half it skipped is
/// the one that lets a corpus quietly under-report the world
/// (`technologies/CLAUDE.md`'s "a bare boolean is blind to divergence" is
/// the same failure shape one axis over).
///
/// **Scoped to the two verdicts [`meets`] can actually produce.** `meets`
/// returns a `bool`, so the only `computed` values this guard ever sees in
/// practice are [`Verdict::Grown`] and [`Verdict::Flat`]. This family's
/// third measured value, [`Verdict::Lost`], names a capability a people
/// held and then released — a trajectory a single-snapshot criterion cannot
/// detect, which is the successor campaign's job (`technologies/
/// CLAUDE.md`'s scope note for `lost`), not a gap in this guard. So the
/// guard fires only when BOTH sides are `Grown` or `Flat` and disagree;
/// anything else — `Lost` on either side, and [`Verdict::Unmeasured`], a
/// lifecycle state that is never a coverage verdict — raises nothing YET
/// (see [`Verdict::Lost`]'s own scope note; the successor campaign is what
/// changes that).
///
/// **Takes the item's `id`, unlike the plan's original two-verdict
/// signature.** 0136 makes diagnosability part of the decision, and this
/// is the verdict that fires when a capability was LOST — the one event
/// this whole campaign exists to make visible — so a finding that could not
/// name which item regressed would fail that standard on its own first use
/// (campaign ledger #30).
///
/// **No live case exists yet** (campaign ledger #27): every trajectory
/// verdict in both committed corpora is `unmeasured`, so this is exercised
/// only against constructed input until a successor campaign makes loss
/// measurable — it must be correct before then, not after.
/// type-audit: bare-ok(identifier-text: id)
pub fn two_way(id: &str, authored: Verdict, computed: Verdict) -> Option<Finding> {
    if authored == computed {
        return None;
    }
    if !matches!(authored, Verdict::Grown | Verdict::Flat)
        || !matches!(computed, Verdict::Grown | Verdict::Flat)
    {
        return None;
    }
    let why = if authored == Verdict::Grown {
        format!(
            "{id}: authored `{}`, computed `{}`: a capability this corpus claims a people \
             GROWS no longer measures grown. That is a finding about the world, not the \
             corpus: investigate the cause before re-verdicting; if the loss is accepted, \
             change the verdict to `flat` in a commit that says why.",
            verdict_name(authored),
            verdict_name(computed)
        )
    } else {
        format!(
            "{id}: authored `{}`, computed `{}`: a capability this corpus records as FLAT \
             now measures grown. Stale pessimism is red for the same reason a loss is: a \
             corpus that under-reports the world is as wrong as one that over-reports it. \
             Promote the verdict to `grown` deliberately, in a commit that claims the gain.",
            verdict_name(authored),
            verdict_name(computed)
        )
    };
    Some(Finding::Regressed {
        id: id.to_string(),
        authored,
        computed,
        why,
    })
}

/// The anchor kind(s) a verdict requires, for failure text.
fn expected_anchor_kind(v: Verdict) -> &'static str {
    match v {
        // `unmeasured` requires a mechanism anchor exactly like `present`
        // does (campaign ledger #18 — `technologies/CLAUDE.md`: "unmeasured
        // CARRIES A MECHANISM ANCHOR"). This is the opposite of
        // `regularities::Verdict::Unmeasured`, which is a different
        // family's lifecycle state and raises nothing at all; do not copy
        // that family's rule here.
        Verdict::Present | Verdict::Unmeasured => "a `test:` or `path:` anchor",
        Verdict::Refused => "a `decision:` anchor",
        Verdict::Deferred => "a `registry:` anchor",
        Verdict::Inapplicable => "a `reason:` anchor",
        Verdict::Grown | Verdict::Flat | Verdict::Lost => "a `doc:` anchor into generated prose",
        Verdict::Absent => "no anchor",
    }
}

/// The anchor's own kind, for failure text.
fn anchor_kind_name(a: &Anchor) -> &'static str {
    match a {
        Anchor::Test(_) => "test:",
        Anchor::Path(_) => "path:",
        Anchor::Decision(_) => "decision:",
        Anchor::Registry(_) => "registry:",
        Anchor::Reason(_) => "reason:",
        Anchor::Doc(_) => "doc:",
    }
}

/// Whether `a`'s kind is one the verdict `v` permits.
fn anchor_matches_verdict(v: Verdict, a: &Anchor) -> bool {
    matches!(
        (v, a),
        (Verdict::Present, Anchor::Test(_))
            | (Verdict::Present, Anchor::Path(_))
            | (Verdict::Unmeasured, Anchor::Test(_))
            | (Verdict::Unmeasured, Anchor::Path(_))
            | (Verdict::Refused, Anchor::Decision(_))
            | (Verdict::Deferred, Anchor::Registry(_))
            | (Verdict::Inapplicable, Anchor::Reason(_))
            | (Verdict::Grown, Anchor::Doc(_))
            | (Verdict::Flat, Anchor::Doc(_))
            | (Verdict::Lost, Anchor::Doc(_))
    )
}

/// The NOVELTY ratchet's baseline (decision 0136): the `absent` count
/// measured when this corpus's resolver was first written, derived from the
/// committed files rather than guessed — `asimov-1989` at 295 (re-measured
/// for The Cadastre's closed 301-item population, Task 2; the 41-item arc
/// corpus's own figure was 35), `henrich-2004-extended` at 31. `None` for a
/// corpus with no baseline on record,
/// which [`audit`] treats as "nothing to ratchet against," never as a
/// silent pass disguised as a baseline of zero.
///
/// There is no committed report for this family yet (that is a later
/// task's), so this is the first and — until one lands — the only place the
/// baseline is recorded. A fixture corpus reusing one of these two corpus
/// ids exercises the real baseline on constructed data, which is how this
/// ratchet is tested without waiting for the real corpora to regress.
/// type-audit: bare-ok(identifier-text: corpus_id), bare-ok(count: return)
pub fn novelty_baseline(corpus_id: &str) -> Option<usize> {
    match corpus_id {
        "asimov-1989" => Some(295),
        "henrich-2004-extended" => Some(31),
        _ => None,
    }
}

/// Audit a corpus against live repository state (`root`): every item's
/// verdict against its anchor, plus the NOVELTY ratchet over the whole
/// corpus's `absent` count. One finding per problem; a clean corpus returns
/// an empty vector.
///
/// **Does not include the cross-corpus ruling-completeness rule** (Step
/// 4b.2) — that is [`cross_corpus_ruling_gaps`], deliberately a separate
/// function. Folding it in here would make every single-item fixture this
/// module's own tests build (and every sibling family's own `corpus_with`-
/// style helper) also score against the two real committed corpora under
/// `root/technologies/`, which is almost never what an isolated per-item
/// test wants — see that function's own doc comment for what it checks and
/// why it takes the same `(c, root)` shape as this one.
///
/// Gathers [`crate::systems::RepoFacts`] and
/// [`crate::regularities::GeneratedPaths`] from `root` itself rather than
/// taking them as parameters (unlike the siblings' own `audit`): this
/// family's resolver has no CLI surface yet, so there is no caller that
/// gathers them once and shares them across columns the way
/// `systems::render_matrix` does. Panics if either gather fails — the same
/// fail-loudly standard [`load`] holds, because a caller unable to read
/// `docs/digest/decisions-in-force.md` or `docs/generated-paths.txt` has no
/// meaningful audit to report.
pub fn audit(c: &Corpus, root: &Path) -> Vec<Finding> {
    let facts = crate::systems::RepoFacts::gather(root)
        .unwrap_or_else(|e| panic!("technology audit: gathering repo facts: {e}"));
    let generated = crate::regularities::GeneratedPaths::read(root)
        .unwrap_or_else(|e| panic!("technology audit: gathering generated-paths facts: {e}"));

    let mut findings: Vec<Finding> = c
        .items
        .iter()
        .filter_map(|item| audit_item(item, &facts, &generated))
        .collect();

    let absent_count = c
        .items
        .iter()
        .filter(|i| i.verdict == Verdict::Absent)
        .count();
    if let Some(baseline) = novelty_baseline(&c.corpus)
        && absent_count > baseline
    {
        findings.push(Finding::Novelty {
            corpus: c.corpus.clone(),
            baseline,
            found: absent_count,
        });
    }

    findings
}

/// All of it together: [`audit`]'s per-item anchor resolution and NOVELTY
/// ratchet, concatenated with [`cross_corpus_ruling_gaps`]'s and
/// [`disclosure_gaps`]'s family-law checks. The three stay separate
/// functions (see the latter two's doc comments for why folding them into
/// `audit` would break every isolated fixture in this crate's own tests),
/// but a caller auditing a REAL corpus almost always wants all of them — a
/// report generator that called only `audit` would silently never enforce
/// family law at all, which is exactly the shape of gap this family exists
/// to catch in everyone else's instruments. This function is that one entry
/// point, so skipping family law would have to be a deliberate choice to
/// call `audit` alone, not an accident of not knowing the other functions
/// exist.
///
/// **`disclosure_gaps` joined this list in the pre-merge fix wave, and the
/// reason is worth keeping**: the rule it enforces was ratified (ledger
/// #13), written into family law, and published as a MUST in a frozen
/// corpus's `provenance` — which is printed verbatim into the committed
/// report — and then never built. It held in the data by coincidence the
/// whole time, so nothing was ever wrong and nothing ever noticed (ledger
/// #38). A rule with a prose home, a report that quotes it, and no
/// contradictor is the shape this family exists to find.
pub fn audit_family(c: &Corpus, root: &Path) -> Vec<Finding> {
    let mut findings = audit(c, root);
    findings.extend(cross_corpus_ruling_gaps(c, root));
    findings.extend(disclosure_gaps(c));
    findings
}

/// Audit a single item. `None` means clean.
fn audit_item(
    item: &Item,
    facts: &crate::systems::RepoFacts,
    generated: &crate::regularities::GeneratedPaths,
) -> Option<Finding> {
    if item.verdict == Verdict::Absent {
        if item.anchor.is_empty() {
            return None;
        }
        return Some(Finding::Unjustified {
            id: item.id.clone(),
            why: format!(
                "{} has verdict `absent` but carries anchor `{}`. An `absent` verdict \
                 claims nothing and must carry no anchor — remove the anchor, or change \
                 the verdict to the one the anchor actually supports.",
                item.id, item.anchor
            ),
        });
    }

    if item.anchor.is_empty() {
        return Some(Finding::Unjustified {
            id: item.id.clone(),
            why: format!(
                "{} has verdict `{}` but no anchor. Every verdict except `absent` must \
                 cite evidence: {} is required. Add an anchor, or change the verdict to \
                 `absent` if there is truly nothing to cite.",
                item.id,
                verdict_name(item.verdict),
                expected_anchor_kind(item.verdict)
            ),
        });
    }

    let Some(anchor) = Anchor::parse(&item.anchor) else {
        return Some(Finding::Unjustified {
            id: item.id.clone(),
            why: format!(
                "{} cites `{}`, which has an unrecognized anchor prefix. Expected one of \
                 test:, path:, decision:, registry:, reason:, doc:.",
                item.id, item.anchor
            ),
        });
    };

    if !anchor_matches_verdict(item.verdict, &anchor) {
        return Some(Finding::Unjustified {
            id: item.id.clone(),
            why: format!(
                "{} has verdict `{}`, which requires {}, but its anchor `{}` is a {} \
                 anchor.",
                item.id,
                verdict_name(item.verdict),
                expected_anchor_kind(item.verdict),
                item.anchor,
                anchor_kind_name(&anchor)
            ),
        });
    }

    // An empty `reason:` is the reasonless-`inapplicable` failure
    // generalized — the same check `systems::audit_item` and
    // `regularities::audit_item` both make before ever resolving the
    // anchor against live state, because a `reason:` anchor's prose is
    // never checked against anything else.
    if let Anchor::Reason(reason) = &anchor
        && reason.trim().is_empty()
    {
        return Some(Finding::Unjustified {
            id: item.id.clone(),
            why: format!(
                "{} has verdict `inapplicable` but its `reason:` anchor carries no \
                 prose. State why this item does not apply.",
                item.id
            ),
        });
    }

    resolve_anchor(item, &anchor, facts, generated)
}

/// The repair a `StaleDeferred` finding should suggest, keyed by the
/// registry row's actual (normalized) status — the repairs genuinely
/// differ, so the message must name the status rather than assuming
/// `shipped`. A `refuted` row in particular has nothing built to promote:
/// its central claim was TESTED and found false, so "re-verdict to
/// `present`" would be nonsense advice for it, unlike for `shipped` or
/// `ratified`.
///
/// A local copy of `systems::deferral_repair_advice`'s per-status
/// judgement, not a reuse of it: that function is private, and the four
/// statuses it distinguishes are a small, closed, shared vocabulary
/// (`systems::DEFERRAL_FALSIFYING_STATUSES`, which this module already
/// imports directly) — widening a fourth private item across this module
/// boundary for one `match` is a worse trade than restating four short
/// phrases in this family's own verdict vocabulary (`present`/`absent`/
/// `refused`, not `systems`'s `present`/`refused` pair).
fn deferral_repair_advice(status: &str) -> &'static str {
    match status {
        "shipped" => {
            "promote this item to `present` (citing the shipping mechanism) or to \
             `absent`/`refused` if what shipped does not actually discharge this \
             item's demand"
        }
        "ratified" => {
            "promote this item to `present` (citing the decision, or the mechanism it \
             enforces, if Hornvale now has the capability) or to `refused` (citing \
             `decision:NNNN`) if the ratified decision settled the question by \
             forbidding it"
        }
        "rejected" => {
            "re-verdict this item to `absent` (the idea was considered and set aside, \
             with no plan behind it now) or to `refused` if a specific decision now \
             forbids it"
        }
        "refuted" => {
            "re-verdict this item to `absent` — a `refuted` row's central claim was \
             tested and found false, with no artifact shipped from it, so there is \
             nothing built to promote"
        }
        _ => "re-verdict this item to whatever the registry's current status actually settled",
    }
}

/// Verify `anchor` still resolves against live repository state. `None`
/// means clean.
fn resolve_anchor(
    item: &Item,
    anchor: &Anchor,
    facts: &crate::systems::RepoFacts,
    generated: &crate::regularities::GeneratedPaths,
) -> Option<Finding> {
    match anchor {
        Anchor::Decision(d) => {
            if facts.decision_in_force(d) {
                None
            } else {
                Some(Finding::Dangling {
                    id: item.id.clone(),
                    anchor: item.anchor.clone(),
                    why: format!(
                        "{} cites decision:{d}, which is not in \
                         docs/digest/decisions-in-force.md. A decision leaves that file \
                         when it is wholly superseded, so the likely cause is a \
                         supersession. Two legitimate repairs: re-verdict this item \
                         against the superseding decision, or restore the anchor if the \
                         supersession was only partial and this item's claim still \
                         stands.",
                        item.id
                    ),
                })
            }
        }
        Anchor::Registry(r) => match facts.registry_status(r) {
            None => Some(Finding::Dangling {
                id: item.id.clone(),
                anchor: item.anchor.clone(),
                why: format!(
                    "{} cites registry:{r}, which does not appear in \
                     book/src/frontier/idea-registry.md. The likely cause is a rename or \
                     a removal of that row. Two legitimate repairs: fix the anchor if the \
                     row id changed, or re-verdict this item against whatever replaced \
                     it.",
                    item.id
                ),
            }),
            Some(status) if crate::systems::DEFERRAL_FALSIFYING_STATUSES.contains(&status) => {
                Some(Finding::StaleDeferred {
                    id: item.id.clone(),
                    row: r.clone(),
                    why: format!(
                        "{} defers to registry:{r}, which now reads `{status}` in \
                         book/src/frontier/idea-registry.md — a status flip that settled \
                         the row's question. A `deferred` verdict claims \"planned, not \
                         built\"; that claim stopped being true — {}.",
                        item.id,
                        deferral_repair_advice(status)
                    ),
                })
            }
            Some(_) => None,
        },
        Anchor::Test(t) => match facts.test_resolution(t) {
            crate::systems::TestResolution::Runs => None,
            crate::systems::TestResolution::Missing => Some(Finding::Dangling {
                id: item.id.clone(),
                anchor: item.anchor.clone(),
                why: format!(
                    "{} cites test:{t}, which does not resolve to a known crate and a \
                     `fn` definition of that name at a word boundary. The likely cause \
                     is a rename or removal. Two legitimate repairs: fix the anchor if \
                     the function moved, or re-verdict this item if it was removed.",
                    item.id
                ),
            }),
            crate::systems::TestResolution::Ignored => Some(Finding::Dangling {
                id: item.id.clone(),
                anchor: item.anchor.clone(),
                why: format!(
                    "{} cites test:{t}, which exists but is `#[ignore]`d — the test does \
                     not run under the gate, so it proves nothing about this item. Two \
                     legitimate repairs: cite a test the gate actually runs, or weaken \
                     the verdict.",
                    item.id
                ),
            }),
        },
        Anchor::Path(p) => {
            if !crate::systems::path_anchor_is_well_formed(p) {
                Some(Finding::Dangling {
                    id: item.id.clone(),
                    anchor: item.anchor.clone(),
                    why: format!(
                        "{} cites path:{p}, which is not a well-formed repo-relative \
                         path — empty, absolute, or escaping the repo with a `..` \
                         component. None of those can be what a path anchor means. The \
                         one legitimate repair is to fix the anchor to a real path \
                         relative to the repo root, e.g. `path:domains/history/src/\
                         record.rs`.",
                        item.id
                    ),
                })
            } else if facts.path_exists(p) {
                None
            } else {
                Some(Finding::Dangling {
                    id: item.id.clone(),
                    anchor: item.anchor.clone(),
                    why: format!(
                        "{} cites path:{p}, which does not exist in the repo. Two \
                         legitimate repairs: fix the anchor if the file moved, or \
                         re-verdict this item if it was deleted.",
                        item.id
                    ),
                })
            }
        }
        Anchor::Reason(_) => None,
        Anchor::Doc(path) => {
            // `has_generator` resolves by LONGEST DECLARED DIRECTORY
            // PREFIX (`GeneratedPaths::declaration`), so a nonexistent file
            // under a real generated directory — `doc:book/src/domesday/
            // utterly-made-up.md` inheriting `book/src/domesday/ ->
            // artifacts` — reads as generated without ever having been
            // written. `regularities::doc_states_the_claim` closed exactly
            // this for its own family (its comment records the experiment:
            // "campaign's final review proved it vacuous by repointing an
            // item's anchor at `book/src/domesday/climate.md` … and
            // watching the whole suite stay green"). `page_text` is the
            // one call that answers "does this page actually exist", and
            // is checked here for the SAME reason, independent of whether a
            // claim-marker line is ever checked inside it (ledger #28).
            if generated.has_generator(path) {
                return match generated.page_text(path) {
                    Ok(_) => None,
                    Err(why) => Some(Finding::Dangling {
                        id: item.id.clone(),
                        anchor: item.anchor.clone(),
                        why: format!(
                            "{} cites doc:{path}, which docs/generated-paths.txt \
                             declares generated but which cannot be read ({why}). \
                             Either the page moved (fix the anchor and the \
                             declaration together) or the regeneration that \
                             authors it has not been run in this checkout.",
                            item.id
                        ),
                    }),
                };
            }
            // The two ways `has_generator` says no want opposite repairs —
            // mirroring `regularities::resolve_anchor`'s `Anchor::Doc` arm,
            // which this reuses rather than re-deriving. Unlike that
            // sibling, this family has no claim-marker page to check a
            // second half against yet (the Domesday renders no
            // technology-trajectory claim today) — that half, and only
            // that half, is later tasks' work; the existence check above is
            // not.
            let detail = if generated.is_declared(path) {
                "which docs/generated-paths.txt declares `none(<reason>)` — hand-written \
                 prose no roster set regenerates. Anchoring a measured verdict there \
                 would be decision 0330's failure: a declaration that moves the score \
                 without moving the world. Re-anchor to the generated page that states \
                 the measured claim, or re-verdict this item."
            } else {
                "which docs/generated-paths.txt does not declare at all, so nothing \
                 asserts it is regenerated or drift-checked. Either the page moved (fix \
                 the anchor), or it is generated but undeclared (add its row to \
                 docs/generated-paths.txt), or it is hand-written and cannot back a \
                 verdict."
            };
            Some(Finding::Unjustified {
                id: item.id.clone(),
                why: format!("{} cites doc:{path}, {detail}", item.id),
            })
        }
    }
}

/// Whether `haystack` mentions the registry-row token `needle` as a whole
/// token, not merely as a substring of a longer one (`MAP-8` inside
/// `MAP-80` must not count). A boundary-aware `contains`, not a parse.
fn mentions_row(haystack: &str, needle: &str) -> bool {
    if needle.is_empty() {
        return false;
    }
    let is_token_char = |c: char| c.is_ascii_alphanumeric() || c == '-';
    let mut start = 0;
    while let Some(offset) = haystack[start..].find(needle) {
        let idx = start + offset;
        let before_ok = haystack[..idx]
            .chars()
            .next_back()
            .map(|c| !is_token_char(c))
            .unwrap_or(true);
        let after = idx + needle.len();
        let after_ok = haystack[after..]
            .chars()
            .next()
            .map(|c| !is_token_char(c))
            .unwrap_or(true);
        if before_ok && after_ok {
            return true;
        }
        start = idx + 1;
    }
    false
}

/// Whether corpus `c` rules on registry row `row` anywhere in its own
/// text — its provenance, its freeze note, or any item's title, source,
/// anchor, note or disclosure. A citation (the row appearing inside some
/// item's `anchor` string) counts as a ruling, because citing is itself a
/// form of stating a verdict about the row; a refusal written into
/// `provenance` counts too, which is where both of this family's real
/// refusals (`MAP-18` in `asimov-1989`, `BIO-animal-domestication` and
/// `MEM-4` in `henrich-2004-extended`) actually live today.
fn corpus_mentions_row(c: &Corpus, row: &str) -> bool {
    if mentions_row(&c.provenance, row) || mentions_row(&c.frozen, row) {
        return true;
    }
    c.items.iter().any(|i| {
        mentions_row(&i.title, row)
            || mentions_row(&i.source, row)
            || mentions_row(&i.anchor, row)
            || i.note.as_deref().is_some_and(|n| mentions_row(n, row))
            || i.disclosure
                .as_deref()
                .is_some_and(|d| mentions_row(d, row))
    })
}

/// Every registry row a SIBLING corpus in this family cites that `c` does
/// not rule on anywhere in its own text (Step 4b.2; `technologies/
/// CLAUDE.md`'s "A row cited by ONE corpus must be ruled on by EVERY
/// corpus"). Sibling corpus files are discovered by listing
/// `<root>/technologies/*.technology.json` and loading each one except the
/// file whose corpus id equals `c.corpus` — reusing [`load`], never a
/// second parser.
///
/// **Deliberately separate from [`audit`]**, not folded into it, even
/// though both take the same `(c: &Corpus, root: &Path)` shape and both are
/// "Task 5's resolver enforcing family law": this check scores `c` against
/// whatever else lives under `root/technologies/`, so calling it from
/// `audit` would make every isolated single-item fixture in this crate's
/// own test suite also score against the two real committed corpora,
/// almost never what such a fixture wants. A caller that wants the full
/// picture for a real corpus calls both.
///
/// **This is a floor, not the judgement family law demands.** "Ruled on"
/// here means the row's identifier appears, at a token boundary, somewhere
/// in `c`'s own text. That is a grep, and a grep cannot tell a genuine
/// per-item ruling ("this row names item X's demand, and is refused,
/// because...") from an incidental mention — the exact shape campaign
/// ledger #21 named as satisfying a grep while ruling on nothing ("named
/// `MEM-4` only while reporting what the sibling did with it"). Passing
/// this check is necessary, not sufficient: the author still owes the
/// judgement this check cannot perform.
///
/// Silently returns no findings if `<root>/technologies/` cannot be
/// listed — `audit`'s two `unwrap_or_else` gathers fail loudly on the repo
/// facts this resolver cannot function without; a missing or unreadable
/// sibling directory is not one of those, because a single corpus audited
/// outside a full checkout (a narrow test fixture, say) has no siblings to
/// rule on by construction, not a broken repo.
pub fn cross_corpus_ruling_gaps(c: &Corpus, root: &Path) -> Vec<Finding> {
    let dir = root.join("technologies");
    let Ok(entries) = std::fs::read_dir(&dir) else {
        return Vec::new();
    };

    let mut gaps = Vec::new();
    let mut reported: BTreeSet<String> = BTreeSet::new();
    for entry in entries.flatten() {
        let path = entry.path();
        if path.extension().and_then(|e| e.to_str()) != Some("json") {
            continue;
        }
        let Some(stem) = path.file_stem().and_then(|s| s.to_str()) else {
            continue;
        };
        let corpus_id = stem.strip_suffix(".technology").unwrap_or(stem);
        if corpus_id == c.corpus {
            continue;
        }

        let sibling = load(&path);
        for item in &sibling.items {
            let Some(row) = item.anchor.strip_prefix("registry:") else {
                continue;
            };
            if reported.contains(row) || corpus_mentions_row(c, row) {
                continue;
            }
            reported.insert(row.to_string());
            gaps.push(Finding::Unjustified {
                id: row.to_string(),
                why: format!(
                    "registry:{row} is cited by {}'s item {} but is never mentioned \
                     anywhere in {} — not in its provenance, its freeze note, or any \
                     item's title, source, anchor, note or disclosure. Family law \
                     (technologies/CLAUDE.md, \"A row cited by ONE corpus must be \
                     ruled on by EVERY corpus\") requires every corpus to explicitly \
                     rule on a row any sibling cites: cite it too if it discharges a \
                     demand here, or refuse it in writing — in provenance or an \
                     item's note — naming the demand it does not discharge. This \
                     check is a grep-level floor, not the judgement itself: passing \
                     it means a mention exists, not that the ruling is sound.",
                    sibling.corpus, item.id, c.corpus
                ),
            });
        }
    }
    gaps
}

/// Every violation of the chosen/inherited **disclosure** rule in `c`, in
/// both directions (`technologies/CLAUDE.md`: "Task 4's resolver must
/// enforce the chosen rule, two-directionally: every chosen item carries a
/// `disclosure`, and no inherited item does").
///
/// **Two-directional, and the second direction is not symmetry for its own
/// sake.** A chosen item with no `disclosure` leaves that column empty in
/// the committed report, which reads as *authored blind* for a verdict that was
/// in fact reached by a session that had read the model — precisely the
/// thing `disclosure` exists to expose, inverted. An inherited item WITH a
/// `disclosure` is the over-coverage defect (ledger #13): a refusal that
/// could not have changed the verdict is evidence for a `note`, not a
/// choice in a verdict, so a disclosure there is noise that makes the real
/// ones harder to find. Family law states both halves; a one-directional
/// check would be the same half-built guard decision 0936 refuses for the
/// trajectory axis one family over.
///
/// **Deliberately a separate function from [`audit`]**, on exactly the
/// precedent [`cross_corpus_ruling_gaps`] set and for a near-identical
/// reason: almost every fixture this crate's own tests build is a one-item
/// corpus with no `presupposes` and no `disclosure`, which is CHOSEN by
/// construction, so folding this into `audit` would make every isolated
/// anchor-resolution fixture also carry a disclosure finding it has nothing
/// to do with. [`audit_family`] is the entry point that runs all of family
/// law, and it is what `hornvale technologies check` calls.
///
/// Takes no `root`: unlike its two neighbours this rule is entirely
/// intra-corpus — it reads the lattice and the verdicts and resolves
/// nothing against the repository.
pub fn disclosure_gaps(c: &Corpus) -> Vec<Finding> {
    c.items
        .iter()
        .filter_map(|item| {
            let chosen = is_chosen(c, &item.id);
            let disclosed = item
                .disclosure
                .as_deref()
                .is_some_and(|d| !d.trim().is_empty());
            match (chosen, disclosed) {
                (true, false) => Some(Finding::Disclosure {
                    id: item.id.clone(),
                    chosen: true,
                    why: format!(
                        "{} is CHOSEN — no prerequisite anywhere in its `presupposes` \
                         closure is `absent`, so nothing upstream forces its verdict \
                         `{}` — but it carries no `disclosure`. An empty `disclosure` \
                         column in the report reads as \"authored blind\", which is false for a \
                         verdict reached by searching this repository, and it is the one \
                         thing `disclosure` exists to prevent. Two legitimate repairs: \
                         state in a `disclosure` what the author knew about the model \
                         when this verdict was chosen, or — if the verdict really is \
                         forced — re-check the closure, because an `absent` prerequisite \
                         would make this item inherited and the disclosure unnecessary.",
                        item.id,
                        verdict_name(item.verdict)
                    ),
                }),
                (false, true) => Some(Finding::Disclosure {
                    id: item.id.clone(),
                    chosen: false,
                    why: format!(
                        "{} is INHERITED — at least one prerequisite in its \
                         `presupposes` closure is `absent`, so the weakest-demand rule \
                         reads its verdict `{}` off that prerequisite rather than off \
                         any Hornvale fact — yet it carries a `disclosure`. A refusal \
                         that could not have changed the verdict is evidence, not a \
                         choice, and a disclosure on a forced verdict is noise that \
                         makes the real ones harder to find (campaign ledger #13). Two \
                         legitimate repairs: move the argument into this item's `note`, \
                         where model-derived reasoning belongs, or — if the verdict was \
                         genuinely chosen — re-check the closure, because then the \
                         `absent` prerequisite this item inherits from is the thing that \
                         is wrong.",
                        item.id,
                        verdict_name(item.verdict)
                    ),
                }),
                _ => None,
            }
        })
        .collect()
}

// --- Rendering (Task 7) -------------------------------------------------
//
// `wrap`/`percent` are the same shape as `systems::wrap`/`systems::percent`
// and `regularities::wrap`/`regularities::percent` — all three are private
// to their own module, and this family is a deliberate sibling rather than a
// member (decision 0135), so the pair is reimplemented here rather than
// widened to `pub` for a sibling's convenience. Same reasoning as those two.

/// Hard-wrap a prose paragraph at 76 columns on word boundaries, so a
/// byte-ratcheted artifact keeps a single-word edit to a single-line diff.
/// type-audit: bare-ok(prose: text), bare-ok(prose: return)
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
/// purpose — decision 0033 keeps floats away from serialization boundaries,
/// and this figure lands in a byte-ratcheted artifact.
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
    format!("docs/audits/technology-coverage-{}.md", corpus.corpus)
}

/// The command that regenerates a report, for the header. Takes the corpus
/// **id** the caller actually typed (`asimov-1989`), not a path — this
/// family's CLI surface resolves a corpus by id (`hornvale technologies
/// report <id>`), unlike the sibling families' `--corpus <path>`, so the
/// printed command must match that syntax or a reader copying it would get
/// "unknown mode" from `--corpus`.
/// type-audit: bare-ok(identifier-text: corpus_id), bare-ok(identifier-text: return)
pub fn regenerate_command(corpus_id: &str) -> String {
    format!("hornvale technologies report {corpus_id}")
}

/// Coverage-verdict counts across `corpus` — the eight verdicts a completed
/// scoring assigns, decision 0095's five plus this family's three measured
/// additions (`grown`/`flat`/`lost`).
///
/// **`Unmeasured` is deliberately excluded from this array.** It is a
/// lifecycle state (reach passed, trajectory not yet scored), never a
/// coverage judgement (`technologies/CLAUDE.md`'s verdict table), and
/// [`render`] must not fold it into the same percentage denominator these
/// eight verdicts share — see [`unmeasured_count`] and [`coverage_total`],
/// which is why this function returns eight entries, not nine.
fn coverage_tally(corpus: &Corpus) -> [(Verdict, usize); 8] {
    let count = |v: Verdict| corpus.items.iter().filter(|i| i.verdict == v).count();
    [
        (Verdict::Present, count(Verdict::Present)),
        (Verdict::Refused, count(Verdict::Refused)),
        (Verdict::Deferred, count(Verdict::Deferred)),
        (Verdict::Absent, count(Verdict::Absent)),
        (Verdict::Inapplicable, count(Verdict::Inapplicable)),
        (Verdict::Grown, count(Verdict::Grown)),
        (Verdict::Flat, count(Verdict::Flat)),
        (Verdict::Lost, count(Verdict::Lost)),
    ]
}

/// How many items in `corpus` are `unmeasured` right now.
fn unmeasured_count(corpus: &Corpus) -> usize {
    corpus
        .items
        .iter()
        .filter(|i| i.verdict == Verdict::Unmeasured)
        .count()
}

/// The denominator [`coverage_tally`]'s percentages are read against: every
/// item that carries a genuine coverage verdict, excluding the `unmeasured`
/// lifecycle state. An `unmeasured` item has not yet been judged, so
/// counting it in this denominator would let a coverage percentage move
/// simply because a trajectory got scored — the same "count moves for a
/// reason unrelated to what it claims to measure" defect a null needs its
/// own denominator to avoid.
fn coverage_total(corpus: &Corpus) -> usize {
    corpus.items.len() - unmeasured_count(corpus)
}

/// The counts that make this family's per-corpus finding sayable (Task 7's
/// brief), one field per verdict that has PASSED REACH under the ratified
/// pipeline (campaign ledger #6 and #16), plus [`Reach::total`] over them.
///
/// For `asimov-1989`, the total is the individual-invention column's own
/// answer (0 today, since that corpus's items are `absent`/`deferred`); for
/// `henrich-2004-extended` it is the number of documented losses Hornvale's
/// history model can represent at all, and `lost` — 0 today — is how many of
/// those it can show were actually given up, which decision 0936's sibling
/// scope note explains: `tech_for`
/// (`windows/worldgen/src/history_bake.rs`) is monotone in `year`, so no
/// world can yet exhibit a measured loss.
/// type-audit: bare-ok(count: present), bare-ok(count: unmeasured), bare-ok(count: grown), bare-ok(count: flat), bare-ok(count: lost)
struct Reach {
    /// Reach passed, mechanism cited, trajectory not the question.
    present: usize,
    /// Reach passed, trajectory not yet scored.
    unmeasured: usize,
    /// Measured: acquired and kept.
    grown: usize,
    /// Measured: never acquired.
    flat: usize,
    /// Measured: acquired, then given up.
    lost: usize,
}

impl Reach {
    /// How many items Hornvale's mechanism reaches AT ALL.
    ///
    /// **Every verdict above is in this sum, not only `present` and
    /// `unmeasured`.** The ratified vocabulary is an implicit PIPELINE
    /// (ledger #6): a measured value is only reachable if reach already
    /// succeeded, so `grown`, `flat` and `lost` have each passed reach by
    /// definition. Until the pre-merge fix wave this function's predecessor
    /// summed only the first two, which is correct for today's data (both
    /// corpora score every measured verdict at 0) and **arithmetically
    /// impossible the moment a successor campaign scores one item `lost`**:
    /// the report would then read "reaches 0 of them at all … and of those
    /// 0, it can currently represent the LOSS of exactly 1." A definition
    /// that only holds while a count is zero is not a definition; the
    /// invariant it must satisfy is `lost <= total()`, which
    /// `technology_coverage::the_loss_count_never_exceeds_the_reach_count`
    /// holds against a constructed corpus, since no committed one can
    /// exercise it (ledger #27).
    /// type-audit: bare-ok(count: return)
    fn total(&self) -> usize {
        self.present + self.unmeasured + self.grown + self.flat + self.lost
    }
}

/// Tally [`Reach`] over `corpus`.
fn reach_and_loss(corpus: &Corpus) -> Reach {
    let count = |v: Verdict| corpus.items.iter().filter(|i| i.verdict == v).count();
    Reach {
        present: count(Verdict::Present),
        unmeasured: count(Verdict::Unmeasured),
        grown: count(Verdict::Grown),
        flat: count(Verdict::Flat),
        lost: count(Verdict::Lost),
    }
}

/// The derived demand set (decision 0386) for every item this corpus has not
/// yet built — `absent` or `deferred` — as the report's one actionable
/// output. **Never `refused` or `inapplicable`**: those are decided
/// non-goals, not open demands, so listing them here would misstate a
/// deliberate refusal as a backlog entry.
///
/// This is the family's answer to `technologies/CLAUDE.md`'s "the corpus is
/// an instrument, never a roadmap" and the Repertoire's Critical finding
/// Task 7's brief cites: printing the demand SET (what an item requires,
/// derived by closure) rather than a table titled with a work-queue word is
/// what keeps a measurement from reading as a plan.
fn open_demands(corpus: &Corpus) -> Vec<(&Item, BTreeSet<String>)> {
    corpus
        .items
        .iter()
        .filter(|i| matches!(i.verdict, Verdict::Absent | Verdict::Deferred))
        .map(|i| (i, derived_demands(corpus, &i.id)))
        .collect()
}

/// Render the coverage report.
///
/// Order follows decision 0095 (provenance, the declared bias and the
/// selection rule before any tally) with this family's own additions ahead
/// of the tally, exactly where `technologies/CLAUDE.md` and Task 7's brief
/// place them: the `present`/`unmeasured` weak-anchor caveat (0136's
/// consequence clause) prints above the numbers it qualifies, never in a
/// footnote below them.
///
/// `corpus_id` is the id the caller actually resolved (`asimov-1989`), so
/// the regenerate command in the banner names a real invocation rather than
/// a stem derived from `corpus.corpus` that might disagree with it.
/// type-audit: bare-ok(identifier-text: corpus_id), bare-ok(prose: return)
pub fn render(corpus: &Corpus, corpus_id: &str) -> String {
    let mut s = String::new();
    s.push_str(&format!(
        "<!-- GENERATED FILE — do not edit. Regenerate with `{}`. -->\n\n",
        regenerate_command(corpus_id)
    ));
    s.push_str("# Technology coverage\n\n## Provenance\n\n");
    s.push_str(&format!("- **Corpus:** `{}`\n", corpus.corpus));
    s.push_str(&format!("- **Source:** {}\n", wrap(&corpus.provenance)));
    s.push_str(&format!("- **Frozen:** {}\n", wrap(&corpus.frozen)));

    s.push_str("\n## Reading this report\n\n");
    s.push_str(&wrap(
        "This measures whether a PEOPLE acquires, holds, and loses one imported \
         technology-capability catalogue, resolved against the per-people trajectory a \
         census would report — an instrument with known bias (decision 0095), never a \
         standard and never a verdict on the world. `present` and `unmeasured` are both \
         only WEAKLY checked: a mechanism anchor that resolves (a real `test:`/`path:`) is \
         not proof the capability is met, only that something at that location exists. \
         `unmeasured` additionally means the trajectory itself has not been scored at all — \
         reach passed, nothing about growth, flatness or loss has been measured yet — so an \
         `unmeasured` count is not a weaker `present`, it is a different kind of claim. Both \
         are printed here, above the tally they most affect, per 0136's consequence clause: \
         a reader must pass this sentence before reaching a score.",
    ));
    s.push_str("\n\n");

    let counts = coverage_tally(corpus);
    let total = coverage_total(corpus);
    s.push_str("## Tally\n\n");
    s.push_str(&wrap(&format!(
        "The eight coverage verdicts below are percentages of {total} — every item MINUS \
         the ones still `unmeasured` (see the next section). An `unmeasured` item has not \
         been judged, so counting it here would move a coverage percentage for a reason \
         unrelated to what that percentage claims to measure.",
    )));
    s.push_str("\n\n");
    for (v, n) in counts {
        s.push_str(&format!(
            "- {}: {n} ({}%)\n",
            verdict_name(v),
            percent(n, total)
        ));
    }
    s.push_str(&format!("- **coverage total:** {total}\n"));

    let unmeasured = unmeasured_count(corpus);
    let r = reach_and_loss(corpus);
    let reach = r.total();
    let lost = r.lost;
    let present = r.present;
    s.push_str("\n## Unmeasured\n\n");
    if unmeasured == 0 {
        s.push_str("None — every item carries a coverage verdict.\n\n");
    } else {
        s.push_str(&wrap(&format!(
            "{unmeasured} item(s), reported separately from the tally above and never folded \
             into a percentage. Each has PASSED reach — Hornvale's mechanism for the \
             capability exists and is cited by a `test:`/`path:` anchor exactly as a \
             `present` verdict's would be — but its TRAJECTORY (does a people grow it, hold \
             it flat, or lose it) is not yet scored. The reason is structural, not a \
             backlog item: `tech_for` (`windows/worldgen/src/history_bake.rs`) is documented \
             monotone in `year`, so no world this campaign can build yet exhibits a measured \
             loss, and the family's live measurement wiring (comparing an authored \
             trajectory against a fresh `meets()` computation over the census, mirroring \
             `regularities::two_way`) is a successor campaign's work.",
        )));
        s.push_str("\n\n");
        s.push_str("| id | title | anchor |\n|---|---|---|\n");
        for item in corpus
            .items
            .iter()
            .filter(|i| i.verdict == Verdict::Unmeasured)
        {
            s.push_str(&format!(
                "| {} | {} | {} |\n",
                item.id,
                item.title.replace('|', "\\|"),
                item.anchor.replace('|', "\\|")
            ));
        }
        s.push('\n');
    }
    // The two-count finding this family's report exists to make sayable
    // (Task 7's brief): a single tally cannot say "Hornvale can represent K
    // of these at all, and of those K it can show none was ever lost" — that
    // needs both `reach` and `lost` read together. Printed unconditionally,
    // not only when `unmeasured > 0`: a corpus that reaches items through
    // `present` rather than `unmeasured` makes the identical claim, and a
    // `reach` of 0 (true of `asimov-1989` today) is itself part of the
    // finding, not a case to suppress.
    //
    // THE BREAKDOWN NAMES EVERY COMPONENT OF `reach` THAT IS NON-ZERO, so
    // the parenthetical always sums to the number in front of it. The three
    // measured verdicts are appended only when at least one of them is
    // non-zero — which is never, for either committed corpus, so this
    // renders byte-identically to the pre-fix-wave text today and the
    // committed reports do not drift. `present` and `unmeasured` are printed
    // unconditionally even at 0, because a 0 there is part of the finding
    // (see above); a measured verdict at 0 is not, it is the ordinary state
    // of an axis no world can exhibit yet.
    let measured_breakdown = if r.grown + r.flat + r.lost == 0 {
        String::new()
    } else {
        format!(
            ", {} `grown`, {} `flat`, {} `lost`",
            r.grown, r.flat, r.lost
        )
    };
    s.push_str(&wrap(&format!(
        "THE FINDING THIS CORPUS MAKES SAYABLE: of the {} item(s) here, Hornvale's mechanism \
         reaches {reach} of them at all ({present} `present`, {unmeasured} \
         `unmeasured`{measured_breakdown}) — \
         and of those {reach}, it can currently represent the LOSS of exactly {lost}. A \
         single tally has no way to say this; it takes both counts together.",
        corpus.items.len(),
    )));
    s.push('\n');

    // The actionable output (Task 7's brief, `technologies/CLAUDE.md`'s "the
    // corpus is an instrument, never a roadmap"): the derived demand set for
    // every `absent`/`deferred` item, never headed with a work-queue word.
    let open = open_demands(corpus);
    s.push_str("\n## Demand set\n\n");
    s.push_str(&wrap(
        "Not a backlog: this is what each item's own `presupposes` closure (decision 0386) \
         names, derived on read and never authored by hand. An item's demand set always \
         includes its own `introduces` token, so a root's set has one entry and a deep \
         item's may have several. `refused` and `inapplicable` items are excluded — those \
         are decided non-goals, not open demands.",
    ));
    s.push_str("\n\n");
    if open.is_empty() {
        s.push_str("None — no item scores `absent` or `deferred`.\n");
    } else {
        s.push_str("| id | title | verdict | demand set |\n|---|---|---|---|\n");
        for (item, demands) in &open {
            let joined = demands
                .iter()
                .cloned()
                .collect::<Vec<_>>()
                .join(", ")
                .replace('|', "\\|");
            s.push_str(&format!(
                "| {} | {} | {} | {} |\n",
                item.id,
                item.title.replace('|', "\\|"),
                verdict_name(item.verdict),
                joined
            ));
        }
    }

    s.push_str(
        "\n## Items\n\n| id | title | verdict | anchor | contested | disclosure | note |\n\
         |---|---|---|---|---|---|---|\n",
    );
    for item in &corpus.items {
        s.push_str(&format!(
            "| {} | {} | {} | {} | {} | {} | {} |\n",
            item.id,
            item.title.replace('|', "\\|"),
            verdict_name(item.verdict),
            item.anchor.replace('|', "\\|"),
            match item.contested {
                Some(true) => "yes",
                Some(false) | None => "",
            },
            item.disclosure.as_deref().unwrap_or("").replace('|', "\\|"),
            item.note.as_deref().unwrap_or("").replace('|', "\\|")
        ));
    }
    s
}

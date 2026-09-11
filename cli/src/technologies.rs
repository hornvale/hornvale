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
//! (`technologies/CLAUDE.md`'s cross-corpus rule). Criterion evaluation and
//! the two-way trajectory guard are later tasks' work and deliberately do
//! not live here yet: [`Criterion`] is a minimal round-tripping type today,
//! not something this module evaluates.
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
/// corpus as a whole — decision 0136's four conditions. Every variant
/// carries enough to diagnose it without knowing this instrument exists
/// (the same diagnosability standard `systems::Finding` and
/// `regularities::Finding` carry): this family's two inputs are edited by
/// sessions with no reason to know a resolver reads them.
/// type-audit: bare-ok(identifier-text: Unjustified.id), bare-ok(prose: Unjustified.why), bare-ok(identifier-text: Dangling.id), bare-ok(identifier-text: Dangling.anchor), bare-ok(prose: Dangling.why), bare-ok(identifier-text: StaleDeferred.id), bare-ok(identifier-text: StaleDeferred.row), bare-ok(prose: StaleDeferred.why), bare-ok(identifier-text: Novelty.corpus), bare-ok(count: Novelty.baseline), bare-ok(count: Novelty.found)
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
/// committed files rather than guessed — `asimov-1989` at 35, `henrich-
/// 2004-extended` at 31. `None` for a corpus with no baseline on record,
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
        "asimov-1989" => Some(35),
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
                         built\"; that stopped being true. Two legitimate repairs: \
                         re-verdict this item to `present` (if Hornvale now has the \
                         capability, citing the shipping mechanism) or to `absent`/\
                         `refused` (if the settled row does not actually discharge this \
                         item's demand), or treat the status change itself as the thing \
                         to undo if it was a mistake.",
                        item.id
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
            if generated.has_generator(path) {
                return None;
            }
            // The two ways `has_generator` says no want opposite repairs —
            // mirroring `regularities::resolve_anchor`'s `Anchor::Doc` arm,
            // which this reuses rather than re-deriving. Unlike that
            // sibling, this family has no claim-marker page to check a
            // second half against yet (the Domesday renders no
            // technology-trajectory claim today); that half is later
            // tasks' work, not a gap silently dropped here.
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

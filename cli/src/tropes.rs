//! The Repertoire: score a frozen corpus of dramatic situations against the
//! concept registry. Build-state only — no seed, no world save, no census.
//!
//! Wired into the CLI as `hornvale tropes report|check|matrix`; `cmd_tropes`
//! in `main.rs` builds a real world and calls `load`/`resolve`/`render` on
//! it, and Task 4 pins `render`'s output as a ratchet. `matrix` runs the same
//! resolution over every corpus in `CORPORA` and renders the comparison ADR
//! 0095 deferred until a second catalogue existed — one column says what this
//! world supplies, and only the matrix can say what the catalogues ask for.

use hornvale_kernel::ConceptRegistry;
use serde::Deserialize;
use std::cmp::Ordering;
use std::collections::{BTreeMap, BTreeSet};

/// One situation as authored in the corpus.
/// type-audit: bare-ok(identifier-text: id), bare-ok(prose: name), bare-ok(prose: actants), bare-ok(identifier-text: requires), bare-ok(prose: excluded_by)
#[derive(Debug, Deserialize)]
pub struct Situation {
    /// Stable corpus-local identifier.
    pub id: String,
    /// Human-readable situation name.
    pub name: String,
    /// Greimas actant role → the role's description in this situation.
    pub actants: BTreeMap<String, String>,
    /// Namespaced tokens and `bundle:` references this situation needs.
    pub requires: Vec<String>,
    /// Preconditions whose absence makes this situation inapplicable.
    pub excluded_by: Vec<String>,
}

/// A frozen, provenance-stamped corpus.
/// type-audit: bare-ok(identifier-text: corpus), bare-ok(prose: provenance), bare-ok(prose: frozen), bare-ok(identifier-text: bundles)
#[derive(Debug, Deserialize)]
pub struct Corpus {
    /// Corpus identifier, e.g. `polti-1895`.
    pub corpus: String,
    /// Where this catalogue comes from and what bias it carries.
    pub provenance: String,
    /// Note recording that the freeze preceded measurement.
    pub frozen: String,
    /// Bundle name → the tokens it expands to.
    pub bundles: BTreeMap<String, Vec<String>>,
    /// The situations themselves.
    pub situations: Vec<Situation>,
}

/// How one situation resolved.
/// type-audit: bare-ok(identifier-text: Blocked.0), bare-ok(prose: Inapplicable.0)
#[derive(Debug, PartialEq, Eq)]
pub enum Outcome {
    /// Every requirement satisfied.
    Stageable,
    /// One or more tokens absent, listed in corpus order.
    Blocked(Vec<String>),
    /// The world deliberately lacks a precondition; different, not deficient.
    Inapplicable(String),
}

/// Parse a corpus from JSON.
/// type-audit: bare-ok(artifact: json), bare-ok(prose: return)
pub fn load(json: &str) -> Result<Corpus, String> {
    serde_json::from_str(json).map_err(|e| format!("corpus parse: {e}"))
}

/// Every token the registry holds, namespaced.
fn registry_tokens(r: &ConceptRegistry) -> BTreeSet<String> {
    let mut t = BTreeSet::new();
    for p in r.predicates() {
        t.insert(format!("predicate:{}", p.name));
    }
    for (kind, _doc) in r.phenomenon_kinds() {
        t.insert(format!("phenomenon:{kind}"));
    }
    for c in r.concepts() {
        t.insert(format!("concept:{}", c.name));
    }
    t
}

/// Expand a requirement into concrete tokens, following one `bundle:` level.
///
/// A dangling `bundle:` reference expands to the reference itself, which no
/// registry token can ever match, so a typo blocks its situation. Returning
/// an empty list instead would make a situation whose requirements are all
/// dangling resolve `Stageable` — the exact inversion of spec D4's
/// default-deny posture.
fn expand(corpus: &Corpus, req: &str) -> Vec<String> {
    match req.strip_prefix("bundle:") {
        Some(b) => match corpus.bundles.get(b) {
            Some(tokens) => tokens.clone(),
            None => vec![req.to_string()],
        },
        None => vec![req.to_string()],
    }
}

/// Resolve every situation against a registry. Keyed by situation `id`.
/// type-audit: bare-ok(identifier-text: return)
pub fn resolve(corpus: &Corpus, registry: &ConceptRegistry) -> BTreeMap<String, Outcome> {
    let held = registry_tokens(registry);
    let mut out = BTreeMap::new();
    for s in &corpus.situations {
        if let Some(reason) = s.excluded_by.first() {
            out.insert(s.id.clone(), Outcome::Inapplicable(reason.clone()));
            continue;
        }
        // Deduplicated, in corpus order: two bundles may name the same
        // token, and `Blocked` is rendered verbatim into the committed
        // artifact, where a repeat would misstate the count.
        let mut missing: Vec<String> = Vec::new();
        for t in s.requires.iter().flat_map(|r| expand(corpus, r)) {
            if !held.contains(&t) && !missing.contains(&t) {
                missing.push(t);
            }
        }
        out.insert(
            s.id.clone(),
            if missing.is_empty() {
                Outcome::Stageable
            } else {
                Outcome::Blocked(missing)
            },
        );
    }
    out
}

/// Hard-wrap a prose paragraph at 76 columns on word boundaries.
///
/// The report is a byte-ratcheted artifact. An unwrapped paragraph makes every
/// future word change a whole-line diff in the review that has to approve it,
/// so prose is wrapped and tables are not.
///
/// A word starting with `-` is never moved to the start of a line, even when
/// that overruns 76 columns. Markdown reads a leading `-` as a list marker, so
/// a break landing one there turns prose into a spurious bullet — and in
/// `render_matrix` the wrapped text is *already* a bullet, where the result is
/// a nested one. Neither frozen corpus's provenance contains such a word, so
/// this costs nothing today and closes the hazard for the third catalogue.
/// The guard sits under `col > 0`, so a paragraph deliberately opening with a
/// bullet marker still gets one.
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

/// One corpus's headline counts, `(stageable, inapplicable)`.
///
/// Two documents state these same three figures — [`render`] as `Stageable s
/// of t (i inapplicable).` and [`render_matrix`] as that corpus's row — and
/// each used to filter `out.values()` for itself. Changing one filter and
/// running `make rebaseline` re-accepts both goldens together, so the two
/// documents would come apart quietly with nothing red. One function makes
/// that divergence unrepresentable; the integration test that parses both
/// rendered figures is the second half of the same guard.
/// type-audit: bare-ok(identifier-text: out), bare-ok(prose: return)
fn tally(out: &BTreeMap<String, Outcome>) -> (usize, usize) {
    let stageable = out.values().filter(|o| **o == Outcome::Stageable).count();
    let inapplicable = out
        .values()
        .filter(|o| matches!(o, Outcome::Inapplicable(_)))
        .count();
    (stageable, inapplicable)
}

/// Where a corpus's committed report lives.
///
/// Derived from the corpus's own identifier rather than passed alongside it,
/// so a caller cannot pair the wrong corpus with the wrong artifact — which
/// is exactly what the previous hardcoded path did for every corpus except
/// `polti-1895`, silently and always as a failure.
/// type-audit: bare-ok(identifier-text: return)
pub fn artifact_path(corpus: &Corpus) -> String {
    format!("docs/audits/trope-coverage-{}.md", corpus.corpus)
}

/// Every corpus this repository scores against, in render order.
///
/// A constant rather than a directory scan: which corpora are columns is a
/// deliberate act under ADR 0095, and a scan would silently add one.
/// type-audit: bare-ok(artifact)
pub const CORPORA: [&str; 2] = ["tropes/polti.trope.json", "tropes/tvtropes-2012.trope.json"];

/// The command that regenerates a report, for the header.
///
/// Takes the path the caller actually used rather than deriving one from the
/// corpus id: `polti-1895` lives in `tropes/polti.trope.json`, so a derived
/// stem would print a regenerate command naming a file that does not exist.
/// type-audit: bare-ok(identifier-text: path), bare-ok(identifier-text: return)
pub fn regenerate_command(path: &str) -> String {
    format!("hornvale tropes --corpus {path} report")
}

/// Render the coverage report. Four sections, provenance first (spec §4 L2).
/// type-audit: bare-ok(identifier-text: out), bare-ok(prose: return), bare-ok(identifier-text: path)
pub fn render(
    corpus: &Corpus,
    out: &BTreeMap<String, Outcome>,
    registry: &ConceptRegistry,
    path: &str,
) -> String {
    let mut s = String::new();
    s.push_str(&format!(
        "<!-- GENERATED FILE — do not edit. Regenerate with `{}`. -->\n\n",
        regenerate_command(path)
    ));
    s.push_str("# Trope coverage\n\n## Provenance\n\n");
    s.push_str(&format!("- **Corpus:** `{}`\n", corpus.corpus));
    s.push_str(&format!("- **Source:** {}\n", corpus.provenance));
    s.push_str(&format!("- **Frozen:** {}\n", corpus.frozen));
    s.push_str(
        "\nThis measures reach against *that* catalogue. It is not a verdict on the\nworld, and it scores **representability only** — whether an agent could plan\nor recognise a situation is not measured here.\n\nA low score is the expected reading at this stage: the report is a baseline\ntaken before the machinery it measures exists. What carries information is\nmovement between runs, not the absolute number.\n\n",
    );

    let (stageable, inapplicable) = tally(out);
    s.push_str("## Demand\n\n");
    s.push_str(&format!(
        "Stageable {stageable} of {} ({inapplicable} inapplicable).\n\n| Situation | Actants | Outcome |\n|---|---|---|\n",
        out.len()
    ));
    let names: BTreeMap<&str, &str> = corpus
        .situations
        .iter()
        .map(|x| (x.id.as_str(), x.name.as_str()))
        .collect();
    let roles: BTreeMap<&str, String> = corpus
        .situations
        .iter()
        .map(|x| {
            (
                x.id.as_str(),
                x.actants.keys().cloned().collect::<Vec<_>>().join(", "),
            )
        })
        .collect();
    for (id, o) in out {
        let cell = match o {
            Outcome::Stageable => "stageable".to_string(),
            Outcome::Inapplicable(r) => format!("inapplicable — {r}"),
            Outcome::Blocked(m) => format!("blocked — missing `{}`", m.join("`, `")),
        };
        s.push_str(&format!(
            "| {} ({id}) | {} | {cell} |\n",
            names.get(id.as_str()).copied().unwrap_or("?"),
            roles.get(id.as_str()).map(String::as_str).unwrap_or("")
        ));
    }

    // Rank only bundles that are actually MISSING. A blocked situation also
    // requires bundles the world already holds; counting those put seven
    // satisfied bundles in a table headed "missing", and the backlog
    // ordering IS the deliverable here (spec D3, P1).
    let held = registry_tokens(registry);
    let mut fan: BTreeMap<String, Vec<String>> = BTreeMap::new();
    for st in &corpus.situations {
        if let Some(Outcome::Blocked(_)) = out.get(&st.id) {
            for r in st.requires.iter().filter(|r| r.starts_with("bundle:")) {
                if expand(corpus, r).iter().any(|t| !held.contains(t)) {
                    fan.entry(r.clone()).or_default().push(st.id.clone());
                }
            }
        }
    }
    let mut ranked: Vec<_> = fan.into_iter().collect();
    ranked.sort_by(|a, b| b.1.len().cmp(&a.1.len()).then(a.0.cmp(&b.0)));

    // How close is the closest blocked situation? If this is ever 1, a single
    // bundle really would unlock something and the caveat below should change.
    let closest = corpus
        .situations
        .iter()
        .filter(|st| matches!(out.get(&st.id), Some(Outcome::Blocked(_))))
        .map(|st| {
            st.requires
                .iter()
                .filter(|r| r.starts_with("bundle:"))
                .filter(|r| expand(corpus, r).iter().any(|t| !held.contains(t)))
                .count()
        })
        .min()
        .unwrap_or(0);
    let blocked = out
        .values()
        .filter(|o| matches!(o, Outcome::Blocked(_)))
        .count();
    let inapplicable_noun = if inapplicable == 1 {
        "situation is"
    } else {
        "situations are"
    };
    s.push_str("\n## Leverage\n\n");
    s.push_str(&wrap(&format!(
        "Missing bundles ranked by fan-in over the {blocked} **blocked** situations. The \
         {inapplicable} inapplicable {inapplicable_noun} excluded from this ranking, but not \
         from the report: the Supply section below still counts its requirements as demand, \
         which keeps those tokens off the orphan list. The **corpus** column counts all {} \
         situations.",
        out.len()
    )));
    s.push_str("\n\n");
    s.push_str(&wrap(&format!(
        "Fan-in is **not** an unlock count: the closest blocked situation is still missing \
         {closest} bundles, so no single row makes anything stageable on its own."
    )));
    // A bundle required ONLY by inapplicable situations never enters the fan
    // map above, so a genuinely missing capability can vanish from the
    // ranking. This is the mirror image of the defect that put seven
    // SATISFIED bundles under a heading reading "missing": the preamble
    // licenses the mechanism (inapplicable situations are excluded) but
    // nothing lets a reader get from the row count to the true total. The
    // ranked misses are this report's deliverable, so disclose it.
    let shown: BTreeSet<&str> = ranked
        .iter()
        .map(|(b, _)| b.trim_start_matches("bundle:"))
        .collect();
    let hidden: Vec<&String> = corpus
        .bundles
        .iter()
        .filter(|(_, toks)| toks.iter().any(|t| !held.contains(t)))
        .map(|(b, _)| b)
        .filter(|b| !shown.contains(b.as_str()))
        .collect();
    if !hidden.is_empty() {
        // Total is ranked + hidden rather than an independent count over
        // `corpus.bundles`, so the two numbers in the sentence always
        // reconcile to the length of the list beside them. A dangling
        // `bundle:` reference ranks (default-deny) without being a corpus
        // key, and an independent count would silently break that arithmetic.
        let total_missing = ranked.len() + hidden.len();
        let (lead, verb) = if hidden.len() == 1 {
            ("1 missing bundle".to_string(), "is")
        } else {
            (format!("{} missing bundles", hidden.len()), "are")
        };
        let list = hidden
            .iter()
            .map(|b| format!("`bundle:{b}`"))
            .collect::<Vec<_>>()
            .join(", ");
        s.push_str("\n\n");
        s.push_str(&wrap(&format!(
            "**{lead} {verb} not ranked below.** {list} — required only by situations that \
             resolve inapplicable, so they contribute no fan-in and no row. The corpus holds \
             {total_missing} missing bundles against the {} ranked here; that is the difference.",
            ranked.len()
        )));
    }
    s.push_str("\n\n| Bundle | Fan-in (blocked) | Corpus | Situations |\n|---|---|---|---|\n");
    for (bundle, sits) in &ranked {
        let corpus_wide = corpus
            .situations
            .iter()
            .filter(|st| st.requires.contains(bundle))
            .count();
        s.push_str(&format!(
            "| `{bundle}` | {} | {corpus_wide} | {} |\n",
            sits.len(),
            sits.join(", ")
        ));
    }

    let required: BTreeSet<String> = corpus
        .situations
        .iter()
        .flat_map(|st| st.requires.iter().flat_map(|r| expand(corpus, r)))
        .collect();
    let orphans: Vec<String> = held
        .iter()
        .filter(|t| !required.contains(*t))
        .cloned()
        .collect();
    s.push_str(&format!(
        "\n## Supply\n\n{} registered tokens no situation in this corpus requires.\n\n",
        orphans.len()
    ));
    s.push_str(&wrap(
        "**Demand-side only.** Spec §4 L2.4 asks for tokens no situation requires *and no \
         readout consumes*; the second half is not implemented. So this list includes \
         tokens that readouts do consume — `predicate:is-a` carries the Book, and the \
         `moon-*` family carries the almanac. Read it as *unrequired by this catalogue*, \
         not *unused*. Spec D5's Goodhart guard — a rising demand score beside a rising \
         count of genuinely unconsumed tokens — needs the missing half before this list \
         can serve it.",
    ));
    s.push_str("\n\n");
    // Annotate `concept:` orphans with their owning domain. Many come from
    // the language lexicon and are WORDS, not modelled capabilities; an
    // unannotated list invites reading every orphan as a registered-but-
    // unused mechanism, which is the opposite of what the Supply count is
    // for (spec D5).
    let domains: BTreeMap<String, String> = registry
        .concepts()
        .map(|c| (format!("concept:{}", c.name), c.domain.clone()))
        .collect();
    for t in &orphans {
        match domains.get(t) {
            Some(d) => s.push_str(&format!("- `{t}` ({d})\n")),
            None => s.push_str(&format!("- `{t}`\n")),
        }
    }
    s
}

/// Every bundle `corpus` requires, mapped to the number of **situations**
/// requiring it — the numerator of that bundle's share.
///
/// The single definition of demand for the whole matrix. Both the demand
/// table and the per-catalogue ranking read this one map, because they are
/// two views of one quantity: computing them separately let the same bundle
/// print at two different percentages in the same document the moment a
/// corpus named a bundle twice in one situation, which `load` does not
/// reject. Neither frozen corpus does that today, so the bug would have
/// arrived silently with a third catalogue.
///
/// Situations, not occurrences: `requires` is a list, so a situation naming
/// `bundle:x` twice is still one situation demanding `x`, and the denominator
/// it is divided by counts situations.
///
/// Counts the `bundle:` reference as authored, not its expansion: the unit of
/// demand here is the bundle a catalogue reached for, and expanding first
/// would silently merge two bundles that happen to share a token.
fn bundle_demand(corpus: &Corpus) -> BTreeMap<String, usize> {
    let mut demand: BTreeMap<String, usize> = BTreeMap::new();
    for st in &corpus.situations {
        let mut seen: BTreeSet<&str> = BTreeSet::new();
        for r in &st.requires {
            if let Some(b) = r.strip_prefix("bundle:")
                && seen.insert(b)
            {
                *demand.entry(b.to_string()).or_default() += 1;
            }
        }
    }
    demand
}

/// `n` of `total` as a whole percent, rounded half up.
///
/// Integer arithmetic on purpose. This figure is rendered into a
/// byte-ratcheted artifact, and decision 0033 keeps floats away from
/// serialization boundaries; there is no reason to spend a float here when
/// the inputs are two counts.
fn percent(n: usize, total: usize) -> usize {
    if total == 0 {
        0
    } else {
        (n * 200 + total) / (total * 2)
    }
}

/// One catalogue's demand for one bundle, kept as counts beside the rendered
/// percent so the document shows the reader the division it performed.
struct Share {
    /// Situations in that catalogue requiring the bundle.
    required: usize,
    /// Situations in that catalogue, full stop — the denominator.
    total: usize,
    /// `required / total` as a whole percent, rounded half up.
    percent: usize,
}

impl Share {
    /// Compare two shares at full precision, without rounding and without a
    /// float: `a/A` against `b/B` is `a*B` against `b*A`.
    fn exact_cmp(&self, other: &Self) -> Ordering {
        (self.required as u128 * other.total.max(1) as u128)
            .cmp(&(other.required as u128 * self.total.max(1) as u128))
    }
}

/// The unrounded distance between a bundle's highest and lowest share, kept
/// as an exact fraction.
///
/// The rendered **Gap** subtracts two already-rounded percents, so a
/// displayed gap of N covers true spreads anywhere in `(N-1, N+1)` and cannot
/// order the bundles inside one tier — alphabetical order there would reverse
/// the extremes relative to "descending delta". This is the tiebreak that
/// makes the ordering mean what the document says it means, and a reader can
/// recompute it from the counts printed in each cell.
struct Spread {
    /// Numerator of `high - low`.
    num: u128,
    /// Common denominator of `high - low`, never zero.
    den: u128,
}

impl Spread {
    /// The spread across one row's cells. Zero for an empty row.
    fn of(cells: &[Share]) -> Self {
        let hi = cells.iter().max_by(|x, y| x.exact_cmp(y));
        let lo = cells.iter().min_by(|x, y| x.exact_cmp(y));
        match (hi, lo) {
            (Some(h), Some(l)) => {
                let (ht, lt) = (h.total.max(1) as u128, l.total.max(1) as u128);
                Self {
                    num: h.required as u128 * lt - l.required as u128 * ht,
                    den: ht * lt,
                }
            }
            _ => Self { num: 0, den: 1 },
        }
    }

    /// Cross-multiplied comparison, exact and float-free.
    fn exact_cmp(&self, other: &Self) -> Ordering {
        (self.num * other.den).cmp(&(other.num * self.den))
    }
}

/// One row of the demand table.
struct DemandRow {
    /// Highest share minus lowest, in percentage points, as rendered — the
    /// primary sort key, so the printed column stays monotone and a reader
    /// can check the ordering against what the table shows.
    gap: usize,
    /// The same distance unrounded, breaking ties inside a `gap` tier.
    spread: Spread,
    /// The bundle, without its `bundle:` prefix.
    bundle: String,
    /// Whether any catalogue in this matrix *declares* the bundle. A row that
    /// no catalogue declares is a dangling reference: it expands to itself,
    /// matches no registry token, and blocks its situation by construction.
    declared: bool,
    /// One share per column, in the caller's column order.
    cells: Vec<Share>,
}

/// The demand table's order: descending rendered gap, then descending
/// unrounded spread, then bundle name.
///
/// The rendered gap leads so the printed **Gap** column stays monotone and a
/// reader can check the ordering against what the table shows. But a rendered
/// gap of N covers true spreads across a two-point window, so it tiers rather
/// than orders — and inside a tier, falling straight through to the name
/// reversed the extremes relative to "descending delta". The spread breaks
/// those ties at full precision, from the same counts the cells print, so the
/// ordering is what the document says it is and is still re-derivable. The
/// name is last, for rows that are genuinely equal.
fn demand_order(a: &DemandRow, b: &DemandRow) -> Ordering {
    b.gap
        .cmp(&a.gap)
        .then_with(|| b.spread.exact_cmp(&a.spread))
        .then_with(|| a.bundle.cmp(&b.bundle))
}

/// One catalogue's bundles ranked by share, descending, ties broken by name.
///
/// Bundles the catalogue never requires are not ranked: a zero share is not a
/// weak demand, it is the absence of one, and a tail of zeroes sorted by name
/// would put the fork between two catalogues in an alphabetical accident.
/// That exclusion is a property of [`bundle_demand`]'s map, which only gains a
/// key by being required — there is deliberately no filter here, because a
/// filter that can never fire reads as though it were load-bearing.
fn ranked_bundles(demand: &BTreeMap<String, usize>) -> Vec<(&str, usize)> {
    let mut ranked: Vec<(&str, usize)> = demand.iter().map(|(b, n)| (b.as_str(), *n)).collect();
    ranked.sort_by(|a, b| b.1.cmp(&a.1).then(a.0.cmp(b.0)));
    ranked
}

/// Render the matrix over corpora that ADR 0095 deferred until a second
/// catalogue existed.
///
/// Not a scoreboard and not a merge of the columns: the columns each answer
/// "what does this world supply against that catalogue", and both answer
/// zero. What only a matrix can say is what the catalogues *ask for*, and
/// they ask for different things. Every figure below is recomputed from the
/// corpora and the registry — nothing is parsed back out of a rendered
/// column, so this document cannot inherit a column's mistake, and the
/// integration test pins the two derivations together.
/// type-audit: bare-ok(identifier-text: columns), bare-ok(prose: return)
pub fn render_matrix(
    columns: &[(&Corpus, &BTreeMap<String, Outcome>)],
    registry: &ConceptRegistry,
) -> String {
    let mut s = String::new();
    s.push_str(
        "<!-- GENERATED FILE — do not edit. Regenerate with `hornvale tropes matrix`. -->\n\n",
    );
    s.push_str("# The trope matrix\n\n");

    // The disclaimer the columns carry, before any figure. A matrix is more
    // easily mistaken for a scoreboard than a single column is — two numbers
    // side by side read as a contest unless something says otherwise first.
    s.push_str(&wrap(
        "This measures reach against *these* catalogues. It is not a verdict on the world, \
         and it scores **representability only** — whether an agent could plan or recognise \
         a situation is not measured here.",
    ));
    s.push_str("\n\n");
    s.push_str(&wrap(
        "Neither is it a ranking of the catalogues against each other. Each is an instrument \
         carrying a declared bias (ADR 0095), so a column is a reading taken through that \
         bias and nothing more. The finding a single column cannot carry is where the \
         instruments **disagree** — which is what the demand table below is for.",
    ));
    s.push_str("\n\n");

    // Per column: what its own report says, and a pointer to it. The counts
    // come from this run's `resolve`, not from the committed report, which is
    // what makes the drift test meaningful.
    s.push_str("## Columns\n\n");
    let mut all_zero = !columns.is_empty();
    let mut rows = String::new();
    for (corpus, out) in columns {
        // The same `tally` the column's own report headline is rendered from,
        // so the row and the headline cannot state different numbers.
        let (stageable, inapplicable) = tally(out);
        all_zero &= stageable == 0;
        let path = artifact_path(corpus);
        let file = path.rsplit('/').next().unwrap_or(&path);
        rows.push_str(&format!(
            "| `{}` | {stageable} of {} | {inapplicable} | [{file}](./{file}) |\n",
            corpus.corpus,
            out.len()
        ));
    }
    s.push_str(&wrap(&format!(
        "All columns resolve against one registry of {} tokens, built once per run, so a \
         difference between columns is a difference between catalogues and never between \
         two worlds.",
        registry_tokens(registry).len()
    )));
    s.push_str("\n\n| Corpus | Stageable | Inapplicable | Report |\n|---|---|---|---|\n");
    s.push_str(&rows);
    s.push('\n');
    for (corpus, _) in columns {
        s.push_str(&wrap(&format!(
            "- `{}` — {}",
            corpus.corpus, corpus.provenance
        )));
        s.push('\n');
    }

    // The table only a matrix can hold: what each catalogue demands, side by
    // side, ordered by how much they differ. One demand map per catalogue,
    // computed once and shared with the ranking below, so the two sections
    // cannot state different numbers for the same bundle.
    let demands: Vec<BTreeMap<String, usize>> =
        columns.iter().map(|(c, _)| bundle_demand(c)).collect();
    let bundles: BTreeSet<&str> = demands
        .iter()
        .flat_map(|d| d.keys().map(String::as_str))
        .collect();
    let mut table: Vec<DemandRow> = bundles
        .iter()
        .map(|b| {
            let cells: Vec<Share> = columns
                .iter()
                .zip(&demands)
                .map(|((_, out), demand)| {
                    let required = demand.get(*b).copied().unwrap_or(0);
                    Share {
                        required,
                        total: out.len(),
                        percent: percent(required, out.len()),
                    }
                })
                .collect();
            let hi = cells.iter().map(|c| c.percent).max().unwrap_or(0);
            let lo = cells.iter().map(|c| c.percent).min().unwrap_or(0);
            DemandRow {
                gap: hi - lo,
                spread: Spread::of(&cells),
                bundle: (*b).to_string(),
                declared: columns.iter().any(|(c, _)| c.bundles.contains_key(*b)),
                cells,
            }
        })
        .collect();
    table.sort_by(demand_order);
    let undeclared = table.iter().filter(|r| !r.declared).count();

    s.push_str("\n## Demand\n\n");
    s.push_str(&wrap(&format!(
        "Every bundle either catalogue requires ({}), with the share of that catalogue's \
         situations requiring it. Shares are counted over the corpora themselves — a bundle's \
         numerator is the situations requiring it, the denominator is the whole catalogue — \
         and are not read back out of the rendered columns.",
        table.len()
    )));
    s.push_str("\n\n");
    if undeclared > 0 {
        // A `bundle:` reference no catalogue defines renders identically to a
        // real one, so without this the reader counts dangling asks as
        // demands — and two near-synonyms among them read as two capabilities.
        let verb = if undeclared == 1 { "is" } else { "are" };
        s.push_str(&wrap(&format!(
            "**{undeclared} of those {} {verb} marked †**: no catalogue here *declares* the \
             bundle its situations ask for. A dangling reference is not a capability — it \
             expands to itself, matches no registry token, and blocks its situation by \
             construction. Such a row exists because a catalogue asked, and carries the mark \
             because asking is all it can do; two of them may be near-synonyms without being \
             two demands.",
            table.len()
        )));
        s.push_str("\n\n");
    }
    s.push_str(&wrap(
        "**Gap** is the difference between the highest and lowest share, in percentage \
         points, and is what the table is sorted by, descending. Rows sharing a Gap are \
         ordered by their unrounded spread — recomputable from the counts in each cell — and \
         then by bundle name.",
    ));
    s.push_str("\n\n");
    if all_zero {
        s.push_str(&wrap(
            "Every column above reads 0 stageable, so nothing in this table is a score. It \
             says what each catalogue asks the world for, and the catalogues do not agree.",
        ));
        s.push_str("\n\n");
    }
    s.push_str("| Bundle |");
    for (corpus, _) in columns {
        s.push_str(&format!(" `{}` |", corpus.corpus));
    }
    s.push_str(" Gap |\n|---|");
    for _ in columns {
        s.push_str("---|");
    }
    s.push_str("---|\n");
    for row in &table {
        let mark = if row.declared { "" } else { " †" };
        s.push_str(&format!("| `bundle:{}`{mark} |", row.bundle));
        for c in &row.cells {
            s.push_str(&format!(" {}% ({}/{}) |", c.percent, c.required, c.total));
        }
        s.push_str(&format!(" {} |\n", row.gap));
    }

    // Where the two rankings run together and where they part. The table above
    // is sorted by disagreement, so it buries the agreement; this says it.
    s.push_str("\n## Agreement and fork\n\n");
    s.push_str(&wrap(
        "Each catalogue's own bundles ranked by share within that catalogue — descending, \
         ties by name — read down together until they part.",
    ));
    s.push_str("\n\n");
    // The same `demands` maps the table above was built from — so a bundle
    // cannot appear at one percentage in the table and another here.
    let rankings: Vec<Vec<(&str, usize)>> = demands.iter().map(ranked_bundles).collect();
    let totals: Vec<usize> = columns.iter().map(|(_, out)| out.len()).collect();
    let mut shared = 0;
    while let Some(head) = rankings.first().and_then(|r| r.get(shared)) {
        if rankings
            .iter()
            .all(|r| r.get(shared).map(|(b, _)| *b) == Some(head.0))
        {
            shared += 1;
        } else {
            break;
        }
    }
    if shared == 0 {
        s.push_str(&wrap(
            "The catalogues do not agree on even their first bundle.",
        ));
        s.push_str("\n\n");
    } else {
        let ranks = if shared == 1 {
            "their first rank".to_string()
        } else {
            format!("their first {shared} ranks")
        };
        s.push_str(&wrap(&format!("They agree without exception on {ranks}:")));
        s.push_str("\n\n");
        for i in 0..shared {
            let (bundle, _) = &rankings[0][i];
            let shares: Vec<String> = rankings
                .iter()
                .zip(&totals)
                .zip(columns)
                .map(|((r, total), (corpus, _))| {
                    format!("{}% in `{}`", percent(r[i].1, *total), corpus.corpus)
                })
                .collect();
            s.push_str(&format!(
                "{}. `bundle:{bundle}` — {}\n",
                i + 1,
                shares.join(", ")
            ));
        }
        s.push('\n');
    }
    let forked: Vec<String> = rankings
        .iter()
        .zip(&totals)
        .zip(columns)
        .filter_map(|((r, total), (corpus, _))| {
            r.get(shared).map(|(b, n)| {
                format!(
                    "- `{}` asks next for `bundle:{b}` ({}%)\n",
                    corpus.corpus,
                    percent(*n, *total)
                )
            })
        })
        .collect();
    if forked.is_empty() {
        s.push_str(&wrap("Neither catalogue ranks a bundle beyond that."));
        s.push('\n');
    } else {
        s.push_str(&wrap(&format!("They diverge at rank {}:", shared + 1)));
        s.push_str("\n\n");
        for line in &forked {
            s.push_str(line);
        }
    }
    s
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A requirement naming a token the registry does not hold resolves
    /// `Blocked`, never silently satisfied — the default-deny posture.
    #[test]
    fn an_unknown_token_blocks_its_situation() {
        let json = r#"{
          "corpus":"t","provenance":"t","frozen":"t",
          "bundles":{"b":["predicate:no-such-predicate"]},
          "situations":[{"id":"s1","name":"S","actants":{},
                         "requires":["bundle:b"],"excluded_by":[]}]
        }"#;
        let corpus = load(json).expect("corpus parses");
        let registry = hornvale_kernel::ConceptRegistry::default();
        let out = resolve(&corpus, &registry);
        match out.get("s1").expect("s1 resolved") {
            Outcome::Blocked(missing) => {
                assert_eq!(missing, &vec!["predicate:no-such-predicate".to_string()]);
            }
            other => panic!("expected Blocked, got {other:?}"),
        }
    }

    /// A registered token satisfies, so the situation is stageable.
    #[test]
    fn a_registered_token_makes_a_situation_stageable() {
        let json = r#"{
          "corpus":"t","provenance":"t","frozen":"t","bundles":{},
          "situations":[{"id":"s1","name":"S","actants":{},
                         "requires":["predicate:known"],"excluded_by":[]}]
        }"#;
        let corpus = load(json).expect("corpus parses");
        let mut registry = hornvale_kernel::ConceptRegistry::default();
        registry
            .register_predicate("known", false, "a predicate for the test")
            .expect("registers");
        assert_eq!(
            resolve(&corpus, &registry).get("s1"),
            Some(&Outcome::Stageable)
        );
    }

    /// `excluded_by` short-circuits: the world lacks the precondition, so the
    /// situation is inapplicable rather than blocked.
    #[test]
    fn an_excluded_situation_is_inapplicable_not_blocked() {
        let json = r#"{
          "corpus":"t","provenance":"t","frozen":"t","bundles":{},
          "situations":[{"id":"s1","name":"S","actants":{},
                         "requires":["predicate:absent"],
                         "excluded_by":["this world has no marriage"]}]
        }"#;
        let corpus = load(json).expect("corpus parses");
        let registry = hornvale_kernel::ConceptRegistry::default();
        assert_eq!(
            resolve(&corpus, &registry).get("s1"),
            Some(&Outcome::Inapplicable(
                "this world has no marriage".to_string()
            ))
        );
    }

    /// Resolution is deterministic: same inputs, identical output ordering.
    #[test]
    fn resolution_is_order_stable() {
        let json = r#"{
          "corpus":"t","provenance":"t","frozen":"t",
          "bundles":{"b":["predicate:x","predicate:y"]},
          "situations":[{"id":"s2","name":"B","actants":{},"requires":["bundle:b"],"excluded_by":[]},
                        {"id":"s1","name":"A","actants":{},"requires":["bundle:b"],"excluded_by":[]}]
        }"#;
        let corpus = load(json).expect("corpus parses");
        let registry = hornvale_kernel::ConceptRegistry::default();
        let a = resolve(&corpus, &registry);
        let b = resolve(&corpus, &registry);
        assert_eq!(format!("{a:?}"), format!("{b:?}"));
        assert_eq!(a.keys().collect::<Vec<_>>(), vec!["s1", "s2"]);
    }

    /// A dangling `bundle:` reference must block, not silently vanish into
    /// an empty expansion — the exact inversion of default-deny that an
    /// `unwrap_or_default()` would produce.
    #[test]
    fn a_dangling_bundle_reference_blocks() {
        let json = r#"{
          "corpus":"t","provenance":"t","frozen":"t","bundles":{},
          "situations":[{"id":"s1","name":"S","actants":{},
                         "requires":["bundle:does-not-exist"],"excluded_by":[]}]
        }"#;
        let corpus = load(json).expect("corpus parses");
        let registry = hornvale_kernel::ConceptRegistry::default();
        match resolve(&corpus, &registry).get("s1") {
            Some(Outcome::Blocked(missing)) => {
                assert_eq!(missing, &vec!["bundle:does-not-exist".to_string()]);
            }
            other => panic!("expected Blocked, got {other:?}"),
        }
    }

    /// Two bundles that share a token must not double-count it in `Blocked`,
    /// and the survivors keep first-appearance-in-corpus order — not sorted,
    /// which is the assertion `resolution_is_order_stable` cannot make on
    /// its own since this repo bans `HashMap` outright.
    #[test]
    fn blocked_tokens_are_deduplicated_in_corpus_order() {
        let json = r#"{
          "corpus":"t","provenance":"t","frozen":"t",
          "bundles":{
            "first":["predicate:shared","predicate:only-in-first"],
            "second":["predicate:shared","predicate:only-in-second"]
          },
          "situations":[{"id":"s1","name":"S","actants":{},
                         "requires":["bundle:first","bundle:second"],
                         "excluded_by":[]}]
        }"#;
        let corpus = load(json).expect("corpus parses");
        let registry = hornvale_kernel::ConceptRegistry::default();
        match resolve(&corpus, &registry).get("s1") {
            Some(Outcome::Blocked(missing)) => {
                assert_eq!(
                    missing,
                    &vec![
                        "predicate:shared".to_string(),
                        "predicate:only-in-first".to_string(),
                        "predicate:only-in-second".to_string(),
                    ]
                );
            }
            other => panic!("expected Blocked, got {other:?}"),
        }
    }

    /// A line break must never put a word starting with `-` at column 0.
    ///
    /// `render_matrix` wraps each column's provenance into a `- ` bullet, and
    /// Markdown reads a `-` at column 0 as a list marker — so a break landing
    /// one there renders a spurious nested bullet in the middle of a
    /// sentence, and in ordinary wrapped prose it opens a list that was never
    /// written. Neither frozen corpus's provenance holds a dashed clause, so
    /// nothing in the committed artifacts exercises this; a third
    /// catalogue's could, and it would arrive as a rendering defect in a
    /// document nobody hand-edits.
    #[test]
    fn wrapping_never_starts_a_line_with_a_dash() {
        // 71 + 1 + 5 = 77 > 76, so `-dash` is exactly the word the wrapper
        // would otherwise move to the start of the next line.
        let filler = "a".repeat(71);
        let wrapped = wrap(&format!(
            "{filler} -dash and then a tail long enough to wrap again"
        ));
        assert!(
            !wrapped.lines().any(|l| l.starts_with('-')),
            "a wrapped line begins with a Markdown list marker:\n{wrapped}"
        );
        assert!(
            wrapped.contains(&format!("{filler} -dash")),
            "the dashed word did not stay on the line it started:\n{wrapped}"
        );
        // The guard sits under `col > 0`, so a paragraph that deliberately
        // opens with a bullet marker — which is how the matrix renders each
        // column's provenance — still gets one.
        assert!(
            wrap("- `c` — provenance").starts_with("- "),
            "the guard swallowed a deliberate leading bullet"
        );
    }

    /// The report leads with provenance and carries all four sections, so a
    /// reader cannot mistake the number for a verdict on the world.
    #[test]
    fn the_report_states_provenance_and_all_four_sections() {
        let json = r#"{
          "corpus":"t","provenance":"a catalogue with known bias","frozen":"t",
          "bundles":{},
          "situations":[{"id":"s1","name":"S","actants":{},
                         "requires":["predicate:absent"],"excluded_by":[]}]
        }"#;
        let corpus = load(json).expect("corpus parses");
        let registry = hornvale_kernel::ConceptRegistry::default();
        let out = resolve(&corpus, &registry);
        let text = render(&corpus, &out, &registry, "tropes/test.trope.json");
        assert!(text.contains("a catalogue with known bias"));
        for section in ["## Provenance", "## Demand", "## Leverage", "## Supply"] {
            assert!(text.contains(section), "missing {section}");
        }
        assert!(text.contains("GENERATED FILE"));
    }

    /// A bundle required ONLY by inapplicable situations never enters the fan
    /// map, so it silently drops out of the ranked misses. The report must
    /// disclose it in prose — and must not smuggle it back in as a row, since
    /// the ranking is by fan-in and this bundle has none.
    #[test]
    fn a_bundle_masked_by_an_exclusion_is_disclosed_but_not_ranked() {
        let json = r#"{
          "corpus":"t","provenance":"t","frozen":"t",
          "bundles":{
            "masked":["predicate:never-registered"],
            "ranked-bundle":["predicate:also-absent"]
          },
          "situations":[
            {"id":"s1","name":"Blocked","actants":{"subject":"a"},
             "requires":["bundle:ranked-bundle"],"excluded_by":[]},
            {"id":"s2","name":"Excluded","actants":{"subject":"a"},
             "requires":["bundle:masked"],
             "excluded_by":["this world has no such thing"]}
          ]
        }"#;
        let corpus = load(json).expect("corpus parses");
        let registry = hornvale_kernel::ConceptRegistry::default();
        let out = resolve(&corpus, &registry);
        let text = render(&corpus, &out, &registry, "tropes/test.trope.json");

        // Disclosed in prose, with the arithmetic that gets a reader from the
        // row count to the true total.
        assert!(
            text.contains("1 missing bundle is not ranked below"),
            "no disclosure sentence:\n{text}"
        );
        assert!(
            text.contains("`bundle:masked`"),
            "the masked bundle is not named:\n{text}"
        );
        assert!(
            text.contains("2 missing bundles against the 1 ranked here"),
            "the disclosure does not reconcile 2 - 1 = 1:\n{text}"
        );

        // Not a row. A disclosure that becomes a 32nd entry in a fan-in
        // ranking is the defect it was written to fix, one direction over.
        assert!(
            !text.contains("| `bundle:masked` |"),
            "the masked bundle leaked into the ranking table:\n{text}"
        );
        assert!(
            text.contains("| `bundle:ranked-bundle` | 1 |"),
            "the genuinely ranked bundle lost its row:\n{text}"
        );
    }

    /// The matrix computes each share against its **own** catalogue's
    /// denominator and sorts by the gap between them.
    ///
    /// The failure this guards is the one a two-corpus document invites: a
    /// share divided by the wrong total, or by a pooled total across corpora.
    /// Both mistakes leave a plausible-looking percentage, and neither the
    /// byte ratchet nor the drift test can see them — those pin the matrix to
    /// itself and to the columns' headlines, not to the arithmetic. The
    /// denominators here (2 and 4) are deliberately different, so a pooled
    /// or swapped total renders a different number.
    #[test]
    fn matrix_shares_are_per_catalogue_and_sorted_by_the_gap() {
        let small = load(
            r#"{"corpus":"small","provenance":"p","frozen":"f",
                "bundles":{"common":["predicate:a"],"lopsided":["predicate:b"]},
                "situations":[
                  {"id":"s1","name":"A","actants":{},"requires":["bundle:common","bundle:lopsided"],"excluded_by":[]},
                  {"id":"s2","name":"B","actants":{},"requires":["bundle:common"],"excluded_by":[]}]}"#,
        )
        .expect("corpus parses");
        let large = load(
            r#"{"corpus":"large","provenance":"p","frozen":"f",
                "bundles":{"common":["predicate:a"],"other":["predicate:c"]},
                "situations":[
                  {"id":"s1","name":"A","actants":{},"requires":["bundle:common","bundle:other"],"excluded_by":[]},
                  {"id":"s2","name":"B","actants":{},"requires":["bundle:common","bundle:other"],"excluded_by":[]},
                  {"id":"s3","name":"C","actants":{},"requires":["bundle:common"],"excluded_by":[]},
                  {"id":"s4","name":"D","actants":{},"requires":["bundle:common"],"excluded_by":[]}]}"#,
        )
        .expect("corpus parses");
        let registry = hornvale_kernel::ConceptRegistry::default();
        let a = resolve(&small, &registry);
        let b = resolve(&large, &registry);
        let text = render_matrix(&[(&small, &a), (&large, &b)], &registry);

        // 2/2 and 4/4 are both 100% — a pooled denominator of 6 would render
        // 33% and 67% here, and a swapped one 50% and 200%.
        assert!(
            text.contains("| `bundle:common` | 100% (2/2) | 100% (4/4) | 0 |"),
            "the shared bundle's row is wrong:\n{text}"
        );
        assert!(
            text.contains("| `bundle:lopsided` | 50% (1/2) | 0% (0/4) | 50 |"),
            "the row for a bundle only `small` requires is wrong:\n{text}"
        );
        assert!(
            text.contains("| `bundle:other` | 0% (0/2) | 50% (2/4) | 50 |"),
            "the row for a bundle only `large` requires is wrong:\n{text}"
        );
        // Widest gap first, so the disagreement leads; the two 50s tie and
        // fall back to bundle name.
        let lopsided = text.find("`bundle:lopsided` |").expect("lopsided row");
        let other = text.find("`bundle:other` |").expect("other row");
        let common = text.find("`bundle:common` |").expect("common row");
        assert!(
            lopsided < other && other < common,
            "the table is not sorted by gap then name:\n{text}"
        );
        // Both rank `common` first at 100%, then part.
        assert!(
            text.contains("They agree without exception on their first rank:"),
            "no agreement sentence:\n{text}"
        );
        assert!(
            text.contains("1. `bundle:common` — 100% in `small`, 100% in `large`"),
            "the agreed rank is not spelled out:\n{text}"
        );
        assert!(
            text.contains("They diverge at rank 2:"),
            "no fork sentence:\n{text}"
        );
        assert!(
            text.contains("- `small` asks next for `bundle:lopsided` (50%)")
                && text.contains("- `large` asks next for `bundle:other` (50%)"),
            "the fork does not name what each catalogue asks for next:\n{text}"
        );
    }

    /// A `bundle:` reference no catalogue declares still earns a row — it is
    /// real demand, authored deliberately — but is marked, and the count of
    /// such rows is derived rather than written down.
    ///
    /// Without the mark a dangling reference renders identically to a
    /// declared bundle, so a reader counts asks as demands, and two dangling
    /// near-synonyms read as two separate capabilities. That is precisely
    /// what `tvtropes-2012` contains: 12 of the live matrix's 52 rows name no
    /// declared bundle, including `food-and-drink` beside `food-and-eating`.
    #[test]
    fn a_bundle_no_catalogue_declares_is_rowed_but_marked() {
        let corpus = load(
            r#"{"corpus":"c","provenance":"p","frozen":"f",
                "bundles":{"declared":["predicate:a"]},
                "situations":[
                  {"id":"s1","name":"A","actants":{},"requires":["bundle:declared","bundle:dangling"],"excluded_by":[]},
                  {"id":"s2","name":"B","actants":{},"requires":["bundle:declared"],"excluded_by":[]}]}"#,
        )
        .expect("corpus parses");
        let registry = hornvale_kernel::ConceptRegistry::default();
        let out = resolve(&corpus, &registry);
        let text = render_matrix(&[(&corpus, &out)], &registry);

        assert!(
            text.contains("| `bundle:dangling` † | 50% (1/2) | 0 |"),
            "the undeclared bundle is not rowed and marked:\n{text}"
        );
        assert!(
            text.contains("| `bundle:declared` | 100% (2/2) | 0 |"),
            "the declared bundle should carry no mark:\n{text}"
        );
        // The count is computed from the rows, not asserted in prose by hand.
        assert!(
            text.contains("**1 of those 2 is marked †**"),
            "the disclosure does not derive the split:\n{text}"
        );
    }

    /// One counting rule for one quantity: a situation requiring the same
    /// bundle twice is one situation requiring it, in the demand table and in
    /// the ranking alike.
    ///
    /// `load` does not reject a repeated requirement, and the two sections
    /// used to count it differently — the table by situations, the ranking by
    /// occurrences. Neither frozen corpus repeats one today, so the first
    /// document to print a bundle at two different percentages would have
    /// been a third catalogue's, in the artifact built to prevent exactly
    /// that.
    #[test]
    fn a_bundle_required_twice_by_one_situation_counts_once_everywhere() {
        let corpus = load(
            r#"{"corpus":"c","provenance":"p","frozen":"f",
                "bundles":{"twice":["predicate:a"],"once":["predicate:b"]},
                "situations":[
                  {"id":"s1","name":"A","actants":{},"requires":["bundle:twice","bundle:twice"],"excluded_by":[]},
                  {"id":"s2","name":"B","actants":{},"requires":["bundle:once"],"excluded_by":[]}]}"#,
        )
        .expect("corpus parses");
        let registry = hornvale_kernel::ConceptRegistry::default();
        let out = resolve(&corpus, &registry);
        let text = render_matrix(&[(&corpus, &out)], &registry);

        // One situation of two, not two of two — and not 100%.
        assert!(
            text.contains("| `bundle:twice` | 50% (1/2) | 0 |"),
            "the demand table double-counted a repeated requirement:\n{text}"
        );
        // The ranking must print the SAME percentage. Counting occurrences
        // would render `twice` at 100% here while the table said 50%, and
        // would rank it above `once` on a count the table never showed.
        assert!(
            text.contains("2. `bundle:twice` — 50% in `c`"),
            "the ranking disagrees with the table about the repeated bundle:\n{text}"
        );
        assert!(
            text.contains("1. `bundle:once` — 50% in `c`"),
            "the ranking lost the un-repeated bundle or reordered it:\n{text}"
        );
        assert_eq!(bundle_demand(&corpus).get("twice"), Some(&1));
    }

    /// Rows sharing a rendered gap are ordered by their unrounded spread, not
    /// alphabetically.
    ///
    /// These are the four live bundles that all render `Gap` 7: their true
    /// spreads are 7.13, 7.00, 6.96 and 6.54 percentage points. A rendered
    /// gap of N covers true spreads across a two-point window, so it tiers
    /// rather than orders; falling straight through to the bundle name put
    /// `witnessing` (the widest) last and `interpersonal-violence` (the
    /// narrowest) second, which is not "descending delta" in any sense.
    /// Exercises the real comparator, not a copy of it.
    #[test]
    fn equal_rendered_gaps_are_broken_by_the_unrounded_spread() {
        let mut rows: Vec<DemandRow> = [
            ("interpersonal-violence", 9, 129),
            ("impaired-reason", 1, 40),
            ("traversable-geography", 4, 17),
            ("witnessing", 6, 39),
        ]
        .iter()
        .map(|(name, polti, tv)| {
            let cells = vec![
                Share {
                    required: *polti,
                    total: 36,
                    percent: percent(*polti, 36),
                },
                Share {
                    required: *tv,
                    total: 409,
                    percent: percent(*tv, 409),
                },
            ];
            let hi = cells.iter().map(|c| c.percent).max().expect("cells");
            let lo = cells.iter().map(|c| c.percent).min().expect("cells");
            DemandRow {
                gap: hi - lo,
                spread: Spread::of(&cells),
                bundle: (*name).to_string(),
                declared: true,
                cells,
            }
        })
        .collect();

        // The premise: the rendered column cannot order these at all.
        assert!(
            rows.iter().all(|r| r.gap == 7),
            "these four no longer share a rendered gap, so this test's premise is gone"
        );
        rows.sort_by(demand_order);
        assert_eq!(
            rows.iter().map(|r| r.bundle.as_str()).collect::<Vec<_>>(),
            vec![
                "witnessing",
                "impaired-reason",
                "traversable-geography",
                "interpersonal-violence"
            ],
            "the tier is not ordered by descending unrounded spread"
        );
    }

    // The two guarantees that used to live here as one filesystem-walking unit
    // test — every corpus declared in `CORPORA` has a committed column, and
    // every declared corpus identifies its situations uniquely — are now in
    // `cli/tests/trope_coverage.rs`, as
    // `every_declared_matrix_column_links_to_a_committed_report` and
    // `every_corpus_file_identifies_its_situations_uniquely`. Reaching the
    // workspace root from here needed the build-path-embedding `env!` that
    // `cli/tests/build_path_embedding.rs` freezes — it bakes the build
    // directory into the shipped binary and narrows decision 0090's cross-host
    // oracle, and the scan counts it whether or not it sits behind
    // `#[cfg(test)]`. A test binary's embedded path never ships, so `tests/` is
    // where a filesystem assertion belongs. That scan is a plain text match, so
    // do not spell the variable out here either. Nothing that needs `CORPORA`
    // at compile time is lost:
    // the matrix's Columns table is rendered *from* `CORPORA`, so parsing a
    // live `tropes matrix` run binds the declared list to the committed
    // artifacts without importing anything from this binary-only crate.

    /// The live corpus is structurally sound: ids are unique, and there are
    /// exactly thirty-six situations.
    ///
    /// Uniqueness is the load-bearing half. `resolve` returns a `BTreeMap`
    /// keyed by `id`, so a duplicated id means the second write wins, one
    /// situation vanishes, and the report prints a quietly smaller
    /// denominator — `Stageable 0 of 35` — with no warning. This is the one
    /// place the campaign's default-deny posture stopped at the corpus door,
    /// and a second corpus is the explicit next step: a copy-paste while
    /// adding Propp's inventory would shrink the denominator silently and a
    /// reader would take the wrong number for the intended size.
    ///
    /// The hardcoded 36 looks brittle and is not. This corpus is **frozen**,
    /// and a change to its situation count is precisely the event that should
    /// require someone to come here and say so deliberately — the freeze is
    /// what the preregistered scoring rests on. Do not "fix" this by deriving
    /// the number from the file it is checking.
    #[test]
    fn the_live_corpus_has_thirty_six_uniquely_identified_situations() {
        let corpus =
            load(include_str!("../../tropes/polti.trope.json")).expect("the live corpus parses");
        let mut seen = BTreeSet::new();
        for st in &corpus.situations {
            assert!(
                seen.insert(st.id.as_str()),
                "duplicate situation id `{}` — one situation would vanish into a \
                 BTreeMap key collision and the report would understate its denominator",
                st.id
            );
        }
        assert_eq!(
            corpus.situations.len(),
            36,
            "the frozen corpus must hold exactly 36 situations; changing that \
             changes what every preregistered number was scored against"
        );
    }

    /// The second corpus, asserted the same way and for the same reason.
    ///
    /// `tvtropes-2012` is frozen before measurement exactly as `polti-1895`
    /// is, and 409 is what its own provenance document declares. Deriving
    /// this number from the file would make the assertion vacuous — the
    /// point is that a change to the corpus has to come through here and be
    /// said out loud.
    #[test]
    fn the_second_corpus_has_four_hundred_and_nine_uniquely_identified_situations() {
        let corpus = load(include_str!("../../tropes/tvtropes-2012.trope.json"))
            .expect("the second corpus parses");
        let mut seen = BTreeSet::new();
        for st in &corpus.situations {
            assert!(
                seen.insert(st.id.as_str()),
                "duplicate situation id `{}` — one situation would vanish into a \
                 BTreeMap key collision and the report would understate its denominator",
                st.id
            );
        }
        assert_eq!(
            corpus.situations.len(),
            409,
            "the frozen corpus must hold exactly 409 situations; changing that \
             changes what every preregistered number was scored against"
        );
        assert_eq!(corpus.corpus, "tvtropes-2012");
    }

    /// The corpus's actant roles stay inside Greimas' six. A seventh role, or
    /// a situation declaring none, means the hand-authored decomposition
    /// vocabulary drifted — and nothing else in the workspace checks it.
    #[test]
    fn every_situation_declares_only_greimas_actants() {
        const GREIMAS: [&str; 6] = [
            "helper", "object", "opponent", "receiver", "sender", "subject",
        ];
        let corpus =
            load(include_str!("../../tropes/polti.trope.json")).expect("the live corpus parses");
        for st in &corpus.situations {
            assert!(!st.actants.is_empty(), "{} declares no actants", st.id);
            for role in st.actants.keys() {
                assert!(
                    GREIMAS.contains(&role.as_str()),
                    "{}: actant role `{role}` is not one of Greimas' six",
                    st.id
                );
            }
        }
    }
}

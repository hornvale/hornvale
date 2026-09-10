//! Rendering the Domesday: one Book page per domain plus an index.
//!
//! **The prose budget (spec §4.6):** exactly one authored sentence per
//! domain page — [`framing_line`], a plain data table indexed by domain and
//! merely looked up, the same shape as `detect.rs`'s frozen `DOMAIN_CRATES`
//! roster. No sentence anywhere else in this module may state a number that
//! was not read from the committed census at render time; every count,
//! statistic, and share below is computed from `Census`/`Finding` arguments,
//! never typed into a format string as a literal.
//!
//! **§4.6a — a gap in the world is rendered, not fixed:** [`render_domain`]
//! renders a domain with zero metrics by saying so explicitly (the string
//! `"no metrics"` appears on the page), rather than silently omitting the
//! page or inventing a placeholder metric.
//!
//! **The claim line (spec §5) is the one addition to that budget, and it is
//! not an authored sentence:** [`claim_line`] assembles a criterion, a
//! measurement and a verdict for a metric some frozen corpus in
//! `regularities/` scores, from that corpus's own parameters and from the
//! census being rendered. Its numbers are read at render time exactly like
//! every other number on the page; what it adds is that a reader outside
//! the program can disagree with it. A metric no corpus scores gains no
//! such line — see [`render_claims`].
//!
//! **Grouping (spec §4.4's D2 ⊆ D4 observation):** a metric that trips more
//! than one detector is rendered ONCE, as one heading under which every
//! detector that fired is listed — never as several independent-looking
//! findings under the same name. [`group_by_metric`] is the seam that
//! enforces this for both the per-domain findings section and could be
//! reused by an index-level rollup if one is ever added.

use crate::domesday::anomaly::{REPORT_SIZE, TAIL_DEPTH_BAR, TOP_WORLDS, WorldAnomaly};
use crate::domesday::census::{Census, Column};
use crate::domesday::corpus::{ScoredCorpus, ScoredItem};
use crate::domesday::detect::{DECLARED_DETECTORS, Finding};
use crate::domesday::stats::{categorical, numeric};
use crate::metrics::Domain;
use hornvale_kernel::quantize;
use std::collections::{BTreeMap, BTreeSet};

/// The header every generated Domesday page (index + one per domain) opens
/// with, matching the project's existing generated-page convention
/// (`book/src/reference/`, `book/src/laboratory/generated/`). **Not** shared
/// with [`render_anomalies`], which is written by a different subcommand —
/// see [`ANOMALIES_HEADER`].
const HEADER: &str =
    "<!-- GENERATED FILE — do not edit. Regenerate with `hornvale lab domesday`. -->";

/// The header [`render_anomalies`]'s page opens with. A distinct constant
/// from [`HEADER`] on purpose (F3): the anomaly report is written by
/// `hornvale lab anomalies`, not `hornvale lab domesday`, and the two
/// commands must never be conflated in the regenerate instruction a reader
/// would actually run.
const ANOMALIES_HEADER: &str =
    "<!-- GENERATED FILE — do not edit. Regenerate with `hornvale lab anomalies`. -->";

/// The twelve domains a metric may declare (spec §4.1), in the order the
/// survey presents them — also the file-stem roster for `book/src/domesday/`:
/// `cli`'s `domesday` subcommand writes exactly one page per entry here.
///
/// Derived from [`Domain::all()`] (`metrics.rs`), not a second hand-typed
/// list. This module used to carry its own `DOMAINS: &[&str]` const,
/// independently enumerating the same twelve names, with only a one-way
/// guard (`no_domain_is_currently_empty`, below): every `DOMAINS` entry was
/// checked to have a column, but nothing checked the reverse — that every
/// column's domain was one of `DOMAINS`. A 13th [`Domain`] variant would
/// have compiled, silently gotten no page, and left `render_index`'s header
/// total (which counts every domained column) larger than the sum of its
/// own per-domain table. `Domain::all()` already existed as the single
/// source of truth with zero callers; deriving from it here removes the
/// fourth roster instead of adding a fifth test.
/// type-audit: bare-ok(identifier-text: return)
pub fn domains() -> Vec<&'static str> {
    Domain::all().iter().map(Domain::as_str).collect()
}

/// The one authored sentence a domain page is permitted (spec §4.6),
/// stored as data and merely assembled by [`render_domain`] rather than
/// composed inline — the same "authored fact, computed everything else"
/// split The Digest's `self-map-line` pattern uses. None of these mention a
/// number: every number on the page is read from the census at render time.
fn framing_line(domain: &str) -> &'static str {
    match domain {
        "astronomy" => {
            "The sky a world is generated under: its star, its moons, its rotation and \
             tilt, and the orbital rhythm that follows from them."
        }
        "terrain" => {
            "The solid shape of a world: its plates, its elevation, and the landforms \
             the sculpting pipeline leaves behind."
        }
        "climate" => {
            "The temperature and moisture a world's astronomy and terrain resolve into, \
             world over world."
        }
        "hydrology" => {
            "Where water moves and gathers: rivers, lakes, aquifers, and the coasts \
             between land and sea."
        }
        "biology" => {
            "The living things a world supports, from biome cover to the life-history \
             traits of its peoples."
        }
        "settlement" => {
            "Where and how peoples settle: placement, condensation, and the built shape \
             of a community."
        }
        "demography" => {
            "How many, and of what structure: the population counts a world's peoples \
             carry."
        }
        "society" => {
            "How settled peoples organize themselves — the social structures layered \
             atop demography."
        }
        "religion" => {
            "What peoples believe: pantheons, cults, and the vestiges belief leaves in \
             the world."
        }
        "language" => {
            "How peoples speak: phonology, lexicon, and the divergence between related \
             tongues."
        }
        "naming" => {
            "How things are named: the conventions and confusable forms a language's \
             naming layer produces."
        }
        "history" => {
            "How a world's occupation record accumulates: strata, tenure, and how much \
             of it survives to be read back."
        }
        _ => "An uncatalogued domain — this page has no authored framing.",
    }
}

/// Title-case a domain's kebab/lowercase name for a page heading (`"naming"`
/// -> `"Naming"`). All twelve entries [`domains()`] returns are single words, so
/// this only needs to capitalize the first character.
fn title_of(domain: &str) -> String {
    let mut chars = domain.chars();
    match chars.next() {
        Some(first) => first.to_uppercase().collect::<String>() + chars.as_str(),
        None => String::new(),
    }
}

/// Every metric column belonging to `domain`, sorted by name — deterministic
/// regardless of the order `schema.json` happened to list columns in.
fn domain_columns<'a>(c: &'a Census, domain: &str) -> Vec<&'a Column> {
    let mut cols: Vec<&Column> = c
        .columns
        .iter()
        .filter(|col| col.domain == domain)
        .collect();
    cols.sort_by(|a, b| a.name.cmp(&b.name));
    cols
}

/// Both flag rows, always present even when a value is unanimous (spec
/// §4.2: "flag: true/false counts and shares (both rows always present)").
/// `categorical` alone would silently omit a value with zero occurrences —
/// exactly the case an invariant-flag metric produces on 1,000/1,000 worlds.
fn flag_counts(c: &Census, metric: &str) -> Vec<(&'static str, usize)> {
    let counts = categorical(c, metric);
    let mut true_n = 0usize;
    let mut false_n = 0usize;
    for (value, n) in &counts {
        match value.as_str() {
            "true" => true_n = *n,
            "false" => false_n = *n,
            _ => {}
        }
    }
    vec![("true", true_n), ("false", false_n)]
}

/// Render a `numeric`/`integer` metric's statistics: presence counts plus
/// the min/p25/median/p75/max/mean table (spec §4.2), every float quantized
/// at this emit boundary. `integer` additionally reports the exact mode —
/// the single most-frequent raw value, via `categorical`'s count-then-
/// lexicographic ordering, since [`crate::domesday::stats::NumericStats`]
/// carries no mode field of its own.
fn render_numeric_stats(c: &Census, col: &Column) -> String {
    let total = c.rows.len();
    let Some(s) = numeric(c, &col.name) else {
        return format!(
            "n = 0 present, {} absent (of {total} worlds) — no world reports a value.\n",
            c.absent_count(&col.name)
        );
    };
    let mut out = format!(
        "n = {} present, {} absent (of {total} worlds)\n\n\
         | min | p25 | median | p75 | max | mean |\n\
         |---|---|---|---|---|---|\n\
         | {} | {} | {} | {} | {} | {} |\n",
        s.n,
        s.absent,
        quantize(s.min),
        quantize(s.p25),
        quantize(s.median),
        quantize(s.p75),
        quantize(s.max),
        quantize(s.mean),
    );
    if col.kind == "integer"
        && let Some((mode_value, mode_count)) = categorical(c, &col.name).first()
    {
        out.push_str(&format!("\nmode: `{mode_value}` ({mode_count} worlds)\n"));
    }
    out
}

/// Render a `categorical` metric: presence counts plus a value/count/share
/// table, descending by count then lexicographic (spec §4.2), quantizing
/// every computed share at this emit boundary.
fn render_categorical_stats(c: &Census, col: &Column) -> String {
    let total = c.rows.len();
    let present = c.values(&col.name).len();
    let absent = c.absent_count(&col.name);
    let counts = categorical(c, &col.name);
    let mut out = format!(
        "n = {present} present, {absent} absent (of {total} worlds)\n\n\
         | value | count | share |\n|---|---|---|\n"
    );
    for (value, count) in &counts {
        let share = if present > 0 {
            *count as f64 / present as f64 * 100.0
        } else {
            0.0
        };
        out.push_str(&format!(
            "| `{value}` | {count} | {:.1}% |\n",
            quantize(share)
        ));
    }
    out
}

/// Render a `flag` metric: presence counts plus both the `true` and `false`
/// rows (always both, per spec §4.2), quantizing every computed share.
fn render_flag_stats(c: &Census, col: &Column) -> String {
    let total = c.rows.len();
    let present = c.values(&col.name).len();
    let absent = c.absent_count(&col.name);
    let mut out = format!(
        "n = {present} present, {absent} absent (of {total} worlds)\n\n\
         | value | count | share |\n|---|---|---|\n"
    );
    for (value, count) in flag_counts(c, &col.name) {
        let share = if present > 0 {
            count as f64 / present as f64 * 100.0
        } else {
            0.0
        };
        out.push_str(&format!(
            "| `{value}` | {count} | {:.1}% |\n",
            quantize(share)
        ));
    }
    out
}

/// Whether `bytes[start..]` begins a registry-ID-shaped token — 2-8
/// uppercase ASCII letters, a hyphen, 1-4 digits, an optional trailing
/// lowercase letter (`SKY-21`, `MAP-22a`, `BIO-2`) — bounded on both sides
/// by a non-alphanumeric character (or the string's edge), the same shape
/// `cli/tests/docs_consistency.rs`'s `find_registry_id` looks for. Returns
/// the index just past the token if it matches.
fn registry_id_end(bytes: &[u8], start: usize) -> Option<usize> {
    let prev_ok = start == 0 || !bytes[start - 1].is_ascii_alphanumeric();
    if !prev_ok {
        return None;
    }
    let mut i = start;
    while i < bytes.len() && bytes[i].is_ascii_uppercase() {
        i += 1;
    }
    let letters = i - start;
    if !(2..=8).contains(&letters) || bytes.get(i) != Some(&b'-') {
        return None;
    }
    i += 1;
    let digits_start = i;
    while i < bytes.len() && bytes[i].is_ascii_digit() {
        i += 1;
    }
    if i == digits_start {
        return None;
    }
    if i < bytes.len() && bytes[i].is_ascii_lowercase() {
        i += 1;
    }
    let next_ok = i >= bytes.len() || !bytes[i].is_ascii_alphanumeric();
    if next_ok { Some(i) } else { None }
}

/// Collapse doubled internal spacing and space-adjacent-to-punctuation
/// artifacts left behind by deleting a span from the middle of a sentence.
/// Applied repeatedly to a fixed point, since one fix (`" )" -> ")"`) can
/// expose another (`",)" -> ")"`).
fn tidy_punctuation(s: &str) -> String {
    let mut out = s.to_string();
    loop {
        let before = out.clone();
        for (from, to) in [
            ("  ", " "),
            ("( ", "("),
            (" )", ")"),
            (" ,", ","),
            (" ;", ";"),
            // Deliberately " . " (space-dot-space), never bare " .": an
            // ellipsis like "0 (fast/prolific) ... 1" has no space between
            // its dots, so this rule cannot touch it (see this module's
            // tests) while still fixing "target . Absent" left behind by
            // removing a parenthetical that sat directly before a period.
            (" . ", ". "),
            (",)", ")"),
            (";)", ")"),
            ("(,", "("),
            ("(;", "("),
        ] {
            out = out.replace(from, to);
        }
        if out == before {
            break;
        }
    }
    out.trim().to_string()
}

/// Redact any registry-ID citation from a metric's `doc` text before it is
/// embedded in a generated Book page.
///
/// **Why this exists:** `cli/tests/docs_consistency.rs`'s
/// `the_book_carries_no_registry_ids_or_process_vocabulary` (part of `make
/// gate`) forbids citing the idea registry anywhere in the Book outside the
/// marked Frontier part (`docs/CLAUDE.md`) — a rule this survey did not
/// invent but does newly have to honor, because it is the first place
/// several metric `doc` strings (authored for internal contexts: `hornvale
/// lab list-metrics`, the concept audits) get embedded into a merged-
/// reality page. On the committed census this fires on seven citations —
/// `SKY-21`, `MAP-10`, `MAP-22`, `BIO-2`, `CAP-2`, `MEM-7`, `LANG-41` — every
/// one of them inside a trailing parenthetical aside, which is this
/// project's own convention for such citations. So the primary strategy is
/// to drop the WHOLE parenthetical span that contains a citation (verified
/// against all seven to read cleanly afterward — see this module's tests),
/// rather than surgically excising just the token and patching the
/// grammar around it. A citation found outside any parenthetical (not
/// observed in the committed data, but not guaranteed never to occur) falls
/// back to redacting the bare token, plus a trailing possessive `'s` since
/// a lone `'s` cannot grammatically survive its subject's removal.
///
/// Never touches `schema.json`/`metrics.rs` themselves — those doc strings
/// are a committed artifact other consumers read unmodified (`list-
/// metrics`, `backfill-schema`); this redaction is purely a render-time
/// transform, scoped to what this module puts on a Book page.
fn redact_registry_citations(doc: &str) -> String {
    let bytes = doc.as_bytes();

    // Every top-level parenthetical span (nesting-aware, so `log(x)` inside
    // a citation-bearing span is removed as a unit along with it, and an
    // unrelated `log(x)` elsewhere is left alone if its own span carries no
    // citation).
    let mut spans: Vec<(usize, usize)> = Vec::new();
    let mut depth = 0i32;
    let mut span_start = None;
    for (i, &b) in bytes.iter().enumerate() {
        if b == b'(' {
            if depth == 0 {
                span_start = Some(i);
            }
            depth += 1;
        } else if b == b')' {
            depth -= 1;
            if depth == 0
                && let Some(s) = span_start.take()
            {
                spans.push((s, i + 1));
            }
        }
    }

    let carries_citation = |s: usize, e: usize| -> bool {
        (s..e).any(|j| bytes[j].is_ascii_uppercase() && registry_id_end(bytes, j).is_some())
    };
    let mut out = doc.to_string();
    for &(s, e) in spans.iter().rev() {
        if carries_citation(s, e) {
            out.replace_range(s..e, "");
        }
    }

    // Fallback: a citation that was never inside a paren at all.
    let remaining = out.as_bytes().to_vec();
    let mut bare_spans: Vec<(usize, usize)> = Vec::new();
    let mut i = 0;
    while i < remaining.len() {
        if remaining[i].is_ascii_uppercase()
            && let Some(mut end) = registry_id_end(&remaining, i)
        {
            if remaining[end..].starts_with(b"'s") {
                end += 2;
            }
            bare_spans.push((i, end));
            i = end;
        } else {
            i += 1;
        }
    }
    for &(s, e) in bare_spans.iter().rev() {
        out.replace_range(s..e, "");
    }

    tidy_punctuation(&out)
}

/// The label every claim line opens with.
///
/// Public because it is the only thing a test can assert the ABSENCE of: the
/// words "predicted" and "measured" already occur inside several metric doc
/// strings this survey embeds (`weft-legibility-mi-spring`'s among them), so
/// "the page carries no claim" cannot be checked by grepping for either of
/// them. This marker occurs nowhere else in the corpus of generated prose.
/// type-audit: bare-ok(identifier-text)
pub const CLAIM_MARKER: &str = "**Frozen claim** —";

/// A measured number, as a claim line states it.
///
/// Six decimal places on the quantized value: quantization because this is
/// an emit boundary like every other float on these pages (decision 0033),
/// and six places because that is what `hornvale::regularities`' own
/// `measure` mode prints, so the number a reader sees here is spelled the
/// same as the number the resolver reports.
///
/// **Public so a test can state what a page WILL print without transcribing
/// today's census into itself.** Three assertions in
/// `cli/tests/suite/regularity_coverage.rs` carried this number as a literal
/// (`measured -0.577645.`) and went red on the canonical box when an
/// unrelated campaign moved the `rank-size-slope` column — a snapshot of one
/// day's census sitting inside guards whose subject is the corpus, not the
/// census. Those assertions call this instead.
/// type-audit: bare-ok(ratio: measured), bare-ok(prose: return)
pub fn measured_text(measured: f64) -> String {
    format!("{:.6}", quantize(measured))
}

/// The heading of the per-page section that glosses what a frozen claim is
/// and names the corpora behind the page's claims.
///
/// Public so the claim lines can link to it and a test can assert its
/// presence and absence, and so the anchor below cannot drift from it
/// silently.
/// type-audit: bare-ok(identifier-text)
pub const CLAIMS_SECTION_TITLE: &str = "Frozen claims";

/// The in-page anchor mdBook derives from [`CLAIMS_SECTION_TITLE`]
/// (lower-cased, spaces to hyphens). Asserted against the title itself in
/// this module's tests, so renaming the section without the anchor breaks
/// the build rather than the link.
/// type-audit: bare-ok(identifier-text)
pub const CLAIMS_SECTION_ANCHOR: &str = "#frozen-claims";

/// The one authored paragraph the claims section is permitted, held as data
/// on the same pattern as [`framing_line`].
///
/// It exists because "Frozen claim" is a term of art this survey invented,
/// and a marker a reader cannot look up is decoration. It states no number:
/// every number in a claim comes from the corpus or the census.
///
/// **It deliberately makes no blindness claim.** An earlier version said a
/// frozen claim is "a prediction an imported corpus made about this
/// population *before* any of it was measured", which is a UNIFORM claim
/// this page cannot support: one of the founding corpus's four scored items
/// discloses that it is not a blind test. A false uniform assurance is worse
/// on this surface than on any other, because this is the surface a reader
/// is invited to catch us on. Blindness is stated by
/// [`blindness_sentence`] instead, derived per page from what the items
/// actually disclose.
const CLAIMS_PREAMBLE: &str = "\
Some metrics above carry a **frozen claim**: a prediction an imported \
corpus made about this population, printed beside what the committed census \
says today. The corpus is data this survey only reads — the corpus supplies \
the regularity, its source and the criterion, and the survey supplies the \
measurement and re-states the recorded verdict. Every part of a claim line \
is derived from one of those two, so a corpus that changes moves the line.";

/// What this page may honestly say about whether its claims were
/// preregistered blind, given what its own items disclose.
///
/// Two branches, both derived, neither hard-coded to today's corpus:
///
/// - **no disclosure anywhere** — the page states blindness plainly,
///   because on that page it is true;
/// - **one or more disclosures** — the page says so and NAMES the items,
///   so a reader who scrolled straight to a verdict can find out which one
///   is not blind.
///
/// The naming half is what makes this more than a hedge. "Some of these may
/// not be blind" would technically avoid the falsehood while telling a
/// reader nothing they could act on; a named item is checkable.
fn blindness_sentence(scored: &[&ScoredItem]) -> String {
    let disclosed: Vec<&&ScoredItem> = scored
        .iter()
        .filter(|item| item.disclosure.is_some())
        .collect();
    if disclosed.is_empty() {
        return "Every criterion on this page was authored before its statistic was \
                looked at."
            .to_string();
    }
    let names = disclosed
        .iter()
        .map(|item| format!("`{}`", item.id))
        .collect::<Vec<_>>()
        .join(", ");
    format!(
        "Criteria here were authored before their statistics were looked at, with \
         {} declared exception(s) — {} — each of which states its own disclosure on \
         its claim line above. Do not read this page as a page of blind predictions \
         without checking which.",
        disclosed.len(),
        names
    )
}

/// The claim line printed under a scored metric's statistics.
///
/// The whole point of spec §5. The stats table above it is a number nobody
/// can be *wrong* about; this is a sentence someone outside the program can
/// catch us on — it states the regularity, cites where the source states it,
/// names the corpus and item, gives the criterion that corpus froze BEFORE
/// anything was measured, the number today's census produces, and the
/// verdict.
///
/// **Every part of it is derived.** The title, source, corpus id, item id
/// and verdict come from the frozen corpus file; the criterion prose is
/// assembled from that file's own parameters
/// ([`crate::domesday::corpus::Criterion::prose`]); the number comes from
/// the census being rendered. Nothing here is a literal that happens to be
/// true today — a transcribed verdict would go on printing `FLAT` after a
/// future census made it `grown`, which is the failure the regularity
/// family exists to catch.
///
/// The title and source are emitted VERBATIM, never through
/// [`redact_registry_citations`]: that redactor matches on shape with no
/// prefix allowlist and would delete `II-3` out of
/// `Ch. II, 'Emergence'; Animation II-3`. The Book's own registry guard
/// filters by real registry prefixes and is untroubled by it.
/// type-audit: bare-ok(ratio: measured), bare-ok(prose: return)
pub fn claim_line(item: &ScoredItem, measured: f64) -> String {
    claim_sentence(item, &measured_text(measured))
}

/// The claim line for a statistic no world in this census reports.
///
/// Says `absent` where the number would go rather than falling silent: an
/// item whose statistic has gone unreportable is a louder fact than one
/// whose median moved, and dropping the line would hide it behind a page
/// that still looks complete.
///
/// **The narrower silence this does NOT cover:** a statistic that vanishes
/// from the census as a COLUMN takes its claim line off the page entirely,
/// because the metric it hung under is no longer rendered. That is caught
/// by `hornvale regularities check` (a `Dangling` finding naming the column)
/// and not by anything on the page.
/// type-audit: bare-ok(prose: return)
pub fn claim_line_unmeasured(item: &ScoredItem) -> String {
    claim_sentence(item, "absent (no world reported a value)")
}

/// The shared body of the two claim lines: everything except how the
/// measurement is spelled.
///
/// One function rather than two format strings, so the measured and absent
/// arms cannot drift into stating the criterion two different ways.
///
/// A disclosed item carries its disclosure HERE, after the verdict, in bold
/// — not only in the page's closing gloss. A reader who scrolls to a metric,
/// reads `FLAT` and moves on must not be able to miss that this particular
/// claim was not a blind test; a pointer they have to follow is a pointer
/// most readers will not follow.
fn claim_sentence(item: &ScoredItem, measured: &str) -> String {
    format!(
        "*{}* (`{}` `{}`; {}). Predicted {}; measured {measured}. {}.{}",
        item.title,
        item.corpus,
        item.id,
        item.source,
        item.criterion.prose(),
        item.verdict.shouted(),
        match &item.disclosure {
            Some(reason) => format!(" **{reason}**"),
            None => String::new(),
        }
    )
}

/// Every claim line `metric` is entitled to, in corpus order, or the empty
/// string when no frozen corpus scores it.
///
/// The empty string is the common case by a wide margin — the founding
/// corpus scores four of the census's several hundred columns — and it is
/// the load-bearing one: a metric no corpus scores must gain no claim, so
/// that a claim on a page always means a corpus really did freeze one.
fn render_claims(c: &Census, metric: &str, corpora: &[ScoredCorpus]) -> String {
    let mut out = String::new();
    for item in scored_for(corpora, metric) {
        let line = match item.measured(c) {
            Some(measured) => claim_line(item, measured),
            None => claim_line_unmeasured(item),
        };
        out.push_str(&format!(
            "\n{CLAIM_MARKER} {line} ([what this is]({CLAIMS_SECTION_ANCHOR}))\n"
        ));
    }
    out
}

/// Every scored item across every corpus that names `metric`, in corpus
/// order then item order.
fn scored_for<'a>(corpora: &'a [ScoredCorpus], metric: &str) -> Vec<&'a ScoredItem> {
    corpora
        .iter()
        .flat_map(|corpus| corpus.items.iter())
        .filter(|item| item.statistic == metric)
        .collect()
}

/// The page's closing section: what a frozen claim is, and the provenance
/// of every corpus that actually scored one of THIS page's metrics.
///
/// Empty when the page carries no claim, which keeps the gloss where the
/// thing it glosses is. A corpus that scores nothing on this domain
/// contributes nothing here even though the caller was handed it — the
/// section is derived from what the page shows, not from what was loaded.
fn render_claims_section(cols: &[&Column], corpora: &[ScoredCorpus]) -> String {
    let scoring: Vec<&ScoredCorpus> = corpora
        .iter()
        .filter(|corpus| {
            corpus
                .items
                .iter()
                .any(|item| cols.iter().any(|col| col.name == item.statistic))
        })
        .collect();
    if scoring.is_empty() {
        return String::new();
    }
    // Blindness is stated over the items this PAGE shows, not over every
    // item loaded: a disclosure on a metric that lives on another domain's
    // page is that page's business, and naming it here would send a reader
    // looking for a claim line that is not present.
    let shown: Vec<&ScoredItem> = scoring
        .iter()
        .flat_map(|corpus| corpus.items.iter())
        .filter(|item| cols.iter().any(|col| col.name == item.statistic))
        .collect();
    let mut out = format!(
        "## {CLAIMS_SECTION_TITLE}\n\n{CLAIMS_PREAMBLE}\n\n{}\n\n",
        blindness_sentence(&shown)
    );
    for corpus in scoring {
        out.push_str(&format!(
            "### `{}`\n\nFrozen corpus: `{}`\n\n{}\n\n",
            corpus.corpus, corpus.path, corpus.provenance
        ));
    }
    out
}

/// Render one metric's heading, doc line, and statistics block, dispatching
/// on `col.kind` (spec §4.2's per-kind table). The doc line is redacted of
/// any registry citation first — see [`redact_registry_citations`].
fn render_metric(c: &Census, col: &Column, corpora: &[ScoredCorpus]) -> String {
    let stats = match col.kind.as_str() {
        "numeric" | "integer" => render_numeric_stats(c, col),
        "categorical" => render_categorical_stats(c, col),
        "flag" => render_flag_stats(c, col),
        other => format!("(unrecognized metric kind {other:?}; cannot render statistics)\n"),
    };
    format!(
        "### `{}`\n\n{}\n\n{stats}{}\n",
        col.name,
        redact_registry_citations(&col.doc),
        render_claims(c, &col.name, corpora)
    )
}

/// Group findings by the metric they concern, preserving each metric's
/// internal detector order (the input is already sorted by `(detector,
/// metric)` — see `detect::detect` — so scanning it in order and pushing
/// into per-metric buckets keeps every bucket detector-ascending too,
/// without a second sort).
fn group_by_metric<'a>(findings: &[&'a Finding]) -> BTreeMap<&'a str, Vec<&'a Finding>> {
    let mut groups: BTreeMap<&str, Vec<&Finding>> = BTreeMap::new();
    for f in findings {
        groups.entry(f.metric.as_str()).or_default().push(f);
    }
    groups
}

/// The domain a census metric belongs to, or `None` if `metric` is not a
/// column at all (D8's findings name a `domains/` crate, not a metric, so
/// this correctly excludes them from every per-domain rollup).
fn domain_of<'a>(c: &'a Census, metric: &str) -> Option<&'a str> {
    c.columns
        .iter()
        .find(|col| col.name == metric)
        .map(|col| col.domain.as_str())
}

/// Render the "findings" section of one domain's page: every finding whose
/// metric belongs to `domain`, grouped one heading per metric (never one
/// heading per detector-metric pair — spec §4.4's D2/D4 overlap is exactly
/// what this collapsing exists to hide from a reader counting problems).
fn render_findings_for_domain(c: &Census, domain: &str, findings: &[Finding]) -> String {
    let relevant: Vec<&Finding> = findings
        .iter()
        .filter(|f| domain_of(c, &f.metric) == Some(domain))
        .collect();
    if relevant.is_empty() {
        return "No weaknesses detected for this domain in the current census.\n".to_string();
    }
    let mut out = String::new();
    for (metric, group) in group_by_metric(&relevant) {
        out.push_str(&format!("### `{metric}`\n\n"));
        for f in group {
            out.push_str(&format!("- **{}**: {}\n", f.detector, f.detail));
        }
        out.push('\n');
    }
    out
}

/// Render one domain's Book page: header, framing sentence, every metric in
/// the domain (or a "no metrics" notice if it has none — spec §4.6a: a gap
/// in the world is rendered, never quietly patched over), then its
/// findings.
///
/// `corpora` is every frozen corpus in `regularities/`, each carrying the
/// items it scores across all domains; a metric takes the ones naming it and
/// the rest render nothing. A corpus that scores nothing on this page also
/// contributes no provenance block. Passing an empty slice is legal and
/// yields the survey exactly as it read before spec §5.
/// type-audit: bare-ok(identifier-text: domain), bare-ok(artifact: return)
pub fn render_domain(
    c: &Census,
    domain: &str,
    findings: &[Finding],
    corpora: &[ScoredCorpus],
) -> String {
    let cols = domain_columns(c, domain);
    let mut out = format!(
        "{HEADER}\n\n# {} — The Domesday\n\n{}\n\n",
        title_of(domain),
        framing_line(domain)
    );

    if cols.is_empty() {
        out.push_str(
            "This domain has no metrics in the committed census. That is a gap in the \
             world the survey measures, not an error in the survey itself (spec §4.6a) \
             — see [the index](./index.md) for whether an unmeasured `domains/` crate \
             (detector D8) explains it.\n",
        );
        return out;
    }

    out.push_str("## Metrics\n\n");
    for col in &cols {
        out.push_str(&render_metric(c, col, corpora));
    }

    out.push_str("## Weaknesses found here\n\n");
    out.push_str(&render_findings_for_domain(c, domain, findings));

    out.push_str(&render_claims_section(&cols, corpora));

    out
}

/// Render the Domesday's index page: an overview table of every domain
/// (metric count and finding count, both computed, each linking to its
/// page), D8's crate-coverage findings (which name a `domains/` crate, not
/// a metric, so they have no per-domain home), and a per-detector tally.
/// type-audit: bare-ok(artifact: return)
pub fn render_index(c: &Census, findings: &[Finding]) -> String {
    let total_metrics: usize = c
        .columns
        .iter()
        .filter(|col| !col.domain.is_empty())
        .count();
    let mut out = format!(
        "{HEADER}\n\n# The Domesday\n\n\
         A generated survey of the committed census: what Hornvale's worlds actually \
         produce, and where the instrument finds them wanting.\n\n\
         {} worlds, {total_metrics} metrics across {} domains.\n\n",
        c.rows.len(),
        domains().len(),
    );

    out.push_str("## Domains\n\n| domain | metrics | weaknesses | |\n|---|---|---|---|\n");
    for domain in domains() {
        let n_metrics = domain_columns(c, domain).len();
        let n_findings = findings
            .iter()
            .filter(|f| domain_of(c, &f.metric) == Some(domain))
            .count();
        out.push_str(&format!(
            "| {} | {n_metrics} | {n_findings} | [page](./{domain}.md) |\n",
            title_of(domain)
        ));
    }

    out.push_str("\n## Crate coverage (D8)\n\n");
    out.push_str(
        "A `domains/` crate no census metric measures at all is a gap in the world, not \
         a per-metric finding, so it has no domain page of its own.\n\n",
    );
    let d8: Vec<&Finding> = findings.iter().filter(|f| f.detector == "D8").collect();
    if d8.is_empty() {
        out.push_str("Every `domains/` crate is measured by at least one census metric.\n");
    } else {
        for f in d8 {
            out.push_str(&format!("- `{}`: {}\n", f.metric, f.detail));
        }
    }

    out.push_str("\n## Findings by detector\n\n");
    out.push_str(
        "Raw firing counts, not distinct metrics: D2's hits are a subset of D4's by \
         construction (a frozen metric's median trivially equals its min and max), and \
         D3 and D4 also overlap. Each domain page groups its own findings by metric so \
         no reader counts the same metric twice. A detector that found nothing still \
         gets a row, reading `0`: silence here would mean both *this detector does not \
         exist* and *this detector ran and every claim it checks held*, and those two \
         must not share a channel.\n\n",
    );
    out.push_str("| detector | findings |\n|---|---|\n");
    // The UNION of the declared roster and the names actually observed, in
    // ascending name order — the same order `detect` sorts findings in, so
    // the table reads in the same sequence as everything downstream of it.
    //
    // Neither half alone is safe, and each failed in turn. A frozen
    // `["D1".."D8"]` literal dropped a *renamed* detector's real count. Its
    // repair — deriving the roster from the findings — dropped every
    // detector that fired NOTHING, so `D7 | 0` disappeared and this
    // campaign's `D5 direction | 0` null was never published at all, making
    // an absent row mean either "does not exist" or "found nothing"
    // (decision 0119 forbids exactly that sharing). The union degrades in
    // the one safe direction: a stale [`DECLARED_DETECTORS`] can omit a
    // ZERO row, never a real finding.
    let mut detectors: BTreeSet<&str> = DECLARED_DETECTORS.iter().copied().collect();
    detectors.extend(findings.iter().map(|f| f.detector));
    for detector in detectors {
        let n = findings.iter().filter(|f| f.detector == detector).count();
        out.push_str(&format!("| {detector} | {n} |\n"));
    }

    out
}

/// Render the anomaly report's single committed page (spec §3.6): a header
/// naming the three frozen selection bars as selection bars, not
/// significance claims, the top [`TOP_WORLDS`] worlds with their flagged
/// metrics/depths/values, and the full exclusion roster with reasons.
///
/// `ranked` is expected to already be ordered most-anomalous-first (as
/// [`crate::domesday::anomaly::rank`] returns it) — this function only
/// takes the first [`TOP_WORLDS`] of whatever order it is given, it does
/// not re-sort.
/// type-audit: bare-ok(artifact: return), bare-ok(artifact: excluded)
pub fn render_anomalies(
    c: &Census,
    ranked: &[WorldAnomaly],
    excluded: &[(String, String)],
) -> String {
    let mut out = format!(
        "{ANOMALIES_HEADER}\n\n# Anomalies — The Domesday's transpose\n\n\
         Per world, which of its metric values sit deep in the tail of that column's \
         distribution across the {} census worlds — so a world volunteers its own \
         outliers instead of waiting for someone to ask the right question. This is \
         a pure read over the same committed census the rest of the Domesday reads, \
         and never builds a world (spec §3.1).\n\n\
         **Frozen selection bars, not significance claims** (spec §3.4 — the same \
         precedent D1's 80% share bar and D3's 5% IQR bar carry, decision 0016): a \
         column counts toward a world's score once its two-sided tail depth is at or \
         below **{TAIL_DEPTH_BAR}**; each world's published report below carries its \
         **{REPORT_SIZE}** columns of smallest tail depth; this page publishes the top \
         **{TOP_WORLDS}** worlds by score. None of the three is retuned after seeing a \
         result.\n\n",
        c.rows.len(),
    );

    out.push_str("## Top worlds\n\n");
    if ranked.is_empty() {
        out.push_str("No worlds ranked — the evaluable surface is empty.\n\n");
    }
    for wa in ranked.iter().take(TOP_WORLDS) {
        // `wa.score` is the world's TRUE, uncapped count of evaluable columns
        // at or below TAIL_DEPTH_BAR (F2) — never re-derived by counting the
        // REPORT_SIZE-capped `flags` list below, which saturates at
        // REPORT_SIZE and reads as a tie between every world whose true
        // score exceeds it.
        out.push_str(&format!(
            "### Seed `{}`\n\nScore **{}**: this many evaluable columns clear the \
             {TAIL_DEPTH_BAR} tail-depth bar (its {} closest-to-extreme columns are \
             listed below).\n\n| metric | depth | value |\n|---|---|---|\n",
            wa.seed,
            wa.score,
            wa.flags.len(),
        ));
        for flag in &wa.flags {
            out.push_str(&format!(
                "| `{}` | {} | {} |\n",
                flag.metric,
                quantize(flag.depth),
                quantize(flag.value)
            ));
        }
        out.push('\n');
    }

    out.push_str("## Excluded columns\n\n");
    out.push_str(
        "Every numeric/integer census column with a domain and a role that this report \
         did NOT score, and why (spec §3.3). Categorical and flag columns carry no \
         ordering — a tail is an ordering — so they are never candidates at all, and are \
         not listed here (see the module doc on `anomaly.rs`).\n\n",
    );
    if excluded.is_empty() {
        out.push_str("No columns excluded.\n");
    } else {
        out.push_str("| metric | reason |\n|---|---|\n");
        for (metric, reason) in excluded {
            out.push_str(&format!("| `{metric}` | {reason} |\n"));
        }
    }

    out
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::domesday::census::repo_root;
    use crate::domesday::comparators::{load_comparators, load_expectations};
    use crate::domesday::detect::detect;

    fn census() -> Census {
        crate::domesday::census::load(&repo_root().join("book/src/laboratory/generated/the-census"))
            .expect("the committed census loads")
    }

    #[test]
    fn a_domain_page_carries_the_generated_header_and_only_computed_numbers() {
        let c = census();
        let page = render_domain(&c, "climate", &[], &[]);
        assert!(
            page.starts_with("<!-- GENERATED FILE — do not edit."),
            "header required"
        );
        assert!(
            page.contains("mean-land-temperature-c"),
            "climate metrics appear"
        );
        // THE POINT IS THE PROVENANCE, NOT THE VALUE: this substring must be
        // a number the page COMPUTED from the census, so that a page which
        // restated a hardcoded figure would fail. It therefore tracks the
        // census and moves whenever the census does.
        //
        // THE GLASSHOUSE (Stage B, k = 0.30): this used to pin a
        // hand-transcribed median (`-3.6`). That proved only that one day's
        // census was still present. Derive the expected rendered value from
        // the same census passed to the renderer so the witness remains about
        // computed-vs-authored output across legitimate census refreshes.
        let median = numeric(&c, "mean-land-temperature-c")
            .expect("mean-land-temperature-c has census values")
            .median;
        assert!(
            page.contains(&quantize(median).to_string()),
            "the median is read, not restated"
        );
    }

    #[test]
    fn every_domain_renders_and_none_is_empty() {
        let c = census();
        // The full twelve-domain roster (spec §4.1), not the eleven-domain
        // list the brief's own draft test carried — `demography` was
        // missing there, which the "what must render" requirement (all
        // twelve, `demography` named explicitly) and `domains()` above both
        // contradict. Fixed here rather than transcribed.
        for d in domains() {
            let page = render_domain(&c, d, &[], &[]);
            assert!(page.len() > 200, "{d} rendered nothing at all");
            // A domain with NO metrics must still render, announcing the gap.
            // An absence that announces itself is a finding; a missing
            // chapter is silence. Do not "fix" the gap by assigning it
            // metrics — render it (campaign principle, Nathan).
            if c.columns.iter().all(|col| col.domain != d) {
                assert!(
                    page.contains("no metrics"),
                    "{d} has no metrics and must SAY SO on its page"
                );
            }
        }
    }

    #[test]
    fn no_domain_is_currently_empty() {
        // Recorded explicitly rather than left implicit in the loop above:
        // Task 1's reclassification (spec §4.6a) moved hydrology's twelve
        // misfiled metrics out from under terrain, so on the census
        // committed today every one of the twelve domains has at least one
        // metric. If this ever regresses, the loop above still exercises
        // the "no metrics" branch correctly — this test only pins today's
        // state so a silent future regression is visible here too.
        let c = census();
        for d in domains() {
            assert!(
                c.columns.iter().any(|col| col.domain == d),
                "{d} has zero metrics on the committed census"
            );
        }
    }

    #[test]
    fn a_domain_with_no_metrics_renders_the_no_metrics_notice_verbatim() {
        // Fix round 1/5 (review finding): `no_domain_is_currently_empty`
        // proves `cols.is_empty()` (render.rs's §4.6a branch) is UNREACHED
        // by every real call in this test module -- all twelve domains have
        // metrics on the live census, so nothing here ever exercised the
        // "gap in the world is rendered" branch itself. A synthetic Census
        // whose columns cover every domain EXCEPT `hydrology` reaches it
        // directly, and asserts on the actual emitted sentence (not merely
        // that the page is non-empty -- a length check alone would pass
        // just as well if this branch were deleted and replaced with any
        // other non-trivial text).
        fn col(name: &str, domain: &str) -> Column {
            Column {
                name: name.to_string(),
                kind: "flag".to_string(),
                doc: "a synthetic metric".to_string(),
                domain: domain.to_string(),
                role: "descriptor".to_string(),
            }
        }
        let other_domains: Vec<&str> = domains()
            .into_iter()
            .filter(|d| *d != "hydrology")
            .collect();
        assert_eq!(
            other_domains.len(),
            11,
            "sanity: every domain but hydrology"
        );
        let columns: Vec<Column> = other_domains
            .iter()
            .map(|d| col(&format!("{d}-metric"), d))
            .collect();
        let c = Census {
            columns,
            rows: vec![],
        };

        // Every other domain still renders its (empty-of-worlds but
        // present) metric section, never the "no metrics" branch.
        for d in &other_domains {
            let page = render_domain(&c, d, &[], &[]);
            assert!(
                !page.contains("no metrics"),
                "{d} has a column and must not claim it has none: {page}"
            );
        }

        let page = render_domain(&c, "hydrology", &[], &[]);
        assert!(
            page.starts_with(HEADER),
            "the no-metrics page must still carry the generated header: {page}"
        );
        assert!(
            page.contains(&format!("# {} — The Domesday", title_of("hydrology"))),
            "the no-metrics page must still carry its title: {page}"
        );
        assert!(
            page.contains(framing_line("hydrology")),
            "the no-metrics page must still carry its one authored framing \
             sentence: {page}"
        );
        // The exact sentence render_domain's `cols.is_empty()` branch
        // emits -- asserted verbatim, not just "contains no metrics",
        // so a future rewording is a deliberate edit here too.
        assert_eq!(
            page,
            format!(
                "{HEADER}\n\n# Hydrology — The Domesday\n\n{}\n\n\
                 This domain has no metrics in the committed census. That is a gap in the \
                 world the survey measures, not an error in the survey itself (spec §4.6a) \
                 — see [the index](./index.md) for whether an unmeasured `domains/` crate \
                 (detector D8) explains it.\n",
                framing_line("hydrology")
            ),
            "the no-metrics page must match its emitted text exactly: {page}"
        );
    }

    #[test]
    fn a_metric_that_trips_two_detectors_is_grouped_under_one_heading_not_two() {
        // reproductive-tempo-goblin is frozen (D2) and, by construction,
        // also at-rail (D4) -- see S2b and detect.rs's D2-subset-of-D4 doc.
        // The rendered page must show it once, with both detectors listed
        // underneath, not as two separate `###` headings for the same
        // metric.
        let c = census();
        let cmps = load_comparators(&repo_root().join("studies/comparators.json")).unwrap();
        let exps = load_expectations(&repo_root().join("studies/expectations.json")).unwrap();
        let findings = detect(&c, &cmps, &exps);
        let page = render_domain(&c, "biology", &findings, &[]);
        // The metrics section and the findings section each get their own
        // `### <metric>` heading by design (a statistics table and a
        // weakness list are different things), so the page-wide count is 2.
        // What must NOT happen is two headings for the SAME metric within
        // the findings section alone -- that would be D2 and D4 rendered as
        // two independent-looking problems instead of one grouped finding.
        let (_, findings_section) = page
            .split_once("## Weaknesses found here")
            .expect("findings section present");
        let heading = "### `reproductive-tempo-goblin`";
        assert_eq!(
            findings_section.matches(heading).count(),
            1,
            "the metric heading must appear exactly once within the findings section"
        );
        assert!(
            findings_section.contains("**D2**"),
            "D2 must be listed under it"
        );
        assert!(
            findings_section.contains("**D4**"),
            "D4 must be listed under it"
        );
    }

    #[test]
    fn d8_findings_appear_on_the_index_and_not_on_any_domain_page() {
        let c = census();
        let cmps = load_comparators(&repo_root().join("studies/comparators.json")).unwrap();
        let exps = load_expectations(&repo_root().join("studies/expectations.json")).unwrap();
        let findings = detect(&c, &cmps, &exps);

        let index = render_index(&c, &findings);
        assert!(
            index.contains("alchemy"),
            "D8's alchemy finding on the index"
        );
        assert!(
            index.contains("paleoclimate"),
            "D8's paleoclimate finding on the index"
        );

        for d in domains() {
            let page = render_domain(&c, d, &findings, &[]);
            assert!(
                !page.contains("no census metric measures any quantity"),
                "{d}'s page must not carry a D8 crate-coverage finding"
            );
        }
    }

    #[test]
    fn a_novel_detector_name_reaches_the_rendered_index() {
        // render_index's "Findings by detector" table once iterated a
        // hardcoded `["D1".."D8"]` literal (the same class of bug D5's
        // rename exposed): a detector whose name the code never anticipated
        // would fire, be counted by `detect()`, and then be silently
        // dropped from the published table -- an undercount in the
        // artifact whose whole claim is that it computes rather than
        // restates. The observed half of `render_index`'s union is what
        // protects this: the roster it declares is a floor, never a
        // filter, so a name it has never heard of still gets its true
        // count. Fails against the old fixed-eight-detector loop, which
        // would render no row at all for "D9 test".
        let c = census();
        let findings = vec![Finding {
            detector: "D9 test",
            metric: "mean-land-temperature-c".to_string(),
            detail: "a detector this renderer was never told about".to_string(),
        }];
        let index = render_index(&c, &findings);
        assert!(
            index.contains("| D9 test | 1 |"),
            "a novel detector name must get its own row in the findings-by-detector \
             table, not be silently absent: {index}"
        );
    }

    #[test]
    fn a_detector_that_fired_nothing_still_gets_a_zero_row() {
        // The other half of the union, and the defect the observed-only
        // roster introduced: a detector that fires no finding must still
        // appear, with a count of 0. Absence would otherwise carry two
        // meanings at once -- "no such detector" and "this detector ran and
        // found nothing" -- which is the conflation decision 0119 forbids.
        // On the committed census this is not hypothetical: D7 fires
        // nothing, and `D5 direction` -- the campaign's headline null --
        // fires nothing either, so both vanished from the published index.
        //
        // The findings passed in are a single D1 hit rather than none at
        // all, so the test also proves the two halves coexist: a detector
        // that DID fire keeps its true count in the same table.
        let c = census();
        let findings = vec![Finding {
            detector: "D1",
            metric: "dominant-land-biome".to_string(),
            detail: "a synthetic degeneracy".to_string(),
        }];
        let index = render_index(&c, &findings);
        assert!(
            index.contains("| D1 | 1 |"),
            "a detector that fired must still show its true count: {index}"
        );
        for detector in DECLARED_DETECTORS.iter().filter(|d| **d != "D1") {
            assert!(
                index.contains(&format!("| {detector} | 0 |")),
                "{detector} fired nothing and must be published as a zero row, not \
                 omitted: {index}"
            );
        }
    }

    #[test]
    fn the_index_carries_the_generated_header_and_every_domain_link() {
        let c = census();
        let index = render_index(&c, &[]);
        assert!(index.starts_with("<!-- GENERATED FILE — do not edit."));
        for d in domains() {
            assert!(
                index.contains(&format!("(./{d}.md)")),
                "index must link to {d}'s page"
            );
        }
    }

    #[test]
    fn a_flag_metric_always_renders_both_true_and_false_rows() {
        // Several categorical/flag metrics are ~100% single-valued by
        // design (invariants, §4.1a) -- the rendered table must still show
        // the zero-count row rather than silently omitting it, matching
        // spec §4.2's "both rows always present."
        let c = census();
        let flag_col = c
            .columns
            .iter()
            .find(|col| col.kind == "flag")
            .expect("at least one flag metric exists");
        let page = render_metric(&c, flag_col, &[]);
        assert!(page.contains("`true`"), "true row must render");
        assert!(page.contains("`false`"), "false row must render");
    }

    #[test]
    fn an_integer_metric_reports_its_exact_mode() {
        // The live census's only `kind == "integer"` column is the
        // structural `seed` (excluded from every domain by construction —
        // it carries no `domain`), so this branch is otherwise dead code on
        // real data today. A synthetic column exercises it directly rather
        // than leaving spec §4.2's "integer: as numeric, plus exact mode"
        // untested until some future metric happens to be declared integer.
        fn make_col() -> Column {
            Column {
                name: "m".to_string(),
                kind: "integer".to_string(),
                doc: "a synthetic integer metric".to_string(),
                domain: "climate".to_string(),
                role: "descriptor".to_string(),
            }
        }
        let c = Census {
            columns: vec![make_col()],
            rows: ["1", "2", "2", "2", "3"]
                .iter()
                .map(|v| BTreeMap::from([("m".to_string(), (*v).to_string())]))
                .collect(),
        };
        let page = render_metric(&c, &make_col(), &[]);
        assert!(
            page.contains("mode: `2` (3 worlds)"),
            "exact mode must be reported: {page}"
        );
    }

    // --- redact_registry_citations: the seven live citations, verbatim ---
    // (spec §4's "the Book carries no registry IDs outside the Frontier
    // part" rule, enforced by cli/tests/docs_consistency.rs -- these seven
    // are the ones that actually fire on the committed schema.json today.)

    #[test]
    fn drops_a_trailing_parenthetical_citation_entirely() {
        let doc = "Peak-to-peak obliquity swing over one obliquity period (2\u{d7} the \
                    deep-time forcing amplitude, SKY-21); a moonless world keeps the full \
                    drawn wobble, a moon damps it";
        let got = redact_registry_citations(doc);
        assert!(!got.contains("SKY-21"), "citation must be gone: {got}");
        assert!(
            got.contains("obliquity period; a moonless world"),
            "the surrounding sentence must read cleanly: {got}"
        );
    }

    #[test]
    fn drops_a_short_citation_only_parenthetical_to_nothing() {
        let doc = "Mean distortion() over every placed culture's account (C4 LANG-41); \
                    Absent if no culture placed";
        let got = redact_registry_citations(doc);
        assert!(!got.contains("LANG-41"));
        assert_eq!(
            got,
            "Mean distortion() over every placed culture's account; Absent if no \
             culture placed"
        );
    }

    #[test]
    fn leaves_an_unrelated_paren_alone_while_dropping_the_citing_one() {
        let doc = "The OLS slope of log(population) on log(rank) (design spec \u{a7}5; \
                    full Zipf calibration is the later MAP-22 coexistence-stack \
                    campaign's job)";
        let got = redact_registry_citations(doc);
        assert!(!got.contains("MAP-22"), "citation must be gone: {got}");
        assert!(
            got.contains("log(population)") && got.contains("log(rank)"),
            "unrelated parens must survive untouched: {got}"
        );
    }

    #[test]
    fn drops_two_citations_in_one_parenthetical() {
        let doc = "Goblin's reproductive output on the r-K axis (BIO-2 spec \u{a7}4/CAP-2); \
                    Absent if goblin is off-roster";
        let got = redact_registry_citations(doc);
        assert!(!got.contains("BIO-2"));
        assert!(!got.contains("CAP-2"));
        assert_eq!(
            got,
            "Goblin's reproductive output on the r-K axis; Absent if goblin is off-roster"
        );
    }

    #[test]
    fn a_removed_parenthetical_directly_before_a_period_does_not_leave_a_dangling_space() {
        let doc = "...deliberately NOT tuned to a rank-size target (design spec \u{a7}5; \
                    full Zipf calibration is the later MAP-22 coexistence-stack \
                    campaign's job). Absent if fewer than 2 settlements exist";
        let got = redact_registry_citations(doc);
        assert!(!got.contains("MAP-22"));
        assert!(
            got.contains("target. Absent"),
            "must not leave a dangling space before the period: {got}"
        );
    }

    #[test]
    fn an_ellipsis_survives_the_space_dot_space_cleanup_untouched() {
        // The cleanup rule that fixes "target . Absent" is deliberately
        // " . " (space-DOT-space), not bare " ." -- an ellipsis like this
        // one has no space between its own dots, so it must be immune.
        let doc = "Goblin's reproductive output on the r-K axis, 0 (fast/prolific) ... 1 \
                    (slow/sparse) (BIO-2 spec \u{a7}4/CAP-2); Absent if goblin is off-roster \
                    or ametabolic";
        let got = redact_registry_citations(doc);
        assert!(
            got.contains("(fast/prolific) ... 1 (slow/sparse)"),
            "the ellipsis and its spacing must survive untouched: {got}"
        );
    }

    #[test]
    fn a_bare_citation_outside_any_paren_is_redacted_with_its_possessive() {
        // Not observed on the live census (every citation there sits inside
        // a parenthetical), but the fallback path must not be dead code --
        // a future doc string need not follow the parenthetical convention.
        let doc = "MEM-7's handle names this pattern directly";
        let got = redact_registry_citations(doc);
        assert!(!got.contains("MEM-7"), "bare citation must be gone: {got}");
        assert!(
            !got.contains("'s"),
            "the dangling possessive must go with its subject: {got}"
        );
    }

    #[test]
    fn no_domain_page_or_the_index_cites_the_registry() {
        // The direct, end-to-end guarantee: render every domain page (the
        // only place `col.doc` -- the leak vector -- gets embedded) against
        // the live census and its real findings, and confirm none of the
        // seven known citations survive. This is what actually protects
        // `make gate`'s docs_consistency check; the unit tests above pin
        // the redaction mechanism in isolation.
        let c = census();
        let cmps = load_comparators(&repo_root().join("studies/comparators.json")).unwrap();
        let exps = load_expectations(&repo_root().join("studies/expectations.json")).unwrap();
        let findings = detect(&c, &cmps, &exps);
        for d in domains() {
            let page = render_domain(&c, d, &findings, &[]);
            for citation in [
                "SKY-21", "MAP-10", "MAP-22", "BIO-2", "CAP-2", "MEM-7", "LANG-41",
            ] {
                assert!(
                    !page.contains(citation),
                    "{d}'s page must not cite the registry ({citation} leaked)"
                );
            }
        }
    }

    // --- render_anomalies ---

    #[test]
    fn the_anomalies_page_carries_the_generated_header_and_frozen_constants() {
        let c = census();
        let ranked = crate::domesday::anomaly::rank(&c);
        let (_, excluded) = crate::domesday::anomaly::evaluable_columns(&c);
        let page = render_anomalies(&c, &ranked, &excluded);
        assert!(
            page.starts_with(
                "<!-- GENERATED FILE — do not edit. Regenerate with \
                               `hornvale lab anomalies`."
            ),
            "header required, and must name the ANOMALIES command (F3), not \
             `hornvale lab domesday`: {page}"
        );
        assert!(page.contains("0.01"), "the tail-depth bar must be named");
        // F6: a bare `page.contains("10")`/`("25")` matches any digit pair on
        // a page dense with numbers (a metric value, a depth). Match the
        // exact bolded substrings the format string actually emits instead.
        assert!(
            page.contains("**10** columns of smallest tail depth"),
            "the report size must be named as the constant it is: {page}"
        );
        assert!(
            page.contains("top **25** worlds by score"),
            "the top-worlds cap must be named as the constant it is: {page}"
        );
        assert!(
            page.contains("not significance claims"),
            "the header must say these are selection bars, not significance claims"
        );
    }

    #[test]
    fn the_anomalies_page_lists_the_top_worlds_ranked_first() {
        let c = census();
        let ranked = crate::domesday::anomaly::rank(&c);
        let page = render_anomalies(&c, &ranked, &[]);
        let top_seed = ranked.first().expect("at least one ranked world").seed;
        assert!(
            page.contains(&format!("### Seed `{top_seed}`")),
            "the top-ranked world's seed must appear on the page: {page}"
        );
    }

    #[test]
    fn the_anomalies_page_only_publishes_up_to_top_worlds_worlds() {
        let c = census();
        let ranked = crate::domesday::anomaly::rank(&c);
        let page = render_anomalies(&c, &ranked, &[]);
        let seed_headings = page.matches("### Seed `").count();
        assert_eq!(
            seed_headings,
            crate::domesday::anomaly::TOP_WORLDS,
            "exactly TOP_WORLDS worlds must be published, not the full ranked list"
        );
    }

    #[test]
    fn the_anomalies_page_carries_the_full_exclusion_roster_with_reasons() {
        let c = census();
        let (_, excluded) = crate::domesday::anomaly::evaluable_columns(&c);
        assert!(!excluded.is_empty(), "sanity: some columns are excluded");
        let page = render_anomalies(&c, &[], &excluded);
        for (metric, reason) in &excluded {
            assert!(
                page.contains(&format!("`{metric}`")) && page.contains(reason.as_str()),
                "{metric}'s exclusion reason must be published verbatim: {reason}"
            );
        }
    }

    #[test]
    fn an_empty_exclusion_roster_says_so_rather_than_rendering_an_empty_table() {
        let c = census();
        let page = render_anomalies(&c, &[], &[]);
        assert!(page.contains("No columns excluded."));
    }

    /// F2: the printed score is the world's TRUE, uncapped count of columns
    /// at or below `TAIL_DEPTH_BAR`, not the `REPORT_SIZE`-capped `flags`
    /// count. Reviewer's finding: the top twelve real worlds' true scores
    /// are 21, 19, 18, 17, 16, 16, 14, 11, 10, 10, 10, 10 — the top eight of
    /// those exceed `REPORT_SIZE` (10), so counting `flags` instead would
    /// have printed "10" for every one of them, an undetectable twelve-way
    /// tie. Assert directly against `WorldAnomaly::score` (computed by
    /// `rank`, not re-derived here), on the real top-ranked world.
    #[test]
    fn the_anomalies_page_prints_the_true_uncapped_score_not_the_capped_flag_count() {
        let c = census();
        let ranked = crate::domesday::anomaly::rank(&c);
        let top = ranked.first().expect("at least one ranked world");
        assert!(
            top.score > crate::domesday::anomaly::REPORT_SIZE,
            "sanity: the top-ranked real world's true score must exceed REPORT_SIZE \
             for this test to actually exercise the distinction (score was {})",
            top.score
        );
        let page = render_anomalies(&c, &ranked, &[]);
        assert!(
            page.contains(&format!("Score **{}**:", top.score)),
            "the page must print the true score ({}), not flags.len() ({}): {page}",
            top.score,
            top.flags.len()
        );
    }
}

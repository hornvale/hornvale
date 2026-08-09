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
//! **Grouping (spec §4.4's D2 ⊆ D4 observation):** a metric that trips more
//! than one detector is rendered ONCE, as one heading under which every
//! detector that fired is listed — never as several independent-looking
//! findings under the same name. [`group_by_metric`] is the seam that
//! enforces this for both the per-domain findings section and could be
//! reused by an index-level rollup if one is ever added.

use crate::domesday::census::{Census, Column};
use crate::domesday::detect::Finding;
use crate::domesday::stats::{categorical, numeric};
use crate::metrics::Domain;
use hornvale_kernel::quantize;
use std::collections::BTreeMap;

/// The header every generated Domesday page opens with, matching the
/// project's existing generated-page convention (`book/src/reference/`,
/// `book/src/laboratory/generated/`).
const HEADER: &str =
    "<!-- GENERATED FILE — do not edit. Regenerate with `hornvale lab domesday`. -->";

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

/// Render one metric's heading, doc line, and statistics block, dispatching
/// on `col.kind` (spec §4.2's per-kind table). The doc line is redacted of
/// any registry citation first — see [`redact_registry_citations`].
fn render_metric(c: &Census, col: &Column) -> String {
    let stats = match col.kind.as_str() {
        "numeric" | "integer" => render_numeric_stats(c, col),
        "categorical" => render_categorical_stats(c, col),
        "flag" => render_flag_stats(c, col),
        other => format!("(unrecognized metric kind {other:?}; cannot render statistics)\n"),
    };
    format!(
        "### `{}`\n\n{}\n\n{stats}\n",
        col.name,
        redact_registry_citations(&col.doc)
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
/// type-audit: bare-ok(identifier-text: domain), bare-ok(artifact: return)
pub fn render_domain(c: &Census, domain: &str, findings: &[Finding]) -> String {
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
        out.push_str(&render_metric(c, col));
    }

    out.push_str("## Weaknesses found here\n\n");
    out.push_str(&render_findings_for_domain(c, domain, findings));

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
         no reader counts the same metric twice.\n\n",
    );
    out.push_str("| detector | findings |\n|---|---|\n");
    // Derived from the findings themselves, not a hardcoded roster: a
    // detector that fires nothing has no row, which is correct (an empty
    // row would assert a measurement that was never taken), and a detector
    // whose name changes (as D5 split into "D5 direction"/"D5 strength")
    // cannot silently vanish from a frozen literal this table forgot to
    // update. This is the fourth frozen roster this programme has found —
    // close the class, not the instance.
    let mut detectors: Vec<&str> = findings.iter().map(|f| f.detector).collect();
    detectors.sort_unstable();
    detectors.dedup();
    for detector in detectors {
        let n = findings.iter().filter(|f| f.detector == detector).count();
        out.push_str(&format!("| {detector} | {n} |\n"));
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
        let page = render_domain(&census(), "climate", &[]);
        assert!(
            page.starts_with("<!-- GENERATED FILE — do not edit."),
            "header required"
        );
        assert!(
            page.contains("mean-land-temperature-c"),
            "climate metrics appear"
        );
        assert!(page.contains("-11.9"), "the median is read, not restated");
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
            let page = render_domain(&c, d, &[]);
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
            let page = render_domain(&c, d, &[]);
            assert!(
                !page.contains("no metrics"),
                "{d} has a column and must not claim it has none: {page}"
            );
        }

        let page = render_domain(&c, "hydrology", &[]);
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
        let page = render_domain(&c, "biology", &findings);
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
            let page = render_domain(&c, d, &findings);
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
        // restates. `render_index` must derive its roster from the
        // findings themselves, so this passes against the fix and would
        // fail against the old fixed-eight-detector loop (which would
        // render no row at all for "D9 test").
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
        let page = render_metric(&c, flag_col);
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
        let page = render_metric(&c, &make_col());
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
                    or Ametabolic";
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
            let page = render_domain(&c, d, &findings);
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
}

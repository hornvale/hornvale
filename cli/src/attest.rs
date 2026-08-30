//! The Attestation's payoff: diff what the roster and the declared-path list
//! say **should** appear in the timing ledger against what the ledger
//! actually records — in **both directions** — and diff what each declared
//! author's own rows say about when it last ran.
//!
//! This module reads no filesystem itself; [`attest_report`] is pure over
//! the three inputs, so every test below builds them as string literals.
//! `cli/src/main.rs`'s `attest` subcommand is the only caller that reads the
//! real files (`docs/timings.md`, `scripts/lane-sets.tsv`,
//! `docs/generated-paths.txt`).
//!
//! # Two properties of the ledger this module must not claim past
//!
//! **A row has no exit code.** `docs/timings.md`'s columns are `when (UTC) |
//! label | wall_s | user_s | sys_s | cpu_ratio | waited_s | commit | branch |
//! host | cores` — there is no `rc`. A row witnesses that a phase *ran*,
//! never that it *passed*; the pass/fail verdict lives in the queue's own
//! state (`sluice-run.sh`'s `record()`), not here. Every finding below is
//! therefore a claim about **coverage** ("this phase left no trace"), never
//! about health ("this phase failed") — a job that ran every owed phase and
//! failed all of them reports exactly as clean as one that passed.
//!
//! **A job is identified by its rows' adjacency, not a job id.** The ledger
//! carries no column naming which rows belong to the same chamber run. This
//! reader groups rows into a "job" by scanning the ledger in file order (the
//! ledger is append-only, so file order is chronological) and collecting
//! each **maximal run of consecutive rows whose label starts with
//! `sluice:`** — a non-`sluice:` row, or the end of the file, closes the
//! current job. This is a real instrument with real blind spots, named here
//! rather than left implicit:
//!
//! 1. **Stage jobs and merge jobs are indistinguishable except by `heavy`'s
//!    presence.** `scripts/sluice-run.sh`'s merge phase list is the stage
//!    list plus `heavy`; a job showing `{artifacts, outboard, gate,
//!    clients}` and nothing else is *either* a correctly-run stage gate *or*
//!    a merge that silently dropped `heavy`, and the ledger cannot tell them
//!    apart. So this reader never reports `heavy`'s absence from a single
//!    job as "owed but absent" at the job level — only the separate
//!    per-author check (below) can raise `heavy`, and it does so against the
//!    whole ledger, not one job.
//! 2. **Two jobs queued back-to-back with no other labelled row between them
//!    collapse into one apparent job.** Their phase sets union (a phase
//!    appearing twice reads identically to appearing once, since membership
//!    is checked as a set), so a genuinely missing phase in one of the two
//!    real jobs can be masked by the other supplying it. This is not a
//!    hypothetical: `docs/timings.md` contains a real run of three
//!    consecutive stage cycles (2026-08-29T13:28–14:32Z) with no
//!    intervening row.
//! 3. **The roster judging a historical row is always the roster passed in
//!    *today*, never the roster that governed the job when it ran.** A
//!    phase that legitimately belonged to a rung at the time — `seam-guard`
//!    ran inside `outboard`'s stage-rung dispatch before it was split onto
//!    its own `campaign`-rung row — reads as "present but unowed" once the
//!    roster moves on. `docs/timings.md`'s 2026-08-16 through 2026-08-19
//!    `sluice:seam-guard` rows are real examples of this, predating that
//!    split; they are not evidence of a defect at the time they were
//!    written.
//!
//! # The two checks, and why they are not the same shape
//!
//! **Job/phase coverage** (`owed_but_absent` / `present_but_unowed`) reads
//! only `timings` and `roster`; `declared` plays no part. It diffs, per job,
//! the roster's `stage`-rung set names (always owed, in both stage and merge
//! jobs) against the phases the job's own rows actually carry — in both
//! directions, so a phase silently dropped and a phase nothing rostered
//! surface the same way. This is the direction pair the campaign's own
//! thesis requires: a reader reporting only absences would be exactly the
//! one-sided check `cli/tests/suite/lane_sets.rs` was found lacking (The
//! Attestation, §1.1).
//!
//! **Author freshness** (`absent_authors`) reads `declared` and `timings`;
//! `roster` plays no part beyond having already named the three values
//! `declared` is allowed to use. For every distinct roster-author value
//! appearing in `declared` (`artifacts`, `census`, `heavy` — never
//! `none(<reason>)`, see below), it asks whether the ledger carries the
//! canonical evidence `docs/generated-paths.txt`'s own header names:
//! `sluice:<author>`. If it never does, the author is reported absent,
//! alongside how many declared rows name it and — as a courtesy, not a
//! second canonical source — the most recent **bare** `<author>` row (no
//! `sluice:` prefix) if one exists, since `census` and the pre-decision-0426
//! `heavy` are legitimately authored that way (`census-run.sh` and
//! `heavy-run.sh` both call `timed.sh` directly, never through
//! `sluice-run.sh`'s phase loop). This check has only one direction: a
//! roster set running when nothing in `declared` names it (`outboard`,
//! `clients`) is normal, not a defect, because `declared` was never meant to
//! name every roster author — only the ones that write a generated path.
//!
//! **`census` will always report absent under this check, permanently, and
//! that is a real fact about the repository rather than a defect in this
//! reader.** `docs/generated-paths.txt`'s own header claims "an author IS a
//! roster set name IS the suffix of a `sluice:<set>` ledger label, so the
//! whole chain joins with no separate mapping table" — true for `artifacts`
//! and (since decision 0426) `heavy`, but not for `census`: `census` is a
//! `campaign`-rung set, never a member of either of `sluice-run.sh`'s phase
//! lists, so `sluice:census` is not merely rare in the ledger, it cannot
//! occur at all. `census-run.sh` always attests itself with a bare `census`
//! label instead (`scripts/census-run.sh:211`). A `census`-authored row in
//! `declared` will therefore surface here every time this reader runs
//! against the real ledger — not a transient staleness signal, a standing
//! one, softened only by the `last_seen` courtesy field.
//!
//! `none(<reason>)` rows are **excluded before either check runs**. A
//! `none(<reason>)` author declares that no roster author writes the path at
//! all (Task 5, The Attestation), so looking it up in the ledger is a
//! category error, not a stricter check — it would report "absent" on every
//! single one, which is the exact failure this task's brief was written to
//! prevent. They are counted instead, as [`Report::declared_none_count`].

use std::collections::BTreeMap;
use std::collections::BTreeSet;

/// One roster row: `(name, rung, authors)`. `where` and `command`
/// (`scripts/lane-sets.tsv`'s other two columns) are not needed here.
struct RosterRow {
    name: String,
    rung: String,
}

/// Parse `scripts/lane-sets.tsv`'s content into rows, skipping comments and
/// blank lines, exactly as `cli/tests/suite/lane_sets.rs`'s `roster()` does.
///
/// Panics on a malformed row (wrong column count) — the same "a parse error
/// is not a reporting-worthy fact" stance `declared()` takes in
/// `cli/tests/suite/generated_paths.rs`: this function is fed a file that
/// other tests already keep well-formed, and a silent skip here would hide a
/// row this reader should have judged.
fn parse_roster(text: &str) -> Vec<RosterRow> {
    text.lines()
        .map(str::trim)
        .filter(|l| !l.is_empty() && !l.starts_with('#'))
        .map(|l| {
            let fields: Vec<&str> = l.split('\t').collect();
            assert_eq!(
                fields.len(),
                5,
                "every roster row has exactly five tab-separated columns; found {} in {l:?}",
                fields.len()
            );
            RosterRow {
                name: fields[0].to_string(),
                rung: fields[1].to_string(),
            }
        })
        .collect()
}

/// Every set name whose rung is `stage` — owed by a stage job AND a merge
/// job, since a merge runs the stage list plus `heavy`. `integration` is
/// excluded: it is the `merge`-rung row describing `sluice-run.sh` itself,
/// so it can never be one of its own phases (mirrors the exclusion in
/// `cli/tests/suite/lane_sets.rs`).
fn stage_phase_names(roster: &[RosterRow]) -> BTreeSet<String> {
    roster
        .iter()
        .filter(|r| r.name != "integration" && r.rung == "stage")
        .map(|r| r.name.clone())
        .collect()
}

/// Every set name a merge job may legitimately carry: the stage set plus
/// every `merge`-rung set (currently just `heavy`; `integration` excluded
/// for the same reason as [`stage_phase_names`]).
fn merge_phase_names(roster: &[RosterRow]) -> BTreeSet<String> {
    roster
        .iter()
        .filter(|r| r.name != "integration" && (r.rung == "stage" || r.rung == "merge"))
        .map(|r| r.name.clone())
        .collect()
}

/// One ledger row: only the two columns this reader needs.
struct TimingRow {
    when: String,
    label: String,
}

/// Parse `docs/timings.md`'s content into rows, keeping only lines that are
/// plausibly a row of the **main ledger table** — `docs/timings.md` also
/// embeds illustrative markdown tables with different column counts inside
/// its own prose (rung sweeps, load-average comparisons), and those must
/// never be mistaken for ledger rows.
///
/// The filter is structural, not positional (it does not assume the ledger
/// table starts at any particular line): a candidate row must split into
/// exactly 11 `|`-separated fields (the ledger's column count) and its first
/// field must look like a UTC timestamp (`...T...Z`), which none of the
/// embedded example tables' first columns do. The header row itself
/// (`when (UTC)`) and separator rows (`---`) are excluded by the same
/// timestamp check.
fn parse_timings(text: &str) -> Vec<TimingRow> {
    text.lines()
        .filter_map(|line| {
            let line = line.trim();
            if !line.starts_with('|') || !line.ends_with('|') {
                return None;
            }
            let fields: Vec<&str> = line
                .trim_start_matches('|')
                .trim_end_matches('|')
                .split('|')
                .map(str::trim)
                .collect();
            if fields.len() != 11 {
                return None;
            }
            let when = fields[0];
            if !when.contains('T') || !when.ends_with('Z') {
                return None;
            }
            Some(TimingRow {
                when: when.to_string(),
                label: fields[1].to_string(),
            })
        })
        .collect()
}

/// Group ledger row indices into jobs: each job is a maximal run of
/// consecutive rows (in file order) whose label starts with `sluice:`. See
/// the module doc for what this grouping cannot distinguish.
fn group_jobs(rows: &[TimingRow]) -> Vec<Vec<usize>> {
    let mut jobs = Vec::new();
    let mut current: Vec<usize> = Vec::new();
    for (i, row) in rows.iter().enumerate() {
        if row.label.starts_with("sluice:") {
            current.push(i);
        } else if !current.is_empty() {
            jobs.push(std::mem::take(&mut current));
        }
    }
    if !current.is_empty() {
        jobs.push(current);
    }
    jobs
}

/// A declared path's author, per `docs/generated-paths.txt` (Task 5, The
/// Attestation): either a roster set name, or a declared absence carrying
/// its reason.
enum DeclaredAuthor {
    /// A roster set name (`artifacts`, `census`, `heavy`).
    Roster(String),
    /// `none(<reason>)` — no roster author writes this path, by declaration.
    /// The reason itself is validated (non-empty) by [`parse_declared`] but
    /// not needed by this reader, which only counts these rows; it is
    /// discarded here rather than carried unused.
    None,
}

/// Parse `docs/generated-paths.txt`'s content into `(path, author)` pairs,
/// skipping comments and blank lines. Mirrors
/// `cli/tests/suite/generated_paths.rs`'s `declared()`, including its
/// reasonless-`none` parse-error panic — a malformed declaration is not a
/// reporting-worthy fact for this reader either, it is upstream input this
/// module trusts other tests to keep well-formed.
fn parse_declared(text: &str) -> Vec<(String, DeclaredAuthor)> {
    text.lines()
        .map(str::trim)
        .filter(|l| !l.is_empty() && !l.starts_with('#'))
        .map(|l| {
            let mut fields = l.splitn(2, '\t');
            let path = fields
                .next()
                .unwrap_or_else(|| panic!("empty declared-path line"));
            let author = fields.next().unwrap_or_else(|| {
                panic!(
                    "docs/generated-paths.txt row {path:?} has no <TAB>author column — \
                     every declared path must name its author"
                )
            });
            if author == "none" || author.starts_with("none(") {
                let reason = author
                    .strip_prefix("none(")
                    .and_then(|s| s.strip_suffix(')'))
                    .map(str::trim);
                match reason {
                    Some(r) if !r.is_empty() => (path.to_string(), DeclaredAuthor::None),
                    _ => panic!(
                        "docs/generated-paths.txt row {path:?} declares `none` with no \
                         reason — a reasonless `none(...)` is a PARSE ERROR, exactly as a \
                         reasonless `waiver(...)` is for type-audit or `expect(survives: \
                         ...)` is for seam-guard."
                    ),
                }
            } else {
                (path.to_string(), DeclaredAuthor::Roster(author.to_string()))
            }
        })
        .collect()
}

/// One job's phase-coverage anomaly: which phases were missing (an
/// `owed_but_absent` entry) or present without being owed (a
/// `present_but_unowed` entry), and when that job ran.
/// type-audit: bare-ok(identifier-text: started_at), bare-ok(identifier-text: ended_at), bare-ok(identifier-text: phases)
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct JobFinding {
    /// The `when (UTC)` of the job's first ledger row.
    pub started_at: String,
    /// The `when (UTC)` of the job's last ledger row. Equal to
    /// `started_at` for a one-phase job.
    pub ended_at: String,
    /// The phase names implicated — missing for `owed_but_absent`, observed
    /// without being owed for `present_but_unowed`.
    pub phases: Vec<String>,
}

/// A declared author with no canonical ledger attestation.
/// type-audit: bare-ok(identifier-text: author), bare-ok(count: declared_paths), bare-ok(identifier-text: last_seen)
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AuthorAbsence {
    /// The roster set name (`artifacts`, `census`, `heavy`).
    pub author: String,
    /// How many rows in `docs/generated-paths.txt` name this author. Counts
    /// declared ROWS, not the tracked files under them — a directory row
    /// covers many files at once.
    pub declared_paths: usize,
    /// The most recent **bare** `<author>` ledger row (no `sluice:` prefix),
    /// if one exists — legacy or by-hand evidence (`census-run.sh` and
    /// `heavy-run.sh` both write this form directly), offered as context,
    /// never as a second canonical source. `None` means the ledger holds no
    /// trace of this author at all, canonical or otherwise.
    pub last_seen: Option<String>,
}

/// The result of attesting `declared` against `timings`, using `roster` to
/// know which phases a job owes. See the module doc for what each field
/// means and does not mean.
/// type-audit: bare-ok(count: declared_none_count)
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Report {
    /// Jobs missing a phase the roster's `stage` rung owes them.
    pub owed_but_absent: Vec<JobFinding>,
    /// Jobs carrying a phase no rung implies they should run.
    pub present_but_unowed: Vec<JobFinding>,
    /// Declared roster authors with no `sluice:<author>` row anywhere in the
    /// ledger.
    pub absent_authors: Vec<AuthorAbsence>,
    /// How many declared rows are `none(<reason>)` — no roster author is
    /// expected, by declaration. Its own category, never folded into
    /// `absent_authors`: see the module doc.
    pub declared_none_count: usize,
}

/// Diff `declared`'s author claims and `roster`'s phase-ownership rules
/// against what `timings` actually records — in both directions. Pure: no
/// filesystem access, no clock. See the module doc for the two ledger
/// properties this bounds (no exit code; jobs by adjacency, not id) and for
/// why the two checks below are shaped differently.
/// type-audit: bare-ok(artifact: timings), bare-ok(artifact: roster), bare-ok(artifact: declared)
pub fn attest_report(timings: &str, roster: &str, declared: &str) -> Report {
    let roster_rows = parse_roster(roster);
    let stage = stage_phase_names(&roster_rows);
    let merge = merge_phase_names(&roster_rows);

    let rows = parse_timings(timings);
    let jobs = group_jobs(&rows);

    let mut owed_but_absent = Vec::new();
    let mut present_but_unowed = Vec::new();

    for job in &jobs {
        let phases: BTreeSet<String> = job
            .iter()
            .map(|&i| {
                rows[i]
                    .label
                    .strip_prefix("sluice:")
                    .expect("group_jobs only collects sluice:-prefixed rows")
                    .to_string()
            })
            .collect();
        let started_at = rows[job[0]].when.clone();
        let ended_at = rows[*job.last().expect("a job is never empty")]
            .when
            .clone();

        let missing: Vec<String> = stage.difference(&phases).cloned().collect();
        if !missing.is_empty() {
            owed_but_absent.push(JobFinding {
                started_at: started_at.clone(),
                ended_at: ended_at.clone(),
                phases: missing,
            });
        }

        let unowed: Vec<String> = phases.difference(&merge).cloned().collect();
        if !unowed.is_empty() {
            present_but_unowed.push(JobFinding {
                started_at,
                ended_at,
                phases: unowed,
            });
        }
    }

    let declared_rows = parse_declared(declared);
    let mut declared_none_count = 0usize;
    let mut author_counts: BTreeMap<String, usize> = BTreeMap::new();
    for (_, author) in &declared_rows {
        match author {
            DeclaredAuthor::None => declared_none_count += 1,
            DeclaredAuthor::Roster(name) => {
                *author_counts.entry(name.clone()).or_insert(0) += 1;
            }
        }
    }

    let mut absent_authors = Vec::new();
    for (author, declared_paths) in &author_counts {
        let canonical_label = format!("sluice:{author}");
        let has_canonical = rows.iter().any(|r| r.label == canonical_label);
        if has_canonical {
            continue;
        }
        let last_seen = rows
            .iter()
            .filter(|r| r.label == *author)
            .map(|r| r.when.clone())
            .next_back();
        absent_authors.push(AuthorAbsence {
            author: author.clone(),
            declared_paths: *declared_paths,
            last_seen,
        });
    }

    Report {
        owed_but_absent,
        present_but_unowed,
        absent_authors,
        declared_none_count,
    }
}

/// Render a [`Report`] as plain text for the `hornvale attest` subcommand.
/// A well-formed tree renders a short all-clear; otherwise every anomaly is
/// listed with the context needed to act on it.
/// type-audit: bare-ok(artifact: return)
pub fn render_report(report: &Report) -> String {
    let mut out = String::new();
    out.push_str("# Attestation report\n\n");

    if report.owed_but_absent.is_empty()
        && report.present_but_unowed.is_empty()
        && report.absent_authors.is_empty()
    {
        out.push_str("No anomalies: every job's rows cover its owed phases, no job carries an unowed phase, and every declared roster author has a canonical `sluice:<author>` row.\n");
    }

    if !report.owed_but_absent.is_empty() {
        out.push_str("## Owed but absent\n\n");
        for f in &report.owed_but_absent {
            out.push_str(&format!(
                "- job {}..{}: missing {}\n",
                f.started_at,
                f.ended_at,
                f.phases.join(", ")
            ));
        }
        out.push('\n');
    }

    if !report.present_but_unowed.is_empty() {
        out.push_str("## Present but unowed\n\n");
        for f in &report.present_but_unowed {
            out.push_str(&format!(
                "- job {}..{}: unowed {}\n",
                f.started_at,
                f.ended_at,
                f.phases.join(", ")
            ));
        }
        out.push('\n');
    }

    if !report.absent_authors.is_empty() {
        out.push_str("## Declared authors with no canonical row\n\n");
        for a in &report.absent_authors {
            let last = a.last_seen.as_deref().unwrap_or("never");
            out.push_str(&format!(
                "- {} ({} declared row(s)): no `sluice:{}` row found; most recent bare `{}` row: {}\n",
                a.author, a.declared_paths, a.author, a.author, last
            ));
        }
        out.push('\n');
    }

    out.push_str(&format!(
        "declared `none(<reason>)` rows (no author expected): {}\n",
        report.declared_none_count
    ));

    out
}

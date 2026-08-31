//! Reading a committed fixture **as authored** — through the schema that sits
//! beside it on disk, never through the live metric registry.
//!
//! ## Why this module exists
//!
//! A committed `rows.csv` is a historical measurement record. The injection
//! battery's own README calls its arms *"authored evidence, not a generated
//! artifact"*, and a census golden is the same kind of object: a statement
//! about worlds as they were on the day the canonical box measured them.
//!
//! [`crate::load_rows`] reads such a file through the *live* study, and
//! requires the CSV header to match that study's schema **exactly**. That is
//! the right reader for "is this fixture current?" — and it is the wrong one
//! for "what did this fixture record?", because it makes every committed
//! fixture in the repository unreadable the moment anybody registers a new
//! metric. Registering a metric is an ordinary campaign act; invalidating
//! every past measurement is not, and the deadlock it creates is real: the
//! fixtures can only be re-authored on the canonical box against a pushed
//! SHA, and the commit gate will not admit the commit until they are.
//!
//! So the fixture describes itself. Every generated study directory carries a
//! `schema.json` next to its `rows.csv` — emitted from the same `RunResult`,
//! in the same column order (`crate::schema::render_schema`) — and this
//! module reads the pair. [`crate::domesday::census::load`] already reads a
//! census this way and is entirely registry-independent; this is the same
//! idea carried to the typed [`RunResult`] the calibration suite consumes.
//!
//! ## The direction this check enforces — both of them, deliberately
//!
//! A subset check has a direction, and a check written in one direction is
//! structurally blind to the other while still reading as total. So state
//! both:
//!
//! - **fixture ⊆ live** is the tolerated direction. The live registry having
//!   columns the fixture lacks is [`FixtureAge::Predates`] — GREEN, because a
//!   purely additive registry change cannot invalidate a past measurement —
//!   but it is announced on every read (see [`FixtureAge::message`]) so that
//!   nobody mistakes silence for currency.
//! - **live ⊆ fixture** is the refused direction. A column the fixture has
//!   and the live registry does not means the fixture describes a world model
//!   that no longer exists, and there is no honest way to read it: that is
//!   `DIVERGED`, and it is an error, not a warning.
//!
//! Two further refusals fall under `DIVERGED` for the same reason — a shared
//! column whose **kind** changed (the recorded bytes no longer mean what the
//! registry says they mean), and shared columns whose **relative order**
//! differs between fixture and registry (which would make the narrowed study
//! this module returns disagree with the columns it just parsed).
//!
//! Both directions are exercised by this module's own tests, and so is each
//! refusal.
//!
//! ## The trap that makes the naive relaxation wrong
//!
//! [`crate::load_rows`] parses **positionally**, indexing `rec[2 + i]` by the
//! *study's* metric list. Merely relaxing its header check would make a
//! fixture missing one column read every subsequent field into the wrong
//! metric, and read the trailing `refusal` column as a metric value —
//! silently, with plausible output. This module never indexes by the live
//! list. It resolves each column **by name** against the registry and then
//! reads at that column's position **in the fixture's own header**, which is
//! the only position that means anything about the bytes on disk.

use crate::runner::{parse_csv_records, value_from_field};
use crate::{Metric, MetricSelection, Row, RunResult, Study, StudyError, SummaryKind};
use std::collections::BTreeSet;
use std::path::Path;

/// How a committed fixture's schema stands against the live metric registry.
///
/// Two-valued rather than three because the third verdict — `DIVERGED` — is
/// an error and never a value: a diverged fixture yields no [`RunResult`] to
/// carry a verdict on. See the module docs for the direction each arm names.
/// type-audit: bare-ok(identifier-text: Predates.added)
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FixtureAge {
    /// The fixture's metric columns are exactly the live study's, in order.
    Current,
    /// The live study has columns the fixture predates. Green: an additive
    /// registry change cannot invalidate a past measurement.
    Predates {
        /// The live column names absent from the fixture, in registry order.
        added: Vec<&'static str>,
    },
}

impl FixtureAge {
    /// The line a caller should print, or `None` when the fixture is current.
    ///
    /// Split out from printing so the wording is testable: a check whose only
    /// output is a `println!` can only be verified by a human reading a log.
    /// type-audit: bare-ok(identifier-text: label), bare-ok(prose: return)
    pub fn message(&self, label: &str) -> Option<String> {
        match self {
            FixtureAge::Current => None,
            FixtureAge::Predates { added } => Some(format!(
                "[PREDATES] {label}: the live registry carries {} column(s) this committed \
                 fixture predates: {}. The fixture is read AS AUTHORED and remains valid \
                 evidence — an additive registry change cannot invalidate a past \
                 measurement — but it is NOT current. Re-author it on the canonical box \
                 (scripts/census-run.sh, scripts/gnomon-injection.sh) at the next refresh.",
                added.len(),
                added.join(", ")
            )),
        }
    }

    /// Print [`FixtureAge::message`] when there is one. Silent for
    /// [`FixtureAge::Current`], loud for every read of a predating fixture —
    /// on purpose: a once-a-campaign notice is one nobody sees.
    /// type-audit: bare-ok(identifier-text: label)
    pub fn announce(&self, label: &str) {
        if let Some(line) = self.message(label) {
            println!("{line}");
        }
    }
}

/// One column as the fixture's own `schema.json` declares it.
#[derive(Debug, Clone, PartialEq, Eq)]
struct FixtureColumn {
    name: String,
    kind: String,
}

/// The `schema.json` tag for a metric's summary kind — the same mapping
/// `crate::schema::render_schema` writes, read back.
fn kind_tag(kind: &SummaryKind) -> &'static str {
    match kind {
        SummaryKind::Numeric { .. } => "numeric",
        SummaryKind::Flag => "flag",
        SummaryKind::Categorical => "categorical",
    }
}

/// Read a fixture's declared columns from `dir/schema.json`, in file order.
fn read_fixture_columns(dir: &Path) -> Result<Vec<FixtureColumn>, StudyError> {
    let path = dir.join("schema.json");
    let text = std::fs::read_to_string(&path).map_err(|e| StudyError {
        message: format!(
            "{}: {e} — a fixture read as authored must carry its own schema.json",
            path.display()
        ),
    })?;
    let json: serde_json::Value = serde_json::from_str(&text).map_err(|e| StudyError {
        message: format!("{}: {e}", path.display()),
    })?;
    let columns = json["columns"].as_array().ok_or_else(|| StudyError {
        message: format!("{}: no columns array", path.display()),
    })?;
    columns
        .iter()
        .map(|c| {
            let name = c["name"].as_str().ok_or_else(|| StudyError {
                message: format!("{}: a column has no name", path.display()),
            })?;
            let kind = c["kind"].as_str().ok_or_else(|| StudyError {
                message: format!("{}: column {name:?} has no kind", path.display()),
            })?;
            Ok(FixtureColumn {
                name: name.to_string(),
                kind: kind.to_string(),
            })
        })
        .collect()
}

/// Resolve the fixture's metric columns against the live study, or refuse.
///
/// Returns the live [`Metric`]s in the FIXTURE's column order — that order,
/// not the registry's, is what the bytes on disk are laid out in.
fn adjudicate(
    live: &[Metric],
    label: &str,
    fixture: &[FixtureColumn],
) -> Result<(Vec<usize>, FixtureAge), StudyError> {
    // Indices into `live`, never clones: `Metric` holds function pointers and
    // is deliberately not `Clone`.
    let mut resolved: Vec<usize> = Vec::with_capacity(fixture.len());
    let mut retired: Vec<String> = Vec::new();
    let mut rekinded: Vec<String> = Vec::new();
    for col in fixture {
        match live.iter().position(|m| m.name == col.name) {
            None => retired.push(col.name.clone()),
            Some(at) => {
                let live_kind = kind_tag(&live[at].summary);
                if live_kind != col.kind {
                    rekinded.push(format!(
                        "{} (authored {:?}, live {:?})",
                        col.name, col.kind, live_kind
                    ));
                }
                resolved.push(at);
            }
        }
    }
    if !retired.is_empty() || !rekinded.is_empty() {
        return Err(diverged(
            label,
            &format!(
                "columns the fixture has and the live registry does not: {retired:?}; \
                 shared columns whose kind changed: {rekinded:?}"
            ),
        ));
    }

    let fixture_names: BTreeSet<&str> = fixture.iter().map(|c| c.name.as_str()).collect();
    let fixture_order: Vec<&str> = fixture.iter().map(|c| c.name.as_str()).collect();
    let live_shared: Vec<&str> = live
        .iter()
        .map(|m| m.name)
        .filter(|n| fixture_names.contains(n))
        .collect();
    if live_shared != fixture_order {
        return Err(diverged(
            label,
            &format!(
                "the shared columns are in a different relative order in the registry \
                 than in the fixture:\n  fixture:  {fixture_order:?}\n  registry: {live_shared:?}"
            ),
        ));
    }

    let added: Vec<&'static str> = live
        .iter()
        .map(|m| m.name)
        .filter(|n| !fixture_names.contains(n))
        .collect();
    let age = if added.is_empty() {
        FixtureAge::Current
    } else {
        FixtureAge::Predates { added }
    };
    Ok((resolved, age))
}

/// The one shape a `DIVERGED` refusal takes, so every arm reads alike and the
/// remedy is never left to the reader to infer.
fn diverged(label: &str, detail: &str) -> StudyError {
    StudyError {
        message: format!(
            "DIVERGED: the committed fixture {label} cannot be read against the live \
             metric registry. {detail}. This is not a predating fixture — a predating \
             one is read as authored and passes. Re-author the fixture on the canonical \
             box (scripts/census-run.sh for a census, scripts/gnomon-injection.sh for an \
             injection arm) in the same commit as the registry change."
        ),
    }
}

/// Load a committed fixture directory (`rows.csv` + `schema.json`) **as
/// authored**, and report how its schema stands against the live registry.
///
/// The returned [`RunResult`] carries the fixture's own columns, in the
/// fixture's own order, and a `study` narrowed to exactly those columns — so
/// every existing consumer that resolves a column by name
/// (`metric_names.iter().position(...)`) works unchanged, and
/// [`crate::render_diff_results`], which indexes both sides by the *new*
/// side's `metric_names`, sees two consistently shaped results.
///
/// Errors are `DIVERGED` refusals (see the module docs) plus the ordinary
/// I/O, JSON and CSV parse failures.
/// type-audit: bare-ok(identifier-text: label)
pub fn load_authored(
    study: &Study,
    dir: &Path,
    label: &str,
) -> Result<(RunResult, FixtureAge), StudyError> {
    let columns = read_fixture_columns(dir)?;
    let rows_path = dir.join("rows.csv");
    let csv = std::fs::read_to_string(&rows_path).map_err(|e| StudyError {
        message: format!("{}: {e}", rows_path.display()),
    })?;

    let mut records = parse_csv_records(&csv).into_iter();
    let header = records.next().ok_or_else(|| StudyError {
        message: format!("{} is empty", rows_path.display()),
    })?;

    // schema.json and rows.csv must agree exactly, in order. `render_schema`
    // emits them from one `RunResult`, so a disagreement means one of the two
    // was edited or regenerated without the other — the drift that
    // `domesday::census::load` was written to catch, tightened here from a set
    // comparison to an ordered one because this reader parses by position.
    let declared: Vec<&str> = columns.iter().map(|c| c.name.as_str()).collect();
    if header != declared {
        return Err(StudyError {
            message: format!(
                "{label}: schema.json and rows.csv disagree on columns:\n  \
                 rows.csv:    {header:?}\n  schema.json: {declared:?}"
            ),
        });
    }

    // The structural columns are a contract of `render_csv`, not of any
    // registry, so they are checked here rather than adjudicated.
    if columns.len() < 3
        || declared[0] != "seed"
        || declared[1] != "pin_set"
        || declared[columns.len() - 1] != "refusal"
    {
        return Err(StudyError {
            message: format!(
                "{label}: a rows.csv must be seed, pin_set, <metrics...>, refusal; \
                 found {declared:?}"
            ),
        });
    }

    let live = study.selected_metrics()?;
    let (resolved, age) = adjudicate(&live, label, &columns[2..columns.len() - 1])?;
    let metrics: Vec<&Metric> = resolved.iter().map(|at| &live[*at]).collect();
    let n = metrics.len();

    let mut rows = Vec::new();
    for rec in records {
        if rec.len() == 1 && rec[0].is_empty() {
            continue; // tolerate a blank trailing line
        }
        if rec.len() < n + 2 {
            return Err(StudyError {
                message: format!(
                    "{label}: a row has {} fields, need at least {}",
                    rec.len(),
                    n + 2
                ),
            });
        }
        let seed = rec[0].parse::<u64>().map_err(|_| StudyError {
            message: format!("{label}: '{}' is not a seed", rec[0]),
        })?;
        // Positional against the FIXTURE's header — which is the layout of the
        // bytes — with each column's kind taken from the metric its name
        // resolved to. Never positional against the live registry.
        let values = metrics
            .iter()
            .enumerate()
            .map(|(i, m)| value_from_field(&rec[2 + i], &m.summary))
            .collect::<Result<Vec<_>, _>>()?;
        let refusal_text = rec[2 + n..].join(",");
        rows.push(Row {
            seed,
            pin_set: rec[1].clone(),
            values,
            refusal: (!refusal_text.is_empty()).then_some(refusal_text),
        });
    }

    let narrowed = Study {
        metrics: MetricSelection::Named(metrics.iter().map(|m| m.name.to_string()).collect()),
        ..study.clone()
    };
    Ok((
        RunResult {
            study: narrowed,
            metric_names: metrics.iter().map(|m| m.name).collect(),
            rows,
        },
        age,
    ))
}

/// [`crate::render_diff`] over two committed fixture DIRECTORIES, each read as
/// authored.
///
/// A sibling of `render_diff`, never a replacement: `render_diff` takes the
/// live study because its other caller (`hornvale lab diff`) diffs a fresh run
/// against a previous one for the study *as it is now*, which is a different
/// question and wants the strict reader.
///
/// The two fixtures must declare the same metric columns. That is a refusal
/// rather than a merge because [`crate::render_diff_results`] indexes both
/// sides by one shared column list; diffing two differently-aged fixtures
/// would silently compare mismatched columns.
/// type-audit: bare-ok(identifier-text: old_label), bare-ok(identifier-text: new_label), bare-ok(artifact: return)
pub fn render_authored_diff(
    study: &Study,
    old_dir: &Path,
    old_label: &str,
    new_dir: &Path,
    new_label: &str,
) -> Result<(String, FixtureAge), StudyError> {
    let (old, old_age) = load_authored(study, old_dir, old_label)?;
    let (new, new_age) = load_authored(study, new_dir, new_label)?;
    if old.metric_names != new.metric_names {
        return Err(StudyError {
            message: format!(
                "{old_label} and {new_label} declare different metric columns, so a \
                 column-by-column diff of the two would be meaningless. Re-author both \
                 arms together."
            ),
        });
    }
    debug_assert_eq!(
        old_age, new_age,
        "identical column lists must yield identical verdicts"
    );
    Ok((crate::render_diff_results(&old, &new), new_age))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{MetricValue, PinSet, Seeds};

    /// A study over one metric of each `SummaryKind`, in registry order.
    /// `star-class` is Categorical, `tidally-locked` is Flag,
    /// `ocean-fraction` is Numeric — the same trio `schema.rs`'s tests use.
    fn study_of(names: &[&str]) -> Study {
        Study {
            name: "authored-test".to_string(),
            description: "for authored-fixture tests".to_string(),
            seeds: Seeds { from: 0, count: 2 },
            pin_sets: vec![PinSet {
                label: "default".to_string(),
                pins: vec![],
                roster: None,
            }],
            metrics: MetricSelection::Named(names.iter().map(|n| n.to_string()).collect()),
        }
    }

    /// The three test metrics in registry order, which is what the live study
    /// will present them in. Derived, never declared: a hand-written order
    /// that drifted from the registry would make the order test vacuous.
    fn registry_order(names: &[&str]) -> Vec<String> {
        study_of(names)
            .selected_metrics()
            .expect("the trio is in the registry")
            .iter()
            .map(|m| m.name.to_string())
            .collect()
    }

    /// Write a fixture directory: `schema.json` with `cols` (name, kind) and a
    /// `rows.csv` whose header is those names and whose body is `rows`.
    fn write_fixture(tag: &str, cols: &[(&str, &str)], rows: &[&str]) -> std::path::PathBuf {
        let dir =
            std::env::temp_dir().join(format!("hornvale-authored-{}-{tag}", std::process::id()));
        let _ = std::fs::remove_dir_all(&dir);
        std::fs::create_dir_all(&dir).expect("scratch fixture dir");
        let columns: Vec<serde_json::Value> = cols
            .iter()
            .map(|(n, k)| serde_json::json!({ "name": n, "kind": k }))
            .collect();
        std::fs::write(
            dir.join("schema.json"),
            serde_json::to_string_pretty(&serde_json::json!({ "columns": columns }))
                .expect("schema serializes"),
        )
        .expect("write schema.json");
        let header: Vec<&str> = cols.iter().map(|(n, _)| *n).collect();
        let mut csv = header.join(",");
        for row in rows {
            csv.push('\n');
            csv.push_str(row);
        }
        csv.push('\n');
        std::fs::write(dir.join("rows.csv"), csv).expect("write rows.csv");
        dir
    }

    /// The full trio, current against a study that selects exactly it.
    fn current_columns() -> Vec<(&'static str, &'static str)> {
        vec![
            ("seed", "integer"),
            ("pin_set", "categorical"),
            ("star-class", "categorical"),
            ("tidally-locked", "flag"),
            ("ocean-fraction", "numeric"),
            ("refusal", "categorical"),
        ]
    }

    #[test]
    fn the_trio_is_in_the_registry_order_these_tests_assume() {
        // Guards every other test in this module: they lay columns out in this
        // order, and an order test is vacuous if the assumed order is wrong.
        assert_eq!(
            registry_order(&["star-class", "tidally-locked", "ocean-fraction"]),
            vec!["star-class", "tidally-locked", "ocean-fraction"]
        );
    }

    #[test]
    fn a_fixture_matching_the_live_study_reads_current() {
        let dir = write_fixture("current", &current_columns(), &["0,default,G,true,0.7,"]);
        let (result, age) = load_authored(
            &study_of(&["star-class", "tidally-locked", "ocean-fraction"]),
            &dir,
            "current",
        )
        .expect("a current fixture loads");
        assert_eq!(age, FixtureAge::Current);
        assert_eq!(
            result.metric_names,
            vec!["star-class", "tidally-locked", "ocean-fraction"]
        );
        assert_eq!(result.rows.len(), 1);
        assert_eq!(result.rows[0].values[0], MetricValue::Text("G".to_string()));
        assert_eq!(result.rows[0].values[1], MetricValue::Flag(true));
        assert_eq!(result.rows[0].values[2], MetricValue::Number(0.7));
        assert_eq!(result.rows[0].refusal, None);
    }

    #[test]
    fn a_current_fixture_announces_nothing() {
        assert_eq!(FixtureAge::Current.message("the-census"), None);
    }

    /// The PREDATES arm, and simultaneously the positional-trap regression:
    /// the column the fixture lacks is the MIDDLE one, so a reader that
    /// indexed by the live list would read `0.7` as `tidally-locked` and the
    /// empty refusal field as `ocean-fraction`.
    #[test]
    fn a_fixture_missing_a_live_column_reads_predates_without_shifting_its_values() {
        let cols = [
            ("seed", "integer"),
            ("pin_set", "categorical"),
            ("star-class", "categorical"),
            ("ocean-fraction", "numeric"),
            ("refusal", "categorical"),
        ];
        let dir = write_fixture("predates", &cols, &["0,default,G,0.7,"]);
        let (result, age) = load_authored(
            &study_of(&["star-class", "tidally-locked", "ocean-fraction"]),
            &dir,
            "predates",
        )
        .expect("a predating fixture loads as authored");
        assert_eq!(
            age,
            FixtureAge::Predates {
                added: vec!["tidally-locked"]
            }
        );
        assert_eq!(result.metric_names, vec!["star-class", "ocean-fraction"]);
        assert_eq!(result.rows[0].values[0], MetricValue::Text("G".to_string()));
        assert_eq!(result.rows[0].values[1], MetricValue::Number(0.7));
        assert_eq!(result.rows[0].refusal, None);
    }

    #[test]
    fn predates_names_every_column_it_predates() {
        let line = FixtureAge::Predates {
            added: vec!["breached-delving-count"],
        }
        .message("the-census")
        .expect("PREDATES always has a message");
        assert!(line.starts_with("[PREDATES] the-census:"), "{line}");
        assert!(line.contains("breached-delving-count"), "{line}");
    }

    /// The narrowed study must re-select exactly the columns just parsed, in
    /// the same order — that is what lets `render_diff_results` index both
    /// sides consistently.
    #[test]
    fn the_narrowed_study_reselects_the_fixture_columns_in_the_fixtures_order() {
        let cols = [
            ("seed", "integer"),
            ("pin_set", "categorical"),
            ("star-class", "categorical"),
            ("ocean-fraction", "numeric"),
            ("refusal", "categorical"),
        ];
        let dir = write_fixture("narrowed", &cols, &["0,default,G,0.7,"]);
        let (result, _) = load_authored(
            &study_of(&["star-class", "tidally-locked", "ocean-fraction"]),
            &dir,
            "narrowed",
        )
        .expect("loads");
        let reselected: Vec<&str> = result
            .study
            .selected_metrics()
            .expect("the narrowed study selects")
            .iter()
            .map(|m| m.name)
            .collect();
        assert_eq!(reselected, result.metric_names);
    }

    /// DIVERGED, arm 1 — the refused direction. A column the fixture has and
    /// the live registry does not.
    #[test]
    fn a_column_the_live_registry_no_longer_has_is_diverged() {
        let cols = [
            ("seed", "integer"),
            ("pin_set", "categorical"),
            ("star-class", "categorical"),
            ("a-retired-column", "numeric"),
            ("ocean-fraction", "numeric"),
            ("refusal", "categorical"),
        ];
        let dir = write_fixture("retired", &cols, &["0,default,G,1.0,0.7,"]);
        let err = load_authored(
            &study_of(&["star-class", "tidally-locked", "ocean-fraction"]),
            &dir,
            "retired",
        )
        .expect_err("a fixture column the registry lost must be refused");
        assert!(err.message.contains("DIVERGED"), "{}", err.message);
        // The RETIRED clause specifically, not merely the name appearing
        // somewhere: the reordering arm below would also mention it (a column
        // the registry lost is trivially out of order), and an assertion that
        // both arms satisfy cannot tell you which one is holding.
        assert!(
            err.message
                .contains("the live registry does not: [\"a-retired-column\"]"),
            "{}",
            err.message
        );
    }

    /// DIVERGED, arm 2 — a shared column whose kind changed. The bytes no
    /// longer mean what the registry says they mean.
    #[test]
    fn a_shared_column_whose_kind_changed_is_diverged() {
        let mut cols = current_columns();
        cols[4] = ("ocean-fraction", "categorical");
        let dir = write_fixture("rekinded", &cols, &["0,default,G,true,0.7,"]);
        let err = load_authored(
            &study_of(&["star-class", "tidally-locked", "ocean-fraction"]),
            &dir,
            "rekinded",
        )
        .expect_err("a kind change must be refused");
        assert!(err.message.contains("DIVERGED"), "{}", err.message);
        assert!(err.message.contains("ocean-fraction"), "{}", err.message);
    }

    /// DIVERGED, arm 3 — shared columns reordered. Caught even though every
    /// name resolves and every kind matches.
    #[test]
    fn reordered_shared_columns_are_diverged() {
        let cols = [
            ("seed", "integer"),
            ("pin_set", "categorical"),
            ("ocean-fraction", "numeric"),
            ("star-class", "categorical"),
            ("refusal", "categorical"),
        ];
        let dir = write_fixture("reordered", &cols, &["0,default,0.7,G,"]);
        let err = load_authored(
            &study_of(&["star-class", "tidally-locked", "ocean-fraction"]),
            &dir,
            "reordered",
        )
        .expect_err("a reordering must be refused");
        assert!(err.message.contains("DIVERGED"), "{}", err.message);
        assert!(
            err.message.contains("different relative order"),
            "{}",
            err.message
        );
    }

    #[test]
    fn a_schema_that_disagrees_with_its_rows_csv_is_refused() {
        let dir = write_fixture("disagree", &current_columns(), &["0,default,G,true,0.7,"]);
        // Rewrite only the CSV header, leaving schema.json as authored.
        let csv = std::fs::read_to_string(dir.join("rows.csv")).expect("read back");
        let body = csv.split_once('\n').expect("a header and a body").1;
        std::fs::write(
            dir.join("rows.csv"),
            format!("seed,pin_set,star-class,tidally-locked,karst-fraction,refusal\n{body}"),
        )
        .expect("rewrite header");
        let err = load_authored(
            &study_of(&["star-class", "tidally-locked", "ocean-fraction"]),
            &dir,
            "disagree",
        )
        .expect_err("schema/rows drift must be refused");
        assert!(
            err.message.contains("disagree on columns"),
            "{}",
            err.message
        );
    }

    #[test]
    fn a_fixture_without_a_schema_is_refused() {
        let dir = write_fixture("noschema", &current_columns(), &["0,default,G,true,0.7,"]);
        std::fs::remove_file(dir.join("schema.json")).expect("remove schema.json");
        let err = load_authored(
            &study_of(&["star-class", "tidally-locked", "ocean-fraction"]),
            &dir,
            "noschema",
        )
        .expect_err("a fixture with no schema cannot be read as authored");
        assert!(err.message.contains("schema.json"), "{}", err.message);
    }

    #[test]
    fn a_rows_csv_that_is_not_seed_pin_set_metrics_refusal_is_refused() {
        let cols = [
            ("pin_set", "categorical"),
            ("seed", "integer"),
            ("star-class", "categorical"),
            ("refusal", "categorical"),
        ];
        let dir = write_fixture("structural", &cols, &["default,0,G,"]);
        let err = load_authored(&study_of(&["star-class"]), &dir, "structural")
            .expect_err("the structural columns are a contract");
        assert!(err.message.contains("seed, pin_set"), "{}", err.message);
    }

    #[test]
    fn diffing_two_differently_aged_fixtures_is_refused() {
        let old = write_fixture(
            "diff-old",
            &[
                ("seed", "integer"),
                ("pin_set", "categorical"),
                ("star-class", "categorical"),
                ("refusal", "categorical"),
            ],
            &["0,default,G,"],
        );
        let new = write_fixture("diff-new", &current_columns(), &["0,default,G,true,0.7,"]);
        let study = study_of(&["star-class", "tidally-locked", "ocean-fraction"]);
        let err = render_authored_diff(&study, &old, "diff-old", &new, "diff-new")
            .expect_err("two differently shaped fixtures cannot be diffed");
        assert!(
            err.message.contains("different metric columns"),
            "{}",
            err.message
        );
    }

    #[test]
    fn two_identical_fixtures_diff_to_no_movement() {
        let a = write_fixture("same-a", &current_columns(), &["0,default,G,true,0.7,"]);
        let b = write_fixture("same-b", &current_columns(), &["0,default,G,true,0.7,"]);
        let study = study_of(&["star-class", "tidally-locked", "ocean-fraction"]);
        let (report, age) = render_authored_diff(&study, &a, "same-a", &b, "same-b")
            .expect("identical fixtures diff");
        assert_eq!(age, FixtureAge::Current);
        assert!(report.contains("No metric moved."), "{report}");
    }

    /// The positive control for the two-identical-fixtures test above: a
    /// changed value must actually show up, or that test proves nothing.
    #[test]
    fn a_changed_value_moves_the_authored_diff() {
        let a = write_fixture("moved-a", &current_columns(), &["0,default,G,true,0.7,"]);
        let b = write_fixture("moved-b", &current_columns(), &["0,default,K,true,0.7,"]);
        let study = study_of(&["star-class", "tidally-locked", "ocean-fraction"]);
        let (report, _) =
            render_authored_diff(&study, &a, "moved-a", &b, "moved-b").expect("fixtures diff");
        assert!(!report.contains("No metric moved."), "{report}");
        assert!(report.contains("star-class"), "{report}");
    }
}

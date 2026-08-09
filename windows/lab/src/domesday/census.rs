//! Loading the committed census table.
//!
//! Reads `rows.csv` + `schema.json` from a study's generated directory and
//! never rebuilds a world to do it (see the `domesday` module doc). The CSV
//! parser is quote-aware (RFC 4180-lite): several census columns
//! (`goblin-flagship-roles`, `kobold-flagship-roles`) hold comma-joined role
//! lists and are quoted in the committed fixture, so a naive `split(',')`
//! would shred them across extra fields.

use std::collections::{BTreeMap, BTreeSet};
use std::path::Path;
#[cfg(test)]
use std::path::PathBuf;

/// The repository root, resolved from the test binary's working directory.
///
/// Test-only, deliberately: production code gets its census directory from
/// its caller instead — `load(dir: &Path)` already takes one, and the future
/// `hornvale lab domesday` subcommand (Task 6) will pass it explicitly from
/// the CLI. Cargo (and nextest, verified directly against this workspace)
/// run a crate's test binaries with cwd set to the crate's manifest
/// directory — `windows/lab` here — so walking up two levels reaches the
/// workspace root without a build-time compile-directory macro at all:
/// `#[cfg(test)]` alone does not exempt that macro from
/// `cli/tests/build_path_embedding.rs`'s scan (confirmed by trying it — the
/// scan is a textual match over the whole file, not the compiled output, per
/// that test's own doc comment), so avoiding the macro entirely is what
/// keeps this out of the frozen list in `cli/tests/fixtures/manifest-dir-
/// uses.txt` (decision 0090 amendment 2) rather than merely deferring it.
#[cfg(test)]
pub(crate) fn repo_root() -> PathBuf {
    PathBuf::from("../..")
}

/// One column of the census, as `schema.json` describes it.
/// type-audit: bare-ok(identifier-text: name), bare-ok(identifier-text: kind), bare-ok(prose: doc), bare-ok(identifier-text: domain), bare-ok(identifier-text: role)
#[derive(Debug)]
pub struct Column {
    /// Metric name (the CSV header).
    pub name: String,
    /// `numeric` | `categorical` | `flag` | `integer`.
    pub kind: String,
    /// Human-readable description.
    pub doc: String,
    /// Subject the metric belongs to (e.g. `astronomy`, `terrain`).
    pub domain: String,
    /// `descriptor` | `invariant`.
    pub role: String,
}

/// The committed census table: one row per world, one column per metric.
/// type-audit: bare-ok(artifact: rows)
#[derive(Debug)]
pub struct Census {
    /// Column descriptors, in schema order.
    pub columns: Vec<Column>,
    /// One map per world, keyed by column name, holding the raw CSV text
    /// (empty string for an absent/refused value).
    pub rows: Vec<BTreeMap<String, String>>,
}

impl Census {
    /// Whether this census has a column of this name at all.
    ///
    /// The distinction `values`/`absent_count` need: a metric absent from
    /// every world looks, row by row, identical to a metric name that is
    /// not a column at all (both produce `None` from every row's map), so
    /// without a schema-level check a typo'd metric name would silently
    /// read as "1,000 worlds declined to report this" instead of failing.
    /// type-audit: bare-ok(identifier-text: metric), bare-ok(flag: return)
    pub fn has(&self, metric: &str) -> bool {
        self.columns.iter().any(|c| c.name == metric)
    }

    /// Present (non-empty) values for a metric, in row order.
    ///
    /// Panics on an unknown metric name rather than returning a `Vec` that
    /// would be indistinguishable from "every world declined to report
    /// this" — see `has`. Every caller of this reader is our own code (a
    /// detector, an expectations file, a comparator) passing a name that
    /// must exist; an unknown name is a programming error (a typo), not a
    /// data condition, and this is the one artifact whose purpose is not
    /// saying false things about the project.
    /// type-audit: bare-ok(identifier-text: metric), bare-ok(artifact: return)
    pub fn values(&self, metric: &str) -> Vec<&str> {
        assert!(self.has(metric), "unknown census metric: {metric}");
        self.rows
            .iter()
            .filter_map(|r| r.get(metric).map(String::as_str))
            .filter(|v| !v.is_empty())
            .collect()
    }

    /// How many worlds have no value for this metric (absent, not present
    /// with an empty value coincidentally — the CSV never distinguishes the
    /// two, so this is "present count subtracted from total rows"). Panics
    /// on an unknown metric name (see `values`).
    /// type-audit: bare-ok(identifier-text: metric), bare-ok(count: return)
    pub fn absent_count(&self, metric: &str) -> usize {
        assert!(self.has(metric), "unknown census metric: {metric}");
        self.rows.len() - self.values(metric).len()
    }
}

/// Split one CSV line into fields, honoring RFC 4180 quoting: a
/// double-quoted field may contain commas and literal quotes escaped as
/// `""`. Surrounding quotes are stripped; unquoted fields pass through
/// unchanged.
fn split_csv_line(line: &str) -> Vec<String> {
    let mut fields = Vec::new();
    let mut field = String::new();
    let mut in_quotes = false;
    let mut chars = line.chars().peekable();
    while let Some(c) = chars.next() {
        if in_quotes {
            if c == '"' {
                if chars.peek() == Some(&'"') {
                    field.push('"');
                    chars.next();
                } else {
                    in_quotes = false;
                }
            } else {
                field.push(c);
            }
        } else if c == '"' {
            in_quotes = true;
        } else if c == ',' {
            fields.push(std::mem::take(&mut field));
        } else {
            field.push(c);
        }
    }
    fields.push(field);
    fields
}

/// Load `rows.csv` and `schema.json` from a study's generated directory
/// (e.g. `book/src/laboratory/generated/the-census`), and refuse to load a
/// pair that have drifted apart.
///
/// This is not a hypothetical: the census-of-the-meeting and the-census
/// fixtures actually did drift (`schema.json` regenerated six hours before
/// a metric-registry change that `rows.csv` predates), and this reader
/// would otherwise have loaded the mismatch silently. Every column named in
/// `schema.json` must appear in `rows.csv`'s header and vice versa, or
/// `load` errors naming the offending columns instead of returning a
/// `Census` whose `columns` and row keys disagree.
/// type-audit: bare-ok(prose: return)
pub fn load(dir: &Path) -> Result<Census, String> {
    let schema_text = std::fs::read_to_string(dir.join("schema.json"))
        .map_err(|e| format!("schema.json: {e}"))?;
    let schema: serde_json::Value =
        serde_json::from_str(&schema_text).map_err(|e| format!("schema.json parse: {e}"))?;
    let columns: Vec<Column> = schema["columns"]
        .as_array()
        .ok_or("schema.json has no columns array")?
        .iter()
        .map(|c| Column {
            name: c["name"].as_str().unwrap_or_default().to_string(),
            kind: c["kind"].as_str().unwrap_or_default().to_string(),
            doc: c["doc"].as_str().unwrap_or_default().to_string(),
            domain: c["domain"].as_str().unwrap_or_default().to_string(),
            role: c["role"].as_str().unwrap_or_default().to_string(),
        })
        .collect();

    let csv =
        std::fs::read_to_string(dir.join("rows.csv")).map_err(|e| format!("rows.csv: {e}"))?;
    let mut lines = csv.lines();
    let header = split_csv_line(lines.next().ok_or("rows.csv is empty")?);

    let schema_names: BTreeSet<&str> = columns.iter().map(|c| c.name.as_str()).collect();
    let csv_names: BTreeSet<&str> = header.iter().map(String::as_str).collect();
    let schema_only: Vec<&str> = schema_names.difference(&csv_names).copied().collect();
    let csv_only: Vec<&str> = csv_names.difference(&schema_names).copied().collect();
    if !schema_only.is_empty() || !csv_only.is_empty() {
        return Err(format!(
            "schema.json and rows.csv disagree on columns: in schema.json \
             only: {schema_only:?}; in rows.csv only: {csv_only:?}"
        ));
    }

    let rows = lines
        .filter(|l| !l.trim().is_empty())
        .map(|line| {
            header
                .iter()
                .cloned()
                .zip(split_csv_line(line))
                .collect::<BTreeMap<String, String>>()
        })
        .collect();

    Ok(Census { columns, rows })
}

#[cfg(test)]
mod tests {
    use super::*;

    fn committed() -> Census {
        load(&repo_root().join("book/src/laboratory/generated/the-census"))
            .expect("the committed census loads")
    }

    #[test]
    fn loads_the_committed_census() {
        let c = committed();
        assert_eq!(c.rows.len(), 1000, "the census is 1000 worlds");
        assert!(c.columns.len() >= 194, "194 columns at time of writing");
    }

    #[test]
    fn every_column_carries_a_domain_and_role() {
        for col in &committed().columns {
            if col.name == "seed" || col.name == "pin_set" || col.name == "refusal" {
                continue; // structural columns, not metrics
            }
            assert!(!col.domain.is_empty(), "{} has no domain", col.name);
            assert!(!col.role.is_empty(), "{} has no role", col.name);
        }
    }

    #[test]
    fn absent_values_are_counted_not_skipped() {
        let c = committed();
        let name = &c
            .columns
            .iter()
            .find(|c| c.kind == "numeric")
            .expect("a numeric column")
            .name
            .clone();
        assert_eq!(
            c.values(name).len() + c.absent_count(name),
            1000,
            "present + absent must account for every world"
        );
    }

    #[test]
    fn split_csv_line_handles_quoted_commas() {
        assert_eq!(
            split_csv_line(r#"a,"b,c",d"#),
            vec!["a".to_string(), "b,c".to_string(), "d".to_string()]
        );
    }

    #[test]
    fn split_csv_line_passes_unquoted_fields_through() {
        assert_eq!(
            split_csv_line("a,b,c"),
            vec!["a".to_string(), "b".to_string(), "c".to_string()]
        );
    }

    /// A minimal in-memory census: one real column (`seed`, always present)
    /// and one metric column that every row leaves empty — the case
    /// `absent_count` must report as "1000 absent", not confuse with a
    /// metric name that isn't a column at all.
    fn synthetic_census() -> Census {
        Census {
            columns: vec![
                Column {
                    name: "seed".to_string(),
                    kind: "integer".to_string(),
                    doc: String::new(),
                    domain: String::new(),
                    role: String::new(),
                },
                Column {
                    name: "always-absent".to_string(),
                    kind: "numeric".to_string(),
                    doc: "a metric no world reported".to_string(),
                    domain: "terrain".to_string(),
                    role: "descriptor".to_string(),
                },
            ],
            rows: vec![
                BTreeMap::from([
                    ("seed".to_string(), "1".to_string()),
                    ("always-absent".to_string(), String::new()),
                ]),
                BTreeMap::from([
                    ("seed".to_string(), "2".to_string()),
                    ("always-absent".to_string(), String::new()),
                ]),
            ],
        }
    }

    #[test]
    fn an_unknown_metric_is_distinguishable_from_an_all_absent_one() {
        let c = synthetic_census();

        // A real column every world left absent: has() sees it, and
        // absent_count reports every row.
        assert!(c.has("always-absent"));
        assert_eq!(c.absent_count("always-absent"), 2);

        // A name that is not a column at all: has() says so plainly,
        // instead of absent_count silently agreeing to report "2 absent"
        // for a metric that was never measured.
        assert!(!c.has("not-a-real-metric"));
    }

    #[test]
    #[should_panic(expected = "unknown census metric: not-a-real-metric")]
    fn values_panics_loudly_on_an_unknown_metric_rather_than_reporting_zero() {
        synthetic_census().values("not-a-real-metric");
    }

    #[test]
    #[should_panic(expected = "unknown census metric: not-a-real-metric")]
    fn absent_count_panics_loudly_on_an_unknown_metric_rather_than_reporting_all_absent() {
        synthetic_census().absent_count("not-a-real-metric");
    }

    #[test]
    fn load_refuses_a_schema_and_csv_that_disagree_on_columns() {
        let dir = std::env::temp_dir().join(format!(
            "hv-domesday-census-mismatch-test-{}",
            std::process::id()
        ));
        std::fs::create_dir_all(&dir).expect("create scratch dir");
        std::fs::write(
            dir.join("schema.json"),
            r#"{"columns":[
                {"name":"seed","kind":"integer"},
                {"name":"schema-only-metric","kind":"numeric","domain":"terrain","role":"descriptor"}
            ]}"#,
        )
        .expect("write schema.json");
        std::fs::write(dir.join("rows.csv"), "seed,csv-only-metric\n1,2\n")
            .expect("write rows.csv");

        let err = load(&dir).expect_err("mismatched columns must fail to load");
        assert!(
            err.contains("schema-only-metric"),
            "error should name the schema-only column: {err}"
        );
        assert!(
            err.contains("csv-only-metric"),
            "error should name the csv-only column: {err}"
        );

        std::fs::remove_dir_all(&dir).ok();
    }

    #[test]
    fn load_accepts_a_schema_and_csv_that_agree_on_columns() {
        let dir = std::env::temp_dir().join(format!(
            "hv-domesday-census-agree-test-{}",
            std::process::id()
        ));
        std::fs::create_dir_all(&dir).expect("create scratch dir");
        std::fs::write(
            dir.join("schema.json"),
            r#"{"columns":[
                {"name":"seed","kind":"integer"},
                {"name":"a-metric","kind":"numeric","domain":"terrain","role":"descriptor"}
            ]}"#,
        )
        .expect("write schema.json");
        std::fs::write(dir.join("rows.csv"), "seed,a-metric\n1,2\n").expect("write rows.csv");

        let c = load(&dir).expect("agreeing columns must load");
        assert_eq!(c.rows.len(), 1);

        std::fs::remove_dir_all(&dir).ok();
    }
}

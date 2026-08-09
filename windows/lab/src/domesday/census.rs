//! Loading the committed census table.
//!
//! Reads `rows.csv` + `schema.json` from a study's generated directory and
//! never rebuilds a world to do it (see the `domesday` module doc). The CSV
//! parser is quote-aware (RFC 4180-lite): several census columns
//! (`goblin-flagship-roles`, `kobold-flagship-roles`) hold comma-joined role
//! lists and are quoted in the committed fixture, so a naive `split(',')`
//! would shred them across extra fields.

use std::collections::BTreeMap;
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
pub struct Census {
    /// Column descriptors, in schema order.
    pub columns: Vec<Column>,
    /// One map per world, keyed by column name, holding the raw CSV text
    /// (empty string for an absent/refused value).
    pub rows: Vec<BTreeMap<String, String>>,
}

impl Census {
    /// Present (non-empty) values for a metric, in row order.
    /// type-audit: bare-ok(identifier-text: metric), bare-ok(artifact: return)
    pub fn values(&self, metric: &str) -> Vec<&str> {
        self.rows
            .iter()
            .filter_map(|r| r.get(metric).map(String::as_str))
            .filter(|v| !v.is_empty())
            .collect()
    }

    /// How many worlds have no value for this metric (absent, not present
    /// with an empty value coincidentally — the CSV never distinguishes the
    /// two, so this is "present count subtracted from total rows").
    /// type-audit: bare-ok(identifier-text: metric), bare-ok(count: return)
    pub fn absent_count(&self, metric: &str) -> usize {
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
/// (e.g. `book/src/laboratory/generated/the-census`).
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
}

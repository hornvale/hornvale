//! Comparators (real-world benchmarks) and expectations (declared
//! cross-metric relationships) for the census survey.
//!
//! Both are data, not code (decision 0011): `studies/comparators.json` names
//! real, measured worlds a metric's distribution can be checked against;
//! `studies/expectations.json` names cross-metric relationships a detector
//! should look for, and the *strength class* it should find them at — not a
//! numeric threshold. **Real worlds only for v1** (spec §8, ratified at G3):
//! an invented number rendered beside a measured one is the confusion this
//! programme exists to remove.

use std::collections::BTreeMap;
use std::path::Path;

/// A real-world data point to compare a census metric's distribution
/// against.
///
/// `band` is the declared tolerance: a detector fires when
/// `|median − value| > band` for the same metric key.
/// type-audit: bare-ok(identifier-text: name), bare-ok(artifact: values), bare-ok(artifact: band)
#[derive(Debug, Clone, PartialEq)]
pub struct Comparator {
    /// The real world this comparator names (e.g. `Earth`).
    pub name: String,
    /// Measured values, keyed by census metric name.
    pub values: BTreeMap<String, f64>,
    /// Declared tolerance per metric, keyed by the same metric names as
    /// `values`.
    pub band: BTreeMap<String, f64>,
}

/// A declared cross-metric relationship: `tracks` is expected to be an
/// influence on `metric`, at the declared strength class.
///
/// `declared` carries no numeric threshold — it names a strength *class*
/// (`dominant`, `strong`, `moderate`, `weak`, `none`) that a detector
/// reports the observed correlation against, rather than a frozen `r`
/// value chosen after the data was already seen.
/// type-audit: bare-ok(identifier-text: metric), bare-ok(identifier-text: tracks), bare-ok(prose: why), bare-ok(identifier-text: declared)
#[derive(Debug, Clone, PartialEq)]
pub struct Expectation {
    /// The census metric expected to respond.
    pub metric: String,
    /// The census metric expected to drive it.
    pub tracks: String,
    /// Why this relationship is expected, including any caveats about how
    /// well the census can actually measure it.
    pub why: String,
    /// The declared strength class, e.g. `dominant`.
    pub declared: String,
}

/// Read one field's value as a `&str`, erroring with the file and field
/// name if it is missing or not a string.
fn required_str<'a>(
    obj: &'a serde_json::Value,
    field: &str,
    path: &Path,
) -> Result<&'a str, String> {
    obj[field]
        .as_str()
        .ok_or_else(|| format!("{}: missing or non-string field {field}", path.display()))
}

/// Read a JSON object of `string -> number` as a `BTreeMap<String, f64>`,
/// erroring with the file and field name on anything else.
fn map_of_f64(
    obj: &serde_json::Value,
    field: &str,
    path: &Path,
) -> Result<BTreeMap<String, f64>, String> {
    obj[field]
        .as_object()
        .ok_or_else(|| format!("{}: missing or non-object field {field}", path.display()))?
        .iter()
        .map(|(k, v)| {
            let f = v
                .as_f64()
                .ok_or_else(|| format!("{}: {field}.{k} is not a number", path.display()))?;
            Ok((k.clone(), f))
        })
        .collect()
}

/// Load `comparators.json`'s `worlds` array.
///
/// Errors name the file (see `census::load`'s error style): a bad path, bad
/// JSON, or a missing/malformed field all return `Err(String)` describing
/// what went wrong, rather than panicking.
/// type-audit: bare-ok(prose: return)
pub fn load_comparators(path: &Path) -> Result<Vec<Comparator>, String> {
    let text = std::fs::read_to_string(path).map_err(|e| format!("{}: {e}", path.display()))?;
    let json: serde_json::Value =
        serde_json::from_str(&text).map_err(|e| format!("{}: parse: {e}", path.display()))?;
    let worlds = json["worlds"]
        .as_array()
        .ok_or_else(|| format!("{}: no worlds array", path.display()))?;
    worlds
        .iter()
        .map(|w| {
            Ok(Comparator {
                name: required_str(w, "name", path)?.to_string(),
                values: map_of_f64(w, "values", path)?,
                band: map_of_f64(w, "band", path)?,
            })
        })
        .collect()
}

/// Load `expectations.json`'s `expect` array.
///
/// Errors name the file (see `census::load`'s error style).
/// type-audit: bare-ok(prose: return)
pub fn load_expectations(path: &Path) -> Result<Vec<Expectation>, String> {
    let text = std::fs::read_to_string(path).map_err(|e| format!("{}: {e}", path.display()))?;
    let json: serde_json::Value =
        serde_json::from_str(&text).map_err(|e| format!("{}: parse: {e}", path.display()))?;
    let expect = json["expect"]
        .as_array()
        .ok_or_else(|| format!("{}: no expect array", path.display()))?;
    expect
        .iter()
        .map(|e| {
            Ok(Expectation {
                metric: required_str(e, "metric", path)?.to_string(),
                tracks: required_str(e, "tracks", path)?.to_string(),
                why: required_str(e, "why", path)?.to_string(),
                declared: required_str(e, "declared", path)?.to_string(),
            })
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::domesday::census::repo_root;

    #[test]
    fn comparators_load() {
        let cs = load_comparators(&repo_root().join("studies/comparators.json")).expect("load");
        let earth = cs
            .iter()
            .find(|c| c.name == "Earth")
            .expect("Earth present");
        assert!((earth.values["mean-land-temperature-c"] - 14.0).abs() < 1e-9);
    }

    #[test]
    fn a_comparator_naming_an_unknown_metric_is_rejected() {
        let c = crate::domesday::census::load(
            &repo_root().join("book/src/laboratory/generated/the-census"),
        )
        .expect("census");
        let known: Vec<&str> = c.columns.iter().map(|x| x.name.as_str()).collect();
        for cmp in load_comparators(&repo_root().join("studies/comparators.json")).expect("load") {
            for m in cmp.values.keys() {
                assert!(
                    known.contains(&m.as_str()),
                    "{} names unknown metric {m}",
                    cmp.name
                );
            }
        }
    }

    #[test]
    fn expectations_name_known_metrics() {
        let c = crate::domesday::census::load(
            &repo_root().join("book/src/laboratory/generated/the-census"),
        )
        .expect("census");
        let known: Vec<&str> = c.columns.iter().map(|x| x.name.as_str()).collect();
        for e in load_expectations(&repo_root().join("studies/expectations.json")).expect("load") {
            assert!(
                known.contains(&e.metric.as_str()),
                "unknown metric {}",
                e.metric
            );
            assert!(
                known.contains(&e.tracks.as_str()),
                "unknown driver {}",
                e.tracks
            );
        }
    }
}

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
/// `|median − value| > band` for the same metric key. `kind` is always
/// `"real"` — `load_comparators` rejects anything else (real worlds only
/// for v1, spec §8, ratified at G3), so a caller can trust every
/// `Comparator` it receives without re-checking.
/// type-audit: bare-ok(identifier-text: name), bare-ok(identifier-text: kind), bare-ok(artifact: values), bare-ok(artifact: band)
#[derive(Debug, Clone, PartialEq)]
pub struct Comparator {
    /// The real world this comparator names (e.g. `Earth`).
    pub name: String,
    /// Always `"real"` — see the struct doc.
    pub kind: String,
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
/// (`dominant`, `strong`, `moderate`, `weak`, `none`; see
/// [`DECLARED_CLASSES`]) that a detector reports the observed correlation
/// against, rather than a frozen `r` value chosen after the data was
/// already seen. `load_expectations` rejects any other value, so a caller
/// can trust `declared` is one of the five without re-checking.
/// type-audit: bare-ok(identifier-text: metric), bare-ok(identifier-text: tracks), bare-ok(prose: why), bare-ok(identifier-text: declared), bare-ok(identifier-text: direction)
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
    /// The declared sign of the relationship: `positive`, `negative`, or
    /// `none`. A `declared` of `none` requires this to be `none` too — a claim
    /// of no relationship has no sign.
    pub direction: String,
}

/// The permitted values of [`Expectation::declared`] (conventional
/// effect-size bands): `dominant` (`|r| ≥ 0.7`), `strong` (`0.5–0.7`),
/// `moderate` (`0.3–0.5`), `weak` (`0.1–0.3`), `none` (`< 0.1`).
/// `load_expectations` rejects anything outside this set at load, rather
/// than letting a typo survive to silently never match in a later
/// detector.
/// type-audit: bare-ok(identifier-text)
pub const DECLARED_CLASSES: &[&str] = &["dominant", "strong", "moderate", "weak", "none"];

/// The permitted values of [`Expectation::direction`]. A backwards coupling —
/// the right strength with the wrong sign — is a defect a strength-only check
/// cannot see, which is why this field exists (spec §2).
/// type-audit: bare-ok(identifier-text)
pub const DIRECTIONS: &[&str] = &["positive", "negative", "none"];

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
/// what went wrong, rather than panicking. Rejects any comparator whose
/// `kind` is not `"real"`, naming the offending world and its kind — real
/// worlds only for v1 (spec §8, ratified at G3), enforced here rather than
/// left to convention and review.
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
            let name = required_str(w, "name", path)?.to_string();
            let kind = required_str(w, "kind", path)?.to_string();
            if kind != "real" {
                return Err(format!(
                    "{}: comparator {name:?} has kind {kind:?}, but only \"real\" \
                     comparators are supported (real worlds only for v1, spec §8)",
                    path.display()
                ));
            }
            Ok(Comparator {
                name,
                kind,
                values: map_of_f64(w, "values", path)?,
                band: map_of_f64(w, "band", path)?,
            })
        })
        .collect()
}

/// Load `expectations.json`'s `expect` array.
///
/// Errors name the file (see `census::load`'s error style). Rejects any
/// `declared` value outside [`DECLARED_CLASSES`], naming the offending
/// value and the permitted set — a typo'd class must fail loudly here
/// rather than survive to a detector that then silently never matches it.
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
            let metric = required_str(e, "metric", path)?.to_string();
            let tracks = required_str(e, "tracks", path)?.to_string();
            let why = required_str(e, "why", path)?.to_string();
            let declared = required_str(e, "declared", path)?.to_string();
            if !DECLARED_CLASSES.contains(&declared.as_str()) {
                return Err(format!(
                    "{}: expectation {metric:?} declares unknown strength class {declared:?}; \
                     must be one of {DECLARED_CLASSES:?}",
                    path.display()
                ));
            }
            let direction = required_str(e, "direction", path)?.to_string();
            if !DIRECTIONS.contains(&direction.as_str()) {
                return Err(format!(
                    "{}: expectation for {metric} declares direction {direction:?}, \
                     which is not one of {DIRECTIONS:?}",
                    path.display()
                ));
            }
            if (declared == "none") != (direction == "none") {
                return Err(format!(
                    "{}: expectation for {metric} pairs declared {declared:?} with \
                     direction {direction:?}; `none` must appear in both or neither \
                     — a claim of no relationship has no sign",
                    path.display()
                ));
            }
            Ok(Expectation {
                metric,
                tracks,
                why,
                declared,
                direction,
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
    fn a_comparator_naming_an_invented_kind_is_rejected() {
        let path = std::env::temp_dir().join(format!(
            "hv-domesday-comparators-invented-kind-{}.json",
            std::process::id()
        ));
        std::fs::write(
            &path,
            r#"{"worlds":[{"name":"Arrakis","kind":"invented","values":{"mean-land-temperature-c":45.0},"band":{"mean-land-temperature-c":5.0}}]}"#,
        )
        .expect("write fixture");

        let err = load_comparators(&path).expect_err("an invented comparator must be rejected");
        assert!(
            err.contains("Arrakis"),
            "error should name the offending world: {err}"
        );
        assert!(
            err.contains("invented"),
            "error should name the offending kind: {err}"
        );

        std::fs::remove_file(&path).ok();
    }

    #[test]
    fn an_expectation_with_an_unknown_declared_class_is_rejected() {
        let path = std::env::temp_dir().join(format!(
            "hv-domesday-expectations-typo-declared-{}.json",
            std::process::id()
        ));
        std::fs::write(
            &path,
            r#"{"expect":[{"metric":"mean-land-temperature-c","tracks":"year-std-days","why":"typo test","declared":"domnant"}]}"#,
        )
        .expect("write fixture");

        let err = load_expectations(&path).expect_err("an unknown declared class must be rejected");
        assert!(
            err.contains("domnant"),
            "error should name the offending value: {err}"
        );
        assert!(
            err.contains("dominant"),
            "error should name the permitted set: {err}"
        );

        std::fs::remove_file(&path).ok();
    }

    #[test]
    fn an_expectation_with_an_unknown_direction_is_rejected() {
        let dir = std::env::temp_dir().join("armature-dir-test");
        std::fs::create_dir_all(&dir).expect("tmp");
        let p = dir.join("expectations.json");
        std::fs::write(
            &p,
            r#"{"expect":[{"metric":"a","tracks":"b","why":"w","declared":"weak","direction":"sideways"}]}"#,
        )
        .expect("write");
        let err = load_expectations(&p).expect_err("an unknown direction must be rejected");
        assert!(
            err.contains("sideways"),
            "error must name the offending value: {err}"
        );
        assert!(
            err.contains("positive"),
            "error must name the permitted set: {err}"
        );
    }

    #[test]
    fn declared_none_requires_direction_none() {
        let dir = std::env::temp_dir().join("armature-none-test");
        std::fs::create_dir_all(&dir).expect("tmp");
        let p = dir.join("expectations.json");
        std::fs::write(
            &p,
            r#"{"expect":[{"metric":"a","tracks":"b","why":"w","declared":"none","direction":"positive"}]}"#,
        )
        .expect("write");
        let err = load_expectations(&p)
            .expect_err("declared none with a signed direction must be rejected");
        assert!(
            err.contains("none"),
            "error must explain the coupling: {err}"
        );
    }

    #[test]
    fn a_well_formed_signed_expectation_loads() {
        let dir = std::env::temp_dir().join("armature-ok-test");
        std::fs::create_dir_all(&dir).expect("tmp");
        let p = dir.join("expectations.json");
        std::fs::write(
            &p,
            r#"{"expect":[{"metric":"a","tracks":"b","why":"w","declared":"dominant","direction":"negative"}]}"#,
        )
        .expect("write");
        let es = load_expectations(&p).expect("loads");
        assert_eq!(es[0].direction, "negative");
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

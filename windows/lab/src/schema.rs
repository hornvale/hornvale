//! The self-describing census manifest: `schema.json` (census-as-data spec
//! §2). Emitted next to `rows.csv` from the same `RunResult`, so the
//! manifest can never disagree with the CSV it sits beside. Deterministic:
//! serde_json's default map is a BTreeMap (alphabetical keys); arrays keep
//! insertion order, so `columns` preserves the CSV's exact header order.

use crate::{RunResult, SummaryKind};
use hornvale_kernel::quantize;
use hornvale_worldgen::BuildDepth;

/// FNV-1a 64-bit hash of `bytes` — the manifest ↔ `rows.csv` integrity
/// binding (spec §2): mismatch detection, not security.
/// type-audit: bare-ok(artifact: bytes), bare-ok(diagnostic-value: return)
pub fn fnv1a64(bytes: &[u8]) -> u64 {
    let mut hash: u64 = 0xcbf2_9ce4_8422_2325;
    for byte in bytes {
        hash ^= u64::from(*byte);
        hash = hash.wrapping_mul(0x0000_0100_0000_01b3);
    }
    hash
}

/// Render `schema.json` for a run result whose committed CSV bytes are
/// `csv`. `backfilled` marks a manifest generated after the fact for a
/// frozen study (spec §2, "Backfill for the frozen tier").
/// type-audit: bare-ok(artifact: csv), bare-ok(flag: backfilled), bare-ok(artifact: return)
pub fn render_schema(result: &RunResult, csv: &str, backfilled: bool) -> String {
    let metrics = result
        .study
        .selected_metrics()
        .expect("the study validated at load time");

    let mut columns: Vec<serde_json::Value> = Vec::new();
    columns.push(serde_json::json!({ "name": "seed", "kind": "integer" }));
    columns.push(serde_json::json!({ "name": "pin_set", "kind": "categorical" }));
    for name in &result.metric_names {
        let metric = metrics
            .iter()
            .find(|m| m.name == *name)
            .expect("metric_names come from the registry");
        let rung = match metric.rung() {
            BuildDepth::Astronomy => "astronomy",
            BuildDepth::Terrain => "terrain",
            BuildDepth::Settlements => "settlements",
            BuildDepth::Full => "full",
        };
        let mut column = serde_json::json!({
            "name": metric.name,
            "doc": metric.doc,
            "rung": rung,
        });
        match &metric.summary {
            SummaryKind::Categorical => {
                column["kind"] = serde_json::json!("categorical");
            }
            SummaryKind::Flag => {
                column["kind"] = serde_json::json!("flag");
            }
            SummaryKind::Numeric { bucket_edges } => {
                column["kind"] = serde_json::json!("numeric");
                let edges: Vec<f64> = bucket_edges.iter().map(|e| quantize(*e)).collect();
                column["buckets"] = serde_json::json!(edges);
            }
        }
        columns.push(column);
    }
    columns.push(serde_json::json!({ "name": "refusal", "kind": "categorical" }));

    let pin_sets: Vec<serde_json::Value> = result
        .study
        .pin_sets
        .iter()
        .map(|ps| {
            serde_json::json!({
                "label": ps.label,
                "pins": ps.pins,
                "roster": ps.roster,
            })
        })
        .collect();

    let mut manifest = serde_json::json!({
        "schema_version": 1,
        "study": {
            "name": result.study.name,
            "description": result.study.description,
            "seeds": { "from": result.study.seeds.from, "count": result.study.seeds.count },
            "pin_sets": pin_sets,
        },
        "conventions": { "float_quantization": "8-significant-digits" },
        "rows": {
            "count": result.rows.len(),
            "fnv1a64": format!("0x{:016x}", fnv1a64(csv.as_bytes())),
        },
        "columns": columns,
    });
    if backfilled {
        manifest["backfilled"] = serde_json::json!(true);
    }

    let mut out = serde_json::to_string_pretty(&manifest).expect("manifest serializes");
    out.push('\n');
    out
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{MetricSelection, MetricValue, PinSet, Row, Seeds, Study};

    /// A two-seed study over one metric of each kind, mirroring the publish
    /// tests' builder. `star-class` is Categorical, `ocean-fraction` is
    /// Numeric, `tidally-locked` is Flag in the registry.
    fn build_result() -> RunResult {
        let study = Study {
            name: "schema-study".to_string(),
            description: "for schema tests".to_string(),
            seeds: Seeds { from: 0, count: 2 },
            pin_sets: vec![PinSet {
                label: "default".to_string(),
                pins: vec![],
                roster: None,
            }],
            metrics: MetricSelection::Named(vec![
                "star-class".to_string(),
                "ocean-fraction".to_string(),
                "tidally-locked".to_string(),
            ]),
        };
        RunResult {
            study,
            metric_names: vec!["star-class", "ocean-fraction", "tidally-locked"],
            rows: (0..2u64)
                .map(|seed| Row {
                    seed,
                    pin_set: "default".to_string(),
                    values: vec![
                        MetricValue::Text("G".to_string()),
                        MetricValue::Number(0.5),
                        MetricValue::Flag(false),
                    ],
                    refusal: None,
                })
                .collect(),
        }
    }

    #[test]
    fn fnv1a64_matches_the_published_vectors() {
        assert_eq!(fnv1a64(b""), 0xcbf2_9ce4_8422_2325);
        assert_eq!(fnv1a64(b"a"), 0xaf63_dc4c_8601_ec8c);
        assert_eq!(fnv1a64(b"foobar"), 0x85944171f73967e8);
    }

    #[test]
    fn schema_columns_agree_with_the_csv_header_in_order() {
        let result = build_result();
        let csv = crate::runner::render_csv(&result);
        let schema = render_schema(&result, &csv, false);
        let parsed: serde_json::Value = serde_json::from_str(&schema).unwrap();
        let names: Vec<&str> = parsed["columns"]
            .as_array()
            .unwrap()
            .iter()
            .map(|c| c["name"].as_str().unwrap())
            .collect();
        let header = csv.lines().next().unwrap();
        assert_eq!(names.join(","), header);
    }

    #[test]
    fn schema_types_documents_and_binds_the_csv() {
        let result = build_result();
        let csv = crate::runner::render_csv(&result);
        let schema = render_schema(&result, &csv, false);
        let parsed: serde_json::Value = serde_json::from_str(&schema).unwrap();

        assert_eq!(parsed["schema_version"], 1);
        assert_eq!(parsed["study"]["name"], "schema-study");
        assert_eq!(parsed["study"]["seeds"]["count"], 2);
        assert_eq!(
            parsed["conventions"]["float_quantization"],
            "8-significant-digits"
        );
        assert_eq!(parsed["rows"]["count"], 2);
        assert_eq!(
            parsed["rows"]["fnv1a64"],
            format!("0x{:016x}", fnv1a64(csv.as_bytes()))
        );
        // Backfilled flag absent on live manifests.
        assert!(parsed.get("backfilled").is_none());

        let cols = parsed["columns"].as_array().unwrap();
        assert_eq!(cols[0]["name"], "seed");
        assert_eq!(cols[0]["kind"], "integer");
        assert_eq!(cols[1]["name"], "pin_set");
        assert_eq!(cols[1]["kind"], "categorical");
        let star = &cols[2];
        assert_eq!(star["kind"], "categorical");
        assert!(star["doc"].as_str().unwrap().len() > 1);
        assert!(star["rung"].as_str().is_some());
        let ocean = &cols[3];
        assert_eq!(ocean["kind"], "numeric");
        assert!(ocean["buckets"].as_array().unwrap().len() > 1);
        let locked = &cols[4];
        assert_eq!(locked["kind"], "flag");
        assert_eq!(cols.last().unwrap()["name"], "refusal");
        assert_eq!(cols.last().unwrap()["kind"], "categorical");
    }

    #[test]
    fn schema_is_deterministic_and_backfill_flag_appears_when_set() {
        let result = build_result();
        let csv = crate::runner::render_csv(&result);
        assert_eq!(
            render_schema(&result, &csv, false),
            render_schema(&result, &csv, false)
        );
        let back: serde_json::Value =
            serde_json::from_str(&render_schema(&result, &csv, true)).unwrap();
        assert_eq!(back["backfilled"], true);
    }
}

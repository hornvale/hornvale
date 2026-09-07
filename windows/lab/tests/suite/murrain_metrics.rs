//! The Murrain's stable census surface.
//!
//! These checks bind the six preregistered columns to their subject domains
//! and to the source facts named in their published documentation. They are
//! deliberately registry-level: an additive census schema can predate these
//! columns without making an older measurement false, while the live metric
//! contract must name them exactly.

use hornvale_lab::{
    MetricSelection, PinSet, RunResult, Seeds, Study, SummaryKind, registry, render_schema,
};

struct ExpectedMetric {
    name: &'static str,
    domain: &'static str,
    sources: &'static [&'static str],
    flag: bool,
}

const MURRAIN_METRICS: [ExpectedMetric; 6] = [
    ExpectedMetric {
        name: "epidemic-largest-metapopulation-now",
        domain: "demography",
        sources: &["occ-founded", "occ-ended", "occ-peak", "occ-person-years"],
        flag: false,
    },
    ExpectedMetric {
        name: "epidemic-crowd-endemic",
        domain: "biology",
        sources: &["pathogen catalogue", "epidemic-largest-metapopulation-now"],
        flag: true,
    },
    ExpectedMetric {
        name: "epidemic-plague-endings",
        domain: "history",
        sources: &["occ-cause"],
        flag: false,
    },
    ExpectedMetric {
        name: "epidemic-outbreak-events",
        domain: "history",
        sources: &["struck-by", "outbreak-deaths"],
        flag: false,
    },
    ExpectedMetric {
        name: "lot-named-disease-deaths",
        domain: "history",
        sources: &["Life.cause", "struck-by", "outbreak-deaths"],
        flag: false,
    },
    ExpectedMetric {
        name: "lot-slots-filled-mean",
        domain: "history",
        sources: &["Story.slots"],
        flag: false,
    },
];

#[test]
fn six_murrain_metrics_have_stable_names_domains_kinds_and_sources() {
    let metrics = registry();
    for expected in &MURRAIN_METRICS {
        let metric = metrics
            .iter()
            .find(|metric| metric.name == expected.name)
            .unwrap_or_else(|| panic!("missing Murrain metric {}", expected.name));
        assert_eq!(
            metric.domain.as_str(),
            expected.domain,
            "{} domain",
            expected.name
        );
        assert_eq!(
            matches!(metric.summary, SummaryKind::Flag),
            expected.flag,
            "{} summary kind",
            expected.name
        );
        for source in expected.sources {
            assert!(
                metric.doc.contains(source),
                "{} documentation must name source {source:?}; got {:?}",
                expected.name,
                metric.doc
            );
        }
    }
}

#[test]
fn six_murrain_metrics_render_as_additive_full_rung_schema_columns() {
    let study = Study {
        name: "murrain-schema-check".to_string(),
        description: "The Murrain additive schema check".to_string(),
        seeds: Seeds { from: 42, count: 1 },
        pin_sets: vec![PinSet {
            label: "default".to_string(),
            pins: vec![],
            roster: None,
        }],
        metrics: MetricSelection::Named(
            MURRAIN_METRICS
                .iter()
                .map(|metric| metric.name.to_string())
                .collect(),
        ),
    };
    let result = RunResult {
        study,
        metric_names: MURRAIN_METRICS.iter().map(|metric| metric.name).collect(),
        rows: vec![],
    };
    let schema: serde_json::Value =
        serde_json::from_str(&render_schema(&result, "seed,pin_set,refusal\n", false))
            .expect("Murrain schema renders as JSON");
    let columns = schema["columns"].as_array().expect("columns are an array");

    for expected in &MURRAIN_METRICS {
        let column = columns
            .iter()
            .find(|column| column["name"] == expected.name)
            .unwrap_or_else(|| panic!("schema is missing {}", expected.name));
        assert_eq!(
            column["domain"], expected.domain,
            "{} domain",
            expected.name
        );
        assert_eq!(column["rung"], "full", "{} rung", expected.name);
        assert_eq!(
            column["kind"],
            if expected.flag { "flag" } else { "numeric" },
            "{} kind",
            expected.name
        );
    }
}

//! The Murrain's stable census surface.
//!
//! These checks bind the six preregistered columns to their subject domains
//! and to the source facts named in their published documentation. They are
//! deliberately registry-level: an additive census schema can predate these
//! columns without making an older measurement false, while the live metric
//! contract must name them exactly.

use hornvale_astronomy::SkyPins;
use hornvale_kernel::{Seed, Value};
use hornvale_lab::{
    Extractor, FullView, MetricSelection, MetricValue, PinSet, RunResult, Seeds, Study,
    SummaryKind, registry, render_schema,
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
fn murrain_extractors_bind_to_their_documented_source_facts_and_draws() {
    let view = FullView::build(Seed(42), &SkyPins::default()).expect("seed 42 builds");
    let extract = |name: &str| {
        let metric = registry()
            .into_iter()
            .find(|metric| metric.name == name)
            .unwrap();
        match metric.extract {
            Extractor::Full(f) => f(&view),
            _ => panic!("{name} must use FullView"),
        }
    };
    let number = |name: &str| match extract(name) {
        MetricValue::Number(value) => value,
        other => panic!("{name} must be numeric, got {other:?}"),
    };

    let ctx = hornvale_lot::context::assemble_from(view.world(), view.terrain(), view.climate())
        .expect("Lot source context assembles");
    assert_eq!(
        number("epidemic-largest-metapopulation-now"),
        ctx.largest_metapopulation_at(ctx.present_year)
    );
    let population = ctx.largest_metapopulation_at(ctx.present_year);
    let crowd_endemic = hornvale_species::pathogen_registry()
        .iter()
        .any(|(_, traits)| {
            traits.class == hornvale_species::PathogenClass::Crowd
                && traits
                    .r0
                    .zip(traits.infectious_years)
                    .is_some_and(|(r0, years)| {
                        let ccs =
                            hornvale_epidemiology::critical_community_size(r0, years, 1.0 / 30.0);
                        hornvale_epidemiology::persists(population, ccs)
                    })
        });
    assert_eq!(
        extract("epidemic-crowd-endemic"),
        MetricValue::Flag(crowd_endemic)
    );

    let records = hornvale_worldgen::occupation_records(view.world());
    let plague_endings = records
        .iter()
        .filter(|record| record.core.cause == Some(hornvale_history::record::CauseOfEnd::Plague))
        .count() as f64;
    assert_eq!(number("epidemic-plague-endings"), plague_endings);
    let first_plague = view
        .world()
        .ledger
        .find("occ-cause")
        .filter_map(|fact| match (&fact.object, fact.day) {
            (Value::Text(cause), Some(day)) if cause == "plague" => Some(day.as_std_days()),
            _ => None,
        })
        .min_by(f64::total_cmp);
    assert_eq!(
        extract("first-day-occ-cause-plague"),
        first_plague.map_or(MetricValue::Absent, MetricValue::Number)
    );
    assert_eq!(
        number("epidemic-outbreak-events"),
        view.world()
            .ledger
            .find(hornvale_epidemiology::STRUCK_BY)
            .count() as f64
    );

    let lots: Vec<_> = (0..200u64)
        .map(|index| {
            let life = hornvale_lot::draw::draw(
                &ctx,
                hornvale_lot::LotIndex(index),
                &hornvale_lot::Pick::default(),
            )
            .unwrap();
            let story = hornvale_lot::slots::tell(view.world(), &ctx, &life);
            (life, story)
        })
        .collect();
    let named_deaths = lots
        .iter()
        .filter(|(life, _)| {
            matches!(
                life.cause,
                Some(hornvale_lot::draw::DeathCause::Pathogen(_))
            )
        })
        .count() as f64;
    assert_eq!(number("lot-named-disease-deaths"), named_deaths);
    let excluded = ["sex", "family", "work", "literacy"];
    let filled: usize = lots
        .iter()
        .map(|(_, story)| {
            story
                .slots
                .iter()
                .filter(|slot| {
                    !excluded.contains(&slot.key)
                        && matches!(slot.value, hornvale_lot::slots::SlotValue::Filled(_))
                })
                .count()
        })
        .sum();
    assert_eq!(
        number("lot-slots-filled-mean"),
        filled as f64 / lots.len() as f64
    );
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

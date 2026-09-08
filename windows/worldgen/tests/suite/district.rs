use hornvale_kernel::{Seed, World, WorldTime};
use hornvale_worldgen::district::{
    DistrictBasis, DistrictConfig, DistrictId, DistrictInterval, DistrictStatus, project_districts,
};
use hornvale_worldgen::relation::{
    RelationAssertion, RelationDirection, RelationDirectionPolicy, RelationInterval, RelationKind,
    RelationMeasure, RelationMeasureFilter, RelationParticipant, RelationProvenance,
    RelationRecurrence, RelationReference, RelationRole, RelationView,
};
use std::collections::BTreeSet;
use std::thread;
use std::time::Duration;

fn reference(name: &str) -> RelationReference {
    RelationReference::new(name)
}

fn refs(names: &[&str]) -> BTreeSet<RelationReference> {
    names.iter().map(|name| reference(name)).collect()
}

fn relation_interval(start: i64, end: i64) -> RelationInterval {
    RelationInterval {
        start: WorldTime::from_ticks(start),
        end: WorldTime::from_ticks(end),
    }
}

fn district_interval(start: i64, end: i64) -> DistrictInterval {
    DistrictInterval {
        start: WorldTime::from_ticks(start),
        end: WorldTime::from_ticks(end),
    }
}

fn assertion(
    kind: RelationKind,
    from: &str,
    to: &str,
    direction: RelationDirection,
    measure: f64,
) -> RelationAssertion {
    assertion_during(kind, from, to, direction, measure, 0, 10)
}

fn assertion_during(
    kind: RelationKind,
    from: &str,
    to: &str,
    direction: RelationDirection,
    measure: f64,
    start: i64,
    end: i64,
) -> RelationAssertion {
    RelationAssertion {
        kind,
        participants: vec![
            RelationParticipant {
                reference: reference(from),
                role: RelationRole::new("source"),
            },
            RelationParticipant {
                reference: reference(to),
                role: RelationRole::new("target"),
            },
        ],
        interval: relation_interval(start, end),
        recurrence: RelationRecurrence::Once,
        direction,
        measure: RelationMeasure::new(measure),
        provenance: RelationProvenance::new(format!("fixture:{from}:{to}")),
    }
}

fn config(direction: RelationDirectionPolicy) -> DistrictConfig {
    DistrictConfig {
        direction,
        measure: RelationMeasureFilter::all(),
        minimum_members: 2,
        minimum_evidence: 1,
        minimum_duration_ticks: 1,
        maximum_containment_depth: 2,
        maximum_overlaps_per_district: 4,
    }
}

fn district_id(basis: DistrictBasis, interval: DistrictInterval, anchor: &str) -> DistrictId {
    DistrictId {
        basis,
        interval,
        anchor: reference(anchor),
    }
}

#[test]
fn the_row_forms_one_spatial_district_with_canonical_structure() {
    // Catches a projector that follows insertion order, omits boundary/bridge
    // structure, or derives identity from anything but the canonical anchor.
    let view = RelationView::new(vec![
        assertion(
            RelationKind::SpatialAdjacency,
            "locus:b",
            "locus:c",
            RelationDirection::Symmetric,
            1.0,
        ),
        assertion(
            RelationKind::SpatialAdjacency,
            "locus:b",
            "locus:a",
            RelationDirection::Symmetric,
            1.0,
        ),
    ])
    .unwrap();
    let interval = district_interval(0, 10);
    let mut cfg = config(RelationDirectionPolicy::Symmetric);
    cfg.minimum_evidence = 2;

    let projected = project_districts(&view, DistrictBasis::Spatial, interval, &cfg);

    assert_eq!(projected.status, DistrictStatus::Resolved);
    assert_eq!(projected.districts.len(), 1);
    let row = &projected.districts[0];
    assert_eq!(
        row.id,
        district_id(DistrictBasis::Spatial, interval, "locus:a")
    );
    assert_eq!(row.members, refs(&["locus:a", "locus:b", "locus:c"]));
    assert_eq!(row.boundary, refs(&["locus:a", "locus:c"]));
    assert_eq!(row.bridge_members, refs(&["locus:b"]));
    assert_eq!(row.evidence.len(), 2);
    assert_eq!(row.status, DistrictStatus::Resolved);
}

#[test]
fn the_fork_marks_its_bridge_without_inventing_hierarchy() {
    // Catches degree-only bridge detection and accidental parent assignment
    // inside one ordinary connected component.
    let view = RelationView::new(vec![
        assertion(
            RelationKind::SpatialAdjacency,
            "locus:hub",
            "locus:north",
            RelationDirection::Symmetric,
            1.0,
        ),
        assertion(
            RelationKind::SpatialAdjacency,
            "locus:hub",
            "locus:east",
            RelationDirection::Symmetric,
            1.0,
        ),
        assertion(
            RelationKind::SpatialAdjacency,
            "locus:hub",
            "locus:west",
            RelationDirection::Symmetric,
            1.0,
        ),
    ])
    .unwrap();

    let projected = project_districts(
        &view,
        DistrictBasis::Spatial,
        district_interval(0, 10),
        &config(RelationDirectionPolicy::Symmetric),
    );
    let fork = &projected.districts[0];

    assert_eq!(fork.bridge_members, refs(&["locus:hub"]));
    assert_eq!(
        fork.boundary,
        refs(&["locus:east", "locus:north", "locus:west"])
    );
    assert_eq!(fork.parent, None);
}

#[test]
fn the_reach_projects_distant_exchange_without_joining_spatial_components() {
    // Catches cross-basis leakage: exchange evidence must never fabricate a
    // spatial edge even when both bases name the same aggregate references.
    let view = RelationView::new(vec![
        assertion(
            RelationKind::Exchange,
            "cohort:a",
            "cohort:b",
            RelationDirection::Reciprocal,
            9.0,
        ),
        assertion(
            RelationKind::SpatialAdjacency,
            "cohort:a",
            "locus:a",
            RelationDirection::Symmetric,
            1.0,
        ),
        assertion(
            RelationKind::SpatialAdjacency,
            "cohort:b",
            "locus:b",
            RelationDirection::Symmetric,
            1.0,
        ),
    ])
    .unwrap();
    let interval = district_interval(0, 10);

    let exchange = project_districts(
        &view,
        DistrictBasis::Exchange,
        interval,
        &config(RelationDirectionPolicy::Reciprocal),
    );
    let spatial = project_districts(
        &view,
        DistrictBasis::Spatial,
        interval,
        &config(RelationDirectionPolicy::Symmetric),
    );

    assert_eq!(exchange.districts.len(), 1);
    assert_eq!(
        exchange.districts[0].members,
        refs(&["cohort:a", "cohort:b"])
    );
    assert_eq!(spatial.districts.len(), 2);
    assert!(
        spatial
            .districts
            .iter()
            .all(|district| district.members != refs(&["cohort:a", "cohort:b"]))
    );
    assert_eq!(exchange.refusals.len(), 2);
    assert_eq!(spatial.refusals.len(), 1);
}

#[test]
fn the_gate_preserves_one_way_reach_and_refuses_singleton_membership() {
    // Catches reversal of a directed edge and the default promotion of a
    // source with no outward evidence into a singleton district.
    let view = RelationView::new(vec![assertion(
        RelationKind::Access,
        "locus:gate",
        "locus:inside",
        RelationDirection::Directed,
        1.0,
    )])
    .unwrap();
    let interval = district_interval(0, 10);

    let outward = project_districts(
        &view,
        DistrictBasis::Access,
        interval,
        &config(RelationDirectionPolicy::SourceReachable(reference(
            "locus:gate",
        ))),
    );
    let inward = project_districts(
        &view,
        DistrictBasis::Access,
        interval,
        &config(RelationDirectionPolicy::SourceReachable(reference(
            "locus:inside",
        ))),
    );

    assert_eq!(outward.status, DistrictStatus::Resolved);
    assert_eq!(
        outward.districts[0].members,
        refs(&["locus:gate", "locus:inside"])
    );
    assert_eq!(inward.status, DistrictStatus::InsufficientEvidence);
    assert!(inward.districts.is_empty());
}

#[test]
fn the_weave_reports_cross_basis_overlap_explicitly() {
    // Catches an overlap implementation that compares district ids rather
    // than member references (ids intentionally differ across bases).
    let view = RelationView::new(vec![
        assertion(
            RelationKind::Presence,
            "cohort:a",
            "locus:market",
            RelationDirection::Symmetric,
            1.0,
        ),
        assertion(
            RelationKind::Presence,
            "cohort:b",
            "locus:market",
            RelationDirection::Symmetric,
            1.0,
        ),
        assertion(
            RelationKind::Exchange,
            "cohort:a",
            "cohort:b",
            RelationDirection::Reciprocal,
            1.0,
        ),
    ])
    .unwrap();
    let interval = district_interval(0, 10);
    let presence = project_districts(
        &view,
        DistrictBasis::Presence,
        interval,
        &config(RelationDirectionPolicy::Symmetric),
    );
    let exchange = project_districts(
        &view,
        DistrictBasis::Exchange,
        interval,
        &config(RelationDirectionPolicy::Reciprocal),
    );

    let overlap = presence.overlaps_with(&exchange);
    assert_eq!(overlap.len(), 1);
    assert_eq!(overlap[0].members, refs(&["cohort:a", "cohort:b"]));
    assert_ne!(overlap[0].left, overlap[0].right);
}

#[test]
fn the_false_bridge_is_filtered_before_components_are_formed() {
    // Catches filtering after component formation, where one weak accidental
    // edge would already have merged two well-supported groups.
    let view = RelationView::new(vec![
        assertion(
            RelationKind::SpatialAdjacency,
            "locus:left-a",
            "locus:left-b",
            RelationDirection::Symmetric,
            1.0,
        ),
        assertion(
            RelationKind::SpatialAdjacency,
            "locus:right-a",
            "locus:right-b",
            RelationDirection::Symmetric,
            1.0,
        ),
        assertion(
            RelationKind::SpatialAdjacency,
            "locus:left-b",
            "locus:right-a",
            RelationDirection::Symmetric,
            0.1,
        ),
    ])
    .unwrap();
    let mut cfg = config(RelationDirectionPolicy::Symmetric);
    cfg.measure = RelationMeasureFilter::at_least(0.5);

    let projected = project_districts(
        &view,
        DistrictBasis::Spatial,
        district_interval(0, 10),
        &cfg,
    );

    assert_eq!(projected.status, DistrictStatus::Resolved);
    assert_eq!(
        projected
            .districts
            .iter()
            .map(|district| district.members.clone())
            .collect::<Vec<_>>(),
        vec![
            refs(&["locus:left-a", "locus:left-b"]),
            refs(&["locus:right-a", "locus:right-b"]),
        ]
    );
}

#[test]
fn directed_reachability_builds_bounded_acyclic_containment() {
    // Catches parent selection by generation order, skipped intermediate
    // parents, cycles, and failure to enforce the configured depth bound.
    let view = RelationView::new(vec![
        assertion(
            RelationKind::Access,
            "locus:a",
            "locus:b",
            RelationDirection::Directed,
            1.0,
        ),
        assertion(
            RelationKind::Access,
            "locus:b",
            "locus:c",
            RelationDirection::Directed,
            1.0,
        ),
        assertion(
            RelationKind::Access,
            "locus:c",
            "locus:d",
            RelationDirection::Directed,
            1.0,
        ),
    ])
    .unwrap();
    let interval = district_interval(0, 10);
    let projected = project_districts(
        &view,
        DistrictBasis::Access,
        interval,
        &config(RelationDirectionPolicy::SourceReachable(reference(
            "locus:a",
        ))),
    );

    assert_eq!(projected.status, DistrictStatus::Resolved);
    assert_eq!(projected.districts.len(), 3);
    let a = district_id(DistrictBasis::Access, interval, "locus:a");
    let b = district_id(DistrictBasis::Access, interval, "locus:b");
    let c = district_id(DistrictBasis::Access, interval, "locus:c");
    assert_eq!(projected.districts[0].id, a);
    assert_eq!(projected.districts[0].parent, None);
    assert_eq!(projected.districts[1].id, b);
    assert_eq!(projected.districts[1].parent, Some(a));
    assert_eq!(projected.districts[2].id, c);
    assert_eq!(projected.districts[2].parent, Some(b));

    let mut too_shallow = config(RelationDirectionPolicy::SourceReachable(reference(
        "locus:a",
    )));
    too_shallow.maximum_containment_depth = 1;
    let contradicted = project_districts(&view, DistrictBasis::Access, interval, &too_shallow);
    assert_eq!(contradicted.status, DistrictStatus::ContradictoryEvidence);
}

#[test]
fn directed_reachability_records_partial_overlap_with_a_deterministic_bound() {
    // Catches treating every intersection as hierarchy and unbounded overlap
    // fan-out. The sibling reaches intersect at d but neither contains the other.
    let view = RelationView::new(vec![
        assertion(
            RelationKind::Access,
            "locus:a",
            "locus:b",
            RelationDirection::Directed,
            1.0,
        ),
        assertion(
            RelationKind::Access,
            "locus:a",
            "locus:c",
            RelationDirection::Directed,
            1.0,
        ),
        assertion(
            RelationKind::Access,
            "locus:b",
            "locus:d",
            RelationDirection::Directed,
            1.0,
        ),
        assertion(
            RelationKind::Access,
            "locus:c",
            "locus:d",
            RelationDirection::Directed,
            1.0,
        ),
    ])
    .unwrap();
    let interval = district_interval(0, 10);
    let projected = project_districts(
        &view,
        DistrictBasis::Access,
        interval,
        &config(RelationDirectionPolicy::SourceReachable(reference(
            "locus:a",
        ))),
    );
    let b = district_id(DistrictBasis::Access, interval, "locus:b");
    let c = district_id(DistrictBasis::Access, interval, "locus:c");

    assert_eq!(projected.overlaps.len(), 1);
    let overlap = projected.overlaps.iter().next().unwrap();
    assert_eq!((&overlap.left, &overlap.right), (&b, &c));
    assert_eq!(overlap.members, refs(&["locus:d"]));
    assert_eq!(projected.districts[1].overlaps, BTreeSet::from([c]));
    assert_eq!(projected.districts[2].overlaps, BTreeSet::from([b]));
    assert!(
        projected
            .districts
            .iter()
            .all(|district| { district.overlaps.len() <= 4 })
    );
}

#[test]
fn overlap_output_stops_at_the_configured_bound_in_identity_order() {
    // Catches a projector that records an unbounded overlap fan-out or keeps
    // an insertion-order-dependent pair when the diagnostic bound is reached.
    let view = RelationView::new(vec![
        assertion(
            RelationKind::Access,
            "locus:a",
            "locus:b",
            RelationDirection::Directed,
            1.0,
        ),
        assertion(
            RelationKind::Access,
            "locus:a",
            "locus:c",
            RelationDirection::Directed,
            1.0,
        ),
        assertion(
            RelationKind::Access,
            "locus:a",
            "locus:e",
            RelationDirection::Directed,
            1.0,
        ),
        assertion(
            RelationKind::Access,
            "locus:b",
            "locus:d",
            RelationDirection::Directed,
            1.0,
        ),
        assertion(
            RelationKind::Access,
            "locus:c",
            "locus:d",
            RelationDirection::Directed,
            1.0,
        ),
        assertion(
            RelationKind::Access,
            "locus:e",
            "locus:d",
            RelationDirection::Directed,
            1.0,
        ),
    ])
    .unwrap();
    let interval = district_interval(0, 10);
    let mut cfg = config(RelationDirectionPolicy::SourceReachable(reference(
        "locus:a",
    )));
    cfg.maximum_overlaps_per_district = 1;

    let projected = project_districts(&view, DistrictBasis::Access, interval, &cfg);
    let b = district_id(DistrictBasis::Access, interval, "locus:b");
    let c = district_id(DistrictBasis::Access, interval, "locus:c");

    assert_eq!(projected.status, DistrictStatus::ContradictoryEvidence);
    assert_eq!(projected.overlaps.len(), 1);
    let overlap = projected.overlaps.iter().next().unwrap();
    assert_eq!((&overlap.left, &overlap.right), (&b, &c));
    assert!(
        projected
            .districts
            .iter()
            .all(|district| district.overlaps.len() <= 1)
    );
}

#[test]
fn empty_and_short_lived_evidence_keep_distinct_failure_statuses() {
    // Catches collapsing disconnected and transient-only into one empty or
    // generic failure value.
    let empty = RelationView::new(Vec::new()).unwrap();
    let interval = district_interval(0, 10);
    let disconnected = project_districts(
        &empty,
        DistrictBasis::Spatial,
        interval,
        &config(RelationDirectionPolicy::Symmetric),
    );
    assert_eq!(disconnected.status, DistrictStatus::Disconnected);

    let flicker = RelationView::new(vec![assertion_during(
        RelationKind::SpatialAdjacency,
        "locus:a",
        "locus:b",
        RelationDirection::Symmetric,
        1.0,
        4,
        5,
    )])
    .unwrap();
    let mut cfg = config(RelationDirectionPolicy::Symmetric);
    cfg.minimum_duration_ticks = 5;
    let transient = project_districts(&flicker, DistrictBasis::Spatial, interval, &cfg);
    assert_eq!(transient.status, DistrictStatus::TransientOnly);
    assert_eq!(transient.districts.len(), 1);
    assert_eq!(transient.districts[0].status, DistrictStatus::TransientOnly);
}

#[test]
fn projection_is_deterministic_across_repeated_complete_outputs() {
    // Catches unordered traversal, unstable candidate deduplication, and
    // identity derived from insertion or generation order.
    let assertions = vec![
        assertion(
            RelationKind::Access,
            "locus:a",
            "locus:c",
            RelationDirection::Directed,
            1.0,
        ),
        assertion(
            RelationKind::Access,
            "locus:a",
            "locus:b",
            RelationDirection::Directed,
            1.0,
        ),
        assertion(
            RelationKind::Access,
            "locus:c",
            "locus:d",
            RelationDirection::Directed,
            1.0,
        ),
        assertion(
            RelationKind::Access,
            "locus:b",
            "locus:d",
            RelationDirection::Directed,
            1.0,
        ),
    ];
    let mut reversed = assertions.clone();
    reversed.reverse();
    let interval = district_interval(0, 10);
    let cfg = config(RelationDirectionPolicy::SourceReachable(reference(
        "locus:a",
    )));

    let first = project_districts(
        &RelationView::new(assertions).unwrap(),
        DistrictBasis::Access,
        interval,
        &cfg,
    );
    let second = project_districts(
        &RelationView::new(reversed).unwrap(),
        DistrictBasis::Access,
        interval,
        &cfg,
    );
    let third = project_districts(
        &RelationView::new(first.districts[0].evidence.clone()).unwrap(),
        DistrictBasis::Access,
        interval,
        &cfg,
    );

    assert_eq!(first, second);
    assert_eq!(first, third);
}

#[test]
fn projection_is_pure_over_stream_time_source_assertions_and_world_facts() {
    // Catches any hidden stream draw, wall-clock-derived id, source mutation,
    // or ledger write introduced behind the pure projection signature.
    let view = RelationView::new(vec![assertion(
        RelationKind::SpatialAdjacency,
        "aggregate:a",
        "aggregate:b",
        RelationDirection::Symmetric,
        1.0,
    )])
    .unwrap();
    let source_before = view.clone();
    let world = World::new(Seed(91));
    let world_before = world.to_json();
    let mut observed_stream = Seed(91).stream();
    let mut control_stream = Seed(91).stream();
    assert_eq!(observed_stream.next_u64(), control_stream.next_u64());
    let cfg = config(RelationDirectionPolicy::Symmetric);
    let interval = district_interval(0, 10);

    let first = project_districts(&view, DistrictBasis::Spatial, interval, &cfg);
    thread::sleep(Duration::from_millis(2));
    let second = project_districts(&view, DistrictBasis::Spatial, interval, &cfg);

    assert_eq!(first, second);
    assert_eq!(observed_stream.next_u64(), control_stream.next_u64());
    assert_eq!(view, source_before);
    assert_eq!(world.to_json(), world_before);
    assert!(
        first.districts[0]
            .members
            .iter()
            .all(|member| member.as_str().starts_with("aggregate:"))
    );
}

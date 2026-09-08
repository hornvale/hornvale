use hornvale_kernel::{Seed, World, WorldTime};
use hornvale_worldgen::district::{
    DistrictBasis, DistrictConfig, DistrictContinuityState, DistrictId, DistrictInterval,
    DistrictStatus, compare_districts, project_districts,
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
    assertion_with_recurrence(
        kind,
        from,
        to,
        direction,
        measure,
        start,
        end,
        RelationRecurrence::Once,
    )
}

#[allow(clippy::too_many_arguments)]
fn assertion_with_recurrence(
    kind: RelationKind,
    from: &str,
    to: &str,
    direction: RelationDirection,
    measure: f64,
    start: i64,
    end: i64,
    recurrence: RelationRecurrence,
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
        recurrence,
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

#[test]
fn uninterrupted_evidence_establishes_event_continuity() {
    // Catches comparing projection-local ids literally (their intervals
    // differ) or treating every repeated observation as recurrence.
    let view = RelationView::new(vec![assertion_during(
        RelationKind::SpatialAdjacency,
        "locus:a",
        "locus:b",
        RelationDirection::Symmetric,
        1.0,
        0,
        9,
    )])
    .unwrap();
    let previous_interval = district_interval(0, 4);
    let current_interval = district_interval(5, 9);
    let cfg = config(RelationDirectionPolicy::Symmetric);
    let previous = project_districts(&view, DistrictBasis::Spatial, previous_interval, &cfg);
    let current = project_districts(&view, DistrictBasis::Spatial, current_interval, &cfg);

    let continuity = compare_districts(&previous, &current, &cfg);

    assert_eq!(continuity.previous_interval, previous_interval);
    assert_eq!(continuity.current_interval, current_interval);
    assert_eq!(
        continuity.states,
        vec![DistrictContinuityState::EventContinuity {
            previous: district_id(DistrictBasis::Spatial, previous_interval, "locus:a"),
            current: district_id(DistrictBasis::Spatial, current_interval, "locus:a"),
        }]
    );
}

#[test]
fn the_ring_recurs_on_matching_seasonal_windows() {
    // Catches interval selection that ignores periodic occurrences and a
    // comparator that mistakes a periodic gap for uninterrupted continuity.
    let view = RelationView::new(vec![assertion_with_recurrence(
        RelationKind::Access,
        "cohort:ring",
        "locus:summer-ground",
        RelationDirection::Reciprocal,
        1.0,
        0,
        2,
        RelationRecurrence::Periodic { period_ticks: 10 },
    )])
    .unwrap();
    let previous_interval = district_interval(0, 2);
    let current_interval = district_interval(10, 12);
    let cfg = config(RelationDirectionPolicy::Reciprocal);
    let previous = project_districts(&view, DistrictBasis::Access, previous_interval, &cfg);
    let current = project_districts(&view, DistrictBasis::Access, current_interval, &cfg);

    assert_eq!(previous.status, DistrictStatus::Resolved);
    assert_eq!(current.status, DistrictStatus::Resolved);
    assert_eq!(
        compare_districts(&previous, &current, &cfg).states,
        vec![DistrictContinuityState::Recurrence {
            previous: district_id(DistrictBasis::Access, previous_interval, "cohort:ring"),
            current: district_id(DistrictBasis::Access, current_interval, "cohort:ring"),
            period_ticks: 10,
        }]
    );
}

#[test]
fn the_drift_recomposes_one_district_into_ordered_successors() {
    // Catches one-to-one-only matching, member-overlap identity inference,
    // and successor order inherited from the input vector.
    let previous_view = RelationView::new(vec![
        assertion_during(
            RelationKind::SpatialAdjacency,
            "locus:a",
            "locus:b",
            RelationDirection::Symmetric,
            1.0,
            0,
            9,
        ),
        assertion_during(
            RelationKind::SpatialAdjacency,
            "locus:b",
            "locus:c",
            RelationDirection::Symmetric,
            1.0,
            0,
            9,
        ),
        assertion_during(
            RelationKind::SpatialAdjacency,
            "locus:c",
            "locus:d",
            RelationDirection::Symmetric,
            1.0,
            0,
            9,
        ),
    ])
    .unwrap();
    let current_view = RelationView::new(vec![
        assertion_during(
            RelationKind::SpatialAdjacency,
            "locus:c",
            "locus:d",
            RelationDirection::Symmetric,
            1.0,
            10,
            19,
        ),
        assertion_during(
            RelationKind::SpatialAdjacency,
            "locus:a",
            "locus:b",
            RelationDirection::Symmetric,
            1.0,
            10,
            19,
        ),
    ])
    .unwrap();
    let previous_interval = district_interval(0, 9);
    let current_interval = district_interval(10, 19);
    let cfg = config(RelationDirectionPolicy::Symmetric);
    let previous = project_districts(
        &previous_view,
        DistrictBasis::Spatial,
        previous_interval,
        &cfg,
    );
    let current = project_districts(
        &current_view,
        DistrictBasis::Spatial,
        current_interval,
        &cfg,
    );
    let mut reversed_current = current.clone();
    reversed_current.districts.reverse();
    let expected = vec![DistrictContinuityState::Recomposed {
        previous: vec![district_id(
            DistrictBasis::Spatial,
            previous_interval,
            "locus:a",
        )],
        current: vec![
            district_id(DistrictBasis::Spatial, current_interval, "locus:a"),
            district_id(DistrictBasis::Spatial, current_interval, "locus:c"),
        ],
    }];

    assert_eq!(
        compare_districts(&previous, &current, &cfg).states,
        expected
    );
    assert_eq!(
        compare_districts(&previous, &current, &cfg),
        compare_districts(&previous, &reversed_current, &cfg)
    );
}

#[test]
fn unmatched_previous_district_dissolves_at_the_current_interval() {
    // Catches silently dropping a district when no successor has explicit
    // continuity evidence.
    let previous_interval = district_interval(0, 9);
    let current_interval = district_interval(10, 19);
    let cfg = config(RelationDirectionPolicy::Symmetric);
    let previous = project_districts(
        &RelationView::new(vec![assertion_during(
            RelationKind::SpatialAdjacency,
            "locus:a",
            "locus:b",
            RelationDirection::Symmetric,
            1.0,
            0,
            9,
        )])
        .unwrap(),
        DistrictBasis::Spatial,
        previous_interval,
        &cfg,
    );
    let current = project_districts(
        &RelationView::new(Vec::new()).unwrap(),
        DistrictBasis::Spatial,
        current_interval,
        &cfg,
    );

    assert_eq!(
        compare_districts(&previous, &current, &cfg).states,
        vec![DistrictContinuityState::Dissolved {
            previous: district_id(DistrictBasis::Spatial, previous_interval, "locus:a"),
            at: WorldTime::from_ticks(10),
        }]
    );
}

#[test]
fn transient_candidates_remain_distinct_from_refused_candidates() {
    // Catches treating a supported short-lived district and a candidate that
    // failed configured evidence support as the same temporal outcome.
    let previous_interval = district_interval(-1, -1);
    let current_interval = district_interval(0, 2);
    let empty = RelationView::new(Vec::new()).unwrap();
    let view = RelationView::new(vec![assertion_during(
        RelationKind::SpatialAdjacency,
        "locus:a",
        "locus:b",
        RelationDirection::Symmetric,
        1.0,
        1,
        1,
    )])
    .unwrap();
    let mut transient_cfg = config(RelationDirectionPolicy::Symmetric);
    transient_cfg.minimum_duration_ticks = 2;
    let previous = project_districts(
        &empty,
        DistrictBasis::Spatial,
        previous_interval,
        &transient_cfg,
    );
    let transient = project_districts(
        &view,
        DistrictBasis::Spatial,
        current_interval,
        &transient_cfg,
    );

    assert_eq!(
        compare_districts(&previous, &transient, &transient_cfg).states,
        vec![DistrictContinuityState::Transient {
            district: district_id(DistrictBasis::Spatial, current_interval, "locus:a"),
        }]
    );

    let mut refusing_cfg = transient_cfg;
    refusing_cfg.minimum_evidence = 2;
    let refused = project_districts(
        &view,
        DistrictBasis::Spatial,
        current_interval,
        &refusing_cfg,
    );
    assert_eq!(refused.status, DistrictStatus::InsufficientEvidence);
    assert!(
        compare_districts(&previous, &refused, &refusing_cfg)
            .states
            .is_empty()
    );
}

#[test]
fn overlapping_members_and_anchor_do_not_establish_identity() {
    // Catches fuzzy matching by Jaccard overlap, member intersection, or a
    // shared minimum-member anchor without shared producer evidence.
    let previous_interval = district_interval(0, 9);
    let current_interval = district_interval(10, 19);
    let cfg = config(RelationDirectionPolicy::Symmetric);
    let previous = project_districts(
        &RelationView::new(vec![
            assertion_during(
                RelationKind::SpatialAdjacency,
                "locus:a",
                "locus:b",
                RelationDirection::Symmetric,
                1.0,
                0,
                9,
            ),
            assertion_during(
                RelationKind::SpatialAdjacency,
                "locus:b",
                "locus:c",
                RelationDirection::Symmetric,
                1.0,
                0,
                9,
            ),
        ])
        .unwrap(),
        DistrictBasis::Spatial,
        previous_interval,
        &cfg,
    );
    let current = project_districts(
        &RelationView::new(vec![
            assertion_during(
                RelationKind::SpatialAdjacency,
                "locus:a",
                "locus:d",
                RelationDirection::Symmetric,
                1.0,
                10,
                19,
            ),
            assertion_during(
                RelationKind::SpatialAdjacency,
                "locus:d",
                "locus:b",
                RelationDirection::Symmetric,
                1.0,
                10,
                19,
            ),
        ])
        .unwrap(),
        DistrictBasis::Spatial,
        current_interval,
        &cfg,
    );

    assert_eq!(
        intersection(
            &previous.districts[0].members,
            &current.districts[0].members
        ),
        refs(&["locus:a", "locus:b"])
    );
    assert_eq!(
        previous.districts[0].id.anchor,
        current.districts[0].id.anchor
    );
    assert_eq!(
        compare_districts(&previous, &current, &cfg).states,
        vec![DistrictContinuityState::Dissolved {
            previous: district_id(DistrictBasis::Spatial, previous_interval, "locus:a"),
            at: WorldTime::from_ticks(10),
        }]
    );
}

fn intersection(
    left: &BTreeSet<RelationReference>,
    right: &BTreeSet<RelationReference>,
) -> BTreeSet<RelationReference> {
    left.intersection(right).cloned().collect()
}

use hornvale_kernel::WorldTime;
use hornvale_worldgen::relation::{
    RelationAssertion, RelationBasis, RelationDirection, RelationDirectionPolicy, RelationError,
    RelationInterval, RelationKind, RelationMeasure, RelationMeasureFilter, RelationParticipant,
    RelationProvenance, RelationRecurrence, RelationReference, RelationRefusalReason, RelationRole,
    RelationView,
};

fn reference(name: &str) -> RelationReference {
    RelationReference::new(name)
}

fn role(name: &str) -> RelationRole {
    RelationRole::new(name)
}

fn interval(start: i64, end: i64) -> RelationInterval {
    RelationInterval {
        start: WorldTime::from_ticks(start),
        end: WorldTime::from_ticks(end),
    }
}

fn participant(name: &str, role_name: &str) -> RelationParticipant {
    RelationParticipant {
        reference: reference(name),
        role: role(role_name),
    }
}

fn assertion(
    kind: RelationKind,
    participants: Vec<RelationParticipant>,
    direction: RelationDirection,
    measure: f64,
) -> RelationAssertion {
    RelationAssertion {
        kind,
        participants,
        interval: interval(0, 10),
        recurrence: RelationRecurrence::Once,
        direction,
        measure: RelationMeasure::new(measure),
        provenance: RelationProvenance::new("fixture"),
    }
}

fn references(assertions: impl Iterator<Item = RelationAssertion>) -> Vec<(String, String)> {
    assertions
        .map(|assertion| {
            (
                assertion.participants[0].reference.as_str().to_owned(),
                assertion.participants[1].reference.as_str().to_owned(),
            )
        })
        .collect()
}

#[test]
fn valid_binary_and_aggregate_presence_assertions_validate() {
    for (kind, direction) in [
        (RelationKind::SpatialAdjacency, RelationDirection::Symmetric),
        (RelationKind::Access, RelationDirection::Directed),
        (RelationKind::Exchange, RelationDirection::Reciprocal),
        (RelationKind::Presence, RelationDirection::Symmetric),
    ] {
        let assertion = assertion(
            kind,
            vec![
                participant("cohort:river-workers", "aggregate"),
                participant("locus:ford", "site"),
            ],
            direction,
            1.0,
        );
        assert_eq!(assertion.validate(), Ok(()));
        assert_eq!(RelationView::new(vec![assertion]).unwrap().len(), 1);
    }
}

#[test]
fn symmetric_access_is_valid_and_undirected() {
    let assertion = assertion(
        RelationKind::Access,
        vec![
            participant("locus:a", "site"),
            participant("locus:b", "site"),
        ],
        RelationDirection::Symmetric,
        1.0,
    );
    assert_eq!(assertion.validate(), Ok(()));
}

#[test]
fn higher_arity_assertions_are_preserved_but_not_lowered_by_view() {
    let assertion = assertion(
        RelationKind::Exchange,
        vec![
            participant("cohort:a", "source"),
            participant("locus:market", "venue"),
            participant("cohort:b", "destination"),
        ],
        RelationDirection::Directed,
        3.0,
    );

    assert_eq!(assertion.validate(), Ok(()));
    assert_eq!(assertion.participants.len(), 3);
    assert!(matches!(
        RelationView::new(vec![assertion]),
        Err(RelationError::UnsupportedParticipantCount { count: 3, .. })
    ));
}

#[test]
fn malformed_interval_and_non_finite_measure_are_rejected() {
    let mut reversed = assertion(
        RelationKind::SpatialAdjacency,
        vec![
            participant("locus:a", "site"),
            participant("locus:b", "site"),
        ],
        RelationDirection::Symmetric,
        1.0,
    );
    reversed.interval = interval(10, 0);
    assert!(matches!(
        reversed.validate(),
        Err(RelationError::ReversedInterval { .. })
    ));

    let mut non_finite = assertion(
        RelationKind::SpatialAdjacency,
        vec![
            participant("locus:a", "site"),
            participant("locus:b", "site"),
        ],
        RelationDirection::Symmetric,
        f64::NAN,
    );
    non_finite.measure = RelationMeasure::new(f64::NAN);
    assert_eq!(non_finite.validate(), Err(RelationError::NonFiniteMeasure));
}

#[test]
fn unsupported_direction_combinations_are_rejected() {
    let assertion = assertion(
        RelationKind::SpatialAdjacency,
        vec![
            participant("locus:a", "site"),
            participant("locus:b", "site"),
        ],
        RelationDirection::Directed,
        1.0,
    );
    assert!(matches!(
        assertion.validate(),
        Err(RelationError::UnsupportedDirection {
            kind: RelationKind::SpatialAdjacency,
            ..
        })
    ));
}

#[test]
fn relation_view_orders_assertions_deterministically() {
    let later = assertion(
        RelationKind::Access,
        vec![participant("locus:z", "from"), participant("locus:a", "to")],
        RelationDirection::Directed,
        2.0,
    );
    let earlier = assertion(
        RelationKind::Access,
        vec![participant("locus:a", "from"), participant("locus:z", "to")],
        RelationDirection::Directed,
        1.0,
    );
    let view = RelationView::new(vec![later, earlier]).unwrap();
    let names: Vec<_> = view
        .iter()
        .map(|assertion| assertion.participants[0].reference.as_str())
        .collect();
    assert_eq!(names, vec!["locus:a", "locus:z"]);
}

#[test]
fn relation_view_order_includes_recurrence() {
    let mut periodic = assertion(
        RelationKind::Access,
        vec![participant("locus:a", "from"), participant("locus:z", "to")],
        RelationDirection::Directed,
        1.0,
    );
    periodic.recurrence = RelationRecurrence::Periodic { period_ticks: 20 };
    let once = assertion(
        RelationKind::Access,
        vec![participant("locus:a", "from"), participant("locus:z", "to")],
        RelationDirection::Directed,
        1.0,
    );

    let view = RelationView::new(vec![periodic, once]).unwrap();
    let recurrences: Vec<_> = view.iter().map(|a| a.recurrence).collect();
    assert_eq!(
        recurrences,
        vec![
            RelationRecurrence::Once,
            RelationRecurrence::Periodic { period_ticks: 20 }
        ]
    );
}

#[test]
fn periodic_recurrence_requires_a_positive_period() {
    let mut assertion = assertion(
        RelationKind::Access,
        vec![participant("locus:a", "from"), participant("locus:z", "to")],
        RelationDirection::Directed,
        1.0,
    );
    assertion.recurrence = RelationRecurrence::Periodic { period_ticks: 0 };
    assert_eq!(assertion.validate(), Err(RelationError::InvalidRecurrence));
}

#[test]
fn symmetric_view_canonicalizes_reversed_participants() {
    let forward = assertion(
        RelationKind::SpatialAdjacency,
        vec![
            participant("locus:a", "site"),
            participant("locus:z", "site"),
        ],
        RelationDirection::Symmetric,
        1.0,
    );
    let reverse = assertion(
        RelationKind::SpatialAdjacency,
        vec![
            participant("locus:z", "site"),
            participant("locus:a", "site"),
        ],
        RelationDirection::Symmetric,
        1.0,
    );

    let forward_view = RelationView::new(vec![forward]).unwrap();
    let reverse_view = RelationView::new(vec![reverse]).unwrap();
    assert_eq!(forward_view, reverse_view);
}

#[test]
fn basis_views_default_spatial_to_symmetric_and_preserve_directed_bases() {
    let spatial = assertion(
        RelationKind::SpatialAdjacency,
        vec![
            participant("locus:z", "site"),
            participant("locus:a", "site"),
        ],
        RelationDirection::Symmetric,
        1.0,
    );
    let access = assertion(
        RelationKind::Access,
        vec![participant("locus:z", "from"), participant("locus:a", "to")],
        RelationDirection::Directed,
        1.0,
    );
    let exchange = assertion(
        RelationKind::Exchange,
        vec![
            participant("cohort:z", "source"),
            participant("cohort:a", "destination"),
        ],
        RelationDirection::Directed,
        1.0,
    );
    let view = RelationView::new(vec![exchange, access, spatial]).unwrap();

    assert_eq!(
        references(view.spatial(RelationMeasureFilter::all()).iter().cloned()),
        vec![("locus:a".to_owned(), "locus:z".to_owned())]
    );
    assert_eq!(
        references(
            view.access(
                RelationDirectionPolicy::WeaklyConnected,
                RelationMeasureFilter::all(),
            )
            .iter()
            .cloned()
        ),
        vec![("locus:z".to_owned(), "locus:a".to_owned())]
    );
    assert_eq!(
        references(
            view.exchange(
                RelationDirectionPolicy::WeaklyConnected,
                RelationMeasureFilter::all(),
            )
            .iter()
            .cloned()
        ),
        vec![("cohort:z".to_owned(), "cohort:a".to_owned())]
    );
}

#[test]
fn weak_and_strong_policies_select_different_directed_evidence() {
    let one_way = assertion(
        RelationKind::Access,
        vec![participant("locus:a", "from"), participant("locus:b", "to")],
        RelationDirection::Directed,
        1.0,
    );
    let outward = assertion(
        RelationKind::Access,
        vec![participant("locus:b", "from"), participant("locus:c", "to")],
        RelationDirection::Directed,
        1.0,
    );
    let return_path = assertion(
        RelationKind::Access,
        vec![participant("locus:c", "from"), participant("locus:b", "to")],
        RelationDirection::Directed,
        1.0,
    );
    let view = RelationView::new(vec![return_path, one_way, outward]).unwrap();

    let weak = view.access(
        RelationDirectionPolicy::WeaklyConnected,
        RelationMeasureFilter::all(),
    );
    let strong = view.access(
        RelationDirectionPolicy::StronglyConnected,
        RelationMeasureFilter::all(),
    );

    assert_eq!(weak.len(), 3);
    assert_eq!(
        references(strong.iter().cloned()),
        vec![
            ("locus:b".to_owned(), "locus:c".to_owned()),
            ("locus:c".to_owned(), "locus:b".to_owned()),
        ]
    );
}

#[test]
fn strong_policy_admits_a_directed_cycle_without_direct_reverse_edges() {
    let a_to_b = assertion(
        RelationKind::Access,
        vec![participant("locus:a", "from"), participant("locus:b", "to")],
        RelationDirection::Directed,
        1.0,
    );
    let b_to_c = assertion(
        RelationKind::Access,
        vec![participant("locus:b", "from"), participant("locus:c", "to")],
        RelationDirection::Directed,
        1.0,
    );
    let c_to_a = assertion(
        RelationKind::Access,
        vec![participant("locus:c", "from"), participant("locus:a", "to")],
        RelationDirection::Directed,
        1.0,
    );
    let view = RelationView::new(vec![c_to_a, b_to_c, a_to_b]).unwrap();

    let strong = view.access(
        RelationDirectionPolicy::StronglyConnected,
        RelationMeasureFilter::all(),
    );

    assert_eq!(strong.len(), 3);
}

#[test]
fn source_reachable_and_reciprocal_policies_are_explicit() {
    let from_a = assertion(
        RelationKind::Access,
        vec![participant("locus:a", "from"), participant("locus:b", "to")],
        RelationDirection::Directed,
        1.0,
    );
    let from_b = assertion(
        RelationKind::Access,
        vec![participant("locus:b", "from"), participant("locus:c", "to")],
        RelationDirection::Directed,
        1.0,
    );
    let toward_a = assertion(
        RelationKind::Access,
        vec![participant("locus:d", "from"), participant("locus:a", "to")],
        RelationDirection::Directed,
        1.0,
    );
    let reciprocal = assertion(
        RelationKind::Access,
        vec![
            participant("locus:c", "site"),
            participant("locus:e", "site"),
        ],
        RelationDirection::Reciprocal,
        1.0,
    );
    let view = RelationView::new(vec![toward_a, reciprocal, from_b, from_a]).unwrap();

    let reachable = view.access(
        RelationDirectionPolicy::SourceReachable(reference("locus:a")),
        RelationMeasureFilter::all(),
    );
    let reciprocal_only = view.access(
        RelationDirectionPolicy::Reciprocal,
        RelationMeasureFilter::all(),
    );

    assert_eq!(
        references(reachable.iter().cloned()),
        vec![
            ("locus:a".to_owned(), "locus:b".to_owned()),
            ("locus:b".to_owned(), "locus:c".to_owned()),
            ("locus:c".to_owned(), "locus:e".to_owned()),
        ]
    );
    assert_eq!(
        references(reciprocal_only.iter().cloned()),
        vec![("locus:c".to_owned(), "locus:e".to_owned())]
    );
}

#[test]
fn distant_exchange_does_not_fabricate_spatial_adjacency() {
    let exchange = assertion(
        RelationKind::Exchange,
        vec![
            participant("locus:distant-a", "source"),
            participant("locus:distant-b", "destination"),
        ],
        RelationDirection::Directed,
        12.0,
    );
    let view = RelationView::new(vec![exchange.clone()]).unwrap();

    let exchange_view = view.exchange(
        RelationDirectionPolicy::WeaklyConnected,
        RelationMeasureFilter::all(),
    );
    let spatial_view = view.spatial(RelationMeasureFilter::all());

    assert_eq!(exchange_view.iter().collect::<Vec<_>>(), vec![&exchange]);
    assert!(spatial_view.is_empty());
    assert_eq!(spatial_view.refusals().len(), 1);
    assert_eq!(spatial_view.refusals()[0].assertion, exchange);
    assert_eq!(
        spatial_view.refusals()[0].reason,
        RelationRefusalReason::UnsupportedKind {
            basis: RelationBasis::Spatial,
            kind: RelationKind::Exchange,
        }
    );
}

#[test]
fn higher_arity_refusal_retains_the_original_assertion_and_provenance() {
    let ternary = assertion(
        RelationKind::Exchange,
        vec![
            participant("cohort:a", "source"),
            participant("locus:market", "venue"),
            participant("cohort:b", "destination"),
        ],
        RelationDirection::Directed,
        3.0,
    );
    let expected = ternary.clone();

    let error = RelationView::new(vec![ternary]).unwrap_err();

    assert_eq!(
        error,
        RelationError::UnsupportedParticipantCount {
            kind: RelationKind::Exchange,
            count: 3,
            assertion: expected,
        }
    );
}

#[test]
fn measure_filter_is_applied_only_inside_its_basis() {
    let nearby = assertion(
        RelationKind::SpatialAdjacency,
        vec![
            participant("locus:a", "site"),
            participant("locus:b", "site"),
        ],
        RelationDirection::Symmetric,
        2.0,
    );
    let frequent_exchange = assertion(
        RelationKind::Exchange,
        vec![participant("locus:a", "from"), participant("locus:b", "to")],
        RelationDirection::Directed,
        9.0,
    );
    let view = RelationView::new(vec![frequent_exchange, nearby]).unwrap();

    assert!(
        view.spatial(RelationMeasureFilter::at_least(5.0))
            .is_empty()
    );
    assert_eq!(
        view.exchange(
            RelationDirectionPolicy::WeaklyConnected,
            RelationMeasureFilter::at_least(5.0),
        )
        .len(),
        1
    );
}

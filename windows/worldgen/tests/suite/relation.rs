use hornvale_kernel::WorldTime;
use hornvale_worldgen::relation::{
    RelationAssertion, RelationDirection, RelationError, RelationInterval, RelationKind,
    RelationMeasure, RelationParticipant, RelationProvenance, RelationRecurrence,
    RelationReference, RelationRole, RelationView,
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

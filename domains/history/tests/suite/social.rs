//! Append-only realized social-event contract tests.

use hornvale_history::{
    AssociationForm, GroupMembershipEvent, LifecycleEvent, RelationEvent, RelationKind,
    SocialEvent, validate_social_events,
};
use hornvale_kernel::{EntityId, Value, WorldTime};

fn entity(raw: u64) -> EntityId {
    EntityId::new(raw).expect("test ids are nonzero")
}

fn day(raw: i64) -> WorldTime {
    WorldTime::from_ticks(raw * WorldTime::TICKS_PER_STD_DAY)
}

/// Reversing any directional fact or dropping its interval makes one row of
/// this hand-derived relation table fail.
#[test]
fn foundational_relations_preserve_direction_and_intervals() {
    let source = entity(1);
    let target = entity(2);
    let form = AssociationForm::new("mutual-aid").unwrap();
    let cases = [
        (RelationKind::Origin, None, None, "origin"),
        (RelationKind::Descent, None, None, "descent"),
        (RelationKind::Care, None, None, "care"),
        (RelationKind::Dependency, None, None, "dependency"),
        (
            RelationKind::Association,
            Some(form.clone()),
            None,
            "association",
        ),
        (RelationKind::Residence, None, None, "residence"),
        (RelationKind::Custody, None, None, "custody"),
        (RelationKind::Transfer, None, None, "transfer"),
        (
            RelationKind::Recognition,
            None,
            Some("recognized-by-council"),
            "recognition",
        ),
    ];

    for (kind, association_form, interpretation, predicate) in cases {
        let relation = RelationEvent::new(
            kind,
            source,
            target,
            day(3),
            Some(day(9)),
            association_form,
            interpretation.map(str::to_string),
            "synthetic-probe",
        )
        .unwrap();
        let event = SocialEvent::Relation(relation.clone());
        let fact = event.fact();

        assert_eq!(relation.source(), source, "{kind:?} source");
        assert_eq!(relation.target(), target, "{kind:?} target");
        assert_eq!(relation.start(), day(3), "{kind:?} start");
        assert_eq!(relation.end(), Some(day(9)), "{kind:?} end");
        assert_eq!(fact.subject, source, "{kind:?} fact subject");
        assert_eq!(fact.object, Value::Entity(target), "{kind:?} fact object");
        assert_eq!(fact.predicate, predicate, "{kind:?} predicate");
        assert_eq!(fact.day, Some(day(3)), "{kind:?} fact day");
        assert_eq!(fact.provenance, "synthetic-probe");
    }
}

/// Treating association as marriage-only or folding recognition into the
/// relation makes these explicit values disappear.
#[test]
fn association_form_and_recognition_interpretation_are_explicit_values() {
    let association = RelationEvent::new(
        RelationKind::Association,
        entity(1),
        entity(2),
        day(1),
        None,
        Some(AssociationForm::new("seasonal-co-residence").unwrap()),
        None,
        "observed",
    )
    .unwrap();
    let recognition = RelationEvent::new(
        RelationKind::Recognition,
        entity(3),
        entity(1),
        day(2),
        None,
        None,
        Some("ritual-guardian".to_string()),
        "institutional-record",
    )
    .unwrap();

    assert_eq!(
        association.association_form().map(AssociationForm::as_str),
        Some("seasonal-co-residence")
    );
    assert_eq!(association.interpretation(), None);
    assert_eq!(recognition.association_form(), None);
    assert_eq!(recognition.interpretation(), Some("ritual-guardian"));
}

/// Relaxing any local event invariant makes one malformed value construct.
#[test]
fn event_validation_rejects_empty_intervals_missing_provenance_and_self_relations() {
    assert_eq!(
        RelationEvent::new(
            RelationKind::Care,
            entity(1),
            entity(2),
            day(4),
            Some(day(4)),
            None,
            None,
            "observed",
        )
        .unwrap_err()
        .to_string(),
        "relation interval end must be after its start"
    );
    assert_eq!(
        RelationEvent::new(
            RelationKind::Descent,
            entity(1),
            entity(1),
            day(1),
            None,
            None,
            None,
            "observed",
        )
        .unwrap_err()
        .to_string(),
        "descent requires distinct source and target participants"
    );
    assert_eq!(
        GroupMembershipEvent::new(entity(1), entity(9), day(1), None, "  ")
            .unwrap_err()
            .to_string(),
        "group membership provenance must not be empty"
    );
}

/// If `separate` accepts participants in the reverse direction, this test no
/// longer distinguishes the malformed closure from the valid one.
#[test]
fn separation_must_match_the_active_association_direction() {
    let association = SocialEvent::Relation(
        RelationEvent::new(
            RelationKind::Association,
            entity(1),
            entity(2),
            day(1),
            None,
            Some(AssociationForm::new("companionship").unwrap()),
            None,
            "observed",
        )
        .unwrap(),
    );
    let reversed = SocialEvent::Lifecycle(
        LifecycleEvent::separate(entity(2), entity(1), day(5), "witnessed").unwrap(),
    );

    assert_eq!(
        validate_social_events(&[association, reversed])
            .unwrap_err()
            .to_string(),
        "separate participants reverse the active association direction"
    );
}

/// Removing any closure check admits one future event; mutating the input to
/// make a closure work erases a prior fact and fails the final assertions.
#[test]
fn lifecycle_closures_block_future_activity_without_deleting_prior_facts() {
    let association = SocialEvent::Relation(
        RelationEvent::new(
            RelationKind::Association,
            entity(1),
            entity(2),
            day(1),
            None,
            Some(AssociationForm::new("shared-work").unwrap()),
            None,
            "observed",
        )
        .unwrap(),
    );
    let membership = SocialEvent::Membership(
        GroupMembershipEvent::new(entity(3), entity(9), day(1), None, "observed").unwrap(),
    );
    let prior_association_fact = association.fact();
    let prior_membership_fact = membership.fact();

    let separated = vec![
        association.clone(),
        SocialEvent::Lifecycle(
            LifecycleEvent::separate(entity(1), entity(2), day(3), "witnessed").unwrap(),
        ),
        SocialEvent::Relation(
            RelationEvent::new(
                RelationKind::Association,
                entity(1),
                entity(2),
                day(4),
                None,
                Some(AssociationForm::new("shared-work").unwrap()),
                None,
                "observed",
            )
            .unwrap(),
        ),
    ];
    assert_eq!(
        validate_social_events(&separated).unwrap_err().to_string(),
        "association activity occurs after separation"
    );
    assert_eq!(separated[0].fact(), prior_association_fact);

    let dissolved = vec![
        membership.clone(),
        SocialEvent::Lifecycle(LifecycleEvent::dissolve(entity(9), day(3), "recorded").unwrap()),
        SocialEvent::Membership(
            GroupMembershipEvent::new(entity(4), entity(9), day(4), None, "observed").unwrap(),
        ),
    ];
    assert_eq!(
        validate_social_events(&dissolved).unwrap_err().to_string(),
        "group membership activity occurs after dissolution"
    );
    assert_eq!(dissolved[0].fact(), prior_membership_fact);

    let care = SocialEvent::Relation(
        RelationEvent::new(
            RelationKind::Care,
            entity(1),
            entity(2),
            day(1),
            Some(day(2)),
            None,
            None,
            "observed",
        )
        .unwrap(),
    );
    let prior_care_fact = care.fact();
    let died = vec![
        care,
        SocialEvent::Lifecycle(LifecycleEvent::die(entity(1), day(3), "death-record").unwrap()),
        SocialEvent::Relation(
            RelationEvent::new(
                RelationKind::Transfer,
                entity(2),
                entity(1),
                day(4),
                None,
                None,
                None,
                "inheritance-record",
            )
            .unwrap(),
        ),
    ];
    assert_eq!(
        validate_social_events(&died).unwrap_err().to_string(),
        "relation activity involves a person after death"
    );
    assert_eq!(died[0].fact(), prior_care_fact);
}

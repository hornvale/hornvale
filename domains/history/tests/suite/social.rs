//! Append-only realized social-event contract tests.

use hornvale_history::{
    ASSOCIATION_FORM, AssociationForm, GroupMembershipEvent, LifecycleEvent, MEMBERSHIP_ENDED,
    RECOGNITION_INTERPRETATION, RelationEvent, RelationKind, SocialEvent, register_concepts,
    validate_social_events,
};
use hornvale_kernel::{ConceptRegistry, EntityId, Value, WorldTime};

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
        let facts = event.facts();

        assert_eq!(relation.source(), source, "{kind:?} source");
        assert_eq!(relation.target(), target, "{kind:?} target");
        assert_eq!(relation.start(), day(3), "{kind:?} start");
        assert_eq!(relation.end(), Some(day(9)), "{kind:?} end");
        assert_eq!(fact.subject, source, "{kind:?} fact subject");
        assert_eq!(fact.object, Value::Entity(target), "{kind:?} fact object");
        assert_eq!(fact.predicate, predicate, "{kind:?} predicate");
        assert_eq!(fact.day, Some(day(3)), "{kind:?} fact day");
        assert_eq!(fact.provenance, "synthetic-probe");
        assert!(
            facts.iter().any(|fact| {
                fact.subject == source
                    && fact.predicate == kind.ended_predicate()
                    && fact.object == Value::Entity(target)
                    && fact.day == Some(day(9))
                    && fact.provenance == "synthetic-probe"
            }),
            "{kind:?} emitted interval end"
        );
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

    let association_facts = SocialEvent::Relation(association).facts();
    assert_eq!(association_facts.len(), 2);
    assert_eq!(association_facts[0].object, Value::Entity(entity(2)));
    assert_eq!(association_facts[1].subject, entity(1));
    assert_eq!(association_facts[1].predicate, ASSOCIATION_FORM);
    assert_eq!(
        association_facts[1].object,
        Value::Text("seasonal-co-residence".to_string())
    );
    assert_eq!(association_facts[1].day, Some(day(1)));
    assert_eq!(association_facts[1].provenance, "observed");

    let recognition_facts = SocialEvent::Relation(recognition).facts();
    assert_eq!(recognition_facts.len(), 2);
    assert_eq!(recognition_facts[0].object, Value::Entity(entity(1)));
    assert_eq!(recognition_facts[1].subject, entity(3));
    assert_eq!(recognition_facts[1].predicate, RECOGNITION_INTERPRETATION);
    assert_eq!(
        recognition_facts[1].object,
        Value::Text("ritual-guardian".to_string())
    );
    assert_eq!(recognition_facts[1].day, Some(day(2)));
    assert_eq!(recognition_facts[1].provenance, "institutional-record");
}

#[test]
fn membership_fact_bundle_preserves_the_exclusive_end() {
    let event = SocialEvent::Membership(
        GroupMembershipEvent::new(entity(1), entity(9), day(2), Some(day(8)), "recorded").unwrap(),
    );

    let facts = event.facts();

    assert_eq!(facts.len(), 2);
    assert_eq!(facts[1].subject, entity(1));
    assert_eq!(facts[1].predicate, MEMBERSHIP_ENDED);
    assert_eq!(facts[1].object, Value::Entity(entity(9)));
    assert_eq!(facts[1].day, Some(day(8)));
    assert_eq!(facts[1].provenance, "recorded");
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
    assert_eq!(
        RelationEvent::new(
            RelationKind::Recognition,
            entity(1),
            entity(2),
            day(1),
            None,
            None,
            Some("   ".to_string()),
            "observed",
        )
        .unwrap_err()
        .to_string(),
        "recognition relation requires an explicit interpretation"
    );
}

#[test]
fn recognition_registry_description_reads_from_recognizer_to_recognized() {
    let mut registry = ConceptRegistry::default();
    register_concepts(&mut registry).unwrap();

    assert_eq!(
        registry.predicate("recognition").unwrap().doc,
        "the recognized subject, directed from the recognizing person or institution"
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

/// A separation closes one interval, not every future association between the
/// same directed participants.
#[test]
fn separation_permits_later_reassociation_without_deleting_prior_facts() {
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
    let prior_association_fact = association.fact();

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
    validate_social_events(&separated).unwrap();
    assert_eq!(separated[0].fact(), prior_association_fact);
}

#[test]
fn separation_closes_only_the_latest_active_association_instance() {
    let association = |start, end, form| {
        SocialEvent::Relation(
            RelationEvent::new(
                RelationKind::Association,
                entity(1),
                entity(2),
                start,
                end,
                Some(AssociationForm::new(form).unwrap()),
                None,
                "observed",
            )
            .unwrap(),
        )
    };

    validate_social_events(&[
        association(day(1), Some(day(10)), "shared-work"),
        association(day(2), Some(day(8)), "companionship"),
        SocialEvent::Lifecycle(
            LifecycleEvent::separate(entity(1), entity(2), day(3), "witnessed").unwrap(),
        ),
        SocialEvent::Lifecycle(
            LifecycleEvent::separate(entity(1), entity(2), day(4), "witnessed").unwrap(),
        ),
    ])
    .unwrap();
}

#[test]
fn separation_must_be_strictly_inside_an_active_association_interval() {
    let association = || {
        SocialEvent::Relation(
            RelationEvent::new(
                RelationKind::Association,
                entity(1),
                entity(2),
                day(1),
                Some(day(5)),
                Some(AssociationForm::new("shared-work").unwrap()),
                None,
                "observed",
            )
            .unwrap(),
        )
    };

    for at in [day(1), day(5), day(6)] {
        let events = vec![
            association(),
            SocialEvent::Lifecycle(
                LifecycleEvent::separate(entity(1), entity(2), at, "witnessed").unwrap(),
            ),
        ];
        assert_eq!(
            validate_social_events(&events).unwrap_err().to_string(),
            "separate must occur strictly inside an active association interval"
        );
    }

    validate_social_events(&[
        association(),
        SocialEvent::Lifecycle(
            LifecycleEvent::separate(entity(1), entity(2), day(4), "witnessed").unwrap(),
        ),
    ])
    .unwrap();
}

#[test]
fn death_allows_inheritance_and_posthumous_recognition_but_not_dead_person_acts() {
    let death =
        SocialEvent::Lifecycle(LifecycleEvent::die(entity(1), day(3), "death-record").unwrap());
    let inheritance = SocialEvent::Relation(
        RelationEvent::new(
            RelationKind::Transfer,
            entity(1),
            entity(2),
            day(4),
            None,
            None,
            None,
            "inheritance-record",
        )
        .unwrap(),
    );
    let posthumous_recognition = SocialEvent::Relation(
        RelationEvent::new(
            RelationKind::Recognition,
            entity(3),
            entity(1),
            day(5),
            None,
            None,
            Some("ancestor".to_string()),
            "council-record",
        )
        .unwrap(),
    );

    validate_social_events(&[death.clone(), inheritance, posthumous_recognition]).unwrap();

    let dead_person_care = SocialEvent::Relation(
        RelationEvent::new(
            RelationKind::Care,
            entity(1),
            entity(2),
            day(4),
            None,
            None,
            None,
            "impossible",
        )
        .unwrap(),
    );
    assert_eq!(
        validate_social_events(&[death, dead_person_care])
            .unwrap_err()
            .to_string(),
        "relation activity is performed by a person after death"
    );
}

#[test]
fn dissolved_groups_cannot_participate_in_later_social_activity() {
    let dissolved =
        SocialEvent::Lifecycle(LifecycleEvent::dissolve(entity(9), day(3), "recorded").unwrap());

    let later_membership = vec![
        dissolved.clone(),
        SocialEvent::Membership(
            GroupMembershipEvent::new(entity(4), entity(9), day(4), None, "observed").unwrap(),
        ),
    ];
    assert_eq!(
        validate_social_events(&later_membership)
            .unwrap_err()
            .to_string(),
        "group membership activity occurs after dissolution"
    );

    let later_care = SocialEvent::Relation(
        RelationEvent::new(
            RelationKind::Care,
            entity(9),
            entity(2),
            day(4),
            None,
            None,
            None,
            "observed",
        )
        .unwrap(),
    );
    assert_eq!(
        validate_social_events(&[dissolved, later_care])
            .unwrap_err()
            .to_string(),
        "relation activity involves a group after dissolution"
    );

    let dissolved =
        SocialEvent::Lifecycle(LifecycleEvent::dissolve(entity(9), day(3), "recorded").unwrap());
    let later_care_for_group = SocialEvent::Relation(
        RelationEvent::new(
            RelationKind::Care,
            entity(2),
            entity(9),
            day(4),
            None,
            None,
            None,
            "observed",
        )
        .unwrap(),
    );
    assert_eq!(
        validate_social_events(&[dissolved, later_care_for_group])
            .unwrap_err()
            .to_string(),
        "relation activity involves a group after dissolution"
    );
}

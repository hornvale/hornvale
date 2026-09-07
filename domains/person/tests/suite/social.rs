//! Realized person social-axis contract tests.

use hornvale_kernel::{EntityId, Value, WorldTime};
use hornvale_person::{
    GENDER_IDENTITY, GENDER_RECOGNITION, PERSON_SOCIAL_PROVENANCE, PersonSocialFact,
    PersonSocialSeed, REPRODUCTIVE_ROLE, SEX_TRAIT, TRANSITIONED,
};

fn entity(raw: u64) -> EntityId {
    EntityId::new(raw).expect("test ids are nonzero")
}

fn day(raw: i64) -> WorldTime {
    WorldTime::from_ticks(raw * WorldTime::TICKS_PER_STD_DAY)
}

/// Deriving any axis from another changes the literal predicate/object list
/// and makes this test fail.
#[test]
fn realized_axes_coexist_without_deriving_one_another() {
    let role = entity(2);
    let social = PersonSocialSeed::new(
        entity(1),
        vec![
            PersonSocialFact::sex_trait("seasonal-morph", day(1), Some(day(10)), "observed")
                .unwrap(),
            PersonSocialFact::reproductive_role(role, day(2), Some(day(8)), "realized-role")
                .unwrap(),
            PersonSocialFact::gender_identity("self-name", day(3), None, "personal-claim").unwrap(),
            PersonSocialFact::gender_recognition(
                "recognized-category",
                day(4),
                Some(day(9)),
                "council-record",
            )
            .unwrap(),
            PersonSocialFact::transitioned("role-change", day(5), None, "witnessed-transition")
                .unwrap(),
        ],
    )
    .unwrap();

    let observed = social
        .social_facts()
        .iter()
        .map(|fact| (fact.predicate(), fact.object().clone()))
        .collect::<Vec<_>>();

    assert_eq!(
        observed,
        vec![
            (SEX_TRAIT, Value::Text("seasonal-morph".to_string())),
            (REPRODUCTIVE_ROLE, Value::Entity(role)),
            (GENDER_IDENTITY, Value::Text("self-name".to_string())),
            (
                GENDER_RECOGNITION,
                Value::Text("recognized-category".to_string())
            ),
            (TRANSITIONED, Value::Text("role-change".to_string())),
        ]
    );
    assert_eq!(social.person(), entity(1));
    assert_eq!(social.social_facts()[0].start(), day(1));
    assert_eq!(social.social_facts()[0].end(), Some(day(10)));
}

/// Dropping the explicit provenance predicate or reversing the person/value
/// direction makes one of these fact-envelope assertions fail.
#[test]
fn ledger_helpers_preserve_person_direction_time_and_provenance() {
    let person = entity(10);
    let social = PersonSocialSeed::new(
        person,
        vec![
            PersonSocialFact::gender_identity("river-self", day(7), None, "speaker:testimony-4")
                .unwrap(),
        ],
    )
    .unwrap();

    let facts = social.facts();

    assert_eq!(facts.len(), 2, "axis fact plus provenance fact");
    assert_eq!(facts[0].subject, person);
    assert_eq!(facts[0].predicate, GENDER_IDENTITY);
    assert_eq!(facts[0].object, Value::Text("river-self".to_string()));
    assert_eq!(facts[0].day, Some(day(7)));
    assert_eq!(facts[0].provenance, "speaker:testimony-4");
    assert_eq!(facts[1].subject, person);
    assert_eq!(facts[1].predicate, PERSON_SOCIAL_PROVENANCE);
    assert_eq!(
        facts[1].object,
        Value::Text("speaker:testimony-4".to_string())
    );
}

/// Accepting an empty interval or whitespace-only provenance makes one of
/// these constructors succeed instead of returning the field-specific error.
#[test]
fn person_social_facts_reject_empty_intervals_and_missing_provenance() {
    assert_eq!(
        PersonSocialFact::sex_trait("morph", day(4), Some(day(4)), "observed")
            .unwrap_err()
            .to_string(),
        "person social interval end must be after its start"
    );
    assert_eq!(
        PersonSocialFact::transitioned("maturation", day(4), None, "   ")
            .unwrap_err()
            .to_string(),
        "person social provenance must not be empty"
    );
}

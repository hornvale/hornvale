//! Pure kinship, care, and inheritance projection tests.

use hornvale_demography::{
    CareKind, ContextRule, KinshipKind, ProjectionBounds, ProjectionEvent, ProjectionRelationKind,
    SocialContext, derive_care, derive_inheritance, derive_kinship,
};
use hornvale_kernel::{EntityId, WorldTime};

fn entity(raw: u64) -> EntityId {
    EntityId::new(raw).expect("test ids are nonzero")
}

fn day(raw: i64) -> WorldTime {
    WorldTime::from_ticks(raw * WorldTime::TICKS_PER_STD_DAY)
}

fn relation(
    kind: ProjectionRelationKind,
    source: u64,
    target: u64,
    start: i64,
    end: Option<i64>,
    detail: Option<&str>,
    provenance: &str,
) -> ProjectionEvent {
    ProjectionEvent::relation(
        kind,
        entity(source),
        entity(target),
        day(start),
        end.map(day),
        detail.map(str::to_string),
        provenance,
    )
    .unwrap()
}

fn bounds() -> ProjectionBounds {
    ProjectionBounds::new(64, 8).unwrap()
}

/// Storing siblings independently, or losing either descent witness, makes
/// this shared-parent derivation or its literal provenance fail.
#[test]
fn siblings_are_derived_from_shared_descent_with_both_sources() {
    let events = vec![
        relation(
            ProjectionRelationKind::Descent,
            1,
            3,
            1,
            None,
            None,
            "first-line",
        ),
        relation(
            ProjectionRelationKind::Descent,
            1,
            2,
            2,
            None,
            None,
            "second-line",
        ),
    ];

    let kinship = derive_kinship(&events, bounds()).unwrap();
    let siblings = kinship
        .iter()
        .filter(|relation| relation.kind() == KinshipKind::Sibling)
        .collect::<Vec<_>>();

    assert_eq!(siblings.len(), 2);
    assert_eq!(siblings[0].subject(), entity(2));
    assert_eq!(siblings[0].relative(), entity(3));
    assert_eq!(siblings[0].provenance(), ["second-line", "first-line"]);
    assert_eq!(siblings[1].subject(), entity(3));
    assert_eq!(siblings[1].relative(), entity(2));
    assert_eq!(siblings[1].provenance(), ["first-line", "second-line"]);
}

/// Treating origin, descent, care, custody, adoption, and recognition as one
/// edge changes these independently derived rows and their evidence sets.
#[test]
fn care_and_adoption_do_not_imply_origin_descent_or_recognition() {
    let events = vec![
        relation(
            ProjectionRelationKind::Origin,
            1,
            4,
            0,
            None,
            None,
            "origin-record",
        ),
        relation(
            ProjectionRelationKind::Descent,
            2,
            4,
            0,
            None,
            None,
            "descent-record",
        ),
        relation(
            ProjectionRelationKind::Care,
            3,
            4,
            1,
            None,
            None,
            "care-record",
        ),
        relation(
            ProjectionRelationKind::Custody,
            3,
            4,
            2,
            None,
            None,
            "custody-record",
        ),
        relation(
            ProjectionRelationKind::Recognition,
            9,
            3,
            3,
            None,
            Some("recognized-guardian"),
            "registry-record",
        ),
    ];

    let kinship = derive_kinship(&events, bounds()).unwrap();
    let care = derive_care(&events, bounds()).unwrap();

    assert!(kinship.iter().any(|relation| {
        relation.kind() == KinshipKind::OriginSource
            && relation.subject() == entity(1)
            && relation.relative() == entity(4)
            && relation.provenance() == ["origin-record"]
    }));
    assert!(kinship.iter().any(|relation| {
        relation.kind() == KinshipKind::Parent
            && relation.subject() == entity(2)
            && relation.relative() == entity(4)
            && relation.provenance() == ["descent-record"]
    }));
    assert!(!kinship.iter().any(|relation| {
        matches!(
            relation.kind(),
            KinshipKind::Parent | KinshipKind::OriginSource
        ) && relation.subject() == entity(3)
    }));

    let kinds = care
        .iter()
        .map(|projection| projection.kind())
        .collect::<Vec<_>>();
    assert_eq!(
        kinds,
        vec![
            CareKind::Care,
            CareKind::Custody,
            CareKind::Adoption,
            CareKind::InstitutionalRecognition,
        ]
    );
    let adoption = care
        .iter()
        .find(|projection| projection.kind() == CareKind::Adoption)
        .unwrap();
    assert_eq!(adoption.provider(), entity(3));
    assert_eq!(adoption.recipient(), entity(4));
    assert_eq!(adoption.provenance(), ["care-record", "custody-record"]);
}

/// Removing death from an inheritance claim, accepting a pre-death transfer,
/// or discarding descent/property context makes one literal assertion fail.
#[test]
fn inheritance_requires_post_death_transfer_and_keeps_all_provenance() {
    let events = vec![
        relation(
            ProjectionRelationKind::Descent,
            1,
            2,
            0,
            None,
            None,
            "descent-ledger",
        ),
        relation(
            ProjectionRelationKind::Transfer,
            1,
            3,
            2,
            None,
            None,
            "ordinary-gift",
        ),
        ProjectionEvent::death(entity(1), day(4), "death-ledger").unwrap(),
        relation(
            ProjectionRelationKind::Transfer,
            1,
            2,
            5,
            None,
            None,
            "estate-transfer",
        ),
    ];
    let mut context = SocialContext::uninterpreted();
    context.property = ContextRule::recognized("lineal-claim", vec![]).unwrap();

    let claims = derive_inheritance(&events, &context, bounds()).unwrap();

    assert_eq!(claims.len(), 1);
    assert_eq!(claims[0].deceased(), entity(1));
    assert_eq!(claims[0].claimant(), entity(2));
    assert!(claims[0].is_descendant());
    assert_eq!(claims[0].recognized_label(), Some("lineal-claim"));
    assert_eq!(
        claims[0].provenance(),
        ["death-ledger", "estate-transfer", "descent-ledger"]
    );
}

/// Removing the explicit depth refusal lets a chain beyond the caller's
/// declared traversal budget silently produce a partial ancestry answer.
#[test]
fn descent_traversal_refuses_paths_beyond_the_explicit_bound() {
    let events = vec![
        relation(ProjectionRelationKind::Descent, 1, 2, 0, None, None, "one"),
        relation(ProjectionRelationKind::Descent, 2, 3, 1, None, None, "two"),
        relation(
            ProjectionRelationKind::Descent,
            3,
            4,
            2,
            None,
            None,
            "three",
        ),
    ];

    let error = derive_kinship(&events, ProjectionBounds::new(16, 2).unwrap()).unwrap_err();

    assert_eq!(
        error.to_string(),
        "descent traversal exceeds the configured depth bound of 2"
    );
}

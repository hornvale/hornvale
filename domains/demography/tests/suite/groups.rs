//! Pure time-bounded group projection tests.

use hornvale_demography::{
    ContextRule, GroupBasis, GroupKey, ProjectionBounds, ProjectionEvent, ProjectionRelationKind,
    SocialContext, derive_groups,
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

fn membership(member: u64, group: u64, start: i64, end: Option<i64>) -> ProjectionEvent {
    ProjectionEvent::membership(
        entity(member),
        entity(group),
        day(start),
        end.map(day),
        &format!("membership-{member}-{group}"),
    )
    .unwrap()
}

fn bounds() -> ProjectionBounds {
    ProjectionBounds::new(64, 8).unwrap()
}

/// Collapsing membership to one household per person loses person 2's two
/// simultaneous groups or either group's independently witnessed care basis.
#[test]
fn care_groups_overlap_without_becoming_a_foundational_household() {
    let events = vec![
        membership(1, 10, 0, None),
        membership(2, 10, 0, None),
        membership(2, 11, 0, None),
        membership(3, 11, 0, None),
        relation(ProjectionRelationKind::Care, 1, 2, 1, None, None, "care-a"),
        relation(ProjectionRelationKind::Care, 3, 2, 1, None, None, "care-b"),
    ];

    let groups = derive_groups(&events, &SocialContext::uninterpreted(), bounds()).unwrap();

    assert_eq!(groups.len(), 2);
    assert!(
        groups
            .iter()
            .all(|group| group.bases().contains(&GroupBasis::Care))
    );
    assert!(groups.iter().all(|group| group.members().len() == 2));
    assert_eq!(
        groups
            .iter()
            .filter(|group| group
                .members()
                .iter()
                .any(|member| member.person() == entity(2)))
            .count(),
        2
    );
}

/// Ignoring interval ends or dissolution makes the old group remain active
/// after migration and loses the append-only dissolution witness.
#[test]
fn migration_and_dissolution_bound_groups_without_erasing_history() {
    let events = vec![
        membership(1, 10, 0, Some(4)),
        relation(
            ProjectionRelationKind::Residence,
            1,
            10,
            0,
            Some(4),
            None,
            "old-residence",
        ),
        membership(1, 11, 4, None),
        relation(
            ProjectionRelationKind::Residence,
            1,
            11,
            4,
            None,
            None,
            "new-residence",
        ),
        ProjectionEvent::dissolution(entity(10), day(5), "dissolved-record").unwrap(),
    ];

    let groups = derive_groups(&events, &SocialContext::uninterpreted(), bounds()).unwrap();
    let old = groups
        .iter()
        .find(|group| group.key() == GroupKey::Declared(entity(10)))
        .unwrap();
    let new = groups
        .iter()
        .find(|group| group.key() == GroupKey::Declared(entity(11)))
        .unwrap();

    assert_eq!(old.start(), day(0));
    assert_eq!(old.end(), Some(day(5)));
    assert_eq!(old.members()[0].end(), Some(day(4)));
    assert!(old.provenance().contains(&"dissolved-record".to_string()));
    assert_eq!(new.start(), day(4));
    assert_eq!(new.end(), None);
}

/// Treating every association as a domestic household or ignoring separation
/// destroys this explicitly association-based, non-residential projection.
#[test]
fn separated_non_household_association_is_a_bounded_group_projection() {
    let events = vec![
        relation(
            ProjectionRelationKind::Association,
            2,
            1,
            1,
            None,
            Some("seasonal-work"),
            "association-record",
        ),
        ProjectionEvent::separation(entity(2), entity(1), day(6), "separation-record").unwrap(),
    ];
    let mut context = SocialContext::uninterpreted();
    context.subsistence =
        ContextRule::recognized("work-band", vec!["seasonal-work".to_string()]).unwrap();

    let groups = derive_groups(&events, &context, bounds()).unwrap();

    assert_eq!(groups.len(), 1);
    assert!(matches!(groups[0].key(), GroupKey::Association { .. }));
    assert_eq!(groups[0].members().len(), 2);
    assert_eq!(groups[0].start(), day(1));
    assert_eq!(groups[0].end(), Some(day(6)));
    assert!(groups[0].bases().contains(&GroupBasis::Association));
    assert!(groups[0].bases().contains(&GroupBasis::Subsistence));
    assert_eq!(groups[0].recognized_labels(), ["work-band"]);
    assert!(!groups[0].bases().contains(&GroupBasis::Residence));
    assert_eq!(
        groups[0].provenance(),
        ["association-record", "separation-record"]
    );
}

/// Closing every matching association at the first separation loses the
/// substrate's latest-active-instance rule for concurrent associations.
#[test]
fn separation_closes_only_the_latest_active_association_instance() {
    let events = vec![
        relation(
            ProjectionRelationKind::Association,
            1,
            2,
            1,
            None,
            Some("shared-work"),
            "older-association",
        ),
        relation(
            ProjectionRelationKind::Association,
            1,
            2,
            2,
            None,
            Some("companionship"),
            "newer-association",
        ),
        ProjectionEvent::separation(entity(1), entity(2), day(3), "first-separation").unwrap(),
        ProjectionEvent::separation(entity(1), entity(2), day(4), "second-separation").unwrap(),
    ];

    let groups = derive_groups(&events, &SocialContext::uninterpreted(), bounds()).unwrap();

    assert_eq!(groups.len(), 2);
    let older = groups.iter().find(|group| group.start() == day(1)).unwrap();
    let newer = groups.iter().find(|group| group.start() == day(2)).unwrap();
    assert_eq!(older.end(), Some(day(4)));
    assert_eq!(
        older.provenance(),
        ["older-association", "second-separation"]
    );
    assert_eq!(newer.end(), Some(day(3)));
    assert_eq!(
        newer.provenance(),
        ["newer-association", "first-separation"]
    );
}

/// Baking one culture's group word into the event graph makes these two
/// contexts produce the same label or mutates their shared input events.
#[test]
fn external_context_changes_recognition_without_changing_events() {
    let events = vec![relation(
        ProjectionRelationKind::Association,
        1,
        2,
        1,
        None,
        Some("mutual-aid"),
        "association-record",
    )];
    let original = events.clone();
    let mut first = SocialContext::uninterpreted();
    first.contact =
        ContextRule::recognized("guest-circle", vec!["mutual-aid".to_string()]).unwrap();
    let mut second = SocialContext::uninterpreted();
    second.contact =
        ContextRule::recognized("treaty-band", vec!["mutual-aid".to_string()]).unwrap();

    let first_groups = derive_groups(&events, &first, bounds()).unwrap();
    let second_groups = derive_groups(&events, &second, bounds()).unwrap();

    assert_eq!(first_groups[0].recognized_labels(), ["guest-circle"]);
    assert_eq!(second_groups[0].recognized_labels(), ["treaty-band"]);
    assert_eq!(events, original);
}

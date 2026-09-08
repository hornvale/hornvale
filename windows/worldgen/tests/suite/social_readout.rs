//! Composition-root bridge from realized history events to pure projections.

use hornvale_demography::{GroupBasis, GroupKey, KinshipKind, ProjectionBounds, SocialContext};
use hornvale_history::{LifecycleEvent, SocialEvent};
use hornvale_kernel::{EntityId, WorldTime};
use hornvale_worldgen::{SyntheticSociety, derive_social_readout};

use super::social_projection::project;

fn bounds() -> ProjectionBounds {
    ProjectionBounds::new(128, 8).unwrap()
}

/// Dropping relation/lifecycle provenance in the worldgen adapter removes at
/// least one witness from this realized death-to-inheritance readout.
#[test]
fn dual_descent_events_reach_kinship_and_inheritance_projections() {
    let projection = project(SyntheticSociety::DualDescent);
    let readout = derive_social_readout(
        projection.events(),
        &SocialContext::uninterpreted(),
        bounds(),
    )
    .unwrap();

    assert!(
        readout
            .kinship()
            .iter()
            .filter(|relation| relation.kind() == KinshipKind::Parent)
            .count()
            >= 2
    );
    assert_eq!(readout.inheritance().len(), 1);
    assert_eq!(readout.inheritance()[0].provenance().len(), 3);
    assert!(
        readout.inheritance()[0]
            .provenance()
            .iter()
            .all(|source| source.as_str() == "social/projection/v1")
    );
}

/// Replacing overlapping membership with one household per person makes the
/// realized care-cluster projection lose one of these two care groups.
#[test]
fn care_cluster_events_reach_two_overlapping_group_projections() {
    let projection = project(SyntheticSociety::CareCluster);
    let readout = derive_social_readout(
        projection.events(),
        &SocialContext::uninterpreted(),
        bounds(),
    )
    .unwrap();

    let care_groups = readout
        .groups()
        .iter()
        .filter(|group| {
            matches!(group.key(), GroupKey::Declared(_))
                && group.bases().contains(&GroupBasis::Care)
        })
        .count();
    assert_eq!(care_groups, 2);
}

/// Ignoring membership ends, residence intervals, separation, or dissolution
/// makes the realized recomposition history project as one unbounded group.
#[test]
fn recomposing_mobility_events_keep_distinct_time_bounded_groups() {
    let projection = project(SyntheticSociety::RecomposingMobility);
    let readout = derive_social_readout(
        projection.events(),
        &SocialContext::uninterpreted(),
        bounds(),
    )
    .unwrap();

    let declared = readout
        .groups()
        .iter()
        .filter(|group| matches!(group.key(), GroupKey::Declared(_)))
        .collect::<Vec<_>>();
    assert_eq!(declared.len(), 2);
    assert!(declared.iter().any(|group| group.end().is_some()));
    assert!(declared.iter().any(|group| group.end().is_none()));
    assert!(
        readout
            .groups()
            .iter()
            .any(|group| matches!(group.key(), GroupKey::Association { .. })
                && group.end().is_some())
    );
}

/// Converting events before validating their ordered-history contract lets an
/// unmatched separation pass through the public bridge as an empty readout.
#[test]
fn sequence_invalid_history_is_refused_at_the_readout_bridge() {
    let source = EntityId::new(1).unwrap();
    let target = EntityId::new(2).unwrap();
    let events = vec![SocialEvent::Lifecycle(
        LifecycleEvent::separate(
            source,
            target,
            WorldTime::from_ticks(WorldTime::TICKS_PER_STD_DAY),
            "unmatched-separation",
        )
        .unwrap(),
    )];

    let error =
        derive_social_readout(&events, &SocialContext::uninterpreted(), bounds()).unwrap_err();

    assert_eq!(
        error.to_string(),
        "separate has no matching active association"
    );
}

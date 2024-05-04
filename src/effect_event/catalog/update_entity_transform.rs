use bevy::prelude::*;

/// The update-entity-transform event.
#[derive(Debug, Clone, Copy, Event)]
pub struct UpdateEntityTransformEffect(pub Entity, pub Vec3);

impl UpdateEntityTransformEffect {
  /// Apply the effect.
  pub fn apply(
    mut update_entity_transforms: EventReader<UpdateEntityTransformEffect>,
    mut query: Query<&mut Transform>,
  ) {
    for UpdateEntityTransformEffect(entity, delta) in update_entity_transforms.read() {
      if let Ok(mut transform) = query.get_mut(*entity) {
        transform.translation += *delta;
      }
    }
  }
}

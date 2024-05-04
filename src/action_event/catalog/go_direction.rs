use crate::direction::Direction;
use crate::effect_event::prelude::*;
use crate::player::prelude::*;
use crate::rogue_view::prelude::*;
use bevy::prelude::*;

/// The go-direction action event.
#[derive(Debug, Clone, Copy, Event)]
pub struct GoDirectionAction(pub Entity, pub Direction);

impl GoDirectionAction {
  /// When a go-direction action is received, attempt to move the entity.
  pub fn apply(
    mut go_direction_actions: EventReader<GoDirectionAction>,
    mut query: Query<(&mut Transform, Option<&Player>)>,
    camera_query: Query<Entity, With<RogueViewCamera>>,
    mut update_entity_transform_effects: EventWriter<UpdateEntityTransformEffect>,
  ) {
    for GoDirectionAction(entity, direction) in go_direction_actions.read() {
      if let Ok((mut _transform, player)) = query.get_mut(*entity) {
        let delta = Vec3::new(
          direction.to_ivec3().x as f32 * 16.0,
          direction.to_ivec3().y as f32 * 16.0,
          0.0,
        );
        update_entity_transform_effects.send(UpdateEntityTransformEffect(*entity, delta));
        if player.is_some() {
          let entity = camera_query.single();
          update_entity_transform_effects.send(UpdateEntityTransformEffect(entity, delta));
        }
      }
    }
  }
}

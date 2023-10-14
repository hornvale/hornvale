use anyhow::Error as AnyError;
use specs::prelude::*;

use crate::effect::Effect;
use crate::effect::EffectData;
use crate::entity_uuid::ActorUuid;
use crate::entity_uuid::RoomUuid;
use crate::event::OutputEvent;

/// The `EffectType` enum.
///
/// This should be an exhaustive collection of effects.
///
/// Effects should be phrased in the imperative mood.
#[derive(Clone, Debug, Default, Deserialize, PartialEq, Eq, Hash, Ord, PartialOrd, Serialize)]
pub enum EffectType {
  /// No-Op -- absolutely nothing happens.
  #[default]
  NoOp,
  /// Output a blank line.
  OutputBlankLine,
  /// Place an actor in a room.
  PlaceActorInRoom(ActorUuid, RoomUuid),
  /// SetInputReadyFlag -- sets the input-ready flag.
  SetInputReadyFlag(bool),
  /// QuitGame -- the player quit.
  SetQuitFlag(bool),
}

impl EffectType {
  pub fn apply(&self, _effect: &Effect, data: &mut EffectData) -> Result<(), AnyError> {
    debug!("Applying {:#?} effect.", &self);
    use EffectType::*;
    #[allow(unreachable_patterns)]
    match self {
      NoOp => {
        debug!("Applying no-op effect.");
      },
      OutputBlankLine => {
        debug!("Applying output-blank-line effect.");
        data
          .output_event_channel
          .single_write(OutputEvent { output: "".to_string() });
      },
      PlaceActorInRoom(actor_uuid, _room_uuid) => {
        debug!("Applying place-actor-in-room effect.");
        let _actor_entity = (&data.entities, &data.is_an_actor_component)
          .join()
          .find(|(_, is_an_actor)| is_an_actor.0.uuid == *actor_uuid);
        // game_state.current_room_id = Some(room_uuid.clone());
      },
      SetInputReadyFlag(value) => {
        debug!("Applying set-input-ready-flag effect.");
        data.input_ready_flag_resource.0 = *value;
      },
      SetQuitFlag(value) => {
        debug!("Applying set-quit-flag effect.");
        data.quit_flag_resource.0 = *value;
      },
    }
    Ok(())
  }
}

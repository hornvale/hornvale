use anyhow::Error as AnyError;
use specs::prelude::*;

use crate::component::IsInRoomComponent;
use crate::effect::Effect;
use crate::effect::EffectBuilder;
use crate::effect::EffectData;
use crate::event::OutputEvent;
use crate::output::Output;
use crate::output::OutputType;

/// The `EffectType` enum.
///
/// This should be an exhaustive collection of effects.
///
/// Effects should be phrased in the imperative mood.
#[derive(Clone, Debug, Default, PartialEq, Eq, Hash, Ord, PartialOrd)]
pub enum EffectType {
  /// No-Op -- absolutely nothing happens.
  #[default]
  NoOp,
  /// Output a blank line.
  OutputBlankLine,
  /// PlaceActorInRoom(Actor, Room).
  PlaceActorInRoom(Entity, Entity),
  /// SetInputReadyFlag -- sets the input-ready flag.
  SetInputReadyFlag(bool),
  /// QuitGame -- the player quit.
  SetQuitFlag(bool),
  /// ShowRoomDescription(Room).
  ShowRoomDescription(Entity),
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
        data.output_event_channel.single_write(OutputEvent {
          output: Output {
            output_type: OutputType::BlankLine,
          },
        });
      },
      PlaceActorInRoom(actor_entity, room_entity) => {
        debug!("Applying place-actor-in-room effect.");
        data
          .is_in_room_component
          .insert(*actor_entity, IsInRoomComponent(*room_entity))?;
        if let Some(player_entity) = &data.player_resource.entity {
          if actor_entity == player_entity {
            let effect = EffectBuilder::default()
              .effect_type(EffectType::ShowRoomDescription(*room_entity))
              .build()?;
            effect.apply(data)?;
          }
        }
      },
      SetInputReadyFlag(value) => {
        debug!("Applying set-input-ready-flag effect.");
        data.input_ready_flag_resource.0 = *value;
      },
      SetQuitFlag(value) => {
        debug!("Applying set-quit-flag effect.");
        data.quit_flag_resource.0 = *value;
      },
      ShowRoomDescription(room_entity) => {
        debug!("Applying show-room-description effect.");
        let room = &data.is_a_room_component.get(*room_entity).unwrap().0;
        data.output_event_channel.single_write(OutputEvent {
          output: Output {
            output_type: OutputType::RoomName(room.name.clone()),
          },
        });
        data.output_event_channel.single_write(OutputEvent {
          output: Output {
            output_type: OutputType::RoomDescription(room.description.clone()),
          },
        });
        /*
        data.output_event_channel.single_write(OutputEvent {
          output: Output {
            output_type: OutputType::RoomExits(RoomPassagesDescriber {}.describe(&room.passages)),
          },
        });
        */
        data.output_event_channel.single_write(OutputEvent {
          output: Output {
            output_type: OutputType::BlankLine,
          },
        });
      },
    }
    Ok(())
  }
}

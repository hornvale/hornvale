use anyhow::Error as AnyError;

use crate::effect::Effect;
use crate::entity_id::EntityId;
use crate::entity_id::RoomId;
use crate::game_state::DiegeticFlagTrait;
use crate::game_state::GameState;
use crate::game_state::InputReadyFlagTrait;
use crate::game_state::OutputQueueTrait;
use crate::game_state::QuitFlagTrait;
use crate::game_state::RoomsTrait;
use crate::game_state::TickCounterTrait;

/// The `Type` enum.
///
/// This should be an exhaustive collection of effects.
#[derive(Clone, Debug, Default, PartialEq, Eq, Hash, Ord, PartialOrd)]
pub enum Type {
  /// No-Op -- absolutely nothing happens.
  #[default]
  NoOp,
  /// IncrementTickCounter -- increments the tick counter.
  IncrementTickCounter,
  /// SetDiegeticFlag -- sets the diegetic flag.
  SetDiegeticFlag(bool),
  /// SetInputReadyFlag -- sets the input-ready flag.
  SetInputReadyFlag(bool),
  /// QuitGame -- the player quit.
  SetQuitFlag(bool),
  /// Place an entity in a room.
  PlaceEntityInRoom(EntityId, RoomId),
  /// Output a room description.
  OutputRoomDescription(RoomId),
}

impl Type {
  /// Creates a new `Type`.
  pub fn new() -> Self {
    Self::default()
  }

  pub fn apply(&self, _effect: &Effect, game_state: &mut GameState) -> Result<(), AnyError> {
    debug!("Applying {:#?} effect.", &self);
    use Type::*;
    #[allow(unreachable_patterns)]
    match self {
      NoOp => {
        debug!("Applying no-op effect.");
      },
      IncrementTickCounter => {
        debug!("Applying increment-tick-counter effect.");
        game_state.increment_tick_counter();
      },
      SetDiegeticFlag(value) => {
        debug!("Applying set-diegetic-flag effect.");
        game_state.set_diegetic_flag(*value);
      },
      SetInputReadyFlag(value) => {
        debug!("Applying set-input-ready-flag effect.");
        game_state.set_input_ready_flag(*value);
      },
      SetQuitFlag(value) => {
        debug!("Applying set-quit-flag effect.");
        game_state.set_quit_flag(*value);
      },
      PlaceEntityInRoom(_entity_id, room_id) => {
        debug!("Applying place-entity-in-room effect.");
        game_state.current_room_id = room_id.clone();
      },
      OutputRoomDescription(room_id) => {
        debug!("Applying output-room-description effect.");
        let (room_name, room_description) = {
          let room = game_state.get_room(room_id).unwrap();
          (room.name.clone(), room.description.clone())
        };
        game_state.enqueue_output(room_name);
        game_state.enqueue_output(room_description);
      },
      _ => unimplemented!(),
    }
    Ok(())
  }
}

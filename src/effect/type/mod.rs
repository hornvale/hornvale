use anyhow::Error as AnyError;

use crate::effect::Effect;
use crate::entity_id::EntityId;
use crate::entity_id::RoomId;
use crate::game_state::DiegeticFlagTrait;
use crate::game_state::GameState;
use crate::game_state::InputReadyFlagTrait;
use crate::game_state::OutputQueueTrait;
use crate::game_state::QuitFlagTrait;
use crate::game_state::TickCounterTrait;

/// The `Type` enum.
///
/// This should be an exhaustive collection of effects.
///
/// Effects should be phrased as imperative commands.
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
  /// Output a room name as part of the room summary.
  OutputRoomNameAsPartOfRoomSummary(String),
  /// Output a room description as part of the room summary.
  OutputRoomDescriptionAsPartOfRoomSummary(String),
  /// Output a room's passages as part of the room summary.
  OutputRoomPassagesAsPartOfRoomSummary(String),
  /// Output a blank line.
  OutputBlankLine,
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
      OutputRoomNameAsPartOfRoomSummary(room_name) => {
        debug!("Applying output-room-name effect.");
        game_state.enqueue_output(room_name);
      },
      OutputRoomDescriptionAsPartOfRoomSummary(room_description) => {
        debug!("Applying output-room-description effect.");
        game_state.enqueue_output(room_description);
      },
      OutputRoomPassagesAsPartOfRoomSummary(room_passages) => {
        debug!("Applying output-room-passages effect.");
        game_state.enqueue_output(room_passages);
      },
      OutputBlankLine => {
        debug!("Applying output-blank-line effect.");
        game_state.enqueue_output("");
      },
      _ => unimplemented!(),
    }
    Ok(())
  }
}

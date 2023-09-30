use anyhow::Error as AnyError;

use crate::effect::Effect;
use crate::effect::EffectType;
use crate::entity_id::EntityId;
use crate::entity_id::RoomId;
use crate::event::Event;
use crate::game_state::GameState;

/// The `Type` enum.
///
/// This should be an exhaustive collection of events.
#[derive(Clone, Debug, Default, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum Type {
  /// None -- never happens.
  #[default]
  None,
  /// No-Op -- absolutely nothing happens.
  NoOp,
  /// StartedGame -- the game has started.
  StartedGame,
  /// QuitGame -- the player quit.
  QuitGame,
  PlayerWillExitRoom(RoomId),
  EntityWillExitRoom(EntityId, RoomId),
  EntityDidExitRoom(EntityId, RoomId),
  PlayerDidExitRoom(RoomId),
  EntityDidEnterRoom(EntityId, RoomId),
  PlayerDidEnterRoom(RoomId),
  PlayerWillEnterRoom(RoomId),
  EntityWillEnterRoom(EntityId, RoomId),
}

impl Type {
  /// Creates a new `Type`.
  pub fn new() -> Self {
    Self::default()
  }

  pub fn process(&self, event: &Event, game_state: &mut GameState) -> Result<(), AnyError> {
    debug!("Processing {:#?} event.", self);
    use Type::*;
    #[allow(unreachable_patterns)]
    match self {
      NoOp => {
        debug!("Applying no-op event.");
      },
      StartedGame => {
        debug!("Applying start-game event.");
      },
      QuitGame => {
        debug!("Applying quit-game event.");
        Effect::new(EffectType::SetQuitFlag(true), event.backtrace.clone()).apply(game_state)?;
      },
      _ => {
        // By default, we let subscribers react to the event.
      },
    }
    Ok(())
  }
}

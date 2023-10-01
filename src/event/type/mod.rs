use anyhow::Error as AnyError;

use crate::effect::Effect;
use crate::effect::EffectType;
use crate::entity_id::EntityId;
use crate::entity_id::RoomId;
use crate::event::Event;
use crate::event::DEFAULT_PRIORITY;
use crate::game_state::GameState;

/// The `Type` enum.
///
/// This should be an exhaustive collection of events.
///
/// Events should be phrased in the present tense.
#[derive(Clone, Debug, Default, Deserialize, Eq, Hash, Ord, PartialEq, PartialOrd, Serialize)]
pub enum Type {
  /// None -- never happens.
  #[default]
  None,
  /// No-Op -- absolutely nothing happens.
  NoOp,
  /// StartsGame -- the game starts.
  StartsGame,
  /// QuitsGame -- the player quits.
  QuitsGame,
  /// ShowsRoomDescription -- a room description is shown.
  ShowsRoomDescription(RoomId),
  /// An entity appeared in a room.
  EntityAppearsInRoom(EntityId, RoomId),
  /// EntityWalksFromRoomToRoom -- an entity walked from one room to another.
  EntityWalksFromRoomToRoom(EntityId, RoomId, RoomId),
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
      StartsGame => {
        debug!("Applying start-game event.");
      },
      QuitsGame => {
        debug!("Applying quit-game event.");
        Effect::new(EffectType::SetQuitFlag(true), event.backtrace.clone()).apply(game_state)?;
      },
      EntityAppearsInRoom(entity_id, room_id) => {
        debug!("Applying entity-appears-in-room event.");
        Effect::new(
          EffectType::PlaceEntityInRoom(entity_id.clone(), room_id.clone()),
          event.backtrace.clone(),
        )
        .apply(game_state)?;
      },
      EntityWalksFromRoomToRoom(entity_id, _start_room_id, end_room_id) => {
        debug!("Applying entity-walks-from-room-to-room event.");
        Effect::new(
          EffectType::PlaceEntityInRoom(entity_id.clone(), end_room_id.clone()),
          event.backtrace.clone(),
        )
        .apply(game_state)?;
      },
      ShowsRoomDescription(room_id) => {
        debug!("Applying show-room-description event.");
        Effect::new(
          EffectType::OutputRoomDescription(room_id.clone()),
          event.backtrace.clone(),
        )
        .apply(game_state)?;
      },
      _ => {
        // By default, we let subscribers react to the event, but error-log that we did nothing.
        error!("Letting subscribers react to event {:#?}.", self);
      },
    }
    Ok(())
  }

  pub fn get_priority(&self) -> i64 {
    use Type::*;
    match self {
      StartsGame => 1000,
      QuitsGame => 1000,
      _ => DEFAULT_PRIORITY,
    }
  }
}

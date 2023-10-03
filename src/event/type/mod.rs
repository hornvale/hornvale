use anyhow::Error as AnyError;

use crate::effect::Effect;
use crate::effect::EffectType;
use crate::entity_id::ActorId;
use crate::entity_id::EntityId;
use crate::entity_id::RoomId;
use crate::event::Event;
use crate::event::EventTag;
use crate::event::DEFAULT_PRIORITY;
use crate::game_state::EventQueueTrait;
use crate::game_state::GameState;
use crate::game_state::RoomsTrait;

/// The `Type` enum.
///
/// This should be an exhaustive collection of events.
///
/// Events should be phrased as third-person, present tense, indicative mood.
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
  /// ShowsRoomSummary -- a room name, description, etc is shown.
  ShowsRoomSummary(RoomId),
  /// Shows the room's name.
  ShowsRoomNameAsPartOfRoomSummary(String),
  /// Shows the room's description.
  ShowsRoomDescriptionAsPartOfRoomSummary(String),
  /// Shows the room's passages.
  ShowsRoomPassagesAsPartOfRoomSummary(String),
  /// An entity appeared in a room.
  EntityAppearsInRoom(EntityId, RoomId),
  /// EntityWalksFromRoomToRoom -- an entity walked from one room to another.
  EntityWalksFromRoomToRoom(ActorId, RoomId, RoomId),
  /// EntityLooksAroundRoom -- an entity looked around a room.
  EntityLooksAroundRoom(ActorId, RoomId),
  /// Outputs a blank line.
  OutputsBlankLine,
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
        debug!("Processing no-op event.");
      },
      StartsGame => {
        debug!("Processing start-game event.");
      },
      QuitsGame => {
        debug!("Processing quit-game event.");
        Effect::new(EffectType::SetQuitFlag(true), event.backtrace.clone()).apply(game_state)?;
      },
      EntityAppearsInRoom(entity_id, room_id) => {
        debug!("Processing entity-appears-in-room event.");
        Effect::new(
          EffectType::PlaceEntityInRoom(entity_id.clone(), room_id.clone()),
          event.backtrace.clone(),
        )
        .apply(game_state)?;
      },
      EntityWalksFromRoomToRoom(entity_id, _start_room_id, end_room_id) => {
        debug!("Processing entity-walks-from-room-to-room event.");
        Effect::new(
          EffectType::PlaceEntityInRoom(entity_id.clone().into(), end_room_id.clone()),
          event.backtrace.clone(),
        )
        .apply(game_state)?;
      },
      EntityLooksAroundRoom(_entity_id, room_id) => {
        debug!("Processing entity-looks-around-room event.");
        if event.tags.contains(&EventTag::HasPlayerAsPrincipalActor) {
          let event = Event::new(
            Type::ShowsRoomSummary(room_id.clone()),
            1,
            event.backtrace.clone(),
            vec![EventTag::IsInRoom(room_id.clone())],
          );
          game_state.enqueue_event(event);
        }
      },
      ShowsRoomSummary(room_id) => {
        debug!("Processing show-room-description event.");
        let (room_name, room_description, room_passages) = {
          let room = game_state.get_room(room_id).unwrap();
          (room.name.clone(), room.description.clone(), room.describe_passages())
        };
        game_state.enqueue_event(Event::new(
          Type::ShowsRoomNameAsPartOfRoomSummary(room_name),
          3,
          event.backtrace.clone(),
          vec![EventTag::IsInRoom(room_id.clone())],
        ));
        game_state.enqueue_event(Event::new(
          Type::ShowsRoomDescriptionAsPartOfRoomSummary(room_description),
          2,
          event.backtrace.clone(),
          vec![EventTag::IsInRoom(room_id.clone())],
        ));
        game_state.enqueue_event(Event::new(
          Type::ShowsRoomPassagesAsPartOfRoomSummary(room_passages),
          1,
          event.backtrace.clone(),
          vec![EventTag::IsInRoom(room_id.clone())],
        ));
      },
      ShowsRoomNameAsPartOfRoomSummary(room_name) => {
        debug!("Processing output-room-name event.");
        Effect::new(
          EffectType::OutputRoomNameAsPartOfRoomSummary(room_name.clone()),
          event.backtrace.clone(),
        )
        .apply(game_state)?;
      },
      ShowsRoomDescriptionAsPartOfRoomSummary(room_description) => {
        debug!("Processing output-room-description event.");
        Effect::new(
          EffectType::OutputRoomDescriptionAsPartOfRoomSummary(room_description.clone()),
          event.backtrace.clone(),
        )
        .apply(game_state)?;
      },
      ShowsRoomPassagesAsPartOfRoomSummary(room_passages) => {
        debug!("Processing output-room-passages event.");
        Effect::new(
          EffectType::OutputRoomPassagesAsPartOfRoomSummary(room_passages.clone()),
          event.backtrace.clone(),
        )
        .apply(game_state)?;
      },
      OutputsBlankLine => {
        debug!("Processing output-blank-line event.");
        Effect::new(EffectType::OutputBlankLine, event.backtrace.clone()).apply(game_state)?;
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

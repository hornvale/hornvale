use crate::action::Action;
use crate::action::ActionData;
use crate::action::ActionError;
use crate::entity_uuid::ActorUuid;
use crate::entity_uuid::RoomUuid;
use crate::passage::PassageDirection;

/// The `ActionType` enum.
///
/// This should be an exhaustive collection of actions.
///
/// Actions should be phrased in the imperative mood.
#[derive(Clone, Debug, Default, Deserialize, PartialEq, Serialize)]
pub enum ActionType {
  /// No-Op -- absolutely nothing happens.
  #[default]
  NoOp,
  /// QuitGame -- the player quit.
  QuitGame,
  /// Look -- an actor looked around.
  LookAround(ActorUuid, RoomUuid),
  /// Walk -- an actor walked in a passage direction.
  Walk(ActorUuid, PassageDirection),
}

impl ActionType {
  /// Creates a new `Type`.
  pub fn new() -> Self {
    Self::default()
  }

  pub fn attempt(&self, _action: &Action, _data: &mut ActionData) -> Result<(), Box<ActionError>> {
    debug!("Attempting {:#?} action.", self);
    use ActionType::*;
    #[allow(unreachable_patterns)]
    match self {
      NoOp => {
        debug!("Attempting no-op action.");
        // let event = Event::new(EventType::NoOp, DEFAULT_PRIORITY, action.backtrace.clone(), vec![]);
        // game_state.enqueue_event(event);
      },
      QuitGame => {
        debug!("Attempting quit-game action.");
        // let event = Event::new(EventType::QuitsGame, DEFAULT_PRIORITY, action.backtrace.clone(), vec![]);
        // game_state.enqueue_event(event);
      },
      LookAround(actor_id, room_id) => {
        debug!("Actor {} is attempting look-around action in {}.", actor_id, room_id);
        // let mut tags = Vec::new();
        // tags.push(EventTag::HasPrincipalActor(actor_id.clone()));
        // tags.push(EventTag::IsInRoom(room_id.clone()));
        // if actor_id == &game_state.player_id.clone().into() {
        //   tags.push(EventTag::HasPlayerAsPrincipalActor);
        // }
        // let event = Event::new(
        //   EventType::ActorLooksAroundRoom(actor_id.clone(), room_id.clone()),
        //   DEFAULT_PRIORITY,
        //   action.backtrace.clone(),
        //   tags,
        // );
        // game_state.enqueue_event(event);
      },
      Walk(_actor_id, _direction) => {
        debug!("Attempting walk action.");
        // let current_room_id = game_state.current_room_id.clone().unwrap();
        // let current_room = if let Some(current_room) = game_state.rooms.get(&current_room_id) {
        //   current_room
        // } else {
        //   return Err(Box::new(ActionError::NotInARoom {
        //     action: action.clone(),
        //     current_room_id: current_room_id.clone(),
        //   }));
        // };
        // let passage = {
        //   if let Some(passage) = current_room.passages.get(direction) {
        //     passage
        //   } else {
        //     return Err(Box::new(ActionError::NoPassageInThatDirection {
        //       /// The `Action`.
        //       action: action.clone(),
        //       /// The current room's `RoomId`.
        //       current_room_id: current_room_id.clone(),
        //       /// The current room.
        //       current_room: current_room.clone(),
        //       /// The `PassageDirection`.
        //       direction: direction.clone(),
        //     }));
        //   }
        // };
        // let destination_room_id = passage.destination.clone();
        // let mut tags = Vec::new();
        // tags.push(EventTag::HasPrincipalActor(actor_id.clone()));
        // if actor_id == &game_state.player_id.clone().into() {
        //   tags.push(EventTag::HasPlayerAsPrincipalActor);
        // }
        // let event = Event::new(
        //   EventType::ActorMovesFromRoomToRoom(actor_id.clone(), current_room_id, destination_room_id),
        //   DEFAULT_PRIORITY,
        //   action.backtrace.clone(),
        //   tags,
        // );
        // game_state.enqueue_event(event);
      },
    }
    Ok(())
  }
}

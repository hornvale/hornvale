use crate::action::Action;
use crate::action::ActionError;
use crate::entity_id::EntityId;
use crate::event::Event;
use crate::event::EventType;
use crate::event::DEFAULT_PRIORITY;
use crate::game_state::EventQueueTrait;
use crate::game_state::GameState;
use crate::passage::PassageDirection;

/// The `ActionType` enum.
///
/// This should be an exhaustive collection of actions.
#[derive(Clone, Debug, Default, PartialEq)]
pub enum Type {
  /// No-Op -- absolutely nothing happens.
  #[default]
  NoOp,
  /// QuitGame -- the player quit.
  QuitGame,
  /// Walk -- an entity walked in a passage direction.
  Walk(EntityId, PassageDirection),
}

impl Type {
  /// Creates a new `Type`.
  pub fn new() -> Self {
    Self::default()
  }

  pub fn attempt(&self, action: &Action, game_state: &mut GameState) -> Result<(), Box<ActionError>> {
    debug!("Attempting {:#?} action.", self);
    use Type::*;
    #[allow(unreachable_patterns)]
    match self {
      NoOp => {
        debug!("Attempting no-op action.");
        let event = Event::new(EventType::NoOp, DEFAULT_PRIORITY, action.backtrace.clone());
        game_state.enqueue_event(event);
      },
      QuitGame => {
        debug!("Attempting quit-game action.");
        let event = Event::new(EventType::QuitsGame, DEFAULT_PRIORITY, action.backtrace.clone());
        game_state.enqueue_event(event);
      },
      Walk(entity_id, direction) => {
        debug!("Attempting walk action.");
        let current_room_id = game_state.current_room_id.clone();
        let current_room = if let Some(current_room) = game_state.rooms.get(&current_room_id) {
          current_room
        } else {
          return Err(Box::new(ActionError::NotInARoom {
            action: action.clone(),
            current_room_id: current_room_id.clone(),
          }));
        };
        let passage = {
          if let Some(passage) = current_room.passages.get(direction) {
            passage
          } else {
            return Err(Box::new(ActionError::NoPassageInThatDirection {
              /// The `Action`.
              action: action.clone(),
              /// The current room's `RoomId`.
              current_room_id: current_room_id.clone(),
              /// The current room.
              current_room: current_room.clone(),
              /// The `PassageDirection`.
              direction: direction.clone(),
            }));
          }
        };
        let destination_room_id = passage.destination.clone();
        let event = Event::new(
          EventType::EntityWalksFromRoomToRoom(entity_id.clone(), current_room_id, destination_room_id),
          DEFAULT_PRIORITY,
          action.backtrace.clone(),
        );
        game_state.enqueue_event(event);
      },
      _ => unimplemented!(),
    }
    Ok(())
  }
}

#[cfg(test)]
mod tests {

  use super::*;

  use crate::action::Action;
  use crate::game_state::GameState;
  use crate::test::init;

  #[test]
  fn test_new() {
    init();
    let _type = Type::new();
  }

  #[test]
  fn test_attempt() {
    init();
    let mut game_state = GameState::new();
    let action = Action::new(Type::NoOp, vec![]);
    let _ = Type::NoOp.attempt(&action, &mut game_state);
  }
}

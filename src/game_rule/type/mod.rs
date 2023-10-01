use std::sync::Arc;

use crate::entity_id::EntityId;
use crate::entity_id::RoomId;
use crate::event::DidProcessFn;
use crate::event::EventFilterRule;
use crate::event::EventType;
use crate::event::ShouldProcessFn;
use crate::event::WillProcessFn;

/// The `GameRuleType` enum.
#[derive(Clone, Copy, Debug, Deserialize, Display, Eq, Hash, PartialEq, PartialOrd, Serialize)]
pub enum Type {
  ShowRoomDescriptionWhenAppearingInRoom,
  ShowRoomDescriptionWhenEnteringRoom,
}

impl Type {
  pub fn iterator() -> impl Iterator<Item = Type> {
    use Type::*;
    [
      ShowRoomDescriptionWhenAppearingInRoom,
      ShowRoomDescriptionWhenEnteringRoom,
    ]
    .iter()
    .copied()
  }

  /// Get the priority.
  pub fn get_priority(&self) -> i64 {
    use Type::*;
    match self {
      ShowRoomDescriptionWhenAppearingInRoom => 0,
      ShowRoomDescriptionWhenEnteringRoom => 0,
    }
  }

  /// Get the event type.
  pub fn get_event_type(&self) -> EventType {
    use Type::*;
    match self {
      ShowRoomDescriptionWhenAppearingInRoom => EventType::EntityAppearsInRoom(EntityId::default(), RoomId::default()),
      ShowRoomDescriptionWhenEnteringRoom => {
        EventType::EntityWalksFromRoomToRoom(EntityId::default(), RoomId::default(), RoomId::default())
      },
    }
  }

  /// Get the filter rule.
  pub fn get_filter_rule(&self) -> EventFilterRule {
    use Type::*;
    match self {
      ShowRoomDescriptionWhenAppearingInRoom => EventFilterRule::Always,
      ShowRoomDescriptionWhenEnteringRoom => EventFilterRule::Always,
    }
  }

  /// Get the should process function.
  pub fn get_should_process(&self) -> ShouldProcessFn {
    use Type::*;
    match self {
      ShowRoomDescriptionWhenAppearingInRoom => Arc::new(|_event, _game_state| None),
      ShowRoomDescriptionWhenEnteringRoom => Arc::new(|_event, _game_state| None),
    }
  }

  /// Get the will process function.
  pub fn get_will_process(&self) -> WillProcessFn {
    use Type::*;
    match self {
      ShowRoomDescriptionWhenAppearingInRoom => Arc::new(|_event, _game_state| {
        println!("omg appeared in room");
      }),
      ShowRoomDescriptionWhenEnteringRoom => Arc::new(|_event, _game_state| {
        println!("omg entered room");
      }),
    }
  }

  /// Get the did process function.
  pub fn get_did_process(&self) -> DidProcessFn {
    use Type::*;
    match self {
      ShowRoomDescriptionWhenAppearingInRoom => Arc::new(|_event, _game_state| {}),
      ShowRoomDescriptionWhenEnteringRoom => Arc::new(|_event, _game_state| {}),
    }
  }

  /// Whether this is _by default_ enabled or not.
  pub fn is_enabled(&self) -> bool {
    true
  }
}

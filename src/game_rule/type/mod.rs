use std::sync::Arc;

use crate::entity_id::EntityId;
use crate::entity_id::RoomId;
use crate::event::DidProcessFn;
use crate::event::EventBuilder;
use crate::event::EventFilterRule;
use crate::event::EventType;
use crate::event::ShouldProcessFn;
use crate::event::WillProcessFn;
use crate::game_state::EventQueueTrait;

/// The `GameRuleType` enum.
#[derive(Clone, Copy, Debug, Deserialize, Display, Eq, Hash, PartialEq, PartialOrd, Serialize)]
pub enum Type {
  ShowRoomDescriptionWhenPlayerAppearsInRoom,
  ShowRoomDescriptionWhenPlayerEntersRoom,
}

impl Type {
  pub fn iterator() -> impl Iterator<Item = Type> {
    use Type::*;
    [
      ShowRoomDescriptionWhenPlayerAppearsInRoom,
      ShowRoomDescriptionWhenPlayerEntersRoom,
    ]
    .iter()
    .copied()
  }

  /// Get the priority.
  pub fn get_priority(&self) -> i64 {
    use Type::*;
    match self {
      ShowRoomDescriptionWhenPlayerAppearsInRoom => 0,
      ShowRoomDescriptionWhenPlayerEntersRoom => 0,
    }
  }

  /// Get the event type.
  pub fn get_event_type(&self) -> EventType {
    use Type::*;
    match self {
      ShowRoomDescriptionWhenPlayerAppearsInRoom => {
        EventType::EntityAppearsInRoom(EntityId::default(), RoomId::default())
      },
      ShowRoomDescriptionWhenPlayerEntersRoom => {
        EventType::EntityWalksFromRoomToRoom(EntityId::default(), RoomId::default(), RoomId::default())
      },
    }
  }

  /// Get the filter rule.
  pub fn get_filter_rule(&self) -> EventFilterRule {
    use Type::*;
    match self {
      ShowRoomDescriptionWhenPlayerAppearsInRoom => EventFilterRule::Always,
      ShowRoomDescriptionWhenPlayerEntersRoom => EventFilterRule::Always,
    }
  }

  /// Get the should process function.
  pub fn get_should_process(&self) -> ShouldProcessFn {
    use Type::*;
    match self {
      ShowRoomDescriptionWhenPlayerAppearsInRoom => Arc::new(|_event, _game_state| None),
      ShowRoomDescriptionWhenPlayerEntersRoom => Arc::new(|_event, _game_state| None),
    }
  }

  /// Get the will process function.
  pub fn get_will_process(&self) -> WillProcessFn {
    use Type::*;
    match self {
      ShowRoomDescriptionWhenPlayerAppearsInRoom => Arc::new(|_event, _game_state| {}),
      ShowRoomDescriptionWhenPlayerEntersRoom => Arc::new(|_event, _game_state| {}),
    }
  }

  /// Get the did process function.
  pub fn get_did_process(&self) -> DidProcessFn {
    use Type::*;
    match self {
      ShowRoomDescriptionWhenPlayerAppearsInRoom => Arc::new(|_event, game_state| {
        if let EventType::EntityAppearsInRoom(_entity_id, room_id) = &_event.r#type {
          let event = EventBuilder::new()
            .priority(0)
            .r#type(EventType::ShowsRoomDescription(room_id.clone()))
            .build();
          game_state.enqueue_event(event);
        }
      }),
      ShowRoomDescriptionWhenPlayerEntersRoom => Arc::new(|_event, game_state| {
        if let EventType::EntityWalksFromRoomToRoom(_entity_id, _start_room_id, end_room_id) = &_event.r#type {
          let event = EventBuilder::new()
            .priority(0)
            .r#type(EventType::ShowsRoomDescription(end_room_id.clone()))
            .build();
          game_state.enqueue_event(event);
        }
      }),
    }
  }

  /// Whether this is _by default_ enabled or not.
  pub fn is_enabled(&self) -> bool {
    true
  }
}

use colored::*;
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
  ShowRoomSummaryWhenPlayerAppearsInRoom,
  ShowRoomSummaryWhenPlayerEntersRoom,
  StyleRoomNameWhenPartOfRoomSummary,
  StyleRoomDescriptionWhenPartOfRoomSummary,
  StyleRoomPassagesWhenPartOfRoomSummary,
}

impl Type {
  pub fn iterator() -> impl Iterator<Item = Type> {
    use Type::*;
    [
      ShowRoomSummaryWhenPlayerAppearsInRoom,
      ShowRoomSummaryWhenPlayerEntersRoom,
      StyleRoomNameWhenPartOfRoomSummary,
      StyleRoomDescriptionWhenPartOfRoomSummary,
      StyleRoomPassagesWhenPartOfRoomSummary,
    ]
    .iter()
    .copied()
  }

  /// Get the priority.
  pub fn get_priority(&self) -> i64 {
    use Type::*;
    match self {
      ShowRoomSummaryWhenPlayerAppearsInRoom => 1,
      ShowRoomSummaryWhenPlayerEntersRoom => 1,
      StyleRoomNameWhenPartOfRoomSummary => 0,
      StyleRoomDescriptionWhenPartOfRoomSummary => 0,
      StyleRoomPassagesWhenPartOfRoomSummary => 0,
    }
  }

  /// Get the event type.
  pub fn get_event_type(&self) -> EventType {
    use Type::*;
    match self {
      ShowRoomSummaryWhenPlayerAppearsInRoom => EventType::EntityAppearsInRoom(EntityId::default(), RoomId::default()),
      ShowRoomSummaryWhenPlayerEntersRoom => {
        EventType::EntityWalksFromRoomToRoom(EntityId::default(), RoomId::default(), RoomId::default())
      },
      StyleRoomNameWhenPartOfRoomSummary => EventType::ShowsRoomNameAsPartOfRoomSummary(String::default()),
      StyleRoomDescriptionWhenPartOfRoomSummary => {
        EventType::ShowsRoomDescriptionAsPartOfRoomSummary(String::default())
      },
      StyleRoomPassagesWhenPartOfRoomSummary => EventType::ShowsRoomPassagesAsPartOfRoomSummary(String::default()),
    }
  }

  /// Get the filter rule.
  pub fn get_filter_rule(&self) -> EventFilterRule {
    use Type::*;
    match self {
      ShowRoomSummaryWhenPlayerAppearsInRoom => EventFilterRule::Always,
      ShowRoomSummaryWhenPlayerEntersRoom => EventFilterRule::Always,
      StyleRoomNameWhenPartOfRoomSummary => EventFilterRule::Always,
      StyleRoomDescriptionWhenPartOfRoomSummary => EventFilterRule::Always,
      StyleRoomPassagesWhenPartOfRoomSummary => EventFilterRule::Always,
    }
  }

  /// Get the should process function.
  pub fn get_should_process(&self) -> ShouldProcessFn {
    use Type::*;
    match self {
      ShowRoomSummaryWhenPlayerAppearsInRoom => Arc::new(|_event, _game_state| None),
      ShowRoomSummaryWhenPlayerEntersRoom => Arc::new(|_event, _game_state| None),
      StyleRoomNameWhenPartOfRoomSummary => Arc::new(|_event, _game_state| None),
      StyleRoomDescriptionWhenPartOfRoomSummary => Arc::new(|_event, _game_state| None),
      StyleRoomPassagesWhenPartOfRoomSummary => Arc::new(|_event, _game_state| None),
    }
  }

  /// Get the will process function.
  pub fn get_will_process(&self) -> WillProcessFn {
    use Type::*;
    match self {
      ShowRoomSummaryWhenPlayerAppearsInRoom => Arc::new(|_event, _game_state| {}),
      ShowRoomSummaryWhenPlayerEntersRoom => Arc::new(|_event, _game_state| {}),
      StyleRoomNameWhenPartOfRoomSummary => Arc::new(|_event, _game_state| {
        if let EventType::ShowsRoomNameAsPartOfRoomSummary(ref mut room_name) = &mut _event.r#type {
          *room_name = format!("{}", room_name.bold());
        }
      }),
      StyleRoomDescriptionWhenPartOfRoomSummary => Arc::new(|_event, _game_state| {
        if let EventType::ShowsRoomDescriptionAsPartOfRoomSummary(ref mut room_description) = &mut _event.r#type {
          *room_description = format!("{}", room_description.italic());
        }
      }),
      StyleRoomPassagesWhenPartOfRoomSummary => Arc::new(|_event, _game_state| {
        if let EventType::ShowsRoomPassagesAsPartOfRoomSummary(ref mut room_passages) = &mut _event.r#type {
          *room_passages = format!("{}", room_passages.cyan());
        }
      }),
    }
  }

  /// Get the did process function.
  pub fn get_did_process(&self) -> DidProcessFn {
    use Type::*;
    match self {
      ShowRoomSummaryWhenPlayerAppearsInRoom => Arc::new(|_event, game_state| {
        if let EventType::EntityAppearsInRoom(_entity_id, room_id) = &_event.r#type {
          let event = EventBuilder::new()
            .priority(0)
            .r#type(EventType::ShowsRoomSummary(room_id.clone()))
            .build();
          game_state.enqueue_event(event);
        }
      }),
      ShowRoomSummaryWhenPlayerEntersRoom => Arc::new(|_event, game_state| {
        if let EventType::EntityWalksFromRoomToRoom(_entity_id, _start_room_id, end_room_id) = &_event.r#type {
          let event = EventBuilder::new()
            .priority(0)
            .r#type(EventType::ShowsRoomSummary(end_room_id.clone()))
            .build();
          game_state.enqueue_event(event);
        }
      }),
      StyleRoomNameWhenPartOfRoomSummary => Arc::new(|_event, _game_state| {}),
      StyleRoomDescriptionWhenPartOfRoomSummary => Arc::new(|_event, _game_state| {}),
      StyleRoomPassagesWhenPartOfRoomSummary => Arc::new(|_event, _game_state| {}),
    }
  }

  /// Whether this is _by default_ enabled or not.
  pub fn is_enabled(&self) -> bool {
    true
  }
}

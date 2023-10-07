use colored::*;
use std::sync::Arc;

use crate::entity_id::ActorId;
use crate::entity_id::RoomId;
use crate::event::DidProcessFn;
use crate::event::EventBuilder;
use crate::event::EventFilterRule;
use crate::event::EventType;
use crate::event::ShouldProcessFn;
use crate::event::WillProcessFn;
use crate::game_state::EventQueueTrait;

/// The `GameRuleType` enum.
///
/// These should be phrased as directives or conditional statements.
#[derive(Clone, Copy, Debug, Deserialize, Display, Eq, Hash, PartialEq, PartialOrd, Serialize)]
pub enum Type {
  ShowRoomSummaryWhenPlayerAppearsInRoom,
  ShowRoomSummaryWhenPlayerEntersRoom,
  StyleRoomNameWhenPartOfRoomSummary,
  StyleRoomDescriptionWhenPartOfRoomSummary,
  StyleRoomPassagesWhenPartOfRoomSummary,
  OutputBlankLineAfterRoomSummary,
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
      OutputBlankLineAfterRoomSummary,
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
      OutputBlankLineAfterRoomSummary => -1,
    }
  }

  /// Get the event type.
  pub fn get_event_type(&self) -> EventType {
    use Type::*;
    match self {
      ShowRoomSummaryWhenPlayerAppearsInRoom => EventType::ActorAppearsInRoom(ActorId::default(), RoomId::default()),
      ShowRoomSummaryWhenPlayerEntersRoom => {
        EventType::ActorMovesFromRoomToRoom(ActorId::default(), RoomId::default(), RoomId::default())
      },
      StyleRoomNameWhenPartOfRoomSummary => EventType::ShowsRoomNameAsPartOfRoomSummary(String::default()),
      StyleRoomDescriptionWhenPartOfRoomSummary => {
        EventType::ShowsRoomDescriptionAsPartOfRoomSummary(String::default())
      },
      StyleRoomPassagesWhenPartOfRoomSummary => EventType::ShowsRoomPassagesAsPartOfRoomSummary(String::default()),
      OutputBlankLineAfterRoomSummary => EventType::ShowsRoomPassagesAsPartOfRoomSummary(String::default()),
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
      OutputBlankLineAfterRoomSummary => EventFilterRule::Always,
    }
  }

  /// Get the should process function.
  pub fn get_should_process(&mut self) -> ShouldProcessFn {
    use Type::*;
    match self {
      ShowRoomSummaryWhenPlayerAppearsInRoom => Arc::new(|_event, _game_state| Ok(None)),
      ShowRoomSummaryWhenPlayerEntersRoom => Arc::new(|_event, _game_state| Ok(None)),
      StyleRoomNameWhenPartOfRoomSummary => Arc::new(|_event, _game_state| Ok(None)),
      StyleRoomDescriptionWhenPartOfRoomSummary => Arc::new(|_event, _game_state| Ok(None)),
      StyleRoomPassagesWhenPartOfRoomSummary => Arc::new(|_event, _game_state| Ok(None)),
      OutputBlankLineAfterRoomSummary => Arc::new(|_event, _game_state| Ok(None)),
    }
  }

  /// Get the will process function.
  pub fn get_will_process(&mut self) -> WillProcessFn {
    use Type::*;
    match self {
      ShowRoomSummaryWhenPlayerAppearsInRoom => Arc::new(|_event, _game_state| Ok(())),
      ShowRoomSummaryWhenPlayerEntersRoom => Arc::new(|_event, _game_state| Ok(())),
      StyleRoomNameWhenPartOfRoomSummary => Arc::new(|_event, _game_state| {
        if let EventType::ShowsRoomNameAsPartOfRoomSummary(ref mut room_name) = &mut _event.r#type {
          *room_name = format!("{}", room_name.bold());
        }
        Ok(())
      }),
      StyleRoomDescriptionWhenPartOfRoomSummary => Arc::new(|_event, _game_state| {
        if let EventType::ShowsRoomDescriptionAsPartOfRoomSummary(ref mut room_description) = &mut _event.r#type {
          *room_description = format!("{}", room_description.italic());
        }
        Ok(())
      }),
      StyleRoomPassagesWhenPartOfRoomSummary => Arc::new(|_event, _game_state| {
        if let EventType::ShowsRoomPassagesAsPartOfRoomSummary(ref mut room_passages) = &mut _event.r#type {
          *room_passages = format!("{}", room_passages.cyan());
        }
        Ok(())
      }),
      OutputBlankLineAfterRoomSummary => Arc::new(|_event, _game_state| Ok(())),
    }
  }

  /// Get the did process function.
  pub fn get_did_process(&mut self) -> DidProcessFn {
    use Type::*;
    match self {
      ShowRoomSummaryWhenPlayerAppearsInRoom => Arc::new(|event, game_state| {
        if let EventType::ActorAppearsInRoom(_actor_id, room_id) = &event.r#type {
          let event = EventBuilder::new()
            .priority(0)
            .r#type(EventType::ShowsRoomSummary(room_id.clone()))
            .build();
          game_state.enqueue_event(event);
        }
        Ok(())
      }),
      ShowRoomSummaryWhenPlayerEntersRoom => Arc::new(|event, game_state| {
        if let EventType::ActorMovesFromRoomToRoom(_actor_id, _start_room_id, end_room_id) = &event.r#type {
          let event = EventBuilder::new()
            .priority(0)
            .r#type(EventType::ShowsRoomSummary(end_room_id.clone()))
            .build();
          game_state.enqueue_event(event);
        }
        Ok(())
      }),
      StyleRoomNameWhenPartOfRoomSummary => Arc::new(|_event, _game_state| Ok(())),
      StyleRoomDescriptionWhenPartOfRoomSummary => Arc::new(|_event, _game_state| Ok(())),
      StyleRoomPassagesWhenPartOfRoomSummary => Arc::new(|_event, _game_state| Ok(())),
      OutputBlankLineAfterRoomSummary => Arc::new(|event, game_state| {
        if let EventType::ShowsRoomPassagesAsPartOfRoomSummary(_room_passages) = &event.r#type {
          let event = EventBuilder::new()
            .priority(0)
            .r#type(EventType::OutputsBlankLine)
            .build();
          game_state.enqueue_event(event);
        }
        Ok(())
      }),
    }
  }

  /// Whether this is _by default_ enabled or not.
  pub fn is_enabled(&self) -> bool {
    true
  }
}

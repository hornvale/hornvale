use colored::*;
use std::sync::Arc;

use crate::entity_id::ActorId;
use crate::entity_id::ChunkId;
use crate::entity_id::ChunkPlaneId;
use crate::entity_id::RoomId;
use crate::event::DidProcessFn;
use crate::event::EventFilterRule;
use crate::event::EventType;
use crate::event::ShouldProcessFn;
use crate::event::WillProcessFn;

/// The `LookupRuleType` enum.
///
/// These should be phrased as directives or conditional statements.
#[derive(Clone, Copy, Debug, Deserialize, Display, Eq, Hash, PartialEq, PartialOrd, Serialize)]
pub enum Type {
  WhenActorAppearsInRoom,
  WhenActorMovesFromRoomToRoom,
  WhenChunkIsLoaded,
  WhenChunkIsUnloaded,
  WhenChunkPlaneIsLoaded,
  WhenChunkPlaneIsUnloaded,
}

impl Type {
  pub fn iterator() -> impl Iterator<Item = Type> {
    use Type::*;
    [WhenChunkIsLoaded, WhenActorAppearsInRoom, WhenActorMovesFromRoomToRoom]
      .iter()
      .copied()
  }

  /// Get the priority.
  ///
  /// Lookup Rules should always be executed at about the same time, and
  /// should be executed before any other rules.
  pub fn get_priority(&self) -> i64 {
    i64::MAX
  }

  /// Get the event type.
  pub fn get_event_type(&self) -> EventType {
    use Type::*;
    match self {
      WhenActorAppearsInRoom => EventType::ActorAppearsInRoom(ActorId::default(), RoomId::default()),
      WhenActorMovesFromRoomToRoom => {
        EventType::ActorMovesFromRoomToRoom(ActorId::default(), RoomId::default(), RoomId::default())
      },
      WhenChunkIsLoaded => EventType::ChunkIsLoaded(ChunkId::default()),
      WhenChunkIsUnloaded => EventType::ChunkIsUnloaded(ChunkId::default()),
      WhenChunkPlaneIsLoaded => EventType::ChunkPlaneIsLoaded(ChunkPlaneId::default()),
      WhenChunkPlaneIsUnloaded => EventType::ChunkPlaneIsUnloaded(ChunkPlaneId::default()),
    }
  }

  /// Get the filter rule.
  ///
  /// Lookup Rules should always be executed.
  pub fn get_filter_rule(&self) -> EventFilterRule {
    EventFilterRule::Always
  }

  /// Get the should process function.
  pub fn get_should_process(&mut self) -> ShouldProcessFn {
    use Type::*;
    match self {
      WhenActorAppearsInRoom => Arc::new(|_event, _game_state| Ok(None)),
      WhenActorMovesFromRoomToRoom => Arc::new(|_event, _game_state| Ok(None)),
      WhenChunkIsLoaded => Arc::new(|_event, _game_state| Ok(None)),
      WhenChunkIsUnloaded => Arc::new(|_event, _game_state| Ok(None)),
      WhenChunkPlaneIsLoaded => Arc::new(|_event, _game_state| Ok(None)),
      WhenChunkPlaneIsUnloaded => Arc::new(|_event, _game_state| Ok(None)),
    }
  }

  /// Get the will process function.
  pub fn get_will_process(&mut self) -> WillProcessFn {
    use Type::*;
    match self {
      WhenActorAppearsInRoom => Arc::new(|_event, _game_state| Ok(())),
      WhenActorMovesFromRoomToRoom => Arc::new(|_event, _game_state| Ok(())),
      WhenChunkIsLoaded => Arc::new(|_event, _game_state| Ok(())),
      WhenChunkIsUnloaded => Arc::new(|_event, _game_state| Ok(())),
      WhenChunkPlaneIsLoaded => Arc::new(|_event, _game_state| Ok(())),
      WhenChunkPlaneIsUnloaded => Arc::new(|_event, _game_state| Ok(())),
    }
  }

  /// Get the did process function.
  pub fn get_did_process(&mut self) -> DidProcessFn {
    use Type::*;
    match self {
      WhenActorAppearsInRoom => Arc::new(|event, _game_state| {
        debug!("{}", "WhenActorAppearsInRoom".blue());
        if let EventType::ActorAppearsInRoom(_actor_id, _room_id) = &event.event_type {}
        Ok(())
      }),
      WhenActorMovesFromRoomToRoom => Arc::new(|event, _game_state| {
        debug!("{}", "WhenActorMovesFromRoomToRoom".blue());
        if let EventType::ActorMovesFromRoomToRoom(_actor_id, _start_room_id, _end_room_id) = &event.event_type {}
        Ok(())
      }),
      WhenChunkIsLoaded => Arc::new(|event, _game_state| {
        debug!("{}", "WhenChunkIsLoaded".blue());
        if let EventType::ChunkIsLoaded(_chunk_id) = &event.event_type {}
        Ok(())
      }),
      WhenChunkIsUnloaded => Arc::new(|event, _game_state| {
        debug!("{}", "WhenChunkIsUnloaded".blue());
        if let EventType::ChunkIsUnloaded(_chunk_id) = &event.event_type {}
        Ok(())
      }),
      WhenChunkPlaneIsLoaded => Arc::new(|event, _game_state| {
        debug!("{}", "WhenChunkPlaneIsLoaded".blue());
        if let EventType::ChunkPlaneIsLoaded(_chunk_plane_id) = &event.event_type {}
        Ok(())
      }),
      WhenChunkPlaneIsUnloaded => Arc::new(|event, _game_state| {
        debug!("{}", "WhenChunkPlaneIsUnloaded".blue());
        if let EventType::ChunkPlaneIsUnloaded(_chunk_plane_id) = &event.event_type {}
        Ok(())
      }),
    }
  }

  /// Whether this is _by default_ enabled or not.
  pub fn is_enabled(&self) -> bool {
    true
  }
}

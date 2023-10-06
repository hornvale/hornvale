use colored::*;
use std::sync::Arc;

use crate::entity_id::ChunkId;
use crate::event::DidProcessFn;
use crate::event::EventFilterRule;
use crate::event::EventType;
use crate::event::ShouldProcessFn;
use crate::event::WillProcessFn;
use crate::game_state::FileManagerTrait;

/// The `ChunkRuleType` enum.
///
/// These should be phrased as directives or conditional statements.
#[derive(Clone, Copy, Debug, Deserialize, Display, Eq, Hash, PartialEq, PartialOrd, Serialize)]
pub enum Type {
  CreateChunkPlaneWhenGameStarts,
  MapEmptyChunksWhenGameStarts,
  MovePlayerToRoomWhenGameStarts,
  OutputDebugMessageWhenCrossingChunkBoundary,
}

impl Type {
  pub fn iterator() -> impl Iterator<Item = Type> {
    use Type::*;
    [
      CreateChunkPlaneWhenGameStarts,
      MapEmptyChunksWhenGameStarts,
      MovePlayerToRoomWhenGameStarts,
      OutputDebugMessageWhenCrossingChunkBoundary,
    ]
    .iter()
    .copied()
  }

  /// Get the priority.
  pub fn get_priority(&self) -> i64 {
    use Type::*;
    match self {
      CreateChunkPlaneWhenGameStarts => 100,
      MapEmptyChunksWhenGameStarts => 50,
      MovePlayerToRoomWhenGameStarts => 25,
      OutputDebugMessageWhenCrossingChunkBoundary => 0,
    }
  }

  /// Get the event type.
  pub fn get_event_type(&self) -> EventType {
    use Type::*;
    match self {
      CreateChunkPlaneWhenGameStarts => EventType::StartsGame,
      MapEmptyChunksWhenGameStarts => EventType::StartsGame,
      MovePlayerToRoomWhenGameStarts => EventType::StartsGame,
      OutputDebugMessageWhenCrossingChunkBoundary => {
        EventType::PlayerCrossesChunkBoundary(ChunkId::default(), ChunkId::default())
      },
    }
  }

  /// Get the filter rule.
  pub fn get_filter_rule(&self) -> EventFilterRule {
    use Type::*;
    match self {
      CreateChunkPlaneWhenGameStarts => EventFilterRule::Always,
      MapEmptyChunksWhenGameStarts => EventFilterRule::Always,
      MovePlayerToRoomWhenGameStarts => EventFilterRule::Always,
      OutputDebugMessageWhenCrossingChunkBoundary => EventFilterRule::Always,
    }
  }

  /// Get the should process function.
  pub fn get_should_process(&mut self) -> ShouldProcessFn {
    use Type::*;
    match self {
      CreateChunkPlaneWhenGameStarts => Arc::new(|_event, _game_state| None),
      MapEmptyChunksWhenGameStarts => Arc::new(|_event, _game_state| None),
      MovePlayerToRoomWhenGameStarts => Arc::new(|_event, _game_state| None),
      OutputDebugMessageWhenCrossingChunkBoundary => Arc::new(|_event, _game_state| None),
    }
  }

  /// Get the will process function.
  pub fn get_will_process(&mut self) -> WillProcessFn {
    use Type::*;
    match self {
      CreateChunkPlaneWhenGameStarts => Arc::new(|_event, _game_state| {}),
      MapEmptyChunksWhenGameStarts => Arc::new(|_event, _game_state| {}),
      MovePlayerToRoomWhenGameStarts => Arc::new(|_event, _game_state| {}),
      OutputDebugMessageWhenCrossingChunkBoundary => Arc::new(|_event, _game_state| {}),
    }
  }

  /// Get the did process function.
  pub fn get_did_process(&mut self) -> DidProcessFn {
    use Type::*;
    match self {
      CreateChunkPlaneWhenGameStarts => Arc::new(|_event, game_state| {
        if let EventType::StartsGame = &_event.r#type {
          debug!("Clearing game data directory (TEMPORARY).");
          let local_data_dir = game_state.local_data_dir.clone();
          game_state.clear_directory(&local_data_dir).unwrap();
          debug!("Creating chunk plane.");
          if let Ok(mut chunk_plane) = game_state.chunk_manager.create_chunk_plane() {
            debug!("Storing chunk plane.");
            game_state.chunk_manager.store_chunk_plane(&chunk_plane).unwrap();
            debug!("Generating initial chunks.");
            let chunks = game_state
              .chunk_manager
              .generate_initial_chunks(&mut chunk_plane)
              .unwrap();
            debug!("Storing chunks.");
            game_state.chunk_manager.store_chunks(&chunks).unwrap();
            debug!("Setting chunk plane ID.");
            game_state.chunk_plane_id = Some(chunk_plane.id.clone());
          }
        }
      }),
      MapEmptyChunksWhenGameStarts => Arc::new(|_event, game_state| {
        if let EventType::StartsGame = &_event.r#type {
          let chunk_plane_id = game_state.chunk_plane_id.clone().unwrap();
          debug!("Mapping closed chunks.");
          let mut chunk_plane = game_state.chunk_manager.load_chunk_plane(&chunk_plane_id).unwrap();
          game_state.chunk_manager.map_empty_chunks(&mut chunk_plane).unwrap();
          debug!("Selecting a chunk.");
          let chunks = game_state.chunk_manager.load_chunks(&chunk_plane).unwrap();
          let chunk = chunks.first().unwrap();
          debug!("Setting current chunk.");
          game_state.chunk_id = Some(chunk.id.clone());
        }
      }),
      MovePlayerToRoomWhenGameStarts => Arc::new(|_event, game_state| {
        if let EventType::StartsGame = &_event.r#type {
          let chunk_id = game_state.chunk_id.clone().unwrap();
          debug!("Moving player to room.");
          let chunk = game_state.chunk_manager.load_chunk(&chunk_id).unwrap();
          let room = chunk.rooms.values().next().unwrap();
          game_state.current_room_id = Some(room.id.clone());
        }
      }),
      OutputDebugMessageWhenCrossingChunkBoundary => Arc::new(|_event, _game_state| {
        if let EventType::PlayerCrossesChunkBoundary(_from_chunk, _to_chunk) = &_event.r#type {
          debug!(
            "{} from {} to {}",
            "Player crosses chunk boundary".green(),
            _from_chunk,
            _to_chunk
          );
        }
      }),
    }
  }

  /// Whether this is _by default_ enabled or not.
  pub fn is_enabled(&self) -> bool {
    true
  }
}

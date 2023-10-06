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
  OutputDebugMessageWhenCrossingChunkBoundary,
  PopulateClosedChunksWhenGameStarts,
}

impl Type {
  pub fn iterator() -> impl Iterator<Item = Type> {
    use Type::*;
    [
      CreateChunkPlaneWhenGameStarts,
      OutputDebugMessageWhenCrossingChunkBoundary,
      PopulateClosedChunksWhenGameStarts,
    ]
    .iter()
    .copied()
  }

  /// Get the priority.
  pub fn get_priority(&self) -> i64 {
    use Type::*;
    match self {
      CreateChunkPlaneWhenGameStarts => 100,
      OutputDebugMessageWhenCrossingChunkBoundary => 0,
      PopulateClosedChunksWhenGameStarts => 50,
    }
  }

  /// Get the event type.
  pub fn get_event_type(&self) -> EventType {
    use Type::*;
    match self {
      CreateChunkPlaneWhenGameStarts => EventType::StartsGame,
      OutputDebugMessageWhenCrossingChunkBoundary => {
        EventType::PlayerCrossesChunkBoundary(ChunkId::default(), ChunkId::default())
      },
      PopulateClosedChunksWhenGameStarts => EventType::StartsGame,
    }
  }

  /// Get the filter rule.
  pub fn get_filter_rule(&self) -> EventFilterRule {
    use Type::*;
    match self {
      CreateChunkPlaneWhenGameStarts => EventFilterRule::Always,
      OutputDebugMessageWhenCrossingChunkBoundary => EventFilterRule::Always,
      PopulateClosedChunksWhenGameStarts => EventFilterRule::Always,
    }
  }

  /// Get the should process function.
  pub fn get_should_process(&mut self) -> ShouldProcessFn {
    use Type::*;
    match self {
      CreateChunkPlaneWhenGameStarts => Arc::new(|_event, _game_state| None),
      OutputDebugMessageWhenCrossingChunkBoundary => Arc::new(|_event, _game_state| None),
      PopulateClosedChunksWhenGameStarts => Arc::new(|_event, _game_state| None),
    }
  }

  /// Get the will process function.
  pub fn get_will_process(&mut self) -> WillProcessFn {
    use Type::*;
    match self {
      CreateChunkPlaneWhenGameStarts => Arc::new(|_event, _game_state| {}),
      OutputDebugMessageWhenCrossingChunkBoundary => Arc::new(|_event, _game_state| {}),
      PopulateClosedChunksWhenGameStarts => Arc::new(|_event, _game_state| {}),
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
          game_state.clear_directory(&local_data_dir).ok();
          debug!("Creating chunk plane.");
          if let Ok(mut chunk_plane) = game_state.chunk_manager.create_chunk_plane() {
            debug!("Storing chunk plane.");
            game_state.chunk_manager.store_chunk_plane(&chunk_plane).ok();
            debug!("Generating initial chunks.");
            let chunks = game_state
              .chunk_manager
              .generate_initial_chunks(&mut chunk_plane)
              .unwrap();
            debug!("Storing chunks.");
            game_state.chunk_manager.store_chunks(&chunks).ok();
          }
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
      PopulateClosedChunksWhenGameStarts => Arc::new(|_event, _game_state| {
        if let EventType::StartsGame = &_event.r#type {
          debug!("Populating closed chunks.");
        }
      }),
    }
  }

  /// Whether this is _by default_ enabled or not.
  pub fn is_enabled(&self) -> bool {
    true
  }
}

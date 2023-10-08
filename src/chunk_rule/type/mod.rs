use anyhow::Context;
use colored::*;
use std::sync::Arc;

use crate::entity_id::ChunkId;
use crate::entity_id::ChunkPlaneId;
use crate::event::DidProcessFn;
use crate::event::EventFilterRule;
use crate::event::EventType;
use crate::event::ShouldProcessFn;
use crate::event::WillProcessFn;
use crate::game_state::ChunkCreatorServiceTrait;
use crate::game_state::ChunkFileServiceTrait;
use crate::game_state::FileManagerTrait;

/// The `ChunkRuleType` enum.
///
/// These should be phrased as directives or conditional statements.
#[derive(Clone, Copy, Debug, Deserialize, Display, Eq, Hash, PartialEq, PartialOrd, Serialize)]
pub enum Type {
  CreateChunkPlaneWhenGameStarts,
  MapEmptyChunksWhenChunkPlaneIsLoaded,
  MovePlayerToRoomWhenGameStarts,
  OutputDebugMessageWhenCrossingChunkBoundary,
}

impl Type {
  pub fn iterator() -> impl Iterator<Item = Type> {
    use Type::*;
    [
      CreateChunkPlaneWhenGameStarts,
      MapEmptyChunksWhenChunkPlaneIsLoaded,
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
      MapEmptyChunksWhenChunkPlaneIsLoaded => i64::MAX,
      MovePlayerToRoomWhenGameStarts => 25,
      OutputDebugMessageWhenCrossingChunkBoundary => 0,
    }
  }

  /// Get the event type.
  pub fn get_event_type(&self) -> EventType {
    use Type::*;
    match self {
      CreateChunkPlaneWhenGameStarts => EventType::StartsGame,
      MapEmptyChunksWhenChunkPlaneIsLoaded => EventType::ChunkPlaneIsLoaded(ChunkPlaneId::default()),
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
      MapEmptyChunksWhenChunkPlaneIsLoaded => EventFilterRule::Always,
      MovePlayerToRoomWhenGameStarts => EventFilterRule::Always,
      OutputDebugMessageWhenCrossingChunkBoundary => EventFilterRule::Always,
    }
  }

  /// Get the should process function.
  pub fn get_should_process(&mut self) -> ShouldProcessFn {
    use Type::*;
    match self {
      CreateChunkPlaneWhenGameStarts => Arc::new(|_event, _game_state| Ok(None)),
      MapEmptyChunksWhenChunkPlaneIsLoaded => Arc::new(|_event, _game_state| Ok(None)),
      MovePlayerToRoomWhenGameStarts => Arc::new(|_event, _game_state| Ok(None)),
      OutputDebugMessageWhenCrossingChunkBoundary => Arc::new(|_event, _game_state| Ok(None)),
    }
  }

  /// Get the will process function.
  pub fn get_will_process(&mut self) -> WillProcessFn {
    use Type::*;
    match self {
      CreateChunkPlaneWhenGameStarts => Arc::new(|_event, _game_state| Ok(())),
      MapEmptyChunksWhenChunkPlaneIsLoaded => Arc::new(|_event, _game_state| Ok(())),
      MovePlayerToRoomWhenGameStarts => Arc::new(|_event, _game_state| Ok(())),
      OutputDebugMessageWhenCrossingChunkBoundary => Arc::new(|_event, _game_state| Ok(())),
    }
  }

  /// Get the did process function.
  pub fn get_did_process(&mut self) -> DidProcessFn {
    use Type::*;
    match self {
      CreateChunkPlaneWhenGameStarts => Arc::new(|event, game_state| {
        debug!("CreateChunkPlaneWhenGameStarts");
        if let EventType::StartsGame = &event.r#type {
          debug!("Clearing game data directory (TEMPORARY).");
          let local_data_dir = game_state.local_data_dir.clone();
          game_state.clear_directory(&local_data_dir)?;
          debug!("Creating chunk plane.");
          if let Ok(mut chunk_plane) = game_state.create_chunk_plane() {
            debug!("Generating initial chunks.");
            let chunks = game_state.generate_initial_chunks(&mut chunk_plane).unwrap();
            debug!("Storing chunks.");
            game_state.save_chunks(&chunks)?;
            debug!("Reloading chunk plane.");
            game_state.open_chunk_plane(&chunk_plane.id)?;
          }
        }
        Ok(())
      }),
      MapEmptyChunksWhenChunkPlaneIsLoaded => Arc::new(|event, game_state| {
        debug!("MapEmptyChunksWhenChunkPlaneIsLoaded");
        if let EventType::ChunkPlaneIsLoaded(chunk_plane_id) = &event.r#type {
          debug!("Mapping closed chunks.");
          let mut chunk_plane = game_state.open_chunk_plane(chunk_plane_id).with_context(|| {
            format!(
              "Could not find chunk plane with id {} in chunk manager.",
              chunk_plane_id.to_string().red()
            )
          })?;
          game_state.map_empty_chunks(&mut chunk_plane).with_context(|| {
            format!(
              "Could not map empty chunks for chunk plane with id {} in chunk manager.",
              chunk_plane_id.to_string().red()
            )
          })?;
        }
        Ok(())
      }),
      MovePlayerToRoomWhenGameStarts => Arc::new(|_event, _game_state| {
        debug!("MovePlayerToRoomWhenGameStarts");
        if let EventType::StartsGame = &_event.r#type {
          debug!("Selecting an arbitrary startable chunk.");
          /*
          let chunk = game_state
            .get_arbitrary_startable_chunk()
            .with_context(|| "Could not get arbitrary startable chunk.")?;
          game_state.insert_rooms_from_chunk(chunk);
          if chunk.starting_room_id.is_some() {
            game_state.set_current_room_id(chunk.starting_room_id.clone());
          } else {
            let room = chunk.rooms.values().next().unwrap();
            game_state.set_current_room_id(Some(room.id.clone()));
          }
          let entity_appears_in_room = Event::new(
            EventType::ActorAppearsInRoom(
              game_state.get_player_id().clone().into(),
              game_state.get_current_room_id().unwrap(),
            ),
            DEFAULT_PRIORITY + 100,
            Vec::new(),
            Vec::new(),
          );
          game_state.enqueue_event(entity_appears_in_room);
          */
        }
        Ok(())
      }),
      OutputDebugMessageWhenCrossingChunkBoundary => Arc::new(|_event, _game_state| {
        debug!("OutputDebugMessageWhenCrossingChunkBoundary");
        if let EventType::PlayerCrossesChunkBoundary(_from_chunk, _to_chunk) = &_event.r#type {
          debug!(
            "{} from {} to {}",
            "Player crosses chunk boundary".green(),
            _from_chunk,
            _to_chunk
          );
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

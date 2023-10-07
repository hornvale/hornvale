use anyhow::Context;
use colored::*;
use std::sync::Arc;

use crate::entity_id::ChunkId;
use crate::entity_id::ChunkPlaneId;
use crate::event::DidProcessFn;
use crate::event::Event;
use crate::event::EventFilterRule;
use crate::event::EventType;
use crate::event::ShouldProcessFn;
use crate::event::WillProcessFn;
use crate::event::DEFAULT_PRIORITY;
use crate::game_state::ChunkManagementTrait;
use crate::game_state::CurrentRoomIdTrait;
use crate::game_state::EventQueueTrait;
use crate::game_state::FileManagerTrait;
use crate::game_state::PlayerIdTrait;
use crate::game_state::RoomsTrait;

/// The `ChunkRuleType` enum.
///
/// These should be phrased as directives or conditional statements.
#[derive(Clone, Copy, Debug, Deserialize, Display, Eq, Hash, PartialEq, PartialOrd, Serialize)]
pub enum Type {
  AddChunkRoomsToRoom2ChunkLookupWhenChunkIsLoaded,
  AddChunkToChunk2ChunkPlaneLookupWhenChunkIsLoaded,
  AddChunkToLoadedChunksWhenChunkIsLoaded,
  AddPlaneToChunkPlane2ChunkLookupWhenChunkPlaneIsLoaded,
  CreateChunkPlaneWhenGameStarts,
  MapEmptyChunksWhenChunkPlaneIsLoaded,
  MovePlayerToRoomWhenGameStarts,
  OutputDebugMessageWhenCrossingChunkBoundary,
}

impl Type {
  pub fn iterator() -> impl Iterator<Item = Type> {
    use Type::*;
    [
      AddChunkRoomsToRoom2ChunkLookupWhenChunkIsLoaded,
      AddChunkToChunk2ChunkPlaneLookupWhenChunkIsLoaded,
      AddChunkToLoadedChunksWhenChunkIsLoaded,
      AddPlaneToChunkPlane2ChunkLookupWhenChunkPlaneIsLoaded,
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
      AddChunkRoomsToRoom2ChunkLookupWhenChunkIsLoaded => i64::MAX,
      AddChunkToChunk2ChunkPlaneLookupWhenChunkIsLoaded => i64::MAX - 1,
      AddChunkToLoadedChunksWhenChunkIsLoaded => i64::MAX - 2,
      AddPlaneToChunkPlane2ChunkLookupWhenChunkPlaneIsLoaded => i64::MAX - 3,
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
      AddChunkRoomsToRoom2ChunkLookupWhenChunkIsLoaded => EventType::ChunkIsLoaded(ChunkId::default()),
      AddChunkToChunk2ChunkPlaneLookupWhenChunkIsLoaded => EventType::ChunkIsLoaded(ChunkId::default()),
      AddChunkToLoadedChunksWhenChunkIsLoaded => EventType::ChunkIsLoaded(ChunkId::default()),
      AddPlaneToChunkPlane2ChunkLookupWhenChunkPlaneIsLoaded => EventType::ChunkPlaneIsLoaded(ChunkPlaneId::default()),
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
      AddChunkRoomsToRoom2ChunkLookupWhenChunkIsLoaded => EventFilterRule::Always,
      AddChunkToChunk2ChunkPlaneLookupWhenChunkIsLoaded => EventFilterRule::Always,
      AddChunkToLoadedChunksWhenChunkIsLoaded => EventFilterRule::Always,
      AddPlaneToChunkPlane2ChunkLookupWhenChunkPlaneIsLoaded => EventFilterRule::Always,
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
      AddChunkRoomsToRoom2ChunkLookupWhenChunkIsLoaded => Arc::new(|_event, _game_state| Ok(None)),
      AddChunkToChunk2ChunkPlaneLookupWhenChunkIsLoaded => Arc::new(|_event, _game_state| Ok(None)),
      AddChunkToLoadedChunksWhenChunkIsLoaded => Arc::new(|_event, _game_state| Ok(None)),
      AddPlaneToChunkPlane2ChunkLookupWhenChunkPlaneIsLoaded => Arc::new(|_event, _game_state| Ok(None)),
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
      AddChunkRoomsToRoom2ChunkLookupWhenChunkIsLoaded => Arc::new(|_event, _game_state| Ok(())),
      AddChunkToChunk2ChunkPlaneLookupWhenChunkIsLoaded => Arc::new(|_event, _game_state| Ok(())),
      AddChunkToLoadedChunksWhenChunkIsLoaded => Arc::new(|_event, _game_state| Ok(())),
      AddPlaneToChunkPlane2ChunkLookupWhenChunkPlaneIsLoaded => Arc::new(|_event, _game_state| Ok(())),
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
      AddChunkRoomsToRoom2ChunkLookupWhenChunkIsLoaded => Arc::new(|_event, game_state| {
        debug!("AddChunkRoomsToRoom2ChunkLookupWhenChunkIsLoaded");
        if let EventType::ChunkIsLoaded(chunk_id) = &_event.r#type {
          debug!("Adding chunk rooms to room2chunk lookup.");
          let chunk = game_state.loaded_chunks.get(chunk_id).with_context(|| {
            format!(
              "Could not find chunk with id {} in loaded chunks.",
              chunk_id.to_string().red()
            )
          })?;
          chunk.rooms.iter().for_each(|(room_id, _room)| {
            game_state.room_id_to_chunk_id.insert(room_id.clone(), chunk.id.clone());
          });
        }
        Ok(())
      }),
      AddChunkToChunk2ChunkPlaneLookupWhenChunkIsLoaded => Arc::new(|event, game_state| {
        debug!("AddChunkToChunk2ChunkPlaneLookupWhenChunkIsLoaded");
        if let EventType::ChunkIsLoaded(chunk_id) = &event.r#type {
          debug!("Adding chunk to chunk2chunkplane lookup.");
          let chunk = game_state.chunk_manager.load_chunk(chunk_id).with_context(|| {
            format!(
              "Could not find chunk with id {} in chunk manager.",
              chunk_id.to_string().red()
            )
          })?;
          game_state
            .chunk_id_to_chunk_plane_id
            .insert(chunk_id.clone(), chunk.chunk_plane_id);
        }
        Ok(())
      }),
      AddChunkToLoadedChunksWhenChunkIsLoaded => Arc::new(|event, game_state| {
        debug!("AddChunkToLoadedChunksWhenChunkIsLoaded");
        if let EventType::ChunkIsLoaded(chunk_id) = &event.r#type {
          debug!("Adding chunk to loaded chunks.");
          let chunk = game_state.chunk_manager.load_chunk(chunk_id).with_context(|| {
            format!(
              "Could not find chunk with id {} in chunk manager.",
              chunk_id.to_string().red()
            )
          })?;
          game_state.loaded_chunks.insert(chunk_id.clone(), chunk);
        }
        Ok(())
      }),
      AddPlaneToChunkPlane2ChunkLookupWhenChunkPlaneIsLoaded => Arc::new(|event, game_state| {
        debug!("AddPlaneToChunkPlane2ChunkLookupWhenChunkPlaneIsLoaded");
        if let EventType::ChunkPlaneIsLoaded(chunk_plane_id) = &event.r#type {
          debug!("Adding chunk plane to chunkplane2chunk lookup.");
          let chunk_plane = game_state
            .chunk_manager
            .load_chunk_plane(chunk_plane_id)
            .with_context(|| {
              format!(
                "Could not find chunk plane with id {} in chunk manager.",
                chunk_plane_id.to_string().red()
              )
            })?;
          game_state
            .chunk_plane_id_to_chunk_ids
            .insert(chunk_plane_id.clone(), chunk_plane.chunk_ids.iter().cloned().collect());
        }
        Ok(())
      }),
      CreateChunkPlaneWhenGameStarts => Arc::new(|event, game_state| {
        debug!("CreateChunkPlaneWhenGameStarts");
        if let EventType::StartsGame = &event.r#type {
          debug!("Clearing game data directory (TEMPORARY).");
          let local_data_dir = game_state.local_data_dir.clone();
          game_state.clear_directory(&local_data_dir)?;
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
            game_state.chunk_manager.store_chunks(&chunks)?;
            debug!("Reloading chunk plane.");
            game_state.load_chunk_plane(&chunk_plane.id)?;
          }
        }
        Ok(())
      }),
      MapEmptyChunksWhenChunkPlaneIsLoaded => Arc::new(|event, game_state| {
        debug!("MapEmptyChunksWhenChunkPlaneIsLoaded");
        if let EventType::ChunkPlaneIsLoaded(chunk_plane_id) = &event.r#type {
          debug!("Mapping closed chunks.");
          let mut chunk_plane = game_state
            .chunk_manager
            .load_chunk_plane(chunk_plane_id)
            .with_context(|| {
              format!(
                "Could not find chunk plane with id {} in chunk manager.",
                chunk_plane_id.to_string().red()
              )
            })?;
          game_state
            .chunk_manager
            .map_empty_chunks(&mut chunk_plane)
            .with_context(|| {
              format!(
                "Could not map empty chunks for chunk plane with id {} in chunk manager.",
                chunk_plane_id.to_string().red()
              )
            })?;
        }
        Ok(())
      }),
      MovePlayerToRoomWhenGameStarts => Arc::new(|_event, game_state| {
        debug!("MovePlayerToRoomWhenGameStarts");
        if let EventType::StartsGame = &_event.r#type {
          debug!("Selecting a chunk.");
          // Get the first available chunk plane.
          let chunk_plane_id = game_state.loaded_chunk_planes.keys().next().unwrap().clone();
          let mut chunk_plane = game_state.chunk_manager.load_chunk_plane(&chunk_plane_id).unwrap();
          game_state
            .chunk_manager
            .map_empty_chunks(&mut chunk_plane)
            .with_context(|| {
              format!(
                "Could not map empty chunks for chunk plane with id {} in chunk manager.",
                chunk_plane_id.to_string().red()
              )
            })?;
          let chunks = game_state.chunk_manager.load_chunks(&chunk_plane).unwrap();
          // Get the first chunk that is startable.
          let chunk = chunks.iter().find(|c| c.is_startable).unwrap();
          game_state.insert_rooms_from_chunk(chunk);
          if chunk.starting_room_id.is_some() {
            game_state.set_current_room_id(chunk.starting_room_id.clone());
          } else {
            let room = chunk.rooms.values().next().unwrap();
            game_state.set_current_room_id(Some(room.id.clone()));
          }
          let entity_appears_in_room = Event::new(
            EventType::EntityAppearsInRoom(
              game_state.get_player_id().clone().into(),
              game_state.get_current_room_id().unwrap(),
            ),
            DEFAULT_PRIORITY + 100,
            Vec::new(),
            Vec::new(),
          );
          game_state.enqueue_event(entity_appears_in_room);
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

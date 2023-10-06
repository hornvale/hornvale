use colored::*;
use std::sync::Arc;

use crate::entity_id::ChunkId;
use crate::event::DidProcessFn;
use crate::event::EventFilterRule;
use crate::event::EventType;
use crate::event::ShouldProcessFn;
use crate::event::WillProcessFn;

/// The `ChunkRuleType` enum.
///
/// These should be phrased as directives or conditional statements.
#[derive(Clone, Copy, Debug, Deserialize, Display, Eq, Hash, PartialEq, PartialOrd, Serialize)]
pub enum Type {
  OutputDebugMessageWhenCrossingChunkBoundary,
}

impl Type {
  pub fn iterator() -> impl Iterator<Item = Type> {
    use Type::*;
    [OutputDebugMessageWhenCrossingChunkBoundary].iter().copied()
  }

  /// Get the priority.
  pub fn get_priority(&self) -> i64 {
    use Type::*;
    match self {
      OutputDebugMessageWhenCrossingChunkBoundary => 0,
    }
  }

  /// Get the event type.
  pub fn get_event_type(&self) -> EventType {
    use Type::*;
    match self {
      OutputDebugMessageWhenCrossingChunkBoundary => {
        EventType::PlayerCrossesChunkBoundary(ChunkId::default(), ChunkId::default())
      },
    }
  }

  /// Get the filter rule.
  pub fn get_filter_rule(&self) -> EventFilterRule {
    use Type::*;
    match self {
      OutputDebugMessageWhenCrossingChunkBoundary => EventFilterRule::Always,
    }
  }

  /// Get the should process function.
  pub fn get_should_process(&mut self) -> ShouldProcessFn {
    use Type::*;
    match self {
      OutputDebugMessageWhenCrossingChunkBoundary => Arc::new(|_event, _game_state| None),
    }
  }

  /// Get the will process function.
  pub fn get_will_process(&mut self) -> WillProcessFn {
    use Type::*;
    match self {
      OutputDebugMessageWhenCrossingChunkBoundary => Arc::new(|_event, _game_state| {}),
    }
  }

  /// Get the did process function.
  pub fn get_did_process(&mut self) -> DidProcessFn {
    use Type::*;
    match self {
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

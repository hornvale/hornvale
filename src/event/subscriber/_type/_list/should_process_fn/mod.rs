use anyhow::Error as AnyError;

use std::sync::Arc;

use crate::event::Event;
use crate::game_state::GameState;

/// The closure that determines if this subscriber should process an event.
pub type ShouldProcessFn = Arc<dyn Fn(&Event, &GameState) -> Result<Option<bool>, AnyError>>;

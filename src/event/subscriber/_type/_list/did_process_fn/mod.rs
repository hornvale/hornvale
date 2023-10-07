use anyhow::Error as AnyError;
use std::sync::Arc;

use crate::event::Event;
use crate::game_state::GameState;

/// The closure called after an event has been processed.
pub type DidProcessFn = Arc<dyn Fn(&Event, &mut GameState) -> Result<(), AnyError>>;

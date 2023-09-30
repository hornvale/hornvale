use std::sync::Arc;

use crate::event::Event;
use crate::game_state::GameState;

/// The closure that is called when an event is about to be processed.
pub type WillProcessFn = Arc<dyn Fn(&mut Event, &GameState)>;

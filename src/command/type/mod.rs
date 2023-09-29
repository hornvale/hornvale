use anyhow::Error as AnyError;

use crate::command::Command;
use crate::event::Event;
use crate::event::EventType;
use crate::game_state::EventQueueTrait;
use crate::game_state::GameState;

/// The `Type` enum.
///
/// This should be an exhaustive collection of commands.
#[derive(Clone, Debug, Default, Display, PartialEq)]
pub enum Type {
  /// No-Op -- absolutely nothing happens.
  #[default]
  NoOp,
  /// QuitGame -- the player quit.
  QuitGame,
}

impl Type {
  /// Creates a new `Type`.
  pub fn new() -> Self {
    Self::default()
  }

  pub fn execute(&self, command: &Command, game_state: &mut GameState) -> Result<(), AnyError> {
    debug!("Executing {:#?} command.", self);
    use Type::*;
    #[allow(unreachable_patterns)]
    match self {
      NoOp => {
        debug!("Executing no-op command.");
      },
      QuitGame => {
        debug!("Executing quit-game command.");
        let event = Event::new(EventType::QuitGame, command.backtrace.clone());
        game_state.enqueue_event(event);
      },
      _ => {
        // By default, we let subscribers react to the event.
      },
    }
    Ok(())
  }
}

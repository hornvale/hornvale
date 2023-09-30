use anyhow::Error as AnyError;

use crate::action::Action;
use crate::event::Event;
use crate::event::EventType;
use crate::event::DEFAULT_PRIORITY;
use crate::game_state::EventQueueTrait;
use crate::game_state::GameState;

/// The `Type` enum.
///
/// This should be an exhaustive collection of actions.
#[derive(Clone, Debug, Default, Display, PartialEq)]
pub enum Type {
  /// No-Op -- absolutely nothing happens.
  #[default]
  NoOp,
}

impl Type {
  /// Creates a new `Type`.
  pub fn new() -> Self {
    Self::default()
  }

  pub fn attempt(&self, action: &Action, game_state: &mut GameState) -> Result<(), AnyError> {
    debug!("Attempting {:#?} action.", self);
    use Type::*;
    #[allow(unreachable_patterns)]
    match self {
      NoOp => {
        debug!("Attempting no-op action.");
        let event = Event::new(EventType::NoOp, DEFAULT_PRIORITY, action.backtrace.clone());
        game_state.enqueue_event(event);
      },
      _ => unimplemented!(),
    }
    Ok(())
  }
}

#[cfg(test)]
mod tests {

  use super::*;

  use crate::action::Action;
  use crate::game_state::GameState;
  use crate::test::init;

  #[test]
  fn test_new() {
    init();
    let _type = Type::new();
  }

  #[test]
  fn test_attempt() {
    init();
    let mut game_state = GameState::new();
    let action = Action::new(Type::NoOp, vec![]);
    let _ = Type::NoOp.attempt(&action, &mut game_state);
  }
}

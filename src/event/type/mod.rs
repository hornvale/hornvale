use anyhow::Error as AnyError;

use crate::effect::Effect;
use crate::effect::EffectType;
use crate::event::Event;
use crate::game_state::GameState;

/// The `Type` enum.
///
/// This should be an exhaustive collection of events.
#[derive(Debug, Default, Display)]
pub enum Type {
  /// No-Op -- absolutely nothing happens.
  #[default]
  NoOp,
  /// StartedGame -- the game has started.
  StartedGame,
  /// QuitGame -- the player quit.
  QuitGame,
}

impl Type {
  /// Creates a new `Type`.
  pub fn new() -> Self {
    Self::default()
  }

  pub fn process(&self, event: &Event, game_state: &mut GameState) -> Result<(), AnyError> {
    debug!("Processing {} event.", self);
    use Type::*;
    #[allow(unreachable_patterns)]
    match self {
      NoOp => {
        debug!("Applying no-op event.");
      },
      StartedGame => {
        debug!("Applying start-game event.");
      },
      QuitGame => {
        debug!("Applying quit-game event.");
        Effect::new(EffectType::SetQuitFlag(true), event.backtrace.clone()).apply(game_state)?;
      },
      _ => {
        // By default, we let subscribers react to the event.
      },
    }
    Ok(())
  }
}

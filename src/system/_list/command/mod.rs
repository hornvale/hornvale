use crate::effect::Effect;
use crate::effect::EffectType;
use crate::game_state::CommandQueueTrait;
use crate::game_state::GameState;
use crate::system::SystemTrait;

/// The `Command` struct.
///
/// This system dequeues commands from the `GameState` and runs them.
#[derive(Debug, Default)]
pub struct Command {}

impl Command {
  /// Creates a new `Command`.
  pub fn new() -> Self {
    Self {}
  }
}

impl SystemTrait<GameState> for Command {
  /// Runs the `Command`.
  fn run(&self, game_state: &mut GameState) {
    debug!("Running command system.");
    while let Some(command) = game_state.dequeue_command() {
      command
        .execute(game_state)
        .map_err(|error| error!("Error running command: {}", error))
        .ok();
    }
    let effect = Effect::new(EffectType::SetInputReadyFlag(true), vec![]);
    effect.apply(game_state).ok();
  }
}

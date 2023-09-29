use crate::effect::{Effect, EffectType};
use crate::game_state::GameState;
use crate::system::SystemTrait;

/// The `Tick` system.
///
/// This system increments the tick counter.
#[derive(Debug, Default)]
pub struct Tick {}

impl Tick {
  /// Creates a new `Output`.
  pub fn new() -> Self {
    Self {}
  }
}

impl SystemTrait<GameState> for Tick {
  /// Runs the `Tick` system.
  fn run(&mut self, game_state: &mut GameState) {
    debug!("Running tick system.");
    let effect = Effect::new(EffectType::IncrementTickCounter, vec![]);
    effect.apply(game_state).ok();
  }
}

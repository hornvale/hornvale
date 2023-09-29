use crate::game_state::GameStateTrait;

/// The `System` trait.
///
/// This is a trait describing any `System` struct.
pub trait System<T: GameStateTrait> {
  /// Runs the `System`.
  fn run(&mut self, game_state: &mut T);
}

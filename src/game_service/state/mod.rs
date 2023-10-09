/// The `GameServiceState` enum.
#[derive(Clone, Debug, Default)]
pub enum State {
  /// The default, initial state.
  #[default]
  Default,
  /// The game is running.
  Running,
}

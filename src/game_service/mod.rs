pub mod state;
pub use state::State as GameServiceState;

/// The `GameService` service.
///
/// This is a stateful service that manages the game by emitting events.
#[derive(Clone, Debug, Default)]
pub struct GameService {
  /// The `GameService`'s state.
  state: GameServiceState,
}

impl GameService {
  /// Creates a new `GameService`.
  pub fn new() -> Self {
    Self::default()
  }

  /// Starts the game.
  pub fn start(&mut self) {
    self.state = GameServiceState::Running;
  }

  /// Quits the game.
  pub fn quit(&mut self) {
    self.state = GameServiceState::Default;
  }
}

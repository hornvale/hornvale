use crate::game_state::GameState;
use crate::game_state::LocalDataDirTrait;

impl LocalDataDirTrait for GameState {
  /// Get the local data directory.
  fn get_local_data_dir(&self) -> &str {
    self.local_data_dir.as_str()
  }
}

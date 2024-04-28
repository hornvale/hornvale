use crate::database::prelude::*;
use platform_dirs::AppDirs;

impl Default for Database {
  /// Create a new database with default settings.
  /// The default settings are in `~/.local/share/hornvale/`:
  /// - `~/.local/share/hornvale/cache`
  /// - `~/.local/share/hornvale/data`
  /// - `~/.local/share/hornvale/config`
  /// - `~/.local/share/hornvale/state`
  fn default() -> Self {
    let app_dirs = AppDirs::new(Some("hornvale"), true).unwrap();
    Self::at_path(app_dirs.data_dir)
  }
}

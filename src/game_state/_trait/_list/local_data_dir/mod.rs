/// The `LocalDataDir` trait.
pub trait LocalDataDir {
  /// Get local data dir path.
  fn get_local_data_dir(&self) -> &str;
}

/// A trait for checking if the quit flag is set.
pub trait IsQuitFlagSet {
  /// Check to see if the quit flag is set.
  fn is_quit_flag_set(&self) -> bool;
}

/// The `DiegeticFlag` trait.
///
/// This flag indicates that at least one diegetic command has been executed.
pub trait DiegeticFlag {
  /// Get diegetic flag.
  fn get_diegetic_flag(&self) -> bool;
  /// Set diegetic flag.
  fn set_diegetic_flag(&mut self, value: bool);
}

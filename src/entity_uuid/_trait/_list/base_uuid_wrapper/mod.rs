/// The `BaseUuidWrapper` trait.
pub trait BaseUuidWrapper {
  /// Constructor.
  fn new(uuid: String) -> Self
  where
    Self: Sized;
}

use crate::lookup_service::Lookup;

/// The `LookupService` trait.
pub trait LookupService {
  /// Get lookup service.
  fn get_lookup_service(&self) -> &Lookup;
  /// Get lookup service mutably.
  fn get_lookup_service_mut(&mut self) -> &mut Lookup;
}

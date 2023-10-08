use crate::lookup_service::LookupService as LookupServiceType;

/// The `LookupService` trait.
pub trait LookupService {
  /// Get lookup service.
  fn get_lookup_service(&self) -> &LookupServiceType;
  /// Get lookup service mutably.
  fn get_lookup_service_mut(&mut self) -> &mut LookupServiceType;
}

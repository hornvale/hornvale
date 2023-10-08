use crate::game_state::GameState;
use crate::game_state::LookupServiceTrait;
use crate::lookup_service::LookupService;

/// Implementation of the `LookupService` trait.
impl LookupServiceTrait for GameState {
  /// Get lookup service.
  fn get_lookup_service(&self) -> &LookupService {
    &self.lookup_service
  }
  /// Get lookup service mutably
  fn get_lookup_service_mut(&mut self) -> &mut LookupService {
    &mut self.lookup_service
  }
}

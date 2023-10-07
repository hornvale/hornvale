use crate::game_state::GameState;
use crate::game_state::LookupServiceTrait;
use crate::lookup_service::Lookup;

/// Implementation of the `LookupService` trait.
impl LookupServiceTrait for GameState {
  /// Get lookup service.
  fn get_lookup_service(&self) -> &Lookup {
    &self.lookup_service
  }
  /// Get lookup service mutably
  fn get_lookup_service_mut(&mut self) -> &mut Lookup {
    &mut self.lookup_service
  }
}

use mockall::predicate::*;
use mockall::*;

use crate::effect::EffectContextDataTrait;

/// The `EffectContext` struct, which represents the context of an effect
/// application.
#[automock]
pub trait EffectContext {
  fn get_data(&self) -> &dyn EffectContextDataTrait;
  fn get_data_mut(&mut self) -> &mut dyn EffectContextDataTrait;
}

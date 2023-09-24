use crate::effect::EffectContextDataTrait;

/// The `EffectContext` struct, which represents the context of an effect
/// application.
pub trait EffectContext<'data> {
  type Data: EffectContextDataTrait + 'data;
  fn get_data(&self) -> &Self::Data;
  fn get_data_mut(&mut self) -> &mut Self::Data;
}

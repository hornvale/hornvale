use crate::effect::EffectContext;
use crate::effect::EffectError;
use crate::effect::EffectTrait;

/// The `NullEffect` effect struct.
///
/// This effect does nothing.
#[derive(Clone, Debug, Default)]
pub struct Null {}

impl EffectTrait for Null {
  fn apply(&self, _context: &mut EffectContext) -> Result<(), EffectError> {
    Ok(())
  }
}

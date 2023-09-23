use std::fmt::Debug;

use crate::effect::EffectContext;
use crate::effect::EffectError;

/// The `Effect` trait.
///
/// This trait represents an effect that is used to modify the game world or
/// provide some other functionality.
pub trait Effect: Debug + Send + Sync {
  /// Applies this effect.
  ///
  /// This method returns a `Result` that indicates whether the effect was
  /// executed successfully.
  fn apply(&self, _context: &mut EffectContext) -> Result<(), EffectError>;
}

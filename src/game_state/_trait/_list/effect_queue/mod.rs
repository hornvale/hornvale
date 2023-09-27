use crate::effect::EffectTrait;
use crate::game_state::GameState;

/// The `EffectQueue` trait.
pub trait EffectQueue {
  /// Enqueues an effect.
  fn enqueue_effect(&mut self, effect: Box<dyn EffectTrait<GameState>>);
  /// Dequeues an effect.
  fn dequeue_effect(&mut self) -> Option<Box<dyn EffectTrait<GameState>>>;
}

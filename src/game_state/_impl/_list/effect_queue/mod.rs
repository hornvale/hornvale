use crate::effect::EffectTrait;
use crate::game_state::EffectQueueTrait;
use crate::game_state::GameState;

impl EffectQueueTrait for GameState {
  /// Enqueue an effect.
  fn enqueue_effect(&mut self, effect: Box<dyn EffectTrait<GameState>>) {
    self.effect_queue.push_back(effect);
  }

  /// Dequeue an effect.
  fn dequeue_effect(&mut self) -> Option<Box<dyn EffectTrait<GameState>>> {
    self.effect_queue.pop_front()
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  use crate::effect::NoOpEffect;
  use crate::test::init;

  #[test]
  fn test_enqueue_effect() {
    init();
    let mut game_state = GameState::new();
    let effect = Box::new(NoOpEffect::new());
    game_state.enqueue_effect(effect);
    assert_eq!(game_state.effect_queue.len(), 1);
  }

  #[test]
  fn test_dequeue_effect() {
    init();
    let mut game_state = GameState::new();
    let effect = Box::new(NoOpEffect::new());
    game_state.enqueue_effect(effect);
    let _effect = game_state.dequeue_effect();
  }
}

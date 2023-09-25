use specs::prelude::*;

use crate::dispatcher::*;

/// The `DispatcherClock` struct.
///
/// This holds references to system dispatchers and maintains an internal clock
/// that is used to dispatch systems at a fixed rate.
#[derive(Derivative)]
#[derivative(Debug)]
pub struct DispatcherClock {
  /// The current tick.
  pub tick: u64,
  /// The tick dispatcher.
  #[derivative(Debug = "ignore")]
  pub tick_dispatcher: Dispatcher<'static, 'static>,
  /// Every tenth tick (roughly).
  #[derivative(Debug = "ignore")]
  pub deca_tick_dispatcher: Dispatcher<'static, 'static>,
  /// Every hundredth tick (roughly).
  #[derivative(Debug = "ignore")]
  pub hecto_tick_dispatcher: Dispatcher<'static, 'static>,
  /// Every thousandth tick (roughly).
  #[derivative(Debug = "ignore")]
  pub kilo_tick_dispatcher: Dispatcher<'static, 'static>,
}

impl DispatcherClock {
  pub fn new(ecs: &mut World) -> Self {
    let tick = 0;
    let tick_dispatcher = get_tick_dispatcher(ecs);
    let deca_tick_dispatcher = get_deca_tick_dispatcher(ecs);
    let hecto_tick_dispatcher = get_hecto_tick_dispatcher(ecs);
    let kilo_tick_dispatcher = get_kilo_tick_dispatcher(ecs);
    Self {
      tick,
      tick_dispatcher,
      deca_tick_dispatcher,
      hecto_tick_dispatcher,
      kilo_tick_dispatcher,
    }
  }

  pub fn tick(&mut self, ecs: &World) {
    // Each tick, run all of the systems.  We could have multiple
    // dispatchers, each running a subset of the systems, and scheduled
    // differently.
    self.tick = self.tick.wrapping_add(1);
    self.tick_dispatcher.dispatch(ecs);
    if self.tick % 10 == 0 {
      self.deca_tick_dispatcher.dispatch(ecs);
      if self.tick % 100 == 0 {
        self.hecto_tick_dispatcher.dispatch(ecs);
        if self.tick % 1000 == 0 {
          self.kilo_tick_dispatcher.dispatch(ecs);
        }
      }
    }
  }
}

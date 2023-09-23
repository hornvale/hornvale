use specs::prelude::*;
use specs::shrev::ReaderId;

use crate::ecs::event::*;
use crate::ecs::AllData;
use crate::effect::EffectContext;

pub struct EffectProcessor {
  pub reader_id: ReaderId<EffectEvent>,
}

impl EffectProcessor {}

impl<'data> System<'data> for EffectProcessor {
  type SystemData = AllData<'data>;

  /// Run the system.
  fn run(&mut self, mut data: Self::SystemData) {
    let events = data
      .effect_event_channel
      .read(&mut self.reader_id)
      .cloned()
      .collect::<Vec<_>>();
    if events.is_empty() {
      return;
    }
    for event in events {
      debug!("Processing next event {:?}", event);
      let effect = event.effect;
      let mut context = EffectContext::new(&mut data);
      let result = effect.apply(&mut context);
      match result {
        Ok(()) => {},
        Err(error) => {
          error!("Effect failed failed to apply with error {:?}", error);
          write_output_error!(data, format!("Error: {}", error));
        },
      }
    }
  }
}

use specs::prelude::*;

use crate::resource::TickResource;

/// The tick system.
#[derive(Debug, Default)]
pub struct Tick {}

#[derive(SystemData)]
pub struct Data<'data> {
  pub entities: Entities<'data>,
  pub tick_resource: Write<'data, TickResource>,
}

impl<'data> System<'data> for Tick {
  type SystemData = Data<'data>;

  /// Run the system.
  fn run(&mut self, mut data: Self::SystemData) {
    // Increment the tick counter.
    data.tick_resource.0 = data.tick_resource.0.wrapping_add(1);
  }
}

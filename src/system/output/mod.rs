use specs::prelude::*;
use specs::shrev::{EventChannel, ReaderId};
use std::io::Write as _;

use crate::event::OutputEvent;
use crate::resource::OutputResource;

#[derive(Default)]
pub struct Output {
  pub reader_id: Option<ReaderId<OutputEvent>>,
}

impl Output {}

#[derive(SystemData)]
pub struct Data<'data> {
  pub entities: Entities<'data>,
  pub output_resource: Write<'data, OutputResource>,
  pub output_event_channel: Read<'data, EventChannel<OutputEvent>>,
}

impl<'data> System<'data> for Output {
  type SystemData = Data<'data>;

  /// Run the system.
  fn run(&mut self, mut data: Self::SystemData) {
    let output_events = data
      .output_event_channel
      .read(self.reader_id.as_mut().unwrap())
      .collect::<Vec<&OutputEvent>>();
    let event_count = output_events.len();
    if event_count == 0 {
      return;
    }
    info!("Processing {} output event(s)...", event_count);
    if let Some(output) = &mut data.output_resource.0 {
      for event in output_events.iter() {
        writeln!(output, "{}\n", event.output).unwrap();
      }
    }
  }

  fn setup(&mut self, world: &mut World) {
    Self::SystemData::setup(world);
    self.reader_id = Some(world.fetch_mut::<EventChannel<OutputEvent>>().register_reader());
  }
}

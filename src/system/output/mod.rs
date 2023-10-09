use specs::prelude::*;
use specs::shrev::{EventChannel, ReaderId};
use std::io::Write as _;

use crate::event::OutputEvent;
use crate::resource::OutputResource;

pub struct Output {
  pub reader_id: ReaderId<OutputEvent>,
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
      .read(&mut self.reader_id)
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
}

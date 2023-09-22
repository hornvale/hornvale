use rustyline_async::SharedWriter;
use specs::prelude::*;
use specs::shrev::{EventChannel, ReaderId};
use std::io::Write as _;

use crate::ecs::event::OutputEvent;
use crate::ecs::resource::*;
use crate::output::formatting::format_string;

pub struct OutputProcessor {
  pub reader_id: ReaderId<OutputEvent>,
  pub output: SharedWriter,
}

impl OutputProcessor {}

#[derive(Derivative, SystemData)]
#[derivative(Debug)]
pub struct Data<'data> {
  #[derivative(Debug = "ignore")]
  pub entities: Entities<'data>,
  #[derivative(Debug = "ignore")]
  pub output_resource: Write<'data, OutputResource>,
  #[derivative(Debug = "ignore")]
  pub output_event_channel: Read<'data, EventChannel<OutputEvent>>,
}

impl<'data> System<'data> for OutputProcessor {
  type SystemData = Data<'data>;

  /// Run the system.
  fn run(&mut self, data: Self::SystemData) {
    let output = &mut self.output;
    let events = data
      .output_event_channel
      .read(&mut self.reader_id)
      .cloned()
      .collect::<Vec<_>>();
    if events.is_empty() {
      return;
    }
    for event in events {
      let string = format_string(event.output.trim());
      writeln!(output, "{}\n", string).unwrap();
    }
  }
}

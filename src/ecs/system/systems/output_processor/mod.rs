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
pub struct Data<'a> {
  #[derivative(Debug = "ignore")]
  pub entities: Entities<'a>,
  #[derivative(Debug = "ignore")]
  pub output_resource: Write<'a, OutputResource>,
  #[derivative(Debug = "ignore")]
  pub output_event_channel: Read<'a, EventChannel<OutputEvent>>,
}

impl<'a> System<'a> for OutputProcessor {
  type SystemData = Data<'a>;

  /// Run the system.
  fn run(&mut self, data: Self::SystemData) {
    let output = &mut self.output;
    for event in data.output_event_channel.read(&mut self.reader_id) {
      let string = format_string(event.output.trim());
      writeln!(output, "{}\n", string).unwrap();
    }
  }
}

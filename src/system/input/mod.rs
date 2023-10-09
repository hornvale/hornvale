use specs::prelude::*;
use specs::shrev::{EventChannel, ReaderId};

use crate::event::{InputEvent, OutputEvent};

/// The Input system.
pub struct Input {
  pub reader_id: ReaderId<InputEvent>,
}

impl Input {}

#[derive(SystemData)]
pub struct Data<'data> {
  pub entities: Entities<'data>,
  pub input_event_channel: Read<'data, EventChannel<InputEvent>>,
  pub output_event_channel: Write<'data, EventChannel<OutputEvent>>,
}

impl<'data> System<'data> for Input {
  type SystemData = Data<'data>;

  /// Run system.
  fn run(&mut self, mut data: Self::SystemData) {
    let input_events = data
      .input_event_channel
      .read(&mut self.reader_id)
      .cloned()
      .collect::<Vec<InputEvent>>();
    let event_count = input_events.len();
    if event_count == 0 {
      return;
    }
    info!("Processing {} input event(s)...", event_count);
    for input_event in input_events.iter() {
      data.output_event_channel.single_write(OutputEvent {
        output: input_event.input.clone(),
      });
    }
  }
}

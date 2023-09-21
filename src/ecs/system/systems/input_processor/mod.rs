use specs::prelude::*;
use specs::shrev::{EventChannel, ReaderId};

use crate::command::Command;
use crate::ecs::event::*;

pub struct InputProcessor {
  pub reader_id: ReaderId<InputEvent>,
}

impl InputProcessor {}

#[derive(SystemData)]
pub struct Data<'a> {
  pub entities: Entities<'a>,
  pub input_event_channel: Read<'a, EventChannel<InputEvent>>,
  pub command_event_channel: Write<'a, EventChannel<CommandEvent>>,
  pub output_event_channel: Write<'a, EventChannel<OutputEvent>>,
}

impl<'a> System<'a> for InputProcessor {
  type SystemData = Data<'a>;

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
    for event in input_events.iter() {
      let _input_string = &event.input;
      write_command_event!(data, Command {});
    }
  }
}

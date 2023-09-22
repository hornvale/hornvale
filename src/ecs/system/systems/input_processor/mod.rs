use specs::prelude::*;
use specs::shrev::{EventChannel, ReaderId};

use crate::command::ParsingStrategy;
use crate::ecs::event::*;

pub struct InputProcessor {
  pub reader_id: ReaderId<InputEvent>,
  pub parsers: Vec<Box<dyn ParsingStrategy>>,
}

impl InputProcessor {}

#[derive(Derivative, SystemData)]
#[derivative(Debug)]
pub struct Data<'data> {
  #[derivative(Debug = "ignore")]
  pub entities: Entities<'data>,
  #[derivative(Debug = "ignore")]
  pub input_event_channel: Read<'data, EventChannel<InputEvent>>,
  #[derivative(Debug = "ignore")]
  pub command_event_channel: Write<'data, EventChannel<CommandEvent>>,
  #[derivative(Debug = "ignore")]
  pub output_event_channel: Write<'data, EventChannel<OutputEvent>>,
}

impl<'data> System<'data> for InputProcessor {
  type SystemData = Data<'data>;

  /// Run system.
  fn run(&mut self, mut data: Self::SystemData) {
    let events = data
      .input_event_channel
      .read(&mut self.reader_id)
      .cloned()
      .collect::<Vec<_>>();
    if events.is_empty() {
      return;
    }
    for event in events {
      let input_string = &event.input;
      let mut handled = false;
      for parser in &self.parsers {
        if let Some(command) = parser.parse(input_string) {
          handled = true;
          write_command_event!(data, command);
          break;
        }
      }
      if !handled {
        write_output_error!(data, format!("Unknown command: {}", input_string));
      }
    }
  }
}

use specs::prelude::*;
use specs::shrev::{EventChannel, ReaderId};
use std::sync::Arc;

use crate::command::CommandTrait;
use crate::command::ParsingStrategyTrait;
use crate::ecs::event::*;
use crate::ecs::WriteCommandEventTrait;
use crate::ecs::WriteOutputEventTrait;

pub struct InputProcessor {
  pub reader_id: ReaderId<InputEvent>,
  pub parsers: Vec<Box<dyn ParsingStrategyTrait>>,
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

impl WriteCommandEventTrait for Data<'_> {
  fn write_command_event(&mut self, command: Arc<dyn CommandTrait>) {
    self.command_event_channel.single_write(CommandEvent { command });
  }
}

impl WriteOutputEventTrait for Data<'_> {
  fn write_output_event(&mut self, output: String) {
    self.output_event_channel.single_write(OutputEvent { output });
  }
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
          write_command_event!(data, Into::<Arc<dyn CommandTrait>>::into(command));
          break;
        }
      }
      if !handled {
        write_output_error!(data, format!("Unknown command: {}", input_string));
      }
    }
  }
}

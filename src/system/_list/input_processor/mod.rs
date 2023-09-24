use specs::prelude::*;
use specs::shrev::ReaderId;
use std::sync::Arc;

use crate::command::CommandTrait;
use crate::command::ParsingStrategyTrait;
use crate::event::*;
use crate::system_data::AllData;

pub struct InputProcessor {
  pub reader_id: ReaderId<InputEvent>,
  pub parsers: Vec<Box<dyn ParsingStrategyTrait>>,
}

impl InputProcessor {}

impl<'data> System<'data> for InputProcessor {
  type SystemData = AllData<'data>;

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

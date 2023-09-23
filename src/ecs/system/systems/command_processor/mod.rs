use specs::prelude::*;
use specs::shrev::ReaderId;

use crate::command::CommandContext;
use crate::ecs::AllData;
use crate::event::*;

pub struct CommandProcessor {
  pub reader_id: ReaderId<CommandEvent>,
}

impl CommandProcessor {}

impl<'data> System<'data> for CommandProcessor {
  type SystemData = AllData<'data>;

  /// Run the system.
  fn run(&mut self, mut data: Self::SystemData) {
    let events = data
      .command_event_channel
      .read(&mut self.reader_id)
      .cloned()
      .collect::<Vec<_>>();
    if events.is_empty() {
      return;
    }
    for event in events {
      debug!("Processing next event {:?}", event);
      let command = event.command;
      let mut context = CommandContext::new(&mut data);
      let result = command.execute(&mut context);
      match result {
        Ok(()) => {},
        Err(error) => {
          error!("Command failed with error {:?}", error);
          write_output_error!(data, format!("Error: {}", error));
        },
      }
    }
  }
}

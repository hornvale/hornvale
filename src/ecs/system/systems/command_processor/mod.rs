use specs::prelude::*;
use specs::shrev::ReaderId;

use crate::command::CommandContext;
use crate::command::CommandResult;
use crate::ecs::event::*;
use crate::ecs::AllData;

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
        Ok(result) => {
          debug!("Command succeeded with result {:?}", result);
          match result {
            CommandResult::SucceededQuietly => {
              write_output_event!(data, "");
            },
            CommandResult::SucceededWithOutput(output) => {
              write_output_event!(data, output);
            },
            CommandResult::Action(action) => {
              write_action_event!(data, action.into());
            },
          }
        },
        Err(error) => {
          debug!("Command failed with error {:?}", error);
          write_output_error!(data, format!("Error: {}", error));
        },
      }
    }
  }
}

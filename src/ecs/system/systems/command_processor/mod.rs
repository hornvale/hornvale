use specs::prelude::*;
use specs::shrev::ReaderId;

use crate::command::CommandContext;
use crate::ecs::event::*;
use crate::ecs::AllData;

pub struct CommandProcessor {
  pub reader_id: ReaderId<CommandEvent>,
}

impl CommandProcessor {}

impl<'a> System<'a> for CommandProcessor {
  type SystemData = AllData<'a>;

  /// Run the system.
  fn run(&mut self, mut data: Self::SystemData) {
    let commands = data
      .command_event_channel
      .read(&mut self.reader_id)
      .cloned()
      .map(|event| event.command)
      .collect::<Vec<_>>();
    for command in commands {
      debug!("Processing next command {:?}", command);
      {
        let mut context = CommandContext::new(&mut data);
        let result = command.execute(&mut context);
        if let Err(error) = result {
          write_output_event!(data, format!("Error: {}", error));
        }
      }
    }
  }
}

use specs::prelude::*;
use specs::shrev::{EventChannel, ReaderId};

use crate::ecs::event::*;

pub struct CommandProcessor {
  pub reader_id: ReaderId<CommandEvent>,
}

impl CommandProcessor {}

#[derive(SystemData)]
pub struct Data<'a> {
  pub entities: Entities<'a>,
  pub command_event_channel: Read<'a, EventChannel<CommandEvent>>,
  pub output_event_channel: Write<'a, EventChannel<OutputEvent>>,
}

impl<'a> System<'a> for CommandProcessor {
  type SystemData = Data<'a>;

  /// Run the system.
  fn run(&mut self, mut data: Self::SystemData) {
    let command_events = data
      .command_event_channel
      .read(&mut self.reader_id)
      .cloned()
      .collect::<Vec<CommandEvent>>();
    let event_count = command_events.len();
    if event_count == 0 {
      return;
    }
    info!("Processing {} command event(s)...", event_count);
    for event in command_events.iter() {
      debug!("Processing next command event {:?}", event);
      let CommandEvent { command } = event;
      write_output_event!(data, format!("Command: {:?}", command));
    }
  }
}

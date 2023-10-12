use specs::prelude::*;
use specs::shrev::{EventChannel, ReaderId};

use crate::command::CommandData;
use crate::event::CommandEvent;

/// The Command system.
#[derive(Default)]
pub struct Command {
  pub reader_id: Option<ReaderId<CommandEvent>>,
}

impl Command {}

impl<'data> System<'data> for Command {
  type SystemData = CommandData<'data>;

  /// Run system.
  fn run(&mut self, mut data: Self::SystemData) {
    let command_events = data
      .command_event_channel
      .read(self.reader_id.as_mut().unwrap())
      .cloned()
      .collect::<Vec<CommandEvent>>();
    let event_count = command_events.len();
    if event_count == 0 {
      return;
    }
    info!("Processing {} command event(s)...", event_count);
    for command_event in command_events.iter() {
      if let Err(error) = command_event.command.execute(&mut data) {
        eprintln!("{}", error);
      }
    }
  }

  fn setup(&mut self, world: &mut World) {
    Self::SystemData::setup(world);
    self.reader_id = Some(world.fetch_mut::<EventChannel<CommandEvent>>().register_reader());
  }
}

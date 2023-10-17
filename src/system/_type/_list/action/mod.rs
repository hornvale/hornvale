use specs::prelude::*;
use specs::shrev::{EventChannel, ReaderId};

use crate::action::ActionData;
use crate::event::ActionEvent;

/// The Command system.
#[derive(Default)]
pub struct Action {
  pub reader_id: Option<ReaderId<ActionEvent>>,
}

impl Action {}

impl<'data> System<'data> for Action {
  type SystemData = ActionData<'data>;

  /// Run system.
  fn run(&mut self, mut data: Self::SystemData) {
    let action_events = data
      .action_event_channel
      .read(self.reader_id.as_mut().unwrap())
      .cloned()
      .collect::<Vec<ActionEvent>>();
    let event_count = action_events.len();
    if event_count == 0 {
      return;
    }
    info!("Processing {} action event(s)...", event_count);
    for action_event in action_events.iter() {
      if let Err(error) = action_event.action.attempt(&mut data) {
        eprintln!("{}", error);
      }
    }
  }

  fn setup(&mut self, world: &mut World) {
    Self::SystemData::setup(world);
    self.reader_id = Some(world.fetch_mut::<EventChannel<ActionEvent>>().register_reader());
  }
}

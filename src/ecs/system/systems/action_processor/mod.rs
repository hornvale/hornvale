use specs::prelude::*;
use specs::shrev::ReaderId;

use crate::action::ActionContext;
use crate::ecs::event::*;
use crate::ecs::AllData;

pub struct ActionProcessor {
  pub reader_id: ReaderId<ActionEvent>,
}

impl ActionProcessor {}

impl<'data> System<'data> for ActionProcessor {
  type SystemData = AllData<'data>;

  /// Run the system.
  fn run(&mut self, mut data: Self::SystemData) {
    let events = data
      .action_event_channel
      .read(&mut self.reader_id)
      .cloned()
      .collect::<Vec<_>>();
    if events.is_empty() {
      return;
    }
    for event in events {
      debug!("Processing next event {:?}", event);
      let action = event.action;
      let mut context = ActionContext::new(&mut data);
      let result = action.execute(&mut context);
      match result {
        Ok(()) => {},
        Err(error) => {
          error!("Action failed with error {:?}", error);
          write_output_error!(data, format!("Error: {}", error));
        },
      }
    }
  }
}

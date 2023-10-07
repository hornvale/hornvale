use log::Level as LogLevel;
use std::cell::RefCell;
use std::rc::Rc;
use std::sync::Arc;
use uuid::Uuid;

use crate::event::Event;
use crate::event::EventPublisher;
use crate::event::EventSubscriberBuilder;
use crate::event::EventType;
use crate::game_state::GameState;

/// Attach a logger to a specified event type.
pub fn attach_logger(event_type: EventType, log_level: LogLevel, event_publisher: &mut EventPublisher) -> Uuid {
  let log_level = log_level;
  let debug_logger = EventSubscriberBuilder::new()
    .name("Debug Logger".to_string())
    .event_type(event_type)
    .will_process(Arc::new(move |event: &mut Event, _game_state: &GameState| {
      log!(log_level, "Will process event: {:#?}", event);
      Ok(())
    }))
    .did_process(Arc::new(move |event: &Event, _game_state: &mut GameState| {
      log!(log_level, "Did process event: {:#?}", event);
      Ok(())
    }))
    .build();
  let debug_logger_uuid = debug_logger.uuid;
  event_publisher.add_subscriber(Rc::new(RefCell::new(debug_logger)));
  debug_logger_uuid
}

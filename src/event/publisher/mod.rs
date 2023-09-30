use anyhow::Error as AnyError;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use uuid::Uuid;

use crate::event::Event;
use crate::event::EventSubscriber;
use crate::game_state::GameState;

/// The default implementation of the `EventPublisherTrait` trait.
#[derive(Derivative, Default)]
#[derivative(Debug)]
pub struct EventPublisher {
  /// The list of subscribers.
  #[derivative(Debug = "ignore")]
  subscribers: Vec<Rc<RefCell<EventSubscriber>>>,
  /// Subscribers by their unique ID.
  #[derivative(Debug = "ignore")]
  subscribers_by_id: HashMap<Uuid, Rc<RefCell<EventSubscriber>>>,
}

impl EventPublisher {
  /// Creates a new `EventPublisher`.
  pub fn new() -> Self {
    Self {
      subscribers: Vec::new(),
      subscribers_by_id: HashMap::new(),
    }
  }

  pub fn add_subscriber(&mut self, subscriber: EventSubscriber) {
    let subscriber = Rc::new(RefCell::new(subscriber));
    let uuid = subscriber.borrow().uuid;
    self.subscribers.push(subscriber.clone());
    self.subscribers_by_id.insert(uuid, subscriber);
  }

  pub fn remove_subscriber(&mut self, uuid: &Uuid) -> Result<EventSubscriber, AnyError> {
    if let Some(subscriber) = self.subscribers_by_id.remove(uuid) {
      self.subscribers.retain(|s| s.borrow().uuid != *uuid);
      Ok(subscriber.borrow().clone()) // Clone the inner value
    } else {
      Err(anyhow!("Subscriber not found: {}", uuid))
    }
  }

  pub fn publish_event(&mut self, event: &mut Event, game_state: &mut GameState) -> Result<(), AnyError> {
    if self.should_process(event, game_state)? {
      self.will_process(event, game_state)?;
      event.process(game_state)?;
      self.did_process(event, game_state)?;
    }
    Ok(())
  }

  pub fn should_process(&self, event: &Event, game_state: &mut GameState) -> Result<bool, AnyError> {
    let result = self
      .subscribers
      .iter()
      .all(|s| (s.borrow().should_process)(event, game_state) != Some(false));
    Ok(result)
  }

  pub fn will_process(&mut self, event: &mut Event, game_state: &GameState) -> Result<(), AnyError> {
    self
      .subscribers
      .iter_mut()
      .for_each(|s| (s.borrow_mut().will_process)(event, game_state));
    Ok(())
  }

  pub fn did_process(&mut self, event: &Event, game_state: &mut GameState) -> Result<(), AnyError> {
    self
      .subscribers
      .iter_mut()
      .for_each(|s| (s.borrow_mut().did_process)(event, game_state));
    Ok(())
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  use crate::event::Event;
  use crate::event::EventSubscriber;
  use crate::event::EventType;
  use crate::event::DEFAULT_PRIORITY;
  use crate::game_state::GameState;

  #[test]
  fn test_add_subscriber() {
    let mut event_publisher = EventPublisher::new();
    let subscriber = EventSubscriber::default();
    event_publisher.add_subscriber(subscriber);
  }

  #[test]
  fn test_remove_subscriber() {
    let mut event_publisher = EventPublisher::new();
    let subscriber = EventSubscriber::default();
    let uuid = subscriber.uuid.clone();
    event_publisher.add_subscriber(subscriber);
    let _subscriber = event_publisher.remove_subscriber(&uuid);
  }

  #[test]
  fn test_publish_event() {
    let mut event_publisher = EventPublisher::new();
    let subscriber = EventSubscriber::default();
    event_publisher.add_subscriber(subscriber);
    let mut event = Event::new(EventType::NoOp, DEFAULT_PRIORITY, vec![]);
    let mut game_state = GameState::new();
    event_publisher.publish_event(&mut event, &mut game_state).unwrap();
  }

  #[test]
  fn test_should_process() {
    let mut event_publisher = EventPublisher::new();
    let subscriber = EventSubscriber::default();
    event_publisher.add_subscriber(subscriber);
    let event = Event::new(EventType::NoOp, DEFAULT_PRIORITY, vec![]);
    let mut game_state = GameState::new();
    event_publisher.should_process(&event, &mut game_state).unwrap();
  }

  #[test]
  fn test_will_process() {
    let mut event_publisher = EventPublisher::new();
    let subscriber = EventSubscriber::default();
    event_publisher.add_subscriber(subscriber);
    let mut event = Event::new(EventType::NoOp, DEFAULT_PRIORITY, vec![]);
    let game_state = GameState::new();
    event_publisher.will_process(&mut event, &game_state).unwrap();
  }

  #[test]
  fn test_did_process() {
    let mut event_publisher = EventPublisher::new();
    let subscriber = EventSubscriber::default();
    event_publisher.add_subscriber(subscriber);
    let event = Event::new(EventType::NoOp, DEFAULT_PRIORITY, vec![]);
    let mut game_state = GameState::new();
    event_publisher.did_process(&event, &mut game_state).unwrap();
  }

  #[test]
  fn test_new() {
    let event_publisher = EventPublisher::new();
    assert_eq!(event_publisher.subscribers.len(), 0);
    assert_eq!(event_publisher.subscribers_by_id.len(), 0);
  }
}

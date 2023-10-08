use anyhow::Error as AnyError;
use std::cell::RefCell;
use std::collections::BTreeSet;
use std::collections::HashMap;
use std::mem::{discriminant, Discriminant};
use std::rc::Rc;
use uuid::Uuid;

use crate::event::Event;
use crate::event::EventSubscriber;
use crate::event::EventType;
use crate::game_state::GameState;

/// The `EventPublisher` struct.
#[derive(Derivative, Default)]
#[derivative(Debug)]
pub struct EventPublisher {
  // Priority queues of subscribers, organized by event type discriminant.
  #[derivative(Debug = "ignore")]
  subscribers_by_event_type: HashMap<Discriminant<EventType>, BTreeSet<Rc<RefCell<EventSubscriber>>>>,
  /// Subscribers by their unique ID.
  #[derivative(Debug = "ignore")]
  subscribers_by_id: HashMap<Uuid, Rc<RefCell<EventSubscriber>>>,
}

impl EventPublisher {
  /// Creates a new `EventPublisher`.
  pub fn new() -> Self {
    Self {
      subscribers_by_event_type: HashMap::new(),
      subscribers_by_id: HashMap::new(),
    }
  }

  pub fn add_subscriber(&mut self, subscriber: Rc<RefCell<EventSubscriber>>) {
    let event_type = subscriber.borrow().event_type.clone();
    let uuid = subscriber.borrow().uuid;
    #[allow(clippy::mutable_key_type)]
    let subscribers_by_event_type = self
      .subscribers_by_event_type
      .entry(discriminant(&event_type))
      .or_default();
    subscribers_by_event_type.insert(subscriber.clone());
    self.subscribers_by_id.insert(uuid, subscriber);
  }

  pub fn remove_subscriber(&mut self, uuid: &Uuid) -> Result<EventSubscriber, AnyError> {
    if let Some(subscriber) = self.subscribers_by_id.remove(uuid) {
      let event_type = subscriber.borrow().event_type.clone();
      let discriminant = discriminant(&event_type);
      #[allow(clippy::mutable_key_type)]
      let subscribers_by_event_type = self.subscribers_by_event_type.get_mut(&discriminant).unwrap();
      subscribers_by_event_type.remove(&subscriber);
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
    let discriminant = discriminant(&event.event_type);
    if let Some(subscribers) = self.subscribers_by_event_type.get(&discriminant) {
      let result = subscribers
        .iter()
        .all(|s| (s.borrow().should_process)(event, game_state).ok() != Some(Some(false)));
      Ok(result)
    } else {
      Ok(true)
    }
  }

  pub fn will_process(&mut self, event: &mut Event, game_state: &GameState) -> Result<(), AnyError> {
    let discriminant = discriminant(&event.event_type);
    if let Some(subscribers) = self.subscribers_by_event_type.get(&discriminant) {
      subscribers.iter().for_each(|s| {
        (s.borrow().will_process)(event, game_state).ok();
      });
    }
    Ok(())
  }

  pub fn did_process(&mut self, event: &Event, game_state: &mut GameState) -> Result<(), AnyError> {
    let discriminant = discriminant(&event.event_type);
    if let Some(subscribers) = self.subscribers_by_event_type.get(&discriminant) {
      subscribers.iter().for_each(|s| {
        (s.borrow().did_process)(event, game_state).ok();
      });
    }
    Ok(())
  }
}

#[cfg(test)]
mod tests {

  use std::sync::Arc;

  use super::*;

  use crate::event::Event;
  use crate::event::EventFilterRule;
  use crate::event::EventSubscriber;
  use crate::event::EventType;
  use crate::event::DEFAULT_PRIORITY;
  use crate::game_state::GameState;
  use crate::test::init;

  #[test]
  fn test_add_subscriber() {
    init();
    let mut event_publisher = EventPublisher::new();
    let subscriber = EventSubscriber::default();
    event_publisher.add_subscriber(Rc::new(RefCell::new(subscriber)));
  }

  #[test]
  fn test_remove_subscriber() {
    init();
    let mut event_publisher = EventPublisher::new();
    let subscriber = EventSubscriber::default();
    let uuid = subscriber.uuid.clone();
    event_publisher.add_subscriber(Rc::new(RefCell::new(subscriber)));
    let _subscriber = event_publisher.remove_subscriber(&uuid);
  }

  #[test]
  fn test_publish_event() {
    init();
    let mut event_publisher = EventPublisher::new();
    let subscriber = EventSubscriber::default();
    event_publisher.add_subscriber(Rc::new(RefCell::new(subscriber)));
    let mut event = Event::new(EventType::NoOp, DEFAULT_PRIORITY, vec![], vec![]);
    let mut game_state = GameState::new();
    event_publisher.publish_event(&mut event, &mut game_state).unwrap();
  }

  #[test]
  fn test_should_process() {
    init();
    let mut event_publisher = EventPublisher::new();
    let subscriber = EventSubscriber::default();
    event_publisher.add_subscriber(Rc::new(RefCell::new(subscriber)));
    let event = Event::new(EventType::NoOp, DEFAULT_PRIORITY, vec![], vec![]);
    let mut game_state = GameState::new();
    event_publisher.should_process(&event, &mut game_state).unwrap();
  }

  #[test]
  fn test_will_process() {
    init();
    let mut event_publisher = EventPublisher::new();
    let subscriber = EventSubscriber::default();
    event_publisher.add_subscriber(Rc::new(RefCell::new(subscriber)));
    let mut event = Event::new(EventType::NoOp, DEFAULT_PRIORITY, vec![], vec![]);
    let game_state = GameState::new();
    event_publisher.will_process(&mut event, &game_state).unwrap();
  }

  #[test]
  fn test_did_process() {
    init();
    let mut event_publisher = EventPublisher::new();
    let subscriber = EventSubscriber::default();
    event_publisher.add_subscriber(Rc::new(RefCell::new(subscriber)));
    let event = Event::new(EventType::NoOp, DEFAULT_PRIORITY, vec![], vec![]);
    let mut game_state = GameState::new();
    event_publisher.did_process(&event, &mut game_state).unwrap();
  }

  #[test]
  fn test_new() {
    init();
    let event_publisher = EventPublisher::new();
    assert_eq!(event_publisher.subscribers_by_event_type.len(), 0);
    assert_eq!(event_publisher.subscribers_by_id.len(), 0);
  }

  #[test]
  fn test_ordered_subscribers() {
    init();
    let mut event_publisher = EventPublisher::new();
    let subscriber1 = EventSubscriber::new(
      String::from("Subscriber 1"),
      1,
      EventType::NoOp,
      EventFilterRule::Always,
      Arc::new(|_, _| Ok(None)),
      Arc::new(|_, _| Ok(())),
      Arc::new(|_, _| Ok(())),
      true,
    );
    let subscriber2 = EventSubscriber::new(
      String::from("Subscriber 2"),
      2,
      EventType::NoOp,
      EventFilterRule::Always,
      Arc::new(|_, _| Ok(None)),
      Arc::new(|_, _| Ok(())),
      Arc::new(|_, _| Ok(())),
      true,
    );
    let subscriber3 = EventSubscriber::new(
      String::from("Subscriber 3"),
      3,
      EventType::NoOp,
      EventFilterRule::Always,
      Arc::new(|_, _| Ok(None)),
      Arc::new(|_, _| Ok(())),
      Arc::new(|_, _| Ok(())),
      true,
    );
    event_publisher.add_subscriber(Rc::new(RefCell::new(subscriber2)));
    event_publisher.add_subscriber(Rc::new(RefCell::new(subscriber1)));
    event_publisher.add_subscriber(Rc::new(RefCell::new(subscriber3)));
    let mut subscribers = event_publisher
      .subscribers_by_event_type
      .get(&discriminant(&EventType::NoOp))
      .unwrap()
      .iter();
    let subscriber = subscribers.next().unwrap();
    assert_eq!(subscriber.borrow().priority, 3);
    let subscriber = subscribers.next().unwrap();
    assert_eq!(subscriber.borrow().priority, 2);
    let subscriber = subscribers.next().unwrap();
    assert_eq!(subscriber.borrow().priority, 1);
  }
}

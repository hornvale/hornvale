use anyhow::Error as AnyError;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub mod _trait;
pub use _trait::*;

use crate::event::EventTrait;
use crate::event_subscriber::EventSubscriberTrait;
use crate::game_state::GameState;

/// The default implementation of the `EventPublisherTrait` trait.
#[derive(Derivative, Default)]
#[derivative(Debug)]
pub struct EventPublisher {
  /// The list of subscribers.
  #[derivative(Debug = "ignore")]
  subscribers: Vec<Rc<RefCell<dyn EventSubscriberTrait<GameState>>>>,
  /// Subscribers by their subscriber ID.
  #[derivative(Debug = "ignore")]
  subscribers_by_id: HashMap<String, Rc<RefCell<dyn EventSubscriberTrait<GameState>>>>,
}

impl EventPublisher {
  /// Creates a new `EventPublisher`.
  pub fn new() -> Self {
    Self {
      subscribers: Vec::new(),
      subscribers_by_id: HashMap::new(),
    }
  }
}

impl EventPublisherTrait<GameState> for EventPublisher {
  fn add_subscriber(&mut self, subscriber: Rc<RefCell<dyn EventSubscriberTrait<GameState>>>) {
    let subscriber_id = subscriber.borrow().get_id().to_string();
    self.subscribers.push(subscriber.clone());
    self.subscribers_by_id.insert(subscriber_id, subscriber);
  }

  fn remove_subscriber(
    &mut self,
    subscriber_id: &str,
  ) -> Result<Rc<RefCell<dyn EventSubscriberTrait<GameState>>>, AnyError> {
    if let Some(subscriber) = self.subscribers_by_id.remove(subscriber_id) {
      self.subscribers.retain(|s| s.borrow().get_id() != subscriber_id);
      Ok(subscriber)
    } else {
      Err(anyhow!("Subscriber not found: {}", subscriber_id))
    }
  }

  fn publish_event(
    &mut self,
    event: &mut dyn EventTrait<GameState>,
    game_state: &mut GameState,
  ) -> Result<(), AnyError> {
    if self.should_occur(event, game_state)? {
      self.will_occur(event, game_state)?;
      event.process(game_state)?;
      self.did_occur(event, game_state)?;
    }
    Ok(())
  }

  fn should_occur(&self, event: &dyn EventTrait<GameState>, game_state: &mut GameState) -> Result<bool, AnyError> {
    let result = self
      .subscribers
      .iter()
      .all(|s| s.borrow().should_occur(event, game_state) != Some(false));
    Ok(result)
  }

  fn will_occur(&mut self, event: &mut dyn EventTrait<GameState>, game_state: &GameState) -> Result<(), AnyError> {
    self
      .subscribers
      .iter_mut()
      .for_each(|s| s.borrow_mut().will_occur(event, game_state));
    Ok(())
  }

  fn did_occur(&mut self, event: &dyn EventTrait<GameState>, game_state: &mut GameState) -> Result<(), AnyError> {
    self
      .subscribers
      .iter_mut()
      .for_each(|s| s.borrow_mut().did_occur(event, game_state));
    Ok(())
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  use crate::event::EventTrait;
  use crate::event::NoOpEvent;
  use crate::event_subscriber::EventSubscriberTrait;
  use crate::game_state::GameState;

  struct TestEventSubscriber {
    should_occur: Option<bool>,
    will_occur: bool,
    did_occur: bool,
  }

  impl TestEventSubscriber {
    fn new(should_occur: Option<bool>, will_occur: bool, did_occur: bool) -> Self {
      Self {
        should_occur,
        will_occur,
        did_occur,
      }
    }
  }

  impl EventSubscriberTrait<GameState> for TestEventSubscriber {
    fn get_id(&self) -> &str {
      "test_event_subscriber"
    }
    fn should_occur(&self, _event: &dyn EventTrait<GameState>, _game_state: &GameState) -> Option<bool> {
      self.should_occur
    }

    fn will_occur(&mut self, _event: &mut dyn EventTrait<GameState>, _game_state: &GameState) {
      self.will_occur = true;
    }

    fn did_occur(&mut self, _event: &dyn EventTrait<GameState>, _game_state: &mut GameState) {
      self.did_occur = true;
    }
  }

  #[test]
  fn test_add_subscriber() {
    let mut event_publisher = EventPublisher::new();
    let subscriber = Rc::new(RefCell::new(TestEventSubscriber::new(None, false, false)));
    event_publisher.add_subscriber(subscriber);
  }

  #[test]
  fn test_remove_subscriber() {
    let mut event_publisher = EventPublisher::new();
    let subscriber = Rc::new(RefCell::new(TestEventSubscriber::new(None, false, false)));
    event_publisher.add_subscriber(subscriber.clone());
    let _subscriber = event_publisher.remove_subscriber("test_event_subscriber");
  }

  #[test]
  fn test_publish_event() {
    let mut event_publisher = EventPublisher::new();
    let subscriber = Rc::new(RefCell::new(TestEventSubscriber::new(None, false, false)));
    event_publisher.add_subscriber(subscriber.clone());
    let mut event = NoOpEvent::new();
    let mut game_state = GameState::new();
    event_publisher.publish_event(&mut event, &mut game_state).unwrap();
    assert_eq!(subscriber.borrow().will_occur, true);
    assert_eq!(subscriber.borrow().did_occur, true);
  }

  #[test]
  fn test_should_occur() {
    let mut event_publisher = EventPublisher::new();
    let subscriber = Rc::new(RefCell::new(TestEventSubscriber::new(Some(true), false, false)));
    event_publisher.add_subscriber(subscriber.clone());
    let event = NoOpEvent::new();
    let mut game_state = GameState::new();
    let result = event_publisher.should_occur(&event, &mut game_state);
    assert_eq!(result.unwrap(), true);
  }

  #[test]
  fn test_will_occur() {
    let mut event_publisher = EventPublisher::new();
    let subscriber = Rc::new(RefCell::new(TestEventSubscriber::new(None, false, false)));
    event_publisher.add_subscriber(subscriber.clone());
    let mut event = NoOpEvent::new();
    let game_state = GameState::new();
    event_publisher.will_occur(&mut event, &game_state).unwrap();
    assert_eq!(subscriber.borrow().will_occur, true);
  }

  #[test]
  fn test_did_occur() {
    let mut event_publisher = EventPublisher::new();
    let subscriber = Rc::new(RefCell::new(TestEventSubscriber::new(None, false, false)));
    event_publisher.add_subscriber(subscriber.clone());
    let event = NoOpEvent::new();
    let mut game_state = GameState::new();
    event_publisher.did_occur(&event, &mut game_state).unwrap();
    assert_eq!(subscriber.borrow().did_occur, true);
  }

  #[test]
  fn test_should_occur_false() {
    let mut event_publisher = EventPublisher::new();
    let subscriber = Rc::new(RefCell::new(TestEventSubscriber::new(Some(false), false, false)));
    event_publisher.add_subscriber(subscriber.clone());
    let event = NoOpEvent::new();
    let mut game_state = GameState::new();
    let result = event_publisher.should_occur(&event, &mut game_state);
    assert_eq!(result.unwrap(), false);
  }

  #[test]
  fn test_should_occur_true() {
    let mut event_publisher = EventPublisher::new();
    let subscriber = Rc::new(RefCell::new(TestEventSubscriber::new(Some(true), false, false)));
    event_publisher.add_subscriber(subscriber.clone());
    let event = NoOpEvent::new();
    let mut game_state = GameState::new();
    let result = event_publisher.should_occur(&event, &mut game_state);
    assert_eq!(result.unwrap(), true);
  }

  #[test]
  fn test_should_occur_none() {
    let mut event_publisher = EventPublisher::new();
    let subscriber = Rc::new(RefCell::new(TestEventSubscriber::new(None, false, false)));
    event_publisher.add_subscriber(subscriber.clone());
    let event = NoOpEvent::new();
    let mut game_state = GameState::new();
    let result = event_publisher.should_occur(&event, &mut game_state);
    assert_eq!(result.unwrap(), true);
  }
}

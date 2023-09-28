use anyhow::Error as AnyError;
use std::cell::RefCell;
use std::rc::Rc;

use crate::event::EventSubscriberTrait;
use crate::event::EventTrait;
use crate::game_state::GameStateTrait;

/// The `EventPublisher` trait.
pub trait EventPublisher<T: GameStateTrait> {
  /// Add an `EventSubscriber`.
  fn add_subscriber(&mut self, subscriber: Rc<RefCell<dyn EventSubscriberTrait<T>>>);
  /// Remove an `EventSubscriber`.
  fn remove_subscriber(&mut self, subscriber_id: &str) -> Result<Rc<RefCell<dyn EventSubscriberTrait<T>>>, AnyError>;
  /// Publish an `Event`.
  ///
  /// This will query the `EventSubscriber`s to determine whether the event
  /// should occur, and if so, will notify them that the event will occur,
  /// then apply the event to the game state, then notify them that the event
  /// has occurred.
  fn publish_event(&mut self, event: &mut dyn EventTrait<T>, game_state: &mut T) -> Result<(), AnyError>;
  /// Determine whether the event should occur.
  ///
  /// Normally, this will query the `EventSubscriber`s individually to
  /// determine whether the event should occur.
  ///
  /// event is not mutable because it is not expected to change.
  /// game_state is mutable because a rejection may incur a change in the
  /// game state.
  fn should_process(&self, event: &dyn EventTrait<T>, game_state: &mut T) -> Result<bool, AnyError>;
  /// Notify subscribers that the event will occur.
  ///
  /// This is a last chance for subscribers to modify the event, so it is
  /// mutable. This may be used to modify the event to include additional
  /// information, such as the result of a die roll, a penalty, etc.
  ///
  /// This should not, however, modify the game state.
  fn will_process(&mut self, event: &mut dyn EventTrait<T>, game_state: &T) -> Result<(), AnyError>;
  /// Notify subscribers that the event has occurred.
  ///
  /// This is when most subscribers will modify the game state.
  fn did_process(&mut self, event: &dyn EventTrait<T>, game_state: &mut T) -> Result<(), AnyError>;
}

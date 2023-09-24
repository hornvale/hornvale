use specs::shrev::Event as EventTrait;
use specs::shrev::EventChannel;

/// The `GetEventChannel` trait, which allows for getting an event channel.
pub trait GetEventChannel<E: EventTrait> {
  fn get_event_channel(&self) -> &EventChannel<E>;
}

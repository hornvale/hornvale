use specs::shrev::Event as EventTrait;
use specs::shrev::EventChannel;

/// The `GetMutEventChannel` trait, which allows for getting an event channel.
pub trait GetMutEventChannel<E: EventTrait> {
  fn get_mut_event_channel(&mut self) -> &mut EventChannel<E>;
}

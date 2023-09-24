use specs::shrev::Event as EventTrait;

use crate::system_data::GetMutEventChannelTrait;

/// The `WriteEvent` trait, which allows for getting an event channel and
/// writing an event to it.
pub trait WriteEvent<E: EventTrait> {
  fn write_event(&mut self, event: E);
}

impl<T, E> WriteEvent<E> for T
where
  T: GetMutEventChannelTrait<E>,
  E: EventTrait,
{
  fn write_event(&mut self, event: E) {
    self.get_mut_event_channel().single_write(event);
  }
}

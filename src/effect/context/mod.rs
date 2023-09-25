use specs::shrev::Event as EventTrait;
use specs::shrev::EventChannel;

use crate::effect::EffectContextTrait;
use crate::system_data::AllData;
use crate::system_data::GetEventChannelTrait;
use crate::system_data::GetMutEventChannelTrait;

/// The `Context` struct, which represents the context of a effect application.
#[derive(Debug)]
pub struct Context<'context, 'data> {
  /// All data.
  pub data: &'context mut AllData<'data>,
}

impl<'context, 'data> Context<'context, 'data> {
  pub fn new(data: &'context mut AllData<'data>) -> Self {
    Context { data }
  }
}

impl<'context, 'data> EffectContextTrait<'context> for Context<'context, 'data> {
  type Data = AllData<'data>;
  fn get_data(&self) -> &Self::Data {
    self.data
  }

  fn get_data_mut(&mut self) -> &mut Self::Data {
    self.data
  }
}

impl<'context, 'data, T> GetEventChannelTrait<T> for Context<'context, 'data>
where
  T: EventTrait + 'static,
  AllData<'data>: GetEventChannelTrait<T>,
{
  fn get_event_channel(&self) -> &EventChannel<T> {
    let data = self.get_data();
    data.get_event_channel()
  }
}

impl<'context, 'data, T> GetMutEventChannelTrait<T> for Context<'context, 'data>
where
  T: EventTrait + 'static,
  AllData<'data>: GetEventChannelTrait<T>,
  AllData<'data>: GetMutEventChannelTrait<T>,
{
  fn get_mut_event_channel(&mut self) -> &mut EventChannel<T> {
    let data_mut = self.get_data_mut();
    data_mut.get_mut_event_channel()
  }
}

use specs::prelude::*;
use specs::shrev::EventChannel;

use crate::ecs::component::*;
use crate::ecs::event::*;
use crate::ecs::resource::*;

/// The `AllData` type.
///
/// This represents all data available in the ECS world at any time.
///
/// Obviously, this should be used as infrequently as possible, but it is
/// necessary in some scenarios, particularly processing actions and effects.
#[derive(SystemData)]
pub struct AllData<'data> {
  pub entities: Entities<'data>,
}

use mockall::predicate::*;
use mockall::*;
use std::sync::Arc;

use crate::ecs::AllData;
use crate::ecs::AllDataTrait;
use crate::ecs::WriteEffectEventTrait;
use crate::effect::EffectTrait;

/// The `ActionContext` struct, which represents the context of a command
/// execution.
pub trait ActionContextData: AllDataTrait {}

impl ActionContextData for dyn AllDataTrait {}

impl<'data> ActionContextData for AllData<'data> {}

mock! {
  pub ActionContextDataMerged {}
  impl WriteEffectEventTrait for ActionContextDataMerged {
    fn write_effect_event(&mut self, effect: Arc<dyn EffectTrait>);
  }
  impl AllDataTrait for ActionContextDataMerged {}
  impl ActionContextData for ActionContextDataMerged {}
}

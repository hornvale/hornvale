use mockall::predicate::*;
use mockall::*;
use std::sync::Arc;

use crate::action::ActionTrait;
use crate::effect::EffectTrait;
use crate::system_data::AllData;
use crate::system_data::AllDataTrait;
use crate::system_data::WriteActionEventTrait;
use crate::system_data::WriteEffectEventTrait;

/// The `ActionContext` struct, which represents the data in the context of an
/// action execution.
pub trait ActionContextData: AllDataTrait {}

impl ActionContextData for dyn AllDataTrait {}

impl<'data> ActionContextData for AllData<'data> {}

mock! {
  pub ActionContextDataMerged {}
  impl WriteActionEventTrait for ActionContextDataMerged {
    fn write_action_event(&mut self, effect: Arc<dyn ActionTrait>);
  }
  impl WriteEffectEventTrait for ActionContextDataMerged {
    fn write_effect_event(&mut self, effect: Arc<dyn EffectTrait>);
  }
  impl AllDataTrait for ActionContextDataMerged {}
  impl ActionContextData for ActionContextDataMerged {}
}

use mockall::predicate::*;
use mockall::*;
use std::sync::Arc;

use crate::action::ActionTrait;
use crate::effect::EffectTrait;
use crate::system::AllData;
use crate::system::AllDataTrait;
use crate::system::WriteActionEventTrait;
use crate::system::WriteEffectEventTrait;

/// The `EffectContextData` struct, which represents the data in the context
///  of an effect application.
pub trait EffectContextData: AllDataTrait {}

impl EffectContextData for dyn AllDataTrait {}

impl<'data> EffectContextData for AllData<'data> {}

mock! {
  pub EffectContextDataMerged {}
  impl WriteActionEventTrait for EffectContextDataMerged {
    fn write_action_event(&mut self, action: Arc<dyn ActionTrait>);
  }
  impl WriteEffectEventTrait for EffectContextDataMerged {
    fn write_effect_event(&mut self, effect: Arc<dyn EffectTrait>);
  }
  impl AllDataTrait for EffectContextDataMerged {}
  impl EffectContextData for EffectContextDataMerged {}
}

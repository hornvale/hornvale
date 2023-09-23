use mockall::predicate::*;
use mockall::*;
use std::sync::Arc;

use crate::action::ActionTrait;
use crate::command::CommandTrait;
use crate::ecs::AllData;
use crate::ecs::AllDataTrait;
use crate::ecs::WriteActionEventTrait;
use crate::ecs::WriteCommandEventTrait;
use crate::ecs::WriteEffectEventTrait;
use crate::effect::EffectTrait;

/// The `CommandContext` struct, which represents the data in the context of a
/// command execution.
pub trait CommandContextData: AllDataTrait {}

impl CommandContextData for dyn AllDataTrait {}

impl<'data> CommandContextData for AllData<'data> {}

mock! {
  pub CommandContextDataMerged {}
  impl WriteActionEventTrait for CommandContextDataMerged {
    fn write_action_event(&mut self, action: Arc<dyn ActionTrait>);
  }
  impl WriteCommandEventTrait for CommandContextDataMerged {
    fn write_command_event(&mut self, command: Arc<dyn CommandTrait>);
  }
  impl WriteEffectEventTrait for CommandContextDataMerged {
    fn write_effect_event(&mut self, effect: Arc<dyn EffectTrait>);
  }
  impl AllDataTrait for CommandContextDataMerged {}
  impl CommandContextData for CommandContextDataMerged {}
}

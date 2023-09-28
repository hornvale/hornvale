use crate::action::ActionTrait;
use crate::command::CommandTrait;
use crate::effect::EffectTrait;
use crate::event::EventTrait;
use crate::game_state::GameStateTrait;

/// The `TraceTreeNodeType` enum.
#[derive(Clone, Debug, Display, PartialEq)]
pub enum NodeType {
  Action(String),
  Command(String),
  Effect(String),
  Event(String),
}

impl NodeType {
  /// Creates a new `TraceTreeNodeType::Action`.
  pub fn from_action<T: GameStateTrait>(action: &dyn ActionTrait<T>) -> Self {
    Self::Action(action.get_name().to_string())
  }

  /// Creates a new `TraceTreeNodeType::Command`.
  pub fn from_command<T: GameStateTrait>(command: &dyn CommandTrait<T>) -> Self {
    Self::Command(command.get_name().to_string())
  }

  /// Create a new `TraceTreeNodeType::Effect`.
  pub fn from_effect<T: GameStateTrait>(effect: &dyn EffectTrait<T>) -> Self {
    Self::Effect(effect.get_name().to_string())
  }

  /// Creates a new `TraceTreeNodeType::Event`.
  pub fn from_event<T: GameStateTrait>(event: &dyn EventTrait<T>) -> Self {
    Self::Event(event.get_name().to_string())
  }

  /// Gets the name of this `TraceTreeNodeType`.
  pub fn get_name(&self) -> &str {
    match self {
      Self::Action(name) => name,
      Self::Command(name) => name,
      Self::Effect(name) => name,
      Self::Event(name) => name,
    }
  }

  /// Get the kind of this `TraceTreeNodeType`.
  pub fn get_kind(&self) -> &'static str {
    match self {
      Self::Action(_) => "Action",
      Self::Command(_) => "Command",
      Self::Effect(_) => "Effect",
      Self::Event(_) => "Event",
    }
  }
}

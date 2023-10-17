use crate::command::Command as CommandObject;

/// The `CommandEvent` type.
///
/// This represents a command event.
#[derive(Clone, Debug, Derivative)]
pub struct Command {
  pub command: CommandObject,
}

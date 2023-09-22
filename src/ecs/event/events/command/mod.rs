use std::sync::Arc;

use crate::command::Command as CommandTrait;

/// The `CommandEvent` type.
///
/// This represents a command executed by a player.
#[derive(Clone, Debug)]
pub struct Command {
  pub command: Arc<dyn CommandTrait>,
}

use crate::prelude::{CommandContext, CommandError};
use hecs::World;

/// A command execute() function type.
pub type CommandFunction = fn(&mut World, &CommandContext) -> Result<(), CommandError>;

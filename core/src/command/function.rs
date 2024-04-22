use anyhow::Error as AnyError;
use hecs::{Entity, World};

/// A command execute() function type.
pub type CommandFunction = fn(&mut World, &Entity) -> Result<(), AnyError>;

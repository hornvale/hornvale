use crate::prelude::CommandError;
use hecs::{Entity, World};

/// A command execute() function type.
pub type CommandFunction = fn(&mut World, Entity, Option<Entity>, Option<Entity>) -> Result<(), CommandError>;

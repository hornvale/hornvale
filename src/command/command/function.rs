use crate::database::prelude::*;
use anyhow::Error as AnyError;
use hecs::Entity;

/// A command execute() function type.
pub type CommandFunction = fn(&mut Database, Entity, Option<Entity>, Option<Entity>) -> Result<(), AnyError>;

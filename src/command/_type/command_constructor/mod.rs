use crate::command::CommandTrait;

/// The `CommandConstructor` type.
pub type CommandConstructor = Box<dyn Fn() -> Box<dyn CommandTrait> + Send>;

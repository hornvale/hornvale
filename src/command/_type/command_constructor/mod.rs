use crate::command::_trait::Command;

/// The `CommandConstructor` type.
pub type CommandConstructor = Box<dyn Fn() -> Box<dyn Command> + Send>;

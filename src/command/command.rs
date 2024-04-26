use crate::command::prelude::*;
use crate::database::prelude::*;
use anyhow::Error as AnyError;
use hecs::Entity;

/// The "arity" of a command; how many arguments it takes.
pub mod arity;
/// A type definition for a function that executes a command.
pub mod function;
/// A modifier (adverb, preposition) that can be applied to a verb.
pub mod modifier;
/// A syntax form for a command, defining its requirements.
pub mod syntax;

/// A command that can be executed.
pub trait Command {
  /// Get the base name of the command. This is the name that the player will
  /// use to invoke the command.
  const NAME: &'static str;
  /// Get synonyms for the command. These are other names that the player can
  /// use to invoke the command.
  const SYNONYMS: &'static [&'static str];
  /// A brief description of the command. This is used in the help text.
  const BRIEF: &'static str;
  /// A longer description of the command. This is used in the help text.
  const DESCRIPTION: &'static str;
  /// Get the arity of the command: how many and what types of arguments the
  /// command takes.
  const ARITY: CommandArity = CommandArity::Nullary;
  /// Get the modifier for the direct object, if any.
  const DIRECT_OBJECT_MODIFIER: Option<CommandModifier> = None;
  /// Get the modifier for the indirect object, if any.
  const INDIRECT_OBJECT_MODIFIER: Option<CommandModifier> = None;
  /// Get the syntax form of the command.
  const SYNTAX: CommandSyntax = CommandSyntax {
    arity: Self::ARITY,
    direct_object_modifier: Self::DIRECT_OBJECT_MODIFIER,
    indirect_object_modifier: Self::INDIRECT_OBJECT_MODIFIER,
  };

  /// Execute the command.
  fn execute(
    database: &mut Database,
    actor: Entity,
    direct_object: Option<Entity>,
    indirect_object: Option<Entity>,
  ) -> Result<(), AnyError>;
}

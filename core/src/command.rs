use anyhow::Error as AnyError;
use hecs::{Entity, World};

/// The arity, or number of arguments, that a command can take.
pub mod arity;
use arity::CommandArity;
/// The forms that a command can take.
pub mod modifier;
use modifier::CommandModifier;
/// A definition of a command function signature.
pub mod function;
/// A raw command string.
pub mod string;
/// Command syntax.
pub mod syntax;
use syntax::CommandSyntax;

/// A command that can be executed.
///
/// A command's syntax is a tuple of the following:
/// - the verb, or NAME, of the command.
/// - the form,
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
  /// Get the arity of the command. This informs the parser how many arguments
  /// the command can take.
  const ARITY: CommandArity;
  /// Get the modifier of the direct object.
  const DIRECT_OBJECT_MODIFIER: CommandModifier;
  /// Get the modifier of the indirect object.
  const INDIRECT_OBJECT_MODIFIER: CommandModifier;
  /// The syntax of the command.
  const SYNTAX: CommandSyntax = CommandSyntax(
    Self::ARITY,
    Self::DIRECT_OBJECT_MODIFIER,
    Self::INDIRECT_OBJECT_MODIFIER,
  );

  /// Execute the command.
  fn execute(world: &mut World, direct_object: Option<Entity>, indirect_object: Option<Entity>)
    -> Result<(), AnyError>;
}

use crate::command::prelude::*;
use anyhow::Error as AnyError;
use hecs::{Entity, World};

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
    world: &mut World,
    actor: Entity,
    direct_object: Option<Entity>,
    indirect_object: Option<Entity>,
  ) -> Result<(), AnyError>;
}

/// Macro to define a command variant with minimal repetition.
#[macro_export]
macro_rules! define_command_variant {
  ($variant:ident, $original:ty, $dom:expr, $iom:expr) => {
    pub struct $variant;

    impl hornvale_command::prelude::Command for $variant {
      const NAME: &'static str = <$original as Command>::NAME;
      const SYNONYMS: &'static [&'static str] = <$original as Command>::SYNONYMS;
      const BRIEF: &'static str = <$original as Command>::BRIEF;
      const DESCRIPTION: &'static str = <$original as Command>::DESCRIPTION;
      const ARITY: CommandArity = <$original as Command>::ARITY;
      const DIRECT_OBJECT_MODIFIER: Option<CommandModifier> = $dom;
      const INDIRECT_OBJECT_MODIFIER: Option<CommandModifier> = $iom;

      fn execute(
        world: &mut World,
        actor: Entity,
        direct_object: Option<Entity>,
        indirect_object: Option<Entity>,
      ) -> Result<(), AnyError> {
        <$original as Command>::execute(world, actor, direct_object, indirect_object)
      }
    }
  };
}

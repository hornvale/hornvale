use anyhow::Error as AnyError;
use hecs::{Entity, World};

/// The forms that a command can take.
pub mod form;
use form::CommandForm;
/// A definition of a command function signature.
pub mod function;

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
  /// Get the form of the command. This informs the parser how to match input
  /// to the appropriate command.
  const FORM: CommandForm;

  /// Execute the command.
  fn execute(world: &mut World, context: &Entity) -> Result<(), AnyError>;
}

/// Macro to define a command variant with minimal repetition.
#[macro_export]
macro_rules! define_command_variant {
  ($variant:ident, $original:ty, $form:expr) => {
    pub struct $variant;

    impl hornvale_core::prelude::Command for $variant {
      const NAME: &'static str = <$original as Command>::NAME;
      const SYNONYMS: &'static [&'static str] = <$original as Command>::SYNONYMS;
      const BRIEF: &'static str = <$original as Command>::BRIEF;
      const DESCRIPTION: &'static str = <$original as Command>::DESCRIPTION;
      const FORM: CommandForm = $form;

      fn execute(world: &mut World, context: &Entity) -> Result<(), AnyError> {
        <$original as Command>::execute(world, context)
      }
    }
  };
}

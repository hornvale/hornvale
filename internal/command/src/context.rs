use crate::prelude::{CommandArgument, CommandModifier};
use hecs::Entity;
use serde::{Deserialize, Serialize};

/// The command context.
#[derive(Clone, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
pub struct CommandContext {
  /// The original command string.
  ///
  /// This is the string that the player typed in to invoke the command. We
  /// provide it here in case the command needs to be re-parsed or re-processed
  /// in some way, or if the command needs to be echoed back to the player.
  pub raw: String,
  /// The verb of the command.
  ///
  /// This is the first word of the command string, which is typically the verb
  /// that the player is trying to perform. For example, in the command "take
  /// sword", the verb is "take".
  ///
  /// This may not always be the case, however; some commands have synonyms or
  /// aliases that are not verbs, etc.
  pub verb: String,
  /// The command's actor.
  ///
  /// This is the entity that is performing the command. It is typically the
  /// player character, but it could be any entity in the game world.
  pub actor: Option<Entity>,
  /// The form of the command.
  ///
  /// This is the modifier that can be applied to the verb. For example, in the
  /// command "look behind the curtain", the form is "behind".
  pub form: CommandModifier,
  /// The direct object, if any.
  ///
  /// This is the object that the player is trying to interact with. For example,
  /// in the command "take sword", the direct object is "sword".
  pub direct_object: Option<CommandArgument>,
  /// The indirect object, if any.
  ///
  /// This is the object that the player is trying to interact with, but which is
  /// not the primary object of the command. For example, in the command "give
  /// sword to guard", the direct object is "sword" and the indirect object is
  /// "guard".
  pub indirect_object: Option<CommandArgument>,
}

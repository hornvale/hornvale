use crate::prelude::*;
use derive_more::Display;
use serde::{Deserialize, Serialize};

/// A syntax form for a command, defining its requirements.
#[derive(Clone, Copy, Debug, Display, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[display(
  fmt = "arity = {:#?}, direct_object_modifier = {:#?}, indirect_object_modifier = {:#?}",
  arity,
  direct_object_modifier,
  indirect_object_modifier
)]
pub struct CommandSyntax {
  /// The command's arity.
  pub arity: CommandArity,
  /// The command's direct object modifier.
  pub direct_object_modifier: Option<CommandModifier>,
  /// The command's indirect object modifier.
  pub indirect_object_modifier: Option<CommandModifier>,
}

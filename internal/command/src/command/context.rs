use hecs::Entity;
use serde::{Deserialize, Serialize};

/// The command context.
#[derive(Clone, Copy, Debug, Default, Deserialize, Serialize)]
pub struct CommandContext {
  /// The direct object, if any.
  pub direct_object: Option<Entity>,
  /// The indirect object, if any.
  pub indirect_object: Option<Entity>,
}

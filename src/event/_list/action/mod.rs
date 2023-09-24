use std::sync::Arc;

use crate::action::ActionTrait;

/// The `ActionEvent` type.
///
/// This represents an action executed by an entity.
#[derive(Clone, Debug)]
pub struct Action {
  pub action: Arc<dyn ActionTrait>,
}

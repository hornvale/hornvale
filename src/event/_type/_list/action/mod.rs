use crate::action::Action as ActionObject;

/// The `ActionEvent` type.
///
/// This represents an action event.
#[derive(Clone, Debug, Derivative)]
pub struct Action {
  pub action: ActionObject,
}

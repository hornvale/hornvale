use derive_more::Display;

use crate::entity_uuid::ActorUuid;

/// The `Actor` type.
#[derive(Builder, Clone, Debug, Default, Deserialize, Display, Eq, Hash, PartialEq, Serialize)]
pub struct Actor {
  /// The `Actor`'s UUID.
  #[builder(default = "ActorUuid::default()")]
  pub uuid: ActorUuid,
}

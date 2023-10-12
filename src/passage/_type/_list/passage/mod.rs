use uuid::Uuid;

use crate::passage::PassageDirection;
use crate::passage::PassageKind;
use crate::passage::PassageStatus;
use crate::passage::PassageTarget;

/// The `Passage` struct.
#[derive(Builder, Clone, Debug, Deserialize, Hash, PartialEq, Serialize)]
#[builder(derive(Debug))]
pub struct Passage {
  /// The `Passage`'s seed string.
  pub seed_string: String,
  /// The UUID of the passage.
  #[builder(default = "Uuid::new_v4().to_string()")]
  pub uuid: String,
  /// The `Passage`'s status (to be implemented)
  pub status: PassageStatus,
  /// The `Passage`'s direction (e.g. North, South, East, West)
  pub direction: PassageDirection,
  /// The `Passage`'s kind (e.g. Path, Door, WideOpen)
  pub kind: PassageKind,
  /// The `Passage`'s target (e.g. Room, Passage, Message)
  pub target: PassageTarget,
  /// The `Passage`'s name.
  pub name: String,
  /// The `Passage`'s description.
  pub description: String,
}

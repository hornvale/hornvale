use crate::passage::PassageTarget;

/// The `PassageResolver` enum.
#[derive(Clone, Debug, Default, Deserialize, Display, PartialEq, Serialize)]
pub enum PassageResolver {
  /// This passage's resolver is unknown.
  #[default]
  Unknown,
  /// This passage's resolver is a fixed value.
  Fixed(Box<PassageTarget>),
  /// This passage's resolver is a random value from a list.
  Random(Vec<PassageTarget>),
}

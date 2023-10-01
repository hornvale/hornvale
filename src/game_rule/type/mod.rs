/// The `GameRuleType` enum.
#[derive(Clone, Debug, Deserialize, Eq, Hash, PartialEq, PartialOrd, Serialize)]
pub enum Type {
  NoWalkingThroughWalls,
  NoTeleporting,
}

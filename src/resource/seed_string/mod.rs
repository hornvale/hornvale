/// The `SeedString` resource.
///
/// This maintains the seed string for the game.
///
/// This is used to construct the general random number generator for the game.
///
/// However, we also want game behavior to be consistent for a given seed,
/// regardless of the order in which systems are run. To do this, we will
/// construct different generators for certain systems, and we will use the
/// original seed string, modified in some deterministic way, to seed those
/// generators.
#[derive(Clone, Debug, Default, Deserialize, Eq, Hash, PartialEq, Serialize)]
#[repr(transparent)]
pub struct SeedString(pub String);

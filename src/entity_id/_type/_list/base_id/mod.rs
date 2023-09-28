/// The `BaseId` type.
///
/// We do this so that we can perform some compile-time type-checking with IDs.
#[derive(Clone, Copy, Debug, Default, Deserialize, Display, Eq, Hash, PartialEq, Ord, PartialOrd, Serialize)]
#[repr(transparent)]
pub struct BaseId(u64);

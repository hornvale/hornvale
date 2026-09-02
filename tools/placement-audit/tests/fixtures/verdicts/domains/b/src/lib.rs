/// The other side of `Mood` (`domains/a`) — tagged, but with a fingerprint
/// that no longer matches the type's current member set.
///
/// placement: deliberate(kept apart on purpose, for the fixture) shape(000000)
pub enum Temper {
    /// First.
    Alpha,
    /// Second.
    Beta,
    /// Third.
    Gamma,
}

/// The other side of `Grade` (`domains/a`) — deliberately left untagged.
pub enum Rank {
    /// First.
    One,
    /// Second.
    Two,
}

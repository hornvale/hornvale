/// A three-valued example, tagged and current.
///
/// placement: deliberate(kept apart on purpose, for the fixture) shape(6e79e6)
pub enum Mood {
    /// First.
    Alpha,
    /// Second.
    Beta,
    /// Third.
    Gamma,
}

/// A two-valued example, tagged and current — the other side of `Rank`
/// (`domains/b`), which is deliberately left untagged.
///
/// placement: deliberate(kept apart on purpose, for the fixture) shape(e23a3d)
pub enum Grade {
    /// First.
    One,
    /// Second.
    Two,
}

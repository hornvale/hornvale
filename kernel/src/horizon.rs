//! The stratigraphic rock ladder, shared by every domain that names it
//! (decision 0517 clause (a)). Before this module the five bands carried
//! two names: `Horizon` in `domains/terrain` (which derives them from a
//! column) and a mirrored rock half inside `domains/climate::Stratum`,
//! bridged by `windows/worldgen`'s `stratum_of_band` identity match.
//! Climate does not compute a horizon — it cannot import terrain (decision
//! 0002) — so the mirror was a forced duplicate, the same filing error
//! `kernel/src/band.rs` corrected for the delve ladder.
//!
//! **This module holds the roster and the ordering, and nothing else.**
//! Which band a depth falls in (`band_at_depth`), the column that stamps
//! them, and every readout spelling stay in `hornvale_terrain`.

/// A named band of the rock column, top → bottom; resolution coarsens
/// downward. Ordered shallow → deep, so a **greater** horizon is a
/// **deeper** one — the derived `Ord` replaces the ad-hoc ordinal tables
/// callers kept for themselves.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Horizon {
    /// The living skin: soil / weathered regolith.
    Regolith,
    /// Deposited / volcanic surface rock — the legible archive.
    Cover,
    /// Crystalline craton (terrain's inherited `Basement`).
    Basement,
    /// Deep crust: hot, high-pressure.
    Roots,
    /// The primordial substrate / threshold to the not-here.
    Underneath,
}

/// Every horizon, shallowest to deepest.
const ALL: [Horizon; 5] = [
    Horizon::Regolith,
    Horizon::Cover,
    Horizon::Basement,
    Horizon::Roots,
    Horizon::Underneath,
];

impl Horizon {
    /// Every horizon of the ladder in order, shallowest first.
    pub fn all() -> &'static [Horizon] {
        &ALL
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// The derived Ord runs shallow → deep, so depth comparisons read the
    /// way the rock does.
    #[test]
    fn the_ladder_is_ordered_shallow_first() {
        let mut sorted = ALL;
        sorted.sort();
        assert_eq!(sorted, ALL);
        assert!(Horizon::Regolith < Horizon::Underneath);
    }
}

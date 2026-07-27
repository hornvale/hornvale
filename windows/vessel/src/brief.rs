//! The BRIEF: the one thing micro generation may read about a place besides its
//! address and the seed (Rose Window metaplan §1b.4). Macro answers *who holds
//! this land*; micro answers *what is standing here*; the brief is the seam.
//!
//! It is derived, never stored. Fields this campaign does not read yet — the
//! ruin signature (`cause`, `ended_by`, ages) and the district vocabulary — are
//! carried from the start so that adding a consumer never changes the seam.

use hornvale_history::record::{Function, Notability, TechHorizon};
use hornvale_kernel::{CellId, Geosphere, KindId, NearestCellIndex, RoomAddr, World};

/// What macro history says about a place, reduced to the axes micro generation
/// indexes. A COORDINATE in a small orthogonal space — never a label drawn from
/// a catalogue of place types (§1b.4).
/// type-audit: bare-ok(flag: built), bare-ok(flag: cold)
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Brief {
    /// What the alive occupation here was for, if any occupation is alive.
    pub function: Option<Function>,
    /// The alive occupation's technological horizon.
    pub tech: Option<TechHorizon>,
    /// How notable the alive occupation is in its region.
    pub notability: Option<Notability>,
    /// The people occupying this place, if any.
    pub people: Option<KindId>,
    /// Whether a structure stands here — `Terrain::is_built` at the WALK band.
    pub built: bool,
    /// Whether warmth matters here — `Terrain::is_cold` at the WALK band.
    pub cold: bool,
}

impl Brief {
    /// Assemble a brief from already-resolved parts. Exists so the type can be
    /// unit-tested without a world; `brief_of` is the production path.
    /// type-audit: bare-ok(flag: built), bare-ok(flag: cold)
    pub fn from_parts(
        function: Option<Function>,
        tech: Option<TechHorizon>,
        notability: Option<Notability>,
        people: Option<KindId>,
        built: bool,
        cold: bool,
    ) -> Self {
        Self {
            function,
            tech,
            notability,
            people,
            built,
            cold,
        }
    }
}

/// The geosphere cell a place sits in: the maximum-weight corner of its
/// barycentric blend, tie-broken by ascending `CellId`.
///
/// Integer weights only (`corner_weights` returns `u64` numerators), so the
/// choice is cross-platform exact — no float comparison enters world identity.
/// Returns `None` for a place coarser than the canonical grid.
fn containing_cell(place: &RoomAddr, geo: &Geosphere, index: &NearestCellIndex) -> Option<CellId> {
    let weights = place.corner_weights(geo, index)?;
    weights
        .iter()
        .max_by(|a, b| a.1.cmp(&b.1).then(b.0.0.cmp(&a.0.0)))
        .map(|&(cell, _)| cell)
}

/// Derive the brief for `place`. Every read is taken at the walk band, so a
/// chamber and its locale yield the same brief — which is what makes a
/// structure's chambers agree about what building they are in.
/// type-audit: bare-ok(count: walk_depth)
pub fn brief_of(
    world: &World,
    geo: &Geosphere,
    index: &NearestCellIndex,
    place: &RoomAddr,
    terrain: &dyn crate::liveness::Terrain,
    walk_depth: u32,
) -> Brief {
    let locale = crate::band::truncate_to_walk(place, walk_depth);
    let built = terrain.is_built(&locale);
    let cold = terrain.is_cold(&locale);
    let alive = containing_cell(&locale, geo, index)
        .and_then(|cell| {
            // NOTE ON COST: this derives the whole per-cell occupation map on
            // every call. Correct but wasteful, and `brief_of` will be called
            // per descent. If a profile shows it mattering, hoist the map to
            // the caller (the session can hold it for the possession's life) —
            // do NOT memoize inside this function, because a hidden cache in a
            // derivation path is how derived state stops being derived.
            hornvale_worldgen::occupations_by_cell(world).remove(&cell)
        })
        .and_then(|occs| occs.into_iter().find(|o| o.ended.is_none()));
    match alive {
        Some(o) => Brief::from_parts(
            Some(o.function),
            Some(o.tech),
            Some(o.notability),
            Some(o.people),
            built,
            cold,
        ),
        None => Brief::from_parts(None, None, None, None, built, cold),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_history::record::{Function, Notability, TechHorizon};

    #[test]
    fn the_alive_occupation_supplies_the_briefs_axes() {
        let b = Brief::from_parts(
            Some(Function::Trade),
            Some(TechHorizon::Classical),
            Some(Notability::Seat),
            None,
            true,
            true,
        );
        assert_eq!(b.function, Some(Function::Trade));
        assert_eq!(b.tech, Some(TechHorizon::Classical));
        assert_eq!(b.notability, Some(Notability::Seat));
        assert!(b.built);
        assert!(b.cold);
    }

    #[test]
    fn an_unbuilt_place_has_an_empty_brief_but_still_reports_climate() {
        let b = Brief::from_parts(None, None, None, None, false, true);
        assert!(!b.built);
        assert!(
            b.cold,
            "climate is a property of the place, not of a people"
        );
        assert!(b.function.is_none());
    }

    #[test]
    fn the_brief_is_a_coordinate_not_a_label() {
        // §1b.4: patterns index the CROSS-PRODUCT of axes. Two briefs sharing
        // a function but differing in tech must not compare equal, or the
        // vocabulary would collapse into a catalogue of place types.
        let a = Brief::from_parts(
            Some(Function::Fort),
            Some(TechHorizon::Neolithic),
            None,
            None,
            true,
            false,
        );
        let b = Brief::from_parts(
            Some(Function::Fort),
            Some(TechHorizon::Classical),
            None,
            None,
            true,
            false,
        );
        assert_ne!(a, b);
    }
}

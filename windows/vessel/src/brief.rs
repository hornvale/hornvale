//! The BRIEF: the one thing micro generation may read about a place besides its
//! address and the seed (Rose Window metaplan §1b.4). Macro answers *who holds
//! this land*; micro answers *what is standing here*; the brief is the seam.
//!
//! It is derived, never stored — which is why it does NOT carry the fields no
//! consumer reads yet. The ruin signature (`cause`, `ended_by`, ages) and the
//! district vocabulary are absent on purpose: the metaplan argued for carrying
//! them from the start "so that adding a consumer never changes the seam", but
//! that argument only bites for types that PERSIST. Nothing here is serialized,
//! so the campaign that first needs `cause` adds one field, with no save-format
//! consequence and no epoch. Seven unread `Option`s would be dead weight that
//! reads as evidence of intent.
//!
//! FOUR fields are read as of The Blocking: `built`, in `structure_at`'s
//! existence predicate and in `describe_chamber`'s room/hollow word;
//! `notability` and `function`, in `pattern::role_for`'s promotion of a deep
//! chamber; and `peak_population`, added here when the `store` role's strongbox
//! became its first reader — exactly the "one field, no epoch" this doc licenses.
//! `cold` is carried but read only by a debug assertion (`chamber_interior_of`
//! cross-checks it against the terrain), and `tech` and `people` are carried and
//! not read at all.

use hornvale_history::record::{Function, Notability, TechHorizon};
use hornvale_kernel::{CellId, Geosphere, KindId, NearestCellIndex, RoomAddr, World};

/// What macro history says about a place, reduced to the axes micro generation
/// indexes. A COORDINATE in a small orthogonal space — never a label drawn from
/// a catalogue of place types (§1b.4).
/// type-audit: bare-ok(flag: built), bare-ok(flag: cold), bare-ok(count: peak_population)
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
    /// The highest population the alive occupation ever reached, `0` where none
    /// is alive. Not an `Option`: "nobody lives here" and "nobody ever did" are
    /// the same answer to the one question anything asks of this field, which is
    /// [`Self::is_populous`].
    pub peak_population: u32,
    /// Whether a structure stands here — `Terrain::is_built` at the WALK band.
    pub built: bool,
    /// Whether warmth matters here — `Terrain::is_cold` at the WALK band.
    pub cold: bool,
}

impl Brief {
    /// Assemble a brief from already-resolved parts. Exists so the type can be
    /// unit-tested without a world; `brief_of` is the production path.
    /// type-audit: bare-ok(flag: built), bare-ok(flag: cold), bare-ok(count: peak_population)
    pub fn from_parts(
        function: Option<Function>,
        tech: Option<TechHorizon>,
        notability: Option<Notability>,
        people: Option<KindId>,
        peak_population: u32,
        built: bool,
        cold: bool,
    ) -> Self {
        Self {
            function,
            tech,
            notability,
            people,
            peak_population,
            built,
            cold,
        }
    }

    /// Whether this place ever held more people than a hamlet.
    ///
    /// Reads `hornvale_history::flesh::HAMLET_POPULATION_CEILING` rather than a
    /// literal, and it is the SAME threshold the ruin model reads for whether a
    /// place leaves a child's doll behind — a hamlet is a family place in both
    /// readings. The vessel's use is [`crate::interior::pattern::Pattern::
    /// needs_populous`]: the strongbox.
    /// type-audit: bare-ok(flag: return)
    pub fn is_populous(&self) -> bool {
        self.peak_population > hornvale_history::flesh::HAMLET_POPULATION_CEILING
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
        .and_then(|occs| occs.into_iter().find(|o| o.core.ended.is_none()));
    match alive {
        Some(o) => Brief::from_parts(
            Some(o.core.function),
            Some(o.core.tech),
            Some(o.core.notability),
            Some(o.core.people),
            o.core.peak_population,
            built,
            cold,
        ),
        None => Brief::from_parts(None, None, None, None, 0, built, cold),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_history::record::{Function, Notability, TechHorizon};

    #[test]
    fn from_parts_assigns_the_occupation_axes_and_flags() {
        let b = Brief::from_parts(
            Some(Function::Trade),
            Some(TechHorizon::Classical),
            Some(Notability::Seat),
            None,
            900,
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
    fn from_parts_with_no_occupation_axes_still_carries_climate() {
        let b = Brief::from_parts(None, None, None, None, 0, false, true);
        assert!(!b.built);
        assert!(
            b.cold,
            "climate is a property of the place, not of a people"
        );
        assert!(b.function.is_none());
    }

    #[test]
    fn two_briefs_differing_only_in_tech_are_not_equal() {
        // §1b.4: patterns index the CROSS-PRODUCT of axes. Two briefs sharing
        // a function but differing in tech must not compare equal, or the
        // vocabulary would collapse into a catalogue of place types.
        let a = Brief::from_parts(
            Some(Function::Fort),
            Some(TechHorizon::Neolithic),
            None,
            None,
            0,
            true,
            false,
        );
        let b = Brief::from_parts(
            Some(Function::Fort),
            Some(TechHorizon::Classical),
            None,
            None,
            0,
            true,
            false,
        );
        assert_ne!(a, b);
    }
}

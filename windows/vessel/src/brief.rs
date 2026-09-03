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
//! THREE fields are read as of decision 0398: `built`, in `structure_at`'s
//! existence predicate and in `describe_chamber`'s room/hollow word; and
//! `notability` and `function`, in `pattern::role_for`'s promotion of a deep
//! chamber. `cold` is carried but read only by a debug assertion
//! (`chamber_interior_of` cross-checks it against the terrain), and `tech` and
//! `people` are carried and not read at all.
//!
//! `peak_population` was the FOURTH, added here when the `store` role's
//! strongbox became its first reader — "exactly the one field, no epoch this
//! doc licenses", as this paragraph used to say. Decision 0398 relaxed that
//! gate, so the field is now read only by [`Brief::is_populous`], whose value
//! still reaches `pattern::selection_for` on every chamber derivation and
//! currently selects nothing. It is kept for the same reason the doc above
//! gives for keeping the seam thin: removing it would be a second edit to undo
//! the day a population-gated pattern is written, and unlike the seven absent
//! `Option`s this one has a live wire behind it.

use hornvale_history::record::{Function, Notability, OccupationRecord, TechHorizon};
use hornvale_kernel::{Facet, Geosphere, KindId, NearestVertexIndex, Vertex};
use std::collections::BTreeMap;

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
    /// readings.
    ///
    /// **NO VESSEL-SIDE CONSUMER SELECTS ON IT TODAY (decision 0398), and this
    /// doc used to say otherwise.** It read "the vessel's use is
    /// [`crate::interior::pattern::Pattern::needs_populous`]: the strongbox",
    /// which was true when the strongbox was population-gated and false the
    /// moment that gate was relaxed. The predicate is still WIRED —
    /// [`crate::interior::chamber_interior_of`] passes it into
    /// `pattern::selection_for` on every chamber derivation — but no authored
    /// pattern sets `needs_populous`, so it currently selects nothing. Wired
    /// and idle, not dead: the wiring is what lets a future population-gated
    /// pattern work on the day it is written.
    ///
    /// The reason the gate came off is worth carrying here rather than only in
    /// the decision, because this is where the number lives: across three
    /// worlds and a 48-seed sweep, **not one living occupation clears the
    /// ceiling** (max alive peak 84–87 against 150). So this predicate is false
    /// everywhere a session can currently stand, and anything gated on it is
    /// not rare but absent.
    /// type-audit: bare-ok(flag: return)
    pub fn is_populous(&self) -> bool {
        self.peak_population > hornvale_history::flesh::HAMLET_POPULATION_CEILING
    }
}

/// The geosphere vertex a place sits in: the maximum-weight corner of its
/// four-corner bilinear blend, tie-broken by ascending `Vertex` (a
/// three-corner barycentric blend before The Pavement).
///
/// Integer weights only (`corner_weights` returns `u64` numerators), so the
/// choice is cross-platform exact — no float comparison enters world identity.
/// Returns `None` for a place coarser than the canonical grid.
///
/// `pub(crate)` since The Lantern, which needs the same vertex to read the ground
/// a building's fabric is derived from. Shared rather than re-derived on
/// purpose: a second copy of this rule is exactly how a room's *prose* ("granite
/// lowland") and its *picture* would come to disagree about which ground it
/// stands on.
pub(crate) fn containing_vertex(
    place: &Facet,
    geo: &Geosphere,
    index: &NearestVertexIndex,
) -> Option<Vertex> {
    let weights = place.corner_weights(geo, index)?;
    weights
        .iter()
        .max_by(|a, b| a.1.cmp(&b.1).then(b.0.0.cmp(&a.0.0)))
        .map(|&(vertex, _)| vertex)
}

/// Derive the brief for `place`. Every read is taken at the walk band, so a
/// chamber and its locale yield the same brief — which is what makes a
/// structure's chambers agree about what building they are in.
///
/// `occupations` is the world's occupation register,
/// `hornvale_worldgen::occupations_by_vertex(world)`, built ONCE by the
/// caller (`WorldContext::build`) and handed in. **History of this
/// parameter, kept because the note it replaces was right for five weeks
/// before anyone measured it:** from `4569d883d` (2026-07-27) to The Terrier
/// (2026-09-03) this function took `&World` and rebuilt the whole map on
/// every call, under a `NOTE ON COST` that said "if a profile shows it
/// mattering, hoist the map to the caller … do NOT memoize inside this
/// function, because a hidden cache in a derivation path is how derived
/// state stops being derived." The profile showed 8.7-26 ms per call and
/// two to five calls per indoor turn — the whole of what The Rack had
/// attributed to a 0.012 ms shadowcast. The note's prescription is what
/// shipped, and its prohibition still stands: there is no cache here, only
/// a parameter.
/// type-audit: bare-ok(count: walk_depth)
pub fn brief_of(
    occupations: &BTreeMap<Vertex, Vec<OccupationRecord>>,
    geo: &Geosphere,
    index: &NearestVertexIndex,
    place: &Facet,
    terrain: &dyn crate::liveness::Terrain,
    walk_depth: u32,
) -> Brief {
    let locale = crate::depth::truncate_to_walk(place, walk_depth);
    let built = terrain.is_built(&locale);
    let cold = terrain.is_cold(&locale);
    let alive = containing_vertex(&locale, geo, index)
        .and_then(|vertex| occupations.get(&vertex))
        .and_then(|occs| occs.iter().find(|o| o.core.ended.is_none()));
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

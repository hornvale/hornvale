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

use crate::site::{Site, SiteKind};
use hornvale_history::record::{Function, Notability, TechHorizon};
use hornvale_kernel::{Facet, Geosphere, KindId, NearestVertexIndex, Vertex, World};
use hornvale_locale::StrangeSite;
use hornvale_worldgen::{SiteReason, site_facet_for};

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
    /// The site here, if any — the gate every enterable place hangs off.
    /// Decision 0536. For a settlement this mirrors [`Self::built`]; an exotic
    /// site and a cave are each the placed address
    /// `hornvale_worldgen::site_facet_for` gives them, under their own
    /// `hornvale_worldgen::SiteReason` so that a vertex warranting both does
    /// not put them at one facet.
    pub site: Option<Site>,
}

impl Brief {
    /// Assemble a brief from already-resolved parts. Exists so the type can be
    /// unit-tested without a world; `brief_of` is the production path.
    /// type-audit: bare-ok(flag: built), bare-ok(flag: cold), bare-ok(count: peak_population)
    #[allow(clippy::too_many_arguments)] // `site` (Task 2, The Prospect) pushed this to 8; the parameters ARE `Brief`'s fields, and the whole point of this constructor is to assemble them without a world to derive `site` from
    pub fn from_parts(
        function: Option<Function>,
        tech: Option<TechHorizon>,
        notability: Option<Notability>,
        people: Option<KindId>,
        peak_population: u32,
        built: bool,
        cold: bool,
        site: Option<Site>,
    ) -> Self {
        Self {
            function,
            tech,
            notability,
            people,
            peak_population,
            built,
            cold,
            site,
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
/// `exotic_sites` is the world's placed exotic regimes
/// (`hornvale_locale::LocaleContext::strange_sites`) and `cave_sites` is the
/// vertices holding a cave (`GeneratedTerrain::cave_site_vertices`, which is
/// one pass of `cave_at` over the grid). Both are parameters rather than
/// something derived here because a `LocaleContext` is expensive and the
/// caller already holds one; the same reason `geo` and `index` are parameters.
/// `cave_sites` is much the longer roster of the two — ~870-2,440 vertices
/// against ~100 — so a caller that asks per turn should hold it rather than
/// re-scan the grid, which costs ~2.9 ms (`Session` does exactly that).
///
/// **The exotic read runs site→facet, not facet→site, and that direction is
/// deliberate.** The cheap-looking alternative — resolve `locale`'s containing
/// vertex, then ask whether that vertex holds a site — is O(1) but silently
/// wrong at the edges: `hornvale_worldgen::site_facet_for` places an address
/// inside a cube-sphere quad around the vertex, and the cube-sphere quad mesh
/// and the icosphere vertex mesh have been unrelated since The Pavement, so an
/// address may land where [`containing_vertex`] answers with a NEIGHBOUR. Under
/// that direction such a site would exist in the world's own listing and be
/// unreachable at every facet, forever, with nothing red. The membership test
/// below has no such edge, and it is the shape `Terrain::is_built` already uses
/// for settlement territory.
/// type-audit: bare-ok(count: walk_depth)
#[allow(clippy::too_many_arguments)] // `cave_sites` (Task 4, The Prospect) pushed this to 8; every parameter is a value the CALLER already holds and must not re-derive — bundling them into a struct would add a public type whose only content is "the four things `Session` keeps" and whose only reader is this function
pub fn brief_of(
    world: &World,
    geo: &Geosphere,
    index: &NearestVertexIndex,
    place: &Facet,
    terrain: &dyn crate::liveness::Terrain,
    walk_depth: u32,
    exotic_sites: &[StrangeSite],
    cave_sites: &[Vertex],
) -> Brief {
    let locale = crate::depth::truncate_to_walk(place, walk_depth);
    let built = terrain.is_built(&locale);
    let cold = terrain.is_cold(&locale);
    // Settlement wins where both hold — `Site::salience` is the presentation
    // ordering and this is its production consequence (spec §6, Task 1).
    //
    // NOTE ON COST: like the occupation map below, this re-derives every placed
    // site's address on every call — the budget caps placement at 1% of land
    // vertices, so a miss walks the whole list. Same remedy if a profile ever
    // shows it: hoist the placed set to the caller (`Session` already holds
    // `built` exactly that way), never a cache inside a derivation.
    let placed_at = |vertex: Vertex, reason: SiteReason| {
        site_facet_for(vertex, reason, world.seed, geo, walk_depth) == locale
    };
    // Salience order (spec §6): settlement, then exotic, then cave. A facet
    // holding more than one is named by the strongest, and `Site::salience` is
    // the presentation half of the same ordering.
    let site = if built {
        Some(Site::placed(SiteKind::Settlement, None))
    } else if exotic_sites
        .iter()
        .any(|site| placed_at(Vertex(site.vertex), SiteReason::Exotic))
    {
        Some(Site::placed(SiteKind::Exotic, None))
    } else if cave_sites
        .iter()
        .any(|&vertex| placed_at(vertex, SiteReason::Cave))
    {
        Some(Site::placed(SiteKind::Cave, None))
    } else {
        None
    };
    let alive = containing_vertex(&locale, geo, index)
        .and_then(|vertex| {
            // NOTE ON COST: this derives the whole per-vertex occupation map on
            // every call. Correct but wasteful, and `brief_of` will be called
            // per descent. If a profile shows it mattering, hoist the map to
            // the caller (the session can hold it for the possession's life) —
            // do NOT memoize inside this function, because a hidden cache in a
            // derivation path is how derived state stops being derived.
            hornvale_worldgen::occupations_by_vertex(world).remove(&vertex)
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
            site,
        ),
        None => Brief::from_parts(None, None, None, None, 0, built, cold, site),
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
            None,
        );
        assert_eq!(b.function, Some(Function::Trade));
        assert_eq!(b.tech, Some(TechHorizon::Classical));
        assert_eq!(b.notability, Some(Notability::Seat));
        assert!(b.built);
        assert!(b.cold);
    }

    #[test]
    fn from_parts_with_no_occupation_axes_still_carries_climate() {
        let b = Brief::from_parts(None, None, None, None, 0, false, true, None);
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
            None,
        );
        let b = Brief::from_parts(
            Some(Function::Fort),
            Some(TechHorizon::Classical),
            None,
            None,
            0,
            true,
            false,
            None,
        );
        assert_ne!(a, b);
    }

    /// H1's anchor at this task: a brief with `built` true carries a Settlement
    /// site, and one without carries none. The two agree exactly, so swapping
    /// the gate in Task 3 cannot change enterability.
    ///
    /// `from_parts` no longer derives `site` from `built` itself — it has no
    /// world to ask about a cave or an exotic site, so a self-derivation here
    /// would be a half-right answer masquerading as authoritative. The caller
    /// computes it, exactly as `brief_of` does in production.
    #[test]
    fn a_built_brief_carries_a_settlement_site_and_an_unbuilt_one_carries_none() {
        let built_site = Some(Site::placed(SiteKind::Settlement, None));
        let built = Brief::from_parts(None, None, None, None, 0, true, false, built_site);
        let wild = Brief::from_parts(None, None, None, None, 0, false, false, None);
        assert_eq!(
            built.site.as_ref().map(|site| site.kind),
            Some(SiteKind::Settlement)
        );
        assert_eq!(wild.site, None);
    }
}

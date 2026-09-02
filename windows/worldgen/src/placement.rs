//! Where a placed site actually stands (The Prospect, Task 5).
//!
//! A site — a cave mouth, a vent, a crystal flat — is *warranted* by fields
//! that only exist at geosphere vertices, 110-132 km apart at level 6. A walker
//! stands on a facet 1.126 km across. This module is the seam between those two
//! resolutions: it turns "somewhere near vertex N" into a specific address.
//!
//! # Why an address, rather than a per-facet predicate
//!
//! The cheap alternative is to threshold the nearest vertex's field and call
//! every facet in range a site. That is `CLIM-water-label-resolution-vs-walk-
//! band` exactly — the registered defect where all 81 facets of seed 42's
//! flagship band draw the river glyph, because `WaterKind` is a per-vertex
//! label read nearest-vertex rather than interpolated. A site read that way
//! would not be a place; it would be a 110 km region that says "site" wherever
//! you stand in it, which is the same sentence as "no site anywhere".
//!
//! # Why a draw, rather than the vertex's own facet
//!
//! Placing each site at the facet containing its vertex is genuinely derived
//! and needs no draw. It also puts every site in the world on a ~120 km
//! lattice: invisible to a walker, and obvious to anyone who plots them. The
//! draw costs one new label ([`crate::streams::SITE_PLACEMENT`]) and buys an
//! address that carries no signature of the mesh it came from.
//!
//! # Why the label lives at the composition root
//!
//! `windows/CLAUDE.md` allows a draw here only when **no single domain can
//! host it**, and this is that case twice over. The two callers are
//! `domains/terrain`'s cave proneness and `windows/locale`'s rarity budget: a
//! domain crate may not depend on a sibling, and no domain may depend on a
//! window at all, so terrain could host the cave half and nothing else. One
//! mechanism serving both kinds has exactly one legal home, and this is it —
//! the same argument `crate::streams::SETTLEMENT_DISPOSITION` and
//! `crate::streams::VOLCANO` already carry.
//!
//! # What this module does NOT decide
//!
//! Whether a vertex warrants a site at all. That is the caller's question —
//! `windows/locale`'s rarity budget answers it for exotic sites — and this
//! module answers only "where". A caller arrives with a vertex and a reason.

use crate::streams::SITE_PLACEMENT;
use hornvale_kernel::seed::StreamLabel;
use hornvale_kernel::{Facet, Geosphere, Seed, Stream, Vertex};

/// Why a site is being placed at a vertex — the role word its draw's key
/// spells, so that two kinds warranted at the same vertex do not land on the
/// same facet.
///
/// The same discipline `crate::streams::ENTRANCE_MOUTH` and
/// `crate::streams::BAND_DESCENT` apply: one place in the lattice answering two
/// independent questions gets one key per question, never one key and a fixed
/// draw order. It is not [`hornvale_vessel`]'s `SiteKind` and cannot be —
/// `windows/vessel` sits above this crate — and it is deliberately narrower:
/// a settlement is never placed this way, because `Terrain::is_built` already
/// carries a real per-facet answer for it.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum SiteReason {
    /// A cave mouth, warranted by `hornvale_terrain`'s per-vertex cave
    /// proneness.
    Cave,
    /// An exotic regime, warranted by `windows/locale`'s rarity budget.
    Exotic,
}

impl SiteReason {
    /// The role word this reason spells into a placement key. Private: the
    /// spelling is a save-format contract, so it has exactly one caller
    /// ([`placement_key`]) and no way for a second one to paraphrase it.
    fn word(self) -> &'static str {
        match self {
            SiteReason::Cave => "cave",
            SiteReason::Exotic => "exotic",
        }
    }
}

/// How far below the canonical grid the placement QUAD sits — the quad a site
/// is drawn uniformly within.
///
/// Two, so the placement quad is a sixteenth of a grid-level quad in area and
/// a quarter of one on a side: about 39 km across against a 110-132 km vertex
/// spacing.
/// That is the whole calibration, and it is a ratio rather than a distance on
/// purpose — it holds at any globe level, where a metre figure would rot the
/// first time one moved.
///
/// **Not zero**, which would draw a site anywhere in a grid-level quad and let
/// two sites' quads overlap. **Not five**, which would confine every site to a
/// ~5 km neighbourhood of its vertex and leave the ~120 km lattice legible in a
/// plot of them, which is the regularity the draw exists to destroy.
/// type-audit: bare-ok(count)
const PLACEMENT_DEPTH_BELOW_GRID: u32 = 2;

/// The one place a placement key is spelled: the frozen wire word for a
/// geosphere vertex, then the role. `crate::volcano`'s `volcano_key` spells its
/// own vertex the same way, and decision 0246 is why a derivation key's wire
/// word is not the type's name.
fn placement_key(vertex: Vertex, reason: SiteReason) -> String {
    format!("cell/{}/{}", vertex.0, reason.word()) // lexicon: the frozen wire spelling of a VERTEX in a derivation key (decision 0246), as `volcano_key` spells it — never the mesh-area sense
}

/// The stream one placement draws from: [`placement_key`] composed under
/// [`SITE_PLACEMENT`], following the composed-label pattern
/// `crate::volcano::volcano_stream` and `crate::chamber` already use.
fn placement_stream(seed: Seed, vertex: Vertex, reason: SiteReason) -> Stream {
    seed.derive(SITE_PLACEMENT)
        .derive(StreamLabel::dynamic(&placement_key(vertex, reason)))
        .stream()
}

/// The facet a site warranted at `vertex` for `reason` actually stands on.
///
/// Pure, and stable for a `(seed, vertex, reason)` forever: this is an
/// ADDRESS, not a re-rolled guess, and a caller that asks twice is asking about
/// the same place. Nothing here is stored — decision 0100's test (could I
/// recompute this from the seed alone? then it is not a fact) puts the whole
/// answer in the derived register, so it costs no facts and no save-format
/// change beyond the label itself.
///
/// # How
///
/// Start at the facet containing the vertex's own position, then redraw the
/// tail of its path — every digit below [`PLACEMENT_DEPTH_BELOW_GRID`] levels
/// under the canonical grid. The kept head names the placement quad; the drawn
/// tail is a uniform choice of facet inside it. Two draws of the same tail
/// length is one draw per digit, in path order, which is a frozen contract like
/// every other draw shape in this crate.
///
/// # What it does NOT promise
///
/// That the returned facet resolves back to `vertex` under
/// `hornvale_vessel::brief`'s `containing_vertex`. It usually will and it does
/// not have to: the cube-sphere quad mesh and the icosphere vertex mesh are
/// unrelated since The Pavement, so a construction that guaranteed it would
/// have to consult the nearest-vertex index and reject. A consumer therefore
/// asks the question in the direction that needs no such guarantee — "is my
/// facet one of the placed facets?", a set membership test, exactly as
/// `Terrain::is_built` already answers for settlement territory — rather than
/// "which site does my vertex hold?", which would silently lose any site whose
/// address landed across a territory line.
///
/// # Degenerate depth
///
/// A `walk_depth` at or above the grid is the only case a world produces
/// (`hornvale_locale::walk_depth` is the globe level plus 7). A shallower one
/// leaves no digits to draw, and this returns the vertex's own facet having
/// consumed nothing — the honest answer, since there is no room inside the quad
/// to place anything.
/// type-audit: bare-ok(count: walk_depth)
pub fn site_facet_for(
    vertex: Vertex,
    reason: SiteReason,
    seed: Seed,
    geo: &Geosphere,
    walk_depth: u32,
) -> Facet {
    let mut facet = Facet::containing(geo.position(vertex), walk_depth);
    let quad_depth = geo.depth().saturating_add(PLACEMENT_DEPTH_BELOW_GRID);
    let kept = (quad_depth.min(walk_depth)) as usize;
    let mut stream = placement_stream(seed, vertex, reason);
    for digit in facet.path[kept..].iter_mut() {
        *digit = stream.range_u32(0, 3) as u8;
    }
    facet
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A level-6 globe, the canonical grid every shipped world uses.
    fn grid() -> Geosphere {
        Geosphere::new(6)
    }

    /// The placement lands inside the placement QUAD — the quad
    /// [`PLACEMENT_DEPTH_BELOW_GRID`] levels under the grid that contains the
    /// vertex — and nowhere else. This is what makes the address a *local*
    /// one rather than a draw over the whole globe, and it is the property the
    /// module doc's "39 km against 110-132 km" claim rests on.
    #[test]
    fn a_placement_stays_inside_its_vertexs_own_quad() {
        let geo = grid();
        let seed = Seed(42);
        for v in [Vertex(0), Vertex(1953), Vertex(1954), Vertex(40961)] {
            let quad = Facet::containing(geo.position(v), geo.depth() + PLACEMENT_DEPTH_BELOW_GRID);
            let placed = site_facet_for(v, SiteReason::Exotic, seed, &geo, 13);
            assert_eq!(
                placed.ancestor(quad.depth()),
                Some(quad),
                "vertex {} placed outside its own quad",
                v.0
            );
        }
    }

    /// The draw actually moves the site off its vertex. Without this the
    /// mechanism would be indistinguishable from placing every site at the
    /// facet containing its vertex — the ~120 km lattice the module doc
    /// rejects — and every other test here would still pass.
    #[test]
    fn a_placement_is_not_just_the_vertexs_own_facet() {
        let geo = grid();
        let seed = Seed(42);
        let moved = (0..64u32)
            .map(Vertex)
            .filter(|&v| {
                site_facet_for(v, SiteReason::Exotic, seed, &geo, 13)
                    != Facet::containing(geo.position(v), 13)
            })
            .count();
        // One quad holds 4^5 = 1024 walk facets, so landing back on the
        // vertex's own facet is a 1-in-1024 event; 64 of 64 is the expected
        // answer and anything much below it means the tail is not being drawn.
        assert_eq!(
            moved, 64,
            "the drawn tail must move the site off its vertex"
        );
    }

    /// Two reasons at one vertex are two questions, so they get two keys. A
    /// shared key would put every cave that shares a vertex with an exotic site
    /// at the identical facet, in every world — a systematic collocation, not a
    /// coincidence.
    ///
    /// Over 128 vertices: two independent uniform draws over a 1024-facet quad
    /// collide with probability 1/1024 each, so ~0.125 collisions are expected
    /// and the exact count for seed 42 is a fixed, deterministic fact.
    #[test]
    fn a_cave_and_an_exotic_site_at_one_vertex_do_not_share_a_facet() {
        let geo = grid();
        let seed = Seed(42);
        let shared = (0..128u32)
            .map(Vertex)
            .filter(|&v| {
                site_facet_for(v, SiteReason::Cave, seed, &geo, 13)
                    == site_facet_for(v, SiteReason::Exotic, seed, &geo, 13)
            })
            .count();
        assert_eq!(shared, 0, "the reason must key the draw, not decorate it");
    }

    /// A depth with no room below the quad draws nothing and returns the
    /// vertex's own facet, rather than panicking on an empty path slice.
    #[test]
    fn a_walk_depth_at_the_quad_leaves_nothing_to_draw() {
        let geo = grid();
        let depth = geo.depth() + PLACEMENT_DEPTH_BELOW_GRID;
        let v = Vertex(1953);
        assert_eq!(
            site_facet_for(v, SiteReason::Exotic, Seed(42), &geo, depth),
            Facet::containing(geo.position(v), depth)
        );
    }
}

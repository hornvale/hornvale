//! The discovery layer (The Portolan part II, Task 5; design spec Amendment
//! 1, §A4). Two mechanisms answer two different questions, and — per
//! Nathan's own ruling — **must never be wired to each other**:
//!
//! > "Just being in the area where a thing was buried doesn't imply any
//! > knowledge of the buried thing any more than going to Paris means you've
//! > visited the Catacombs or going to southwest Colorado means you've
//! > visited Mesa Verde."
//!
//! - **[`Visited`] answers "where have I been?"** — a property of the
//!   possession's own walk-band ROOMS. It reuses
//!   `windows/vessel/src/purview.rs`'s own fog predicate SHAPE — an
//!   ancestor test over [`Facet::path`], upward-only by construction —
//!   rather than inventing a second notion of visitedness. That module's
//!   doc states the overlay "WRITES NOTHING"; this struct is the same kind
//!   of read, just accumulated across turns instead of rebuilt from
//!   `Knowledge` on every call.
//! - **[`Discovered`] answers "what do I know is there?"** — a property of
//!   FEATURES, and it is never derived from [`Visited`]. Spec §A4b's two
//!   kinds: an **extent feature** (the ground itself — landmass, sea,
//!   river, salt lake, volcano; `hornvale_terrain::landscape::FeatureId`)
//!   is discovered by entering any vertex of its extent, because the ground
//!   and the feature are the same object there. A **point site**
//!   (settlement, cave mouth) is a thing standing AT a vertex, not the vertex
//!   itself, so co-location buys nothing — it is discovered only by
//!   encountering the thing.
//!
//! **Structural guarantee, not a comment:** [`Visited`] and [`Discovered`]
//! do not call each other, and neither type appears as a parameter to the
//! other's methods. Grep this file: there is no `Visited` in `Discovered`'s
//! `impl` block or vice versa. That absence is the whole enforcement
//! mechanism spec Amendment 1 asks for.
//!
//! **[`Visited`] is currently WRITTEN, never READ, by anything but a
//! test.** `Driver::update_discovery` calls [`Visited::record`] every
//! turn (real, if small, cost), and [`Driver::visited`] is a public
//! accessor -- but nothing in `plate.rs` consults it, so the world map
//! does not currently draw visited and unvisited cells any differently.
//! This is deliberately unwired pending a future campaign, not dead code
//! by oversight: F6' (design spec §A10) leaves open whether a coarse-zoom
//! "you have been somewhere in here" reading needs its own visual
//! treatment at all, and this type exists so that campaign has the data
//! already flowing rather than needing to add the write path too.
//! Recorded as `CLIENT-world-map-visitedness-is-unwired`.

use hornvale_kernel::{Facet, Vertex};
use std::collections::BTreeSet;

/// Every walk-band room the possession has stood in this session — the raw
/// material [`Visited::contains_at_rung`]'s ancestor test reads.
///
/// **Why `Facet`, not `hornvale_kernel::Vertex`.** `Vertex` is a flat
/// index into one fixed-resolution icosphere mesh
/// (`kernel/src/geosphere.rs`) with no parent/child structure of its own —
/// there is no coarser or finer `Vertex` to be "rung-aware" against without
/// inventing a second coarsening scheme, which is exactly the kind of new
/// machinery the task brief warns against minting. `Facet` **is** the
/// rung system already shipped (`path: Vec<u8>`, depth up to
/// [`hornvale_kernel::Facet`]'s own `MAX_DEPTH`), and it is the type
/// `windows/vessel/src/purview.rs`'s own fog predicate already operates
/// over — reusing it here is "reuse that predicate's shape," not a
/// substitution of one for the other.
#[derive(Debug, Clone, Default)]
pub struct Visited(BTreeSet<Facet>);

impl Visited {
    /// Record one walked room. Idempotent: walking the same room twice (or
    /// standing still while `Driver::refresh` runs) costs nothing extra.
    pub fn record(&mut self, addr: Facet) {
        self.0.insert(addr);
    }

    /// Whether `addr`, coarsened to `rung` path levels (never past `addr`'s
    /// own depth), contains a walked descendant — the SAME ancestor test
    /// `windows/vessel/src/purview.rs` already runs to promote a cell to
    /// `state = "remembered"` (`w.path[..addr.path.len()] ==
    /// addr.path[..]`), reused rather than reinvented.
    ///
    /// **Upward-only by construction**, matching Nathan's own F6 ruling:
    /// "consider the cell visited but not the village. As zoom levels
    /// increase, you should be able to pick out what you have and have not
    /// visited accurately." Lowering `rung` only ever coarsens the query —
    /// walking one fine room lights every coarser room that contains it and
    /// **no sibling** (a different branch at the same depth never matches
    /// the truncated-path equality test below).
    pub fn contains_at_rung(&self, addr: &Facet, rung: u32) -> bool {
        let keep = (rung as usize).min(addr.path.len());
        let wanted = &addr.path[..keep];
        self.0.iter().any(|walked| {
            walked.face == addr.face
                && walked.path.len() >= wanted.len()
                && walked.path[..wanted.len()] == *wanted
        })
    }
}

/// A discoverable feature's own identity — the union of both §A4b kinds,
/// because a point site is not in `hornvale_terrain::landscape`'s own
/// `FeatureId`/`VertexFeatureIndex` at all (that index only ever carries the
/// five extent classes — see `FeatureClass`) and [`Discovered`] answers
/// both.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum FeatureId {
    /// An extent feature — landmass, sea, river, salt lake, volcano — the
    /// SAME identity `windows/worldgen`'s `ChainLink`/`resolve_chain_at`
    /// already carry.
    Extent(hornvale_terrain::landscape::FeatureId),
    /// A settlement, keyed to the terrain vertex its committed
    /// latitude/longitude resolves nearest to (`NearestVertexIndex::nearest`)
    /// — the same canonical-vertex keying
    /// `hornvale_terrain::landscape::FeatureId` already uses for an extent
    /// feature's own identity, so two settlements can never collide unless
    /// they share a nearest vertex (in which case they are, for this map's
    /// purposes, the same point).
    Settlement(Vertex),
    /// A cave mouth, keyed to the terrain vertex that WARRANTS it
    /// (`hornvale_terrain::GeneratedTerrain::cave_at`'s own key — one cave
    /// per vertex, by that function's own contract).
    ///
    /// **The key is the warranting vertex, not the facet the mouth stands
    /// on**, and since The Prospect those are different places: a cave is
    /// placed inside a quad around its vertex by
    /// `hornvale_worldgen::site_facet_for`. The vertex is the site's
    /// IDENTITY (one cave per vertex, by contract) and the facet is its
    /// ADDRESS; keying on the address would be keying on a value the
    /// placement draw owns, so an epoch there would silently orphan every
    /// recorded discovery.
    Cave(Vertex),
    /// A placed exotic site — strange biota, mineral crystal, a fungal
    /// canopy — keyed to the canonical-grid vertex
    /// `hornvale_locale::LocaleContext::strange_sites` warrants it at (The
    /// Prospect, Task 8).
    ///
    /// **A point site, so it is discovered by encounter and never by
    /// co-location** — the same §A4b rule [`FeatureId::Cave`] and
    /// [`FeatureId::Settlement`] obey. `Driver::update_discovery` records it
    /// when the possession stands on the site's own placed FACET, a
    /// 1.126 km room, never merely within its 110-132 km vertex: standing
    /// in the mouth of the thing is an encounter, standing somewhere in the
    /// province is not.
    Exotic(Vertex),
}

/// Every feature the possession has DISCOVERED this session (§A4b:
/// encountered, never merely co-located with).
///
/// See the module doc for why this type never reads or is read by
/// [`Visited`].
#[derive(Debug, Clone, Default)]
pub struct Discovered(BTreeSet<FeatureId>);

impl Discovered {
    /// Record one discovered feature. Idempotent, and — H6, "discovery is
    /// monotonic" — nothing in this type ever removes an entry, so a name
    /// once earned stays for the session.
    pub fn record(&mut self, id: FeatureId) {
        self.0.insert(id);
    }

    /// Whether `id` has been discovered.
    pub fn contains(&self, id: FeatureId) -> bool {
        self.0.contains(&id)
    }

    /// How many features have been discovered.
    ///
    /// **This is a sound VERSION for a cache key precisely because this
    /// type is monotonic** (H6): [`Discovered::record`] is its only
    /// mutator, it only ever inserts, and nothing removes or clears — so
    /// the count strictly increases and two different discovery sets can
    /// never share one. If a future campaign adds removal, every consumer
    /// keying on this count silently serves stale data; change them
    /// together.
    pub fn len(&self) -> usize {
        self.0.len()
    }

    /// Whether nothing has been discovered yet.
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn addr(face: u8, path: &[u8]) -> Facet {
        Facet {
            face,
            path: path.to_vec(),
        }
    }

    /// A freshly walked room reads as visited at its own full rung, and at
    /// every coarser rung above it — the "upward only" propagation F6a
    /// states in prose, checked directly.
    #[test]
    fn walking_a_room_marks_it_and_every_coarser_ancestor() {
        let mut v = Visited::default();
        v.record(addr(3, &[0, 1, 2, 3]));

        for rung in 0..=4u32 {
            assert!(
                v.contains_at_rung(&addr(3, &[0, 1, 2, 3]), rung),
                "rung {rung} should read as visited: it is an ancestor of the walked room"
            );
        }
    }

    /// Upward-only means NO SIBLING lights up: a different branch at the
    /// same depth as the walked room must not read as visited, even though
    /// it shares every ancestor rung above the divergence point.
    #[test]
    fn a_sibling_branch_is_not_visited() {
        let mut v = Visited::default();
        v.record(addr(3, &[0, 1, 2, 3]));

        // Diverges at the last digit (a true sibling at full depth).
        assert!(!v.contains_at_rung(&addr(3, &[0, 1, 2, 9]), 4));
        // Diverges one level up (a cousin, not even a sibling).
        assert!(!v.contains_at_rung(&addr(3, &[0, 1, 9, 3]), 4));
        // But the shared coarser ancestor above the divergence still reads
        // as visited — this is what "upward only" means, not "nothing else
        // ever matches."
        assert!(v.contains_at_rung(&addr(3, &[0, 1, 9, 3]), 2));
    }

    /// A different base face never matches, regardless of path.
    #[test]
    fn a_different_face_is_never_visited() {
        let mut v = Visited::default();
        v.record(addr(3, &[0, 1, 2, 3]));
        assert!(!v.contains_at_rung(&addr(7, &[0, 1, 2, 3]), 0));
    }

    /// `Discovered` is monotonic and keyed by identity, not by insertion
    /// count: recording the same feature twice changes nothing observable.
    #[test]
    fn discovered_is_idempotent() {
        let mut d = Discovered::default();
        let id = FeatureId::Cave(Vertex(42));
        assert!(!d.contains(id));
        d.record(id);
        assert!(d.contains(id));
        d.record(id);
        assert!(d.contains(id));
    }

    /// The three site kinds are genuinely distinct identities even when
    /// their underlying vertex coincides — a settlement and a cave at the
    /// same `Vertex` are two different discoverable things.
    #[test]
    fn site_kinds_at_the_same_vertex_are_distinct() {
        let mut d = Discovered::default();
        let vertex = Vertex(7);
        d.record(FeatureId::Settlement(vertex));
        assert!(d.contains(FeatureId::Settlement(vertex)));
        assert!(!d.contains(FeatureId::Cave(vertex)));
    }
}

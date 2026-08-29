//! The anchor graph — the room's interior as nodes and edges. Anchors are
//! REGIONS (a hearth, an alcove, a threshold), which is what makes RCC-8 the
//! right vocabulary. Nothing here is serialized: an anchor has no coordinate,
//! and its identity within a room is positional, not persisted.

use super::relation::{Rcc8, converse};

/// An anchor's index within its [`Interior`]. Not an entity id and never
/// serialized — a derived anchor has no identity until promotion (spec §4,
/// reserved).
/// type-audit: bare-ok(index)
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct AnchorId(pub u16);

/// Declares [`AnchorKind`] **and** its roster [`AnchorKind::ALL`] from one
/// list of variants, so that no roster of anchor kinds exists anywhere that
/// can go short of the enum.
///
/// **Why this crate grew a macro (The Chattel, Task 9 fix round 1).** Three
/// separate hand-written `[AnchorKind; 14]` rosters had accumulated —
/// `windows/vessel/tests/suite/affordance.rs`,
/// `cli/tests/suite/anchor_thing_correspondence.rs`,
/// `windows/vessel/src/chamber_prose.rs` — and each carried a comment saying
/// it was "kept in step by an exhaustive match". **That claim is false in the
/// direction that actually happens.** The compiler forces an arm per variant
/// in `thing_kind_of`, `noun` and `detail`; it says nothing about a *list*
/// sitting beside them. A reviewer added a fifteenth variant, wrote the arms
/// the compiler demanded, pointed it at `KindId("cave-mouth")`, and 1209
/// tests passed — including the precondition whose own doc comment promised
/// to stop being evidence in exactly that case. Dropping a variant reddened
/// a test; adding one reddened nothing.
///
/// No test can enumerate a variant it has never heard of, so no test can
/// close that gap: the roster has to be produced by the same declaration
/// that produces the enum. That is what this macro is, and it is the whole
/// mechanism — with the roster generated, an added variant lengthens
/// [`AnchorKind::ALL`], and every sweep and every frozen table measured
/// against it moves with it.
///
/// Deliberately not exported and deliberately single-purpose: it declares
/// this one enum. A second use would be a second design decision.
macro_rules! anchor_kinds {
    ($( $(#[$variant_doc:meta])* $variant:ident ),+ $(,)?) => {
        /// What an anchor IS. An object earns a place here by the activity it
        /// affords (spec §7), never by decoration. (No `type-audit:` tag: a
        /// fieldless enum has no primitive at its boundary, and `tag` is NOT a
        /// ratified `bare-ok` class — see `tools/type-audit/src/tag.rs:4` for
        /// the eleven that are.)
        #[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
        pub enum AnchorKind {
            $( $(#[$variant_doc])* $variant, )+
        }

        impl AnchorKind {
            /// Every variant, once, in declaration order — the single roster
            /// of anchor kinds in this workspace.
            ///
            /// **Generated from the enum's own declaration, which is the
            /// only reason it cannot go short.** It carries no length
            /// annotation on purpose: a hand-written count is one more thing
            /// to forget in step with the list it counts, and the length
            /// that matters is asserted where it is load-bearing — against
            /// the deliberately frozen verb table in
            /// `the_re_key_preserves_every_anchor_kinds_offer`, which is
            /// what turns an appended variant into a red rather than a
            /// silently wider sweep.
            pub const ALL: &[AnchorKind] = &[ $( AnchorKind::$variant, )+ ];
        }
    };
}

anchor_kinds! {
    /// A fire: emits warmth and light; the canonical gathering place.
    Hearth,
    /// A doorway — an anchor that is ALSO a room-graph edge (the two-level seam).
    Threshold,
    /// A place to sleep.
    Bed,
    /// A water vessel or basin.
    Vessel,
    /// A screen or pillar: affords nothing, shapes sightlines (reserved).
    Screen,
    /// A natural pool (the wilderness half of the catalogue).
    Pool,
    /// A fallen log (the wilderness half).
    Log,
    /// The room's open middle — every room has one, built or wild. The anchor
    /// other patterns attach to when they attach to nothing more specific.
    Ground,
    /// A recess off the main space: what makes a room deeper than a hub.
    Alcove,
    // --- appended by The Blocking (Task 6), for the chamber ROLES ---
    //
    // Appended rather than inserted. `Ord` here is derivation order, and
    // `pattern::compose` keys two `BTree*` collections on it — by lookup only,
    // never by iteration — so declaration order does not reach a composition
    // today. Appending keeps it that way without needing that argument to hold.
    /// A locked chest: what a place with more than a hamlet's people has to keep.
    Strongbox,
    /// The seat that commands the entrance — a regional seat's own chair.
    HighSeat,
    /// An upright loom: the domestic craft, and it wants the light of a doorway.
    Loom,
    /// A smith's anvil, which is why it stands within reach of the quenching water.
    Anvil,
    /// An altar, and the basin beside it is the washing the rite asks for first.
    Altar,
    // --- appended by The Chattel (Task 11), for the CONTAINER half ---
    //
    // The first anchor kind authored to be CONTAINED rather than to contain or
    // to stand beside: `the-key-in-the-strongbox` attaches it
    // `Attach::Within(Strongbox)`. Until it existed the grammar's only `within`
    // relation anywhere was `{(Alcove, Hearth)}` — a full census over all 60
    // production gate combinations — so `Openable` had nothing to reveal and
    // `Lockable` nothing to lock (spec §3.8).
    /// A small key, kept where a place keeps what it locks up.
    Key,
}

/// One anchor: what it is, and the anchor it lies strictly within, if any.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Anchor {
    /// What this anchor is.
    pub kind: AnchorKind,
    /// The anchor this one lies strictly inside (`Ntpp`), if any.
    pub within: Option<AnchorId>,
}

/// A room's interior: the anchors and which touch which. Deterministic
/// throughout — `Vec` order is the anchor order, adjacency is a `BTreeSet`.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Interior {
    anchors: Vec<Anchor>,
    adjacency: std::collections::BTreeSet<(AnchorId, AnchorId)>,
}

impl Interior {
    /// An empty interior.
    pub fn new() -> Self {
        Self::default()
    }

    /// Append an anchor, returning its id.
    pub fn push(&mut self, kind: AnchorKind, within: Option<AnchorId>) -> AnchorId {
        let id = AnchorId(self.anchors.len() as u16);
        self.anchors.push(Anchor { kind, within });
        id
    }

    /// Record that two anchors touch (`Ec`). Symmetric: stored in both orders so
    /// the read is order-independent.
    pub fn connect(&mut self, a: AnchorId, b: AnchorId) {
        self.adjacency.insert((a, b));
        self.adjacency.insert((b, a));
    }

    /// Every anchor id, ascending.
    pub fn ids(&self) -> Vec<AnchorId> {
        (0..self.anchors.len())
            .map(|i| AnchorId(i as u16))
            .collect()
    }

    /// The anchor at `id`.
    pub fn anchor(&self, id: AnchorId) -> &Anchor {
        &self.anchors[id.0 as usize]
    }

    /// The anchors directly touching `a` (`Ec`), ascending. Adjacency ONLY —
    /// containment (`Ntpp`) is a SEPARATE relation (see [`Anchor::within`]),
    /// which is why a creature crossing from an alcove into the hearth it
    /// contains is not a "neighbour" step in THIS sense even though it is a
    /// genuine single-hop walk ([`Self::walkable_neighbors`] is the union
    /// callers that actually MOVE a creature need).
    pub fn neighbors(&self, a: AnchorId) -> Vec<AnchorId> {
        self.adjacency
            .iter()
            .filter(|(x, _)| *x == a)
            .map(|(_, y)| *y)
            .collect()
    }

    /// Every anchor a creature standing at `a` may step to in ONE hop:
    /// adjacency (`Ec`, [`Self::neighbors`]) AND containment (`Ntpp`) in
    /// EITHER direction — the anchor `a` lies strictly within (its
    /// container, if any) and every anchor that lies strictly within `a`
    /// (its contents). Deterministic (ascending, duplicate-free).
    ///
    /// This is the SINGLE definition of "one walkable hop" the interior
    /// layer has: [`crate::interior::route_within`]'s A* successors,
    /// [`crate::liveness::Occupancy::walk`]'s adjacency check, and
    /// [`Interior::is_connected`]'s reachability walk all call this rather
    /// than each re-deriving their own notion of "adjacent enough to walk
    /// to" — a route planner, the thing that actually executes one step of
    /// its plan, and the validator that certifies a room's reachability
    /// must all agree on what a step IS, or a planned step silently fails
    /// to execute, or the validator certifies a room a creature cannot
    /// cross.
    pub fn walkable_neighbors(&self, a: AnchorId) -> Vec<AnchorId> {
        let mut out = self.neighbors(a);
        if let Some(parent) = self.anchor(a).within {
            out.push(parent);
        }
        for id in self.ids() {
            if self.anchor(id).within == Some(a) {
                out.push(id);
            }
        }
        out.sort();
        out.dedup();
        out
    }

    /// Whether `a` lies strictly within `b`, following the containment chain
    /// (`Ntpp` is transitive — T1's `is_transitive`).
    ///
    /// TERMINATION IS STRUCTURAL, not assumed: the walk only follows a parent
    /// whose index is strictly SMALLER than the child's, so the visited indices
    /// are a strictly decreasing sequence of `u16` and the loop cannot run more
    /// than `a.0` times. That is exactly the validator's invariant (`within` may
    /// only name an EARLIER anchor — T4) read as a guard rather than a promise,
    /// so a malformed interior yields a wrong-but-finite answer instead of
    /// hanging.
    fn within_chain(&self, a: AnchorId, b: AnchorId) -> bool {
        let mut child = a;
        while let Some(p) = self.anchor(child).within {
            if p >= child {
                // Violates the earlier-anchor invariant: refuse to follow it
                // rather than risk a cycle.
                return false;
            }
            if p == b {
                return true;
            }
            child = p;
        }
        false
    }

    /// The single RCC-8 relation holding between `a` and `b` (JEPD: exactly one,
    /// because this is a function). Converse-consistent by construction.
    pub fn relation(&self, a: AnchorId, b: AnchorId) -> Rcc8 {
        if a == b {
            return Rcc8::Eq;
        }
        if self.within_chain(a, b) {
            return Rcc8::Ntpp;
        }
        if self.within_chain(b, a) {
            return converse(Rcc8::Ntpp);
        }
        if self.adjacency.contains(&(a, b)) {
            return Rcc8::Ec;
        }
        Rcc8::Dc
    }

    /// Whether every anchor is reachable from anchor `0` by adjacency or
    /// containment — the validator's well-formedness rule (T4). An empty
    /// interior is trivially connected.
    /// type-audit: bare-ok(flag: return)
    pub fn is_connected(&self) -> bool {
        if self.anchors.is_empty() {
            return true;
        }
        let mut seen: std::collections::BTreeSet<AnchorId> = [AnchorId(0)].into_iter().collect();
        let mut frontier = vec![AnchorId(0)];
        while let Some(cur) = frontier.pop() {
            for n in self.walkable_neighbors(cur) {
                if seen.insert(n) {
                    frontier.push(n);
                }
            }
        }
        seen.len() == self.anchors.len()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A hall with a hearth inside it, a threshold touching the hall, and a
    /// screen disconnected from the hearth. Three of the four v1 relations.
    fn planted() -> (Interior, AnchorId, AnchorId, AnchorId, AnchorId) {
        let mut i = Interior::new();
        let hall = i.push(AnchorKind::Pool, None); // stand-in region
        let hearth = i.push(AnchorKind::Hearth, Some(hall));
        let door = i.push(AnchorKind::Threshold, None);
        let screen = i.push(AnchorKind::Screen, None);
        i.connect(hall, door);
        i.connect(hall, screen);
        (i, hall, hearth, door, screen)
    }

    #[test]
    fn exactly_one_relation_holds_for_every_ordered_pair() {
        // JEPD, asserted rather than assumed: `relation` is a FUNCTION, so it
        // returns exactly one — this pins that it is also CONVERSE-CONSISTENT,
        // which is where a hand-written table would drift.
        let (i, ..) = planted();
        for a in i.ids() {
            for b in i.ids() {
                assert_eq!(
                    i.relation(b, a),
                    converse(i.relation(a, b)),
                    "relation({a:?},{b:?}) and its converse disagree"
                );
            }
        }
    }

    #[test]
    fn containment_reads_ntpp_and_its_converse() {
        let (i, hall, hearth, ..) = planted();
        assert_eq!(
            i.relation(hearth, hall),
            Rcc8::Ntpp,
            "the hearth is in the hall"
        );
        assert_eq!(i.relation(hall, hearth), Rcc8::NtppI);
        assert_eq!(i.relation(hall, hall), Rcc8::Eq);
    }

    #[test]
    fn adjacency_reads_ec_and_non_adjacency_reads_dc() {
        let (i, hall, _hearth, door, screen) = planted();
        assert_eq!(
            i.relation(hall, door),
            Rcc8::Ec,
            "the threshold touches the hall"
        );
        assert_eq!(i.relation(door, screen), Rcc8::Dc, "the PRIVACY primitive");
    }

    #[test]
    fn connectivity_is_detected_in_both_directions() {
        let (i, ..) = planted();
        assert!(i.is_connected(), "the planted interior is connected");
        // An orphan anchor makes it unreachable — the validator's rule (T4).
        let mut broken = Interior::new();
        let a = broken.push(AnchorKind::Hearth, None);
        let _b = broken.push(AnchorKind::Bed, None);
        assert!(
            !broken.is_connected(),
            "two anchors with no edge between them are disconnected"
        );
        let mut fixed = Interior::new();
        let x = fixed.push(AnchorKind::Hearth, None);
        let y = fixed.push(AnchorKind::Bed, None);
        fixed.connect(x, y);
        assert!(fixed.is_connected());
        let _ = a;
    }
}

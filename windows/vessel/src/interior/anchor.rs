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

/// What an anchor IS. An object earns a place here by the activity it affords
/// (spec §7), never by decoration. (No `type-audit:` tag: a fieldless enum has
/// no primitive at its boundary, and `tag` is NOT a ratified `bare-ok` class —
/// see `tools/type-audit/src/tag.rs:4` for the eleven that are.)
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum AnchorKind {
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

    /// The anchors directly touching `a`, ascending.
    pub fn neighbors(&self, a: AnchorId) -> Vec<AnchorId> {
        self.adjacency
            .iter()
            .filter(|(x, _)| *x == a)
            .map(|(_, y)| *y)
            .collect()
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
            let mut linked = self.neighbors(cur);
            if let Some(p) = self.anchor(cur).within {
                linked.push(p);
            }
            for id in self.ids() {
                if self.anchor(id).within == Some(cur) {
                    linked.push(id);
                }
            }
            for n in linked {
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

//! A *site* is something at a facet with an interior worth entering.
//!
//! This replaces `Brief::built` as the gate every enterable place hangs off.
//! `built` meant "a structure stands here" (`brief.rs`), which is true of a
//! settlement and false of a cave (dissolved by water) or an exotic site
//! (grown) — so widening `built` would have put a lie in the predicate.
//! Decision 0536.

/// What kind of place a site is.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum SiteKind {
    /// A cave mouth, derived from `cave_proneness` — no seeded draw.
    Cave,
    /// A placed exotic site: strange biota, mineral crystal, a fungal canopy.
    Exotic,
    /// A settlement — the only kind for which `Terrain::is_built` is true.
    Settlement,
}

/// How much ground a site covers.
///
/// **Only [`Extent::Point`] is emitted by The Prospect.** The enum exists
/// because exotic sites are not uniform in scale — a cursed land is miles
/// across with components inside it — and modelling extent later would be a
/// migration of every consumer rather than a fill-in. Decision 0538.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Extent {
    /// One facet.
    Point,
}

/// Something at a facet with an interior worth entering.
/// type-audit: bare-ok(identifier-text: name)
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Site {
    /// What kind of place this is.
    pub kind: SiteKind,
    /// The site's own name, where it has one. Caves and exotic sites do not.
    pub name: Option<String>,
    /// How much ground it covers. Always [`Extent::Point`] in this campaign.
    pub extent: Extent,
}

impl Site {
    /// A point site of the given kind.
    /// type-audit: bare-ok(identifier-text: name)
    pub fn new(kind: SiteKind, name: Option<String>) -> Self {
        Self {
            kind,
            name,
            extent: Extent::Point,
        }
    }

    /// Presentation rank for choosing which sites a locale NAMES when it holds
    /// more than one (spec §6). Higher is more salient. Never world-state.
    ///
    /// Not `count` — this is not a cardinality of anything, it is an
    /// ordinal position in the presentation-salience ordering over
    /// [`SiteKind`] (decision 0028's `index` class: "a position into a
    /// structure whose type carries the meaning"), the same reasoning
    /// `Band::rank` in `kernel/src/band.rs` uses for its own rank return.
    /// type-audit: bare-ok(index: return)
    pub fn salience(&self) -> u8 {
        match self.kind {
            SiteKind::Settlement => 3,
            SiteKind::Exotic => 2,
            SiteKind::Cave => 1,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Salience orders what gets NAMED when a facet holds more than one site
    /// (spec §6). It is presentation only and never world-state.
    #[test]
    fn salience_ranks_settlement_over_exotic_over_cave() {
        let s = Site::new(SiteKind::Settlement, Some("Doaba".into()));
        let x = Site::new(SiteKind::Exotic, None);
        let c = Site::new(SiteKind::Cave, None);
        assert!(s.salience() > x.salience());
        assert!(x.salience() > c.salience());
    }

    /// `Site::new` must not silently drop or alter what it is given — a
    /// regression that swapped or discarded `kind`/`name` would still
    /// compile and would still pass every other test in this module.
    #[test]
    fn new_round_trips_kind_and_name() {
        let named = Site::new(SiteKind::Settlement, Some("Doaba".into()));
        assert_eq!(named.kind, SiteKind::Settlement);
        assert_eq!(named.name, Some("Doaba".to_string()));

        let unnamed = Site::new(SiteKind::Cave, None);
        assert_eq!(unnamed.kind, SiteKind::Cave);
        assert_eq!(unnamed.name, None);
    }

    /// This campaign emits `Point` only (spec §7). **Currently vacuous**:
    /// `Extent` has exactly one variant, so any `Site::new` that compiles
    /// necessarily sets it. Kept anyway as a FORWARD guard — it exists to
    /// catch a future `Site::new` that defaults to a `Region` variant once
    /// one lands, not to discriminate today.
    #[test]
    fn a_new_site_is_a_point() {
        assert_eq!(Site::new(SiteKind::Cave, None).extent, Extent::Point);
    }
}

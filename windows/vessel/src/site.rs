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
    /// A cave mouth. Existence is `GeneratedTerrain::cave_at`'s answer at a
    /// geosphere vertex — the fluid-flow point process over material,
    /// drainage, crust age and plate-boundary distance, noise-gated so caves
    /// cluster — and the facet is PLACED by
    /// `hornvale_worldgen::site_facet_for`, exactly as an exotic site is.
    /// There is no separate site predicate: this doc named one for a day, and
    /// two disagreeing answers to "is there a cave here" is a correctness bug,
    /// not untidiness.
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

/// Which of the two ways a site came to exist, and what that costs.
///
/// Decision 0539 measured a ceiling nobody had computed: 40,962 level-6
/// vertices cannot populate 402,653,184 walk facets (a placed site at
/// every facet would need one placed per 9,830 — 0.0102% coverage). So a
/// per-facet surface must eventually come from noise interacting with macro
/// features rather than from placement, and the two mechanisms carry
/// genuinely different costs and guarantees — this type names that split
/// rather than blurring it into one `Site`.
///
/// **This goes further than [`Extent`] does, and the difference is worth
/// naming rather than eliding.** Everything The Prospect builds is
/// [`Tier::Placed`]; [`Tier::Derived`] is modelled now, unused, so the
/// surface tier a later campaign builds is a fill-in against an
/// already-widened type rather than a migration of every `Site` consumer.
///
/// These docs used to say "this is `Extent::Region`'s situation again", four
/// times over, and `Extent::Region` **does not exist** — the enum has one
/// variant and always did on this branch. `cargo doc` reported the two
/// intra-doc links as broken, and decision 0538 was corrected on 2026-09-03
/// for declaring the variant in its own Decision block. The two postures are
/// therefore NOT the same: `Tier` really does carry an unused variant, so a
/// later campaign fills in an arm; `Extent` carries only the FIELD, so a later
/// campaign adds the variant first. Both avoid a signature change at every
/// consumer, which was the point; only one of them has the arm.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Tier {
    /// Generated from a level-6 vertex by a seeded draw. Bounded — roughly
    /// 10^2 to 10^4 sites planet-wide — and its facts live in the ledger:
    /// a placed site is deterministic by **record**, replayed from committed
    /// facts rather than recomputed, which is what lets it be read, invaded,
    /// or destroyed by another system and have that outcome persist. A
    /// placed delve can be a Dwarven Kingdom whose fall through a dimensional
    /// gate other systems go on to read as history.
    ///
    /// Everything The Prospect emits is `Placed`.
    Placed,
    /// Computed from noise at facet resolution: unbounded (~10^6+ planet-
    /// wide), stored nowhere, and deterministic by **derivation** — a pure
    /// function of `(seed, position)` that costs no stream label, no
    /// save-format contract, and no storage, recomputed identically on every
    /// read. A derived site may be every bit as large and as complex as a
    /// placed one, but it cannot shape world history: nothing records its
    /// fate, so no later system can read what became of it. (A derived site
    /// a player *enters* may later be promoted into the record — decision
    /// 0539 deliberately leaves that mutable state unnamed; it is a
    /// separate axis from this one, not a third `Tier` variant.)
    ///
    /// **Modelled and unused** — no constructor in this campaign builds a
    /// `Derived` site. Kept as a declared forward guard (see the `tests`
    /// module below) rather than a silently-vacuous one. (This said "exactly
    /// as `Extent::Region` is"; that variant does not exist — see [`Tier`]'s
    /// own doc.)
    Derived,
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
    /// Whether this site was placed from a vertex or derived from noise.
    /// Always [`Tier::Placed`] in this campaign — decision 0539. Orthogonal
    /// to [`Site::salience`]: a derived cave and a placed cave are equally
    /// salient *as caves*, because salience ranks what gets NAMED in prose
    /// and tier is never presentation.
    pub tier: Tier,
}

impl Site {
    /// A placed point site of the given kind — generated from a level-6
    /// vertex by a seeded draw, its facts committed to the ledger.
    ///
    /// **There is no bare `Site::new` and no third `tier` parameter on this
    /// constructor.** Every call site The Prospect has, all in this crate,
    /// wants exactly this — a settlement, cave, or exotic site placed from a
    /// vertex — and a bare `new` would give a future author a shorter, more
    /// familiar name to reach for than [`Site::derived`], silently
    /// defaulting new call sites to the wrong tier once derived sites are
    /// actually built. Naming the two constructors symmetrically forces a
    /// conscious choice instead: `placed` and `derived` read the same
    /// weight at a call site, so picking one is a decision rather than a
    /// habit. This holds at one call site or a hundred, so none is counted
    /// here. See [`Site::derived`], decision 0539.
    /// type-audit: bare-ok(identifier-text: name)
    pub fn placed(kind: SiteKind, name: Option<String>) -> Self {
        Self {
            kind,
            name,
            extent: Extent::Point,
            tier: Tier::Placed,
        }
    }

    /// A derived point site of the given kind — computed from noise at
    /// facet resolution, recorded nowhere.
    ///
    /// **Modelled and unused.** No call site in The Prospect calls this; it
    /// exists so the surface tier a later campaign builds is a fill-in
    /// against an already-widened constructor pair rather than a migration of
    /// every `Site` consumer. (This said "exactly as `Extent::Region` is";
    /// that variant does not exist — see [`Tier`]'s own doc.)
    /// Decision 0539.
    /// type-audit: bare-ok(identifier-text: name)
    pub fn derived(kind: SiteKind, name: Option<String>) -> Self {
        Self {
            kind,
            name,
            extent: Extent::Point,
            tier: Tier::Derived,
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
    /// Ranked over [`SiteKind`] alone — [`Tier`] never enters this ordering.
    /// A derived cave and a placed cave are equally salient *as caves*:
    /// tier is a generation-and-persistence fact, salience is a
    /// presentation fact, and this campaign keeps the two axes orthogonal.
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
        let s = Site::placed(SiteKind::Settlement, Some("Doaba".into()));
        let x = Site::placed(SiteKind::Exotic, None);
        let c = Site::placed(SiteKind::Cave, None);
        assert!(s.salience() > x.salience());
        assert!(x.salience() > c.salience());
    }

    /// Tier is orthogonal to salience: a derived and a placed site of the
    /// same kind rank identically, because salience ranks `SiteKind` alone
    /// (decision 0539 — tier must never leak into presentation).
    #[test]
    fn salience_ignores_tier() {
        let placed_cave = Site::placed(SiteKind::Cave, None);
        let derived_cave = Site::derived(SiteKind::Cave, None);
        assert_eq!(placed_cave.salience(), derived_cave.salience());
    }

    /// `Site::placed` must not silently drop or alter what it is given — a
    /// regression that swapped or discarded `kind`/`name` would still
    /// compile and would still pass every other test in this module.
    #[test]
    fn placed_round_trips_kind_and_name() {
        let named = Site::placed(SiteKind::Settlement, Some("Doaba".into()));
        assert_eq!(named.kind, SiteKind::Settlement);
        assert_eq!(named.name, Some("Doaba".to_string()));

        let unnamed = Site::placed(SiteKind::Cave, None);
        assert_eq!(unnamed.kind, SiteKind::Cave);
        assert_eq!(unnamed.name, None);
    }

    /// This campaign emits `Point` only (spec §7). **Currently vacuous**:
    /// [`Extent`] has exactly one variant, so any `Site::placed` that
    /// compiles necessarily sets it. Kept anyway as a FORWARD guard — it
    /// exists to catch a future `Site::placed` that defaults to a
    /// multi-facet variant once one is added, not to discriminate today.
    ///
    /// The variant is not merely unbuilt, it is **absent**: nothing named
    /// `Region` is in the enum, and H3's
    /// `windows/lab/tests/suite/site_density.rs::facets_per_site` is where
    /// adding one first bites — its exhaustive match feeds the site-density
    /// ceiling, so a multi-facet extent cannot land without re-deriving that
    /// baseline.
    #[test]
    fn a_placed_site_is_a_point() {
        assert_eq!(Site::placed(SiteKind::Cave, None).extent, Extent::Point);
    }

    /// `Site::placed` sets [`Tier::Placed`] — the only tier this campaign
    /// emits (decision 0539).
    #[test]
    fn placed_sets_tier_placed() {
        assert_eq!(Site::placed(SiteKind::Cave, None).tier, Tier::Placed);
    }

    /// `Site::derived` sets [`Tier::Derived`]. **Declared forward guard, not
    /// a discriminating test today**: no production call site in The
    /// Prospect calls `Site::derived` at all (decision 0539 — modelled and
    /// unused), so this only pins the
    /// constructor's own behaviour against a future edit that collapses
    /// both constructors to the same tier, the same role
    /// `a_placed_site_is_a_point` plays for `Extent`.
    #[test]
    fn derived_sets_tier_derived() {
        assert_eq!(Site::derived(SiteKind::Cave, None).tier, Tier::Derived);
    }
}

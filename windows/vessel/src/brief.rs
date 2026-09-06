//! The BRIEF: the one thing micro generation may read about a place besides its
//! address and the seed (Rose Window metaplan §1b.4). Macro answers *who holds
//! this land*; micro answers *what is standing here*; the brief is the seam.
//!
//! It is derived, never stored — which is why it does NOT carry every field a
//! future consumer might want. §1b.4's metaplan reserved a set of fields —
//! `cause`, `ended_by`, `founded`, `ended`, `tongue`, `deity`,
//! `peak_population`, `stratigraphy` — and argued for carrying all of them
//! from the start, "so that adding a consumer never changes the seam". That
//! argument only bites for types that PERSIST, and nothing here is
//! serialized: a campaign that needs a reserved field adds just that field,
//! with no save-format consequence and no epoch. Shipping every reserved
//! field as an `Option` at genesis, before any consumer existed, would have
//! been dead weight that reads as evidence of intent, so v1 shipped none of
//! them.
//!
//! **This header used to say the ruin signature was absent on purpose. The
//! Weft made that false, so this paragraph replaces the claim instead of
//! correcting it a second time.** `cause`, `ended_by` (as
//! [`RuinSignature::by_hand`]) and `ended` are carried now, folded into one
//! [`Brief::ruin`] rather than kept as loose separate fields. `peak_population`
//! is also carried, as a bare `u32` rather than an `Option` — see below.
//! The transient indoor frame carries one resolved copy so every chamber reads
//! the same fallible production result; that copy is likewise never serialized.
//! `founded`, `tongue`, `deity` and `stratigraphy` remain genuinely absent,
//! for the same reason: nothing here is serialized, so nothing forces them
//! in before a consumer needs them.
//!
//! **The structure grammar widened this list (The Cruck, Task 3), and the
//! shape of the widening matters more than its length.** `built` is now the
//! METHOD selector in `structure_at` — grammar for a built site, the wild
//! chain for everything else — as well as `describe_chamber`'s room/hollow
//! word; `site` is the existence gate (decision 0666). And `function`,
//! `notability`, `housemark` and `cold` are all read by
//! `structure::grammar::frame_for`, which turns them into a chamber TREE: what
//! rooms stand here and which room each one opens off. `cold` in particular
//! was "carried but read only by a debug assertion" until this campaign and is
//! now load-bearing — it is the axis that nests a dwelling's rooms on its
//! hearth. `tech` and `people` are still carried and not read at all.
//! `housemark` remains chamber-only in the sense that matters: it never
//! reaches locale selection or a fact writer.
//!
//! `peak_population` was added here when the `store` role's strongbox became
//! its first reader. Decision 0398 relaxed that gate, so the field is now
//! read only by [`Brief::is_populous`], whose value still reaches
//! `pattern::selection_for` on every chamber derivation and currently
//! selects nothing. It is kept for the same reason the doc above gives for
//! keeping the seam thin: removing it would be a second edit to undo the day
//! a population-gated pattern is written, and unlike the fields still absent
//! from this struct — `founded`, `tongue`, `deity`, `stratigraphy` — this
//! one has a live wire behind it.

use crate::housemark::{Housemark, HousemarkError};
use crate::site::{Site, SiteKind};
use hornvale_history::record::{
    CauseOfEnd, Ended, Function, Notability, OccupationRecord, TechHorizon,
};
use hornvale_kernel::{Facet, FacetId, Geosphere, KindId, NearestVertexIndex, Seed, Vertex};
use hornvale_locale::StrangeSite;
use hornvale_species::{SocietyVector, society_registry};
use hornvale_worldgen::{SiteReason, site_facet_for};
use std::collections::BTreeMap;
use std::fmt;

/// Why a production brief could not resolve a living occupation's culture.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum BriefError {
    /// A living occupation names a people with no authored society row.
    UnregisteredPeople(KindId),
    /// The authored society row cannot be classified as a housemark.
    InvalidHousemark {
        /// The people whose authored row could not be classified.
        people: KindId,
        /// The classification failure from the vessel-owned derivation.
        source: HousemarkError,
    },
}

impl fmt::Display for BriefError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::UnregisteredPeople(people) => {
                write!(f, "living people {} has no society row", people.0)
            }
            Self::InvalidHousemark { people, source } => {
                write!(
                    f,
                    "cannot derive a housemark for living people {}: {source}",
                    people.0
                )
            }
        }
    }
}

impl std::error::Error for BriefError {}

/// What a dead occupation leaves for a walker to read.
/// type-audit: bare-ok(flag: by_hand), bare-ok(count: ended)
#[derive(Clone, Debug, PartialEq)]
pub struct RuinSignature {
    /// Why it ended, when the record names a cause.
    pub cause: Option<CauseOfEnd>,
    /// When it ended, in the same units `Occupation::ended` carries.
    pub ended: f64,
    /// Whether another party ended it (`Ended::By`) rather than nature.
    pub by_hand: bool,
}

/// What macro history says about a place, reduced to the axes micro generation
/// indexes. A COORDINATE in a small orthogonal space — never a label drawn from
/// a catalogue of place types (§1b.4).
///
/// **No longer `Eq`, as of The Weft.** [`RuinSignature::ended`] is an `f64`,
/// and `f64` has no `Eq` impl (NaN), so a struct carrying one transitively
/// cannot derive it either. Nothing in this crate needed `Brief: Eq` — every
/// comparison here already went through `PartialEq` (`assert_eq!`,
/// `assert_ne!`) — so the derive is simply dropped rather than worked around.
/// type-audit: bare-ok(flag: built), bare-ok(flag: cold), bare-ok(count: peak_population)
#[derive(Clone, Debug, PartialEq)]
pub struct Brief {
    /// What the alive occupation here was for, if any occupation is alive.
    pub function: Option<Function>,
    /// The alive occupation's technological horizon.
    pub tech: Option<TechHorizon>,
    /// How notable the alive occupation is in its region.
    pub notability: Option<Notability>,
    /// The people occupying this place, if any.
    pub people: Option<KindId>,
    /// The living people's vessel-owned cultural reading, if any.
    pub housemark: Option<Housemark>,
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
    /// Decision 0666. For a settlement this mirrors [`Self::built`]; an exotic
    /// site and a cave are each the placed address
    /// `hornvale_worldgen::site_facet_for` gives them, under their own
    /// `hornvale_worldgen::SiteReason` so that a vertex warranting both does
    /// not put them at one facet.
    pub site: Option<Site>,
    /// The dead occupation standing here, if any. Absent on a living
    /// settlement and on empty ground.
    ///
    /// **Why this is here now and was not before.** This module's own header
    /// states the general rule this field follows: nothing here is
    /// serialized, so a campaign that needs a reserved field adds just that
    /// field, with no save-format consequence and no epoch. The Weft is the
    /// campaign that needed `cause`, and picked up `ended_by` and `ended`
    /// alongside it, since `cause`, `ended_by` and `ended` each describe the
    /// one dead occupation and belong together in one [`RuinSignature`]
    /// rather than as separate loose fields.
    pub ruin: Option<RuinSignature>,
}

impl Brief {
    /// Assemble a brief from already-resolved parts. Exists so the type can be
    /// unit-tested without a world; `brief_of` is the production path.
    /// type-audit: bare-ok(flag: built), bare-ok(flag: cold), bare-ok(count: peak_population)
    #[allow(clippy::too_many_arguments)] // `site` (Task 2, The Prospect) pushed this to 8, and `ruin` (Task 1, The Weft) to 9; the parameters ARE `Brief`'s fields, and the whole point of this constructor is to assemble them without a world to derive `site`/`ruin` from
    pub fn from_parts(
        function: Option<Function>,
        tech: Option<TechHorizon>,
        notability: Option<Notability>,
        people: Option<KindId>,
        housemark: Option<Housemark>,
        peak_population: u32,
        built: bool,
        cold: bool,
        site: Option<Site>,
        ruin: Option<RuinSignature>,
    ) -> Self {
        Self {
            function,
            tech,
            notability,
            people,
            housemark,
            peak_population,
            built,
            cold,
            site,
            ruin,
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
/// caller already holds one; the same reason `geo` is a parameter.
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
///
/// `seed` is the world's seed, the one thing `site_facet_for` needs from a
/// `World` and the only reason this function ever held one. It is a parameter
/// for exactly the reason the paragraph below gives for the occupation maps: the
/// caller has it, and a derivation path should not reach for a whole world to
/// read one field off it.
///
/// `living_occupations` is keyed by the exact packed settlement room.
/// `WorldContext::build` constructs it ONCE by joining
/// `hornvale_worldgen::occupations_by_vertex(world)` to the production
/// settlement roster. Looking a living room back up through
/// [`containing_vertex`] is deliberately forbidden: settlement placement and
/// the cube-sphere room mesh are independent, so that geometric reverse may
/// name a neighbour rather than the settlement's source vertex.
///
/// `occupation_history` is that same already-hoisted source register, retained
/// by geosphere vertex for the ruin read. A ruin is an extent around an ended
/// occupation rather than a production settlement-room identity, so it must
/// use [`containing_vertex`] and must not be collapsed onto the living-room
/// index. Both maps are derived once in `WorldContext::build`; neither is a
/// hidden cache in this derivation path. Before The Terrier, this function
/// rebuilt the source register on every call (8.7-26 ms, two to five calls per
/// indoor turn); preserving both views keeps that recovered cost while joining
/// Housemark's exact-room identity to The Weft's ruin extent.
///
/// A living occupation also resolves its `people` through
/// `hornvale_species::society_registry` exactly once while assembling the
/// returned [`Brief`]. Missing rows and unclassifiable radii are contextual
/// [`BriefError`]s; an unoccupied place receives neither people nor housemark.
/// No fallback uses `SocietyVector::MANIKIN`: the manikin is nobody, and a ruin
/// does not silently acquire occupants.
/// type-audit: bare-ok(count: walk_depth)
#[allow(clippy::too_many_arguments)] // The two occupation views plus `index` make ten parameters; each is a value the CALLER already holds and must not re-derive — bundling them into a public one-reader context would only hide the same dependency list
pub fn brief_of(
    living_occupations: &BTreeMap<FacetId, OccupationRecord>,
    occupation_history: &BTreeMap<Vertex, Vec<OccupationRecord>>,
    geo: &Geosphere,
    index: &NearestVertexIndex,
    place: &Facet,
    terrain: &dyn crate::liveness::Terrain,
    walk_depth: u32,
    seed: Seed,
    exotic_sites: &[StrangeSite],
    cave_sites: &[Vertex],
) -> Result<Brief, BriefError> {
    let locale = crate::depth::truncate_to_walk(place, walk_depth);
    let built = terrain.is_built(&locale);
    let cold = terrain.is_cold(&locale);
    //
    // NOTE ON COST: like the occupation map below, this re-derives every placed
    // site's address on every call, and a miss walks the whole list.
    //
    // THE LIST IS 5-15x LONGER THAN THIS NOTE USED TO SAY. It read "the budget
    // caps placement at 1% of land vertices, so a miss walks the whole list".
    // `BUDGET_FRACTION = 0.01` (`windows/locale/src/budget.rs`) binds EXOTIC
    // sites only — ~100-180 of them. Caves are uncapped: H3
    // (`windows/lab/tests/suite/site_density.rs`) measures 874/1,647/1,681/
    // 1,116/2,440 cave vertices on seeds 42/13/7/1/100, i.e. 7-13% of land
    // vertices, and `cave_sites` is the parameter this function scans. The
    // doc above already says cave_sites is "much the longer roster of the two
    // — ~870-2,440 vertices against ~100"; this note contradicted it two
    // paragraphs later by pricing the whole scan at the exotic budget.
    //
    // `brief_of` runs on every `look` and every `enter` (`session.rs`), so the
    // per-turn cost is real. Same remedy if a profile ever shows it: hoist the
    // placed set to the caller (`Session` already holds `built` exactly that
    // way), never a cache inside a derivation.
    let placed_at = |vertex: Vertex, reason: SiteReason| {
        site_facet_for(vertex, reason, seed, geo, walk_depth) == locale
    };
    // Where a facet holds more than one candidate, `Site::salience` — and
    // ONLY `Site::salience` — decides which one wins (spec §6). This used to
    // be an if/else chain (settlement, then exotic, then cave) that stated
    // the same order `Site::salience` states, independently: change one and
    // the other silently keeps the old order, the same two-sources-of-truth
    // shape the two cave predicates this campaign found and fixed earlier
    // were. Assembling every candidate the facet could hold and taking the
    // maximum BY `Site::salience` makes that function load-bearing rather
    // than aspirational, and reduces this call site to consulting exactly
    // one AUTHORITY for the order. It is not the only STATEMENT of it —
    // `SiteKind`'s own `derive(Ord)` (`site.rs`) declares
    // `Cave < Exotic < Settlement`, identical to this ranking, and nothing
    // enforces that the two agree if either changes. Two statements, one
    // authority: this call site reads only `Site::salience`, never the
    // derived `Ord`.
    // `salience` returns `u8`, so the comparison is exact — no float, no
    // `total_cmp` — and no tie is reachable today: each kind contributes at
    // most one candidate here, and every kind's own salience is distinct.
    //
    // NOTE ON COST: this array's three elements are evaluated unconditionally,
    // where the if/else chain it replaced short-circuited — a BUILT facet used
    // to stop at the settlement arm and never touch the `exotic_sites` or
    // `cave_sites` scans, and now runs both `.any(…)` scans regardless. Measured
    // at or under noise (~0.2-0.3 s over 300 turns, interleaved release
    // binaries) and judged not worth fixing at this campaign's scale; recorded
    // here so the next reader does not re-derive it.
    let candidates = [
        // THE NAME COMES FROM THE PLACE, NOT FROM THE POSSESSION (Task 7).
        // `Terrain::settlement_name` reads the injected settlement-territory
        // map, keyed by ROOM — the same entry `is_built` just tested — so the
        // name belongs to the facet. The tempting alternative,
        // `liveness::village_or_fallback`, resolves the possessed BODY's own
        // home village: identical at the flagship, because a possession
        // starts in its own village, and a one-turn observable falsehood
        // anywhere else. A cave and an exotic site take `None`: neither has a
        // name and neither may borrow one.
        built.then(|| {
            Site::placed(
                SiteKind::Settlement,
                terrain.settlement_name(&locale).map(str::to_string),
            )
        }),
        exotic_sites
            .iter()
            .any(|site| placed_at(Vertex(site.vertex), SiteReason::Exotic))
            .then(|| Site::placed(SiteKind::Exotic, None)),
        cave_sites
            .iter()
            .any(|&vertex| placed_at(vertex, SiteReason::Cave))
            .then(|| Site::placed(SiteKind::Cave, None)),
    ];
    let site = candidates.into_iter().flatten().max_by_key(Site::salience);
    let vertex = containing_vertex(&locale, geo, index);
    // Where more than one occupation at this vertex has ended, the most
    // RECENT ending is the ruin a walker reads — an older ruin buried under a
    // younger one is not what stands here. `total_cmp`, never `partial_cmp`:
    // float ordering must be deterministic (constitutional, see CLAUDE.md).
    let ruin = vertex
        .and_then(|vertex| occupation_history.get(&vertex))
        .and_then(|recs| {
            recs.iter()
                .filter(|r| r.core.ended.is_some())
                .max_by(|a, b| a.core.ended.unwrap().total_cmp(&b.core.ended.unwrap()))
        })
        .map(|r| RuinSignature {
            cause: r.core.cause,
            ended: r.core.ended.expect("filtered to Some above"),
            by_hand: matches!(r.ended_by, Ended::By(_)),
        });
    let alive = locale
        .pack()
        .ok()
        .and_then(|room| living_occupations.get(&room));
    Ok(match alive {
        Some(o) => {
            let societies = society_registry();
            let housemark = housemark_for(o.core.people, &societies)?;
            Brief::from_parts(
                Some(o.core.function),
                Some(o.core.tech),
                Some(o.core.notability),
                Some(o.core.people),
                Some(housemark),
                o.core.peak_population,
                built,
                cold,
                site,
                ruin,
            )
        }
        None => Brief::from_parts(None, None, None, None, None, 0, built, cold, site, ruin),
    })
}

fn housemark_for(
    people: KindId,
    societies: &hornvale_kernel::ComponentStore<KindId, SocietyVector>,
) -> Result<Housemark, BriefError> {
    let society = societies
        .get(&people)
        .copied()
        .ok_or(BriefError::UnregisteredPeople(people))?;
    Housemark::try_from_society(society)
        .map_err(|source| BriefError::InvalidHousemark { people, source })
}

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_history::record::{
        Ended, Founding, Function, Notability, Occupation, TechHorizon,
    };
    use hornvale_kernel::{ComponentStore, EntityId};
    use hornvale_species::{Sociality, StatusBasis};

    struct StubTerrain;

    impl crate::liveness::Terrain for StubTerrain {
        fn elevation(&self, _room: &Facet) -> f64 {
            0.0
        }

        fn is_fresh_water(&self, _room: &Facet) -> bool {
            false
        }

        fn temperature(&self, _room: &Facet, _day: hornvale_kernel::WorldTime) -> f64 {
            25.0
        }
    }

    fn eid(value: u64) -> EntityId {
        EntityId(std::num::NonZeroU64::new(value).expect("test entity ids are nonzero"))
    }

    fn production_brief_for(people: KindId) -> Result<Brief, BriefError> {
        let geo = Geosphere::new(0);
        let index = NearestVertexIndex::new(&geo);
        let place = Facet {
            face: 0,
            path: Vec::new(),
        };
        let vertex = containing_vertex(&place, &geo, &index)
            .expect("a depth-zero facet resolves on a depth-zero geosphere");
        let occupation = OccupationRecord {
            core: Occupation {
                people,
                site: vertex,
                founded: 0.0,
                ended: None,
                peak_population: 42,
                tech: TechHorizon::Classical,
                function: Function::Trade,
                deity: None,
                tongue: None,
                cause: None,
                notability: Notability::Common,
                delve_depth_m: 0.0,
            },
            id: eid(1),
            founded_from: Founding::Genesis(vertex),
            ended_by: Ended::Nature,
        };
        let occupation_history = [(vertex, vec![occupation.clone()])].into_iter().collect();
        let occupations = [(place.pack().expect("the test room packs"), occupation)]
            .into_iter()
            .collect();
        brief_of(
            &occupations,
            &occupation_history,
            &geo,
            &index,
            &place,
            &StubTerrain,
            0,
            Seed(42),
            &[],
            &[],
        )
    }

    #[test]
    fn from_parts_assigns_the_occupation_axes_and_flags() {
        let b = Brief::from_parts(
            Some(Function::Trade),
            Some(TechHorizon::Classical),
            Some(Notability::Seat),
            None,
            None,
            900,
            true,
            true,
            None,
            None,
        );
        assert_eq!(b.function, Some(Function::Trade));
        assert_eq!(b.tech, Some(TechHorizon::Classical));
        assert_eq!(b.notability, Some(Notability::Seat));
        assert_eq!(b.housemark, None, "synthetic construction stays uncultured");
        assert!(b.built);
        assert!(b.cold);
    }

    #[test]
    fn from_parts_with_no_occupation_axes_still_carries_climate() {
        let b = Brief::from_parts(None, None, None, None, None, 0, false, true, None, None);
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
            None,
            0,
            true,
            false,
            None,
            None,
        );
        let b = Brief::from_parts(
            Some(Function::Fort),
            Some(TechHorizon::Classical),
            None,
            None,
            None,
            0,
            true,
            false,
            None,
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
        let built = Brief::from_parts(
            None, None, None, None, None, 0, true, false, built_site, None,
        );
        let wild = Brief::from_parts(None, None, None, None, None, 0, false, false, None, None);
        assert_eq!(
            built.site.as_ref().map(|site| site.kind),
            Some(SiteKind::Settlement)
        );
        assert_eq!(wild.site, None);
    }

    #[test]
    fn a_living_occupation_carries_the_housemark_of_its_authored_society_row() {
        // Kobold's communal/inward row deliberately differs from MANIKIN's
        // command/plain default, so this catches a fallback to nobody as well
        // as a missing derivation.
        let people = KindId("kobold");
        let expected = Housemark::try_from_society(
            *society_registry()
                .get(&people)
                .expect("kobold has an authored society row"),
        )
        .expect("the authored kobold row occupies an admitted band");

        let brief = production_brief_for(people).expect("a registered living people has a brief");

        assert_eq!(brief.people, Some(people));
        assert_eq!(brief.housemark, Some(expected));
    }

    #[test]
    fn an_empty_production_occupation_register_carries_no_people_or_housemark() {
        let geo = Geosphere::new(0);
        let index = NearestVertexIndex::new(&geo);
        let place = Facet {
            face: 0,
            path: Vec::new(),
        };
        let occupations: BTreeMap<FacetId, OccupationRecord> = BTreeMap::new();
        let occupation_history: BTreeMap<Vertex, Vec<OccupationRecord>> = BTreeMap::new();

        let brief = brief_of(
            &occupations,
            &occupation_history,
            &geo,
            &index,
            &place,
            &StubTerrain,
            0,
            Seed(42),
            &[],
            &[],
        )
        .expect("an unoccupied production place has a brief");

        assert_eq!(
            (brief.people, brief.housemark),
            (None, None),
            "nobody must not acquire MANIKIN's culture"
        );
    }

    #[test]
    fn a_living_occupation_with_no_society_row_refuses_with_its_people_id() {
        let people = KindId("unregistered-test-people");

        let error = production_brief_for(people)
            .expect_err("a living people with no society row must not receive a brief");

        assert_eq!(error, BriefError::UnregisteredPeople(people));
        assert!(error.to_string().contains(people.0), "{error}");
    }

    #[test]
    fn an_unclassifiable_society_row_refuses_with_its_people_id() {
        let people = KindId("invalid-radius-test-people");
        let societies: ComponentStore<KindId, SocietyVector> = [(
            people,
            SocietyVector {
                sociality: Sociality::Communal,
                status_basis: StatusBasis::Knowledge,
                in_group_radius: 0.4,
            },
        )]
        .into_iter()
        .collect();

        let error = housemark_for(people, &societies)
            .expect_err("an unassigned radius band must not receive a housemark");

        assert!(
            matches!(error, BriefError::InvalidHousemark { people: p, .. } if p == people),
            "{error:?}"
        );
        assert!(error.to_string().contains(people.0), "{error}");
        assert!(error.to_string().contains("0.4"), "{error}");
    }
}

//! The atlas view (Task 5): the world as drawn, with settlements appearing
//! as they are placed.
//!
//! # `can_speak`: silent through astronomy, speaking from terrain on
//!
//! Contract rule 2 ("show what EXISTS — never a placeholder for what does
//! not yet") matters most here of the four views: at [`BuildDepth::Astronomy`]
//! there is no [`hornvale_terrain::GeneratedTerrain`] at all
//! (`artifacts.terrain` is `None`), so there is no honest raster to draw. An
//! empty plate at that rung would read as a broken map, not as "the terrain
//! hasn't generated yet" — so [`AtlasView::can_speak`] answers `false` there
//! and the frame skips this view entirely, exactly as the trait's own doc
//! prescribes. From [`BuildDepth::Terrain`] on, `artifacts.terrain` is always
//! `Some` (`RungArtifacts`'s own doc), so this view speaks at every rung from
//! there through [`BuildDepth::Full`].
//!
//! # The route chosen for terrain and features: `plate::draw_with`
//!
//! This view draws nothing of its own — [`crate::plate::draw_with`] is the
//! full renderer The Quadrat already built (terrain raster plus the
//! settlement/cave feature layer, composed), and writing a second one here
//! would be exactly the "do not write a second renderer" the task brief
//! warns against. `render` below does the READS `draw_with` needs (the
//! settlement roster off the ledger, the discovery gate, the frame, the
//! window) and hands them over.
//!
//! # Memoisation: `&mut self` for the `NearestVertexIndex`
//!
//! [`View::render`]'s own doc states the doctrine this view exists to use:
//! building a [`NearestVertexIndex`] costs ~200 ms
//! (`crate::plate`'s module doc), and [`super::Frame::observe`] renders every
//! speaking view at every rung it can speak at — so an atlas built at
//! `Terrain`, `Settlements` and `Full` without a memo would pay that cost
//! three times per startup.
//!
//! **`windows/worldgen/src/lib.rs`'s own "Sculpt the terrain ONCE here and
//! KEEP it" comment on `build_to` is a claim about VALUE identity (the
//! content is byte-identical to a `terrain_of` re-derivation), not about
//! ADDRESS identity — and this module's doc used to conflate the two.**
//! `build_world_observed`'s real observer calls, measured directly by
//! [`tests::the_real_build_observer_reports_how_often_terrain_actually_moves`],
//! show the `Terrain`-rung call receiving the "terrain" stage's own local
//! (`lib.rs:8031`), and the `Settlements`/`Full`-rung calls both receiving a
//! DIFFERENT, shared address — the "climate+settlements" stage
//! (`lib.rs:8098`) takes that original local BY MOVE and returns it inside a
//! tuple, and a move through a closure return does not preserve a stack
//! address. So one real session sees exactly **two** distinct addresses
//! across the three observer calls, not one: the memo this view keeps
//! rebuilds ONCE partway through a session (`Terrain` → `Settlements`), then
//! is reused unchanged for `Full`. The saving is real (one `NearestVertexIndex`
//! build avoided at `Full`, not the two the original wording implied), and
//! nothing renders wrong either way — see the next paragraph.
//!
//! **The memo is keyed on the `GeneratedTerrain` reference's own address**,
//! not on the world's seed or on nothing at all. This key is honest about
//! what it can and cannot promise: it never serves a stale index for the
//! wrong mesh (an address mismatch always rebuilds, including the
//! mid-session rebuild measured above), so a render handed a DIFFERENT
//! terrain reference — whether that is a genuinely different build (a new
//! game, or a second world in a test) or, as measured, the SAME build's
//! value having moved under it — safely rebuilds rather than guessing. That
//! safety does not depend on `build_to` ever achieving true address
//! stability; it depends only on "same address implies same content", which
//! The Single Sculpt's value-identity guarantee upholds regardless of how
//! many times the value itself is relocated. Keying on the world's seed
//! instead was considered and rejected: it would be correct for this
//! campaign's own worlds (one seed, one set of pins, one build) but would
//! silently misbehave the day two DIFFERENT `GeneratedTerrain`s ever shared a
//! seed (a dev tool rebuilding the same seed under different pins, say) —
//! identity of the actual value handed in is the only key that cannot go
//! stale under a hypothetical it does not need to rule out. A
//! content-derived key (e.g. `geo.vertex_count()` plus some cheap hash) was
//! also considered and rejected as strictly worse here: it would need to be
//! computed on every call to decide whether to rebuild, which is exactly the
//! per-call cost this memo exists to avoid paying.
//!
//! # The discovery decision: every placed settlement is visible
//!
//! [`crate::plate::draw_feature_layer`] gates every point site on a
//! [`Discovered`] roster (`crate::plate`'s own doc: "nothing is drawn and
//! then hidden"), and that roster is a real design decision for this view,
//! not a property that falls out of the plumbing. The overture is the world
//! being SHOWN before play begins — there is no possession, no walk band, no
//! session in which "discovery" (a possessed agent encountering a place) has
//! started happening yet. So [`AtlasView::render`] marks every settlement the
//! ledger has placed as discovered: the atlas's job is to show what genesis
//! built, and an overview that hid a settlement the world already committed
//! would be contract rule 2's failure in the opposite direction — a
//! placeholder standing in for something that *does* exist. This is the
//! controller's own reading and is deliberately made structural rather than
//! left implicit: [`plate::settlements_of`] and [`discovered_of`] are two separate,
//! narrow functions, and the test module asserts the resulting behaviour
//! (settlements appear once placed) rather than merely exercising the code
//! path.
//!
//! # Caves, volcanoes and waterfalls: skipped, deliberately
//!
//! `Driver::start_from_world` builds its own cave roster with a full scan of
//! every mesh vertex (`(0..geo.vertex_count())...filter(|&c|
//! terrain.cave_at(c).is_some())`, `driver.rs`), because a possessed agent can
//! eventually explore into one. `AtlasView::render` passes an empty cave
//! roster instead and pays no per-render mesh scan for a feature this view
//! does not draw.
//!
//! **The reason is a genre convention, not a discovery argument** (fix
//! round 1: the previous wording here leaned on "an undiscovered cave is
//! buried, not on the surface" — the same player's-eye discovery framing the
//! settlement decision one paragraph above this deliberately overrides. The
//! two cannot both stand in one document: if discovery does not gate what
//! this view shows, it cannot be the reason a cave is withheld either.) The
//! actual reason is simpler and does not depend on discovery at all: a
//! printed atlas draws coastlines, mountains and towns, never the inside of
//! an unopened cave system — that is a fact about what an ATLAS is, not
//! about what the viewer has or has not encountered. This view follows that
//! convention.
//!
//! **The Legend's Task 7 added volcano and waterfall rosters to
//! [`plate::draw_with`]'s own parameter list**, and this view passes empty
//! ones for both, for the identical reason: neither is a discovery read
//! (`Driver::start_from_world` scans `features`/`GeneratedTerrain::
//! waterfalls()` once at genesis, the same "ground truth, no per-render
//! scan" shape caves already follow here), so the cost argument above
//! applies unchanged, and an atlas that draws coastlines, mountains and
//! towns is under no more obligation to draw a volcano's anchor or a
//! waterfall's vertex than it is a cave mouth.
//!
//! **This is a real, deliberate narrowing of spec §4's literal wording**
//! ("settlements and caves appearing as placed"), not an oversight — worth
//! carrying into the campaign chronicle as a named omission rather than
//! rediscovering it later as a missing feature. The task brief's own tests
//! do not ask for a cave glyph either.
//!
//! # The window: the whole globe, not a scrolled fragment
//!
//! [`crate::plate::Window`] names a MESH RUNG and an origin into that rung's
//! own virtual chart (`crate::plate`'s own doc: "the chart is a property of
//! the rung; the plate is a window onto it"). `Driver::start_from_world`
//! picks [`crate::plate::BAND_B_RUNG`] at origin `(0, 0)` because its session
//! immediately re-centres that window on the possessed agent
//! (`enter_map`/`centre_band_b_on_the_observer`) — but the overture has no
//! agent to centre on, and `BAND_B_RUNG` is the FINEST rung the client draws,
//! whose virtual chart is one drawn tile per mesh facet: an un-centred window
//! at that rung would show one arbitrary, likely empty, patch of the planet.
//!
//! [`fit_depth`] picks the FINEST rung whose own virtual chart
//! (`crate::plate::virtual_dims`) still fits entirely inside the drawn
//! `w`x`h` plate, so `origin (0, 0)` shows the chart's every cell at least
//! once — the whole globe, at the coarsest resolution that buys full
//! coverage without discarding detail the plate has room for. This is a
//! structural guarantee, not a fact checked against one seed: it holds for
//! any world, at any drawn size, because it never depends on where a
//! settlement or a coastline happens to sit. (Verified empirically too,
//! against seed 42's real `Full`-depth settlement roster, before relying on
//! the argument alone — see the task report.)

use std::collections::BTreeSet;

use hornvale_game_core::Grid;
use hornvale_kernel::{Geosphere, NearestVertexIndex, Vertex, World};
use hornvale_worldgen::{BuildDepth, RungArtifacts};

use crate::discovery::Discovered;
use crate::mercator;
use crate::overture::view::View;
use crate::plate::{self, Window};

/// The finest mesh rung whose own virtual chart
/// ([`plate::virtual_dims`]) still fits entirely inside a `w`x`h` plate —
/// see the module doc's "The window" section for why this is the rung an
/// un-centred, un-scrolled overview wants.
///
/// Starts at rung 0 (whose own chart is 6x6 — see [`plate::virtual_dims`])
/// and grows one rung at a time while the NEXT rung would still fit, capped
/// at
/// [`plate::BAND_B_RUNG`] (the finest rung the client draws at all, per
/// [`plate::virtual_dims`]'s own doc) so this terminates even for a plate
/// far larger than any terminal in practice.
fn fit_depth(w: u16, h: u16) -> u32 {
    let w = u32::from(w.max(1));
    let h = u32::from(h.max(1));
    let mut depth = 0u32;
    while depth < plate::BAND_B_RUNG {
        let (next_w, next_h) = plate::virtual_dims(depth + 1);
        if next_w > w || next_h > h {
            break;
        }
        depth += 1;
    }
    depth
}

/// Every entry of `sites`, marked discovered — see the module doc's
/// "The discovery decision" section for why the overture atlas shows every
/// placed settlement rather than gating on session discovery, which has not
/// started yet.
///
/// Takes each entry's own [`plate::MapSite::feature_id`] (The Prospect,
/// Task 8; before it, the roster's `BTreeMap<Vertex, u64>` KEYS) rather than
/// building `FeatureId::Settlement` here. That is not a tidy-up: this view's
/// roster is settlements-only *by what it passes to* `plate::sites_of`, and
/// minting the identity here as well would put a second assumption about
/// WHICH kind is present in a function that cannot see the roster's
/// construction. Reading the id off the entry keeps this correct if the view
/// ever admits a second kind, and keeps it honest about the one it admits
/// today.
fn discovered_of(sites: &[plate::MapSite]) -> Discovered {
    let mut discovered = Discovered::default();
    for site in sites {
        discovered.record(site.feature_id());
    }
    discovered
}

/// The expensive derived structures [`AtlasView::render`] memoises across
/// calls: a clone of the built world's own mesh and the index that answers
/// "which vertex is nearest" against it. See the module doc's "Memoisation"
/// section for why `terrain_key` — the address of the
/// [`hornvale_terrain::GeneratedTerrain`] this was built from — is the invalidation key.
struct AtlasMemo {
    /// The address of the [`hornvale_terrain::GeneratedTerrain`] this memo was built from —
    /// an invalidation key, never dereferenced or exposed. Private, so it
    /// carries no `type-audit:` tag (decision 0027/0028 binds `pub`-boundary
    /// primitives only; see `tools/type-audit/src/extract.rs::is_bare_pub`).
    terrain_key: usize,
    /// The mesh, cloned once per distinct `terrain_key`
    /// (`GeneratedTerrain::geosphere` hands back a borrow; [`draw_with`]'s
    /// own caller-builds-it-once convention wants an owned copy to keep).
    geo: Geosphere,
    /// The nearest-vertex index built once over `geo`.
    nearest: NearestVertexIndex,
}

/// The atlas: the world as drawn, with settlements appearing as they are
/// placed. See the module doc for the four decisions this view makes (the
/// rung it speaks from, the discovery gate, the cave omission, and the
/// window).
#[derive(Default)]
pub struct AtlasView {
    /// The memoised mesh and index — see [`AtlasMemo`]. `None` until the
    /// first render that has terrain to build one from.
    memo: Option<AtlasMemo>,
}

impl View for AtlasView {
    fn name(&self) -> &'static str {
        "atlas"
    }

    /// Speaks from [`BuildDepth::Terrain`] on — see the module doc's
    /// `can_speak` section for why astronomy alone has nothing to draw.
    fn can_speak(&self, rung: BuildDepth) -> bool {
        rung >= BuildDepth::Terrain
    }

    fn render(
        &mut self,
        world: &World,
        _rung: BuildDepth,
        artifacts: RungArtifacts<'_>,
        w: u16,
        h: u16,
    ) -> Grid {
        let w = w.max(1);
        let h = h.max(1);

        // Defensive only: the frame never calls `render` when `can_speak`
        // returned `false` (the trait's own contract), so this arm is
        // unreached on the shipped path — but a view must not panic a
        // caller that violates its own contract, so an honest blank grid is
        // returned rather than an unwrap.
        let Some(terrain) = artifacts.terrain else {
            return Grid::new(w, h);
        };

        let terrain_key = std::ptr::from_ref(terrain) as usize;
        let stale = !matches!(&self.memo, Some(memo) if memo.terrain_key == terrain_key);
        if stale {
            let geo = terrain.geosphere().clone();
            let nearest = NearestVertexIndex::new(&geo);
            self.memo = Some(AtlasMemo {
                terrain_key,
                geo,
                nearest,
            });
        }
        // `stale` just ensured `self.memo` is `Some` on every path.
        let memo = self.memo.as_ref().expect("memo populated above");
        let geo = &memo.geo;
        let nearest = &memo.nearest;

        let locked = world
            .ledger
            .find(hornvale_astronomy::facts::TIDALLY_LOCKED)
            .next()
            .is_some();
        let frame = mercator::frame_for(locked);
        let window = Window {
            depth: fit_depth(w, h),
            origin_col: 0,
            origin_row: 0,
        };

        // SETTLEMENTS ONLY — see the module doc's "Caves, volcanoes and
        // waterfalls: skipped, deliberately" section, which is the same
        // genre-convention argument for every non-settlement kind: a printed
        // atlas draws coastlines, mountains and towns, never the
        // mesh-projected extras a possessed agent's own map layers in as
        // they are encountered. The Prospect's Task 8 adds a third site
        // kind (the placed exotic site) and this view withholds it under the
        // identical argument, by passing empty PLACED rosters rather than by
        // any filter downstream.
        //
        // The walk depth handed to `sites_of` is therefore never consumed:
        // it addresses placed sites, and there are none. `plate::BAND_B_RUNG`
        // is passed as the honest "the canonical globe's walk band" value
        // rather than an invented sentinel, and `sites_of`'s own doc records
        // that a caller in this position may pass any depth.
        let sites = plate::sites_of(world, geo, nearest, &[], &[], plate::BAND_B_RUNG);
        let discovered = discovered_of(&sites);
        let volcanoes = BTreeSet::new();
        let waterfalls: Vec<Vertex> = Vec::new();

        // NO SPECTRAL CONTEXT HERE, AND THAT IS THE ANSWER THIS VIEW GIVES
        // (The Wash, Task 6). `plate::Spectral` takes `Option<&LocaleContext>`
        // exactly so the two shipped callers of `draw_with` may answer
        // differently, and this one answers `None`.
        //
        // **What it costs, stated rather than hidden:** land tiles claim no
        // colour at all in the overture. `RELIEF_COLORS` — the six-entry
        // elevation ramp this view used to paint from — is deleted, so
        // "fall back to the ladder" is not on the menu; the honest fallback
        // is no ink, and relief is carried by the glyph ladder, which is
        // decision 0389's whole point. Ocean and salt basin keep their
        // invented palette claims (`plate::color_for`'s own doc), so the
        // coastline still reads in colour.
        //
        // **Why not build one. The reason is COST, at every rung this view
        // speaks at.** `LocaleContext::build_from` builds a second
        // `NearestVertexIndex` (~200 ms — the cost this module's whole memo
        // exists to avoid paying twice) plus a `StrangenessBudget`, per
        // distinct terrain, on the STARTUP path, and `Frame::observe`
        // renders every speaking view at every rung it can speak at. Paying
        // that so a progressive title sequence can tint its land is the
        // wrong trade; the live session, which is where a reader actually
        // studies the map, gets the real thing.
        //
        // **Availability rules out only the FIRST rung, and this comment
        // used to claim more than that** (fix round 1). `build_from` needs a
        // `GeneratedClimate`, and `RungArtifacts::climate` is `Some` iff the
        // fired rung is at least `BuildDepth::Settlements` — so at
        // `BuildDepth::Terrain` there is genuinely no climate at any price.
        // But `can_speak` admits `rung >= Terrain`, so this view also
        // renders at `Settlements` and `Full`, where a climate IS on the
        // artifacts. "Could not have one at any price" was true of one rung
        // out of three and was written as though it settled the question;
        // the cost argument above is what actually settles it, and it holds
        // at all three.
        let light = plate::PlateLight::for_terminal();
        let mut spectral = light.unlit();

        plate::draw_with(
            terrain,
            geo,
            nearest,
            &frame,
            &window,
            w,
            h,
            plate::colour_allowed(),
            &sites,
            &volcanoes,
            &waterfalls,
            &discovered,
            &mut spectral,
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_astronomy::SkyPins;
    use hornvale_kernel::Seed;
    use hornvale_terrain::TerrainPins;
    use hornvale_worldgen::{
        BuildArtifacts, SettlementPins, WorldComponents, build_world_observed,
        build_world_to_with_artifacts,
    };
    use std::sync::OnceLock;

    /// A real seed-42 build's artifacts at `depth`, built once per test
    /// binary — the same reasoning `sky`'s own `world_at` gives (a genuine
    /// committed ledger and a genuine sculpted mesh, not a synthetic
    /// stand-in), extended to keep the terrain artifact alongside the world
    /// since this view's `render` needs both.
    fn artifacts_at(depth: BuildDepth) -> &'static BuildArtifacts {
        static TERRAIN: OnceLock<BuildArtifacts> = OnceLock::new();
        static FULL: OnceLock<BuildArtifacts> = OnceLock::new();
        let build = || {
            let wc = WorldComponents::assemble().expect("canonical registries are well-formed");
            build_world_to_with_artifacts(
                Seed(42),
                &SkyPins::default(),
                &TerrainPins::default(),
                &SettlementPins::default(),
                &wc,
                depth,
            )
            .expect("seed 42 builds")
        };
        match depth {
            BuildDepth::Terrain => TERRAIN.get_or_init(build),
            BuildDepth::Full => FULL.get_or_init(build),
            other => panic!("no cached build for {other:?}; add one deliberately"),
        }
    }

    fn terrain_world() -> &'static BuildArtifacts {
        artifacts_at(BuildDepth::Terrain)
    }

    fn full_world() -> &'static BuildArtifacts {
        artifacts_at(BuildDepth::Full)
    }

    fn atlas_view() -> AtlasView {
        AtlasView::default()
    }

    /// `RungArtifacts` borrowing `built`'s own terrain — the same shape
    /// [`super::View::render`]'s real caller (`Frame::observe`) hands a
    /// view, built here from a stored [`BuildArtifacts`] rather than from a
    /// live build.
    fn rung_artifacts(built: &BuildArtifacts) -> RungArtifacts<'_> {
        RungArtifacts {
            terrain: built.terrain.as_ref(),
            climate: built.climate.as_ref(),
        }
    }

    #[test]
    fn the_atlas_is_silent_before_terrain_and_speaks_after() {
        // Contract rule 2, on the view where it matters most: at rung 0
        // there is no terrain, so the atlas must DECLARE it cannot speak
        // rather than drawing an empty plate that reads as a broken map.
        assert!(!atlas_view().can_speak(BuildDepth::Astronomy));
        assert!(atlas_view().can_speak(BuildDepth::Terrain));
    }

    #[test]
    fn settlements_appear_in_the_atlas_only_once_placed() {
        // The prefix property, visible: a settlement glyph before the
        // settlements rung would be a placeholder for something that does
        // not exist. Two fresh views (each is a memo scoped to one
        // terrain reference) rather than one reused across two different
        // builds — a real session never hands one view two different
        // worlds, so this does not exercise memo invalidation; that is
        // `the_memo_rebuilds_for_a_different_terrain_reference`'s job below.
        let terrain_built = terrain_world();
        let full_built = full_world();

        let at_terrain = atlas_view().render(
            &terrain_built.world,
            BuildDepth::Terrain,
            rung_artifacts(terrain_built),
            78,
            20,
        );
        let at_full = atlas_view().render(
            &full_built.world,
            BuildDepth::Full,
            rung_artifacts(full_built),
            78,
            20,
        );

        let terrain_text = at_terrain.to_plain_text();
        let full_text = at_full.to_plain_text();

        assert!(
            !terrain_text.contains(plate::SETTLEMENT_MAJOR_GLYPH)
                && !terrain_text.contains(plate::SETTLEMENT_MINOR_GLYPH),
            "a settlement glyph appeared before any settlement was placed"
        );
        // NON-VACUITY: the assertion above would also pass against a blank
        // or broken grid that drew nothing at all. Prove the terrain layer
        // actually painted a real raster. The Legend (Task 6) retired the
        // `~` ocean / `.` land binary `crate::plate`'s module doc used to
        // name as its whole vocabulary for a water-class-and-elevation-band
        // one, so "drew land" is no longer one hardcoded character — it is
        // any drawn glyph that is not the ocean mark.
        assert!(
            terrain_text.chars().any(|c| c == '~'),
            "the terrain layer drew no ocean at all: {terrain_text:?}"
        );
        assert!(
            terrain_text.chars().any(|c| !c.is_whitespace() && c != '~'),
            "the terrain layer drew no land at all: {terrain_text:?}"
        );

        assert!(
            full_text.contains(plate::SETTLEMENT_MAJOR_GLYPH)
                || full_text.contains(plate::SETTLEMENT_MINOR_GLYPH),
            "settlements never appeared even at Full"
        );
    }

    #[test]
    fn the_memo_rebuilds_for_a_different_terrain_reference() {
        // `&mut self` is a memo, not a licence (`View::render`'s own
        // doctrine): two DIFFERENT builds must not share one view's cached
        // index, or a second world would be drawn against the first
        // world's mesh. Render the `Terrain` build, then the `Full` build,
        // on the SAME view — a stale-memo bug would either panic (the
        // index's own bucket lookups assume `geo`'s vertex count) or,
        // worse, silently draw the wrong settlements. Neither test above
        // exercises this: each uses a fresh view per world.
        let mut view = atlas_view();
        let terrain_built = terrain_world();
        let full_built = full_world();

        let _ = view.render(
            &terrain_built.world,
            BuildDepth::Terrain,
            rung_artifacts(terrain_built),
            78,
            20,
        );
        let first_key = view
            .memo
            .as_ref()
            .expect("a render with terrain populates the memo")
            .terrain_key;

        let at_full = view.render(
            &full_built.world,
            BuildDepth::Full,
            rung_artifacts(full_built),
            78,
            20,
        );
        let second_key = view
            .memo
            .as_ref()
            .expect("a render with terrain populates the memo")
            .terrain_key;

        assert_ne!(
            first_key, second_key,
            "the memo did not rebuild for a different terrain reference"
        );
        let at_full_text = at_full.to_plain_text();
        assert!(
            at_full_text.contains(plate::SETTLEMENT_MAJOR_GLYPH)
                || at_full_text.contains(plate::SETTLEMENT_MINOR_GLYPH),
            "the full-world render (after a rebuild) drew no settlement"
        );
    }

    #[test]
    fn render_is_pure_given_the_same_terrain_reference() {
        // The trait's own limit on `&mut self`: two calls with the same
        // arguments must return the same grid. Rendering the SAME build
        // twice on one view exercises both the fresh-memo path (first call)
        // and the memo-hit path (second call), and both must agree.
        let mut view = atlas_view();
        let built = full_world();
        let first = view.render(
            &built.world,
            BuildDepth::Full,
            rung_artifacts(built),
            78,
            20,
        );
        let second = view.render(
            &built.world,
            BuildDepth::Full,
            rung_artifacts(built),
            78,
            20,
        );
        assert_eq!(first.to_plain_text(), second.to_plain_text());
    }

    #[test]
    fn fit_depth_never_exceeds_the_drawn_plate() {
        // The structural guarantee the module doc's "The window" section
        // claims: the chosen rung's own virtual chart fits inside the
        // drawn plate in both dimensions, for a range of plate sizes — not
        // just the 78x20 the other tests happen to use.
        for (w, h) in [(78u16, 20u16), (40, 20), (120, 40), (10, 6), (200, 60)] {
            let depth = fit_depth(w, h);
            let (vw, vh) = plate::virtual_dims(depth);
            assert!(
                vw <= u32::from(w) && vh <= u32::from(h),
                "fit_depth({w}, {h}) = {depth} whose chart ({vw}x{vh}) exceeds the plate"
            );
        }
    }

    #[test]
    fn fit_depth_is_the_finest_rung_that_still_fits() {
        // Not merely "fits" (a constant `0` would also fit everything) —
        // the NEXT rung up must fail to fit, so this is really the fitting
        // rung and not a trivially-safe floor.
        let (w, h) = (78u16, 20u16);
        let depth = fit_depth(w, h);
        let (next_w, next_h) = plate::virtual_dims(depth + 1);
        assert!(
            next_w > u32::from(w) || next_h > u32::from(h),
            "fit_depth({w}, {h}) = {depth} is not maximal: rung {} ({next_w}x{next_h}) also fits",
            depth + 1
        );
    }

    #[test]
    fn the_real_build_observer_reports_how_often_terrain_actually_moves() {
        // Important 1 (fix round 1): the module doc used to assert, as fact,
        // that the build "sculpts terrain ONCE and threads the SAME
        // `GeneratedTerrain` value through every rung" — a claim about
        // ADDRESS STABILITY that neither `the_memo_rebuilds_for_a_different_
        // terrain_reference` (two unrelated builds) nor
        // `render_is_pure_given_the_same_terrain_reference` (one cached
        // artifact rendered twice) actually exercises. This test drives the
        // REAL observer (`build_world_observed`) across one genuine build and
        // records the address `RungArtifacts::terrain` carries at each rung
        // that has one, establishing the count rather than asserting it.
        let wc = WorldComponents::assemble().expect("canonical registries are well-formed");
        let mut addresses: Vec<(BuildDepth, usize)> = Vec::new();
        let mut observer = |rung: BuildDepth, _world: &World, artifacts: RungArtifacts<'_>| {
            if let Some(terrain) = artifacts.terrain {
                addresses.push((rung, std::ptr::from_ref(terrain) as usize));
            }
        };
        build_world_observed(
            Seed(42),
            &SkyPins::default(),
            &TerrainPins::default(),
            &SettlementPins::default(),
            &wc,
            BuildDepth::Full,
            &mut observer,
        )
        .expect("seed 42 builds");

        // Terrain, Settlements, Full each carry `Some` terrain — three
        // addresses recorded, in rung order.
        assert_eq!(
            addresses.iter().map(|(r, _)| *r).collect::<Vec<_>>(),
            vec![
                BuildDepth::Terrain,
                BuildDepth::Settlements,
                BuildDepth::Full
            ]
        );

        let distinct: std::collections::BTreeSet<usize> =
            addresses.iter().map(|(_, a)| *a).collect();
        eprintln!(
            "terrain addresses across rungs: {:?} ({} distinct)",
            addresses,
            distinct.len()
        );

        // MEASURED (see the task report for the printed addresses): the
        // `Terrain`-rung observer fires against the stage's original local
        // (`windows/worldgen/src/lib.rs:8031`); the `Settlements`/`Full`-rung
        // observers fire against the REBOUND local from the
        // "climate+settlements" stage's tuple destructure
        // (`:8098`), which moved the original through a closure return. A
        // move through a closure return does not preserve a stack address,
        // so this asserts exactly two distinct addresses: one for `Terrain`
        // alone, a second shared by `Settlements` and `Full` (unchanged
        // between those two — no further rebinding happens after :8098).
        // If a future refactor of `build_to` changes this, this test's
        // failure is the signal to re-word `atlas.rs`'s module doc again,
        // not to "fix" the test back to matching stale prose.
        assert_eq!(
            distinct.len(),
            2,
            "expected exactly one rebuild (Terrain's own address, then one shared \
             by Settlements and Full); got {} distinct addresses: {addresses:?}",
            distinct.len()
        );
        assert_eq!(
            addresses[1].1, addresses[2].1,
            "Settlements and Full disagreed on terrain's address, but nothing \
             rebinds `terrain` between those two observer calls in build_to"
        );
        assert_ne!(
            addresses[0].1, addresses[1].1,
            "Terrain's own address matched the post-rebind address; if a future \
             build_to refactor makes this true, AtlasView's memo would then \
             rebuild only once per build rather than twice — update the module \
             doc's rebuild-count claim to match, do not just relax this assertion"
        );
    }
}

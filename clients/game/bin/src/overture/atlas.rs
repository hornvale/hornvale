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
//! three times per startup for a mesh that never changes underneath it (the
//! build sculpts terrain ONCE and threads the same value through every later
//! rung, `windows/worldgen/src/lib.rs`'s own "Sculpt the terrain ONCE here and
//! KEEP it" comment on `build_to`).
//!
//! **The memo is keyed on the `GeneratedTerrain` reference's own address**,
//! not on the world's seed or on nothing at all. `artifacts.terrain` is a
//! borrow of the SAME value across every rung of one build (the build never
//! rebuilds it), so two renders that are handed the identical reference are,
//! by construction, looking at the identical mesh — reusing the memo for them
//! is exactly the case `render`'s own doc calls "a memo, not a licence": the
//! two calls return the same grid because nothing that could change the
//! answer changed. A render handed a DIFFERENT terrain reference (a fresh
//! build — a new game, or, in a test, a second world entirely) rebuilds
//! rather than silently reusing a stale index for the wrong mesh. Keying on
//! the world's seed instead was considered and rejected: it would be correct
//! for this campaign's own worlds (one seed, one set of pins, one build) but
//! would silently misbehave the day two DIFFERENT `GeneratedTerrain`s ever
//! shared a seed (a dev tool rebuilding the same seed under different pins,
//! say) — identity of the actual value handed in is the only key that cannot
//! go stale under a hypothetical it does not need to rule out.
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
//! left implicit: [`settlements_of`] and [`discovered_of`] are two separate,
//! narrow functions, and the test module asserts the resulting behaviour
//! (settlements appear once placed) rather than merely exercising the code
//! path.
//!
//! # Caves: skipped, deliberately
//!
//! `Driver::start_from_world` builds its own cave roster with a full scan of
//! every mesh vertex (`(0..geo.vertex_count())...filter(|&c|
//! terrain.cave_at(c).is_some())`, `driver.rs`), because a possessed agent can
//! eventually explore into one. The overture atlas has no possession and no
//! walk band — it is a pre-game overview of what genesis placed on the
//! surface — and an undiscovered cave is not a fact about the SURFACE the
//! way a settlement is; it is a fact about what is buried under it, which
//! this view has no more business showing unprompted than a real atlas shows
//! the inside of an unopened cave system. The task brief's own tests do not
//! ask for one either. So [`AtlasView::render`] passes an empty cave roster
//! and pays no per-render mesh scan for a feature this view does not draw.
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
use hornvale_kernel::{Geosphere, NearestVertexIndex, Value, Vertex, World};
use hornvale_worldgen::{BuildDepth, RungArtifacts};

use crate::discovery::{Discovered, FeatureId};
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

/// Every placed settlement's nearest terrain vertex — the same read
/// `Driver::start_from_world` performs (`driver.rs:690-710`), reused here
/// rather than re-derived by a second copy of the idiom: `IS_SETTLEMENT`'s
/// subject plus its committed `LATITUDE`/`LONGITUDE`, resolved to a
/// [`Vertex`] through [`NearestVertexIndex::nearest`]. A settlement missing
/// either coordinate fact is skipped rather than guessed at — matching
/// `sky::stars_of`'s own rule for a fact set genesis always commits together
/// but a hand-built world might not.
fn settlements_of(
    world: &World,
    geo: &Geosphere,
    nearest: &NearestVertexIndex,
) -> BTreeSet<Vertex> {
    world
        .ledger
        .find(hornvale_settlement::IS_SETTLEMENT)
        .filter_map(|fact| {
            let lat = match world
                .ledger
                .value_of(fact.subject, hornvale_settlement::LATITUDE)
            {
                Some(Value::Number(n)) => *n,
                _ => return None,
            };
            let lon = match world
                .ledger
                .value_of(fact.subject, hornvale_settlement::LONGITUDE)
            {
                Some(Value::Number(n)) => *n,
                _ => return None,
            };
            Some(nearest.nearest(geo, lat, lon))
        })
        .collect()
}

/// Every entry of `settlements`, marked discovered — see the module doc's
/// "The discovery decision" section for why the overture atlas shows every
/// placed settlement rather than gating on session discovery, which has not
/// started yet.
fn discovered_of(settlements: &BTreeSet<Vertex>) -> Discovered {
    let mut discovered = Discovered::default();
    for &vertex in settlements {
        discovered.record(FeatureId::Settlement(vertex));
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

        let settlements = settlements_of(world, geo, nearest);
        let discovered = discovered_of(&settlements);
        // No caves — see the module doc's "Caves: skipped, deliberately"
        // section.
        let caves = BTreeSet::new();

        plate::draw_with(
            terrain,
            geo,
            nearest,
            &frame,
            &window,
            w,
            h,
            plate::colour_allowed(),
            &settlements,
            &caves,
            &discovered,
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
        BuildArtifacts, SettlementPins, SkyChoice, WorldComponents, build_world_to_with_artifacts,
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
                SkyChoice::Generated,
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
            !terrain_text.contains(plate::SETTLEMENT_GLYPH),
            "a settlement glyph appeared before any settlement was placed"
        );
        // NON-VACUITY: the assertion above would also pass against a blank
        // or broken grid that drew nothing at all. Prove the terrain layer
        // actually painted a real raster — both glyphs `crate::plate`'s own
        // module doc names as its vocabulary (`~` ocean, `.` land) — so the
        // absence of the settlement glyph is a real absence, not a symptom
        // of nothing having been drawn.
        assert!(
            terrain_text.chars().any(|c| c == '~'),
            "the terrain layer drew no ocean at all: {terrain_text:?}"
        );
        assert!(
            terrain_text.chars().any(|c| c == '.'),
            "the terrain layer drew no land at all: {terrain_text:?}"
        );

        assert!(
            full_text.contains(plate::SETTLEMENT_GLYPH),
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
        assert!(
            at_full.to_plain_text().contains(plate::SETTLEMENT_GLYPH),
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
}

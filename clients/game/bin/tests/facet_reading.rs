//! The addressing and the reading are two things (The Sett, Task 2).
//!
//! `plate::terrain_at_tile` used to be one function that did both: it
//! unprojected a box's centre through the Mercator, resolved the facet that
//! point falls in, and then read everything a tile shows off that facet.
//! Task 3 needs the second half alone, addressed through the rose raster
//! instead of the projection, so the reading is extracted as
//! `plate::terrain_at_facet` and `terrain_at_tile` becomes its Mercator
//! caller.
//!
//! ## Why this is an integration test and not one of `plate.rs`'s own
//!
//! `plate.rs`'s in-module tests build a world through `test_world_at(level)`
//! and never build a `hornvale_locale::LocaleContext` at all — the module
//! holds no reference to one outside doc comments. So `ctx` is always `None`
//! there and every `TileTerrain::reflectance` is `None`, which would make the
//! reflectance half of the non-vacuity guard below unsatisfiable: the test
//! would pass while never once exercising the expensive half of the reading
//! it claims to compare. `terrain_at_facet` is `pub`, so an integration test
//! can call it, and out here a real context is three lines.
//!
//! ## Why a new file rather than joining `wash.rs`
//!
//! `wash.rs` is the one other file in this crate that builds a real context,
//! and its `wash_support` module lives INSIDE it — an integration test file
//! is its own crate, so "reuse its helper" can only mean putting these tests
//! into `wash.rs` beside The Wash's reflectance work. The helper that would
//! have justified that is the heavy one, `wash_support::seed_42_world`: a
//! `Box::leak`ed world, context and possession, forty lines, built to reach
//! `Session::day()`. Nothing here needs a possession (see
//! [`land_window`] — the window is centred on the terrain's own first dry
//! vertex, which needs no flagship to find), so what would actually be
//! reused is the three-line `seed_42_context`. That is not enough to pay for
//! mixing two subjects into one file, and `wash.rs`'s own module doc records
//! the standing arrangement it would break: nothing in `clients/game`'s
//! integration tests is shared across files, so each owns its fixture.
//!
//! The one-copy discipline this repository holds hard is about RULES. There
//! is no rule here — `seed_42_context` is a constructor, and the thing it
//! constructs is pinned by the fixture it reads, not by this copy of the
//! call.

use hornvale_game::mercator::{self, Frame};
use hornvale_game::plate::{self, BAND_B_RUNG, Window};
use hornvale_kernel::{Facet, FacetId, RoomMeshMemo, WorldTime};
use hornvale_locale::LocaleContext;
use hornvale_terrain::GeneratedTerrain;
use std::collections::BTreeSet;

/// The plate both sweeps below walk, in boxes — ONE window, so the
/// disagreement count is a fact about the very sample the identity is
/// asserted over.
///
/// Small on purpose: every box on the agreement sweep pays a full
/// reflectance read through a real context, and the identity it checks is a
/// property of one box, so more boxes buy repetition rather than reach.
///
/// **A wider window was tried and dropped, and the reason is worth keeping
/// because the reasoning that motivated it was wrong.** The argument was
/// that a grid quad spans 128 facets on a side at [`BAND_B_RUNG`] while the
/// two candidate points sit at most half a facet apart, so they could only
/// part near the quad's diagonal midlines, so a 12-box-wide window would
/// report zero almost wherever it were placed. Measured, on this window:
/// **6 of 72 boxes**, 8.3%. A 160x48 window over the same ground reported
/// **66 of 7,680**, 0.86% — an order of magnitude LOWER. So the rate is
/// strongly position-dependent, neither number is "the" rate, and widening
/// the sample would have bought a smoother average of a quantity nobody
/// wants an average of. What is wanted is whether the window the identity is
/// asserted over contains boxes where the choice of point would have changed
/// the reading, and it does.
const W: u32 = 12;
/// See [`W`].
const H: u32 = 6;

/// Seed 42's locale context, built from the committed world — the same
/// two-line construction `wash.rs`'s own `seed_42_context` makes, for the
/// same reason (the terrain, geosphere and nearest-vertex index are read
/// back OFF it, so the world is derived once).
///
/// The world is returned alongside it because the season bucket
/// [`plate::terrain_at_tile`]'s contract demands is derived from the world's
/// own calendar, which only `hornvale_worldgen::sky_of` can answer and which
/// a `LocaleContext` does not carry.
fn seed_42() -> (hornvale_kernel::World, LocaleContext) {
    let world = hornvale_worldgen::fixture::seed_42_world();
    let ctx = LocaleContext::build(&world).expect("seed 42 builds a locale context");
    (world, ctx)
}

/// The Mercator frame this file projects through.
///
/// `frame_for(false)` — the untilted frame — rather than one derived from
/// the world's rotation regime, because the frame is an INPUT to both sides
/// of every comparison below and cancels out of all of them. `wash.rs` makes
/// the same call for the same reason.
fn frame() -> Frame {
    mercator::frame_for(false)
}

/// A `w`x`h` window at [`BAND_B_RUNG`] centred on seed 42's first dry-land
/// grid vertex.
///
/// **Why a vertex and not the flagship settlement.** The window has one job:
/// be non-vacuous — more than one facet, and at least one box on dry land
/// with a reflectance the context can actually resolve. A settlement would
/// deliver that only by way of a whole possession (`hornvale_vessel::
/// Session::start_in`, a `Box::leak`ed world and context), while the terrain
/// already answers the question directly and deterministically: the lowest
/// `Vertex` that is not ocean and falls inside the projection's polar clamp.
/// Nothing about the identity under test cares which patch of ground it is
/// read on.
fn land_window(f: &Frame, terrain: &GeneratedTerrain, w: u32, h: u32) -> Window {
    let geo = terrain.geosphere();
    let (vw, vh) = plate::virtual_dims(BAND_B_RUNG);
    let (row, col) = geo
        .vertices()
        .filter(|&v| !terrain.is_ocean(v))
        .find_map(|v| {
            let p = geo.position(v);
            let lat = hornvale_kernel::math::asin(p[2].clamp(-1.0, 1.0)).to_degrees();
            let lon = hornvale_kernel::math::atan2(p[1], p[0]).to_degrees();
            mercator::project(f, lat, lon, vw, vh)
        })
        .expect("seed 42 has a dry vertex inside the projection's polar clamp");
    Window {
        depth: BAND_B_RUNG,
        // Longitude wraps, latitude clamps — the same asymmetry
        // `Driver::centre_window_on` obeys.
        origin_col: (col + vw - w / 2) % vw,
        origin_row: row.saturating_sub(h / 2).min(vh - h),
    }
}

/// The point [`plate::terrain_at_tile`] resolves for screen box
/// `(row, col)`, reproduced here so a caller can hand the SAME point to
/// [`plate::terrain_at_facet`].
///
/// This is the one place this file restates arithmetic that lives in
/// `plate.rs`. It has to: the whole claim under test is that the extracted
/// reader, given the facet and the point its Mercator caller resolved,
/// returns exactly what that caller returns. A helper that asked
/// `terrain_at_tile` for the point instead would be comparing the function
/// to itself.
fn tile_centre(f: &Frame, win: &Window, vw: u32, vh: u32, row: u32, col: u32) -> [f64; 3] {
    let (lat, lon) = mercator::unproject(f, win.origin_row + row, win.origin_col + col, vw, vh);
    hornvale_kernel::math::unit_sphere_from_lat_lon(lat, lon)
}

/// Every `(row, col)` of a `w`x`h` plate, in a stable order.
fn boxes(w: u32, h: u32) -> impl Iterator<Item = (u32, u32)> {
    (0..h).flat_map(move |row| (0..w).map(move |col| (row, col)))
}

/// A facet's identity, for counting distinct ones.
fn id(facet: &Facet) -> FacetId {
    facet
        .pack()
        .expect("a facet at BAND_B_RUNG (13) is far inside Facet::MAX_DEPTH (29)")
}

/// The two readers are one reader. This is an EXACT identity, not an
/// approximation: [`plate::terrain_at_facet`] is handed the same facet AND
/// the same point [`plate::terrain_at_tile`] resolved, so every field must
/// match.
///
/// The comparison is `assert_eq!` on the whole `TileTerrain` rather than a
/// field-by-field walk, so a field added later is covered without anyone
/// remembering to add it here. (`TileTerrain` is `PartialEq` and not `Eq` —
/// it carries a float height — which means a `NaN` height would fail this
/// spuriously. Nothing on this window produces one; if that ever changes the
/// failure names the box.)
///
/// **It was checked against a positive control, not merely observed green.**
/// Handing [`plate::terrain_at_facet`] `facet.centroid()` instead of the
/// point [`plate::terrain_at_tile`] resolved reddens it at box (row 1,
/// col 5) of this very window — `vertex` 19555 against 18394, same facet,
/// same reflectance, same height. So the identity below is sensitive to the
/// one thing the extraction could have got wrong, and `at_position` being a
/// PARAMETER rather than a derivation is load-bearing at the first window
/// anyone would pick, not at some contrived one.
///
/// **Guards its own non-vacuity twice:** the sampled boxes must resolve to
/// more than one distinct facet (a constant-facet window would make the
/// comparison a tautology), and at least one must be dry land with a
/// resolved reflectance — the expensive half of the reading, which a window
/// entirely at sea, or one drawn without a context, would never exercise.
#[test]
fn terrain_at_facet_agrees_with_terrain_at_tile() {
    let (world, ctx) = seed_42();
    let terrain = ctx.terrain();
    let geo = terrain.geosphere();
    let index = ctx.nearest_index();
    let f = frame();
    let (vw, vh) = plate::virtual_dims(BAND_B_RUNG);
    let win = land_window(&f, terrain, W, H);
    let mut memo = RoomMeshMemo::default();

    // Day zero, and the season bucket that instant actually names — the
    // contract `terrain_at_tile`'s own doc states, rather than a literal `0`
    // that would file a reading under a key misrepresenting it.
    let at = WorldTime::GENESIS;
    let calendar = hornvale_worldgen::sky_of(&world)
        .ok()
        .map(|sky| sky.calendar().clone());
    let season = hornvale_game::driver::season_bucket_for(calendar.as_ref(), at);

    let mut facets: BTreeSet<FacetId> = BTreeSet::new();
    let mut dry_with_reflectance = 0usize;

    for (row, col) in boxes(W, H) {
        let through_the_projection = plate::terrain_at_tile(
            terrain,
            geo,
            index,
            &mut memo,
            &f,
            &win,
            vw,
            vh,
            row,
            col,
            Some(&ctx),
            at,
            season,
            // No `(facet, season)` cache on either side: the claim is that
            // the two readers agree, and a cache shared between them would
            // let the second one return the first one's answer without ever
            // reading anything — the comparison would hold whatever the
            // extracted body did.
            None,
        );

        // The MEMO is shared, and that is not the same concession. It skips
        // a repeated nearest-vertex SEARCH and returns the answer a fresh
        // search would; `hornvale-locale`'s own
        // `every_cached_reader_bit_equals_its_recomputing_sibling_with_a_partial_prefill`
        // is what pins that.
        let through_the_facet = plate::terrain_at_facet(
            terrain,
            geo,
            index,
            &mut memo,
            &through_the_projection.facet,
            tile_centre(&f, &win, vw, vh, row, col),
            Some(&ctx),
            at,
            season,
            None,
        );

        assert_eq!(
            through_the_projection, through_the_facet,
            "the two readers disagreed at box (row {row}, col {col})"
        );

        facets.insert(id(&through_the_projection.facet));
        if !through_the_projection.ocean && through_the_projection.reflectance.is_some() {
            dry_with_reflectance += 1;
        }
    }

    assert!(
        facets.len() > 1,
        "the window resolved {} distinct facet(s): a constant-facet window \
         makes the comparison above a tautology",
        facets.len()
    );
    assert!(
        dry_with_reflectance > 0,
        "no box on the window was dry land with a resolved reflectance, so \
         the expensive half of the reading was never compared"
    );
}

/// The centroid and the box's own centre are DIFFERENT points inside the
/// same facet, and the reading can differ between them. This is the
/// measurement Task 3's graph arm is choosing against, kept so the choice
/// stays visible — and so that a future reader who decides to derive
/// `at_position` inside [`plate::terrain_at_facet`] finds out here rather
/// than in a drawn map.
///
/// The mechanism, since the number below means nothing without it: the
/// reading resolves a grid-level QUAD from the facet, then picks whichever
/// of that quad's four corners is nearest the point it was handed. At
/// [`BAND_B_RUNG`] the quad spans 128 facets on a side and the two points
/// sit at most half a facet apart, so they always lie in the same quad —
/// but not always on the same side of the boundary between two corners, and
/// where they part the resolved `vertex` parts with them, taking `ocean`,
/// `water` and the snapped elevation fallback along.
///
/// **The rate is a property of the window, not of the rung** — see [`W`]'s
/// own doc for the two measurements that establish that, and for why a
/// wider sweep was tried and dropped.
///
/// It asserts a COUNT, not an absence: if the two points agree everywhere on
/// the sampled window that is a fact worth recording, and if they disagree
/// the test names how often. **A disagreement is not a failure** — both
/// readings are legitimate; only a silent switch between them is not. The
/// only thing asserted is that the comparison was non-vacuous.
///
/// No context is supplied, deliberately: `vertex` is resolved from the mesh
/// and the point alone, so a reflectance read per box would multiply this
/// sweep's cost by the one thing it does not look at.
#[test]
fn the_centroid_and_the_tile_centre_can_resolve_different_vertices() {
    let (_world, ctx) = seed_42();
    let terrain = ctx.terrain();
    let geo = terrain.geosphere();
    let index = ctx.nearest_index();
    let f = frame();
    let (vw, vh) = plate::virtual_dims(BAND_B_RUNG);
    let win = land_window(&f, terrain, W, H);
    let mut memo = RoomMeshMemo::default();
    let at = WorldTime::GENESIS;

    let mut facets: BTreeSet<FacetId> = BTreeSet::new();
    let mut differed = 0usize;
    let mut total = 0usize;

    for (row, col) in boxes(W, H) {
        let centre = tile_centre(&f, &win, vw, vh, row, col);
        let facet = Facet::containing(centre, win.depth);

        let at_centre = plate::terrain_at_facet(
            terrain, geo, index, &mut memo, &facet, centre, None, at, 0, None,
        );
        let at_centroid = plate::terrain_at_facet(
            terrain,
            geo,
            index,
            &mut memo,
            &facet,
            facet.centroid(),
            None,
            at,
            0,
            None,
        );

        facets.insert(id(&facet));
        total += 1;
        if at_centre.vertex != at_centroid.vertex {
            differed += 1;
        }
    }

    println!(
        "the centroid and the box centre resolved different vertices at \
         {differed} of {total} boxes on a {W}x{H} window \
         at rung {BAND_B_RUNG} ({} distinct facets)",
        facets.len()
    );

    assert!(
        facets.len() > 1,
        "the window resolved {} distinct facet(s): the comparison above was \
         vacuous",
        facets.len()
    );
}

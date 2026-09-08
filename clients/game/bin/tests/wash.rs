//! The plate's tiles carry the ground's own spectral curve (The Wash, Task 3).
//!
//! ## The fixture, and why it is a new one
//!
//! `plate_vocabulary.rs` owns a `test_world()` that derives seed 42's
//! `GeneratedTerrain` DIRECTLY (`hornvale_terrain::generate`), and
//! `walk_band_agreement.rs` builds a `hornvale_vessel::WorldContext` to reach
//! a real `LocaleContext` — the cheapest route available to it at the time,
//! because this crate did not then depend on `hornvale-locale` at all. Neither
//! is the fixture this file needs: the first has no context to ask for a
//! reflectance, and the second derives the world through a heavier wrapper
//! than the one thing wanted from it. Nothing in `clients/game`'s integration
//! tests is shared across files (`plate_vocabulary.rs`'s own doc records
//! that there is no such helper anywhere in this tree), so this file owns
//! its own — and it takes terrain, geosphere and nearest-vertex index OFF
//! the context rather than deriving a second copy, so seed 42's terrain is
//! generated exactly once here.

mod wash_support {
    use hornvale_game::mercator::{self, Frame};
    use hornvale_game::plate::{self, ReflectanceCache, Window};
    use hornvale_kernel::{Geosphere, NearestVertexIndex, RoomMeshMemo, Seed, World};
    use hornvale_locale::LocaleContext;
    use hornvale_terrain::GeneratedTerrain;
    use hornvale_vessel::Session;

    /// The window's width and height in terminal columns and rows — an
    /// 80x24 terminal, what a player actually sees at the client's floor
    /// rung.
    pub const W: u16 = 80;
    /// See [`W`].
    pub const H: u16 = 24;
    /// Seed 42's locale context, built from the committed world. Everything else
    /// this file needs — the terrain, the geosphere, the nearest-vertex
    /// index — is read back off it, so the world is derived once.
    pub fn seed_42_context() -> LocaleContext {
        LocaleContext::build(&hornvale_worldgen::fixture::seed_42_world())
            .expect("seed 42 builds a context")
    }

    /// A live seed-42 possession, plus the terrain/geosphere/index triple a
    /// direct [`plate::terrain_at_tile`] call needs, plus the world's own
    /// calendar — everything Task 4's cache test asks for, in the shape it
    /// asks for it.
    ///
    /// **Why a real [`Session`], not the lighter [`seed_42_context`] above.**
    /// The cache test needs `session.context()` (a [`LocaleContext`]) AND
    /// `session.day()` (a [`hornvale_kernel::WorldTime`]) — `day()` is a
    /// `Session`-only method (the day the possession stands on), so a bare
    /// `LocaleContext` cannot answer it. Built the same way
    /// `Driver::start_from_world` builds one — `hornvale_worldgen::build_world`
    /// with the same default pins, `WorldContext::build`, `Session::start_in`
    /// — leaking the world and its context to `'static` (`Box::leak`) rather
    /// than the raw-pointer trick `Driver` uses: this is a test fixture with
    /// no `Drop` to write, so the simpler tool is the right one.
    ///
    /// `terrain`/`geo`/`index` are re-derived independently of the context's
    /// own copy, the same "cheap enough to reconstruct, never a second
    /// drifting genesis" idiom `Driver::start_from_world` follows for its own
    /// `terrain`/`geo`/`nearest` fields.
    ///
    /// **The `Option<Calendar>` (fix round 1, Fix 3) is what lets a caller
    /// derive a `season` that actually corresponds to `at`**, via
    /// [`hornvale_game::driver::season_bucket_for`] — see
    /// [`plate::terrain_at_tile`]'s own doc for the contract this exists to
    /// satisfy. Derived the same way `Driver::start_from_world` derives
    /// `Driver::calendar`: `hornvale_worldgen::sky_of(world)`, never a
    /// second, drifting genesis.
    ///
    /// **The leaked `&'static World` (Task 5) is the same reference `ctx`
    /// and `session` already borrow** — not a second genesis, just the one
    /// this function already built, handed back so a caller can reach
    /// [`hornvale_game::driver::plate_illuminant`]/`plate_illuminant_at`,
    /// neither of which `Session`'s own public API can answer (it exposes
    /// no seed or world accessor — confirmed by grep, not assumed; see
    /// Task 5's own report for the citation this replaces).
    // Named construction site (decision 0092): `terrain_of` re-derives the
    // tectonic globe once here, the same reason `Driver::start_from_world`
    // carries this same allow at its own `terrain_of` call.
    #[allow(clippy::disallowed_methods)]
    pub fn seed_42_world() -> (
        Session<'static>,
        Geosphere,
        GeneratedTerrain,
        NearestVertexIndex,
        RoomMeshMemo,
        Option<hornvale_astronomy::Calendar>,
        &'static World,
    ) {
        let world = hornvale_worldgen::build_world(
            Seed(42),
            &hornvale_astronomy::SkyPins::default(),
            &hornvale_terrain::TerrainPins::default(),
            &hornvale_worldgen::SettlementPins::default(),
        )
        .expect("seed 42 generates");
        let world: &'static World = Box::leak(Box::new(world));

        let ctx = hornvale_vessel::WorldContext::build(world).expect("seed 42 builds a context");
        let ctx: &'static hornvale_vessel::WorldContext<'static> = Box::leak(Box::new(ctx));

        let opts = hornvale_vessel::PossessOpts {
            target: hornvale_vessel::PossessTarget::Flagship,
            ..hornvale_vessel::PossessOpts::default()
        };
        let (session, _opening) =
            Session::start_in(ctx, &opts).expect("seed 42 has a flagship settlement to possess");

        let terrain = hornvale_worldgen::terrain_of(world).expect("seed 42 sculpts");
        let geo = terrain.geosphere().clone();
        let index = NearestVertexIndex::new(&geo);
        let calendar = hornvale_worldgen::sky_of(world)
            .ok()
            .map(|sky| sky.calendar().clone());

        (
            session,
            geo,
            terrain,
            index,
            RoomMeshMemo::default(),
            calendar,
            world,
        )
    }

    /// The projection and window this file samples through:
    /// [`plate::GLOBE_RUNG`] (the world map's own coarsest, always-shipped
    /// rung), an equatorial 80x24 plate — the same construction
    /// `plate_vocabulary.rs`'s `seed_42_window` uses.
    pub fn frame_and_window() -> (Frame, Window, u32, u32) {
        let f = mercator::frame_for(false);
        let (vw, vh) = plate::virtual_dims(plate::GLOBE_RUNG);
        let win = Window {
            depth: plate::GLOBE_RUNG,
            origin_col: 0,
            origin_row: vh / 2 - u32::from(H) / 2,
        };
        (f, win, vw, vh)
    }

    /// Every (row, col) of the window, in a stable order.
    pub fn sample_tiles() -> impl Iterator<Item = (u32, u32)> {
        (0..u32::from(H)).flat_map(|row| (0..u32::from(W)).map(move |col| (row, col)))
    }

    /// A fresh mesh memo for the sweep.
    pub fn memo() -> RoomMeshMemo {
        RoomMeshMemo::default()
    }

    /// One `terrain_at_tile` draw through a real context, at a single
    /// representative tile — enough to exercise the `(FacetId, season)`
    /// cache's own consult-then-fill behaviour without sweeping a whole
    /// window (Task 4's test only cares whether a repeated draw grows
    /// `store`, not about coverage over many tiles — that is Task 3's own
    /// test's job).
    ///
    /// `(row, col) = (0, 0)` at [`plate::GLOBE_RUNG`] is deliberate, not
    /// arbitrary: that rung IS the grid level (this module's own
    /// `frame_and_window` doc), so [`plate::terrain_at_tile`]'s address can
    /// never be coarser than the grid there and `reflectance_at_facet_cached`
    /// cannot refuse it — the same guarantee `two_tiles_in_one_band_with_
    /// different_ground_differ_spectrally`'s non-vacuity assertion checks by
    /// sweeping. A tile that resolved `None` would leave `store` at zero
    /// after the FIRST draw, which is exactly the failure this helper must
    /// not risk producing.
    ///
    /// **`season` is DERIVED from `calendar` and `at` (fix round 1, Fix 3),
    /// never hardcoded.** The first draft passed a literal `0` here while
    /// `at` was seed 42's real `session.day()` — a `season` uncoupled from
    /// the `at` the value was actually computed at, exactly the mislabeling
    /// [`plate::terrain_at_tile`]'s own doc warns a caller against. Both
    /// draws in the covering test use the SAME `at`, so they derive the
    /// SAME season either way (which is why the bug did not fail the test);
    /// this fixes the derivation because it is wrong to file a value under
    /// a key that misrepresents it, not because the test could tell.
    #[allow(clippy::too_many_arguments)] // mirrors `plate::terrain_at_tile`'s own allow, one level up
    pub fn draw_once(
        terrain: &GeneratedTerrain,
        geo: &Geosphere,
        index: &NearestVertexIndex,
        memo: &mut RoomMeshMemo,
        ctx: &LocaleContext,
        at: hornvale_kernel::WorldTime,
        calendar: Option<&hornvale_astronomy::Calendar>,
        store: &mut ReflectanceCache,
    ) {
        let (f, win, vw, vh) = frame_and_window();
        let season = hornvale_game::driver::season_bucket_for(calendar, at);
        let _ = plate::terrain_at_tile(
            terrain,
            geo,
            index,
            memo,
            &f,
            &win,
            vw,
            vh,
            0,
            0,
            Some(ctx),
            at,
            season,
            Some(store),
        );
    }
}

/// FIRES WHEN: the plate stops asking the locale for reflectance and goes
/// back to inferring appearance from elevation. Two tiles in the SAME
/// elevation band with different ground must differ spectrally — that is
/// the category error this campaign exists to fix, asserted on the datum
/// rather than on the rendered glyph.
///
/// # What this witnesses, and what it does not
///
/// It witnesses **wiring**: that `terrain_at_tile` reaches a real
/// `LocaleContext`, gets a curve back, and that the curve varies with the
/// ground rather than with the elevation band. That is the whole of its
/// claim.
///
/// It does **not** witness that the wetness behind that curve is
/// *grounded*. `micro_field(seed, None)` — pure address noise, the exact
/// degradation `reflectance_at_facet` exists to prevent — would satisfy
/// every assertion below just as well, because address noise also varies
/// from tile to tile within a band. Measured, not assumed: dropping the
/// grounding entirely was caught by two tests in `windows/locale` and by
/// nothing in this file (Task 3 fix round, Finding 2).
///
/// The grounding is owned by
/// `hornvale_locale`'s unit test
/// `the_grounded_wetness_is_the_moisture_redistributed_by_the_watercourse`,
/// which pins `grounded_wetness_for`'s output against the composition it
/// claims to be and against the ungrounded reading at the same address.
/// This file is on the wrong side of the crate boundary to check that
/// cheaply — the moisture blend, the channel network and the catchment
/// partition are all private to the context — so it does not try.
#[test]
fn two_tiles_in_one_band_with_different_ground_differ_spectrally() {
    let ctx = wash_support::seed_42_context();
    let terrain = ctx.terrain();
    let geo = terrain.geosphere();
    let index = ctx.nearest_index();
    let mut memo = wash_support::memo();
    let (f, win, vw, vh) = wash_support::frame_and_window();
    // Day zero. The cover term `reflectance_at_facet` integrates is
    // seasonal, so `at` picks WHICH season — genesis is as good a day as any
    // and needs no possession to be started for it.
    let at = hornvale_kernel::WorldTime::GENESIS;

    let mut by_band: std::collections::BTreeMap<u32, Vec<[f64; hornvale_kernel::color::BANDS]>> =
        std::collections::BTreeMap::new();
    let mut resolved = 0usize;

    for (row, col) in wash_support::sample_tiles() {
        let t = hornvale_game::plate::terrain_at_tile(
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
            // No `(Facet, season)` cache here, deliberately: this test's own
            // claim (see its doc) is that the memoized reflectance agrees
            // with a FRESH recomputation at every tile, so threading the
            // Task 4 cache through would make some of those recomputations
            // cache hits instead — a weaker, not equivalent, check. Task 4's
            // own test (`a_redraw_in_the_same_season_adds_no_cache_entries`,
            // below) is what pins the cache's hit path against a fresh read.
            0,
            None,
        );
        // THE MEMO IS A SEARCH SKIPPED, NEVER A DIFFERENT ANSWER.
        // `terrain_at_tile` reads reflectance through
        // `reflectance_at_facet_cached`, sharing the memo it filled for the
        // grid-level ancestor; at this rung that address IS the tile's own
        // facet, so it is a cache HIT on the shipped path. Determinism is
        // constitutional here, so the equality is asserted on the real
        // addressing rather than inferred from the locale crate's own
        // neighbourhood test.
        let uncached = ctx
            .reflectance_at_facet(&t.facet, at)
            .ok()
            .map(|r| *r.get());
        assert_eq!(
            t.reflectance.as_ref().map(|r| *r.get()),
            uncached,
            "the memoized reflectance disagreed with the recomputed one at (row {row}, col {col})"
        );

        if let Some(r) = t.reflectance.as_ref() {
            resolved += 1;
            by_band.entry(t.band).or_default().push(*r.get());
        }
    }

    // NON-VACUITY: a sweep where the context refused every address would
    // satisfy the assertion below by finding nothing to disagree about — so
    // say out loud that the tiles were coloured at all.
    assert!(
        resolved > 0,
        "no tile in the window resolved a reflectance at all; the context refused every \
         address, so this test would pass for the wrong reason"
    );

    let differing = by_band
        .values()
        .find(|curves| curves.len() > 1 && curves.iter().any(|c| c != &curves[0]));
    assert!(
        differing.is_some(),
        "no elevation band contained two spectrally different tiles; \
         either the sample is degenerate or reflectance is still keyed on elevation"
    );
}

/// FIRES WHEN: the cache key stops varying with season. That is the
/// spine's own invariant (Task 1) made concrete: reflectance is
/// seasonal-rate, so a key that ignores season would freeze the map at
/// whatever season it first drew.
#[test]
fn the_reflectance_key_changes_between_midwinter_and_midsummer() {
    assert_ne!(
        hornvale_game::plate::season_bucket(0.0),
        hornvale_game::plate::season_bucket(0.5),
        "midwinter and midsummer must land in different buckets"
    );
}

/// The non-vacuity arm: two instants in the SAME season must share a
/// bucket, or the assertion above holds for a bucket function that simply
/// returns its input.
#[test]
fn two_instants_in_one_season_share_a_bucket() {
    assert_eq!(
        hornvale_game::plate::season_bucket(0.01),
        hornvale_game::plate::season_bucket(0.02)
    );
}

/// FIRES WHEN: the cache stops being consulted. A second draw at the same
/// season must not grow the store.
#[test]
fn a_redraw_in_the_same_season_adds_no_cache_entries() {
    let (session, geo, terrain, index, mut memo, calendar, _world) = wash_support::seed_42_world();
    let mut store = hornvale_game::plate::ReflectanceCache::new();
    let ctx = session.context();
    let at = session.day();
    let calendar = calendar.as_ref();

    wash_support::draw_once(
        &terrain, &geo, &index, &mut memo, ctx, at, calendar, &mut store,
    );
    let after_first = store.len();
    assert!(after_first > 0, "the first draw must populate the cache");

    wash_support::draw_once(
        &terrain, &geo, &index, &mut memo, ctx, at, calendar, &mut store,
    );
    assert_eq!(
        store.len(),
        after_first,
        "a same-season redraw must add nothing"
    );
}

/// FIRES WHEN: the illuminant stops varying with the sun's altitude. Dawn
/// and noon must differ, and DIRECTIONALLY — a low sun is warmer, so its
/// long-wavelength bands carry relatively more. Mere inequality would be
/// satisfied by any change at all.
///
/// **Adapted from the brief's own `session: &Session` signature.** `Session`
/// exposes no seed or world accessor anywhere in its public API (grepped,
/// not assumed: `windows/vessel/src/session.rs`'s one `impl<'w> Session<'w>`
/// block has 77 `pub fn`s and none of them hands back `&World`, `Seed`, or
/// anything that reaches one), so `plate_illuminant_at`/`plate_illuminant`
/// take `&World` directly instead — the same thing
/// [`hornvale_vessel`]'s own room-scale `eyes::daylight_at` takes, which
/// this task's brief named as the pattern to follow. `wash_support::
/// seed_42_world` hands back the `&'static World` it already built rather
/// than a second genesis.
#[test]
fn a_low_sun_is_warmer_than_a_high_one() {
    let (.., world) = wash_support::seed_42_world();

    let dawn = hornvale_game::driver::plate_illuminant_at(world, 2.0);
    let noon = hornvale_game::driver::plate_illuminant_at(world, 60.0);

    let warmth = |i: &hornvale_kernel::color::Illuminant| -> f64 {
        let b = i.get();
        let n = b.len();
        let long: f64 = b[n / 2..].iter().sum();
        let short: f64 = b[..n / 2].iter().sum();
        long / short
    };

    assert!(
        warmth(&dawn) > warmth(&noon),
        "a low sun must be warmer: dawn {:.4} vs noon {:.4}",
        warmth(&dawn),
        warmth(&noon)
    );
}

/// FIRES WHEN: `plate_illuminant_at` stops being a pure function of its
/// arguments — the property that lets [`plate_illuminant`] be called
/// **once per draw** rather than once per tile. Per this task's own
/// constraint, "computed once" cannot be shown by timing anything
/// (`Instant` is banned even in tests); this shows it structurally instead,
/// the way the brief's own hint puts it ("restructure so computing it twice
/// would not type-check"): neither function takes a tile/facet parameter at
/// all, so there is nothing tile-shaped for a second call to vary on — two
/// calls with the identical `(world, sun_elevation_deg)` must return the
/// identical `Illuminant`, bit for bit, which is what this test pins.
#[test]
fn plate_illuminant_at_is_deterministic_across_repeated_calls() {
    let (.., world) = wash_support::seed_42_world();

    let first = hornvale_game::driver::plate_illuminant_at(world, 30.0);
    let second = hornvale_game::driver::plate_illuminant_at(world, 30.0);

    assert_eq!(
        first, second,
        "the same (world, elevation) must produce the identical illuminant"
    );
}

/// FIRES WHEN: [`hornvale_game::driver::plate_illuminant`] stops folding an
/// absent sun altitude to [`hornvale_game::driver::flat_illuminant`] and
/// instead panics or guesses a sun it cannot honestly place.
///
/// **This is the WRAPPER, and it is the only test that touches it.** Every
/// other illuminant test in this file calls the inner
/// `plate_illuminant_at`, which never sees a calendar; the `None` arm lives
/// in the wrapper alone. `gate-commit` does not scan `clients/`, `make
/// game-check` asserts nothing about this path, and no ratchet binds it, so
/// a regression here is silent.
///
/// **It is not a retired-provider test, and the version it replaces only
/// looked like one.** The Zenith retired `a_starless_world_lights_the_plate_flat`,
/// which built a stipulated acyclic world purely to obtain a `None` calendar.
/// `plate_illuminant` documents TWO `None` cases and the second —
/// a `Some` calendar whose `solar_altitude_at` returns `None` under zero
/// obliquity AND zero eccentricity — outlives the tier entirely. A literal
/// `None` reaches the same arm from both, the way
/// `driver.rs`'s `a_starless_world_resolves_season_bucket_zero` already
/// does for `season_bucket_for`'s identical fold.
///
/// The world is bare (`World::new`) because on this arm the function never
/// reads it: `plate_illuminant` short-circuits to `flat_illuminant()`
/// before `plate_illuminant_at` would derive the star from `world.seed`.
/// That also keeps it clear of `sky_of`, so it is unaffected by the sky
/// provider's own refusal work.
#[test]
fn an_unplaceable_sun_lights_the_plate_flat() {
    let world = hornvale_kernel::World::new(hornvale_kernel::Seed(42));

    let lit = hornvale_game::driver::plate_illuminant(
        &world,
        None,
        hornvale_kernel::WorldTime::GENESIS,
        45.0,
    );

    assert_eq!(
        *lit.get(),
        [1.0; hornvale_kernel::color::BANDS],
        "a sun that cannot be placed must fall back to a flat unit illuminant"
    );
}

/// An ocean tile (`water == 0`) with no ground reflectance at all — the
/// wet arm never reads [`hornvale_game::plate::TileTerrain::reflectance`],
/// so every other field is a placeholder built from the cheapest legal
/// value for its type. `facet`/`vertex` are the empty-path root facet and
/// vertex 0; `height_asl` is a plausible ocean depth. None of these three
/// participate in [`hornvale_game::plate::color_for`]'s wet arms — only
/// `water` does.
fn ocean_tile() -> hornvale_game::plate::TileTerrain {
    hornvale_game::plate::TileTerrain {
        ocean: true,
        facet: hornvale_kernel::Facet {
            face: 0,
            path: Vec::new(),
        },
        vertex: hornvale_kernel::Vertex(0),
        band: 0,
        reflectance: None,
        height_asl: hornvale_kernel::SeaLevelHeight::from_metres(-1000.0),
        water: 0,
    }
}

/// B6: open water must answer to the clock like everything else. Before
/// The Newel, `color_for`'s wet arms called `obs.show`, which takes no
/// illuminant at all, so the ocean ink was byte-identical from a sun 80
/// degrees up to one 60 degrees below the horizon.
#[test]
fn an_ocean_tile_is_a_different_colour_at_noon_and_at_midnight() {
    let obs = hornvale_game::observer::TerminalObserver::new(
        hornvale_game::observer::ColorDepth::TrueColor,
    );
    let (.., world) = wash_support::seed_42_world();
    let noon = hornvale_game::driver::plate_illuminant_at(world, 80.0);
    let night = hornvale_game::driver::plate_illuminant_at(world, -60.0);
    let tile = ocean_tile();
    let a = hornvale_game::plate::color_for(&tile, &noon, &obs).expect("truecolor renders");
    let b = hornvale_game::plate::color_for(&tile, &night, &obs).expect("truecolor renders");
    assert_ne!(a, b, "the ocean drew the same ink at noon and at midnight");
}

// =====================================================================
// Task 6: the collapse — colour stops being keyed on elevation.
// =====================================================================

mod wash_collapse {
    use hornvale_game::observer::{ColorDepth, TerminalObserver};
    use hornvale_game::plate::{self, PlateLight};
    use hornvale_game_core::{Grid, Ink};
    use std::collections::BTreeSet;

    /// One whole terrain-layer plate, drawn through a REAL locale context at
    /// the given colour regime and display depth.
    ///
    /// **`draw_with`, not `draw_terrain_layer`, and not `draw`.** `draw`
    /// resolves `colour_allowed` from the environment, which would make
    /// every assertion below depend on whether `NO_COLOR` happened to be set
    /// in the runner — the exact hermeticity the `draw_with` seam exists to
    /// give (`plate::draw_with`'s own doc, and the threaded-tests incident
    /// its fix round records).
    ///
    /// **What this composes, exactly** (fix round 1 — the sentence here used
    /// to say "the composed surface a reader actually sees", which claims
    /// two layers this never reaches): the terrain layer, its river line
    /// layer, and `draw_feature_layer` **over empty rosters**, so the
    /// feature layer paints nothing. `draw_perception_layer` is not in
    /// `draw_with` at all — `Driver::world_plate_for_redraw` composes it
    /// separately, band B only. So every claim below is a claim about the
    /// TERRAIN layer's own output; neither of the other two carries its
    /// colours through an observer, and nothing here would notice if they
    /// stopped.
    fn plate_at(colour_allowed: bool, depth: ColorDepth) -> Grid {
        let ctx = super::wash_support::seed_42_context();
        let terrain = ctx.terrain();
        let geo = terrain.geosphere();
        let index = ctx.nearest_index();
        let (f, win, _, _) = super::wash_support::frame_and_window();

        // A flat unit illuminant, deliberately: this file's illuminant
        // claims are Task 5's (`a_low_sun_is_warmer_than_a_high_one`), and
        // a flat light is the one that CANNOT manufacture the chromatic
        // variation asserted below — every band weighted 1.0 contributes no
        // colour of its own, so whatever hue difference appears came from
        // the ground. Reaching for a real sun here would have made the
        // strongest assertion in this module ambiguous about its own cause.
        let light = PlateLight::flat(colour_allowed).with_observer(TerminalObserver::new(depth));
        let mut spectral = light.lit(
            &ctx,
            hornvale_kernel::WorldTime::GENESIS,
            plate::season_bucket(0.0),
            None,
        );

        plate::draw_with(
            terrain,
            geo,
            index,
            &f,
            &win,
            super::wash_support::W,
            super::wash_support::H,
            colour_allowed,
            &[],
            &BTreeSet::new(),
            &[],
            &Default::default(),
            &mut spectral,
        )
    }

    /// Every drawn position of a grid as `(glyph, ink)`, in row-major
    /// order.
    ///
    /// The pair rather than the grid's own struct, deliberately: that
    /// struct is named for the raster-area word the Lexicon of Place guard
    /// counts (`cli/tests/suite/lexicon_guard.rs`), and these two fields are
    /// the whole of what this module reads. Destructuring at the boundary
    /// costs nothing and keeps a token this file has no need of out of the
    /// inventory.
    fn drawn_positions(g: &Grid) -> Vec<(Option<char>, Ink)> {
        (0..g.height())
            .flat_map(|y| (0..g.width()).filter_map(move |x| g.get(x, y).map(|c| (c.glyph, c.ink))))
            .collect()
    }

    /// Every distinct colour a grid claims.
    fn distinct_colours(g: &Grid) -> BTreeSet<[u8; 3]> {
        drawn_positions(g)
            .iter()
            .filter_map(|(_, ink)| match ink {
                Ink::Rgb(rgb) => Some(*rgb),
                Ink::Plain => None,
            })
            .collect()
    }

    /// The lowest water class [`plate::color_for`] answers from
    /// `tile.reflectance` — its spectral `_` arm. Classes 0 (ocean) and 1
    /// (salt basin) take client-authored CONSTANT spectra
    /// (`ocean_reflectance()`, `salt_basin_reflectance()`) and never read
    /// the ground at all; 2 (river) and 3 (dry land) do. Mirrors
    /// `color_for`'s own match, which is the only place the split is
    /// decided.
    const FIRST_SPECTRAL_WATER_CLASS: u8 = 2;

    /// What [`plate::color_for`] claims over the sample window, split by the
    /// axis the two assertions below need it split by:
    ///
    /// 0. **every** colour, all four water classes — the population for the
    ///    count and wiring halves, which ask what a whole plate paints;
    /// 1. the colours from the **spectral `_` arm alone** (`water >=`
    ///    [`FIRST_SPECTRAL_WATER_CLASS`]) — the population for the
    ///    chromaticity half;
    /// 2. how many tiles actually took that arm AND resolved a colour there.
    ///
    /// **Why the chromaticity half must NOT see the wet arms, even though
    /// B6 put them under the same illuminant.** B6 routes ocean and salt
    /// basin through `TerminalObserver::observe` rather than the old
    /// fixed-sRGB `show`, so all four classes now vary on the same
    /// ILLUMINANT axis — which is what made widening this population look
    /// safe. The chromaticity assertion measures the other axis:
    /// `color_for`'s wet arms read client-authored constant spectra and
    /// never `tile.reflectance`, so a greyscale collapse of the `_` arm
    /// leaves ocean-blue and salt-basin-white untouched, and those two alone
    /// clear the floor with ~20x headroom.
    ///
    /// Measured, not reasoned, twice over. Collapsing the `_` arm to
    /// `[g, g, g]` left the whole-DRAWN-GRID form of the hue assertion GREEN
    /// (satisfied by ocean, salt basin and the river line `rasterize_rivers`
    /// paints over the relief), which is why this reads `color_for` directly
    /// rather than the grid. The same mutation then left the WIDENED form
    /// green too — the same blind spot one level down — which is why the
    /// `water >=` scoping is back for that half only.
    fn spectral_land_colours() -> (BTreeSet<[u8; 3]>, BTreeSet<[u8; 3]>, usize) {
        let ctx = super::wash_support::seed_42_context();
        let terrain = ctx.terrain();
        let geo = terrain.geosphere();
        let index = ctx.nearest_index();
        let (f, win, vw, vh) = super::wash_support::frame_and_window();
        let mut memo = super::wash_support::memo();
        let light = PlateLight::flat(true);

        let mut colours = BTreeSet::new();
        let mut spectral = BTreeSet::new();
        let mut spectral_sampled = 0usize;
        for (row, col) in super::wash_support::sample_tiles() {
            let t = plate::terrain_at_tile(
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
                hornvale_kernel::WorldTime::GENESIS,
                plate::season_bucket(0.0),
                None,
            );
            if let Some(c) = plate::color_for(&t, light.illuminant(), light.observer()) {
                colours.insert(c);
                if t.water >= FIRST_SPECTRAL_WATER_CLASS {
                    spectral.insert(c);
                    spectral_sampled += 1;
                }
            }
        }
        (colours, spectral, spectral_sampled)
    }

    /// A colour's CHROMATICITY — its `(r, g)` share of total intensity.
    ///
    /// **The instrument finding (c) asked for.** Counting distinct colours
    /// cannot tell a spectral render from a greyscale one: a bug that copied
    /// one channel into all three would still produce many distinct values,
    /// and Task 2's own palette test used `r == g == b` inputs throughout,
    /// so nothing upstream had ever exercised an unequal channel. Dividing
    /// out intensity leaves only the part a greyscale render cannot vary —
    /// every grey, black to white, maps to the same `(1/3, 1/3)`.
    fn chromaticity(rgb: [u8; 3]) -> (f64, f64) {
        let total = f64::from(rgb[0]) + f64::from(rgb[1]) + f64::from(rgb[2]);
        if total <= 0.0 {
            return (1.0 / 3.0, 1.0 / 3.0);
        }
        (f64::from(rgb[0]) / total, f64::from(rgb[1]) / total)
    }

    /// Every colour the pre-Wash plate could possibly claim: the six-entry
    /// `RELIEF_COLORS` ladder plus the three invented water-palette entries
    /// (`OCEAN_COLOR`, `SALT_BASIN_COLOR`, `RIVER_COLOR`).
    ///
    /// **This ceiling is 9, and the brief for this task said 6.** Six is the
    /// ladder alone, and it is not the number a whole-plate colour count has
    /// to beat: a plate also carries water, so a render with the ladder
    /// fully restored produces 6 + 3 = 9 distinct colours and clears a
    /// `> 6` bar without difficulty. Measured, not reasoned: restoring the
    /// deleted ladder inside `plate::color_for` and re-running left the
    /// `> 6` form of this test GREEN. A threshold that the defect itself
    /// passes is not a threshold.
    ///
    /// The feature layer contributes nothing to this count — `plate_at`
    /// passes empty site, volcano and waterfall rosters — so 9 really is
    /// the whole of what the old vocabulary could paint here.
    const PRE_WASH_PALETTE: usize = 9;

    /// H1: the category error is fixed. Distinct rendered colours per plate
    /// strictly exceed what the whole pre-Wash palette could produce.
    ///
    /// Measured on COLOUR COUNT, never on a banded value: The Hachure's H2
    /// was falsified because it asserted about a quantity whose quantizer
    /// was coarse enough to erase the refinement, and colour count is
    /// downstream of no band.
    ///
    /// Measured on the DRAWN GRID rather than on `color_for`'s return value,
    /// so what it witnesses is the picture a reader sees and not a function
    /// nothing calls — which is the failure mode this whole task exists to
    /// close.
    #[test]
    fn the_plate_renders_more_distinct_colours_than_the_elevation_ladder_could() {
        let colours = distinct_colours(&plate_at(true, ColorDepth::TrueColor));
        assert!(
            colours.len() > PRE_WASH_PALETTE,
            "the whole pre-Wash palette was {PRE_WASH_PALETTE} entries              (six relief bands plus ocean, salt basin and river);              the spectral path rendered {}",
            colours.len()
        );
    }

    /// H1, the half a count cannot witness: the plate's colours differ in
    /// HUE, not merely in value.
    ///
    /// **FIRES WHEN: the collapse goes achromatic** — a channel copied into
    /// all three, an observer that lost its colour-matching curves, an
    /// illuminant applied as a scalar. Every one of those leaves the
    /// distinct-colour count healthy while destroying the thing H1 is
    /// actually about, because a greyscale ramp has as many distinct values
    /// as a coloured one.
    ///
    /// The tolerance is not a tuned threshold — it is a floor well under
    /// what any real spectral difference produces and well over float noise;
    /// the failure message prints the observed spread so a future reader can
    /// see how much headroom the claim actually has.
    #[test]
    fn the_plates_colours_differ_in_hue_and_not_only_in_value() {
        let (set, spectral_set, sampled) = spectral_land_colours();
        let colours: Vec<[u8; 3]> = spectral_set.iter().copied().collect();

        // NON-VACUITY, both halves. A window that held no land at all, or a
        // context that refused every address, would leave nothing to compare
        // and the assertion below would hold for the wrong reason.
        //
        // `sampled` counts tiles that TOOK THE SPECTRAL ARM and resolved a
        // colour there — not tiles asked. The sample window is a fixed
        // `24 * 80`, so a count of tiles asked is a constant that no defect
        // can move, and a context resolving zero ground reflectances would
        // report 1920 and pass while the assertion's own message
        // ("degenerate") was exactly true.
        assert!(
            sampled > 100,
            "only {sampled} tiles of the window took the spectral arm; the sample is degenerate"
        );
        assert!(
            colours.len() > 1,
            "the spectral arm produced {} distinct colours over {sampled} tiles; \
             a hue comparison needs at least two",
            colours.len()
        );

        let mut widest = 0.0f64;
        for a in &colours {
            for b in &colours {
                let (ax, ay) = chromaticity(*a);
                let (bx, by) = chromaticity(*b);
                widest = widest.max((ax - bx).abs()).max((ay - by).abs());
            }
        }
        assert!(
            widest > 0.01,
            "the spectral arm's {} distinct colours span only {widest:.6} of \
             chromaticity — a greyscale render would look exactly like this to a \
             count, which is what this assertion exists to catch",
            colours.len()
        );

        // WIRING: the colours measured above must be the colours DRAWN. This
        // test reaches `color_for` directly, so without this it would prove a
        // property of a function the renderer might not call — the exact
        // failure ("shipped with no consumer") this whole task exists to
        // close, and the one Task 1's `violations` actually committed.
        //
        // Measured over the WIDE population (every water class), unlike the
        // chromaticity half above: this asks whether the renderer paints
        // what `color_for` answers, and that question is about the whole
        // function, not about one of its arms.
        let drawn = distinct_colours(&plate_at(true, ColorDepth::TrueColor));
        let shared = set.intersection(&drawn).count();
        assert!(
            // A MAJORITY, not all of them, and the shortfall is accounted
            // for rather than tolerated: `rasterize_rivers` paints its line
            // layer OVER the relief inside the same cached tile, so a land
            // colour whose every tile carries a river is genuinely not on
            // the finished plate. Measured at 138 of 156 (88%) on this
            // window; half is the floor, chosen far enough below that
            // figure that a river-density change cannot flap it, and far
            // enough above zero that a renderer ignoring `color_for`
            // entirely still fails.
            shared * 2 > set.len(),
            "only {shared} of `color_for`'s {} colours reached the drawn plate; \
             the renderer is not painting what `color_for` answers",
            set.len()
        );
    }

    /// FIRES WHEN: the glyph stops carrying elevation. 0389 gives the glyph
    /// the order, and the NO_COLOR path depends on it entirely.
    #[test]
    fn the_glyph_still_carries_elevation_order() {
        // Water class 3 is `WaterKind::DryLand` — the only class whose mark
        // the relief band decides (`plate::glyph_for`'s own doc). Asking
        // this of an ocean tile would compare a glyph against itself.
        let a = plate::glyph_for(3, 2);
        let b = plate::glyph_for(3, 4);
        assert_ne!(a, b, "two different bands must draw different glyphs");
    }

    /// H5: the observer degrades without loss of legibility. Under NO_COLOR
    /// the plate emits no colour at all AND still distinguishes elevation,
    /// because the glyph carries it. Neither half alone is the claim — a
    /// plate that emits no colour and no distinction is "degraded" into
    /// uselessness, and a plate that still emits colour has not degraded at
    /// all. This is the test that 0389's "nothing a reader must trust may
    /// live only in colour" is honoured on the shipped surface rather than
    /// in principle.
    ///
    /// **Two arms, and the second is the load-bearing one.** The first
    /// drives the shipped coupling (`NO_COLOR` sets both `colour_allowed`
    /// and the observer's depth, `observer::terminal_observer`), which
    /// `Ink::resolve` alone would satisfy — it forces `Ink::Plain` whenever
    /// colour is disallowed, so arm one cannot tell whether the OBSERVER
    /// degraded or merely whether the ink gate closed. Arm two allows colour
    /// and hands the plate a `ColorDepth::None` observer, so the only thing
    /// that can produce a colourless plate is the observer itself. That is
    /// also why `plate::color_for`'s wet arms go through the observer rather
    /// than emitting a colour raw: without that, arm two would find ocean
    /// tiles still coloured. Before The Newel (B6) the call they made was
    /// `TerminalObserver::show` on an invented sRGB triple; it is
    /// `TerminalObserver::observe` on an authored spectrum now, and this
    /// arm's claim is unchanged by the swap because both end at the same
    /// depth collapse.
    #[test]
    fn a_no_colour_plate_emits_no_colour_and_still_shows_relief() {
        for (colour_allowed, depth, arm) in [
            (false, ColorDepth::None, "the shipped NO_COLOR coupling"),
            (
                true,
                ColorDepth::None,
                "a colour-allowing terminal that shows none",
            ),
        ] {
            let drawn = plate_at(colour_allowed, depth);
            let positions = drawn_positions(&drawn);

            assert!(
                positions.iter().all(|(_, ink)| *ink == Ink::Plain),
                "{arm}: must emit no colour anywhere; {} of {} positions claimed one",
                positions
                    .iter()
                    .filter(|(_, ink)| *ink != Ink::Plain)
                    .count(),
                positions.len()
            );

            let glyphs: BTreeSet<char> = positions.iter().filter_map(|(g, _)| *g).collect();
            assert!(
                glyphs.len() > 1,
                "{arm}: the plate must still distinguish relief without colour; \
                 got {} distinct glyphs",
                glyphs.len()
            );
        }
    }

    /// The positive control for the arms above: the SAME plate, same window,
    /// with a truecolor observer, must claim colour. Without it both arms
    /// would pass on a plate that had stopped drawing anything at all.
    #[test]
    fn the_same_plate_does_claim_colour_when_the_terminal_can_show_it() {
        let drawn = plate_at(true, ColorDepth::TrueColor);
        assert!(
            drawn_positions(&drawn)
                .iter()
                .any(|(_, ink)| *ink != Ink::Plain),
            "a truecolor plate claimed no colour anywhere"
        );
    }

    /// H4 over the REAL layers, not hand-built ones. FIRES WHEN: a layer is
    /// declared at a rate coarser than something it reads — e.g. if
    /// `terrain` were declared `Seasonal` while reading the diurnal
    /// illuminant, which is exactly the bug the spine exists to prevent and
    /// exactly what this campaign would have shipped without this step.
    ///
    /// **The honest limit, restated here because a green assertion is where
    /// a reader will look for it:** `plate::LAYERS` is a DECLARATION.
    /// Nothing derives it from what the code reads, so if a layer's reads
    /// change and the declaration does not, this stays green and says
    /// nothing. What it does buy is that the declaration and the spine can
    /// no longer disagree silently, which is strictly more than Task 1
    /// shipped — `violations` had no consumer at all, so H4 was vacuous.
    #[test]
    fn the_plates_declared_layers_satisfy_the_rate_spine() {
        let found = hornvale_game::rate::violations(plate::LAYERS);
        assert_eq!(
            found,
            Vec::<String>::new(),
            "rate-spine violations: {found:?}"
        );
    }

    /// The non-vacuity arm for the assertion above: the spine must still
    /// object to a layer that IS mis-declared. Without this,
    /// `the_plates_declared_layers_satisfy_the_rate_spine` would pass for a
    /// `violations` that had been changed to return an empty vector.
    #[test]
    fn the_spine_still_objects_to_a_terrain_layer_declared_coarser_than_its_light() {
        let bad = [hornvale_game::rate::LayerDecl {
            name: "terrain",
            rate: hornvale_game::rate::Rate::Seasonal,
            reads: &[hornvale_game::rate::Rate::Diurnal],
        }];
        let found = hornvale_game::rate::violations(&bad);
        assert_eq!(found.len(), 1, "expected one violation, got {found:?}");
    }

    /// FIRES WHEN: [`hornvale_game::rate::Rate`]'s coarse-to-fine ORDER
    /// moves.
    ///
    /// **Written because Task 6 made one of the unpinned middle variants
    /// load-bearing.** Task 1's own tests pin only `Geological` vs
    /// `Seasonal`; the relative position of `Built`, `Diurnal`, `PerTurn`
    /// and `Ornamental` rested on a doc comment. `plate::LAYERS` now
    /// declares the terrain layer `Diurnal` and asserts the spine over it,
    /// and `violations` compares variants with `>` — so a reordering of this
    /// enum would silently change what that green assertion MEANS without
    /// changing a character of either file.
    ///
    /// **The exhaustive `match` is the ratchet.** A new variant fails to
    /// compile here rather than slipping into the middle of the ladder
    /// unnoticed, which a list of pairwise `<` assertions would have
    /// allowed.
    #[test]
    fn the_rate_ladder_runs_coarse_to_fine_in_the_declared_order() {
        use hornvale_game::rate::Rate;

        fn rung(r: Rate) -> usize {
            match r {
                Rate::Geological => 0,
                Rate::Built => 1,
                Rate::Seasonal => 2,
                Rate::Diurnal => 3,
                Rate::PerTurn => 4,
                Rate::Ornamental => 5,
                Rate::Instantaneous => 6,
            }
        }

        let ladder = [
            Rate::Geological,
            Rate::Built,
            Rate::Seasonal,
            Rate::Diurnal,
            Rate::PerTurn,
            Rate::Ornamental,
            Rate::Instantaneous,
        ];

        for (i, a) in ladder.iter().enumerate() {
            assert_eq!(
                rung(*a),
                i,
                "{a:?} is not at rung {i} of the declared ladder"
            );
            for b in &ladder[i + 1..] {
                assert!(
                    a < b,
                    "{a:?} must compare coarser than {b:?}; the enum's discriminant order moved"
                );
            }
        }
    }
}

/// FIRES WHEN: the `(FacetId, season)` cache stops SERVING and starts
/// silently recomputing.
///
/// **Why this test exists (Task 6, carried finding (d)).** Task 4's
/// `a_redraw_in_the_same_season_adds_no_cache_entries` above asserts that a
/// second draw leaves `store.len()` unchanged — and it cannot fail:
/// `BTreeMap::insert` under an identical key never grows the map, so a hit
/// path replaced by "recompute and reinsert" passes it exactly as well. The
/// hit path was a structural guarantee that nothing tested. Task 6 puts it
/// under ~1,920 consults a frame, which is where an unnoticed miss stops
/// being free.
///
/// **Counted, not timed.** `Instant` is banned in this project, tests
/// included, so the question "was it served from the cache" is answered by
/// how many times the expensive locale call was MADE
/// (`plate::ReflectanceCache::misses`, incremented at that call and nowhere
/// else) rather than by how long the draw took. Nothing in `windows/locale`
/// was touched to get this: the counter sits in the client's own wrapper.
#[test]
fn a_same_season_redraw_is_served_from_the_cache_rather_than_recomputed() {
    let (session, geo, terrain, index, mut memo, calendar, _world) = wash_support::seed_42_world();
    let mut store = hornvale_game::plate::ReflectanceCache::new();
    let ctx = session.context();
    let at = session.day();
    let calendar = calendar.as_ref();

    wash_support::draw_once(
        &terrain, &geo, &index, &mut memo, ctx, at, calendar, &mut store,
    );
    assert_eq!(
        (store.hits(), store.misses(), store.len()),
        (0, 1, 1),
        "the first draw of one tile must be exactly one miss and one entry"
    );

    wash_support::draw_once(
        &terrain, &geo, &index, &mut memo, ctx, at, calendar, &mut store,
    );
    assert_eq!(
        (store.hits(), store.misses(), store.len()),
        (1, 1, 1),
        "the second draw must be served from the cache: the locale must not be \
         called again, and no entry may be added"
    );
}

/// The non-vacuity arm the test above needs, and the spine's own claim made
/// concrete: a draw at an instant in a DIFFERENT season must MISS.
///
/// Without this, `a_same_season_redraw_is_served_from_the_cache_rather_than_
/// recomputed` would pass just as happily for a cache whose key had dropped
/// the season column entirely — which is the freeze-on-the-first-frame
/// defect `plate::ReflectanceKey`'s own doc describes.
///
/// **The second instant is SEARCHED for, never assumed.** `season` must be
/// `season_bucket_for(calendar, at)` for the `at` it is filed under
/// (`plate::terrain_at_tile`'s own contract), so this walks forward a day at
/// a time until the world's own calendar reports a different bucket rather
/// than passing a hand-picked `season` beside an unchanged `at` — which
/// would file a value under a key that lies about it, exactly the
/// mislabeling that contract exists to forbid.
#[test]
fn a_redraw_in_a_different_season_misses_and_mints_a_new_entry() {
    let (session, geo, terrain, index, mut memo, calendar, _world) = wash_support::seed_42_world();
    let mut store = hornvale_game::plate::ReflectanceCache::new();
    let ctx = session.context();
    let at = session.day();
    let calendar = calendar.as_ref();
    let here = hornvale_game::driver::season_bucket_for(calendar, at);

    // A year is bounded above by a few hundred standard days for any world
    // this client can possess, so a full year's walk is a bounded search and
    // never an open-ended one.
    let elsewhere = (1..=400i64)
        .map(|d| {
            hornvale_kernel::WorldTime::from_ticks(
                at.ticks() + d * hornvale_kernel::WorldTime::TICKS_PER_STD_DAY,
            )
        })
        .find(|t| hornvale_game::driver::season_bucket_for(calendar, *t) != here)
        .expect("seed 42's calendar must reach a second season within a year");

    wash_support::draw_once(
        &terrain, &geo, &index, &mut memo, ctx, at, calendar, &mut store,
    );
    wash_support::draw_once(
        &terrain, &geo, &index, &mut memo, ctx, elsewhere, calendar, &mut store,
    );

    assert_eq!(
        (store.hits(), store.misses(), store.len()),
        (0, 2, 2),
        "a second season must not be served the first season's reflectance"
    );
}

// =====================================================================
// Task 7: the seasonal layer — H2's seasonal half.
// =====================================================================

/// The seasonal half of H2, measured on the quantity that actually reaches
/// the map.
///
/// # What is measured, and why this and not the snow endmember's weight
///
/// **Albedo** — the sum of the ten bands of
/// [`hornvale_locale::LocaleContext::reflectance_at_facet`] — at one facet,
/// at midwinter and at midsummer. That call is not a proxy for the map's
/// ink: it is the *same call the plate makes*, pinned equal to the plate's
/// own per-tile answer by this file's
/// `two_tiles_in_one_band_with_different_ground_differ_spectrally`
/// (`assert_eq!(t.reflectance, ctx.reflectance_at_facet(&t.facet, at))`).
///
/// The brief's alternative — count `Snow`-dominant tiles behind a new
/// `cover_class_at_facet` on `windows/locale` — was not needed, and the
/// reason is worth recording: `LocaleContext::cover_class_at` is **already**
/// public and the `MicroField` it wants is **already** reachable, off
/// `describe(addr, at)?.regime.micro`. Verified line by line, not assumed:
/// `describe_with_weights` builds it as
/// `micro_field(addr.seed(self.seed), grounded_wetness_for(addr, expr,
/// blend(moisture)))` with `expr = climate.biome_expr_at(dominant_corner)`
/// for `stratum: None`, and `reflectance_at_facet_with_weights` builds it
/// from the identical three expressions. Task 3's finding — that a client
/// cannot *construct* a correct `MicroField` — is exact and unchanged; it
/// does not say a client cannot *obtain* one. So no new public API was
/// added.
///
/// # How the sample is established to actually snow
///
/// A tropical facet has no snow in either season, so "no difference" there
/// would falsify nothing. The precondition is therefore asserted, not
/// assumed, and through a **different quantity than the one measured**: the
/// dominant [`hornvale_locale::CoverClass`] must be `Snow` at the winter
/// instant and something other than `Snow` at the summer instant. Dominance
/// is an argmax over component *weights*; albedo is a band sum of the
/// integrated mixture, mineral remainder included. One does not entail the
/// other arithmetically — it entails it only if the composition is wired
/// the way spec §3.2 says, which is the claim.
///
/// The sweep is over *every* qualifying facet the scan finds, not the first
/// one: a per-place assertion cannot be satisfied by having picked a lucky
/// place. Non-vacuity is asserted separately — a scan that qualified nothing
/// would otherwise pass by finding nothing to check.
///
/// # The trap the brief names, and what the code actually does
///
/// The brief warns that leaf-off lowers forest albedo in winter, so a
/// whole-plate mean can cancel to nothing. Grepped rather than trusted:
/// `windows/locale/src/surface.rs`'s `cover_components` reads `at` in
/// **exactly one place**, `climate.is_frozen_at(vertex, at)`. There is no
/// leaf-off term — the chlorophyll/litter split is set by `micro.openness`,
/// which is time-independent. So the trap does not exist in today's model,
/// and this test still refuses to average over a plate, because the trap
/// would return the moment a seasonal vegetation term were added.
///
/// # What it measured (seed 42, this commit)
///
/// **H2's seasonal half holds, and the effect is large.** 77 of seed 42's
/// 40,962 grid facets (0.19%) are snow-covered at midwinter and not at
/// midsummer. Winter albedo exceeds summer albedo at **all 77**;
/// winter/summer ratio min **1.4661**, median **2.2777**, max **3.9718**.
/// The first place in scan order, `Vertex(152)` (lat 36.00), reads winter
/// **7.351812** against summer **3.358797**, its summer cover sand.
///
/// # Two red controls, both taken against PRODUCTION code
///
/// A directional assertion is worth what it can fail on, so both halves were
/// witnessed red before this was called green — by mutating
/// `windows/locale/src/surface.rs`, never the test:
///
/// - `is_frozen_at(vertex, at)` -> `is_frozen_at(vertex, WorldTime::GENESIS)`
///   (the season no longer threaded) reddens the **non-vacuity** clause:
///   "no sampled facet ... is snow-covered at midwinter and not at
///   midsummer".
/// - `endmembers::SNOW` darkened to `[0.02 .. 0.00]` reddens the
///   **directional** clause while the precondition still passes — cover is
///   still `Snow` in winter, and the panic reads "winter albedo 1.262014 <=
///   summer albedo 3.358797". That is the mutation that shows the two
///   clauses are independent: dominance did not change, brightness did.
///
/// FIRES WHEN: `at` stops being threaded to the cover layer (both seasons
/// then read identical and the strict inequality fails), when the seasonal
/// freeze gate stops being consulted, or when the composition stops letting
/// a bright cover raise the ground's albedo.
mod wash_seasonal {
    use hornvale_astronomy::{Calendar, StdInstant};
    use hornvale_kernel::math::unit_sphere_from_lat_lon;
    use hornvale_kernel::{Facet, Vertex, WorldTime};
    use hornvale_locale::{CoverClass, LocaleContext};

    /// The season phase of midwinter in the northern hemisphere.
    ///
    /// `hornvale_climate`'s seasonal term is
    /// `mean + amp * latitude.signum() * sin(TAU * phase)`
    /// (`domains/climate/src/temperature.rs`), and `Calendar::season_phase`
    /// is the same `frac(day / year + year_phase_offset)`
    /// (`domains/astronomy/src/calendar.rs`), so `sin = -1` at phase 0.75 is
    /// the northern coldest and phase 0.25 the northern warmest. Nothing here
    /// depends on that reading being right, though: the sweep below keeps
    /// only facets whose cover is snow at [`MIDWINTER_PHASE`] and not at
    /// [`MIDSUMMER_PHASE`], so a hemisphere convention read backwards would
    /// simply select southern places instead of northern ones and the
    /// directional claim would be unaffected.
    const MIDWINTER_PHASE: f64 = 0.75;
    /// See [`MIDWINTER_PHASE`]. Half a year away from it.
    const MIDSUMMER_PHASE: f64 = 0.25;

    /// Distance between two phases on the unit circle, `[0, 0.5]`.
    fn phase_distance(a: f64, b: f64) -> f64 {
        let d = (a - b).abs().rem_euclid(1.0);
        d.min(1.0 - d)
    }

    /// The **whole** day of the world's first year whose
    /// [`Calendar::season_phase`] sits nearest `target`.
    ///
    /// Whole days, deliberately: `temperature_at` carries a diurnal term
    /// keyed on `day.rem_euclid(1.0)` as well as the seasonal one, so two
    /// instants at a fractional offset from each other would differ by an
    /// hour-of-day term this test makes no claim about. Both samples land at
    /// fraction zero, which holds that term as nearly fixed as a whole-day
    /// lattice allows.
    ///
    /// Scanned rather than solved algebraically so the phase convention is
    /// read off the calendar itself instead of re-derived from
    /// `year_phase_offset` here — a second copy of a formula this test would
    /// then be asserting against itself.
    fn whole_day_nearest_phase(
        calendar: &Calendar,
        year_length_std: f64,
        target: f64,
    ) -> WorldTime {
        let days = year_length_std.ceil().max(1.0) as i64;
        let mut best: Option<(f64, i64)> = None;
        for d in 0..days {
            let Ok(t) = StdInstant::new(d as f64) else {
                continue;
            };
            let Some(phase) = calendar.season_phase(t) else {
                continue;
            };
            let err = phase_distance(phase, target);
            if best.is_none_or(|(e, _)| err < e) {
                best = Some((err, d));
            }
        }
        let (_, day) = best.expect(
            "seed 42's calendar must report a season phase on at least one day of its own year; \
             without one there is no midwinter to sample and this measurement has no subject",
        );
        WorldTime::from_std_days(day as f64).expect("a whole day inside the first year is a tick")
    }

    /// The ground's albedo at `addr` on `at`: the sum of the ten bands of the
    /// reflectance the plate itself draws with. Higher is brighter.
    fn albedo(ctx: &LocaleContext, addr: &Facet, at: WorldTime) -> Option<f64> {
        ctx.reflectance_at_facet(addr, at)
            .ok()
            .map(|r| r.get().iter().sum())
    }

    /// The dominant surface cover at `addr` on `at`, taken through the
    /// `MicroField` the reflectance path itself builds (see this module's
    /// doc for the line-by-line agreement that makes that true).
    fn cover(ctx: &LocaleContext, addr: &Facet, at: WorldTime) -> Option<CoverClass> {
        let micro = ctx.describe(addr, at).ok()?.regime.micro;
        ctx.cover_class_at(addr, &micro, at).ok()
    }

    /// One place that snows in winter and not in summer, with both albedos.
    struct SnowyPlace {
        /// The vertex whose coordinate seeded the address.
        vertex: Vertex,
        /// Its latitude in degrees, for the report.
        latitude: f64,
        /// Ground albedo at the midwinter instant.
        winter: f64,
        /// Ground albedo at the midsummer instant.
        summer: f64,
        /// The dominant cover class in summer (winter is `Snow` by
        /// selection) — reported so a reader can see what the snow gave way
        /// to.
        summer_cover: CoverClass,
    }

    #[test]
    fn midwinter_differs_from_midsummer_directionally_at_a_place_that_snows() {
        let (session, _geo, _terrain, _index, _memo, calendar, _world) =
            super::wash_support::seed_42_world();
        let ctx = session.context();
        let calendar = calendar.expect(
            "seed 42 under a generated sky must carry a calendar; without one there are no \
             seasons to sample and this measurement has no subject",
        );

        let year = ctx.climate().year_length_std();
        let winter = whole_day_nearest_phase(&calendar, year, MIDWINTER_PHASE);
        let summer = whole_day_nearest_phase(&calendar, year, MIDSUMMER_PHASE);
        assert_ne!(
            winter, summer,
            "midwinter and midsummer must be different days"
        );

        let geo = ctx.climate().geosphere();
        let depth = hornvale_game::plate::GLOBE_RUNG;

        let mut places: Vec<SnowyPlace> = Vec::new();
        // EVERY vertex, not a stride. Measured before choosing: a stride of
        // 97 (422 candidates) qualified 2 places, a stride of 13 (3,151
        // candidates) qualified 5, and the whole grid (40,962) qualifies 77
        // — a seasonally-snowing facet is rare enough on seed 42 (0.19% of
        // the grid) that a subsample is a handful of anecdotes. The whole
        // sweep costs ~6 s on top of the ~5 s the fixture already pays, and
        // this file is not in the commit gate's sub-floor tier (`clients/
        // game/bin` is in root `Cargo.toml`'s `exclude`), so `make
        // game-check` is what pays it.
        for i in 0..geo.vertex_count() {
            let vertex = Vertex(i as u32);
            let coord = geo.coord(vertex);
            let addr = Facet::containing(
                unit_sphere_from_lat_lon(coord.latitude, coord.longitude),
                depth,
            );
            let (Some(cw), Some(cs)) = (cover(ctx, &addr, winter), cover(ctx, &addr, summer))
            else {
                continue;
            };
            if cw != CoverClass::Snow || cs == CoverClass::Snow {
                continue;
            }
            let (Some(aw), Some(asu)) = (albedo(ctx, &addr, winter), albedo(ctx, &addr, summer))
            else {
                continue;
            };
            places.push(SnowyPlace {
                vertex,
                latitude: coord.latitude,
                winter: aw,
                summer: asu,
                summer_cover: cs,
            });
        }

        // NON-VACUITY: without this, a scan that qualified nothing would
        // satisfy the per-place assertion below by having nothing to check.
        assert!(
            !places.is_empty(),
            "no sampled facet on seed 42 is snow-covered at midwinter and not at midsummer, so \
             there is nowhere the seasonal term can be observed at all; the sample is bad, not \
             the hypothesis"
        );

        for p in &places {
            assert!(
                p.winter > p.summer,
                "H2 (seasonal) FALSIFIED at {:?} (lat {:.2}): a place that is snow-covered at \
                 midwinter and {} at midsummer must be brighter in winter, but winter albedo \
                 {:.6} <= summer albedo {:.6}",
                p.vertex,
                p.latitude,
                p.summer_cover.name(),
                p.winter,
                p.summer
            );
        }

        // The measured numbers, printed so the campaign's report carries the
        // effect SIZE and not merely its sign — a real-but-tiny effect is a
        // different finding from a robust one, and an assertion cannot tell
        // the two apart.
        let first = &places[0];
        let ratio = |p: &SnowyPlace| p.winter / p.summer;
        let mut ratios: Vec<f64> = places.iter().map(ratio).collect();
        ratios.sort_by(f64::total_cmp);
        println!(
            "H2 seasonal: {} snowing places sampled; winter/summer albedo ratio min {:.4} \
             median {:.4} max {:.4}; first place {:?} (lat {:.2}) winter {:.6} summer {:.6} \
             (summer cover {})",
            places.len(),
            ratios[0],
            ratios[ratios.len() / 2],
            ratios[ratios.len() - 1],
            first.vertex,
            first.latitude,
            first.winter,
            first.summer,
            first.summer_cover.name(),
        );
    }
}

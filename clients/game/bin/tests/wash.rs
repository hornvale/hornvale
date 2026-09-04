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
    use hornvale_game::plate::{self, ReflectanceKey, Window};
    use hornvale_kernel::{
        ComponentStore, Geosphere, NearestVertexIndex, RoomMeshMemo, Seed, World,
    };
    use hornvale_locale::LocaleContext;
    use hornvale_terrain::GeneratedTerrain;
    use hornvale_vessel::Session;

    /// The window's width and height in terminal columns and rows — an
    /// 80x24 terminal, what a player actually sees at the client's floor
    /// rung.
    pub const W: u16 = 80;
    /// See [`W`].
    pub const H: u16 = 24;
    /// A representative mid-northern latitude, degrees — the observer
    /// latitude Task 5's illuminant tests sample at. Not seed 42's flagship
    /// latitude (this file has no need of that one specifically): any fixed
    /// latitude for which the sun clears the horizon at both sample
    /// elevations below serves the directional claim just as well.
    pub const SAMPLE_LATITUDE_DEG: f64 = 45.0;

    /// Seed 42's locale context, built from a bare world. Everything else
    /// this file needs — the terrain, the geosphere, the nearest-vertex
    /// index — is read back off it, so the world is derived once.
    pub fn seed_42_context() -> LocaleContext {
        LocaleContext::build(&World::new(Seed(42))).expect("a bare seed-42 world builds a context")
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
            hornvale_worldgen::SkyChoice::Generated,
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
            .and_then(|sky| sky.calendar().cloned());

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
        store: &mut ComponentStore<ReflectanceKey, hornvale_kernel::color::Reflectance>,
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
    let mut store = hornvale_kernel::component::ComponentStore::new();
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

/// FIRES WHEN: a starless world (tier-0 `ConstantSun`, no calendar) stops
/// falling back to a flat, colourless illuminant and instead panics or
/// silently guesses a sun it cannot honestly place. The two `None` cases
/// [`hornvale_game::driver::plate_illuminant`] documents are modelled
/// worlds, not errors — this is the first of them; the repo's own keystone
/// fixture is generated `--sky constant`, so it is not exotic.
#[test]
fn a_starless_world_lights_the_plate_flat() {
    let world = hornvale_worldgen::build_world(
        hornvale_kernel::Seed(42),
        &hornvale_astronomy::SkyPins::default(),
        hornvale_worldgen::SkyChoice::Constant,
        &hornvale_terrain::TerrainPins::default(),
        &hornvale_worldgen::SettlementPins::default(),
    )
    .expect("seed 42 generates under a constant sun");
    let calendar = hornvale_worldgen::sky_of(&world)
        .ok()
        .and_then(|sky| sky.calendar().cloned());
    assert!(
        calendar.is_none(),
        "a tier-0 constant sun must have no calendar to place a sun by"
    );

    let lit = hornvale_game::driver::plate_illuminant(
        &world,
        calendar.as_ref(),
        hornvale_kernel::WorldTime::GENESIS,
        wash_support::SAMPLE_LATITUDE_DEG,
    );

    assert_eq!(
        *lit.get(),
        [1.0; hornvale_kernel::color::BANDS],
        "a starless world must fall back to a flat unit illuminant"
    );
}

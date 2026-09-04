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
    use hornvale_game::plate::{self, Window};
    use hornvale_kernel::{RoomMeshMemo, Seed, World};
    use hornvale_locale::LocaleContext;

    /// The window's width and height in terminal columns and rows — an
    /// 80x24 terminal, what a player actually sees at the client's floor
    /// rung.
    pub const W: u16 = 80;
    /// See [`W`].
    pub const H: u16 = 24;

    /// Seed 42's locale context, built from a bare world. Everything else
    /// this file needs — the terrain, the geosphere, the nearest-vertex
    /// index — is read back off it, so the world is derived once.
    pub fn seed_42_context() -> LocaleContext {
        LocaleContext::build(&World::new(Seed(42))).expect("a bare seed-42 world builds a context")
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

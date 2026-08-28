//! The sky view (Task 4) — the first view the frame ever shows.
//!
//! **Why this view goes first**: [`hornvale_worldgen::BuildDepth::Astronomy`]
//! is the shallowest rung the ladder has, and everything a night sky needs —
//! every neighbor star's class, distance, brightness, and its own celestial
//! coordinates — is committed by the time that rung fires. So `sky` opens
//! *complete*, where every later view (the atlas, the almanac, the tongue)
//! opens empty until its own rung lands. [`SkyView::render`] must therefore
//! be a pure function of the committed neighbor facts alone: identical
//! output at [`BuildDepth::Astronomy`] and at every rung after it is the
//! property this module's tests exist to hold.
//!
//! # Route chosen: the ledger, not the almanac
//!
//! Two routes reach a neighbor's coordinates (see the task brief's resolution
//! of this ambiguity). This view takes the **ledger** route —
//! [`hornvale_astronomy::facts::IS_NEIGHBOR`] and its four sibling
//! predicates, read straight off `world.ledger` — and does not depend on
//! `hornvale-almanac` at all. Two things were checked before settling this,
//! not assumed:
//!
//! 1. `hornvale_worldgen::almanac_context` — the obvious almanac entry point
//!    — calls `terrain_of`/`climate_from` internally (see its body in
//!    `windows/worldgen/src/lib.rs`). Those two functions are banned from
//!    this crate's own call sites (decision 0092), and calling them from
//!    inside a rung-0 render would also cost ~199 ms + ~69 ms *every* render
//!    for data this view never uses (no view here reads terrain or climate).
//! 2. The narrower `hornvale_worldgen::night_sky_lines(world)` needs no
//!    terrain/climate, but its heliacal-event line resolves its observing
//!    latitude from `hornvale_terrain::places(world).first()` — `None`
//!    (falling back to a fixed 35° reference) before any settlement is
//!    placed, and a real committed latitude once one is. That means its
//!    OUTPUT would change between the astronomy-only rung and a later one,
//!    which is exactly the property this view must not have. That is a real
//!    reason, not a preference, so the ledger route is what is implemented
//!    below; nothing here depends on `hornvale-almanac`, and no manifest
//!    change was needed.
//!
//! The cost of NOT reading almanac captions is real too: this view draws
//! only the star field itself (no pole-star sentence, no wanderer count,
//! no figure summary) — see the report for why those were left for a later
//! task rather than hand-rolling a rung-0-safe subset of `night_sky_lines`
//! under time pressure.
//!
//! # The projection
//!
//! [`sky_position`] maps a star's own (right ascension, declination) onto a
//! `w`×`h` grid: right ascension (0..360°, periodic) onto the column, west
//! to east; declination (−90..90°) onto the row, north pole at the top row
//! and south pole at the bottom — an ordinary equirectangular star-chart
//! layout. See its own doc for the exact formula and worked examples.

use crate::overture::view::View;
use hornvale_astronomy::facts::{
    IS_NEIGHBOR, NEIGHBOR_BRIGHTNESS_REL, NEIGHBOR_DECLINATION_DEG, NEIGHBOR_RA_DEG,
};
use hornvale_game_core::{Cell, Grid, Source, Weight};
use hornvale_kernel::{Value, World};
use hornvale_worldgen::{BuildDepth, RungArtifacts};

/// A neighbor star's sky-relevant coordinates and brightness, read off the
/// committed ledger.
///
/// Deliberately **not** [`hornvale_astronomy::neighborhood::Neighbor`]
/// itself: that type is the *genesis-time* shape (a drawn spectral class, a
/// `color` string, a typed `LightYears` distance, …), and this view reads
/// the *presentation-time* ledger, which does not commit a neighbor's
/// color at all (only [`IS_NEIGHBOR`] and the four `NEIGHBOR_*` predicates
/// this struct mirrors). Re-hydrating the generator's own struct here would
/// mean inventing a `color` value nothing on screen reads — this struct
/// carries exactly what the projection and the glyph choice use, and no
/// more.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct SkyStar {
    /// Right ascension in degrees, 0..360 (`NEIGHBOR_RA_DEG`).
    pub right_ascension: f64,
    /// Declination in degrees, −90..90 (`NEIGHBOR_DECLINATION_DEG`).
    pub declination: f64,
    /// Apparent brightness, relative units (`NEIGHBOR_BRIGHTNESS_REL`).
    pub brightness: f64,
}

/// Read `value` as a committed [`Value::Number`], or `None` for any other
/// shape (a missing fact, or a predicate that somehow committed the wrong
/// variant — this view draws nothing for a star it cannot fully read rather
/// than guessing a coordinate).
fn as_number(value: Option<&Value>) -> Option<f64> {
    match value {
        Some(Value::Number(n)) => Some(*n),
        _ => None,
    }
}

/// Every committed neighbor star in `world`, in ledger commit order.
///
/// A neighbor missing any of the three coordinate/brightness facts is
/// skipped rather than drawn at a guessed position — this should not
/// happen for a world `genesis` produced (the three facts are committed
/// together, see `domains/astronomy/src/facts.rs`), but a partial or
/// hand-built world must not panic a view.
fn stars_of(world: &World) -> Vec<SkyStar> {
    world
        .ledger
        .find(IS_NEIGHBOR)
        .map(|fact| fact.subject)
        .filter_map(|id| {
            let declination = as_number(world.ledger.value_of(id, NEIGHBOR_DECLINATION_DEG))?;
            let right_ascension = as_number(world.ledger.value_of(id, NEIGHBOR_RA_DEG))?;
            let brightness = as_number(world.ledger.value_of(id, NEIGHBOR_BRIGHTNESS_REL))?;
            Some(SkyStar {
                right_ascension,
                declination,
                brightness,
            })
        })
        .collect()
}

/// Where `star` lands on a `w`×`h` grid — an equirectangular star-chart
/// projection of its own coordinates, and nothing else.
///
/// Right ascension is periodic (0..360°, wrapping): `col = floor(ra / 360 *
/// w)`, so `ra = 0` is the leftmost column and the column index grows
/// eastward, wrapping back to column 0 as `ra` approaches 360 from below.
/// Declination is not periodic (−90..90°, pole to pole): `row =
/// round((90 − dec) / 180 * (h − 1))`, so declination 90° (the north
/// celestial pole) is row 0 and −90° (the south pole) is row `h − 1`, the
/// ordinary "north up" convention. Both are clamped into the grid's own
/// bounds as a defensive floor against float edge cases, never as the
/// normal path.
///
/// Worked examples (`w = 78`, `h = 20`, so scaling by `w = 78` and
/// `h − 1 = 19`):
/// - `(ra=0, dec=90)` → col `floor(0/360×78)=0`, row `round(0/180×19)=0` → `(0, 0)`
/// - `(ra=90, dec=45)` → col `floor(0.25×78)=floor(19.5)=19`, row
///   `round(0.25×19)=round(4.75)=5` → `(19, 5)`
/// - `(ra=180, dec=0)` → col `floor(0.5×78)=39`, row `round(0.5×19)=round(9.5)=10`
///   → `(39, 10)`
pub fn sky_position(star: &SkyStar, w: u16, h: u16) -> (u16, u16) {
    let w = f64::from(w.max(1));
    let h = f64::from(h.max(1));
    let ra = star.right_ascension.rem_euclid(360.0);
    let col = (ra / 360.0 * w).floor().clamp(0.0, w - 1.0);
    let dec = star.declination.clamp(-90.0, 90.0);
    let row = (((90.0 - dec) / 180.0) * (h - 1.0))
        .round()
        .clamp(0.0, h - 1.0);
    (col as u16, row as u16)
}

/// The brightness value splitting `stars` into "bright" and "dim" halves,
/// for the glyph choice in [`SkyView::render`].
///
/// A fixed absolute threshold would be meaningless here: a neighbor's
/// apparent brightness spans several orders of magnitude across a single
/// sky (a nearby red dwarf and a distant blue giant can both be "notable"),
/// so the median is the scale-invariant split — half the sky's stars read
/// as `*`, half as `·`, whatever the sky's own brightness range happens to
/// be. Sorted with `total_cmp` (no NaN/inf reaches a committed brightness
/// fact, but float `Ord` still needs a total order to sort by at all —
/// project convention, not a defensive check).
fn median_brightness(stars: &[SkyStar]) -> f64 {
    let mut values: Vec<f64> = stars.iter().map(|s| s.brightness).collect();
    values.sort_by(f64::total_cmp);
    let mid = values.len() / 2;
    if values.len().is_multiple_of(2) && mid > 0 {
        (values[mid - 1] + values[mid]) / 2.0
    } else {
        values[mid]
    }
}

/// The night sky, drawn from the committed neighbor stars alone.
///
/// No memo: this view needs no expensive derived structure across renders
/// (see [`View::render`]'s own doc on why the trait takes `&mut self`
/// anyway), so `render` never mutates `self`.
#[derive(Debug, Default, Clone, Copy)]
pub struct SkyView;

impl View for SkyView {
    fn name(&self) -> &'static str {
        "sky"
    }

    /// Always speaks: astronomy is the shallowest rung the ladder has, so a
    /// `world` handed to this view already carries every neighbor fact it
    /// will ever carry.
    fn can_speak(&self, _rung: BuildDepth) -> bool {
        true
    }

    fn render(
        &mut self,
        world: &World,
        _rung: BuildDepth,
        _artifacts: RungArtifacts<'_>,
        w: u16,
        h: u16,
    ) -> Grid {
        let w = w.max(1);
        let h = h.max(1);
        let mut grid = Grid::new(w, h);
        let stars = stars_of(world);
        if stars.is_empty() {
            return grid;
        }
        let median = median_brightness(&stars);
        for star in &stars {
            let (col, row) = sky_position(star, w, h);
            let glyph = if star.brightness >= median { '*' } else { '·' };
            grid.set(
                col,
                row,
                Cell::glyph(glyph, Weight::Normal, Source::Overture),
            );
        }
        grid
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_astronomy::SkyPins;
    use hornvale_kernel::Seed;
    use hornvale_terrain::TerrainPins;
    use hornvale_worldgen::{SettlementPins, SkyChoice, WorldComponents, build_world_to};
    use std::sync::OnceLock;

    /// A real seed-42 world at `depth`, built once per test binary — the same
    /// reasoning `overture::mod`'s own `world_at` gives: these tests want a
    /// genuine committed ledger (a synthetic one could not exercise the real
    /// fact shapes), and this crate's tests are compiled unoptimized.
    fn world_at(depth: BuildDepth) -> &'static World {
        static ASTRONOMY: OnceLock<World> = OnceLock::new();
        static FULL: OnceLock<World> = OnceLock::new();
        let build = || {
            let wc = WorldComponents::assemble().expect("canonical registries are well-formed");
            build_world_to(
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
            BuildDepth::Astronomy => ASTRONOMY.get_or_init(build),
            BuildDepth::Full => FULL.get_or_init(build),
            other => panic!("no cached world for {other:?}; add one deliberately"),
        }
    }

    fn astronomy_only_world() -> &'static World {
        world_at(BuildDepth::Astronomy)
    }

    fn full_world() -> &'static World {
        world_at(BuildDepth::Full)
    }

    #[test]
    fn the_sky_is_complete_at_the_very_first_rung() {
        // The property that earns this view its place in the ladder:
        // astronomy finishes before anything else exists, so `sky` must
        // render fully at `BuildDepth::Astronomy` and not improve at later
        // rungs.
        let early = SkyView.render(
            astronomy_only_world(),
            BuildDepth::Astronomy,
            RungArtifacts::none(),
            78,
            20,
        );
        let late = SkyView.render(
            full_world(),
            BuildDepth::Full,
            RungArtifacts::none(),
            78,
            20,
        );
        assert_eq!(
            early.to_plain_text(),
            late.to_plain_text(),
            "the sky changed after astronomy; it must be complete at rung 0"
        );
        // NON-VACUITY: two blank grids would also be equal. The glyph set
        // asserted on here is exactly the one `render` draws above.
        assert!(
            early.to_plain_text().chars().any(|c| c == '*' || c == '·'),
            "the sky drew no stars at all"
        );
    }

    /// Hand-computed expectations, independent of [`sky_position`] — see the
    /// task report for the arithmetic. `sky_position` is NEVER called here;
    /// a lookup miss panics rather than silently returning a wrong tuple, so
    /// this cannot degrade into `f(x) == f(x)` by a future edit adding a
    /// case that happens to call through.
    fn expected_from(ra: f64, dec: f64, w: u16, h: u16) -> (u16, u16) {
        // Not a `match` on float literals: matching floats by pattern trips
        // rustc's `illegal_floating_point_literal_pattern` lint (denied
        // under this crate's `-D warnings` gate), so this is a plain
        // equality chain instead — still an exact, independent lookup table,
        // never a call through `sky_position`.
        if (w, h) != (78, 20) {
            panic!("no hand-computed expectation for w={w}, h={h}");
        }
        if ra == 0.0 && dec == 90.0 {
            (0, 0)
        } else if ra == 90.0 && dec == 45.0 {
            (19, 5)
        } else if ra == 180.0 && dec == 0.0 {
            (39, 10)
        } else if ra == 270.0 && dec == -45.0 {
            (58, 14)
        } else if ra == 0.0 && dec == -90.0 {
            (0, 19)
        } else {
            panic!("no hand-computed expectation for ra={ra}, dec={dec}")
        }
    }

    #[test]
    fn a_star_lands_where_its_own_coordinates_put_it() {
        // Placement must come from the star's OWN right ascension and
        // declination, not from an index or an arbitrary scatter — the same
        // by-construction discipline The Quadrat's perception overlay used.
        // Covers both corners and the centre of the (ra, dec) domain, not a
        // single point: a single point cannot distinguish a correct
        // projection from one with a sign flip or a swapped axis.
        let cases = [
            (0.0, 90.0),  // north pole, west edge
            (90.0, 45.0), // the brief's own worked example
            (180.0, 0.0), // the equator, opposite side (centre column)
            (270.0, -45.0),
            (0.0, -90.0), // south pole, west edge
        ];
        for (ra, dec) in cases {
            let star = SkyStar {
                right_ascension: ra,
                declination: dec,
                brightness: 1.0,
            };
            let got = sky_position(&star, 78, 20);
            let want = expected_from(ra, dec, 78, 20);
            assert_eq!(got, want, "ra={ra} dec={dec}");
        }
    }

    #[test]
    fn a_neighbor_missing_a_coordinate_fact_is_skipped_not_guessed() {
        // `stars_of` must not panic or fabricate a position for a fact set
        // it cannot fully read — exercised directly since a hand-built
        // world is the only way to construct that shape (`genesis` always
        // commits all three facts together).
        use hornvale_kernel::{Fact, Lineage, WorldTime};
        let wc = WorldComponents::assemble().expect("canonical registries are well-formed");
        let mut world = build_world_to(
            Seed(42),
            &SkyPins::default(),
            SkyChoice::Generated,
            &TerrainPins::default(),
            &SettlementPins::default(),
            &wc,
            BuildDepth::Astronomy,
        )
        .expect("seed 42 builds");
        let before = stars_of(&world).len();
        let id = world.ledger.mint_entity(Lineage {
            parent: None,
            role: "neighbor",
            ordinal: 9999,
        });
        world
            .ledger
            .commit(
                Fact {
                    subject: id,
                    predicate: IS_NEIGHBOR.to_string(),
                    object: Value::Flag(true),
                    place: None,
                    day: Some(WorldTime::GENESIS),
                    provenance: "test".to_string(),
                },
                &world.registry,
            )
            .expect("a flag fact commits cleanly");
        // Declination and right ascension are deliberately never committed
        // for `id`.
        let after = stars_of(&world).len();
        assert_eq!(
            after, before,
            "a neighbor with no coordinates must not be drawn"
        );
    }
}

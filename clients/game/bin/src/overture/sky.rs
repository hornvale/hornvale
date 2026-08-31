//! The sky view (Task 4) — the first view the frame ever shows.
//!
//! **Why this view goes first**: [`hornvale_worldgen::BuildDepth::Astronomy`]
//! is the shallowest rung the ladder has, and everything a night sky needs —
//! every neighbor star's class, distance, brightness, and its own celestial
//! coordinates, plus the system-level pole-star/wanderer/figure facts the
//! caption band reads — is committed by the time that rung fires. So `sky`
//! opens *complete*, where every later view (the atlas, the almanac, the
//! tongue) opens empty until its own rung lands. [`SkyView::render`] must
//! therefore be a pure function of those committed facts alone: identical
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
//!    — calls `terrain_of`/`climate_from` internally (`windows/worldgen/src/
//!    lib.rs:10126-10127`, confirmed on review). Those two functions are
//!    banned from this crate's own call sites (decision 0092), and calling
//!    them from inside a rung-0 render would also cost ~199 ms + ~69 ms
//!    *every* render for data this view never uses (no view here reads
//!    terrain or climate).
//! 2. The narrower `hornvale_worldgen::night_sky_lines(world)` needs no
//!    terrain/climate, but its heliacal-event line resolves its observing
//!    latitude from `hornvale_terrain::places(world).first()`
//!    (`lib.rs:9656-9661`, confirmed on review) — `None` (falling back to a
//!    fixed 35° reference) before any settlement is placed, and a real
//!    committed latitude once one is. That means its OUTPUT would change
//!    between the astronomy-only rung and a later one, which is exactly the
//!    property this view must not have. That is a real reason, not a
//!    preference, so the ledger route is what is implemented below; nothing
//!    here depends on `hornvale-almanac`, and no manifest change was needed.
//!
//! # The caption band (ruling R8, fix round 1)
//!
//! The star field alone left a title-vs-delivery gap against spec §4's named
//! content for this view (neighbours, moons, wanderers, the ecliptic, the
//! eclipse ladder). [`caption_lines`] closes the part of that gap reachable
//! without a new dependency or a banned call: it draws exactly the WORLD-
//! level astronomy facts already committed at rung 0 that the almanac route
//! could not have safely supplied anyway —
//! [`hornvale_astronomy::facts::POLE_STAR_NORTH`]/`POLE_STAR_SOUTH`,
//! [`hornvale_astronomy::facts::WANDERER_COUNT_FACT`], and
//! [`hornvale_astronomy::facts::FIGURE_COUNT`]/`FIGURE_ON_ECLIPTIC`. Each
//! line is drawn only when ITS OWN fact is committed — contract rule 2, never
//! a placeholder for a fact that does not exist — so the band holds anywhere
//! from zero to four lines depending on what this world's genesis actually
//! found.
//!
//! Two things named in spec §4 are deliberately still absent, and both were
//! checked rather than assumed away:
//!
//! - **Moons and wanderers drawn at a position.** They move, so placing one
//!   means picking a fixed reference instant — the choice
//!   `hornvale_worldgen::night_sky_lines` makes with its own
//!   `StdInstant::new(0.0)`. The plan never decided that choice for this
//!   view, and inventing a reference-time model inside a `render` call is
//!   the wrong place to decide it. A *count* caption
//!   (`WANDERER_COUNT_FACT`) is in scope; a *position* is not.
//! - **The eclipse-ladder caption.** `domains/astronomy/src/facts.rs` commits
//!   NO eclipse predicate anywhere — grepped for `ECLIPSE`/`eclipse` across
//!   that file and found only a doc reference to
//!   `crate::eclipses::node_regression_period` (a moon-node fact, not an
//!   eclipse event) and the `crate::eclipses` module name in a comment. The
//!   only producer of eclipse prose is
//!   `hornvale_worldgen::night_sky_lines`'s own `eclipse_events(...)` call
//!   (`lib.rs:~9724`), which recomputes dated events from the live
//!   `GeneratedSky` at a chosen instant rather than reading a committed
//!   fact — the same reference-time problem as moon/wanderer positions, on
//!   top of the almanac route already being closed. **Verified absence, not
//!   an oversight**: there is no rung-0 eclipse fact to draw.
//!
//! The ecliptic itself, by contrast, DOES have a rung-0 fact:
//! [`hornvale_astronomy::facts::FIGURE_ON_ECLIPTIC`] is committed exactly
//! when genesis found at least one star figure standing on the ecliptic, so
//! that one line is drawn (see its own doc for why it can only ever say
//! "at least one", never a count or which figure).
//!
//! # The projection
//!
//! [`sky_position`] maps a star's own (right ascension, declination) onto a
//! `w`×`h` grid: right ascension (0..360°, periodic) onto the column, west
//! to east; declination (−90..90°) onto the row, north pole at the top row
//! and south pole at the bottom — an ordinary equirectangular star-chart
//! layout. See its own doc for the exact formula and worked examples. The
//! star field is drawn BELOW the caption band (see [`SkyView::render`]):
//! `sky_position` itself knows nothing about captions and is tested against
//! a plain `h`-row area.

use crate::overture::view::View;
use hornvale_astronomy::facts::{
    FIGURE_COUNT, FIGURE_ON_ECLIPTIC, IS_NEIGHBOR, NEIGHBOR_BRIGHTNESS_REL,
    NEIGHBOR_DECLINATION_DEG, NEIGHBOR_RA_DEG, POLE_STAR_NORTH, POLE_STAR_SOUTH, STAR_CLASS,
    WANDERER_COUNT_FACT,
};
use hornvale_game_core::{Cell, Grid, Source, Weight};
use hornvale_kernel::{EntityId, Value, World};
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
/// **Two stars can project onto the same cell** — the sky is continuous and
/// the grid is not, so two sufficiently close (ra, dec) pairs round to the
/// same (col, row). [`SkyView::render`] draws stars in ledger commit order,
/// so the LAST-drawn one wins that cell; deterministic (ledger order is
/// fixed for a given world) but otherwise unremarkable — no test depends on
/// which of two colliding stars is the one left on screen.
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
///
/// # Panics
///
/// If `stars` is empty (`values[mid]` indexes an empty slice). The only
/// caller, [`SkyView::render`], upholds this by returning before the star
/// field is drawn at all when `stars_of` comes back empty — this function
/// does not re-check it, so a `debug_assert` stands in as the documented
/// contract for any future caller.
fn median_brightness(stars: &[SkyStar]) -> f64 {
    debug_assert!(
        !stars.is_empty(),
        "median_brightness requires at least one star; the caller must check first"
    );
    let mut values: Vec<f64> = stars.iter().map(|s| s.brightness).collect();
    values.sort_by(f64::total_cmp);
    let mid = values.len() / 2;
    if values.len().is_multiple_of(2) && mid > 0 {
        (values[mid - 1] + values[mid]) / 2.0
    } else {
        values[mid]
    }
}

/// Locate the world entity: the unique subject carrying a [`STAR_CLASS`]
/// fact. The same idiom `windows/explain` uses to read system-level
/// astronomy facts off the ledger (`windows/explain/src/lib.rs`'s own
/// `world_entity`): the neighbor-level `NEIGHBOR_*` facts [`stars_of`] reads
/// live on their own per-neighbor subjects, but the caption band's four
/// facts are committed on the WORLD entity instead (see
/// `domains/astronomy/src/facts.rs::genesis`'s own doc), so the star field
/// and the caption band read off two different kinds of subject from the
/// same ledger.
fn world_entity(world: &World) -> Option<EntityId> {
    world.ledger.find(STAR_CLASS).map(|f| f.subject).next()
}

/// The caption band's fact-to-line rules, given the WORLD entity directly —
/// split out from [`world_entity`]'s resolution so each rule can be tested
/// against a `subject` this module fully controls, independent of whatever
/// the real generated system already committed for its own world entity
/// (see the test module).
///
/// Each line is drawn only when its OWN fact is committed on `subject` —
/// never a placeholder for one that is not (contract rule 2) — so this can
/// return anywhere from zero to four lines. Order is fixed: pole star,
/// wanderer count, figure count, ecliptic.
fn captions_for(world: &World, subject: EntityId) -> Vec<String> {
    let mut lines = Vec::new();

    if let Some(sep) = as_number(world.ledger.value_of(subject, POLE_STAR_NORTH)) {
        lines.push(format!(
            "A star stands {sep:.1}\u{b0} from the north celestial pole."
        ));
    } else if let Some(sep) = as_number(world.ledger.value_of(subject, POLE_STAR_SOUTH)) {
        lines.push(format!(
            "A star stands {sep:.1}\u{b0} from the south celestial pole."
        ));
    }

    if let Some(count) = as_number(world.ledger.value_of(subject, WANDERER_COUNT_FACT)) {
        let n = count.round() as u64;
        if n > 0 {
            let noun = if n == 1 { "wanderer" } else { "wanderers" };
            lines.push(format!("{n} {noun} cross this sky."));
        }
    }

    if let Some(count) = as_number(world.ledger.value_of(subject, FIGURE_COUNT)) {
        let n = count.round() as u64;
        if n > 0 {
            let noun = if n == 1 { "figure" } else { "figures" };
            lines.push(format!("The sky holds {n} {noun}."));
        }
    }

    if world.ledger.value_of(subject, FIGURE_ON_ECLIPTIC).is_some() {
        lines.push("At least one figure stands on the sun's road.".to_string());
    }

    lines
}

/// The caption band for `world`: [`captions_for`] the resolved
/// [`world_entity`], or no lines at all for a world that (somehow) never
/// committed a star class — never a placeholder line in its place.
fn caption_lines(world: &World) -> Vec<String> {
    match world_entity(world) {
        Some(subject) => captions_for(world, subject),
        None => Vec::new(),
    }
}

/// Write `line` starting at column 0 of `row`, clipping at the grid's right
/// edge. `overture::mod`'s own `write_text` does the same thing but is
/// private to that module, so this is a second, minimal copy scoped to what
/// the caption band needs: one row, column zero, no color.
fn write_caption(grid: &mut Grid, row: u16, line: &str) {
    for (i, ch) in line.chars().enumerate() {
        let Ok(col) = u16::try_from(i) else { break };
        if col >= grid.width() {
            break;
        }
        grid.set(col, row, Cell::glyph(ch, Weight::Normal, Source::Overture));
    }
}

/// The night sky: a caption band of committed world-level facts (see the
/// module doc's "The caption band" section) over a star field of committed
/// neighbor facts.
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
    /// `world` handed to this view already carries every fact this view
    /// will ever read.
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

        let captions = caption_lines(world);
        let caption_rows = u16::try_from(captions.len()).unwrap_or(h).min(h);
        for (i, line) in captions.iter().enumerate() {
            let Ok(row) = u16::try_from(i) else { continue };
            if row >= caption_rows {
                continue;
            }
            write_caption(&mut grid, row, line);
        }

        // The star field gets whatever rows the caption band left — never
        // the same rows, so a star cannot overwrite caption text (or vice
        // versa). `sky_position` itself knows nothing about this offset: it
        // is asked about an `h − caption_rows`-row area and the result is
        // shifted down by `caption_rows` here.
        let star_rows = h - caption_rows;
        if star_rows == 0 {
            return grid;
        }
        let stars = stars_of(world);
        if stars.is_empty() {
            return grid;
        }
        let median = median_brightness(&stars);
        for star in &stars {
            let (col, row_in_band) = sky_position(star, w, star_rows);
            let row = row_in_band + caption_rows;
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
    use hornvale_kernel::{Fact, Lineage, Seed, WorldTime};
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

    /// Commit a single hand-authored fact for a test, panicking on any
    /// commit failure (a test's own setup must never silently no-op).
    fn commit_fact(world: &mut World, subject: EntityId, predicate: &str, object: Value) {
        world
            .ledger
            .commit(
                Fact {
                    subject,
                    predicate: predicate.to_string(),
                    object,
                    place: None,
                    day: Some(WorldTime::GENESIS),
                    provenance: "test".to_string(),
                },
                &world.registry,
            )
            .expect("a hand-committed test fact must commit cleanly");
    }

    /// A fresh entity in `world`, unrelated to the real world entity or any
    /// real neighbor — so a test can commit exactly the facts it wants on a
    /// subject [`captions_for`] is handed directly, without colliding with
    /// (or being confused for) whatever the real generated system already
    /// committed.
    fn fresh_subject(world: &mut World, ordinal: u16) -> EntityId {
        world.ledger.mint_entity(Lineage {
            parent: None,
            role: "test-caption-subject",
            ordinal,
        })
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
        let early_text = early.to_plain_text();
        assert_eq!(
            early_text,
            late.to_plain_text(),
            "the sky changed after astronomy; it must be complete at rung 0"
        );
        // NON-VACUITY: two blank grids would also be equal. The glyph set
        // asserted on here is exactly the one `render` draws above.
        assert!(
            early_text.chars().any(|c| c == '*' || c == '·'),
            "the sky drew no stars at all"
        );
        // NON-VACUITY, the caption band's own half: `early_text` is the
        // WHOLE composed grid (not a star-field-only sub-region), so this
        // also proves the equality check above is comparing the captions,
        // not merely sitting alongside them. Seed 42's astronomy commits no
        // pole star (verified: `POLE_STAR_NORTH`/`POLE_STAR_SOUTH` are both
        // absent for this seed), 2 wanderers, 2 figures, and at least one
        // figure on the ecliptic — pinned here the same way
        // `the_fact_count_comes_off_the_observed_world_not_a_guess` in
        // `overture::mod` pins a real seed-42 ledger value rather than a
        // synthetic one.
        assert!(
            early_text.contains("2 wanderers cross this sky."),
            "wanderer caption missing or wrong: {early_text:?}"
        );
        assert!(
            early_text.contains("The sky holds 2 figures."),
            "figure-count caption missing or wrong: {early_text:?}"
        );
        assert!(
            early_text.contains("At least one figure stands on the sun's road."),
            "ecliptic caption missing: {early_text:?}"
        );
        assert!(
            !early_text.contains("celestial pole"),
            "seed 42 commits no pole star; a pole-star caption would be fabricated: {early_text:?}"
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
        let mut world = astronomy_only_world().clone();
        let before = stars_of(&world).len();
        let id = world.ledger.mint_entity(Lineage {
            parent: None,
            role: "neighbor",
            ordinal: 9999,
        });
        commit_fact(&mut world, id, IS_NEIGHBOR, Value::Flag(true));
        // Declination and right ascension are deliberately never committed
        // for `id`.
        let after = stars_of(&world).len();
        assert_eq!(
            after, before,
            "a neighbor with no coordinates must not be drawn"
        );
    }

    #[test]
    fn every_caption_is_drawn_when_its_own_fact_is_committed() {
        let mut world = astronomy_only_world().clone();
        let subject = fresh_subject(&mut world, 0);
        commit_fact(&mut world, subject, POLE_STAR_NORTH, Value::Number(3.5));
        commit_fact(&mut world, subject, WANDERER_COUNT_FACT, Value::Number(4.0));
        commit_fact(&mut world, subject, FIGURE_COUNT, Value::Number(2.0));
        commit_fact(&mut world, subject, FIGURE_ON_ECLIPTIC, Value::Flag(true));

        let lines = captions_for(&world, subject);
        assert_eq!(
            lines,
            vec![
                "A star stands 3.5\u{b0} from the north celestial pole.".to_string(),
                "4 wanderers cross this sky.".to_string(),
                "The sky holds 2 figures.".to_string(),
                "At least one figure stands on the sun's road.".to_string(),
            ]
        );
    }

    #[test]
    fn a_subject_with_no_captioned_facts_gets_no_caption_band() {
        let mut world = astronomy_only_world().clone();
        let subject = fresh_subject(&mut world, 0);
        assert_eq!(
            captions_for(&world, subject),
            Vec::<String>::new(),
            "an entity with none of the four facts must caption nothing"
        );
    }

    #[test]
    fn the_north_pole_star_is_preferred_when_both_are_somehow_committed() {
        // Genesis itself only ever commits one (`facts.rs`'s own doc: "north
        // or south, never both"), but the reader must still resolve a
        // definite order rather than depend on ledger iteration order.
        let mut world = astronomy_only_world().clone();
        let subject = fresh_subject(&mut world, 0);
        commit_fact(&mut world, subject, POLE_STAR_NORTH, Value::Number(1.0));
        commit_fact(&mut world, subject, POLE_STAR_SOUTH, Value::Number(2.0));
        let lines = captions_for(&world, subject);
        assert_eq!(
            lines,
            vec!["A star stands 1.0\u{b0} from the north celestial pole.".to_string()]
        );
    }

    #[test]
    fn zero_counts_are_not_captioned_as_a_placeholder() {
        // A committed zero is a real, existing fact — but contract rule 2
        // is about never drawing a placeholder for what an observer would
        // read as "nothing here", and an explicit "0 wanderers cross this
        // sky" line reads that way. Suppressed, same as the almanac's own
        // "never rendered for a sky with no figures at all" convention for
        // the figure-count line.
        let mut world = astronomy_only_world().clone();
        let subject = fresh_subject(&mut world, 0);
        commit_fact(&mut world, subject, WANDERER_COUNT_FACT, Value::Number(0.0));
        commit_fact(&mut world, subject, FIGURE_COUNT, Value::Number(0.0));
        assert_eq!(captions_for(&world, subject), Vec::<String>::new());
    }

    #[test]
    fn median_brightness_splits_a_tie_at_the_tied_value() {
        // An even count with the two middle values equal: the average of
        // the middle pair is that shared value, not a stray in-between one.
        let stars: Vec<SkyStar> = [1.0, 5.0, 5.0, 9.0]
            .into_iter()
            .map(|brightness| SkyStar {
                right_ascension: 0.0,
                declination: 0.0,
                brightness,
            })
            .collect();
        assert_eq!(median_brightness(&stars), 5.0);
    }

    #[test]
    fn median_brightness_is_not_dragged_by_one_dominant_outlier() {
        // The whole reason this is a median and not a mean (module doc):
        // one blue-giant-bright outlier must not pull the split away from
        // where the other stars actually cluster.
        let stars: Vec<SkyStar> = [1.0, 1.1, 1.2, 1_000_000.0]
            .into_iter()
            .map(|brightness| SkyStar {
                right_ascension: 0.0,
                declination: 0.0,
                brightness,
            })
            .collect();
        let median = median_brightness(&stars);
        assert!(
            (1.0..=1.2).contains(&median),
            "the outlier dragged the split away from the cluster: {median}"
        );
    }
}

//! The world plate's TILE CACHE (The Quadrat, Task 5) — the thing Tasks
//! 1-4 exist to make possible.
//!
//! **What it caches, and why it can.** [`crate::plate::draw_terrain_layer`]
//! is a pure function of `(frame, rung, window origin, size,
//! colour_allowed)` and the world's own fixed terrain — Task 4's layer
//! split is exactly the statement that it reads no `Discovered`, no
//! settlement roster and no cave roster. So a fixed-size block of the
//! VIRTUAL chart at a given rung has one right answer for the whole
//! session, and this module stores it. The feature layer is composed over
//! the assembled terrain per redraw, unchanged and uncached, exactly as
//! [`crate::driver`] already does it.
//!
//! **The unit is a chart tile, never a plate.** The plate cache this
//! replaces was keyed on the drawn plate's own `(w, h)`, so scrolling one
//! column threw away a whole plate and redrew it: measured before this
//! campaign, a full 104x52 plate is 130 ms and one column is 1.208 ms —
//! about 108x cheaper — and none of that 108x was reachable while the
//! cache unit was the plate. A tile is [`TILE_EDGE`] chart cells square,
//! addressed in the chart's own coordinates, so a scroll re-renders the
//! tile column it uncovers and nothing else, and a RESIZE re-renders
//! nothing at all when the new plate is a subrect of the old.
//!
//! **THE KEY CARRIES THE FRAME'S BITS, NOT A QUANTIZED FORM.**
//! [`crate::mercator::Frame`] is two `f64` and derives only `PartialEq`, so
//! it cannot key a `BTreeMap` as it stands. Rounding it would let two
//! genuinely different frames collide onto one entry, and the cache would
//! then serve a tile drawn under the WRONG PROJECTION — a silently wrong
//! picture. `f64::to_bits` cannot: two frames that are bit-identical render
//! identically, and two that differ by an ULP simply get two entries. The
//! asymmetry is the whole argument — over-missing costs time, over-hitting
//! is a wrong answer — and it is why
//! [`hornvale_kernel::quantize`](hornvale_kernel::quantize) is deliberately
//! NOT reached for here: that function is the emit boundary (decision
//! 0033), and a cache key is not an emitted value.
//!
//! **Eviction is MAP-70's two-radius hot/warm policy** (The Excursion's
//! own, established for the Orrery's flat Map rung and reused rather than
//! reinvented). See [`TileCache::evict`].

use hornvale_game_core::Grid;
use hornvale_kernel::{Geosphere, NearestVertexIndex, RoomMeshMemo};
use hornvale_terrain::GeneratedTerrain;
use std::collections::BTreeMap;

use crate::mercator::Frame;
use crate::plate::{self, Window};

/// One cached tile's edge, in VIRTUAL-CHART cells (never screen cells).
///
/// A power of two so a chart coordinate splits into a tile index and an
/// in-tile offset by division alone, and small enough that a one-column
/// scroll re-renders a thin strip: at `32`, a 200x200 plate covers 7x7
/// tiles, so a scroll costs about one seventh of a cold draw rather than
/// all of it.
/// type-audit: bare-ok(count)
pub const TILE_EDGE: u32 = 32;

/// The HOT radius, in tiles: no tile this close to the current window is
/// ever evicted, at any cache pressure. `1` is the neighbour ring — the
/// ground one keystroke of scroll can uncover.
/// type-audit: bare-ok(count)
const HOT_RADIUS: u32 = 1;

/// The WARM radius, in tiles: kept until the cache is over [`CAPACITY`],
/// dropped then. This is the halo a player pacing back and forth across a
/// few screens re-enters, and it is the half MAP-70 gives up first.
/// type-audit: bare-ok(count)
const WARM_RADIUS: u32 = 3;

/// How many tiles the cache holds before [`TileCache::evict`] falls back
/// from [`WARM_RADIUS`] to [`HOT_RADIUS`]. At [`TILE_EDGE`] `= 32` a tile
/// is 1,024 [`hornvale_game_core::Cell`]s, so this is a few megabytes.
///
/// **It was 512 and 512 WAS UNREACHABLE, which is the worse of the two
/// mistakes available here.** Rule 3 of [`TileCache::evict`] only ever runs
/// past this number, so a bound no shipped path can reach is a branch that
/// never executes wearing a safety mechanism's clothes — and nothing in the
/// suite could tell the difference, because the only way to observe the
/// rule is to trip it. A 200x200 plate holds about 56 tiles per rung and
/// the shipped ladder is seven rungs, so the whole ladder is ~392 tiles:
/// under 512, forever. 320 is chosen so that **six rungs of a 200x200 plate
/// trip it** (the test below does exactly that) while still comfortably
/// holding one rung's full warm halo — at most 169 tiles for a 200x200
/// plate, 247 for the 392x196 plate a large terminal draws.
/// type-audit: bare-ok(count)
const CAPACITY: usize = 320;

/// One tile's identity: the projection it was drawn under, the rung, and
/// which tile of that rung's chart it is.
///
/// **`w`/`h` are deliberately absent**, and their absence is the whole
/// gain over the `PlateKey` this replaces: a tile has one fixed size that
/// follows from its own rung and position, so the drawn plate's size is
/// not an input to it. A resize re-renders only the tiles the new plate
/// reaches that the old one did not.
///
/// **`Discovered` is absent for the reason Task 4 exists**
/// (`CLIENT-tiles-need-the-overlay-split`): a key carrying the discovery
/// version is invalidated by every discovery — the whole pyramid, for one
/// settlement.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
struct TileKey {
    /// `Frame::pole_lat_deg.to_bits()` — see the module doc for why this is
    /// the bit pattern and not a rounded value.
    /// type-audit: bare-ok(opaque bits)
    pole_lat_bits: u64,
    /// `Frame::pole_lon_deg.to_bits()`. See [`Self::pole_lat_bits`].
    /// type-audit: bare-ok(opaque bits)
    pole_lon_bits: u64,
    /// The mesh rung the virtual chart is drawn at — `Window::depth`.
    /// type-audit: bare-ok(count)
    depth: u32,
    /// The tile's row in the chart's own tile grid.
    /// type-audit: bare-ok(count)
    tile_row: u32,
    /// The tile's column in the chart's own tile grid.
    /// type-audit: bare-ok(count)
    tile_col: u32,
}

/// A frame's two `f64` as their exact bit patterns — the module doc's
/// ruling, in one place so no call site can quietly round instead.
fn frame_bits(f: &Frame) -> (u64, u64) {
    (f.pole_lat_deg.to_bits(), f.pole_lon_deg.to_bits())
}

/// How many tiles across and down the virtual chart at `depth` is —
/// [`plate::virtual_dims`] divided by [`TILE_EDGE`], rounding UP, so the
/// last tile of each axis is a PARTIAL one covering whatever the chart has
/// left.
///
/// **The partial tile is not a rounding convenience, it is the polar
/// fabrication refusal** (spec §6). A full-height last row would address
/// chart rows at or past `virtual_h`, and
/// [`crate::mercator::unproject`] maps those to latitudes outside
/// `LAT_CLAMP_DEG` — ground the projection has no right to draw. Truncating
/// the tile means the cache can never ask for one.
pub fn tile_grid_dims(depth: u32) -> (u32, u32) {
    let (vw, vh) = plate::virtual_dims(depth);
    (vw.div_ceil(TILE_EDGE), vh.div_ceil(TILE_EDGE))
}

/// The chart tiles of one rung, drawn once and reused until evicted.
///
/// Owns a [`RoomMeshMemo`] for its whole lifetime, which the per-plate memo
/// [`plate::draw_with`] builds could not be: the memo caches a pure function
/// of `(Facet, Geosphere::level())`, so its lifetime is a performance choice
/// and never a correctness one, and a session-long one carries the
/// grid-level corner lookups ACROSS tiles and across rungs. It is bounded by
/// the mesh itself (a level-6 globe has 81,920 facets) and so is never
/// evicted.
#[derive(Debug, Default)]
pub struct TileCache {
    /// The drawn tiles. `BTreeMap`, not a `HashMap`: the root
    /// `clippy.toml`'s `disallowed-types` ban reaches `clients/` even
    /// though this tree is outside the cargo workspace.
    tiles: BTreeMap<TileKey, Grid>,
    /// The grid-level corner-weight memo, shared by every tile this cache
    /// ever draws. See the struct doc.
    memo: RoomMeshMemo,
    /// How many tile requests, ever, were served from [`Self::tiles`].
    /// type-audit: bare-ok(count)
    hits: u64,
    /// How many tile requests, ever, actually drew a tile.
    /// type-audit: bare-ok(count)
    misses: u64,
}

impl TileCache {
    /// How many tile requests, ever, were served without drawing.
    ///
    /// **Observable because a cache with no observable hit is
    /// indistinguishable from a cache that never hits** — every correctness
    /// test passes either way, so the counters are part of the design
    /// rather than instrumentation bolted on.
    /// type-audit: bare-ok(count)
    pub fn hits(&self) -> u64 {
        self.hits
    }

    /// How many tile requests, ever, drew a tile. See [`Self::hits`].
    /// type-audit: bare-ok(count)
    pub fn misses(&self) -> u64 {
        self.misses
    }

    /// How many tiles are resident right now — the quantity
    /// [`Self::evict`] bounds.
    /// type-audit: bare-ok(count)
    pub fn len(&self) -> usize {
        self.tiles.len()
    }

    /// Whether no tile is resident.
    /// type-audit: bare-ok(flag)
    pub fn is_empty(&self) -> bool {
        self.tiles.is_empty()
    }

    /// ONE chart tile's terrain layer, drawn on first ask and reused after.
    ///
    /// `tile_col`/`tile_row` are indices into [`tile_grid_dims`]`(depth)`'s
    /// own grid, in the CHART's coordinates — never the screen's. An index
    /// past that grid names no ground, and yields an empty
    /// (zero-dimensioned) grid rather than fabricating one: `origin_row`
    /// past `virtual_h` would inverse-project outside the projection's own
    /// clamp (spec §6, and [`tile_grid_dims`]'s own doc).
    ///
    /// `index` must be built over the SAME `geo`, exactly as
    /// [`plate::draw_terrain_layer`] requires.
    #[allow(clippy::too_many_arguments)] // mirrors `plate::draw_terrain_layer`'s own allow, one level up
    pub fn terrain(
        &mut self,
        terrain: &GeneratedTerrain,
        geo: &Geosphere,
        index: &NearestVertexIndex,
        f: &Frame,
        depth: u32,
        tile_col: u32,
        tile_row: u32,
        colour_allowed: bool,
    ) -> &Grid {
        let (pole_lat_bits, pole_lon_bits) = frame_bits(f);
        let key = TileKey {
            pole_lat_bits,
            pole_lon_bits,
            depth,
            tile_row,
            tile_col,
        };
        if self.tiles.contains_key(&key) {
            self.hits += 1;
        } else {
            self.misses += 1;
            let (vw, vh) = plate::virtual_dims(depth);
            let origin_col = tile_col.saturating_mul(TILE_EDGE);
            let origin_row = tile_row.saturating_mul(TILE_EDGE);
            // The partial last tile, and the empty out-of-range one, fall
            // out of the same `saturating_sub`.
            let w = vw.saturating_sub(origin_col).min(TILE_EDGE) as u16;
            let h = vh.saturating_sub(origin_row).min(TILE_EDGE) as u16;
            let win = Window {
                depth,
                origin_col,
                origin_row,
            };
            let drawn = plate::draw_terrain_layer(
                terrain,
                geo,
                index,
                &mut self.memo,
                f,
                &win,
                w,
                h,
                colour_allowed,
            );
            self.tiles.insert(key, drawn);
        }
        self.tiles
            .get(&key)
            .expect("the miss branch above always inserts")
    }

    /// The terrain layer for a whole `w`x`h` plate at `win`, assembled from
    /// tiles — the drop-in replacement for one
    /// [`plate::draw_terrain_layer`] call.
    ///
    /// **Byte-identical to that call for any window the driver can hold**,
    /// which is what `the_cached_redraw_path_draws_what_an_uncached_world_
    /// plate_would` asserts end to end. The one place the two arithmetics
    /// could part company is the seam: this path reduces every chart column
    /// modulo `virtual_w` (a tile index must be canonical, or the same
    /// ground would be cached twice under two names), while
    /// [`plate::terrain_at_tile`] adds `origin_col + col` unreduced. Those
    /// are the same longitude — `unproject`'s `lon` is exactly periodic in
    /// `virtual_w` columns, so the two differ by exactly 360°, and
    /// `from_frame`'s trigonometry is periodic in it. `composing_across_the_
    /// seam_matches_the_uncached_draw` pins that agreement rather than
    /// assuming it.
    ///
    /// Rows are NOT reduced, because latitude does not wrap: a row past
    /// `virtual_h` is left as unmarked paper rather than fabricated. The
    /// driver's own `reclamp_window` keeps `origin_row + h` inside the
    /// chart, so no shipped path reaches that branch.
    ///
    /// Evicts on the way out, so a caller that only ever composes still
    /// gets a bounded cache — see [`Self::evict`].
    #[allow(clippy::too_many_arguments)] // mirrors `plate::draw_terrain_layer`'s own allow
    pub fn compose(
        &mut self,
        terrain: &GeneratedTerrain,
        geo: &Geosphere,
        index: &NearestVertexIndex,
        f: &Frame,
        win: &Window,
        w: u16,
        h: u16,
        colour_allowed: bool,
    ) -> Grid {
        let mut out = Grid::new(w, h);
        let (vw, vh) = plate::virtual_dims(win.depth);
        if vw == 0 || vh == 0 {
            return out;
        }
        let base_col = win.origin_col % vw;
        let width = u32::from(w);
        let height = u32::from(h);

        let mut row = 0u32;
        while row < height {
            let chart_row = win.origin_row + row;
            if chart_row >= vh {
                break; // past the projection's own bound: unmarked paper
            }
            let tile_row = chart_row / TILE_EDGE;
            let y0 = chart_row % TILE_EDGE;
            // The tile's own height minus `y0`, which is also `vh -
            // chart_row` whenever the last tile row is a partial one.
            let band_h = (TILE_EDGE - y0).min(vh - chart_row).min(height - row);

            let mut col = 0u32;
            while col < width {
                let chart_col = (base_col + col) % vw;
                // **The reduction above is load-bearing twice over, and the
                // second time is not obvious**: without it `chart_col` can
                // reach exactly `vw`, `band_w` becomes 0, and this loop
                // never advances. Found by mutation — dropping the `% vw`
                // did not fail the suite, it HUNG it, which is a far worse
                // failure than a red assertion. The invariant is asserted
                // rather than left implicit; it costs nothing in release.
                debug_assert!(chart_col < vw, "a chart column escaped the wrap");
                let tile_col = chart_col / TILE_EDGE;
                let x0 = chart_col % TILE_EDGE;
                let band_w = (TILE_EDGE - x0).min(vw - chart_col).min(width - col);
                debug_assert!(band_w > 0, "a zero-width band would never advance");

                let tile = self.terrain(
                    terrain,
                    geo,
                    index,
                    f,
                    win.depth,
                    tile_col,
                    tile_row,
                    colour_allowed,
                );
                for dy in 0..band_h {
                    for dx in 0..band_w {
                        if let Some(cell) = tile.get((x0 + dx) as u16, (y0 + dy) as u16) {
                            out.set((col + dx) as u16, (row + dy) as u16, *cell);
                        }
                    }
                }
                col += band_w;
            }
            row += band_h;
        }

        self.evict(f, win, w, h);
        out
    }

    /// MAP-70's TWO-RADIUS HOT/WARM POLICY (The Excursion), applied to the
    /// chart's tiles rather than the Orrery's ring.
    ///
    /// Three rules, in the order they bite:
    ///
    /// 1. **A tile of another FRAME goes immediately.** The projection has
    ///    moved; `centre_on` produces a fresh pair of `f64` from the
    ///    cursor's own position, so the odds of returning to a previous
    ///    frame's exact bit pattern are the odds of re-centring on the
    ///    identical chart cell. Keeping them would grow the cache without
    ///    bound for a hit that is not coming.
    /// 2. **A tile of the current frame's current rung, further than
    ///    [`WARM_RADIUS`] from the window, goes** — the far field a scroll
    ///    leaves behind. Other RUNGS are untouched here, so a zoom out and
    ///    back in is free while there is room for it.
    /// 3. **Over [`CAPACITY`], everything outside [`HOT_RADIUS`] of the
    ///    current window goes**, at every rung. This is the halo MAP-70
    ///    gives up first; the hot ring is what it never gives up.
    ///
    /// Distance is Chebyshev in tiles from the window's own tile
    /// rectangle — **wrapped in the column direction and not in the row
    /// direction**, the same asymmetry `move_cursor`'s scroll obeys, so a
    /// window straddling the seam does not evict the tiles just behind it.
    pub fn evict(&mut self, f: &Frame, win: &Window, w: u16, h: u16) {
        let (pole_lat_bits, pole_lon_bits) = frame_bits(f);
        let (vw, vh) = plate::virtual_dims(win.depth);
        if vw == 0 || vh == 0 {
            return;
        }
        let (across, _down) = tile_grid_dims(win.depth);

        // The window's own tile rectangle. Columns are a span of length
        // `col_span` starting at `col0` and wrapping; rows are a plain
        // inclusive range.
        let base_col = win.origin_col % vw;
        let col0 = base_col / TILE_EDGE;
        let last_col = (base_col + u32::from(w).saturating_sub(1)) % vw;
        let col_span = if u32::from(w) >= vw {
            across
        } else {
            ((last_col / TILE_EDGE) + across - col0) % across + 1
        };
        let row0 = win.origin_row.min(vh - 1) / TILE_EDGE;
        let row1 = (win.origin_row + u32::from(h).saturating_sub(1)).min(vh - 1) / TILE_EDGE;

        let distance = |tile_col: u32, tile_row: u32| -> u32 {
            let forward = (tile_col + across - col0) % across;
            let dc = if forward < col_span {
                0
            } else {
                (forward - col_span + 1).min(across - forward)
            };
            let dr = if tile_row < row0 {
                row0 - tile_row
            } else {
                tile_row.saturating_sub(row1)
            };
            dc.max(dr)
        };

        self.tiles.retain(|k, _| {
            if k.pole_lat_bits != pole_lat_bits || k.pole_lon_bits != pole_lon_bits {
                return false; // rule 1
            }
            if k.depth != win.depth {
                return true; // rule 2 says nothing about another rung
            }
            distance(k.tile_col, k.tile_row) <= WARM_RADIUS
        });

        if self.tiles.len() > CAPACITY {
            self.tiles.retain(|k, _| {
                k.depth == win.depth && distance(k.tile_col, k.tile_row) <= HOT_RADIUS
            });
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::mercator;
    use crate::plate::GLOBE_RUNG;
    use hornvale_kernel::Seed;
    use hornvale_terrain::TerrainPins;
    use std::collections::BTreeSet;

    /// The three world-shaped arguments every entry point here takes,
    /// bundled so a test reads as the question it is asking. Built exactly
    /// the way `plate.rs`'s own `test_world` builds one — `hornvale_terrain::
    /// generate` directly, never a full `build_world`, since nothing here
    /// reads the ledger.
    struct Fixture {
        terrain: GeneratedTerrain,
        geo: Geosphere,
        index: NearestVertexIndex,
    }

    fn fixture() -> Fixture {
        let geo = Geosphere::new(hornvale_terrain::GLOBE_LEVEL);
        let outcome = hornvale_terrain::generate(Seed(42), &geo, &TerrainPins::default())
            .expect("default pins generate seed 42");
        let terrain = GeneratedTerrain::new(geo.clone(), outcome);
        let index = NearestVertexIndex::new(&geo);
        Fixture {
            terrain,
            geo,
            index,
        }
    }

    /// One plate's terrain layer THROUGH the cache — the brief's own
    /// `render_through`, written against the real API rather than the API
    /// invented to match the sketch.
    fn render_through(
        cache: &mut TileCache,
        world: &Fixture,
        f: &Frame,
        win: &Window,
        w: u16,
        h: u16,
    ) -> Grid {
        cache.compose(
            &world.terrain,
            &world.geo,
            &world.index,
            f,
            win,
            w,
            h,
            false,
        )
    }

    /// The same plate WITHOUT the cache — `plate::draw_terrain_layer` with a
    /// memo of its own, which is what every byte-identity assertion here
    /// compares against.
    fn render_direct(world: &Fixture, f: &Frame, win: &Window, w: u16, h: u16) -> Grid {
        let mut memo = RoomMeshMemo::default();
        plate::draw_terrain_layer(
            &world.terrain,
            &world.geo,
            &world.index,
            &mut memo,
            f,
            win,
            w,
            h,
            false,
        )
    }

    /// How many DISTINCT pictures the tiles covering `win` hold.
    ///
    /// **The non-vacuity instrument for every hit/miss assertion below.**
    /// This campaign has now caught five tests whose input space collapsed
    /// to one value; a window over uniform ocean is exactly that failure for
    /// a tile cache — the counts would read correctly while proving nothing
    /// about WHICH tile was served, because every tile is the same tile.
    fn distinct_tiles(
        cache: &mut TileCache,
        world: &Fixture,
        f: &Frame,
        win: &Window,
        w: u16,
        h: u16,
    ) -> usize {
        let (vw, vh) = plate::virtual_dims(win.depth);
        let base_col = win.origin_col % vw;
        let mut seen: BTreeSet<String> = BTreeSet::new();
        let mut row = 0u32;
        while row < u32::from(h) && win.origin_row + row < vh {
            let chart_row = win.origin_row + row;
            let tile_row = chart_row / TILE_EDGE;
            let mut col = 0u32;
            while col < u32::from(w) {
                let chart_col = (base_col + col) % vw;
                let tile_col = chart_col / TILE_EDGE;
                let text = cache
                    .terrain(
                        &world.terrain,
                        &world.geo,
                        &world.index,
                        f,
                        win.depth,
                        tile_col,
                        tile_row,
                        false,
                    )
                    .to_plain_text();
                seen.insert(text);
                col += TILE_EDGE - (chart_col % TILE_EDGE);
            }
            row += TILE_EDGE - (chart_row % TILE_EDGE);
        }
        seen.len()
    }

    /// A window whose RIGHT EDGE lands exactly on a tile boundary at
    /// [`GLOBE_RUNG`], parked on the equator band where the terrain is most
    /// varied — so scrolling it by ONE column uncovers exactly one new tile
    /// column and nothing else, which is the fact the headline test is
    /// about.
    fn edge_aligned_window(w: u16, h: u16) -> Window {
        let (_vw, vh) = plate::virtual_dims(GLOBE_RUNG);
        // `origin_col + w - 1 == TILE_EDGE - 1 (mod TILE_EDGE)`.
        let origin_col = (TILE_EDGE - (u32::from(w) % TILE_EDGE)) % TILE_EDGE;
        Window {
            depth: GLOBE_RUNG,
            origin_col,
            origin_row: (vh / 2).saturating_sub(u32::from(h) / 2),
        }
    }

    // -- Step 1's two tests, adapted to the real API ------------------

    /// **THE HEADLINE.** Measured before this campaign: a full 104x52 plate
    /// is 130 ms and ONE column is 1.208 ms — about 108x cheaper — and none
    /// of that was reachable while the cache unit was the whole plate.
    /// Asserted with hit/miss counts rather than timings, because a timing
    /// assertion is a flake on a contended box.
    ///
    /// **Three assertions, and the second and third are what make the first
    /// mean anything.** The counts alone would pass over a uniform ocean
    /// window (nothing distinguishes a reused tile from a wrong one there),
    /// and they would pass on a cache that served the WRONG tile fast. So:
    /// the tiles are proved distinguishable first, and the composed picture
    /// is proved byte-identical to an uncached draw last.
    #[test]
    fn scrolling_one_column_reuses_every_tile_but_the_new_edge() {
        let world = fixture();
        let f = mercator::frame_for(false);
        let (w, h) = (200u16, 200u16);
        let mut win = edge_aligned_window(w, h);

        let mut cache = TileCache::default();
        let before = render_through(&mut cache, &world, &f, &win, w, h);
        let first = cache.misses();
        assert!(
            first > 0,
            "the first draw hit an empty cache and should have missed"
        );
        assert_eq!(cache.hits(), 0, "an empty cache cannot hit");

        // NON-VACUITY, before any count is believed.
        let distinct = distinct_tiles(&mut cache, &world, &f, &win, w, h);
        assert!(
            distinct > 1,
            "every tile in this window holds the same picture ({distinct} distinct), so a \
             hit/miss count cannot tell a reused tile from a wrong one"
        );
        assert!(
            before.to_plain_text().contains('~') && before.to_plain_text().contains('.'),
            "the window must show both ocean and land, or it proves nothing"
        );

        let asks_before = cache.hits() + cache.misses();
        win.origin_col += 1;
        let after = render_through(&mut cache, &world, &f, &win, w, h);
        let scroll = cache.misses() - first;
        let asks = cache.hits() + cache.misses() - asks_before;

        assert!(
            scroll * 4 < first,
            "a one-column scroll cost {scroll} misses against {first} for a full draw"
        );
        // The window's right edge was on a tile boundary, so one column of
        // scroll uncovers exactly one column of tiles — `asks - scroll` of
        // the tiles it asked for were REUSED, which is the claim in the name.
        assert!(
            asks - scroll >= first,
            "a one-column scroll reused only {} of the {first} tiles it already had",
            asks - scroll
        );

        // AND THE PICTURE IS RIGHT. A fast cache serving the wrong tile
        // would satisfy every count above.
        assert_eq!(
            after.to_plain_text(),
            render_direct(&world, &f, &win, w, h).to_plain_text(),
            "the scrolled plate is not what an uncached draw would have painted"
        );
    }

    /// The key carries the rung. Two rungs are two pictures of the same
    /// ground, and serving one for the other is the failure mode a
    /// `(frame, tile)` key without the depth would have.
    ///
    /// **THE TWO WINDOWS ARE AT THE IDENTICAL ORIGIN ON PURPOSE, AND AN
    /// EARLIER DRAFT OF THIS TEST WAS VACUOUS FOR WANT OF THAT.** It put the
    /// coarse window at column 96 and the fine one at 192 — the same GROUND,
    /// since the chart doubles — which are tiles 3 and 6: two different
    /// keys whatever the depth field does. A mutation setting `depth: 0` in
    /// [`TileKey`] left that draft GREEN, measured, because the two rungs
    /// never asked for the same tile index in the first place. Same origin,
    /// same tile index, different rung is the only arrangement in which the
    /// depth field is the thing under test.
    #[test]
    fn a_rung_change_does_not_serve_stale_tiles() {
        let world = fixture();
        let f = mercator::frame_for(false);
        let (w, h) = (64u16, 32u16);
        // Identical origin at both rungs => identical tile indices.
        // Chosen by probe, not by hope: this is one of the origins at which
        // BOTH rungs show land AND ocean, which the guard below re-checks.
        let origin_col = 24;
        let origin_row = 140;
        let coarse = Window {
            depth: GLOBE_RUNG,
            origin_col,
            origin_row,
        };
        let fine = Window {
            depth: GLOBE_RUNG + 1,
            origin_col,
            origin_row,
        };
        for win in [&coarse, &fine] {
            let (_vw, vh) = plate::virtual_dims(win.depth);
            assert!(
                origin_row + u32::from(h) < vh,
                "rung {} cannot show this window without fabricating a pole",
                win.depth
            );
        }

        let mut cache = TileCache::default();
        let a = render_through(&mut cache, &world, &f, &coarse, w, h);
        let drew_coarse = cache.misses();
        let b = render_through(&mut cache, &world, &f, &fine, w, h);
        let drew_fine = cache.misses() - drew_coarse;

        // THE DIRECT PIN: the finer rung shared not one tile with the
        // coarser one, even though it asked for the same tile indices.
        assert_eq!(
            drew_fine,
            drew_coarse,
            "the finer rung was served {} of the coarser rung's tiles",
            drew_coarse - drew_fine
        );

        // Non-vacuity: a pair of blank or uniform plates would "differ"
        // for reasons that have nothing to do with the key.
        for (name, g) in [("coarse", &a), ("fine", &b)] {
            let text = g.to_plain_text();
            assert!(
                text.contains('~') && text.contains('.'),
                "the {name} plate is uniform, so this comparison proves nothing"
            );
        }
        assert_ne!(
            a.to_plain_text(),
            b.to_plain_text(),
            "two rungs served the same tiles"
        );
        assert_eq!(
            b.to_plain_text(),
            render_direct(&world, &f, &fine, w, h).to_plain_text(),
            "the finer rung's plate is not what an uncached draw would have painted"
        );
    }

    // -- The composition is byte-identical to the uncached path -------

    /// The whole cache is only worth having if it draws the same picture.
    /// Compared at three rungs, because the memo's reuse — and so the code
    /// path through `terrain_at_tile` — is rung-conditional.
    #[test]
    fn composing_matches_the_uncached_draw_at_every_shipped_rung() {
        let world = fixture();
        let f = mercator::frame_for(false);
        let (w, h) = (96u16, 48u16);
        for depth in [GLOBE_RUNG, GLOBE_RUNG + 2, plate::BAND_B_RUNG] {
            let (vw, vh) = plate::virtual_dims(depth);
            let win = Window {
                depth,
                origin_col: vw / 3,
                origin_row: (vh / 2).saturating_sub(u32::from(h) / 2),
            };
            let mut cache = TileCache::default();
            let composed = render_through(&mut cache, &world, &f, &win, w, h);
            let direct = render_direct(&world, &f, &win, w, h);
            for y in 0..direct.height() {
                for x in 0..direct.width() {
                    assert_eq!(
                        composed.get(x, y),
                        direct.get(x, y),
                        "rung {depth} disagrees at ({x}, {y})"
                    );
                }
            }
        }
    }

    /// **THE SEAM.** `compose` reduces every chart column modulo
    /// `virtual_w` — it must, or the same ground would cache twice under two
    /// names — while `plate::terrain_at_tile` adds `origin_col + col`
    /// unreduced. Those are the same longitude 360° apart and the
    /// projection is periodic in it, but that is an argument, and this is
    /// the measurement of it: a window deliberately straddling the seam,
    /// compared cell by cell.
    #[test]
    fn composing_across_the_seam_matches_the_uncached_draw() {
        let world = fixture();
        let f = mercator::frame_for(false);
        let (w, h) = (96u16, 48u16);
        let (vw, vh) = plate::virtual_dims(GLOBE_RUNG);
        let win = Window {
            depth: GLOBE_RUNG,
            origin_col: vw - u32::from(w) / 2, // half the plate is past the wrap
            origin_row: (vh / 2).saturating_sub(u32::from(h) / 2),
        };
        assert!(
            win.origin_col + u32::from(w) > vw,
            "this window does not actually straddle the seam"
        );
        let mut cache = TileCache::default();
        let composed = render_through(&mut cache, &world, &f, &win, w, h);
        let direct = render_direct(&world, &f, &win, w, h);
        assert!(
            direct.to_plain_text().contains('~') && direct.to_plain_text().contains('.'),
            "a uniform plate would hide a seam disagreement"
        );
        for y in 0..direct.height() {
            for x in 0..direct.width() {
                assert_eq!(
                    composed.get(x, y),
                    direct.get(x, y),
                    "the seam disagrees at ({x}, {y})"
                );
            }
        }
    }

    // -- The key's fields, one at a time ------------------------------

    /// **The controller's ruling, as an assertion.** Two frames one ULP
    /// apart are two entries, never one. A quantized key would collide them
    /// and serve a tile drawn under the wrong projection; a bit key can only
    /// over-miss.
    #[test]
    fn two_frames_one_ulp_apart_are_two_entries() {
        let world = fixture();
        let a = mercator::frame_for(false);
        let b = Frame {
            pole_lat_deg: f64::from_bits(a.pole_lat_deg.to_bits() + 1),
            pole_lon_deg: a.pole_lon_deg,
        };
        assert_ne!(
            a.pole_lat_deg, b.pole_lat_deg,
            "the fixture must actually hold two different frames"
        );
        // Any rounding to fewer than ~17 significant digits maps these two
        // onto the same number — which is precisely the collision refused.
        assert_eq!(
            format!("{:.8e}", a.pole_lat_deg),
            format!("{:.8e}", b.pole_lat_deg),
            "these frames must be indistinguishable to a quantized key, or this \
             test does not exercise the ruling"
        );

        let mut cache = TileCache::default();
        let _ = cache.terrain(
            &world.terrain,
            &world.geo,
            &world.index,
            &a,
            GLOBE_RUNG,
            3,
            3,
            false,
        );
        let _ = cache.terrain(
            &world.terrain,
            &world.geo,
            &world.index,
            &b,
            GLOBE_RUNG,
            3,
            3,
            false,
        );
        assert_eq!(cache.misses(), 2, "the two frames shared an entry");
        assert_eq!(cache.hits(), 0);
        assert_eq!(cache.len(), 2);
    }

    /// The frame is in the key at all: a genuinely different projection
    /// draws a genuinely different plate through the cache.
    #[test]
    fn a_recentred_frame_is_not_served_the_old_projections_tiles() {
        let world = fixture();
        let (w, h) = (64u16, 32u16);
        let win = edge_aligned_window(w, h);
        let a = mercator::frame_for(false);
        let b = mercator::centre_on(30.0, 40.0);
        let mut cache = TileCache::default();
        let first = render_through(&mut cache, &world, &a, &win, w, h);
        let second = render_through(&mut cache, &world, &b, &win, w, h);
        assert_ne!(
            first.to_plain_text(),
            second.to_plain_text(),
            "a re-centred projection was served the old frame's tiles"
        );
        assert_eq!(
            second.to_plain_text(),
            render_direct(&world, &b, &win, w, h).to_plain_text(),
            "the re-centred plate is not what an uncached draw would have painted"
        );
    }

    // -- The size is NOT in the key, and that is the point ------------

    /// **The gain the `PlateKey` this replaces could not have.** A resize to
    /// a strict subrect renders nothing at all, because the drawn plate's
    /// size is not an input to a tile.
    #[test]
    fn a_resize_to_a_subrect_renders_no_new_tiles() {
        let world = fixture();
        let f = mercator::frame_for(false);
        let win = edge_aligned_window(200, 200);
        let mut cache = TileCache::default();
        let _ = render_through(&mut cache, &world, &f, &win, 200, 200);
        let after_first = cache.misses();
        assert!(after_first > 0);
        let _ = render_through(&mut cache, &world, &f, &win, 96, 48);
        assert_eq!(
            cache.misses(),
            after_first,
            "shrinking the plate re-rendered tiles it already held"
        );
    }

    // -- Polar fabrication, and the partial tile that refuses it ------

    /// No tile ever addresses a chart row at or past `virtual_h`, because
    /// the last tile row is a PARTIAL one (spec §6). Asserted on the tile's
    /// own drawn height, at every shipped rung.
    #[test]
    fn the_last_tile_row_is_truncated_to_the_charts_own_height() {
        let world = fixture();
        let f = mercator::frame_for(false);
        for depth in [GLOBE_RUNG, GLOBE_RUNG + 1, plate::BAND_B_RUNG] {
            let (_vw, vh) = plate::virtual_dims(depth);
            let (_across, down) = tile_grid_dims(depth);
            let expected = vh - (down - 1) * TILE_EDGE;
            assert!(
                expected <= TILE_EDGE,
                "rung {depth}: the tile grid does not cover the chart"
            );
            let mut cache = TileCache::default();
            let tile = cache.terrain(
                &world.terrain,
                &world.geo,
                &world.index,
                &f,
                depth,
                0,
                down - 1,
                false,
            );
            assert_eq!(
                u32::from(tile.height()),
                expected,
                "rung {depth}'s last tile row would have drawn past the clamp"
            );
        }
    }

    /// A tile index off the chart names no ground and fabricates none.
    #[test]
    fn a_tile_past_the_chart_is_empty_rather_than_fabricated() {
        let world = fixture();
        let f = mercator::frame_for(false);
        let (_across, down) = tile_grid_dims(GLOBE_RUNG);
        let mut cache = TileCache::default();
        let tile = cache.terrain(
            &world.terrain,
            &world.geo,
            &world.index,
            &f,
            GLOBE_RUNG,
            0,
            down + 4,
            false,
        );
        assert_eq!(tile.height(), 0, "a tile below the chart drew ground");
    }

    // -- MAP-70's two-radius eviction ---------------------------------

    /// Rule 1: the projection moved, so the old frame's tiles go. Observed
    /// through the consequence — coming back re-renders them — rather than
    /// by reaching into the map.
    #[test]
    fn a_frame_change_evicts_the_old_projections_tiles() {
        let world = fixture();
        let (w, h) = (64u16, 32u16);
        let win = edge_aligned_window(w, h);
        let a = mercator::frame_for(false);
        let b = mercator::centre_on(30.0, 40.0);
        let mut cache = TileCache::default();
        let _ = render_through(&mut cache, &world, &a, &win, w, h);
        let cold = cache.misses();
        let _ = render_through(&mut cache, &world, &b, &win, w, h);
        let before_return = cache.misses();
        let _ = render_through(&mut cache, &world, &a, &win, w, h);
        assert_eq!(
            cache.misses() - before_return,
            cold,
            "the old frame's tiles survived a projection change"
        );
    }

    /// **THE WARM HALO IS A SEPARATE RADIUS FROM THE HOT RING, AND THIS IS
    /// THE TEST THAT SAYS SO.** A mutation shrinking [`WARM_RADIUS`] to
    /// [`HOT_RADIUS`] — collapsing MAP-70's two radii into one — left the
    /// suite GREEN before this existed, measured. Every other eviction
    /// assertion here is satisfied by the one-radius policy, because the
    /// difference only shows for ground you have LEFT and then come back to.
    ///
    /// One window's width of pan puts the tiles just behind you at
    /// Chebyshev distance 2: inside the warm halo, outside the hot ring. So
    /// stepping out and back must cost nothing.
    #[test]
    fn the_warm_halo_survives_a_short_pan_and_back() {
        let world = fixture();
        let f = mercator::frame_for(false);
        let depth = plate::BAND_B_RUNG;
        let (_vw, vh) = plate::virtual_dims(depth);
        let (w, h) = (64u16, 64u16);
        assert_eq!(
            u32::from(w) % TILE_EDGE,
            0,
            "an unaligned window spans a third tile column and moves the distance"
        );
        let home = Window {
            depth,
            origin_col: TILE_EDGE * 100,
            origin_row: (vh / 2 / TILE_EDGE) * TILE_EDGE,
        };
        let away = Window {
            origin_col: home.origin_col + u32::from(w),
            ..home
        };

        let mut cache = TileCache::default();
        let _ = render_through(&mut cache, &world, &f, &home, w, h);
        let after_home = cache.misses();
        let _ = render_through(&mut cache, &world, &f, &away, w, h);
        assert!(
            cache.misses() > after_home,
            "the pan must actually have moved onto new ground, or coming back \
             proves nothing"
        );
        let after_away = cache.misses();
        let _ = render_through(&mut cache, &world, &f, &home, w, h);
        assert_eq!(
            cache.misses(),
            after_away,
            "one window's pan gave up the warm halo — the two radii have collapsed into one"
        );
    }

    /// Rule 3: past [`CAPACITY`] the warm halo goes and the HOT ring stays.
    /// Reached the way a player reaches it — a 200x200 plate carried down
    /// the whole rung ladder, each rung keeping its own halo until the total
    /// will not fit.
    ///
    /// **The trip is detected, not assumed.** Rule 2 can never shrink the
    /// cache across a rung change (it does not touch another rung's tiles)
    /// and a fresh rung only ever ADDS, so a resident count that FALLS from
    /// one rung to the next can only be rule 3 firing. The test refuses to
    /// pass without observing one, which is what stops it from silently
    /// becoming a test of a bound nothing reaches — the exact defect that
    /// set [`CAPACITY`] to 320.
    #[test]
    fn cache_pressure_gives_up_the_warm_halo_and_keeps_the_hot_ring() {
        let world = fixture();
        let f = mercator::frame_for(false);
        let (w, h) = (200u16, 200u16);
        let mut cache = TileCache::default();
        let mut previous = 0usize;
        let mut dropped_at = None;
        let mut coarse_cost = 0u64;
        for depth in GLOBE_RUNG..=plate::BAND_B_RUNG {
            let (vw, vh) = plate::virtual_dims(depth);
            let win = Window {
                depth,
                origin_col: vw / 2,
                origin_row: (vh / 2).saturating_sub(u32::from(h) / 2),
            };
            let before = cache.misses();
            let _ = render_through(&mut cache, &world, &f, &win, w, h);
            if depth == GLOBE_RUNG {
                coarse_cost = cache.misses() - before;
            }
            assert!(
                cache.len() <= CAPACITY,
                "rung {depth} left {} tiles resident, over the {CAPACITY} bound",
                cache.len()
            );
            if cache.len() < previous {
                dropped_at = Some(depth);
            }
            previous = cache.len();

            // THE HOT RING: recomposing where we stand still costs nothing.
            let before = cache.misses();
            let _ = render_through(&mut cache, &world, &f, &win, w, h);
            assert_eq!(
                cache.misses(),
                before,
                "rung {depth}'s own tiles were evicted from under it"
            );
        }
        let dropped_at = dropped_at.expect(
            "the ladder never tripped the capacity bound, so rule 3 was never exercised \
             and this test would pass with rule 3 deleted",
        );
        assert!(dropped_at > GLOBE_RUNG);

        // And what it gave up was the far rung's halo: the coarsest rung is
        // gone and costs its full price again.
        let (vw, vh) = plate::virtual_dims(GLOBE_RUNG);
        let coarse = Window {
            depth: GLOBE_RUNG,
            origin_col: vw / 2,
            origin_row: (vh / 2).saturating_sub(u32::from(h) / 2),
        };
        let before = cache.misses();
        let _ = render_through(&mut cache, &world, &f, &coarse, w, h);
        assert_eq!(
            cache.misses() - before,
            coarse_cost,
            "the coarsest rung survived the pressure that dropped it at rung {dropped_at}"
        );
    }

    /// Rule 2: the far field of the CURRENT rung goes, so a long pan does
    /// not grow the cache without bound — and the window's own tiles never
    /// do (the hot ring).
    #[test]
    fn a_long_pan_evicts_the_far_field_and_keeps_the_hot_ring() {
        let world = fixture();
        let f = mercator::frame_for(false);
        let depth = plate::BAND_B_RUNG;
        let (vw, vh) = plate::virtual_dims(depth);
        let (w, h) = (64u16, 64u16);
        let mut cache = TileCache::default();
        let home = Window {
            depth,
            origin_col: vw / 2,
            origin_row: vh / 2,
        };
        let _ = render_through(&mut cache, &world, &f, &home, w, h);
        let resident = cache.len();
        assert!(resident > 0);

        // A pan far past WARM_RADIUS, one screen at a time.
        let mut win = home;
        let mut peak = cache.len();
        for _ in 0..12 {
            win.origin_col += u32::from(w);
            let _ = render_through(&mut cache, &world, &f, &win, w, h);
            peak = peak.max(cache.len());
            // THE HOT RING: recomposing where we stand costs nothing.
            let before = cache.misses();
            let _ = render_through(&mut cache, &world, &f, &win, w, h);
            assert_eq!(
                cache.misses(),
                before,
                "the window's own tiles were evicted from under it"
            );
        }
        // The bound: the window's tile span plus WARM_RADIUS on each side.
        let span = u32::from(w).div_ceil(TILE_EDGE) + 1;
        let bound = ((span + 2 * WARM_RADIUS) * (span + 2 * WARM_RADIUS)) as usize;
        assert!(
            cache.len() <= bound && peak <= bound,
            "a 12-screen pan left {} tiles resident (peak {peak}), over the {bound} the \
             warm halo allows",
            cache.len()
        );

        // And home really was given up.
        let before = cache.misses();
        let _ = render_through(&mut cache, &world, &f, &home, w, h);
        assert!(
            cache.misses() > before,
            "the far field survived a 12-screen pan"
        );
    }
}

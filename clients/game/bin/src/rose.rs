//! The rose raster: the walk-band picture drawn as the compass rose
//! iterated outward, together with its inverse.
//!
//! Box `(col, row)` is the facet reached by walking the observer's own
//! [`hornvale_locale::heading_rose`] chains — north or south to the row,
//! then east or west along it. The picture is the movement rule itself, so
//! "the box to the left" and "what the left arrow does" are the same object
//! by construction rather than by geometry. That is the whole of The Sett:
//! the reported bug is that a Mercator raster makes them two different
//! things, and on the polar caps they disagree for two thirds of the words
//! (design amendment §A1, ledger S5).
//!
//! **This module owns the transport and nothing else.** It answers "which
//! facet is in this box" and "which box holds this facet". Everything
//! downstream of that — corner weights, the elevation blend, reflectance,
//! the glyph — is unchanged and lives where it already lived.
//!
//! **A refused bearing ends its chain.** `heading_rose` returns `None` for
//! exactly one word at exactly the 24 cube-corner facets; every box past
//! that point in that direction is blank, and nothing fills it — not a
//! repeat, not a seeded draw, not a substitute glyph (ledger decision #4,
//! following The Newel's R12 on The Stipple's biome boundary: a seeded draw
//! is honest per value and dishonest per pattern).
//!
//! Nothing here is committed or serialized. The plate is drawn, never
//! stored, so no save-format contract passes through this file.

use hornvale_kernel::{Facet, FacetId};
use hornvale_locale::heading_rose;
use std::collections::BTreeMap;
use std::collections::btree_map::Entry;

/// Index of the north word in [`hornvale_locale::heading_rose`]'s return,
/// which is `Compass::all()` order: `[N, Ne, E, Se, S, Sw, W, Nw]`.
/// type-audit: bare-ok(index)
const NORTH: usize = 0;
/// Index of the east word in `Compass::all()` order — see [`NORTH`].
/// type-audit: bare-ok(index)
const EAST: usize = 2;
/// Index of the south word in `Compass::all()` order — see [`NORTH`].
/// type-audit: bare-ok(index)
const SOUTH: usize = 4;
/// Index of the west word in `Compass::all()` order — see [`NORTH`].
/// type-audit: bare-ok(index)
const WEST: usize = 6;

/// A memo over [`hornvale_locale::heading_rose`], carried across redraws.
///
/// **It is a cost instrument and never a correctness one.** `heading_rose`
/// is a pure function of its facet, so a memoised answer is the same answer;
/// `the_raster_is_deterministic_and_the_memo_changes_nothing` pins that
/// against a memo in two different states.
///
/// **Why it exists at all:** ledger S7 measured the unmemoised rose chain
/// at 114.16 ms per redraw against Mercator's 0.22 ms, which is not
/// shippable. Memoised it is 11.31 ms cold and **1.04 ms warm**, because a
/// step shifts the picture and leaves the overlap unchanged (ledger S8), so
/// the second and every later redraw hits on almost every box.
///
/// **Keyed on [`FacetId`], not [`Facet`]** — the same argument
/// [`crate::plate::ReflectanceKey`] states and for the same reason: a
/// `Facet` is `{ face: u8, path: Vec<u8> }`, so keying on one costs a heap
/// allocation on every consult, hits included, and makes the `BTreeMap`
/// compare a `Vec<u8>` lexicographically instead of a single integer. The
/// consult count is one per step walked, so it is the plate's box count:
/// **3,328 a redraw** on the 64x52 plate a 104x56 terminal draws (Task 7's
/// bench reports it, and measured 16,738 hits over six builds). That would
/// be 3,328 allocations a frame purely to ask a question. An earlier
/// revision of this line said ~800, unscoped — a real figure for the 40x20
/// plate `rasterize_rivers` names beside its own measurement, but not for
/// the plate this client actually draws.
///
/// [`Facet::pack`] fails only past `MAX_DEPTH` (29) and the walk rung is 13,
/// so the fall-through below is unreachable in practice and is written as a
/// correct answer rather than a panic.
#[derive(Debug, Default)]
pub struct RoseMemo {
    /// The full eight-word rose for each facet consulted so far. The whole
    /// rose is stored, not the one word asked for, because building a row
    /// asks for east and west from the same facets a column asked north and
    /// south of.
    store: BTreeMap<FacetId, Vec<Option<Facet>>>,
    /// How many consults were served from [`Self::store`].
    /// type-audit: bare-ok(count)
    hits: u64,
    /// How many consults called `heading_rose` because the key was absent.
    /// type-audit: bare-ok(count)
    misses: u64,
}

impl RoseMemo {
    /// An empty memo.
    pub fn new() -> Self {
        Self::default()
    }

    /// How many consults were served without calling `heading_rose`.
    /// type-audit: bare-ok(count)
    pub fn hits(&self) -> u64 {
        self.hits
    }

    /// How many consults called `heading_rose`.
    /// type-audit: bare-ok(count)
    pub fn misses(&self) -> u64 {
        self.misses
    }

    /// How many distinct facets the memo currently holds a rose for.
    /// type-audit: bare-ok(count)
    pub fn entries(&self) -> usize {
        self.store.len()
    }

    /// The facet one step from `from` along compass word `word`, or `None`
    /// if that bearing is refused — which happens at a cube corner, for
    /// exactly one word.
    /// type-audit: bare-ok(index: word)
    fn step(&mut self, from: &Facet, word: usize) -> Option<Facet> {
        let Ok(id) = from.pack() else {
            // Past `MAX_DEPTH` there is no key; answer correctly, uncached.
            return heading_rose(from)[word].clone();
        };
        if let Some(rose) = self.store.get(&id) {
            self.hits += 1;
            return rose[word].clone();
        }
        self.misses += 1;
        let rose = heading_rose(from);
        let out = rose[word].clone();
        self.store.insert(id, rose);
        out
    }
}

/// A drawn window of the world, addressed by compass chain, with the
/// `FacetId -> (col, row)` inverse every overlay places through.
///
/// The inverse is half the deliverable, not a convenience (ledger decision
/// #2): point sites, rivers, the perception layer and the observer's own
/// mark are placed today by `mercator::project`, and an overlay left
/// projecting through Mercator onto a graph raster lands in the wrong box
/// silently.
#[derive(Debug, Clone)]
pub struct RoseRaster {
    /// Boxes across.
    /// type-audit: bare-ok(count)
    w: u16,
    /// Boxes down.
    /// type-audit: bare-ok(count)
    h: u16,
    /// The lattice depth every facet in the raster sits at — the anchor's
    /// own `path.len()`, since a rose step never changes depth.
    /// type-audit: bare-ok(count)
    depth: u32,
    /// The anchor's box, `(w / 2, h / 2)`.
    centre: (u16, u16),
    /// Row-major, `w * h` long. `None` is a box no chain reached.
    boxes: Vec<Option<Facet>>,
    /// The inverse. A facet drawn in more than one box — which happens
    /// inside either fold, near a cube corner (ledger S6, S9) or at a pole
    /// (S10) — maps to the box **nearest the centre**, measured in
    /// [`RoseRaster::steps_from_centre`] and tie-broken row-major, so the
    /// map is a right inverse of [`RoseRaster::facet_at`] everywhere and a
    /// two-sided one wherever the picture is duplicate-free.
    ///
    /// **Nearest-the-centre is a rule; first-scanned was not** (ledger
    /// decision #5). At the exact pole the meridian chain reverses — north
    /// steps off the pole and north again returns to it — so the pole is
    /// drawn in every other box of the centre column and a first-row-major
    /// inverse named the top of that column for the one facet the anchor's
    /// own contract puts at the centre. Since the observer's `@` is placed
    /// through this map, a polar observer's mark drew near the top of the
    /// plate. The rule also names, for any other repeated facet, the
    /// instance the observer stands nearest.
    inverse: BTreeMap<FacetId, (u16, u16)>,
}

impl RoseRaster {
    /// Walk the rose chains outward from `anchor`, which sits at box
    /// `(w / 2, h / 2)`.
    ///
    /// The centre column is built first — north repeatedly upward, south
    /// repeatedly downward — and each row is then built outward from that
    /// column's facet, east rightward and west leftward. A refused bearing
    /// ends its chain and every box past it stays blank.
    ///
    /// **The anchor is not centred symmetrically and the caller must not
    /// assume it is:** for an even `w` there is one more box to the left of
    /// the anchor than to its right, and likewise above for an even `h`.
    /// type-audit: bare-ok(count: w), bare-ok(count: h)
    pub fn build(anchor: &Facet, w: u16, h: u16, memo: &mut RoseMemo) -> Self {
        let centre = (w / 2, h / 2);
        let (uw, uh) = (usize::from(w), usize::from(h));
        let mut boxes: Vec<Option<Facet>> = vec![None; uw * uh];

        if w > 0 && h > 0 {
            let (cc, cr) = centre;

            // The centre column, from the anchor outward in both directions.
            let mut column: Vec<Option<Facet>> = vec![None; uh];
            column[usize::from(cr)] = Some(anchor.clone());
            let mut up = Some(anchor.clone());
            for row in (0..cr).rev() {
                up = up.and_then(|f| memo.step(&f, NORTH));
                column[usize::from(row)] = up.clone();
            }
            let mut down = Some(anchor.clone());
            for row in (cr + 1)..h {
                down = down.and_then(|f| memo.step(&f, SOUTH));
                column[usize::from(row)] = down.clone();
            }

            // Each row, outward from its own column facet.
            for row in 0..h {
                let off = usize::from(row) * uw;
                let base = column[usize::from(row)].clone();
                boxes[off + usize::from(cc)] = base.clone();
                let mut east = base.clone();
                for col in (cc + 1)..w {
                    east = east.and_then(|f| memo.step(&f, EAST));
                    boxes[off + usize::from(col)] = east.clone();
                }
                let mut west = base;
                for col in (0..cc).rev() {
                    west = west.and_then(|f| memo.step(&f, WEST));
                    boxes[off + usize::from(col)] = west.clone();
                }
            }
        }

        let mut inverse: BTreeMap<FacetId, (u16, u16)> = BTreeMap::new();
        if w > 0 {
            for (i, drawn) in boxes.iter().enumerate() {
                let Some(f) = drawn else { continue };
                let Ok(id) = f.pack() else { continue };
                let col = (i % uw) as u16;
                let row = (i / uw) as u16;
                // Nearest the centre wins; the scan is row-major, so
                // keeping the incumbent on a tie IS the row-major
                // tie-break.
                match inverse.entry(id) {
                    Entry::Vacant(slot) => {
                        slot.insert((col, row));
                    }
                    Entry::Occupied(mut slot) => {
                        if Self::steps_from_centre(centre, (col, row))
                            < Self::steps_from_centre(centre, *slot.get())
                        {
                            slot.insert((col, row));
                        }
                    }
                }
            }
        }

        Self {
            w,
            h,
            depth: anchor.path.len() as u32,
            centre,
            boxes,
            inverse,
        }
    }

    /// Boxes across.
    /// type-audit: bare-ok(count)
    pub fn width(&self) -> u16 {
        self.w
    }

    /// Boxes down.
    /// type-audit: bare-ok(count)
    pub fn height(&self) -> u16 {
        self.h
    }

    /// The lattice depth every facet in the raster sits at.
    /// type-audit: bare-ok(count)
    pub fn depth(&self) -> u32 {
        self.depth
    }

    /// The anchor's box, `(col, row)` = `(w / 2, h / 2)`.
    pub fn centre(&self) -> (u16, u16) {
        self.centre
    }

    /// The facet drawn in box `(col, row)`, or `None` for a box out of
    /// range or one no chain reached.
    /// type-audit: bare-ok(index: col), bare-ok(index: row)
    pub fn facet_at(&self, col: u16, row: u16) -> Option<&Facet> {
        if col >= self.w || row >= self.h {
            return None;
        }
        self.boxes[usize::from(row) * usize::from(self.w) + usize::from(col)].as_ref()
    }

    /// Rose steps from `centre` to box `at` — `|dcol| + |drow|`, which is
    /// exactly the number of chain steps [`RoseRaster::build`] walked to
    /// reach that box: down or up the centre column to the row, then out
    /// along it. Chebyshev and Euclidean distance are equally defensible
    /// and agree with this one at the pole; the step count is chosen
    /// because it is the raster's own construction metric, so "nearest the
    /// centre" reads as "fewest steps from the anchor" — which is the
    /// distance an observer consulting the map would actually walk.
    /// type-audit: bare-ok(count)
    fn steps_from_centre(centre: (u16, u16), at: (u16, u16)) -> u32 {
        let span = |a: u16, b: u16| u32::from(a.max(b) - a.min(b));
        span(centre.0, at.0) + span(centre.1, at.1)
    }

    /// Where `facet` is drawn, or `None` if this raster does not draw it.
    ///
    /// A facet the picture repeats — only ever inside a fold — answers with
    /// its box nearest the centre; see [`RoseRaster::inverse`].
    pub fn box_of(&self, facet: &Facet) -> Option<(u16, u16)> {
        self.box_of_id(facet.pack().ok()?)
    }

    /// Where the facet with this packed id is drawn — the same question as
    /// [`RoseRaster::box_of`], for a caller already holding a [`FacetId`]
    /// (the perception overlay reads one straight off `scene/surrounds/v2`).
    pub fn box_of_id(&self, id: FacetId) -> Option<(u16, u16)> {
        self.inverse.get(&id).copied()
    }
}

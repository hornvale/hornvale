//! The rose raster's geometry, as committed assertions.
//!
//! **This file exists because The Newel's own probes did not.**
//! `newel_rasters.rs` and `newel_grid.rs` produced the measurements
//! decision #8 rested on and appear in no commit, so this campaign had to
//! re-derive every one of them from scratch (ledger S1-S8). These
//! assertions are the same measurements, kept.

use hornvale_game::rose::{RoseMemo, RoseRaster};
use hornvale_kernel::Facet;
use hornvale_locale::heading_rose;

/// The walk rung, and the plate at the enforced 80x24 terminal floor:
/// `spread::world_plate_width(80, 24) == 40` and
/// `spread::content_height(24) == 20`. **Not 53x27** — that is the plate at
/// 93x31, which The Newel's R14 misattributed to 80x24 (ledger S3).
const DEPTH: u32 = 13;
const W: u16 = 40;
const H: u16 = 20;

/// A facet at face-lattice `(x, y)`, built by descending the quadtree.
fn lattice(face: u8, x: i64, y: i64, depth: u32) -> Facet {
    let mut path = Vec::with_capacity(depth as usize);
    for level in (0..depth).rev() {
        path.push(((((x >> level) & 1) as u8) << 1) | (((y >> level) & 1) as u8));
    }
    Facet { face, path }
}

/// A uniform sweep over all six faces — the population R13 measured —
/// PLUS a targeted roster of facets whose own window crosses a face seam or
/// reaches a cube corner.
///
/// **The targeted half is not a garnish, and leaving it out is the defect
/// this doc comment exists to prevent.** A uniform sweep at any usable
/// `side` puts its nearest sample hundreds of facets from a face edge
/// (`side = 12` at depth 13 puts it at 341, against a plate half-width of
/// 20), so a uniform-only population never crosses a seam at all — and a
/// seam is exactly where a transport could flip handedness and where R14's
/// competing option blanks half the screen. A test that swept only the
/// interior would pass on a raster that fell apart at every face edge.
///
/// The last three entries of the targeted roster are cube corners rather
/// than plain seams, and the two are not interchangeable — see
/// [`near_a_cube_corner`].
fn sweep(side: i64) -> Vec<Facet> {
    let scale: i64 = 1 << DEPTH;
    let mut out = Vec::new();
    for face in 0..6u8 {
        for i in 0..side {
            for j in 0..side {
                out.push(lattice(
                    face,
                    (2 * i + 1) * scale / (2 * side),
                    (2 * j + 1) * scale / (2 * side),
                    DEPTH,
                ));
            }
        }
        // Facets whose 40x20 window runs off the face's own lattice. The
        // first seven cross a plain seam; the last three sit at or beside a
        // cube corner.
        for &(x, y) in &[
            (0, scale / 2),
            (scale - 1, scale / 2),
            (scale / 2, 0),
            (scale / 2, scale - 1),
            (3, scale / 3),
            (scale - 4, 2 * scale / 3),
            (2 * scale / 5, 1),
            (0, 0),
            (scale - 1, scale - 1),
            (2, 2),
        ] {
            out.push(lattice(face, x, y, DEPTH));
        }
    }
    out
}

/// How many of `facets` sit close enough to a face edge that a `W`x`H`
/// window runs off it. The tests assert this is non-zero before they assert
/// anything about seams — see [`sweep`].
fn crossing_a_seam(facets: &[Facet]) -> usize {
    facets
        .iter()
        .filter(|f| {
            let l = f.face_lattice();
            let hw = i64::from(W / 2);
            let hh = i64::from(H / 2);
            l.x < hw || l.x >= l.scale - hw || l.y < hh || l.y >= l.scale - hh
        })
        .count()
}

/// Is a cube corner close enough to sit inside this facet's own `W`x`H`
/// window — within the plate's half-width on **both** lattice axes?
///
/// **THE PLAN ASSERTED TWO PROPERTIES UNIVERSALLY THAT THIS CAMPAIGN HAD
/// ALREADY RATIFIED AS FAILING HERE, and this predicate is the correction.**
/// Ledger decision #4 rules that a refused bearing ends its chain and
/// nothing fills the boxes past it, and ledger S6 measures what that costs
/// at a corner: 39 blanks and 243 repeats of an 861-box plate, decaying to
/// 0/0 twenty steps out. A picture with repeats has no box-for-box inverse,
/// and a picture with blanks that appear and vanish as the observer moves is
/// not the old picture shifted. So `the_inverse_map_...` and
/// `one_step_shifts_...` hold **off** this region and are asserted there;
/// `a_chain_through_a_cube_corner_ends_and_nothing_fills_it` asserts the
/// degradation itself, so the exclusion has a witness rather than being a
/// place the suite simply stops looking.
///
/// Measured before the exclusion was written, over the full `sweep(4)` and
/// `sweep(6)` populations: **every** facet that failed either property was
/// at lattice corner-distance `(0, 0)` or `(2, 2)`, and **no** plain
/// seam-crossing facet failed either one — 0 blanks, 0 repeats and 0
/// shift mismatches at `(0, scale/2)`, `(scale/2, 0)`, `(3, scale/3)`,
/// `(2*scale/5, 1)`, `(scale-4, 2*scale/3)`, `(scale-1, scale/2)` and
/// `(scale/2, scale-1)` on all six faces. The seam is clean; only the
/// corner is not.
///
/// The bound is the plate's own half-width on both axes, so it is derived
/// from the window rather than fitted to the observed failures — it is a
/// superset of them, which is the safe direction for an exclusion to err.
fn near_a_cube_corner(f: &Facet) -> bool {
    let l = f.face_lattice();
    let half = i64::from(W / 2);
    let dx = l.x.min(l.scale - 1 - l.x);
    let dy = l.y.min(l.scale - 1 - l.y);
    dx < half && dy < half
}

/// `sweep(side)` minus the corner neighbourhood, with both halves of the
/// population guarded: the survivors must still cross face seams in force,
/// and exactly the three corner entries per face must have been removed.
/// A future edit that dropped seam samples, or that widened the corner
/// exclusion into a blanket, reddens here rather than quietly narrowing
/// what the two strict properties are asserted over.
fn sweep_off_the_corners(side: i64) -> Vec<Facet> {
    let all = sweep(side);
    let kept: Vec<Facet> = all
        .iter()
        .filter(|f| !near_a_cube_corner(f))
        .cloned()
        .collect();
    assert_eq!(
        all.len() - kept.len(),
        18,
        "the corner exclusion must remove exactly the three corner entries \
         on each of the six faces, and nothing else"
    );
    assert!(
        crossing_a_seam(&kept) >= 40,
        "vacuity guard: only {} of the {} kept facets have a window that \
         crosses a face seam, so this population would pass on a raster that \
         fell apart at every face edge",
        crossing_a_seam(&kept),
        kept.len()
    );
    kept
}

/// THE CAMPAIGN'S WHOLE CLAIM. Nathan's report is about the arrow keys, and
/// the arrows are the four cardinals (decision 0510: four-way primary).
/// Every cardinal must land in its own box, on every face — the polar caps
/// included, which is where the raster The Newel chose reaches 66.7%
/// (ledger S5).
///
/// This one is asserted over the **whole** population, cube corners
/// included. A corner refuses one word and the assertion skips it, but
/// every word the rose does offer lands in its own box even there.
#[test]
fn every_cardinal_lands_in_its_own_box() {
    let mut memo = RoseMemo::new();
    let facets = sweep(12);
    assert!(
        facets.len() > 400,
        "the sweep must be a population, not a spot check"
    );
    assert!(
        crossing_a_seam(&facets) >= 60,
        "vacuity guard: only {} of the sampled facets have a window that crosses \
         a face seam, so this sweep would pass on a raster that fell apart at \
         every face edge",
        crossing_a_seam(&facets)
    );
    let mut checked = 0u64;
    for f in &facets {
        let r = RoseRaster::build(f, W, H, &mut memo);
        let (cc, cr) = r.centre();
        let rose = heading_rose(f);
        for (word, (dcol, drow)) in [
            (0usize, (0i32, -1i32)),
            (2, (1, 0)),
            (4, (0, 1)),
            (6, (-1, 0)),
        ] {
            let Some(want) = rose[word].as_ref() else {
                continue;
            };
            checked += 1;
            let got = r.facet_at((i32::from(cc) + dcol) as u16, (i32::from(cr) + drow) as u16);
            assert_eq!(
                got,
                Some(want),
                "compass word {word} at {f:?} does not draw in its own box"
            );
        }
    }
    assert!(
        checked > 1600,
        "vacuity guard: only {checked} words were compared"
    );
}

/// The picture after a step is the picture before it, shifted. This is what
/// makes the plate read as a map rather than re-deal itself every turn, and
/// it is the property that would license an incremental build (ledger S8).
/// The raster The Newel chose holds this on the equatorial faces and at
/// 66.7% on the caps.
///
/// Off the corner neighbourhood — see [`near_a_cube_corner`] for why, and
/// for the measurement showing no plain seam crossing needs the exemption.
#[test]
fn one_step_shifts_the_picture_and_changes_nothing_else() {
    let mut memo = RoseMemo::new();
    let mut compared = 0u64;
    for f in sweep_off_the_corners(6) {
        let before = RoseRaster::build(&f, W, H, &mut memo);
        let rose = heading_rose(&f);
        // East: the new picture's column j is the old picture's column j+1.
        if let Some(next) = rose[2].as_ref() {
            let after = RoseRaster::build(next, W, H, &mut memo);
            for row in 0..H {
                for col in 1..W {
                    compared += 1;
                    assert_eq!(
                        before.facet_at(col, row),
                        after.facet_at(col - 1, row),
                        "an east step moved more than the window at {f:?}"
                    );
                }
            }
        }
        // North: the new picture's row r+1 is the old picture's row r.
        if let Some(up) = rose[0].as_ref() {
            let after = RoseRaster::build(up, W, H, &mut memo);
            for row in 0..H - 1 {
                for col in 0..W {
                    compared += 1;
                    assert_eq!(
                        before.facet_at(col, row),
                        after.facet_at(col, row + 1),
                        "a north step moved more than the window at {f:?}"
                    );
                }
            }
        }
    }
    assert!(
        compared > 100_000,
        "vacuity guard: only {compared} boxes compared"
    );
}

/// The inverse map answers for every box the raster drew, and for nothing
/// else. Task 4's overlays are placed entirely through it.
///
/// Three assertions, because "agrees with the raster" means three different
/// things and the plan's version only ever checked the middle one:
///
/// 1. **It is a right inverse everywhere, corners included** — whatever box
///    `box_of` names really does hold that facet. This is the property an
///    overlay depends on: place a river at `box_of(f)` and the terrain under
///    it is `f`. It must hold even where the picture repeats.
/// 2. **It is two-sided off the corner neighbourhood** — the box a facet was
///    drawn in is the box `box_of` returns. This cannot hold at a corner,
///    where ledger S6 measures 243 repeated boxes, so it is asserted where
///    the picture is duplicate-free. See [`near_a_cube_corner`].
/// 3. **It answers for nothing else** — a facet the raster does not draw
///    gets `None`. The plan's doc comment claimed this and its body never
///    checked it; an inverse that answered for undrawn facets would place
///    overlays for things off the screen.
#[test]
fn the_inverse_map_agrees_with_the_raster_box_for_box() {
    let mut memo = RoseMemo::new();

    // (1) a right inverse over the WHOLE population, cube corners included.
    let mut right_inverted = 0u64;
    for f in sweep(4) {
        let r = RoseRaster::build(&f, W, H, &mut memo);
        for row in 0..H {
            for col in 0..W {
                let Some(here) = r.facet_at(col, row) else {
                    continue;
                };
                right_inverted += 1;
                let named = r.box_of(here).expect("a drawn facet must have a box");
                assert_eq!(
                    r.facet_at(named.0, named.1),
                    Some(here),
                    "box_of named {named:?}, which does not hold the facet it was asked about"
                );
            }
        }
    }
    assert!(
        right_inverted > 10_000,
        "vacuity guard: {right_inverted} boxes"
    );

    // (2) two-sided where the picture is duplicate-free.
    let mut round_tripped = 0u64;
    for f in sweep_off_the_corners(4) {
        let r = RoseRaster::build(&f, W, H, &mut memo);
        for row in 0..H {
            for col in 0..W {
                let Some(here) = r.facet_at(col, row) else {
                    continue;
                };
                round_tripped += 1;
                assert_eq!(
                    r.box_of(here),
                    Some((col, row)),
                    "the inverse map disagrees with the raster at ({col}, {row})"
                );
            }
        }
    }
    assert!(
        round_tripped > 10_000,
        "vacuity guard: {round_tripped} boxes"
    );

    // (3) and for nothing else: a facet on the far side of the world, and
    // one on another face entirely, are both absent.
    let anchor = lattice(0, 4096, 4096, DEPTH);
    let r = RoseRaster::build(&anchor, W, H, &mut memo);
    for stranger in [
        lattice(0, 6000, 4096, DEPTH),
        lattice(0, 4096, 6000, DEPTH),
        lattice(3, 4096, 4096, DEPTH),
        lattice(5, 100, 200, DEPTH),
    ] {
        assert_eq!(
            r.box_of(&stranger),
            None,
            "the inverse map answered for a facet the raster never drew"
        );
    }
    // Non-vacuity for (3): the same map does answer for what it did draw.
    assert_eq!(r.box_of(&anchor), Some(r.centre()));
}

/// A cube corner refuses one bearing, so a chain through it ends. Nothing
/// fills the boxes past it — not a repeat, not a seeded draw, not a
/// substitute glyph (ledger decision #4, following R12).
///
/// This test also GUARDS ITS OWN NON-VACUITY: it first proves the chosen
/// facet really does produce blanks, so a future change that quietly filled
/// them would fail here rather than pass by never reaching the branch.
///
/// **It is also the witness for [`near_a_cube_corner`]'s exclusion.** The
/// two strict properties above are asserted off the corner neighbourhood;
/// the reason they must be is that the picture there carries both blanks
/// and repeats, and this test asserts both, so the exclusion cites a
/// measured fact rather than an absence of evidence.
#[test]
fn a_chain_through_a_cube_corner_ends_and_nothing_fills_it() {
    let mut memo = RoseMemo::new();
    let corner = lattice(0, 0, 0, DEPTH);
    let r = RoseRaster::build(&corner, W, H, &mut memo);
    let mut blanks = 0u64;
    let mut repeats = 0u64;
    for row in 0..H {
        for col in 0..W {
            match r.facet_at(col, row) {
                None => blanks += 1,
                Some(here) if r.box_of(here) != Some((col, row)) => repeats += 1,
                Some(_) => {}
            }
        }
    }
    assert!(
        blanks > 0,
        "non-vacuity: a raster anchored ON a cube corner must have blanks"
    );
    assert!(
        repeats > 0,
        "the corner exclusion the two strict properties take is justified by \
         repeated boxes here (ledger S6); this raster shows none, so either \
         the corner no longer degrades or this facet is not one"
    );
    // Twenty steps in from the corner, the picture is whole again.
    let inland = lattice(0, 24, 24, DEPTH);
    let clean = RoseRaster::build(&inland, W, H, &mut memo);
    for row in 0..H {
        for col in 0..W {
            assert!(
                clean.facet_at(col, row).is_some(),
                "a facet well inside its face drew a blank at ({col}, {row})"
            );
        }
    }
}

/// Same anchor, same raster — twice, and from two memos in different
/// states. The memo is a performance choice and never a correctness one.
#[test]
fn the_raster_is_deterministic_and_the_memo_changes_nothing() {
    let anchor = lattice(2, 3000, 5000, DEPTH);
    let mut cold = RoseMemo::new();
    let a = RoseRaster::build(&anchor, W, H, &mut cold);
    let mut warm = RoseMemo::new();
    let _ = RoseRaster::build(&lattice(2, 3001, 5000, DEPTH), W, H, &mut warm);
    let b = RoseRaster::build(&anchor, W, H, &mut warm);
    assert!(warm.hits() > 0, "non-vacuity: the warm memo never hit");
    for row in 0..H {
        for col in 0..W {
            assert_eq!(a.facet_at(col, row), b.facet_at(col, row));
        }
    }
}

/// The anchor is at `(w / 2, h / 2)` and the plate is even-sized, so it is
/// **not** centred symmetrically: at 40x20 there are 20 boxes to its left
/// and 19 to its right, 10 above and 9 below. Every consumer that walks
/// outward from the centre depends on this and the plan warns about it
/// twice, so it is pinned rather than left to be rediscovered.
#[test]
fn the_anchor_sits_at_half_the_plate_and_the_halves_are_uneven() {
    let mut memo = RoseMemo::new();
    let anchor = lattice(1, 4000, 4000, DEPTH);
    let r = RoseRaster::build(&anchor, W, H, &mut memo);
    assert_eq!(r.width(), W);
    assert_eq!(r.height(), H);
    assert_eq!(r.depth(), DEPTH);
    assert_eq!(r.centre(), (W / 2, H / 2));
    assert_eq!(r.facet_at(W / 2, H / 2), Some(&anchor));
    let (cc, cr) = r.centre();
    assert_eq!((cc, W - 1 - cc), (20, 19), "left of the anchor, then right");
    assert_eq!((cr, H - 1 - cr), (10, 9), "above the anchor, then below");
    // Out of range is None, never a panic and never a wrap.
    assert_eq!(r.facet_at(W, 0), None);
    assert_eq!(r.facet_at(0, H), None);
}

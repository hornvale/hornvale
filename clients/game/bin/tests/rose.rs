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
/// The roster's last seven entries are not plain seams. Three sit at or
/// beside a **cube corner**, two probe the **polar corner** and the **pole**
/// — the two folds S9 and S10 measured — and two sit deliberately just
/// OUTSIDE each fold, so the boundary is exercised from the clean side as
/// well as the dirty one. See [`inside_a_fold`].
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
        // Facets whose 40x20 window runs off the face's own lattice.
        for &(x, y) in &[
            // Seven plain seam crossings — no corner, no pole.
            (0, scale / 2),
            (scale - 1, scale / 2),
            (scale / 2, 0),
            (scale / 2, scale - 1),
            (3, scale / 3),
            (scale - 4, 2 * scale / 3),
            (2 * scale / 5, 1),
            // Three at or beside a cube corner (S6, S9).
            (0, 0),
            (scale - 1, scale - 1),
            (2, 2),
            // Corner distance 24: INSIDE the polar corner fold (28) and
            // outside the equatorial one (10), so the same lattice position
            // is excluded on a cap and asserted on an equatorial face. This
            // is the entry that would have found the hole S9 names — the old
            // half-width predicate excluded neither.
            (24, 24),
            // Corner distance 30: outside BOTH corner folds, so it is
            // asserted on every face and witnesses the clean side of the
            // wider boundary.
            (30, 30),
            // The face centre. On faces 4 and 5 that is the geographic pole
            // and the raster folds there (S10); on faces 0-3 it is ordinary
            // deep interior and must draw clean.
            (scale / 2, scale / 2),
            // Eighteen facets off the face centre: outside the pole fold
            // (16) on a cap, ordinary interior everywhere else.
            (scale / 2 + 18, scale / 2),
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

/// Where the rose raster folds, and how wide each fold is.
///
/// **THE PLAN ASSERTED TWO PROPERTIES UNIVERSALLY THAT THIS CAMPAIGN HAD
/// ALREADY RATIFIED AS FAILING HERE, and this predicate is the correction.**
/// Ledger decision #4 rules that a refused bearing ends its chain and
/// nothing fills the boxes past it. Two regions carry the consequence, and
/// they are different phenomena at different places:
///
/// - **A cube corner** (S6, S9). The anchor's own chain terminates, so the
///   plate blanks and repeats. Blanks clear ten steps in on every face, but
///   *repeats* persist to 28 on a polar cap against 10 on an equatorial
///   face — a single figure is true of one band and false of the other.
/// - **The pole** (S10). Faces 4 and 5 are centred on the geographic poles,
///   where the meridians the N/S chains follow converge. It is not a cube
///   corner, it *never blanks*, and it repeats out to 16 facets — 0.176
///   degrees. Any raster built on meridian chains folds where the meridians
///   meet; this one degrades rather than refusing, which S11 shows is the
///   gentler half of a comparison `mercator::project` loses ~800x on area.
///
/// **The first version of this predicate was a single plate-half-width, 20,
/// and it was narrower than the polar corner it had to cover.** It did not
/// fail only because the roster happened to contain no facet at corner
/// distance 24 on a cap — one now exists, deliberately, and it is excluded
/// here and asserted on an equatorial face. Nothing about the half-width was
/// ever a measurement; these three constants are.
///
/// **The radii are exactly the measured clearing distances, not a margin
/// above them**, so the exclusion is minimal: a facet at distance
/// `FOLD` is kept and is clean, and a facet at `FOLD - 1` is excluded and is
/// dirty. `the_folds_are_where_the_measurement_says_and_no_wider` holds both
/// halves of that against a fresh walk, two-sided.
/// type-audit: bare-ok(count)
const EQUATORIAL_CORNER_FOLD: i64 = 10;
/// The same radius on a polar cap, where repeats persist far further —
/// see [`EQUATORIAL_CORNER_FOLD`]. Covers both caps: face 5 is the wider of
/// the two at 28, face 4 clears at 27.
/// type-audit: bare-ok(count)
const POLAR_CORNER_FOLD: i64 = 28;
/// The pole fold's radius on a polar cap, in lattice steps from the face
/// centre — see [`EQUATORIAL_CORNER_FOLD`].
/// type-audit: bare-ok(count)
const POLE_FOLD: i64 = 16;
/// How far under a fold radius the measured clearing distance may sit before
/// the extent test calls it a change of kind rather than of sample. The
/// floor arm a `<=` ceiling cannot give: an improvement nobody banks leaves a
/// bound that has stopped meaning anything. Same instrument as
/// `hornvale_locale`'s `ROSE_WORST_FLOOR_DEG`.
/// type-audit: bare-ok(count)
const FOLD_FLOOR_SLACK: i64 = 4;

/// Faces 4 and 5 are the polar caps. Asserted rather than assumed —
/// `the_folds_are_where_the_measurement_says_and_no_wider` checks each
/// face's centre latitude, so a change to the cube's face order reddens
/// there instead of silently re-banding every exclusion below.
fn is_a_polar_face(face: u8) -> bool {
    face >= 4
}

/// Chebyshev distance to the nearest corner of the facet's own face. A
/// corner is near only when **both** axes are near an edge, so the scalar is
/// the larger of the two per-axis distances: `(0, scale/2)` is a plain seam
/// crossing at distance `scale/2`, not a corner.
fn corner_distance(f: &Facet) -> i64 {
    let l = f.face_lattice();
    let dx = l.x.min(l.scale - 1 - l.x);
    let dy = l.y.min(l.scale - 1 - l.y);
    dx.max(dy)
}

/// Chebyshev distance to the face centre — the pole, on a cap. Measured in
/// half-steps (`|2x + 1 - scale| / 2`) because the centre is a lattice
/// *vertex* between facets `scale/2 - 1` and `scale/2`, not a facet: the
/// naive `|x - scale/2|` is asymmetric by one and the fold is not.
fn pole_distance(f: &Facet) -> i64 {
    let l = f.face_lattice();
    let dx = (2 * l.x + 1 - l.scale).abs() / 2;
    let dy = (2 * l.y + 1 - l.scale).abs() / 2;
    dx.max(dy)
}

/// Does this facet's own plate carry a fold — a blank or a repeated box?
/// See [`EQUATORIAL_CORNER_FOLD`] for the two regions and their measured
/// extents.
fn inside_a_fold(f: &Facet) -> bool {
    let polar = is_a_polar_face(f.face);
    let corner = if polar {
        POLAR_CORNER_FOLD
    } else {
        EQUATORIAL_CORNER_FOLD
    };
    corner_distance(f) < corner || (polar && pole_distance(f) < POLE_FOLD)
}

/// `sweep(side)` minus the two folds, with both halves of the population
/// guarded: the survivors must still cross face seams in force, and exactly
/// the expected facets must have been removed — three corner entries on each
/// of the four equatorial faces, and five on each cap (the same three, plus
/// the corner-distance-24 entry the wider polar radius catches, plus the
/// pole).
///
/// **The count is the anti-blanket guard.** Widening any of the three radii
/// to make a failure go away changes it, which is the failure mode a bare
/// "exclude the folds" predicate would hide. It is deliberately stated as a
/// total and a per-band split rather than a single number, so a change that
/// moved one facet from one band to the other cannot cancel out.
fn sweep_off_the_folds(side: i64) -> Vec<Facet> {
    let all = sweep(side);
    let kept: Vec<Facet> = all.iter().filter(|f| !inside_a_fold(f)).cloned().collect();
    let dropped_equatorial = all
        .iter()
        .filter(|f| !is_a_polar_face(f.face) && inside_a_fold(f))
        .count();
    let dropped_polar = all
        .iter()
        .filter(|f| is_a_polar_face(f.face) && inside_a_fold(f))
        .count();
    assert_eq!(
        (dropped_equatorial, dropped_polar, all.len() - kept.len()),
        (12, 10, 22),
        "the fold exclusion must remove three entries on each of the four \
         equatorial faces and five on each of the two caps, and nothing else"
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

/// Blanks and repeats on the plate anchored at `f` — the two defects the
/// folds consist of, counted separately because they differ in kind: a cube
/// corner produces both, the pole produces only repeats.
fn fold_shape(f: &Facet, memo: &mut RoseMemo) -> (u32, u32) {
    let r = RoseRaster::build(f, W, H, memo);
    let mut blanks = 0u32;
    let mut drawn = 0u32;
    let mut distinct = std::collections::BTreeSet::new();
    for row in 0..H {
        for col in 0..W {
            match r.facet_at(col, row) {
                None => blanks += 1,
                Some(g) => {
                    drawn += 1;
                    distinct.insert(g.pack().ok());
                }
            }
        }
    }
    (blanks, drawn - distinct.len() as u32)
}

/// The smallest distance at which `anchor_at` stops folding and stays
/// stopped. "Stays stopped" is load-bearing and not pedantry: the decay is
/// **not monotone** — a polar cube corner's repeat count rises from 260 at
/// distance 0 to 324 at distance 8 before falling — so the first clean
/// reading is not necessarily the last dirty one plus one.
fn clears_at(memo: &mut RoseMemo, anchor_at: impl Fn(i64) -> Facet) -> i64 {
    const RUN: i64 = 8;
    const CEILING: i64 = 80;
    let mut d = 0;
    while d <= CEILING {
        if (d..=d + RUN).all(|k| fold_shape(&anchor_at(k), memo) == (0, 0)) {
            return d;
        }
        d += 1;
    }
    panic!("the fold did not clear within {CEILING} steps — it has grown enormously");
}

/// The two folds are exactly as wide as the measurement says, and no wider.
///
/// **This converts S9's and S10's tables from three numbers in a ledger into
/// a guard.** The exclusion the two strict properties take is only honest if
/// something independent pins how far it has to reach; without this test a
/// future change that widened either fold would be absorbed silently by the
/// exclusion, and a future change that widened the *exclusion* would narrow
/// what those properties are asserted over with nothing to object.
///
/// Four things are held, and the first is the one that makes the rest mean
/// anything:
///
/// 1. **Sufficiency, exactly.** Walking outward from each fold, every anchor
///    whose plate actually folds is excluded by [`inside_a_fold`]. This is
///    the load-bearing direction and it is asserted per step, not in
///    aggregate: an exclusion that missed a dirty facet would leave a strict
///    property asserted somewhere it cannot hold.
/// 2. **A two-sided pin on the extent**, the shape `hornvale_locale`'s
///    `ROSE_WORST_DEG` / `ROSE_WORST_FLOOR_DEG` pair uses. The ceiling fails
///    if a fold GREW past its radius; the floor fails if it shrank by more
///    than [`FOLD_FLOOR_SLACK`], which is good news that must be banked into
///    the radius rather than absorbed. Neither pins an exact number, so an
///    ordinary change of one facet does not redden it and a change of kind
///    does.
/// 3. **S9's actual finding**: the polar corner fold is strictly wider than
///    the equatorial one. Robust to the exact figures, and it fails if the
///    asymmetry vanishes or inverts — either of which is a real behavioural
///    change that should be looked at rather than absorbed.
/// 4. **The two folds differ in KIND**, which is why they need separate
///    radii: a cube corner blanks and repeats, the pole only ever repeats.
///    A pole that started blanking would be a chain terminating where no
///    bearing is refused.
///
/// The band split is measured here too rather than assumed, so a change to
/// the cube's face order reddens at a named latitude instead of silently
/// applying the wrong radius to every face.
#[test]
fn the_folds_are_where_the_measurement_says_and_no_wider() {
    let mut memo = RoseMemo::new();
    let scale: i64 = 1 << DEPTH;

    // (0) the band split, from the world rather than from a comment.
    for face in 0..6u8 {
        let centre = lattice(face, scale / 2, scale / 2, DEPTH).coord();
        let polar = centre.latitude.abs() > 80.0;
        assert_eq!(
            polar,
            is_a_polar_face(face),
            "face {face}'s centre is at latitude {:.3}, which disagrees with \
             is_a_polar_face — the cube's face order moved and every fold \
             radius below is now applied to the wrong band",
            centre.latitude
        );
    }

    // (1) + (2), corner folds, on EVERY face. Not one face per band: each
    // band's radius is set by whichever of its faces folds widest, so a
    // sample of one leaves the other free to grow unwatched. Measured, the
    // two caps do not agree — face 5 clears at 28 and face 4 at 27 — and it
    // was walking face 4 alone that hid the face setting the constant.
    let mut widest = [0i64; 2];
    for face in 0..6u8 {
        let polar = is_a_polar_face(face);
        let radius = if polar {
            POLAR_CORNER_FOLD
        } else {
            EQUATORIAL_CORNER_FOLD
        };
        let label = if polar { "polar" } else { "equatorial" };
        let at = |d: i64| lattice(face, d, d, DEPTH);
        for d in 0..=radius + FOLD_FLOOR_SLACK {
            let (blanks, repeats) = fold_shape(&at(d), &mut memo);
            if blanks + repeats > 0 {
                assert!(
                    inside_a_fold(&at(d)),
                    "face {face}'s {label} corner still folds {d} steps in \
                     ({blanks} blanks, {repeats} repeats) and inside_a_fold does \
                     not exclude it, so a strict property is being asserted \
                     where it cannot hold"
                );
            }
        }
        let clearing = clears_at(&mut memo, at);
        assert!(
            clearing <= radius,
            "face {face}'s {label} cube-corner fold now clears at {clearing}, \
             past its radius of {radius} — it has GROWN. Widen the radius \
             deliberately and say in its doc what moved."
        );
        let slot = usize::from(polar);
        widest[slot] = widest[slot].max(clearing);
    }
    // The floor arm is taken against each band's WIDEST face, since that is
    // the face the radius is set from; a narrower sibling is not an
    // improvement to bank.
    for (slot, (radius, label)) in [
        (EQUATORIAL_CORNER_FOLD, "equatorial"),
        (POLAR_CORNER_FOLD, "polar"),
    ]
    .into_iter()
    .enumerate()
    {
        assert!(
            widest[slot] >= radius - FOLD_FLOOR_SLACK,
            "the widest {label} cube-corner fold clears at {}, more than \
             {FOLD_FLOOR_SLACK} under its radius of {radius} — the raster \
             improved, which is good news that must be banked: lower the \
             radius toward {} and say in its doc what moved.",
            widest[slot],
            widest[slot]
        );
    }

    // (1) + (2), the pole fold, on both caps. Walked along +x;
    // `pole_distance` is the half-step Chebyshev, so distance and step index
    // agree exactly, and that agreement is itself asserted.
    let mut widest_pole = 0i64;
    for face in [4u8, 5u8] {
        let at_pole = |d: i64| lattice(face, scale / 2 + d, scale / 2, DEPTH);
        for d in 0..=POLE_FOLD + FOLD_FLOOR_SLACK {
            let (blanks, repeats) = fold_shape(&at_pole(d), &mut memo);
            assert_eq!(
                pole_distance(&at_pole(d)),
                d,
                "the pole distance and the walk index have come apart"
            );
            if blanks + repeats > 0 {
                assert!(
                    inside_a_fold(&at_pole(d)),
                    "face {face}'s pole still folds {d} facets out ({repeats} \
                     repeats) and inside_a_fold does not exclude it"
                );
            }
        }
        let clearing = clears_at(&mut memo, at_pole);
        assert!(
            clearing <= POLE_FOLD,
            "face {face}'s pole fold now clears at {clearing}, past its radius \
             of {POLE_FOLD} — it has GROWN"
        );
        widest_pole = widest_pole.max(clearing);
    }
    assert!(
        widest_pole >= POLE_FOLD - FOLD_FLOOR_SLACK,
        "the widest pole fold clears at {widest_pole}, more than \
         {FOLD_FLOOR_SLACK} under its radius of {POLE_FOLD} — bank it into the \
         radius"
    );

    // (3) S9's finding: the bands are not the same width.
    assert!(
        widest[1] > widest[0],
        "the polar cube-corner fold ({}) is no longer wider than the \
         equatorial one ({}) — S9's asymmetry has gone, which is a change of \
         behaviour, not of sample",
        widest[1],
        widest[0]
    );

    // (4) the two folds differ in kind, and each is non-vacuously present.
    let (corner_blanks, corner_repeats) = fold_shape(&lattice(4, 0, 0, DEPTH), &mut memo);
    assert!(
        corner_blanks > 0 && corner_repeats > 0,
        "non-vacuity: a plate anchored ON a polar cube corner must both blank \
         and repeat; got {corner_blanks} blanks, {corner_repeats} repeats"
    );
    let at_the_pole = lattice(4, scale / 2, scale / 2, DEPTH);
    assert_eq!(pole_distance(&at_the_pole), 0);
    let (pole_blanks, pole_repeats) = fold_shape(&at_the_pole, &mut memo);
    assert!(
        pole_repeats > 0,
        "non-vacuity: a plate anchored at the pole must repeat"
    );
    assert_eq!(
        pole_blanks, 0,
        "the pole BLANKED. No bearing is refused there — the meridians only \
         converge — so a blank means a chain terminated where nothing should \
         end it"
    );
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
/// Off the two folds — the cube corner and, on a cap, the pole. See
/// [`inside_a_fold`] for both, and for the measurement showing no plain seam
/// crossing needs the exemption on any face.
#[test]
fn one_step_shifts_the_picture_and_changes_nothing_else() {
    let mut memo = RoseMemo::new();
    let mut compared = 0u64;
    for f in sweep_off_the_folds(6) {
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
/// 2. **It is two-sided off the two folds** — the box a facet was drawn in
///    is the box `box_of` returns. This cannot hold where the picture
///    repeats, which is a cube corner (S6, S9) and, on a cap, the pole
///    (S10), so it is asserted where the picture is duplicate-free. See
///    [`inside_a_fold`].
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
    for f in sweep_off_the_folds(4) {
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
/// **It witnesses the CORNER half of [`inside_a_fold`]'s exclusion**, in
/// kind: the picture there carries both blanks and repeats, and this test
/// asserts both, so the exclusion cites a measured fact rather than an
/// absence of evidence. The EXTENT of both folds — and the pole's, which
/// repeats without ever blanking — is
/// `the_folds_are_where_the_measurement_says_and_no_wider`'s job.
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
        "the corner half of the fold exclusion is justified by repeated boxes \
         here (ledger S6/S9); this raster shows none, so either the corner no \
         longer degrades or this facet is not one"
    );
    // Well past the equatorial corner fold, the picture is whole again.
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

/// Rose steps from the centre to box `(col, row)` — `|dcol| + |drow|`,
/// which is exactly the number of chain steps [`RoseRaster::build`] walked
/// to draw that box (down or up the centre column, then out along the row).
fn steps_from_centre(centre: (u16, u16), at: (u16, u16)) -> u32 {
    let d = |a: u16, b: u16| u32::from(a.max(b) - a.min(b));
    d(centre.0, at.0) + d(centre.1, at.1)
}

/// A facet the picture repeats has more than one box, and the inverse map
/// answers with the one **nearest the centre** — nearest in rose steps (see
/// [`steps_from_centre`]), ties broken row-major so the answer stays
/// deterministic (ledger decision #5).
///
/// **The pole is the fixture** (ledger S18). At the exact pole the meridian
/// chain reverses: `north` steps off the pole and `north` again returns to
/// it, so the pole is drawn in every other box of the centre column, top to
/// bottom. The first-row-major inverse this replaces answered row 0 for the
/// one facet everything else agrees sits at the centre — and
/// `plate::Placement::box_of_facet` is how `perception_boxes` places the
/// observer's own mark, so a polar observer's `@` drew near the top of the
/// column rather than at the middle, while `facet_at(centre)` correctly
/// held their facet.
///
/// **The `repeats > 1` guard is the point of the fixture, not decoration.**
/// Asked about a facet with one box, every tie-break rule agrees and the
/// assertion is about a scan order that cannot express itself.
///
/// Part (2) states the rule itself over the whole population rather than
/// the pole alone: no box drawn with the named facet is nearer the centre
/// than the one named, and an equally near one never precedes it row-major.
/// It cannot constrain a facet drawn once, so it carries its own count of
/// how many boxes were repeats.
#[test]
fn the_inverse_map_names_the_box_nearest_the_centre() {
    let mut memo = RoseMemo::new();

    // (1) the pole, where the chain reverses and the centre column repeats.
    let scale: i64 = 1 << DEPTH;
    let pole = lattice(4, scale / 2, scale / 2, DEPTH);
    let r = RoseRaster::build(&pole, W, H, &mut memo);
    let mut pole_boxes = Vec::new();
    for row in 0..H {
        for col in 0..W {
            if r.facet_at(col, row) == Some(&pole) {
                pole_boxes.push((col, row));
            }
        }
    }
    assert!(
        pole_boxes.len() > 1,
        "non-vacuity: the pole must be drawn in more than one box for a \
         tie-break to have anything to decide; it was drawn in {:?}",
        pole_boxes
    );
    assert_eq!(
        r.box_of(&pole),
        Some(r.centre()),
        "the anchor's own box must be the centre; the raster draws it in \
         {pole_boxes:?}"
    );

    // (2) the rule, over the whole population including both folds.
    let mut repeated = 0u64;
    let mut examined = 0u64;
    for f in sweep(4) {
        let r = RoseRaster::build(&f, W, H, &mut memo);
        let centre = r.centre();
        for row in 0..H {
            for col in 0..W {
                let Some(here) = r.facet_at(col, row) else {
                    continue;
                };
                examined += 1;
                let named = r.box_of(here).expect("a drawn facet must have a box");
                if named == (col, row) {
                    continue;
                }
                repeated += 1;
                let (near, far) = (
                    steps_from_centre(centre, named),
                    steps_from_centre(centre, (col, row)),
                );
                assert!(
                    near <= far,
                    "box_of named {named:?} at {near} steps when {:?} sits at \
                     {far}",
                    (col, row)
                );
                if near == far {
                    assert!(
                        (named.1, named.0) < (row, col),
                        "an equally near box must be broken row-major: \
                         box_of named {named:?} over {:?}",
                        (col, row)
                    );
                }
            }
        }
    }
    assert!(examined > 10_000, "vacuity guard: {examined} boxes");
    assert!(
        repeated > 0,
        "non-vacuity: the population must contain a repeated box for the \
         rule to bind; both folds are in `sweep`"
    );
}

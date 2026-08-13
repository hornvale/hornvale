//! The channel network's byte pin (The Ford, Task 6).
//!
//! **The gap this closes.** A reviewer mutated `channel_seed` to derive off
//! `streams::LITHOLOGY` instead of `streams::CHANNEL_MEANDER` — reshaping
//! every meander of every river in every world — and the entire terrain suite
//! stayed green: 189 + 19 + 3 passed. Every property the suite asserts about
//! the network (well-formedness, determinism, band ordering, per-vertex
//! geometry, continuity of the meander field) is true of *any* meander field,
//! so none of them can see the field being replaced by a different one.
//!
//! **That exact mutation is now caught twice, and the second catch is the
//! reason this file exists.** Task 5's fixup added
//! `tectonic_properties.rs::channel_seed_is_the_derived_leaf_…`, which asserts
//! the stored seed equals `ROOT.derive(CHANNEL_MEANDER)` — so it reddens on
//! that mutation too. But it can only ever see the *label at one call site*.
//! It is blind to the meander amplitude, the frequency, the octave count, the
//! anchoring rule for heads and mouths, the perpendicular the displacement is
//! taken along, the run construction, and the width and confinement laws —
//! every one of which reshapes the same rivers just as thoroughly. Measured:
//! mutating `MEANDER_AMPLITUDE_RATIO` from 0.25 to 0.24 leaves the seed
//! equality test green (20 passed) and the whole rest of the terrain suite
//! green (189 + 6 + 2 + 20), and reddens only this fixture. A guard on a
//! derivation is not a guard on what the derivation produces.
//!
//! **THIS FIXTURE WAS RE-BASELINED BY THE RILL'S TASK 3, AND A RE-BASELINED
//! FIXTURE JUDGES NOTHING.** It witnesses whatever it was regenerated from, so
//! the guards that had to exist BEFORE the re-baseline live in
//! `rill_properties.rs`: `the_network_renders_every_river_cells_downhill_edge`
//! (watched green on the pre-change network — every edge the old network drew,
//! the new one still draws, in the same direction) and
//! `the_meander_field_is_pinned` (the noise field's own values at eight fixed
//! positions, bit-exact, which no change to run construction can move). Neither
//! reads a fixture. What this file still uniquely covers is the meander
//! AMPLITUDE, the head/mouth anchoring rule and the perpendicular the
//! displacement is taken along — and it covers them going forward, which is all
//! a re-baselined witness can ever do. The migration itself was checked rather
//! than assumed: all 54 non-terminal cells of the old fixture carry IDENTICAL
//! band edges in the new one, and all 32 cells interior in both networks carry
//! an identical displacement magnitude AND sign.
//!
//! **What is pinned, and why it is a witness rather than a restatement.**
//! Not the meander formula, and not `SphereFbm`'s output — either would be
//! this test agreeing with the code's own arithmetic, and a mutation that
//! changed the arithmetic would change both sides together. What is pinned is
//! the *observable consequence*: for every vertex of every polyline, how far
//! and which way the channel was displaced from the cell it was placed from.
//! That displacement is read by comparing two things the network publishes —
//! `run_cells[i][j]`'s position on the geosphere, and `polylines[i][j]` — so
//! the fixture is derived from the network's output, never from the noise
//! call that produced it. Change the seed the field is drawn from and every
//! interior row moves.
//!
//! It also pins the band geometry (`band_edges`), so a change to the width
//! law, `GORGE_SLOPE`, or the confinement law lands here too. That is
//! deliberate: those are calibration constants, and a calibration should be a
//! reviewed migration rather than a silent one.
//!
//! **The mutation to re-run if you want to know this fixture still sees the
//! confluence repair's DIRECTION — and the trap in choosing one.** In
//! `ChannelNetwork::build`'s confluence pass, replace the mouth assignment
//! with a reflection of the trunk vertex `t` across the great circle through
//! the mouth cell `b` and its predecessor `pv`:
//!
//! ```text
//! let n = normalize(cross(b, pv));            // n is perpendicular to b
//! let d2 = 2.0 * dot(t, n);
//! [t[0] - d2*n[0], t[1] - d2*n[1], t[2] - d2*n[2]]
//! ```
//!
//! Because `n` is perpendicular to `b`, this leaves `dot(b, t')` equal to
//! `dot(b, t)`: the displacement's MAGNITUDE is preserved exactly and only
//! its sign flips. That is the whole point — it is the mutation that isolates
//! direction. Row `9 1 7700` goes `+0.00087797556` to `-0.00087797556`.
//!
//! Do **not** reach for `normalize(2b - t)`. It looks like a mirror and is
//! not distance-preserving: it perturbs the magnitude in the eighth
//! significant digit, so the fixture reddens on the magnitude change and
//! tells you nothing about whether it can see a sign. Measured both ways —
//! the pre-fix render, which really was blind to direction, reddened on
//! `normalize(2b - t)` and stayed **green** on the reflection above. A
//! mutation proves only what it perturbs.
//!
//! Regenerate deliberately: `REBASELINE=1 cargo test -p hornvale-terrain
//! --test channel_golden` (or `make rebaseline-goldens`), then read the diff
//! as a change to every river in every world.

use hornvale_kernel::{Geosphere, Seed, quantize::quantize};
use hornvale_terrain::{GeneratedTerrain, TerrainPins};

/// Level 5, not the canonical 6: the point of this fixture is the derivation,
/// which is grid-independent, and level 5 keeps the commit gate cheap (a
/// sub-second world; 3887 channel vertices since The Rill's Task 3, **76
/// immediately before Task 3, and 46 before the campaign began**). Both
/// baselines are given because the ~51x below is 3887/76 — Task 2 had already
/// taken the fixture 46 -> 76 by making every run reach the cell it drains
/// into, so dividing by the pre-campaign 46 gives 84.5 and contradicts it.
/// Level 6 is the readout's grid.
///
/// **The fixture is ~3900 rows and ~290 KB, and that is the price of the only
/// topology witness in the repo.** Task 3 made the network render the whole
/// land flow tree rather than the top 6.7% of it, so the row count grew by
/// ~51x on this grid. Moving the fixture to level 4 to shrink it would break
/// its continuity with everything measured against it and would pin a grid the
/// campaign never reads.
const LEVEL: u32 = 5;

/// Signed magnitude of the displacement from `base` to `placed`, radians:
/// the great-circle angle, signed by which side of the channel's direction of
/// travel it landed on. The sign is what distinguishes a meander field from
/// its own negation.
///
/// `travel` is the direction of travel AT this vertex, as a vector rather
/// than as the next cell's position. That distinction is a bug fix, not a
/// style choice: the caller used to pass `cells[(j+1).min(len-1)]`, which at a
/// run's final vertex is the vertex itself, making `cross(base, base)` the
/// zero vector and rendering EVERY final vertex with a positive sign whatever
/// side it actually landed on. That is the one place the confluence repair
/// writes to, so the single row pinning the repair was pinning magnitude
/// only, and a mutation placing the mouth at the trunk's mirror-image
/// position would have rendered an identical fixture.
fn signed_displacement(base: [f64; 3], placed: [f64; 3], travel: [f64; 3]) -> f64 {
    let dot = |a: [f64; 3], b: [f64; 3]| a[0] * b[0] + a[1] * b[1] + a[2] * b[2];
    let cross = |a: [f64; 3], b: [f64; 3]| {
        [
            a[1] * b[2] - a[2] * b[1],
            a[2] * b[0] - a[0] * b[2],
            a[0] * b[1] - a[1] * b[0],
        ]
    };
    // An ANCHORED vertex is the cell's own position, bit for bit, and has no
    // side. Say that exactly instead of letting `acos(dot(p, p))` report the
    // ~1.5e-8 rad of float residue it actually does: that residue is five
    // orders of magnitude below a real displacement (~1e-4), its sign is
    // arbitrary — it flipped on cell 824 purely from this function's change
    // of travel vector — and it was silently counting anchored rows as
    // "moved" in `the_pinned_displacements_are_not_all_zero`, inflating the
    // very number that guards this fixture against vacuity.
    if base == placed {
        return 0.0;
    }
    let magnitude = hornvale_kernel::math::acos(dot(base, placed).clamp(-1.0, 1.0));
    if magnitude == 0.0 {
        return 0.0;
    }
    // `left` is the same left-positive convention `SphericalPolyline`'s
    // signed distance uses, so a reader comparing the two is not comparing
    // opposite sign conventions.
    let left = cross(base, travel);
    let side = dot(left, placed);
    if side < 0.0 { -magnitude } else { magnitude }
}

/// `a - b`, componentwise.
fn sub(a: [f64; 3], b: [f64; 3]) -> [f64; 3] {
    [a[0] - b[0], a[1] - b[1], a[2] - b[2]]
}

fn render() -> String {
    let geo = Geosphere::new(LEVEL);
    let outcome =
        hornvale_terrain::generate(Seed(42), &geo, &TerrainPins::default()).expect("seed 42");
    let terrain = GeneratedTerrain::new(geo, outcome);
    let geo = terrain.geosphere();
    let net = terrain.channels();

    let mut out = String::new();
    out.push_str("# The Ford: seed 42, Geosphere::new(5) channel network.\n");
    out.push_str(
        "# Columns: line vertex cell displacement_rad half_width_rad bank_rad valley_rad terrace_rad\n",
    );
    out.push_str(&format!(
        "# lines={} vertices={}\n",
        net.polylines.len(),
        net.polylines.iter().map(|l| l.points.len()).sum::<usize>()
    ));
    for (i, line) in net.polylines.iter().enumerate() {
        let cells = &net.run_cells[i];
        for (j, &placed) in line.points.iter().enumerate() {
            let base = geo.position(cells[j]);
            // The direction of travel: toward the next cell, or — at the last
            // vertex, where there is no next — away from the previous one. A
            // run always has at least two cells, so both arms are in range.
            // `cross(base, ·)` annihilates any component along `base`, so
            // passing the difference vector gives the identical left-normal
            // the next cell's position did at interior vertices.
            let travel = if j + 1 < cells.len() {
                sub(geo.position(cells[j + 1]), base)
            } else {
                sub(base, geo.position(cells[j - 1]))
            };
            let e = net.band_edges[i][j];
            out.push_str(&format!(
                "{i} {j} {} {} {} {} {} {}\n",
                cells[j].0,
                quantize(signed_displacement(base, placed, travel)),
                quantize(e[0]),
                quantize(e[1]),
                quantize(e[2]),
                quantize(e[3]),
            ));
        }
    }
    out
}

#[test]
fn the_channel_network_is_pinned() {
    hornvale_kernel::golden::assert_golden(
        std::path::Path::new(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/tests/fixtures/channel-network-seed-42-level-5.txt"
        )),
        &render(),
        "the channel network moved. If you changed the seed the meander field is drawn from \
         (`TectonicGlobe::channel_seed` / `streams::CHANNEL_MEANDER`), you have reshaped every \
         river in every world and this is the only thing in the repo that would have told you \
         — that is what this fixture exists for. If you changed the width law, GORGE_SLOPE or \
         the confinement law, this is a calibration migration. Either way accept it \
         deliberately with REBASELINE=1 and review the diff",
    );
}

/// The fixture is only a guard if a change to the meander seed actually moves
/// it, and the displacement column is the only column that could. Assert the
/// column is not all zeros: if the meander were disabled — or if the anchoring
/// rule ever extended to every vertex — the fixture would be a constant and
/// the seed guard would be silently vacuous.
#[test]
fn the_pinned_displacements_are_not_all_zero() {
    let rendered = render();
    // EVERY row is counted, anchored heads and mouths included — they are not
    // filtered out, and the threshold is set knowing they are in the
    // denominator. Measured on the Task 3 network: **1849 of 3887 rows (47.6%)**
    // carry a displacement, so a third is cleared with room. The breakdown is
    // worth having, because two of its three parts are counter-intuitive: 2234
    // rows are anchored (two per run, and runs are short), of which **267 are
    // non-zero anyway** — those are confluence mouths, which the repair
    // relocates onto their trunk's vertex, so "anchored" does not imply
    // "undisplaced"; and of the 1653 interior rows **71 read exactly zero**,
    // which are gorge vertices, where `confinement` is 0 and the meander
    // amplitude with it. (Before Task 3 the same figures were 21 of 46.)
    //
    // That reasoning only became true when `signed_displacement` started
    // reporting an anchored vertex as exactly 0. Before it, `acos(dot(p, p))`
    // gave anchored rows ~1.5e-8 rad of float residue, so **31** of 46 read
    // as "moved" against the 21 that carry a real displacement — 10 of the 31
    // were noise, and about a third of this guard was satisfied by it, the
    // opposite of what the paragraph above claimed about anchored rows.
    //
    // 31 and not 36: five of the anchored rows rendered the literal `-0`, and
    // this loop parses to `f64`, where `-0.0 != 0.0` is FALSE. So the guard
    // never counted those five, and a count taken by eye off the fixture
    // disagrees with the count the code performs. Read the parse, not the
    // column.
    let mut rows = 0usize;
    let mut moved = 0usize;
    for row in rendered.lines().filter(|l| !l.starts_with('#')) {
        let f: Vec<&str> = row.split(' ').collect();
        let displacement: f64 = f[3].parse().expect("a numeric displacement column");
        if displacement != 0.0 {
            moved += 1;
        }
        rows += 1;
    }
    assert!(rows > 30, "too few rows to be a real check");
    assert!(
        moved * 3 > rows,
        "only {moved} of {rows} vertices carry a meander displacement — the column this \
         fixture guards the seed WITH is nearly constant, so the guard is vacuous"
    );
}

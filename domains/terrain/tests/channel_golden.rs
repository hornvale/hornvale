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
//! Regenerate deliberately: `REBASELINE=1 cargo test -p hornvale-terrain
//! --test channel_golden` (or `make rebaseline-goldens`), then read the diff
//! as a change to every river in every world.

use hornvale_kernel::{Geosphere, Seed, quantize::quantize};
use hornvale_terrain::{GeneratedTerrain, TerrainPins};

/// Level 5, not the canonical 6: the point of this fixture is the derivation,
/// which is grid-independent, and level 5 keeps the commit gate cheap (46
/// channel vertices, a sub-second world). Level 6 is the readout's grid.
const LEVEL: u32 = 5;

/// Signed magnitude of the displacement from `base` to `placed`, radians:
/// the great-circle angle, signed by which side of the channel's direction of
/// travel it landed on. The sign is what distinguishes a meander field from
/// its own negation.
fn signed_displacement(base: [f64; 3], placed: [f64; 3], along: [f64; 3]) -> f64 {
    let dot = |a: [f64; 3], b: [f64; 3]| a[0] * b[0] + a[1] * b[1] + a[2] * b[2];
    let cross = |a: [f64; 3], b: [f64; 3]| {
        [
            a[1] * b[2] - a[2] * b[1],
            a[2] * b[0] - a[0] * b[2],
            a[0] * b[1] - a[1] * b[0],
        ]
    };
    let magnitude = hornvale_kernel::math::acos(dot(base, placed).clamp(-1.0, 1.0));
    // `left` is the same left-positive convention `SphericalPolyline`'s
    // signed distance uses, so a reader comparing the two is not comparing
    // opposite sign conventions.
    let left = cross(base, along);
    let side = dot(left, placed);
    if side < 0.0 { -magnitude } else { magnitude }
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
            let next = geo.position(cells[(j + 1).min(cells.len() - 1)]);
            let e = net.band_edges[i][j];
            out.push_str(&format!(
                "{i} {j} {} {} {} {} {} {}\n",
                cells[j].0,
                quantize(signed_displacement(base, placed, next)),
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
    let mut interior = 0usize;
    let mut moved = 0usize;
    for row in rendered.lines().filter(|l| !l.starts_with('#')) {
        let f: Vec<&str> = row.split(' ').collect();
        // A head and a mouth are anchored by design, so only interior
        // vertices are expected to carry a displacement.
        let displacement: f64 = f[3].parse().expect("a numeric displacement column");
        if displacement != 0.0 {
            moved += 1;
        }
        interior += 1;
    }
    assert!(interior > 30, "too few rows to be a real check");
    assert!(
        moved * 3 > interior,
        "only {moved} of {interior} vertices carry a meander displacement — the column this \
         fixture guards the seed WITH is nearly constant, so the guard is vacuous"
    );
}

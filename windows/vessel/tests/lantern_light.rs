//! The light field on the geometry production actually embeds (The Lantern,
//! spec §4.1).
//!
//! MEASURED ON REAL LATTICES, NOT DRAWN BOXES. `light.rs`'s own unit tests
//! use an authored room, deliberately — a drawn box states the distances the
//! *law* turns on. This battery answers the different question: does the
//! architectural claim survive contact with a lattice `allocate` and `grow`
//! actually produce, with its doorways, its slivers and its pinched non-convex
//! blobs? The Beholding's 28-of-255 on authored fixtures collapsed to 2-of-255
//! on real ground, and a claim about geometry is exactly the kind that failure
//! mode fakes.
//!
//! **Why this file lives under `windows/vessel/tests/` and not
//! `windows/worldgen/tests/`**, where the plan first put it: `hornvale-vessel`
//! depends on `hornvale-worldgen`, so worldgen cannot see `light` at all.
//!
//! No world is built here and none is needed. A lattice is `FRAME`-tier
//! (decision 0069) and derived from a structure plus a seed, so the fixtures
//! below are as real as the ones the possession walks without paying for a
//! terrain sculpt.

use hornvale_kernel::color::blackbody;
use hornvale_kernel::{RoomAddr, Seed};
use hornvale_vessel::lattice::shadowcast;
use hornvale_vessel::light::{Source, TORCH_KELVIN, light_field};
use hornvale_vessel::structure::{Structure, structure_at};
use hornvale_vessel::{Brief, Cell, Lattice, embed_with, extent_for};
use std::collections::BTreeSet;

/// The walk depth the vessel's own lattice fixtures use.
const WALK: u32 = 12;

/// A built place; the brief `allocate` is selected by.
fn built() -> Brief {
    Brief::from_parts(None, None, None, None, 0, true, true)
}

/// A wild place; the brief `grow` is selected by — the hostile geometry.
fn wild() -> Brief {
    Brief::from_parts(None, None, None, None, 0, false, true)
}

fn locale_number(n: u64) -> RoomAddr {
    RoomAddr {
        face: 3,
        path: (0..WALK).map(|i| ((n >> (2 * i)) & 0b11) as u8).collect(),
    }
}

fn structure_of(chamber_count: usize, seed: Seed) -> Structure {
    for n in 0u64..4096 {
        let locale = locale_number(n);
        let s = structure_at(&locale, &built(), seed, WALK).expect("built");
        if s.chambers.len() == chamber_count {
            return s;
        }
    }
    panic!("no locale in 4096 draws a {chamber_count}-chamber structure at {seed:?}");
}

fn embedded(chamber_count: usize, seed: Seed, method: &Brief) -> Lattice {
    let structure = structure_of(chamber_count, seed);
    let extent = extent_for(&structure);
    embed_with(&structure, method, extent, seed)
}

/// The lattices this battery reads: both embedders, several chamber counts.
fn fixtures() -> Vec<(String, Lattice)> {
    let mut out = Vec::new();
    for (label, brief) in [("rectilinear", built()), ("grown", wild())] {
        for (n, seed) in [(2usize, Seed(4)), (3, Seed(1)), (4, Seed(2))] {
            out.push((format!("{label} n={n} {seed:?}"), embedded(n, seed, &brief)));
        }
    }
    out
}

/// Every passable cell of a lattice, in `Cell` order.
fn floors(lattice: &Lattice) -> Vec<Cell> {
    lattice
        .cells
        .iter()
        .filter(|(_, kind)| kind.passable())
        .map(|(cell, _)| *cell)
        .collect()
}

/// **The architectural claim of spec §4.1, on real geometry.** `shadowcast`
/// is symmetric, so what can see a cell and what lights it are the same set:
/// light needs no new geometry, and `light_field` must therefore reach
/// exactly the field of view and not one cell more or less.
///
/// FIRES WHEN: `light_field` grows a second opinion about reach — its own ray
/// cast, a Euclidean radius clip, a "walls are not lit" filter, or a
/// post-pass that drops cells below some brightness. Any of those would look
/// entirely reasonable and would break the claim the whole campaign rests on.
///
/// Checked at every floor cell of every fixture rather than at a sampled one:
/// a divergence that appears only at a doorway or a sliver is exactly what a
/// sampled check misses.
#[test]
fn light_reaches_exactly_what_sight_reaches() {
    for (label, lattice) in fixtures() {
        let origins = floors(&lattice);
        assert!(
            origins.len() > 8,
            "{label}: too little floor to be a test ({} cells)",
            origins.len()
        );
        for origin in origins {
            for radius in [1, 4, 12] {
                let field = light_field(
                    &lattice,
                    &[Source {
                        at: origin,
                        illuminant: blackbody(TORCH_KELVIN),
                        radius,
                    }],
                );
                let lit: BTreeSet<Cell> = field.keys().copied().collect();
                assert_eq!(
                    lit,
                    shadowcast(&lattice, origin, radius),
                    "{label}: the light from {origin:?} at radius {radius} is not \
                     the field of view from {origin:?}"
                );
            }
        }
    }
}

/// A real chamber is not uniformly lit under the implicit torch.
///
/// The torch rides on the observer and `shadowcast` is symmetric, so every
/// visible cell is lit **by construction** — attenuation is the only thing
/// left that can vary across a room (spec §4.2). This states that it actually
/// does so on a lattice the game embeds, and reports the spread it produces.
///
/// This is a model-level reading and deliberately not H4a: H4a asks how dark
/// a chamber gets *once rendered*, which needs the scotopic term that has not
/// shipped yet (Task 7). The number printed here is its precursor, and the
/// attenuation constant may not be tuned to move either (§11 risk 2).
///
/// FIRES WHEN: attenuation goes flat, or the torch's reach collapses to its
/// own cell — either would leave a possession with no gradient to read.
#[test]
fn a_real_chamber_is_not_uniformly_lit() {
    for (label, lattice) in fixtures() {
        let origin = *floors(&lattice).first().expect("a lattice holds floor");
        let field = light_field(
            &lattice,
            &[Source {
                at: origin,
                illuminant: blackbody(TORCH_KELVIN),
                radius: 8,
            }],
        );
        // Band 5 stands for the whole curve: attenuation is achromatic, so
        // every band carries the same ratio and one of them says it all.
        let mut levels: Vec<f64> = field.values().map(|light| light.get()[5]).collect();
        levels.sort_by(|a, b| a.total_cmp(b));
        let (dimmest, brightest) = (levels[0], levels[levels.len() - 1]);
        eprintln!(
            "{label}: {} lit cells from {origin:?}, dimmest/brightest = {:.4}",
            levels.len(),
            dimmest / brightest
        );
        assert!(
            dimmest < brightest,
            "{label}: every one of the {} cells lit from {origin:?} is equally \
             bright — the possession has no gradient at all",
            levels.len()
        );
    }
}

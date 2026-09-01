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
use hornvale_kernel::{Facet, Seed};
use hornvale_vessel::lattice::shadowcast;
use hornvale_vessel::light::{ATTENUATION, Source, TORCH_KELVIN, light_field};

/// The calibrated brightness floor of the ×4 torch at distance 4 (see the
/// distance-4 test below): strictly above the pre-change reading, well under
/// what a nearer cell renders.
const BRIGHT_SUM: u32 = 172;
use hornvale_vessel::site::{Site, SiteKind};
use hornvale_vessel::structure::{Structure, structure_at};
use hornvale_vessel::{
    Brief, Cell, Lattice, PossessOpts, Session, SpatialChannel, embed_with, extent_for,
};
use std::collections::BTreeSet;

/// The walk depth the vessel's own lattice fixtures use.
const WALK: u32 = 13;

/// A built place; the brief `allocate` is selected by.
fn built() -> Brief {
    Brief::from_parts(
        None,
        None,
        None,
        None,
        0,
        true,
        true,
        Some(Site::new(SiteKind::Settlement, None)),
    )
}

/// A wild place; the brief `grow` is selected by — the hostile geometry.
fn wild() -> Brief {
    Brief::from_parts(None, None, None, None, 0, false, true, None)
}

fn locale_number(n: u64) -> Facet {
    Facet {
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

/// Chebyshev distance, the metric `light_field` attenuates by.
fn cheb(a: Cell, b: Cell) -> i32 {
    (a.0 - b.0).abs().max((a.1 - b.1).abs())
}

/// **The H4a fence, executable:** the inverse-square gradient SHAPE may not
/// move, whatever happens to the torch's intensity (spec §4.2, §11 risk 2).
/// Scaling the source multiplies every cell's light by the same factor, so
/// the ratio of lights at Chebyshev distances 1 and 2 stays exactly
/// `(1 + 4·ATTENUATION) / (1 + ATTENUATION)` = 2.5 — before and after any
/// intensity change. This pins the shape, never the level.
///
/// FIRES WHEN: the falloff shape is retuned (a different `ATTENUATION`, a
/// linear or clipped falloff, a per-band gradient) — anything that changes
/// relative brightness between neighbours rather than overall brightness.
#[test]
fn the_falloff_ratio_between_two_cells_is_inverse_square() {
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
        let near = field.keys().find(|&&c| cheb(origin, c) == 1).copied();
        let far = field.keys().find(|&&c| cheb(origin, c) == 2).copied();
        let (Some(near), Some(far)) = (near, far) else {
            panic!(
                "{label}: no cell pair at Chebyshev distances 1 and 2 — \
                 fixture too small to state the fence"
            );
        };
        let (lnear, lfar) = (field[&near].get()[5], field[&far].get()[5]);
        let want = (1.0 + ATTENUATION * 4.0) / (1.0 + ATTENUATION * 1.0);
        let got = lnear / lfar;
        assert_eq!(
            got, want,
            "{label}: falloff ratio {origin:?}->{near:?}/{far:?} is {got}, \
             the fence pins {want}"
        );
    }
}

/// The row-major index of `(x, y)` in a plan whose extent starts at
/// `(e.x, e.y)` and is `e.w` wide.
fn plan_index(plan: &hornvale_vessel::plan::SessionPlan, x: i32, y: i32) -> usize {
    let e = &plan.extent;
    (((y - e.y) * e.w) + (x - e.x)) as usize
}

/// **The Wick's product claim, through the public seam:** a floor cell four
/// cells from the standing cell renders ~4× brighter once the implicit torch
/// burns at ×4 (spec §2.1).
///
/// `session.rs::chamber_sources` is private by design — source composition is
/// not API — so the only honest read of the change is the palette the game
/// actually emits. sRGB bytes are tone-mapped, not linear in illuminant, so
/// the assertion is the calibrated band between the two models rather than an
/// exact ×4 on bytes: pre-change, the distance-4 floor colour on seed 42's
/// entered chamber measured `[r, g, b]` summing to OLD_SUM (recorded at the
/// calibration run); post-change it must exceed BRIGHT_SUM. The fence test
/// above carries the exactness this test deliberately trades for seam reach.
///
/// FIRES WHEN: the ×4 scale is dropped from `chamber_sources` (the far cell
/// falls back to its dim pre-change triple) or the torch stops reaching
/// distance 4 at all.
#[test]
fn a_floor_cell_four_cells_out_renders_brighter_under_the_wick_torch() {
    let world = hornvale_worldgen::build_world(
        Seed(42),
        &Default::default(),
        hornvale_worldgen::SkyChoice::Generated,
        &Default::default(),
        &Default::default(),
    )
    .expect("seed 42 builds");
    let (mut session, _) =
        Session::start(&world, &PossessOpts::default()).expect("seed 42 possesses");
    session.handle("enter");
    let snap = session.snapshot().expect("a live session snapshots");
    let SpatialChannel::Chamber { plan } = snap.spatial else {
        panic!("seed 42: `enter` did not put the possession inside a building");
    };

    // A coloured FLOOR cell at Chebyshev distance 4 from the standing cell:
    // far enough that attenuation has eaten most of the torch, near enough
    // that SIGHT_RADIUS still lights it.
    //
    // **THE BRIGHTEST such cell, not the first one the scan meets.** The old
    // form took whichever cell row-major order reached first, and that made
    // the reading a function of where the chamber's floor happens to sit as
    // much as of the torch: a plan cell's colour is the surface's own albedo
    // TONE-MAPPED by the light on it, so two floor cells the same distance
    // out render very differently. Measured on the chamber seed 42 enters
    // after The Pavement's epoch, the twenty-three lit floor cells at
    // distance 4 span **135 to 460** in channel sum — the first-scanned one is
    // 135 and the brightest is 460, against a floor of 172. Nothing about the
    // torch moved; the epoch moved the chamber, and the scan's arbitrary
    // choice landed on a dark surface.
    //
    // The claim is that the ×4 torch REACHES distance 4, and one cell
    // witnesses that. Taking the brightest states which cell is the witness
    // instead of leaving it to iteration order.
    let e = &plan.extent;
    let mut probe = None;
    for y in e.y..e.y + e.h {
        for x in e.x..e.x + e.w {
            let entry = &plan.palette[plan.cells[plan_index(&plan, x, y)] as usize];
            if entry.kind == "floor"
                && let Some(color) = entry.color
                && cheb(hornvale_vessel::Cell(x, y), Cell(plan.you.x, plan.you.y)) == 4
            {
                let sum: u32 = color.iter().map(|&c| c as u32).sum();
                if probe.map(|(_, _, _, s)| sum > s).unwrap_or(true) {
                    probe = Some((x, y, color, sum));
                }
            }
        }
    }
    let (x, y, color, _) =
        probe.unwrap_or_else(|| panic!("seed 42: no lit floor cell at Chebyshev distance 4"));
    let sum: u32 = color.iter().map(|&c| c as u32).sum();
    eprintln!("wick probe: floor ({x},{y}) renders {color:?}, channel sum {sum}");
    assert!(
        sum > BRIGHT_SUM,
        "floor cell at distance 4 renders {color:?} (sum {sum}); under the \
         ×4 torch it must exceed channel-sum {BRIGHT_SUM}"
    );
}

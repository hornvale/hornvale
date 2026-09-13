//! THE TRENCHER, Task 8 (T4): the **two-way control** on the metaphysics
//! gate (The Ground, spec §8).
//!
//! The gate's inert half is already guarded, in the ordinary tier, by
//! `domains/terrain/src/lithology.rs`'s
//! `buffer_axes_are_bounded_and_thaumic_is_zero`: seed 42 under
//! `TerrainPins::default()`, `thaumic == 0.0` at every vertex. That arm is
//! **not** duplicated here. What it cannot do — what no inert arm can do —
//! is tell a working gate from a dead one: a `thaumic_at` whose charged
//! branch were deleted outright, or a pin that never reached
//! `TectonicGlobe::metaphysics`, would leave that guard green forever.
//!
//! This file supplies the other half, and it deliberately makes **two**
//! claims of opposite sign over the same pair of worlds:
//!
//! 1. **It fires.** Under `metaphysics: Some(Metaphysics::Thaumic)`,
//!    `thaumic` is non-zero at a substantial, named fraction of vertices.
//! 2. **It fires ONLY where it should.** Every other `MaterialBuffer` axis —
//!    and `elevation`, `is_ocean` and `rock` beneath them — is *identical*
//!    between the inert and charged builds of the same seed, at every
//!    vertex. This is the claim the gate's own author measured and it is the
//!    one re-measured here by a different implementer, from the composition
//!    root rather than from `assemble_material` directly.
//!
//! The two arms are what make the control two-way: a gate wired to nothing
//! fails (1), and a gate that consumed a draw or leaked into a neighbouring
//! axis fails (2). Neither can be satisfied by weakening the other.
//!
//! # The one thing that looks like duplication and is not
//!
//! This file does assert `thaumic == 0.0` on its **inert** arm. That is the
//! *base of the subtraction*, not a second opinion about inert worlds: the
//! headline number below is a per-vertex **difference** between two builds,
//! and a difference is only readable as "the gate fired" once the side it is
//! measured from is known to be zero. Stated the other way — if the inert
//! side were ever non-zero, the table would still print and would mean
//! something else entirely. The domain guard's claim (level 4, through
//! `assemble_material` directly) and this one (level 6, through the provider
//! and the composition root) are different constructions of the same value,
//! and neither subsumes the other.
//!
//! # Why the composition root and not `terrain::generate`
//!
//! The gate spans three hops — `TerrainPins` → `generate`'s resolution →
//! `TectonicGlobe::metaphysics` → `assemble_material` — and only the last
//! two are visible from inside `domains/terrain`. Building through
//! `build_world_to_with_artifacts` exercises the pin's whole journey,
//! including the `globe_level` default a domain-level test would have to
//! choose for itself.
//!
//! Test fixture (decision 0092): calls the composition-root entry points
//! directly, the sanctioned posture for this crate's live-worldgen
//! batteries.

use hornvale_astronomy::SkyPins;
use hornvale_kernel::{Seed, VertexMap};
use hornvale_terrain::{GeneratedTerrain, MaterialBuffer, Metaphysics, TerrainPins};
use hornvale_worldgen::{
    BuildDepth, SettlementPins, WorldComponents, build_world_to_with_artifacts,
};

/// The seed both arms build. Seed 42 is the project's canonical probe seed
/// and is the seed the inert guard this file complements already uses, so
/// the two halves of the control describe the same world.
const SEED: u64 = 42;

/// Every axis of [`MaterialBuffer`], in declaration order. The comparison
/// below is driven by this list rather than by a hand-written sequence of
/// asserts so that a **new** axis added to the buffer shows up as a compile
/// error in [`axis_differs`] instead of being silently unchecked.
const AXES: [&str; 10] = [
    "silica",
    "grain",
    "induration",
    "carbonate",
    "metamorphic_grade",
    "porosity",
    "margin",
    "soil_depth",
    "basement",
    "thaumic",
];

/// The index of `thaumic` in [`AXES`] — the one axis the gate is licensed
/// to move.
const THAUMIC: usize = 9;

/// The floor the charged arm must clear: `thaumic` non-zero at at least this
/// fraction of vertices.
///
/// **Measured at 0.305454 (12,512 of 40,962) when this control was written**,
/// so the floor sits an order of magnitude below the observation on purpose.
/// A bare `> 0` would pass on a gate that fired at a single degenerate vertex;
/// a floor at the measured value would be a *golden*, and this is a control,
/// not a calibration — an ordinary retune of `LEY_REACH` or `WELL_RADIUS_RAD`
/// is entitled to move that fraction and must not redden a test whose subject
/// is whether the gate is wired at all.
const FIRES_AT_LEAST: f64 = 0.01;

/// Whether axis `i` of [`AXES`] differs between two buffers. Exact
/// inequality, never a tolerance: the claim under test is byte-identity of
/// the inert substrate, not approximate agreement.
fn axis_differs(a: &MaterialBuffer, b: &MaterialBuffer, i: usize) -> bool {
    match i {
        0 => a.silica != b.silica,
        1 => a.grain != b.grain,
        2 => a.induration != b.induration,
        3 => a.carbonate != b.carbonate,
        4 => a.metamorphic_grade != b.metamorphic_grade,
        5 => a.porosity != b.porosity,
        6 => a.margin != b.margin,
        7 => a.soil_depth.get() != b.soil_depth.get(),
        8 => a.basement != b.basement,
        9 => a.thaumic != b.thaumic,
        _ => unreachable!("AXES has exactly 10 entries"),
    }
}

/// Build `SEED` to [`BuildDepth::Terrain`] under `pins` and hand back the
/// sculpted terrain.
///
/// A sibling of `trencher_probe::world_at` rather than a parameterisation of
/// it: that helper returns the substrate field and reconstructed climate T1's
/// energy arms need and this control reads neither, and T1's measured numbers
/// are committed, so its call sites are left exactly as they were.
fn terrain_at(pins: &TerrainPins, wc: &WorldComponents) -> GeneratedTerrain {
    build_world_to_with_artifacts(
        Seed(SEED),
        &SkyPins::default(),
        pins,
        &SettlementPins::default(),
        wc,
        BuildDepth::Terrain,
    )
    .expect("seed 42 builds")
    .terrain
    .expect("terrain is Some at BuildDepth::Terrain")
}

#[test]
fn the_metaphysics_gate_fires_and_moves_nothing_but_thaumic() {
    let wc = WorldComponents::assemble().expect("canonical registries are well-formed");
    let inert = terrain_at(&TerrainPins::default(), &wc);
    let charged = terrain_at(
        &TerrainPins {
            metaphysics: Some(Metaphysics::Thaumic),
            ..TerrainPins::default()
        },
        &wc,
    );

    let geo = inert.geosphere();
    assert_eq!(
        geo.vertex_count(),
        charged.geosphere().vertex_count(),
        "the two arms must be the same grid"
    );

    let inert_buf: VertexMap<MaterialBuffer> = VertexMap::from_fn(geo, |v| inert.material_at(v));
    let charged_buf: VertexMap<MaterialBuffer> =
        VertexMap::from_fn(geo, |v| charged.material_at(v));

    let mut moved = [0usize; 10];
    let mut elevation_moved = 0usize;
    let mut ocean_moved = 0usize;
    let mut rock_moved = 0usize;
    let mut inert_nonzero_thaumic = 0usize;
    let mut charged_nonzero = 0usize;
    let mut charged_saturated = 0usize;
    let mut charged_on_land = 0usize;
    let mut charged_max = 0.0f64;
    let mut charged_sum = 0.0f64;
    let mut total = 0usize;

    for v in geo.vertices() {
        total += 1;
        let a = inert_buf.get(v);
        let b = charged_buf.get(v);
        for (i, m) in moved.iter_mut().enumerate() {
            if axis_differs(a, b, i) {
                *m += 1;
            }
        }
        if inert.elevation_at(v) != charged.elevation_at(v) {
            elevation_moved += 1;
        }
        if inert.is_ocean(v) != charged.is_ocean(v) {
            ocean_moved += 1;
        }
        if inert.rock_at(v) != charged.rock_at(v) {
            rock_moved += 1;
        }
        if a.thaumic != 0.0 {
            inert_nonzero_thaumic += 1;
        }
        if b.thaumic > 0.0 {
            charged_nonzero += 1;
            charged_sum += b.thaumic;
            if !charged.is_ocean(v) {
                charged_on_land += 1;
            }
            if b.thaumic >= 1.0 {
                charged_saturated += 1;
            }
        }
        if b.thaumic > charged_max {
            charged_max = b.thaumic;
        }
    }

    let frac = |n: usize| n as f64 / total as f64;
    println!("\n=== metaphysics gate: inert vs charged, seed {SEED}, {total} vertices ===");
    println!("{:<20} {:>10} {:>12}", "axis", "moved", "fraction");
    for (i, name) in AXES.iter().enumerate() {
        println!("{:<20} {:>10} {:>12.6}", name, moved[i], frac(moved[i]));
    }
    println!(
        "{:<20} {:>10} {:>12.6}",
        "elevation",
        elevation_moved,
        frac(elevation_moved)
    );
    println!(
        "{:<20} {:>10} {:>12.6}",
        "is_ocean",
        ocean_moved,
        frac(ocean_moved)
    );
    println!(
        "{:<20} {:>10} {:>12.6}",
        "rock",
        rock_moved,
        frac(rock_moved)
    );
    println!("--- charged thaumic ---");
    println!(
        "nonzero            {charged_nonzero} ({:.6})",
        frac(charged_nonzero)
    );
    println!("  of which land    {charged_on_land}");
    println!("saturated (>= 1.0) {charged_saturated}");
    println!("max                {charged_max:.6}");
    println!(
        "mean over nonzero  {:.6}",
        if charged_nonzero == 0 {
            0.0
        } else {
            charged_sum / charged_nonzero as f64
        }
    );
    println!(
        "inert nonzero      {inert_nonzero_thaumic} (readout only; guarded in domains/terrain)"
    );

    for (i, name) in AXES.iter().enumerate() {
        if i == THAUMIC {
            continue;
        }
        assert_eq!(
            moved[i], 0,
            "the metaphysics gate moved `{name}` at {} of {total} vertices; \
             it is licensed to move `thaumic` alone",
            moved[i]
        );
    }
    assert_eq!(elevation_moved, 0, "the gate moved elevation");
    assert_eq!(ocean_moved, 0, "the gate moved is_ocean");
    assert_eq!(rock_moved, 0, "the gate moved rock class");
    assert_eq!(
        inert_nonzero_thaumic, 0,
        "the INERT arm of this comparison lit `thaumic` at {inert_nonzero_thaumic} \
         vertices, so the diff above does not measure the gate firing"
    );
    assert!(
        frac(charged_nonzero) >= FIRES_AT_LEAST,
        "THE GATE DID NOT FIRE: `thaumic` is non-zero at {charged_nonzero} of \
         {total} vertices ({:.6}), below the {FIRES_AT_LEAST} floor",
        frac(charged_nonzero)
    );
}

/// The gate must survive the ledger, and **nothing else pins that it does**.
///
/// `artifacts::hoisted_terrain_equals_the_re_derived_terrain_under_pins`
/// carries a comment claiming this ground — "a charged world that re-derived
/// as an inert one would be a lossy round trip of exactly the kind this test
/// exists to catch" — and it cannot: that test compares `projection`, which
/// is `elevation_at` and `is_ocean` and nothing else. The test above measures
/// that the gate moves *neither* of those at *any* vertex, so the charged and
/// inert re-derivations are identical under that projection by construction.
/// The two statements are mutually exclusive; this one is the one that holds,
/// which leaves the round trip unpinned until here.
///
/// A lossy `pin_strings`/`parse_pin` for `metaphysics` — the pin dropped, or
/// spelled one way and read another — would make `terrain_of` rebuild a
/// charged world as an inert one, silently, and no other assertion in this
/// repository would move.
#[test]
fn a_charged_world_re_derives_from_its_ledger_as_charged() {
    let wc = WorldComponents::assemble().expect("canonical registries are well-formed");
    let pins = TerrainPins {
        metaphysics: Some(Metaphysics::Thaumic),
        ..TerrainPins::default()
    };
    let artifacts = build_world_to_with_artifacts(
        Seed(SEED),
        &SkyPins::default(),
        &pins,
        &SettlementPins::default(),
        &wc,
        BuildDepth::Terrain,
    )
    .expect("seed 42 builds charged");
    let built = artifacts
        .terrain
        .expect("terrain is Some at BuildDepth::Terrain");

    // Decision 0092: a named construction site. `terrain_of` re-sculpts the
    // globe from the committed ledger alone, which is the whole claim under
    // test — the round trip cannot be observed without paying for it.
    #[allow(clippy::disallowed_methods)]
    let rederived = hornvale_worldgen::terrain_of(&artifacts.world)
        .expect("the charged world re-derives from its own ledger");

    let geo = built.geosphere();
    let mut disagreed = 0usize;
    let mut rederived_nonzero = 0usize;
    for v in geo.vertices() {
        let a = built.material_at(v).thaumic;
        let b = rederived.material_at(v).thaumic;
        if a != b {
            disagreed += 1;
        }
        if b > 0.0 {
            rederived_nonzero += 1;
        }
    }
    println!(
        "round trip: {disagreed} of {} vertices disagree on thaumic; \
         {rederived_nonzero} non-zero after re-derivation",
        geo.vertex_count()
    );
    assert_eq!(
        disagreed, 0,
        "the re-derived terrain's thaumic disagrees with the built one at \
         {disagreed} vertices — the metaphysics pin does not survive the ledger"
    );
    assert!(
        rederived_nonzero > 0,
        "the re-derived world is INERT: the metaphysics pin was lost in the \
         pin_strings/parse_pin round trip"
    );
}

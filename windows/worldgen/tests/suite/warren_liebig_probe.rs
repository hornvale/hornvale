//! **Why The Warren's substrate swap is inert** — the evidence behind the
//! falsification `warren_readout.rs` pins, kept as a runnable probe rather
//! than a paragraph, because the next campaign to touch either the tolerance
//! model or the subterranean substrate will want to re-read it rather than
//! trust a summary.
//!
//! `tolerance_liebig` (The Tilth, stage 5) floors temperature, moisture and
//! insolation by the sovereignty floor and calls elevation with floor `0.0`.
//! Its own doc states the consequence: a floored axis can never bind, so the
//! bare one becomes the sole determinant wherever it dips below the others'
//! floor. Elevation is that axis, and `subterranean_substrate` passes
//! `height_asl_m` through unchanged — still true after The Underworld gave a
//! chamber a real depth coordinate, because that depth reaches temperature and
//! moisture and deliberately not the altitude axis.
//!
//! This prints all four terms for rust-monster on cave-bearing vertices, surface
//! against subterranean, and shows the two things that matter together: the
//! three floored axes move — since The Underworld, in BOTH directions, with
//! insolation the only one a chamber wins outright, temperature degrading
//! where the gradient warms past a cool optimum, and moisture rising or
//! falling with the chamber's height above its water table — and the minimum
//! does not move at all.
//!
//! Test fixture (decision 0092): calls the sculpt/fit derivation entry points
//! directly to build its own world state, the same way `warren_gate.rs` and
//! `waterline_probe.rs` do.
#![allow(clippy::disallowed_methods)]

use hornvale_astronomy::SkyPins;
use hornvale_kernel::Seed;
use hornvale_terrain::TerrainPins;
use hornvale_worldgen::{
    SettlementPins, WorldComponents, build_world, climate_of, sky_of, substrate_field,
    subterranean_substrate_field, terrain_of,
};

/// claim: structural(seed: 42) — off-gate (heavy:); false-positive
/// seed-loop flag (Fix round 1); `s` binds a `Substrate` in
/// `for (label, s) in [("surface", surf), ("subterranean", sub)]`
#[test]
#[ignore = "probe: which axis binds for a subterranean kind (Liebig-minimum substrate probe); run by hand (The Warren answered its question; demoted by The Governor 2026-08-28)"]
fn which_axis_binds_for_a_subterranean_kind() {
    let world = build_world(
        Seed(42),
        &SkyPins::default(),
        &TerrainPins::default(),
        &SettlementPins::default(),
    )
    .expect("seed 42 builds");
    let terrain = terrain_of(&world).unwrap();
    let climate = climate_of(&world).unwrap();
    let sky = sky_of(&world).unwrap();
    let system = sky.system().expect("generated system");
    let insolation = hornvale_astronomy::insolation_rel(&system.star, &system.anchor);
    let obliquity = system.anchor.obliquity.get();
    let regime = match system.anchor.rotation {
        hornvale_astronomy::Rotation::Spinning { day, .. } => {
            hornvale_climate::RotationRegime::Spinning {
                day_std: day.as_std_days(),
            }
        }
        hornvale_astronomy::Rotation::Locked => hornvale_climate::RotationRegime::Locked,
    };
    let geo = terrain.geosphere();
    let substrate = substrate_field(geo, &terrain, &climate, obliquity, insolation, &regime);

    let wc = WorldComponents::assemble().unwrap();
    let bio = wc.biosphere.get_by_label("rust-monster").unwrap();
    let cn = &bio.condition_niche;
    let floor_buf = hornvale_kernel::sovereignty_floor(bio.mass, bio.potency);
    println!("rust-monster sovereignty floor = {floor_buf:.6}");

    // The Underworld: the chamber reading now depends on the vertex's own
    // depth, gradient, water table and porosity, so it comes from the one
    // shared derivation rather than from the surface reading alone.
    let subterranean = subterranean_substrate_field(geo, &terrain, &substrate);

    let mut shown = 0;
    for vertex in geo.vertices() {
        if terrain.is_ocean(vertex) || terrain.cave_at(vertex).is_none() {
            continue;
        }
        let surf = *substrate.get(vertex);
        let sub = *subterranean.get(vertex);
        for (label, s) in [("surface", surf), ("subterranean", sub)] {
            let t = cn.temperature.eval(s.temperature_c, floor_buf);
            let m = cn.moisture.eval(s.moisture, floor_buf);
            let i = cn.insolation.eval(s.insolation, floor_buf);
            let e = cn.elevation.eval(s.height_asl_m.get(), 0.0);
            let min = t.min(m).min(i).min(e);
            let which = if min == e {
                "ELEVATION (unfloored)"
            } else if min == i {
                "insolation"
            } else if min == m {
                "moisture"
            } else {
                "temperature"
            };
            println!(
                "  {label:<13} temp={t:.4} moist={m:.4} insol={i:.4} elev={e:.4} -> min={min:.4} [{which}]"
            );
        }
        shown += 1;
        if shown >= 4 {
            break;
        }
    }
}

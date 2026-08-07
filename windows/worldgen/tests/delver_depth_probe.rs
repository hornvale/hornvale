//! THE DELVERS — how deep is a cave, in metres?
//!
//! Measurement only. This exists because the campaign's Task 3b proposes to
//! give a chamber its real elevation —
//! `height_asl_m(chamber) = height_asl_m(surface) - top_depth_m(deepest_band)`
//! — and the spec asserted that was a clean fidelity correction ("a chamber
//! under a 2000 m peak is genuinely not at 2000 m") **without measuring the
//! magnitude**.
//!
//! Reading `domains/terrain/src/strata.rs:157-215` first, the arithmetic is
//! alarming:
//!
//! ```text
//!   moho_m            = crust_thickness_km * 1000.0   crust validated to [0,100] km
//!   Regolith.top      = 0.0
//!   Cover.top         = soil_depth_m
//!   Basement.top      = dtb = soil + sediment
//!   Roots.top         = dtb + (moho_m - dtb) * 0.5
//!   Underneath.top    = moho_m
//! ```
//!
//! An authored elevation curve has `width` around 4000 m. If a cave's
//! `deepest_band` is `Roots`, the chamber sits tens of kilometres down and
//! every elevation curve evaluates to ~0 there. That is arguably *correct* —
//! `BandKind::Roots` is documented as "deep crust: hot, high-pressure", and
//! nobody lives 20 km down — but it decides the campaign, because a
//! subterranean dwarf must still clear `hornvale_demography::FLOOR` on some
//! cell of every seed (`non_void_roster`, which admits no allowlist).
//!
//! So the question this answers is narrow and load-bearing: **over cave-
//! bearing land cells, what is the distribution of `deepest_band`, and what
//! depth in metres does it imply?** If karst and lava-tube caves dominate the
//! shallow bands, the literal read is safe and deep fracture caves are
//! correctly uninhabitable — a real selection effect. If `Roots` dominates,
//! the design needs a different treatment and that is Nathan's call.
//!
//! Nothing here asserts a preferred answer. It prints the distribution.
//!
//! ## Measured, 2026-08-07, seeds 42 / 7 / 1234
//!
//! ```text
//!   band       share of cave cells        depth p50          depth max
//!              s42     s7    s1234
//!   Cover    17.5%  43.9%   26.3%             0.0 m          9 - 10 m
//!   Basement 54.5%  27.2%   40.4%             0.0 m       645 - 1807 m
//!   Roots    28.0%  28.9%   33.4%    13714 - 14774 m     16607 - 21561 m
//!   (Regolith and Underneath are never a cave's deepest band)
//!
//!   resulting underground height_asl_m
//!     p50      +1045    +1482     +368 m
//!     p10    -11808   -12892   -12787 m
//!     min    -13225   -15757   -14762 m
//!   cave cells above -12,000 m:  91.6%   87.0%   81.7%
//! ```
//!
//! **Two findings, and they are not the same finding.**
//!
//! **1. The literal read is safe.** Two thirds of caves (Cover + Basement,
//! 67-72%) sit at metres to hundreds of metres, so the median cave chamber
//! barely moves and `non_void_roster` has ample habitat. The remaining
//! 28-33% are `Roots` caves at 14-21 km, which become correctly
//! uninhabitable — that is a real selection effect and arguably the honest
//! result: `BandKind::Roots` is "deep crust: hot, high-pressure".
//!
//! **2. It probably does NOT separate Mountain from Duergar, which was the
//! reason for building it.** Among the shallow caves that remain habitable,
//! the median depth is **0.0 m** on every seed — the variation lives in a
//! thin tail reaching 645-1807 m. Two kinds differing only in an authored
//! elevation optimum will therefore score almost identically across most of
//! their shared habitat, because most of that habitat is at the same depth.
//! P2' (the two correlate below 0.95) is at genuine risk, and the risk was
//! invisible until these percentiles existed.

// `terrain_of` is a named derivation entry point (decision 0092); a probe
// measuring a handful of worlds is exactly the site the allowance is for.
#![allow(clippy::disallowed_methods)]

use hornvale_terrain::BandKind;
use hornvale_worldgen::{
    SettlementPins, SkyChoice, build_world, climate_of, sky_of, substrate_field, terrain_of,
};

/// Seeds the campaign states its preregistrations on.
const SEEDS: [u64; 3] = [42, 7, 1234];

/// Name a band for the table, in column order (top to bottom).
fn band_name(b: BandKind) -> &'static str {
    match b {
        BandKind::Regolith => "Regolith",
        BandKind::Cover => "Cover",
        BandKind::Basement => "Basement",
        BandKind::Roots => "Roots",
        BandKind::Underneath => "Underneath",
    }
}

/// Percentile of an ascending slice.
fn pct(sorted: &[f64], q: f64) -> f64 {
    if sorted.is_empty() {
        return f64::NAN;
    }
    let i = ((sorted.len() - 1) as f64 * q).round() as usize;
    sorted[i]
}

#[test]
#[ignore = "heavy: live-worldgen battery (minutes); deferred from the commit gate to make gate-full"]
fn how_deep_is_a_cave() {
    for seed_value in SEEDS {
        let seed = hornvale_kernel::Seed(seed_value);
        let world = build_world(
            seed,
            &hornvale_astronomy::SkyPins::default(),
            SkyChoice::Generated,
            &hornvale_terrain::TerrainPins::default(),
            &SettlementPins::default(),
        )
        .expect("probe seed builds");
        let terrain = terrain_of(&world).expect("terrain");
        let climate = climate_of(&world).expect("climate");
        let geo = terrain.geosphere();
        // `height_asl_m` is what `tolerance_liebig` actually reads — the raw
        // `elevation_at` is the isostatic reading, which The Benchmark moved
        // the suitability layer OFF. Read the substrate, not the terrain.
        let sky = sky_of(&world).expect("sky");
        let generated = match &sky {
            hornvale_worldgen::Sky::Generated(g) => g,
            _ => panic!("probe expects a generated sky"),
        };
        let system = generated.system();
        let insolation_scalar = hornvale_astronomy::insolation_rel(&system.star, &system.anchor);
        let obliquity_deg = system.anchor.obliquity.get();
        let regime = match system.anchor.rotation {
            hornvale_astronomy::Rotation::Spinning { day, .. } => {
                hornvale_climate::RotationRegime::Spinning { day_std: day.get() }
            }
            hornvale_astronomy::Rotation::Locked => hornvale_climate::RotationRegime::Locked,
        };
        let substrate = substrate_field(
            geo,
            &terrain,
            &climate,
            obliquity_deg,
            insolation_scalar,
            &regime,
        );

        // Per band: how many cave-bearing land cells reach it, and the depths.
        let mut counts: [usize; 5] = [0; 5];
        let mut depths: [Vec<f64>; 5] = Default::default();
        // The resulting underground elevation, over all cave cells.
        let mut underground_asl: Vec<f64> = Vec::new();
        let mut land = 0usize;
        let mut cave_cells = 0usize;

        for cell in geo.cells() {
            if terrain.is_ocean(cell) {
                continue;
            }
            land += 1;
            let Some(cave) = terrain.cave_at(cell) else {
                continue;
            };
            cave_cells += 1;
            let column = terrain.column_at(cell);
            let band = column
                .bands
                .iter()
                .find(|b| b.kind == cave.deepest_band)
                .expect("deepest_band is one of the five column bands");
            let idx = match cave.deepest_band {
                BandKind::Regolith => 0,
                BandKind::Cover => 1,
                BandKind::Basement => 2,
                BandKind::Roots => 3,
                BandKind::Underneath => 4,
            };
            counts[idx] += 1;
            depths[idx].push(band.top_depth_m);
            let surface_asl = substrate.get(cell).height_asl_m.get();
            underground_asl.push(surface_asl - band.top_depth_m);
        }

        println!("\n== seed {seed_value} ==  land {land}  cave-bearing {cave_cells}");
        println!(
            "  {:<11} {:>7} {:>7}   {:>12} {:>12} {:>12}",
            "band", "cells", "share", "depth p50", "depth p90", "depth max"
        );
        for idx in 0..5 {
            if counts[idx] == 0 {
                continue;
            }
            let kind = [
                BandKind::Regolith,
                BandKind::Cover,
                BandKind::Basement,
                BandKind::Roots,
                BandKind::Underneath,
            ][idx];
            let mut d = depths[idx].clone();
            d.sort_by(f64::total_cmp);
            let share = counts[idx] as f64 / cave_cells.max(1) as f64;
            println!(
                "  {:<11} {:>7} {:>6.2}%   {:>12.1} {:>12.1} {:>12.1}",
                band_name(kind),
                counts[idx],
                share * 100.0,
                pct(&d, 0.50),
                pct(&d, 0.90),
                d.last().copied().unwrap_or(f64::NAN),
            );
        }

        underground_asl.sort_by(f64::total_cmp);
        println!(
            "  resulting underground height_asl_m:  p10 {:.1}  p50 {:.1}  p90 {:.1}  min {:.1}",
            pct(&underground_asl, 0.10),
            pct(&underground_asl, 0.50),
            pct(&underground_asl, 0.90),
            underground_asl.first().copied().unwrap_or(f64::NAN),
        );

        // How many cave cells would remain within reach of an elevation curve
        // of the shape the roster actually authors (width 4000 m)? Beyond
        // about 3 sigma from any plausible optimum the curve is numerically
        // dead, so this counts cells no authored curve could rescue.
        let reachable = underground_asl.iter().filter(|v| **v > -12_000.0).count();
        println!(
            "  cave cells above -12,000 m (3 sigma of a width-4000 curve centred near 0): \
             {reachable} / {cave_cells}  ({:.2}%)",
            reachable as f64 / cave_cells.max(1) as f64 * 100.0
        );
    }
}

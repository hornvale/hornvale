//! The Hollow's measuring instrument: the five numbers the campaign moves.
//!
//! Deliberately a REPORT, not a judgement — the preregistered criteria live in
//! `cave_substrate_meets_preregistered_criteria` (Task 5) and in the spec's §4
//! table. This battery exists so the baseline and the readout are produced by
//! the identical code path.
//!
//! **Land** is `!terrain.is_ocean(cell)` — the predicate `cave_at` itself gates
//! on internally, so no second land test is introduced.
//!
//! Built to `BuildDepth::Terrain`, the shallowest rung producing terrain:
//! caves are a terrain-only feature and nothing here reads climate or
//! settlements.
//!
//! Test fixture (decision 0092): calls the derivation entry point directly,
//! the sanctioned test-fixture posture.
#![allow(clippy::disallowed_methods)]

use hornvale_astronomy::SkyPins;
use hornvale_kernel::{CellId, Seed};
use hornvale_terrain::{BandKind, CaveKind, TerrainPins};
use hornvale_worldgen::{
    BuildDepth, SettlementPins, SkyChoice, WorldComponents, build_world_to_with_artifacts,
};
use std::collections::BTreeSet;

/// Seeds measured. Matches C2a's `deep_realm_substrate.rs` so the two
/// campaigns' numbers are directly comparable.
const SEEDS: std::ops::RangeInclusive<u64> = 1..=30;

/// The `presence_prob` buckets the gate-calibration readout reports, as
/// `[low, high)` pairs. Chosen to match the spec's §2.3 table exactly.
const PROB_BUCKETS: [(f64, f64); 6] = [
    (0.00, 0.05),
    (0.20, 0.25),
    (0.25, 0.30),
    (0.30, 0.35),
    (0.35, 0.40),
    (0.40, 0.45),
];

/// Everything the campaign measures, accumulated over all seeds.
#[derive(Default)]
struct Readout {
    /// Worlds measured.
    worlds: usize,
    /// Worlds with no cave at all.
    caveless_worlds: usize,
    /// Land cells (`!is_ocean`) across all worlds.
    land: usize,
    /// Land cells carrying a cave.
    caves: usize,
    /// Per-world cave fraction of land, one entry per seed.
    per_world_fraction: Vec<f64>,
    /// Cave cells by kind, in `CaveKind` declaration order.
    kinds: [usize; 3],
    /// Cave cells by `deepest_band`, in `BandKind` declaration order
    /// (Regolith, Cover, Basement, Roots, Underneath).
    bands: [usize; 5],
    /// Cave cells with at least one caved neighbour.
    clustered: usize,
    /// Cave cells with no caved neighbour.
    solitary: usize,
    /// Per `PROB_BUCKETS` entry: (land cells in bucket, caves in bucket).
    gate: [(usize, usize); 6],
}

/// Verbatim copies of `features::belt_weight` / `presence_prob`, which are
/// `pub` inside `hornvale_terrain` but not re-exported from its crate root.
/// If either formula changes, this harness must change with it — the gate
/// readout is meaningless otherwise.
fn belt_weight(hops: Option<u32>) -> f64 {
    match hops {
        Some(h) => (1.0 / (1.0 + h as f64 * 0.1)).max(0.3),
        None => 0.3,
    }
}

/// See [`belt_weight`].
fn presence_prob(field: f64, belt: f64) -> f64 {
    (field * (0.4 + 0.6 * belt)).clamp(0.0, 1.0)
}

/// Build one seed to `BuildDepth::Terrain` and fold its land cells into `out`.
fn measure_one(seed: Seed, wc: &WorldComponents, out: &mut Readout) {
    let artifacts = build_world_to_with_artifacts(
        seed,
        &SkyPins::default(),
        SkyChoice::Generated,
        &TerrainPins::default(),
        &SettlementPins::default(),
        wc,
        BuildDepth::Terrain,
    )
    .unwrap_or_else(|e| panic!("{seed:?} failed to build: {e:?}"));
    let terrain = artifacts
        .terrain
        .unwrap_or_else(|| panic!("{seed:?} at BuildDepth::Terrain produced no terrain"));
    let geo = terrain.geosphere();

    let mut cave_set: BTreeSet<CellId> = BTreeSet::new();
    let (mut world_land, mut world_caves) = (0usize, 0usize);

    for cell in geo.cells() {
        if terrain.is_ocean(cell) {
            continue;
        }
        world_land += 1;

        let prob = presence_prob(
            terrain.cave_proneness_at(cell),
            belt_weight(terrain.boundary_distance_at(cell)),
        );
        let bucket = PROB_BUCKETS
            .iter()
            .position(|&(lo, hi)| prob >= lo && prob < hi);

        let cave = terrain.cave_at(cell);
        if let Some(cave) = cave {
            world_caves += 1;
            cave_set.insert(cell);
            out.kinds[match cave.kind {
                CaveKind::Karst => 0,
                CaveKind::LavaTube => 1,
                CaveKind::Fracture => 2,
            }] += 1;
            out.bands[match cave.deepest_band {
                BandKind::Regolith => 0,
                BandKind::Cover => 1,
                BandKind::Basement => 2,
                BandKind::Roots => 3,
                BandKind::Underneath => 4,
            }] += 1;
        }
        if let Some(b) = bucket {
            out.gate[b].0 += 1;
            if cave.is_some() {
                out.gate[b].1 += 1;
            }
        }
    }

    for &cell in &cave_set {
        if geo.neighbors(cell).iter().any(|nb| cave_set.contains(nb)) {
            out.clustered += 1;
        } else {
            out.solitary += 1;
        }
    }

    out.worlds += 1;
    out.land += world_land;
    out.caves += world_caves;
    if world_caves == 0 {
        out.caveless_worlds += 1;
    }
    out.per_world_fraction.push(if world_land == 0 {
        0.0
    } else {
        world_caves as f64 / world_land as f64
    });
}

/// Measure every seed in `SEEDS`.
fn measure() -> Readout {
    let wc = WorldComponents::assemble().expect("canonical registries are well-formed");
    let mut out = Readout::default();
    for seed in SEEDS {
        measure_one(Seed(seed), &wc, &mut out);
    }
    out
}

/// Print the five numbers, in the spec's §4 order.
fn report(r: &Readout) {
    println!(
        "== The Hollow readout — {} worlds, {} land cells",
        r.worlds, r.land
    );
    println!(
        "prevalence: {} caves = {:.4}% of land; {} of {} worlds have NO cave",
        r.caves,
        100.0 * r.caves as f64 / r.land as f64,
        r.caveless_worlds,
        r.worlds
    );

    let mut sorted = r.per_world_fraction.clone();
    sorted.sort_by(f64::total_cmp);
    let pct = |q: f64| -> f64 {
        if sorted.is_empty() {
            return 0.0;
        }
        let idx = ((sorted.len() - 1) as f64 * q) as usize;
        sorted[idx]
    };
    println!(
        "per-world cave fraction: p50={:.5} p90={:.5} max={:.5}",
        pct(0.5),
        pct(0.9),
        pct(1.0)
    );

    let names = ["Karst", "LavaTube", "Fracture"];
    for (i, name) in names.iter().enumerate() {
        println!(
            "kind {name}: {} ({:.4}% of caves)",
            r.kinds[i],
            if r.caves == 0 {
                0.0
            } else {
                100.0 * r.kinds[i] as f64 / r.caves as f64
            }
        );
    }

    let bands = ["Regolith", "Cover", "Basement", "Roots", "Underneath"];
    for (i, name) in bands.iter().enumerate() {
        println!(
            "band {name}: {} ({:.4}% of caves)",
            r.bands[i],
            if r.caves == 0 {
                0.0
            } else {
                100.0 * r.bands[i] as f64 / r.caves as f64
            }
        );
    }

    let placed = r.clustered + r.solitary;
    println!(
        "clustering: {} clustered / {} solitary = {:.4}%",
        r.clustered,
        r.solitary,
        if placed == 0 {
            0.0
        } else {
            100.0 * r.clustered as f64 / placed as f64
        }
    );

    println!("gate calibration — nominal presence_prob vs realized hit rate:");
    for (i, &(lo, hi)) in PROB_BUCKETS.iter().enumerate() {
        let (cells, hits) = r.gate[i];
        if cells == 0 {
            continue;
        }
        println!(
            "  [{lo:.2},{hi:.2})  cells={cells:>8}  caves={hits:>7}  realized={:.5}  nominal~{:.3}",
            hits as f64 / cells as f64,
            (lo + hi) / 2.0
        );
    }
}

#[test]
fn report_cave_substrate() {
    let r = measure();
    report(&r);
    assert!(
        r.land > 0,
        "the harness found no land cells — it is measuring nothing"
    );
    assert_eq!(r.worlds, 30, "expected 30 worlds");
}

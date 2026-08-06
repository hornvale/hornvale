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
/// `[low, high)` pairs, exhaustive over `[0, 1)` in 0.05-wide bins.
///
/// The original six bins were the spec's §2.3 table, which was exhaustive over
/// land only because the pre-campaign gate read a single field (`Karst`
/// proneness) whose land distribution happened to be bimodal. The gate now
/// reads whichever process `cave_process` selects, whose range is the union of
/// three, so a partial table would silently drop cells out of the readout.
const PROB_BUCKETS: [(f64, f64); 20] = [
    (0.00, 0.05),
    (0.05, 0.10),
    (0.10, 0.15),
    (0.15, 0.20),
    (0.20, 0.25),
    (0.25, 0.30),
    (0.30, 0.35),
    (0.35, 0.40),
    (0.40, 0.45),
    (0.45, 0.50),
    (0.50, 0.55),
    (0.55, 0.60),
    (0.60, 0.65),
    (0.65, 0.70),
    (0.70, 0.75),
    (0.75, 0.80),
    (0.80, 0.85),
    (0.85, 0.90),
    (0.90, 0.95),
    (0.95, 1.00),
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
    /// Per `PROB_BUCKETS` entry: (land cells in bucket, caves in bucket, sum
    /// of those cells' nominal probabilities). The third element makes the
    /// bucket's *mean* nominal readable alongside its midpoint — they differ
    /// whenever a bucket's interior distribution is not uniform, which the
    /// `[0.00,0.05)` bucket's mass at exactly zero guarantees.
    gate: [(usize, usize, f64); 20],
    /// Land cells whose nominal probability fell outside every bucket, i.e.
    /// exactly 1.0. Reported so "exhaustive over land" stays checkable.
    unbucketed: usize,
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

        // The proneness the GATE reads — `cave_process`'s selected process,
        // not `cave_proneness_at` (which is the Karst term alone). Bucketing
        // on the Karst term was correct only while the gate read it; since the
        // gate became kind-first, a Fracture or LavaTube cave was being
        // credited to a probability that never gated it.
        let selected = hornvale_terrain::cave_process(
            &terrain.material_at(cell),
            terrain.drainage_at(cell),
            terrain.crust_age_at(cell),
            terrain.nearest_boundary_at(cell),
        );
        // No supporting process is proneness zero: the cell cannot host a cave
        // and its nominal probability is zero, which is a real bucket entry.
        let proneness = selected.map_or(0.0, |(_, p)| p);
        let prob = presence_prob(proneness, belt_weight(terrain.boundary_distance_at(cell)));
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
        match bucket {
            Some(b) => {
                out.gate[b].0 += 1;
                if cave.is_some() {
                    out.gate[b].1 += 1;
                }
                out.gate[b].2 += prob;
            }
            None => out.unbucketed += 1,
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

    let bucketed: usize = r.gate.iter().map(|&(c, _, _)| c).sum();
    println!(
        "gate calibration — nominal presence_prob vs realized hit rate \
         ({bucketed} of {} land cells bucketed, {} outside every bucket):",
        r.land, r.unbucketed
    );
    for (i, &(lo, hi)) in PROB_BUCKETS.iter().enumerate() {
        let (cells, hits, prob_sum) = r.gate[i];
        if cells == 0 {
            continue;
        }
        println!(
            "  [{lo:.2},{hi:.2})  cells={cells:>8}  caves={hits:>7}  realized={:.5}  \
             mid={:.3}  mean-nominal={:.5}",
            hits as f64 / cells as f64,
            (lo + hi) / 2.0,
            prob_sum / cells as f64
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

/// The spec's §4 preregistered criteria, frozen at commit `2808f59d` before
/// any behavioural change. A failure here is a finding, not a defect to be
/// tuned away: see the campaign chronicle before touching a threshold.
///
/// Every threshold below is copied verbatim from the §4 table. One thing is
/// **not** verbatim and is disclosed here: H4 compares the realized hit rate
/// against each bucket's **mean** nominal probability, where the plan's Task 5
/// wrote the bucket's midpoint. §4 says only `|realized - nominal| / nominal <
/// 0.25`; the midpoint is a fair estimator of a bucket's nominal only when the
/// probabilities inside it are spread evenly, which was true of the plan's six
/// hand-picked bins over a bimodal field and is false of the exhaustive
/// `[0,1)` table this harness now carries. `[0.00,0.05)` holds ~64% of all
/// land, massed near zero, and its mean nominal is 0.0148 against a midpoint of
/// 0.025 — so the midpoint reading reports a 39% miss where the gate is in fact
/// firing at 0.01525 against a true nominal of 0.01484, an agreement of 2.8%.
/// The threshold is untouched; the estimator is corrected, for the same reason
/// the bucketing itself was corrected in `56881b5f`.
#[test]
fn cave_substrate_meets_preregistered_criteria() {
    let r = measure();
    report(&r);

    // H1 — every kind occurs at >= 5% of cave cells.
    let names = ["Karst", "LavaTube", "Fracture"];
    for (i, name) in names.iter().enumerate() {
        let share = r.kinds[i] as f64 / r.caves as f64;
        assert!(
            share >= 0.05,
            "H1: {name} is {share:.4} of caves, under the 0.05 floor"
        );
    }

    // H2 — at least 3 distinct bands occur, and the mode is under 90%.
    let distinct = r.bands.iter().filter(|&&c| c > 0).count();
    assert!(
        distinct >= 3,
        "H2: only {distinct} distinct depth bands occur"
    );
    let modal = *r.bands.iter().max().expect("five bands") as f64 / r.caves as f64;
    assert!(modal < 0.90, "H2: the modal band holds {modal:.4} of caves");

    // H3 — prevalence off the floor, with an absurd-high ceiling.
    assert_eq!(
        r.caveless_worlds, 0,
        "H3: {} worlds have no cave",
        r.caveless_worlds
    );
    let mut sorted = r.per_world_fraction.clone();
    sorted.sort_by(f64::total_cmp);
    let median = sorted[sorted.len() / 2];
    assert!(
        median >= 0.02,
        "H3: median cave fraction {median:.4} is below 0.02"
    );
    assert!(
        median <= 0.5,
        "H3: median cave fraction {median:.4} is absurdly high"
    );

    // H4 — realized hit rate tracks nominal probability.
    for (i, &(lo, hi)) in PROB_BUCKETS.iter().enumerate() {
        let (cells, hits, prob_sum) = r.gate[i];
        if cells < 500 {
            continue; // too few samples for a rate to mean anything
        }
        let realized = hits as f64 / cells as f64;
        let nominal = prob_sum / cells as f64;
        assert!(
            (realized - nominal).abs() / nominal < 0.25,
            "H4: bucket [{lo:.2},{hi:.2}) realized {realized:.5} against nominal {nominal:.5}"
        );
    }

    // H5 — GUARD. Clustering must survive the monotone warp. If this fails,
    // the warp was not monotone or fbm's spatial structure did not survive it,
    // and spec §3.2's central claim is false.
    let placed = r.clustered + r.solitary;
    let clustered = r.clustered as f64 / placed as f64;
    assert!(
        clustered >= 0.90,
        "H5: clustering fell to {clustered:.4}, under the 0.90 guard"
    );
}

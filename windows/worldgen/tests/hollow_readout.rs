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

/// How many classes the restated H2 partitions the depth budget into.
///
/// **Five, deliberately** — the same cardinality as the five `BandKind` rungs
/// the original H2 counted, so the restatement changes the *classifier* and
/// nothing else. A finer partition would make "at least 3 occupied" easier to
/// satisfy, which would be a widening; this is not one.
const REACH_BINS: usize = 5;

/// Which of the [`REACH_BINS`] equal-width classes a depth budget falls in.
///
/// The partition is over `[0, CAVE_REACH_CEILING_M]` — the range
/// `cave_depth_reach_m` itself declares and clamps to — read from the terrain
/// crate rather than duplicated here, so the classifier cannot drift from the
/// quantity it classifies. A reach exactly at the ceiling lands in the last
/// bin.
fn reach_bin(reach_m: f64) -> usize {
    let width = hornvale_terrain::CAVE_REACH_CEILING_M / REACH_BINS as f64;
    ((reach_m / width) as usize).min(REACH_BINS - 1)
}

/// How many of `bins` are occupied, and the modal bin's share of the total.
/// The two statistics the restated H2 asserts on — extracted so the criterion's
/// arithmetic can be exercised against a synthetic distribution without
/// building thirty worlds (see
/// `the_restated_h2_rejects_a_collapsed_and_a_two_valued_depth`).
fn variety_of(bins: &[usize; REACH_BINS]) -> (usize, f64) {
    let total: usize = bins.iter().sum();
    let occupied = bins.iter().filter(|&&c| c > 0).count();
    let modal = if total == 0 {
        0.0
    } else {
        *bins.iter().max().expect("REACH_BINS is non-empty") as f64 / total as f64
    };
    (occupied, modal)
}

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
    /// Worlds in which each kind occurs at least once, same order. This is
    /// the reachability signal H1 actually cares about: The Hollow's defect
    /// was `LavaTube` and `Fracture` being UNREACHABLE, which is a statement
    /// about worlds, not about a share of a pooled total.
    kind_worlds: [usize; 3],
    /// Cave cells by `deepest_band`, in `BandKind` declaration order
    /// (Regolith, Cover, Basement, Roots, Underneath). REPORTED, and no
    /// longer asserted on — see H2's disclosure in
    /// `cave_substrate_meets_preregistered_criteria`.
    bands: [usize; 5],
    /// Cave cells by [`reach_bin`] — the restated H2's classifier. Five equal
    /// bins over the depth budget's own declared range.
    reach_bins: [usize; REACH_BINS],
    /// Every cave's `depth_reach_m`, for the distributional readout H2's
    /// restatement rests on. Unsorted until `report` sorts a copy.
    reaches: Vec<f64>,
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
    /// The same triples, kept PER WORLD — one entry per seed. H4's variance
    /// model needs the world as its sampling unit, not the cell: the gate
    /// reads a smooth spatial field, so cells within a world are nowhere near
    /// independent draws. See `cave_substrate_meets_preregistered_criteria`.
    gate_per_world: Vec<[(usize, usize, f64); 20]>,
    /// Land cells whose nominal probability fell outside every bucket, i.e.
    /// exactly 1.0. Reported so "exhaustive over land" stays checkable.
    unbucketed: usize,
}

/// Cluster-robust standard error of a sum of per-world residuals, treating
/// each world as one independent cluster: `SE = sqrt(W * s^2)` for `s^2` the
/// sample variance of the `W` residuals. Returns `None` below two worlds,
/// where a variance is undefined.
fn cluster_robust_se(residuals: &[f64]) -> Option<f64> {
    let w = residuals.len();
    if w < 2 {
        return None;
    }
    let mean = residuals.iter().sum::<f64>() / w as f64;
    let s2 = residuals.iter().map(|r| (r - mean).powi(2)).sum::<f64>() / (w - 1) as f64;
    Some((w as f64 * s2).sqrt())
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
    let mut world_kinds = [0usize; 3];
    let mut world_gate = [(0usize, 0usize, 0.0f64); 20];

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
            let ki = match cave.kind {
                CaveKind::Karst => 0,
                CaveKind::LavaTube => 1,
                CaveKind::Fracture => 2,
            };
            out.kinds[ki] += 1;
            world_kinds[ki] += 1;
            out.bands[match cave.deepest_band {
                BandKind::Regolith => 0,
                BandKind::Cover => 1,
                BandKind::Basement => 2,
                BandKind::Roots => 3,
                BandKind::Underneath => 4,
            }] += 1;
            out.reach_bins[reach_bin(cave.depth_reach_m)] += 1;
            out.reaches.push(cave.depth_reach_m);
        }
        match bucket {
            Some(b) => {
                out.gate[b].0 += 1;
                world_gate[b].0 += 1;
                if cave.is_some() {
                    out.gate[b].1 += 1;
                    world_gate[b].1 += 1;
                }
                out.gate[b].2 += prob;
                world_gate[b].2 += prob;
            }
            None => out.unbucketed += 1,
        }
    }
    out.gate_per_world.push(world_gate);

    for &cell in &cave_set {
        if geo.neighbors(cell).iter().any(|nb| cave_set.contains(nb)) {
            out.clustered += 1;
        } else {
            out.solitary += 1;
        }
    }

    for (k, &seen) in world_kinds.iter().enumerate() {
        if seen > 0 {
            out.kind_worlds[k] += 1;
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
            "kind {name}: occurs in {}/{} worlds; {} ({:.4}% of caves)",
            r.kind_worlds[i],
            r.worlds,
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

    // The depth coordinate itself (The Underworld, spec §4.0) — the quantity
    // the restated H2 asserts on, and the independent evidence that the
    // property H2 protects is intact whatever the band histogram above says.
    let mut reaches = r.reaches.clone();
    reaches.sort_by(f64::total_cmp);
    let rpct = |q: f64| -> f64 {
        if reaches.is_empty() {
            return f64::NAN;
        }
        reaches[((reaches.len() - 1) as f64 * q) as usize]
    };
    let mut distinct: Vec<f64> = reaches.clone();
    distinct.dedup_by(|a, b| a == b);
    // How often the ceiling actually binds. Reported because a clamp that
    // binds often is shaping the distribution rather than merely bounding it,
    // and `CAVE_REACH_CEILING_M`'s own doc claims the latter.
    let at_ceiling = reaches
        .iter()
        .filter(|r| **r >= hornvale_terrain::CAVE_REACH_CEILING_M)
        .count();
    println!(
        "depth reach m: n={} distinct={} p05={:.1} p25={:.1} p50={:.1} p75={:.1} p95={:.1} max={:.1} at-ceiling={at_ceiling}",
        reaches.len(),
        distinct.len(),
        rpct(0.05),
        rpct(0.25),
        rpct(0.50),
        rpct(0.75),
        rpct(0.95),
        rpct(1.0)
    );
    let (occupied, modal) = variety_of(&r.reach_bins);
    let width = hornvale_terrain::CAVE_REACH_CEILING_M / REACH_BINS as f64;
    for (i, &count) in r.reach_bins.iter().enumerate() {
        println!(
            "reach bin [{:.0}, {:.0}) m: {count} ({:.4}% of caves)",
            i as f64 * width,
            (i + 1) as f64 * width,
            if r.caves == 0 {
                0.0
            } else {
                100.0 * count as f64 / r.caves as f64
            }
        );
    }
    println!("depth variety: {occupied}/{REACH_BINS} bins occupied, modal {modal:.4}");

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

    // H1 — every kind is REACHABLE. Restated by The Glasshouse (decision
    // 0135), on Nathan's explicit authorisation, from a pooled share floor to
    // a per-world reachability claim. Recording why, because changing a
    // preregistered criterion after seeing a result is exactly what decision
    // 0016 forbids when it is done to rescue one:
    //
    // The Hollow's defect was that `LavaTube` and `Fracture` were UNREACHABLE
    // — its own H1 row reads "Karst 100%, others 0%". The 5% pooled share was
    // a PROXY for reachability, and it is a proxy that breaks when the mix
    // legitimately moves: The Glasshouse's terrain epoch raised mean land
    // crust 25.73 -> 29.87 km, leaving less low-silica volcanic substrate, and
    // lava tubes became fracture caves. Measured over the same 30 seeds:
    //
    //     kind        pre-epoch            post-epoch
    //     Karst       22846 (40.84%)       21027 (43.52%)
    //     LavaTube     9837 (17.58%)        2379 ( 4.92%)
    //     Fracture    23264 (41.58%)       24910 (51.56%)
    //     caves       55947 (11.93% land)  48316 (10.21% land)
    //
    // A SUBSTITUTION, not a decline: caves barely moved and Fracture gained
    // what LavaTube lost. And `LavaTube` still occurs in **30 of 30 worlds**,
    // so the property H1 exists to protect is comprehensively intact while its
    // proxy reads failure.
    //
    // 30/30 is not a threshold fitted to this data — it is the maximum, and
    // the definitional statement of reachability. It is STRICTLY STRONGER than
    // the old floor at detecting The Hollow's actual defect: a kind confined
    // to a few worlds passes a pooled share test and fails this one.
    let names = ["Karst", "LavaTube", "Fracture"];
    for (i, name) in names.iter().enumerate() {
        assert_eq!(
            r.kind_worlds[i], r.worlds,
            "H1: {name} occurs in only {}/{} worlds — a cave kind has become \
             unreachable somewhere, which is the defect this criterion exists \
             to catch",
            r.kind_worlds[i], r.worlds
        );
    }
    // H1b — anti-collapse backstop. Deliberately MUCH weaker than the retired
    // 5%: the mix is legitimately world-dependent, so a share floor cannot be
    // a reachability test. This only catches a kind present everywhere but
    // vanishingly thin (30 worlds x 1 cell would pass H1 alone). 1% sits 5x
    // under the measured 4.92%, so it is a backstop, not a calibration.
    for (i, name) in names.iter().enumerate() {
        let share = r.kinds[i] as f64 / r.caves as f64;
        assert!(
            share >= 0.01,
            "H1b: {name} is {share:.4} of caves — present in every world but \
             vanishingly thin"
        );
    }

    // H2 — the depth coordinate must not collapse. RESTATED by The Underworld
    // (spec §4.0) under decision 0138, which permits restatement only when the
    // property is independently verified intact, the estimator's defect is
    // DEMONSTRATED rather than asserted, and the restated criterion is
    // re-proved against the defect the original existed to catch. All three
    // are discharged; this comment is the disclosure 0138 requires.
    //
    // WHAT THE ORIGINAL SAID. "At least 3 distinct `BandKind`s occur among
    // caves' `deepest_band`, and the modal band holds under 90%." Its property
    // is The Hollow's spec §2.2 defect: `depth_reach_bands` was arithmetically
    // incapable of returning anything but 2, so "every cave in every world sat
    // at band 2" — a depth coordinate collapsed to one value.
    //
    // WHY THE ESTIMATOR WAS INVALID. `deepest_band` was not a measurement of
    // the world; it was the return value of a three-armed match on
    // `(kind, proneness >= 0.5, column.unconformity)`. Its range over its
    // ENTIRE input domain is exactly {Cover, Basement, Roots} — three values,
    // by enumeration of the arms, with no reference to any world. So "at least
    // 3 distinct bands" was satisfied by the generator's arity alone, and
    // would have passed over a world whose every cave had an identical depth.
    // A criterion that a constant-depth world satisfies cannot be testing
    // whether depth collapsed. This is demonstrated, not argued, by
    // `the_retired_h2_estimator_is_satisfied_by_its_own_generator` below, which
    // holds the column fixed so the world contributes nothing.
    //
    // The band histogram then went 2-valued not because depth collapsed but
    // because depth became REAL: a budget in metres capped at 3 km cannot reach
    // `Roots` (~14 km) or `Underneath` (~28 km), and `depth_to_basement_m` is 0
    // over most land so `Basement`'s top is 0 m and absorbs nearly everything.
    // The band lookup is now archival — which rungs a void penetrates — and
    // spec §4.0 states in as many words that it is no longer the depth
    // coordinate. Asserting depth variety on it is a category error.
    //
    // WHAT THE RESTATEMENT MEASURES. The same two statistics, with the same two
    // thresholds (3 and 0.90, both untouched), over the same number of classes
    // (5, see REACH_BINS), applied to the depth coordinate itself: five
    // equal-width bins over `cave_depth_reach_m`'s own declared range. Only the
    // classifier changed. It cannot be satisfied by the generator's arity the
    // way the original was, because `cave_depth_reach_m` is a continuous
    // function with no finite range of outputs.
    //
    // RESOLUTION, and where it is blind (0138's second consequence): with five
    // 600 m bins this resolves a collapse of the budget onto fewer than three
    // 600 m-wide classes. A distribution spread across three bins but degenerate
    // *within* them would pass; the honest way to sharpen that is finer bins
    // against more worlds, not a different threshold.
    let (occupied, modal) = variety_of(&r.reach_bins);
    assert!(
        occupied >= 3,
        "H2: the depth budget occupies only {occupied} of {REACH_BINS} classes \
         ({:?}) — the depth coordinate has collapsed",
        r.reach_bins
    );
    assert!(
        modal < 0.90,
        "H2: the modal depth class holds {modal:.4} of caves ({:?})",
        r.reach_bins
    );

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
    //
    // Restated by The Glasshouse (decision 0138, the same record as H1's), on Nathan's
    // explicit authorisation, from a bare 0.25 relative bound to that SAME
    // bound conjoined with a CLUSTER-ROBUST significance test. Recording why
    // in full, because changing a preregistered criterion after seeing a
    // result is exactly what decision 0016 forbids when it is done to rescue
    // one — and this is the second such restatement in this campaign, after
    // H1's.
    //
    // WHAT WAS WRONG. The 0.25 bound is a claim about a rate's precision, and
    // it was being applied as though the bucket's CELLS were independent
    // Bernoulli draws. They are not, and H5 four lines below is the proof:
    // it ASSERTS >=90% clustering, i.e. that the gate field is spatially
    // smooth by design. A subpopulation's effective sample size is therefore
    // set by how many independent noise regions it occupies (tens), not by
    // its cell count (thousands). Measured overdispersion across the 30
    // worlds is chi2/df = 5.5-48.8 in EVERY bucket, against 1.0 for
    // independent cells. H4 and H5 were in direct tension and H5 is the one
    // stating the intended physics.
    //
    // WHAT THE FAILURE ACTUALLY WAS. Buckets [0.05,0.10) and [0.15,0.20) are
    // ~93% LavaTube (Karst is exactly 0 in both, in all 30 worlds), so they
    // are the population the terrain epoch thinned by 76% (9837 -> 2379).
    // What remained was dominated by ONE world: seed 3 supplied 36% and 30%
    // of their cells, and 90% and >100% of their excess — the other 29 worlds
    // are collectively NEGATIVE in the second bucket. Excluding seed 3 the
    // buckets read +8.2% and -14.5%. Under the correct variance model the
    // pooled excess is 1.09 and 0.72 sigma; the binomial reading was 8.64 and
    // 5.95. Those buckets were EMPTY at the battery's founding commit
    // 34cfaeb7, so this rule had only ever been exercised on dense, spatially
    // diffuse populations where cell-count precision is roughly adequate.
    //
    // WHAT IS NOT THE REASON. The gate is not decalibrated: `uniformize`
    // maps the field onto a uniform correctly — over all 473 318 land cells
    // every 5%-wide bin of U holds 4.72-5.14%, mean(U) = 0.50005. And the
    // harness reconstruction still matches production, which
    // `provider.rs::cave_at_agrees_with_the_kind_first_gate` pins directly.
    // Neither the 0.25 bound nor the 500-cell floor was widened, and no seed
    // was re-pinned — all three would have silenced the symptom and left the
    // estimator wrong.
    //
    // THE RESTATED RULE IS TWO ARMS, and it has to be, because the obvious
    // one-arm version is not sufficient — that was MEASURED, not reasoned:
    //
    //   H4a (aggregate) carries the DETECTION of a decalibrated gate.
    //   H4b (per-bucket) catches a deviation LOCALISED to one probability
    //     range, and fails only when a bucket is BOTH >25% off nominal (the
    //     original bound, untouched) AND >=3 sigma against a cluster-robust
    //     SE with the world as the sampling unit.
    //
    // A NOTE AGAINST THE TEMPTING WRONG SUMMARY: it is NOT true that "a real
    // decalibration moves every world together and so still fails H4b". That
    // sentence was written here first and a mutation test refuted it — a
    // ~0.13-SD shift of GATE_NOISE_MEAN moves every bucket and every world and
    // still clears H4b, because in a thin bucket the same shift inflates the
    // between-world variance along with the excess. Thirty worlds simply have
    // no power to resolve a 25% shift in a bucket holding ~70 expected caves
    // per world. That is why H4a exists and why deleting it would restore the
    // blindness while leaving every H4b comment looking correct.
    // H4a — THE AGGREGATE CALIBRATION CLAIM, and the arm that carries the
    // detection power. A per-bucket test cannot be both honest and sensitive
    // in a THIN bucket: with ~70 expected caves per world spread over a smooth
    // field, thirty worlds have no power to resolve a 25% shift, so a
    // per-bucket rule is either blind or fires on geography. This arm exists
    // because the physically meaningful statement — "the gate fires at its
    // nominal rate" — is a claim about ALL land, where the geography averages
    // out and the power is.
    //
    // It is what catches a decalibrated `uniformize`, and it was added after a
    // mutation test proved the per-bucket arm alone did NOT: shifting
    // `GATE_NOISE_MEAN` 0.5003 -> 0.5100 (~0.13 SD) moves every bucket the same
    // way, is caught here at once, and slipped through H4b entirely.
    // THE BOUND IS SET FROM THE MEASURED SCATTER, AND ITS BLIND SPOT IS NAMED.
    // The between-world relative SE of this aggregate is ~3.9% (SE 1936 caves
    // against 49 246 expected), so a 5% bound would sit 1.25 sigma from zero
    // and fire on ordinary geography. 10% is ~2.5 sigma. WHAT THAT BUYS AND
    // WHAT IT DOES NOT: with 30 worlds this arm resolves an aggregate
    // decalibration of >=10% and is BLIND below that. The mutation that
    // motivated it — GATE_NOISE_MEAN 0.5003 -> 0.5100, which fires the gate
    // 11.85% hot — is caught; HALF that mutation would not be. Widening the
    // seed set is the only honest way to sharpen this, not lowering the bound.
    // The `agg_z` arm is the second, independent trigger: a deviation under
    // 10% that is nonetheless consistent across all 30 worlds is a resolved
    // decalibration and fails on significance instead.
    const H4_AGGREGATE_BOUND: f64 = 0.10;
    let (mut agg_hits, mut agg_prob) = (0usize, 0.0f64);
    for &(_, hits, prob_sum) in r.gate.iter() {
        agg_hits += hits;
        agg_prob += prob_sum;
    }
    let agg_residuals: Vec<f64> = r
        .gate_per_world
        .iter()
        .map(|w| w.iter().map(|b| b.1 as f64).sum::<f64>() - w.iter().map(|b| b.2).sum::<f64>())
        .collect();
    let agg_se = cluster_robust_se(&agg_residuals).expect("30 worlds");
    let agg_dev = (agg_hits as f64 - agg_prob) / agg_prob;
    let agg_z = (agg_hits as f64 - agg_prob).abs() / agg_se;
    println!(
        "  H4a aggregate: {agg_hits} caves against {agg_prob:.1} expected \
         ({:+.2}%), cluster-robust SE {agg_se:.1} = {agg_z:.2} sigma",
        100.0 * agg_dev
    );
    assert!(
        agg_dev.abs() < H4_AGGREGATE_BOUND && agg_z < H4_SIGMA,
        "H4a: the gate fires at {:+.2}% of its nominal rate over ALL land \
         ({agg_hits} caves against {agg_prob:.1} expected, {agg_z:.2} sigma \
         cluster-robust). This is the whole-globe calibration claim, where \
         geography averages out — a deviation here is a DECALIBRATED GATE, not \
         one world's terrain. Check `uniformize`'s GATE_NOISE_MEAN/SD against \
         the field's measured moments before touching anything else.",
        100.0 * agg_dev
    );

    // H4b — per-bucket, for a deviation LOCALISED to one probability range,
    // which the aggregate would average away.
    const H4_SIGMA: f64 = 3.0;
    for (i, &(lo, hi)) in PROB_BUCKETS.iter().enumerate() {
        let (cells, hits, prob_sum) = r.gate[i];
        if cells < 500 {
            continue; // too few samples for a rate to mean anything
        }
        let realized = hits as f64 / cells as f64;
        let nominal = prob_sum / cells as f64;
        let relative = (realized - nominal).abs() / nominal;
        if relative < 0.25 {
            continue;
        }
        // The world is the cluster. Residual = observed caves - expected.
        let residuals: Vec<f64> = r
            .gate_per_world
            .iter()
            .map(|w| w[i].1 as f64 - w[i].2)
            .collect();
        let excess: f64 = residuals.iter().sum();
        let se = cluster_robust_se(&residuals).expect("30 worlds");
        let z = if se > 0.0 {
            excess.abs() / se
        } else {
            f64::MAX
        };
        assert!(
            z < H4_SIGMA,
            "H4: bucket [{lo:.2},{hi:.2}) realized {realized:.5} against nominal \
             {nominal:.5} ({:+.1}%) — and this is RESOLVED, not one world's \
             geography: excess {excess:+.2} caves against a cluster-robust SE of \
             {se:.2} is {z:.2} sigma with the world as the sampling unit. A \
             decalibrated gate moves every world together; check `uniformize` \
             against the field's measured mean/SD before touching a threshold.",
            100.0 * (realized - nominal) / nominal
        );
        println!(
            "  H4 note: bucket [{lo:.2},{hi:.2}) is {:+.1}% off nominal but only \
             {z:.2} sigma (excess {excess:+.2}, cluster-robust SE {se:.2}) — \
             within one world's geography, not a gate defect",
            100.0 * (realized - nominal) / nominal
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

/// **Decision 0138 clause 2 for H2's restatement: the estimator's defect,
/// demonstrated rather than asserted, and not inferred from the failure.**
///
/// The retired `features::cave_depth` (verbatim below, from `1e92c152`) is
/// reproduced here as a fixture, not called — it no longer exists. Its range
/// over its *entire* input domain is enumerated: three cave kinds x the
/// `proneness >= 0.5` predicate x the `unconformity` flag is eight cases, and
/// that is every distinguishable input the function had.
///
/// The column is held FIXED, so the world contributes nothing. The retired
/// estimator still yields three distinct bands. Therefore the original H2's
/// "at least 3 distinct bands" was satisfied by the generator's arity alone —
/// it would have passed over a world in which every single cave had an
/// identical depth, which is exactly the defect H2 existed to catch.
///
/// This is a property of a function, provable without building a world, so it
/// is independent of the failure that occasioned the restatement.
#[test]
fn the_retired_h2_estimator_is_satisfied_by_its_own_generator() {
    /// Verbatim body of the retired `hornvale_terrain::features::cave_depth`
    /// at `1e92c152`, before The Underworld cut `MAP-cave-depth-weld`. Kept
    /// here only as the subject of this demonstration.
    fn retired_cave_depth(kind: CaveKind, unconformity: bool, proneness: f64) -> BandKind {
        const DEEP_PROCESS_PRONENESS: f64 = 0.5;
        let strong = proneness >= DEEP_PROCESS_PRONENESS;
        match kind {
            CaveKind::Karst => {
                if strong || unconformity {
                    BandKind::Basement
                } else {
                    BandKind::Cover
                }
            }
            CaveKind::LavaTube => BandKind::Cover,
            CaveKind::Fracture => {
                if strong {
                    BandKind::Roots
                } else {
                    BandKind::Basement
                }
            }
        }
    }

    /// `BandKind` is not `Ord`, so name it to collect a set.
    fn name_of(band: BandKind) -> &'static str {
        match band {
            BandKind::Regolith => "Regolith",
            BandKind::Cover => "Cover",
            BandKind::Basement => "Basement",
            BandKind::Roots => "Roots",
            BandKind::Underneath => "Underneath",
        }
    }

    // The whole input domain. `proneness` enters only through one predicate,
    // so two values on either side of it exhaust its influence; `unconformity`
    // is a bool; `kind` has three variants. Eight cases is total.
    let mut range: BTreeSet<&'static str> = BTreeSet::new();
    for kind in [CaveKind::Karst, CaveKind::LavaTube, CaveKind::Fracture] {
        for unconformity in [false, true] {
            for proneness in [0.0, 1.0] {
                range.insert(name_of(retired_cave_depth(kind, unconformity, proneness)));
            }
        }
    }

    assert_eq!(
        range.len(),
        3,
        "the retired estimator's total range was {range:?}"
    );
    assert!(
        range.len() >= 3,
        "…and 3 is exactly what the original H2 demanded, so the criterion was \
         satisfied by the match statement's arity and never by the world"
    );

    // The other half of the same point: the count above is reached without any
    // world at all, so a world of perfectly uniform depth produces it too. A
    // criterion a constant-depth world satisfies is not a test of collapse.
    let uniform_world_bands: BTreeSet<&'static str> =
        [CaveKind::Karst, CaveKind::LavaTube, CaveKind::Fracture]
            .into_iter()
            .map(|k| name_of(retired_cave_depth(k, false, 1.0)))
            .collect();
    assert_eq!(
        uniform_world_bands.len(),
        3,
        "three kinds at one identical depth still spell three bands: {uniform_world_bands:?}"
    );
}

/// **Decision 0138 clause 3 for H2's restatement: the restated criterion
/// re-proved against the defect the original existed to catch.**
///
/// The live half of this is the mutation run recorded in The Underworld's
/// Task 1b report — `cave_depth_reach_m` forced to a constant, the whole
/// battery run, `cave_substrate_meets_preregistered_criteria` red. This is its
/// permanent, cheap counterpart: the criterion's own arithmetic, fed the two
/// collapse shapes by hand.
///
/// Both shapes are the real thing, not inventions. The first is a fully
/// collapsed budget (The Hollow's §2.2 defect: one depth everywhere). The
/// second is the pre-1b two-valued coordinate Task 1 measured — ~0 m or
/// ~14 km, which the ceiling clamps into the top class — in the 65/35 ratio the
/// probe found on seed 42. The original H2 passed on that second shape; the
/// restatement must not.
#[test]
fn the_restated_h2_rejects_a_collapsed_and_a_two_valued_depth() {
    // Positive control FIRST: the criterion must be capable of passing, or
    // every rejection below is vacuous.
    let healthy = [400usize, 300, 200, 100, 50];
    let (occupied, modal) = variety_of(&healthy);
    assert!(
        occupied >= 3 && modal < 0.90,
        "the control distribution must PASS: {occupied} occupied, modal {modal:.4}"
    );

    // Collapse: every cave at one depth.
    let mut collapsed = [0usize; REACH_BINS];
    collapsed[reach_bin(500.0)] = 1000;
    let (occupied, modal) = variety_of(&collapsed);
    assert!(
        occupied < 3,
        "a fully collapsed budget must fail the occupancy arm, got {occupied}"
    );
    assert!(modal >= 0.90, "…and the modal arm, got {modal:.4}");

    // The pre-1b two-valued coordinate: `top_depth_m(deepest_band)` was ~0 m
    // or ~14 km, the latter clamped by the ceiling into the top class.
    let mut two_valued = [0usize; REACH_BINS];
    two_valued[reach_bin(0.0)] = 655;
    two_valued[reach_bin(hornvale_terrain::CAVE_REACH_CEILING_M)] = 345;
    let (occupied, modal) = variety_of(&two_valued);
    assert!(
        occupied < 3,
        "the pre-1b two-valued coordinate must fail the occupancy arm, got \
         {occupied} ({two_valued:?})"
    );
    assert!(
        modal < 0.90,
        "the modal arm alone does NOT catch it ({modal:.4}) — recorded so the \
         occupancy arm is known to be the load-bearing one here"
    );
}

//! THE TRENCHER, Task 14: what SHAPE is the aquifer set, and where do
//! springs come from?
//!
//! Task 13 made `carbonate` and `metamorphic_grade` continuous, which moved
//! `porosity` and so moved `hydrogeology`'s classes. `Hydro::Spring` roughly
//! doubled. The hypothesis this file was written to test was that the rise is
//! a FRAGMENTATION artifact of a particular shape — that a continuous porosity
//! field SPECKLES the aquifer set, and that an isolated one-vertex aquifer,
//! being entirely margin, promotes almost surely under `promote_to_spring`
//! while a large contiguous body promotes only its rim. The remedy that
//! follows from it is an EXTENT rule: a spring should need an aquifer body big
//! enough to feed it.
//!
//! # The verdict, measured here against a pre-Task-13 control
//!
//! **The speckle half is false and the extent remedy cannot work.** Isolated
//! aquifer vertices are 0.3-0.5% of the aquifer set after the change (0.1-0.3%
//! before), and components of 4 vertices or fewer supply 2.6-5.3% of all
//! springs (1.2-3.1% before). An extent threshold that rejected every body of
//! 20 vertices or fewer would withhold 8.7-12.2% of springs, against a rise of
//! 37-66% that needs explaining. There is no speckle to remove.
//!
//! **The perimeter half is true, at a scale two orders of magnitude coarser
//! than "a speck".** The aquifer set did not disperse into dust; it broke into
//! MORE, SMALLER, RAGGEDER BODIES. Seeds 0/7/42 at level 6:
//!
//! ```text
//!                       before            after
//!   components          29 / 37 / 18      74 / 70 / 43
//!   mean component     162 / 171 / 209    67 / 71 /  96   (vertices)
//!   margin / aquifer  .251 / .255 / .252  .443 / .549 / .447
//!   springs / margin  .711 / .807 / .801  .639 / .661 / .675
//! ```
//!
//! The margin fraction — the share of the aquifer set touching a non-aquifer
//! vertex — is the whole of the rise, at 1.77-2.15x. The promotion rule itself
//! became LESS permissive, not more: the share of margin vertices that find a
//! descending contact FELL on all three seeds. That is what a level set of a
//! genuinely continuous field looks like next to a level set of a near-
//! categorical one, and it is the intended consequence of Task 13 rather than
//! a defect in `promote_to_spring`.
//!
//! **And extent has no physical footing at this resolution.** A level-6 vertex
//! carries 4πR²/40962 ≈ 12,450 km² at Earth's radius. Every aquifer body in
//! every seed measured here — the singletons included — is larger than nearly
//! any real aquifer. "Too small to feed a spring" is not a statement this mesh
//! can make.
//!
//! # What this file is
//!
//! A MEASUREMENT, not a gate — `#[ignore]`d under the `probe:` token
//! `rift_probe.rs` established. Two arms. Run them by hand:
//!
//! ```text
//! cargo test --release -p hornvale-terrain --test suite -- aquifer_shape_probe --ignored --nocapture
//! ```
//!
//! 1. [`aquifer_patch_shape_and_spring_provenance`] — the connected components
//!    of the aquifer set under the geosphere's own adjacency (land only:
//!    `hydrogeology` returns `Aquitard` for every ocean vertex, so a body never
//!    spans the sea), each size's spring yield, the margin decomposition above,
//!    and the two shares the committed guard
//!    `a_real_world_produces_a_porous_non_carbonate_vertex_in_bounded_shares`
//!    asserts. Component labelling uses an explicit stack over a `Vec<usize>`
//!    label buffer — no map, no recursion.
//! 2. [`clastic_aquifer_threshold_sweep_reproduces_the_admissible_band`] — the
//!    `CLASTIC_AQUIFER_MIN_POROSITY` sweep, re-run. It exists because the
//!    figures Task 13 recorded do not reproduce (see that constant's doc), and
//!    it cross-checks its reproduced classifier against the shipped
//!    `hydrogeology` before sweeping, so a drifted restatement of the private
//!    constants fails loudly instead of quietly shifting a band.
//!
//! The before/after table above was taken by checking out the pre-Task-13
//! `lithology.rs`/`elevation.rs`/`globe.rs` under this same probe; the "before"
//! column is not re-derivable from today's tree and is recorded here because
//! of that. Its `aquifer-share` readings were validated against the committed
//! census (`book/src/laboratory/generated/the-census/rows.csv`,
//! `aquifer-fraction`), which the probe reproduces to all eight of the
//! census's significant digits at seeds 0 and 7.

use hornvale_kernel::{Geosphere, Seed, Vertex};
use hornvale_terrain::{GeneratedTerrain, Hydro, TerrainPins, generate};
use std::collections::BTreeMap;

/// The seeds this probe sweeps — the three the spring measurement in
/// `CLASTIC_AQUIFER_MIN_POROSITY`'s doc reports.
const SEEDS: [u64; 3] = [0, 7, 42];

/// The production grid.
const LEVEL: u32 = 6;

/// Build one seed's terrain at [`LEVEL`].
fn terrain_at(seed: u64, geo: &Geosphere) -> GeneratedTerrain {
    let outcome = generate(Seed(seed), geo, &TerrainPins::default()).expect("seed generates");
    GeneratedTerrain::new(geo.clone(), outcome)
}

/// Label the connected components of `member` under geosphere adjacency.
/// Returns (label per vertex, `usize::MAX` for non-members; component sizes).
fn components(geo: &Geosphere, member: &[bool]) -> (Vec<usize>, Vec<usize>) {
    let n = geo.vertex_count();
    let mut label = vec![usize::MAX; n];
    let mut sizes: Vec<usize> = Vec::new();
    let mut stack: Vec<usize> = Vec::new();
    for start in 0..n {
        if !member[start] || label[start] != usize::MAX {
            continue;
        }
        let id = sizes.len();
        let mut size = 0usize;
        stack.push(start);
        label[start] = id;
        while let Some(v) = stack.pop() {
            size += 1;
            for &nb in geo.neighbors(Vertex(v as u32)).iter() {
                let nbi = nb.0 as usize;
                if member[nbi] && label[nbi] == usize::MAX {
                    label[nbi] = id;
                    stack.push(nbi);
                }
            }
        }
        sizes.push(size);
    }
    (label, sizes)
}

/// claim: readout(the aquifer set's connected-component sizes, its margin
/// fraction, and each size class's share of the world's springs, over three
/// seeds at the production mesh level)
#[test]
#[ignore = "probe: measurement instrument, not a gate; ~3 worlds at level 6"]
fn aquifer_patch_shape_and_spring_provenance() {
    let geo = Geosphere::new(LEVEL);
    let n = geo.vertex_count();
    println!("# aquifer patch shape, level {LEVEL}, {n} vertices");
    for seed in SEEDS {
        let terrain = terrain_at(seed, &geo);
        // The base (pre-promotion) class per vertex, plus the shipped class.
        let mut is_land = vec![false; n];
        let mut base_aquifer = vec![false; n];
        let mut shipped = vec![Hydro::Aquitard; n];
        for v in 0..n {
            let id = Vertex(v as u32);
            let ocean = terrain.is_ocean(id);
            is_land[v] = !ocean;
            let base = hornvale_terrain::hydrogeology(&terrain.material_at(id), ocean);
            base_aquifer[v] = base == Hydro::Aquifer;
            shipped[v] = terrain.hydro_at(id);
        }
        let land = is_land.iter().filter(|&&l| l).count();
        let aquifer_n = base_aquifer.iter().filter(|&&a| a).count();
        let spring_n = shipped.iter().filter(|&&h| h == Hydro::Spring).count();
        let (label, sizes) = components(&geo, &base_aquifer);

        // Perimeter decomposition: of the base-aquifer set, how many vertices
        // have ANY non-aquifer neighbour (the margin), and of those how many
        // have a LOWER one (the springs). Separates "the set grew more
        // perimeter" from "more of the perimeter descends".
        let mut margin = 0usize;
        for v in 0..n {
            if !base_aquifer[v] {
                continue;
            }
            let id = Vertex(v as u32);
            if geo
                .neighbors(id)
                .iter()
                .any(|&nb| !base_aquifer[nb.0 as usize])
            {
                margin += 1;
            }
        }
        let shipped_aquifer = shipped.iter().filter(|&&h| h == Hydro::Aquifer).count();
        println!(
            "\nGUARD-EXACT seed {seed}: aquifer-share {:.4}  spring-share {:.4}  \
             (land {land})",
            shipped_aquifer as f64 / land as f64,
            spring_n as f64 / land as f64
        );
        println!(
            "PERIMETER seed {seed}: margin {margin}/{aquifer_n} = {:.3} of the aquifer set; \
             springs/margin = {:.3}; mean component size {:.1}",
            margin as f64 / aquifer_n as f64,
            spring_n as f64 / margin as f64,
            aquifer_n as f64 / sizes.len() as f64
        );

        // Per-component spring counts.
        let mut springs_in: Vec<usize> = vec![0; sizes.len()];
        for v in 0..n {
            if label[v] != usize::MAX && shipped[v] == Hydro::Spring {
                springs_in[label[v]] += 1;
            }
        }

        // Size histogram, and the share of springs contributed by components
        // at or below each size cut.
        let mut by_size: BTreeMap<usize, (usize, usize, usize)> = BTreeMap::new();
        for (i, &s) in sizes.iter().enumerate() {
            let e = by_size.entry(s).or_insert((0, 0, 0));
            e.0 += 1; // components of this size
            e.1 += s; // vertices in them
            e.2 += springs_in[i]; // springs from them
        }
        let mut sorted = sizes.clone();
        sorted.sort_unstable();
        let biggest = sorted.last().copied().unwrap_or(0);
        println!(
            "\n## seed {seed}: land {land}, aquifer(base) {aquifer_n} ({:.2}% of land), \
             spring {spring_n} ({:.2}% of land), components {}, largest {biggest}",
            100.0 * aquifer_n as f64 / land as f64,
            100.0 * spring_n as f64 / land as f64,
            sizes.len()
        );
        println!("size | #comps | vertices | springs | spring rate within size");
        let mut cum_v = 0usize;
        let mut cum_s = 0usize;
        for (&s, &(c, v, sp)) in by_size.iter() {
            cum_v += v;
            cum_s += sp;
            if s <= 12 || c > 1 {
                println!(
                    "{s:>5} | {c:>6} | {v:>8} | {sp:>7} | {:.3}   (cum: {:.1}% of aquifer, \
                     {:.1}% of springs)",
                    sp as f64 / v as f64,
                    100.0 * cum_v as f64 / aquifer_n as f64,
                    100.0 * cum_s as f64 / spring_n as f64
                );
            }
        }
        // Explicit cuts.
        for cut in [1usize, 2, 3, 4, 5, 8, 12, 20] {
            let (mut v, mut sp) = (0usize, 0usize);
            for (i, &s) in sizes.iter().enumerate() {
                if s <= cut {
                    v += s;
                    sp += springs_in[i];
                }
            }
            println!(
                "cut<= {cut:>3}: {v:>6} aquifer vertices ({:.1}%), {sp:>6} springs ({:.1}% of \
                 all springs), within-cut spring rate {:.3}",
                100.0 * v as f64 / aquifer_n as f64,
                100.0 * sp as f64 / spring_n as f64,
                sp as f64 / v.max(1) as f64
            );
        }
    }
}

/// `KARST_MIN_POROSITY`, reproduced: the constants `hydrogeology` reads are
/// private to `lithology.rs`, so the sweep arm below restates them the way
/// every out-of-module calibration sweep in this project must. Any drift
/// between these and the real constants invalidates the sweep, which is why
/// the arm cross-checks its own classifier against the shipped `hydro_at`
/// at the shipped threshold before sweeping.
const SWEEP_KARST_MIN_POROSITY: f64 = 0.4;

/// `AQUITARD_MAX_POROSITY`, reproduced — see [`SWEEP_KARST_MIN_POROSITY`].
const SWEEP_AQUITARD_MAX_POROSITY: f64 = 0.15;

/// `CLASTIC_AQUIFER_MIN_POROSITY`'s shipped value, reproduced — the point the
/// cross-check is taken at.
const SWEEP_SHIPPED_THRESHOLD: f64 = 0.53;

/// claim: readout(the aquifer- and spring-share admissible intervals of
/// `CLASTIC_AQUIFER_MIN_POROSITY`, re-swept over three seeds against the
/// figures Task 13 recorded, with a classifier cross-check as its control)
#[test]
#[ignore = "probe: measurement instrument, not a gate; sweeps 3 worlds x 111 thresholds"]
fn clastic_aquifer_threshold_sweep_reproduces_the_admissible_band() {
    let geo = Geosphere::new(LEVEL);
    let n = geo.vertex_count();
    println!("# clastic-aquifer threshold sweep, level {LEVEL}");
    for seed in SEEDS {
        let terrain = terrain_at(seed, &geo);
        let mut land_ids: Vec<Vertex> = Vec::new();
        let mut porosity = vec![0.0f64; n];
        let mut carbonate = vec![0.0f64; n];
        for v in 0..n {
            let id = Vertex(v as u32);
            if terrain.is_ocean(id) {
                continue;
            }
            land_ids.push(id);
            let m = terrain.material_at(id);
            porosity[v] = m.porosity;
            carbonate[v] = m.carbonate;
        }
        let land = land_ids.len();
        // Cross-check: the reproduced classifier at the shipped threshold
        // must agree with the shipped `hydrogeology` on every land vertex,
        // or the reproduced constants above have drifted.
        let base_of = |v: usize, thr: f64| -> Hydro {
            if carbonate[v] > 0.5 && porosity[v] > SWEEP_KARST_MIN_POROSITY {
                Hydro::Karst
            } else if porosity[v] < SWEEP_AQUITARD_MAX_POROSITY {
                Hydro::Aquitard
            } else if porosity[v] > thr {
                Hydro::Aquifer
            } else {
                Hydro::Runoff
            }
        };
        let mismatches = land_ids
            .iter()
            .filter(|&&id| {
                base_of(id.0 as usize, SWEEP_SHIPPED_THRESHOLD)
                    != hornvale_terrain::hydrogeology(&terrain.material_at(id), false)
            })
            .count();
        assert_eq!(
            mismatches, 0,
            "the reproduced classifier disagrees with the shipped one on {mismatches} land \
             vertices at seed {seed} -- the constants restated above have drifted"
        );
        // Sweep.
        let mut admissible: Vec<f64> = Vec::new();
        let mut spring_admissible: Vec<f64> = Vec::new();
        let mut at_shipped = (0.0, 0.0);
        let mut thr_i = 400;
        while thr_i <= 620 {
            let thr = thr_i as f64 / 1000.0;
            let mut member = vec![false; n];
            for &id in &land_ids {
                member[id.0 as usize] = base_of(id.0 as usize, thr) == Hydro::Aquifer;
            }
            let mut aquifer = 0usize;
            let mut spring = 0usize;
            for &id in &land_ids {
                let v = id.0 as usize;
                if !member[v] {
                    continue;
                }
                let e = terrain.elevation_at(id);
                let promoted = geo.neighbors(id).iter().any(|&nb| {
                    !member[nb.0 as usize]
                        && terrain.elevation_at(nb).total_cmp(e) == std::cmp::Ordering::Less
                });
                if promoted {
                    spring += 1;
                } else {
                    aquifer += 1;
                }
            }
            let a = aquifer as f64 / land as f64;
            let sp = spring as f64 / land as f64;
            if (0.05..=0.35).contains(&a) {
                admissible.push(thr);
            }
            if (0.005..=0.08).contains(&sp) {
                spring_admissible.push(thr);
            }
            if (thr - SWEEP_SHIPPED_THRESHOLD).abs() < 1e-9 {
                at_shipped = (a, sp);
            }
            thr_i += 2;
        }
        println!(
            "seed {seed}: at thr={SWEEP_SHIPPED_THRESHOLD} aquifer-share {:.4} spring-share \
             {:.4}; aquifer-band [{:.3}, {:.3}]; spring-band [{:.3}, {:.3}]",
            at_shipped.0,
            at_shipped.1,
            admissible.first().copied().unwrap_or(f64::NAN),
            admissible.last().copied().unwrap_or(f64::NAN),
            spring_admissible.first().copied().unwrap_or(f64::NAN),
            spring_admissible.last().copied().unwrap_or(f64::NAN),
        );
    }
}

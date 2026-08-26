//! THE WINZE, Task P1: do fields from OTHER derivations decorrelate, where
//! lithology's do not?
//!
//! **A measurement. Changes no production code.** Task 2M
//! (`winze_energy_probe.rs`) found the six `MaterialBuffer` lithology axes
//! (`silica`/`grain`/`induration`/`carbonate`/`metamorphic_grade`/`porosity`)
//! strongly inter-correlated over cave-bearing vertices (no pairwise `|r|` below
//! 0.5488, 64% at or above 0.8) — structural, because six labelled axes
//! reduce to four shared upstream tectonic inputs plus one noise patch, and
//! `porosity` and `induration` are literally weighted sums of the others.
//! Plural chemotrophic energy sources keyed on those six axes would
//! co-locate. This probe asks the next question: does the campaign have
//! *any* field, from a derivation outside that chain, that would actually
//! spread sources across the underworld?
//!
//! ## Candidates, resolved to their real accessors
//!
//! The brief named seven candidates by module-name inference; two forms did
//! not exist as named and were resolved by reading:
//!
//! - **volcanism/arc history** → `GeneratedTerrain::has_edifice` (bool).
//!   `ARC_GATE` is the private noise stream that feeds it, not itself a
//!   per-vertex accessor.
//! - **sediment thickness** → `GeneratedTerrain::sediment_thickness_at` (m).
//! - **water table depth** → `hornvale_terrain::water_table_depth_m(drainage,
//!   porosity, height_asl_m)`, a free function, not a per-vertex accessor —
//!   computed per vertex from `drainage_at`, `material_at(vertex).porosity`, and
//!   `elevation_at(vertex) - sea_level()`. **This candidate is NOT off-lithology
//!   as named**: `porosity` is one of its three inputs (via the function's
//!   internal `transmissivity(porosity)` term), so it is a partial,
//!   non-deterministic-but-not-independent function of the very axis Task 2M
//!   already ruled out. Measured anyway; its correlation with `porosity`
//!   specifically is flagged, not treated as evidence, in the report.
//! - **unconformity** → `GeneratedTerrain::unconformity_at` (bool),
//!   `f(depth_to_basement_m, crust_age)` — exists as named.
//! - **geothermal gradient** → `GeneratedTerrain::geothermal_gradient_at`
//!   returns `GeothermalGradient`; `.get()` gives K/km.
//!   `GeothermalGradient::gradient_at` named in the brief does not exist —
//!   the free function is `strata::geothermal_gradient`, reached here via
//!   the provider method.
//! - **paleoclimate** → `PaleoRecord` (via
//!   `hornvale_worldgen::paleoclimate_from`) has no single scalar per-vertex
//!   read, but three per-vertex **bool** fields do exist and are genuinely
//!   per-vertex: `envelope` (ever glaciated), `shoreline` (swept by eustatic
//!   sea level across eras), `refugia` (habitable through the glacial
//!   maximum). All three measured as point-biserial correlations, same
//!   convention as `unconformity`.
//! - **cave depth reach** → `Cave::depth_reach_m`
//!   (`cave_depth::cave_depth_reach_m`) exists as named, but reading it
//!   found it is **not off-lithology at all**: it is a direct, deterministic
//!   function of `induration` (`closure_depth_m`, every cave kind) and, for
//!   `CaveKind::Karst` specifically, also `carbonate` and `unconformity` (the
//!   paleokarst gain). Measured anyway per the brief's instruction to flag
//!   rather than omit; its correlations with `induration`/`carbonate` are not
//!   evidence of anything.
//!
//! Two candidates measured clean of any lithology-axis arithmetic:
//! `sediment_thickness_at` (feeds only `soil_depth`, not the six axes) and
//! `geothermal_gradient_at` (`f(crust_thickness, crust_age, continental)` —
//! shares upstream tectonic *inputs* with `grain`/`induration` but is not
//! built from their *values*).
//!
//! ## A methodology note against Task 2M's own probe
//!
//! `winze_energy_probe.rs`'s `measure()` sorts each of its six field vectors
//! **independently** (`for axis in &mut fields { axis.sort_by(...) }`)
//! *before* computing `pearson(&m.fields[i], &m.fields[j])` on those same
//! sorted vectors. That destroys the per-vertex pairing: by the rearrangement
//! inequality, correlating two independently-ascending-sorted vectors
//! computes the maximum achievable Pearson correlation over any pairing of
//! the two multisets (the comonotonic coupling), not the actual per-vertex
//! correlation, and can only equal the true correlation when the two fields
//! already induce the same vertex ordering. This probe keeps one **unsorted,
//! vertex-aligned** copy of every field vector for correlation, and only
//! clones-and-sorts a copy for the percentile/distinct-count summary, so the
//! two computations cannot share state. Task 2M's own STOP verdict (branch
//! 2, majority-`>=0.8`) is not necessarily wrong — the six axes are
//! mechanically inter-derived regardless of measurement method — but its
//! *numbers* were not measuring per-vertex correlation as reported, and this
//! discrepancy is reported here rather than silently repeated.
//!
//! World-building idiom, cave-bearing-vertex enumeration, and seed panel
//! copied from `winze_energy_probe.rs` (in turn from
//! `delver_depth_probe.rs`/`underworld_lithology_probe.rs`).
//!
//! Test fixture (decision 0092): calls the sculpt/fit derivation entry
//! points directly to build its own world state, once per test.
#![allow(clippy::disallowed_methods)]

use hornvale_astronomy::SkyPins;
use hornvale_kernel::Seed;
use hornvale_terrain::{TerrainPins, water_table_depth_m};
use hornvale_worldgen::{SettlementPins, SkyChoice, build_world, paleoclimate_from, terrain_of};

/// Seeds the campaign states its preregistrations on (same panel as
/// `winze_energy_probe.rs`).
const SEEDS: [u64; 3] = [42, 7, 1234];

/// The six `MaterialBuffer` lithology axes Task 2M measured, followed by the
/// off-lithology candidates resolved to their real accessors (see the module
/// doc for which brief names were wrong and what was substituted) — the
/// fixed column order for the full cave-bearing-vertex correlation matrix
/// (index 9 is `cave_depth_reach_m`, the one field that does not exist off a
/// cave-bearing vertex). `water_table_depth_m` and `cave_depth_reach_m` are
/// flagged (not omitted) as partially/fully derived from a lithology axis —
/// see `CLEAN_OFF_LITHOLOGY_FIELDS` below.
const ALL_FIELDS: [&str; 15] = [
    "silica",
    "grain",
    "induration",
    "carbonate",
    "metamorphic_grade",
    "porosity",
    "sediment_thickness",
    "geothermal_gradient_k_per_km",
    "water_table_depth_m",
    "cave_depth_reach_m",
    "has_edifice",
    "unconformity",
    "paleo_envelope",
    "paleo_shoreline",
    "paleo_refugia",
];

/// `ALL_FIELDS` without `cave_depth_reach_m` (index 9) — used for the
/// land-vertex contrast distributions, since a cave's depth reach has no
/// meaning off a cave-bearing vertex.
const LAND_FIELDS: [&str; 14] = [
    "silica",
    "grain",
    "induration",
    "carbonate",
    "metamorphic_grade",
    "porosity",
    "sediment_thickness",
    "geothermal_gradient_k_per_km",
    "water_table_depth_m",
    "has_edifice",
    "unconformity",
    "paleo_envelope",
    "paleo_shoreline",
    "paleo_refugia",
];

/// Off-lithology candidates NOT flagged as derived from a lithology axis —
/// the set the branch table (weak-pairwise-correlation clique search) is
/// evaluated over. `water_table_depth_m` and `cave_depth_reach_m` are
/// excluded here (still measured and printed in the full matrix, per the
/// brief's "flag, don't omit" instruction).
const CLEAN_OFF_LITHOLOGY_FIELDS: [&str; 7] = [
    "sediment_thickness",
    "geothermal_gradient_k_per_km",
    "has_edifice",
    "unconformity",
    "paleo_envelope",
    "paleo_shoreline",
    "paleo_refugia",
];

/// Nearest-rank percentile of an ascending-sorted slice.
fn pct(sorted: &[f64], q: f64) -> f64 {
    if sorted.is_empty() {
        return f64::NAN;
    }
    let i = ((sorted.len() - 1) as f64 * q).round() as usize;
    sorted[i]
}

/// Count of distinct values in an ascending-sorted slice.
fn distinct_count(sorted: &[f64]) -> usize {
    let mut d = sorted.to_vec();
    d.dedup_by(|a, b| a == b);
    d.len()
}

/// Pearson correlation coefficient between two equal-length, **vertex-aligned**
/// series (index i of `xs` and index i of `ys` must be the same vertex). `NaN`
/// when either series has zero variance — a genuinely undefined correlation,
/// not a zero one. A bool field encoded as `0.0`/`1.0` and correlated this
/// way is the point-biserial coefficient (they are the same statistic).
///
/// **Callers must never sort `xs`/`ys` before calling this** — sorting
/// either series independently destroys the vertex pairing (see the module
/// doc's methodology note against `winze_energy_probe.rs`).
fn pearson(xs: &[f64], ys: &[f64]) -> f64 {
    assert_eq!(xs.len(), ys.len(), "pearson: mismatched series lengths");
    let n = xs.len() as f64;
    if n == 0.0 {
        return f64::NAN;
    }
    let mx = xs.iter().sum::<f64>() / n;
    let my = ys.iter().sum::<f64>() / n;
    let mut cov = 0.0;
    let mut vx = 0.0;
    let mut vy = 0.0;
    for i in 0..xs.len() {
        let dx = xs[i] - mx;
        let dy = ys[i] - my;
        cov += dx * dy;
        vx += dx * dx;
        vy += dy * dy;
    }
    if vx <= 0.0 || vy <= 0.0 {
        return f64::NAN;
    }
    cov / (vx.sqrt() * vy.sqrt())
}

/// One seed's full measurement. `cave_fields`/`land_fields` hold one
/// **unsorted, vertex-aligned** `Vec<f64>` per field in `ALL_FIELDS`/
/// `LAND_FIELDS` order — never sorted in place, so correlation and
/// percentile summaries can never disagree about which computation ran on
/// which copy.
struct SeedMeasurement {
    seed_value: u64,
    cave_fields: Vec<Vec<f64>>,
    land_fields: Vec<Vec<f64>>,
    cave_vertex_count: usize,
    land_vertex_count: usize,
}

fn measure(seed_value: u64) -> SeedMeasurement {
    let seed = Seed(seed_value);
    let world = build_world(
        seed,
        &SkyPins::default(),
        SkyChoice::Generated,
        &TerrainPins::default(),
        &SettlementPins::default(),
    )
    .expect("probe seed builds");
    let terrain = terrain_of(&world).expect("terrain");
    let geo = terrain.geosphere();
    let sea = terrain.sea_level().get();
    let paleo = paleoclimate_from(&world, &terrain).expect("paleoclimate");

    let mut cave_fields: Vec<Vec<f64>> = vec![Vec::new(); ALL_FIELDS.len()];
    let mut land_fields: Vec<Vec<f64>> = vec![Vec::new(); LAND_FIELDS.len()];

    for vertex in geo.vertices() {
        if terrain.is_ocean(vertex) {
            continue;
        }
        let b = terrain.material_at(vertex);
        let sediment_thickness = terrain.sediment_thickness_at(vertex);
        let geothermal = terrain.geothermal_gradient_at(vertex).get();
        let drainage = terrain.drainage_at(vertex);
        let height_asl_m = terrain.elevation_at(vertex).get() - sea;
        let table = water_table_depth_m(drainage, b.porosity, height_asl_m);
        let has_edifice = if terrain.has_edifice(vertex) {
            1.0
        } else {
            0.0
        };
        let unconformity = if terrain.unconformity_at(vertex) {
            1.0
        } else {
            0.0
        };
        let paleo_envelope = if *paleo.envelope.get(vertex) {
            1.0
        } else {
            0.0
        };
        let paleo_shoreline = if *paleo.shoreline.get(vertex) {
            1.0
        } else {
            0.0
        };
        let paleo_refugia = if *paleo.refugia.get(vertex) { 1.0 } else { 0.0 };

        let land_values = [
            b.silica,
            b.grain,
            b.induration,
            b.carbonate,
            b.metamorphic_grade,
            b.porosity,
            sediment_thickness,
            geothermal,
            table,
            has_edifice,
            unconformity,
            paleo_envelope,
            paleo_shoreline,
            paleo_refugia,
        ];
        for (axis, &v) in land_fields.iter_mut().zip(land_values.iter()) {
            axis.push(v);
        }

        if let Some(cave) = terrain.cave_at(vertex) {
            let cave_values = [
                b.silica,
                b.grain,
                b.induration,
                b.carbonate,
                b.metamorphic_grade,
                b.porosity,
                sediment_thickness,
                geothermal,
                table,
                cave.depth_reach_m,
                has_edifice,
                unconformity,
                paleo_envelope,
                paleo_shoreline,
                paleo_refugia,
            ];
            for (axis, &v) in cave_fields.iter_mut().zip(cave_values.iter()) {
                axis.push(v);
            }
        }
    }

    let cave_vertex_count = cave_fields[0].len();
    let land_vertex_count = land_fields[0].len();
    SeedMeasurement {
        seed_value,
        cave_fields,
        land_fields,
        cave_vertex_count,
        land_vertex_count,
    }
}

/// Prints one field's distribution line from an **unsorted** vector — clones
/// and sorts the clone, never the caller's copy.
fn print_distribution(name: &str, values: &[f64]) {
    let mut sorted = values.to_vec();
    sorted.sort_by(f64::total_cmp);
    let p90 = pct(&sorted, 0.90);
    let top_decile: Vec<f64> = sorted.iter().copied().filter(|&x| x >= p90).collect();
    let mut top_decile_sorted = top_decile.clone();
    top_decile_sorted.sort_by(f64::total_cmp);
    println!(
        "  {name:<30} min {:.4}  p25 {:.4}  p50 {:.4}  p75 {:.4}  p90 {:.4}  p99 {:.4}  max {:.4}  top-decile distinct {}/{}",
        sorted.first().copied().unwrap_or(f64::NAN),
        pct(&sorted, 0.25),
        pct(&sorted, 0.50),
        pct(&sorted, 0.75),
        p90,
        pct(&sorted, 0.99),
        sorted.last().copied().unwrap_or(f64::NAN),
        distinct_count(&top_decile_sorted),
        top_decile.len(),
    );
}

/// claim: readout(off-gate, heavy:, prints only, plus branch-pinning
/// assertions) — Task P1 of `task-p1-brief.md`, seeds 42/7/1234. A decision
/// instrument, not a pass/fail gate on a chosen constant.
#[test]
#[ignore = "heavy: live-worldgen battery; deferred from the commit gate to the heavy set (decision 0132)"]
fn off_lithology_decorrelation_probe() {
    let mut any_field_measured = false;

    // Cross-seed accumulator: for every pair among CLEAN_OFF_LITHOLOGY_FIELDS,
    // the max |r| seen across all three seeds (the most conservative "weak"
    // reading — a pair counts as weak only if it is weak on every seed).
    let n_clean = CLEAN_OFF_LITHOLOGY_FIELDS.len();
    let mut max_abs_r_clean_pair = vec![vec![0.0_f64; n_clean]; n_clean];
    // For every field in ALL_FIELDS, the max |r| against every OTHER field in
    // ALL_FIELDS, across all three seeds — the "how independent is this field,
    // against everything measured" ranking.
    let n_all = ALL_FIELDS.len();
    let mut max_abs_r_vs_all = vec![vec![0.0_f64; n_all]; n_all];
    // Task 2M's own branch-1 check, applied here: how many of the three
    // seeds does each field read as a literal zero-variance constant over
    // cave-bearing vertices (not merely a small IQR/range — an exact constant,
    // which makes any correlation reading for it either NaN or a near-zero
    // artifact of no information, not evidence of independence). A field
    // degenerate on any seed is excluded from the ranking and clique search
    // below, same discipline Task 2M applied to `prospectivity`.
    let mut degenerate_seed_count = vec![0u32; n_all];

    for seed_value in SEEDS {
        let m = measure(seed_value);
        println!(
            "\n== seed {} ==  cave-bearing vertices {}  land vertices {}",
            m.seed_value, m.cave_vertex_count, m.land_vertex_count
        );

        println!("  -- distributions, cave-bearing vertices --");
        for (name, v) in ALL_FIELDS.iter().zip(m.cave_fields.iter()) {
            any_field_measured |= !v.is_empty();
            print_distribution(name, v);
        }
        println!("  -- distributions, all land vertices (contrast) --");
        for (name, v) in LAND_FIELDS.iter().zip(m.land_fields.iter()) {
            print_distribution(name, v);
        }

        for (i, v) in m.cave_fields.iter().enumerate() {
            let degenerate = v.is_empty() || v.iter().all(|&x| x == v[0]);
            if degenerate {
                degenerate_seed_count[i] += 1;
            }
        }

        // Full correlation matrix, cave-bearing vertices, ALL_FIELDS order —
        // computed on the UNSORTED, vertex-aligned vectors in `m.cave_fields`.
        println!("  -- correlation matrix, cave-bearing vertices --");
        print!("                                  ");
        for name in &ALL_FIELDS {
            print!("{:>12}", &name[..name.len().min(12)]);
        }
        println!();
        for (i, name_i) in ALL_FIELDS.iter().enumerate() {
            print!("  {name_i:<32}");
            for (j, _name_j) in ALL_FIELDS.iter().enumerate() {
                let r = if i == j {
                    1.0
                } else {
                    pearson(&m.cave_fields[i], &m.cave_fields[j])
                };
                if i != j && r.is_finite() {
                    max_abs_r_vs_all[i][j] = max_abs_r_vs_all[i][j].max(r.abs());
                }
                print!("{r:>12.4}");
            }
            println!();
        }

        for (ci, ci_name) in CLEAN_OFF_LITHOLOGY_FIELDS.iter().enumerate() {
            let i = ALL_FIELDS.iter().position(|f| *f == *ci_name).unwrap();
            for (cj, cj_name) in CLEAN_OFF_LITHOLOGY_FIELDS.iter().enumerate() {
                if ci == cj {
                    continue;
                }
                let j = ALL_FIELDS.iter().position(|f| *f == *cj_name).unwrap();
                let r = pearson(&m.cave_fields[i], &m.cave_fields[j]);
                if r.is_finite() {
                    max_abs_r_clean_pair[ci][cj] = max_abs_r_clean_pair[ci][cj].max(r.abs());
                }
            }
        }
    }

    assert!(
        any_field_measured,
        "the survey found no cave-bearing vertices across {} seeds — this probe is measuring nothing",
        SEEDS.len()
    );

    println!("\n== near-constant check, cave-bearing vertices (branch-1-style floor) ==");
    let mut any_degenerate = false;
    for (i, name) in ALL_FIELDS.iter().enumerate() {
        if degenerate_seed_count[i] > 0 {
            any_degenerate = true;
            println!(
                "  {name:<30} DEGENERATE (zero-variance) on {}/{} seeds — excluded from \
                 the independent-signal ranking and clique search below",
                degenerate_seed_count[i],
                SEEDS.len(),
            );
        }
    }
    if !any_degenerate {
        println!("  none — every field varies on every seed");
    }

    // Usable subset of CLEAN_OFF_LITHOLOGY_FIELDS: indices into
    // CLEAN_OFF_LITHOLOGY_FIELDS (0..n_clean) whose ALL_FIELDS vector was
    // never a literal constant. `has_edifice` is expected to drop out here
    // (see task-p1-report.md): zero-variance on 2 of 3 seeds over
    // cave-bearing vertices, so its near-zero correlation readings are an
    // absence of information, not evidence of independence.
    let usable_clean: Vec<usize> = (0..n_clean)
        .filter(|&ci| {
            let all_idx = ALL_FIELDS
                .iter()
                .position(|f| *f == CLEAN_OFF_LITHOLOGY_FIELDS[ci])
                .unwrap();
            degenerate_seed_count[all_idx] == 0
        })
        .collect();

    // -- Which off-lithology candidates carry the most independent signal --
    // Rank the USABLE clean candidates by their max |r| against every OTHER
    // field in ALL_FIELDS (lithology axes AND other off-lithology
    // candidates) — lower is more independent.
    let mut ranked: Vec<(&str, f64)> = usable_clean
        .iter()
        .map(|&ci| {
            let name = CLEAN_OFF_LITHOLOGY_FIELDS[ci];
            let i = ALL_FIELDS.iter().position(|f| *f == name).unwrap();
            let worst = max_abs_r_vs_all[i].iter().copied().fold(0.0_f64, f64::max);
            (name, worst)
        })
        .collect();
    ranked.sort_by(|a, b| a.1.total_cmp(&b.1).then(a.0.cmp(b.0)));
    println!(
        "\n== independent-signal ranking, usable candidates (max |r| vs every other field, all seeds) =="
    );
    for (name, worst) in &ranked {
        println!("  {name:<30} max|r| {worst:.4}");
    }

    // -- Weak-pairwise-correlation clique among the USABLE clean candidates --
    // A pair is "weak" iff its max |r| across all three seeds is < 0.5
    // (the brief's own threshold). Brute-force max clique over at most 7
    // candidates (2^7 = 128 subsets) in the "weak" graph.
    const WEAK_THRESHOLD: f64 = 0.5;
    let n_usable = usable_clean.len();
    let mut best_clique: Vec<&str> = Vec::new();
    for mask in 1u32..(1 << n_usable) {
        let members: Vec<usize> = (0..n_usable)
            .filter(|&k| mask & (1 << k) != 0)
            .map(|k| usable_clean[k])
            .collect();
        if members.len() < 3 || members.len() <= best_clique.len() {
            continue;
        }
        let all_weak = members.iter().all(|&a| {
            members
                .iter()
                .all(|&b| a == b || max_abs_r_clean_pair[a][b] < WEAK_THRESHOLD)
        });
        if all_weak {
            best_clique = members
                .iter()
                .map(|&k| CLEAN_OFF_LITHOLOGY_FIELDS[k])
                .collect();
        }
    }
    // Also record the single best (lowest max |r|) weakly-correlated PAIR
    // among the usable candidates, for the brief's "exactly 2 decorrelate"
    // branch.
    let mut best_pair: Option<(&str, &str, f64)> = None;
    for &i in &usable_clean {
        for &j in &usable_clean {
            if j <= i {
                continue;
            }
            let r = max_abs_r_clean_pair[i][j];
            if r < WEAK_THRESHOLD && best_pair.is_none_or(|(_, _, best_r)| r < best_r) {
                best_pair = Some((
                    CLEAN_OFF_LITHOLOGY_FIELDS[i],
                    CLEAN_OFF_LITHOLOGY_FIELDS[j],
                    r,
                ));
            }
        }
    }

    println!("\n== branch table ==");
    println!(
        "  largest mutually-weak (|r| < {WEAK_THRESHOLD}) clique among the {} usable clean off-lithology candidates: {} members {:?}",
        n_usable,
        best_clique.len(),
        best_clique
    );
    if let Some((a, b, r)) = best_pair {
        println!("  best single weakly-correlated pair: {a} / {b}  max|r| {r:.4}");
    } else {
        println!(
            "  no pair of usable clean off-lithology candidates is weakly correlated on every seed"
        );
    }

    // Branch-pinning assertion (task-p1-report.md records the full reasoning;
    // re-derive it before trusting this verdict if this reddens). Measured
    // 2026-08-20, seeds 42/7/1234: see task-p1-report.md for the number this
    // pins — a 4-member clique (`sediment_thickness`/`geothermal_gradient_k_
    // per_km`/`paleo_envelope`/`paleo_shoreline`), landing branch "reachable".
    assert!(
        best_clique.len() >= 3,
        "the largest mutually-weak-correlation clique among the usable clean off-lithology \
         candidates fell to {} members {:?} (< 3) — re-derive the P1 branch table: \
         plural sources keyed on these fields may no longer be reachable, or may now \
         land in the 'exactly 2 decorrelate' branch instead.",
        best_clique.len(),
        best_clique,
    );

    // Second branch-pinning assertion, named on a FIXED field rather than
    // "whatever ranks first" — a dynamic pick would silently stop checking
    // the very field a mutation neutralises the moment that field drops out
    // of the ranking, which is exactly what happened while developing this
    // assertion (see task-p1-report.md's mutation-control section: the
    // clique assertion above tolerates any ONE candidate collapsing, because
    // the usable pool has redundant overlapping cliques). `sediment_thickness`
    // is the #1-ranked independent-signal candidate as measured 2026-08-20
    // (lowest max|r|, 0.3610); this assertion pins that it stays a real,
    // varying field, not a zero-variance constant, on every seed.
    let sediment_idx = ALL_FIELDS
        .iter()
        .position(|f| *f == "sediment_thickness")
        .unwrap();
    assert_eq!(
        degenerate_seed_count[sediment_idx],
        0,
        "sediment_thickness (the #1-ranked independent-signal candidate, 2026-08-20) read \
         as a zero-variance constant on {}/{} seeds over cave-bearing vertices — its \
         correlations collapsed to NaN. Re-derive the P1 independent-signal ranking: this \
         field no longer carries measurable signal.",
        degenerate_seed_count[sediment_idx],
        SEEDS.len(),
    );
}

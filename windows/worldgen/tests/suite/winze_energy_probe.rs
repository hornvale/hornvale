//! THE WINZE, Task 2M: what would it take to feed the Underworld?
//!
//! **A measurement dispatched before any design.** Changes no production
//! code. Three mechanisms in this line of work already died from a design
//! written before its substrate was measured (`ore_separation_probe.rs`,
//! `ore_viability_probe.rs` and their reports). This probe is the same
//! discipline applied to the campaign's next proposal: plural chemotrophic
//! energy sources for a subterranean people, keyed on rock chemistry
//! (`serpentinization`/low silica, `radiolysis`/granite, `sulphide
//! oxidation`/metamorphic grade, `methanogenesis`/carbonate+porosity, `iron
//! reduction`/mafic, `geothermal`/gradient×depth).
//!
//! Five measurements, M4 first because it is the stop condition: the whole
//! point of plural sources is that they are distributed *differently*, and
//! Task 1 (`ore_separation_probe.rs`) already found `prospectivity` —
//! mineral prospectivity, a sibling field on the same `MaterialBuffer` — is
//! a near-constant floor over land (seed 42: 75% of land within a 0.0067
//! band). If the six lithology axes a plural-source design would key on are
//! equally flat, or all move together, six sources collapse into one source
//! wearing six hats.
//!
//! - **M4** — do `silica`/`grain`/`induration`/`carbonate`/
//!   `metamorphic_grade`/`porosity` discriminate, over cave-bearing cells?
//!   Distributions plus the pairwise correlation matrix.
//! - **M1** — the surface calibration target: `PHOTOSYNTHATE`
//!   (`base_carrying`) and `PLANT_FORAGE` supply, over land.
//! - **M2** — the addressable-chamber count (reading `chamber_exists`, not
//!   assuming the lattice is full) and the per-chamber supply a founding
//!   needs to clear `SURVIVE_K = GENESIS_POP / COLLAPSE_PRESSURE = 5.0`.
//! - **M3** — "as lush as the Overworld": comparable-total or
//!   comparable-per-place? The two read wildly differently given the
//!   ~50-80x address-count ratio between the underworld's chamber lattice
//!   and the surface's land cells.
//! - **M5** — is `prospectivity` an abundance, or only a `[0,1]`
//!   probability? (`domains/terrain/src/lithology.rs`'s own doc comment on
//!   `prospectivity`: "the deposits campaign turns this field into point
//!   bodies; here it is a probability" — answered by reading, not measuring,
//!   and recorded here for the report rather than re-derived.)
//!
//! World-building idiom copied from `delver_depth_probe.rs`/
//! `ore_separation_probe.rs` (`build_world`/`terrain_of`, no
//! `BuildDepth::Full` — that symbol does not exist in this idiom).
//! Cave-bearing-cell enumeration copied from `underworld_lithology_probe.rs`
//! (`geo.cells()` filtered by `terrain.cave_at(cell).is_some()`, ocean cells
//! excluded first since `cave_at` already refuses them). Chamber addressing
//! copied from `delve_seating.rs::made_chambers` (`ChamberAddr { cell,
//! entrance: 0, band, slot }`, `chamber_exists` gating existence, never
//! assumed).
//!
//! Test fixture (decision 0092): calls the sculpt/fit derivation entry
//! points directly to build its own world state, once per test — the
//! sanctioned test-fixture posture the weir's spec carves out.
#![allow(clippy::disallowed_methods)]

use hornvale_astronomy::SkyPins;
use hornvale_kernel::{CellId, Seed};
use hornvale_terrain::TerrainPins;
use hornvale_worldgen::chamber::{ChamberAddr, SLOTS_PER_BAND, chamber_exists};
use hornvale_worldgen::{
    SettlementPins, SkyChoice, build_world, carrying_inputs_of, climate_of, forage_supply_field,
    terrain_of,
};

/// Seeds the campaign states its preregistrations on.
const SEEDS: [u64; 3] = [42, 7, 1234];

/// `GENESIS_POP / COLLAPSE_PRESSURE` — the viability floor a genesis
/// founding's own starvation arithmetic uses (`windows/worldgen/src/
/// history_bake.rs`: `GENESIS_POP = 10.0`, `COLLAPSE_PRESSURE = 2.0`). Local
/// copy, as every sibling probe that needs it keeps its own (both constants
/// are private to `history_bake.rs`).
const SURVIVE_K: f64 = 10.0 / 2.0;

/// The six `MaterialBuffer` axes M4 asks about, in a fixed order shared by
/// every per-seed table and the correlation matrix.
const FIELD_NAMES: [&str; 6] = [
    "silica",
    "grain",
    "induration",
    "carbonate",
    "metamorphic_grade",
    "porosity",
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

/// Pearson correlation coefficient between two equal-length series. `NaN`
/// when either series has zero variance (a genuinely undefined correlation,
/// not a zero one — a constant field correlates with nothing, including
/// itself in the usual sense, and reporting 0.0 there would hide exactly the
/// M4-STOP case this probe exists to catch).
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

/// One seed's full measurement: the six field vectors over cave-bearing
/// cells (M4), `PHOTOSYNTHATE`/`PLANT_FORAGE` over land (M1), the
/// addressable-chamber count (M2), and land-cell/surface-total figures used
/// by M2/M3. Bundled into one struct so the per-seed print block and the
/// cross-seed assertions read off the same computation rather than two
/// passes that could disagree.
struct SeedMeasurement {
    seed_value: u64,
    /// Six vectors, cave-bearing cells only, in `FIELD_NAMES` order.
    /// **PAIRED by cell** — index `i` is the same cell in every vector, which
    /// is what makes `pearson` over them meaningful. Never sort these.
    fields: [Vec<f64>; 6],
    /// The same six, each independently ascending. For percentiles ONLY;
    /// correlating these computes a maximum over pairings, not a correlation.
    sorted_fields: [Vec<f64>; 6],
    photosynthate_land: Vec<f64>,
    plant_forage_land: Vec<f64>,
    land_cells: usize,
    chamber_count: u64,
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
    let climate = climate_of(&world).expect("climate");
    let geo = terrain.geosphere();

    // M4 — the six lithology axes, over cave-bearing cells (not just land:
    // that is where the energy would be consumed).
    let mut fields: [Vec<f64>; 6] = Default::default();
    let mut cave_cells: Vec<CellId> = Vec::new();
    for cell in geo.cells() {
        if terrain.cave_at(cell).is_some() {
            cave_cells.push(cell);
        }
    }
    for &cell in &cave_cells {
        let b = terrain.material_at(cell);
        let values = [
            b.silica,
            b.grain,
            b.induration,
            b.carbonate,
            b.metamorphic_grade,
            b.porosity,
        ];
        for (axis, &v) in fields.iter_mut().zip(values.iter()) {
            axis.push(v);
        }
    }
    // PAIRED, not sorted. An earlier version sorted each axis here, before
    // `pearson` saw them — which destroys the per-cell pairing and, by the
    // rearrangement inequality, computes the MAXIMUM correlation achievable
    // over any pairing of the two multisets rather than the correlation of
    // the data. Two unrelated fields with similar marginal shapes score near
    // 1.0 under that bug. It produced a matrix with no pairwise |r| below
    // 0.5488 and a STOP verdict that did not survive being re-measured.
    //
    // Percentiles genuinely need sorted input, so the sorted copies are
    // taken separately and the paired vectors are what `pearson` reads.
    let mut sorted_fields: [Vec<f64>; 6] = Default::default();
    for (dst, src) in sorted_fields.iter_mut().zip(fields.iter()) {
        *dst = src.clone();
        dst.sort_by(f64::total_cmp);
    }

    // M1 — PHOTOSYNTHATE (`base_carrying`) and PLANT_FORAGE, over land.
    let inputs = carrying_inputs_of(geo, &terrain, &climate);
    let base_carrying = hornvale_demography::carrying_capacity(geo, &inputs);
    let forage = forage_supply_field(geo, base_carrying.as_cell_map());
    let mut photosynthate_land = Vec::new();
    let mut plant_forage_land = Vec::new();
    let mut land_cells = 0usize;
    for cell in geo.cells() {
        if terrain.is_ocean(cell) {
            continue;
        }
        land_cells += 1;
        photosynthate_land.push(base_carrying.at(cell));
        plant_forage_land.push(*forage.get(cell));
    }
    photosynthate_land.sort_by(f64::total_cmp);
    plant_forage_land.sort_by(f64::total_cmp);

    // M2 — addressable chambers: read `chamber_exists` at every (cell, band,
    // slot) a cave-bearing cell's lattice could hold, rather than assuming
    // the lattice is full (spec: `chamber.rs`'s own existence gate is a
    // fixed-density draw, `EXISTENCE_DENSITY = 0.5`, never a certainty).
    let mut chamber_count: u64 = 0;
    for &cell in &cave_cells {
        let cave = terrain
            .cave_at(cell)
            .expect("cave_cells only holds cave-bearing cells");
        let gradient = terrain.geothermal_gradient_at(cell);
        for band in 0u8..5 {
            for slot in 0..SLOTS_PER_BAND {
                let addr = ChamberAddr {
                    cell,
                    entrance: 0,
                    band,
                    slot,
                };
                if chamber_exists(seed, &cave, gradient, addr) {
                    chamber_count += 1;
                }
            }
        }
    }

    SeedMeasurement {
        seed_value,
        fields,
        sorted_fields,
        photosynthate_land,
        plant_forage_land,
        land_cells,
        chamber_count,
    }
}

/// claim: readout(off-gate, heavy:, prints only, plus branch-pinning
/// assertions) — M1-M5 of task-2m-brief.md, seeds 42/7/1234. Ruling 2: this
/// is a decision instrument, not a pass/fail gate on a chosen constant.
#[test]
#[ignore = "heavy: live-worldgen battery; deferred from the commit gate to the heavy set (decision 0132)"]
fn winze_energy_probe() {
    // Cross-seed accumulators for the branch-table assertions below.
    let mut max_abs_corr_over_all_seeds: f64 = 0.0;
    let mut min_abs_corr_over_all_seeds: f64 = f64::INFINITY;
    let mut min_iqr_over_range_over_all_seeds: f64 = f64::INFINITY;
    let mut pairs_total: u32 = 0;
    let mut pairs_at_least_0_8: u32 = 0;
    let mut any_field_measured = false;

    for seed_value in SEEDS {
        let m = measure(seed_value);
        println!(
            "\n== seed {} ==  cave-bearing cells {}  land cells {}",
            m.seed_value,
            m.fields[0].len(),
            m.land_cells
        );

        // --- M4: distributions ---
        for (name, v) in FIELD_NAMES.iter().zip(m.sorted_fields.iter()) {
            any_field_measured |= !v.is_empty();
            let p90 = pct(v, 0.90);
            let top_decile: Vec<f64> = v.iter().copied().filter(|&x| x >= p90).collect();
            let mut top_decile_sorted = top_decile.clone();
            top_decile_sorted.sort_by(f64::total_cmp);
            let range =
                v.last().copied().unwrap_or(f64::NAN) - v.first().copied().unwrap_or(f64::NAN);
            let iqr = pct(v, 0.75) - pct(v, 0.25);
            let iqr_over_range = if range > 0.0 { iqr / range } else { 0.0 };
            min_iqr_over_range_over_all_seeds =
                min_iqr_over_range_over_all_seeds.min(iqr_over_range);
            println!(
                "  [M4] {name:<18} min {:.4}  p25 {:.4}  p50 {:.4}  p75 {:.4}  p90 {:.4}  p99 {:.4}  max {:.4}  top-decile distinct {}/{}",
                v.first().copied().unwrap_or(f64::NAN),
                pct(v, 0.25),
                pct(v, 0.50),
                pct(v, 0.75),
                p90,
                pct(v, 0.99),
                v.last().copied().unwrap_or(f64::NAN),
                distinct_count(&top_decile_sorted),
                top_decile.len(),
            );
        }

        // --- M4: pairwise correlation matrix ---
        println!("  [M4] correlation matrix:");
        print!("           ");
        for name in &FIELD_NAMES {
            print!("{:>10}", &name[..name.len().min(10)]);
        }
        println!();
        for (i, name_i) in FIELD_NAMES.iter().enumerate() {
            print!("  {name_i:<9}");
            for j in 0..FIELD_NAMES.len() {
                let r = if i == j {
                    1.0
                } else {
                    pearson(&m.fields[i], &m.fields[j])
                };
                if i < j && r.is_finite() {
                    max_abs_corr_over_all_seeds = max_abs_corr_over_all_seeds.max(r.abs());
                    min_abs_corr_over_all_seeds = min_abs_corr_over_all_seeds.min(r.abs());
                    pairs_total += 1;
                    if r.abs() >= 0.8 {
                        pairs_at_least_0_8 += 1;
                    }
                }
                print!("{r:>10.4}");
            }
            println!();
        }

        // --- M1 ---
        println!(
            "  [M1] PHOTOSYNTHATE (land): min {:.4}  p25 {:.4}  p50/median {:.4}  p75 {:.4}  p90 {:.4}  max {:.4}",
            m.photosynthate_land.first().copied().unwrap_or(f64::NAN),
            pct(&m.photosynthate_land, 0.25),
            pct(&m.photosynthate_land, 0.50),
            pct(&m.photosynthate_land, 0.75),
            pct(&m.photosynthate_land, 0.90),
            m.photosynthate_land.last().copied().unwrap_or(f64::NAN),
        );
        println!(
            "  [M1] PLANT_FORAGE   (land): min {:.4}  p25 {:.4}  p50/median {:.4}  p75 {:.4}  p90 {:.4}  max {:.4}",
            m.plant_forage_land.first().copied().unwrap_or(f64::NAN),
            pct(&m.plant_forage_land, 0.25),
            pct(&m.plant_forage_land, 0.50),
            pct(&m.plant_forage_land, 0.75),
            pct(&m.plant_forage_land, 0.90),
            m.plant_forage_land.last().copied().unwrap_or(f64::NAN),
        );

        // --- M2 ---
        let surface_total: f64 = m.photosynthate_land.iter().sum();
        let surface_per_cell = surface_total / m.land_cells.max(1) as f64;
        let total_underworld_needed_at_survive_k = SURVIVE_K * m.chamber_count as f64;
        println!(
            "  [M2] addressable chambers: {}  (SURVIVE_K {:.1} per chamber -> total {:.2} needed to found everywhere)",
            m.chamber_count, SURVIVE_K, total_underworld_needed_at_survive_k,
        );

        // --- M3: per-place parity vs total parity ---
        let per_place_parity_total = surface_per_cell * m.chamber_count as f64;
        let per_place_parity_ratio = if surface_total > 0.0 {
            per_place_parity_total / surface_total
        } else {
            f64::NAN
        };
        let total_parity_per_chamber = surface_total / m.chamber_count.max(1) as f64;
        let total_parity_ratio = if surface_per_cell > 0.0 {
            total_parity_per_chamber / surface_per_cell
        } else {
            f64::NAN
        };
        println!(
            "  [M3] surface total {:.2} over {} land cells (mean/cell {:.6})",
            surface_total, m.land_cells, surface_per_cell,
        );
        println!(
            "  [M3] per-place parity: underworld total would need to be {:.2} ({:.2}x surface total, = chambers/land_cells)",
            per_place_parity_total, per_place_parity_ratio,
        );
        println!(
            "  [M3] total parity:     underworld per-chamber would need to be {:.6} ({:.4}x surface per-cell, = land_cells/chambers = {:.4})",
            total_parity_per_chamber,
            total_parity_ratio,
            m.land_cells as f64 / m.chamber_count.max(1) as f64,
        );
    }

    // Harness guard: a survey that saw no cave-bearing cells measured
    // nothing, and every table above would be a page of NaNs read as data.
    assert!(
        any_field_measured,
        "the survey found no cave-bearing cells across {} seeds — M4 is measuring nothing",
        SEEDS.len()
    );

    let pct_high_corr = 100.0 * pairs_at_least_0_8 as f64 / pairs_total.max(1) as f64;
    println!(
        "\n== M4 cross-seed summary ==  |r| min {:.4}  max {:.4}  pairs >= 0.8: {}/{} ({:.1}%)  min (IQR/range) {:.4}",
        min_abs_corr_over_all_seeds,
        max_abs_corr_over_all_seeds,
        pairs_at_least_0_8,
        pairs_total,
        pct_high_corr,
        min_iqr_over_range_over_all_seeds,
    );

    // M4 branch-pinning assertions (task-2m-report.md records the full
    // branch-table reasoning; re-derive it before trusting any of M4's
    // conclusions if either of these reddens).
    //
    // 1. RULES OUT branch 1 ("every field near-constant, as prospectivity
    //    is"). Task 1's `ore_separation_probe.rs` measured prospectivity's
    //    IQR at ~0.0067 against a ~0.68 range (IQR/range ~1%) on seed 42 —
    //    that is what "near-constant" looks like in this codebase. Measured
    //    here (2026-08-19, seeds 42/7/1234): the tightest of the 18
    //    field x seed combinations is `grain` on seed 42 at IQR/range
    //    0.1740 — over 17x prospectivity's floor. The six lithology axes
    //    are not the prospectivity-style collapse.
    assert!(
        min_iqr_over_range_over_all_seeds > 0.05,
        "a lithology field's IQR/range fell to {min_iqr_over_range_over_all_seeds:.4} \
         (<= 0.05) on some seed — this is the prospectivity-style near-constant \
         collapse (branch 1). Re-derive the M4 branch table: 'plural sources not \
         reachable from lithology' may now be the answer."
    );
    // 2. LANDS branch 2 ("fields vary but are strongly inter-correlated").
    //    Measured: no pairwise |r| below 0.5488 (seed 42, carbonate vs
    //    metamorphic_grade) across all 45 seed x pair combinations, and a
    //    majority (29/45, 64%) are >= 0.8. `induration` and `porosity` are
    //    literally defined as weighted sums of the other buffer axes
    //    (`domains/terrain/src/lithology.rs`'s `induration_at`/
    //    `assemble_material`), so the correlation is structural, not
    //    incidental to these three seeds. Guard: if some future change
    //    genuinely decouples the six axes, this reddens and the branch
    //    table must be re-walked (it would then land in branch 3, and
    //    plural sources would become reachable).
    assert!(
        min_abs_corr_over_all_seeds > 0.5,
        "a pairwise |correlation| fell to {min_abs_corr_over_all_seeds:.4} (<= 0.5) — \
         that would be genuine independent signal between two lithology axes. \
         Re-derive the M4 branch table: branch 2 (STOP, co-located sources) may no \
         longer be the right verdict."
    );
    assert!(
        pairs_at_least_0_8 * 2 >= pairs_total,
        "only {pairs_at_least_0_8}/{pairs_total} field pairs now correlate >= 0.8 \
         (previously a majority, 29/45) — re-derive the M4 branch table."
    );
}

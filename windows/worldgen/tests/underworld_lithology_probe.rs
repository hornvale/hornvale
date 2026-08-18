//! THE UNDERWORLD, Task 1b: what does the INPUT domain of a depth budget look
//! like?
//!
//! **This probe shaped the derivation, so it is in the record.** Spec §4.0
//! constraint 2 says to prefer a pure function of fields terrain already owns
//! and to add a stream draw only "if a pure derivation proves degenerate".
//! That is not answerable without knowing how the candidate input fields are
//! actually distributed over cave-bearing cells, so this was run *before* any
//! formula was written, and its result chose the functional form. A control
//! that steered a design decision has to be reproducible from the record; this
//! file is that reproduction.
//!
//! **What it establishes, and what it therefore costs.** The `MaterialBuffer`
//! axes are far more quantized than their `[0,1]` types suggest: `carbonate`
//! is effectively two-valued, `porosity` narrow within a kind,
//! `metamorphic_grade` five-valued, and `depth_to_basement_m` has p50 = 0 on
//! every seed. Only `induration` (through `grain = 0.4 + 0.5 * crust_age`)
//! varies continuously and widely. A product of near-uniform factors would
//! therefore have spanned ~3-8x, against the ~25x the delve ladder's ΔT
//! buckets need to be occupied. That is what pointed
//! `hornvale_terrain::cave_depth` at a **log-linear** strength scale rather
//! than a product of ratios, and it is also why `depth_to_basement_m` is not a
//! multiplicative term anywhere — with p50 = 0 it would hand half the world a
//! zero reach.
//!
//! **CONSEQUENCE A LATER READER MUST NOT MISS.** The derivation was chosen to
//! produce a span, and `underworld_ladder_probe`'s post-1b table then measured
//! that it did. **That is a fit, not a prediction that held**, and Step 5's ΔT
//! spread is therefore *not* independent confirmation of the derivation. It is
//! a legitimate thing to have done — a budget with no spread would have been
//! useless — but it is only evidence that the construction works as
//! constructed. The *physics* (ISRM log-spaced strength grades, lithostatic
//! closure) is what carries the claim that the shape is right, and that is
//! argued in `cave_depth.rs`'s own doc, not measured here.
//!
//! It asserts only harness sanity: the result is the printed table.
//!
//! ## Measured, 2026-08-16, seeds 42 / 7 / 1234, at the production globe level
//!
//! ```text
//! seed 42 Karst: n=576 unconformity=414
//!     carbonate      p05=0.0500 p25=0.7000 p50=0.7000 p75=0.7000 p95=0.7000 max=0.7000
//!     porosity       p05=0.4677 p25=0.7809 p50=0.8011 p75=0.8187 p95=0.8187 max=0.8187
//!     induration     p05=0.4745 p25=0.4922 p50=0.5016 p75=0.5292 p95=0.6745 max=0.8774
//!     silica         p05=0.4619 p25=0.4889 p50=0.5215 p75=0.6809 p95=0.7357 max=0.7549
//!     grain          p05=0.6226 p25=0.6991 p50=0.7581 p75=0.8960 p95=0.8960 max=0.8960
//!     metamorphic    p05=0.0000 p25=0.0000 p50=0.0000 p75=0.0000 p95=0.5000 max=1.0000
//!     dtb_m          p05=0.0000 p25=0.0000 p50=0.0000 p75=0.4160 p95=62.2242 max=726.2109
//! seed 42 LavaTube: n=14 unconformity=0
//!     carbonate      p05=0.0500 p50=0.0500 max=0.0500
//!     porosity       p05=0.3738 p50=0.3738 max=0.3738
//!     induration     p05=0.3900 p50=0.3900 max=0.3900
//!     grain          p05=0.2000 p50=0.2000 max=0.2000
//!     dtb_m          p05=0.0000 p25=0.1665 p50=871.2140 p75=1824.8730 max=2293.4064
//! seed 42 Fracture: n=284 unconformity=63
//!     carbonate      p05=0.0500 p25=0.0500 p50=0.0500 p75=0.0500 p95=0.7000 max=0.7000
//!     porosity       p05=0.0548 p25=0.0558 p50=0.0562 p75=0.3738 p95=0.3812 max=0.3812
//!     induration     p05=0.3900 p25=0.8548 p50=0.8774 p75=0.8898 p95=0.9016 max=0.9292
//!     metamorphic    p05=0.0000 p25=1.0000 p50=1.0000 p75=1.0000 p95=1.0000 max=1.0000
//!     dtb_m          p05=0.0000 p25=0.0000 p50=0.0000 p75=0.5964 p95=249.5986 max=1785.3323
//! seed 7 Karst: n=789 unconformity=48
//!     carbonate      p05=0.0500 p25=0.7000 p50=0.7000 p95=0.7000 max=0.7000
//!     porosity       p05=0.3810 p25=0.6688 p50=0.7649 p75=0.7898 p95=0.7918 max=0.8048
//!     induration     p05=0.4543 p25=0.4556 p50=0.4860 p75=0.5070 p95=0.7858 max=0.9192
//!     dtb_m          p05=0.0000 p25=0.0000 p50=0.0000 p75=0.6714 p95=142.3190 max=683.8011
//! seed 7 Fracture: n=879 unconformity=266
//!     induration     p05=0.7858 p25=0.8682 p50=0.8860 p75=0.9083 p95=0.9192 max=0.9192
//!     porosity       p05=0.0523 p25=0.0540 p50=0.0559 p75=0.0560 p95=0.3804 max=0.3811
//! seed 1234 Karst: n=615 unconformity=427
//!     induration     p05=0.4456 p25=0.4941 p50=0.5268 p75=0.5663 p95=0.8055 max=0.9268
//! seed 1234 Fracture: n=619 unconformity=337
//!     induration     p05=0.7663 p25=0.8663 p50=0.8936 p75=0.9268 p95=0.9275 max=0.9275
//! ```
//!
//! (Abridged in this doc to the axes the derivation reads plus the ones it
//! rejected; the run prints all eight for every kind on every seed.)
//!
//! Test fixture (decision 0092): calls the composition-root entry points
//! directly, the sanctioned posture for this crate's live-worldgen batteries.
#![allow(clippy::disallowed_methods)]

use hornvale_astronomy::SkyPins;
use hornvale_terrain::{CaveKind, TerrainPins};
use hornvale_worldgen::{
    BuildDepth, SettlementPins, SkyChoice, WorldComponents, build_world_to_with_artifacts,
};

/// Seeds this campaign preregisters on (spec §5) — the same three
/// `underworld_ladder_probe` uses, so the two readouts describe one population.
const SEEDS: [u64; 3] = [42, 7, 1234];

/// Percentile of an ascending slice.
fn pct(sorted: &[f64], q: f64) -> f64 {
    if sorted.is_empty() {
        return f64::NAN;
    }
    sorted[((sorted.len() - 1) as f64 * q).round() as usize]
}

/// Print one axis's spread.
fn report(name: &str, mut v: Vec<f64>) {
    v.sort_by(f64::total_cmp);
    println!(
        "    {name:<14} n={:<6} p05={:.4} p25={:.4} p50={:.4} p75={:.4} p95={:.4} max={:.4}",
        v.len(),
        pct(&v, 0.05),
        pct(&v, 0.25),
        pct(&v, 0.50),
        pct(&v, 0.75),
        pct(&v, 0.95),
        pct(&v, 1.0)
    );
}

/// claim: readout(off-gate, heavy:, prints only, harness guards excepted) —
/// the distribution of every `MaterialBuffer` and column axis a pure depth
/// derivation could read, over cave-bearing land cells, split by cave kind.
/// The input-domain survey that chose `cave_depth`'s functional form; not a
/// gate on any value.
#[test]
#[ignore = "heavy: live-worldgen battery; deferred from the commit gate to the heavy set (decision 0132)"]
fn what_does_a_depth_budget_have_to_read() {
    let wc = WorldComponents::assemble().expect("canonical registries are well-formed");
    let mut total = 0usize;
    for seed_value in SEEDS {
        let seed = hornvale_kernel::Seed(seed_value);
        let artifacts = build_world_to_with_artifacts(
            seed,
            &SkyPins::default(),
            SkyChoice::Generated,
            &TerrainPins::default(),
            &SettlementPins::default(),
            &wc,
            BuildDepth::Terrain,
        )
        .expect("probe seed builds");
        let terrain = artifacts
            .terrain
            .expect("terrain is Some at BuildDepth::Terrain");
        let geo = terrain.geosphere();

        for kind in [CaveKind::Karst, CaveKind::LavaTube, CaveKind::Fracture] {
            let mut carbonate = Vec::new();
            let mut porosity = Vec::new();
            let mut induration = Vec::new();
            let mut silica = Vec::new();
            let mut grain = Vec::new();
            let mut meta = Vec::new();
            let mut dtb = Vec::new();
            let mut soil = Vec::new();
            let mut unconformities = 0usize;
            for cell in geo.cells() {
                if terrain.is_ocean(cell) {
                    continue;
                }
                let Some(cave) = terrain.cave_at(cell) else {
                    continue;
                };
                if cave.kind != kind {
                    continue;
                }
                let b = terrain.material_at(cell);
                let col = terrain.column_at(cell);
                carbonate.push(b.carbonate);
                porosity.push(b.porosity);
                induration.push(b.induration);
                silica.push(b.silica);
                grain.push(b.grain);
                meta.push(b.metamorphic_grade);
                dtb.push(col.depth_to_basement_m);
                soil.push(col.bands[1].top_depth_m);
                if col.unconformity {
                    unconformities += 1;
                }
            }
            println!(
                "seed {seed_value} {kind:?}: n={} unconformity={unconformities}",
                carbonate.len()
            );
            total += carbonate.len();
            report("carbonate", carbonate);
            report("porosity", porosity);
            report("induration", induration);
            report("silica", silica);
            report("grain", grain);
            report("metamorphic", meta);
            report("dtb_m", dtb);
            report("soil_m", soil);
        }
    }

    // Harness guard, in the spirit of `hollow_readout`'s: a survey that saw no
    // caves measured nothing, and its table would read as a set of NaNs rather
    // than as a finding.
    assert!(
        total > 0,
        "the survey found no cave-bearing cells across {} seeds — it is \
         measuring nothing",
        SEEDS.len()
    );
}

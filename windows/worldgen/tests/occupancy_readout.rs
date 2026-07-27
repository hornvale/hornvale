//! The occupancy readout (The Vacancy, T3): a committed measurement of
//! where each of the roster's kinds actually lives, over seeds 1..=30 at
//! full build depth. Authored condition optima (spec §5) only mean something
//! relative to the distribution of land a world actually produces — this is
//! the instrument that makes an optimum checkable against that distribution
//! instead of against the author's intuition. A prior campaign shipped the
//! kobold's elevation optimum at or above the highest land on most worlds
//! (its "exclusive highland stronghold" was unoccupiable) undetected for
//! campaigns; this readout exists so that failure mode is now a diffable CSV
//! rather than a silent gap. Tasks 7, 8, and 9 author every new kind's
//! condition niche against percentiles read out of the committed fixture
//! this file regenerates (`fixtures/occupancy.csv`).
//!
//! World-building idiom reused verbatim from `demesne.rs`/`waterline_probe.rs`
//! (`hornvale_worldgen::build_world`, `WorldComponents::assemble`,
//! `terrain_of`/`climate_of`/`sky_of` "reconstruct, never store"). The
//! per-species K comes from [`hornvale_worldgen::niche_per_species_k`],
//! whose returned `u32` is a **build-local dense index, not identity** (see
//! its doc comment) — it is the position in the `species_biosphere` slice
//! passed in, so the index -> [`hornvale_kernel::KindId`] mapping here is
//! rebuilt from that exact same `wc.biosphere.iter()` ordering, once, and
//! reused for every seed (the roster's ordering does not vary by seed).
//!
//! The viability floor is [`hornvale_demography::FLOOR`] (1e-6) — the same
//! "AUTHORED prior (task A14)" constant `hornvale_demography::coexist::pack`
//! already uses to decide a share is ecological noise rather than presence
//! (`domains/demography/src/coexist.rs`), reused here unchanged rather than
//! inventing a second threshold, per this task's brief (Task 4 reuses the
//! identical value).

use hornvale_astronomy::SkyPins;
use hornvale_kernel::{KindId, Seed, quantize};
use hornvale_terrain::TerrainPins;
use hornvale_worldgen::{
    SettlementPins, SkyChoice, WorldComponents, build_world, climate_of, niche_per_species_k,
    sky_of, terrain_of,
};
use std::collections::BTreeMap;
use std::ops::RangeInclusive;

/// The viability floor below which a cell's K is ecological noise rather
/// than presence — [`hornvale_demography::FLOOR`], unchanged. Task 4 reuses
/// this identical value; two different floors would let a kind pass one
/// test and fail the other.
const VIABILITY_FLOOR: f64 = hornvale_demography::FLOOR;

/// Linear-interpolation percentile over an ascending-sorted slice (nearest
/// pair, weighted by fractional rank) — a plain float computation, not a
/// serialization boundary, so no quantization here (quantize happens once,
/// at render, per the constitutional emit-only rule).
fn percentile(sorted: &[f64], p: f64) -> f64 {
    match sorted.len() {
        0 => 0.0,
        1 => sorted[0],
        n => {
            let rank = (p / 100.0) * (n - 1) as f64;
            let lo = rank.floor() as usize;
            let hi = rank.ceil() as usize;
            if lo == hi {
                sorted[lo]
            } else {
                let frac = rank - lo as f64;
                sorted[lo] + (sorted[hi] - sorted[lo]) * frac
            }
        }
    }
}

/// Render the occupancy readout CSV for every seed in `seeds`, one row per
/// (kind, biome) that the kind occupies at least once across the whole
/// sweep. Pure aside from world genesis: same `seeds` in, byte-identical
/// string out (the drift check below depends on this).
fn render_occupancy_readout(seeds: RangeInclusive<u64>) -> String {
    let wc = WorldComponents::assemble().expect("canonical registries are well-formed");
    // The build-local dense index -> KindId mapping, built ONCE from the
    // exact same `wc.biosphere` ordering passed to `niche_per_species_k`
    // below (ascending-KindId order, per `niche_per_species_k`'s doc
    // comment) so every seed's returned `u32` tags resolve to the correct
    // kind. Verified below in the mandatory inspection step (kobold ->
    // highland, no kind -> predominantly marine).
    let kinds: Vec<KindId> = wc.biosphere.iter().map(|(k, _)| *k).collect();
    let bios: Vec<&hornvale_species::BiosphereTraits> =
        wc.biosphere.iter().map(|(_, b)| b).collect();

    // Accumulated across every seed in the sweep, keyed by (kind, biome).
    // `occupied_k`: the K values of cells at/above the viability floor (the
    // presence distribution mean_k/p50_k/p95_k are computed over).
    // `biome_k_sum`: this biome's total K for the kind, unfiltered by the
    // floor (the numerator of `share_of_kind_k` — a biome's fraction of the
    // kind's total carrying capacity, not merely its occupied-cell count).
    // `kind_k_total`: the kind's world total K, unfiltered (the denominator).
    let mut occupied_k: BTreeMap<(&'static str, &'static str), Vec<f64>> = BTreeMap::new();
    let mut biome_k_sum: BTreeMap<(&'static str, &'static str), f64> = BTreeMap::new();
    let mut kind_k_total: BTreeMap<&'static str, f64> = BTreeMap::new();

    for seed in seeds {
        let world = build_world(
            Seed(seed),
            &SkyPins::default(),
            SkyChoice::Generated,
            &TerrainPins::default(),
            &SettlementPins::default(),
        )
        .unwrap_or_else(|e| panic!("seed {seed} failed to build: {e:?}"));

        let terrain = terrain_of(&world).expect("terrain reconstructs");
        let climate = climate_of(&world).expect("climate reconstructs");
        let sky = sky_of(&world).expect("sky reconstructs");
        let geo = terrain.geosphere();
        let system = sky
            .system()
            .unwrap_or_else(|| panic!("seed {seed} has a generated star system"));
        let insolation = hornvale_astronomy::insolation_rel(&system.star, &system.anchor);
        let obliquity = system.anchor.obliquity.get();
        let regime = match system.anchor.rotation {
            hornvale_astronomy::Rotation::Spinning { day, .. } => {
                hornvale_climate::RotationRegime::Spinning { day_std: day.get() }
            }
            hornvale_astronomy::Rotation::Locked => hornvale_climate::RotationRegime::Locked,
        };

        let ks = niche_per_species_k(
            geo, &terrain, &climate, obliquity, insolation, &regime, &bios,
        );
        let biome_map = climate.biome_map();

        for (tag, k) in &ks {
            let kind = kinds[*tag as usize].0;
            for cell in geo.cells() {
                let v = *k.get(cell);
                *kind_k_total.entry(kind).or_insert(0.0) += v;
                let biome = biome_map.get(cell).name();
                *biome_k_sum.entry((kind, biome)).or_insert(0.0) += v;
                if v >= VIABILITY_FLOOR {
                    occupied_k.entry((kind, biome)).or_default().push(v);
                }
            }
        }
    }

    // `occupied_k` is a `BTreeMap<(&str, &str), _>`, so iterating it already
    // yields rows in ascending (kind, biome) order — no separate sort, and
    // no float ordering anywhere in that order (percentile's internal sort
    // uses `total_cmp` but never influences row order).
    let mut out = String::from("kind,biome,cells_occupied,share_of_kind_k,mean_k,p50_k,p95_k\n");
    for (&(kind, biome), values) in &occupied_k {
        let cells_occupied = values.len() as u32;
        let total_kind = *kind_k_total.get(kind).unwrap_or(&0.0);
        let biome_sum = *biome_k_sum.get(&(kind, biome)).unwrap_or(&0.0);
        let share = if total_kind > 0.0 {
            biome_sum / total_kind
        } else {
            0.0
        };
        let mean = values.iter().sum::<f64>() / values.len() as f64;
        let mut sorted = values.clone();
        sorted.sort_by(|a, b| a.total_cmp(b));
        let p50 = percentile(&sorted, 50.0);
        let p95 = percentile(&sorted, 95.0);
        // Quantize at emit only (constitutional rule): every computation
        // above ran at full precision, and only the rendered string is
        // rounded to 8 significant digits, exactly as `render_csv` does.
        out.push_str(&format!(
            "{kind},{biome},{cells_occupied},{},{},{},{}\n",
            quantize(share),
            quantize(mean),
            quantize(p50),
            quantize(p95),
        ));
    }
    out
}

#[test]
#[ignore = "heavy: live-worldgen battery (minutes); deferred from the commit gate to make gate-full"]
fn occupancy_readout_is_current() {
    let rendered = render_occupancy_readout(1..=30);
    let committed = include_str!("fixtures/occupancy.csv");
    assert_eq!(
        rendered, committed,
        "occupancy readout drifted - if this is intended, rewrite the fixture \
         in the SAME commit as the change that drifted it"
    );
}

#[test]
// Deliberately NOT a `heavy:` reason. The heavy tier is what `make gate-full`
// runs, and this test WRITES the fixture — running it there would have CI
// silently rewrite the artifact the drift check above exists to check, so a
// drifted readout would self-heal instead of failing. Run it by hand when a
// change is meant to move the readout. Same rationale as the census fixtures'
// non-heavy reasons.
#[ignore = "regenerates the committed occupancy fixture; run by hand - the drift check above is the gate"]
fn regenerate_occupancy_readout() {
    let rendered = render_occupancy_readout(1..=30);
    std::fs::write(
        concat!(env!("CARGO_MANIFEST_DIR"), "/tests/fixtures/occupancy.csv"),
        rendered,
    )
    .expect("write occupancy.csv fixture");
}

//! The Generalist, Task 1: the land distribution human's `ConditionNiche` is
//! authored against, and the pre-human per-people fit baseline the campaign's
//! preregistered readout compares to.
//!
//! **Task 5b extension (the re-authoring, 2026-08-04):** Task 1 measured only
//! elevation. An attribution run showed human's authored widths were
//! *narrower* than goblin's on temperature and elevation, contradicting the
//! "widest curve in the roster" claim, so the widths needed re-authoring
//! against measured quantiles on all four axes, not just elevation. This
//! file now reports p5/p15/p50/p85/p95 for temperature, moisture,
//! insolation, and elevation, all four read from the identical
//! [`hornvale_worldgen::Substrate`] `per_species_suitability` scores a
//! `ConditionNiche` against - see [`AxisSamples`]'s doc comment.
//!
//! **Task 6 extension (the preregistered readout, 2026-08-04):** the campaign's
//! actual measurement, over the roster WITH human folded in
//! ([`PEOPLES_WITH_HUMAN`], six peoples, not [`PEOPLES`]'s five - `measure_one`
//! now takes the peoples list as a parameter so Task 1's pre-human baseline and
//! Task 6's post-human readout can share one world-building body without
//! silently changing what "settleable land" means for Task 1's own frozen
//! assertions). Two distinct quantities are measured and reported separately,
//! per the task's ruling on not conflating them:
//!
//! - **Fit (K)** - `per_species_suitability`'s raw per-species score, computed
//!   independently per species. "Best-fit people on a stronghold" (H2) is a
//!   comparison of this quantity.
//! - **Competitive share** - `hornvale_demography::coexist::cell_share`'s
//!   overlap-weighted `K^β` softmax over whichever species are present at a
//!   cell, at the frozen [`hornvale_demography::BETA`]/[`hornvale_demography::FLOOR`].
//!   This is what §4 of the design spec calls "human's per-cell competitive
//!   share from the coexistence packer" (H1's correlation axis, H3's
//!   majority-share test). It depends on who else is present at the cell;
//!   fit does not. `measure_one` computes it directly via `cell_share` (not
//!   via `hornvale_demography::coexist::pack`'s final density), since
//!   `pack`'s density additionally divides by home range and applies
//!   trophic coupling - neither of which is part of "the share the K^β
//!   softmax produces," and peoples have no `predation` entries against each
//!   other so trophic coupling would be a no-op for this roster in any case.
//!
//! **Stronghold bands (H2).** Kobold's and bugbear's bands are elevation
//! thresholds stated directly by §4 of the design spec (kobold ≥ 3000 m,
//! bugbear ≤ 500 m) and echoed in each kind's own `ConditionNiche` doc
//! comment (`domains/species/src/lib.rs`). Hobgoblin's and gnoll's bands are
//! read from their own doc comments rather than forced into the same
//! elevation-only shape:
//!
//! - **Hobgoblin** - its doc comment states its elevation response's
//!   `optimum ± width` band (600 ± 1400 m) "spans p10-p60" of settleable
//!   land, "the plains band between bugbear's lowland (p15) and kobold's
//!   highland (p79)" - so the band here is elevation in `[p10, p60]` of the
//!   aggregated elevation sample, computed the same way Task 1/5b compute
//!   every other percentile in this file.
//! - **Gnoll** - its doc comment ties it explicitly to
//!   `domains/climate/src/biome.rs`'s `classify_land` Desert criterion
//!   ("the same climate tile `giant_scorpion_condition_niche` claims...
//!   Desert requires `temp_c >= 20` and `moisture < 0.2`"), not an elevation
//!   band (gnoll's own elevation devotion, 0.40, is its weakest axis) - so
//!   the band here is `temperature_c >= 20.0 && moisture < 0.2`, the
//!   published predicate rather than a hand-rederived elevation stand-in.
//!
//! Ignored: builds 30 worlds. Reason token `heavy:` puts it in the heavy tier
//! (cli/tests/heavy_tier.rs), not the commit gate.
//!
//! World-building idiom reused verbatim from
//! `windows/worldgen/tests/non_void_roster.rs` (`hornvale_worldgen::build_world`,
//! `WorldComponents::assemble`, `terrain_of`/`climate_of`/`sky_of`
//! "reconstruct, never store"). The per-species K comes from
//! [`hornvale_worldgen::per_species_suitability`], whose returned `u32` is a
//! **build-local dense index, not identity** (see its doc comment) - it is
//! the position in the `species_biosphere` slice passed in, so the index ->
//! [`hornvale_kernel::KindId`] mapping here is rebuilt fresh, per seed, from
//! that exact same `wc.biosphere.iter()` ordering.
//!
//! **"Settleable land"** is not a second, independently-chosen filter: K is 0
//! on every submerged cell for the whole roster today (`per_species_suitability`'s
//! own doc, The Tumult's land mask), so "does at least one of the peoples
//! passed to `measure_one` clear [`VIABILITY_FLOOR`] here" already separates
//! occupiable land from both ocean and the uninhabitable land the condition
//! niches themselves exclude - the same viability test `non_void_roster.rs`
//! applies per-kind, applied here per-cell across the whole roster passed in.
//! Both outputs of `measure_one` (the axis samples and every people's fit/share
//! samples) are drawn from that identical filtered cell set, seed by seed, so
//! the mean fits/shares are means over the same population the elevation
//! quantiles describe - the spec's D3 concern ("a quantile from the wrong
//! population carries the authority of evidence") applies equally to a mean.
//! Task 1's five-people population and Task 6's six-people population are
//! therefore two DIFFERENT "settleable land" sets (human's wide niche can only
//! grow the set), each internally consistent for the test that uses it.
//!
//! Test fixture (decision 0092): calls the sculpt/fit derivation entry
//! points directly to build its own world state, once per test - the
//! sanctioned test-fixture posture the weir's spec carves out.
#![allow(clippy::disallowed_methods)]

use hornvale_astronomy::SkyPins;
use hornvale_kernel::{KindId, Seed};
use hornvale_terrain::TerrainPins;
use hornvale_worldgen::{
    SettlementPins, SkyChoice, WorldComponents, build_world, climate_of, per_species_suitability,
    sky_of, substrate_field, terrain_of,
};
use std::collections::BTreeMap;

/// The viability floor below which a cell's K is ecological noise rather
/// than presence - [`hornvale_demography::FLOOR`], unchanged. Reused
/// identical to `non_void_roster.rs`'s `VIABILITY_FLOOR`; two different
/// floors would let a kind pass one test and disagree with the other about
/// which land is "settleable".
const VIABILITY_FLOOR: f64 = hornvale_demography::FLOOR;

const SEEDS: std::ops::RangeInclusive<u64> = 1..=30;

/// The pre-human roster: `wc.biosphere` holds all 29 kinds (fauna and
/// peoples together), but Task 1's baseline is about the five **peoples**
/// human joins - so `measure_one` filters `wc.biosphere` down to exactly
/// these before ever calling `per_species_suitability`, rather than measuring
/// "settleable" against the whole 29-kind fauna+peoples roster. Frozen as
/// Task 1 shipped it; Task 6's readout uses [`PEOPLES_WITH_HUMAN`] instead,
/// never this constant, so this population and its "settleable land" never
/// silently changes meaning underneath the existing pre-human assertions.
const PEOPLES: [&str; 5] = ["bugbear", "gnoll", "goblin", "hobgoblin", "kobold"];

/// The post-human roster (Task 6): [`PEOPLES`] plus `"human"` - the exact set
/// §4 of the design spec measures the readout over ("human's per-cell
/// competitive share... against the five existing peoples' shares on the
/// same cells").
const PEOPLES_WITH_HUMAN: [&str; 6] =
    ["bugbear", "gnoll", "goblin", "hobgoblin", "human", "kobold"];

/// The four condition axes, sampled over the settleable-cell population, in
/// the identical frame `per_species_suitability` scores a `ConditionNiche`
/// against - each field is a direct per-cell read of
/// [`hornvale_worldgen::Substrate`] (via [`substrate_field`]), the exact
/// struct `per_species_suitability` builds internally and reads as `s.temperature_c`/
/// `s.moisture`/`s.insolation`/`s.height_asl_m` (`windows/worldgen/src/lib.rs`,
/// `per_species_suitability`'s body). Task 5b (The Generalist re-authoring):
/// elevation was already read this way by Task 1; temperature/moisture/
/// insolation are new here, from the same source rather than a hand-rederived
/// stand-in, so a unit mismatch ("a unit is not a frame") cannot creep in
/// between what this harness measures and what the response curve sees.
struct AxisSamples {
    /// Mean annual temperature, deg C - `Substrate::temperature_c` verbatim.
    temperature: Vec<f64>,
    /// Moisture in `[0, 1]` - `Substrate::moisture` verbatim.
    moisture: Vec<f64>,
    /// Annual-mean top-of-atmosphere insolation, relative to the planet's
    /// global scalar - `Substrate::insolation` verbatim.
    insolation: Vec<f64>,
    /// Height above this world's sea level, metres - `Substrate::height_asl_m`
    /// verbatim (the same `elevation_at(cell).above(sea_level())` The
    /// Tumult's re-datum, retyped by The Benchmark, performs).
    elevation: Vec<f64>,
}

/// Build `seed` to full depth and return `(axes, per_people_fits, per_people_shares)`
/// over the cells settleable by at least one of `peoples`.
///
/// `axes` is that cell set's four condition-axis readings, in the identical
/// frame [`per_species_suitability`] scores a `ConditionNiche` against - see
/// [`AxisSamples`]. `per_people_fits` maps each people's name to its own
/// per-cell K (the raw `per_species_suitability` output, computed independently
/// per species - NOT a coexistence share) over that exact same cell set, one
/// entry per settleable cell. `per_people_shares` maps each people's name to
/// its own per-cell overlap-weighted competitive share
/// (`hornvale_demography::coexist::cell_share`, at the frozen
/// [`hornvale_demography::BETA`]/[`hornvale_demography::FLOOR`], over
/// exactly `peoples` as the competing roster - present-species K > 0 only,
/// mirroring `hornvale_demography::coexist::pack`'s own per-cell `present`
/// filter), `0.0` for any people whose K at that cell was not `> 0.0` (so it
/// never entered `present`, matching what `pack` would do). Every vector -
/// `axes`' four fields, every `per_people_fits` value, every
/// `per_people_shares` value - is the same length and indexed the same way,
/// cell for cell, since all three are pushed inside one shared per-cell loop.
fn measure_one(
    seed: Seed,
    peoples: &[&'static str],
) -> (AxisSamples, PeopleSamples, PeopleSamples) {
    let wc = WorldComponents::assemble().expect("canonical registries are well-formed");
    // The build-local dense index -> KindId mapping, built from the exact
    // same `wc.biosphere` ordering (filtered to `peoples`, so still
    // ascending-KindId order) passed to `per_species_suitability` below, per its
    // doc comment, so the returned `u32` tags resolve to the correct kind.
    let kinds: Vec<KindId> = wc
        .biosphere
        .iter()
        .filter(|(k, _)| peoples.contains(&k.0))
        .map(|(k, _)| *k)
        .collect();
    let bios: Vec<&hornvale_species::BiosphereTraits> = wc
        .biosphere
        .iter()
        .filter(|(k, _)| peoples.contains(&k.0))
        .map(|(_, b)| b)
        .collect();
    // Same filter, same order, so the realm slice stays index-aligned with
    // `bios` — every shipped person is absent from the sparse habitat-realm
    // store and defaults to `Surface`.
    let realm: Vec<hornvale_species::HabitatRealm> = wc
        .biosphere
        .iter()
        .filter(|(k, _)| peoples.contains(&k.0))
        .map(|(k, _)| {
            wc.habitat_realm
                .get(k)
                .copied()
                .unwrap_or(hornvale_species::HabitatRealm::SURFACE)
        })
        .collect();

    let world = build_world(
        seed,
        &SkyPins::default(),
        SkyChoice::Generated,
        &TerrainPins::default(),
        &SettlementPins::default(),
    )
    .unwrap_or_else(|e| panic!("{seed:?} failed to build: {e:?}"));

    let terrain = terrain_of(&world).expect("terrain reconstructs");
    let climate = climate_of(&world).expect("climate reconstructs");
    let sky = sky_of(&world).expect("sky reconstructs");
    let geo = terrain.geosphere();
    let system = sky
        .system()
        .unwrap_or_else(|| panic!("{seed:?} has a generated star system"));
    let insolation_scalar = hornvale_astronomy::insolation_rel(&system.star, &system.anchor);
    let obliquity = system.anchor.obliquity.get();
    let regime = match system.anchor.rotation {
        hornvale_astronomy::Rotation::Spinning { day, .. } => {
            hornvale_climate::RotationRegime::Spinning { day_std: day.get() }
        }
        hornvale_astronomy::Rotation::Locked => hornvale_climate::RotationRegime::Locked,
    };

    let ks = per_species_suitability(
        geo,
        &terrain,
        &climate,
        obliquity,
        insolation_scalar,
        &regime,
        &bios,
        &realm,
    );
    // The exact substrate `per_species_suitability` builds internally (same
    // geo/terrain/climate/obliquity/insolation_scalar/regime) - not a
    // hand-rederived stand-in, so `axes` is guaranteed to be in the frame the
    // response curve sees.
    let substrate = substrate_field(
        geo,
        &terrain,
        &climate,
        obliquity,
        insolation_scalar,
        &regime,
    );

    // The competing roster's (id, mass, niche) triples and the resulting
    // Pianka guild-overlap matrix - both cell-invariant, so (mirroring
    // `hornvale_demography::coexist::pack`'s own hoist) derived once here
    // rather than inside the per-cell loop below.
    let species: Vec<(u32, hornvale_kernel::Mass, hornvale_kernel::ResourceVector)> = bios
        .iter()
        .enumerate()
        .map(|(tag, bio)| (tag as u32, bio.mass, bio.niche.clone()))
        .collect();
    let projected_niche: Vec<(u32, hornvale_kernel::ResourceVector)> = species
        .iter()
        .map(|(id, _mass, niche)| (*id, niche.clone()))
        .collect();
    let overlap = hornvale_demography::niche::guild_overlap(&projected_niche);

    let mut axes = AxisSamples {
        temperature: Vec::new(),
        moisture: Vec::new(),
        insolation: Vec::new(),
        elevation: Vec::new(),
    };
    let mut per_people: BTreeMap<&'static str, Vec<f64>> = BTreeMap::new();
    let mut per_people_share: BTreeMap<&'static str, Vec<f64>> = BTreeMap::new();

    for cell in geo.cells() {
        let settleable = ks.iter().any(|(_, k)| *k.get(cell) >= VIABILITY_FLOOR);
        if !settleable {
            continue;
        }
        let s = substrate.get(cell);
        axes.temperature.push(s.temperature_c);
        axes.moisture.push(s.moisture);
        axes.insolation.push(s.insolation);
        axes.elevation.push(s.height_asl_m.get());
        for (tag, k) in &ks {
            let name = kinds[*tag as usize].0;
            per_people.entry(name).or_default().push(*k.get(cell));
        }

        // The competitive share this cell's present species (K > 0, the
        // same filter `pack`'s per-cell loop applies) get from the K^β
        // softmax - `cell_share` directly, not `pack`'s final density,
        // since density additionally divides by home range and applies
        // trophic coupling, neither of which is part of "the share the
        // packer's BETA/FLOOR produce" (see the module doc).
        let present: Vec<(u32, f64)> = ks
            .iter()
            .map(|(tag, k)| (*tag, *k.get(cell)))
            .filter(|(_, k)| *k > 0.0)
            .collect();
        let capacity: f64 = present.iter().map(|(_, k)| *k).sum();
        let shares = hornvale_demography::coexist::cell_share(
            capacity,
            &present,
            &overlap,
            hornvale_demography::BETA,
            hornvale_demography::FLOOR,
        );
        for (tag, _k) in &ks {
            let name = kinds[*tag as usize].0;
            let share = shares.get(tag).copied().unwrap_or(0.0);
            per_people_share.entry(name).or_default().push(share);
        }
    }

    (axes, per_people, per_people_share)
}

/// Per-people, per-cell samples keyed by name - the shape both
/// `per_people_fits` and `per_people_shares` share, factored into a named
/// alias so `measure_one`'s signature reads as intent rather than tripping
/// clippy's `type_complexity` lint on the raw nested type.
type PeopleSamples = BTreeMap<&'static str, Vec<f64>>;

/// `p`-th percentile of a pre-sorted, non-empty `vals` (same integer-division
/// indexing Task 1 used for elevation, reused verbatim for the other three
/// axes so all four quantile reports are computed identically).
fn percentile_of_sorted(vals: &[f64], p: u32) -> f64 {
    let idx = (vals.len() * p as usize) / 100;
    vals[idx]
}

/// Pianka symmetric niche overlap over two aligned per-cell samples: `Σ aᵢbᵢ
/// / √(Σ aᵢ² · Σ bᵢ²)`, the identical formula
/// [`hornvale_kernel::ResourceVector::overlap`] applies to a fixed
/// resource-axis vector, generalized here to an arbitrary-length per-cell
/// sample (a per-cell K/fit distribution over space is exactly the kind of
/// utilization vector Pianka's index is defined for). `a` and `b` must be
/// the same length and cell-aligned (index `i` = the same cell in both) -
/// every caller here builds both from `measure_one`'s single per-cell loop,
/// so alignment holds by construction. Returns `0.0` if either vector is the
/// zero vector (matching `ResourceVector::overlap`'s own convention).
fn pianka(a: &[f64], b: &[f64]) -> f64 {
    let sum_a2: f64 = a.iter().map(|x| x * x).sum();
    let sum_b2: f64 = b.iter().map(|x| x * x).sum();
    if sum_a2 == 0.0 || sum_b2 == 0.0 {
        return 0.0;
    }
    let numerator: f64 = a.iter().zip(b.iter()).map(|(x, y)| x * y).sum();
    numerator / (sum_a2 * sum_b2).sqrt()
}

/// Pearson correlation coefficient over two aligned per-cell samples (same
/// alignment contract as [`pianka`]). Returns `0.0` if either sample has
/// zero variance (a constant vector correlates with nothing; this guards the
/// division rather than producing `NaN`).
fn pearson(a: &[f64], b: &[f64]) -> f64 {
    let n = a.len() as f64;
    let mean_a = a.iter().sum::<f64>() / n;
    let mean_b = b.iter().sum::<f64>() / n;
    let cov: f64 = a
        .iter()
        .zip(b.iter())
        .map(|(x, y)| (x - mean_a) * (y - mean_b))
        .sum();
    let var_a: f64 = a.iter().map(|x| (x - mean_a) * (x - mean_a)).sum();
    let var_b: f64 = b.iter().map(|y| (y - mean_b) * (y - mean_b)).sum();
    if var_a == 0.0 || var_b == 0.0 {
        return 0.0;
    }
    cov / (var_a * var_b).sqrt()
}

/// Report one H2 stronghold band: every people's mean fit (K) and mean/max
/// competitive share over `indices` (positions into the aggregated,
/// cell-aligned `per_people_fit`/`per_people_share` vectors), the best-fit
/// people by mean K, a POSITIVE CONTROL for the `>0.5` majority-share test
/// (fix round 1, review finding 3: the locally-dominant-by-mean-share
/// people's own majority-share fraction, so "0.5 is crossed by nobody" and
/// "0.5 is crossed by nobody but human specifically" can be told apart), and
/// the fraction of the band where human holds a majority (`> 0.5`)
/// competitive share - the H3 question, asked locally to this band rather
/// than only over all settleable land.
fn report_band(
    label: &str,
    indices: &[usize],
    per_people_fit: &PeopleSamples,
    per_people_share: &PeopleSamples,
) {
    println!("H2 band {label}: n = {}", indices.len());
    if indices.is_empty() {
        println!("H2 band {label}: EMPTY - no cells in this band, nothing to report");
        return;
    }
    let mut best_fit: Option<(&str, f64)> = None;
    for (name, vals) in per_people_fit {
        let mean = indices.iter().map(|&i| vals[i]).sum::<f64>() / indices.len() as f64;
        println!("H2 band {label}: mean fit (K) {name} = {mean:.6}");
        if best_fit.is_none_or(|(_, m)| mean > m) {
            best_fit = Some((name, mean));
        }
    }
    println!(
        "H2 band {label}: best-fit by mean K = {}",
        best_fit.expect("per_people_fit is non-empty").0
    );
    let mut best_share: Option<(&str, f64)> = None;
    for (name, vals) in per_people_share {
        let mean = indices.iter().map(|&i| vals[i]).sum::<f64>() / indices.len() as f64;
        let max = indices
            .iter()
            .map(|&i| vals[i])
            .fold(f64::NEG_INFINITY, f64::max);
        println!("H2 band {label}: mean competitive share {name} = {mean:.6}, max = {max:.6}");
        if best_share.is_none_or(|(_, m)| mean > m) {
            best_share = Some((name, mean));
        }
    }
    let (dominant_name, dominant_mean) = best_share.expect("per_people_share is non-empty");
    let dominant_share = &per_people_share[dominant_name];
    let dominant_majority = indices.iter().filter(|&&i| dominant_share[i] > 0.5).count();
    println!(
        "H3 band {label}: positive control - {dominant_name} (locally dominant, mean share {dominant_mean:.6}) holds majority share (>0.5) on {:.6} ({dominant_majority}/{})",
        dominant_majority as f64 / indices.len() as f64,
        indices.len()
    );
    let human_share = &per_people_share["human"];
    let human_majority = indices.iter().filter(|&&i| human_share[i] > 0.5).count();
    println!(
        "H3 band {label}: fraction human holds majority share (>0.5) = {:.6} ({human_majority}/{})",
        human_majority as f64 / indices.len() as f64,
        indices.len()
    );
}

/// claim: readout(off-gate, heavy:) — prints axis percentiles and per-people
/// pre-human fit means over SEEDS
#[test]
#[ignore = "heavy: live-worldgen battery (minutes); deferred from the commit gate to make gate-full"]
fn report_land_distribution_and_pre_human_fits() {
    let mut temperature: Vec<f64> = Vec::new();
    let mut moisture: Vec<f64> = Vec::new();
    let mut insolation: Vec<f64> = Vec::new();
    let mut elevations: Vec<f64> = Vec::new();
    let mut per_people: BTreeMap<&'static str, Vec<f64>> = BTreeMap::new();

    for seed in SEEDS {
        let (axes, fits, _shares) = measure_one(Seed(seed), &PEOPLES);
        temperature.extend(axes.temperature);
        moisture.extend(axes.moisture);
        insolation.extend(axes.insolation);
        elevations.extend(axes.elevation);
        for (kind, vals) in fits {
            per_people.entry(kind).or_default().extend(vals);
        }
    }

    temperature.sort_by(f64::total_cmp);
    moisture.sort_by(f64::total_cmp);
    insolation.sort_by(f64::total_cmp);
    elevations.sort_by(f64::total_cmp);

    for (axis, vals) in [
        ("temperature_c", &temperature),
        ("moisture", &moisture),
        ("insolation", &insolation),
        ("elevation_m", &elevations),
    ] {
        for p in [5u32, 15, 50, 85, 95] {
            println!("{axis} p{p} = {:.4}", percentile_of_sorted(vals, p));
        }
    }
    for (kind, vals) in &per_people {
        let mean = vals.iter().sum::<f64>() / vals.len() as f64;
        println!("pre-human mean fit {kind} = {mean:.4}");
    }
    println!("n = {} settleable cells", elevations.len());

    // Guard assertions (pre-flight ruling, 2026-08-03). This is a measurement
    // harness, not a hypothesis test - H1/H2/H3 are REPORTED in Task 6, never
    // asserted, because H3 firing is the campaign's most valuable finding and
    // must not present as a red build. But a harness that silently measures
    // nothing looks identical to one that works, so it guards its own inputs.
    assert!(!elevations.is_empty(), "no settleable land sampled");
    assert!(
        elevations.iter().all(|e| e.is_finite()),
        "non-finite elevation in the sample"
    );
    assert_eq!(
        per_people.len(),
        5,
        "all five pre-human peoples must be measured; got {:?}",
        per_people.keys().collect::<Vec<_>>()
    );
}

/// Task 6: the campaign's preregistered readout (design spec §4). Builds the
/// same 30 seeds a second time (`PEOPLES_WITH_HUMAN`, not `PEOPLES`) so
/// human is folded into the packer, and reports H1/H2/H3's numbers - never
/// asserts them, per the pre-flight ruling recorded on
/// `report_land_distribution_and_pre_human_fits` above and repeated in the
/// design spec's §4: a preregistered prediction encoded as a build failure
/// creates pressure to retune the niche until the suite goes green, which
/// the spec forbids. `human_condition_niche()` and `BETA` are untouched by
/// this test and must stay that way regardless of which way the numbers land.
/// claim: readout(off-gate, heavy:, preregistered) — Gause-competition
/// readout over SEEDS
#[test]
#[ignore = "heavy: live-worldgen battery (minutes); deferred from the commit gate to make gate-full"]
fn report_the_preregistered_gause_readout() {
    let mut temperature: Vec<f64> = Vec::new();
    let mut moisture: Vec<f64> = Vec::new();
    let mut elevations: Vec<f64> = Vec::new();
    let mut per_people_fit: BTreeMap<&'static str, Vec<f64>> = BTreeMap::new();
    let mut per_people_share: BTreeMap<&'static str, Vec<f64>> = BTreeMap::new();

    for seed in SEEDS {
        let (axes, fits, shares) = measure_one(Seed(seed), &PEOPLES_WITH_HUMAN);
        temperature.extend(axes.temperature);
        moisture.extend(axes.moisture);
        elevations.extend(axes.elevation);
        for (kind, vals) in fits {
            per_people_fit.entry(kind).or_default().extend(vals);
        }
        for (kind, vals) in shares {
            per_people_share.entry(kind).or_default().extend(vals);
        }
    }

    let n = elevations.len();
    println!(
        "n = {n} settleable cells (6-people population: bugbear, gnoll, goblin, hobgoblin, human, kobold)"
    );

    // Guard assertions (pre-flight ruling, 2026-08-03; task brief step 1).
    // H1/H2/H3 are REPORTED below, never asserted - only the harness's own
    // inputs are guarded, so a silently-broken measurement cannot look like a
    // working one.
    assert!(!elevations.is_empty(), "no settleable land sampled");
    assert!(
        elevations.iter().all(|e| e.is_finite()),
        "non-finite elevation in the sample"
    );
    assert_eq!(
        per_people_fit.len(),
        6,
        "human must be in the packer; got {:?}",
        per_people_fit.keys().collect::<Vec<_>>()
    );
    assert!(
        per_people_fit.contains_key("human"),
        "the readout measured every people EXCEPT the one this campaign added"
    );

    // Mean fit (K) and mean competitive share per people, over ALL
    // settleable land - the fit/share pair the module doc warns not to
    // conflate, reported side by side so neither reading can be mistaken for
    // the other downstream.
    for (name, vals) in &per_people_fit {
        let mean = vals.iter().sum::<f64>() / vals.len() as f64;
        println!("mean fit (K) {name} = {mean:.6}");
    }
    for (name, vals) in &per_people_share {
        let mean = vals.iter().sum::<f64>() / vals.len() as f64;
        println!("mean competitive share {name} = {mean:.6}");
    }
    // Positive control for the majority-share (`>0.5`) test below (fix round
    // 1, review finding 3): the max competitive share ANY cell gives each
    // people, over all settleable land. Without this, "0/142593 cells cross
    // 0.5" cannot be told apart from "nothing in this dataset ever crosses
    // 0.5, for any people, regardless of dominance" - the max establishes
    // whether the threshold is reachable at all.
    for (name, vals) in &per_people_share {
        let max = vals.iter().copied().fold(f64::NEG_INFINITY, f64::max);
        println!("max competitive share {name} = {max:.6}");
    }

    // H1 - the ecotone prediction: human takes marginal/ecotone ground and
    // competes hardest with goblin, i.e. the human-goblin pair should rank
    // FIRST by both Pianka overlap (on K, the spatial niche-overlap reading)
    // and by share correlation (on competitive share, the packer's output) -
    // ranked against every other pair in the roster, not asserted in
    // isolation.
    let names: Vec<&str> = per_people_fit.keys().copied().collect();
    let mut pianka_pairs: Vec<(String, f64)> = Vec::new();
    let mut corr_pairs: Vec<(String, f64)> = Vec::new();
    for i in 0..names.len() {
        for j in (i + 1)..names.len() {
            let a = &per_people_fit[names[i]];
            let b = &per_people_fit[names[j]];
            pianka_pairs.push((format!("{}-{}", names[i], names[j]), pianka(a, b)));

            let sa = &per_people_share[names[i]];
            let sb = &per_people_share[names[j]];
            corr_pairs.push((format!("{}-{}", names[i], names[j]), pearson(sa, sb)));
        }
    }
    pianka_pairs.sort_by(|a, b| b.1.total_cmp(&a.1));
    corr_pairs.sort_by(|a, b| b.1.total_cmp(&a.1));
    for (pair, v) in &pianka_pairs {
        println!("H1 pianka_overlap(K) {pair} = {v:.6}");
    }
    for (pair, v) in &corr_pairs {
        println!("H1 share_correlation {pair} = {v:.6}");
    }

    // H2 - the refuge prediction: human must NOT become the best-fit people
    // on any of the four specialists' strongholds. Bands per the module doc:
    // kobold/bugbear from §4's own elevation thresholds; hobgoblin/gnoll read
    // from their own `ConditionNiche` doc comments.
    let mut elevation_sorted = elevations.clone();
    elevation_sorted.sort_by(f64::total_cmp);
    let hobgoblin_p10 = percentile_of_sorted(&elevation_sorted, 10);
    let hobgoblin_p60 = percentile_of_sorted(&elevation_sorted, 60);
    println!("H2 hobgoblin band bounds: p10 = {hobgoblin_p10:.4} m, p60 = {hobgoblin_p60:.4} m");

    let kobold_band: Vec<usize> = (0..n).filter(|&i| elevations[i] >= 3000.0).collect();
    let bugbear_band: Vec<usize> = (0..n).filter(|&i| elevations[i] <= 500.0).collect();
    let hobgoblin_band: Vec<usize> = (0..n)
        .filter(|&i| elevations[i] >= hobgoblin_p10 && elevations[i] <= hobgoblin_p60)
        .collect();
    let gnoll_band: Vec<usize> = (0..n)
        .filter(|&i| temperature[i] >= 20.0 && moisture[i] < 0.2)
        .collect();

    report_band(
        "kobold-highland(elevation>=3000m)",
        &kobold_band,
        &per_people_fit,
        &per_people_share,
    );
    report_band(
        "bugbear-lowland(elevation<=500m)",
        &bugbear_band,
        &per_people_fit,
        &per_people_share,
    );
    report_band(
        "hobgoblin-plains(elevation-in-[p10,p60])",
        &hobgoblin_band,
        &per_people_fit,
        &per_people_share,
    );
    report_band(
        "gnoll-desert(temp>=20&moisture<0.2)",
        &gnoll_band,
        &per_people_fit,
        &per_people_share,
    );

    // H3 - the falsification: the fraction of ALL settleable land (not only
    // the stronghold bands, which `report_band` already covers) where human
    // holds a majority (> 0.5) competitive share. Positive control (fix round
    // 1, review finding 3) alongside it: the same test applied to hobgoblin,
    // the roster's actual dominant specialist (highest mean fit AND mean
    // share of the six, see above) - if hobgoblin also never crosses 0.5,
    // that says the threshold itself is the wrong instrument for this
    // dataset, not that human specifically failed to dominate anything.
    let hobgoblin_share = &per_people_share["hobgoblin"];
    let hobgoblin_majority_all = (0..n).filter(|&i| hobgoblin_share[i] > 0.5).count();
    println!(
        "H3 all-settleable-land: positive control - fraction hobgoblin holds majority share (>0.5) = {:.6} ({hobgoblin_majority_all}/{n})",
        hobgoblin_majority_all as f64 / n as f64
    );
    let human_share = &per_people_share["human"];
    let human_majority_all = (0..n).filter(|&i| human_share[i] > 0.5).count();
    println!(
        "H3 all-settleable-land: fraction human holds majority share (>0.5) = {:.6} ({human_majority_all}/{n})",
        human_majority_all as f64 / n as f64
    );
}

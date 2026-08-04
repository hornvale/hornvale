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
//! [`hornvale_worldgen::Substrate`] `niche_per_species_k` scores a
//! `ConditionNiche` against - see [`AxisSamples`]'s doc comment.
//!
//! Ignored: builds 30 worlds. Reason token `heavy:` puts it in the heavy tier
//! (cli/tests/heavy_tier.rs), not the commit gate.
//!
//! World-building idiom reused verbatim from
//! `windows/worldgen/tests/non_void_roster.rs` (`hornvale_worldgen::build_world`,
//! `WorldComponents::assemble`, `terrain_of`/`climate_of`/`sky_of`
//! "reconstruct, never store"). The per-species K comes from
//! [`hornvale_worldgen::niche_per_species_k`], whose returned `u32` is a
//! **build-local dense index, not identity** (see its doc comment) - it is
//! the position in the `species_biosphere` slice passed in, so the index ->
//! [`hornvale_kernel::KindId`] mapping here is rebuilt fresh, per seed, from
//! that exact same `wc.biosphere.iter()` ordering.
//!
//! **"Settleable land"** is not a second, independently-chosen filter: K is 0
//! on every submerged cell for the whole roster today (`niche_per_species_k`'s
//! own doc, The Tumult's land mask), so "does at least one of the five
//! pre-human peoples clear [`VIABILITY_FLOOR`] here" already separates
//! occupiable land from both ocean and the uninhabitable land the condition
//! niches themselves exclude - the same viability test `non_void_roster.rs`
//! applies per-kind, applied here per-cell across the whole roster. Both
//! outputs of `measure_one` (the elevation sample and every people's fit
//! sample) are drawn from that identical filtered cell set, seed by seed, so
//! the pre-human mean fits are means over the same population the elevation
//! quantiles describe - the spec's D3 concern ("a quantile from the wrong
//! population carries the authority of evidence") applies equally to a mean.
//!
//! Test fixture (decision 0092): calls the sculpt/fit derivation entry
//! points directly to build its own world state, once per test - the
//! sanctioned test-fixture posture the weir's spec carves out.
#![allow(clippy::disallowed_methods)]

use hornvale_astronomy::SkyPins;
use hornvale_kernel::{KindId, Seed};
use hornvale_terrain::TerrainPins;
use hornvale_worldgen::{
    SettlementPins, SkyChoice, WorldComponents, build_world, climate_of, niche_per_species_k,
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
/// peoples together), but this campaign's baseline is about the five
/// **peoples** human joins - so `measure_one` filters `wc.biosphere` down to
/// exactly these before ever calling `niche_per_species_k`, rather than
/// measuring "settleable" against the whole 29-kind fauna+peoples roster.
const PEOPLES: [&str; 5] = ["bugbear", "gnoll", "goblin", "hobgoblin", "kobold"];

/// The four condition axes, sampled over the settleable-cell population, in
/// the identical frame `niche_per_species_k` scores a `ConditionNiche`
/// against - each field is a direct per-cell read of
/// [`hornvale_worldgen::Substrate`] (via [`substrate_field`]), the exact
/// struct `niche_per_species_k` builds internally and reads as `s.temperature_c`/
/// `s.moisture`/`s.insolation`/`s.elevation` (`windows/worldgen/src/lib.rs`,
/// `niche_per_species_k`'s body). Task 5b (The Generalist re-authoring):
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
    /// Height above this world's sea level, metres - `Substrate::elevation`
    /// verbatim (the same `elevation_at(cell) - sea_level()` The Tumult's
    /// re-datum performs).
    elevation: Vec<f64>,
}

/// Build `seed` to full depth and return `(axes, per_people_fits)` over the
/// cells settleable by at least one of the five pre-human peoples.
///
/// `axes` is that cell set's four condition-axis readings, in the identical
/// frame [`niche_per_species_k`] scores a `ConditionNiche` against - see
/// [`AxisSamples`]. `per_people_fits` maps each people's name to its own
/// per-cell K (the raw `niche_per_species_k` output, not a coexistence
/// share) over that exact same cell set, one entry per settleable cell - so
/// every people's vector, and every `axes` field, is the same length and
/// indexed the same way, cell for cell.
fn measure_one(seed: Seed) -> (AxisSamples, BTreeMap<&'static str, Vec<f64>>) {
    let wc = WorldComponents::assemble().expect("canonical registries are well-formed");
    // The build-local dense index -> KindId mapping, built from the exact
    // same `wc.biosphere` ordering (filtered to PEOPLES, so still
    // ascending-KindId order) passed to `niche_per_species_k` below, per its
    // doc comment, so the returned `u32` tags resolve to the correct kind.
    let kinds: Vec<KindId> = wc
        .biosphere
        .iter()
        .filter(|(k, _)| PEOPLES.contains(&k.0))
        .map(|(k, _)| *k)
        .collect();
    let bios: Vec<&hornvale_species::BiosphereTraits> = wc
        .biosphere
        .iter()
        .filter(|(k, _)| PEOPLES.contains(&k.0))
        .map(|(_, b)| b)
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

    let ks = niche_per_species_k(
        geo,
        &terrain,
        &climate,
        obliquity,
        insolation_scalar,
        &regime,
        &bios,
    );
    // The exact substrate `niche_per_species_k` builds internally (same
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

    let mut axes = AxisSamples {
        temperature: Vec::new(),
        moisture: Vec::new(),
        insolation: Vec::new(),
        elevation: Vec::new(),
    };
    let mut per_people: BTreeMap<&'static str, Vec<f64>> = BTreeMap::new();

    for cell in geo.cells() {
        let settleable = ks.iter().any(|(_, k)| *k.get(cell) >= VIABILITY_FLOOR);
        if !settleable {
            continue;
        }
        let s = substrate.get(cell);
        axes.temperature.push(s.temperature_c);
        axes.moisture.push(s.moisture);
        axes.insolation.push(s.insolation);
        axes.elevation.push(s.elevation);
        for (tag, k) in &ks {
            let name = kinds[*tag as usize].0;
            per_people.entry(name).or_default().push(*k.get(cell));
        }
    }

    (axes, per_people)
}

/// `p`-th percentile of a pre-sorted, non-empty `vals` (same integer-division
/// indexing Task 1 used for elevation, reused verbatim for the other three
/// axes so all four quantile reports are computed identically).
fn percentile_of_sorted(vals: &[f64], p: u32) -> f64 {
    let idx = (vals.len() * p as usize) / 100;
    vals[idx]
}

#[test]
#[ignore = "heavy: live-worldgen battery (minutes); deferred from the commit gate to make gate-full"]
fn report_land_distribution_and_pre_human_fits() {
    let mut temperature: Vec<f64> = Vec::new();
    let mut moisture: Vec<f64> = Vec::new();
    let mut insolation: Vec<f64> = Vec::new();
    let mut elevations: Vec<f64> = Vec::new();
    let mut per_people: BTreeMap<&'static str, Vec<f64>> = BTreeMap::new();

    for seed in SEEDS {
        let (axes, fits) = measure_one(Seed(seed));
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

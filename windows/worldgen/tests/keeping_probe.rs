//! TASK 0 PROBE (The Keeping) — measurement only, not a shipped feature.
//!
//! Answers the five questions the campaign spec
//! (`docs/superpowers/specs/2026-08-04-the-keeping-design.md` §4, Task 0) freezes
//! **before** the placement gate is rewired, because the answers can redirect or
//! stop the campaign:
//!
//! 1. Per settling species, how much land has non-negligible per-species K, and
//!    where is it the best-fit species?
//! 2. **The headroom** — how much land sits at gnoll's corner (arid + hot),
//!    ground the global gate calls uninhabitable *by definition* today?
//! 3. The same for kobold's >3000 m stronghold, and for land the `-5..35 °C`
//!    band excludes.
//! 4. **The clinging band** — where is per-species capacity enough to survive
//!    (`pressure < COLLAPSE_PRESSURE`) but not enough to throw a daughter
//!    (`pressure < DAUGHTER_MAX_PRESSURE`)? That band is marginal persistence.
//! 5. **The expansion magnitude** — how much land becomes settleable for *at
//!    least one* species, against today's habitable count?
//!
//! ## The ocean trap, inherited
//!
//! `waterline_probe.rs` documents a correction worth repeating: ocean is
//! `terrain.is_ocean(cell)` (`elevation < sea_level`), **never**
//! `elevation < 0.0`. Sea level on seed 42 is −2,936.17 m, and the two
//! predicates disagree on 8,162 cells. This probe uses `is_ocean` throughout and
//! reports elevation as height *above sea level*.
//!
//! ## CORRECTION (2026-08-04, before any result was acted on)
//!
//! The first run of this probe reported `K × SETTLERS_PER_CAPACITY` as a
//! headcount capacity, inheriting the spec's §3.2 assumption that
//! `niche_per_species_k` — as it was named then; **renamed to
//! `per_species_suitability` by decision 0103**, because the `_k` was half the
//! error — returns a field on the same footing as the bake's `capacity`.
//! **It does not.** Its last statement is
//!
//! ```text
//! let saturated = supply / (1.0 + supply);   // <= 1 always
//! saturated * temperature.eval(..) * moisture.eval(..)
//!           * insolation.eval(..)  * elevation.eval(.., 0.0)
//! ```
//!
//! — a Michaelis–Menten saturation times four factors each `<= 1`, so
//! **K ∈ [0, 1] is a dimensionless suitability, not a capacity**. It is the
//! *factor*, occupying exactly the role `Bake::factor` plays today.
//!
//! Two consequences. The design is smaller than §3.2 claims: the base capacity
//! field **stays**, `eff_capacity = capacity × factor` keeps its shape and its
//! units, and only `factor` changes from binary to per-species K. And the first
//! run's "capacity never exceeds 11.5, so no daughter could ever form" and
//! "expansion ratio < 1" were **artifacts of the wrong denominator**, not
//! findings. This probe now measures `carrying_capacity × SETTLERS_PER_CAPACITY
//! × K_species`, which is what the rewired bake would actually see.
//!
//! The findings that do NOT depend on the scale — that `K > 0`'s support is
//! species-independent, and that best-fit territory collapses onto two species —
//! survive the correction unchanged, because both are comparisons.
//!
//! # RESULTS (2026-08-04, five probe seeds) — the campaign is redirected
//!
//! ## 1. BLOCKING: the gate is applied TWICE and the spec targets the wrong copy
//!
//! ```text
//! Bake::factor                  if !era.habitable.get(cell) { 0.0 }   <- spec's target
//! carrying_capacity             if !i.habitable { return 0.0 }        <- what BINDS
//!                               (domains/demography/src/carrying_capacity.rs:59)
//! ```
//!
//! The second is **upstream** of `per_species_suitability` — `base_carrying` is its
//! `PHOTOSYNTHATE` supply — so `K == 0` wherever the global mask says
//! uninhabitable. Measured consequence, all five seeds:
//!
//! ```text
//!   cells with K>0 that the gate excludes (NEW ground):   0    0    0    0    0
//! ```
//!
//! **Rewiring `Bake::factor` alone opens exactly zero new ground.** It would have
//! compiled, passed, held byte-identity, and done nothing — and a null census
//! result would have looked like the spec's H4 ("the roster is the constraint")
//! when the truth was a second gate silently binding.
//!
//! ## 2. The scale is wrong by 1–2 orders of magnitude
//!
//! ```text
//!   today's eff_capacity on habitable cells   median 29.65 .. 44.90, max 89 .. 130
//!   cells clearing the daughter bar (11.43)
//!     with K as the factor                    0 .. 5   of 40,962   (every species)
//! ```
//!
//! `K` is `saturation(supply) × 4 factors each ≤ 1`, typically ~0.01–0.05, so it
//! acts as a 20–100× **divisor** where the binary factor it replaces is `1.0`.
//! Even with finding 1 fixed, using `K` raw as the factor would halt daughter
//! founding almost everywhere. A normalization decision is required and the spec
//! contains none.
//!
//! ## 3. The gate is LOOSER than capacity, not tighter
//!
//! ```text
//!   cells the gate admits where K==0 (DEAD ground)
//!     seed 42 1171 (10.58%)   7 765 (4.02%)   999999 213 (1.33%)
//!     16244..  312 (2.63%)    1234 2393 (20.68%)
//! ```
//!
//! Seed 1234's median capacity on habitable cells is **0.00**, which is why it
//! has no settlements at all.
//!
//! ## 4. Best-fit territory has already collapsed onto two species
//!
//! `hobgoblin` and `kobold` take essentially every cell; **`goblin`, `bugbear`
//! and `gnoll` win zero on every seed.** The authored optima are well separated
//! (spec §2.1) and it does not translate into distinct territory — MAP-22's Gause
//! collapse, measured.
//!
//! ## 5. The headroom is COLD, and no settler wants it
//!
//! Excluded land is 15–59% `temp < -5 °C` and 18–32% `moisture < 0.2`, while
//! gnoll's hot-arid corner is 0–4.75%. The world's extremes are **cold**-arid;
//! the roster's arid specialist is hot-arid. So even with both gates fixed and
//! normalized, the excluded ground has no claimant.

// `terrain_of` is a named derivation entry point (decision 0092): it sculpts a
// full tectonic globe per call. A probe measuring five worlds is exactly such a
// site, and it calls it once per seed — the same allowance `waterline_probe.rs`
// takes for the same reason.
#![allow(clippy::disallowed_methods)]

use hornvale_kernel::CellMap;
use hornvale_worldgen::components::WorldComponents;
use hornvale_worldgen::{
    SETTLERS_PER_CAPACITY, SettlementPins, SkyChoice, build_world, carrying_inputs_of, climate_of,
    per_species_suitability, sky_of, terrain_of,
};

/// The settling roster — `SocialForm::Settled`, verified as exactly these six.
const SETTLERS: [&str; 6] = ["kobold", "goblin", "hobgoblin", "bugbear", "gnoll", "human"];

/// Per-capita need; `history_bake::NEED`, private, so mirrored here.
const NEED: f64 = 1.0;
/// `history_bake::COLLAPSE_PRESSURE` (public, mirrored for arithmetic clarity).
const COLLAPSE_PRESSURE: f64 = 2.0;
/// `history_bake::DAUGHTER_MAX_PRESSURE`, private, so mirrored here.
const DAUGHTER_MAX_PRESSURE: f64 = 0.7;
/// `history_bake::DAUGHTER_POP`, private, so mirrored here.
const DAUGHTER_POP: f64 = 8.0;
/// `history_bake::VIABLE_MIN`, private, so mirrored here.
const VIABLE_MIN: f64 = 2.0;

/// Candidate "non-negligible fit" thresholds, in headcount capacity. §7 q2 says
/// to pick this from the measured distribution rather than authoring it blind,
/// so the probe reports every candidate and the choice is made after reading.
const FIT_THRESHOLDS: [f64; 4] = [0.0, 0.5, 1.0, 11.5];

/// Probe seeds — the spec's set. 1234 has zero settlements today.
const SEEDS: [u64; 5] = [42, 7, 999_999, 16_244_526_067_196_353_746, 1234];

/// claim: sanctioned-sweep(probe: measurement only, run explicitly) — prints
/// over SEEDS
#[test]
#[ignore = "probe: measurement only, run explicitly"]
fn keeping_task0_probe() {
    println!(
        "\n################ THE KEEPING — Task 0 probe ################\n\
         survival needs eff_capacity > pop*NEED/{COLLAPSE_PRESSURE} \
         (pop {VIABLE_MIN} -> {:.2})\n\
         a daughter needs eff_capacity > {DAUGHTER_POP}/{DAUGHTER_MAX_PRESSURE} \
         (-> {:.2})\n",
        VIABLE_MIN * NEED / COLLAPSE_PRESSURE,
        DAUGHTER_POP / DAUGHTER_MAX_PRESSURE,
    );

    for seed in SEEDS {
        probe_seed(seed);
    }
}

fn probe_seed(seed: u64) {
    let world = build_world(
        hornvale_kernel::Seed(seed),
        &hornvale_astronomy::SkyPins::default(),
        SkyChoice::Generated,
        &hornvale_terrain::TerrainPins::default(),
        &SettlementPins::default(),
    )
    .expect("probe seed builds");

    let terrain = terrain_of(&world).unwrap();
    let climate = climate_of(&world).unwrap();
    let sky = sky_of(&world).unwrap();
    let geo = terrain.geosphere();

    let generated = match &sky {
        hornvale_worldgen::Sky::Generated(g) => g,
        _ => panic!("probe expects a generated sky"),
    };
    let system = generated.system();
    let insolation_scalar = hornvale_astronomy::insolation_rel(&system.star, &system.anchor);
    let obliquity_deg = system.anchor.obliquity.get();
    let regime = match system.anchor.rotation {
        hornvale_astronomy::Rotation::Spinning { day, .. } => {
            hornvale_climate::RotationRegime::Spinning { day_std: day.get() }
        }
        hornvale_astronomy::Rotation::Locked => hornvale_climate::RotationRegime::Locked,
    };

    let wc = WorldComponents::assemble().unwrap();
    let names: Vec<&'static str> = wc.biosphere.ids().map(|k| k.0).collect();
    let bio: Vec<&hornvale_species::BiosphereTraits> =
        wc.biosphere.iter().map(|(_, b)| b).collect();
    // Same `wc.biosphere` order as `bio`, so the realm slice stays
    // index-aligned — a kind absent from the sparse habitat-realm store
    // defaults to `Surface`.
    let realm: Vec<hornvale_species::HabitatRealm> = wc
        .biosphere
        .iter()
        .map(|(k, _)| {
            wc.habitat_realm
                .get(k)
                .copied()
                .unwrap_or(hornvale_species::HabitatRealm::SURFACE)
        })
        .collect();

    let ks = per_species_suitability(
        geo,
        &terrain,
        &climate,
        obliquity_deg,
        insolation_scalar,
        &regime,
        &bio,
        &realm,
    );
    let tag_of = |name: &str| -> u32 { names.iter().position(|n| *n == name).unwrap() as u32 };
    let k_of = |tag: u32| -> &CellMap<f64> { &ks.iter().find(|(t, _)| *t == tag).unwrap().1 };

    // The bake's own capacity field, verbatim from `bake_history_from`. K is a
    // dimensionless factor in [0,1]; this is what carries the headcount units.
    let base =
        hornvale_demography::carrying_capacity(geo, &carrying_inputs_of(geo, &terrain, &climate));
    // `scaled` keeps this a capacity by construction (decision 0103).
    let capacity = base.scaled(SETTLERS_PER_CAPACITY);
    // eff_capacity as the rewired bake would compute it: base x per-species K.
    let eff = |c: hornvale_kernel::CellId, tag: u32| -> f64 { capacity.at(c) * *k_of(tag).get(c) };

    let sea = terrain.sea_level();
    let cells: Vec<_> = geo.cells().collect();
    let land: Vec<_> = cells
        .iter()
        .copied()
        .filter(|c| !terrain.is_ocean(*c))
        .collect();

    // Today's gate, recomputed exactly as `habitability_map` does.
    let habitable: Vec<_> = cells
        .iter()
        .copied()
        .filter(|c| {
            hornvale_climate::is_habitable(
                climate.mean_temperature_at(*c),
                climate.moisture_at(*c),
                terrain.elevation_at(*c),
                sea,
            )
        })
        .collect();

    println!(
        "\n=== seed {seed} — {} cells, {} land, {} habitable today \
              ({:.2}% of land)",
        cells.len(),
        land.len(),
        habitable.len(),
        100.0 * habitable.len() as f64 / land.len().max(1) as f64,
    );

    // --- Item 2/3: the ground today's thresholds exclude -------------------
    let mut gnoll_corner = 0usize;
    let mut too_hot = 0usize;
    let mut too_cold = 0usize;
    let mut too_arid = 0usize;
    let mut above_3000 = 0usize;
    for &c in &land {
        let t = climate.mean_temperature_at(c).get();
        let m = climate.moisture_at(c);
        let h = terrain.elevation_at(c).get() - sea.get();
        if m < 0.2 && t > 25.0 {
            gnoll_corner += 1;
        }
        if t > 35.0 {
            too_hot += 1;
        }
        if t < -5.0 {
            too_cold += 1;
        }
        if m < 0.2 {
            too_arid += 1;
        }
        if h > 3000.0 {
            above_3000 += 1;
        }
    }
    let pct = |n: usize| 100.0 * n as f64 / land.len().max(1) as f64;
    println!(
        "  HEADROOM (land cells the global gate excludes today):\n\
         \x20   gnoll corner (moisture<0.2 AND temp>25C) : {gnoll_corner:6} ({:.2}% of land)\n\
         \x20   arid          (moisture<0.2)             : {too_arid:6} ({:.2}%)\n\
         \x20   too hot       (temp>35C)                 : {too_hot:6} ({:.2}%)\n\
         \x20   too cold      (temp<-5C)                 : {too_cold:6} ({:.2}%)\n\
         \x20   kobold stake  (>3000 m above sea level)  : {above_3000:6} ({:.2}%)",
        pct(gnoll_corner),
        pct(too_arid),
        pct(too_hot),
        pct(too_cold),
        pct(above_3000),
    );

    // --- Item 1/4/5: per-species capacity over land ------------------------
    // Scale baseline: today's eff_capacity on a habitable cell IS `capacity`
    // (factor == 1), so this is the frame the bake reasons in right now.
    let mut caps: Vec<f64> = habitable.iter().map(|c| capacity.at(*c)).collect();
    caps.sort_by(f64::total_cmp);
    if !caps.is_empty() {
        println!(
            "  SCALE (today's eff_capacity on habitable cells = base capacity): \
             median {:.2}  p90 {:.2}  max {:.2}",
            caps[caps.len() / 2],
            caps[caps.len() * 9 / 10],
            caps[caps.len() - 1],
        );
    }
    println!("  PER-SPECIES (eff_capacity = base capacity x per-species K):");
    println!(
        "    {:<10} {:>7} {:>9} {:>9} {:>9} {:>9} {:>8}",
        "species", "floor", "K>0", ">0.5", ">1.0", ">11.5", "best-fit"
    );

    let mut any_fit = vec![false; cells.len()];
    let mut cling = vec![0usize; SETTLERS.len()];

    for (si, name) in SETTLERS.iter().enumerate() {
        let tag = tag_of(name);
        let traits = wc.biosphere.iter().find(|(id, _)| id.0 == *name).unwrap().1;
        let floor = hornvale_kernel::sovereignty_floor(traits.mass, traits.potency);

        let mut counts = [0usize; FIT_THRESHOLDS.len()];
        let mut best = 0usize;
        for &c in &land {
            let cap = eff(c, tag);
            for (i, th) in FIT_THRESHOLDS.iter().enumerate() {
                if cap > *th {
                    counts[i] += 1;
                }
            }
            if cap > 0.0 {
                any_fit[c.0 as usize] = true;
            }
            // Item 4: survives at VIABLE_MIN but cannot throw a daughter.
            if cap > VIABLE_MIN * NEED / COLLAPSE_PRESSURE
                && cap <= DAUGHTER_POP / DAUGHTER_MAX_PRESSURE
            {
                cling[si] += 1;
            }
            // Best-fit among the settling six only.
            let mine = cap;
            let beaten = SETTLERS
                .iter()
                .any(|other| other != name && eff(c, tag_of(other)) > mine);
            if mine > 0.0 && !beaten {
                best += 1;
            }
        }
        println!(
            "    {:<10} {:>7.3} {:>9} {:>9} {:>9} {:>9} {:>8}",
            name, floor, counts[0], counts[1], counts[2], counts[3], best
        );
    }

    println!("  CLINGING BAND (survives at pop {VIABLE_MIN}, cannot throw a daughter):");
    for (si, name) in SETTLERS.iter().enumerate() {
        println!(
            "    {:<10} {:>7} cells ({:.2}% of land)",
            name,
            cling[si],
            pct(cling[si])
        );
    }

    // THE HEADROOM, correctly posed: cells someone could live on that today's
    // gate excludes, and cells the gate admits that nobody could live on anyway.
    let is_hab = |c: hornvale_kernel::CellId| -> bool {
        hornvale_climate::is_habitable(
            climate.mean_temperature_at(c),
            climate.moisture_at(c),
            terrain.elevation_at(c),
            sea,
        )
    };
    let mut newly_opened = 0usize;
    let mut newly_survivable = 0usize;
    let mut newly_expandable = 0usize;
    let mut dead_habitable = 0usize;
    for &c in &land {
        let best_eff = SETTLERS
            .iter()
            .map(|n| eff(c, tag_of(n)))
            .fold(0.0_f64, f64::max);
        let hab = is_hab(c);
        if best_eff > 0.0 && !hab {
            newly_opened += 1;
            if best_eff > VIABLE_MIN * NEED / COLLAPSE_PRESSURE {
                newly_survivable += 1;
            }
            if best_eff > DAUGHTER_POP / DAUGHTER_MAX_PRESSURE {
                newly_expandable += 1;
            }
        }
        if best_eff <= 0.0 && hab {
            dead_habitable += 1;
        }
    }
    println!(
        "  HEADROOM (correctly posed):\n\
         \x20   K>0 but gate-excluded (NEW ground)       : {newly_opened:6} ({:.2}% of land)\n\
         \x20     ...of which survivable (eff>1.0)       : {newly_survivable:6} ({:.2}%)\n\
         \x20     ...of which expandable (eff>11.43)     : {newly_expandable:6} ({:.2}%)\n\
         \x20   gate-admitted but K==0 (DEAD ground)     : {dead_habitable:6} ({:.2}%)",
        pct(newly_opened),
        pct(newly_survivable),
        pct(newly_expandable),
        pct(dead_habitable),
    );

    let expanded = any_fit.iter().filter(|b| **b).count();
    println!(
        "  EXPANSION: {expanded} land cells have K>0 for at least one settler \
         ({:.2}% of land) vs {} habitable today ({:.2}% of land) — ratio {:.2}x",
        pct(expanded),
        habitable.len(),
        pct(habitable.len()),
        expanded as f64 / habitable.len().max(1) as f64,
    );
}

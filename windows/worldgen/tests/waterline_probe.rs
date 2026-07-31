//! # CORRECTION (2026-07-26)
//!
//! This probe originally classified ocean as `elevation < 0.0`. **Sea level on
//! seed 42 is −2,936.17 m**, and terrain publishes the real predicate as
//! `is_ocean` (`elevation < sea_level`). The two disagree on 8,162 cells, so
//! every land/ocean figure this probe reported before the correction was
//! wrong — including the headline "the goblin dominates 930 cells, all below
//! sea level" (those 930 cells are all LAND) and "prey production is 77.9%
//! ocean" (it is 8.2%). The probe now uses `terrain.is_ocean` throughout, and
//! keeps an explicit check that the two tests disagree, so the trap cannot
//! silently return. See the campaign spec §11.
//!
//! FEASIBILITY PROBE (The Waterline; opened as The Chase / BIO-35 Stage 2) — measurement only, not a
//! shipped feature. Answers three questions before the prey-field spec is
//! written:
//!
//! ## Task 1 result: does support restriction move settlement placement? (P4)
//!
//! Measured 2026-07-26 on seed 42, this branch's base (includes The Vigil).
//! A throwaway gate was added as the first statement of `niche_per_species_k`'s
//! `CellMap::from_fn` closure in `windows/worldgen/src/lib.rs` (reverted
//! immediately after measuring, not shipped here):
//! ```text
//! if terrain.is_ocean(cell) { return 0.0; }
//! ```
//! - Baseline (`cargo run -q -p hornvale -- new --seed 42 --out
//!   /tmp/wl-before.json`): `world of seed 42 written to
//!   /tmp/wl-before.json (3553 facts; village: Vngoashshngaoshshngoogootao)` — matches
//!   the expected line exactly.
//! - Gated (`--out /tmp/wl-after.json`): `world of seed 42 written to
//!   /tmp/wl-after.json (3553 facts; village: Vngoashshngaoshshngoogootao)` — same fact
//!   count, same village name.
//! - `cargo test -p hornvale --test lens_purity`:
//!   `seed_42_world_json_matches_the_committed_fixture ... ok` — the
//!   committed fixture still matches with the gate applied.
//! - Fact-level diff (`/tmp/wl-before.json` vs `/tmp/wl-after.json`):
//!   `cmp` reports **byte-identical**; the Python set-diff over
//!   `(subject, predicate, object)` triples independently confirms
//!   `only-before: 0  only-after: 0` (3553 facts on both sides).
//!
//! **Answer: no, settlement placement does not move.** Zeroing every
//! species' carrying capacity at ocean cells (the exact form the shipped
//! fix will take) produced a byte-identical world at seed 42 — same fact
//! count, same fact set, same village name, same committed-fixture match.
//! The remaining Waterline tasks can proceed without a world-identity
//! re-scoping conversation or fixture re-pin.
//!
//! 1. **Does the home-range factor compose exactly once?** `home_range` is
//!    cells-per-individual and already divides a species' capacity share on
//!    the DENSITY side. If prey supply is additionally integrated over the
//!    home range on the SUPPLY side, the two are inverse operations — correct
//!    as a pair, a ~150x error if only one is applied. The goblin's home range
//!    is 1.01 by construction, so every existing test is structurally blind to
//!    this.
//! 2. **Does a dragon clear any cell** once `ANIMAL_PREY` has a real supply?
//! 3. **Do the four peoples separate spatially** on the prey axis, which is
//!    the axis their authored niches actually differ on?
//!
//! Run with:
//!   cargo test -p hornvale-worldgen --test waterline_probe -- --nocapture --ignored
//!
//! Test fixture (decision 0092): calls the sculpt/fit derivation entry
//! points directly to build its own world state, once per test — the
//! sanctioned test-fixture posture the weir's spec carves out.
#![allow(clippy::disallowed_methods)]

use hornvale_demography::home_range;
use hornvale_kernel::{ANIMAL_PREY, CellMap, DETRITUS, MINERAL, PHOTOSYNTHATE, PLANT_FORAGE};
use hornvale_worldgen::components::WorldComponents;
use hornvale_worldgen::{
    SettlementPins, SkyChoice, build_world, climate_of, niche_per_species_k, sky_of, terrain_of,
};

/// Lindeman trophic transfer efficiency — the Earth-anchored ~10%.
const TRANSFER_EFFICIENCY: f64 = 0.10;

#[test]
#[ignore = "probe: measurement only, run explicitly"]
fn waterline_probe() {
    let world = build_world(
        hornvale_kernel::Seed(42),
        &hornvale_astronomy::SkyPins::default(),
        SkyChoice::Generated,
        &hornvale_terrain::TerrainPins::default(),
        &SettlementPins::default(),
    )
    .expect("seed 42 builds");

    let terrain = terrain_of(&world).unwrap();
    let climate = climate_of(&world).unwrap();
    let sky = sky_of(&world).unwrap();
    let geo = terrain.geosphere();
    // Replicated from worldgen's private `stellar_inputs` using public
    // astronomy APIs, so the probe touches no source.
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

    let ks = niche_per_species_k(
        geo,
        &terrain,
        &climate,
        obliquity_deg,
        insolation_scalar,
        &regime,
        &bio,
    );
    let k_of = |tag: u32| -> &CellMap<f64> { &ks.iter().find(|(t, _)| *t == tag).unwrap().1 };
    let tag_of = |name: &str| -> u32 { names.iter().position(|n| *n == name).unwrap() as u32 };

    let cells: Vec<_> = geo.cells().collect();
    println!(
        "\n=== THE WATERLINE — feasibility probe (seed 42, {} cells)\n",
        cells.len()
    );

    // --- Home ranges, and the blindness they hide behind -------------------
    println!("-- home_range (cells per individual)");
    for (i, b) in bio.iter().enumerate() {
        let hr = home_range(b.mass);
        if hr > 3.0 || names[i] == "goblin" {
            println!(
                "   {:16} {:8.1} kg  {:8.2} cells/ind",
                names[i],
                b.mass.kilograms(),
                hr
            );
        }
    }

    // --- Prey production field --------------------------------------------
    // production = K_prey * reproductive_tempo, summed over every kind that
    // is NOT an obligate carnivore (i.e. anything a predator could eat that
    // itself rides a real axis), then scaled by transfer efficiency.
    // CORRECTION to the first probe pass: prey must be defined by the REAL
    // mass-windowed, trophic-gated food web (`niche::predation`), not by an
    // ad-hoc "anything that is not a pure carnivore" filter — which swept in
    // mineral-eaters and autotrophs whose K is non-zero at sea.
    let species_rows: Vec<(u32, hornvale_kernel::Mass, hornvale_kernel::ResourceVector)> = bio
        .iter()
        .enumerate()
        .map(|(i, b)| (i as u32, b.mass, b.niche.clone()))
        .collect();
    let web = hornvale_demography::niche::predation(&species_rows);
    println!("-- derived food web (mass-windowed, trophic-gated)");
    for (pred, prey) in web.iter() {
        let prey_names: Vec<&str> = prey.iter().map(|p| names[*p as usize]).collect();
        println!("   {:14} eats {:?}", names[*pred as usize], prey_names);
    }

    // Production of the prey a DRAGON can actually eat.
    let dragon_tag = names.iter().position(|n| *n == "white-dragon").unwrap() as u32;
    let dragon_prey: Vec<u32> = web.get(&dragon_tag).cloned().unwrap_or_default();
    let mut prod_vec: Vec<f64> = vec![0.0; cells.len()];
    for &prey_tag in &dragon_prey {
        let b = bio[prey_tag as usize];
        let lh = hornvale_species::life_history(b.mass, b.metabolic_class);
        let Some(r) = lh.reproductive_tempo else {
            continue;
        };
        let k = k_of(prey_tag);
        for (idx, &c) in cells.iter().enumerate() {
            prod_vec[idx] += *k.get(c) * r;
        }
    }
    let prod_at = |idx: usize| prod_vec[idx];
    let prod_total: f64 = prod_vec.iter().sum();
    let prod_max = prod_vec.iter().cloned().fold(0.0f64, f64::max);
    let prod_nonzero = prod_vec.iter().filter(|v| **v > 0.0).count();
    println!(
        "\n-- prey production (K x reproductive_tempo, summed over non-apex kinds)\n   total {prod_total:.3}  max/cell {prod_max:.6}  cells>0 {prod_nonzero}"
    );

    // --- Q2: does a dragon clear any cell? --------------------------------
    // Per-cell supply: the dragon eats only what is in THIS cell.
    // Home-range supply: the dragon eats across `home_range` cells, modelled
    // here as the mean production over a k-ring neighbourhood of that size.
    println!("\n-- dragon supply, per-cell vs home-range-integrated");
    for name in ["white-dragon", "red-dragon", "black-dragon"] {
        let b = bio[tag_of(name) as usize];
        let hr = home_range(b.mass);

        // supply available to ONE individual, per cell (Type-II saturated in
        // the real model; raw here so the magnitudes are legible).
        let per_cell_supply: Vec<f64> = (0..cells.len())
            .map(|i| TRANSFER_EFFICIENCY * prod_at(i))
            .collect();
        // Integrated: the individual ranges over hr cells, so it reaches hr
        // cells' worth of production. Modelled as hr * (local mean), which for
        // a uniform neighbourhood equals hr * local value.
        let integrated_supply: Vec<f64> = per_cell_supply.iter().map(|s| s * hr).collect();

        let pc_max = per_cell_supply.iter().cloned().fold(0.0f64, f64::max);
        let in_max = integrated_supply.iter().cloned().fold(0.0f64, f64::max);
        let pc_sat = pc_max / (1.0 + pc_max);
        let in_sat = in_max / (1.0 + in_max);
        println!(
            "   {name:14} hr {hr:6.1}  per-cell max supply {pc_max:.6} (saturated {pc_sat:.6})"
        );
        println!(
            "   {:14} {:6}  integrated  max supply {in_max:.6} (saturated {in_sat:.6})",
            "", ""
        );
    }

    // --- Q1: does home range compose exactly once? ------------------------
    // Total individuals supported over the whole world, computed two ways.
    //   (a) per-cell supply -> per-cell capacity -> divide by hr -> sum
    //   (b) integrated supply -> per-cell capacity -> divide by hr -> sum
    // These differ by exactly the hr factor if it is applied on only one side.
    println!("\n-- home-range composition check (total supported individuals)");
    for name in ["white-dragon", "goblin"] {
        let b = bio[tag_of(name) as usize];
        let hr = home_range(b.mass);
        let supply_pc: f64 = (0..cells.len())
            .map(|i| {
                let s = TRANSFER_EFFICIENCY * prod_at(i);
                s / (1.0 + s)
            })
            .sum();
        let supply_int: f64 = (0..cells.len())
            .map(|i| {
                let s = TRANSFER_EFFICIENCY * prod_at(i) * hr;
                s / (1.0 + s)
            })
            .sum();
        println!(
            "   {name:14} hr {hr:6.1}  Sum(sat per-cell)/hr = {:.4}   Sum(sat integrated)/hr = {:.4}   ratio {:.1}x",
            supply_pc / hr,
            supply_int / hr,
            (supply_int / supply_pc).max(0.0)
        );
    }

    // --- Q3: do the peoples separate on the prey axis? --------------------
    // Correlate each people's CURRENT K map (prey supply = 0) against the
    // prey production field. If the four peoples' prey weights matter, the
    // ordering of their correlation with production should track those
    // weights: bugbear 0.85 > kobold 0.45 ~ goblin 0.50 > hobgoblin 0.35.
    println!("\n-- peoples vs the prey axis (authored ANIMAL_PREY weight)");
    for name in ["bugbear", "goblin", "kobold", "hobgoblin"] {
        let tag = tag_of(name);
        let b = bio[tag as usize];
        let k = k_of(tag);
        let w = b.niche.weight(ANIMAL_PREY);
        let forage_w = b.niche.weight(PLANT_FORAGE);
        // Where is this people's K concentrated relative to production?
        let k_total: f64 = cells.iter().map(|&c| *k.get(c)).sum();
        let overlap: f64 = cells
            .iter()
            .enumerate()
            .map(|(i, &c)| *k.get(c) * prod_at(i))
            .sum::<f64>();
        println!(
            "   {name:12} prey_w {w:.2} forage_w {forage_w:.2}  K_total {k_total:10.2}  K.production {overlap:12.4}"
        );
    }

    // --- #5: WHY is production non-zero in every cell? --------------------
    // `ConditionResponse::eval` is a Gaussian bump plus a sovereignty floor,
    // so it is never exactly zero: `> 0.0` measures float positivity, not
    // ecological presence. The question that matters is the MAGNITUDE split
    // between land and sea.
    println!(
        "\n-- production: land vs ocean (ocean = terrain.is_ocean, i.e. elevation < sea_level)"
    );
    let mut land_total = 0.0f64;
    let mut sea_total = 0.0f64;
    let mut land_cells = 0usize;
    let mut sea_cells = 0usize;
    let mut land_max = 0.0f64;
    let mut sea_max = 0.0f64;
    for (i, &c) in cells.iter().enumerate() {
        let v = prod_at(i);
        if terrain.is_ocean(c) {
            sea_total += v;
            sea_cells += 1;
            sea_max = sea_max.max(v);
        } else {
            land_total += v;
            land_cells += 1;
            land_max = land_max.max(v);
        }
    }
    println!(
        "   land  {land_cells:6} cells  total {land_total:10.4}  max/cell {land_max:.6}  mean {:.8}",
        land_total / land_cells as f64
    );
    println!(
        "   ocean {sea_cells:6} cells  total {sea_total:10.4}  max/cell {sea_max:.6}  mean {:.8}",
        sea_total / sea_cells as f64
    );
    println!(
        "   ocean share of total production: {:.2}%",
        100.0 * sea_total / (land_total + sea_total)
    );

    // How much of that is the sovereignty floor rather than the Gaussian?
    println!("\n-- sovereignty_floor (buffers temperature/moisture/insolation, NOT elevation)");
    for name in ["goblin", "owlbear", "white-dragon", "red-dragon"] {
        let b = bio[tag_of(name) as usize];
        let f = hornvale_kernel::sovereignty_floor(b.mass, b.potency);
        println!(
            "   {name:14} mass {:7.1} kg  potency {:.3}  floor {f:.4}",
            b.mass.kilograms(),
            b.potency
        );
    }

    // --- Is the SHIPPED dominance result land-masked? ---------------------
    // `menagerie_full_roster_dominant_breakdown` iterates `geo.cells()` with
    // no land filter. Measure the split directly rather than inferring it.
    println!("\n-- dominance by land/ocean (the shipped metric's own definition)");
    let report = hornvale_worldgen::demography_report_from(&world, &wc, &terrain, &climate)
        .expect("demography report");
    let mut dom_land: std::collections::BTreeMap<u32, usize> = std::collections::BTreeMap::new();
    let mut dom_sea: std::collections::BTreeMap<u32, usize> = std::collections::BTreeMap::new();
    for &cell in &cells {
        let mut best: Option<(u32, f64)> = None;
        for (id, density) in &report.stack.density {
            let d = *density.get(cell);
            if d <= 0.0 {
                continue;
            }
            best = match best {
                None => Some((*id, d)),
                Some((_, bd)) if d.total_cmp(&bd) == std::cmp::Ordering::Greater => Some((*id, d)),
                other => other,
            };
        }
        if let Some((id, _)) = best {
            if terrain.is_ocean(cell) {
                *dom_sea.entry(id).or_insert(0) += 1;
            } else {
                *dom_land.entry(id).or_insert(0) += 1;
            }
        }
    }
    let mut rows: Vec<(&str, usize, usize)> = names
        .iter()
        .enumerate()
        .map(|(i, n)| {
            let i = i as u32;
            (
                *n,
                *dom_land.get(&i).unwrap_or(&0),
                *dom_sea.get(&i).unwrap_or(&0),
            )
        })
        .filter(|(_, l, s)| l + s > 0)
        .collect();
    rows.sort_by_key(|r| std::cmp::Reverse(r.1 + r.2));
    println!(
        "   {:16} {:>8} {:>8} {:>8}  ocean %",
        "kind", "land", "ocean", "total"
    );
    for (n, l, sea) in rows {
        let t = l + sea;
        println!(
            "   {n:16} {l:>8} {sea:>8} {t:>8}  {:.1}%",
            100.0 * sea as f64 / t as f64
        );
    }
    println!("   (world has {land_cells} land cells and {sea_cells} ocean cells)");

    // --- Do any DERIVED settlements sit at sea? (census-exposure check) ----
    // `composition-variance` (a census metric) reads the demography report's
    // `stack_settlements`. If none of those ever sat on an ocean cell, the
    // metric cannot have moved, and the census carve-out is not needed.
    println!("\n-- derived stack settlements, land vs ocean");
    let ss = &report.stack_settlements;
    let at_sea = ss.iter().filter(|s| terrain.is_ocean(s.cell)).count();
    println!(
        "   stack settlements: {} total, {} at sea",
        ss.len(),
        at_sea
    );

    // --- SEA LEVEL: is `elevation < 0` the same test as `is_ocean`? -------
    let sl = terrain.sea_level().get();
    let below_zero = cells
        .iter()
        .filter(|&&c| terrain.elevation_at(c).get() < 0.0)
        .count();
    let is_ocean = cells.iter().filter(|&&c| terrain.is_ocean(c)).count();
    let disagree = cells
        .iter()
        .filter(|&&c| (terrain.elevation_at(c).get() < 0.0) != terrain.is_ocean(c))
        .count();
    println!("\n-- sea level vs the zero datum");
    println!("   sea_level = {sl:.2} m");
    println!("   cells with elevation < 0 : {below_zero}");
    println!("   cells with is_ocean(c)   : {is_ocean}");
    println!("   cells where the two tests DISAGREE: {disagree}");

    // --- Does the EXISTING habitability mask reach every axis? ------------
    println!("\n-- habitability mask vs the per-axis supply fields");
    let habitable = climate.habitability();
    let hab_land = cells
        .iter()
        .filter(|&&c| *habitable.get(c) && !terrain.is_ocean(c))
        .count();
    let hab_sea = cells
        .iter()
        .filter(|&&c| *habitable.get(c) && terrain.is_ocean(c))
        .count();
    println!(
        "   habitable cells: {hab_land} on land, {hab_sea} at sea (of {land_cells} land / {sea_cells} ocean)"
    );

    // Per-axis supply at a sample of ocean cells: which axes are non-zero
    // where the mask says "not habitable"?
    let base_inputs_nonhab: Vec<hornvale_kernel::CellId> = cells
        .iter()
        .copied()
        .filter(|&c| !*habitable.get(c))
        .take(3)
        .collect();
    println!(
        "   sample NON-habitable cells, per-species K (should be ~0 if the mask reached every axis):"
    );
    for &c in &base_inputs_nonhab {
        let elev = terrain.elevation_at(c).get();
        let mut parts: Vec<String> = Vec::new();
        for name in ["goblin", "xorn", "rust-monster", "twig-blight"] {
            let t = tag_of(name);
            parts.push(format!("{name} {:.6}", k_of(t).get(c)));
        }
        println!("      cell elev {elev:8.1} m  {}", parts.join("  "));
    }

    // Sanity: confirm the axes the model currently reads.
    println!("\n-- axis basis sanity");
    for axis in [PHOTOSYNTHATE, PLANT_FORAGE, ANIMAL_PREY, DETRITUS, MINERAL] {
        println!("   {axis:?}");
    }
    println!();
}

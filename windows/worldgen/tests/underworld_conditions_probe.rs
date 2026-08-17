//! THE UNDERWORLD, Task 5: what does a chamber's substrate actually read, now
//! that it is not a constant?
//!
//! Two jobs in one file, for the reason `underworld_water_table_probe` states
//! for its own pair: the same code path must produce both, or the calibration
//! and the readout can disagree about which population they describe.
//!
//! 1. **The calibration** that chose `SEEPAGE_REACH_M`
//!    (`how_far_does_the_seepage_reach`), swept through the shipped
//!    `hornvale_worldgen::chamber_moisture_at_reach` so the table below is
//!    regenerable rather than transcribed.
//! 2. **The readout** (`what_does_a_chamber_read`): the temperature and
//!    moisture a chamber actually reads over every cave-bearing land cell of
//!    the three preregistered seeds, and whether two chambers can be told
//!    apart at all — which is the question the whole campaign turns on.
//!
//! ## The decision rule, frozen before the sweep was run
//!
//! `SEEPAGE_REACH_M` is authored, so what is frozen here is the *criterion*,
//! written and committed before any row of the table below existed:
//!
//! > Choose the candidate that leaves the vadose population **spread** rather
//! > than piled against either end of the axis — formally, the candidate
//! > minimising `max(share within 0.02 of the dry floor, share within 0.02 of
//! > saturation)` over the pooled vadose cells of the three seeds, tie-broken
//! > by the larger `p90 − p10` spread.
//!
//! The criterion is *not* "make the deep habitable" or "put the median near
//! some target". A constant is the defect this task exists to remove, and a
//! floor-pile and a saturation-pile are both constants wearing a distribution
//! for a hat; nothing else about where the mass sits is this constant's
//! business.
//!
//! ### The first clause turned out to carry no information, and the shipped
//! ### value comes from the tie-break
//!
//! Recorded here rather than quietly repaired, because "the criterion decided
//! it" would otherwise be a claim this run does not support.
//!
//! The dry-side pile is **0.0% at every candidate**: no chamber in three
//! worlds comes within 0.02 of `VADOSE_DRY_MOISTURE`, because the measured
//! rises above the table (p50 219 m) are small next to `reach × porosity` for
//! any candidate on the ladder. So `max-pile` is just the wet pile, which is
//! monotone in the constant, and minimising it returns whatever the **smallest
//! candidate offered** happens to be — an endpoint of a ladder chosen by the
//! author, not a measurement. The frozen tie-break decided instead, and it has
//! an interior maximum. `how_far_does_the_seepage_reach` asserts both halves
//! of that story (the dry pile never fires; clause 1 lands on the smallest
//! candidate) so that a later change which gives clause 1 information turns
//! this into a deliberate re-authoring rather than a silent inheritance.
//!
//! **The candidate ladder was refined after the first pass**, from five
//! geometric steps to nine. Disclosed because it is the kind of move that can
//! be metric-chasing: it was not, and the test of that is the direction. The
//! first pass put the spread maximum at an *interior* candidate with the two
//! neighbours below it, which is a resolution problem — the fix is more
//! candidates around the peak. Had the maximum sat at an endpoint the honest
//! move would have been to *extend* the ladder, not subdivide it.
//!
//! ## Measured 2026-08-17, seeds 42 / 7 / 1234
//!
//! ### The sweep (pooled vadose cells, n = 775)
//!
//! ```text
//!   rise above table m  p10=47.3 p25=111.3 p50=219.4 p75=349.2 p90=464.1 max=1095.7
//!   porosity            p10=0.455 p50=0.781 p90=0.817
//! reach_m |    p10    p50    p90 | dry-pile wet-pile | max-pile | spread
//!      75 |  0.191  0.269  0.562 |     0.0%     0.4% |     0.4% |  0.372
//!     150 |  0.265  0.384  0.711 |     0.0%     1.2% |     1.2% |  0.446
//!     225 |  0.326  0.468  0.784 |     0.0%     1.4% |     1.4% |  0.458
//!     300 |  0.378  0.532  0.828 |     0.0%     1.9% |     1.9% |  0.450
//!     450 |  0.461  0.622  0.877 |     0.0%     2.3% |     2.3% |  0.416
//!     600 |  0.525  0.683  0.905 |     0.0%     2.5% |     2.5% |  0.380
//!     900 |  0.616  0.761  0.934 |     0.0%     3.5% |     3.5% |  0.318
//!    1200 |  0.677  0.808  0.950 |     0.0%     4.6% |     4.6% |  0.272
//!    2400 |  0.803  0.892  0.974 |     0.0%     7.1% |     7.1% |  0.171
//! clause 1 (minimise the larger pile) picks reach_m = 75
//! clause 2 (maximise p90-p10 spread) picks reach_m = 225
//! ```
//!
//! **225 m ships**, and the peak is a **plateau**: 150 / 225 / 300 score
//! 0.446 / 0.458 / 0.450, within 3% of each other. Read the constant as "a
//! couple of hundred metres", not as a value fitted to three figures.
//!
//! ### The readout
//!
//! ```text
//! seed 42: cave columns=874
//!   chamber temperature C   p10=14.6 p50=30.5 p90=63.2   surface p10=3.2 p50=14.8 p90=21.7
//!   deltaT K                p10=5.6 p50=10.7 p90=55.0
//!   distinct chamber temperatures (0.01 C)=776 of 874
//!   moisture: saturated (phreatic) 599 (68.5%)  vadose 275 (31.5%)
//!   moisture vadose         p10=0.309 p50=0.415 p90=0.781
//!   distinct moistures (0.001)=214 of 874
//!   distinct (temperature, moisture) pairs=807 of 874
//!   drow niche fit  surface mean=0.039  chamber mean=0.056  chamber>surface on 870 (99.5%)
//!   deltaT by reach decile  shallowest p50=0.7 K (reach<=200 m)  deepest p50=56.0 K (reach>=2272 m)
//! seed 7: cave columns=1681
//!   chamber temperature C   p10=9.6 p50=22.3 p90=53.8   surface p10=-30.6 p50=3.9 p90=14.0
//!   deltaT K                p10=5.9 p50=33.3 p90=56.7
//!   distinct chamber temperatures (0.01 C)=1387 of 1681
//!   moisture: saturated (phreatic) 1390 (82.7%)  vadose 291 (17.3%)
//!   moisture vadose         p10=0.357 p50=0.489 p90=0.790
//!   distinct moistures (0.001)=222 of 1681
//!   distinct (temperature, moisture) pairs=1483 of 1681
//!   drow niche fit  surface mean=0.032  chamber mean=0.058  chamber>surface on 1681 (100.0%)
//!   deltaT by reach decile  shallowest p50=5.5 K (reach<=215 m)  deepest p50=57.1 K (reach>=2474 m)
//! seed 1234: cave columns=1266
//!   chamber temperature C   p10=-3.0 p50=16.1 p90=50.6   surface p10=-34.1 p50=-3.3 p90=5.3
//!   deltaT K                p10=5.8 p50=27.0 p90=58.9
//!   distinct chamber temperatures (0.01 C)=1137 of 1266
//!   moisture: saturated (phreatic) 1057 (83.5%)  vadose 209 (16.5%)
//!   moisture vadose         p10=0.332 p50=0.486 p90=0.783
//!   distinct moistures (0.001)=184 of 1266
//!   distinct (temperature, moisture) pairs=1172 of 1266
//!   drow niche fit  surface mean=0.036  chamber mean=0.061  chamber>surface on 1266 (100.0%)
//!   deltaT by reach decile  shallowest p50=0.8 K (reach<=202 m)  deepest p50=58.4 K (reach>=2694 m)
//! ```
//!
//! ## Why this file carries the only assertions that see the live path
//!
//! Two mutations, run at Task 5's close. Neutralising the depth term *inside*
//! `subterranean_substrate` (`gradient, 0.0 * depth_m / 1000.0`) reddens the
//! crate's unit tests immediately — they pass a depth in directly. Neutralising
//! it where the LIVE path supplies it (`0.0 * cave.depth_reach_m` in
//! `subterranean_substrate_field`) left **all 614 worldgen tests green, and both
//! tests in this file green as first written**: the unit tests never see the
//! field stop supplying a depth, and nothing else in the tree asserted on it.
//!
//! The two `deltaT` assertions in `what_does_a_chamber_read` were added under
//! that mutation and verified against it — the first catches a disconnected
//! geothermal term (ΔT p10 collapses to 0.0 K), and the second catches a
//! *constant* depth that the first would wave through (mutating to a flat
//! 500 m gives the deepest and shallowest reach deciles ΔT medians of 12.0 K
//! and 12.3 K; the shipped model gives 56.0 K and 0.7 K on seed 42).
//!
//! ## Five findings
//!
//! **1. Chambers can now be told apart, and it is temperature that does it.**
//! 807 / 1483 / 1172 distinct `(temperature, moisture)` readings over 874 /
//! 1681 / 1266 cave columns — 88–93%. Before this task every chamber in a
//! world read `(that cell's surface temperature, 0.90)`, so the only thing
//! separating two chambers was the surface reading they had not yet left.
//! Temperature carries most of the separation on its own (776 / 1387 / 1137
//! distinct at 0.01 °C) because it composes three independently-varying
//! per-cell terms — the climate datum, the geothermal gradient and the depth
//! budget — so `cave_depth_reach_m`'s clamp atoms do not survive into it.
//!
//! **2. Moisture varies, but the majority of it is one value.** 68.5 / 82.7 /
//! 83.5% of cave columns are flooded *at their reach depth* and read
//! saturated. That is not an artifact of this task: it is
//! `underworld_water_table_probe`'s own "sumped (cave bottom below the table)
//! = 68.5 / 82.7 / 83.5%" seen from the other side, and spec §4.2.1 clause 2 is
//! the model's answer to it — a `Made` chamber is drained regardless of the
//! table. **Nothing writes `Made` yet** (bound to §4.6's capacity task), so the
//! shipped reading says the deep is wet and will keep saying so until the
//! drainage rule gains its writer. Stated here rather than left inside a
//! percentile, because "moisture is no longer a constant" is true and would
//! mislead a reader about how much.
//!
//! **3. The deep is hot.** ΔT medians of 10.7 / 33.3 / 27.0 K and p90s of
//! 55–59 K put chamber temperatures at 30.5 / 22.3 / 16.1 °C median against
//! surface medians of 14.8 / 3.9 / −3.3 °C, with a p90 above 50 °C on every
//! seed. This is spec §4.1's habitable ceiling (ΔT = 50 K, authored) reaching
//! the suitability field for the first time; a deep-reaching column is now a
//! thermally hostile place, which is the discrimination the campaign wanted
//! and could not previously express.
//!
//! **4. None of this moves a world yet, and the reason is upstream of this
//! task.** The whole worldgen suite stayed green across Task 5 — not because
//! nothing changed, but because `tolerance_liebig` floors temperature,
//! moisture and insolation by the sovereignty floor while calling elevation
//! with floor `0.0`, so on a cave-bearing cell the unfloored elevation term is
//! the minimum and the other three cannot bind. `warren_readout`'s P1 tripwire
//! measures `ratio = 1.000` for rust-monster, xorn and drow before and after
//! (pooled means 0.039860 / 0.015078 / 0.029498 over 40,362 cave-bearing cells
//! across 25 seeds, unchanged to six figures). The `drow niche fit` column in
//! the readout above is the RAW four-axis product, not the Liebig minimum, so
//! it is the right instrument for "does the reading differ" and the wrong one
//! for "does placement move" — read it as the former only. Spec §4.5's
//! `EnvironmentNiche` and §4.6's realm-aware capacity are the consumers that
//! can see this; neither could have been built on a constant, which is why the
//! derivation comes first.
//!
//! **5. Drow read BETTER underground than on the surface on 99.5–100% of cave
//! columns, and the heat did not change that** — mean niche fit 0.056 / 0.058 /
//! 0.061 below against 0.039 / 0.032 / 0.036 above. The axis doing that work is
//! insolation, not moisture or temperature: drow's authored optimum is 0.02 at
//! width 0.10 and a chamber reads exactly 0.0, so the surface term is crushed
//! on nearly every land cell whatever the heat below does. Worth stating
//! plainly because it is the opposite of the naive expectation from finding 3,
//! and because it means the collapsed light axis spec §4.4 preregisters as a
//! *finding* is simultaneously the strongest signal a subterranean kind has.
#![allow(clippy::disallowed_methods)]

use hornvale_astronomy::SkyPins;
use hornvale_terrain::TerrainPins;
use hornvale_worldgen::{
    BuildDepth, SettlementPins, SkyChoice, Substrate, WorldComponents,
    build_world_to_with_artifacts, chamber_moisture_at_reach, climate_of, substrate_field,
    subterranean_substrate_field,
};

/// Seeds this campaign preregisters on (spec §5) — the same three
/// `underworld_water_table_probe` and `underworld_ladder_probe` use, so all
/// three readouts describe the same worlds.
const SEEDS: [u64; 3] = [42, 7, 1234];

/// The candidate seepage half-heights swept, metres. A geometric ladder rather
/// than a linear one: the term is a ratio `reach / (reach + rise)`, so equal
/// *factors* — not equal metre steps — are what move it by equal amounts.
const CANDIDATE_REACH_M: [f64; 9] = [
    75.0, 150.0, 225.0, 300.0, 450.0, 600.0, 900.0, 1200.0, 2400.0,
];

/// How close to an end of the moisture axis counts as "piled against it", in
/// moisture units. `0.02` is one fiftieth of the axis — small enough that a
/// genuinely spread population does not register, wide enough that a cluster
/// does.
const PILE_BAND: f64 = 0.02;

/// Percentile of an ascending slice.
fn pct(sorted: &[f64], q: f64) -> f64 {
    if sorted.is_empty() {
        return f64::NAN;
    }
    sorted[((sorted.len() - 1) as f64 * q).round() as usize]
}

/// The number of distinct values in a sample once bucketed at `unit`, so float
/// noise cannot split one spike into neighbours. The same instrument
/// `underworld_water_table_probe::atoms` counts with, reduced to its count.
fn distinct(values: &[f64], unit: f64) -> usize {
    let mut seen: std::collections::BTreeSet<i64> = std::collections::BTreeSet::new();
    for v in values {
        seen.insert((v / unit).round() as i64);
    }
    seen.len()
}

/// One seed's cave-bearing land cells, as `(surface, chamber)` substrate pairs
/// plus the hydrology inputs the sweep needs to re-evaluate moisture at other
/// calibrations.
struct CaveCells {
    /// The surface reading at each cave-bearing cell.
    surface: Vec<Substrate>,
    /// The chamber reading at the same cells, at the SHIPPED calibration.
    chamber: Vec<Substrate>,
    /// `(depth_m, water_table_m, porosity)` at the same cells — the three
    /// arguments `chamber_moisture_at_reach` needs.
    hydrology: Vec<(f64, f64, f64)>,
}

/// Build `seed` to `BuildDepth::Terrain` and collect its cave-bearing land
/// cells. `Terrain` is the shallowest rung carrying what this reads, matching
/// `underworld_water_table_probe`'s own fixture.
fn cave_cells(seed_value: u64, wc: &WorldComponents) -> CaveCells {
    let seed = hornvale_kernel::Seed(seed_value);
    let artifacts = build_world_to_with_artifacts(
        seed,
        &SkyPins::default(),
        SkyChoice::Generated,
        &TerrainPins::default(),
        &SettlementPins::default(),
        wc,
        BuildDepth::Terrain,
    )
    .expect("probe seed builds");
    let world = artifacts.world;
    let terrain = artifacts
        .terrain
        .expect("terrain is Some at BuildDepth::Terrain");
    let climate = climate_of(&world).expect("climate reconstructs");
    let geo = terrain.geosphere();

    let surface_field = substrate_field(
        geo,
        &terrain,
        &climate,
        climate.obliquity_deg(),
        climate.insolation(),
        &climate.regime(),
    );
    let chamber_field = subterranean_substrate_field(geo, &terrain, &surface_field);

    let mut out = CaveCells {
        surface: Vec::new(),
        chamber: Vec::new(),
        hydrology: Vec::new(),
    };
    for cell in geo.cells() {
        let Some(cave) = terrain.cave_at(cell) else {
            continue;
        };
        let s = *surface_field.get(cell);
        let porosity = terrain.material_at(cell).porosity;
        let water_table_m = hornvale_terrain::water_table_depth_m(
            terrain.drainage_at(cell),
            porosity,
            s.height_asl_m.get(),
        );
        out.surface.push(s);
        out.chamber.push(*chamber_field.get(cell));
        out.hydrology
            .push((cave.depth_reach_m, water_table_m, porosity));
    }
    out
}

/// claim: readout(off-gate, heavy:, prints the sweep, asserts only that the
/// shipped constant is the one the frozen criterion picks) — the calibration
/// behind `SEEPAGE_REACH_M`, over the three preregistered seeds.
#[test]
#[ignore = "heavy: live-worldgen battery; deferred from the commit gate to the heavy set (decision 0132)"]
fn how_far_does_the_seepage_reach() {
    let wc = WorldComponents::assemble().expect("canonical registries are well-formed");
    // Pool the three seeds' VADOSE cells: the criterion is about the shape of
    // the population the constant applies to, and that population is the union
    // of the worlds, not any one of them.
    let mut vadose: Vec<(f64, f64, f64)> = Vec::new();
    for seed_value in SEEDS {
        let cells = cave_cells(seed_value, &wc);
        for &(depth_m, table_m, porosity) in &cells.hydrology {
            if !hornvale_terrain::is_phreatic(depth_m, table_m) {
                vadose.push((depth_m, table_m, porosity));
            }
        }
    }
    assert!(
        vadose.len() > 100,
        "too few vadose cells ({}) to calibrate against",
        vadose.len()
    );
    // The inputs the constant is calibrated against, printed BEFORE the sweep:
    // a reach constant is only meaningful next to the rises it is competing
    // with, and a sweep table with no input distribution beside it cannot be
    // read at all.
    let mut rises: Vec<f64> = vadose.iter().map(|&(d, t, _)| t - d).collect();
    let mut porosities: Vec<f64> = vadose.iter().map(|&(_, _, p)| p).collect();
    rises.sort_by(f64::total_cmp);
    porosities.sort_by(f64::total_cmp);
    println!(
        "\n=== SEEPAGE_REACH_M sweep (pooled vadose cells, n = {}) ===",
        vadose.len()
    );
    println!(
        "  rise above table m  p10={:.1} p25={:.1} p50={:.1} p75={:.1} p90={:.1} max={:.1}",
        pct(&rises, 0.10),
        pct(&rises, 0.25),
        pct(&rises, 0.50),
        pct(&rises, 0.75),
        pct(&rises, 0.90),
        pct(&rises, 1.00),
    );
    println!(
        "  porosity            p10={:.3} p50={:.3} p90={:.3}",
        pct(&porosities, 0.10),
        pct(&porosities, 0.50),
        pct(&porosities, 0.90),
    );
    println!(
        "{:>7} | {:>6} {:>6} {:>6} | {:>8} {:>8} | {:>8} | {:>6}",
        "reach_m", "p10", "p50", "p90", "dry-pile", "wet-pile", "max-pile", "spread"
    );

    // Both clauses of the frozen rule are evaluated and both winners printed,
    // because which of them DECIDES is itself a finding: clause 1's dry-side
    // pile is identically 0.0% on this population, so `max-pile` is just the
    // wet pile, which is monotone in the constant — minimising it returns the
    // smallest candidate offered rather than measuring anything.
    let mut pile_winner: Option<(f64, f64)> = None;
    let mut spread_winner: Option<(f64, f64)> = None;
    let mut dry_pile_ever_fired = false;
    for reach in CANDIDATE_REACH_M {
        let mut moisture: Vec<f64> = vadose
            .iter()
            .map(|&(d, t, p)| chamber_moisture_at_reach(d, t, p, reach))
            .collect();
        moisture.sort_by(f64::total_cmp);
        let n = moisture.len() as f64;
        let dry_pile = moisture.iter().filter(|m| **m <= 0.10 + PILE_BAND).count() as f64 / n;
        let wet_pile = moisture.iter().filter(|m| **m >= 1.0 - PILE_BAND).count() as f64 / n;
        let max_pile = dry_pile.max(wet_pile);
        let spread = pct(&moisture, 0.90) - pct(&moisture, 0.10);
        println!(
            "{reach:>7.0} | {:>6.3} {:>6.3} {:>6.3} | {:>7.1}% {:>7.1}% | {:>7.1}% | {spread:>6.3}",
            pct(&moisture, 0.10),
            pct(&moisture, 0.50),
            pct(&moisture, 0.90),
            100.0 * dry_pile,
            100.0 * wet_pile,
            100.0 * max_pile,
        );
        dry_pile_ever_fired |= dry_pile > 0.0;
        if pile_winner.is_none_or(|(_, best)| max_pile < best) {
            pile_winner = Some((reach, max_pile));
        }
        if spread_winner.is_none_or(|(_, best)| spread > best) {
            spread_winner = Some((reach, spread));
        }
    }
    let (pile_pick, _) = pile_winner.expect("the sweep evaluated at least one candidate");
    let (spread_pick, _) = spread_winner.expect("the sweep evaluated at least one candidate");
    println!("clause 1 (minimise the larger pile) picks reach_m = {pile_pick:.0}");
    println!("clause 2 (maximise p90-p10 spread) picks reach_m = {spread_pick:.0}");

    // Clause 1 is recorded as non-discriminating, and that record is an
    // ASSERTION rather than a sentence: if a later change to the water table,
    // the depth budget or `VADOSE_DRY_MOISTURE` ever pushes chambers down to
    // the dry floor, this goes red and the constant is re-authored on the
    // criterion as frozen rather than inheriting the tie-break by habit.
    assert!(
        !dry_pile_ever_fired,
        "the dry-side pile now fires, so clause 1 of the frozen criterion has \
         information again — re-author SEEPAGE_REACH_M on the full rule, not \
         on its tie-break"
    );
    assert_eq!(
        pile_pick, CANDIDATE_REACH_M[0],
        "clause 1 was expected to degenerate to the smallest candidate; it \
         picked {pile_pick} instead, which means it is discriminating after all"
    );

    // The one substantive assertion: the shipped constant is the one the
    // deciding clause picks. Compared through a probe point rather than by
    // reading the private constant — a chamber 300 m above its table in
    // median-porosity rock, which is squarely inside the measured population
    // and where the candidates are furthest apart.
    assert_eq!(
        hornvale_worldgen::chamber_moisture(0.0, 300.0, 0.781),
        chamber_moisture_at_reach(0.0, 300.0, 0.781, spread_pick),
        "the shipped SEEPAGE_REACH_M no longer agrees with the criterion's \
         pick of {spread_pick:.0} m — re-author it deliberately"
    );
}

/// claim: readout(off-gate, heavy:, prints the distribution, asserts only that
/// chambers are distinguishable at all) — what a chamber reads over every
/// cave-bearing land cell of the three preregistered seeds.
#[test]
#[ignore = "heavy: live-worldgen battery; deferred from the commit gate to the heavy set (decision 0132)"]
fn what_does_a_chamber_read() {
    let wc = WorldComponents::assemble().expect("canonical registries are well-formed");
    let bio = wc
        .biosphere
        .get_by_label("drow")
        .expect("drow is on the roster");
    let floor = hornvale_kernel::sovereignty_floor(bio.mass, bio.potency);
    let cn = &bio.condition_niche;
    let fit = |s: &Substrate| {
        cn.temperature.eval(s.temperature_c, floor)
            * cn.moisture.eval(s.moisture, floor)
            * cn.insolation.eval(s.insolation, floor)
            * cn.elevation.eval(s.height_asl_m.get(), 0.0)
    };

    for seed_value in SEEDS {
        let cells = cave_cells(seed_value, &wc);
        let n = cells.chamber.len();
        assert!(n > 0, "seed {seed_value} has no caves");

        let mut chamber_t: Vec<f64> = cells.chamber.iter().map(|s| s.temperature_c).collect();
        let mut surface_t: Vec<f64> = cells.surface.iter().map(|s| s.temperature_c).collect();
        let mut delta_t: Vec<f64> = cells
            .chamber
            .iter()
            .zip(&cells.surface)
            .map(|(c, s)| c.temperature_c - s.temperature_c)
            .collect();
        let all_moisture: Vec<f64> = cells.chamber.iter().map(|s| s.moisture).collect();
        let mut vadose_moisture: Vec<f64> = cells
            .chamber
            .iter()
            .zip(&cells.hydrology)
            .filter(|(_, hydro)| !hornvale_terrain::is_phreatic(hydro.0, hydro.1))
            .map(|(s, _)| s.moisture)
            .collect();
        let saturated = n - vadose_moisture.len();
        chamber_t.sort_by(f64::total_cmp);
        surface_t.sort_by(f64::total_cmp);
        delta_t.sort_by(f64::total_cmp);
        vadose_moisture.sort_by(f64::total_cmp);

        // The campaign's actual question: can two chambers be told apart? A
        // pair bucketed at (0.01 C, 0.001 moisture) is the resolution below
        // which a difference is not a difference.
        let mut pairs: std::collections::BTreeSet<(i64, i64)> = std::collections::BTreeSet::new();
        for s in &cells.chamber {
            pairs.insert((
                (s.temperature_c * 100.0).round() as i64,
                (s.moisture * 1000.0).round() as i64,
            ));
        }

        let surface_fit: f64 = cells.surface.iter().map(fit).sum::<f64>() / n as f64;
        let chamber_fit: f64 = cells.chamber.iter().map(fit).sum::<f64>() / n as f64;
        let better_below = cells
            .chamber
            .iter()
            .zip(&cells.surface)
            .filter(|(c, s)| fit(c) > fit(s))
            .count();

        println!("\nseed {seed_value}: cave columns={n}");
        println!(
            "  chamber temperature C   p10={:.1} p50={:.1} p90={:.1}   surface p10={:.1} p50={:.1} p90={:.1}",
            pct(&chamber_t, 0.10),
            pct(&chamber_t, 0.50),
            pct(&chamber_t, 0.90),
            pct(&surface_t, 0.10),
            pct(&surface_t, 0.50),
            pct(&surface_t, 0.90),
        );
        println!(
            "  deltaT K                p10={:.1} p50={:.1} p90={:.1}",
            pct(&delta_t, 0.10),
            pct(&delta_t, 0.50),
            pct(&delta_t, 0.90),
        );
        println!(
            "  distinct chamber temperatures (0.01 C)={} of {n}",
            distinct(&chamber_t, 0.01)
        );
        println!(
            "  moisture: saturated (phreatic) {saturated} ({:.1}%)  vadose {} ({:.1}%)",
            100.0 * saturated as f64 / n as f64,
            vadose_moisture.len(),
            100.0 * vadose_moisture.len() as f64 / n as f64,
        );
        println!(
            "  moisture vadose         p10={:.3} p50={:.3} p90={:.3}",
            pct(&vadose_moisture, 0.10),
            pct(&vadose_moisture, 0.50),
            pct(&vadose_moisture, 0.90),
        );
        println!(
            "  distinct moistures (0.001)={} of {n}",
            distinct(&all_moisture, 0.001)
        );
        println!(
            "  distinct (temperature, moisture) pairs={} of {n}",
            pairs.len()
        );
        println!(
            "  drow niche fit  surface mean={surface_fit:.3}  chamber mean={chamber_fit:.3}  chamber>surface on {better_below} ({:.1}%)",
            100.0 * better_below as f64 / n as f64,
        );

        // The one assertion, and it is the campaign's own headline claim
        // rather than a number: chambers must be tellable apart. Before this
        // task every chamber in a world read `(surface temperature, 0.90)`,
        // so this held only as far as the surface datum varied and moisture
        // contributed nothing at all. A model that regressed to a constant
        // would collapse this count.
        assert!(
            pairs.len() > n / 2,
            "seed {seed_value}: only {} distinct chamber readings over {n} cave \
             columns — chambers are not tellable apart",
            pairs.len()
        );
        // And moisture specifically must not be back to one value.
        assert!(
            distinct(&all_moisture, 0.001) > 1,
            "seed {seed_value}: moisture is a constant again"
        );

        // THE LIVE-PATH DEPTH ASSERTION, and it exists because a mutation
        // proved it had to. Neutralising the depth this field derives from a
        // cave's reach (`0.0 * cave.depth_reach_m` in
        // `subterranean_substrate_field`) left the whole 614-test worldgen
        // suite AND both tests in this file green: the unit tests pass a depth
        // in directly, so they never see the live path stop supplying one.
        // These two assertions are the ones that go red for it.
        //
        // First: a chamber is warmer than the cell above it, everywhere. A
        // p10 above 1 K cannot be satisfied by a world where the geothermal
        // term has been disconnected.
        assert!(
            pct(&delta_t, 0.10) > 1.0,
            "seed {seed_value}: the coldest tenth of chambers sit only {:.3} K \
             above their surface datum — the geothermal term is not reaching \
             the live substrate field",
            pct(&delta_t, 0.10)
        );
        // Second, and this is the one that pins DEPTH rather than merely some
        // offset: sort the columns by how far their caves reach and compare
        // the deepest tenth against the shallowest tenth. A model that added a
        // constant warming would pass the assertion above and fail this one.
        let mut by_reach: Vec<(f64, f64)> = cells
            .hydrology
            .iter()
            .zip(&cells.surface)
            .zip(&cells.chamber)
            .map(|((hydro, s), c)| (hydro.0, c.temperature_c - s.temperature_c))
            .collect();
        by_reach.sort_by(|a, b| a.0.total_cmp(&b.0));
        let decile = (n / 10).max(1);
        let mut shallow: Vec<f64> = by_reach[..decile].iter().map(|&(_, dt)| dt).collect();
        let mut deep: Vec<f64> = by_reach[n - decile..].iter().map(|&(_, dt)| dt).collect();
        shallow.sort_by(f64::total_cmp);
        deep.sort_by(f64::total_cmp);
        println!(
            "  deltaT by reach decile  shallowest p50={:.1} K (reach<={:.0} m)  deepest p50={:.1} K (reach>={:.0} m)",
            pct(&shallow, 0.50),
            by_reach[decile - 1].0,
            pct(&deep, 0.50),
            by_reach[n - decile].0,
        );
        assert!(
            pct(&deep, 0.50) > pct(&shallow, 0.50) + 1.0,
            "seed {seed_value}: the deepest tenth of caves ({:.3} K above datum) \
             is not warmer than the shallowest tenth ({:.3} K) — depth is not \
             reaching the chamber's temperature in a real world",
            pct(&deep, 0.50),
            pct(&shallow, 0.50)
        );
    }
}

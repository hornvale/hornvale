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
//!    moisture a chamber actually reads over every cave-bearing land vertex of
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
//! > saturation)` over the pooled vadose vertices of the three seeds, tie-broken
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
//! ### The sweep (pooled vadose vertices, n = 775)
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
//!   distinct (temperature, moisture) pairs=807 of 874   CONTROL (pre-change reading)=691
//!   surface-temperature buckets carrying >1 distinct chamber reading=135 of 691 (pre-change: 0 by construction)
//!   drow niche fit  surface mean=0.039  chamber mean=0.056  chamber>surface on 870 (99.5%)
//!   deltaT by reach decile  shallowest p50=0.7 K (reach<=200 m)  deepest p50=56.0 K (reach>=2272 m)
//! seed 7: cave columns=1681
//!   chamber temperature C   p10=9.6 p50=22.3 p90=53.8   surface p10=-30.6 p50=3.9 p90=14.0
//!   deltaT K                p10=5.9 p50=33.3 p90=56.7
//!   distinct chamber temperatures (0.01 C)=1387 of 1681
//!   moisture: saturated (phreatic) 1390 (82.7%)  vadose 291 (17.3%)
//!   moisture vadose         p10=0.357 p50=0.489 p90=0.790
//!   distinct moistures (0.001)=222 of 1681
//!   distinct (temperature, moisture) pairs=1483 of 1681   CONTROL (pre-change reading)=1323
//!   surface-temperature buckets carrying >1 distinct chamber reading=263 of 1323 (pre-change: 0 by construction)
//!   drow niche fit  surface mean=0.032  chamber mean=0.058  chamber>surface on 1681 (100.0%)
//!   deltaT by reach decile  shallowest p50=5.5 K (reach<=215 m)  deepest p50=57.1 K (reach>=2474 m)
//! seed 1234: cave columns=1266
//!   chamber temperature C   p10=-3.0 p50=16.1 p90=50.6   surface p10=-34.1 p50=-3.3 p90=5.3
//!   deltaT K                p10=5.8 p50=27.0 p90=58.9
//!   distinct chamber temperatures (0.01 C)=1137 of 1266
//!   moisture: saturated (phreatic) 1057 (83.5%)  vadose 209 (16.5%)
//!   moisture vadose         p10=0.332 p50=0.486 p90=0.783
//!   distinct moistures (0.001)=184 of 1266
//!   distinct (temperature, moisture) pairs=1172 of 1266   CONTROL (pre-change reading)=1030
//!   surface-temperature buckets carrying >1 distinct chamber reading=180 of 1030 (pre-change: 0 by construction)
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
//! **1. The headline is the depth gradient, not the pair count — and the
//! first version of this file got that wrong by omitting a control.**
//!
//! The pair count rose from **691 → 807, 1323 → 1483, 1030 → 1172** (+16.8 /
//! +12.1 / +13.8%). The control is not a second world: before this task a
//! chamber read `(that vertex's own surface temperature, SUBTERRANEAN_MOISTURE)`,
//! so with moisture constant the pre-change pair count is exactly the number
//! of distinct surface temperatures, computed here from the same field on the
//! same worlds. **The surface temperature already varied per vertex, so the
//! "before" figure was never 1**, and the sentence "807/874 carry a distinct
//! reading where before there was one value" — which this file, the task
//! report and spec §4.3 all carried — is wrong. A real but modest rise is what
//! the pair count shows.
//!
//! What the pair count *cannot* show, and what is genuinely new, is that the
//! chamber reading now carries information the surface reading does not:
//! **135 / 263 / 180 surface-temperature buckets (17.5–19.9%) carry more than
//! one distinct chamber reading**, where pre-change there could be **zero by
//! construction**, the chamber being a pure function of that bucket. That is
//! the assertion this file now pins, because it is the one the old model fails
//! necessarily rather than probably.
//!
//! **The controlled headline is the ΔT spread by depth: 0.7 → 56.0 K between
//! the shallowest and deepest reach deciles on seed 42** (5.5 → 57.1 and
//! 0.8 → 58.4 on the others). Its control is exact and needs no estimate:
//! pre-change ΔT was **identically 0.0 K at every cave column in every
//! world**, because temperature passed through. An eighty-fold spread opening
//! out of a flat zero is the result; the pair count is a corollary of it.
//!
//! **2. Moisture varies, but the majority of it is one value.** 68.5 / 82.7 /
//! 83.5% of cave columns are flooded *at their reach depth* and read
//! saturated. That is not an artifact of this task: it is
//! `underworld_water_table_probe`'s own "sumped (cave bottom below the table)
//! = 68.5 / 82.7 / 83.5%" seen from the other side, and spec §4.2.1 clause 2 is
//! the model's answer to it — a `Made` chamber is drained regardless of the
//! table. **`Made` has a writer now and still no call site**: §4.6's capacity
//! task shipped `delve_seating::made_chambers`, but nothing in the shipped
//! path constructs the override map it writes into, so the reading below says
//! the deep is wet and will keep saying so until the drainage rule gains a
//! *caller*, not merely a writer. (This block said "nothing writes `Made` yet"
//! after the writer landed.) Stated here rather than left inside a
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
//! with floor `0.0`, so on a cave-bearing vertex the unfloored elevation term is
//! the minimum and the other three cannot bind. `warren_readout`'s P1 tripwire
//! measures `ratio = 1.000` for rust-monster, xorn and drow before and after
//! (pooled means 0.039860 / 0.015078 / 0.029498 over 40,362 cave-bearing vertices
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
//! on nearly every land vertex whatever the heat below does. Worth stating
//! plainly because it is the opposite of the naive expectation from finding 3,
//! and because it means the collapsed light axis spec §4.4 preregisters as a
//! *finding* is simultaneously the strongest signal a subterranean kind has.
//!
//! Test fixture (decision 0092): calls the composition-root entry points
//! directly, the sanctioned posture for this crate's live-worldgen batteries.
#![allow(clippy::disallowed_methods)]

use hornvale_astronomy::SkyPins;
use hornvale_terrain::TerrainPins;
use hornvale_worldgen::{
    BuildDepth, SettlementPins, SkyChoice, Substrate, WorldComponents,
    build_world_to_with_artifacts, chamber_moisture_at_reach, climate_of, substrate_field,
    subterranean_substrate_field, subterranean_substrate_field_per_rung,
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

/// **AUTHORED.** How close to an end of the moisture axis counts as "piled
/// against it", in moisture units.
///
/// `0.02` is one fiftieth of the axis — small enough that a genuinely spread
/// population does not register, wide enough that a cluster does. Nothing
/// derives it, and it is worth flagging because it is the constant the frozen
/// criterion's *failed* clause is measured against: the dry-side pile reads
/// 0.0% at every candidate, and this band is what "0.0%" is 0.0% of. Widening
/// it would not rescue that clause by any plausible amount — at the shipped
/// constant the vadose p10 sits at 0.326, eleven bands above the floor, and
/// even at the smallest candidate on the ladder it is 0.191, four bands above
/// — but a reader checking whether the clause failed for a real reason or a
/// narrow window deserves the numbers rather than an assurance.
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

/// One seed's cave-bearing land vertices, as `(surface, chamber)` substrate pairs
/// plus the hydrology inputs the sweep needs to re-evaluate moisture at other
/// calibrations.
struct CaveVertices {
    /// The surface reading at each cave-bearing vertex.
    surface: Vec<Substrate>,
    /// The chamber reading at the same vertices, at the SHIPPED calibration.
    chamber: Vec<Substrate>,
    /// `(depth_m, water_table_m, porosity)` at the same vertices — the three
    /// arguments `chamber_moisture_at_reach` needs.
    hydrology: Vec<(f64, f64, f64)>,
}

/// Build `seed` to `BuildDepth::Terrain` and collect its cave-bearing land
/// vertices. `Terrain` is the shallowest rung carrying what this reads, matching
/// `underworld_water_table_probe`'s own fixture.
fn cave_vertices(seed_value: u64, wc: &WorldComponents) -> CaveVertices {
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

    let mut out = CaveVertices {
        surface: Vec::new(),
        chamber: Vec::new(),
        hydrology: Vec::new(),
    };
    for vertex in geo.vertices() {
        let Some(cave) = terrain.cave_at(vertex) else {
            continue;
        };
        let s = *surface_field.get(vertex);
        let porosity = terrain.material_at(vertex).porosity;
        let water_table_m = hornvale_terrain::water_table_depth_m(
            terrain.drainage_at(vertex),
            porosity,
            s.height_asl_m.get(),
        );
        out.surface.push(s);
        out.chamber.push(*chamber_field.get(vertex));
        out.hydrology
            .push((cave.depth_reach_m, water_table_m, porosity));
    }
    out
}

/// The one seed the non-heavy live-path guard below builds. Named rather than
/// spelled inline so it is obviously ONE world and not a sweep.
const LIVE_GUARD_SEED: u64 = 42;

/// claim: structural(seed: 42) — one world, one build, no sweep.
///
/// **The only live-path guard in this file that either gate can reach**, and
/// it exists because a mutation showed the seam was otherwise unguarded:
/// neutralising the depth `subterranean_substrate_field` derives from a cave's
/// reach left all 614 worldgen tests green. The two assertions that catch it
/// live in `what_does_a_chamber_read`, which carries the canonical heavy
/// ignore token and therefore runs only under `make heavy-remote` — off
/// `gate-commit` and off the stage gate. (Spelling that attribute out here,
/// even inside backticks and with an ellipsis for its reason, reddens
/// `cli/tests/heavy_tier.rs`: its scan is source-level and cannot tell a
/// mention from a use. Found the hard way; do not restore it.) A guard nothing routinely runs is a guard with the reach of
/// a comment, so the cheapest half is duplicated here without the ignore.
///
/// It is cheap because one `BuildDepth::Terrain` world costs about a second —
/// the same posture `warren_gate.rs` already takes for a non-ignored
/// live-worldgen test. **Its reach is the stage gate**, and `gate-commit` only
/// once the sub-floor roster has a recorded duration for it; coverage is the
/// stage gate's job by design (root `CLAUDE.md`), so that is the honest
/// statement of where this runs rather than a claim that it runs everywhere.
///
/// Deliberately ONE seed and TWO assertions: the distribution work, the
/// calibration and the readout stay heavy, because those are what cost. This
/// asks only the question the mutation exposed — does the live field still
/// carry a depth at all.
#[test]
fn the_live_substrate_field_carries_depth() {
    let wc = WorldComponents::assemble().expect("canonical registries are well-formed");
    let vertices = cave_vertices(LIVE_GUARD_SEED, &wc);
    let n = vertices.chamber.len();
    assert!(n > 0, "seed {LIVE_GUARD_SEED} has no caves");

    let mut delta_t: Vec<f64> = vertices
        .chamber
        .iter()
        .zip(&vertices.surface)
        .map(|(c, s)| c.temperature_c - s.temperature_c)
        .collect();
    delta_t.sort_by(f64::total_cmp);
    assert!(
        pct(&delta_t, 0.10) > 1.0,
        "the coldest tenth of chambers sit only {:.3} K above their surface \
         datum — the geothermal term is not reaching the live substrate field",
        pct(&delta_t, 0.10)
    );

    // And the half a constant depth would pass: deeper caves must be hotter.
    let mut by_reach: Vec<(f64, f64)> = vertices
        .hydrology
        .iter()
        .zip(&vertices.surface)
        .zip(&vertices.chamber)
        .map(|((hydro, s), c)| (hydro.0, c.temperature_c - s.temperature_c))
        .collect();
    by_reach.sort_by(|a, b| a.0.total_cmp(&b.0));
    let decile = (n / 10).max(1);
    let mut shallow: Vec<f64> = by_reach[..decile].iter().map(|&(_, dt)| dt).collect();
    let mut deep: Vec<f64> = by_reach[n - decile..].iter().map(|&(_, dt)| dt).collect();
    shallow.sort_by(f64::total_cmp);
    deep.sort_by(f64::total_cmp);
    assert!(
        pct(&deep, 0.50) > pct(&shallow, 0.50) + 1.0,
        "the deepest tenth of caves ({:.3} K above datum) is not warmer than \
         the shallowest tenth ({:.3} K) — depth is not reaching the chamber's \
         temperature in a real world",
        pct(&deep, 0.50),
        pct(&shallow, 0.50)
    );
}

/// Build `seed` to `BuildDepth::Terrain` and return its terrain and surface
/// substrate field — the two inputs both `subterranean_substrate_field` and
/// `subterranean_substrate_field_per_rung` need. Mirrors `cave_vertices`'s
/// own world-building idiom above; returns `terrain` rather than a `geo`
/// reference directly, because `Geosphere` borrows from `GeneratedTerrain`
/// and the two cannot be packaged as an owned pair — callers derive
/// `terrain.geosphere()` themselves once both are in scope.
fn terrain_and_surface(
    seed_value: u64,
    wc: &WorldComponents,
) -> (
    hornvale_terrain::GeneratedTerrain,
    hornvale_kernel::VertexMap<Substrate>,
) {
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

    let surface = substrate_field(
        geo,
        &terrain,
        &climate,
        climate.obliquity_deg(),
        climate.insolation(),
        &climate.regime(),
    );
    (terrain, surface)
}

/// claim: structural(seed: 42) — one world, one build, no sweep.
///
/// THE POSITIVE CONTROL. `subterranean_substrate_field` reads every
/// cave-bearing vertex at `depth_reach_m`; `rung_evaluation_depth_m` gives
/// `Band::Nadir` that same depth (Task 2's own doc). So the per-rung field's
/// `Nadir` entry must equal the old field EXACTLY — bit-for-bit, not
/// approximately — on every cave-bearing vertex. A change that moves this
/// has changed something it was not asked to change.
#[test]
fn the_deepest_rung_reproduces_todays_per_vertex_reading() {
    let wc = WorldComponents::assemble().expect("canonical registries are well-formed");
    let (terrain, surface) = terrain_and_surface(LIVE_GUARD_SEED, &wc);
    let geo = terrain.geosphere();
    let old = subterranean_substrate_field(geo, &terrain, &surface);
    let new = subterranean_substrate_field_per_rung(geo, &terrain, &surface);
    let mut compared = 0;
    for vertex in geo.vertices() {
        if terrain.cave_at(vertex).is_none() {
            continue;
        }
        let nadir = new.get(vertex)[hornvale_kernel::Band::Nadir as usize]
            .expect("a cave-bearing vertex has a Nadir reading");
        let was = old.get(vertex);
        assert_eq!(
            nadir.temperature_c.to_bits(),
            was.temperature_c.to_bits(),
            "vertex {vertex:?}: Nadir temperature moved"
        );
        assert_eq!(
            nadir.moisture.to_bits(),
            was.moisture.to_bits(),
            "vertex {vertex:?}: Nadir moisture moved"
        );
        compared += 1;
    }
    assert!(
        compared > 100,
        "only {compared} cave-bearing vertices compared — vacuous"
    );
}

/// claim: structural(seed: 42) — one world, one build, no sweep.
///
/// The floor the control above needs. A per-rung field where every rung
/// equalled `Nadir` would pass the control and mean nothing changed at all.
#[test]
fn shallower_rungs_are_cooler_than_the_deepest() {
    let wc = WorldComponents::assemble().expect("canonical registries are well-formed");
    let (terrain, surface) = terrain_and_surface(LIVE_GUARD_SEED, &wc);
    let geo = terrain.geosphere();
    let field = subterranean_substrate_field_per_rung(geo, &terrain, &surface);
    let mut vertices_with_a_spread = 0;
    for vertex in geo.vertices() {
        if terrain.cave_at(vertex).is_none() {
            continue;
        }
        let rungs = field.get(vertex);
        let nadir = rungs[hornvale_kernel::Band::Nadir as usize].expect("Nadir reading");
        if let Some(u) = rungs[hornvale_kernel::Band::Undercroft as usize]
            && u.temperature_c < nadir.temperature_c
        {
            vertices_with_a_spread += 1;
        }
    }
    assert!(
        vertices_with_a_spread > 100,
        "only {vertices_with_a_spread} vertices show a shallow/deep temperature \
         spread — the per-rung field has collapsed to the per-vertex one"
    );
}

/// claim: readout(off-gate, heavy:, prints the sweep, asserts only that the
/// shipped constant is the one the frozen criterion picks) — the calibration
/// behind `SEEPAGE_REACH_M`, over the three preregistered seeds.
#[test]
#[ignore = "heavy: live-worldgen battery; deferred from the commit gate to the heavy set (decision 0132)"]
fn how_far_does_the_seepage_reach() {
    let wc = WorldComponents::assemble().expect("canonical registries are well-formed");
    // Pool the three seeds' VADOSE vertices: the criterion is about the shape of
    // the population the constant applies to, and that population is the union
    // of the worlds, not any one of them.
    let mut vadose: Vec<(f64, f64, f64)> = Vec::new();
    for seed_value in SEEDS {
        let vertices = cave_vertices(seed_value, &wc);
        for &(depth_m, table_m, porosity) in &vertices.hydrology {
            if !hornvale_terrain::is_phreatic(depth_m, table_m) {
                vadose.push((depth_m, table_m, porosity));
            }
        }
    }
    assert!(
        vadose.len() > 100,
        "too few vadose vertices ({}) to calibrate against",
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
        "\n=== SEEPAGE_REACH_M sweep (pooled vadose vertices, n = {}) ===",
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
/// cave-bearing land vertex of the three preregistered seeds.
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
        let vertices = cave_vertices(seed_value, &wc);
        let n = vertices.chamber.len();
        assert!(n > 0, "seed {seed_value} has no caves");

        let mut chamber_t: Vec<f64> = vertices.chamber.iter().map(|s| s.temperature_c).collect();
        let mut surface_t: Vec<f64> = vertices.surface.iter().map(|s| s.temperature_c).collect();
        let mut delta_t: Vec<f64> = vertices
            .chamber
            .iter()
            .zip(&vertices.surface)
            .map(|(c, s)| c.temperature_c - s.temperature_c)
            .collect();
        let all_moisture: Vec<f64> = vertices.chamber.iter().map(|s| s.moisture).collect();
        let mut vadose_moisture: Vec<f64> = vertices
            .chamber
            .iter()
            .zip(&vertices.hydrology)
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
        //
        // AND ITS CONTROL, which the first version of this probe did not take
        // and which made its headline wrong. The PRE-CHANGE chamber reading
        // was `(that vertex's own surface temperature, SUBTERRANEAN_MOISTURE)` —
        // the temperature already varied per vertex, so the "before" count was
        // never 1. With moisture constant, the pre-change pair count is
        // exactly the number of distinct surface temperatures at the same
        // bucket width, computable here from the surface field the probe
        // already holds. No before-arm build is needed; it is the same world,
        // read the way the old code read it.
        let mut pairs: std::collections::BTreeSet<(i64, i64)> = std::collections::BTreeSet::new();
        // Chamber readings grouped by the surface temperature bucket that,
        // before this change, DETERMINED them. A bucket carrying two distinct
        // chamber readings is information the surface reading does not have —
        // and pre-change there could be none, by construction, because the
        // chamber reading was a pure function of that bucket.
        let mut by_surface: std::collections::BTreeMap<
            i64,
            std::collections::BTreeSet<(i64, i64)>,
        > = std::collections::BTreeMap::new();
        for (c, s) in vertices.chamber.iter().zip(&vertices.surface) {
            let pair = (
                (c.temperature_c * 100.0).round() as i64,
                (c.moisture * 1000.0).round() as i64,
            );
            pairs.insert(pair);
            by_surface
                .entry((s.temperature_c * 100.0).round() as i64)
                .or_default()
                .insert(pair);
        }
        // The pre-change counterfactual: distinct (surface temperature, 0.90)
        // pairs === distinct surface temperatures.
        let control_pairs = distinct(&surface_t, 0.01);
        let informative_buckets = by_surface.values().filter(|set| set.len() > 1).count();

        let surface_fit: f64 = vertices.surface.iter().map(fit).sum::<f64>() / n as f64;
        let chamber_fit: f64 = vertices.chamber.iter().map(fit).sum::<f64>() / n as f64;
        let better_below = vertices
            .chamber
            .iter()
            .zip(&vertices.surface)
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
            "  distinct (temperature, moisture) pairs={} of {n}   CONTROL (pre-change reading)={control_pairs}",
            pairs.len()
        );
        println!(
            "  surface-temperature buckets carrying >1 distinct chamber reading={informative_buckets} of {} (pre-change: 0 by construction)",
            by_surface.len()
        );
        println!(
            "  drow niche fit  surface mean={surface_fit:.3}  chamber mean={chamber_fit:.3}  chamber>surface on {better_below} ({:.1}%)",
            100.0 * better_below as f64 / n as f64,
        );

        // THE CONTROLLED FORM OF THE HEADLINE. The first version of this
        // assertion was `pairs.len() > n / 2`, which the pre-change code would
        // very likely have passed too — the surface temperature alone already
        // separated most columns — so it pinned nothing this task did.
        //
        // What this task actually did is make the chamber reading carry
        // information the surface reading does not. Pre-change that was
        // impossible BY CONSTRUCTION, not by measurement: the chamber was
        // `(surface temperature, one constant)`, so every surface-temperature
        // bucket mapped to exactly one chamber reading and this count was
        // necessarily 0. Any positive value falsifies the old model.
        assert!(
            informative_buckets > 0,
            "seed {seed_value}: no surface-temperature bucket carries more than \
             one chamber reading, so the chamber reading is still a function of \
             the surface reading alone — which is the pre-Underworld model"
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
        // First: a chamber is warmer than the vertex above it, everywhere. A
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
        let mut by_reach: Vec<(f64, f64)> = vertices
            .hydrology
            .iter()
            .zip(&vertices.surface)
            .zip(&vertices.chamber)
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

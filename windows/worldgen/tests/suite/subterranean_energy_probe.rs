//! THE SOURCES, Task 5: does a field derived from ROCK reproduce
//! `domains/climate/src/underworld.rs`'s own frozen claim that underground
//! energy INVERTS with depth?
//!
//! `underworld.rs`'s module doc states the supply as detrital import near
//! the surface and chemolithotrophy off the geothermal gradient at depth,
//! and its `energy_is_not_monotone_in_depth` test asserts the shape: the
//! mid-ladder `Band::Deeps` trough sits strictly below both the shallow and
//! the deep halves of the ladder. Task 4 shipped seven energy sources with
//! nothing in any one of them shaped to produce a trough (see
//! `windows/worldgen/src/energy.rs`'s module doc, "nothing here was shaped
//! to produce a U"); this file sums them into a real per-rung field over
//! real worlds and asks whether the SAME shape falls out.
//!
//! **A falsification here is a FINDING, not a failure** (decision 0016).
//! `derived_energy_is_monotone_not_a_trough`'s doc comment records the
//! measured profile with the date it was measured, and its assertion is
//! whichever branch of `task-5-brief.md`'s decision table the measurement
//! actually landed on — never retuned after the fact to rescue a
//! prediction. **Named for what it measured, not the brief's sketch name**
//! (`derived_energy_troughs_in_the_ladders_middle`): the trough did not
//! survive contact with a real world, and a test named after a falsified
//! prediction is worse than an honest rename — see task-5-report.md for the
//! full account of this deviation from the brief.
//!
//! Test fixture (decision 0092): calls the composition-root entry points
//! directly, the sanctioned posture for this crate's live-worldgen
//! batteries. `world_at` mirrors `underworld_conditions_probe::
//! terrain_and_surface`'s own idiom (`Geosphere` borrows from
//! `GeneratedTerrain`, so the two cannot be packaged as an owned pair —
//! callers derive `terrain.geosphere()` themselves once both are in scope).
#![allow(clippy::disallowed_methods)]

use hornvale_astronomy::SkyPins;
use hornvale_kernel::{Band, Geosphere, Seed, VertexMap};
use hornvale_terrain::delve::rung_evaluation_depth_m;
use hornvale_terrain::{GeneratedTerrain, TerrainPins};
use hornvale_worldgen::energy::{
    EnergySource, dominant_source, subterranean_energy, subterranean_energy_field_per_rung,
};
use hornvale_worldgen::{
    BuildDepth, SettlementPins, SkyChoice, Substrate, WorldComponents,
    build_world_to_with_artifacts, climate_of, substrate_field, subterranean_substrate_at_rung,
    subterranean_substrate_field_per_rung,
};

/// Seeds this campaign preregisters on (spec §5) — the same three
/// `underworld_conditions_probe` and its siblings use, so every readout in
/// this campaign describes the same three worlds.
const SEEDS: [u64; 3] = [42, 7, 1234];

/// THE SOURCES, Task 6: the twelve seeds spec §6 preregisters for the
/// between-worlds `separation` statistic and Q2/S1's within-world `width`
/// — a different, larger set than [`SEEDS`] above (every other probe in
/// this file preregisters on the three-seed set). Reproduced in the spec's
/// own listed order, `S = {1, 7, 42, 99, 123, 256, 512, 777, 1024, 1234,
/// 4096, 9001}`.
const Q6_SEEDS: [u64; 12] = [1, 7, 42, 99, 123, 256, 512, 777, 1024, 1234, 4096, 9001];

/// The five underground rungs `Band::all()` carries below `Surface` — named
/// so Task 6's loops read as "every underground rung" rather than a magic
/// range, and so `Band::Surface`'s always-`None` slot is never iterated.
const UNDERGROUND_RUNGS: [Band; 5] = [
    Band::Undercroft,
    Band::Shallows,
    Band::Deeps,
    Band::Underdeep,
    Band::Nadir,
];

/// Build `seed` to `BuildDepth::Terrain` and return its terrain and surface
/// substrate field — the two inputs [`subterranean_energy_field_per_rung`]
/// needs. Mirrors `underworld_conditions_probe::terrain_and_surface`'s own
/// world-building idiom exactly.
fn world_at(seed_value: u64, wc: &WorldComponents) -> (GeneratedTerrain, VertexMap<Substrate>) {
    let seed = Seed(seed_value);
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

/// The one seed the positive-control guard below builds — named rather than
/// spelled inline so it is obviously ONE world, no sweep. Mirrors
/// `underworld_conditions_probe::LIVE_GUARD_SEED`'s own convention.
const LIVE_GUARD_SEED: u64 = 42;

/// claim: structural(seed: 42) — one world, one build, no sweep.
///
/// **THE POSITIVE CONTROL Task 5's fix round 1 needed.** A reviewer
/// reproduced this file's own reported uncaught mutation — zeroing
/// `depth_m` at `subterranean_energy_field_per_rung`'s call site — and
/// found it real and structural: `derived_energy_is_monotone_not_a_trough`
/// stays green (`[NaN, 0.159, 0.163, 0.167, 0.167, 0.167]`, still
/// non-decreasing) because `Substrate::moisture`'s own indirect
/// depth-dependence is enough to hold the shape up even with every
/// depth-direct source neutered. The gap that mutation exposed is GENERAL,
/// not specific to `depth_m`: nothing anywhere asserted that the field
/// function threads ANY of its five arguments correctly to the pure
/// function ([`subterranean_energy`]) it is supposed to be nothing but a
/// per-rung evaluation of.
///
/// So: for every cave-bearing vertex and every rung it reaches, this
/// independently recomputes each of `subterranean_energy`'s five arguments
/// via the SAME public accessors the field function itself uses
/// (`GeneratedTerrain::material_at`, `::geothermal_gradient_at`,
/// `::drainage_at`, [`rung_evaluation_depth_m`],
/// [`subterranean_substrate_at_rung`]'s `Substrate::moisture`) — written
/// out explicitly here, not by calling
/// [`subterranean_energy_field_per_rung`] a second time, so a
/// mis-threaded argument inside that function's own call site has no way
/// to be silently mirrored by this test's independent one — and asserts
/// the field's own entry at that vertex/rung is bit-for-bit
/// (`.to_bits()`) identical to calling [`subterranean_energy`] directly.
/// A mis-threaded `depth_m`, a swapped `moisture`/`drainage` pair, a wrong
/// `rung_evaluation_depth_m` call, or a vertex/rung index slip all diverge
/// this comparison — see `task-5-report.md`'s fix-round-1 addendum for the
/// two mutation controls (the reproduced `depth_m` zero and a second,
/// different mis-threading) that prove it.
#[test]
fn every_field_entry_reproduces_the_pure_function_at_its_own_rung() {
    let wc = WorldComponents::assemble().expect("canonical registries are well-formed");
    let (terrain, surface) = world_at(LIVE_GUARD_SEED, &wc);
    let geo = terrain.geosphere();
    let subterranean_per_rung = subterranean_substrate_field_per_rung(geo, &terrain, &surface);
    let field = subterranean_energy_field_per_rung(geo, &terrain, &subterranean_per_rung);
    let mut compared = 0usize;
    for vertex in geo.vertices() {
        let entry = field.get(vertex);
        let Some(cave) = terrain.cave_at(vertex) else {
            for slot in entry {
                assert!(
                    slot.is_none(),
                    "vertex {vertex:?} has no cave but the field entry is {slot:?}"
                );
            }
            continue;
        };
        let gradient = terrain.geothermal_gradient_at(vertex);
        let material = terrain.material_at(vertex);
        let drainage = terrain.drainage_at(vertex);
        let s = *surface.get(vertex);
        for &rung in Band::all() {
            let expected_depth = rung_evaluation_depth_m(rung, gradient, cave.depth_reach_m);
            let expected_sub = expected_depth
                .and_then(|_| subterranean_substrate_at_rung(s, rung, &terrain, vertex));
            let (Some(depth_m), Some(sub)) = (expected_depth, expected_sub) else {
                assert!(
                    entry[rung as usize].is_none(),
                    "vertex {vertex:?} rung {rung:?}: independent recomputation has no                      reading but the field entry is {:?}",
                    entry[rung as usize]
                );
                continue;
            };
            let expected =
                subterranean_energy(&material, gradient, depth_m, sub.moisture, drainage);
            let actual = entry[rung as usize].unwrap_or_else(|| {
                panic!(
                    "vertex {vertex:?} rung {rung:?}: field entry is None but the                      independent recomputation has a reading ({expected})"
                )
            });
            assert_eq!(
                actual.to_bits(),
                expected.to_bits(),
                "vertex {vertex:?} rung {rung:?}: field entry {actual} does not                  reproduce subterranean_energy({material:?}, {gradient:?}, {depth_m},                  {}, {drainage}) = {expected} called directly with the same five                  arguments — a mis-threaded argument or a vertex/rung index slip",
                sub.moisture
            );
            compared += 1;
        }
    }
    assert!(
        compared > 100,
        "only {compared} vertex-rung entries compared — vacuous"
    );
}

/// The median of a slice, sorted in place. `v.len() / 2` on an odd-length
/// slice is the exact middle; on an even-length one it is the upper of the
/// two central values — a convention, not a source of bias at the sample
/// sizes this file pools (tens of thousands of vertex-rung readings).
///
/// **Empty is `NaN`, not a panic.** `Band::Surface`'s slot
/// (`field.get(vertex)[Band::Surface as usize]`) is *always* `None` —
/// [`subterranean_energy_field_per_rung`]'s own doc states it, mirroring
/// [`subterranean_substrate_field_per_rung`]'s identical contract — so
/// `profile[Band::Surface as usize]` is always empty and `v[v.len() / 2]`
/// on it would be an out-of-bounds panic (found running this file, not in
/// the brief's sketch). Every read this file does of the resulting median
/// vector is at `Band::Undercroft`..=`Band::Nadir`, never `Surface`, so the
/// `NaN` is inert; it exists so the vector stays indexable by `Band as
/// usize` without a hole.
fn median(v: &mut [f64]) -> f64 {
    if v.is_empty() {
        return f64::NAN;
    }
    v.sort_by(f64::total_cmp);
    v[v.len() / 2]
}

/// THE SOURCES, Task 6: nearest-rank percentile of an ascending-sorted
/// slice — the same convention `ore_separation_probe.rs`'s `pct` uses, the
/// direct precedent this task's brief cites for `mineral_supply_field`'s
/// 0.0067-wide near-constant band. `q` is a fraction in `[0,1]` (`0.5` is
/// the median, `0.9` is p90). `NaN` on an empty slice, matching [`median`]
/// above rather than panicking.
fn pct(sorted: &[f64], q: f64) -> f64 {
    if sorted.is_empty() {
        return f64::NAN;
    }
    let i = (((sorted.len() - 1) as f64) * q).round() as usize;
    sorted[i]
}

/// `p75 - p25` of an ascending-sorted slice — the interquartile range both
/// Q1's `separation` formula (spec §6) and this file's sibling
/// `winze_energy_probe.rs` (its `IQR/range` diagnostic) use.
fn iqr(sorted: &[f64]) -> f64 {
    pct(sorted, 0.75) - pct(sorted, 0.25)
}

/// A moisture value at which every source's water gate is already
/// saturated to `1.0` — the largest saturate constant among the six
/// moisture-gated sources is `SULPHIDE_OXIDATION_MOISTURE_SATURATE` (0.4),
/// so any value at or above it saturates all six simultaneously. Used only
/// by the hold-moisture-constant diagnostic below, never by the
/// preregistered assertion, to isolate each source's OWN depth behaviour
/// from `Substrate::moisture`'s *indirect* depth-dependence via
/// `chamber_moisture` (the module doc's "nothing here was shaped to
/// produce a U" caveat).
const SATURATED_MOISTURE: f64 = 1.0;

/// [`subterranean_energy_field_per_rung`] with moisture pinned to
/// [`SATURATED_MOISTURE`] at every rung, rather than each rung's own
/// [`Substrate::moisture`] — the diagnostic counterfactual for "how much of
/// the derived field's depth-shape is `Substrate::moisture` doing the work,
/// versus the sources' own lithology/gradient/drainage terms?" Everything
/// else (material, gradient, drainage, which rungs a vertex has) is read
/// identically to the real field.
fn energy_field_with_fixed_moisture(
    geo: &Geosphere,
    terrain: &GeneratedTerrain,
    moisture: f64,
) -> VertexMap<[Option<f64>; 6]> {
    VertexMap::from_fn(geo, |vertex| {
        let mut out = [None; 6];
        let Some(cave) = terrain.cave_at(vertex) else {
            return out;
        };
        let gradient = terrain.geothermal_gradient_at(vertex);
        let material = terrain.material_at(vertex);
        let drainage = terrain.drainage_at(vertex);
        for &rung in Band::all() {
            let Some(depth_m) = rung_evaluation_depth_m(rung, gradient, cave.depth_reach_m) else {
                continue;
            };
            out[rung as usize] = Some(subterranean_energy(
                &material, gradient, depth_m, moisture, drainage,
            ));
        }
        out
    })
}

/// **Does the DERIVED field reproduce the AUTHORED corpus's central claim?**
///
/// `domains/climate/src/underworld.rs` authors energy for 22 communities and
/// its `energy_is_not_monotone_in_depth` asserts the trough sits at `Deeps`.
/// This asks the same question of a field derived from rock, with nothing
/// shaped to produce the answer.
///
/// **A falsification here is a FINDING, not a failure** (decision 0016), and
/// this run found one: **MONOTONE, not a trough** — the brief's third branch
/// row. Do not retune a Task 4 source to rescue the inversion; this is a
/// disagreement between the derived field and the authored corpus, recorded
/// as measured.
///
/// ## Measured 2026-08-26, seeds 42 / 7 / 1234, `BuildDepth::Terrain`
///
/// Per-rung `ENERGY` medians (`subterranean_energy`'s **mean** of the seven
/// sources — see that function's doc for why mean, not a clamped sum),
/// `Band::all()` order (`Surface` is always `None` and carries no median):
///
/// | `Undercroft` | `Shallows` | `Deeps` | `Underdeep` | `Nadir` |
/// |---|---|---|---|---|
/// | 0.168609 | 0.200822 | 0.265342 | 0.281421 | 0.281449 |
///
/// **Strictly non-decreasing with depth.** `Deeps` (0.265) sits *above*
/// both shallower rungs and *below* both deeper ones — the opposite of a
/// trough, which is exactly what the frozen corpus's own
/// `sump-gallery`/`sulphuric-hall` pair asserts should NOT happen
/// (`Deeps` should be the corpus minimum among wet rows). The deep end is
/// nearly flat (`Underdeep` 0.281421 vs `Nadir` 0.281449, a 2.6e-5
/// difference) because `Geothermal`'s saturating term and
/// `SulphideOxidation`'s ΔT front are both close to their own ceiling by
/// `Underdeep`.
///
/// **Hold-moisture-constant diagnostic** (moisture pinned to
/// [`SATURATED_MOISTURE`] at every rung instead of each rung's own
/// `Substrate::moisture`): 0.179553 / 0.207553 / 0.266491 / 0.282692 /
/// 0.282692 — the SAME monotone shape, each rung within ~0.001-0.011 of the
/// real-moisture reading. Most of this field's depth-shape is therefore the
/// sources' own direct depth/gradient/drainage dependence, not
/// `Substrate::moisture`'s indirect one via `chamber_moisture` — the
/// opposite of what would make this a moisture-field artifact.
///
/// See `.superpowers/sdd/2026-08-26-the-sources/task-5-report.md` for the
/// full readout (histogram, mutation control, red/green transcript).
///
/// claim: readout(off-gate, heavy:, prints the per-rung medians, asserts
/// only that the measured shape holds) — the derived ENERGY field over the
/// three preregistered seeds.
#[test]
#[ignore = "probe: whether derived subterranean energy is monotone rather than a trough; run by hand (The Sources, Task 5, answered its question; demoted by The Governor 2026-08-28)"]
fn derived_energy_is_monotone_not_a_trough() {
    let wc = WorldComponents::assemble().expect("canonical registries are well-formed");
    let mut profile: [Vec<f64>; 6] = Default::default();
    let mut fixed_profile: [Vec<f64>; 6] = Default::default();
    for &seed in &SEEDS {
        let (terrain, surface) = world_at(seed, &wc);
        let geo = terrain.geosphere();
        let subterranean_per_rung = subterranean_substrate_field_per_rung(geo, &terrain, &surface);
        let field = subterranean_energy_field_per_rung(geo, &terrain, &subterranean_per_rung);
        let fixed_field = energy_field_with_fixed_moisture(geo, &terrain, SATURATED_MOISTURE);
        for vertex in geo.vertices() {
            for (i, e) in field.get(vertex).iter().enumerate() {
                if let Some(e) = e {
                    profile[i].push(*e);
                }
            }
            for (i, e) in fixed_field.get(vertex).iter().enumerate() {
                if let Some(e) = e {
                    fixed_profile[i].push(*e);
                }
            }
        }
    }
    let m: Vec<f64> = profile.iter_mut().map(|v| median(v)).collect();

    println!("derived per-rung ENERGY medians (real moisture): {m:?}");

    // Diagnostic only — never asserted on. Holds moisture fixed at
    // SATURATED_MOISTURE so every water gate reads 1.0 at every rung,
    // isolating the sources' own lithology/gradient/drainage shape from
    // Substrate::moisture's indirect depth-dependence via chamber_moisture.
    let fixed_m: Vec<f64> = fixed_profile.iter_mut().map(|v| median(v)).collect();
    println!(
        "hold-moisture-constant per-rung ENERGY medians (moisture={SATURATED_MOISTURE}): {fixed_m:?}"
    );

    // PREREGISTERED PREDICTION FALSIFIED 2026-08-26 (spec §4.3, decision
    // 0016): the derived field does not trough at Deeps. Measured over the
    // frozen seed set it is instead MONOTONE, strictly non-decreasing with
    // depth (see this test's doc comment for the six-value profile and the
    // hold-moisture-constant diagnostic). The authored corpus
    // (`energy_is_not_monotone_in_depth`) asserts the opposite shape; a
    // derived field disagreeing with it is a finding about one of the two,
    // not a bug to silently patch over. This assertion now pins the
    // MEASURED shape rather than the originally hoped-for trough — if it
    // ever fails, the shape has moved again and needs a fresh measurement
    // recorded here, never a source retuned to force either shape back.
    let habitation = [
        m[Band::Undercroft as usize],
        m[Band::Shallows as usize],
        m[Band::Deeps as usize],
        m[Band::Underdeep as usize],
        m[Band::Nadir as usize],
    ];
    assert!(
        habitation.windows(2).all(|w| w[0] <= w[1] + 1e-9),
        "MEASURED SHAPE CHANGED from the 2026-08-26 reading recorded in this \
         test's doc comment: the derived field was monotonically \
         non-decreasing with depth (Undercroft..=Nadir: {habitation:?}) over \
         the frozen seed set, and is no longer. Record a fresh measurement \
         with today's date — do NOT retune a Task 4 source to force a \
         particular shape either way."
    );
}

/// If a single source dominates every chamber in every world, the other six
/// are decoration and `BIO-subterranean-energy-sources`'s claim that the
/// DIFFERENCES motivate ecology, trade, exploration and mining is not yet
/// true of the code, whatever the source functions say individually.
///
/// claim: readout(off-gate, heavy:, prints the dominant-source histogram,
/// asserts only that more than one source occupies it) — over the three
/// preregistered seeds.
#[test]
#[ignore = "probe: whether more than one energy source dominates somewhere; run by hand (The Sources, Task 5, answered its question; demoted by The Governor 2026-08-28)"]
fn more_than_one_source_dominates_somewhere() {
    // A FIXED-SIZE TALLY INDEXED BY `EnergySource::ALL` POSITION, not a
    // BTreeMap. `EnergySource` derives `Debug, Clone, Copy, PartialEq, Eq`
    // and deliberately NOT `Ord` (see the controller note in
    // `task-5-brief.md`): these seven are nominal categories, and an `Ord`
    // would assert a precedence among them that does not exist purely to
    // satisfy a container.
    let wc = WorldComponents::assemble().expect("canonical registries are well-formed");
    let mut histogram = [0usize; EnergySource::ALL.len()];
    let mut chambers = 0usize;
    for &seed in &SEEDS {
        let (terrain, surface) = world_at(seed, &wc);
        let geo = terrain.geosphere();
        let moisture = subterranean_substrate_field_per_rung(geo, &terrain, &surface);
        for vertex in geo.vertices() {
            let Some(cave) = terrain.cave_at(vertex) else {
                continue;
            };
            let m = terrain.material_at(vertex);
            let g = terrain.geothermal_gradient_at(vertex);
            let drainage = terrain.drainage_at(vertex);
            for &rung in Band::all() {
                let Some(depth) = rung_evaluation_depth_m(rung, g, cave.depth_reach_m) else {
                    continue;
                };
                let Some(sub) = moisture.get(vertex)[rung as usize] else {
                    continue;
                };
                let winner = dominant_source(&m, g, depth, sub.moisture, drainage);
                let slot = EnergySource::ALL
                    .iter()
                    .position(|s| *s == winner)
                    .expect("a dominant source is one of ALL");
                histogram[slot] += 1;
                chambers += 1;
            }
        }
    }
    assert!(
        chambers > 10_000,
        "only {chambers} chambers sampled — vacuous"
    );
    // Report BEFORE asserting: the histogram is the finding, the assertion
    // is only its floor.
    println!("dominant-source histogram over {chambers} chambers: {histogram:?}");
    println!("EnergySource::ALL order: {:?}", EnergySource::ALL);
    let occupied = histogram.iter().filter(|n| **n > 0).count();
    assert!(
        occupied > 1,
        "one source dominates every chamber in every world ({histogram:?} \
         over {:?}). The other six are decoration. Record this in the \
         chronicle as measured — do NOT retune a source to spread the \
         histogram.",
        EnergySource::ALL
    );
}

/// THE SOURCES, Task 6: does lithology-derived energy vary BETWEEN worlds
/// (Q1), and — Nathan's ruling of 2026-08-26 — does it vary enough WITHIN a
/// world, at a given depth, to be different KINDS of place (Q2)? Both
/// questions are in doubt for the same reason Task 1 (`ore_separation_probe.rs`)
/// already measured: mineral prospectivity was assumed to vary usefully
/// across the map and turned out to sit 75% of all land inside a band
/// 0.0067 wide. `mineral_supply_field` reads that very quantity, and until
/// this test nothing had asked whether [`subterranean_energy_field_per_rung`]
/// shares the collapse.
///
/// **Q1 is frozen in the spec (spec §6), reproduced here verbatim** (the
/// implementer is not to interpret it):
///
/// ```text
/// E_s        = per-rung subterranean energy over all cave-bearing vertices of seed s
/// m_s        = median(E_s)
/// S          = {1, 7, 42, 99, 123, 256, 512, 777, 1024, 1234, 4096, 9001}   (n = 12)
///
/// separation = IQR({ m_s : s in S }) / median({ IQR(E_s) : s in S })
///
/// PREDICTION: separation >= 0.25
/// ```
///
/// `E_s` pools every cave-bearing vertex's reading at every underground rung
/// for seed `s` into one sample — one scalar per seed either way (`m_s`,
/// `IQR(E_s)`), so `separation` is a single number over the twelve-seed set,
/// never a per-rung one. (Q2/S1 below is the per-rung question.)
///
/// **Q2/S1 is frozen 2026-08-26 (Nathan's ruling on Task 5's monotone
/// result), also verbatim:**
///
/// ```text
/// width(r) = median({ p90(E(r,s)) - p10(E(r,s)) : s in S })
///
/// PREDICTION: width(r) >= 0.25 at every underground rung
/// ```
///
/// `E(r,s)` is rung `r`'s energy values over seed `s`'s cave-bearing
/// vertices — per-rung, per-seed, unlike `E_s` above, which pools every
/// rung together. `0.25` is read off `domains/climate/src/underworld.rs`'s
/// own five-level `ENERGY` ruler (`E_INERT` 0.0 .. `E_TEEMING` 1.0, spaced
/// exactly 0.25 apart), not chosen: a p10-p90 width under one band means the
/// middle 80% of chambers at that depth all round to the same authored
/// level — one kind of place with rounding, not different kinds of place.
///
/// **S2 and S3 are diagnostics, not predictions.** S2 asks whether the
/// field is even on the corpus's own `[0,1]` ruler (the share of chambers
/// per rung falling in each of the corpus's five bands); S3 asks whether
/// more than one source ever dominates, per rung (Task 5's
/// `more_than_one_source_dominates_somewhere` asked the same question
/// pooled over all rungs at once — this repeats it per rung, since two
/// chambers at the same depth with the same energy but different dominant
/// sources are different kinds of place in a way the energy scalar alone
/// cannot see).
///
/// **Every number is reported before any verdict is drawn** — the brief's
/// own discipline, because S1's p10-p90 width discards both tails, and a
/// world of 95% identical chambers plus 5% extraordinary ones is exactly
/// the shape Nathan's ruling names ("dry, dusty hallways ... and lush,
/// richly carpeted fungal forests"). So p1/p10/p50/p90/p99/min/max are
/// printed per rung ahead of S1's assertion, not folded into it.
///
/// # Measured 2026-08-26, `Q6_SEEDS` (n=12), `BuildDepth::Terrain`
///
/// **BOTH PREDICTIONS FALSIFIED — the campaign's most valuable output from
/// this task, not a failure (decision 0016).** Neither assertion below was
/// retuned to rescue its prediction; both pin the measured null.
///
/// **Q1: `separation = 0.145249` (< 0.25).** Twelve per-seed medians
/// (`Q6_SEEDS` order — 1, 7, 42, 99, 123, 256, 512, 777, 1024, 1234, 4096,
/// 9001):
///
/// ```text
/// [0.202369, 0.225615, 0.212768, 0.244645, 0.224343, 0.250312,
///  0.201980, 0.176259, 0.230781, 0.225153, 0.235623, 0.230189]
/// ```
///
/// `IQR(m_s) = 0.018013`, `median(IQR(E_s)) = 0.124017`. The twelve worlds'
/// medians sit inside an 0.018-wide band while a single world's own
/// chamber-to-chamber spread (its `IQR(E_s)`) is ~6.9x that band. The
/// prospectivity-style collapse Task 1 measured (`ore_separation_probe.rs`,
/// 75% of land inside a 0.0067-wide band) is not confined to prospectivity:
/// world-to-world variation in lithology-derived energy is real but small
/// relative to the variation already present inside any one world — twelve
/// worlds do not separate from each other as distinct "kinds of world" by
/// this statistic.
///
/// **Q2/S1: every rung's width sits below 0.25 — no rung clears one full
/// `ENERGY` band.**
///
/// | `Undercroft` | `Shallows` | `Deeps` | `Underdeep` | `Nadir` |
/// |---|---|---|---|---|
/// | 0.112298 | 0.092286 | 0.146620 | 0.191982 | 0.198093 |
///
/// Widths are non-monotone (`Shallows` is the narrowest rung, not
/// `Undercroft`) but every one falls short of the 0.25 bar, `Nadir`
/// (deepest, widest) coming closest at 79% of the threshold. p1/p10/p50/
/// p90/p99/min/max per rung (pooled over all twelve seeds' cave-bearing
/// vertices, n=16427 at every rung):
///
/// | rung | p1 | p10 | p50 | p90 | p99 | min | max |
/// |---|---|---|---|---|---|---|---|
/// | `Undercroft` | 0.101725 | 0.116021 | 0.162966 | 0.227720 | 0.282050 | 0.060765 | 0.331381 |
/// | `Shallows` | 0.108481 | 0.154032 | 0.195551 | 0.248999 | 0.284363 | 0.096622 | 0.317492 |
/// | `Deeps` | 0.108783 | 0.160484 | 0.264144 | 0.315674 | 0.353714 | 0.096622 | 0.382683 |
/// | `Underdeep` | 0.108783 | 0.160484 | 0.277751 | 0.361448 | 0.402222 | 0.096622 | 0.420840 |
/// | `Nadir` | 0.108783 | 0.160484 | 0.277751 | 0.366804 | 0.407140 | 0.096622 | 0.424277 |
///
/// **The blind zone the brief named does not rescue the prediction**: even
/// p1..p99 (98% of chambers, not just the p10-p90 middle) stays inside a
/// band well under 0.4 wide at every rung, and the tails move with the
/// bulk rather than sitting far outside it. This is not "5% extraordinary
/// chambers hiding inside a narrow p10-p90" — the whole distribution is
/// narrow.
///
/// **Every realized max stays inside `[0,1]`** — the live question this
/// task was asked to settle. The largest reading anywhere in the sweep is
/// `Nadir`'s `0.424277`, well short of `1.0`. `Serpentinization` and
/// `Methanogenesis`'s unclamped `moisture` multiply (see `energy.rs`'s
/// module doc) never realizes a value the mean-of-seven combination
/// (`subterranean_energy`) cannot absorb, at least over this twelve-seed
/// sweep — a clean answer, recorded as measured rather than assumed safe.
///
/// **S2 confirms Task 5's "upper bands may be structurally unreachable"
/// note** — corpus-band occupancy per rung, pooled over all twelve seeds
/// (n=16427/rung):
///
/// | rung | inert | lean | fed | rich | teeming |
/// |---|---|---|---|---|---|
/// | `Undercroft` | 19.08% | 80.92% | 0.00% | 0.00% | 0.00% |
/// | `Shallows` | 2.01% | 97.99% | 0.00% | 0.00% | 0.00% |
/// | `Deeps` | 1.96% | 97.99% | 0.05% | 0.00% | 0.00% |
/// | `Underdeep` | 1.96% | 95.32% | 2.72% | 0.00% | 0.00% |
/// | `Nadir` | 1.96% | 92.36% | 5.68% | 0.00% | 0.00% |
///
/// `rich` and `teeming` — the corpus's top two of five levels — are
/// **never realized at any rung, in any of the twelve worlds.** `fed`
/// barely opens even at `Nadir` (5.68%). The field occupies only the
/// bottom third of the `[0,1]` ruler the corpus's five levels are spread
/// across: a calibration finding distinct from Q1/S1's variation finding,
/// per the brief's own instruction, and not corrected here (no source
/// retuned).
///
/// **S3: more than one source dominates at every rung** (histograms in the
/// test body's `println!`s; `SulphideOxidation` and `Geothermal` are the
/// two sources absent at `Undercroft` specifically, both being
/// depth/gradient-gated).
///
/// claim: readout(off-gate, heavy:, prints every number the brief requires
/// before drawing any verdict, then pins BOTH measured falsifications) —
/// over the frozen twelve-seed set, `BuildDepth::Terrain`.
#[test]
#[ignore = "probe: between-worlds separation vs. within-world width of the energy mix; run by hand (The Sources, Task 6, answered its question; demoted by The Governor 2026-08-28)"]
fn between_worlds_separation_and_within_world_width() {
    let wc = WorldComponents::assemble().expect("canonical registries are well-formed");

    // Q1's per-seed pooled sample (every underground rung together).
    let mut per_seed_pooled: Vec<Vec<f64>> = Vec::with_capacity(Q6_SEEDS.len());
    // Q2/S1's per-rung, per-seed samples. Indexed by `Band as usize`;
    // index 0 (`Surface`) is never populated.
    let mut per_rung_per_seed: [Vec<Vec<f64>>; 6] = [
        Vec::new(),
        Vec::new(),
        Vec::new(),
        Vec::new(),
        Vec::new(),
        Vec::new(),
    ];
    // Per-rung, pooled across all twelve seeds — the tails report and S2's
    // band occupancy.
    let mut per_rung_pooled: [Vec<f64>; 6] = [
        Vec::new(),
        Vec::new(),
        Vec::new(),
        Vec::new(),
        Vec::new(),
        Vec::new(),
    ];
    // S3: dominant-source histogram, per rung, pooled over all twelve seeds.
    let mut dominant_histogram: [[usize; 7]; 6] = [[0usize; 7]; 6];
    let mut chambers_sampled = 0usize;

    for &seed_value in &Q6_SEEDS {
        let (terrain, surface) = world_at(seed_value, &wc);
        let geo = terrain.geosphere();
        let moisture_field = subterranean_substrate_field_per_rung(geo, &terrain, &surface);
        let field = subterranean_energy_field_per_rung(geo, &terrain, &moisture_field);

        let mut seed_pooled: Vec<f64> = Vec::new();
        let mut seed_by_rung: [Vec<f64>; 6] = [
            Vec::new(),
            Vec::new(),
            Vec::new(),
            Vec::new(),
            Vec::new(),
            Vec::new(),
        ];

        for vertex in geo.vertices() {
            let Some(cave) = terrain.cave_at(vertex) else {
                continue;
            };
            let entry = field.get(vertex);
            let material = terrain.material_at(vertex);
            let gradient = terrain.geothermal_gradient_at(vertex);
            let drainage = terrain.drainage_at(vertex);
            for &rung in &UNDERGROUND_RUNGS {
                let idx = rung as usize;
                let Some(e) = entry[idx] else {
                    continue;
                };
                seed_pooled.push(e);
                seed_by_rung[idx].push(e);

                let Some(depth_m) = rung_evaluation_depth_m(rung, gradient, cave.depth_reach_m)
                else {
                    continue;
                };
                let Some(sub) = moisture_field.get(vertex)[idx] else {
                    continue;
                };
                let winner = dominant_source(&material, gradient, depth_m, sub.moisture, drainage);
                let slot = EnergySource::ALL
                    .iter()
                    .position(|s| *s == winner)
                    .expect("a dominant source is one of ALL");
                dominant_histogram[idx][slot] += 1;
                chambers_sampled += 1;
            }
        }

        for idx in 0..6 {
            per_rung_pooled[idx].extend_from_slice(&seed_by_rung[idx]);
            per_rung_per_seed[idx].push(std::mem::take(&mut seed_by_rung[idx]));
        }
        per_seed_pooled.push(seed_pooled);
    }

    assert!(
        chambers_sampled > 10_000,
        "only {chambers_sampled} chambers sampled across {} seeds — vacuous",
        Q6_SEEDS.len()
    );

    // ---- Q1: between-worlds separation --------------------------------
    let mut m_values: Vec<f64> = Vec::with_capacity(Q6_SEEDS.len());
    let mut iqr_values: Vec<f64> = Vec::with_capacity(Q6_SEEDS.len());
    for (&seed_value, mut v) in Q6_SEEDS.iter().zip(per_seed_pooled) {
        v.sort_by(f64::total_cmp);
        let m = pct(&v, 0.5);
        let q = iqr(&v);
        println!(
            "Q1 seed {seed_value}: m_s={m:.6} IQR(E_s)={q:.6} n={}",
            v.len()
        );
        m_values.push(m);
        iqr_values.push(q);
    }
    println!("Q1 twelve per-seed medians m_s (Q6_SEEDS order): {m_values:?}");
    let mut m_sorted = m_values.clone();
    m_sorted.sort_by(f64::total_cmp);
    let mut iqr_sorted = iqr_values.clone();
    iqr_sorted.sort_by(f64::total_cmp);
    let separation = iqr(&m_sorted) / pct(&iqr_sorted, 0.5);
    println!(
        "Q1 separation = IQR(m_s)/median(IQR(E_s)) = {:.6}/{:.6} = {separation:.6}",
        iqr(&m_sorted),
        pct(&iqr_sorted, 0.5)
    );

    // ---- Q2/S1: within-world width, per rung, with tails reported first ----
    let mut widths: Vec<(Band, f64)> = Vec::with_capacity(UNDERGROUND_RUNGS.len());
    let mut realized_max_over_1: Vec<(Band, f64)> = Vec::new();
    for &rung in &UNDERGROUND_RUNGS {
        let idx = rung as usize;
        let mut pooled = per_rung_pooled[idx].clone();
        pooled.sort_by(f64::total_cmp);
        let p1 = pct(&pooled, 0.01);
        let p10 = pct(&pooled, 0.10);
        let p50 = pct(&pooled, 0.50);
        let p90 = pct(&pooled, 0.90);
        let p99 = pct(&pooled, 0.99);
        let min = pooled.first().copied().unwrap_or(f64::NAN);
        let max = pooled.last().copied().unwrap_or(f64::NAN);
        println!(
            "{rung:?}: pooled n={} p1={p1:.6} p10={p10:.6} p50={p50:.6} p90={p90:.6} p99={p99:.6} min={min:.6} max={max:.6}",
            pooled.len()
        );
        if max > 1.0 {
            realized_max_over_1.push((rung, max));
        }

        let mut per_seed_widths: Vec<f64> = Vec::with_capacity(Q6_SEEDS.len());
        for seed_vals in &per_rung_per_seed[idx] {
            let mut sv = seed_vals.clone();
            sv.sort_by(f64::total_cmp);
            per_seed_widths.push(pct(&sv, 0.90) - pct(&sv, 0.10));
        }
        let mut sorted_widths = per_seed_widths.clone();
        sorted_widths.sort_by(f64::total_cmp);
        let width = pct(&sorted_widths, 0.5);
        println!(
            "{rung:?}: S1 width = median(p90-p10 per seed) = {width:.6} (per-seed p90-p10: {per_seed_widths:?})"
        );
        widths.push((rung, width));
    }

    if realized_max_over_1.is_empty() {
        println!("S2/max check: every rung's realized max stayed within [0,1].");
    } else {
        println!(
            "S2/max check: realized max EXCEEDED 1.0 at {realized_max_over_1:?} — \
             Serpentinization/Methanogenesis's unclamped moisture multiply is live, not latent."
        );
    }

    // ---- S2: corpus-band occupancy, per rung (diagnostic) --------------
    for &rung in &UNDERGROUND_RUNGS {
        let idx = rung as usize;
        let vals = &per_rung_pooled[idx];
        let mut counts = [0usize; 5];
        for &e in vals {
            let band = if e < 0.125 {
                0
            } else if e < 0.375 {
                1
            } else if e < 0.625 {
                2
            } else if e < 0.875 {
                3
            } else {
                4
            };
            counts[band] += 1;
        }
        let n = vals.len().max(1) as f64;
        println!(
            "{rung:?}: S2 band occupancy inert={:.2}% lean={:.2}% fed={:.2}% rich={:.2}% teeming={:.2}% (n={})",
            100.0 * counts[0] as f64 / n,
            100.0 * counts[1] as f64 / n,
            100.0 * counts[2] as f64 / n,
            100.0 * counts[3] as f64 / n,
            100.0 * counts[4] as f64 / n,
            vals.len()
        );
    }

    // ---- S3: dominant-source histogram, per rung (diagnostic) ----------
    for &rung in &UNDERGROUND_RUNGS {
        let idx = rung as usize;
        println!(
            "{rung:?}: S3 dominant-source histogram {:?} (ALL order {:?})",
            dominant_histogram[idx],
            EnergySource::ALL
        );
    }

    // ---- Verdicts ----------------------------------------------------------
    //
    // PREREGISTERED PREDICTION FALSIFIED 2026-08-26 (spec §6, decision 0016):
    // separation measured 0.145249, below the 0.25 bar. Twelve worlds' own
    // per-rung-pooled medians vary far less than the chamber-to-chamber
    // spread already present inside any single one of them — the same shape
    // of collapse Task 1 found in mineral prospectivity. This assertion
    // pins the MEASURED finding, not the original prediction: if it ever
    // crosses 0.25, that is drift or a genuine change worth a fresh
    // measurement recorded here with today's date, never a threshold to
    // force back down.
    assert!(
        separation < 0.25,
        "MEASURED FINDING CHANGED from the 2026-08-26 reading recorded in \
         this test's doc comment: Q1's separation was {separation:.6} (< \
         0.25, falsifying spec §6's between-worlds prediction) and has \
         since crossed 0.25. Record a fresh measurement with today's date \
         — do NOT retune anything to force a particular verdict either way. \
         Twelve per-seed medians: {m_values:?}"
    );

    // PREREGISTERED PREDICTION FALSIFIED 2026-08-26 (Nathan's ruling,
    // decision 0016): every underground rung's S1 width measured below
    // 0.25 (one full ENERGY band) — see this test's doc comment for the
    // full per-rung table (widths, and the p1/p10/p50/p90/p99/min/max
    // tails that show the shortfall is not merely a narrow p10-p90 hiding
    // a wide-tailed distribution). This assertion pins the MEASURED
    // finding, not the original prediction: a later run crossing 0.25 at
    // any rung is drift or a genuine change worth a fresh measurement
    // recorded here, never a bar to retune past.
    assert!(
        widths.iter().all(|(_, w)| *w < 0.25),
        "MEASURED FINDING CHANGED from the 2026-08-26 reading recorded in \
         this test's doc comment: every underground rung's S1 width was \
         measured below 0.25 and at least one rung has since crossed it — \
         {widths:?}. Record a fresh measurement with today's date — do NOT \
         retune anything to force a particular verdict either way."
    );
}

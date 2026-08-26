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
    let field = subterranean_energy_field_per_rung(geo, &terrain, &surface);
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
#[ignore = "heavy: live-worldgen battery; deferred from the commit gate to the heavy set (decision 0132)"]
fn derived_energy_is_monotone_not_a_trough() {
    let wc = WorldComponents::assemble().expect("canonical registries are well-formed");
    let mut profile: [Vec<f64>; 6] = Default::default();
    let mut fixed_profile: [Vec<f64>; 6] = Default::default();
    for &seed in &SEEDS {
        let (terrain, surface) = world_at(seed, &wc);
        let geo = terrain.geosphere();
        let field = subterranean_energy_field_per_rung(geo, &terrain, &surface);
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
#[ignore = "heavy: live-worldgen battery; deferred from the commit gate to the heavy set (decision 0132)"]
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

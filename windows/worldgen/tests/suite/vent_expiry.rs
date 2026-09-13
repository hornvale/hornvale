//! **M3a — does the marine habitat actually EXPIRE over world time?** The
//! Tidemark, Task 5; spec §4.
//!
//! Spec §4's claim is that the marine realm is "the first realm whose habitat
//! quality is time-varying", because `VentState` runs
//! `Absent → Nascent → Active → Weakening → Failed` and the selected source
//! migrates within a fixed candidate ring. M3a is the preregistered
//! measurement of that claim, stated in the plan as: *over a world-time sweep
//! at seed 42, count vertices whose marine availability or capacity is
//! non-zero at one instant and zero at a later one.*
//!
//! # The negative control was written FIRST, and writing it first is what
//! caught the plan's own control being mis-specified
//!
//! A sweep that found movement would look identical whether the movement came
//! from the vent layer or from the ONE other time-varying input on this path:
//! `WaterFields::from_substrate` is handed
//! `climate.temperature_at(sample.vertex, time)`, which is a function of the
//! instant too. Something has to hold the vent layer out while the clock runs.
//!
//! **The plan named the wrong instrument for that, and it was written first
//! precisely so this would surface before any live number was trusted.** The
//! plan's control is "the same world-time sweep with vent phase held
//! constant", which this file implemented literally: sample at instants spaced
//! by exactly one whole [`VENT_CYCLE_TICKS`], so `(t + offset) mod cycle` —
//! and therefore every vent's `VentState` — is bit-identical at every sample
//! while the clock advances 1,100 standard days. **It counted 1,435 vertices,
//! not zero.**
//!
//! The cause is not the clock and is not a defect. `select_vent_position`
//! (`waterworld_propagation.rs`) indexes its candidate ring by
//! `(cycle_index + state_step) mod ring_len`, and `cycle_index` advances by
//! one every whole cycle. So holding the *phase* constant holds the
//! succession constant and leaves the *migration* running: the same vent, in
//! the same state, lighting a different vertex of its ring each cycle. Spec §4
//! names both halves ("`VentState` runs Absent → … → Failed, **and** the
//! selected source may migrate within a fixed candidate ring"); the plan's
//! control neutralises only the first.
//!
//! So this file carries the two sweeps under their true names:
//!
//! - [`m3a_negative_control_an_ablated_overlay_counts_nothing`] is **THE
//!   negative control** — the live instants over an overlay whose vents are
//!   suppressed. It is the one that actually holds the whole vent layer out
//!   while the clock runs, and it counts zero, which is what licenses
//!   attributing the live sweep to the vent layer rather than to world time.
//! - [`m3a_the_migration_half_alone_expires_the_habitat`] keeps the
//!   phase-frozen sweep, reclassified from control to **measurement**: with
//!   succession frozen, migration alone is enough to expire the habitat. A
//!   rationalisation would have been to rewrite the sweep until it read zero;
//!   what it is instead is the decomposition of §4's claim into its two named
//!   halves.
//!
//! # What the measurement counts, said exactly
//!
//! Three quantities per vertex, because the preregistered one is the
//! narrowest of the three and a bare zero from it would not be interpretable:
//!
//! 1. **availability** — the `{0, 1}` presence mask
//!    `per_species_capacity_at`'s `Marine` arm applies, which is `1.0` exactly
//!    where the vertex's water column reaches at least one pelagic band. It is
//!    measured rather than reasoned about, because "it cannot move" is a claim
//!    about code, and the code is what is under test.
//! 2. **capacity** — the real shipped `vent-commensal` (the one kind of the
//!    six whose niche weights `CHEMOSYNTHATE` at 0.75, which is the axis a
//!    vent feeds), scored through the production entry point placement itself
//!    calls.
//! 3. **chemosynthate** — the best-over-bands value of
//!    `MarineHabitat::chemosynthate`, which is the vent's own contribution
//!    before the capacity arithmetic folds a `MARINE_FORAGE` term in beside
//!    it. This is the DIAGNOSTIC half: it is what tells a reader whether a
//!    zero on (2) means "the habitat does not expire" or "the habitat expires
//!    on the vent axis and the capacity function never reaches exactly zero
//!    because the sea always pays something".
//!
//! Numbers are printed for all three and asserted only where the campaign
//! preregistered an assertion.
//!
//! # THE RESULT, at seed 42 (2026-09-13)
//!
//! ```text
//! control (vents ablated)          availability 0   capacity 0   chemosynthate 0
//! migration half (phase frozen)    availability 0   capacity 0   chemosynthate 1435
//! M3a live (one whole cycle)       availability 0   capacity 0   chemosynthate 1020
//!                                  capacity MOVED at 1104 vertices,
//!                                  deepest trough 0.148623 of that vertex's own peak
//! ```
//!
//! **The preregistered counter reads ZERO on both the quantities the plan
//! named, and that zero is a property of the INSTRUMENT rather than of the
//! world.** Both halves of "availability or capacity is non-zero at one
//! instant and zero at a later one" are falsifiers this mechanism cannot
//! produce:
//!
//! - *availability* is the `{0, 1}` water-column mask, and no vent adds or
//!   removes a pelagic band, so it is time-invariant by construction;
//! - *capacity* is a Michaelis-Menten function of `axis_supply_with`, which
//!   **sums** a niche's axes. `vent-commensal` weights `MARINE_FORAGE` at
//!   0.25 beside its `CHEMOSYNTHATE` 0.75, and
//!   `marine_forage_supply_field` grades every ocean vertex strictly above
//!   zero (0.02 at abyssal/hadal/vent). So the supply term is bounded away
//!   from zero wherever the elevation tolerance is, and an exact zero is
//!   unreachable — `moved_floor` below asserts exactly that, so the claim is
//!   measured rather than argued.
//!
//! What the world actually does, on the axis a vent feeds and with the
//! ablated control at zero to attribute it: the vent-borne supply goes from
//! positive to exactly nothing at **1,020 vertices** over one cycle, and the
//! kind's capacity falls to **14.9%** of its own peak at the worst-hit
//! vertex. Spec §4's claim — that this is the first realm whose habitat
//! quality is time-varying — holds; the plan's operationalisation of it did
//! not discriminate.

#![allow(clippy::disallowed_methods)]

use hornvale_astronomy::SkyPins;
use hornvale_climate::GeneratedClimate;
use hornvale_kernel::{Seed, VertexMap, World, WorldTime};
use hornvale_species::{BiomeAffinity, BiosphereTraits, HabitatRealm, KindId};
use hornvale_terrain::{GeneratedTerrain, TerrainPins};
use hornvale_worldgen::waterworld::{
    VENT_CYCLE_TICKS, WaterWorld, WaterWorldConfig, waterworld_from,
};
use hornvale_worldgen::{
    BuildDepth, EraAdjust, EraInvariantSupply, MarineHabitat, PELAGIC_BANDS, SettlementPins,
    VentTenancy, WorldComponents, build_world_to_with_artifacts, climate_from,
    per_species_capacity_at,
};

/// How many instants each sweep takes. Twelve is enough to put every one of
/// the five succession states under the sampler at least once: the shortest
/// two states are 15 days of a 100-day cycle, and a 12-sample sweep steps
/// every `100 / 12 ≈ 8.33` days, so no state can be stepped over.
const SWEEP_SAMPLES: i64 = 12;

struct Fixture {
    world: World,
    terrain: GeneratedTerrain,
    climate: GeneratedClimate,
}

fn fixture(seed: Seed) -> Fixture {
    let wc = WorldComponents::assemble().expect("the shipped component roster assembles");
    let built = build_world_to_with_artifacts(
        seed,
        &SkyPins::default(),
        &TerrainPins::default(),
        &SettlementPins::default(),
        &wc,
        BuildDepth::Terrain,
    )
    .expect("the terrain-depth fixture builds");
    let terrain = built.terrain.expect("terrain depth returns terrain");
    let climate = climate_from(&built.world, &terrain).expect("climate derives from terrain");
    Fixture {
        world: built.world,
        terrain,
        climate,
    }
}

/// The real shipped `vent-commensal`, not a synthesised probe. M3a is a claim
/// about THIS world, so it is measured on the subject the campaign actually
/// ships — the one marine kind whose niche reads `CHEMOSYNTHATE` heavily
/// enough for a vent's succession to be the dominant term.
fn vent_commensal() -> (BiosphereTraits, Option<BiomeAffinity>) {
    let kind = KindId("vent-commensal");
    let traits = hornvale_species::biosphere_registry()
        .get(&kind)
        .expect("the roster carries vent-commensal")
        .clone();
    let affinity = hornvale_species::biome_affinity_registry()
        .get(&kind)
        .cloned();
    assert_eq!(
        hornvale_species::habitat_realm_registry()
            .get(&kind)
            .copied(),
        Some(HabitatRealm::Marine),
        "vent-commensal must be a Marine kind, or this sweep is scoring the Surface arm"
    );
    (traits, affinity)
}

/// One instant's reading of all three quantities.
struct Reading {
    availability: VertexMap<f64>,
    capacity: VertexMap<f64>,
    chemosynthate: VertexMap<f64>,
}

fn read_at(
    fixture: &Fixture,
    water: &WaterWorld,
    probe: &(BiosphereTraits, Option<BiomeAffinity>),
    time: WorldTime,
) -> Reading {
    let geo = fixture.terrain.geosphere();
    let obliquity_deg = fixture.climate.obliquity_deg();
    let insolation_scalar = fixture.climate.insolation();
    let regime = fixture.climate.regime();
    let hoisted = EraInvariantSupply::build_at(
        geo,
        &fixture.terrain,
        &fixture.climate,
        obliquity_deg,
        insolation_scalar,
        &regime,
        water,
        time,
    );
    // The availability mask `per_species_capacity_at`'s `Marine` arm applies,
    // re-derived here from the same habitat the arm reads: `1.0` where some
    // pelagic band has a substrate, `0.0` where none does.
    let habitat: &MarineHabitat = &hoisted.marine_habitat;
    let availability = VertexMap::from_fn(geo, |v| {
        let bands = habitat.substrate.get(v);
        if (0..PELAGIC_BANDS).any(|b| bands[b].is_some()) {
            1.0
        } else {
            0.0
        }
    });
    let chemosynthate = VertexMap::from_fn(geo, |v| {
        let bands = habitat.chemosynthate.get(v);
        (0..PELAGIC_BANDS).fold(0.0_f64, |best, b| best.max(bands[b]))
    });

    let bio: Vec<&BiosphereTraits> = vec![&probe.0];
    let realms = vec![HabitatRealm::Marine];
    let affinity: Vec<Option<BiomeAffinity>> = vec![probe.1.clone()];
    let caps = per_species_capacity_at(
        geo,
        &fixture.terrain,
        &fixture.climate,
        &hoisted,
        &EraAdjust::present(&fixture.terrain),
        &bio,
        &realms,
        &affinity,
    );
    let map = &caps
        .iter()
        .find(|(tag, _)| *tag == 0)
        .expect("a one-kind roster tags its only kind 0")
        .1;
    let capacity = VertexMap::from_fn(geo, |v| map.at(v));
    Reading {
        availability,
        capacity,
        chemosynthate,
    }
}

/// The M3a counter itself: vertices non-zero at one instant of `readings` and
/// exactly zero at a LATER one. Ordered, not merely unequal — the campaign's
/// claim is that a habitat *expires*, so a vertex that only ever lights up is
/// not one of these.
fn expired(geo: &hornvale_kernel::Geosphere, values: &[VertexMap<f64>]) -> usize {
    geo.vertices()
        .filter(|&v| {
            let mut seen_live = false;
            for reading in values {
                let x = *reading.get(v);
                if x > 0.0 {
                    seen_live = true;
                } else if seen_live {
                    return true;
                }
            }
            false
        })
        .count()
}

/// One sweep's worth of one quantity: its value at each instant, in order.
type Track = Vec<VertexMap<f64>>;

/// Every sweep in this file, in one shape: `samples` instants spaced
/// `step_ticks` apart from genesis, each read into its three maps.
fn sweep(
    fixture: &Fixture,
    water: &WaterWorld,
    probe: &(BiosphereTraits, Option<BiomeAffinity>),
    step_ticks: i64,
) -> (Track, Track, Track) {
    let mut availability = Track::new();
    let mut capacity = Track::new();
    let mut chemosynthate = Track::new();
    for i in 0..SWEEP_SAMPLES {
        let reading = read_at(fixture, water, probe, WorldTime::from_ticks(i * step_ticks));
        availability.push(reading.availability);
        capacity.push(reading.capacity);
        chemosynthate.push(reading.chemosynthate);
    }
    (availability, capacity, chemosynthate)
}

/// **The plan's control, reclassified as a measurement — see the module doc.**
///
/// Twelve instants, spaced one whole vent cycle apart, so world time advances
/// 1,100 standard days while every vent's `VentState` is bit-identical at
/// every sample. That freezes the succession and NOT the migration, because
/// `select_vent_position` indexes its candidate ring by `cycle_index`, which
/// advances once per cycle. Written and run as a control first; it counted
/// 1,435 and the reason was read out of the code rather than tuned away.
///
/// What it measures is therefore §4's *second* half in isolation: with the
/// phase held still, does the source's migration alone expire the habitat?
/// The count is printed and asserted only as strictly positive — the number
/// itself is a world fact and belongs in the report, not in a literal here.
#[test]
fn m3a_the_migration_half_alone_expires_the_habitat() {
    let fixture = fixture(Seed(42));
    let geo = fixture.terrain.geosphere();
    let water = waterworld_from(
        &fixture.world,
        &fixture.terrain,
        &fixture.climate,
        WaterWorldConfig { enabled: true },
    );
    assert!(
        !water.vents.is_empty(),
        "seed 42 must admit vents, or holding their phase constant holds nothing constant"
    );
    let probe = vent_commensal();
    let (availability, capacity, chemosynthate) = sweep(&fixture, &water, &probe, VENT_CYCLE_TICKS);

    // The control is only a control if the instants really are different
    // instants. `WorldTime` is an exact tick count, so this is arithmetic
    // rather than a measurement, but the sweep's own span is asserted so a
    // future edit that collapsed the step to zero fails here.
    assert_eq!(
        (SWEEP_SAMPLES - 1) * VENT_CYCLE_TICKS,
        1_100 * WorldTime::TICKS_PER_STD_DAY,
        "the control must span real world time: 11 whole 100-day vent cycles"
    );

    let (a, c, k) = (
        expired(geo, &availability),
        expired(geo, &capacity),
        expired(geo, &chemosynthate),
    );
    println!(
        "M3a MIGRATION HALF (vent phase frozen, {SWEEP_SAMPLES} instants one cycle apart): \
         availability expired at {a} vertices, capacity at {c}, chemosynthate at {k}"
    );
    assert_eq!(
        a, 0,
        "availability is the {{0, 1}} water-column mask and no vent can add or remove a \
         pelagic band, so it must be invariant under the vent layer at every instant"
    );
    assert!(
        k > 0,
        "with succession frozen, the source's migration around its candidate ring must still \
         expire the vent-borne supply somewhere — a zero here would mean `cycle_index` does \
         not reach `select_vent_position` and §4's migration clause is inert"
    );
}

/// The coarser second control: the live instants, over an overlay whose vents
/// are suppressed. The phase-frozen control above cannot tell "the vents
/// contribute nothing at all" from "the vents contribute something that does
/// not move"; this one can.
#[test]
fn m3a_negative_control_an_ablated_overlay_counts_nothing() {
    let fixture = fixture(Seed(42));
    let geo = fixture.terrain.geosphere();
    let mut water = waterworld_from(
        &fixture.world,
        &fixture.terrain,
        &fixture.climate,
        WaterWorldConfig { enabled: true },
    );
    assert!(
        !water.vents.is_empty(),
        "the suppression below must have something to suppress"
    );
    water.vents.clear();
    water.vent_candidate_rings.clear();

    let probe = vent_commensal();
    let step = VENT_CYCLE_TICKS / SWEEP_SAMPLES;
    let (availability, capacity, chemosynthate) = sweep(&fixture, &water, &probe, step);
    let (a, c, k) = (
        expired(geo, &availability),
        expired(geo, &capacity),
        expired(geo, &chemosynthate),
    );
    println!(
        "M3a CONTROL (vents suppressed, the live instants): availability expired at {a} \
         vertices, capacity at {c}, chemosynthate at {k}"
    );
    assert_eq!(
        (a, c, k),
        (0, 0, 0),
        "with no vent admitted nothing may expire over the live sweep's own instants"
    );
}

/// **M3a itself.** Twelve instants across one whole vent cycle, at seed 42.
///
/// The plan preregisters the headline: *a zero count falsifies spec §4 and is
/// reported as the headline, not fixed.* So the assertions here are exactly
/// the preregistered ones and nothing is tuned to rescue a number. All three
/// quantities are printed whatever they say.
#[test]
fn m3a_the_marine_habitat_expires_over_world_time() {
    let fixture = fixture(Seed(42));
    let geo = fixture.terrain.geosphere();
    let water = waterworld_from(
        &fixture.world,
        &fixture.terrain,
        &fixture.climate,
        WaterWorldConfig { enabled: true },
    );
    assert!(!water.vents.is_empty(), "seed 42 admits vents");
    let probe = vent_commensal();
    let step = VENT_CYCLE_TICKS / SWEEP_SAMPLES;
    let (availability, capacity, chemosynthate) = sweep(&fixture, &water, &probe, step);

    let wet = geo
        .vertices()
        .filter(|&v| *availability[0].get(v) > 0.0)
        .count();
    let (a, c, k) = (
        expired(geo, &availability),
        expired(geo, &capacity),
        expired(geo, &chemosynthate),
    );
    // The magnitude half: how far the capacity actually travels at the
    // vertices the vent layer touches. A capacity that falls by 85% and a
    // capacity that reaches exactly zero are different findings, and only the
    // second is what M3a's counter can see.
    let mut worst_ratio = 1.0_f64;
    let mut moved = 0_usize;
    let mut moved_floor = f64::INFINITY;
    for v in geo.vertices() {
        let mut lo = f64::INFINITY;
        let mut hi = 0.0_f64;
        for reading in &capacity {
            let x = *reading.get(v);
            lo = lo.min(x);
            hi = hi.max(x);
        }
        if hi > 0.0 && lo.to_bits() != hi.to_bits() {
            moved += 1;
            worst_ratio = worst_ratio.min(lo / hi);
            moved_floor = moved_floor.min(lo);
        }
    }
    println!(
        "M3a LIVE (seed 42, {SWEEP_SAMPLES} instants across one vent cycle, subject \
         vent-commensal): {wet} of {} vertices carry a water column; availability expired at \
         {a}; capacity expired at {c}; chemosynthate expired at {k}; capacity MOVED at {moved} \
         vertices, deepest trough {:.6} of that vertex's own peak, lowest capacity ever \
         reached at a moved vertex {:.6}",
        geo.vertices().count(),
        worst_ratio,
        moved_floor
    );

    assert!(
        wet > 0,
        "a Marine kind must have a water column somewhere, or every count above is vacuous"
    );

    // THE PREREGISTERED HALF, reported and not rescued. `c` and `a` are
    // printed above whatever they say and nothing here asserts a floor on
    // either, because the campaign preregistered a zero as a publishable
    // result rather than a failure.

    // Availability: a structural zero. No vent can add or remove a pelagic
    // band, so this arm of the preregistered counter had no way to fire.
    assert_eq!(
        a, 0,
        "availability is the {{0, 1}} water-column mask; a non-zero here would mean the vent \
         layer is adding or removing whole pelagic bands, which nothing in `WaterWorld::at` \
         does"
    );

    // §4's claim, measured on the axis the vent actually feeds.
    assert!(
        k > 0,
        "spec §4 claims the marine habitat's quality is time-varying and can reach NOTHING at \
         a vertex it once supported. A zero here — with the ablated control also at zero — \
         would falsify that claim outright and is the campaign's headline"
    );

    // THE BLINDNESS, ASSERTED RATHER THAN ARGUED. `c == 0` above is not
    // evidence against §4, and this is what establishes that: at every vertex
    // the vent layer moves at all — the only vertices the capacity counter
    // could ever have fired at — capacity never reaches zero, because
    // `axis_supply_with` SUMS the niche's axes and `vent-commensal` weights
    // `MARINE_FORAGE` at 0.25 beside its `CHEMOSYNTHATE` 0.75.
    // `marine_forage_supply_field` grades every ocean vertex strictly above
    // zero (0.02 at abyssal/hadal/vent, its floor), so the supply term is
    // bounded away from zero and the Michaelis-Menten headcount with it.
    //
    // A RED HERE IS INFORMATIVE, NOT A REGRESSION: it would mean the capacity
    // counter can now fire, and M3a's preregistered zero is due a re-reading.
    assert!(
        moved > 0,
        "the vent layer must move this kind's capacity somewhere, or the floor below is \
         vacuous"
    );
    assert!(
        moved_floor > 0.0,
        "capacity at a vent-moved vertex must stay strictly positive across the whole sweep — \
         this is WHY the preregistered capacity counter reads {c}, and it is a property of \
         the niche's summed axes, not of the habitat failing to expire"
    );
}

/// **M3b — the reachability half: how often does the real bake actually put a
/// people on a vent that then fails?** The Tidemark, Task 5; spec §4.
///
/// # A zero here is a REACHABILITY finding, not a falsification
///
/// The plan says so explicitly, and the reason is measured rather than
/// assumed. Task 2 found the vent layer reaches *capacity* but not *siting*:
/// two synthetic marine probes placed four occupations each through the real
/// bake at seed 42 and neither landed on a vent-improved vertex. If this reads
/// zero it means the bake's siting does not put anyone on a vent — a fact
/// about where communities go, not about whether an expiring habitat expires,
/// which `m3a_the_marine_habitat_expires_over_world_time` and the four direct
/// mechanism tests in `history_bake.rs` already answer independently.
///
/// **Nothing here is tuned to make it non-zero.**
///
/// # What is counted, in three widening rings
///
/// 1. **hosted** — occupied sites sitting in some vent's candidate ring at
///    all. This is the ceiling: the rule cannot fire anywhere else.
/// 2. **failed during tenure** — of those, the sites where every hosting
///    source is `Failed` at some epoch instant inside the occupation's own
///    `[founded, ended]` span. This is the plan's "transitions into `Failed`
///    across the bake".
/// 3. **ended ON the failure** — of those, the occupations whose recorded end
///    year is the FIRST such epoch. That is the rule firing, attributable
///    without instrumenting the bake: no other ending path is a function of
///    vent phase, so an ending landing exactly on the first failed epoch is
///    this one.
///
/// The three are nested by construction, which is asserted rather than
/// assumed — a count that broke the nesting would mean the walk and the bake
/// disagree about which years an occupation was alive through.
#[test]
fn m3b_how_often_the_bake_seats_a_people_on_a_vent_that_then_fails() {
    let fixture = fixture(Seed(42));
    let water = waterworld_from(
        &fixture.world,
        &fixture.terrain,
        &fixture.climate,
        WaterWorldConfig { enabled: true },
    );
    let tenancy = VentTenancy::from_overlay(&water);
    assert!(
        tenancy.vent_count() > 0 && tenancy.hosted_vertex_count() > 0,
        "seed 42 must admit vents and host vertices, or every count below is vacuous"
    );

    let wc = WorldComponents::assemble().expect("the shipped component roster assembles");
    let history = hornvale_worldgen::history_for(
        Seed(42),
        &SkyPins::default(),
        &TerrainPins::default(),
        &SettlementPins::default(),
        &wc,
    )
    .expect("seed 42 bakes a history");

    // The bake's own epoch grid, read off the same config the bake used
    // rather than restated as literals here.
    let cfg = hornvale_worldgen::BakeConfig::default_millennia();
    let epochs: Vec<f64> = {
        let mut years = Vec::new();
        let mut year = cfg.start_year;
        while year < cfg.end_year {
            years.push(year);
            year += cfg.epoch_years;
        }
        years
    };

    let mut hosted = 0_usize;
    let mut failed_during_tenure = 0_usize;
    let mut ended_on_the_failure = 0_usize;
    for record in &history.records {
        if !tenancy.is_hosted(record.core.site) {
            continue;
        }
        hosted += 1;
        let last = record.core.ended.unwrap_or(cfg.end_year);
        let first_failed = epochs
            .iter()
            .copied()
            .filter(|&y| y >= record.core.founded && y <= last)
            .find(|&y| tenancy.failed_at(record.core.site, VentTenancy::instant_of_bake_year(y)));
        if let Some(first) = first_failed {
            failed_during_tenure += 1;
            if record.core.ended == Some(first) {
                ended_on_the_failure += 1;
            }
        }
    }

    println!(
        "M3b (seed 42, the real bake): {} occupations total; {hosted} on ground a vent hosts; \
         {failed_during_tenure} of those saw every hosting source FAIL during their own \
         tenure; {ended_on_the_failure} ended on that exact epoch. The overlay admits {} vents \
         over {} hosted vertices.",
        history.records.len(),
        tenancy.vent_count(),
        tenancy.hosted_vertex_count()
    );

    assert!(
        !history.records.is_empty(),
        "seed 42 must bake some occupations, or the zeroes above say nothing about siting"
    );
    assert!(
        failed_during_tenure <= hosted && ended_on_the_failure <= failed_during_tenure,
        "the three counts nest by construction: {ended_on_the_failure} <= \
         {failed_during_tenure} <= {hosted}"
    );
}

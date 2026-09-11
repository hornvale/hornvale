//! The Tidemark, Task 2: the pelagic ladder is not vacuous, and the
//! Waterworld overlay's vents are what make it move.
//!
//! # The shape of the proof
//!
//! A `Marine` arm that compiled, ran and produced a number would look
//! exactly like this one whether or not a single marine input reached it.
//! So the arm is exercised **twice over one frozen world**, through the
//! production entry point placement itself calls
//! (`per_species_capacity_at`), with exactly one thing different between the
//! arms: whether the `EraInvariantSupply` carries the vent-bearing marine
//! habitat (`build_at`) or the ambient one (`build`). Terrain, climate, seed,
//! era adjustment, and the kind's own identity are the same object in both.
//!
//! Three controls, because a difference alone proves less than it looks:
//!
//! 1. **The precondition is asserted before the perturbation**, so a
//!    mutation that could not move anything cannot pass as evidence: the
//!    world must actually admit vents, and the vent-bearing habitat must
//!    actually differ from the ambient one at some (vertex, band). A run
//!    where those are equal is a RED here, not a quiet green.
//! 2. **The same kind forced to `Surface`** must be bit-identical across the
//!    two arms. Every other field of `EraInvariantSupply` is built by the
//!    same code in both, so a difference there would mean the divergence
//!    came from somewhere other than the marine arm.
//! 3. **An overlay with `enabled: false`** must make `build_at` and `build`
//!    agree bit-for-bit even on the `Marine` arm — the ablation the
//!    `WaterWorldConfig` flag exists for. Without it, "the arms differ"
//!    could be an artifact of the two constructors disagreeing about
//!    something other than vents.
//!
//! Test fixture (decision 0092): calls the sculpt/fit derivation entry
//! points directly to build its own world state, copied rather than shared.
#![allow(clippy::disallowed_methods)]

use hornvale_astronomy::SkyPins;
use hornvale_climate::GeneratedClimate;
use hornvale_kernel::seed::StreamLabel;
use hornvale_kernel::{Seed, World, WorldTime};
use hornvale_species::{BiomeAffinity, BiosphereTraits, HabitatRealm};
use hornvale_terrain::{GeneratedTerrain, TerrainPins};
use hornvale_worldgen::waterworld::{WaterWorld, WaterWorldConfig, waterworld_from};
use hornvale_worldgen::{
    BuildDepth, EraAdjust, EraInvariantSupply, MarineHabitat, PELAGIC_BANDS, SettlementPins,
    WorldComponents, build_world_to_with_artifacts, climate_from, per_species_capacity_at,
};

/// The probe kind, synthesised rather than borrowed from the roster, and
/// the reason is a measurement rather than a convenience: **no shipped kind
/// can score in the water column at all.** Forcing `human` to `Marine`
/// yields a capacity of exactly zero at every one of seed 42's 40,962
/// vertices — its elevation curve is authored for land, so
/// `ConditionResponse::eval` underflows to `0.0` some kilometres below sea
/// level, and its resource weights read axes the sea does not supply. A
/// probe that cannot move is not a probe, so this one is authored to be
/// able to: a deep-water elevation optimum, a cold-water temperature
/// optimum, and weights on the two axes a marine column actually carries.
///
/// It is **not** a marine people and does not pretend to be one — Task 3
/// authors those. It is the smallest kind that makes the ladder's arithmetic
/// observable, which is what "prove it is not vacuous" needs.
fn pelagic_probe() -> BiosphereTraits {
    let shipped = hornvale_species::biosphere_registry();
    let base = shipped
        .get(&hornvale_species::KindId("human"))
        .expect("the roster carries human")
        .clone();
    BiosphereTraits {
        niche: hornvale_kernel::ecology::ResourceVector::new(&[
            (hornvale_kernel::MARINE_FORAGE, 0.6),
            (hornvale_kernel::CHEMOSYNTHATE, 0.4),
        ])
        .expect("two non-negative weights are a legal resource vector"),
        condition_niche: hornvale_species::ConditionNiche {
            temperature: hornvale_kernel::ecology::ConditionResponse {
                optimum: 8.0,
                width: 20.0,
                devotion: 0.8,
            },
            moisture: hornvale_kernel::ecology::ConditionResponse {
                optimum: 1.0,
                width: 1.0,
                devotion: 0.8,
            },
            insolation: hornvale_kernel::ecology::ConditionResponse {
                optimum: 0.5,
                width: 1.0,
                devotion: 0.8,
            },
            // Deep, and wide enough to reach the whole pelagic column: the
            // bands run 0 m to the seabed, so a curve centred at -1500 m
            // with a 3 km breadth scores every one of them without picking
            // a winner in advance.
            elevation: hornvale_kernel::ecology::ConditionResponse {
                optimum: -1500.0,
                width: 3000.0,
                devotion: 0.8,
            },
        },
        ..base
    }
}

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

/// The probe kind's capacity map under `realm`, scored through the
/// production entry point placement itself calls, against an explicitly
/// supplied hoist. A one-kind roster: `per_species_capacity_at` maps over
/// the slice it is handed, so there is nothing a longer one would add.
fn capacity_of(
    fixture: &Fixture,
    hoisted: &EraInvariantSupply,
    probe: &BiosphereTraits,
    realm: HabitatRealm,
) -> hornvale_kernel::VertexMap<f64> {
    let bio: Vec<&BiosphereTraits> = vec![probe];
    let realms = vec![realm];
    let affinity: Vec<Option<BiomeAffinity>> = vec![None];
    let caps = per_species_capacity_at(
        fixture.terrain.geosphere(),
        &fixture.terrain,
        &fixture.climate,
        hoisted,
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
    hornvale_kernel::VertexMap::from_fn(fixture.terrain.geosphere(), |v| map.at(v))
}

/// Build both hoists over one world: ambient, and read at `time` against
/// `water`.
fn hoists(
    fixture: &Fixture,
    water: &WaterWorld,
    time: WorldTime,
) -> (EraInvariantSupply, EraInvariantSupply) {
    let geo = fixture.terrain.geosphere();
    let obliquity_deg = fixture.climate.obliquity_deg();
    let insolation_scalar = fixture.climate.insolation();
    let regime = fixture.climate.regime();
    (
        EraInvariantSupply::build(
            geo,
            &fixture.terrain,
            &fixture.climate,
            obliquity_deg,
            insolation_scalar,
            &regime,
        ),
        EraInvariantSupply::build_at(
            geo,
            &fixture.terrain,
            &fixture.climate,
            obliquity_deg,
            insolation_scalar,
            &regime,
            water,
            time,
        ),
    )
}

/// How many `(vertex, band)` slots the two habitats disagree at, comparing
/// bit-for-bit rather than by epsilon.
fn habitat_differences(
    geo: &hornvale_kernel::Geosphere,
    a: &MarineHabitat,
    b: &MarineHabitat,
) -> usize {
    let mut differences = 0;
    for vertex in geo.vertices() {
        let (sa, sb) = (a.substrate.get(vertex), b.substrate.get(vertex));
        let (ca, cb) = (a.chemosynthate.get(vertex), b.chemosynthate.get(vertex));
        for band in 0..PELAGIC_BANDS {
            let moved_substrate = match (sa[band], sb[band]) {
                (Some(x), Some(y)) => {
                    x.temperature_c.to_bits() != y.temperature_c.to_bits()
                        || x.moisture.to_bits() != y.moisture.to_bits()
                        || x.insolation.to_bits() != y.insolation.to_bits()
                        || x.height_asl_m.get().to_bits() != y.height_asl_m.get().to_bits()
                }
                (None, None) => false,
                _ => true,
            };
            if moved_substrate || ca[band].to_bits() != cb[band].to_bits() {
                differences += 1;
            }
        }
    }
    differences
}

fn count_differences(
    geo: &hornvale_kernel::Geosphere,
    a: &hornvale_kernel::VertexMap<f64>,
    b: &hornvale_kernel::VertexMap<f64>,
) -> usize {
    geo.vertices()
        .filter(|&v| a.get(v).to_bits() != b.get(v).to_bits())
        .count()
}

#[test]
fn the_marine_ladder_reads_the_vents() {
    let fixture = fixture(Seed(42));
    let geo = fixture.terrain.geosphere();
    let water = waterworld_from(
        &fixture.world,
        &fixture.terrain,
        &fixture.climate,
        WaterWorldConfig { enabled: true },
    );

    // CONTROL 1 — the precondition, asserted BEFORE the perturbation is
    // believed. A world with no vent, or a genesis instant at which no vent
    // contributes anything, would make everything below vacuously equal and
    // the test would then be measuring nothing.
    assert!(
        !water.vents.is_empty(),
        "seed 42 must admit at least one WaterVent for this probe to say anything"
    );
    let ambient = MarineHabitat::ambient(geo, &fixture.terrain, &fixture.climate);
    let vented = MarineHabitat::at_instant(geo, &fixture.climate, &water, WorldTime::GENESIS);
    let habitat_moved = habitat_differences(geo, &ambient, &vented);
    println!(
        "seed 42: vents={} habitat slots moved by the vent layer at GENESIS={habitat_moved}",
        water.vents.len()
    );
    assert!(
        habitat_moved > 0,
        "the vent-bearing habitat must differ from the ambient one somewhere, or there is no \
         marine input for the ladder to read and the assertions below are vacuous"
    );

    let probe = pelagic_probe();
    let (plain, at_genesis) = hoists(&fixture, &water, WorldTime::GENESIS);

    // CONTROL 2 — the same kind, same world, same hoists, scored on the
    // SURFACE arm: bit-identical. This is what localises the difference
    // below to the marine arm rather than to some other era-invariant field.
    let surface_plain = capacity_of(&fixture, &plain, &probe, HabitatRealm::Surface);
    let surface_vented = capacity_of(&fixture, &at_genesis, &probe, HabitatRealm::Surface);
    assert_eq!(
        count_differences(geo, &surface_plain, &surface_vented),
        0,
        "the Surface arm must not see the vent layer at all — a difference here means the two \
         hoists disagree about something other than the marine habitat"
    );

    // THE MEASUREMENT: the same frozen kind on the MARINE arm, ambient vs
    // vent-bearing.
    let marine_plain = capacity_of(&fixture, &plain, &probe, HabitatRealm::Marine);
    let marine_vented = capacity_of(&fixture, &at_genesis, &probe, HabitatRealm::Marine);
    let moved = count_differences(geo, &marine_plain, &marine_vented);
    let wet = geo
        .vertices()
        .filter(|&v| *marine_plain.get(v) > 0.0)
        .count();
    println!(
        "seed 42: the pelagic probe on the Marine arm — non-zero capacity at {wet} of {} \
         vertices; the vent layer moves {moved} of them",
        geo.vertices().count()
    );
    assert!(
        wet > 0,
        "a Marine kind must have non-zero capacity somewhere, or the presence mask is gating \
         the whole globe to zero and the ladder is unreachable"
    );
    assert!(
        moved > 0,
        "the vent layer must move the marine capacity somewhere: the habitat differs at \
         {habitat_moved} slots, so a zero here means the ladder is reading the habitat's \
         vent-bearing half and discarding it"
    );
}

#[test]
fn an_ablated_overlay_leaves_the_marine_ladder_exactly_where_it_was() {
    // CONTROL 3 — the ablation seam `WaterWorldConfig { enabled }` exists
    // for. With the overlay withheld, `build_at` and `build` must agree
    // bit-for-bit on the marine arm too; otherwise the difference the test
    // above measures could be the two constructors disagreeing about
    // something that is not a vent.
    let fixture = fixture(Seed(42));
    let geo = fixture.terrain.geosphere();
    let withheld = waterworld_from(
        &fixture.world,
        &fixture.terrain,
        &fixture.climate,
        WaterWorldConfig { enabled: false },
    );
    assert!(
        withheld.substrate.is_empty() && withheld.vents.is_empty(),
        "a withheld overlay is empty — that is what makes it an ablation"
    );

    let probe = pelagic_probe();
    let (plain, ablated) = hoists(&fixture, &withheld, WorldTime::GENESIS);
    let marine_plain = capacity_of(&fixture, &plain, &probe, HabitatRealm::Marine);
    let marine_ablated = capacity_of(&fixture, &ablated, &probe, HabitatRealm::Marine);
    // An empty overlay yields an empty habitat, so every vertex gates to
    // `availability = 0.0` and the whole map is zero — which is exactly the
    // pre-Task-2 behaviour, and the honest reading for a caller that
    // withheld the sea.
    assert_eq!(
        geo.vertices()
            .filter(|&v| *marine_ablated.get(v) > 0.0)
            .count(),
        0,
        "withholding the overlay must leave a Marine kind with nowhere to live"
    );
    assert!(
        geo.vertices().any(|v| *marine_plain.get(v) > 0.0),
        "the ambient habitat, which needs no overlay, must still seat a Marine kind — \
         otherwise the zero above is not evidence of the ablation"
    );
}

#[test]
fn the_ambient_habitat_is_the_overlay_read_at_genesis_without_vents() {
    // The two constructors are one derivation over one column walk, so at
    // genesis they may differ ONLY by the vent layer. Proved by suppressing
    // the vents rather than by re-deriving: an overlay whose vent list is
    // emptied must read back exactly as `ambient` does.
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

    let ambient = MarineHabitat::ambient(geo, &fixture.terrain, &fixture.climate);
    let ventless = MarineHabitat::at_instant(geo, &fixture.climate, &water, WorldTime::GENESIS);
    assert_eq!(
        habitat_differences(geo, &ambient, &ventless),
        0,
        "at GENESIS with no vent admitted, the overlay reading and the ambient reading must be \
         bit-identical — they are the same column walk and the same field derivation"
    );
}

#[test]
fn vent_admission_stays_keyed_to_its_own_vertex() {
    // The determinism claim spec §6 makes about this campaign's one seeded
    // input, checked rather than assumed: `WATERWORLD_VENT` is consumed
    // through a PER-VERTEX sub-stream, so a vent at one vertex cannot shift
    // another vertex's draws. Re-derive each admitted vent's four values
    // from its own key alone and require them to match what the overlay
    // built — which can only hold if nothing else shares that stream.
    let fixture = fixture(Seed(42));
    let water = waterworld_from(
        &fixture.world,
        &fixture.terrain,
        &fixture.climate,
        WaterWorldConfig { enabled: true },
    );
    assert!(!water.vents.is_empty(), "seed 42 admits vents");
    for vent in &water.vents {
        let key = format!("vertex/{}", vent.vertex.0);
        let mut stream = fixture
            .world
            .seed
            .derive(hornvale_worldgen::streams::WATERWORLD_VENT)
            .derive(StreamLabel::dynamic(&key))
            .stream();
        let admission = stream.next_f64();
        assert!(
            admission < 0.25,
            "an admitted vent's own sub-stream must reproduce its admission draw"
        );
        let strength = 0.25 + stream.next_f64() * 0.75;
        let temperature_delta = 5.0 + stream.next_f64() * 95.0;
        let chemistry = stream.next_f64();
        assert_eq!(
            (
                strength.to_bits(),
                temperature_delta.to_bits(),
                chemistry.to_bits()
            ),
            (
                vent.strength.to_bits(),
                vent.temperature_delta.to_bits(),
                vent.chemistry.to_bits()
            ),
            "vent at {:?} must be reproducible from its own per-vertex sub-stream alone",
            vent.vertex
        );
    }
}

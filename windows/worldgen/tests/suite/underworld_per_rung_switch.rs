//! THE SOURCES, Task 9: the per-rung switch's own regression pin.
//!
//! `MAP-per-rung-substrate`'s consumer switch (Ruling 7's "the campaign's
//! only movement in shipped world numbers") reads `per_species_suitability`'s
//! `Subterranean` arm off the best rung of `subterranean_substrate_field_per_
//! rung`/`subterranean_energy_field_per_rung`, in place of the single
//! deepest-point reading `subterranean_substrate_field` took before, and
//! feeds `CHEMOSYNTHATE` its real supply for the first time. Two claims need
//! a live-world witness, not a unit test over a hand-built vertex: nothing
//! outside the three-kind `Subterranean` roster (`rust-monster`, `xorn`,
//! `drow` — `hornvale_species::habitat_realm_registry`) moved, and something
//! inside it did.
//!
//! # The before-arm, and why it is captured rather than re-derived
//!
//! [`capture_before_arm`] ran once, against the UNMODIFIED pre-switch tree
//! (commit `3d074d2af`, before this task's edit to
//! `windows/worldgen/src/lib.rs`), and wrote
//! `tests/fixtures/task9-before-cave-suitability-seed-42.csv`. A before-arm
//! re-derived from the new code would prove nothing (`submerged_before_arm.rs`
//! states the same rule) — this is real, executed, pre-switch evidence, not a
//! description of the old formula reconstructed after the fact.
//!
//! **Ignored in the ordinary suite.** Running it again after this task's edit
//! would silently regenerate the fixture from the FIXED code and defeat the
//! whole point of freezing it first. It is kept, not deleted, so the capture
//! stays reproducible against the pre-switch tree if anyone needs to redo it:
//!
//! ```text
//! git checkout 3d074d2af -- windows/worldgen/src/lib.rs
//! cargo test -p hornvale-worldgen --test suite -- underworld_per_rung_switch::capture_before_arm --ignored
//! git checkout HEAD -- windows/worldgen/src/lib.rs
//! ```
//!
//! # Why the comparison universe is cave-bearing vertices, not the whole globe
//!
//! A vertex with no cave is arithmetically untouched by this task on EITHER
//! side of the switch: a `Subterranean` kind reads `availability = 0.0`
//! there before and after (the per-rung fields' every slot is `None`
//! whenever `terrain.cave_at` is `None` — see `subterranean_substrate_field_
//! per_rung`'s and `subterranean_energy_field_per_rung`'s own doc comments),
//! and a `Surface` kind's arithmetic never reads cave state at all. So a
//! cave-bearing vertex is not merely a cheaper sample than the full ~40,962-
//! vertex globe — it is the ENTIRE population where this switch has any
//! chance of moving a number, on either side of the roster boundary. Seed 42
//! alone (not the full `[42, 7, 1234]` frozen set `underworld_separation.rs`
//! and `subterranean_energy_probe.rs` use) keeps the committed fixture
//! proportionate to this crate's other fixtures (`repose-exposure.csv`,
//! `occupancy.csv`) while still comparing the full production roster (39
//! kinds, `hornvale_species::biosphere_registry`) over every one of seed 42's
//! cave-bearing vertices — tens of thousands of (kind, vertex) pairs, not a
//! hand-picked handful.
#![allow(clippy::disallowed_methods)]

use std::collections::{BTreeMap, BTreeSet};

use hornvale_kernel::{Seed, Vertex};
use hornvale_species::{BiomeAffinity, BiosphereTraits, HabitatRealm};
use hornvale_worldgen::components::WorldComponents;
use hornvale_worldgen::{
    BuildDepth, SettlementPins, SkyChoice, build_world_to_with_artifacts, per_species_suitability,
    sky_of,
};

/// The three kinds `hornvale_species::habitat_realm_registry` places in
/// [`HabitatRealm::Subterranean`] — the only kinds this task's switch may
/// move (Ruling 7; verified independently against the registry in
/// `domains/species/src/lib.rs`).
const SUBTERRANEAN_ROSTER: [&str; 3] = ["rust-monster", "xorn", "drow"];

/// The one seed this file's fixture is captured against. Named so both
/// tests below build the same world rather than each spelling `42` inline.
const SEED: u64 = 42;

/// The full production roster's biosphere/realm/affinity arrays, in
/// ascending-`KindId` order — the same construction
/// `demography_report_with_beta_from` (`windows/worldgen/src/lib.rs`) uses
/// for the whole-roster production call, reproduced here rather than
/// imported because that function is `pub(crate)`. Returns kind labels
/// alongside so a fixture row can name its kind directly rather than by a
/// build-local dense index that is never itself serialized.
#[allow(clippy::type_complexity)]
fn species_arrays(
    wc: &WorldComponents,
) -> (
    Vec<&'static str>,
    Vec<&BiosphereTraits>,
    Vec<HabitatRealm>,
    Vec<Option<BiomeAffinity>>,
) {
    let mut names = Vec::new();
    let mut bios = Vec::new();
    let mut realms = Vec::new();
    let mut affinities = Vec::new();
    for (kind, bio) in wc.biosphere.iter() {
        names.push(kind.0);
        bios.push(bio);
        realms.push(
            wc.habitat_realm
                .get(kind)
                .copied()
                .unwrap_or(HabitatRealm::SURFACE),
        );
        affinities.push(wc.biome_affinity.get(kind).cloned());
    }
    (names, bios, realms, affinities)
}

/// Build `SEED` to `BuildDepth::Settlements` (terrain AND climate, matching
/// `per_species_suitability`'s own inputs) and return everything a caller
/// needs to call it: the terrain, the climate, and the three scalar sky
/// inputs. Mirrors `underworld_separation.rs`'s own world-building block.
fn world_at(
    wc: &WorldComponents,
) -> (
    hornvale_terrain::GeneratedTerrain,
    hornvale_climate::GeneratedClimate,
    f64,
    f64,
    hornvale_climate::RotationRegime,
) {
    let artifacts = build_world_to_with_artifacts(
        Seed(SEED),
        &hornvale_astronomy::SkyPins::default(),
        SkyChoice::Generated,
        &hornvale_terrain::TerrainPins::default(),
        &SettlementPins::default(),
        wc,
        BuildDepth::Settlements,
    )
    .expect("seed 42 builds");
    let terrain = artifacts
        .terrain
        .expect("terrain is Some at BuildDepth::Settlements");
    let climate = artifacts
        .climate
        .expect("climate is Some at BuildDepth::Settlements");
    let sky = sky_of(&artifacts.world).expect("sky reconstructs");
    let system = sky
        .system()
        .unwrap_or_else(|| panic!("seed {SEED} has a generated star system"));
    let insolation = hornvale_astronomy::insolation_rel(&system.star, &system.anchor);
    let obliquity = system.anchor.obliquity.get();
    let regime = match system.anchor.rotation {
        hornvale_astronomy::Rotation::Spinning { day, .. } => {
            hornvale_climate::RotationRegime::Spinning {
                day_std: day.as_std_days(),
            }
        }
        hornvale_astronomy::Rotation::Locked => hornvale_climate::RotationRegime::Locked,
    };
    (terrain, climate, insolation, obliquity, regime)
}

/// The committed fixture's path, relative to this crate's manifest — shared
/// by the capture and the comparison so the two can never point at two
/// different files.
const FIXTURE_PATH: &str = concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/tests/fixtures/task9-before-cave-suitability-seed-42.csv"
);

#[test]
#[ignore = "one-shot before-arm capture (The Sources, Task 9 Step 1); run by hand, not a standing regression test - see module doc"]
fn capture_before_arm() {
    let wc = WorldComponents::assemble().expect("canonical registries are well-formed");
    let (names, bios, realms, affinities) = species_arrays(&wc);
    let (terrain, climate, insolation, obliquity, regime) = world_at(&wc);
    let geo = terrain.geosphere();

    let suitabilities = per_species_suitability(
        geo,
        &terrain,
        &climate,
        obliquity,
        insolation,
        &regime,
        &bios,
        &realms,
        &affinities,
    );

    let cave_vertices: Vec<Vertex> = geo
        .vertices()
        .filter(|v| terrain.cave_at(*v).is_some())
        .collect();
    assert!(
        !cave_vertices.is_empty(),
        "seed {SEED} has no cave-bearing vertex — this fixture would be vacuous"
    );

    let mut body = String::from("kind,vertex,bits\n");
    for (tag, map) in suitabilities.iter() {
        let kind = names[*tag as usize];
        for vertex in &cave_vertices {
            body.push_str(&format!(
                "{kind},{},{}\n",
                vertex.0,
                map.get(*vertex).to_bits()
            ));
        }
    }

    std::fs::write(FIXTURE_PATH, &body).expect("the fixtures directory exists");
    eprintln!(
        "wrote {} rows over {} cave-bearing vertices and {} kinds to {FIXTURE_PATH}",
        (suitabilities.len()) * cave_vertices.len(),
        cave_vertices.len(),
        suitabilities.len()
    );
}

/// claim: exactly the [`SUBTERRANEAN_ROSTER`] moves between the committed
/// before-arm and today's tree, over every cave-bearing vertex of seed 42.
///
/// Both directions are load-bearing, and the second is the one a switch
/// wired to nothing would still pass: `underworld_conditions_probe.rs:129`
/// documents exactly that shape shipping once already (neutralising the
/// depth left all 614 worldgen tests green).
#[test]
#[ignore = "heavy: live-worldgen battery; deferred from the commit gate to the heavy set (decision 0132)"]
fn exactly_the_subterranean_roster_moves() {
    let subterranean: BTreeSet<&str> = SUBTERRANEAN_ROSTER.into_iter().collect();

    let mut before: BTreeMap<(&str, u32), u64> = BTreeMap::new();
    let fixture = include_str!("../fixtures/task9-before-cave-suitability-seed-42.csv");
    for line in fixture.lines().skip(1) {
        let mut cols = line.split(',');
        let kind = cols.next().expect("kind column");
        let vertex: u32 = cols
            .next()
            .expect("vertex column")
            .parse()
            .expect("vertex is a u32");
        let bits: u64 = cols
            .next()
            .expect("bits column")
            .parse()
            .expect("bits is a u64");
        before.insert((kind, vertex), bits);
    }
    assert!(
        before.len() > 10_000,
        "only {} rows in the committed fixture — vacuous",
        before.len()
    );

    let wc = WorldComponents::assemble().expect("canonical registries are well-formed");
    let (names, bios, realms, affinities) = species_arrays(&wc);
    let (terrain, climate, insolation, obliquity, regime) = world_at(&wc);
    let geo = terrain.geosphere();

    let now = per_species_suitability(
        geo,
        &terrain,
        &climate,
        obliquity,
        insolation,
        &regime,
        &bios,
        &realms,
        &affinities,
    );

    let mut moved: BTreeSet<&str> = BTreeSet::new();
    let mut compared = 0usize;
    let mut missing = 0usize;
    for vertex in geo.vertices() {
        if terrain.cave_at(vertex).is_none() {
            continue;
        }
        for (tag, map) in now.iter() {
            let kind = names[*tag as usize];
            let after_bits = map.get(vertex).to_bits();
            match before.get(&(kind, vertex.0)) {
                Some(&before_bits) => {
                    compared += 1;
                    if before_bits != after_bits {
                        moved.insert(kind);
                    }
                }
                None => missing += 1,
            }
        }
    }

    assert_eq!(
        missing, 0,
        "the committed fixture no longer covers every (kind, cave-vertex) pair a live \
         seed-42 build produces — the roster or seed 42's cave placement moved since the \
         fixture was captured; re-run capture_before_arm against the PRE-SWITCH tree \
         (see module doc) before trusting this test again"
    );
    assert!(
        compared > 10_000,
        "only {compared} samples compared — vacuous"
    );

    // DIRECTION 1: nothing outside the roster moved.
    let strays: Vec<&&str> = moved.difference(&subterranean).collect();
    assert!(
        strays.is_empty(),
        "{strays:?} moved and are not Subterranean — the per-rung switch leaked out of \
         the Subterranean arm"
    );

    // DIRECTION 2: something INSIDE the roster did.
    assert!(
        !moved.is_empty(),
        "no kind moved at all — the per-rung field and the CHEMOSYNTHATE supply are not \
         reaching the capacity loop"
    );

    eprintln!("moved: {moved:?} (compared {compared} (kind, cave-vertex) pairs)");
}

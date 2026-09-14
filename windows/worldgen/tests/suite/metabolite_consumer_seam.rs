//! **Does a niche that weights a metabolite actually get fed?**
//!
//! This is the one end-to-end claim The Trencher makes and the one nothing
//! asserted until ledger #46. Every other guard in this campaign is over a
//! *field* — `chemical_supply`'s four axes, their distributions, their
//! dominance. None of them touches the contract a future species author will
//! rely on: **weight `METHANE` in a niche and you get more capacity where
//! methane is abundant.**
//!
//! # Why the gap existed, and why a field probe could never have closed it
//!
//! No shipped species weights `HYDROGEN`, `REDUCED_IRON`, `REDUCED_SULPHUR` or
//! `METHANE` — the count is zero on all four (`domains/species/src/lib.rs`).
//! The axes are computed, measured by a 104,845-reading probe, narrated to the
//! player through `dominant_source`, and **eaten by nobody**. So the entire
//! per-axis half of `per_species_capacity`'s dot product is multiplied by a
//! zero weight in every world Hornvale currently generates, and any bug in it
//! is invisible: The Trencher's Task 15 changed all four axes and produced a
//! byte-identical world on five seeds.
//!
//! A probe over the supply fields cannot see this. It reads the fields
//! directly, so it is green whether or not anything downstream consumes them.
//! **The only instrument that can is one that runs a consumer**, which is what
//! this file is.
//!
//! # The synthetic consumer, and why it is not a shipped species
//!
//! Authoring a real methane-eater is an accession-cohort event — a species
//! kind registers a `<kind>-kind` concept that must land in an *appended*
//! cohort, and an inserted one re-deals every later lexicon draw and renames
//! committed peoples — plus a world change and a census refresh. Nathan ruled
//! the biota out to a successor campaign. So the consumer here is built **in
//! the test**: a real subterranean species' traits, cloned, with **only the
//! niche vector replaced**. Nothing is registered, no world moves, no draw is
//! consumed.
//!
//! Cloning rather than constructing is the experimental design, not
//! convenience: mass, thermal strategy, condition niche, realm and affinity
//! stay byte-identical between the two arms, so the *only* difference between
//! them is which resource axis they eat. Any divergence in capacity is
//! therefore attributable to the axes and to nothing else.
use hornvale_astronomy::SkyPins;
use hornvale_kernel::{METHANE, REDUCED_SULPHUR, Seed, ecology::ResourceVector};
use hornvale_species::{BiosphereTraits, HabitatRealm, KindId};
use hornvale_terrain::TerrainPins;
use hornvale_worldgen::{
    BuildDepth, SettlementPins, WorldComponents, build_world_to_with_artifacts,
    per_species_capacity,
};

/// The species whose traits both arms borrow. Any `Subterranean` kind with
/// real condition tolerances would do; `xorn` is chosen because it is the one
/// kind already weighting `CHEMOSYNTHATE`, so its condition niche is known to
/// admit the deep rungs where the metabolite axes carry signal.
const BASE_KIND: &str = "xorn";

/// Seeds the seam is checked on. More than one because a single world is an
/// anecdote — The Trencher learned that expensively in ledger #45, where a
/// 44% swing on seed 42 turned out to be a 3-4x outlier against a ~10% effect.
const SEEDS: [u64; 3] = [0, 7, 42];

/// Build one world and return the two arms' capacity maps plus the geosphere.
fn arms(
    seed_value: u64,
    wc: &WorldComponents,
) -> (
    hornvale_terrain::GeneratedTerrain,
    hornvale_kernel::ecology::CapacityMap,
    hornvale_kernel::ecology::CapacityMap,
    hornvale_kernel::VertexMap<[Option<hornvale_worldgen::energy::ChemicalSupply>; 6]>,
) {
    let artifacts = build_world_to_with_artifacts(
        Seed(seed_value),
        &SkyPins::default(),
        &TerrainPins::default(),
        &SettlementPins::default(),
        wc,
        BuildDepth::Settlements,
    )
    .expect("seam seed builds");
    let terrain = artifacts.terrain.expect("terrain at Settlements");
    let climate = artifacts.climate.expect("climate at Settlements");
    let geo = terrain.geosphere();

    let sky = hornvale_worldgen::sky_of(&artifacts.world).expect("sky");
    let system = sky.generated().system();
    let insolation_scalar = hornvale_astronomy::insolation_rel(&system.star, &system.anchor);
    let obliquity_deg = system.anchor.obliquity.get();
    let regime = match system.anchor.rotation {
        hornvale_astronomy::Rotation::Spinning { day, .. } => {
            hornvale_climate::RotationRegime::Spinning {
                day_std: day.as_std_days(),
            }
        }
        hornvale_astronomy::Rotation::Locked => hornvale_climate::RotationRegime::Locked,
    };

    let base: &BiosphereTraits = wc
        .biosphere
        .get(&KindId(BASE_KIND))
        .unwrap_or_else(|| panic!("'{BASE_KIND}' has biosphere traits"));

    // The ONLY difference between the arms.
    let mut eats_methane = base.clone();
    eats_methane.niche = ResourceVector::new(&[(METHANE, 1.0)]).expect("a unit niche is legal");
    let mut eats_sulphur = base.clone();
    eats_sulphur.niche =
        ResourceVector::new(&[(REDUCED_SULPHUR, 1.0)]).expect("a unit niche is legal");

    let biosphere: Vec<&BiosphereTraits> = vec![&eats_methane, &eats_sulphur];
    let realm = vec![HabitatRealm::Subterranean, HabitatRealm::Subterranean];
    let affinity = vec![None, None];

    let caps = per_species_capacity(
        geo,
        &terrain,
        &climate,
        obliquity_deg,
        insolation_scalar,
        &regime,
        &biosphere,
        &realm,
        &affinity,
    );
    assert_eq!(caps.len(), 2, "one capacity map per synthetic arm");
    let m = caps[0].1.clone();
    let s = caps[1].1.clone();

    // The same per-rung supply field the capacity loop reads, built from the
    // same inputs, so the direction check compares capacity against the very
    // numbers that produced it rather than against a re-derivation.
    let substrate = hornvale_worldgen::substrate_field(
        geo,
        &terrain,
        &climate,
        obliquity_deg,
        insolation_scalar,
        &regime,
    );
    let subterranean =
        hornvale_worldgen::subterranean_substrate_field_per_rung(geo, &terrain, &substrate);
    let chem =
        hornvale_worldgen::energy::chemical_supply_field_per_rung(geo, &terrain, &subterranean);
    (terrain, m, s, chem)
}

/// claim: invariant(seam, per-axis-weight-reaches-capacity) — THE CONTRACT.
/// Two species identical in every trait but the resource axis they eat must
/// not receive identical capacity fields. If they do, the per-axis half of
/// `per_species_capacity`'s dot product is dead and every metabolite
/// calibration in this campaign is unobservable.
///
/// This is deliberately a *difference* assertion and not a threshold: the
/// campaign's claim is that the axes are DISTINGUISHABLE to a consumer, and a
/// threshold would invite tuning to reach it.
#[test]
fn a_niche_that_weights_a_metabolite_is_fed_differently_from_one_that_does_not() {
    let wc = WorldComponents::assemble().expect("registries");
    for seed in SEEDS {
        let (terrain, methane_cap, sulphur_cap, _chem) = arms(seed, &wc);
        let geo = terrain.geosphere();
        let mut differing = 0usize;
        let mut methane_fed = 0usize;
        let mut sulphur_fed = 0usize;
        for vertex in geo.vertices() {
            let m = methane_cap.at(vertex);
            let s = sulphur_cap.at(vertex);
            if m != s {
                differing += 1;
            }
            if m > s {
                methane_fed += 1;
            }
            if s > m {
                sulphur_fed += 1;
            }
        }
        assert!(
            differing > 0,
            "seed {seed}: a METHANE-eating niche and a REDUCED_SULPHUR-eating niche — identical \
             in mass, thermal strategy, condition niche, realm and affinity, differing ONLY in \
             which axis they weight — received byte-identical capacity at every one of the \
             world's vertices. The per-axis metabolite weights reach nothing, so no species an \
             author writes can ever eat this chemistry. See this file's module doc."
        );
        assert!(
            methane_fed > 0 && sulphur_fed > 0,
            "seed {seed}: the two axes do not each win somewhere (methane ahead at \
             {methane_fed} vertices, sulphur ahead at {sulphur_fed}). One axis dominating \
             everywhere means a chamber cannot be characterised by its chemistry, which is \
             what the per-axis basis exists to allow."
        );
        eprintln!(
            "SEAM seed {seed}: differing={differing} methane-ahead={methane_fed} \
             sulphur-ahead={sulphur_fed}"
        );
    }
}

/// claim: invariant(seam, capacity-follows-the-weighted-axis) — DIRECTION.
/// The previous test proves the axes are distinguishable; this one proves they
/// are distinguishable *in the right direction*. Both arms saturate the same
/// way and carry the same tolerance term, so the arm weighting the locally
/// richer axis must be the arm with the higher capacity. A seam that
/// discriminated backwards would pass the difference test and mis-site every
/// specialist an author ever writes.
#[test]
fn the_better_fed_arm_is_the_one_whose_axis_is_locally_richer() {
    let wc = WorldComponents::assemble().expect("registries");
    for seed in SEEDS {
        let (terrain, methane_cap, sulphur_cap, chem) = arms(seed, &wc);
        let geo = terrain.geosphere();
        let (mut agree, mut disagree) = (0usize, 0usize);
        for vertex in geo.vertices() {
            let m = methane_cap.at(vertex);
            let s = sulphur_cap.at(vertex);
            if m == s {
                continue;
            }
            // The best rung's own supply on each axis — the same `max` the
            // capacity loop takes, read from the same field it reads.
            let rungs = chem.get(vertex);
            let best = |pick: fn(&hornvale_worldgen::energy::ChemicalSupply) -> f64| -> f64 {
                rungs
                    .iter()
                    .filter_map(|r| r.as_ref().map(pick))
                    .fold(0.0_f64, f64::max)
            };
            let methane_supply = best(|c| c.methane);
            let sulphur_supply = best(|c| c.reduced_sulphur);
            if methane_supply == sulphur_supply {
                continue;
            }
            if (m > s) == (methane_supply > sulphur_supply) {
                agree += 1;
            } else {
                disagree += 1;
            }
        }
        assert!(
            agree > 0,
            "seed {seed}: no vertex where the better-fed arm could be compared against its \
             axis's own supply — the direction check sampled nothing and would pass vacuously"
        );
        assert_eq!(
            disagree, 0,
            "seed {seed}: at {disagree} vertices the arm with the HIGHER capacity is the one \
             whose axis is LOCALLY POORER ({agree} agree). Capacity does not follow the axis a \
             niche weights, so a methane specialist would be sited where methane is scarce."
        );
        eprintln!("SEAM seed {seed}: direction agree={agree} disagree={disagree}");
    }
}

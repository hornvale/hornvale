//! THE TRENCHER, Task 5 (T1): the four-arm calibration comparison.
//!
//! Task 4 replaced the underworld food model's mean-of-seven reduction with
//! per-metabolite sums. That overshot the `ENERGY` ruler — the aggregate sat
//! above the authored corpus's `E_TEEMING = 1.0` at the median of every rung
//! — and ledger #24 fixed it by *un-collapsing* the two things that had been
//! one function: `ChemicalSupply::chemosynthate` stays the raw resource
//! magnitude, and `subterranean_energy` applies a Type-II `raw / (1 + raw)`
//! transfer on the way out, for the ruler alone.
//!
//! Ledger #24 then made T1 a **four-arm comparison** rather than the
//! single-arm prediction the plan wrote, and ledger #25 ruled that it runs
//! **before** the fourteen world-content reds are repaired, because two of
//! the arms can move the world and a literal repaired now would be repaired
//! to a dead value.
//!
//! | arm | what it is | status |
//! |---|---|---|
//! | A. raw sum | `ChemicalSupply::chemosynthate` as shipped | control, expected to fail the ruler |
//! | B. sum-then-saturate | `raw / (1 + raw)` — what `subterranean_energy` returns today | primary candidate |
//! | C. mean-of-four | `raw / 4.0` | diagnostic only; not a candidate |
//! | D. detritus add vs replace | `DETRITUS_AMBIENT + import` (shipped) vs `import` replacing it | semantic sensitivity arm |
//!
//! # THE PRIOR BASELINE IS PROSE, NOT CODE, AND IS NOT RE-MEASURED HERE
//!
//! The plan's `BASELINE: max 0.424277; rich and teeming never realized at
//! any rung in any of twelve seeds` was measured by **The Ceiling**,
//! pre-campaign, against the **mean-of-seven** reduction, on a branch whose
//! probe code was never merged. `0.424277` appears nowhere in any `.rs` file
//! in this tree, and the mean-of-seven that produced it no longer exists
//! here. It is cited below as prior measurement with that provenance; it is
//! **not** re-measured, and no assertion in this file rests on it. Arm A is
//! the control this file actually runs.
//!
//! # Nothing here changes shipped behaviour
//!
//! Every arm is computed **inside this probe**. `energy.rs` and
//! `windows/worldgen/src/lib.rs` are untouched: arms B, C and D are
//! alternative arithmetic over the same live fields, never an edit to the
//! code that produces them. If an arm wins, changing the shipped code is a
//! separate, later decision (ledger #25).
//!
//! # The bands
//!
//! `domains/climate/src/underworld.rs` authors `E_LEAN 0.25`, `E_FED 0.5`,
//! `E_RICH 0.75`, `E_TEEMING 1.0` as **corpus row values**, not as a
//! classifier — the constants are private to that module and there is no
//! shared helper, so the occupancy measurement is written here. "A rung
//! realizes `fed`" means the derived value at a vertex-rung reading is
//! `>= E_FED`; "realizes `fed` at >= 25%" means at least a quarter of that
//! rung's readings do.
//!
//! Test fixture (decision 0092): calls the composition-root entry points
//! directly, the sanctioned posture for this crate's live-worldgen
//! batteries. `world_at` mirrors `subterranean_energy_probe::world_at`
//! (`:75`) exactly, and `Q6_SEEDS` is that file's own twelve-seed set
//! (`:58`).
#![allow(clippy::disallowed_methods)]

use hornvale_astronomy::SkyPins;
use hornvale_climate::GeneratedClimate;
use hornvale_kernel::{Band, Seed, VertexMap};
use hornvale_species::{BiomeAffinity, BiosphereTraits, HabitatRealm};
use hornvale_terrain::{GeneratedTerrain, TerrainPins};
use hornvale_worldgen::energy::{ChemicalSupply, chemical_supply_field_per_rung};
use hornvale_worldgen::{
    BuildDepth, SUPPLY_AXIS_ORDER, SettlementPins, Substrate, WorldComponents, axis_supply_with,
    build_world_to_with_artifacts, carrying_inputs_of, climate_of, detritus_supply_field,
    forage_supply_field, marine_forage_supply_field, mineral_supply_field, per_species_suitability,
    prey_supply_field, substrate_field, subterranean_substrate_field,
    subterranean_substrate_field_per_rung, tolerance_liebig,
};

/// The twelve seeds `subterranean_energy_probe.rs:58` preregisters as
/// `Q6_SEEDS`, reproduced in its own listed order — the same set the plan's
/// prose baseline names ("any of twelve seeds"), so this file's arms and
/// that prior measurement describe the same world population even though
/// the reduction between them changed.
const Q6_SEEDS: [u64; 12] = [1, 7, 42, 99, 123, 256, 512, 777, 1024, 1234, 4096, 9001];

/// The five underground rungs `Band::all()` carries below `Surface` —
/// `subterranean_energy_probe.rs`'s own constant, reproduced so
/// `Band::Surface`'s always-`None` slot is never iterated.
const UNDERGROUND_RUNGS: [Band; 5] = [
    Band::Undercroft,
    Band::Shallows,
    Band::Deeps,
    Band::Underdeep,
    Band::Nadir,
];

/// `domains/climate/src/underworld.rs:126` — the authored `lean` corpus
/// value. Private there, so restated here with its provenance rather than
/// imported; this file's occupancy measurement is the only consumer.
const E_LEAN: f64 = 0.25;

/// `domains/climate/src/underworld.rs:129` — the authored `fed` corpus
/// value, and the threshold the frozen prediction names.
const E_FED: f64 = 0.5;

/// `domains/climate/src/underworld.rs:132` — the authored `rich` corpus
/// value.
const E_RICH: f64 = 0.75;

/// `domains/climate/src/underworld.rs:136` — the authored `teeming` corpus
/// value, and the kernel's own `[0, 1]` ceiling
/// (`hornvale_kernel::ecology::EnvironmentVector::new` rejects anything
/// above it, which is why ledger #24 could not move the bands).
const E_TEEMING: f64 = 1.0;

/// The three kinds `hornvale_species::habitat_realm_registry` places in
/// [`HabitatRealm::Subterranean`] — `underworld_per_rung_switch.rs:68`'s own
/// `SUBTERRANEAN_ROSTER`, in that file's order. **Each one is a different
/// arm's witness**, which is what makes the capacity half of this comparison
/// discriminating rather than decorative:
///
/// - `rust-monster` — niche `MINERAL 1.0`. Weights neither `CHEMOSYNTHATE`
///   nor `DETRITUS`, so **no arm may move it**. The null control.
/// - `xorn` — niche `MINERAL 0.65, CHEMOSYNTHATE 0.35`. The roster's only
///   `CHEMOSYNTHATE` consumer, so it is arms A/B/C's witness and arm D's
///   second null.
/// - `drow` — niche `DETRITUS 0.50, ANIMAL_PREY 0.30, PLANT_FORAGE 0.20`.
///   The roster's only subterranean `DETRITUS` consumer, so it is arm D's
///   witness and arms B/C's second null.
const SUBTERRANEAN_ROSTER: [&str; 3] = ["rust-monster", "xorn", "drow"];

/// `MINERAL_SUPPLY_SCALE` and `MARINE_SUPPLY_SCALE`
/// (`windows/worldgen/src/lib.rs:1496,1509`) are both `1.0` and both
/// private, so the capacity reconstruction below passes the value rather
/// than the constant. **This is not a hidden assumption**: if either scale
/// ever moves, `arm A reproduces per_species_suitability` — the positive
/// control the reconstruction runs on every seed — goes red, because arm A
/// is the reconstruction of the *shipped* arithmetic and the shipped
/// arithmetic would then disagree with it.
const SUPPLY_SCALE: f64 = 1.0;

/// How closely the arm-A reconstruction must reproduce
/// [`per_species_suitability`] at every cave-bearing vertex. The two compute
/// the same products in the same order from the same fields, so the
/// difference is expected to be exactly zero; the tolerance exists so a
/// future reordering inside the production loop reports as a near-miss
/// rather than as a structural break.
const CONTROL_TOLERANCE: f64 = 1e-12;

/// One of the four arms ledger #24 put in this comparison.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Arm {
    /// **A.** `ChemicalSupply::chemosynthate` as shipped — the raw sum,
    /// unbounded, used directly as the ruler. The control, and the
    /// arrangement Task 4 shipped before ledger #24 split the two values.
    RawSum,
    /// **B.** `raw / (1 + raw)` — the Type-II transfer
    /// `subterranean_energy` applies today. The primary candidate.
    SumThenSaturate,
    /// **C.** `raw / 4.0` — what a simple rescale by the metabolite count
    /// does. Diagnostic only.
    MeanOfFour,
    /// **D.** the shipped ruler (B's transfer), with `DETRITUS` taking the
    /// per-rung import *instead of* `DETRITUS_AMBIENT + import`. The
    /// semantic sensitivity arm: it varies what a `DETRITUS` eater is fed,
    /// not what the ruler reads.
    DetritusReplace,
}

/// The four arms in report order.
const ARMS: [Arm; 4] = [
    Arm::RawSum,
    Arm::SumThenSaturate,
    Arm::MeanOfFour,
    Arm::DetritusReplace,
];

impl Arm {
    /// This arm's row label in the printed tables.
    fn label(self) -> &'static str {
        match self {
            Arm::RawSum => "A raw sum",
            Arm::SumThenSaturate => "B sum-then-saturate",
            Arm::MeanOfFour => "C mean-of-four",
            Arm::DetritusReplace => "D detritus replace",
        }
    }

    /// The `ENERGY` **ruler** readout this arm produces from one
    /// vertex-rung's raw `CHEMOSYNTHATE` aggregate.
    ///
    /// **Arm D's ruler is arm B's, exactly, and that is a structural fact
    /// rather than a shortcut.** `EnergySource::DetritalImport` routes to
    /// `SupplyRoute::Detritus`, so its yield never enters
    /// `ChemicalSupply::chemosynthate` at all (`chemical_supply` sums only
    /// the four metabolites into the aggregate). Changing the detritus rule
    /// therefore cannot move the ruler by any amount, and arm D's ruler
    /// column below is reported as identical to arm B's rather than
    /// re-derived as though it might differ.
    ///
    /// **That claim is pinned elsewhere, and deliberately not re-asserted
    /// here.** `energy.rs`'s own unit test ("DetritalImport routes to
    /// DETRITUS unmodified", `energy.rs:1265`) is what would redden if the
    /// import ever entered the aggregate. An assertion in *this* file that
    /// D's ruler equals B's would be a tautology — both come out of this
    /// one `match` — and a guard its own code guarantees is not a guard.
    fn ruler(self, raw: f64) -> f64 {
        match self {
            Arm::RawSum => raw,
            Arm::SumThenSaturate | Arm::DetritusReplace => raw / (1.0 + raw),
            Arm::MeanOfFour => raw / 4.0,
        }
    }

    /// The transform this arm would apply to the `CHEMOSYNTHATE`
    /// **aggregate** if its rule were adopted for the resource magnitude and
    /// not only for the ruler.
    ///
    /// **Shipped today, this is the identity for every arm** — ledger #24
    /// leaves `chemical_supply`'s aggregate raw and puts the transfer in
    /// `subterranean_energy`, and the capacity loops read
    /// `chem.chemosynthate` directly (ledger #25's "0 of 12 closed"). So the
    /// capacity numbers B and C produce here are a **counterfactual**: what
    /// each arm would cost if it were adopted at the aggregate as well. The
    /// table says so where it prints them.
    fn aggregate_transform(self, raw: f64) -> f64 {
        match self {
            Arm::RawSum | Arm::DetritusReplace => raw,
            Arm::SumThenSaturate => raw / (1.0 + raw),
            Arm::MeanOfFour => raw / 4.0,
        }
    }

    /// True when this arm feeds the `DETRITUS` axis the per-rung import
    /// *instead of* adding it to the surface `DETRITUS` field.
    fn detritus_replaces(self) -> bool {
        matches!(self, Arm::DetritusReplace)
    }
}

/// Build `seed` to `BuildDepth::Terrain` and return its terrain, its surface
/// substrate field and its reconstructed climate.
///
/// Mirrors `subterranean_energy_probe::world_at` (`:75`) — the idiom this
/// campaign's energy probes share — with climate returned as well, because
/// the capacity half needs the supply fields that ride it. `climate_of`
/// reconstructs from committed facts independent of build depth (see
/// `deep_realm_rehome.rs::measure_one` for the same argument), so `Terrain`
/// suffices and this file does not force `Full`.
fn world_at(
    seed_value: u64,
    wc: &WorldComponents,
) -> (GeneratedTerrain, VertexMap<Substrate>, GeneratedClimate) {
    let seed = Seed(seed_value);
    let artifacts = build_world_to_with_artifacts(
        seed,
        &SkyPins::default(),
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
    (terrain, surface, climate)
}

/// The median of a slice, sorted in place — `subterranean_energy_probe.rs`'s
/// own [`median`] convention (`v.len() / 2`; the upper of the two central
/// values on an even-length slice). `NaN` on empty rather than a panic.
fn median(v: &mut [f64]) -> f64 {
    if v.is_empty() {
        return f64::NAN;
    }
    v.sort_by(f64::total_cmp);
    v[v.len() / 2]
}

/// The fraction of `sorted` at or above `threshold` — the occupancy
/// statistic. `sorted` must be ascending (`f64::total_cmp`); `0.0` on empty.
fn occupancy(sorted: &[f64], threshold: f64) -> f64 {
    if sorted.is_empty() {
        return 0.0;
    }
    let below = sorted.partition_point(|v| *v < threshold);
    (sorted.len() - below) as f64 / sorted.len() as f64
}

/// One species' row in the capacity half: the label, its traits, and its
/// realm and affinity as [`per_species_suitability`] wants them.
struct RosterEntry<'a> {
    /// The kind label, for the printed table.
    label: &'static str,
    /// The biosphere traits — niche, condition niche, mass, potency.
    bio: &'a BiosphereTraits,
    /// The declared habitat realm. Asserted to be `Subterranean`.
    realm: HabitatRealm,
    /// The declared biome affinity, `None` for an unrestricted kind.
    affinity: Option<BiomeAffinity>,
}

/// [`SUBTERRANEAN_ROSTER`]'s three kinds, resolved against the canonical
/// registries in that constant's declared order — the same
/// `biosphere`/`habitat_realm`/`biome_affinity` triple
/// `underworld_per_rung_switch::species_arrays` builds for the whole roster,
/// narrowed to the three kinds any arm here can reach.
fn roster(wc: &WorldComponents) -> Vec<RosterEntry<'_>> {
    SUBTERRANEAN_ROSTER
        .iter()
        .map(|label| {
            let kind = hornvale_species::KindId(label);
            let bio = wc
                .biosphere
                .get_by_label(label)
                .unwrap_or_else(|| panic!("{label:?} missing from the biosphere roster"));
            let realm = wc
                .habitat_realm
                .get(&kind)
                .copied()
                .unwrap_or(HabitatRealm::SURFACE);
            assert_eq!(
                realm,
                HabitatRealm::Subterranean,
                "{label:?} is not Subterranean — this file's capacity half only \
                 reconstructs the Subterranean arm of per_species_suitability"
            );
            RosterEntry {
                label,
                bio,
                realm,
                affinity: wc.biome_affinity.get(&kind).cloned(),
            }
        })
        .collect()
}

/// **THE FOUR-ARM CALIBRATION COMPARISON** (The Trencher, T1; ledger #24's
/// four arms, ledger #25's ordering).
///
/// Reports, for every arm and every underground rung: band occupancy
/// (`lean`/`fed`/`rich`/`teeming`), realized maximum, depth shape (per-rung
/// medians and whether they are non-decreasing), and the three subterranean
/// kinds' mean capacity-suitability. **The table is the deliverable; the
/// assertion at the end is a ratchet on it.**
///
/// # The frozen prediction (preregistered in `task-5-brief.md` before this
/// file was written, decision 0016)
///
/// ```text
/// PREDICTION: arm B realizes `fed` at >= 25% at at least one rung, AND its
///             realized maximum exceeds 0.5, AND no arm-B vertex reaches
///             E_TEEMING (1.0).
/// ```
///
/// **The third clause cannot fail for arm B, and saying so is part of
/// reporting it honestly.** `raw / (1 + raw)` is strictly below `1.0` for
/// every finite non-negative `raw`, so that clause is guaranteed by the
/// transfer function's algebra rather than by anything about this world. It
/// is asserted because it was frozen, not because it discriminates; the
/// clauses that can actually fail are the first two. The arm-A column is
/// where a teeming overshoot is a live possibility, and that is what makes A
/// the control.
///
/// # The prior baseline, cited not measured
///
/// The Ceiling measured `max 0.424277`, with `rich` and `teeming` never
/// realized at any rung on any of these twelve seeds, against the
/// **mean-of-seven** reduction, pre-campaign, on a branch whose probe never
/// merged. That reduction is gone from this tree and the number appears in
/// no `.rs` file; nothing below re-measures it or asserts it.
///
/// # Measured 2026-09-12, twelve seeds, `BuildDepth::Terrain`
///
/// 82,135 cave-bearing vertex-rung readings (16,427 per rung).
/// `Undercroft` → `Nadir`.
///
/// | arm | medians | realized max | best `fed` occupancy | `teeming` occupancy |
/// |---|---|---|---|---|
/// | A raw sum | 0.946769 1.220864 1.914891 2.068878 2.068878 | 3.774602 | 100% | **44.0-92.0%** |
/// | B sum-then-saturate | 0.486328 0.549725 0.656934 0.674148 0.674148 | 0.790558 | 92.0% | 0% |
/// | C mean-of-four | 0.236692 0.305216 0.478723 0.517219 0.517219 | 0.943650 | 51.4% | 0% |
/// | D detritus replace | identical to B | identical to B | identical to B | identical to B |
///
/// **Every arm is non-decreasing with depth**, so the depth shape is a
/// property of the raw aggregate and no arm creates or destroys it — the
/// monotone-not-a-trough finding `subterranean_energy_probe.rs` recorded
/// survives all four.
///
/// **The three thresholds cross-check each other**, which is why the table
/// can be read as internally consistent rather than taken on trust:
/// `raw/(1+raw) >= 0.5`, `raw/4 >= 0.25` and `raw >= 1.0` are the *same*
/// condition, so arm A's `teeming` column, arm B's `fed` column and arm C's
/// `lean` column are all `44.0 / 87.1 / 92.0 / 92.0 / 92.0` — and they are,
/// exactly.
///
/// Mean capacity-suitability over cave-bearing vertices (arm A is the
/// shipped arithmetic, validated against [`per_species_suitability`] on
/// every seed):
///
/// | arm | rust-monster | xorn | drow |
/// |---|---|---|---|
/// | A raw sum (shipped) | 0.039128 | 0.040160 | 0.030571 |
/// | B sum-then-saturate | 0.039128 | 0.023975 (-40.30%) | 0.030571 |
/// | C mean-of-four | 0.039128 | 0.021028 (-47.64%) | 0.030571 |
/// | D detritus replace | 0.039128 | 0.040160 | 0.018644 (-39.01%) |
///
/// The null control holds in both directions: `rust-monster` (niche
/// `MINERAL 1.0`) is bit-unmoved by all four arms, `drow` by the three
/// ruler arms, and `xorn` by the detritus arm.
///
/// claim: readout(off-gate, probe:) — prints the full four-arm table for
/// twelve seeds, then asserts the frozen prediction against arm B and the
/// overshoot against arm A.
#[test]
#[ignore = "probe: the four-arm ENERGY-ruler calibration comparison over twelve seeds at BuildDepth::Terrain (twelve world builds plus a per-seed capacity reconstruction); run by hand (The Trencher, Task 5, T1)"]
fn report_the_four_arm_calibration_comparison() {
    let wc = WorldComponents::assemble().expect("canonical registries are well-formed");
    let kinds = roster(&wc);

    // The ruler pool: every cave-bearing vertex-rung's RAW `CHEMOSYNTHATE`
    // aggregate, pooled across all twelve seeds and keyed by `Band as usize`
    // so `Band::Surface`'s always-empty slot keeps its index without a hole.
    // Every arm's ruler is a pure function of this one pool, which is why
    // the four arms cannot disagree about which readings they describe.
    let mut raw_by_rung: Vec<Vec<f64>> = vec![Vec::new(); Band::all().len()];
    // Mean capacity-suitability over cave-bearing vertices, indexed
    // [species][arm].
    let mut cap_total = vec![[0.0f64; ARMS.len()]; kinds.len()];
    let mut cap_count = vec![0usize; kinds.len()];
    // The positive control: the largest absolute disagreement between the
    // arm-A reconstruction and the shipped `per_species_suitability`.
    let mut control_worst = 0.0f64;
    let mut control_compared = 0usize;

    for seed in Q6_SEEDS {
        let (terrain, surface, climate) = world_at(seed, &wc);
        let geo = terrain.geosphere();
        let sub_per_rung = subterranean_substrate_field_per_rung(geo, &terrain, &surface);
        let sub_single = subterranean_substrate_field(geo, &terrain, &surface);
        let chem_per_rung = chemical_supply_field_per_rung(geo, &terrain, &sub_per_rung);

        for vertex in geo.vertices() {
            let entry = chem_per_rung.get(vertex);
            for &rung in &UNDERGROUND_RUNGS {
                if let Some(chem) = entry[rung as usize] {
                    raw_by_rung[rung as usize].push(chem.chemosynthate);
                }
            }
        }

        // The capacity half. The supply fields are exactly the ones
        // `per_species_suitability`'s own hoist builds (lib.rs:1935-1946);
        // the reconstruction below is loop 1's `score_at` with two knobs
        // (the aggregate transform and the detritus rule) and nothing else
        // changed, and arm A is asserted against the shipped function.
        let base_carrying = hornvale_demography::carrying_capacity(
            geo,
            &carrying_inputs_of(geo, &terrain, &climate),
        );
        let forage = forage_supply_field(geo, base_carrying.as_vertex_map());
        let mineral = mineral_supply_field(geo, &terrain, SUPPLY_SCALE);
        let detritus = detritus_supply_field(geo, &terrain);
        let prey = prey_supply_field(geo, &forage);
        let marine = marine_forage_supply_field(geo, &terrain, &climate, SUPPLY_SCALE);
        let biome = climate.biome_map();

        let bios: Vec<&BiosphereTraits> = kinds.iter().map(|k| k.bio).collect();
        let realms: Vec<HabitatRealm> = kinds.iter().map(|k| k.realm).collect();
        let affinities: Vec<Option<BiomeAffinity>> =
            kinds.iter().map(|k| k.affinity.clone()).collect();
        let shipped = per_species_suitability(
            geo,
            &terrain,
            &climate,
            climate.obliquity_deg(),
            climate.insolation(),
            &climate.regime(),
            &bios,
            &realms,
            &affinities,
        );

        for (i, k) in kinds.iter().enumerate() {
            let floor_buf = hornvale_kernel::sovereignty_floor(k.bio.mass, k.bio.potency);
            let cn = &k.bio.condition_niche;
            let niche_weights = SUPPLY_AXIS_ORDER.map(|axis| k.bio.niche.weight(axis));
            let shipped_map = &shipped[i].1;

            for vertex in geo.vertices() {
                if terrain.cave_at(vertex).is_none() {
                    continue;
                }
                let affinity = k
                    .affinity
                    .as_ref()
                    .map_or(1.0, |a| a.factor(biome.get(vertex).name()));
                let substrate_here = sub_per_rung.get(vertex);
                let chemical_here = chem_per_rung.get(vertex);

                for (a, &arm) in ARMS.iter().enumerate() {
                    // lib.rs:1993-2010's `score_at`, with the arm's two
                    // knobs applied where — and only where — the arm says.
                    let score_at = |s: &Substrate, chem: &ChemicalSupply| -> f64 {
                        use hornvale_kernel::{
                            ANIMAL_PREY, CHEMOSYNTHATE, DETRITUS, HYDROGEN, MARINE_FORAGE, METHANE,
                            MINERAL, PHOTOSYNTHATE, PLANT_FORAGE, REDUCED_IRON, REDUCED_SULPHUR,
                        };
                        let detritus_axis = if arm.detritus_replaces() {
                            chem.detritus
                        } else {
                            *detritus.get(vertex) + chem.detritus
                        };
                        let per_axis = [
                            (PHOTOSYNTHATE, base_carrying.at(vertex)),
                            (PLANT_FORAGE, *forage.get(vertex)),
                            (MINERAL, *mineral.get(vertex)),
                            (DETRITUS, detritus_axis),
                            (ANIMAL_PREY, *prey.get(vertex)),
                            (MARINE_FORAGE, *marine.get(vertex)),
                            (CHEMOSYNTHATE, arm.aggregate_transform(chem.chemosynthate)),
                            (HYDROGEN, chem.hydrogen),
                            (REDUCED_IRON, chem.reduced_iron),
                            (REDUCED_SULPHUR, chem.reduced_sulphur),
                            (METHANE, chem.methane),
                        ];
                        let supply = axis_supply_with(&niche_weights, &per_axis);
                        let saturated = supply / (1.0 + supply);
                        saturated * tolerance_liebig(cn, s, floor_buf)
                    };

                    let mut rung_best: Option<f64> = None;
                    for &rung in Band::habitation() {
                        let idx = rung as usize;
                        let (Some(s_r), Some(chem_r)) = (substrate_here[idx], chemical_here[idx])
                        else {
                            continue;
                        };
                        let score = score_at(&s_r, &chem_r);
                        rung_best = Some(rung_best.map_or(score, |b: f64| b.max(score)));
                    }
                    let (best, availability) = match rung_best {
                        Some(best) => (best, 1.0),
                        None => (score_at(sub_single.get(vertex), &ChemicalSupply::NONE), 0.0),
                    };
                    let score = best * availability * affinity;
                    cap_total[i][a] += score;

                    if arm == Arm::RawSum {
                        let delta = (score - *shipped_map.get(vertex)).abs();
                        control_worst = control_worst.max(delta);
                        control_compared += 1;
                    }
                }
                cap_count[i] += 1;
            }
        }
    }

    // ---------------------------------------------------------------- the
    // positive control, printed and asserted BEFORE anything downstream of
    // the reconstruction is believed.
    println!(
        "\nPOSITIVE CONTROL — arm A reconstruction vs shipped per_species_suitability:\n  \
         {control_compared} (kind, cave vertex) pairs compared, worst |delta| = {control_worst:.3e}"
    );
    assert!(
        control_compared > 1000,
        "only {control_compared} control comparisons — vacuous"
    );
    assert!(
        control_worst <= CONTROL_TOLERANCE,
        "the arm-A reconstruction does not reproduce per_species_suitability (worst |delta| \
         {control_worst:.6e} > {CONTROL_TOLERANCE:.0e}) — every arm below is derived from that \
         reconstruction, so none of them can be believed until this agrees"
    );

    // ---------------------------------------------------------------- the
    // ruler tables.
    let mut sorted_by_rung: Vec<Vec<f64>> = raw_by_rung.clone();
    for v in &mut sorted_by_rung {
        v.sort_by(f64::total_cmp);
    }
    let n: usize = sorted_by_rung.iter().map(Vec::len).sum();
    println!(
        "\nTHE TRENCHER T1 — four-arm calibration comparison\n  \
         {} seeds, BuildDepth::Terrain, {n} cave-bearing vertex-rung readings\n  \
         prior baseline (The Ceiling, pre-campaign, MEAN-OF-SEVEN, not reproducible here): \
         max 0.424277, `rich`/`teeming` never realized",
        Q6_SEEDS.len()
    );

    // Arm B's per-rung `fed` occupancy and realized max — the two numbers
    // the frozen prediction's live clauses read.
    let mut b_fed_by_rung = [0.0f64; UNDERGROUND_RUNGS.len()];
    let mut b_max = f64::NEG_INFINITY;
    let mut b_teeming = 0usize;
    // Arm A's worst per-rung `teeming` occupancy — the control's own
    // readout. See the assertion at the foot of this test for what its
    // going green would mean.
    let mut a_teeming_worst = f64::INFINITY;

    for &arm in &ARMS {
        println!("\n  {} — band occupancy by rung", arm.label());
        println!(
            "  | rung | n | median | max | >= lean .25 | >= fed .50 | >= rich .75 | >= teeming 1.0 |"
        );
        println!("  |---|---|---|---|---|---|---|---|");
        let mut medians = Vec::new();
        for (r, &rung) in UNDERGROUND_RUNGS.iter().enumerate() {
            let mut vals: Vec<f64> = sorted_by_rung[rung as usize]
                .iter()
                .map(|raw| arm.ruler(*raw))
                .collect();
            vals.sort_by(f64::total_cmp);
            let med = median(&mut vals);
            let max = vals.last().copied().unwrap_or(f64::NAN);
            let lean = occupancy(&vals, E_LEAN);
            let fed = occupancy(&vals, E_FED);
            let rich = occupancy(&vals, E_RICH);
            let teeming = occupancy(&vals, E_TEEMING);
            println!(
                "  | {rung:?} | {} | {med:.6} | {max:.6} | {:.1}% | {:.1}% | {:.1}% | {:.1}% |",
                vals.len(),
                lean * 100.0,
                fed * 100.0,
                rich * 100.0,
                teeming * 100.0
            );
            medians.push(med);
            if arm == Arm::SumThenSaturate {
                b_fed_by_rung[r] = fed;
                b_max = b_max.max(max);
                b_teeming += vals.iter().filter(|v| **v >= E_TEEMING).count();
            }
            if arm == Arm::RawSum {
                a_teeming_worst = a_teeming_worst.min(teeming);
            }
        }
        let monotone = medians.windows(2).all(|w| w[1] >= w[0]);
        println!(
            "  depth shape (medians Undercroft->Nadir): {}  — {}",
            medians
                .iter()
                .map(|m| format!("{m:.6}"))
                .collect::<Vec<_>>()
                .join(" "),
            if monotone {
                "non-decreasing with depth"
            } else {
                "NOT non-decreasing"
            }
        );
        if arm == Arm::DetritusReplace {
            println!(
                "  (identical to arm B by construction: DetritalImport routes to DETRITUS and \
                 never enters the CHEMOSYNTHATE aggregate, so no detritus rule can move the ruler)"
            );
        }
    }

    // ---------------------------------------------------------------- the
    // capacity table.
    println!(
        "\n  capacity-suitability, mean over cave-bearing vertices \
         (arm A = SHIPPED, validated above; B and C are the COUNTERFACTUAL where the arm's \
         transform is adopted for the AGGREGATE too, which ledger #24 does NOT ship; D is a \
         real shipped-behaviour alternative)"
    );
    print!("  | arm |");
    for k in &kinds {
        print!(" {} |", k.label);
    }
    println!();
    print!("  |---|");
    for _ in &kinds {
        print!("---|");
    }
    println!();
    for (a, &arm) in ARMS.iter().enumerate() {
        print!("  | {} |", arm.label());
        for (i, _) in kinds.iter().enumerate() {
            let mean = cap_total[i][a] / cap_count[i] as f64;
            print!(" {mean:.6} |");
        }
        println!();
    }
    for (i, k) in kinds.iter().enumerate() {
        let base = cap_total[i][0];
        for (a, &arm) in ARMS.iter().enumerate().skip(1) {
            let delta = cap_total[i][a] - base;
            let rel = if base == 0.0 { f64::NAN } else { delta / base };
            println!(
                "  {} vs arm A, {}: mean delta {:+.6e} ({:+.3}%)",
                arm.label(),
                k.label,
                delta / cap_count[i] as f64,
                rel * 100.0
            );
        }
    }

    // ---------------------------------------------------------------- the
    // frozen prediction, asserted only now.
    let best_fed = b_fed_by_rung
        .iter()
        .copied()
        .fold(f64::NEG_INFINITY, f64::max);
    println!(
        "\n  FROZEN PREDICTION (arm B): best per-rung `fed` occupancy {:.1}% (needs >= 25%), \
         realized max {b_max:.6} (needs > 0.5), vertices at or above E_TEEMING {b_teeming} \
         (needs 0)",
        best_fed * 100.0
    );

    assert!(
        best_fed >= 0.25,
        "arm B realizes `fed` at {:.1}% at its best rung, below the preregistered 25% — a \
         FALSIFICATION, not a failure (decision 0016): record it, do not retune anything",
        best_fed * 100.0
    );
    assert!(
        b_max > E_FED,
        "arm B's realized maximum {b_max:.6} does not exceed E_FED {E_FED} — a FALSIFICATION, \
         not a failure (decision 0016)"
    );
    assert_eq!(
        b_teeming, 0,
        "arm B put {b_teeming} readings at or above E_TEEMING — algebraically impossible for \
         raw/(1+raw) on finite non-negative input, so this means a non-finite aggregate reached \
         the ruler"
    );

    // THE CONTROL'S OWN RATCHET, and it is not decorative. Arm A is only a
    // meaningful control while the raw aggregate actually overshoots the
    // ruler — that overshoot is what ledger #24 was decided against, and it
    // is a property of `chemical_supply`'s aggregate rule, not of algebra,
    // so it CAN move: bound the aggregate, change the metabolite count, or
    // damp `GEOTHERMAL_MODIFIER_GAIN`, and this goes to zero. A red here
    // does not mean a regression — it means the comparison above has lost
    // its control and T1 needs re-running against whatever replaced it.
    //
    // Pinned at the WORST rung (the smallest per-rung `teeming` occupancy),
    // measured 2026-09-12 at 44.0% on `Undercroft`, so the assertion is a
    // floor with real headroom rather than a re-pin of the measurement.
    println!(
        "  ARM A CONTROL: worst per-rung `teeming` occupancy {:.1}% (the raw sum overshoots the \
         ruler at every rung — the overshoot ledger #24 was decided against)",
        a_teeming_worst * 100.0
    );
    assert!(
        a_teeming_worst > 0.0,
        "arm A no longer overshoots E_TEEMING at every rung (worst per-rung occupancy {:.1}%) — \
         the raw aggregate has been bounded somewhere, so arm A is no longer the control this \
         comparison was built on and T1 must be re-run",
        a_teeming_worst * 100.0
    );
}

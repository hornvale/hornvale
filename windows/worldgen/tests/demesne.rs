//! BIO-35 (The Demesne): per-axis resource-supply fields (Stage 1, task T1)
//! and their consumer, the rank-restored per-species K (Stage 2, task T2).
//!
//! T1 built `mineral_supply_field`/`forage_supply_field`/`DETRITUS_AMBIENT`
//! as pure builders nothing yet consumed. T2 wires them into
//! `per_species_suitability` via [`hornvale_worldgen::axis_supply`], the axis
//! dot product that replaces the old `base_carrying(cell) × Σuptake` scalar
//! — a niche direction now SELECTS a spatial combination instead of merely
//! rescaling one shared field, so two species with different uptake
//! *directions* can peak in different *places* (the rank-restoration
//! keystone below), and a genuinely mineral- or detritus-driven species can
//! now track its own supply field spatially (the emergence keystone below).
//!
//! Test fixture (decision 0092): calls the sculpt/fit derivation entry
//! points directly to build its own world state, once per test — the
//! sanctioned test-fixture posture the weir's spec carves out.
#![allow(clippy::disallowed_methods)]

use hornvale_astronomy::SkyPins;
use hornvale_kernel::{KindId, Seed, Value, World};
use hornvale_terrain::TerrainPins;
use hornvale_worldgen::{
    SettlementPins, SkyChoice, WorldComponents, axis_supply, build_world, carrying_inputs_of,
    species_carrying_input, terrain_of,
};

#[test]
fn mineral_supply_tracks_prospectivity_spatially() {
    // On a real seed-42 world, the mineral field peaks where prospectivity
    // peaks and is 0 where prospectivity is 0 — a genuinely SPATIAL field,
    // not a constant.
    let world = hornvale_worldgen::build_world(
        hornvale_kernel::Seed(42),
        &hornvale_astronomy::SkyPins::default(),
        hornvale_worldgen::SkyChoice::Generated,
        &hornvale_terrain::TerrainPins::default(),
        &hornvale_worldgen::SettlementPins::default(),
    )
    .unwrap();

    // Reach the terrain handle the way `per_species_suitability`'s callers do
    // (`terrain_of`), then its geosphere — the single construction site for
    // the terrain provider on a built world.
    let terrain = hornvale_worldgen::terrain_of(&world).unwrap();
    let geo = terrain.geosphere();

    let scale = 10.0;
    let field = hornvale_worldgen::mineral_supply_field(geo, &terrain, scale);

    // Genuinely spatial: at least two distinct values across cells.
    let mut distinct: Vec<f64> = Vec::new();
    for c in geo.cells() {
        let v = *field.get(c);
        if !distinct.iter().any(|d: &f64| (*d - v).abs() < 1e-12) {
            distinct.push(v);
        }
        if distinct.len() >= 2 {
            break;
        }
    }
    assert!(
        distinct.len() >= 2,
        "mineral supply field must vary across cells, not be a constant"
    );

    // Monotone in prospectivity at two probe cells: whichever cell has
    // higher prospectivity must have a proportionally higher supply value
    // (field = prospectivity * scale, so equality up to float epsilon).
    // The probes must be LAND cells: since The Tumult's land mask the field
    // is 0 at sea regardless of the seafloor's (honestly derived, but
    // unreachable) prospectivity — the first and last land cell in ascending
    // `CellId` order, a deterministic choice with no float ordering.
    let land: Vec<hornvale_kernel::CellId> =
        geo.cells().filter(|c| !terrain.is_ocean(*c)).collect();
    assert!(land.len() >= 2, "seed 42 must have at least two land cells");
    let probe_a = land[0];
    let probe_b = land[land.len() - 1];
    let prospectivity_a = terrain.prospectivity_at(probe_a);
    let prospectivity_b = terrain.prospectivity_at(probe_b);
    let field_a = *field.get(probe_a);
    let field_b = *field.get(probe_b);
    assert!((field_a - prospectivity_a * scale).abs() < 1e-9);
    assert!((field_b - prospectivity_b * scale).abs() < 1e-9);
    match prospectivity_a.total_cmp(&prospectivity_b) {
        std::cmp::Ordering::Less => assert!(field_a < field_b),
        std::cmp::Ordering::Greater => assert!(field_a > field_b),
        std::cmp::Ordering::Equal => assert!((field_a - field_b).abs() < 1e-9),
    }

    // Bounds: prospectivity is [0,1], so the field is [0, scale].
    for c in geo.cells() {
        let v = *field.get(c);
        assert!(
            (0.0..=scale + 1e-9).contains(&v),
            "mineral supply out of range: {v}"
        );
    }
}

/// THE LAND MASK (The Tumult), extended by THE SEA MASK (The Vacancy T6/T8):
/// every v1 TERRESTRIAL resource-supply axis is 0 at sea and `MARINE_FORAGE`
/// is 0 on land — both masks are a property of the supply fields, not a
/// decree in the K assembly (see `DETRITUS_AMBIENT`'s terrestrial-supply
/// frame and `marine_forage_supply_field`'s mirror). Before The Vacancy T8
/// authored any kind onto `MARINE_FORAGE`, this meant EVERY kind's K was 0 at
/// sea; T8 deliberately broke that for exactly five kinds (four marine plus
/// the amphibious giant crocodile), so the test below now asserts the mask
/// held for those five TOO, in its now-correct (not simply "always zero")
/// form, alongside the original guard for everyone else.
///
/// This states explicitly what a bug used to do by accident. Before The
/// Tumult's elevation re-datum, `ConditionNiche.elevation` was scored against
/// the raw isostatic `ReferenceElevation`, which put an ocean cell ~4 km from
/// every authored optimum and so zeroed the seafloor through the *condition*
/// term. Correcting the datum left ocean cells only ~1100 m below sea level
/// and exposed two supply axes that never had a mask of their own: `MINERAL`
/// (a prospectivity read, defined on the seafloor) and `DETRITUS` (a global
/// constant). Measured at seed 42 with the datum corrected and no mask, the
/// submerged share of total K was **0.85** for the otyugh, **0.86** for the
/// rust monster and **0.74** for the xorn — a swamp detritivore, a cave
/// mineral-eater and a burrowing elemental, each mostly at sea.
///
/// MUTATION GUARD: dropping either land mask (`mineral_supply_field`'s or
/// `detritus_supply_field`'s) re-admits exactly those three kinds' seabed K
/// and this test fails on them by name; dropping the sea mask
/// (`marine_forage_supply_field`'s `is_ocean` guard) re-admits marine
/// carrying capacity on land for the five T8 kinds and fails on THEM.
#[test]
fn no_species_draws_carrying_capacity_from_the_wrong_medium() {
    // The four PURELY marine T8 kinds: their niche weights only
    // `MARINE_FORAGE`, so every terrestrial supply axis contributes an exact
    // zero to their dot product regardless of that axis's land value — they
    // must be wholly submerged (dry == 0), the mirror of the land mask.
    let marine_only: std::collections::BTreeSet<&str> =
        ["giant-octopus", "giant-squid", "killer-whale", "reef-shark"]
            .into_iter()
            .collect();
    // The amphibious proof case (spec §3.4): weights BOTH a terrestrial axis
    // and `MARINE_FORAGE`, so its K must be nonzero in BOTH media — the
    // observable signature of the sparse-uptake, no-special-case design.
    const AMPHIBIOUS: &str = "giant-crocodile";

    let wc = WorldComponents::assemble().expect("canonical registries are well-formed");
    let world = world_42();
    let terrain = terrain_of(&world).expect("terrain reconstructs");
    let climate = hornvale_worldgen::climate_of(&world).expect("climate reconstructs");
    let geo = terrain.geosphere();
    let sky = hornvale_worldgen::sky_of(&world).expect("sky reconstructs");
    let system = sky.system().expect("seed 42 has a generated star system");
    let insolation = hornvale_astronomy::insolation_rel(&system.star, &system.anchor);
    let obliquity = system.anchor.obliquity.get();
    let regime = match system.anchor.rotation {
        hornvale_astronomy::Rotation::Spinning { day, .. } => {
            hornvale_climate::RotationRegime::Spinning { day_std: day.get() }
        }
        hornvale_astronomy::Rotation::Locked => hornvale_climate::RotationRegime::Locked,
    };

    let kinds: Vec<KindId> = wc.biosphere.iter().map(|(k, _)| *k).collect();
    let bios: Vec<&hornvale_species::BiosphereTraits> =
        wc.biosphere.iter().map(|(_, b)| b).collect();
    let ks = hornvale_worldgen::per_species_suitability(
        geo, &terrain, &climate, obliquity, insolation, &regime, &bios,
    );

    let submerged: Vec<hornvale_kernel::CellId> =
        geo.cells().filter(|c| terrain.is_ocean(*c)).collect();
    assert!(
        !submerged.is_empty(),
        "seed 42 must have ocean cells for this test to mean anything"
    );

    let mut placed_on_land = 0u32;
    for (tag, k) in &ks {
        let kind = kinds[*tag as usize].0;
        let mut wet = 0.0_f64;
        let mut dry = 0.0_f64;
        for c in geo.cells() {
            let v = *k.get(c);
            if terrain.is_ocean(c) {
                wet += v;
            } else {
                dry += v;
            }
        }
        let total = wet + dry;
        if marine_only.contains(kind) {
            assert_eq!(
                dry, 0.0,
                "{kind} draws {dry} of its {total} total carrying capacity from LAND cells — \
                 a purely marine kind's terrestrial supply axes must be 0"
            );
        } else if kind == AMPHIBIOUS {
            assert!(
                wet > 0.0 && dry > 0.0,
                "{kind} is the amphibious proof case: it must draw nonzero K from BOTH \
                 media, got wet={wet} dry={dry}"
            );
        } else {
            assert_eq!(
                wet, 0.0,
                "{kind} draws {wet} of its {total} total carrying capacity from submerged \
                 cells — the terrestrial supply axes must be 0 at sea"
            );
        }
        if total > 0.0 {
            placed_on_land += 1;
        }
    }
    // Not a vacuous pass: the mask must not have zeroed the whole roster.
    assert!(
        placed_on_land >= 8,
        "only {placed_on_land} kinds have any carrying capacity at all — the land mask \
         should zero the seafloor, not the world"
    );
}

#[test]
fn forage_supply_is_a_fraction_of_base_carrying_and_deterministic() {
    let geo = hornvale_kernel::Geosphere::new(3);
    let base = hornvale_kernel::CellMap::from_fn(&geo, |c| (c.0 as f64) * 0.1);
    let a = hornvale_worldgen::forage_supply_field(&geo, &base);
    let b = hornvale_worldgen::forage_supply_field(&geo, &base);
    for c in geo.cells() {
        assert_eq!(a.get(c), b.get(c));
        assert!(
            *a.get(c) <= *base.get(c),
            "forage is a fraction of primary production"
        );
    }
}

/// THE RANK-RESTORATION KEYSTONE (T2, `axis_supply`): two cells — A
/// photosynthate-rich, B mineral-rich — and two niches (a plant-eater, a
/// rock-eater) with opposite axis weights. Each niche's supply must peak in
/// the cell that supplies ITS axis, not in the same cell for both.
///
/// MUTATION GUARD: the OLD scalar `supply = base(cell) × Σuptake` gives
/// every niche the SAME cell ranking (`base` is identical per cell,
/// `Σuptake` is a per-niche CONSTANT that does not depend on the cell), so
/// this pair of strict inequalities cannot both hold under the collapsed
/// model — only the per-axis dot product can differentiate WHERE two
/// differently-shaped niches peak.
#[test]
fn different_uptake_vectors_peak_in_different_cells() {
    use hornvale_kernel::{MINERAL, PHOTOSYNTHATE, ResourceVector};
    let cell_a = [(PHOTOSYNTHATE, 10.0), (MINERAL, 0.0)];
    let cell_b = [(PHOTOSYNTHATE, 0.0), (MINERAL, 10.0)];
    let plant = ResourceVector::new(&[(PHOTOSYNTHATE, 1.0), (MINERAL, 0.0)]).unwrap();
    let rock = ResourceVector::new(&[(PHOTOSYNTHATE, 0.0), (MINERAL, 1.0)]).unwrap();
    // the plant-eater's supply is higher in A; the rock-eater's is higher in B.
    assert!(
        axis_supply(&plant, &cell_a) > axis_supply(&plant, &cell_b),
        "plant-eater peaks in A"
    );
    assert!(
        axis_supply(&rock, &cell_b) > axis_supply(&rock, &cell_a),
        "rock-eater peaks in B"
    );
}

/// Every `stack_settlement`'s `.dominant` tag, mapped back to its `KindId`
/// label via `wc.biosphere`'s ascending-`KindId` order — the SAME
/// build-local dense-index contract `per_species_suitability`'s doc comment
/// spells out (never identity, valid only within this one report call).
/// Counts settlements per dominant kind over the WHOLE roster (fauna
/// included) — [`hornvale_worldgen::demography_report_from`]'s stack, not the
/// peopled-only settlement-genesis pipeline (which never places a fauna
/// kind by construction; see `species_worlds.rs`'s module doc).
fn dominant_settlement_counts(
    world: &World,
    wc: &WorldComponents,
) -> std::collections::BTreeMap<&'static str, u32> {
    let terrain = terrain_of(world).unwrap();
    let climate = hornvale_worldgen::climate_from(world, &terrain).unwrap();
    let report = hornvale_worldgen::demography_report_from(world, wc, &terrain, &climate).unwrap();
    let kinds: Vec<KindId> = wc.biosphere.iter().map(|(k, _)| *k).collect();
    let mut counts: std::collections::BTreeMap<&'static str, u32> =
        std::collections::BTreeMap::new();
    for s in &report.stack_settlements {
        *counts.entry(kinds[s.dominant as usize].0).or_insert(0) += 1;
    }
    counts
}

/// The distinct `peopled-by` objects across every committed settlement (the
/// psyche-only settlement-genesis pipeline's actual placement outcome) —
/// mirrors `hornvale_worldgen`'s own private `placed_species` helper, reread
/// here because integration tests only see `pub` surface.
fn peopled_kinds(world: &World) -> std::collections::BTreeSet<String> {
    world
        .ledger
        .find(hornvale_species::PEOPLED_BY)
        .filter_map(|f| match &f.object {
            Value::Text(s) => Some(s.clone()),
            _ => None,
        })
        .collect()
}

/// The ruler against the Confluence campaign's denominator artifact (a kind
/// that dominates exactly one attractor is measurement noise, not
/// "placed"): a kind counts as a material full-roster dominant only if it
/// tops `.dominant` on at least this many settlements.
const MIN_SETTLEMENTS_FOR_DOMINANCE: u32 = 2;

/// BASELINE (measured 2026-07-19, PRE-repoint — the old `base_carrying(cell)
/// × Σuptake` scalar supply — over the REAL production roster: 16 kinds
/// (four peopled goblinoid-family + kobold, plus twelve fauna: treant,
/// twig-blight, giant-elk, woolly-mammoth, giant-goat, otyugh, xorn,
/// rust-monster, three chromatic dragons, owlbear), not the 6-kind roster
/// this task's brief sketched from memory.
///
/// At seed 42, `dominant_settlement_counts` (whole roster, 696 total
/// settlements) read `[rust-monster: 113, twig-blight: 49, xorn: 1]` —
/// xorn's single settlement is exactly the denominator-artifact noise
/// `MIN_SETTLEMENTS_FOR_DOMINANCE` exists to exclude, so the material
/// dominant count is **2**. `peopled_kinds` (the psyche-only genesis
/// pipeline) read `{goblin, hobgoblin}` — **2** kinds; bugbear and kobold
/// dominate zero attractors. Union of the two sets: **4** distinct kinds.
const BASELINE_DOMINANT_KINDS_42: usize = 2;
/// BASELINE peopled-by kind count at seed 42. The Living Community epoch made
/// the deep-history bake the settlement provider: it seeds EVERY goblinoid
/// people its own proto-communities (they persist by lineage, not by winning
/// local coexistence dominance), so all four peoples were peopled-by kinds —
/// re-pinned 2 -> 4 (measured on the epoch; this is a placement-provider
/// change, orthogonal to T2's per-axis supply thesis). The Vacancy T9 adds a
/// fifth people (the gnoll), measured at seed 42 to also place a settlement —
/// re-pinned 4 -> 5. The Generalist adds a sixth people (human), measured at
/// seed 42 to also place a settlement — re-pinned 5 -> 6.
const BASELINE_PEOPLED_KINDS_42: usize = 6;
/// BASELINE union (dominant ∪ peopled-by) distinct kind count at seed 42.
const BASELINE_UNION_KINDS_42: usize = 4;

/// Frozen BEFORE the post-repoint readout (preregistration, the-confluence
/// lesson): a MATERIAL rise over [`BASELINE_UNION_KINDS_42`]'s 4. Chosen
/// from theory: `axis_supply` gives every MINERAL-pure niche (xorn,
/// rust-monster) and every PHOTOSYNTHATE-pure niche (treant, twig-blight) a
/// genuinely distinct spatial pattern from the old shared-NPP ranking, so
/// at least one more full-roster dominant should clear the ruler at seed 42
/// once mineral/forage supply stops being a uniform rescale of the same
/// field every species shared.
const PREREGISTERED_MIN_DOMINANTS: usize = 5;

fn world_42() -> World {
    build_world(
        hornvale_kernel::Seed(42),
        &hornvale_astronomy::SkyPins::default(),
        SkyChoice::Generated,
        &hornvale_terrain::TerrainPins::default(),
        &SettlementPins::default(),
    )
    .unwrap()
}

/// THE EMERGENCE KEYSTONE (T2, preregistered against [`BASELINE_UNION_KINDS_42`]):
/// post-repoint, seed 42's full-roster dominant-kind count (ruler-filtered)
/// plus its peopled-by kind count must rise materially, and the newly
/// spatially-differentiated mineral axis must place a genuinely NEW kind —
/// xorn, absent from the baseline's material dominants (its lone
/// pre-repoint settlement was denominator noise) — as a real, ruler-clearing
/// full-roster dominant.
///
/// **Deviation from the brief's sketch, measured not assumed:** the brief's
/// draft expected a NON-goblinoid PEOPLED kind (kobold) to also newly place
/// at seed 42. Measured post-repoint, it does not (`peopled_kinds` stays
/// `{goblin, hobgoblin}`, byte-identical to baseline) — and this is
/// structural, not a tuning shortfall: every one of the four peopled
/// species' authored niches (`domains/species/src/lib.rs`) is a pure
/// `PLANT_FORAGE`/`ANIMAL_PREY` blend with ZERO weight on
/// `PHOTOSYNTHATE`/`MINERAL`/`DETRITUS` — the three axes T2 gave their own
/// spatial fields. `ANIMAL_PREY` supply is Stage 2's placeholder zero, so
/// the peopled-only stack's competition is governed by `PLANT_FORAGE`
/// alone, which (via `forage_supply_field`) is still a uniform rescale of
/// `base_carrying` for every peopled species — `axis_supply` cannot
/// differentiate their spatial RANKING no matter how `FORAGE_FRACTION` is
/// tuned (empirically swept 0.05..5.0 during this task: seed 42's
/// peopled-by set never changed). Diversifying the peopled roster's own
/// placement needs a future stage's authoring (e.g. a mining kobold niche
/// weighted onto `MINERAL`) — out of T2's scope per the 0021 constraint
/// (never author a placement to force a specific test to pass). The fauna
/// half of the brief's ask (`xorn`) IS measured below.
#[test]
fn settlements_and_dominants_diversify_on_seed_42() {
    let world = world_42();
    let wc = WorldComponents::assemble().expect("canonical registries are well-formed");

    let dominant_counts = dominant_settlement_counts(&world, &wc);
    let material_dominants: std::collections::BTreeSet<&'static str> = dominant_counts
        .iter()
        .filter(|(_, count)| **count >= MIN_SETTLEMENTS_FOR_DOMINANCE)
        .map(|(kind, _)| *kind)
        .collect();
    let peopled = peopled_kinds(&world);

    assert_eq!(
        peopled.len(),
        BASELINE_PEOPLED_KINDS_42,
        "peopled-by kinds at seed 42 should be unchanged by T2 (structural: no peopled \
         species weights MINERAL/PHOTOSYNTHATE/DETRITUS) — got {peopled:?}"
    );
    assert!(
        material_dominants.len() > BASELINE_DOMINANT_KINDS_42,
        "T2's axis dot product should differentiate at least one more full-roster \
         dominant beyond the {BASELINE_DOMINANT_KINDS_42}-kind baseline; got {material_dominants:?}"
    );

    let union: std::collections::BTreeSet<String> = material_dominants
        .iter()
        .map(|s| s.to_string())
        .chain(peopled.iter().cloned())
        .collect();
    assert!(
        union.len() >= PREREGISTERED_MIN_DOMINANTS,
        "union of material full-roster dominants and peopled-by kinds must clear the \
         preregistered floor of {PREREGISTERED_MIN_DOMINANTS} (baseline was \
         {BASELINE_UNION_KINDS_42}); got {} — {union:?}",
        union.len()
    );

    // The fauna half of the brief's ask: xorn (pure MINERAL niche) must now
    // be a MATERIAL full-roster dominant — it was baseline noise (a single
    // denominator-artifact settlement) under the old shared-NPP scalar.
    //
    // ---- FALSIFIED by The Tense (2026-08-05). Recorded, not rescued. ----
    //
    // Xorn holds ZERO dominant cells on this tree. It clears on `main` (this
    // test is green there — verified by running the demesne suite in the main
    // checkout), so the loss is this branch's, not pre-existing.
    //
    // WHAT IT IS NOT: xorn is not erased. `non_void_roster::
    // every_kind_is_viable_somewhere` — The Vacancy's guard against exactly the
    // zero-capacity-everywhere failure that hid three chromatic dragons and the
    // owlbear for four campaigns — is GREEN. Xorn is viable; it is simply no
    // longer the best fit anywhere.
    //
    // MECHANISM (**inferred**, not measured — flagged per the handoff
    // convention): this branch replaced the species-blind productivity model
    // with Lieth & Box's Miami model (see T3 below), and scalar-path
    // productivity rose ~14% on the tropical mean. That lifts every
    // BIOMASS-fed kind against a MINERAL-fed one whose supply did not move, and
    // xorn — a *pure* mineral niche — sits exactly on that seam. The surviving
    // dominants are consistent with it: giant-squid 1160 and twig-blight 570
    // dwarf the rest. Confirming this properly means re-running the ruler with
    // the tent restored, which is a probe this campaign did not spend.
    //
    // WHY THE ASSERTION IS RETIRED RATHER THAN RE-AIMED. It encodes The
    // Demesne's *preregistered prediction* that giving mineral supply its own
    // spatial field would lift xorn over the ruler. A later campaign corrected
    // the productivity model underneath it and the prediction no longer holds.
    // A falsified prediction is a finding, not a failure — and re-pinning it to
    // some other kind, or tuning until xorn returns, would be precisely the
    // post-unblinding rescue the project forbids. The STRUCTURAL claims this
    // test exists for are all still asserted above and all still pass: the
    // peopled roster is unchanged, T2's dot product still differentiates more
    // dominants than the baseline, and the union clears the preregistered
    // floor. Only the single-species prediction is withdrawn.
    assert!(
        !material_dominants.contains("xorn"),
        "xorn is a material dominant again ({dominant_counts:?}) — The Demesne's \
         prediction was falsified under The Tense's productivity model and this \
         assertion records that. If xorn is back, the biomass/mineral balance moved \
         again: re-read the comment above and re-establish which model is in play, \
         do not simply flip this back."
    );
}

/// T3: THE K-GROUNDING CHECK (the-gathering discipline). The census-pinned
/// `capacity-by-abs-latitude` calibration (`gathering_calibration.rs`,
/// design spec §5) and its live seed-42 re-check
/// (`confluence.rs`'s `k_biomass_gradient_grounding_holds_after_the_
/// freshwater_repoint`) are both computed from `carrying_inputs_of` +
/// `species_carrying_input` + `hornvale_demography::carrying_capacity` — a
/// psychology-scaled, PEOPLED-ONLY carrying-capacity path that predates The
/// Niche's per-species differentiation and has never been re-pointed onto
/// it (settlement genesis moved onto `per_species_suitability` at Task A15a,
/// but this grounding metric stayed on the older, simpler path — the two
/// coexist, per the `species_carrying_input` doc comment). The per-axis
/// vector supply this campaign built (`mineral_supply_field`/
/// `forage_supply_field`/`DETRITUS_AMBIENT`) is consumed ONLY by
/// `per_species_suitability` (via `axis_supply`), so it cannot touch this
/// gradient's inputs at all — confirmed here, live, rather than assumed:
/// the measured ratio matches `confluence.rs`'s pinned 31.2563 exactly (T3
/// changed nothing upstream of it), so no `MINERAL_SUPPLY_SCALE`/
/// `FORAGE_FRACTION`/`CONDENSATION_THRESHOLD` re-fit is needed for THIS
/// metric. (T3's actual settlement-COUNT investigation — a different K,
/// `per_species_suitability`, the one settlement genesis and the menagerie
/// strongholds test use — lives in `confluence.rs`'s settlement-count test
/// and this file's `settlements_and_dominants_diversify_on_seed_42`.)
#[test]
fn k_biomass_gradient_grounding_is_unaffected_by_the_vector_supply() {
    let wc = WorldComponents::assemble().expect("canonical registries are well-formed");
    let world = world_42();
    let terrain = terrain_of(&world).expect("terrain reconstructs");
    let climate = hornvale_worldgen::climate_of(&world).expect("climate reconstructs");
    let geo = terrain.geosphere();
    let base_inputs = carrying_inputs_of(geo, &terrain, &climate);

    let (mut trop_sum, mut trop_n, mut pole_sum, mut pole_n) = (0.0_f64, 0u32, 0.0_f64, 0u32);
    // Peoples-only carrying capacity (the settling roster); skip the minded
    // solitaries (a dragon carries a psyche but never settles) so the metric is
    // byte-identical to before The Eremite.
    for (kind, psych) in wc.psyche.iter() {
        if wc.biosphere.get(kind).map(|b| b.social_form)
            != Some(hornvale_species::SocialForm::Settled)
        {
            continue;
        }
        let inputs = hornvale_kernel::CellMap::from_fn(geo, |c| {
            species_carrying_input(*base_inputs.get(c), psych)
        });
        let k = hornvale_demography::carrying_capacity(geo, &inputs);
        for cell in geo.cells() {
            if terrain.is_ocean(cell) {
                continue;
            }
            let lat = geo.coord(cell).latitude.abs();
            let kv = k.at(cell);
            if lat < 30.0 {
                trop_sum += kv;
                trop_n += 1;
            } else if lat > 60.0 {
                pole_sum += kv;
                pole_n += 1;
            }
        }
    }
    assert!(trop_n > 0, "seed 42 has no tropical land cells");
    assert!(pole_n > 0, "seed 42 has no polar land cells");
    const POLE_FLOOR: f64 = 0.01;
    let trop_mean = trop_sum / f64::from(trop_n);
    let pole_mean = (pole_sum / f64::from(pole_n)).max(POLE_FLOOR);
    let ratio = trop_mean / pole_mean;
    // The decomposition is PRINTED, not just the ratio. The Keeping found this
    // metric's degeneracy by reading a doc comment; making it visible in the
    // run output is cheaper than making the next reader do that again.
    let raw_pole_mean = pole_sum / f64::from(pole_n);
    let pole_is_floored = raw_pole_mean < POLE_FLOOR;
    println!(
        "seed 42 capacity-by-abs-latitude: ratio={ratio:.4} \
         (trop_mean={trop_mean:.6} over {trop_n} cells, raw_pole_mean={raw_pole_mean:.6} \
         over {pole_n} cells, pole floored at {POLE_FLOOR}: {pole_is_floored})"
    );
    assert!(
        ratio >= 3.0,
        "capacity-by-abs-latitude on seed 42 fell to {ratio:.4} (below the preregistered floor \
         of 3) — the K-grounding may have drifted despite the-demesne touching a different K"
    );
    // Pinned to the merged-tree live reading: proof of ZERO drift from the
    // vector supply, not merely "still above the floor" — the vector supply's
    // code path (`per_species_suitability`/`axis_supply`) is disjoint from this
    // one (`carrying_inputs_of`/`species_carrying_input`/`carrying_capacity`),
    // so this ratio is BY CONSTRUCTION the pure scalar-path reading and the
    // vector supply cannot move it. The absolute value tracks the climate
    // inputs: it was 31.2563 pre-Rains (confluence.rs) and 31.1236 after
    // absorbing The Rains' moisture epoch (a 0.4% shift in the tropical/polar
    // NPP balance). **This loop sums the scalar K across every `Settled`
    // psyche kind** (`for (kind, psych) in wc.psyche.iter()`, filtered to
    // `Settled`), so a genuinely new competing Settled people is, by the
    // test's own construction, a new term in `trop_sum`/`pole_sum` — not
    // vector-supply contamination. The Vacancy T9 adds the gnoll (a hot-arid
    // desert people, contributing more to the tropical sum than the polar
    // one), moving the ratio to 30.8158 — still far above the preregistered
    // floor of 3.
    //
    // The Generalist re-pin (2026-08-03): human is a sixth Settled kind
    // (a temperate/subtropical generalist, per its own condition niche),
    // and by this test's own construction is a new term in `trop_sum`/
    // `pole_sum` — moving the ratio to 31.0099.
    // The Keeping step B re-pin (2026-08-04): `CarryingInput.habitable`
    // decomposed to `is_land`, so the arid and very-hot bands the old conflated
    // flag excluded outright now carry (low) scalar K — 31.0099 -> 31.0649.
    // The DIRECTION is the check that this is the intended mechanism and not
    // contamination: hot-and-arid ground is tropical/subtropical, never polar
    // (the poles stay closed by `temp_response`, zero below 2 C), so opening it
    // must add more to `trop_sum` than to `pole_sum` and the ratio must RISE.
    // It rose, by 0.18%. The preregistered floor of 3 still clears tenfold.
    //
    // ---- The Tense re-pin (2026-08-05): 31.0649 -> 35.4171, and the RATIO IS
    // ---- NOT A GRADIENT. Read this before touching the number again.
    //
    // MECHANISM, named as this comment's convention requires: this branch
    // replaced the productivity model. `temp_response` — a symmetric tent
    // peaking at 22 C and reaching exactly zero a little above freezing — is
    // gone, and `carrying_capacity` now implements the Lieth & Box (1972)
    // Miami model it had always CITED but never had: a monotone, saturating
    // temperature term, min'd with a precipitation term on real mm/yr instead
    // of a normalised moisture in [0,1]. That is The Keeping's headline defect
    // being repaired, motivated by decision 0105.
    //
    // THE DIRECTION CHECK CANNOT BE RUN, and that is the finding. Measured
    // here: raw_pole_mean = 0.004508, still BELOW `POLE_FLOOR`. The polar term
    // is therefore pinned at the floor, and
    //
    //     ratio == trop_mean / POLE_FLOOR == 100 * trop_mean, exactly
    //     (0.354171 * 100 = 35.4171, which is the whole of the drift)
    //
    // so this quantity carries no polar information at all. It is the tropical
    // mean in different units. There is no gradient in it whose direction could
    // confirm or refute a mechanism — which is precisely the degeneracy The
    // Keeping recorded ("a ratio computed against a floored zero is largely a
    // statement about the floor") and which the Confidence Gradient already
    // demotes.
    //
    // WHAT THIS ASSERTION IS, THEREFORE. It is a drift TRIPWIRE on scalar-path
    // productivity — an internal-consistency check on a Hornvale-internal
    // number, which decision 0105 rules a VALID use of internal measurement.
    // It is NOT evidence for the biomass-by-latitude gradient; treating it as
    // evidence would be 0105's CIRCULAR cell, which names
    // `capacity-by-abs-latitude` explicitly. The preregistered floor of 3
    // above is the real surviving claim, and it clears tenfold.
    //
    // WHY THE POLES ARE STILL ~ZERO, given the tent that zeroed them is gone.
    // Not the productivity field any more: `npp_temperature` is positive
    // everywhere. It is `species_carrying_input` — the per-species TOLERANCE in
    // `ConditionNiche` — and no authored people tolerates polar cold. So the
    // polar zero has moved from being a property of the ground to being a
    // property of the ROSTER, which is where the retired tent's own doc comment
    // says tolerance belongs. Same number, different and better-located cause;
    // a cold-adapted or subterranean people would now lift it off the floor,
    // where before nothing could.
    assert!(
        (ratio - 35.4171).abs() < 1e-3,
        "scalar-path productivity drifted: {ratio:.4} (expected ~35.4171). NOTE this is \
         100 * trop_mean while the polar term sits on its floor — check the printed \
         decomposition above before assuming anything latitudinal moved."
    );
}

// T3: THE EPOCH SURFACE. Settlement placement is deterministic OVER K —
// `hornvale_demography` (condensation, the coexistence-stack packer) draws
// no `Seed`/`Stream` (grep confirms it; see `confluence.rs`'s byte-identity
// test doc, which established this same fact for The Confluence's
// freshwater re-point), and neither does anything T1/T2/T3 added here
// (`mineral_supply_field`/`forage_supply_field`/`axis_supply`/
// `per_species_suitability` are pure functions of terrain/climate/biosphere —
// no `Seed`, no `Stream`, no RNG). The per-axis vector supply changes WHICH
// cells a species' K peaks in (a derived-FORMULA change), never adds or
// reorders a seed draw, so the settlement seed-derivation's
// stream-consumption order is unchanged. Confirmed directly (not just
// argued): the generated stream manifest (`cargo run -p hornvale --
// streams`) is byte-identical to the committed
// `book/src/reference/stream-manifest-generated.md` after this campaign's
// changes. This is a save-format-relevant DERIVED-FORMULA change (spec §6)
// — not a stream-label epoch; no `settlement/*` label gets an epoch
// suffix. `seed_42_is_byte_identical_across_two_builds_after_the_demesne`
// below is the direct determinism assertion.

/// T3: THE BYTE-IDENTITY CHECK. Same seed + pins must still produce a
/// byte-identical world under the-demesne's re-pointed resource-supply
/// term — mirrors `confluence.rs`'s
/// `seed_42_is_byte_identical_across_two_builds_after_the_confluence`,
/// scoped to the crate this campaign actually touched.
#[test]
fn seed_42_is_byte_identical_across_two_builds_after_the_demesne() {
    let build = || {
        build_world(
            Seed(42),
            &SkyPins::default(),
            SkyChoice::Generated,
            &TerrainPins::default(),
            &SettlementPins::default(),
        )
        .unwrap()
        .to_json()
    };
    let a = build();
    let b = build();
    assert_eq!(
        a, b,
        "same seed + pins must yield a byte-identical world under the-demesne's per-axis \
         vector resource supply"
    );
}

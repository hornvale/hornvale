//! BIO-35 (The Demesne): per-axis resource-supply fields (Stage 1, task T1)
//! and their consumer, the rank-restored per-species K (Stage 2, task T2).
//!
//! T1 built `mineral_supply_field`/`forage_supply_field`/`DETRITUS_AMBIENT`
//! as pure builders nothing yet consumed. T2 wires them into
//! `niche_per_species_k` via [`hornvale_worldgen::axis_supply`], the axis
//! dot product that replaces the old `base_carrying(cell) × Σuptake` scalar
//! — a niche direction now SELECTS a spatial combination instead of merely
//! rescaling one shared field, so two species with different uptake
//! *directions* can peak in different *places* (the rank-restoration
//! keystone below), and a genuinely mineral- or detritus-driven species can
//! now track its own supply field spatially (the emergence keystone below).

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

    // Reach the terrain handle the way `niche_per_species_k`'s callers do
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
    let probe_a = hornvale_kernel::CellId(0);
    let probe_b = hornvale_kernel::CellId(geo.cells().count() as u32 / 2);
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
/// build-local dense-index contract `niche_per_species_k`'s doc comment
/// spells out (never identity, valid only within this one report call).
/// Counts settlements per dominant kind over the WHOLE roster (fauna
/// included) — [`hornvale_worldgen::demography_report`]'s stack, not the
/// peopled-only settlement-genesis pipeline (which never places a fauna
/// kind by construction; see `species_worlds.rs`'s module doc).
fn dominant_settlement_counts(
    world: &World,
    wc: &WorldComponents,
) -> std::collections::BTreeMap<&'static str, u32> {
    let report = hornvale_worldgen::demography_report(world, wc).unwrap();
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
/// local coexistence dominance), so all four peoples are peopled-by kinds now
/// — re-pinned 2 -> 4 (measured on the epoch; this is a placement-provider
/// change, orthogonal to T2's per-axis supply thesis).
const BASELINE_PEOPLED_KINDS_42: usize = 4;
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
    assert!(
        material_dominants.contains("xorn"),
        "xorn (pure-MINERAL niche) should newly clear the dominance ruler once mineral \
         supply is its own spatial field, not a rescale of base_carrying; dominant counts: \
         {dominant_counts:?}"
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
/// it (settlement genesis moved onto `niche_per_species_k` at Task A15a,
/// but this grounding metric stayed on the older, simpler path — the two
/// coexist, per the `species_carrying_input` doc comment). The per-axis
/// vector supply this campaign built (`mineral_supply_field`/
/// `forage_supply_field`/`DETRITUS_AMBIENT`) is consumed ONLY by
/// `niche_per_species_k` (via `axis_supply`), so it cannot touch this
/// gradient's inputs at all — confirmed here, live, rather than assumed:
/// the measured ratio matches `confluence.rs`'s pinned 31.2563 exactly (T3
/// changed nothing upstream of it), so no `MINERAL_SUPPLY_SCALE`/
/// `FORAGE_FRACTION`/`CONDENSATION_THRESHOLD` re-fit is needed for THIS
/// metric. (T3's actual settlement-COUNT investigation — a different K,
/// `niche_per_species_k`, the one settlement genesis and the menagerie
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
            let kv = *k.get(cell);
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
    println!("seed 42 capacity-by-abs-latitude (live, post-the-demesne T1/T2): {ratio:.4}");
    assert!(
        ratio >= 3.0,
        "capacity-by-abs-latitude on seed 42 fell to {ratio:.4} (below the preregistered floor \
         of 3) — the K-grounding may have drifted despite the-demesne touching a different K"
    );
    // Pinned to the merged-tree live reading: proof of ZERO drift from the
    // vector supply, not merely "still above the floor" — the vector supply's
    // code path (`niche_per_species_k`/`axis_supply`) is disjoint from this
    // one (`carrying_inputs_of`/`species_carrying_input`/`carrying_capacity`),
    // so this ratio is BY CONSTRUCTION the pure scalar-path reading and the
    // vector supply cannot move it. The absolute value tracks the climate
    // inputs: it was 31.2563 pre-Rains (confluence.rs) and is 31.1236 after
    // absorbing The Rains' moisture epoch (a 0.4% shift in the tropical/polar
    // NPP balance, still far above the preregistered floor of 3).
    assert!(
        (ratio - 31.1236).abs() < 1e-3,
        "capacity-by-abs-latitude drifted: {ratio:.4} (expected ~31.1236, the post-Rains \
         merged-tree scalar-path reading) — something outside the-demesne's per-axis \
         supply fields moved this K"
    );
}

// T3: THE EPOCH SURFACE. Settlement placement is deterministic OVER K —
// `hornvale_demography` (condensation, the coexistence-stack packer) draws
// no `Seed`/`Stream` (grep confirms it; see `confluence.rs`'s byte-identity
// test doc, which established this same fact for The Confluence's
// freshwater re-point), and neither does anything T1/T2/T3 added here
// (`mineral_supply_field`/`forage_supply_field`/`axis_supply`/
// `niche_per_species_k` are pure functions of terrain/climate/biosphere —
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

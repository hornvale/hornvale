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

use hornvale_kernel::{KindId, Value, World};
use hornvale_worldgen::{SettlementPins, SkyChoice, WorldComponents, axis_supply, build_world};

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
/// BASELINE peopled-by kind count at seed 42 (see
/// [`BASELINE_DOMINANT_KINDS_42`]'s doc for the measurement).
const BASELINE_PEOPLED_KINDS_42: usize = 2;
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

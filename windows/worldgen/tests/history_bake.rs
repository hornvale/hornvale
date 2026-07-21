//! The deep-history bake (Task 3): determinism + the displacement-fires
//! falsification gate. The workload test is the campaign's founding lesson
//! made executable — displacement must GENUINELY fire, at volume, driven by
//! the paleoclimate era swing, never by a floor (measure-don't-narrate).

use hornvale_kernel::{CellId, CellMap, Geosphere, KindId, ReferenceElevation, Seed};
use hornvale_paleoclimate::EraClimate;
use hornvale_worldgen::history_bake::{BakeConfig, bake, census};
use std::collections::BTreeSet;

/// Test-only helper: a validated `ReferenceElevation`.
fn e(m: f64) -> ReferenceElevation {
    ReferenceElevation::new(m).unwrap()
}

/// The four goblinoid peoples the campaign seeds history with.
fn peoples() -> Vec<KindId> {
    vec![
        KindId("goblin"),
        KindId("kobold"),
        KindId("hobgoblin"),
        KindId("bugbear"),
    ]
}

/// A small test world with a genuine, *oscillating* glacial swing — the
/// honest driver of displacement at volume:
///
/// - Warm eras: the lowland majority is habitable; the refuge cluster is not.
///   Communities settle and grow toward the lowland's high capacity.
/// - Glacial eras: the lowlands turn hostile and the refuge cluster becomes
///   the only habitable land. Every lowland community must migrate INTO the
///   low-capacity refuge, arriving OVER its capacity — pressure crosses 1.0
///   and raids (hence flees and resettlements) fire.
///
/// The mask oscillates warm/glacial across four glacial cycles, so the
/// concentration event — and the displacement it drives — recurs, exactly
/// as a real paleoclimate's era-variance would. Nothing here is a floor:
/// remove the swing (make every era warm) and displacement goes to zero.
///
/// Refuge cells are habitable ONLY in glacial eras, so they sit vacant when a
/// glacial onset drives migrants in — that vacancy is what lets the migrants
/// concentrate instead of colliding with prior settlement.
///
/// The `seed` argument is unused (the world is fixed; only the bake `Seed`
/// varies between runs), and is kept so callers read `fixture(42)`.
fn fixture(
    _seed: u64,
) -> (
    Geosphere,
    CellMap<f64>,
    CellMap<f64>,
    Vec<EraClimate>,
    CellMap<bool>,
) {
    let geo = Geosphere::new(1); // 42 cells

    // Refuge cluster: cell 0, its neighbours, and their neighbours (two rings)
    // — a compact upland island, viable only when the lowlands freeze.
    let mut refuge: BTreeSet<CellId> = BTreeSet::new();
    refuge.insert(CellId(0));
    for &n in geo.neighbors(CellId(0)) {
        refuge.insert(n);
    }
    let ring: Vec<CellId> = refuge.iter().copied().collect();
    for c in ring {
        for &n in geo.neighbors(c) {
            refuge.insert(n);
        }
    }
    let refugia = CellMap::from_fn(&geo, |c| refuge.contains(&c));

    // Refuge capacity is well below the lowland's, so a lowland community
    // carrying a near-lowland-capacity population lands over-capacity and raids.
    let capacity = CellMap::from_fn(&geo, |c| if refuge.contains(&c) { 60.0 } else { 120.0 });

    // Warm ⇒ lowlands (non-refuge) habitable; glacial ⇒ refuge habitable.
    let era = |day: f64, glacial: bool| EraClimate {
        day,
        ice: CellMap::from_fn(&geo, |_| false),
        habitable: CellMap::from_fn(&geo, |c| refuge.contains(&c) == glacial),
        sea_level: e(0.0),
        ice_fraction: if glacial { 0.6 } else { 0.0 },
    };
    // Eight eras across the two millennia: warm/glacial alternating, four
    // glacial cycles.
    let eras: Vec<EraClimate> = (0..8).map(|i| era(i as f64 * 250.0, i % 2 == 1)).collect();

    // River proximity is uniformly zero here (Task 5b): the fixture tests the
    // era-swing displacement mechanism, not the freshwater bias, so the river
    // weighting is a deliberate no-op and the displacement gate is unchanged.
    let river_prox = CellMap::from_fn(&geo, |_| 0.0);

    (geo, capacity, river_prox, eras, refugia)
}

#[test]
fn same_seed_bakes_byte_identical_history() {
    let (geo, cap, river, eras, refugia) = fixture(42);
    let people = peoples();
    let cfg = BakeConfig::default_millennia();
    let a = bake(Seed(42), &geo, &cap, &river, &eras, &refugia, &people, &cfg);
    let b = bake(Seed(42), &geo, &cap, &river, &eras, &refugia, &people, &cfg);
    assert_eq!(a.records, b.records, "same seed must bake byte-identical");
}

#[test]
fn different_seeds_diverge() {
    let (geo, cap, river, eras, refugia) = fixture(42);
    let people = peoples();
    let cfg = BakeConfig::default_millennia();
    let a = bake(Seed(42), &geo, &cap, &river, &eras, &refugia, &people, &cfg);
    let b = bake(Seed(43), &geo, &cap, &river, &eras, &refugia, &people, &cfg);
    assert_ne!(a.records, b.records, "different seeds must diverge");
}

#[test]
fn the_workload_fires_displacement_at_volume() {
    // measure-don't-narrate: the phenomenon the campaign exists on MUST fire.
    let (geo, cap, river, eras, refugia) = fixture(42);
    let people = peoples();
    let cfg = BakeConfig::default_millennia();
    let h = bake(Seed(42), &geo, &cap, &river, &eras, &refugia, &people, &cfg);
    let c = census(&h);
    assert!(c.fled + c.resettled > 50, "displacement inert: {c:?}");
    assert!(
        c.collapsed > 0 && c.alive_at_now > 0,
        "expected some collapses and some survivors: {c:?}"
    );
    // measure-don't-narrate: `fled` and `raided` are incremented in lockstep
    // (every raid closes with a flee), so their sum above overstates the
    // independent signal. Pin the causal MECHANISM directly: migration fires
    // (the era mask is genuinely displacing communities), raids fire, and
    // every flee is the raid's consequence, not a floor.
    assert!(
        c.migrated > 0,
        "no migration — the era mask isn't displacing anyone: {c:?}"
    );
    assert!(c.raided > 0, "no raids fired: {c:?}");
    assert_eq!(
        c.fled, c.raided,
        "flee must be the consequence of a raid: {c:?}"
    );
}

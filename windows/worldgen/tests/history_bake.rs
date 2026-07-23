//! The deep-history bake (Task 3): determinism + the displacement-fires
//! falsification gate. The workload test is the campaign's founding lesson
//! made executable — displacement must GENUINELY fire, at volume, driven by
//! the paleoclimate era swing, never by a floor (measure-don't-narrate).

use hornvale_kernel::{CellId, CellMap, Geosphere, KindId, ReferenceElevation, Seed};
use hornvale_paleoclimate::EraClimate;
use hornvale_topology::{ConnectionGraph, Edge, EdgeKind};
use hornvale_worldgen::history_bake::{BakeConfig, bake, census};
use std::collections::BTreeSet;

/// Test-only helper: a validated `ReferenceElevation`.
fn e(m: f64) -> ReferenceElevation {
    ReferenceElevation::new(m).unwrap()
}

/// A pure-land connection graph over `geo` (unit-conductance adjacency, no water
/// routes). `traversable_neighbors` over this equals `geo.neighbors`, so on an
/// all-land world the bake is byte-identical to the pre-Sundering raw-adjacency
/// bake — the no-op seam.
fn full_land_graph(geo: &Geosphere) -> ConnectionGraph {
    let mut g = ConnectionGraph::new(geo.cell_count());
    for cell in geo.cells() {
        for &n in geo.neighbors(cell) {
            if n.0 > cell.0 {
                g.add_edge(
                    cell,
                    Edge {
                        to: n,
                        kind: EdgeKind::Adjacency,
                        conductance: 1.0,
                    },
                );
            }
        }
    }
    g
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
    let graphs: Vec<ConnectionGraph> = eras.iter().map(|_| full_land_graph(&geo)).collect();
    let a = bake(
        Seed(42),
        &geo,
        &cap,
        &river,
        &eras,
        &refugia,
        &people,
        &cfg,
        &graphs,
    );
    let b = bake(
        Seed(42),
        &geo,
        &cap,
        &river,
        &eras,
        &refugia,
        &people,
        &cfg,
        &graphs,
    );
    assert_eq!(a.records, b.records, "same seed must bake byte-identical");
}

#[test]
fn different_seeds_diverge() {
    let (geo, cap, river, eras, refugia) = fixture(42);
    let people = peoples();
    let cfg = BakeConfig::default_millennia();
    let graphs: Vec<ConnectionGraph> = eras.iter().map(|_| full_land_graph(&geo)).collect();
    let a = bake(
        Seed(42),
        &geo,
        &cap,
        &river,
        &eras,
        &refugia,
        &people,
        &cfg,
        &graphs,
    );
    let b = bake(
        Seed(43),
        &geo,
        &cap,
        &river,
        &eras,
        &refugia,
        &people,
        &cfg,
        &graphs,
    );
    assert_ne!(a.records, b.records, "different seeds must diverge");
}

#[test]
fn the_workload_fires_displacement_at_volume() {
    // measure-don't-narrate: the phenomenon the campaign exists on MUST fire.
    let (geo, cap, river, eras, refugia) = fixture(42);
    let people = peoples();
    let cfg = BakeConfig::default_millennia();
    let graphs: Vec<ConnectionGraph> = eras.iter().map(|_| full_land_graph(&geo)).collect();
    let h = bake(
        Seed(42),
        &geo,
        &cap,
        &river,
        &eras,
        &refugia,
        &people,
        &cfg,
        &graphs,
    );
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

/// A saturating world: a tiny habitable cluster (cell 0 and its direct
/// neighbours) that genesis fills completely, surrounded by permanently
/// uninhabitable land. Once the cluster is full there is nowhere vacant, so
/// when a final era turns cell 0 hostile its community cannot migrate to
/// vacant land — it must DISPLACE an occupied neighbour, and that neighbour
/// cascades onward (the Sea-Peoples avalanche). Two eras: a long warm span
/// that lets the cluster saturate and stabilise, then a hostile span that
/// evicts cell 0 with no vacant refuge anywhere.
fn saturating_fixture() -> (
    Geosphere,
    CellMap<f64>,
    CellMap<f64>,
    Vec<EraClimate>,
    CellMap<bool>,
) {
    let geo = Geosphere::new(1); // 42 cells
    let mut hab: BTreeSet<CellId> = BTreeSet::new();
    hab.insert(CellId(0));
    for &n in geo.neighbors(CellId(0)) {
        hab.insert(n);
    }
    // Uniform capacity across the cluster; the rest of the world is worthless
    // AND uninhabitable in every era, so the cluster is the whole playfield.
    let capacity = CellMap::from_fn(&geo, |c| if hab.contains(&c) { 100.0 } else { 0.0 });
    let refugia = CellMap::from_fn(&geo, |_| false);
    let river_prox = CellMap::from_fn(&geo, |_| 0.0);
    // Warm: the whole cluster is habitable. Hostile: cell 0 turns hostile with
    // the rest of the cluster still habitable AND occupied — no vacant refuge.
    let warm = EraClimate {
        day: 0.0,
        ice: CellMap::from_fn(&geo, |_| false),
        habitable: CellMap::from_fn(&geo, |c| hab.contains(&c)),
        sea_level: e(0.0),
        ice_fraction: 0.0,
    };
    let hostile = EraClimate {
        day: 1000.0,
        ice: CellMap::from_fn(&geo, |_| false),
        habitable: CellMap::from_fn(&geo, |c| hab.contains(&c) && c.0 != 0),
        sea_level: e(0.0),
        ice_fraction: 0.0,
    };
    (geo, capacity, river_prox, vec![warm, hostile], refugia)
}

#[test]
fn a_full_world_cascades_when_a_cell_turns_hostile() {
    let (geo, cap, river, eras, refugia) = saturating_fixture();
    let people = peoples();
    let cfg = BakeConfig {
        start_year: 0.0,
        end_year: 1200.0,
        epoch_years: 25.0,
    };
    let graphs: Vec<ConnectionGraph> = eras.iter().map(|_| full_land_graph(&geo)).collect();
    let h = bake(
        Seed(42),
        &geo,
        &cap,
        &river,
        &eras,
        &refugia,
        &people,
        &cfg,
        &graphs,
    );
    let c = census(&h);
    // (a) a cascade fired: some log2 bin of the histogram is populated, and the
    //     per-displacement raid/flee tallies rose (a cascade of size N raids and
    //     flees N times as it walks the saturated graph).
    assert!(
        c.cascade_hist.iter().any(|&b| b > 0),
        "no cascade recorded — the hostile cell found vacant land instead of \
         displacing: {c:?}"
    );
    assert!(
        c.raided > 0 && c.fled > 0,
        "a cascade must raid and flee per displacement: {c:?}"
    );
    // (b) the bake TERMINATED — reaching this line proves the depth cap held
    //     (a runaway cascade would hang, not return). Guard the bound too.
    assert!(
        c.raided <= u64::from(hornvale_worldgen::history_bake::CASCADE_DEPTH_CAP) * 4,
        "cascade did not respect the depth cap: {c:?}"
    );
    // (c) determinism: same seed → byte-identical skeleton.
    let h2 = bake(
        Seed(42),
        &geo,
        &cap,
        &river,
        &eras,
        &refugia,
        &people,
        &cfg,
        &graphs,
    );
    assert_eq!(
        h.records, h2.records,
        "cascade must be deterministic (same seed → identical records)"
    );
}

#[test]
fn ocean_sunders_and_a_lane_leapfrogs() {
    use hornvale_worldgen::history_bake::{BakeConfig, History, bake, census};
    let geo = Geosphere::new(1);
    let ring2 = |seed: CellId| {
        let mut s = BTreeSet::new();
        s.insert(seed);
        for &n in geo.neighbors(seed) {
            s.insert(n);
        }
        for c in s.clone() {
            for &n in geo.neighbors(c) {
                s.insert(n);
            }
        }
        s
    };
    let a = ring2(CellId(0));
    let b_seed = geo
        .cells()
        .filter(|c| !a.contains(c))
        .max_by_key(|&c| geo.hops_between(CellId(0), c, 16).unwrap_or(0))
        .unwrap();
    let b: BTreeSet<CellId> = ring2(b_seed).difference(&a).copied().collect();
    assert!(
        a.is_disjoint(&b) && b.len() >= 3,
        "islands must be disjoint & non-trivial"
    );

    let build_graph = |lane: bool| {
        let mut g = ConnectionGraph::new(geo.cell_count());
        for cell in geo.cells() {
            for &n in geo.neighbors(cell) {
                if n.0 <= cell.0 {
                    continue;
                }
                let same =
                    (a.contains(&cell) && a.contains(&n)) || (b.contains(&cell) && b.contains(&n));
                if same {
                    g.add_edge(
                        cell,
                        Edge {
                            to: n,
                            kind: EdgeKind::Adjacency,
                            conductance: 1.0,
                        },
                    );
                }
            }
        }
        if lane {
            let (&fa, &fb) = (a.iter().next().unwrap(), b.iter().next().unwrap());
            g.add_edge(
                fa,
                Edge {
                    to: fb,
                    kind: EdgeKind::WaterRoute,
                    conductance: 0.5,
                },
            );
        }
        g
    };
    assert!(
        build_graph(false).reachable_regions(1e-9).len() >= 2,
        "islands must be sundered"
    );

    let refugia = CellMap::from_fn(&geo, |c| b.contains(&c));
    let capacity = CellMap::from_fn(&geo, |c| if a.contains(&c) { 120.0 } else { 60.0 });
    let river = CellMap::from_fn(&geo, |_| 0.0);
    let era = |day: f64, glacial: bool| EraClimate {
        day,
        ice: CellMap::from_fn(&geo, |_| false),
        habitable: CellMap::from_fn(&geo, |c| {
            if glacial {
                b.contains(&c)
            } else {
                a.contains(&c)
            }
        }),
        sea_level: e(0.0),
        ice_fraction: if glacial { 0.6 } else { 0.0 },
    };
    let eras: Vec<EraClimate> = (0..8).map(|i| era(i as f64 * 250.0, i % 2 == 1)).collect();
    let cfg = BakeConfig::default_millennia();
    let people = vec![KindId("goblin")];
    let on_b = |h: &History| h.records.iter().any(|r| b.contains(&r.site));

    let graphs_no = vec![build_graph(false); eras.len()];
    let no_lane = bake(
        Seed(7),
        &geo,
        &capacity,
        &river,
        &eras,
        &refugia,
        &people,
        &cfg,
        &graphs_no,
    );
    assert!(
        !on_b(&no_lane),
        "ocean must sunder: crossed with no lane: {:?}",
        census(&no_lane)
    );

    let graphs_lane = vec![build_graph(true); eras.len()];
    let lane = bake(
        Seed(7),
        &geo,
        &capacity,
        &river,
        &eras,
        &refugia,
        &people,
        &cfg,
        &graphs_lane,
    );
    assert!(
        on_b(&lane),
        "a lane must let a people leapfrog to island B: {:?}",
        census(&lane)
    );
}

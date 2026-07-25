//! The deep-history bake (Task 3): determinism + the displacement-fires
//! falsification gate. The workload test is the campaign's founding lesson
//! made executable — displacement must GENUINELY fire, at volume, driven by
//! the paleoclimate era swing, never by a floor (measure-don't-narrate).

use hornvale_history::record::{CauseOfEnd, Ended, Founding};
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
/// honest driver of climate displacement at volume:
///
/// - Warm eras: the lowland majority is habitable; the refuge cluster is not.
///   Communities settle and grow toward the lowland's high capacity.
/// - Glacial eras: the lowlands turn hostile and the refuge cluster becomes
///   the only habitable land. Every lowland community must migrate INTO the
///   low-capacity refuge, arriving over its capacity.
///
/// The mask oscillates warm/glacial across four glacial cycles, so the
/// concentration event — and the displacement it drives — recurs, exactly
/// as a real paleoclimate's era-variance would. Nothing here is a floor:
/// remove the swing (make every era warm) and displacement goes to zero.
///
/// Capacity is uniform WITHIN each region (refuge 60, lowland 120) but the two
/// regions are adjacent, so a 60→120 gradient does run along their boundary and
/// a refuge-dweller genuinely covets its lowland neighbour. What makes this the
/// negative control for predation is the era mask, not flatness: refuge and
/// lowland are habitable in DISJOINT eras, and the epoch snapshot is taken
/// before any migration, so a low-value community is never stepped while an
/// occupied high-value neighbour is still indexed. Covetousness therefore never
/// finds a live target, and `raided` stays 0 however crowded the refuge gets —
/// crowding alone starts no fights.
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
    // driven into the refuge by a glacial onset arrives over-capacity — the
    // pressure that drives the growth damping and, at the extreme, famine.
    // It never drives a raid: crowding is not a conflict trigger.
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
fn the_workload_fires_climate_displacement_at_volume_without_conflict() {
    // measure-don't-narrate: the era swing MUST genuinely displace communities
    // — and (The Tumult re-point) it must do so WITHOUT starting a war. The
    // crowding trigger this test used to assert is retired: conflict is
    // predation on a value gradient, and this world has none (both regions are
    // internally flat), so the honest reading of this fixture is climate
    // displacement at volume with zero raids. Equals do not prey.
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
    // Displacement at volume: the mask genuinely evicts communities, over and
    // over, across the four glacial cycles.
    assert!(c.migrated > 50, "climate displacement inert: {c:?}");
    assert!(
        c.collapsed > 0 && c.alive_at_now > 0,
        "expected some collapses and some survivors: {c:?}"
    );
    // The negative control. The 60→120 gradient along the refuge/lowland
    // boundary is real, but the mask makes the two regions habitable in
    // DISJOINT eras and the epoch snapshot precedes migration, so covetousness
    // never finds a LIVE occupant on the richer side. No raid may fire —
    // however crowded the refuge gets. Density is NOT a conflict trigger.
    assert_eq!(
        c.raided, 0,
        "a value-flat world must start no fights (density is not the trigger): {c:?}"
    );
    assert_eq!(c.fled, 0, "no raid, so nobody may be driven off: {c:?}");
}

/// A world with LAND TO SPARE and a sharp value gradient — the fixture that
/// isolates predation from crowding. Every cell is habitable in every era (the
/// mask never evicts anyone), capacity steps 20 → 100 from cell to cell (so a
/// community can sit on poor land beside much richer land), and no cell is so
/// poor that a genesis (pop 10) or daughter (pop 8) community starts anywhere
/// near the crowding thresholds. Nothing here can ever reach `pressure >= 1.0`,
/// so any conflict this world produces is driven by coveted VALUE down a
/// STRENGTH gradient, never by density.
fn land_to_spare_fixture() -> (
    Geosphere,
    CellMap<f64>,
    CellMap<f64>,
    Vec<EraClimate>,
    CellMap<bool>,
) {
    let geo = Geosphere::new(1); // 42 cells
    let capacity = CellMap::from_fn(&geo, |c| 20.0 + 20.0 * f64::from(c.0 % 5));
    let river_prox = CellMap::from_fn(&geo, |_| 0.0);
    let refugia = CellMap::from_fn(&geo, |_| false);
    let era = EraClimate {
        day: 0.0,
        ice: CellMap::from_fn(&geo, |_| false),
        habitable: CellMap::from_fn(&geo, |_| true),
        sea_level: e(0.0),
        ice_fraction: 0.0,
    };
    (geo, capacity, river_prox, vec![era], refugia)
}

#[test]
fn a_strong_community_raids_a_weaker_richer_neighbour_with_land_to_spare() {
    // The Tumult's founding claim: conflict is predation, not congestion. In a
    // world where nobody is crowded and nobody is evicted, raids must STILL
    // fire — a community that can beat its neighbour takes the better land.
    let (geo, cap, river, eras, refugia) = land_to_spare_fixture();
    // Two peoples, not four: the map must stay demonstrably under-occupied, so
    // that "there was nowhere else to go" is never available as an explanation.
    let people = vec![KindId("goblin"), KindId("kobold")];
    let cfg = BakeConfig {
        start_year: 0.0,
        end_year: 500.0,
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
    // (a) Climate displaced nobody: every cell is habitable in every era.
    assert_eq!(
        c.migrated, 0,
        "the mask must never evict anyone here: {c:?}"
    );
    // (b) Land genuinely to spare: most of the map is still empty at `now`.
    assert!(
        (c.alive_at_now as usize) * 2 < geo.cell_count(),
        "fixture must leave land to spare (alive {} of {} cells): {c:?}",
        c.alive_at_now,
        geo.cell_count()
    );
    // (c) …and conflict fired anyway — coveted value down a strength gradient.
    assert!(c.raided > 0, "no raid fired with land to spare: {c:?}");
    // (d) The raid had teeth, and it was a CONQUEST OF LAND rather than a
    //     bookkeeping event: some occupation ended `Fled` at a named raider's
    //     hand, and that same raider seated a new occupation ON THE VERY CELL
    //     it drove the loser off. Checked against the records, not the tally:
    //     `raided` and `fled` are incremented unconditionally and adjacently in
    //     `maybe_raid`, so an `assert!(c.fled > 0)` here would be implied by (c)
    //     and could never fail on its own.
    //     The seat must be a DISTINCT occupation opened at the very moment the
    //     loser ended: without those two guards the loser's own record can
    //     satisfy the predicate whenever the loser happens to be a daughter of
    //     the community that later raided it (mutation-verified — moving the
    //     raider's `open` back onto its own cell must, and does, redden this).
    let conquest = h.records.iter().any(|loser| {
        loser.cause == Some(CauseOfEnd::Fled)
            && match loser.ended_by {
                Ended::By(raider) => h.records.iter().any(|seat| {
                    seat.founded_from == Founding::From(raider)
                        && seat.site == loser.site
                        && seat.community != loser.community
                        && Some(seat.founded) == loser.ended
                }),
                Ended::Nature => false,
            }
    });
    assert!(
        conquest,
        "no raider ever seated itself on the cell it drove a neighbour off: {c:?}"
    );
}

/// A saturating world: a tiny habitable cluster (cell 0 and its direct
/// neighbours) that genesis fills completely, surrounded by permanently
/// uninhabitable land. Once the cluster is full there is nowhere vacant, so
/// when a final era turns cell 0 hostile its community cannot migrate to
/// vacant land at all. Two eras: a long warm span that lets the cluster
/// saturate and stabilise, then a hostile span that evicts cell 0 with no
/// vacant refuge anywhere. Capacity is uniform across the cluster, so there
/// is nothing to covet either — the trapped community has no way out.
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
fn a_hostile_cell_in_a_full_world_starves_instead_of_cascading() {
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
    // (a) The Tumult re-point: a climate eviction is NOT a war. With no vacant
    //     refuge the trapped community starves where it stands — it does not
    //     take a neighbour's land, so no raid, no flight, no avalanche. (The
    //     crowding cascade this test used to assert was the falsified model:
    //     displacement-by-congestion is retired; conflict is predation.)
    assert!(c.collapsed > 0, "the trapped community must starve: {c:?}");
    assert_eq!(c.raided, 0, "a climate eviction must start no fight: {c:?}");
    assert_eq!(c.fled, 0, "no raid, so nobody may be driven off: {c:?}");
    assert!(
        c.cascade_hist.iter().all(|&b| b == 0),
        "climate eviction must never cascade: {c:?}"
    );
    // (b) The world was not emptied by the eviction — the rest of the cluster
    //     lives on, so this is a targeted death, not a collapse of everything.
    assert!(c.alive_at_now > 0, "the world must not be emptied: {c:?}");
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
        "the bake must be deterministic (same seed → identical records)"
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

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
        ..BakeConfig::default_millennia()
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
        loser.core.cause == Some(CauseOfEnd::Fled)
            && match loser.ended_by {
                Ended::By(raider) => h.records.iter().any(|seat| {
                    seat.founded_from == Founding::From(raider)
                        && seat.core.site == loser.core.site
                        && seat.community != loser.community
                        && Some(seat.core.founded) == loser.core.ended
                }),
                Ended::Nature => false,
            }
    });
    assert!(
        conquest,
        "no raider ever seated itself on the cell it drove a neighbour off: {c:?}"
    );
}

/// An ESCARPMENT: capacity falls off in steps with graph distance from cell 0
/// (200 at the crown, 20 at the rim), every cell habitable in every era. Two
/// properties matter:
///
/// - **Neighbours differ in value**, everywhere and always, so covetousness has
///   something to find without any climate event; and
/// - **the vacant remainder is the POOR ground** — daughters settle the best
///   vacant neighbour, so occupation climbs the escarpment and what is left
///   empty is the rim.
///
/// That is the geometry spec §4.3's amended rule is about: a people driven off
/// the crown can pioneer the marginal rim or take a rich holding it can beat.
/// Under the vacant-first rule it always pioneers and the branching ratio is
/// zero by construction; under the one-comparison rule the rich holding wins
/// whenever the roller can win the fight, and the relaxation chains.
///
/// **The escarpment needs room to relax LOCALLY.** Spec §4.3's rule is
/// nearest-ring, not global, so a chained relaxation is a *neighbourhood*
/// event: the loser's own first ring has to hold a beatable, richer holding.
/// At `Geosphere::new(1)` (42 cells, ~5 rings from pole to pole) a displaced
/// people's first ring is most of the interesting world and the escarpment is
/// only four steps wide, so the fixture measured cascades only on some seeds —
/// an instrument too coarse for a local rule, not a physics finding. The same
/// escarpment on `Geosphere::new(2)` (162 cells) gives it the rings it needs;
/// the geometry, the arithmetic (20 per step) and the assertions are
/// unchanged.
///
/// The gradient is DEEPER, not exhaustive: `(200 - 20 × hops).max(20)` falls
/// for nine hops and then sits on its 20 floor, while the sphere spans twelve
/// rings from cell 0. So the outer three rings are flat, uniformly poor rim —
/// which is fine for what the fixture is for (the interesting relaxations
/// happen on the slope), but it is not the "out to the full radius" an earlier
/// revision of this comment claimed.
fn escarpment_fixture() -> (
    Geosphere,
    CellMap<f64>,
    CellMap<f64>,
    Vec<EraClimate>,
    CellMap<bool>,
) {
    let geo = Geosphere::new(2); // 162 cells
    let capacity = CellMap::from_fn(&geo, |c| {
        let hops = geo.hops_between(CellId(0), c, 32).unwrap_or(9);
        (200.0 - 20.0 * hops as f64).max(20.0)
    });
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
fn a_displaced_people_rolls_downhill_and_the_cascade_is_recorded() {
    // The Tumult's headline instrument, made to move. Task 1 measured a
    // cascade histogram that was all-zero BY CONSTRUCTION: `relocate` took
    // vacant land whenever any was reachable, so a displaced people never
    // displaced anyone in turn and the branching ratio could not even be
    // asked about. Spec §4.3's amended rule (one comparison over every
    // reachable cell, held cells carrying the settled premium) is what makes
    // a chained relaxation possible at all — this test fails, with an
    // all-zero histogram, against the vacant-first rule.
    let (geo, cap, river, eras, refugia) = escarpment_fixture();
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
    eprintln!("ESCARPMENT census: {c:?}");
    // (a) The mask never evicts anyone here: every displacement in this world
    //     is conflict, not climate.
    //
    //     `migrated` counts BOTH orderly moves — the climate eviction this
    //     clause is about, and spec §4.3d's vassal flight, which shares the
    //     tally deliberately (a flight is a self-directed move, never a
    //     `fled`). `vassal_flights` is the flight subset, so the climate
    //     eviction count is the difference, and it is the difference this
    //     clause has always meant. Comparing `migrated` against zero read as
    //     the same claim only while no flight happened to fire on this
    //     fixture, which stopped being true the moment a fleeing vassal kept
    //     its relation (spec §4.3e) and so had reason to leave more than once.
    assert_eq!(
        c.migrated, c.vassal_flights,
        "the mask must never evict anyone here — every orderly move must be a flight: {c:?}"
    );
    // (b) Conflict fires…
    assert!(c.raided > 0, "no raid fired on the escarpment: {c:?}");
    // (c) …and it CHAINS: at least one raid drove a loser onto ground that was
    //     itself held, which is the displacement the histogram counts.
    let cascades: u64 = c.cascade_hist.iter().sum();
    assert!(
        cascades > 0,
        "no relaxation chained — the branching ratio is still zero: {c:?}"
    );
    // (d) …without emptying the world (dissipation must bound the avalanche,
    //     not consume the map).
    assert!(c.alive_at_now > 0, "the cascade emptied the world: {c:?}");
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
        ..BakeConfig::default_millennia()
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

/// A **value-flat** world: every cell carries the same capacity and every cell
/// is habitable in the single era. Two properties make it the exact negative
/// control the subordination trigger needs (spec §4.1):
///
/// - **No cell is ever worth more than its neighbour**, so the shipped covet
///   test (`eff_capacity(target) > eff_capacity(raider)`) is false *everywhere,
///   always*. Eviction is impossible by construction, not by luck of the seed.
/// - **Nobody is ever crowded and nobody is ever evicted**: capacity 120 sits
///   far above the genesis (10) and daughter (8) populations, and the logistic
///   growth term asymptotes at pressure 1, well below `COLLAPSE_PRESSURE`. So
///   every community stays productive — `has_spoils` holds — and no climate
///   mask ever moves anybody.
///
/// What is left is a pure strength gradient: communities founded at different
/// years sit at wildly different populations (and reach their tech horizons at
/// different years), so a mature genesis community towers over a fresh daughter
/// next door. Under the shipped rule that neighbour is invisible — poorer land
/// is not coveted, so a strong community simply ignores it. Under tribute the
/// people are the prize, and it is milked where it stands.
fn value_flat_fixture() -> (
    Geosphere,
    CellMap<f64>,
    CellMap<f64>,
    Vec<EraClimate>,
    CellMap<bool>,
) {
    let geo = Geosphere::new(1); // 42 cells
    let capacity = CellMap::from_fn(&geo, |_| 120.0);
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
fn a_strong_community_subordinates_a_productive_neighbour_it_would_not_evict() {
    // The Tithe's founding claim: asset mobility decides how a raid ends
    // (spec §4.1). The prize the shipped rule knows is IMMOBILE — the cell —
    // so a raid can only evict, and a neighbour whose land is no better is
    // ignored outright (`t_val <= raider_val { continue }`). The prize this
    // task adds is MOBILE — the people and their product — takeable
    // repeatedly without displacing anyone.
    //
    // The fixture makes the shipped path IMPOSSIBLE: capacity is uniform, so
    // the covet test is false on every pair in every era. Any raid this world
    // resolves is therefore the new branch, and `raided == 0` is asserted
    // alongside so the test cannot pass by the old mechanism firing.
    let h = value_flat_history();
    let c = census(&h);
    // (a) The world really is value-flat and quiet: no eviction, no climate
    //     displacement, nobody starved. Nothing here can explain a raid.
    assert_eq!(
        c.raided, 0,
        "equal-value land: eviction must not fire: {c:?}"
    );
    assert_eq!(c.fled, 0, "no eviction, so nobody may be driven off: {c:?}");
    assert_eq!(
        c.migrated, 0,
        "the mask must never evict anyone here: {c:?}"
    );
    // (b) …and the new branch fired anyway.
    assert!(
        c.subordinations_formed > 0,
        "a productive, beatable, no-richer neighbour must be subordinated: {c:?}"
    );
    // (c) The relation is REAL, not just a counter: relations still STAND at
    //     `now`, and at least one patron holds a subordinate. Both are read off
    //     the live relation table, so they fail if the tally is bumped without
    //     a relation being recorded (mutation-verified).
    assert!(
        c.tribute_relations_at_now > 0,
        "the tally moved but no relation stands at now: {c:?}"
    );
    assert!(
        c.max_subordinates > 0,
        "the tally moved but no relation is held at now: {c:?}"
    );
    // Depth (spec §9's deferred chaining lever, which §5 preregisters the
    // headline on the absence of) is bound on this and every other REAL bake by
    // the `debug_assert!` at the point of formation in `history_bake.rs` — the
    // table is private, and an end-of-bake reading would in any case miss a
    // chain that formed and dissolved mid-span. Tests run in debug.
    // (d) Tribute redistributes rather than consuming: subordination moves
    //     nobody, so every community that was ever opened and not starved is
    //     still standing.
    assert_eq!(
        c.alive_at_now, c.records_total,
        "subordination must displace and kill nobody: {c:?}"
    );
}

/// [`value_flat_fixture`]'s world, baked over twenty 25-year epochs — long
/// enough that the relations it forms have epochs left to actually collect
/// over. "A relation stands" and "tribute flowed" are genuinely different
/// claims: a remittance is paid out of the epoch's growth first and only then
/// out of the stock standing above `FARM_FLOOR` (spec §4.2b), so a relation
/// formed on the last epoch of a bake collects once and learns nothing.
/// Shared by every value-flat test, so they bake one world between them
/// rather than two identical ones.
fn value_flat_history() -> hornvale_worldgen::history_bake::History {
    value_flat_history_with(std::collections::BTreeMap::new())
}

/// [`value_flat_history`] with an authored `in_group_radius` map — the one
/// input the concealment arms differ in. An empty map is the fail-open the
/// composition root hands a bake whose peoples carry no `SocietyVector`, so
/// `value_flat_history()` is exactly the world every other value-flat test
/// reads.
fn value_flat_history_with(
    in_group_radius: std::collections::BTreeMap<KindId, f64>,
) -> hornvale_worldgen::history_bake::History {
    value_flat_history_seeded_with(42, in_group_radius)
}

/// [`value_flat_history_with`] over an arbitrary seed. The fixture's own
/// construction is seed-INDEPENDENT — 42 cells, uniform capacity, one era —
/// so the seed enters only through `bake`, which is what makes a seed band
/// affordable here: the whole 1..=100 sweep the concealment test runs costs
/// well under a second.
fn value_flat_history_seeded_with(
    seed: u64,
    in_group_radius: std::collections::BTreeMap<KindId, f64>,
) -> hornvale_worldgen::history_bake::History {
    let (geo, cap, river, eras, refugia) = value_flat_fixture();
    let people = peoples();
    let cfg = BakeConfig {
        start_year: 0.0,
        end_year: 500.0,
        epoch_years: 25.0,
        in_group_radius,
        ..BakeConfig::default_millennia()
    };
    let graphs: Vec<ConnectionGraph> = eras.iter().map(|_| full_land_graph(&geo)).collect();
    bake(
        Seed(seed),
        &geo,
        &cap,
        &river,
        &eras,
        &refugia,
        &people,
        &cfg,
        &graphs,
    )
}

#[test]
fn tribute_flows_along_a_standing_relation() {
    // Spec §4.2/§4.2a over a REAL bake: Task 2 built the standing relation but
    // nothing moved along it. What must be true now is that the relation is a
    // conduit — wealth leaves the subordinate and lands in the patron's store.
    //
    // The other half of the claim — that being farmed is SURVIVABLE, which
    // since amendment 3 means "never bled through `FARM_FLOOR`" (spec §4.2b,
    // §8.3) — is deliberately NOT asserted here, because in this fixture no
    // census reading can carry it. A headcount identity (`alive_at_now ==
    // records_total`) is unreddenable by any tribute defect: starvation needs
    // population at `COLLAPSE_PRESSURE` times capacity, the logistic growth
    // term is bounded BY capacity, and tribute only ever LOWERS population —
    // so nobody here can die however hard they are farmed, and a subordinate
    // drained to nothing stays alive at zero. The survival claim is bound
    // instead where the state it is about is visible:
    // `no_subordinate_is_farmed_below_the_farm_floor_by_tribute` (in
    // `history_bake.rs`) drives the real epoch loop and reads the
    // per-subordinate population between epochs, which a finished `History`
    // does not carry.
    let h = value_flat_history();
    let c = census(&h);
    assert!(
        c.subordinations_formed > 0,
        "precondition: a relation must form before anything can flow: {c:?}"
    );
    assert!(
        c.tribute_collected > 0.0,
        "tribute must actually flow along a standing relation: {c:?}"
    );
    assert!(
        c.max_stores_at_now > 0.0,
        "the flow must LAND: some patron must hold a store at now: {c:?}"
    );
}

#[test]
fn concealment_moves_what_a_patron_collects_and_under_the_setpoint_it_moves_it_down() {
    // Spec §4.2's concealment term over a REAL bake, not a hand-driven pair —
    // and **restated at task 5b, because amendment 3 inverted what it
    // measures.** The history is worth keeping, because the inversion is a
    // finding about the amended mechanism and not a defect in it:
    //
    //   * T4 built this as `an_insular_world_yields_less_tribute_than_an_
    //     expansive_one`, which its own review cut back to a DIRECT-term claim
    //     (`concealment_lowers_the_direct_term_over_a_structurally_invariant_
    //     fixture`) after the whole-world proposition proved FALSE on seed 42:
    //     `tribute_collected` ROSE +9.1% (6002.56 -> 6549.04) with concealment
    //     switched on, because a concealing vassal lives longer and pays for
    //     more epochs than it discounts per epoch.
    //   * Amendment 3 (spec §4.2b) then brought that inversion onto THIS
    //     fixture too, and the reason is structural rather than incidental.
    //     Concealment scales the AVAILABILITY (`(surplus + bleed) × (1 −
    //     concealment)`), and the bleed makes availability the whole stock
    //     above `FARM_FLOOR` — normally far above the standing demand. So the
    //     `min` selects the assessment branch on most collections and the
    //     concealment factor never binds there at all. What concealment then
    //     did was shield the vassal: it was bled more slowly, stayed larger,
    //     kept clearing its patron's demand, and therefore paid MORE over the
    //     span, not less. Measured then: insular 419.87 vs expansive 412.60
    //     (+1.8%) over 20 epochs and 244 collections in each arm.
    //   * **Amendment 4 (spec §4.3a) flipped it back, and the flip is the
    //     amendment working.** The reach now stops at the patron's setpoint
    //     rather than at `FARM_FLOOR`, so availability is what stands above
    //     that setpoint — a much smaller number, of the order of the epoch's
    //     increment — and it is no longer far above the standing demand. The
    //     `min` therefore selects the availability branch often, where the
    //     concealment factor DOES bind, and a hidden share is once again
    //     simply a share not handed over. The shielding effect that reversed
    //     the sign at T5b is gone with the thing it shielded against: a vassal
    //     is not bled toward a floor it cannot recover from any more, so
    //     concealment has far less to save it from. Measured here: insular
    //     306.43 vs expansive 324.89 (−5.7%), over the same structurally
    //     invariant fixture.
    //
    // **The direct term itself is unmoved and is still bound**, in
    // `an_insular_subordinate_remits_less_than_an_expansive_one` (in
    // `history_bake.rs`), which compares the two radii against the SAME state
    // in a single epoch — the only frame in which "insular remits less" is a
    // statement about concealment rather than about two different histories.
    //
    // What this test binds is the pair of claims that survive the amendment,
    // and both are sharp:
    //
    //   (a) the fixture is genuinely structurally invariant — same formations,
    //       same transfers, same standing relations, same collections, same
    //       records, same survivors — so the arms differ in what MOVED and in
    //       nothing else; and
    //   (b) concealment is NOT INERT: the totals differ. Delete the term (a
    //       concealment of zero for everybody) and the two arms become
    //       bit-identical, which reddens (b) — mutation-verified.
    //
    // The two arms are the same world, the same seed, the same span and the
    // same peoples; they differ in EXACTLY ONE input — the authored
    // `in_group_radius` of the peoples that live in it — so only concealment
    // can explain the gap between what the patrons collected. Task 6's
    // attribution must therefore read `tribute_collected` alongside
    // `tribute_collection_events` and `tribute_relations_at_now`: the sign of
    // this term has moved twice under two amendments without the term itself
    // changing a line, because what it multiplies — the availability branch —
    // changed shape underneath it. A volume reading alone cannot tell "moved
    // the rate" from "moved the payer", and here it has been each in turn.
    // **Widened to a seed band by The Contour (decision 0097), and the
    // widening is a finding.** What stood here ran the two arms at seed 42
    // alone and guarded the comparison with nine equality assertions on the
    // ground that the arms "differ in EXACTLY ONE input". They do differ in
    // exactly one INPUT. They do not differ in exactly one OUTCOME, and they
    // never could: concealment moves tribute, tribute moves the subordinate's
    // population, population moves strength, and strength enters the
    // `RAID_MARGIN` comparison that decides a takeover
    // (`history_bake.rs`'s `maybe_raid`). The guard held at seed 42's old
    // draws only because no takeover happened to sit astride the margin.
    //
    // The `history/bake/v2` epoch re-minted those draws and one did. Measured
    // over seeds 1..=100, both arms, all nine originally-guarded fields: the
    // fixture is structurally invariant on **63 of 100 seeds**, and seed 42
    // moved from the 63 to the 37 on the re-mint alone — the test passes at
    // pre-epoch `e8c85d68` with the position-aware raid rule already live, so
    // the mechanism did not move it. The mechanism could not have: this
    // fixture's `full_land_graph` gives every edge conductance 1.0, so
    // `defensibility` evaluates to the single constant 0.750001838 (`DEF_MIN`)
    // over all 240 ordered adjacent pairs and is arm-invariant by
    // construction.
    //
    // So the old guard was decision 0097's row three exactly — an existence
    // claim near its threshold, carrying a value pin's noise profile with an
    // invariant's authority. It is not re-pinned and not relaxed; it is
    // replaced by the reading that is available at n = 100 and was not
    // available at n = 1.
    //
    // **And the guard was asking the wrong question.** It treated
    // concealment's own downstream consequences as confounds to be excluded.
    // Over a band, they are not confounds — they ARE the effect, and the total
    // effect is the honest thing to report. The DIRECT term, which is the one
    // claim that genuinely needs a same-state comparison, is bound separately
    // and unchanged in
    // `an_insular_subordinate_remits_less_than_an_expansive_one`, which
    // compares the two radii against the SAME state in a single epoch.
    //
    // Structural invariance is therefore REPORTED here and asserted nowhere
    // (0097 clause 3: never the same claim in two instruments). What is
    // asserted is the pooled sign, plus a strict majority of seeds — a
    // majority being the weakest statement that still says "down" rather than
    // "either way", so it is a floor with a reason and not a bar set just
    // under a measurement.
    //
    // The sign itself is the amendment-4 direction (spec §4.3a): the reach
    // stops at the patron's setpoint rather than at `FARM_FLOOR`, so
    // availability is of the order of the epoch's increment, the `min` selects
    // the availability branch often, and there the concealment factor binds —
    // what a vassal hides is once again a share it does not hand over.
    const BAND: u64 = 100;

    let expansive: std::collections::BTreeMap<KindId, f64> =
        peoples().into_iter().map(|k| (k, 1.0)).collect();
    let insular: std::collections::BTreeMap<KindId, f64> =
        peoples().into_iter().map(|k| (k, 0.0)).collect();

    let mut pooled_expansive = 0.0_f64;
    let mut pooled_insular = 0.0_f64;
    let mut sign_holds = 0u64;
    let mut structurally_invariant = 0u64;

    for seed in 1..=BAND {
        let ce = census(&value_flat_history_seeded_with(seed, expansive.clone()));
        let ci = census(&value_flat_history_seeded_with(seed, insular.clone()));

        assert!(
            ce.subordinations_formed > 0 && ci.subordinations_formed > 0,
            "precondition: relations must form in both arms at seed {seed}: \
             expansive {ce:?}, insular {ci:?}"
        );
        assert!(
            ce.tribute_collected > 0.0 && ci.tribute_collected > 0.0,
            "precondition: tribute must flow in BOTH arms at seed {seed} — a difference over \
             two zeros proves nothing: expansive {}, insular {}",
            ce.tribute_collected,
            ci.tribute_collected
        );

        pooled_expansive += ce.tribute_collected;
        pooled_insular += ci.tribute_collected;
        if ci.tribute_collected < ce.tribute_collected {
            sign_holds += 1;
        }
        // Reported, not asserted — see above.
        let same = [
            (ce.subordinations_formed, ci.subordinations_formed),
            (ce.patronage_transfers, ci.patronage_transfers),
            (ce.tribute_relations_at_now, ci.tribute_relations_at_now),
            (ce.tribute_collection_events, ci.tribute_collection_events),
            (ce.records_total, ci.records_total),
            (ce.alive_at_now, ci.alive_at_now),
            (ce.raided, ci.raided),
            (ce.migrated, ci.migrated),
            (ce.collapsed, ci.collapsed),
        ]
        .iter()
        .all(|(e, i)| e == i);
        if same {
            structurally_invariant += 1;
        }
    }

    println!(
        "concealment over seeds 1..={BAND}: structurally invariant {structurally_invariant}/{BAND}, \
         sign holds {sign_holds}/{BAND}, pooled tribute expansive {pooled_expansive:.2} vs \
         insular {pooled_insular:.2}"
    );

    // (a) Concealment is not inert, and pooled over the band it moves what the
    //     patrons collected DOWN.
    assert!(
        pooled_insular < pooled_expansive,
        "concealment must move what the patrons collected, and under the setpoint it moves it \
         DOWN: pooled over seeds 1..={BAND}, insular {pooled_insular} vs expansive \
         {pooled_expansive}. Equal totals mean the term is inert."
    );
    // (b) …and not by one outlier seed carrying the pool: it runs the same
    //     direction on a strict majority of individual worlds.
    assert!(
        sign_holds * 2 > BAND,
        "the pooled direction must not rest on a few large worlds: concealment lowered \
         collections on only {sign_holds} of {BAND} seeds, which is not a majority"
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
    let on_b = |h: &History| h.records.iter().any(|r| b.contains(&r.core.site));

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

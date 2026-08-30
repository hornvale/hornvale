//! MINES EXIST — The Winze, Task 2 (spec §B.3).
//!
//! Before this campaign, `Bake::open` hardcoded `Function::Agrarian` and it
//! was the only `Function` any production path ever assigned, so four of the
//! five vestige kinds — `VestigeKind::AbandonedDelving` among them — could not
//! occur in any world (spec §3.1, §3.2). These gates hold the population that
//! statement was about.
//!
//! **A mine is FOUNDED for ore, not reclassified onto it.** Task 1
//! (`ore_separation_probe.rs`) measured settlements to be *under*-represented
//! in high-ore ground — the agrarian siting objective is correct and puts
//! settlements where ore is not — so relabelling some of them could never
//! work (spec §B.2). What is asserted here is the replacement: an expansion
//! out of `Bake::grow` is occasionally a **working**, scored on
//! `prospectivity_at` alone and sited on ore-bearing ground.
//!
//! Pooled over the campaign's preregistered panel, with the per-seed counts
//! printed so a run that drifts is readable rather than merely red.
//!
//! # THE POPULATION, AND WHAT SETS IT
//!
//! Measured on the panel at the reach that ships (`WORKING_REACH = 3`):
//!
//! ```text
//!             mines   occupations   share    throws with ore in reach
//! seed 42        16          1212   1.32%                          32
//! seed 7         19           656   2.90%                          37
//! seed 1234       4           914   0.44%                           7
//! pooled         39          2782   1.40%                          76
//! ```
//!
//! **Task 2 shipped a one-hop scan and it read 1 / 13 / 2.** What bound it was
//! not the rate and not [`ORE_CUT`]: it was that ore is rarely one hop from
//! farmland, which is Task 1's finding (settlements are *under*-represented in
//! high-ore ground, because agrarian siting is correct and puts them where ore
//! is not) arriving one hop out and biting the design built to answer it. Only
//! 4 of seed 42's 466 daughter throws had any workable-ore candidate at all.
//! Spec amendment E replaces the one-hop scan with a ring scan bounded by
//! [`WORKING_REACH`]; see that constant's own doc for why the bound exists and
//! what an unbounded scan does to a world.
//!
//! **The obvious way to raise the count further is the one this campaign must
//! not take.** Dropping [`ORE_CUT`] to the field's barren floor yielded 42 /
//! 19 / 6 mines under the one-hop scan — inside spec §5.1's preregistered
//! range — with 23 of seed 42's 42 then standing on ground of prospectivity
//! 0.0600, which is to say on no ore at all. That is measured, not argued: it
//! is what the mutation proving `every_mine_stands_on_ore_bearing_ground`
//! non-vacuous actually printed. Spec §5.1 and §E.5 both forbid the cut by
//! name.
//!
//! # AGAINST SPEC §5.1'S FOUR-ROW TABLE, WHICH §E.4.1 LEAVES IN FORCE
//!
//! `1-5 per world` on seed 1234 and `6-40 per world` on the other two: the
//! panel straddles two rows rather than sitting in one. The binding row is
//! therefore still **"viable but thin"** — pool across the panel, and
//! single-seed claims stay banned for the rest of the campaign. §E.4.1's two
//! added rows both pass: no seed's mine count exceeds its count of throws with
//! a workable candidate in reach (16 <= 32, 19 <= 37, 4 <= 7), and the radius
//! distribution is reported by `a_workings_founding_radius_stays_inside_the_supply_bound`.

use hornvale_astronomy::SkyPins;
use hornvale_history::record::Founding;
use hornvale_history::record::Function;
use hornvale_kernel::{Seed, Vertex};
use hornvale_terrain::TerrainPins;
use hornvale_worldgen::history_bake::{ORE_CUT, WORKING_REACH};
use hornvale_worldgen::{
    BuildDepth, SettlementPins, SkyChoice, WorldComponents, build_world_to, occupation_records,
    terrain_of,
};

/// The panel the campaign preregisters on (spec §5.1).
const SEEDS: [u64; 3] = [42, 7, 1234];

/// Every occupation of one world, with its function and its site's
/// prospectivity.
///
/// `terrain_of` re-sculpts the tectonic globe (decision 0092's derivation
/// entry point), which is what these gates need: the ore field the bake sited
/// on is not committed to the ledger, so the only way to ask "was this mine on
/// ore" is to re-derive the same field the composition root built. A named
/// construction site, called once per seed.
#[allow(clippy::disallowed_methods)] // decision 0092: named construction site
fn functions_and_ore(seed_value: u64) -> Vec<(Function, f64)> {
    let seed = Seed(seed_value);
    let world = build_world_to(
        seed,
        &SkyPins::default(),
        SkyChoice::Generated,
        &TerrainPins::default(),
        &SettlementPins::default(),
        &WorldComponents::assemble().expect("canonical components assemble"),
        BuildDepth::Settlements,
    )
    .expect("panel seed builds");
    let terrain = terrain_of(&world).expect("terrain reconstructs");
    occupation_records(&world)
        .into_iter()
        .map(|r| (r.core.function, terrain.prospectivity_at(r.core.site)))
        .collect()
}

/// Mines occur at all, on the preregistered panel — the population spec §3.1
/// measured at zero.
///
/// claim: reachability(seeds: 42/7/1234 — can `Function::Mine` occur in any
/// world at all, pooled; not a per-world rate)
#[test]
fn mines_are_founded_on_the_panel() {
    let mut total_mines = 0usize;
    let mut firing_seeds = 0usize;
    for seed_value in SEEDS {
        let occs = functions_and_ore(seed_value);
        let mines = occs.iter().filter(|(f, _)| *f == Function::Mine).count();
        println!(
            "seed {seed_value}: {mines} mines / {} occupations ({:.2}%)",
            occs.len(),
            mines as f64 / occs.len().max(1) as f64 * 100.0
        );
        total_mines += mines;
        if mines > 0 {
            firing_seeds += 1;
        }
    }
    // POOLED, per the plan's Task 2 Step 4, and not per-seed — deliberately.
    // A world's mine count is bounded by how many expansions it makes at all,
    // and that varies by more than an order of magnitude across the panel
    // (`Bake::grow` throws 489 daughters on seed 42 and 26 on seed 1234), so a
    // per-seed floor would be asserting about a world's expansiveness rather
    // than about whether mines can exist.
    assert!(
        total_mines > 0,
        "no Function::Mine occupation was founded on any of {SEEDS:?}. Before \
         The Winze this was true of every world ever generated (spec §3.1); \
         after spec §B.3's working objective it must not be."
    );
    // The pool must not be one world's accident: the mechanism has to fire on
    // more than one seed. Guards the failure a bare total cannot see.
    assert!(
        firing_seeds >= 2,
        "mines were founded on only {firing_seeds} of {} seeds ({total_mines} \
         pooled) — one world carrying the whole population means the ore \
         objective is firing on that world's tectonics, not on the rule.",
        SEEDS.len()
    );
}

/// Every mine stands on ore-bearing ground. This is what separates "a second
/// objective was scored" from "some daughters were relabelled": a working is
/// sited by `prospectivity_at` alone, and the candidate set it maximises over
/// is filtered at [`ORE_CUT`], so no mine can sit below it.
///
/// claim: invariant(forall-seed over the panel — every Mine's site is at or
/// above `ORE_CUT`)
#[test]
fn every_mine_stands_on_ore_bearing_ground() {
    for seed_value in SEEDS {
        let occs = functions_and_ore(seed_value);
        let below: Vec<f64> = occs
            .iter()
            .filter(|(f, _)| *f == Function::Mine)
            .map(|&(_, p)| p)
            .filter(|&p| p < ORE_CUT)
            .collect();
        assert!(
            below.is_empty(),
            "seed {seed_value}: {} mines sit below ORE_CUT ({ORE_CUT}): {below:?}. \
             A working is sited on the ore objective, so its site cannot be \
             below the cut its candidate set was filtered at.",
            below.len()
        );
    }
}

/// The complement, and the reason the gate above is not vacuous: agrarian
/// settlements are NOT confined to ore-bearing ground. Without this, a world
/// in which every occupation happened to sit above the cut would satisfy the
/// gate above while proving nothing about the objective.
///
/// claim: invariant(forall-seed over the panel — the positive control for the
/// gate above: agrarian siting is NOT confined to ore-bearing ground)
#[test]
fn agrarian_settlements_are_not_confined_to_ore_bearing_ground() {
    for seed_value in SEEDS {
        let occs = functions_and_ore(seed_value);
        let agrarian_below = occs
            .iter()
            .filter(|&&(f, p)| f == Function::Agrarian && p < ORE_CUT)
            .count();
        assert!(
            agrarian_below > 0,
            "seed {seed_value}: every agrarian settlement sits above ORE_CUT \
             ({ORE_CUT}), so `every_mine_stands_on_ore_bearing_ground` would \
             pass on a world with no ore objective at all."
        );
    }
}

/// A mine is a specialisation, not the default. Spec §5.1's upper STOP: more
/// than 40% of all occupations being mines means the rate is wrong.
///
/// claim: rate(forall-seed over the panel — the Mine share of all occupations
/// stays under spec §5.1's 40% upper STOP)
#[test]
fn mines_are_a_minority_of_occupations() {
    for seed_value in SEEDS {
        let occs = functions_and_ore(seed_value);
        let mines = occs.iter().filter(|(f, _)| *f == Function::Mine).count();
        let share = mines as f64 / occs.len().max(1) as f64;
        assert!(
            share < 0.40,
            "seed {seed_value}: {mines} of {} occupations ({:.1}%) are mines — \
             spec §5.1's upper STOP is 40%.",
            occs.len(),
            share * 100.0
        );
    }
}

/// Every mine of one world, paired with the site of the community that founded
/// it and the **geosphere-adjacency** hop distance between the two.
///
/// # THE UNIT IS NOT THE UNIT THE SITING WORKS IN, AND THE DIFFERENCE IS REAL
///
/// The siting walk runs over the era `ConnectionGraph`, and that graph is
/// **not** a subgraph of the geosphere adjacency: `add_water_routes`
/// (`graph_derive.rs`) adds a `WaterRoute` edge from a coastal vertex to
/// whatever other coast a current reaches within
/// `GraphConfig::water_route_max_steps` (20 by default), so ONE graph hop can
/// be twenty vertices of open ocean. An earlier draft of this file asserted
/// "graph distance is never less than adjacency distance" and gated on it; the
/// gate failed on seed 7, where 6 of 19 mines sit 5-6 adjacency hops from
/// parents they are 3 graph hops from. The premise was false, not the design.
///
/// **The bake's own notion of "next door" includes a sailing lane**, and it
/// does so for every siting decision in the file, not just this one: the
/// agrarian daughter scan a few lines above calls the same
/// `traversable_neighbors`, so an ordinary farm daughter has always been
/// foundable across a lane at "one hop". A working supplied by sea is a better
/// supply relation than one supplied over 330 km of land, not a worse one.
///
/// So the hard bound is asserted where it is true — in graph rings, by
/// `history_bake.rs`'s `working_site_never_reaches_past_the_supply_bound` —
/// and what is asserted here is the observable consequence: a distribution
/// that is bounded, which an unbounded scan's is not (uncapped, the pooled
/// median founding ring is 5 and the maximum 38).
#[allow(clippy::disallowed_methods)] // decision 0092: named construction site
fn hops_from_parent(seed_value: u64) -> Vec<(Function, Vertex, Vertex, Option<u32>)> {
    let seed = Seed(seed_value);
    let world = build_world_to(
        seed,
        &SkyPins::default(),
        SkyChoice::Generated,
        &TerrainPins::default(),
        &SettlementPins::default(),
        &WorldComponents::assemble().expect("canonical components assemble"),
        BuildDepth::Settlements,
    )
    .expect("panel seed builds");
    let terrain = terrain_of(&world).expect("terrain reconstructs");
    let geo = terrain.geosphere();
    let records = occupation_records(&world);
    // The parent is named by entity, and an occupation's site never moves — a
    // community that relocates closes its record and opens another — so the
    // site on the parent's record is where it stood when it threw.
    let site_of: std::collections::BTreeMap<_, _> =
        records.iter().map(|r| (r.id, r.core.site)).collect();
    let mut out = Vec::new();
    for r in &records {
        let Founding::From(parent) = r.founded_from else {
            if r.core.function == Function::Mine {
                // Reported as a missing parent rather than skipped: a genesis
                // mine would mean a working was minted somewhere other than the
                // scan, which is spec §E.4.1's "impossible" row. A genesis
                // AGRARIAN occupation is ordinary and is simply not a daughter.
                out.push((r.core.function, r.core.site, r.core.site, None));
            }
            continue;
        };
        let from = *site_of
            .get(&parent)
            .expect("a daughter's founding parent is an occupation of the same world");
        // A generous window: the point is to MEASURE the distance, so the
        // bound must not be the search's own limit. 64 hops is a quarter of
        // the way round the globe at this mesh resolution.
        out.push((
            r.core.function,
            from,
            r.core.site,
            geo.hops_between(from, r.core.site, 64),
        ));
    }
    out
}

// # THE AMENDMENT'S OWN CLAIM HAS NO LEDGER-LEVEL GATE, AND THAT IS A FINDING
//
// A test named `a_working_may_be_founded_beyond_the_parents_own_neighbourhood`
// stood here, asserting that at least one mine on the panel stands more than
// one adjacency hop from its parent — spec §E.2's claim, apparently. **It was
// vacuous, and the mutation that was supposed to prove it wasn't is what
// showed that.** Reverting `working_site`'s walk to a single ring
// (`ring_no > WORKING_REACH` -> `ring_no > 1`, `scripts/mutate.py`) reproduces
// Task 2's worlds exactly — 1 / 13 / 2 mines over 1,240 / 661 / 898
// occupations — and the test still passed, on 8 of those 16 mines. Its doc
// comment asserted "under the one-hop scan every mine sat at exactly one hop,
// by construction", and that is simply false: a `WaterRoute` edge is one hop
// to the bake and up to twenty vertices of ocean to the geosphere, so a
// one-hop scan already places mines 4, 5 and 19 adjacency hops out.
//
// **No ledger read can distinguish a one-ring scan from a three-ring one**,
// because the distinction is in the era `ConnectionGraph` and no world commits
// one — `windows/worldgen` exposes no era-graph re-derivation, only
// `terrain_of`. So the claim is asserted where it is checkable, in graph
// rings, by `history_bake.rs`'s `working_site_never_reaches_past_the_supply_bound`
// (which the same mutation reds, on its "ore at exactly WORKING_REACH is
// workable" arm). What remains here is the observable consequence, below.

/// **The supply relation is bounded** (spec §E.2, §B.3's "from a parent that
/// supplies it"), and this reports the radius distribution spec §E.4.1
/// requires reported regardless of the verdict.
///
/// Two things are asserted, and both would hold under an unbounded scan only
/// by accident:
///
/// - **Every working has a founding parent.** A `Founding::Genesis` mine would
///   mean a working was minted somewhere other than [`working_site`]'s scan,
///   which is spec §E.4.1's "impossible" row.
/// - **The typical working is inside [`WORKING_REACH`] even measured in
///   adjacency hops**, the strict unit the siting does *not* work in (see
///   `hops_from_parent` for why the two differ). Uncapped, the pooled
///   median founding ring is 5 in the loose unit and no smaller in the strict
///   one, so this discriminates a bounded scan from an unbounded one.
///
/// # WHAT THE READOUT SAYS, AND IT IS THE NUMBER SPEC §E.4.1 ASKED FOR
///
/// ```text
///                       mines   radius: min  median  max   beyond reach
///   seed 42                16              1       3    3        0
///   seed 7                 19              1       3    6        6
///   seed 1234               4              1       5   19        2
///   pooled                 39              1       3   19        8  (20.5%)
///   ordinary daughters   2603              -       1    8       19  ( 0.7%)
/// ```
///
/// **Eight of 39 workings stand further from their parent than
/// [`WORKING_REACH`] adjacency hops, and one stands 19.** Every one of them
/// was three graph hops or fewer — the bound held — and the gap is sailing
/// lanes. The control below it is what makes that legible: ordinary
/// daughters, which have never looked past ring 1 and still do not, show the
/// same tail at 0.7%. The ring scan does not introduce lane-crossing, it
/// **compounds** it: a one-hop scan can take at most one lane, a three-ring
/// scan can chain three, which is how 19 adjacency hops (~2,100 km at this
/// mesh's ~110 km spacing) fits inside a bound of three.
///
/// That is reported rather than repaired, and deliberately. Excluding
/// `EdgeKind::WaterRoute` from the working scan alone would give it a
/// traversal no other siting decision in `history_bake.rs` uses, which is a
/// larger change than spec §E.2 authorises; §E.4.1's row for this case says
/// *report the distribution*, which is what this test does.
///
/// claim: invariant(forall-seed over the panel — every `Function::Mine` has a
/// founding parent occupation, and the pooled median parent-to-mine adjacency
/// distance is within `WORKING_REACH`)
#[test]
fn a_workings_founding_radius_stays_inside_the_supply_bound() {
    let mut pooled: Vec<u32> = Vec::new();
    let mut pooled_agrarian: Vec<u32> = Vec::new();
    for seed_value in SEEDS {
        let rows = hops_from_parent(seed_value);
        let mut hops: Vec<u32> = Vec::new();
        let mut agrarian: Vec<u32> = Vec::new();
        for &(function, from, site, h) in &rows {
            let h = h.unwrap_or_else(|| {
                panic!(
                    "seed {seed_value}: the mine at {site:?} has no founding parent \
                     within reach (parent site {from:?}). Spec §E.4.1: a founding \
                     was minted somewhere other than the scan."
                )
            });
            if function == Function::Mine {
                hops.push(h);
            } else {
                agrarian.push(h);
            }
        }
        hops.sort_unstable();
        agrarian.sort_unstable();
        assert!(
            !hops.is_empty(),
            "seed {seed_value} founded no working, so its radius distribution \
             is empty — see `mines_are_founded_on_the_panel`."
        );
        println!(
            "seed {seed_value}: {} mines, adjacency radius min {} median {} max {} — {:?}",
            hops.len(),
            hops[0],
            hops[hops.len() / 2],
            hops[hops.len() - 1],
            histogram(&hops),
        );
        println!(
            "           {} ordinary daughters, adjacency radius median {} max {} \
             ({} beyond WORKING_REACH)",
            agrarian.len(),
            agrarian[agrarian.len() / 2],
            agrarian[agrarian.len() - 1],
            agrarian.iter().filter(|&&h| h > WORKING_REACH).count(),
        );
        pooled.extend(hops);
        pooled_agrarian.extend(agrarian);
    }
    pooled_agrarian.sort_unstable();
    pooled.sort_unstable();
    let median = pooled[pooled.len() / 2];
    let within = pooled.iter().filter(|&&h| h <= WORKING_REACH).count();
    println!(
        "POOLED: {} mines, adjacency radius min {} median {median} max {} — {:?} \
         ({within} of {} within WORKING_REACH = {WORKING_REACH})",
        pooled.len(),
        pooled[0],
        pooled[pooled.len() - 1],
        histogram(&pooled),
        pooled.len(),
    );
    // THE CONTROL, and the reason a long tail here is not this amendment's
    // doing: the ORDINARY daughter scan has never looked past ring 1, and it
    // still does not, yet its foundings show the same tail — because a
    // `WaterRoute` edge is ring 1 to the bake and up to twenty vertices of
    // ocean to the geosphere. Whatever share of workings sits beyond
    // `WORKING_REACH` in adjacency hops, the farms are already there.
    println!(
        "POOLED: {} ordinary daughters, adjacency radius median {} max {} ({} beyond \
         WORKING_REACH, {:.2}% — against {:.2}% of workings)",
        pooled_agrarian.len(),
        pooled_agrarian[pooled_agrarian.len() / 2],
        pooled_agrarian[pooled_agrarian.len() - 1],
        pooled_agrarian
            .iter()
            .filter(|&&h| h > WORKING_REACH)
            .count(),
        pooled_agrarian
            .iter()
            .filter(|&&h| h > WORKING_REACH)
            .count() as f64
            / pooled_agrarian.len() as f64
            * 100.0,
        (pooled.len() - within) as f64 / pooled.len() as f64 * 100.0,
    );
    assert!(
        median <= WORKING_REACH,
        "the pooled median parent-to-mine distance is {median} hops against a \
         WORKING_REACH of {WORKING_REACH}. A working is a daughter founded on \
         ore FROM A PARENT THAT SUPPLIES IT (spec §B.3); a scan whose typical \
         founding sits outside its own bound is not bounded in any useful \
         sense. Distribution: {:?}",
        histogram(&pooled),
    );
}

/// `(distance, count)` pairs in ascending distance, for the radius readouts.
fn histogram(sorted: &[u32]) -> Vec<(u32, usize)> {
    let mut out: Vec<(u32, usize)> = Vec::new();
    for &h in sorted {
        match out.last_mut() {
            Some((d, n)) if *d == h => *n += 1,
            _ => out.push((h, 1)),
        }
    }
    out
}

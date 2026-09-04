//! The Staple, Task 0: are the `Brief`'s axes REACHABLE?
//!
//! §1b.5 states the rung-2 target as "a `Trade` + `Seat` + `Classical` +
//! high-population coastal site draws docks, warehouse row, market, curia,
//! temple precinct, uptown villas, tenements, walls and gates, an extramural
//! suburb". Every axis it names is a `Brief` field. This probe measures the
//! JOINT distribution of those axes over the population a brief can actually
//! be taken at — ALIVE occupations — so the campaign learns before it
//! authors a vocabulary whether that vocabulary can ever be drawn.
#![allow(clippy::disallowed_methods)]

use hornvale_astronomy::SkyPins;
use hornvale_history::record::{Function, Notability, TechHorizon};
use hornvale_kernel::Seed;
use hornvale_terrain::TerrainPins;
use hornvale_worldgen::{
    SettlementPins, SkyChoice, build_world, occupations_by_vertex, terrain_of,
};

const SEEDS: [u64; 5] = [42, 7, 13, 100, 1234];

/// claim: readout(off-gate, prints only, no assertion) - the joint distribution of the
/// `Brief`'s axes over the population a brief can be taken at. Decision 0093: a seed loop
/// is a quantified claim, and this one quantifies a DISTRIBUTION, not a threshold - it
/// exists to say what varies and what does not, and every number The Staple's metaplan
/// rests on comes from here. Deliberately assertion-free: a ratchet here would freeze a
/// degenerate distribution as though it were intended.
#[test]
#[ignore = "probe: The Staple Task 0 brief-axis reachability; run by hand"]
fn brief_axis_probe() {
    let ceiling = hornvale_history::flesh::HAMLET_POPULATION_CEILING;
    println!("HAMLET_POPULATION_CEILING = {ceiling}");
    for seed_value in SEEDS {
        let seed = Seed(seed_value);
        let world = build_world(
            seed,
            &SkyPins::default(),
            SkyChoice::Generated,
            &TerrainPins::default(),
            &SettlementPins::default(),
        )
        .expect("probe seed builds");
        let terrain = terrain_of(&world).expect("terrain");
        let by_vertex = occupations_by_vertex(&world);

        let mut peoples: std::collections::BTreeMap<&'static str, usize> =
            std::collections::BTreeMap::new();
        // Sedimentary: how many ENDED occupations lie under each alive one.
        let mut strata: Vec<usize> = Vec::new();
        let mut alive = 0usize;
        let mut dead = 0usize;
        let mut dfunc = [0usize; 5];
        let mut dnota = [0usize; 3];
        let mut dtech = [0usize; 4];
        let mut dpops: Vec<u32> = Vec::new();
        let mut func = [0usize; 5];
        let mut nota = [0usize; 3];
        let mut tech = [0usize; 4];
        let mut pops: Vec<u32> = Vec::new();
        let mut populous = 0usize;
        let mut coastal_alive = 0usize;
        let mut seat_and_classical = 0usize;
        let mut trade_seat_classical = 0usize;
        let mut worked_example = 0usize;
        let mut bronze_or_better_fort_or_seat = 0usize;

        for (&vertex, occs) in &by_vertex {
            // Coastal: a land vertex with an ocean neighbour.
            let coastal = !terrain.is_ocean(vertex)
                && terrain
                    .geosphere()
                    .neighbors(vertex)
                    .iter()
                    .any(|&n| terrain.is_ocean(n));
            for o in occs {
                if o.core.ended.is_some() {
                    dead += 1;
                    dfunc[o.core.function as usize] += 1;
                    dnota[o.core.notability as usize] += 1;
                    dtech[o.core.tech as usize] += 1;
                    dpops.push(o.core.peak_population);
                    continue;
                }
                alive += 1;
                *peoples.entry(o.core.people.0).or_default() += 1;
                strata.push(occs.iter().filter(|x| x.core.ended.is_some()).count());
                func[o.core.function as usize] += 1;
                nota[o.core.notability as usize] += 1;
                tech[o.core.tech as usize] += 1;
                pops.push(o.core.peak_population);
                if o.core.peak_population > ceiling {
                    populous += 1;
                }
                if coastal {
                    coastal_alive += 1;
                }
                let is_seat = o.core.notability == Notability::Seat;
                let is_classical = o.core.tech == TechHorizon::Classical;
                let is_trade = o.core.function == Function::Trade;
                if is_seat && is_classical {
                    seat_and_classical += 1;
                }
                if is_trade && is_seat && is_classical {
                    trade_seat_classical += 1;
                }
                if is_trade
                    && is_seat
                    && is_classical
                    && o.core.peak_population > ceiling
                    && coastal
                {
                    worked_example += 1;
                }
                if o.core.tech >= TechHorizon::Bronze
                    && (o.core.function == Function::Fort || is_seat)
                {
                    bronze_or_better_fort_or_seat += 1;
                }
            }
        }
        pops.sort_unstable();
        let max = pops.last().copied().unwrap_or(0);
        let med = pops.get(pops.len() / 2).copied().unwrap_or(0);
        println!("\n== seed {seed_value} ==  alive {alive}  ended {dead}");
        println!(
            "  function   Agrarian {} Mine {} Trade {} Cult {} Fort {}",
            func[0], func[1], func[2], func[3], func[4]
        );
        println!(
            "  notability Backwater {} Common {} Seat {}",
            nota[0], nota[1], nota[2]
        );
        println!(
            "  tech       Neolithic {} Bronze {} Iron {} Classical {}",
            tech[0], tech[1], tech[2], tech[3]
        );
        println!("  peak_population  median {med}  max {max}  > ceiling: {populous}");
        println!("  coastal alive {coastal_alive}");
        println!("  Seat & Classical            {seat_and_classical}");
        println!("  Trade & Seat & Classical    {trade_seat_classical}");
        println!("  tech>=Bronze & (Fort|Seat)  {bronze_or_better_fort_or_seat}");
        println!("  THE WORKED EXAMPLE (+pop +coastal) {worked_example}");
        strata.sort_unstable();
        println!(
            "  PEOPLE (KindId -> alive count): {:?}   distinct {}",
            peoples,
            peoples.len()
        );
        println!(
            "  STRATA (ended occupations under an alive one): min {} median {} max {}",
            strata.first().copied().unwrap_or(0),
            strata.get(strata.len() / 2).copied().unwrap_or(0),
            strata.last().copied().unwrap_or(0),
        );
        dpops.sort_unstable();
        println!(
            "  ENDED: function A {} M {} T {} C {} F {} | notability B {} C {} S {} | tech N {} B {} I {} C {} | peakmax {}",
            dfunc[0],
            dfunc[1],
            dfunc[2],
            dfunc[3],
            dfunc[4],
            dnota[0],
            dnota[1],
            dnota[2],
            dtech[0],
            dtech[1],
            dtech[2],
            dtech[3],
            dpops.last().copied().unwrap_or(0),
        );
    }
}

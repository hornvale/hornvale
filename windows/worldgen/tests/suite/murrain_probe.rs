//! The Murrain, Task 0: how large is the connected population a pathogen
//! would have to persist in?
//!
//! A crowd disease persists only above a critical community size — the
//! supply of new susceptibles has to outrun the pathogen's burn-through —
//! and the size that matters is not one community's but the CONNECTED
//! METAPOPULATION's: every community a pathogen can reach over the transport
//! graph before it fades. Before the campaign freezes its prediction (spec
//! §8) this probe measures, from the committed occupation facts and the
//! bake's own per-era connection graphs, the largest such metapopulation any
//! of the nine seeds ever holds, proxied as the sum of `peak_population`
//! over the occupations alive in an era at the sites of one connected
//! component — an UPPER bound, since a community spends part of every tenure
//! below its peak. Every population number The Murrain's spec rests on comes
//! from here.
#![allow(clippy::disallowed_methods)]

use hornvale_astronomy::SkyPins;
use hornvale_kernel::{Seed, Vertex};
use hornvale_terrain::TerrainPins;
use hornvale_worldgen::{
    SettlementPins, bake_era_graphs, build_world, occupation_records, present_year,
};
use std::collections::{BTreeMap, BTreeSet};

/// The Living Community's cross-seed sweep, so the rows line up with
/// `book/src/laboratory/generated/the-history/rows.csv` and with
/// `lot_probe.rs`.
const SEEDS: [u64; 9] = [1, 2, 3, 7, 13, 42, 100, 256, 777];

/// Metapopulation thresholds to count against: the literature's critical
/// community sizes for the acute immunising crowd diseases sit in the
/// hundreds of thousands (measles ~250,000–500,000; smallpox and pertussis of
/// the same order); the lower rungs are here so the readout shows HOW FAR
/// under the bar these worlds sit, not just that they are under it.
const THRESHOLDS: [f64; 4] = [1_000.0, 10_000.0, 100_000.0, 250_000.0];

/// The bake's own traversability rule (`history_bake::traversable_neighbors`
/// filters `conductance > 0.0`); `reachable_regions` takes `>=`, so the
/// smallest positive floor reproduces the same partition.
const TRAVERSABLE: f64 = f64::MIN_POSITIVE;

/// claim: readout(off-gate, prints only, no assertion) - the connected
/// metapopulation a pathogen would persist in, over the committed occupation
/// facts and the bake's own era graphs. Decision 0093: a seed loop is a
/// quantified claim, and this one quantifies a DISTRIBUTION, not a threshold.
/// It is deliberately assertion-free: the spec freezes the prediction from
/// this reading, and a ratchet here would freeze whatever the world happens
/// to do as though it were intended.
#[test]
#[ignore = "probe: The Murrain Task 0 metapopulation shape; run by hand"]
fn murrain_probe() {
    for seed_value in SEEDS {
        let seed = Seed(seed_value);
        let world = build_world(
            seed,
            &SkyPins::default(),
            &TerrainPins::default(),
            &SettlementPins::default(),
        )
        .expect("probe seed builds");
        let now = present_year(&world);
        let occs = occupation_records(&world);
        let eras = bake_era_graphs(&world).expect("era graphs derive");

        // Community size: the distribution of peaks, and the bands the spec
        // will speak in.
        let mut peaks: Vec<u32> = occs.iter().map(|o| o.core.peak_population).collect();
        peaks.sort_unstable();
        let pct = |q: f64| -> u32 {
            if peaks.is_empty() {
                0
            } else {
                peaks[((peaks.len() - 1) as f64 * q).round() as usize]
            }
        };
        let bands = [
            ("<10", 0u32, 10u32),
            ("10-24", 10, 25),
            ("25-49", 25, 50),
            ("50-89", 50, 90),
            (">=90", 90, u32::MAX),
        ];
        let band_counts: Vec<String> = bands
            .iter()
            .map(|(label, lo, hi)| {
                let n = peaks.iter().filter(|&&p| p >= *lo && p < *hi).count();
                format!("{label}:{n}")
            })
            .collect();

        // Per era: the connected components of the bake's graph for that
        // era, and the summed peaks of the occupations alive in it, per
        // component.
        let mut max_meta = (0.0f64, 0usize, 0usize); // (metapop, era index, occs in it)
        let mut present_meta = (0.0f64, 0usize, 0usize);
        let mut over: [usize; 4] = [0; 4]; // (era, component) pairs at or over each threshold
        let mut era_lines: Vec<String> = Vec::new();
        let mut present_degree: Vec<usize> = Vec::new();
        for (e, (era_start, graph)) in eras.iter().enumerate() {
            let era_end = eras.get(e + 1).map_or(now, |(y, _)| *y);
            let alive: Vec<usize> = (0..occs.len())
                .filter(|&i| {
                    let o = &occs[i].core;
                    o.founded < era_end && o.ended.unwrap_or(now) > *era_start
                })
                .collect();
            let regions = graph.reachable_regions(TRAVERSABLE);
            let mut region_of: BTreeMap<Vertex, usize> = BTreeMap::new();
            for (r, region) in regions.iter().enumerate() {
                for &v in region {
                    region_of.insert(v, r);
                }
            }
            let mut meta: BTreeMap<usize, (f64, usize)> = BTreeMap::new();
            let mut world_total = 0.0;
            for &i in &alive {
                let o = &occs[i].core;
                let r = region_of.get(&o.site).copied().unwrap_or(usize::MAX);
                let entry = meta.entry(r).or_insert((0.0, 0));
                entry.0 += f64::from(o.peak_population);
                entry.1 += 1;
                world_total += f64::from(o.peak_population);
            }
            let (largest, n_in) = meta
                .values()
                .copied()
                .max_by(|a, b| a.0.total_cmp(&b.0).then(a.1.cmp(&b.1)))
                .unwrap_or((0.0, 0));
            for (k, t) in THRESHOLDS.iter().enumerate() {
                over[k] += meta.values().filter(|(m, _)| *m >= *t).count();
            }
            if largest > max_meta.0 {
                max_meta = (largest, e, n_in);
            }
            // "Present" is the last era that BEGINS before the present year:
            // the final era starts exactly at `now` (the bake's end year), so
            // nothing is alive inside it and it reads as an empty world.
            let is_present = *era_start < now && eras.get(e + 1).is_none_or(|(y, _)| *y >= now);
            if is_present {
                present_meta = (largest, e, n_in);
                // Contact structure at the present era: how many OCCUPIED
                // neighbours each occupied site has over traversable edges.
                let occupied: BTreeSet<Vertex> = alive.iter().map(|&i| occs[i].core.site).collect();
                for &v in &occupied {
                    let mut ns: Vec<Vertex> = graph
                        .edges(v)
                        .iter()
                        .filter(|edge| edge.conductance > 0.0 && occupied.contains(&edge.to))
                        .map(|edge| edge.to)
                        .collect();
                    ns.sort();
                    ns.dedup();
                    present_degree.push(ns.len());
                }
            }
            era_lines.push(format!(
                "{e:>2} y{:>5.0} alive={:<4} comps={:<3} largest={:>7.0} ({} occs) world={:>7.0}",
                era_start,
                alive.len(),
                meta.len(),
                largest,
                n_in,
                world_total
            ));
        }
        present_degree.sort_unstable();
        let isolates = present_degree.iter().filter(|&&d| d == 0).count();
        let mean_degree = if present_degree.is_empty() {
            0.0
        } else {
            present_degree.iter().sum::<usize>() as f64 / present_degree.len() as f64
        };
        let max_degree = present_degree.last().copied().unwrap_or(0);

        println!("== seed {seed_value} ==");
        println!(
            "  occupations {}  peaks: max {} p90 {} p50 {}  bands {}",
            occs.len(),
            pct(1.0),
            pct(0.9),
            pct(0.5),
            band_counts.join(" ")
        );
        println!(
            "  largest metapopulation EVER  {:>8.0}  (era {}, {} occupations)",
            max_meta.0, max_meta.1, max_meta.2
        );
        println!(
            "  largest metapopulation NOW   {:>8.0}  (era {}, {} occupations)",
            present_meta.0, present_meta.1, present_meta.2
        );
        let over_line: Vec<String> = THRESHOLDS
            .iter()
            .zip(over.iter())
            .map(|(t, n)| format!(">={t:.0}:{n}"))
            .collect();
        println!(
            "  (era, component) pairs at or over a threshold, all {} eras: {}",
            eras.len(),
            over_line.join("  ")
        );
        println!(
            "  present contact: occupied sites {}  mean occupied-degree {:.2}  max {}  isolates {}",
            present_degree.len(),
            mean_degree,
            max_degree,
            isolates
        );
        println!("  per era (start year, alive occs, components, largest, world total):");
        for line in &era_lines {
            println!("    {line}");
        }
    }
}

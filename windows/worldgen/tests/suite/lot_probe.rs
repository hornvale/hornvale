//! The Lot, Task 0: what does the population a life is drawn FROM look like?
//!
//! Any Human Ever draws one life from everyone who ever lived, weighted by
//! births over time and place. Hornvale's ledger commits, per occupation,
//! only `founded`, `ended` and `peak_population` — never the population
//! trajectory. Before the campaign decides what to commit (spec §4), this
//! probe measures, from the committed facts alone, the shape of the
//! population a lot would be drawn from: how person-years (proxied here as
//! `tenure × peak`, an UPPER bound) distribute over the bake's four
//! quarters, over peoples, over community size, and over the fate the
//! community met. Every number The Lot's spec rests on comes from here.
#![allow(clippy::disallowed_methods)]

use hornvale_astronomy::SkyPins;
use hornvale_history::record::CauseOfEnd;
use hornvale_kernel::Seed;
use hornvale_terrain::TerrainPins;
use hornvale_worldgen::{SettlementPins, SkyChoice, build_world, occupation_records, present_year};

/// The Living Community's cross-seed sweep, so the rows line up with
/// `book/src/laboratory/generated/the-history/rows.csv`.
const SEEDS: [u64; 9] = [1, 2, 3, 7, 13, 42, 100, 256, 777];
/// The bake span (`BakeConfig::default_millennia`): years 0..2000 in 25-year
/// epochs. Stated here rather than read, because the probe reads the LEDGER
/// and the ledger does not carry the config.
const SPAN_YEARS: f64 = 2000.0;
const EPOCH_YEARS: f64 = 25.0;

/// claim: readout(off-gate, prints only, no assertion) - the distribution a lot is
/// drawn from, over the committed occupation facts. Decision 0093: a seed loop is a
/// quantified claim, and this one quantifies a DISTRIBUTION, not a threshold. It is
/// deliberately assertion-free: a ratchet here would freeze whatever the world happens
/// to do as though it were intended.
#[test]
#[ignore = "probe: The Lot Task 0 draw-population shape; run by hand"]
fn lot_probe() {
    let epochs = (SPAN_YEARS / EPOCH_YEARS) as usize;
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
        let now = present_year(&world);
        let occs = occupation_records(&world);

        // Per-epoch: how many occupations are alive, and the sum of their peaks
        // (an upper bound on the population alive in that epoch).
        let mut alive_by_epoch = vec![0usize; epochs];
        let mut peak_sum_by_epoch = vec![0u64; epochs];
        // Person-years proxy per occupation: tenure × peak.
        let mut py_total = 0.0;
        let mut py_quarter = [0.0f64; 4];
        let mut py_by_people: std::collections::BTreeMap<&'static str, f64> =
            std::collections::BTreeMap::new();
        let mut py_by_size = [0.0f64; 4]; // peak <10, <25, <50, >=50
        let mut py_by_fate: std::collections::BTreeMap<String, f64> =
            std::collections::BTreeMap::new();
        let mut tenures: Vec<f64> = Vec::new();
        let mut peaks: Vec<u32> = Vec::new();
        let mut alive = 0usize;
        let mut ended = 0usize;
        let mut zero_peak = 0usize;

        for o in &occs {
            let founded = o.core.founded;
            let end = o.core.ended.unwrap_or(now);
            let tenure = (end - founded).max(0.0);
            let peak = o.core.peak_population;
            if peak == 0 {
                zero_peak += 1;
            }
            if o.core.ended.is_some() {
                ended += 1;
            } else {
                alive += 1;
            }
            tenures.push(tenure);
            peaks.push(peak);
            let py = tenure * f64::from(peak);
            py_total += py;
            *py_by_people.entry(o.core.people.0).or_default() += py;
            let size_bin = match peak {
                0..=9 => 0,
                10..=24 => 1,
                25..=49 => 2,
                _ => 3,
            };
            py_by_size[size_bin] += py;
            let fate = match o.core.cause {
                None => "alive".to_string(),
                Some(c) => format!("{c:?}"),
            };
            *py_by_fate.entry(fate).or_default() += py;
            // Spread the proxy over the epochs the occupation overlaps.
            for (e, slot) in alive_by_epoch.iter_mut().enumerate() {
                let e0 = e as f64 * EPOCH_YEARS;
                let e1 = e0 + EPOCH_YEARS;
                let overlap = (end.min(e1) - founded.max(e0)).max(0.0);
                if overlap > 0.0 {
                    *slot += 1;
                    peak_sum_by_epoch[e] += u64::from(peak);
                    let q = ((e0 / SPAN_YEARS) * 4.0) as usize;
                    py_quarter[q.min(3)] += overlap * f64::from(peak);
                }
            }
        }
        tenures.sort_by(|a, b| a.total_cmp(b));
        peaks.sort_unstable();
        let q = |v: &[f64], p: f64| {
            v.get(((v.len() as f64) * p) as usize)
                .copied()
                .unwrap_or(0.0)
        };
        let qi = |v: &[u32], p: f64| v.get(((v.len() as f64) * p) as usize).copied().unwrap_or(0);

        println!(
            "\n== seed {seed_value} ==  now {now}  occupations {}  alive {alive}  ended {ended}  peak==0 {zero_peak}",
            occs.len()
        );
        println!(
            "  tenure yrs   p10 {:.0}  p50 {:.0}  p90 {:.0}  max {:.0}",
            q(&tenures, 0.1),
            q(&tenures, 0.5),
            q(&tenures, 0.9),
            tenures.last().copied().unwrap_or(0.0)
        );
        println!(
            "  peak         p10 {}  p50 {}  p90 {}  max {}",
            qi(&peaks, 0.1),
            qi(&peaks, 0.5),
            qi(&peaks, 0.9),
            peaks.last().copied().unwrap_or(0)
        );
        println!("  person-years proxy (tenure x peak) total {py_total:.0}");
        println!(
            "  by quarter of span   {:.3} {:.3} {:.3} {:.3}",
            py_quarter[0] / py_total,
            py_quarter[1] / py_total,
            py_quarter[2] / py_total,
            py_quarter[3] / py_total
        );
        let mut peoples: Vec<_> = py_by_people.iter().collect();
        peoples.sort_by(|a, b| b.1.total_cmp(a.1));
        let people_line: Vec<String> = peoples
            .iter()
            .map(|(k, v)| format!("{k} {:.3}", *v / py_total))
            .collect();
        println!("  by people            {}", people_line.join("  "));
        println!(
            "  by peak size         <10 {:.3}  10-24 {:.3}  25-49 {:.3}  >=50 {:.3}",
            py_by_size[0] / py_total,
            py_by_size[1] / py_total,
            py_by_size[2] / py_total,
            py_by_size[3] / py_total
        );
        let mut fates: Vec<_> = py_by_fate.iter().collect();
        fates.sort_by(|a, b| b.1.total_cmp(a.1));
        let fate_line: Vec<String> = fates
            .iter()
            .map(|(k, v)| format!("{k} {:.3}", *v / py_total))
            .collect();
        println!("  by community fate    {}", fate_line.join("  "));
        // The curve itself, one column per 100 years: alive occupations / sum of peaks.
        let curve: Vec<String> = (0..epochs)
            .step_by(4)
            .map(|e| format!("{}/{}", alive_by_epoch[e], peak_sum_by_epoch[e]))
            .collect();
        println!("  curve (per 100y: alive occs / sum of peaks)");
        println!("    {}", curve.join(" "));
        let _ = CauseOfEnd::Famine; // the fate map is keyed on the enum's Debug names
    }
}

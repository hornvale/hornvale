//! Is the raid proxy ambiguous, or is its population stale?
//!
//! # It landed on P1, and this file outlived the file it was written about
//!
//! The verdict (seeds `1..=30`): `census().raided` pooled **6163**, the
//! UNFILTERED per-record count **6163** — exact on all thirty seeds, none
//! disagreeing — and the count filtered to the frozen six **5782**, whose
//! **381**-record gap is *exactly* the out-of-population victims (desert-dwarf
//! 182, hill-dwarf 174, gully-dwarf 25). Attribution is perfectly unique; the
//! panic message was an inherited diagnosis and it was wrong.
//!
//! So the repair was **not** to fix the filter. The Confusion retired
//! `tolerance_baseline.rs` outright — its two readouts served The Tolerance's
//! preregistration, which is discharged (`book/src/chronicle/the-tolerance.md`
//! carries the H1/H2/H3 verdicts) — and migrated the quantities onto the
//! 1,000-world census as `raid-victim-rate`, `raid-initiator-rate` and
//! `raid-attribution-unresolved` (`windows/lab/src/metrics.rs`).
//!
//! **This file is deliberately kept, and not as a memento.** It is the only
//! place the ledger-side proxy is checked against the bake's OWN counter.
//! `BakeCensus` lives on `History::tally`, which `build_world_to` discards
//! after `emit_history`, and a census view holds only a `World` — so no metric
//! can reach it, and the census-wide invariant
//! (`raid-attribution-unresolved`) can only ask whether each `Ended::By`
//! reference resolves, never whether the set of them matches what the bake
//! counted. That second question needs a live `History`, which is what this
//! probe builds.
//!
//! The original framing follows, unchanged.
//!
//! `tolerance_baseline.rs`'s two guards fail at the nine-people roster with a
//! message asserting that **"`Ended::By(raider)` no longer names exactly one
//! occupation record"**. That sentence is an *inherited diagnosis* — The
//! Tolerance wrote it into the panic text when the guard was built, describing
//! the failure it imagined, not one it had seen. This probe settles which of
//! two rival explanations is true, because they predict different numbers.
//!
//! **P1 — the frozen population expired.** `measure_one` filters its numerator
//! to `PEOPLES_AS_OF_THE_GENERALIST` (the six peoples that existed when The
//! Generalist measured) and compares it against `census().raided`, which counts
//! the **whole world**. That comparison is only valid under the premise the
//! module doc states outright: *"Every occupation record in the shipped roster
//! belongs to one of the six settling peoples."* Three new settling peoples make
//! it false. If P1 holds, attribution is still perfectly unique and the gap is
//! exactly the out-of-population victims.
//!
//! **P2 — attribution really is ambiguous**, as the message claims: some
//! `Ended::By(raider)` names zero records or several, so no filter can
//! reconcile the two counts.
//!
//! **The decisive quantity is the UNFILTERED per-record count.** Drop the
//! population filter and count every `Fled + Ended::By` record in the world:
//!
//! - equals `census().raided` on every seed  => **P1**, and the message is wrong;
//! - differs on any seed                     => **P2** survives, and by how much.
//!
//! Frozen before running. The probe reports both readings either way and
//! asserts only the branch it lands in, so it cannot be read after the fact as
//! having predicted whichever happened.

use hornvale_astronomy::SkyPins;
use hornvale_history::record::{CauseOfEnd, Ended};
use hornvale_kernel::Seed;
use hornvale_terrain::TerrainPins;
use hornvale_worldgen::seed_sweep;
use hornvale_worldgen::{SettlementPins, WorldComponents, census, history_for};
use std::collections::BTreeMap;

/// The same range `tolerance_baseline.rs` pools its readings over, so this
/// probe speaks about exactly the worlds whose guards failed.
const SEEDS: std::ops::RangeInclusive<u64> = 1..=30;

/// `tolerance_baseline.rs`'s frozen population, mirrored verbatim.
const PEOPLES_AS_OF_THE_GENERALIST: [&str; 6] =
    ["bugbear", "gnoll", "goblin", "hobgoblin", "human", "kobold"];

/// claim: invariant(forall-seed 1..=30, heavy:) — the ledger-side raid proxy
/// reconciles EXACTLY with the bake's own `census().raided` on every seed.
/// Kept as an invariant and not retired with the readouts it diagnosed: this
/// comparison is unreachable from the census, because `History::tally` is
/// discarded after `emit_history` and a metric holds only a `World`.
///
/// nextest: sized-sweep
#[test]
#[ignore = "heavy: live-worldgen battery; deferred from the commit gate to the heavy set (decision 0132)"]
fn is_the_raid_proxy_ambiguous_or_is_its_population_stale() {
    let wc = WorldComponents::assemble().expect("canonical registries are well-formed");

    // One seed's raw per-record tally: (census().raided, unfiltered count,
    // filtered-to-the-six count, whether unfiltered disagreed with the
    // census, and the out-of-population victims by people). Built in
    // parallel below and reduced to the pooled totals afterward, so the
    // reduction is pure integer addition — commutative and exact, hence
    // order-independent regardless of which worker finishes first.
    type SeedTally = (u64, u64, u64, bool, BTreeMap<&'static str, u64>);

    // The seed sweep runs across the machine's CPUs (see
    // `seed_sweep::map_seeds`), but the pooled totals below are
    // byte-identical to the serial loop this replaced: each seed's tally is
    // a pure function of its seed, each worker shares nothing, and results
    // come back in SEED order — matched back up with `SEEDS.zip(...)` below,
    // the same pairing the serial loop produced one seed at a time. Set
    // `HV_SEED_SWEEP_THREADS=1` to reproduce that serial loop exactly.
    let per_seed: Vec<SeedTally> = seed_sweep::map_seeds(SEEDS, |seed_value| {
        let seed = Seed(seed_value);
        let history = history_for(
            seed,
            &SkyPins::default(),
            &TerrainPins::default(),
            &SettlementPins::default(),
            &wc,
        )
        .unwrap_or_else(|e| panic!("{seed:?} failed to build: {e:?}"));

        let mut unfiltered: u64 = 0;
        let mut filtered: u64 = 0;
        let mut out_of_population: BTreeMap<&'static str, u64> = BTreeMap::new();
        for rec in &history.records {
            let raided = matches!(rec.core.cause, Some(CauseOfEnd::Fled))
                && matches!(rec.ended_by, Ended::By(_));
            if !raided {
                continue;
            }
            unfiltered += 1;
            let people = rec.core.people.0;
            if PEOPLES_AS_OF_THE_GENERALIST.contains(&people) {
                filtered += 1;
            } else {
                *out_of_population.entry(people).or_default() += 1;
            }
        }

        let tally = census(&history);
        let disagreed = unfiltered != tally.raided;
        (
            tally.raided,
            unfiltered,
            filtered,
            disagreed,
            out_of_population,
        )
    });

    // Totals pooled over the seed range, plus the per-seed disagreement list
    // that decides the branch — reduced from `per_seed` in the exact order
    // the serial loop accumulated them.
    let mut census_raided_total: u64 = 0;
    let mut unfiltered_total: u64 = 0;
    let mut filtered_total: u64 = 0;
    let mut seeds_disagreeing_unfiltered: Vec<u64> = Vec::new();
    let mut out_of_population: BTreeMap<&'static str, u64> = BTreeMap::new();

    for (seed_value, (raided, unfiltered, filtered, disagreed, seed_out_of_population)) in
        SEEDS.zip(per_seed)
    {
        if disagreed {
            seeds_disagreeing_unfiltered.push(seed_value);
        }
        census_raided_total += raided;
        unfiltered_total += unfiltered;
        filtered_total += filtered;
        for (people, n) in seed_out_of_population {
            *out_of_population.entry(people).or_default() += n;
        }
    }

    let gap = census_raided_total - filtered_total;
    let out_total: u64 = out_of_population.values().sum();

    println!("=== raid attribution probe, seeds {SEEDS:?} ===");
    println!("census().raided pooled            {census_raided_total}");
    println!("per-record, UNFILTERED            {unfiltered_total}");
    println!("per-record, filtered to the six   {filtered_total}");
    println!("gap (census - filtered)           {gap}");
    println!("out-of-population victims         {out_total}");
    println!("seeds where UNFILTERED disagrees  {seeds_disagreeing_unfiltered:?}");
    for (people, n) in &out_of_population {
        println!("  out-of-population victim: {people:<14} {n}");
    }

    // THE BRANCH. Asserted, not narrated — and only the branch the numbers
    // actually landed in.
    if seeds_disagreeing_unfiltered.is_empty() {
        // P1. State it as the two equalities that make it a diagnosis rather
        // than a coincidence: the unfiltered count reconciles exactly, AND the
        // gap is fully explained by victims outside the frozen population.
        assert_eq!(
            unfiltered_total, census_raided_total,
            "P1: the unfiltered per-record count must reconcile with the census exactly"
        );
        assert_eq!(
            gap, out_total,
            "P1: the gap must be EXACTLY the out-of-population victims — if these differ, \
             something else is also wrong and P1 is not the whole story"
        );
        assert!(
            out_total > 0,
            "P1 is vacuous if no victim falls outside the frozen population: the probe would \
             then be asserting a tautology on a roster where the premise still holds"
        );
        println!(
            "\nVERDICT: P1. Attribution is UNIQUE — every Ended::By names exactly one \
             record. The guard's own message misdiagnoses its failure: the numerator is \
             frozen to six peoples and the denominator is not."
        );
    } else {
        // P2. Report the size of the genuine ambiguity rather than only its
        // existence, since the repair differs by magnitude.
        let ambiguity = unfiltered_total.abs_diff(census_raided_total);
        println!(
            "\nVERDICT: P2. The unfiltered count disagrees with the census on {} of {} seeds \
             by {ambiguity} records pooled — a genuine attribution ambiguity, and the \
             message's diagnosis stands.",
            seeds_disagreeing_unfiltered.len(),
            SEEDS.count(),
        );
        panic!(
            "P2: unfiltered per-record count {unfiltered_total} != census {census_raided_total}; \
             attribution is genuinely ambiguous on seeds {seeds_disagreeing_unfiltered:?}"
        );
    }
}

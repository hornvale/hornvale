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
//! # THE POPULATION IS SMALL, AND THAT IS THE CAMPAIGN'S FINDING, NOT A BUG
//!
//! Measured at the commit that introduced the objective (2026-08-29):
//!
//! ```text
//!             mines   occupations   share
//! seed 42         1          1240   0.08%
//! seed 7         13           661   1.97%
//! seed 1234       2           898   0.22%
//! ```
//!
//! Spec §5.1 preregistered "6-40 mines per world" as the range to proceed on.
//! **One of three seeds is inside it and two are below, and the reason is
//! measurable rather than mysterious.**
//! That range was written for the *reclassification* design, whose denominator
//! was every occupation in the world (~900-1400). Spec §B.2 killed that design
//! and §B.3's replacement has a different denominator entirely: the daughter
//! throws `Bake::grow` makes, which are 489 / 420 / 26 across the panel — and
//! of those, 1 / 9 / 3 have any vacant candidate carrying workable ore
//! (`ore_siting_probe.rs`). No rate over that population can reach 6 on seed
//! 42. Task 1's finding — settlements are sited where ore is not — reaches
//! one hop out to their neighbours, which is the candidate set §B.3 confines
//! the working objective to.
//!
//! **The obvious way to raise the count is the one this campaign must not
//! take.** Dropping [`ORE_CUT`] to the field's barren floor yields 42 / 19 / 6
//! mines — inside the preregistered range — and 32 of seed 42's 42 then stand
//! on ground of prospectivity 0.0600, which is to say on no ore at all. That
//! is measured, not argued: it is what the mutation proving
//! `every_mine_stands_on_ore_bearing_ground` non-vacuous actually printed.
//! (That mutation was taken against the first cut of this task, which drew off
//! the bake's sequential stream; the counts it printed are of that world, and
//! the point it makes about WHERE the extra mines stand is unaffected.)
//! Widening the candidate set (a ring scan rather than the parent's direct
//! neighbours) is the change that would move the number honestly, and it is a
//! change to spec §B.3 rather than to a constant.

use hornvale_astronomy::SkyPins;
use hornvale_history::record::Function;
use hornvale_kernel::Seed;
use hornvale_terrain::TerrainPins;
use hornvale_worldgen::history_bake::ORE_CUT;
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

//! A DELVING HAS A COMMITTED DEPTH — The Winze, Task 3 (spec §4.2).
//!
//! Spec §3.3 measured the gap these gates close: `OccupationRecord` carried no
//! depth at all, so *"how deep did this delving get"* was unsaved and
//! unaskable from the ledger. `Occupation::delve_depth_m` is the answer, and
//! `occ-delve-depth` is the fact that carries it across the emit boundary.
//!
//! # WHY THE FIELD IS COMMITTED WHEN THE SEAT IS DELIBERATELY NOT
//!
//! `windows/worldgen`'s `Community::rung` carries a standing argument against
//! committing a delve coordinate: a *seat* is a pure function of
//! `(people, vertex)` through the seating, so committing it would add a
//! save-format surface for a value the seed re-derives exactly. That argument
//! is correct and it does not reach this field, because the two halves of
//! "how deep did they get" differ in kind. The seat is re-derivable and is
//! not committed. The **working** — how far below the seat they dug — is the
//! integral of a *live* quantity over a tenure: it accrues once per epoch out
//! of the community's population and tech horizon as they stood in that
//! epoch, and the ledger keeps neither trajectory. `occ-peak` is the maximum
//! population ever reached, `occ-tech` the final horizon; the path that
//! produced the depth is gone the moment the occupation closes.
//!
//! # THE PANEL, NOT A SEED
//!
//! Task 2 measured 1 / 13 / 2 mines across seeds 42 / 7 / 1234 — spec §5.1's
//! "viable but thin" row, under whose condition single-seed claims are banned
//! for the rest of the campaign. Every gate here pools.

use hornvale_astronomy::SkyPins;
use hornvale_history::record::{Function, OccupationRecord};
use hornvale_kernel::{Seed, World};
use hornvale_terrain::TerrainPins;
use hornvale_worldgen::{
    BuildDepth, SettlementPins, SkyChoice, WorldComponents, build_world_to, occupation_records,
};

/// The panel the campaign preregisters on (spec §5.1).
const SEEDS: [u64; 3] = [42, 7, 1234];

/// One panel seed's world, built to the depth that runs the history bake.
fn panel_world(seed_value: u64) -> World {
    build_world_to(
        Seed(seed_value),
        &SkyPins::default(),
        SkyChoice::Generated,
        &TerrainPins::default(),
        &SettlementPins::default(),
        &WorldComponents::assemble().expect("canonical components assemble"),
        BuildDepth::Settlements,
    )
    .expect("panel seed builds")
}

/// Every occupation of one panel world, read back **off the ledger** rather
/// than out of the bake — which is the whole point of the field being
/// committed, and what makes these gates test the emit/decode crossing and not
/// just the accrual.
fn panel_occupations(seed_value: u64) -> Vec<OccupationRecord> {
    occupation_records(&panel_world(seed_value))
}

/// A mine reaches a non-zero depth, pooled over the panel.
///
/// claim: reachability(seeds: 42/7/1234 — at least one `Function::Mine`
/// occupation carries `delve_depth_m > 0` after a round trip through the
/// ledger, pooled; not a per-world rate)
#[test]
fn a_mine_has_a_nonzero_delve_depth() {
    let mut mines = 0usize;
    let mut deepened = 0usize;
    let mut deepest = 0.0f64;
    for seed_value in SEEDS {
        let occs = panel_occupations(seed_value);
        let depths: Vec<f64> = occs
            .iter()
            .filter(|r| r.core.function == Function::Mine)
            .map(|r| r.core.delve_depth_m)
            .collect();
        let seed_deepest = depths.iter().copied().fold(0.0f64, f64::max);
        println!(
            "seed {seed_value}: {} mines, {} with a working, deepest {seed_deepest:.1} m",
            depths.len(),
            depths.iter().filter(|d| **d > 0.0).count(),
        );
        // The per-mine roster, printed rather than asserted. Task 5's
        // survivorship comparison is a distribution question and this is the
        // distribution as it stands BEFORE the hazard exists — the honest
        // baseline for it, and the readout that says whether the rate constant
        // is putting every working at the same depth.
        let mut roster: Vec<(f64, Option<f64>, u32)> = occs
            .iter()
            .filter(|r| r.core.function == Function::Mine)
            .map(|r| {
                (
                    r.core.delve_depth_m,
                    r.core.ended.map(|e| e - r.core.founded),
                    r.core.peak_population,
                )
            })
            .collect();
        roster.sort_by(|a, b| a.0.total_cmp(&b.0));
        for (depth, tenure, peak) in &roster {
            let tenure = tenure.map_or("still open".to_string(), |t| format!("{t:.0} y"));
            println!("    {depth:9.1} m  tenure {tenure:>10}  peak {peak:4}");
        }
        mines += depths.len();
        deepened += depths.iter().filter(|d| **d > 0.0).count();
        deepest = deepest.max(seed_deepest);
    }
    // The precondition: without mines this gate would pass vacuously on
    // "0 of 0 delved". Task 2's own gates hold the population; this states the
    // dependency so a regression there reds here with the right message.
    assert!(
        mines > 0,
        "no Function::Mine occupation exists on {SEEDS:?}, so this gate cannot \
         say anything about delve depth — see `mines_exist.rs`."
    );
    assert!(
        deepened > 0,
        "{mines} mines pooled over {SEEDS:?} and NONE carries a working \
         (`delve_depth_m > 0`). Spec §4.2's depth either never accrues in the \
         bake or never survives the emit boundary."
    );
    // A depth is metres of rock, not a token. The magnitude is not
    // preregistered — this only refuses a working that is a rounding error,
    // which is what a mis-scaled rate constant would look like while still
    // passing the `> 0` test above.
    assert!(
        deepest >= 1.0,
        "the deepest working on the whole panel is {deepest} m — non-zero but \
         under a single metre, which is a scale error rather than a delving."
    );
}

/// The complement, and the reason the gate above is not "every occupation has
/// a number in a new field": an occupation that is not a working never
/// delves, and there are 2,700-odd of them.
///
/// **Renamed from the plan's `a_surface_occupation_does_not_delve` (Ruling
/// 1).** That name claims something this test cannot establish: nothing on
/// the ledger says which rung an occupation is seated at — the seat is
/// deliberately not committed (see this file's header) — so "surface" is not a
/// property a ledger read can filter on. What *is* checkable, and is what the
/// sketch was after, is that the depth belongs to workings alone.
///
/// claim: invariant(forall-seed over the panel — every non-`Mine` occupation
/// has `delve_depth_m == 0.0`, with the population asserted so the quantifier
/// is not empty)
#[test]
fn an_occupation_that_is_not_a_working_never_delves() {
    for seed_value in SEEDS {
        let occs = panel_occupations(seed_value);
        let others: Vec<&OccupationRecord> = occs
            .iter()
            .filter(|r| r.core.function != Function::Mine)
            .collect();
        assert!(
            !others.is_empty(),
            "seed {seed_value} has no non-mine occupation at all, so this gate \
             quantifies over nothing."
        );
        let delving: Vec<(u32, f64)> = others
            .iter()
            .filter(|r| r.core.delve_depth_m != 0.0)
            .map(|r| (r.core.site.0, r.core.delve_depth_m))
            .collect();
        // Truncated on purpose: the mutation that proves this gate can fail
        // puts a depth on ~1,100 of seed 42's occupations, and a failure
        // message carrying every one of them is unreadable in a gate log.
        assert!(
            delving.is_empty(),
            "seed {seed_value}: {} of {} non-mine occupations carry a working \
             depth (first ten as (site, metres)): {:?}. Only a \
             `Function::Mine` drives one.",
            delving.len(),
            others.len(),
            &delving[..delving.len().min(10)],
        );
    }
}

/// The field is committed, so it must survive the world file — not merely the
/// in-process decode `occupation_records` performs.
///
/// Serializes each panel world exactly as `hornvale new --out` does,
/// deserializes it, and compares the depth of every occupation position by
/// position. A field that reached the `Occupation` struct but never reached a
/// `Fact` would pass every gate above and fail here.
///
/// claim: invariant(forall-seed over the panel — `delve_depth_m` is identical
/// before and after a JSON save/load, over every occupation, with the count of
/// non-zero depths asserted so the comparison is not `0.0 == 0.0` throughout)
#[test]
fn delve_depth_survives_a_save_load_round_trip() {
    let mut nonzero_pooled = 0usize;
    for seed_value in SEEDS {
        let world = panel_world(seed_value);
        let before: Vec<f64> = occupation_records(&world)
            .iter()
            .map(|r| r.core.delve_depth_m)
            .collect();
        let json = serde_json::to_string(&world).expect("a world serializes");
        let reloaded: World = serde_json::from_str(&json).expect("a world deserializes");
        let after: Vec<f64> = occupation_records(&reloaded)
            .iter()
            .map(|r| r.core.delve_depth_m)
            .collect();
        assert_eq!(
            before, after,
            "seed {seed_value}: the delve depths differ across a save/load round trip"
        );
        nonzero_pooled += before.iter().filter(|d| **d > 0.0).count();
    }
    // Without this the comparison above would hold trivially on a world where
    // every depth is zero — which is exactly the state this task started from.
    assert!(
        nonzero_pooled > 0,
        "every occupation on {SEEDS:?} round-tripped a depth of 0.0, so the \
         round trip proved nothing about the new fact."
    );
}

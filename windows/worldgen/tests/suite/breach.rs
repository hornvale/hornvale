//! A DELVING THAT BREAKS THROUGH STOPS — The Winze, Task 4 (spec §4.3).
//!
//! The campaign's mechanism. Each increment of a working's advance carries a
//! small breach probability; a delving that breaches closes as
//! `CauseOfEnd::Breached` by `Ended::Nature`, and its committed
//! `delve_depth_m` includes the metre that broke through.
//!
//! # NOTHING SELECTS ON DEPTH
//!
//! No gate here compares a depth against a threshold, and neither does the
//! code they cover. The hazard's only input is the metres a working cut *this
//! epoch* (`Bake::deepen`'s return), so two workings at the same depth face
//! different hazards if one is cutting harder, and two workings cutting the
//! same metres face the same hazard however deep they already stand. The
//! survivorship shape spec §5.2 preregisters is an output of that, and
//! measuring it is Task 5's job, not this file's — these gates only establish
//! that the mechanism exists, fires, and fires on the right population.
//!
//! # NOTHING IS NAMED
//!
//! Spec §4.6. `a_breach_names_nothing_that_came_through` is the assertion
//! that carries it: every breach ends by `Ended::Nature`, so no entity is the
//! agent of one, and there is nothing in the record to read an antagonist off.
//!
//! # THE PANEL, NOT A SEED
//!
//! Single-seed claims are banned for this campaign (spec §5.1's "viable but
//! thin" row). Every gate pools.

use hornvale_astronomy::SkyPins;
use hornvale_history::record::{CauseOfEnd, Ended, Function, OccupationRecord};
use hornvale_kernel::{Seed, World};
use hornvale_terrain::TerrainPins;
use hornvale_worldgen::{
    BuildDepth, SettlementPins, SkyChoice, WorldComponents, build_world_to, occupation_records,
};

/// The panel the campaign preregisters on (spec §5.1), extended by
/// **consecutive** seeds under spec amendment E.4.2's rule.
///
/// E.4.2 froze that rule before this code existed: fewer than 60 pooled
/// workings across `[42, 7, 1234]` extends the panel by seeds `0, 1, 2, …`
/// until the count is reached or the panel hits twelve. Task 2b measured 39,
/// so the rule has already fired. This file borrows the *panel*, which is a
/// sample-size decision E.4.2 owns; it does not borrow a target, and the
/// hazard rate itself was chosen without reference to any of it
/// (`BREACH_FREE_PATH_M`'s own doc, and E.4.2's closing paragraph).
const SEEDS: [u64; 12] = [42, 7, 1234, 0, 1, 2, 3, 4, 5, 6, 8, 9];

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

/// Every occupation of every panel world, read back **off the ledger** — so
/// these gates cross the emit boundary, and a `Breached` that encoded but did
/// not decode (`history_emit::parse_cause` is a `&str` match the compiler does
/// not enumerate) fails here rather than passing silently.
fn panel_occupations() -> Vec<(u64, Vec<OccupationRecord>)> {
    SEEDS
        .iter()
        .map(|&s| (s, occupation_records(&panel_world(s))))
        .collect()
}

/// A working breaks through somewhere on the panel, and the breached delvings
/// are workings that had actually dug.
///
/// claim: reachability(seeds: the E.4.2 panel — at least one occupation reads
/// back off the ledger with `cause == Some(CauseOfEnd::Breached)`, pooled; not
/// a per-world rate, and NOT a preregistered count — the rate was fixed from
/// `BREACH_FREE_PATH_M`'s read-off before this ran)
#[test]
fn a_working_can_break_through() {
    let panel = panel_occupations();
    let mut mines = 0usize;
    let mut breached = 0usize;
    let mut deepest_breach = 0.0f64;
    for (seed_value, occs) in &panel {
        let seed_mines = occs
            .iter()
            .filter(|r| r.core.function == Function::Mine)
            .count();
        let seed_breached: Vec<&OccupationRecord> = occs
            .iter()
            .filter(|r| r.core.cause == Some(CauseOfEnd::Breached))
            .collect();
        println!(
            "seed {seed_value}: {seed_mines} workings, {} breached",
            seed_breached.len()
        );
        for r in &seed_breached {
            println!(
                "    breached at {:9.1} m  founded {:.0}  ended {:.0}",
                r.core.delve_depth_m,
                r.core.founded,
                r.core.ended.unwrap_or(f64::NAN),
            );
            deepest_breach = deepest_breach.max(r.core.delve_depth_m);
        }
        mines += seed_mines;
        breached += seed_breached.len();
    }
    // The precondition. Without workings this gate would pass vacuously on
    // "0 of 0 broke through", which is the shape a regression in Task 2's
    // founding would take.
    assert!(
        mines > 0,
        "no Function::Mine occupation exists on the panel, so this gate cannot \
         say anything about breaching — see `mines_exist.rs`."
    );
    assert!(
        breached > 0,
        "{mines} workings pooled over {SEEDS:?} and NONE broke through. The \
         hazard in `Bake::maybe_breach` either never fires or never reaches \
         the ledger (`history_emit`'s `cause_label`/`parse_cause` pair is the \
         other place this can be lost)."
    );
    // A breach is a working that had cut rock, so its committed depth is
    // positive by construction — the fatal increment is accrued by
    // `Bake::deepen` before the hazard is asked. A zero here would mean the
    // close is firing somewhere the depth is not.
    assert!(
        deepest_breach > 0.0,
        "{breached} delvings broke through and the deepest carries \
         delve_depth_m = 0.0, so the close is not happening where the digging is."
    );
}

/// **A rule that fires on everything and a rule that works are identical in a
/// count**, and this is the gate that tells them apart.
///
/// Every other gate in this file passes unchanged if the hazard is forced to
/// certainty — `p > 1`, so every working breaks through on its first
/// increment. That mutation was run before this test existed and all four
/// gates stayed green, which is the reason it exists: the plan's Step 7 asks
/// for both directions, and "at least one breach" is only half of a
/// mechanism.
///
/// **No rate is asserted, deliberately.** Spec amendment E.4.2 fixes the
/// hazard rate as a modelling claim taken without reference to any count, so
/// pinning a breached *fraction* here would smuggle a tuned constant in
/// through a test — and would red on any seed panel change for reasons that
/// are not about the mechanism. What is asserted is the qualitative content
/// of §4.3's word "probability": some workings break through and some do not.
///
/// claim: invariant(seeds: the E.4.2 panel — pooled, the breached count is
/// strictly less than the working count, at least one working ended by some
/// other cause, and at least one is still open)
#[test]
fn breaching_is_a_hazard_not_a_certainty() {
    let panel = panel_occupations();
    let mut workings = 0usize;
    let mut breached = 0usize;
    let mut ended_otherwise = 0usize;
    let mut still_open = 0usize;
    for (_, occs) in &panel {
        for r in occs.iter().filter(|r| r.core.function == Function::Mine) {
            workings += 1;
            match r.core.cause {
                Some(CauseOfEnd::Breached) => breached += 1,
                Some(_) => ended_otherwise += 1,
                None => still_open += 1,
            }
        }
    }
    println!(
        "pooled over {SEEDS:?}: {workings} workings — {breached} breached, \
         {ended_otherwise} ended otherwise, {still_open} still open"
    );
    assert!(
        workings > 0,
        "no Function::Mine occupation exists on the panel, so this gate \
         quantifies over nothing."
    );
    assert!(
        breached < workings,
        "all {workings} workings on {SEEDS:?} broke through. The breach is a \
         hazard per increment of advance (spec §4.3), not the way a delving \
         ends — a probability that has saturated to 1 produces exactly this."
    );
    assert!(
        ended_otherwise > 0,
        "every one of the {workings} workings that ENDED on {SEEDS:?} ended by \
         breaching. A working is stopped by the world — famine, a raid, a \
         climate eviction — as well as by what it digs into; if breaching is \
         the only end a working can have, the hazard is saturated."
    );
    assert!(
        still_open > 0,
        "not one of the {workings} workings on {SEEDS:?} survived to the end \
         of the bake. A delving that always ends before the record closes is a \
         certainty wearing a probability's clothes."
    );
}

/// The complement, and the reason the gate above is not "some occupation
/// somewhere carries a new enum variant": only a working can break through,
/// because only a working cuts rock.
///
/// claim: invariant(forall over the E.4.2 panel — every occupation whose cause
/// is `Breached` has `function == Function::Mine` and a positive
/// `delve_depth_m`, with the breached population asserted non-empty so the
/// quantifier is not vacuous)
#[test]
fn nothing_but_a_working_ever_breaches() {
    let panel = panel_occupations();
    let mut breached = 0usize;
    for (seed_value, occs) in &panel {
        let offenders: Vec<(u32, Function, f64)> = occs
            .iter()
            .filter(|r| r.core.cause == Some(CauseOfEnd::Breached))
            .filter(|r| r.core.function != Function::Mine || r.core.delve_depth_m <= 0.0)
            .map(|r| (r.core.site.0, r.core.function, r.core.delve_depth_m))
            .collect();
        assert!(
            offenders.is_empty(),
            "seed {seed_value}: {} occupations ended `Breached` without being a \
             working that had dug (as (site, function, metres)): {:?}",
            offenders.len(),
            &offenders[..offenders.len().min(10)],
        );
        breached += occs
            .iter()
            .filter(|r| r.core.cause == Some(CauseOfEnd::Breached))
            .count();
    }
    assert!(
        breached > 0,
        "no occupation on {SEEDS:?} ended `Breached`, so this gate quantifies \
         over nothing."
    );
}

/// **Nothing is named** (spec §4.6). A breach records that a delving broke
/// through and stops there: it ends by `Ended::Nature`, so there is no agent
/// entity on the record and nothing downstream can read one off it.
///
/// This is the gate that would fail if the design drifted toward
/// `Ended::By(..)` — an antagonist the campaign refuses to author — which is
/// the specific drift the plan's Step 5 says to stop and report on.
///
/// claim: invariant(forall over the E.4.2 panel — every `Breached` occupation
/// carries `Ended::Nature`, with the population asserted non-empty)
#[test]
fn a_breach_names_nothing_that_came_through() {
    let panel = panel_occupations();
    let mut breached = 0usize;
    for (seed_value, occs) in &panel {
        let named: Vec<u32> = occs
            .iter()
            .filter(|r| r.core.cause == Some(CauseOfEnd::Breached))
            .filter(|r| !matches!(r.ended_by, Ended::Nature))
            .map(|r| r.core.site.0)
            .collect();
        assert!(
            named.is_empty(),
            "seed {seed_value}: {} breached delvings name an agent through \
             `Ended::By(..)` (sites {:?}). Spec §4.6: nothing knows what came \
             through, so there is nothing to name.",
            named.len(),
            &named[..named.len().min(10)],
        );
        breached += occs
            .iter()
            .filter(|r| r.core.cause == Some(CauseOfEnd::Breached))
            .count();
    }
    assert!(
        breached > 0,
        "no occupation on {SEEDS:?} ended `Breached`, so this gate quantifies \
         over nothing."
    );
}

/// The cause is committed, so `Breached` must survive the world file.
///
/// The real risk this pins is not serde: it is `history_emit`'s
/// `cause_label`/`parse_cause` pair. The encoder is a `match` the compiler
/// enumerates; the decoder is a `&str` match with a `_ => return None` arm
/// that it does not. A variant added to one and forgotten in the other encodes
/// fine and reads back as **`None` — "never ended"** — on every occupation
/// that ended that way, which is a silent loss of both the cause and the
/// ending year.
///
/// claim: invariant(forall over the E.4.2 panel — the full `(cause, ended)`
/// sequence is identical before and after a JSON save/load, with the count of
/// `Breached` records asserted so the comparison is not vacuous)
#[test]
fn a_breach_survives_a_save_load_round_trip() {
    let mut breached_pooled = 0usize;
    for seed_value in SEEDS {
        let world = panel_world(seed_value);
        let before: Vec<(Option<CauseOfEnd>, Option<f64>)> = occupation_records(&world)
            .iter()
            .map(|r| (r.core.cause, r.core.ended))
            .collect();
        let json = serde_json::to_string(&world).expect("a world serializes");
        let reloaded: World = serde_json::from_str(&json).expect("a world deserializes");
        let after: Vec<(Option<CauseOfEnd>, Option<f64>)> = occupation_records(&reloaded)
            .iter()
            .map(|r| (r.core.cause, r.core.ended))
            .collect();
        assert_eq!(
            before, after,
            "seed {seed_value}: the ending causes differ across a save/load round trip"
        );
        breached_pooled += before
            .iter()
            .filter(|(c, _)| *c == Some(CauseOfEnd::Breached))
            .count();
    }
    assert!(
        breached_pooled > 0,
        "no `Breached` record exists anywhere on {SEEDS:?}, so this round trip \
         proved nothing about the new variant."
    );
}

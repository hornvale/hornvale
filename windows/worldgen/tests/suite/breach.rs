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
use hornvale_kernel::{Seed, Vertex, World};
use hornvale_terrain::TerrainPins;
use hornvale_worldgen::{
    BuildDepth, SealState, SettlementPins, Valence, VestigeKind, WorldComponents, build_world_to,
    build_world_to_with_artifacts, occupation_records, present_year, vestige_dread,
    vestige_from_occupation, vestige_lines_from,
};
use std::collections::BTreeSet;

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

// -------------------------------------------------------------------------
// WHAT A LATER CULTURE CAN KNOW — Task 6 (spec §4.5).
//
// The three states are derived in `windows/worldgen/src/vestige.rs` and
// asserted there against constructed records, which is where the arithmetic
// belongs. This is the half that module cannot do: showing that a REAL world
// reaches each state, off a real ledger, at ages the hazard actually produces.
// -------------------------------------------------------------------------

/// §4.5's three states, each reached by a breached delving on the panel.
///
/// ```text
/// RECENT   the warning is legible                       max legibility on the panel
/// DECAYED  legibility -> 0, dread -> high               min legibility on the panel
/// WARDED   a kept seal reads SAFE, and may be wrong     a living layer over a breach
/// ```
///
/// # THE TWO `Breached`ES ARE DIFFERENT THINGS, AND THIS GATE SEPARATES THEM
///
/// `CauseOfEnd::Breached` is why a delving stopped; `SealState::Breached` is
/// the ward having failed, derived from `ended` alone. The first never implies
/// the second's absence and neither implies the other's presence — so this
/// asserts, over every breach on the panel, that a breached delving's OWN
/// layer is never `SealState::Maintained`. It cannot be: `Maintained` means
/// `ended.is_none()`, and a breach is an ending. §4.5's WARDED state is
/// therefore never a property of the breach's own layer, and the gate below
/// finds where it actually lives.
///
/// # WARDED IS PER-LAYER, AND THE FIELD DOES NOT HIDE A REMEMBERED BREACH
///
/// Measured on the panel: three vertices carry a LIVING occupation standing
/// over a delving that broke through, and all three living layers read
/// `Maintained` / `Venerated` / `dread 0.1` / `legibility 1.0` — byte-identical
/// to a living layer at a site nothing ever happened at. That is §4.5's
/// property, and it must not be repaired.
///
/// What is asserted alongside it, so the limit is on the record rather than
/// assumed either way: `vestige_dread` takes the MAX over a vertex's
/// palimpsest, so at those same three vertices the FIELD still reads the old
/// layer's dread (0.936..0.998 measured), not the living layer's 0.1. The
/// model is source-blind at the layer and is not amnesiac at the vertex. A
/// future change of `vestige_dread`'s aggregation to a most-recent-layer read
/// would delete the DECAYED state wholesale, and this assertion is what would
/// object.
///
/// claim: invariant(forall over the E.4.2 panel — no breached delving's own
/// layer reads `SealState::Maintained`, and every living layer standing over
/// one reads the kept-seal tuple exactly; with the RECENT, DECAYED and WARDED
/// populations each asserted non-empty so no quantifier is vacuous)
#[test]
fn a_later_culture_reads_all_three_states_of_a_breach() {
    let mut breached_layers = 0usize;
    let mut most_legible = f64::NEG_INFINITY;
    let mut least_legible = f64::INFINITY;
    let mut dread_at_least_legible = f64::NAN;
    let mut living_over_a_breach = 0usize;

    for seed_value in SEEDS {
        let world = panel_world(seed_value);
        let occs = occupation_records(&world);
        let now = present_year(&world);

        let breach_sites: BTreeSet<Vertex> = occs
            .iter()
            .filter(|r| r.core.cause == Some(CauseOfEnd::Breached))
            .map(|r| r.core.site)
            .collect();

        for record in occs
            .iter()
            .filter(|r| r.core.cause == Some(CauseOfEnd::Breached))
        {
            let v = vestige_from_occupation(record, now);
            assert_ne!(
                v.seal_state,
                SealState::Maintained,
                "seed {seed_value}: a delving that ended by breaching read as a KEPT \
                 seal — `SealState::Breached` and `CauseOfEnd::Breached` have been \
                 conflated somewhere"
            );
            assert_eq!(
                v.kind,
                VestigeKind::AbandonedDelving,
                "seed {seed_value}: only a working breaches, so every breach layer \
                 is a delving"
            );
            breached_layers += 1;
            if v.warning_legibility > most_legible {
                most_legible = v.warning_legibility;
            }
            if v.warning_legibility < least_legible {
                least_legible = v.warning_legibility;
                dread_at_least_legible = v.dread;
            }
        }

        if breach_sites.is_empty() {
            continue;
        }
        let field = vestige_dread(&world).expect("a panel world derives its own dread field");
        for record in occs
            .iter()
            .filter(|r| r.core.ended.is_none() && breach_sites.contains(&r.core.site))
        {
            let v = vestige_from_occupation(record, now);
            assert_eq!(
                (v.seal_state, v.valence),
                (SealState::Maintained, Valence::Venerated),
                "seed {seed_value}: a living occupation over a breach reads as a kept seal"
            );
            assert_eq!(
                (v.dread, v.warning_legibility),
                (0.1, 1.0),
                "seed {seed_value}: the living layer carries no trace of what is under \
                 it — §4.5's WARDED state, and NOT a defect to repair"
            );
            assert!(
                *field.get(record.core.site) > 0.9,
                "seed {seed_value}: `vestige_dread` is a MAX over the palimpsest, so a \
                 remembered breach still reads at the vertex even under a living \
                 community; got {}",
                field.get(record.core.site)
            );
            living_over_a_breach += 1;
        }
    }

    assert!(
        breached_layers > 0,
        "no delving broke through anywhere on {SEEDS:?}, so every quantifier above \
         ranged over nothing."
    );
    assert!(
        most_legible > 0.5,
        "§4.5 RECENT is unreached: the most legible breach on {SEEDS:?} reads \
         {most_legible}, so no later people on the panel can read what happened \
         anywhere."
    );
    assert!(
        least_legible < 0.05 && dread_at_least_legible > 0.95,
        "§4.5 DECAYED is unreached: the least legible breach on {SEEDS:?} reads \
         legibility {least_legible} at dread {dread_at_least_legible}, so nowhere on \
         the panel do they know something is wrong and not what."
    );
    assert!(
        living_over_a_breach > 0,
        "§4.5 WARDED is unreached: nobody on {SEEDS:?} lives over a delving that \
         broke through, so the kept-seal tuple above was never checked against a \
         real world."
    );
}

/// The almanac names a breached delving where a world has one, and says
/// **nothing** where a world has none.
///
/// # A LINE THAT ALWAYS APPEARS IS A TEMPLATE, NOT NARRATION
///
/// So both directions are gates, and both populations are asserted non-empty:
/// the panel has to contain a world that renders the line and a world that
/// does not, or one of the two quantifiers ranged over nothing.
///
/// # THE EXPECTED COUNT IS DERIVED FROM THE RECORDS, NOT FROM THE FIELD
///
/// `vestige_lines_from` counts vestige LAYERS (`AbandonedDelving` +
/// `HazardKind::Numinous`). Re-deriving the expectation the same way would
/// only assert that the code agrees with itself, so this reads the committed
/// occupations instead — a breach is `CauseOfEnd::Breached` at a land site —
/// and the two paths meet only if the cause→hazard→kind chain is intact.
///
/// # THE SENTENCE IS FROZEN, BECAUSE §4.6 IS A CLAIM ABOUT ITS WORDS
///
/// A breach records that a delving ended by breaking through and nothing
/// about what came through, because nothing in the model knows. That is not
/// a property of a count, it is a property of a sentence, so the sentence is
/// pinned verbatim and its disclaiming clause is asserted separately by name.
/// One evocative noun added here is the whole campaign's constraint broken,
/// and this is what objects.
///
/// claim: invariant(forall over the E.4.2 panel — `vestige_lines_from` emits
/// the breach line exactly when the world carries a breached delving on land,
/// carrying that world's own count and no other word; with both the
/// has-a-breach and the has-none populations asserted non-empty)
#[test]
fn the_almanac_names_a_breach_only_where_one_happened() {
    const TAIL: &str = "of those delvings ended where they broke through — the digging \
                        stopped there, and no account of what was found survives.";
    let (mut worlds_with, mut worlds_without) = (0usize, 0usize);

    for seed_value in SEEDS {
        // The terrain the BUILD already sculpted, not a second sculpt through
        // `terrain_of` — same bytes, half the work, and it keeps decision
        // 0092's derivation entry point out of a test that does not need it.
        let built = build_world_to_with_artifacts(
            Seed(seed_value),
            &SkyPins::default(),
            &TerrainPins::default(),
            &SettlementPins::default(),
            &WorldComponents::assemble().expect("canonical components assemble"),
            BuildDepth::Settlements,
        )
        .expect("panel seed builds");
        let world = built.world;
        let terrain = built
            .terrain
            .expect("a Settlements-depth build carries its sculpted terrain");
        let lines =
            vestige_lines_from(&world, &terrain).expect("a panel world renders its residue");

        // Independent of the render's own counting path: the committed
        // records, land-filtered the way the almanac's land-only sections are.
        let expected = occupation_records(&world)
            .iter()
            .filter(|r| r.core.cause == Some(CauseOfEnd::Breached))
            .filter(|r| !terrain.is_ocean(r.core.site))
            .count();

        let matched: Vec<&String> = lines.iter().filter(|l| l.contains(TAIL)).collect();
        println!(
            "seed {seed_value}: {expected} breached on land, {} line(s)",
            matched.len()
        );

        if expected == 0 {
            assert!(
                matched.is_empty(),
                "seed {seed_value} has no breached delving on land, so the almanac must \
                 say nothing about one; it said {matched:?}"
            );
            worlds_without += 1;
            continue;
        }

        assert_eq!(
            matched.len(),
            1,
            "seed {seed_value}: exactly one breach line, got {matched:?}"
        );
        assert_eq!(
            matched[0],
            &format!("{expected} {TAIL}"),
            "seed {seed_value}: the breach line must carry this world's own count and \
             the frozen sentence"
        );
        assert!(
            matched[0].contains("no account of what was found survives"),
            "seed {seed_value}: spec §4.6 — the line must disclaim any account of what \
             came through, because nothing in the model holds one; got {:?}",
            matched[0]
        );
        worlds_with += 1;
    }

    assert!(
        worlds_with > 0,
        "no world on {SEEDS:?} rendered a breach line, so the positive direction of \
         this gate ranged over nothing."
    );
    assert!(
        worlds_without > 0,
        "every world on {SEEDS:?} rendered a breach line, so the gate cannot tell \
         narration from a template — the negative direction ranged over nothing."
    );
}

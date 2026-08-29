//! Substrate probe 4 (The Retelling): what does `stance` actually COST, and
//! how are stance-pairs distributed?
//!
//! Two questions in one pass, both of which must be answered before the
//! predicate is locked:
//!
//! 1. **Cost.** `stance(who, claim)` needs "is the claim's subject an ancestor
//!    of `who`", and `Lineage::ancestry` walks to the root every call. Counted
//!    in NODE VISITS rather than seconds: `Instant` is banned by the
//!    wall-clock rule, and a step count is deterministic and machine-free,
//!    which a duration is not.
//! 2. **Distribution.** A predicate that is elegant and fires on 2% of edges is
//!    worse than an awkward one that fires on 30%. The party-key quadrants
//!    measured earlier do NOT transfer — stance is a different partition.

use hornvale_hearsay::lineage::{Lineage, lineage_of};
use hornvale_kernel::ledger::{EntityId, Value};
use std::collections::{BTreeMap, BTreeSet};

/// Where a community stands relative to one ending.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
enum Stance {
    Perpetrator,
    VictimLine,
    Bystander,
}

/// The naive read: walk `who`'s ancestry looking for the subject, every time.
/// Returns the stance and the number of nodes visited.
fn stance_naive(
    lin: &Lineage,
    perpetrator: Option<EntityId>,
    subject: EntityId,
    who: EntityId,
) -> (Stance, usize) {
    if perpetrator == Some(who) {
        return (Stance::Perpetrator, 0);
    }
    let anc = lin.ancestry(who);
    let visits = anc.len();
    if who == subject || anc.contains(&subject) {
        (Stance::VictimLine, visits)
    } else {
        (Stance::Bystander, visits)
    }
}

/// The memoised read: the victim-line set is computed ONCE per claim, then
/// every holder is a set membership. Returns the stance; the caller counts the
/// one-off build cost separately.
fn stance_memo(
    victim_line: &BTreeSet<EntityId>,
    perpetrator: Option<EntityId>,
    who: EntityId,
) -> Stance {
    if perpetrator == Some(who) {
        Stance::Perpetrator
    } else if victim_line.contains(&who) {
        Stance::VictimLine
    } else {
        Stance::Bystander
    }
}

/// claim: structural(seed: 42) — false-positive seed-loop flag; the loops bind
/// occupation ids, not seeds.
#[test]
#[ignore = "probe: what stance costs and how stance pairs are distributed on seed 42; run by hand (The Retelling answered its question; demoted by The Governor 2026-08-28)"]
fn what_does_stance_cost_and_how_are_stance_pairs_distributed_on_seed_42() {
    let world = hornvale_worldgen::build_world(
        hornvale_kernel::Seed(42),
        &hornvale_astronomy::SkyPins::default(),
        hornvale_worldgen::SkyChoice::Generated,
        &hornvale_terrain::TerrainPins::default(),
        &hornvale_worldgen::SettlementPins::default(),
    )
    .expect("seed 42 builds");
    let led = &world.ledger;
    let lin = lineage_of(led);
    let all = lin.all();

    // Every ending, with its attacker if one is named.
    let mut endings: Vec<(EntityId, Option<EntityId>)> = Vec::new();
    for s in &all {
        if led.value_of(*s, hornvale_history::OCC_ENDED).is_none() {
            continue;
        }
        let perp = match led.value_of(*s, hornvale_history::OCC_ENDED_BY) {
            Some(Value::Entity(a)) => Some(*a),
            _ => None,
        };
        endings.push((*s, perp));
    }

    // --- 1. NAIVE: ancestry walked per (holder, event) pair ---------------
    let mut naive_visits = 0usize;
    let mut naive_pairs = 0usize;
    for (subject, perp) in &endings {
        for who in &all {
            let (_, v) = stance_naive(&lin, *perp, *subject, *who);
            naive_visits += v;
            naive_pairs += 1;
        }
    }

    // --- 2. MEMOISED: victim-line built once per event -------------------
    // Build cost counted honestly: descendants_of itself walks ancestry.
    let mut memo_build_visits = 0usize;
    let mut victim_lines: BTreeMap<EntityId, BTreeSet<EntityId>> = BTreeMap::new();
    for (subject, _) in &endings {
        let mut set: BTreeSet<EntityId> = lin.descendants_of(*subject).into_iter().collect();
        set.insert(*subject);
        // descendants_of calls ancestry once per candidate node.
        memo_build_visits += all.len();
        victim_lines.insert(*subject, set);
    }

    // --- 2b. TIER 3: ancestry memoised ONCE PER NODE -------------------
    let mut per_node_visits = 0usize;
    let mut ancestors: BTreeMap<EntityId, BTreeSet<EntityId>> = BTreeMap::new();
    for who in &all {
        let anc = lin.ancestry(*who);
        per_node_visits += anc.len();
        ancestors.insert(*who, anc.into_iter().collect());
    }

    // --- 3. The stance-pair distribution over inheritance edges ----------
    let mut pairs: BTreeMap<(Stance, Stance), usize> = BTreeMap::new();
    let mut edges = 0usize;
    let mut lossy = 0usize;
    for (subject, perp) in &endings {
        let vl = &victim_lines[subject];
        for child in &all {
            let Some(parent) = lin.parent(*child) else {
                continue;
            };
            // ONLY edges that actually carry this claim. An edge outside the
            // witness subtree transmits nothing, so counting it is the
            // wrong-population error: it buries the signal under Bystander
            // pairs that never occur in the model.
            let carries = vl.contains(child) || Some(*child) == *perp || vl.contains(&parent);
            if !carries {
                continue;
            }
            let st = stance_memo(vl, *perp, parent);
            let sh = stance_memo(vl, *perp, *child);
            *pairs.entry((st, sh)).or_default() += 1;
            edges += 1;
            if st != sh {
                lossy += 1;
            }
        }
    }

    println!("OCCUPATIONS {} | ENDINGS {}", all.len(), endings.len());
    println!(
        "NAIVE   ancestry node-visits = {naive_visits} over {naive_pairs} (holder, event) pairs \
         ({:.1} visits/pair)",
        naive_visits as f64 / naive_pairs as f64
    );
    println!(
        "MEMOISED build node-visits   = {memo_build_visits}; lookups are O(log n) set membership \
         -> speedup {:.2}x on the counted work",
        naive_visits as f64 / memo_build_visits.max(1) as f64
    );
    println!(
        "TIER3   per-node ancestry visits = {per_node_visits} -> speedup {:.1}x vs naive",
        naive_visits as f64 / per_node_visits.max(1) as f64
    );
    println!(
        "STANCE PAIRS over {edges} CARRYING (edge x event) pairs: lossy = {lossy} ({:.1}%)",
        100.0 * lossy as f64 / edges as f64
    );
    for ((t, h), n) in &pairs {
        println!("  teller {t:?} -> hearer {h:?} : {n}");
    }
}

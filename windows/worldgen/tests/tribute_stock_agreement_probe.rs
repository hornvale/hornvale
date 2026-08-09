//! THE TARE — why this probe survives instead of being scratch.
//!
//! `windows/lab/src/metrics.rs`'s `tribute-relations-standing` measures the
//! ledger's tribute STOCK (`PAYS_TRIBUTE_TO` facts standing at `now`) because
//! the bake's own tribute FLOW (`BakeCensus::tribute_collected`, integrated
//! inside `History::tally`) is discarded by `build_world_to` before any
//! census view exists — `BakeCensus` lives only on a live `History`, and no
//! census metric can ever reach it. That means **this file is the only place
//! the agreement between the census-visible stock and the bake's own flow can
//! be checked at all**: a metric extractor only ever sees the `World` a
//! census world already discarded `History` from, so nothing downstream of
//! the census can hold both quantities at once to compare them. The same
//! shape kept The Confusion's raid probe alive after its own census columns
//! shipped (`windows/worldgen/tests/raid_attribution_probe.rs`).
//!
//! **Measured agreement:** `stock` (this file's `Observed::stock`, identical
//! to what the shipped metric counts) correlates with `tribute_collected` at
//! spearman **+0.9344** over seeds `1..=36`, `BuildDepth::Settlements`. Three
//! rejected alternatives, scored the same way:
//!
//!   relation_years  SUM(now - since)              +0.8909
//!   patrons         distinct patrons               +0.8419
//!   top_share       largest patron's share         -0.7692  (substantially
//!             an arithmetic artifact: `top_share >= 1/stock` by
//!             construction, decaying toward -0.539 once worlds with
//!             `stock <= 60` are excluded)
//!
//! `oldest` (`MAX(now - since)`) is also printed, at +0.4571, but was never a
//! serious candidate — it answers "did any relation persist," not "how much
//! tribute moved."
//!
//! Candidate observables, all computable from a `World` alone:
//!   stock          relations standing at now                    <- chosen
//!   relation_years SUM(now - since)      — accumulates, as the flow does
//!   oldest         MAX(now - since)      — did any relation persist?
//!   patrons        distinct patrons      — the graph's out-degree support
//!   top_share      largest patron's share of relations — empire vs many holds
//!
//! Printed beside the bake's own `tribute_collected` and
//! `tribute_collection_events`, which only a live `History` can reach.

#![allow(clippy::disallowed_methods)]

use hornvale_astronomy::SkyPins;
use hornvale_kernel::{Seed, Value};
use hornvale_terrain::TerrainPins;
use hornvale_worldgen::{
    BuildDepth, SettlementPins, SkyChoice, WorldComponents, build_world_to, census, history_for,
};
use std::collections::BTreeMap;

/// Every ledger-visible tribute observable, read off a built world.
struct Observed {
    stock: u64,
    relation_years: f64,
    oldest: f64,
    patrons: u64,
    top_share: f64,
}

fn observe(world: &hornvale_kernel::World) -> Observed {
    let now = world
        .ledger
        .find(hornvale_history::HISTORY_NOW)
        .find_map(|f| match &f.object {
            Value::Number(n) => Some(*n),
            _ => None,
        })
        .expect("a baked world commits history-now");

    let mut relation_years = 0.0f64;
    let mut oldest = 0.0f64;
    let mut per_patron: BTreeMap<hornvale_kernel::EntityId, u64> = BTreeMap::new();
    let mut stock = 0u64;

    for f in world.ledger.find(hornvale_history::PAYS_TRIBUTE_TO) {
        stock += 1;
        // The fact is dated by the day the relation was ESTABLISHED, so its
        // age at `now` is a per-edge duration the ledger genuinely carries.
        let age = f.day.map(|since| now - since).unwrap_or(0.0);
        relation_years += age;
        if age > oldest {
            oldest = age;
        }
        if let Value::Entity(p) = &f.object {
            *per_patron.entry(*p).or_insert(0) += 1;
        }
    }
    let patrons = per_patron.len() as u64;
    let top = per_patron.values().copied().max().unwrap_or(0);
    let top_share = if stock > 0 {
        top as f64 / stock as f64
    } else {
        0.0
    };
    Observed {
        stock,
        relation_years,
        oldest,
        patrons,
        top_share,
    }
}

/// claim: readout(probe: measurement only, run explicitly) — prints the
/// stock/relation_years/oldest/patrons/top_share observables beside the
/// bake's own tribute_collected/tribute_collection_events per seed; no
/// assertion, the module doc records the measured spearman agreement.
#[test]
#[ignore = "probe: measurement only, run explicitly"]
fn ledger_visible_tribute_observables_against_the_bakes_own_flow() {
    let wc = WorldComponents::assemble().expect("registries well-formed");
    println!("TRIB seed,collected,events,stock,relation_years,oldest,patrons,top_share");
    for seed in 1..=36u64 {
        let h = history_for(
            Seed(seed),
            &SkyPins::default(),
            SkyChoice::Generated,
            &TerrainPins::default(),
            &SettlementPins::default(),
            &wc,
        )
        .expect("bakes");
        let c = census(&h);
        let w = build_world_to(
            Seed(seed),
            &SkyPins::default(),
            SkyChoice::Generated,
            &TerrainPins::default(),
            &SettlementPins::default(),
            &wc,
            BuildDepth::Settlements,
        )
        .expect("builds");
        let o = observe(&w);
        println!(
            "TRIB {seed},{:.3},{},{},{:.3},{:.3},{},{:.4}",
            c.tribute_collected,
            c.tribute_collection_events,
            o.stock,
            o.relation_years,
            o.oldest,
            o.patrons,
            o.top_share
        );
    }
}

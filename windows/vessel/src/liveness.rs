//! The Quickening: the world's first autonomous motion. NPCs derived like the
//! possessed agent walk deterministic daily routes; their position over time is
//! a pure schedule (derived, reversible — the routine). This module is the
//! pure foundation only: deriving NPCs and their daily-route schedule. No
//! ledger facts are committed here and no session/tick wiring exists yet
//! (that is later Quickening work); domains are untouched (The Walk §11).

use crate::agent::{settlement_position, walk_depth};
use hornvale_kernel::{
    EntityId, Fact, Ledger, RoomAddr, RoomId, TickSystem, Value, World, WorldTime,
};
use hornvale_locale::LocaleContext;
use hornvale_species::ActivityCycle;

/// A derived non-player agent: a minted entity, a home and a destination room,
/// and its species' activity-cycle. Derived from the genesis world, never
/// stored (re-derivable).
/// type-audit: bare-ok(identifier-text: label)
#[derive(Clone, Debug)]
pub struct Npc {
    /// The NPC's minted ledger entity (subject of its future `agent-at` facts).
    pub entity: EntityId,
    /// Where the NPC rests (its home settlement's room).
    pub home: RoomAddr,
    /// Where the NPC goes when active (a deterministic neighbor room).
    pub destination: RoomAddr,
    /// The species activity-cycle driving the routine.
    pub activity: ActivityCycle,
    /// A short human label for prose ("the herder").
    pub label: String,
}

/// Is the species active at the day-fraction of `t`? Diurnal → active in the
/// daylight half `[0.25, 0.75)`; nocturnal → the complement; crepuscular → the
/// twilight bands. A pure, periodic function (Lorenz-safe: never seeds a
/// forward-integrator, just reads a fraction of `t.day`).
/// type-audit: bare-ok(flag: return)
pub fn active_phase(activity: ActivityCycle, t: WorldTime) -> bool {
    let frac = t.day - t.day.floor(); // day fraction in [0, 1); 0.5 == noon
    match activity {
        ActivityCycle::Diurnal => (0.25..0.75).contains(&frac),
        ActivityCycle::Nocturnal => !(0.25..0.75).contains(&frac),
        // Crepuscular: active at the dawn/dusk bands only (idle this
        // campaign — no crepuscular species is placed yet, authored now so a
        // future species is a data change, not a code change).
        ActivityCycle::Crepuscular => (0.20..0.30).contains(&frac) || (0.70..0.80).contains(&frac),
    }
}

/// The NPC's scheduled position at `t`: its destination when active, else
/// home. Pure and deterministic — the derived routine (never committed).
/// THE ANTI-INERT GUARD: this must actually differ between phases (see
/// `scheduled_position_moves_between_home_and_destination` below) — the
/// world's first liveness work was scrapped once for shipping an actor that
/// never moved.
pub fn scheduled_position(npc: &Npc, t: WorldTime) -> RoomAddr {
    if active_phase(npc.activity, t) {
        npc.destination.clone()
    } else {
        npc.home.clone()
    }
}

/// A game-layer predicate: an agent's room position on a day. Non-functional
/// (position changes over sim time — c5's kind-change shape); the current
/// position is the latest committed one. Registered by the possess session,
/// NOT at genesis (spec §3).
/// type-audit: bare-ok(identifier-text)
pub const AGENT_AT: &str = "agent-at";

/// The movement system (spec §4.3): for each NPC, derive its scheduled
/// position at `at_time` and, if it differs from the last committed position
/// (else the NPC's genesis home), emit a dated `agent-at` fact. Run through
/// c6's `tick`.
pub struct AgentMovements {
    /// The NPCs this tick advances.
    pub npcs: Vec<Npc>,
    /// The world-time being advanced to.
    pub at_time: WorldTime,
}

impl TickSystem for AgentMovements {
    fn label(&self) -> &'static str {
        "agent-movements"
    }
    fn step(&self, frozen: &Ledger) -> Vec<Fact> {
        self.npcs
            .iter()
            .filter_map(|npc| {
                let want = scheduled_position(npc, self.at_time);
                let current =
                    latest_committed_position(frozen, npc).unwrap_or_else(|| npc.home.clone());
                if want == current {
                    None
                } else {
                    Some(Fact {
                        subject: npc.entity,
                        predicate: AGENT_AT.to_string(),
                        object: Value::Text(room_to_text(&want)),
                        place: None,
                        day: Some(self.at_time.day),
                        provenance: "the-quickening".to_string(),
                    })
                }
            })
            .collect()
    }
}

/// The NPC's current position: latest committed `agent-at` ELSE the derived
/// scheduled position at `t`.
pub fn agent_position(ledger: &Ledger, npc: &Npc, t: WorldTime) -> RoomAddr {
    latest_committed_position(ledger, npc).unwrap_or_else(|| scheduled_position(npc, t))
}

/// The last committed `agent-at` position for `npc`, if any.
fn latest_committed_position(ledger: &Ledger, npc: &Npc) -> Option<RoomAddr> {
    match ledger.latest_value_of(npc.entity, AGENT_AT) {
        Some(Value::Text(s)) => Some(room_from_text(s)),
        _ => None,
    }
}

/// Encode a `RoomAddr` as save-format text: the packed `RoomId` (decision
/// 0006), rendered as a decimal `u64` string. Reuses the existing pack/unpack
/// contract rather than inventing a new encoding.
fn room_to_text(r: &RoomAddr) -> String {
    r.pack()
        .expect("a scheduled room is always within MAX_DEPTH")
        .0
        .to_string()
}

/// Decode a `RoomAddr` from its packed-`RoomId` decimal text. Panics on a
/// malformed committed value — a corrupted save is a bug, not a runtime case
/// to route around.
fn room_from_text(s: &str) -> RoomAddr {
    let id: u64 = s
        .parse()
        .unwrap_or_else(|_| panic!("agent-at text '{s}' is not a decimal RoomId"));
    RoomId(id)
        .unpack()
        .unwrap_or_else(|_| panic!("agent-at RoomId {id} does not unpack to a valid RoomAddr"))
}

/// Derive `k` NPCs from the `k` most-populous settlements. Each NPC is minted
/// in `ledger` (a session-owned clone), homed at its settlement's cell room,
/// with a deterministic destination neighbor and its species' activity-cycle.
/// type-audit: bare-ok(count: k)
pub fn derive_npcs(world: &World, ctx: &LocaleContext, ledger: &mut Ledger, k: usize) -> Vec<Npc> {
    // Settlements by population, ties broken by EntityId for determinism.
    let mut settlements = hornvale_settlement::all_settlements(world);
    settlements.sort_by(|a, b| b.population.cmp(&a.population).then(a.id.cmp(&b.id)));
    settlements.truncate(k);

    settlements
        .into_iter()
        .map(|village| {
            let home = settlement_room(world, ctx, village.id);
            let destination = deterministic_destination(home.clone());
            let species = hornvale_species::species_of(world, village.id)
                .unwrap_or_else(|| "goblin".to_string());
            let activity = species_activity(world, &species);
            let entity = ledger.mint_entity();
            Npc {
                entity,
                home,
                destination,
                activity,
                label: format!("{species} of {}", village.name),
            }
        })
        .collect()
}

/// The room containing a settlement's cell at walk depth (mirrors
/// `mint_flagship`, via the shared `settlement_position` helper).
fn settlement_room(world: &World, ctx: &LocaleContext, settlement: EntityId) -> RoomAddr {
    let pos = settlement_position(world, settlement);
    RoomAddr::containing(pos, walk_depth(ctx))
}

/// A deterministic destination neighbor of `home` — the smallest of its three
/// mesh neighbors by address (a fixed, seed-independent, total-order pick;
/// `RoomAddr` is `Ord`, so this needs no extra tie-break key).
fn deterministic_destination(home: RoomAddr) -> RoomAddr {
    home.neighbors()
        .into_iter()
        .min()
        .expect("a room has three neighbors")
}

/// The species' activity-cycle, from its committed `SPECIES_ACTIVITY_CYCLE`
/// fact on the species' own entity (resolved by name via `species_entity`).
/// Defaults to `Diurnal` if the species or the fact is missing.
fn species_activity(world: &World, species: &str) -> ActivityCycle {
    hornvale_species::species_entity(world, species)
        .and_then(|e| {
            match world
                .ledger
                .value_of(e, hornvale_species::SPECIES_ACTIVITY_CYCLE)
            {
                Some(Value::Text(t)) => Some(parse_activity(t)),
                _ => None,
            }
        })
        .unwrap_or(ActivityCycle::Diurnal)
}

/// Parse the committed activity-cycle text (see
/// `windows/worldgen/src/lib.rs`'s species genesis, which commits exactly
/// these three strings). Unknown text defaults to `Diurnal`.
fn parse_activity(t: &str) -> ActivityCycle {
    match t {
        "nocturnal" => ActivityCycle::Nocturnal,
        "crepuscular" => ActivityCycle::Crepuscular,
        _ => ActivityCycle::Diurnal,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_kernel::Seed;

    fn diurnal_npc() -> Npc {
        // A hand-built NPC for the pure-schedule tests (no world needed).
        let home = hornvale_kernel::RoomAddr::containing([1.0, 0.0, 0.0], 6);
        let destination = home.neighbors()[0].clone();
        Npc {
            entity: hornvale_kernel::EntityId::new(1).unwrap(),
            home,
            destination,
            activity: hornvale_species::ActivityCycle::Diurnal,
            label: "herder".to_string(),
        }
    }

    #[test]
    fn active_phase_diurnal_is_day_not_night() {
        // day fraction 0.5 = noon (active for diurnal); 0.0 = midnight (rest).
        assert!(active_phase(
            hornvale_species::ActivityCycle::Diurnal,
            WorldTime { day: 3.5 }
        ));
        assert!(!active_phase(
            hornvale_species::ActivityCycle::Diurnal,
            WorldTime { day: 3.0 }
        ));
    }

    #[test]
    fn scheduled_position_moves_between_home_and_destination() {
        // THE ANTI-INERT GUARD (ledger #8): the actor must actually move.
        let npc = diurnal_npc();
        let at_noon = scheduled_position(&npc, WorldTime { day: 3.5 });
        let at_midnight = scheduled_position(&npc, WorldTime { day: 3.0 });
        assert_eq!(at_noon, npc.destination);
        assert_eq!(at_midnight, npc.home);
        assert_ne!(at_noon, at_midnight, "the NPC must move between phases");
    }

    #[test]
    fn scheduled_position_is_deterministic_and_total() {
        let npc = diurnal_npc();
        // splitmix over t: every t yields a defined position, equal on repeat.
        let mut st = 1u64;
        for _ in 0..200 {
            st = st.wrapping_mul(6364136223846793005).wrapping_add(1);
            let t = WorldTime {
                day: (st % 100_000) as f64 / 1000.0,
            };
            assert_eq!(scheduled_position(&npc, t), scheduled_position(&npc, t));
        }
    }

    #[test]
    fn derive_npcs_are_distinct_and_placed() {
        // Use the real worldgen build for a populated world:
        let world = hornvale_worldgen::build_world(
            Seed(42),
            &hornvale_astronomy::SkyPins::default(),
            hornvale_worldgen::SkyChoice::Generated,
            &hornvale_terrain::TerrainPins::default(),
            &hornvale_worldgen::SettlementPins::default(),
        )
        .unwrap();
        let ctx = LocaleContext::build(&world).unwrap();
        let mut ledger = world.ledger.clone();
        let npcs = derive_npcs(&world, &ctx, &mut ledger, 3);
        assert_eq!(npcs.len(), 3);
        // distinct entities, and each moves (home != destination)
        let ids: std::collections::BTreeSet<_> = npcs.iter().map(|n| n.entity).collect();
        assert_eq!(ids.len(), 3);
        for n in &npcs {
            assert_ne!(
                n.home, n.destination,
                "NPC {} must have a real route",
                n.label
            );
        }
    }

    #[test]
    fn room_text_round_trips() {
        let home = hornvale_kernel::RoomAddr::containing([1.0, 0.0, 0.0], 6);
        let dest = home.neighbors()[0].clone();
        for r in [home, dest] {
            assert_eq!(room_from_text(&room_to_text(&r)), r);
        }
    }

    fn registry_with_agent_at() -> hornvale_kernel::ConceptRegistry {
        let mut r = hornvale_kernel::ConceptRegistry::default();
        r.register_predicate(AGENT_AT, false, "an agent's position on a day")
            .unwrap();
        r
    }

    #[test]
    fn tick_commits_agent_at_when_the_npc_moves() {
        let r = registry_with_agent_at();
        let mut ledger = Ledger::default();
        let e = ledger.mint_entity();
        let home = hornvale_kernel::RoomAddr::containing([1.0, 0.0, 0.0], 6);
        let npc = Npc {
            entity: e,
            home: home.clone(),
            destination: home.neighbors()[0].clone(),
            activity: hornvale_species::ActivityCycle::Diurnal,
            label: "herder".into(),
        };
        // Advance to noon (active) -> the NPC is at its destination -> one agent-at fact.
        let sys = AgentMovements {
            npcs: vec![npc.clone()],
            at_time: WorldTime { day: 0.5 },
        };
        let next = hornvale_kernel::tick(&ledger, &[&sys], &["agent-movements"], &r).unwrap();
        assert_eq!(
            agent_position(&next, &npc, WorldTime { day: 0.5 }),
            npc.destination
        );
        assert_eq!(next.find(AGENT_AT).count(), 1);
        ledger = next;
        // Advance to midnight (rest) -> back home -> a second agent-at fact.
        let sys2 = AgentMovements {
            npcs: vec![npc.clone()],
            at_time: WorldTime { day: 1.0 },
        };
        let n2 = hornvale_kernel::tick(&ledger, &[&sys2], &["agent-movements"], &r).unwrap();
        assert_eq!(agent_position(&n2, &npc, WorldTime { day: 1.0 }), npc.home);
        assert_eq!(n2.find(AGENT_AT).count(), 2);
    }

    #[test]
    fn tick_commits_nothing_when_position_unchanged() {
        let r = registry_with_agent_at();
        let mut ledger = Ledger::default();
        let e = ledger.mint_entity();
        let home = hornvale_kernel::RoomAddr::containing([1.0, 0.0, 0.0], 6);
        let npc = Npc {
            entity: e,
            home: home.clone(),
            destination: home.neighbors()[0].clone(),
            activity: hornvale_species::ActivityCycle::Diurnal,
            label: "herder".into(),
        };
        // Two ticks both at rest (midnight) -> position never changes -> at most one fact.
        let s1 = AgentMovements {
            npcs: vec![npc.clone()],
            at_time: WorldTime { day: 1.0 },
        };
        let n1 = hornvale_kernel::tick(&ledger, &[&s1], &["agent-movements"], &r).unwrap();
        let s2 = AgentMovements {
            npcs: vec![npc.clone()],
            at_time: WorldTime { day: 2.0 },
        };
        let n2 = hornvale_kernel::tick(&n1, &[&s2], &["agent-movements"], &r).unwrap();
        // home == genesis default, so the first tick may emit 0 (already home); the
        // key invariant: no SPURIOUS second fact for an unchanged position.
        assert!(
            n2.find(AGENT_AT).count() <= 1,
            "no spurious agent-at for an unchanged position"
        );
    }

    #[test]
    fn jump_past_a_phase_lands_coherent() {
        // THE JUMP CASE (spec §5.2): a single wait from day 0.2 (rest) to day 1.9
        // (rest again, after a full active phase) must leave the ledger coherent:
        // the NPC's latest committed position equals its scheduled position at 1.9.
        let r = registry_with_agent_at();
        let mut ledger = Ledger::default();
        let e = ledger.mint_entity();
        let home = hornvale_kernel::RoomAddr::containing([1.0, 0.0, 0.0], 6);
        let npc = Npc {
            entity: e,
            home: home.clone(),
            destination: home.neighbors()[0].clone(),
            activity: hornvale_species::ActivityCycle::Diurnal,
            label: "herder".into(),
        };
        let sys = AgentMovements {
            npcs: vec![npc.clone()],
            at_time: WorldTime { day: 1.9 },
        };
        let next = hornvale_kernel::tick(&ledger, &[&sys], &["agent-movements"], &r).unwrap();
        assert_eq!(
            agent_position(&next, &npc, WorldTime { day: 1.9 }),
            scheduled_position(&npc, WorldTime { day: 1.9 }),
            "after a multi-day jump, the committed position matches the schedule at t_new"
        );
    }
}

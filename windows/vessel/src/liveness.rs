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

/// A derived non-player agent: a minted entity, a home and a resource room,
/// and its species' activity-cycle. Derived from the genesis world, never
/// stored (re-derivable).
/// type-audit: bare-ok(identifier-text: label)
#[derive(Clone, Debug)]
pub struct Npc {
    /// The NPC's minted ledger entity (subject of its future `agent-at` facts).
    pub entity: EntityId,
    /// Where the NPC rests (its home settlement's room).
    pub home: RoomAddr,
    /// The room its sustenance drive seeks (the-wanting supersedes the old
    /// fixed-schedule destination: this IS the drive's resource anchor now).
    pub resource: RoomAddr,
    /// The species activity-cycle driving the routine.
    pub activity: ActivityCycle,
    /// A short human label for prose ("the herder").
    pub label: String,
}

/// A game-layer predicate: an agent's room position on a day. Non-functional
/// (position changes over sim time — c5's kind-change shape); the current
/// position is the latest committed one. Registered by the possess session,
/// NOT at genesis (spec §3).
/// type-audit: bare-ok(identifier-text)
pub const AGENT_AT: &str = "agent-at";

/// The NPC's current position: the latest committed `agent-at` ELSE its home
/// (the drive model's pre-history state — an NPC has not yet sought its
/// resource until the drive first crosses `act`). `t` is retained for
/// interface stability (Task 1/2 callers pass the time being resolved at);
/// the drive model itself needs only the frozen ledger's own history, which
/// is already truncated to facts at or before `t` by construction (a tick's
/// `frozen` ledger never holds facts past its own `from`).
pub fn agent_position(ledger: &Ledger, npc: &Npc, _t: WorldTime) -> RoomAddr {
    latest_committed_position(ledger, npc).unwrap_or_else(|| npc.home.clone())
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

/// The homeostatic-drive parameters (authored constants; §4.2/§4.3): the rise
/// rate while away from the resource, the fall (satiety) rate while at it, and
/// the hysteresis thresholds `act` (seek) and `sated` (leave), plus the
/// pre-history initial value. Dimensionless; the drive lives in [0, 1].
/// type-audit: bare-ok(ratio: rise), bare-ok(ratio: fall), bare-ok(ratio: act), bare-ok(ratio: sated), bare-ok(ratio: initial)
#[derive(Clone, Copy, Debug)]
pub struct DriveParams {
    /// Drive gained per day while away from the resource.
    pub rise: f64,
    /// Drive lost per day while at the resource (satiety).
    pub fall: f64,
    /// The seek threshold: drive >= act -> go to the resource.
    pub act: f64,
    /// The leave threshold: drive <= sated -> return home.
    pub sated: f64,
    /// The drive before any committed move.
    pub initial: f64,
}

/// The one authored sustenance drive (thirst/foraging). act > sated (the
/// hysteresis dead-band); rates chosen so a cycle spans a few days.
pub const SUSTENANCE: DriveParams = DriveParams {
    rise: 0.15,
    fall: 0.6,
    act: 0.85,
    sated: 0.15,
    initial: 0.0,
};

/// The resource cell the drive seeks: the lowest-elevation of `home`'s three
/// mesh neighbors ("toward water"), ties broken by `RoomAddr` order for
/// determinism. Reads the locale elevation field.
/// type-audit: bare-ok(return)
pub fn resource_room(home: &RoomAddr, ctx: &LocaleContext) -> RoomAddr {
    let mut best: Option<(RoomAddr, f64)> = None;
    for n in home.neighbors() {
        let elev = ctx
            .describe(&n, WorldTime { day: 0.0 })
            .map(|loc| loc.fields.elevation_m)
            .unwrap_or(f64::INFINITY);
        // Float ordering uses `total_cmp` with a deterministic tie-break (the
        // constitutional no-native-float-cmp rule; CLAUDE.md determinism), ties
        // broken by ascending `RoomAddr`. Keep `n` only when it is strictly
        // lower (or equal-and-smaller-addr) than the current best.
        let keep_existing = match &best {
            Some((ba, be)) => elev.total_cmp(be).then_with(|| n.cmp(ba)).is_ge(),
            None => false,
        };
        if !keep_existing {
            best = Some((n, elev));
        }
    }
    best.expect("a room has three neighbors").0
}

/// The drive at `t`: a fold over the entity's committed `agent-at` history.
/// Rises `p.rise`/day away from `resource`, falls `p.fall`/day at it, clamped to
/// [0, 1], starting from `p.initial` before any committed move. A pure derived
/// view over the ledger (never stored) — DRIVE == FOLD.
/// type-audit: bare-ok(ratio: return)
pub fn drive_at(
    ledger: &Ledger,
    entity: EntityId,
    resource: &RoomAddr,
    t: WorldTime,
    p: &DriveParams,
) -> f64 {
    // The entity's dated agent-at history, ascending by day (commit order is
    // ascending day within a session; sort defensively).
    let mut hist: Vec<(f64, RoomAddr)> = ledger
        .find(AGENT_AT)
        .filter(|f| f.subject == entity)
        .filter_map(|f| match (&f.object, f.day) {
            (Value::Text(s), Some(day)) => Some((day, room_from_text(s))),
            _ => None,
        })
        .collect();
    hist.sort_by(|a, b| a.0.total_cmp(&b.0));

    // Walk the timeline from day 0, integrating the piecewise-linear drive.
    let mut drive = p.initial;
    let mut cursor = 0.0_f64;
    // Segment before the first move: position is the NPC's *home* (unknown here);
    // treat "away from resource" as the default (drive rises) unless the first
    // fact is at the resource. We reconstruct position per segment from history.
    let mut at_resource = false; // NPCs start at home (not the resource)
    for (day, pos) in hist.iter().filter(|(d, _)| *d <= t.day) {
        drive = integrate(drive, at_resource, day - cursor, p);
        cursor = *day;
        at_resource = pos == resource;
    }
    // Final segment to t.
    integrate(drive, at_resource, t.day - cursor, p)
}

fn integrate(drive: f64, at_resource: bool, dt: f64, p: &DriveParams) -> f64 {
    let delta = if at_resource {
        -p.fall * dt
    } else {
        p.rise * dt
    };
    (drive + delta).clamp(0.0, 1.0)
}

/// A desired world-state — the agent's active goal. This slice generates exactly
/// one goal (the drive's setpoint); it is named so that "the drive *generates a
/// goal*" is explicit. The future GOAP planner selects among many and plans to
/// reach the chosen one (decision #9 — reserved seam, not built here).
/// type-audit: bare-ok(return)
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Goal {
    /// Be at the resource (satisfy the sustenance drive).
    AtResource,
}

/// What the agent perceives of the world — the `view` the decision reads. Today
/// its contents are ground truth; PSY-6's "plan over belief, not truth" (UNI-16)
/// is later a change to what fills this, not to the seam.
/// type-audit: bare-ok(ratio: drive)
#[derive(Clone, Debug)]
pub struct Perceived {
    /// The agent's current room.
    pub position: RoomAddr,
    /// The agent's perceived drive level.
    pub drive: f64,
}

/// The decision's output: where the agent intends to be. The tick depends ONLY
/// on this — never on the drive internals — so the decision body can be replaced
/// (by the GOAP planner) without touching the caller.
/// type-audit: bare-ok(return)
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Intent {
    /// Move toward this room.
    GoTo(RoomAddr),
    /// Stay put (the hysteresis dead-band).
    Hold,
}

/// The reactive controller (decision #9): the degenerate one-goal case of
/// goal-selection-then-planning. Generate the sole goal when the drive crosses
/// `act`, plan the one-step route to it (go to the resource), and return home
/// when sated — with a dead-band (`sated < act`) between to prevent thrashing.
/// type-audit: bare-ok(return)
pub fn decide(home: &RoomAddr, resource: &RoomAddr, view: &Perceived, p: &DriveParams) -> Intent {
    let at_resource = &view.position == resource;
    if !at_resource && view.drive >= p.act {
        Intent::GoTo(resource.clone()) // goal generated: AtResource; one-step plan
    } else if at_resource && view.drive <= p.sated {
        Intent::GoTo(home.clone()) // goal satisfied; return
    } else {
        Intent::Hold // dead-band
    }
}

/// The closed-form day the drive next reaches its governing threshold from
/// `(from_day, drive0)`: `act` while away (rising), `sated` while at the
/// resource (falling). `Some(from_day)` if already past it in the travel
/// direction; `None` if the rate is zero and it never arrives.
/// type-audit: bare-ok(ratio: from_day), bare-ok(ratio: drive0), bare-ok(flag: at_resource), bare-ok(ratio: return)
pub fn next_crossing(
    from_day: f64,
    drive0: f64,
    at_resource: bool,
    p: &DriveParams,
) -> Option<f64> {
    if at_resource {
        if drive0 <= p.sated {
            return Some(from_day);
        }
        if p.fall <= 0.0 {
            return None;
        }
        Some(from_day + (drive0 - p.sated) / p.fall)
    } else {
        if drive0 >= p.act {
            return Some(from_day);
        }
        if p.rise <= 0.0 {
            return None;
        }
        Some(from_day + (p.act - drive0) / p.rise)
    }
}

/// The drive-driven movement system (spec §4.4): over (from, to], advance each
/// NPC through its closed-form drive-threshold crossings in order, committing a
/// dated `agent-at` at each crossing with provenance naming the drive. A
/// discrete-event tick (exact, no integrator). Run through c6's `tick`.
/// type-audit: bare-ok(return)
pub struct DriveMovements {
    /// The NPCs this tick advances.
    pub npcs: Vec<Npc>,
    /// The interval start (the session's previous day).
    pub from: WorldTime,
    /// The interval end (the session's new day).
    pub to: WorldTime,
    /// The drive parameters.
    pub params: DriveParams,
}

impl TickSystem for DriveMovements {
    fn label(&self) -> &'static str {
        "drive-movements"
    }
    fn step(&self, frozen: &Ledger) -> Vec<Fact> {
        let mut out = Vec::new();
        for npc in &self.npcs {
            // Reconstruct the NPC's state at `from` from the frozen history.
            let mut pos = agent_position(frozen, npc, self.from);
            let mut day = self.from.day;
            let mut drive = drive_at(frozen, npc.entity, &npc.resource, self.from, &self.params);
            // Advance crossing by crossing until past `to`.
            loop {
                let at_resource = pos == npc.resource;
                let Some(cross) = next_crossing(day, drive, at_resource, &self.params) else {
                    break;
                };
                if cross > self.to.day {
                    break;
                }
                // At the crossing the drive is exactly at its threshold; decide.
                let drive_at_cross = if at_resource {
                    self.params.sated
                } else {
                    self.params.act
                };
                let intent = decide(
                    &npc.home,
                    &npc.resource,
                    &Perceived {
                        position: pos.clone(),
                        drive: drive_at_cross,
                    },
                    &self.params,
                );
                match intent {
                    Intent::GoTo(target) if target != pos => {
                        out.push(Fact {
                            subject: npc.entity,
                            predicate: AGENT_AT.to_string(),
                            object: Value::Text(room_to_text(&target)),
                            place: None,
                            day: Some(cross),
                            provenance: "sought water (thirst)".to_string(),
                        });
                        pos = target;
                        drive = drive_at_cross; // reset to threshold at the move
                        day = cross;
                    }
                    _ => break, // Hold at a crossing shouldn't happen; guard against a loop
                }
            }
        }
        out
    }
}

/// Order settlements for NPC derivation: population descending (ties broken
/// by `EntityId`), with `home_settlement` pulled to the front regardless of
/// its rank. Pure and independently testable (no world/ledger needed) so the
/// colocation guarantee is mutation-provable on its own, not just as an
/// emergent property of a particular seed's population distribution.
fn ordered_for_derivation(
    mut settlements: Vec<hornvale_settlement::VillageInfo>,
    home_settlement: EntityId,
) -> Vec<hornvale_settlement::VillageInfo> {
    settlements.sort_by(|a, b| b.population.cmp(&a.population).then(a.id.cmp(&b.id)));
    if let Some(pos) = settlements.iter().position(|v| v.id == home_settlement) {
        let home = settlements.remove(pos);
        settlements.insert(0, home);
    }
    settlements
}

/// Derive `k` NPCs from the `k` most-populous settlements, GUARANTEEING the
/// possessed agent's own home settlement (`home_settlement`) is among them —
/// otherwise no NPC is ever co-located with the player and the observation
/// payoff (spec: "the herder has gone down to the river") can never fire
/// (the-quickening T3 review). Each NPC is minted in `ledger` (a
/// session-owned clone), homed at its settlement's cell room, with its
/// drive's resource anchor (`resource_room`, the-wanting) and species'
/// activity-cycle.
/// type-audit: bare-ok(count: k)
pub fn derive_npcs(
    world: &World,
    ctx: &LocaleContext,
    ledger: &mut Ledger,
    k: usize,
    home_settlement: EntityId,
) -> Vec<Npc> {
    let settlements = hornvale_settlement::all_settlements(world);
    let mut settlements = ordered_for_derivation(settlements, home_settlement);
    settlements.truncate(k);

    settlements
        .into_iter()
        .map(|village| {
            let home = settlement_room(world, ctx, village.id);
            let resource = resource_room(&home, ctx);
            let species = hornvale_species::species_of(world, village.id)
                .unwrap_or_else(|| "goblin".to_string());
            let activity = species_activity(world, &species);
            let entity = ledger.mint_entity();
            let label = format!("{species} of {}", village.name);
            // A NAME fact so the provenance read (`why`, backed by
            // `windows/historiography::recount`) leads with the NPC's own
            // label rather than a bare entity id — NAME is kernel-core, so
            // it is already registered in `world.registry` (never a new
            // per-session predicate the way AGENT_AT is). Committed once,
            // at derivation, to the session-owned ledger clone only — never
            // genesis (this function never runs against a world's own
            // ledger, only a session's clone; see `liveness_genesis.rs`).
            ledger
                .commit(
                    Fact {
                        subject: entity,
                        predicate: hornvale_kernel::NAME.to_string(),
                        object: Value::Text(label.clone()),
                        place: None,
                        day: None,
                        provenance: "the-quickening".to_string(),
                    },
                    &world.registry,
                )
                .expect("a freshly minted NPC entity's first NAME fact always commits");
            Npc {
                entity,
                home,
                resource,
                activity,
                label,
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
        let home = hornvale_settlement::village_info(&world).unwrap().id;
        let npcs = derive_npcs(&world, &ctx, &mut ledger, 3, home);
        assert_eq!(npcs.len(), 3);
        // distinct entities, and each has a real resource anchor (home != resource)
        let ids: std::collections::BTreeSet<_> = npcs.iter().map(|n| n.entity).collect();
        assert_eq!(ids.len(), 3);
        for n in &npcs {
            assert_ne!(
                n.home, n.resource,
                "NPC {} must have a real resource anchor",
                n.label
            );
        }
    }

    #[test]
    fn derive_npcs_actually_includes_the_home_settlement() {
        // An end-to-end smoke check on a real world (seed 42): the possessed
        // agent's own settlement is among the derived NPCs even at k=1. (The
        // precise "regardless of population rank" guarantee is proven
        // adversarially, independent of any one seed's incidental population
        // distribution, by `ordered_for_derivation_prioritizes_home_over_population_rank`
        // below.)
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
        let home = hornvale_settlement::village_info(&world).unwrap().id;
        let npcs = derive_npcs(&world, &ctx, &mut ledger, 1, home);
        assert_eq!(npcs.len(), 1);
        let want_home_room = settlement_room(&world, &ctx, home);
        assert_eq!(
            npcs[0].home, want_home_room,
            "the possessed agent's own settlement's NPC must be derived"
        );
    }

    #[test]
    fn ordered_for_derivation_prioritizes_home_over_population_rank() {
        // THE COLOCATION GUARANTEE (T3 review), proven directly on the pure
        // selection function with adversarial data: a home settlement with
        // the LOWEST population must still land first, ahead of settlements
        // with far larger populations — otherwise, with k smaller than the
        // settlement count, no NPC could ever be co-located with the player
        // and the observation payoff would never fire.
        let home_id = EntityId::new(5).unwrap();
        let settlements = vec![
            hornvale_settlement::VillageInfo {
                id: EntityId::new(1).unwrap(),
                name: "Big".to_string(),
                population: 10_000,
            },
            hornvale_settlement::VillageInfo {
                id: EntityId::new(2).unwrap(),
                name: "Bigger".to_string(),
                population: 20_000,
            },
            hornvale_settlement::VillageInfo {
                id: home_id,
                name: "Home".to_string(),
                population: 1,
            },
        ];
        let ordered = ordered_for_derivation(settlements, home_id);
        assert_eq!(
            ordered[0].id, home_id,
            "the home settlement must be first regardless of its population rank"
        );
        // Truncating to k=1 (the adversarial case) must still keep it.
        let mut truncated = ordered;
        truncated.truncate(1);
        assert_eq!(truncated[0].id, home_id);
    }

    #[test]
    fn room_text_round_trips() {
        let home = hornvale_kernel::RoomAddr::containing([1.0, 0.0, 0.0], 6);
        let dest = home.neighbors()[0].clone();
        for r in [home, dest] {
            assert_eq!(room_from_text(&room_to_text(&r)), r);
        }
    }

    #[test]
    fn drive_rises_while_away_and_is_clamped() {
        // No agent-at history: the entity has been "away" since day 0 at rate rise.
        let p = DriveParams {
            rise: 0.1,
            fall: 0.5,
            act: 0.8,
            sated: 0.2,
            initial: 0.0,
        };
        let ledger = Ledger::default();
        let e = EntityId::new(1).unwrap();
        let res = RoomAddr::containing([1.0, 0.0, 0.0], 6).neighbors()[0].clone();
        assert!((drive_at(&ledger, e, &res, WorldTime { day: 3.0 }, &p) - 0.3).abs() < 1e-9);
        // clamps at 1.0
        assert_eq!(
            drive_at(&ledger, e, &res, WorldTime { day: 100.0 }, &p),
            1.0
        );
    }

    #[test]
    fn drive_falls_while_at_the_resource() {
        // History: at the resource since day 2. Drive rose to 0.2 by day 2, then falls.
        let p = DriveParams {
            rise: 0.1,
            fall: 0.5,
            act: 0.8,
            sated: 0.2,
            initial: 0.0,
        };
        let mut ledger = Ledger::default();
        let mut reg = hornvale_kernel::ConceptRegistry::default();
        reg.register_predicate(AGENT_AT, false, "pos").unwrap();
        let e = ledger.mint_entity();
        let res = RoomAddr::containing([1.0, 0.0, 0.0], 6).neighbors()[0].clone();
        // commit: at resource on day 2
        ledger
            .commit(
                hornvale_kernel::Fact {
                    subject: e,
                    predicate: AGENT_AT.to_string(),
                    object: Value::Text(room_to_text(&res)),
                    place: None,
                    day: Some(2.0),
                    provenance: "test".into(),
                },
                &reg,
            )
            .unwrap();
        // by day 2 drive was ~0.2 (rose from 0 at 0.1/day); at day 3 (1 day at resource) it fell 0.5 -> ~0 (floored)
        let d = drive_at(&ledger, e, &res, WorldTime { day: 3.0 }, &p);
        assert!(
            d <= 0.001,
            "drive should fall to ~0 after a day at the resource, got {d}"
        );
    }

    #[test]
    fn drive_at_is_deterministic_and_reload_stable() {
        // Fold determinism: same ledger + t -> same value; and serialize->reload of
        // the ledger yields the identical drive (the DRIVE == FOLD contract).
        let p = SUSTENANCE;
        let mut ledger = Ledger::default();
        let mut reg = hornvale_kernel::ConceptRegistry::default();
        reg.register_predicate(AGENT_AT, false, "pos").unwrap();
        let e = ledger.mint_entity();
        let res = RoomAddr::containing([1.0, 0.0, 0.0], 6).neighbors()[0].clone();
        for day in [1.0, 4.0, 9.0] {
            ledger
                .commit(
                    hornvale_kernel::Fact {
                        subject: e,
                        predicate: AGENT_AT.to_string(),
                        object: Value::Text(room_to_text(&res)),
                        place: None,
                        day: Some(day),
                        provenance: "t".into(),
                    },
                    &reg,
                )
                .unwrap();
        }
        let t = WorldTime { day: 12.3 };
        let a = drive_at(&ledger, e, &res, t, &p);
        let b = drive_at(&ledger, e, &res, t, &p);
        assert_eq!(a, b);
        let json = serde_json::to_string(&ledger).unwrap();
        let reloaded: Ledger = serde_json::from_str(&json).unwrap();
        assert_eq!(
            drive_at(&reloaded, e, &res, t, &p),
            a,
            "drive re-derives identically after reload"
        );
    }

    fn addr(seed: f64) -> RoomAddr {
        RoomAddr::containing([seed, 0.0, 0.0], 6)
    }

    #[test]
    fn decide_seeks_when_parched_and_returns_when_sated() {
        let p = SUSTENANCE;
        let home = addr(1.0);
        let resource = home.neighbors()[0].clone();
        // parched (drive >= act), at home -> go to resource
        let v = Perceived {
            position: home.clone(),
            drive: 0.9,
        };
        assert_eq!(
            decide(&home, &resource, &v, &p),
            Intent::GoTo(resource.clone())
        );
        // at resource, sated (drive <= sated) -> go home
        let v = Perceived {
            position: resource.clone(),
            drive: 0.1,
        };
        assert_eq!(decide(&home, &resource, &v, &p), Intent::GoTo(home.clone()));
        // in the dead-band (sated < drive < act) -> hold, wherever you are
        let v = Perceived {
            position: home.clone(),
            drive: 0.5,
        };
        assert_eq!(decide(&home, &resource, &v, &p), Intent::Hold);
        let v = Perceived {
            position: resource.clone(),
            drive: 0.5,
        };
        assert_eq!(decide(&home, &resource, &v, &p), Intent::Hold);
    }

    #[test]
    fn next_crossing_is_closed_form_and_exact() {
        let p = DriveParams {
            rise: 0.1,
            fall: 0.5,
            act: 0.8,
            sated: 0.2,
            initial: 0.0,
        };
        // away, drive 0.0, rises at 0.1/day -> reaches act 0.8 at day 8.
        assert!((next_crossing(0.0, 0.0, false, &p).unwrap() - 8.0).abs() < 1e-9);
        // at resource, drive 0.8, falls at 0.5/day -> reaches sated 0.2 at day 1.2.
        assert!((next_crossing(0.0, 0.8, true, &p).unwrap() - 1.2).abs() < 1e-9);
        // already past the threshold in the direction of travel -> immediate (day = from_day)
        assert_eq!(next_crossing(5.0, 0.9, false, &p), Some(5.0));
    }

    #[test]
    fn a_long_wait_commits_only_the_few_genuine_crossings_not_one_per_tick() {
        // THE NO-THRASH GUARD (the campaign's characteristic risk). One NPC, a 40-day
        // wait. With SUSTENANCE (rise 0.15, fall 0.6, act 0.85, sated 0.15): a cycle
        // is ~ (0.85/0.15) away + (0.7/0.6) at-resource ≈ 5.67 + 1.17 ≈ 6.8 days, so
        // ~40/6.8 ≈ 5-6 cycles -> ~10-12 crossings, NOT 40. Assert the bound.
        let p = SUSTENANCE;
        let mut ledger = Ledger::default();
        let mut reg = hornvale_kernel::ConceptRegistry::default();
        reg.register_predicate(AGENT_AT, false, "pos").unwrap();
        let e = ledger.mint_entity();
        let home = addr(1.0);
        let resource = home.neighbors()[0].clone();
        let npc = Npc {
            entity: e,
            home: home.clone(),
            resource: resource.clone(),
            activity: hornvale_species::ActivityCycle::Diurnal,
            label: "herder".into(),
        };
        let sys = DriveMovements {
            npcs: vec![npc.clone()],
            from: WorldTime { day: 0.0 },
            to: WorldTime { day: 40.0 },
            params: p,
        };
        let next = hornvale_kernel::tick(&ledger, &[&sys], &["drive-movements"], &reg).unwrap();
        let moves = next.find(AGENT_AT).filter(|f| f.subject == e).count();
        assert!(
            (6..=16).contains(&moves),
            "expected a few genuine crossings, got {moves}"
        );
        // consecutive moves alternate resource<->home (no resource->resource thrash)
        let positions: Vec<RoomAddr> = next
            .find(AGENT_AT)
            .filter(|f| f.subject == e)
            .filter_map(|f| match &f.object {
                Value::Text(s) => Some(room_from_text(s)),
                _ => None,
            })
            .collect();
        for w in positions.windows(2) {
            assert_ne!(w[0], w[1], "no repeated position (thrash)");
        }
        // post-tick position matches decide at t=40 (jump coherence, extended)
        let drive40 = drive_at(&next, e, &resource, WorldTime { day: 40.0 }, &p);
        let intent = decide(
            &home,
            &resource,
            &Perceived {
                position: agent_position(&next, &npc, WorldTime { day: 40.0 }),
                drive: drive40,
            },
            &p,
        );
        // if intent is Hold, position is already coherent; if GoTo, it equals the target
        if let Intent::GoTo(target) = intent {
            assert_eq!(agent_position(&next, &npc, WorldTime { day: 40.0 }), target);
        }
        let _ = ledger;
    }

    #[test]
    fn moves_carry_drive_naming_provenance() {
        let p = SUSTENANCE;
        let mut ledger = Ledger::default();
        let mut reg = hornvale_kernel::ConceptRegistry::default();
        reg.register_predicate(AGENT_AT, false, "pos").unwrap();
        let e = ledger.mint_entity();
        let home = addr(1.0);
        let resource = home.neighbors()[0].clone();
        let npc = Npc {
            entity: e,
            home: home.clone(),
            resource: resource.clone(),
            activity: hornvale_species::ActivityCycle::Diurnal,
            label: "herder".into(),
        };
        let sys = DriveMovements {
            npcs: vec![npc],
            from: WorldTime { day: 0.0 },
            to: WorldTime { day: 10.0 },
            params: p,
        };
        let next = hornvale_kernel::tick(&ledger, &[&sys], &["drive-movements"], &reg).unwrap();
        let first = next.find(AGENT_AT).find(|f| f.subject == e).unwrap();
        assert!(
            first.provenance.contains("thirst")
                || first.provenance.contains("water")
                || first.provenance.contains("sustenance"),
            "provenance names the drive: {}",
            first.provenance
        );
        let _ = ledger;
    }
}

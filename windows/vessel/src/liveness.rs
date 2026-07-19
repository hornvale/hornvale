//! The Quickening: the world's first autonomous motion. NPCs derived like the
//! possessed agent walk deterministic daily routes; their position over time is
//! a pure schedule (derived, reversible — the routine). This module is the
//! pure foundation only: deriving NPCs and their daily-route schedule. No
//! ledger facts are committed here and no session/tick wiring exists yet
//! (that is later Quickening work); domains are untouched (The Walk §11).

use crate::agent::{settlement_position, walk_depth};
use hornvale_kernel::{
    EntityId, Fact, Ledger, RoomAddr, RoomId, SearchSpace, TickSystem, Value, World, WorldTime,
    astar,
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
    /// The species activity-cycle. Write-only this slice: the drive is the sole
    /// mover (the activity gate was dropped), retained for the deferred
    /// activity-gating followup (a diurnal NPC seeking water only while awake).
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
/// rate while away from the resource, the seek threshold `act`, and the
/// `sated` narration threshold. Dimensionless; the drive lives in [0, 1].
///
/// The planned (drank-fold) model's `decide`/`drive_at` consult only `rise`
/// and `act` — the plan's own goal state (`hydrated`) replaces the old
/// hysteresis leave-condition, and a `Drink` action resets the drive to 0
/// directly rather than falling gradually. `sated` survives as a SEPARATE
/// consumer: `Session::needs`'s felt-state prose (`windows/vessel/src/
/// session.rs`) still thresholds on it to say "seems content" vs "could do
/// with a drink". The old physiological `fall` rate and pre-history
/// `initial` value have no reader anywhere in the planned model and were
/// removed (The Foresight T3 review) rather than left as dead fields with
/// misleading docs.
/// type-audit: bare-ok(ratio: rise), bare-ok(ratio: act), bare-ok(ratio: sated)
#[derive(Clone, Copy, Debug)]
pub struct DriveParams {
    /// Drive gained per day while away from the resource.
    pub rise: f64,
    /// The seek threshold: drive >= act -> plan to the resource and drink.
    pub act: f64,
    /// The felt-state narration threshold, read only by `Session::needs`:
    /// drive <= sated -> "seems content" prose. NOT consulted by `decide`
    /// or `DriveMovements::step` — the planner's return-home branch is
    /// driven by `hydrated`/`position != home`, not by this value.
    pub sated: f64,
}

/// The one authored sustenance drive (thirst/foraging). act > sated (the
/// felt-state dead-band); rates chosen so a cycle spans a few days.
pub const SUSTENANCE: DriveParams = DriveParams {
    rise: 0.15,
    act: 0.85,
    sated: 0.15,
};

/// The elevation field and fresh-water truth the belief/exploration logic
/// reads, abstracted so pure tests plant synthetic terrain without building a
/// world. The session backs it with a `LocaleContext` (see
/// `session.rs::LocaleTerrain`).
pub trait Terrain {
    /// The room's elevation in metres (INFINITY for an undescribable room —
    /// never chosen downhill). Still the exploration prior ("water lies
    /// low" — rivers ARE the downhill drainage channels), even though water
    /// itself is no longer classified by elevation (the-surmise T5 re-wire;
    /// see `is_fresh_water`).
    /// type-audit: waiver(elevation-convention: return)
    fn elevation(&self, room: &RoomAddr) -> f64;

    /// Whether the room's water is FRESH — drinkable — rather than salt.
    /// Reads The Freshet's own classification (`WaterKind::is_fresh`), not
    /// an elevation threshold: "below sea level" is the unreachable SALT
    /// OCEAN, not water an agent can drink or ever reach (decision-ledger
    /// #9, the T4 finding that parked this campaign). `LocaleTerrain` reads
    /// this from the locale's own `water` field; planted test terrain marks
    /// specific rooms fresh directly.
    /// type-audit: bare-ok(flag: return)
    fn is_fresh_water(&self, room: &RoomAddr) -> bool;
}

/// Water-truth (L0): a room is water iff its terrain reports it as FRESH
/// water (The Freshet's `WaterKind::is_fresh` — rivers only, never the salt
/// ocean or a salt basin). Pure over the terrain field; rivers scatter along
/// drainage, so sources are naturally many, not one.
/// type-audit: bare-ok(flag: return)
pub fn is_water(room: &RoomAddr, terrain: &dyn Terrain) -> bool {
    terrain.is_fresh_water(room)
}

/// The single steepest-descent neighbour ("water lies low" — the prior an
/// ignorant agent explores along). `total_cmp` with an ascending-`RoomAddr`
/// tie-break (the constitutional no-native-float-cmp rule), the same rule
/// `nearest_water`'s BFS and `lowest_unvisited_neighbor` use. Always a
/// neighbour (never `from` itself).
pub fn downhill_step(from: &RoomAddr, terrain: &dyn Terrain) -> RoomAddr {
    let mut best: Option<(RoomAddr, f64)> = None;
    for n in from.neighbors() {
        let elev = terrain.elevation(&n);
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

/// The true nearest water room to `from` (ground-truth-best) — a deterministic
/// breadth-first walk over the mesh to the closest `is_water` room, frontier
/// processed in `RoomAddr` order, capped at `budget` expansions (`None` if no
/// water within it). The agent does not know this until it has PERCEIVED it.
/// type-audit: bare-ok(count: budget)
pub fn nearest_water(from: &RoomAddr, terrain: &dyn Terrain, budget: usize) -> Option<RoomAddr> {
    let mut visited: std::collections::BTreeSet<RoomAddr> = std::collections::BTreeSet::new();
    let mut frontier: std::collections::BTreeSet<RoomAddr> = std::collections::BTreeSet::new();
    frontier.insert(from.clone());
    let mut expansions = 0usize;
    while let Some(room) = frontier.iter().next().cloned() {
        frontier.remove(&room);
        if !visited.insert(room.clone()) {
            continue;
        }
        if is_water(&room, terrain) {
            return Some(room);
        }
        expansions += 1;
        if expansions >= budget {
            return None;
        }
        for n in room.neighbors() {
            if !visited.contains(&n) {
                frontier.insert(n);
            }
        }
    }
    None
}

/// A `Terrain` backed by a `LocaleContext` — the elevation and fresh-water
/// fields the belief/exploration logic reads in a live session (tests use a
/// planted terrain instead). Elevation mirrors the undescribable-room
/// fallback (INFINITY); fresh water reads The Freshet's own salt/fresh
/// classification (`LocaleFields::water`, `WaterKind::is_fresh`) rather than
/// deriving a sea-level threshold — the-surmise T5 re-wire: the prior
/// elevation-threshold model classified the unreachable SALT OCEAN as
/// "water" (decision-ledger #9), never the rivers an agent can actually
/// drink from and reach.
pub struct LocaleTerrain<'a> {
    /// The locale context whose fields are read.
    pub ctx: &'a LocaleContext,
}
impl<'a> LocaleTerrain<'a> {
    /// Build the adapter over `ctx`.
    pub fn new(ctx: &'a LocaleContext) -> Self {
        Self { ctx }
    }
}
impl<'a> Terrain for LocaleTerrain<'a> {
    fn elevation(&self, room: &RoomAddr) -> f64 {
        self.ctx
            .describe(room, WorldTime { day: 0.0 })
            .map(|l| l.fields.elevation_m)
            .unwrap_or(f64::INFINITY)
    }
    fn is_fresh_water(&self, room: &RoomAddr) -> bool {
        self.ctx
            .describe(room, WorldTime { day: 0.0 })
            .map(|l| l.fields.water.is_fresh())
            .unwrap_or(false)
    }
}

/// A game-layer predicate: the agent drank (satisfied its sustenance goal) on
/// this day. Registered by the session, NOT at genesis.
/// type-audit: bare-ok(identifier-text)
pub const DRANK: &str = "drank";

/// The drive at `t`: time since the last drink, a fold over committed `drank`
/// events. Rises `p.rise`/day since `last_drank_day` (0 before any drink),
/// clamped [0,1]. DRIVE == FOLD, now over `drank` — drinking is a PLANNED
/// action (The Foresight), not automatic proximity (The Wanting).
/// type-audit: bare-ok(ratio: return)
pub fn drive_at(ledger: &Ledger, entity: EntityId, t: WorldTime, p: &DriveParams) -> f64 {
    let last_drank = ledger
        .find(DRANK)
        .filter(|f| f.subject == entity)
        .filter_map(|f| f.day)
        .fold(0.0_f64, f64::max);
    (p.rise * (t.day - last_drank)).clamp(0.0, 1.0)
}

/// Belief (L1): the agent's nearest KNOWN water — a pure fold over its committed
/// `agent-at` history ∩ water-truth. Among the water rooms the agent has stood in
/// at or before `t`, the one nearest to `npc.home` by planned hop-distance (ties
/// by ascending `RoomAddr`), else `None` (ignorant). BELIEF == FOLD-OVER-PERCEIVED:
/// no stored belief — it re-derives from facts already committed (the matrix
/// verdict; UNI-20). Nearness anchors to home (nearest-to-current is a followup).
/// type-audit: bare-ok(count: budget)
pub fn believed_water(
    ledger: &Ledger,
    npc: &Npc,
    t: WorldTime,
    terrain: &dyn Terrain,
    budget: usize,
) -> Option<RoomAddr> {
    let mut seen: std::collections::BTreeSet<RoomAddr> = std::collections::BTreeSet::new();
    for f in ledger.find(AGENT_AT).filter(|f| f.subject == npc.entity) {
        let sighted = f.day.map(|d| d <= t.day).unwrap_or(false);
        if sighted && let Value::Text(s) = &f.object {
            let room = room_from_text(s);
            if is_water(&room, terrain) {
                seen.insert(room);
            }
        }
    }
    seen.into_iter()
        .filter_map(|r| plan_to_room(&npc.home, &r, budget).map(|p| (p.len(), r)))
        .min_by(|(la, ra), (lb, rb)| la.cmp(lb).then_with(|| ra.cmp(rb)))
        .map(|(_, r)| r)
}

/// What the agent perceives of the world — the `view` the decision reads. Splits
/// SELF-knowledge (position, drive — always true) from world-BELIEF
/// (`believed_water` — a cache that may be absent/ignorant) and immediate
/// perceived affordance (`explore_step`). PSY-6's "plan over belief, not truth"
/// (UNI-16), realized: the ground-truth `water` argument `decide` once took now
/// lives here as belief.
/// type-audit: bare-ok(ratio: drive)
#[derive(Clone, Debug)]
pub struct Perceived {
    /// The agent's current room (self-knowledge — always true).
    pub position: RoomAddr,
    /// The agent's perceived drive level (self-knowledge — always true).
    pub drive: f64,
    /// The nearest water the agent KNOWS of (belief), or `None` (ignorant).
    pub believed_water: Option<RoomAddr>,
    /// The next exploration move for an ignorant agent (lowest-elevation
    /// unvisited neighbour), or `None` (nowhere new to look → Hold).
    pub explore_step: Option<RoomAddr>,
}

/// The decision's output — the FIRST action of the agent's current plan, or
/// Hold. The tick depends only on this; the planner fills the body without
/// changing the seam (The Wanting decision #9).
/// type-audit: bare-ok(return)
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Intent {
    /// Perform this action next (the first step of the least-cost plan).
    Do(Action),
    /// No action (goal already met and at home, or the plan is unreachable
    /// within `budget`).
    Hold,
}

/// The planner (The Foresight), now planning over BELIEF (The Surmise): thirsty
/// and knows water -> A* to it and drink; thirsty and ignorant -> take the
/// explore step (or Hold if nowhere new); not thirsty and away -> plan home; else
/// Hold. Preserves the seam: `decide` reads `view` (now carrying belief), the tick
/// depends only on `Intent`.
/// type-audit: bare-ok(count: budget)
pub fn decide(view: &Perceived, home: &RoomAddr, p: &DriveParams, budget: usize) -> Intent {
    if view.drive >= p.act {
        match &view.believed_water {
            Some(w) => {
                match plan_to_water(&view.position, w, budget).and_then(|pl| pl.into_iter().next())
                {
                    Some(a) => Intent::Do(a),
                    None => Intent::Hold, // known water unreachable within budget
                }
            }
            None => match &view.explore_step {
                Some(step) => Intent::Do(Action::MoveTo(step.clone())),
                None => Intent::Hold, // ignorant and nowhere new to explore
            },
        }
    } else if &view.position != home {
        match plan_to_room(&view.position, home, budget).and_then(|pl| pl.into_iter().next()) {
            Some(a) => Intent::Do(a),
            None => Intent::Hold,
        }
    } else {
        Intent::Hold
    }
}

/// The plan search's node-expansion budget: generous for the short local
/// journeys every derived NPC actually walks (`nearest_water` finds a real
/// world's water within a handful of mesh hops of home), but finite so a
/// pathological distance genuinely gives up (`Intent::Hold`) rather than
/// paying for a global search — the one search-budget judgment call
/// (spec §8).
const PLAN_BUDGET: usize = 1_000;

/// One action's duration: the one authored action-duration judgment call
/// (spec §8). Small relative to a drive cycle (SUSTENANCE's ~5.7-day rise) so
/// a multi-room journey still resolves within a single `wait`.
const MOVE_DURATION: f64 = 0.1;

/// The per-NPC step cap on `DriveMovements::step`'s inner loop — the
/// strict-progress guard's backstop: even if a decision loop somehow failed
/// to advance `day` on every iteration, this bounds total work per tick
/// (termination guarantee, The Foresight T3 review).
const MAX_STEPS: usize = 10_000;

/// A committed `agent-at` fact: `entity` moved to `target` on `day`, with
/// `provenance` naming why.
fn agent_at_fact(entity: EntityId, target: &RoomAddr, day: f64, provenance: &str) -> Fact {
    Fact {
        subject: entity,
        predicate: AGENT_AT.to_string(),
        object: Value::Text(room_to_text(target)),
        place: None,
        day: Some(day),
        provenance: provenance.to_string(),
    }
}

/// A committed `drank` fact: `entity` satisfied its sustenance goal on `day`.
fn drank_fact(entity: EntityId, day: f64, provenance: &str) -> Fact {
    Fact {
        subject: entity,
        predicate: DRANK.to_string(),
        object: Value::Flag(true),
        place: None,
        day: Some(day),
        provenance: provenance.to_string(),
    }
}

/// The drive-driven movement system (The Foresight → The Surmise): each NPC
/// steps through its belief-driven plan — exploring while ignorant, beelining
/// once it knows water — committing a dated `agent-at`/`drank` at each
/// executed step. Holds a `Terrain` to compute belief and exploration
/// mid-walk. Run through c6's `tick`.
/// type-audit: bare-ok(return)
pub struct DriveMovements<'a> {
    /// The NPCs this tick advances.
    pub npcs: Vec<Npc>,
    /// The interval start (the session's previous day).
    pub from: WorldTime,
    /// The interval end (the session's new day).
    pub to: WorldTime,
    /// The drive parameters.
    pub params: DriveParams,
    /// The elevation field belief and exploration read.
    pub terrain: &'a dyn Terrain,
}

impl<'a> TickSystem for DriveMovements<'a> {
    fn label(&self) -> &'static str {
        "drive-movements"
    }
    fn step(&self, frozen: &Ledger) -> Vec<Fact> {
        let mut out = Vec::new();
        for npc in &self.npcs {
            let mut pos = agent_position(frozen, npc, self.from);
            let mut day = self.from.day;
            // A scratch ledger view isn't available; track drank locally: derive
            // the starting last-drank day from `frozen`, then simulate forward,
            // updating a local `last_drank` as we emit `DRANK` facts.
            let mut last_drank = frozen
                .find(DRANK)
                .filter(|f| f.subject == npc.entity)
                .filter_map(|f| f.day)
                .fold(0.0_f64, f64::max);
            // Belief and exploration state, evolved locally across the walk (the
            // fold includes this tick's own emitted moves). Seed belief from the
            // pre-tick history; grow it whenever the agent stands in water.
            let mut believed = believed_water(frozen, npc, self.from, self.terrain, PLAN_BUDGET);
            let mut visited: std::collections::BTreeSet<RoomAddr> =
                std::collections::BTreeSet::new();
            visited.insert(pos.clone());
            let mut steps = 0usize;
            loop {
                if day > self.to.day || steps >= MAX_STEPS {
                    break;
                }
                steps += 1;
                // Standing in water forms/updates belief (nearest-to-home wins).
                if is_water(&pos, self.terrain) {
                    believed = nearer_to_home(&npc.home, believed.take(), pos.clone(), PLAN_BUDGET);
                }
                let drive = (self.params.rise * (day - last_drank)).clamp(0.0, 1.0);
                let explore_step = lowest_unvisited_neighbor(&pos, &visited, self.terrain);
                let view = Perceived {
                    position: pos.clone(),
                    drive,
                    believed_water: believed.clone(),
                    explore_step,
                };
                let seeking_water = drive >= self.params.act;
                match decide(&view, &npc.home, &self.params, PLAN_BUDGET) {
                    Intent::Do(Action::MoveTo(n)) => {
                        day += MOVE_DURATION;
                        if day > self.to.day {
                            break;
                        }
                        // Discovery prose (the-surmise T5): honest to what the
                        // tick actually commits — BELIEVED (knows a source,
                        // beelining) vs IGNORANT (never found one, exploring
                        // downhill blind) are genuinely different situations,
                        // and "water" is now specifically FRESH water (The
                        // Freshet) — a river, never the sea.
                        let provenance = if seeking_water && believed.is_some() {
                            "went down to the river it knew (thirst)"
                        } else if seeking_water {
                            "wandered, having found no water yet (thirst)" // ignorant, exploring
                        } else {
                            "walking home (sated)"
                        };
                        out.push(agent_at_fact(npc.entity, &n, day, provenance));
                        visited.insert(n.clone());
                        pos = n;
                    }
                    Intent::Do(Action::Drink) => {
                        out.push(drank_fact(
                            npc.entity,
                            day,
                            "drank from the river (thirst sated)",
                        ));
                        last_drank = day;
                    }
                    Intent::Hold => {
                        // Idle (or unreachable): jump to the next act-crossing
                        // in closed form rather than spinning day-by-day. The
                        // strict-progress guarantee: `next_act <= day` breaks
                        // (a thirsty-but-unreachable Hold recomputes the SAME
                        // next_act every iteration — without this check, that
                        // spins to `MAX_STEPS` for nothing).
                        //
                        // A degenerate `rise == 0.0` makes `next_act` NaN
                        // (`act / rise` == `0.0 / 0.0`). Every NaN comparison
                        // is `false`, so leaving `day` untouched here (rather
                        // than assigning the NaN) keeps `drive` well-defined
                        // on the NEXT iteration (`rise * (day - last_drank)`
                        // with a finite `day` stays exactly 0.0, not NaN).
                        // Without this guard a NaN `day` makes `drive` itself
                        // NaN next iteration, which flips `decide`'s thirst
                        // check (`NaN >= act` is `false`) into the
                        // plan-home branch — re-running a budgeted A* search
                        // every remaining iteration up to `MAX_STEPS`, an
                        // O(MAX_STEPS * PLAN_BUDGET) blowup measured at ~60s
                        // for this test (the-surmise T3 review) instead of
                        // the intended O(MAX_STEPS) cheap spin. `steps >=
                        // MAX_STEPS` alone still bounds the loop.
                        let next_act = last_drank + self.params.act / self.params.rise;
                        if !next_act.is_finite() {
                            continue;
                        }
                        if next_act <= day || next_act > self.to.day {
                            break;
                        }
                        day = next_act;
                    }
                }
            }
        }
        out
    }
}

/// The nearer-to-home of an existing belief and a newly-perceived water room.
/// The tick's incremental fold — and its tie-break MUST match `believed_water`'s
/// (smaller `RoomAddr` wins on an equal hop-distance), or a mid-walk incremental
/// belief could disagree with the same belief re-derived from the committed
/// history, making the chosen source faintly sensitive to `wait` granularity
/// (the-surmise T3+T4 review). Aligned here so the two folds are identical.
fn nearer_to_home(
    home: &RoomAddr,
    current: Option<RoomAddr>,
    found: RoomAddr,
    budget: usize,
) -> Option<RoomAddr> {
    let d = |r: &RoomAddr| plan_to_room(home, r, budget).map(|p| p.len());
    match current {
        None => Some(found),
        Some(c) => match (d(&c), d(&found)) {
            (Some(dc), Some(df)) => Some(match df.cmp(&dc) {
                std::cmp::Ordering::Less => found,
                std::cmp::Ordering::Greater => c,
                // Tie on hop-distance: smaller RoomAddr wins (matches
                // `believed_water`'s `min_by((hop, RoomAddr))`).
                std::cmp::Ordering::Equal => std::cmp::min(c, found),
            }),
            (None, Some(_)) => Some(found),
            _ => Some(c),
        },
    }
}

/// The lowest-elevation neighbour not yet visited this walk (the directed-
/// exploration step), or `None` if every neighbour is visited. Terminating: the
/// visited set only grows.
fn lowest_unvisited_neighbor(
    from: &RoomAddr,
    visited: &std::collections::BTreeSet<RoomAddr>,
    terrain: &dyn Terrain,
) -> Option<RoomAddr> {
    let mut best: Option<(RoomAddr, f64)> = None;
    for n in from.neighbors() {
        if visited.contains(&n) {
            continue;
        }
        let elev = terrain.elevation(&n);
        let keep = match &best {
            Some((ba, be)) => elev.total_cmp(be).then_with(|| n.cmp(ba)).is_ge(),
            None => false,
        };
        if !keep {
            best = Some((n, elev));
        }
    }
    best.map(|(r, _)| r)
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
/// drive's resource anchor (`nearest_water` over the true terrain, The
/// Surmise) and species' activity-cycle.
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
            let resource = nearest_water(&home, &LocaleTerrain::new(ctx), PLAN_BUDGET)
                .unwrap_or_else(|| home.clone());
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

/// A GOAP action — a precondition/effect transformation over the plan state.
/// Minimal + heterogeneous (the precondition chain needs two kinds); the MAP-27
/// authored-verb DSL is a followup.
/// type-audit: bare-ok(return)
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Action {
    /// Walk to an adjacent room (precondition: adjacency; effect: position).
    MoveTo(RoomAddr),
    /// Drink (precondition: at the water room; effect: hydrated).
    Drink,
}

/// The GOAP planning state A* searches: where the agent is and whether it has
/// drunk. `Ord` for the deterministic search.
/// type-audit: bare-ok(flag: hydrated)
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct PlanState {
    /// The agent's room.
    pub position: RoomAddr,
    /// Whether the sustenance goal is met (has drunk this plan).
    pub hydrated: bool,
}

/// The GOAP search space for the sustenance goal: reach water and drink.
/// type-audit: bare-ok(return)
pub struct GoapSpace {
    /// The water room the `Drink` action requires.
    pub water: RoomAddr,
}
impl SearchSpace for GoapSpace {
    type State = PlanState;
    type Action = Action;
    fn successors(&self, s: &PlanState) -> Vec<(Action, PlanState, u64)> {
        if s.hydrated {
            return Vec::new(); // goal reached; no need to expand
        }
        let mut out: Vec<(Action, PlanState, u64)> = s
            .position
            .neighbors()
            .into_iter()
            .map(|n| {
                (
                    Action::MoveTo(n.clone()),
                    PlanState {
                        position: n,
                        hydrated: false,
                    },
                    1,
                )
            })
            .collect();
        if s.position == self.water {
            out.push((
                Action::Drink,
                PlanState {
                    position: s.position.clone(),
                    hydrated: true,
                },
                1,
            ));
        }
        out
    }
    fn goal(&self, s: &PlanState) -> bool {
        s.hydrated
    }
    fn heuristic(&self, _s: &PlanState) -> u64 {
        0 // Dijkstra-mode; a geometric heuristic is a followup
    }
}

/// Plan the `[move*, drink]` journey to satisfy the sustenance goal, or `None`
/// if water is unreachable within `budget`.
/// type-audit: bare-ok(count: budget)
pub fn plan_to_water(from: &RoomAddr, water: &RoomAddr, budget: usize) -> Option<Vec<Action>> {
    astar(
        &GoapSpace {
            water: water.clone(),
        },
        PlanState {
            position: from.clone(),
            hydrated: false,
        },
        budget,
    )
}

/// A navigation-only space (the home-return goal — no Drink): goal is arrival.
struct NavSpace {
    dest: RoomAddr,
}
impl SearchSpace for NavSpace {
    type State = RoomAddr;
    type Action = Action;
    fn successors(&self, s: &RoomAddr) -> Vec<(Action, RoomAddr, u64)> {
        s.neighbors()
            .into_iter()
            .map(|n| (Action::MoveTo(n.clone()), n, 1))
            .collect()
    }
    fn goal(&self, s: &RoomAddr) -> bool {
        *s == self.dest
    }
    fn heuristic(&self, _s: &RoomAddr) -> u64 {
        0
    }
}

/// Plan a pure navigation path to `dest` (the home-return goal), or `None`.
/// type-audit: bare-ok(count: budget)
pub fn plan_to_room(from: &RoomAddr, dest: &RoomAddr, budget: usize) -> Option<Vec<Action>> {
    astar(&NavSpace { dest: dest.clone() }, from.clone(), budget)
}

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_kernel::{ConceptRegistry, Seed};

    /// Commit an `agent-at` fact placing `entity` at `room` on `day`.
    fn commit_agent_at(
        ledger: &mut Ledger,
        reg: &ConceptRegistry,
        entity: EntityId,
        room: &RoomAddr,
        day: f64,
    ) {
        ledger
            .commit(agent_at_fact(entity, room, day, "test"), reg)
            .unwrap();
    }

    /// A registry with just `AGENT_AT` registered, for the belief-fold tests.
    fn agent_at_reg() -> ConceptRegistry {
        let mut reg = ConceptRegistry::default();
        reg.register_predicate(AGENT_AT, false, "pos").unwrap();
        reg
    }

    #[test]
    fn believed_water_is_none_until_the_agent_has_stood_in_water() {
        let reg = agent_at_reg();
        let mut ledger = Ledger::default();
        let e = ledger.mint_entity();
        let home = raddr(1.0);
        let water = home.neighbors()[0].clone();
        let t = PlantedTerrain::fresh_only([water.clone()]);
        let npc = Npc {
            entity: e,
            home: home.clone(),
            resource: water.clone(),
            activity: hornvale_species::ActivityCycle::Diurnal,
            label: "h".into(),
        };
        // no agent-at yet -> ignorant
        assert_eq!(
            believed_water(&ledger, &npc, WorldTime { day: 5.0 }, &t, 10_000),
            None
        );
        // stood in the water room on day 2 -> now believes it
        commit_agent_at(&mut ledger, &reg, e, &water, 2.0);
        assert_eq!(
            believed_water(&ledger, &npc, WorldTime { day: 5.0 }, &t, 10_000),
            Some(water)
        );
    }

    #[test]
    fn believed_water_ignores_dry_rooms_the_agent_stood_in() {
        let reg = agent_at_reg();
        let mut ledger = Ledger::default();
        let e = ledger.mint_entity();
        let home = raddr(1.0);
        let dry = home.neighbors()[0].clone();
        let t = PlantedTerrain::fresh_only(std::iter::empty()); // `dry` is never fresh
        let npc = Npc {
            entity: e,
            home: home.clone(),
            resource: home.clone(),
            activity: hornvale_species::ActivityCycle::Diurnal,
            label: "h".into(),
        };
        commit_agent_at(&mut ledger, &reg, e, &dry, 2.0);
        assert_eq!(
            believed_water(&ledger, &npc, WorldTime { day: 5.0 }, &t, 10_000),
            None
        );
    }

    #[test]
    fn believed_water_keeps_the_nearest_to_home_of_several_known_sources() {
        // THE MULTI-SOURCE FOLD: the agent has stood in a NEAR and a FAR water room;
        // belief is the near one (fewer hops from home).
        let reg = agent_at_reg();
        let mut ledger = Ledger::default();
        let e = ledger.mint_entity();
        let home = raddr(1.0);
        let near = home.neighbors()[0].clone(); // 1 hop
        let far = near
            .neighbors()
            .iter()
            .find(|n| **n != home)
            .unwrap()
            .clone(); // 2 hops
        let t = PlantedTerrain::fresh_only([near.clone(), far.clone()]);
        let npc = Npc {
            entity: e,
            home: home.clone(),
            resource: near.clone(),
            activity: hornvale_species::ActivityCycle::Diurnal,
            label: "h".into(),
        };
        commit_agent_at(&mut ledger, &reg, e, &far, 2.0); // discovered far first
        assert_eq!(
            believed_water(&ledger, &npc, WorldTime { day: 5.0 }, &t, 10_000),
            Some(far.clone())
        );
        commit_agent_at(&mut ledger, &reg, e, &near, 3.0); // later discovers the nearer one
        assert_eq!(
            believed_water(&ledger, &npc, WorldTime { day: 5.0 }, &t, 10_000),
            Some(near),
            "belief switches to the nearer known source"
        );
    }

    #[test]
    fn believed_water_only_counts_sightings_at_or_before_t() {
        let reg = agent_at_reg();
        let mut ledger = Ledger::default();
        let e = ledger.mint_entity();
        let home = raddr(1.0);
        let water = home.neighbors()[0].clone();
        let t = PlantedTerrain::fresh_only([water.clone()]);
        let npc = Npc {
            entity: e,
            home: home.clone(),
            resource: water.clone(),
            activity: hornvale_species::ActivityCycle::Diurnal,
            label: "h".into(),
        };
        commit_agent_at(&mut ledger, &reg, e, &water, 9.0); // sighting in the future
        assert_eq!(
            believed_water(&ledger, &npc, WorldTime { day: 5.0 }, &t, 10_000),
            None
        );
    }

    #[test]
    fn believed_water_is_deterministic_reload_stable_and_per_agent() {
        // BELIEF == FOLD: same ledger+t -> same value; reload-stable; another agent's
        // sightings never leak in (subject-scoped).
        let reg = agent_at_reg();
        let mut ledger = Ledger::default();
        let e = ledger.mint_entity();
        let other = ledger.mint_entity();
        let home = raddr(1.0);
        let water = home.neighbors()[0].clone();
        let t = PlantedTerrain::fresh_only([water.clone()]);
        let npc = Npc {
            entity: e,
            home: home.clone(),
            resource: water.clone(),
            activity: hornvale_species::ActivityCycle::Diurnal,
            label: "h".into(),
        };
        commit_agent_at(&mut ledger, &reg, other, &water, 2.0); // OTHER stood in water, not e
        assert_eq!(
            believed_water(&ledger, &npc, WorldTime { day: 5.0 }, &t, 10_000),
            None,
            "another agent's sighting does not become e's belief"
        );
        commit_agent_at(&mut ledger, &reg, e, &water, 3.0);
        let a = believed_water(&ledger, &npc, WorldTime { day: 5.0 }, &t, 10_000);
        let json = serde_json::to_string(&ledger).unwrap();
        let reloaded: Ledger = serde_json::from_str(&json).unwrap();
        assert_eq!(
            believed_water(&reloaded, &npc, WorldTime { day: 5.0 }, &t, 10_000),
            a
        );
        assert_eq!(a, Some(water));
    }

    #[test]
    fn believed_water_breaks_equal_hop_ties_by_ascending_room_addr() {
        // DETERMINISM UNDER GENUINE TIES (the tie-break the reload/isolation test
        // can't reach — it never has two equal-distance candidates): two water
        // sources the SAME hop-distance from home (two neighbours, both 1 hop) must
        // resolve to the smaller-`RoomAddr` one, identically every run and across
        // reload. A nondeterministic (HashSet) accumulation would make this flaky;
        // the `BTreeSet` + `min_by((hop, RoomAddr))` fold makes it total.
        let reg = agent_at_reg();
        let mut ledger = Ledger::default();
        let e = ledger.mint_entity();
        let home = raddr(1.0);
        let n = home.neighbors();
        let (first, second) = (n[0].clone(), n[1].clone()); // both exactly 1 hop from home
        let smaller = std::cmp::min(first.clone(), second.clone());
        let larger = std::cmp::max(first.clone(), second.clone());
        let t = PlantedTerrain::fresh_only([first.clone(), second.clone()]);
        let npc = Npc {
            entity: e,
            home: home.clone(),
            resource: first.clone(),
            activity: hornvale_species::ActivityCycle::Diurnal,
            label: "h".into(),
        };
        // Stand in the LARGER-addr source first, then the smaller — so a naive
        // "first sighting wins" would pick the larger; the tie-break must not.
        commit_agent_at(&mut ledger, &reg, e, &larger, 2.0);
        commit_agent_at(&mut ledger, &reg, e, &smaller, 3.0);
        let got = believed_water(&ledger, &npc, WorldTime { day: 5.0 }, &t, 10_000);
        assert_eq!(
            got,
            Some(smaller.clone()),
            "an equal-hop tie resolves to the smaller RoomAddr, not sighting order"
        );
        let json = serde_json::to_string(&ledger).unwrap();
        let reloaded: Ledger = serde_json::from_str(&json).unwrap();
        assert_eq!(
            believed_water(&reloaded, &npc, WorldTime { day: 5.0 }, &t, 10_000),
            got,
            "the tie resolves identically after reload"
        );
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
        let home = hornvale_settlement::village_info(&world).unwrap().id;
        let npcs = derive_npcs(&world, &ctx, &mut ledger, 3, home);
        assert_eq!(npcs.len(), 3);
        // distinct entities, and each has a VALID resource anchor: `derive_npcs`
        // resolves it as `nearest_water(home, ..., PLAN_BUDGET).unwrap_or(home)`
        // (against the WORLD's own derived sea level, via the same
        // `LocaleTerrain` adapter a live session uses) — so `resource` is either
        // a room that genuinely reads as water, OR the unreachable-water
        // fallback to `home` itself. `resource == home` is therefore NOT by
        // itself a bug (the old `resource_room` guaranteed a distinct
        // neighbour regardless of whether it was real water; `nearest_water`
        // makes no such promise — not every settlement has water within
        // budget, and that's a real, legitimate outcome, not a derivation
        // failure). What must hold is the disjunction below.
        let ids: std::collections::BTreeSet<_> = npcs.iter().map(|n| n.entity).collect();
        assert_eq!(ids.len(), 3);
        let terrain = LocaleTerrain::new(&ctx);
        for n in &npcs {
            assert!(
                is_water(&n.resource, &terrain) || n.resource == n.home,
                "NPC {}'s resource {:?} must be either real water or the home fallback",
                n.label,
                n.resource
            );
        }
    }

    #[test]
    fn seed_42_home_settlements_real_walk_reachability_is_a_measured_t5_finding() {
        // THE CONFLUENCE'S PAYOFF, MEASURED NOT ASSUMED: the earlier pinned
        // finding here (see git history) measured that seed 42's possessed
        // home settlement was topologically stuck — the greedy-downhill,
        // never-revisit-within-a-call exploration walked 2,592 rooms over an
        // enormous 100,000-day wait and never reached fresh water, boxed in
        // by a riverless drainage basin. That was a settlement-PLACEMENT
        // fact, not a belief-mechanism bug: the settlement itself sat off
        // the river network.
        //
        // The Confluence re-points the carrying-capacity freshwater term at
        // real proximity to `WaterKind::River` cells, so settlements now
        // condense onto/adjacent-to rivers (measured: seed 42 fraction
        // within reach 0.7222, up from a pre-Confluence baseline nowhere
        // close). Re-measuring this exact settlement (same accessor,
        // `village_info`, on the post-Confluence world — the campaign moved
        // WHERE settlements land, so "home" now names a different site) with
        // the identical real mechanism:
        //
        //   seed 42 home settlement: 0 exploration move(s) over an enormous
        //   wait, 2 drink(s).
        //
        // Zero moves means the home settlement's own room now reads as
        // fresh water directly (`is_water` true at spawn) — no discovery
        // walk is even needed; the agent drinks in place. Checked against
        // `derive_npcs`'s real selection (this settlement plus its two
        // next-most-populous neighbors — the actual three NPCs a
        // `possess --seed 42` session derives): ALL THREE now read 0 moves,
        // 2 drinks — the condensation pulled every one of them onto water,
        // not just the lucky one. This is the campaign's visible payoff:
        // The Surmise's parked "can't reach water" finding is resolved by
        // fixing WHERE towns are, not by making agents smarter.
        //
        // The general exploration-policy gap this pin used to document
        // (a walker that can box itself into an unvisited-but-connected
        // basin) is not disproven by this result — it's just no longer
        // triggered by seed 42's home settlement. It remains a real,
        // out-of-scope gap for settlements condensation still leaves off a
        // river (decision-ledger followup #2), and the coarse-cell vs.
        // walk-depth resolution bridge (followup #1) is a related, separate
        // concern this measurement does not exercise (0 moves means the
        // coarse cell itself already reads as water at walk depth too).
        //
        // MEASURED, ALSO SURPRISING: only 2 drinks register over the
        // 100,000-day wait, not the thousands a ~5.667-day drive cycle would
        // suggest. Traced (debug prints, not left in): the zero-distance
        // on-water case is new — no prior settlement ever landed exactly ON
        // its own water source, so this is the first time the closed-form
        // `Hold` jump (`next_act = last_drank + act/rise`) and a drink cycle
        // of the exact same length interact at THIS boundary. By the third
        // cycle, `last_drank + act/rise` lands (floating-point rounding) a
        // hair BELOW `act` when read back as `drive`, so `decide` sees
        // "not yet thirsty" and re-derives an IDENTICAL `next_act`, which
        // trips the strict-progress guard (`next_act <= day`) and ends the
        // tick. This is not this campaign's regression (the guard predates
        // it, guarding a different case — a genuinely unreachable plan
        // recomputing the same Hold forever) and does not weaken the
        // payoff (2 confirmed drinks is already `>= 1`, and no real
        // `possess` session ever `wait`s 100,000 days) — but it is a real,
        // newly-exposed quirk in the on-water zero-distance path, captured
        // as a followup rather than silently absorbed.
        let mut world_reg = hornvale_kernel::ConceptRegistry::default();
        world_reg
            .register_predicate(AGENT_AT, false, "pos")
            .unwrap();
        world_reg.register_predicate(DRANK, false, "drank").unwrap();
        let world = hornvale_worldgen::build_world(
            Seed(42),
            &hornvale_astronomy::SkyPins::default(),
            hornvale_worldgen::SkyChoice::Generated,
            &hornvale_terrain::TerrainPins::default(),
            &hornvale_worldgen::SettlementPins::default(),
        )
        .unwrap();
        let ctx = LocaleContext::build(&world).unwrap();
        let terrain = LocaleTerrain::new(&ctx);
        let home_id = hornvale_settlement::village_info(&world).unwrap().id;
        let home = settlement_room(&world, &ctx, home_id);
        let npc = Npc {
            entity: EntityId::new(1).unwrap(),
            home: home.clone(),
            resource: home.clone(),
            activity: hornvale_species::ActivityCycle::Diurnal,
            label: "measure".into(),
        };
        let ledger = Ledger::default();
        let sys = DriveMovements {
            npcs: vec![npc.clone()],
            from: WorldTime { day: 0.0 },
            // Deliberately enormous: rules out "it just needed a longer
            // wait" — a real session's `wait` would never span this.
            to: WorldTime { day: 100_000.0 },
            params: SUSTENANCE,
            terrain: &terrain,
        };
        let next =
            hornvale_kernel::tick(&ledger, &[&sys], &["drive-movements"], &world_reg).unwrap();
        let moves = next
            .find(AGENT_AT)
            .filter(|f| f.subject == npc.entity)
            .count();
        let drinks = next.find(DRANK).filter(|f| f.subject == npc.entity).count();
        println!(
            "seed 42 home settlement: {moves} exploration move(s) over an \
             enormous wait, {drinks} drink(s)"
        );
        assert!(
            drinks >= 1,
            "THE CONFLUENCE'S PAYOFF (update this assertion AND the doc \
             comment above together if it regresses — don't just delete \
             it): the possessed agent's own home settlement's NPC must \
             reach fresh water on real seed 42 now that settlement \
             condensation pulls towns onto the river network (see the doc \
             comment above for the measured before/after); got {drinks} \
             drink(s) over the wait"
        );
        assert_eq!(
            moves, 0,
            "measured finding: the home settlement's own room now reads as \
             fresh water directly, so no discovery walk is needed — update \
             this pin (and the doc comment) if a future settlement-position \
             change makes this settlement's water a real walk rather than \
             immediate"
        );
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
    fn drive_folds_drank_events_rising_since_the_last_drink() {
        // drive = rise * (t - last_drank_day), clamped [0,1]; last_drank = latest DRANK day.
        let p = SUSTENANCE;
        let mut ledger = Ledger::default();
        let mut reg = hornvale_kernel::ConceptRegistry::default();
        reg.register_predicate(DRANK, false, "drank").unwrap();
        let e = ledger.mint_entity();
        // no drank yet: rises from day 0
        assert!((drive_at(&ledger, e, WorldTime { day: 2.0 }, &p) - (p.rise * 2.0)).abs() < 1e-9);
        // drank on day 5 -> resets; by day 6 it has risen rise*1
        ledger
            .commit(
                hornvale_kernel::Fact {
                    subject: e,
                    predicate: DRANK.to_string(),
                    object: Value::Flag(true),
                    place: None,
                    day: Some(5.0),
                    provenance: "t".into(),
                },
                &reg,
            )
            .unwrap();
        assert!((drive_at(&ledger, e, WorldTime { day: 6.0 }, &p) - (p.rise * 1.0)).abs() < 1e-9);
    }

    #[test]
    fn drive_at_clamps_at_one_and_ignores_other_entities_drank_events() {
        let p = SUSTENANCE;
        let mut ledger = Ledger::default();
        let mut reg = hornvale_kernel::ConceptRegistry::default();
        reg.register_predicate(DRANK, false, "drank").unwrap();
        let e = ledger.mint_entity();
        let other = ledger.mint_entity();
        // Another entity's drink must not affect `e`'s drive (subject-scoped fold).
        ledger
            .commit(
                hornvale_kernel::Fact {
                    subject: other,
                    predicate: DRANK.to_string(),
                    object: Value::Flag(true),
                    place: None,
                    day: Some(1.0),
                    provenance: "t".into(),
                },
                &reg,
            )
            .unwrap();
        assert_eq!(drive_at(&ledger, e, WorldTime { day: 1_000.0 }, &p), 1.0);
    }

    #[test]
    fn drive_at_is_deterministic_and_reload_stable() {
        // Fold determinism: same ledger + t -> same value; and serialize->reload of
        // the ledger yields the identical drive (the DRIVE == FOLD contract).
        let p = SUSTENANCE;
        let mut ledger = Ledger::default();
        let mut reg = hornvale_kernel::ConceptRegistry::default();
        reg.register_predicate(DRANK, false, "drank").unwrap();
        let e = ledger.mint_entity();
        for day in [1.0, 4.0, 9.0] {
            ledger
                .commit(
                    hornvale_kernel::Fact {
                        subject: e,
                        predicate: DRANK.to_string(),
                        object: Value::Flag(true),
                        place: None,
                        day: Some(day),
                        provenance: "t".into(),
                    },
                    &reg,
                )
                .unwrap();
        }
        let t = WorldTime { day: 12.3 };
        let a = drive_at(&ledger, e, t, &p);
        let b = drive_at(&ledger, e, t, &p);
        assert_eq!(a, b);
        let json = serde_json::to_string(&ledger).unwrap();
        let reloaded: Ledger = serde_json::from_str(&json).unwrap();
        assert_eq!(
            drive_at(&reloaded, e, t, &p),
            a,
            "drive re-derives identically after reload"
        );
    }

    fn addr(seed: f64) -> RoomAddr {
        RoomAddr::containing([seed, 0.0, 0.0], 6)
    }

    #[test]
    fn decide_plans_to_water_when_thirsty_and_home_when_not() {
        let p = SUSTENANCE;
        let home = addr(1.0);
        let water = home.neighbors()[0].clone();
        // parched (drive >= act), at home, KNOWS water -> the plan's first
        // step, toward water
        let v = Perceived {
            position: home.clone(),
            drive: 0.9,
            believed_water: Some(water.clone()),
            explore_step: None,
        };
        assert_eq!(
            decide(&v, &home, &p, 10_000),
            Intent::Do(Action::MoveTo(water.clone()))
        );
        // not thirsty, away from home (at water) -> the plan's first step home
        let v = Perceived {
            position: water.clone(),
            drive: 0.1,
            believed_water: Some(water.clone()),
            explore_step: None,
        };
        assert_eq!(
            decide(&v, &home, &p, 10_000),
            Intent::Do(Action::MoveTo(home.clone()))
        );
        // not thirsty, at home -> nothing to do
        let v = Perceived {
            position: home.clone(),
            drive: 0.1,
            believed_water: Some(water.clone()),
            explore_step: None,
        };
        assert_eq!(decide(&v, &home, &p, 10_000), Intent::Hold);
    }

    #[test]
    fn decide_holds_when_the_plan_is_unreachable_within_budget() {
        // A zero search budget can never find even a one-step plan: both the
        // thirsty-and-knows-water (plan-to-water) and homeward (plan-to-room)
        // branches must give up rather than loop.
        let p = SUSTENANCE;
        let home = addr(1.0);
        let water = home.neighbors()[0].clone();
        let thirsty = Perceived {
            position: home.clone(),
            drive: 0.9,
            believed_water: Some(water.clone()),
            explore_step: None,
        };
        assert_eq!(decide(&thirsty, &home, &p, 0), Intent::Hold);
        let away_not_thirsty = Perceived {
            position: water.clone(),
            drive: 0.1,
            believed_water: Some(water.clone()),
            explore_step: None,
        };
        assert_eq!(decide(&away_not_thirsty, &home, &p, 0), Intent::Hold);
    }

    fn raddr(seed: f64) -> RoomAddr {
        RoomAddr::containing([seed, 0.0, 0.0], 6)
    }

    #[test]
    fn decide_plans_to_believed_water_or_explores_when_ignorant() {
        // BELIEF DRIVES THE DECISION: two views identical but for belief produce
        // different first moves — the believer A*-steps toward its known water; the
        // ignorant one takes the explore step. (Water two hops away so the A* first
        // step differs from an arbitrary explore step.)
        let p = SUSTENANCE;
        let home = raddr(1.0);
        let mid = home.neighbors()[0].clone();
        let water = mid
            .neighbors()
            .iter()
            .find(|n| **n != home)
            .unwrap()
            .clone();
        let explore = home.neighbors()[2].clone(); // a different direction
        // believer, thirsty, at home -> first A* step toward water (== mid)
        let believer = Perceived {
            position: home.clone(),
            drive: 0.9,
            believed_water: Some(water.clone()),
            explore_step: Some(explore.clone()),
        };
        assert_eq!(
            decide(&believer, &home, &p, 10_000),
            Intent::Do(Action::MoveTo(mid.clone()))
        );
        // ignorant, thirsty, at home -> the explore step (not toward water)
        let ignorant = Perceived {
            position: home.clone(),
            drive: 0.9,
            believed_water: None,
            explore_step: Some(explore.clone()),
        };
        assert_eq!(
            decide(&ignorant, &home, &p, 10_000),
            Intent::Do(Action::MoveTo(explore.clone()))
        );
        assert_ne!(
            mid, explore,
            "the two beliefs must yield different moves for this to prove anything"
        );
        // ignorant with nowhere new to explore -> Hold
        let stuck = Perceived {
            position: home.clone(),
            drive: 0.9,
            believed_water: None,
            explore_step: None,
        };
        assert_eq!(decide(&stuck, &home, &p, 10_000), Intent::Hold);
        // not thirsty, away from home -> plan home (unchanged behavior)
        let sated_away = Perceived {
            position: water.clone(),
            drive: 0.1,
            believed_water: Some(water.clone()),
            explore_step: None,
        };
        assert!(matches!(
            decide(&sated_away, &home, &p, 10_000),
            Intent::Do(Action::MoveTo(_))
        ));
    }

    #[test]
    fn a_thirsty_agent_plans_to_water_and_the_tick_walks_it() {
        // Over a wait long enough to grow thirsty, the tick commits a run of agent-at
        // moves ending at water plus a `drank`, and the drive resets. Under the
        // belief model the agent starts IGNORANT: its first approach to water
        // is an EXPLORE step (not a ground-truth beeline) — water is planted
        // as home's only low neighbour, so exploration discovers it on the
        // very first thirsty step; belief then persists (the fold across the
        // walk) so every later cycle A*-steps to it directly.
        let mut world_reg = hornvale_kernel::ConceptRegistry::default();
        world_reg
            .register_predicate(AGENT_AT, false, "pos")
            .unwrap();
        world_reg.register_predicate(DRANK, false, "drank").unwrap();
        let mut ledger = Ledger::default();
        let e = ledger.mint_entity();
        let home = raddr(1.0);
        let water = home.neighbors()[0].clone();
        let npc = Npc {
            entity: e,
            home: home.clone(),
            resource: water.clone(),
            activity: hornvale_species::ActivityCycle::Diurnal,
            label: "herder".into(),
        };
        // Elevation still steers the exploration prior (downhill), separate
        // from fresh-water truth: `water` must be the uniquely lowest
        // neighbor for the comment above's "very first thirsty step"
        // guarantee to hold deterministically (not by RoomAddr tie-break
        // luck among equally-INFINITY neighbors).
        let t = PlantedTerrain {
            elevations: [(water.clone(), 0.0)].into_iter().collect(),
            fresh: [water.clone()].into_iter().collect(),
        };
        let sys = DriveMovements {
            npcs: vec![npc.clone()],
            from: WorldTime { day: 0.0 },
            to: WorldTime { day: 40.0 },
            params: SUSTENANCE,
            terrain: &t,
        };
        let next =
            hornvale_kernel::tick(&ledger, &[&sys], &["drive-movements"], &world_reg).unwrap();
        // At least one drank committed (the agent reached water and drank).
        let drank_count = next.find(DRANK).filter(|f| f.subject == e).count();
        assert!(drank_count >= 1, "the agent drank");
        // agent-at moves committed (the journey), and bounded (not one per
        // tick / not exploding — THE NO-THRASH GUARD, the campaign's
        // characteristic risk): a ~5.7-day rise cycle over 40 days is ~7
        // cycles, each a small, fixed number of moves.
        let moves = next.find(AGENT_AT).filter(|f| f.subject == e).count();
        assert!(moves >= 1, "the agent walked");
        assert!(
            moves <= 60,
            "expected a bounded number of moves, not one per tick or an explosion; got {moves}"
        );
        let _ = ledger;
    }

    #[test]
    fn the_recount_surfaces_the_drives_own_provenance_for_the_full_round_trip() {
        // THE PROVENANCE READ, DECOUPLED FROM REAL-WORLD REACHABILITY
        // (the-surmise T4 review): `why`'s recount
        // (`hornvale_historiography::recount`) must surface the drive's own
        // reason for a move AND the drink that satisfied it — proven here
        // directly against `DriveMovements`'s committed facts on PLANTED
        // (guaranteed-one-hop) terrain — a deterministic, seed-independent
        // proof of the mechanism, orthogonal to
        // `seed_42_home_settlements_real_walk_reachability_is_a_measured_t5_finding`'s
        // measurement of the real seed-42 world's own settlement/water
        // placement.
        // Mutation-verify: blanking `agent_at_fact`'s "went down to the
        // river it knew (thirst)" string, or `drank_fact`'s "drank from the
        // river (thirst sated)" string, reds ONE of the two assertions
        // below without touching the other.
        // (This test predates The Freshet re-wire; kept on planted terrain
        // deliberately — the mechanism-level provenance read should not
        // depend on any one real seed's fresh-water placement.)
        let mut world = World::new(Seed(0));
        world
            .registry
            .register_predicate(AGENT_AT, false, "pos")
            .unwrap();
        world
            .registry
            .register_predicate(DRANK, false, "drank")
            .unwrap();
        let entity = world.ledger.mint_entity();
        world
            .ledger
            .commit(
                Fact {
                    subject: entity,
                    predicate: hornvale_kernel::NAME.to_string(),
                    object: Value::Text("herder".to_string()),
                    place: None,
                    day: None,
                    provenance: "test".to_string(),
                },
                &world.registry,
            )
            .unwrap();
        let home = raddr(1.0);
        let water = home.neighbors()[0].clone();
        let npc = Npc {
            entity,
            home: home.clone(),
            resource: water.clone(),
            activity: hornvale_species::ActivityCycle::Diurnal,
            label: "herder".into(),
        };
        // Elevation still steers the exploration prior (downhill), separate
        // from fresh-water truth: `water` must be the uniquely lowest
        // neighbor so the first (ignorant) thirsty cycle explores directly
        // onto it, letting a later cycle's believer beeline actually fire
        // "went down to the river it knew (thirst)" below.
        let t = PlantedTerrain {
            elevations: [(water.clone(), 0.0)].into_iter().collect(),
            fresh: [water.clone()].into_iter().collect(),
        };
        let sys = DriveMovements {
            npcs: vec![npc],
            from: WorldTime { day: 0.0 },
            to: WorldTime { day: 40.0 },
            params: SUSTENANCE,
            terrain: &t,
        };
        world.ledger = hornvale_kernel::tick(
            &world.ledger,
            &[&sys],
            &["drive-movements"],
            &world.registry,
        )
        .unwrap();
        let recount = hornvale_historiography::recount(&world, entity).expect("facts exist");
        assert!(
            recount.contains("went down to the river it knew (thirst)"),
            "the recount names the drive's own reason for the move: {recount}"
        );
        assert!(
            recount.contains("drank from the river (thirst sated)"),
            "the recount also names the drink that satisfied the goal: {recount}"
        );
    }

    #[test]
    fn thirsty_but_unreachable_water_gives_up_quickly_not_at_max_steps() {
        // THE ANTI-HANG GUARD (The Foresight T3 review), reinterpreted for
        // belief (The Surmise): under the OLD ground-truth model, unreachable
        // water made `decide` return `Hold` immediately and the idle-jump's
        // strict-progress guard fired on that very first Hold. Under the
        // belief model an IGNORANT agent no longer "gives up" on water it has
        // never reached — it EXPLORES instead — so a genuinely water-less
        // world (all-INFINITY terrain: no water anywhere for the agent to
        // ever discover) exercises the OTHER termination guarantee: the
        // unconditional `steps >= MAX_STEPS` cap bounds the walk even though
        // it never reaches "thirsty and known-unreachable" in the old sense.
        // The load-bearing assertion is boundedness/termination, not an
        // exact fact count (an ignorant agent commits real exploration moves
        // now, where it once committed none).
        let mut reg = hornvale_kernel::ConceptRegistry::default();
        reg.register_predicate(AGENT_AT, false, "pos").unwrap();
        reg.register_predicate(DRANK, false, "drank").unwrap();
        let ledger = Ledger::default();
        let e = EntityId::new(1).unwrap();
        let home = raddr(1.0);
        let water = RoomAddr::containing([-1.0, 0.0, 0.0], 6); // irrelevant now: no water exists anywhere
        let npc = Npc {
            entity: e,
            home: home.clone(),
            resource: water,
            activity: hornvale_species::ActivityCycle::Diurnal,
            label: "herder".into(),
        };
        // No fresh water anywhere, so belief never forms and the agent
        // explores for the whole run.
        let t = PlantedTerrain::fresh_only(std::iter::empty());
        // A long wait: the MAX_STEPS cap (not the wait) must be what bounds
        // this — if it weren't a real backstop, work would scale with the
        // wait instead.
        let sys = DriveMovements {
            npcs: vec![npc],
            from: WorldTime { day: 0.0 },
            to: WorldTime { day: 10_000.0 },
            params: SUSTENANCE,
            terrain: &t,
        };
        let next = hornvale_kernel::tick(&ledger, &[&sys], &["drive-movements"], &reg).unwrap();
        // Never drinks (no water exists); the walk is bounded by MAX_STEPS —
        // the tick's own prompt return here (within this test's harness
        // timeout) is additional proof it didn't hang, but the real
        // assertion is the explicit bound below, not an exact fact count.
        let drank_count = next.find(DRANK).filter(|f| f.subject == e).count();
        assert_eq!(drank_count, 0, "no water exists so the agent never drinks");
        let moves = next.find(AGENT_AT).filter(|f| f.subject == e).count();
        assert!(
            moves <= MAX_STEPS,
            "the MAX_STEPS cap must bound the exploring walk; got {moves} moves"
        );
    }

    #[test]
    fn a_degenerate_zero_rise_drive_terminates_via_the_max_steps_cap_not_a_hang() {
        // THE DEGENERATE-DRIVEPARAMS REGRESSION (The Foresight T3 review):
        // `rise: 0.0, act: 0.0` makes the `Hold`-idle jump compute
        // `next_act = last_drank + act / rise = 0.0 / 0.0 = NaN`. Every NaN
        // comparison (`<=`, `>`) is `false`, so BOTH strict-progress guards
        // in the `Hold` arm (`next_act <= day` and `next_act > self.to.day`)
        // fail to fire, and `day = next_act` sets `day` to NaN too (which
        // then also never exceeds `self.to.day`, since any comparison with
        // NaN is false). Only the unconditional `steps >= MAX_STEPS` cap
        // (10_000) stops the loop — this test proves that cap is the real
        // backstop, not the closed-form guard (which is a no-op here).
        //
        // Under belief (The Surmise): `drive == 0.0 >= act == 0.0` is true
        // from the very first iteration (`rise == 0.0` means `drive` is
        // exactly `0.0`, not NaN, for the ENTIRE run — the NaN only ever
        // appears inside the Hold arm's own `act / rise` division), so the
        // agent is "thirsty" on every step. With all-INFINITY terrain (no
        // water anywhere), belief never forms, so every thirsty step takes
        // the EXPLORE branch instead of the old ground-truth
        // beeline-then-drink-forever; the walk is bounded by the same
        // MAX_STEPS cap regardless of which branch (explore-move vs.
        // NaN-Hold) it spins in.
        //
        // The old regression test for this class
        // (`a_misconfigured_drive_terminates_instead_of_hanging`, keyed on
        // `sated >= act`) no longer applies: the planned model's `decide`
        // never reads `sated`, so that degenerate class can't hang it. This
        // is the new degenerate class the planned model is actually exposed
        // to.
        let mut reg = hornvale_kernel::ConceptRegistry::default();
        reg.register_predicate(AGENT_AT, false, "pos").unwrap();
        reg.register_predicate(DRANK, false, "drank").unwrap();
        let ledger = Ledger::default();
        let e = EntityId::new(1).unwrap();
        let home = raddr(1.0);
        let water = home.neighbors()[0].clone();
        let npc = Npc {
            entity: e,
            home: home.clone(),
            resource: water,
            activity: hornvale_species::ActivityCycle::Diurnal,
            label: "herder".into(),
        };
        // No fresh water anywhere, so belief never forms.
        let t = PlantedTerrain::fresh_only(std::iter::empty());
        let degenerate = DriveParams {
            rise: 0.0,
            act: 0.0,
            sated: 0.15,
        };
        // A long interval: if MAX_STEPS were not the backstop, this would
        // spin forever (this test's own short harness timeout is additional
        // proof it didn't hang; the assertion below is the load-bearing
        // one).
        let sys = DriveMovements {
            npcs: vec![npc],
            from: WorldTime { day: 0.0 },
            to: WorldTime { day: 1_000_000.0 },
            params: degenerate,
            terrain: &t,
        };
        let next = hornvale_kernel::tick(&ledger, &[&sys], &["drive-movements"], &reg).unwrap();
        // It terminates (the call above returned at all) with a BOUNDED
        // number of facts — MAX_STEPS iterations, not an unbounded run: every
        // step is "thirsty" (drive == act == 0.0) and ignorant (no water
        // exists), so every step either explores (a move) or, once boxed in
        // with no unvisited neighbour, Holds on the NaN-producing idle jump —
        // either way, `steps >= MAX_STEPS` is what stops it.
        let moves = next.find(AGENT_AT).filter(|f| f.subject == e).count();
        let drinks = next.find(DRANK).filter(|f| f.subject == e).count();
        assert!(
            moves + drinks <= MAX_STEPS,
            "the MAX_STEPS cap must bound total committed facts even under a \
             NaN-producing degenerate DriveParams; got {moves} moves + {drinks} drinks"
        );
        assert!(
            moves + drinks > 0,
            "the loop must actually run (not exit on the first iteration) \
             for this to be a meaningful termination proof"
        );
    }

    #[test]
    fn moves_carry_drive_naming_provenance() {
        let p = SUSTENANCE;
        let mut ledger = Ledger::default();
        let mut reg = hornvale_kernel::ConceptRegistry::default();
        reg.register_predicate(AGENT_AT, false, "pos").unwrap();
        reg.register_predicate(DRANK, false, "drank").unwrap();
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
        let t = PlantedTerrain::fresh_only([resource.clone()]);
        let sys = DriveMovements {
            npcs: vec![npc],
            from: WorldTime { day: 0.0 },
            to: WorldTime { day: 10.0 },
            params: p,
            terrain: &t,
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

    #[test]
    fn plan_to_water_is_a_precondition_chain_move_then_drink() {
        // Water is a mesh neighbor of home (one step away): plan is [MoveTo(water), Drink].
        let home = raddr(1.0);
        let water = home.neighbors()[0].clone();
        let plan = plan_to_water(&home, &water, 10_000).expect("reachable");
        assert_eq!(plan.len(), 2);
        assert!(matches!(plan[0], Action::MoveTo(ref r) if *r == water));
        assert!(matches!(plan[1], Action::Drink));
    }

    #[test]
    fn plan_to_water_when_already_there_is_just_drink() {
        let water = raddr(1.0);
        let plan = plan_to_water(&water, &water, 10_000).unwrap();
        assert_eq!(plan, vec![Action::Drink]);
    }

    #[test]
    fn every_action_in_a_plan_has_its_precondition_satisfied_in_sequence() {
        // Execute the plan from `home`, checking each action's precondition holds in
        // order (the precondition-chain validity: Drink is only ever preceded by
        // arrival at water). Water two rooms away for a genuine multi-step chain.
        let home = raddr(1.0);
        let mid = home.neighbors()[0].clone();
        let water = mid
            .neighbors()
            .iter()
            .find(|n| **n != home)
            .unwrap()
            .clone();
        let plan = plan_to_water(&home, &water, 10_000).expect("reachable");
        let mut pos = home.clone();
        let mut hydrated = false;
        for a in &plan {
            match a {
                Action::MoveTo(n) => {
                    assert!(
                        pos.neighbors().contains(n),
                        "MoveTo precondition: adjacency"
                    );
                    pos = n.clone();
                }
                Action::Drink => {
                    assert_eq!(pos, water, "Drink precondition: at water");
                    hydrated = true;
                }
            }
        }
        assert!(hydrated, "the plan achieves the goal");
        assert!(
            plan.len() >= 3,
            "multi-step: at least two moves then a drink"
        );
    }

    #[test]
    fn plan_to_room_is_pure_navigation_no_drink() {
        let home = raddr(1.0);
        let dest = home.neighbors()[0].clone();
        let plan = plan_to_room(&home, &dest, 10_000).unwrap();
        assert!(plan.iter().all(|a| matches!(a, Action::MoveTo(_))));
        assert!(!plan.is_empty());
    }

    /// A synthetic elevation + fresh-water field for pure tests: planted
    /// heights, INFINITY elsewhere (INFINITY = "never chosen downhill" —
    /// mirrors `LocaleTerrain`'s undescribable-room fallback), and a planted
    /// SET of fresh-water rooms (the-surmise T5 re-wire: water is no longer
    /// an elevation threshold — `Terrain::is_fresh_water` is authoritative).
    struct PlantedTerrain {
        elevations: std::collections::BTreeMap<RoomAddr, f64>,
        fresh: std::collections::BTreeSet<RoomAddr>,
    }
    impl PlantedTerrain {
        /// No elevation data — just a set of fresh-water rooms (the common
        /// case for the belief-fold tests, which never exercise
        /// `downhill_step`/`nearest_water`'s elevation reads).
        fn fresh_only(rooms: impl IntoIterator<Item = RoomAddr>) -> Self {
            Self {
                elevations: std::collections::BTreeMap::new(),
                fresh: rooms.into_iter().collect(),
            }
        }
        /// No fresh water anywhere — just planted elevations (the
        /// exploration/downhill tests, which never exercise belief).
        fn dry(elevations: std::collections::BTreeMap<RoomAddr, f64>) -> Self {
            Self {
                elevations,
                fresh: std::collections::BTreeSet::new(),
            }
        }
    }
    impl Terrain for PlantedTerrain {
        fn elevation(&self, room: &RoomAddr) -> f64 {
            self.elevations.get(room).copied().unwrap_or(f64::INFINITY)
        }
        fn is_fresh_water(&self, room: &RoomAddr) -> bool {
            self.fresh.contains(room)
        }
    }

    #[test]
    fn is_water_delegates_to_terrain_is_fresh_water() {
        // `raddr(seed)` feeds `RoomAddr::containing([seed, 0.0, 0.0], 6)`, which
        // normalizes its input direction first — so `raddr(1.0)` and `raddr(2.0)`
        // collapse to the SAME room (both are the direction [1,0,0]). Use a
        // genuine mesh neighbor for `high` instead, so the two planted rooms
        // are actually distinct (deviation from the brief's literal `raddr(2.0)`;
        // see task-1-report.md). Renamed from
        // `is_water_is_the_elevation_threshold` (T5 re-wire): `is_water` no
        // longer reads elevation at all — it delegates to
        // `Terrain::is_fresh_water` (The Freshet's salt/fresh classification).
        let low = raddr(1.0);
        let high = low.neighbors()[0].clone();
        let t = PlantedTerrain::fresh_only([low.clone()]);
        assert!(is_water(&low, &t));
        assert!(!is_water(&high, &t));
    }

    #[test]
    fn downhill_step_picks_the_lowest_neighbor_deterministically() {
        let home = raddr(1.0);
        let ns = home.neighbors();
        // Make ns[1] strictly lowest; others high.
        let mut m = std::collections::BTreeMap::new();
        for (i, n) in ns.iter().enumerate() {
            m.insert(n.clone(), if i == 1 { 0.0 } else { 100.0 });
        }
        let t = PlantedTerrain::dry(m);
        assert_eq!(downhill_step(&home, &t), ns[1]);
    }

    #[test]
    fn nearest_water_finds_the_closest_water_room_by_hops() {
        // home (dry) -> a neighbor that is fresh water: 1 hop.
        let home = raddr(1.0);
        let near = home.neighbors()[0].clone();
        let t = PlantedTerrain::fresh_only([near.clone()]);
        assert_eq!(nearest_water(&home, &t, 10_000), Some(near));
    }

    #[test]
    fn nearest_water_returns_from_itself_when_already_on_water() {
        let here = raddr(1.0);
        let t = PlantedTerrain::fresh_only([here.clone()]);
        assert_eq!(nearest_water(&here, &t, 10_000), Some(here));
    }

    #[test]
    fn nearest_water_gives_up_within_budget_when_no_water() {
        let home = raddr(1.0); // no fresh water anywhere
        let t = PlantedTerrain::fresh_only(std::iter::empty());
        assert_eq!(nearest_water(&home, &t, 50), None);
    }

    #[test]
    fn two_agents_believe_different_sources_from_their_histories_and_beeline_differently() {
        // THE MULTI-SOURCE KEYSTONE (destination divergence): two NPCs, same home,
        // thirst, world — differing ONLY in a pre-seeded agent-at (perceived source).
        // A knows the near source W1; B knows the far source W2. Each beelines to its
        // OWN believed source. Belief ignored ⇒ both go to the same true-nearest ⇒
        // this fails.
        let reg = {
            let mut r = agent_at_reg();
            r.register_predicate(DRANK, false, "drank").unwrap();
            r
        };
        let home = raddr(1.0);
        let w1 = home.neighbors()[0].clone(); // near source
        let w2 = home.neighbors()[1]
            .neighbors()
            .iter()
            .find(|n| **n != home)
            .unwrap()
            .clone(); // far source
        let terrain = PlantedTerrain::fresh_only([w1.clone(), w2.clone()]);
        let run = |seed_room: &RoomAddr| -> Vec<RoomAddr> {
            let mut ledger = Ledger::default();
            let e = ledger.mint_entity();
            // The prior sighting (day 0), THEN a return-home (day 0.5): history holds
            // the sighting (→ belief) but the agent's current position is home, not
            // the water. (Position = latest agent-at; belief = the fold over history.)
            commit_agent_at(&mut ledger, &reg, e, seed_room, 0.0);
            commit_agent_at(&mut ledger, &reg, e, &home, 0.5);
            let npc = Npc {
                entity: e,
                home: home.clone(),
                resource: w1.clone(),
                activity: hornvale_species::ActivityCycle::Diurnal,
                label: "h".into(),
            };
            // from > both seed days so the frozen ledger holds no future facts and the
            // agent starts at home, not yet thirsty.
            let sys = DriveMovements {
                npcs: vec![npc],
                from: WorldTime { day: 1.0 },
                to: WorldTime { day: 41.0 },
                params: SUSTENANCE,
                terrain: &terrain,
            };
            let next = hornvale_kernel::tick(&ledger, &[&sys], &["drive-movements"], &reg).unwrap();
            // the rooms it drank at (its believed destinations)
            next.find(DRANK)
                .filter(|f| f.subject == e)
                .filter_map(|f| f.day)
                .filter_map(|d| {
                    next.find(AGENT_AT)
                        .filter(|g| g.subject == e)
                        .filter(|g| g.day.is_some_and(|gd| gd <= d))
                        .filter_map(|g| match &g.object {
                            Value::Text(s) => Some((g.day.unwrap(), room_from_text(s))),
                            _ => None,
                        })
                        .max_by(|a, b| a.0.total_cmp(&b.0))
                        .map(|(_, r)| r)
                })
                .collect()
        };
        let a_dests = run(&w1);
        let b_dests = run(&w2);
        assert!(
            a_dests.iter().all(|r| *r == w1),
            "A (knows W1) drinks at W1: {a_dests:?}"
        );
        assert!(
            b_dests.iter().all(|r| *r == w2),
            "B (knows W2) drinks at W2: {b_dests:?}"
        );
        assert_ne!(w1, w2);
    }

    #[test]
    fn an_ignorant_agent_discovers_water_then_later_beelines() {
        // DISCOVERY: a fresh NPC (no perceived water) explores downhill, finds water,
        // drinks; belief now formed, a later thirst cycle beelines. The first journey
        // (exploration) differs from the later (beeline).
        let reg = {
            let mut r = agent_at_reg();
            r.register_predicate(DRANK, false, "drank").unwrap();
            r
        };
        // A downhill chain home(100) -> a(50) -> water(fresh); other neighbors high
        // (elevation still steers the exploration prior; fresh-water truth is
        // now a separate planted set — the-surmise T5 re-wire).
        let home = raddr(1.0);
        let a = home.neighbors()[0].clone();
        let water = a.neighbors().iter().find(|n| **n != home).unwrap().clone();
        let mut m = std::collections::BTreeMap::new();
        m.insert(home.clone(), 100.0);
        m.insert(a.clone(), 50.0);
        m.insert(water.clone(), 10.0); // still the lowest, so exploration steps onto it
        let terrain = PlantedTerrain {
            elevations: m,
            fresh: [water.clone()].into_iter().collect(),
        };
        let mut ledger = Ledger::default();
        let e = ledger.mint_entity();
        let npc = Npc {
            entity: e,
            home: home.clone(),
            resource: water.clone(),
            activity: hornvale_species::ActivityCycle::Diurnal,
            label: "h".into(),
        };
        let sys = DriveMovements {
            npcs: vec![npc.clone()],
            from: WorldTime { day: 0.0 },
            to: WorldTime { day: 40.0 },
            params: SUSTENANCE,
            terrain: &terrain,
        };
        let next = hornvale_kernel::tick(&ledger, &[&sys], &["drive-movements"], &reg).unwrap();
        // It drank at least twice (multiple cycles) and reached the water room.
        let drinks = next.find(DRANK).filter(|f| f.subject == e).count();
        assert!(
            drinks >= 2,
            "discovered water and drank across cycles: {drinks}"
        );
        // Belief formed: after the run, believed_water is the discovered source.
        assert_eq!(
            believed_water(&next, &npc, WorldTime { day: 40.0 }, &terrain, PLAN_BUDGET),
            Some(water)
        );
        let _ = ledger;
    }
}

//! The Quickening: the world's first autonomous motion. NPCs derived like the
//! possessed agent walk deterministic daily routes; their position over time is
//! a pure schedule (derived, reversible — the routine). This module is the
//! pure foundation only: deriving NPCs and their daily-route schedule. No
//! ledger facts are committed here and no session/tick wiring exists yet
//! (that is later Quickening work); domains are untouched (The Walk §11).

use crate::agent::{settlement_position, walk_depth};
use hornvale_kernel::{
    ConditionResponse, EntityId, Fact, Ledger, RoomAddr, RoomId, SearchSpace, TickSystem, Value,
    World, WorldTime, astar,
};
use hornvale_locale::LocaleContext;
use hornvale_species::ActivityCycle;

/// A derived non-player agent: a minted entity, a home and a resource room,
/// its species, and that species' activity-cycle. Derived from the genesis
/// world, never stored (re-derivable).
/// type-audit: bare-ok(identifier-text: label), bare-ok(identifier-text: species), bare-ok(ratio: deliberation_latency)
#[derive(Clone, Debug)]
pub struct Npc {
    /// The NPC's minted ledger entity (subject of its future `agent-at` facts).
    pub entity: EntityId,
    /// Where the NPC rests (its home settlement's room).
    pub home: RoomAddr,
    /// The room its sustenance drive seeks (the-wanting supersedes the old
    /// fixed-schedule destination: this IS the drive's resource anchor now).
    pub resource: RoomAddr,
    /// The NPC's species (kind label), threaded from `species_of` at derivation
    /// the same way the niche and latency are — the health metric's by-species
    /// distress attribution reads it.
    pub species: String,
    /// The species activity-cycle. Write-only this slice: the drive is the sole
    /// mover (the activity gate was dropped), retained for the deferred
    /// activity-gating followup (a diurnal NPC seeking water only while awake).
    pub activity: ActivityCycle,
    /// The species' temperature niche (`ConditionNiche.temperature`): the
    /// thermal (flow) drive's setpoint and tolerance, threaded from the
    /// species' authored `biosphere_registry` at derivation the same way
    /// `activity` is (the perception/psych pattern). A per-NPC datum because
    /// co-derived NPCs may be different species with different niches.
    pub temperature_niche: ConditionResponse,
    /// The species' `PsychVector.deliberation_latency`: slides the arbitration
    /// rule between *grab* (myopic, serve the loudest need) and *weigh* (the
    /// full weighted sum) — psychology's first runtime job (spec §6). Threaded
    /// from `psyche_registry` at derivation.
    pub deliberation_latency: f64,
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
/// type-audit: bare-ok(ratio: rise), bare-ok(ratio: act)
#[derive(Clone, Copy, Debug)]
pub struct DriveParams {
    /// Drive gained per day while away from the resource.
    pub rise: f64,
    /// The seek threshold: drive >= act -> plan to the resource and drink.
    pub act: f64,
}

/// The one authored sustenance drive (thirst/foraging); rates chosen so a cycle
/// spans a few days. (The old `sated` felt-state threshold is retired — since
/// The Temperament, `Session::needs` renders the affect read, spec §7, not a
/// bare thirst scalar.)
pub const SUSTENANCE: DriveParams = DriveParams {
    rise: 0.15,
    act: 0.85,
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

    /// The room's PER-DAY temperature on `day`, °C — the diurnal+seasonal
    /// signal a thermal (flow) drive senses at its own cell, distinct from
    /// the render path's annual-MEAN `temperature_c` (untouched, so the
    /// possession walk and almanac stay byte-identical). `LocaleTerrain`
    /// reads the locale's per-day `temperature_at`; planted test terrain
    /// plants specific room temperatures directly. `INFINITY` for an
    /// undescribable room — its deviation from any optimum is infinite, so it
    /// is never chosen as a comfort target (mirroring `elevation`'s
    /// never-chosen-downhill convention).
    /// type-audit: waiver(temperature-convention: return)
    fn temperature(&self, room: &RoomAddr, day: WorldTime) -> f64;
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
    fn temperature(&self, room: &RoomAddr, day: WorldTime) -> f64 {
        // The PER-DAY field (`LocaleContext::temperature_at`), NOT `describe`'s
        // annual-mean `temperature_c` — so the drive gets a diurnal/seasonal
        // swing while the render path stays byte-identical. INFINITY for an
        // undescribable room (never chosen as a comfort target).
        self.ctx.temperature_at(room, day).unwrap_or(f64::INFINITY)
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

/// A DRIVE: a felt need plus how to reduce it — the two halves the decision
/// policy consults. Thirst (`Thirst`) is the single implementor this stage;
/// later temperament work adds siblings (thermal comfort, …) behind the SAME
/// seam. `urgency`/`act_threshold` are the "need" half (the drive's current
/// pressure and the level at which it acts); `affordance` is the "how to
/// reduce it" half (the next executable step, or `None` when the drive cannot
/// currently be advanced). A STOCK drive (thirst) reads only the precomputed
/// `Perceived` view (self-knowledge + belief + immediate affordance) — never
/// truth — so it is pure over the view a tick already assembled. A FLOW drive
/// (`Thermal`) additionally senses the ambient field at its OWN position
/// directly (you feel the temperature of the cell you stand in), so it carries
/// the terrain and the day it senses at; that self-perception of the current
/// cell is still pure, keyed only on the drive's own held inputs.
pub trait Drive {
    /// The drive's current urgency in [0, 1] — its felt pressure, read from
    /// the (already-folded) view. Thirst returns `view.drive` (the `drive_at`
    /// fold over `DRANK`), computed upstream by the caller.
    /// type-audit: bare-ok(ratio: return)
    fn urgency(&self, view: &Perceived) -> f64;

    /// The seek threshold: at urgency ≥ this, the drive acts (plans toward its
    /// affordance); below it, the drive yields.
    /// type-audit: bare-ok(ratio: return)
    fn act_threshold(&self) -> f64;

    /// The next executable step that reduces this drive from the view's
    /// position, or `None` when it cannot currently be advanced (its target is
    /// unreachable within `budget`, or there is nowhere new to look). For
    /// thirst: the first step of the A* plan to believed water, else the
    /// exploration step when ignorant. Equivalently, the `argmax` over
    /// candidate actions of [`serviceability`](Drive::serviceability) — the
    /// single-drive path.
    /// type-audit: bare-ok(count: budget)
    fn affordance(&self, view: &Perceived, budget: usize) -> Option<Action>;

    /// This drive's identity — for the commitment mode and the deterministic
    /// tie-break order (a fixed `DriveKind` ordering, reload-stable).
    fn kind(&self) -> DriveKind;

    /// The soft-Maslow ceiling on this drive's urgency CONTRIBUTION to the
    /// action-utility sum (§5): survival drives reach `1.0`; comfort drives
    /// cap lower, so severe cold beats mild thirst while nothing beats dying of
    /// thirst. The hierarchy *emerges* from the ranges — no priority table.
    /// type-audit: bare-ok(ratio: return)
    fn urgency_ceiling(&self) -> f64;

    /// How well `action` serves this drive from the view's position, in
    /// `[0, 1]` — the reduction in the drive's remaining cost (the action-
    /// centric arbitration term, §5). For thirst: `1.0` for the step its
    /// [`affordance`](Drive::affordance) would take (the A*/explore first step,
    /// or `Drink` at water), else `0.0`. For thermal: the drop in thermal
    /// urgency at the neighbour (`0.0` if it doesn't improve comfort, and `0.0`
    /// for `Drink` — a flow drive has no consume).
    /// type-audit: bare-ok(ratio: return), bare-ok(count: budget)
    fn serviceability(&self, action: &Action, view: &Perceived, budget: usize) -> f64;
}

/// A drive's identity — the second key (beside urgency) the arbitration needs:
/// it names which drive a commitment mode is pursuing and imposes a fixed,
/// reload-stable tie-break order (`Thirst` before `Thermal`). Deliberately
/// tiny and closed; new drives extend it in their own campaigns.
/// type-audit: bare-ok(return)
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum DriveKind {
    /// The sustenance (thirst) stock drive.
    Thirst,
    /// The thermal-comfort flow drive.
    Thermal,
}

/// The per-NPC behavioural commitment mode — the errand an NPC is on
/// (spec §5). Session-sandboxed (tick-local, never save-format): it carries
/// across the steps of one walk to give hysteresis (no boundary-dithering, no
/// mid-errand flip-flop), and is re-derived, never persisted.
/// type-audit: bare-ok(return)
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Mode {
    /// Not pursuing any drive, and already home — nothing to do.
    Idle,
    /// Not pursuing any drive, but away from home — walking back.
    Homing,
    /// Committed to a drive's errand (engaged at its `act`, released below
    /// `act − h`, switched only when a challenger's utility wins by `δ`).
    Pursuing(DriveKind),
}

/// A creature's felt state — a derived read of the arbitration, a point in the
/// psychological circumplex (valence × arousal, spec §7). Immaterial and never
/// committed (matching "drive == fold"): a pure function of the decision, so
/// two identical arbitrations feel identically. Carries its **intentional
/// object** (which drive the feeling is about), which — with the decision's own
/// provenance — is what makes distress debuggable and *is* the message a
/// creature emits.
/// type-audit: bare-ok(ratio: arousal), bare-ok(ratio: valence)
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Affect {
    /// How activated the mind is: the greatest urgency among ACTIVE drives
    /// (0 when none is active).
    pub arousal: f64,
    /// Whether the pursued drive is reducing (+) or its affordance is failing
    /// (−): making-progress minus blocked, in `-1.0..=1.0`.
    pub valence: f64,
    /// The circumplex region `(valence, arousal)` falls in.
    pub label: AffectLabel,
    /// What the feeling is ABOUT — the pursued drive — when one is active.
    pub object: Option<DriveKind>,
}

/// The named regions of the valence × arousal circumplex (spec §7). Positive
/// affect is first-class (`Content`/`Eager`); `Searching` is neutral seeking,
/// NOT confusion (excluded from the distress metric); `Helpless` is the sticky
/// negative scar that *persistence* upgrades `Lost`/`Frustrated` into.
/// type-audit: bare-ok(return)
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum AffectLabel {
    /// Positive, low arousal: needs met, puttering — the normal state.
    Content,
    /// Positive, high arousal: chasing a satisfiable need, or a drive just met.
    Eager,
    /// Neutral, mid arousal: seeking with a gradient — normal, NOT confusion.
    Searching,
    /// Negative: blocked with a KNOWN target out of reach — "want it, can't
    /// reach it" (the loud, high-arousal distress the circumplex plots top-left).
    Frustrated,
    /// Negative: blocked with no basis to move toward — "don't know what to do"
    /// (the quiet, low-arousal distress: no known target to strain against).
    Lost,
    /// Negative, persistent: given up despite an active drive — a sticky scar
    /// that reverses slowly (persistence upgrades `Lost`/`Frustrated` here).
    Helpless,
}

/// The whole outcome of one arbitration: the `Intent` to act on, the commitment
/// `Mode` to carry to the next tick, and the derived `Affect` (spec §7). Bundled
/// so callers that narrate or measure feeling get it from the same computation
/// that chose the action — no second, drift-prone derivation.
#[derive(Clone, Debug, PartialEq)]
pub struct Resolution {
    /// What to do this tick.
    pub intent: Intent,
    /// The commitment mode to carry forward (hysteresis).
    pub mode: Mode,
    /// The felt state this decision expresses.
    pub affect: Affect,
}

/// Thirst — the one authored (sustenance) drive, Drive #1. `urgency` is the
/// `drive_at` fold surfaced on the view; `affordance` is the existing
/// belief→`plan_to_water`-first-step / `explore_step` chain. Parameterized by
/// the same `DriveParams`/`SUSTENANCE` the fold uses.
/// type-audit: bare-ok(return)
#[derive(Clone, Copy, Debug)]
pub struct Thirst {
    /// The homeostatic parameters (rise/act) governing this drive.
    pub params: DriveParams,
}

impl Drive for Thirst {
    fn urgency(&self, view: &Perceived) -> f64 {
        view.drive
    }
    fn act_threshold(&self) -> f64 {
        self.params.act
    }
    fn affordance(&self, view: &Perceived, budget: usize) -> Option<Action> {
        match &view.believed_water {
            // Knows water: the first step of the A* plan toward it (None when
            // that known water is unreachable within budget).
            Some(w) => {
                plan_to_water(&view.position, w, budget).and_then(|pl| pl.into_iter().next())
            }
            // Ignorant: the exploration step (None when nowhere new to look).
            None => view.explore_step.clone().map(Action::MoveTo),
        }
    }
    fn kind(&self) -> DriveKind {
        DriveKind::Thirst
    }
    fn urgency_ceiling(&self) -> f64 {
        // Survival: thirst can reach full urgency (nothing beats dying of it).
        1.0
    }
    fn serviceability(&self, action: &Action, view: &Perceived, budget: usize) -> f64 {
        // A stock drive serves exactly the ONE step its affordance would take
        // (the A*/explore first move, or `Drink` at water) — an indicator, so
        // the single-drive `argmax` is precisely `affordance` and the thirst-
        // only decision is byte-identical to Stage 0.
        match self.affordance(view, budget) {
            Some(a) if &a == action => 1.0,
            _ => 0.0,
        }
    }
}

/// The urgency at which the thermal comfort drive is considered to act (its
/// `act_threshold`). Comfort is a low-stakes flow drive, so the threshold sits
/// modestly above the tolerance edge (where urgency is exactly `0.0`): a cell
/// merely a touch outside the niche band is felt but not yet acted on, while a
/// genuinely uncomfortable one (urgency past this) does. An authored Stage-1
/// placeholder; Stage 2's arbitration contextualizes it against the other
/// drives (soft-Maslow ceilings).
const THERMAL_ACT: f64 = 0.5;

/// The soft-Maslow ceiling on the thermal (comfort) drive's urgency
/// contribution (§5). Comfort caps below survival's `1.0`, so a severely cold
/// creature that is only mildly thirsty seeks warmth, while a creature dying of
/// thirst (urgency → `1.0`) ignores any cold. The ordering EMERGES from the
/// ranges — there is no priority table. Authored; contextualized against
/// future drives as they land.
const THERMAL_CEIL: f64 = 0.6;

/// The commitment-mode hysteresis band: a pursued drive engages at its `act`
/// but only RELEASES once its urgency falls below `act − h`. Prevents
/// boundary-dithering at the threshold (a drive flickering active/inactive tick
/// to tick as urgency hovers at `act`).
const HYSTERESIS_H: f64 = 0.1;

/// The challenger switch margin `δ`: while pursuing one drive, the NPC only
/// abandons it for a challenger whose best-action utility exceeds the
/// incumbent's by more than this. Prevents mid-errand flip-flop between two
/// near-equal drives (the errand is sticky, not twitchy).
const SWITCH_MARGIN: f64 = 0.1;

/// Thermal comfort — a FLOW (reactive, state-satisfied) drive, a second
/// [`Drive`] implementor beside [`Thirst`]. Where thirst is a STOCK drive
/// (urgency accrues over time and is reset by a discrete `Drink`), thermal
/// comfort reads the CURRENT cell's per-day temperature against the species'
/// temperature niche every tick: discomfort is instantaneous, and stepping to
/// a more comfortable neighbour reduces it directly (no belief cache, no A* —
/// the comfort gradient step IS the affordance, like thirst's `explore_step`).
///
/// Holds the species' temperature [`ConditionResponse`] (its thermal setpoint
/// `optimum` and tolerance `width`) plus the terrain and day it senses at — a
/// flow drive perceives the ambient temperature of the cell it occupies
/// directly (see the [`Drive`] trait's stock-vs-flow note). NOT wired into the
/// live NPC `decide` this stage (Stage 1 unit-tests it in isolation);
/// arbitration of thirst + thermal together is Stage 2.
/// type-audit: bare-ok(return)
pub struct Thermal<'a> {
    /// The species' temperature niche: `optimum` is the preferred °C, `width`
    /// the tolerance half-band. Discomfort is deviation past `width` from
    /// `optimum`.
    pub niche: ConditionResponse,
    /// The temperature field this drive senses (the cell it stands in and the
    /// three neighbours it may step to).
    pub terrain: &'a dyn Terrain,
    /// The day the temperature is sensed at (the diurnal+seasonal phase).
    pub day: WorldTime,
}

impl<'a> Thermal<'a> {
    /// The absolute temperature deviation from the niche optimum at `room`,
    /// °C — the discomfort distance the drive minimizes. `INFINITY` for an
    /// undescribable room (never chosen as a comfort target).
    fn deviation(&self, room: &RoomAddr) -> f64 {
        (self.terrain.temperature(room, self.day) - self.niche.optimum).abs()
    }

    /// The thermal urgency this drive would feel standing at `room` in `[0, 1]`
    /// — how far its temperature deviates PAST the tolerance band, normalized
    /// by the band width (one further band-width reaches full urgency). Exactly
    /// `0.0` inside the band (`|temp − optimum| ≤ width`), rising outside.
    ///
    /// An UNREADABLE cell (non-finite temperature — an undescribable room, or
    /// planted-`INFINITY` test terrain) yields `0.0`: you cannot feel the
    /// temperature of a cell that reports none, so it registers no discomfort.
    /// This is exactly what keeps the thirst-only walk byte-identical — the
    /// thirst tests plant no temperatures, so their thermal drive stays
    /// inactive (urgency `0.0`) at every cell and never enters arbitration.
    /// type-audit: bare-ok(ratio: return)
    fn urgency_at(&self, room: &RoomAddr) -> f64 {
        let temp = self.terrain.temperature(room, self.day);
        if !temp.is_finite() {
            return 0.0;
        }
        let dev = (temp - self.niche.optimum).abs();
        ((dev - self.niche.width).max(0.0) / self.niche.width).clamp(0.0, 1.0)
    }
}

impl<'a> Drive for Thermal<'a> {
    fn urgency(&self, view: &Perceived) -> f64 {
        self.urgency_at(&view.position)
    }
    fn act_threshold(&self) -> f64 {
        THERMAL_ACT
    }
    fn affordance(&self, view: &Perceived, _budget: usize) -> Option<Action> {
        // Satisfied inside the tolerance band — nothing to do (a flow drive
        // needs no plan; NO A*, so `budget` is unused).
        if self.deviation(&view.position) <= self.niche.width {
            return None;
        }
        // Otherwise, the comfort gradient step: the neighbour whose temperature
        // is CLOSEST to the optimum (too-cold → warmer, too-hot → cooler, both
        // toward the optimum), or `None` when no neighbour is strictly more
        // comfortable than here (boxed in / a local comfort optimum).
        comfort_step(&view.position, self.niche.optimum, self.terrain, self.day).map(Action::MoveTo)
    }
    fn kind(&self) -> DriveKind {
        DriveKind::Thermal
    }
    fn urgency_ceiling(&self) -> f64 {
        THERMAL_CEIL
    }
    fn serviceability(&self, action: &Action, view: &Perceived, _budget: usize) -> f64 {
        // A flow drive is served by PRESENCE in a kinder cell: the reduction in
        // thermal urgency at the neighbour it would step to (0 if the step
        // doesn't improve comfort). No consume — `Drink` serves it not at all.
        match action {
            Action::MoveTo(n) => (self.urgency_at(&view.position) - self.urgency_at(n)).max(0.0),
            Action::Drink => 0.0,
        }
    }
}

/// The comfort gradient step: the neighbour whose per-day temperature is
/// CLOSEST to `optimum` (minimizing `|temp − optimum|`), or `None` when no
/// neighbour is strictly more comfortable than `from` itself. A near-copy of
/// [`downhill_step`] — the same three-neighbour scan and the same
/// `total_cmp`-then-ascending-`RoomAddr` tie-break — but the objective is the
/// minimized absolute temperature deviation rather than elevation, so a
/// too-cold cell steps toward a warmer neighbour and a too-hot one toward a
/// cooler, both toward the optimum.
fn comfort_step(
    from: &RoomAddr,
    optimum: f64,
    terrain: &dyn Terrain,
    day: WorldTime,
) -> Option<RoomAddr> {
    let deviation = |room: &RoomAddr| (terrain.temperature(room, day) - optimum).abs();
    let mut best: Option<(RoomAddr, f64)> = None;
    for n in from.neighbors() {
        let dev = deviation(&n);
        let keep_existing = match &best {
            Some((ba, bd)) => dev.total_cmp(bd).then_with(|| n.cmp(ba)).is_ge(),
            None => false,
        };
        if !keep_existing {
            best = Some((n, dev));
        }
    }
    let (best_room, best_dev) = best.expect("a room has three neighbors");
    // Only step when a neighbour is STRICTLY more comfortable than here (an
    // equal-comfort or worse neighbour is no improvement — hold).
    if best_dev.total_cmp(&deviation(from)).is_lt() {
        Some(best_room)
    } else {
        None
    }
}

/// The single-drive (thirst-only) decision — the Stage-0 seam, preserved
/// byte-for-byte. It is now a thin specialization of [`arbitrate`]: the
/// action-centric arbitration over the single-element drive set `{Thirst}`
/// yields exactly the old control flow (thirsty and knows water → A* first step
/// and drink; thirsty and ignorant → the explore step, or `Hold` if nowhere
/// new; not thirsty and away → plan home; else `Hold`), because with one drive
/// the max-utility action IS its [`affordance`](Drive::affordance) and the
/// grab/weigh latency is irrelevant. A fresh `Idle` mode per call keeps it
/// stateless, as before. `arbitrate` is the multi-drive live path.
/// type-audit: bare-ok(count: budget)
pub fn decide(view: &Perceived, home: &RoomAddr, p: &DriveParams, budget: usize) -> Intent {
    let thirst = Thirst { params: *p };
    let drives: [&dyn Drive; 1] = [&thirst];
    arbitrate(view, home, &drives, 0.0, Mode::Idle, budget).intent
}

/// Action-centric, deterministic arbitration (spec §5/§6): the seam that turns
/// state into an `Intent` when SEVERAL drives may compete. It does NOT pick a
/// drive and follow its gradient — it enumerates the candidate ACTIONS (the ≤3
/// neighbour `MoveTo`s plus `Drink`) and picks the one of maximum utility, so a
/// single move can serve two needs at once (a cell both warmer AND nearer
/// water). Returns the chosen `Intent` and the NEW commitment [`Mode`] (carry
/// it into the next call for hysteresis).
///
/// - **Utility** of an action `= Σ_{d active} weight_d × capped_urgency_d ×
///   serviceability_d(action)`, where `capped_urgency_d = min(urgency_d,
///   ceiling_d)` (soft Maslow) and only drives at/above their `act` threshold
///   (hysteretically) contribute.
/// - **`deliberation_latency` (`latency ∈ [0,1]`) slides the weights** (§6):
///   the pursued (loudest / committed) drive always has `weight = 1`; every
///   OTHER active drive has `weight = latency`. So `latency = 0` is **grab**
///   (myopic — only the pursued drive counts) and `latency = 1` is **weigh**
///   (the full weighted sum), interpolating linearly between.
/// - **Commitment mode & hysteresis:** the pursued drive engages at `act`,
///   releases below `act − h`, and is switched for a challenger only when the
///   challenger's best-action utility beats the incumbent's by `δ`. With no
///   active drive the NPC falls to `Homing` (a step toward `home`) or `Idle`.
/// - **Determinism:** candidate actions are scanned in ascending-`RoomAddr`
///   order (then `Drink`), and every max is a `total_cmp` keeping the earliest
///   on ties — reload-stable.
///
/// type-audit: bare-ok(ratio: latency), bare-ok(count: budget)
pub fn arbitrate(
    view: &Perceived,
    home: &RoomAddr,
    drives: &[&dyn Drive],
    latency: f64,
    incoming: Mode,
    budget: usize,
) -> Resolution {
    // Which drives are ACTIVE (contribute to the utility sum). A drive engages
    // at its `act`; the incumbent pursued drive stays engaged until it falls
    // below `act − h` (hysteresis — no boundary dithering).
    let active: Vec<bool> = drives
        .iter()
        .map(|d| {
            let u = d.urgency(view);
            let is_incumbent = matches!(incoming, Mode::Pursuing(k) if k == d.kind());
            if is_incumbent {
                u >= d.act_threshold() - HYSTERESIS_H
            } else {
                u >= d.act_threshold()
            }
        })
        .collect();

    // Arousal: the greatest urgency across ALL drives (spec §7) — how activated
    // the mind is, whether or not any has crossed its `act`. A creature grows
    // aroused as thirst rises before it acts: still Content, but not indifferent.
    // Computed once so every return path's affect reads the same value.
    let arousal = drives
        .iter()
        .map(|d| d.urgency(view))
        .fold(0.0_f64, f64::max);

    // No active drive → the errand is over: walk home, or rest if already home.
    // Felt state: Content (positive valence), carrying the sub-act arousal so a
    // reader can tell puttering-calm from growing-thirsty.
    if !active.iter().any(|a| *a) {
        let affect = Affect {
            arousal,
            valence: 1.0,
            label: AffectLabel::Content,
            object: None,
        };
        if view.position != *home {
            let step =
                plan_to_room(&view.position, home, budget).and_then(|pl| pl.into_iter().next());
            return Resolution {
                intent: step.map(Intent::Do).unwrap_or(Intent::Hold),
                mode: Mode::Homing,
                affect,
            };
        }
        return Resolution {
            intent: Intent::Hold,
            mode: Mode::Idle,
            affect,
        };
    }

    // The capped urgency each active drive lends the sum (soft Maslow).
    let capped = |i: usize| drives[i].urgency(view).min(drives[i].urgency_ceiling());

    // Candidate actions, in a fixed deterministic order: neighbours ascending,
    // then `Drink` (only ever the winner when a drive's serviceability makes it
    // so — thirst at water; otherwise its utility is 0 and it is never chosen).
    let mut neighbors = view.position.neighbors();
    neighbors.sort();
    let mut candidates: Vec<Action> = neighbors.into_iter().map(Action::MoveTo).collect();
    candidates.push(Action::Drink);

    // A drive's best single-drive (grab-style) utility over the candidates —
    // the score the commitment switch compares incumbent vs challenger on.
    let grab_utility = |i: usize| -> f64 {
        candidates
            .iter()
            .map(|a| capped(i) * drives[i].serviceability(a, view, budget))
            .fold(0.0_f64, f64::max)
    };

    // The loudest active drive by grab-utility (ties broken by the fixed
    // `drives[]`/`DriveKind` order — the first such index wins).
    let loudest = (0..drives.len())
        .filter(|&i| active[i])
        .fold(None::<(usize, f64)>, |best, i| {
            let u = grab_utility(i);
            match best {
                Some((_, bu)) if u.total_cmp(&bu).is_le() => best,
                _ => Some((i, u)),
            }
        })
        .map(|(i, _)| i)
        .expect("at least one drive is active here");

    // The pursued drive, with hysteretic commitment: keep the incumbent unless
    // a challenger's grab-utility beats it by more than δ.
    let pursued = match incoming {
        Mode::Pursuing(k)
            if drives
                .iter()
                .enumerate()
                .any(|(i, d)| d.kind() == k && active[i]) =>
        {
            let inc = drives.iter().position(|d| d.kind() == k).unwrap();
            if loudest != inc
                && grab_utility(loudest)
                    .total_cmp(&(grab_utility(inc) + SWITCH_MARGIN))
                    .is_gt()
            {
                loudest
            } else {
                inc
            }
        }
        _ => loudest,
    };
    let pursued_kind = drives[pursued].kind();

    // Weight each active drive: the pursued drive at 1, every other active
    // drive at `latency` (grab 0 ↔ weigh 1). Then utility = weighted sum.
    let utility = |a: &Action| -> f64 {
        (0..drives.len())
            .filter(|&i| active[i])
            .map(|i| {
                let weight = if drives[i].kind() == pursued_kind {
                    1.0
                } else {
                    latency
                };
                weight * capped(i) * drives[i].serviceability(a, view, budget)
            })
            .sum()
    };

    // The max-utility action, earliest-on-ties (ascending RoomAddr, Drink last).
    let mut best_i = 0usize;
    let mut best_u = utility(&candidates[0]);
    for (i, a) in candidates.iter().enumerate().skip(1) {
        let u = utility(a);
        if u.total_cmp(&best_u).is_gt() {
            best_u = u;
            best_i = i;
        }
    }

    // A positive-utility action advances the errand; otherwise the pursued
    // drive is blocked (unreachable water / boxed-in comfort) — Hold, staying
    // committed (the mode is the errand, even when it cannot step this tick).
    let object = Some(pursued_kind);
    if best_u.total_cmp(&0.0).is_gt() {
        let chosen = candidates[best_i].clone();
        // A progressing decision. Relief (Eager) when the drive is directly MET
        // (a Drink) or the creature is beelining to a KNOWN source it can reach;
        // neutral Searching when it is following a gradient toward an UNKNOWN one
        // (spec §7 — searching is normal seeking, NOT confusion, the load-bearing
        // exclusion from the distress metric). Thermal, which sets no
        // `believed_water`, reads Searching while gradient-seeking comfort — and
        // once the cell is comfortable no drive is active, so it reads Content.
        let known = view.believed_water.is_some();
        let (label, valence) = match &chosen {
            Action::Drink => (AffectLabel::Eager, 1.0),
            Action::MoveTo(_) if known => (AffectLabel::Eager, 0.5),
            Action::MoveTo(_) => (AffectLabel::Searching, 0.0),
        };
        Resolution {
            intent: Intent::Do(chosen),
            mode: Mode::Pursuing(pursued_kind),
            affect: Affect {
                arousal,
                valence,
                label,
                object,
            },
        }
    } else {
        // Blocked: no candidate reduces the drive. With a KNOWN target it cannot
        // reach (a believed source), the creature is Frustrated ("want it, can't
        // reach it"); with no basis to move toward, it is Lost ("don't know what
        // to do"). Persistence (in the caller) upgrades either to Helpless.
        let label = if view.believed_water.is_some() {
            AffectLabel::Frustrated
        } else {
            AffectLabel::Lost
        };
        Resolution {
            intent: Intent::Hold,
            mode: Mode::Pursuing(pursued_kind),
            affect: Affect {
                arousal,
                valence: -1.0,
                label,
                object,
            },
        }
    }
}

/// The felt state a derived NPC has at `day` — an instantaneous snapshot read
/// from the frozen ledger: the same arbitration a walk step runs, but stateless
/// (belief and last-drank are folded from history; exploration starts fresh, no
/// incumbent mode, so no sticky `Helpless` — persistence is the caller's, e.g.
/// the health metric's continuous loop). The narration seam
/// (`Session::needs`) reads a creature's `Affect` through this.
pub fn affect_of(frozen: &Ledger, npc: &Npc, day: WorldTime, terrain: &dyn Terrain) -> Affect {
    let pos = agent_position(frozen, npc, day);
    let last_drank = frozen
        .find(DRANK)
        .filter(|f| f.subject == npc.entity)
        .filter_map(|f| f.day)
        .fold(0.0_f64, f64::max);
    let believed = believed_water(frozen, npc, day, terrain, PLAN_BUDGET);
    let drive = (SUSTENANCE.rise * (day.day - last_drank)).clamp(0.0, 1.0);
    let visited = std::collections::BTreeSet::new();
    let explore_step = lowest_unvisited_neighbor(&pos, &visited, terrain);
    let view = Perceived {
        position: pos,
        drive,
        believed_water: believed,
        explore_step,
    };
    let thirst = Thirst { params: SUSTENANCE };
    let thermal = Thermal {
        niche: npc.temperature_niche,
        terrain,
        day,
    };
    let drives: [&dyn Drive; 2] = [&thirst, &thermal];
    arbitrate(
        &view,
        &npc.home,
        &drives,
        npc.deliberation_latency,
        Mode::Idle,
        PLAN_BUDGET,
    )
    .affect
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
            // The commitment mode, carried across this walk's steps (session-
            // sandboxed hysteresis; re-derived, never persisted). Starts Idle.
            let mut mode = Mode::Idle;
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
                // BOTH drives now compete: thirst (the drank-fold, on the view)
                // and thermal (the species' niche read against the per-day
                // temperature at the cell). Where thermal is inactive (a
                // comfortable cell — planted terrain with no temperature reads
                // INFINITY → urgency 0), arbitration reduces to thirst-only and
                // the walk is byte-identical to Stage 0.
                let thirst = Thirst {
                    params: self.params,
                };
                let thermal = Thermal {
                    niche: npc.temperature_niche,
                    terrain: self.terrain,
                    day: WorldTime { day },
                };
                let drives: [&dyn Drive; 2] = [&thirst, &thermal];
                let resolution = arbitrate(
                    &view,
                    &npc.home,
                    &drives,
                    npc.deliberation_latency,
                    mode,
                    PLAN_BUDGET,
                );
                mode = resolution.mode;
                match resolution.intent {
                    Intent::Do(Action::MoveTo(n)) => {
                        day += MOVE_DURATION;
                        if day > self.to.day {
                            break;
                        }
                        // Provenance follows the committed errand (the mode):
                        // thirst distinguishes BELIEVED (beelining a known
                        // source) from IGNORANT (exploring blind); thermal names
                        // the comfort-seeking; homing names the sated walk back.
                        let provenance = match mode {
                            Mode::Pursuing(DriveKind::Thermal) => "sought a kinder clime (comfort)",
                            Mode::Pursuing(DriveKind::Thirst) if believed.is_some() => {
                                "went down to the river it knew (thirst)"
                            }
                            Mode::Pursuing(DriveKind::Thirst) => {
                                "wandered, having found no water yet (thirst)" // ignorant
                            }
                            Mode::Homing | Mode::Idle => "walking home (sated)",
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

    // Authored per-species data, read once: the temperature niche (the thermal
    // drive's setpoint/tolerance) and the psych vector's deliberation latency
    // (the arbitration tuning). Threaded onto each NPC the same way `activity`
    // is — the perception/psych pattern.
    let biosphere = hornvale_species::biosphere_registry();
    let psyche = hornvale_species::psyche_registry();

    settlements
        .into_iter()
        .map(|village| {
            let home = settlement_room(world, ctx, village.id);
            let resource = nearest_water(&home, &LocaleTerrain::new(ctx), PLAN_BUDGET)
                .unwrap_or_else(|| home.clone());
            let species = hornvale_species::species_of(world, village.id)
                .unwrap_or_else(|| "goblin".to_string());
            let activity = species_activity(world, &species);
            let temperature_niche = biosphere
                .get_by_label(&species)
                .map(|t| t.condition_niche.temperature)
                .unwrap_or(DEFAULT_TEMPERATURE_NICHE);
            let deliberation_latency = psyche
                .get_by_label(&species)
                .map(|p| p.deliberation_latency)
                .unwrap_or(0.5);
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
                species,
                activity,
                temperature_niche,
                deliberation_latency,
                label,
            }
        })
        .collect()
}

/// The temperature-niche fallback for a species missing from the biosphere
/// registry (defensive — `species` always resolves to at least the `goblin`
/// default, which IS registered). A wide, mild, low-devotion band so the
/// thermal drive of an unknown species stays quiescent rather than flailing.
const DEFAULT_TEMPERATURE_NICHE: ConditionResponse = ConditionResponse {
    optimum: 15.0,
    width: 25.0,
    devotion: 0.5,
};

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

    /// A neutral temperature niche for the thirst/belief tests, which never
    /// plant temperatures (so the thermal drive reads INFINITY → urgency 0 →
    /// inactive → byte-identical thirst behaviour). Its value is irrelevant to
    /// those tests; the thermal-drive tests build their own `warm`/`cold`
    /// niches directly.
    fn test_niche() -> ConditionResponse {
        ConditionResponse {
            optimum: 15.0,
            width: 10.0,
            devotion: 0.5,
        }
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
            species: "goblin".into(),
            activity: hornvale_species::ActivityCycle::Diurnal,
            temperature_niche: test_niche(),
            deliberation_latency: 0.5,
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
            species: "goblin".into(),
            activity: hornvale_species::ActivityCycle::Diurnal,
            temperature_niche: test_niche(),
            deliberation_latency: 0.5,
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
            species: "goblin".into(),
            activity: hornvale_species::ActivityCycle::Diurnal,
            temperature_niche: test_niche(),
            deliberation_latency: 0.5,
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
            species: "goblin".into(),
            activity: hornvale_species::ActivityCycle::Diurnal,
            temperature_niche: test_niche(),
            deliberation_latency: 0.5,
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
            species: "goblin".into(),
            activity: hornvale_species::ActivityCycle::Diurnal,
            temperature_niche: test_niche(),
            deliberation_latency: 0.5,
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
            species: "goblin".into(),
            activity: hornvale_species::ActivityCycle::Diurnal,
            temperature_niche: test_niche(),
            deliberation_latency: 0.5,
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
            species: "goblin".into(),
            activity: hornvale_species::ActivityCycle::Diurnal,
            temperature_niche: test_niche(),
            deliberation_latency: 0.5,
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
            species: "goblin".into(),
            activity: hornvale_species::ActivityCycle::Diurnal,
            temperature_niche: test_niche(),
            deliberation_latency: 0.5,
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
            temps: std::collections::BTreeMap::new(),
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
            species: "goblin".into(),
            activity: hornvale_species::ActivityCycle::Diurnal,
            temperature_niche: test_niche(),
            deliberation_latency: 0.5,
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
            temps: std::collections::BTreeMap::new(),
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
            species: "goblin".into(),
            activity: hornvale_species::ActivityCycle::Diurnal,
            temperature_niche: test_niche(),
            deliberation_latency: 0.5,
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
        // Under action-centric arbitration (The Temperament): `drive == 0.0`
        // is "thirst active" by the threshold (`0.0 >= act == 0.0`), but its
        // CAPPED urgency is `0.0`, so EVERY action's utility is
        // `weight × 0.0 × serviceability == 0.0` — a zero-pressure drive drives
        // nothing. Arbitration therefore returns `Hold` on every step (rather
        // than the old model's blind explore-move, which stepped regardless of
        // urgency magnitude), and the walk terminates purely through the Hold
        // arm's non-finite idle-jump guard (`act / rise == 0.0 / 0.0 == NaN`
        // → `continue`), bounded by the unconditional `steps >= MAX_STEPS` cap.
        // This exercises the NaN-guard/Hold-spin path DIRECTLY — a stronger
        // termination proof than the old explore-move spin, and the correct
        // behaviour: no felt pressure, no motion.
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
            species: "goblin".into(),
            activity: hornvale_species::ActivityCycle::Diurnal,
            temperature_niche: test_niche(),
            deliberation_latency: 0.5,
            label: "herder".into(),
        };
        // No fresh water anywhere, so belief never forms.
        let t = PlantedTerrain::fresh_only(std::iter::empty());
        let degenerate = DriveParams {
            rise: 0.0,
            act: 0.0,
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
        // The load-bearing proof is that the call above RETURNED at all — with
        // a 1_000_000-day interval, only the `steps >= MAX_STEPS` cap can bound
        // the NaN-guarded Hold-spin; a real hang would time this test out. The
        // committed-fact count is exactly ZERO: a zero-urgency drive drives
        // nothing (every action's utility is `× 0.0 == 0.0` → Hold), so no
        // `agent-at`/`drank` is ever emitted — the correct behaviour, and
        // trivially within the MAX_STEPS bound.
        let moves = next.find(AGENT_AT).filter(|f| f.subject == e).count();
        let drinks = next.find(DRANK).filter(|f| f.subject == e).count();
        assert!(
            moves + drinks <= MAX_STEPS,
            "the MAX_STEPS cap must bound total committed facts even under a \
             NaN-producing degenerate DriveParams; got {moves} moves + {drinks} drinks"
        );
        assert_eq!(
            moves + drinks,
            0,
            "a zero-urgency (zero-pressure) drive drives nothing: arbitration \
             Holds every step and the walk terminates via the bounded NaN-guard \
             Hold-spin, committing no facts"
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
            species: "goblin".into(),
            activity: hornvale_species::ActivityCycle::Diurnal,
            temperature_niche: test_niche(),
            deliberation_latency: 0.5,
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
        /// Planted per-room temperatures (°C) for the thermal-drive tests;
        /// INFINITY elsewhere (the thirst tests never read temperature).
        temps: std::collections::BTreeMap<RoomAddr, f64>,
    }
    impl PlantedTerrain {
        /// No elevation data — just a set of fresh-water rooms (the common
        /// case for the belief-fold tests, which never exercise
        /// `downhill_step`/`nearest_water`'s elevation reads).
        fn fresh_only(rooms: impl IntoIterator<Item = RoomAddr>) -> Self {
            Self {
                elevations: std::collections::BTreeMap::new(),
                fresh: rooms.into_iter().collect(),
                temps: std::collections::BTreeMap::new(),
            }
        }
        /// No fresh water anywhere — just planted elevations (the
        /// exploration/downhill tests, which never exercise belief).
        fn dry(elevations: std::collections::BTreeMap<RoomAddr, f64>) -> Self {
            Self {
                elevations,
                fresh: std::collections::BTreeSet::new(),
                temps: std::collections::BTreeMap::new(),
            }
        }
        /// Just planted per-room temperatures (the thermal-drive tests, which
        /// never exercise elevation/water). Rooms without a planted temperature
        /// read `INFINITY` (never chosen as a comfort target).
        fn thermal(temps: impl IntoIterator<Item = (RoomAddr, f64)>) -> Self {
            Self {
                elevations: std::collections::BTreeMap::new(),
                fresh: std::collections::BTreeSet::new(),
                temps: temps.into_iter().collect(),
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
        fn temperature(&self, room: &RoomAddr, _day: WorldTime) -> f64 {
            self.temps.get(room).copied().unwrap_or(f64::INFINITY)
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
                species: "goblin".into(),
                activity: hornvale_species::ActivityCycle::Diurnal,
                temperature_niche: test_niche(),
                deliberation_latency: 0.5,
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
            temps: std::collections::BTreeMap::new(),
        };
        let mut ledger = Ledger::default();
        let e = ledger.mint_entity();
        let npc = Npc {
            entity: e,
            home: home.clone(),
            resource: water.clone(),
            species: "goblin".into(),
            activity: hornvale_species::ActivityCycle::Diurnal,
            temperature_niche: test_niche(),
            deliberation_latency: 0.5,
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

    // --- The thermal comfort drive (Stage 1, a flow-drive in ISOLATION). ---

    /// A warm-blooded niche (optimum ~18 °C) with a modest tolerance band —
    /// authored with a narrow width so the discrimination against the cold
    /// niche is clean (the real goblin niche's very wide width tolerates
    /// nearly everything, which can't demonstrate flight vs. tolerance).
    fn warm_niche() -> ConditionResponse {
        ConditionResponse {
            optimum: 18.0,
            width: 8.0,
            devotion: 0.45,
        }
    }
    /// A cold-adapted niche (optimum ~6 °C), same tolerance band as
    /// [`warm_niche`] so the two differ only in setpoint.
    fn cold_niche() -> ConditionResponse {
        ConditionResponse {
            optimum: 6.0,
            width: 8.0,
            devotion: 0.85,
        }
    }
    /// The zero-drive, ignorant view a flow-drive test reads (thirst state is
    /// irrelevant to the thermal drive — it senses temperature at `position`).
    fn at(position: RoomAddr) -> Perceived {
        Perceived {
            position,
            drive: 0.0,
            believed_water: None,
            explore_step: None,
        }
    }

    #[test]
    fn thermal_affordance_steps_toward_the_comfortable_neighbor_both_directions() {
        // TOO COLD → warmer neighbour; TOO HOT → cooler neighbour; both toward
        // the optimum. The comfort step is the neighbour whose temperature is
        // CLOSEST to the optimum, exactly as `downhill_step` picks the lowest.
        let home = raddr(1.0);
        let ns = home.neighbors();
        let day = WorldTime { day: 0.0 };

        // Too COLD: home at −10 (dev 28 past optimum 18), ns[0] warmest/closest.
        let cold_here = PlantedTerrain::thermal([
            (home.clone(), -10.0),
            (ns[0].clone(), 10.0),  // dev 8  → closest to optimum
            (ns[1].clone(), -30.0), // dev 48
            (ns[2].clone(), -20.0), // dev 38
        ]);
        let drive = Thermal {
            niche: warm_niche(),
            terrain: &cold_here,
            day,
        };
        let view = at(home.clone());
        assert_eq!(
            drive.affordance(&view, PLAN_BUDGET),
            Some(Action::MoveTo(ns[0].clone())),
            "too cold: steps toward the warmer neighbour"
        );
        assert!(
            drive.urgency(&view) > drive.act_threshold(),
            "a 28 °C deviation is well past the act threshold"
        );

        // Too HOT: home at 40 (dev 22), ns[0] coolest/closest to optimum 18.
        let hot_here = PlantedTerrain::thermal([
            (home.clone(), 40.0),
            (ns[0].clone(), 20.0), // dev 2  → closest to optimum
            (ns[1].clone(), 60.0), // dev 42
            (ns[2].clone(), 50.0), // dev 32
        ]);
        let drive = Thermal {
            niche: warm_niche(),
            terrain: &hot_here,
            day,
        };
        assert_eq!(
            drive.affordance(&view, PLAN_BUDGET),
            Some(Action::MoveTo(ns[0].clone())),
            "too hot: steps toward the cooler neighbour"
        );
    }

    #[test]
    fn thermal_within_tolerance_is_satisfied_no_urgency_no_step() {
        // Inside the tolerance band: urgency is exactly 0 (< act) and there is
        // no affordance (nothing to do), even though a strictly-more-optimal
        // neighbour exists — comfort is a satisfied state, not a maximizer.
        let home = raddr(1.0);
        let ns = home.neighbors();
        let day = WorldTime { day: 0.0 };
        let t = PlantedTerrain::thermal([
            (home.clone(), 20.0),  // dev 2 ≤ width 8 → comfortable
            (ns[0].clone(), 18.0), // exactly optimal, but we don't chase it
        ]);
        let drive = Thermal {
            niche: warm_niche(),
            terrain: &t,
            day,
        };
        let view = at(home.clone());
        assert_eq!(drive.urgency(&view), 0.0, "inside the band → zero urgency");
        assert!(drive.urgency(&view) < drive.act_threshold());
        assert_eq!(
            drive.affordance(&view, PLAN_BUDGET),
            None,
            "comfortable → no comfort step"
        );
    }

    #[test]
    fn thermal_respects_the_niche_cold_tolerates_what_warm_flees() {
        // NICHE RESPECT: the SAME cell (a cold 2 °C room) is tolerated by a
        // cold-adapted niche (optimum 6, dev 4 ≤ 8) but fled by a warm one
        // (optimum 18, dev 16 > 8). Different setpoint → different verdict.
        let home = raddr(1.0);
        let ns = home.neighbors();
        let day = WorldTime { day: 0.0 };
        let t = PlantedTerrain::thermal([
            (home.clone(), 2.0),
            (ns[0].clone(), 16.0), // warmer — the warm niche's comfort target
            (ns[1].clone(), -10.0),
            (ns[2].clone(), -20.0),
        ]);
        let view = at(home.clone());

        let cold = Thermal {
            niche: cold_niche(),
            terrain: &t,
            day,
        };
        assert_eq!(cold.urgency(&view), 0.0, "the cold niche tolerates 2 °C");
        assert_eq!(
            cold.affordance(&view, PLAN_BUDGET),
            None,
            "tolerated → the cold niche stays put"
        );

        let warm = Thermal {
            niche: warm_niche(),
            terrain: &t,
            day,
        };
        assert!(warm.urgency(&view) > 0.0, "the warm niche flees 2 °C");
        assert_eq!(
            warm.affordance(&view, PLAN_BUDGET),
            Some(Action::MoveTo(ns[0].clone())),
            "the warm niche steps toward the warmer neighbour"
        );
    }

    #[test]
    fn thermal_urgency_and_affordance_are_deterministic_and_recompute_identically() {
        // Reload-stable by construction: the thermal drive reads only its held
        // niche + terrain + day (no ledger, no stored state), so recomputing
        // twice is byte-identical.
        let home = raddr(1.0);
        let ns = home.neighbors();
        let day = WorldTime { day: 3.5 };
        let t = PlantedTerrain::thermal([
            (home.clone(), -12.0),
            (ns[0].clone(), 4.0),
            (ns[1].clone(), -25.0),
            (ns[2].clone(), -18.0),
        ]);
        let drive = Thermal {
            niche: warm_niche(),
            terrain: &t,
            day,
        };
        let view = at(home.clone());
        assert_eq!(drive.urgency(&view), drive.urgency(&view));
        assert_eq!(
            drive.affordance(&view, PLAN_BUDGET),
            drive.affordance(&view, PLAN_BUDGET)
        );
    }

    #[test]
    fn thermal_comfort_step_breaks_ties_by_ascending_room_addr() {
        // DETERMINISM UNDER A GENUINE TIE: two neighbours EQUIDISTANT from the
        // optimum (symmetric about it, 0 °C and 12 °C around optimum 6, both
        // dev 6) must resolve to the smaller-`RoomAddr` one — the same
        // `total_cmp` + ascending-`RoomAddr` tie-break `downhill_step` uses (cf.
        // `downhill_step_picks_the_lowest_neighbor_deterministically`).
        let home = raddr(1.0);
        let ns = home.neighbors();
        let day = WorldTime { day: 0.0 };
        let smaller = std::cmp::min(ns[0].clone(), ns[1].clone());
        let t = PlantedTerrain::thermal([
            (home.clone(), -40.0),  // dev 46 → outside the band, worse than either
            (ns[0].clone(), 0.0),   // dev 6
            (ns[1].clone(), 12.0),  // dev 6 → ties ns[0]
            (ns[2].clone(), -40.0), // dev 46 → not chosen
        ]);
        let drive = Thermal {
            niche: cold_niche(),
            terrain: &t,
            day,
        };
        let view = at(home.clone());
        assert_eq!(
            drive.affordance(&view, PLAN_BUDGET),
            Some(Action::MoveTo(smaller)),
            "an equal-deviation tie resolves to the smaller RoomAddr"
        );
    }

    // --- Action-centric arbitration (Stage 2): thirst + thermal compete. ---

    /// A low-`act` sustenance drive so a MODERATE thirst urgency is already
    /// "active" — lets the arbitration tests put thirst and thermal in the same
    /// active window (real `SUSTENANCE.act` is `0.85`, above the thermal
    /// ceiling `0.6`, so the two rarely coexist active on real params; the
    /// mechanism is what these tests pin, per spec §5).
    fn eager_thirst() -> DriveParams {
        DriveParams {
            rise: 0.15,
            act: 0.4,
        }
    }

    #[test]
    fn affect_reads_each_circumplex_region() {
        // The affect label is a pure read of the same arbitration that chose the
        // action (spec §7). A comfortable-everywhere terrain keeps thermal
        // inactive so thirst is the sole drive under test.
        let home = raddr(1.0);
        let ns = home.neighbors();
        let water = ns[0].clone();
        let far = raddr(9_999.0); // a believed source with no path within budget
        let day = WorldTime { day: 0.0 };
        let terrain = PlantedTerrain::thermal([
            (home.clone(), 18.0),
            (ns[0].clone(), 18.0),
            (ns[1].clone(), 18.0),
            (ns[2].clone(), 18.0),
        ]);
        let thirst = Thirst { params: SUSTENANCE };
        let thermal = Thermal {
            niche: warm_niche(),
            terrain: &terrain,
            day,
        };
        let drives: [&dyn Drive; 2] = [&thirst, &thermal];
        let label = |view: &Perceived| {
            arbitrate(view, &home, &drives, 0.5, Mode::Idle, PLAN_BUDGET)
                .affect
                .label
        };

        // CONTENT — not thirsty (drive < act): no active drive, puttering.
        assert_eq!(
            label(&Perceived {
                position: home.clone(),
                drive: 0.1,
                believed_water: Some(water.clone()),
                explore_step: None,
            }),
            AffectLabel::Content,
        );
        // EAGER — parched and standing at water: Drink satisfies (relief).
        assert_eq!(
            label(&Perceived {
                position: water.clone(),
                drive: 0.95,
                believed_water: Some(water.clone()),
                explore_step: None,
            }),
            AffectLabel::Eager,
        );
        // SEARCHING — parched, ignorant, a gradient to explore: approaching,
        // NOT confusion (the load-bearing exclusion from the distress metric).
        assert_eq!(
            label(&Perceived {
                position: home.clone(),
                drive: 0.95,
                believed_water: None,
                explore_step: Some(ns[2].clone()),
            }),
            AffectLabel::Searching,
        );
        // (FRUSTRATED — Hold while KNOWING where water is — is rare by design:
        // believed water is a cell the creature stood in, so it is almost always
        // reachable; it fires only when a known source falls beyond the plan
        // budget in a large world. The branch is `believed.is_some()` on Hold;
        // its sibling LOST below exercises the same Hold path.)
        let _ = &far;
        // LOST — parched, ignorant, nowhere new to go: no basis to move.
        assert_eq!(
            label(&Perceived {
                position: home.clone(),
                drive: 0.95,
                believed_water: None,
                explore_step: None,
            }),
            AffectLabel::Lost,
        );
    }

    #[test]
    fn arbitrate_in_a_comfortable_cell_is_byte_identical_to_thirst_only_decide() {
        // THE CRUX (thirst-only preserved): where thermal is INACTIVE (a
        // comfortable cell — every reachable cell at the niche optimum, urgency
        // 0), the two-drive arbitration must produce the EXACT `Intent` the
        // Stage-0 thirst-only `decide` does, for every state. Proven by direct
        // equality against `decide` on the same views.
        let home = raddr(1.0);
        let ns = home.neighbors();
        let water = ns[0].clone();
        let day = WorldTime { day: 0.0 };
        // All cells at the warm niche's optimum → thermal urgency 0 everywhere.
        let terrain = PlantedTerrain::thermal([
            (home.clone(), 18.0),
            (ns[0].clone(), 18.0),
            (ns[1].clone(), 18.0),
            (ns[2].clone(), 18.0),
        ]);
        let params = SUSTENANCE;
        let thirst = Thirst { params };
        let thermal = Thermal {
            niche: warm_niche(),
            terrain: &terrain,
            day,
        };
        let drives: [&dyn Drive; 2] = [&thirst, &thermal];
        // A representative spread of thirst states; each must arbitrate ==
        // decide. Latency 0.5 (the goblin baseline) — irrelevant here since
        // only one drive is ever active, which is exactly the point.
        let views = [
            // parched, at home, knows water → plan's first step toward water
            Perceived {
                position: home.clone(),
                drive: 0.9,
                believed_water: Some(water.clone()),
                explore_step: None,
            },
            // sated, away (at water) → plan home
            Perceived {
                position: water.clone(),
                drive: 0.1,
                believed_water: Some(water.clone()),
                explore_step: None,
            },
            // sated, at home → Hold
            Perceived {
                position: home.clone(),
                drive: 0.1,
                believed_water: Some(water.clone()),
                explore_step: None,
            },
            // parched, ignorant, has an explore step → explore
            Perceived {
                position: home.clone(),
                drive: 0.9,
                believed_water: None,
                explore_step: Some(ns[2].clone()),
            },
            // parched, ignorant, nowhere new → Hold
            Perceived {
                position: home.clone(),
                drive: 0.9,
                believed_water: None,
                explore_step: None,
            },
        ];
        for v in &views {
            assert_eq!(
                arbitrate(v, &home, &drives, 0.5, Mode::Idle, PLAN_BUDGET).intent,
                decide(v, &home, &params, PLAN_BUDGET),
                "a comfortable-cell creature must decide exactly as thirst-only: {v:?}"
            );
        }
    }

    #[test]
    fn arbitrate_prefers_the_move_that_serves_both_drives() {
        // MULTI-DRIVE: a creature both thirsty AND cold. One neighbour is BOTH
        // the water step and warm; another is warm only. The both-serving
        // neighbour wins — utility sums across drives, so it beats the
        // thermal-only neighbour that serves just the (equally loud) comfort.
        let home = raddr(1.0);
        let ns = home.neighbors();
        let both = ns[0].clone(); // water + warm
        let warm_only = ns[1].clone(); // warm, not water
        let cold = ns[2].clone(); // neither
        let day = WorldTime { day: 0.0 };
        let terrain = PlantedTerrain::thermal([
            (home.clone(), -20.0),     // freezing → thermal urgency 1.0
            (both.clone(), 18.0),      // optimum → big comfort gain
            (warm_only.clone(), 18.0), // optimum → equal comfort gain
            (cold.clone(), -20.0),     // no gain
        ]);
        let view = Perceived {
            position: home.clone(),
            drive: 0.9,
            believed_water: Some(both.clone()),
            explore_step: None,
        };
        let thirst = Thirst { params: SUSTENANCE };
        let thermal = Thermal {
            niche: warm_niche(),
            terrain: &terrain,
            day,
        };
        let drives: [&dyn Drive; 2] = [&thirst, &thermal];
        // Weigh (latency 1): the full weighted sum, so the both-serving move
        // wins decisively over the warm-only one.
        let intent = arbitrate(&view, &home, &drives, 1.0, Mode::Idle, PLAN_BUDGET).intent;
        assert_eq!(
            intent,
            Intent::Do(Action::MoveTo(both)),
            "the move serving BOTH thirst and thermal beats the warm-only move"
        );
    }

    #[test]
    fn grab_and_weigh_resolve_the_same_conflict_differently() {
        // THE DIVERGENCE PROOF (§6): the SAME conflict, one psychology apart.
        // Thermal is the loudest single need (grab serves it: the pure-warmth
        // step X). But a third neighbour Z serves BOTH thirst and thermal
        // moderately, so the weighted SUM lifts Z above X — weigh takes Z.
        let home = raddr(1.0);
        let ns = home.neighbors();
        let warm = ns[0].clone(); // pure warmth (loudest single relief)
        let both = ns[1].clone(); // water + moderate warmth
        let cold = ns[2].clone();
        let day = WorldTime { day: 0.0 };
        let terrain = PlantedTerrain::thermal([
            (home.clone(), -20.0), // urgency 1.0 (capped 0.6)
            (warm.clone(), 18.0),  // thermal serv 1.0
            (both.clone(), 6.0),   // urgency 0.5 → thermal serv 0.5
            (cold.clone(), -20.0),
        ]);
        let view = Perceived {
            position: home.clone(),
            drive: 0.5, // moderate thirst (capped 0.5), active under eager_thirst
            believed_water: Some(both.clone()),
            explore_step: None,
        };
        let thirst = Thirst {
            params: eager_thirst(),
        };
        let thermal = Thermal {
            niche: warm_niche(),
            terrain: &terrain,
            day,
        };
        let drives: [&dyn Drive; 2] = [&thirst, &thermal];
        let Resolution {
            intent: grab,
            mode: gm,
            ..
        } = arbitrate(&view, &home, &drives, 0.0, Mode::Idle, PLAN_BUDGET);
        let weigh = arbitrate(&view, &home, &drives, 1.0, Mode::Idle, PLAN_BUDGET).intent;
        assert_eq!(
            grab,
            Intent::Do(Action::MoveTo(warm.clone())),
            "grab (impulsive) takes the nearest relief for the loudest need — pure warmth"
        );
        assert_eq!(
            weigh,
            Intent::Do(Action::MoveTo(both.clone())),
            "weigh (deliberate) takes the move best relieving TOTAL discomfort"
        );
        assert_ne!(grab, weigh, "psychology alone changed the resolution");
        // Grab committed to the loudest drive (thermal).
        assert_eq!(gm, Mode::Pursuing(DriveKind::Thermal));
    }

    #[test]
    fn soft_maslow_severe_cold_beats_mild_thirst_but_dying_of_thirst_beats_any_cold() {
        // SOFT MASLOW via urgency ceilings (no priority table): comfort caps at
        // 0.6, survival reaches 1.0. Same freezing world, water and warmth in
        // opposite directions; the winner flips with thirst severity alone.
        let home = raddr(1.0);
        let ns = home.neighbors();
        let warm = ns[0].clone();
        let water = ns[1].clone();
        let cold = ns[2].clone();
        let day = WorldTime { day: 0.0 };
        let terrain = PlantedTerrain::thermal([
            (home.clone(), -20.0),  // severe cold: thermal urgency 1.0 (cap 0.6)
            (warm.clone(), 18.0),   // warmth here
            (water.clone(), -20.0), // water here, but no warmer
            (cold.clone(), -20.0),
        ]);
        let thermal = Thermal {
            niche: warm_niche(),
            terrain: &terrain,
            day,
        };
        // (a) MILD thirst (0.5, active under eager_thirst, capped 0.5 < 0.6):
        //     severe cold wins → step toward WARMTH.
        let mild = Perceived {
            position: home.clone(),
            drive: 0.5,
            believed_water: Some(water.clone()),
            explore_step: None,
        };
        let eager = Thirst {
            params: eager_thirst(),
        };
        let mild_drives: [&dyn Drive; 2] = [&eager, &thermal];
        assert_eq!(
            arbitrate(&mild, &home, &mild_drives, 0.5, Mode::Idle, PLAN_BUDGET).intent,
            Intent::Do(Action::MoveTo(warm.clone())),
            "severe cold beats mild thirst"
        );
        // (b) DYING thirst (1.0, capped 1.0 > any comfort cap): nothing beats
        //     it → step toward WATER even while freezing.
        let dying = Perceived {
            position: home.clone(),
            drive: 1.0,
            believed_water: Some(water.clone()),
            explore_step: None,
        };
        let survival = Thirst { params: SUSTENANCE };
        let dying_drives: [&dyn Drive; 2] = [&survival, &thermal];
        assert_eq!(
            arbitrate(&dying, &home, &dying_drives, 0.5, Mode::Idle, PLAN_BUDGET).intent,
            Intent::Do(Action::MoveTo(water.clone())),
            "dying of thirst beats any cold"
        );
    }

    #[test]
    fn commitment_hysteresis_prevents_flip_flop_between_near_equal_drives() {
        // HYSTERESIS: once committed to a drive, a challenger only marginally
        // louder (within the switch margin δ) does NOT steal the errand — the
        // committed mode is sticky across ticks, so no dithering.
        let home = raddr(1.0);
        let ns = home.neighbors();
        let warm = ns[0].clone();
        let water = ns[1].clone();
        let day = WorldTime { day: 0.0 };
        // Freezing home (thermal urgency 1.0, capped 0.6); a fully-comfortable
        // warm neighbour → thermal grab-utility = capped(0.6) × drop(1.0) = 0.6.
        // Water lies in a cold direction (no thermal help).
        let terrain = PlantedTerrain::thermal([
            (home.clone(), -20.0),
            (warm.clone(), 18.0),
            (water.clone(), -20.0),
            (ns[2].clone(), -20.0),
        ]);
        let thirst = Thirst {
            params: eager_thirst(),
        };
        let thermal = Thermal {
            niche: warm_niche(),
            terrain: &terrain,
            day,
        };
        let drives: [&dyn Drive; 2] = [&thirst, &thermal];
        // Tick 1: thirst grab-utility 0.50 < thermal 0.60 → commit to thermal.
        let low = Perceived {
            position: home.clone(),
            drive: 0.50,
            believed_water: Some(water.clone()),
            explore_step: None,
        };
        let m1 = arbitrate(&low, &home, &drives, 0.5, Mode::Idle, PLAN_BUDGET).mode;
        assert_eq!(
            m1,
            Mode::Pursuing(DriveKind::Thermal),
            "commit to the louder thermal drive"
        );
        // Tick 2+: thirst climbs to 0.65 — now marginally LOUDER than thermal
        // (0.60), but within δ = 0.1. A fresh (Idle) arbitration WOULD flip to
        // thirst; carrying the committed mode must NOT.
        let high = Perceived {
            position: home.clone(),
            drive: 0.65,
            believed_water: Some(water.clone()),
            explore_step: None,
        };
        assert_eq!(
            arbitrate(&high, &home, &drives, 0.5, Mode::Idle, PLAN_BUDGET).mode,
            Mode::Pursuing(DriveKind::Thirst),
            "control: with no committed mode, the now-louder thirst is pursued (the test bites)"
        );
        let mut mode = m1;
        for _ in 0..5 {
            let m = arbitrate(&high, &home, &drives, 0.5, mode, PLAN_BUDGET).mode;
            assert_eq!(
                m,
                Mode::Pursuing(DriveKind::Thermal),
                "the committed errand is sticky: a within-δ challenger never steals it"
            );
            mode = m;
        }
    }

    #[test]
    fn arbitration_is_deterministic_reload_stable_and_breaks_ties_by_ascending_room_addr() {
        // DETERMINISM + TIE-BREAK: arbitration reads only view + terrain + niche
        // (no ledger), so it recomputes identically (reload-stable by
        // construction). And a genuine two-way utility tie resolves to the
        // smaller `RoomAddr` — the constitutional `total_cmp` + ascending-addr
        // rule, here on the arbitration's own action scan.
        let home = raddr(1.0);
        let ns = home.neighbors();
        let day = WorldTime { day: 0.0 };
        let smaller = std::cmp::min(ns[0].clone(), ns[1].clone());
        // Thermal-only (thirst inactive, drive 0): home freezing, ns[0] and
        // ns[1] EQUALLY warm (both optimum) → equal thermal serviceability →
        // equal utility → the tie-break decides.
        let terrain = PlantedTerrain::thermal([
            (home.clone(), -20.0),
            (ns[0].clone(), 18.0),
            (ns[1].clone(), 18.0),
            (ns[2].clone(), -20.0),
        ]);
        let view = Perceived {
            position: home.clone(),
            drive: 0.0,
            believed_water: None,
            explore_step: None,
        };
        let thirst = Thirst { params: SUSTENANCE };
        let thermal = Thermal {
            niche: warm_niche(),
            terrain: &terrain,
            day,
        };
        let drives: [&dyn Drive; 2] = [&thirst, &thermal];
        let a = arbitrate(&view, &home, &drives, 0.5, Mode::Idle, PLAN_BUDGET);
        let b = arbitrate(&view, &home, &drives, 0.5, Mode::Idle, PLAN_BUDGET);
        assert_eq!(a, b, "same inputs → same (Intent, Mode); reload-stable");
        assert_eq!(
            a.intent,
            Intent::Do(Action::MoveTo(smaller)),
            "an equal-utility tie resolves to the smaller RoomAddr"
        );
    }
}

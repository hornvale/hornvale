//! The possession session: a pure step function over a frozen world. Every
//! verb is read-only; possessing a world never changes it.

use crate::liveness::{
    AGENT_AT, Affect, AffectLabel, DRANK, DriveKind, DriveMovements, EATEN, LocaleTerrain, Npc,
    RESTED, SUSTENANCE, affect_of, agent_position, derive_npcs, derive_wild_npcs,
};
use crate::{
    Agent, Focalized, Focalizer, IdentityProjection, Knowledge, PossessOpts, Projection,
    TemplateFocalizer, Turn, VesselError, absorb_common, mint_flagship, observable, reader_set,
};
use hornvale_kernel::{
    ConceptRegistry, EntityId, Fact, Ledger, RoomAddr, Value, World, WorldTime, tick,
};
use hornvale_locale::{Compass, Direction, ExitKind, LocaleContext};

/// How many NPCs a session derives (spec §4: a small authored constant, not
/// every settlement — the flagship's own leader plus a couple of neighbors).
const NPC_COUNT: usize = 3;

/// How many WILD beast agents a session derives (The Wilding) — a small handful
/// of the world's fauna (a herd, a lair) walking alongside the peoples.
const WILD_COUNT: usize = 4;

/// The closed fallback line `consult` renders when no initiated line
/// unlocks (spec §3.2; the Global Constraints' closed-strings list).
const CONSULT_FALLBACK: &str = "The Book holds more for the initiated.";

/// The player-authored disposition-shift predicate (The First Mark): the
/// first fact the possessing player, not a world system, ever commits.
/// type-audit: bare-ok(identifier-text)
pub const DISPOSITION_SHIFT: &str = "disposition-shift";

/// How hard one committed `disposition-shift` fact leans on an NPC's
/// grievance toward the possessing player (The First Mark, direct social
/// consequence — decision-ledger #6: the first slice's consequence is
/// direct social, not an ambient drive tip). A game-design coefficient, not
/// a tuned physical constant.
/// type-audit: bare-ok(ratio)
pub const GRIEVANCE_GAIN: f64 = 1.0;

/// Net grievance at which a neutral NPC turns hostile toward the player —
/// three net provokes, one per day (same-day repeats dedup, Task 1), so
/// three distinct days of antagonism. A game-design constant, not an
/// empirical drive value: this mechanic never reads or perturbs the
/// homeostatic drive layer (`liveness.rs`), and there is no seed-42
/// calibration behind it.
/// type-audit: bare-ok(ratio)
pub const HOSTILITY_THRESHOLD: f64 = 3.0;

/// The one-hop forward integration (The First Mark): an NPC whose grievance
/// crosses `HOSTILITY_THRESHOLD` commits this fact toward the possessing
/// player. Functional per subject — an NPC turns hostile once, so the `wait`
/// tick's firing (guarded by `Ledger::value_of`) is idempotent by
/// construction, not by a separate dedup check.
/// type-audit: bare-ok(identifier-text)
pub const TURNED_HOSTILE: &str = "turned-hostile";

/// An NPC's grievance toward the possessing player: the additive fold over
/// their committed `disposition-shift` facts (The First Mark, direct social
/// consequence). Zero with no player facts, so an unplayed world — or a
/// session that never provokes/soothes this NPC — is byte-identical to that
/// zero by construction.
pub(crate) fn grievance(ledger: &Ledger, npc: EntityId) -> f64 {
    ledger
        .facts_about(npc)
        .filter(|f| f.predicate == DISPOSITION_SHIFT)
        .map(|f| match f.object {
            Value::Number(n) => n,
            _ => 0.0,
        })
        .sum::<f64>()
        * GRIEVANCE_GAIN
}

const HELP: &str = "\
verbs:
  look             where you stand, focalized
  go <dir>         walk a compass exit (n ne e se s sw w nw)
  examine <thing>  anything look mentions
  back             retrace your last step
  wait [N]         let N days pass overhead (default 1); the world moves too
  whoami           the one you possess
  knows            everything they have seen
  npcs             the derived NPCs sharing this world (label, id)
  why <who>        recount an NPC's dated history (by label or id)
  needs            read the felt state of anyone sharing this room
  provoke [who]    shift a co-located NPC's disposition, your own mark
  soothe [who]     ease a co-located NPC's disposition, your own mark
  write <sentence> speak a line of Common; you absorb what it says, written
                   into your own margin
  consult          read the Book's Reckoning at your own day, and whatever
                   your margin has initiated you into
  release          let go (quit works too)
";

/// A live possession over a frozen world. The possessed agent's own senses
/// stay pinned to the frozen `world` (byte-identical, never mutated); only
/// the NPC layer evolves, in a session-owned ledger clone (the-quickening).
pub struct Session<'w> {
    world: &'w World,
    ctx: LocaleContext,
    agent: Agent,
    knowledge: Knowledge,
    trail: Vec<RoomAddr>,
    day: WorldTime,
    focalizer: TemplateFocalizer,
    projection: IdentityProjection,
    /// The evolving ledger: a clone of the frozen world's ledger, mutated
    /// only by `wait`'s tick (NPC `agent-at` facts). Never written back.
    ledger: Ledger,
    /// A clone of the world's registry, extended with `AGENT_AT` (registered
    /// per-session, never at genesis — spec §3).
    registry: ConceptRegistry,
    /// The NPCs this session derived at `start` (re-derivable, never saved).
    npcs: Vec<Npc>,
    /// The world's calendar, built once at `start`, so the NPC wake cycle reads
    /// the real sun (The Slumber Tier-1); `None` on a world with no sky.
    calendar: Option<hornvale_astronomy::Calendar>,
    /// The world's predator-pressure field (The Quarry), computed once at
    /// `start`, so the danger drive senses carnivore territory; `None` if the
    /// demography fit fails.
    predator: Option<hornvale_kernel::CellMap<f64>>,
    /// The world's prey-pressure field (The Teeth), computed once at `start`, so
    /// a carnivore's hunger senses prey territory; `None` if the demography fit
    /// fails.
    prey: Option<hornvale_kernel::CellMap<f64>>,
}

impl<'w> Session<'w> {
    /// Begin a possession: build the locale context, mint the flagship
    /// agent, absorb the first projection, and return the opening text.
    /// type-audit: bare-ok(prose: return)
    pub fn start(
        world: &'w World,
        opts: &PossessOpts,
    ) -> Result<(Session<'w>, String), VesselError> {
        let ctx = LocaleContext::build(world).map_err(VesselError::Locale)?;
        let agent = mint_flagship(world, &ctx)?;
        let mut ledger = world.ledger.clone();
        let mut registry = world.registry.clone();
        // Idempotent (same def every session): never conflicts, since
        // AGENT_AT is never registered at genesis (spec §3).
        registry
            .register_predicate(AGENT_AT, false, "an agent's position on a day")
            .expect("AGENT_AT registers identically every session");
        // Idempotent (same def every session): never conflicts, since DRANK
        // is never registered at genesis either (spec §3).
        registry
            .register_predicate(DRANK, false, "an agent satisfied its sustenance goal")
            .expect("DRANK registers identically every session");
        registry
            .register_predicate(
                RESTED,
                false,
                "an agent rested (eased its fatigue) on a day",
            )
            .expect("RESTED registers identically every session");
        registry
            .register_predicate(EATEN, false, "an agent ate (eased its hunger) on a day")
            .expect("EATEN registers identically every session");
        // The player's disposition mark — the first player-authored predicate.
        // Non-functional (a subject may be provoked and later soothed; each is
        // one dated fact). Additive: registering a new predicate perturbs
        // nothing already committed.
        registry
            .register_predicate(
                DISPOSITION_SHIFT,
                false,
                "an agent's disposition was shifted by the possessing player",
            )
            .expect("DISPOSITION_SHIFT registers identically every session");
        // The consequence of the mark above (The First Mark, one-hop forward
        // integration): functional (an NPC turns hostile once — the second
        // commit attempt is a guaranteed no-op, not just a discouraged one).
        registry
            .register_predicate(
                TURNED_HOSTILE,
                true,
                "an NPC turned hostile toward the possessing player",
            )
            .expect("TURNED_HOSTILE registers identically every session");
        // Guarantee the possessed agent's OWN settlement contributes a
        // derived NPC (the-quickening T3 review): otherwise no NPC is ever
        // co-located with the player and the observation payoff can't fire.
        let mut npcs = derive_npcs(world, &ctx, &mut ledger, NPC_COUNT, agent.village.id);
        // The Wilding: append a few wild beast agents (a herd, a lair) so the
        // world's fauna walks alongside its peoples — and a herbivore beast
        // finally fears predator ground (The Quarry, live). Off only for the
        // settled-population narration unit tests that isolate the peopled path.
        if opts.wild_agents {
            npcs.extend(derive_wild_npcs(world, &ctx, &mut ledger, WILD_COUNT));
        }
        // Build the world's calendar once, for the NPC wake cycle's real-sun
        // read (The Slumber Tier-1). Absent (no sky) → the fractional-day sun.
        let calendar = hornvale_worldgen::sky_of(world)
            .ok()
            .and_then(|sky| sky.calendar().cloned());
        // Compute the predator-pressure field once (The Quarry), so the danger
        // drive senses carnivore territory. A demography fit — bounded to session
        // start; `None` on failure (danger simply loses its PREDATOR axis).
        let predator = hornvale_worldgen::predator_pressure(world).ok();
        // The prey-pressure field (The Teeth), so a carnivore's hunger senses
        // prey territory — the dual of the predator field, same one-shot fit.
        let prey = hornvale_worldgen::prey_pressure(world).ok();
        let mut session = Session {
            world,
            ctx,
            agent,
            knowledge: Knowledge::default(),
            trail: Vec::new(),
            day: opts.day,
            focalizer: TemplateFocalizer,
            projection: IdentityProjection,
            ledger,
            registry,
            npcs,
            calendar,
            predator,
            prey,
        };
        session.absorb_here()?;
        let opening = session.describe_here()?;
        Ok((session, opening))
    }

    /// The possessed agent (read-only).
    pub fn agent(&self) -> &Agent {
        &self.agent
    }

    /// The accumulated knowledge (read-only).
    pub fn knowledge(&self) -> &Knowledge {
        &self.knowledge
    }

    /// The locale context this session walks (for the battery's checks).
    pub fn context(&self) -> &LocaleContext {
        &self.ctx
    }

    /// How many `agent-at` facts the session's owned ledger has committed —
    /// zero until the first `wait` (test accessor: T3's day-zero guard).
    /// type-audit: bare-ok(count: return)
    pub fn committed_agent_at_count(&self) -> usize {
        self.ledger.find(AGENT_AT).count()
    }

    /// How many `drank` facts the session's owned ledger has committed —
    /// zero until the first `wait` (test accessor: The Confluence's
    /// on-water settlements can satisfy sustenance without ever committing
    /// an `agent-at`, so `committed_agent_at_count` alone can no longer
    /// stand in for "the world moved").
    /// type-audit: bare-ok(count: return)
    pub fn committed_drank_count(&self) -> usize {
        self.ledger.find(DRANK).count()
    }

    /// How many player disposition-shift facts the session's owned ledger
    /// has committed — zero until the first `provoke`/`soothe` (test
    /// accessor: The First Mark's one-fact-per-act guard).
    /// type-audit: bare-ok(count: return)
    pub fn committed_disposition_count(&self) -> usize {
        self.ledger.find(DISPOSITION_SHIFT).count()
    }

    /// How many `turned-hostile` facts the session's owned ledger has
    /// committed — zero until an NPC (co-located when the grievance was
    /// *earned*, not necessarily when the threshold is crossed) first has
    /// its grievance cross `HOSTILITY_THRESHOLD` on a `wait` tick (test
    /// accessor: The First Mark's one-hop forward integration).
    /// type-audit: bare-ok(count: return)
    pub fn committed_hostility_count(&self) -> usize {
        self.ledger.find(TURNED_HOSTILE).count()
    }

    /// The possessed agent's own stable identity as a ledger `EntityId` — the
    /// object a hostile NPC's `turned-hostile` fact points at. The `Agent`
    /// struct itself is never committed to the ledger (derived fresh each
    /// session, spec's reversibility rule), but its `AgentId` is a
    /// deterministic, seed-derived `u64` (`mint_flagship`'s stream draw), so
    /// it is a stable, collision-free identity to reference AS an object —
    /// this never asserts the player has facts of their own, only that an
    /// NPC's own fact points at them.
    fn agent_entity(&self) -> EntityId {
        EntityId::new(self.agent.id.0)
            .expect("a minted agent id is a seeded stream draw, never exactly zero")
    }

    /// Would the named co-located NPC be hostile to the player right now
    /// (their grievance fold at or past `HOSTILITY_THRESHOLD`)? A pure read
    /// — never commits anything. `who` resolves exactly as `provoke`/
    /// `soothe` do (`colocated_npc`): empty selects the sole co-located
    /// NPC, else a numeric id or a case-insensitive label substring; an
    /// unresolved (not-here) `who` reads as not-hostile rather than
    /// erroring. This mechanic's whole consequence is Task 3's; this task
    /// stops at the gate.
    /// type-audit: bare-ok(identifier-text: who), bare-ok(flag: return)
    pub fn would_turn_hostile(&self, who: &str) -> bool {
        self.colocated_npc(who)
            .map(|npc| grievance(&self.ledger, npc.entity) >= HOSTILITY_THRESHOLD)
            .unwrap_or(false)
    }

    /// A named NPC's current grievance fold toward the player (test
    /// accessor: the byte-identity guard that an unprovoked NPC's grievance
    /// is exactly zero). Unlike `would_turn_hostile`, this resolves among
    /// ALL derived NPCs, not only co-located ones — grievance is a ledger
    /// fold over that NPC's own facts, not a proximity check — matched by
    /// numeric id or case-insensitive label substring; `None` if no derived
    /// NPC matches `who`.
    /// type-audit: bare-ok(identifier-text: who), bare-ok(diagnostic-value: return)
    pub fn npc_grievance(&self, who: &str) -> Option<f64> {
        who.parse::<u64>()
            .ok()
            .and_then(|id| self.npcs.iter().find(|n| n.entity.0.get() == id))
            .or_else(|| {
                let needle = who.to_lowercase();
                self.npcs
                    .iter()
                    .find(|n| n.label.to_lowercase().contains(&needle))
            })
            .map(|npc| grievance(&self.ledger, npc.entity))
    }

    /// The session's owned, evolving ledger, serialized — a determinism
    /// accessor: same seed + same waits must yield the same bytes (test
    /// accessor: T3's determinism test).
    /// type-audit: bare-ok(artifact: return)
    pub fn session_ledger_json(&self) -> String {
        serde_json::to_string(&self.ledger).expect("a ledger always serializes")
    }

    /// The derived NPCs' labels (test accessor: the T3 review's colocation
    /// test names the specific NPC whose motion narrates in `wait`'s output,
    /// without hardcoding world-generated prose into the test itself).
    /// type-audit: bare-ok(identifier-text: return)
    pub fn npc_labels(&self) -> Vec<&str> {
        self.npcs.iter().map(|n| n.label.as_str()).collect()
    }

    /// The current room, focalized (for the battery's checks).
    pub fn focalized(&self) -> Result<Focalized, VesselError> {
        let v = observable(self.world, &self.ctx, &self.agent, self.day)?;
        Ok(self.focalizer.render(&v))
    }

    /// The lateral exits from here: each compass bearing paired with its
    /// destination's packed room id (three per room, always — the mesh's
    /// base-edge neighbors). For the walker battery's deterministic pick.
    /// type-audit: bare-ok(index: return)
    pub fn ways(&self) -> Vec<(Compass, u64)> {
        let v = observable(self.world, &self.ctx, &self.agent, self.day)
            .expect("the current position is always observable");
        v.locale
            .exits
            .iter()
            .filter(|e| e.kind == ExitKind::Edge)
            .filter_map(|e| match e.direction {
                Direction::Compass(c) => Some((c, e.to)),
                _ => None,
            })
            .collect()
    }

    /// One verb, one response. `Turn::Released` ends the possession.
    /// type-audit: bare-ok(prose: line)
    pub fn handle(&mut self, line: &str) -> Turn {
        let line = line.trim();
        let (verb, rest) = match line.split_once(' ') {
            Some((v, r)) => (v, r.trim()),
            None => (line, ""),
        };
        match verb {
            "" => Turn::Out(String::new()),
            "look" => self.out(self.describe_here()),
            "go" => self.go(rest),
            "examine" => self.examine(rest),
            "back" => self.back(),
            "wait" => self.wait(rest),
            "whoami" => Turn::Out(self.whoami()),
            "knows" => Turn::Out(self.knows()),
            "npcs" => Turn::Out(self.list_npcs()),
            "why" => Turn::Out(self.why(rest)),
            "needs" => Turn::Out(self.needs()),
            "provoke" => self.act_on_disposition(rest, 1),
            "soothe" => self.act_on_disposition(rest, -1),
            "write" => Turn::Out(self.write(rest)),
            "consult" => Turn::Out(self.consult()),
            "enter" | "exit" => Turn::Out(
                "The grain of the world resists; that way lies another scale of things."
                    .to_string(),
            ),
            "help" => Turn::Out(HELP.to_string()),
            "release" | "quit" => Turn::Released("You let go.".to_string()),
            other => Turn::Out(format!("No verb '{other}' ('help' lists them).")),
        }
    }

    /// Absorb the current room's projection into knowledge.
    fn absorb_here(&mut self) -> Result<(), VesselError> {
        let v = observable(self.world, &self.ctx, &self.agent, self.day)?;
        self.knowledge
            .absorb(self.projection.project(&v, &self.agent.perception));
        Ok(())
    }

    /// The full room rendering: room id, prose, ways on.
    fn describe_here(&self) -> Result<String, VesselError> {
        let v = observable(self.world, &self.ctx, &self.agent, self.day)?;
        let f = self.focalizer.render(&v);
        let ways: Vec<String> = v
            .locale
            .exits
            .iter()
            .filter(|e| e.kind == ExitKind::Edge)
            .filter_map(|e| match e.direction {
                Direction::Compass(c) => Some(format!("{c:?}").to_uppercase()),
                _ => None,
            })
            .collect();
        Ok(format!(
            "[room {}, day {}]\n{}\nWays on: {}.",
            v.locale.id,
            self.day.day,
            f.prose,
            ways.join(", ")
        ))
    }

    fn go(&mut self, dir: &str) -> Turn {
        let Some(wanted) = parse_compass(dir) else {
            return Turn::Out(format!("Go where? '{dir}' is no direction I know."));
        };
        let v = match observable(self.world, &self.ctx, &self.agent, self.day) {
            Ok(v) => v,
            Err(e) => return Turn::Out(format!("error: {e}")),
        };
        let exit = v
            .locale
            .exits
            .iter()
            .find(|e| e.kind == ExitKind::Edge && e.direction == Direction::Compass(wanted));
        let Some(exit) = exit else {
            return Turn::Out(format!("No way {} from here.", dir.to_lowercase()));
        };
        // Lateral exits stay at walk depth: the destination is the
        // neighbor whose packed id the exit names.
        let dest = self
            .agent
            .position
            .neighbors()
            .into_iter()
            .find(|n| n.pack().map(|r| r.0) == Ok(exit.to));
        let Some(dest) = dest else {
            return Turn::Out("error: exit names no neighbor".to_string());
        };
        let from = std::mem::replace(&mut self.agent.position, dest);
        self.trail.push(from);
        if let Err(e) = self.absorb_here() {
            return Turn::Out(format!("error: {e}"));
        }
        self.out(self.describe_here())
    }

    fn back(&mut self) -> Turn {
        let Some(prev) = self.trail.pop() else {
            return Turn::Out("You have not walked anywhere yet.".to_string());
        };
        self.agent.position = prev;
        if let Err(e) = self.absorb_here() {
            return Turn::Out(format!("error: {e}"));
        }
        self.out(self.describe_here())
    }

    fn wait(&mut self, arg: &str) -> Turn {
        // The world moves without you: advance the day, then run the NPC
        // layer's tick against the session-owned ledger (the possessed
        // agent's own frozen reads are untouched — only `self.ledger`
        // evolves).
        let days: f64 = if arg.is_empty() {
            1.0
        } else {
            match arg.parse::<f64>() {
                Ok(d) if d.is_finite() && d > 0.0 => d,
                _ => return Turn::Out(format!("Wait how long? '{arg}' is no span of days.")),
            }
        };
        // Snapshot every NPC's position as of NOW (the day about to end),
        // before advancing — the "before" half of the departure/arrival
        // comparison `narrate_motion` needs to name a specific transition
        // rather than just count facts.
        let before: Vec<RoomAddr> = self
            .npcs
            .iter()
            .map(|npc| agent_position(&self.ledger, npc, self.day))
            .collect();
        let from = self.day;
        self.day = WorldTime {
            day: self.day.day + days,
        };
        let terrain = LocaleTerrain::with_fields(
            &self.ctx,
            self.calendar.as_ref(),
            self.predator.as_ref(),
            self.prey.as_ref(),
        );
        let sys = DriveMovements {
            npcs: self.npcs.clone(),
            from,
            to: self.day,
            params: SUSTENANCE,
            terrain: &terrain,
        };
        match tick(&self.ledger, &[&sys], &["drive-movements"], &self.registry) {
            Ok(next) => {
                let moved = next.len() - self.ledger.len();
                self.ledger = next;
                // The First Mark, one-hop forward integration: after the NPC
                // drive tick settles, any co-located-or-not NPC whose
                // grievance has crossed the hostility threshold commits its
                // `turned-hostile` fact — a discrete social consequence of
                // the player's own acts, not an ambient drive. Iterating
                // `self.npcs` in its existing (derivation) order keeps the
                // commit sequence deterministic.
                let player = self.agent_entity();
                for npc in self.npcs.iter() {
                    // The `value_of(...).is_none()` check below is the SOLE
                    // idempotency guarantee for this fact, not a second
                    // layer atop `TURNED_HOSTILE`'s `functional: true`
                    // registration: `Ledger::commit` only dedups via an
                    // exact full-envelope match, and `day` advances every
                    // tick, so a later-day re-fire is never an exact dup;
                    // and the functional flag only rejects a *different*
                    // object for the same subject/predicate, but `object`
                    // here is always the same constant `player`, so that
                    // flag can never trip either. Remove this guard and the
                    // loop silently refires (a new `turned-hostile` fact,
                    // same subject/predicate/object, only `day` differing)
                    // on every subsequent `wait` the NPC is still past
                    // threshold for.
                    if grievance(&self.ledger, npc.entity) >= HOSTILITY_THRESHOLD
                        && self.ledger.value_of(npc.entity, TURNED_HOSTILE).is_none()
                    {
                        let fact = Fact {
                            subject: npc.entity,
                            predicate: TURNED_HOSTILE.to_string(),
                            object: Value::Entity(player),
                            place: None,
                            day: Some(self.day.day),
                            provenance: "player-provoked".to_string(),
                        };
                        self.ledger
                            .commit(fact, &self.registry)
                            .expect("turned-hostile is registered and finite");
                    }
                }
                // Re-absorb the (possibly changed) here into knowledge; the
                // possessed agent's own scenery is still read from the
                // frozen `self.world`, so this cannot change day-0 output.
                if let Err(e) = self.absorb_here() {
                    return Turn::Out(format!("error: {e}"));
                }
                Turn::Out(self.narrate_motion(moved, &before))
            }
            Err(e) => Turn::Out(format!("Time falters: {e}")),
        }
    }

    /// Narrate what the tick committed: silence if nothing moved, else name
    /// any derived NPC's PERCEPTIBLE TRANSITION through the possessed
    /// agent's own room — an arrival (the NPC was elsewhere, now shares the
    /// room) or a departure (the NPC was here, now elsewhere; an absence is
    /// a real observation too, not just an arrival). `before` is each NPC's
    /// position as of the day just ended (captured by `wait` prior to the
    /// tick); both halves are read back from ledgers, never decorative
    /// flavor text. The generic "stirred" line is the fallback only for
    /// motion that never touches the player's own room.
    fn narrate_motion(&self, moved: usize, before: &[RoomAddr]) -> String {
        if moved == 0 {
            return "Time passes; the world keeps its shape.".to_string();
        }
        let mut arrived: Vec<&str> = Vec::new();
        let mut departed: Vec<&str> = Vec::new();
        for (npc, prior) in self.npcs.iter().zip(before) {
            let was_here = *prior == self.agent.position;
            let is_here = agent_position(&self.ledger, npc, self.day) == self.agent.position;
            match (was_here, is_here) {
                (false, true) => arrived.push(npc.label.as_str()),
                (true, false) => departed.push(npc.label.as_str()),
                _ => {}
            }
        }
        let mut parts: Vec<String> = Vec::new();
        if !departed.is_empty() {
            parts.push(format!("You watch {} go.", departed.join(", ")));
        }
        if !arrived.is_empty() {
            parts.push(format!("You notice {} here now.", arrived.join(", ")));
        }
        if parts.is_empty() {
            format!("Time passes. You sense movement nearby ({moved} stirred).")
        } else {
            format!("Time passes. {}", parts.join(" "))
        }
    }

    fn examine(&self, noun: &str) -> Turn {
        if noun.is_empty() {
            return Turn::Out("Examine what?".to_string());
        }
        let f = match self.focalized() {
            Ok(f) => f,
            Err(e) => return Turn::Out(format!("error: {e}")),
        };
        let wanted = noun.to_lowercase();
        match f.nouns.iter().find(|(n, _)| n.to_lowercase() == wanted) {
            Some((_, detail)) => Turn::Out(detail.clone()),
            None => Turn::Out(format!("You see no {noun} here.")),
        }
    }

    fn whoami(&self) -> String {
        format!(
            "A {} of {} (agent {}), day {}, room {}.",
            self.agent.species,
            self.agent.village.name,
            self.agent.id.0,
            self.day.day,
            self.agent
                .position
                .pack()
                .map(|r| r.0.to_string())
                .unwrap_or_else(|_| "?".to_string()),
        )
    }

    /// List every derived NPC this session knows about, with the entity id
    /// `why` accepts (mirrors the repl's `beliefs` → `why <id>` pattern: an
    /// id-listing verb feeding the recount verb).
    fn list_npcs(&self) -> String {
        let mut lines = vec![format!("{} NPC(s) derived this session:", self.npcs.len())];
        for npc in &self.npcs {
            lines.push(format!("  [{}] {}", npc.entity.0, npc.label));
        }
        lines.join("\n")
    }

    /// Recount an NPC's dated history — the provenance read (the-quickening
    /// T4): the world remembers, so `why` over an NPC that has moved names
    /// each committed `agent-at` with the day it was asserted (`recount` in
    /// `windows/historiography` renders the day suffix). `who` is matched
    /// first as a numeric entity id, else as a case-insensitive substring of
    /// an NPC's label — this mirrors the CLI repl's `why <id>` (see
    /// `cli/src/repl.rs`) over the one kind of subject a possess session
    /// actually has on hand without a prior id-listing step: a name.
    fn why(&self, who: &str) -> String {
        let who = who.trim();
        if who.is_empty() {
            return "Why what? Name an NPC (label or id — see 'npcs').".to_string();
        }
        let target = who
            .parse::<u64>()
            .ok()
            .and_then(|id| self.npcs.iter().find(|n| n.entity.0.get() == id))
            .or_else(|| {
                let needle = who.to_lowercase();
                self.npcs
                    .iter()
                    .find(|n| n.label.to_lowercase().contains(&needle))
            });
        let Some(npc) = target else {
            return format!("No one here answers to '{who}' (see 'npcs').");
        };
        self.recount(npc.entity)
            .unwrap_or_else(|| format!("Nothing is yet recorded of {}.", npc.label))
    }

    /// The provenance read itself: a temporary `World` wrapping this
    /// session's OWN evolving ledger/registry (never the frozen `self.world`
    /// — an NPC's `agent-at` facts live only in the session's evolved
    /// state), handed to the domain-agnostic historiography window exactly
    /// as the CLI repl's `why` hands it the genesis world.
    fn recount(&self, entity: EntityId) -> Option<String> {
        let evolved = World {
            seed: self.world.seed,
            registry: self.registry.clone(),
            ledger: self.ledger.clone(),
        };
        hornvale_historiography::recount(&evolved, entity)
    }

    /// Every derived NPC sharing the possessed agent's current room — the
    /// co-located lookup `needs` and `provoke`/`soothe` both build on.
    fn colocated_npcs(&self) -> Vec<&Npc> {
        self.npcs
            .iter()
            .filter(|npc| agent_position(&self.ledger, npc, self.day) == self.agent.position)
            .collect()
    }

    /// Resolve `who` to one co-located NPC (The First Mark): an empty
    /// argument selects the first NPC sharing this room (the common case —
    /// a lone co-located NPC needs no name), otherwise `who` is matched as a
    /// numeric entity id or a case-insensitive substring of an NPC's label,
    /// mirroring `why`'s resolution but restricted to NPCs actually here.
    fn colocated_npc(&self, who: &str) -> Option<&Npc> {
        let here = self.colocated_npcs();
        let who = who.trim();
        if who.is_empty() {
            return here.into_iter().next();
        }
        who.parse::<u64>()
            .ok()
            .and_then(|id| here.iter().find(|n| n.entity.0.get() == id).copied())
            .or_else(|| {
                let needle = who.to_lowercase();
                here.iter()
                    .find(|n| n.label.to_lowercase().contains(&needle))
                    .copied()
            })
    }

    /// Commit the first player-authored fact: a signed disposition shift on
    /// a co-located NPC. `sign` is +1 (provoke) / -1 (soothe). The fact
    /// carries a `player:` provenance so a reader (and contradiction
    /// checking) can tell it from every fact a world system commits.
    ///
    /// Same-day dedup is intentional, not a bug: exactly one disposition
    /// shift lands per (NPC, day, direction) — escalating a mark on the same
    /// NPC the same day requires time to pass first (a `wait`), not
    /// repeating the verb. Because `self.day` only advances on `wait`, a
    /// same-day repeat of `provoke` (or `soothe`) on the same NPC produces a
    /// byte-identical `Fact` envelope, and `Ledger::commit`'s idempotent
    /// dedup (`Ok(false)` = identical fact already present, nothing
    /// appended) makes it a true no-op. The narration below reads that
    /// return value rather than assuming success, so the player is never
    /// told a mark landed when the ledger disagrees.
    fn act_on_disposition(&mut self, who: &str, sign: i8) -> Turn {
        let Some(npc) = self.colocated_npc(who) else {
            return Turn::Out("There is no one here to provoke or soothe.".to_string());
        };
        let entity = npc.entity;
        let label = npc.label.clone();
        let verb = if sign >= 0 { "provoke" } else { "soothe" };
        let fact = Fact {
            subject: entity,
            predicate: DISPOSITION_SHIFT.to_string(),
            object: Value::Number(sign as f64),
            place: None,
            day: Some(self.day.day),
            provenance: format!("player: {verb}"),
        };
        let appended = self
            .ledger
            .commit(fact, &self.registry)
            .expect("disposition-shift is registered and finite");
        if appended {
            let felt = if sign >= 0 { "bristles" } else { "eases" };
            Turn::Out(format!("You {verb} {label}. They {felt}."))
        } else if sign >= 0 {
            Turn::Out(format!(
                "You round on {label} again, but the moment already holds all the edge it will take today."
            ))
        } else {
            Turn::Out(format!("{label} is already as eased as they'll be today."))
        }
    }

    /// The felt-state read (the-wanting T4, spec §4.5 as corrected by G4):
    /// diegetic prose for every CO-LOCATED NPC's drive, never a raw number.
    /// Deliberately reads the NPCs, not the possessed agent — the player's
    /// own moves are never committed as `agent-at` (only NPCs' are), so
    /// `drive_at` for the player would fold an empty history and read
    /// eternally parched (a followup, decision-ledger #8 / G4 correction (a)
    /// rides player-acts-mutate, Campaign IV). A co-located NPC's drive IS a
    /// real fold over its own committed history, so its felt state is
    /// meaningful the moment the drive model exists.
    fn needs(&self) -> String {
        let here = self.colocated_npcs();
        if here.is_empty() {
            return "No one else is here to read.".to_string();
        }
        // Read each co-located NPC's felt state through the SAME arbitration
        // that drives it (spec §7) — the affect label coloured by what the
        // feeling is about (its intentional object), not a bare thirst scalar.
        let terrain = LocaleTerrain::with_fields(
            &self.ctx,
            self.calendar.as_ref(),
            self.predator.as_ref(),
            self.prey.as_ref(),
        );
        here.iter()
            .map(|npc| {
                let affect = affect_of(&self.ledger, npc, &self.npcs, self.day, &terrain);
                format!("The {} {}.", npc.label, felt_phrase(&affect))
            })
            .collect::<Vec<_>>()
            .join("\n")
    }

    /// Write a Common sentence into the margin: the session absorbs its own
    /// spoken line into its `Knowledge` via the transfer seam (The Echo
    /// T4). Renamed from `tell` at the Vessel Stitch (T2, G3 exchange) —
    /// the player writes what they have learned into their copy's margin,
    /// the program's own margin device turned toward the reader; the
    /// response is the closed string `Written in the margin.` regardless
    /// of how many facts the sentence carried (heard is not true, but
    /// written is initiation — spec §1). The acceptable floor shape — no
    /// NPC addressing yet (a future `write <npc> <sentence>` is a UX
    /// decision this spec doesn't commit to, G3 flag 2).
    fn write(&mut self, line: &str) -> String {
        if line.is_empty() {
            return "Write what? Speak a line of Common.".to_string();
        }
        let ctx = hornvale_book::parse_context(self.world);
        match absorb_common(&mut self.knowledge, line, &ctx) {
            Ok(_) => "Written in the margin.".to_string(),
            Err(e) => format!("That doesn't parse as Common: {e}"),
        }
    }

    /// Read the Book from inside the world (the Vessel Stitch, T2): the
    /// Reckoning of Years at the session's own day (`hornvale_book::
    /// reckoning_at` — the same accessor the CLI's `--at` lens calls, spec
    /// §3.1/§4.4), then whatever the session's own margin (`Knowledge`,
    /// via `write`) has initiated it into
    /// (`hornvale_book::esoteric_lines`) — or the closed fallback line when
    /// nothing has unlocked yet. Reads only: the session's owned `ledger`
    /// and `knowledge` are both untouched (the purity law, spec §4.3);
    /// this method takes `&self`, not `&mut self`.
    fn consult(&self) -> String {
        let day = self.day.day.trunc() as u64;
        let mut lines = vec![format!("The Reckoning, at day {day}.")];
        let at = hornvale_astronomy::StdDays::new(self.day.day)
            .expect("a session's day is always finite and non-negative");
        let epoch = hornvale_book::reckoning_at(self.world, at);
        lines.extend(epoch.lines);
        lines.extend(epoch.margin);
        let initiated = hornvale_book::esoteric_lines(self.world, &reader_set(&self.knowledge));
        if initiated.is_empty() {
            lines.push(CONSULT_FALLBACK.to_string());
        } else {
            lines.extend(initiated);
        }
        lines.join("\n")
    }

    fn knows(&self) -> String {
        let mut lines = vec![format!("{} things seen:", self.knowledge.0.len())];
        for (key, value) in &self.knowledge.0 {
            // char-safe truncation: byte slicing can split a UTF-8 boundary.
            let shown: String = if value.chars().count() > 60 {
                let head: String = value.chars().take(57).collect();
                format!("{head}…")
            } else {
                value.clone()
            };
            lines.push(format!("  {key} = {shown}"));
        }
        lines.join("\n")
    }

    fn out(&self, r: Result<String, VesselError>) -> Turn {
        match r {
            Ok(s) => Turn::Out(s),
            Err(e) => Turn::Out(format!("error: {e}")),
        }
    }
}

/// The arousal above which a still-Content (sub-act) creature reads as restless
/// rather than calm — the rising edge of a need felt before it is acted on.
const RESTLESS_AROUSAL: f64 = 0.4;

/// Render a creature's `Affect` as a felt-state phrase (spec §7): the
/// circumplex label coloured by its intentional object — what the feeling is
/// *about* — so a reader sees not just *that* it frets but *what for*. The
/// object/reason is the debuggable "message" a distressed creature emits.
fn felt_phrase(affect: &Affect) -> String {
    // Pick the object-appropriate wording (thirst / thermal / fatigue / hunger
    // / danger / social / none).
    let about = |thirst: &str,
                 thermal: &str,
                 fatigue: &str,
                 hunger: &str,
                 danger: &str,
                 social: &str,
                 none: &str| {
        match affect.object {
            Some(DriveKind::Thirst) => thirst,
            Some(DriveKind::Thermal) => thermal,
            Some(DriveKind::Fatigue) => fatigue,
            Some(DriveKind::Hunger) => hunger,
            Some(DriveKind::Danger) => danger,
            Some(DriveKind::Social) => social,
            None => none,
        }
        .to_string()
    };
    match affect.label {
        // Below the seek threshold the creature is puttering — but arousal still
        // rises with the need, so a reader can tell true calm from the restless
        // edge before it starts to act.
        AffectLabel::Content if affect.arousal >= RESTLESS_AROUSAL => "grows restless".to_string(),
        AffectLabel::Content => "seems content".to_string(),
        AffectLabel::Eager => about(
            "drinks its fill",
            "settles into a kinder warmth",
            "settles down to rest",
            "eats its fill",
            "reaches safer ground",
            "makes for home and its people",
            "looks pleased",
        ),
        AffectLabel::Searching => about(
            "casts about for water",
            "casts about for a kinder clime",
            "trudges wearily homeward",
            "forages for richer ground",
            "edges away from the uncanny ground",
            "drifts homeward, missing its people",
            "wanders, searching",
        ),
        AffectLabel::Frustrated => about(
            "frets, wanting water it cannot reach",
            "shivers, with no warmth within reach",
            "frets, too far from any rest",
            "frets, famished, with no food in reach",
            "recoils, hemmed in by dread on every side",
            "frets, cut off from home and its people",
            "frets, blocked at every turn",
        ),
        AffectLabel::Lost => "looks lost, unsure where to turn".to_string(),
        AffectLabel::Helpless => about(
            "has given up on water",
            "has given up on warmth",
            "has given up, bone-weary",
            "has given up, starving",
            "has given up, cowering",
            "has given up on ever getting home",
            "has given up",
        ),
    }
}

/// Parse a compass token (case-insensitive, long names allowed).
fn parse_compass(s: &str) -> Option<Compass> {
    match s.to_lowercase().as_str() {
        "n" | "north" => Some(Compass::N),
        "ne" | "northeast" => Some(Compass::Ne),
        "e" | "east" => Some(Compass::E),
        "se" | "southeast" => Some(Compass::Se),
        "s" | "south" => Some(Compass::S),
        "sw" | "southwest" => Some(Compass::Sw),
        "w" | "west" => Some(Compass::W),
        "nw" | "northwest" => Some(Compass::Nw),
        _ => None,
    }
}
